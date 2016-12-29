{-# LANGUAGE TemplateHaskell #-}
module Text.PDF.Slave.Server.Monad(
  -- * Monad
    ServerM
  , runServerM
  , serverMtoHandler
  -- ** Monad environment
  , ServerEnv(..)
  , newServerEnv
  -- * Utilities
  , getConfig
  , emitRenderItem
  , module Text.PDF.Slave.Server.Util
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan
import Control.Concurrent.Thread.Delay
import Control.Monad
import Control.Monad.Base
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.STM
import Control.Monad.Trans.Control
import Data.Monoid
import Data.Text (pack, Text)
import Data.Time (getCurrentTime, diffUTCTime, addUTCTime)
import Servant.Server
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Text.PDF.Slave
import Text.PDF.Slave.Server.API (APINotificationBody(..), toAPIRenderId)
import Text.PDF.Slave.Server.Config
import Text.PDF.Slave.Server.DB
import Text.PDF.Slave.Server.Notification
import Text.PDF.Slave.Server.Util

import qualified Control.Immortal as Immortal
import qualified Shelly as Sh

-- | Server private environment
data ServerEnv = ServerEnv {
  envConfig      :: ServerConfig    -- ^ Configuration used to create the server
, envDB          :: AcidState Model -- ^ Server DB
-- | Coordination channel that is pushed when new item arrived. Workers wait for
-- new value in the channel when they finished all available work from rendering
-- queue. Thats helps to avoid polling when there is no work to do.
, envRenderChan  :: TChan ()
-- | Coordination channel that is pushed when new item arrived. Workers wait for
-- new value in the channel when they finished all available work from notification
-- queue. Thats helps to avoid polling when there is no work to do.
, envNotificationChan  :: TChan ()
-- | Connection manager for sending notifications
, envManager :: Manager
}

-- | Create new server environment
newServerEnv :: (MonadIO m, MonadBaseControl IO m)
  => ServerConfig -> m ServerEnv
newServerEnv cfg = do
  acid <- createDB (serverDatabaseConf cfg)
  renderChan <- liftIO newTChanIO
  notificChan <- liftIO newTChanIO
  mng <- liftIO $ newManager tlsManagerSettings
  let env = ServerEnv {
        envConfig = cfg
      , envDB = acid
      , envRenderChan = renderChan
      , envNotificationChan = notificChan
      , envManager = mng
      }
  liftIO . runServerMIO env $ do
    replicateM_ (serverRenderWorkers cfg) spawnRendererWorker
    replicateM_ (serverNotificationWorkers cfg) spawnNotificationWorker
  return env

-- | Server monad that holds internal environment
newtype ServerM a = ServerM { unServerM :: ReaderT ServerEnv (LoggingT Handler) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadReader ServerEnv
    , MonadLogger, MonadLoggerIO, MonadError ServantErr)

newtype StMServerM a = StMServerM { unStMServerM :: StM (ReaderT ServerEnv (LoggingT Handler)) a }

instance MonadBaseControl IO ServerM where
    type StM ServerM a = StMServerM a
    liftBaseWith f = ServerM $ liftBaseWith $ \q -> f (fmap StMServerM . q . unServerM)
    restoreM = ServerM . restoreM . unStMServerM

instance HasAcidState Model ServerM where
  getAcidState = asks envDB

-- | Execution of 'ServerM'
runServerM :: ServerEnv -> ServerM a -> Handler a
runServerM e = runStdoutLoggingT . flip runReaderT e . unServerM

-- | Execution of 'ServerM' in IO monad
runServerMIO :: ServerEnv -> ServerM a -> IO a
runServerMIO env m = do
  ea <- runExceptT $ runServerM env m
  case ea of
    Left e -> fail $ "runServerMIO: " <> show e
    Right a -> return a

-- | Transformation to Servant 'Handler'
serverMtoHandler :: ServerEnv -> ServerM :~> Handler
serverMtoHandler e = Nat (runServerM e)

-- | Getting server configuration
getConfig :: ServerM ServerConfig
getConfig = asks envConfig

-- | Awake sleeping workers when we have a work to do
emitRenderItem :: ServerM ()
emitRenderItem = do
  chan <- asks envRenderChan
  liftIO . atomically $ writeTChan chan ()

-- | Awake sleeping workers when we have a work to do
emitNotificationItem :: ServerM ()
emitNotificationItem = do
  chan <- asks envNotificationChan
  liftIO . atomically $ writeTChan chan ()

-- | Spawn a worker that reads next available render task from queue and executes it
spawnRendererWorker :: ServerM ()
spawnRendererWorker = void . Immortal.createWithLabel "rendererWorker" $ const $ do
  $logDebug "Spawning renderer worker"
  work
  where
    work = do
      hasWork <- runQuery CheckNextRenderItem
      if hasWork then do
          mitem <- runUpdate FetchRenderItem
          case mitem of
            Nothing -> work
            Just item -> do
              let ri = renderId item
              $logInfo $ "Start rendering of " <> showt ri
              renderRes <- renderDocument item
              $logInfo $ "Finished rendering of " <> showt ri
              case renderRes of
                Left er -> $logWarn $ "Rendering failed with: " <> er
                Right _ -> $logInfo $ "Rendering finished successfully"
              registerNotification item renderRes
              work
        else sleep

    sleep = do
      chan <- asks envRenderChan
      _ <- liftIO . atomically $ readTChan chan
      work

-- | Render given item
renderDocument :: RenderItem -> ServerM (Either Text PDFContent)
renderDocument RenderItem{..} = do
  let template = maybe renderTemplate (\i -> renderTemplate { templateInput = Just i }) renderInput
  Sh.shelly $ Sh.handleany_sh (return . Left . pack . show) $
    Right <$> renderBundleToPDF template

-- | Convert rendering result to notification and save it
registerNotification :: RenderItem -> Either Text PDFContent -> ServerM ()
registerNotification RenderItem{..} res = do
  $logInfo $ "Registering notification for " <> showt renderId
  t <- liftIO getCurrentTime
  let notification = Notification {
          notifTarget = renderUrl
        , notifRenderId = renderId
        , notifDocument = res
        , notifTries = 0
        , notifLastError = Nothing
        , notifNextTry = t
        }
  runUpdate . AddNotification $ notification
  emitNotificationItem -- awake notification workers

-- | Spawn a worker that waits for notifications and tries to deliver them.
spawnNotificationWorker :: ServerM ()
spawnNotificationWorker = void . Immortal.createWithLabel "rendererWorker" $ const $ do
  $logDebug "Spawning notification worker"
  work
  where
    work = do
      $logDebug "Check pending notifications"
      t <- liftIO getCurrentTime
      hasWork <- runQuery $ CheckNextNotification t
      if hasWork then do
          $logDebug $ "Detected unprocessed notifications..."
          mitem <- runUpdate $ FetchNotification t
          case mitem of
            Nothing -> do
              $logDebug $ "But there is no work actually"
              work
            Just notification -> do
              mnotification <- deliverNotification notification
              whenJust mnotification $ runUpdate . AddNotification
              work
        else do
          $logDebug "No work for notification worker, sleep"
          sleep

    sleep = do
      chan <- asks envNotificationChan
      -- run thread that will awake the worker when next notification is ready
      mt <- runQuery GetNotificationNextTime
      whenJust mt $ \t -> do
        curTime <- liftIO getCurrentTime
        let dt = toMicroseconds $ diffUTCTime t curTime
        $logDebug $ "Next notification in " <> showt dt <> " ms"
        void . liftIO . forkIO $ do
          delay dt
          atomically $ writeTChan chan ()
      -- wait for new notifications
      _ <- liftIO . atomically $ readTChan chan
      work

-- | Try to send notification to client, if failed, delay notification for additional
-- try. If maximum count of tries is hit, the notification is deleted.
deliverNotification :: Notification -> ServerM (Maybe Notification)
deliverNotification n@Notification{..} = do
  $logInfo $ "Trying to deliver notification for " <> showt notifRenderId
  let body = APINotificationBody {
          apiNotificationId = toAPIRenderId . unRenderId $ notifRenderId
        , apiNotificationError = either Just (const Nothing) notifDocument
        , apiNotificationDocument = either (const Nothing) Just notifDocument
        }
  mng <- asks envManager
  res <- postNotification mng notifTarget body
  case res of
    Left (BadBaseUrl e) -> do
      $logError $ "Notification for " <> showt notifRenderId
        <> " is failed, wrong url " <> notifTarget <> ": " <> showt e
      return Nothing
    Left HasRedirectError -> do
      $logError $ "Notification for " <> showt notifRenderId
        <> " is failed, forbidden redirections"
      return Nothing
    Left (WrongSuccessStatus s) -> do
      $logWarn $ "Notification for " <> showt notifRenderId
        <> " is succeded, but returned strage status " <> showt s
      return Nothing
    Left (NotificationFail e) -> do
      $logWarn $ "Notification for " <> showt notifRenderId
        <> " is failed, reason: " <> showt e <> ", will retry later."
      ServerConfig{..} <- asks envConfig
      case (notifTries >=) <$> serverMaxNotificationTries of
        Just True -> do
          $logError $ "Notification for " <> showt notifRenderId
            <> " is not delivered! Maximum count of tries is hit."
          return Nothing
        _ -> do
          t <- liftIO getCurrentTime
          let n' = n {
                  notifTries = notifTries + 1
                , notifLastError = Just $ showt e
                , notifNextTry = serverNotificationDelay `addUTCTime` t
                }
          return $ Just n'
    Right () -> do
      $logInfo $ "Notification for " <> showt notifRenderId
        <> " is succeded!"
      return Nothing
