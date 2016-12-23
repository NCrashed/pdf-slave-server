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

import Control.Concurrent.STM.TChan
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
import Data.Time (getCurrentTime)
import Servant.Server

import Text.PDF.Slave
import Text.PDF.Slave.Server.Config
import Text.PDF.Slave.Server.DB
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
}

-- | Create new server environment
newServerEnv :: (MonadIO m, MonadBaseControl IO m)
  => ServerConfig -> m ServerEnv
newServerEnv cfg = do
  acid <- createDB (serverDatabaseConf cfg)
  renderChan <- liftIO newTChanIO
  let env = ServerEnv {
        envConfig = cfg
      , envDB = acid
      , envRenderChan = renderChan
      }
  liftIO . runServerMIO env $ replicateM_ (serverRenderWorkers cfg) spawnRendererWorker
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

-- | Awake sleeping workers about that we have a work to do
emitRenderItem :: ServerM ()
emitRenderItem = do
  chan <- asks envRenderChan
  liftIO . atomically $ writeTChan chan ()

-- | Spawn a worker that reads next available render task from queue and executes it
spawnRendererWorker :: ServerM ()
spawnRendererWorker = void . Immortal.createWithLabel "rendererWorker" $ const work
  where
    work = do
      hasWork <- runQuery CheckNextRenderItem
      if hasWork then do
          mitem <- runUpdate FetchRenderItem
          case mitem of
            Nothing -> work
            Just item -> do
              renderRes <- renderDocument item
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