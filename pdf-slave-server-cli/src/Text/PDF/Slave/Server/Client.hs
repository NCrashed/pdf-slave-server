module Text.PDF.Slave.Server.Client(
    PDFSlaveClientConfig(..)
  , PDFSlaveClientM
  , runPDFSlaveClientM
  , renderTemplate
  , withAuth
  , authGetToken
  , APIRenderId
  , fromAPIRenderId
  , toAPIRenderId
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson (Value)
import Data.Aeson.WithField
import Data.Aeson.Unit
import Data.IORef
import Data.Proxy
import Data.Text (Text, unpack)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS
import Servant.API
import Servant.API.Auth.Token
import Servant.Client
import Text.PDF.Slave.Server.API

-- | Client configuration
data PDFSlaveClientConfig = PDFSlaveClientConfig {
  -- | Connection URL
  pdfSlaveUrl :: Text
  -- | Where to post notification
, pdfSlaveReturnUrl :: Text
  -- | Optional connection manager
, pdfSlaveManager :: Maybe Manager
}

-- | Environment of 'PDFSlaveClientM' monad
data PDFSlaveClientEnv = PDFSlaveClientEnv {
  envReturnUrl :: Text
, envTokenRef  :: IORef (MToken' '["render"])
}

-- | Wrapper for client monad
newtype PDFSlaveClientM a = PDFSlaveClientM { unPDFSlaveClientM :: ReaderT PDFSlaveClientEnv ClientM a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch
    , MonadReader PDFSlaveClientEnv)

-- | Execute client actions
runPDFSlaveClientM :: (MonadIO m, MonadThrow m)
  => PDFSlaveClientConfig -- ^ Configuration of client
  -> PDFSlaveClientM a -- ^ Client action
  -> m (Either ServantError a)
runPDFSlaveClientM PDFSlaveClientConfig{..} m = do
  baseUrl <- parseBaseUrl (unpack pdfSlaveUrl)
  ref <- liftIO $ newIORef Nothing
  mng <- case pdfSlaveManager of
    Nothing -> liftIO getGlobalManager
    Just mng -> return mng
  let env = PDFSlaveClientEnv {
          envReturnUrl = pdfSlaveReturnUrl
        , envTokenRef  = ref
        }
  liftIO $ runClientM (runReaderT (unPDFSlaveClientM m) env) (ClientEnv mng baseUrl)

-- | Lifting 'ClientM' actions into 'PDFSlaveClientM' monad
liftClientM :: ClientM a -> PDFSlaveClientM a
liftClientM = PDFSlaveClientM . lift

-- | Send template bundle with input for rendering
renderTemplate :: Template -- ^ Template bundle to render
  -> Maybe APIRenderId -- ^ Optional id overwrite
  -> Maybe Value -- ^ Optional value
  -> PDFSlaveClientM APIRenderId
renderTemplate t mid minput = do
  PDFSlaveClientEnv{..} <- ask
  mtoken <- liftIO $ readIORef envTokenRef
  OnlyField i <- liftClientM $ renderTemplateEndpoint APIRenderBody {
      apiRenderBodyId = mid
    , apiRenderBodyTemplate = t
    , apiRenderBodyInput = minput
    , apiRenderBodyUrl = envReturnUrl
    } mtoken
  return i

-- | Sign in the server and cache authentification token
authSignin :: Login -> Password -> Seconds -> PDFSlaveClientM ()
authSignin login password expire = do
  OnlyField token <- liftClientM $ authSigninMethod (Just login) (Just password) (Just expire)
  PDFSlaveClientEnv{..} <- ask
  liftIO $ writeIORef envTokenRef (Just $ Token token)

-- | Signouts from server
authSignout :: PDFSlaveClientM ()
authSignout = do
  PDFSlaveClientEnv{..} <- ask
  mtoken <- liftIO $ readIORef envTokenRef
  case mtoken of
    Nothing -> return ()
    Just token -> void . liftClientM $ authSignoutMethod (Just $ downgradeToken' token)

-- | Get authorisation token of current session if any
authGetToken :: PDFSlaveClientM (Maybe SimpleToken)
authGetToken = do
  PDFSlaveClientEnv{..} <- ask
  fmap unToken <$> liftIO (readIORef envTokenRef)

-- | Run given scope with authorisation
withAuth :: Login -> Password -> Seconds -> PDFSlaveClientM a -> PDFSlaveClientM a
withAuth login password expire m = do
  authSignin login password expire
  a <- m
  authSignout
  return a

-- | Client API is subset of full API of server
type ClientAPI = PDFSlaveAPI
  :<|> AuthSigninMethod
  :<|> AuthSignoutMethod

renderTemplateEndpoint :: APIRenderBody -> MToken' '["render"] -> ClientM (OnlyId APIRenderId)
authSigninMethod :: Maybe Login -> Maybe Password -> Maybe Seconds -> ClientM (OnlyField "token" SimpleToken)
authSignoutMethod :: MToken' '[] -> ClientM Unit
(      renderTemplateEndpoint
  :<|> authSigninMethod
  :<|> authSignoutMethod
  ) = client (Proxy :: Proxy ClientAPI)
