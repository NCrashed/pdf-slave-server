module Text.PDF.Slave.Server.Monad(
  -- * Monad
    ServerM
  , runServerM
  , serverMtoHandler
  -- ** Monad environment
  , ServerEnv(..)
  , newServerEnv
  -- * Utilities
  , ffor
  , whenJust
  , showl
  , showt
  ) where

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Text
import Servant.Server

import Text.PDF.Slave.Server.Config
import Text.PDF.Slave.Server.DB

-- | Server private environment
data ServerEnv = ServerEnv {
  envConfig :: ServerConfig -- ^ Configuration used to create the server
, envPool   :: ConnectionPool -- ^ Connection pool to DB
}

-- | Create new server environment
newServerEnv :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
  => ServerConfig -> m ServerEnv
newServerEnv cfg = do
  pool <- createDBPool (serverDatabaseConf cfg)
  return ServerEnv {
      envConfig = cfg
    , envPool = pool
    }

-- | Server monad that holds internal environment
newtype ServerM a = ServerM { unServerM :: ReaderT ServerEnv (LoggingT Handler) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader ServerEnv, MonadLogger, MonadLoggerIO)

instance HasConnectionPool ServerM where
  getConnectionPool = asks envPool

-- | Execution of 'ServerM'
runServerM :: ServerEnv -> ServerM a -> Handler a
runServerM e = runStdoutLoggingT . flip runReaderT e . unServerM

-- | Transformation to Servant 'Handler'
serverMtoHandler :: ServerEnv -> ServerM :~> Handler
serverMtoHandler e = Nat (runServerM e)

-- | Fliped fmap
ffor :: Functor f => f a -> (a -> b) -> f b
ffor = flip fmap

-- | Execute applicative action only when the value is 'Just'
whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust (Just a) m = m a
whenJust Nothing _ = pure ()

-- | Convert anything to log string
showl :: Show a => a -> LogStr
showl = toLogStr . show

-- | Convert anything to log string
showt :: Show a => a -> Text
showt = pack . show