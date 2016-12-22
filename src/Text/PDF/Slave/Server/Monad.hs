module Text.PDF.Slave.Server.Monad(
  -- * Monad
    ServerM
  , runServerM
  , serverMtoHandler
  -- ** Monad environment
  , ServerEnv(..)
  , newServerEnv
  -- * Utilities
  , module Text.PDF.Slave.Server.Util
  ) where

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Servant.Server

import Text.PDF.Slave.Server.Config
import Text.PDF.Slave.Server.DB
import Text.PDF.Slave.Server.Util

-- | Server private environment
data ServerEnv = ServerEnv {
  envConfig :: ServerConfig    -- ^ Configuration used to create the server
, envDB     :: AcidState Model -- ^ Server DB
}

-- | Create new server environment
newServerEnv :: (MonadIO m, MonadBaseControl IO m)
  => ServerConfig -> m ServerEnv
newServerEnv cfg = do
  acid <- createDB (serverDatabaseConf cfg)
  return ServerEnv {
      envConfig = cfg
    , envDB = acid
    }

-- | Server monad that holds internal environment
newtype ServerM a = ServerM { unServerM :: ReaderT ServerEnv (LoggingT Handler) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader ServerEnv, MonadLogger, MonadLoggerIO)

-- | Execution of 'ServerM'
runServerM :: ServerEnv -> ServerM a -> Handler a
runServerM e = runStdoutLoggingT . flip runReaderT e . unServerM

-- | Transformation to Servant 'Handler'
serverMtoHandler :: ServerEnv -> ServerM :~> Handler
serverMtoHandler e = Nat (runServerM e)
