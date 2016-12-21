-- | Configuration file of server
module Text.PDF.Slave.Server.Config(
    ServerConfig(..)
  , DatabaseConfig(..)
  , readConfig
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text
import Data.Yaml
import Data.Yaml.Config
import GHC.Generics

-- | Configuration for database connection
data DatabaseConfig = DatabaseConfig {
  -- | Name of a DB
  databaseDatabase    :: !Text
  -- | Host where a DB listens for connections
, databaseHost        :: !Text
  -- | Port where a DB listens for connections
, databasePort        :: !Int
  -- | DB authorisation user
, databaseUser        :: !Text
  -- | DB authorisation password
, databasePassword    :: !Text
  -- | Number of preallocated connections in pool (for each sub-pool)
, databasePoolSize    :: !Int
} deriving (Generic, Show)

instance FromJSON DatabaseConfig where
  parseJSON (Object o) = do
    databaseDatabase <- o .: "database"
    databaseHost <- o .: "host"
    databasePort <- o .: "port"
    databaseUser <- o .: "user"
    databasePassword <- o .: "password"
    databasePoolSize <- o .: "poolSize"
    return DatabaseConfig{..}
  parseJSON _ = mzero

-- | Startup configuration of server
data ServerConfig = ServerConfig {
  -- | Server host name
  serverHost             :: !Text
  -- | Server port number
, serverPort             :: !Int
  -- | If set, HTTP log would be more readable
, serverDetailedLogging  :: !Bool
  -- | Database connection options
, serverDatabaseConf     :: !DatabaseConfig
} deriving (Generic, Show)

instance FromJSON ServerConfig where
  parseJSON (Object o) = ServerConfig
    <$> o .: "host"
    <*> o .: "port"
    <*> o .: "detailed-logging"
    <*> o .: "database"
  parseJSON _ = mzero

readConfig :: MonadIO m => FilePath -> m ServerConfig
readConfig f = liftIO $ loadYamlSettings [f] [] useEnv