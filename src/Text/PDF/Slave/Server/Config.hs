-- | Configuration file of server
module Text.PDF.Slave.Server.Config(
    ServerConfig(..)
  , DatabaseConfig(..)
  , readConfig
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text
import Data.Time
import Data.Yaml
import Data.Yaml.Config
import GHC.Generics

-- | Configuration for database connection
data DatabaseConfig = DatabaseConfig {
  -- | Path to acid-state storage folder
  databasePath           :: !Text
  -- | How often to make a checkpoint
, databaseCheckpointTime :: !NominalDiffTime
  -- | How often to make a archive
, databaseArchiveTime    :: !NominalDiffTime
} deriving (Generic, Show)

-- | Helper to parse nominal diff time with default value
parseNominalDiff :: Object -> Text -> Parser NominalDiffTime
parseNominalDiff o label = do
    t <- o .: label
    return $ realToFrac (t :: Double)

instance FromJSON DatabaseConfig where
  parseJSON (Object o) = do
    databasePath <- o .: "path"
    databaseCheckpointTime <- parseNominalDiff o "checkpoint-time"
    databaseArchiveTime <- parseNominalDiff o "archive-time"
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