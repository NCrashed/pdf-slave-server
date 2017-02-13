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
import Servant.Server.Auth.Token.Config

-- | Configuration for database connection
data DatabaseConfig = DatabaseConfig {
  -- | Path to acid-state storage folder
  databasePath           :: !Text
  -- | How often to make a checkpoint
, databaseCheckpointTime :: !(Maybe NominalDiffTime)
  -- | How often to make a archive
, databaseArchiveTime    :: !(Maybe NominalDiffTime)
} deriving (Generic, Show)

-- | Helper to parse nominal diff time with default value
parseNominalDiff :: Object -> Text -> Parser (Maybe NominalDiffTime)
parseNominalDiff o label = do
    t :: Maybe Double <- o .:? label
    return $ fmap realToFrac t

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
  serverHost                 :: !Text
  -- | Server port number
, serverPort                 :: !Int
  -- | If set, HTTP log would be more readable
, serverDetailedLogging      :: !Bool
  -- | Database connection options
, serverDatabaseConf         :: !DatabaseConfig
  -- | Maximum allowed queue of templates to render
, serverMaximumQueue         :: !(Maybe Int)
  -- | Number of rendering workers
, serverRenderWorkers        :: !Int
  -- | Number of notification workers
, serverNotificationWorkers  :: !Int
  -- | Delays between notification tries
, serverNotificationDelay    :: !NominalDiffTime
  -- | Maximum number of failed notification delivers
, serverMaxNotificationTries :: !(Maybe Int)
  -- | Fixed password for admin account
, serverAdminPassword        :: !Text
  -- | Server authorisation config
, serverAuthConfig           :: !AuthConfig
} deriving (Generic)

-- | Prepare 'AuthConfig' from JSON object
parseAuthConfig :: Value -> Parser AuthConfig
parseAuthConfig (Object o) = do
  tokenExpire <- o .: "token-expire"
  strength <- o .: "password-strength"
  maxExpire <- o .: "maximum-expire"
  return defaultAuthConfig {
      defaultExpire = tokenExpire
    , passwordsStrength = strength
    , maximumExpire = maxExpire
    }
parseAuthConfig _ = mzero

instance FromJSON ServerConfig where
  parseJSON v@(Object o) = ServerConfig
    <$> o .:  "host"
    <*> o .:  "port"
    <*> o .:  "detailed-logging"
    <*> o .:  "database"
    <*> o .:? "max-queue-size"
    <*> o .:  "render-workers"
    <*> o .:  "notification-workers"
    <*> o .:  "notification-delay"
    <*> o .:? "notification-tries"
    <*> o .:  "admin-password"
    <*> parseAuthConfig v
  parseJSON _ = mzero

readConfig :: MonadIO m => FilePath -> m ServerConfig
readConfig f = liftIO $ loadYamlSettings [f] [] useEnv
