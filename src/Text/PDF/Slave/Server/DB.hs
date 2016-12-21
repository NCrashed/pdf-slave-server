module Text.PDF.Slave.Server.DB(
    ConnectionPool
  , HasConnectionPool(..)
  , createDBPool
  , runDB
  , runMigrations
  ) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.Monoid
import Data.Text (pack)
import Data.Text.Encoding
import Database.Persist.Postgresql

import Text.PDF.Slave.Server.Config
import Text.PDF.Slave.Server.DB.Model

-- | Create connection pool for DB
createDBPool :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
  => DatabaseConfig -- ^ Connection configuration
  -> m ConnectionPool
createDBPool DatabaseConfig{..} = createPostgresqlPool connstr databasePoolSize
  where
  connstr = encodeUtf8 $
        "host="     <> databaseHost
    <> " port="     <> (pack . show $ databasePort)
    <> " user="     <> databaseUser
    <> " password=" <> databasePassword
    <> " dbname="   <> databaseDatabase

-- | We can run DB queries for any monad that has a connection pool
class HasConnectionPool m where
  getConnectionPool :: m ConnectionPool

-- | Lift DB computation into internal monad
runDB :: (HasConnectionPool m, MonadIO m) => SqlPersistT IO a -> m a
runDB ma = do
  pool <- getConnectionPool
  liftIO $ runSqlPool ma pool

-- | Execute DB migrations
runMigrations :: MonadIO m => ConnectionPool -> m ()
runMigrations pool = liftIO $ runSqlPool (runMigration migrateAll) pool
