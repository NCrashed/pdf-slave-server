module Text.PDF.Slave.Server.DB(
    Model
  , AcidState
  , createDB
  -- * Persistent entities
  , RenderItem(..)
  , Notification(..)
  -- * Queries
  , AddRenderItem(..)
  , CheckNextRenderItem(..)
  , FetchRenderItem(..)
  ) where

import Control.Concurrent.Thread.Delay
import Control.Monad (void, forever)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Acid
import Data.Text (unpack)
import Data.Time

import Text.PDF.Slave.Server.Config
import Text.PDF.Slave.Server.DB.Model
import Text.PDF.Slave.Server.Util

import qualified Control.Immortal as Immortal

-- | Create connection pool for DB
createDB :: (MonadIO m, MonadBaseControl IO m)
  => DatabaseConfig -- ^ Connection configuration
  -> m (AcidState Model)
createDB DatabaseConfig{..} = do
  db <- liftIO $ openLocalStateFrom (unpack databasePath) initialModel
  whenJust databaseCheckpointTime $ \dt -> checkpointWorker dt db
  whenJust databaseArchiveTime $ \dt -> archiveWorker dt db
  return db

-- | Convert time internval to count of microseconds
toMicroseconds :: NominalDiffTime -> Integer
toMicroseconds dt = round $ (realToFrac dt :: Rational) * 1000000

-- | Worker thread that creates DB checkpoints in given interval
checkpointWorker :: (MonadIO m, MonadBaseControl IO m)
  => NominalDiffTime -- ^ Time interval of creation of checkpoints
  -> AcidState Model -- ^ DB to make checkpoint from
  -> m ()
checkpointWorker dt db = liftIO . void . Immortal.createWithLabel "checkpointWorker" $
  const $ forever $ do
    delay . toMicroseconds $ dt
    createCheckpoint db

-- | Worker thread that creates archives from DB in given interval
archiveWorker :: (MonadIO m, MonadBaseControl IO m)
  => NominalDiffTime -- ^ Time interval of creation of checkpoints
  -> AcidState Model -- ^ DB to make checkpoint from
  -> m ()
archiveWorker dt db = liftIO . void . Immortal.createWithLabel "archiveWorker" $
  const $ forever $ do
    delay . toMicroseconds $ dt
    createArchive db