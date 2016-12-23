module Text.PDF.Slave.Server.DB(
    Model
  , AcidState
  , createDB
  -- * Execution DB queries
  , HasAcidState(..)
  , runQuery
  , runUpdate
  -- * Persistent entities
  , RenderId(..)
  , RenderItem(..)
  , Notification(..)
  -- * Queries
  -- ** Render queue
  , AddRenderItem(..)
  , GetRenderQueueSize(..)
  , CheckNextRenderItem(..)
  , FetchRenderItem(..)
  -- ** Notification queue
  , AddNotification(..)
  , GetNotificationQueueSize(..)
  , CheckNextNotification(..)
  , FetchNotification(..)
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

-- | Describes monads that has internal acid-state storage
class HasAcidState a m | m -> a where
  getAcidState :: m (AcidState a)

-- | Run acid-state query in monad with internal acid storage
runQuery :: (MonadIO m, HasAcidState (EventState event) m, QueryEvent event)
  => event -- ^ Event constructed from query with 'makeAcidic'
  -> m (EventResult event)
runQuery e = do
  db <- getAcidState
  liftIO $ query db e

-- | Run acid-state update query in monad with internal acid storage
runUpdate :: (MonadIO m, HasAcidState (EventState event) m, UpdateEvent event)
  => event -- ^ Event constructed from query with 'makeAcidic'
  -> m (EventResult event)
runUpdate e = do
  db <- getAcidState
  liftIO $ update db e

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