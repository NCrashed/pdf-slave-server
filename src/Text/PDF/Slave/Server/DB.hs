module Text.PDF.Slave.Server.DB(
    Model
  , AcidState
  , createDB
  ) where

import Control.Monad.IO.Class
import Data.Acid
import Data.Text (unpack)

import Text.PDF.Slave.Server.Config
import Text.PDF.Slave.Server.DB.Model

-- | Create connection pool for DB
createDB :: (MonadIO m)
  => DatabaseConfig -- ^ Connection configuration
  -> m (AcidState Model)
createDB DatabaseConfig{..} = liftIO $ openLocalStateFrom (unpack databasePath) initialModel

