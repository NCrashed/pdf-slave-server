{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.PDF.Slave.Server.DB.Model where

import Control.Monad.State
import Data.Acid
import Data.Aeson
import Data.Hashable
import Data.SafeCopy
import Data.Scientific
import Data.Sequence
import Data.Text
import GHC.Generics
import Text.PDF.Slave

import qualified Data.Sequence as S
import qualified Data.HashMap.Strict as H

-- | Element of rendering queue
data RenderItem = RenderItem {
  -- | Template to render
  renderTemplate :: Template
  -- | Optional input data
, renderInput    :: Maybe Value
} deriving (Generic)

deriveSafeCopy 0 'base ''TemplateDependency
deriveSafeCopy 0 'base ''Template
deriveSafeCopy 0 'base ''RenderItem

-- | Notification about finished rendering
data Notification = Notification {
  -- | URL to send notification to
  notifTarget   :: Text
  -- | Content of document
, notifDocument :: PDFContent
  -- | Number of tries to send the document
, notifTries    :: Int
}

deriveSafeCopy 0 'base ''Notification

-- | DB persistent model
data Model = Model {
  -- | Rendering queue
  modelRenderQueue :: Seq RenderItem
  -- | Notification queue
, modelNotifications :: Seq Notification
}

deriveSafeCopy 0 'base ''Model

-- | Initialisation value of empty DB
initialModel :: Model
initialModel = Model {
    modelRenderQueue = mempty
  , modelNotifications = mempty
  }

-- | Register new render item in queue
addRenderItem :: RenderItem -> Update Model ()
addRenderItem i = modify $ \model -> model {
    modelRenderQueue = modelRenderQueue model S.|> i
  }

-- | Get next item from rendering queue
fetchRenderItem :: Update Model (Maybe RenderItem)
fetchRenderItem = do
  model <- get
  let (mi, qleft) = case S.viewl $ modelRenderQueue model of
        EmptyL       -> (Nothing, S.empty)
        i S.:< is -> (Just i, is)
  put $ model {
      modelRenderQueue = qleft
    }
  return mi

makeAcidic ''Model ['fetchRenderItem, 'addRenderItem]

-- | ACID for aeson

deriveSafeCopy 0 'base ''Value
deriveSafeCopy 0 'base ''Scientific

-- | An instance of SafeCopy for the Object Value.
instance (SafeCopy a, Eq a, Hashable a, SafeCopy b) => SafeCopy (H.HashMap a b) where
    getCopy = contain $ fmap H.fromList safeGet
    putCopy = contain . safePut . H.toList