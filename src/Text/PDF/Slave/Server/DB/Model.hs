{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.PDF.Slave.Server.DB.Model where

import Control.Lens
import Data.Acid
import Data.Aeson (Value)
import Data.Hashable
import Data.Ord
import Data.SafeCopy
import Data.Scientific
import Data.Sequence
import Data.Text
import Data.Time
import Data.UUID
import GHC.Generics
import Text.PDF.Slave

import qualified Data.Sequence as S
import qualified Data.HashMap.Strict as H

-- | Wrapper around UUID
newtype RenderId = RenderId { unRenderId :: UUID }
  deriving (Generic, Show, Eq)

deriveSafeCopy 0 'base ''UUID
deriveSafeCopy 0 'base ''RenderId

-- | Element of rendering queue
data RenderItem = RenderItem {
  renderId       :: RenderId
  -- | Template to render
, renderTemplate :: Template
  -- | Optional input data
, renderInput    :: Maybe Value
  -- | Notification URL for positing results
, renderUrl      :: Text
} deriving (Generic)

deriveSafeCopy 0 'base ''RenderItem
deriveSafeCopy 0 'base ''Template
deriveSafeCopy 0 'base ''TemplateDependency

-- | Notification about finished rendering
data Notification = Notification {
  -- | Corresponding rendering task id
  notifRenderId  :: RenderId
  -- | URL to send notification to
, notifTarget    :: Text
  -- | Content of document or rendering error
, notifDocument  :: Either Text PDFContent
  -- | Number of tries to send the document
, notifTries     :: Int
  -- | If last notification sending failed the field stores error message
, notifLastError :: Maybe Text
  -- | After the time the notification should be tried to be delivered
, notifNextTry   :: UTCTime
} deriving (Generic, Show)

deriveSafeCopy 0 'base ''Notification

-- | DB persistent model
data Model = Model {
  -- | Rendering queue
  _modelRenderQueue :: Seq RenderItem
  -- | Notification queue
, _modelNotificationQueue :: Seq Notification
}

makeLenses ''Model
deriveSafeCopy 0 'base ''Model

-- | Initialisation value of empty DB
initialModel :: Model
initialModel = Model {
    _modelRenderQueue = mempty
  , _modelNotificationQueue = mempty
  }

-- | Register new item in queue
addQueueItem :: Setter' Model (Seq a) -> a -> Update Model ()
addQueueItem queueField i = queueField %= (\q -> q S.|> i)

-- | Check if queue is not empty
checkNextQueueItem :: Getter Model (Seq a) -> Query Model Bool
checkNextQueueItem queueField = do
  q <- view queueField
  return $ case S.viewl q of
    EmptyL -> False
    _      -> True

-- | Get current size of queue
getQueueSize :: Getter Model (Seq a) -> Query Model Int
getQueueSize queueField = S.length <$> view queueField

-- | Get next item from rendering queue
fetchQueueItem :: Lens' Model (Seq a) -> Update Model (Maybe a)
fetchQueueItem queueField = do
  q <- use queueField
  let (mi, qleft) = case S.viewl q of
        EmptyL       -> (Nothing, S.empty)
        i S.:< is -> (Just i, is)
  queueField .= qleft
  return mi

-- | Register new render item in queue
addRenderItem :: RenderItem -> Update Model ()
addRenderItem = addQueueItem modelRenderQueue

-- | Check if queue is not empty
checkNextRenderItem :: Query Model Bool
checkNextRenderItem = checkNextQueueItem modelRenderQueue

-- | Get current size of render queue
getRenderQueueSize :: Query Model Int
getRenderQueueSize = getQueueSize modelRenderQueue

-- | Get next item from rendering queue
fetchRenderItem :: Update Model (Maybe RenderItem)
fetchRenderItem = fetchQueueItem modelRenderQueue

-- | Register new notification item in queue
addNotification :: Notification -> Update Model ()
addNotification = addQueueItem modelNotificationQueue

-- | Check if notification queue is not empty
checkNextNotification :: UTCTime -- ^ Fetch only those whom 'notifNextTry' is less than the value
  -> Query Model Bool
checkNextNotification t = do
  q <- view modelNotificationQueue
  return $ case S.viewl . S.filter ((t >=) . notifNextTry) $ q of
    EmptyL -> False
    _      -> True

-- | Get current size of notification queue
getNotificationQueueSize :: Query Model Int
getNotificationQueueSize = getQueueSize modelNotificationQueue

-- | Get next item from notification queue with minimum expected deliver time
fetchNotification :: UTCTime -- ^ Fetch only those whom 'notifNextTry' is less than the value
  -> Update Model (Maybe Notification)
fetchNotification t = do
  q <- use modelNotificationQueue
  -- first, remove notifications whose time have not come yet and sort in ascending order
  let q' = S.sortBy (comparing notifNextTry) . S.filter ((t >=) . notifNextTry) $ q
      (mi, qleft) = case S.viewl q' of
        EmptyL    -> (Nothing, S.empty)
        i S.:< is -> (Just i, is)
  modelNotificationQueue .= qleft
  return mi

-- | Get time when next notification should be sended to client
getNotificationNextTime :: Query Model (Maybe UTCTime)
getNotificationNextTime = do
  q <- view modelNotificationQueue
  return $ case S.viewl . S.sort . fmap notifNextTry $ q of
    EmptyL   -> Nothing
    t S.:< _ -> Just t

makeAcidic ''Model [
    'addRenderItem
  , 'checkNextRenderItem
  , 'fetchRenderItem
  , 'getRenderQueueSize
  , 'addNotification
  , 'checkNextNotification
  , 'getNotificationQueueSize
  , 'fetchNotification
  , 'getNotificationNextTime
  ]

-- | ACID for aeson

deriveSafeCopy 0 'base ''Value
deriveSafeCopy 0 'base ''Scientific

-- | An instance of SafeCopy for the Object Value.
instance (SafeCopy a, Eq a, Hashable a, SafeCopy b) => SafeCopy (H.HashMap a b) where
    getCopy = contain $ fmap H.fromList safeGet
    putCopy = contain . safePut . H.toList