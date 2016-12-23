module Text.PDF.Slave.Server.API(
    PDFSlaveAPI
  , PDFSlaveAPIVersion
  , RenderTemplateEndpoint
  , APIRenderId
  , fromAPIRenderId
  , toAPIRenderId
  , APIRenderBody(..)
  , APINotificationBody(..)
  -- * Reexports
  , OnlyField(..)
  , OnlyId
  , Template
  ) where

import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.WithField
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.UUID (UUID)
import GHC.Generics
import Servant.API
import Text.PDF.Slave.Template (Template)

import qualified Data.ByteString.Base64 as B64
import qualified Data.UUID as UUID

-- | Current version of API
type PDFSlaveAPIVersion = "v1.0"

-- | ID of item in rendering queue
newtype APIRenderId = APIRenderId { unAPIRenderId :: UUID }
  deriving (Generic, Show, Eq)

instance FromJSON APIRenderId where
  parseJSON (String s) = case UUID.fromText s of
    Nothing -> fail $ "Cannot parse render id " <> show s
    Just i -> return . APIRenderId $ i
  parseJSON _ = mzero

instance ToJSON APIRenderId where
  toJSON (APIRenderId n) = String . UUID.toText $ n

-- | Conversion from render id
fromAPIRenderId :: APIRenderId -> UUID
fromAPIRenderId = unAPIRenderId

-- | Conversion to render id
toAPIRenderId :: UUID -> APIRenderId
toAPIRenderId = APIRenderId

-- | Body of 'RenderTemplateEndpoint'
data APIRenderBody = APIRenderBody {
  apiRenderBodyTemplate :: Template -- ^ Template bundle
, apiRenderBodyInput    :: Maybe Value -- ^ Optional input for template
, apiRenderBodyUrl      :: Text -- ^ URL where to post results of rendering
} deriving (Generic, Show)

instance FromJSON APIRenderBody where
  parseJSON (Object o) = APIRenderBody
    <$> o .: "template"
    <*> o .:? "input"
    <*> o .: "url"
  parseJSON _ = mzero

instance ToJSON APIRenderBody where
  toJSON APIRenderBody{..} = object [
      "template" .= apiRenderBodyTemplate
    , "input"    .= apiRenderBodyInput
    , "url"      .= apiRenderBodyUrl
    ]

-- | Notification format that is posted by the server to user supplied URL
data APINotificationBody = APINotificationBody {
  -- | ID of rendering item
  apiNotificationId       :: APIRenderId
  -- | Persists if rendering is failed
, apiNotificationError    :: Maybe Text
  -- | Persists if rendering is finished successfully. Base64 encoding.
, apiNotificationDocument :: Maybe ByteString
} deriving (Generic, Show)

instance FromJSON APINotificationBody where
  parseJSON (Object o) = do
    mbs <- o .:? "document"
    doc <- case mbs of
      Nothing -> return Nothing
      Just bs -> case B64.decode . encodeUtf8 $ bs of
        Left e  -> fail $ "Cannot decode document content: " <> e
        Right a -> return $ Just a
    APINotificationBody
      <$> o .: "id"
      <*> o .:? "error"
      <*> pure doc
  parseJSON _ = mzero

instance ToJSON APINotificationBody where
  toJSON APINotificationBody{..} = object [
      "id"       .= apiNotificationId
    , "error"    .= apiNotificationError
    , "document" .= fmap (decodeUtf8 . B64.encode) apiNotificationDocument
    ]

-- | Add template with input to rendering queue
type RenderTemplateEndpoint = "template" :> "render"
  :> ReqBody '[JSON] APIRenderBody
  :> Post '[JSON] (OnlyId APIRenderId)

-- | API of PDF slave server
type PDFSlaveAPI = PDFSlaveAPIVersion :> (
    RenderTemplateEndpoint
  )
