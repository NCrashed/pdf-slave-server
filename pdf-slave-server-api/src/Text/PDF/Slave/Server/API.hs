module Text.PDF.Slave.Server.API(
    PDFSlaveAPI
  , PDFSlaveAPIVersion
  , RenderTemplateEndpoint
  , RenderId
  , fromRenderId
  , toRenderId
  , RenderRequestBody(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.WithField
import Data.Monoid
import Data.Scientific (toBoundedInteger)
import Data.Word
import GHC.Generics
import Servant.API
import Text.PDF.Slave.Template (Template)

-- | Current version of API
type PDFSlaveAPIVersion = "v1.0"

-- | ID of item in rendering queue
newtype RenderId = RenderId { unRenderId :: Word64 }
  deriving (Generic, Show, Eq)

instance FromJSON RenderId where
  parseJSON (Number n) = case toBoundedInteger n of
    Nothing -> fail $ "Cannot convert too large numeric " <> show n <> " to RenderId"
    Just i -> return . RenderId $ i
  parseJSON _ = mzero

instance ToJSON RenderId where
  toJSON (RenderId n) = Number . fromIntegral $ n

-- | Conversion from render id
fromRenderId :: Integral a => RenderId -> a
fromRenderId = fromIntegral . unRenderId

-- | Conversion to render id
toRenderId :: Integral a => a -> RenderId
toRenderId = RenderId . fromIntegral

-- | Body of 'RenderTemplateEndpoint'
data RenderRequestBody = RenderRequestBody {
  renderReqTemplate :: Template -- ^ Template bundle
, renderReqInput    :: Maybe Value -- ^ Optional input for template
} deriving (Generic, Show)

instance FromJSON RenderRequestBody where
  parseJSON (Object o) = RenderRequestBody
    <$> o .: "template"
    <*> o .:? "input"
  parseJSON _ = mzero

instance ToJSON RenderRequestBody where
  toJSON RenderRequestBody{..} = object [
      "template" .= renderReqTemplate
    , "input"    .= renderReqInput
    ]

-- | Add template with input to rendering queue
type RenderTemplateEndpoint = "template" :> "render"
  :> ReqBody '[JSON] RenderRequestBody
  :> Post '[JSON] (OnlyId RenderId)

-- | API of PDF slave server
type PDFSlaveAPI = PDFSlaveAPIVersion :> (
    RenderTemplateEndpoint
  )
