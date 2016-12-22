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
import Data.UUID (UUID)
import GHC.Generics
import Servant.API
import Text.PDF.Slave.Template (Template)

import qualified Data.UUID as UUID

-- | Current version of API
type PDFSlaveAPIVersion = "v1.0"

-- | ID of item in rendering queue
newtype RenderId = RenderId { unRenderId :: UUID }
  deriving (Generic, Show, Eq)

instance FromJSON RenderId where
  parseJSON (String s) = case UUID.fromText s of
    Nothing -> fail $ "Cannot parse render id " <> show s
    Just i -> return . RenderId $ i
  parseJSON _ = mzero

instance ToJSON RenderId where
  toJSON (RenderId n) = String . UUID.toText $ n

-- | Conversion from render id
fromRenderId :: RenderId -> UUID
fromRenderId = unRenderId

-- | Conversion to render id
toRenderId :: UUID -> RenderId
toRenderId = RenderId

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
