module Text.PDF.Slave.Server.API(
    PDFSlaveAPI
  , PDFSlaveAPIVersion
  , RenderTemplateEndpoint
  , APIRenderId
  , fromAPIRenderId
  , toAPIRenderId
  , APIRenderBody(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.WithField
import Data.Monoid
import Data.Text
import Data.UUID (UUID)
import GHC.Generics
import Servant.API
import Text.PDF.Slave.Template (Template)

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

-- | Add template with input to rendering queue
type RenderTemplateEndpoint = "template" :> "render"
  :> ReqBody '[JSON] APIRenderBody
  :> Post '[JSON] (OnlyId APIRenderId)

-- | API of PDF slave server
type PDFSlaveAPI = PDFSlaveAPIVersion :> (
    RenderTemplateEndpoint
  )
