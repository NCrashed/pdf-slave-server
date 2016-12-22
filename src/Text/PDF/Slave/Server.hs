module Text.PDF.Slave.Server(
  -- * Server config
    ServerConfig(..)
  , readConfig
  -- * Server environment
  , ServerEnv
  , newServerEnv
  -- * Execution of server
  , pdfSlaveServerApp
  ) where

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Data.Aeson.WithField
import Data.Proxy
import Servant.Server

import qualified Data.UUID.V4 as UUID

import Text.PDF.Slave.Server.API
import Text.PDF.Slave.Server.Config
import Text.PDF.Slave.Server.DB
import Text.PDF.Slave.Server.Monad

-- | Full Server API
type ServerAPI = PDFSlaveAPI

-- | WAI application of server
pdfSlaveServerApp :: ServerEnv -> Application
pdfSlaveServerApp e = serve (Proxy :: Proxy ServerAPI) $ enter (serverMtoHandler e) $
  pdfSlaveServer

-- | Implementation of main server API
pdfSlaveServer :: ServerT PDFSlaveAPI ServerM
pdfSlaveServer = renderTemplateEndpoint

-- | Implementation of 'RenderTemplateEndpoint'
renderTemplateEndpoint :: RenderRequestBody -> ServerM (OnlyId RenderId)
renderTemplateEndpoint RenderRequestBody{..} = do
  ServerConfig{..} <- getConfig
  n <- runQuery GetRenderQueueSize
  whenJust serverMaximumQueue $ \n' -> unless (n < n') $
    throwError $ err507 { errBody = "Rendering queue is full"}
  i <- liftIO UUID.nextRandom
  runUpdate . AddRenderItem $ RenderItem {
      renderId       = i
    , renderTemplate = renderReqTemplate
    , renderInput    = renderReqInput
    }
  return $ OnlyField . toRenderId $ i

-- | Definition of 507 HTTP error `Insufficient Storage`
err507 :: ServantErr
err507 = ServantErr {
    errHTTPCode = 507
  , errReasonPhrase = "Insufficient Storage"
  , errBody = ""
  , errHeaders = []
  }