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
import Servant.API.Auth.Token
import Servant.Server
import Servant.Server.Auth.Token

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
renderTemplateEndpoint :: APIRenderBody
  -> MToken' '["render"]
  -> ServerM (OnlyId APIRenderId)
renderTemplateEndpoint APIRenderBody{..} token = do
  runAuth $ guardAuthToken token
  ServerConfig{..} <- getConfig
  n <- runQuery GetRenderQueueSize
  whenJust serverMaximumQueue $ \n' -> unless (n < n') $
    throwError $ err507 { errBody = "Rendering queue is full"}
  i <- maybe (liftIO UUID.nextRandom) (return . fromAPIRenderId) apiRenderBodyId
  runUpdate . AddRenderItem $ RenderItem {
      renderId       = RenderId i
    , renderTemplate = apiRenderBodyTemplate
    , renderInput    = apiRenderBodyInput
    , renderUrl      = apiRenderBodyUrl
    }
  emitRenderItem -- awake workers
  return $ OnlyField . toAPIRenderId $ i

-- | Definition of 507 HTTP error `Insufficient Storage`
err507 :: ServantErr
err507 = ServantErr {
    errHTTPCode = 507
  , errReasonPhrase = "Insufficient Storage"
  , errBody = ""
  , errHeaders = []
  }
