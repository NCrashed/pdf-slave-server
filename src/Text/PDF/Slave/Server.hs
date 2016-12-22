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

import Servant.Server
import Data.Proxy

import Text.PDF.Slave.Server.API
import Text.PDF.Slave.Server.Config
import Text.PDF.Slave.Server.Monad

-- | Full Server API
type ServerAPI = PDFSlaveAPI

-- | WAI application of server
pdfSlaveServerApp :: ServerEnv -> Application
pdfSlaveServerApp e = serve (Proxy :: Proxy ServerAPI) $ enter (serverMtoHandler e) $
  pdfSlaveServer

-- | Implementation of main server API
pdfSlaveServer :: ServerT PDFSlaveAPI ServerM
pdfSlaveServer = undefined