-- | Small server for receiving notification from pdf-slave-server
module Server(
    waitNotification
  ) where

import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Proxy
import Network.Wai
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server

import Text.PDF.Slave.Server.API

-- | API of server, simply accept notification
type ServerAPI =
     ReqBody '[JSON] APINotificationBody
  :> Post '[JSON] ()

-- | Serve server for notification receiving
server :: MVar APINotificationBody -- ^ Where to place notification
  -> Server ServerAPI
server mvar notification = liftIO $ putMVar mvar notification

-- | WAI application for the server
serverApp :: MVar APINotificationBody -> Application
serverApp = serve (Proxy :: Proxy ServerAPI) . server

-- | Start internal server and listen for incoming notifications, terminates
-- immediatily after notification arrive.
waitNotification :: Int -- ^ Port
  -> IO APINotificationBody
waitNotification port = do
  mvar <- newEmptyMVar
  let startServer = forkIO $ run port $ serverApp mvar
  bracket startServer killThread $ const $ takeMVar mvar
