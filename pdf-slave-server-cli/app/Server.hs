-- | Small server for receiving notification from pdf-slave-server
module Server(
    waitNotification
  ) where

import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.IORef
import Data.Proxy
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Servant.API
import Servant.API.Auth.Token
import Servant.Server
import System.IO.Unsafe (unsafePerformIO)

import Text.PDF.Slave.Server.API
import Text.PDF.Slave.Server.Client.Signature

import qualified Data.Vault.Lazy as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

-- | API of server, simply accept notification
type ServerAPI =
     ReqBody '[JSON] APINotificationBody
  :> SignatureHeader
  :> Vault
  :> Post '[JSON] ()

-- | Id of request body in middleware vault
bodyKey :: V.Key BSL.ByteString
bodyKey = unsafePerformIO V.newKey
{-# NOINLINE bodyKey #-}

-- | Middleware that puts request body to Vault to check signature againts it
bodyCacheMiddleware :: Middleware
bodyCacheMiddleware app req respond = do
  let
    readAllBody :: [ByteString] -> IO [ByteString]
    readAllBody !acc = do
      ch <- requestBody req
      if BS.length ch == 0 then return acc
        else readAllBody (ch : acc)

  body <- BSL.fromChunks . reverse <$> readAllBody []
  readRef <- newIORef False
  print $ requestHeaders req
  let vault' = V.insert bodyKey body (vault req)
      req' = req {
          vault = vault'
        , requestBody = do
            isRead <- readIORef readRef
            writeIORef readRef True
            return $ if isRead then BS.empty else BSL.toStrict body
        , requestBodyLength = KnownLength $ fromIntegral $ BSL.length body
        }
  app req' respond


-- | Serve server for notification receiving
server :: MVar (Either String APINotificationBody) -- ^ Where to place notification
  -> String -- ^ Full url of notification target to check signature with
  -> SimpleToken -- ^ Authorisation token to check signature with
  -> Server ServerAPI
server mvar url token notification msig vault = case msig of
  Nothing -> putRes $ Left "Missing signature for notification!"
  Just sig -> case V.lookup bodyKey vault of
    Nothing -> putRes $ Left "Impossible: vault doesn't contain request body!"
    Just body -> if checkSignature url body token sig
      then putRes $ Right notification
      else putRes $ Left "Signatures don't match, possible hijack!"
  where
    putRes = liftIO . putMVar mvar

-- | WAI application for the server
serverApp :: MVar (Either String APINotificationBody)
  -> String -- ^ Full url of notification target to check signature with
  -> SimpleToken -- ^ Authorisation token to check signature with
  -> Application
serverApp mvar url token = serve (Proxy :: Proxy ServerAPI) $ server mvar url token

-- | Start internal server and listen for incoming notifications, terminates
-- immediatily after notification arrive.
waitNotification :: Int -- ^ Port
  -> String -- ^ Full url of notification target to check signature with
  -> SimpleToken -- ^ Authorisation token to check signature with
  -> IO (Either String APINotificationBody)
waitNotification port url token = do
  mvar <- newEmptyMVar
  let startServer = forkIO $ run port $ logStdoutDev . bodyCacheMiddleware $ serverApp mvar url token
  bracket startServer killThread $ const $ do
    res <- takeMVar mvar
    -- let server to send OK response
    threadDelay $ 500 * 10^(3 :: Int)
    return res
