module Text.PDF.Slave.Server.Notification(
    NotificationError(..)
  , postNotification
  ) where

import Control.Exception        (try)
import Control.Lens
import Control.Monad            (unless)
import Control.Monad.Except     (ExceptT (..), throwError, runExceptT)
import Control.Monad.IO.Class   (MonadIO(..))
import Crypto.Hash              (Digest, SHA256, hashlazy)
import Data.Aeson               (encode)
import Data.Bifunctor           (first)
import Data.ByteArray           (convert)
import Data.Monoid
import Data.Text                (Text, unpack, pack)
import Data.Text.Encoding       (encodeUtf8)
import GHC.Generics             (Generic)
import Network.HTTP.Client      (HttpException (..), Manager)
import Network.Wreq
import Servant.API.Auth.Token

import Text.PDF.Slave.Server.API

import qualified Data.ByteString          as BS (ByteString)
import qualified Data.ByteString.Base16   as B16
import qualified Data.ByteString.Lazy     as BSL (ByteString, fromStrict)

-- | Wrapper around client 'HttpException' to distinguish errors when an ill
-- formatted base url is specified and when an error that requires retry is
-- occured.
data NotificationError =
    -- | Ill formated base url, don't retry
      BadBaseUrl !String
    -- | The url leads to redirection what is forbidden
    | HasRedirectError
    -- | The request succeeded with 2xx status but not equal to 200
    | WrongSuccessStatus !Status
    -- | Other error that requires retry
    | NotificationFail !HttpException
    deriving (Generic, Show)

-- | Convert http exception to semantically rich 'NotificationError'
toNotificationError :: HttpException -> NotificationError
toNotificationError e = case e of
    InvalidUrlException s1 s2 -> BadBaseUrl (s1 <> " : " <> s2)
    TooManyRedirects _        -> HasRedirectError
    UnparseableRedirect _     -> HasRedirectError
    _                         -> NotificationFail e

-- | Send notification to remote server.
postNotification :: MonadIO m
  -- | Connection manager
  => Manager
  -- | Base url of shop endpoint
  -> Text
  -- | Body of notification
  -> APINotificationBody
  -- | Token that was used at registration of rendering
  -> SimpleToken
  -- | If returned 'Right' then notification is
  -- succeded, else it should be retried
  -> m (Either NotificationError ())
postNotification mng baseUrl' notification token = liftIO . runExceptT $ do
    let baseUrl = unpack baseUrl'
        body = encode notification
        sign = makeSignature baseUrl body token
        opts = defaults & manager               .~ Right mng
                        & redirects             .~ 1
                        & header "X-Signature"  .~ [sign]
                        & header "Content-Type" .~ ["application/json"]
    response <- catchHttpException $ postWith opts baseUrl body
    unless (response ^. responseStatus . statusCode == 200) $
        throwError $ WrongSuccessStatus (response ^. responseStatus)
  where
    catchHttpException = ExceptT . fmap (first toNotificationError) . try

-- | Make 'X-Signature' header for 'postNotification'.
--
-- The request is signed with SHA256. The hash is applied to URI, full body of
-- query, authorisation token that are separated by commas. The signatures
-- is placed in 'X-Signature' header. Header content is encoded in Base16.
makeSignature :: String -- ^ Base url of shop endpoint
    -> BSL.ByteString -- ^ Request body that we sign
    -> SimpleToken -- ^ Authorisation token that is used to request the render
    -> BS.ByteString -- ^ SHA256 hash aka signature
makeSignature url body token = B16.encode . convert $ digets
    where
    toBs = BSL.fromStrict . encodeUtf8 . pack
    signBody = toBs url <> "," <> body <> "," <> toBs (show token)
    digets = hashlazy signBody :: Digest SHA256
