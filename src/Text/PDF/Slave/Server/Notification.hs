module Text.PDF.Slave.Server.Notification(
    NotificationError(..)
  , postNotification
  ) where

import Control.Exception        (try)
import Control.Lens
import Control.Monad            (unless)
import Control.Monad.Except     (ExceptT (..), throwError, runExceptT)
import Control.Monad.IO.Class   (MonadIO(..))
import Data.Aeson               (encode)
import Data.Bifunctor           (first)
import Data.Monoid
import Data.Text                (Text, unpack)
import GHC.Generics             (Generic)
import Network.HTTP.Client      (HttpException (..), Manager)
import Network.Wreq

import Text.PDF.Slave.Server.API

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
  -- | If returned 'Right' then notification is
  -- succeded, else it should be retried
  -> m (Either NotificationError ())
postNotification mng baseUrl notification = liftIO . runExceptT $ do
    let body = encode notification
        opts = defaults & manager               .~ Right mng
                        & redirects             .~ 1
                        & header "Content-Type" .~ ["application/json"]
    response <- catchHttpException $ postWith opts (unpack baseUrl) body
    unless (response ^. responseStatus . statusCode == 200) $
        throwError $ WrongSuccessStatus (response ^. responseStatus)
  where
    catchHttpException = ExceptT . fmap (first toNotificationError) . try