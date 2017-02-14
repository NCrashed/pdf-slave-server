module Text.PDF.Slave.Server.Client.Signature(
    checkSignatureHeader
  , checkSignature
  -- * Helpers
  , makeSignature
  ) where

import Crypto.Hash              (Digest, SHA256, hashlazy)
import Data.ByteArray           (convert)
import Data.Monoid
import Data.Text                (pack)
import Data.Text.Encoding       (encodeUtf8, decodeUtf8)
import Servant.API
import Servant.API.Auth.Token

import Text.PDF.Slave.Server.API

import qualified Data.ByteString.Base16   as B16
import qualified Data.ByteString.Lazy     as BSL (ByteString, fromStrict)

-- | Check notification signature that is packed in response header.
checkSignatureHeader :: String -- ^ Base url of shop endpoint
  -> BSL.ByteString -- ^ Request body that the sign is calculated to
  -> SimpleToken -- ^ Authorisation token that is used to request the render
  -> SignatureHeader -- ^ Signature that is arrived with the request
  -> Either String () -- ^ Left is fail, right is success
checkSignatureHeader url body tok h = case h of
  Header sig -> if checkSignature url body tok sig then Right ()
    else Left "Signatures are not equal"
  MissingHeader -> Left "Signature is missing in header"
  UndecodableHeader bs -> Left $ "Cannot decode header signature: " <> show bs

-- | Check notification signature.
checkSignature :: String -- ^ Base url of shop endpoint
  -> BSL.ByteString -- ^ Request body that the sign is calculated to
  -> SimpleToken -- ^ Authorisation token that is used to request the render
  -> Signature -- ^ Signature that is arrived with the request
  -> Bool
checkSignature url body tok sig = sig == makeSignature url body tok

-- | Make 'X-Signature' header for 'postNotification'.
--
-- The request is signed with SHA256. The hash is applied to URI, full body of
-- query, authorisation token that are separated by commas. The signatures
-- is placed in 'X-Signature' header. Header content is encoded in Base16.
makeSignature :: String -- ^ Base url of shop endpoint
    -> BSL.ByteString -- ^ Request body that we sign
    -> SimpleToken -- ^ Authorisation token that is used to request the render
    -> Signature -- ^ SHA256 hash aka signature
makeSignature url body token = decodeUtf8 . B16.encode . convert $ digets
    where
    toBs = BSL.fromStrict . encodeUtf8 . pack
    signBody = toBs url <> "," <> body <> "," <> toBs (show token)
    digets = hashlazy signBody :: Digest SHA256
