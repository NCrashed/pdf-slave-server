module Text.PDF.Slave.Server.Util(
    ffor
  , whenJust
  , showl
  , showt
  ) where

import Control.Monad.Logger
import Data.Text

-- | Fliped fmap
ffor :: Functor f => f a -> (a -> b) -> f b
ffor = flip fmap

-- | Execute applicative action only when the value is 'Just'
whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust (Just a) m = m a
whenJust Nothing _ = pure ()

-- | Convert anything to log string
showl :: Show a => a -> LogStr
showl = toLogStr . show

-- | Convert anything to log string
showt :: Show a => a -> Text
showt = pack . show