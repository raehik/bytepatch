{-# LANGUAGE TypeFamilies #-}

module Util where

import           Text.Megaparsec
import           Data.Void

--parseFromCharStream :: (MonadParsec e s m, Token s ~ Char) => m a -> s -> Maybe a
parseFromCharStream :: (Stream s, Token s ~ Char) => Parsec Void s a -> s -> Maybe a
parseFromCharStream parser text = parseMaybe parser text
