module Raehik.HexByteString where

import Data.Text qualified as Text
import Data.Text ( Text )
import Bytezap.Poke qualified as Bytezap
import Data.Word ( Word8 )
import Data.ByteString ( ByteString )
import Data.Char qualified as Char
import Data.ByteString.RaehikExt qualified as B

import Foreign.Ptr ( Ptr )
import GHC.Storable ( writeWord8OffPtr )

-- | Parse a hex bytestring from 'Text' to a strict 'ByteString'.
--
-- TODO so, I build a builder while also tracking the length, then I can run the
-- builder at the end because I know the precise length. this is fairly good.
-- a faster way would be to allocate a buffer of the maximum size, then do all
-- the work in IO where we do the writing work inline. but then we either
-- reallocated if non-max, or simply ignore the unused space. the former is
-- slow, the latter is potentially bad behaviour (not a memory leak, but
-- close).
--
-- It'd be nice to encode such a self-tracking buffer in a library somewhere.
--
-- I wonder if even with reallocation it would still be better than as-is. I
-- don't really think I can do the silent unused memory trick, it seems Bad.
--
-- This just seems like a case of "easy pure, faster impure". Like I swapped out
-- megaparsec for 30 lines of light hand-rolled parsing and some clumsy CPS,
-- retaining purity. Faster would be in IO, and would mean weirder code.
--
-- I should write each version (3) and benchmark them, especially on larger
-- inputs. I want to see how both memory and performance differs.
textToByteString :: Text -> Either String ByteString
textToByteString tTop = go mempty 0 tTop
  where
    go bld1 len t1 =
        case Text.uncons t1 of
          Just (c1, t2) ->
            case c1 of
              ' ' -> go bld1 len t2
              _   -> withCharAsHexDigit c1 errNotHexDigit $ \c1i ->
                case Text.uncons t2 of
                  Just (c2, t3) -> withCharAsHexDigit c2 errNotHexDigit $ \c2i ->
                    let b :: Word8 = fromIntegral $ 0x10 * c1i + c2i
                        bld2 = bld1 <> Bytezap.prim b
                    in  go bld2 (len+1) t3
                  Nothing       -> Left "ended during byte (TODO)"
          Nothing       ->
            Right $ Bytezap.unsafeRunPokeBS len bld1
    errNotHexDigit = \c -> Left $ "not a hexadecimal digit: " <> show c

-- | Parse a hex bytestring from 'Text' to a strict 'ByteString' with messy IO.
xxx :: Ptr Word8 -> Text -> Int -> IO (Either String Int)
xxx buf = \t1 len ->
    case Text.uncons t1 of
      Just (c1, t2) ->
        case c1 of
          ' ' -> xxx buf t2 len
          _   -> withCharAsHexDigit c1 errNotHexDigit $ \c1i ->
            case Text.uncons t2 of
              Just (c2, t3) -> withCharAsHexDigit c2 errNotHexDigit $ \c2i -> do
                let b :: Word8 = fromIntegral $ 0x10*c1i + c2i
                writeWord8OffPtr buf len b
                xxx buf t3 (len+1)
              Nothing       -> pure $ Left "ended during byte (TODO)"
      Nothing       -> pure $ Right len
  where
    errNotHexDigit = \c -> pure $ Left $ "not a hexadecimal digit: " <> show c
{-# INLINE xxx #-}

-- | Parse a hex bytestring from 'Text' to a strict 'ByteString'
--   efficiently using low-level primitives.
--
-- One buffer is allocated that will fit the maximum length result possible for
-- the given input. If the actual result's length is any shorter, we reallocate.
--
-- Inputs with lots of spaces will result in lots of temporary wasted memory.
textToByteStringTrimIO :: Text -> IO (Either String ByteString)
textToByteStringTrimIO t =
    B.createAndTrimFailable
        (Text.length t `quot` 2)
        (\buf -> xxx buf t 0)
{-# INLINE textToByteStringTrimIO #-}

textToByteStringUptoIO :: Text -> IO (Either String ByteString)
textToByteStringUptoIO t =
    B.createUptoNFailable
        (Text.length t `quot` 2)
        (\buf -> xxx buf t 0)
{-# INLINE textToByteStringUptoIO #-}

-- Performs 10% worse in microbenchmarks than Either version. INLINE or not.
xxx'
    :: Ptr Word8 -> (Int -> IO ByteString) -> Text -> Int
    -> IO (Either String ByteString)
xxx' buf fMk = go
  where
    errNotHexDigit = \c -> pure $ Left $ "not a hexadecimal digit: " <> show c
    go = \t1 len ->
        case Text.uncons t1 of
          Just (c1, t2) ->
            case c1 of
              ' ' -> xxx' buf fMk t2 len
              _   -> withCharAsHexDigit c1 errNotHexDigit $ \c1i ->
                case Text.uncons t2 of
                  Just (c2, t3) -> withCharAsHexDigit c2 errNotHexDigit $ \c2i -> do
                    let b :: Word8 = fromIntegral $ 0x10*c1i + c2i
                    writeWord8OffPtr buf len b
                    xxx' buf fMk t3 (len+1)
                  Nothing       -> pure $ Left "ended during byte (TODO)"
          Nothing       -> Right <$> fMk len
{-# INLINE xxx' #-}

textToByteStringTrimCPSIO :: Text -> IO (Either String ByteString)
textToByteStringTrimCPSIO t =
    B.createAndTrimCPS
        (Text.length t `quot` 2)
        (\buf fMk -> xxx' buf fMk t 0)
{-# INLINE textToByteStringTrimCPSIO #-}

textToByteStringTrimCPS :: Text -> Either String ByteString
textToByteStringTrimCPS t =
    B.unsafeCreateAndTrimCPS
        (Text.length t `quot` 2)
        (\buf fMk -> xxx' buf fMk t 0)
{-# INLINE textToByteStringTrimCPS #-}

-- no realloc
textToByteStringUptoCPSIO :: Text -> IO (Either String ByteString)
textToByteStringUptoCPSIO t =
    B.createUptoNCPS
        (Text.length t `quot` 2)
        (\buf fMk -> xxx' buf fMk t 0)
{-# INLINE textToByteStringUptoCPSIO #-}


-- adapted from Data.Char.digitToInt
withCharAsHexDigit :: Char -> (Char -> r) -> (Int -> r) -> r
withCharAsHexDigit c fFail f
  | (fromIntegral dec  :: Word) <= 9 = f dec
  | (fromIntegral hexl :: Word) <= 5 = f $ hexl + 10
  | (fromIntegral hexu :: Word) <= 5 = f $ hexu + 10
  | otherwise = fFail c
  where
    dec  = Char.ord c - Char.ord '0'
    hexl = Char.ord c - Char.ord 'a'
    hexu = Char.ord c - Char.ord 'A'

{-

-- | Pretty print to default format @00 12 AB FF@: space between each byte, all
--   caps.
--
-- This format I consider most human readable. I prefer caps to draw attention
-- to this being data instead of text (you don't see that many capital letters
-- packed together in prose).
prettyHexByteString :: (a -> [Word8]) -> a -> Text
prettyHexByteString unpack =
      Text.concat
    . List.intersperse (Text.singleton ' ')
    . fmap (f . prettyHexByte Char.toUpper)
    . unpack
  where
    f :: (Char, Char) -> Text
    f (c1, c2) = Text.cons c1 $ Text.singleton c2

prettyHexByte :: (Char -> Char) -> Word8 -> (Char, Char)
prettyHexByte f w = (prettyNibble h, prettyNibble l)
  where
    (h,l) = fromIntegral w `divMod` 0x10
    prettyNibble = f . Char.intToDigit -- Char.intToDigit returns lower case

-- | Pretty print to "compact" format @0012abff@ (often output by hashers).
prettyHexByteStringCompact :: (a -> [Word8]) -> a -> Text
prettyHexByteStringCompact unpack =
    Text.concat . fmap (f . prettyHexByte id) . unpack
  where
    f :: (Char, Char) -> Text
    f (c1, c2) = Text.cons c1 $ Text.singleton c2

-- TODO could add some integer instances to print them as hex too

-- No harm in being polymorphic over the byte representation.
newtype Hex a = Hex { unHex :: a }
    deriving stock Generic
    deriving Eq via a

-- But most users will probably just want this.
type HexByteString = Hex B.ByteString

instance Show (Hex B.ByteString) where
    show = Text.unpack . prettyHexByteString B.unpack . unHex

instance FromJSON (Hex B.ByteString) where
    parseJSON = withText "hex bytestring" $ \t ->
        case parseMaybe @Void (parseHexByteString B.pack) t of
          Nothing -> fail "failed to parse hex bytestring (TODO)"
          Just t' -> pure (Hex t')

instance ToJSON   (Hex B.ByteString) where
    toJSON = String . prettyHexByteString B.unpack . unHex

instance Show (Hex B.Short.ShortByteString) where
    show = Text.unpack . prettyHexByteString B.Short.unpack . unHex

instance FromJSON (Hex B.Short.ShortByteString) where
    parseJSON = withText "hex bytestring" $ \t ->
        case parseMaybe @Void (parseHexByteString B.Short.pack) t of
          Nothing -> fail "failed to parse hex bytestring (TODO)"
          Just t' -> pure (Hex t')

instance ToJSON   (Hex B.Short.ShortByteString) where
    toJSON = String . prettyHexByteString B.Short.unpack . unHex

-}
