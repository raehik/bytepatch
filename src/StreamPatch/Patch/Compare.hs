{- TODO
  * Via -> Strategy (? I do like the brevity of Via)
  * Rename Compare (but not sure what to...)
-}

{-# LANGUAGE AllowAmbiguousTypes, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module StreamPatch.Patch.Compare where

import GHC.Generics ( Generic )

import Numeric.Natural
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Text ( Text )

import Data.Void
import Control.Monad ( void )

import GHC.Exts ( Proxy#, proxy# )
import GHC.TypeLits ( KnownSymbol, symbolVal' )

import Raehik.HexBytestring

import Data.Aeson qualified as Aeson
import Data.Aeson ( ToJSON(..), FromJSON(..) )

import Text.Megaparsec
import Text.Megaparsec.Char qualified as MC
import Text.Megaparsec.Char.Lexer qualified as MCL

import BLAKE3 qualified as B3
import Data.ByteArray qualified as BA

import Data.Singletons.TH
-- required for deriving instances (seems like bug)
import Prelude.Singletons hiding ( AbsSym0, Compare )
import Data.Singletons.Base.TH ( FromString, sFromString )

$(singletons [d|
    -- | What sort of equality check to do.
    data EqualityCheck
      = Exact -- ^ "Exact equality" is defined as whatever the 'Eq' class does. (lol)
      | PrefixOf
        deriving stock (Eq, Show)
    |])
deriving stock instance Generic EqualityCheck

$(singletons [d|
    data HashFunc
      = HashFuncB3
      | HashFuncSHA256
      | HashFuncMD5
        deriving stock (Eq, Show)
    |])
deriving stock instance Generic HashFunc

$(singletons [d|
    -- | How should we compare two values?
    data Via
      -- | Are they equal in some way?
      = ViaEq EqualityCheck

      -- | Do they have the same size?
      | ViaSize

      -- | Do they have the same hash under the given hash function?
      | ViaHash HashFunc
        deriving (Eq, Show)
    |])
deriving stock instance Generic Via

data Meta (v :: Via) a = Meta
  { mCompare :: Maybe (CompareRep v a)
  } deriving stock (Generic)

deriving stock instance Eq   (CompareRep v a) => Eq   (Meta v a)
deriving stock instance Show (CompareRep v a) => Show (Meta v a)

instance SingI v => Functor     (Meta v) where
    fmap f (Meta c) = case sing @v of
      SViaEq   _ -> Meta $ fmap f c
      SViaSize   -> Meta c
      SViaHash _ -> Meta c

instance SingI v => Foldable    (Meta v) where
    foldMap f (Meta c) = case sing @v of
      SViaEq   _ -> foldMap f c
      SViaSize   -> mempty
      SViaHash _ -> mempty

instance SingI v => Traversable (Meta v) where
    traverse f (Meta c) = case sing @v of
      SViaEq   _ -> Meta <$> traverse f c
      SViaSize   -> pure $ Meta c
      SViaHash _ -> pure $ Meta c

type family CompareRep (v :: Via) a where
    CompareRep ('ViaEq _) a   = a
    CompareRep 'ViaSize _     = Natural
    CompareRep ('ViaHash h) _ = Hash h

-- | A bytestring representing the output of hashing something using the given
--   hash function.
newtype Hash (h :: HashFunc) = Hash { hashBytes :: BS.ByteString }
    deriving stock (Generic, Eq, Show)

-- may as well do it at type level lol
type family HashFuncLabel (h :: HashFunc) where
    HashFuncLabel 'HashFuncB3     = "b3"
    HashFuncLabel 'HashFuncSHA256 = "sha256"
    HashFuncLabel 'HashFuncMD5    = "md5"

-- ...but we will need to reflect the 'Symbol' to value level
hashFuncLabel :: forall h l. (l ~ HashFuncLabel h, KnownSymbol l) => Text
hashFuncLabel = Text.pack (symbolVal' (proxy# :: Proxy# l))

-- | Pretty print a hash like @hashfunc:123abc@.
prettyHash :: forall h l. (l ~ HashFuncLabel h, KnownSymbol l) => Hash h -> Text
prettyHash h =
       hashFuncLabel @h
    <> Text.singleton ':'
    <> prettyHexBytestringCompact (hashBytes h)

-- | Add a @hash:@ prefix to better separate from regular text.
instance (l ~ HashFuncLabel h, KnownSymbol l) => ToJSON   (Hash h) where
    toJSON = Aeson.String . Text.append "hash:" . prettyHash

instance (l ~ HashFuncLabel h, KnownSymbol l) => FromJSON (Hash h) where
    parseJSON = Aeson.withText "hash string" $ \t ->
        case parseMaybe @Void parseHash t of
          Nothing   -> fail "failed to parse hash"
          Just hash -> pure hash

-- Bad naming, these maybe aren't symbols/lexemes in the expected sense
-- (horizontal spacing is optional).
parseHash
    :: forall h l e s m
    .  (l ~ HashFuncLabel h, KnownSymbol l, MonadParsec e s m, Token s ~ Char, Tokens s ~ Text)
    => m (Hash h)
parseHash = do
    symbol "hash"
    symbol ":"
    symbol $ hashFuncLabel @h
    symbol ":"
    Hash <$> parseHexBytestring
  where symbol = void . MCL.lexeme MC.hspace . chunk

class Compare (v :: Via) a where
    compare'     :: CompareRep v a -> CompareRep v a -> Maybe String
    toCompareRep :: a -> CompareRep v a

compareTo :: forall v a. Compare v a => CompareRep v a -> a -> Maybe String
compareTo cmp = compare' @v @a cmp . toCompareRep @v @a

-- Free instance: Compare for exact equality via 'Eq'.
instance Eq a => Compare ('ViaEq 'Exact) a where
    toCompareRep = id
    compare' c1 c2 | c1 == c2  = Nothing
                   | otherwise = Just "values not equal"

instance Compare ('ViaHash 'HashFuncB3) BS.ByteString where
    toCompareRep = Hash . hashB3
    compare' c1 c2 | c1 == c2  = Nothing
                   | otherwise = Just "hashes not equal"

-- I unpack to '[Word8]' then repack to 'ByteString' because the memory library
-- is very keen on complicated unsafe IO. cheers no thanks
hashB3 :: BS.ByteString -> BS.ByteString
hashB3 bs = BS.pack $ BA.unpack $ B3.hash @B3.DEFAULT_DIGEST_LEN [bs]

class SwapCompare a (vFrom :: Via) (vTo :: Via) where
    swapCompare :: CompareRep vFrom a -> Either String (CompareRep vTo a)

instance SwapCompare a v v where
    swapCompare = Right

instance SwapCompare BS.ByteString ('ViaEq 'Exact) ('ViaHash 'HashFuncB3) where
    swapCompare = Right . Hash . hashB3
