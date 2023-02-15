{- TODO
  * Via -> Strategy (? I do like the brevity of Via)
  * Rename Compare (but not sure what to...)
-}

{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module StreamPatch.Patch.Compare where

import GHC.Generics ( Generic )
import Data.Data ( Typeable, Data )

import Numeric.Natural
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Text ( Text )

import Data.Void
import Control.Monad ( void )

import GHC.Exts ( Proxy#, proxy# )
import GHC.TypeLits ( KnownSymbol, symbolVal' )

import Binrep.Extra.HexByteString

import Data.Aeson qualified as Aeson
import Data.Aeson ( ToJSON(..), FromJSON(..) )

import Text.Megaparsec
import Text.Megaparsec.Char qualified as MC
import Text.Megaparsec.Char.Lexer qualified as MCL

import BLAKE3 qualified as B3
import Data.ByteArray.Sized qualified as BA
import Data.ByteString qualified as B
import Data.Word ( Word8 )

import Data.Singletons.TH
-- required for deriving instances (seems like bug)
import Prelude.Singletons hiding ( AbsSym0, Compare )
import Data.Singletons.Base.TH ( FromString, sFromString )

$(singletons [d|
    -- | What sort of equality check to do.
    data EqualityCheck
      = Exact -- ^ "Exact equality" is defined as whatever the 'Eq' class does. (lol)
      | PrefixOf
        deriving stock (Show, Eq)
    |])
deriving stock instance Generic  EqualityCheck
deriving stock instance Typeable EqualityCheck
deriving stock instance Data     EqualityCheck

$(singletons [d|
    data HashFunc
      = B3
      | SHA256
      | MD5
        deriving stock (Show, Eq)
    |])
deriving stock instance Generic  HashFunc
deriving stock instance Typeable HashFunc
deriving stock instance Data     HashFunc

$(singletons [d|
    -- | How should we compare two values?
    data Via
      -- | Are they equal in some way?
      = ViaEq EqualityCheck

      -- | Do they have the same size?
      | ViaSize

      -- | Do they have the same digest under the given hash function?
      | ViaDigest HashFunc
        deriving stock (Show, Eq)
    |])
deriving stock instance Generic  Via
deriving stock instance Typeable Via
deriving stock instance Data     Via

data Meta (v :: Via) a = Meta
  { mCompare :: Maybe (CompareRep v a)
  } deriving stock (Generic)

deriving stock instance Eq   (CompareRep v a) => Eq   (Meta v a)
deriving stock instance Show (CompareRep v a) => Show (Meta v a)

deriving anyclass instance ToJSON   (CompareRep v a) => ToJSON   (Meta v a)
deriving anyclass instance FromJSON (CompareRep v a) => FromJSON (Meta v a)

instance SingI v => Functor     (Meta v) where
    fmap f (Meta c) = case sing @v of
      SViaEq     _ -> Meta $ fmap f c
      SViaSize     -> Meta c
      SViaDigest _ -> Meta c

instance SingI v => Foldable    (Meta v) where
    foldMap f (Meta c) = case sing @v of
      SViaEq     _ -> foldMap f c
      SViaSize     -> mempty
      SViaDigest _ -> mempty

instance SingI v => Traversable (Meta v) where
    traverse f (Meta c) = case sing @v of
      SViaEq     _ -> Meta <$> traverse f c
      SViaSize     -> pure $ Meta c
      SViaDigest _ -> pure $ Meta c

type family CompareRep (v :: Via) a where
    CompareRep ('ViaEq _)     a = a
    CompareRep 'ViaSize       _ = Natural
    CompareRep ('ViaDigest h) _ = Digest h B.ByteString

-- | The resulting digest from hashing some data using the given hash function.
--
-- TODO
-- As of 2022, most good cryptographic hash functions produce digest sizes
-- between 256-512 bits. That's 32-64 bytes. So I want to use a ShortByteString,
-- but the BLAKE3 library uses the memory library, which I can't figure out. I
-- bet it'd be more efficient. So, I'm polymorphising in preparation.
newtype Digest (h :: HashFunc) a = Digest { getDigest :: a }
    deriving stock (Generic, Eq, Show)

type Digest' h = Digest h B.ByteString

type family HashFuncLabel (h :: HashFunc) where
    HashFuncLabel 'B3     = "b3"
    HashFuncLabel 'SHA256 = "sha256"
    HashFuncLabel 'MD5    = "md5"

hashFuncLabel :: forall h l. (l ~ HashFuncLabel h, KnownSymbol l) => Text
hashFuncLabel = Text.pack (symbolVal' (proxy# :: Proxy# l))

-- | Add a @digest:@ prefix to better separate from regular text.
instance (l ~ HashFuncLabel h, KnownSymbol l) => ToJSON   (Digest h B.ByteString) where
    toJSON = Aeson.String . Text.append "digest:" . prettyDigest B.unpack

instance (l ~ HashFuncLabel h, KnownSymbol l) => FromJSON (Digest h B.ByteString) where
    parseJSON = Aeson.withText "hex hash digest" $ \t ->
        case parseMaybe @Void parseDigest t of
          Nothing   -> fail "failed to parse hex hash digest"
          Just hash -> pure hash

-- | Pretty print a hash like @hashfunc:123abc@.
prettyDigest
    :: forall h a l. (l ~ HashFuncLabel h, KnownSymbol l)
    => (a -> [Word8]) -> Digest h a -> Text
prettyDigest unpack (Digest d) =
       hashFuncLabel @h
    <> Text.singleton ':'
    <> prettyHexByteStringCompact unpack d

-- Bad naming, these maybe aren't symbols/lexemes in the expected sense
-- (horizontal spacing is optional).
parseDigest
    :: forall h l e s m
    .  (l ~ HashFuncLabel h, KnownSymbol l, MonadParsec e s m, Token s ~ Char, Tokens s ~ Text)
    => m (Digest h B.ByteString)
parseDigest = do
    symbol "digest"
    symbol ":"
    symbol $ hashFuncLabel @h
    symbol ":"
    Digest <$> parseHexByteString B.pack
  where symbol = void . MCL.lexeme MC.hspace . chunk

class Compare (v :: Via) a where
    compare'     :: CompareRep v a -> CompareRep v a -> Maybe String
    toCompareRep :: a -> CompareRep v a

compareTo :: forall v a. Compare v a => CompareRep v a -> a -> Maybe String
compareTo cmp = compare' @v @a cmp . toCompareRep @v @a

-- Free instance: Compare for exact equality via 'Eq'.
-- TODO show is bandaid. no need to handle it here.
instance (Eq a, Show a) => Compare ('ViaEq 'Exact) a where
    toCompareRep = id
    compare' c1 c2 | c1 == c2  = Nothing
                   | otherwise = Just $ "values not equal: "<>show c1<>" /= "<>show c2

instance Compare ('ViaDigest 'B3) BS.ByteString where
    toCompareRep = Digest . hashB3
    compare' c1 c2 | c1 == c2  = Nothing
                   | otherwise = Just "digests not equal"

-- TODO I need to define the compare class better lol, I'm confused which is
-- real and which is test.
instance Compare ('ViaEq 'PrefixOf) BS.ByteString where
    toCompareRep = id
    compare' c1 c2 | c2 `B.isPrefixOf` c1 = Nothing
                   | otherwise = Just $ "prefix compare check fail: "<>show c1<>" vs. "<>show c2

-- I unpack to '[Word8]' then repack to 'ByteString' because the memory library
-- is very keen on complicated unsafe IO. cheers no thanks
hashB3 :: BS.ByteString -> BS.ByteString
hashB3 bs = BA.unSizedByteArray $ B3.hash @B3.DEFAULT_DIGEST_LEN Nothing [bs]

class SwapCompare a (vFrom :: Via) (vTo :: Via) where
    swapCompare :: CompareRep vFrom a -> Either String (CompareRep vTo a)

instance SwapCompare a v v where
    swapCompare = Right

instance SwapCompare BS.ByteString ('ViaEq 'Exact) ('ViaDigest 'B3) where
    swapCompare = Right . Digest . hashB3
