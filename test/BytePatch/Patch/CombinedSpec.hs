-- | Various combined patch tests (using multiple layers).

{-# LANGUAGE ScopedTypeVariables #-}

module BytePatch.Patch.CombinedSpec ( spec ) where

import           BytePatch.Patch.Linearize
import qualified BytePatch.Patch.Linearize  as Linearize
import           BytePatch.Patch.Binary
import qualified BytePatch.Patch.Binary     as Bin
import           Test.Hspec
import           Util

import           BytePatch.Patch
import           Data.Functor.Const
import           Data.Either.Combinators
import           GHC.Natural
import           Optics
import           Data.Generics.Product.Any
import qualified Data.ByteString            as BS

type Bytes = BS.ByteString

{-
TODO. Aim is to present how linearization and binary conversion can be done in
either order, but that they mean different things.
-}

spec :: Spec
spec = do
    let p  = \d s -> Patch d $ Pos s (Const ())
        --p' = \d s -> Patch (fromRight' (toBinRep d)) $ Pos s (Const ())
    it "shows that for multibyte UTF-8, order of linearizing/binarifying matters" $ do
      pending
    it "shows that for ASCII, order of linearizing/binarifying doesn't matter" $ do
      linearizeThenBinarify (asBin exAscii) `shouldBe` Right exAsciiBinLinearized

linearizeThenBinarify
    :: (BinRep a, HasLength a, Traversable d)
    => [Patch 'AbsSeek (Meta d) a]
    -> Either (Linearize.Error (Meta d) a) [Patch 'FwdSeek d Bytes]
linearizeThenBinarify d =
    case f1 d of
      Left err -> Left err
      Right x  ->
        case x of
          Left _   -> error "unexpected binrep error"
          Right x' -> Right $ map stripBinMeta x'

asPlain :: (Const () a -> b) -> b
asPlain f = f $ Const ()

asBin :: (Meta (Const ()) a -> t) -> t
asBin f = f $ Bin.Meta Nothing Nothing Nothing (Const ())

exAscii :: d String -> [Patch 'AbsSeek d String]
exAscii m =
  [ p "0" 0
  , p "1" 1
  , p "2" 2
  ] where p d s = Patch d $ Pos s m

exAsciiBinLinearized :: [Patch 'FwdSeek (Const ()) Bytes]
exAsciiBinLinearized =
  [ p "0" 0
  , p "1" 0
  , p "2" 0
  ] where p d s = Patch (fromRight' (toBinRep d)) $ Pos s (Const ())

exMultibytes :: d String -> [Patch 'AbsSeek d String]
exMultibytes m =
  [ p "0"  0 -- 1-byte UTF-8
  , p "か" 1 -- 3-byte UTF-8
  , p "😂" 4 -- 4-byte UTF-8
  , p "E"  8
  ] where p d s = Patch d $ Pos s m

f1
    :: (BinRep a, HasLength a, Traversable d)
    => [Patch 'AbsSeek (Meta d) a]
    -> Either
        (Linearize.Error (Meta d) a)
        (Either
          (Bin.Error a)
          [Patch 'FwdSeek (Meta d) Bytes])
f1 d = traverse patchBinRep <$> linearize d

f2
    :: (BinRep a, HasLength a, Traversable d)
    => [Patch 'AbsSeek (Meta d) a]
    -> Either
        (Bin.Error a)
        (Either
          (Linearize.Error (Meta d) Bytes)
          [Patch 'FwdSeek (Meta d) Bytes])
f2 d = linearize <$> traverse patchBinRep d

f2'
    :: (BinRep a, HasLength a, Traversable d)
    => [Patch 'AbsSeek (Meta d) a]
    -> Either (Linearize.Error (Meta d) Bytes) [Patch 'FwdSeek (Meta d) Bytes]
f2' = fromRight' . f2

-- TODO linearize type will change. no choice but to ignore
data Error' d la ba = ErrorLinearize (Linearize.Error d la) | ErrorBin (Bin.Error ba)

eitherSquish :: (a -> d) -> (b -> d) -> Either a (Either b c) -> Either d c
eitherSquish a b = error "This would be useful but it seems tricky to define"

errorSquish1
    :: Either (Bin.Error ba) (Either (Linearize.Error d la) x)
    -> Either (Error' d la ba) x
errorSquish1 =
    \case
      Left eb -> Left $ ErrorBin eb
      Right a ->
        case a of
          Left el -> Left $ ErrorLinearize el
          Right x -> Right $ x

errorSquish2
    :: Either (Linearize.Error d la) (Either (Bin.Error ba) x)
    -> Either (Error' d la ba) x
errorSquish2 =
    \case
      Left eb -> Left $ ErrorLinearize eb
      Right a ->
        case a of
          Left el -> Left $ ErrorBin el
          Right x -> Right $ x

-- I love optics and generic-optics. I love them very much
stripBinMeta :: Patch s (Bin.Meta d) a -> Patch s d a
stripBinMeta = over (the @"patchPos" % the @"posMeta") Bin.mInner
