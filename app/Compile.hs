-- | Holy shit lol

{-# LANGUAGE AllowAmbiguousTypes, OverloadedStrings #-}

module Compile where

import Config ( CCmdCompile )

import StreamPatch.Patch.Compare
import StreamPatch.Patch
import Data.ByteString qualified as BS
import Raehik.HexBytestring ( HexBytestring(..) )
import StreamPatch.Patch.Binary qualified as Bin
import StreamPatch.Patch.Compare qualified as Compare

import BytePatch

import StreamPatch.HFunctorList
import Optics
import Data.Generics.Product.Any
import Data.Vinyl
import Data.Vinyl.TypeLevel ( RIndex )

import Data.Yaml.Pretty qualified as YamlPretty
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Aeson ( ToJSON )

compileMeta
    :: forall v
    .  Compare v BS.ByteString
    => Meta ('ViaEq 'Exact) BS.ByteString
    -> Meta v BS.ByteString
compileMeta (Meta cmp) = Meta $ fmap (toCompareRep @v) cmp

compilePatch
    :: forall v s f f' fs fs' i
    .  ( Compare v BS.ByteString
       , f  ~ Meta ('ViaEq 'Exact)
       , f' ~ Meta v
       , RElem f fs i
       , RecElem Rec f f' fs fs' i
       , i ~ RIndex f fs
       )
    => Patch s fs  BS.ByteString
    -> Patch s fs' BS.ByteString
compilePatch = over ((the @"patchMeta") % hflLens) (compileMeta @v)

compileSeek
    :: forall v s
    .  Compare v BS.ByteString
    => Seek s ('ViaEq 'Exact) BS.ByteString
    -> Seek s v BS.ByteString
--compileSeek = over (the @"sCompare") $ fmap $ toCompareRep @('ViaHash 'HashFuncB3)
compileSeek s = s { sCompare = fmap (toCompareRep @v) (sCompare s) }

compileMultiPatch
    :: forall v s
    .  Compare v BS.ByteString
    => MultiPatch s ('ViaEq 'Exact) BS.ByteString
    -> MultiPatch s v BS.ByteString
compileMultiPatch = over (the @"mpAt") $ fmap compileSeek

runCompileCompareBin
    :: forall v s m
    .  ( MonadIO m
       , Functor (Seek s v)
       , ToJSON (SeekRep s), ToJSON (CompareRep v HexBytestring)
       )
    => CCmdCompile
    -> [Patch s '[Compare.Meta v, Bin.Meta] BS.ByteString]
    -> m ()
runCompileCompareBin _cfg ps =
      let ps' = fmap (fmap HexBytestring . convertBackBin) ps
       in liftIO $ BS.putStr $ YamlPretty.encodePretty yamlPrettyCfg ps'

-- silly stuff
yamlPrettyCfg :: YamlPretty.Config
yamlPrettyCfg = YamlPretty.setConfCompare cmp $ YamlPretty.setConfDropNull True $ YamlPretty.defConfig
  where
    cmp "data" _  = LT
    cmp _  "data" = GT
    cmp "seek" _ = LT
    cmp _ "seek" = GT
    cmp k1     k2 = Prelude.compare k1 k2
