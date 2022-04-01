-- | Holy shit lol

{-# LANGUAGE AllowAmbiguousTypes #-}

module Compile where

import StreamPatch.Patch.Compare
import StreamPatch.Patch
import Data.ByteString qualified as BS

import BytePatch

import StreamPatch.HFunctorList
import Optics
import Data.Generics.Product.Any
import Data.Vinyl
import Data.Vinyl.TypeLevel ( RIndex )

compileMeta
    :: forall v
    .  Compare v BS.ByteString
    => Meta ('ViaEq 'Exact) BS.ByteString
    -> Meta v BS.ByteString
compileMeta (Meta cmp) = Meta $ fmap (toCompareRep @v) cmp

compilePatch
    :: forall v s f f' fs fs'
    .  ( Compare v BS.ByteString
       , f  ~ Meta ('ViaEq 'Exact)
       , f' ~ Meta v
       , RElem f fs (RIndex f fs)
       , RecElem Rec f f' fs fs' (RIndex f fs)
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
