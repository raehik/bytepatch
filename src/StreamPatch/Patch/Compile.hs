-- | Compiling/normalizing patches using 'Compare'.

{-# LANGUAGE AllowAmbiguousTypes #-}

module StreamPatch.Patch.Compile where

import StreamPatch.Patch
import StreamPatch.Patch.Compare qualified as Compare
import StreamPatch.Patch.Compare ( Compare(..), Via(..), EqualityCheck(..) )

import StreamPatch.HFunctorList
import Optics
import Data.Generics.Product.Any
import Data.Vinyl
import Data.Vinyl.TypeLevel ( RIndex )

compilePatch
    :: forall v a s f f' fs fs' i
    .  ( Compare v a
       , f  ~ Compare.Meta ('ViaEq 'Exact)
       , f' ~ Compare.Meta v
       , RElem f fs i
       , RecElem Rec f f' fs fs' i
       , i ~ RIndex f fs
       )
    => Patch s fs  a
    -> Patch s fs' a
compilePatch = over ((the @"patchMeta") % hflLens) (compileCompareMeta @v)

compileCompareMeta
    :: forall v a. Compare v a
    => Compare.Meta ('ViaEq 'Exact) a
    -> Compare.Meta v a
compileCompareMeta (Compare.Meta cmp) =
    Compare.Meta $ fmap (toCompareRep @v) cmp
