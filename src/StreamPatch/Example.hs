{-# LANGUAGE OverloadedStrings #-}

module StreamPatch.Example where

import StreamPatch.Patch
import StreamPatch.Patch.Compare qualified as Compare
import StreamPatch.Patch.Compare ( Via(..), EqualityCheck(..) )
import StreamPatch.HFunctorList
import Data.Vinyl

import Data.Text ( Text )

ex :: Patch 'AbsSeek '[Compare.Meta ('ViaEq 'Exact)] Text
ex = Patch "no way this works LMAO" 0 $ HFunctorList $ Flap (Compare.Meta Nothing) :& RNil

ex2 :: Patch 'AbsSeek '[] Text
ex2 = Patch "no way this works LMAO" 0 $ HFunctorList RNil
