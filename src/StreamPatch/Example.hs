{-# LANGUAGE OverloadedStrings #-}

module StreamPatch.Example where

import StreamPatch.Patch
import StreamPatch.Seek
import StreamPatch.Patch.Compare qualified as Compare
import StreamPatch.Patch.Compare ( Via(..), EqualityCheck(..) )
import StreamPatch.HFunctorList
import Data.Vinyl

import Data.Text ( Text )
import Numeric.Natural

ex :: Patch (SIx Natural) '[Compare.Meta ('ViaEq 'Exact)] Text
ex = Patch "no way this works LMAO" (SIx 0) $ HFunctorList $ Flap (Compare.Meta Nothing) :& RNil

ex2 :: Patch (SIx Natural) '[] Text
ex2 = Patch "no way this works LMAO" (SIx 0) $ HFunctorList RNil
