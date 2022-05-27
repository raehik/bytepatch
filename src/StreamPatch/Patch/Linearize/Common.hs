module StreamPatch.Patch.Linearize.Common where

import StreamPatch.Patch

comparePatchSeeks :: Ord s => Patch s fs a -> Patch s fs a -> Ordering
comparePatchSeeks p1 p2 = compare (patchSeek p1) (patchSeek p2)
