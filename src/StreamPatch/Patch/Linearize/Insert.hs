module StreamPatch.Patch.Linearize.Insert where

import Control.Monad.State
import Data.List qualified as List
import StreamPatch.Util ( traverseM )

{-
-- Result seeks are non-negative, so natural-like types are safe to use.
linearizeInsert
    :: (Integral sf, Integral st)
    => [Patch sf fs a] -> Either (Error (Patch sf fs a)) [Patch st fs a]
linearizeInsert = linearize patchSeek (\s p -> p { patchSeek = s })
-}

-- | Linearize some list of @a@s.
--
-- For non-empty lists, the result is a tuple of the first @a@, followed by the
-- linearized @a@s (converted to @b@). Linearized values are non-negative, so
-- natural-like types are safe to use for @st@.
linearize
    :: (Integral sf, Integral st)
    => (a -> sf)
    -> (st -> a -> b)
    -> [a] -> Either (a, a) (Maybe (a, [b]))
linearize f g as =
    case List.sortBy cmp as of
      []      -> Right Nothing
      (a:as') -> do
        bs <- evalState (traverseM go as') a
        Right $ Just (a, bs)
  where
    cmp a1 a2 = compare (f a1) (f a2)
    go a = do
        a' <- get
        let s' = fromIntegral $ f a - f a'
        if s' == 0
        then return $ Left (a, a')
        else put a >> return (Right (g s' a))

linearize' :: (Integral sf, Integral st) => [sf] -> Either (sf, sf) (Maybe (sf, [st]))
linearize' = linearize id const
