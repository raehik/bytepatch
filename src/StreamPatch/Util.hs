module StreamPatch.Util where

import           Data.Foldable ( sequenceA_ )

-- lol. ty hw-kafka-client
traverseM
    :: (Traversable t, Applicative f, Monad m)
    => (v -> m (f v'))
    -> t v
    -> m (f (t v'))
traverseM f xs = sequenceA <$> traverse f xs

-- wonder if this is the correct type? oh well it works
traverseM_
    :: (Traversable t, Applicative f, Monad m)
    => (v -> m (f ()))
    -> t v
    -> m (f ())
traverseM_ f xs = sequenceA_ <$> traverse f xs
