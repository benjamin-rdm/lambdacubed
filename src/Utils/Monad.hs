module Utils.Monad where

import Control.Monad (when, unless)
import Data.List (sortOn)

sortOnM :: (Monad m, Ord b1) => (b2 -> m b1) -> [b2] -> m [b2]
sortOnM f xs = do
  ys <- mapM (\x -> (,) x <$> f x) xs
  pure (map fst (sortOn snd ys))

whenM :: Monad m => m Bool -> m () -> m ()
whenM x a = do
  xv <- x
  when xv a

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM x a = do
  xv <- x
  unless xv a

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = pure ()
whenJust (Just x) f = f x