module Utils.Monad where

import Control.Monad (unless, when)
import Data.Foldable ( for_ )
import Data.List (sortOn)

anyM :: (Monad m) => (t -> m Bool) -> [t] -> m Bool
anyM p = go
  where
    go [] = pure False
    go (x : xs) = do
      b <- p x
      if b then pure True else go xs

sortOnM :: (Monad m, Ord b1) => (b2 -> m b1) -> [b2] -> m [b2]
sortOnM f xs = do
  ys <- mapM (\x -> (,) x <$> f x) xs
  pure (map fst (sortOn snd ys))

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM x a = do
  xv <- x
  when xv a

unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM x a = do
  xv <- x
  unless xv a

whenJust :: Applicative f => Maybe t -> (t -> f ()) -> f ()
whenJust Nothing _ = pure ()
whenJust (Just x) f = f x

whenJustM :: (Monad m) => m (Maybe a) -> (a -> m b) -> m ()
whenJustM a f = do
  va <- a
  for_ va f