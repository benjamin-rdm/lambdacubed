module Utils.Monad where

import Data.List (sortOn)

sortOnM :: (Monad m, Ord b1) => (b2 -> m b1) -> [b2] -> m [b2]
sortOnM f xs = do
  ys <- mapM (\x -> (,) x <$> f x) xs
  pure (map fst (sortOn snd ys))

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = pure ()
whenJust (Just x) f = f x