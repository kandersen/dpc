module NetSim.Util(
  oneOfP,
  oneOf,
  snoc,
  groupsOf,
  stepThrough
) where

import Control.Applicative
import Data.Foldable

oneOfP :: Alternative m => (a -> Bool) -> [a] -> m (a, [a])
oneOfP _ [] = empty
oneOfP p (x:xs) = 
  (if p x then pure (x, xs) else empty) <|> 
  (replace <$> oneOfP p xs)
  where
    replace (a, ys) = (a, x:ys)

oneOf :: Alternative m => [a] -> m (a, [a])
oneOf = oneOfP (const True)

snoc :: [a] -> a -> [a]
snoc xs a = foldr (:) [a] xs

groupsOf :: Int -> [a] -> [[a]]
groupsOf n xs = case splitAt n xs of
  (grp, []) -> [grp]
  (grp, xs') -> grp : groupsOf n xs'

stepThrough :: (a -> String) -> [a] -> IO ()
stepThrough pp = traverse_ (printAndAwait . pp)
  where
    printAndAwait x = putStrLn x >> getLine