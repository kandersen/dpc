module NetSim.Util where

import Control.Applicative

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
snoc [] a = [a]
snoc (x:xs) a = x : snoc xs a