module NetSim.Util where

import Control.Applicative

oneOf :: Alternative m => [a] -> m (a, [a])
oneOf = go []
  where
    go _ [] = empty
    go l (x:rs) = pure (x, l ++ rs) <|> go (x : l) rs
