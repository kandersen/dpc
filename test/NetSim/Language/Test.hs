module NetSim.Language.Test where

import NetSim.Language
import qualified Data.Map as Map
--
-- Tests
--
simpleConf :: MonadDiSeL m => m a -> Configuration m a
simpleConf code = Configuration {
  _confNodes = [0],
  _confSoup = [],
  _confNodeStates = Map.fromList [(0, code)]
  }

simpleConf' :: MonadDiSeL m => [m a] -> Configuration m a
simpleConf' codes = Configuration {
  _confNodes = [0..length codes - 1],
  _confSoup = [],
  _confNodeStates = Map.fromList $ zip [0..] codes
}

test1 :: MonadDiSeL m => m Int
test1 = return 42

test2a :: MonadDiSeL m => m Int
test2a = send (0, "test", [42], 1) >> return 0

test2b :: MonadDiSeL m => m Int
test2b = spinReceive 0 ["test"] >>= (\(_, _, [ans],_) -> return ans)

test3s :: MonadDiSeL m => m a
test3s = do
  (_, _, [x], c) <- spinReceive 0 ["testQ"]
  send (0, "testA", [x + 1], c)
  test3s

test3c :: MonadDiSeL m => Int -> m Int
test3c n = do
  send (0, "testQ", [n], 0 )
  (_, _, [ans], _) <- spinReceive 0 ["testA"]
  return ans

test4 :: MonadDiSeL m => m [Int]
test4 = par (pure <$> [0..10]) pure

test3s' :: MonadDiSeL m => m a
test3s' = par [test3s, pure undefined] (const test3s')