module NetSim.Language where

newtype Eff a = Eff a

class Semantics repr where
  int :: Int -> repr Int
  bool :: Bool -> repr Bool
  unit :: repr ()
  bind :: repr (Eff a) -> repr (a -> Eff b) -> repr (Eff b)
  return :: repr a -> repr a
  fix :: (repr a -> repr a) -> repr a
  letbind :: repr a -> (repr a -> repr b) -> repr b
  send :: repr Int -> repr (Eff ())
  receive :: repr (Eff Int)

-- data Term = Bind Var Term Term
--           | Var Var
--           | Fix Var Type Term
--           | If Term Term Term
--           | Return Term
--           | Let Var Term Term
--           | TermUnit
--           | TermInt Integer
--           | TermBool Bool
--           | Send
--           | Receive

-- value :: Term -> Bool
-- value TermUnit = True
-- value (TermInt _) = True
-- value (TermBool _) = True
-- value _ = False

-- subst :: Term -> Var -> (Term -> Term)
-- subst = undefined

-- step :: Term -> Maybe Term
-- step (Bind x (Return v) k) | value k   = pure $ subst v x k
--                            | otherwise = do
--                                v' <- step v
--                                pure $ Bind x (Return v') k
-- step (Bind x )
