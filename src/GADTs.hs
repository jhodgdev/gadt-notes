module GADTs where

import Data.Type.Equality

-- Vanilla data types and their limitations.
data VTerm a
  = VLit a
  | VSucc (VTerm a)
  | VIsZero (VTerm a)

nonsense = VSucc (VLit True)

-- Generalised algebraic data type example.
data Term a :: * where
  Lit :: a -> Term a
  Succ :: Term Int -> Term Int
  IsZero :: Term Int -> Term Bool
  If :: Term Bool -> Term a -> Term a -> Term a

-- willFail = Succ (Lit True)

eval :: Term a -> a
eval (Lit i) = i
eval (Succ t) = 1 + eval t
eval (IsZero i) = eval i == 0
eval (If b e1 e2) =
  if eval b
    then eval e1
    else eval e2

f :: a -> a -> (a, a)
f = (,)

f' :: (a ~ b) => a -> b -> (a, b)
f' = f

-- Kind signatures.
data Vec :: * -> * -> * where
  Nil :: Vec n a
  Cons :: a -> Vec n a -> Vec n a

data Fix :: (* -> *) -> * where
  In :: f (Fix f) -> Fix f
