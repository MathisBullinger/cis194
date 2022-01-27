import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit n)   = n
eval (Add a b) = eval a + (eval b)
eval (Mul a b) = eval a * (eval b)

evalStr :: String -> Maybe Integer
evalStr str = case expr of
                Just x -> Just $ eval x 
                _       -> Nothing
              where expr = parseExp Lit Add Mul str 

class Expr a where
  lit :: Integer -> a 
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*) 

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Show, Eq, Ord)
newtype Mod7 = Mod7 Integer deriving (Show, Eq)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 a) (Mod7 b) = Mod7 (a + b `mod` 7)
  mul (Mod7 a) (Mod7 b) = Mod7 (a * b `mod` 7)

