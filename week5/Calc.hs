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
