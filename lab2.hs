import Data.List

type Name   = String
type Domain = [Integer]
data Expr = Val Integer
          | Var Name
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
          | Expr :/: Expr
          | Expr :%: Expr

instance Show Expr where
  show (Val x) = show x
  show (Var x) = x
  show (a :+: b) = inbetween a b "+"
  show (a :-: b) = inbetween a b "-"
  show (a :*: b) = inbetween a b "*"
  show (a :/: b) = inbetween a b "/"
  show (a :%: b) = inbetween a b "%"
  

embrace :: String -> String
embrace x = "(" ++ x ++ ")"

inbetween :: Show a => a -> a -> String -> String
inbetween a b x = embrace $ show a ++ x ++ show b

vars = nub . sort . f
  where
    f :: Expr -> [String]
    f (Val _) = []
    f (Var x) = [x]
    f (a :+: b) = vars a ++ vars b
    f (a :-: b) = vars a ++ vars b
    f (a :*: b) = vars a ++ vars b
    f (a :/: b) = vars a ++ vars b
    f (a :%: b) = vars a ++ vars b

type Valuation = [(Name, Integer)]

evalExpr :: Expr -> Valuation -> Integer
evalExpr (Val x) v = x
evalExpr (Var x) v = case lookup x v of
  Just a -> a
  Nothing -> error $ "var " ++ x ++ " not found"
evalExpr (a :+: b) v = evalExpr a v + evalExpr b v
evalExpr (a :-: b) v = evalExpr a v - evalExpr b v
evalExpr (a :*: b) v = evalExpr a v * evalExpr b v
evalExpr (a :/: b) v = evalExpr a v `div` evalExpr b v
evalExpr (a :%: b) v = evalExpr a v `mod` evalExpr b v

valuations :: [(Name, Domain)] -> [Valuation]
valuations [(name, domain)] = [[(name, x)] | x <- domain]
valuations ((name, domain):xs) =
  [(name,x):xs | x<-domain, xs<-valuations xs ]

-- [("a", [1..10]),("b", [1..10]),("c", [1..10])]
-- genVal :: Integer -> [(Name, Domain)]
-- genVal x = filter (\[(_, a),(_,b),(_,c)] -> a <= b) x
--   where a = [("a", [1..x]),("b", [1..x]),("c", [1..x])]

rev :: [Int] -> Int -> [Int]
rev [] _ = []
rev (1:xs) curr = curr:rev xs (if curr == 1 then 2 else 1)
rev (2:xs) curr = curr:curr:rev xs (if curr == 1 then 2 else 1)

