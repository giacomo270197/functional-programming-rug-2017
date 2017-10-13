{-# LANGUAGE DeriveFunctor #-}

import Data.List
import Data.Char
import Control.Applicative hiding (many, some)
import Data.Foldable
import Data.Bifunctor

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
  
main = return ()

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

takeWhileTulple :: (a -> Bool) -> [a] -> ([a],[a])
takeWhileTulple f [] = ([],[])
takeWhileTulple f (x:xs)
  | f x = (x:) <$> takeWhileTulple f xs
  | otherwise = (x:xs,[])

tokenize :: String -> [String]
tokenize [] = []
tokenize (x:xs)
  | isAlpha x = let (rest, tok) = takeWhileTulple isAlpha xs
                in (x:tok):tokenize rest
  | isDigit x = let (rest, tok) = takeWhileTulple isNumber xs
                 in (x:tok):tokenize rest
  | elem x "*/-+()" = [x]:tokenize xs
  | isSpace x = tokenize xs

------------------------------

type Parse x = [String] -> Maybe ([String], x)

val :: Parse Expr
val [] = error "Parse error"
val (x:xs)
  | all isAlpha x = Just (xs, Var x)
  | all isNumber x = Just (xs, Val $ read x)
  | x == "(" = let (newInput, out) = expr xs
                in case newInput of
                     (")":rest) -> Just (rest, out)
                     _ -> error "Syntax error"
  | otherwise = error "Syntax error"

-- | top level parser
expr :: [String] -> ([String], Expr)
expr input =
  let (input1, v) = maybe (error "") id $ val input
      (input2, f) = maybe (input1, id) id $ factor input1
      (input3, t) = maybe (input2, id) id $ term input2
   in (input3, t . f $ v)

-- | parse one of the parsers in the first argument, prefers the left ones
choice :: [Parse a] -> Parse a
choice actions x = foldr' (<|>) Nothing . map (\f -> f x) $ actions

-- | parse 0 or more times the parser in the first argument
many :: Parse a -> [String] -> ([String], [a])-- parse [a] -- [String] -> [a]
many f input = case f input of
                 Just (newInput, res) -> fmap (++ [res]) $ many f newInput
                 Nothing -> (input, [])

term :: Parse (Expr -> Expr)
term input = Just $ foldr' (.) id <$> many f input
  where
    f :: Parse (Expr -> Expr)
    f input = do
      (input2, sign) <- termSign input
      (input3, v) <- val input2
      (input4, b) <- factor input3
      return (input4, (`sign` (b v)))

termSign :: Parse (Expr -> Expr -> Expr)
termSign [] = Nothing
termSign (x:xs)
  | x == "+" = Just (xs,(:+:))
  | x == "-" = Just (xs,(:-:))
  | otherwise = Nothing

factor :: Parse (Expr -> Expr)
factor input = Just $ foldr (.) id <$> many f input 
  where 
    f :: Parse (Expr -> Expr)
    f input = do
      (input2, sign) <- factorSign input
      (input3, b) <- val input2
      return (input3, (`sign` b))

factorSign :: Parse (Expr -> Expr -> Expr)
factorSign [] = Nothing
factorSign (x:xs)
  | x == "*" = Just (xs,(:*:))
  | x == "/" = Just (xs,(:/:))
  | x == "%" = Just (xs,(:%:))
  | otherwise = Nothing

------ | simplify functions

-- toPos :: Expr -> Expr
-- toPos (a:-:b) = a:+:(negate b)
-- toPos (a:/:b) = a:*:(1:/:b)

simplify :: Expr -> Expr
simplify = undefined -- todo for bonus
