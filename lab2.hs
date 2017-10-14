{-# LANGUAGE DeriveFunctor #-}

import Data.List (sort, nub)
import Data.Char (isAlpha, isDigit, isSpace)
import Control.Applicative ((<|>))

type Name   = String
type Domain = [Integer]
data Expr = Val Integer
          | Var Name
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
          | Expr :/: Expr
          | Expr :%: Expr

-- | exercise 1
instance Show Expr where
  show (Val x) = show x
  show (Var x) = x
  show (a :+: b) = show a ++ "+" ++ show b
  show (a :-: b) = show a ++ "-" ++ show b
  show (a :*: b) = showf a ++ "*" ++ showf b
  show (a :/: b) = showf a ++ "/" ++ showf b
  show (a :%: b) = showf a ++ "%" ++ showf b

-- | show function while in a factor
showf :: Expr -> String
showf a
  | isTerm a = embrace $ show a
  | otherwise = show a

isTerm :: Expr -> Bool
isTerm (_ :+: _) = True
isTerm (_ :-: _) = True
isTerm _ = False

embrace :: String -> String
embrace x = "(" ++ x ++ ")"

-- | exercise 2
vars :: Expr -> [String]
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

-- | exercise 3
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

-- | exercise 4? (there is no 4 in the pdf)
valuations :: [(Name, Domain)] -> [Valuation]
valuations [(name, domain)] = [[(name, x)] | x <- domain]
valuations ((name, domain):xs) =
  [(name,x):xs | x<-domain, xs<-valuations xs ]

pytriples :: Integer -> [[(String, Integer)]]
pytriples = filter (\x -> evalParse "a*a+b*b-c*c" x == 0) . filteredVal
  where 
    filteredVal = filter (\[a,b,c] -> snd a <= snd b) . allVal
    allVal n = valuations [("a",[1..n]),("b",[1..n]),("c",[1..n])]

-- | exercise 5
parse = expr . tokenize
evalParse = evalExpr . snd . parse

tokenize :: String -> [String]
tokenize [] = []
tokenize (x:xs)
  | isAlpha x = (x:takeWhile isAlpha xs):tokenize (dropWhile isAlpha xs)
  | isDigit x = (x:takeWhile isDigit xs):tokenize (dropWhile isDigit xs)
  | elem x "*/-+()" = [x]:tokenize xs
  | isSpace x = tokenize xs
  | otherwise = error $ "unknown char '" ++ [x] ++ "'"

type Parse x = [String] -> Maybe ([String], x)

val :: Parse Expr
val [] = error "Parse error"
val (x:xs)
  | all isAlpha x = Just (xs, Var x)
  | all isDigit x = Just (xs, Val $ read x)
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
choice actions x = foldr (<|>) Nothing . map (\f -> f x) $ actions

-- | parse 0 or more times the parser in the first argument
many :: Parse a -> [String] -> ([String], [a])-- parse [a] -- [String] -> [a]
many f input = case f input of
                 Just (newInput, res) -> map (++ [res]) $ many f newInput
                 Nothing -> (input, [])

term :: Parse (Expr -> Expr)
term input = Just $ foldr (.) id <$> many f input
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
