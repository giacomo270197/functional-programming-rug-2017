import Data.Char

data Value
  = Number Int
  | Plus | Min | Times | Div

rpnEval :: String -> Int
rpnEval xs = eval (map parse tokens) []
  where
    tokens :: [String]
    tokens = words xs

parse :: String -> Value
parse item
  | all isDigit item = Number $ read item
  | otherwise = case item of
                  "+" -> Plus
                  "-" -> Min
                  "*" -> Times
                  "/" -> Div
                  x -> error $ "not on operator " ++ x

eval :: [Value] -> [Int] -> Int
eval [] (x:_) = x
eval (x:xs) acum = case x of
  Number y -> eval xs (y:acum)
  Plus  -> eval xs (a + b:restAcum)
  Min   -> eval xs (b - a:restAcum)
  Times -> eval xs (a * b:restAcum)
  Div   -> eval xs (b `div` a:restAcum)
  where
    a:b:restAcum = acum

main = print . rpnEval =<< getLine
