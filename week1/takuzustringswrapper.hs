import Data.List

takuzuStrings :: Int -> [String]
takuzuStrings = filter follows . filter count . f

f :: Int -> [String]
f 1 = ["0","1"]
f a = do
  x <- ['0','1']
  y <- f (a-1)
  return $ x:y

count :: String -> Bool
count x = f x == 0
  where
    f :: String -> Int
    f [] = 0
    f ('1':xs) = 1 + (f xs)
    f ('0':xs) = (-1) + (f xs) 

follows :: String -> Bool
follows (x:y:z:xs)
  | x /= y = follows (y:z:xs)
  | y /= z = follows (z:xs)
  | otherwise = False
follows _ = True

wrapper :: [String] -> [String]
wrapper (a:_) = takuzuStrings (read a::Int)

main =  print . wrapper . words =<< getLine
