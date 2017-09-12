import Data.List

takuzuStrings :: Int -> [String]
takuzuStrings n = sort $ nub $ filter f all
  where
    all :: [String]
    all = permutations $ (replicate (n `div` 2) '0') ++ (replicate (n `div` 2) '1')
    f :: String -> Bool
    f "" = True
    f (_:[]) = True
    f (_:_:[]) = True
    f (x:y:z:xs) = if x /= y
                      then f (y:z:xs)
                      else if y /= z
                        then f (z:xs)
                        else False

readBinary :: String -> Int
readBinary string = f string 0
  where
    f [] n = n
    f ('1':xs) n = f xs $ n * 2 + 1
    f ('0':xs) n = f xs $ n * 2

showBinary :: Int -> String
showBinary 0 = []
showBinary x = curr : (showBinary ( x `div` 2))
  where
    curr :: Char
    curr = case even x of
             True -> '0'
             False -> '1'

wrapper :: [String] -> [String]
wrapper (a:_) = takuzuStrings (read a::Int)

main =  print . wrapper . words =<< getLine
