import Data.Char

cipherEncode :: Int -> String -> String
cipherEncode n input = f input (map (*n) [1..])
  where
    f :: [Char] -> [Int] -> [Char]
    f [] _ = []
    f (x:xs) (y:ys) = case isUpper x of 
                        False -> x:f xs (y:ys)
                        True -> (toEnum . wrapAround . (+ (negate y)) . fromEnum) x:f xs ys
    wrapAround x 
      | x > fromEnum 'Z' = x - 26
      | x < fromEnum 'A' = x + 26
      | otherwise = x
      

cipherDecode :: Int -> String -> String
cipherDecode n = cipherEncode (negate n)

wrapper :: String -> String
wrapper line
  | cmd == "ENCODE"  = cipherEncode key txt
  | cmd == "DECODE"  = cipherDecode key txt
  where
    str  = dropWhile (not.isAlpha) line
    cmd  = takeWhile isAlpha str
    tail = dropWhile (not.isDigit) str
    key = read (takeWhile isDigit tail)::Int
    txt = dropWhile (not.isAlpha) (dropWhile isDigit tail)

main =  print . wrapper =<< getLine
