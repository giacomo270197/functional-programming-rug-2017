import Data.List

numberOfPalindromicComposites :: Integer -> Integer
numberOfPalindromicComposites n = toInteger . length . takeWhile (< n) $ f

palins n = [s ++ tail (reverse s) | s <- gen n] ++ [s ++ reverse s | s <- gen n]

palinsinf = concatMap palins [0..]

f = filter (\x -> primeTest x 0 primes) . map read . filter ((/='0') . head) $ palinsinf

primeTest :: Integer -> Integer -> [Integer] -> Bool
primeTest n t (x:xs)
  | n == x && t /= 1 = False
  | (t == 2 && n == 1) = True
  | t == 2 = False
  | t == 0 && n == 1 = False
  -- | n < x*2 = False
  | (n `mod` x == 0) = primeTest (n `div` x) (t+1) (x:xs)
  | otherwise = primeTest n t xs

gen :: Integer -> [[Char]]
gen 0 = map (:[]) ['0'..'9']
gen n = [a:b | a <- ['0'..'9'], b <- gen (n-1)]

primes = 2 : 3 : 5 : primes'
  where
    isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
    primes' = 7 : filter (isPrime primes') (scanl (+) 11 $ cycle [2,4,2,4,6,2,6,4])

-- Do not change the following wrapper code
wrapper :: String -> Integer
wrapper input = numberOfPalindromicComposites (read input::Integer)

main =  print . wrapper =<< getLine

