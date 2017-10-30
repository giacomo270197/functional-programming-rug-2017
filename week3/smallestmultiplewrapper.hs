import Data.Foldable

smallestMultiple :: Integer -> Integer
smallestMultiple n = product $ foldr' f [] [2..n]
  where
    f i acc = merge (fac i) acc

fac :: Integer -> [Integer]
fac n = f n primes

f :: (Integral a, Ord a) => a -> [a] -> [a]
f n (x:xs)
  | n < 2 = []
  | n < x^2 = [n]
  | n `mod` x == 0 = x:f (n `div` x) (x:xs)
  | otherwise = f n xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] b = b
merge a [] = a
merge (a:as) (b:bs)
  | a < b = a:merge as (b:bs)
  | a > b = b:merge (a:as) bs
  | a == b = b:merge as bs

-- Do not change the following wrapper code
wrapper :: String -> Integer
wrapper input = smallestMultiple (read input::Integer)

main =  print . wrapper =<< getLine

primes = [n | n<-[2..], all ((> 0).rem n) [2..n-1]]
