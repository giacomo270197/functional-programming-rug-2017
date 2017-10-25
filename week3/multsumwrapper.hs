import Data.Foldable
import Data.List

multsum :: Integer -> [Integer] -> Integer
multsum n = product . take (fromInteger n) . multiples

mults :: Integer -> [Integer]
mults n = [n,n*2..]

multiples :: [Integer] -> [Integer]
multiples xs = multiMerge $ map mults xs

multiMerge :: (Ord a) => [[a]] -> [a]
multiMerge = nub . sort . concat

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] b = b
merge a [] = a
merge (a:as) (b:bs)
  | a < b = a:merge as (b:bs)
  | a > b = b:merge (a:as) bs
  | a == b = b:merge as bs

-- Do not change the following wrapper code

stringToList :: String -> [Integer]
stringToList xs = map read (words input)::[Integer]
  where input = [if elem x ",[]" then ' ' else x | x <- xs]
  
wrapper :: String -> Integer
wrapper input = multsum (head xs) (tail xs)
  where xs = stringToList input

main =  print . wrapper =<< getLine
