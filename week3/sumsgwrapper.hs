sumsg :: Integer -> Integer
sumsg n = sum $ map sg [1..n]

f :: Integer -> Integer
f = sum . map fac . toDi

fac :: Integer -> Integer
fac n = product [1..n]

sf :: Integer -> Integer
sf = sum . toDi . f

g :: Integer -> Integer
g i = f 1 i
  where
    f n i
      -- | n == i = f (n+1) i
      | sf n == i = n
      | otherwise = f (n+1) i

sg :: Integer -> Integer
sg = sum . toDi . g

toDi :: Integer -> [Integer]
toDi 0 = []
toDi n = n `mod` 10:toDi (n `div` 10)

-- Do not change the following wrapper code
wrapper :: String -> Integer
wrapper input = sumsg (read input::Integer)

main =  print . wrapper =<< getLine
