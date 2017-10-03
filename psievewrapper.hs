primes :: [Integer]
primes = 2 : [2*x+1 | x <- [1..] , not (x `elem` (checkIt x))]

checkIt :: Integer -> [Integer]
checkIt x = filter( <= x ) [i+j+2*i*j| i<-[1.. limit1 x], j<-[i..limit2 x i]]
  where 
    limit1 x = floor(sqrt(fromIntegral(x `div` 2)))
    limit2 x y = floor(((fromIntegral x)-(fromIntegral y))/(2*(fromIntegral y) + 1))

-- Do not change the following wrapper code
wrapper :: String -> [Integer]
wrapper input = take (read input::Int) primes

main =  print . wrapper =<< getLine
