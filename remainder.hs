remainder :: [Double] -> [Double] -> [Double]
remainder xs ys
       | length xs < length ys         = xs
       | otherwise                     = remainder (step xs ys) ys

step :: [Double] -> [Double] -> [Double]
step (x:xs) (y:ys) = clean sub (x:xs) (map (* (round x `div` round y)) (y:ys))

sub :: [Double] -> [Double] -> [Double]
sub xs ys = [x-y | (x,y) <- listGen xs ys]

listGen :: [Double] -> [Double] -> [(Double, Double)]
listGen xs ys = zip xs (fill ys (length xs -length ys))

fill :: [Double] -> Int -> [Double]
fill xs n = xs ++ replicate n 0
 
clean :: [Double] -> [Double]
clean (x:xs)
     |  x /= 0      = xs
     |  otherwise   = clean xs
