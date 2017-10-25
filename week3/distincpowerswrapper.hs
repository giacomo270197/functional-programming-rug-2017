distinctPowers :: Int -> Int -> Int
distinctPowers m n = length(foldr union [] [take (n-1) (powers a) | a <- [2..m]])

powers :: Int -> [Int]
powers a = [a^x | x <- [2..]]

union :: [Int] -> [Int] -> [Int]
union [] ys = ys
union xs [] = xs
union (x:xs) (y:ys)
    | x < y     = x : union xs (y:ys)
    | x > y     = y : union (x:xs) ys
    | x == y    = x : union xs ys

-- Do not change the following wrapper code
wrapper :: [String] -> Int
wrapper (a:b:_) = distinctPowers (read a::Int) (read b::Int)

main =  print . wrapper . words =<< getLine
