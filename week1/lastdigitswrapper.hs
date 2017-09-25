lastDigits :: Integer -> Int -> [Integer]
lastDigits = undefined


wrapper :: [String] -> [Integer]
wrapper (a:b:_) = lastDigits (read a::Integer) (read b::Int)

main =  print . wrapper . words =<< getLine
