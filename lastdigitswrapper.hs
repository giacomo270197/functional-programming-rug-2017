{- Insert here your own code. The type of lastDigits must be:

lastDigits  :: Integer -> Int -> [Integer]

-}

wrapper :: [String] -> [Integer]
wrapper (a:b:_) = lastDigits (read a::Integer) (read b::Int)

main =  print . wrapper . words =<< getLine
