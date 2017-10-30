countHappyNumbers :: Int -> Int -> Int
countHappyNumbers a b = length $ filter isHappy [a..b]

isHappy :: Int -> Bool
isHappy 1 = True
isHappy 4 = False
isHappy x = isHappy . sum . map (^2) $ (items x)
items 0 = []
items x = x `mod` 10:items (x `div` 10)

wrapper :: [String] -> Int
wrapper (a:b:_) = countHappyNumbers (read a::Int) (read b::Int)

main =  print . wrapper . words =<< getLine
