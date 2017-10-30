{- Insert here your own code. The type of polDivision must be:

polDivision :: [Double] -> [Double] -> ([Double],[Double])

-}

wrapper :: String -> ([Double],[Double])
wrapper line = polDivision (makeList num) (makeList denom)
  where
    num = takeWhile (/= '/') line
    denom = tail (dropWhile (/= '/') line)
    makeList str = map (\s -> read s::Double) (words str)

main =  print . wrapper =<< getLine
