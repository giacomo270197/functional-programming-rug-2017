import Data.List

type Takuzu = [String]

main = print . isCorrectTakuzu . lines =<< getContents

-- True is there is only 1 working takuzu
isCorrectTakuzu :: Takuzu -> Bool
isCorrectTakuzu = (== 1) . length . countTakuzus 

-- only has to check the transposed version
-- normal version is already check in the generation function
countTakuzus :: Takuzu -> [Takuzu]
countTakuzus = filter check . possibleTakuzus

-- | check functions
check :: Takuzu -> Bool
check = all checkRow . transpose

checkRow :: String -> Bool
checkRow x = countSingle x && followsSingle x

-- check for equal amount of 1's and 0's
countSingle x = f x == 0
  where
    f :: String -> Int
    f [] = 0
    f ('1':xs) = 1 + (f xs)
    f ('0':xs) = (-1) + (f xs) 

-- checks for repeating characters
followsSingle :: String -> Bool
followsSingle (x:y:z:xs)
  | x /= y = followsSingle (y:z:xs)
  | y /= z = followsSingle (z:xs)
  | otherwise = False
followsSingle _ = True

-- | generation functions
-- generates all posible takazus by replacing '.'
possibleTakuzus :: Takuzu -> [Takuzu]
possibleTakuzus [x] =
  map (:[]) $ filter checkRow $ possibleTakuzusSingleRow x
possibleTakuzus (x:xs) = 
  [ thisRow:rest
  | thisRow <- filter checkRow (possibleTakuzusSingleRow x)
  , rest <- possibleTakuzus xs]

-- generates all posible options in a row
possibleTakuzusSingleRow :: String -> [String]
possibleTakuzusSingleRow ['.'] = ["0","1"]
possibleTakuzusSingleRow [x] = return . return $ x
possibleTakuzusSingleRow ('.':xs) = 
  [ curr:rest
  | curr <- ['0','1']
  , rest <- possibleTakuzusSingleRow xs]
possibleTakuzusSingleRow (x:xs) = [x:rest | rest <- possibleTakuzusSingleRow xs]
