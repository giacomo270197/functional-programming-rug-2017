import Data.List 
import Data.Ord

get1 :: (a, a, a) -> a
get1 (x,_,_) = x

get2 :: (a, a, a) -> a
get2 (_,x,_) = x

get3 :: (a, a, a) -> a
get3 (_,_,x) = x

type Position = (Int, Int)
type Setup = (Position, Position, Position)

board :: [Position]
board = [(x, y) | x <- [1..8], y <- [1..8]]

rookMoves :: Position -> [Position]
rookMoves y = filter (\x -> fst x == fst y || snd x == snd y) board

kingMoves :: Position -> [Position]
kingMoves y = filter (\x -> distance x y) board

conf :: Position -> Position -> Position -> Int
conf kw rw kb = conflicts' kw rw kb 0 
    where conflicts' kw rw kb found
              |  fst rw == fst kb         = conflicts' kw (999 , snd rw) kb (found+1)
              |  distance kw kb           = found + 1
              |  otherwise                = found 

distance :: Position -> Position -> Bool
distance x y
     |  fst x - fst y <= 1 && fst x - fst y >= -1   = next x y
     |  otherwise                                   = False
    where next a b
              |  snd a - snd b <= 1 && snd a - snd b >= -1   = True
              |  otherwise                                   = False

                 
bfs :: [Setup] -> [Setup] -> Bool -> Int -> Int
bfs pos vis turn depth
      | checkMate  (head pos)     =   0
      | otherwise                 =   bfs' pos vis turn depth

bfs' :: [Setup] -> [Setup] -> Bool -> Int -> Int
bfs' pos vis turn depth
    | z == ((6,7),(8,3),(8,7))      =    999
    | nbg z == []                   =    depth     
    | not turn                      =    bfs' (pos ++ f vis (nbg z)) (vis ++ f vis (nbg z)) (not turn) (depth + 1)
    | otherwise                     =    bfs' (pos ++ f vis (nwg z)) (vis ++ f vis (nwg z)) (not turn) (depth + 1)
   where   f a = filter (\x -> not(x `elem` a))
           z = head pos

checkMate :: Setup -> Bool
checkMate pos
          | head (sort [conf (get1 pos) (get2 pos) a | a <- kingMoves (get3 pos)]) /= 0           = True 
          | otherwise                                                                             = False

nbg :: Setup -> [Setup]  
nbg a = [(get1(a), get2(a), x) | x<-kingMoves(get3 a), conf (get1 a) (get2 a) x == 0, (get1(a), get2(a), x) /= a]

nwg :: Setup -> [Setup]
nwg a = [(get1 a , x , get3 a) | x <- rookMoves(get2 a), (get1 a, x , get3 a) /= a{-, getCloser x-}] ++
        [(x , get2 a , get3 a) | x <- kingMoves(get1 a), not(distance x (get3 a)), (x,get2 a ,get3 a ) /= a]
    where getCloser z =  (fst(get3 a)-fst(get2 a))+(snd(get3 a)-snd(get2 a)) <= (fst(get3 a)-(fst z))+(snd(get3 a)-(snd z))       







