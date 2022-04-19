module Poker.Math where
    
import Data.List
    
nOverKIndices :: Int -> Int -> [[Int]]
nOverKIndices n k 
    | k <= 0 || k > n  = []
    | k == 1 || k == n = map (\s -> [s]) [0 .. n-1]
    | k <= 2           = [[i,j] | j <- [0 .. n-1], i <- [j+1 .. n-1]]
    | otherwise        = [a:h:t | h:t <- nOverKIndices n (k-1), a <- [h+1 .. n-1 ]]

nOverK :: Eq a => Int -> [a] -> [[a]]
nOverK k list = map fromIndex $ nOverKIndices n k where
    fromIndex = map (set !!)
    n = length set
    set = nub list
