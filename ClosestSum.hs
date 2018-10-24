import Data.List (minimumBy)
import Data.Ord (comparing)

{- 
Closest sum to the value n in a pair combined from a given array.  
@return a pair of values which is the closest sum to the value n,
	Example: [1,3,4,7,10] 15. Output: 4 and 10.
			 [1,3,4,7,10] 8. Output: 4 and 4.
-}
closestsumto' n = snd . minimumBy (comparing fst) . aux_pair
  where 
    aux_pair xs = sums xs (reverse xs)
    sums [] _ = []
    sums _ [] = []
    sums (x:xs) (y:ys) 
      | n < s     = (s - n, (x, y)) : sums (x:xs) ys
      | otherwise = (n - s, (x, y)) : sums xs (y:ys)
      where
        s = x + y
closestsumto n xs = let result = closestsumto' n xs 
    in show (fst (result)) ++ " and " ++ show (snd (result))
