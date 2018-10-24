
{- 
The Interpolation Search.
For the entire definition, see Problem 1 at http://bit.ly/2vH9Lco 
-}
interposearch :: Int -> [Int] -> Int
interposearch n [] = error "eRROR: empty array or sub-array reduces to zero"
interposearch n xs
   | ( (pos > hi) || (pos < lo) ) = error "eRROR: false index, not found element"
   | ( n == el ) = pos 
   | ( n > el ) = interposearch n (take pos xs)
   | otherwise = interposearch n (drop pos xs)
     where pos = lo + ((n-xs!!lo)*(hi-lo) `div` (xs!!hi - xs!!lo))
           el = (xs!!pos)
           lo = 0
           hi = (length xs)- 1

