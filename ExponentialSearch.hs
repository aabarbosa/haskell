import Data.List


-- exponential search with output
exposearch :: Ord a => a -> [a] -> String
exposearch n xs
   | (null xs) = "Empty list"
   | (search == -1) = "Element not found"
   | otherwise = "Element found at index " ++ (show ( search ))
       where search = exposearch' 1 n xs

exposearch' :: Ord a => Int -> a -> [a] -> Int
exposearch' i n xs 
   | (i >= hi) = binarysearch (i `div` 2) hi n xs
   | ((last sublist) >= n) = binarysearch (i `div` 2) i n xs -- do binary search,
   | otherwise = exposearch' (i*2) n xs -- find index.
       where hi = (length xs) - 1
             sublist = take i xs
             binarysearch lo hi n xs  -- binary search
               | ( lo > hi ) = -1
               | ( el > n ) = binarysearch lo (mid - 1) n xs
               | ( el < n ) = binarysearch (mid + 1) hi n xs
               | otherwise = mid
                  where mid = lo + ((hi - lo) `div` 2)
                        el = xs!!mid
