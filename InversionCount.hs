{-
Inversion Count for an array indicates – how far (or close) the array is from being sorted. If array is already sorted then inversion count is 0. If array is sorted in reverse order that inversion	count is the maximum.	
Example: Input [8,2,4,2,1,0,-1]. Output: 19
-}
inversions :: [Int] -> Int
inversions [] = 0
inversions (x:xs) = (length (filter (<x) xs)) + inversions xs
