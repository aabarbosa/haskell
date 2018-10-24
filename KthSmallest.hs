import Data.List (sort)

{-
Given an array and a number k where k is smaller than size of array,
we need to find the k’th smallest element in the given array. 
It is given that all array elements are distinct.
-}
ksmallest k xs = ys!!(k-1)
                   where ys = sort xs
