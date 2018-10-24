type Set a = [a]

{-
This functions gives you the power set of xs 
@parameters (x:xs): a given list with unique elements.
-}
powerset :: Set a -> Set (Set a)
powerset [] = [[]]
powerset xs@(x:ys) = [x:ps | ps <- powerset ys] ++ powerset ys


-- The power set S, namely ℘(S) is defined as the set that contains all subsets of S. Example, S = {a, b}, ℘ S = {∅, a , b , {a, b}}

