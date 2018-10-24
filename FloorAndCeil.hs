
{-
Floor and ceil function.

@parameters n: a value, xs: a list
@return a tuple (Maybe a, Maybe a) = (a, b), where 
a: the higher value of the values lower then n,  
b: the lowest value of the values higher then n.
   Obs: if the value do not exist, it is Nothing
-}
floorandceil :: Ord a => a -> [a] -> (Maybe a, Maybe a)
floorandceil n xs = (floor, ceil)
     where floor = if(null smallers) then (Nothing) else Just (maximum smallers)
           ceil = if(null largers) then (Nothing) else Just (minimum largers)
           smallers = filter (<= n) xs
           largers = filter (>= n) xs
