{-
the Queue
-}
data Queue a = Queue [a] deriving (Show)

{-- this function initiate an empty queue --}
queue = (Queue [])

{-- this function verify is the queue is empty --}
isEmpty :: Queue a -> Bool
isEmpty (Queue []) = True
isEmpty (Queue xs) = False

{-- this function adds an element to the end of the queue --}
push :: a -> Queue a -> Queue a
push n (Queue xs) = Queue (xs ++ [n])

{-- this function receives a non-empty queue and removes its head --}
pop :: Queue a -> (a, Queue a)
pop (Queue []) = error "eRROR: empty queue"
pop (Queue (x:xs)) = (x, Queue xs)

{-- this function gets the peek of a non-empty queue --}
peek :: Queue a -> a
peek (Queue []) = error "eRROR: empty queue"
peek (Queue (x:xs)) = x
