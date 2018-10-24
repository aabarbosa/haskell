{-
the Stack.
-}
-- abstract Stack data type
data Stack a = Stack ([a], Int) deriving (Show)

-- create new empty Stack
stackNew = Stack ([],0)

-- compute number of elements contained in the Stack
stackSize :: Stack a -> Int
stackSize (Stack x) = snd x

-- pop most recently added item without removing from the Stack
stackPeek :: Stack a -> a
stackPeek (Stack ([], _)) = error "stack is empty"
stackPeek (Stack x) = last (fst x)

-- test if stack is empty
stackIsEmpty :: Stack a -> Bool
stackIsEmpty (Stack stack) 
         | snd stack == 0 = True
         | otherwise = False

-- push item onto Stack
stackPush :: Stack a -> a -> Stack a
stackPush (Stack stack) element = Stack (fst stack ++ [element], snd stack + 1)


-- pop most recently added item from Stack
stackPop :: Stack a -> (Stack a, a)
stackPop (Stack (values, size)) | size == 0 = error "stack is empty"
                                | otherwise = (Stack (init values, size - 1), out)
                                  where
                                     out = last values
                                     
