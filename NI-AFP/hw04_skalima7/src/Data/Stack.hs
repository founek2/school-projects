module Data.Stack where

-- https://en.wikipedia.org/wiki/Stack_(abstract_data_type)
data Stack a = Empty | a :<| (Stack a)
             deriving (Show, Read, Eq)

infixr 5 :<|

-- The empty stack
empty :: Stack a
empty = Empty

-- A singleton stack
-- TODO: implement stack
singleton :: a -> Stack a
singleton a = a :<| Empty

-- Get element from top of stack
-- If stack is empty error with message "Empty stack" will be raised
-- TODO: implement top
top :: Stack a -> a
top Empty     = error "Empty stack"
top (a :<| _) = a

-- Get element from top of stack (if there is some, otherwise return Nothing)
-- TODO: implement safe top
topSafe :: Stack a -> Maybe a
topSafe Empty     = Nothing
topSafe (a :<| _) = Just a

-- Pop element from top of stack
-- If stack is empty error with message "Empty stack" will be raised
-- TODO: implement pop
pop :: Stack a -> Stack a
pop Empty     = error "Empty stack"
pop (_ :<| s) = s

-- Pop element from top of stack (if there is some, otherwise return Nothing)
-- TODO: implement safe pop
popSafe :: Stack a -> Maybe (Stack a)
popSafe Empty     = Nothing
popSafe (_ :<| s) = Just s

-- Push element to top of stack
-- TODO: implement push
push :: a -> Stack a -> Stack a
push a s = a :<| s 

-- Get number of elements in stack
-- TODO: implement size
size :: Num n => Stack a -> n
size Empty = 0
size (_ :<| s) = 1 + size s

-- Check if stack is empty
-- Note: is more effective than checking if size is zero
-- TODO: implement null (not by using size!)
null :: Stack a -> Bool
null Empty = True
null _     = False
