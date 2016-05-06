module Stack
    (Stack (..), push, top, pop, empty, isEmpty, size) where

newtype Stack a = Stk [a] deriving (Show, Eq)

push :: a -> Stack a -> Stack a
push a (Stk s) = Stk (a : s)

top :: Stack a -> a
top (Stk (a:as)) = a
top _ = error "Stack Error: Can't use top because Stack is empty."

pop :: Stack a -> (a, Stack a)
pop (Stk (a:as)) = (a, Stk as)
pop _ = error "Stack Error: Can't use pop because Stack is empty."

empty :: Stack a
empty = Stk []

isEmpty :: Stack a -> Bool
isEmpty (Stk []) = True
isEmpty _ = False

size :: Stack a -> Int
size (Stk s) = length s
