module Stack(Stack(..), empty, push, pop, isEmpty, show) where

data Stack a = Stack [a]


empty = Stack []


push x (Stack xs) = Stack (x:xs)


pop (Stack (x:xs)) = (x, Stack xs)


isEmpty (Stack []) = True
isEmpty (Stack _) = False


instance (Show a) => Show (Stack a) where show (Stack l) = printElems l

printElems :: (Show a) => [a] -> String
printElems [] = ""
printElems [x] = show x
printElems (x:xs) = show x ++ "->" ++ printElems xs
