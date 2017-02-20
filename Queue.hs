module Queue(makeQueue, empty, isEmpty, enqueue, dequeue, show) where

data Queue a = NuQu [a] [a]

makeQueue :: [a] -> Queue a
makeQueue l = NuQu l []


empty :: Queue a
empty = NuQu [] []


isEmpty :: Queue a -> Bool
isEmpty (NuQu [] []) = True
isEmpty (NuQu _ _) = False


enqueue :: a -> Queue a -> Queue a
enqueue x (NuQu ys zs) = NuQu ys (x:zs)


dequeue (NuQu (x:xs) ys) = (x, NuQu xs ys)
dequeue (NuQu [] ys) = (z, NuQu zs [])
    where z:zs = reverse ys


instance (Show a) => Show (Queue a) 
    where show (NuQu xs ys) = show "{[" ++ printElems (xs ++ reverse ys) ++ "]}"

printElems :: (Show a) => [a] -> String
printElems [] = ""
printElems [x] = show x
printElems (x:xs) = show x ++ "->" ++ printElems xs
