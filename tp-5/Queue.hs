module Queue
    (Queue , emptyQ , isEmptyQ , enqueue , firstQ ,dequeue)
    where

data Queue a = Q [a]

emptyQ :: Queue a
isEmptyQ :: Queue a -> Bool
enqueue :: a -> Queue a -> Queue a
firstQ :: Queue a -> a
dequeue :: Queue a -> Queue a


emptyQ = Q []

isEmptyQ (Q []) = True


