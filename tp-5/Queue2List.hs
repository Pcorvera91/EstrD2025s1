module Queue2List
    (Queue , emptyQ , isEmptyQ , enqueue , firstQ ,dequeue)
    where

data Queue a = Q [a] [a]

emptyQ :: Queue a
isEmptyQ :: Queue a -> Bool
enqueue :: a -> Queue a -> Queue a
firstQ :: Queue a -> a
-- hay al menos 1 elemento en la cola
dequeue :: Queue a -> Queue a
--hay al menos 1 elemento en la cola


emptyQ = Q [] []

isEmptyQ (Q [] []) = True
isEmptyQ (Q _ _) = False

enqueue x (Q fs bs) = Q fs (x:bs)


dequeue (Q fs bs) = Q (sinLosPrimeros 1 fs) bs

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros n [] = []
sinLosPrimeros n (x:xs) =
  if n > 0
    then sinLosPrimeros (n - 1) xs
    else x : xs

firstQ (Q xs (y:ys)) = y