module Stack
    (Stack , emptyS , isEmptyS , push , top ,pop ,lenS)
    where
        
data Stack a = St [a]

emptyS :: Stack a
isEmptyS :: Stack a -> Bool
push :: a -> Stack a -> Stack a
top :: Stack a -> a
pop :: Stack a -> Stack a
lenS :: Stack a -> Int