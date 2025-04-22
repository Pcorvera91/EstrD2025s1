module Stack
    (Stack , emptyS , isEmptyS , push , top ,pop ,lenS)
    where
        
data Stack a = St [a] Int

emptyS :: Stack a
isEmptyS :: Stack a -> Bool
push :: a -> Stack a -> Stack a
top :: Stack a -> a
pop :: Stack a -> Stack a
lenS :: Stack a -> Int

emptyS = St [] 0

isEmptyS (St [] _) = True
isEmptyS (St _ _) = False

push x (St xs n) = (St (x : xs) (n+1))

top (St (x:xs) n) = x 

pop (St (x:xs) n) = St xs (n-1)

lenS (St _ n) = n