import Stack

apilar :: [a] -> Stack a
apilar [] = emptyS
apilar (x:xs) = push x (apilar xs)

desapilar :: Stack a -> [a]
desapilar st =  if isEmptyS st
                then []
                else push (top st) : desapilar (pop st)

insertarEnPos :: Int -> a -> Stack a -> Stack a
insertarEnPos n x st = push x (desapilarHasta n st)

desapilarHasta :: n -> Stack a -> Stack a 
desapilarHasta 0 st = st 
desapilarHasta n st = desapilar (desapilarHasta (n-1) st)