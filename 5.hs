
myReverse [] = []
myReverse xs = last xs : myReverse (init xs)


-- tco optimized 
{- what is going wront here?

myReverseTCO [] c = c:[] 
myReverseTCO (x:xs) c = myReverseTCO (init x:xs) (c : (last xs))

myReverse' xs = myReverseTCO xs 0
-}

tcoRev [] c = c 
tcoRev xs c = tcoRev (init xs) (c ++ [last xs])

myReverse' xs = tcoRev xs []

{-
reverse in Prelude is defined like this 

reverse          :: [a] -> [a]
reverse          =  foldl (flip (:)) []

I dont understand that a foldl and flip means 
-}