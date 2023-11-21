
myReverse [] = []
myReverse xs = last xs : myReverse (init xs)


-- tco optimized 
{- what is going wront here?

myReverseTCO [] c = c:[] 
myReverseTCO (x:xs) c = myReverseTCO (init x:xs) (c : (last xs))

myReverse' xs = myReverseTCO xs 0
-}

