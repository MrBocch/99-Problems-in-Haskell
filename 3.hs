-- Find the K'th element in the list

elementAtc xs k passes = if k == passes
    then head xs 
    else elementAtc (tail xs) k (passes +1)

-- wrapper function, dont have to call function
-- passing in passes, does it for you 
elementAt xs k = elementAtc xs k 1

elementAt' xs k = xs !! (k-1)

elementAt'' xs k 
    | (length xs) <  k = error "trying to reach out of list"
    | k           <  1 = error "cant do that" 
    | (length xs) == k = last xs 
    | otherwise        = xs !! (k-1) 

elementAt''' xs k = last (take k xs)