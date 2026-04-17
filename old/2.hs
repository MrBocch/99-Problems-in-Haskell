-- Find the last-but-one (or second-last) element of a list

-- gaurds
myButLast xs  
    | (length xs) == 2 = head xs 
    | (length xs) >  2 = myButLast (tail xs)
    | otherwise        = error "List is less than 2"

myButLast' []  = error "List is empty"
myButLast' [x] = error "List only have one"
myButLast' [x, _] = x
myButLast' (_:xs) = myButLast xs

myButLast'' []  = error "List is empty"
myButLast'' [x] = error "List only have one"
myButLast'' xs = head (tail (reverse xs))

-- why did i not think of these 
myButLast''' xs = (reverse xs) !! 1

myButLast'''' xs = last (init xs)