-- find the last element of a list 

-- with guards 
myLast xs 
  | length xs == 1 = head xs 
  | otherwise      = myLast (tail xs)

-- with pattern matching 
myLast2 [x] = x 
myLast2 xs = myLast2 (tail xs)

-- with if else 
myLast3 xs = if (length xs) == 1
    then head xs 
    else myLast3 (tail xs)

-- i dont know how to write a test
-- that is not so ugly
tester func input answer = 
    output == answer 
    where output = func input 

-- how do i call all these functions, not manually
test1 = tester myLast [1,2,3,4] 4 
test2 = tester myLast ['x', 'y','z'] 'z'

test3 = tester myLast2 [1,2,3,4] 4 
test4 = tester myLast2 ['x', 'y','z'] 'z'

test5 = tester myLast3 [1,2,3,4] 4 
test6 = tester myLast3 ['x', 'y','z'] 'z'

-- why did i not think of this 
myLast4 [] = error "No end for empty list"
myLast4 xs = head (reverse xs)

-- index based
myLast5 [] = error "No end for empty list"
myLast5 xs = xs !! (length xs -1)