-- Find the last element of a list
p1 :: [a] -> a
p1 []     = error "What do you return on a empty list?"
p1 [x]    = x 
p1 (x:xs) = p1 xs 


-- Find the last-but-one (or second-last) element of a list
p2 :: [a] -> a
p2 []  = error "What do you return on a empty list?"
p2 [x] = error "What do you return on a list of size 1"
p2 [x,_] = x  
p2 (x:xs)   = p2 xs 
-- if you try p2 (x:_) it just returns the first item of the list. 
-- i thought it meant of size (x:y:[]) no.
-- I must have made this mistake before thinkking that
-- (x:xs) != [x,xs] 


-- Find the K'th element of a list.
p3 :: [a] -> Int -> a
p3 xs n = traverse xs 1
  where
    traverse :: [a] -> Int -> a
    traverse (x:xs) i
      | n < 0     = error "can't negative index"
      | null xs   = error "over index" 
      | n == i    = x  
      | otherwise = traverse xs (i+1)

-- naming a function null to check if a list is empty is weird.
-- Just didn't expect to see the n word....
-- null.


-- Find the number of elements in a list
p4 :: [a] -> Int
p4 = sum . map (const 1) 

-- First time encountering const function
-- only found it because lsp rewrote (\x -> 1) to const
-- how did it know?
-- const :: a -> b -> a 
-- const x _ = x 


-- Reverse a list
p5 :: [a] -> [a]
p5 []     = []
p5 (x:xs) = p5 xs ++ [x]

-- Remember really struggeling when I first
-- tried to reverse a list.
-- Try not to be so computational wasteful.
-- Point free version/TCO version
-- prelude = foldl (flip (:)) []
-- hard to visualize.

-- Find out whether a list is a palidrome
p6 :: Eq a => [a] -> Bool
p6 = starling (==) reverse 
  where
    starling f g x = f x (g x)

-- Interesting that the type checker couldn't
-- infer without the Eq type class
-- luckily the error message told me to try
-- adding Eq a
--
-- i think that `(==) <*> reverse` looks strange 
-- I prefer the pretty bird names!
-- 
-- combinator bird package.
-- https://hackage.haskell.org/package/data-aviary-0.4.0/docs/Data-Aviary-Birds.html
--
-- yeah! the pheonix combinator also works (liftM2)
-- liftM2 (==) id reverse
-- 
-- in uiua its
-- IsPalidrome ← ≍⊸⇌
-- I just love this page, i reference it all the time
-- https://www.uiua.org/docs/combinators


-- Flatten a nested list structure

data NestedList a = Elem a | List [NestedList a]

p7 :: NestedList a -> [a]
p7 (List [])        = []
p7 (List [Elem x])  = [x]  
p7 (List (x:xs)) =  error "getting skilled issue so hard"
p7 (Elem x)         = [x]


-- Eliminate consecutive duplicates of list elemnts
p8 :: Eq a => [a] -> [a]
p8 []     = []
p8 (x:xs) = x : removesDuplicates x xs 
  where
    removesDuplicates :: Eq a => a -> [a] -> [a]
    removesDuplicates _ [] = []
    removesDuplicates last (x:xs)
      | last == x =     removesDuplicates x xs
      | otherwise = x : removesDuplicates x (xs)


-- Have no idea how I would point free this.
-- compress = map head . group
-- what is group? 
-- my guess
-- "aaabbccceddf"
-- ["aaa", "bb", ....]
-- define your own group function. point free



-- Pack consecutive duplicates of list elements into sublists
p9 :: [Char] -> [String]
p9 [] = []
p9 (x:xs) = [x: takeWhile (==x) xs] ++ p9 (dropWhile (== x) xs)





-- Run-length encoding of a list:
p10 :: [Char] -> [(Int, Char)]
p10 = map (phoenix (,) length head) . p9 
  where 
    -- Data.Aviary.Birds
    phoenix :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
    phoenix f g h x = f (g x) (h x)

-- so cool that making tuples is a function (,) (,,) (,,,)
