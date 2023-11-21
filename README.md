# I got 99 problems, but a monad aint one

https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

I want to practice what im learning in haskell
by working my way thrue these problems

> These are Haskell translations of Ninety-Nine Lisp Problems, which are themselves translations of Ninety-Nine Prolog Problems. 

I will check those out one day

it appears the wiki page is not complete yet?

Sorted by, my solutions, then the solutions that 
are not mine but I thought were cool

## 1. Find the last element of a list 
```
myLast [1,2,3,4]
> 4

myLast ['x','y','z']
> 'z'
```

## 2. Find the last-but-one (or second-last) element of a list
```
myButLast [1,2,3,4]
> 3

myButLast ['a'..'z']
> 'y'
```

## 3. Find the K'th element of a list
```
elementAt [1,2,3] 2
> 2

elementAt "haskell"
> 'e'
```

## 4. Find the number of elements in a list 
```
myLength [123, 456, 789]
> 3

myLength "Hello, world!"
> 13
```

## 5. Reverse a list
```
myReverse "A man, a plan, a canal, panama!"
> "!amanap ,lanac a ,nalp a ,nam A"

myReverse [1,2,3,4]
> [4,3,2,1]
```