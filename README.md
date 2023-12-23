# 99 Problems in Haskell

https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

I want to practice what im learning in haskell
by working my way thrue these problems

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

elementAt "haskell" 5
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

## 6. Find out whether a list is a palindrome 
```
isPalindrome [1,2,3]
> False

isPalindrome "madamimadam"
> True

isPalindrome [1,2,4,8,16,8,4,2,1]
> True
```
