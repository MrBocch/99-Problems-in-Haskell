
-- lame version
myLength xs = length xs 

myLength' []     = 0
myLength' (_:xs) = 1 + myLength xs 

-- TCO cool kidz version
myLengthTCO [] collector     = collector 
myLengthTCO (_:xs) collector = myLengthTCO xs (collector +1) 
-- wrapper function 
myLength'' xs = myLengthTCO xs 0 

-- this one is so clever
myLength''' :: [a] -> Int 
myLength''' = sum . map (\_ -> 1)