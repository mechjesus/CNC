> import Data.Char

8.10

-- Palindrome program
> getPal :: IO()
> getPal =	do 
>		putStrLn "Input string"
>		line <- getLine
>		putStrLn (reverse line)

8.11

-- Sumification
> addNum :: IO()
> addNum = do
>		putStrLn "Submit two integers"
>		line1 <- getLine
>		line2 <- getLine
>		putStr "Equals"
>		putStrLn show $ (read line1) + (read line2)

8.12

-- string n times multi lines
> putNtimes :: Integer -> String -> IO() 
> putNtimes n str =
>		if n <= 0
>		then return ()
>		else do 
>			putStrLn str
>			putNtimes (n-1) str


8.13

-- Adds n amount of ints together
> theSum :: IO()
> theSum = do
>		putStrLn "Submit one positive integer"
>		n <- getLine
>		if (read n) <= 0 
>		then return()
>		else theSum' 0 (read n)

> theSum' :: Integer -> Integer -> IO() 
> theSum' a n = do
>		if n <= 0 
>		then do
>			putStrLn (show a)
>		else do
>			num <- getLine
>			theSum' ((read num) + a) (n-1)

8.14

-- word count
> wc :: Integer -> Integer -> Int -> IO()
> wc l w c = do 
>		putStrLn ("Submit string") 
>		line <- getLine
>		if line == ""
>			then do
>				putStrLn ("Lines:" ++ (show l))
>				putStrLn ("words:" ++ (show w))		
>				putStrLn ("chars:" ++ (show c)) 
>		else do
>			putStrLn line
>			wc (l+1) ((wordCnt line)+w) (c + (length line))

> wordCnt :: String -> Integer
> wordCnt [] = 0
> wordCnt (x:xs)
>	|x == ' '	= 1+wordCnt xs  
>	|otherwise  = 0+wordCnt xs


8.15

-- sentence palindrome
> isPali :: IO()
> isPali = do
>		putStrLn ("Submit string")
>		line <- getLine
>		if (reverse $ toLower' line) == (toLower' line)
>			then putStrLn ("This is a palindrome")
>		else 
>			putStrLn ("This is not a palindrome")

> toLower' :: [Char] -> [Char]
> toLower' s = rmChar $ map toLower s

> chars :: [Char]
> chars = ['a'..'z'] 

> rmChar :: [Char] -> [Char]
> rmChar s = [x | x <- s, elem x chars] 

8.16

-- multi palindrome
> paliChecker :: IO()
> paliChecker = do
>		putStrLn ("input string")
>		line <- getLine
>		if line == ""
>			then do
>			putStrLn ("must input string")
>			return()
>		else do
>			if (reverse $ toLower' line) == (toLower' line)
>				then putStrLn ("this is a palindrome")
>			else
>				putStrLn ("this is not a palindrome")
>		paliChecker


8.17

-- multi string summer
> multiSum :: IO()
> multiSum = readSum 0

> readSum :: Integer -> IO()
> readSum x = do
>		putStrLn ("submit integer")
>		line <- getLine
>		if (read line) == 0 
>		then do
>			putStrLn (show x)
>		else
>			readSum (x+(read line))


8.18

-- still in progress



8.19

The program is taking in strings and returns them back to the user. This will continue until an empty line is submited. 
