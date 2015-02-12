import Data.Either
import Data.Char
import Test.QuickCheck

--14.4 define function

data Expr = Lit Integer | Add Expr Expr | Sub Expr Expr
						| Mul Expr Expr | Div Expr Expr

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)
eval (Div e1 e2) = (eval e1) `div` (eval e2)

msg' :: Expr -> String
msg' (Lit n) = msg n
msg' (Add e1 e2) = "(" ++ msg' e1 ++ "+" ++ msg' e2 ++ ")"
msg' (Sub e1 e2) = "(" ++ msg' e1 ++ "-" ++ msg' e2 ++ ")"
msg' (Mul e1 e2) = "(" ++ msg' e1 ++ "*" ++ msg' e2 ++ ")"
msg' (Div e1 e2) = "(" ++ msg' e1 ++ "/" ++ msg' e2 ++ ")"

size :: Expr -> Integer
size (Lit n) = 0
size (Add e1 e2) = 1 + (size e1) + (size e2)
size (Sub e1 e2) = 1 + (size e1) + (size e2)
size (Mul e1 e2) = 1 + (size e1) + (size e2)
size (Div e1 e2) = 1 + (size e1) + (size e2)

-- 14.5
-- 14.6
-- 14.8
-- These 3 are still giving me trouble, asked a couple classmates and 
-- and they were stuck on them as well. will do best to find a way to 
-- complete 

--14.10 is function Ntree

data NTree = NilT | Node Integer NTree Ntree

sumTree,depth :: NTree -> Integer
sumTree NilT = 0
sumTree (Node n t1 t2) = n + sumTree t1 sumTree t2

depth NilT = 0
depth (Node n t1 t2) = n + sumree t1 sumTree t2

isof :: Ntree -> Integer -> Integer
isof NilT _ = 0
isoff (Node n t1 t2) p
		| n==p			= 1 + isof t1 p + isoff t2 p
		| otherwise = isof t1 p +isof t2 p

element :: NTree -> Integer -> Bool
element Nilt _ = False
element (Node n t1 t2) p
		| n == p				= True
		| otherwise				= (element t1 p) || (element t2 p)

-- 14.13 define collapse sort function

collapse,sort' :: NTree -> [Integer]
collapse NilT = []
collapse (Node n t1 t2) = [n] ++ (collapse t1) ++ (collapse t2)

sort' NilT = []
sort' tree = sort $ collapse tree

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (x:xs) = (sort lesser) ++ [x] ++ (sort greater)
	where
		lesser = filter (< x) xs
		greater = filter (>= x) xs

-- 14.15
-- 14.19
-- 14.20
-- 14.23
-- 14.25
-- 14.26
-- 14.27
-- the above are even more i need to complete. 

-- 5 bin2int function

data Bit = Zero | One
	deriving (Show)
instance Eq Bit where
	Zero == Zero = True
	One == One = True
	Zero == One = False
	One == Zero = False

bin2int :: [bit] -> int
bin2int [] = 0
bin2int (Zero:xs) = 0 + bin2int xs
bin2int (one:xs) = (2 ^ length xs) + bin2int xs

-- 6 inverse to int2bin

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n
		| n `mod` 2 == 1 		= One : int2bin (n `div` 2)
		| n `mod` 2 == 0		= Zero : int2bin (n `div` 2)
int2bin' :: Int -> [Bit]
int2bin' b = reverse $ int2bin b

-- 7
-- 8
-- 9
-- 10
-- 11
-- 12
-- these are also in need of work. I have attempted to solve a few 
-- times but keeps prooving to be erroneous. 
