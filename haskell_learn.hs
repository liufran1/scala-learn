-- first single input function
doubleMe x = x + x

-- double input function
doubleUs x y = x*2+y*2

-- if statements
doubleSmallNumber x = if x>100 then x else x*2

-- list comprehensions
boomBang xs = [if x `mod` 3==0 then "Fizz" else "Buzz"|x<-xs,odd x]
--fizzBuzz xs = [if x `mod` 3==0 then "Fizz" else x|x<-xs]

-- list comprehension. for every element in list xs, it maps to 1, so length takes the sum of all the 1's
length xs = sum[1|_<-xs]

-- list comprehension using range of chars
removeNonUppercase st =[c|c<-st,elem c ['A'..'Z']]

-- example of defining the expected types of input and output
-- pythagoreanTriples upper=[(m^2-n^2,2*m*n,m^2+n^2)|m<-[1..upper],n<-[1..upper],(mod m n)/=0,(mod m-n 2)==1]
pythagoreanTriples :: Int->[(Int,Int,Int)]
pythagoreanTriples upper =[(a,b,c)|c<-[1..upper],b<-[1..c],a<-[1..b],a^2+b^2==c^2]

-- original factorial function
factorial1 :: Integer->Integer
factorial1 n = product [1..n]

-- pattern matching factorial function
factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1) 

-- pattern matching example
tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y 

-- length function using pattern matching and recursion
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

-- patterns implementation
sum' :: (Num a) => [a] -> a
sum' [] = 0 
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty String"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

--guards: like an if/else tree
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          (skinny, normal, fat) = (18.5, 25.0, 30.0) 

--guards with pattern matching
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

--let bindings
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea
    
calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

--case expressions
head' :: [a] -> a
head' xs = case xs of [] -> error "No head for empty lists"
                      (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty"
                                               [x] -> "a singleton list"
                                               xs -> "a longer list"

--
--recursion implementation of max element of a list
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs
--
--recursion implementation of maximum using max function
maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

--replicate using recursion, input Int n and an element a and get a list of n a's
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    =[]
    | otherwise = x:replicate' (n-1) x
    
--take using recursion, takes first n elements of a list
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

--reverse a list using recursion
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

--zip two lists together using recursion
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

--find element in list using recursion
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = a `elem'` xs

--quicksort implementation
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

--curried functions
--step 1: function taking 3 inputs and multiplying them together
multTriple :: (Num a) => a -> a-> a-> a
multTriple x y z = x*y*z

--step 2: function that takes 2 inputs and passes them to the above function which has the last input hardcoded
let multByNine = multTriple 9

--curried function example passing input to the compare function with second variable of 100 hardcoded
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x
--can refactor as
--compareWithHundred = compare 100   
--since the compare function already returns a function that takes 2 inputs and compares them
--and compareWithHundred is returning a function

--curried infix function examples
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])


applyTwice :: (a -> a) -> a -> a  -- takes two inputs: a function that takes (a -> a), another input of type a, and outputs type a
applyTwice f x = f (f x) -- applyTwice applies the input function twice

applyTwice (+3) 10 
-- 16 -- adds 3 to 10 twice


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  -- takes two lists [a] and [b] and applies f(a,b)->c to them
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys  

zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]  
-- ["foo fighters","bar hoppers","baz aldrin"]  
-- Useful to use higher order functions on functions
-- One example is implementing 'map'

map :: (a -> b) -> [a] -> [b]  
map _ [] = []  
map f (x:xs) = f x : map f xs  

-- Or 'filter'
filter :: (a -> Bool) -> [a] -> [a]  
filter _ [] = []  
filter p (x:xs)   
    | p x       = x : filter p xs  
    | otherwise = filter p xs  

-- can use map and filter with lazy evaluation
-- find the largest number under 100,000 that's divisible by 3829
largestDivisible :: (Integral a) => a  
largestDivisible = head (filter p [100000,99999..])  -- lazy evaluation means the list doesn't have to be finite
    where p x = x `mod` 3829 == 0  

-- Or count number of collatz chains longer than a certain length
chain :: (Integral a) => a -> [a]  -- Construct the Collatz chain as a list
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)  

numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  -- return number of chains longer than 15
    where isLong xs = length xs > 15  


-- instead of defining isLong, can use an annonymous function. a lambda
-- \xs -> length xs > 15
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))  


-- left and right fold:
sum' :: (Num a) => [a] -> a  
sum' xs = foldl (\acc x -> acc + x) 0 xs  



-- function composition syntax
-- instead of 
map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24] 

-- use '.' syntax
map (negate . abs) [5,-3,-6,7,-3,2,-19,24]

-- can use this syntax to express functions as 'point free,' removing extraneous variable invocations
fn x = ceiling (negate (tan (cos (max 50 x))))  
-- can get rid of the x, converts to
fn = ceiling . negate . tan . cos . max 50  

-- need the function composition notation. can't just remove x, otherwise could be ambiguous
-- for example, the x could have been part of cos instead of max
-- fn x = ceiling (negate (tan (cos (max 50 ) x)))  
-- function composition notation makes clear how the functions are applied

-- Module Imports
import Data.List

-- Can resolve name clashes with qualified imports
import qualified Data.Map as M 
-- M.filter --this calls the 'filter' function from Data.Map instead of the default one in the namespace

-- some modules - http://learnyouahaskell.com/modules


-- Creating data types
-- can create them using the data keyword
data Bool = False | True  
-- 'False' and 'True' are the different values the Bool datatype could possibly take
-- add more possible values with additional 'or':| operators

data Shape = Circle Float Float Float | Rectangle Float Float Float Float   
-- a 'Shape' data type can be either a 'Circle', which takes 3 float constructor values (center (x,y) and radius)
-- or a 'Rectangle', which takes 4 float constructor values upper left (x,y) and bottom right (x,y)


-- Creating a function taking a Shape as an input
surface :: Shape -> Float  
surface (Circle _ _ r) = pi * r ^ 2  
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)  
-- pattern match against the two different possible data values

-- can use 'deriving' to add the datatype to a typeclass
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)  
-- by adding to the Show typeclass, 'deriving (Show)', calling 'print' on Shapes works

-- constructors are functions
map (Circle 10 20) [4,5,6,6]
-- this creates four circles centered at (10, 20) with the four radii

-- Can create an intermediate data type to describe a cartesian point
data Point = Point Float Float deriving (Show)  

-- We then update 'Shape'
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)  


-- Can use record syntax to name the field types
data Car = Car {company :: String, 
                model :: String, 
                year :: Int} deriving (Show)  


-- functions are automatically created for each of these fields
-- can instantiate a data type that has record syntax like this
let myCar = Car {company="Ford", model="Mustang", year=1967}  -- and don't need to write them in the order they were defined


-- can create type constructors that take types as inputs
data Vector a = Vector a a a deriving (Show)   


-- Can define types as synonyms of each other
type String = [Char]  -- a string is the same thing as a list of chars, and behaves the same


-- Can make recursive data structures, where the constructor contains the data type
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)  
-- build up lists by recursively adding heads to the empty list, where 'Cons'==':'


-- functor typeclass: for things that can be mapped over
class Functor f where  
    fmap :: (a -> b) -> f a -> f b 



-- Input-output
-- main = do syntax
main = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  -- IO string getLine. <- used for assignment, instead of = which is definition
    putStrLn ("Hey " ++ name ++ ", you rock!")  

-- in general, Exceptions are IO actions because they print an error to stdout
-- should avoid IO in functions to keep them pure
-- Errors should mostly be handled using the strict type checking in Haskell





