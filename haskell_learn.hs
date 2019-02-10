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