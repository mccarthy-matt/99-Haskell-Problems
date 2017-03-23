-- PROBLEM 1
-- Find the last element of a list.
-- Ex. last' [1,2,3] = 3
last' :: [a] -> a
last' [] = error "empty list"
last' [x] = x
last' (x:xs) = last xs

-- PROBLEM 2
-- Find the last but one element of a list.
-- Ex. lastButOne [1,2,3] = 2
lastButOne :: [a] -> a
lastButOne [] = error "empty list"
lastButOne [x] = error "list has only one element"
lastButOne [x,y] = x
lastButOne (x:xs) = lastButOne xs

-- PROBLEM 3
-- Find the Kth element of a list. The first element in the list is number 1.
-- Ex. [4,7,3] `elementAt` 2 = 7
elementAt :: [a] -> Int -> a
elementAt [] n = error "index out of range"
elementAt (x:xs) n
  | n == 1 = x
  | otherwise = xs `elementAt` (n-1)

-- PROBLEM 4
-- Find the number of elements in a list.
-- Ex. length' [1,2,3,4,5] = 5
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length xs

-- PROBLEM 5
-- Reverse a list.
-- Ex. reverse' [1,2,3] = [3,2,1]
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- PROBLEM 6
-- Find out whether a list is a palindrome. A list is a palindrome if it is
-- the same when reversed.
-- Ex. isPalindrome [1,2,3,2,1] = True
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = x == last xs && isPalindrome (init xs)

-- PROBLEM 7
-- Flatten a nested list structure.
-- Ex. flatten (List [Elem 1,List [Elem 2, Elem 3]]) = [1,2,3]
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- PROBLEM 8
-- Eliminate consecutive duplicates of list elements.
-- Ex. compress [1,2,2,3] = [1,2,3]
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs)
  | x == head xs = compress xs
  | otherwise = x:compress xs
  
compress' :: (Eq a) => [a] -> [a]
compress' [] = []
compress' [x] = [x]
compress' (x:xs) = compress'' x (compress' xs) where
                  compress'' x ys'@(y:ys)
                    | x == y = ys'
                    | otherwise = x:ys'

-- PROBLEM 9
-- Pack consecutive duplicates of list elements into sublists. If a list
-- contains repeated elements they should be placed in separate sublists.
-- Ex. pack [1,1,2,2,3,3,4] = [[1,1],[2,2],[3,3],[4]]
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs)
  | x `elem` (head (pack xs)) = (x:(head (pack xs))):(tail (pack xs))
  | otherwise = [x]:(pack xs)
  
pack' :: (Eq a) => [a] -> [[a]]
pack' [] = []
pack' [x] = [[x]]
pack' (x:xs) = pack'' x (pack' xs) where
              pack'' x ys'@(y:ys)
                | x `elem` y = (x:y):ys
                | otherwise = [x]:ys'
                
-- PROBLEM 10
-- Implement the run-length encoding data compression method. Consecutive
-- duplicates of elements are encoded as lists (N E) where N is the
-- number of duplicates of the element E.
-- Ex. encode [1,1,1,2,2,2,3] = [(3,1),(3,2),(1,3)]
encode :: (Eq a) => [a] -> [(Int,a)]
encode [] = []
encode [x] = [(1,x)]
encode (x:xs)
  | x == b    = (a+1,x):(tail (encode xs))
  | otherwise = (1,x):(encode xs)
  where (a,b) = head (encode xs)

encode' :: (Eq a) => [a] -> [(Int,a)]
encode' [] = []
encode' [x] = [(1,x)]
encode' (x:xs) = encode'' x (encode' xs) where
                 encode'' x ys'@(y:ys)
                  | x == b = (a+1,x):ys
                  | otherwise = (1,x):ys'
                  where (a,b) = y

-- PROBLEM 11
-- Modify PROBLEM 10 in such a way that if an element has no duplicates
-- it is simply copied into the result list. Only elements with duplicates
-- are transferred as (N E) lists.
-- Ex. encodeModified [1,1,2,3,3] =[Multiple 2 1, Single 2, Multiple 2,3]
data ListItem a = Single a | Multiple Int a
  deriving (Show)
  
toTuple :: ListItem a -> (Int,a)
toTuple (Single y) = (1,y)
toTuple (Multiple x y) = (x,y)

isSingle :: ListItem a -> Bool
isSingle x
  | amt == 1 = True
  | otherwise = False
  where (amt,_) = toTuple x

encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified [] = []
encodeModified [x] = [Single x]
encodeModified [x,y]
  | x == y    = [Multiple 2 y]
  | otherwise = [Single x, Single y]
encodeModified (x:xs)
  | x == headVal && headAmt == 1 = 
      (Multiple 2 x):(tail (encodeModified xs))
  | x == headVal && headAmt > 1 = 
      (Multiple (headAmt+1) x):(tail (encodeModified xs))
  | otherwise =
      (Single x):(encodeModified xs)
  where (headAmt, headVal) = toTuple (head (encodeModified xs))

encodeModified' :: (Eq a) => [a] -> [ListItem a]
encodeModified' [] = []
encodeModified' [x] = [Single x]
encodeModified' [x,y]
  | x == y = [Multiple 2 y]
  | otherwise = [Single x, Single y]
encodeModified' (x:xs) =
  encodeModified'' x (encodeModified' xs) where
  encodeModified'' x ys'@(y:ys)
    | a == 1 && x == b = (Multiple 2 x):ys
    | a > 1 && x == b  = (Multiple (a+1) x):ys
    | otherwise = (Single x):ys'
    where (a,b) = toTuple y

encodeModified''' :: (Eq a) => [a] -> [ListItem a]
encodeModified''' [] = []
encodeModified''' (x:xs)
  | count == 1 = (Single x):(encodeModified''' xs)
  | otherwise = (Multiple count x):(encodeModified''' rest)
  where (matched, rest) = span (==x) xs
        count = 1 + (length matched)

encodeModified'''' :: (Eq a) => [a] -> [ListItem a]
encodeModified'''' [] = []
encodeModified''''(x:xs) =
  encodeModified''''' x (encodeModified'''' xs) where
    encodeModified''''' x ys'@(y:ys)
      | count == 1 = (Single x):ys'
      | otherwise = (Multiple count x):ys
      where (matched, rest) = span (==x) xs
            count = 1 + (length matched)

-- PROBLEM 12
-- Decode a run-length encoded list.
-- Ex. decodeModified [Multiple 3 2, Single 1, Multiple 2 4] =[2,2,2,1,4,4]
decodeModified :: [ListItem a] -> [a]
decodeModified [] = []
decodeModified [Single y] = [y]
decodeModified [Multiple 2 y] = [y,y]
decodeModified (x:xs)
  | amt == 1 = val:(decodeModified xs)
  | otherwise =
      (decodeModified [Multiple (amt-1) val]) ++ (val:(decodeModified xs))
  where (amt,val) = toTuple x

decodeModified' :: [ListItem a] -> [a]
decodeModified' [] = []
decodeModified' [Single y] = [y]
decodeModified' [Multiple 2 y] = [y,y]
decodeModified' (x:xs) =
  decodeModified'' x (decodeModified' xs) where
  decodeModified'' x dmxs
    | a == 1 = b:dmxs
    | otherwise = (decodeModified' [Multiple (a-1) b]) ++ (b:dmxs)
    where (a,b) = toTuple x

-- PROBLEM 14
-- Duplicate elements of a list.
-- Ex. dupli [1,2,3] = [1,1,2,2,3,3]
dupli :: [a] -> [a]
dupli [] = []
dupli [x] = [x,x]
dupli (x:xs) = x:x:(dupli xs)

dupli' :: [a] -> [a]
dupli' [] = []
dupli' [x] = [x,x]
dupli' (x:xs) = 
  dupli'' x (dupli' xs) where
  dupli'' x dxs = x:x:dxs

-- PROBLEM 15
-- Replicate the elements of a list a given number of times.
-- Ex. repli [1,2,3] 3 = [1,1,1,2,2,2,3,3,3]
repli :: [a] -> Int -> [a]
repli [] n = []
repli (x:xs) n
  | n == 1 = x:(repli xs n)
  | otherwise = (repli [x] (n-1)) ++ (x:(repli xs n))
  
-- Quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  (quicksort lessOrEqual) ++ [x] ++ (quicksort greater) where
  lessOrEqual = [ y | y <- xs, y <= x ]
  greater     = [ y | y <- xs, y > x ]