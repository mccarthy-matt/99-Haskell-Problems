-- PROBLEM 1
-- Find the last element of a list.
last' :: [a] -> a
last' [] = error "empty list"
last' [x] = x
last' (x:xs) = last xs

-- PROBLEM 2
-- Find the last but one element of a list.
lastButOne :: [a] -> a
lastButOne [] = error "empty list"
lastButOne [x] = error "list has only one element"
lastButOne [x,y] = x
lastButOne (x:xs) = lastButOne xs

-- PROBLEM 3
-- Find the Kth element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt [] n = error "index out of range"
elementAt (x:xs) n
  | n == 1 = x
  | otherwise = elementAt xs (n-1)

-- PROBLEM 4
--  Find the number of elements in a list.
length' :: [a] -> Int
length' [] = 0
length' [x] = 1
length' (x:xs) = 1 + length xs

-- PROBLEM 5
-- Reverse a list.
reverse' :: [a] -> [a]
reverse' [] = []
reverse' [x] = [x]
reverse' (x:xs) = reverse xs ++ [x]

-- PROBLEM 6
-- Find out weather a list is a palindrome.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = x == last xs && isPalindrome (init xs)

-- PROBLEM 8
-- Eliminate consecutive duplicates of list elements.
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs)
  | x == head xs = compress xs
  | otherwise = x:compress xs