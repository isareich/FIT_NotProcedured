module Main where
import Data.List (sort)

fibonacci :: Int -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

min' :: Ord a => [a] -> a
min' [] = error "Empty list"
min' [x] = x
min' (x:xs) = min x (min' xs)

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) = x == y || elem' x ys

sortList :: (Ord a) => [a] -> [a]
sortList = sort

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

printNaturalNumbers :: Double -> IO ()
printNaturalNumbers n
  | n < 1     = putStrLn "N should be a positive integer."
  | otherwise = mapM_ (print) [1..n]

printNaturalNumbersInRange :: Double -> Double -> IO ()
printNaturalNumbersInRange a b
  | a > b     = putStrLn "A should be less than or equal to B."
  | a < 1     = putStrLn "A should be a positive integer."
  | b < 1     = putStrLn "B should be a positive integer."
  | otherwise = mapM_ (print) [truncate a..truncate b]
 

average :: Fractional a => [a] -> a
average [] = 0  
average xs = sum xs / fromIntegral (length xs)

main :: IO ()
main = do
  let n = 15
      nPlus5 = n + 5
      nPlus12 = n + 12

  let fibN = fibonacci n
      fibNPlus5 = fibonacci nPlus5
      fibNPlus12 = fibonacci nPlus12

  putStrLn $ "15th Fibonachi (F(" ++ show n ++ ")) = " ++ show fibN
  putStrLn $ "20th Fibonachi (F(" ++ show nPlus5 ++ ")) = " ++ show fibNPlus5
  putStrLn $ "27th Fibonachi (F(" ++ show nPlus12 ++ ")) = " ++ show fibNPlus12

  let list1 = [3, 1, 4, 1, 5, 9, 2, 6, 5]
  putStrLn $ "Minimum element of list1: " ++ show (min' list1)

  let list2 = [3, 1, 4, 1, 5, 9, 2, 6, 5]
  putStrLn $ "Is 4 in list1? " ++ show (elem' 4 list1)

  let unsortedList = [3, 1, 4, 1, 5, 9, 2, 6, 5]
  putStrLn $ "Unsorted list: " ++ show unsortedList
  let sortedList = sortList unsortedList
  putStrLn $ "Sorted list: " ++ show sortedList

  let n2 = 15
  putStrLn $ "Factorial of " ++ show n ++ " is " ++ show (factorial n2)

  let n3 = 10 
      p = 1.4
      k = -5

  putStrLn $ "Printing natural numbers from 1 to " ++ show n3
  printNaturalNumbers n3
  putStrLn $ "Printing natural numbers from 1 to " ++ show k
  printNaturalNumbers k
  putStrLn $ "Printing natural numbers from 1 to " ++ show p
  printNaturalNumbers p

  let a = 5
      b = 12
      a1 = 7
      b1 = -5
      a2 = 1.4
      b2 = 5.0
  putStrLn $ "Printing natural numbers from " ++ show a ++ " to " ++ show b
  printNaturalNumbersInRange a b
  putStrLn $ "Printing natural numbers from " ++ show a1 ++ " to " ++ show b1
  printNaturalNumbersInRange a1 b1
  putStrLn $ "Printing natural numbers from " ++ show a2 ++ " to " ++ show b2
  printNaturalNumbersInRange a2 b2

  let numbers = [1.0, 2.0, 3.0, 4.0, 5.0]

  putStrLn $ "List of numbers: " ++ show numbers
  putStrLn $ "Arithmetic mean: " ++ show (average numbers)

  
  
