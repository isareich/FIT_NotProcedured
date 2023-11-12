module Main where
import Data.List (foldr)

average :: [Double] -> Double
average xs = sum' / count
  where (sum', count) = foldr (\x (s, c) -> (s + x, c + 1)) (0, 0) xs

scalarProduct :: Num a => [a] -> [a] -> a
scalarProduct xs ys = foldr (+) 0 (zipWith (*) xs ys)

countEven :: [Int] -> Int
countEven xs = length (filter even xs)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x]
                ++ [x] ++
                quicksort [y | y <- xs, y > x]

main :: IO ()
main = do
  let myList1 = [1.0, 2.0, 3.0, 4.0, 5.0]
  let myList2 = [1.4, 2.7, 3.3, 4.9, 5.1]
  let myList3 = [1, 2, 3, 4, 5, 6, 7, 8, 9]
  let myList4 = [1, 2, 3, 4, 5, 6, 4, 3, 2]
  
  let myList5 = [1, 2, 3, 4, 5, 6, 7, 8, 9]

  let avg1 = average myList1
  let avg2 = average myList2
  putStrLn $ "Середнє арифметичне 1: " ++ show avg1
  putStrLn $ "Середнє арифметичне 2: " ++ show avg2

  let result = scalarProduct myList3 myList4
  print $ "Scalar Product: " ++ show result

  let count = countEven myList5
  print $ "Elements: " ++ show count

  let myList = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]

  let sortedList = quicksort myList
  putStrLn $ "Sorted list: " ++ show sortedList
