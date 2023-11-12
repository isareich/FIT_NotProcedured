
module Main where
import System.Environment (getArgs)

main :: IO ()
main = do
    putStrLn "Write first number:"
    input1 <- getLine
    putStrLn "Write second number:"
    input2 <- getLine

    let number1 = read input1 :: Double
        number2 = read input2 :: Double
        sumResult = number1 + number2

    putStrLn ("Sum of two numbers: " ++ show sumResult)