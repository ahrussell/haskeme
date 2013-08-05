module Main where
import System.Environment

-- basic hello world program, just copy pasta'd
main :: IO ()
main = do
    args <- getArgs
    putStrLn ("Hello, " ++ args !! 0)
