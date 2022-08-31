{-# Language TemplateHaskell #-}
module Main where

import System.Environment (getArgs)

import Dice
import Version


main :: IO ()
main = do
    args <- getArgs
    if args == ["-v"] || args == ["--version"]
        then putStrLn $$(versionExp)
        else putStrLn "" >> mapM_ execRoll args
