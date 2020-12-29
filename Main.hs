{-# Language RecursiveDo #-}
module Main where

import Data.Char (isDigit)
import System.Environment (getArgs)

import Data.List

import Text.Earley
import Control.Applicative
import Control.Applicative.Combinators
import Control.Monad

import System.Random

import Data.Validation

data DiceRoll = Constant Integer
              | Roll Integer Integer
              | Sum DiceRoll DiceRoll
              | Diff DiceRoll DiceRoll
              | Product DiceRoll DiceRoll
    deriving (Show, Eq, Ord)


diceG :: Grammar r (Prod r () Char DiceRoll)
diceG = mdo
    sums    <- rule $ flip ($) <$> sums <*> ops <*> product <|> product
    product <- rule $ Product <$> product <* mult <*> term <|> term
    term    <- rule $ constant <|> dieRoll
    let ops      = Sum <$ token '+' <|> Diff <$ token '-'
        mult     = token 'x' <|> token '*'
        constant = Constant <$> number
        dieRoll  = Roll <$> option 1 number <* token 'd' <*> number
        number   = read <$> some (satisfy isDigit)
    return sums


parseDescriptor :: String -> Validation String DiceRoll
parseDescriptor s = case fullParses (parser diceG) s of
    ([], rep) -> Failure $ "Invalid input: " ++ msg rep
    ([x], _)  -> Success x
    (xs, _)   -> error $ "Ambiguous parse, this is an internal error: " ++
                 show s ++ " became " ++ show xs
  where
    msg (Report _ _ u) = if null u then "input was incomplete."
                         else "error parsing at " ++ u


check :: RandomGen g => DiceRoll
      -> Validation [String] (g -> ([String], String, Integer, g))
check d = case d of
    Constant x -> Success $ \g -> ([], show x, x, g)
    Sum d1 d2 -> liftA2 (pair "+" (+)) (check d1) (check d2)
    Diff d1 d2 -> liftA2 (pair "-" (-)) (check d1) (check d2)
    Product d1 d2 -> liftA2 (pair "x" (*)) (check d1) (check d2)
    Roll num faces -> liftA2 roll (val num "count") (val faces "faces")
  where
    pair sym op f1 f2 g0 = let (h1, e1, v1, g1) = f1 g0
                               (h2, e2, v2, g2) = f2 g1
                           in (h1 ++ h2, e1 ++ sym ++ e2, op v1 v2, g2)
    val x str | x < 1 = Failure [ show x ++ " is invalid for the " ++ str ++
                                  " of rolled dice" ]
              | otherwise = pure x
    roll num faces g0 =
        let (dice, g1) = multidice num faces ([], g0)
            shown = map show dice
            msg = [ "Rolling " ++ show num ++ "d" ++ show faces ++ ":  " ++
                    intercalate ", " shown ]
            expr | num > 1   = "(" ++ intercalate "+" shown ++ ")"
                 | otherwise = concat shown
        in (msg, expr, sum dice, g1)
    multidice c f r0@(h, g0)
        | c == 0    = r0
        | otherwise = let (n, g1) = uniformR (1, f) g0
                          r1 = (n : h, g1)
                      in multidice (c - 1) f r1


execRoll :: String -> IO ()
execRoll s = do
    let parsed = parseDescriptor s
    case parsed of
        Failure err -> do
            putStrLn $ "unable to parse " ++ s ++ " -- " ++ err
            putStrLn ""
        Success expr -> case check expr of
            Failure errs -> putStrLn $
                            "unable to run " ++ s ++ ", " ++ show errs
            Success doRoll -> do
                g <- newStdGen
                let (hist, filled, res, _) = doRoll g
                putStrLn $ "  * Calculating " ++ s
                when (length hist > 1) $ mapM_ putStrLn hist
                putStrLn $ filled ++ " = " ++ show res


main :: IO ()
main = do
    args <- getArgs
    putStrLn ""
    mapM_ ((>> putStrLn "") . execRoll) args
