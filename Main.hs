{-# Language RecursiveDo #-}
module Main where

import Data.Char (isDigit)
import System.Environment (getArgs)

import Text.Earley
import Control.Applicative
import Control.Applicative.Combinators

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
    val x str | x < 1    = Failure [show x ++
                                    " is invalid for the " ++
                                    str ++ " of rolled dice"]
              | otherwise = pure x
    roll num faces g0 = let (h, e, v, g1) = go num faces ("", "", 0, g0)
                            msg = ["Rolling " ++ show num ++ "d" ++
                                   show faces ++ ": " ++ h]
                            expr = if num > 1 then "(" ++ e ++ ")" else e
                        in (msg, expr, v, g1)
    go c f (h, e, v, g0)
        | c == 0    = (zipWith const h (drop 2 h), init e, v, g0)
        | otherwise = let (n, g1) = uniformR (1, f) g0
                          r = (show n ++ ", " ++ h, show n ++ "+" ++ e, n + v, g1)
                      in go (c - 1) f r

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
                putStrLn $ "Executing " ++ s
                mapM_ putStrLn hist
                putStrLn $ filled ++ " = " ++ show res
                putStrLn ""


main :: IO ()
main = do
    args <- getArgs
    mapM_ execRoll args
