{-# Language RecursiveDo #-}
module Main where

import Data.Char (isDigit)
import System.Environment (getArgs)

import Data.List

import Text.Earley
import Control.Applicative
import Control.Applicative.Combinators

import System.Random

data DiceRoll = Constant Integer
              | Roll Integer Integer
              | Sum DiceRoll DiceRoll
              | Diff DiceRoll DiceRoll
              | Product DiceRoll DiceRoll
    deriving (Show, Eq, Ord)


diceDescriptorG :: Grammar r (Prod r () Char DiceRoll)
diceDescriptorG = mdo
    sums    <- rule $ flip ($) <$> sums <*> ops <*> product <|> product
    product <- rule $ Product <$> product <* mult <*> term <|> term
    term    <- rule $ constant <|> dieRoll
    let ops      = Sum <$ token '+' <|> Diff <$ token '-'
        mult     = token 'x' <|> token '*'
        constant = Constant <$> positive
        dieRoll  = Roll <$> option 1 positive <* token 'd' <*> positive
        positive = read <$> many (token '0') <> posDigit <> digits
        posDigit = pure <$> satisfy (\c -> isDigit c && c /= '0')
        digits   = many (satisfy isDigit)
    return sums


parseDescriptor :: String -> Either String DiceRoll
parseDescriptor input = case fullParses (parser diceDescriptorG) input of
    ([], rep) -> Left $ "Invalid input: " ++ msg rep
    ([x], _)  -> Right x
    (xs, _)   -> error $ "Ambiguous parse, this is an internal error: " ++
                 show input ++ " became " ++ show xs
  where
    msg (Report _ _ u) = if null u then "input was incomplete."
                         else "didn't understand input somewhere around " ++ u


run :: RandomGen g => DiceRoll -> g -> (String, Integer, g)
run d g0 = case d of
    Constant x -> (show x, x, g0)
    Sum d1 d2 -> pair "+" (+) d1 d2
    Diff d1 d2 -> pair "-" (-) d1 d2
    Product d1 d2 -> pair "x" (*) d1 d2
    Roll num faces -> roll num faces
  where
    pair sym op d0 d1 = let (e1, v1, g1) = run d0 g0
                            (e2, v2, g2) = run d1 g1
                        in (e1 ++ sym ++ e2, op v1 v2, g2)
    roll num faces = (expr, sum dice, g1)
      where
        (dice, g1) = multidice num faces ([], g0)
        shown = map show dice
        expr | num > 1   = "(" ++ intercalate "+" shown ++ ")"
             | otherwise = concat shown
    multidice c f r0@(h, g)
        | c == 0    = r0
        | otherwise = let (n, g') = uniformR (1, f) g
                          r1      = (n : h, g')
                      in multidice (c - 1) f r1


execRoll :: String -> IO ()
execRoll s = do
    let parsed = parseDescriptor s
    case parsed of
        Left err -> do
            putStrLn $ "unable to parse " ++ s ++ " -- " ++ err
            putStrLn ""
        Right expr -> do
            g <- newStdGen
            let (filled, res, _) = run expr g
            putStrLn $ "  * Rolling " ++ s
            putStrLn $ if all isDigit filled then filled
                       else filled ++ " = " ++ show res
            putStrLn ""


main :: IO ()
main = do
    args <- getArgs
    putStrLn ""
    mapM_ execRoll args
