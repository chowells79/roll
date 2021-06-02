{-# Language RecursiveDo #-}
module Main where

import Data.Char (isDigit)
import System.Environment (getArgs)

import Data.Function ((&))
import Data.List (intercalate)
import Control.Monad (replicateM)

import Text.Earley
import Control.Applicative
import Control.Applicative.Combinators

import System.Random.Stateful

data DiceRoll = Constant Int
              | Roll Int Int
              | Sum DiceRoll DiceRoll
              | Diff DiceRoll DiceRoll
              | Product DiceRoll DiceRoll
    deriving (Show, Eq, Ord)


diceDescriptorG :: Grammar r (Prod r () Char DiceRoll)
diceDescriptorG = mdo
    sums    <- rule $ (&) <$> sums <*> ops <*> product <|> product
    product <- rule $ Product <$> product <* mult <*> term <|> term
    term    <- rule $ constant <|> dieRoll
    let ops      = Sum <$ token '+' <|> Diff <$ token '-'
        mult     = token 'x' <|> token '*'
        constant = Constant <$> positive
        dieRoll  = Roll <$> option 1 positive <* token 'd' <*> positive
        positive = read <$> many (token '0') <> posDigit <> digits
        posDigit = (:[]) <$> satisfy (\c -> isDigit c && c /= '0')
        digits   = many (satisfy isDigit)
    return sums


parseDescriptor :: String -> Either String DiceRoll
parseDescriptor input = case fullParses (parser diceDescriptorG) input of
    ([], rep) -> Left $ "Invalid input: " ++ msg rep
    ([x], _)  -> Right x
    (xs, _)   -> error $ "Ambiguous parse, this is an internal error: " ++
                 show input ++ " became " ++ show xs
  where
    msg (Report _ _ u)
        | null u = "input was incomplete."
        | otherwise = "didn't understand input somewhere around " ++ u


run :: StatefulGen g m => DiceRoll -> g -> m (String, Int)
run d g = go d
  where
    go (Constant x)     = pure (show x, x)
    go (Sum d1 d2)      = pair "+" (+) d1 d2
    go (Diff d1 d2)     = pair "-" (-) d1 d2
    go (Product d1 d2)  = pair "x" (*) d1 d2
    go (Roll num faces) = roll num faces

    pair sym op d0 d1 = do
        (exp1, val1) <- go d0
        (exp2, val2) <- go d1
        pure (exp1 ++ sym ++ exp2, op val1 val2)

    roll num faces = do
        dice <- replicateM num (uniformRM (1, faces) g)
        let shown = map show dice
            expr | num > 1   = "(" ++ intercalate "+" shown ++ ")"
                 | otherwise = concat shown
        pure (expr, sum dice)


execRoll :: String -> IO ()
execRoll s = case parseDescriptor s of
    Left err -> do
        putStrLn $ "  * unable to parse " ++ s
        putStrLn err
        putStrLn ""
    Right roll -> do
        g <- newStdGen
        let (filled, res) = runStateGen_ g (run roll)
        putStrLn $ "  * Rolling " ++ s
        putStrLn $ if all isDigit filled then filled
                   else filled ++ " = " ++ show res
        putStrLn ""


main :: IO ()
main = do
    args <- getArgs
    putStrLn ""
    mapM_ execRoll args
