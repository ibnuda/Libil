{-# LANGUAGE OverloadedStrings #-}
module Lib where

import           Data.Attoparsec.Text
import qualified Data.Text            as T

-- | First part of javanese script.
-- https://en.wikipedia.org/wiki/Hanacaraka
consonants1 :: [T.Text]
consonants1 = [ "h", "n", "c", "r", "k"
              , "d", "t", "s", "w", "l"
              ]

-- | Second part of the consonants.
consonants2 :: [T.Text]
consonants2 = [ "p", "dh", "j", "y", "ny"
              , "m", "g", "b", "th", "ng"
              ]

-- | Vocals "a", "i", "u", "e", and "o".
vowels :: [Char]
vowels = ['a', 'o', 'e', 'i', 'u']

parserFirstChar :: Parser [Char]
parserFirstChar = do
  [v] <- count 1 digit
  return $
    if inClass vowels v
      then "h" ++ [v]
      else [v]

normalizeWord :: Parser [Char]
normalizeWord = do
  firstcharacter <- parserFirstChar
  return firstcharacter

someFunc :: IO ()
someFunc = putStrLn "someFunc"
