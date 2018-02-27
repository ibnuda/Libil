{-# LANGUAGE OverloadedStrings #-}
module Lib where

import           Prelude                    hiding (take, takeWhile)

import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromJust)
import qualified Data.Text                  as Text

-- | First part of javanese script.
-- https://en.wikipedia.org/wiki/Hanacaraka
consonants1 :: [Text.Text]
consonants1 = [ "h", "n", "c", "r", "k"
              , "d", "t", "s", "w", "l"
              ]

-- | Second part of the consonants.
consonants2 :: [Text.Text]
consonants2 = [ "p", "dh", "j", "y", "ny"
              , "m", "g", "b", "th", "ng"
              ]

-- | Pairs of consonants for the rules.
pairsOfConsonants :: Map.Map Text.Text Text.Text
pairsOfConsonants =
  Map.fromList $ zip consonants1 consonants2 ++ zip consonants2 consonants1

-- | Vowels "a", "i", "u", "e", and "o".
vowels :: [Char]
vowels = ['a', 'o', 'e', 'i', 'u']

-- | When a word starts with a vowel, we should append a 'h' letter in front of it.
normalizeWords a =
  case Text.unpack a of
    x:xs
      | inClass vowels x -> Text.pack $ "h" ++ (x : xs)
    otherwise -> a

-- | Splits the words to end with vowels or to the end of the input.
parseSyllable :: Parser Text.Text
parseSyllable = do
  consonants <- takeWhile (not . inClass vowels)
  vowels <- takeWhile (inClass vowels)
  return $ Text.append consonants vowels

parseWord :: Parser Text.Text
parseWord = do
  parsed <- manyTill parseSyllable endOfInput
  return $ Text.intercalate " " parsed

someFunc :: IO ()
someFunc = putStrLn "someFunc"
