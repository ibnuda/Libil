{-# LANGUAGE OverloadedStrings #-}
module Lib where

import           Data.Char
import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust)
import           Data.String

-- | First part of javanese script.
-- https://en.wikipedia.org/wiki/Hanacaraka
consonants1 :: [String]
consonants1 = ["h", "n", "c", "r", "k", "d", "t", "s", "w", "l"]

-- | Second part of the consonants.
consonants2 :: [String]
consonants2 = ["p", "dh", "j", "y", "ny", "m", "g", "b", "th", "ng"]

-- | Pairs of consonants for the rules.
pairsOfConsonants :: Map.Map String String
pairsOfConsonants =
  Map.fromList $ zip consonants1 consonants2 ++ zip consonants2 consonants1

-- | Vowels "a", "o", "e", "u", and "i".
vowels :: String
vowels = "aoeui"

-- | Appends a 'h' letter when a word doesn't start with a consonants.
-- Used in `ruleYoja`
normalize :: String -> String
normalize (x:xs)    | x `elem` vowels = 'h':x:xs
normalize otherwise = otherwise

-- | Rules of Jogjakarta's style of Walikan.
ruleYoja :: String -> String
ruleYoja (f:s:r) =
  case Map.lookup [toLower f, toLower s] pairsOfConsonants of
    Nothing ->
      case Map.lookup [toLower f] pairsOfConsonants of
        Nothing  -> f : s : ruleYoja r
        Just syl -> syl ++ ruleYoja (s : r)
    Just syl -> syl ++ ruleYoja r
ruleYoja (f:r) =
  case Map.lookup [toLower f] pairsOfConsonants of
    Nothing  -> f : r
    Just syl -> syl ++ r
ruleYoja [] = ""

-- | Splits words into its syllables.
toSyllables :: String -> [String]
toSyllables []        = []
toSyllables (f:[])    = []
toSyllables (f:s:r)   | s`elem` vowels = [f,s] : toSyllables r
toSyllables (f:s:t:r) | t `elem` vowels = [f,s,t] : toSyllables r
toSyllables input     = [input]

-- | Converts sentences to a Jogjakarta styled Walikan to Bahasa, vice versa.
convertJogja :: String -> String
convertJogja input =
  concat . yojanizedSyllables . flattenedInput . syllabledInput . words . map toLower $ input
  where
    syllabledInput :: [String] -> [[String]]
    syllabledInput = map (toSyllables . normalize)
    flattenedInput :: [[String]] -> [String]
    flattenedInput = intercalate [" "]
    yojanizedSyllables :: [String] -> [String]
    yojanizedSyllables = map ruleYoja

-- | Rules of Malang's style of Walikan.
ruleMalang :: String -> String
ruleMalang (f:s:r)   | [f, s] == "ng" || [toLower f, s] == "ng" = ruleMalang r ++ [f, s]
ruleMalang (x:xs)    = ruleMalang xs ++ [x]
ruleMalang otherwise = otherwise

-- | Converts sentences to a Malang styled Walikan to bahasa, vice versa.
convertMalang :: String -> String
convertMalang input = unwords . map ruleMalang . words . map toLower $ input
