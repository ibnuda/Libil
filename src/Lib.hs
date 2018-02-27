{-# LANGUAGE OverloadedStrings #-}
module Lib where

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

normalize :: String -> String
normalize (x:xs)    | x `elem` vowels = 'h':x:xs
normalize otherwise = otherwise

ruleYoja :: String -> String
ruleYoja (f:s:r) =
  case Map.lookup [f, s] pairsOfConsonants of
    Nothing ->
      case Map.lookup [f] pairsOfConsonants of
        Nothing  -> f : s : ruleYoja r
        Just syl -> syl ++ ruleYoja (s : r)
    Just syl -> syl ++ ruleYoja r
ruleYoja (f:r) =
  case Map.lookup [f] pairsOfConsonants of
    Nothing  -> f : r
    Just syl -> syl ++ r
ruleYoja [] = ""

toSyllables :: String -> [String]
toSyllables []        = []
toSyllables ( f:[] )  = []
toSyllables (f:s:r)   | s`elem` vowels = [f,s] : toSyllables r
toSyllables (f:s:t:r) | t `elem` vowels = [f,s,t] : toSyllables r
toSyllables input     = [input]

toWalikanYoja :: String -> [Char]
toWalikanYoja =
  concat .
  map ruleYoja . intercalate [" "] . map (toSyllables . normalize) . words

toYoja :: String -> String
toYoja input =
  let syllabledInput = map (toSyllables . normalize)
      flattenedInput = intercalate [" "]
      yojanizedSyllables = map ruleYoja
  in concat
       (yojanizedSyllables . flattenedInput . syllabledInput . words $ input)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
