{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( someFunc
  ) where

import           Data.Attoparsec.Text
import           Data.Text

-- | Consonants of https://en.wikipedia.org/wiki/Hanacaraka
-- javanese script.
consonants :: [Text]
consonants = [ "h", "n", "c", "r", "k"
             , "d", "t", "s", "w", "l"
             , "p", "dh", "j", "y", "ny"
             , "m", "g", "b", "th", "ng"
             ]

someFunc :: IO ()
someFunc = putStrLn "someFunc"
