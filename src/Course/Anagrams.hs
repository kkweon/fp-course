{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.Anagrams where

import Course.Core
import Course.Functor
import Course.List

{-

Functions you will need
--
* fmap :: (a -> b) -> IO a -> IO b
* readFile :: FilePath -> IO Str
* lines :: Str -> [Str]
* permutations :: [a] -> [[a]]
* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
* toLower :: Char -> Char

Functions that might help
-
* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}
-- Return all anagrams of the given string
-- that appear in the given dictionary file.
anagrams :: Chars -> FilePath -> IO (List Chars)
anagrams text filename = go <$> readFile filename
  where
    go :: Chars -> List Chars
    go fileContent =
      intersectBy equalIgnoringCase (permutations text) (lines fileContent)

-- Compare two strings for equality, ignoring case
equalIgnoringCase :: Chars -> Chars -> Bool
equalIgnoringCase = on (==) (map toUpper)
