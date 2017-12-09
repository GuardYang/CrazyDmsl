{-# LANGUAGE ApplicativeDo #-}

module Main where

import Parsers
import CrazyDmsl

main :: IO ()
main = let file = "in.mbg" in do
  allCodes <- readFile file
  putStrLn "File contents:"
  putStrLn allCodes
  print $ p <!-- allCodes
  where p = do
          f <- crazyDmslP
          newLines0P
          return f
--
