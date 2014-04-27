{-# LANGUAGE ImplicitParams #-}
module Main where

import Diablo
import Text.Printf

main :: IO ()
main = run $ do
   printf "Grepping %s stats for %s %s:\n" ?host ?battletag ?char
   generate
