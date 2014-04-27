{-# LANGUAGE ImplicitParams #-}
module Main where

import Diablo
import Data.Traversable
import Text.Printf
import Prelude hiding (mapM)

main :: IO ()
main = run $ do
   printf "Grepping %s stats for %s %s:\n" ?host ?battletag ?char
   generate >>= mapM putStrLn
   return ()
