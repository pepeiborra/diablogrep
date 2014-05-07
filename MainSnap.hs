{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module MainSnap where

import           Control.Applicative
import Control.Monad.Trans
import Data.ByteString.Char8 (pack)
import Data.Foldable
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import Text.Printf

import Diablo

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "hello") <|>
    route [ ("fetch", fetchHandler)
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (serveDirectory ".")

fetchHandler :: MonadSnap m => m ()
fetchHandler = run $ do
  writeBS(pack $ printf "Grepping %s stats for %s %s:\n" ?host ?battletag ?char )
  liftIO diablo >>= writeBS . pack

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

diablo = do
--  printf "Grepping %s stats for %s %s:\n" ?host ?battletag ?char
  text <- toList `fmap` generate
  return (unlines text)
