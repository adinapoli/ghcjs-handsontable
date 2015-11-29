{-# LANGUAGE OverloadedStrings #-}
module Main where

import Handsontable
import GHCJS.DOM
import GHCJS.DOM.Document
import JavaScript.JQuery as J
import Handsontable.JQuery as J
import Control.Monad

main :: IO ()
main = J.ready $ do
  putStrLn "CSV Editor demo!"
  randomData <- createSpreadsheetData 10 20
  let cfg = disablePlugins (newHandsonConfig randomData)
  void (J.select "#table-example" >>= J.newHandsonTable cfg)
