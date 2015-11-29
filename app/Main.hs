{-# LANGUAGE OverloadedStrings #-}
module Main where

import Handsontable
import JavaScript.JQuery as J
import Handsontable.JQuery as J
import Control.Monad

main :: IO ()
main = J.ready $ do
  randomData <- createSpreadsheetData 10 20
  let cfg = disablePlugins (newHandsonOptions randomData)
  void (J.select "#table-example" >>= J.newHandsonTable cfg)
