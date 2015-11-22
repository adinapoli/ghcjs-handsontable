module Main where

import Handsontable
import GHCJS.DOM
import GHCJS.DOM.Document
import Control.Monad

main :: IO ()
main = runWebGUI $ \webView -> do
  putStrLn "CSV Editor demo!"
  randomData <- createSpreadsheetData 10 20
  let cfg = newHandsonConfig randomData
  (Just doc)  <- webViewGetDomDocument webView
  theEl <- getElementById doc "table-example"
  case theEl of
    Nothing -> return ()
    Just el -> void (newHandsonTable cfg el)
