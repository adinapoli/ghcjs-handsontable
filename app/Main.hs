module Main where

import Handsontable
import GHCJS.DOM
import GHCJS.DOM.Document
import Control.Monad

main :: IO ()
main = runWebGUI $ \webView -> do
  putStrLn "CSV Editor demo!"
  randomData <- createSpreadsheetData 10 20
  let cfg = HandsonConfig {
            hsn_data = randomData
          , hsn_minSpareRows = Just 1
          , hsn_rowHeaders = Nothing
          , hsn_colHeaders = Nothing
          , hsn_contextMenu = Nothing
          }

  (Just doc)  <- webViewGetDomDocument webView
  theEl <- getElementById doc "table-example"
  case theEl of
    Nothing -> return ()
    Just el -> void (newHandsonTable cfg el)
