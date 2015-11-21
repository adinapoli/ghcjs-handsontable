module Main where

import Handsontable
import GHCJS.DOM.Document
import JavaScript.JQuery as J
import Control.Monad

main :: IO ()
main = do
  putStrLn "CSV Editor demo!"
  randomData <- createSpreadsheetData 10 20
  let cfg = HandsonConfig {
            hsn_data = randomData
          , hsn_minSpareRows = Just 1
          , hsn_rowHeaders = Nothing
          , hsn_colHeaders = Nothing
          , hsn_contextMenu = Nothing
          }
  doc <- newDocument
  theEl <- getElementById doc "table-example"
  case theEl of
    Nothing -> return ()
    Just el -> void (newHandsonTable cfg el)
