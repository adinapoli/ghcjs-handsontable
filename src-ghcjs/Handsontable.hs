{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Handsontable where

import GHCJS.Types
import GHCJS.Marshal
import Handsontable.Internal
import JavaScript.Object
import GHCJS.DOM.Types

type Row = JSVal

--------------------------------------------------------------------------------
data HandsonConfig = HandsonConfig {
    hsn_data :: [Row]
  , hsn_minSpareRows :: Maybe Int
  , hsn_rowHeaders :: Maybe Bool
  , hsn_colHeaders :: Maybe Bool
  , hsn_contextMenu :: Maybe Bool
  }

--------------------------------------------------------------------------------
newHandsonTable :: HandsonConfig -> Element -> IO Handsontable
newHandsonTable HandsonConfig{..} el = hst_newHandsontable el =<< marshallCfg
  where
    marshallCfg :: IO Object
    marshallCfg = do
      o <- create
      toJSVal hsn_data >>= \d -> setProp "data" d o
      toJSVal hsn_minSpareRows >>= \m ->setProp "minSpareRows" m o
      toJSVal hsn_rowHeaders >>= \r -> setProp "rowHeaders" r o
      toJSVal hsn_colHeaders >>= \c -> setProp "colHeaders" c o
      toJSVal hsn_contextMenu >>= \cm -> setProp "contextMenu" cm o
      return o

--------------------------------------------------------------------------------
createSpreadsheetData :: Int -> Int -> IO [JSVal]
createSpreadsheetData r c = do
  v <- fromJSVal =<< hst_createSpreadsheetData r c
  case v of
    Nothing -> error "createSpreadsheetData fromJSVal failed"
    Just x -> return x
