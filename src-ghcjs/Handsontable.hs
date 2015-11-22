{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Handsontable (
    Row
  , HandsonConfig
  , createSpreadsheetData
  , newHandsonTable
  , newHandsonConfig
  , minSpareRows
  , rowHeaders
  , colHeaders
  , contextMenu
  ) where

import GHCJS.Types
import GHCJS.Marshal
import Handsontable.Internal
import JavaScript.Object
import GHCJS.DOM.Types

type Row = JSVal

--------------------------------------------------------------------------------
data HandsonConfig = HandsonConfig {
    _data :: [Row]
  , minSpareRows :: Int
  , rowHeaders   :: Bool
  , colHeaders   :: Bool
  , contextMenu  :: Bool
  }

--------------------------------------------------------------------------------
defaultHandsonConfig :: HandsonConfig
defaultHandsonConfig = HandsonConfig mempty 1 True True False

--------------------------------------------------------------------------------
newHandsonConfig :: [Row] -> HandsonConfig
newHandsonConfig dt = defaultHandsonConfig { _data = dt }

--------------------------------------------------------------------------------
newHandsonTable :: HandsonConfig -> Element -> IO Handsontable
newHandsonTable HandsonConfig{..} el = hst_newHandsontable el =<< marshallCfg
  where
    marshallCfg :: IO Object
    marshallCfg = do
      o <- create
      toJSVal _data >>= \d -> setProp "data" d o
      toJSVal minSpareRows >>= \m ->setProp "minSpareRows" m o
      toJSVal rowHeaders >>= \r -> setProp "rowHeaders" r o
      toJSVal colHeaders >>= \c -> setProp "colHeaders" c o
      toJSVal contextMenu >>= \cm -> setProp "contextMenu" cm o
      return o

--------------------------------------------------------------------------------
createSpreadsheetData :: Int -> Int -> IO [JSVal]
createSpreadsheetData r c = do
  v <- fromJSVal =<< hst_createSpreadsheetData r c
  case v of
    Nothing -> error "createSpreadsheetData fromJSVal failed"
    Just x -> return x
