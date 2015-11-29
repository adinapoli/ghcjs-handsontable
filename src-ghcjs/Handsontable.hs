{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Handsontable (
    Row
  , HandsonConfig
  , AutoColumnSize
  , AutoRowSize
  , CellOverride
  , newOverride
  , override
  , createSpreadsheetData
  , newHandsonTable
  , newHandsonConfig
  , minSpareRows
  , rowHeaders
  , colHeaders
  , contextMenu
  , disablePlugins
  -- * Internal use
  , marshalCfg
  ) where

import GHCJS.Types
import GHCJS.Marshal
import Handsontable.Internal
import JavaScript.Object
import GHCJS.DOM.Types

--------------------------------------------------------------------------------
type Row = JSVal

--------------------------------------------------------------------------------
data AutoColumnSize = AutoColumnSize {
  autoColumnSyncLimit :: Int
  }

--------------------------------------------------------------------------------
defaultAutoColumnSize :: AutoColumnSize
defaultAutoColumnSize = AutoColumnSize 50

--------------------------------------------------------------------------------
data AutoRowSize = AutoRowSize {
  autoRowSyncLimit :: Int
  }

--------------------------------------------------------------------------------
defaultAutoRowSize :: AutoRowSize
defaultAutoRowSize = AutoRowSize 1000

--------------------------------------------------------------------------------
data CellOverride = CellOverride {
    cellOverrideRow :: Int
  , cellOverrideCol :: Int
  , cellOverride :: HandsonConfig
  }

--------------------------------------------------------------------------------
data HandsonConfig = HandsonConfig {
    _data :: [Row]
  , allowInsertColumn :: Bool
  , allowInsertRow :: Bool
  , allowInvalid :: Bool
  , allowRemoveColumn :: Bool
  , allowRemoveRow :: Bool
  , autoColumnSize :: Maybe AutoColumnSize
  -- , autocomplete -- Not supported yet
  , autoRowSize  :: Maybe AutoRowSize
  , autoWrapCol  :: Bool
  , autoWrapRow  :: Bool
  , cell         :: [CellOverride]
  , minSpareRows :: Int
  , rowHeaders   :: Bool
  , colHeaders   :: Bool
  , contextMenu  :: Bool
  }

--------------------------------------------------------------------------------
defaultHandsonConfig :: HandsonConfig
defaultHandsonConfig = HandsonConfig {
    _data = []
  , allowInsertColumn = True
  , allowInsertRow = True
  , allowInvalid = True
  , allowRemoveColumn = True
  , allowRemoveRow = True
  , autoColumnSize = Just defaultAutoColumnSize
  , autoRowSize = Just defaultAutoRowSize
  , autoWrapCol  = False
  , autoWrapRow  = False
  , cell = mempty
  , minSpareRows = 1
  , rowHeaders   = True
  , colHeaders   = True
  , contextMenu  = False
  }

--------------------------------------------------------------------------------
newOverride :: Int -> Int -> HandsonConfig -> CellOverride
newOverride = CellOverride

--------------------------------------------------------------------------------
override :: HandsonConfig -> CellOverride -> HandsonConfig
override cfg@HandsonConfig{..} o = cfg { cell = o : cell }

--------------------------------------------------------------------------------
newHandsonConfig :: [Row] -> HandsonConfig
newHandsonConfig dt = defaultHandsonConfig { _data = dt }

--------------------------------------------------------------------------------
disablePlugins :: HandsonConfig -> HandsonConfig
disablePlugins cfg = cfg { autoColumnSize = Nothing
                         , autoRowSize    = Nothing
                         }

--------------------------------------------------------------------------------
newHandsonTable :: HandsonConfig -> Element -> IO Handsontable
newHandsonTable cfg el = hst_newHandsontable el =<< marshalCfg cfg

--------------------------------------------------------------------------------
marshalCfg :: HandsonConfig -> IO Object
marshalCfg HandsonConfig{..}= do
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
