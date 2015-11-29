{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Handsontable (
    Data
  , HandsonOptions
  , AutoColumnSize
  , AutoRowSize
  -- * Constructors
  , createSpreadsheetData
  , newHandsonTable
  , newHandsonOptions
  -- * Options getters & setters
  , allowInsertColumn
  , allowInsertRow
  , allowInvalid
  , allowRemoveColumn
  , allowRemoveRow
  , autoColumnSize
  , autoRowSize
  , autoWrapCol
  , autoWrapRow
  , minSpareRows
  , rowHeaders
  , colHeaders
  , contextMenu
  , tableClassName
  -- * Utilities
  , disablePlugins
  -- * Internal use
  , marshalCfg
  ) where

import Data.Aeson as JSON hiding (Object)
import Data.Aeson.TH
import Data.Monoid
import qualified Data.Text as T
import GHCJS.DOM.Types
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import GHCJS.Prim (jsNull)
import GHCJS.Types
import Handsontable.Internal
import JavaScript.Object

--------------------------------------------------------------------------------
type Data = JSVal

--------------------------------------------------------------------------------
data AutoColumnSize = AutoColumnSize {
  autoColumnSyncLimit :: Int
  }

deriveToJSON defaultOptions ''AutoColumnSize

--------------------------------------------------------------------------------
defaultAutoColumnSize :: AutoColumnSize
defaultAutoColumnSize = AutoColumnSize 50

--------------------------------------------------------------------------------
data AutoRowSize = AutoRowSize {
  autoRowSyncLimit :: Int
  }

deriveToJSON defaultOptions ''AutoRowSize

--------------------------------------------------------------------------------
defaultAutoRowSize :: AutoRowSize
defaultAutoRowSize = AutoRowSize 1000

--------------------------------------------------------------------------------
-- |Setting true or false will enable or disable the default column headers (A, B, C).
-- You can also define an array ['One', 'Two', 'Three', ...] or a function to define the headers.
-- If a function is set the index of the column is passed as a parameter.
data ColHeaders =
    ColHeaders_Bool Bool
  | ColHeaders_Array [T.Text]
  | ColHeaders_Func  (JSVal -> IO ())

instance ToJSVal ColHeaders where
  toJSVal (ColHeaders_Bool b) = toJSVal b
  toJSVal (ColHeaders_Array a) = toJSVal a
  toJSVal (ColHeaders_Func f)  = jsval <$> asyncCallback1 f

--------------------------------------------------------------------------------
data HandsonOptions = HandsonOptions {
    _data :: Data
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
  -- , cell      Not supported yet
  -- , cells      Not supported yet
  -- , checkedTemplate      Not supported yet
  -- , className      Not supported yet
  , colHeaders   :: ColHeaders
  , minSpareRows :: Int
  , rowHeaders   :: Bool
  , contextMenu  :: Bool
  , tableClassName :: [T.Text]
  }

--------------------------------------------------------------------------------
defaultHandsonOptions :: HandsonOptions
defaultHandsonOptions = HandsonOptions {
    _data = jsNull
  , allowInsertColumn = True
  , allowInsertRow = True
  , allowInvalid = True
  , allowRemoveColumn = True
  , allowRemoveRow = True
  , autoColumnSize = Just defaultAutoColumnSize
  , autoRowSize = Just defaultAutoRowSize
  , autoWrapCol  = False
  , autoWrapRow  = False
  , minSpareRows = 1
  , rowHeaders   = True
  , colHeaders   = ColHeaders_Bool True
  , contextMenu  = False
  , tableClassName = mempty
  }

--------------------------------------------------------------------------------
newHandsonOptions :: Data -> HandsonOptions
newHandsonOptions dt = defaultHandsonOptions { _data = dt }

--------------------------------------------------------------------------------
disablePlugins :: HandsonOptions -> HandsonOptions
disablePlugins cfg = cfg { autoColumnSize = Nothing
                         , autoRowSize    = Nothing
                         }

--------------------------------------------------------------------------------
newHandsonTable :: HandsonOptions -> Element -> IO Handsontable
newHandsonTable cfg el = hst_newHandsontable el =<< marshalCfg cfg

--------------------------------------------------------------------------------
marshalCfg :: HandsonOptions -> IO Object
marshalCfg HandsonOptions{..}= do
  o <- create
  o ..= ("data", _data)
  o ..= ("allowInsertColumn", allowInsertColumn)
  o ..= ("allowInsertRow", allowInsertRow)
  o ..= ("allowInvalid", allowInvalid)
  o ..= ("allowRemoveColumn", allowRemoveColumn)
  o ..= ("allowRemoveRow", allowRemoveRow)
  o |.= ("autoColumnSize", autoColumnSize)
  o |.= ("autoRowSize", autoRowSize)
  o ..= ("autoWrapCol", autoWrapCol)
  o ..= ("autoWrapRow", autoWrapRow)
  o ..= ("minSpareRows", minSpareRows)
  o ..= ("rowHeaders", rowHeaders)
  o ..= ("colHeaders", colHeaders)
  o ..= ("contextMenu", contextMenu)
  o ..= ("tableClassName", tableClassName)
  return o

--------------------------------------------------------------------------------
(..=) :: (ToJSVal a) => Object -> (JSString, a) -> IO ()
o ..= (k,v) = toJSVal v >>= flip (setProp k) o

--------------------------------------------------------------------------------
(|.=) :: (ToJSON a) => Object -> (JSString, a) -> IO ()
o |.= (k,v) = toJSVal (JSON.toJSON v) >>= flip (setProp k) o

--------------------------------------------------------------------------------
createSpreadsheetData :: Int -> Int -> IO JSVal
createSpreadsheetData r c = do
  v <- fromJSVal =<< hst_createSpreadsheetData r c
  case v of
    Nothing -> error "createSpreadsheetData fromJSVal failed"
    Just x -> return x
