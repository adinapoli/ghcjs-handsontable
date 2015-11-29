{-# LANGUAGE JavaScriptFFI #-}
module Handsontable.JQuery.Internal where

import JavaScript.JQuery
import GHCJS.Types
import JavaScript.Object (Object)

newtype Handsontable = Handsontable JSVal

foreign import javascript unsafe
 "$1.handsontable($2)" hst_newHandsontable :: JQuery -> Object -> IO Handsontable
