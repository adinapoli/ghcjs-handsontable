{-# LANGUAGE JavaScriptFFI #-}
module Handsontable.Internal where

import GHCJS.DOM.Types
import GHCJS.Types
import JavaScript.Object (Object)

newtype Handsontable = Handsontable JSVal

foreign import javascript unsafe
 "new HandsonTable($1,$2)" hst_newHandsontable :: Element -> Object -> IO Handsontable
