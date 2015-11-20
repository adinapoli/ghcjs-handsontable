{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Handsontable where

import GHCJS.Types
import GHCJS.Marshal
import Handsontable.Internal
import JavaScript.Object
import GHCJS.DOM.Types

type Row = [JSVal]

data HandsonConfig = HandsonConfig {
    _data :: [Row]
  , minSpareRows :: Maybe Int
  , rowHeaders :: Maybe Bool
  , colHeaders :: Maybe Bool
  , contextMenu :: Maybe Bool
  }

newHandsonTable :: Element -> HandsonConfig -> IO Handsontable
newHandsonTable el HandsonConfig{..} = hst_newHandsontable el =<< marshallCfg
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
