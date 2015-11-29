{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Handsontable.JQuery (
  newHandsonTable
  ) where

import Handsontable (marshalCfg, HandsonConfig)
import Handsontable.JQuery.Internal
import JavaScript.JQuery

--------------------------------------------------------------------------------
newHandsonTable :: HandsonConfig -> JQuery -> IO Handsontable
newHandsonTable cfg jq = hst_newHandsontable jq =<< marshalCfg cfg
