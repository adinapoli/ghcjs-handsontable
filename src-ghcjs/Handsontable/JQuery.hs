{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Handsontable.JQuery (
  newHandsonTable
  ) where

import Handsontable (marshalCfg, HandsonOptions)
import Handsontable.JQuery.Internal
import JavaScript.JQuery

--------------------------------------------------------------------------------
newHandsonTable :: HandsonOptions -> JQuery -> IO Handsontable
newHandsonTable cfg jq = hst_newHandsontable jq =<< marshalCfg cfg
