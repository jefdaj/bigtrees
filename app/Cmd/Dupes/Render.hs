module Cmd.Dupes.Render
  ( renderDupesFunctions
  )
  where

import qualified System.Directory.BigTrees as BT

dupesRenderFunctions :: [(String, BT.ExplainFn)]
dupesRenderFunctions =
  [ ("suggestions"      , renderDupesSuggestions)
  , ("rsync-filter-file", renderDupesRsyncFilter)
  ]

import Cmd.Dupes.Render.RsyncFilter (renderDupesRsyncFilter)
import Cmd.Dupes.Render.Suggestions (renderDupesSuggestions)
