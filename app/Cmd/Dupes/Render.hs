module Cmd.Dupes.Render
  ( DupesRenderFn
  , dupesRenderFunctions
  )
  where

import Cmd.Dupes.Render.Types
import Cmd.Dupes.Render.RsyncFilter
import Cmd.Dupes.Render.Suggestions

dupesRenderFunctions :: [(String, DupesRenderFn)]
dupesRenderFunctions =
  [ ("suggestions"      , renderSuggestions)
  , ("rsync-filter-file", renderRsyncFilter)
  ]
