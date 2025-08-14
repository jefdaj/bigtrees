module Cmd.Dupes.Render
  ( DupesRenderFn
  , dupesRenderFunctions
  )
  where

import Cmd.Dupes.Render.Types
import Cmd.Dupes.Render.RsyncFilter
import Cmd.Dupes.Render.Suggestions
import Cmd.Dupes.Render.DedupScript
import Cmd.Dupes.Render.TestScript

dupesRenderFunctions :: [(String, DupesRenderFn)]
dupesRenderFunctions =
  [ ("suggestions"      , renderSuggestions) -- default
  , ("rsync-filter-file", renderRsyncFilter)
  , ("dedup-script"     , renderDedupScript )
  , ("test-script"      , renderTestScript )
  ]
