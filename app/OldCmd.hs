module OldCmd
  -- ( oldCmdAdd
  -- , cmdDedup
  ( oldCmdCat
  , oldCmdDiff
  , oldCmdDupes
  , oldCmdHash
  -- , oldCmdInit
  -- , oldCmdMv
  -- , oldCmdRm
  , oldCmdTest
  , oldCmdUpdate
  )
  where

import OldCmd.Cat (oldCmdCat)
import OldCmd.Diff (oldCmdDiff)
import OldCmd.Dupes (oldCmdDupes)
import OldCmd.Hash (oldCmdHash)
import OldCmd.Test (oldCmdTest)
import OldCmd.Update (oldCmdUpdate)
