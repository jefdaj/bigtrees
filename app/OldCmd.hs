module OldCmd
  -- ( oldCmdAdd
  -- , cmdDedup
  ( oldCmdCat
  , cmdDiff
  , cmdDupes
  , cmdHash
  -- , oldCmdInit
  -- , oldCmdMv
  -- , oldCmdRm
  -- , oldCmdTest
  , oldCmdUpdate
  )
  where

import Cmd.Diff (cmdDiff)
import Cmd.Dupes (cmdDupes)
import Cmd.Hash (cmdHash)
import OldCmd.Cat (oldCmdCat)
-- import OldCmd.Test (oldCmdTest)
import OldCmd.Update (oldCmdUpdate)
