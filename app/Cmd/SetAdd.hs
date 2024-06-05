module Cmd.SetAdd where

import Text.Pretty.Simple (pPrint)
import Config (Config)

cmdSetAdd :: Config -> FilePath -> Maybe String -> [FilePath] -> IO ()
cmdSetAdd cfg set note trees = do
  pPrint cfg
  pPrint set
  pPrint note
  pPrint trees
