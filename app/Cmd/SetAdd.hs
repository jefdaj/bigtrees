module Cmd.SetAdd where

import Text.Pretty.Simple (pPrint)
import Config (Config)

cmdSetAdd :: Config -> FilePath -> Maybe String -> [FilePath] -> IO ()
cmdSetAdd cfg outfile note trees = do
  pPrint cfg
  pPrint outfile
  pPrint note
  pPrint trees
