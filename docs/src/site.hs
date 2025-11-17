--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as TIO
import System.FilePath (takeBaseName, takeDirectory, (</>))
import System.Directory (getCurrentDirectory, doesFileExist)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf, isSuffixOf)

config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs" -- TODO is it too unsafe to have this contain src?
  , providerDirectory = "docs/src"
  , storeDirectory = ".hakyll-cache"
  , inMemoryCache = True
  }

--------------------------------------------------------------------------------

includeStart = "{{" :: String
includeEnd   = "}}" :: String

includesCompiler :: Compiler (Item String)
includesCompiler = do
  let srcDir = providerDirectory config -- TODO make an arg if distributing this
  content <- getResourceString
  processedContent <- unsafeCompiler $ processIncludes srcDir $ itemBody content
  makeItem processedContent

processIncludes :: FilePath -> String -> IO String
processIncludes srcDir content = do
  let includeLines = lines content
  processedLines <- mapM (processLine srcDir) includeLines
  return $ unlines processedLines

processLine :: FilePath -> String -> IO String
processLine srcDir line
  | includeStart `isPrefixOf` line && includeEnd `isSuffixOf` line = do
      let relPath  = extractIncludePath line
          fullPath = srcDir </> relPath
      exists <- doesFileExist fullPath
      if exists
	 then readFile fullPath
         else do
	  putStrLn $ "ERROR included path does not exist:\n" ++ line
          return line
  | otherwise = return line

extractIncludePath :: String -> FilePath
extractIncludePath line =
  let stripped = drop (length includeStart) line
  in take (length stripped - length includeEnd) stripped

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match ("images/*" .||. "casts/*") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["download.md", "questions.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "examples/*" $ do
        route $ setExtension "html"
        compile $
            includesCompiler
            >>= renderPandoc
            >>= loadAndApplyTemplate "templates/example.html" exampleCtx
            >>= loadAndApplyTemplate "templates/default.html" exampleCtx
            >>= relativizeUrls

    create ["examples.html"] $ do
        route idRoute
        compile $ do
	    -- can't do recent first when not giving them dates:
            -- examples <- reverse . recentFirst =<< loadAll "examples/*"
            examples <- loadAll "examples/*"
            let examplesCtx =
                    listField "examples" exampleCtx (return examples) `mappend`
                    constField "title" "Examples" `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/examples.html" examplesCtx
                >>= loadAndApplyTemplate "templates/default.html" examplesCtx
                >>= relativizeUrls


    match "index.html" $ do
	-- TODO replace examples here with a short pitch + pretty picture?
	-- TODO or maybe one basic asciinema demo that autoplays
        route idRoute
        compile $ do
            -- examples <- fmap (take 3 .reverse) . recentFirst =<< loadAll "examples/*"
	    let indexCtx =
                    -- listField "examples" exampleCtx (return examples) `mappend`
                    constField "title" ""                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
exampleCtx :: Context String
exampleCtx =
    -- dateField "date" "%B %e, %Y" `mappend`
    defaultContext
