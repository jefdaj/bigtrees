--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as TIO
import System.FilePath (takeBaseName, (</>))
import Data.List (isPrefixOf, isSuffixOf)

config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs" -- TODO is it too unsafe to have this contain src?
  , providerDirectory = "docs/src"
  , storeDirectory = ".hakyll-cache"
  , inMemoryCache = True
  }

--------------------------------------------------------------------------------

includeSnippetsCompiler :: Compiler (Item String)
includeSnippetsCompiler = do
  content <- getResourceString
  processedContent <- unsafeCompiler $ processIncludes (itemBody content)
  makeItem processedContent

processIncludes :: String -> IO String
processIncludes content = do
  let includeLines = lines content
  processedLines <- mapM processLine includeLines
  return $ unlines processedLines

processLine :: String -> IO String
processLine line
  | ("{{include:" :: String) `isPrefixOf` line && ("}}" :: String) `isSuffixOf` line = do
      let snippetPath = extractSnippetPath line
      snippetContent <- readFile snippetPath
      return snippetContent
  | otherwise = return line

extractSnippetPath :: String -> FilePath
extractSnippetPath line =
  let stripped = drop (length ("{{include:" :: String)) line
  in take (length stripped - length ("}}" :: String)) stripped

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
            includeSnippetsCompiler
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
