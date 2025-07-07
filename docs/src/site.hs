--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs" -- TODO is it too unsafe to have this contain src?
  , providerDirectory = "docs/src"
  , storeDirectory = ".hakyll-cache"
  , inMemoryCache = True
  }

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
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/example.html" exampleCtx
            >>= loadAndApplyTemplate "templates/default.html" exampleCtx
            >>= relativizeUrls

    create ["examples.html"] $ do
        route idRoute
        compile $ do
            examples <- fmap reverse . recentFirst =<< loadAll "examples/*"
            let examplesCtx =
                    listField "examples" exampleCtx (return examples) `mappend`
                    constField "title" "Examples"            `mappend`
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
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

