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

    match "usecases/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/usecase.html" usecaseCtx
            >>= loadAndApplyTemplate "templates/default.html" usecaseCtx
            >>= relativizeUrls

    create ["usecases.html"] $ do
        route idRoute
        compile $ do
            usecases <- fmap reverse . recentFirst =<< loadAll "usecases/*"
            let usecasesCtx =
                    listField "usecases" usecaseCtx (return usecases) `mappend`
                    constField "title" "Use Cases" `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/usecases.html" usecasesCtx
                >>= loadAndApplyTemplate "templates/default.html" usecasesCtx
                >>= relativizeUrls


    match "index.html" $ do
	-- TODO replace usecases here with a short pitch + pretty picture?
	-- TODO or maybe one basic asciinema demo that autoplays
        route idRoute
        compile $ do
            -- usecases <- fmap (take 3 .reverse) . recentFirst =<< loadAll "usecases/*"
            let indexCtx =
                    -- listField "usecases" usecaseCtx (return usecases) `mappend`
                    constField "title" ""                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
usecaseCtx :: Context String
usecaseCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

