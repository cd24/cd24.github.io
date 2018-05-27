--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "jobs/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/job.html" metaCtx
            >>= relativizeUrls

    match "papers/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/paper.html" metaCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls


    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["work.html"] $ do
        route idRoute
        compile $ do
          jobs <- recentFirst =<< loadAll "jobs/*"
          let jobsCtx =
                listField "alljobs" metaCtx (return jobs) `mappend`
                constField "title" "Work" `mappend`
                defaultContext
          makeItem ""
            >>= loadAndApplyTemplate "templates/job-list.html" jobsCtx
            >>= loadAndApplyTemplate "templates/default.html" jobsCtx
            >>= relativizeUrls

    create ["papers.html"] $ do
        route idRoute
	compile $ do
	     papers <- recentFirst =<< loadAll "papers/*"
	     let papersCtx =
 	     	  listField "posts" metaCtx (return papers) `mappend`
		  constField "title" "Papers" `mappend`
		  defaultContext
             makeItem ""
                  >>= loadAndApplyTemplate "templates/post-list.html" papersCtx
                  >>= loadAndApplyTemplate "templates/default.html" papersCtx
                  >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

metaCtx :: Context String
metaCtx = postCtx `mappend` metadataField
