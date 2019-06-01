
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

import Data.List (isPrefixOf, tails, findIndex, intercalate, sortBy)
import Data.Maybe (fromMaybe)
import Control.Applicative (Alternative (..))
import Control.Monad (forM_, zipWithM_, liftM)

import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html ((!), toHtml, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

postsGlob :: Pattern
postsGlob = "posts/**.md"

papersGlob :: Pattern
papersGlob = "papers/**.md"

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    postCategories <- buildCategories postsGlob (fromCapture "postCategories/**.html")
    paperCategories <- buildCategories papersGlob (fromCapture "paperCategories/**.html")

    rulesForTags postCategories (\tag -> "Posts in category \"" ++ tag ++ "\"")
    rulesForTags paperCategories (\tag -> "Papers in category \"" ++ tag ++ "\"")

    match postsGlob $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "blog"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "jobs/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/job.html" metaCtx
            >>= relativizeUrls

    match papersGlob $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/paper.html" metaCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls


    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll postsGlob
            let archiveCtx =
                    listField "posts" postCtx (return (reverse posts)) `mappend`
                    constField "title" "Blog"                `mappend`
                    field "categorylist" (\_ -> renderTagListLines postCategories) `mappend`
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
	     papers <- recentFirst =<< loadAll papersGlob
	     let papersCtx =
 	     	  listField "posts" metaCtx (return papers) `mappend`
		  constField "title" "Papers" `mappend`
                  field "categorylist" (\_ -> renderTagListLines paperCategories) `mappend`
		  defaultContext
             makeItem ""
                  >>= loadAndApplyTemplate "templates/post-list.html" papersCtx
                  >>= loadAndApplyTemplate "templates/default.html" papersCtx
                  >>= relativizeUrls

    create ["rss.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
            loadAllSnapshots "posts/*" "blog"
        renderRss blogFeedConfiguration feedCtx posts

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

blogFeedConfiguration :: FeedConfiguration
blogFeedConfiguration = FeedConfiguration
    { feedTitle       = "Lifted"
    , feedDescription = "A personal blog focused on technology, and philosophy."
    , feedAuthorName  = "John McAvey"
    , feedAuthorEmail = "john@mcavey.net"
    , feedRoot        = "http://john.mcavey.net"
    }

renderTagListLines :: Tags -> Compiler (String)
renderTagListLines =
    renderTags makeLink (intercalate ",<br>")
  where
    makeLink tag url count _ _ = renderHtml $
        H.a ! A.href (toValue url) $ toHtml (tag ++ " (" ++ show count ++ ")")

rulesForTags :: Tags -> (String -> String) -> Rules ()
rulesForTags tags titleForTag =
    tagsRules tags $ \tag pattern -> do
    let title = titleForTag tag -- tag ++ " Posts"
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx = constField "title" title
                  `mappend` listField "posts" postCtx (return posts)
                  `mappend` field "categorylist" (\_ -> renderTagListLines tags)
                  `mappend` defaultContext

        makeItem ""
            >>= loadAndApplyTemplate "templates/tag.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls
