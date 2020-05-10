
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

import           Data.Monoid                     (mappend)
import           Hakyll

import           Control.Applicative             (Alternative (..))
import           Control.Monad                   (forM_, liftM, zipWithM_)
import           Data.Csv
import           Data.List                       (findIndex, intercalate,
                                                  isPrefixOf, sortBy, tails)
import           Data.Maybe                      (fromMaybe)

import qualified Data.ByteString.Lazy            as BL
import           Data.List.Split
import qualified Data.Vector                     as V
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

orElse :: Maybe a -> a -> a
orElse Nothing def  = def
orElse (Just val) _ = val

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

    match "papers/files/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    postCategories <- buildCategories postsGlob (fromCapture "postCategories/**.html")
    paperCategories <- buildCategories papersGlob (fromCapture "paperCategories/**.html")

    rulesForTags postCategories (\tag -> "Posts in category \"" ++ tag ++ "\"")
    rulesForTags paperCategories (\tag -> "Papers in category \"" ++ tag ++ "\"")

    peerList <- preprocess $ fmap (fmap $ uncurry Peer) peerInfo

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
            >>= loadAndApplyTemplate "templates/default.html" (postCtx `mappend` (paperCtx peerList))
            >>= relativizeUrls


    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll postsGlob
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
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
            let archiveCtx =
                    listField "papers" (paperCtx peerList) (return papers) `mappend`
                    constField "title" "Papers"                `mappend`
                    field "categorylist" (\_ -> renderTagListLines paperCategories) `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/paper-list.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
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

data Peer = Peer String String

peerInfo :: IO [(String, String)]
peerInfo = do
    csvData <- BL.readFile "peers.csv"
    case decode NoHeader csvData of
        Left _  -> return []
        Right v -> return $ V.toList v

find :: String -> [Peer] -> Maybe Peer
find _ [] = Nothing
find fid (p@(Peer entryID _ ):xs) | entryID == fid = Just p
                                  | otherwise = find fid xs

peerLink :: Peer -> String
peerLink (Peer name website) = "<a href=" ++ website ++ ">" ++ name ++ "</a>"

displayContentFor :: [Peer] -> String -> String
displayContentFor peers pid = valueOrDefault $ link currentPeer
    where
        link = fmap peerLink
        currentPeer = find pid peers
        valueOrDefault = flip orElse pid

authorsLine :: [Peer] -> String -> String
authorsLine peers = intercalate ", " . fmap (displayContentFor peers) . splitOn ", "

authorsCtx :: [Peer] -> Context String
authorsCtx peers = field "authors" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return $ fromMaybe "" $ authorsLine peers <$> lookupString "authors" metadata

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

paperCtx :: [Peer] -> Context String
paperCtx peers = authorsCtx peers `mappend` postCtx

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
