{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid      ((<>))
import           Hakyll           hiding (paginateContext)
import           System.Directory (getCurrentDirectory)
import           System.Exit      (ExitCode)
import           System.Process   (system)
import           Paginator
import           Rewriter
import           Sass
import           Sitemap

postsPerPage :: Int
postsPerPage = 5

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration {
    feedTitle       = "Coding Infinity"
  , feedDescription = "Personal blog of Jakub Fija\x142kowski"
  , feedAuthorName  = "Jakub Fija\x142kowski"
  , feedAuthorEmail = "fiolek@fiolek.org"
  , feedRoot        = "https://www.codinginfinity.me"
}

siteConfig :: Configuration
siteConfig = def {
    deploySite = \_ -> deployWithWinSCP
}

sitemapConfig :: SitemapConfiguration
sitemapConfig = def {
    sitemapBase     = "https://codinginfinity.me/"
  , sitemapRewriter = rewriteUrl
}

main :: IO ()
main = hakyllWith siteConfig $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromRegex "css/[^_].*") $ do
        route   $ setExtension "css"
        compile sassCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "web.config" $ do
        route   idRoute
        compile copyFileCompiler

    match "posts/*" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx

    match "drafts/*" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/draft.html"   draftCtx
            >>= loadAndApplyTemplate "templates/default.html" draftCtx

    match "pages/*" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" customContext

    match "templates/*" $ compile templateCompiler

    match "favicon.ico" $ do
        route   idRoute
        compile copyFileCompiler

    -- Paginate page
    paginatedPosts <- buildPaginateWith
        (fmap (paginateEvery postsPerPage) . sortRecentFirst)
        "posts/*"
        (fromFilePath . postsPagePath)
    paginateRules paginatedPosts $ \page pattern -> do
        route   idRoute
        compile $ do
            posts <- loadAll pattern >>= recentFirst
            let ctx =
                    listField "posts" postCtx (return posts) <>
                    postPageCtx paginatedPosts page
            makeItem ""
                >>= loadAndApplyTemplate "templates/post-list.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx

    -- RSS feed
    create ["rss.xml"] $ do
        route   idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            posts <- loadAllSnapshots "posts/*" "content" >>= recentFirst
            renderRss feedConfig feedCtx posts

    create ["sitemap.xml"] $ do
        route   idRoute
        compile $ generateSitemap sitemapConfig

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"                                    <>
    dateField "isoDate" isoFormat                                   <>
    teaserField "teaser" "content"                                  <>
    mapContext sanitize (teaserField "meta_description" "content")  <>
    customContext
    where
        isoFormat = "%Y-%m-%dT%H:%M:%S"
        sanitize = stripTags . filter (/= '\n')

draftCtx :: Context String
draftCtx = customContext

postPageCtx :: Paginate -> PageNumber -> Context String
postPageCtx paginate page =
    paginateContext paginate page                  <>
    field "title" (pageTitleField page)            <>
    field "hasPaginator" hasPaginator              <>
    listField "pages" (pageLinkCtx page) pageItems <>
    customContext
    where
        totalPages = paginateNumPages paginate
        pageItems = return $ fmap makePageItem (postPagesFor totalPages page)
        hasPaginator _ = failIf (totalPages == 1) ""
        makePageItem i = Item (fromFilePath "") i
        pageTitleField p _ = failIf (page == 1) $ "Page " ++ show p

pageLinkCtx :: PageNumber -> Context Int
pageLinkCtx page =
    field "pageLink" makeLink <>
    field "pageNo" makeLabel  <>
    field "isCurrentPage" isCurrentPage
    where
        makeLink (Item _ i) = return $ rewriteUrl $ '/' : postsPagePath i
        makeLabel (Item _ i)
            | i == -1 = return "&hellip;"
            | otherwise = return $ show i
        isCurrentPage (Item _ i) = failIf (page /= i && i /= -1) ""

customContext :: Context String
customContext =
    bodyField "body"                       <>
    metadataField                          <>
    mapContext rewriteUrl (urlField "url") <>
    pathField "path"

failIf :: (Monad m) => Bool -> a -> m a
failIf True _ = fail "failWhen"
failIf False v = return v

deployWithWinSCP :: IO ExitCode
deployWithWinSCP = do
    dir <- getCurrentDirectory
    let siteDir = dir ++ "\\_site"
    system $ "winscp.com /script=\"deploy.txt\" /parameter // \"" ++ siteDir ++ "\""
