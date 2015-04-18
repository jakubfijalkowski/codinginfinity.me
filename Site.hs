{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map         as M
import           Data.Monoid             ((<>), mconcat)
import           Hakyll           hiding (paginateContext)
import           System.Directory        (getCurrentDirectory)
import           System.Exit             (ExitCode)
import           System.Process          (system)
import           Text.Regex.TDFA         ((=~))
import           Text.Sass


postRegex :: String
postRegex = "/posts/(.+-.+-.+)-(.+)\\.html"

postPageRegex :: String
postPageRegex = "/page_(.+)\\.html"

postsPagePath :: Int -> String
postsPagePath 1 = "index.html"
postsPagePath n = "page_" ++ (show n) ++ ".html"

staticPageRegex :: String
staticPageRegex = "/(.+)\\.html"

rewriteUrl :: String -> String
rewriteUrl "/index.html" = "/"
rewriteUrl s
    | [_:d:t:_] <- s =~ postRegex     = "/post/" ++ d ++ "/" ++ t
    | [_:p:_] <- s =~ postPageRegex   = "/page/" ++ p
    | [_:p:_] <- s =~ staticPageRegex = "/"      ++ p
    | otherwise = s

postsPerPage :: Int
postsPerPage = 5

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration {
    feedTitle       = "Coding Infinity"
  , feedDescription = "Personal blog of Jakub Fija\x142kowski"
  , feedAuthorName  = "Jakub Fija\x142kowski"
  , feedAuthorEmail = "fiolek@fiolek.org"
  , feedRoot        = "http://www.codinginfinity.me"
}

siteConfig :: Configuration
siteConfig = def {
    deploySite = \_ -> deployWithWinSCP
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
            >>= saveSnapshot "feed"
            >>= loadAndApplyTemplate "templates/default.html" postCtx

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
            posts <- loadAllSnapshots "posts/*" "feed" >>= recentFirst
            renderRss feedConfig feedCtx posts

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

postPagesFor :: Int -> PageNumber -> [Int]
postPagesFor total page
    | total <= 6 = [1..total]
    | otherwise = leftSide ++ rightSide
    where
        leftSide = if page <= 4
            then [1..page]
            else [1, 2, -1, page - 1, page]
        rightSide = if total - page <= 3
            then [page + 1 .. total]
            else [page + 1, -1, total - 1, total]

failIf :: (Monad m) => Bool -> a -> m a
failIf True _ = fail "failWhen"
failIf False v = return v

sassCompilerWith :: SassOptions -> Compiler (Item String)
sassCompilerWith opts = getResourceBody >>= compileItem
    where
        compileItem item = flip withItemBody item $ \body -> unsafeCompiler $ do
            result <-  compileString body opts'
            case result of
                Left e -> do
                    msg <- errorMessage e
                    fail $ "Cannot parse Sass file " ++ path ++ "\n" ++ msg
                Right r ->
                    return r
            where
                path = toFilePath $ itemIdentifier item
                opts' = opts { sassInputPath = Just path }

sassCompiler :: Compiler (Item String)
sassCompiler = sassCompilerWith def { sassOutputStyle = SassStyleCompressed }

deployWithWinSCP :: IO ExitCode
deployWithWinSCP = do
    dir <- getCurrentDirectory
    let siteDir = dir ++ "\\_site"
    system $ "winscp.com /script=\"deploy.txt\" /parameter // \"" ++ siteDir ++ "\"" 

-- Copied from Hakyll sources
paginateContext :: Paginate -> PageNumber -> Context a
paginateContext pag currentPage = mconcat
    [ field "firstPageNum"    $ \_ -> otherPage 1                 >>= num
    , field "firstPageUrl"    $ \_ -> otherPage 1                 >>= url
    , field "previousPageNum" $ \_ -> otherPage (currentPage - 1) >>= num
    , field "previousPageUrl" $ \_ -> otherPage (currentPage - 1) >>= url
    , field "nextPageNum"     $ \_ -> otherPage (currentPage + 1) >>= num
    , field "nextPageUrl"     $ \_ -> otherPage (currentPage + 1) >>= url
    , field "lastPageNum"     $ \_ -> otherPage lastPage          >>= num
    , field "lastPageUrl"     $ \_ -> otherPage lastPage          >>= url
    , field "currentPageNum"  $ \i -> thisPage i                  >>= num
    , field "currentPageUrl"  $ \i -> thisPage i                  >>= url
    , constField "numPages"   $ show $ paginateNumPages pag
    ]
  where
    lastPage = paginateNumPages pag

    thisPage i = return (currentPage, itemIdentifier i)
    otherPage n
        | n == currentPage = fail $ "This is the current page: " ++ show n
        | otherwise        = case paginatePage pag n of
            Nothing -> fail $ "No such page: " ++ show n
            Just i  -> return (n, i)

    num :: (Int, Identifier) -> Compiler String
    num = return . show . fst

    url :: (Int, Identifier) -> Compiler String
    url (n, i) = getRoute i >>= \mbR -> case mbR of
        Just r  -> return $ rewriteUrl $ toUrl r
        Nothing -> fail $ "No URL for page: " ++ show n

paginatePage :: Paginate -> PageNumber -> Maybe Identifier
paginatePage pag pageNumber
    | pageNumber < 1                      = Nothing
    | pageNumber > (paginateNumPages pag) = Nothing
    | otherwise                           = Just $ paginateMakeId pag pageNumber

paginateNumPages :: Paginate -> Int
paginateNumPages = M.size . paginateMap
