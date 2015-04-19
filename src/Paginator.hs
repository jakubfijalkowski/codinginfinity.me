module Paginator
  (
    postPagesFor
  , postsPagePath
  , paginateContext
  , paginateNumPages
  ) where

import qualified Data.Map    as M
import           Data.Monoid (mconcat)
import           Hakyll      hiding (paginateContext)
import           Rewriter

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

postsPagePath :: Int -> String
postsPagePath 1 = "index.html"
postsPagePath n = "page_" ++ (show n) ++ ".html"

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
