module Rewriter (rewriteUrl) where

import           Text.Regex.TDFA  ((=~))

postRegex :: String
postRegex = "/?posts/(.+-.+-.+)-(.+)\\.html"

postPageRegex :: String
postPageRegex = "/?page_(.+)\\.html"

staticPageRegex :: String
staticPageRegex = "/?pages/(.+)\\.html"

rewriteUrl :: String -> String
rewriteUrl "index.html"  = "/"
rewriteUrl "/index.html" = "/"
rewriteUrl s
    | [_:d:t:_] <- s =~ postRegex     = "/post/" ++ d ++ "/" ++ t
    | [_:p:_] <- s =~ postPageRegex   = "/page/" ++ p
    | [_:p:_] <- s =~ staticPageRegex = "/"      ++ p
    | otherwise = s