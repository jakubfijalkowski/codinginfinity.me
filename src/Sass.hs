module Sass where

import           Hakyll
import           Text.Sass

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
