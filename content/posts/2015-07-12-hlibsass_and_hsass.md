+++
title = "hLibsass and hSass"
date = 2015-07-12T17:00:00+02:00
aliases = ["post/2015-07-12/hlibsass_and_hsass"]
[taxonomies]
tags = ["haskell", "sass"]
+++

I've started my journey with Haskell not so long ago, but because that was my third approach, I didn't want to do only small projects, as it would surely not motivate me enough. I was looking for some kind of project that will not be too complicated and I will be able to make open source. That's how the idea for a [hLibsass] and a [hSass] arose. My pull request to include them in Stackage has just been accepted, so I think it is high time to write about them. ;)

<!-- more -->

Basically, these libraries are wrappers over [LibSass], a C++ implementation of a Sass compiler. The former is very low-level, as it consists of FFI bindings only and has just the same feature set as a C interface of LibSass.

[hSass] is a more interesting one, as it has more code and let me really appreciate Haskell. It is just a couple of types and functions, but *I think* it covers most of the features of LibSass. You can compile strings, files, use Haskell functions in Sass code and examine all the information LibSass provides.

Quick example:

```haskell
import Control.Monad      ((>=>))
import System.Environment (getProgName, getArgs)
import Text.Sass

main :: IO ()
main = do
    progName <- getProgName
    args <- getArgs
    case args of
        (f:_) -> compile f
        _     -> putStrLn $ "Usage: " ++ progName ++ " FILE"

compile :: FilePath -> IO ()
compile file = do
    result <- compileFile file def
    either (errorMessage >=> putStrLn) putStrLn result

```

And we have a simple, standalone Sass compiler.

If you want to play with these libraries, go ahead and `cabal install` or `stack install` them (next [Stackage] nightly build should have them). If you want to use them with GHCi you must compile hLibsass with a shared version of LibSass (there is some sort of a bug in GHC's runtime system that prevents hLibsass to correctly load, but I have not been able to dig into it deep enough yet) - just use the flag `sharedlibsass` (as described in a [README]).

GitHub: [hLibsass], [hSass]<br>
Hackage: [hLibsass](https://hackage.haskell.org/package/hlibsass), [hSass](https://hackage.haskell.org/package/hsass)

[hLibsass]: https://github.com/jakubfijalkowski/hlibsass
[hSass]: https://github.com/jakubfijalkowski/hsass
[LibSass]: https://github.com/sass/libsass/
[Stackage]: https://www.stackage.org/
[README]: https://github.com/jakubfijalkowski/hlibsass/blob/master/README.md
