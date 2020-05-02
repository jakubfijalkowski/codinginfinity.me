+++
title = "Haskell and external C++ library - the easy way"
date = 2015-04-18T23:01:00+02:00
aliases = ["post/2015-04-18/haskell_and_cpp"]
[taxonomies]
tags = ["haskell", "c++"]
+++

Combining Haskell and a well-known C library is easy. `apt-get/yum install`, link with it, create bindings and we're done. Problems start to emerge when we don't want to, or can't, install the library globally. Things get even worse, when the library uses C++. I faced this problem when writing [hlibsass] and I *think* that I've managed to solve it in a not-so-terrible way. 

<!-- more -->

### Background

Let's say that we have a C++ library. There is a problem here - GHC does not support interfacing with it directly. There are [tricks to achieve this], but I'm not going that crazy path. As it is C++, we may be able to write (or we already have) a C wrapper over the library which will allow us to use it from Haskell without all (or almost all) that hassle.

There is another problem here which, I think, is the main one - how to bundle the library with a package (as we excluded the possibility of relying on the library being installed in the system). Cabal does have a system to compile C sources during build phase, but it only works for small libraries with several files and simple build process and this makes it rather useless.

There is also another possibility - build the library separately and link against the resulting file. This is the way that I've used with [hlibsass] and it worked (at least for me ;) ). So what do we need to do? Let's see!

### Building

I assume that we're working with [this project] - we have simple C++ library with C wrapper and a Haskell library with tests. We don't have bindings yet, as we don't have a way to link the Haskell code and external library together. First, we need to `make` it, but Cabal does not provide straightforward way to do it, but we may adjust the Cabal as we like using [hooks].

The idea is simple - call `make` in a `preConf` hook. We can't do this on `preBuild` because Cabal will scream *missing C library* on `configure`. Sadly, we won't be able to respect the `-j` switch, as this information is not available anywhere outside the build phase, but we have no other choice.

So, let's write our simple hook that just calls `make` with `rawSystemExit`:

```haskell
makeExtLib :: Args -> ConfigFlags -> IO HookedBuildInfo
makeExtLib _ flags = do
    let verbosity = fromFlag $ configVerbosity flags
    rawSystemExit verbosity "env"
        ["make", "--directory=ext_lib"]
    return emptyHookedBuildInfo
```

And hook it up:

```haskell
main = defaultMainWithHooks simpleUserHooks
  {
    preConf = makeExtLib
  }
```

### Linking 

We have the library built. Now we need to change `build-type` to *Custom*, add `ext` to `extra-libraries` and specify `extra-lib-dirs` (pointing at `ext_lib/lib`).

We use C++, so we have to link against C++ standard library. We (probably) use G++, so we link against `libstdc++`, but using different implementation should not be a problem. I assume that everyone has it installed globally, so simply adding `stdc++` to `extra-libraries` will suffice. It needs to be added at the end, as the order matters.

#### Fighting relative extra library path

Unfortunately, `ghc-pkg` will not be happy now. `ext_lib/lib` is relative path and although everything works, we have an awful warning. What to do? Use hooks!

This time, we need to modify `LocalBuildInfo` and add absolute path to `extra-lib-dirs` programmatically. This process is not so pleasant, as it's mostly accessing properties, but the code isn't complicated:

```haskell
updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs localBuildInfo = do
    let packageDescription = localPkgDescr localBuildInfo
        lib = fromJust $ library packageDescription
        libBuild = libBuildInfo lib
    dir <- getCurrentDirectory
    return localBuildInfo {
        localPkgDescr = packageDescription {
            library = Just $ lib {
                libBuildInfo = libBuild {
                    extraLibDirs = (dir ++ "/ext_lib/lib") :
                        extraLibDirs libBuild
                }
            }
        }
    }
```

Also, specifying hook involves a little more work because we need to run the default configure before our hook:

```haskell
main = defaultMainWithHooks simpleUserHooks
  {
    preConf = makeExtLib
  , confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraLibDirs
  }
```

### Distributing the library

There is one more thing to do - in order to allow other packages to link against your library, you have to copy the `.a` file to the installation path. This isn't great, as you may have different versions of the same file flying around, but we don't have a choice. How to do it, you may ask? Hooks, of course. ;) This time - `postCopy`.

```haskell
copyExtLib :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyExtLib _ flags pkg_descr lbi = do
    let libPref = libdir . absoluteInstallDirs pkg_descr lbi
                . fromFlag . copyDest
                $ flags
    let verbosity = fromFlag $ copyVerbosity flags
    rawSystemExit verbosity "cp" ["ext_lib/lib/libext.a", libPref]
```

This code extracts desired installation path (`~/.cabal/lib/...`, `.cabal-sandbox` or somewhere globally) and just calls `cp` to copy the file. Hooking is easy - just specify `postCopy`.

You must also bundle the Makefile and sources with your package (if you want to put it on Hackage), but this requires only updating `extra-source-files` in .cabal file.

### Cleaning

One little addition may be made - we have to `make clean` when we `cabal clean`. The code is analogous to the first one presented here, so I leave any modifications necessary [as an exercise].

### Summary

So, now we have a simple cabal package that may be easily `sdist`ributed and built on any machine without requiring manual installation of an additional library. This may not be the best option (especially flying `.a` file) for every project, but for my [hlibsass] and [hsass], it was the only sensible.

### TL;DR

Use hooks and distribute library with your package - the code is on [GitHub], so you can see it right away.

[hlibsass]: https://github.com/jakubfijalkowski/hlibsass "hlibsass"
[tricks to achieve this]: https://wiki.haskell.org/CPlusPlus_from_Haskell "C++ from Haskell"
[this project]: https://github.com/jakubfijalkowski/haskell-and-cpp
[GitHub]: https://github.com/jakubfijalkowski/haskell-and-cpp "Haskell and C++ on GitHub"
[hooks]: https://hackage.haskell.org/package/Cabal-1.22.2.0/docs/Distribution-Simple.html#t:UserHooks "Cabal hooks"
[as an exercise]: https://github.com/jakubfijalkowski/haskell-and-cpp/blob/master/Setup.hs#L49-L52
[hsass]: https://github.com/jakubfijalkowski/hsass "hsass"
