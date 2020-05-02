+++
title = "FAKE your way to ASP.NET Core on Azure WebApps, part 1 - The Problem"
date = 2016-08-19T20:00:00+02:00
aliases = ["post/2016-08-19/fake_your_way_part1"]
[taxonomies]
tags = ["f#", "azure", ".net core"]
+++

A year has passed and times have changed, but my love for programming will always be the same... OK, enough. That opening didn't quite work out, right? ;)

Recently, I've been extensively working with .NET Core apps (YES! I can finally ditch Visual Studio!). I'm used to working with technologies that have unstable tooling (Hey! I'm talking to you, [\@drunkvs]!), so it was rather pleasant experience. However, there is one question that I couldn't find an answer to - how to automatically deploy these things into the wild?

<!-- more -->

## The problem

Let's say that we have the following problem - we need to deploy ASP.NET Core app. It needs to be automatic (deploy after successful build) and reliable. The app is rather small, it doesn't have many additional dependencies (maybe a database, but that's another topic) and we really don't want to use sophisticated tools that require non-trivial knowledge (and infrastructure).

Given these requirements, my lack of knowledge about real-world deployments and time constraints, I've decided to go with TeamCity for continuous builds, Azure WebApps for hosting and my beloved [FAKE] to make these things work together. ;)

## The solution

Deploying .NET Core apps on Azure App Services (the base for WebApps if I get it correctly) is relatively easy. Write it, test it, `dotnet publish` it (with IIS integration tools installed), copy it to the WebSite and you're done. Deploy from source? You've got Kudu.

But Kudu (and *deploy from Git*) isn't a replacement for proper, automated deployments. What if the build process isn't a simple `dotnet build`? What if you need to do something else besides build (e.g. update configuration)? You can script Kudu, but that really isn't a flexible way to do things.

I haven't found any article that would cover this problem the way I could be satisfied with, so the only thing left is a custom solution. Fortunately, I've been playing with F#, FAKE and TeamCity recently and they seem to be a good choice for this.

So, how will this work? Well, simple - TeamCity will run the FAKE build script that will:

 1. Build, test and `publish` the project,
 2. ZIP the published folder,
 3. Upload the archive to Kudu's ZIP Controller (or just upload files with FTP).

Easy-peasy. However, there is this little *but* that makes that a little bit harder - locks.

## The real problem

Everything boils down to one very *tiny* thing - Windows - and the fact it holds locks on executable files. The .NET Core App is run by the `dotnet` CLI that loads its DLLs and the CLI is run by IIS. IIS has a lock on the `dotnet.exe` (that isn't a problem - it won't be updated) and it has a lock on the application DLL file that also may hold locks on some dependencies. In other words - we're screwed.

Of course there is the possibility that app won't be running at all (IIS may kill the app pool if it isn't used, however I assume that's not the case), but we need to be **sure** that it is shut down and our files aren't locked.

Luckily, we can control Azure using an API, but that has its own problems (although much less severe) and because this post is getting long enough, the battle with Azure and .NET Core will be continued in the next episode. ;)

[\@drunkvs]: https://twitter.com/drunkvs
[FAKE]: https://fsharp.github.io/FAKE/
