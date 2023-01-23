+++
title = "FAKE your way to ASP.NET Core on Azure WebApps, part 3 - The Solution, Again"
date = 2017-03-18T17:00:00+02:00
aliases = ["post/2017-03-18/fake_your_way_part3"]
[taxonomies]
tags = ["f#", "azure", ".net core"]
+++

Deploying sites to Azure should be relatively easy. And it is, provided that you deploy from Visual Studio (and this is wrong on so many levels...). But if you want to automate things, well... You're in a bad position.

I've tried to develop a solution [last time]. It works, but not always. I've finally got around to improve it and force it to work more frequently, but this time I am not going to say that it will be flawless or will **ever** be.

<!-- more -->

## The Problem

As I said in [the first post] of the series, locks are the problem. In [the second one], I stated that stopping the WebApp is not enough and it is a **must** to ensure that the app has been killed. Unfortunately, Azure does not terminate IIS immediately, it just queues the stop command. Luckily, it is not that hard to check whether it has stopped serving requests, as it's just a matter of examining whether the site responds with `403 Site disabled` status (`503`, which I've used previously, was a mistake). The point is, it **does not** mean that the underlying process (be it `dotnet.exe` or `node.exe` or anything else) has been stopped. It just means that IIS is not serving the requests anymore, the underlying process may still be running. And that's bad.

## The Solution (well... ;) )

We want it dead, so we should wait for it to be killed. Or kill it, but I consider this a last resort and will defer implementing this as far as I can.

Kudu provides [`command` endpoint] that executes arbitrary shell commands in the WebApp "container".

>
> By the way, it runs the scripts in the Bash shell, using Windows Subsystem for Linux. This is a recent change, 3 months ago or so it was using CMD.
> 
> The question is, what's next? WebApps on Linux? I hope so!
>

So, checking if the `dotnet.exe` is running is just a matter of executing `pgrep dotnet` (WSL does not have `pgrep` but let's just assume that it exists) or `Get-Process -Name 'dotnet'` and examining the exit code. Killing process? Easy - `pkill dotnet` or `Get-Process -Name 'dotnet' | Stop-Process`.

I would really love to use `pgrep` (or even `ps` and `grep` separately), but sadly WSL partly sandboxes Bash and it doesn't report Windows processes. Falling back to PowerShell or CMD is required, but it isn't that hard.

## The Code

The idea is simple. The code is rather straightforward, albeit it's a little long thanks to HTTP calls and JSON deserialization.

```fs
type CommandResponse = JsonProvider<"""{"Output":"test","Error":"test","ExitCode":0}""">

let executeCommand settings credentials cmd dir =
    let url = sprintf "https://%s.scm.azurewebsites.net/api/command" settings.WebAppName
    let content =
        JsonValue.Record
            [| "command", JsonValue.String cmd
               "dir", JsonValue.String dir |]
    let response = 
        Http.RequestString
            (url,
             httpMethod = HttpMethod.Post,
             headers =
                 [ makeBasicAuthHeader credentials
                   HttpRequestHeaders.ContentType HttpContentTypes.Json ],
             body = TextRequest (content.ToString()) )
    (CommandResponse.Parse response).ExitCode

let rec ensureDotNetIsNotRunnig settings credentials =
    let cmd = "powershell -NoProfile -Command \"Get-Process -Name 'dotnet'\""
    let response = executeCommand settings credentials cmd ""
    if response = 0 then
        System.Threading.Thread.Sleep 1000
        ensureDotNetIsNotRunnig settings credentials
```

What does the code do? It just `POST`s a JSON of the form `{"command": "(...)", "dir": "(...)"}` to the `command` endpoint (with `powershell -NoProfile -Command "Get-Process -Name 'dotnet'"` as the command) and parses the result. If the command executes successfully, it means that the `Get-Process` cmdlet has found the `dotnet` process running, otherwise (`dotnet` has stopped) it returns 1 as an exit code. Simple.

I based the code on the [previous post]. [Fake.Azure.WebApps] has a little bit saner interface (and error checking).

## Summary

It probably isn't the only nor the best option that is left, but I hope that it will finally make [Fake.Azure.WebApps] immune to the locks. It will stop working someday (executing PowerShell from Bash is fragile), but I am sure that by then this lib would not be needed anymore.

By the way - do you know of any other way for this kind of automated deployments (no Docker?)?

PS. I finally have my B.Sc. degree (yay!) and started my master's studies (yay yay!), so I _should have_ more time for my pet projects. Maybe I will start blogging (semi-)regularly again? Who knows...

[last time]: https://www.codinginfinity.me/post/2016-08-29/fake_your_way_part2 (Fake your way to ASP.NET Core on Azure WebApps, part 2 - The Solution)
[the first post]: https://www.codinginfinity.me/post/2016-08-19/fake_your_way_part1 (FAKE your way to ASP.NET Core on Azure WebApps, part 1 - The Problem)
[the second one]: https://www.codinginfinity.me/post/2016-08-29/fake_your_way_part2 (Fake your way to ASP.NET Core on Azure WebApps, part 2 - The Solution)
[previous post]: https://www.codinginfinity.me/post/2016-08-29/fake_your_way_part2 (Fake your way to ASP.NET Core on Azure WebApps, part 2 - The Solution)
[`command` endpoint]: https://github.com/projectkudu/kudu/wiki/REST-API#command (Kudu's command endpoint documentation)
[Fake.Azure.WebApps]: https://github.com/jakubfijalkowski/Fake.Azure.WebApps/ (FAKE extension described in this post)
