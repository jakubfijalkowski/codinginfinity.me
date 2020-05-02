+++
title = "FAKE your way to ASP.NET Core on Azure WebApps, part 2 - The Solution"
date = 2016-08-29T23:30:00+02:00
aliases = ["post/2016-08-29/fake_your_way_part2"]
[taxonomies]
tags = ["f#", "azure", ".net core"]
+++

[Last time] I talked about an easy problem of deploying .NET Core programs to Azure WebApps (or, well, any other app that can run there). As it turned out, it isn't that easy, but let's face the truth - there are far more complicated things in IT and we shouldn't spend too much time talking about simple deployments. So let's get started!

<!-- more -->

## The plan of attack

As I stated in my previous post, everything boils down to ensuring that the WebApp is stopped and then sending the app packed as a ZIP to Kudu. It should be easy... Except it's not. At least not THAT easy. The way you access WebApps (using [Azure Resource Manager]) and Kudu is rather different. ARM is quite complicated (especially when accessing it from unattended machines). And ensuring that the app is stopped is painful. Only Kudu endpoints are sort of easy to work with (albeit getting access credentials is not) and has fairly [decent documentation].

To sum up, we have to:

 1. Build, publish and pack the app,
 2. Stop the WebApp,
 3. Upload the ZIP using Kudu,
 4. Restart the app.

Let's tackle them one-by-one.

## Step one - build & pack

This is the easy one - we have whole [FAKE] at our disposal. It has really friendly API with helpers for .NET Core CLI. I assume we have the basic building script working (if not - head to [the tutorial]) and a .NET Core project bootstrapped in the `src` directory (`dotnet new -t web` suffices ;) ). So, the only problem left is to create required `Target`s (and maybe some configuration entries).

First, define some constants:

```fsharp
let baseDir = __SOURCE_DIRECTORY__
let sourceDir = baseDir @@ "src"
let projectDir = sourceDir @@ ProjectName |> FullName
let deployDir = baseDir @@ "deploy" |> FullName
let deployZip = baseDir @@ "deploy.zip" |> FullName
```

Next, we need to create our targets. What do we want them to do? `Clean`, `Restore`, `Build` and `Publish` the app. Easy, right? Of course it is! ;)

```fsharp
Target "Clean" (fun () ->
    !! (projectDir @@ "bin")
    ++ (projectDir @@ "obj")
    ++ deployDir
    |> CleanDirs

    // Clean node_modules and bower files too?

    DeleteFile deployZip
)

Target "Restore" (fun () ->
    // If we have multiple projects, this command will restore all of them
    DotNetCli.Restore (fun c -> { c with WorkingDir = baseDir })
)

Target "Build" (fun () ->
    DotNetCli.Build (fun c -> { c with Configuration = currentConfig }) [projectDir]
)

Target "Publish" (fun () ->
    // Unfortunately the DotNetCli helper does not wrap `publish` command - I need to change this!
    DotNetCli.RunCommand id (sprintf "publish \"%s\" --configuration %s --output \"%s\"" projectDir currentConfig deployDir)
    !! (deployDir @@ "**") |> Zip deployDir deployZip
)
```

Now we have the app freshly built, published (with the `web.config` for IIS) and zipped!

## Step two point one - sign in to ARM

OK, we have our app prepared, how do we stop the WebApp then? As I stated earlier, [Azure Resource Manager] allows us to manage the app state. To use it, we need to get the required credentials first. It's not that complicated but really boring, so I'll just [link to the docs].

Having the credentials (tenant id, client id and client secret), we can issue the request for Access Token (ARM uses OAuth2 for authentication). It's just a simple `client_credentials` flow, so a single `GET` request is enough. I'm using `HTTP Utilities` and the magnificent `JsonProvider` from [FSharp.Data] to simplify the process:

```fsharp
// Let's wrap the credentials in a record
type AzureWebAppSettings = {
    TenantId       : string
    ClientId       : string
    ClientSecret   : string
}

type AzureTokenResponse = JsonProvider<"""{"token_type":"","expires_in":"","ext_expires_in":"","expires_on":"","not_before":"","resource":"","access_token":""}""">

let acquireAccessToken settings =
    let url = sprintf "https://login.microsoftonline.com/%s/oauth2/token" settings.TenantId
    let response =
        Http.RequestString (url, body = FormValues
            [ "resource",      "https://management.azure.com/";
              "grant_type",    "client_credentials";
              "client_id",     settings.ClientId;
              "client_secret", settings.ClientSecret ])
    let json = response |> AzureTokenResponse.Parse
    json.AccessToken.JsonValue.AsString ()
```

## Step two point two - stop the WebApp

Apart from the previously specified credentials, the ARM needs the subscription id, the resource group and the app name (for a total of 6 configuration parameters...). After providing them, we can finally tell the Azure to stop the app! But how to do that? Don't try to find it on MSDN - it's simply not there (or at least I couldn't find it). You can read how to do it with [Azure CLI] and [PowerShell], but the raw endpoint is not documented. Fortunately, it's rather easy to get it from Google (or extract it from [the source of Azure PowerShell]). It's just a `POST` to `stop` using the `Microsoft.Web` provider:

```fsharp
// Extend the settings with the new ones
type AzureWebAppSettings = {
    TenantId       : string
    ClientId       : string
    ClientSecret   : string
    SubscriptionId : string
    ResourceGroup  : string
    WebAppName     : string
}

let stopWebApp settings accessToken =
    let url = sprintf "https://management.azure.com/subscriptions/%s/resourcegroups/%s/providers/Microsoft.Web/sites/%s/stop?api-version=2016-03-01" settings.SubscriptionId settings.ResourceGroup settings.WebAppName
    Http.RequestString
        (url,
         httpMethod = HttpMethod.Post,
         headers = [HttpRequestHeaders.Authorization ("Bearer " + accessToken)],
         body = BinaryUpload [||] // This is needed, as Azure requires Content-Length header in POST requests
         )
```

And voilà! The app will be stopped! That's right, it **WILL** be stopped. Some time in the future. Maybe now. But maybe not. If you ask the ARM, it'll tell you that it really **IS** stopped, but if you call the website, you'll get `200 OK`. I know why this is the way it is, but Azure should provide some way of ensuring that the action has really taken place (please, tell me I am wrong and it's just my unawareness).

I've tried a few distinct approaches on how to ensure the app is stopped (even tried `Thread.Sleep` with a really long time-out), but none of them is as clean as I would want to. The easiest and most reliable one is probably requesting the website until `503 Service Unavailable` is returned (which IIS returns when the app pool is stopped and `HEAD` method is used):

```fsharp
let rec ensureWebAppIsStopped settings =
    let url = sprintf "https://%s.azurewebsites.net" settings.WebAppName
    let response = Http.Request(url, httpMethod = HttpMethod.Head, silentHttpErrors = true)
    if response.StatusCode <> 503 then
        System.Threading.Thread.Sleep 1000 // Reduce the amount of calls
        ensureWebAppIsStopped settings
```

Then it should really be dead, `dotnet.exe` should be killed and `w3wp.exe` that serves the requests should be gone.

## Step three point one - get Kudu credentials

Kudu uses a [totally different set of credentials]. We could provide them together with the tenant id/client id/..., but 8 configuration entries is too much for me (hell, 6 is too much, but I have no idea how to reduce it). Fortunately, we can extract them from the publish settings of the WebApp. They are easily accessible using ARM - we just need to `GET` `publishxml`, parse it and extract FTP credentials:

```fsharp
type AzurePublishXmlResponse = XmlProvider<"""<publishData><publishProfile publishMethod="" userName="" userPWD="" /><publishProfile publishMethod="" userName="" userPWD="" /></publishData>"""> // We don't need more properties

let getKuduCredentials settings accessToken =
    let url = sprintf "https://management.azure.com/subscriptions/%s/resourcegroups/%s/providers/Microsoft.Web/sites/%s/publishxml?api-version=2016-03-01" settings.SubscriptionId settings.ResourceGroup settings.WebAppName
    let response =
        Http.RequestString
            (url,
             httpMethod = HttpMethod.Get,
             headers = [HttpRequestHeaders.Authorization ("Bearer " + accessToken)]
             )
        |> AzurePublishXmlResponse.Parse
    let pubMethod = response.PublishProfiles |> Array.find (fun t -> t.PublishMethod = "FTP")
    let idx = pubMethod.UserName.IndexOf '\\' // Kudu doesn't like the app specification
    (pubMethod.UserName.Substring(idx + 1), pubMethod.UserPwd)
```

Rather easy, especially with `XmlProvider`, isn't it?

## Step three point two - upload the ZIP

Okay, we now have everything that is required to upload the ZIP package we have prepared earlier. The app is surely stopped, we have Kudu's credentials, we only need to call the [ZIP Controller]:

```fsharp
let makeBasicAuthHeader (username, password) =
    sprintf "%s:%s" username password
    |> Encoding.UTF8.GetBytes
    |> Convert.ToBase64String
    |> (+) "Basic "
    |> HttpRequestHeaders.Authorization

let pushZipFile settings credentials =
    let url = sprintf "https://%s.scm.azurewebsites.net/api/zip/site/wwwroot" settings.WebAppName
    let content = File.ReadAllBytes deployZip
    Http.Request
        (url,
         httpMethod = HttpMethod.Put,
         headers = [makeBasicAuthHeader credentials],
         body = BinaryUpload content) |> ignore
```

BAM! The app have just got deployed!

## Step four - restart the app

Knowing how to stop the app, starting it again is really just a matter of changing `stop` to `start` and I'll leave it as an exercise. ;)

Now we can wrap it in a target and configure dependencies:

```fsharp
Target "Upload" (fun () ->
    let token = acquireAccessToken armSettings
    let credentials = getKuduCredentials armSettings token

    stopWebApp armSettings token
    ensureWebAppIsStopped armSettings
    pushZipFile armSettings credentials
    startWebApp armSettings token
)

"Clean" ==> "Restore" ==> "Build" ==> "Publish" ==> "Upload"
RunTargetOrDefault "Build"
```

## Summary

And this is it! Simple, reliable (well... probably - I need to test it on a living system) and rather clean deployments of .NET Core websites to Azure WebApps. No doubt there are better ways of doing this ([Octopus Deploy]?), but if we don't have required infrastructure and the app is uncomplicated - why investing time and money? Simple scripts FTW!

The source code, cleaned and packed as a [FAKE] helper, is available on my [GitHub] and as the [NuGet package]. Hope you'll like it!


[Last time]: https://www.codinginfinity.me/post/2016-08-19/fake_your_way_part1 (Fake your way to ASP.NET Core on Azure WebApps, part 1 - The Problem)
[Azure Resource Manager]: https://azure.microsoft.com/en-us/documentation/articles/resource-group-overview/ "Azure Resource Manager"
[decent documentation]: https://github.com/projectkudu/kudu/wiki/REST-API "Kudu's documentation"
[FAKE]: https://fsharp.github.io/FAKE/ "F# Make"
[the tutorial]: https://fsharp.github.io/FAKE/gettingstarted.html "FAKE tutorial"
[link to the docs]: https://azure.microsoft.com/en-us/documentation/articles/resource-manager-api-authentication/ "ARM API Authentication"
[FSharp.Data]: https://fsharp.github.io/FSharp.Data/ "FSharp.Data"
[Azure CLI]: https://docs.microsoft.com/en-us/cli/azure/get-started-with-azure-cli "Azure CLI docs"
[PowerShell]: https://docs.microsoft.com/en-us/powershell/ "Azure PowerShell docs"
[the source of Azure PowerShell]: https://github.com/Azure/azure-powershell "Source code of Azure PowerShell"
[totally different set of credentials]: https://github.com/projectkudu/kudu/wiki/Deployment-credentials "Kudu's deployment credentials"
[ZIP Controller]: https://github.com/projectkudu/kudu/wiki/REST-API#zip
[Octopus Deploy]: https://octopus.com
[GitHub]: https://github.com/jakubfijalkowski/Fake.Azure.WebApps "source code"
[NuGet package]: https://www.nuget.org/packages/Fake.Azure.WebApps
