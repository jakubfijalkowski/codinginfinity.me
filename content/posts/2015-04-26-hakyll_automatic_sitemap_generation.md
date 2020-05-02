+++
title = "Hakyll - automatic sitemap generation "
date = 2015-04-26T14:30:00+02:00
aliases = ["post/2015-04-26/hakyll_automatic_sitemap_generation"]
[taxonomies]
tags = ["haskell", "hakyll"]
+++

Search engines like sitemaps. Hakyll doesn't have this ability out of the box, but it is quite trivial to add. Here is [my approach].

<!-- more -->

The [Sitemaps] protocol is an easy one. Just create a XML file with a bunch of elements, put it on a website and tell search engines ([Bing], [Google]) that it exists. Here is a sample file, used on this site:

```xml
<?xml version='1.0' ?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
  <!--...-->
  <url>
    <loc>http://codinginfinity.me/about_me</loc>
    <lastmod>2015-04-18T20:58:08Z</lastmod>
    <changefreq>weekly</changefreq>
    <priority>0.5</priority>
  </url>
  <!--...-->
</urlset>
```

Most of blog engines have it built-in. Hakyll does not, which isn't great, but because modifying a build process of a site is fairly easy, we can generate `sitemap.xml` ourselves.

### Configuration

Let's start with two types.

```haskell
data SitemapConfiguration = SitemapConfiguration {
    sitemapExtensions :: [String]                    -- defaults to [".html"]
  , sitemapChangeFreq :: FilePath -> ChangeFrequency -- defaults to const Weekly
  , sitemapPriority   :: FilePath -> Double          -- defaults to const 0.5
  , sitemapBase       :: String
  , sitemapRewriter   :: FilePath -> FilePath        -- defaults to ('/':)
}

data ChangeFrequency = Always | Hourly | Daily | Weekly | Monthly | Yearly | Never
```

My aim is to make it a little bit elastic, so that everyone could use it on their site with simple copy-and-paste approach. Exposing `SitemapConfiguration` allows us to separate configuration from logic and do not hardcode every decision.

So what does this type control? `sitemapExtension` is used as a filter, specifying which files should be included (extension must be in this list), `sitemapChangeFreq` and `sitemapPriority` allow to control `changefreq` and `priority` fields respectively, `sitemapBase` is prepended to every `loc` and `sitemapRewriter` controls rewriting of URLs if we want to have nice links on our website (e.g. without extension, as I have).

### Generate XML

Although the format is very simple, I didn't want to generate XML files by hand. I used [xml package], as I [found] it easy to use and not full of needless (for this task) features. Unluckily, it does not have a nice way to build XML documents, but we may create a small DSL for it. Two simple functions should suffice for our purposes - one will generate element with text as a content (`elementString`), and one with children (`element`). They are obvious to write, so I'll [skip them for brevity].

With these, creating our document is as easy as making a couple of [one/two-liners] for each element:

```haskell
xmlUrlSet config = add_attr xmlns . element "urlset" . map (xmlUrl config)
    where xmlns = Attr (unqual "xmlns") "http://www.sitemaps.org/schemas/sitemap/0.9"
xmlUrl config r = element "url" [f config r | f <- sub]
    where sub = [xmlLoc, xmlLastMod, xmlChangeFreq, xmlPriority]

-- snip
```

### Putting it all together

Now, we just have to list all pages, get modification time, filter them, generate XML and save the final sitemap to a file. 'Listing pages' and 'saving to file' is Hakyll's business, so we'll just use `Compiler` monad and (over)use its capabilities.

Listing all pages means getting all `Identifier`s. We have `getMatches` (from `MonadMetadata`) and it supports recursive glob patterns, so `getMatches "**"` will return every file that Hakyll knows of.

```haskell
ids <- getMatches "**"
```

Next, we have to get route (destination path) and its modification time. Retrieving route is easy, but modification time is not trivial to extract from Hakyll - `resourceModificationTime` from `Hakyll.Core.Provider.Internal` is, well, internal. We could access it with `modificationTimeField` from `Hakyll.Web.Template.Context`, but this will require some gruesome code. I decided to go a different way and use modification time of source file, using *normal* Haskell function - `getModificationTime`. We need to convert Hakyll's `Identifier` to `FilePath` and handle errors, but [the code is simple].

```haskell
mapM getRouteWithModTime ids -- We'll use this later
-- ...
getRouteWithModTime i = do
    mtime <- itemModTime i
    rt <- getRoute i
    return $ maybe Nothing (Just.(,mtime)) rt
```

Now we can filter out unnecessary pages. Some won't have route (e.g. templates), some won't be needed (e.g. images, CSS). This can be achieved with this simple lines:

```haskell
urls <- filter extFilter . catMaybes <$> mapM getRouteWithModTime ids
-- ...
extFilter = flip elem (sitemapExtensions config) . takeExtensions
```

And now we can generate XML document and pretty print it as an `Item`:

```haskell
let urlset = xmlUrlSet config urls
makeItem $ ppcTopElement prettyConfigPP urlset
```

### Summary

To sum up, even though Hakyll lacks this feature (and some other ;) ), it is quite easy (and pleasant) to extend, thanks to Haskell terseness and simplicity of Hakyll itself. Generating sitemap is an easy task, but I think that even more complex won't require much code.

Entire source code used in this post is available on [GitHub] as part of my site. It should be ready to copy-and-paste, if you want to use it. ;)

[my approach]: https://github.com/jakubfijalkowski/codinginfinity/blob/cafb0a0254e2b6a488cc703f83a74846e3187e0b/src/Sitemap.hs
[sitemaps]: https://www.sitemaps.org/
[Bing]: https://www.bing.com/webmaster "Bing Webmaster Tools"
[Google]: https://www.google.com/webmasters/tools/ "Google Webmaster Tools"
[xml package]: https://hackage.haskell.org/package/xml
[found]: https://stackoverflow.com/questions/1361307/which-haskell-xml-library-to-use
[skip them for brevity]: https://github.com/jakubfijalkowski/codinginfinity/blob/cafb0a0254e2b6a488cc703f83a74846e3187e0b/src/Sitemap.hs#L79-L93
[one/two-liners]: https://github.com/jakubfijalkowski/codinginfinity/blob/cafb0a0254e2b6a488cc703f83a74846e3187e0b/src/Sitemap.hs#L95-L114
[the code is simple]: https://github.com/jakubfijalkowski/codinginfinity/blob/cafb0a0254e2b6a488cc703f83a74846e3187e0b/src/Sitemap.hs#L68-L77
[GitHub]: https://github.com/jakubfijalkowski/codinginfinity/blob/cafb0a0254e2b6a488cc703f83a74846e3187e0b/src/Sitemap.hs
