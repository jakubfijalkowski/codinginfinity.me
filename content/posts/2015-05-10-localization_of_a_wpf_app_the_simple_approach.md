+++
title = "Localization of a WPF app - the simple approach"
date = 2015-05-10T22:40:00+02:00
aliases = ["post/2015-05-10/localization_of_a_wpf_app_the_simple_approach"]
[taxonomies]
tags = ["c#", "wpf"]
+++

Localization is hard, WPF makes it even harder with the [locbaml] approach. There are many other ways to solve this problem, some are basic, some are powerful, but I think that none is perfect. Here is my simple way of dealing with this.

<!-- more -->

I recently had to support multiple languages in (very simple) WPF application. The main requirement was to allow changing language at run-time, without restarting the application. There is [WPF Localization Extension] which is very powerful and quite simple, it has one flaw though - it's big! `.resx` + `{x:Static}` approach is quite simple, but it does not support dynamic changes of culture and using resource dictionaries and `DynamicResource` is painful (especially changing the language).

There is another way to solve this. It involves a little bit of coding, but it's far from being advanced.

As I said, I wanted this to be simple, both the XAML and the code. So this is my goal (it's mostly the same as with WPF Localization Extension):

```xml
<Button Content="{ns:Loc ButtonText}" />
```

And this should suffice. How to achieve this? Use good old `.resx` files and bindings!

As we all know, WPF bindings support automatic updates (with `INotifyPropertyChanged`) and are able to attach themselves to any property, indexer or even properties in nested objects. This features can be abused a little in order to serve our needs. We just have to introduce an object which will expose resources we want to use and set it to as [the Source property]. My first idea was to use `dynamic` and [DynamicObject], but this seemed to me as an overkill. Fortunately, we may just abuse the indexers. ;)

First, let's introduce the `LocExtensions`. It derives from `Binding`, so only correct `Path` and `Source` has to be set. I said that indexers will be used, therefore we have to format `Path` a little bit.

```cs
public class LocExtension : Binding
{
    public LocExtension(string name)
        : base("[" + name + "]")
    {
        this.Source = TranslationSource.Instance;
    }
}
```

`TranslationSource` is our class that wraps access to the resources (or rather, `ResourceManager`). It's a singleton because we want to synchronize access to resources across all the uses of above extension and allow to change language in only one place.

Retrieving single resource is easy - it's just a call to [ResourceManager.GetString] with name and correct culture:

```cs
private readonly ResourceManager resManager = Properties.Resources.ResourceManager;
private CultureInfo currentCulture;

public string this[string key]
{
    get { return this.resManager.GetString(key, this.CurrentCulture); }
}
```

The tricky part is changing current culture AND updating all the bindings. Happily, WPF bindings register `PropertyChanged` event if the object supports it and update themselves when it is raised (and the property name is correct). We can keep track of resources used and raise `PropertyChanged` for each one, but this would be a little bit of a pain. Fortunately, there are two special names:

 * `string.Empty`/`null` indicates that all properties has changed, and
 * `Item[]` that indicate that value of indexer has changed.

Both would serve our needs well, but the latter one would also require raising `PropertyChanged` for `CurrentCulture`. This leads to the following code:

```cs
public CultureInfo CurrentCulture
{
    get { return this.currentCulture; }
    set
    {
        if (this.currentCulture != value)
        {
            this.currentCulture = value;
            var @event = this.PropertyChanged;
            if (@event != null)
            {
                @event.Invoke(this, new PropertyChangedEventArgs(string.Empty));
            }
        }
    }
}
```

And that's it! We now can easily localize our application with using `{ns:Loc}` and resource files and change language by updating `TranslationSource.CurrentCulture`. It lacks some features (e.g. support for images) and is not that extensible, but it should get the job done quite fast. And if we decide to use [WPF Localization Extension] instead, we just have to update namespaces in XAML and everything stays the same.

Whole code used in this post is [available on GitHub].

**Update 2018-12-27**

I haven't thought that this approach would be used outside my & my friends projects. Yet the gist stars and comments tell a different story. :)

Recently, [Urban] e-mailed me that he has been trying to make this a little bit more flexible. He wanted to use multiple `ResourceManager`s. We've came up with an idea to use attached properties to anchor correct `ResourceManagers` to component roots, so we can decide (in XAML) where to get the resources from. Now go, take a look what [Urban have made]. :)

[locbaml]: https://msdn.microsoft.com/en-us/library/ms746621.aspx
[WPF Localization Extension]: https://github.com/SeriousM/WPFLocalizationExtension
[the Source property]: https://msdn.microsoft.com/en-us/library/system.windows.data.binding.source.aspx
[DynamicObject]: https://msdn.microsoft.com/en-us/library/system.dynamic.dynamicobject.aspx
[ResourceManager.GetString]: https://msdn.microsoft.com/en-us/library/bsb0cfet.aspx
[available on GitHub]: https://gist.github.com/jakubfijalkowski/0771bfbd26ce68456d3e
[Urban]: https://github.com/Jinjinov
[Urban have made]: https://github.com/Jinjinov/wpf-localization-multiple-resource-resx-one-language
