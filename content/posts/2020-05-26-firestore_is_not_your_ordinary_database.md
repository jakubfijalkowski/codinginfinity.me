+++
title = "Firestore is not your ordinary database, so don't treat it like one"
date = 2020-05-26T12:00:00+02:00
[taxonomies]
tags = ["mobile", "google", "rants"]
+++

Firestore, being part of Firebase suite and having its roots in Firebase Realtime Database, is another offering from Google (they acquired Firebase in 2014) that wants to ease app development. It is a document database, but this one prides itself in being for backend as well as mobile & web development. Integration should be seamless, everything should work out of the box and all the problems with "ordinary" databases will be solved for you. :)

This won't be another post that glorifies Firestore. I worked with it for quite some time now. I've seen it being used in other projects as well. In all cases it was more of a burden than a relief, at least in the beginning. I *think* I know why, but let's dig into it a little bit, shall we?

<!-- more -->

## What Firestore is, really?

Firestore isn't yet another NoSQL database on the market. It is being advertised as **the** database for mobile and client apps. It differs from other solutions in that it is meant to be accessed *directly* from the app (or web, but I will ignore the "web" part from now on as that doesn't make any difference), which isn't a common approach. It isn't [that new](https://realm.io/) but on the other hand it doesn't have much competition in this niche market.

Firestore focuses on mobile. It thrives there. It tries to give you everything that you possibly need, from authorization (combined with Firebase Authentication of course), through synchronization (this is quite novel), querying (as all good databases do), streaming (because *realtime*) and whatnot. It transforms the problems that were non-trivial before into a slight inconvenience. In theory. In practice, everything there is *great* as long as you are using simple queries, with simple streaming and trivial auth.

So, let's dive in!

## The good

First and foremost, Firestore is very easy to use. In no time you can have your data stored and you can ***share it*** with others in **near real-time**. This isn't simple to achieve using other means, especially when you're developing the solution yourself. Handling all the WebSockets stuff, lost connections, syncs, conflicts and everything else is plain hard, and with Firestore you have that *almost* for free.

It also integrates really well with other Firebase services. Need some simple authentication? You've got Firebase Auth for that. Push messages? FCM. Analytics? Well... Google Analytics for Firebase got you covered. Serverless? Cloud Functions (although I've got some mixed feelings about this one).

I'm not a mobile developer but from what I've been told they have terrific mobile SDKs. Everything "just" works and feels right. You just plug it in and all of the Firebase suite is at your disposal. That's a tremendous advantage considering how fragile "big" SDKs can be (all SDKs, not only mobile SDK ;) ).

Firestore is advertised as a great technology and, to some extent, it is. It helps you create the "wow effect", but everything has its price and it isn't that small here.

## The bad & the ugly

### Security & data validation

Being able to access the data directly from end-user devices creates some non-trivial problems. Normally, your backend system would act as an intermediary but here there isn't one. Everything that would normally be done there is now the responsibility of Firestore.

And this is a bad idea.

First and foremost, backend systems do validation & authorization. Most of the time those are separate concerns. Firestore, on the other hand, treats them as equal and presents you with the same solution to both. You basically have to write authorization rules that sometimes do validation. You have to do that in a custom language that somewhat resembles JavaScript but isn't JS. Normally I would say that using a custom language is a good idea. But not here. These things are too complex to be handled with a language that is able to do only simple comparisons, retrieve related documents by id or do a simple query. You can't really express anything meaningful and stay sane. Adding validation to the mix doesn't really help (so most of the time there is no validation at all).

Of course, there are cases when that is not a problem. For example, you can "easily" shield yourself from bad actors by just ignoring/sanitizing malformed data on the client. There are cases when you don't really need any authorization besides simple "these users can only write these documents and read from those ones". I would even say that most projects fall under these cases in the beginning. Then the complexity creeps in and you find yourself deep in the broken Firestore rules, with validation code everywhere in your app. And no security.

### Pricing, usage calculations & indices

Uh oh. I think this is the thing that made me (well... my client) scream in agony. [Firestore pricing](https://cloud.google.com/firestore/pricing) looks normal - you pay for ingress/egress, storage & operations. That's understandable. What makes it hard is expense monitoring. Or to be more specific - lack of it.

Firestore doesn't really give you a way to check how much you use it. You can see how much ops you've already used, but when it comes to storage and ingress/egress, you're pretty much left to yourself. Firestore doesn't give you anything meaningful there. All you have is a single "storage used" on your GCP bill. It doesn't tell you how much data you really have or how much new data you're creating, it only tells you how much they've billed you. Nothing else. You can try to derive the changes from it, but that won't be nowhere near "accurate".

Theoretically, [documentation tells](https://firebase.google.com/docs/firestore/storage-size#index-entry-size) you how to calculate the storage that you use (or will use). You can calculate everything yourself but that requires you to **download every single document** in the database or do the calculation up-front. It's also painfully complicated (for such a simply stated problem), terribly slow and will cost you money just to calculate how much you will pay.

What does count under the term *storage used*? Well, everything. Documents, collections (i.e. paths, as collection isn't really a thing when we're talking about storage space), indices, you name it. You pay for every byte that you create and for every byte that Firestore creates for you. And it creates *a lot* *of* data for you.

By default, Firestore indexes all of the fields in your documents. *All of them*. You must explicitly disable indices and you can only [create 200 exemption rules](https://firebase.google.com/docs/firestore/query-data/index-overview#indexing_limits) (as of 26-05-2020). This makes it extremely important to model your data carefully because one wrong index can result in a tremendous amount of unused data. I am myself guilty of overlooking this. In one project, we generated almost 24GB of indices for every 1GB of data and we haven't used any of that.

So, when it comes to pricing, you have to be *really, really, **really*** careful, even for simple cases (it only takes one bad actor to pollute your data :) ).

### Other

There are other things that I don't like about Firestore:

1. No management-ability - you need a real person to enable Firestore. You can't script that,
2. .NET Libraries are... mediocre; Node.js libs aren't that good either,
3. You have *eventual consistency* right from the start and there is nothing you can do about it,
4. You start with like 100 or 200ms of latency for every request.

I think it got enough bashing for a single post so I'm just leaving that list as-is for now. If you want more then leave a comment!

## It all comes down to one thing

When I've started writing this post (and it was loong time ago) I haven't really thought that it would be a one big complaint. Firestore isn't inherently bad tech. I've used it successfully in a few projects and probably wouldn't change that. It is just advertised as an all-purpose database that will ease all your pains, but skips the parts about all the trade-offs that it makes. If you're not over-using it, you will be happy and it will improve your experience (even on backend :) ). It just sets so many traps for you that I think it shouldn't be your go-to solution for anything.
