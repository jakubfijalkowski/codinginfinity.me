+++
title = "Reverse-proxy yourself to localhost with SSL/TLS"
date = 2019-01-04T17:50:00+01:00
aliases = ["post/2019-01-04/reverse_proxy_yourself_to_localhost_with_ssltls"]
[taxonomies]
tags = ["docker", "security", "tools"]
+++

Some time ago [Scott Hanselman](https://www.hanselman.com/blog/DevelopingLocallyWithASPNETCoreUnderHTTPSSSLAndSelfSignedCerts.aspx) described how to setup self-signed certificates for `localhost` using `dotnet dev-certs`. Having SSL on `localhost` is, for me, a must-have since we all want to have our dev env resemble production as much as possible. The approach Scott showed is great but it might be a little bit hard to use on Linux. On Linux-based systems there are multiple libraries, multiple (probably embedded) stores and hundreds of options to configure all of this. I'll show you an another approach that will allow to develop apps locally with full SSL/TLS and _nice_ addresses.

<!-- more -->

### You own a domain

The idea is that DNS servers allow any valid IPv4/6 address in `A`/`AAAA` records. So who will prevent us from putting `127.0.0.1`/`::1` there? :) When you own a domain and control DNS servers (zones), you can point one of the sub-domains to `localhost`. Besides `codinginfinity.me`, I own `codinginfinity.xyz` and I did exactly that - set `A` and `AAAA` records of `local.codinginfinity.xyz` and `*.local.codinginfinity.xyz` to `127.0.0.1`/`::1`:

```sh
$ drill local.codinginfinity.xyz ANY
;; ->>HEADER<<- opcode: QUERY, rcode: NOERROR, id: 40407
;; flags: qr tc rd ra ; QUERY: 1, ANSWER: 2, AUTHORITY: 0, ADDITIONAL: 0
;; QUESTION SECTION:
;; local.codinginfinity.xyz.    IN  ANY

;; ANSWER SECTION:
local.codinginfinity.xyz.   3479    IN  A   127.0.0.1
local.codinginfinity.xyz.   3479    IN  AAAA    ::1
# Cut...
$ drill *.local.codinginfinity.xyz ANY
;; ->>HEADER<<- opcode: QUERY, rcode: NOERROR, id: 62844
;; flags: qr tc rd ra ; QUERY: 1, ANSWER: 2, AUTHORITY: 0, ADDITIONAL: 0
;; QUESTION SECTION:
;; *.local.codinginfinity.xyz.  IN  ANY

;; ANSWER SECTION:
*.local.codinginfinity.xyz. 3598    IN  A   127.0.0.1
*.local.codinginfinity.xyz. 3598    IN  AAAA    ::1
# Cut...
```

From now on instead of going to `localhost:8080`, I (and you too!) can use these addresses.

### Add reverse proxy to the mix

It is rarely the case that a project consists of a single application. With the raise of containers, we tend to split the projects into multiple semi-independent apps (I won't call them microservices ;) ) that are developed by different people. Let's assume that we have a simple backend (that returns the string `backend` when requesting `/`) with even simpler frontend that just returns `frontend`. We can run them directly on our PC and use different ports but why not use containers? That way we can bind them to the same port (because of different net namespaces) and give ourselves some flexibility (combined with ease of configuration and better resemblance of production environment).

We still have one minor problem - how to access the containers? Let's run another container! It will bind to port 80 (and 443) on _real_ `localhost` and it will be responsible for routing the _external_ (i.e. requests from outside of the Docker network) traffic to corresponding containers as it will have access to both the host and the overlay network. It might do this based on the `Host` header (that's why I've added `A` record for `*.local`) or anything you want, really. `Host` seems to be the easiest method because of the awesome projects like [jwilder/nginx-proxy](https://github.com/jwilder/nginx-proxy). It does _magic_ and automatically discovers containers that want to be exposed. Add a little bit of YAML and you have fully featured, nginx-based applications with reverse proxy and custom domain names running on your local computer:

```yaml
version: "3"
services:
  backend:
    image: nginx
    environment:
      - VIRTUAL_HOST=backend.local.codinginfinity.xyz
    # Why would you create separate Dockerfiles when you can abuse the
    # entrypoint? ;)
    entrypoint: >-
      /bin/sh -c 'echo backend > /usr/share/nginx/html/index.html &&
      nginx -g "daemon off;"'

  frontend:
    image: nginx
    environment:
      - VIRTUAL_HOST=local.codinginfinity.xyz
    entrypoint: >-
      /bin/sh -c 'echo frontend > /usr/share/nginx/html/index.html &&
      nginx -g "daemon off;"'

  proxy:
    image: jwilder/nginx-proxy
    ports:
      - "80:80"
      # - "443:443" we still don't have certificates, so leaving it disabled for now
    volumes:
      - /var/run/docker.sock:/tmp/docker.sock:ro
```

Et voilà! We now have two separate apps running on our local PC that can be accessed using

```sh
$ curl http://backend.local.codinginfinity.xyz
backend
$ curl http://local.codinginfinity.xyz
frontend
```

But beware, even though I use `docker-compose` here, I'm giving the `proxy` container access to `docker.sock` and not some `docker-compose`-constrained socket. It can (and will) inspect the whole state of Docker engine so it will expose all containers that have `VIRTUAL_HOST` set.

### And a little bit of Let's Encrypt

We now have two sites - so-called backend and frontend - running on our local machine that can be accessed (from our local machine only!) using _normal_ addresses. Unfortunately, it all works over HTTP but the main promise of this post was to expose them over HTTP**S**. Here, the awesome [Let's Encrypt](https://letsencrypt.org) service comes with help.

Let's Encrypt will generate a (trusted!) certificate for your domain for free, provided that you show them you own it. This is one of the best services out there, especially since the beginning of last year, when they started supporting wildcard certificates.

Let's Encrypt and their ACME protocol requires to prove one owns the domain it tries to generate certificate for. You might either expose some well-known file with challenge token (the [HTTP](https://ietf-wg-acme.github.io/acme/draft-ietf-acme-acme.html#rfc.section.8.3) challenge) or use DNS records for the same purpose (the [DNS](https://ietf-wg-acme.github.io/acme/draft-ietf-acme-acme.html#rfc.section.8.4) challenge). We cannot really use the first approach to generate certificates for `local.codinginfinity.xyz` as we've already pointed the domain to `127.0.0.1` and would need to correctly handle changes, TTL of records and many more.

In our case the DNS challenge is much easier to use. There are also multiple tools that can automate DNS updates for us. One of my favorites is the [acme.sh](https://github.com/Neilpang/acme.sh) script. It automatically adds necessary records (supports [multiple providers](https://github.com/acmesh-official/acme.sh/wiki/dnsapi)), generates the certificate, handles renewals, cleans up after itself and more.

Since I use OVH servers for the domain and [acme.sh supports it](https://github.com/Neilpang/acme.sh/wiki/How-to-use-OVH-domain-api), issuing certificate for `local.codinginfinity.xyz` is as easy as

```sh
$ export OVH_AK="..."
$ export OVH_AS="..."
$ # Optional export OVH_CK="..."
$ acme.sh --issue -d 'local.codinginfinity.xyz' -d '*.local.codinginfinity.xyz' --dns dns_ovh --test # Always use LE Staging first
```

### Mix it all

All the blocks are now ready, we just need to give the certificate to the proxy. It is possible to do so with only a slight modification of the `docker-compose.yml` file shown above. We just need to `acme.sh --install-cert` into a known location and attach it as a volume to `/etc/nginx/certs`. It will work but will make moving to another machine painful (i.e. you need to embed full path to the certificate in the `compose` file). We can do _better_ here - let's embed the certificate directly in the image!

This can be done with multi-stage builds. The first stage generates & exports the certificate (using `neilpang/acme.sh` image) and the second one (based on `jwilder/nginx-proxy`) just copies it to correct location. Nothing fancy, just a couple of lines in `Dockerfile`:

```dockerfile
FROM neilpang/acme.sh AS cert

ARG OVH_AK
ARG OVH_AS
ARG OVH_CK

# Re-export args as ENV
ENV OVH_AK=${OVH_AK}
ENV OVH_AS=${OVH_AS}
ENV OVH_CK=${OVH_CK}

# Issue & export the certificate
# This has to be done in a single RUN statement as the base image marks /acme.sh
# as VOLUME so it will be purged after the statement (and we cannot mount
# volumes during build phase)
RUN mkdir /export
RUN acme.sh --issue \
    --dns dns_ovh \
    -d 'local.codinginfinity.xyz' -d '*.local.codinginfinity.xyz' && \
    \
    acme.sh --install-cert -d 'local.codinginfinity.xyz' \
    --key-file /export/key.pem \
    --fullchain-file /export/fullchain.pem

# And the final proxy
FROM jwilder/nginx-proxy:alpine

COPY --from=cert /export/fullchain.pem /etc/nginx/certs/local.codinginfinity.xyz.crt
COPY --from=cert /export/key.pem /etc/nginx/certs/local.codinginfinity.xyz.key
```

Build it, tag it (or change compose file to do this for you):

```sh
$ docker build \
    -t proxy-with-ssl \
    --build-arg OVH_AK=$OVH_AK \
    --build-arg OVH_AS=$OVH_AS \
    --build-arg OVH_CK=$OVH_CK \
    .
```

Change the image used in `compose` file and everything will _just work_:

```sh
$ curl https://backend.local.codinginfinity.xyz
backend
$ curl https://local.codinginfinity.xyz
frontend
$ openssl s_client -connect local.codinginfinity.xyz:443 </dev/null
...
+++
Certificate chain
 0 s:CN = local.codinginfinity.xyz
   i:C = US, O = Let's Encrypt, CN = Let's Encrypt Authority X3
 1 s:C = US, O = Let's Encrypt, CN = Let's Encrypt Authority X3
   i:O = Digital Signature Trust Co., CN = DST Root CA X3
+++
...
```

Yay! Mission accomplished! The main drawback of this approach is that you need to have sensible API for your DNS servers (but that should not be a problem) and generate some tokens for it. You also need to renew the certificate in about 60 days (it is valid for 90 days).

I am aware that embedding secrets in the image itself might not be the most secure approach, so **NEVER, EVER DO THIS WITH production images OR WITH PUBLIC DOMAINS**.

For dev purposes it helps tremendously and might ease the development process but always have in mind security implications that it has. Also, do not publish the image to Docker Hub or any public registry. ;)

Final code is available in [this gist](https://gist.github.com/jakubfijalkowski/cbebd432cfbd29de9051a9b265a53ced).

### Summary

Pointing the domain you own to a `localhost`, having valid certificate for it and using _normal_ domains for local development blurs the differences between dev and production environment even more (which is a good thing!). It helped me and my team at work to use SSL/TLS everywhere and I think that now every one of us has a better understanding how it all works (and we don't have mixed content any more ;) ). It has its limitations but I think it is worth checking.

In the next episode (probably not 2 years from now ;) ) - how to run proxy in docker and everything else on localhost (also - HSTS).
