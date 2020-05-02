+++
title = "Reverse-proxy yourself to the host"
date = 2019-01-13T21:30:00+01:00
aliases = ["post/2019-01-13/reverse_proxy_yourself_to_the_host"]
[taxonomies]
tags = ["docker", "security", "tools"]
+++

This is a follow-up post to the one [I've written last week](https://www.codinginfinity.me/post/2019-01-04/reverse_proxy_yourself_to_localhost_with_ssltls). I've showed there how to leverage Docker and [Let's Encrypt](https://letsencrypt.org) for easier HTTPS on localhost but that solution required developing your app inside a container. I personally prefer that option but sometimes the tooling is not so well suited for work inside container (e.g. webpack + Docker for Windows). For that we need to take a little different route.

<!-- more -->

### Forward the traffic

Docker for Windows/Mac uses a virtual machine with Linux to host Docker Engine. It then integrates with your host OS - shares drives using SMB (that really is PITA :(), abstracts networking configuration and configures your environment so that `docker` CLI can access the engine over TCP. That however makes us pay a penalty in performance and stability. And it makes filesystem go over network, which strips it from most of its functionalities. All of this makes using containers for dev environment less than perfect. Connect this with instabilities of the tooling and you have a disaster. ;) That's why we might want to run our tooling right on the host system.

Docker uses your host computer (by default) as a gateway when routing network traffic. Unfortunately, in each net namespace (each container) your host might have a different IP address. Both Docker for Windows and Docker for Mac resolve a `host.docker.internal` address from inside of container (it's available starting from 18.03 as far as I know) to the host IP, so we don't need to detect that on our own. By default there is no firewall there so we can tell our nginx reverse proxy to route the traffic there instead of passing the packets to other container! That way we can have the proxy inside the container but all of our tooling (e.g. webpack-dev-server or Kestrel) will be on the host.

So, when we know where we should route our traffic, we can tell nginx to do exactly that. Starting where [we left off](https://www.codinginfinity.me/post/2019-01-04/reverse_proxy_yourself_to_localhost_with_ssltls), let's extend the image and just statically configure [server blocks](https://www.nginx.com/resources/wiki/start/topics/examples/server_blocks/) to proxy traffic to host:

```
server {
    server_name api.local.codinginfinity.xyz;
    listen 443 ssl http2; # HTTPS FTW!

    # Some sensible values here, tweak as necessary
    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_prefer_server_ciphers on;
    ssl_session_cache shared:SSL:50m;

    # We need to manually specify which certificate/key to use
    ssl_certificate /etc/nginx/certs/local.codinginfinity.xyz.crt;
    ssl_certificate_key /etc/nginx/certs/local.codinginfinity.xyz.key;

    location / {
        proxy_pass http://host.docker.internal:5000;
    }
}
```

It is also a good idea to force HTTPS and add HSTS header so we can really have everything set properly. This can be simply done by adding another server block (for HTTP -> HTTPS redirect) and telling nginx to add header to the reverse proxy part. It will look like this:

```
# HTTP -> HTTPS
server {
    server_name api.local.codinginfinity.xyz;
    listen 80;

    return 301 https://$host$request_uri;
}

server {
    server_name api.local.codinginfinity.xyz;
    listen 443 ssl http2;

    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_prefer_server_ciphers on;
    ssl_session_cache shared:SSL:50m;
    ssl_certificate /etc/nginx/certs/local.codinginfinity.xyz.crt;
    ssl_certificate_key /etc/nginx/certs/local.codinginfinity.xyz.key;

    # HSTS
    add_header Strict-Transport-Security "max-age=31536000";

    location / {
        proxy_pass http://host.docker.internal:5000;
    }
}
```

We can now copy the above file (let's call it `api.conf`) to `/etc/nginx/conf.d` and the image will be ready to serve our traffic:

```dockerfile
FROM proxy-with-ssl

COPY api.conf /etc/nginx/conf.d/
```

Generation of the config file(s) can be easily scripted with some basic shell script but I leave that as an exercise for the reader. ;)

>
> Side note - it might be necessary to configure your firewall. On Windows/Ubuntu this might work out-of-the-box but for example Arch configures `iptables` to deny all incoming traffic and Docker does not change that. To fix this, one can allow incoming traffic from Docker interfaces using something like:
>
> ```sh
> $ sudo iptables -I INPUT -i docker0 -j ACCEPT
> ```
> or
> ```sh
> $ sudo iptables -I INPUT -i br-XXX -j ACCEPT
> ```
> The second solution is for `docker-compose`-like configurations as it by default creates separate bridged network for the containers (put correct id in place of `XXX`).
>

#### For this you don't need nginx-proxy really

This part of the solution doesn't really use the dynamic behavior of [nginx-proxy](https://github.com/jwilder/nginx-proxy). If we only need a reverse proxy to the host, we can base our solution on just plain nginx, not on the proxy. Yet I think that it is worth developing most of the app inside containers and fall back to dev-on-host when tooling lacks only.

### Connect container to host

This solution has a slight limitation - works only on Docker for Windows and Docker for Mac. ;) I don't think that `host.docker.internal` [will be available on Linux](https://github.com/docker/for-linux/issues/264) anytime soon. I've come up with this solution when still working on Windows but when switching to Linux on all of my machines I needed to fix that. Fortunately for me, the solution to this has already been sketched in the linked GH issue and [Mitz described whole solution](https://dev.to/bufferings/access-host-from-a-docker-container-4099). I just adapted it to my needs and integrated into `nginx-proxy`. The script just checks if `host.docker.internal` is reachable and if not, adds correct entry to `/etc/hosts`:

```sh
#!/bin/sh

# Check is `host.docker.internal` is reachable
HOST_DOMAIN="host.docker.internal"
ping -q -c1 $HOST_DOMAIN > /dev/null 2>&1
if [ $? -ne 0 ]; then
  # Get gateway address and add it to /etc/hosts
  HOST_IP=$(ip route | awk 'NR==1 {print $3}')
  echo -e "$HOST_IP\t$HOST_DOMAIN" >> /etc/hosts
fi

# Run nginx-proxy entrypoint
exec /app/docker-entrypoint.sh forego start -r
```

Put it inside the container, override `ENTRYPOINT` and we are all set:

```dockerfile
FROM proxy-with-ssl

COPY api.conf /etc/nginx/conf.d/
COPY entrypoint.sh /app

ENTRYPOINT ["/app/entrypoint.sh"]
```

>
> There seems to be another option to this problem - custom container that [forwards the traffic to host](https://github.com/qoomon/docker-host) effectively doing NAT. I think this is an overkill as we already have a container working and don't need another one (nor NAT).
>

The full code (as an extension to previous post) is available in [this gist](https://gist.github.com/jakubfijalkowski/983aec848857d018924cf3eeee194b24).

### Summary

I've showed how to extend my [previous solution](https://www.codinginfinity.me/post/2019-01-04/reverse_proxy_yourself_to_localhost_with_ssltls) to work as a reverse proxy (with SSL/TLS termination!) to your host machine. It is self-contained and can easily overcome tooling limitations. It is not perfect as it needs some manual tweaking but greatly reduces configuration needs nevertheless.

Now that the solution is more or less finished - is it helpful or just unnecessarily complicates the development story? Is the certificate generation with Let's Encrypt overkill and self-signed certificates are a way to go? Maybe the reverse proxy approach is not that good for you? Tell me what do you think about it in the comments!
