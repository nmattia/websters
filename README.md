# Fully functional webapp

Make sure you have [nix](https://nixos.org/nix/) installed

``` shell
$ curl https://nixos.org/nix/install | sh
```

Build the webapp with

``` shell
$ nix-build -A server-exe
```

Run the webapp with

``` shell
$ $(nix-build -A server-exe)
```

To speed up the build, edit `/etc/nix/nix.conf` to retrieve cached builds:

```
binary-caches = http://hydra.nixos.org http://cache.nixos.org https://nixcache.reflex-frp.org/
trusted-binary-caches = http://hydra.nixos.org http://cache.nixos.org http://hydra.cryp.to
binary-cache-public-keys = hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=
signed-binary-caches = *
```
