Rebar3 Docker
=====

A rebar plugin for generating docker images for running tests and dialyzer against as well as a final image for running a release.

Use
---

Requires Docker 18.09 or above.

Add the plugin to your `rebar.config` as a `project_plugin`:

```erlang
{project_plugins, [
  {rebar_docker, {git, "https://github.com/tsloughter/rebar_docker.git", {branch, "master"}}}
]}.
```

Running `rebar3 docker` will build 3 images:

* Builder image, the releases name with suffix `_builder` and tagged with a checksum of the contents from the lock file and `erl_opts`.
* Running image, the releases name and tagged with the current git ref.
* PLT image, the releases name with suffix `_plt` and tagged with the same checksum as the builder image.

Each image is built with the docker `cache-from` argument to include the builder image on the current checksum value as a cache. This means when building the runner or plt image the dependencies will not have to be fetched or built again because they will be up to date in the builder image used as a docker layers cache.

    
