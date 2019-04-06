Rebar3 Docker
=====

A rebar plugin for generating docker images for running tests and dialyzer against as well as a final image for running a release.

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {project_plugins, [
        {rebar_docker, {git, "https://github.com/tsloughter/rebar_docker.git", {branch, "master"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 docker
    
