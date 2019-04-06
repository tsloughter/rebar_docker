rebar_docker
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar_docker, {git, "https://host/user/rebar_docker.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar_docker
    ===> Fetching rebar_docker
    ===> Compiling rebar_docker
    <Plugin Output>
