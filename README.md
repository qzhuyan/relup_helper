relup_helper
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {relup_helper, {git, "https://host/user/relup_helper.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 relup_helper
    ===> Fetching relup_helper
    ===> Compiling relup_helper
    <Plugin Output>
