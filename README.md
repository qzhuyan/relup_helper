# relup_helper

A rebar plugin for https://github.com/emqx/emqx relup generation

## Branches

Starting from EMQ X v4.3, [emqx](https://github.com/emqx/emqx) starts to pin
release tags of this repo.

`main` branch is forked from commit e38a9d4 and made to be the default branch.

`master` branch should not be deleted as it is used by emqx old version releases.

## Build

    $ rebar3 compile

## Use

Add the plugin to your rebar config:

    {plugins, [
        {relup_helper, {git, "https://host/user/relup_helper.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 relup_helper
    ===> Fetching relup_helper
    ===> Compiling relup_helper
    <Plugin Output>
