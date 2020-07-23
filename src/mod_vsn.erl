-module(mod_vsn).

-export([parse_transform/2]).

parse_transform(AST, _Opts) ->
    AST.
