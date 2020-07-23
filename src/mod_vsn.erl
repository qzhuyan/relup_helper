-module(mod_vsn).

-compile({parse_transform, emqx_logger}).

-export([parse_transform/2]).

parse_transform(AST, _Opts) ->
    AST.
