-module(mod_vsn).

-export([parse_transform/2]).

parse_transform(AST, _Opts) ->
    case os:getenv("BUILD_VERSION") of
        false -> io:format("--- BUILD_VERSION not found ~n", []), AST;
        Vsn -> trans(Vsn, AST)
    end.

trans(Vsn, AST) ->
    lists:map(
        fun ({attribute, L, vsn, _OldVsn}) ->
                io:format("--- changing vsn from ~p to ~p~n", [_OldVsn, Vsn]),
                {attribute, L, vsn, Vsn};
            (Token) -> Token
        end, AST).
