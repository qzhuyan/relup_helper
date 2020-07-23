-module(mod_vsn).

-export([parse_transform/2]).

-vsn("0.1.0").

parse_transform(AST, _Opts) ->
    case os:getenv("BUILD_VERSION") of
        false -> AST;
        Vsn -> trans(AST, Vsn, [])
    end.

trans([], _Vsn, ResAST) ->
    lists:reverse(ResAST);
trans([{eof, L} | AST], _Vsn, ResAST) ->
    lists:reverse([{eof, L} | ResAST]) ++ AST;
trans([{attribute, _L, vsn, _Vsn} | AST], Vsn, ResAST) ->
    trans(AST, Vsn, ResAST);
trans([{function,L,_,_, _} | AST], Vsn, ResAST) ->
    trans(AST, Vsn, [{attribute, L, vsn, Vsn} | ResAST]);
trans([F | AST], Vsn, ResAST) ->
    trans(AST, Vsn, [F | ResAST]).
