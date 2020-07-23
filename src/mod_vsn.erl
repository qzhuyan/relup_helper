-module(mod_vsn).

-export([parse_transform/2]).

-vsn("0.1.0").

parse_transform(AST, _Opts) ->
    case os:getenv("BUILD_VERSION") of
        false -> AST;
        Vsn -> trans(AST, Vsn, false, [])
    end.

trans([], _Vsn, _Changed, ResAST) ->
    lists:reverse(ResAST);
trans([{eof, L} | AST], _Vsn, _Changed, ResAST) ->
    lists:reverse([{eof, L} | ResAST]) ++ AST;
trans([{attribute, _L, vsn, _Vsn} | AST], Vsn, Changed, ResAST) ->
    trans(AST, Vsn, Changed, ResAST);
trans([F = {function,L,_,_, _} | AST], Vsn, false, ResAST) ->
    trans(AST, Vsn, true, [F, {attribute, L, vsn, Vsn} | ResAST]);
trans([F | AST], Vsn, Changed, ResAST) ->
    trans(AST, Vsn, Changed, [F | ResAST]).
