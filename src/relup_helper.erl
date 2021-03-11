-module(relup_helper).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State0} = relup_helper_gen_appups:init(State),
    {ok, _}  = relup_helper_untar:init(State0).
