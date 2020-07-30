-module(relup_helper).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State0} = relup_helper_gen_appups:init(State),
    {ok, State1} = relup_helper_untar:init(State0),
    {ok, State2} = relup_helper_otp_vsn:init(State1),
    {ok, State2}.
