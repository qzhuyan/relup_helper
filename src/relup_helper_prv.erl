-module(relup_helper_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, relup_helper).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 relup_helper"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "A relup helper tool"},
            {desc, "A rebar3 tool that helps create release upgrade packages"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    include_otp_version(State),
    gen_appups:do(State),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

include_otp_version(State) ->
    OTP_VSN_DST = filename:join([rebar_dir:base_dir(State), "rel", "emqx", "etc", "OTP_VERSION"]),
    file:delete(OTP_VSN_DST),
    OTP_VSN_SRC = filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"]),
    io:format("--- cp ~s ~s~n", [OTP_VSN_SRC, OTP_VSN_DST]),
    file:copy(OTP_VSN_SRC, OTP_VSN_DST).
