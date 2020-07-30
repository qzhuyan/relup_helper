-module(relup_helper_otp_vsn).

-define(supported_pre_vsns(_CurrVsn), <<".*">>).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, otp_vsn).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {namespace, relup_helper},
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 relup_helper otp_vsn"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "include OTP_VERSION file into release"},
            {desc, "A rebar3 tool that helps copy the OTP_VERSION from current building\n"
                   "system to the emqx release package"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    include_otp_version(State),
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
