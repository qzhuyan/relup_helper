-module(relup_helper_untar).

-include("include/relup_helper.hrl").

-define(supported_pre_vsns(_CurrVsn), <<".*">>).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, untar).
-define(DEPS, [{default, release}]).

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
            {example, "rebar3 relup_helper untar"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "untar previous versions of emqx.*.zip or emqx.*tar.gz"},
            {desc, "A rebar3 tool that helps untar previous versions of emqx.*.zip or emqx.*tar.gz\n"
                   "into _build/<profile>/rel/emqx/release/ and _build/<profile>/rel/emqx/lib/"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    BaseDir = rebar_dir:base_dir(State),
    {ok, CWD} = file:get_cwd(),
    Tarballs = filelib:wildcard(filename:join([CWD, "*.tar.gz"])),
    {ok, ARCH} = run_cmd("uname -m", nolog),
    [Profile] = rebar_state:current_profiles(State) -- [default],
    Zipballs = filelib:wildcard(filename:join([CWD, "tmp", "relup_packages", Profile,  "*" ++ string:trim(ARCH) ++ ".zip"])),
    ?LOG(info, "untar previous versions of balls: ~p", [Tarballs ++ Zipballs]),
    uncompress_and_copy(tar, Tarballs, BaseDir),
    uncompress_and_copy(zip, Zipballs, BaseDir),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

uncompress_and_copy(Type, Balls, BaseDir) ->
    [begin
        TmpDir = filename:join(["/tmp", "emqx_untar", integer_to_list(erlang:system_time())]),
        untar_pre_vsn_packages(Type, Ball, TmpDir),
        copy_dirs([TmpDir, "emqx", "lib", "*"], [BaseDir, "rel", "emqx", "lib"]),
        copy_dirs([TmpDir, "emqx", "releases", "*"], [BaseDir, "rel", "emqx", "releases"]),
        file:del_dir(TmpDir)
     end || Ball <- Balls, filelib:is_file(Ball)].

copy_dirs(SrcPath, DstPath) ->
    SrcPath0 = filename:join(SrcPath),
    [begin
        DirName = filename:basename(PrevD),
        DstPath0 = filename:join(DstPath ++ [DirName]),
        case filelib:is_dir(DstPath0) of
            true -> %% already exists
                skip;
            false ->
                do_copy(PrevD, DstPath0)
        end
     end|| PrevD <- filelib:wildcard(SrcPath0), filelib:is_dir(PrevD)].

untar_pre_vsn_packages(tar, Ball, TmpDir) ->
    UnTarDir = filename:join([TmpDir, "emqx"]),
    ok = filelib:ensure_dir(filename:join([UnTarDir, "dummy"])),
    Cmd = io_lib:format("tar zxf ~s -C ~s", [Ball, UnTarDir]),
    {ok,_} = run_cmd(Cmd, debug);
untar_pre_vsn_packages(zip, Ball, TmpDir) ->
    ok = filelib:ensure_dir(filename:join([TmpDir, "dummy"])),
    Cmd = io_lib:format("unzip -q ~s -d ~s", [Ball, TmpDir]),
    {ok,_} = run_cmd(Cmd, debug).

do_copy(SrcPath, DstPath) ->
    Cmd = io_lib:format("cp -r ~s ~s", [SrcPath, DstPath]),
    {ok,_} = run_cmd(Cmd, debug).

run_cmd(Cmd, LogLevel) ->
    LogLevel =/= nolog andalso ?LOG(LogLevel, Cmd, []),
    case rebar_utils:sh(Cmd, [return_on_error]) of
        {ok, Result} -> {ok, Result};
        {error, {Code, Details}} ->
            ?LOG(error, "`~p` failed with exit code: ~p~n", [Cmd, Code]),
            ?LOG(error, "~s~n", [Details]),
            failed
    end.
