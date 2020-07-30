-module(relup_helper_untar).

-define(supported_pre_vsns(_CurrVsn), <<".*">>).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, untar).
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
    untar_and_copy(tar, filename:join([CWD, "*.tar.gz"]), BaseDir),
    untar_and_copy(zip, filename:join([CWD, "*.zip"]), BaseDir),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

untar_and_copy(Type, Pattern, BaseDir) ->
    [begin
        TmpDir = filename:join(["/tmp", "emqx_untar", integer_to_list(erlang:system_time())]),
        ok = filelib:ensure_dir(filename:join([TmpDir, "dummy"])),
        untar_pre_vsn_packages(Type, Ball, TmpDir),
        copy_dirs([TmpDir, "emqx", "lib", "*"], [BaseDir, "rel", "emqx", "lib"]),
        copy_dirs([TmpDir, "emqx", "releases", "*"], [BaseDir, "rel", "emqx", "releases"]),
        file:del_dir(TmpDir)
     end || Ball <- filelib:wildcard(Pattern), filelib:is_file(Ball)].

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
    Cmd = io_lib:format("tar zxf ~s -C ~s~n", [Ball, TmpDir]),
    run_cmd(Cmd, true);
untar_pre_vsn_packages(zip, Ball, TmpDir) ->
    Cmd = io_lib:format("unzip -q ~s -d ~s~n", [Ball, TmpDir]),
    run_cmd(Cmd, true).

do_copy(SrcPath, DstPath) ->
    Cmd = io_lib:format("cp -r ~s ~s~n", [SrcPath, DstPath]),
    run_cmd(Cmd, true).

run_cmd(Cmd, Debug) ->
    Debug andalso io:format(Cmd),
    os:cmd(Cmd).
