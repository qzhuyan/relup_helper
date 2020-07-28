-module(gen_appups).

-define(supported_pre_vsns(_CurrVsn), <<".*">>).

-export([do/1]).

do(State) ->
    gen_appups(rebar_dir:base_dir(State)).

gen_appups(BaseDir) ->
    {ok, CWD} = file:get_cwd(),
    [begin
        AppName = filename:basename(Dir),
        AppFile = filename:join([Dir, "ebin", AppName++".app"]),
        {ok, [{_, _, Attrs}]} = file:consult(AppFile),
        RelVsn = case proplists:get_value(vsn, Attrs) of
            undefined -> error({no_vsn_found, AppFile});
            Vsn -> Vsn
        end,
        AppupText = appup_text(RelVsn),
        AppupFile = filename:join([Dir, "ebin", AppName++".appup"]),
        case filelib:is_file(AppupFile) of
            true -> %% already exists
                case file:consult(AppupFile) of
                    {ok, [{RelVsn, _, _}]} -> %% keep the appup file
                        skip;
                    {ok, [{_OtherVsn, _, _}]} -> %% replace the old file
                        write_file(AppupFile, AppupText);
                    _ ->
                        write_file(AppupFile, AppupText)
                end;
            false -> %% no appup file, create one
                write_file(AppupFile, AppupText)
        end
     end || Dir <- filelib:wildcard(filename:join([BaseDir, "lib", "*"])), filelib:is_dir(Dir)],
     untar_and_copy(tar, filename:join([CWD, "*.tar.gz"]), BaseDir),
     untar_and_copy(zip, filename:join([CWD, "*.zip"]), BaseDir).

write_file(Filename, Text) ->
    io:format("writing to file: ~p~n", [Filename]),
    ok = file:write_file(Filename, Text).

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

appup_text(RelVsn) ->
    io_lib:format("~p.",
        [{RelVsn, % Current version
            [{?supported_pre_vsns(RelVsn), []}], % Upgrade from
            [{?supported_pre_vsns(RelVsn), []}]  % Downgrade to
         }]).
