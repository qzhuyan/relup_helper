-module(relup_helper_gen_appups).

-include("include/relup_helper.hrl").

-define(supported_pre_vsns(_CurrVsn), <<".*">>).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, gen_appups).
-define(DEPS, [{default, compile}]).

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
            {example, "rebar3 relup_helper gen_appups"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "generate *.appup file for each appliction"},
            {desc, "A rebar3 tool that helps generate *.appup file for each appliction,\n"
                   "including all of the deps app."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?LOG(info, "running gen_appups", []),
    Res = gen_appups(rebar_dir:base_dir(State)),
    ?LOG(debug, "gen_appups results: ~p", [Res]),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

gen_appups(BaseDir) ->
    [begin
        {AppName,RelVsn} = vsn_from_app_file(filename:join([Dir, "ebin", "*.app"])),
        AppupText = appup_text(RelVsn),
        AppupFile = filename:join([Dir, "ebin", AppName++".appup"]),
        AppupSrcFile = filename:join([Dir, "src", AppName++".appup.src"]),
        case {filelib:is_file(AppupSrcFile), filelib:is_file(AppupFile)} of
            {true, _} -> %% .appup.src file in src exists, copy the content to ebin
                {ok, AppupText1} = file:script(AppupSrcFile),
                {Dir, write_file(AppupFile, io_lib:format("~p.", [AppupText1]))};
            {false, true} -> %% .appup file in ebin already exists
                case file:consult(AppupFile) of
                    {ok, [{RelVsn, _, _}]} -> %% keep the appup file
                        {Dir, skip};
                    {ok, [{_OtherVsn, _, _}]} -> %% replace the old file
                        {Dir, write_file(AppupFile, AppupText)};
                    _ ->
                        {Dir, write_file(AppupFile, AppupText)}
                end;
            {false, false} -> %% no appup file, create one
                {Dir, write_file(AppupFile, AppupText)}
        end
     end || Dir <- filelib:wildcard(filename:join([BaseDir, "lib", "*"])), filelib:is_dir(Dir)].

write_file(Filename, Text) ->
    ?LOG(debug, "writing to file: ~p", [Filename]),
    ok = file:write_file(Filename, Text).

appup_text(RelVsn) ->
    io_lib:format("~p.",
        [{RelVsn, % Current version
            [{?supported_pre_vsns(RelVsn), []}], % Upgrade from
            [{?supported_pre_vsns(RelVsn), []}]  % Downgrade to
         }]).

vsn_from_app_file(AppFileWildcard) ->
    case filelib:wildcard(AppFileWildcard) of
        [] -> error({no_app_file, AppFileWildcard});
        [AppFile] ->
            case file:consult(AppFile) of
                {ok, [{_, AppName, Attrs0}]} ->
                    case proplists:get_value(vsn, Attrs0) of
                        undefined -> error({no_vsn_found, AppFile});
                        Vsn -> {str(AppName),Vsn}
                    end;
                {error, Error} -> error({consult_app_file, {AppFile, Error}})
            end;
        MultAppFiles ->
            error({multi_app_files_found, MultAppFiles})
    end.

str(Bin) when is_binary(Bin) -> binary_to_list(Bin);
str(Atom) when is_atom(Atom) -> atom_to_list(Atom);
str(Str) when is_list(Str) -> Str.
