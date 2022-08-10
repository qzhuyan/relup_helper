-module(relup_helper_gen_appups).

-include("include/relup_helper.hrl").

-define(supported_pre_vsns(_CurrVsn), <<".*">>).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, gen_appups).
-define(DEPS, [{default, compile}]).
-define(OPT_WARN_AS_ERROR, warn_as_error).

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
            {opts, [{?OPT_WARN_AS_ERROR, $e, "warn-as-error", undefined, "Warn as Error"}]}, % list of options understood by the plugin
            {short_desc, "generate *.appup file for each appliction"},
            {desc, "A rebar3 tool that helps generate *.appup file for each appliction,\n"
                   "including all of the deps app."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?LOG(info, "running gen_appups", []),
    ok = validate_app_deps(State),
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
                case file:script(AppupSrcFile, [{'VSN', RelVsn}]) of
                    {ok, AppupText1} ->
                        {Dir, write_file(AppupFile, io_lib:format("~p.", [AppupText1]))};
                    {error, Error} ->
                        ?LOG(debug, "read_appup_src_error: ~p", [{AppupSrcFile,Error}]),
                        error({read_appup_src_error, AppupSrcFile, Error})
                end;
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
     end || Dir <- filelib:wildcard(filename:join([BaseDir, "lib", "*"])), is_app_dir(Dir)].

is_app_dir(Dir) ->
    filename:basename(Dir) =/= ".rebar3" andalso filelib:is_dir(Dir).

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

validate_app_deps(State) ->
    rebar_api:info("Checking deps graph...", []),
    G = digraph:new([acyclic]),
    ProjApps = rebar_state:project_apps(State),
    DepApps  = rebar_state:all_deps(State),
    rebar_api:info("PROJECT_APPS: ~p", [lists:map(fun rebar_app_info:name/1, ProjApps)]),
    rebar_api:info("ALL_DEPENDENCIES: ~p", [lists:map(fun rebar_app_info:name/1, DepApps)]),
    Apps = ProjApps ++ DepApps,
    RootApp = rebar_state:current_app(State),
    RootAppName = rebar_app_info:name(RootApp),

    rebar_api:info("Root app is : ~p", [RootAppName]),

    %% Adding graph vertics with all the `app_info'
    lists:foreach(fun(App) ->
                          AppName = rebar_app_info:name(App),
                          digraph:add_vertex(G, AppName, App)
                  end, Apps),

    lists:foreach(fun(App) ->
                          AppName = rebar_app_info:name(App),
                          rebar_api:debug("handling ~p", [AppName]),
                          Deps = rebar_app_info:deps(App),
                          lists:foreach(fun(D) when is_binary(D) andalso is_binary(AppName) ->
                                                case digraph:add_edge(G, AppName, D) of
                                                    {error, _} = E ->
                                                        rebar_api:abort("~p: Add edge {~p,~p} failed: ~p",
                                                                        [?MODULE, AppName, D, E]);
                                                    _ ->
                                                        rebar_api:debug("add deps ~p to ~p~n", [D, AppName]),
                                                        ok
                                                end
                                        end, Deps)
                  end, Apps),

    %% Now we add deps (specified as rebar options) of root app to the graph
    %% RootApp and ProjApps share the same top-level deps
    %% But for some reason the deps field of ProjApps are [].
    lists:foreach(fun(X) ->
                          AppOptDeps = case dict:find(deps, rebar_app_info:opts(X)) of
                                           {ok, DList} when is_list(DList) -> DList;
                                           error -> []
                                       end,
                          ParentName = rebar_app_info:name(X),
                          lists:foreach(fun(Y) ->
                                                AppName = atom_to_binary(element(1, Y)),
                                                case digraph:add_edge(G, ParentName, AppName) of
                                                    {error, _} = E -> rebar:abort("Failed to add edge ~p -> ~p : ~p~n",
                                                                                  [ParentName, AppName, E]);
                                                    _ ->
                                                        rebar_api:debug("add edge ~p -> ~p success",
                                                                        [ParentName, AppName]),
                                                        ok
                                                end
                                        end, AppOptDeps)
                  end, [RootApp]), %% RootApp and ProjApp have the same dep list, so we have RootApp is enough

    %% Abort if we find cycle.
    not digraph_utils:is_acyclic(G) andalso rebar_api:abort("~p: dependency cycle detected", [?MODULE]),
    %% Now find the apps that need to double check the vsns.
    is_dep_discrepancy(G, Apps, false)
        andalso is_warn_as_error(State)
         andalso rebar_api:abort("~p Dependency discrepancy", [?MODULE]),
    digraph:delete(G),
    ok.

-spec is_dep_discrepancy(digraph:graph(), [rebar_app_info:t()], AccRes :: boolean()) -> boolean().
is_dep_discrepancy(_G, [], Res) ->
    Res;
is_dep_discrepancy(G, [App | T], Res) ->
    AppName = rebar_app_info:name(App),
    AName = binary_to_atom(AppName),
    IsDiscrep =
        case digraph:in_neighbours(G, AppName) of
            NList when length(NList) =< 1  ->
                false;
            NList ->
                AppVsn = rebar_app_info:vsn(App),
                NeighDeps = lists:filtermap(
                              fun(X) ->
                                      {X, Label} = digraph:vertex(G, X),
                                      case dict:find(deps, rebar_app_info:opts(Label)) of
                                          {ok, Deps} ->
                                              Val = lists:keyfind(AName, 1, Deps),
                                              {true, {X, Val}};
                                          _ ->
                                              rebar_api:abort("~p: dep ~p not found for ~p", [?MODULE, AName, X])
                                      end
                              end, NList),
                {_, AppOptDeps} = lists:unzip(NeighDeps),
                case lists:usort(fun(A, A) -> true;
                                    ({A, {git, _, {tag, Vsn}}}, {A, Vsn}) -> true;
                                    ({A, Vsn}, {A, {git, _, {tag, Vsn}}}) -> true;
                                    (_,_) -> false
                                 end, AppOptDeps) of
                    [_] ->
                        false;
                    _L ->
                        %% check if root app is using the latest tag/vsn
                        case is_using_latest_dep(AppVsn, AppOptDeps) of
                            true ->
                                false;
                            false ->
                                rebar_api:warn("Dependency discrepancy: App: ~p~n, Evaluated Version: ~p~n, Deps versions: ~p",
                                               [AppName, AppVsn, NeighDeps]),
                                true
                        end
                end
        end,
    is_dep_discrepancy(G, T, Res orelse IsDiscrep).

-spec is_warn_as_error(rebar_state:t()) -> boolean().
is_warn_as_error(State) ->
    {RawArgs, _} = rebar_state:command_parsed_args(State),
    proplists:get_value(?OPT_WARN_AS_ERROR, RawArgs ++ rebar_state:get(State, relup_helper_opts, []), false).

-spec is_using_latest_dep(string(), [Resources::any()]) -> boolean().
is_using_latest_dep(LastestVsn, DepList) ->
    try do_is_using_latest_dep(LastestVsn, DepList)
    catch _:_:ST ->
            %% we have problem of understanding the versioning
            %% thus return `false'
            rebar_api:debug("error while finding lastest tag: ~p, stacktrace: ~p", [DepList, ST]),
            false
    end.
do_is_using_latest_dep(LastestVsn, DepList) ->
    Tags = lists:map(fun ({_, {git, _, {tag, Tag}}}) -> sortable_tag(Tag);
                         ({_, Tag}) when is_list(Tag) -> sortable_tag(Tag);
                         (Other) ->
                             rebar_api:debug("~p, wrong resource ~p", [?MODULE, Other]),
                             undefined
                     end, DepList),
    sortable_tag(LastestVsn) == lists:max(Tags).

sortable_tag(Tag) ->
    lists:map(fun list_to_integer/1, string:tokens(Tag, ".")).

str(Bin) when is_binary(Bin) -> binary_to_list(Bin);
str(Atom) when is_atom(Atom) -> atom_to_list(Atom);
str(Str) when is_list(Str) -> Str.
