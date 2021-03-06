-module(clean_provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, clean).
-define(NAMESPACE, rosie).
-define(DEPS, [{default, app_discovery}]).

-include_lib("include/compiler_macros.hrl").

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        providers:create([
            {name, ?PROVIDER},
            {namespace, ?NAMESPACE},
            {module, ?MODULE},
            {bare, true},
            {deps, ?DEPS},
            {example, "rebar3 rosie clean"},
            {opts, []},
            {short_desc, "Clean up generated modules"},
            {desc, ""}
        ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps =
        case rebar_state:current_app(State) of
            undefined ->
                rebar_state:project_apps(State);
            AppInfo ->
                [AppInfo]
        end,
    [
        begin
            %Opts = rebar_app_info:opts(AppInfo),
            SearchDir = filename:join([rebar_app_info:dir(AppInfo), "src", ?GEN_CODE_DIR]),
            FoundFiles = rebar_utils:find_files(SearchDir, ".*\$"),
            [remove_file(File) || File <- FoundFiles]
        end
     || AppInfo <- Apps
    ],

    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

remove_file(Filename) ->
    rebar_api:info("ROSIE: deleting ~s", [Filename]),
    file:delete(Filename).
