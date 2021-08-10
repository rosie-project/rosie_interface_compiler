-module(root_compiler).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(NAMESPACE, rosie).
-define(DEPS, [{default, app_discovery}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},
            {namespace, ?NAMESPACE},
            {module, ?MODULE},
            {bare, true},
            {deps, ?DEPS},
            {example, "rebar3 rosie compile"},
            {opts, []},
            {short_desc, "Erlang Compiler for ros messages"},
            {desc, "Compiler plugin to automate compilation of .msg .srv and .action files for ROSIE"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
                  undefined ->
                      rebar_state:project_apps(State);
                  AppInfo ->
                      [AppInfo]
              end,
    [begin
         Opts = rebar_app_info:opts(AppInfo),
         OutDir = rebar_app_info:out_dir(AppInfo),
         SourceDir = filename:join(rebar_app_info:dir(AppInfo), "srv"),
         FoundFiles = rebar_utils:find_files(SourceDir, ".*\\.srv\$"),

         CompileFun = fun(Source, Opts1) ->
                            srv_compile(Opts1, Source, OutDir)
                      end,

         rebar_base_compiler:run(Opts, [], FoundFiles, CompileFun)
     end || AppInfo <- Apps],

    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

srv_compile(_Opts, Source, OutDir) ->
    rebar_api:info("ROSIE: called for: ~p\n",[Source]),
    {ok, Filename, Code} = service_compile:file(Source),
    OutFile = filename:join([OutDir, Filename]),
    rebar_api:info("ROSIE: writing out ~s", [OutFile]),
    file:write_file(OutFile, Code).
    % {ok, Binary} = file:read_file(Source),
    % OutFile = filename:join([OutDir, "priv", filename:basename(Source)]),
    % filelib:ensure_dir(OutFile),
    % rebar_api:info("Writing out ~s", [OutFile]),
    % file:write_file(OutFile, Binary).