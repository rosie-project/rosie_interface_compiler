-module(compile_provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(NAMESPACE, rosie).
-define(DEPS, [{default, app_discovery}]).


-include_lib("include/compiler_macros.hrl").

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
            {short_desc, "Compile ros2 messages into erl modules."},
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
        compile_services(rebar_app_info:opts(AppInfo),binary_to_list(rebar_app_info:name(AppInfo)),rebar_app_info:dir(AppInfo)),
        compile_messages(rebar_app_info:opts(AppInfo),binary_to_list(rebar_app_info:name(AppInfo)),rebar_app_info:dir(AppInfo))
     end || AppInfo <- Apps],

    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

compile_services(Opts, PkgName, AppDir) -> 
    OutDir = filename:join([AppDir, "src", ?GEN_CODE_DIR]),
    SourceDir = filename:join(AppDir, "srv"),
    FoundFiles = rebar_utils:find_files(SourceDir, ".*\\.srv\$"),
    CompileFun = fun(Source, Opts1) -> srv_compile(Opts1, PkgName, Source, OutDir) end,
    rebar_base_compiler:run(Opts, [], FoundFiles, CompileFun).

compile_messages(Opts, PkgName, AppDir) -> 
    OutDir = filename:join([AppDir, "src", ?GEN_CODE_DIR]),
    SourceDir = filename:join(AppDir, "msg"),
    FoundFiles = rebar_utils:find_files(SourceDir, ".*\\.msg\$"),
    CompileFun = fun(Source, Opts1) -> msg_compile(Opts1, PkgName, Source, OutDir) end,
    rebar_base_compiler:run(Opts, [], FoundFiles, CompileFun).

compile_messages( PkgName, AppDir) -> ok.

srv_compile(_Opts, PkgName, Source, OutDir) ->
    rebar_api:info("ROSIE: called for: ~p\n",[Source]),
    {ok, Filename, Code} = service_compile:file(PkgName,Source),
    OutFile = filename:join([OutDir, Filename]),
    filelib:ensure_dir(OutFile),
    rebar_api:info("ROSIE: writing out ~s", [OutFile]),
    file:write_file(OutFile, Code).


msg_compile(_Opts, PkgName,Source, OutDir) ->
    rebar_api:info("ROSIE: called for: ~p",[Source]), ok.
    % {ok, Filename, Code} = service_compile:file(Source),
    % OutFile = filename:join([OutDir, Filename]),
    % filelib:ensure_dir(OutFile),
    % rebar_api:info("ROSIE: writing out ~s", [OutFile]),
    % file:write_file(OutFile, Code).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

compile_msg_test() -> 
    Files = rebar_utils:find_files("test_interfaces/msg",".*\\.msg\$"),
    io:format("~p\n",[Files]),
    ?assert( 1 == 1).

compile_srv_test() -> 
    Files = rebar_utils:find_files("test_interfaces/srv",".*\\.srv\$"),
    [srv_compile([], "test_interfaces" ,F, "test_interfaces/"++?GEN_CODE_DIR) || F <- Files],
    ModuleFiles = rebar_utils:find_files("test_interfaces/_rosie",".*\\.erl\$"),
    [compile:file(M,[binary]) || M <- ModuleFiles],
    [check_compilation_result(R) || R <- [compile:file(M,[binary]) || M <- ModuleFiles]].

check_compilation_result({ok,_,_}) -> ok;
check_compilation_result(error) -> io:format("ERROR compiling...\n"), ?assert(1==2);
check_compilation_result({error,Errors,_}) -> io:format("~p\n",[Errors]), ?assert(1==2).
-endif.
