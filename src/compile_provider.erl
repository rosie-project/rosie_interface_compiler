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
    Provider =
        providers:create([{name, ?PROVIDER},
                          {namespace, ?NAMESPACE},
                          {module, ?MODULE},
                          {bare, true},
                          {deps, ?DEPS},
                          {example, "rebar3 rosie compile"},
                          {opts, []},
                          {short_desc, "Compile ros2 messages into erl modules."},
                          {desc,
                           "Compiler plugin to automate compilation of .msg .srv and .action files for ROSIE"}]),
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
    [begin
         Opts = rebar_app_info:opts(AppInfo),
         AppName = binary_to_list(rebar_app_info:name(AppInfo)),
         AppDir = rebar_app_info:dir(AppInfo),
         compile_actions(Opts, AppName, AppDir),
         compile_services(Opts, AppName, AppDir),
         compile_messages(Opts, AppName, AppDir)
     end
     || AppInfo <- Apps],

    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

compile_actions(Opts, PkgName, AppDir) ->
    OutDir = filename:join([AppDir, "src", ?GEN_CODE_DIR]),
    SourceDir = filename:join(AppDir, "action"),
    FoundFiles = rebar_utils:find_files(SourceDir, ".*\\.action\$"),
    CompileFun = fun(Source, Opts1) -> compile_action(PkgName, Source, OutDir) end,
    rebar_base_compiler:run(Opts, [], FoundFiles, CompileFun).

compile_services(Opts, PkgName, AppDir) ->
    OutDir = filename:join([AppDir, "src", ?GEN_CODE_DIR]),
    SourceDir = filename:join(AppDir, "srv"),
    FoundFiles = rebar_utils:find_files(SourceDir, ".*\\.srv\$"),
    CompileFun = fun(Source, Opts1) -> compile(PkgName, Source, OutDir, service_compile) end,
    rebar_base_compiler:run(Opts, [], FoundFiles, CompileFun).

compile_messages(Opts, PkgName, AppDir) ->
    OutDir = filename:join([AppDir, "src", ?GEN_CODE_DIR]),
    SourceDir = filename:join(AppDir, "msg"),
    FoundFiles = rebar_utils:find_files(SourceDir, ".*\\.msg\$"),
    CompileFun = fun(Source, Opts1) -> compile(PkgName, Source, OutDir, message_compile) end,
    rebar_base_compiler:run(Opts, [], FoundFiles, CompileFun).

compile_action(PkgName, Source, OutDir) ->
    % rebar_api:info("ROSIE: called for: ~p\n",[Source]),
    {ok, {ActionName, ActionModule, ActionHeader, Goal_srv, Result_srv, Feedback_msg}} =
        action_compile:file(PkgName, Source),

    write_file(OutDir, ActionName ++ "_action.erl", ActionModule),
    write_file(OutDir, ActionName ++ "_action.hrl", ActionHeader),

    OutDirMeta = filename:join([OutDir, "actions", ActionName]),

    write_file(OutDirMeta, "SendGoal.srv", Goal_srv),
    write_file(OutDirMeta, "GetResult.srv", Result_srv),
    write_file(OutDirMeta, "FeedbackMessage.msg", Feedback_msg),

    % OutDirActions = filename:join([OutDir, "actions"]),
    ActionfileName = filename:basename(Source, ".action"),

    %types require package name
    compile(PkgName,
            ActionfileName,
            filename:join([OutDirMeta, "SendGoal.srv"]),
            OutDir,
            service_compile),
    compile(PkgName,
            ActionfileName,
            filename:join([OutDirMeta, "GetResult.srv"]),
            OutDir,
            service_compile),
    compile(PkgName,
            ActionfileName,
            filename:join([OutDirMeta, "FeedbackMessage.msg"]),
            OutDir,
            message_compile).

compile(PkgName, ActionName, Source, OutDir, CompilerModule) ->
    % rebar_api:info("ROSIE: called for: ~p\n",[Source]),
    {ok, Filename, Code, Header} = CompilerModule:file(PkgName, ActionName, Source),
    % Module
    write_file(OutDir, Filename ++ ".erl", Code),
    % HEADER
    write_file(OutDir, Filename ++ ".hrl", Header).

compile(PkgName, Source, OutDir, CompilerModule) ->
    % rebar_api:info("ROSIE: called for: ~p\n",[Source]),
    {ok, Filename, Code, Header} = CompilerModule:file(PkgName, Source),
    % Module
    write_file(OutDir, Filename ++ ".erl", Code),
    % HEADER
    write_file(OutDir, Filename ++ ".hrl", Header).

write_file(OutDir, Filename, Text) ->
    OutFile = filename:join([OutDir, Filename]),
    filelib:ensure_dir(OutFile),
    rebar_api:info("ROSIE: writing out ~s", [OutFile]),
    file:write_file(OutFile, Text).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

clean_before_test() ->
    FoundFiles = rebar_utils:find_files("test_interfaces/" ++ ?GEN_CODE_DIR, ".*"),
    [file:delete(File) || File <- FoundFiles].

compile_action_test() ->
    Files = rebar_utils:find_files("test_interfaces/action", ".*\\.action\$"),
    [compile_action("test_interfaces", F, "test_interfaces/src/" ++ ?GEN_CODE_DIR)
     || F <- Files],

    % MsgFiles = rebar_utils:find_files("test_interfaces/_rosie/actions",".*\\.msg\$"),
    % [compile("test_interfaces" ,F, "test_interfaces/"++?GEN_CODE_DIR++"/actions",message_compile) || F <- MsgFiles],
    % SrvFiles = rebar_utils:find_files("test_interfaces/_rosie/actions",".*\\.srv\$"),
    % [compile("test_interfaces" ,F, "test_interfaces/"++?GEN_CODE_DIR++"/actions",service_compile) || F <- SrvFiles],
    ModuleFiles =
        rebar_utils:find_files("test_interfaces/_rosie/actions", ".*((_msg)|(_srv))\\.erl\$"),
    [compile:file(M, [binary]) || M <- ModuleFiles],
    [check_compilation_result(R)
     || R
            <- [compile:file(M, [verbose, report_errors, report_warnings, binary])
                || M <- ModuleFiles]].

compile_msg_test() ->
    Files = rebar_utils:find_files("test_interfaces/msg", ".*\\.msg\$"),
    [compile("test_interfaces", F, "test_interfaces/src/" ++ ?GEN_CODE_DIR, message_compile)
     || F <- Files],
    ModuleFiles = rebar_utils:find_files("test_interfaces/_rosie", ".*_msg\\.erl\$"),
    [compile:file(M, [binary]) || M <- ModuleFiles],
    [check_compilation_result(R)
     || R
            <- [compile:file(M, [verbose, report_errors, report_warnings, binary])
                || M <- ModuleFiles]].

compile_srv_test() ->
    Files = rebar_utils:find_files("test_interfaces/srv", ".*\\.srv\$"),
    [compile("test_interfaces", F, "test_interfaces/src/" ++ ?GEN_CODE_DIR, service_compile)
     || F <- Files],
    ModuleFiles = rebar_utils:find_files("test_interfaces/_rosie", ".*_srv\\.erl\$"),
    [compile:file(M, [binary]) || M <- ModuleFiles],
    [check_compilation_result(R)
     || R
            <- [compile:file(M, [verbose, report_errors, report_warnings, binary])
                || M <- ModuleFiles]].

check_compilation_result({ok, _, _}) ->
    ok;
check_compilation_result(error) ->
    io:format("ERROR compiling...\n"),
    ?assert(1 == 2);
check_compilation_result({error, Errors, _}) ->
    io:format("~p\n", [Errors]),
    ?assert(1 == 2).

-endif.
