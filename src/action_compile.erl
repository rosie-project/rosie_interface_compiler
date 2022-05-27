-module(action_compile).

-export([file/2]).

% -include_lib("include/compiler_macros.hrl").

file(PkgName, Filename) ->
    {ok, gen_interface(PkgName, Filename, action_scanner, action_parser)}.

gen_interface(PkgName, Filename, Scanner, Parser) ->
    {ok, Bin} = file:read_file(Filename),
    %io:format(Bin),
    % checking the work of the scanner
    case Scanner:string(binary_to_list(Bin)) of
        {ok, Tokens, _} ->
            io:format("~p\n",[Tokens]),
            % checking the work of the Yecc
            case Parser:parse(Tokens) of
                % print_parsed_info(Res),
                {ok, Res} ->
                    generate_interface(PkgName, Filename, Res);
                Else ->
                    io:format("Action Parser failed: ~p\n", [Else])
            end;
        ErrorInfo ->
            io:format("Action Scanner failed: ~p\n", [ErrorInfo])
    end.

generate_interface(PkgName, Filename, {Goal_Items, Result_Items, Feedback_Items}) ->
    Name = filename:basename(Filename, ".action"),
    InterfaceName = rosie_utils:file_name_to_interface_name(Name),

    % io:format("~p\n",[Goal_Items]),
    % io:format("~p\n",[Result_Items]),
    % io:format("~p\n",[Feedback_Items]),
    GOAL_SRV =
        "# generated from " ++
            Name ++
            "\n" ++
            "unique_identifier_msgs/UUID goal_id #16 bytes\n" ++
            [L ++ "\n" || {line, L} <- Goal_Items] ++
            "---\n" ++
            "int32 responce_code\n" ++
            "builtin_interfaces/Time timestamp\n",

    RESULT_SRV =
        "# generated from " ++
            Name ++
            "\n" ++
            "unique_identifier_msgs/UUID goal_id\n" ++
            "---\n" ++
            "int32 goal_status\n" ++
            [L ++ "\n" || {line, L} <- Result_Items],

    Feedback_msg =
        "# generated from " ++
            Name ++
            "\n" ++
            "unique_identifier_msgs/UUID goal_id\n" ++
            [L ++ "\n" || {line, L} <- Feedback_Items],

    {InterfaceName, gen_action_erl(PkgName, InterfaceName), gen_action_hrl(PkgName, InterfaceName),
        GOAL_SRV, RESULT_SRV, Feedback_msg}.

gen_action_hrl(PkgName, InterfaceName) ->
    HEADER_DEF = string:to_upper(PkgName ++ "_" ++ InterfaceName ++ "_action" ++ "_hrl"),
    "-ifndef(" ++ HEADER_DEF ++
        ").\n"
        "-define(" ++ HEADER_DEF ++
        ", true).\n"
        "\n"
        "-include_lib(\"" ++ PkgName ++ "/src/_rosie/" ++ PkgName ++ "_" ++ InterfaceName ++ "_" ++
        "send_goal_srv.hrl\").\n"
        "-include_lib(\"" ++ PkgName ++ "/src/_rosie/" ++ PkgName ++ "_" ++ InterfaceName ++ "_" ++
        "get_result_srv.hrl\").\n"
        "-include_lib(\"" ++ PkgName ++ "/src/_rosie/" ++ PkgName ++ "_" ++ InterfaceName ++ "_" ++
        "feedback_message_msg.hrl\").\n"
        "\n"
        "-include_lib(\"action_msgs/src/_rosie/action_msgs_cancel_goal_srv.hrl\").\n"
        "-include_lib(\"action_msgs/src/_rosie/action_msgs_goal_status_array_msg.hrl\").\n"
        "\n"
        "-endif.".

gen_action_erl(PkgName, InterfaceName) ->
    "-module(" ++ PkgName ++ "_" ++ InterfaceName ++ "_action).\n" ++
        "-include_lib(\"" ++ PkgName ++ "/src/_rosie/" ++ PkgName ++ "_" ++ InterfaceName ++
        "_action.hrl\").\n" ++
        "-export([goal/0, get_goal_id/1, get_goal_srv_module/0, get_result_srv_module/0, get_feedback_msg_module/0, get_action_name/0, get_responce_code/1, accept_goal_reply/0, reject_goal_reply/0, failed_result_reply/1, identify_msg/1]).\n\n" ++
        "goal() -> #" ++ PkgName ++ "_" ++ InterfaceName ++
        "_send_goal_rq{goal_id = #unique_identifier_msgs_u_u_i_d{uuid = binary:bin_to_list(<<(crypto:strong_rand_bytes(16))/binary>>) } }.\n" ++
        "get_goal_id(#" ++ PkgName ++ "_" ++ InterfaceName ++
        "_feedback_message{goal_id = UUID }) -> UUID;\n" ++
        "get_goal_id(#" ++ PkgName ++ "_" ++ InterfaceName ++
        "_send_goal_rq{goal_id = UUID }) -> UUID;\n" ++
        "get_goal_id(#" ++ PkgName ++ "_" ++ InterfaceName ++
        "_get_result_rq{goal_id = UUID }) -> UUID.\n" ++
        "get_goal_srv_module() -> " ++
        PkgName ++ "_" ++ InterfaceName ++ "_send_goal_srv.\n" ++
        "get_result_srv_module() -> " ++
        PkgName ++ "_" ++ InterfaceName ++ "_get_result_srv.\n" ++
        "get_feedback_msg_module() -> " ++
        PkgName ++ "_" ++ InterfaceName ++ "_feedback_message_msg.\n\n" ++
        "get_action_name() -> \"" ++
        InterfaceName ++
        "\".\n"
        "get_responce_code(#" ++ PkgName ++ "_" ++ InterfaceName ++
        "_send_goal_rp{responce_code=C}) -> C.\n" ++
        "accept_goal_reply() -> #" ++ PkgName ++ "_" ++ InterfaceName ++
        "_send_goal_rp{responce_code=1}.\n" ++
        "reject_goal_reply() -> #" ++ PkgName ++ "_" ++ InterfaceName ++
        "_send_goal_rp{responce_code=0}.\n" ++
        "failed_result_reply(StatusCode) -> #" ++ PkgName ++ "_" ++ InterfaceName ++
        "_get_result_rp{goal_status=StatusCode}.\n"
        "identify_msg(#" ++ PkgName ++ "_" ++ InterfaceName ++ "_send_goal_rq{}) -> send_goal_rq;\n" ++
        "identify_msg(#" ++ PkgName ++ "_" ++ InterfaceName ++ "_send_goal_rp{}) -> send_goal_rp;\n" ++
        "identify_msg(#" ++ PkgName ++ "_" ++ InterfaceName ++
        "_get_result_rq{}) -> get_result_rq;\n" ++
        "identify_msg(#" ++ PkgName ++ "_" ++ InterfaceName ++
        "_get_result_rp{}) -> get_result_rp;\n" ++
        "identify_msg(#action_msgs_cancel_goal_rq{}) -> cancel_goal_rq;\n" ++
        "identify_msg(#action_msgs_cancel_goal_rp{}) -> cancel_goal_rp;\n" ++
        "identify_msg(#" ++ PkgName ++ "_" ++ InterfaceName ++
        "_feedback_message{}) -> feedback_message;\n" ++
        "identify_msg(#action_msgs_goal_status_array{}) -> goal_status_array;\n" ++
        "identify_msg(_) -> unknow_record.\n".
