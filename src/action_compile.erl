-module(action_compile).

-export([file/2]).

-include_lib("include/compiler_macros.hrl").

file(PkgName,Filename) ->
    {ok, gen_interface(PkgName,Filename,action_scanner,action_parser)}.

gen_interface(PkgName,Filename,Scanner,Parser) -> 
    {ok,Bin} = file:read_file(Filename),
    %io:format(Bin),
    % checking the work of the scanner
    case Scanner:string(binary_to_list(Bin)) of
        {ok,Tokens,_} -> 
            %io:format("~p\n",[Tokens]),
            % checking the work of the Yecc
            case Parser:parse(Tokens) of
                {ok,Res} ->% print_parsed_info(Res),
                     generate_interface(PkgName,Filename,Res);
                Else -> io:format("Action Parser failed: ~p\n",[Else])
            end;
        ErrorInfo -> io:format("Action Scanner failed: ~p\n",[ErrorInfo])
    end.


generate_interface(PkgName,Filename,{Goal_Items,Result_Items,Feedback_Items}) ->
        Name = filename:basename(Filename,".action"),
        InterfaceName = rosie_utils:file_name_to_interface_name(Name),

        % io:format("~p\n",[Goal_Items]),
        % io:format("~p\n",[Result_Items]),
        % io:format("~p\n",[Feedback_Items]),

        GOAL_SRV = 
        "# generated from "++Name++"\n"
        ++"unique_identifier_msgs/UUID goal_id #16 bytes\n"
        ++[ L++"\n" || {line, L} <- Goal_Items]
        ++"---\n"
        ++"int32 responce_code\n"
        ++"builtin_interfaces/Time timestamp\n",
        
        RESULT_SRV = 
        "# generated from "++Name++"\n"
        ++"unique_identifier_msgs/UUID goal_id\n"
        ++"---\n"
        ++"int32 goal_status\n"
        ++[ L++"\n" || {line, L} <- Result_Items],

        Feedback_msg = 
        "# generated from "++Name++"\n"
        ++"unique_identifier_msgs/UUID goal_id\n"
        ++[ L++"\n" || {line, L} <- Feedback_Items],

        {InterfaceName,
        gen_action_erl(PkgName, InterfaceName),
        gen_action_hrl(PkgName, InterfaceName),
        GOAL_SRV,
        RESULT_SRV,
        Feedback_msg}.

gen_action_hrl(PkgName, InterfaceName) -> 
    HEADER_DEF =string:to_upper(InterfaceName++"_action"++"_hrl"),
    "-ifndef("++HEADER_DEF++").
-define("++HEADER_DEF++", true).

-include_lib(\""++PkgName++"/src/_rosie/"++InterfaceName++"_"++"send_goal_srv.hrl\").
-include_lib(\""++PkgName++"/src/_rosie/"++InterfaceName++"_"++"get_result_srv.hrl\").
-include_lib(\""++PkgName++"/src/_rosie/"++InterfaceName++"_"++"feedback_message_msg.hrl\").

-include_lib(\"action_msgs/src/_rosie/"++"cancel_goal_srv.hrl\").
-include_lib(\"action_msgs/src/_rosie/"++"goal_status_array_msg.hrl\").

-endif.".


gen_action_erl(PkgName, InterfaceName) -> 
    "-module("++InterfaceName++"_action).\n"++
    "-include_lib(\""++PkgName++"/src/_rosie/"++InterfaceName++"_action.hrl\").\n"++
    "-export([goal/0,get_goal_id/1, get_goal_srv_module/0, get_result_srv_module/0, get_feedback_msg_module/0,get_action_name/0,get_responce_code/1,identify_msg/1]).\n\n"++
    
    "goal() -> #"++InterfaceName++"_send_goal_rq{goal_id = #u_u_i_d{uuid = binary:bin_to_list(<<(crypto:strong_rand_bytes(16))/binary>>) } }.\n"++
    "get_goal_id(#"++InterfaceName++"_send_goal_rq{goal_id = UUID }) -> UUID;\n"++
    "get_goal_id(#"++InterfaceName++"_get_result_rq{goal_id = UUID }) -> UUID.\n"++
    "get_goal_srv_module() -> "++InterfaceName++"_send_goal_srv.\n"++
    "get_result_srv_module() -> "++InterfaceName++"_get_result_srv.\n"++
    "get_feedback_msg_module() -> "++InterfaceName++"_feedback_message_msg.\n\n"++
    "get_action_name() -> \""++InterfaceName++"\".\n"
    "get_responce_code(#"++InterfaceName++"_send_goal_rp{responce_code=C}) -> C.\n"++
    "identify_msg(#"++InterfaceName++"_send_goal_rq{}) -> send_goal_rq;\n"++
    "identify_msg(#"++InterfaceName++"_send_goal_rp{}) -> send_goal_rp;\n"++
    "identify_msg(#"++InterfaceName++"_get_result_rq{}) -> get_result_rq;\n"++
    "identify_msg(#"++InterfaceName++"_get_result_rp{}) -> get_result_rp;\n"++
    "identify_msg(#cancel_goal_rq{}) -> cancel_goal_rq;\n"++
    "identify_msg(#cancel_goal_rp{}) -> cancel_goal_rp;\n"++
    "identify_msg(#"++InterfaceName++"_feedback_message{}) -> feedback_message;\n"++
    "identify_msg(#goal_status_array{}) -> goal_status_array;\n"++
    "identify_msg(_) -> unknow_record.\n".