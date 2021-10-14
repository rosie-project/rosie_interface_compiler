-module(service_compile).

-export([file/2, file/3]).

-include_lib("include/compiler_macros.hrl").

file(PkgName, Filename) ->
    {InterfaceName, Code, Header} =
        gen_interface(PkgName, "srv", "", Filename, scanner, service_parser),
    {ok, InterfaceName, Code, Header}.

file(PkgName, ActionName, Filename) ->
    {InterfaceName, Code, Header} =
        gen_interface(PkgName, "action", ActionName, Filename, scanner, service_parser),
    {ok, InterfaceName, Code, Header}.

gen_interface(PkgName, Tag, ActionName, Filename, Scanner, Parser) ->
    {ok, Bin} = file:read_file(Filename),
    %io:format(Bin),
    % checking the work of the scanner
    case Scanner:string(binary_to_list(Bin)) of
        {ok, Tokens, _} ->
            %io:format("~p\n",[Tokens]),
            % checking the work of the Yecc
            case Parser:parse(Tokens) of
                {ok, Res} ->% print_parsed_info(Res),
                    generate_interface(PkgName, Tag, ActionName, Filename, Res);
                Else ->
                    io:format("Service Parser failed: ~p\n On tokens : ~p\n", [Else, Tokens])
            end;
        ErrorInfo ->
            io:format("Service Scanner failed: ~p\n", [ErrorInfo])
    end.

generate_interface(PkgName, Tag, ActionName, Filename, {Constants, Request, Reply}) ->
    Name = filename:basename(Filename, ".srv"),
    InterfaceName = rosie_utils:file_name_to_interface_name(ActionName ++ Name),
    HEADER_DEF = string:to_upper(InterfaceName ++ "_srv" ++ "_hrl"),
    IncludedHeaders = rosie_utils:produce_includes(PkgName, Request ++ Reply),

    {RequestInput, RequestOutput, SerializerRequest, DeserializerRequest} =
        rosie_utils:produce_in_out(Request),
    RequestRecordData = rosie_utils:produce_record_def(Request),

    {ReplyInput, ReplyOutput, SerializerReply, DeserializerReply} =
        rosie_utils:produce_in_out(Reply),
    ReplyRecordData = rosie_utils:produce_record_def(Reply),
    % string of code as output
    {InterfaceName ++ "_srv",
     "-module("
     ++ InterfaceName
     ++ "_srv).

-export([get_name/0, get_type/0, serialize_request/3, serialize_reply/3, parse_request/1, parse_reply/1]).

% self include
-include(\""
     ++ InterfaceName
     ++ "_srv.hrl\").

% GENERAL

get_name() ->
        \""
     ++ case ActionName /= "" of
            true ->
                string:lowercase(ActionName) ++ "/_action/";
            false ->
                ""
        end
     ++ rosie_utils:file_name_to_interface_name(Name)
     ++ "\".

get_type() ->
        \""
     ++ PkgName
     ++ "::"
     ++ Tag
     ++ "::dds_::"
     ++ case ActionName /= "" of
            true ->
                ActionName ++ "_";
            false ->
                ""
        end
     ++ Name
     ++ "_"
     ++ "\".

"
     ++ case rosie_utils:items_contain_usertyped_arrays(Request ++ Reply) of
            true ->
                ?PARSE_N_TIMES_CODE; %paste extra code
            false ->
                ""
        end
     ++ case rosie_utils:items_contain_std_arrays(Request ++ Reply) of
            true ->
                ?BIN_TO_BIN_LIST_CODE; %paste extra code
            false ->
                ""
        end
     ++ "
% CLIENT
serialize_request(Client_ID, RequestNumber, #"
     ++ InterfaceName
     ++ "_rq{"
     ++ RequestInput
     ++ "}) -> 
        <<Client_ID:8/binary, RequestNumber:64/little"
     ++ case SerializerRequest of
            [] ->
                "";
            Code ->
                "," ++ Code
        end
     ++ ">>.

parse_reply(<<Client_ID:8/binary, RequestNumber:64/little, Payload_0/binary>>) ->
        "
     ++ case DeserializerReply of
            [] ->
                "";
            Code ->
                Code ++ ","
        end
     ++ "
        { Client_ID, RequestNumber, #"
     ++ InterfaceName
     ++ "_rp{"
     ++ ReplyOutput
     ++ "} }.

% SERVER        
serialize_reply(Client_ID, RequestNumber, #"
     ++ InterfaceName
     ++ "_rp{"
     ++ ReplyInput
     ++ "}) -> 
        <<Client_ID:8/binary, RequestNumber:64/little, "
     ++ SerializerReply
     ++ ">>.

parse_request(<<Client_ID:8/binary, RequestNumber:64/little, Payload_0/binary>>) ->
        "
     ++ case DeserializerRequest of
            [] ->
                "";
            Code ->
                Code ++ ","
        end
     ++ "
        { Client_ID, RequestNumber, #"
     ++ InterfaceName
     ++ "_rq{"
     ++ RequestOutput
     ++ "} }.

",
     % .hrl
     "-ifndef("
     ++ HEADER_DEF
     ++ ").
-define("
     ++ HEADER_DEF
     ++ ", true).

"
     ++ IncludedHeaders
     ++ "

"
     ++ rosie_utils:produce_defines(Constants)
     ++ "

-record("
     ++ InterfaceName
     ++ "_rq,{"
     ++ RequestRecordData
     ++ "}).
-record("
     ++ InterfaceName
     ++ "_rp,{"
     ++ ReplyRecordData
     ++ "}).

-endif.
"}.
