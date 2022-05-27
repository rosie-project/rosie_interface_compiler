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
                % print_parsed_info(Res),
                {ok, Res} ->
                    generate_interface(PkgName, Tag, ActionName, Filename, Res);
                Else ->
                    io:format("Service Parser failed: ~p\n On tokens : ~p\n", [Else, Tokens])
            end;
        ErrorInfo ->
            io:format("Service Scanner failed on file ~p:\n ~p\n", [Filename,ErrorInfo])
    end.

generate_interface(PkgName, Tag, ActionName, Filename, {Constants, Request, Reply}) ->
    Name = filename:basename(Filename, ".srv"),
    InterfaceName = rosie_utils:file_name_to_interface_name(ActionName ++ Name),
    HEADER_DEF = string:to_upper(PkgName ++ "_" ++ InterfaceName ++ "_srv" ++ "_hrl"),
    IncludedHeaders = rosie_utils:produce_includes(PkgName, Request ++ Reply),

    {RequestInput, RequestOutput, SerializerRequest, DeserializerRequest} =
        rosie_utils:produce_in_out(PkgName, Request),
    RequestRecordData = rosie_utils:produce_record_def(PkgName, Request),

    {ReplyInput, ReplyOutput, SerializerReply, DeserializerReply} =
        rosie_utils:produce_in_out(PkgName, Reply),
    ReplyRecordData = rosie_utils:produce_record_def(PkgName, Reply),
    % string of code as output
    {
        PkgName ++ "_" ++ InterfaceName ++ "_srv",
        "-module(" ++
            PkgName ++
            "_" ++
            InterfaceName ++
            "_srv).\n"
            "\n"
            "-export([get_name/0, get_type/0, serialize_request/3, serialize_reply/3, parse_request/1, parse_reply/1]).\n"
            "\n"
            "% self include\n"
            "-include(\"" ++ PkgName ++ "_" ++ InterfaceName ++
            "_srv.hrl\").\n"
            "\n"
            "% GENERAL\n"
            "\n"
            "get_name() ->\n"
            "        \"" ++
            case ActionName /= "" of
                true ->
                    string:lowercase(ActionName) ++ "/_action/";
                false ->
                    ""
            end ++
            rosie_utils:file_name_to_interface_name(Name) ++
            "\".\n"
            "\n"
            "get_type() ->\n"
            "        \"" ++ PkgName ++ "::" ++ Tag ++ "::dds_::" ++
            case ActionName /= "" of
                true ->
                    ActionName ++ "_";
                false ->
                    ""
            end ++ Name ++ "_" ++
            "\".\n"
            "\n" ++
            case rosie_utils:items_contain_usertyped_arrays(Request ++ Reply) of
                true ->
                    %paste extra code
                    ?SERIALIZE_ARRAY_CODE?PARSE_N_TIMES_CODE;
                false ->
                    ""
            end ++
            case rosie_utils:items_need_dinamic_bin_split(Request ++ Reply) of
                true ->
                    %paste extra code
                    ?BIN_TO_BIN_LIST_CODE;
                false ->
                    ""
            end ++
            "\n"
            "% CLIENT\n"
            "serialize_request(Client_ID, RequestNumber, #" ++ PkgName ++ "_" ++ InterfaceName ++
            "_rq{" ++ RequestInput ++
            "}) ->\n\t"
            ++ ?PAYLOAD"0 = <<>>,\n\t" ++
            case SerializerRequest of
                [] ->
                    "";
                Code ->
                    Code ++ ",\n"
            end ++
            "\t<<Client_ID:8/binary, RequestNumber:64/little, "?PAYLOAD++integer_to_list(length(Request))++"/binary>>.\n"
            "\n"
            "parse_reply(<<Client_ID:8/binary, RequestNumber:64/little, "?PAYLOAD"0/binary>>) ->\n"
            "\t_CDR_offset = 0,\n"
            "        " ++
            case DeserializerReply of
                [] ->
                    "";
                Code ->
                    Code ++ ","
            end ++
            "\n"
            "        { Client_ID, RequestNumber, #" ++ PkgName ++ "_" ++ InterfaceName ++ "_rp{" ++
            ReplyOutput ++
            "} }.\n"
            "\n"
            "% SERVER\n"
            "serialize_reply(Client_ID, RequestNumber, #" ++ PkgName ++ "_" ++ InterfaceName ++
            "_rp{" ++ ReplyInput ++
            "}) ->\n\t" ++
            ""?PAYLOAD"0 = <<>>,\n\t" ++
            case SerializerReply of
                [] ->
                    "";
                Code ->
                    Code ++ ",\n"
            end ++
            "        <<Client_ID:8/binary, RequestNumber:64/little, "?PAYLOAD++integer_to_list(length(Reply))++"/binary>>.\n"
            "\n"
            "parse_request(<<Client_ID:8/binary, RequestNumber:64/little, "?PAYLOAD"0/binary>>) ->\n"
            "\t_CDR_offset = 0,\n"
            "        " ++
            case DeserializerRequest of
                [] ->
                    "";
                Code ->
                    Code ++ ","
            end ++
            "\n"
            "        { Client_ID, RequestNumber, #" ++
            PkgName ++
            "_" ++
            InterfaceName ++
            "_rq{" ++
            RequestOutput ++
            "} }.\n"
            "\n",
        % .hrl
        "-ifndef(" ++ HEADER_DEF ++
            ").\n"
            "-define(" ++ HEADER_DEF ++
            ", true).\n"
            "\n" ++
            IncludedHeaders ++
            "\n"
            "\n" ++
            rosie_utils:produce_defines(Constants) ++
            "\n"
            "\n"
            "-record(" ++ PkgName ++ "_" ++ InterfaceName ++ "_rq,{" ++ RequestRecordData ++
            "}).\n"
            "-record(" ++ PkgName ++ "_" ++ InterfaceName ++ "_rp,{" ++ ReplyRecordData ++
            "}).\n"
            "\n"
            "-endif.\n"
    }.
