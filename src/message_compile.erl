-module(message_compile).

-export([file/2, file/3]).

-include_lib("include/compiler_macros.hrl").

file(PkgName, Filename) ->
    {InterfaceName, Code, Header} =
        gen_interface(PkgName, "msg", "", Filename, scanner, message_parser),
    {ok, InterfaceName, Code, Header}.

% for messages that compose a ros2 action
file(PkgName, ActionName, Filename) ->
    {InterfaceName, Code, Header} =
        gen_interface(PkgName, "action", ActionName, Filename, scanner, message_parser),
    {ok, InterfaceName, Code, Header}.

gen_interface(PkgName, Tag, ActionName, Filename, Scanner, Parser) ->
    {ok, Bin} = file:read_file(Filename),
    %io:format(Bin),
    % checking the work of the scanner
    case Scanner:string(binary_to_list(Bin)) of
        {ok, [], _} ->
            generate_interface(PkgName, Tag, ActionName, Filename, {[], []});
        {ok, Tokens, _EndLine} ->
            %io:format("~p\n",[Tokens]),
            % checking the work of the Yecc
            case Parser:parse(Tokens) of
                {ok, Res} ->
                    generate_interface(PkgName, Tag, ActionName, Filename, Res);
                Else ->
                    io:format("Message Parser failed: ~p\n On tokens : ~p\n", [Else, Tokens])
            end;
        ErrorInfo ->
            io:format("Message Scanner failed: ~p\n On File ~p\n", [ErrorInfo, Filename])
    end.

generate_interface(PkgName, Tag, ActionName, Filename, {Constants, Items}) ->
    Name = filename:basename(Filename, ".msg"),
    InterfaceName = rosie_utils:file_name_to_interface_name(ActionName++Name),
    {Input, Output, Serializer, Deserializer} = rosie_utils:produce_in_out(PkgName, Items),
    IncludedHeaders = rosie_utils:produce_includes(PkgName, Items),
    HEADER_DEF = string:to_upper(PkgName ++ "_" ++ InterfaceName ++ "_msg" ++ "_hrl"),
    RecordData = rosie_utils:produce_record_def(PkgName, Items),
    % string of code as output
    {
        PkgName ++ "_" ++ InterfaceName ++ "_msg",
        % .erl
        "-module(" ++ PkgName ++ "_" ++ InterfaceName ++
            "_msg).\n"
            "\n"
            "-export([get_type/0, serialize/1, serialize/2, parse/1, parse/2]).\n"
            "\n"
            "% self include\n"
            "-include(\"" ++ PkgName ++ "_" ++ InterfaceName ++
            "_msg.hrl\").\n"
            "\n"
            "get_type() ->\n"

            "        \"" ++ PkgName ++ "::" ++ Tag ++ "::dds_::" ++
            case ActionName of
                "" -> "";
                _ -> ActionName++"_"
            end 
            ++ Name ++ "_" ++
            "\".\n\n"
            "\n"
            "serialize(Payload_0,#" ++
            PkgName ++ "_" ++ InterfaceName ++ "{" ++ Input ++
            "}) ->\n"
            "\t" ++ 
            case Serializer of
                [] ->
                    "";
                Code ->
                    Code ++ ","
            end ++
            "\n\tPayload_" ++ integer_to_list(length(Items))++".\n"
            "serialize(MSG) ->\n\tserialize(<<>>,MSG).\n\n"
            "\n" ++
            case rosie_utils:items_contain_usertyped_arrays(Items) of
                true ->
                    %paste extra code
                    ?SERIALIZE_ARRAY_CODE?PARSE_N_TIMES_CODE;
                false ->
                    ""
            end ++
            case rosie_utils:items_contain_std_arrays(Items) of
                true ->
                    %paste extra code
                    ?BIN_TO_BIN_LIST_CODE;
                false ->
                    ""
            end ++
            "\n"
            "\n"
            "parse(Payload) -> \n\tparse(0,Payload).\n"
            "parse(CDR_offset,Payload_0) ->\n"
            "        " ++
            case Deserializer of
                [] ->
                    "";
                Code ->
                    Code ++ ","
            end ++
            "\n"
            "        ParseResult = #" ++ PkgName ++ "_" ++ InterfaceName ++ "{" ++ Output ++
            "},\n"
            "        {ParseResult,Payload_" ++ integer_to_list(length(Items)) ++
            "}.\n"
            "\n",
        % .hrl NOTE: _bitsize is USELESS, we can't know at compile time the length of a user defined datatype, it could contain dinamic arrays
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
            "-record(" ++ PkgName ++ "_" ++ InterfaceName ++ ",{" ++ RecordData ++
            "}).\n"
            "\n"
            "-endif.\n"
    }.
