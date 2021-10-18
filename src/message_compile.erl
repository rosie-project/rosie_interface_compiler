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
        gen_interface(PkgName, "action", ActionName ++ "_", Filename, scanner, message_parser),
    {ok, InterfaceName, Code, Header}.

gen_interface(PkgName, Tag, ActionName, Filename, Scanner, Parser) ->
    {ok, Bin} = file:read_file(Filename),
    %io:format(Bin),
    % checking the work of the scanner
    case Scanner:string(binary_to_list(Bin)) of
        {ok, [], _} ->
            generate_interface(PkgName, Tag, ActionName, Filename, {[], []});
        {ok, Tokens, EndLine} ->
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
    InterfaceName =
        string:lowercase(ActionName) ++ rosie_utils:file_name_to_interface_name(Name),
    {Input, Output, Serializer, Deserializer} = rosie_utils:produce_in_out(PkgName, Items),
    IncludedHeaders = rosie_utils:produce_includes(PkgName, Items),
    HEADER_DEF = string:to_upper(PkgName ++ "_" ++ InterfaceName ++ "_msg" ++ "_hrl"),
    RecordData = rosie_utils:produce_record_def(PkgName, Items),
    % string of code as output
    {PkgName ++ "_" ++ InterfaceName ++ "_msg",
     % .erl
     "-module("
     ++ PkgName
     ++ "_"
     ++ InterfaceName
     ++ "_msg).

-export([get_type/0, serialize/1, parse/1]).

% self include
-include(\""
     ++ PkgName
     ++ "_"
     ++ InterfaceName
     ++ "_msg.hrl\").

get_type() ->
        \""
     ++ PkgName
     ++ "::"
     ++ Tag
     ++ "::dds_::"
     ++ ActionName
     ++ Name
     ++ "_"
     ++ "\".

serialize(#"
     ++ PkgName
     ++ "_"
     ++ InterfaceName
     ++ "{"
     ++ Input
     ++ "}) ->
        <<"
     ++ Serializer
     ++ ">>.

"
     ++ case rosie_utils:items_contain_usertyped_arrays(Items) of
            true ->
                ?PARSE_N_TIMES_CODE; %paste extra code
            false ->
                ""
        end
     ++ case rosie_utils:items_contain_std_arrays(Items) of
            true ->
                ?BIN_TO_BIN_LIST_CODE; %paste extra code
            false ->
                ""
        end
     ++ "

parse(Payload_0) ->
        "
     ++ case Deserializer of
            [] ->
                "";
            Code ->
                Code ++ ","
        end
     ++ "
        ParseResult = #"
     ++ PkgName
     ++ "_"
     ++ InterfaceName
     ++ "{"
     ++ Output
     ++ "},
        {ParseResult,Payload_"
     ++ integer_to_list(length(Items))
     ++ "}.

",
     % .hrl NOTE: _bitsize is USELESS, we can't know at compile time the length of a user defined datatype, it could contain dinamic arrays
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
     ++ PkgName
     ++ "_"
     ++ InterfaceName
     ++ ",{"
     ++ RecordData
     ++ "}).

-endif.
"}.
