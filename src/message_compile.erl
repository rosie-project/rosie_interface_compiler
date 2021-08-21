-module(message_compile).

-export([file/2]).

-include_lib("include/compiler_macros.hrl").

file(PkgName,Filename) ->     
    {InterfaceName,Code,Header} = gen_interface(PkgName,Filename,scanner,message_parser),
    {ok,InterfaceName, Code, Header}.

gen_interface(PkgName,Filename,Scanner,Parser) -> 
    {ok,Bin} = file:read_file(Filename),
    %io:format(Bin),
    % checking the work of the scanner
    case Scanner:string(binary_to_list(Bin)) of
        {ok,Tokens,EndLine} -> 
            %io:format("~p\n",[Tokens]),
            % checking the work of the Yecc
            case Parser:parse(Tokens) of
                {ok,Res} ->% print_parsed_info(Res),
                     generate_interface(PkgName,Filename,Res);
                Else -> io:format("Parser failed: ~p\n",[Else])
            end;
        ErrorInfo -> io:format("Scanner failed: ~p\n",[ErrorInfo])
    end.


generate_interface(PkgName,Filename,{Items}) ->
    Name = filename:basename(Filename,".msg"),
    InterfaceName = rosie_utils:file_name_to_interface_name(Name),
    {Input,Output, Serializer,Deserializer}  = rosie_utils:produce_in_out(Items),
    IncludedHeaders = rosie_utils:produce_includes(Items),
    HEADER_DEF = string:to_upper(InterfaceName++"_msg"++"_hrl"),
    Sizes = string:join(rosie_utils:get_bitsizes(Items),"+"),
    RecordData = rosie_utils:produce_record_def(Items),
    % string of code as output
    {InterfaceName++"_msg", 
% .erl
"-module("++InterfaceName++"_msg).

-export([get_type/0, serialize/1, parse/1]).

% self include
-include(\""++InterfaceName++"_msg.hrl\").

get_type() ->
        \""++PkgName++"::msg::dds_::"++Name++"_"++"\".

serialize(#"++InterfaceName++"{"++Input++"}) -> 
        <<"++Serializer++">>.

parse(<<"++Deserializer++">>) ->
        #"++InterfaceName++"{"++Output++"}.

",
% .hrl
"-ifndef("++HEADER_DEF++").
-define("++HEADER_DEF++", true).

"++IncludedHeaders++"
% the bit size if it's known
-define("++Name++"_bitsize, "++Sizes++" ).

-record("++InterfaceName++",{"++RecordData++"}).

-endif.
"}.

