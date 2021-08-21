-module(service_compile).

-export([file/2]).

file(PkgName,Filename) ->     
    {InterfaceName, Code, Header} = gen_interface(PkgName,Filename,scanner,service_parser),
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


generate_interface(PkgName,Filename,{Request,Reply}) ->
    Name = filename:basename(Filename,".srv"),
    InterfaceName = rosie_utils:file_name_to_interface_name(Name),
    HEADER_DEF = string:to_upper(InterfaceName++"_srv"++"_hrl"),
    IncludedHeaders = rosie_utils:produce_includes(Request++Reply),

    {RequestInput,RequestOutput, SerializerRequest,DeserializerRequest}  = rosie_utils:produce_in_out(Request),
    RequestSizes = string:join(rosie_utils:get_bitsizes(Request),"+"),
    RequestRecordData = rosie_utils:produce_record_def(Request),

    {ReplyInput,ReplyOutput,SerializerReply,DeserializerReply}  = rosie_utils:produce_in_out(Reply),
    ReplySizes = string:join(rosie_utils:get_bitsizes(Reply),"+"),
    ReplyRecordData = rosie_utils:produce_record_def(Reply),
    % string of code as output
    {InterfaceName++"_srv", 
"-module("++InterfaceName++"_srv).

-export([get_name/0, get_type/0, serialize_request/2, serialize_reply/2, parse_request/1, parse_reply/1]).

% self include
-include(\""++InterfaceName++"_srv.hrl\").

% GENERAL
get_name() ->
        \""++InterfaceName++"\".

get_type() ->
        \""++PkgName++"::srv::dds_::"++Name++"_"++"\".

% CLIENT
serialize_request(Client_ID,#"++InterfaceName++"_rq{"++RequestInput++"}) -> 
        <<Client_ID:8/binary, 1:64/little,"++SerializerRequest++">>.


parse_reply(<<Client_ID:8/binary, 1:64/little,"++DeserializerReply++">>) ->
        {Client_ID, #"++InterfaceName++"_rp{"++ReplyOutput++"}}.

% SERVER        
serialize_reply(Client_ID,#"++InterfaceName++"_rp{"++ReplyInput++"}) -> 
        <<Client_ID:8/binary, 1:64/little, "++SerializerReply++">>.

parse_request(<<Client_ID:8/binary, 1:64/little,"++DeserializerRequest++">>) ->        
        {Client_ID,#"++InterfaceName++"_rq{"++RequestOutput++"}}.
",
% .hrl
"-ifndef("++HEADER_DEF++").
-define("++HEADER_DEF++", true).

"++IncludedHeaders++"
% the bit size if it's known
-define("++Name++"_rq_bitsize, "++RequestSizes++" ).
-define("++Name++"_rp_bitsize, "++ReplySizes++" ).

-record("++InterfaceName++"_rq,{"++RequestRecordData++"}).
-record("++InterfaceName++"_rp,{"++ReplyRecordData++"}).

-endif.
"}.
