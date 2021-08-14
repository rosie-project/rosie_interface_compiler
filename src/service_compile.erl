-module(service_compile).

-export([file/1]).


file(Filename) ->     
    {InterfaceName,Code} = gen_interface(Filename,service_scanner,service_parser),

    {ok,InterfaceName++".erl", Code}.

gen_interface(Filename,Scanner,Parser) -> 
    {ok,Bin} = file:read_file(Filename),
    %io:format(Bin),
    % checking the work of the scanner
    case Scanner:string(binary_to_list(Bin)) of
        {ok,Tokens,EndLine} -> 
            %io:format("~p\n",[Tokens]),
            % checking the work of the Yecc
            case Parser:parse(Tokens) of
                {ok,Res} ->% print_parsed_info(Res),
                     generate_interface(Filename,Res);
                Else -> io:format("Parser failed: ~p\n",[Else])
            end;
        ErrorInfo -> io:format("Scanner failed: ~p\n",[ErrorInfo])
    end.


generate_interface(Filename,{Request,Reply}) ->
    Name = filename:basename(Filename,".srv"),
    InterfaceName = file_name_to_interface_name(Name),
    {RequestInput,SerializerRequest,DeserializerRequest}  = produce_in_out(Request),
    {ReplyInput,SerializerReply,DeserializerReply}  = produce_in_out(Reply),
    % string of code as output
    {InterfaceName, 
"-module("++InterfaceName++").

-export([get_name/0, get_type/0, serialize_request/2, serialize_reply/2, parse_request/1, parse_reply/1]).

% GENERAL
get_name() ->
        \""++InterfaceName++"\".

get_type() ->
        \"example_interfaces::srv::dds_::"++Name++"_"++"\".

% CLIENT
serialize_request(Client_ID,{"++RequestInput++"}) -> 
        <<Client_ID:8/binary, 1:64/little,"++SerializerRequest++">>.


parse_reply(<<Client_ID:8/binary, 1:64/little,"++DeserializerReply++">>) ->
        {Client_ID, "++ReplyInput++"}.

% SERVER        
serialize_reply(Client_ID,"++ReplyInput++") -> 
        <<Client_ID:8/binary, 1:64/little, "++SerializerReply++">>.

parse_request(<<Client_ID:8/binary, 1:64/little,"++DeserializerRequest++">>) ->        
        {Client_ID,{"++RequestInput++"}}.
"}.

file_name_to_interface_name(Name) -> 
    Splitted = re:split(Name,"([A-Z])",[{return,list}]),
    Separated = lists:map(fun (S) -> 
                    case string:lowercase(S) /= S of
                        true -> "_" ++ S;
                        false -> S
                    end
                end, Splitted),
    string:lowercase(string:trim(Separated, leading, "_")).

produce_in_out(DataList) ->
    VarNames = lists:map(fun({_,{name,N}}) -> N++"," end, DataList),
    VarTypes = lists:map(fun({{type,T},_}) -> T end, DataList),
    InputCode = string:to_upper(string:trim(VarNames,trailing,",")),
    SerializerCode = string:trim(
                            lists:map(fun({T,VarName}) -> " "++type_code(serialize,VarName,T)++"," end, 
                                lists:zip(VarTypes, 
                                    string:split(InputCode, ",",all)))
                        ,trailing, ","),
    DeserializerCode = string:trim(
                            lists:map(fun({T,VarName}) -> " "++type_code(deserialize,VarName,T)++"," end, 
                                lists:zip(VarTypes, 
                                    string:split(InputCode, ",",all)))
                        ,trailing, ","),
    {InputCode,SerializerCode,DeserializerCode}.

type_code(_,VarName,int64) -> VarName++":64/signed-little";
type_code(_,VarName,float32) -> VarName++":32/float-little";
type_code(serialize,VarName,string) -> "(length("++VarName++")+1):32/little,(list_to_binary("++VarName++"))/binary,0:8";
type_code(deserialize,VarName,string) -> "L:32/little, "++VarName++":(L-1)/binary,0:8".

print_parsed_info({Request,Reply}) ->
    io:format("Request is : ~p\nReply is: ~p\n",[Request,Reply]).