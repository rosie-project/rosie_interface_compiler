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
    InterfaceName = file_name_to_interface_name(Name),
    {Input,Output, Serializer,Deserializer}  = produce_in_out(Items),
    IncludedHeaders = produce_includes(Items),
    HEADER_DEF = string:to_upper(InterfaceName++"_msg"++"_hrl"),
    Sizes = string:join(get_bitsizes(Items),"+"),
    RecordData = produce_record_def(Items),
    % string of code as output
    {InterfaceName++"_msg", 
% .erl
"-module("++InterfaceName++"_msg"++").

-export([get_type/0, serialize/1, parse/1]).

% self include
-include(\""++InterfaceName++"_msg.hrl\").
"++IncludedHeaders++"
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

file_name_to_interface_name(Name) -> 
    Splitted = re:split(Name,"([A-Z])",[{return,list}]),
    Separated = lists:map(fun (S) -> 
                    case string:lowercase(S) /= S of
                        true -> "_" ++ S;
                        false -> S
                    end
                end, Splitted),
    string:lowercase(string:trim(Separated, leading, "_")).

produce_includes(Items) -> 
    VarTypes = lists:map(fun({{type,T},_}) -> T end, Items),
    UserTypes = [T || T <- VarTypes, not lists:member(T,?ROS2_PRIMITIVES)],
    No_duplicates = sets:to_list(sets:from_list(UserTypes)),
    IncludeLines = lists:map(fun(T) -> "-include(\""++string:lowercase(T)++"_msg.hrl\").\n" end,No_duplicates),
    lists:flatten(IncludeLines).

produce_in_out(DataList) ->
    VarNames = lists:map(fun({_,{name,N}}) -> N end, DataList),
    VarTypes = lists:map(fun({{type,T},_}) -> T end, DataList),
    InputVars = lists:map(fun({_,{name,N}}) -> string:to_upper(N) end, DataList),
    InputCode = string:join(
                    lists:map(fun({LowName,CapName}) -> " "++LowName++" = "++CapName end, 
                        lists:zip(VarNames, InputVars))
                    ,","),
    OutputCode = InputCode,
    SerializerCode = string:join(
                            lists:map(fun({T,InputVar}) -> " "++type_code(serialize,InputVar,T) end, 
                                lists:zip(VarTypes,InputVars))
                    ,","),
    DeserializerCode = string:join(
                            lists:map(fun({T,InputVar}) -> " "++type_code(deserialize,InputVar,T) end, 
                                lists:zip(VarTypes, InputVars))
                    ,","),
    {InputCode,OutputCode,SerializerCode,DeserializerCode}.

get_bitsizes(Items) -> 
    VarTypes = lists:map(fun({{type,T},_}) -> T end, Items),
    get_bitsizes(VarTypes,[]).

get_bitsizes([],Sizes) -> Sizes;
get_bitsizes([I|TL],S) -> 
    get_bitsizes(TL,[get_size_of(I)|S]).

get_size_of(int32) -> "32";
get_size_of(int64) -> "64";
get_size_of(float32) -> "32";
get_size_of(float64) -> "64";
get_size_of(string) -> "undefined";
get_size_of(UserType) -> "?"++UserType++"_bitsize".

produce_record_def(Items) ->
    string:join(lists:map(fun({{type,T},{name,N}}) -> N++"="++get_defalt_val_for(T) end, Items),",").

get_defalt_val_for(int32) -> "0";
get_defalt_val_for(int64) -> "0";
get_defalt_val_for(float32) -> "0.0";
get_defalt_val_for(float64) -> "0.0";
get_defalt_val_for(string) -> "\"\"";
get_defalt_val_for(UserType) -> "#"++file_name_to_interface_name(UserType)++"{}".

type_code(output,VarName,int64) -> VarName;
type_code(_,VarName,int64) -> VarName++":64/signed-little";

type_code(output,VarName,float32) -> VarName;
type_code(_,VarName,float32) -> VarName++":32/float-little";

type_code(output,VarName,float64) -> VarName;
type_code(_,VarName,float64) -> VarName++":64/float-little";

type_code(serialize,VarName,string) -> "(length("++VarName++")+1):32/little,(list_to_binary("++VarName++"))/binary,0:8";
type_code(deserialize,VarName,string) -> "L:32/little, "++VarName++":(L-1)/binary,_/binary";
type_code(output,VarName,string) -> "binary_to_list("++VarName++")";

type_code(serialize,VarName,USER_TYPE) -> "("++string:lowercase(USER_TYPE)++"_msg:serialize("++VarName++"))/binary";
type_code(deserialize,VarName,USER_TYPE) -> VarName++":(?"++USER_TYPE++"_bitsize)";
type_code(output,VarName,USER_TYPE) -> string:lowercase(USER_TYPE)++"_msg:parse("++VarName++")".
