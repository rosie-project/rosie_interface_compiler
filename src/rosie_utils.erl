-module(rosie_utils).

-include_lib("include/compiler_macros.hrl").

-export([
        get_bitsizes/1,
        file_name_to_interface_name/1,
        produce_includes/1,
        produce_in_out/1,
        get_size_of/1,
        get_defalt_val_for/1,
        produce_record_def/1,
        type_code/3
]).


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
    VarTypes = lists:map(fun({TypeToken,_}) -> 
                            case TypeToken of
                                {type,T} -> T;
                                {{type,T},{array,_}} -> T                                 
                            end end, Items),
    UserTypes = [T || T <- VarTypes, not lists:member(T,?ROS2_PRIMITIVES)],
    No_duplicates = sets:to_list(sets:from_list(UserTypes)),
    IncludeLines = lists:map(fun(T) -> "-include(\""++file_name_to_interface_name(T)++"_msg.hrl\").\n" end,No_duplicates),
    lists:flatten(IncludeLines).

    
produce_in_out(DataList) ->
        VarNames = lists:map(fun({_,{name,N}}) -> N end, DataList),
        VarTypes = lists:map(fun({TypeToken,_}) -> 
                            case TypeToken of
                                {type,T} -> T;
                                {{type,T},{array,L}} -> {T,L}                                 
                            end end, DataList),
        InputVars = lists:map(fun({_,{name,N}}) -> string:to_upper(N) end, DataList),
        InputCode = string:join(
                        lists:map(fun({LowName,CapName}) -> " "++LowName++" = "++CapName end, 
                            lists:zip(VarNames, InputVars))
                        ,","),
        OutputCode = string:join(
                        lists:map(fun({LowName,CapName,T}) -> " "++LowName++" = "++rosie_utils:type_code(output,CapName,T) end, 
                            lists:zip3(VarNames, InputVars,VarTypes))
                        ,","),
        SerializerCode = string:join(
                                lists:map(fun({T,InputVar}) -> " "++rosie_utils:type_code(serialize,InputVar,T) end, 
                                    lists:zip(VarTypes,InputVars))
                        ,","),
        DeserializerCode = string:join(
                                lists:map(fun({T,InputVar}) -> " "++rosie_utils:type_code(deserialize,InputVar,T) end, 
                                    lists:zip(VarTypes, InputVars))
                        ,","),
        {InputCode,OutputCode,SerializerCode,DeserializerCode}.
    

get_bitsizes(Items) -> 
    VarTypes = lists:map(fun({TypeToken,_}) -> 
                        case TypeToken of
                            {type,T} -> T;
                            {{type,T},{array,L}} -> {T,L}                                 
                        end end, Items),
    get_bitsizes(VarTypes,[]).

get_bitsizes([],Sizes) -> Sizes;
get_bitsizes([I|TL],S) -> 
    get_bitsizes(TL,[get_size_of(I)|S]).

get_size_of({Type,any}) -> "dinamic";
get_size_of({Type,Array_L}) -> get_size_of(Type)++"*"++Array_L;
get_size_of(char) -> "8";
get_size_of(int32) -> "32";
get_size_of(int64) -> "64";
get_size_of(float32) -> "32";
get_size_of(float64) -> "64";
get_size_of(string) -> "dinamic";
get_size_of(UserType) -> "?"++UserType++"_bitsize".

produce_record_def(Items) ->
    string:join(lists:map(fun({TypeToken,{name,N}}) -> 
                    case TypeToken of
                        {type,T} -> N++"="++get_defalt_val_for(T);
                        {{type,T},{array,L}} -> N++"="++get_defalt_val_for({T,L})                             
                        end end, Items),",").

get_defalt_val_for({Type,any}) -> "[]";
get_defalt_val_for({Type,Array_L}) -> "[]";
get_defalt_val_for(char) -> "0";
get_defalt_val_for(int32) -> "0";
get_defalt_val_for(int64) -> "0";
get_defalt_val_for(float32) -> "0.0";
get_defalt_val_for(float64) -> "0.0";
get_defalt_val_for(string) -> "\"\"";
get_defalt_val_for(UserType) -> "#"++file_name_to_interface_name(UserType)++"{}".

%dinamic arrays
type_code(serialize,VarName,{T,any}) -> 
    "(length("++VarName++")):32/little, 
    (list_to_binary(lists:map(fun (E) -> <<"++type_code(serialize,"E",T) ++">> end,"++VarName++")))/binary";
type_code(deserialize,VarName,{T,any}) -> VarName++"_L:32/little, "++VarName++":("++VarName++"_L*"++get_size_of(T)++" div 8)/binary";
type_code(output,VarName,{T,any}) -> "lists:map(fun(N) -> lists:sublist(binary:bin_to_list("++VarName++"),N*"++get_size_of(T)++" div 8,"++get_size_of(T)++" div 8) end,lists:seq(1,"++VarName++"_L))";

%static arrays
type_code(serialize,VarName,{T,L}) -> 
    "(list_to_binary(lists:map(fun (E) -> <<"++type_code(serialize,"E",T) ++">> end,"++VarName++")))/binary";
type_code(deserialize,VarName,{T,L}) -> VarName++":("++L++"*"++get_size_of(T)++" div 8)/binary";
type_code(output,VarName,{T,L}) -> "lists:map(fun(N) -> lists:sublist(binary:bin_to_list("++VarName++"),N*"++get_size_of(T)++" div 8,"++get_size_of(T)++" div 8) end,lists:seq(1,"++L++"))";

type_code(output,VarName,char) -> VarName;
type_code(_,VarName,char) -> VarName++":8/little";

type_code(output,VarName,int64) -> VarName;
type_code(_,VarName,int64) -> VarName++":64/signed-little";

type_code(output,VarName,float32) -> VarName;
type_code(_,VarName,float32) -> VarName++":32/float-little";

type_code(output,VarName,float64) -> VarName;
type_code(_,VarName,float64) -> VarName++":64/float-little";

type_code(serialize,VarName,string) -> "(length("++VarName++")+1):32/little,(list_to_binary("++VarName++"))/binary,0:8";
type_code(deserialize,VarName,string) -> VarName++"_L:32/little, "++VarName++":("++VarName++"_L-1)/binary,_:(4 -("++VarName++"_L-1) rem 4)/binary";
type_code(output,VarName,string) -> "binary_to_list("++VarName++")";

type_code(serialize,VarName,USER_TYPE) -> "("++string:lowercase(USER_TYPE)++"_msg:serialize("++VarName++"))/binary";
type_code(deserialize,VarName,USER_TYPE) -> VarName++":(?"++USER_TYPE++"_bitsize)";
type_code(output,VarName,USER_TYPE) -> string:lowercase(USER_TYPE)++"_msg:parse("++VarName++")".
