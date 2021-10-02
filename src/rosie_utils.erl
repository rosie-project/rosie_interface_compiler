-module(rosie_utils).

-include_lib("include/compiler_macros.hrl").

-export([
        %get_bitsizes/1,
        file_name_to_interface_name/1,
        produce_includes/2,
        produce_in_out/1,
        get_size_of_base_type/1,
        get_defalt_val_for/1,
        produce_record_def/1,
        type_code/3,
        parse_code/3,
        items_contain_usertyped_arrays/1,
        items_contain_std_arrays/1
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




produce_includes(PkgName, Items) -> 
    VarTypes = lists:map(fun({TypeToken,_}) -> 
                            case TypeToken of
                                {type,T} -> T;
                                {{type,T},{array,_}} -> T                                 
                            end end, Items),
    UserTypes = [T || T <- VarTypes, not lists:member(T,?ROS2_PRIMITIVES)],

    No_duplicates = sets:to_list(sets:from_list(UserTypes)),
    
    LocalPkg = lists:filter(fun({_,_}) -> false; (_) -> true end, No_duplicates),
    ExtPkg = lists:filter(fun({Pkg,Type}) -> true; (_) -> false end, No_duplicates),

    LocalIncludeLines = lists:map(fun(T) -> "-include_lib(\""++PkgName++"/src/_rosie/"++file_name_to_interface_name(T)++"_msg.hrl\").\n" end, LocalPkg),
    ExternalIncludeLines = lists:map(fun({Pkg,T}) -> "-include_lib(\""++Pkg++"/src/_rosie/"++file_name_to_interface_name(T)++"_msg.hrl\").\n" end, ExtPkg),
    lists:flatten(LocalIncludeLines ++ ExternalIncludeLines).

alignement_for_type({array,_,any}) -> "0";
alignement_for_type({array,Type,L}) ->
    case lists:member(Type, ?ROS2_STATIC_PRIMITIVES) of
        true ->    TypeSize = list_to_integer(get_size_of_base_type(Type))*list_to_integer(L),
                    case TypeSize < 32 of
                        true -> integer_to_list(32-TypeSize);
                        false -> "0"
                    end;
        false -> "0"
    end;
alignement_for_type(Type) ->
    case lists:member(Type, ?ROS2_STATIC_PRIMITIVES) of
        true ->    TypeSize = list_to_integer(get_size_of_base_type(Type)),
                    case TypeSize < 32 of
                        true ->integer_to_list(32-TypeSize);
                        false -> "0"
                    end;
        false -> "0"
    end.
    
produce_in_out(DataList) ->
        VarNames = lists:map(fun({_,{name,N}}) -> N end, DataList),
        VarTypes = lists:map(fun({TypeToken,_}) -> 
                            case TypeToken of
                                {type,T} -> T;
                                {{type,T},{array,L}} -> {array,T,L}                                 
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
                                lists:map(fun({T,InputVar}) -> " "++rosie_utils:type_code(serialize,InputVar,T)++",0:"++alignement_for_type(T) end, 
                                    lists:zip(VarTypes,InputVars))
                        ,","),
        DeserializerCode = string:join(
                                lists:map(fun({T,InputVar,Index}) -> rosie_utils:parse_code(InputVar,T,Index) end, 
                                    lists:zip3(VarTypes, InputVars, lists:seq(1,length(DataList))))
                        ,",\n\t"),
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
    get_bitsizes(TL,[get_size_of_base_type(I)|S]).

get_size_of_base_type({Type,any}) -> "0";
get_size_of_base_type({Type,Array_L}) -> get_size_of_base_type(Type)++"*"++Array_L;
get_size_of_base_type(char) -> "8";
get_size_of_base_type(int8) -> "8";
get_size_of_base_type(uint8) -> "8";
get_size_of_base_type(uint32) -> "32";
get_size_of_base_type(int32) -> "32";
get_size_of_base_type(int64) -> "64";
get_size_of_base_type(float32) -> "32";
get_size_of_base_type(float64) -> "64";
get_size_of_base_type(string) -> "0".

produce_record_def(Items) ->
    string:join(lists:map(fun({TypeToken,{name,N}}) -> 
                    case TypeToken of
                        {type,T} -> N++"="++get_defalt_val_for(T);
                        {{type,T},{array,L}} -> N++"="++get_defalt_val_for({array,T,L})                             
                        end end, Items),",").

get_defalt_val_for({array, Type, any}) -> "[]";
get_defalt_val_for({array, Type, Array_L}) -> "[ "++get_defalt_val_for(Type)++" || _ <- lists:seq(1,"++Array_L++")]";
get_defalt_val_for(char) -> "0";
get_defalt_val_for(uint8) -> "0";
get_defalt_val_for(uint32) -> "0";
get_defalt_val_for(int8) -> "0";
get_defalt_val_for(int32) -> "0";
get_defalt_val_for(int64) -> "0";
get_defalt_val_for(float32) -> "0.0";
get_defalt_val_for(float64) -> "0.0";
get_defalt_val_for(string) -> "\"\"";
get_defalt_val_for({_,UserType}) -> "#"++file_name_to_interface_name(UserType)++"{}";
get_defalt_val_for(UserType) -> "#"++file_name_to_interface_name(UserType)++"{}".

check(T) -> T /= char.


type_code(serialize, VarName, {array, T, any}) -> 
    "(length("++VarName++")):32/little, 
    (list_to_binary(lists:map(fun (E) -> <<"++type_code(serialize,"E",T) ++">> end,"++VarName++")))/binary";
type_code(deserialize, VarName, {array, T, any}) -> 
    case lists:member(T, ?ROS2_STATIC_PRIMITIVES) of
        true -> VarName++"_L:32/little, "++VarName++":("++VarName++"_L*"++get_size_of_base_type(T)++" div 8)/binary";
        false -> VarName
    end;
type_code(output,VarName,{array, T, any}) -> 
    case lists:member(T, ?ROS2_STATIC_PRIMITIVES) of
        true -> "[ E || <<"++type_code(deserialize,"E",T)++">> <- break_binary("++VarName++","++VarName++"_L,"++get_size_of_base_type(T)++")]";
        false -> VarName
    end;

%static arrays, simple if elem type has fixed length, otherwise parsing needs to be delegated
type_code(serialize, VarName, {array, T, L}) -> 
    "(list_to_binary(lists:map(fun (E) -> <<"++type_code(serialize,"E",T) ++">> end,"++VarName++")))/binary";
type_code(deserialize, VarName, {array, T, L}) -> 
    case lists:member(T, ?ROS2_STATIC_PRIMITIVES) of
        true -> VarName++":("++L++"*"++get_size_of_base_type(T)++" div 8)/binary";
        false -> VarName
    end;
type_code(output, VarName, {array, T, L}) -> 
    case lists:member(T, ?ROS2_STATIC_PRIMITIVES) of
        true -> "lists:flatten( lists:map(fun(N) -> lists:sublist(binary:bin_to_list("++VarName++"),N*"++get_size_of_base_type(T)++" div 8,"++get_size_of_base_type(T)++" div 8) end,lists:seq(1,"++L++")))";
        false -> VarName
    end;

type_code(output,VarName,char) -> VarName;
type_code(_,VarName,char) -> VarName++":8/little";

type_code(output,VarName,uint8) -> VarName;
type_code(_,VarName,uint8) -> VarName++":8/little";

type_code(output,VarName,uint32) -> VarName;
type_code(_,VarName,uint32) -> VarName++":32/little";

type_code(output,VarName,int8) -> VarName;
type_code(_,VarName,int8) -> VarName++":8/signed-little";

type_code(output,VarName,int32) -> VarName;
type_code(_,VarName,int32) -> VarName++":32/signed-little";

type_code(output,VarName,int64) -> VarName;
type_code(_,VarName,int64) -> VarName++":64/signed-little";

type_code(output,VarName,float32) -> VarName;
type_code(_,VarName,float32) -> VarName++":32/float-little";

type_code(output,VarName,float64) -> VarName;
type_code(_,VarName,float64) -> VarName++":64/float-little";

type_code(serialize,VarName,string) -> "(length("++VarName++")+1):32/little,(list_to_binary("++VarName++"))/binary,0:((4 - (length("++VarName++") rem 4)) * 8)";
type_code(deserialize,VarName,string) -> VarName++"_L:32/little, "++VarName++":("++VarName++"_L-1)/binary,_:(4 -("++VarName++"_L-1) rem 4)/binary";
type_code(output,VarName,string) -> "binary_to_list("++VarName++")";

type_code(serialize,VarName,{_, USER_TYPE}) -> "("++file_name_to_interface_name(USER_TYPE)++"_msg:serialize("++VarName++"))/binary";
type_code(deserialize,VarName,{_, USER_TYPE}) -> VarName++":(?"++USER_TYPE++"_bitsize)";
type_code(output,VarName,{_, USER_TYPE}) -> VarName;

type_code(serialize,VarName,USER_TYPE) -> "("++file_name_to_interface_name(USER_TYPE)++"_msg:serialize("++VarName++"))/binary";
type_code(deserialize,VarName,USER_TYPE) -> VarName++":(?"++USER_TYPE++"_bitsize)";
type_code(output,VarName,USER_TYPE) -> VarName.




parse_code(VarName,{string,any},Index) ->
        "<<"++VarName++"_L:32/little, Str_"++integer_to_list(Index)++"/binary>> = Payload_"++integer_to_list(Index-1)++",\n\t"
        ++"{"++VarName++", Payload_"++integer_to_list(Index)++"} = parse_n_times(string, "++VarName++"_L, Str_"++integer_to_list(Index)++")";
parse_code(VarName,{string,L},Index) ->
        "{"++VarName++", Payload_"++integer_to_list(Index)++"} = parse_n_times(string, L, Str_"++integer_to_list(Index)++")";

parse_code(VarName,{array,T,any},Index) -> 
    case lists:member(T, ?ROS2_PRIMITIVES) of
        true -> "<<"++type_code(deserialize,VarName,{array,T,any})++", Payload_"++integer_to_list(Index)++"/binary>> = Payload_"++integer_to_list(Index-1);
        false -> "<<"++VarName++"_L:32/little, Array_"++integer_to_list(Index)++"/binary>> = Payload_"++integer_to_list(Index-1)++",\n\t"
                ++"{"++VarName++", Payload_"++integer_to_list(Index)++"} = parse_n_times("++file_name_to_interface_name(T)++"_msg, "++VarName++"_L, Array_"++integer_to_list(Index)++")"
    end;
parse_code(VarName,{array,T,L},Index) -> 
    case lists:member(T, ?ROS2_PRIMITIVES) of
        true -> "<<"++type_code(deserialize,VarName,{array,T,L})++", Payload_"++integer_to_list(Index)++"/binary>> = Payload_"++integer_to_list(Index-1);
        false -> "{"++VarName++", Payload_"++integer_to_list(Index)++"} = parse_n_times("++file_name_to_interface_name(T)++"_msg, "++L++", Payload_"++integer_to_list(Index-1)++")"
    end;
parse_code(VarName,{_,T},Index) ->
    "{"++VarName++", Payload_"++integer_to_list(Index)++"} = "++file_name_to_interface_name(T)++"_msg:parse(Payload_"++integer_to_list(Index-1)++")";
parse_code(VarName,T,Index) -> 
    case lists:member(T, ?ROS2_PRIMITIVES) of
        true -> "<< "++type_code(deserialize,VarName,T)++",_:"++alignement_for_type(T)++",Payload_"++integer_to_list(Index)++"/binary>> =  Payload_"++integer_to_list(Index-1);
        false -> "{"++VarName++", Payload_"++integer_to_list(Index)++"} = "++file_name_to_interface_name(T)++"_msg:parse(Payload_"++integer_to_list(Index-1)++")"
    end.


items_contain_usertyped_arrays(Items) ->
    lists:any(fun ({{{type,T},{array,_}},_}) -> 
                    not lists:member(T,?ROS2_STATIC_PRIMITIVES);
                (_) -> 
                    false 
                end, Items).

items_contain_std_arrays(Items) ->
    lists:any(fun ({{{type,T},{array,_}},_}) -> 
                    lists:member(T,?ROS2_STATIC_PRIMITIVES);
                (_) -> 
                    false 
                end, Items).
