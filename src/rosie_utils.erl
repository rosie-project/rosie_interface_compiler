-module(rosie_utils).

-include_lib("include/compiler_macros.hrl").

-export([
    file_name_to_interface_name/1,
    produce_includes/2,
    produce_defines/1,
    produce_in_out/2,
    get_size_of_base_type/1,
    produce_record_def/2,
    type_code/3,
    serialization_code/3,
    parse_code/3,
    items_contain_usertyped_arrays/1,
    items_need_dinamic_bin_split/1
]).


file_name_to_interface_name(Name) ->
    Splitted = re:split(Name, "([A-Z])", [{return, list}]),
    Separated =
        lists:map(
            fun(S) ->
                case string:lowercase(S) /= S of
                    true ->
                        "_" ++ S;
                    false ->
                        S
                end
            end,
            Splitted
        ),
    string:lowercase(
        string:trim(Separated, leading, "_")
    ).

get_type_from_item({TypeToken, _}) ->
    case TypeToken of
        {type, T} ->
            T;
        {{type, T}, {array, _}} ->
            T
    end.

produce_includes(PkgName, Items) ->
    VarTypes = lists:map(fun get_type_from_item/1, Items),
    UserTypes = [T || T <- VarTypes, not lists:member(T, ?ROS2_PRIMITIVES)],

    No_duplicates =
        sets:to_list(
            sets:from_list(UserTypes)
        ),

    LocalPkg =
        lists:filter(
            fun
                ({_, _}) ->
                    false;
                (_) ->
                    true
            end,
            No_duplicates
        ),
    ExtPkg =
        lists:filter(
            fun
                ({_Pkg, _Type}) ->
                    true;
                (_) ->
                    false
            end,
            No_duplicates
        ),

    LocalIncludeLines =
        lists:map(
            fun(T) ->
                "-include_lib(\"" ++ PkgName ++ "/src/_rosie/" ++ PkgName ++ "_" ++
                    file_name_to_interface_name(T) ++ "_msg.hrl\").\n"
            end,
            LocalPkg
        ),
    ExternalIncludeLines =
        lists:map(
            fun({Pkg, T}) ->
                "-include_lib(\"" ++ Pkg ++ "/src/_rosie/" ++ Pkg ++ "_" ++
                    file_name_to_interface_name(T) ++ "_msg.hrl\").\n"
            end,
            ExtPkg
        ),
    lists:flatten(LocalIncludeLines ++ ExternalIncludeLines).

produce_defines(Constants) ->
    string:join(
        lists:map(
            fun({_, {macro, N}, {value, V}}) -> "-define(" ++ N ++ ", " ++ V ++ ")." end,
            Constants
        ),
        "\n"
    ).

produce_in_out(PkgName, DataList) ->
    VarNames =
        lists:map(
            fun
                ({_, {{name, N}, _}}) ->
                    N;
                ({_, {name, N}}) ->
                    N
            end,
            DataList
        ),
    VarTypes =
        lists:map(
            fun({TypeToken, _}) ->
                case TypeToken of
                    {type, {ExtPkg, T}} ->
                        {ExtPkg, T};
                    {type, T} ->
                        {PkgName, T};
                    {{type, {ExtPkg, T}}, {array, L}} ->
                        {array, {ExtPkg, T}, L};
                    {{type, T}, {array, L}} ->
                        {array, {PkgName, T}, L}
                end
            end,
            DataList
        ),
    InputVars =
        lists:map(
            fun
                ({_, {{name, N}, _}}) ->
                    string:to_upper(N);
                ({_, {name, N}}) ->
                    string:to_upper(N)
            end,
            DataList
        ),
    InputCode =
        string:join(
            lists:map(
                fun({LowName, CapName}) -> " " ++ LowName ++ " = " ++ CapName end,
                lists:zip(VarNames, InputVars)
            ),
            ","
        ),
    OutputCode =
        string:join(
            lists:map(
                fun({LowName, CapName, T}) ->
                    " " ++ LowName ++ " = " ++ rosie_utils:type_code(output, CapName, T)
                end,
                lists:zip3(VarNames, InputVars, VarTypes)
            ),
            ","
        ),
    SerializerCode =
        string:join(
            lists:map(
                fun({T, InputVar, Index}) -> rosie_utils:serialization_code(InputVar, T, Index) end,
                lists:zip3(VarTypes, InputVars, lists:seq(1, length(DataList)))
            ),
            ",\n\t"
        ),
    DeserializerCode =
        string:join(
            lists:map(
                fun({T, InputVar, Index}) -> rosie_utils:parse_code(InputVar, T, Index) end,
                lists:zip3(VarTypes, InputVars, lists:seq(1, length(DataList)))
            ),
            ",\n\t"
        ),
    {InputCode, OutputCode, SerializerCode, DeserializerCode}.


get_size_of_base_type({_Type, any}) ->
    "0";
get_size_of_base_type({Type, Array_L}) ->
    get_size_of_base_type(Type) ++ "*" ++ Array_L;
get_size_of_base_type(bool) ->
    "8";
get_size_of_base_type(char) ->
    "8";
get_size_of_base_type(uint8) ->
    "8";
get_size_of_base_type(uint16) ->
    "16";
get_size_of_base_type(uint32) ->
    "32";
get_size_of_base_type(uint64) ->
    "64";
get_size_of_base_type(int8) ->
    "8";
get_size_of_base_type(int16) ->
    "16";
get_size_of_base_type(int32) ->
    "32";
get_size_of_base_type(int64) ->
    "64";
get_size_of_base_type(float32) ->
    "32";
get_size_of_base_type(float64) ->
    "64";
get_size_of_base_type(string) ->
    get_size_of_base_type(uint32).

record_field_from_item(_PkgName, {_TypeToken, {{name, N}, {value, DEFAULT}}}) ->
    N ++ "=" ++ DEFAULT;
record_field_from_item(PkgName, {TypeToken, {name, N}}) ->
    case TypeToken of
        {type, T} ->
            N ++ "=" ++ get_defalt_val_for(PkgName, T);
        {{type, T}, {array, L}} ->
            N ++ "=" ++ get_defalt_val_for(PkgName, {array, T, L})
    end.

produce_record_def(PkgName, Items) ->
    string:join(
        lists:map(fun(I) -> record_field_from_item(PkgName, I) end, Items), ","
    ).

get_defalt_val_for(_, {array, _Type, any}) ->
    "[]";
get_defalt_val_for(LocalPkg, {array, Type, Array_L}) ->
    "[ " ++ get_defalt_val_for(LocalPkg, Type) ++ " || _ <- lists:seq(1," ++ Array_L ++ ")]";
get_defalt_val_for(_, bool) ->
    "false";
get_defalt_val_for(_, char) ->
    "0";
get_defalt_val_for(_, uint8) ->
    "0";
get_defalt_val_for(_, uint16) ->
    "0";
get_defalt_val_for(_, uint32) ->
    "0";
get_defalt_val_for(_, uint64) ->
    "0";
get_defalt_val_for(_, int8) ->
    "0";
get_defalt_val_for(_, int16) ->
    "0";
get_defalt_val_for(_, int32) ->
    "0";
get_defalt_val_for(_, int64) ->
    "0";
get_defalt_val_for(_, float32) ->
    "0.0";
get_defalt_val_for(_, float64) ->
    "0.0";
get_defalt_val_for(_, string) ->
    "\"\"";
get_defalt_val_for(_, {ExtPkg, UserType}) ->
    "#" ++ ExtPkg ++ "_" ++ file_name_to_interface_name(UserType) ++ "{}";
get_defalt_val_for(LocalPkg, UserType) ->
    "#" ++ LocalPkg ++ "_" ++ file_name_to_interface_name(UserType) ++ "{}".

type_code(serialize, VarName, {array, T, _}) ->
    "(list_to_binary(lists:map(fun (E) -> <<" ++ type_code(serialize, "E", T) ++ ">> end," ++ VarName ++ ")))/binary";
type_code(deserialize, VarName, {array, {_, T}, any}) ->
    VarName ++ "_L:32/little, " ++ VarName ++ ":(" ++ VarName ++ "_L*" ++ get_size_of_base_type(T) ++ " div 8)/binary";
type_code(output, VarName, {array, {Pkg, T}, any}) ->
    case lists:member(T, ?ROS2_STATIC_PRIMITIVES) of
        true ->
            "[ E || <<" ++ type_code(deserialize, "E", {Pkg, T}) ++ ">> <- break_binary(" ++ VarName ++
                "," ++ VarName ++ "_L," ++ get_size_of_base_type(T) ++ ")]";
        false ->
            VarName
    end;

type_code(deserialize, VarName, {array, {_, T}, L}) ->
    VarName ++ ":(" ++ L ++ "*" ++ get_size_of_base_type(T) ++ " div 8)/binary";
type_code(output, VarName, {array, {_, T}, L}) ->
    case lists:member(T, ?ROS2_STATIC_PRIMITIVES) of
        true ->
            "lists:flatten( lists:map(fun(N) -> lists:sublist(binary:bin_to_list(" ++ VarName ++
                "),N*" ++ get_size_of_base_type(T) ++ " div 8," ++ get_size_of_base_type(T) ++
                " div 8) end,lists:seq(1," ++ L ++ ")))";
        false ->
            VarName
    end;
type_code(output, VarName, {_, bool}) ->
    "case "++VarName++" of 0 -> false; 1 -> true end";
type_code(deserialize, VarName, {_, bool}) ->
    VarName++":8/little";
type_code(_, VarName, {_, bool}) ->
    "(case "++VarName++" of true -> 1; false -> 0 end):8/little";
type_code(output, VarName, {_, char}) ->
    VarName;
type_code(_, VarName, {_, char}) ->
    VarName ++ ":8/little";
type_code(output, VarName, {_, uint8}) ->
    VarName;
type_code(_, VarName, {_, uint8}) ->
    VarName ++ ":8/little";
type_code(output, VarName, {_, uint16}) ->
    VarName;
type_code(_, VarName, {_, uint16}) ->
    VarName ++ ":16/little";
type_code(output, VarName, {_, uint32}) ->
    VarName;
type_code(_, VarName, {_, uint32}) ->
    VarName ++ ":32/little";
type_code(output, VarName, {_, uint64}) ->
    VarName;
type_code(_, VarName, {_, uint64}) ->
    VarName ++ ":64/little";
type_code(output, VarName, {_, int8}) ->
    VarName;
type_code(_, VarName, {_, int8}) ->
    VarName ++ ":8/signed-little";
type_code(output, VarName, {_, int16}) ->
    VarName;
type_code(_, VarName, {_, int16}) ->
    VarName ++ ":16/signed-little";
type_code(output, VarName, {_, int32}) ->
    VarName;
type_code(_, VarName, {_, int32}) ->
    VarName ++ ":32/signed-little";
type_code(output, VarName, {_, int64}) ->
    VarName;
type_code(_, VarName, {_, int64}) ->
    VarName ++ ":64/signed-little";
type_code(output, VarName, {_, float32}) ->
    VarName;
type_code(_, VarName, {_, float32}) ->
    VarName ++ ":32/float-little";
type_code(output, VarName, {_, float64}) ->
    VarName;
type_code(_, VarName, {_, float64}) ->
    VarName ++ ":64/float-little";
type_code(serialize, VarName, {_, string}) ->
    "(length(" ++ VarName ++ ")+1):32/little, (list_to_binary(" ++ VarName ++"))/binary, 0";
type_code(deserialize, VarName, {_, string}) ->
    VarName ++ "_L:32/little, " ++ VarName ++ ":(" ++ VarName ++ "_L-1)/binary,_:1/binary";
type_code(output, VarName, {_, string}) ->
    "binary_to_list(" ++ VarName ++ ")";
type_code(serialize, VarName, {Pkg, USER_TYPE}) ->
    "(" ++ Pkg ++ "_" ++ file_name_to_interface_name(USER_TYPE) ++ "_msg:serialize(" ++ VarName ++"))/binary";
type_code(deserialize, VarName, {Pkg, USER_TYPE}) ->
    VarName ++ ":(?" ++ Pkg ++ "_" ++ USER_TYPE ++ "_bitsize)";
type_code(output, VarName, {_, _USER_TYPE}) ->
    VarName.

parse_code(VarName, {array, {Pkg, T}, any}, Index) ->
    case lists:member(T, ?ROS2_STATIC_PRIMITIVES) of
        true ->
            "<< _:"++?CDR_ALIGNEMENT_CODE(?PAYLOAD, Index-1, get_size_of_base_type(uint32))++"," 
            ++ type_code(deserialize, VarName, {array, {Pkg, T}, any}) ++ "," ?PAYLOAD ++
            integer_to_list(Index) ++ "/binary>> =" ?PAYLOAD ++ integer_to_list(Index - 1);
        false ->
            "<< _:"++?CDR_ALIGNEMENT_CODE(?PAYLOAD, Index-1, get_size_of_base_type(uint32))++"," ++ VarName ++ "_L:32/little, Array_" ++ integer_to_list(Index) ++
                "/binary>> =" ?PAYLOAD ++ integer_to_list(Index - 1) ++ ",\n\t" ++
                "{" ++ VarName ++ "," ?PAYLOAD ++ integer_to_list(Index) ++ "} = parse_n_times(" ++
                case T  of string -> "string, "; _ ->  Pkg ++ "_" ++ file_name_to_interface_name(T) ++ "_msg, " end
                ++ VarName ++ "_L, "++" _CDR_offset + bit_size("?PAYLOAD"0) - bit_size(Array_" ++ integer_to_list(Index) ++ "), "
                "Array_" ++ integer_to_list(Index) ++ ")"
    end;
parse_code(VarName, {array, {Pkg, T}, L}, Index) ->
    case lists:member(T, ?ROS2_STATIC_PRIMITIVES) of
        true ->
            "<< _:"++?CDR_ALIGNEMENT_CODE(?PAYLOAD, Index-1, get_size_of_base_type(T))++"," 
            ++ type_code(deserialize, VarName, {array, {Pkg, T}, L}) ++ "," ?PAYLOAD ++
            integer_to_list(Index) ++ "/binary>> =" ?PAYLOAD ++ integer_to_list(Index - 1);
        false ->
            "{" ++ VarName ++ "," ?PAYLOAD ++ integer_to_list(Index) ++ "} = parse_n_times(" ++
            case T  of string -> "string, "; _ ->  Pkg ++ "_" ++ file_name_to_interface_name(T) ++ "_msg, " end
            ++ L ++
            ", _CDR_offset" ++ case Index > 1 of 
                        true -> " + bit_size("?PAYLOAD"0) - bit_size("?PAYLOAD ++ integer_to_list(Index - 1) ++ ")";
                        false -> "" end ++
            "," ?PAYLOAD ++ integer_to_list(Index - 1) ++ ")"
    end;
parse_code(VarName, {Pkg, T}, Index) ->
    case lists:member(T, ?ROS2_PRIMITIVES) of
        true ->
            "<< _:"++?CDR_ALIGNEMENT_CODE(?PAYLOAD, Index-1, get_size_of_base_type(T))++"," 
            ++ type_code(deserialize, VarName, {Pkg, T}) ++ 
            ","?PAYLOAD ++ integer_to_list(Index) ++"/binary>> = " ?PAYLOAD ++ integer_to_list(Index - 1);
        false ->
            "{" ++ VarName ++ "," ?PAYLOAD ++ integer_to_list(Index) ++ "} = " ++ Pkg ++ "_" ++
                file_name_to_interface_name(T) ++ "_msg:parse( _CDR_offset " 
                ++ case Index > 1 of 
                    true -> " + bit_size("?PAYLOAD"0) - bit_size("?PAYLOAD ++ integer_to_list(Index-1) ++ ")";
                    false -> "" end ++
                "," ?PAYLOAD ++integer_to_list(Index - 1) ++ ")"
    end.

serialization_code(VarName, {array, {Pkg, T}, any}, Index) ->
    case lists:member(T, ?ROS2_STATIC_PRIMITIVES) of
        true ->
            "Array_" ++integer_to_list(Index)++ " = "
            "<<" ?PAYLOAD ++ integer_to_list(Index - 1)++"/binary, 0:"++?CDR_ALIGNEMENT_CODE(?PAYLOAD++integer_to_list(Index-1), get_size_of_base_type(T))++", "
            "(length("++VarName++")):32/little>>,\n\t" 
            ?PAYLOAD ++integer_to_list(Index)++ " = << Array_" ++integer_to_list(Index)++"/binary, 0:"?CDR_ALIGNEMENT_CODE(?PAYLOAD++integer_to_list(Index-1), get_size_of_base_type(T))++", "
            ++ type_code(serialize, VarName, {array, {Pkg, T}, any}) ++ ">>";
        false ->
            ?PAYLOAD ++integer_to_list(Index)++ " = " ++" serialize_array(" ++
            case T of string -> "string, "; _ ->  Pkg ++ "_" ++ file_name_to_interface_name(T) ++ "_msg, " end ++ "\n\t\t"
            ++ "<<" ?PAYLOAD ++ integer_to_list(Index - 1)++"/binary, 0:"?CDR_ALIGNEMENT_CODE(?PAYLOAD++integer_to_list(Index-1), get_size_of_base_type(uint32))++", "
            "(length("++VarName++")):32/little>>,\n\t\t" ++ VarName ++ ")"
    end;  
serialization_code(VarName, {array, {Pkg, T}, L}, Index) ->
    case lists:member(T, ?ROS2_STATIC_PRIMITIVES) of
        true ->
            ?PAYLOAD ++integer_to_list(Index)++ " = <<" ?PAYLOAD ++ integer_to_list(Index-1)++"/binary, 0:"?CDR_ALIGNEMENT_CODE(?PAYLOAD++integer_to_list(Index-1), get_size_of_base_type(T))++", "
            ++ type_code(serialize, VarName, {array, {Pkg, T}, L}) ++ ">>";
        false ->             
            ?PAYLOAD ++integer_to_list(Index)++ " = serialize_array(" ++
            case T of string -> "string, "; _ ->  Pkg ++ "_" ++ file_name_to_interface_name(T) ++ "_msg, " end ++
            ?PAYLOAD ++integer_to_list(Index-1)++"/binary, " ++ VarName ++ ")"
    end;
serialization_code(VarName, {Pkg, T}, Index) ->
    case lists:member(T, ?ROS2_PRIMITIVES) of
        true ->
            ?PAYLOAD ++ integer_to_list(Index) ++" = <<" ?PAYLOAD ++ integer_to_list(Index-1) ++"/binary, 0:"?CDR_ALIGNEMENT_CODE(?PAYLOAD++integer_to_list(Index-1), get_size_of_base_type(T))++"," 
            ++ type_code(serialize, VarName, {Pkg, T}) ++ ">>";
        false ->
            ?PAYLOAD ++ integer_to_list(Index) ++ " = " ++ Pkg ++ "_" ++
                file_name_to_interface_name(T) ++ "_msg:serialize("?PAYLOAD ++ integer_to_list(Index-1) ++ ", "++VarName++")"
    end.


items_contain_usertyped_arrays(Items) ->
    lists:any(
        fun
            ({{{type, T}, {array, _}}, _}) ->
                not lists:member(T, ?ROS2_STATIC_PRIMITIVES);
            (_) ->
                false
        end,
        Items
    ).

items_need_dinamic_bin_split(Items) ->
    lists:any(
        fun
            ({{{type, T}, {array, any}}, _}) ->
                lists:member(T, ?ROS2_STATIC_PRIMITIVES);
            (_) ->
                false
        end,
        Items
    ).
