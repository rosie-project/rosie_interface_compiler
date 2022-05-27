-ifndef(COMPILER_MACROS_HRL).

-define(COMPILER_MACROS_HRL, true).
-define(GEN_CODE_DIR, "_rosie").
-define(ROS2_STATIC_PRIMITIVES,
        [bool,
         byte,
         char,
         float32,
         float64,
         int8,
         uint8,
         int16,
         uint16,
         int32,
         uint32,
         int64,
         uint64]).

-define(PAYLOAD, "_Payload_").

-define(ROS2_PRIMITIVES, [string | ?ROS2_STATIC_PRIMITIVES]).

-define(CDR_ALIGNEMENT_CODE(Bin,N,NumBits), 
        case N >= 1 of 
                true ->"(("++NumBits++" - (( _CDR_offset + bit_size("?PAYLOAD"0) - bit_size("++Bin++integer_to_list(N)++")) rem "++NumBits++")) rem "++NumBits++")";
                false ->"(("++NumBits++" - ( _CDR_offset rem "++NumBits++")) rem "++NumBits++")"
        end).

-define(CDR_ALIGNEMENT_CODE(Bin, NumBits), 
        "(("++NumBits++" - (  bit_size("++Bin++")  rem "++NumBits++")) rem "++NumBits++")").

-define(CDR_ALIGNEMENT_CODE(NumBits), 
        "(("++NumBits++" - ( _CDR_offset  rem "++NumBits++")) rem "++NumBits++")").

-define(BIN_TO_BIN_LIST_CODE,
        "% When there is an array of elements of base types is usefull to break the binary in a list of sub binaries
break_binary(Bin,Length,TypeL) ->
        lists:map( fun erlang:list_to_binary/1, lists:map(fun(N) -> lists:sublist(binary:bin_to_list(Bin),N*TypeL div 8 - (TypeL div 8 - 1), TypeL div 8) end,lists:seq(1,Length))).
").

-define(SERIALIZE_ARRAY_CODE, 
"serialize_array(_, Payload, []) -> 
        Payload;
% string special case
serialize_array(string, Payload, [STR|List]) ->
        NewPayload = <<Payload/binary, 0:"++?CDR_ALIGNEMENT_CODE("Payload","32")++",(length(STR)+1):32/little, (list_to_binary(STR))/binary, 0>>,
        serialize_array(string, NewPayload, List);
serialize_array(Module, Payload, [Obj|List]) -> 
        NewPayload = Module:serialize(Payload,Obj),
        serialize_array(Module, NewPayload, List).
"
).

-define(PARSE_N_TIMES_CODE,
        "% The payload is parsed a number of Times with the specified Module, all the binary left is returned.
% this function is used to parse binary arrays into erlang lists
parse_n_times(_, 0, _, Payload, List) -> 
        {lists:reverse(List), Payload};
% string special case
parse_n_times(string, Times, _CDR_offset, Payload, List) -> 
        << _:((32 - (_CDR_offset rem 32)) rem 32), L:32/little, STR:(L-1)/binary,0:8,REST/binary>> = Payload,
        parse_n_times(string, Times-1, _CDR_offset + (bit_size(Payload) - bit_size(REST)), REST, [binary:bin_to_list(STR)|List]);
parse_n_times(Module, Times, _CDR_offset, Payload, List) -> 
        {Obj, REST} = Module:parse(_CDR_offset, Payload),
        parse_n_times(Module, Times-1, _CDR_offset + (bit_size(Payload) - bit_size(REST)), REST, [Obj|List]).
parse_n_times(Module, Times, _CDR_offset, Payload) -> 
        parse_n_times(Module, Times, _CDR_offset, Payload, []).
").

-endif.
