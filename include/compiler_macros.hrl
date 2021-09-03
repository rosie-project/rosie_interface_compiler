-ifndef(COMPILER_MACROS_HRL).
-define(COMPILER_MACROS_HRL, true).


-define(GEN_CODE_DIR, "_rosie").


-define(ROS2_STATIC_PRIMITIVES, [
    bool,
    byte,
    char,
    float32, float64,
    int8, uint8,
    int16, uint16,
    int32, uint32,
    int64, uint64
]).

-define(ROS2_PRIMITIVES, [string | ?ROS2_STATIC_PRIMITIVES]).

-define(PARSE_N_TIMES_CODE,
"% The payload is parsed a number of Times with the specified Module, all the binary left is returned.
% this function is used to parse binary arrays into erlang lists
parse_n_times(_, 0, Payload, List) -> 
        {lists:reverse(List), Payload};
% string special case
parse_n_times(string, Times, Payload, List) -> 
        <<L:32/little, STR:(L-1)/binary,_:(4 -(L-1) rem 4)/binary,REST/binary>> = Payload,
        parse_n_times(string, Times-1, REST, [STR|List]);
parse_n_times(Module, Times, Payload, List) -> 
        {Obj, REST} = Module:parse(Payload),
        parse_n_times(Module, Times-1, REST, [Obj|List]).
parse_n_times(Module, Times, Payload) -> 
        parse_n_times(Module, Times, Payload, []).
").

-endif.