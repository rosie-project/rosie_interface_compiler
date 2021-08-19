-ifndef(COMPILER_MACROS_HRL).
-define(COMPILER_MACROS_HRL, true).


-define(GEN_CODE_DIR, "_rosie").

-define(ROS2_PRIMITIVES, [
    bool,
    byte,
    char,
    float32, float64,
    int8, uint8,
    int16, uint16,
    int32, uint32,
    int64, uint64,
    string
]).

-endif.