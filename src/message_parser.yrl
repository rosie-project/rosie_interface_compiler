Nonterminals message element elements.

Terminals type name array.

Rootsymbol message.


element -> type array name : { {'$1','$2'}, '$3'}.
element -> type name : {'$1','$2'}.
elements -> element : ['$1'].
elements -> element elements : ['$1'] ++ '$2'.

message -> elements : {'$1'}.

Erlang code.
