Nonterminals message element elements.

Terminals type name.

Rootsymbol message.

element -> type name : {'$1','$2'}.
elements -> element : ['$1'].
elements -> element elements : ['$1'] ++ '$2'.

message -> elements : {'$1'}.

Erlang code.
