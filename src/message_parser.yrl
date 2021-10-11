Nonterminals message element elements.

Terminals type name array macro assignement value.

Rootsymbol message.


element -> type name value: {'$1', {'$2','$3'}}.
element -> type name : {'$1','$2'}.
element -> type array name : { {'$1','$2'}, '$3'}.
element -> type macro assignement value : { '$1', '$2', '$4'}.
elements -> element : ['$1'].
elements -> element elements : ['$1'] ++ '$2'.


message -> elements : split_macros_from_fields('$1').


Erlang code.

split_macros_from_fields(Elements) ->
        {[E || {_,_,_}=E <- Elements], [E || {_,_}=E <- Elements]}.
