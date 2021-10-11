Nonterminals service reply request element elements.

Terminals type name array separator macro assignement value.

Rootsymbol service.

element -> type name value : { '$1',{'$2', '$3'}}.
element -> type array name : { {'$1','$2'}, '$3'}.
element -> type name : {'$1','$2'}.
element -> type macro assignement value : { '$1', '$2', '$4'}.
elements -> element : ['$1'].
elements -> element elements : ['$1'] ++ '$2'.


request -> elements : '$1'.
reply -> separator elements : '$2'.

service -> request reply : split_macros_from_fields('$1', '$2').
service -> reply : split_macros_from_fields([], '$1').
service -> request : split_macros_from_fields('$1', []).

Erlang code.


split_macros_from_fields(RequestElem, ReplyElem) ->
        {[E || {_,_,_}=E <- ReplyElem]++[E || {_,_,_}=E <- RequestElem], 
        [E || {_,_}=E <- RequestElem],
        [E || {_,_}=E <- ReplyElem]}.
