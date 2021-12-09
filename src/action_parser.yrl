Nonterminals action element elements.

Terminals line separator.

Rootsymbol action.

element -> line : '$1'.
elements -> element : ['$1'].
elements -> element elements : ['$1'] ++ '$2'.

action -> elements separator elements separator : {'$1', '$3', []}.
action -> elements separator elements separator elements : {'$1', '$3', '$5'}.

Erlang code.
