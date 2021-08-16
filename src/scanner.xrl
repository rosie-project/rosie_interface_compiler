
Definitions.

Rules.

int64 : {token, {type, int64}}. % TokenLine for line
float32 : {token, {type, float32}}.
string : {token, {type, string}}.
[\-]+ : {token, {separator}}.
[a-z]+ : {token, {name, TokenChars}}.
#.*\r?\n : skip_token.
[\s\t\n]+ : skip_token.
[.]+ : {error, syntax}.

Erlang code.
