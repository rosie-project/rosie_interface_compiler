
Definitions.

Rules.

int64 : {token, {type, int64}}. % TokenLine for line
float32 : {token, {type, float32}}.
float64 : {token, {type, float64}}.
string : {token, {type, string}}.
[A-Z][A-Za-z0-9]* : {token, {type, TokenChars}}. % THIS is a user defined type
[\-]+ : {token, {separator}}.
[a-z_]+ : {token, {name, TokenChars}}.
#.*\r?\n : skip_token.
[\s\t\n\r]+ : skip_token.
[.]+ : {error, syntax}.

Erlang code.
