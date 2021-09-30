
Definitions.

Rules.


\[[0-9]+\] : {token, {array, string:trim(TokenChars,both,[$[,$]])}}.
\[\] : {token, {array, any}}.
char : {token, {type, char}}.
uint8 : {token, {type, uint8}}.
uint32 : {token, {type, uint32}}.
int8 : {token, {type, int8}}.
int32 : {token, {type, int32}}.
int64 : {token, {type, int64}}.
float32 : {token, {type, float32}}.
float64 : {token, {type, float64}}.
string : {token, {type, string}}.
string<=[0-9]+ : {token, {type, string}}. % bound string is treated as an infinite one for simplicity
[A-Z][A-Za-z0-9]* : {token, {type, TokenChars}}. % User defined type
[a-z_]+/[A-Z][A-Za-z0-9]* : {token, {type, split_pkg_and_type(TokenChars)}}. % User defined type in an external pkg
--- : {token, {separator}}.
% [a-z_]+/ : skip_token. % prefixes like packages specified before a msg type are ignored
[a-z_]+ : {token, {name, TokenChars}}.
#.* : skip_token. % Comments
[\s\t\n\r]+ : skip_token.
[.]+ : {error, syntax}.

Erlang code.


split_pkg_and_type(S) -> 
        [P,T] = string:split(S,"/"),
        {P,T}.

