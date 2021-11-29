
Definitions.

Rules.


\[[0-9]+\] : {token, {array, string:trim(TokenChars,both,[$[,$]])}}.
\[<=[0-9]+\] : {token, {array, any}}. % limited arrays are treated as undefined ones
\[\] : {token, {array, any}}.
bool : {token, {type, bool}}. % bool is same as uint8
byte : {token, {type, char}}. % byte is same as char
char : {token, {type, char}}.
uint8 : {token, {type, uint8}}.
uint16 : {token, {type, uint16}}.
uint32 : {token, {type, uint32}}.
uint64 : {token, {type, uint64}}.
int8 : {token, {type, int8}}.
int16 : {token, {type, int16}}.
int32 : {token, {type, int32}}.
int64 : {token, {type, int64}}.
float32 : {token, {type, float32}}.
float64 : {token, {type, float64}}.
string : {token, {type, string}}.
wstring : {token, {type, string}}.
[A-Z_]+ : {token, {macro, TokenChars}}.
string<=[0-9]+ : {token, {type, string}}. % bound string is treated as an infinite one for simplicity
[A-Z][A-Za-z0-9]* : {token, {type, TokenChars}}. % User defined type
[a-z_]+/[A-Z][A-Za-z0-9]* : {token, {type, split_pkg_and_type(TokenChars)}}. % User defined type in an external pkg
= :  {token, {assignement}}.
-?[0-9]+\.?[0-9]* : {token,{value, TokenChars}}.
\".*\" : {token,{value, TokenChars}}.
false : {token,{value, "false"}}.
true : {token,{value, "true"}}.
--- : {token, {separator}}.
[a-z][a-zA-Z0-9_]*+ : {token, {name, TokenChars}}.
#.* : skip_token. % Comments
[\s\t\n\r]+ : skip_token.
[.]+ : {error, syntax}.

Erlang code.


split_pkg_and_type(S) -> 
        [P,T] = string:split(S,"/"),
        {P,T}.

