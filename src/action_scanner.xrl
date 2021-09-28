
Definitions.

Rules.


#.*\r?\n : skip_token.
[\s\t\n\r]+ : skip_token.
[a-z0-9A-Z\[\]\s]+ : {token, {line, TokenChars}}. % any line
[\-]+ : {token, {separator}}.
[.]+ : {error, syntax}.

Erlang code.
