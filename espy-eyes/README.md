## espy-eyes (lexer)

espy-eyes produces tokens with an "origin" string and a "lexigram".
for many tokens the "origin" string is a known value,
but the address and length of its string slice also implies the position of the token.
identifiers and numbers are the exception to this;
their origin string is the identifier or number associated with them, respectively.

refer to [src/lib.rs](src/lib.rs) for a complete list of lexigrams.
