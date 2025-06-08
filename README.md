# espyscript

as with evscript, this is stylized as lowercase!
do not capitalize the e!

## goals

- simple to implement
- performant and compilable
- statically typed
- functional
- sandboxed
- extensible

## non-goals

- lua familiarity/compatibility
- borrow semantics
- garbage collection
- "eval" (though runtimes are free to provide it)

## lexing

the espy-eyes crate handles lexing and the espy-ears crate handles parsing.
both are lalr-only (`next` and `peek`).

the espy-eyes lexer produces tokens with an "origin" string and a "lexigram".
for many tokens the "origin" string is a known value,
but the address and length of its string slice implies the position of the token.
identifiers and numbers are the exception to this;
their origin string is the identifier or number associated with them, respectively.

refer to espy-eyes for a complete list of lexigrams and reserved symbols.

## parsing

the parser consists of two major contexts: the expression and the block.
expressions define a sequence of operations that produce a value,
while blocks are capable of binding expressions to identifiers and disrupting control flow.
an espy script should be interpreted as a "block" at its top level.

a block may be turned into a function using the `with` keyword.

```
let add = {
  with x, y;
  x + y
};

add(2, 3) == 5
```

since an espy script is just a block,
the only way for the runtime to provide values to it is by calling a function it returns.
the `with` syntax makes this a little more convenient than it might be in other languages,
since creating a function doesn't require any indentation:

```
#!/usr/bin/env espyscript

with std;

std.print "hello, world!";
```

all bindings in the block that existed before the `with` statement continue to be bound within the resulting function.

```
let closure = {
  let x = 1 + 2;
  with y;
  x * y
};

closure 5 == 15
```
