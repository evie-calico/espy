# espyscript

as with evscript, this is stylized as lowercase!
do not capitalize the e!

## goals

- simple to implement
- performant and compilable
- static typing[*](#a-note-on-static-typing)
- functional
- sandboxed
- extensible

## non-goals

- lua familiarity/compatibility
- borrow semantics
- garbage collection
- "eval" (though runtimes are free to provide it)

## a note on static typing

type inference and strong, static typing imply a more expensive compilation step,
which is sometimes undesirable for a scripting language.
because of this, espyscript is designed to be both dynamically and statically typed.
static typing is provided through an additional, optional compiler pass,
which can be used for precompilation, linting, or optimizing espyscript code at runtime.

this has limitations:
type inference cannot have any semantic implications
—such as changing the types of numeric constants—
because the behavior of a script would change if the runtime applied type checking.
i don't want the language to have integer promotion rules,
so this means we're stuck with just i64s and f64s.

on the other hand, it enables certain optimizations.
for example,
string indexing of named tuples may be optimized away to integer indexing when the type is known
(and named tuples support both forms of indexing to facilitate this).

i think this is a reasonable compromise:
users compiling espyscript code immediately before running it may skip the type checking step
to reduce compilation time without any semantic change to the code,
espyscript files may be linted outside of their runtime environment before this happens,
should the programmer desire static guarantees,
and precompiled espyscript bytecode may have optimizations applied to it
—even if the runtime does not perform them.

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
