# espy
as with evscript, this is stylized as lowercase!
do not capitalize the e!

the espy crate is the primary library for end users,
and provides a simple API for parsing, compiling, and executing espy.
refer to the individual component crates' README.md files for implementation details;
this document will describe the espy language.

- [espy-eyes](espy-eyes/README.md) (lexer)
- [espy-ears](espy-ears/README.md) (parser)
- [espy-heart](espy-heart/README.md) (bytecode)
- [espy-tail](espy-tail/README.md) (compiler)
- [espy-paws](espy-paws/README.md) (interpreter)

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
because of this, espy is designed to be both dynamically and statically typed.
static typing is provided through an additional, optional compiler pass,
which can be used for precompilation, linting, or optimizing espy code at runtime.

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
users compiling espy code immediately before running it may skip the type checking step
to reduce compilation time without any semantic change to the code,
espy files may be linted outside of their runtime environment before this happens,
should the programmer desire static guarantees,
and precompiled espy bytecode may have optimizations applied to it
—even if the runtime does not perform them.

it's also worth noting that this could change as the language develops.
if introducing static typing doesn't have a serious impact on bytecode compilation speeds
then i'll try to have a fully-static type system.
i'm inclined to do so anyways for the sake of jit compilation.
