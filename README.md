# espy

the espy crate is the primary library for end users, and provides a simple api
for parsing, compiling, and executing espy. refer to the individual component
crates' README.md files for implementation details; this document will describe
the espy language.

- [espy-eyes](espy-eyes/README.md) (lexer)
- [espy-ears](espy-ears/README.md) (parser)
- [espy-heart](espy-heart/README.md) (bytecode)
- [espy-tail](espy-tail/README.md) (compiler)
- [espy-paws](espy-paws/README.md) (interpreter)

## the espy language

espy is a functional scripting language designed to be extended by
interoperability with its host environments. it should feel similar to an
expression-based lua without a reliance on tables (instead using named tuples
for ad-hoc structured data).

as a functional language, espy diverges from lua in a few ways:
- all variables are immutable by default
- there are no global variables in the language
  - this allows espy to be easily sandboxed
- instead of looping primatives (like `for` and `while`), espy uses iterators.

### functions

the syntax for invoking functions in espy is different from c-style languages
like lua (and should look very familiar to ml/lisp programmers): any sequence of
two values not separated by an operator is considered a function call. function
calls have a very low precendence, meaning parenthesis are not required around
higher-precedence expressions:

```espy
print "hello, world!"; # hello, world!
add 1, 2; # 3
square 4 + 4 # 16
```

it's worth noting that this means the rules around function precedence may be
different from what you expect. writing `print concat "hello, ", "world!"` will
print the function `concat` and then complain that `()` (unit) cannot be called
with a tuple of strings. in order to fix this, parenthesis must surround the
function *and* its arguments:

```espy
print (concat "hello, ", "world!")
```

### tuples

the use of `,` to separate arguments may be unfamiliar to programmers used
to other functional programming languages. like other functional programming
languages, espy functions take exactly one argument and return exactly one
value. the `,` in the above examples is an operator that concatenates its
operands into a tuple, which the function then destructures in order to use it
in a way that looks like multiple arguments.

```espy
let x = 1;
let y = "foo";
let z = 3;

# concatenation
let xyz = x, y, z;

# indexing
print xyz.1 # foo

# destructuring
let (one, foo, three) = xyz;
```

the comma operator accepts unit, singles, and tuples. two singles obvious
produce a tuple of two values, while concatenating two tuples produces a new
tuple with their values in order (not nested). unit is a special case: it
behaves as if it was an empty tuple, returning the other operand with no change.

named tuples may be constructed using the `:` operator. on the left side of
the operator is the name, and on the right the value. this constructs a tuple
containing a single value, which can be composed into a larger tuple via the
comma operator.

```espy
let fur_patterns = sugar: "tuxedo", evie: "calico";

print fur_patterns.sugar; # tuxedo
# indexing via integers is also supported.
print fur_patterns.1; # calico
```

### pipes

the idiomatic way to pass multiple arguments in espy is to pass them as a
tuple rather than relying on currying. because of this, the pipe operator uses
tuple concatenation rules to compose an argument pipeline. the function is only
invoked when the typical call syntax is used on the result of the pipe operator,
though because concatenating with unit is a no-op, you can still specify all of
your arguments on the "left side" of the function.

```espy
"hello, world" |> print ();

# the result of a pipe can be bound to a variable and reused.
let add4 = 4 |> add;

add4 3; # 7
add4 28; # 32
```

### functions

espy functions may be declared in any block using the `with` statement.

```espy
let f = {
  with x;
  x * x
};
```

any values bound to the current scope (but not the scope's parent) are captured by the function.

```espy
let add4 = {
  let four = 4;
  with x;
  x + four
};

let eleven = add4 7;

let sub11 = {
  # this moves eleven into the current scope,
  # making it captured.
  let eleven = eleven;
  with x;
  x - eleven
};

sub11 23; # 12
```

because espy is largely "pure" and does not have global variables, libraries
may only be provided to an espy program by passing them as function arguments
from the host environment. this means that, practically, every single espyscript
program is a single function which is immediately returned to the host. in most
languages this would mean the entirety of the program has to be indented in a
block, so espy's `with` syntax is designed to reuse the top-level block for the
body of its inevitable function.

```espy
with host;

host.print "hi, host?";
```

## the espy runtime

espy programs maintain a minimal runtime environment, consisting only of
a short-lived stack and reference-counted pointers. there is no garbage
collection, and no state is preserved between function calls except the
function's input and output. this makes sandboxing espy trivial, unlike lua
which typically *depends* on the use of global variables for libraries and such.

because espy has no chance of storing variables long-term, interoperability with
a host rust program can be acheived using *borrowed values*--as far as i know
espy is the only scripting language for rust to allow this.

espy's memory management is entirely deterministic as well. all allocations are
made though reference-counted pointers, so there's no garbage collection pause
or leftover memory that could otherwise be freed.

## a note on static typing

type inference and strong, static typing imply a more expensive compilation
step, which is sometimes undesirable for a scripting language. because of this,
espy is designed to be both dynamically and statically typed. static typing is
provided through an additional, optional compiler pass, which can be used for
precompilation, linting, or optimizing espy code at runtime.

this has limitations: type inference cannot have any semantic implications, such
as changing the types of numeric constants, because the behavior of a script
would change if the runtime applied type checking. i don't want the language to
have integer promotion rules, so this means we're stuck with just i64s and f64s.

on the other hand, it enables certain optimizations. for example, string
indexing of named tuples may be optimized away to integer indexing when the type
is known (and named tuples support both forms of indexing to facilitate this).

i think this is a reasonable compromise: users compiling espy code immediately
before running it may skip the type checking step to reduce compilation time
without any semantic change to the code, espy programs may be linted outside
of their runtime environment before this happens should the programmer desire
static guarantees, and precompiled espy bytecode may have optimizations applied
to it, even if the host otherwise does not perform optimizations on programs
compiled at runtime.

it's also worth noting that this is subject to change as the language develops.
if introducing static typing doesn't have a serious impact on bytecode
compilation speeds then i'll try to have a fully-static type system. i'm
inclined to do so anyways for the sake of jit compilation.
