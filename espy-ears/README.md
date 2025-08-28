## espy-ears (parser)

the parser consists of two major contexts: the expression and the block.
expressions define a sequence of operations that produce a value,
while blocks are capable of binding expressions to identifiers and disrupting control flow.
an espy program should be interpreted as a "block" at its top level.

a block may be turned into a function using the `with` keyword.

```espy
let add = {
  with x, y;
  x + y
};

add(2, 3) == 5
```

since an espy program is just a block,
the only way for the runtime to provide values to it is by calling a function it returns.
the `with` syntax makes this a little more convenient than it might be in other languages,
since creating a function doesn't require any indentation:

```espy
#!/usr/bin/env espy

with std;

std.print "hello, world!";
```

all bindings in the block that existed before the `with` statement continue to be bound within the resulting function.

```espy
let closure = {
  let x = 1 + 2;
  with y;
  x * y
};

closure 5 == 15
```
