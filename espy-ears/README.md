## espy-ears (parser)

the parser consists of two major contexts: the expression and the block.
expressions define a sequence of operations that produce a value, while blocks
are capable of binding expressions to identifiers and disrupting control flow.
an espy program should be interpreted as a "block" at its top level.

block parsing is "infallible"; instead of returning `Result` types, most ast
nodes contain "diagnostics" fields to collect errors without giving up on
parsing.

