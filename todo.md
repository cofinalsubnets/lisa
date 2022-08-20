## stop using tagged pointers

this is good for portability and is needed to prevent
stack overflows during garbage collection (see below).
distinguishing pointers and immediate values using 1 bit
is acceptable. that means we can run on 16-bit hardware
in principle.

## avoid recursion in C

we need this to preclude stack overflows, which we can't
handle safely in a portable way. the number one culprit
here is garbage collection. using cheney's algorithm would
fix it, but to do that we would need to be able to collect
an object from an untagged pointer. the parser and bootstrap
compiler are also recursive, but are much less likely to
overflow the stack. still, fixing the parser is be easy, so
we might as well.

