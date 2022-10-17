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
an object from an untagged pointer. the bootstrap compiler
is also recursive, but that's acceptable since we only need
it to handle the stage 2 compiler. the parser avoids recursion
by using the internal stack.

## embedded compile-time options
## gc statistics

