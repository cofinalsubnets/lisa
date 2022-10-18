## stop using tagged pointers

this is good for portability (32-bit hardware) and is needed
to prevent stack overflows during garbage collection (see
below). distinguishing pointers from immediate values with
the least significant bit is probably ok.

## avoid recursion in C

we need this to stop stack overflows, which we can't handle
safely in a portable way. the number one culprit here is
garbage collection. using cheney's algorithm would
fix it, but to do that we would need to be able to collect
an object from an untagged pointer. the bootstrap compiler
is also recursive, but that's acceptable since we only need
it to handle the stage 2 compiler. the parser avoids recursion
by using the internal stack.

## semispace gc

this will minimize calls to the block allocator and ensure we
can always recover from OOM.

## compile options for embedded

at a minimum this means adding an option for statically allocated
memory blocks.

## gc statistics

## benchmarks

