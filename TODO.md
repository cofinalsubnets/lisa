## remove recursion from C
either replace recursive routines with nonrecursive
versions, or move the recursion onto the managed stack.
this is the only portable way to handle stack overflows.

- gc (use cheney's algorithm)
- eql (recursive on pairs; use off semispace)
- tx (recursive on pairs; use off semispace)
- hash (recursive on pairs; use off semispace)

## stop using null-terminated strings
we already store the length so this is mostly a matter of
switching up C library functions.

## benchmarks
this is in the git history somewhere, but it's a ruby
script. we should add enough scripting functionality to do
it natively.

## missing functionality
- dynamic arrays
- continuations 
- error handling
- finalizers
- shell functions
- file & network i/o
- more string escape sequences
- hash literals
- quasiquotes
- sprintf

## better hashing
right now hashing performance on functions and esp. hash
tables is very poor. also the hashing algorithm is totally
ad hoc and untested.

## use hash table for internal symbols
instead of the current binary tree.

## design C API & build as a dynamic library
and link with main executable. this also means choosing a
client API.

## static type inference / checking
use k2 code for this.

## semispace GC
this is the "traditional" way to do copying GC. it will
minimize calls to the block allocator and ensure we can
always recover from OOM.

## embedded compile options
at a minimum this means adding an option for statically
allocated memory blocks.

## configurable memory scaling
it would make sense to do this after adding gc stats and
benchmarks. fibonacci numbers would give a gentler memory
curve than powers of 2.

## collect GC statistics
number of cycles, average/extreme latency, average memory
usage ...
