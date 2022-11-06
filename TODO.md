# TODO

## missing user features
- floating point
- bignums
- namespaces / module system
- shell/os/general scripting functions; "backticks" like bash/perl/ruby
- sprintf
- quasiquotes
- hash/array literals
- richer string syntax (delimiters, escapes, interpolation)
- dynamic arrays
- continuations
- exception-like error handling (or at least protect eval somehow)
- file/network i/o (with async)
- explicit locale/encoding support
- C API

## build options
- as dynamic/static library
- without malloc/stdio/libc

## test on more platforms
- 32-bit x86/ARM/RISC-V
- ESP32

## benchmarks
this is in git somewhere, but it's a ruby script.
we should add enough scripting functionality to do
it natively.

## compiler improvements
- static type inference & checking
- function inlining

## GC improvements
### finalizers
pretty simple (in git somewhere); we need this to
clean up i/o streams & enable memory management
of user data.

### semispace GC
this is the "traditional" way to do copying GC. it will
minimize calls to the block allocator, ensure we can
always recover from OOM, and give us a free block of
memory we can use for heap-bounded non-allocating
recursive operations so they can't overflow the stack.

### cheney's algorithm
should be simple now that tagged pointers are out.
split up copy function into evac/walk & trampoline so
as not to use C stack.

### collect GC statistics
number of cycles, average/extreme latency, average memory
usage ...

### generational GC
this might not be worth it in the end. we need benchmarks
to tell!

## other runtime improvements
- parse from strings (not FILE\*s)
- closure pointer on stack (instead of in frame)
- improve hashing of mutable data
- use hash table for internal symbols
- remove recursion from eq, tx, hash (use off semispace)
- reverse stack/heap for left-to-right argument evaluation
