# TODO

## misc runtime tasks
- hash table for internal symbols
- don't use so many data threads (eg. for hash cells/bucket arrays)
- closure pointer on stack (instead of in frame)
- semispace gc
- cheney's algorithm
- remove recursion from eq, tx, hash
- parse from strings (not io handles)
- improve hashing of mutable data (if possible)
- reverse stack/heap for left-to-right argument evaluation
- add GC finalizers (to release file handles etc.)

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

## platform stuff
- as dynamic/static library
- freestanding
- test on 32-bit x86/ARM/RISC-V
- build for ESP32

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
this is probably desirable despite the extra complexity of
having a write barrier because it will greatly improve copy
scaling and hopefully make it feasible to use memory addresses
for hashing by reducing the cost of rehashes each GC cycle.
we should probably do stats/benchmarks first though!
