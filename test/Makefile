bin=../build/lisa.dbg
boot=../lib/boot.la
cmd=$(bin) -_ $(boot) *.la

test: $(bin)
	/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time $(cmd)"

# run the tests a lot of times to try and catch nondeterministic bugs :(
lots: $(bin)
	for n in {1..2048}; do $(cmd) || exit 1; done
