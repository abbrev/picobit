all: compiler vm

# compiler can always be run, raco make will figure the rest out
.PHONY: compiler vm

compiler: vm/.primitives.p compiler/gen.config.rkt compiler/gen.library.scm
	raco make compiler/picobit.rkt

vm/.primitives.p:
	make -C vm .primitives.p

compiler/gen.config.rkt:
	make -C vm ../compiler/gen.config.rkt

compiler/gen.library.scm:
	make -C vm ../compiler/gen.library.scm

vm:
	make -C vm
	[ -e vm/picobit-vm ] && cp vm/picobit-vm . || rm -f picobit-vm

clean:
	make -C vm clean
	rm -rf compiler/compiled

test: compiler vm
	raco make tests/run-tests.rkt
	racket tests/run-tests.rkt
