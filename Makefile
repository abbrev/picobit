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

%.hex: %.scm compiler
	./picobit $<

%.bin: %.hex
	objcopy -I ihex -O binary $< $@

ifeq ($(SCM_FILE),)
SCM_FILE := app.scm
endif

vm:
	@if [ -z "$(SCM_FILE)" ]; then echo >&2 "Please specify SCM_FILE!"; false; fi
	make $(SCM_FILE:scm=bin)
	xxd -i <$(SCM_FILE:scm=bin) >vm/arch/arduino/rom_mem.hex
	make -C vm
	[ -e vm/picobit-vm ] && cp vm/picobit-vm . || rm -f picobit-vm

clean:
	make -C vm clean
	rm -rf compiler/compiled

test: compiler vm
	raco make tests/run-tests.rkt
	racket tests/run-tests.rkt
