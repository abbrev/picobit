#include <stdio.h>
#include <stdlib.h>

#include <picobit.h>
#include <dispatch.h>
#include <gc.h>

#include <avr/pgmspace.h>

uint8_t ram_mem[RAM_BYTES + VEC_BYTES];

void halt_with_error ()
{
	while(1);
}


int main (int argc, char *argv[])
{
	// TODO make this work on Arduino!
	// TODO initialize stuff here

	if (rom_get (CODE_START+0) == 0xfb &&
	    rom_get (CODE_START+1) == 0xd7) {
		interpreter ();
	}

	return 0;
}
