#include <picobit.h>
#include <bignum.h>
#include <debug.h>
#include <stdio.h>

void show_type (obj o)
{
	fprintf(stderr, "%04x : ", o);

	if (o == OBJ_FALSE) {
		fprintf(stderr, "#f");
	} else if (o == OBJ_TRUE) {
		fprintf(stderr, "#t");
	} else if (o == OBJ_NULL) {
		fprintf(stderr, "()");
	} else if (o < MIN_ROM_ENCODING) {
		fprintf(stderr, "fixnum");
	} else if (IN_RAM (o)) {
		if (RAM_BIGNUM_P(o)) {
			fprintf(stderr, "ram bignum");
		} else if (RAM_PAIR_P(o)) {
			fprintf(stderr, "ram pair");
		} else if (RAM_SYMBOL_P(o)) {
			fprintf(stderr, "ram symbol");
		} else if (RAM_STRING_P(o)) {
			fprintf(stderr, "ram string");
		} else if (RAM_VECTOR_P(o)) {
			fprintf(stderr, "ram vector");
		} else if (RAM_CONTINUATION_P(o)) {
			fprintf(stderr, "ram continuation");
		} else if (RAM_CLOSURE_P(o)) {
			fprintf(stderr, "ram closure");
		}
	} else { // ROM
		if (ROM_BIGNUM_P(o)) {
			fprintf(stderr, "rom bignum");
		} else if (ROM_PAIR_P(o)) {
			fprintf(stderr, "rom pair");
		} else if (ROM_SYMBOL_P(o)) {
			fprintf(stderr, "rom symbol");
		} else if (ROM_STRING_P(o)) {
			fprintf(stderr, "rom string");
		} else if (ROM_VECTOR_P(o)) {
			fprintf(stderr, "rom vector");
		} else if (ROM_CONTINUATION_P(o)) {
			fprintf(stderr, "rom continuation");
		}

		// ROM closures don't exist
	}

	fprintf(stderr, "\n");
}

void show_obj (obj o)
{
	if (o == OBJ_FALSE) {
		fprintf (stderr, "#f");
	} else if (o == OBJ_TRUE) {
		fprintf (stderr, "#t");
	} else if (o == OBJ_NULL) {
		fprintf (stderr, "()");
	} else if (o <= (MIN_FIXNUM_ENCODING + (MAX_FIXNUM - MIN_FIXNUM))) {
		fprintf (stderr, "%d", DECODE_FIXNUM(o));
	} else {
		uint8 in_ram;

		if (IN_RAM(o)) {
			in_ram = 1;
		} else {
			in_ram = 0;
		}

		if ((in_ram && RAM_BIGNUM_P(o)) || (!in_ram && ROM_BIGNUM_P(o))) { // TODO fix for new bignums, especially for the sign, a -5 is displayed as 251
			fprintf (stderr, "%d", decode_int (o));
		} else if ((in_ram && RAM_COMPOSITE_P(o)) || (!in_ram && ROM_COMPOSITE_P(o))) {
			obj car;
			obj cdr;

			if ((in_ram && RAM_PAIR_P(o)) || (!in_ram && ROM_PAIR_P(o))) {
				if (in_ram) {
					car = ram_get_car (o);
					cdr = ram_get_cdr (o);
				} else {
					car = rom_get_car (o);
					cdr = rom_get_cdr (o);
				}

				fprintf (stderr, "(");

loop:
				show_obj (car);

				if (cdr == OBJ_NULL) {
					fprintf (stderr, ")");
				} else if ((IN_RAM(cdr) && RAM_PAIR_P(cdr))
					   || (IN_ROM(cdr) && ROM_PAIR_P(cdr))) {
					if (IN_RAM(cdr)) {
						car = ram_get_car (cdr);
						cdr = ram_get_cdr (cdr);
					} else {
						car = rom_get_car (cdr);
						cdr = rom_get_cdr (cdr);
					}

					fprintf (stderr, " ");
					goto loop;
				} else {
					fprintf (stderr, " . ");
					show_obj (cdr);
					fprintf (stderr, ")");
				}
			} else if ((in_ram && RAM_SYMBOL_P(o)) || (!in_ram && ROM_SYMBOL_P(o))) {
				fprintf (stderr, "#<symbol>");
			} else if ((in_ram && RAM_STRING_P(o)) || (!in_ram && ROM_STRING_P(o))) {
				fprintf (stderr, "#<string>");
			} else if ((in_ram && RAM_VECTOR_P(o)) || (!in_ram && ROM_VECTOR_P(o))) {
				fprintf (stderr, "#<vector %d>", o);
			} else {
				fprintf (stderr, "(");
				cdr = ram_get_car (o);
				car = ram_get_cdr (o);
				// ugly hack, takes advantage of the fact that pairs and
				// continuations have the same layout
				goto loop;
			}
		} else { // closure
			obj env;
			rom_addr pc;

			env = ram_get_car (o);
			pc = ram_get_entry (o);

			fprintf (stderr, "{0x%04x ", pc);
			show_obj (env);
			fprintf (stderr, "}");
		}
	}
}

void show_state (rom_addr pc) {
	fprintf (stderr, "\n");
	fprintf (stderr, "pc=0x%04x bytecode=0x%02x env=", pc, rom_get (pc));
	show_obj (env);
	fprintf (stderr, " cont=");
	show_obj (cont);
	fprintf (stderr, "\n");
}
