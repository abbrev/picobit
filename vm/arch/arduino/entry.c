#include <stdio.h>
#include <stdlib.h>

#include <picobit.h>
#include <dispatch.h>
#include <gc.h>

#include <avr/pgmspace.h>
#include <avr/io.h>

uint8_t ram_mem[RAM_BYTES + VEC_BYTES];

void halt_with_error ()
{
	while(1);
}

#define USART_BAUDRATE 9600
#define BAUD_PRESCALE (((F_CPU/(USART_BAUDRATE*16UL)))-1)

static void uart_init(void)
{
	UCSR0B |= (1<<RXEN0)  | (1<<TXEN0);
	UCSR0C |= (1<<UCSZ00) | (1<<UCSZ01);
	UBRR0H  = (BAUD_PRESCALE >> 8);
	UBRR0L  = BAUD_PRESCALE;
}

int main (int argc, char *argv[])
{
	// TODO make this work on Arduino!
	// TODO initialize stuff here
	uart_init();

	if (rom_get (0) == 0xfb &&
	    rom_get (1) == 0xd7) {
		interpreter ();
	}

	return 0;
}
