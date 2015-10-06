#include <picobit.h>
#include <primitives.h>
#include <bignum.h>

#include <stdio.h>
//#include <sys/time.h>

PRIMITIVE_UNSPEC(print, print, 1)
{
	arg1 = OBJ_FALSE;
}


static uint32 read_clock ()
{
	uint32 now = 0;

	// TODO

	return now;
}

PRIMITIVE(clock, clock, 0)
{
	arg1 = encode_int (read_clock ());
}

#if 0
PRIMITIVE_UNSPEC(motor, motor, 2)
{
	decode_2_int_args ();

	if (a1 < 1 || a1 > 2 || a2 < -100 || a2 > 100) { // TODO since we now use unsigned values, we can't go backwards anymore
		ERROR("motor", "argument out of range");
	}



	arg1 = OBJ_FALSE;
	arg2 = OBJ_FALSE;
}

PRIMITIVE_UNSPEC(led, led, 3)
{
	decode_2_int_args ();
	a3 = decode_int (arg3);

	if (a1 < 1 || a1 > 3) {
		ERROR("led", "argument out of range");
	}



	arg1 = OBJ_FALSE;
	arg2 = OBJ_FALSE;
	arg3 = OBJ_FALSE;
}

PRIMITIVE_UNSPEC(#%led2-color, led2_color, 1)
{
	a1 = decode_int (arg1);

	if (a1 > 1) {
		ERROR("led2-colors", "argument out of range");
	}



	arg1 = OBJ_FALSE;
}
#endif

static int serial_read(int port)
{
	if ((UCSR0A & (1 << RXC0)) == 0)
		return -1;

	return UDR0;
}

static int serial_read_wait(int port)
{
	int c;

	while ((c = serial_read(port)) == -1)
		;

	return c;
}

PRIMITIVE(#%getchar-wait, getchar_wait, 2)
{
	decode_2_int_args();
	a1 = read_clock () + a1;

	if (a2 < 1 || a2 > 3) {
		ERROR("getchar-wait", "argument out of range");
	}

	arg1 = encode_int(serial_read_wait(a2));
}

static int serial_write(int port, int c)
{
	if ((UCSR0A & (1 << UDRE0)) == 0)
		return -1;

	UDR0 = c;
	return (unsigned char)c;
}

static int serial_write_wait(int port, int c)
{
	int x;

	while ((x = serial_write(port, c)) == -1)
		;

	return x;
}

PRIMITIVE(#%putchar, putchar, 2)
{
	decode_2_int_args ();

	if (a1 > 255 || a2 < 1 || a2 > 3) {
		ERROR("putchar", "argument out of range");
	}

	serial_write_wait(a2, a1);

	arg1 = OBJ_FALSE;
	arg2 = OBJ_FALSE;
}

#if 0
PRIMITIVE(beep, beep, 2)
{
	decode_2_int_args ();

	if (a1 < 1 || a1 > 255) {
		ERROR("beep", "argument out of range");
	}

	arg1 = OBJ_FALSE;
	arg2 = OBJ_FALSE;
}
#endif

PRIMITIVE(adc, adc, 1)
{
	uint16 x = 0;

	a1 = decode_int (arg1);

	if (a1 < 1 || a1 > 3) {
		ERROR("adc", "argument out of range");
	}

#ifdef  PICOBOARD2
	// TODO adapt to Arduino
	x = adc( a1 );
#endif

	arg1 = encode_int (x);
}

PRIMITIVE(sernum, sernum, 0)
{
	uint16 x = 0;

	arg1 = encode_int (x);
}


/*---------------------------------------------------------------------------*/

#ifdef CONFIG_NETWORKING

#include "net.h"

// networking primitives
// to enable them, compilation must be done with the -lpcap option

PRIMITIVE_UNSPEC(network-init, network_init, 0)
{
	network_init();
}

PRIMITIVE_UNSPEC(network-cleanup, network_cleanup, 0)
{
	network_cleanup();
}

PRIMITIVE(receive-packet-to-u8vector, receive_packet_to_u8vector, 1)
{
	// arg1 is the vector in which to put the received packet
	if (!RAM_VECTOR_P(arg1)) {
		TYPE_ERROR("receive-packet-to-u8vector", "vector");
	}

	uint16 len = receive_packet_to_u8vector(arg1);

	if (len > 0) {
		arg1 = encode_int (len);
		arg2 = OBJ_FALSE;
	} else {
		arg1 = OBJ_FALSE;
		// XXX arg2?
	}
}

PRIMITIVE(send-packet-from-u8vector, send_packet_from_u8vector, 2)
{
	// arg1 is the vector which contains the packet to be sent
	// arg2 is the length of the packet
	// TODO only works with ram vectors for now
	if (!RAM_VECTOR_P(arg1)) {
		TYPE_ERROR("send-packet-from-vector!", "vector");
	}

	// TODO test if the length of the packet is longer than the length of the vector
	if (ram_get_car (arg1) < a2) {
		ERROR("send-packet-from-u8vector", "packet cannot be longer than vector");
	}

	uint8 b = send_packet_from_u8vector(arg1, decode_int (arg2));

	arg1 = encode_bool(b);
	arg2 = OBJ_FALSE;
}

#endif
