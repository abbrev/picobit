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

PRIMITIVE(#%getchar-wait, getchar_wait, 2)
{
	decode_2_int_args();
	a1 = read_clock () + a1;

	if (a2 < 1 || a2 > 3) {
		ERROR("getchar-wait", "argument out of range");
	}

	arg1 = OBJ_FALSE;


#ifdef PICOBOARD2
	// TODO adapt to Arduino
	{
		serial_port_set ports;
		ports = serial_rx_wait_with_timeout( a2, a1 );

		if (ports != 0) {
			arg1 = encode_int (serial_rx_read( ports ));
		}
	}
#endif

}

PRIMITIVE(#%putchar, putchar, 2)
{
	decode_2_int_args ();

	if (a1 > 255 || a2 < 1 || a2 > 3) {
		ERROR("putchar", "argument out of range");
	}

#ifdef  PICOBOARD2
	// TODO adapt to Arduino
	serial_tx_write( a2, a1 );
#endif

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

// networking primitives
// to enable them, compilation must be done with the -lpcap option

PRIMITIVE_UNSPEC(network-init, network_init, 0)
{
	// TODO maybe put in the initialization of the vm
#ifdef NETWORKING
	handle = pcap_open_live(INTERFACE, MAX_PACKET_SIZE, PROMISC, TO_MSEC, errbuf);

	if (handle == NULL) {
		ERROR("network-init", "interface not responding");
	}

#endif
}

PRIMITIVE_UNSPEC(network-cleanup, network_cleanup, 0)
{
	// TODO maybe put in halt ?
#ifdef NETWORKING
	pcap_close(handle);
#endif
}

PRIMITIVE(receive-packet-to-u8vector, receive_packet_to_u8vector, 1)
{
	// arg1 is the vector in which to put the received packet
	if (!RAM_VECTOR_P(arg1)) {
		TYPE_ERROR("receive-packet-to-u8vector", "vector");
	}

#ifdef NETWORKING
	// receive the packet in the buffer
	struct pcap_pkthdr header;
	const u_char *packet;

	packet = pcap_next(handle, &header);

	if (packet == NULL) {
		header.len = 0;
	}

	if (ram_get_car (arg1) < header.len) {
		ERROR("receive-packet-to-u8vector", "packet longer than vector");
	}

	if (header.len > 0) { // we have received a packet, write it in the vector
		arg2 = VEC_TO_RAM_OBJ(ram_get_cdr (arg1));
		arg1 = header.len; // we return the length of the received packet
		a1 = 0;

		while (a1 < arg1) {
			ram_set_fieldn (arg2, a1 % 4, (char)packet[a1]);
			a1++;
			arg2 += (a1 % 4) ? 0 : 1;
		}

		arg2 = OBJ_FALSE;
	} else { // no packet to be read
		arg1 = OBJ_FALSE;
	}

#endif
}

PRIMITIVE(send-packet-from-u8vector, send_packet_from_u8vector, 2)
{
	// arg1 is the vector which contains the packet to be sent
	// arg2 is the length of the packet
	// TODO only works with ram vectors for now
	if (!RAM_VECTOR_P(arg1)) {
		TYPE_ERROR("send-packet-from-vector!", "vector");
	}

	a2 = decode_int (arg2); // TODO fix for bignums
	a1 = 0;

#ifdef NETWORKING

	// TODO test if the length of the packet is longer than the length of the vector
	if (ram_get_car (arg1) < a2) {
		ERROR("send-packet-from-u8vector", "packet cannot be longer than vector");
	}

	arg1 = VEC_TO_RAM_OBJ(ram_get_cdr (arg1));

	// copy the packet to the output buffer
	while (a1 < a2) {
		buf[a1] = ram_get_fieldn (arg1, a1 % 4);
		a1++;
		arg1 += (a1 % 4) ? 0 : 1;
	}

	// TODO maybe I could just give pcap the pointer to the memory

	if (pcap_sendpacket(handle, buf, a2) < 0) { // TODO an error has occurred, can we reuse the interface ?
		arg1 = OBJ_FALSE;
	} else {
		arg1 = OBJ_TRUE;
	}

#endif

	arg2 = OBJ_FALSE;
}
