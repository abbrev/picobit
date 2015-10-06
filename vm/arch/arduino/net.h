#ifndef PICOBIT_ARCH_ARDUINO_NET_H
#define PICOBIT_ARCH_ARDUINO_NET_H

extern void network_init(void);
extern void network_cleanup(void);
extern uint16 receive_packet_to_u8vector(obj vector);
extern uint8 send_packet_from_u8vector(obj vector, uint16 length);

#endif
