#ifndef PICOBIT_ARCH_ARDUINO_MEMORY_H
#define PICOBIT_ARCH_ARDUINO_MEMORY_H

//#include "arduino.h"

#include <avr/pgmspace.h>

extern uint8_t ram_mem[];

#define RAM(a) ram_mem[(uint16_t)(a)]
#define ram_get(a) RAM(a)
#define ram_set(a,x) (RAM(a) = (x))

extern const uint8_t rom_mem[] PROGMEM;

//#define ROM_BYTES 8192
#define rom_get(a) pgm_read_byte(&rom_mem[a])

#endif
