arch-y   += entry.c primitives.c
stdlib-y += stdlib.scm

ifneq ($(CONFIG_NETWORKING_SLIP),)
arch-y   += net-slip.c
endif

ifneq ($(CONFIG_NETWORKING_ENC28J60),)
arch-y   += net-enc28j60.c
endif

ifneq ($(CONFIG_NETWORKING_W5x00),)
arch-y   += net-w5x00.c
endif

load-address := \#x8000

arch-all: picobit-vm

picobit-vm: $(obj-y) .config
	$(CROSS)gcc $(CFLAGS) -g -o $@ $(obj-y)
