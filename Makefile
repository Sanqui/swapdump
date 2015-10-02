#gawk sort order
export LC_CTYPE=C

.SUFFIXES: .asm .o .gbc

all: swapdump.gbc

swapdump.o: swapdump.asm constants.asm
	rgbasm -o swapdump.o swapdump.asm

swapdump.gbc: swapdump.o
	rgblink -o $@ $<
	rgbfix -jv -i XXXX -k XX -l 0x33 -m 0x13 -p 0 -r 0 -t swapdump $@

clean:
	rm -f swapdump.o swapdump.gbc
