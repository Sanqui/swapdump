#gawk sort order
export LC_CTYPE=C

.SUFFIXES: .asm .o .gbc

all: cartswap.gbc

cartswap.o: cartswap.asm constants.asm
	rgbasm -o cartswap.o cartswap.asm

cartswap.gbc: cartswap.o
	rgblink -o $@ $<
	rgbfix -jv -i XXXX -k XX -l 0x33 -m 0x13 -p 0 -r 0 -t CARTSWAP $@

clean:
	rm -f cartswap.o cartswap.gbc
