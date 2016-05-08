#!/bin/sh
#
# run cc65's assembler/linker executables
#

TITLE=berzerk-mmx

set -o xtrace
ca65 --cpu 6502 -t vic20 --include-dir . --listing basic.s
ca65 --cpu 6502 --include-dir . --listing $TITLE.s
ca65 --cpu 6502 --include-dir . --listing VIC-SSS-MMX.s
ld65 -C basic+8k.cfg -Ln $TITLE.sym -m $TITLE.map -o $TITLE.prg basic.o $TITLE.o VIC-SSS-MMX.o

ca65 --cpu 6502 -t vic20 --include-dir . --listing -D VOICES basic.s
ca65 --cpu 6502 --include-dir . --listing -D VOICES $TITLE.s
ca65 --cpu 6502 --include-dir . --listing voices.s
ld65 -C basic+16k.cfg -Ln ${TITLE}+.sym -m ${TITLE}+.map -o ${TITLE}+.prg basic.o $TITLE.o VIC-SSS-MMX.o voices.o
set +o xtrace

echo -n "Press RETURN: " && read N

# UNCOMMENT YOUR CHOICE OF EMULATORS ...

#mess -debug vic20 -ramsize 16k -quik $TITLE.prg
#xvic -memory 8k -autostart $TITLE.prg
xvic -memory 16k -autostart ${TITLE}+.prg

exit

