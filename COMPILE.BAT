set TITLE=berzerk-mmx

@echo on
ca65.exe --cpu 6502 -t vic20 --listing --include-dir . basic.s
ca65.exe --cpu 6502 -t vic20 --include-dir . %TITLE%.s
ca65.exe --cpu 6502 -t vic20 --include-dir . vic-sss-mmx.s
ld65.exe -C basic+8k.cfg -Ln %TITLE%.sym -m %TITLE%.map -o %TITLE%.prg basic.o %TITLE%.o vic-sss-mmx.o
@echo off

choice /C DMV /D M /T 30 /M "[D]ebug, [M]ESS, [P]al, or [V]ICE? " /N
set CHOICE=%ERRORLEVEL%

if %CHOICE% EQU 1 mess -rp roms -debug -window -natural -skip_gameinfo -skip_warnings vic20 -ramsize 16k -quik %TITLE%.prg
if %CHOICE% EQU 2 mess -rp roms -skip_gameinfo -skip_warnings -newui vic20 -ramsize 16k -quik %TITLE%.prg
if %CHOICE% EQU 3 mess -rp roms -skip_gameinfo -skip_warnings -newui vic20pal -ramsize 16k -quik %TITLE%.prg
if %CHOICE% EQU 4 xvic -memory 8k -autostart %TITLE%.prg

@echo on
ca65.exe --cpu 6502 -t vic20 --listing --include-dir . -D VOICES basic.s
ca65.exe --cpu 6502 -t vic20 --include-dir . -D VOICES %TITLE%.s
ca65.exe --cpu 6502 -t vic20 --include-dir . voices.s
ld65.exe -C basic+16k.cfg -Ln %TITLE%+.sym -m %TITLE%+.map -o %TITLE%+.prg basic.o %TITLE%.o vic-sss-mmx.o voices.o
@echo off

choice /C DMV /D M /T 30 /M "[D]ebug, [M]ESS, [P]al, or [V]ICE? " /N
set CHOICE=%ERRORLEVEL%

if %CHOICE% EQU 1 mess -rp roms -debug -window -natural -skip_gameinfo -skip_warnings vic20 -ramsize 24k -quik %TITLE%+.prg
if %CHOICE% EQU 2 mess -rp roms -skip_gameinfo -skip_warnings -newui vic20 -ramsize 24k -quik %TITLE%+.prg
if %CHOICE% EQU 3 mess -rp roms -skip_gameinfo -skip_warnings -newui vic20pal -ramsize 24k -quik %TITLE%+.prg
if %CHOICE% EQU 4 xvic -memory 16k -autostart %TITLE%+.prg

exit

