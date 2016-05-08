;*********************************************************************
; COMMODORE VIC 20 BOOT USING BASIC 2.0
; written by Robert Hurst <robert@hurst-ri.us>
; updated version: 30-Oct-2011
;
		.fileopt author,	"Robert Hurst"
        .fileopt comment,	"Berzerk MMX"
        .fileopt compiler,	"VIC 20 ASSEMBLER"

		.include "VIC-SSS-MMX.h"

;*********************************************************************
; Commodore BASIC 2.0 program
;
; LOAD "BERZERK-MMX.PRG",8
; RUN
;
		.segment "BASIC"

		.word	RUN		; load address
RUN:	.word	@end	; next line link
		.word	2011	; line number
		.byte	$9E		; BASIC token: SYS
		.byte	<(MAIN / 1000 .mod 10) + $30
		.byte	<(MAIN / 100 .mod 10) + $30
		.byte	<(MAIN / 10 .mod 10) + $30
		.byte	<(MAIN / 1 .mod 10) + $30
		.byte	0		; end of line
@end:	.word	0		; end of program

;*********************************************************************
; Starting entry point for this program
;
		.segment "STARTUP"

MAIN:
		LDX $FFFC
		LDY $FFFD
		STX $0318
		STY $0319		; enable RESTORE key as RESET
		LDA MACHINE
		CMP #$05
		BNE PAL
		;
		; NTSC setup
NTSC:	LDX #<@NTSC		; load the timer low-byte latches
		LDY #>@NTSC
		LDA #$75		; raster line 234/235
		BNE IRQSYNC
@NTSC = $4243			; (261 * 65 - 2)
		;
		; PAL setup
PAL:	LDX #<@PAL		; load the timer low-byte latches
		LDY #>@PAL
		LDA #$82		; raster line 260/261
@PAL = $5686			; (312 * 71 - 2)
		;
IRQSYNC:
		CMP VIC+$04
		BNE IRQSYNC
		STX $9126		; load T1 latch low
		STY $9125		; load T1 latch high, and transfer both to T1 counter


;*********************************************************************
; Now that all the VIC startup initialization stuff is completed,
; you can append one-time startup code/data here, i.e., like a splash
; title screen.  Then, you must jump to your CODE segment, linked
; outside of VIC's internal RAM address space ...
;
RUNONCE:				; init VIC
		.global HISCORE
		MEGACART= $9D80	; memory bank register
		NVRAM	= $9C5A	; 3-bytes for Berzerk's hi-score
		;
.ifdef VOICES
		LDA #<@msg
		LDY #>@msg
		JSR $CB1E
@user:	JSR GETIN
		BEQ @user
		.GLOBAL INTRUDER_ALERT
		JSR INTRUDER_ALERT
		JSR INTRUDER_ALERT
		JMP @hi
@msg:	.byte 13,18,"turn volume up higher ",0
.endif
@hi:	LDA #$FE
		STA MEGACART	; init MC memory register
		STA MEGACART	; init MC memory register a 2nd time
		CMP MEGACART	; detect if this memory location was really writable
		BNE @init		; no, might be an emulator or a real VIC with only 8k
		LDX NVRAM+2		; load saved high score
		TXA
		AND #$0F
		BNE @init		; no one's allowed -- reset hi-score
		LDY NVRAM+1
		CPY #$25
		BCC @init		; less than 2,500 points?
		LDA NVRAM
		STA HISCORE
		STY HISCORE+1
		STX HISCORE+2
		;
@init:	LDA #$00+$16	; set for videoram @ $1400 with 22-columns
		STA VIC+$02		; video matrix address + columns
		LDA #%10101110	; 8x8 height + 23-rows
		STA VIC+$03		; rows / character height
		LDA #$DF		; set video @ $1400 and char table @ $1C00
		STA VIC+$05
		LDA #138		; Programmer's Reference Guide: Appendix B
		STA VIC+$0F		; orange screen / red border
		; reset sound channels
@cont:	LDA #$00
		TAY
@snd:	STA VIC+$0A,Y
		INY
		CPY #$04
		BNE @snd
.ifdef VOICES
		LDA #$13		; white & low for sound effects
.else
		LDA #$1F		; white & highest
.endif
		STA VIC+$0E		; auxiliary color & volume
		LDA #$80
		STA SHIFTMODE	; locked
		;
		.global MYIRQ
		SEI
		LDX #<MYIRQ
		LDY #>MYIRQ
		STX $0314
		STY $0315
		CLI
		;
		LDX #<SPLASHCOLOR
		LDY #>SPLASHCOLOR
		STX $FB
		STY $FC
		LDX #$00
		LDY #$94
		STX $FD
		STY $FE
		LDX #$02
		LDY #$00
@fill:	LDA ($FB),Y
		STA ($FD),Y
		INY
		BNE @fill
		INC $FC
		INC $FE
		DEX
		BNE @fill
		;
		.global OREH
		LDX #<OREH
		LDY #>OREH
		STX $F7
		STY $F8
		LDX #$CF		; col
		LDY #$14		; row
		STX $FD
		STY $FE
		LDY #0
		STY $01

		.GLOBAL FRAME
		.GLOBAL	KABOOM
		.GLOBAL	SHOOTING

@loop:
		LDA #3
		STA VSYNC
@wait:	LDA JIFFYL
		BNE @vsync
		LDX #$F0
		INC FRAME
		LDA FRAME
		AND #$03
		BEQ @vsync
		AND #$01
		BEQ @2
		STX SHOOTING
		BNE @vsync
@2:		STX KABOOM
@vsync:	LDA VSYNC
		BNE @vsync		; and wait for it to occur
		LDX #$00
		LDY $01
		LDA RMAN,Y		; running man frame
		TAY
@anim:	LDA ($F7),Y
		STA C27,X
		INY
		INX
		CPX #$10
		BNE @anim
		INC $01			; next frame
		LDA $01
		CMP #4			; last frame?
		BNE @eye
		LDA #0			; reset anim
		STA $01
@eye:	INC EYEW
		LDA EYEW
		LSR
		BCS	@move		; every other iteration
		JMP @key
@move:	LDX EYE
		LDY EYEY
		LDA EYEDX
		BEQ @eyer
		JMP @eyel
@eyegor:
		LDX #0			; reset
		STX EYEDX
		LDY #0
		STY EYEY
@eyer:	CPX #2
		BEQ @eyer2		; ready for alternate frame?
		LDX #2
		STX EYE
		LDA EYER,X
		STA ($FD),Y
		INY
		LDA EYER+1,X
		STA ($FD),Y
		BNE @key
@eyer2:	LDA #$20
		STA ($FD),Y		; erase old
		INY
		STY EYEY		; advance to right
		CPY #3
		BEQ @eyegol		; reached end of eye traveling right
		LDX #0
		STX EYE
		LDA EYER,X
		STA ($FD),Y
		INY
		LDA EYER+1,X
		STA ($FD),Y
		BNE @key
@eyegol:
		INC EYEDX
@eyel:	CPX #2
		BEQ @eyel2		; ready for alternate frame?
		LDX #2
		STX EYE
		LDA EYEL,X
		STA ($FD),Y
		INY
		LDA EYEL+1,X
		STA ($FD),Y
		BNE @key
@eyel2:	DEY
		STY EYEY		; advance to left
		BMI @eyegor		; reached end of eye traveling left
		LDX #0
		STX EYE
		LDA EYEL,X
		STA ($FD),Y
		INY
		LDA EYEL+1,X
		STA ($FD),Y
		CPY #3
		BEQ @key
		INY
		LDA #$20
		STA ($FD),Y		; erase old
@key:	LDA $028D
		AND #$02		; got C= key?
		BNE @go
@no:	LDA #$FF
		STA $9122
		LDA $9111
		AND #$20		; got FIRE ?
		BEQ @go
@scan:	JMP @loop
@go:	; seed the random number generator
		LDA $9124
		LDY $9125
		JSR $D391               ; givayf: Convert Integer in (AC/YR) to Flpt
		LDX #<RNDSEED
		LDY #>RNDSEED
		JSR $DBD7               ; mov2f: Store FAC#1 in Memory
		.global RESTART	; useful symbol for MAP and hotkey restarting
		JMP RESTART		; the entry point into your program

RMAN:	.byte	$00,$10,$20,$10

		.segment "SPLASH"
SPLASHDATA:
		.byte	$DA,$A0,$A0,$DA,$A0,$A0,$DA,$A0,$A0,$DA,$A0,$A0,$DA,$A0,$A0,$DA,$A0,$A0,$DA,$A0,$A0,$DA
		.byte	$A0,$A0,$A0,$A0,$A0,$A0,$A0,$2D,$35,$46,$53,$5D,$6D,$75,$78,$A0,$A0,$A0,$A0,$A0,$A0,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$A0,$A0,$2F,$43,$4B,$5B,$5E,$70,$77,$79,$A0,$A0,$A0,$A0,$A0,$A0,$A0
		.byte	$A0,$94,$88,$89,$93,$A0,$87,$81,$8D,$85,$A0,$92,$85,$91,$95,$89,$92,$85,$93,$A0,$81,$A0
		.byte	$01,$0A,$0A,$8A,$8F,$99,$93,$94,$89,$83,$8B,$A0,$94,$8F,$A0,$90,$8C,$81,$99,$0A,$0A,$02
		.byte	$09,$0C,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$0C,$09
		.byte	$09,$0D,$A0,$A0,$A0,$A0,$A0,$30,$38,$40,$48,$50,$58,$60,$68,$A0,$A0,$A0,$A0,$A0,$0D,$09
		.byte	$09,$A0,$A0,$0C,$A0,$21,$29,$31,$39,$41,$49,$51,$59,$61,$69,$71,$A0,$A0,$0C,$A0,$A0,$09
		.byte	$09,$A0,$A0,$0D,$A0,$22,$20,$20,$3A,$42,$4A,$52,$5A,$62,$20,$20,$7A,$A0,$0D,$A0,$A0,$09
		.byte	$09,$0C,$A0,$A0,$A0,$23,$20,$20,$3B,$20,$20,$20,$20,$63,$20,$20,$7B,$A0,$A0,$A0,$0C,$09
		.byte	$09,$0D,$A0,$A0,$A0,$24,$20,$20,$3C,$44,$4C,$54,$5C,$64,$20,$20,$7C,$A0,$A0,$A0,$0D,$09
		.byte	$09,$A0,$A0,$0C,$A0,$25,$20,$20,$3D,$45,$4D,$55,$45,$65,$20,$20,$7D,$A0,$0C,$A0,$A0,$09
		.byte	$09,$A0,$A0,$0D,$A0,$26,$2E,$36,$3E,$45,$4E,$56,$45,$66,$6E,$76,$7E,$A0,$0D,$A0,$A0,$09
		.byte	$09,$0C,$A0,$A0,$A0,$A0,$A0,$37,$3F,$47,$4F,$57,$5F,$67,$6F,$A0,$A0,$A0,$A0,$A0,$0C,$09
		.byte	$09,$0D,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$0D,$09
		.byte	$03,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$A0,$27,$A0,$0A,$0A,$0A,$0A,$0A,$0A,$04
		.byte	$A0,$82,$85,$92,$9A,$85,$92,$8B,$A0,$8D,$8D,$98,$A0,$28,$A0,$92,$B1,$B0,$AE,$B3,$B0,$A0
		.byte	$A0,$A0,$7F,$B2,$B0,$B1,$B1,$A0,$92,$8F,$82,$85,$92,$94,$A0,$88,$95,$92,$93,$94,$A0,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$8D,$81,$84,$85,$A0,$89,$8E,$A0,$95,$93,$81,$A0,$A0,$A0,$A0,$A0,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$A0,$2A,$32,$6A,$72,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
		.byte	$A0,$90,$92,$85,$93,$93,$2B,$33,$6B,$73,$94,$8F,$A0,$83,$8F,$8E,$94,$89,$8E,$95,$85,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$A0,$2C,$34,$6C,$74,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
		.byte	$DA,$A0,$A0,$DA,$A0,$A0,$DA,$A0,$A0,$DA,$A0,$A0,$DA,$A0,$A0,$DA,$A0,$A0,$DA,$A0,$A0,$DA
EYE:	.byte	$00		; 0,2: word index into EYEL / EYER
EYEDX:	.byte	$00		; 0=right, 1=left
EYEL:	.byte	$1A,$1B,$18,$19
SPLASHCOLOR:
		.byte	$00,$00,$00,$01,$00,$00,$02,$00,$00,$03,$00,$00,$04,$00,$00,$05,$00,$00,$06,$00,$00,$07
		.byte	$00,$00,$00,$00,$00,$00,$00,$0E,$0E,$0F,$0F,$0E,$0F,$0F,$0F,$00,$00,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$00,$00,$0A,$0A,$0F,$0D,$0A,$0C,$0C,$0C,$00,$00,$00,$00,$00,$00,$00
		.byte	$00,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$00
		.byte	$06,$06,$06,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$06,$06,$06
		.byte	$06,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$06
		.byte	$06,$07,$00,$00,$00,$00,$00,$08,$08,$08,$08,$08,$08,$08,$08,$00,$00,$00,$00,$00,$07,$06
		.byte	$06,$00,$00,$02,$00,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$00,$02,$00,$00,$06
		.byte	$06,$00,$00,$02,$00,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$00,$02,$00,$00,$06
		.byte	$06,$03,$00,$00,$00,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$00,$00,$00,$03,$06
		.byte	$06,$03,$00,$00,$00,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$00,$00,$00,$03,$06
		.byte	$06,$00,$00,$04,$00,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$00,$04,$00,$00,$06
		.byte	$06,$00,$00,$04,$00,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$00,$04,$00,$00,$06
		.byte	$06,$01,$00,$00,$00,$00,$00,$08,$08,$08,$08,$08,$08,$08,$08,$00,$00,$00,$00,$00,$01,$06
		.byte	$06,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$06
		.byte	$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$00,$05,$00,$06,$06,$06,$06,$06,$06,$06
		.byte	$00,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$00,$05,$00,$03,$03,$03,$03,$03,$03,$00
		.byte	$00,$00,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$00,$00
		.byte	$00,$00,$00,$00,$00,$03,$03,$03,$03,$03,$03,$03,$00,$02,$01,$06,$00,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$00,$0E,$0E,$0E,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$00,$0E,$0E,$0E,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$00,$0E,$0E,$0E,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte	$07,$00,$00,$06,$00,$00,$05,$00,$00,$04,$00,$00,$03,$00,$00,$02,$00,$00,$01,$00,$00,$00
EYER:	.byte	$1C,$1D,$1E,$1F
EYEW:	.byte	$00
EYEY:	.byte	$00		; screen index offset to range of motion

;*********************************************************************
; VIC Software Sprite Stack 2010 (VIC-SSS-MMX)
;
; The above BASIC loader will be overwritten by SSS upon its
; initialization (SSSINIT).  The linker will fill this reserved space
; with values used for the dual video frame buffers, play field, and
; the sprite image buffers and registers: 4096 - 6207 ($1000 - $1BFF)
;
; $1000 - $11FF		VICFRAME1 - first video buffer
; $1200 - $13FF		VICFRAME2 - second video buffer
; $1400 - $15FF		PLAYFIELD - write-pending screen buffer
; $1600 - $17FF		PLAYCOLOR - write-pending color / dirty buffer
; $1800 - $1BFF		Sprite image buffers & registers
;
			.segment "SSSBUF"

SSSBUF:		.res 52 * 8	; this can be resized smaller as required --
						; if all 64-chars are used by sprites, that
						; exhausts all 128 custom characters for
						; double-buffering (x2)
;
; SPRITE REGISTERS (17)
;
SPRITEBACK:	.res SPRITEMAX	; 1st char this sprite is in collision with
SPRITEBUFH:	.res SPRITEMAX	; pointer within sprite image buffer
SPRITEBUFL:	.res SPRITEMAX	; pointer within sprite image buffer
SPRITEC1H:	.res SPRITEMAX	; pointer within sprite display character pool
SPRITEC1L:	.res SPRITEMAX	; pointer within sprite display character pool
SPRITEC2H:	.res SPRITEMAX	; pointer within sprite display character pool
SPRITEC2L:	.res SPRITEMAX	; pointer within sprite display character pool
SPRITECOL:	.res SPRITEMAX	; 4-bit VIC color code
SPRITECX:	.res SPRITEMAX	; sprite collision X-coord
SPRITECY:	.res SPRITEMAX	; sprite collision Y-coord
SPRITEDEF:	.res SPRITEMAX	; function/matrix definition (see explanation below)
SPRITEH:	.res SPRITEMAX	; number of raster lines (1-16)
SPRITEIMGH:	.res SPRITEMAX	; pointer to source graphic for rendering at 0,0
SPRITEIMGL:	.res SPRITEMAX	; pointer to source graphic for rendering at 0,0
SPRITEX:	.res SPRITEMAX	; horizontal pixel coordinate, visible >0 - <SSSCLIPX
SPRITEY:	.res SPRITEMAX	; vertical pixel coordinate, visible >0 - <SSSCLIPY
SPRITEZ:	.res SPRITEMAX	; bit 0: last rendered (0 = SPRITEC1; 1 = SPRITEC2)
							; bit 1: fast copy (0 = merge; 1 = copy)
							; bit 3: sprite collision
							; bit 4: sprite image is clipped by a static cell
							; bit 5: background is all SSSNULLs
							; bit 6: copy/merge into alternate sprite character pool
							; bit 7: copy/shift sprite image into its buffer
;
; SPRITEDEF is a bit-structure of these characteristics:
; - height		bit 0: 0 = 8px; 1 = 16px
; - width		bit 1: 0 = 8px; 1 = 16px
; - float Y		bit 2: flag: 0=fixed cell, 1=vertical float
; - float X		bit 3: flag: 0=fixed cell, 1=horizontal float
; - repeat		bit 4: flag: 0=independent, 1=same as previous
; - ghost		bit 5: flag: 0=merge image, 1=invert image
; - collision	bit 6: flag: 0=fast copy, 1=detect
; - enabled		bit 7: flag: 0=invisible, 1=visible
;
						; SSS runtime variables:
sss:		.res 24*2	; screen row index, computed from PLAYCOLS in SSSINIT
;
; other initialized data can be appended here:
;
			;.segment "RODATA"
sssALLOC:	; 8x8, 16x8, 8x16, 16x16
			.byte	8,16,16,32	; fixed:	1,2,2,4
			.byte	16,24,32,48	; float Y:	2,3,4,6
			.byte	16,32,24,48	; float X:	2,4,3,6
			.byte	32,48,48,72	; both:		4,6,6,9
sssROWS:	.byte	1,2,1,2	; fixed
			.byte	2,3,2,3	; float Y
			.byte	1,2,1,2	; float X
			.byte	2,3,2,3	; both
sssCOLS:	.byte	1,1,2,2	; fixed
			.byte	1,1,2,2	; float Y
			.byte	2,2,3,3	; float X
			.byte	2,2,3,3	; both

;*********************************************************************
; VIC Custom Graphic Characters
;
; If < 64 will be used for the software sprite stack, the remaining
; unused characters can be used for other custom graphics, beginning
; at $1C00 where "@", "A", "B", ... characters can be redefined.
;
; Do not use this as an initialized segment if you plan on linking
; this source as a future game cartridge later.  You must COPY any
; read-only data into this address space.
;
; If your data was saved from some tool in binary format, you can
; include that binary file here as:
;		.incbin "graphics.dat"
;
; else, just enter each 8x8 values here, such as:
;		.byte	$FF,$81,$81,$81,$81,$81,$81,$FF
; or:
;		.byte	%11111111	; square
;		.byte	%10000001
;		.byte	%10000001
;		.byte	%10000001
;		.byte	%10000001
;		.byte	%10000001
;		.byte	%10000001
;		.byte	%11111111
;
		.segment "MYCHAR"

C0:		; player life icon
		.byte	%00011000
		.byte	%00000000
		.byte	%00111100
		.byte	%01011010
		.byte	%00011000
		.byte	%00011000
		.byte	%00100100
		.byte	%01000010
C1:		; top-left
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00011111
		.byte	%00011111
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
C2:		; top-right
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%11111000
		.byte	%11111000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
C3:		; bottom-left
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011111
		.byte	%00011111
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
C4:		; bottom-right
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%11111000
		.byte	%11111000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
C5:		; top-mid
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%11111111
		.byte	%11111111
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
C6:		; right-mid
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%11111000
		.byte	%11111000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
C7:		; bottom-mid
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%11111111
		.byte	%11111111
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
C8:		; left-mid
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011111
		.byte	%00011111
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
C9:		; vertical
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
CA:		; horizontal
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%11111111
		.byte	%11111111
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
CB:		; cross
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%11111111
		.byte	%11111111
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
CC:		; robot fixture
		.byte	%00000000
		.byte	%00000000
		.byte	%00111100
CYCLOPS:.global CYCLOPS
		.byte	%01111110
		.byte	%11111111
		.byte	%10111101
		.byte	%10111101
CD:		.byte	%10111101
		.byte	%00111100
		.byte	%00100100
		.byte	%00100100
		.byte	%00100100
		.byte	%01100110
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		;
C0E:	; zero
		.byte	%01111100
		.byte	%10000010
		.byte	%10000010
		.byte	%10000010
		.byte	%10000010
		.byte	%10000010
		.byte	%10000010
		.byte	%01111100
C0F:	; one
		.byte	%00010000
		.byte	%00110000
		.byte	%00010000
		.byte	%00010000
		.byte	%00010000
		.byte	%00010000
		.byte	%00010000
		.byte	%01111100
C10:	; two
		.byte	%01111100
		.byte	%00000010
		.byte	%00000010
		.byte	%01111100
		.byte	%10000000
		.byte	%10000000
		.byte	%10000000
		.byte	%11111110
C11:	; three
		.byte	%11111110
		.byte	%00000010
		.byte	%00000010
		.byte	%00011100
		.byte	%00000010
		.byte	%00000010
		.byte	%10000010
		.byte	%01111100
C12:	; four
		.byte	%01000100
		.byte	%01000100
		.byte	%10000100
		.byte	%11111110
		.byte	%00000100
		.byte	%00000100
		.byte	%00000100
		.byte	%00000100
C13:	; five
		.byte	%11111100
		.byte	%10000000
		.byte	%10000000
		.byte	%11111100
		.byte	%00000010
		.byte	%00000010
		.byte	%10000010
		.byte	%01111100
C14:	; six
		.byte	%01111100
		.byte	%10000000
		.byte	%10000000
		.byte	%11111100
		.byte	%10000010
		.byte	%10000010
		.byte	%10000010
		.byte	%01111100
C15:	; seven
		.byte	%11111110
		.byte	%00000010
		.byte	%00000100
		.byte	%00001000
		.byte	%00010000
		.byte	%00010000
		.byte	%00010000
		.byte	%00010000
C16:	; eight
		.byte	%01111100
		.byte	%10000010
		.byte	%10000010
		.byte	%01111100
		.byte	%10000010
		.byte	%10000010
		.byte	%10000010
		.byte	%01111100
C17:	; nine
		.byte	%01111100
		.byte	%10000010
		.byte	%10000010
		.byte	%01111110
		.byte	%00000010
		.byte	%00000010
		.byte	%00000010
		.byte	%01111100
		;
		; start of RUNONCE graphics - these will be re-used by SSSINIT
		;
C18:	; 0:	Left - a
		.byte	%10101010
		.byte	%10101010
		.byte	%10010100	; -rro
		.byte	%01010101	; rrrr
		.byte	%01010101	; rrrr
		.byte	%10010100	; -rro
		.byte	%10101010
		.byte	%10101010
C19:	; 0:	Left - b
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010	; ----
		.byte	%00101010	; o---
		.byte	%00101010	; o---
		.byte	%10101010	; ----
		.byte	%10101010
		.byte	%10101010
C1A:	; 1:	Left - a
		.byte	%10101010
		.byte	%10101010
		.byte	%10101001	; ---r
		.byte	%10100101	; --rr
		.byte	%10100101	; --rr
		.byte	%10101001	; ---r
		.byte	%10101010
		.byte	%10101010
C1B:	; 1:	Left - b
		.byte	%10101010
		.byte	%10101010
		.byte	%01001010	; ro--
		.byte	%01010010	; rro-
		.byte	%01010010	; rro-
		.byte	%01001010	; ro--
		.byte	%10101010
		.byte	%10101010
		;
C1C:	; 0:	Right - a
		.byte	%10101010
		.byte	%10101010
		.byte	%10000101	; -orr
		.byte	%00010101	; orrr
		.byte	%00010101	; orrr
		.byte	%10000101	; -orr
		.byte	%10101010
		.byte	%10101010
C1D:	; 0:	Right - b
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010	; ----
		.byte	%01101010	; r---
		.byte	%01101010	; r---
		.byte	%10101010	; ----
		.byte	%10101010
		.byte	%10101010
C1E:	; 1:	Right - a
		.byte	%10101010
		.byte	%10101010
		.byte	%10101000	; ---o
		.byte	%10100001	; --or
		.byte	%10100001	; --or
		.byte	%10101000	; ---o
		.byte	%10101010
		.byte	%10101010
C1F:	; 1:	Right - b
		.byte	%10101010
		.byte	%10101010
		.byte	%01011010	; rr--
		.byte	%01010110	; rrr-
		.byte	%01010110	; rrr-
		.byte	%01011010	; rr--
		.byte	%10101010
		.byte	%10101010
		;
C20:	.byte	%10101010	; BLACK fill square
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
@cylon:
		; 1,0
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000011
		; 2,0
		.byte	%00000011
		.byte	%00000011
		.byte	%00000011
		.byte	%00001111
		.byte	%00001110
		.byte	%00001110
		.byte	%00111110
		.byte	%00111010
		; 3,0
		.byte	%00111010
		.byte	%00111010
		.byte	%00111010
		.byte	%11111010
		.byte	%11101010
		.byte	%11101010
		.byte	%11101010
		.byte	%11101010
		; 4,0
		.byte	%11101010
		.byte	%11101010
		.byte	%11101010
		.byte	%11101010
		.byte	%11111010
		.byte	%00111010
		.byte	%00111010
		.byte	%00111010
		; 5,0
		.byte	%00111010
		.byte	%00111110
		.byte	%00001110
		.byte	%00001110
		.byte	%00001111
		.byte	%00000011
		.byte	%00000011
		.byte	%00000011
		; 6,0
		.byte	%00000011
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		;
C27:	.res	8			; top-half of running man
C28:	.res	8			; bot-half of running man
		;
		; 1,1
		.byte	%00000000
		.byte	%00000011
		.byte	%00001111
		.byte	%00111110
		.byte	%00111010
		.byte	%11111010
		.byte	%11101010
		.byte	%11101010
	; CKEY
	; 0,0
C2A:	.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000011
		.byte	%00000011
		.byte	%00000011
		.byte	%00000011
		.byte	%00000011
	; 1,0
C2B:	.byte	%00000011
		.byte	%00000011
		.byte	%00000011
		.byte	%00000011
		.byte	%00000011
		.byte	%00000011
		.byte	%00000011
		.byte	%00000011
	; 2,0
C2C:	.byte	%00000011
		.byte	%00000011
		.byte	%00000011
		.byte	%00000011
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		;
C2D:	.byte	%11111111	; 0,0 (V) LOGO
		.byte	%11101011
		.byte	%11101011
		.byte	%11101011
		.byte	%11101011
		.byte	%11010111
		.byte	%11110101
		.byte	%11110101
		;
		; 6,1
		.byte	%11101010
		.byte	%11101010
		.byte	%11111010
		.byte	%00111010
		.byte	%00111110
		.byte	%00001111
		.byte	%00000011
		.byte	%00000000
		;
C2F:	.byte	%11111100	; 1,0 (V) LOGO
		.byte	%11111100
		.byte	%11111111
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		;
		; 0,2
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000011
		.byte	%00001111
		.byte	%00111110
		; 1,2
		.byte	%11111010
		.byte	%11101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
	; CKEY
	; 0,1
C32:	.byte	%00000000
		.byte	%00000000
		.byte	%11111111
		.byte	%11111111
		.byte	%11111010
		.byte	%11101010
		.byte	%11101010
		.byte	%10101011
	; 1,1
C33:	.byte	%10101111
		.byte	%10101111
		.byte	%10101111
		.byte	%10101111
		.byte	%10101111
		.byte	%10101111
		.byte	%10101111
		.byte	%10101011
	; 2,1
C34:	.byte	%11101010
		.byte	%11101010
		.byte	%11111010
		.byte	%11111111
		.byte	%11111111
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		;
C35:	.byte	%11111111	; 0,1 (V) LOGO
		.byte	%11101011
		.byte	%11010111
		.byte	%11010111
		.byte	%11010111
		.byte	%11010111
		.byte	%01011111
		.byte	%00001111
		;
		; 6,2
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%11101010
		.byte	%11111010
		; 7,2
		.byte	%00111110
		.byte	%00001111
		.byte	%00000011
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		; 0,3
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000011
		.byte	%00111111
		.byte	%11111110
		.byte	%11101010
		.byte	%10101010
		; 1,3
		.byte	%10101010
		.byte	%10101010
		.byte	%10101011
		.byte	%10101011
		.byte	%10101111
		.byte	%10101111
		.byte	%10101111
		.byte	%10101111
		; 2,3
		.byte	%10101111
		.byte	%10101111
		.byte	%10101111
		.byte	%10101110
		.byte	%10101011
		.byte	%10111111
		.byte	%10111111
		.byte	%10111111
		; 3,3
		.byte	%10111111
		.byte	%10111110
		.byte	%10111110
		.byte	%10111110
		.byte	%10111110
		.byte	%10111110
		.byte	%10111110
		.byte	%10111110
		; 4,3
		.byte	%10111110
		.byte	%10111110
		.byte	%10111111
		.byte	%10111111
		.byte	%10101011
		.byte	%10101011
		.byte	%10101011
		.byte	%10101011
		; 5,3
		.byte	%10101011
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		; 6,3
		.byte	%10101010
		.byte	%10101011
		.byte	%10101011
		.byte	%10101011
		.byte	%10101011
		.byte	%10101011
		.byte	%10101011
		.byte	%10101111
		; 7,3
		.byte	%10101111
		.byte	%10101111
		.byte	%11101111
		.byte	%11111111
		.byte	%00111111
		.byte	%00111111
		.byte	%00111111
		.byte	%00000000
		; 0,4
		.byte	%00000011
		.byte	%00111111
		.byte	%11111110
		.byte	%11101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		; 1,4
		.byte	%10101111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		; 2,4
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%10111111
		.byte	%11101111
		.byte	%11111010
		.byte	%11111111
		;
C43:	.byte	%00111111	; 1,1 (V) LOGO
		.byte	%00111111
		.byte	%11111111
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		;
		; 4,4
		.byte	%10101011
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		; 5,4
C45:	.byte	%11111111	; WHITE square
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		;
c46:	.byte	%11111111	; 0,2 (I) LOGO
		.byte	%01011111
		.byte	%01011100
		.byte	%01001100
		.byte	%00001100
		.byte	%00001110
		.byte	%00101110
		.byte	%10101110
		;
		; 7,4
		.byte	%11111110
		.byte	%11111110
		.byte	%11111110
		.byte	%11111110
		.byte	%11111110
		.byte	%11111110
		.byte	%11111110
		.byte	%11111110
		; 0,5
		.byte	%11111010
		.byte	%11101111
		.byte	%10101111
		.byte	%10101111
		.byte	%10101111
		.byte	%10101111
		.byte	%10101111
		.byte	%11101111
		; 1,5
		.byte	%11101111
		.byte	%11101111
		.byte	%11101111
		.byte	%11101111
		.byte	%11101111
		.byte	%11101111
		.byte	%11101111
		.byte	%11101111
		; 2,5
		.byte	%11101111
		.byte	%11101111
		.byte	%11101111
		.byte	%11101111
		.byte	%11101111
		.byte	%10111111
		.byte	%11111111
		.byte	%11101010
		;
c4B:	.byte	%10101110	; 1,2 (I) LOGO
		.byte	%10101111
		.byte	%11111111
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		; 4,5
		.byte	%11101010
		.byte	%11111110
		.byte	%11111110
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		; 5,5
		.byte	%11111111
		.byte	%11111010
		.byte	%11101010
		.byte	%11101111
		.byte	%11101111
		.byte	%11101111
		.byte	%10101010
		.byte	%10101010
		; 6,5
		.byte	%10111111
		.byte	%10111111
		.byte	%10111111
		.byte	%10101010
		.byte	%10101010
		.byte	%10111111
		.byte	%10111111
		.byte	%10111111
		; 7,5
		.byte	%10101010
		.byte	%10101010
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		; 0,6
		.byte	%10101111
		.byte	%11111011
		.byte	%11111010
		.byte	%11111010
		.byte	%11111010
		.byte	%11111010
		.byte	%11111010
		.byte	%11111011
		; 1,6
		.byte	%11111011
		.byte	%11111011
		.byte	%11111011
		.byte	%11111011
		.byte	%11111011
		.byte	%11111011
		.byte	%11111011
		.byte	%11111011
		; 2,6
		.byte	%11111011
		.byte	%11111011
		.byte	%11111011
		.byte	%11111011
		.byte	%11111011
		.byte	%11111110
		.byte	%11111111
		.byte	%10101011
		;
C53:	.byte	%11111111	; 0,3 (C) LOGO
		.byte	%00000010
		.byte	%00001010
		.byte	%00111111
		.byte	%10111111
		.byte	%10111111
		.byte	%10111111
		.byte	%10111111
		;
		; 4,6
		.byte	%10101111
		.byte	%10111111
		.byte	%10111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		; 5,6
		.byte	%11111111
		.byte	%10101111
		.byte	%10101011
		.byte	%11111011
		.byte	%11111011
		.byte	%11111011
		.byte	%10101010
		.byte	%10101010
		; 6,6
		.byte	%11111110
		.byte	%11111110
		.byte	%11111110
		.byte	%10101010
		.byte	%10101010
		.byte	%11111110
		.byte	%11111110
		.byte	%11111110
		; 7,6
		.byte	%10101010
		.byte	%10101010
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		; 0,7
		.byte	%11000000
		.byte	%11111100
		.byte	%10111111
		.byte	%10101011
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		; 1,7
		.byte	%11111010
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		; 2,7
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111110
		.byte	%11111011
		.byte	%10101111
		.byte	%11111111
		;
C5B:	.byte	%10101010	; 1,3 (C) LOGO
		.byte	%10101010
		.byte	%11111111
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		;
		; 4,7
		.byte	%11101010
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		;
C5D:	.byte	%11111111	; 0,4 (=) LOGO
		.byte	%11111111
		.byte	%11111111
		.byte	%10101111
		.byte	%10111111
		.byte	%11111111
		.byte	%01111111
		.byte	%01011111
		;
C5E:	.byte	%11111111	; 1,4 (=) LOGO
		.byte	%11111111
		.byte	%11111111
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		;
		; 7,7
		.byte	%10111111
		.byte	%10111111
		.byte	%10111111
		.byte	%10111111
		.byte	%10111111
		.byte	%10111111
		.byte	%10111111
		.byte	%10111111
		; 0,8
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%11000000
		.byte	%11111100
		.byte	%10111111
		.byte	%10101011
		.byte	%10101010
		; 1,8
		.byte	%10101010
		.byte	%10101010
		.byte	%11101010
		.byte	%11101010
		.byte	%11111010
		.byte	%11111010
		.byte	%11111010
		.byte	%11111010
		; 2,8
		.byte	%11111010
		.byte	%11111010
		.byte	%11111010
		.byte	%10111010
		.byte	%11101010
		.byte	%11111110
		.byte	%11111110
		.byte	%11111110
		; 3,8
		.byte	%11111110
		.byte	%10111110
		.byte	%10111110
		.byte	%10111110
		.byte	%10111110
		.byte	%10111110
		.byte	%10111110
		.byte	%10111110
		; 4,8
		.byte	%10111110
		.byte	%10111110
		.byte	%11111110
		.byte	%11111110
		.byte	%11101010
		.byte	%11101010
		.byte	%11101010
		.byte	%11101010
		; 5,8
		.byte	%11101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		; 6,8
		.byte	%10101010
		.byte	%11101010
		.byte	%11101010
		.byte	%11101010
		.byte	%11101010
		.byte	%11101010
		.byte	%11101010
		.byte	%11111010
		; 7,8
		.byte	%11111010
		.byte	%11111010
		.byte	%11111011
		.byte	%11111111
		.byte	%11111100
		.byte	%11111100
		.byte	%11111100
		.byte	%00000000
		; 0,9
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%11000000
		.byte	%11110000
		.byte	%10111100
		; 1,9
		.byte	%10101111
		.byte	%10101011
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
	; CKEY
	; 0,2
C6A:	.byte	%00000000
		.byte	%00000000
		.byte	%11111111
		.byte	%11111111
		.byte	%10111111
		.byte	%10111111
		.byte	%10111111
		.byte	%11101010
	; 1,2
C6B:	.byte	%11101010
		.byte	%11101011
		.byte	%11101011
		.byte	%11111111
		.byte	%11010111
		.byte	%11010111
		.byte	%11010101
		.byte	%11010101
	; 2,2
C6C:	.byte	%10111111
		.byte	%10111111
		.byte	%10111111
		.byte	%11111111
		.byte	%11111111
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		;
C6D:	.byte	%01010101	; 0,5 (2) LOGO
		.byte	%01011111
		.byte	%01111111
		.byte	%00110000
		.byte	%00000011
		.byte	%10101111
		.byte	%10111110
		.byte	%10111010
		;
		; 6,9
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101011
		.byte	%10101111
		; 7,9
		.byte	%10111100
		.byte	%11110000
		.byte	%11000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		;
C70:	.byte	%10111111	; 1,5 (2) LOGO
		.byte	%10111111
		.byte	%10101010
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		;
		; 1,10
		.byte	%00000000
		.byte	%11000000
		.byte	%11110000
		.byte	%10111100
		.byte	%10111100
		.byte	%10101111
		.byte	%10101011
		.byte	%10101011
	; CKEY
	; 0,3
C72:	.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%11000000
		.byte	%11000000
		.byte	%11000000
		.byte	%11000000
		.byte	%11000000
	; 1,3
C73:	.byte	%11000000
		.byte	%11000000
		.byte	%11000000
		.byte	%11000000
		.byte	%11000000
		.byte	%11000000
		.byte	%11000000
		.byte	%11000000
	; 2,3
C74:	.byte	%11000000
		.byte	%11000000
		.byte	%11000000
		.byte	%11000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		;
C75:	.byte	%01010101	; 0,6 (0) LOGO
		.byte	%01010101
		.byte	%11010111
		.byte	%11000011
		.byte	%11000011
		.byte	%10101011
		.byte	%10101011
		.byte	%10101011
		;
		; 6,10
		.byte	%10101011
		.byte	%10101011
		.byte	%10101111
		.byte	%10101100
		.byte	%10111100
		.byte	%11110000
		.byte	%11000000
		.byte	%00000000
		;
C77:	.byte	%11101011	; 1,6 (0) LOGO
		.byte	%11101010
		.byte	%10101010
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		;
C78:	.byte	%01010101	; 1,7 (0) LOGO
		.byte	%11110101
		.byte	%11111101
		.byte	%00001100
		.byte	%00001100
		.byte	%10101110
		.byte	%10101110
		.byte	%10101110
		;
C79:	.byte	%11111110	; 1,7 (0) LOGO
		.byte	%11111010
		.byte	%10101010
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		;
		; 2,11
		.byte	%11000000
		.byte	%11000000
		.byte	%11000000
		.byte	%11110000
		.byte	%10110000
		.byte	%10110000
		.byte	%10111100
		.byte	%10101100
		; 3,11
		.byte	%10101100
		.byte	%10101100
		.byte	%10101100
		.byte	%10101111
		.byte	%10101011
		.byte	%10101011
		.byte	%10101011
		.byte	%10101011
		; 4,11
		.byte	%10101011
		.byte	%10101011
		.byte	%10101011
		.byte	%10101011
		.byte	%10101111
		.byte	%10101100
		.byte	%10101100
		.byte	%10101100
		; 5,11
		.byte	%10101100
		.byte	%10111100
		.byte	%10110000
		.byte	%10110000
		.byte	%11110000
		.byte	%11000000
		.byte	%11000000
		.byte	%11000000
		; 6,11
		.byte	%11000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		;
C7F:	.byte	%00111100
		.byte	%01000010
		.byte	%10011101
		.byte	%10100001
		.byte	%10100001
		.byte	%10011101
		.byte	%01000010
		.byte	%00111100

