;*********************************************************************
; BERZERK MMX
; written by Robert Hurst <robert@hurst-ri.us>
; updated version: 30-Oct-2011

			.include	"VIC-SSS-MMX.h"

			.segment "SSSBUF"
			.global HISCORE
;
; runtime variables
;
FRAME:		.byte 0		; video frame counter for action/animation timing
						; bit# 4:FIRE, 3:UP, 2:LEFT, 1:DOWN, 0:RIGHT
ACTION:		.byte 0		; player's action this frame
ACTION2:	.byte 0		; player's action last frame
ACTIONS:	.byte 0		; frames this action was repeated
BONUS:		.byte 0		; bonus for clearing level
CORNERS:	.res  24	; maze wall corners
DEATH:		.byte 0		; hero death sequence
ENTRANCE:	.word 0		; JMP pointer
EXIT:		.byte 0		; closed bit# 3:UP, 2:LEFT, 1:DOWN, 0:RIGHT
EXTRA:		.byte 0		; extra life @ 5000-points, baby!
FIRE:		.byte 0		; >0: firing
HEROSHOT:	.res  2		; 
HISCORE:	.byte $00,$25,$00
KABOOM:		.byte 0		; exploding robot sound effect
LEVEL:		.byte 0		; 0-255
LIVES:		.byte 0		; 0-4
MAZE:		.res  15	; bit# 7:flag, 3:top, 2:left, 1:bottom, 0:right
MONO:		.byte 0		; monochrome color code used for this level
NMES:		.byte 0		; # of robots created for this level
NMEBITS:	.res  10	; which frame (1-3)
NMEPAUSE:	.byte 0		; 
NMERELOAD:	.byte 0		; progressive reload
NMESHOT:	.res  5		; 
NMESHOTS:	.byte 0		; # of bullets allowed for this level
NMESPEED:	.byte 0		; progressive walking
NMESSPEED:	.byte 0		; progressive shooting
NMEWAIT:	.res  10	; frame countdown
NMEX:		.res  10	;
NMEY:		.res  10	;
NMEZ:		.res  10	; 0=avail; 1=fixed; 8-15=sprite#
OTTO:		.byte 0		; speed (0-6)
OTTODY:		.byte 0		; 0:up, 1:down
OTTOJUMP:	.byte 0		; 0-1-3-7-15-31
OTTOTIMER:	.byte 2		; # of jiffym's (NTSC, PAL)
OTTOY:		.byte 0		; 
SCORE:		.res  3		; hero's score in BCD
SHOOTING:	.byte 0		; hero shooting sound effect
SHOOTING2:	.byte 0		; robot shooting sound effect
VAPORIZE:	.byte 0		; hero vaporizing sound sequence
;
; other global symbols
;
PLAYER		= 15		; sprite# of player
ROBOT		= 9			; starting sprite# of robot(s)
ROBOTS		= 6			; number of robot sprites
.global		CYCLOPS
.global		FRAME
.global		KABOOM
.global		MYIRQ
.global		RESTART
.global		SHOOTING

.ifdef VOICES
.global	INTRUDER_ALERT
.global	WAH_THE_WAH
.endif

;*********************************************************************
; GAME INITIALIZATION
;
		.segment "CODE"

RESTART:
		LDA #$08
		STA VIC+$0F		; black border / black screen
		JSR SSSINIT		; initialize software sprite stack
.ifdef VOICES
		JSR INTRUDER_ALERT
.endif
		;
		; evil otto
		LDA #%01101100	; disable an 8x8 sprite that floats X-Y
		LDY #8			; pixel height
		JSR SSSCREATE	; sprite #0 (4-chars: $78-$7F)
		LDX #<EVILOTTO
		LDY #>EVILOTTO
		JSR SSSANIM
		; explosion
		LDA #%00100001	; disable an 8x16 fixed sprite
		LDY #16			; pixel height
		JSR SSSCREATE	; sprite #1 (2-chars: $74-$77)
@nmeshots:				; setup robot shots
		LDA #%01100100	; disable an 8x8 sprite that floats X or Y
		LDY #4			; pixel height
		JSR SSSCREATE
		CPX #6			; sprites #2-6 (10-chars: $60-$73)
		BNE @nmeshots
@heroshots:				; setup hero shots
		LDA #%01100100	; disable an 8x8 sprite that floats X or Y
		LDY #5			; pixel height
		JSR SSSCREATE
		CPX #8			; sprites #7-8 (4-chars: $58-$5F)
		BNE @heroshots
		; setup robots
@nmes:	LDA #%01101101	; disable an 8x16 sprite that roams freely
		LDY #12			; pixel height
		JSR SSSCREATE
		CPX #10			; sprites #9-10 (12-chars: $40-$57)
		BNE @nmes
@nmeh:	LDA #%01101001	; disable an 8x16 sprite that roams X only
		LDY #12			; pixel height
		JSR SSSCREATE
		CPX #12			; sprites #11-12 (8-chars: $30-$3F)
		BNE @nmeh
@nmev:	LDA #%01100101	; disable an 8x16 sprite that roams Y only
		LDY #12			; pixel height
		JSR SSSCREATE
		CPX #14			; sprites #13-14 (6-chars: $24-$2F)
		BNE @nmev
		; our hero last
		LDA #%01001101	; disable an 8x16 sprite that floats X-Y
		LDY #16			; pixel height
		JSR SSSCREATE	; sprite #15 (6-chars: $18-$23)

;*********************************************************************
; Start a new game
;
INITNEW:
		LDA #-1
		STA LEVEL
		LDA #3
		STA LIVES
		LDA #0			; reset bonus & score
		STA EXTRA
		STA SCORE
		STA SCORE+1
		STA SCORE+2
		JSR NEXTLEVEL
.ifdef VOICES
		JSR INTRUDER_ALERT
.endif
		LDX #4			; a seasoned player's observation
		BNE MAYHEM
STARTLEVEL:
		LDA JIFFYL
		AND #$03
		ASL
		TAX
MAYHEM:	LDA DOORWAY,X
		STA ENTRANCE
		LDA DOORWAY+1,X
		STA ENTRANCE+1
		JSR RNDNEXT
		JMP (ENTRANCE)
DOORTOP:
		AND #$07
		CLC
		ADC #(4*8)
		TAY
		LDA JIFFYL
		AND #$18
		CLC
		ADC #88
		TAX
		BNE PLOTHERO
DOORBOT:
		AND #$07
		CLC
		ADC #(18*8)
		TAY
		LDA JIFFYL
		AND #$18
		CLC
		ADC #88
		TAX
		BNE PLOTHERO
DOORLFT:
		AND #$07
		CLC
		ADC #(4*8)
		TAX
		LDA JIFFYL
		AND #$1F
		CLC
		ADC #80
		TAY
		BNE PLOTHERO
DOORRHT:
		AND #$07
		CLC
		ADC #(20*8)
		TAX
		LDA JIFFYL
		AND #$1F
		CLC
		ADC #80
		TAY
		;
PLOTHERO:
		STX SPRITEX+PLAYER
		STY SPRITEY+PLAYER
		LDX #PLAYER
		STX sssNUM
		LDA SPRITEDEF,X
		ORA #$80
		STA SPRITEDEF,X
		LDA #$05		; green
		LDX #<HERO
		LDY #>HERO
		JSR SSSANIM
		LDA #0			; clear any pending shot(s) ...
		STA HEROSHOT	; don't want to shoot ourselves from a
		STA HEROSHOT+1	; bullet dispensed from the last level
		STA JIFFYL		; reset timer
		STA JIFFYM
		STA VIC+$0A		; voice #1
		LDY #25
		JSR SSSFLIP
		;
PLOTNME:
		LDA MONO
		STA COLORCODE
		LDA #8
		JSR RND
		CLC
		ADC #3			; min 3, max 10
		STA NMES
		;
		LDA #0
		LDX #10
@clr:	STA NMEBITS-1,X
		STA NMEWAIT-1,X
		STA NMEZ-1,X
		DEX
		BNE @clr
		;
		LDX #24
@zero:	STA CORNERS-1,X
		DEX
		BNE @zero
		;
		LDX #2
		STX NMERELOAD	; watchdog
		LDX #0
		STX R0
		STX DATANEXT
		;
@loopx:	LDY DATANEXT
		LDA #21
		JSR RND
		CPY DATANEXT
		BCC @goodx
		INC NMERELOAD	; allow for robots on same X or Y axis
		JMP @skip1
@goodx:	STA ACOPY
		TAX
		INC CORNERS,X
		LDA CORNERS,X
		CMP NMERELOAD	; watchdog
		BCS @loopx
		LDA ACOPY
		ASL
		ASL
		ASL				; x8
		BNE @x
		ADC #$08
@x:		ADC #$10
		LDX R0
		STA NMEX,X
@loopy:	LDY DATANEXT
		LDA #21
		JSR RND
		CPY DATANEXT
		BCC @goody
		INC NMERELOAD	; allow for robots on same X or Y axis
		BNE @skip1
@goody:	STA ACOPY
		TAX
		INC CORNERS,X
		LDA CORNERS,X
		CMP NMERELOAD	; watchdog
		BCS @loopy
		LDA ACOPY
		ASL
		ASL
		ASL				; x8
		BNE @y
		ADC #$08
@y:		ADC #$10
		LDX R0
		STA NMEY,X
		SEC
		SBC SPRITEY+PLAYER
		BCC @cc1
		CMP #24
		BCS @ok
		BCC @ckx
@cc1:	EOR #-1
		CMP #24
		BCS @ok
@ckx:	LDA NMEX,X
		SEC
		SBC SPRITEX+PLAYER
		BCC @cc2
		CMP #16
		BCS @ok
		JMP @loopx
@cc2:	EOR #-1
		CMP #16
		BCS @ok
@ng:	JMP @loopx
@ok:	JSR NMEFIX		; make this robot a fixture
		CPX R0
		BNE @ng			; oops, another robot in that spot
		INC R0
		;
@skip1:	LDX R0
		CPX NMES
		BEQ @mt
		JMP @loopx
@mt:	CPX #10
		BCS @ready
		LDA #0
		STA NMEX,X
		STA NMEY,X
		STA NMEZ,X
		INX
		BNE @mt
		;
@ready:	LDA #4
		STA R0
@flash:	LDA SPRITEDEF+PLAYER
		EOR #$80
		STA SPRITEDEF+PLAYER
		LDY #25
		JSR SSSFLIP
		DEC R0
		BNE @flash
		;
		LDA #12
		SEC
		SBC LEVEL
		BCS @adv
@limit:	LDA #3
@adv:	CMP #3
		BCC @limit
		ASL
		ASL
		ASL				; x8
		STA NMERELOAD
		LDX #1			; walking speed
		LDY #3			; shooting speed
		LDA LEVEL
		CMP #3
		BCC @level0
		LDA #3
		INY
@level0:
		STA NMESHOTS
		BEQ @level
		INX				; walking speed
		LDA LEVEL
@level1:
		CMP #7			; 1st white level
		BCC @level
		INY				; shooting speed
@level2:
		CMP #15			; 2nd white level
		BCC @level
		INC NMESHOTS
		INX				; walking speed
		INY				; shooting speed
@level3:
		LDA EXTRA
		BEQ @level
		INC NMESHOTS	; extra bullet
		INY				; shooting speed
@level:
		STX NMESPEED	; walking speed
		STY NMESSPEED	; shooting speed
		TXA
		ASL
		ASL
		ASL
		STA NMEPAUSE	; hero's head start
		;
		LDX #0			; speed
		STX JIFFYM		; timer
		STX OTTO
		STX OTTODY		; up
		LDY #15
		STY OTTOJUMP
		LDA SPRITEX+PLAYER
		SEC
		SBC #$08
		CMP #$80
		BCC @ottox
		CLC
		ADC #$10
@ottox:	STA SPRITEX
		LDA SPRITEY+PLAYER
		SEC
		SBC #$10
		CMP #$80
		BCC @ottoy
		CLC
		ADC #$20
@ottoy:	STA OTTOY
		;
		LDA #64			; NTSC=-1.0
		LDX MACHINE
		CPX #$05
		BEQ @ntsc
		ASL				; PAL=-2.75
@ntsc:	STA ACOPY
		LDA LEVEL
		LSR
		ADC ACOPY
		STA JIFFYL		; timer
		LDA #2			; add seconds: NTSC=+8.5, PAL=+10.25
		STA OTTOTIMER
		LDA NMES
		CMP #7
		BCC GAMELOOP
		INC OTTOTIMER	; cut the player some slack

;*********************************************************************
; Main game playing loop
;
GAMELOOP:
		; determine player, enemy, objects actions ... 
		JSR HEROMOVE
		JSR NMEMOVE
		JSR OTTOMOVE
		JSR OLDHEROSHOT
		JSR OLDNMESHOT
		;
		; update the action ... 
		LDY #1			; keep pace in sync, if ...
		LDA NMEPAUSE
		BNE @flip		; robots are not sprites yet?
		LDA NMES
		BEQ @flip		; no more robots on this level?
		LDY #0
@flip:	JSR SSSFFLIP
		INC FRAME
		;
		; determine results ... 
		JSR OTTOHIT
		JSR HEROHIT
		JSR NMEHIT
		JSR NMEDYING
		;
		; last, check if our hero left the room ... 
		LDA SPRITEX+PLAYER
		CMP #$11
		BCC @esclft
		CMP #$B8
		BCS @escrht
		LDA SPRITEY+PLAYER
		CMP #$11
		BCC @esctop
		CMP #$B0
		BCS @escbot
		JMP GAMELOOP
@esclft:
		JMP ESCLFT
@escrht:
		JMP ESCRHT
@esctop:
		JMP ESCTOP
@escbot:
		JMP ESCBOT

;*********************************************************************
; Hero death sequence
;
DIES:
		LDX #$10
		STX VAPORIZE
		LDX #$04
		STX DEATH
@die:	LDX #$00
		STX R4
@sic:	LDX #PLAYER		; hero sprite#
		STX sssNUM
		LDX R4
		LDA CYCLE,X
		STA COLORCODE
		LDY #>DYING
		TXA
		AND #$03
		ASL
		ASL
		ASL
		ASL				; x16
		CLC
		ADC #<DYING
		BCC @cc
		INY
@cc:	TAX
		LDA COLORCODE
		JSR SSSANIM
		; keep other events alive
		JSR OTTOMOVE
		JSR OLDHEROSHOT
		JSR OLDNMESHOT
		LDY #2			; burn some phosphor into the player's brain
		JSR SSSFLIP
		INC FRAME
		; keep post events alive
		JSR NMEHIT
		INC R4
		LDA R4
		CMP #$08
		BNE @sic
		DEC DEATH
		BNE @die
		;
		ASL SPRITEDEF+PLAYER
		LSR SPRITEDEF+PLAYER
		JSR SSSREFRESH
		LDY #1			; ashes-to-ashes
		JSR SSSFLIP
		LDY #10			; dust-to-dust
		JSR SSSFLIP
@wait:	LDX VAPORIZE
		BNE @wait		; sound effect complete?
		RTS

;*********************************************************************
; level end sequences: left, right, top, and bottom doorways
;
ESCAPE:
		STX ENTRANCE
		STY ENTRANCE+1
		JSR NEXTLEVEL
		JMP (ENTRANCE)

ESCLFT:
		JSR MAKEBLUE
		LDX #$00
		LDY SCRNLINE+1
		STX VECTORBG	; rows  0-10
		STY VECTORBG+1
		LDX #(11*22)
		STX VECTORFG	; rows 11-21
		STY VECTORFG+1
		LDA #22			; shift 22x
		STA R0
@bycol:	LDX #11			; 11-rows per half to scroll
		LDY #0
		STY R1
		LDY #20
@shift:	LDA (VECTORBG),Y
		INY				; shift right
		STA (VECTORBG),Y
		DEY
		LDA (VECTORFG),Y
		INY				; shift right
		STA (VECTORFG),Y
		DEY
		DEY
		CPY R1
		BPL @shift
		INY
		LDA #SSSNULL
		STA (VECTORBG),Y
		STA (VECTORFG),Y
		LDA R1
		CLC
		ADC #22
		STA R1
		CLC
		ADC #20
		TAY
		DEX
		BNE @shift
		LDA #4			; load # of vsyncs
		STA VSYNC
@vsync:	LDA VSYNC
		BNE @vsync		; and wait for it to occur
		DEC R0
		BNE @bycol
		LDA #%00000001	; close right exit
		LDX #<DOORRHT
		LDY #>DOORRHT
		JMP ESCAPE
ESCRHT:
		JSR MAKEBLUE
		LDX #$00
		LDY SCRNLINE+1
		STX VECTORBG	; rows  0-10
		STY VECTORBG+1
		LDX #(11*22)
		STX VECTORFG	; rows 11-21
		STY VECTORFG+1
		LDA #22			; shift 22x
		STA R0
@bycol:	LDX #11			; 11-rows per half to scroll
		LDY #22
		STY R1
		LDY #1
@shift:	LDA (VECTORBG),Y
		DEY				; shift left
		STA (VECTORBG),Y
		INY
		LDA (VECTORFG),Y
		DEY				; shift left
		STA (VECTORFG),Y
		INY
		INY
		CPY R1
		BCC @shift
		DEY
		LDA #SSSNULL
		STA (VECTORBG),Y
		STA (VECTORFG),Y
		LDA R1
		CLC
		ADC #22
		STA R1
		SEC
		SBC #21
		TAY
		DEX
		BNE @shift
		LDA #4			; load # of vsyncs
		STA VSYNC
@vsync:	LDA VSYNC
		BNE @vsync		; and wait for it to occur
		DEC R0
		BNE @bycol
		LDA #%00000100	; close left exit
		LDX #<DOORLFT
		LDY #>DOORLFT
		JMP ESCAPE

ESCTOP:
		JSR MAKEBLUE
		LDA #22
		STA R0			; 22x
@loop:	LDX #<(20*22)
		LDY SCRNLINE+1
		INY
		STX VECTORBG
		STY VECTORBG+1
		LDX #<(21*22)
		STX VECTORFG
		STY VECTORFG+1
		LDX #21			; 21x
@byrow:	LDY #21			; 22-cols
@shift:	LDA (VECTORBG),Y
		STA (VECTORFG),Y
		DEY
		BPL @shift
		LDA VECTORBG+1
		STA VECTORFG+1
		LDA VECTORBG
		STA VECTORFG
		SEC
		SBC #22
		BCS @nocc
		DEC VECTORBG+1
@nocc:	STA VECTORBG
		DEX
		BNE @byrow
		LDY #21
		LDA #SSSNULL
@clear:	STA (VECTORFG),Y
		DEY
		BPL @clear
		LDA #4			; load # of vsyncs
		STA VSYNC
@vsync:	LDA VSYNC
		BNE @vsync		; and wait for it to occur
		DEC R0
		BNE @loop
		LDA #%00000010	; close bottom exit
		LDX #<DOORBOT
		LDY #>DOORBOT
		JMP ESCAPE

ESCBOT:
		JSR MAKEBLUE
		LDA #22
		STA R0			; 22x
@loop:	LDX #22
		LDY SCRNLINE+1
		STX VECTORBG
		STY VECTORBG+1
		LDX #0
		STX VECTORFG
		STY VECTORFG+1
		LDX #21			; 21x
@byrow:	LDY #21			; 22-cols
@shift:	LDA (VECTORBG),Y
		STA (VECTORFG),Y
		DEY
		BPL @shift
		LDA VECTORBG+1
		STA VECTORFG+1
		LDA VECTORBG
		STA VECTORFG
		CLC
		ADC #22
		BCC @cc
		INC VECTORBG+1
@cc:	STA VECTORBG
		DEX
		BNE @byrow
		LDY #21
		LDA #SSSNULL
@clear:	STA (VECTORFG),Y
		DEY
		BPL @clear
		LDA #4			; load # of vsyncs
		STA VSYNC
@vsync:	LDA VSYNC
		BNE @vsync		; and wait for it to occur
		DEC R0
		BNE @loop
		LDA #%00001000	; close top exit
		LDX #<DOORTOP
		LDY #>DOORTOP
		JMP ESCAPE

MAKEBLUE:
		LDA BONUS
		CMP NMES
		BNE @nobonus
@loop:	LDA #$F8
		STA VIC+$0C		; voice #3
		LDX #2
		LDA #$10
		JSR SCOREUPDATE
		LDY #3
		JSR SSSFLIP
		LDA #$00
		STA VIC+$0C		; voice #3
		LDY #2
		JSR SSSFLIP
		DEC BONUS
		BNE @loop
@nobonus:
		LDX #$00
		LDY #$00
		JSR SSSPLOTS
		LDA SCRNLINE+1
		PHA				;++
		LDA COLORLINE+1
		PHA				;++
		LDY #10			; gratuitous
		JSR SSSFLIP
		;
		LDY #$00
		STY COLORLINE
		STY SCRNLINE
		PLA				;--
		STA COLORLINE+1
		PLA				;--
		STA SCRNLINE+1
		LDA #$06		; paint blue
@loop1:	STA (COLORLINE),Y
		INY
		BNE @loop1
		INC COLORLINE+1
@loop2:	STA (COLORLINE),Y
		INY
		CPY #$E2
		BNE @loop2
		LDA #20			; load # of vsyncs
		STA VSYNC
@vsync:	LDA VSYNC
		BNE @vsync		; and wait for it to occur
		;
		ASL SPRITEDEF+PLAYER
		LSR SPRITEDEF+PLAYER
		LDA #0
		STA JIFFYL		; reset timer
		STA JIFFYM
		STA VIC+$0A		; voice #1
.ifdef VOICES
		JSR WAH_THE_WAH
.endif
		RTS

;*********************************************************************
; process hero action(s)
;
HEROMOVE:
		LDX #PLAYER		; setup our hero
		STX sssNUM
		LDY #$00
		STY ACTION
		STY $9113
		LDA #$7F
		STA $9122
		;
		LDA $9120
		AND #$80
		BNE @joy0
		LDA #$01		; RIGHT
		STA ACTION
@joy0:	LDA #$FF
		STA $9122
		LDY $9111
		;
		TYA
		AND #$20		; FIRE
		BNE @xfire
		LDA #$10
		ORA ACTION
		STA ACTION
		;
@xfire:	TYA
		AND #$08
		BNE @joy1
		LDA #$02		; DOWN
		ORA ACTION
		STA ACTION
		;
@joy1:	TYA
		AND #$10
		BNE @joy2
		LDA #$04		; LEFT
		ORA ACTION
		STA ACTION
		;
@joy2:	TYA
		AND #$04
		BNE @joy3
		LDA #$08		; UP
		ORA ACTION
		STA ACTION
@joy3:
		LDA ACTION
		CMP ACTION2
		BEQ @oldact		; same as last poll
		STA ACTION2
		LDA #00			; reset poll on new action request
		STA ACTIONS
@oldact:
		INC ACTIONS
		LDA ACTIONS
		AND #$01		; repeat same action every other poll
		BNE @do
@xhero:	RTS
@do:
		LDX FIRE
		BEQ @do2
		DEC FIRE
@do2:	LDA ACTION
		AND #%00010000
		BEQ @moving
		LDA ACTION
		AND #%00001111	; which direction?
		BNE @firing
		JMP @move		; just aiming (or posing)
@firing:
		LDX FIRE
		BNE @fired
		LDX #11		; timer event
		STX FIRE
		ASL
		TAY
		LDA FIREAT,Y
		TAX
		LDA FIREAT+1,Y
		TAY
		STX SPRITEIMGL+PLAYER
		STY SPRITEIMGH+PLAYER
		JSR NEWHEROSHOT
@fired:	JMP @move
@moving:
		LDX #<HERO
		LDY #>HERO
		STX SPRITEIMGL+PLAYER
		STY SPRITEIMGH+PLAYER
		LDA ACTION
		AND #%00001111
		BNE @doing
		JMP @move
@doing:	LDA ACTIONS
		LSR
		AND #$03
		CMP #$03
		BEQ @ck0
		CMP #$02
		BEQ @r2
@r1:
		LDX #<(HERO+16)
		LDY #>(HERO+16)
		STX SPRITEIMGL+PLAYER
		STY SPRITEIMGH+PLAYER
		BNE @ck0
@r2:
		LDX #<(HERO+32)
		LDY #>(HERO+32)
		STX SPRITEIMGL+PLAYER
		STY SPRITEIMGH+PLAYER
@ck0:					; process RIGHT
		LDA ACTION
		AND #%00000001
		BEQ @ck1
		LDA SPRITEX+PLAYER
		CLC
		ADC #$08
		CMP SSSCLIPX
		BEQ @ck1
		INC SPRITEX+PLAYER
@ck1:					; process DOWN
		LDA ACTION
		AND #%00000010
		BEQ @ck2
		LDA SPRITEY+PLAYER
		CLC
		ADC #$18
		CMP SSSCLIPY
		BEQ @ck2
		INC SPRITEY+PLAYER
@ck2:					; process LEFT
		LDA ACTION
		AND #%00000100
		BEQ @ck3
		LDA SPRITEIMGL+PLAYER
		CLC
		ADC #$30
		BCC @lcc
		INC SPRITEIMGH+PLAYER
@lcc:	STA SPRITEIMGL+PLAYER
		LDA SPRITEX+PLAYER
		CMP #$10
		BEQ @ck3
		DEC SPRITEX+PLAYER
@ck3:					; process UP
		LDA ACTION
		AND #%00001000
		BEQ @move
		LDA SPRITEY+PLAYER
		CMP #$10
		BEQ @move
		DEC SPRITEY+PLAYER
@move:
		LDX SPRITEX+PLAYER
		LDY SPRITEY+PLAYER
		JSR SSSMOVEXY
		RTS

;*********************************************************************
; process a new Hero shot request, if possible
;
NEWHEROSHOT:
		LDX #$01
@mt:	LDA HEROSHOT,X
		BEQ @okbyme
		DEX
		BEQ @mt
		RTS
@okbyme:
		LDA ACTION
		AND #%00001111	; bit# 3=up, 2=left, 1=down, 0=right
		STA HEROSHOT,X	; save direction (1,2,3,4,6,8,9,12)
		TAY
		TXA
		CLC
		ADC #7			; sprite# offset
		TAX
		LDA MISSILE,Y	; enable display & collision w/ direction
		STA SPRITEDEF,X
		JSR SSSTOUCH
		TYA
		ASL				; compute shot image & origin
		TAY
		LDA SHOTAT,Y
		STA SPRITEIMGL,X
		LDA SHOTAT+1,Y
		STA SPRITEIMGH,X
		LDA SPRITEX+PLAYER
		CLC
		ADC ORIGIN,Y
		STA SPRITEX,X
		LDA SPRITEY+PLAYER
		CLC
		ADC ORIGIN+1,Y
		STA SPRITEY,X
		LDA #$F0
		STA SHOOTING
		RTS

;*********************************************************************
; process Hero shot(s), if any
;
OLDHEROSHOT:
		LDY #6			; fixed bullet speed
		LDA FRAME
		AND #$07
		TAX
		LDA MASK,X
		AND SPEED,Y
		BNE @action
		RTS
@action:
		LDX #1
		STX R0
@loop:	LDA HEROSHOT,X
		BNE @shoot
@loopx:	DEC R0	
		LDX R0
		BEQ @loop
		RTS
@shoot:
		TAY
		TXA
		CLC
		ADC #7			; sprite# offset
		TAX
		STX sssNUM
		LDA SPRITEZ,X
		AND #%1000		; collision?
		BNE @hit
		;
		LDA HEROSHOTDX,Y
		CLC
		ADC SPRITEX,X
		STA SPRITEX,X
		LDA HEROSHOTDY,Y
		CLC
		ADC SPRITEY,X
		STA SPRITEY,X
		LDA SPRITEY,X
		TAY
		LDA SPRITEX,X
		TAX
		CPX #10			; float shot off screen left
		BCC @xhit
		CPX #192		; float shot off screen right
		BCS @xhit
		CPY #10			; float shot off top screen
		BCC @xhit
		CPY #187		; float shot off bottom screen
		BCS @xhit
		LDX sssNUM
		JSR SSSTOUCH	; continue journey
		BNE @loopx
@hit:
		LDA SPRITEBACK,X
		CMP #$C
		BCC @xhit		; playfield?
		CMP #$E
		BCS @hit2
		LDY #-1			; C/D = fixed robot
		JSR NMEDIES
		JMP @xhit
@hit2:	CMP #$74
		BCS @xhit
		CMP #$60
		BCC @xhit
		SBC #$60		; NMESHOTs use chars $60-$73
		LSR
		LSR
		STA ACOPY		; /4 chars each shot sprite
		LDA #4
		SEC
		SBC ACOPY
		TAX
		LDA #0
		STA NMESHOT,X
		ASL SPRITEDEF+2,X
		LSR SPRITEDEF+2,X
@xhit:
		LDY R0
		LDA #0			; disable shot
		STA HEROSHOT,Y
		LDX sssNUM
		ASL SPRITEDEF,X
		LSR SPRITEDEF,X
		JMP @loopx

;*********************************************************************
; process hero collision detection
;
HEROHIT:
		LDA SPRITEZ+PLAYER
		AND #%1000
		BNE @hit
@fini:	RTS
@hit:	LDA SPRITEBACK+PLAYER
		CMP #$74
		BCS @die		; fixed robot or Otto?
		CMP #$58		; HEROSHOTS use chars $58-$5F
		BCC @die		; likely walked into a wall
		CMP #$60
		BCC @fini		; don't shoot ourself?
		SBC #$60		; NMESHOTs use chars $60-$73
		LSR
		LSR
		STA ACOPY		; /4 chars each shot sprite
		LDA #4
		SEC
		SBC ACOPY
		TAX
		LDA #0
		STA NMESHOT,X
		ASL SPRITEDEF+2,X
		LSR SPRITEDEF+2,X
@die:	JSR DIES
		PLA
		PLA
		LDA #0			; open all exits
		STA EXIT
		DEC LIVES
		BNE @cont
		JSR GAMEOVER
		JMP RESTART
@cont:	JSR NEWLEVEL
		JMP STARTLEVEL

;*********************************************************************
; make robot a fixture
;
NMEFIX:
		LDA #1			; allocate this slot
		STA NMEZ,X
		LDA NMEY,X
		TAY
		LDA NMEX,X
		TAX
		JSR SSSPEEKXY
		CMP #SSSNULL
		BNE @err
		TYA
		CLC
		ADC PLAYCOLS
		TAY
		LDA (SCRNLINE),Y
		CMP #SSSNULL
		BNE @err
		LDA MONO
		STA COLORCODE
		LDA #$C			; top half
		JSR SSSPOKE
		LDY CRSRROW
		INY
		LDX CRSRCOL
		JSR SSSPLOT
		LDA #$D			; bottom half
		JSR SSSPOKE
		LDX R0
		RTS
@err:
		LDX #-1
		RTS

;*********************************************************************
; process robot action(s)
;
NMEMOVE:
		LDY NMESPEED	; walking speed
		LDA FRAME
		AND #$07
		TAX
		LDA MASK,X
		AND SPEED,Y
		BNE @action
		RTS
@action:
		LDA NMEPAUSE
		BEQ @cont
		DEC NMEPAUSE
		RTS
@cont:
		LDY #0
@for:	LDA NMEBITS,Y	; exploding?
		BEQ @cont1
@skip:	JMP @next
@cont1:	LDA NMEZ,Y
		BEQ @skip		; empty?
		TAX
		STX sssNUM
		CPX #1
		BNE @2			; fixed?
@1:		JSR @upgrade
		CPX #(ROBOT+ROBOTS)
		BCS @skip		; sprite was available?
@2:		LDA #>ROBOT0
		STA SPRITEIMGH,X
		LDA #<ROBOT0
		STA SPRITEIMGL,X
		LDA LIVES
		BNE @play
		JMP @refresh
@play:
		LDA SPRITEDEF,X
		AND #%00000100
		BEQ @movex		; vertical?
		LDA NMEY,Y
		SEC
		SBC #2
		CMP SPRITEY+PLAYER
		BEQ @movex
		BCS @moveu
@moved:
		LDA #0
		STA VECTORBG
		LDA #12
		STA VECTORBG+1
		JSR @peekxy
		CMP #SSSNULL
		BNE @movex
		LDA NMEY,Y
		AND #$01
		BNE @imgd
		LDA #>ROBOT2
		STA SPRITEIMGH,X
		LDA #<ROBOT2
		BNE @imgd2
@imgd:	LDA #>(ROBOT2+12)
		STA SPRITEIMGH,X
		LDA #<(ROBOT2+12)
@imgd2:	STA SPRITEIMGL,X
		INC SPRITEY,X
		BNE @movex
@moveu:
		LDA #0
		STA VECTORBG
		LDA #-1
		STA VECTORBG+1
		JSR @peekxy
		CMP #SSSNULL
		BNE @movex
		LDA NMEY,Y
		AND #$01
		BNE @imgu
		LDA #>ROBOT8
		STA SPRITEIMGH,X
		LDA #<ROBOT8
		BNE @imgu2
@imgu:	LDA #>(ROBOT8+12)
		STA SPRITEIMGH,X
		LDA #<(ROBOT8+12)
@imgu2:	STA SPRITEIMGL,X
		DEC SPRITEY,X
@movex:
		LDA SPRITEDEF,X
		AND #%00001000
		BEQ @moveit		; horizontal?
		LDA NMEX,Y
		CMP SPRITEX+PLAYER
		BEQ @moveit
		BCS @movel
@mover:
		LDA #8
		STA VECTORBG
		LDA #0
		STA VECTORBG+1
		JSR @peekxy
		CMP #SSSNULL
		BNE @moveit
		LDA NMEX,Y
		AND #$01
		BNE @imgr
		LDA #>ROBOT1
		STA SPRITEIMGH,X
		LDA #<ROBOT1
		BNE @imgr2
@imgr:	LDA #>(ROBOT1+12)
		STA SPRITEIMGH,X
		LDA #<(ROBOT1+12)
@imgr2:	STA SPRITEIMGL,X
		INC SPRITEX,X
		BNE @moveit
@movel:
		LDA #-1
		STA VECTORBG
		LDA #0
		STA VECTORBG+1
		JSR @peekxy
		CMP #SSSNULL
		BNE @moveit
		LDA NMEX,Y
		AND #$01
		BNE @imgl
		LDA #>ROBOT4
		STA SPRITEIMGH,X
		LDA #<ROBOT4
		BNE @imgl2
@imgl:	LDA #>(ROBOT4+12)
		STA SPRITEIMGH,X
		LDA #<(ROBOT4+12)
@imgl2:	STA SPRITEIMGL,X
		DEC SPRITEX,X
@moveit:
		LDA SPRITEX,X
		STA NMEX,Y
		LDA SPRITEY,X
		STA NMEY,Y
@refresh:
		LDX sssNUM
		JSR SSSTOUCH	; refresh
@next:
		INY
		CPY NMES
		BCS @fire
		JMP @for
		;
@peekxy:
		STX XCOPY
		STY YCOPY
		LDA NMEX,Y
		CLC
		ADC VECTORBG
		TAX
		AND #$07
		BNE @doy		; X-aligned?
		INC VECTORBG
@doy:	LDA NMEY,Y
		CLC
		ADC VECTORBG+1
		TAY
		JSR SSSPEEKXY
		CMP #SSSNULL
		BNE @wall
		LDX VECTORBG
		BNE @cky
		LDX CRSRCOL
		INX
		LDY CRSRROW
		JSR SSSPEEK		; check next col, too
@cky:	LDY VECTORBG+1
		BNE @wall
		LDX CRSRCOL
		LDY CRSRROW
		INY
		JSR SSSPEEK		; check next row, too
@wall:	LDX XCOPY
		LDY YCOPY
		RTS
		;
@upgrade:
		LDX #ROBOT
@uloop:	LDA SPRITEDEF,X
		ASL
		BCC @mt			; already in use?
		INX
		CPX #(ROBOT+ROBOTS)
		BNE @uloop
		RTS
@mt:	STX sssNUM		; save this free sprite#
		JSR NMENULL		; erase old robot
		LDX sssNUM
		TXA
		STA NMEZ,Y		; upgraded to this sprite#
		LDA NMEX,Y
		STA SPRITEX,X
		LDA NMEY,Y
		CLC
		ADC #2			; +Y offset
		STA SPRITEY,X	; copy over position
		LDA #12
		STA SPRITEH,X
		LDA SPRITEDEF,X
		ORA #%11100000	; enable sprite, et al
		STA SPRITEDEF,X
		RTS
		;
@fire:	LDA LIVES
		BNE @play2
		RTS
@play2:	LDY #0
		STY R0
@for2:	LDA NMEZ,Y
		STA sssNUM		; robot sprite#
		BEQ @nextj
		LDA NMEBITS,Y	; exploding?
		BEQ @real
@nextj:	JMP @next2
@real:	LDA NMEWAIT,Y
		BEQ @round
		SEC
		SBC #1
		STA NMEWAIT,Y
		JMP @next2
@round:
		LDA NMEX,Y
		CLC
		ADC #6			; NMEORIGIN for down-X
		SEC
		SBC SPRITEX+PLAYER
		CMP #8
		BCS @notx
		LDX #2			; down
		LDA NMEY,Y
		CMP SPRITEY+PLAYER
		BCC @ud
		LDX #8			; up
@ud:	JSR @newnmeshot
		JMP @next2
@notx:
		LDA NMEY,Y
		CLC
		ADC #5
		SEC
		SBC SPRITEY+PLAYER
		CMP #16
		BCS @noty
		LDX #1			; right
		LDA NMEX,Y
		CMP SPRITEX+PLAYER
		BCC @lr
		LDX #4			; left
@lr:	JSR @newnmeshot
		JMP @next2
@noty:
		LDX #12			; (8+4) up-left
		STX XCOPY
		LDA NMEY,Y
		SEC
		SBC #2
		SBC SPRITEY+PLAYER
		BCS @cs1
		EOR #$FF
		LSR XCOPY		; (4+2) down-left
@cs1:	AND #$F8
		STA ACOPY
		LDA NMEX,Y
		SEC
		SBC SPRITEX+PLAYER
		BCS @cs2
		EOR #$FF
		LSR XCOPY		; (2+1) down-right
		LDX XCOPY
		CPX #3
		BEQ @cs2
		LDX #9
		STX XCOPY		; (8+1) up-right
@cs2:	AND #$F8
		CMP ACOPY
		BNE @next2
		LDX XCOPY
		JSR @newnmeshot
@next2:	INC R0
		LDY R0
		CPY NMES
		BCS @fini2
		JMP @for2
@fini2:	RTS
		;
@newnmeshot:
		STX XCOPY
		LDX NMESHOTS	; Mayhem's feedback
		BEQ @ez
@eh:	DEX
		LDA NMESHOT,X
		BEQ @okbyme
		CPX #0
		BNE @eh
@ez:	RTS
@okbyme:
		LDA NMERELOAD
		STA NMEWAIT,Y
		LDA XCOPY
		STA NMESHOT,X	; save direction (1,2,3,4,6,8,9,12)
		TAY
		TXA
		CLC
		ADC #2			; sprite# offset
		TAX
		STX XCOPY		; bullet sprite#
		LDA MISSILE,Y	; enable display & collision w/ direction
		STA SPRITEDEF,X
		JSR SSSTOUCH
		TYA
		ASL				; compute shot image & origin
		TAY
		LDA NMESHOTAT,Y
		STA SPRITEIMGL,X
		LDA NMESHOTAT+1,Y
		STA SPRITEIMGH,X
		LDX R0			; robot#
		LDA NMEX,X
		CLC
		ADC NMEORIGIN,Y
		LDX XCOPY		; bullet sprite#
		STA SPRITEX,X
		LDX R0			; robot#
		LDA NMEY,X
		CLC
		ADC NMEORIGIN+1,Y
		LDX XCOPY		; bullet sprite#
		STA SPRITEY,X
		LDA #$F0
		STA SHOOTING2
		RTS

;*********************************************************************
; process robot collision detection
;
NMEHIT:
		LDY #0
		STY R0
@loop:	LDA NMEZ,Y
		CMP #ROBOT
		BCC @next
		TAX
		LDA SPRITEZ,X
		AND #%1000
		BEQ @next
		LDA SPRITEBACK,X
		; HEROSHOTs use chars $58-$5F
		CMP #$58
		BCC @die
		CMP #$60
		BCS @cont
		LDX #1
		CMP #$5C		; sprite #8?
		BCC @hs
		DEX
@hs:	LDA #0
		STA HEROSHOT,X
		ASL SPRITEDEF+7,X
		LSR SPRITEDEF+7,X
		BNE @die
@cont:	CMP #$74
		BCS @die		; Otto eats his own, too
		SBC #$60		; NMESHOTs use chars $60-$73
		LSR
		LSR
		STA ACOPY		; /4 chars each shot sprite
		LDA #4
		SEC
		SBC ACOPY
		TAX
		LDA #0
		STA NMESHOT,X
		ASL SPRITEDEF+2,X
		LSR SPRITEDEF+2,X
@die:	JSR NMEDIES
@next:	INC R0
		LDY R0
		CPY NMES
		BNE @loop
		RTS

;*********************************************************************
; destroy a moving robot (Y) or find the fixed one (X=sprite#)
;
NMEDIES:
		CPY NMES
		BCC @erase		; kill this robot?
		LDY #0			; no, which fixed robot took the hit ...
@loop:	LDA NMEBITS,Y
		BNE @next
		LDA NMEZ,Y
		BEQ @next		; is this slot empty?
		LDA SPRITECX,X
		AND #$F8
		CMP NMEX,Y
		BNE @next
		LDA #0			; top half
		STA ACOPY
		LDA SPRITEBACK,X
		CMP #$C
		BEQ @half
		LDA #-8			; hit lower-half, account for delta in Y
		STA ACOPY
@half:	LDA SPRITECY,X
		AND #$F8
		CLC
		ADC ACOPY
		CMP NMEY,Y
		BEQ @erase
@next:	INY
		CPY NMES
		BNE @loop
		RTS				; timing could make this happen, so ignore for now
@erase:
		LDA #4
		STA NMEBITS,Y
		LDA #8
		STA NMEWAIT,Y
		JSR NMENULL		; erase
		LDA #16
		STA SPRITEH,X
		LDA #<EXPLODE
		STA SPRITEIMGL,X
		LDA #>EXPLODE
		STA SPRITEIMGH,X
		LDA NMEX,Y
		STA SPRITEX,X
		LDA NMEY,y
		STA SPRITEY,X
		LDA SPRITEDEF,X
		AND #%10111111	; disable collision-detection
		ORA #$80		; enable sprite
		STA SPRITEDEF,X
		JSR SSSTOUCH
		;
		INC BONUS		; collect this accrual if you clear this level
		LDA #$F8
		STA KABOOM
		LDX #2
		LDA #$50		; award 50-points
		JSR SCOREUPDATE
		RTS

;*********************************************************************
; Nullify a robot (Y) and return sprite# to use if it is destroyed
;
NMENULL:
		LDA NMEZ,Y
		TAX
		CPX #ROBOT
		BCS @fini		; sprite?
@fix:
		TYA
		PHA				;++
		LDA MONO
		STA COLORCODE
		LDA NMEX,Y
		TAX
		LDA NMEY,Y
		TAY
		JSR SSSPEEKXY
		LDA #SSSNULL
		JSR SSSPOKE
		LDX CRSRCOL
		LDY CRSRROW
		INY
		JSR SSSPLOT
		LDA #SSSNULL
		JSR SSSPOKE
		LDX #1			; fixed explosion
		PLA				;--
		TAY
@fini:	RTS

;*********************************************************************
; robot explosion sequence
;
NMEDYING:
		LDY #0
@loop:	LDA NMEBITS,Y
		BEQ @next
		TYA
		TAX
		DEC NMEWAIT,X
		BNE @next		; next frame?
		LDA #8
		STA NMEWAIT,Y	; reset timer for next frame
		LDA NMEZ,Y
		TAX
		STX sssNUM
		LDA SPRITEIMGL,X
		CLC
		ADC #16
		BCC @cc
		INC SPRITEIMGH,X
@cc:	STA SPRITEIMGL,X
		JSR SSSTOUCH
		TYA
		TAX
		DEC NMEBITS,X
		BNE @next		; more explosions?
		LDA #0
		STA NMEZ,Y		; eradicate monster
		LDX sssNUM
		ASL SPRITEDEF,X
		LSR SPRITEDEF,X
@next:	INY
		CPY NMES
		BNE @loop
		RTS

;*********************************************************************
; process robot shot(s), if any
;
OLDNMESHOT:
		LDA LEVEL
		BEQ @ez
		LDY NMESSPEED	; bullet speed
		LDA FRAME
		AND #$07
		TAX
		LDA MASK,X
		AND SPEED,Y
		BNE @action
@ez:	RTS
@action:
		LDX #0
		STX R0
@loop:	LDA NMESHOT,X
		BNE @shoot
@loopx:	INC R0
		LDX R0
		CPX NMESHOTS
		BCC @loop
		RTS
@shoot:
		TAY
		TXA
		CLC
		ADC #2			; sprite# offset
		TAX
		STX sssNUM
		LDA SPRITEZ,X
		AND #%1000		; collision?
		BNE @hit
		;
		LDA NMESHOTDX,Y
		CLC
		ADC SPRITEX,X
		STA SPRITEX,X
		LDA NMESHOTDY,Y
		CLC
		ADC SPRITEY,X
		STA SPRITEY,X
		LDA SPRITEY,X
		TAY
		LDA SPRITEX,X
		TAX
		CPX #11
		BCC @xhit
		CPX #192
		BCS @xhit
		CPY #11
		BCC @xhit
		CPY #188
		BCS @xhit
		LDX sssNUM
		JSR SSSTOUCH	; continue journey
		BNE @loopx
@hit:
		LDA SPRITEBACK,X
		CMP #$C
		BCC @xhit		; playfield?
		CMP #$E
		BCS @xhit
		LDY #-1			; fixed robot
		JSR NMEDIES
@xhit:
		LDY R0
		LDA #0			; disable shot
		STA NMESHOT,Y
		LDX sssNUM
		ASL SPRITEDEF,X
		LSR SPRITEDEF,X
		JMP @loopx

;*********************************************************************
; process evil otto action
;
OTTOMOVE:
		LDX OTTO
		BNE @alive
		LDA OTTOTIMER
		CMP JIFFYM
		BEQ @cont
@fini:	RTS				; not yet
@cont:	INC OTTO
		LDA SPRITEDEF
		ORA #$80		; enable
		STA SPRITEDEF
		BNE @anim
@alive:
		LDA FRAME
		AND #$07
		TAX
		LDA MASK,X
		LDX OTTO
		AND SPEED,X
		BNE @move
		RTS
@move:
		LDX SPRITEX+PLAYER
		CPX SPRITEX
		BEQ @x
		BCC @left
		INC SPRITEX
		BNE @x
@left:	DEC SPRITEX
@x:		LDY SPRITEY+PLAYER
		DEY
		DEY
		CPY OTTOY
		BEQ @y
		BCC @up
		INC OTTOY
		BNE @y
@up:	DEC OTTOY
@y:		LDA OTTODY
		LDX OTTOJUMP
		BEQ @rev		; reached top?
		CPX #16
		BCC @jumping	; reached bottom?
		LDY #2
		LDA EXTRA
		BEQ @ez			; this play is still early?
		INY				; bump base speed
@ez:	LDA NMES
		SEC
		SBC BONUS
		CMP #3
		BCS @busy		; too many distractions already?
		TYA
		ASL				; x2
		TAY
@busy:	LDA LIVES
		BNE @var
		LDY #5			; end-of-game speed
@var:	STY OTTO		; perhaps updating Otto's speed
		LDA OTTODY
@rev:	EOR #1			; reverse direction
		STA OTTODY
@jumping:
		CMP #1
		BNE @ascend
		SEC
		ROL OTTOJUMP
		BNE @anim
@ascend:
		LSR OTTOJUMP
@anim:
		LDA OTTOY
		CLC
		ADC OTTOJUMP
		STA SPRITEY
		LDX #0
		JSR SSSTOUCH
		RTS

;*********************************************************************
; process Evil Otto collision detection
;
OTTOHIT:
		LDX OTTO
		BEQ @fini		; alive?
		LDX #0			; sprite#
		LDA SPRITEZ,X
		AND #%1000		; collision?
		BEQ @fini
		LDA SPRITEBACK,X
		CMP #$C
		BCC @fini		; playfield?
		CMP #$E
		BCS @fini
		LDY #-1			; fixed robot
		JSR NMEDIES
@fini:	RTS

;*********************************************************************
; generate & render the next floor level
;
NEXTLEVEL:
		STA EXIT		; mask for any openings
		INC LEVEL
NEWPOOL:
		LDX #32
		STX NMERELOAD	; watchdog
		JSR RNDPOOL		; re-make a new set of sequences
		;
NEWLEVEL:
		LDX #0
		STX JIFFYM
		STX VIC+$0A
		STX BONUS
@off:	ASL SPRITEDEF,X
		LSR SPRITEDEF,X
		INX
		CPX #SPRITEMAX
		BNE @off
		;
		LDY #14
@seed1:	LDA MAZEDATA,Y
		STA MAZE,Y
		DEY
		BPL @seed1
		LDY #23
@seed2:	LDA XDATA,Y
		STA CORNERS,Y
		DEY
		BPL @seed2
		;
		LDY #0
		STY R2			; row 0-2
		LDY #1			; cell #1
		STY R1			; col 0-4
@maze:
		JSR RNDNEXT
		AND #%1111		; random wall(s)
		STA ACOPY
		ORA MAZE,Y		; merge with this area
		CMP #%10001111
		BEQ @mazenext	; skip 4-walled cells
		STA MAZE,Y
@mb0:	LSR	ACOPY		; right wall?
		BCC @mb1
		LDA R1
		CMP #4
		BEQ @mb1		; right-most cell?
		LDA MAZE+1,Y
		ORA #%0100		; add left wall
		STA MAZE+1,Y
@mb1:	LSR ACOPY		; bottom wall?
		BCC @mb2
		LDA R2
		CMP #2
		BEQ @mb2		; bottom-most cell?
		LDA MAZE+5,Y
		ORA #%1000		; add top wall
		STA MAZE+5,Y
@mb2:	LSR ACOPY		; left wall?
		BCC @mb3
		LDA R1
		BEQ @mb3		; left-most cell?
		LDA MAZE-1,Y
		ORA #%0001		; add right wall
		STA MAZE-1,Y
@mb3:	LSR ACOPY		; top wall?
		BCC @mazenext
		LDA R2
		BEQ @mazenext	; top-most cell?
		LDA MAZE-5,Y
		ORA #%0010		; add bottom wall
		STA MAZE-5,Y
@mazenext:
		INY				; next cell
		INC R1			; next col
		CPY #4			; skip top-right
		BEQ @mazenext
		CPY #7			; skip center
		BEQ @mazenext
		CPY #14			; skip bottom-right
		BEQ @validate
		LDA R1
		CMP #5
		BCC @maze		; do next col
		LDA #0
		STA R1			; reset col
		INC R2			; next row
		CPY #10			; skip bottom-left
		BEQ @mazenext
		JMP @maze		; do next row
@ng:	DEC NMERELOAD	; watchdog
		BEQ @ng2
		JMP NEWLEVEL	; redo from start
@ng2:	JMP NEWPOOL		; redo with new random mix
		;
@validate:
		LDA #0			; cell
		STA R0
		JSR CRAWL
		LDA MAZE+7		; allow for this rare exemption ...
		AND #$7F
		STA MAZE+7		; Paul Lynde in the center square
		;
		LDY #14
@clr:	LDA MAZE,Y
		CMP #$80
		BCS @ng
		DEY
		BPL @clr
		;
@exits:	; knock-down any exits
		LDA MAZE+2
		AND EXIT
		STA MAZE+2		; update top exit
		LDA MAZE+5
		AND EXIT
		STA MAZE+5		; update left exit
		LDA MAZE+9
		AND EXIT
		STA MAZE+9		; update right exit
		LDA MAZE+12
		AND EXIT
		STA MAZE+12		; update bottom exit
		;
		;== TOP ROW ==
@c1:	LDA MAZE+1
		AND #%0100		; left wall?
		BEQ @c2
		LDA #$05		; top-mid
		STA CORNERS+1
@c2:	LDA MAZE+2
		AND #%0100		; left wall?
		BEQ @c3
		LDA MAZE+2
		AND #%1000
		BNE @c2t		; top exit opened?
		LDA #$02		; top-right
		BNE @c2x
@c2t:	LDA #$05		; top-mid
@c2x:	STA CORNERS+2
@c3:	LDA MAZE+3
		AND #%0100		; left wall?
		BEQ @c4
		LDA MAZE+2
		AND #%1000
		BNE @c3t		; top exit opened?
		LDA #$01		; top-left
		BNE @c3x
@c3t:	LDA #$05		; top-mid
@c3x:	STA CORNERS+3
@c4:	LDA MAZE+4
		AND #%0100		; left wall?
		BEQ @c6
		LDA #$05		; top-mid
		STA CORNERS+4
		;== MID ROW #1 ==
@c6:	LDA MAZE+5
		AND #%1000		; top wall?
		BEQ @cm1
		LDA MAZE+5
		AND #%0100
		BNE @c6t		; left exit opened?
		LDA #$03		; bottom-left
		BNE @c6x
@c6t:	LDA #$08		; left-mid
@c6x:	STA CORNERS+6
@cm1:	LDX #7
		LDY #0
		JSR @xb
@c11:	LDA MAZE+9
		AND #%1000		; top wall?
		BEQ @c12
		LDA MAZE+9
		AND #%0001
		BNE @c11t		; right exit opened?
		LDA #$04		; bottom-right
		BNE @c11x
@c11t:	LDA #$06		; right-mid
@c11x:	STA CORNERS+11
		;== MID ROW #2 ==
@c12:	LDA MAZE+10
		AND #%1000		; top wall?
		BEQ @cm2
		LDA MAZE+5
		AND #%0100
		BNE @c12t		; left exit opened?
		LDA #$01		; top-left
		BNE @c12x
@c12t:	LDA #$08		; left-mid
@c12x:	STA CORNERS+12
@cm2:	LDX #13
		LDY #5
		JSR @xb
@c17:	LDA MAZE+14
		AND #%1000		; top wall?
		BEQ @c19
		LDA MAZE+9
		AND #%0001
		BNE @c17t		; right exit opened?
		LDA #$02		; top-right
		BNE @c17x
@c17t:	LDA #$06		; right-mid
@c17x:	STA CORNERS+17
		;== BOTTOM ROW ==
@c19:	LDA MAZE+11
		AND #%0100		; left wall?
		BEQ @c20
		LDA #$07		; bottom-mid
		STA CORNERS+19
@c20:	LDA MAZE+12
		AND #%0100		; left wall?
		BEQ @c21
		LDA MAZE+12
		AND #%0010
		BNE @c20t		; bottom exit opened?
		LDA #$04		; bottom-right
		BNE @c20x
@c20t:	LDA #$07		; bottom-mid
@c20x:	STA CORNERS+20
@c21:	LDA MAZE+13
		AND #%0100		; left wall?
		BEQ @c22
		LDA MAZE+12
		AND #%0010
		BNE @c21t		; bottom exit opened?
		LDA #$03		; bottom-left
		BNE @c21x
@c21t:	LDA #$07		; bottom-mid
@c21x:	STA CORNERS+21
@c22:	LDA MAZE+14
		AND #%0100		; left wall?
		BEQ @render
		LDA #$07		; bottom-mid
		STA CORNERS+22
;
;	              1         2
;	    0123456789012345678901
;	   +----------------------+
;	  0|X###X###X----X###X###X|0
;	  1|#   |   |    |   |   #|1
;	  2|#   |   |    |   |   #|2
;	  3|#   |   |    |   |   #|3
;	  4|#   |   |    |   |   #|4
;	  5|#   |   |    |   |   #|5
;	  6|#   |   |    |   |   #|6
;	  7|X---+---+----+---+---X|7
;	  8||   |   |    |   |   ||8
;	  9||   |   |    |   |   ||9
;	 10||   |   |    |   |   ||0
;	  1||   |   |    |   |   ||1
;	  2||   |   |    |   |   ||2
;	  3||   |   |    |   |   ||3
;	  4|X---+---+----+---+---X|4
;	  5|#   |   |    |   |   #|5
;	  6|#   |   |    |   |   #|6
;	  7|#   |   |    |   |   #|7
;	  8|#   |   |    |   |   #|8
;	  9|#   |   |    |   |   #|9
;	 20|#   |   |    |   |   #|0
;	  1|X###X###X----X###X###X|1
;	  2| 000000 &&&&&& 000000 |2
;	   +----------------------+
;	    0123456789012345678901
;
@render:
		LDA LEVEL
		AND #$07
		TAX
		LDA ROBOTCLR,X
		STA MONO
		STA COLORCODE
		LDA #SSSNULL	; empty cells
		JSR SSSCLEAR
		JSR SCORESTATUS
		LDY #22
		LDX #11
		JSR SSSPLOT		; cursor moves here
		JSR SSSPRINTS
		.byte $F5
		.asciiz "BERZERK MMX"
		;
		; first, do vertical bars only ...
@renderv:
		LDX #0
		STX R0			; cell: col + (0|5|10)
		STX R1			; col (0-4)
		STX R2			; row (0-2)
		LDA #%0100		; mask for vertical bar (left)
		STA R4			; mask
@loopv:
		LDX R0
		LDA MAZE,X
		AND R4
		BEQ @nextv		; any vertical edge requirements?
		LDA #$06		; blue walls
		LDY R1
		BEQ @lc
		CPY #5
		BCC @colorv
@lc:	LDY R2
		CPY #1
		BNE @colorv
		LDA #$02		; red walls
@colorv:
		STA COLORCODE
		LDX R2
		LDA FLOORY,X
		TAY
		LDX R1
		LDA FLOORX,X
		TAX
@drawv:
		JSR SSSPLOT
		LDA #$09		; vertical
		JSR SSSPOKE
		LDA CRSRROW
		LDX R2
		CMP FLOORY+1,X
		BEQ @nextv
		LDX CRSRCOL
		TAY
		INY
		BNE @drawv
@nextv:
		INC R2			; next row
		LDA R0
		CLC
		ADC #5
		STA R0			; next cell down
		TAX
		CPX #15
		BCC @loopv
		LDA #0
		STA R2			; reset row
		INC R1			; next col
		LDX R1
		STX R0			; reset cell to do next vertical wall sequence
		CPX #5
		BCC @loopv
		DEC R0
		LDA #%0001		; mask for vertical bar (right)
		STA R4
		CPX #6
		BCC @loopv
@dov:
		LDX #0
		STX R0			; cell: col + (0|5|10)
		STX R1			; col (0-4)
		STX R2			; row (0-2)
		STX R3			; corner (0-23)
		LDA #%1000		; top wall
		STA R4			; mask
@vloopy:				; vertical loop: render horizontal lines for each row
		LDY R2
		LDA FLOORY,Y
		TAY
		LDX R1
		LDA FLOORX,X
		TAX
		JSR SSSPLOT
@vloopx:
		LDX R3
		JSR DOCORNER
		LDA #$06		; blue walls
		STA COLORCODE
		INC R3			; next corner
		LDX R1
		CPX #2			; middle col?
		BNE @v3
		LDY R2
		CPY #1
		BEQ @hc
		CPY #2
		BEQ @hc
		LDX R0
		LDA MAZE,X
		AND R4			; exit walled?
		BEQ @hc
		LDA #$02		; red walls
		STA COLORCODE
@hc:	LDA #$A			; yes, 4-chars wide
		STA @vmid
		LDA #SSSNULL
		STA @vmid2
		LDA #0
		STA @vmid+1
		STA @vmid2+1
		BEQ @vcont
@v3:	LDA #0			; no, 3-chars wide
		STA @vmid
		STA @vmid2
		LDA #$EA		; NOP
		STA @vmid+1
		STA @vmid2+1
@vcont:	LDX R0
		LDA MAZE,X
		AND R4			; horizontal wall?
		BEQ @vnull
		JSR SSSPRINTS
		.byte $A,$A,$A
@vmid:	.byte $A,$0
		JMP @vnextx
@vnull:	LDA MONO
		STA COLORCODE
		JSR SSSPRINTS
		.byte SSSNULL,SSSNULL,SSSNULL
@vmid2:	.byte SSSNULL,$0
@vnextx:
		INC R0			; next cell
		INC R1			; next col
		LDX R1
		CPX #5
		BCS @vlastx
		JMP @vloopx
@vlastx:
		LDX R3
		JSR DOCORNER	; display right-most corner
		INC R3			; next corner for next line
@vnexty:
		LDX #0			; reset col for next horizontal wall sequence
		STX R1
		INC R2			; next row
		LDY R2
		CPY #3
		BCC @jmpy
		LDA #10			; reset cell to do bottom wall sequence
		STA R0
		LDA #%0010		; mask for horizontal bar (bottom)
		STA R4
		CPY #4
		BCS @doh
@jmpy:	JMP @vloopy
@doh:
		LDY #0			; immediate
		JSR SSSFLIP
		RTS
		;
@xb:	STX R3			; corner
		LDA #4
		STA R4			; counter
@xbdo:	LDA #0
		STA ACOPY
		LDA MAZE+1,Y
		AND #%0100		; check left
		BEQ @xb5
		LDA #%1000		; add top
		STA ACOPY
@xb5:	LDA MAZE+5,Y
		AND #%1000		; check top
		BEQ @xb6
		LDA #%0100		; add left
		ORA ACOPY
		STA ACOPY
@xb6:	LDA MAZE+6,Y
		AND #%1000		; check top
		BEQ @xb6x
		LDA #%0001		; add right
		ORA ACOPY
		STA ACOPY
@xb6x:	LDA MAZE+6,Y
		AND #%0100		; check left
		BEQ @xbx
		LDA #%0010		; add bottom
		ORA ACOPY
		STA ACOPY
@xbx:	LDX ACOPY
		LDA TILE,X
		LDX R3
		STA CORNERS,X
		INY				; next cell
		INC R3			; next corner
		DEC R4
		BNE @xbdo
		RTS

;*********************************************************************
; validate new floor
;
CRAWL:
		LDX R0
		LDA MAZE,X
		CMP #$80
		BCC @fini		; been here?
		AND #$7F		; clear accessible bit
		STA MAZE,X
@right:
		LSR
		BCS @down		; right wall?
		PHA				;++ push walls
		LDA R0
		PHA				;++ push cell
		INC R0
		JSR CRAWL		; explore right cell
		PLA				;-- pop cell
		STA R0
		PLA				;-- pop walls
@down:
		LSR
		BCS @left		; bottom wall?
		PHA				;++ push walls
		LDA R0
		PHA				;++ push cell
		CLC
		ADC #5
		STA R0
		JSR CRAWL		; explore cell below
		PLA				;-- pop cell
		STA R0
		PLA				;-- pop walls
@left:
		LSR
		BCS @up
		PHA				;++ push walls
		LDA R0
		PHA				;++ push cell
		DEC R0
		JSR CRAWL		; explore left cell
		PLA				;-- pop cell
		STA R0
		PLA				;-- pop walls
@up:
		LSR
		BCS @fini		; top wall?
		PHA				;++ push walls
		LDA R0
		PHA				;++ push cell
		SEC
		SBC #5
		STA R0
		JSR CRAWL		; explore cell above
		PLA				;-- pop cell
		STA R0
		PLA				;-- pop walls
@fini:	RTS

;*********************************************************************
; display a maze corner
;
DOCORNER:
		LDY #$06		; blue
		LDA CORNERS,X
		CMP #SSSNULL
		BNE @do
		LDY MONO
@do:	STY COLORCODE
		JSR SSSPRINT
@fini:	RTS

;*********************************************************************
; end of game sequence
;
GAMEOVER:
		MEGACART= $9D80	; memory bank register
		NVRAM	= $9C5A	; 3-bytes for Berzerk's hi-score
		; check for new hi score
		LDA SCORE
		LDY SCORE+1
		LDX SCORE+2
		CMP HISCORE
		BCC @print
		BNE @woot
		CPY HISCORE+1
		BCC @print
		BNE @woot
		CPX HISCORE+2
		BCC @print
		;
@woot:	PHA
		LDA #$FE
		STA MEGACART
		PLA
		STA HISCORE
		STY HISCORE+1
		STX HISCORE+2
		STA NVRAM
		STY NVRAM+1
		STX NVRAM+2
		;
@print:	LDX #11
		LDY #22
		JSR SSSPLOT
		JSR SSSPRINTS
		.byte	$F3,SSSNULL,$88,$89,$BA,$F1,$00
		LDX #$00
@loop:	LDA HISCORE,X
		LSR
		LSR
		LSR
		LSR
		CLC
		ADC #$0E
		JSR SSSPRINT
		LDA HISCORE,X
		AND #$0F
		CLC
		ADC #$0E
		JSR SSSPRINT
		INX
		CPX #$03
		BNE @loop
		LDA MONO
		STA COLORCODE
		LDA #$A0
		JSR SSSPRINT
		LDA #0
		STA KEYCHARS	; empty keyboard buffer
		STA JIFFYL
		LDX OTTOTIMER
		DEX
		STX JIFFYM
@rnd:
		JSR RNDNEXT
		AND #$0F
		ASL
		ASL
		ASL				; x8
		CLC
		ADC #8*3
		TAY
		JSR RNDNEXT
		AND #$0F
		ASL
		ASL
		ASL				; x8
		CLC
		ADC #8*4
		TAX
		STX SPRITEX+PLAYER
		STY SPRITEY+PLAYER
		LDY #0
@nme:	LDA NMEZ,Y
		CMP #2
		BCC @next
		LDA NMEX,Y
		STA SPRITEX+PLAYER
		LDA NMEY,Y
		STA SPRITEY+PLAYER
@next:	INY
		CPY #10
		BNE @nme
		;
@key:	INC FRAME
		BEQ @rnd
		LDX #$06
		LDY #$0B
		JSR SSSPLOT		; at center of screen
		LDA JIFFYL
		AND #$40
		BNE @text
		LDA MONO
		STA COLORCODE
		JSR SSSPRINTS
		.asciiz "          "
		JMP @anim
@text:	JSR SSSPRINTS
		.byte	$F2
		.asciiz "GAME"
		INC CRSRCOL
		INC CRSRCOL
		JSR SSSPRINTS
		.asciiz "OVER"
@anim:	JSR NMEMOVE
		JSR OTTOMOVE
		JSR OLDHEROSHOT
		JSR OLDNMESHOT
		JSR SSSREFRESH
		LDY #2			; no rush
		JSR SSSFFLIP
		JSR NMEHIT
		JSR NMEDYING
		LDX OTTO
		BEQ @key		; no Otto yet?
		LDA $028D
		AND #$02		; got C= key?
		BNE @fini
		JSR GETIN
		BNE @fini		; got a keystroke?
		LDA #$FF
		STA $9122
		LDA $9111
		AND #$20		; got FIRE ?
		BNE @key
@fini:	RTS

;*********************************************************************
; Generate a pool of random integers (0-99)
; NOTE: floating point work here will corrupt DIRTYLINE2 in FAC#1
;
RNDPOOL:
		LDA #192
		STA DATANEXT
@loop:
		JSR $E094		; rnd: perform BASIC RND
		JSR $DAE2		; mul10: Multiply FAC#1 by 10
		JSR $DAE2		; mul10: Multiply FAC#1 by 10
		JSR $D1AA		; facinx: FAC#1 to Integer in (AC/YR)
		TYA				; A is always zero, make A = 0-99
		LDX DATANEXT	; store result in cassette buffer
		STA DATASETTE-1,X
		DEC DATANEXT
		BNE @loop
		RTS

;*********************************************************************
; Retrieve a random int
; Pass A with a CEILING value, i.e., 1 - 100
; returns A (0 thru CEILING-1)
;
RND:
		STA $BA
@retry:	JSR RNDNEXT
		CMP $BA
		BCS @retry
		RTS

;*********************************************************************
; Retrieve next random int
; returns A (0-99)
;
RNDNEXT:
		INC DATANEXT
		LDX DATANEXT
		CPX #$C0		; 192 buffered values
		BCC @cont
		LDX #$00
		STX DATANEXT
@cont:	LDA DATASETTE,X
		RTS

;*********************************************************************
; Show player's score
;
SCORESTATUS:
		LDA #$01		; white
		STA COLORCODE
		LDX #1
		LDY #22
		JSR SSSPLOT
		LDX #$00
@loop:	LDA SCORE,X
		LSR
		LSR
		LSR
		LSR
		CLC
		ADC #$0E
		JSR SSSPRINT
		LDA SCORE,X
		AND #$0F
		CLC
		ADC #$0E
		JSR SSSPRINT
		INX
		CPX #$03
		BNE @loop
		;
		LDA #$05		; green
		STA COLORCODE
		LDX LIVES
		CPX #4
		BEQ @lives
		LDA #$A0
		JSR SSSPRINT
@lives:	DEX
		BEQ @fini
		LDA #$00		; life icon
		JSR SSSPRINT
		JMP @lives
@fini:	RTS

;*********************************************************************
; Update player's score
; send A with decimal number to add
; send X with placeholder (2=1s, 1=100s, 0=10,000s)
;
SCOREUPDATE:
		LDY LIVES
		BEQ @fini		; no score if you're dead already ...
		SED
		CLC
		ADC SCORE,X
		STA SCORE,X
		BCC @cc
@cs:	DEX				; too bad if 1,000,000 is breached ... no one
		LDA SCORE,X		; likes a smarty-pants!  :P
		CLC
		ADC #$01
		STA SCORE,X
		BCS @cs
@cc:	CLD
		LDX EXTRA
		BNE @show
		LDA SCORE+1
		CMP #$50
		BCC @show
		INC EXTRA		; woot!
		INC LIVES
@show:	JSR SCORESTATUS
@fini:	RTS

;*********************************************************************
; Background software interrupt routine
;
MYIRQ:
		CLD
		LDA JIFFYL
		LSR
		LSR
		LSR
		AND #$07
		TAX
		LDA EYEBALL,X
		STA CYCLOPS
		STA IDLE
@snd1:
		LDX VAPORIZE
		BEQ @snd2
		TXA
		SEC
		SBC #$08
		STA VAPORIZE
		CMP #$B0
		BCS @eff1
		LDA DEATH
		TAX
		BEQ @nxt1
		ASL
		ASL
		ASL
		EOR #$E8
@nxt1:	STA VAPORIZE
@eff1:	STX VIC+$0C		; voice #3
@snd2:
		LDX SHOOTING
		BEQ @snd3
		DEX
		DEX
		CPX #$A0
		BCS @eff2
		LDX #$00
@eff2:	STX SHOOTING
		STX VIC+$0B		; voice #2
@snd3:
		LDX SHOOTING2
		BEQ @snd4
		DEX
		DEX
		CPX #$A0
		BCS @eff3
		LDX #$00
@eff3:	STX SHOOTING2
		STX VIC+$0A		; voice #1
@snd4:
		LDX KABOOM
		BEQ @snd5
		LDA JIFFYL
		AND #$01
		BEQ @reg4
		DEX
		DEX
		DEX
		TXA
		CMP #$C0
		BCS @eff4
		LDA #$00
@eff4:	STA KABOOM
@reg4:	STA VIC+$0D		; noise
@snd5:
		LDA LIVES
		BEQ @v1
		LDA OTTOTIMER
		SEC
		SBC JIFFYM
		CMP #$01
		BNE @cont
		LDA JIFFYL
		CMP #$40
		BCC @cont
		AND #$20
		BNE @v1
		LDA #$80		; INTRUDER ALERT!
@v1:	STA VIC+$0A		; voice #1
@cont:	JMP SSSIRQ		; process synchronous flipping

;*********************************************************************
; Game data
;
		.segment "RODATA"

		; level vitals
				;blu,red,grn,mag,yel,cyn,mag,grn,blu
CYCLE:	.byte	$06,$02,$05,$04,$07,$03,$04,$05,$06
DOORWAY:
		.word	DOORTOP,DOORBOT,DOORLFT,DOORRHT
EYEBALL:
		.byte	%01100110
		.byte	%01110010
		.byte	%01111010
		.byte	%01110010
		.byte	%01100110
		.byte	%01001110
		.byte	%01011110
		.byte	%01001110
ROBOTCLR:		;yel,red,red,cyn,cyn,mag,mag,wht
		.byte	$07,$02,$02,$03,$03,$04,$04,$01
		; floor vitals
FLOORX:	.byte	0,4,8,13,17,21
FLOORY:	.byte	0,7,14,21
MAZEDATA:		;bit# 3=top, 2=left, 1=bottom, 0=right
		.byte	%10001100,%10001000,%10001000,%10001000,%10001001
		.byte	%10000100,%10000000,%10000000,%10000000,%10000001
		.byte	%10000110,%10000010,%10000010,%10000010,%10000011
XDATA:	.byte	$1,$A,$A,$A,$A,$2
		.byte	$9,SSSNULL,SSSNULL,SSSNULL,SSSNULL,$9
		.byte	$9,SSSNULL,SSSNULL,SSSNULL,SSSNULL,$9
		.byte	$3,$A,$A,$A,$A,$4
		; custom walls
TILE:	.byte	SSSNULL							; empty
		.byte	$A								; right
		.byte	$9,$1							; bottom
		.byte	$A,$A,$2,$5						; left
		.byte	$9,$3,$9,$8,$4,$7,$6,$B			; top
FIREAT:	.word 0				; 0: -n/a-
		.word FIRING+(0*16)	; 1: right
		.word FIRING+(1*16)	; 2: down
		.word FIRING+(2*16) ; 3: down-right
		.word FIRING+(3*16) ; 4: left
		.word 0				; 5: -n/a-
		.word FIRING+(4*16) ; 6: down-left
		.word 0				; 7: -n/a-
		.word FIRING+(5*16) ; 8: up
		.word FIRING+(6*16)	; 9: up-right
		.word 0				;10: -n/a-
		.word 0				;11: -n/a-
		.word FIRING+(7*16) ;12: up-left
SHOTAT:	.word 0				; 0: -n/a-
		.word HEROHSHOT		; 1: right
		.word HEROVSHOT		; 2: down
		.word HEROBSHOT		; 3: down-right
		.word HEROHSHOT		; 4: left
		.word 0				; 5: -n/a-
		.word HEROFSHOT		; 6: down-left
		.word 0				; 7: -n/a-
		.word HEROVSHOT		; 8: up
		.word HEROFSHOT		; 9: up-right
		.word 0				;10: -n/a-
		.word 0				;11: -n/a-
		.word HEROBSHOT		;12: up-left
MISSILE:.byte %00000000		; 0: -n/a-
		.byte %11101000		; 1: right
		.byte %11100100		; 2: down
		.byte %11100100		; 3: down-right
		.byte %11101000		; 4: left
		.byte %00000000		; 5: -n/a-
		.byte %11100100		; 6: down-left
		.byte %00000000		; 7: -n/a-
		.byte %11100100		; 8: up
		.byte %11100100		; 9: up-right
		.byte %00000000		;10: -n/a-
		.byte %00000000		;11: -n/a-
		.byte %11100100		;12: up-left
ORIGIN:	.byte 0,0			; 0: -n/a-
		.byte 4,3			; 1: right
		.byte 6,5			; 2: down
		.byte 5,4			; 3: down-right
		.byte 0,3			; 4: left
		.byte 0,0			; 5: -n/a-
		.byte -2,4			; 6: down-left
		.byte 0,0			; 7: -n/a-
		.byte 7,0			; 8: up
		.byte 4,0			; 9: up-right
		.byte 0,0			;10: -n/a-
		.byte 0,0			;11: -n/a-
		.byte -1,0			;12: up-left
HEROSHOTDX:
		.byte 0,2,0,1,-2,0,-1,0,0,1,0,0,-1
HEROSHOTDY:
		.byte 0,0,2,1,0,0,1,0,-2,-1,0,0,-1
NMEORIGIN:
		.byte 0,0			; 0: -n/a-
		.byte 8,5			; 1: right
		.byte 6,12			; 2: down
		.byte 6,10			; 3: down-right
		.byte -3,5			; 4: left
		.byte 0,0			; 5: -n/a-
		.byte -1,10			; 6: down-left
		.byte 0,0			; 7: -n/a-
		.byte 6,-4			; 8: up
		.byte 6,-2			; 9: up-right
		.byte 0,0			;10: -n/a-
		.byte 0,0			;11: -n/a-
		.byte -1,-2			;12: up-left
NMESHOTDX:
		.byte 0,1,0,1,-1,0,-1,0,0,1,0,0,-1
NMESHOTDY:
		.byte 0,0,1,1,0,0,1,0,-1,-1,0,0,-1
NMESHOTAT:
		.word 0				; 0: -n/a-
		.word NMEHSHOT		; 1: right
		.word NMEVSHOT		; 2: down
		.word NMEBSHOT		; 3: down-right
		.word NMEHSHOT		; 4: left
		.word 0				; 5: -n/a-
		.word NMEFSHOT		; 6: down-left
		.word 0				; 7: -n/a-
		.word NMEVSHOT		; 8: up
		.word NMEFSHOT		; 9: up-right
		.word 0				;10: -n/a-
		.word 0				;11: -n/a-
		.word NMEBSHOT		;12: up-left
SPEED:
		.byte	%00000000
		.byte	%00001000	; robot speed level 0
		.byte	%10001000	; robot speed level 2-6
		.byte	%10001010	; robot speed level 7-14; shot speed 0-2
		.byte	%10101010	; robot speed level 15+; shot speed 3-6
		.byte	%10101101	; robot speed level 15+ after extra life; shot speed 7-14
		.byte	%01110111	; hero bullet speed (x2 pixels); shot speed 15+
		.byte	%01111111	; shot speed 15+ after extra life

		.include "graphics.s"

