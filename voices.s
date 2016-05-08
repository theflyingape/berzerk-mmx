;*********************************************************************
; COMMODORE VIC 20: RAW sound player
; written by Robert Hurst <robert@hurst-ri.us>
; updated version: 13-Mar-2010
;
; acknowledgements and technical credits go to the Denial User Community,
; please visit us at http://sleepingelephant.com/denial/
;
; special thanks to:
; - Jeff-20 for his demands
; - darkatx for his code samples
; - Mayhem, Mike, and Nippur72 for their support
; - Pedro Lambrini for his pure enthusiasm
; - and all the rest that contributed useful feedback
; ... thank you all again!!
;
		.fileopt author,	"Robert Hurst"
        .fileopt comment,	"Berzerk MMX"
        .fileopt compiler,	"VIC 20 ASSEMBLER"

		.include "VIC-SSS-MMX.h"
		.segment "VOICES"

		.global	INTRUDER_ALERT
		.global	WAH_THE_WAH

INTRUDER_ALERT:
	lda #30
	sta $033e
	ldx #<SOUND_INTRUDER
	ldy #>SOUND_INTRUDER
	jsr PLAY_SOUND
	ldx #<SOUND_ALERT
	ldy #>SOUND_ALERT
	jsr PLAY_SOUND
	rts

WAH_THE_WAH:
	lda $033e
	and #$03
	asl
	asl
	clc
	adc #21		; pad rate
	sta $033e
	lda $033c
	lsr
	bcc @even
	ldx #<SOUND_KILL
	ldy #>SOUND_KILL
	bne @verb
@even:	ldx #<SOUND_DESTROY
	ldy #>SOUND_DESTROY
@verb:	jsr PLAY_SOUND
	;
	ldx #<SOUND_THE
	ldy #>SOUND_THE
	jsr PLAY_SOUND
	;
	lda $033d
	lsr
	bcs @odd
	ldx #<SOUND_HUMANOID
	ldy #>SOUND_HUMANOID
	bne @noun
@odd:	ldx #<SOUND_INTRUDER
	ldy #>SOUND_INTRUDER
@noun:	jsr PLAY_SOUND
	rts

PLAY_SOUND:
	stx $fa
	sty $fb
	ldy #0
@loop:
	lda ($fa),y
	beq @fini
@even:
	pha
	lsr
	lsr
	lsr
	lsr
	jsr @snd
@odd:
	pla
	and #$0f
	jsr @snd
@next:
	iny
	bne @loop
	inc $fb
	bne @loop
@fini:
	lda #$13	; keep white & volume low for sound effects
	sta $900e
	rts

@snd:
	ora #$f0	; make for a highlight
	sta $900e
	ldx $033e	; playback pace
@wait:
	dex
	bne @wait
	rts

;
;	MAME 0.36 Berzerk sound samples were converted from WAV to RAW:
;	sox {sample}.wav -r 5760 {sample}.raw
;
;	... then each high-nybble from a pair of bytes were packed:
;
;	#include <stdio.h>
;
;	void main(int c, char **v)
;	{
;		FILE *in, *out;
;		unsigned char b, o;
;
;		out = fopen("sound.vic", "w");
;
;		if((in = fopen(v[1], "r"))) {
;			while(!feof(in)) {
;				b = fgetc(in);
;				o = b & 240;
;				b = fgetc(in);
;				o |= b >> 4;
;				if (o) fputc(o, out);
;			}
;			fclose(in);
;		}
;
;		fclose(out);
;	}
;
;	... each resulting binary file is included below:
;
SOUND_ALERT:	.incbin "alert.bin"
				.byte $00

SOUND_DESTROY:	.incbin "destroy.bin"
				.byte $00

SOUND_HUMANOID:	.incbin "humanoid.bin"
				.byte $00

SOUND_INTRUDER:	.incbin "intruder.bin"
				.byte $00

SOUND_KILL:		.incbin "kill.bin"
				.byte $00

SOUND_THE:		.incbin "the.bin"
				.byte $00

