		.segment "SSSBUF"
		;
		; shoe horn this graphic sequence into VIC RAM
		; to keep this within +8K RAM expansion ...
DYING:
		; frame #0 (blue-yellow)
		.byte	%00000000
		.byte	%00011000
		.byte	%00011000
		.byte	%00000000
		.byte	%00111100
		.byte	%01011010
		.byte	%01011010
		.byte	%01011010
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00111100
		.byte	%00000000

		; frame #1 (red-cyan)
		.byte	%00011000
		.byte	%00100100
		.byte	%00100100
		.byte	%00011000
		.byte	%01000010
		.byte	%10100101
		.byte	%10100101
		.byte	%10100101
		.byte	%01100110
		.byte	%00100100
		.byte	%00100100
		.byte	%00100100
		.byte	%00100100
		.byte	%00100100
		.byte	%01000010
		.byte	%00111100

		; frame #2 (green-purple)
		.byte	%00011000
		.byte	%00111100
		.byte	%00111100
		.byte	%00011000
		.byte	%01111110
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%01111110
		.byte	%00111100
		.byte	%00111100
		.byte	%00111100
		.byte	%00111100
		.byte	%00111100
		.byte	%01111110
		.byte	%00111100

		; frame #3 (mag-green)
		.byte	%00011000
		.byte	%00100100
		.byte	%00100100
		.byte	%00100100
		.byte	%01000010
		.byte	%10000001
		.byte	%10000001
		.byte	%10000001
		.byte	%01000010
		.byte	%00100100
		.byte	%00100100
		.byte	%00100100
		.byte	%00100100
		.byte	%00100100
		.byte	%01000010
		.byte	%00111100

		.segment "GRAPHICS"

HERO:
		; moving right frame #0 (& standing)
		.byte	%00011000
		.byte	%00011000
		.byte	%00000000
		.byte	%00111100
		.byte	%01011010
		.byte	%01011010
		.byte	%01011010
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011100
		.byte	%00010000
		; moving right frame #1 / #3
		.byte	%00000000
		.byte	%00011000
		.byte	%00011000
		.byte	%00000000
		.byte	%00111100
		.byte	%01011100
		.byte	%01011100
		.byte	%00111110
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00010100
		.byte	%11110010
		.byte	%10000010
		.byte	%00000010
		.byte	%00000011
		; moving right frame #2
		.byte	%00011000
		.byte	%00011000
		.byte	%00000000
		.byte	%00111100
		.byte	%01011010
		.byte	%10011001
		.byte	%01011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00100100
		.byte	%00100010
		.byte	%01000001
		.byte	%01000001
		.byte	%10000001
		.byte	%10000001
		.byte	%00000000
OREH:	.global OREH
		; moving left frame #0
		.byte	%00011000
		.byte	%00011000
		.byte	%00000000
		.byte	%00111100
		.byte	%01011010
		.byte	%01011010
		.byte	%01011010
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00111000
		.byte	%00001000
		; moving left frame #1 / #3
		.byte	%00000000
		.byte	%00011000
		.byte	%00011000
		.byte	%00000000
		.byte	%00111100
		.byte	%00111010
		.byte	%00111010
		.byte	%01111100
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00101000
		.byte	%01001111
		.byte	%01000001
		.byte	%01000000
		.byte	%11000000
		; moving left frame #2
		.byte	%00011000
		.byte	%00011000
		.byte	%00000000
		.byte	%00111100
		.byte	%01011010
		.byte	%10011001
		.byte	%00011010
		.byte	%00011000
		.byte	%00011000
		.byte	%00100100
		.byte	%01000100
		.byte	%10000010
		.byte	%10000010
		.byte	%10000001
		.byte	%10000001
		.byte	%00000000

FIRING:
		; firing #0 (right)
		.byte	%00011000
		.byte	%00011000
		.byte	%00000000
		.byte	%00011111
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011100
		.byte	%00000000

		; firing #1 (down)
		.byte	%00011000
		.byte	%00011000
		.byte	%00000000
		.byte	%00111100
		.byte	%00111100
		.byte	%00111010
		.byte	%00111010
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011100
		.byte	%00000000

		; firing #2 (down-right)
		.byte	%00011000
		.byte	%00011000
		.byte	%00000000
		.byte	%00111100
		.byte	%01011100
		.byte	%01011010
		.byte	%01011010
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011100
		.byte	%00010000

		; firing #3 (left)
		.byte	%00011000
		.byte	%00011000
		.byte	%00000000
		.byte	%11111000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00111000
		.byte	%00000000

		; firing #4 (down-left)
		.byte	%00011000
		.byte	%00011000
		.byte	%00000000
		.byte	%00111100
		.byte	%00111010
		.byte	%01011010
		.byte	%01011010
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00111000
		.byte	%00001000

		; firing #5 (up)
		.byte	%00011000
		.byte	%00011000
		.byte	%00000000
		.byte	%00011101
		.byte	%00011011
		.byte	%00011001
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00111000
		.byte	%00000000

		; firing #6 (up-right)
		.byte	%00011000
		.byte	%00011001
		.byte	%00000010
		.byte	%00011100
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011100
		.byte	%00000000

		; firing #7 (up-left)
		.byte	%00011000
		.byte	%10011000
		.byte	%01000000
		.byte	%00111000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00111000
		.byte	%00000000

HEROHSHOT:
		.byte	%11110000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
HEROVSHOT:
		.byte	%10000000
		.byte	%10000000
		.byte	%10000000
		.byte	%10000000
		.byte	%10000000
HEROFSHOT:
		.byte	%00001000
		.byte	%00010000
		.byte	%00100000
		.byte	%01000000
		.byte	%10000000
HEROBSHOT:
		.byte	%10000000
		.byte	%01000000
		.byte	%00100000
		.byte	%00010000
		.byte	%00001000

NMEHSHOT:
		.byte	%11100000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
NMEVSHOT:
		.byte	%10000000
		.byte	%10000000
		.byte	%10000000
		.byte	%10000000
NMEFSHOT:
		.byte	%00010000
		.byte	%00100000
		.byte	%01000000
		.byte	%10000000
NMEBSHOT:
		.byte	%10000000
		.byte	%01000000
		.byte	%00100000
		.byte	%00010000
ROBOT0:
		; idle
		.byte	%00111100
IDLE:	.byte	%01100110
		.byte	%11111111
		.byte	%10111101
		.byte	%10111101
		.byte	%10111101
		.byte	%00111100
		.byte	%00100100
		.byte	%00100100
		.byte	%00100100
		.byte	%01100110
		.byte	%00000000
ROBOT1:
		; right
		.byte	%00111100
		.byte	%01111000
		.byte	%11111111
		.byte	%10111101
		.byte	%10111101
		.byte	%10111101
		.byte	%00111100
		.byte	%00100100
		.byte	%00100100
		.byte	%00100100
		.byte	%00110110
		.byte	%00000000
		; right #2
		.byte	%00111100
		.byte	%01111000
		.byte	%11111111
		.byte	%10111101
		.byte	%10111101
		.byte	%10111101
		.byte	%00111100
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00011100
		.byte	%00000000
ROBOT2:
		; down
		.byte	%00111100
		.byte	%01100110
		.byte	%11111111
		.byte	%10111101
		.byte	%10111101
		.byte	%10111101
		.byte	%00111100
		.byte	%00100100
		.byte	%00100100
		.byte	%00100110
		.byte	%00100000
		.byte	%01100000
		; down #2
		.byte	%00111100
		.byte	%01100110
		.byte	%11111111
		.byte	%10111101
		.byte	%10111101
		.byte	%10111101
		.byte	%00111100
		.byte	%00100100
		.byte	%00100100
		.byte	%01100100
		.byte	%00000100
		.byte	%00000110
ROBOT4:
		; left
		.byte	%00111100
		.byte	%00011110
		.byte	%11111111
		.byte	%10111101
		.byte	%10111101
		.byte	%10111101
		.byte	%00111100
		.byte	%00100100
		.byte	%00100100
		.byte	%00100100
		.byte	%01101100
		.byte	%00000000
		; left #2
		.byte	%00111100
		.byte	%00011110
		.byte	%11111111
		.byte	%10111101
		.byte	%10111101
		.byte	%10111101
		.byte	%00111100
		.byte	%00011000
		.byte	%00011000
		.byte	%00011000
		.byte	%00111000
		.byte	%00000000
ROBOT8:
		; up
		.byte	%00111100
		.byte	%01111110
		.byte	%11111111
		.byte	%10111101
		.byte	%10111101
		.byte	%10111101
		.byte	%00111100
		.byte	%00100100
		.byte	%00100100
		.byte	%00100110
		.byte	%00100000
		.byte	%01100000
		; up #2
		.byte	%00111100
		.byte	%01111110
		.byte	%11111111
		.byte	%10111101
		.byte	%10111101
		.byte	%10111101
		.byte	%00111100
		.byte	%00100100
		.byte	%00100100
		.byte	%01100100
		.byte	%00000100
		.byte	%00000110
EXPLODE:
		; frame #0
		.byte	%00000000
		.byte	%00011000
		.byte	%00111100
		.byte	%01111110
		.byte	%10111101
		.byte	%00101100
		.byte	%00101100
		.byte	%00110100
		.byte	%00111100
		.byte	%00100100
		.byte	%00100100
		.byte	%00100100
		.byte	%01100110
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		; frame #1
		.byte	%00000000
		.byte	%00011000
		.byte	%00100100
		.byte	%01000010
		.byte	%10100101
		.byte	%00101100
		.byte	%01001100
		.byte	%00110010
		.byte	%00111100
		.byte	%00100100
		.byte	%01000010
		.byte	%00100100
		.byte	%01100110
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		; frame #2
		.byte	%00100100
		.byte	%10000001
		.byte	%00110000
		.byte	%00000010
		.byte	%01000001
		.byte	%00000000
		.byte	%01000010
		.byte	%00000010
		.byte	%10100000
		.byte	%00011100
		.byte	%01000000
		.byte	%10000010
		.byte	%01000001
		.byte	%00000000
		.byte	%01000100
		.byte	%01100110
		; frame #3
		.byte	%10000001
		.byte	%01000010
		.byte	%00000000
		.byte	%00000000
		.byte	%10000001
		.byte	%00000000
		.byte	%00000000
		.byte	%10000001
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00011000
		.byte	%10000001
		.byte	%00000000
		.byte	%01000010
		.byte	%10000001
EVILOTTO:
		.byte	%00111100
		.byte	%01111110
		.byte	%11011011
		.byte	%11111111
		.byte	%11111111
		.byte	%10111101
		.byte	%01000010
		.byte	%00111100

