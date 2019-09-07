; Blinky.asm: toggles an LED attached to pin 17 (P3.7)
$MODLP51
org 0000H
ljmp myprogram
; When using a 22.1184MHz crystal in fast mode
; one cycle takes 1.0/22.1184MHz = 45.21123 ns
WaitHalfSec:
mov R2, #89
L3: mov R1, #250
L2: mov R0, #166
L1: djnz R0, L1 ; 3 cycles->3*45.21123ns*166=22.51519us
djnz R1, L2 ; 22.51519us*250=5.629ms
djnz R2, L3 ; 5.629ms*89=0.5s (approximately)
ret
myprogram:
mov SP, #7FH
M0:
cpl P3.7
lcall WaitHalfSec
sjmp M0
END