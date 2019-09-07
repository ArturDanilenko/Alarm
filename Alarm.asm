; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P3.7 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'BOOT' pushbutton connected to P4.5 is pressed.
$NOLIST
$MODLP51
$LIST

; There is a couple of typos in MODLP51 in the definition of the timer 0/1 reload
; special function registers (SFRs), so:

TIMER0_RELOAD_L DATA 0xf2
TIMER1_RELOAD_L DATA 0xf3
TIMER0_RELOAD_H DATA 0xf4
TIMER1_RELOAD_H DATA 0xf5

CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 500     ; 1000Hz, for a timer tick of 1ms rn 4000hz=0.25ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

BOOT_BUTTON   equ P4.5 ;29
SOUND_OUT     equ P3.7 ;17
SecondsB        equ P0.0 ;39
MinutesB		equ P0.2;37
HoursB			equ P0.5;34
AlarmTurnOff 	equ P0.6;
DayB			equ P0.7;36
setalarm 		equ p0.1;

; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count1ms:     ds 2 ; Used to determine when half second has passed
BCD_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
AlarmSec: ds 1
AlarmMin: ds 1
AlarmSec2: ds 1
AlarmMin2: ds 1
Minute_counter: ds 1 ;1 byte (8 bits) 

Temp_Reg: ds 1;

; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
AM_PM: dbit 1 ;
Alarm_flag: dbit 1
Alarm_reset_flag: dbit 2
AlarmHour: dbit 4
AlarmAMPM: dbit 1
AlarmDay2: dbit 3
AlarmHour2: dbit 4
AlarmAMPM2: dbit 1
AlarmDay: dbit 3
Hour_counter: dbit 4 ;
Day_counter: dbit 4 ;

cseg
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P1.1
LCD_RW equ P1.2
LCD_E  equ P1.3
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5
$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;                     1234567890123456    <- This helps determine the location of the counter
Initial_Message:  db 'Time: h:m:s xm d', 0
Initial_Message2:  db 'xx:xx:xx        ', 0
Initial_Message3:  db '                ', 0
Initial_Message4:  db 'Alarm Setup     ', 0

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Set autoreload value
	mov TIMER0_RELOAD_H, #high(TIMER0_RELOAD)
	mov TIMER0_RELOAD_L, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	cpl SOUND_OUT ; Connect speaker to P3.7!
	reti

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret
InAlarm2: 
	mov Alarm_Reset_Flag, #0x0 
	mov a, Temp_Reg
	ljmp Timer2_ISR_da
TurningOff:;==================================EXITING ALARM SETUP ARF = 0=========================================================
	cjne a, #0x1, InAlarm2
	mov Alarm_Reset_Flag, #0x2 
	mov a, Temp_Reg
	ljmp Timer2_ISR_da
AlarmFlagAct:;================================Entering Alarm Setup ARF = 1 ===========================================================
	mov Temp_reg, a
	mov a, Alarm_Reset_Flag
	cjne a, #0x0, TurningOff
	mov Alarm_Reset_Flag, #0x1
	mov a, BCD_counter
	add a, #0x001
	ljmp Timer2_ISR_da
;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	cpl P3.6 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	; Check if half second has passed
	mov a, Count1ms+0
	cjne a, #low(500), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(500), Timer2_ISR_done
	
	jnb AlarmTurnOff, AlarmOn
	
	; 500 milliseconds have passed.  Set a flag so the main program knows
	setb half_seconds_flag ; Let the main program know half second had passed
	mov a, Day_counter
	cjne a, AlarmDay, AlarmOff
	mov a, Hour_counter
	cjne a, AlarmHour, AlarmOff
	mov a, Minute_counter
	cjne a, AlarmMin, AlarmOff
	mov a, BCD_counter
	cjne a, AlarmSec, AlarmOff
	ljmp AlarmOn
	mov a, Day_counter
AlarmOff:
	cjne a, AlarmDay2, AlarmOff1
	mov a, Hour_counter
	cjne a, AlarmHour2, AlarmOff1
	mov a, Minute_counter
	cjne a, AlarmMin2, AlarmOff1
	mov a, BCD_counter
	cjne a, AlarmSec2, AlarmOff1
;===============================TURNING ALARM ON===========================================================
AlarmOn:
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	
AlarmOff1:	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	;============================================KEEPING ALARM UNTOUCHED===============================================================
	clr a
	mov R0, #0x01
	mov Count1ms+0, a
	mov Count1ms+1, a
	
	
	; Increment the BCD counter
	mov a, Alarm_Reset_Flag
	
	cjne a, #0x0, AlarmSetup
	;================================MANUAL ADJUSTMENT BUTTONS===========================================================
	jnb SecondsB, Timer2_ISR_S_madjust
	jnb MinutesB, Timer2_ISR_M_madjust
	jnb HoursB, Timer2_ISR_H_madjust
	jnb DayB, Timer2_ISR_D_madjust
	jnb setalarm, AlarmFlagAct
	;===========================================================================================
AlarmSetup:

	mov a, BCD_counter
	add a, #0x001
	jnb setalarm, AlarmFlagAct
	ljmp AlarmTimeSetup

Timer2_ISR_done:;======================================Exit from ISR=====================================================
	pop psw
	pop acc
	reti
Timer2_ISR_S_madjust:;================================Manual Second Adjustment===========================================================
	mov a, BCD_counter
	add a, #0x10 ; Adding the 10-complement of -1 is like subtracting 1.
;	add BCD_counter, #0x99
;Timer2_ISR_inc:
Timer2_ISR_da:;=======================================Decimal Adj==================================================
	da a ; Decimal adjust instruction.  Check datasheet for more details!	
	mov BCD_counter, a

	cjne a, #0x60, Timer2_ISR_done ;if seconds are not 60, go to ISR_done
	mov BCD_counter, #0x0 ;reset seconds to 0
	mov a, Minute_counter ; set a to previous minutes
	add a, #0x01 ;add one to obtain current minutes
	da a ;makes formatting nice 
	mov Minute_counter, a ;put updated minutes into the counter
	sjmp Timer2_ISR_M_aadjust
	
Timer2_ISR_M_madjust:;=========================Manual Minute Adjustment===================================================
	mov a, Minute_counter
	add a, #0x01
	da a
	mov Minute_counter, a
	
Timer2_ISR_M_aadjust:;===================================Automatic Minute Adjustment========================================================
	
	cjne a, #0x60, Timer2_ISR_done ;if minutes are not 60, go to ISR_done
	mov Minute_counter, #0x0 ;reset minutes to 0
	mov a, Hour_counter ;Put previous hours into a
	add a, #0x01;add one to obtain current hours
	da a;makes hex into decimal
	mov Hour_counter, a ;updates hour counter with new hours
	sjmp Timer2_ISR_H_aadjust
	
Timer2_ISR_H_madjust:;===============================Manual Hour Adjustment============================================================
	mov a, Hour_counter
	add a, #0x01
	da a
	mov Hour_counter, a
	
Timer2_ISR_H_aadjust:;===================================Auto Hour Adjustment========================================================
	
	cjne a, #0x12, Timer2_ISR_Hour_Rotation ;if hours are not 12 Go to ISR_Rotation to check if its 13
	mov a, AM_PM ;store AMOMPM value in a
	cjne a, #0x1, Timer2_ISR_PM ;if AM go to ISR_PM else updates PM into AM
	mov AM_PM, #0x0 ;1 stands for PM, 0 - AM,
	mov a, Day_counter
	add a, #0x1
	mov Day_counter, a
	sjmp Timer2_ISR_Hour_Rotation ; jump to ISR_Hour_Rotation
	
Timer2_ISR_PM: ;usually skipped but if swiching PM was detected oreviously, updates it into AM;==========PM/AM rotation=================================================================================
	mov AM_PM, #0x1

Timer2_ISR_Hour_Rotation:;checks for hour transition instead of 13 into 1;==========================Hour Flip=================================================================
	
	cjne a, #0x13, Timer2_ISR_done ;Reset hour once 1o'clock is reached
	mov Hour_counter, #0x01
	ljmp Timer2_ISR_done
	
		
Timer2_ISR_D_madjust:;==========================Manual Day Adjust================================================================
	mov Temp_Reg, a
	mov a, Day_counter
	add a, #0x1
	cjne a, #0x8, DayKeep
	mov a, #0x1
	mov Day_counter, a
	mov a, Temp_Reg
	ljmp Timer2_ISR_done

DayKeep:	;==========================NO ROLLOVER================================================================
	mov Day_counter, a
	ljmp Timer2_ISR_done


Alarm2Sec: 
	mov a, AlarmSec2
	cjne a, #0x59, skipRO2
	mov AlarmSec2, #0x0
	mov a, BCD_counter
	add a, #0x001	
	ljmp Timer2_ISR_da
skipRO2: ;==========================AS seconds no rollover================================================================
	mov a, AlarmSec2
	add a, #0x1
	da a
	mov AlarmSec2, a
	mov a, BCD_counter
	add a, #0x001
	ljmp Timer2_ISR_da
Alarm1Seconds:;==========================AS seconds================================================================
	mov a, Alarm_Reset_Flag
	cjne a, #0x1, Alarm2Sec
	mov a, AlarmSec
	cjne a, #0x59, skipRO
	mov AlarmSec, #0x0
	mov a, BCD_counter
	add a, #0x001	
	ljmp Timer2_ISR_da
skipRO: ;==========================AS seconds no rollover================================================================
	mov a, AlarmSec
	add a, #0x1
	da a
	mov AlarmSec, a
	mov a, BCD_counter
	add a, #0x001
	ljmp Timer2_ISR_da
Alarm1Minutes:;==========================AS MINUTES================================================================
	mov a, Alarm_Reset_Flag
	cjne a, #0x1, Alarm2Minutes
	mov a, AlarmMin
	cjne a, #0x59, skip2RO
	mov AlarmMin, #0x0
	mov a, BCD_counter
	add a, #0x001	
	ljmp Timer2_ISR_da
skip2RO: ;==========================AS Min NRO================================================================
	mov a, AlarmMin
	add a, #0x1
	da a
	mov AlarmMin, a
	mov a, BCD_counter
	add a, #0x001
	ljmp Timer2_ISR_da

Alarm2Minutes:;==========================AS MINUTES================================================================
	mov a, AlarmMin2
	cjne a, #0x59, skip3RO
	mov AlarmMin2, #0x0
	mov a, BCD_counter
	add a, #0x001	
	ljmp Timer2_ISR_da
skip3RO: ;==========================AS Min NRO================================================================
	mov a, AlarmMin2
	add a, #0x1
	da a
	mov AlarmMin2, a
	mov a, BCD_counter
	add a, #0x001
	ljmp Timer2_ISR_da
AlarmTimeSetup:;==========================Alarm Serup Core================================================================
	jnb SecondsB, Alarm1Seconds
	jnb MinutesB, Alarm1Minutes
	jnb HoursB, Alarm1Hours
	jnb DayB, Alarm1Days
	ljmp Timer2_ISR_da
Alarm1Hours:
	mov a, Alarm_Reset_Flag
	cjne a, #0x1, Alarm2Hours
	mov a, AlarmHour
	cjne a, #0x12, skipRO3
	mov AlarmHour, #0x1
	mov a, AlarmAMPM
	cjne a, #0x0, IMSORRY
	mov AlarmAMPM, #0x1
	mov a, BCD_counter
	add a, #0x001	
	ljmp Timer2_ISR_da
IMSORRY:
	mov AlarmAMPM, #0x0
	mov a, BCD_counter
	add a, #0x001	
	ljmp Timer2_ISR_da
skipRO3: ;==========================AS HOUR NRO================================================================
	mov a, AlarmHour
	add a, #0x1
	da a
	mov AlarmHour, a
	mov a, BCD_counter
	add a, #0x001
	ljmp Timer2_ISR_da
Alarm2Hours:
	mov a, AlarmHour2
	cjne a, #0x12, skip2RO3
	mov AlarmHour2, #0x1
	mov a, AlarmAMPM2
	cjne a, #0x0, IMSORRY2
	mov AlarmAMPM2, #0x1
	mov a, BCD_counter
	add a, #0x001	
	ljmp Timer2_ISR_da
IMSORRY2:
	mov AlarmAMPM2, #0x0
	mov a, BCD_counter
	add a, #0x001	
	ljmp Timer2_ISR_da
skip2RO3: ;==========================AS HOUR NRO================================================================
	mov a, AlarmHour2
	add a, #0x1
	da a
	mov AlarmHour2, a
	mov a, BCD_counter
	add a, #0x001
	ljmp Timer2_ISR_da
Alarm1Days:
	mov a, Alarm_Reset_Flag
	cjne a, #0x1, Alarm2Days
	mov a, AlarmDay
	cjne a, #0x5, skipRO4
	mov AlarmDay, #0x1
	mov a, BCD_counter
	add a, #0x001	
	ljmp Timer2_ISR_da
skipRO4: ;==========================AS DAY NRO================================================================
	mov a, AlarmDay
	add a, #0x1
	da a
	mov AlarmDay, a
	mov a, BCD_counter
	add a, #0x001
	ljmp Timer2_ISR_da
Alarm2Days:
	mov a, AlarmDay2
	cjne a, #0x7, skip3RO4
	mov AlarmDay2, #0x6
	mov a, BCD_counter
	add a, #0x001	
	ljmp Timer2_ISR_da
skip3RO4: ;==========================AS DAY NRO================================================================
	mov a, AlarmDay2
	add a, #0x1
	da a
	mov AlarmDay2, a
	mov a, BCD_counter
	add a, #0x001
	ljmp Timer2_ISR_da
;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:;=========================================================================================================================
	; Initialization
    mov SP, #0x7F
    lcall Timer0_Init
    lcall Timer2_Init
    ; In case you decide to use the pins of P0, configure the port in bidirectional mode:
    mov P0M0, #0
    mov P0M1, #0
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
;	Set_Cursor(1, 1)
   ; Send_Constant_String(#Initial_Message)
    setb half_seconds_flag
	mov BCD_counter, #0x00
	mov Minute_counter, #0x59
	mov Hour_counter, #0x11
	mov AM_PM, #0x1
	mov Day_counter, #0x3
	mov Alarm_Reset_Flag, #0x0
	mov AlarmSec, #0x0
	mov AlarmMin, #0x0
	mov AlarmHour, #0x0
	mov AlarmAMPM, #0x0
	mov AlarmDay, #0x1
	mov AlarmSec2, #0x0
	mov AlarmMin2, #0x0
	mov AlarmHour2, #0x0
	mov AlarmAMPM2, #0x0
	mov AlarmDay2, #0x6
	cpl TR0
	
	; After initialization the program stays in this 'forever' loop
loop:
	jb BOOT_BUTTON, loop_a  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, loop_a  ; if the 'BOOT' button is not pressed skip
	jnb BOOT_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'BOOT' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
	clr TR2                 ; Stop timer 2
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Now clear the BCD counter
	mov BCD_counter, a
	mov Minute_counter, a
	mov Hour_counter, a
	mov AM_PM, a
	mov Day_counter, a
	mov AlarmSec, a
	setb TR2                ; Start timer 2
	
	sjmp loop_b             ; Display the new value
loop_a:
	jnb half_seconds_flag, loop
loop_b:
    clr half_seconds_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
    mov a, Alarm_Reset_Flag
    Set_Cursor(1,1)
  ;  Send_Constant_String(#Initial_Message3)
    Send_Constant_String(#Initial_Message)
    ;Send_Constant_String(#Initial_Message2)
    cjne a, #0x0, AlarmInResetPhase
    Set_Cursor(2, 1)
    Display_BCD(Hour_counter)
	Set_Cursor(2, 4)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(Minute_counter) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(2, 7)
	Display_BCD(BCD_counter)
	Set_Cursor(2, 10)
	Display_char(AM_PM)
	Set_Cursor(2, 13)
	Display_BCD(Day_counter)
	ljmp loop
AlarmInResetPhase:
	Set_Cursor(1, 1)
	Send_Constant_String(#Initial_Message4)
	Set_Cursor(2, 1)
	Send_Constant_String(#Initial_Message3)
	cjne a, #0x2, Clean
	;Set_Cursor(1, 1)
	;Send_Constant_String(#Initial_Message3)
	Set_Cursor(2, 1)
	Display_BCD(AlarmHour2)
	Set_Cursor(2, 4)
	Display_BCD(AlarmMin2)
	Set_Cursor(2, 7)
	Display_BCD(AlarmSec2)
	Set_Cursor(2, 10)
	Display_BCD(AlarmAMPM2)
	Set_Cursor(2, 13)
	Display_char(AlarmDay2)
	ljmp loop
Clean:
	Set_Cursor(2, 1)
	Display_BCD(AlarmHour)
	Set_Cursor(2, 4)
	Display_BCD(AlarmMin)
	Set_Cursor(2, 7)
	Display_BCD(AlarmSec)
	Set_Cursor(2, 10)
	Display_BCD(AlarmAMPM)
	Set_Cursor(2, 13)
	Display_char(AlarmDay)
    ljmp loop

END
