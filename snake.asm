;	set game state memory location
.equ    HEAD_X,         0x1000  ; Snake head's position on x
.equ    HEAD_Y,         0x1004  ; Snake head's position on y
.equ    TAIL_X,         0x1008  ; Snake tail's position on x
.equ    TAIL_Y,         0x100C  ; Snake tail's position on Y
.equ    SCORE,          0x1010  ; Score address
.equ    GSA,            0x1014  ; Game state array address

.equ    CP_VALID,       0x1200  ; Whether the checkpoint is valid.
.equ    CP_HEAD_X,      0x1204  ; Snake head's X coordinate. (Checkpoint)
.equ    CP_HEAD_Y,      0x1208  ; Snake head's Y coordinate. (Checkpoint)
.equ    CP_TAIL_X,      0x120C  ; Snake tail's X coordinate. (Checkpoint)
.equ    CP_TAIL_Y,      0x1210  ; Snake tail's Y coordinate. (Checkpoint)
.equ    CP_SCORE,       0x1214  ; Score. (Checkpoint)
.equ    CP_GSA,         0x1218  ; GSA. (Checkpoint)

.equ    LEDS,           0x2000  ; LED address
.equ    SEVEN_SEGS,     0x1198  ; 7-segment display addresses
.equ    RANDOM_NUM,     0x2010  ; Random number generator address
.equ    BUTTONS,        0x2030  ; Buttons addresses

; initialize stack pointer
addi    sp, zero, LEDS

; main
; arguments
;     none
;
; return values
;     This procedure should never return.
main:
    ; TODO: Finish this procedure.
	call clear_leds ; on sait pas oourquoi ca marche ni ce que l'on fait SOS SVP
	addi a0, zero, 3
	addi a1, zero, 0
	call set_pixel
	addi a0, zero, 7
	addi a1, zero, 1
	call set_pixel

	addi a0, zero, 5
	addi a1, zero, 3
	call set_pixel
	
	addi a0, zero, 2
	addi a1, zero, 4
	call set_pixel

	addi a0, zero, 10
	addi a1, zero, 2
	call set_pixel

	addi a0, zero, 11
	addi a1, zero, 3
	call set_pixel
	
	
	
    ret


; BEGIN: clear_leds
clear_leds:
	stw zero, LEDS(zero)
	stw zero, (LEDS + 4)(zero)
	stw zero, (LEDS + 8)(zero) 

	ret

; END: clear_leds
	

; BEGIN: set_pixel
set_pixel:

	;compute the LEDS WORD adress
	srli t0, a0, 2 ; shifts right by 2 (divides by 4)
	slli t0, t0, 2 ; shifts left by 2 : Word adress In lEDS stored in t0

	;compute the index of the word to set
	andi t1, a0, 3 ; acts as a modulo 4 {[x]mod4}
	slli t1, t1, 3 ; shifts [x]mod4 left by 3 (multiplies by 8) {8*x}
	add t1, t1, a1; {8[x]mod4 + y}

	;computes the word to store
	addi t3, zero, 1 ; stores the value 1
	sll t2,t3, t1 ; shifts the value 1 by t1 indexes
	;loads word and make an or
	ldw t4, LEDS(t0)
	or t2, t4, t2 ; previous value OR new value (bitwise)
	
	;store the new word 
	stw t2, LEDS(t0)

	ret

; END: set_pixel


; BEGIN: display_score
display_score:

; END: display_score


; BEGIN: init_game
init_game:

; END: init_game


; BEGIN: create_food
create_food:

; END: create_food


; BEGIN: hit_test
hit_test:

; END: hit_test


; BEGIN: get_input
get_input:

; END: get_input


; BEGIN: draw_array
draw_array:

; END: draw_array


; BEGIN: move_snake
move_snake:

; END: move_snake


; BEGIN: save_checkpoint
save_checkpoint:

; END: save_checkpoint


; BEGIN: restore_checkpoint
restore_checkpoint:

; END: restore_checkpoint


; BEGIN: blink_score
blink_score:

; END: blink_score
