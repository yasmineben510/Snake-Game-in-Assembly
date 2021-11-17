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
    add a0, zero, zero           ; sets food to zero
    addi t0, zero, 3

    stw zero, HEAD_X(zero)		 ; position x of head
	stw zero, HEAD_Y(zero) 		 ; position y of head
    stw zero, TAIL_X(zero)		 ; position x of head
	stw zero, TAIL_Y(zero) 		 ; position y of head
    stw t3, GSA(zero)            ; direction right in GSA
  
	call clear_leds 		
	call get_input
	call move_snake
	call draw_array
    jmpi main

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
	srli t0, a0, 2    		    ; shifts right by 2 (divides by 4)
	slli t0, t0, 2				; shifts left by 2 : Word adress In lEDS stored in t0

	;----compute the index of the word to set---
	andi t1, a0, 3 				; acts as a modulo 4 {[x]mod4}
	slli t1, t1, 3 				; shifts [x]mod4 left by 3 (multiplies by 8) {8*x}
	add t1, t1, a1				; {8[x]mod4 + y}

	;-----computes the word to store----
	addi t3, zero, 1			; stores the value 1
	sll t2,t3, t1 				; shifts the value 1 by t1 indexes
	;----loads word and make an or------
	ldw t4, LEDS(t0)
	or t2, t4, t2 				; previous value OR new value (bitwise)
	
	;----store the new word----
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
    ldw t0, (BUTTONS + 4)(zero)  ; gets the edgecapture
    andi s0, t0, 31              ; gets the 5 LSBs
     
    ldw t1, BUTTONS(zero)        ; gets the status
    andi s1, t1, 31              ; gets the 5 LSBs
    
    bne  s0, zero, checkpoint    ; branches to checkpoint if buttons were pushed
    addi  v0, zero, 0            ; returns 0
    jmpi  endGET                 ; jumps to endGET 


checkpoint:
    srli t2, s0, 4               ; gets the checkpoint bit
    add t0, zero, zero           ; initializes our current value to 0 (useful if direction is called)
    add t3, zero, zero           ; initializes our current value to 0 (useful if state is called)

    beq  t2, zero, directions    ; branches to direction if checkpoint button hasn't been pressed
    addi  v0, zero, 5            ; returns 5
    jmpi  endGET                 ; jumps to endGET 


directions:
    
    addi t0, t0, 1               ; increment current by one
    srl t1, s0, t0               ; 
    andi t1, t1, 1               ; gives the t0'th bit of the edgecapture, with step above
    beq t1, zero, directions     ; if the bit is not 1, it is not the right one, we loop
    add v0, zero, t0             ; return value
    jmpi  state


state:
    bne  s1, zero, computeState    ; branches to computeState if buttons were pushed previously
    jmpi changeState


computeState:

    addi t3, t3, 1               ; increment current by one
    srl t1, s0, t3               ; 
    andi t1, t1, 1               ; gives the t3'th bit of the edgecapture, with step above
    beq t1, zero, state          ; if the bit is not 1, it is not the right one, we loop again
   
    add t4, v0, t3               ; add our pushed button with our status, if the addition is equal to 5 we do not modify the state of our snake
    addi t5, zero, 5            
    bne  t5, t4, changeState
    jmpi endGET


changeState:
   
    ldw t5, HEAD_X(zero)		 ; position x of head
	ldw t6, HEAD_Y(zero) 		 ; position y of head
   	slli t7, t5, 3 			   	 ; multiplies x by 8
	add t7, t7, t6 				 ; adds y => linear adress of tail on GSA in t0
    stw v0, GSA(t7) 			 ;sets the orientation vector of the head (same as the last head
    jmpi endGET

endGET: 
    stw zero, (BUTTONS + 4)(zero); sets edgecapture at 0
    ret
; END: get_input


; BEGIN: draw_array
draw_array:

	addi t0, zero, 0 			;stores value 0
	addi t1, zero, 0			;iterator init
	addi t3, zero, 388			;stop value of loop: last gsa word + 1 : (96+1)*4 #word-aligned
	add s0, ra, zero			;stacks the main return address in s0

	loopDraw:
	beq t1, t3, endDraw			;stop loop condition

	ldw t2, GSA(t1)				;value of the current GSA cell
	addi t1, t1, 4 				;increments by 4

	beq t0, t2, loopDraw		; if t2 is zero, loop
		 
	; sets a0 and a1 (how to compute these values?)
	call set_pixel 
	jmpi loopDraw

	endDraw: 
	add ra, s0, zero		;pop the initial return value 
	ret
; END: draw_array


; BEGIN: move_snake
move_snake:
;-----------possible orientations values --------
	addi t1, zero, 1			 ; left
	addi t2, zero, 2			 ; up
	addi t3, zero, 3			 ; down
	addi t4, zero, 4			 ; right

;-------value of snake's head -----------
	ldw t5, HEAD_X(zero)		 ; position x of head
	ldw t6, HEAD_Y(zero) 		 ; position y of head

	slli t0, t5, 3				 ; multiplies x by 8
	add t0, t0, t6				 ; adds 8 => linear adress of head on GSA
	slli t0, t0, 2				 ; adresses should be WORD_ALIGNED (*4)

	ldw t0, GSA(t0)				 ; VALUE of the SNAKE HEAD IN THE GSA => Orientation vector head

;----flow---
	beq t0, t1, left
	beq t0, t2, up
	beq t0, t3, down
	beq t0, t4, right

;---updates the HEAD X AND HEAD Y COORDINATES ; NOTE Still needs to compute mod 12 for the new x and y
left:							; updates the x coord to the left 
	ldw t7, HEAD_X(zero) 		; position x of head
	addi t5, t7, -1
	stw t5, HEAD_X(zero)
	jmpi endHead
up:								; updates the y coord to the up 
	ldw t7, HEAD_Y(zero)		; position y of head
	addi t5, t7, -1
	stw t5, HEAD_Y(zero)
	jmpi endHead

down:
	 ; updates the y coord to the down 
	ldw t7, HEAD_Y(zero) 		; position y of head
	addi t5, t7, 1
	stw t5, HEAD_Y(zero)
	jmpi endHead

right:  ; updates the x coord to the right 
	ldw t7, HEAD_X(zero) 		; position x of head
	addi t5, t7, 1
	stw t5, HEAD_X(zero)
	jmpi endHead

endHead:
	ldw t5, HEAD_X(zero) 		; position x of head
	ldw t6, HEAD_Y(zero)		; position y of head

	slli t7, t5, 3 				; multiplies x by 8
	add t7, t7, t6				; adds 8 => linear adress of head on GSA
	slli t7, t7, 2				; adresse are word aligned

	stw t0, GSA(t0) 			;sets the orientation vector of the head (same as the last head)

	;--------value of snake's tail ---------------
	ldw t5, TAIL_X(zero) 		; position x of tail
	ldw t6, TAIL_Y(zero) 		; position y of tail

	slli t7, t5, 3 				; multiplies x by 8
	add t0, t7, t6 				; adds 8 => linear adress of tail on GSA in t0
	slli t0, t0, 2				; adresse are word aligned

	ldw t7, GSA(t0) 			; VALUE of the SNAKE TAIL IN THE GSA => Orientation vector tail

	;------compute new tail coord----

	beq t7, t1, lefty
	beq t7, t2, upy
	beq t7, t3, downy
	beq t7, t4, righty
	jmpi endTail
;---updates the TAIL X AND TAIL Y COORDINATES ; NOTE Still needs to compute mod 12 for the new x and y

lefty: 							; updates the x coord to the left 
								; position x in t5
	addi t5, t5, -1
	stw t5, TAIL_X(zero)
	jmpi endTail
upy:  							; updates the y coord to the up 
	 							; position y of tail in t6
	addi t6, t6, -1
	stw t6, TAIL_Y(zero)
	jmpi endTail

downy:
	 							; updates the y coord to the down 
	 							; position y of tail in t6
	addi t6, t6, 1
	stw t6, TAIL_Y(zero)
	jmpi endTail

righty:  						; updates the x coord to the right 
								; position x of tail in t5
	addi t5, t5, 1
	stw t5, TAIL_X(zero)
	jmpi endTail

endTail:
;-----Clear last tail position---
	addi t7, zero, 0
	stw t7, GSA(t0)
	ret


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
