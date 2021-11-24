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

; button state
.equ    BUTTON_NONE,    0
.equ    BUTTON_LEFT,    1
.equ    BUTTON_UP,      2
.equ    BUTTON_DOWN,    3
.equ    BUTTON_RIGHT,   4
.equ    BUTTON_CHECKPOINT,    5

; array state
.equ    DIR_LEFT,       1       ; leftward direction
.equ    DIR_UP,         2       ; upward direction
.equ    DIR_DOWN,       3       ; downward direction
.equ    DIR_RIGHT,      4       ; rightward direction
.equ    FOOD,           5       ; food

; constants
.equ    NB_ROWS,        8       ; number of rows
.equ    NB_COLS,        12      ; number of columns
.equ    NB_CELLS,       96      ; number of cells in GSA
.equ    RET_ATE_FOOD,   1       ; return value for hit_test when food was eaten
.equ    RET_COLLISION,  2       ; return value for hit_test when a collision was detected
.equ    ARG_HUNGRY,     0       ; a0 argument for move_snake when food wasn't eaten
.equ    ARG_FED,        1       ; a0 argument for move_snake when food was eaten
.equ	COUNTER,		32767	; value of the wait decrement counter


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

	addi sp, sp, -12
	stw s0, 8(sp)					; STACKS registers s0/ previous values
	stw s1, 4(sp)
	stw s2, 0(sp)

	;--CODE HERE: INIT CP VALID TO 0
	addi t0, zero, 0
	stw t0, CP_VALID(zero)
  init:
	call init_game
  mainloop:
	call get_input
	addi s0, v0, 0 				; stores the returned input value in s0
	addi t0, zero, BUTTON_CHECKPOINT
	beq t0,s0, state_Checkpoint ; branches to the state checkpoint if button checkpoint pressed otherwise continues

	state_hitTest:
	call hit_test
	addi s2, v0, 0 				; stores the return value of the hit test (register s2)
	addi t0, zero, RET_ATE_FOOD
	beq t0,s2, state_increment_score

	addi t0,zero ,RET_COLLISION
	beq s2,t0, init
	addi a0, zero, ARG_HUNGRY		;sets arguments for move_snake when food not eaten
	call move_snake
	jmpi state_draw


	state_increment_score:
	ldw t1, SCORE(zero)
	addi t1, t1, 1				; increments the score by 1
	stw t1, SCORE(zero)
	call display_score
	addi a0, zero, ARG_FED		;sets arguments for move_snake when food eaten
	call move_snake
	call create_food
	call save_checkpoint
	addi t0, zero, 1 			; stores value for a saved checkpoint
	beq v0,t0, state_blinkScore
	jmpi state_draw


	state_Checkpoint:
	call restore_checkpoint
	addi s1, v0, 0
	addi t0, zero, 0
	beq s1, t0, mainloop

	state_blinkScore:
	call blink_score

	state_draw:
	call clear_leds
	call draw_array
    jmpi mainloop


	ldw s2, 0(sp)
	addi sp, sp, 4
	ldw s1, 0(sp)
	addi sp, sp, 4
	ldw s0, 0(sp)
	addi sp, sp, 4

    ret


; BEGIN: clear_leds
clear_leds:
	stw zero, LEDS(zero)
	stw zero, (LEDS + 4)(zero)
	stw zero, (LEDS + 8)(zero)

	ret

; END: clear_leds

;----------------------------------GET_INPUT--------------------------------------

; BEGIN: set_pixel
set_pixel:
	;----compute the LEDS WORD adress-----------
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

;----------------------------------DISPLAY--------------------------------------

; BEGIN: display_score
display_score:
  ldw t0, CP_SCORE(zero)              ; takes the value of the score
  add t1, zero, zero                  ; prepares the quotient q (now =0)
  bne t0, zero, moduloop              ; proceeds to compute the values of the 7-seg display if the value of the score is not the all zero value
  jmpi DISP_sel

moduloop:
  addi t2, t0, -10                    ; compute the number of time we have 10 in t0 by substracting 10
  blt  t2, zero, DISP_sel             ; if value is smaller than zero, we already have our result : t1=q (first iter => 0), t0=the right value
  addi t1, t1, 1                      ; increments quotient value
  add  t0, t2, zero                   ; computes new rest value
  jmpi moduloop

DISP_sel :
  slli t0, t0, 4                      ; allows us to find the word-aligned address in our table
  slli t1, t1, 4                      ; allows us to find the word-aligned address in our table
  ldw  t3,  digit_map(t1)             ; finds the right combination to the 3rd 7-seg
  ldw  t4,  digit_map(t0)             ; finds the right combination to the 4th 7-seg
  ldw  t5,  digit_map(zero)           ; finds the default value for zero

  stw  t5, SEVEN_SEGS(zero)           ; 7-seg 0 equals zero
  stw  t5, (SEVEN_SEGS + 4)(zero)     ; 7-seg 1 equals zero
  stw  t3, (SEVEN_SEGS + 8)(zero)     ; 7-seg 2 equals the quotient value
  stw  t4, (SEVEN_SEGS + 12)(zero)     ; 7-seg 3 equals the rest value

  ret

; END: display_score

;----------------------------------INIT_GAME--------------------------------------

; BEGIN: init_game
init_game:
	;-----clear GSA----
	addi sp, sp, -8
	stw s0, 4(sp)					; STACKS registers s0/s1 previous values
	stw s1, 0(sp)

	addi s0, zero, -1				; iterator x init

	loopDrawXClear:
	addi s0, s0, 1					; increments x by 1
	addi s1, zero, -1				; iterator y init again
	addi t3, zero, NB_COLS			; stop value of loop x

	beq s0, t3, endClearGSA			; stop loop condition

	loopDrawYClear:
	addi s1, s1, 1					; increment y by 1
	addi t5, zero, NB_ROWS			; stop value of loop y

	beq s1, t5, loopDrawXClear		; end of first range loop of y, need to increment x
	;-----computes the linear adress ---------
	slli t6, s0, 3					; multiplies x by 8
	add t6, t6, s1					; adds y => linear adress of head on GSA
	slli t6, t6, 2					; adresses should be WORD_ALIGNED (*4)
	;-----sets the adress to 0
	addi t0, zero, 0 				; stores value 0
	stw t0, GSA(t6)					; stores value 0 in GSA

	jmpi loopDrawYClear				; loop

	endClearGSA:
	ldw s1, 0(sp)
	addi sp, sp, 4
	ldw s0, 0(sp)
	addi sp, sp, 4

	;-----Sets the snake to the top left corner-----
	stw zero, HEAD_X(zero)		 ; position x of head
	stw zero, HEAD_Y(zero) 		 ; position y of head
    stw zero, TAIL_X(zero)		 ; position x of head
	stw zero, TAIL_Y(zero) 		 ; position y of head

	;----sets movement to the right------------------
	addi t0, zero, BUTTON_RIGHT
	stw t0, GSA(zero)            ; direction right in GSA

	;---set the score to zero
	addi t0, zero, 0
	stw t0, SCORE(zero)
	;--display food at random location
	jmpi create_food

; END: init_game

;----------------------------------CREATE_FOOD--------------------------------------

; BEGIN: create_food
create_food:
	addi t0, zero, 96				; off limite value for the GSA array
	addi t1, zero, 255				; create mask
loopFood:
	ldw t2, RANDOM_NUM(zero)		; random adress received
	and t2, t2, t1					; takes the lowest byte
	bge t2, t0, loopFood			; checks if the random num is in range
	slli t2, t2, 2					; makes the adress word aligned
	ldw t4, GSA(t2)					; gets the value of the GSA cell in T2
	addi t3, zero, 0 				; value 0 stored in t3
	bne t4, t3, loopFood			; checks if the cell is empty to generate food => if not loop again
	addi t3, zero, 5				; stores value 5
	stw t3, GSA(t2)					; stores value 5 in the GSA AT LOCATION T2 = random * 4
	ret
; END: create_food

;----------------------------------HIT_TEST--------------------------------------

; BEGIN: hit_test
hit_test:

    ldw t0, HEAD_X(zero)		 ; position x of head
	ldw t1, HEAD_Y(zero) 		 ; position y of head
   	slli t2, t0, 3 			   	 ; multiplies x by 8
	add t2, t2, t1 				 ; adds y => t2 is the linear adress of head on GSA
    slli t2, t2, 2               ; gets the right address
    ldw t3, GSA(t2) 			 ; gets the orientation vector of the head (same as the last head)

    beq t0, zero, checkLeft      ; checks if both x=0 and GSA=1 = Left
    addi t4, zero, 11
    beq t0, t4, checkRight       ; checks if both x=11 and GSA =4 = Right
    beq t1, zero, checkUp        ; checks if both y=0 and GSA =2 = Up
    addi t4, zero, 7
    beq t1, t4, checkDown        ; checks if both y=7 and GSA =3  = Down

    jmpi GSAValue


checkLeft:
    addi t4, zero, 1            
    beq t3, t4, abort           ; checks if GSA = 1
    addi t4, zero, 7
    beq t1, zero, checkUp       ; checks if y=0 as well, as x=0 has precedence
    beq t1, t4, checkDown       ; checks if y=7 as well, as x=0 has precedence
    jmpi GSAValue

checkUp:
    addi t4, zero, 2           ; checks if GSA = 2
    beq t3, t4, abort
    jmpi GSAValue

checkDown:
    addi t4, zero, 3           ; checks if GSA = 3
    beq t3, t4, abort
    jmpi GSAValue

checkRight:
    addi t4, zero, 4           ; checks if GSA = 4
    beq t3, t4, abort
    addi t4, zero, 7
    beq t1, zero, checkUp       ; checks if y=0 as well, as x=11 has precedence
    beq t1, t4, checkDown       ; checks if y=7 as well, as x=11 has precedence
    jmpi GSAValue

GSAValue:
    addi t4, zero, 1
    beq t3, t4, GSA_left
    addi t4, t4, 1
	beq t3, t4, GSA_up
    addi t4, t4, 1
	beq t3, t4, GSA_down
	jmpi GSA_right

GSA_left:
  	addi t0, t0, -1
    jmpi ComputeGSA

GSA_up:
	addi t1, t1, -1
    jmpi ComputeGSA

GSA_down:
	addi t1, t1, 1
    jmpi ComputeGSA

GSA_right:
	addi t0, t0, 1
    jmpi ComputeGSA

ComputeGSA:
    slli t2, t0, 3 			   	 ; multiplies x by 8
	add t2, t2, t1 				 ; adds y => t2 is the linear adress of head on GSA
    slli t2, t2, 2               ; gets the right address
    ldw t3, GSA(t2) 			 ; gets the future position of the head 

    beq t3, zero, noCollision   ; if the GSA value is zero, there is no collision
    addi t4, zero, FOOD         ; cste food = 5
    beq t3, t4, scoreIncrement  ; if the GSA value is one there is a food => score increment
    jmpi abort                  ; else there is a bit of the snake => abort

abort:
    addi v0, zero, 2
    ret

noCollision:
    add v0, zero, zero
    ret

scoreIncrement:
    addi v0, zero, 1
    ret

; END: hit_test

;----------------------------------GET_INPUT--------------------------------------

; BEGIN: get_input
get_input:
    ldw t0, (BUTTONS + 4)(zero)  ; gets the edgecapture
    andi t6, t0, 31              ; gets the 5 LSBs

    ldw t1, BUTTONS(zero)        ; gets the status
    andi t7, t1, 31              ; gets the 5 LSBs

    bne  t6, zero, checkpoint    ; branches to checkpoint if buttons were pushed
    add  v0, zero, zero          ; returns 0
    jmpi  endGET                 ; jumps to endGET


checkpoint:
    srli t2, t6, 4               ; gets the checkpoint bit
    add t0, zero, zero           ; initializes our current value to 0 (useful if direction is called)
    add t3, zero, zero           ; initializes our current value to 0 (useful if state is called)

    beq  t2, zero, directions    ; branches to direction if checkpoint button hasn't been pressed
    addi  v0, zero, 5            ; returns 5
    jmpi  endGET                 ; jumps to endGET


directions:

    srl t1, t6, t0               ;
    andi t1, t1, 1               ; gives the t0'th bit of the edgecapture, with step above
    addi t0, t0, 1               ; increment current by one
    beq t1, zero, directions     ; if the bit is not 1, it is not the right one, we loop
    add v0, zero, t0             ; return value
    jmpi  state


state:
    bne  t7, zero, computeState    ; branches to computeState if buttons were pushed previously
    jmpi changeState


computeState:

    srl t1, t7, t3               ;
    andi t1, t1, 1               ; gives the t3'th bit of the status, with step above
    addi t3, t3, 1               ; increment current by one
    bne t1, zero, computeState          ; if the bit is not 0, it is not the right one, we loop again
    add t4, v0, t3               ; add our pushed button with our status, if the addition is equal to 5 we do not modify the state of our snake
    addi t5, zero, 5
    bne  t5, t4, changeState
    jmpi endGET


changeState:
    ; do not need the ancient t6 and t7 values
    ldw t5, HEAD_X(zero)		 ; position x of head
	ldw t6, HEAD_Y(zero) 		 ; position y of head
   	slli t7, t5, 3 			   	 ; multiplies x by 8
	add t7, t7, t6 				 ; adds y => linear adress of head on GSA in t0
    slli t7, t7, 2               ; fins the corresponding address
    stw v0, GSA(t7) 			 ;sets the orientation vector of the head (same as the last head
    jmpi endGET

endGET:
    stw zero, (BUTTONS + 4)(zero); sets edgecapture at 0
    ret
; END: get_input

;----------------------------------DRAW_ARRAY--------------------------------------

; BEGIN: draw_array
draw_array:
	addi sp, sp, -12
	stw ra, 8(sp)				; stacks the main return address
	stw s0, 4(sp)				; STACKS registers s0/s1 previous values
	stw s1, 0(sp)

	addi s0, zero, -1			; iterator x init

	loopDrawX:
	addi s0, s0, 1				; increments x by 1
	addi s1, zero, -1			; iterator y init again
	addi t3, zero, NB_COLS			; stop value of loop x

	beq s0, t3, endDraw			; stop loop condition
	loopDrawY:

	addi s1, s1, 1				; increment y by 1
	addi t5, zero, NB_ROWS		; stop value of loop y

	beq s1, t5, loopDrawX		; end of first range loop of y, need to increment x
	;-----computes the linear adress ---------
	slli t6, s0, 3				; multiplies x by 8
	add t6, t6, s1				; adds y => linear adress of head on GSA
	slli t6, t6, 2				; adresses should be WORD_ALIGNED (*4)

	ldw t2, GSA(t6)				; value of the current GSA cell

	addi t0, zero, 0 			; stores value 0
	beq t0, t2, loopDrawY		; if t2 is zero, loop

	; ------sets a0 to x and a1 to y----
	add a0, s0, zero
	add a1, s1, zero
	call set_pixel
	jmpi loopDrawY

	endDraw:
	ldw s1, 0(sp)
	addi sp, sp, 4
	ldw s0, 0(sp)
	addi sp, sp, 4
	ldw ra, 0(sp)				;pop the initial return value
	addi sp, sp, 4
	ret
; END: draw_array

;----------------------------------MOVE_SNAKE--------------------------------------

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
	add t0, t0, t6				 ; adds y => linear adress of head on GSA
	slli t0, t0, 2				 ; adresses should be WORD_ALIGNED (*4)

	ldw t0, GSA(t0)				 ; VALUE of the SNAKE HEAD IN THE GSA => Orientation vector head

;----flow---
	beq t0, t1, left
	beq t0, t2, up
	beq t0, t3, down
	beq t0, t4, right

;---updates the HEAD X AND HEAD Y COORDINATES ;
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
	add t7, t7, t6				; adds y => linear adress of head on GSA
	slli t7, t7, 2				; adresse are word aligned

	stw t0, GSA(t7) 			; sets the orientation vector of the head (same as the last head)

	;---------Checks if the food was eaten--------
	addi t5, zero, ARG_FED		; stores in value t5 the value of ARG_FED argument
	beq a0, t5, endMove			; will skip the changing coordinates of the tail if food was eaten

	;--------value of snake's tail ---------------
	ldw t5, TAIL_X(zero) 		; position x of tail
	ldw t6, TAIL_Y(zero) 		; position y of tail

	slli t7, t5, 3 				; multiplies x by 8
	add t0, t7, t6 				; adds y => linear adress of tail on GSA in t0
	slli t0, t0, 2				; adresse are word aligned

	ldw t7, GSA(t0) 			; VALUE of the SNAKE TAIL IN THE GSA => Orientation vector tail

	;------compute new tail coord----

	beq t7, t1, lefty
	beq t7, t2, upy
	beq t7, t3, downy
	beq t7, t4, righty
	jmpi endTail
;---updates the TAIL X AND TAIL Y COORDINATES ;

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
endMove:
	ret
; END: move_snake

;----------------------------------SAVE_CHECKPOINT--------------------------------------

; BEGIN: save_checkpoint
save_checkpoint:
	addi v0, zero, 0
moduloopSave:
	addi t2, t0, -10				  ; compute the number of time we have 10 in t0 by substracting 10
	blt  t2, zero, endModuloop		  ; if value is smaller than zero, result : t0=the result of the modulo 10
	addi t1, t1, 1                    ; increments quotient value
	add  t0, t2, zero                 ; computes new rest value
	jmpi moduloopSave
endModuloop:

	bne t0, zero, endSave			  ; if the score is not a multiple of 10, finsishes the method	
	;-----CASE CP CALL IS VALID-----
	addi v0, zero, 1				; returns adress 0
	stw v0, CP_VALID(zero)			; sets the CP_VALID to 1

	;----stores the gamestate----
	addi sp, sp, -4 			; pushes the return adress
	stw ra, 0(sp)
	
	addi a1, zero, CP_HEAD_X 	; init value of iteration over dest data adress
	addi a0, zero, HEAD_X		; init value of iteration over source data adress

	addi t5, zero, GSA			; computing of limit value for t7
	addi t5, t5, 384			; computing of limit value for t7=> GSAaddr + 96*4

loopsave:
	call copy_data
	addi a1, a1, 4
	addi a0, a0, 4
	bne a0, t5, loopsave
	;----------------endloop and END CASE restore--------

	ldw ra, 0(sp)				;pop the initial return value
	addi sp, sp, 4
endSave:
	ret

; END: save_checkpoint

;----------------------------------RESTORE_CHECKPOINT--------------------------------------

; BEGIN: restore_checkpoint
restore_checkpoint:
	ldw t0, CP_VALID(zero)		; loads the value of CP_VALID in t0
	addi t1, zero, 1 			; value of a valid checkpoint
	addi v0, zero, 0
	bne t0, t1, endRestore		; if the checkpoint is not valid, finishes the method

	;-----CASE restores the gamestate checkpoint-----
	addi sp, sp, -4 			; pushes the return adress
	stw ra, 0(sp)

	addi v0, zero, 1			; returns 1 because the checkpoint is valid
	addi a0, zero, CP_HEAD_X 	; init value of iteration over source data adress
	addi a1, zero, HEAD_X		; init value of iteration over dest data adress

	addi t5, zero, GSA			; computing of limit value for t7
	addi t5, t5, 384			; computing of limit value for t7=> GSAaddr + 96*4
loopstore:
	call copy_data
	addi a0, a0, 4
	addi a1, a1, 4
	bne a1, t5, loopstore
	;----------------endloop and END CASE restore--------
	ldw ra, 0(sp)				;pop the initial return value
	addi sp, sp, 4
endRestore:
	ret

; END: restore_checkpoint

;------------------------------------------COPY_DATA------------------------------------
;---procedure to copy data from adress to another
; 	arg a0: adress to load data to copy
;	arg a1: adress to store copied data

copy_data:
	ldw t0, 0(a0)
	stw t0, 0(a1)
	ret

;----------------------------------BLINK_SCORE--------------------------------------

; BEGIN: blink_score
blink_score:
	addi t3, zero, 3                     ; number of times it will blink = 3
	jmpi blink_loop

blink_loop:
	addi sp, sp, -4 
	stw ra, 0(sp) 				; stacks the main return address

	addi t3, t3, -1
	stw  zero, SEVEN_SEGS(zero)           ; 7-seg 0 equals zero
	stw  zero, (SEVEN_SEGS + 4)(zero)     ; 7-seg 1 equals zero
	stw  zero, (SEVEN_SEGS + 8)(zero)     ; 7-seg 2 equals the quotient value
	stw  zero, (SEVEN_SEGS + 12)(zero)    ; 7-seg 3 equals the rest value
	call wait							  ; wait procedure
	call display_score                    ; lightens the 7 seg again
	beq t3, zero, BLINK_end               ; ends the blink procedure if it has done it 3  times
	jmpi blink_loop                       ; else loops again

BLINK_end:
	ldw ra, 0(sp)
	addi sp, sp, 4
	ret

; END: blink_score

wait:
	addi t0, zero, COUNTER
loopWait1:
	addi t0, t0, -1
	addi t1, zero, COUNTER
loopWait2:
	addi t1, t1, -1
	bge t1, zero, loopWait2
	bge t0, zero, loopWait1
	ret


digit_map:
  .word 0xFC ; 0
  .word 0x60 ; 1
  .word 0xDA ; 2
  .word 0xF2 ; 3
  .word 0x66 ; 4
  .word 0xB6 ; 5
  .word 0xBE ; 6
  .word 0xE0 ; 7
  .word 0xFE ; 8
  .word 0xF6 ; 9
