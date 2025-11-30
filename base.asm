IDEAL
MODEL small
STACK 100h
DATASEG
;------------------
; Sound data
;------------------
	cn db 1
	note1 dw 1436h 
	note2 dw 1917h 
	note3 dw 2873h 
	note4 dw 2559h
	stop db ?
;------------------
; Game data
;------------------
	score dw 0
	missed_shot_score_penalty dw ?
	score_msg db 'Score - '
	ez_difficult_msg db 'for easy press 1'
	medium_difficult_msg db 'for medium press 2'
	hard_difficult_msg db 'for hard press 3'
	start_guide_msg db 'Press space to start'
	movement_guide_msg db 'Press a & d to move'
	shooting_guide_msg db 'Press e & q to shoot'
	win_criteria_msg db 'Score over 2000 to win'
	game_lost_msg db 'Game over, You Lose'
	game_won_msg db 'Game over, You Win'
; ------------------
; ship variables
; ------------------
	spaceship_curr_x dw 130
	spaceship_curr_y dw 180

	spaceship_width equ 12
	spaceship_height equ 10
	spaceship_model		db 00,00,00,00,00,00,02,00,00,00,00,00,00
						db 00,00,00,00,00,02,02,02,00,00,00,00,00
						db 00,00,00,00,00,02,00,02,00,00,00,00,00
						db 00,00,00,00,00,02,00,02,00,00,00,00,00
						db 00,00,00,00,00,02,02,02,00,00,00,00,00
						db 00,00,00,02,00,02,02,02,00,02,00,00,00
						db 00,00,02,02,02,02,02,02,02,02,02,00,00
						db 00,00,02,02,02,02,02,02,02,02,02,00,00
						db 00,00,02,02,02,02,02,02,02,02,02,00,00
						db 00,00,02,02,00,02,02,02,00,02,02,00,00
						db 00,00,00,00,00,00,02,00,00,00,00,00,00
;------------------
; Enemy variables
;------------------
    enemy_width equ 12
    enemy_height equ 10
	dead_count dw 0
	enemies_moving_right dw 1
	enemy_tick_x_movement equ 1
	enemy_border_y_movement equ 3
	enemies_move_down dw 0
	max_active_enemies equ 144
	active_enemies 	dw 1,40,20
					dw 1,80,20
					dw 1,120,20
					dw 1,160,20
					dw 1,200,20
					dw 1,240,20
					dw 1,40,50
					dw 1,80,50
					dw 1,120,50
					dw 1,160,50
					dw 1,200,50
					dw 1,240,50
					dw 1,40,80
					dw 1,80,80
					dw 1,120,80
					dw 1,160,80
					dw 1,200,80
					dw 1,240,80
					dw 1,40,110
					dw 1,80,110
					dw 1,120,110
					dw 1,160,110
					dw 1,200,110
					dw 1,240,110

	active_enemy_id equ 1
	Enemy			db 00,00,00,00,00,00,00,00,00,00,00,00,00
					db 00,00,00,00,00,00,00,00,00,00,00,00,00
					db 00,00,00,00,00,00,00,00,00,00,00,00,00
					db 00,00,00,11,00,00,00,00,00,11,00,00,00
					db 00,00,00,00,11,00,00,00,11,00,00,00,00
					db 00,00,00,11,11,11,11,11,11,11,00,00,00
					db 00,00,11,11,00,11,11,11,00,11,11,00,00
					db 00,11,11,11,11,11,11,11,11,11,11,11,00
					db 00,11,00,11,11,11,11,11,11,11,00,11,00
					db 00,11,00,11,00,00,00,00,00,11,00,11,00
					db 00,00,00,00,11,11,00,11,11,00,00,00,00

	inactive_enemy_id equ 0
	dead 			db 00,00,00,00,00,00,00,00,00,00,00,00,00
					db 00,00,00,00,00,00,00,00,00,00,00,00,00
					db 00,00,00,00,00,00,00,00,00,00,00,00,00
					db 00,00,00,00,00,00,00,00,00,00,00,00,00
					db 00,00,00,00,00,00,00,00,00,00,00,00,00
					db 00,00,00,00,00,00,00,00,00,00,00,00,00
					db 00,00,00,00,00,00,00,00,00,00,00,00,00
					db 00,00,00,00,00,00,00,00,00,00,00,00,00
					db 00,00,00,00,00,00,00,00,00,00,00,00,00
					db 00,00,00,00,00,00,00,00,00,00,00,00,00
					db 00,00,00,00,00,00,00,00,00,00,00,00,00
; ------------------
; projectile variables
; ------------------
    projectile_width equ 7
    projectile_height equ 10
	active_projectiles dw 10 dup (0,0,0) 
	max_active_projectiles equ 60
	active_projectiles_next_slot dw offset active_projectiles
	time db 5
	projectile_tick_movement equ 2
	ticks_since_last_projectile_registration dw 0
	minimum_ticks_between_projectiles dw 9

	no_projectile_id equ 0

	delete_projectile_id equ 1
	delete_projectile_model			db 00,00,00,00,00,00,00,00
									db 00,00,00,00,00,00,00,00
									db 00,00,00,00,00,00,00,00
									db 00,00,00,00,00,00,00,00
									db 00,00,00,00,00,00,00,00
									db 00,00,00,00,00,00,00,00
									db 00,00,00,00,00,00,00,00
									db 00,00,00,00,00,00,00,00
									db 00,00,00,00,00,00,00,00
									db 00,00,00,00,00,00,00,00
									db 00,00,00,00,00,00,00,00

	laser_id equ 2
	laser_model		    			db 00,00,00,00,15,00,00,00
									db 00,00,00,15,12,15,00,00
									db 00,00,00,15,12,15,00,00
									db 00,00,00,15,12,15,00,00
									db 00,00,00,15,12,15,00,00
									db 00,00,00,15,12,15,00,00
									db 00,00,00,15,12,15,00,00
									db 00,00,00,00,15,00,00,00
									db 00,00,00,00,00,00,00,00
									db 00,00,00,00,00,00,00,00
									db 00,00,00,00,00,00,00,00

	missile_id equ 3
	missile_model					db 00,00,00,03,03,00,00,00
									db 00,00,00,08,08,00,00,00
									db 00,00,00,03,03,00,00,00
									db 00,00,00,03,03,00,00,00
									db 00,00,00,08,08,00,00,00
									db 00,00,03,03,03,03,00,00
									db 00,03,03,03,03,03,03,00
									db 00,03,03,00,00,03,03,00
									db 00,03,00,00,00,00,03,00
									db 00,00,00,00,00,00,00,00
									db 00,00,00,00,00,00,00,00

CODESEG
proc draw_pixel
;--------------------------------------------------------
; Purpose:    Draw a pixel on the screen at a specified location.
; Inputs:     [BP+4] - Color of the pixel
;             [BP+6] - X position
;             [BP+8] - Y position
; Outputs:    None
;--------------------------------------------------------
    push bp
    mov bp, sp

	; setup configurations from stack
    mov al, [bp + 4]      ; Color of the pixel
    mov cx, [bp + 6]      ; X position
    mov dx, [bp + 8]      ; Y position

	; execute BIOS interrupt
    mov ah, 0ch           ; BIOS interrupt for drawing a pixel
    int 10h               ; Call BIOS interrupt

    pop bp
    ret 6
endp draw_pixel


proc DrawModel
;--------------------------------------------------------
; Purpose:    Draw a rectangular model on the screen.
; Inputs:     
;			  [BP+4]  - Starting Y position (word)
;             [BP+6]  - Starting X position (word)
;             [BP+8]  - Pointer to the model bytes (offset)
;             [BP+10] - Model width (word)
;             [BP+12] - Model height (word)
; Behavior:
;			  Iterates over the model bytes row-by-row. For each
;             model byte, it calls `draw_pixel` (passing the
;             model byte as the color) and advances the drawing
;             position. Stops after drawing `width` pixels per
;             row and `height` rows.
; Notes:
; 			  Callers should push parameters in this order:
;             push <height>
;             push <width>
;             push <offset model>
;             push <x>
;             push <y>
;--------------------------------------------------------
	push bp 				; Save the base pointer
    mov bp, sp             	; Set up the stack frame
	
	mov ax, [bp + 4]	    ; Load starting Y
	mov dx, [bp + 6]		; Load starting x
	mov si, [bp + 8]		; Load pointer to model start

@@pixels_loop:
	push ax               	; save current Y
	push dx                	; save current X

	push ax                	; Y for draw
	push dx                	; X for draw
	push [si]              	; color for draw
	call draw_pixel			; Draw a pixel

	pop dx                 	; restore saved X
	pop ax                 	; restore saved Y

	inc si                 	; advance to next byte in model (color for next pixel)
	inc dx                 	; advance current X position on screen

	; Check if we've drawn `width` pixels on this row:
	mov cx, dx
	sub cx, [bp + 6]       	; cx = currX - startX
	cmp cx, [bp + 10]      	; compare with width
	jbe @@pixels_loop       	; if currX - startX <= width, continue row

	; End of row: reset X and move down one Y
	mov dx, [bp + 6]       	; reset X to startX
	inc ax                 	; advance to next Y row

	; Check if we've drawn `height` rows:
	mov cx, ax
	sub cx, [bp + 4]       	; cx = currY - startY
	cmp cx, [bp + 12]      	; compare with height
	jbe @@pixels_loop       	; if currY - startY <= height, continue column

    pop bp                 	; Restore the base pointer
    ret 10                  ; Clean up the stack and return
endp DrawModel


proc print_string
;--------------------------------------------------------
; Purpose:    Print a string on the screen at a specified location.
; Inputs:     [BP+4] - Length of the string (word).
;             [BP+6] - Screen location (word, row and column packed into a single word).
;             [BP+8] - Pointer to the string in the data segment to print (word).
; Outputs:    None.
;--------------------------------------------------------
    push bp
    mov bp, sp

	; setup configurations from stack
    mov cx, [bp + 4]       ; Length of the string to print
    mov dx, [bp + 6]       ; Screen location (row and column packed into a single word)
    mov bp, [bp + 8]       ; Pointer to the string to print
    mov bl, 00001111b      ; Set BL to the text color (white on black background)
    mov al, 1              ; Set AL to 1 (String is written and the cursor position is updated)

    ; Load data segment for the string
    mov si, @data
    mov es, si

	; execute BIOS interrupt
    mov ah, 13h            ; BIOS interrupt for printing a string
    int 10h                ; Call BIOS interrupt

    pop bp
    ret 6
endp print_string


proc play_note
;--------------------------------------------------------
; Purpose:    Play a musical note at a specified frequency for a short duration.
; Inputs:     
;             [BP+4] - Frequency of the note in Hz (word).
; Behavior:   
;             - Activates the PC speaker.
;             - Configures the Programmable Interval Timer (PIT) to generate the specified frequency.
;             - Waits for a short duration using the system clock.
;             - Deactivates the PC speaker after the note is played.
; Outputs:    None.
;--------------------------------------------------------
    push bp                ; Save the base pointer
    mov bp, sp             ; Set up the stack frame

    ; Open the PC speaker
    in al, 61h             ; Read the current state of the speaker control port
    or al, 3               ; Set bits 0 and 1 to enable the speaker
    out 61h, al            ; Write the updated state back to the speaker control port

    ; Send control word to the PIT to set the frequency
    mov al, 0B6h           ; Control word for PIT channel 2 in square wave mode
    out 43h, al            ; Send the control word to the PIT control port

    ; Send the frequency to the PIT
    mov ax, [bp + 4]       ; Load the frequency from the stack into AX
    out 42h, al            ; Send the lower byte of the frequency to the PIT data port
    mov al, ah             ; Move the upper byte of the frequency into AL
    out 42h, al            ; Send the upper byte of the frequency to the PIT data port
	
    ; Wait for a short duration using the system clock
    mov ah, 2Ch            ; Function 2Ch: Get system time
    int 21h                ; Call DOS interrupt to get the current time
    mov bl, dl             ; Store the current time (hundredths of a second) in BL

retry_time:
    mov ah, 2Ch            ; Function 2Ch: Get system time
    int 21h                ; Call DOS interrupt to get the current time
    cmp bl, dl             ; Compare the stored time with the current time
    je retry_time          ; If the time hasn't changed, keep waiting

    ; Close the PC speaker
    in al, 61h             ; Read the current state of the speaker control port
    and al, 0FCh           ; Clear bits 0 and 1 to disable the speaker
    out 61h, al            ; Write the updated state back to the speaker control port

    pop bp                 ; Restore the base pointer
    ret 2                  ; Clean up the stack and return
endp play_note


proc register_projectile
;--------------------------------------------------------
; Purpose:    Registers a projectile into the active_projectiles array.
; Inputs:     
;             [BP + 4] - The ID of the projectile.
; Behavior:   
;             - Checks if enough time has passed since the last projectile was registered.
;             - Adds the projectile's ID, X position, and Y position to the 
;               `active_projectiles` array.
;             - Updates the `active_projectiles_next_slot` pointer to the next available slot.
; Outputs:    
;             - Updates the `active_projectiles` array with the new projectile's data.
;             - Resets the `ticks_since_last_projectile_registration` counter.
;--------------------------------------------------------
    push bp
    mov bp, sp

    ; Check if enough time has passed since the last projectile
    mov ax, [ticks_since_last_projectile_registration]
    cmp ax, [minimum_ticks_between_projectiles]
    jb @@exit ; Exit if not enough time has passed

    ; Load the next available slot in the active_projectiles array
    mov ax, [active_projectiles_next_slot]
    sub ax, offset active_projectiles
    cmp ax, max_active_projectiles
    jb @@register
    ; Reset to the start of the array if the end is reached
    mov [active_projectiles_next_slot], offset active_projectiles

@@register:
    mov si, [active_projectiles_next_slot]

    ; Store the projectile ID in the array
    mov dx, [bp + 4]           ; Get the projectile ID from the stack
    mov [si], dx               ; Store the projectile ID in the array

    ; Store the X position of the projectile
    mov dx, [spaceship_curr_x] ; Get the spaceship's current X position
    add dx, 2                  ; Adjust the X position for the projectile
    mov [si + 2], dx           ; Store the adjusted X position in the array

    ; Store the Y position of the projectile
    mov dx, [spaceship_curr_y] ; Get the spaceship's current Y position
    sub dx, 12                 ; Adjust the Y position for the projectile
    mov [si + 4], dx           ; Store the adjusted Y position in the array

    ; Update the next available slot pointer
    add [active_projectiles_next_slot], 6

    ; Reset the ticks counter
    mov [ticks_since_last_projectile_registration], 0

@@exit:
    pop bp
    ret 2                      ; Clean up the stack and return
endp register_projectile


proc animate_projectile
;--------------------------------------------------------
; Purpose:    
;             Animates a single projectile by updating its position on the screen.
; Inputs:     
;             [BP + 4] - Pointer to the projectile's data in the `active_projectiles` array.
; Behavior:   
;             - Retrieves the projectile's ID, X, and Y positions from the `active_projectiles` array.
;             - Checks the projectile's ID to determine its type:
;                 - `delete_projectile_id`: Marks the projectile as inactive and animates its deletion.
;                 - `laser_id`: Animates the laser projectile.
;                 - `missile_id`: Animates the missile projectile.
;             - Updates the projectile's Y position to move it upward on the screen by a fixed step.
;             - Calls `DrawModel` to redraw the projectile at its new position.
; Outputs:    
;             - Updates the projectile's position in the `active_projectiles` array.
;             - Redraws the projectile at its new position on the screen.
; Notes:
;             - The movement step is defined by the `projectile_tick_movement` variable.
;             - This procedure assumes that the projectile's data structure is organized as:
;               [ID, X position, Y position].
;             - If the projectile is marked for deletion, it is replaced with the `delete_projectile_model`.
;--------------------------------------------------------
    push bp                ; Save the base pointer
    mov bp, sp             ; Set up the stack frame

    mov si, [bp + 4]       ; Load the pointer to the projectile's data
    mov dx, [si]           ; Load the projectile ID

    cmp dx, [delete_projectile_id] ; Check if the projectile is marked for deletion
    je @@delete

    cmp dx, [laser_id]     ; Check if the projectile is a laser
    je @@laser

    cmp dx, [missile_id]   ; Check if the projectile is a missile
    je @@missile

    jmp @@exit        ; Skip to the end if no match found

@@delete:
    mov ax, [no_projectile_id] ; Mark the projectile as inactive
    mov [si], ax
    mov cx, offset delete_projectile_model ; Load the deletion model
    jmp @@animate             ; Proceed to animate the deletion

@@laser:
    mov cx, offset laser_model ; Load the laser model
    jmp @@animate

@@missile:
    mov cx, offset missile_model ; Load the missile model
    jmp @@animate

@@animate:
    mov dx, projectile_tick_movement ; Load the movement step
    sub [si + 4], dx           ; Update the Y position (move upward)

	; Push projectile dimensions and model to the stack
	push projectile_height      ; Height of the projectile
	push projectile_width      ; Width of the projectile
    push cx                    ; Pointer to the projectile's model
    push [si + 2]              ; Push the X position onto the stack
    push [si + 4]              ; Push the updated Y position onto the stack

    call DrawModel             ; Call the procedure to redraw the projectile

@@exit:
    pop bp                     ; Restore the base pointer
    ret 2                      ; Clean up the stack and return
endp animate_projectile


proc projectile_hit_check
;--------------------------------------------------------
; Purpose:    
;             Checks if a projectile has hit the ceiling or missed its target.
; Inputs:     
;             [BP + 4] - Pointer to the projectile's data in the `active_projectiles` array.
; Behavior:   
;             - Retrieves the projectile's ID and Y position.
;             - Determines if the projectile is a laser or missile.
;             - Checks if the projectile has hit the ceiling.
;             - Deducts a penalty from the score if the projectile missed.
;             - Marks the projectile for deletion if it hit the ceiling.
; Outputs:    
;             - Updates the `score` if a penalty is applied.
;             - Marks the projectile as inactive if it hit the ceiling.
; Notes:
;             - The penalty for missing is defined by the `missed_shot_score_penalty` variable.
;             - This procedure assumes the projectile's data structure is organized as:
;               [ID, X position, Y position].
;--------------------------------------------------------
    push bp                ; Save the base pointer
    mov bp, sp             ; Set up the stack frame

    mov si, [bp + 4]       ; Load the pointer to the projectile's data
    mov dx, [si]           ; Load the projectile ID

    ; Check if the projectile id is not active
    cmp dx, [no_projectile_id]
    je @@exit

	; Check if the projectile id is marked for deletion
    cmp dx, [delete_projectile_id]
    je @@exit

@@check_ceiling:
    ; Check if the projectile has hit the ceiling
    mov dx, 0
    cmp [si + 4], dx       ; Compare Y position with 0 (ceiling)
    ja @@exit       ; If below the ceiling, skip

    ; Mark the projectile for deletion
    mov dx, [delete_projectile_id]
    mov [si], dx

    ; Penalize score if the projectile missed
    cmp [score], 0
    jbe @@exit      ; Skip if score is already 0 or negative
    mov dx, [missed_shot_score_penalty]
    sub [score], dx        ; Deduct penalty from score

@@exit:
    pop bp                 ; Restore the base pointer
    ret 2                  ; Clean up the stack and return
endp projectile_hit_check


proc projectiles_handler
;--------------------------------------------------------
; Purpose:    
;             Handles all active projectiles by checking for collisions and animating them.
; Inputs:     
;             None (operates on the `active_projectiles` array).
; Behavior:   
;             - Iterates through the `active_projectiles` array.
;             - Calls `projectile_hit_check` to check if a projectile has hit the ceiling or missed.
;             - Calls `animate_projectile` to update the projectile's position and redraw it.
; Outputs:    
;             - Updates the `active_projectiles` array to reflect changes in projectile states.
; Notes:
;             - Each projectile's data structure is assumed to be organized as:
;               [ID, X position, Y position].
;             - The procedure stops processing when all projectiles in the array have been handled.
;--------------------------------------------------------
	push bp                				; Save the base pointer
    mov bp, offset active_projectiles 	; Start of the `active_projectiles` array

@@projectile_loop:
    push bp								; Pointer to current projectile
    call projectile_hit_check         	; Check if the projectile hit the ceiling or missed
    push bp								; Pointer to current projectile
    call animate_projectile           	; Animate the projectile

    add bp, 6                         	; Move to the next projectile slot
    mov ax, bp
    sub ax, offset active_projectiles	; Calculate the offset from the start of the array
    cmp ax, max_active_projectiles    	; Check if we've reached the end of the array
    jb @@projectile_loop                ; Continue looping if not at the end

	pop bp								; Restore the base pointer
    ret                               	; Return when all projectiles are processed
endp projectiles_handler


proc animate_enemy
;--------------------------------------------------------
; Purpose:
;             Animates a single enemy by updating its position on the screen.
; Inputs:
;             [BP + 4] - Pointer to the enemy's data in the `active_enemies` array.
; Behavior:
;             - Retrieves the enemy's Status, X, and Y positions from the `active_enemies` array.
;             - Checks if the enemy is active (Status not 0).
;             - Updates the enemy's X position based on 'enemies_moving_right' flag is set.
;             - Updates the enemy's Y position if the `enemies_move_down` flag is set.
;             - Calls `DrawModel` to redraw the enemy at its new position.
; Outputs:
;             - Updates the enemy's position in the `active_enemies` array.
;             - Redraws the enemy at its new position on the screen.
; Notes:
;             - The movement step is defined by the `enemy_tick_x_movement` and 'enemy_border_y_movement' variable.
;             - This procedure assumes that the enemy's data structure is organized as:
;               [Status, X position, Y position].
;--------------------------------------------------------
    push bp              			  	; Save the base pointer
    mov bp, sp           				; Set up the stack frame

	mov si, [bp + 4]					; Load enemy data
	mov dx, [si]						; Get enemy status

	cmp dx, inactive_enemy_id    		; If enemy status is inactive
	je @@exit							; Skip animation for inactive enemies

	cmp [enemies_moving_right], 1		; Check movement direction
	je @@move_right						; Flag is set, move right

@@move_left:
	mov dx, enemy_tick_x_movement		; Get movement step
	sub [si + 2], dx					; Subtract from X position to move left
	jmp @@move_down_check				; Check for downward movement

@@move_right:
	mov dx, enemy_tick_x_movement		; Get movement step
	add [si + 2], dx					; Add to X position to move right
	jmp @@move_down_check				; Check for downward movement

@@move_down_check:
	cmp [enemies_move_down], 0			; Check if enemies should move down
	je @@animate						; No downward movement, proceed to animation
	mov dx, enemy_border_y_movement 	; Get movement step
	add [si + 4], dx					; Add to Y position to move down

@@animate:
	push enemy_height					; Enemy height
	push enemy_width					; Enemy width
	push offset Enemy					; Pointer to enemy model
	push [si + 2]						; X position
	push [si + 4]						; Y position
	call DrawModel						; Draw the enemy model

@@exit:
    pop bp                 				; Restore the base pointer
    ret 2                  				; Clean up the stack and return
endp animate_enemy


proc enemies_handler
;--------------------------------------------------------
; Purpose:    
;             Handles all enemies by checking for collisions and animating them.
; Inputs:     
;             None (operates on the `active_enemies` array).
; Behavior:   
;             - Iterates through the `active_enemies` array.
;             - Calls `animate_enemy` to update the enemy position and redraw it.
; Outputs:    
;             - Updates the `active_enemies` array to reflect changes in enemies states.
; Notes:
;             - Each enemy's data structure is assumed to be organized as:
;               [Status, X position, Y position].
;             - The procedure stops processing when all enemies in the array have been handled.
;--------------------------------------------------------
	push bp              			  	; Save the base pointer
    mov bp, offset active_enemies		; Start of the `active_enemies` array

@@enemy_loop:
	push bp								; Pointer to current enemy
	call animate_enemy					; Animates it

	add bp, 6							; Move to the next enemy slot	
	mov ax, bp							
	sub ax, offset active_enemies		; Calculate the offset from the start of the array
	cmp ax, max_active_enemies			; Check if we've reached the end of the array
	jb @@enemy_loop						; Continue looping if not at the end

	pop bp								; Restore the base pointer
	ret									; Clean up the stack and return
endp enemies_handler


proc display_score
;--------------------------------------------------------
; Purpose:    
;             Displays the player's current score on the screen.
; Inputs:     
;             None (uses the `score` variable from the data segment).
; Behavior:   
;             - Prints a "Score -" message on the screen.
;             - Converts the `score` value into its decimal representation.
;             - Pads the score with leading zeros to ensure it is at least 4 digits long.
;             - Prints the score digits one by one on the screen.
; Outputs:    
;             - Displays the "Score -" message followed by the player's score.
; Assumptions:
;             - The `score` variable is a positive integer.
;             - Uses BIOS interrupt 21h (function 02h) to print characters.
; Notes:
;             - The procedure does not handle negative scores.
;             - The score will always be displayed as a 4-digit number, padded with leading zeros if necessary.
;--------------------------------------------------------
    push bp                ; Save the base pointer
    mov bp, sp             ; Set up the stack frame

    ; Print the "Score -" message
    push offset score_msg  ; Address of the "Score -" message
    push 0101h             ; Screen position (row 1, column 1)
    push 8                 ; Length of the message
    call print_string      ; Call the print_string procedure

    ; Load the score into AX and clear CX (digit counter)
    mov ax, [score]        ; Load the score into AX
    xor cx, cx             ; Clear CX (digit counter)

@@find_digits:
    cmp ax, 0              ; Check if the score division result is 0
    je @@zero_fill           ; If 0, jump to zero-padding logic

    xor dx, dx             ; Clear DX (remainder)
    mov bx, 10             ; Set divisor to 10
    div bx                 ; Divide AX by BX (result in AX, remainder in DX)

    push dx                ; Push the remainder (digit) onto the stack
    inc cx                 ; Increment the digit counter
    jmp @@find_digits        ; Repeat until the score is fully converted

@@zero_fill:
    cmp cx, 4              ; Check if the score has at least 4 digits
    jae @@print_digits       ; If yes, jump to printing digits
    push 0                 ; Push a zero onto the stack
    inc cx                 ; Increment the digit counter
    jmp @@zero_fill          ; Repeat until 4 digits are reached

@@print_digits:
    cmp cx, 0              ; Check if there are digits to print
    je @@exit           ; If no digits, finish

    pop dx                 ; Pop the next digit from the stack
    add dx, 48             ; Convert the digit to its ASCII representation

    mov ah, 02h            ; BIOS interrupt to print a character
    int 21h                ; Call BIOS interrupt

    dec cx                 ; Decrement the digit counter
    jmp @@print_digits       ; Repeat until all digits are printed

@@exit:
    pop bp                 ; Restore the base pointer
    ret                    ; Return from the procedure
endp display_score


proc echeck ;checks if the enemy touched the corners
	push bp
	mov bp, sp
	mov ax, offset active_enemies
@@cycle:
	mov si, ax
	add si, 2
	
	xor dx,dx
	mov dx, 20
	cmp [si], dx
	ja noright
	mov [enemies_moving_right], 1
	mov [enemies_move_down], 1
	jmp hell
noright:
	mov dx, 280
	cmp [si], dx
	jb ex
	mov [enemies_moving_right], 0
	mov [enemies_move_down], 1
	jmp hell
ex:
	add si, 2
	mov dx, 170
	cmp [si], dx
	jb notlost
	mov [dead_count], 30
notlost:	
	add ax, 6
	mov si, ax
	sub si, offset active_enemies
	cmp si, max_active_enemies
	jb @@cycle 
	mov [enemies_move_down], 0
hell:
	pop bp
	ret 
endp echeck


proc checkhit
	push bp
	mov bp, sp
	mov si, offset active_projectiles
	mov di, offset active_enemies
xcheck:	
	xor dx, dx
	cmp [di], dx
	je gonext
	cmp [si], dx
	je gonext
	jmp nonext
gonext:
jmp next
nonext:
	add si, 2
	mov ax, [si]
	add ax, 4

	add di, 2
	mov dx, [di]
	cmp ax, dx
	jb failx
	add dx, enemy_width
	cmp ax, dx
	ja failx
ycheck:
	add si, 2
	mov ax,[si]
	add ax, 2
	
	add di,2
	mov dx, [di]
	cmp ax, dx
	jb faily
	add dx, enemy_height
	cmp ax, dx
	ja faily
boom:
	inc [dead_count]
	add [score], 100d
	sub si, 4
	mov dx, [delete_projectile_id]
	mov [si], dx
	
	sub di, 4
	xor dx,dx
	mov [di], dx

	push enemy_height
	push enemy_width
	push offset dead
	add di,2
	push [di]
	add di,2
	push [di]
	call DrawModel
	sub di,4
	jmp next
failx:
	sub si, 2
	sub di, 2
	jmp next
faily:
	sub si, 4
	sub di, 4
next:
	add si, 6
	mov dx, si
	sub dx, offset active_projectiles
	cmp dx, max_active_projectiles
	jb jump_shortcut
	mov si, offset active_projectiles
	add di, 6
	mov dx, di
	sub dx, offset active_enemies
	cmp dx, max_active_enemies
	ja fail
jump_shortcut:
	jmp xcheck
fail:
	pop bp
	ret
endp checkhit

start:
    mov ax, @data
    mov ds, ax
	
    mov ax, 0013h
    int 10h
;difficulty select 
	push offset ez_difficult_msg
	push 409h
	push 16
	call print_string
	push offset medium_difficult_msg
	push 809h
	push 18
	call print_string
	push offset hard_difficult_msg
	push 0c09h
	push 16
	call print_string
difficulty?:
	mov ah, 1
    int 16h 
    jz difficulty? 

    mov ah, 0 
    int 16h
	cmp al, '1'
	je easy
	cmp al, '2'
	je med1um
	cmp al, '3'
	je hard
	jmp difficulty?
easy:
	mov [missed_shot_score_penalty], 10
	jmp enddif
med1um:
	mov [missed_shot_score_penalty], 50
	jmp enddif
hard:
	mov [missed_shot_score_penalty], 100
	jmp enddif
enddif:
	mov ax,0600h    
	mov BH,00h    
	mov CX,0000h    
	mov DX,184fh    
    int 10h        
	mov ax, 0600h
	int 10h
;starting page
	push offset start_guide_msg
	push 409h
	push 20
	call print_string
	push offset movement_guide_msg
	push 809h
	push 19
	call print_string
	push offset shooting_guide_msg
	push 0c09h
	push 20
	call print_string
	push offset win_criteria_msg
	push 1008h
	push 22
	call print_string
	
	
start?:
	mov ah, 1
    int 16h ;checking for input
    jz start? ;no input:looping

    mov ah, 0 ;saving the input
    int 16h
	cmp al, ' '
	jne start?
	
	mov ax,0600h    
	mov BH,00h    
	mov CX,0000h    
	mov DX,184fh    
    int 10h        
	mov ax, 0600h
	int 10h
	shipandammo:
	push spaceship_height
	push spaceship_width
	push offset spaceship_model
	push [spaceship_curr_x]
	push [spaceship_curr_y]
	call DrawModel
shots_annimation:  
    mov ah,2ch
    int 21h
    cmp [time],dl
    je noshot
	
	inc [ticks_since_last_projectile_registration]
	
	call projectiles_handler
	call enemies_handler
	call echeck
	call checkhit
	
	mov ah,2ch
    int 21h
    mov [time], dl
noshot:
	call display_score
	jmp victoycheck
inputloop:
	mov ah, 1
    int 16h 
    jz shots_annimation 

    mov ah, 0 
    int 16h
	
	cmp al, 'a'
	jne NotLeft
	cmp [spaceship_curr_x], 20
	jbe NotLeft
	sub [spaceship_curr_x],2 
	push spaceship_height
	push spaceship_width
	push offset spaceship_model
	push [spaceship_curr_x]
	push [spaceship_curr_y]
	call DrawModel
	jmp shots_annimation

NotLeft:
	cmp al, 'd'
	jne NotRight
	cmp [spaceship_curr_x], 280 
	jae NotRight
	add [spaceship_curr_x],2 
	push spaceship_height
	push spaceship_width
	push offset spaceship_model
	push [spaceship_curr_x]
	push [spaceship_curr_y]
	call DrawModel
	jmp shots_annimation
	
NotRight:
	cmp al, 'q'
	jne NotMissile

	push [missile_id]
	call register_projectile
	jmp shots_annimation
NotMissile:
	cmp al, 'e'
	jne NotLaser
	
	push [laser_id]
	call register_projectile
	jmp	shots_annimation
NotLaser:
	cmp al, 'p'
	jne exitoffrange
	jmp exit
exitoffrange:	
	jmp shots_annimation 
victoycheck:
	cmp [dead_count], 24
	je win
	cmp [dead_count], 30
	je lose
	jmp inputloop
win:
	cmp [score], 2000
	jb lose
	push offset game_won_msg
	push 709h
	push 18
	call print_string
	jmp outO
lose:
	push offset game_lost_msg
	push 709h
	push 19
	call print_string
outO:
	mov ah,2ch
    int 21h
    cmp [stop],dl
    je outO

	cmp [cn],1
	je firstnote
	cmp [cn],2
	je secendnote
	cmp [cn],3
	je thrednote
	cmp [cn],4
	je  fournote
	jmp endsound
firstnote:
	push [note1]
	jmp callsound
secendnote:
	push [note2]
	jmp callsound
thrednote:
	push [note3]
	jmp callsound
fournote:
	push [note4]
	jmp callsound
callsound:
	call play_note
	inc [cn]
endsound:
	mov ah,2ch
    int 21h
    mov [stop],dl

	mov ah, 1
    int 16h ;checking for input
    jz outO ;no input:looping

    mov ah, 0 ;saving the input
    int 16h
	cmp al, 'p';checking for pause
	je exit ; F
	jmp outO
exit:
    mov ax, 4c00h
    int 21h
    END start