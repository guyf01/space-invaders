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
	right_boundary equ 280
	left_boundary equ 20
; ------------------
; ship variables
; ------------------
	spaceship_curr_x dw 130
	spaceship_curr_y dw 180

	spaceship_move_step equ 2
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
	enemies_floor_boundary equ 170
	enemy_reached_bottom dw 0
	enemies_moving_right dw 1
	enemy_tick_x_movement equ 1
	enemy_border_y_movement equ 3
	enemies_move_down dw 0
	enemy_score_value equ 100
	max_active_enemies equ 144
	active_enemies 	dw 2,40,20
					dw 2,80,20
					dw 2,120,20
					dw 2,160,20
					dw 2,200,20
					dw 2,240,20
					dw 2,40,50
					dw 2,80,50
					dw 2,120,50
					dw 2,160,50
					dw 2,200,50
					dw 2,240,50
					dw 2,40,80
					dw 2,80,80
					dw 2,120,80
					dw 2,160,80
					dw 2,200,80
					dw 2,240,80
					dw 2,40,110
					dw 2,80,110
					dw 2,120,110
					dw 2,160,110
					dw 2,200,110
					dw 2,240,110

	inactive_enemy_id equ 0

	delete_enemy_id equ 1
	delete_enemy_model 		db 00,00,00,00,00,00,00,00,00,00,00,00,00
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

	active_enemy_id equ 2
	active_enemy_model		db 00,00,00,00,00,00,00,00,00,00,00,00,00
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
	push bp 			  ; Save the base pointer
    mov bp, sp            ; Set up the stack frame

	; setup configurations from stack
    mov al, [bp + 4]      ; Color of the pixel
    mov cx, [bp + 6]      ; X position
    mov dx, [bp + 8]      ; Y position

	; execute BIOS interrupt
    mov ah, 0ch           ; BIOS interrupt for drawing a pixel
    int 10h               ; Call BIOS interrupt

    pop bp                ; Restore the base pointer
    ret 6                 ; Clean up the stack and return
endp draw_pixel


proc draw_model
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
	jbe @@pixels_loop       ; if currX - startX <= width, continue row

	; End of row: reset X and move down one Y
	mov dx, [bp + 6]       	; reset X to startX
	inc ax                 	; advance to next Y row

	; Check if we've drawn `height` rows:
	mov cx, ax
	sub cx, [bp + 4]       	; cx = currY - startY
	cmp cx, [bp + 12]      	; compare with height
	jbe @@pixels_loop       ; if currY - startY <= height, continue column

    pop bp                 	; Restore the base pointer
    ret 10                  ; Clean up the stack and return
endp draw_model


proc register_projectile
;--------------------------------------------------------
; Purpose:    Registers a projectile into the active_projectiles array.
; Inputs:     
;             [BP + 4] - The key pressed on the keyboard representing the projectile type.
; Behavior:   
;             - Checks if enough time has passed since the last projectile was registered.
;             - Adds the projectile's ID, X position, and Y position to the 
;               `active_projectiles` array.
;             - Updates the `active_projectiles_next_slot` pointer to the next available slot.
; Outputs:    
;             - Updates the `active_projectiles` array with the new projectile's data.
;             - Resets the `ticks_since_last_projectile_registration` counter.
;--------------------------------------------------------
    push bp                											; Save the base pointer
    mov bp, sp             											; Set up the stack frame

    ; Check if enough time has passed since the last projectile
    mov ax, [ticks_since_last_projectile_registration]				; Load the ticks since last registration
    cmp ax, [minimum_ticks_between_projectiles]						; Compare with minimum required ticks
    jb @@exit 														; Exit if not enough time has passed

	mov ax, [bp + 4]           										; Get the key pressed from the stack

	cmp al, 'e'														; Check if key is 'e'
	jne @@check_missile												; If not, check for 'q'

	mov dx, [laser_id]         										; Load laser projectile ID
	jmp @@load_active_slot											; Proceed to load active slot

@@check_missile:
	cmp al, 'q'														; Check if key is 'q'
	jne @@exit														; Exit if key is neither 'e' nor 'q'

	mov dx, [missile_id]       										; Load missile projectile ID

@@load_active_slot:
    ; Load the next available slot in the active_projectiles array
    mov ax, [active_projectiles_next_slot]							; Load the pointer to the next available slot
    sub ax, offset active_projectiles								; Calculate the offset from the start of the array
    cmp ax, max_active_projectiles									; Check if we've reached the end of the array
    jb @@register

    mov [active_projectiles_next_slot], offset active_projectiles	; Reset to the start of the array if the end is reached

@@register:
    mov si, [active_projectiles_next_slot]							; Load the pointer to the next available slot

    ; Store the projectile ID in the array
    mov [si], dx             									  	; Store the projectile ID in the array

    ; Store the X position of the projectile
    mov dx, [spaceship_curr_x] 										; Get the spaceship's current X position
    add dx, 2                  										; Adjust the X position for the projectile
    mov [si + 2], dx           										; Store the adjusted X position in the array

    ; Store the Y position of the projectile
    mov dx, [spaceship_curr_y] 										; Get the spaceship's current Y position
    sub dx, 12                 										; Adjust the Y position for the projectile
    mov [si + 4], dx           										; Store the adjusted Y position in the array

    ; Update the next available slot pointer
    add [active_projectiles_next_slot], 6							; Move to the next slot (each projectile uses 6 bytes)

    ; Reset the ticks counter
    mov [ticks_since_last_projectile_registration], 0				; Reset the ticks since last registration	

@@exit:
    pop bp                 											; Restore the base pointer
    ret 2                 											; Clean up the stack and return
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
;             - Calls `draw_model` to redraw the projectile at its new position.
; Outputs:    
;             - Updates the projectile's position in the `active_projectiles` array.
;             - Redraws the projectile at its new position on the screen.
; Notes:
;             - The movement step is defined by the `projectile_tick_movement` variable.
;             - This procedure assumes that the projectile's data structure is organized as:
;               [ID, X position, Y position].
;             - If the projectile is marked for deletion, it is replaced with the `delete_projectile_model`.
;--------------------------------------------------------
    push bp               							; Save the base pointer
    mov bp, sp             							; Set up the stack frame

    mov si, [bp + 4]       							; Load the pointer to the projectile's data
    mov dx, [si]          							; Load the projectile ID

    cmp dx, [delete_projectile_id] 					; Check if the projectile is marked for deletion
    je @@delete										; Jump to deletion animation

    cmp dx, [laser_id]     							; Check if the projectile is a laser
    je @@laser										; Jump to laser animation

    cmp dx, [missile_id]   							; Check if the projectile is a missile
    je @@missile									; Jump to missile animation

    jmp @@exit        								; Skip to the end if no match found

@@delete:
    mov ax, [no_projectile_id] 						; Mark the projectile as inactive
    mov [si], ax				   					; Update the projectile ID to inactive
    mov cx, offset delete_projectile_model			; Load the deletion model
    jmp @@animate             						; Proceed to animate the deletion

@@laser:
    mov cx, offset laser_model 						; Load the laser model
    jmp @@animate									; Proceed to animate the laser

@@missile:
    mov cx, offset missile_model 					; Load the missile model
    jmp @@animate									; Proceed to animate the missile

@@animate:
    mov dx, projectile_tick_movement 				; Load the movement step
    sub [si + 4], dx           						; Update the Y position (move upward)

	push projectile_height      					; Height of the projectile
	push projectile_width     						; Width of the projectile
    push cx                    						; Pointer to the projectile's model
    push [si + 2]              						; Push the X position onto the stack
    push [si + 4]              						; Push the updated Y position onto the stack

    call draw_model             					; Call the procedure to redraw the projectile

@@exit:
    pop bp                     						; Restore the base pointer
    ret 2                      						; Clean up the stack and return
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
    push bp                				; Save the base pointer
    mov bp, sp            	 			; Set up the stack frame

    mov si, [bp + 4]       				; Load the pointer to the projectile's data
    mov dx, [si]           				; Load the projectile ID

    cmp dx, [no_projectile_id]			; Check if the projectile id is not active
    je @@exit							; Exit if not active

    cmp dx, [delete_projectile_id]		; Check if the projectile id is marked for deletion
    je @@exit							; Exit if marked for deletion

@@check_ceiling:
    mov dx, 0							; Load 0 (ceiling Y position)
    cmp [si + 4], dx       				; Compare Y position with 0 (ceiling)
    ja @@exit       					; If below the ceiling, skip

	; Mark the projectile for deletion
    mov dx, [delete_projectile_id]		; Load delete projectile ID
    mov [si], dx						; Update the projectile ID to mark for deletion

    ; Penalize score if the projectile missed
    cmp [score], 0						; Check if score is 0 or negative
    jbe @@exit      					; Skip if score is already 0 or negative
    mov dx, [missed_shot_score_penalty] ; Load the penalty value
    sub [score], dx        				; Deduct penalty from score

@@exit:
    pop bp                 				; Restore the base pointer
    ret 2                  				; Clean up the stack and return
endp projectile_hit_check


proc enemy_hit_check
;--------------------------------------------------------
; Purpose:
;             Checks if a projectile has hit any active enemies.
; Inputs:
;             [BP + 4] - Pointer to the projectile's data in the `active_projectiles` array.
; Behavior:
;             - Retrieves the projectile's ID and position.
;             - Iterates through the `active_enemies` array.
;             - For each active enemy, checks if the projectile's position overlaps with the enemy's position.
;             - If a hit is detected, marks both the projectile and enemy for deletion,
;               increments the `dead_count`, and updates the score.
; Outputs:
;             - Updates the `active_enemies` array and `score` if a hit is detected.
; Notes:
;             - The procedure assumes the projectile's data structure is organized as:
;               [ID, X position, Y position].
;             - Each enemy's data structure is organized as:
;               [Status, X position, Y position].
;--------------------------------------------------------
    push bp							; Save the base pointer
    mov bp, sp						; Set up the stack frame

    mov di, [bp + 4]				; Load the pointer to the projectile's data
    mov dx, [di]					; Load the projectile ID

    cmp dx, [no_projectile_id]		; Check if the projectile id is not active
    je @@exit						; Exit if not active

    cmp dx, [delete_projectile_id]	; Check if the projectile id is marked for deletion
    je @@exit						; Exit if marked for deletion

	mov si, offset active_enemies	; Start of the `active_enemies` array

@@enemy_loop:
	mov dx, [si]					; Load current enemy's Status

	cmp dx, [inactive_enemy_id]		; If current enemy's Status == inactive, skip it
	je @@next_enemy                 ; Jump to advance to the next slot

	cmp dx, [delete_enemy_id]       ; If current enemy's Status == inactive, skip it
	je @@next_enemy                 ; Jump to advance to the next slot

@@check_x:
	mov ax, [di + 2]                ; Load projectile X position
	add ax, 6						; Adjust projectile X for width for collision
	mov dx, [si + 2]                ; Load enemy X position
	cmp ax, dx                      ; Compare projectile X with enemy X
	jb @@next_enemy                 ; If projectile X < enemy X, go to next enemy

	add dx, 16						; Add enemy width to enemy X position for collision
	cmp ax, dx                      ; Compare projectile X with enemy right boundary
	ja @@next_enemy                 ; If projectile X > enemy right boundary, go to next enemy

@@check_y:
	mov ax, [di + 4]                ; Load projectile Y position
	mov dx, [si + 4]                ; Load enemy Y position
	cmp ax, dx                      ; Compare projectile Y with enemy Y
	jb @@next_enemy                 ; If projectile Y < enemy Y, go to next enemy

	add dx, enemy_height            ; Calculate enemy bottom boundary
	cmp ax, dx                      ; Compare projectile Y with enemy bottom boundary
	ja @@next_enemy                 ; If projectile Y > enemy bottom boundary, go to next enemy

@@hit:
	mov dx, [delete_projectile_id]	; Load delete projectile ID
	mov [di], dx					; Mark projectile for deletion

	mov dx, [delete_enemy_id]		; Load delete enemy ID
	mov [si], dx					; Mark enemy for deletion

	inc [dead_count]				; Increment dead enemy count

	mov dx, enemy_score_value		; Load enemy score value
	add [score], dx					; Update score

@@next_enemy:
	add si, 6						; Move to the next enemy slot (each enemy is 6 bytes: Status, X, Y)
	mov ax, si						; Load current position
	sub ax, offset active_enemies	; Calculate the offset from the start of the array
	cmp ax, max_active_enemies		; Check if we've reached the end of the array
	jb @@enemy_loop					; Continue looping if not at the end

@@exit:
    pop bp                 			; Restore the base pointer
    ret 2                  			; Clean up the stack and return
endp enemy_hit_check


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
	push bp								; Pointer to current projectile
	call enemy_hit_check				; Check if the projectile hit any enemy

    add bp, 6                         	; Move to the next projectile slot
    mov ax, bp
    sub ax, offset active_projectiles	; Calculate the offset from the start of the array
    cmp ax, max_active_projectiles    	; Check if we've reached the end of the array
    jb @@projectile_loop                ; Continue looping if not at the end

	pop bp								; Restore the base pointer
    ret                               	; Return when all projectiles are processed
endp projectiles_handler


proc enemies_direction_handler
;--------------------------------------------------------
; Purpose:    
;             Handles the movement direction and boundary detection for all enemies.
; Inputs:     
;             None (operates on the `active_enemies` array).
; Behavior:   
;             - Iterates through the `active_enemies` array.
;             - For each active enemy, checks its boundaries:
;                 - Checks if the enemy has reached the bottom of the screen (Y >= enemies_floor_boundary).
;                 - Checks if the enemy has reached the left boundary (X <= left_boundary).
;                 - Checks if the enemy has reached the right boundary (X >= right_boundary).
;             - Updates movement flags accordingly:
;                 - `enemies_moving_right`: Toggles direction when hitting left or right boundary.
;                 - `enemies_move_down`: Set when hitting left/right boundaries to move all enemies down.
;                 - `enemy_reached_bottom`: Set if any enemy reaches the bottom of the screen.
; Outputs:    
;             - Updates the `enemies_moving_right` flag to change horizontal movement direction.
;             - Updates the `enemies_move_down` flag to trigger vertical movement.
;             - Updates the `enemy_reached_bottom` flag if any enemy reaches the floor.
; Notes:
;             - Each enemy's data structure is assumed to be organized as:
;               [Status, X position, Y position].
;             - Inactive enemies (Status = inactive_enemy_id) are skipped during processing.
;             - The procedure stops processing on the first boundary condition encountered.
;--------------------------------------------------------
	push bp                          ; Save the base pointer
	mov si, offset active_enemies    ; Start of `active_enemies` array (pointer to first enemy slot)

@@enemy_loop:
	mov dx, [si]                     ; Load current enemy's Status

	cmp dx, [inactive_enemy_id]      ; If current enemy's Status == inactive, skip it
	je @@next_enemy                  ; Jump to advance to the next slot

	cmp dx, [delete_enemy_id]		 ; If current enemy's Status == delete, skip it
	je @@next_enemy                  ; Jump to advance to the next slot

@@check_floor:
	mov dx, enemies_floor_boundary   ; Load bottom boundary constant
	cmp [si + 4], dx                 ; Compare Y position (offset +4) with floor
	jb @@check_left                  ; If Y < floor, continue boundary checks

	mov [enemy_reached_bottom], 1    ; Enemy reached floor — set game-over flag
	jmp @@exit                       ; Exit early; no further processing required

@@check_left:
	mov dx, left_boundary    		 ; Load left boundary constant
	cmp [si + 2], dx                 ; Compare X position (offset +2) with left boundary
	ja @@check_right                 ; If X > left boundary, check right side

	mov [enemies_moving_right], 1    ; Set direction to move right
	mov [enemies_move_down], 1       ; Request a move-down on next enemy update
	jmp @@exit						 ; Exit early; no further processing required

@@check_right:
	mov dx, right_boundary   		 ; Load right boundary constant
	cmp [si + 2], dx                 ; Compare X position with right boundary
	jb @@next_enemy                  ; If X < right boundary, go to next enemy

	mov [enemies_moving_right], 0    ; Clear moving-right flag — change direction to left
	mov [enemies_move_down], 1       ; Request a move-down on next enemy update
	jmp @@exit						 ; Exit early; no further processing required

@@next_enemy:
	add si, 6						 ; Move to the next enemy slot (each enemy is 6 bytes: Status, X, Y)
	mov ax, si						 ; Load current position
	sub ax, offset active_enemies	 ; Calculate the offset from the start of the array
	cmp ax, max_active_enemies		 ; Check if we've reached the end of the array
	jb @@enemy_loop					 ; Continue looping if not at the end

@@exit:
	pop bp							 ; Restore the base pointer
	ret								 ; Clean up the stack and return
endp enemies_direction_handler


proc enemies_animation_handler
;--------------------------------------------------------
; Purpose:
;             Animates all enemies by by updating their position on the screen.
; Inputs:
;             None (operates on the `active_enemies` array).
; Behavior:
;             - Iterates through the `active_enemies` array.
;             - Retrieves the enemy's Status, X, and Y positions from the `active_enemies` array.
;             - Checks if the enemy is active (Status not 0).
;             - Updates the enemy's X position based on 'enemies_moving_right' flag is set.
;             - Updates the enemy's Y position if the `enemies_move_down` flag is set.
;             - Calls `draw_model` to redraw the enemy at its new position.
; 			  - Resets the `enemies_move_down` flag after processing all enemies.
; Outputs:
;             - Updates the enemy's position in the `active_enemies` array.
;             - Redraws the enemy at its new position on the screen.
; Notes:
;             - The movement step is defined by the `enemy_tick_x_movement` and 'enemy_border_y_movement' variable.
;             - This procedure assumes that the enemy's data structure is organized as:
;               [Status, X position, Y position].
;             - The procedure stops processing when all enemies in the array have been handled.
;--------------------------------------------------------
	push bp              			  	; Save the base pointer
    mov si, offset active_enemies		; Start of the `active_enemies` array

@@enemy_loop:
	mov dx, [si]                     	; Load current enemy's Status

	cmp dx, [inactive_enemy_id]      	; If current enemy's Status == inactive, skip it
	je @@next_enemy                 	; Jump to advance to the next slot

	cmp dx, [delete_enemy_id]		  	; If current enemy's Status == delete
	jne @@active_enemy_checks

	mov dx, [inactive_enemy_id]		  	; Load delete enemy ID for comparison
	mov [si], dx						; Ensure enemy is marked as deleted
	mov cx, offset delete_enemy_model	; Pointer to delete enemy model
	jmp @@animate

@@active_enemy_checks:
	mov cx, offset active_enemy_model	; Pointer to enemy model

	cmp [enemies_move_down], 0			; Check if enemies should move down
	je @@direction_check				; No downward movement, proceed to direction check

	mov dx, enemy_border_y_movement 	; Get movement step
	add [si + 4], dx					; Add to Y position to move down

@@direction_check:
	cmp [enemies_moving_right], 1		; Check movement direction
	je @@move_right						; Flag is set, move right

@@move_left:
	mov dx, enemy_tick_x_movement		; Get movement step
	sub [si + 2], dx					; Subtract from X position to move left
	jmp @@animate						; proceed to animation

@@move_right:
	mov dx, enemy_tick_x_movement		; Get movement step
	add [si + 2], dx					; Add to X position to move right

@@animate:
	push si								; Save pointer to current enemy
	push enemy_height					; Enemy height
	push enemy_width					; Enemy width
	push cx								; Pointer to enemy model
	push [si + 2]						; X position
	push [si + 4]						; Y position
	call draw_model						; Draw the enemy model
	pop si								; Restore pointer to current enemy

@@next_enemy:
	add si, 6							; Move to the next enemy slot (each enemy is 6 bytes: Status, X, Y)
	mov ax, si						 	; Load current position
	sub ax, offset active_enemies	 	; Calculate the offset from the start of the array
	cmp ax, max_active_enemies		 	; Check if we've reached the end of the array
	jb @@enemy_loop					 	; Continue looping if not at the end

	cmp [enemies_move_down], 0			; Reset move down flag after processing all enemies
	je @@exit							; If already 0, exit
	mov [enemies_move_down], 0			; Reset the flag

@@exit:
	pop bp								; Restore the base pointer
	ret									; Clean up the stack and return
endp enemies_animation_handler


proc print_string
;--------------------------------------------------------
; Purpose:    Print a string on the screen at a specified location.
; Inputs:     [BP+4] - Length of the string (word).
;             [BP+6] - Screen location (word, row and column packed into a single word).
;             [BP+8] - Pointer to the string in the data segment to print (word).
; Outputs:    None.
;--------------------------------------------------------
	push bp 				; Save the base pointer
    mov bp, sp				; Set up the stack frame

	; setup configurations from stack
    mov cx, [bp + 4]		; Length of the string to print
    mov dx, [bp + 6]		; Screen location (row and column packed into a single word)
    mov bp, [bp + 8]        ; Pointer to the string to print
    mov bl, 00001111b       ; Set BL to the text color (white on black background)
    mov al, 1               ; Set AL to 1 (String is written and the cursor position is updated)

    ; Load data segment for the string
    mov si, @data			; Load data segment address
    mov es, si				; Set ES to data segment

	; execute BIOS interrupt
    mov ah, 13h             ; BIOS interrupt for printing a string
    int 10h                 ; Call BIOS interrupt

    pop bp                  ; Restore the base pointer
    ret 6                   ; Clean up the stack and return
endp print_string


proc clear_screen
;--------------------------------------------------------
; Purpose:
;             Clears the screen by filling it with black pixels.
; Inputs:
;             None (operates on the video memory directly).
; Behavior:
;             - Fills the entire video memory (64,000 bytes) with black pixels (color 0).
; Outputs:
;             - The screen is cleared to black.
; Notes:
;             - Uses the `stosw` instruction to efficiently fill the video memory.
;--------------------------------------------------------
	push bp				; Save the base pointer
	mov bp, sp          ; Set up the stack frame

	mov ax, 0A000h      ; Video memory segment for mode 13h
	mov es, ax          ; Set ES to video memory segment
	xor di, di          ; Start at offset 0
	xor ax, ax          ; AX = 0 (black pixel color)
	mov cx, 32000       ; 32000 words = 64000 bytes

	rep stosw           ; Fill video memory with black pixels

	pop bp              ; Restore the base pointer
	ret                 ; Clean up the stack and return
endp clear_screen


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
    je @@zero_fill         ; If 0, jump to zero-padding logic

    xor dx, dx             ; Clear DX (remainder)
    mov bx, 10             ; Set divisor to 10
    div bx                 ; Divide AX by BX (result in AX, remainder in DX)

    push dx                ; Push the remainder (digit) onto the stack
    inc cx                 ; Increment the digit counter
    jmp @@find_digits      ; Repeat until the score is fully converted

@@zero_fill:
    cmp cx, 4              ; Check if the score has at least 4 digits
    jae @@print_digits     ; If yes, jump to printing digits
    push 0                 ; Push a zero onto the stack
    inc cx                 ; Increment the digit counter
    jmp @@zero_fill        ; Repeat until 4 digits are reached

@@print_digits:
    cmp cx, 0              ; Check if there are digits to print
    je @@exit              ; If no digits, finish

    pop dx                 ; Pop the next digit from the stack
    add dx, 48             ; Convert the digit to its ASCII representation

    mov ah, 02h            ; BIOS interrupt to print a character
    int 21h                ; Call BIOS interrupt

    dec cx                 ; Decrement the digit counter
    jmp @@print_digits     ; Repeat until all digits are printed

@@exit:
    pop bp                 ; Restore the base pointer
    ret                    ; Clean up the stack and return
endp display_score


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


proc difficulty_select
;--------------------------------------------------------
; Purpose:
;             Allows the player to select a difficulty level for the game.
; Inputs:
;             None (waits for keyboard input).
; Behavior:
;             - Displays difficulty options on the screen.
;             - Waits for the player to press '1', '2', or '3' to select Easy, Medium, or Hard difficulty.
;             - Sets the `missed_shot_score_penalty` variable based on the selected difficulty.
; Outputs:
;             - Updates the `missed_shot_score_penalty` variable.
; Notes:
;             - '1' sets Easy difficulty (10 points penalty).
;             - '2' sets Medium difficulty (50 points penalty).
;             - '3' sets Hard difficulty (100 points penalty).
;--------------------------------------------------------
	push bp                 				; Save the base pointer
	mov bp, sp              				; Set up the stack frame

	push offset ez_difficult_msg			; Display Easy difficulty message
	push 409h								; Screen position
	push 16									; Length of the message
	call print_string

	push offset medium_difficult_msg		; Display Medium difficulty message
	push 809h								; Screen position
	push 18									; Length of the message
	call print_string

	push offset hard_difficult_msg			; Display Hard difficulty message
	push 0c09h								; Screen position
	push 16									; Length of the message
	call print_string

@@get_input:
    mov ah, 0 								; Wait for keyboard input
    int 16h									; BIOS interrupt to read keyboard input

	cmp al, '1'								; Check if '1' (Easy) was pressed
	jne @@check_medium						; If not, check for Medium

	mov [missed_shot_score_penalty], 10		; Set penalty for Easy difficulty
	jmp @@exit								; Difficulty selected, exit

@@check_medium:
	cmp al, '2'								; Check if '2' (Medium) was pressed
	jne @@check_hard						; If not, check for Hard

	mov [missed_shot_score_penalty], 50		; Set penalty for Medium difficulty
	jmp @@exit								; Difficulty selected, exit

@@check_hard:
	cmp al, '3'								; Check if '3' (Hard) was pressed
	jne @@get_input							; Not valid input, wait again
	
	mov [missed_shot_score_penalty], 100	; Set penalty for Hard difficulty

@@exit:
	call clear_screen						; Clear the screen after selection

	pop bp                  				; Restore the base pointer
	ret                     				; Clean up the stack and return
endp difficulty_select


proc display_guide_page
;--------------------------------------------------------
; Purpose:
;             Displays the game guide page with instructions.
; Inputs:
;             None.
; Behavior:
;             - Displays various instructional messages on the screen.
; Outputs:
;             - The guide page is displayed until the player presses the space bar.
;--------------------------------------------------------
	push bp                 				; Save the base pointer
	mov bp, sp              				; Set up the stack frame

	push offset start_guide_msg				; Display start guide message
	push 409h								; Screen position
	push 20									; Length of the message
	call print_string

	push offset movement_guide_msg			; Display movement guide message
	push 809h								; Screen position
	push 19									; Length of the message
	call print_string

	push offset shooting_guide_msg			; Display shooting guide message
	push 0c09h								; Screen position
	push 20									; Length of the message
	call print_string

	push offset win_criteria_msg			; Display win criteria message
	push 1008h								; Screen position
	push 22									; Length of the message
	call print_string

@@get_input:
    mov ah, 0 								; Wait for keyboard input
    int 16h									; BIOS interrupt to read keyboard input

	cmp al, ' '								; Check if space bar was pressed
	jne @@get_input							; If not, wait again

	call clear_screen						; Clear the screen after exiting guide

	pop bp                  				; Restore the base pointer
	ret                     				; Clean up the stack and return
endp display_guide_page


proc spaceship_movement_handler
;--------------------------------------------------------
; Purpose:
;             Handles the movement of the spaceship based on player input.
; Inputs:
;             [BP + 4] - Key input character.
; Behavior:
;             - Moves the spaceship left if 'a' is pressed.
;             - Moves the spaceship right if 'd' is pressed.
; Outputs:
;             - Updates the spaceship's position on the screen.
;			  - redraws the spaceship at the new position.
; Notes:
;             - The spaceship cannot move beyond the screen boundaries.
;--------------------------------------------------------
	push bp                 				; Save the base pointer
	mov bp, sp              				; Set up the stack frame

	mov ax, [bp + 4]       					; Load the key input character

	cmp al, 'a'            					; Check if 'a' (move left) was pressed
	jne @@check_right						; If not, check for right movement

	mov dx, left_boundary					; Load left boundary
	cmp [spaceship_curr_x], dx				; Compare current X position with left boundary
	jbe @@animate							; If at or beyond left boundary, skip movement

	mov dx, spaceship_move_step				; Load movement step
	sub [spaceship_curr_x], dx				; Move spaceship left
	jmp @@animate							; Proceed to animation

@@check_right:
	cmp al, 'd'            					; Check if 'd' (move right) was pressed
	jne @@animate							; If not, skip movement

	mov dx, right_boundary					; Load right boundary
	cmp [spaceship_curr_x], dx				; Compare current X position with right boundary
	jae @@animate							; If at or beyond right boundary, skip movement

	mov dx, spaceship_move_step				; Load movement step
	add [spaceship_curr_x], dx				; Move spaceship right

@@animate:
	push spaceship_height					; Pass spaceship height
	push spaceship_width					; Pass spaceship width
	push offset spaceship_model				; Pass pointer to spaceship model
	push [spaceship_curr_x]					; Pass current X position
	push [spaceship_curr_y]					; Pass current Y position
	call draw_model							; Draw the spaceship model

	pop bp                  				; Restore the base pointer
	ret 2                    				; Clean up the stack and return
endp spaceship_movement_handler



start:
    mov ax, @data 										; Initialize data segment
    mov ds, ax
	
    mov ax, 0013h 										; Set video mode 13h (320x200, 256 colors)
    int 10h												; BIOS interrupt to set video mode

	call difficulty_select								; Call difficulty selection procedure
	call display_guide_page								; Call display guide page procedure

	push 0												; Pass meaningless value to initialize spaceship position
	call spaceship_movement_handler						; Initialize spaceship position

tick_loop:  
    mov ah, 2ch											; Get system time
    int 21h
    cmp [time], dl										; Compare current time with last recorded time
    je flow_control_actions								; If equal, skip to flow control actions

tick_actions:
    mov [time], dl										; Update last recorded time
	inc [ticks_since_last_projectile_registration]		; Increment ticks since last projectile registration
	call projectiles_handler							; Handle projectiles

	call enemies_direction_handler						; Handle enemy movement direction and boundaries
	call enemies_animation_handler						; Animate enemies

flow_control_actions:
	call display_score
	jmp victoycheck
inputloop:
	mov ah, 1
    int 16h 
    jz tick_loop 

    mov ah, 0 
    int 16h
	
	push ax									; Save the input
	push ax									; Pass the input register projectile registration
	push ax									; Pass the input to spaceship movement handler
	call spaceship_movement_handler			; Handle spaceship movement
	call register_projectile				; Register projectile if shooting key pressed
	pop ax									; Restore the input

	cmp al, 'p'
	jne exitoffrange
	jmp exit
exitoffrange:	
	jmp tick_loop 
victoycheck:
	cmp [dead_count], 24
	je win
	cmp [enemy_reached_bottom], 1
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