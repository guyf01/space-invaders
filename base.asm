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
	spaceship_hight equ 10
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
    enemy_hight equ 10
	deadcount dw 0
	limit equ 144
	side dw 1
	rn dw 0
	enemies_num dw 1,40,20
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

	Enemie			db 00,00,00,00,00,00,00,00,00,00,00,00,00
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
    projectile_hight equ 10
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
proc DrawModel
	push bp
	mov bp,sp
	
	mov ax, [bp + 4]
	mov dx, [bp + 6]
	mov si, [bp + 8]
Again:
	push ax
    push dx
	push ax
    push dx
    push [si]
    call draw_pixel
	pop dx
	pop ax

    inc si
    inc dx
    mov cx, dx
    sub cx, [bp + 6]
    cmp cx, [bp + 10]
    jbe  Again
    mov dx,[bp + 6]
    inc ax
    mov cx, ax
    sub cx, [bp + 4]
    cmp cx, [bp + 12]
    jbe  Again
	pop bp
	ret 10
endp DrawModel


proc draw_pixel
;--------------------------------------------------------
; Purpose:    Draw a pixel on the screen at a specified location.
; Inputs:     [BP+4] - Color of the pixel (passed via stack)
;             [BP+6] - X position (passed via stack)
;             [BP+8] - Y position (passed via stack)
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


proc print_string
;--------------------------------------------------------
; Purpose:    Print a string on the screen at a specified location.
; Inputs:     [BP+4] - Length of the string (word, passed via stack).
;             [BP+6] - Screen location (word, row and column packed into a single word, passed via stack).
;             [BP+8] - Pointer to the string in the data segment to print (word, passed via stack).
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
;             [BP+4] - Frequency of the note in Hz (word, passed via stack).
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
    jb end_projectile_registration ; Exit if not enough time has passed

    ; Load the next available slot in the active_projectiles array
    mov si, [active_projectiles_next_slot]
    sub si, offset active_projectiles
    cmp si, max_active_projectiles
    jb within_bounds
    ; Reset to the start of the array if the end is reached
    mov [active_projectiles_next_slot], offset active_projectiles
within_bounds:

    mov si, [active_projectiles_next_slot]

    ; Store the projectile ID in the array
    mov dx, [bp + 4]           ; Get the projectile ID from the stack
    mov [si], dx               ; Store the projectile ID in the array

    ; Store the X position of the projectile
    mov dx, [spaceship_curr_x] ; Get the spaceship's current X position
    add dx, 2                  ; Adjust the X position for the projectile
    mov [si + 2], dx               ; Store the adjusted X position in the array

    ; Store the Y position of the projectile
    mov dx, [spaceship_curr_y] ; Get the spaceship's current Y position
    sub dx, 12                 ; Adjust the Y position for the projectile
    mov [si + 4], dx               ; Store the adjusted Y position in the array

    ; Update the next available slot pointer
    add [active_projectiles_next_slot], 6

    ; Reset the ticks counter
    mov [ticks_since_last_projectile_registration], 0

end_projectile_registration:
    pop bp
    ret 2                      ; Clean up the stack and return
endp register_projectile


proc animate_projectile
;--------------------------------------------------------
; Purpose:    
;             Animates a single projectile by updating its position on the screen.
; Inputs:     
;             [BP + 4] - Pointer to the projectile's model (used for rendering).
;             [BP + 6] - Pointer to the projectile's data in the `active_projectiles` array.
; Behavior:   
;             - Retrieves the projectile's current X and Y positions from the `active_projectiles` array.
;             - Updates the projectile's Y position to move it upward on the screen by a fixed step.
;             - Calls `DrawModel` to redraw the projectile at its new position.
; Outputs:    
;             - Updates the projectile's position in the `active_projectiles` array.
;             - Redraws the projectile at its new position on the screen.
; Notes:
;             - The movement step is defined by the `projectile_tick_movement` variable.
;             - This procedure assumes that the projectile's data structure is organized as:
;               [ID, X position, Y position].
;--------------------------------------------------------
    push bp                ; Save the base pointer
    mov bp, sp             ; Set up the stack frame

    mov si, [bp + 6]       ; Load the pointer to the projectile's data
    mov dx, projectile_tick_movement ; Load the movement step
    sub [si + 4], dx           ; Update the Y position (move upward)

    ; Push projectile dimensions and model to the stack
    push projectile_hight  ; Height of the projectile
    push projectile_width  ; Width of the projectile
    push [bp + 4]          ; Pointer to the projectile's model
    push [si + 2]          ; Push the X position onto the stack
    push [si + 4]          ; Push the updated Y position onto the stack

    call DrawModel         ; Call the procedure to redraw the projectile

    pop bp                 ; Restore the base pointer
    ret 4                  ; Clean up the stack and return
endp animate_projectile


proc animate_projectiles
;--------------------------------------------------------
; Purpose:    
;             Animates all active projectiles by iterating through the `active_projectiles` array.
; Inputs:     
;             None (uses global variables and constants).
; Behavior:   
;             - Loops through the `active_projectiles` array.
;             - Checks the projectile ID to determine its type:
;                 - `delete_projectile_id`: Marks the projectile as inactive and animates its deletion.
;                 - `laser_id`: Animates the laser projectile.
;                 - `missile_id`: Animates the missile projectile.
;             - Skips empty slots or inactive projectiles.
; Outputs:    
;             - Updates the positions of all active projectiles.
;             - Redraws each projectile at its new position.
; Notes:
;             - The `active_projectiles` array is organized as a list of projectiles, 
;               where each projectile has the following structure:
;               [ID, X position, Y position].
;--------------------------------------------------------
    mov si, offset active_projectiles ; Start of the `active_projectiles` array

projectile_loop:
    push si                           ; Save the current pointer

    mov cx, [si]                      ; Load the projectile ID
    cmp cx, [delete_projectile_id]    ; Check if the projectile is marked for deletion
    je delete_projectile

    cmp cx, [laser_id]                ; Check if the projectile is a laser
    je projectile_laser

    cmp cx, [missile_id]              ; Check if the projectile is a missile
    je projectile_missile

    jmp next_projectile               ; Skip to the next projectile if no match

delete_projectile:
    mov ax, [no_projectile_id]        ; Mark the projectile as inactive
    mov [si], ax
    push si                           ; Push the projectile data pointer
    push offset delete_projectile_model ; Push the model for deletion
    call animate_projectile           ; Animate the projectile
    jmp next_projectile               ; Move to the next projectile

projectile_laser:
    push si                           ; Push the projectile data pointer
    push offset laser_model           ; Push the laser model
    call animate_projectile           ; Animate the laser
    jmp next_projectile               ; Move to the next projectile

projectile_missile:
    push si                           ; Push the projectile data pointer
    push offset missile_model         ; Push the missile model
    call animate_projectile           ; Animate the missile
    jmp next_projectile               ; Move to the next projectile

next_projectile:
    pop si                            ; Restore the current pointer
    add si, 6                         ; Move to the next projectile slot
    mov ax, si
    sub ax, offset active_projectiles ; Calculate the offset from the start of the array
    cmp ax, max_active_projectiles    ; Check if we've reached the end of the array
    jb projectile_loop                ; Continue looping if not at the end

    ret                               ; Return when all projectiles are processed
endp animate_projectiles


proc projectile_hit_check
    mov si, offset active_projectiles ; Start of the `active_projectiles` array

projectile_loop2:
    push si                            ; Save the current pointer

    mov cx, [si]
    cmp cx, [no_projectile_id]
	je next_projectile2

	cmp cx, [delete_projectile_id]
	je next_projectile2

    cmp cx, [laser_id]
    je ceiling_hit_check

    cmp cx, [missile_id]
    je ceiling_hit_check

	jmp next_projectile2

ceiling_hit_check:
	mov dx, 0
	cmp [si + 4], dx
	ja next_projectile2
	cmp [score], 0
	jbe next_projectile2
	mov dx, [missed_shot_score_penalty]
	sub [score], dx
	mov dx, [delete_projectile_id]
	mov [si], dx
	jmp next_projectile2

next_projectile2:
    pop si                            ; Restore the current pointer
    add si, 6                         ; Move to the next projectile slot
    mov ax, si
    sub ax, offset active_projectiles ; Calculate the offset from the start of the array
    cmp ax, max_active_projectiles    ; Check if we've reached the end of the array
    jb projectile_loop2                ; Continue looping if not at the end

    ret    
endp projectile_hit_check


proc core
	push bp
	mov bp, sp
	mov ax, [bp + 4] 
	xor cx, cx
	remaking: 
		xor dx,dx 
        cmp ax,0 
        je zerocheck       
        mov bx,10         
        div bx                   
        push dx               
        inc cx               
        jmp remaking 
	zerocheck:
		cmp cx, 4
		jae printing
		push dx
		add cx, 1
		jmp zerocheck
	printing:
        cmp cx,0 
        je finished
        pop dx 
        add dx,48 
        mov ah,02h 
        int 21h 
        dec cx 
        jmp printing
	finished:
	pop bp
	ret 2
endp core


proc echeck ;checks if the enemy touched the corners
	push bp
	mov bp, sp
	mov ax, offset enemies_num
@@cycle:
	mov si, ax
	add si, 2
	
	xor dx,dx
	mov dx, 20
	cmp [si], dx
	ja noright
	mov [side], 1
	mov [rn], 1
	jmp hell
noright:
	mov dx, 280
	cmp [si], dx
	jb ex
	mov [side], 0
	mov [rn], 1
	jmp hell
ex:
	add si, 2
	mov dx, 170
	cmp [si], dx
	jb notlost
	mov [deadcount], 30
notlost:	
	add ax, 6
	mov si, ax
	sub si, offset enemies_num
	cmp si, limit
	jb @@cycle 
	mov [rn], 0
hell:
	pop bp
	ret 
endp echeck

proc enemie_annimtion
	push bp
	mov bp, sp
	mov ax, offset enemies_num
@@cycle:
	mov si, ax
	mov cx, [si]
	cmp  cx, 0
	je enemie_dead
	push ax
	push enemy_hight
	push enemy_width
	push offset Enemie
	add si,2
	cmp [side], 1
	jne RightSide
	mov dx, 1
	add [si], dx
	jmp rests
RightSide:
	mov dx, 1
	sub [si], dx
rests:
	push [si]
	add si,2
	
	cmp [rn], 0
	je NoYMovement
	mov dx, 3
	add [si], dx
NoYMovement:
	push [si]
	call DrawModel
	pop ax
enemie_dead:
	add ax, 6
	mov si, ax
	sub si, offset enemies_num
	cmp si, limit
	jb @@cycle 
	pop bp
	ret 
endp enemie_annimtion


proc checkhit
	push bp
	mov bp, sp
	mov si, offset active_projectiles
	mov di, offset enemies_num
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
	add dx, enemy_hight
	cmp ax, dx
	ja faily
boom:
	inc [deadcount]
	add [score], 100d
	sub si, 4
	mov dx, [delete_projectile_id]
	mov [si], dx
	
	sub di, 4
	xor dx,dx
	mov [di], dx
	
	push enemy_hight
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
	sub dx, offset enemies_num
	cmp dx, limit
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
	push spaceship_hight
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
	
	call projectile_hit_check
	call animate_projectiles
	call enemie_annimtion
	call echeck
	call checkhit
	
	mov ah,2ch
    int 21h
    mov [time], dl
noshot:
	push offset score_msg
	push 0101h
	push 8
	call print_string
	push [score]
	call core
	
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
	push spaceship_hight
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
	push spaceship_hight
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
	cmp [deadcount], 24
	je win
	cmp [deadcount], 30
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