IDEAL
MODEL small
STACK 100h
DATASEG
;sound data
cn db 1
note1 dw 1436h 
note2 dw 1917h 
note3 dw 2873h 
note4 dw 2559h
timesound db ?
stop db ?
;screens & scores
	score dw 0
	owww db 'Score - '
	ezmsg db 'for easy press 1'
	medmsg db 'for medium press 2'
	hardmsg db 'for hard press 3'
	smsg db 'Press space to start'
	movinfo db 'Press a & d to move'
	shootinfo db 'Press e & q to shoot'
	e1msg db 'Game over, You Lose'
	e2msg db 'Game over, You Win'
	wininfo db 'Score over 2000 to win'
;ship info-----------------
    ship_spawnx dw 130
    ship_spawny dw 180
	difclt dw 10

    x_ship equ 12
    y_ship equ 10
    Spaceship	db 00,00,00,00,00,00,02,00,00,00,00,00,00
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
;Enemies info-----------------
    x_Enemie equ 12
    y_Enemie equ 10
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
				
;firing mode info--------------
    x_mode equ 7
    y_mode equ 10
	ammo_box dw 10 dup (0,0,0) 
	ammo equ 60
	caliber dw ? 
	time db 5
	shot_stopper dw 0
;missile mode info-------------
	Missile		db 00,00,00,03,03,00,00,00
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
;Laser mode info-------------
	Laser		db 00,00,00,00,15,00,00,00
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
				
	delete_shot	db 00,00,00,00,00,00,00,00
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
    call Draw
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


proc Draw
	push bp
	mov bp, sp

    mov ah, 0ch
    mov al,[bp+4]
    mov cx,[bp+6]
    mov dx,[bp+8]
    mov bh,1
    int 10h
	pop bp
	ret 6
endp Draw


proc print
	push bp
	mov bp, sp
	mov si, @data
	mov es,si
	mov al, 1 ;move curser to loc
	mov bh, 0 ;page
	mov bl, 00001111b ;color
	
	mov cx, [bp + 4] ; length
	mov dx, [bp + 6] ;loction
	mov ah, 13h 
	mov bp, [bp + 8] ;msg
	int 10h
	pop bp
	ret 6
endp print

proc sound
	push bp
	mov bp,sp
; open speaker
	in al, 61h
	or al, 00000011b
	out 61h, al
; send control word to change frequency
	mov al, 0B6h
	out 43h, al
; play frequency 131Hz
	mov ax, [bp + 4]
	out 42h, al ; Sending lower byte
	mov al, ah
	out 42h, al ; Sending upper byte
	mov ah,2ch
    int 21h
    mov [timesound],dl
wait1:
	mov ah,2ch
    int 21h
    cmp [timesound],dl
    je wait1

	; close the speaker
	in al, 61h
	and al, 11111100b
	out 61h, al
	pop bp
	ret 2
endp sound
;---------------------------------------------
proc shotinfo
	add si, 2
	mov dx, [ship_spawnx]
	add dx, 2
	mov [si], dx
	add si, 2
	mov dx, [ship_spawny]
	sub dx, 12
	mov [si], dx
	add [caliber], 6
	ret
endp shotinfo


proc lazers
	push y_mode
	push x_mode
	push offset Laser
	add si, 2
	push [si]
	add si, 2
	push [si]
	xor dx,dx
	mov dx, 2
	sub [si], dx
	call DrawModel
	ret
endp lazers


proc missiles
	push y_mode
	push x_mode
	push offset missile
	add si, 2
	push [si]
	add si, 2
	push [si]
	xor dx,dx
	mov dx, 2
	sub [si], dx
	call DrawModel
	ret
endp missiles


proc Deletep
	push y_mode
	push x_mode
	push offset delete_shot
	xor dx,dx
	mov [si], dx
	add si, 2
	push [si]
	add si, 2
	push [si]
	call DrawModel
	ret
endp Deletep
;-------------------------------------------
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
	push y_Enemie
	push x_Enemie
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


proc shot_annimtion
	push bp
	mov bp, sp
	mov ax, offset ammo_box
@@cycle:
	mov si, ax
	push ax
	mov cx, [si]
	
	cmp  cx, 0
	je no_bullet
	
	cmp  cx, 1
	je Use_Laser

	cmp  cx, 2
	je Use_Missile
	
Use_Laser:
	add si, 4
	mov dx, 0
	cmp [si], dx
	jbe miss
	sub si, 4
	call lazers
	jmp no_bullet
Use_Missile:
	add si, 4
	mov dx, 0
	cmp [si], dx
	jbe miss
	sub si, 4
	call missiles
	jmp no_bullet
miss:
	cmp [score], 0
	jbe delete_bullet
	mov cx, [difclt]
	sub [score], cx
delete_bullet:
	sub si, 4
	call Deletep
no_bullet:
	pop ax
	add ax, 6
	mov si, ax
	sub si, offset ammo_box
	cmp si, ammo
	jb @@cycle 
	pop bp
	ret 
endp shot_annimtion


proc checkhit
	push bp
	mov bp, sp
	mov si, offset ammo_box
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
	add dx, x_Enemie
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
	add dx, y_Enemie
	cmp ax, dx
	ja faily
boom:
	inc [deadcount]
	add [score], 100d
	sub si, 4
	push si
	call Deletep
	pop si
	
	sub di, 4
	xor dx,dx
	mov [di], dx
	
	push y_Enemie
	push x_Enemie
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
	sub dx, offset ammo_box
	cmp dx, ammo
	jb jump_shortcut
	mov si, offset ammo_box
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
    mov ax,@data
    mov ds, ax
	
    mov ax, 0013h
    int 10h
;difficulty select 
	push offset ezmsg
	push 409h
	push 16
	call print
	push offset medmsg
	push 809h
	push 18
	call print
	push offset hardmsg
	push 0c09h
	push 16
	call print
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
	mov [difclt], 10
	jmp enddif
med1um:
	mov [difclt], 50
	jmp enddif
hard:
	mov [difclt], 100
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
	push offset smsg
	push 409h
	push 20
	call print
	push offset movinfo
	push 809h
	push 19
	call print
	push offset shootinfo
	push 0c09h
	push 20
	call print
	push offset wininfo
	push 1008h
	push 22
	call print
	
	
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
	push y_ship
	push x_ship
	push offset Spaceship
	push [ship_spawnx]
	push [ship_spawny]
	call DrawModel
	mov [caliber], offset ammo_box
main:
;ammo box check
	mov si, [caliber]
	sub si, offset ammo_box
	cmp si, ammo
	jb shots_annimation
	mov [caliber], offset ammo_box
shots_annimation:  
    mov ah,2ch
    int 21h
    cmp [time],dl
    je noshot
	
	inc [shot_stopper]
	
	call shot_annimtion
	call enemie_annimtion
	call echeck
	call checkhit
	
	mov ah,2ch
    int 21h
    mov [time], dl
noshot:
	push offset owww
	push 0101h
	push 8
	call print
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
	cmp [ship_spawnx], 20
	jbe NotLeft
	sub [ship_spawnx],2 
	push y_ship
	push x_ship
	push offset Spaceship
	push [ship_spawnx]
	push [ship_spawny]
	call DrawModel
	jmp shots_annimation

NotLeft:
	cmp al, 'd'
	jne NotRight
	cmp [ship_spawnx], 280 
	jae NotRight
	add [ship_spawnx],2 
	push y_ship
	push x_ship
	push offset Spaceship
	push [ship_spawnx]
	push [ship_spawny]
	call DrawModel
	jmp shots_annimation
	
NotRight:
	cmp al, 'q'
	jne NotMissile
	mov si,[caliber]
	
	cmp [shot_stopper], 9
	jb NotMissile
	
	mov dx, 2
	mov [si], dx
	call shotinfo
	mov [shot_stopper], 0
	jmp main
NotMissile:
	cmp al, 'e'
	jne NotLaser
	mov si,[caliber]
	
	cmp [shot_stopper], 9
	jb NotLaser	
	
	mov dx, 1
	mov [si], dx
	call shotinfo
	mov [shot_stopper], 0
	jmp	main
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
	push offset e2msg
	push 709h
	push 18
	call print
	jmp outO
lose:
	push offset e1msg
	push 709h
	push 19
	call print
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
	call sound
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