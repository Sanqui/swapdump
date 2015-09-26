INCLUDE "constants.asm"

; rst vectors go unused
SECTION "rst00",HOME[0]
    ret ; nonzero value must be here

SECTION "rst08",HOME[8]
    ret

SECTION "rst10",HOME[$10]
    ret

SECTION "rst18",HOME[$18]
    ret

SECTION "rst20",HOME[$20]
    ret

SECTION "rst30",HOME[$30]
    ret

SECTION "rst38",HOME[$38]
    ret

SECTION "vblank",HOME[$40]
    reti
SECTION "lcdc",HOME[$48]
	reti
SECTION "timer",HOME[$50]
	reti
SECTION "serial",HOME[$58]
	reti
SECTION "joypad",HOME[$60]
	reti

SECTION "bank0",HOME[$61]

SECTION "romheader",HOME[$100]
    nop
    jp Start

SECTION "start",HOME[$150]

Tiles:
    INCBIN "tiles.gb"
TilesEnd
    
DisableLCD_: ; $0061
	xor a
	ld [$ff0f],a
	ld a,[$ffff]
	ld b,a
	res 0,a
	ld [$ffff],a
.waitVBlank
	ld a,[$ff44]
	cp a,$91
	jr nz,.waitVBlank
	ld a,[$ff40]
	and a,$7f	; res 7,a
	ld [$ff40],a
	ld a,b
	ld [$ffff],a
	ret


CopyData_:
; copy bc bytes of data from hl to de
	ld a,[hli]
	ld [de],a
	inc de
	dec bc
	ld a,c
	or b
	jr nz,CopyData_
	ret

Start:
    di
    
    ; palettes
    ld a, %11100100
    ld [rBGP], a
    ld [rOBP0], a
    
    ld a, 0
    ld [rSCX], a
    ld [rSCY], a
    
    ld a, %11000001
    ld [rLCDC], a
    
    ei
    
    call DisableLCD_
    
    push af
    
    ld hl, $C000
.loop
    ld a, 0
    ld [hli], a
    ld a, h
    cp $e0
    jr nz, .loop
    
    ld sp, $dffe

    ld hl, $ff80
.loop2
    ld a, 0
    ld [hli], a
    ld a, h
    cp $00
    jr nz, .loop2
    
    ld a, $98
    ld [H_VCOPY_D], a
    ld a, $D0
    ld [H_VCOPY_H], a
    ld a, $8
    ld [H_VCOPY_ROWS], a
    
    ld hl, Tiles
    ld de, $9000
    ld bc, TilesEnd-Tiles
    call CopyData_
    
    ld hl, RAMCode
    ld de, $c000
    ld bc, EndRAMCode-RAMCode
    call CopyData_
    
    ld hl, HRAMCode
    ld de, $ff80
    ld bc, EndHRAMCode-HRAMCode
    call CopyData_

    jpram StartRAM

HRAMCode:
.loop
	ld a,%00010000 ; select button keys
	ld [rJOYP],a
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	xor $ff
	and %00001000
	jr z, .loop
	ret
EndHRAMCode

SECTION "bank2",ROMX,BANK[$1]
RAMCode:

StartRAM:
    jpram StartRAM_

VBlankHandler:
    push af
    push bc
    push de
    push hl
    callram CopyTilemap
    callram ReadJoypadRegister
    ld hl, H_TIMER
    inc [hl]
    ld a, [rDIV]
    ld b, a
    
    ; derp
    ld a, $00
    ld [$FF1A], a
    
    pop hl
    pop de
    pop bc
    pop af
    reti

CopyTilemap: ; We can copy just 8 lines per vblank.
    ld a, [H_VNOCOPY]
    and a
    ret nz
; Contains an unrolled loop for speed.
    ;ld de, $9800
    ;ld hl, W_MAP
    ld hl, H_VCOPY_D
    ld a, [hli]
    ld d, a
    ld a, [hli]
    ld e, a
    ld a, [hli]
    ld c, a
    ld a, [hl]
    ld l, a
    ld h, c
    ld a, [H_VCOPY_ROWS]
    ld c, a
.row

    dec c
    jr z, .done

    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    
    ld a, e
    add $c
    ld e, a
    jr nc, .row
;carry
    inc d
    jr .row
.done
    ld a, [H_VCOPY_TIMES]
    inc a
    ld [H_VCOPY_TIMES], a
    cp a, $03
    jr z, .reset
    cp a, $02
    jr nz, .eightrows
    ; only 5 rows left
    ld a, $5
    ld [H_VCOPY_ROWS], a

.eightrows
    ld a, d
    ld [H_VCOPY_D], a
    ld a, e
    ld [H_VCOPY_E], a
    ld a, h
    ld [H_VCOPY_H], a
    ld a, l
    ld [H_VCOPY_L], a
    ret
.reset
    ld a, $98
    ld [H_VCOPY_D], a
    xor a
    ld [H_VCOPY_E], a
    ld a, $C0
    ld [H_VCOPY_H], a
    xor a
    ld [H_VCOPY_L], a
    ld [H_VCOPY_TIMES], a
    ld a, $8
    ld [H_VCOPY_ROWS], a
    ret

DisableLCD: ; $0061
	xor a
	ld [$ff0f],a
	ld a,[$ffff]
	ld b,a
	res 0,a
	ld [$ffff],a
.waitVBlank
	ld a,[$ff44]
	cp a,$91
	jr nz,.waitVBlank
	ld a,[$ff40]
	and a,$7f	; res 7,a
	ld [$ff40],a
	ld a,b
	ld [$ffff],a
	ret

EnableLCD:
	ld a,[$ff40]
	set 7,a
	ld [$ff40],a
	ret

CopyData:
; copy bc bytes of data from hl to de
	ld a,[hli]
	ld [de],a
	inc de
	dec bc
	ld a,c
	or b
	jr nz,CopyData
	ret

CopyDataFF:
; copy data from hl to de ending with $ff (inclusive)
	ld a,[hli]
	ld [de],a
	inc de
	inc a
	ret z
	jr CopyDataFF

WriteDataInc:
; write data in hl increasing a until b.
.loop
    ld [hli], a
    inc a
    cp a, b
    jr nz, .loop
    ret

WriteBTimes:
; write a in hl b times
.loop
    ld [hli], a
    dec b
    jr nz, .loop
    ret

; copypasta:
; this function directly reads the joypad I/O register
; it reads many times in order to give the joypad a chance to stabilize
; it saves a result in [$fff8] in the following format
; (set bit indicates pressed button)
; bit 0 - A button
; bit 1 - B button
; bit 2 - Select button
; bit 3 - Start button
; bit 4 - Right
; bit 5 - Left
; bit 6 - Up
; bit 7 - Down
ReadJoypadRegister: ; 15F
    ld a, [H_JOY]
    ld [H_JOYOLD], a
	ld a,%00100000 ; select direction keys
	ld c,$00
	ld [rJOYP],a
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	cpl ; complement the result so that a set bit indicates a pressed key
	and a,%00001111
	swap a ; put direction keys in upper nibble
	ld b,a
	ld a,%00010000 ; select button keys
	ld [rJOYP],a
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	cpl ; complement the result so that a set bit indicates a pressed key
	and a,%00001111
	or b ; put button keys in lower nibble
	ld [$fff8],a ; save joypad state
	ld a,%00110000 ; unselect all keys
	ld [rJOYP],a
	
	ld a, [H_JOY]
	ld b, a
	ld a, [H_JOYOLD]
	xor $ff
	and b
	ld [H_JOYNEW], a
	ret

GetTileAddr: ; bc = xy
    push bc
    push de
    inc c
    ld hl, W_MAP
    ld e, $14
.loop
    dec c
    jr z, .end
    ld a, l
    add e
    ld l, a
    jr nc, .loop
    inc h
    jr .loop
.end
    ld a, l
    add b
    ld l, a
    jr nc, .nc
    inc h
.nc
    pop de
    pop bc
    ret

DrawBox: ; draws a box from bc to de
    callram GetTileAddr ; top left corner
    ld a, $10
    ld [hli], a
    ld a, d
    sub b
    dec a
    push bc
    ld b, a
    ld a, $11
    callram WriteBTimes
    ld a, $12
    ld [hli], a
    pop bc
    ; top drawn
.mid
    inc c
    ld a, c
    cp e
    jr z, .last
    callram GetTileAddr
    ld a, $13
    ld [hli], a
    ld a, d
    sub b
    dec a
    push bc
    ld b, a
    ld a, $14
    callram WriteBTimes
    ld a, $15
    ld [hli], a
    pop bc
    jr .mid
    
.last
    callram GetTileAddr ; top left corner
    ld a, $16
    ld [hli], a
    ld a, d
    sub b
    dec a
    push bc
    ld b, a
    ld a, $17
    callram WriteBTimes
    ld a, $18
    ld [hli], a
    pop bc
    ret
    

WriteString:
.loop
    ld a, [hli]
    cp a, "@"
    jr z, .done
    cp a, " "
    jr nz, .nospace
    ld a, 0
.nospace
    ld [de], a
    inc de
    jr .loop
.done
    ret
   
ModuloB:
.loop
    cp a, b
    ret c
    sub a, b
    jr .loop

DivB:
    ld c, 0
.loop
    cp a, b
    jr c, .ret
    sub a, b
    inc c
    jr .loop
.ret
    ld a, c
    ret
    
WriteNumber: ; writes number to de
    push af
    ld b, 10
    callram DivB
    add a, $30
    ld [de], a
    inc de
    pop af
    callram ModuloB
    add a, $30
    ld [de], a
    ret

WriteHexNumber: ; writes hex number to de
    push af
    swap a
    and a, $0f
    cp $a
    jr nc, .ten
    add a, $30
    jr .write1
.ten
    add a, $37
.write1
    ld [de], a
    inc de
    pop af
    and a, $0f
    cp $a
    jr nc, .ten2
    add a, $30
    jr .write2
.ten2
    add a, $37
.write2
    ld [de], a
    inc de
    ret

ClearScreen:
    ld hl, $c000
.loop
    xor a
    ld [hli], a
    ld a, h
    cp $c1
    jr nz, .loop
    ld a, l
    cp $68
    jr nz, .loop
    ret

CartswapString:
    db "   CARTSWAP v0.1@"
InstructionsString:
    db "LOADED. PLEASE@"
    db "REMOVE CART AND@"
    db "PUT IN THE ONE@"
    db "YOU WANT TO DUMP.@"
    
    db "THEN PRESS START.@"

CorrectString:
    db "A=DUMP?@"
    db "B=CHANGE?@"

ChangeString:
    db "INSERT NEW CART@"
    db "THEN PRESS START.@"

ReloadScreen:
    ld a, $98
    ld [H_VCOPY_D], a
    ld a, $D0
    ld [H_VCOPY_H], a
    ld a, $8
    ld [H_VCOPY_ROWS], a
    callram DisableLCD
    callram CopyTilemap
    callram CopyTilemap
    callram CopyTilemap
    jpram EnableLCD

StartRAM_:
.begin
    callram EnableLCD
    ld bc, $0001
    ld de, $1311
    callram DrawBox
    ld hl, CartswapString
    decoord 0, 0
    callram WriteString
    
    ld hl, InstructionsString
    decoord 6, 1
    callram WriteString
    decoord 7, 1
    callram WriteString
    decoord 8, 1
    callram WriteString
    decoord 9, 1
    callram WriteString
    decoord 11, 1
    callram WriteString
    callram ReloadScreen
  
.waitnew
    call $ff80 ; wait for input
    
    ld bc, $0001
    ld de, $1303
    callram DrawBox
    ld bc, $0004
    ld de, $1311
    callram DrawBox
    ld hl, $0134
    ld de, W_TMP_NAME
    ld bc, $10
    callram CopyData
    ld a, "@"
    ld [de], a
    ld hl, W_TMP_NAME
    decoord 2, 1
    callram WriteString
    ld hl, CorrectString
    decoord 5, 1
    callram WriteString
    decoord 6, 1
    callram WriteString
    callram ReloadScreen
    
.joyloop
    callram ReadJoypadRegister
    ld a, [H_JOY]
    cp $1
    jr z, .dump
    cp $2
    jr nz, .joyloop
    
    ld bc, $0001
    ld de, $1311
    callram DrawBox
    ld hl, ChangeString
    decoord 7, 1
    callram WriteString
    decoord 8, 1
    callram WriteString
    callram ReloadScreen
    jp .waitnew
.dump
    
    halt
    
    

EndRAMCode

LogData:
    INCBIN "logdata.txt"
