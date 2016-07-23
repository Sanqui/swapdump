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
    INCBIN "gfx/tiles.2bpp"
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
    
    ld hl, Tiles
    ld de, $9000
    ld bc, TilesEnd-Tiles
    call CopyData_
    
    ld hl, $4000
    ld de, $c000
    ld bc, $1000
    call CopyData_
    
    ld hl, HRAMCode
    ld de, $ff80
    ld bc, EndHRAMCode-HRAMCode
    call CopyData_
    
    ld hl, $0134
    ld de, W_OWNNAME
    ld bc, $10
    call CopyData_
    
    jp StartRAM

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

COPYSECTION "ram code", ROMX[$4000], BANK[$1]

SECTION "ram code",WRAM0[$C000]
StartRAM:
    jp StartRAM_

CopyTilemap:
; Contains an unrolled loop for speed.
    ld de, $9800
    ld hl, W_MAP
    ld c, 3
.row

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
    dec c
    jr nz, .row
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
    call GetTileAddr ; top left corner
    ld a, $10
    ld [hli], a
    ld a, d
    sub b
    dec a
    push bc
    ld b, a
    ld a, $11
    call WriteBTimes
    ld a, $12
    ld [hli], a
    pop bc
    ; top drawn
.mid
    inc c
    ld a, c
    cp e
    jr z, .last
    call GetTileAddr
    ld a, $13
    ld [hli], a
    ld a, d
    sub b
    dec a
    push bc
    ld b, a
    ld a, $14
    call WriteBTimes
    ld a, $15
    ld [hli], a
    pop bc
    jr .mid
    
.last
    call GetTileAddr ; top left corner
    ld a, $16
    ld [hli], a
    ld a, d
    sub b
    dec a
    push bc
    ld b, a
    ld a, $17
    call WriteBTimes
    ld a, $18
    ld [hli], a
    pop bc
    ret
    

WriteString:
.loop
    ld a, [hli]
    cp a, "@"
    jr z, .done
    cp a, $0a
    jr z, .done
    and a
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

CartswapString:
    db "   SWAPDUMP v0.1@"
InstructionsString:
    db "Loaded! Please@"
    db "remove cartridge@"
    db "and put in the one@"
    db "you want to dump.@"
    
    db "Then press START.@"

CorrectString:
    db "A = DUMP@"
    db "B = SWAP@"
    db "SELECT = VIEW@"
    db "@"

ChangeString:
    db "Insert new cart,@"
    db "then press START.@"

DoneDumpingString:
    db "Done dumping!@"
    
    db "Please swap a@"
    db "target cartridge,@"
    db "then press START.@"

    
NotLogDataString:
    db "ERROR: Wrong@"
    db "logdata header@"
    db "(not LOG1).@"
    
ParseErrorString:
    db "ERROR: Cannot@"
    db "parse logdata.@"


NotStartingROMString:
    db "WARNING: ROM is@"
    db "different from@"
    db "the starting ROM.@"
    
FinalChoicesString:
    db "A = WRITE@"
    db "B = SWAP@"
    db "SELECT = VIEW@"
    db "RIGHT = BANKS@"
    
DoneString:
    db "   Data copied@"
    db "    to SRAM!@"
    db "  We should be@"
    db " done here!  :D@"
    
    db "ANY = VIEW@"

ReloadScreen:
    call DisableLCD
    call CopyTilemap
    jp EnableLCD

StartRAM_:
.begin
    call EnableLCD
    call DrawLargeBox
    ld hl, CartswapString
    decoord 0, 0
    call WriteString
    
    ld hl, LogData
    call ReadLogDataHeader
    jp nz, NotLogData
    
    ld hl, InstructionsString
    decoord 6, 1
    call WriteString
    decoord 7, 1
    call WriteString
    decoord 8, 1
    call WriteString
    decoord 9, 1
    call WriteString
    decoord 11, 1
    call WriteString
    call ReloadScreen
  
.waitnew
    call $ff80 ; wait for input
    
.giveoptions
    call DrawBoxWithROMName
    
    ld bc, $0004
    ld de, $1311
    call DrawBox
    
    ld hl, CorrectString
    decoord 5, 1
    call WriteString
    decoord 6, 1
    call WriteString
    decoord 7, 1
    call WriteString
    decoord 8, 1
    call WriteString
    call ReloadScreen
    
    call ReadJoypadRegister
    
.joyloop
    call ReadJoypadRegister
    ld a, [H_JOYNEW]
    cp $1
    jr z, .dump
    cp $2
    jr z, .change
    cp $4
    jr nz, .joyloop
    call Viewer
    jr .giveoptions
.change
    
    call DrawLargeBox
    ld hl, ChangeString
    decoord 7, 1
    call WriteString
    decoord 8, 1
    call WriteString
    call ReloadScreen
    jp .waitnew
.dump
    call ReadBankData
    ld hl, LogData
    call ReadLogDataHeader
    jp nz, NotLogData
    ld a, "O"
    ld [hli], a
    
DoReadsWrites::
    ld a, [hli]
    cp "W"
    jr z, DoWrite
    cp "R"
    jr z, DoRead
    cp ";"
    jr z, .gonewline
    cp $0a
    jr z, DoReadsWrites
    and a
    jp z, DoEnd
    dec hl
    ld a, "?"
    ld [hli], a
.gonewline
    ld a, [hli]
    cp $0a
    jr nz, .gonewline
    jr DoReadsWrites
DoWrite:
    call ReadWhitespace
    call ReadAsciiByte
    ld d, a
    call ReadAsciiByte
    ld e, a
    call ReadWhitespace
    call ReadAsciiByte
    ld [de], a
    call ReadWhitespace
    call ReadComment
    ld a, [hli]
    cp "\n"
    jp nz, ParseError
    jr DoReadsWrites
DoRead:
    call ReadWhitespace
    call ReadAsciiByte
    ld d, a
    call ReadAsciiByte
    ld e, a
    call ReadWhitespace
    ld a, [de]
    call WriteAsciiByte
    call ReadWhitespace
    call ReadComment
    ld a, [hli]
    cp "\n"
    jp nz, ParseError
    jr DoReadsWrites
    
ReadWhitespace:
.loop
    ld a, [hli]
    cp " "
    jr z, .loop
    cp $09 ; tab
    jr z, .loop
.notwhitespace
    dec hl
    ret
    
ReadComment:
.loop
    ld a, [hl]
    cp $0a
    jr z, .end
    ld a, [hli]
    cp ";"
    jp nz, ParseError
.nlloop
    ld a, [hli]
    cp $0a
    jr nz, .nlloop
    dec hl
    ret
.end
    ret

ReadAsciiByte:
    ld a, [hli]
    sub "0"
    cp $a
    jr c, .nota
    sub 7
.nota
    swap a
    ld b, a
    ld a, [hli]
    sub "0"
    cp $a
    jr c, .nota2
    sub 7
.nota2
    or b
    ret


WriteAsciiByte:
    ld b, a
    swap a
    and a, $0f
    cp $a
    jr nc, .letter
    add "0"
    jr .write
.letter
    add "A"-$0a
.write
    ld [hli], a
    
    ld a, b
    and a, $0f
    cp $a
    jr nc, .letter2
    add "0"
    jr .write2
.letter2
    add "A"-$0a
.write2
    ld [hli], a
    ret
    
DrawLargeBox:
    ld bc, $0001
    ld de, $1311
    jp DrawBox
    
NotLogData:
    call DrawLargeBox
    
    ld hl, NotLogDataString
    decoord 6, 1
    call WriteString
    decoord 7, 1
    call WriteString
    decoord 8, 1
    call WriteString
    call ReloadScreen
    halt

ParseError:
    call DrawLargeBox
    
    ld hl, ParseErrorString
    decoord 6, 1
    call WriteString
    decoord 7, 1
    call WriteString
    call ReloadScreen
    halt
    
ReadLogDataHeader:
    ld a, [hli]
    cp "L"
    ret nz
    ld a, [hli]
    cp "O"
    ret nz
    ld a, [hli]
    cp "G"
    ret nz
    ld a, [hli]
    cp "1"
    ret nz
    ld a, [hli]
    cp " "
    ret

DrawBoxWithROMName:
    ld bc, $0001
    ld de, $1303
    call DrawBox
    ld hl, $0134
    ld de, W_TMP_NAME
    ld bc, $10
    call CopyData
    ld a, "@"
    ld [de], a
    ld hl, W_TMP_NAME
    decoord 2, 1
    jp WriteString

DoEnd:
    call DrawLargeBox
    
    ld hl, DoneDumpingString
    decoord 6, 1
    call WriteString
    decoord 8, 1
    call WriteString
    decoord 9, 1
    call WriteString
    decoord 10, 1
    call WriteString
    call ReloadScreen
    call $ff80

.lastchoices
    ld hl, $0134
    ld de, W_TMP_NAME
    ld bc, $10
    call CopyData
    
    call DrawBoxWithROMName
    ld bc, $0004
    ld de, $1311
    call DrawBox
    
    ld c, $10
    ld hl, W_TMP_NAME
    ld de, W_OWNNAME
.verifyloop
    ld a, [hli]
    ld b, a
    ld a, [de]
    inc de
    cp b
    jr nz, .wrong
    dec c
    jr nz, .verifyloop
    jr .right
.wrong
    ld hl, NotStartingROMString
    decoord 10, 1
    call WriteString
    decoord 11, 1
    call WriteString
    decoord 12, 1
    call WriteString

.right
    ld hl, FinalChoicesString
    decoord 5, 1
    call WriteString
    decoord 6, 1
    call WriteString
    decoord 7, 1
    call WriteString
    decoord 8, 1
    call WriteString
    
    call ReloadScreen
    
.joyloop
    call ReadJoypadRegister
    ld a, [H_JOYNEW]
    cp $1
    jr z, .write
    cp $2
    jp z, DoEnd
    cp $10
    jr z, .bankviewer
    cp $4
    jr nz, .joyloop
    call Viewer
    jr .lastchoices
.bankviewer
    call BankViewer
    jr .lastchoices
    
.write
    ld a, $0a
    ld [$0000], a
    xor a
    ld [$4000], a
    ld hl, LogData
    ld de, $a000
    ld bc, LogDataEnd-LogData
    call CopyData
    xor a
    ld [$0000], a
    
    call DrawLargeBox
    ld hl, DoneString
    decoord 6, 1
    call WriteString
    decoord 7, 1
    call WriteString
    decoord 8, 1
    call WriteString
    decoord 9, 1
    call WriteString
    decoord 11, 1
    call WriteString
    
    call ReloadScreen
    
.endloop
    call ReadJoypadRegister
    ld a, [H_JOYNEW]
    and a
    call nz, Viewer
    jr .endloop

Viewer::
    xor a
    ld [H_DISPLAYTOPLINE], a
    ld [H_LASTLINE], a
    ld hl, LogData
    ld a, l
    ld [H_DISPLAYPOS], a
    ld a, h
    ld [H_DISPLAYPOS+1], a
    
.countlines
    ld b, 0
.countloop
    ld a, [hli]
    and a
    jr z, .donecounting
    cp $0a
    jr nz, .countloop
    inc b
    jr .countloop
.donecounting
    ld a, b
    dec a
    ld [H_LASTLINE], a
    
.render
    xor a
    ld [H_DISPLAYLINE], a
    ld bc, $0001
    ld de, $1312
    call DrawBox
    
    ld a, [H_DISPLAYPOS]
    ld l, a
    ld a, [H_DISPLAYPOS+1]
    ld h, a
    
.printloop
    ld bc, 20
    decoord 2, 1
    ld a, [H_DISPLAYLINE]
    call Multiply
    call WriteString
    ld a, [H_DISPLAYLINE]
    inc a
    ld [H_DISPLAYLINE], a
    cp 16
    jr nz, .printloop
    
    ld bc, $0e10
    ld de, $1312
    call DrawBox
    hlcoord $11, $0f
    ld a, [H_DISPLAYTOPLINE]
    call WriteAsciiByte
    ld a, "/"
    ld [hli], a
    ld a, [H_LASTLINE]
    call WriteAsciiByte
    hlcoord $10, $13
    ld a, $11
    ld [hl], a
    
    call ReloadScreen
    call Sleep
    
.joyloop
    call ReadJoypadRegister
    ld a, [H_JOY]
    bit 6, a
    jr nz, .up
    bit 7, a
    jr nz, .down
    bit 1, a
    ret nz
    jr .joyloop
.up
    ld a, [H_DISPLAYTOPLINE]
    and a
    jr z, .joyloop
    dec a
    ld [H_DISPLAYTOPLINE], a
    ld a, [H_DISPLAYPOS]
    ld l, a
    ld a, [H_DISPLAYPOS+1]
    ld h, a
    dec hl
    dec hl
.lastnlloop
    ld a, [hld]
    cp $0a
    jr nz, .lastnlloop
    inc hl
    inc hl
    ld a, l
    ld [H_DISPLAYPOS], a
    ld a, h
    ld [H_DISPLAYPOS+1], a
    jr .render
.down
    ld a, [H_LASTLINE]
    ld b, a
    ld a, [H_DISPLAYTOPLINE]
    cp b
    jr nc, .joyloop
    inc a
    ld [H_DISPLAYTOPLINE], a
    
    ld a, [H_DISPLAYPOS]
    ld l, a
    ld a, [H_DISPLAYPOS+1]
    ld h, a
.nextnlloop
    ld a, [hli]
    cp $0a
    jr nz, .nextnlloop
    ld a, l
    ld [H_DISPLAYPOS], a
    ld a, h
    ld [H_DISPLAYPOS+1], a
    jp .render
    
    halt

BankViewer:
    ld bc, $0001
    ld de, $1312
    call DrawBox
    
    ld de, W_BANKDATA
    ld c, 4*$11
    hlcoord $1, 0
.loopwrite
    ld a, [de]
    inc de
    call WriteAsciiByte
    ld a, [de]
    inc de
    call WriteAsciiByte
    ld a, " "
    ld [hli], a
    dec c
    jr nz, .loopwrite

    call ReloadScreen
    call Sleep
.loop
    call ReadJoypadRegister
    ld a, [H_JOYNEW]
    and a
    jr z, .loop
    ret

Multiply:
    and a
    ret z
    push hl
    push de
    pop hl
.loop
    add hl, bc
    dec a
    jr nz, .loop
    push hl
    pop de
    pop hl
    ret

Sleep:
    call Sleep_
    call Sleep_
Sleep_:
    ld bc, $ffff
.loop
    push af
    pop af
    push af
    pop af
    push af
    pop af
    dec bc
    ld a, b
    and c
    jr nz, .loop
    ret
    
ReadBankData:
    ld hl, W_BANKDATA
    ld b, 0
    
.loop
    ld a, b
    ld [$2000], a
    and $0f
    call z, UpdateScreenBank
    push hl
    call ChecksumBank
    pop hl
    ld a, e
    ld [hli], a
    ld a, d
    ld [hli], a
    inc b
    jr nz, .loop
    ret
    
ChecksumBank:
    ld [H_TMPSP], sp
    ld sp, $4000
    ld hl, $0000
    xor a
.loop
rept 32
    pop de
    add hl, de
endr
    dec a
    jr nz, .loop
    ld e, l
    ld d, h
    ld a, [H_TMPSP]
    ld l, a
    ld a, [H_TMPSP+1]
    ld h, a
    ld sp, hl
    ret
    
UpdateScreenBank:
    push hl
    push de
    push bc
    hlcoord $10, $11
    ld a, b
    call WriteAsciiByte
    call ReloadScreen
    pop bc
    pop de
    pop hl
    ret
    
    db $0a
LogData:
    INCBIN "logdata.txt"
LogDataEnd

EndRAMCode
