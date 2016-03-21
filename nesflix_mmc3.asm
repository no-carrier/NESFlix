;       ----------------------------------------------------

;    NESflix - version 0.1
;    Copyright 2010 Don Miller
;    For more information, visit: http://www.no-carrier.com

;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.

;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.

;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.

;       ----------------------------------------------------

buttons EQU $c0

scroll_h EQU $c3
scroll_v EQU $c4

NewButtons = $41
OldButtons = $42
JustPressed = $43

ScreenNumber = $44
OldScreen = $45

up = $49
down = $50
left = $51
right = $52

bank1 = $53

PaletteNumber = $54

ani_counter = $55

bank2 = $56

dir = $57

sleeping EQU $d1 ; nonzero if main thread is waiting for VBlank

cc_toggle = $d2
PalNumber = $d3
PalCounter =$d4
Color0 = $d5
Color1 = $d6
Color2 = $d7
Color3 = $d8

frame = $d9

ani_speed = $da

NewButtons2 EQU $db
OldButtons2 EQU $dc
JustPressed2 EQU $dd

fcount = $de

;       ----------------------------------------------------

        .ORG $7ff0
Header:                         ;16 byte .NES header (iNES)
	.db "NES", $1a		;NES followed by MS-DOS end-of-file
	.db $02			;size of PRG ROM in 16kb units
	.db $20                 ;size of CHR ROM in 8kb units
	.db #%01000000		;flags 6, set to: mapper 4, HORZ mirroring
	.db #%00000000		;flags 7, set to: mapper 4
        .db $00                 ;size of PRG RAM in 8kb RAM
        .db $00                 ;flags 9 -- SET to 0 for NTSC
        .db $00                 ;flags 10, set to 0
        .db $00                 ;11 - the rest are zeroed out
        .db $00                 ;12
        .db $00                 ;13
        .db $00                 ;14
        .db $00                 ;15

;       ----------------------------------------------------

Reset:                          ; reset routine
        SEI
        CLD
	LDX #$00
	STX $2000
	STX $2001
	DEX
	TXS
  	LDX #0
  	TXA
ClearMemory:
	STA 0, X
	STA $100, X
	STA $200, X
	STA $300, X
	STA $400, X
	STA $500, X
	STA $600, X
	STA $700, X
        STA $800, X
        STA $900, X
        INX
	BNE ClearMemory

;       ----------------------------------------------------

        ;Disable APU IRQs
        lda #$40
        sta $4017

        ;Disable DMC IRQs
         lda #$00
         sta $4010

;       ----------------------------------------------------

                                ;MMC3 settings

        lda #%00000110          ;set PRG bank 0
        sta $8000
        lda #%00000000
        sta $8001

        lda #%00000111          ;set PRG bank 1
        sta $8000
        lda #%00000001
        sta $8001

        lda #$01                ;set mirroring to HORZ
        sta $A000

        lda #%00000000
        sta $8000               ;set bank 0
        lda #%00000000
        sta $8001               ;set as 0 2kb chunk

        lda #%00000001
        sta $8000               ;set bank 1
        lda #%00000010
        sta $8001               ;set as 2 2kb chunk

        lda #%00000010
        sta $8000               ;set bank 2
        lda #%00000100
        sta $8001               ;set as 4 2kb chunk

        lda #%00000011
        sta $8000               ;set bank 3
        lda #%00000101
        sta $8001               ;set as 5 2kb chunk

        lda #%00000100
        sta $8000               ;set bank 4
        lda #%00000110
        sta $8001               ;set as 6 2kb chunk

        lda #%00000101
        sta $8000               ;set bank 5
        lda #%00000111
        sta $8001               ;set as 7 2kb chunk

;       ----------------------------------------------------


        lda #$00                ; setting up variables
        sta scroll_h
        sta scroll_v
        
        STA ScreenNumber
        STA PaletteNumber

        sta up
        sta down
        sta left
        sta right
        
        sta bank1
        
        sta frame

        lda #1
        sta dir

        lda #2
        sta bank2

        lda #3
        sta ani_counter
        sta ani_speed

        lda #6                  ; lower this number to increase
        sta PalNumber           ; the speed of the color cycle
        sta PalCounter

        lda #$0F                ; these are the 4 colors
        sta Color0              ; that will cycle when you
        lda #$00                ; hit the "start" button,
        sta Color1              ; will change when you change palettes
        lda #$10
        sta Color2
        lda #$30
        sta Color3

;       ----------------------------------------------------

; set up number of frames

        ;clc
        lda #64 ; total # of frames - max 64
        sta fcount
        ;ASL
        ;ASL
        ;STA frames

;       ----------------------------------------------------

	LDX #$02                ; warm up
WarmUp:
	bit $2002
	bpl WarmUp
	dex
	BNE WarmUp

       	LDA #$3F
	STA $2006
	LDA #$00
	STA $2006
load_pal:                       ; load palette
        LDA palette,x
        sta $2007
        inx
        cpx #$20
        bne load_pal

	LDA #$20
	STA $2006
	LDA #$00
	STA $2006

	ldy #$04                ; clear nametables
ClearName:
	LDX #$00
	LDA #$00
PPULoop:
	STA $2007
	DEX
	BNE PPULoop

	DEY
	BNE ClearName

;       ----------------------------------------------------

        LDA #<pic0              ; load low byte of first picture
        STA $10

        LDA #>pic0              ; load high byte of first picture
        STA $11

;       ----------------------------------------------------

        JSR DrawScreen          ; draw initial nametable
        JSR LoadScreen
        JSR DrawScreen2         ; draw initial nametable
        JSR Vblank              ; turn on screen

;       ----------------------------------------------------

InfLoop:                        ; loop forever
        
        jsr WaitFrame

        dec ani_counter
        bne no_ani
        lda ani_speed
        sta ani_counter
        jsr animate
no_ani:
        JMP InfLoop

;       ----------------------------------------------------

LoadNewPalette:
       	LDX PaletteNumber       ; load palette lookup value
        LDY #$00
        LDA #$3F
	    STA $2006
	    LDA #$00
	    STA $2006
LoadNewPal:                     ; load palette
        LDA palette, x
        STA $2007
        STA $d5, y
        INX
        INY
        CPY #4
        BNE LoadNewPal
        RTS

;       ----------------------------------------------------

DrawScreen:

   	 LDA #$20                ; set to beginning of first nametable
    	STA $2006
    	LDA #$00
    	STA $2006

        LDY #$00
        LDX #$04

NameLoop:                       ; loop to draw entire nametable
        LDA ($10),y
        STA $2007
        INY
        BNE NameLoop
        INC $11
        DEX
        BNE NameLoop

        RTS

;       ----------------------------------------------------

DrawScreen2:

   	LDA #$28                ; set to beginning of first nametable
    	STA $2006
    	LDA #$00
    	STA $2006

        LDY #$00
        LDX #$04

NameLoop2:                       ; loop to draw entire nametable
        LDA ($10),y
        STA $2007
        INY
        BNE NameLoop2
        INC $11
        DEX
        BNE NameLoop2

        RTS

;       ----------------------------------------------------

Vblank:                         ; turn on the screen and start the party
	bit $2002
	bpl Vblank

        ldx scroll_h
        stx $2005
        ldx scroll_v
        stx $2005

	LDA #%10001000
	STA $2000
        LDA #%00001110
	STA $2001

        RTS

;       ----------------------------------------------------

LoadScreen:

        LDA ScreenNumber

Test0:                          ; 4 up
        CMP #0                  ; compare ScreenNumber to find out which picture / palette to load
        BNE Test1
        LDA #<pic0              ; load low byte of picture
        STA $10
        LDA #>pic0              ; load high byte of picture
        STA $11

        lda #0
        sta scroll_h
        sta scroll_v
        sta up
        sta down
        sta left
        sta right

        RTS

Test1:                          ; center
        CMP #1
        BNE Test2
        LDA #<pic1
        STA $10
        LDA #>pic1
        STA $11

        lda #0
        sta scroll_h
        sta scroll_v
        sta up
        sta down
        sta left
        sta right

        RTS

Test2:                          ; UL
        CMP #2
        BNE Test3
        LDA #<pic2
        STA $10
        LDA #>pic2
        STA $11

        lda #64
        sta scroll_h
        lda #56
        sta scroll_v
        lda #0
        sta up
        sta down
        sta left
        sta right

        RTS

Test3:                          ; UR
        CMP #3
        BNE Test4
        LDA #<pic3
        STA $10
        LDA #>pic3
        STA $11

        lda #-64
        sta scroll_h
        lda #56
        sta scroll_v
        lda #0
        sta up
        sta down
        sta left
        sta right

        RTS

Test4:                          ; LL
        CMP #4
        BNE Test5
        LDA #<pic4
        STA $10
        LDA #>pic4
        STA $11

        lda #64
        sta scroll_h
        lda #-72
        sta scroll_v
        lda #0
        sta up
        sta down
        sta left
        sta right

        RTS

Test5:                          ; LR
        ;CMP #5
        ;BNE Test6
        LDA #<pic5
        STA $10
        LDA #>pic5
        STA $11

        lda #-64
        sta scroll_h
        lda #-72
        sta scroll_v
        lda #0
        sta up
        sta down
        sta left
        sta right

        RTS

;       ----------------------------------------------------

controller_test:

        LDA NewButtons
	STA OldButtons
        LDA NewButtons2
	STA OldButtons2

	LDA ScreenNumber        ; save old screen number for later compare
	STA OldScreen

	LDA #$01		; strobe joypad
	STA $4016
	LDA #$00
	STA $4016

; CONTROLLER 1

        LDX #$00
ConLoop:
	LDA $4016		; check the state of each button
	LSR
	ROR NewButtons
        INX
        CPX #$08
        bne ConLoop

	LDA OldButtons          ; invert bits
	EOR #$FF
	AND NewButtons
	STA JustPressed

; CONTROLLER 2

        LDX #$00
ConLoop2:
	LDA $4017		; check the state of each button
	LSR
	ROR NewButtons2
        INX
        CPX #$08
        bne ConLoop2

	LDA OldButtons2          ; invert bits
	EOR #$FF
	AND NewButtons2
	STA JustPressed2


CheckSelect:
	LDA #%00000100
	AND JustPressed
	BEQ CheckStart

        inc ScreenNumber        ; change screens
        lda ScreenNumber
        cmp #6
        bne CheckStart
        lda #0
        sta ScreenNumber

CheckStart:
	LDA #%00001000
	AND JustPressed
	BEQ CheckLeft
	
        lda cc_toggle
        eor #$01
        sta cc_toggle           ; toggles color cycling

CheckLeft:
	LDA #%01000000
	AND JustPressed
	BEQ CheckRight
	
	lda left
	eor #$01
	sta left
	lda #$00
	sta right

CheckRight:
	LDA #%10000000
	AND JustPressed
	BEQ CheckDown

	lda right
	eor #$01
	sta right
	lda #$00
	sta left

CheckDown:
	LDA #%00100000
	AND JustPressed
	BEQ CheckUp

	lda down
	eor #$01
	sta down
	lda #$00
	sta up

CheckUp:
	LDA #%00010000
	AND JustPressed
	BEQ CheckB

	lda up
	eor #$01
	sta up
	lda #$00
	sta down

CheckB:
	LDA #%00000010
	AND JustPressed
	BEQ CheckA

        CLC
        LDA PaletteNumber
        ADC #$10
        CMP #$80
        BNE WritePal
        LDA #$00
WritePal:
        STA PaletteNumber
        JSR LoadNewPalette

CheckA:
	LDA #%00000001
	AND JustPressed
	BEQ CheckDown2

        lda dir       ; toggle direction
        eor #$01
        sta dir

CheckDown2:
	LDA #%00100000
	AND JustPressed2
	BEQ CheckUp2

        dec ani_speed
        lda ani_speed
        bne CheckUp2
        lda #1
        sta ani_speed

CheckUp2:
	LDA #%00010000
	AND JustPressed2
	BEQ EndDrawChk

        inc ani_speed

EndDrawChk:

        LDA ScreenNumber        ; has screen number changed? if not, skip redraw
	CMP OldScreen
	BEQ ConCheckOver

    	LDA #%00000000          ; disable NMI's and screen display
 	STA $2000
   	LDA #%00000000
   	STA $2001

        JSR LoadScreen          ; turn off and load new screen data
        JSR DrawScreen          ; draw new screen
        JSR LoadScreen          ; turn off and load new screen data
        JSR DrawScreen2         ; draw new screen
        JSR Vblank              ; turn the screen back on

ConCheckOver:

        RTS

;       ----------------------------------------------------

animate:

        lda dir
        bne forward_ani

backward_ani:

        dec frame
        lda frame
        cmp #255
        bne keep_ani_back

        clc
        lda fcount
        sbc #1
        sta frame
        asl
        asl
        sta bank1

        lda #%00000000     ; to select first 2kb bank
        sta $8000

        lda bank1	   ; to select bank number
        sta $8001

        clc
        lda bank1
        adc #2

        ldx #%00000001         ; to select second 2kb bank
        stx $8000

        sta $8001

        rts

keep_ani_back:

        dec bank1    ;must inc 2x because of 2kb banks
        dec bank1
        dec bank1
        dec bank1

        lda #%00000000     ; to select first 2kb bank
        sta $8000

        lda bank1	   ; to select bank number
        sta $8001

        clc
        lda bank1
        adc #2

        ldx #%00000001         ; to select second 2kb bank
        stx $8000

        sta $8001

        RTS

;       ----------------------------------------------------

forward_ani:

;change bank 1

        inc frame
        lda frame
        cmp fcount
        bne keep_ani

        lda #0
        sta bank1
        sta frame

        lda #%00000000     ; to select first 2kb bank
        sta $8000

        lda bank1	   ; to select bank number
        sta $8001

        clc
        lda bank1
        adc #2

        ldx #%00000001         ; to select second 2kb bank
        stx $8000

        sta $8001

        rts

keep_ani:
        inc bank1    ;must inc 2x because of 2kb banks
        inc bank1
        inc bank1
        inc bank1

        lda #%00000000     ; to select first 2kb bank
        sta $8000

        lda bank1	   ; to select bank number
        sta $8001

        clc
        lda bank1
        adc #2

        ldx #%00000001         ; to select second 2kb bank
        stx $8000

        sta $8001

        RTS

;       ----------------------------------------------------

; WaitFrame - waits for VBlank, returns after NMI handler is done
WaitFrame:
        inc sleeping
sleep_loop:
        lda sleeping
        bne sleep_loop
        rts
;       ----------------------------------------------------

scroll_test:

CheckUpTog:
        lda up
        beq CheckDownTog
        inc scroll_v
        lda scroll_v
        cmp #240
        bne CheckDownTog
        lda #0
        sta scroll_v
CheckDownTog:
        lda down
        beq CheckLeftTog
        dec scroll_v
        lda scroll_v
        cmp #255
        bne CheckLeftTog
        lda #239
        sta scroll_v
CheckLeftTog:
        lda left
        beq CheckRightTog
        inc scroll_h
CheckRightTog:
        lda right
        beq CheckScrollOver
        dec scroll_h
CheckScrollOver:
        rts

;       ----------------------------------------------------

color_cycle:

	DEC PalCounter	        ; decrement counter, skip if not 0
        BNE NoCycling

	LDA Color3             	; rotate the color values
	LDX Color0
	STA Color0
        LDA Color1
        STX Color1
        LDX Color2
        STA Color2
	STX Color3

	LDA #$3F    	    
	STA $2006           
	LDA #$00      
	STA $2006

    LDA Color0
    STA $2007
	LDA Color1              ; write the rotated palette to the PPU
	STA $2007
	LDA Color2
	STA $2007
	LDA Color3
	STA $2007

	LDA PalNumber
	STA PalCounter

NoCycling:
        RTS

;       ----------------------------------------------------

NMI:
        ;pha ; back up registers
        ;txa
        ;pha
        ;tya
        ;pha

        jsr controller_test
        jsr scroll_test

        LDA cc_toggle
        BEQ no_cc
        JSR color_cycle
no_cc:
        lda scroll_h
        sta $2005
        lda scroll_v
        sta $2005
        
        lda #0             ; clear the sleeping flag so that WaitFrame will exit
        sta sleeping

        ;pla ; restore regs and exit
        ;tay
        ;pla
        ;tax
        ;pla

        RTI
IRQ:
        RTI

;       ----------------------------------------------------

palette:                        ; palette data
  
        .byte $0F,$00,$10,$30,$0F,$05,$26,$30,$0F,$13,$23,$33,$0F,$01,$11,$21 ; palette 0 - b/w
        .byte $0F,$1C,$2B,$39,$0F,$05,$26,$30,$0F,$13,$23,$33,$0F,$1C,$2B,$39 ; palette 1 - blue/greenish
        .byte $06,$16,$26,$36,$0F,$05,$26,$30,$0F,$13,$23,$33,$0F,$1C,$2B,$39 ; palette 2 - red
        .byte $07,$17,$27,$37,$0F,$05,$26,$30,$0F,$13,$23,$33,$0F,$1C,$2B,$39 ; palette 3 - gold
        .byte $0F,$05,$26,$30,$0F,$05,$26,$30,$0F,$05,$26,$30,$0F,$05,$26,$30 ; palette 4 - red/orange/black
        .byte $0F,$13,$25,$31,$0F,$13,$25,$31,$0F,$13,$25,$31,$0F,$13,$25,$31 ; palette 5 - purple/blue
        .byte $0F,$09,$2a,$3B,$0F,$09,$2a,$3B,$0F,$09,$2a,$3B,$0F,$09,$2a,$3B ; palette 6 - green / black
        .byte $19,$29,$14,$27,$19,$29,$14,$27,$19,$29,$14,$27,$19,$29,$14,$27 ; palette 7 - cyberdelic

;       ----------------------------------------------------

pic0:                       ; 4 up
        .INCBIN "order.nam"
pic1:                       ; centered
        .INCBIN "icon.nam"
pic2:                       ; UL
        .INCBIN "icon.nam"
pic3:                       ; UR
        .INCBIN "icon.nam"
pic4:                       ; LL
        .INCBIN "icon.nam"
pic5:                       ; LR
        .INCBIN "icon.nam"

;       ----------------------------------------------------

	.ORG $fffa
	.dw NMI
	.dw Reset
	.dw IRQ

;       ----------------------------------------------------