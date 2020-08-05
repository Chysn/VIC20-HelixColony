;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                         Helix Colony
;                    (c)2020, Jason Justian
;                  
; Assembled with XA
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This software is released under the Creative Commons
; Attribution-NonCommercial 4.0 International
; License. The license should be included with this file.
; If not, please see: 
;
; https://creativecommons.org/licenses/by-nc/4.0/legalcode.txt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BASIC LAUNCHER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This is the tokenization of the following BASIC program, which
; runs this game
;     42 SYS4110
* = $1001
BASIC:      .byte $0b,$04,$2a,$00,$9e,$34,$31,$31
            .byte $30,$00,$00,$00,$00
         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LABEL DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Starting Constants
ST_ENERGY   = 300               ; Starting energy
COLONISTS   = 10                ; Starting colonists
GEISERS     = 36                ; Number of geisers

; Scoring Constants
LENGTH      = 300               ; Days (seconds) in a game
REMAIN      = 25                ; Remaining colonist - 25 energy
GOOD_MINE   = 50                ; Mine built on geiser - 50 energy
BAD_MINE    = 50                ; Mine not on geiser - -50 energy

; Constants - Game Configuration
SCRCOM      = $08               ; Screen color
TXTCOL      = $01               ; Text color
NORTH       = $01               ; Directional constants
EAST        = $02               ; ,,
SOUTH       = $03               ; ,,
WEST        = $04               ; ,,
FIRE        = $05               ; Joystick fire button pressed
PL_SPEED    = $02               ; Player speed (delay per pixel, in jiffies)

; Character constants
CH_PLAYER   = $21               ; Player character code
CO_PLAYER   = $07               ; Player color
CH_LANDED   = $2c               ; Landed ship (starting point)
CH_GEISER   = $2b               ; Geiser
CO_GEISER   = $00               ; Geiser color
CO_GEISER_V = $09               ; Visible Geiser
CH_SENSOR   = $26               ; Sensor signal level
CO_SENSOR   = $04               ; Level color
CH_BORDER   = $1f               ; Border
CO_BORDER   = $06               ; Border color
CH_LSENSOR  = $3b               ; Large sensor signal
CHAR_S      = $22               ; Source bitmap character code
CHAR_D      = $23               ; Destination bitmap character code

; System Resources
CINV        = $0314             ; ISR vector
;NMINV       = $0318             ; NMI vector
NMINV       = $fffe             ; Development NMI non-vector
SCREEN      = $1e00             ; Screen character memory (unexpanded)
COLOR       = $9600             ; Screen color memory (unexpanded)
IRQ         = $eabf             ; System ISR   
BASRND      = $e094             ; Routine for BASIC's RND() function
RNDNUM      = $8d               ; Result storage location for RND()
VICCR5      = $9005             ; Character map register
VOICEH      = $900c             ; High sound register
VOICEM      = $900b             ; Mid sound register
VOICEL      = $900a             ; Low sound register (unused)
NOISE       = $900d             ; Noise register
VOLUME      = $900e             ; Sound volume register/aux color
BACKGD      = $900f             ; Background color
VIA1DD      = $9113             ; Data direction register for joystick
VIA1PA      = $9111             ; Joystick port (up, down, left, fire)
VIA2DD      = $9122             ; Data direction register for joystick
VIA2PB      = $9120             ; Joystick port (for right)
CLSR        = $e55f             ; Clear screen/home
HOME        = $e581             ; Home text
TCOLOR      = $0286             ; Text color
PRTSTR      = $cb1e             ; Print from data (Y,A)
CASECT      = $0291             ; Disable Commodore case
PRTFIX      = $ddcd             ; Decimal display routine
CHROUT      = $ffd2             ; Output one character
TIME_L      = $a2               ; Jiffy counter low
TIME_M      = $a1               ; Jiffy counter middle  

; Game Memory - Zeropage
SCRPAD      = $f9               ; Temporary scratchpad
CURSOR      = $fa               ; Cursor (2 bytes)
PLAYER      = $fc               ; Player location (2 bytes)

; Game Data
HI_SCORE    = $033c             ; High score (2 bytes)
ENERGY      = $033e             ; Energy (score, 2 bytes)
DIR         = $0340             ; Current travel direction
UNDER       = $0341             ; Character under player
UNDER_COL   = $0342             ; Color under player
JOYDIR      = $0343             ; Joystick direction capture
SENSOR_CT   = $0344             ; Local sensor count
BITMAP_S    = $1d10
BITMAP_D    = $1d18

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This is temporary, and will be replaced once the program code gets
; big enough to push the character set to $1c00 by itself
tmpchar:    ldy #$00
-loop:      lda CharSet,y
            sta $1c00,y
            lda CharSet+$0100,y
            sta $1d00,y
            iny
            bne loop
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAIN PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Startup:    jsr SetupHW         ; Set up hardware           
            lda #$00            ; Initialize high score
            sta HI_SCORE        ; ,,
            sta HI_SCORE+1      ; ,,
           
; Welcome screen and manual         
Welcome:    lda #<Intro
            ldy #>Intro
            jsr PRTSTR
            jsr Wait4Fire
            lda #<Manual
            ldy #>Manual
            jsr PRTSTR

; Start a new game            
Start:      jsr Wait4Fire
            jsr InitGame

; Main loop            
Main:       jsr Joystick
            lda JOYDIR
            beq Main
            cmp #FIRE
            beq Main
            jsr Move
            jmp Main
            
 ; Custom ISR for music player and frame countdown
ISR:        jmp IRQ           
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GAME MECHANICS ROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;            
NewShip:    lda #" "            ; Initialize character under player
            sta UNDER           ; ,,
            lda #$e9            ; Initialize player loction
            sta CURSOR          ; ,,
            lda #$1e            ; ,,
            sta CURSOR+1        ; ,,
            jsr UDPlayer        ; ,,
            lda #CH_LANDED
            ldy #CO_PLAYER
            jsr DrawChar
            jmp StatusBar
           
StatusBar:  lda #$e4
            sta CURSOR
            lda #$1f
            sta CURSOR+1
            ldy #$15
            lda #" "            
-loop:      sta (CURSOR),y
            dey
            bpl loop
            ldy #$00
            lda #CH_LSENSOR
            sta SCRPAD
-loop:      cpy SENSOR_CT
            beq col_disp
            iny
            lda SCRPAD
            sta (CURSOR),y
            inc SCRPAD
            jmp loop
col_disp:   rts
            
            
            
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Wait for Fire
Wait4Fire:  jsr Joystick        ; Wait for fire to be released
            cpx #FIRE           ; ,,
            beq Wait4Fire       ; ,,
wait_fire:  jsr Joystick        ; Wait for the fire button
            cpx #FIRE           ; ,,
            bne wait_fire       ; ,,
            rts
            
; Read the Joystick
; Return the direction byte in A. If the joystick has not
; been moved this frame, also store the direction in the
; JOYDIR register.
Joystick:   lda VIA1PA          ; Read VIA1 port
            and #$3c            ; Keep track of bits 2,3,4,5
            sta SCRPAD
            lda VIA2PB          ; Combine with read of bit 7
            and #$80            ;   from VIA2-B
            ora SCRPAD
            eor #$bc            ; Flip each joystick bit in the
                                ;   combined read byte, so that
                                ;   on = 1
            sta SCRPAD          ; Store temporary direction
            ldx #$05            ; Check five directions (5=fire)
-loop:      lda JoyTable,x      ; Is the joystick pointed in the
            bit SCRPAD            ;   direction indexed by X?
            bne found_dir       ; If so, set that as the joystick direction
            dex                 ; Loop back until found, or 0
            bne loop            ; ,,
found_dir:  stx JOYDIR          ; 0 means no direction
            rts         

; Delay A Jiffies
Delay:      clc
            adc TIME_L
-loop:      cmp TIME_L
            bne loop
            rts     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MOVEMENT ROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;            
; Move character
Move:       sta DIR             ; Set the direction
            ldy #$07            ; Initialize the source and destination
-loop:      lda #$00            ;   characters in the character set
            sta BITMAP_D,y      ;   ,,
            lda SRC_CHAR,y      ;   ,,
            sta BITMAP_S,y      ;   ,,
            dey                 ;   ,,
            bpl loop            ;   ,,
            jsr RSCursor
            lda #CHAR_S         ; Draw the bitmap source character at the
            ldy #CO_PLAYER      ;   player location
            jsr DrawChar        ;   ,,
            ldx DIR             ; Move the cursor
            jsr MoveCursor      ; ,,
            ldx #$00            ; Get the character at the Cursor location and
            lda (CURSOR,x)      ;   put it on the stack for later use as the
            cmp #CH_BORDER+1    ;   UNDER character
            bcs do_move         ; If there's a border, do not move
            rts                 ; ,,
do_move:    pha
            jsr GetColor        ; Get the color at the Cursor location and
            pha                 ;   put it on the stack, too
            lda #CHAR_D         ; Draw the bitmap destination character at the
            ldy #CO_PLAYER      ;   destination location with player color
            jsr DrawChar        ;   ,,
            ldx #$08            ; Move the character 8 pixels in the selected
-loop:      jsr MoveBitmap      ;   direction, with a short delay between each
            lda #PL_SPEED       ;   movement
            jsr Delay           ;   ,,
            dex                 ;   ,,
            bne loop            ;   ,,
            lda UNDER           ; Get the character that was under the player
            ldx #$00            ;   and replace it
            sta (PLAYER,x)      ;   ,,
            lda PLAYER+1        ;   ,,
            pha                 ;   ,,
            clc                 ;   ,,
            adc #$78            ;   ,,
            sta PLAYER+1        ;   ,,
            lda UNDER_COL       ;   ,,
            sta (PLAYER,x)      ;   ,,
            pla                 ;   ,,
            sta PLAYER+1        ;   ,,
            pla                 ; Get the color that was under the destination
            sta UNDER_COL        ;   off the stack and save it
            pla                 ; Get the character that WAS under the
            sta UNDER           ;   destination off the stack and save it
            jsr UDPlayer        ; Update player location
            lda #CH_PLAYER      ; Draw player character at the CURSOR location
            ldy #CO_PLAYER      ; ,,
            jsr DrawChar        ; ,,
            ; Fall through to Sensor

; Activate Sensor            
Sensor:     ldx #$00
            stx SENSOR_CT
            lda UNDER
            cmp #CH_GEISER
            beq Dead
            ldy #$00
-loop:      tya
            pha
            lda SearchPatt,y
            tax
            jsr MoveCursor
            ldx #$00
            lda (CURSOR,x)
            cmp #CH_GEISER
            bne no_geiser
            inc SENSOR_CT
no_geiser:  pla
            tay
            iny
            cpy #$08
            bne loop
            lda SENSOR_CT
            beq sensor_r
            cmp #$05
            bcc add_sensor
            lda #$04
add_sensor: sta SENSOR_CT
            clc
            adc #CH_SENSOR-1
            sta UNDER
            lda #CO_SENSOR
            sta UNDER_COL
sensor_r:   jmp StatusBar

; Player hit Geiser 
; Move player back to starting point           
Dead:       lda #CH_GEISER
            ldy #CO_GEISER_V
            jsr DrawChar
            jmp NewShip

; Move Bitmap
MoveBitmap: lda DIR
            cmp #NORTH
            beq mv_north
            cmp #EAST
            beq mv_east
            cmp #SOUTH
            beq mv_south
            ; Fall through to mv_west
mv_west:    ldy #$07            ; For each byte
-loop:      lda BITMAP_D,y      ; Shift the destination character left
            asl                 ; ,,
            sta BITMAP_D,y      ; ,,
            lda BITMAP_S,y      ; Shift the source character left
            asl                 ; ,,
            sta BITMAP_S,y      ; ,,
            lda #$00            ; If the high bit of the source byte was 1,
            adc BITMAP_D,y      ;   then add 1 to the destination byte
            sta BITMAP_D,y      ;   and save it
            dey
            bpl loop
            rts
mv_east:    ldy #$07            ; For each byte
-loop:      lda BITMAP_D,y      ; Shift the destination character right
            lsr                 ; ,,
            sta BITMAP_D,y      ; ,,
            lda BITMAP_S,y      ; Shift the source character right
            lsr                 ; ,,
            sta BITMAP_S,y      ; ,,
            bcc east_next       ; 
            lda BITMAP_D,y      ; If the low bit of the source byte was 1,
            ora #$80            ;   then set the high bit of the destination
            sta BITMAP_D,y      ;   and save it
east_next:  dey
            bpl loop
            rts
mv_north:   ldy #$00
-loop:      lda BITMAP_D+1,y    ; Copy each byte from the byte below it
            sta BITMAP_D,y      ; ,,
            iny
            cpy #$07
            bne loop
            lda BITMAP_S        ; Move the top byte of the source character to
            sta BITMAP_D+7      ;   the bottom of the destination character
            ldy #$00
-loop:      lda BITMAP_S+1,y
            sta BITMAP_S,y
            iny
            cpy #$07
            bne loop
            lda #$00            ; Clear the bottom byte of the source character
            sta BITMAP_S+7      ; ,,
            rts
mv_south:   ldy #$06            ; Copy each byte from the byte above it
-loop:      lda BITMAP_D,y      ; ,,
            sta BITMAP_D+1,y    ; ,,
            dey
            bpl loop
            lda BITMAP_S+7      ; Copy the bottom of the source to the top of
            sta BITMAP_D        ;   the destination
            ldy #$06
-loop:      lda BITMAP_S,y
            sta BITMAP_S+1,y
            dey
            bpl loop                    
            lda #$00            ; Clear the top byte of the source character
            sta BITMAP_S        ; ,,               
            rts                   

; Reset Cursor to Player
RSCursor:   lda PLAYER
            sta CURSOR
            lda PLAYER+1
            sta CURSOR+1
            rts

; Update Player to Cursor
UDPlayer:   lda CURSOR
            sta PLAYER
            lda CURSOR+1
            sta PLAYER+1
            rts

; Get Color at Cursor
; Return color code in A
GetColor:   lda CURSOR+1
            pha
            clc
            adc #$78            ; Color location is $7800 more than screen
            sta CURSOR+1
            ldx #$00
            lda (CURSOR,x)
            tax
            pla                 ; Reset CURSOR to screen pointer
            sta CURSOR+1        ; ,,
            txa
            rts
            
; Set Color at Cursor
; Use color code in Y
SetColor:   lda CURSOR+1
            pha
            clc
            adc #$78
            sta CURSOR+1
            ldx #$00
            tya
            sta (CURSOR,x)
            pla
            sta CURSOR+1
            rts
  
; Draw Character at Cursor
; A is the character, Y is the color           
DrawChar:   ldx #$00
            sta (CURSOR,x)
            jmp SetColor
            
; Move Cursor
; In the direction specified by X
MoveCursor: lda DirTable,x
            pha
            and #%10000000      ; Extend the sign to 16 bits
            beq sign            ; ,,
            ora #$ff            ; ,,
sign:       tay                 ; ,,
            pla                 ; Get original direction value
            clc                 ; Add (or subtract) the signed direction
            adc CURSOR          ;   and update CURSOR
            sta CURSOR          ;   ,,
            tya                 ;   ,,
            adc CURSOR+1        ;   ,,
            sta CURSOR+1        ;   ,,
            rts            
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SETUP ROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Setup Hardware
SetupHW:    lda TIME_L          ; Seed random number generator
            sta RNDNUM          ; ,,
            lda #SCRCOM         ; Set background color
            sta BACKGD          ; ,,
            lda #$ff            ; Set custom character location
            sta VICCR5          ; ,,
            lda #$4f            ; Set volume and aux color
            sta VOLUME          ; ,,
            lda #$00            ; Initialize sound registers
            sta VOICEM          ; ,,
            sta VOICEH          ; ,,
            sta NOISE           ; ,,
            lda #$7f            ; Set DDR to read East
            sta VIA2DD          ; ,,
            lda #$80            ; Disabled Commodore-Shift
            sta CASECT          ; ,,
            lda #TXTCOL         ; Set color of screen text, like
            sta TCOLOR          ;   intro, game over, score, etc.
            sei                 ; Install the custom ISR
            lda #<ISR           ; ,,
            sta CINV            ; ,,
            lda #>ISR           ; ,,
            sta CINV+1          ; ,,
            lda #<Welcome       ; Install the custom NMI (restart)
            sta NMINV           ; ,, 
            lda #>Welcome       ; ,,
            sta NMINV+1         ; ,,
            cli
            rts
            
; Initialize Game
InitGame:   jsr CLSR
            lda #<ST_ENERGY     ; Initialize Energy to starting value
            sta ENERGY          ; ,,
            lda #>ST_ENERGY     ; ,,
            sta ENERGY+1        ; ,,
            ldy #GEISERS/2      ; Place geisers
-loop:      jsr NewGeiser       ; ,,
            dey                 ; ,,
            bne loop            ; ,,
            lda #<DrawHab       ; Draw Hab
            ldy #>DrawHab       ; ,,
            jsr PRTSTR          ; ,,
            jsr NewShip         ; Place ship
            lda #<SCREEN
            sta CURSOR
            lda #>SCREEN
            sta CURSOR+1
            lda #SOUTH
            jsr MoveCursor
            ldy #$00
-loop:      tya
            pha
            lda BorderPatt,y
            beq bord_done
            tax
            jsr MoveCursor
            lda #CH_BORDER
            ldy #CO_BORDER
            jsr DrawChar
            lda #$01
            jsr Delay
            pla
            tay
            iny
            bne loop
bord_done:  pla
            jmp Sensor          ; Check sensor

; Place Geiser Pair            
NewGeiser:  tya
            pha
            lda #>SCREEN
            sta CURSOR+1
            jsr BASRND
            lda RNDNUM
            sta CURSOR
            lda #CH_GEISER
            ldy #CO_GEISER
            jsr DrawChar
            inc CURSOR+1
            jsr BASRND
            lda RNDNUM
            sta CURSOR
            lda #CH_GEISER
            ldy #CO_GEISER
            jsr DrawChar  
            pla
            tay
            rts          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Intro:      .asc $93,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11
            .asc $1c,"     H",$1e,"ELIX",$1c,"C",$1e,"OLONY",$0d
            .asc $1e,"     J",$1c,"ASON",$1e,"J",$1c,"USTIAN",$0d
            .asc $1c,"     P",$1e,"RESS",$1c,"F",$1e,"IRE"
            ; Fall through to DrawHab

DrawHab:    .asc $13,$11,$11,$11,$11,$11,$11,$11,$11,$11,"          "
            .asc $1e,$5b,$11,$9d,$5c,$11,$9d,$9d,$5d," ",$5e,$00
            
Manual:     .asc $93,"THE YEAR IS 1999",$0d,$0d
            .asc "EARTH HAS ENTRUSTED",$0d,$0d
            .asc "YOU TO ESTABLISH THE",$0d,$0d
            .asc "HELIX COLONY IN THE",$0d,$0d
            .asc "DENEB SYSTEM",$0d,$0d
            .asc $1c,"EXPLORE",$1e," THE PLANET",$0d,$0d
            .asc $1c,"BUILD",$1e," GAMMA MINES",$0d,$0d
            .asc $1c,"PROTECT",$1e," THE COLONISTS",$0d,$0d
            .asc "YOU HAVE 300 DAYS",$0d,$0d,$0d
            .asc $1c,"@@",$1e,"AGENT ANZU",$00
            
DirTable:   .byte 0,$ea,$01,$16,$ff
JoyTable:   .byte 0,$04,$80,$08,$10,$20            ; Corresponding direction bit
SearchPatt: .byte 1,2,3,3,4,4,1,1                  ; Sensor search pattern

BorderPatt: .byte 3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
            .byte 2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3
            .byte 3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4
            .byte 4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,1
            .byte 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
            .byte 1,1,1,0

CharSet:    .byte $00,$00,$00,$00,$7e,$00,$00,$00  ; -
            .byte $10,$28,$28,$44,$44,$80,$82,$00  ; A
            .byte $f8,$04,$04,$f8,$84,$84,$f8,$00  ; B
            .byte $30,$40,$80,$84,$84,$48,$30,$00  ; C
            .byte $f0,$88,$84,$84,$04,$08,$f0,$00  ; D
            .byte $fc,$00,$00,$fc,$00,$00,$fc,$00  ; E
            .byte $fc,$00,$00,$fc,$80,$80,$80,$00  ; F
            .byte $38,$40,$80,$94,$84,$44,$38,$00  ; G
            .byte $84,$84,$84,$b4,$84,$84,$84,$00  ; H
            .byte $10,$10,$10,$10,$10,$10,$10,$00  ; I
            .byte $04,$04,$04,$84,$84,$84,$78,$00  ; J
            .byte $84,$88,$90,$a0,$90,$88,$84,$00  ; K
            .byte $80,$80,$80,$80,$80,$40,$3c,$00  ; L
            .byte $82,$c4,$a8,$90,$80,$80,$80,$00  ; M
            .byte $84,$c4,$a4,$94,$8c,$04,$84,$00  ; N
            .byte $30,$08,$04,$84,$84,$48,$30,$00  ; O
            .byte $b8,$04,$04,$f8,$80,$80,$80,$00  ; P
            .byte $18,$04,$02,$82,$92,$54,$38,$10  ; Q
            .byte $f8,$04,$04,$f8,$80,$88,$84,$00  ; R
            .byte $70,$84,$80,$78,$04,$84,$78,$00  ; S
            .byte $f2,$10,$10,$10,$10,$10,$10,$00  ; T
            .byte $84,$04,$84,$84,$84,$84,$78,$00  ; U
            .byte $82,$80,$44,$44,$28,$28,$10,$00  ; V
            .byte $80,$80,$80,$90,$a8,$c4,$82,$00  ; W
            .byte $82,$40,$20,$10,$28,$44,$82,$00  ; X
            .byte $82,$40,$20,$10,$10,$10,$10,$00  ; Y
            .byte $be,$04,$08,$10,$20,$40,$fe,$00  ; Z
            .byte $3c,$7e,$df,$ef,$ff,$7e,$3c,$10  ; Hab North
            .byte $10,$18,$18,$18,$3c,$66,$c3,$81  ; Hab Connector
            .byte $07,$3c,$7e,$df,$ef,$ff,$7e,$3c  ; Hab Southwest
            .byte $e0,$3c,$7e,$df,$ef,$ff,$7e,$3c  ; Hab Southeast
            .byte $00,$00,$00,$99,$99,$00,$00,$00  ; Border
            .byte $00,$00,$00,$00,$00,$00,$00,$00  ; Space
SRC_CHAR:   .byte $00,$18,$24,$24,$ff,$7e,$18,$00  ; ! Player's Ship ($21)
XBITMAP_S:  .byte $00,$00,$00,$00,$00,$00,$00,$00  ; " Bitmap Source ($22)
XBITMAP_D:  .byte $00,$00,$00,$00,$00,$00,$00,$00  ; # Bitmap Destination ($23)
            .byte $00,$00,$00,$7e,$3c,$3c,$7e,$ff  ; Failed Mine
            .byte $0c,$30,$00,$aa,$28,$28,$aa,$aa  ; Successful Mine
            .byte $00,$00,$00,$00,$00,$00,$80,$80  ; Signal 1
            .byte $00,$00,$00,$00,$20,$20,$a0,$a0  ; Signal 2
            .byte $00,$00,$08,$08,$28,$28,$a8,$a8  ; Signal 3
            .byte $02,$02,$0a,$0a,$2a,$2a,$aa,$aa  ; Signal 4
            .byte $38,$00,$38,$54,$10,$10,$28,$44  ; Colonist Symbol
            .byte $30,$00,$0c,$00,$30,$08,$28,$aa  ; Gamma geiser
            .byte $18,$24,$24,$ff,$7e,$18,$24,$42  ; , Landed player ship
            .byte $ff,$cc,$88,$ff,$33,$22,$ff,$00  ; 
            .byte $10,$54,$38,$c6,$38,$54,$10,$00  ; 
            .byte $ff,$cc,$88,$ff,$33,$22,$ff,$00  ; 
            .byte $30,$48,$84,$84,$80,$40,$30,$00  ; 0
            .byte $30,$10,$10,$10,$10,$10,$10,$00  ; 1
            .byte $38,$44,$04,$08,$10,$20,$64,$00  ; 2
            .byte $fc,$00,$00,$18,$04,$84,$78,$00  ; 3
            .byte $14,$24,$44,$fc,$00,$00,$04,$00  ; 4
            .byte $f4,$80,$80,$78,$04,$84,$78,$00  ; 5
            .byte $74,$80,$80,$b8,$84,$84,$78,$00  ; 6
            .byte $82,$04,$08,$10,$20,$40,$80,$00  ; 7
            .byte $78,$04,$84,$78,$84,$80,$78,$00  ; 8
            .byte $78,$84,$84,$74,$04,$00,$04,$00  ; 9
            .byte $00,$30,$08,$04,$3e,$10,$08,$06  ; Energy Symbol
            .byte $00,$00,$00,$00,$00,$00,$f0,$f0  ; Large Signal 1
            .byte $00,$00,$00,$00,$f0,$f0,$f0,$f0  ; Large Signal 2
            .byte $00,$00,$f0,$f0,$f0,$f0,$f0,$f0  ; Large Signal 3
            .byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0  ; Large Signal 4
            .byte $00,$00,$20,$30,$00,$20,$30,$00  ; Colon
            