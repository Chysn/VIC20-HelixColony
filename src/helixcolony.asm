;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                                  Helix Colony
;                            (c)2020, Jason Justian
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
ST_ENERGY   = 200               ; Starting energy
COLONISTS   = 6                 ; Starting colonists
GEISERS     = 48                ; Approximate number of geisers
LENGTH      = 60                ; Days in a game

; Scoring Constants
COL_BONUS   = 50                ; Remaining colonist bonus
MINE_COST   = 50                ; Energy cost of building a mine
MINE_EN     = 3                 ; Daily energy from mine

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
CH_LANDED   = $2c               ; Landed ship
CH_GEISER   = $2b               ; Geiser
CO_GEISER   = $00               ; Geiser color
CO_GEISER_V = $09               ; Visible geiser color
CH_SENSOR   = $26               ; Sensor signal level
CO_SENSOR   = $04               ; Level color
CH_BORDER   = $1f               ; Border
CO_BORDER   = $06               ; Border color
CH_LSENSOR  = $3b               ; Large sensor signal
CH_BADMINE  = $24               ; Failed mine
CO_BADMINE  = $02               ; Failed mine color
CH_GOODMINE = $25               ; Successful mine
CO_GOODMINE = $03               ; Successful mine color
CH_COLONIST = $2a               ; Colonist
CO_COLONIST = $05               ; Colonist color
CHAR_S      = $22               ; Source bitmap character code
CHAR_D      = $23               ; Destination bitmap character code

; System Resources
CINV        = $0314             ; ISR vector
;NMINV       = $0318             ; Release NMI vector
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
PRTFIX      = $ddcd             ; Decimal display routine (A,X)
CHROUT      = $ffd2             ; Output one character
TIME_L      = $a2               ; Jiffy counter low
TIME_M      = $a1               ; Jiffy counter middle  

; Game Memory - Zeropage Pointers
CURSOR      = $f9               ; Cursor (2 bytes)
PLAYER      = $fb               ; Player location (2 bytes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAIN PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Startup:    jsr SetupHW         ; Set up hardware           
            lda #$00            ; Initialize high score
            sta HISCORE         ; ,,
            sta HISCORE+1       ; ,,
            sta LEVEL           ; Initialize level to "normal"
           
; Welcome Screen
; Show intro, set level, then show manual page      
Welcome:    jsr MStop
            jsr CLSR            ; Clear screen
            ldy #5              ; Draw Hab
            jsr TextLine        ; ,,
            lda #<DrawHab       ; ,,
            ldy #>DrawHab       ; ,,
            jsr PRTSTR          ; ,,
            lda #<Intro         ; Show Intro
            ldy #>Intro         ; ,,
            jsr PRTSTR          ; ,,
            jsr SetLevel        ; Set level
            lda #<Manual
            ldy #>Manual
            jsr PRTSTR

; Start a new game            
Start:      jsr Wait4Fire
            jsr InitGame

; Main loop            
Main:       bit SCORE_FLAG      ; The score flag is set by the ISR; check it
            bpl ch_end          ;   and display the Score Bar if requested
            jsr ScoreBar        ;   ,,
ch_end:     lda COL_CT          ; Any colonists left?
            beq game_over       ; ,,
            lda DAY             ; Any time left?
            cmp #LENGTH         ; ,,
            bcc game_on         ; ,,
game_over:  jmp GameOver        ; Game Over if any game-ending conditions
game_on:    jsr Joystick        ; Read the joystick
            beq Main            ; If not moved, check again
            cmp #FIRE           ; Has fire been pressed?
            bne han_dir         ; If not, handle a direction
            jsr TogBuild        ; If fire, toggle the build flag
            jmp Main
han_dir:    bit BUILD_FLAG      ; Is the build flag on?
            bpl norm_move       ; If not, do a normal directional move
            jsr BuildMine       ; If build flag is on, build a mine
            jmp Main
norm_move:  jsr Move
            jmp Main
            
 ; Custom ISR for music player and day counting
ISR:        lda TIME_L
            bne music
            lda #$80
            sta SCORE_FLAG
            jsr DailyMaint      ; Daily energy maintenance production and usage
            inc DAY
            lda THEME           ; Every day, slightly modify the music
            eor #$01            ; ,,
            sta THEME           ; ,,
music:      jsr NextNote
            jsr NextFX
            jmp IRQ           
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GAME MECHANICS ROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;            
; New Ship
; Return ship to starting point, decrement colonist count, launch sound, then
; display the status bar
NewShip:    lda COL_CT          ; Only present a new ship if there are
            beq StatusBar       ;   colonists remaining
            lda #" "            ; Initialize character under player
            sta UNDER           ; ,,
            lda #$e7            ; Initialize player loction
            sta PLAYER          ; ,,
            lda #$1e            ; ,,
            sta PLAYER+1        ; ,,
            lda #EAST           ; Move the ship away from the Hab
            jsr Move            ; ,,
            lda #EAST           ; ,,
            jsr Move            ; ,,
            jsr RSCursor        ; Place landed ship  
            lda #CH_LANDED      ; ,,
            ldy #CO_PLAYER      ; ,,
            jsr DrawChar
            lda #" "            ; Initialize character under player
            sta UNDER           ; ,,
            lda #$00
            sta SENSOR_CT
            ; Fall through to StatusBar

; Display Status Bar
; At the bottom of the screen, and includes signal strength and colonist display
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
col_disp:   lda #$f8
            sta CURSOR
            lda COL_CT          ; Get the number of colonists
            beq status_r        ; If there are no colonists, don't show any
            pha
-loop:      lda #CH_COLONIST
            ldy #CO_COLONIST
            jsr DrawChar
            ldx #WEST
            jsr MoveCursor
            dec COL_CT
            bne loop
            pla
            sta COL_CT
status_r:   rts
            
; Score Bar
ScoreBar:   lsr SCORE_FLAG      ; Clear score flag
            lda #<ScEnergy
            ldy #>ScEnergy
            jsr PRTSTR
            ldx ENERGY
            lda ENERGY+1
            jsr PRTFIX
            lda #" "
            jsr CHROUT
            jsr CHROUT
            lda #<ScDay
            ldy #>ScDay
            jsr PRTSTR
            ldx DAY
            cpx #LENGTH
            bcc show_day
            ldx #LENGTH
show_day:   lda #$00
            jmp PRTFIX
            
; Game Over
GameOver:   lda #$0f            ; Fade out the music
            sta FADE            ; ,,
            lda #<GameOverTx    ; Show Game Over message
            ldy #>GameOverTx    ; ,,
            jsr PRTSTR          ; ,,
            lda #$64
            jsr Delay
bonus1:     lda COL_CT          ; Add points for each remaining
            beq bonus2          ;   colonist
            dec COL_CT          ;   ,,
            lda #COL_BONUS      ; Add remaining colonist bonus
            jsr AddEnergy       ; ,,
            jsr ScoreBar
            jsr StatusBar
            lda #$03
            jsr Sound
            lda #$18
            jsr Delay
            jmp bonus1
bonus2:     lda MINE_CT         ; Add points for each successful mine
            beq scored          ;   built
            dec MINE_CT         ;   ,,
            lda #MINE_COST      ; Add mine cost bonus
            jsr AddEnergy       ; ,,
            jsr ScoreBar
            lda #$03
            jsr Sound
            lda #$08
            jsr Delay
            jmp bonus2
scored:     lda HISCORE+1       ; Is the last score greater than
            cmp ENERGY+1        ;   the high score?
            bcc new_hs          ;   ,,
            bne show_hs         ;   ,,
            lda ENERGY          ;   ,,
            cmp HISCORE         ;   ,,
            bcc show_hs         ;   ,,
new_hs:     lda ENERGY          ; A new high score has been
            sta HISCORE         ; achived; update high score
            lda ENERGY+1        ; ,,
            sta HISCORE+1       ; ,,        
show_hs:    ldy #21             ; Show High Score
            jsr TextLine        ; ,,
            lda #<HiScoreTx     ; ,,
            ldy #>HiScoreTx     ; ,,
            jsr PRTSTR          ; ,,
            ldx HISCORE         ; ,,
            lda HISCORE+1       ; ,,
            jsr PRTFIX          ; ,,
            jsr ScoreBar
            jmp Start          
            
; Toggle Build Flag
; Then draw the ship, either landed or flying
TogBuild:   jsr RSCursor        ; Set Cursor to the player position
            ldy #CH_PLAYER      ; Set a default character
            bit BUILD_FLAG      ; Check the build flag
            bmi clear_bf        ; If it's set, clear it
set_bf:     lda #$80            ; If it's clear, set it
            sta BUILD_FLAG      ; ,,
            ldy #CH_LANDED      ; And also change the default character (land)
            .byte $3c           ; TOP (skip word)
clear_bf:   lsr BUILD_FLAG      ; Clear the build flag
            tya                 ; Draw the ship based on the new state
            ldy #CO_PLAYER      ; ,,
            jsr DrawChar        ; ,,
            bit BUILD_FLAG      ; Select music based on state of flag
            bpl build_off       ; ,,
            lda THEME           ; Save the main theme's variation, so that
            sta THEME_TMP       ;   it can be restored when the build is over
            lda THEME+1         ;   ,,
            sta THEME_TMP+1     ;   ,,
            lda #$01            ; Select the build theme
            jsr Music           ; ,,
            ldy #22             ; Show help for build
            jsr TextLine        ; ,,
            lda #<BuildTx       ; ,,
            ldy #>BuildTx       ; ,,
            jsr PRTSTR          ; ,,
            jmp debounce
build_off:  lda THEME_TMP       ; Restore the main theme where it left off
            sta THEME           ; ,,
            lda THEME_TMP+1     ; ,,
            sta THEME+1         ; ,,
            jsr StatusBar       ; Replace help with status bar
debounce:   lda #$14            ; Short delay to debounce the fire button
            jmp Delay           ; ,,

; Build Mine
BuildMine:  ldy ENERGY+1        ; Make sure that the colony has enough
            bne has_enough      ;   energy to build a mine
            ldy ENERGY          ;   ,,
            cpy #MINE_COST      ;   ,,
            bcc build_r         ; If not, don't build it
has_enough: jsr RSCursor
            jsr MoveCursor
            jsr GetChar         ; Get the character at the move location
            cmp #CH_GEISER      ; Is it a geiser?
            beq build_good      ; If so, build a successful mine
            cmp #" "            ; Is it a border or hab?
            bcc build_r         ; If so, do nothing
            cmp #CH_GOODMINE    ; Is there already a mine at this location?
            beq build_r         ; ,,
            cmp #CH_BADMINE     ; ,,
            beq build_r         ; ,,
            lda #$01            ; Failed mine sound
            jsr Sound           ; ,,
            lda #CH_BADMINE
            ldy #CO_BADMINE
            bne draw_mine
build_good: inc MINE_CT         ; Increment successful mine count
            lda #$00            ; Successful mine sound
            jsr Sound           ; ,,
            lda #CH_GOODMINE
            ldy #CO_GOODMINE
draw_mine:  jsr DrawChar 
            lda #MINE_COST      ; Pay for the mine
            jsr UseEnergy       ; ,,
build_r:    jmp TogBuild        ; Clear the build flag and return 
       
; Use Energy  
; In A          
UseEnergy:  sta SCRPAD
            lda ENERGY
            sec
            sbc SCRPAD
            sta ENERGY
            bcs pos
            dec ENERGY+1
ch_pos:     lda ENERGY+1        ; Constrain energy to positive
            bpl pos             ; ,,
            lda #$00            ; ,,
            sta ENERGY          ; ,,
            sta ENERGY+1        ; ,,
pos:        lda #$80
            sta SCORE_FLAG
            rts
            
; Add Energy
; In A
AddEnergy:  clc
            adc ENERGY
            sta ENERGY
            bcc add_r
            inc ENERGY+1
add_r:	    jmp pos                      

; Daily Energy Maintenance
DailyMaint: ldy MINE_CT         ; Each successful mine generates energy
            beq maint_use       ; No mines
-loop:      lda #MINE_EN        ;   every day
            jsr AddEnergy       ;   ,,
            dey                 ;   ,,
            bne loop            ;   ,,
maint_use:  lda #$01            ; The colonists use one energy per day
            jsr UseEnergy       ; ,,            
            rts            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MUSIC AND EFFECT PLAYER SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Start the music player
MPlay:      lda #$01
            sta PLAY
            lda #$00
            sta FADE
            rts
    
; Stop the music player
MStop:      lda #$00
            sta VOICEL
            sta PLAY
            rts
            
; Select Music
; Set a musical theme 
;
; Preparations
;     A is the theme index
Music:      asl                 ; Multiply level by 2 for theme index
            tax
            lda Themes,X        ; Set the musical theme
            sta THEME 
            lda Themes+1,X
            sta THEME+1
            rts            
                                
; Play Next Note
; Rotates the 16-bit register one bit to the left
; and plays the note
NextNote:   lda #$01
            bit PLAY
            beq note_r
            dec MUCD 
            bne note_r
            lda TEMPO
            sta MUCD
            lda #$00            ; Shift the register left
            asl THEME           ; ,,
            rol THEME+1         ; ,,
            adc THEME           ; ,,
            sta THEME           ; ,,
            ora #$80            ; Gate the voice
            sta VOICEL          ; ,,
            lda FADE            ; Fade is a volume override. If fade is
            beq volreg          ;   set, it will decrease every note,
            dec FADE            ;   and the music will stop when it
            bne vol             ;   reaches zero
            jmp MStop
volreg:     lda THEME+1         ; Set the music volume and flash
vol:        sta VOLUME          ;   the aux color
note_r:     rts

; Play Next Sound Effect
; Rotates the 8-bit sound effect register and
; plays the pitch      
NextFX:     lda FXLEN           ; Has the sound been launched?
            beq endfx           ; If unlaunched, kill voice and return
            dec FXLEN
            dec FXCD
            BNE fx_r
            lda FXCDRS          ; Reset the countdown
            sta FXCD            ; ,,
            lda #$00            ; Rotate the register left
            asl REG_FX          ; ,,
            adc REG_FX          ; ,,
            sta REG_FX          ; ,,
            ora #$80            ; Gate the high voice
endfx:      sta VOICEH          ; ,,
fx_r:       rts      
        
; Launch Sound Effect
; Preparations
;     A - The sound effect index
Sound:      sei                 ; Don't play anything while setting up
            stx SCRPAD
            asl                 ; Each effect has two parameters in the
                                ;   table, register and length (in
                                ;   jiffies)
            tax
            lda FXType,X        ; Get the register
            sta REG_FX          ;   and activate it
            inx
            lda FXType,X        ; Get the length
            and #$F0            ;   ,,
            sta FXLEN           ;   and set it
            lda FXType,X
            and #$0F
            sta FXCDRS          ; Record the reset value
            sta FXCD            ; Set the countdown
            ldx SCRPAD
            cli                 ; Go! 
            rts
        
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
; Return the direction in A
; 1=North, 2=East, 3=South, 4=West, 5=Fire, 0=None
Joystick:   lda VIA1PA          ; Read VIA1 port
            and #$3c            ; Keep track of bits 2,3,4,5
            sta JOYREG
            lda VIA2PB          ; Combine with read of bit 7
            and #$80            ;   from VIA2-B
            ora JOYREG
            eor #$bc            ; Flip each joystick bit in the
                                ;   combined read byte, so that
                                ;   on = 1
            sta JOYREG          ; Store temporary direction
            ldx #$05            ; Check five directions (5=fire)
-loop:      lda JoyTable,x      ; Is the joystick pointed in the
            bit JOYREG          ;   direction indexed by X?
            bne found_dir       ; If so, set that as the joystick direction
            dex                 ; Loop back until found, or 0
            bne loop            ; ,,
found_dir:  txa
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
; Move character in direction specified in A
Move:       ldy ENERGY+1        ; Does the player have enough energy to move?
            bne may_move        ; ,,
            ldy ENERGY          ; ,,
            bne may_move        ; ,,
            rts                 ; If not, don't move
may_move:   sta DIR             ; Set the direction
            jsr PickChar        ; Set Cursor to the right source character
            ldy #$07            ; Initialize the source and destination
-loop:      lda #$00            ;   characters in the character set
            sta BITMAP_D,y      ;   ,,
            lda (CURSOR),y      ;   ,,
            sta BITMAP_S,y      ;   ,,
            dey                 ;   ,,
            bpl loop            ;   ,,
            jsr RSCursor
            lda DIR             ; Engine noise
            asl                 ; ,,
            asl                 ; ,,
            asl                 ; ,,
            asl                 ; ,,
            ora #$80            ; ,,
            sta NOISE           ; ,,
            lda #CHAR_S         ; Draw the bitmap source character at the
            ldy #CO_PLAYER      ;   player location
            jsr DrawChar        ;   ,,
            ldx DIR             ; Move the cursor
            jsr MoveCursor      ; ,,
            jsr GetChar         ; Get the character at the Cursor location and
            cmp #CH_BORDER+1    ; If there's a border or hab, do not move
            bcs do_move         ; ,,
            rts                 ; ,,
do_move:    pha                 ; Put character on stack for later use as UNDER
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
            lda #$01            ; Take one energy unit per turn
            jsr UseEnergy       ; ,,
            lda #$00
            sta NOISE
            ; Fall through to Sensor

; Activate Sensor    
; Check the area around the player for geisers and upate the character
; memory and Status Bar with the results        
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
            beq detected
            cmp #CH_GOODMINE
            bne no_geiser
detected:   inc SENSOR_CT
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
            ldy UNDER           ; Don't overwrite mines with sensor
            cpy #CH_BADMINE     ;   readings
            beq sensor_r        ;   ,,
            cpy #CH_GOODMINE    ;   ,,
            beq sensor_r        ;   ,,
            clc
            adc #CH_SENSOR-1
            sta UNDER
            lda #CO_SENSOR
            sta UNDER_COL
sensor_r:   jmp StatusBar

; Player hit Geiser 
; Move player back to starting point           
Dead:       lda #$7f            ; Set aux color and high volume
            sta VOLUME          ; ,,
            lda #$00            ; Set character of destroyed ship
            ldy #$09            ; Set color of destroyed ship
            jsr DrawChar        ; ,,
            lda #$02            ; Killed colonist sound
            jsr Sound           ; ,,
            lda #$40            ; Delay for destruction
            jsr Delay           ; ,,
            lda #CH_GEISER      ; Mise well show the player where the
            ldy #CO_GEISER_V    ;   geiser is
            jsr DrawChar        ;   ,,
            lda #$40            ; Another delay for destruction
            jsr Delay           ; ,,
            dec COL_CT          ; Kill off a colonist. Way to go, player ;(
            bpl next_ship       ; Enforce low range of 0
            lda #$00            ; ,,
            sta COL_CT          ; ,,
next_ship:  jmp NewShip

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
            
; Get Character at Cursor            
GetChar:    ldx #$00
            lda (CURSOR,x)
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
  
; Pick Character
; Depending on which direction the player is moving, a different character
; will be loaded into the source bitmap character. Direction is in A.
PickChar:   tax
            lda PlSrcL,x
            sta CURSOR
            lda PlSrcH,x
            sta CURSOR+1
            rts 
            
; Postion Text
; To line in Y            
TextLine:   lda #$13            ; Home cursor
            jsr CHROUT
            lda #$11            ; Cursor down
-loop:      jsr CHROUT
            dey
            bne loop
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
            sta VOICEL          ; ,,
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
            lda #$05            ; Play game startup sound
            jsr Sound           ; ,,
            lda #$09            ; Initialize Tempo
            sta TEMPO           ; ,,
            lda #<ST_ENERGY     ; Initialize Energy to starting value
            sta ENERGY          ; ,,
            lda #>ST_ENERGY     ; ,,
            sta ENERGY+1        ; ,,
            lda LEVEL
ch_harder:  cmp #$01            ; Harder= Subtract half the energy
            bne ch_easier       ; ,,
            lda #ST_ENERGY/2    ; ,,
            jsr UseEnergy       ; ,,
ch_easier:  cmp #$02            ; Easier= Add half the energy
            bne init_build      ; ,,
            lda #ST_ENERGY/2    ; ,,
            jsr AddEnergy       ; ,,
init_build: lda #$00            ; Initialize build flag
            sta BUILD_FLAG      ; ,,
            sta DAY             ; Initialize day number
            sta DAY+1           ; ,,
            sta MINE_CT         ; Initialize successful mine count
            sta SENSOR_CT       ; Initialize sensor count
            jsr Music           ; Select default musical theme
            lda #COLONISTS      ; Initialize colonist count
            sta COL_CT          ; ,,
            ldy #GEISERS/2      ; Place geisers
-loop:      jsr NewGeiser       ; ,,
            dey                 ; ,,
            bne loop            ; ,,
            ldy #9              ; Draw Hab
            jsr TextLine        ; ,,
            lda #<DrawHab       ; ,,
            ldy #>DrawHab       ; ,,
            jsr PRTSTR          ; ,,
            lda #<SCREEN        ; Draw the border around the planet
            sta CURSOR          ; ,,
            lda #>SCREEN        ; ,,
            sta CURSOR+1        ; ,,
            lda #SOUTH          ; Start in the upper-left corner and move
            jsr MoveCursor      ;   one space south
            ldy #$00            ; From there, follow the pattern in the
-loop:      tya                 ;   BorderPatt table to surround the play
            pha                 ;   area
            lda BorderPatt,y    ;   ,,
            beq bord_done       ;   ,,
            tax                 ;   ,,
            jsr MoveCursor      ;   ,,
            lda #CH_BORDER      ;   ,,
            ldy #CO_BORDER      ;   ,,
            jsr DrawChar        ;   ,,
            lda #$01            ; A one jiffy delay to make the border
            jsr Delay           ;   sort of animated
            pla                 ; The iterator, Y, is protected from MoveCursor
            tay                 ;   on the stack
            iny                 ; Continue the pattern until done
            bne loop            ; ,,
bord_done:  pla                 ; There's a "hanging" stack entry, so pull it
            lda #$e4            ; Set the color for the Status Bar
            sta CURSOR          ; ,,
            lda #$97            ; ,,
            sta CURSOR+1        ; ,,
            ldy #$15            ; ,,
            lda #$07            ; ,,
-loop:      sta (CURSOR),y      ; ,,
            dey                 ; ,,
            bpl loop            ; ,,
            jsr MPlay           ; Start music
            lda #" "            ; Make sure that the path to the ship
            sta $1ee7           ;   isn't beset with geisers
            sta $1ee8           ;   ,,
            sta $1ee9           ;   ,,
            jsr NewShip         ; Place ship
            jsr RSCursor        ; Clear the area around the ship of geisers
            ldy #$00            ; ,,
-loop:      tya                 ; ,,
            pha                 ; ,,
            lda SearchPatt,y    ; ,,
            tax                 ; ,,
            jsr MoveCursor      ; ,,
            lda #" "            ; ,,
            ldy #$00            ; ,,
            jsr DrawChar        ; ,,
            pla                 ; ,,
            tay                 ; ,,
            iny                 ; ,,
            cpy #$08            ; ,,
            bne loop            ; ,,
            rts

; Place Geiser Pair       
; Gamma geisers are place in pairs to try to get them evenly
; distributed between the two pages of screen memory     
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

; Set Level
; Allow player to set starting energy, and return when Fire is pressed
SetLevel:   jsr Joystick        ; Debounce the joystick
            bne SetLevel        ; ,,
            ldy #17             ; Show the "LEVEL" text
            jsr TextLine        ; ,,
            lda #<LevelTx       ; ,,
            ldy #>LevelTx       ; ,,
            jsr PRTSTR          ; ,,
            ldx LEVEL           ; Use the level value as an index to
            lda LevelNameH,x    ;   find the level name pointer
            tay                 ;   ,,
            lda LevelNameL,x    ;   ,,
            jsr PRTSTR          ; Display the level name
level_js:   jsr Joystick        ; Wait for joystick input
            beq level_js        ; ,,
            cmp #FIRE           ; If fire is pressed, continue to whatever
            bne ch_west         ;   is next
            rts                 ;   ,,
ch_west:    cmp #WEST           ; If the joystick was moved west, decrease
            bne ch_east         ;   the level
            dec LEVEL           ;   ,,
            bpl set_done        ;   ,,
            lda #$02            ; If the level goes below 0, set it back
            sta LEVEL           ;   to 2
            jmp set_done        ;   ,,
ch_east:    cmp #EAST           ; If the joystick was moved east, increase
            bne level_js        ;   the level
            inc LEVEL           ;   ,,
            lda LEVEL           ; I the level goes above 2, set it back
            cmp #$03            ;   to 0
            bne set_done        ;   ,,
            lda #$00            ;   ,,
            sta LEVEL           ;   ,,
set_done:   lda #$04            ; Play a sound when the level changes
            jsr Sound           ; ,,
            jmp SetLevel
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Intro:      .asc $0d,$0d,$1c,"     H",$1e,"ELIX",$1c,"C",$1e,"OLONY",$0d
            .asc $1e,"     J",$1c,"ASON",$1e,"J",$1c,"USTIAN",$0d
            .asc $1c,"     P",$1e,"RESS",$1c,"F",$1e,"IRE",$00

; Hab Image
DrawHab:    .asc "          "
            .asc $1e,$5b,$11,$9d,$5c,$11,$9d,$9d,$5d," ",$5e,$00

; Manual Text            
Manual:     .asc $93,$1c,"I",$1e,"T IS THE  YEAR 1999",$0d,$0d,$0d
            .asc $1c,"E",$1e,"ARTH  HAS ENTRUSTED",$0d,$0d
            .asc "YOU TO ESTABLISH THE",$0d,$0d
            .asc "HELIX COLONY  IN THE",$0d,$0d
            .asc "PROCYON SYSTEM",$0d,$0d,$0d
            .asc $9e,$21,$1c," EXPLORE",$1e," THE PLANET",$0d,$0d
            .asc $9f,$25,$1c," BUILD",$1e,"  GAMMA MINES",$0d,$0d
            .asc $1e,$2a,$1c," PROTECT",$1e,"  YOUR CREW",$0d,$0d,$0d
            .asc $1c,"Y",$1e,"OU HAVE  SIXTY DAYS",$0d,$0d,$0d
            .asc "           ",$1c,"A",$1e,"GENT",$1c,"A",$1e,"NZU",$00

; Score Bar            
ScEnergy:   .asc $13,$1d,$9e,$3a,$1e," ",$00
ScDay:      .asc $13,$1d,$1d,$1d,$1d,$1d,$1d,$1d
            .asc $1d,$1d,$1d,$1d,$1d,$1d,$1d,$1d
            .asc "DAY ",$00     

; In-Game Text            
GameOverTx: .asc $13,$11,$1d,$1d,$1d,$1d,$1d,$1d
            .asc $1d,$1c,"G",$1e,"AME",$1c,"O",$1e
            .asc "VER",$00   
            
HiScoreTx:  .asc $1d,$1d,$1d,$1d,$1d,$1e,"H",$1c,"IGH"
            .asc $1e,"S",$1c,"CORE",$1e,$00               
    
BuildTx:    .asc " ",$9e,$3a," 50 ",$1e,"MOVE TO BUILD "
            .asc $9f,CH_BADMINE,$1e,$00  
            
; Set Level Text and Table            
LevelTx:    .asc $1e,"     L",$1c,"EVEL ",$00  
NormalTx:   .asc $9e,"NORMAL",$00
HarderTx:   .asc $1c,"HARDER",$00
EasierTx:   .asc $1e,"EASIER",$00
LevelNameL: .byte <NormalTx,<HarderTx,<EasierTx
LevelNameH: .byte >NormalTx,>HarderTx,>EasierTx
 
; Direction Tables                       
DirTable:   .byte 0,$ea,$01,$16,$ff
JoyTable:   .byte 0,$04,$80,$08,$10,$20            ; Corresponding direction bit
SearchPatt: .byte 1,2,3,3,4,4,1,1                  ; Sensor search pattern

; Border Pattern Table
BorderPatt: .byte 3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
            .byte 2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3
            .byte 3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4
            .byte 4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,1
            .byte 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
            .byte 1,1,1,0
            
; Player character movement sources
PlSrcL:     .byte 0,<SHIP_NORTH,<SHIP_EAST,<SHIP_SOUTH,<SHIP_WEST 
PlSrcH:     .byte 0,>SHIP_NORTH,>SHIP_EAST,>SHIP_SOUTH,>SHIP_WEST           

; Musical Themes
Themes:     .word $4030
            .word $5555
            
; Sound effects for the sound effects player
; Each effect has three parameters
;   (1) First byte is the starting shift register value
;   (2) High nybble of second byte is the length in jiffies x 16
;   (3) Low nybble of second byte is refresh rate in jiffies
FXType:     .byte $11,$56                       ; Successful mine
            .byte $2b,$51                       ; Failed Mine
            .byte $55,$41                       ; Colonist killed
            .byte $2f,$12                       ; Bonus Colonist
            .byte $a3,$14                       ; Level Select
            .byte $a3,$57                       ; Game Start

; Padding to 3583 bytes 
Padding:    .asc "ALLWORKANDNOPLAYMAKESJACKADULLBOY"
            .asc "ALLWORKANDNOPLAYMAKESJACKADULLBOY"
            .asc "ALLWORKANDNOPLAYMAKESJACKADULLBOY"
            .asc "ALLWORKANDNOPLAYMAKESJACKADULLBOY"
            .asc "ALLWORKANDNOPLAYMAKESJACKADULLBOY"
            .asc "ALLWORKANDNOPLAYMAKESJACKADULLBOY"
            .asc "ALLWORKANDNOPLAYMAKESJACKADULLBOY"
            .asc "ALLWORKANDNOPLAYMAKESJACKADULLBOY"
            .asc "ALLWORKANDNOPLAYMAKESJACKADULLBOY"
            .asc "ALLWORKANDNOPLAYMAKESJACKADULLBOY"
            .asc "ALLWORKANDNOPLAYMAKESJACKADULLBOY"
            .asc "ALLWORKANDNOPLAYMAKESJACKADULLBOY"
            .asc "ALLWORKANDNOPLAYMAKESJACKADULLBOY"
            .asc "ALLWORKANDNOPLAYMAKESJACKADULLBOY"
            .asc "ALLWORKANDNOPLAYMAKESJACKADULLBOY"
            .asc "ALLWORKANDNOPLAYMAKESJACKADULLBOY"
            .asc "ALLWORKANDNOPLAYMAKESJACKADULLBOY"
            .asc "ALLWORKANDNOPLAYMAKESJACKADULLBOY"
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Game Data
            .asc "GAME VARIABLES FOLLOW :"
HISCORE:    .byte $00,$00       ; High Score
ENERGY:     .byte $00,$00       ; Energy
DIR:        .byte $00           ; Direction
UNDER:      .byte $00           ; Character under player
UNDER_COL:  .byte $00           ; Color under player
SENSOR_CT:  .byte $00           ; Local sensor count
COL_CT:     .byte $00           ; Colonist count
DAY:        .byte $00           ; Day number
MINE_CT:    .byte $00           ; Successful mine count
LEVEL:      .byte $00,$00       ; Level (0=normal, 1=harder, 2=easier)
SCRPAD:     .byte $00           ; Temporary scratchpad
JOYREG:     .byte $00           ; Joystick register
BUILD_FLAG: .byte $00           ; Bit 7 set if in build mode
SCORE_FLAG: .byte $00           ; Bit 7 set when score has changed

; Music Player Memory                 
THEME       .byte $00,$00       ; Music shift register theme (2 bytes)
TEMPO       .byte $00           ; Tempo (lower numbers are faster)
MUCD        .byte $00           ; Tempo countdown
PLAY        .byte $00           ; Music is playing
FADE        .byte $00           ; Fadeout volume
THEME_TMP   .byte $00,$00       ; Theme temporary storage (2 bytes)

; Sound Effects Player Memory
REG_FX      .byte $00           ; Sound effects register
FXLEN       .byte $00           ; Sound effects length
FXCD        .byte $00           ; Sound effects countdown
FXCDRS      .byte $00           ; Countdown reset value

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CUSTOM CHARACTER SET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The character set must start at $1C00. If you change anything
; anywhere, you must account for this. The easiest way is to use
; padding bytes immediately before this character data.
;
; The easiest way to tell if you've done this right is to
; make sure that the object code is exactly 3583 bytes. This is
; a reliable method as long as you don't add anything AFTER this
; character data.
;                       
CharSet:    .byte $c0,$03,$3c,$03,$c0,$3c,$c0,$03  ; Ship explosion (multicolor)
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
            .byte $30,$08,$84,$84,$84,$48,$30,$00  ; O
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
            .byte $00,$18,$24,$24,$ff,$7e,$18,$00  ; ! Player's Ship ($21)
BITMAP_S:   .byte $00,$00,$00,$00,$00,$00,$00,$00  ; " Bitmap Source ($22)
BITMAP_D:   .byte $00,$00,$00,$00,$00,$00,$00,$00  ; # Bitmap Destination ($23)
            .byte $00,$00,$00,$7e,$3c,$3c,$7e,$ff  ; Failed Mine
            .byte $52,$24,$00,$7e,$3c,$3c,$7e,$ff  ; Successful Mine
            .byte $00,$00,$00,$00,$00,$00,$10,$10  ; Signal 1
            .byte $00,$00,$00,$00,$08,$08,$28,$28  ; Signal 2
            .byte $00,$00,$04,$04,$14,$14,$54,$54  ; Signal 3
            .byte $02,$02,$0a,$0a,$2a,$2a,$aa,$aa  ; Signal 4
            .byte $38,$00,$38,$54,$10,$10,$28,$44  ; Colonist Symbol
            .byte $30,$00,$0c,$00,$30,$08,$28,$aa  ; Gamma geiser
            .byte $18,$24,$24,$ff,$7e,$18,$24,$42  ; , Landed player ship
SHIP_WEST:  .byte $08,$14,$24,$2f,$7e,$f8,$10,$00  ; Ship moving west 
SHIP_EAST:  .byte $10,$28,$24,$f6,$7f,$1c,$08,$00  ; Ship moving east
SHIP_SOUTH: .byte $00,$18,$24,$e7,$7e,$18,$00,$00  ; Ship moving south
            .byte $30,$48,$84,$84,$80,$40,$30,$00  ; 0
            .byte $30,$10,$10,$10,$10,$10,$10,$00  ; 1
            .byte $38,$44,$04,$08,$10,$20,$64,$00  ; 2
            .byte $fc,$00,$00,$18,$04,$84,$78,$00  ; 3
            .byte $14,$24,$44,$fc,$00,$00,$04,$00  ; 4
            .byte $f4,$80,$80,$78,$04,$84,$78,$00  ; 5
            .byte $74,$80,$80,$b8,$84,$84,$78,$00  ; 6
            .byte $be,$04,$08,$10,$20,$40,$80,$00  ; 7
            .byte $78,$04,$84,$78,$84,$80,$78,$00  ; 8
            .byte $78,$84,$84,$74,$04,$00,$04,$00  ; 9
            .byte $00,$30,$08,$04,$3e,$10,$08,$06  ; Energy Symbol
            .byte $00,$00,$00,$00,$00,$00,$f0,$f0  ; Large Signal 1
            .byte $00,$00,$00,$00,$f0,$f0,$f0,$f0  ; Large Signal 2
            .byte $00,$00,$f0,$f0,$f0,$f0,$f0,$f0  ; Large Signal 3
            .byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0  ; Large Signal 4
SHIP_NORTH: .byte $00,$18,$24,$ff,$7e,$3c,$00,$00  ; Ship moving north
            