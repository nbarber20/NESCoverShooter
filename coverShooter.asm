.segment "HEADER"
	.byte "NES"
	.byte $1a
	.byte $02; 2* 16kb prg rom
	.byte $01; 1 * 8kb chr rom
	.byte %00000001; mapper and mirroring
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00, $00, $00, $00, $00; filler

.scope EntityType
	NOEntity = 0
	PlayerEntity = 1
	Bullet = 2
	Enemy = 3
 .endscope

.struct Entity
 	xpos .byte
 	ypos .byte
 	type .byte
 	spritetile .byte
 	direction .byte
.endstruct

.segment "STARTUP"

.segment "ZEROPAGE"
	world: .res 2
	ControllerInput: .res 1
	MAXENTITIES = 20
	entities: .res .sizeof(Entity) * MAXENTITIES
	TOTALENTITIES = .sizeof(Entity) * MAXENTITIES
	spritemem: .res 2
	abtnLatch: .res 1
	bulletOffset: .res 1
	temp: .res 1
	tempX: .res 1
	tempY: .res 1
.segment "CODE"

WAITFORVBLANK:
	BIT $2002
	BPL WAITFORVBLANK
	RTS

RESET:
	SEI ; Disable all interrupt
	CLD ; Disable decimal mode
	LDX #$40
	STX $4017 
	LDX #$FF
	TXS
	INX ;FF+1 = 00
	STX $2000
	STX $2001
	STX $4010
	JSR WAITFORVBLANK
	TXA

CLEARMEM:
	STA $0000, X
	STA $0100, X
	STA $0300, X
	STA $0400, X
	STA $0500, X
	STA $0600, X
	STA $0700, X
	LDA #$FF
	STA $0200, X
	LDA #$00
	INX; roll over to zero flag and branch if not equal to see if we rolled over
	BNE CLEARMEM

;Init entities
	LDA #$80
	STA entities+Entity::xpos ;set player pos x
	LDA #$78
	STA entities+Entity::ypos ;set player pos y
	LDA #EntityType::PlayerEntity
	STA entities+Entity::type ;set as player
	LDA #$00
	STA entities+Entity::spritetile
	LDA #$01
	STA entities+Entity::direction


	LDX #.sizeof(Entity)

	LDA #$80
	STA entities+Entity::xpos,x ;set enemy pos x
	LDA #$78
	STA entities+Entity::ypos,x ;set enemy pos y
	LDA #EntityType::Enemy
	STA entities+Entity::type,x ;set as enemy
	LDA #$04
	STA entities+Entity::spritetile,x
	LDA #$01
	STA entities+Entity::direction,x


	LDX #.sizeof(Entity) *2
	LDA #$FF

CLEARENTITIES:
	STA entities+Entity::xpos, x
	STA entities+Entity::ypos, x
	LDA #$00
	STA entities+Entity::type,x
	STA entities+Entity::spritetile,x
	STA entities+Entity::direction,x	
	TXA
	CLC
	ADC #.sizeof(Entity)
	TAX
	CPX #TOTALENTITIES
	BNE CLEARENTITIES




;interface with ppu
	JSR WAITFORVBLANK
	LDA #$02
	STA $4014
	NOP

	LDA #$3F
	STA $2006
	LDA #$00
	STA $2006

	LDX #$00

LoadPalettes:
    LDA PALETTEDATA, X
    STA $2007
    INX
    CPX #$20
    BNE LoadPalettes    

    ; Initialize world to point to world data
    LDA #<WorldData
    STA world
    LDA #>WorldData
    STA world+1

    ; setup address in PPU for nametable data
    BIT $2002
    LDA #$20
    STA $2006
    LDA #$00
    STA $2006

    LDX #$00
    LDY #$00
LoadWorld:
    LDA (world), Y
    STA $2007
    INY
    CPX #$03
    BNE :+
    CPY #$C0
    BEQ DoneLoadingWorld
:
    CPY #$00
    BNE LoadWorld
    INX
    INC world+1
    JMP LoadWorld

DoneLoadingWorld:
    LDX #$00

SetAttributes:
    LDA #$55
    STA $2007
    INX
    CPX #$40
    BNE SetAttributes

    LDX #$00
    LDY #$00    

    CLI

    LDA #%10010000 ; enable NMI change background to use second chr set of tiles ($1000)
    STA $2000
    ; Enabling sprites and background for left-most 8 pixels
    ; Enable sprites and background
    LDA #%00011110
    STA $2001


Loop:
	JMP Loop



GameLoop:
	LDX #$00

UPDATEENTITIES:
	LDA entities+Entity::type,x
	CMP #EntityType::Bullet
	BEQ BULLETUPDATE
	CMP #EntityType::PlayerEntity
	BEQ PlayerUpdate
	JMP NextEntity

BULLETUPDATE:
	JMP BULLETUPDATESTART

PlayerUpdate:
	LDA entities+Entity::direction
	AND #%00001111
	STA entities+Entity::direction
TopLeft:
	LDA entities+Entity::xpos
	STA tempX
	LDA entities+Entity::ypos
	STA tempY
	LDA #$00
	STA temp
	JSR Player_WorldCollision
	LDA temp
	CMP #$01
	BEQ HitTopLeft
TopRight:
	LDA entities+Entity::xpos
	CLC
	ADC #$10
	STA tempX
	LDA entities+Entity::ypos
	STA tempY
	LDA #$00
	STA temp
	JSR Player_WorldCollision
	LDA temp
	CMP #$01
	BEQ HitTopRight
BotLeft:
	LDA entities+Entity::xpos
	STA tempX
	LDA entities+Entity::ypos
	CLC
	ADC #$10
	STA tempY
	LDA #$00
	STA temp
	JSR Player_WorldCollision
	LDA temp
	CMP #$01
	BEQ HitBotLeft
BotRight:
	LDA entities+Entity::xpos
	CLC
	ADC #$10
	STA tempX
	LDA entities+Entity::ypos
	CLC
	ADC #$10
	STA tempY
	LDA #$00
	STA temp
	JSR Player_WorldCollision
	LDA temp
	CMP #$01
	BEQ HitBotRight
	JMP ResolveCollision


HitTopLeft:	
	LDA entities+Entity::direction
	ORA #%00010000
	STA entities+Entity::direction
	JMP TopRight
HitTopRight:	
	LDA entities+Entity::direction
	ORA #%00100000
	STA entities+Entity::direction	
	JMP BotLeft
HitBotLeft:	
	LDA entities+Entity::direction
	ORA #%01000000
	STA entities+Entity::direction
	JMP BotRight
HitBotRight:	
	LDA entities+Entity::direction
	ORA #%10000000
	STA entities+Entity::direction
	JMP ResolveCollision

ResolveCollision:
	LDA entities+Entity::direction
	AND #%11110000
	BEQ ExitResolve
	LDA entities+Entity::direction
	AND #%00000001
	BNE ResolveLeft
	LDA entities+Entity::direction
	AND #%00000010
	BNE ResolveRight
ResolveColVertical:
	LDA entities+Entity::direction
	AND #%00000100
	BNE ResolveUp

	LDA entities+Entity::direction
	AND #%00001000
	BNE ResolveDown

ExitResolve:
	JMP NextEntity

ResolveLeft:	
  LDA entities+Entity::xpos
  SEC 
  SBC #$01
  STA entities+Entity::xpos
  JMP ResolveColVertical
ResolveRight:	
  LDA entities+Entity::xpos
  CLC 
  ADC #$01
  STA entities+Entity::xpos
  JMP ResolveColVertical

ResolveUp:	
  LDA entities+Entity::ypos
  SEC 
  SBC #$01
  STA entities+Entity::ypos
  JMP ExitResolve
ResolveDown:	
  LDA entities+Entity::ypos
  CLC 
  ADC #$01
  STA entities+Entity::ypos
  JMP ExitResolve


Player_WorldCollision:	
	lda tempY
 	AND #%11111000
	ASL
	ROL world+1
	ASL
	ROL world+1
	CLC
	ADC #<WorldData
	STA world+0
	LDA world+1
	AND #%00000011
	ADC #>WorldData
	STA world+1
	LDA tempX
	LSR
	LSR
	LSR
	TAY
	LDA (world), y
	CMP #$00
	BNE WallHit
	RTS

WallHit:
	LDA #$01
	STA temp
	RTS

DESTROYENTITY:	
	LDA #$00
	STA entities+Entity::spritetile,x
	STA entities+Entity::type,x
	LDA #$FF
	STA entities+Entity::xpos, x
	STA entities+Entity::ypos, x	
	jmp NextEntity

BULLETUPDATESTART:
	LDA entities+Entity::direction,x	
  	AND #%00001000  
	BNE BULLET_MOVEUP
	LDA entities+Entity::direction,x	
  	AND #%00000100  
	BNE BULLET_MOVEDOWN
BulletHorizMovement:	 
	LDA entities+Entity::direction,x	
  	AND #%00000010  
	BNE BULLET_MOVERIGHT
	LDA entities+Entity::direction,x	
  	AND #%00000001  
	BNE BULLET_MOVELEFT
	JMP BULLET_COLLISION 	

BULLET_MOVERIGHT:
	LDA entities+Entity::xpos, x
	SEC
	SBC #$03
	BCC DESTROYENTITY
	STA entities+Entity::xpos,x		
	JMP BULLET_COLLISION
BULLET_MOVELEFT:
	LDA entities+Entity::xpos, x
	CLC
	ADC #$03
	BCS DESTROYENTITY
	STA entities+Entity::xpos,x
	JMP BULLET_COLLISION
BULLET_MOVEUP:
	LDA entities+Entity::ypos, x
	SEC
	SBC #$03
	BCC DESTROYENTITY
	STA entities+Entity::ypos,x
	JMP BulletHorizMovement
BULLET_MOVEDOWN:
	LDA entities+Entity::ypos, x
	CLC
	ADC #$03
	BCS DESTROYENTITY
	STA entities+Entity::ypos,x
	JMP BulletHorizMovement

BULLET_COLLISION:
	LDY #$00
BulletCheckLoop:
	LDA entities+Entity::type,y
	CMP #EntityType::Enemy
	BEQ EntityCollisionCheck
ContinueBulletLoop:
	TYA
	CLC
	ADC #.sizeof(Entity)
	TAY 
	CPY #MAXENTITIES
	BEQ Bullet_WorldCollision
	JMP BulletCheckLoop

Bullet_WorldCollision:	
	;get Y coordinate in pixels and keep only the relevant bits
	lda entities+Entity::ypos, x
 	AND #%11111000
	;multiply the Y coordinate by 4 (the equivalent of dividing by 8 and then multiplying by 32) and add it to the base address of the map
	ASL
	ROL world+1
	ASL
	ROL world+1
	CLC
	ADC #<WorldData
	STA world+0
	LDA world+1
	AND #%00000011
	ADC #>WorldData
	STA world+1

	;divide X coordinate by 8 to get the index
	LDA entities+Entity::xpos, x
	LSR
	LSR
	LSR
	TAY

	;get the tile from the map
	LDA (world), y
	CMP #$00
	BNE BulletHitWall
	JMP NextEntity

BulletHitWall:
	JMP DESTROYENTITY


BulletHitEntity:

	LDA #$00
	STA entities+Entity::spritetile,y
	STA entities+Entity::type,y
	LDA #$FF
	STA entities+Entity::xpos,y
	STA entities+Entity::ypos,y	
	JMP DESTROYENTITY


EntityCollisionCheck:
CheckXLow:
	LDA entities+Entity::xpos,y
	SEC
	SBC #$08
	CMP entities+Entity::xpos,x
	BCC CheckXHigh
	JMP ContinueBulletLoop
CheckXHigh:
	LDA entities+Entity::xpos,y
	CLC
	ADC #$10
	CMP entities+Entity::xpos,x
	BCS CheckYLow
	JMP ContinueBulletLoop
CheckYLow:
	LDA entities+Entity::ypos,y
	SEC
	SBC #$08
	CMP entities+Entity::ypos,x
	BCC CheckYHigh
	JMP ContinueBulletLoop
CheckYHigh:
	LDA entities+Entity::ypos,y
	CLC
	ADC #$10
	CMP entities+Entity::ypos,x
	BCS BulletHitEntity
	JMP ContinueBulletLoop

NextEntity:
	TXA
	CLC
	ADC #.sizeof(Entity)
	TAX 
	CPX #TOTALENTITIES
	BEQ ENDGAMELOOP
	JMP UPDATEENTITIES

ENDGAMELOOP:
	RTS





VBLANK:
	
	LDY #$00
	STY $07FF

	LDX #$00
	LDY #$00
	LDA #$00
	STA spritemem
	LDA #$02
	STA spritemem+1



DrawEntities:
	LDA entities+Entity::type, X
	CMP #EntityType::PlayerEntity
	BEQ PLAYERSPRITE
	CMP #EntityType::Enemy
	BEQ PLAYERSPRITE
	CMP #EntityType::Bullet
	BEQ PLAYERSPRITE
	JMP CHEKENDSPRITE



BULLETSPRITE:	
	LDA entities+Entity::ypos, x
	STA (spritemem),y
	INY
	LDA entities+Entity::spritetile,x
	STA (spritemem),y
	INY
	LDA #$00; Pallet
	STA (spritemem),y 
	INY
	LDA entities+Entity::xpos, x
	STA (spritemem),y
	INY
	jmp CHEKENDSPRITE

PLAYERSPRITE:
	LDA entities+Entity::ypos, x
	STA (spritemem),y
	INY
	LDA entities+Entity::spritetile,x
	STA (spritemem),y
	INY
	LDA #$00; Pallet
	STA (spritemem),y 
	INY
	LDA entities+Entity::xpos, x
	STA (spritemem),y
	INY

	LDA entities+Entity::ypos, x
	STA (spritemem),y
	INY
	LDA entities+Entity::spritetile,x
	CLC
	ADC #$01
	STA (spritemem),y
	INY
	LDA #$00; Pallet
	STA (spritemem),y 
	INY
	LDA entities+Entity::xpos, x
	CLC
	ADC #$08
	STA (spritemem),y
	INY

	LDA entities+Entity::ypos, x
	CLC
	ADC #$08
	STA (spritemem),y
	INY
	LDA entities+Entity::spritetile,x
	CLC
	ADC #$02
	STA (spritemem),y
	INY
	LDA #$00; Pallet
	STA (spritemem),y 
	INY
	LDA entities+Entity::xpos, x
	STA (spritemem),y
	INY

	LDA entities+Entity::ypos, x
	CLC
	ADC #$08
	STA (spritemem),y
	INY
	LDA entities+Entity::spritetile,x
	CLC
	ADC #$03
	STA (spritemem),y
	INY
	LDA #$00; Pallet
	STA (spritemem),y 
	INY
	LDA entities+Entity::xpos, x
	CLC
	ADC #$08
	STA (spritemem),y
	INY



CHEKENDSPRITE:
	TXA
	CLC
	ADC #.sizeof(Entity)
	TAX 
	CPX #TOTALENTITIES
	BEQ DONESPRITE
	JMP DrawEntities

DONESPRITE:
	LDA #$00
	STA $2003
	LDA #$02
	STA $4014
	LDA #$01 ;done with ppu
	STA $07FF

InitSprites:
	LDY #$00
	LDA #$FF
InitSpriteLoop:
	STA (spritemem),y
	INY
	EOR #$FF
	STA (spritemem),y
	INY
	STA (spritemem),y
	INY	
	EOR #$FF
	STA (spritemem),y
	INY
	BEQ ReadController
	JMP InitSpriteLoop


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Input

ReadController:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016
  LDX #$08


ReadControllerLoop:
  LDA $4016
  LSR A           
  ROL ControllerInput 
  DEX
  BNE ReadControllerLoop

InputHandling: 
	LDA ControllerInput
	AND #%00001111
	BNE SetDirection
	JMP ABTN

SetDirection:
	
	STA entities+Entity::direction

ABTN:
  LDA ControllerInput
  AND #%10000000  
  BEQ ABTNUP  

  LDA abtnLatch
  CMP #$01
  BEQ MoveLeft

  LDA #$01
  STA abtnLatch

  BNE SPAWNBULLET

ABTNUP:
  LDA #$00
  STA abtnLatch 

MoveLeft:
  LDA ControllerInput
  AND #%00000001  
  BEQ MoveRight
  
  LDA entities+Entity::xpos
  CLC
  ADC #$01
  STA entities+Entity::xpos

MoveRight:
  LDA ControllerInput
  AND #%00000010  
  BEQ MoveUp

  LDA entities+Entity::xpos
  SEC 
  SBC #$01
  STA entities+Entity::xpos

MoveUp:
  LDA ControllerInput
  AND #%00000100  
  BEQ MoveDown

  LDA entities+Entity::ypos
  CLC
  ADC #$01
  STA entities+Entity::ypos

MoveDown:
  LDA ControllerInput
  AND #%00001000  
  BEQ ReadInputDone

  LDA entities+Entity::ypos
  SEC 
  SBC #$01
  STA entities+Entity::ypos   
  JMP ReadInputDone

SPAWNBULLET:
	LDX #$00
EntitySpawnLoop:
	CPX #TOTALENTITIES-.sizeof(Entity)
	BEQ ReadInputDone
	LDA entities+Entity::type,x
	CMP #EntityType::NOEntity
	BEQ ADDBULLET
	TXA
	CLC
	ADC #.sizeof(Entity)
	TAX
	JMP EntitySpawnLoop
ADDBULLET:
	LDA entities+Entity::xpos
	CLC
	ADC bulletOffset
	STA entities+Entity::xpos,x
	LDA entities+Entity::ypos
	CLC
	ADC bulletOffset
	STA entities+Entity::ypos,x
 	

	INC bulletOffset
	INC bulletOffset
	LDA bulletOffset
	CMP #$08
	BEQ WrapBulletOffset
	JMP EndBullet
 
 WrapBulletOffset:
	LDA #$00
	STA bulletOffset


EndBullet:
	LDA #EntityType::Bullet
	STA entities+Entity::type,x
	LDA #$08
	STA entities+Entity::spritetile,x

	LDA entities+Entity::direction
	STA entities+Entity::direction,x


	JMP MoveLeft



ReadInputDone:

JSR GameLoop

RTI



PALETTEDATA:
  	.incbin "BGPallet.pal"
	.incbin "CharPallet.pal"

WorldData:
	.incbin "worldData.nam"


.segment "VECTORS"
	.word VBLANK 
	.word RESET
	.word 0

.segment "CHARS"
	.incbin "tiles.chr"