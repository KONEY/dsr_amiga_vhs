;*** CODE: KONEY ***
;*** MiniStartup by Photon ***
	INCDIR	"NAS:AMIGA/CODE/VHS_FX/"
	SECTION	"Code",CODE
	INCLUDE	"PhotonsMiniWrapper1.04!.s"
	INCLUDE	"custom-registers.i"
	;INCLUDE	"med/med_feature_control.i"	; MED CFGs
	;INCLUDE	"med/MED_PlayRoutine.i"
;********** Constants **********
wi	EQU 320
he	EQU 256		; screen height
bpls	EQU 6		; depth
bypl	EQU wi/16*2	; byte-width of 1 bitplane line (40bytes)
bwid	EQU bpls*bypl	; byte-width of 1 pixel line (all bpls)
;*******************************
;********** Demo **********	;Demo-specific non-startup code below.
Demo:			;a4=VBR, a6=Custom Registers Base addr
	;*--- init ---*
	MOVE.L	#VBint,$6C(A4)
	MOVE.W	#%1110000000100000,INTENA
	MOVE.W	#%1000001111100000,DMACON
	;*--- start copper ---*
	LEA	PLANE_0,A0
	LEA	COPPER\.BplPtrs,A1
	BSR.W	PokePtrs
	LEA	PLANE_1,A0
	LEA	COPPER\.BplPtrs+8,A1
	BSR.W	PokePtrs
	LEA	PLANE_2,A0
	LEA	COPPER\.BplPtrs+16,A1
	BSR.W	PokePtrs
	LEA	TEST_GRID,A0
	LEA	-40(A0),A0
	LEA	COPPER\.BplPtrs+24,A1
	BSR.W	PokePtrs
	LEA	TEST_GRID,A0
	LEA	COPPER\.BplPtrs+32,A1
	BSR.W	PokePtrs
	;LEA	PLANE_5,A0
	LEA	OVERLAY,A0
	LEA	COPPER\.BplPtrs+40,A1
	BSR.W	PokePtrs

	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC
	LEA	PLANE_0,A4	; FILLS A PLANE
	BSR.W	__FILLSOLID	; SOME DUMMY OPERATION...

	LEA	PLANE_1,A4	; FILLS A PLANE
	BSR.W	__FILLRND		; SOME DUMMY OPERATION...

	LEA	PLANE_2,A4	; FILLS A PLANE
	BSR.W	__FILLRND		; SOME DUMMY OPERATION...

	LEA	PLANE_5,A4	; FILLS A PLANE
	MOVE.L	#$500000F5,D0
	MOVE.L	#$AFFFFF0A,D1
	;MOVE.L	#$AAAAAAAA,D0
	;MOVE.L	#$55555555,D1
	;BSR.W	__SCANLINIZE_PLANE	; __TEXTURIZE_PLANE
	;MOVE.L	#$FFFFFFFF,(A4)

	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC

	; in photon's wrapper comment:;move.w d2,$9a(a6) ;INTENA
	;MOVE.W	#27,MED_START_POS	; skip to pos# after first block
	;JSR	_startmusic
	MOVE.L	#COPPER,COP1LC
;********************  main loop  ********************
MainLoop:	
	;BTST	#6,$BFE001	; POTINP - LMB pressed?
	;BNE.S	.skip
	;MOVE.W	#$0A0F,$DFF180	; show rastertime left down to $12c
	;;BSR.W	__RND
	;;MOVE.W	D5,NOISE_SEED_0
	;ADD.B	#2,NOISE_SEED_0
	;.skip:
	BSR.W	__HW_DISPLACE

	;CLR.W	$100		; DEBUG | w 0 100 2
	CLR.L	D5
	LEA	PLANE_1,A0
	MOVE.B	NOISE_SEED_0,D5
	BCLR	#0,D5
	ADD.L	D5,A0
	LEA	COPPER\.BplPtrs,A1
	BSR.W	PokePtrs
	ADD.B	#bypl,D5
	MOVE.B	D5,NOISE_SEED_0

	CLR.L	D5
	LEA	PLANE_2,A0
	MOVE.B	NOISE_SEED_1,D5
	BCLR	#0,D5
	ADD.L	D5,A0
	LEA	COPPER\.BplPtrs+8,A1
	BSR.W	PokePtrs
	MOVE.B	(A0),NOISE_SEED_1

	.WaitRasterCopper:
	;MOVE.W	#$0A0F,$DFF180	; show rastertime left down to $12c
	BTST	#$4,INTENAR+1
	BNE.S	.WaitRasterCopper
	;MOVE.W	#$0000,$DFF180	; show rastertime left down to $12c
	MOVE.W	#$8010,INTENA

	;*--- main loop end ---*
	;BTST	#6,$BFE001	; POTINP - LMB pressed?
	;BEQ.W	.exit
	BTST	#2,$DFF016	; POTINP - RMB pressed?
	BNE.W	MainLoop		; then loop
	;*--- exit ---*
	.exit:
	; ---  quit MED code  ---
	;MOVEM.L	D0-A6,-(SP)
	;JSR	_endmusic
	;MOVEM.L	(SP)+,D0-A6
	RTS

;********** Demo Routines **********
PokePtrs:				; SUPER SHRUNK REFACTOR
	MOVE.L	A0,-4(A0)		; Needs EMPTY plane to write addr
	MOVE.W	-4(A0),2(A1)	; high word of address
	MOVE.W	-2(A0),6(A1)	; low word of address
	RTS

VBint:				; Blank template VERTB interrupt
	movem.l	d0/a6,-(sp)	; Save used registers
	lea	$dff000,a6
	btst	#5,$1f(a6)	; check if it's our vertb int.
	beq.s	.notvb
	;*--- do stuff here ---*
	moveq	#$20,d0		; poll irq bit
	move.w	d0,$9c(a6)
	move.w	d0,$9c(a6)
	.notvb:	
	movem.l	(sp)+,d0/a6	; restore
	rte

__RANDOMIZE_PLANE:
	;SWAP	D5
	;MOVE.B	D7,D5
	MOVE.L	D5,D1
	MOVE.W	#8-1,D2
	.outerLoop:
	MOVE.W	#(bypl/2)*5-1,D4
	.innerLoop:
	;BSR.W	__RND
	ROR.L	D1
	MOVE.L	D1,D5
	NOT.L	D5
	EOR.B	D3,D5		; D3 contains semi-random value from RND
	ASR.W	D5
	ROL.L	D5
	EOR.W	D2,D5
	NOT.L	D5
	MOVE.L	D5,(A4)
	;MOVE.L	#-1,(A4)
	BTST	D4,D5
	BNE.S	.skip
	ROL.L	D1
	SWAP	D1
	.skip:
	LEA	2(A4),A4
	DBRA	D4,.innerLoop
	;LEA	-2(A4),A4
	MOVE.L	D5,(A4)+
	ROR.L	D1
	SWAP	D1
	DBRA	D2,.outerLoop
	;MOVE.L	D5,(A4)
	RTS

__RND:
	BSR	._word
	SWAP	D5
	._word:
	BSR	._byte2
	ROL.W	#$8,D5
	._byte:
	MOVE.B	$DFF007,D5	;$dff00a $dff00b for mouse pos
	MOVE.B	$BFD800,D3
	EOR.B	D3,D5
	BRA.S	.noByte2
	._byte2:
	MOVE.B	$DFF007,D3	;$dff00a $dff00b for mouse pos
	MOVE.B	$BFD800,D5
	OR.B	D3,D5
	.noByte2:
	RTS

__SCANLINIZE_PLANE:
	MOVE.W	#(bypl/4*he/16)-1,D7
	.outerLoop:
	MOVEM.L	D0-D1,(A4)
	LEA	8(A4),A4
	DBRA	D7,.outerLoop
	RTS

__FILLSOLID:
	MOVE.W	#he-1,D4		; QUANTE LINEE
	.outerloop:		; NUOVA RIGA
	CLR	D6
	MOVE.W	#bypl/2-1,D6	; RESET D6
	.innerloop:
	BSR.S	._RandomWord
	MOVE.W	#-1,(A4)+
	DBRA	D6,.innerloop
	DBRA	D4,.outerloop
	RTS
	._RandomWord:	
	BSR	._RandomByte
	ROL.W	#8,D5
	._RandomByte:	
	MOVE.B	$DFF007,D5 ;$dff00a $dff00b for mouse pos
	MOVE.B	$BFD800,D3
	EOR.B	D3,D5
	RTS

__FILLRND:
	MOVE.W	#he-1,D4		; QUANTE LINEE
	.outerloop:		; NUOVA RIGA
	CLR	D6
	MOVE.W	#bypl/2-1,D6	; RESET D6
	.innerloop:
	BSR.S	._RandomWord
	MOVE.W	D5,(A4)+
	DBRA	D6,.innerloop
	DBRA	D4,.outerloop
	RTS
	._RandomWord:	
	BSR	._RandomByte
	ROL.W	#8,D5
	._RandomByte:	
	MOVE.B	$DFF007,D5 ;$dff00a $dff00b for mouse pos
	MOVE.B	$BFD800,D3
	EOR.B	D3,D5
	RTS

__HW_DISPLACE:
	LEA	SCROLL_LFO1,A0
	MOVE.W	SCROLL_IDX,D0
	ADD.W	#$2,D0
	AND.W	#$3F-1,D0
	MOVE.W	D0,SCROLL_IDX
	CLR.L	D2
	MOVE.W	$DFF006,D4	; for bug?
	.waitVisibleRaster:
	MOVE.W	$DFF006,D4
	AND.W	#$FF00,D4		; read vertical beam
	CMP.W	#$3700,D4		; 2C
	BNE.S	.waitVisibleRaster

	.waitNextRaster:
	MOVE.W	$DFF006,D2
	AND.W	#$FF00,D2		; read vertical beam
	CMP.W	D4,D2
	BEQ.S	.waitNextRaster

	;CLR.L	D5
	;MOVE.W	D2,D4
	;MOVE.B	$DFF007,D5	; $dff00a $dff00b for mouse pos
	;MOVE.B	$BFD800,D1
	;EOR.B	D1,D5
	;MOVE.W	D5,BPLCON1
	MOVE.W	(A0,D0.W),BPLCON1	; 19DEA68E GLITCHA
	ADD.W	#$2,D0
	AND.W	#$3F-1,D0

	MOVE.W	$DFF004,D1	; Read vert most sig. bits
	BTST	#0,D1
	BEQ.S	.waitNextRaster

	BTST	#6,$BFE001	; POTINP - LMB pressed?
	BNE.S	.skip
	MOVE.B	D0,BPL1MOD
	MOVE.B	D0,BPL2MOD
	.skip:

	;CMP.W	#$0A00,D2		; DONT DISPLACE TXT
	;BGE.S	.dontSkip		; DONT DISPLACE TXT
	;;MOVE.W	#0,BPLCON1	; RESET REGISTER
	;;MOVE.L	#0,BPL1MOD	; RESET
	;
	;.dontSkip:
	CMP.W	#$2F00,D2		; 12.032
	BNE.S	.waitNextRaster

	;MOVE.W	#0,BPLCON1	; RESET REGISTER
	;MOVE.L	#0,$DFF108	; RESET
	RTS

FRAME_STROBE:	DC.B 0,0
NOISE_SEED_0:	DC.B 0
NOISE_SEED_1:	DC.B 0
SCROLL_IDX:	DC.W 0
SCROLL_LFO1:	DC.W 1,   1,   1,   2,   2,   2,   3,   4,   4,   5,   5,   6,   6,   7,   7,   7,   7,   7,   7,   6,   6,   5,   5,   4,   4,   3,   2,   2,   2,   1,   1,   1 

;FONT:		DC.L 0,0		; SPACE CHAR
;		INCBIN "c_font_leftpadding2.raw";,0
		EVEN
;TEXT:		INCLUDE "textscroller.i"

;*******************************************************************************
	SECTION	ChipData,DATA_C	;declared data that must be in chipmem
;*******************************************************************************
TEST_GRID:	INCBIN "VHS_GRID_TEST.raw"
OVERLAY:		INCBIN "H_BAR_TEST.raw"

;MED_MODULE:	INCBIN "med/RustEater_2022_FIX4.med"
;_chipzero:	DC.L 0
;_MED_MODULE:

COPPER:	; #### COPPERLIST ####################################################
	DC.W $1FC,0	; Slow fetch mode, remove if AGA demo.
	DC.W $8E,$2C81	; 238h display window top, left | DIWSTRT - 11.393
	DC.W $90,$2CC1	; and bottom, right.	| DIWSTOP - 11.457
	DC.W $92,$38	; Standard bitplane dma fetch start
	DC.W $94,$D0	; and stop for standard screen.
	DC.W $106,$0C00	; (AGA compat. if any Dual Playf. mode)
	.OddMod:
	DC.W $108,0	; BPL1MOD	 Bitplane modulo (odd planes)
	.EvenMod:
	DC.W $10A,0	; BPL2MOD Bitplane modulo (even planes)
	;.OddScroll:
	;DC.W $102,$00	; SCROLL REGISTER (AND PLAYFIELD PRI)

	.Palette:
	DC.W $0180,$0000,$0182,$0319,$0184,$031A,$0186,$031B
	DC.W $0188,$033B,$018A,$033C,$018C,$042D,$018E,$042E
	DC.W $0190,$034C,$0192,$055B,$0194,$045D,$0196,$056D
	DC.W $0198,$067D,$019A,$088D,$019C,$099D,$019E,$0CCD
	DC.W $01A0,$011C,$01A2,$0009,$01A4,$0119,$01A6,$011A
	DC.W $01A8,$000B,$01AA,$032A,$01AC,$00f1,$01AE,$00f0
	DC.W $01B0,$0449,$01B2,$044B,$01B4,$056A,$01B6,$066C
	DC.W $01B8,$067B,$01BA,$098B,$01BC,$0EED,$01BE,$0FFD

	.SpritePointers:
	DC.W $0120,0,$122,0	; 0
	DC.W $0124,0,$126,0	; 1
	DC.W $0128,0,$12A,0	; 2
	DC.W $012C,0,$12E,0	; 3
	DC.W $0130,0,$132,0	; 4
	DC.W $0134,0,$136,0	; 5
	DC.W $0138,0,$13A,0	; 6
	DC.W $013C,0,$13E,0	; 7

	.BplPtrs:
	DC.W $E0,0,$E2,0
	DC.W $E4,0,$E6,0
	DC.W $E8,0,$EA,0
	DC.W $EC,0,$EE,0
	DC.W $F0,0,$F2,0
	DC.W $F4,0,$F6,0		;full 6 ptrs, in case you increase bpls
	DC.W $100,bpls*$1000+$200	;enable bitplanes
	;DC.W $100,bpls*$1000+%011000000000

	; https://gradient-blaster.grahambates.com/?points=118@0,10c@53,30d@129,01d@198,00a@255&steps=256&blendMode=perceptual&ditherMode=shuffle&target=amigaOcs&shuffleCount=2
	Gradient:
	dc.w $182,$118
	dc.w $2f07,$fffe
	dc.w $182,$109
	dc.w $3007,$fffe
	dc.w $182,$118
	dc.w $3107,$fffe
	dc.w $182,$109
	dc.w $3207,$fffe
	dc.w $182,$118
	dc.w $3307,$fffe
	dc.w $182,$109
	dc.w $3407,$fffe
	dc.w $182,$118
	dc.w $3507,$fffe
	dc.w $182,$109
	dc.w $3b07,$fffe
	dc.w $182,$10a
	dc.w $3c07,$fffe
	dc.w $182,$109
	dc.w $3d07,$fffe
	dc.w $182,$10a
	dc.w $3e07,$fffe
	dc.w $182,$109
	dc.w $3f07,$fffe
	dc.w $182,$10a
	dc.w $4007,$fffe
	dc.w $182,$109
	dc.w $4107,$fffe
	dc.w $182,$10a
	dc.w $4807,$fffe
	dc.w $182,$10b
	dc.w $4907,$fffe
	dc.w $182,$10a
	dc.w $4a07,$fffe
	dc.w $182,$10b
	dc.w $5407,$fffe
	dc.w $182,$10c
	dc.w $5507,$fffe
	dc.w $182,$10b
	dc.w $5607,$fffe
	dc.w $182,$10c
	dc.w $5707,$fffe
	dc.w $182,$10b
	dc.w $5807,$fffe
	dc.w $182,$10c
	dc.w $5907,$fffe
	dc.w $182,$10b
	dc.w $5a07,$fffe
	dc.w $182,$10c
	dc.w $6d07,$fffe
	dc.w $182,$10d
	dc.w $7707,$fffe
	dc.w $182,$20d
	dc.w $7807,$fffe
	dc.w $182,$10d
	dc.w $7907,$fffe
	dc.w $182,$20d
	dc.w $a007,$fffe
	dc.w $182,$30d
	dc.w $a107,$fffe
	dc.w $182,$20d
	dc.w $a207,$fffe
	dc.w $182,$30d
	dc.w $b807,$fffe
	dc.w $182,$20d
	dc.w $d207,$fffe
	dc.w $182,$10d
	dc.w $d307,$fffe
	dc.w $182,$20d
	dc.w $d407,$fffe
	dc.w $182,$10d
	dc.w $d507,$fffe
	dc.w $182,$20d
	dc.w $d607,$fffe
	dc.w $182,$10d
	dc.w $d707,$fffe
	dc.w $182,$20d
	dc.w $d807,$fffe
	dc.w $182,$10d
	dc.w $e507,$fffe
	dc.w $182,$00d
	dc.w $e607,$fffe
	dc.w $182,$10d
	dc.w $e707,$fffe
	dc.w $182,$00d
	dc.w $e807,$fffe
	dc.w $182,$10d
	dc.w $e907,$fffe
	dc.w $182,$00d
	dc.w $ea07,$fffe
	dc.w $182,$10d
	dc.w $eb07,$fffe
	dc.w $182,$01d
	dc.w $fa07,$fffe
	dc.w $182,$00d
	dc.w $ff07,$fffe
	dc.w $182,$00c
	dc.w $ffdf,$fffe ; PAL fix
	dc.w $007,$fffe
	dc.w $182,$00d
	dc.w $107,$fffe
	dc.w $182,$00c
	dc.w $207,$fffe
	dc.w $182,$00d
	dc.w $307,$fffe
	dc.w $182,$00c
	dc.w $407,$fffe
	dc.w $182,$00d
	dc.w $507,$fffe
	dc.w $182,$00c
	dc.w $1107,$fffe
	dc.w $182,$00b
	dc.w $1207,$fffe
	dc.w $182,$00c
	dc.w $1307,$fffe
	dc.w $182,$00b
	dc.w $1407,$fffe
	dc.w $182,$00c
	dc.w $1507,$fffe
	dc.w $182,$00b
	dc.w $1607,$fffe
	dc.w $182,$00c
	dc.w $1707,$fffe
	dc.w $182,$00b
	dc.w $2307,$fffe
	dc.w $182,$00a
	dc.w $2407,$fffe
	dc.w $182,$00b
	dc.w $2507,$fffe
	dc.w $182,$00a
	dc.w $2607,$fffe
	dc.w $182,$00b
	dc.w $2707,$fffe
	dc.w $182,$00a
	dc.w $2807,$fffe
	dc.w $182,$00b
	dc.w $2907,$fffe
	dc.w $182,$00a

	;DC.W $FFDF,$FFFE		; allow VPOS>$ff
	DC.W $3507,$FF00		; ## RASTER END ## #$12C?
	DC.W $009A,$0010		; CLEAR RASTER BUSY FLAG
	DC.W $FFFF,$FFFE		; magic value to end copperlist

;*******************************************************************************
	SECTION ChipBuffers,BSS_C	;BSS doesn't count toward exe size
;*******************************************************************************
DUMMY_0:	DS.B bypl
PLANE_0:	DS.B he*bypl
PLANE_1:	DS.B he*bypl
DUMMY_2:	DS.B he/4*bypl
PLANE_2:	DS.B he*bypl
PLANE_3:	DS.B he*bypl
PLANE_4:	DS.B he*bypl
PLANE_5:	DS.B he*bypl

END
