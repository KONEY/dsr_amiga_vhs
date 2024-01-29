;*** CODE: KONEY ***
;*** MiniStartup by Photon ***
	INCDIR	"NAS:AMIGA/CODE/VHS_FX/"
	SECTION	"Code",CODE
	INCLUDE	"PhotonsMiniWrapper1.04!.s"
	INCLUDE	"custom-registers.i"
	;INCLUDE	"med/med_feature_control.i"	; MED CFGs
	;INCLUDE	"med/MED_PlayRoutine.i"
;********** Constants **********
wi		EQU 320
he		EQU 256		; screen height
bpls		EQU 6		; depth
bypl		EQU wi/16*2	; byte-width of 1 bitplane line (40bytes)
bwid		EQU bpls*bypl	; byte-width of 1 pixel line (all bpls)
;*******************************
DYNCOPPER		EQU 1
	IFNE DYNCOPPER
COP_WAITS		EQU 56
COP_FRAMES	EQU 39
COP_COLS_REGS	EQU 4
COP_BLIT_SIZE	EQU COP_COLS_REGS*2+2
	ENDC
;********** Demo **********	;Demo-specific non-startup code below.
Demo:			;a4=VBR, a6=Custom Registers Base addr
	;*--- init ---*
	MOVE.L	#VBint,$6C(A4)
	MOVE.W	#%1110000000100000,INTENA
	MOVE.W	#%1000001111100000,DMACON
	;*--- start copper ---*
	LEA	TEST_GRID,A0		; PF_1 OSD
	LEA	40(A0),A0
	LEA	COPPER\.BplPtrs,A1
	BSR.W	PokePtrs
	LEA	PLANE_1,A0		; PF_2 NOIZE
	LEA	COPPER\.BplPtrs+8,A1
	BSR.W	PokePtrs
	LEA	TEST_GRID,A0		; PF_1 OSD
	LEA	-40(A0),A0
	LEA	COPPER\.BplPtrs+16,A1
	BSR.W	PokePtrs
	LEA	PLANE_3,A0		; PF_2 NOIZE
	LEA	COPPER\.BplPtrs+24,A1
	BSR.W	PokePtrs
	LEA	TEST_GRID,A0		; PF_1 OSD
	LEA	COPPER\.BplPtrs+32,A1
	BSR.W	PokePtrs
	LEA	PLANE_5,A0		; PF_2 NOIZE
	LEA	COPPER\.BplPtrs+40,A1
	BSR.W	PokePtrs

	IFNE DYNCOPPER
	; #### EXTRACT COPPERLISTS  ######
	LEA	GRADIENT_VALS,A0
	LEA	COPPER_BUFFER,A1	; COPPER_BUFFER
	LEA	GRADIENT_REGISTERS,A3
	LEA	GRADIENT_PTRS,A4
	;LEA	(A4),A5
	;ADD.L	#COP_FRAMES*4-4,A5 ; A4 PTR START - A5 PTR STOP
	MOVE.W	#COP_FRAMES-1,D4
	.loop2:
	MOVE.L	A1,(A4)+
	;MOVE.L	A1,-(A5)
	BSR.W	__DECRUNCH_COPPERLIST
	DBRA	D4,.loop2
	;LEA	GRADIENT_VALS,A0	; INITIAL COPPER
	;LEA	COPPER\.Waits,A1
	;BSR.W	__DECRUNCH_COPPERLIST
	; #### EXTRACT COPPERLISTS  ######
	LEA	COPPER_BUFFER,A4
	LEA	COPPER\.Waits,A5
	BSR.W	__BLIT_GRADIENT_IN_COPPER
	ENDC

	MOVE.L	#COPPER,COP1LC	; ## POINT COPPERLIST ##
	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC	
	LEA	PLANE_1,A4	; FILLS A PLANE
	BSR.W	__FILLSOLID		; SOME DUMMY OPERATION...
	LEA	PLANE_3,A4	; FILLS A PLANE
	BSR.W	__FILLRND		; SOME DUMMY OPERATION...
	LEA	PLANE_5,A4	; FILLS A PLANE
	BSR.W	__FILLRND		; SOME DUMMY OPERATION...
	LEA	DUMMY_1,A4	; FILLS A PLANE
	BSR.W	__FILLSOLID	; SOME DUMMY OPERATION...
	BSR.W	__FILLSOLID	; SOME DUMMY OPERATION...

	;LEA	PLANE_5,A4	; FILLS A PLANE
	;MOVE.L	#$500000F5,D0
	;MOVE.L	#$AFFFFF0A,D1
	;MOVE.L	#$AAAAAAAA,D0
	;MOVE.L	#$55555555,D1
	;BSR.W	__SCANLINIZE_PLANE	; __TEXTURIZE_PLANE
	;MOVE.L	#$FFFFFFFF,(A4)
	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC

	; in photon's wrapper comment:;move.w d2,$9a(a6) ;INTENA
	;MOVE.W	#27,MED_START_POS	; skip to pos# after first block
	;JSR	_startmusic
;********************  main loop  ********************
MainLoop:	
	BTST	#6,$BFE001	; POTINP - LMB pressed?
	BNE.S	.skip
	;MOVE.W	#$0A0F,$DFF180	; show rastertime left down to $12c
	;;BSR.W	__RND
	.skip:
	TST.B	FRAME_STROBE
	BNE.W	.oddFrame
	MOVE.B	#1,FRAME_STROBE
	LEA	PLANE_3,A0
	LEA	PLANE_5,A2
	LEA	COPPER\.BplPtrs+40,A1
	BRA.W	.evenFrame
	.oddFrame:
	MOVE.B	#0,FRAME_STROBE
	LEA	PLANE_3,A2
	LEA	PLANE_5,A0
	LEA	COPPER\.BplPtrs+24,A1
	.evenFrame:
	CLR.L	D5
	MOVE.B	NOISE_SEED_1,D5
	BCLR	#0,D5
	ADD.L	D5,A0
	BSR.W	PokePtrs
	ADD.B	#bypl*2,D5
	ADD.L	D5,A2
	MOVE.B	(A2),NOISE_SEED_1

	BSR.W	__HW_DISPLACE

	IFNE DYNCOPPER
	BSR.W	__BLIT_GRADIENT_IN_COPPER
	ENDC

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

	IFNE DYNCOPPER
__DECRUNCH_COPPERLIST:
	MOVE.W	#COP_WAITS,D7
	.loop:
	TST.W	(A0)		; ZEROED WORD = allow VPOS>$ff
	BNE.S	.notFF
	MOVE.L	#$FFDFFFFE,(A1)+	; allow VPOS>$ff
	LEA	2(A0),A0		; NEXT
	BRA.S	.skip
	.notFF:
	MOVE.B	(A0)+,D0		; FIRST WAIT
	LSL.W	#8,D0
	MOVE.B	#$07,D0		; CMD RESTORED $1C07
	MOVE.W	D0,(A1)+		; WAIT
	MOVE.W	#$FFFE,(A1)+	; WAIT
	CLR.L	D1
	MOVE.B	(A0),D1		; BYTE FOR COLOR

	;MOVE.B	D6,D1		; FOR RED VALUE
	;ADD.B	D7,D1
	;SUB.B	D6,D1
	LSR.W	#2,D1		; EXTEND FIRST NIBBLE
	MOVE.B	(A0)+,D1		; FOR RED VALUE
	LSL.B	D1

	MOVE.W	#COP_COLS_REGS-1,D6
	.innerLoop:
	LSL.W	D6		; ONLY EVEN VALUES
	MOVE.W	(A3,D6.W),(A1)+	; COLOR REGISTER
	LSR.W	D6		; GO BACK TO COUNTER
	LSR.B	D1
	MOVE.W	D1,(A1)+		; COLOR VALUE
	DBRA	D6,.innerLoop
	.skip:
	DBRA	D7,.loop
	RTS
	ENDC
	IFNE DYNCOPPER
__BLIT_GRADIENT_IN_COPPER:
	MOVE.W	GRADIENT_INDEX,D0
	LEA	GRADIENT_PTRS,A3
	MOVE.L	(A3,D0.W),A4
	ADD.W	#$4,D0
	CMP.W	#COP_FRAMES*4-4,D0
	BLO.S	.dontReset
	MOVE.W	#$0,D0
	.dontReset:
	MOVE.W	D0,GRADIENT_INDEX
	LEA	COPPER\.Waits,A5
	MOVE.L	(A4)+,(A5)+	; Trick for alignment ;)
	_WaitBlitterNasty
	MOVE.L	#$FFFFFFFF,BLTAFWM
	MOVE.L	#(%0000100111110000<<16),BLTCON0
	MOVE.W	#0,BLTAMOD
	MOVE.W	#0,BLTDMOD
	MOVE.L	A4,BLTAPTH
	MOVE.L	A5,BLTDPTH
	MOVE.W	#(COP_WAITS<<6)+COP_BLIT_SIZE,BLTSIZE
	RTS
	ENDC

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
	MOVE.B	$DFF007,D5	;$dff00a $dff00b for mouse pos
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
	MOVE.B	$DFF007,D5	;$dff00a $dff00b for mouse pos
	MOVE.B	$BFD800,D3
	EOR.B	D3,D5
	RTS

__HW_DISPLACE:
	LEA	LFO_SINE_1,A0
	LEA	LFO_SINE_2,A1
	MOVE.W	SCROLL_IDX,D0
	ADD.W	#$2,D0
	AND.W	#$3F-1,D0
	MOVE.W	D0,SCROLL_IDX
	MOVE.W	BPLMOD_IDX,D5
	AND.W	#$7F-1,D5
	SUB.W	#$2,D5
	MOVE.W	D5,BPLMOD_IDX
	CLR.L	D2
	MOVE.W	VHPOSR,D4		; for bug?
	.waitVisibleRaster:
	MOVE.W	VHPOSR,D4
	AND.W	#$FF00,D4		; read vertical beam
	CMP.W	#$3700,D4		; 2C
	BNE.S	.waitVisibleRaster

	.waitNextRaster:
	CLR.W	$100		; DEBUG | w 0 100 2
	MOVE.W	VHPOSR,D2
	AND.W	#$FF00,D2		; read vertical beam
	CMP.W	D4,D2
	BEQ.S	.waitNextRaster

	MOVE.W	VHPOSR,D4
	AND.W	#$FF00,D4		; read vertical beam

	;CLR.L	D5
	;MOVE.W	D2,D4
	;MOVE.B	$DFF007,D5	; $dff00a $dff00b for mouse pos
	;MOVE.B	$BFD800,D1
	;EOR.B	D1,D5
	;MOVE.W	D5,BPLCON1

	BTST	#6,$BFE001	; POTINP - LMB pressed?
	BNE.W	.skip
	;MOVE.W	#$0EEE,$DFF192	; WHITE NOISELINE ?
	;MOVE.B	D0,DDFSTRT
	;MOVE.B	D0,DDFSTOP
	;MOVE.W	D0,BPL1MOD
	MOVE.W	D5,BPL2MOD
	LEA	LFO_NOISE,A0
	;MOVE.W	#(bpls-1)*$1000+$A00,BPLCON0	; HAM!?!
	;MOVE.L	#$0F1F010E,$DFF182		; HAM + EHB
	;MOVE.W	#bpls*$1000+$200,BPLCON0	; EHB!?!	
	;MOVE.L	#$00E00F0F,$DFF186		; EHB
	;MOVE.L	#$0F0F0A1A,$DFF18A		; EHB
	.skip:

	MOVE.W	(A0,D0.W),D3	; 19DEA68E GLITCHA
	;ROR.L	#4,D3
	;MOVE.W	(A1,D0.W),D3	; 19DEA68E GLITCHA
	;ROL.L	#4,D3
	MOVE.W	D3,BPLCON1	; 19DEA68E GLITCHA
	ADD.W	#$2,D0
	AND.W	#$3F-1,D0

	MOVE.W	VPOSR,D1		; Read vert most sig. bits
	BTST	#0,D1
	BEQ.W	.waitNextRaster

	;CMP.W	#$0A00,D2		; DONT DISPLACE TXT
	;BGE.S	.dontSkip		; DONT DISPLACE TXT
	;;MOVE.W	#0,BPLCON1	; RESET REGISTER
	;;MOVE.L	#0,BPL1MOD	; RESET
	;
	;.dontSkip:
	CMP.W	#$0400,D2		; 12.032 - #$2F00
	BNE.W	.waitNextRaster
	;MOVE.W	#$0FF0,$DFF180	; show rastertime left down to $12c

	;MOVE.W	#0,BPLCON1	; RESET REGISTER
	;MOVE.L	#0,BPL1MOD	; RESET
	RTS

FRAME_STROBE:	DC.B 0,0
NOISE_SEED_0:	DC.B 0
NOISE_SEED_1:	DC.B 0
BPLMOD_IDX:	DC.W 0
SCROLL_IDX:	DC.W 0
LFO_SINE_1:	DC.W 1,1,1,2,2,2,3,4,4,5,5,6,6,7,7,7,7,7,7,6,6,5,5,4,4,3,2,2,2,1,1,1
LFO_SINE_2:	DC.W 5,5,4,5,5,4,5,4,5,5,4,4,3,2,3,2,1,0,0,0,1,0,1,2,2,3,4,4,5,4,5,4
LFO_NOISE:	DC.W 1,4,1,5,2,4,3,5,2,4,2,5,2,4,1,5,1,4,1,5,3,4,2,5,1,4,5,5,4,4,6,5 

	IFNE DYNCOPPER
	GRADIENT_REGISTERS:	DC.W $0192,$0196,$019A,$019E
	GRADIENT_INDEX:	DC.W 0
	GRADIENT_VALS:	INCLUDE "CopGradients.i"
	ENDC

;FONT:		DC.L 0,0		; SPACE CHAR
;		INCBIN "c_font_leftpadding2.raw";,0
		EVEN
;TEXT:		INCLUDE "textscroller.i"
;*******************************************************************************
	SECTION	ChipData,DATA_C	;declared data that must be in chipmem
;*******************************************************************************
		DS.B he*8		; For PointPtr...
TEST_GRID:	INCBIN "VHS_GRID_TEST.raw"
		DS.B he*bypl		; For PointPtr...

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
	DC.W $0180,$0000,$0182,$00D1,$0184,$0F0E,$0186,$000F
	DC.W $0188,$0FFF,$018A,$0DEF,$018C,$0DDD,$018E,$0CDE

	DC.W $0190,$0000,$0192,$013E,$0194,$033D,$0196,$032F
	DC.W $0198,$022E,$019A,$000F,$019C,$002C,$019E,$031F

	DC.W $01A0,$0F0F,$01A2,$0CCC,$01A4,$0048,$01A6,$0048
	DC.W $01A8,$0158,$01AA,$025C,$01AC,$014E,$01AE,$0FFF
	DC.W $01B0,$045D,$01B2,$005A,$01B4,$004F,$01B6,$004F
	DC.W $01B8,$0DDD,$01BA,$00F0,$01BC,$0CCC,$01BE,$0BAB

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
	DC.W $100,bpls*$1000+$600	;enable bitplanes
	;DC.W $104,%0000000001000000	; BPLCON2

	IFNE DYNCOPPER
	.Waits:
	DS.W COP_BLIT_SIZE*COP_WAITS+2	; +2 vpos >$FF
	ENDC

	IFEQ DYNCOPPER
	DC.W $FFDF,$FFFE		; allow VPOS>$ff
	ENDC
	DC.W $3507,$FF00		; ## RASTER END ## #$12C?
	DC.W $009A,$0010		; CLEAR RASTER BUSY FLAG
	DC.W $FFFF,$FFFE		; magic value to end copperlist

;*******************************************************************************
	SECTION ChipBuffers,BSS_C	;BSS doesn't count toward exe size
;*******************************************************************************
	IFNE DYNCOPPER
	GRADIENT_PTRS:	DS.L COP_FRAMES
	COPPER_BUFFER:	DS.W COP_FRAMES*(COP_BLIT_SIZE*COP_WAITS+2)	; +2 vpos >$FF
	ENDC
DUMMY_0:		DS.B he/4*bypl
PLANE_1:		DS.B he*bypl
PLANE_3:		DS.B he*bypl
PLANE_5:		DS.B he*bypl
DUMMY_1:		DS.B he*2*bypl

END
