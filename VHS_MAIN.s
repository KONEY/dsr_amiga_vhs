;*** CODE: KONEY ***
;*** MiniStartup by Photon ***
	INCDIR	"NAS:AMIGA/CODE/VHS_FX/"
	SECTION	"Code",CODE
	INCLUDE	"PhotonsMiniWrapper1.04!.s"
	INCLUDE	"custom-registers.i"
	INCLUDE	"med/med_feature_control.i"	; MED CFGs
	INCLUDE	"med/MED_PlayRoutine.i"
;********** Constants **********
wi		EQU 320
he		EQU 256		; screen height
bpls		EQU 6		; depth
bypl		EQU wi/16*2	; byte-width of 1 bitplane line (40bytes)
bwid		EQU bpls*bypl	; byte-width of 1 pixel line (all bpls)
;*******************************
FONT_W		EQU 10
FONT_H		EQU 16
FONT_PAD		EQU 2
FONT_OFFSET	EQU 16-FONT_W-FONT_PAD
FONT_SCROLL	EQU FONT_W+FONT_PAD
LINE_H		EQU FONT_H*bypl
CHARS_PER_LINE	EQU wi/(FONT_W+FONT_PAD)
;*******************************
EPILEPSY		EQU 1
DYNCOPPER		EQU 1
	IFNE DYNCOPPER
COP_WAITS		EQU 42
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
	LEA	TXT_GRID,A0	; PF_1 OSD
	LEA	COPPER\.BplPtrs+2,A1
	BSR.W	PokePtrs
	LEA	PLANE_1,A0	; PF_2 NOIZE (FILLED)
	LEA	COPPER\.BplPtrs+2+8,A1
	BSR.W	PokePtrs
	LEA	TXT_GRID,A0	; PF_1 OSD
	LEA	COPPER\.BplPtrs+2+16,A1
	BSR.W	PokePtrs
	LEA	PLANE_3,A0	; PF_2 NOIZE
	LEA	COPPER\.BplPtrs+2+24,A1
	BSR.W	PokePtrs
	LEA	TXT_GRID,A0	; PF_1 OSD
	LEA	COPPER\.BplPtrs+2+32,A1
	BSR.W	PokePtrs
	LEA	PLANE_5,A0	; PF_2 NOIZE
	LEA	COPPER\.BplPtrs+2+40,A1
	BSR.W	PokePtrs

	IFNE DYNCOPPER
	; #### EXTRACT COPPERLISTS  ######
	LEA	GRADIENT_VALS,A0
	LEA	COPPER_BUFFER,A1	; COPPER_BUFFER
	LEA	GRADIENT_REGISTERS,A3
	LEA	GRADIENT_PTRS,A4
	MOVE.W	#COP_FRAMES-1,D4
	LEA	BLUE_COLS,A6
	.loop2:
	MOVE.L	A1,(A4)+
	BSR.W	__DECRUNCH_COPPERLIST
	DBRA	D4,.loop2
	BSR.W	__UpdateAllCopJmps
	ENDC

	IFNE DYNCOPPER
	;;MOVE.L	#COPPER\.returnPoint,D0	; LOAD ADDRESS OF OUR SCREEN IN D0
	;;LEA	COPPER,A0
	;;BSR.W	_PokeCopJmp
	;;MOVE.L	 #COPPER,D0		; LOAD ADDRESS OF OUR SCREEN IN D0
	;;LEA	COPPER\.returnPoint,A0
	;;BSR.W	_PokeCopJmp
	;MOVE.L	 #COPPER3,D0		; LOAD ADDRESS OF OUR SCREEN IN D0
	;LEA	COPPER2,A0
	;BSR.W	_PokeCopJmp
	;MOVE.L	 #COPPER2,D0		; LOAD ADDRESS OF OUR SCREEN IN D0
	;LEA	COPPER3,A0
	;BSR.W	_PokeCopJmp
	;MOVE.L	#COPPER2,COP2LC		; ## POINT COPPERLIST ##
	ENDC

	MOVE.L	#COPPER,COP1LC	; ## POINT COPPERLIST ##
	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC	
	LEA	PLANE_1,A4	; FILLS A PLANE
	;BSR.W	__SCANLINIZE_PLANE
	BSR.W	__FILLSOLID	; SOME DUMMY OPERATION...
	MOVE.L	#$F0F0F0F0,(A4)+	; MASK?
	MOVE.L	#$A5A5A5A5,(A4)+	; MASK?
	MOVE.L	#$00A000AA,(A4)+	; MASK?
	MOVE.L	#$FEDCBA98,(A4)+	; MASK?
	ADD.L	#bypl,A4		; LEAVE EMPTY LINES FOR SCAN FX
	MOVE.L	#$FFFF0000,(A4)+	; MASK?
	MOVE.L	#$F0F0F0F0,(A4)+	; MASK?
	MOVE.L	#$0000FFFF,(A4)+	; MASK?
	ADD.L	#bypl*2,A4	; LEAVE EMPTY LINES FOR SCAN FX
	MOVE.L	#$FEDCBA98,(A4)+	; MASK?
	MOVE.L	#$0000FFFF,(A4)+	; MASK?
	MOVE.L	#$F0F0F0F0,(A4)+	; MASK?
	MOVE.L	#$00A000AA,(A4)+	; MASK?
	MOVE.L	#$0000FFFF,(A4)+	; MASK?
	BSR.W	__FILLSOLID	; SOME DUMMY OPERATION...
	LEA	PLANE_3,A4	; FILLS A PLANE
	BSR.W	__FILLRND		; SOME DUMMY OPERATION...
	BSR.W	__FILLRND		; SOME DUMMY OPERATION...
	LEA	PLANE_5,A4	; FILLS A PLANE
	BSR.W	__FILLRND		; SOME DUMMY OPERATION...
	BSR.W	__FILLRND		; SOME DUMMY OPERATION...
	LEA	PLANE_5,A4	; FILLS A PLANE
	BSR.W	__PXLX2_PLANE	; SOME DUMMY OPERATION...
	BSR.W	__PXLX2_PLANE	; SOME DUMMY OPERATION...
	LEA	DUMMY_1,A4	; FILLS A PLANE
	BSR.W	__FILLRND		; SOME DUMMY OPERATION...
	;BSR.W	__FILLRND		; SOME DUMMY OPERATION...

	; ### PREFILLS ###############
	MOVE.W	#$0,TXT_IDX
	MOVE.W	#CHARS_PER_LINE-1,D7
	.loop3:
	LEA	LINE_BUF,A4
	BSR.W	__SCROLL_X
	LEA	TXT_IDX,A0
	LEA	TXT_HEADER,A3
	LEA	LINE_BUF,A4
	BSR.W	__FETCH_LETTER
	BSR.W	__BLIT_LETTER
	DBRA	D7,.loop3
	LEA	LINE_BUF,A5
	MOVE.L	VPOS_HEADER,A4
	BSR.W	__BLIT_LINE
	LEA	LINE_BUF,A5
	MOVE.L	VPOS_HEADER,A4
	ADD.L	#bypl,A5
	ADD.L	#bypl,A4
	BSR.W	__BLIT_LINE
	; ### PREFILLS ###############
	MOVE.W	#$0,TXT_IDX
	MOVE.W	#CHARS_PER_LINE-1,D7
	.loop5:
	LEA	LINE_BUF,A4
	BSR.W	__SCROLL_X
	LEA	TXT_IDX,A0
	LEA	TXT_FOOTER,A3
	LEA	LINE_BUF,A4
	BSR.W	__FETCH_LETTER
	BSR.W	__BLIT_LETTER
	DBRA	D7,.loop5
	LEA	LINE_BUF,A5
	MOVE.L	VPOS_FOOTER,A4
	BSR.W	__BLIT_LINE
	LEA	LINE_BUF,A5
	MOVE.L	VPOS_FOOTER,A4
	ADD.L	#bypl,A5
	ADD.L	#bypl,A4
	BSR.W	__BLIT_LINE
	; ### PREFILLS ###############
	MOVE.W	#$0,TXT_IDX
	MOVE.W	#CHARS_PER_LINE-1,D7
	.loop4:
	LEA	LINE_BUF,A4
	BSR.W	__SCROLL_X
	LEA	TXT_IDX,A0
	LEA	TXT_BODY,A3
	LEA	LINE_BUF,A4
	BSR.W	__FETCH_LETTER
	BSR.W	__BLIT_LETTER
	DBRA	D7,.loop4
	LEA	LINE_BUF,A5
	MOVE.L	VPOS_BODY,A4
	BSR.W	__BLIT_LINE
	LEA	LINE_BUF,A5
	MOVE.L	VPOS_BODY,A4
	ADD.L	#bypl,A5
	ADD.L	#bypl,A4
	BSR.W	__BLIT_LINE
	ADD.W	#$1,(A0)
	; ### PREFILLS ###############

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
	JSR	_startmusic
;********************  main loop  ********************
MainLoop:	
	IFNE EPILEPSY
	BSR.W	__UPDATE_V_LINE
	ENDC

	;BTST	#6,$BFE001		; POTINP - LMB pressed?
	;BNE.S	.skip
	;MOVE.W	#0,NOISE_IDX5
	;.skip:

	;* FOR TIMED EVENTS ON BLOCK ****
	MOVE.W	TXT_TIMELINE_IDX,D5
	LEA	TXT_TIMELINE,A3
	MOVE.L	(A3,D5),A4		; THANKS HEDGEHOG!!
	ADD.W	#$4,D5
	MOVE.W	D5,TXT_TIMELINE_IDX
	JSR	(A4)			; EXECUTE SUBROUTINE BLOCK#

	LEA	BLUE_COLS,A6
	MOVE.W	-2(A6),D0			; IDX

	BCHG	#$1,FRAME_STROBE
	BNE.W	.oddFrame			; #############
		
	;MOVE.L	#$000F030A,$DFF182		; BLUE
	MOVE.L	#$0F0000FF,$DFF182		; TIKTOK

	LEA	COPPER\.BplPtrs+2,A1	; VERTICAL TXT
	MOVE.W	V_IDX_1,D0		; VERTICAL TXT
	BSR.W	__V_DISPLACE		; VERTICAL TXT
	MOVE.W	D0,V_IDX_1		; VERTICAL TXT

	; ## RANDOMIZE ADDRESS POINTS OF NOISE PLANES ##
	LEA	NOISE_IDX3,A2
	LEA	PLANE_3,A0
	;MOVE.W	#bypl/3,D1		; OFFSET
	;MOVE.W	#bypl/4,D2		; SUBSTR
	MOVE.W	#bypl*2*2+12,D1		; OFFSET
	MOVE.W	#bypl*2,D2		; SUBSTR
	BSR.W	__UPDT_BPL_PTR
	MOVE.L	A0,NOISE_UPD_PLANE3
	; ## RANDOMIZE ADDRESS POINTS OF NOISE PLANES ##

	BRA.W	.evenFrame
	.oddFrame:			; #############

	;MOVE.L	#$010A000E,$DFF182		; BLUE
	MOVE.L	#$0F0700F5,$DFF182		; G+P

	LEA	COPPER\.BplPtrs+2+16,A1	; VERTICAL TXT
	MOVE.W	V_IDX_2,D0		; VERTICAL TXT
	BSR.W	__V_DISPLACE		; VERTICAL TXT
	MOVE.W	D0,V_IDX_2		; VERTICAL TXT

	; ## RANDOMIZE ADDRESS POINTS OF NOISE PLANES ##
	LEA	NOISE_IDX5,A2
	LEA	PLANE_5,A0
	MOVE.W	#bypl*2*2+12,D1		; OFFSET
	MOVE.W	#bypl*3+2,D2		; SUBSTR
	BSR.W	__UPDT_BPL_PTR
	MOVE.L	A0,NOISE_UPD_PLANE5
	; ## RANDOMIZE ADDRESS POINTS OF NOISE PLANES ##

	; ## UPDATE POINTERS ##
	MOVE.L	NOISE_COP_PTR3,A1
	MOVE.L	RELOC3,A5
	MOVE.L	(A5),A0
	IFNE EPILEPSY
	BSR.W	PokePtrs
	ENDC
	MOVEM.L	NOISE_COP_PTR3(PC),A0-A1
	EXG	A0,A1
	MOVEM.L	A0-A1,NOISE_COP_PTR3
	; ## SWAP FOR NEXT ####
	MOVEM.L	RELOC3(PC),A0-A1
	EXG	A0,A1
	MOVEM.L	A0-A1,RELOC3
	; ## UPDATE POINTERS ##

	.evenFrame:			; ###########

	BSR.W	__RACE_BEAM

	IFNE DYNCOPPER
	BSR.W	__BLIT_GRADIENT_IN_COPPER
	ENDC

	;.WaitRasterCopper:
	;;MOVE.W	#$0A0F,$DFF180	; show rastertime left down to $12c
	;BTST	#$4,INTENAR+1
	;BNE.S	.WaitRasterCopper
	;;MOVE.W	#$0000,$DFF180	; show rastertime left down to $12c
	;MOVE.W	#$8010,INTENA

	;*--- main loop end ---*
	;BTST	#6,$BFE001	; POTINP - LMB pressed?
	;BEQ.W	.exit
	BTST	#2,$DFF016	; POTINP - RMB pressed?
	BNE.W	MainLoop		; then loop
	;*--- exit ---*
	.exit:
	; ---  quit MED code  ---
	MOVEM.L	D0-A6,-(SP)
	JSR	_endmusic
	MOVEM.L	(SP)+,D0-A6
	RTS

;********** Demo Routines **********
PokePtrs:				; EVEN SHRUNKER REFACTOR! :)
	MOVE.L	A0,(A0)		; Needs EMPTY plane to write addr
	MOVE.W	(A0),(A1)		; high word of address
	MOVE.W	A0,4(A1)		; low word of address
	CLR.L	(A0)		; Clear the inital mess?
	RTS

VBint:				; Blank template VERTB interrupt
	MOVEM.L	D0/A6,-(SP)	; SAVE USED REGISTERS
	LEA	$DFF000,A6
	BTST	#5,$1F(A6)	; CHECK IF IT'S OUR VERTB INT.
	BEQ.S	.notVB
	;*--- DO STUFF HERE ---*
	MOVEQ	#$20,D0		; POLL IRQ BIT
	MOVE.W	D0,$9C(A6)
	MOVE.W	D0,$9C(A6)
	.notVB:	
	MOVEM.L	(SP)+,D0/A6	; RESTORE
	RTE

_WipeMEM:		; a1=screen destination address to clear
	BSR	WaitBlitter
	MOVE.L	#$0,BLTAFWM		; BLTAFWM
	MOVE.W	#bypl,BLTDMOD		; for HALF with lines.
	MOVE.W	#$100,BLTCON0		; set operation type in BLTCON0/1
	MOVE.W	#$000,BLTCON1
	MOVE.L	A4,BLTDPTH		; destination address
	MOVE.W	#FONT_H*64+wi/16,BLTSIZE	; Start Blitter (Blitsize)
	RTS

	IFNE DYNCOPPER
_PokeCopJmp:
	; D0 = src A0=dest
	MOVE.W	 D0,6(A0)			; LOAD LOW SCR ADDR IN COPPER LIST
	SWAP  	 D0			; SWAP ADDRESS IN D0
	MOVE.W	 D0,2(A0)			; LOAD HIGH SCR ADDR IN COPPER LIST
	RTS
__DECRUNCH_COPPERLIST:
	; GRADIENT_VALS,A0 		COPPER_BUFFER,A1
	; GRADIENT_REGISTERS,A3	LEA GRADIENT_PTRS,A4
	MOVE.W	-2(A6),D5		; IDX
	ADD.W	-4(A6),D5		; OFFSET
	MOVE.W	#COP2LCH,(A1)+	; TO-DO: ADD H-L POINTERS
	MOVE.W	#$FABE,(A1)+	; TO-DO: ADD H-L POINTERS
	MOVE.W	#COP2LCL,(A1)+	; TO-DO: ADD H-L POINTERS
	MOVE.W	#$DEBA,(A1)+	; TO-DO: ADD H-L POINTERS
	MOVE.W	#COP_WAITS,D7
	.waitsLoop:
	TST.B	(A0)+		; ZEROED WORD = allow VPOS>$ff
	BNE.S	.notFF
	MOVE.L	#$FFDFFFFE,(A1)+	; allow VPOS>$ff
	MOVE.B	(A0)+,D0		; DUMMY
	BRA.S	.skip
	.notFF:
	MOVE.B	(A0)+,D0		; FIRST WAIT
	LSL.W	#8,D0
	MOVE.B	#$07,D0		; CMD RESTORED $1C07
	MOVE.W	D0,(A1)+		; WAIT
	MOVE.W	#$FFFE,(A1)+	; WAIT
	MOVE.W	#COP_COLS_REGS-1,D6
	.registersLoop:
	LSL.W	D6		; ONLY EVEN VALUES
	MOVE.W	(A3,D6.W),(A1)+	; COLOR REGISTER
	MOVE.W	(A6,D5.W),(A1)+	; COLOR VALUE
	ADD.W	#$2,D5		; GLOBAL COLOR IDX
	LSR.W	D6		; GO BACK TO COUNTER
	DBRA	D6,.registersLoop
	;SUB.W	#(COP_COLS_REGS-1)*2,D5
	SUB.W	#$2,D5		; GLOBAL COLOR IDX
	AND.W	#$FF,D5		; RESET
	MOVE.W	D5,-2(A6)		; SAVE
	.skip:
	DBRA	D7,.waitsLoop
	ADD.W	#$8,-4(A6)
	MOVE.L	#$FFFFFFFE,(A1)+	; END COP
	RTS
__UpdateAllCopJmps:
	LEA	GRADIENT_PTRS,A1
	MOVE.L	(A1),D1
	MOVE.W	#COP_FRAMES-1,D4
	.loop2:
	MOVE.L	(A1),A2
	MOVE.L	(A1)+,A0
	MOVE.L	(A1),D0
	BSR.W	_PokeCopJmp
	DBRA	D4,.loop2
	MOVE.L	D1,D0
	BSR.W	_PokeCopJmp
	MOVE.L	D1,COP2LC
	RTS
	ENDC
	IFNE DYNCOPPER
__BLIT_GRADIENT_IN_COPPER:
	;MOVE.W	GRADIENT_INDEX,D0
	;LEA	GRADIENT_PTRS,A3
	;MOVE.L	(A3,D0.W),A4
	;ADD.W	#$4,D0
	;CMP.W	#COP_FRAMES*4-4,D0
	;BLO.S	.dontReset
	;MOVE.W	#$0,D0
	;.dontReset:
	;MOVE.W	D0,GRADIENT_INDEX
	;LEA	COPPER\.Waits,A5
	;MOVE.L	(A4)+,(A5)+	; Trick for alignment ;)
	;_WaitBlitterNasty
	;MOVE.L	#$FFFFFFFF,BLTAFWM
	;MOVE.L	#(%0000100111110000<<16),BLTCON0
	;MOVE.W	#0,BLTAMOD
	;MOVE.W	#0,BLTDMOD
	;MOVE.L	A4,BLTAPTH
	;MOVE.L	A5,BLTDPTH
	;MOVE.W	#(COP_WAITS<<6)+COP_BLIT_SIZE,BLTSIZE
	RTS
	ENDC

__RANDOMIZE_PLANE:
	;SWAP	D5
	;MOVE.W	(PC),D5
	MOVE.L	D5,D1
	MOVE.W	#8-1,D2
	.outerLoop:
	MOVE.W	#(bypl/2)*5-1,D4
	.innerLoop:
	BSR.W	__RND
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
	MOVE.B	D5,D7
	LSR.W	#$6,D7
	LSR.W	#$6,D7
	.dummyLoop:		; ADDS RANDOMNESS...
	MOVE.B	$BFD800,D3
	EOR.B	D3,D5
	ROR.L	D5
	DBRA	D7,.dummyLoop
	;BRA.S	.noByte2
	RTS
	._byte2:
	MOVE.B	$DFF007,D3	;$dff00a $dff00b for mouse pos
	MOVE.B	$BFD800,D5
	OR.B	D3,D5
	.noByte2:
	RTS

__PXLX2_PLANE:
	MOVE.W	#he/2-1,D7
	.outerLoop:
	MOVE.W	#bypl/4-1,D6	; RESET D6
	.innerloop:
	CLR.L	D1
	MOVE.L	(A4),D0
	MOVE.W	#$F,D2
	.bitLoop:
	BTST	#0,D0
	BEQ.S	.skipBit
	BSET	#0,D1
	BSET	#1,D1
	.skipBit:
	ROL.W	D0
	ROL.L	#2,D1
	DBRA	D2,.bitLoop
	MOVE.L	D1,40(A4)
	MOVE.L	D1,(A4)+
	DBRA	D6,.innerloop
	LEA	40(A4),A4
	DBRA	D7,.outerLoop
	RTS

__SCANLINIZE_PLANE:
	MOVE.L	#$0,D0
	MOVE.L	#-1,D1
	MOVE.W	#(bypl/4*he)-1,D7
	.outerLoop:
	MOVEM.L	D0-D1,(A4)
	LEA	8(A4),A4
	EXG	D0,D1
	DBRA	D7,.outerLoop
	RTS

__FILLSOLID:
	CLR	D6
	MOVE.W	#he-1,D4		; QUANTE LINEE
	.outerloop:		; NUOVA RIGA
	MOVE.L	#-1,D0		; ALL BITS
	IFNE EPILEPSY
	BSR.W	__RND\._byte
	ENDC
	AND.W	#$F000,D5
	CMP.W	#$A000,D5
	BNE.S	.noNoiseLine
	MOVE.L	#$0,D0
	.noNoiseLine:
	MOVE.W	#bypl/4-1,D6	; 40b = 10 Long
	.innerloop:
	MOVE.L	D0,(A4)+
	DBRA	D6,.innerloop
	DBRA	D4,.outerloop
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

__UPDT_BPL_PTR:
	ADD.W	(A2),A0
	ADD.W	D1,(A2)
	BTST	#$0,(A0)
	BNE.S	.dontShuffle
	SUB.W	D2,(A2)
	.dontShuffle:
	CMP.W	#bypl*he,(A2)
	BLO.S	.dontReset
	MOVE.W	#$0,(A2)
	.dontReset:
	RTS

__RACE_BEAM:
	LEA	LFO_VIBRO,A0
	LEA	LFO_NOSYNC,A1
	MOVE.W	V_LINE_IDX,D6
	MOVE.W	SCROLL_IDX,D0
	ADD.W	#$2,D0
	AND.W	#$3F-1,D0
	MOVE.W	D0,SCROLL_IDX
	MOVE.W	BPLMOD_IDX,D5
	AND.W	#$7F-1,D5
	SUB.W	#$2,D5
	MOVE.W	D5,BPLMOD_IDX
	CLR.L	D2
	CLR.L	D7		; SYNC IDX
	;MOVE.W	VHPOSR,D4		; for bug?
	;.waitVisibleRaster:
	;MOVE.W	VHPOSR,D4
	;AND.W	#$FF00,D4		; read vertical beam
	;CMP.W	#$3700,D4		; 2C
	;BNE.S	.waitVisibleRaster

	.dummyWait:
	MOVE.W	VPOSR,D1		; Read vert most sig. bits
	BTST	#0,D1
	BNE.S	.dummyWait

	.waitNextRaster:
	MOVE.W	VHPOSR,D2
	AND.W	#$FF00,D2		; read vertical beam
	CMP.W	D4,D2
	BEQ.S	.waitNextRaster

	MOVE.W	VHPOSR,D4		; RACE THE BEAM!
	AND.W	#$FF00,D4		; RACE THE BEAM!

	IFNE EPILEPSY
	CMP.W	D6,D2		; 12.032 - #$2F00
	BNE.S	.noLine
	MOVE.W	#$0123,$DFF19E	; WHITE LINE
	MOVE.W	#$0012,$DFF192
	MOVE.W	#$0345,$DFF196
	MOVE.W	#$0234,$DFF19A
	MOVE.W	#$3F-1,D7
	BRA.S	.noLine2
	.noLine:
	ENDC

	;SUB.W	#1,D6
	;CMP.W	D6,D2		; 12.032 - #$2F00
	;BNE.S	.noLine2
	;MOVE.W	#$000F,$DFF19A	; WHITE LINE
	.noLine2:

	CMP.W	#$5700,D2		; 12.032 - #$2F00
	BNE.S	.keepLFO
	LEA	LFO_SINE1,A0
	MOVE.L	#$0F0000FF,$DFF182	; TIKTOK
	BRA.S	.keepLFO2
	.keepLFO:

	CMP.W	#$B700,D2		; 12.032 - #$2F00
	BNE.S	.keepLFO2
	LEA	LFO_SINE2,A0
	.keepLFO2:

	;CLR.L	D5
	;MOVE.W	D2,D4
	;MOVE.B	$DFF007,D5	; $dff00a $dff00b for mouse pos
	;MOVE.B	$BFD800,D1
	;EOR.B	D1,D5
	;MOVE.W	D5,BPLCON1

	BTST	#6,$BFE001	; POTINP - LMB pressed?
	BNE.W	.skip
	MOVE.L	#$09990777,$DFF182	; G+P
	MOVE.L	#$011B0666,$DFF186	; G+P
	MOVE.B	D0,DDFSTRT
	MOVE.W	D5,BPL2MOD
	;MOVE.B	D5,DDFSTOP
	;MOVE.W	D5,BPL1MOD
	;LEA	LFO_NOSYNC,A0
	;MOVE.W	#$C000+$600,BPLCON0		; HIRES?
	;MOVE.W	#(bpls-1)*$1000+$A00,BPLCON0	; HAM!?!
	;MOVE.L	#$0F1F010E,$DFF182		; HAM + EHB
	;MOVE.W	#bpls*$1000+$200,BPLCON0	; EHB!?!
	;MOVE.L	#$00E00F0F,$DFF186		; EHB
	;MOVE.L	#$0F0F0A1A,$DFF18A		; EHB
	.skip:

	MOVE.W	(A0,D0.W),D3	; 19DEA68E GLITCHA
	TST.W	D7
	BEQ.S	.noSyncShift
	ROR.L	#4,D3
	MOVE.W	(A1,D7.W),D3	; 19DEA68E GLITCHA
	SUB.W	#$2,D7
	ROL.L	#4,D3
	.noSyncShift:
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
	;CMP.W	#$0400,D2		; 12.032 - #$2F00
	CMP.W	#$1F00,D2		; 12.032
	BEQ.W	.waitNextRaster
	;MOVE.W	#0,BPLCON1	; RESET REGISTER
	;MOVE.L	#0,BPL1MOD	; RESET
	RTS

__V_DISPLACE:
	LEA	TXT_GRID,A0		; PF_1 OSD
	LEA	V_OFFSET,A2
	MOVE.W	#$3F,D3
	ADD.W	#$2,D0
	AND.W	D3,D0
	MOVE.W	(A2,D0.W),D1
	ADD.W	D1,A0
	BSR.W	PokePtrs
	RTS

__UPDATE_V_LINE:
	LEA	COPPER\.BplPtrs+2+8+4,A1
	TST.B	V_LINE_IDX
	BNE.S	.skip
	LEA	PLANE_1,A0
	MOVE.W	A0,(A1)
	MOVE.B	#$FF,V_LINE_IDX
	RTS
	.skip:
	SUB.B	#$7,V_LINE_IDX
	ADD.W	#$86,(a1)
	RTS

__FETCH_LETTER:
	; in: A0 text_idx, A3 Text, A4 destination, A5 Font
	; out: A5 current letter from font, A4 destination
	LEA	FONT-32,A5
	ADD.L	#bypl-2,A4		; POSITIONING
	;CMP.W	#CHARS_PER_LINE,(A0)
	;BNE.S	.skip
	;MOVE.W	#0,(A0)
	;.skip:
	ADD.W	(A0),A3
	ADD.W	#$1,(A0)

	CLR.L	D2
	MOVE.B	(A3),D2
	SUBI.B	#$20,D2
	MULU.W	#32,D2
	ADD.W	D2,A5
	RTS

__BLIT_LETTER:
	BSR.W	WaitBlitter
	MOVE.W	#%0000100111110000,BLTCON0	; BLTCON0
	MOVE.W	#%0000000000000000,BLTCON1	; BLTCON1
	MOVE.L	#$FFC0FFFF,BLTAFWM		; BLTAFWM
	MOVE.W	#$0,BLTAMOD		; BLTAMOD
	MOVE.W	#bypl-2,BLTDMOD		; Init modulo Dest D
	MOVE.L	A5,BLTAPTH
	MOVE.L	A4,BLTDPTH
	MOVE.W	#FONT_H*64+FONT_H/16,BLTSIZE	; Start Blitter (Blitsize)
	RTS

__SCROLL_X:				; A4 source/dest
	ADD.L	#LINE_H-1,A4
	LEA	(A4),A5
	BSR	WaitBlitter
	MOVE.L	#(((FONT_SCROLL<<12)+%100111110000)<<16)+%10,BLTCON0
	MOVE.L	#$FFFFFFFF,BLTAFWM
	MOVE.L	#$0,BLTAMOD
	MOVE.L	A4,BLTAPTH
	MOVE.L	A5,BLTDPTH
	MOVE.W	#FONT_H*64+wi/16,BLTSIZE
	RTS

__BLIT_LINE:				; A5 Src, A4 dest
	BSR.W	WaitBlitter
	MOVE.W	#%0000100111110000,BLTCON0	; BLTCON0
	MOVE.W	#%0000000000000000,BLTCON1	; BLTCON1
	MOVE.L	#$FFFFFFFF,BLTAFWM		; BLTAFWM
	MOVE.W	#bypl,BLTAMOD		; BLTAMOD
	MOVE.W	#bypl,BLTDMOD		; Init modulo Dest D
	MOVE.L	A5,BLTAPTH
	MOVE.L	A4,BLTDPTH
	MOVE.W	#FONT_H*64+wi/16,BLTSIZE	; Start Blitter (Blitsize)
	RTS

__SCROLL_BODY:
	LEA	LINE_BUF,A4
	BSR.W	__SCROLL_X
	RTS

__LETTER_BODY:
	LEA	TXT_IDX,A0
	LEA	TXT_BODY,A3
	LEA	LINE_BUF,A4
	BSR.W	__FETCH_LETTER
	BSR.W	__BLIT_LETTER
	RTS

__PLACE_BODY:
	LEA	LINE_BUF,A5
	MOVE.L	VPOS_BODY,A4
	ADD.L	#bypl,A5
	ADD.L	#bypl,A4
	BSR.W	__BLIT_LINE
	RTS
	.half:
	LEA	LINE_BUF,A5
	MOVE.L	VPOS_BODY,A4
	BSR.W	__BLIT_LINE
	RTS

__WIPE_BODY:
	MOVE.L	VPOS_BODY,A4
	ADD.L	#bypl,A4
	BSR.W	_WipeMEM
	RTS
	.half:
	MOVE.L	VPOS_BODY,A4
	BSR.W	_WipeMEM
	RTS

__NOP:	RTS

__RESET_BODY:
	MOVE.W	#$0,TXT_TIMELINE_IDX
	LEA	TXT_IDX,A0
	ADD.W	#$1,(A0)
	LEA	TXT_BODY,A3
	ADD.W	(A0),A3
	TST.B	(A3)
	BNE.S	.skip
	MOVE.W	#$0,(A0)
	.skip:
	RTS

FRAME_STROBE:	DC.B 0,0
NOISE_IDX3:	DC.W 0
NOISE_IDX5:	DC.W 0
BPLMOD_IDX:	DC.W 0
SCROLL_IDX:	DC.W 0
LFO_SINE1:	DC.W 0,1,1,2,2,2,3,4,4,5,5,6,6,7,6,7,6,7,7,6,6,5,5,4,4,3,2,2,2,1,1,0
LFO_SINE2:	DC.W 5,5,4,5,5,4,5,4,5,5,4,4,3,2,3,2,1,0,0,0,1,0,1,2,2,3,4,4,5,4,5,4
LFO_NOISE:	DC.W 1,4,1,5,2,4,3,5,2,4,2,5,2,4,1,5,1,4,1,5,3,4,2,5,1,4,5,5,4,4,6,5
LFO_VIBRO:	DC.W 4,5,4,5,4,5,4,5,4,5,2,1,2,1,2,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,2
LFO_NOSYNC:	DC.W 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,3,4,3,4,7,10,13,15
V_IDX_1:		DC.W $2
V_IDX_2:		DC.W $32
V_OFFSET:		DC.W 0,40,40,80,80,40,40,0,0,-40,-80,-80,-40,-40,0,0
		DC.W 0,0,40,40,80,80,40,0,-40,-40,-80,-80,-40,-40,-40,0
BLUE_COLS_OFFSET:	DC.W $0
BLUE_COLS_IDX:	DC.W $0
BLUE_COLS:	
		DC.W $0226,$0218,$0224,$0137,$0127,$0128,$0036,$0027	; BHO
		DC.W $012B,$0319,$011C,$011B,$030A,$0128,$003B,$003A	; BLU?
		DC.W $0506,$0406,$0405,$0305,$0305,$0305,$0203,$0204	; BHO
		DC.W $012B,$002C,$002A,$012B,$001A,$004C,$0049,$000A	; BLU?

		DC.W $0007,$011A,$003C,$021A,$021C,$020C,$013A,$012C	; BLU?
		DC.W $0319,$0135,$0026,$0118,$0116,$021A,$0216,$0126	; BHO
		DC.W $002B,$031B,$030D,$020D,$0218,$010B,$001D,$011E	; BLU?
		DC.W $0204,$0205,$0104,$0205,$0115,$0213,$0114,$0105	; BHO

		DC.W $011A,$001D,$030B,$0318,$0345,$0045,$0044,$0043	; 1
		DC.W $011E,$011D,$011C,$010E,$003D,$002E,$002D,$002C	; 2
		DC.W $0224,$0421,$0025,$0331,$0322,$0430,$0134,$0240	; 1
		DC.W $001E,$002B,$0128,$001A,$0319,$003A,$0237,$000A	; 2

		DC.W $003C,$021E,$021D,$021C,$020D,$012D,$012C,$011F	; 2
		DC.W $0042,$0053,$0051,$0042,$0240,$0431,$0221,$0122	; 1
		DC.W $040D,$020E,$030A,$031D,$001D,$010F,$010D,$011A	; 2
		DC.W $0311,$0309,$0222,$0232,$0319,$0121,$0022,$0111	; 1

		DC.W $0705,$0521,$001D,$001A,$0323,$0243,$0319,$0224	; 3
		DC.W $0118,$0116,$011D,$0115,$011B,$0019,$012B,$0319	; 5
		DC.W $0323,$0506,$0319,$001A,$030A,$011A,$0133,$0043	; 3
		DC.W $004C,$001A,$0037,$0026,$001D,$0122,$0046,$0014	; 5

		DC.W $001D,$040D,$030B,$030D,$030F,$030E,$031B,$020D	; 4
		DC.W $0222,$0224,$0333,$0243,$0331,$0444,$0556,$0777	; 6
		DC.W $010D,$021E,$000D,$011F,$011E,$011D,$011C,$001E	; 4
		DC.W $0333,$0224,$0323,$0243,$0331,$0444,$0556,$0788	; 6

		DC.W $0216,$0218,$020A,$021B,$010B,$011C,$0009,$030A	; 5
		DC.W $0233,$0136,$030A,$011A,$0042,$0441,$0311,$0123	; 3
		DC.W $002C,$002B,$002A,$0128,$011A,$003B,$0027,$003A	; 5
		DC.W $0343,$0051,$0131,$0160,$0139,$0240,$0331,$0521	; 3

		DC.W $0111,$0112,$0122,$0121,$0232,$0233,$0211,$0212	; 6
		DC.W $030A,$011A,$020F,$020E,$010C,$021D,$021C,$010E	; 4
		DC.W $0111,$0113,$0122,$0121,$0232,$0233,$0211,$0222	; 6
		DC.W $001D,$001C,$001A,$012E,$002C,$002D,$0319,$000A	; 4
		; ################################################################
		DC.W $0222,$0005,$011B,$0516,$0236,$0163,$0168,$03B9	; NOIZ
		DC.W $0689,$0777,$0865,$0543,$0ABC,$0BBB,$0DEF,$0FFF	; NOIZ

		DC.W $012B,$012C,$011B,$012A,$011C,$020C,$003B,$012A	; BLU2
		DC.W $002B,$0128,$012A,$003A,$021A,$021D,$010C,$012D	; BLU2
		DC.W $0136,$0218,$002A,$001F,$020A,$0117,$004C,$0049	; BLU2
		DC.W $011B,$0139,$020C,$020A,$0509,$040E,$040D,$0307	; BLU2

		DC.W $000C,$000E,$000C,$000B,$000E,$020E,$000C,$000B	; BLU3
		DC.W $000C,$0009,$010B,$000B,$010B,$010F,$000E,$000F	; BLU3
		DC.W $0117,$0109,$000B,$000F,$010B,$0109,$000D,$000A	; BLU3
		DC.W $000C,$000B,$021C,$010B,$0218,$020F,$020E,$000F	; BLU3

	IFNE DYNCOPPER
GRADIENT_REGISTERS:	DC.W $019E,$0192,$0196,$019A
GRADIENT_INDEX:	DC.W $0
GRADIENT_VALS:	INCLUDE "CopGradientFrames.i"
	ENDC
TXT_IDX:		DC.W $0
TXT_HEADER:	DC.B " PLAY >         SYNTECHNO  "
TXT_BODY:		DC.B " PLEASE INSERT A CASSETTE  "
		DC.B " SYNTECHNO BY KONEY 2024   "
		DC.B " AKA AMIGA VHS             "
		DC.B " MY 3RD 40K AMIGA INTRO!   "
		DC.B " VHS FX ALL BY AMIGA HW    "
		DC.B " TO LEAVE THE CPU FREE     "
		DC.B " FOR RACING THE  BEAM!!    "
		DC.B " I JUST REALIZED NOW WHAT  "
		DC.B " A PITA IT IS TO WRITE     "
		DC.B " MAX 26 CHARS PER LINE...  "
		DC.B " OF COURSE READING WILL    "
		DC.B " BE A NIGHTMARE :)         "
		DC.B " THIS LITTLE TECHNO TUNE   "
		DC.B " WAS COMPOSED THESE DAYS   "
		DC.B " WITH OCTAMED SS AND IT    "
		DC.B " HEAVILY MAKES USE OF MED  "
		DC.B " SYNTHSOUNDS, HENCE THE    "
		DC.B " TITLE. OF COURSE MED ASM  "
		DC.B " PLAYROUTINES WERE USED    "
		DC.B " FOR PLAYBACK AND THEY     "
		DC.B " HELPED A LOT KEEPING LOW  "
		DC.B " THE TOTAL SIZE OF MUSIC!  "
		DC.B " I'VE ALSO USED A NEW WAY  "
		DC.B " OF CREATING BLOCKS WITH   "
		DC.B " 16 LINES = 2 BEATS PER    "
		DC.B " BLOCK TO MINIMIZE SCORE   "
		DC.B " SIZE EVEN MORE!           "
		DC.B " PT TOY WOULDN'T ALLOW IT. "
		DC.B " REWIND...                 "
		DC.B " >>>>>>>>>>>>>>>>>>>>>>>>  "
		DC.W $0
TXT_FOOTER:	DC.B " TRACKING ----I---------   "
		EVEN
TXT_TIMELINE_IDX:	DC.W $0
TXT_TIMELINE:	DC.L __NOP,__NOP,__NOP,__NOP,__NOP,__NOP,__NOP,__NOP,__NOP,__NOP
		DC.L __NOP,__NOP,__NOP,__NOP,__NOP,__NOP,__NOP,__NOP,__NOP,__NOP
		DC.L __SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY
		DC.L __SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY
		DC.L __SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY
		DC.L __SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY
		DC.L __SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY
		DC.L __SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY
		DC.L __SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY
		DC.L __WIPE_BODY\.half
		DC.L __NOP
		DC.L __PLACE_BODY\.half
		DC.L __NOP
		DC.L __WIPE_BODY
		DC.L __NOP
		DC.L __WIPE_BODY\.half
		DC.L __NOP,__NOP,__NOP
		DC.L __PLACE_BODY\.half
		DC.L __NOP,__NOP					
		DC.L __PLACE_BODY
		DC.L __NOP,__NOP,__NOP,__NOP
		DC.L __RESET_BODY

VPOS_HEADER:	DC.L TXT_GRID+LINE_H
VPOS_COUNTER:	DC.L TXT_GRID+LINE_H*2+bypl*6
VPOS_BODY:	DC.L TXT_GRID+LINE_H*(2+5)
VPOS_FOOTER:	DC.L TXT_GRID+LINE_H*(2+3+7)

RELOC3:		DC.L NOISE_UPD_PLANE3
RELOC5:		DC.L NOISE_UPD_PLANE5
		DC.L 0
NOISE_UPD_PLANE3:	DC.L PLANE_3
		DC.L 0
NOISE_UPD_PLANE5:	DC.L PLANE_5
NOISE_COP_PTR3:	DC.L COPPER\.BplPtrs+2+24
NOISE_COP_PTR5:	DC.L COPPER\.BplPtrs+2+40
V_LINE_IDX:	DC.B $FF,0
;*******************************************************************************
	SECTION	ChipData,DATA_C	;declared data that must be in chipmem
;*******************************************************************************
		DS.B he/4*bypl		; For PointPtr...

		DS.B he*bypl		; For PointPtr...

FONT:		INCBIN "VHS_font.raw",0
		EVEN

MED_MODULE:	INCBIN "med/SYNTECHNO.med"
_chipzero:	DC.L 0
_MED_MODULE:

COPPER:	; #### COPPERLIST ####################################################
	DC.W $1FC,0	; Slow fetch mode, remove if AGA demo.
	DC.W $8E,$2C81	; 238h display window top, left | DIWSTRT - 11.393
	DC.W $90,$1CC1	; and bottom, right.	| DIWSTOP - 11.457
	DC.W $92,$38	; Standard bitplane dma fetch start
	DC.W $94,$D0	; and stop for standard screen.
	DC.W $106,$0C00	; (AGA compat. if any Dual Playf. mode)
	DC.W $108,0	; BPL1MOD	 Bitplane modulo (odd planes)
	DC.W $10A,0	; BPL2MOD Bitplane modulo (even planes)
	;DC.W $102,$00	; SCROLL REGISTER (AND PLAYFIELD PRI)

	.Palette:
	DC.W $0180,$0001
	;DC.W $0182,$00D1,$0184,$0F0E	; Managed by CPU
	DC.W $0186,$000F
	DC.W $0188,$0FFF,$018A,$0DEF,$018C,$0DDD,$018E,$0CDE

	IFEQ DYNCOPPER
	.PaletteBG:
	DC.W $019E,$000F,$0192,$000A,$0196,$000C,$019A,$000D
	ENDC

	.PaletteHLine:
	;DC.W $0190,$055B,$0194,$0B37,$0198,$0282,$019C,$0C99
	DC.W $0190,$0AAA,$0194,$0EEE,$0198,$0666,$019C,$0888

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
	DC.W COPJMP2,$0		; Force a jump to  COP2LC
	ENDC

	IFEQ DYNCOPPER
	DC.W $FFDF,$FFFE		; allow VPOS>$ff
	;DC.W $3709,$FF00		; ## RASTER END ## #$12C?
	;DC.W $009A,$0010		; CLEAR RASTER BUSY FLAG
	DC.W $FFFF,$FFFE		; magic value to end copperlist
	ENDC

COPPER2:
	dc.w COP2LCH,0
	dc.w COP2LCL,0
	dc.w $2f07,$fffe
	dc.w $180,$0fe
	DC.W COPJMP1,$1		; Force a jump to  COP1LC 

COPPER3:
	dc.w COP2LCH,0
	dc.w COP2LCL,0
	dc.w $2c07,$fffe
	dc.w $180,$f05
	DC.W COPJMP1,$1		; Force a jump to  COP1LC 

;*******************************************************************************
	SECTION ChipBuffers,BSS_C	;BSS doesn't count toward exe size
;*******************************************************************************
	IFNE DYNCOPPER
	GRADIENT_PTRS:	DS.L COP_FRAMES
	COPPER_BUFFER:	DS.W COP_FRAMES*(COP_BLIT_SIZE*COP_WAITS+2+12)	; +2 vpos >$FF
	ENDC

LINE_BUF:		DS.B LINE_H
DUMMY_:		DS.B he/8*bypl
TXT_GRID:		DS.B he*bypl
DUMMY_0:		DS.B he/8*bypl
TXT_SCROLBUF:	DS.B (bypl+1)*FONT_H
PLANE_3:		DS.B he*bypl	; NOIZE
PLANE_4:		DS.B he*bypl
PLANE_5:		DS.B he*bypl	; NOIZE
PLANE_6:		DS.B he*bypl
PLANE_1:		DS.B he*bypl
PLANE_2:		DS.B (he+2)*bypl
DUMMY_1:		DS.B he*8*bypl
END
