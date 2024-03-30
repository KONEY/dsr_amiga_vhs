;*** CODE: KONEY ***
;*** MiniStartup by Photon ***
	INCDIR	"NAS:AMIGA/CODE/VHS_FX/"
	SECTION	"Code",CODE
	INCLUDE	"custom-registers.i"
	INCLUDE	"med/med_feature_control.i"	; MED CFGs
	INCLUDE	"PhotonsMiniWrapper1.04!.S"
	IFNE MED_PLAY_ENABLE
	INCLUDE	"med/MED_PlayRoutine.i"
	ENDC
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
COP_WAITS		EQU 240
COP_FRAMES	EQU 32
;COP_BLIT_SIZE	EQU 2+2+8+4+4
COP_WAITS_SIZE	EQU COP_WAITS*(4+4)
COP_SIZE_DIFF	EQU 4+4+4+4+4+4+8+8
	ENDC
;********** Demo **********	;Demo-specific non-startup code below.
Demo:			;a4=VBR, a6=Custom Registers Base addr
	;*--- init ---*
	MOVE.L	#VBint,$6C(A4)
	MOVE.W	#$C020,INTENA
	MOVE.W	#$87C0,DMACON
	;MOVE.W	#%1000011111100000,DMACON
	;*--- start copper ---*
	LEA	TXT_GRID,A0	; PF_1 OSD
	LEA	COPPER\.BplPtrs+2,A1
	BSR.W	PokePtrs
	LEA	PLANE2,A0		; PF_2 NOIZE (FILLED)
	LEA	COPPER\.BplPtrs+2+8,A1
	BSR.W	PokePtrs
	LEA	TXT_GRID,A0	; PF_1 OSD
	LEA	COPPER\.BplPtrs+2+16,A1
	BSR.W	PokePtrs
	LEA	PLANE3,A0		; PF_2 NOIZE
	LEA	COPPER\.BplPtrs+2+24,A1
	BSR.W	PokePtrs
	LEA	TXT_GRID,A0	; PF_1 OSD
	LEA	COPPER\.BplPtrs+2+32,A1
	BSR.W	PokePtrs
	LEA	PLANE5,A0		; PF_2 NOIZE
	LEA	COPPER\.BplPtrs+2+40,A1
	BSR.W	PokePtrs

	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC	
	IFNE DYNCOPPER
	; #### EXTRACT COPPERLISTS  ######
	;LEA	GRADIENT_VALS,A0
	LEA	COPPER_BUFFER,A1	; COPPER_BUFFER
	LEA	GRADIENT_PTRS,A4
	MOVE.W	#COP_FRAMES-1,D4
	MOVE.L	#$0F0000FF,D5	; TIKTOK
	MOVE.L	#$0F0B00F5,D6	; G+P
	.loop2:
	MOVE.L	A1,(A4)+
	BSR.W	__DECRUNCH_COPPERLIST
	DBRA	D4,.loop2
	BSR.W	__UpdateAllCopJmps
	ENDC
	BSR.W	__PREFILLS
	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC

	IFNE MED_PLAY_ENABLE
	;MOVE.W	#2,MED_START_POS	 ; skip to pos# after first block
	MOVE.W	#%1000000000001100,INTENA	; Master and lev6	; NO COPPER-IRQ!
	JSR	_startmusic
	ENDC

	LEA	MIX_COLS1,A6
	MOVE.L	#COPPER,COP1LC	; ## POINT COPPERLIST ##
;********************  main loop  ********************
MainLoop:
	BTST	#6,$BFE001		; POTINP - LMB pressed?
	BNE.S	.skip
	LEA	8(A6),A6
	.skip:

	BSR.W	__RACE_BEAM

	ADD.W	#$8,SCANLINE_IDX1
	ADD.W	#$7,SCANLINE_IDX2
	ADD.W	#$8,SCANLINE_IDX3
	SUB.W	#$2,SCANLINE_IDX0
	ADD.W	#$7,SCANLINE_IDX4
	ADD.W	#$6,SCANLINE_IDX5
	ADD.W	#$6,SCANLINE_IDX6

	CMP.L	#_COLORS_END,A6
	BLO.S	.dontResetColors
	;MOVE.W	(A6),$DFF180
	LEA	MIX_COLS1,A6
	.dontResetColors:

	IFNE EPILEPSY
	BSR.W	__UPDATE_V_LINE
	ENDC

	;* FOR TIMED EVENTS ON BLOCK ****
	MOVE.W	TXT_TIMELINE_IDX,D5
	LEA	TXT_TIMELINE,A3
	MOVE.L	(A3,D5),A4		; THANKS HEDGEHOG!!
	ADD.W	#$4,D5
	MOVE.W	D5,TXT_TIMELINE_IDX
	JSR	(A4)			; EXECUTE SUBROUTINE BLOCK#

	BCHG	#$1,FRAME_STROBE
	BNE.W	.oddFrame			; #############

	;ADD.B	#$A,SCANLINE_IDX1
	;ADD.B	#$7,SCANLINE_IDX2
	;ADD.B	#$B,SCANLINE_IDX3
	;MOVE.L	#$000F030A,$DFF182		; BLUE
	;MOVE.L	#$0F0000FF,$DFF182		; TIKTOK

	LEA	COPPER\.BplPtrs+2,A1	; VERTICAL TXT
	MOVE.W	V_IDX1,D0		; VERTICAL TXT
	BSR.W	__V_DISPLACE		; VERTICAL TXT
	MOVE.W	D0,V_IDX1		; VERTICAL TXT

	; ## RANDOMIZE ADDRESS POINTS OF NOISE PLANES ##
	LEA	NOISE_IDX3,A2
	LEA	PLANE3,A0
	;MOVE.W	#bypl/3,D1		; OFFSET
	;MOVE.W	#bypl/4,D2		; SUBSTR
	MOVE.W	#bypl*2*2+12,D1		; OFFSET
	MOVE.W	#bypl*2,D2		; SUBSTR
	BSR.W	__UPDT_BPL_PTR
	MOVE.L	A0,NOISE_UPD_PLANE3
	; ## RANDOMIZE ADDRESS POINTS OF NOISE PLANES ##

	BRA.W	.evenFrame
	.oddFrame:			; #############

	;ADD.B	#$9,SCANLINE_IDX1
	;ADD.B	#$6,SCANLINE_IDX2
	;ADD.B	#$C,SCANLINE_IDX3
	;MOVE.L	#$010A000E,$DFF182		; BLUE
	;MOVE.L	#$0F0700F5,$DFF182		; G+P

	LEA	COPPER\.BplPtrs+2+16,A1	; VERTICAL TXT
	MOVE.W	V_IDX2,D0		; VERTICAL TXT
	BSR.W	__V_DISPLACE		; VERTICAL TXT
	MOVE.W	D0,V_IDX2		; VERTICAL TXT

	; ## RANDOMIZE ADDRESS POINTS OF NOISE PLANES ##
	LEA	NOISE_IDX5,A2
	LEA	PLANE5,A0
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
	MOVE.L	A0,(A0)		; ADD the inital mess!!
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

	BSR.W	__STATIC_NOISE

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
	IFNE MED_PLAY_ENABLE
	; --- quit MED code ---
	MOVEM.L	D0-A6,-(SP)
	JSR	_endmusic
	MOVEM.L	(SP)+,D0-A6
	ENDC
	RTS

;********** Demo Routines **********
PokePtrs:				; EVEN SHRUNKER REFACTOR! :)
	MOVE.L	A0,(A0)		; Needs EMPTY plane to write addr
	MOVE.W	(A0),(A1)		; high word of address
	MOVE.W	A0,4(A1)		; low word of address
	CLR.L	(A0)		; Clear the inital mess?
	RTS

VBint:				; Blank template VERTB interrupt
	BTST	#5,INTREQR+1	; CHECK IF IT'S OUR VERTB INT.
	BEQ.S	.notVB
	;*--- DO STUFF HERE ---*
	MOVE.W	#$20,INTREQ	; POLL IRQ BIT
	MOVE.W	#$20,INTREQ
	.notVB:	
	RTE

_WipeMEM:		; a1=screen destination address to clear
	BSR	WaitBlitter
	MOVE.L	#$F5A5A5AF,BLTAFWM		; BLTAFWM
	MOVE.W	#bypl,BLTDMOD		; for HALF with lines.
	MOVE.W	#$100,BLTCON0		; set operation type in BLTCON0/1
	MOVE.W	#$000,BLTCON1
	MOVE.L	A4,BLTDPTH		; destination address
	MOVE.W	#FONT_H/2*64+wi/16,BLTSIZE	; Start Blitter (Blitsize)
	RTS

	IFNE DYNCOPPER
_PokeCopJmp:
	; D0 = src A0=dest
	MOVE.W	D0,6(A0)		; LOAD LOW SCR ADDR IN COPPER LIST
	SWAP	D0		; SWAP ADDRESS IN D0
	MOVE.W	D0,2(A0)		; LOAD HIGH SCR ADDR IN COPPER LIST
	RTS
__DECRUNCH_COPPERLIST:
	LEA	LFO_SINE3,A2	; FOR BG
	LEA	LFO_VIBRO,A0	; FOR TXT
	MOVE.W	SCROLL_IDX,D1
	ADD.W	#$2,D1
	AND.W	#COP_FRAMES*2-1,D1
	MOVE.W	D1,SCROLL_IDX
	; #####################################################
	MOVE.W	#COP2LCH,(A1)+	; TO-DO: ADD H-L POINTERS
	MOVE.W	#$FABE,(A1)+	; TO-DO: ADD H-L POINTERS
	MOVE.W	#COP2LCL,(A1)+	; TO-DO: ADD H-L POINTERS
	MOVE.W	#$DEBA,(A1)+	; TO-DO: ADD H-L POINTERS
	MOVE.B	#$1C,D0		; COP START
	ADD.B	#FONT_H,D0	; FIRST WAIT

	; ## TXT COLORS ##
	MOVE.W	#$0182,(A1)+
	MOVE.W	D6,(A1)+		; TIKTOK
	SWAP	D6
	MOVE.W	#$0184,(A1)+
	MOVE.W	D6,(A1)+		; TIKTOK
	SWAP	D6
	EXG	D5,D6
	; ## TXT COLORS ##

	MOVE.W	#COP_WAITS-1,D7
	.waitsLoop:
	LSL.W	#8,D0
	MOVE.B	#$07,D0		; CMD RESTORED $1C07
	MOVE.W	D0,(A1)+		; WAIT
	MOVE.W	#$FFFE,(A1)+	; WAIT
	LSR.W	#8,D0
	ADD.B	#$1,D0

	; ## FROM OLD RACE BEAM ##
	CMP.W	#$57,D0		; 12.032 - #$2F00
	BNE.S	.keepLFO
	LEA	LFO_SINE1,A0

	; ## TXT COLORS ##
	MOVE.W	#$0182,(A1)+
	MOVE.W	D6,(A1)+		; TIKTOK
	SWAP	D6
	MOVE.W	#$0184,(A1)+
	MOVE.W	D6,(A1)+		; TIKTOK
	SWAP	D6
	; ## TXT COLORS ##

	BRA.S	.keepLFO2
	.keepLFO:
	CMP.W	#$B7,D0		; 12.032 - #$2F00
	BNE.S	.keepLFO2
	LEA	LFO_SINE2,A0
	.keepLFO2:

	MOVE.W	#$0102,(A1)+

	MOVE.W	(A0,D1.W),D3
	; ## BG ##
	;ROR.L	#4,D3
	;MOVE.W	(A2,D1.W),D3
	;ROL.L	#4,D3
	; ## BG ##

	MOVE.W	D3,(A1)+
	ADD.W	#$2,D1
	AND.W	#COP_FRAMES*2-1,D1
	.skip:

	CMP.W	#$FF,D0		; ZEROED WORD = allow VPOS>$ff
	BNE.S	.notFF
	MOVE.W	#$0,D0
	MOVE.L	#$FFDFFFFE,(A1)+	; allow VPOS>$ff
	.notFF:

	MOVE.W	D7,$DFF180	; SHOW ACTIVITY
	DBRA	D7,.waitsLoop
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
	MOVE.B	D4,$DFF181	; SHOW ACTIVITY
	DBRA	D4,.loop2
	MOVE.L	D1,D0
	BSR.W	_PokeCopJmp
	MOVE.L	D1,COP2LC
	RTS
	ENDC

__PREFILLS:
	LEA	PLANE1,A4		; FILLS A PLANE
	BSR.W	__FILLSOLID	; SOME DUMMY OPERATION...
	BSR.W	__FILLSOLID	; SOME DUMMY OPERATION...
	LEA	PLANE2,A4		; FILLS A PLANE
	ADD.L	#he/6*4*bypl,A4
	MOVE.L	#$00000500,(A4)+	; MASK?
	ADD.L	#bypl*3,A4	; LEAVE EMPTY LINES FOR SCAN FX
	MOVE.L	#$00A000AA,(A4)+	; MASK?
	MOVE.L	#$F0F0F0F0,(A4)+	; MASK?
	MOVE.L	#$A5A5A5A5,(A4)+	; MASK?
	ADD.L	#bypl*4,A4	; LEAVE EMPTY LINES FOR SCAN FX
	MOVE.L	#$00A000AA,(A4)+	; MASK?
	MOVE.L	#$FEDCBA98,(A4)+	; MASK?
	MOVE.L	#$5FFF0000,(A4)+	; MASK?
	MOVE.L	#$F0F0F0F0,(A4)+	; MASK?
	ADD.L	#bypl/2,A4	; LEAVE EMPTY LINES FOR SCAN FX
	MOVE.L	#$0000FFFF,(A4)+	; MASK?
	MOVE.L	#$FEDCBA98,(A4)+	; MASK?
	MOVE.L	#$F0F0F0F0,(A4)+	; MASK?
	MOVE.L	#$00000000,(A4)+	; MASK?
	MOVE.L	#$A5A5A5A5,(A4)+	; MASK?
	ADD.L	#bypl/2,A4	; LEAVE EMPTY LINES FOR SCAN FX
	MOVE.L	#$0000FFFF,(A4)+	; MASK?
	MOVE.L	#$FEDCBA98,(A4)+	; MASK?
	ADD.L	#bypl*6,A4	; LEAVE EMPTY LINES FOR SCAN FX
	MOVE.L	#$00A000AA,(A4)+	; MASK?
	ADD.L	#bypl*5,A4	; LEAVE EMPTY LINES FOR SCAN FX
	MOVE.L	#$FF000050,(A4)+	; MASK?
	MOVE.L	#$0000FF0F,(A4)+	; MASK?

	LEA	PLANE3,A4		; FILLS A PLANE
	BSR.W	__FILLRND		; SOME DUMMY OPERATION...
	BSR.W	__FILLRND		; SOME DUMMY OPERATION...
	LEA	PLANE5,A4		; FILLS A PLANE
	BSR.W	__FILLRND		; SOME DUMMY OPERATION...
	BSR.W	__FILLRND		; SOME DUMMY OPERATION...
	LEA	PLANE5,A4		; FILLS A PLANE
	BSR.W	__PXLX2_PLANE	; SOME DUMMY OPERATION...
	BSR.W	__PXLX2_PLANE	; SOME DUMMY OPERATION...
	LEA	DUMMY1,A4	;	 FILLS A PLANE
	BSR.W	__FILLRND		; SOME DUMMY OPERATION...
	;BSR.W	__FILLRND		; SOME DUMMY OPERATION...
	; ### PREFILLS ###############
	MOVE.W	#$0,TXT_IDX
	MOVE.W	#CHARS_PER_LINE-1,D7
	.loop3:
	LEA	LINE_PLAY_BUF,A4
	BSR.W	__SCROLL_X
	LEA	TXT_IDX,A0
	LEA	TXT_HEADER1,A3
	LEA	LINE_PLAY_BUF,A4
	BSR.W	__FETCH_LETTER
	BSR.W	__BLIT_LETTER
	DBRA	D7,.loop3
	; ### PREFILLS ###############
	MOVE.W	#$0,TXT_IDX
	MOVE.W	#CHARS_PER_LINE-1,D7
	.loop6:
	LEA	LINE_KONEY_BUF,A4
	BSR.W	__SCROLL_X
	LEA	TXT_IDX,A0
	LEA	TXT_HEADER2,A3
	LEA	LINE_KONEY_BUF,A4
	BSR.W	__FETCH_LETTER
	BSR.W	__BLIT_LETTER
	DBRA	D7,.loop6
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
	RTS

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
	AND.L	#$AAA5A555,D1	; MORE TEXTURE
	MOVE.L	D1,40(A4)
	ROR.L	D1		; MORE TEXTURE
	MOVE.L	D1,(A4)+
	DBRA	D6,.innerloop
	LEA	40(A4),A4
	MOVE.W	D7,$DFF180	; SHOW ACTIVITY
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
	;BSR.W	__RND\._byte
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
	MOVE.W	D4,$DFF180	; SHOW ACTIVITY
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
	MOVE.B	D4,$DFF181	; SHOW ACTIVITY
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
	MOVEM.W	SCANLINE_IDX0(PC),D0-D6
	;CLR.L	D1		; RESET FLAG
	;CLR.L	D7
	;MOVE.W	VHPOSR,D0		; for bug?
	;.waitVisibleRaster:
	;MOVE.W	VHPOSR,D0
	;AND.W	#$FF00,D0		; read vertical beam
	;CMP.W	#$3700,D0		; 2C
	;BNE.S	.waitVisibleRaster
	.dummyWait:
	MOVE.W	VPOSR,D7		; Read vert most sig. bits
	BTST	#0,D7
	BNE.S	.dummyWait

	CMP.B	D0,D3
	BNE.S	.notSameLine
	LEA	8(A6),A6
	MOVE.B	D0,SCANLINE_IDX1
	MOVE.B	D0,SCANLINE_IDX2
	;MOVE.B	#$0,V_LINE_IDX
	;MOVE.W	#30,BPL1MOD
	;MOVE.W	D6,BPL2MOD
	.notSameLine:

	.waitNextRaster:
	MOVE.B	VHPOSR,D7
	BEQ.S	.waitNextRaster
	;MOVE.W	#$8080,DMACON	; ENABLE COPDMA
	;MOVE.B	VHPOSR,D0		; RACE THE BEAM!
	CMP.B	D0,D7
	BNE.S	.noLine0
	;MOVE.W	#$0F0F,$DFF180
	MOVE.W	#$0,BPL2MOD
	MOVE.W	#$9,BPLCON1	; HW SCROLL
	MOVE.W	(A6),$DFF192
	MOVE.W	2(A6),$DFF196
	MOVE.W	4(A6),$DFF19A
	MOVE.W	6(A6),$DFF19E
	BRA.S	.waitNextRaster
	.noLine0:

	CMP.B	D1,D7
	BNE.S	.noLine1
	;MOVE.W	#$000F,$DFF180
	;MOVE.W	#$0,BPL2MOD
	MOVE.W	#$4,BPLCON1	; HW SCROLL
	MOVE.W	8(A6),$DFF192
	MOVE.W	10(A6),$DFF196
	MOVE.W	12(A6),$DFF19A
	MOVE.W	14(A6),$DFF19E
	BRA.S	.waitNextRaster
	.noLine1:

	CMP.B	D2,D7
	BNE.S	.noLine2
	;MOVE.W	#$00F0,$DFF180
	;MOVE.W	D2,BPL2MOD
	MOVE.W	16(A6),$DFF192
	MOVE.W	18(A6),$DFF196
	MOVE.W	20(A6),$DFF19A
	MOVE.W	22(A6),$DFF19E
	;MOVE.W	#$0EEE,$DFF190
	;MOVE.W	#$0111,$DFF194
	;MOVE.W	#$0DDD,$DFF198
	;MOVE.W	#$0FFF,$DFF19C
	BRA.W	.waitNextRaster
	.noLine2:

	CMP.B	D3,D7
	BNE.S	.noLine3
	;MOVE.W	#$0F00,$DFF180
	;MOVE.W	#$0,BPL2MOD
	MOVE.W	#$8,BPLCON1	; HW SCROLL
	MOVE.W	24(A6),$DFF192
	MOVE.W	26(A6),$DFF196
	MOVE.W	28(A6),$DFF19A
	MOVE.W	30(A6),$DFF19E
	BRA.W	.waitNextRaster
	.noLine3:

	CMP.B	D4,D7
	BNE.S	.noLine4
	;MOVE.W	#$00FF,$DFF180
	;MOVE.W	#$0,BPL2MOD
	MOVE.W	#$7,BPLCON1	; HW SCROLL
	MOVE.W	32(A6),$DFF192
	MOVE.W	34(A6),$DFF196
	MOVE.W	36(A6),$DFF19A
	MOVE.W	38(A6),$DFF19E
	BRA.W	.waitNextRaster
	.noLine4:

	CMP.B	D5,D7
	BNE.S	.noLine5
	;MOVE.W	#$0,BPL2MOD
	;MOVE.W	D5,BPL2MOD
	MOVE.W	#$7,BPLCON1	; HW SCROLL
	;MOVE.W	#$80,DMACON	; DISABLE COPDMA
	;MOVE.W	(A6),$DFF18E
	;MOVE.W	2(A6),$DFF18C
	;MOVE.W	#$0999,$DFF192
	;MOVE.W	#$0000,$DFF196
	;MOVE.W	#$0333,$DFF19A
	;MOVE.W	#$0FFF,$DFF19E
	;MOVE.W	40(A6),$DFF192
	;MOVE.W	42(A6),$DFF196
	;MOVE.W	44(A6),$DFF19A
	;MOVE.W	46(A6),$DFF19E
	BRA.W	.waitNextRaster
	.noLine5:

	CMP.B	D6,D7
	BNE.S	.noLine6
	;MOVE.W	#$0,BPL2MOD
	MOVE.W	#$1,BPLCON1	; HW SCROLL
	;MOVE.W	#$8080,DMACON	; ENABLE COPDMA
	;MOVE.W	#$0DDD,$DFF18E
	;MOVE.W	#$0DEF,$DFF18C
	MOVE.W	16(A6),$DFF192
	MOVE.W	18(A6),$DFF196
	MOVE.W	20(A6),$DFF19A
	MOVE.W	22(A6),$DFF19E
	BRA.W	.waitNextRaster
	.noLine6:

	MOVE.W	VPOSR,D7		; Read vert most sig. bits
	BTST	#0,D7
	BEQ.W	.waitNextRaster

	CMP.B	#$1F,D7		; 12.032
	BEQ.W	.waitNextRaster
	RTS

__RACE_BEAM_OLD:
	LEA	LFO_VIBRO,A0
	LEA	LFO_NOSYNC,A1
	MOVE.W	V_LINE_IDX,D6
	MOVE.W	SCROLL_IDX,D0
	ADD.W	#$2,D0
	AND.W	#$3F-1,D0
	MOVE.W	D0,SCROLL_IDX
	;MOVE.W	BPLMOD_IDX,D5
	;AND.W	#$7F-1,D5
	;SUB.W	#$2,D5
	;MOVE.W	D5,BPLMOD_IDX
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
	;BRA.S	.noLine2
	.noLine:
	ENDC

	;SUB.W	#1,D6
	;CMP.W	D6,D2		; 12.032 - #$2F00
	;BNE.S	.noLine2
	;MOVE.W	#$000F,$DFF19A	; WHITE LINE
	;.noLine2:

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

	;BTST	#6,$BFE001	; POTINP - LMB pressed?
	;BNE.W	.skip
	;MOVE.L	#$09990777,$DFF182	; G+P
	;MOVE.L	#$011B0666,$DFF186	; G+P
	;MOVE.B	D0,DDFSTRT
	;MOVE.W	D5,BPL2MOD
	;;MOVE.B	D5,DDFSTOP
	;;MOVE.W	D5,BPL1MOD
	;;LEA	LFO_NOSYNC,A0
	;;MOVE.W	#$C000+$600,BPLCON0		; HIRES?
	;;MOVE.W	#(bpls-1)*$1000+$A00,BPLCON0	; HAM!?!
	;;MOVE.L	#$0F1F010E,$DFF182		; HAM + EHB
	;;MOVE.W	#bpls*$1000+$200,BPLCON0	; EHB!?!
	;;MOVE.L	#$00E00F0F,$DFF186		; EHB
	;;MOVE.L	#$0F0F0A1A,$DFF18A		; EHB
	;.skip:

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
	SUB.W	#$42,(a1)
	ADD.B	#$8,V_LINE_IDX
	TST.B	V_LINE_IDX
	BNE.S	.skip
	LEA	PLANE2,A0
	MOVE.W	A0,(A1)
	.skip:
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
	MOVE.W	#FONT_H/2*64+wi/16,BLTSIZE	; Start Blitter (Blitsize)
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

__SWAP_HEADER_TEXT
	MOVEM.L	HEADERS_PTRS(PC),A4-A5
	EXG	A4,A5
	MOVEM.L	A4-A5,HEADERS_PTRS
	;LEA	LINE_PLAY_BUF,A5
	MOVE.L	VPOS_HEADER,A4
	BSR.W	__BLIT_LINE
	;LEA	LINE_PLAY_BUF,A5
	;MOVE.L	VPOS_HEADER,A4
	ADD.L	#bypl,A5
	ADD.L	#bypl,A4
	BSR.W	__BLIT_LINE
	RTS

__WIPE_HEADER:
	MOVE.L	VPOS_HEADER,A4
	ADD.L	#bypl,A4
	BSR.W	_WipeMEM
	RTS
	.half:
	MOVE.L	VPOS_HEADER,A4
	BSR.W	_WipeMEM
	RTS

__BLIT_NOISE:
	LEA	PLANE2,A5
	ADD.L	#he/6*4*bypl,A5
	LEA	(A5),A4
	MOVE.W	#%0000100111110000,BLTCON0	; BLTCON0
	MOVE.W	#%0000000000000000,BLTCON1	; BLTCON1
	MOVE.L	SCANLINE_IDX1,D0		; BLTAFWM
	MOVE.L	#$FFFFFFFF,BLTAFWM		; BLTAFWM
	LSR.W	#3,D0
	MOVE.W	D0,BLTAMOD		; Init modulo Dest D
	;SWAP	D0
	LSR.W	#2,D0
	SUB.L	D0,A4
	MOVE.W	#40,BLTDMOD		; Init modulo Dest D
	MOVE.L	A5,BLTAPTH
	MOVE.L	A4,BLTDPTH
	MOVE.W	#FONT_H*64+wi/16,BLTSIZE	; Start Blitter (Blitsize)
	;BSR.W	__BLIT_LINE
	RTS

__STATIC_NOISE:
	BSR.W	__RND
	LEA	MED_MODULE,A1
	LEA	PLANE2,A0
	ADD.L	#he*bypl-2-bypl,A0
	BSR.W	BlitterFill
	LEA	PLANE2,A0
	ADD.L	#he/7*6*bypl-2,A0
	SWAP	D5
	BSR.W	BlitterFill
	;LEA	PLANE2,A4
	;ADD.L	#he/6*4*bypl,A4
	;BSR.W	_WipeMEM
	RTS

BlitterFill:
	BSR.W	WaitBlitter
	MOVE.L	#$AAAA5555,BLTAFWM		; BLTAFWM
	MOVE.W	#%0000100111110000,BLTCON0	; BLTCON0
	MOVE.W	#%0000000000001010,BLTCON1	; BLTCON1
	;TST.B	FRAME_STROBE
	;BNE.S	.changeMode
	;MOVE.W	#%0000000000001010,BLTCON1	; BLTCON1
	;.changeMode:
	MOVE.B	D5,BLTAMOD		; BLTAMOD
	MOVE.W	#bypl*2,BLTDMOD		; Init modulo Dest D
	MOVE.L	A1,BLTAPTH		; BLTAPT  (fisso alla figura sorgente)
	MOVE.L	A0,BLTDPTH
	MOVE.W	#he/6/4*64+wi*2/16,BLTSIZE	; Start Blitter (Blitsize)
	RTS

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

DrawLine:
	SUB.W	d0,d2		; D2 = Dx = X1 - X2
	BMI.B	.Oct2345		; Nagative? Octant could be 2,3,4,5
	SUB.W	d1,d3		; D3 = Dy = Y1 - Y2 
	BMI.B	.Oct01		; Negative? Octant is 0 or 1
	CMP.W	d3,d2		; Compare Dy with Dx
	BMI.B	.Oct6		; Dy > Dx? Octant 6!
	MOVEQ	#$0011,d4		; Select LINE + octant 7!
	BRA.B	.DoneOctant

	.Oct6:
	EXG	d2,d3		; Ensure D2=Dmax and D3=Dmin
	MOVEQ	#$0001,d4		; Select LINE + octant 6
	BRA.B	.DoneOctant

	.Oct2345:
	NEG.W	d2		; Make Dx positive 
	SUB.W	d1,d3		; D3 = Dy = Y1 - Y2
	BMI.B	.Oct23		; Negative? Octant is 2 or 3
	CMP.W	d3,d2		; Compare Dy with Dx
	BMI.B	.Oct5		; Dy > Dx? Octant 5!
	MOVEQ	#$0015,d4		; Select LINE + octant 4
	BRA.B	.DoneOctant

	.Oct5:
	EXG	d2,d3		; Ensure D2=Dmax and D3=Dmin
	MOVEQ	#$0009,d4		; Select LINE + octant 5
	BRA.B	.DoneOctant

	.Oct23:
	NEG.W	d3		; Make Dy positive
	CMP.W	d3,d2		; Compare Dy with Dx
	BMI.B	.Oct2		; Dy > Dx? Octant 2!
	MOVEQ	#$001D,d4		; Select LINE + octant 3
	BRA.B	.DoneOctant

	.Oct2:
	EXG	d2,d3		; Ensure D2=Dmax and D3=Dmin
	MOVEQ	#$000D,d4		; Select LINE + octant 2
	BRA.B	.DoneOctant

	.Oct01:
	NEG.W	d3		; Make Dy positive
	CMP.W	d3,d2		; Compare Dy with Dx
	BMI.B	.Oct1		; Dy > Dx? Octant 1!
	MOVEQ	#$0019,d4		; Select LINE + octant 0
	BRA.B	.DoneOctant

	.Oct1:
	EXG	d2,d3		; Ensure D2=Dmax and D3=Dmin
	MOVEQ	#$0005,d4		; Select LINE + octant 1

	.DoneOctant:
	ADD.W	d2,d2		; D2 = 2 * Dmax	
	ASL.W	#2,d3		; D3 = 4 * Dmin

	MULU	#40,d1		; Convert Y1 pos into offset
	ADD.L	d1,a0		; Add ofset to bitplane pointer
	EXT.L	d0		; Clear top bits of D0
	ROR.L	#4,d0		; Roll shift bits to top word 
	ADD.W	d0,d0		; Bottom word: convert to byte offset 
	ADDA.W	d0,a0		; Add byte offset to bitplane pointer
	SWAP	d0		; Move shift value to bottom word
	OR.W	#$0B5A,d0		; USEA, C and D. Minterm $5A, D=A/C+/AC

	MOVE.W	d2,d1		; D1 = 2 * Dmax
	LSL.W	#5,d1		; Shift Dmax to Hx pos for BLTSIZE
	ADD.W	#$0042,d1		; Add 1 to Hx and set Wx to 2

	BSR	WaitBlitter

	MOVE.W	#$FFFF,BLTAFWM	; No first word masking
	MOVE.W	#$FFFF,BLTALWM	; No last word masking
	MOVE.W	#40,BLTCMOD	; Bitplane is 40 bytes wide
	MOVE.W	#40,BLTDMOD	; Bitplane is 40 bytes wide
	MOVE.L	a0,BLTCPTH	; Source C = bitplane to draw on
	MOVE.L	a0,BLTDPTH	; Destination = bitplane to draw on
	MOVE.W	d0,BLTCON0	; Source A shift and logic function
	MOVE.W	d3,BLTBMOD	; Set 4 * Dmin
	MOVE.W	#$8000,BLTADAT	; A data = 0x8000 (write to BLTCON0 first!)
	MOVE.W	#$FFFF,BLTBDAT

	SUB.W	d2,d3		; D3 = (2 * Dmax)-(4 * Dmin)
	EXT.L	d3		; Make full long sized
	MOVE.L	d3,BLTAPTL	; Store in A pointer
	BPL.B	.NotNeg		; Skip if positive
	OR.W	#$0040,d4		; Set SIGN bit if negative
	.NotNeg:
	MOVE.W	d4,BLTCON1	; Octant selection, SIGN and LINE
	SUB.W	d2,d3		; D2 = (2*Dmax), D3 = (2*Dmax)-(4*Dmin)
	MOVE.W	d3,BLTAMOD	; D3 = 4 * (DMax - Dmin)
	MOVE.W	d1,BLTSIZE	; Set length and start the Blitter
	RTS

FRAME_STROBE:	DC.B 0,0
NOISE_IDX3:	DC.W 0
NOISE_IDX5:	DC.W 0
BPLMOD_IDX:	DC.W 0
SCROLL_IDX:	DC.W 0
LFO_SINE1:	DC.W 0,1,1,2,2,2,3,4,4,5,5,6,6,7,6,7,6,7,7,6,6,5,5,4,4,3,2,2,2,1,1,0
LFO_SINE2:	DC.W 5,5,4,5,5,4,5,4,5,5,4,4,3,2,3,2,1,0,0,0,1,0,1,2,2,3,4,4,5,4,5,4
LFO_SINE3:	DC.W 2,2,3,3,3,3,3,2,2,2,1,1,1,1,1,2,2,2,3,3,3,3,3,2,2,2,1,1,1,1,1,2
LFO_NOISE:	DC.W 1,4,1,5,2,4,3,5,2,4,2,5,2,4,1,5,1,4,1,5,3,4,2,5,1,4,5,5,4,4,6,5
LFO_VIBRO:	DC.W 4,5,4,5,4,5,4,5,4,5,2,1,2,1,2,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,2
LFO_NOSYNC:	DC.W 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,3,4,3,4,7,10,13,15
V_IDX1:		DC.W $2
V_IDX2:		DC.W $32
V_OFFSET:		DC.W 0,40,40,80,80,40,40,0,0,-40,-80,-80,-40,-40,0,0
		DC.W 0,0,40,40,80,80,40,0,-40,-40,-80,-80,-40,-40,-40,0
BLUE_COLS_OFFSET:	DC.W $0
BLUE_COLS_IDX:	DC.W $0
MIX_COLS1:	DC.W $000C,$000D,$000A,$000B		; BLU3
		DC.W $0027,$0029,$002A,$002C		; BLU2
		DC.W $011E,$011D,$011C,$010E		; BLU?
		DC.W $000C,$000E,$000F,$001B		; BLU3
		DC.W $012B,$0319,$011C,$011B		; BLU?
		DC.W $0242,$0044,$0043,$0042		; green new
		DC.W $000C,$000A,$010B,$000B		; BLU3_
		DC.W $0138,$013C,$0137,$013A		; blu2
		DC.W $0242,$0044,$0043,$0042		; green new
		DC.W $0013,$0016,$0015,$0017		; blu1
		DC.W $012B,$0319,$011C,$011B		; BLU?
		DC.W $0302,$0403,$0302,$0204		; purlpe2_2
		DC.W $0111,$0222,$0223,$0333		; gray1
		DC.W $0027,$0029,$002A,$002C		; BLU2
		DC.W $0020,$0031,$0040,$0050		; green1
		DC.W $000C,$000E,$000F,$001B		; BLU3
		DC.W $0204,$0205,$0104,$0205		; PURPL
		DC.W $0302,$0403,$0302,$0204		; purlpe2_2
		DC.W $0222,$0333,$0444,$0111		; gray4
		DC.W $012B,$0319,$011C,$011B		; BLU?
		DC.W $000C,$000A,$010B,$000B		; BLU3_
		DC.W $040D,$020E,$030A,$031D		; PURPL
		DC.W $0138,$013C,$0137,$013A		; blu2
		DC.W $0013,$0016,$0015,$0017		; blu1
		DC.W $0242,$0044,$0043,$0042		; green new
		DC.W $040D,$020E,$030A,$031D		; PURPL
		DC.W $000C,$000D,$000A,$000B		; BLU3
		DC.W $0204,$0205,$0104,$0205		; PURPL
		DC.W $0027,$0029,$002A,$002C		; BLU2
		DC.W $004C,$004E,$004F,$015F		; cyan
		DC.W $0042,$0053,$0051,$0043		; GREEN
		DC.W $004C,$004E,$004F,$015F		; cyan
		DC.W $0404,$0505,$0504,$0405		; purlpe1
		DC.W $0204,$0205,$0104,$0205		; PURPL
		DC.W $0242,$0044,$0043,$0042		; green new
		DC.W $000C,$000A,$010B,$000B		; BLU3_
		DC.W $040D,$020E,$030A,$031D		; PURPL
		DC.W $0121,$0010,$0030,$0040		; green4
		DC.W $0138,$013C,$0137,$013A		; blu2
		DC.W $0242,$0044,$0043,$0042		; green new
		DC.W $0121,$0010,$0030,$0040		; green4
		DC.W $000C,$000E,$000F,$001B		; BLU3
		DC.W $0027,$0029,$002A,$002C		; BLU2
		DC.W $011D,$040D,$030B,$030D		; PURPL_
		DC.W $0242,$0044,$0043,$0042		; green new
		DC.W $011E,$011D,$011C,$010E		; BLU?
		DC.W $000C,$000E,$000F,$001B		; BLU3
		DC.W $0138,$013C,$0137,$013A		; blu2
		DC.W $012B,$0319,$011C,$011B		; BLU?
		DC.W $004C,$004E,$004F,$015F		; cyan
		DC.W $0343,$0051,$0131,$0160		; GREEN
		DC.W $0242,$0044,$0043,$0042		; green new
		DC.W $011E,$011D,$011C,$010E		; BLU?
		DC.W $0013,$0016,$0015,$0017		; blu1
_COLORS_END:
BLUE_COLS1:
		DC.W $000C,$000D,$000A,$000B		; BLU3
		DC.W $0027,$0029,$002A,$002C		; BLU2
		DC.W $011E,$011D,$011C,$010E		; BLU?
		DC.W $000C,$000E,$000F,$001B		; BLU3
		DC.W $012B,$0319,$011C,$011B		; BLU?
		DC.W $000C,$000A,$010B,$000B		; BLU3_
		DC.W $0138,$013C,$0137,$013A		; blu2
		DC.W $0013,$0016,$0015,$0017		; blu1
GREEN_COLS1:
		DC.W $0343,$0051,$0131,$0160		; GREEN
		DC.W $0042,$0053,$0051,$0043		; GREEN_
		DC.W $0121,$0010,$0030,$0040		; green4
		DC.W $0110,$0120,$0131,$0242		; green3
		DC.W $0010,$0020,$0030,$0141		; green2
		DC.W $0020,$0031,$0040,$0050		; green1 !!
		DC.W $0242,$0044,$0043,$0042		; green new
PURPLE_COLS1:
		DC.W $0506,$0406,$0405,$0305		; PURPL
		DC.W $040D,$020E,$030A,$031D		; PURPL
		DC.W $011D,$040D,$030B,$030D		; PURPL_
		DC.W $0204,$0205,$0104,$0205		; PURPL
		DC.W $0302,$0403,$0302,$0204		; purlpe2_2
		DC.W $0404,$0505,$0504,$0405		; purlpe1
		DC.W $0323,$0423,$0523,$0513		; purple new
GREY_COLS1:
		DC.W $0322,$0454,$0666,$0221		; GREY
		DC.W $004C,$004E,$004F,$015F		; cyan
		DC.W $0001,$0111,$0222,$0233		; gray2
		DC.W $0577,$0677,$0777,$0678		; grey new
NOIZ_COLS1:
		DC.W $0DDD,$0000,$0ABA,$0666		; NOIZ
		DC.W $0111,$0555,$0AAA,$0EED		; NOIZ
		DC.W $0BBB,$0667,$0FFF,$0000		; NOIZ

		; ################################################################

TXT_IDX:		DC.W $0
TXT_HEADER2:	DC.B " KONEY           153  BPM  "
TXT_HEADER1:	DC.B " PLAY >         SYNTECHNO  "
TXT_BODY:		DC.B " PLEASE INSERT A CASSETTE  "
		DC.B " SYNTECHNO BY KONEY 2024   "
		DC.B " AKA AMIGA VHS             "
		DC.B " MY 3RD 40K AMIGA INTRO!   "
		DC.B " VHS FX ALL BY AMIGA HW    "
		DC.B " TO LEAVE THE CPU FREE     "
		DC.B " FOR RACING THE BEAM!!!    "
		DC.B " THESE HARDWARE TRICKS     "
		DC.B " WERE USED IN THIS INTRO:  "
		DC.B " DUAL PLAYFIELD            "
		DC.B " BEAM RACING               "
		DC.B " HARDWARE SCROLLING        "
		DC.B " COPPER JUMPS              "
		DC.B " BLITTER COPY              "
		DC.B " THE LAST ONLY OCCURRING   "
		DC.B " ON VERTICAL BLANKINGS     "
		DC.B " I JUST REALIZED NOW WHAT  "
		DC.B " A PITA IT IS TO WRITE     "
		DC.B " MAX 26 CHARS PER LINE...  "
		DC.B " OF COURSE READING WILL    "
		DC.B " BE A NIGHTMARE :)         "
		DC.B " I FORGOT THE USUAL NOTE   "
		DC.B " ABOUT EPILEPSY DANGER...  "
		DC.B " BUT I DOUBT ANYONE WOULD  "
		DC.B " EVER EXPECT FROM ME ANY   "
		DC.B " NON GLITCHY VISUALS!      "
		DC.B " THIS NASTY TECHNO TUNE    "
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
		DC.B " PT TOY WOULDN'T ALLOW IT  "
		DC.B " REWIND...                 "
		DC.B "  - - - - - - - - - - - -  "
		DC.W $0
TXT_FOOTER:	DC.B " TRACKING ----I---------   "
		EVEN
TXT_TIMELINE_IDX:	DC.W $0
TXT_TIMELINE:	DC.L __SWAP_HEADER_TEXT
		DC.L __NOP,__NOP,__NOP,__NOP
		DC.L __NOP,__NOP,__NOP,__NOP,__NOP,__NOP,__NOP,__NOP,__NOP,__NOP
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
		DC.L __NOP,__NOP,__NOP,__NOP
		DC.L __WIPE_HEADER\.half
		DC.L __NOP,__NOP,__NOP
		DC.L __WIPE_HEADER
		DC.L __NOP
		DC.L __RESET_BODY

VPOS_HEADER:	DC.L TXT_GRID+LINE_H
VPOS_COUNTER:	DC.L TXT_GRID+LINE_H*2+bypl*6
VPOS_BODY:	DC.L TXT_GRID+LINE_H*(2+5)
VPOS_FOOTER:	DC.L TXT_GRID+LINE_H*(2+3+7)
HEADERS_PTRS:	DC.L LINE_PLAY_BUF,LINE_KONEY_BUF

RELOC3:		DC.L NOISE_UPD_PLANE3
RELOC5:		DC.L NOISE_UPD_PLANE5
		DC.L 0
NOISE_UPD_PLANE3:	DC.L PLANE3
		DC.L 0
NOISE_UPD_PLANE5:	DC.L PLANE5
NOISE_COP_PTR3:	DC.L COPPER\.BplPtrs+2+24
NOISE_COP_PTR5:	DC.L COPPER\.BplPtrs+2+40
V_LINE_IDX:	DC.B 0,0
SCANLINE_IDX0:	DC.W $30	; GOES BACK
SCANLINE_IDX1:	DC.W $50
SCANLINE_IDX2:	DC.W $60
SCANLINE_IDX3:	DC.W $70	; WHEN MEETS 0 1&2 ARE ENTAGLED
SCANLINE_IDX4:	DC.W $80
SCANLINE_IDX5:	DC.W $0	; ENTANGLED
SCANLINE_IDX6:	DC.W $F	; ENTANGLED
GRADIENT_PTRS:	DS.L COP_FRAMES

;*******************************************************************************
	SECTION	ChipData,DATA_C	;declared data that must be in chipmem
;*******************************************************************************
		DS.B he/4*bypl		; For PointPtr...
		DS.B he*bypl		; For PointPtr...

FONT:		INCBIN "VHS_font.raw",0
		EVEN

MED_MODULE:	INCBIN "med/SYNTECHNO_stripped.med"
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
	DC.W $0180,$0000
	;DC.W $0182,$00D1,$0184,$0F0E	; Managed by CPU
	DC.W $0186,$000F
	DC.W $0188,$0FFF,$018A,$0DEF,$018C,$0DDD,$018E,$0CDE

	IFEQ DYNCOPPER
	.PaletteBG:
	;DC.W $0192,$000A,$0196,$000C,$019A,$000D,$019E,$000F
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

;*******************************************************************************
	SECTION ChipBuffers,BSS_C	;BSS doesn't count toward exe size
;*******************************************************************************
LINE_PLAY_BUF:	DS.B LINE_H
LINE_KONEY_BUF:	DS.B LINE_H
LINE_BUF:		DS.B LINE_H
DUMMY:		DS.B he/8*bypl
TXT_GRID:		DS.B he*bypl
DUMMY0:		DS.B he/8*bypl
;TXT_SCROLBUF:	DS.B (bypl+1)*FONT_H
PLANE3:		DS.B he*bypl	; NOIZE
PLANE4:		DS.B he*bypl
PLANE5:		DS.B he*bypl	; NOIZE
PLANE6:		DS.B he*bypl
PLANE1:		DS.B he*bypl
PLANE2:		DS.B (he+2)*bypl
DUMMY1:		DS.B he*8*bypl
	IFNE DYNCOPPER
	COPPER_BUFFER:	DS.W COP_FRAMES*(COP_WAITS_SIZE+COP_SIZE_DIFF)
			DS.L COP_WAITS
	ENDC
END
