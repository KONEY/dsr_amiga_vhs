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
COP_WAITS_SIZE	EQU COP_WAITS*(4+4)
COP_SIZE_DIFF	EQU 4+4+4+4+4+4+8+8+4
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
	BSR.W	__PREFILLS_BPLS
	BSR.W	__PREFILLS_COLORS
	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC

	IFNE MED_PLAY_ENABLE
	;MOVE.W	#2,MED_START_POS		; skip to pos# after first block
	MOVE.W	#%1000000000001100,INTENA	; Master and lev6	; NO COPPER-IRQ!
	JSR	_startmusic
	ENDC

	LEA	PREFILLED_COLORS_BLUE0,A6
	MOVE.L	#COPPER,COP1LC	; ## POINT COPPERLIST ##
;********************  main loop  ********************
MainLoop:
	;* FOR TIMED EVENTS ON BLOCK ****
	MOVE.W	MED_SECT_POS,D5
	LSL.W	#2,D5		; CALCULATES OFFSET (OPTIMIZED)
	LEA	TIMELINE,A3
	MOVE.L	(A3,D5),A4	; THANKS HEDGEHOG!!
	JSR	(A4)		; EXECUTE SUBROUTINE BLOCK#

	BTST	#6,$BFE001		; POTINP - LMB pressed?
	BNE.S	.skip
	;LEA	DB,A0
	;MOVEA.L	_module-DB(A0),A2
	;MOVE.W	mmd_psecnum(A2),D0	;get play sequence number
	;CLR.W	$100		; DEBUG | w 0 100 2
	.skip:

	BSR.W	__RACE_THE_BEAM

	CMP.L	#_COLORS_END,A6
	BLO.S	.dontResetColors
	LEA	PREFILLED_COLORS_BLUE0,A6
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

	LEA	COPPER\.BplPtrs+2,A1	; VERTICAL TXT
	MOVE.W	V_IDX1,D0			; VERTICAL TXT
	BSR.W	__V_DISPLACE		; VERTICAL TXT
	MOVE.W	D0,V_IDX1			; VERTICAL TXT

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
	BSR.W	__STATIC_NOISE

	BRA.W	.evenFrame
	.oddFrame:			; #############

	LEA	COPPER\.BplPtrs+2+16,A1	; VERTICAL TXT
	MOVE.W	V_IDX2,D0			; VERTICAL TXT
	BSR.W	__V_DISPLACE		; VERTICAL TXT
	MOVE.W	D0,V_IDX2			; VERTICAL TXT

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
	MOVE.L	(A0),D1
	BSR.W	PokePtrs
	SWAP	D1
	MOVE.L	D1,(A0)			; Clear the inital mess?
	ENDC
	MOVEM.L	NOISE_COP_PTR3(PC),A0-A1
	EXG	A0,A1
	MOVEM.L	A0-A1,NOISE_COP_PTR3
	; ## SWAP FOR NEXT ####
	MOVEM.L	RELOC3(PC),A0-A1
	EXG	A0,A1
	MOVEM.L	A0-A1,RELOC3
	; ## UPDATE POINTERS ##
	BSR.W	__STATIC_NOISE
	.evenFrame:			; ###########

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
	;CLR.L	(A0)		; Clear the inital mess?
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

__RACE_THE_BEAM:
	MOVEM.W	SCANLINE_IDX0(PC),D0-D6
	LEA	$DFF000,A1
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
	MOVE.B	D0,D1
	MOVE.B	D0,D2
	MOVE.W	#$0CCD,$194(A1)
	MOVE.W	#16,BPL1MOD
	MOVE.W	#-20,BPL2MOD
	ADD.B	#$2,D3
	BRA.W	.waitNextRaster
	.notSameLine:
	CMP.B	D1,D2
	BNE.S	.notSameLine2
	MOVE.B	#$0,V_LINE_IDX
	MOVE.W	D1,BPL1MOD
	MOVE.W	#20,BPL2MOD
	ADD.B	#$1,D2
	BRA.W	.waitNextRaster
	.notSameLine2:
	CMP.B	D4,D5
	BNE.S	.notSameLine3
	MOVE.W	#12,BPL1MOD
	MOVE.W	#10,BPL2MOD
	ADD.B	#$1,D5
	.notSameLine3:

	.waitNextRaster:
	MOVE.B	VHPOSR,D7
	BEQ.S	.waitNextRaster

	CMP.B	D0,D7
	BNE.S	.noLine0
	MOVE.W	#$9,BPLCON1	; HW SCROLL
	MOVE.W	0(A6),$192(A1)
	MOVE.W	2(A6),$196(A1)
	MOVE.W	4(A6),$19A(A1)
	MOVE.W	6(A6),$19E(A1)
	BRA.S	.waitNextRaster
	.noLine0:

	CMP.B	D1,D7
	BNE.S	.noLine1
	MOVE.W	#$4,BPLCON1	; HW SCROLL
	MOVE.W	08(A6),$192(A1)
	MOVE.W	10(A6),$196(A1)
	MOVE.W	12(A6),$19A(A1)
	MOVE.W	14(A6),$19E(A1)
	BRA.S	.waitNextRaster
	.noLine1:

	CMP.B	D2,D7
	BNE.S	.noLine2
	MOVE.W	16(A6),$192(A1)
	MOVE.W	18(A6),$196(A1)
	MOVE.W	20(A6),$19A(A1)
	MOVE.W	22(A6),$19E(A1)
	BRA.W	.waitNextRaster
	.noLine2:

	CMP.B	D3,D7
	BNE.S	.noLine3
	MOVE.W	#$8,BPLCON1	; HW SCROLL
	MOVE.W	24(A6),$192(A1)
	MOVE.W	26(A6),$196(A1)
	MOVE.W	28(A6),$19A(A1)
	MOVE.W	30(A6),$19E(A1)
	BRA.W	.waitNextRaster
	.noLine3:

	CMP.B	D4,D7
	BNE.S	.noLine4
	MOVE.W	#$7,BPLCON1	; HW SCROLL
	MOVE.W	32(A6),$192(A1)
	MOVE.W	34(A6),$196(A1)
	MOVE.W	36(A6),$19A(A1)
	MOVE.W	38(A6),$19E(A1)
	BRA.W	.waitNextRaster
	.noLine4:

	CMP.B	D5,D7		; ENTANGLED
	BNE.S	.noLine5
	MOVE.W	#$7,BPLCON1	; HW SCROLL
	MOVE.W	42(A6),$192(A1)
	MOVE.W	40(A6),$196(A1)
	MOVE.W	44(A6),$19A(A1)
	MOVE.W	46(A6),$19E(A1)
	MOVE.W	40(A6),$182(A1)
	MOVE.W	42(A6),$184(A1)
	MOVE.W	44(A6),$188(A1)
	MOVE.W	46(A6),$18E(A1)	; CHANGE TEXT COLOR
	BRA.W	.waitNextRaster
	.noLine5:

	CMP.B	D6,D7		; ENTANGLED
	BNE.S	.noLine6
	MOVE.W	#$1,BPLCON1	; HW SCROLL
	MOVE.W	16(A6),$192(A1)
	MOVE.W	18(A6),$196(A1)
	MOVE.W	20(A6),$19A(A1)
	MOVE.W	22(A6),$19E(A1)
	MOVE.W	#$0CCC,$188(A1)
	MOVE.W	#$0BCD,$18E(A1)	; REVERT TEXT COLOR
	BRA.W	.waitNextRaster
	.noLine6:

	MOVE.W	VPOSR,D7		; Read vert most sig. bits
	BTST	#0,D7
	BEQ.W	.waitNextRaster

	CMP.B	#$1F,D7		; 12.032
	BEQ.W	.waitNextRaster

	; ## UPDATES ARE BETTER HERE ##
	ADD.B	#$6,D0
	ADD.B	#$5,D1
	ADD.B	#$7,D2
	ADD.B	#$8,D3
	ADD.B	#$A,D4
	ADD.B	#$4,D5
	ADD.B	#$4,D6
	MOVEM.W	D0-D6,SCANLINE_IDX0
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
	CLR.L	(A0)		; Clear the inital mess?
	RTS

__UPDATE_V_LINE:
	LEA	COPPER\.BplPtrs+2+8+4,A1
	SUB.W	#$42,(a1)
	TST.B	V_LINE_IDX
	BNE.S	.skip
	LEA	PLANE2,A0
	MOVE.W	A0,(A1)
	.skip:
	ADD.B	#$8,V_LINE_IDX	
	RTS

__FETCH_LETTER:
	; in: A0 text_idx, A3 Text, A4 destination, A5 Font
	; out: A5 current letter from font, A4 destination
	LEA	FONT-32,A5
	ADD.L	#bypl-2,A4		; POSITIONING
	ADD.W	(A0),A3
	ADD.W	#$1,(A0)

	CLR.L	D2
	MOVE.B	(A3),D2
	SUBI.B	#$20,D2
 	LSL.W		#5,D2
	;MULU.W	#32,D2
	ADD.W	D2,A5
	RTS

__BLIT_LETTER:
	BSR.W	WaitBlitter
	MOVE.L	#(((2<<12)+%100111110000)<<16)+%00,BLTCON0
	;MOVE.W	#%0000100111110000,BLTCON0	; BLTCON0
	;MOVE.W	#%0000000000000000,BLTCON1	; BLTCON1
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

__SCROLL_X_RIGHT:				; A4 source/dest
	LEA	16(A4),A4
	BSR	WaitBlitter
	MOVE.L	#(((FONT_SCROLL<<12)+%100111110000)<<16)+%00,BLTCON0
	MOVE.W	#$FFFF,BLTAFWM
	MOVE.W	#$0000,BLTALWM
	MOVE.W	#18,BLTAMOD
	MOVE.W	#18,BLTDMOD
	MOVE.L	A4,BLTAPTH
	MOVE.L	A4,BLTDPTH
	MOVE.W	#FONT_H*64+(176)/16,BLTSIZE
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

__SCROLL_FOOTER:
	LEA	FONT+12*32,A5	; CHAR "-"
	BRA.S	.execute
	.char_I:
	LEA	FONT+40*32,A5	; CHAR "I"
	.execute:
	MOVE.L	VPOS_FOOTER,A4
	BSR.W	__SCROLL_X_RIGHT
	BSR.W	__BLIT_LETTER
	RTS

__SWAP_HEADER_TEXT:
	MOVEM.L	HEADERS_PTRS(PC),A4-A5
	EXG	A4,A5
	MOVEM.L	A4-A5,HEADERS_PTRS
	MOVE.L	VPOS_HEADER,A4
	BSR.W	__BLIT_LINE
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

__STATIC_NOISE:
	BSR.W	__RND\._byte
	MOVE.W	#$0D0E,$DFF198
	LEA	_chipzero,A5	
	LEA	PLANE2,A4
	ADD.L	#he/7*6*bypl-2,A4
	BSR.W	BlitterFill
	MOVE.W	#$00D0,$DFF198
	RTS

BlitterFill:
	BSR.W	WaitBlitter
	MOVE.L	#$AAAA5555,BLTAFWM		; BLTAFWM
	MOVE.W	#%0000100111110000,BLTCON0	; BLTCON0
	MOVE.W	#%0000000000001010,BLTCON1	; BLTCON1
	MOVE.B	D5,BLTAMOD		; BLTAMOD
	MOVE.W	#bypl*2,BLTDMOD		; Init modulo Dest D
	MOVE.L	A5,BLTAPTH		; BLTAPT  (fisso alla figura sorgente)
	MOVE.L	A4,BLTDPTH
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

__BLK_VHS:
	MOVE.W	#$0,TXT_TIMELINE_IDX
	RTS

__BLK_INTRO:
	LEA	PREFILLED_COLORS_BLUE1,A6
	RTS

__BLK_GENERIC:
	LEA	PREFILLED_COLORS_BLUE2,A6
	RTS

__BLK_KICK:
	LEA	PREFILLED_COLORS_PURP1,A6
	RTS

__BLK_CRAZY_END:
	MOVE.W	MED_SONG_POS,D0
	CMP.W	#28,D0
	BGE.W	__BLK_MIX1
	LEA	PREFILLED_COLORS_BLUE0,A6
	RTS

__BLK_MIX1:
	LEA	PREFILLED_COLORS_MIX1,A6
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

	CMP.W	#$F4,D0		; 12.032 - #$2F00
	BNE.S	.dontRecolorNoise
	MOVE.W	#$0194,(A1)+	; RECOLOR NOISE
	MOVE.W	#$0BBB,(A1)+	; RECOLOR NOISE
	.dontRecolorNoise:

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

__PREFILLS_BPLS:
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
	; ### TRACKING ----- x14 #####
	MOVE.W	#13,D7
	.loop7:
	BSR.W	__SCROLL_FOOTER
	DBRA	D7,.loop7
	; ### TRACKING ----- x14 #####
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

__PREFILLS_COLORS:
	LEA	BLUE_COLS,A4
	LEA	PREFILLED_COLORS_BLUE0,A5
	MOVE.L	24(A4),(A5)+
	MOVE.L	28(A4),(A5)+
	MOVE.L	00(A4),(A5)+
	MOVE.L	04(A4),(A5)+
	MOVE.L	40(A4),(A5)+
	MOVE.L	44(A4),(A5)+
	MOVE.L	00(A4),(A5)+
	MOVE.L	04(A4),(A5)+
	MOVE.L	16(A4),(A5)+
	MOVE.L	20(A4),(A5)+
	MOVE.L	24(A4),(A5)+
	MOVE.L	28(A4),(A5)+
	MOVE.L	40(A4),(A5)+
	MOVE.L	44(A4),(A5)+
	;LEA	PREFILLED_COLORS_BLUE1,A5
	MOVE.L	00(A4),(A5)+
	MOVE.L	04(A4),(A5)+
	MOVE.L	08(A4),(A5)+
	MOVE.L	12(A4),(A5)+
	MOVE.L	16(A4),(A5)+
	MOVE.L	20(A4),(A5)+
	MOVE.L	24(A4),(A5)+
	MOVE.L	28(A4),(A5)+
	MOVE.L	32(A4),(A5)+
	MOVE.L	36(A4),(A5)+
	MOVE.L	40(A4),(A5)+
	MOVE.L	44(A4),(A5)+
	MOVE.L	48(A4),(A5)+
	MOVE.L	52(A4),(A5)+
	;LEA	PREFILLED_COLORS_BLUE2,A5
	MOVE.L	56(A4),(A5)+
	MOVE.L	60(A4),(A5)+
	MOVE.L	00(A4),(A5)+
	MOVE.L	04(A4),(A5)+
	MOVE.L	32(A4),(A5)+
	MOVE.L	36(A4),(A5)+
	MOVE.L	20(A4),(A5)+
	MOVE.L	24(A4),(A5)+
	MOVE.L	40(A4),(A5)+
	MOVE.L	44(A4),(A5)+
	MOVE.L	16(A4),(A5)+
	MOVE.L	20(A4),(A5)+
	MOVE.L	08(A4),(A5)+
	MOVE.L	12(A4),(A5)+
	;LEA	PREFILLED_COLORS_PURP1,A5
	LEA	PURPLE_COLS,A3
	MOVE.L	24(A4),(A5)+
	MOVE.L	28(A4),(A5)+
	MOVE.L	00(A3),(A5)+
	MOVE.L	04(A3),(A5)+
	MOVE.L	08(A4),(A5)+
	MOVE.L	12(A4),(A5)+
	MOVE.L	08(A3),(A5)+
	MOVE.L	12(A3),(A5)+
	MOVE.L	32(A4),(A5)+
	MOVE.L	36(A4),(A5)+
	MOVE.L	16(A3),(A5)+
	MOVE.L	20(A3),(A5)+
	MOVE.L	48(A4),(A5)+
	MOVE.L	52(A4),(A5)+
	;LEA	PREFILLED_COLORS_PURP2,A5
	MOVE.L	40(A4),(A5)+
	MOVE.L	44(A4),(A5)+
	MOVE.L	24(A3),(A5)+
	MOVE.L	28(A3),(A5)+
	MOVE.L	16(A4),(A5)+
	MOVE.L	20(A4),(A5)+
	MOVE.L	32(A3),(A5)+
	MOVE.L	36(A3),(A5)+
	MOVE.L	56(A4),(A5)+
	MOVE.L	60(A4),(A5)+
	MOVE.L	40(A3),(A5)+
	MOVE.L	44(A3),(A5)+
	MOVE.L	00(A4),(A5)+
	MOVE.L	04(A4),(A5)+
	;LEA	PREFILLED_COLORS_GREEN1,A5
	LEA	GREEN_COLS,A2
	MOVE.L	08(A4),(A5)+
	MOVE.L	12(A4),(A5)+
	MOVE.L	00(A2),(A5)+
	MOVE.L	04(A2),(A5)+
	MOVE.L	56(A4),(A5)+
	MOVE.L	60(A4),(A5)+
	MOVE.L	08(A3),(A5)+	; PURPLE!
	MOVE.L	12(A3),(A5)+	; PURPLE!
	MOVE.L	00(A4),(A5)+
	MOVE.L	04(A4),(A5)+
	MOVE.L	08(A2),(A5)+
	MOVE.L	12(A2),(A5)+
	MOVE.L	16(A4),(A5)+
	MOVE.L	20(A4),(A5)+
	;LEA	PREFILLED_COLORS_GREEN2,A5
	MOVE.L	24(A3),(A5)+	; PURPLE!
	MOVE.L	28(A3),(A5)+	; PURPLE!
	MOVE.L	16(A2),(A5)+
	MOVE.L	20(A2),(A5)+
	MOVE.L	56(A4),(A5)+
	MOVE.L	60(A4),(A5)+
	MOVE.L	24(A2),(A5)+
	MOVE.L	28(A2),(A5)+
	MOVE.L	00(A4),(A5)+
	MOVE.L	04(A4),(A5)+
	MOVE.L	32(A2),(A5)+
	MOVE.L	36(A2),(A5)+
	MOVE.L	56(A4),(A5)+
	MOVE.L	60(A4),(A5)+
	;LEA	PREFILLED_COLORS_GRAY1,A5
	LEA	GRAY_COLS,A1
	MOVE.L	24(A4),(A5)+
	MOVE.L	28(A4),(A5)+
	MOVE.L	00(A1),(A5)+
	MOVE.L	04(A1),(A5)+
	MOVE.L	00(A4),(A5)+
	MOVE.L	04(A4),(A5)+
	MOVE.L	08(A1),(A5)+
	MOVE.L	12(A1),(A5)+
	MOVE.L	40(A4),(A5)+
	MOVE.L	44(A4),(A5)+
	MOVE.L	16(A1),(A5)+
	MOVE.L	20(A1),(A5)+
	MOVE.L	56(A4),(A5)+
	MOVE.L	60(A4),(A5)+
	MOVE.L	24(A1),(A5)+
	MOVE.L	28(A1),(A5)+
	MOVE.L	32(A4),(A5)+
	MOVE.L	36(A4),(A5)+
	;LEA	PREFILLED_COLORS_GRAY2,A5
	MOVE.L	16(A4),(A5)+
	MOVE.L	20(A4),(A5)+
	MOVE.L	24(A1),(A5)+
	MOVE.L	28(A1),(A5)+
	MOVE.L	00(A4),(A5)+
	MOVE.L	04(A4),(A5)+
	MOVE.L	00(A1),(A5)+
	MOVE.L	04(A1),(A5)+
	MOVE.L	48(A4),(A5)+
	MOVE.L	52(A4),(A5)+
	MOVE.L	00(A2),(A5)+	; GREEN!
	MOVE.L	04(A2),(A5)+	; GREEN!
	MOVE.L	24(A4),(A5)+
	MOVE.L	28(A4),(A5)+
	MOVE.L	16(A1),(A5)+
	MOVE.L	20(A1),(A5)+
	MOVE.L	56(A4),(A5)+
	MOVE.L	60(A4),(A5)+
	;LEA	PREFILLED_COLORS_MIX1,A5
	MOVE.L	20(A4),(A5)+
	MOVE.L	24(A4),(A5)+
	MOVE.L	00(A1),(A5)+
	MOVE.L	04(A1),(A5)+
	MOVE.L	08(A4),(A5)+
	MOVE.L	12(A4),(A5)+
	MOVE.L	16(A2),(A5)+
	MOVE.L	28(A2),(A5)+
	MOVE.L	32(A4),(A5)+
	MOVE.L	36(A4),(A5)+
	MOVE.L	40(A3),(A5)+
	MOVE.L	44(A3),(A5)+
	MOVE.L	48(A4),(A5)+
	MOVE.L	52(A4),(A5)+
	;LEA	PREFILLED_COLORS_MIX2,A5
	MOVE.L	56(A4),(A5)+
	MOVE.L	60(A4),(A5)+
	MOVE.L	00(A1),(A5)+
	MOVE.L	04(A1),(A5)+
	MOVE.L	32(A3),(A5)+
	MOVE.L	36(A3),(A5)+
	MOVE.L	20(A4),(A5)+
	MOVE.L	24(A4),(A5)+
	MOVE.L	40(A3),(A5)+
	MOVE.L	44(A3),(A5)+
	MOVE.L	16(A2),(A5)+
	MOVE.L	20(A2),(A5)+
	MOVE.L	08(A1),(A5)+
	MOVE.L	12(A1),(A5)+
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
	MOVE.L	#$AAAA5555,D3	; MORE TEXTURE
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
	AND.L	D3,D1		; MORE TEXTURE
	ROR.L	D3
	MOVE.L	D1,40(A4)
	ROR.L	D1		; MORE TEXTURE
	MOVE.L	D1,(A4)+
	DBRA	D6,.innerloop
	LEA	40(A4),A4
	MOVE.W	D7,$DFF180	; SHOW ACTIVITY
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

BLUE_COLS:
		DC.W $000C,$000D,$000A,$000B		; BLU3
		DC.W $0027,$0029,$002A,$002C		; BLU2
		DC.W $011E,$011D,$011C,$010E		; BLU?
		DC.W $000C,$000E,$000F,$001B		; BLU3
		DC.W $012B,$0319,$011C,$011B		; BLU?
		DC.W $000C,$000A,$010B,$000B		; BLU3_
		DC.W $0138,$013C,$0137,$013A		; blu2
		DC.W $0013,$0016,$0015,$0017		; blu1
GREEN_COLS:
		DC.W $0343,$0051,$0131,$0160		; GREEN
		DC.W $0042,$0053,$0051,$0043		; GREEN_
		DC.W $0242,$0141,$0151,$0131		; GREEN OK
		DC.W $0241,$0140,$0231,$0131		; GREEN OK2
		DC.W $0010,$0020,$0030,$0141		; green2
		DC.W $0020,$0031,$0040,$0050		; green1 !!
PURPLE_COLS:
		DC.W $0506,$0406,$0405,$0305		; PURPL
		DC.W $040D,$020E,$030A,$031D		; PURPL
		DC.W $011D,$040D,$030B,$030D		; PURPL_
		DC.W $0204,$0205,$0104,$0205		; PURPL
		DC.W $0302,$0403,$0302,$0204		; purlpe2_2
		DC.W $0404,$0505,$0504,$0405		; purlpe1
		DC.W $0323,$0423,$0523,$0513		; purple new
GRAY_COLS:
		DC.W $0443,$0445,$0344,$0545		; GREY_OK
		DC.W $004C,$004E,$004F,$015F		; cyan
		DC.W $0555,$0455,$0566,$0544		; GREY_OK2
		DC.W $0577,$0677,$0777,$0678		; grey new
PREFILLED_COLORS_BLUE0:
		DS.W 4*7
PREFILLED_COLORS_BLUE1:
		DS.W 4*7
PREFILLED_COLORS_BLUE2:
		DS.W 4*7
PREFILLED_COLORS_PURP1:
		DS.W 4*7
PREFILLED_COLORS_PURP2:
		DS.W 4*7
PREFILLED_COLORS_GREEN1:
		DS.W 4*7
PREFILLED_COLORS_GREEN2:
		DS.W 4*7
PREFILLED_COLORS_GRAY1:
		DS.W 4*9
PREFILLED_COLORS_GRAY2:
		DS.W 4*9
PREFILLED_COLORS_MIX1:
		DS.W 4*7
PREFILLED_COLORS_MIX2:
		DS.W 4*7
_COLORS_END:

TXT_IDX:		DC.W $0
TXT_HEADER2:	DC.B " KONEY            153 BPM  "
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
		DC.B " AS I TYPE REVISON 24 IS   "
		DC.B " TAKING PLACE AND AGAIN I  "
		DC.B " COULDN'T ATTEND IT :(     "
		DC.B " ANYWAY SOON I WILL BE     "
		DC.B " RUNNING ALL NEW PRODZ ON  "
		DC.B " MY AMIGA 1000! SO COOL!   "
		DC.B "     REAL IRON RULEZ!      "
		DC.B " GREETINGS TIME NOW -----  "
		DC.B " RAMON/DSR AND ALL DESIRE  "
  		DC.B " PHOTON/SCOOPEX      "
		DC.B " DANSCOTT/LEMON FROM EAB   "
		DC.B " FOR SUGGESTING A BLITTER    "
  		DC.B " TRICK TO GENERATE NOIZE   "
    		DC.B " NO MORE BYTES FOR TXT SO "
		DC.B " REWIND...                 "
		DC.W $0
TXT_FOOTER:	DC.B " TRACKING                  "
		EVEN
TXT_TIMELINE_IDX:	DC.W $0
TXT_TIMELINE:	DC.L __SCROLL_FOOTER
		DC.L __SWAP_HEADER_TEXT
		DC.L __NOP,__NOP,__NOP,__NOP
		DC.L __SCROLL_FOOTER,__NOP,__NOP,__NOP,__NOP,__NOP,__SCROLL_FOOTER,__NOP,__NOP,__NOP,__SCROLL_FOOTER
		DC.L __NOP,__NOP,__NOP,__NOP,__NOP,__NOP,__NOP,__NOP,__NOP,__SCROLL_FOOTER
		DC.L __SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY
		DC.L __SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY
		DC.L __SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY
		DC.L __SCROLL_FOOTER
		DC.L __SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY
		DC.L __SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY
		DC.L __SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY
		DC.L __SCROLL_BODY,__LETTER_BODY,__SCROLL_BODY,__LETTER_BODY
		DC.L __WIPE_BODY\.half
		DC.L __SCROLL_FOOTER
		DC.L __PLACE_BODY\.half
		DC.L __NOP
		DC.L __WIPE_BODY
		DC.L __NOP
		DC.L __WIPE_BODY\.half
		DC.L __NOP,__NOP,__SCROLL_FOOTER
		DC.L __PLACE_BODY\.half
		DC.L __NOP,__SCROLL_FOOTER,__NOP	
		DC.L __PLACE_BODY
		DC.L __NOP,__NOP,__NOP,__SCROLL_FOOTER
		DC.L __NOP,__NOP,__NOP,__SCROLL_FOOTER
		DC.L __WIPE_HEADER\.half
		DC.L __NOP,__NOP,__SCROLL_FOOTER
		DC.L __WIPE_HEADER
		DC.L __NOP,__NOP,__SCROLL_FOOTER
		DC.L __NOP,__SCROLL_FOOTER\.char_I
		DC.L __RESET_BODY

TIMELINE:		DC.L __BLK_VHS,__BLK_INTRO,__BLK_GENERIC,__BLK_KICK

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
SCANLINE_IDX0:	DC.W $00	; GOES BACK - NOT ANYMORE :)
SCANLINE_IDX1:	DC.W $00
SCANLINE_IDX2:	DC.W $00
SCANLINE_IDX3:	DC.W $00	; WHEN MEETS 0 1&2 ARE ENTANGLED
SCANLINE_IDX4:	DC.W $00
SCANLINE_IDX5:	DC.W $00	; ENTANGLED
SCANLINE_IDX6:	DC.W $05	; ENTANGLED
GRADIENT_PTRS:	DS.L COP_FRAMES

;*******************************************************************************
	SECTION	ChipData,DATA_C	;declared data that must be in chipmem
;*******************************************************************************
		DS.B he/4*bypl		; For PointPtr...
		DS.B he*bypl		; For PointPtr...

FONT:		INCBIN "VHS_font.raw",0
		EVEN

MED_MODULE:	INCBIN "med/SYNTECHNO+VHS_stripped.med"
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
	DC.W $0190,$0AAA,$0194,$0445,$0198,$0666,$019C,$0888

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
