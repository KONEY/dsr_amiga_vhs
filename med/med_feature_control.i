;****** Feature control ******
; The less features you include, the faster and shorter the play-routine  will be.

AUDDEV		EQU	0	;1 = allocate channels using audio.device
SYNTH		EQU	1	;1 = include synth-sound handler
CHECK		EQU	0	;1 = do range checkings (track, sample in mem etc.)
RELVOL		EQU	0	;1 = include relative volume handling code
IFFMOCT		EQU	0	;1 = play IFF multi-octave samples/ExtSamples correctly
HOLD		EQU	0	;1 = handle hold/decay
PLAYMMD0 		EQU	0	;1 = play old MMD0 modules

; #### KONEY ####
MED_PLAY_ENABLE	EQU	1	; Global disable of all MED code.
STOP_AT_END	EQU	0	; Dont loop at end of sequence | TO VERIFY
START_POS		EQU	0	; After SEQ 0 jump to value in MED_START_POS
SKIP_TO_NEXT	EQU	0	; LMB to skip to next block. Just an example
STEP_SEQ		EQU	0	; Enable step sequencer in MED_STEPSEQ_POS
SONG_POS_TRACKING	EQU	0	; Keep track of song position in MED_SONG_POS
BLOCK_LINE_TRACKING	EQU	0	; Keep track of block line in MED_BLOCK_LINE
INSTR_TRACKING	EQU	0	; Tracks instruments#, notes and levels for each track
SPLIT_RELOCS	EQU	0	; Samples are expected at label MED_SAMPLES so all the rest can be in fast ram
; #### KONEY ####

;****** Timing control ******
VBLANK		EQU	0	;1 = use VBlank interrupt (when absolutely necessary)
CIAB		EQU	1	;1 = use CIA timers (default)
; Please use CIAB whenever possible to avoid problems with variable
; VBlank speeds and to allow the use of command F01 - FF0 (set tempo)
; If both are set to 0, the timing is left for you (never set both to 1!!),
; then you just call _IntHandler for each timing pulse.
; ======
; If you need vertical blanking timing, you can set VBLANK to 1 and CIAB to 0.
; In normal use this is not recommended (because of the 16 % difference in
; playing speed with NTSC and PAL Amigas), but if tight synchronization to
; vertical blanking (e.g. in most demos/games) is required, VBLANK can be
; used.
; For VBlank timing, the song has to be composed with primary tempo of about
; 33. The primary tempo cannot be changed with command F. Only the secondary
; tempo control can be used (command 9).
;============================================================================

;If you are making a demo/game with only a single tune you'd like to
;incorporate in the code (like "easyplayer.a" of MED V3), set the following
;flag to 1. This requires an assembler with INCBIN (or equivalent) directive.
;You have to insert the module name to the INCBIN statement (located near the
;end of this file, on line 2052).
EASY		EQU	1
;Call _startmusic to play the music, and _endmusic to stop it (before
;exiting). Note: don't call _startmusic twice!! This would cause the module
;to be relocated twice (= Guru). If you need to stop and continue playing,
;don't use the EASY routines, use PlayModule/StopPlayer... instead.
;============================================================================
; The MMD structure offsets
mmd_id		EQU	0
mmd_modlen	EQU	4
mmd_songinfo	EQU	8
mmd_psecnum	EQU	12	; these two for MMD2s only!
mmd_pseq 		EQU	14	; these two for MMD2s only!
mmd_blockarr	EQU	16
mmd_smplarr	EQU	24
mmd_expdata	EQU	32
mmd_pstate	EQU	40	; <0 = play song, 0 = don't play, >0 = play block
mmd_pblock	EQU	42
mmd_pline 	EQU	44
mmd_pseqnum	EQU	46
mmd_counter	EQU	50
mmd_songsleft	EQU	51

; The Song structure
; Instrument data here (504 bytes = 63 * 8)
msng_numblocks	EQU	504
msng_songlen	EQU	506
msng_playseq	EQU	508
msng_deftempo	EQU	764
msng_playtransp	EQU	766
msng_flags	EQU	767
msng_flags2	EQU	768
msng_tempo2	EQU	769
; msng_trkvol applies to MMD0/MMD1 only.
msng_trkvol	EQU	770
msng_mastervol	EQU	786
msng_numsamples	EQU	787
; Fields below apply to MMD2 modules only.
msng_pseqs	EQU	508
msng_sections	EQU	512
msng_trkvoltbl	EQU	516
msng_numtracks	EQU	520
msng_numpseqs	EQU	522

; Instrument data
inst_repeat	EQU	0
inst_replen	EQU	2
inst_midich	EQU	4
inst_midipreset	EQU	5
inst_svol 	EQU	6
inst_strans	EQU	7

; Audio hardware offsets
ac_ptr		EQU	$00
ac_len		EQU	$04
ac_per		EQU	$06
ac_vol		EQU	$08

; Trackdata sizes
T03SZ		EQU	106
T415SZ		EQU	22
;offset of trk_audioaddr
TAAOFFS		EQU	24
TTMPVOLOFFS	EQU	102

; Maximum number of tracks allowed. If you don't need this much tracks,
; you can decrease the number to save some space. (Be sure that the
; song really has no more than MAX_NUMTRACKS tracks. Minimum allowed
; value = 4.)
MAX_NUMTRACKS	EQU	4

; This value is used for MMD0/1 conversion. If MAX_NUMTRACKS <= 16,
; this should be the same. If MAX_NUMTRACKS > 16, this should be 16.
MAX_MMD1_TRACKS	EQU	4