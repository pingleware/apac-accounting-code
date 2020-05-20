      $set LINKCOUNT"468"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	  ****	   ******   ********   *
      *   **	**     **     **    **	 **  **   **	**  **	       *
      *   **	       **     **    **	**    **  **	**  *******    *
      *    ******      **     *******	**    **   *******	  **   *
      * 	**     **     **	**    **	**	  **   *
      *   **	**     **     **	 **  **   **	**  **	  **   *
      *    ******      **     **	  ****	   ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *   D E L E T E	S T O C K   R A N G E	( FLAG 9 TO STK-DISC)  *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STP095.
000030 AUTHOR.         J.W.LEMMON (CSS).
000040 DATE-WRITTEN.   AUGUST 1989.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1989 - 2008
				     by James William Lemmon.
				       (Id No. 4412165050082).

		   All rights reserved.

		   e-mail jwlemmon@gmail.com.

000050 SECURITY.
		   This program is free software; you can redistribute
		   it and/or modify it under the terms of the GNU General
		   Public License as published by the Free Software
		   Foundation; either version 2 of the License, or (at
		   your option) any later version.

		   This program is distributed in the hope that it will
		   be useful, but WITHOUT ANY WARRANTY; without even the
		   implied warranty of MERCHANTABILITY or FITNESS FOR A
		   PARTICULAR PURPOSE.	See the GNU General Public License
		   for more details.

		   You should have received a copy of the GNU General
		   Public License along with this program; if not, write
		   to the Free Software Foundation, Inc., 59 Temple Place
		   - Suite 330, Boston, MA 02111-1307, USA.

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090                 CONSOLE IS CRT
                       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

000120 COPY CONTROL.SL.

000120 COPY DEPART.SL.

000120 COPY PARAM.SL.

000140 COPY SPARTS.SL.

000140 COPY STKMEM.SL.

000140 COPY STOCK.SL.

       COPY WSTOCK.SL.

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

       COPY DEPART.FDE.

000280 COPY PARAM.FDE.

000140 COPY SPARTS.FDE.

000140 COPY STKMEM.FDE.

000300 COPY STOCK.FDE.

000280 COPY WSTOCK.FDE.

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001700 77  WS-DISC	   PIC	999V99	 COMP-3.
001710 77  WS-SUB	   PIC	9(04)	 COMP-5.
001710 77  WS-S1	   PIC	9(04)	 COMP-5.
001720 77  WS-S2	   PIC	9(04)	 COMP-5.
001730 77  WS-S3	   PIC	9(04)	 COMP-5.
001740 77  WS-S4	   PIC	9(04)	 COMP-5.
001750 77  WS-S5	   PIC	9(04)	 COMP-5.
001760 77  WS-S6	   PIC	9(04)	 COMP-5.
001770 77  WS-S7	   PIC	9(04)	 COMP-5.
001780 77  WS-S8	   PIC	9(04)	 COMP-5.
001790 77  WS-IXS	   PIC	9(04)	 COMP-5.
001800 77  WS-IXS1	   PIC	9(04)	 COMP-5.
001810 77  WS-IXS2	   PIC	9(04)	 COMP-5.
001820 77  WS-IXS3	   PIC	9(04)	 COMP-5.
001830 77  WS-IXS4	   PIC	9(04)	 COMP-5.
001840 77  WS-TRFKEY	   PIC	9(06)	 COMP-5.
001840 77  WS-PRCKEY	   PIC	9(06)	 COMP-5.
001860 77  WS-PARKEY	   PIC	9(06)	 COMP-5.
001870 77  WS-NETKEY	   PIC	9(06)	 COMP-5.
001890 77  WS-RECKEY	   PIC	9(06)	 COMP-5.
001900 77  WS-RECOVER	   PIC	9(06)	 COMP-5.
001900 77  WS-RECORDS	   PIC	9(06)	 COMP-5.
001910 77  WS-TRANS	   PIC	9(04)	 COMP-5 VALUE 1.
001920 77  WS-KEYSTR	   PIC	9(04)	 COMP-5.
001940 77  WS-MONTHS	   PIC S9(01)V99 COMP-3.
001960 77  WS-KEY1	   PIC	9(04)	 COMP-5.
001980 77  WS-ITM	   PIC	X(18).
       77  WS-ETYPE	   PIC	X(01).
002010 77  WS-PAGE	   PIC	9(04)	 COMP-5.
003050 77  WS-NORM         PIC  X(04).
003060 77  WS-COND         PIC  X(04).
003070 77  WS-EXPP         PIC  X(04).
003080 77  WS-ECAN         PIC  X(04).
       77  WS-10CPI        PIC  X(04).
       77  WS-12CPI        PIC  X(04).
       77  WS-17CPI        PIC  X(04).
002020 77  WS-OPTION	   PIC	X(01).
002020 77  WS-SKIP	   PIC	X(01).
002040 77  WS-ADJ	   PIC	X(01).
002070 77  WS-ERROR	   PIC	9(01).
002120 77  WS-AMEND	   PIC	9(01)	 VALUE 0.
002140 77  WS-CONREC	   PIC	9(01) VALUE ZERO.
       77  WS-WH-QUANT	   PIC S9(08)V999 COMP-3.
002150 77  WS-LEDG	   PIC	X(04).
002160 77  WS-LEDG1	   PIC	X(04).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
002200 77  WS-COMP	   PIC	9(01).
002210 77  WS-WEEK	   PIC	9(01).
002220 77  WS-TYPE	   PIC	X(01).
002260 77  WS-BATCH	   PIC	9(05).
002270 77  WS-PASSWORD	   PIC	X(08).
002280 77  WS-PASS	   PIC	X(08).
001330 77  WS-DESC-H1	   PIC	X(14) VALUE "Description".
       77  WS-DESC-H2	   PIC	X(14) VALUE "Description 2".
       77  WS-RT-HD	   PIC	X(10) VALUE "-Retail   ".
       77  WS-WS-HD	   PIC	X(10) VALUE "-Wholesale".
       77  WS-CS-HD	   PIC	X(10) VALUE "-Cash sale".
       77  WS-NODEP	   PIC	X(30) VALUE
			   "NO SUCH   D E P A R T M E N T".
       77  PRG-NAME	   PIC	X(12) VALUE "STP\STPLOOK".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440         05  WS-TCHR PIC X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC  X(78).
               05  FILLER  PIC X(01).
002430     03  WS-TOP-LNE2.
002440         05  WS-TCH  PIC X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
002450	   03  WS-MID-LNE.
002460         05  WS-MCHR PIC X(01) OCCURS 80.
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
002450	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
002470	   03  WS-BOT-LNE.
002480         05  WS-BCHR PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC X(01).
               05  WS-F-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002470     03  WS-BOT-LNE2.
002480         05  WS-BCH  PIC X(01) OCCURS 80.
003240     03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
           03  WS-MM-LINE.
               05  FILLER  PIC  X(01) VALUE X"B3".
               05  FILLER  PIC  X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

002330 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "STP".
	   03  WS-PROG	       PIC  X(03) VALUE "095".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002350 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

000140 COPY SPARTS.ID.

000140 COPY STKMEM.ID.

000300 COPY STOCK.ID.

000360 COPY WSTOCK.ID.

       COPY W05.VAT.

       01  W05-EXTRA.
	   03  W05-ROUND       PIC  X(01).
           03  W05-ROUND-VAL   PIC  9V9(02).
           03  W05-RVAL REDEFINES W05-ROUND-VAL.
               05  W05-RAND    PIC  9(01).
               05  W05-CENTS   PIC  9(02).
           03  W05-RVAL2 REDEFINES W05-ROUND-VAL.
               05  FILLER      PIC  9(01).
               05  W05-RVC1    PIC  9(01).
               05  W05-RVC2    PIC  9(01).
           03  W05-HLF-VAL     PIC  9V99.
           03  W05-TQTR-VAL    PIC  9V99.
           03  W05-CALC-VAL    PIC  9(02)V99.
           03  W05-CALC-RRCC REDEFINES W05-CALC-VAL.
               05  W05-RVR1    PIC  9(01).
               05  W05-RVRCC   PIC  9(01)V99.
       01  W09-STOCK.
002780	   03  W09-MARKUP     PIC S9(03)V9999  COMP-3.
002790	   03  W09-CMARKUP    PIC S9(03)V9999  COMP-3.
002790	   03  W09-WMARKUP    PIC S9(03)V9999  COMP-3.

       COPY W10.STK.

       COPY W11.STK.

       COPY W12.WS.

       COPY W15.STK.

       COPY W20.WS.

003700 01  W25-CALCS.
005040     03  W25-RESULT      PIC 9(05)V99.
004330     03  W25-RESULT1 REDEFINES W25-RESULT.
004340         05  W25-WHOLE   PIC 9(05).
004350         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
003750     03  W25-SCOST      PIC S9(09)V99 COMP-3.
003760     03  W25-PCOST      PIC S9(09)V99 COMP-3.
003760     03  W25-PCST       PIC S9(09)V99 COMP-3.
003760     03  W25-PTOT       PIC S9(09)V99 COMP-3.
003770     03  W25-TSELL      PIC S9(09)V99 COMP-3.
003770     03  W25-TOTAL      PIC S9(09)V99 COMP-3.
003780     03  W25-VALUE      PIC S9(09)V99 COMP-3.
003780     03  W25-ADJUST     PIC S9(09)V99 COMP-3.

       COPY W40.WS.

       COPY FUNCTION.WS.

003800 COPY W50.WS.

       COPY W75.STK.

003930 01  W80-EDIT.
003940     03  W80-CNO        PIC 9(04).
003950     03  W80-V1.
003960         05  W80-S7V2   PIC Z(06)9.99-.
003970     03  W80-V2.
003980         05  W80-S5V2   PIC Z(04)9.99-.
003990     03  W80-DTE.
004000	       05  W80-DATE   PIC Z9/99/9999.
004010     03  W80-QNT.
004020         05  W80-5      PIC Z(04)9.99.
004030     03  W80-QNT1 REDEFINES W80-QNT.
004040         05  W80-3V2    PIC Z(02)9.9999.
004050     03  W80-NO.
004060         05  W80-5N     PIC 9(05).
004070         05  W80-REC REDEFINES W80-5N
004080                        PIC ZZZZ9.
004090     03  W80-DEL.
004100         05  W80-2N      PIC 9(02).
006270 01  W85-PASS.
006280     03  W85-SUPER       PIC  X(06)    OCCURS 9.
006290     03  W85-ENTRY       PIC  9(02)    COMP-3.
006300     03  W85-MARG        PIC S9(02)V99.
004110 01  W90-BAL             PIC S9(07)V99 COMP-3.
004120 01  W95-COMP            PIC X(40).
005120 01  W100-EDIT.
003970     03  W100-V2.
003980         05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.
       01  W200-MEMO.
           03  W200-MEM-FLDS. 
               05  W200-MEM1   PIC  X(60).
               05  W200-MEM2   PIC  X(60).
               05  W200-MEM3   PIC  X(60).
               05  W200-MEM4   PIC  X(60).
               05  W200-MEM5   PIC  X(60).
               05  W200-MEM6   PIC  X(60).
               05  W200-MEM7   PIC  X(60).
               05  W200-MEM8   PIC  X(60).
               05  W200-MEM9   PIC  X(60).
               05  W200-MEM10  PIC  X(60).
               05  W200-MEM11  PIC  X(60).
               05  W200-MEM12  PIC  X(60).
           03  W200-MEM-FOC REDEFINES W200-MEM-FLDS.
               05  W200-MEM    PIC  X(60) OCCURS 12.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004130 SCREEN SECTION.

       COPY BLANK.CRT.

       COPY STKLUP.CRT.

       01 S05-ITEM.
	  03  LINE 12 COLUMN 23 FOREGROUND-COLOR 3 HIGHLIGHT
				BACKGROUND-COLOR 5
				PIC  X(14) USING W10-ITEM.

       01 S05-ITM.
	  03  LINE 12 COLUMN 23 FOREGROUND-COLOR 3 HIGHLIGHT
				BACKGROUND-COLOR 5
				PIC  X(14) USING W10-ITEM.
	  03	      COLUMN 37 FOREGROUND-COLOR 0
				BACKGROUND-COLOR 5
				VALUE "/".
	  03	      COLUMN 38 FOREGROUND-COLOR 3 HIGHLIGHT
				BACKGROUND-COLOR 5
				PIC  X(03) USING W10-ITM.

       01 S05-EXT-ITEM.
	  03  LINE 12 COLUMN 23 FOREGROUND-COLOR 3 HIGHLIGHT
				BACKGROUND-COLOR 5
				PIC  X(18) USING W10-EXT-ITEM.

       01 S06-ITEM.
	  03  LINE 13 COLUMN 23 FOREGROUND-COLOR 3 HIGHLIGHT
				BACKGROUND-COLOR 5
				PIC  X(14) USING W10-ITEM.

       01 S06-ITM.
	  03  LINE 13 COLUMN 23 FOREGROUND-COLOR 3 HIGHLIGHT
				BACKGROUND-COLOR 5
				PIC  X(14) USING W10-ITEM.
	  03	      COLUMN 37 FOREGROUND-COLOR 0
				BACKGROUND-COLOR 5
				VALUE "/".
	  03	      COLUMN 38 FOREGROUND-COLOR 3 HIGHLIGHT
				BACKGROUND-COLOR 5
				PIC  X(03) USING W10-ITM.

       01 S06-EXT-ITEM.
	  03  LINE 13 COLUMN 23 FOREGROUND-COLOR 3 HIGHLIGHT
				BACKGROUND-COLOR 5
				PIC  X(18) USING W10-EXT-ITEM.

       01  S40.
	   03  FOREGROUND-COLOR 11 BACKGROUND-COLOR 5
				PIC X(04) USING WS-LEDG AUTO.

       01  S-MEM.
           03  LINE  6 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G9.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G10.
           03  LINE  7 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G3.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7 VALUE 
           "   M E M O / R E M A R K S / S P E C I F I C A T I O N S    
      -    "".
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G3.
           03  LINE  8 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G14.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-HME-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G13.
           03  LINE  9 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 10 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 11 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 12 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 13 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 14 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 15 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 16 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 17 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 18 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 19 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 20 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 21 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G11.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G12.
       01  SA-MEM.
           03  LINE  9 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM1 AUTO.
           03  LINE 10 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM2 AUTO.
           03  LINE 11 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM3 AUTO.
           03  LINE 12 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM4 AUTO.
           03  LINE 13 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM5 AUTO.
           03  LINE 14 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM6 AUTO.
           03  LINE 15 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM7 AUTO.
           03  LINE 16 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM8 AUTO.
           03  LINE 17 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM9 AUTO.
           03  LINE 18 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM10 AUTO.
           03  LINE 19 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM11 AUTO.
           03  LINE 20 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM12 AUTO.

       COPY S99.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
007740 AA000-MAIN	       SECTION.
007750 AA000-INIT.
003240	   IF LS0-STLEV < 9
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
007760	     PERFORM ZA000-INIT.
007780	     PERFORM BZ000.

       AA49.
	     EXIT PROGRAM.

       COPY AA50.LUP.

       COPY FUNCTION.CRT.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY AA300.STK.

      /
      *    ****    READ FILES ROUTINES
      *
008180 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY DEPART.RD.

       COPY PARAM.RD.

000140 COPY STKMEM.RD.

       COPY STOCK.RD.

       COPY SPARTS.RD.

       COPY WSTOCK.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
009850 AD000-WRITE             SECTION.

       COPY CONTROL.WR.

       COPY DEPART.WR.

       COPY PARAM.WR.

000140 COPY STKMEM.WR.

       COPY STOCK.WR.

       COPY SPARTS.WR.

       COPY WSTOCK.WR.

       COPY STOCK.1ST.

       COPY APAC.HLP.

       COPY CA000.STK.

       COPY CA100.STK.

       COPY CA200.STK.

       COPY CA500.STK.

       COPY ROUND.PRC.

      /
      *    ****    D E L E T E   A   S T O C K   R A N G E
      *
035140 BZ000	       SECTION 7.
035150 BZ00.
035160       DISPLAY CLR-SCREEN.
             DISPLAY "DELETE A RANGE OF STOCK ITEMS" AT 0226
                      WITH FOREGROUND-COLOR 15.
035170       DISPLAY "Run deletion routine (Y/N)" AT 0404.
035180 BZ02.
035190       ACCEPT WS-OPTION AT 0431 WITH FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
035250	   IF NOT (WS-OPTION = "N" OR "Y")
035260         GO TO BZ02.
035270     IF WS-OPTION = "N"
035280	       GO TO BZ999.
032000       MOVE 5              TO WS-PARKEY.
032010       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
032020       MOVE PAR-PW         TO W85-PASS. 
018260       DISPLAY "Enter password >      <" AT 2551
018270		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
018280	     ACCEPT W40-CSHPASS AT 2567
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15
018290			 AUTO NO-ECHO.
018300	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W40-CSHPASS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
018360	     MOVE 1		 TO WS-LENGTH.
018370     IF NOT (W40-CSHPASS = W85-SUPER (1) OR 
                                 W85-SUPER (7) OR
                                 "ZERODP") 
               GO TO BZ999.
018260       DISPLAY "                       " AT 2551
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.

       BZ02A.
032590	     MOVE SPACES	 TO WS-LEDG.
032600	     DISPLAY "Sales ledger Code" AT 1004.
             DISPLAY "Press 'ENTER' to exit" AT 2304.

032610 BZ03.
032620	     ACCEPT S40 AT 1022.
019250	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
032640	   IF WS-LEDG = SPACES
032650         GO TO BZ999.
009330	     MOVE WS-LEDG	 TO DPT-CODE.
	   IF (DPT-CODE = "XXXX" OR "YYYY") OR
	      (DPT-SCDE = "VAT")
032670	       MOVE "Invalid ledger number"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
032680	       GO TO BZ03.
032700       PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
032720	       MOVE "No record"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
032730	       GO TO BZ03.
032740       DISPLAY DPT-DESC AT 1045 WITH FOREGROUND-COLOR 11.
	     PERFORM CHECK-CORRECT.
	   IF WS-OPTION = "N"
               GO TO BZ03.
	     DISPLAY "Item Code - Start: " AT 1204
		     "                  "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5.
	     DISPLAY "- End  : " AT 1314
		     "                  "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5.
	   IF WS-USE-ITM = "Y"
	       DISPLAY "/" AT 1237 WITH FOREGROUND-COLOR 0
					BACKGROUND-COLOR 5
	       DISPLAY "/" AT 1337 WITH FOREGROUND-COLOR 0
					BACKGROUND-COLOR 5.

       BZ05.
	     PERFORM AA300.
	   IF WS-USE-ITM = "Y"
033230	       ACCEPT S05-ITM
	   ELSE
	   IF WS-EXT-STK = "Y"
	       ACCEPT S05-EXT-ITEM
	   ELSE
	       ACCEPT S05-ITEM.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
		     GO TO BZ999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO BZ05
		 WHEN F2-KEY
		     MOVE "I"	 TO W10-ETYPE
		 WHEN F3-KEY
		     MOVE "A"	 TO W10-ETYPE
		 WHEN F4-KEY
		     MOVE "D"	 TO W10-ETYPE
		 WHEN F5-KEY
		     MOVE "2"	 TO W10-ETYPE
		 WHEN F6-KEY
		     MOVE "X"	 TO W10-ETYPE
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BZ05
	       END-EVALUATE
	       PERFORM AA50-LOOKUP
	       IF WS-EXT-STK = "Y"
		   DISPLAY S05-EXT-ITEM
	       ELSE
	       IF WS-USE-ITM = "Y"
		   DISPLAY S05-ITM
	       ELSE
		   DISPLAY S05-ITEM
	       END-IF
	       IF W10-EXT-ITEM = SPACES
                   GO TO BZ05.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
	     DISPLAY CLEAR-L25.
036330	   IF W10-EXT-ITEM = SPACES
036340         GO TO BZ999.

       BZ10.
015850       PERFORM CA000.
015860     IF WS-F-ERROR = 22
	       MOVE "Z" 	 TO W10-ETYPE
018620	       PERFORM AA50-LOOKUP
	       IF WS-EXT-STK = "Y"
		   DISPLAY S05-EXT-ITEM
	       ELSE
	       IF WS-USE-ITM = "Y"
		   DISPLAY S05-ITM
	       ELSE
		   DISPLAY S05-ITEM
	       END-IF
018640	       IF W10-EXT-ITEM = SPACES
018720             GO TO BZ999
               ELSE
                   GO TO BZ10.
             DISPLAY STK-DESC AT 1239 WITH FOREGROUND-COLOR 14.
	     PERFORM CHECK-CORRECT.
	   IF WS-OPTION = "N"
               GO TO BZ05.
	     MOVE W10-EXT-ITEM	 TO WS-ITM.

       BZ15.
             PERFORM AA300.
	   IF WS-USE-ITM = "Y"
033230	       ACCEPT S06-ITM
	   ELSE
	   IF WS-EXT-STK = "Y"
	       ACCEPT S06-EXT-ITEM
	   ELSE
	       ACCEPT S06-ITEM.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
		     GO TO BZ999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO BZ15
		 WHEN F2-KEY
		     MOVE "I"	 TO W10-ETYPE
		 WHEN F3-KEY
		     MOVE "A"	 TO W10-ETYPE
		 WHEN F4-KEY
		     MOVE "D"	 TO W10-ETYPE
		 WHEN F5-KEY
		     MOVE "2"	 TO W10-ETYPE
		 WHEN F6-KEY
		     MOVE "X"	 TO W10-ETYPE
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BZ15
	       END-EVALUATE
	       PERFORM AA50-LOOKUP
	       IF WS-EXT-STK = "Y"
		   DISPLAY S06-EXT-ITEM
	       ELSE
	       IF WS-USE-ITM = "Y"
		   DISPLAY S06-ITM
	       ELSE
		   DISPLAY S06-ITEM
	       END-IF
	       IF W10-EXT-ITEM = SPACES
                   GO TO BZ15.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
	     DISPLAY CLEAR-L25.
036330	   IF W10-EXT-ITEM = SPACES
036340         GO TO BZ999.

       BZ20.
015850       PERFORM CA000.
015860     IF WS-F-ERROR = 22
	       MOVE "Z" 	 TO W10-ETYPE
018620	       PERFORM AA50-LOOKUP
	       IF WS-EXT-STK = "Y"
		   DISPLAY S06-EXT-ITEM
	       ELSE
	       IF WS-USE-ITM = "Y"
		   DISPLAY S06-ITM
	       ELSE
		   DISPLAY S06-ITEM
	       END-IF
018640	       IF W10-EXT-ITEM = SPACES
018720             GO TO BZ999
               ELSE
                   GO TO BZ20.
             DISPLAY STK-DESC AT 1339 WITH FOREGROUND-COLOR 14.
	     PERFORM CHECK-CORRECT.
	   IF WS-OPTION = "N"
               GO TO BZ15.
	     MOVE W10-EXT-ITEM	 TO W10-EXT-CODE2.
             MOVE WS-ITM         TO STK-CODE.
             PERFORM START-AT-STOCK-CODE THRU READ-STOCK-EXIT.
	     DISPLAY "Deleting :" AT 1504.

       BZ25.
             PERFORM READ-STOCK-NEXT-LOCK THRU READ-STOCK-EXIT.
	   IF WS-F-ERROR = 22
	       GO TO BZ90.
	   IF STK-CODE > W10-EXT-CODE2
               UNLOCK STOCK
	       GO TO BZ90.
	   IF WS-LEDG NOT = STK-LDG
               UNLOCK STOCK
               GO TO BZ25.
	   IF (STK-QUANT NOT = ZERO) OR
	      (STK-ORD NOT = ZERO) OR
	      (STK-QPIC NOT = ZERO) OR
	      (STK-QJOB NOT = ZERO)
	       UNLOCK STOCK
	       GO TO BZ25.
	     MOVE ZERO		 TO WS-WH-QUANT.
	     PERFORM BZ30 THRU BZ40.
	   IF NOT (WS-WH-QUANT = ZERO)
	       UNLOCK STOCK
	       GO TO BZ25.
	     DISPLAY STK-CODE AT 1515 WITH FOREGROUND-COLOR 15
                     " " STK-DESC WITH FOREGROUND-COLOR 15.
             MOVE STK-CODE       TO PRT-ITEM.
020530       PERFORM READ-SPARTS-LOCK THRU READ-SPARTS-EXIT.
020510     IF WS-F-ERROR = 0
022210	       PERFORM DELETE-SPARTS-REC THRU WRITE-SPARTS-EXIT.
	     PERFORM BZ45 THRU BZ55.
	     MOVE 9		 TO STK-DISC.
	     PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
             PERFORM PRINT-ABORT.
           IF WS-OPTION = "Y"
               DISPLAY "Process aborted by operator - Press any key" 
		       AT 2502
		       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
	       ACCEPT WS-OPTION AT 2546
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO
               GO TO BZ999.
             GO TO BZ25.
      *
      *    ****    C H E C K   F O R   Q U A N T I T Y	 I N
      * 	   W A R E H O U S E   R E C O R D S
      *
       BZ30.
	     MOVE STK-CODE	 TO WST-EXT-CODE.
	     MOVE SPACES	 TO WST-WAR.
	     PERFORM START-AT-WH-ITEM THRU READ-WSTOCK-EXIT.
	   IF WS-F-ERROR = 52
	       GO TO BZ40.

       BZ35.
	     PERFORM READ-WSTOCK-NEXT THRU READ-WSTOCK-EXIT.
	   IF WS-F-ERROR = 52
	       GO TO BZ40.
	   IF NOT (STK-CODE = WST-EXT-CODE)
	       GO TO BZ40.
	     ADD WST-QUANT	 TO WS-WH-QUANT.
	     GO TO BZ35.

       BZ40.
	     EXIT.
      *
      *    ****    D E L E T E	 W A R E H O U S E   R E C O R D S
      *
       BZ45.
	     MOVE STK-CODE	 TO WST-EXT-CODE.
	     MOVE SPACES	 TO WST-WAR.
	     PERFORM START-AT-WH-ITEM THRU READ-WSTOCK-EXIT.
	   IF WS-F-ERROR = 52
	       GO TO BZ55.

       BZ50.
	     PERFORM READ-WSTOCK-NEXT-LOCK THRU READ-WSTOCK-EXIT.
	   IF WS-F-ERROR = 52
	       GO TO BZ55.
	   IF NOT (STK-CODE = WST-EXT-CODE)
	       GO TO BZ55.
	     PERFORM DELETE-WSTOCK-REC THRU WRITE-WSTOCK-EXIT.
	     GO TO BZ50.

       BZ55.
	     EXIT.

       BZ90.
             DISPLAY "Range deleted" AT 1704 WITH FOREGROUND-COLOR 14.
             DISPLAY "Press any key to continue" AT 1904.
             ACCEPT WS-OPTION AT 1930 WITH AUTO.

       BZ999.
             EXIT.

      /
044130 ZA000-INIT	       SECTION 8.
044140 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
044150	     DISPLAY CLR-SCREEN.
      *
      *    ****    D O	 N O T	 D I S P L A Y	 E R R O R
      * 	   M E S S A G E S
      *
      *	     MOVE 44		 TO USER-NUMBER.
      *	     MOVE 1		 TO USER-SETTING.
      *	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    S E T   U P   T H E   F U N C T I O N   K E Y S
      *
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *
      *    ESC, F1 to F10 keys
      *
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    PAGE-UP AND PAGE-DOWN KEYS
      *
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    PRE-DISPLAY ACCEPT FIELDS BEFORE DATA-ENTRY
      *
      *      MOVE 76		 TO USER-NUMBER.
      *      MOVE 3		 TO USER-SETTING.
      *      MOVE 1		 TO USER-KEYS.
      *      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    A C T I V A T E   M O U S E
      *
	     MOVE 64		 TO MOUSE-FUNC.
	     MOVE 1		 TO MOUSE-PARAM.
	     CALL X"AF" USING MOUSE-FUNC
			      MOUSE-PARAM.
	   IF MOUSE-FUNC NOT = 255
	       MOVE "Y"		 TO MOUSE-ATTACHED
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM
	   ELSE
	       GO TO ZA00A.
      *
      *    ****    S E T   M O U S E   K E Y   T O   A C T
      * 	   A S	 F U N C T I O N   K E Y
      *
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *      CALL CBL_INIT_MOUSE USING MOUSE-HANDLE
      *				       MOUSE-BUTTONS
      *				 RETURNING MOUSE-STATUS.
      *    IF MOUSE-STATUS = ZERO
      *        MOVE "Y"		 TO MOUSE-ATTACHED.
      *
      *    ****    P A R A M E T E R   F I L E
      *
       ZA00A.
	     MOVE "PARAM"	 TO AFID-KEY.

       ZA00-READ-APACFIDS.
000030	     READ APACFIDS WITH IGNORE LOCK
	       KEY IS AFID-KEY.
	   IF WS-STATUS = "00"
	       GO TO ZA00-READ-APACFIDS-EXIT.
006260	     STRING "Missing " DELIMITED SIZE
		     AFID-KEY DELIMITED BY " "
		     " file ID - Status " DELIMITED SIZE
		     WS-STATUS DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
006370	     STOP RUN.

       ZA00-READ-APACFIDS-EXIT.
	     EXIT.

       ZA00A-CONTINUE.
	     MOVE AFID-PATH	 TO W02-PARAM.
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "SPARTS"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SPARTS.
	     MOVE "STKMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKMEM.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
	     MOVE "WSTOCK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-WSTOCK.
044230	     MOVE 3		 TO WS-PARKEY.
044240       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.

       ZA01B.
044580       MOVE 1              TO WS-PARKEY.
044590       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
044600       MOVE PAR-DMY        TO W12-TODAY.
044610       MOVE PAR-YMD        TO W12-T-YMD.
044620       MOVE PAR-COMPANY    TO W95-COMP.
044650       MOVE 3              TO WS-NETKEY.
044660       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
044750       MOVE STK-WEEK       TO WS-WEEK.
             MOVE STK-RECORDS    TO WS-RECORDS.
044760       MOVE STK-PASSWORD   TO WS-PASSWORD.
	     MOVE 1		 TO W05-V-RATE.

       ZA05-VAT.
	     MOVE W05-VAT-CODE	 TO DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	     MOVE DPT-R-DATE	 TO WS-VAT-DATE(W05-V-RATE).
	     MOVE DPT-RATE	 TO W05-VAT(W05-V-RATE).
	     ADD 6 W05-V-RATE	 GIVING WS-S1.
	     MOVE DPT-P-RATE	 TO W05-VAT(WS-S1).
	   IF W05-V-RATE < 6
	       ADD 1		 TO W05-V-RATE
	       GO TO ZA05-VAT.
	     GO TO ZA999-EXIT.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
041240 ZA60.
041250       MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1 TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2          TO WS-TCHR (18) WS-TCHR (49)
021940                              WS-TCHR (60) WS-TCHR (70).
021950	     MOVE WS-G3 	 TO WS-MCHR (10) WS-MCHR (50)
021960                              WS-MCHR (60) WS-MCHR (70)
				    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4          TO WS-BCHR (18) WS-BCHR (49)
021980                              WS-BCHR (60) WS-BCHR (70).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
044790 ZA200.
044800       DISPLAY "Files locked - Try later" AT 2312
044810                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
044820       DISPLAY "Press any key" AT 2512 
044830		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
044840	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.

044870 ZA999-EXIT.
044880       EXIT.
      /
044890 ZB000-ERROR	       SECTION 8.
      
041480 OPEN-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
      
041480 READ-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
      
041520 WRITE-ERROR.
041530       DISPLAY CLR-SCREEN.
041540       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK  TO WS-FILE
041630         MOVE WS-NETKEY    TO WS-KEY
041800     ELSE
041810     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870         MOVE WS-PARKEY    TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 21
041860         MOVE W02-SPARTS   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE PRT-ITEM     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 25
041860         MOVE W02-STKMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE SME-KEY	 TO WS-KEYX.
041880     IF WS-STATUS = "10"
041890         MOVE "End of FILE" TO WS-STAT-MESSAGE
041900     ELSE
041910     IF WS-STATUS = "22"
041920         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
041930     ELSE
041940     IF WS-STATUS = "23"
041950         MOVE "Invalid record number" TO WS-STAT-MESSAGE
041960     ELSE
041970     IF WS-STATUS = "24"
041980         MOVE "DISK full" TO WS-STAT-MESSAGE
041990     ELSE
042000     IF WS-STATUS = "30"
042010         MOVE "DISK error" TO WS-STAT-MESSAGE
042020     ELSE
042030     IF WS-STATUS = "94"
042040         MOVE "FILE locked" TO WS-STAT-MESSAGE.
045380       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
045390       DISPLAY "Status " AT 1212 
                      WS-STATUS WITH FOREGROUND-COLOR 11
                     ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
045400     IF WS-STATUS NOT = "94" 
045410         DISPLAY "Key    " AT 1412 
                        WS-KEY WITH FOREGROUND-COLOR 11
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170	     STOP RUN.
