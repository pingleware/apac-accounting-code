      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *   *******   **	      *******	  ****	   ******   ********   *
      *   **	**  **	      **    **	 **  **   **	**  **	       *
      *   **	**  **	      **    **	**    **       **   *******    *
      *   **	**  **	      *******	**    **     ** 	  **   *
      *   **	**  **	      **	**    **   **		  **   *
      *   **	**  **	      **	 **  **   **	    **	  **   *
      *   *******   ********  **	  ****	  ********   ******    *
      * 							       *
      *     ENGLISH                                                    *
      *                                                                *
      *  A D D	 S T O C K   R E C O R D S   T O   W A R E H O U S E   *
      * 							       *
      *     VERSION 8.14.03 - May 2010				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DLP025.
000030 AUTHOR.	       J.W.LEMMON (JWL Software).
000040 DATE-WRITTEN.   APRIL 1996.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1996 - 2010
				     by James William Lemmon.

		   All rights reserved.

000050 SECURITY.   PROGRAM MATERIAL IS THE SOLE PROPERTY OF
000060		   James William Lemmon.(Id No. 4412165050082).

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CONTROL.SL.

       COPY DEPART.SL.

000120 COPY PARAM.SL.

000140 COPY STKMEM.SL.

000140 COPY STOCK.SL.

       COPY WARHSE.SL.

       COPY WSTOCK.SL.

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

       COPY DEPART.FDE.

000280 COPY PARAM.FDE.

000140 COPY STKMEM.FDE.

000300 COPY STOCK.FDE.

000280 COPY WARHSE.FDE.

000280 COPY WSTOCK.FDE.

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001710 77  WS-SUB	    PIC 9(04)	  COMP-5.
001710 77  WS-S1            PIC 9(04)     COMP-5.
001720 77  WS-S2            PIC 9(04)     COMP-5.
001730 77  WS-S3            PIC 9(04)     COMP-5.
001740 77  WS-S4            PIC 9(04)     COMP-5.
001750 77  WS-S5            PIC 9(04)     COMP-5.
001760 77  WS-S6            PIC 9(04)     COMP-5.
001770 77  WS-S7            PIC 9(04)     COMP-5.
001780 77  WS-S8            PIC 9(04)     COMP-5.
001790 77  WS-IXS           PIC 9(04)     COMP-5.
001800 77  WS-IXS1          PIC 9(04)     COMP-5.
001810 77  WS-IXS2          PIC 9(04)     COMP-5.
001820 77  WS-IXS3          PIC 9(04)     COMP-5.
001830 77  WS-IXS4          PIC 9(04)     COMP-5.
001860 77  WS-PARKEY        PIC 9(06)     COMP-5.
001870 77  WS-DEPKEY	    PIC 9(06)	  COMP-5.
001870 77  WS-NETKEY        PIC 9(06)     COMP-5.
001900 77  WS-RECORDS       PIC 9(06)     COMP-5.
       77  WS-ETYPE         PIC X(01).
002020 77  WS-OPTION	    PIC X(01).
002020 77  WS-SKIP          PIC X(01).
002040 77  WS-ADJ           PIC X(01).
002070 77  WS-ERROR         PIC 9(01).
002120 77  WS-AMEND         PIC 9(01)     VALUE 0.
002140 77  WS-CONREC        PIC 9(01) VALUE ZERO.
002150 77  WS-LEDG          PIC 9(03).
002200 77  WS-COMP          PIC 9(01).
002200 77  WS-IND	    PIC 9(01) VALUE ZERO.
002220 77  WS-TYPE          PIC X(01).
002270 77  WS-PASSWORD      PIC X(08).
       77  WS-PRINT         PIC 9(01).
002280 77  WS-PASS          PIC X(08).
       77  WS-START	   PIC	X(14).
       77  WS-END	   PIC	X(14).
       77  TODAY-DDMMYY	   PIC	9(06) COMP-5.
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
	       05  WS-BLK  PIC	X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

002330 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "STP".
	   03  WS-PROG	       PIC  X(03) VALUE "065".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002350 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

000140 COPY STKMEM.ID.

000300 COPY STOCK.ID.

000360 COPY WARHSE.ID.

000360 COPY WSTOCK.ID.

       01  W05-TAX-RATES.
000760     03  W05-VAT-RATES.
000770         05  W05-VAT     PIC S9(03)V99 COMP-3 OCCURS 6.
000780     03  W05-TAX-RATES REDEFINES W05-VAT-RATES.
000790         05  W05-VAT-R1  PIC S9(03)V99 COMP-3.
000800         05  W05-VAT-R2  PIC S9(03)V99 COMP-3.
000810         05  W05-VAT-R3  PIC S9(03)V99 COMP-3.
000820         05  W05-VAT-R4  PIC S9(03)V99 COMP-3.
000830         05  W05-VAT-R5  PIC S9(03)V99 COMP-3.
000840         05  W05-VAT-R6  PIC S9(03)V99 COMP-3.
           03  W05-RATE        PIC S9(03)V99 COMP-3.
           03  W05-RTE REDEFINES W05-RATE
                               PIC S9V9(04)  COMP-3.

       COPY W10.STK.

       COPY W12.WS.

       COPY W15.STK.

       COPY W40.WS.

002920 01  W40-WAREHOUSE.
002930     03  W40-NAME1.
002940	       05  W40-NAME    PIC  X(40).
003020	   03  W40-NAME2 REDEFINES W40-NAME1.
003030	       05  W40-NCHAR OCCURS 40 PIC X(01).
           03  W40-CONTACT     PIC  X(30).
	   03  W40-CODE	       PIC  X(06).
	   03  W40-CODE1       PIC  X(06).
	   03  W40-CODE2       PIC  X(06).
003070	   03  W40-ADD.
003080	       05  W40-ADDRESS1.
003090		   07  W40-ADD1	PIC X(30).
003100		   07  W40-ADD2	PIC X(30).
003110		   07  W40-ADD3	PIC X(30).
003120		   07  W40-ADD4	PIC X(30).
003130	       05  W40-ADDRESS2 REDEFINES W40-ADDRESS1.
003140		   07  W40-ACHAR OCCURS 120 PIC X(01).
003230     03  W40-POST        PIC 9(04).
003250     03  W40-TEL         PIC X(14).
003260     03  W40-FAX         PIC X(14).

       COPY FUNCTION.WS.

003800 COPY W50.WS.

003550 01  W60-ADD.
003580     03  W60-ADDRESS.
003590         05  W60-ACHAR OCCURS 70 PIC X(01).
003580     03  W60-ADDR REDEFINES W60-ADDRESS.
               05  W60-ADD     PIC X(60).
               05  FILLER      PIC X(10).

       01  W75-KEYS.
	   03  W75-S	      PIC  9(02) COMP-5.
	   03  W75-S1	      PIC  9(02) COMP-5.
	   03  W75-S2	      PIC  9(02) COMP-5.
	   03  W75-WHOUSE OCCURS 18.
	       05  W75-WHCDE  PIC  X(06).
	   03  W75-ITEMS OCCURS 18.
	       05  W75-CODE   PIC  X(14).
	       05  W75-ITM    PIC  X(03).
	       05  W75-SELL   PIC  9(07)V99 COMP-3.
	       05  W75-VSELL  PIC  9(07)V99 COMP-3.
	       05  W75-CASH   PIC  9(07)V99 COMP-3.
	       05  W75-VCASH  PIC  9(07)V99 COMP-3.
	       05  W75-WSALE  PIC  9(07)V99 COMP-3.
	       05  W75-VWSALE PIC  9(07)V99 COMP-3.
	   03  W75-SLEDG      PIC  9(03)    COMP-3 OCCURS 10.

003930 01  W80-EDIT.
003950	   03  W80-Q1.
003960	       05  W80-S7V4   PIC Z(06)9.9999-.

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

       01  S01.
	   03  LINE  2 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "ADD STOCK RECORDS TO W/H".
	   03  LINE  4 COLUMN  5 VALUE "Warehouse   ".
	   03	       COLUMN 18 BACKGROUND-COLOR 5
				 PIC  X(06) FROM WS-BLK.
	   03  LINE  6 COLUMN  5 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Stock Details".
	   03  LINE  7 COLUMN  5 VALUE "Sales Ledger".
	   03	       COLUMN 18 BACKGROUND-COLOR 5
				 PIC  X(03) FROM WS-BLK.
	   03  LINE  8 COLUMN  5 VALUE "Start Item  ".
	   03	       COLUMN 18 BACKGROUND-COLOR 5
				 PIC  X(14) FROM WS-BLK.
	   03  LINE  9 COLUMN  5 VALUE "End Item    ".
	   03	       COLUMN 18 BACKGROUND-COLOR 5
				 PIC  X(14) FROM WS-BLK.

       01  S02.
007920	   03  LINE  4 COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(06) USING W40-CODE AUTO.

       01  S03.
007920	   03  LINE  7 COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ9 USING W10-LEDG AUTO.

005530 01  S05.
005540	   03  COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5
005550			 PIC X(14) USING W10-ITEM AUTO.

005530 01  S05-3.
005540	   03  COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5
005550			 PIC Z(03) USING W10-I3 AUTO.

005530 01  S05-4.
005540	   03  COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5
005550			 PIC Z(04) USING W10-I4 AUTO.

005530 01  S05-5.
005540	   03  COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5
005550			 PIC Z(05) USING W10-I5 AUTO.

005530 01  S05-6.
005540	   03  COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5
005550			 PIC Z(06) USING W10-I6 AUTO.

005530 01  S05-7.
005540	   03  COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5
005550			 PIC Z(07) USING W10-I7 AUTO.

005530 01  S05-8.
005540	   03  COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5
005550			 PIC Z(08) USING W10-I8 AUTO.

005530 01  S05-9.
005540	   03  COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5
005550			 PIC Z(09) USING W10-I9 AUTO.

005530 01  S05-10.
005540	   03  COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5
005550			 PIC Z(10) USING W10-I10 AUTO.

005530 01  S05-11.
005540	   03  COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5
005550			 PIC Z(11) USING W10-I11 AUTO.

005530 01  S05-12.
005540	   03  COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5
005550			 PIC Z(12) USING W10-I12 AUTO.

005530 01  S05-13.
005540	   03  COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5
005550			 PIC Z(13) USING W10-I13 AUTO.

005530 01  S05-14.
005540	   03  COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5
005550			 PIC Z(14) USING W10-I14 AUTO.

       COPY DEPART.CRT.

       COPY STKLUP.CRT.

       COPY WARLUP.CRT.

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

004870 COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
008230 AA000		       SECTION.
008240 AA00.
003240	   IF LS0-STLEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
008250	     PERFORM ZA000-INIT.
	     PERFORM CB000.
013140 AA49.
013150       EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
       AA50.
	     PERFORM SAVE-SCREEN.
011080	     MOVE 4		 TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
             PERFORM STOCK-LOOKUP.
	     PERFORM RESTORE-SCREEN.
	     GO TO AA99.

       AA50W.
011050       PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090       PERFORM SCREEN-CONTENTS.
	     PERFORM WARHSE-LOOKUP.
011320       PERFORM RESTORE-SCREEN.

       AA99.
             EXIT.
      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S 
      *
       AA300              SECTION.
       AA301.
011010	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011020		     " Item code/ "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011030		     "F3" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011040		     " Alt code/ "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011050		     "F4" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011060		     " Description/ "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011070		     "F5" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011080		     " X/Ref Lookup"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
       AA305.
             EXIT.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

      /
      *    ****    READ FILES ROUTINES
      *
008680 AC000              SECTION.

       COPY CONTROL.RD.

       COPY DEPART.RD.

       COPY PARAM.RD.

000140 COPY STKMEM.RD.

       COPY STOCK.RD.

       COPY WARHSE.RD.

       COPY WSTOCK.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010170 AD000             SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

       COPY STOCK.WR.

       COPY WARHSE.WR.

       COPY WSTOCK.WR.

       COPY DEPART.LUP.

       COPY STOCK.LUP.

       COPY WARHSE.LUP.

       COPY APAC.HLP.

       COPY CA000.STK.

       COPY CA100.STK.

       COPY CA200.STK.

       COPY CA500.STK.

      *
      *    ****   G E T   W A R E H O U S E   R E C O R D
      *
015170 CA600	     SECTION.
015180 CA601.
013310	     MOVE 6		 TO W50-LENGTH.

       CA605.
013320	     MOVE W40-CODE	 TO W50-DATA.
           IF W50-1 = SPACE
	       MOVE W50-5	 TO W40-CODE
	       GO TO CA605.
	     CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
013380	     MOVE W50-DENT	 TO W40-CODE.

       CA606.
013380	     MOVE W50-DENT	 TO WAR-CODE.

       CA610.
	     PERFORM READ-WARHSE THRU READ-WARHSE-EXIT.

       CA699.
             EXIT.
      /
      *    ****   C R E A T E	W / H	S T O C K   R E C O R D S
      *   
018130 CB000		 SECTION 50.
018140 CB00.
018150	     DISPLAY CLR-SCREEN.
	     DISPLAY S01.
	     MOVE SPACES	 TO W40-CODE.
	     MOVE ZERO		 TO W10-LEDG.

       CB05.
011010	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011020		     "=Warehouse Lookup,"
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ape to Exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     ACCEPT S02.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO CB999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO CB05
		 WHEN F2-KEY
016560		     PERFORM AA50W
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CB05
	       END-EVALUATE
	       DISPLAY S02
015830	       IF W40-CODE = SPACE
015840		   GO TO CB05.
019250	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
019250	     DISPLAY CLEAR-L25.
019490	   IF W40-CODE = SPACE
	       GO TO CB999.

       CB10.
	     PERFORM CA600.
015860	   IF WS-F-ERROR = 51
017520	       MOVE "Z" 	 TO WS-ETYPE
018620	       PERFORM AA50W
018630	       DISPLAY S02
018640	       IF W40-CODE = SPACES
018720		   GO TO CB05
               ELSE
		   GO TO CB10.
019840	     MOVE WAR-NAME	 TO W40-NAME.
	     DISPLAY W40-NAME AT 0425
		     WITH HIGHLIGHT BACKGROUND-COLOR 5.

       CB15.
011010	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F6" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011020		     " Sales Ledger Lookup"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     ACCEPT S03.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO CB05
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO CB15
		 WHEN F6-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM DEPART-VIEW
011320		     PERFORM RESTORE-SCREEN
		     GO TO CB15
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CB15
	       END-EVALUATE.
016040	   IF W10-LEDG < 1 OR > 296
016050	       MOVE "Invalid ledger code"
				 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
016060	       GO TO CB15.
	     MOVE W10-LEDG	 TO WS-DEPKEY.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF DPT-DESC = SPACES
	       MOVE "NO SUCH   D E P A R T M E N T"
				 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
016060	       GO TO CB15.
	     DISPLAY DPT-DESC AT 0722
		     WITH HIGHLIGHT BACKGROUND-COLOR 5.

       CB20.
019460	     MOVE SPACE 	 TO W10-ITEM W10-ITM.
	   IF W10-STCK = "N"
	       IF W10-SLNGTH = 3
		   MOVE ZERO	 TO W10-I3
	       ELSE
	       IF W10-SLNGTH = 4
		   MOVE ZERO	 TO W10-I4
	       ELSE
	       IF W10-SLNGTH = 5
		   MOVE ZERO	 TO W10-I5
	       ELSE
	       IF W10-SLNGTH = 6
		   MOVE ZERO	 TO W10-I6
	       ELSE
	       IF W10-SLNGTH = 7
		   MOVE ZERO	 TO W10-I7
	       ELSE
	       IF W10-SLNGTH = 8
		   MOVE ZERO	 TO W10-I8
	       ELSE
	       IF W10-SLNGTH = 9
		   MOVE ZERO	 TO W10-I9
	       ELSE
	       IF W10-SLNGTH = 10
		   MOVE ZERO	 TO W10-I10
	       ELSE
	       IF W10-SLNGTH = 11
		   MOVE ZERO	 TO W10-I11
	       ELSE
	       IF W10-SLNGTH = 12
		   MOVE ZERO	 TO W10-I12
	       ELSE
	       IF W10-SLNGTH = 13
		   MOVE ZERO	 TO W10-I13
	       ELSE
		   MOVE ZERO	 TO W10-I14.
	     MOVE W10-ITEM	 TO WS-START WS-END.

       CB25.
	     PERFORM AA300.
	   IF W10-STCK = "A"
021070	       ACCEPT S05 AT LINE 8
	   ELSE
	   IF W10-SLNGTH = 3
	       ACCEPT S05-3 AT LINE 8
	   ELSE
	   IF W10-SLNGTH = 4
	       ACCEPT S05-4 AT LINE 8
	   ELSE
	   IF W10-SLNGTH = 5
	       ACCEPT S05-5 AT LINE 8
	   ELSE
	   IF W10-SLNGTH = 6
	       ACCEPT S05-6 AT LINE 8
	   ELSE
	   IF W10-SLNGTH = 7
	       ACCEPT S05-7 AT LINE 8
	   ELSE
	   IF W10-SLNGTH = 8
	       ACCEPT S05-8 AT LINE 8
	   ELSE
	   IF W10-SLNGTH = 9
	       ACCEPT S05-9 AT LINE 8
	   ELSE
	   IF W10-SLNGTH = 10
	       ACCEPT S05-10 AT LINE 8
	   ELSE
	   IF W10-SLNGTH = 11
	       ACCEPT S05-11 AT LINE 8
	   ELSE
	   IF W10-SLNGTH = 12
	       ACCEPT S05-12 AT LINE 8
	   ELSE
	   IF W10-SLNGTH = 13
	       ACCEPT S05-13 AT LINE 8
	   ELSE
	   IF W10-SLNGTH = 14
	       ACCEPT S05-14 AT LINE 8.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                     MOVE SPACES TO W10-ITEM
		     GO TO CB15
                 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO CB25
		 WHEN F2-KEY
		     MOVE "I"	 TO WS-ETYPE
		 WHEN F3-KEY
		     MOVE "A"	 TO WS-ETYPE
		 WHEN F4-KEY
		     MOVE "D"	 TO WS-ETYPE
		 WHEN F5-KEY
		     MOVE "X"	 TO WS-ETYPE
                 WHEN OTHER
                     CALL X"E5"
		     GO TO CB25
               END-EVALUATE
	       PERFORM AA50-LOOKUP
	       DISPLAY S05 AT LINE 8
               IF W10-ITEM = SPACES
		   GO TO CB25.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
019250	     DISPLAY CLEAR-L25.
019490     IF W10-ITEM = SPACE
019500	       GO TO CB15.

       CB30.
019510	     PERFORM CA000.
019520     IF WS-F-ERROR = 22
	       MOVE "Z"		 TO WS-ETYPE
018620	       PERFORM AA50-LOOKUP
018630	       DISPLAY S05 AT LINE 8
018640         IF W10-ITEM = SPACES
018720		   GO TO CB25
               ELSE
		   GO TO CB30.
	     DISPLAY STK-DESC AT 0833
		     WITH HIGHLIGHT BACKGROUND-COLOR 5.
	     MOVE STK-CODE	 TO WS-START.

       CB35.
	     PERFORM AA300.
	   IF W10-STCK = "A"
021070	       ACCEPT S05 AT LINE 9
	   ELSE
	   IF W10-SLNGTH = 3
	       ACCEPT S05-3 AT LINE 9
	   ELSE
	   IF W10-SLNGTH = 4
	       ACCEPT S05-4 AT LINE 9
	   ELSE
	   IF W10-SLNGTH = 5
	       ACCEPT S05-5 AT LINE 9
	   ELSE
	   IF W10-SLNGTH = 6
	       ACCEPT S05-6 AT LINE 9
	   ELSE
	   IF W10-SLNGTH = 7
	       ACCEPT S05-7 AT LINE 9
	   ELSE
	   IF W10-SLNGTH = 8
	       ACCEPT S05-8 AT LINE 9
	   ELSE
	   IF W10-SLNGTH = 9
	       ACCEPT S05-9 AT LINE 9
	   ELSE
	   IF W10-SLNGTH = 10
	       ACCEPT S05-10 AT LINE 9
	   ELSE
	   IF W10-SLNGTH = 11
	       ACCEPT S05-11 AT LINE 9
	   ELSE
	   IF W10-SLNGTH = 12
	       ACCEPT S05-12 AT LINE 9
	   ELSE
	   IF W10-SLNGTH = 13
	       ACCEPT S05-13 AT LINE 9
	   ELSE
	   IF W10-SLNGTH = 14
	       ACCEPT S05-14 AT LINE 9.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     GO TO CB25
                 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO CB35
		 WHEN F2-KEY
		     MOVE "I"	 TO WS-ETYPE
		 WHEN F3-KEY
		     MOVE "A"	 TO WS-ETYPE
		 WHEN F4-KEY
		     MOVE "D"	 TO WS-ETYPE
		 WHEN F5-KEY
		     MOVE "X"	 TO WS-ETYPE
                 WHEN OTHER
                     CALL X"E5"
		     GO TO CB35
               END-EVALUATE
	       PERFORM AA50-LOOKUP
	       DISPLAY S05 AT LINE 9
               IF W10-ITEM = SPACES
		   GO TO CB35.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
019250	     DISPLAY CLEAR-L25.
019490     IF W10-ITEM = SPACE
019500	       GO TO CB25.

       CB40.
019510	     PERFORM CA000.
019520     IF WS-F-ERROR = 22
	       MOVE "Z"		 TO WS-ETYPE
018620	       PERFORM AA50-LOOKUP
018630	       DISPLAY S05 AT LINE 9
018640         IF W10-ITEM = SPACES
018720		   GO TO CB35
               ELSE
		   GO TO CB40.
	     DISPLAY STK-DESC AT 0933
		     WITH HIGHLIGHT BACKGROUND-COLOR 5.
	     MOVE STK-CODE	 TO WS-END.
	     MOVE 16		 TO SLIN.
	     PERFORM CHECK-SETUP THRU CHECK-EXIT.
015980	   IF WS-OPTION = "N"
	       GO TO CB05.
	   IF WS-END < WS-START
	       MOVE "End Item < Start Item"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO CB25.
	     MOVE W10-LEDG	 TO STK-LEDG.
	     MOVE WS-START	 TO STK-CODE.
	     PERFORM START-AT-STOCK-LEDG THRU READ-STOCK-EXIT.
	   IF WS-F-ERROR = 22
	       GO TO CB50.
	     DISPLAY "Adding :" AT 1205.
       CB45.
	     PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
	   IF WS-F-ERROR = 22
	       GO TO CB55.
	   IF (W10-LEDG NOT = STK-LEDG) OR
	      (STK-CODE > WS-END)
	       GO TO CB55.
	   IF STK-IND > ZERO
	       GO TO CB45.
	     DISPLAY STK-CODE AT 1214 WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     DISPLAY STK-DESC AT 1229 WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     MOVE W40-CODE	 TO WST-WAR.
	     MOVE STK-CODE	 TO WST-CODE.
	     PERFORM READ-WSTOCK THRU READ-WSTOCK-EXIT.
	   IF WS-F-ERROR = 0
	       GO TO CB45.
	     INITIALIZE WST-RECORD1.
	     MOVE W40-CODE	 TO WST-WAR.
	     MOVE STK-CODE	 TO WST-CODE.
	     MOVE SPACES	 TO WST-ITM WST-BIN.
	     MOVE ZERO		 TO WST-MTD WST-MTDV WST-QUANT
				    WST-YTD WST-YTDV.
	     PERFORM WRITE-WSTOCK THRU WRITE-WSTOCK-EXIT.
	     GO TO CB45.

       CB50.
	     MOVE "No details in range"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.

       CB55.
032920	     MOVE "'C'ontinue, 'E'xit  [ ]"
				 TO WS-ERR-MES.
	     MOVE SPACES	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
015980	   IF NOT (WS-OPTION = "C" OR "E")
	       GO TO CB55.
	   IF WS-OPTION = "C"
	       GO TO CB00.

       CB999.
	     EXIT.
      /
      *    ****    I N I T I A L I Z E   P R O G R A M
      *   
041980 ZA000-INIT	       SECTION 80.
041990 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
042000	     DISPLAY CLR-SCREEN.
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



042080	     MOVE 3		 TO WS-PARKEY.
042090       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042110	     MOVE WS-PTH15	 TO W02-PTH2  W02-PTH7
				    W02-PTH22 W02-PTH25
				    W02-PTH51 W02-PTH52.
	   IF NOT (PAR-STOCK = "A" OR "N")
	       MOVE "A" 	 TO PAR-STOCK.
	     MOVE PAR-STOCK	 TO W10-STCK.
	   IF PAR-STOCK = "N"
	       MOVE PAR-SLNGTH	 TO W10-SLNGTH
	   ELSE
	       MOVE 14		 TO W10-SLNGTH.
	   IF W10-SLNGTH < 3
	       MOVE 3		 TO W10-SLNGTH
	   ELSE
	   IF W10-SLNGTH > 14
	       MOVE 14		 TO W10-SLNGTH.
042720	     MOVE 1		 TO WS-PARKEY.
042730       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042740       MOVE PAR-COMPANY    TO W95-COMP.
042750       MOVE PAR-DMY        TO W12-TODAY.
042760       MOVE PAR-YMD        TO W12-T-YMD.
042520	     MOVE 5		 TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
050550       MOVE PAR-VAT-RATES  TO W05-VAT-RATES.
044780	     GO TO ZA999-EXIT.
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
041240 ZA60.
041250       MOVE WS-G1          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8          TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1             TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2          TO WS-TCHR (8)  WS-TCHR (49)
021940                              WS-TCHR (61) WS-TCHR (72).
021950       MOVE WS-G3          TO WS-MCHR (8)  WS-MCHR (49)
021960                              WS-MCHR (61) WS-MCHR (72)
021960				    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4          TO WS-BCHR (8)  WS-BCHR (49)
021980                              WS-BCHR (61) WS-BCHR (72).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
042880 ZA999-EXIT.
042890       EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *   
042900 ZB000-ERROR	     SECTION 81.
      
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
041510       GO TO DISPLAY-FILE-NAME.
       I-O-ERROR-EXIT.
             EXIT.

041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK TO WS-FILE
041630         MOVE WS-NETKEY TO WS-KEY
041800	   ELSE
041810     IF WS-F-ERROR = 7
041820         MOVE W02-DEPART   TO WS-FILE
041830         MOVE WS-DEPKEY    TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870         MOVE WS-PARKEY TO WS-KEY
041840     ELSE
041850	   IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 25
041860         MOVE W02-STKMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE SME-KEY	 TO WS-KEYX
041840	   ELSE
041770	   IF WS-F-ERROR = 51
041780	       MOVE W02-WARHSE TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
	       MOVE WAR-CODE	 TO WS-KEYX
041840	   ELSE
041770	   IF WS-F-ERROR = 52
041780	       MOVE W02-WSTOCK TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
	       MOVE WST-KEY	 TO WS-KEYX.
041880	   IF WS-STATUS = "10"
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
042170       STOP RUN.
