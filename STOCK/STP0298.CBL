      $set LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *  ******  ******** *******    ****    ******   ******   ******  *
      * **    **    **	  **	**  **	**  **	  ** **    ** **    ** *
      * **	    **	  **	** **	 **	 **  **    ** **    ** *
      *  ******     **	  *******  **	 **    **     *******  ******  *
      *       **    **	  **	   **	 **  ** 	   ** **    ** *
      * **    **    **	  **	    **	**  **	     **    ** **    ** *
      *  ******     **	  **	     ****   ********  ******   ******  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     G E N E R A T E   B A R - C O D E S	  X R E F	       *
      *                                                                *
      *     A M E N D	W H O L E S A L E   =	C O S T 	       *
      *                                                                *
      *     VERSION 8.06 - August 2002				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STP0298.
000030 AUTHOR.         J.W.LEMMON (CSS).
000040 DATE-WRITTEN.   JULY 1993.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1993 - 2002
				     by James William Lemmon.

		   All rights reserved.

000050 SECURITY.   PROGRAM MATERIAL IS THE SOLE PROPERTY OF
000060		   James William Lemmon.

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090                 CONSOLE IS CRT
                       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CONTROL.SL.

000270 COPY PARAM.SL.

000140 COPY STOCK.SL.

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000560 COPY PARAM.FDE.

000300 COPY STOCK.FDE.

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001700 77  WS-DISC	   PIC 999V99	 COMP-3.
001710 77  WS-SUB	   PIC 9(04)	 COMP-5.
001710 77  WS-S1	   PIC 9(04)	 COMP-5.
001720 77  WS-S2	   PIC 9(04)	 COMP-5.
001730 77  WS-S3	   PIC 9(04)	 COMP-5.
001740 77  WS-S4	   PIC 9(04)	 COMP-5.
001750 77  WS-S5	   PIC 9(04)	 COMP-5.
001760 77  WS-S6	   PIC 9(04)	 COMP-5.
001770 77  WS-S7	   PIC 9(04)	 COMP-5.
001780 77  WS-S8	   PIC 9(04)	 COMP-5.
001790 77  WS-IXS	   PIC 9(04)	 COMP-5.
001800 77  WS-IXS1	   PIC 9(04)	 COMP-5.
001810 77  WS-IXS2	   PIC 9(04)	 COMP-5.
001820 77  WS-IXS3	   PIC 9(04)	 COMP-5.
001830 77  WS-IXS4	   PIC 9(04)	 COMP-5.
       77  WS-NETKEY	   PIC 9(04)	 COMP-5 VALUE 3.
002320 77  WS-PARKEY       PIC  9(04)    COMP-5.
002070 77  WS-WHICH-CODE   PIC	X(01).
	   88  USE-XREF    VALUE "X".
	   88  USE-ITEM    VALUE "I".
       77  WS-BC-STYLE	   PIC	X(01).
001940 77  WS-MONTHS	   PIC S9(01)V99 COMP-3.
001960 77  WS-KEY1	   PIC 9(04)	 COMP-5.
001980 77  WS-ITM	   PIC X(14).
       77  WS-ETYPE	   PIC X(01).
002020 77  WS-OPTION	   PIC X(01).
002020 77  WS-SKIP	   PIC X(01).
002040 77  WS-ADJ	   PIC X(01).
002070 77  WS-ERROR	   PIC 9(01).
002080 77  WS-IND	   PIC 9(01)	 VALUE 0.
002090 77  WS-INDB	   PIC 9(01)	 VALUE 0.
002100 77  WS-INDP	   PIC 9(01)	 VALUE 0.
002110 77  WS-INDS	   PIC 9(01)	 VALUE 0.
002120 77  WS-AMEND	   PIC 9(01)	 VALUE 0.
      /
       01  BAR-CODE-WORK.
	   03  BAR-CODE-TYPE	 PIC  X(01).
	   03  BAR-CODE 	 PIC  9(14) COMP VALUE ZERO.
	   03  BAR-CODE-CH REDEFINES BAR-CODE
				 PIC  9(01) COMP OCCURS 14.
	   03  BAR-CODE-ITF14 REDEFINES BAR-CODE.
	       05  BAR-CODE-13	 PIC  9(13) COMP.
	       05  BAR-CODE-CHK-DIG  PIC 9(01) COMP.
	   03  BAR-CODE-EAN13 REDEFINES BAR-CODE.
	       05  FILLER	 PIC  9(01) COMP.
	       05  BAR-CODE-JAN13.
		 07  BAR-CODE-12 PIC  9(12) COMP.
		 07  FILLER	 PIC  9(01) COMP.
	   03  BAR-CODE-EAN8 REDEFINES BAR-CODE.
	       05  FILLER	 PIC  9(06) COMP.
	       05  BAR-CODE-JAN8.
		 07  BAR-CODE-7	 PIC  9(07) COMP.
		 07  FILLER	 PIC  9(01) COMP.
	   03  BAR-CODE-RESULT-1 PIC  9(04) COMP.
	   03  BAR-CODE-RESULT-2 PIC  9(04) COMP.
	   03  BAR-CODE-RESULT-3 PIC  9(04) COMP.
	   03  BAR-CODE-CALC REDEFINES BAR-CODE-RESULT-3.
	       05  BAR-CODE-TEN  PIC  9(03) COMP.
	       05  BAR-CODE-UNIT PIC  9(01) COMP.
	   03  BAR-CODE-COUNT	 PIC  9(06) COMP.
	   03  BAR-CODE-OUT	 PIC  X(01).
	   03  BAR-CODE-COL	 PIC  9(04) COMP-5.
	   03  BAR-CODE-LIN	 PIC  9(04) COMP-5.

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
               05  FILLER      PIC  X(39).
002450     03  WS-MID-LNE.
002460         05  WS-MCHR PIC X(01) OCCURS 80.
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
002450	   03  WS-MID-LNE2.
002460         05  WS-MCH  PIC X(01) OCCURS 80.
	   03  WS-MID78 REDEFINES WS-MID-LNE2.
	       05  FILLER      PIC  X(01).
	       05  WS-BLNK78   PIC  X(78).
	       05  FILLER      PIC  X(01).
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

002330 COPY CHAIN.WS.

002350 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY PARAM.ID.

       COPY STOCK.ID.

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
001660	   03  W25-TIME        PIC  9(08).
001670     03  W25-TRED REDEFINES  W25-TIME.
001680	       05  FILLER      PIC  9(02).
001690	       05  W25-FACT    PIC  9(06).
	       05  W25-DATE REDEFINES W25-FACT.
		   07  W25-YY  PIC  9(02).
		   07  W25-MM  PIC  9(02).
		   07  W25-DD  PIC  9(02).
	   03  W25-CUR-CC      PIC  9(02) COMP-5.
	   03  W25-PRV-CC      PIC  9(02) COMP-5.
	   03  W25-NXT-CC      PIC  9(02) COMP-5.

       COPY W40.WS.

       COPY FUNCTION.WS.

003800 COPY W50.WS.

003930 01  W80-EDIT.
003940     03  W80-CNO        PIC 9(04).
003950     03  W80-V1.
003960         05  W80-S7V2   PIC Z(06)9.99-.
003970     03  W80-V2.
003980         05  W80-S5V2   PIC Z(04)9.99-.
003990     03  W80-DTE.
004000         05  W80-DATE   PIC Z9/99/99.
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
      /
004130 SCREEN SECTION.
      *
      *    ****  THIS SCREEN - CLEARS THE DISPLAY AND SETS THE DEFAULT
      *          COLOUR TO 3 (LIGHT BLUE).
      *
001950 01  CLR-SCREEN.
003170     03  BLANK SCREEN FOREGROUND-COLOR 3
                            BACKGROUND-COLOR 1.
	   03  CLR-L1-2.
003860	   05  LINE  1 COLUMN  1 BACKGROUND-COLOR 0
				 PIC X(80) USING WS-TOP-LNE2.
003870	   05  LINE  2 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "�".
004160	   05	       COLUMN  2 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 4
004170                           VALUE "APAC Accounting".
	   05	       COLUMN 17 PIC  X(50) FROM WS-BACKGROUND.
004180	   05	       COLUMN 66 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 4
004190				 VALUE "Version - 6.75".
003920	   05	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "�".
003930	   03  LINE  3 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "�".
	   03	       COLUMN  2 PIC  X(64) FROM WS-BACKGROUND.
	   03	       COLUMN 66 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE "Date: ".
	   03	       COLUMN 72 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 PIC Z9/99/99 USING TODAY-DDMMYY.
003940	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "�".
003950	   03  LINE  4 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "�".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
003960	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "�".
003970	   03  LINE  5 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "�".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
003980	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "�".
003990	   03  LINE  6 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "�".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004000	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "�".
004010	   03  LINE  7 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "�".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004020	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "�".
004030	   03  LINE  8 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "�".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004040	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "�".
004050	   03  LINE  9 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "�".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004060	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "�".
004070	   03  LINE 10 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "�".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004080	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "�".
004090	   03  LINE 11 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "�".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004100	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "�".
004110	   03  LINE 12 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "�".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004120	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "�".
004130	   03  LINE 13 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "�".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004140	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "�".
004150	   03  LINE 14 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "�".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004160	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "�".
004170	   03  LINE 15 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "�".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004180	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "�".
004190	   03  LINE 16 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "�".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004200	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "�".
004210	   03  LINE 17 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "�".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004220	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "�".
004230	   03  LINE 18 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "�".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004240	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "�".
004250	   03  LINE 19 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "�".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004260	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "�".
004270	   03  LINE 20 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "�".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004280	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "�".
004290	   03  LINE 21 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "�".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004300	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "�".
004310	   03  LINE 22 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "�".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004320	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "�".
004330	   03  LINE 23 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "�".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004340	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "�".
006550	   03  LINE 24 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "��������������������������������
      - 	     "������������������������������������������������".
	   03  CLEAR-L25.
	   05  LINE 25 BLANK LINE BACKGROUND-COLOR 3
				  FOREGROUND-COLOR 1.

       COPY ERROR.CRT.

       COPY OPT.CRT.

       COPY S99.CRT.

      /
007730 PROCEDURE DIVISION.
007740 AA000-MAIN              SECTION.
007750 AA000-INIT.
007760       PERFORM ZA000-INIT.
	   IF WS-ERROR = 9
	       GO TO AA50.
007780	     PERFORM BX000.
	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.

       AA49.
	     CLOSE NETWORK
		   STOCK.

       AA50.
	     STOP RUN.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

       COPY DATE.CHK.

      /
      *    ****    READ FILES ROUTINES
      *
008180 AC000-READ              SECTION.

       COPY CONTROL.RD.

011270 COPY PARAM.RD.

       COPY STOCK.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
009850 AD000-WRITE             SECTION.

       COPY CONTROL.WR.

       COPY STOCK.WR.

       COPY STOCK.1ST.

      /
      *    ****    G E N E R A T E   B A R   C O D E S	 I N   T H E
      * 	   X R E F   F I E L D
      *
035140 BX000	       SECTION.
035150 BX00.
035160       DISPLAY CLR-SCREEN.
	     DISPLAY "GENERATE BAR CODES (XREF)" AT 0228
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "This routine will generate barcodes for items in t
      -		     "he stock file" AT 0509
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT.
	     DISPLAY "excluding labour, text and heading records.
      -		     "             " AT 0609
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT.

035180 BX02.
035170	     MOVE "Run generate routine (Y/N)  [ ]"
				 TO WS-ERR-MES.
	     MOVE " "		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
035250	   IF NOT (WS-OPTION = "N" OR "Y")
035260         GO TO BX02.
035270     IF WS-OPTION = "N"
035280         GO TO BX999.
	     MOVE 05		 TO CLIN.
             MOVE 01             TO CCOL.
	     PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 10.
	     MOVE 11		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 34		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY "�����������������������������������" AT 1026
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "�" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "� Start with Barcode               " AT 1126
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "�" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "�" AT 1226
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "�����������������������������������"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     PERFORM SCREEN-SHADOW.
	     MOVE SPACES	 TO W10-ITEM.

035300 BX05.
	     ACCEPT W10-ITEM AT 1147
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF W10-ITEM = SPACES
	       GO TO BX999.
	     MOVE 14		 TO WS-S8.
	   IF BAR-CODE-TYPE = "1"
	       MOVE W10-ITEM	 TO BAR-CODE-12
	       MOVE BAR-CODE-12	 TO W10-ITEM
	   ELSE
	   IF BAR-CODE-TYPE = "2"
	       MOVE W10-ITEM	 TO BAR-CODE-13
	       MOVE BAR-CODE-13	 TO W10-ITEM
	   ELSE
	   IF BAR-CODE-TYPE = "3"
	       MOVE W10-ITEM	 TO BAR-CODE-7
	       MOVE BAR-CODE-7	 TO W10-ITEM.

       BX10.
	   IF W10-IC (WS-S8) = SPACE
	       SUBTRACT 1	 FROM WS-S8
	       GO TO BX10.
	     MOVE WS-S8 	 TO W10-SLNGTH.
	     MOVE 10		 TO CLIN.
             MOVE 01             TO CCOL.
	     PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 14.

035300 BX15.
035360       MOVE "    !"        TO STK-CODE.
035370       PERFORM START-AT-STOCK-CODE THRU READ-STOCK-EXIT.
035460     IF WS-F-ERROR = 22
035470         GO TO BX999.
035380       DISPLAY "Stock Record :" AT 1212. 

035390 BX20.
035450       PERFORM READ-STOCK-NEXT-LOCK THRU READ-STOCK-EXIT.
035460     IF WS-F-ERROR = 22
035470         GO TO BX999.
035440       DISPLAY STK-CODE AT 1228 WITH FOREGROUND-COLOR 11
		     " " STK-DESC WITH FOREGROUND-COLOR 14.
	   IF NOT (STK-IND = 0)
	       GO TO BX25.
	   IF BAR-CODE-TYPE = "0"
	       GO TO BX22.
	   IF BAR-CODE-TYPE = "1"
	       MOVE W10-ITEM	 TO BAR-CODE-12
	   ELSE
	   IF BAR-CODE-TYPE = "2"
	       MOVE W10-ITEM	 TO BAR-CODE-13
	   ELSE
	   IF BAR-CODE-TYPE = "3"
	       MOVE W10-ITEM	 TO BAR-CODE-7.
	     ADD BAR-CODE-CH (1),
		 BAR-CODE-CH (3),
		 BAR-CODE-CH (5),
		 BAR-CODE-CH (7),
		 BAR-CODE-CH (9),
		 BAR-CODE-CH (11),
		 BAR-CODE-CH (13)
				 GIVING BAR-CODE-RESULT-1.
	     ADD BAR-CODE-CH (2),
		 BAR-CODE-CH (4),
		 BAR-CODE-CH (6),
		 BAR-CODE-CH (8),
		 BAR-CODE-CH (10),
		 BAR-CODE-CH (12)
				 GIVING BAR-CODE-RESULT-2.
	     MULTIPLY 3 	 BY BAR-CODE-RESULT-1.
	     ADD BAR-CODE-RESULT-1
		 BAR-CODE-RESULT-2
				 GIVING BAR-CODE-RESULT-3.
	     SUBTRACT BAR-CODE-UNIT FROM 10
				 GIVING BAR-CODE-CHK-DIG.
	   IF BAR-CODE-TYPE = "1"
	       MOVE BAR-CODE-JAN13
				 TO W10-ITEM
	   ELSE
	   IF BAR-CODE-TYPE = "2"
	       MOVE BAR-CODE-ITF14
				 TO W10-ITEM
	   ELSE
	   IF BAR-CODE-TYPE = "3"
	       MOVE BAR-CODE-JAN8
				 TO W10-ITEM.

       BX22.
	     MOVE W10-ITEM	 TO STK-XREF.
	     PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
	   IF W10-SLNGTH = 6
	       ADD 1		 TO W10-I6
	   ELSE
	   IF W10-SLNGTH = 7
	       ADD 1		 TO W10-I7
	   ELSE
	   IF W10-SLNGTH = 8
	       ADD 1		 TO W10-I8
	   ELSE
	   IF W10-SLNGTH = 9
	       ADD 1		 TO W10-I9
	   ELSE
	   IF W10-SLNGTH = 10
	       ADD 1		 TO W10-I10
	   ELSE
	   IF W10-SLNGTH = 11
	       ADD 1		 TO W10-I11
	   ELSE
	   IF W10-SLNGTH = 12
	       ADD 1		 TO W10-I12
	   ELSE
	   IF W10-SLNGTH = 13
	       ADD 1		 TO W10-I13
	   ELSE
	   IF W10-SLNGTH = 14
	       ADD 1		 TO W10-I14.
	     MOVE W10-ITEM	 TO STK-BC.

       BX25.
      *	     MOVE STK-CODE	 TO W10-CODE2.
      *	   IF (W10-CD2C (1) NOT = "S") OR
      *	      (W10-CD2C (2) NOT NUMERIC)
      *	       UNLOCK STOCK
      *	       GO TO BX20.
      *	     MOVE 1		 TO WS-S7 WS-S8.
      *	     MOVE SPACES	 TO W10-CODE1.

       BX30.
      *	   IF W10-CD2C (WS-S8) = "-" OR " "
      *	       GO TO BX40.
      *	     MOVE W10-CD2C (WS-S8)
      *				 TO W10-CD1C (WS-S7).
      *	   IF WS-S8 < 14
      *	       ADD 1		 TO WS-S7 WS-S8
      *	       GO TO BX30.
      *	   IF WS-S8 = WS-S7
      *	       UNLOCK STOCK
      *	       GO TO BX20.

       BX35.
      *	   IF W10-CD1C (WS-S7) = SPACE
      *	       MOVE ZERO	 TO W10-CD1C (WS-S7)
      *	       IF WS-S7 < 13
      *		   ADD 1	 TO WS-S7
      *		   GO TO BX35.
      *	     MOVE STK-CODE	 TO W10-CODE2.
      *	     PERFORM DELETE-STOCK-REC THRU WRITE-STOCK-EXIT.
      *	     MOVE W10-CODE1	 TO STK-CODE.
      *	     PERFORM WRITE-STOCK THRU WRITE-STOCK-EXIT.
      *	     MOVE W10-CODE2	 TO STK-CODE.
035370*	     PERFORM START-AT-STOCK-CODE THRU READ-STOCK-EXIT.
035460*     IF WS-F-ERROR = 22
035470* 	GO TO BX999.
035720	     GO TO BX20.

       BX40.
	   IF WS-S8 < 14
	       ADD 1		 TO WS-S8
	       GO TO BX30.
	     GO TO BX35.

035730 BX999.
035740       EXIT.

      /
044130 ZA000-INIT	       SECTION.
044140 ZA000-OPEN.
	     ACCEPT W25-FACT FROM DATE.
	     MOVE W25-YY	 TO W12-YEAR.
	     MOVE W25-MM	 TO W12-MONTH.
	     MOVE W25-DD	 TO W12-DAY.
	   IF W25-YY < 50
	       MOVE 20		 TO W12-CENT
	   ELSE
	       MOVE 19		 TO W12-CENT.
	     MOVE W12-TODAY	 TO TODAY-DDMMYY.
003250	     MOVE W12-DAY	 TO W12-DD.
	     MOVE W12-MONTH	 TO W12-MM.
003260	     MOVE W12-CENT	 TO W12-CC.
003270	     MOVE W12-YEAR	 TO W12-YY.
	     PERFORM ZA55 THRU ZA60.
	     MOVE "Utility"	 TO WS-APAC-SYSTEM.
021020       DISPLAY CLR-SCREEN.
	     DISPLAY "���������������������������Ŀ" AT 1126
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "� Company/Business Code     �" AT 1226
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "�����������������������������" AT 1326
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     ACCEPT WS-SYS-ID AT 1250
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE WS-L-OR-N	 TO W02-L-OR-N.
	     OPEN I-O APACFIDS.
044150	     DISPLAY CLR-SCREEN.
      *
      *    ****    D O	 N O T	 D I S P L A Y	 E R R O R
      * 	   M E S S A G E S
      *
	     MOVE 44		 TO USER-NUMBER.
	     MOVE 1		 TO USER-SETTING.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.

       ZA00B.
044300	     OPEN I-O STOCK.
	   IF WS-STATUS = "05" OR "25"
	       MOVE "Code selected - not on file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO ZA00B.
044310     IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 22       TO WS-F-ERROR
                   GO TO ZA50.
044330     IF WS-STATUS NOT = "00"
044340         MOVE 22           TO WS-F-ERROR
050210         PERFORM OPEN-ERROR.
      *
      *    ****    C O N T R O L   F I L E
      *
038470	     OPEN I-O NETWORK.
038480     IF RUNTIME-ERROR
038490         IF FLE-LOCKED
038500             GO TO ZA200
038510         ELSE
038520         IF FLE-LIMIT
038530             GO TO ZA49.
038540     IF WS-STATUS NOT = "00"
038550         MOVE 2            TO WS-F-ERROR
038560         PERFORM OPEN-ERROR.
	     PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
       ZA05.
      *
      *    ****    P A R A M E T E R   F I L E
      *
	     OPEN I-O PARAM.
038210	   IF RUNTIME-ERROR
038220	       IF FLE-LOCKED OR FLE-LIMIT
		   DISPLAY "WAITING" AT 2502
			    WITH BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 14
038230		   GO TO ZA05.
038270	   IF WS-STATUS NOT = "00" AND
	      WS-STAT1 NOT = "4"
038280         MOVE 15           TO WS-F-ERROR
038290         PERFORM OPEN-ERROR.
052980	     MOVE 3		 TO WS-PARKEY.
052990       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-BAR-CODE	 TO WS-WHICH-CODE.
	   IF NOT (WS-WHICH-CODE = "I")
	       MOVE "X" 	 TO WS-WHICH-CODE.
	     MOVE PAR-BC-STYLE	 TO WS-BC-STYLE.
	   IF WS-BC-STYLE = 3
	       MOVE 1		 TO BAR-CODE-TYPE
	   ELSE
	   IF WS-BC-STYLE = 4
	       MOVE 3		 TO BAR-CODE-TYPE
	   ELSE
	   IF WS-BC-STYLE = 6
	       MOVE 2		 TO BAR-CODE-TYPE
	   ELSE
	       MOVE ZERO	 TO BAR-CODE-TYPE.
	     GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.

039850 ZA50.
           IF WS-F-ERROR = 22
	       MOVE "STOCK file" TO WS-FILE.
039980	     DISPLAY "Rebuild " AT 0812
039990                WS-FILE WITH FOREGROUND-COLOR 14.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

040020 ZA51.
040030       EXIT PROGRAM.

       ZA55.
041220       MOVE 1              TO WS-S1.
021870       MOVE SPACES         TO WS-MID-LNE.

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
021950       MOVE WS-G3          TO WS-MCHR (18) WS-MCHR (49)
021960                              WS-MCHR (60) WS-MCHR (70)
                                    WS-MCHR (1)  WS-MCHR (80) 
                                    WS-MCH  (1)  WS-MCH  (80).
021970       MOVE WS-G4          TO WS-BCHR (18) WS-BCHR (49)
021980                              WS-BCHR (60) WS-BCHR (70).
      *      MOVE WS-COMPANY	 TO WS-TOP-COMP.

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
044890 ZB000-ERROR	       SECTION.
      
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
056510	   IF WS-F-ERROR = 15
056520         MOVE WS-PARID     TO WS-FILE
056530         MOVE WS-PARKEY    TO WS-KEY
056620	   ELSE
041850	   IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE STK-CODE	 TO WS-KEYX.
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
