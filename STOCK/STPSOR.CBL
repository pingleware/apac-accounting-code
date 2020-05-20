      $SET LINKCOUNT"576"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	 ******    ******   *******    *
      *   **	**     **     **    **	**    **  **	**  **	  **   *
      *   **	       **     **    **	**	  **	**  **	  **   *
      *    ******      **     *******	 ******   **	**  *******    *
      * 	**     **     **	      **  **	**  **	 **    *
      *   **	**     **     **	**    **  **	**  **	  **   *
      *    ******      **     **	 ******    ******   **	  **   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S T O C K	E N Q U I R Y	P R O G R A M	(SALES ORDERS) *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STPSOR.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   MARCH 1995.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1995 - 2008
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
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

000120 COPY CONTROL.SL.

000200 COPY CORDER.SL.

000160 COPY CREDIT.SL.

000120 COPY DEPART.SL.

000120 COPY PARAM.SL.

000200 COPY PORDER.SL.

000140 COPY STKMEM.SL.

000140 COPY STOCK.SL.

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY APACFIDS.FDE.

000360 COPY CONTROL.FDE.

000520 COPY CORDER.FDE.

000160 COPY CREDIT.FDE.

000420 COPY DEPART.FDE.

       COPY PARAM.FDE.

000520 COPY PORDER.FDE.

000140 COPY STKMEM.FDE.

000540 COPY STOCK.FDE.

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
001860 77  WS-PARKEY	   PIC	9(06)	 COMP-5.
001870 77  WS-NETKEY	   PIC	9(06)	 COMP-5.
001890 77  WS-RECKEY	   PIC	9(06)	 COMP-5.
001900 77  WS-RECOVER	   PIC	9(06)	 COMP-5.
001910 77  WS-TRANS	   PIC	9(04)	 COMP-5 VALUE 1.
001920 77  WS-KEYSTR	   PIC	9(04)	 COMP-5.
001940 77  WS-MONTHS	   PIC S9(01)V99 COMP-3.
001960 77  WS-KEY1	   PIC	9(04)	 COMP-5.
001970 77  WS-KSTORE	   PIC	9(04)	 COMP-5.
       77  WS-ETYPE	   PIC	X(01).
002010 77  WS-PAGE	   PIC	9(04)	 COMP-5.
002020 77  WS-OPTION	   PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
002020 77  WS-SKIP	   PIC	X(01).
002040 77  WS-ADJ	   PIC	X(01).
002070 77  WS-ERROR	   PIC	9(01).
002080 77  WS-IND	   PIC	9(01)	  VALUE 0.
002120 77  WS-AMEND	   PIC	9(01)	 VALUE 0.
002140 77  WS-CONREC	   PIC	9(01) VALUE ZERO.
002150 77  WS-LEDG	   PIC	X(04).
002160 77  WS-LEDG1	   PIC	X(04).
002200 77  WS-COMP	   PIC	9(01).
002210 77  WS-WEEK	   PIC	9(01).
002220 77  WS-TYPE	   PIC	X(01).
002270 77  WS-PASSWORD	   PIC	X(08).
       77  WS-PRINT	   PIC	9(01).
002280 77  WS-PASS	   PIC	X(08).
001330 77  WS-DESC-H1	   PIC	X(14) VALUE "Description".
       77  WS-DESC-H2	   PIC	X(14) VALUE "Description 2".
       77  WS-RT-HD	   PIC	X(10) VALUE "-Retail   ".
       77  WS-WS-HD	   PIC	X(10) VALUE "-Wholesale".
       77  WS-CS-HD	   PIC	X(10) VALUE "-Cash sale".
       77  WS-NODEP	   PIC	X(30) VALUE
			   "NO SUCH   D E P A R T M E N T".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440         05  WS-TCHR PIC X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC X(78).
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
	   03  WS-PROG	       PIC  X(03) VALUE "ORD".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002350 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

000360 COPY CORDER.ID.

000160 COPY CREDIT.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

000360 COPY PORDER.ID.

000140 COPY STKMEM.ID.

000300 COPY STOCK.ID.

       01  W02-FILE-IDS.
           03  W02-PRINTER.
	       05  W02-REPORT PIC X(07) VALUE "STPORD.".
               05  W02-USER   PIC X(05) VALUE SPACES.

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

	   03  W10-CONTACT     PIC  X(30).
	   03  W10-SUPP        PIC  X(05).
	   03  W10-SNAME       PIC  X(40).
	   03  W10-STERMS.
	       05  W10-30      PIC  9(01).
	       05  W10-60      PIC  9(01).
	       05  W10-90      PIC  9(01).
	       05  W10-120     PIC  9(01).
	       05  W10-150     PIC  9(01).
	       05  W10-180     PIC  9(01).
	   03  W10-TERMS REDEFINES W10-STERMS
			       PIC  9(06).
	   03  W10-TEL	       PIC  X(14).

       COPY W11.STK.

       COPY W12.WS.

001360 01  W15-INPUT-AREA.
001480     03  W15-NAME.
001490         05  W15-NCHAR PIC  X(01) OCCURS 40.
001500     03  W15-N         REDEFINES W15-NAME.
001510         05  W15-DESC.
001520             07  W15-ENTRY
                             PIC  X(08).
001530             07  FILLER
                             PIC  X(16).
001540         05  FILLER    PIC  X(16).

       01  W16-CALL.
	   03  W16-ITEM      PIC  X(14).
	   03  W16-CRED      PIC  X(05).
	   03  W16-COST      PIC S9(07)V99 COMP-3.
	   03  W16-CANCEL    PIC  X(01).

       COPY W20.WS.

004310 01  W25-CALCS.
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

       01  W42-ATTRIB5.
           03  FILLER          PIC X(2000).

       01  W42-ATTRIB6.
           03  FILLER          PIC X(2000).

       01  W43-SCREEN5.
           03  FILLER          PIC X(2000).

       01  W43-SCREEN6.
           03  FILLER          PIC X(2000).

       COPY FUNCTION.WS.

003800 COPY W50.WS.

       COPY W75.STK.

       01  W75-CKEYS.
	   03  W75-CS	     PIC  9(02) COMP-5.
	   03  W75-CS1	     PIC  9(02) COMP-5.
           03  W75-CNO       PIC  X(05) OCCURS 18.

004120 01  W95-COMP            PIC X(40).

005120 01  W100-EDIT.
003970     03  W100-V2.
003980         05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V5 REDEFINES W100-V2.
	       05  W100-QNT    PIC  Z(03)9-.
	       05  W100-NO     PIC  Z(03)9.
	       05  FILLER      PIC  X(01).

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

004850 01  S04C.
004860	   03  LINE  2 COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT
004870				 VALUE "STOCK ORDER ENQUIRY".

       COPY STOCK.CRT.

       01  S07.
	   02  S07A.
	   03  LINE  7 COLUMN 18 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE
	   "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  8 COLUMN 18 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE
	   "³      A D D   I T E M   T O   O R D E R   F I L E      ³".
	   03  LINE  9 COLUMN 18 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE
	   "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE 10 COLUMN 18 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE
	   "³Stock code/Ext ".
	   03	       COLUMN 34 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(14) FROM W10-ITEM.
	   03	       COLUMN 48 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE "/".
	   03	       COLUMN 49 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(03) FROM W10-ITM.
	   03	       COLUMN 52 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE
				 "			³".
	   03  LINE 11 COLUMN 18 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE
	   "³Description    ".
	   03	       COLUMN 34 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(30) FROM W10-DESC.
	   03	       COLUMN 64 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE
				 "	    ³".
	   02  S07B.
	   03  LINE 12 COLUMN 18 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE
	   "³Supplier	    ".
	   03	       COLUMN 34 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(05) USING W10-SUPP AUTO.
	   03	       COLUMN 39 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE
				 "			             ³".
	   03  LINE 13 COLUMN 18 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE
	   "³		    ".
	   03	       COLUMN 34 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(40) FROM W10-SNAME.
	   03	       COLUMN 74 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE
				 "³".
	   02  S07C.
	   03  LINE 14 COLUMN 18 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE
	   "³Contact	    ".
	   03	       COLUMN 34 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(30) USING W10-CONTACT AUTO.
	   03	       COLUMN 64 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE
				 "	    ³".
	   02  S07D.
	   03  LINE 15 COLUMN 18 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE
	   "³Telephone      ".
	   03	       COLUMN 34 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(14) USING W10-TEL AUTO.
	   03	       COLUMN 48 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE
				 "                          ³".
	   02  S07E.
	   03  LINE 16 COLUMN 18 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE
	   "³Quantity	    ".
	   03	       COLUMN 34 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  Z(06)9.99 USING W10-ORDER AUTO.
	   03	       COLUMN 44 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE
				 "	                        ³".
	   02  S07F.
	   03  LINE 17 COLUMN 18 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE
	   "³Unit cost	    ".
	   03	       COLUMN 34 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  Z(05)9.999 USING W10-COST AUTO.
	   03	       COLUMN 44 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE
				 "            			³".
	   02  S07G.
	   03  LINE 18 COLUMN 18 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE
	   "³Terms	 30 ".
	   03	       COLUMN 34 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  9 USING W10-30 AUTO.
	   03	       COLUMN 35 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE "  60 ".
	   03	       COLUMN 40 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  9 USING W10-60 AUTO.
	   03	       COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE "  90	".
	   03	       COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  9 USING W10-90 AUTO.
	   03	       COLUMN 47 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE " 120 ".
	   03	       COLUMN 52 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  9 USING W10-120 AUTO.
	   03	       COLUMN 53 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE " 150	".
	   03	       COLUMN 58 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  9 USING W10-150 AUTO.
	   03	       COLUMN 59 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE " 180	".
	   03	       COLUMN 64 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  9 USING W10-180 AUTO.
	   03	       COLUMN 65 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE "	    ³".
	   02  S07H.
	   03  LINE 19 COLUMN 18 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE
	   "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".


008680 01  S-STK.
008690     03  LINE  2 COLUMN 35 FOREGROUND-COLOR 15
008700				 VALUE "STOCK ORDERS".
008710	   03  LINE  3 COLUMN  2 "Start Code/Desc :".
	   03	       COLUMN 34 "/".
008720     03  LINE  4 COLUMN  2 REVERSE-VIDEO VALUE 
008730	       " Item/Bar Code   Description                      Qty  M
      -        "TD M/AV  Ord    Cost  ".
009160	   03  LINE  5 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009160	   03	       COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÑÍÍÍÍÑÍÍÍÍÑÍÍÍÍÑÍÍÍÍÍÍÍÍÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
009160	   03  LINE 22 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009170	   03	       COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÏÍÍÍÍÏÍÍÍÍÏÍÍÍÍÏÍÍÍÍÍÍÍÍÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
008770     03  LINE 23 COLUMN  2 FOREGROUND-COLOR 7 VALUE 
008780       "Key in at least the first character of the code or press".
008790     03          COLUMN 59 FOREGROUND-COLOR 14 VALUE "Esc".
008800     03          COLUMN 62 FOREGROUND-COLOR 7 VALUE " to exit".
008810	   03  LINE 25 COLUMN  2 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1 VALUE "Pressing '".
008820	   03	       COLUMN 12 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 14 VALUE "ENTER".
008830	   03	       COLUMN 17 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1 VALUE
008840                           "' key only, will start from the beginn
008850-                          "ing of the stock file".
008860 01  S-STKI.
008870	   03  LINE  3 COLUMN  2 "Start Item Code :".
008880 01  S-STKC.
008890     03  LINE  3 COLUMN  2 "Start XRef Code :".
008900 01  S-STKD.
008910     03  LINE  3 COLUMN  2 "Start Description :".
008920     03  LINE 23 COLUMN  2 FOREGROUND-COLOR 7 VALUE 
008930       "Key in at least the first character of the description or 
008940-      "press".
008950     03          COLUMN 66 FOREGROUND-COLOR 14 VALUE "Esc".
008960     03          COLUMN 69 FOREGROUND-COLOR 7 VALUE " to exit".
008970 01  SA-STK.
008980	   03  LINE  3 COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
008990                           PIC  X(14) USING W10-ITEM AUTO.

005530 01  SA-STK-3.
005540	   03  LINE  3 COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(03) USING W10-I3 AUTO.

005530 01  SA-STK-4.
005540	   03  LINE  3 COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(04) USING W10-I4 AUTO.

005530 01  SA-STK-5.
005540	   03  LINE  3 COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(05) USING W10-I5 AUTO.

005530 01  SA-STK-6.
005540	   03  LINE  3 COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(06) USING W10-I6 AUTO.

005530 01  SA-STK-7.
005540	   03  LINE  3 COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(07) USING W10-I7 AUTO.

005530 01  SA-STK-8.
005540	   03  LINE  3 COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(08) USING W10-I8 AUTO.

005530 01  SA-STK-9.
005540	   03  LINE  3 COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(09) USING W10-I9 AUTO.

005530 01  SA-STK-10.
005540	   03  LINE  3 COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(10) USING W10-I10 AUTO.

005530 01  SA-STK-11.
005540	   03  LINE  3 COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(11) USING W10-I11 AUTO.

005530 01  SA-STK-12.
005540	   03  LINE  3 COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(12) USING W10-I12 AUTO.

005530 01  SA-STK-13.
005540	   03  LINE  3 COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(13) USING W10-I13 AUTO.

005530 01  SA-STK-14.
005540	   03  LINE  3 COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(14) USING W10-I14 AUTO.

009000 01  SA-STKA.
009010	   03  LINE  3 COLUMN 25 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
009020                           PIC  X(10) USING W10-ACODE AUTO.
009030 01  SA-STKC.
009040	   03  LINE  3 COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
009050                           PIC  X(14) USING W10-CODE2 AUTO.
009060 01  SA-STKD.
009070	   03  LINE  3 COLUMN 35 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
009080                           PIC  X(30) USING W10-DESC AUTO.

006240 01  S-CRD.
006250	   03  LINE  2 COLUMN 33 FOREGROUND-COLOR 15
				 VALUE "CREDITOR LOOKUP".
           03  LINE  3 COLUMN  2 VALUE "Starting Name >".
	   03	       COLUMN 57 VALUE	"<".
           03  LINE  4 COLUMN  2 REVERSE-VIDEO VALUE 
               "Ac/No               Supplier name                  Telep
      -        "hone        Balance   ".
009160	   03  LINE  5 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009160	   03	       COLUMN  2 VALUE "ÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
009160	   03  LINE 22 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009170	   03	       COLUMN  2 VALUE "ÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
	   03  LINE 23 COLUMN  2 FOREGROUND-COLOR 7 VALUE
	    "Key in at least the first character of the name or press ".
           03          COLUMN 59 FOREGROUND-COLOR 14 VALUE "Esc".
	   03	       COLUMN 62 FOREGROUND-COLOR 7 VALUE
				 " to exit          ".
	   03  LINE 25 COLUMN  2 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1 VALUE "Pressing '".
	   03	       COLUMN 12 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 14 VALUE "ENTER".
	   03	       COLUMN 17 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1 VALUE
                                 "' key only, will start from the beginn
      -                          "ing of the creditor file".
       01  SA-CRD.
	   03  LINE  3 COLUMN 17 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC  X(40) USING W15-NAME AUTO.
       01  SD-CRD.
	   03  COLUMN  2 FOREGROUND-COLOR 11 PIC X(05) USING P-NUMBER.
	   03  COLUMN  9 FOREGROUND-COLOR 11 PIC X(40) USING P-NAME.
	   03  COLUMN 51 FOREGROUND-COLOR 11 PIC X(14) USING P-TEL.
	   03  COLUMN 68 FOREGROUND-COLOR 11 PIC Z(07)9.99- USING P-BAL.
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

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
007740 AA000-MAIN	       SECTION.
007750 AA00.
007760       PERFORM ZA000-INIT.
	     MOVE "N"		 TO W16-CANCEL.
	     PERFORM BA000.
	   IF W16-CANCEL = "Y"
	       CANCEL "STP\STPPOR".
	     CLOSE CORDER
		   PORDER.

       AA49.
	     EXIT PROGRAM.

       COPY FUNCTION.CRT.

       SAVE-SCREEN-5.
	     CALL "CBL_READ_SCR_CHATTRS" USING SAVE-POS
					       W43-SCREEN5
					       W42-ATTRIB5
					       W41-LENGTH
					 RETURNING WS-STATUS.

       RESTORE-SCREEN-5.
	     MOVE 2000		 TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_CHATTRS" USING SAVE-POS
						W43-SCREEN5
						W42-ATTRIB5
						W41-LENGTH
					  RETURNING WS-STATUS.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

      *
      *    ****    D I S P L A Y   D E P A R T M E N T	 N A M E
      *
       AA400              SECTION.
       AA401.
	     MOVE W10-LEDG	 TO DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF (WS-F-ERROR = 7) OR
	      (DPT-DESC = SPACES)
	       MOVE WS-NODEP	 TO W10-DEPT
	   ELSE
	       MOVE DPT-DESC	 TO W10-DEPT.
	     DISPLAY W10-DEPT AT 0623
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT
			  BACKGROUND-COLOR 5.
       AA405.
             EXIT.

      /
      *    ****    READ FILES ROUTINES
      *
008180 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY CORDER.RD.

       COPY CREDIT.RD.

       COPY DEPART.RD.

       COPY PARAM.RD.

       COPY PORDER.RD.

000140 COPY STKMEM.RD.

       COPY STOCK.RD.

       COPY STOCK.1ST.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010170 AD000             SECTION.

       COPY CORDER.WR.

       COPY APAC.HLP.

      /
      *    ****    S T O C K   L O O K U P   R O U T I N E
      *
      * 	   O R D E R S
      *
000010 STOCK-LOOKUP                   SECTION.

000030 STOCK-LUP-SCREEN.
000040       DISPLAY CLR-SCREEN. 
000050       DISPLAY S-STK.
             MOVE SPACE          TO WS-OPTION.

       STOCK-LUP-CLR-SCREEN.
000140       MOVE 0601           TO CPOS.
000150       PERFORM STOCK-CRT-EDGE UNTIL CLIN = 22.
000160       MOVE SPACES         TO W10-ACODE W10-DESC 
000170				    W10-CODE2 W10-CODE1
				    W10-ITEM.
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
	     MOVE "I"		 TO WS-ETYPE.

000200 STOCK-LUP-DATA.
	   IF W10-STCK = "A"
021070	       ACCEPT SA-STK
	   ELSE
	   IF W10-SLNGTH = 3
	       ACCEPT SA-STK-3
	   ELSE
	   IF W10-SLNGTH = 4
	       ACCEPT SA-STK-4
	   ELSE
	   IF W10-SLNGTH = 5
	       ACCEPT SA-STK-5
	   ELSE
	   IF W10-SLNGTH = 6
	       ACCEPT SA-STK-6
	   ELSE
	   IF W10-SLNGTH = 7
	       ACCEPT SA-STK-7
	   ELSE
	   IF W10-SLNGTH = 8
	       ACCEPT SA-STK-8
	   ELSE
	   IF W10-SLNGTH = 9
	       ACCEPT SA-STK-9
	   ELSE
	   IF W10-SLNGTH = 10
	       ACCEPT SA-STK-10
	   ELSE
	   IF W10-SLNGTH = 11
	       ACCEPT SA-STK-11
	   ELSE
	   IF W10-SLNGTH = 12
	       ACCEPT SA-STK-12
	   ELSE
	   IF W10-SLNGTH = 13
	       ACCEPT SA-STK-13
	   ELSE
	   IF W10-SLNGTH = 14
	       ACCEPT SA-STK-14.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
000140		     MOVE SPACES TO W10-ITEM
000150		     GO TO STOCK-RESET-LIN
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO STOCK-LUP-DATA.
	   IF NOT (W10-ITEM = SPACES)
	       GO TO STOCK-LUP-CONTINUE.
	     ACCEPT SA-STKD.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
000140		     MOVE SPACES TO W10-ITEM
000150		     GO TO STOCK-RESET-LIN
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO STOCK-LUP-DATA.

       STOCK-LUP-CONTINUE.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
000340	     DISPLAY CLEAR-L25.
000350       MOVE 6 TO CLIN.
000360       MOVE 1 TO W75-S W75-S1.

000380 STOCK-LUP-TAB-LOOP.
000390	     MOVE SPACES	 TO W75-CODE (W75-S1).
000400     IF W75-S1 < 18
000410	       ADD 1		 TO W75-S1
000420         GO TO STOCK-LUP-TAB-LOOP.
000430	     MOVE ZERO		 TO W75-S1.

000450 STOCK-LUP-ENTRY-TYPE.
000470	   IF (W10-ITEM = SPACES) AND (W10-DESC = SPACES)
000480	       MOVE "!" 	 TO W10-ITEM
	   ELSE
	   IF W10-ITEM = SPACES
000640	       MOVE "D" 	 TO WS-ETYPE
000650	       MOVE W10-DESC	 TO W50-DATA
000660	       MOVE 30		 TO W50-LENGTH
	       CALL "CBL_TOUPPER" USING W50-30
				  BY VALUE W50-LENGTH
				  RETURNING WS-STATUS
000720	       MOVE W50-30 TO W10-DESC.
	   IF WS-ETYPE = "D"
000860	       MOVE W10-DESC	 TO STK-DESC
000870	       PERFORM START-AT-STOCK-DESC THRU READ-STOCK-EXIT
	   ELSE
000590	       PERFORM CA00 THRU CA06
000740	       MOVE W10-ITEM	 TO STK-CODE
000750	       PERFORM START-AT-STOCK-CODE THRU READ-STOCK-EXIT.
000880     IF WS-F-ERROR = 22
000890         GO TO STOCK-NO-RECORD.

000910 STOCK-GET-STOCK-REC.
           IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
               GO TO STOCK-SKIP-STOCK-READ.
000920       PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
000930     IF WS-F-ERROR = 22
000940	       SUBTRACT 1	 FROM W75-S
000950         GO TO STOCK-FULL-SCREEN.
	   IF STK-IND > 1
	       GO TO STOCK-GET-STOCK-REC.
	   IF WS-TYPE = "P"
	       IF NOT (STK-IND = 1)
		   GO TO STOCK-GET-STOCK-REC.

       STOCK-SKIP-STOCK-READ.
000960	     MOVE STK-CODE	 TO W75-CODE (W75-S).
	   IF STK-IND = 0
	       MOVE "HIGHLIGHT FOREGROUND-COLOR  3"
				 TO  WS-CONT
	   ELSE
	   IF STK-IND = 1
	       MOVE "HIGHLIGHT FOREGROUND-COLOR  6"
				 TO  WS-CONT
	   ELSE
	   IF STK-IND = 2
	       MOVE "HIGHLIGHT FOREGROUND-COLOR  7"
				 TO  WS-CONT
	   ELSE
	       MOVE "HIGHLIGHT FOREGROUND-COLOR  5"
				 TO  WS-CONT.
000970	     DISPLAY (CLIN, 2) STK-CODE WITH CONTROL WS-CONT.
	   IF NOT (STK-ORD = ZERO)
	       DISPLAY ( , 17) "*" WITH CONTROL WS-CONT.
	     MOVE STK-DESC	 TO W10-DESC.
000980	     DISPLAY ( , 19) W10-SDSC WITH CONTROL WS-CONT.
000990	     MOVE STK-QUANT	 TO W100-QNT.
001000	     DISPLAY ( , 50) W100-QNT WITH CONTROL WS-CONT.
001010	     MOVE STK-MTD	 TO W100-NO.
001020	     DISPLAY ( , 56) W100-NO WITH CONTROL WS-CONT.
001010	     MOVE STK-MAVG	 TO W100-NO.
001020	     DISPLAY ( , 61) W100-NO WITH CONTROL WS-CONT.
	     MOVE STK-COST	 TO W100-S6V2.
001040	     DISPLAY ( , 71) W100-PRICE WITH CONTROL WS-CONT.
	     MOVE ZERO		 TO W100-NO.
001020	     DISPLAY ( , 66) W100-NO WITH CONTROL WS-CONT.
001050     IF CLIN < 21
001060	       ADD 1		 TO CLIN W75-S
001070         GO TO STOCK-GET-STOCK-REC.

001090 STOCK-FULL-SCREEN.
001020	     DISPLAY " " AT 2303 WITH FOREGROUND-COLOR 14
001030		     "," WITH FOREGROUND-COLOR 7
		     "PgUp" WITH FOREGROUND-COLOR 14
                     "/" WITH FOREGROUND-COLOR 7
		     "PgDn" WITH FOREGROUND-COLOR 14
		     "," WITH FOREGROUND-COLOR 7
001040		     "ENTER" WITH FOREGROUND-COLOR 14
001050		     "=view," WITH FOREGROUND-COLOR 7
		     "F8" WITH FOREGROUND-COLOR 14
		     "=order," WITH FOREGROUND-COLOR 7
		     "F9" WITH FOREGROUND-COLOR 14
		     "=memo," WITH FOREGROUND-COLOR 7
		     "F10" WITH FOREGROUND-COLOR 14
		     "=purch.history,"
		      WITH FOREGROUND-COLOR 7
001170		     "Esc" WITH FOREGROUND-COLOR 14
		     "ape" WITH FOREGROUND-COLOR 7.

       STOCK-POSITION-CURSOR.
	     PERFORM SAVE-SCREEN-2.
001220	     MOVE 0		 TO W75-S1.
001230       MOVE 0601           TO CPOS.

001250 STOCK-CRT-ATTRIB.
	     MOVE 1920		 TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
001290	     MOVE 2000		 TO W41-LENGTH.

001310 STOCK-GET-USER-OPT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     DISPLAY SPACE AT 2540
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
001320		     GO TO STOCK-ADJ-NO-KEYS
		 WHEN DOWN-KEY
001340		     GO TO STOCK-CHK-NO-KEYS
		 WHEN ENTER-KEY
		 WHEN CR-KEY
		     PERFORM BD000
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO STOCK-GET-USER-OPT
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-ITEM
		     GO TO STOCK-RESET-LIN
		 WHEN PAGE-DOWN
001340		     GO TO STOCK-GET-NEXT-PAGE
		 WHEN PAGE-UP
001340		     GO TO STOCK-GET-PREV-PAGE
		 WHEN F8-KEY
		     MOVE W42-ATTRIB2
				 TO W42-ATTRIB6
		     MOVE W43-SCREEN2
				 TO W43-SCREEN6
		     PERFORM BE000
		     MOVE W42-ATTRIB6
				 TO W42-ATTRIB2
		     MOVE W43-SCREEN6
				 TO W43-SCREEN2
		 WHEN F9-KEY
		     PERFORM STOCK-ADD-SPECS THRU STOCK-ADD-EXIT
		     PERFORM STOCK-FULL-SCREEN
		 WHEN F10-KEY
002040		     IF NOT (W75-S1 = ZERO)
002070			 MOVE W75-CODE (W75-S1)
				 TO W16-ITEM
			 PERFORM SAVE-SCREEN-5
			 CALL "STP\STPPOR" USING
			       LS-PARID LS-USER-ID LS0-PROGRAMS
			       LS0-SECURITY W16-CALL
			 PERFORM RESTORE-SCREEN-5
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE.
001280	     GO TO STOCK-GET-USER-OPT.

       STOCK-GET-NEXT-PAGE.
001550     IF W75-S < 16
001560         GO TO STOCK-GET-USER-OPT.

       STOCK-NEW-PAGE.
001570       MOVE 0601           TO CPOS.
001580       PERFORM STOCK-CRT-EDGE UNTIL CLIN = 22.
001590       MOVE 6              TO CLIN.
001600       MOVE LOW-VALUES TO W75-KEYS.
001610       MOVE 1 TO W75-S W75-S1.
001620       PERFORM STOCK-LUP-TAB-LOOP.
001630       GO TO STOCK-GET-STOCK-REC.

       STOCK-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
             MOVE "J"            TO WS-OPTION.

       STOCK-GET-PREV-LOOP.
             PERFORM READ-STOCK-PREV THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
	       MOVE SPACE	 TO WS-OPTION
               GO TO STOCK-NEW-PAGE.
	   IF STK-IND > 1
	       GO TO STOCK-GET-PREV-LOOP.
	   IF WS-TYPE = "P"
	       IF NOT (STK-IND = 1)
		   GO TO STOCK-GET-PREV-LOOP.
	   IF WS-IXS1 < 32
               ADD 1             TO WS-IXS1
               GO TO STOCK-GET-PREV-LOOP.
             GO TO STOCK-NEW-PAGE.

       STOCK-SAVE-FULL-SCREEN.
	     DISPLAY " " AT 2572 WITH BACKGROUND-COLOR 3
				      FOREGROUND-COLOR 1.
001990       PERFORM STOCK-CRT-ATTRIB.
001180       MOVE 0 TO W44-FUNCTION.
001190	     CALL X"B7" USING W44-FUNCTION W41-SAVE W43-SCREEN2.
           IF W75-S1 > 0
               GO TO STOCK-HIGHLIGHT.
             GO TO STOCK-GET-USER-OPT.

001650 STOCK-NO-RECORD.
001660	     DISPLAY "No record - " AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
001670		     "N" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
001680		     "ext or "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
001690		     "Q" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
001700		     "uit > <"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

001720 STOCK-GET-USER-OPT-2.
001730	     ACCEPT WS-OPTION AT 2538
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
001790	   IF NOT (WS-OPTION = "N" OR "Q")
001800         GO TO STOCK-GET-USER-OPT-2.
001810     IF WS-OPTION = "N"
001820         GO TO STOCK-LUP-SCREEN.
001830       MOVE SPACES         TO W10-ITEM.
001840	     GO TO STOCK-RESET-LIN.

001860 STOCK-ADJ-NO-KEYS.
001870     IF W75-S1 > 1
001880         SUBTRACT 1        FROM W75-S1 CLIN.
001890       GO TO STOCK-HIGHLIGHT.

001910 STOCK-CHK-NO-KEYS.
001920     IF W75-S1 = ZERO
001930         MOVE 1            TO W75-S1
001940         GO TO STOCK-HIGHLIGHT.
001950     IF W75-S1 < W75-S
001960         ADD 1             TO W75-S1 CLIN.

001980 STOCK-HIGHLIGHT.
001990       PERFORM STOCK-CRT-ATTRIB.
002000       DISPLAY W43-DET (CLIN) AT CPOS WITH REVERSE-VIDEO.
002010       GO TO STOCK-GET-USER-OPT.

       STOCK-ADD-SPECS.
002040     IF W75-S1 = ZERO
002050         GO TO STOCK-ADD-EXIT.
002070       MOVE W75-CODE (W75-S1) 
                                 TO SME-CODE W10-ITEM.
             MOVE 1              TO SME-SEQ.
             PERFORM READ-STKMEM THRU READ-STKMEM-EXIT.
           IF WS-F-ERROR NOT = ZERO
	       DISPLAY "No add. specifications" AT 2545
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
	       CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS
               GO TO STOCK-ADD-EXIT.
             INITIALIZE W200-MEM-FLDS.

       STOCK-ADD-LOOP.
             MOVE SME-SEQ        TO WS-S1.
             MOVE SME-DATA       TO W200-MEM (WS-S1).
             PERFORM READ-STKMEM-NEXT THRU READ-STKMEM-EXIT.
           IF WS-F-ERROR = ZERO
               IF SME-CODE = W10-ITEM
                   GO TO STOCK-ADD-LOOP.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 19		 TO SHADE-COL.
	     MOVE 60		 TO SHADE-WIDTH.
	     MOVE 15		 TO SHADE-LINES.
	     DISPLAY S-MEM.
	     PERFORM SCREEN-SHADOW.
	     DISPLAY SA-MEM.
021280       DISPLAY "ENTER to continue" AT 2355
                      WITH FOREGROUND-COLOR 14.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     PERFORM RESTORE-SCREEN-3.

       STOCK-ADD-EXIT.
             EXIT.

002120 STOCK-CRT-EDGE.
002130	     DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR 0
				      FOREGROUND-COLOR 3
		     "                ³                              ³
      - 	     "   ³    ³    ³    ³         "
		      WITH FOREGROUND-COLOR 3
		     "³" WITH BACKGROUND-COLOR 0
			      FOREGROUND-COLOR 3.
002140       ADD 1 TO CLIN.

002160 STOCK-RESET-LIN.
             MOVE STORE-LIN      TO LIN.

002160 STOCK-LOOKUP-EXIT.
002170       EXIT.

      /
      *    ****    C R E D I T O R   L O O K U P   R O U T I N E
      *
      *            (P R O C E S S   F U N C T I O N   K E Y S)
      *
012510 CREDITOR-LOOKUP         SECTION.

012520 CREDIT-LUP-SCREEN.
012530       DISPLAY CLR-SCREEN. 
             DISPLAY S-CRD.
             MOVE SPACE          TO WS-OPTION.

       CREDIT-LUP-CLR-SCREEN.
             MOVE 0601           TO CPOS.
             PERFORM CREDIT-CRT-EDGE UNTIL CLIN = 22.
	     MOVE SPACES	 TO W15-NAME.

       CREDIT-LUP-DATA.
             ACCEPT SA-CRD.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
000140		     MOVE SPACES TO W10-SUPP
000150		     GO TO CREDIT-LOOKUP-EXIT
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CREDIT-LUP-DATA.
	     MOVE SPACES	 TO WS-BLNK78.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
	     DISPLAY SPACE AT 2501
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     MOVE ALL "°"	 TO WS-BLNK78.
             MOVE 6 TO CLIN.
	     MOVE 1 TO W75-CS W75-CS1.

       CREDIT-LUP-TAB-LOOP.
	     MOVE SPACES	 TO W75-CNO (W75-CS1).
	   IF W75-CS1 < 18
	       ADD 1		 TO W75-CS1
               GO TO CREDIT-LUP-TAB-LOOP.
	     MOVE ZERO		 TO W75-CS1.

       CREDIT-LUP-ENTRY-TYPE.
           IF W15-NAME = SPACES
               MOVE "!"          TO W15-NAME.
             MOVE 40             TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W15-NAME
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
             MOVE W15-NAME       TO P-NAME.
             PERFORM START-AT-CRED-NAME THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
               GO TO CREDIT-NO-RECORD.

       CREDIT-GET-CRED-RECORD.
           IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
               GO TO CREDIT-SKIP-CRED-READ.
027520       PERFORM READ-CREDIT-NEXT THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
	       SUBTRACT 1	 FROM W75-CS
               GO TO CREDIT-FULL-SCREEN.

       CREDIT-SKIP-CRED-READ.
	     MOVE P-NUMBER	 TO W75-CNO (W75-CS).
	     DISPLAY SD-CRD AT LINE CLIN.
027700	   IF CLIN < 21
027710	       ADD 1 TO CLIN W75-CS
027720         GO TO CREDIT-GET-CRED-RECORD.

       CREDIT-FULL-SCREEN.
001020	     DISPLAY " " AT 2303 WITH FOREGROUND-COLOR 14
001030		     " highlight - " WITH FOREGROUND-COLOR 7
		     "PgUp" WITH FOREGROUND-COLOR 14
                     "/" WITH FOREGROUND-COLOR 7
		     "PgDn" WITH FOREGROUND-COLOR 14
		     " - " WITH FOREGROUND-COLOR 7
001040		     "ENTER" WITH FOREGROUND-COLOR 14
001050		     " select - " WITH FOREGROUND-COLOR 7
001170		     "Esc" WITH FOREGROUND-COLOR 14
		     "ape" WITH FOREGROUND-COLOR 7.

       CREDIT-SAVE-FULL-SCREEN.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 0		 TO W75-CS1.
             MOVE 0601           TO CPOS.

       CREDIT-CRT-ATTRIB.
             MOVE 1920           TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
	     MOVE 2000		 TO W41-LENGTH.

       CREDIT-GET-USER-OPT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     DISPLAY CLEAR-L25.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO CREDIT-ADJ-NO-KEYS
		 WHEN DOWN-KEY
001340		     GO TO CREDIT-CHK-NO-KEYS
		 WHEN ENTER-KEY
		 WHEN CR-KEY
		     GO TO CREDIT-CHK-SELECT
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CREDIT-GET-USER-OPT
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-SUPP
		     GO TO CREDIT-LOOKUP-EXIT
		 WHEN PAGE-DOWN
001340		     GO TO CREDIT-GET-NEXT-PAGE
		 WHEN PAGE-UP
001340		     GO TO CREDIT-GET-PREV-PAGE
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CREDIT-GET-USER-OPT.
	     CALL X"E5".
001280	     GO TO CREDIT-GET-USER-OPT.

001240 CREDIT-GET-NEXT-PAGE.
	   IF W75-CS < 16
               GO TO CREDIT-GET-USER-OPT.

       CREDIT-NEW-PAGE.
             MOVE 0601           TO CPOS.
             PERFORM CREDIT-CRT-EDGE UNTIL CLIN = 22.
             MOVE 6              TO CLIN.
	     MOVE LOW-VALUES TO W75-CKEYS.
	     MOVE 1 TO W75-CS W75-CS1.
             PERFORM CREDIT-LUP-TAB-LOOP.
027830       GO TO CREDIT-GET-CRED-RECORD.

       CREDIT-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
             MOVE "J"            TO WS-OPTION.

       CREDIT-GET-PREV-LOOP.
             PERFORM READ-CREDIT-PREV THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
	       MOVE SPACE	 TO WS-OPTION
               GO TO CREDIT-NEW-PAGE.
           IF WS-IXS1 < 32
               ADD 1             TO WS-IXS1
               GO TO CREDIT-GET-PREV-LOOP.
             GO TO CREDIT-NEW-PAGE.

027970 CREDIT-NO-RECORD.
027730	     DISPLAY "No record - " AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "N" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ext or "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Q" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "uit > <"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       CREDIT-GET-USER-OPT-2.
027990	     ACCEPT WS-OPTION AT 2538
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
027750       CALL "_MSUPCASE" USING WS-OPTION
023170                        LENGTH WS-OPTION
023180                               WS-LENGTH
023190                        LENGTH WS-LENGTH
023200                               WS-STATUS.
           IF NOT (WS-OPTION = "N" OR "Q")
               GO TO CREDIT-GET-USER-OPT-2.
027800     IF WS-OPTION = "N"
027810         GO TO CREDIT-LUP-SCREEN.
	     MOVE SPACES	 TO W10-SUPP.
028000       GO TO CREDIT-LOOKUP-EXIT.

       CREDIT-ADJ-NO-KEYS.
	   IF W75-CS1 > 1
	       SUBTRACT 1 FROM W75-CS1 CLIN.
             GO TO CREDIT-HIGHLIGHT.

       CREDIT-CHK-NO-KEYS.
	   IF W75-CS1 = ZERO
	       MOVE 1 TO W75-CS1
               GO TO CREDIT-HIGHLIGHT.
	   IF W75-CS1 < W75-CS
	       ADD 1 TO W75-CS1 CLIN.

       CREDIT-HIGHLIGHT.
             PERFORM CREDIT-CRT-ATTRIB.
             DISPLAY W43-DET (CLIN) AT CPOS WITH REVERSE-VIDEO.
             GO TO CREDIT-GET-USER-OPT.

       CREDIT-CHK-SELECT.
	   IF W75-CS1 = ZERO
               GO TO CREDIT-GET-USER-OPT.
             MOVE CSTART TO WS1-CURSOR.
	     MOVE W75-CNO (W75-CS1) TO P-NUMBER.
             PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
             MOVE ZERO TO COL-AND-LINE.
	     MOVE P-NUMBER	 TO W10-SUPP.
             GO TO CREDIT-LOOKUP-EXIT.

       CREDIT-CRT-EDGE.
002130	     DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR 0
				      FOREGROUND-COLOR 3
		     "      ³                                         ³
      - 	     "             ³            "
		      WITH FOREGROUND-COLOR 3
		     "³" WITH BACKGROUND-COLOR 0
			      FOREGROUND-COLOR 3.
019260	     ADD 1 TO CLIN.

       CREDIT-LOOKUP-EXIT.
             EXIT.

       COPY CA000.STK.

       COPY CA100.STK.

       COPY CA200.STK.

       COPY CA500.STK.

      /
      *  ****  E N Q U I R Y   ( O R D E R S )
      *
021000 BA000		       SECTION 50.
021010 BA000-ENQUIRY.
021020	     DISPLAY CLR-SCREEN.
	     PERFORM STOCK-LOOKUP.
021020       DISPLAY CLR-SCREEN.
021030	     DISPLAY S04C.
021040	     DISPLAY "Esc" AT 1210 WITH BACKGROUND-COLOR 5
					FOREGROUND-COLOR 6 HIGHLIGHT
		     "ape or " WITH BACKGROUND-COLOR 5
				    FOREGROUND-COLOR 3
		     "N" WITH BACKGROUND-COLOR 5
			      FOREGROUND-COLOR 6 HIGHLIGHT
		     "ext range" WITH BACKGROUND-COLOR 5
				      FOREGROUND-COLOR 3.
021050       MOVE SPACE          TO W10-ITEM.

       BA10.
021290	     ACCEPT WS-OPTION AT 2531
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                     MOVE SPACES TO W10-ITEM
		     GO TO BA999
                 WHEN OTHER
                     CALL X"E5"
		     GO TO BA10
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
020250	   IF NOT (WS-OPTION = "N")
	       GO TO BA10.
	     GO TO BA000-ENQUIRY.

021720 BA999.
021730       EXIT.

      /
      *  ****  D I S P L A Y   S T O C K   D E T A I L S
      *
021000 BD000		       SECTION 51.
021010 BD000-ENQUIRY.
002040	   IF W75-S1 = ZERO
002050	       GO TO BD999.
011050	     PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090       PERFORM SCREEN-CONTENTS.
021020	     DISPLAY CLR-SCREEN.
021040       DISPLAY S04.
002070	     MOVE W75-CODE (W75-S1)
				 TO W10-ITEM.

       BD05.
021100       PERFORM CA000.
037200	     MOVE STK-CODE	 TO W10-ITEM.
037210       MOVE STK-XREF       TO W10-CODE2.
037220       MOVE STK-BIN        TO W10-BIN.
037230       MOVE STK-DESC       TO W10-DESC.
037240       MOVE STK-TAX        TO W10-TAX.
037250       MOVE STK-MARKUP     TO W10-MARKUP.
037260       MOVE STK-CMARKUP    TO W10-CMARKUP.
037270       MOVE STK-WMARKUP    TO W10-WMARKUP.
037280       MOVE STK-IND        TO W10-IND.
037290       MOVE STK-QUANT      TO W10-QUANT.
037300       MOVE STK-LEVEL      TO W10-REORD.
037310       MOVE STK-LTIME      TO W10-LTIME.
037320       MOVE STK-COST       TO W10-COST.
037330       MOVE STK-SELL       TO W10-SELL.
037340       MOVE STK-CASH       TO W10-CASH.
037350       MOVE STK-WSALE      TO W10-WSALE.
037360       MOVE STK-LEDG       TO W10-LEDG.
037370       MOVE STK-GUAR       TO W10-GUAR.
037380       MOVE STK-ORD        TO W10-ORDER.
037390       MOVE STK-DTE        TO W10-ODTE.
037400       MOVE STK-PUR        TO W10-PURCH.       
	   IF STK-IND < 2
016950	       IF STK-AVGE = ZERO
016960		   MOVE STK-COST TO STK-AVGE.
037410	     MOVE STK-AVGE	 TO W10-AVRG.
037420       MOVE STK-ACODE      TO W10-ACODE.
010310	     COMPUTE W09-WMARKUP ROUNDED =
		   ((STK-WSALE - STK-COST) * 100.000) / STK-COST.
010310	     COMPUTE W09-MARKUP ROUNDED =
		   ((STK-SELL - STK-COST) * 100.000) / STK-COST.
010330	     COMPUTE W09-CMARKUP ROUNDED =
		   ((STK-CASH - STK-COST) * 100.000) / STK-COST.
	     DISPLAY S05.
021150	   IF STK-ACT = ZERO
021160         MOVE SPACE TO W10-ACTIVE
021170     ELSE
021180         MOVE STK-ACT TO W20-DTE
021190         STRING W20-DAY DELIMITED SIZE "/" DELIMITED SIZE
021200                W20-MONTH DELIMITED SIZE "/" DELIMITED SIZE
021210                W20-YEAR DELIMITED SIZE INTO W10-ACTIVE.
021220       MOVE STK-MTDC TO W10-MTDC.
021230       MOVE STK-YTDC TO W10-YTDC.
021240	   IF LS-OK = "N"
021250         MOVE ZERO         TO W10-COST W10-MARKUP  
021260                              W10-AVRG W10-CMARKUP 
                                    W10-MTDC W10-WMARKUP 
				    W10-YTDC
	   ELSE
	      MOVE W09-MARKUP	 TO W10-MARKUP
	      MOVE W09-CMARKUP	 TO W10-CMARKUP
	      MOVE W09-WMARKUP	 TO W10-WMARKUP.
021270       DISPLAY S06.
             PERFORM AA400.
           IF NOT (STK-SUPP = SPACES OR LOW-VALUES)
               MOVE STK-SUPP     TO P-NUMBER
               PERFORM READ-CREDIT THRU READ-CREDIT-EXIT
               IF WS-F-ERROR = ZERO
		   DISPLAY "Supplier      " AT 2306
			    P-NAME AT 2322
			    WITH FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5.
	   IF NOT (STK-ORD = ZERO)
	       MOVE STK-CODE	 TO ORD-ITEM
	       PERFORM START-AT-ORD-ITEM THRU READ-PORDER-EXIT
	       IF WS-F-ERROR = ZERO
		   PERFORM READ-PORDER-NEXT THRU READ-PORDER-EXIT
		   IF ORD-ITEM = STK-CODE
		       MOVE ORD-CRED	 TO P-NUMBER
		       PERFORM READ-CREDIT THRU READ-CREDIT-EXIT
		       IF WS-F-ERROR = ZERO
			   DISPLAY "Supp:" AT 1533
				    P-NAME AT 1539
				    WITH FOREGROUND-COLOR 3 HIGHLIGHT
					 BACKGROUND-COLOR 5.

       BD10.
	     DISPLAY "F9" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
                     " to show additional specifications"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
021280       DISPLAY "Press any other key to continue" AT 2540
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
021290	     ACCEPT WS-OPTION AT 2572
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO.
019250	     DISPLAY CLEAR-L25.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
		 WHEN NOT F9-KEY GO TO BD20
	       END-EVALUATE
	   ELSE
	       GO TO BD20.
	     MOVE STK-CODE	 TO SME-CODE.
             MOVE 1              TO SME-SEQ.
             PERFORM READ-STKMEM-LOCK THRU READ-STKMEM-EXIT.
           IF WS-F-ERROR NOT = ZERO
	       MOVE W42-ATTRIB	 TO W42-ATTRIB6
	       MOVE W43-SCREEN	 TO W43-SCREEN6
	       MOVE "No additional specifications"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       MOVE W42-ATTRIB6  TO W42-ATTRIB
	       MOVE W43-SCREEN6  TO W43-SCREEN
	       GO TO BD10.
             INITIALIZE W200-MEM-FLDS.

       BD15.
             MOVE SME-SEQ        TO WS-S1.
             MOVE SME-DATA       TO W200-MEM (WS-S1).
             PERFORM READ-STKMEM-NEXT-LOCK THRU READ-STKMEM-EXIT.
           IF WS-F-ERROR = ZERO
               IF SME-CODE = STK-CODE
		   GO TO BD15.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 19		 TO SHADE-COL.
	     MOVE 60		 TO SHADE-WIDTH.
             MOVE 15             TO SHADE-LINES.
             DISPLAY S-MEM.
             PERFORM SCREEN-SHADOW.
	     DISPLAY SA-MEM.
021280       DISPLAY "ENTER to continue" AT 2324
                      WITH FOREGROUND-COLOR 14.
021290	     ACCEPT WS-OPTION AT 2342
		    WITH FOREGROUND-COLOR 15 BACKGROUND-COLOR 5 AUTO.
	     PERFORM RESTORE-SCREEN-3.
	     GO TO BD10.

       BD20.
	     MOVE 18		 TO W75-S.

       BD25.
	   IF W75-CODE (W75-S) = SPACES
	       SUBTRACT 1	 FROM W75-S
	       GO TO BD25.
002070	     MOVE W75-CODE (W75-S)
				 TO W10-ITEM.
021100       PERFORM CA000.
	   IF WS-ETYPE = "I"
	       GO TO BD35.
000870	     PERFORM START-AT-STOCK-DESC THRU READ-STOCK-EXIT.
       BD30.
000920       PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
	   IF NOT (W10-ITEM = STK-CODE)
	       GO TO BD30.

       BD35.
011320	     PERFORM RESTORE-SCREEN.

021720 BD999.
021730       EXIT.

       BE000	       SECTION 52.
       BE00.
002040	   IF W75-S1 = ZERO
002050	       GO TO BE999.
	     MOVE WS1-CURS	 TO CRT-LINE.
011050	     PERFORM SAVE-SCREEN.
002070	     MOVE W75-CODE (W75-S1)
				 TO W10-ITEM.
021100       PERFORM CA000.
	     MOVE STK-DESC	 TO W10-DESC.
	     MOVE STK-COST	 TO W10-COST.
	     MOVE ZERO		 TO W10-ORDER W10-TERMS.
	     MOVE SPACES	 TO W10-SUPP  W10-SNAME W10-ITM.
	   IF STK-IND = ZERO
	       IF NOT (STK-QUANT > STK-LEVEL)
		   COMPUTE W10-QUANT ROUNDED =
		       (STK-MAVG * STK-LTIME * 2) - STK-QUANT
		   MOVE W10-QUANT    TO W10-ORDER.
	   IF NOT (STK-SUPP = SPACES OR LOW-VALUES)
               MOVE STK-SUPP     TO P-NUMBER
               PERFORM READ-CREDIT THRU READ-CREDIT-EXIT
               IF WS-F-ERROR = ZERO
		   MOVE P-NUMBER TO W10-SUPP
		   MOVE P-NAME	 TO W10-SNAME
		   MOVE P-CONTACT
				 TO W10-CONTACT
		   MOVE P-TEL	 TO W10-TEL
		   MOVE P-TERMS  TO W10-STERMS.
021040	     DISPLAY S07.
	     MOVE 8		 TO SHADE-ROW.
	     MOVE 20		 TO SHADE-COL.
	     MOVE 55		 TO SHADE-WIDTH.
	     MOVE 12		 TO SHADE-LINES.
             PERFORM SCREEN-SHADOW.

       BE05.
018790	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
018800		     "=Creditor Lookup"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F10" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
018800		     "=Purchase History"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     ACCEPT S07B.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-SUPP
		     GO TO BE15
016550		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
011050		     PERFORM SAVE-SCREEN-5
011080		     MOVE 4	 TO W44-FUNCTION
011090		     PERFORM SCREEN-CONTENTS
010750		     PERFORM CREDITOR-LOOKUP
011320		     PERFORM RESTORE-SCREEN-5
		 WHEN F10-KEY
002070		     MOVE W10-ITEM
				 TO W16-ITEM
		     MOVE W10-SUPP
				 TO W16-CRED
		     MOVE W10-COST
				 TO W16-COST
		     PERFORM SAVE-SCREEN-5
		     CALL "STP\STPPOR" USING
			   LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY W16-CALL
		     PERFORM RESTORE-SCREEN-5
		     MOVE W16-CRED
				 TO W10-SUPP
		     MOVE W16-COST
				 TO W10-COST
		     DISPLAY S07
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BE05
	       END-EVALUATE
018840	       DISPLAY W10-SUPP AT 1234
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 5
018850	       IF W10-SUPP = SPACES
018860		   GO TO BE05.
013020	   IF W10-SUPP = SPACES
013030	       GO TO BE15.
	     MOVE 5		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W10-SUPP
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     MOVE W10-SUPP	 TO P-NUMBER.
011130	     PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
011140     IF WS-F-ERROR = 3 
011150	       MOVE 1		 TO WS-ERROR
011160     ELSE
011170	   IF P-NUMBER NOT = W10-SUPP
011180	       MOVE 2		 TO WS-ERROR
011190     ELSE
011200	       MOVE 0		 TO WS-ERROR.
013050	   IF WS-ERROR NOT = ZERO
013060	       MOVE "No Record"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
013070	       GO TO BE05.
	     MOVE P-NAME	 TO W10-SNAME.
	     MOVE P-NUMBER	 TO COR-CRED.
	     MOVE W10-ITEM	 TO COR-ITEM.
	     MOVE W10-ITM	 TO COR-ITM.
	     MOVE W12-T-YMD	 TO COR-DATE.
	     PERFORM READ-CORDER THRU READ-CORDER-EXIT.
	   IF WS-F-ERROR = 48
	       MOVE P-CONTACT	 TO W10-CONTACT
	       MOVE P-TERMS	 TO W10-STERMS
	       DISPLAY S07B
	       MOVE ZERO	 TO WS-IND
	   ELSE
	       MOVE 1		 TO WS-IND
	       MOVE COR-CONTACT  TO W10-CONTACT
	       MOVE COR-TERMS	 TO W10-TERMS
	       MOVE COR-QUANT	 TO W10-ORDER
	       MOVE COR-UCOST	 TO W10-COST
	       DISPLAY S07.
000340	     DISPLAY CLEAR-L25.
	     ACCEPT S07C.
	     ACCEPT S07E.
	   IF W10-ORDER = ZERO
	       GO TO BE15.

       BE10.
	     ACCEPT S07F.
	   IF W10-COST = ZERO
	       GO TO BE10.
	     MOVE W42-ATTRIB	 TO W42-ATTRIB5.
	     MOVE W43-SCREEN	 TO W43-SCREEN5.
	     PERFORM CHECK-CORRECT.
	     MOVE W42-ATTRIB5	 TO W42-ATTRIB.
	     MOVE W43-SCREEN5	 TO W43-SCREEN.
018520	   IF WS-OPTION = "N"
018530	       GO TO BE05.
	     MOVE W10-SUPP	 TO COR-CRED.
	     MOVE W10-ITEM	 TO COR-ITEM.
	     MOVE W10-ITM	 TO COR-ITM.
	     MOVE W12-T-YMD	 TO COR-DATE.
	     MOVE W10-ORDER	 TO COR-QUANT.
	     MOVE W10-COST	 TO COR-UCOST.
	     MOVE W10-TERMS	 TO COR-TERMS.
	     MOVE W10-CONTACT	 TO COR-CONTACT.
	     MOVE LS0-NO	 TO COR-DELIV.
	   IF WS-IND = ZERO
	       PERFORM WRITE-CORDER THRU WRITE-CORDER-EXIT
	   ELSE
	       PERFORM REWRITE-CORDER THRU WRITE-CORDER-EXIT.

       BE15.
	     MOVE 18		 TO W75-S.

       BE20.
	   IF W75-CODE (W75-S) = SPACES
	       SUBTRACT 1	 FROM W75-S
	       GO TO BE20.
002070	     MOVE W75-CODE (W75-S)
				 TO W10-ITEM.
021100       PERFORM CA000.
	   IF WS-ETYPE = "I"
	       GO TO BE35.
000870	     PERFORM START-AT-STOCK-DESC THRU READ-STOCK-EXIT.

       BE30.
000920       PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
	   IF NOT (W10-ITEM = STK-CODE)
	       GO TO BE30.

       BE35.

011320	     PERFORM RESTORE-SCREEN.
	     MOVE CRT-LINE	 TO WS1-CURS.

       BE999.
	     EXIT.

      /
044130 ZA000-INIT	       SECTION 90.
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
	     MOVE "CORDER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-CORDER.
	     MOVE "CREDIT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CREDIT.
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "PORDER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-PORDER.
	     MOVE "STKMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKMEM.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
044230	     MOVE 3		 TO WS-PARKEY.
044240       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.
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
	   IF NOT (PAR-SAL-ORD = "P" OR "C")
	       MOVE "P" 	 TO WS-TYPE
	   ELSE
	       MOVE PAR-SAL-ORD  TO WS-TYPE.
044580	     MOVE 1		 TO WS-PARKEY.
044590       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
044600       MOVE PAR-DMY        TO W12-TODAY.
044610       MOVE PAR-YMD        TO W12-T-YMD.
044620       MOVE PAR-COMPANY    TO W95-COMP.
044650       MOVE 3              TO WS-NETKEY.
044660       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
044750       MOVE STK-WEEK       TO WS-WEEK.
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
042520	     MOVE 8		 TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF NOT (PAR-DESC-H1 = X"0000000000000000000000000000")
	       MOVE PAR-DESC-H1  TO WS-DESC-H1.
	   IF NOT (PAR-DESC-H2 = X"0000000000000000000000000000")
	       MOVE PAR-DESC-H2  TO WS-DESC-H2.
	   IF NOT (PAR-WS-HD = X"00000000000000000000")
	       MOVE PAR-WS-HD	 TO WS-WS-HD.
	   IF NOT (PAR-CS-HD = X"00000000000000000000")
	       MOVE PAR-CS-HD	 TO WS-CS-HD.
	   IF NOT (PAR-RT-HD = X"00000000000000000000")
	       MOVE PAR-RT-HD	 TO WS-RT-HD.
042170	     OPEN I-O PORDER.
042180     IF RUNTIME-ERROR
               IF FLE-LOCKED
042130             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 16       TO WS-F-ERROR
                   GO TO ZA50.
050190     IF WS-STATUS NOT = "00"
050200         MOVE 16           TO WS-F-ERROR
050210         PERFORM OPEN-ERROR.
044300	     OPEN I-O CORDER.
           IF WS-STATUS = "05"
	       CLOSE CORDER
	       OPEN OUTPUT CORDER
	       CLOSE CORDER
	       OPEN I-O CORDER.
044310     IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
		   MOVE 48	 TO WS-F-ERROR
                   GO TO ZA50.
	     GO TO ZA999-EXIT.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
039850 ZA50.
041850	   IF WS-F-ERROR = 16
041860         MOVE "PURCHASE ORDER"
				 TO WS-FILE
041840	   ELSE
041850	   IF WS-F-ERROR = 48
041860	       MOVE "ORDER file"
				 TO WS-FILE.
039980       DISPLAY "Rebuild " AT 0812
039990                WS-FILE WITH FOREGROUND-COLOR 14.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040020 ZA51.
040030       EXIT PROGRAM.
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
021950       MOVE WS-G3          TO WS-MCHR (18) WS-MCHR (49)
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
044890 ZB000-ERROR	       SECTION 91.
      
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
041800	   ELSE
041650	   IF WS-F-ERROR = 3
041660         MOVE W02-CREDIT   TO WS-FILE
041670         MOVE ZERO         TO WS-KEY
               MOVE P-NUMBER     TO WS-KEYX
041800     ELSE
041810	   IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 16
041860         MOVE W02-PORDER   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 25
041860         MOVE W02-STKMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE SME-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 48
041860	       MOVE W02-CORDER	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE COR-KEY	 TO WS-KEYX.
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
