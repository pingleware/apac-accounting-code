      $SET LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	  ****	  *******   ********   *
      *   **	**     **     **    **	 **  **   **	**     **      *
      *   **	       **     **    **	**    **  **	**     **      *
      *    ******      **     *******	********  **	**     **      *
      * 	**     **     **	**    **  **	**     **      *
      *   **	**     **     **	**    **  **	**  ** **      *
      *    ******      **     **	**    **  *******    ***       *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     A D J U S T M E N T	  T R A C K I N G   P R O G R A M      *
      *                                                                *
      *     VERSION 8.13.09 - November 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STPADJ.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.	       APRIL 1999.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1999 - 2009
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

000120 COPY DEPART.SL.

000120 COPY PARAM.SL.

       COPY STKDEX.SL.

000140 COPY STOCK.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

000120 COPY CONTROL.FDE.

000420 COPY DEPART.FDE.

000290 COPY PARAM.FDE.

       COPY STKDEX.FDE.

000540 COPY STOCK.FDE.

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   ** 
      *         **    *    **   **    **   **   **    **  **  
      *         **   ***   **   **    **   ******     *****   
      *          ** ** ** **    **    **   **   **    **  **  
      *           ***   ***     **    **   **    **   **   ** 
      *            *     *       ******    **    **   **    **
      *
001550 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001560 77  WS-SUB	   PIC	9(04)	 COMP-5.
001560 77  WS-S            PIC  9(04)    COMP-5.
001570 77  WS-S1           PIC  9(04)    COMP-5.
001580 77  WS-S2           PIC  9(04)    COMP-5.
001590 77  WS-S3           PIC  9(04)    COMP-5.
001600 77  WS-S4           PIC  9(04)    COMP-5.
001610 77  WS-S5           PIC  9(04)    COMP-5.
001620 77  WS-S6           PIC  9(04)    COMP-5.
001630 77  WS-S7           PIC  9(04)    COMP-5.
001640 77  WS-S8           PIC  9(04)    COMP-5.
001650 77  WS-S9           PIC  9(04)    COMP-5.
002850 77  WS-IXS1         PIC  9(04)    COMP-5.
003270 77  WS-PAGE	   PIC	9(04)	 COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
001690 77  WS-ENQPOS	   PIC	9(06)	 COMP-5.
001700 77  WS-ENQEND       PIC  9(06)    COMP-5.
001670 77  WS-NETKEY	   PIC	9(06)	 COMP-5.
001660 77  WS-PARKEY       PIC  9(06)    COMP-5.
001810 77  WS-KEYSTR       PIC  9(06)    COMP-5.
001820 77  WS-OPTION       PIC  X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
001820 77  WS-ENQ	   PIC	X(01).
001820 77  WS-REP	   PIC	X(01).
       77  WS-MONTH	   PIC	9(02).
001820 77  WS-ETYPE	   PIC	X(01).
002270 77  WS-REFERENCE    PIC	X(08).
001330 77  WS-DESC-H1	   PIC	X(14) VALUE "Description".
       77  WS-DESC-H2	   PIC	X(14) VALUE "Description 2".
       77  WS-RT-HD	   PIC	X(10) VALUE "-Retail   ".
       77  WS-WS-HD	   PIC	X(10) VALUE "-Wholesale".
       77  WS-CS-HD	   PIC	X(10) VALUE "-Cash sale".
001850 77  WS-ERROR	   PIC	9(01) VALUE ZERO.
002020 77  WS-ERR2         PIC  X(09) VALUE "No Record".
       77  PRG-NAME	   PIC	X(12) VALUE "STP\STPLOOK".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
       77  WS-QTOT	   PIC S9(07)V99 COMP-3.
       77  WS-VTOT	   PIC S9(09)V99 COMP-3.

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
003290	   03  WS-MID-LNE.
003300	       05  WS-MCHR  PIC  X(80) VALUE "³                ³
      -        "                      ³          ³         ³         ³".
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

002230 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "STP".
	   03  WS-PROG	       PIC  X(03) VALUE "ADJ".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002310 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

000140 COPY DEPART.ID.

       COPY PARAM.ID.

000300 COPY STKDEX.ID.

000140 COPY STOCK.ID.

       01  W02-PRINTER-DETAILS.
	   03  W02-PRINTER     PIC  X(12).
002570	   03  W02-PGE-LENGTH  PIC  9(02).
001870	   03  W02-PRN-LENGTH  PIC  9(02).
	   03  W02-LINAGE      PIC  9(02).
	   03  W02-PRN-STATUS  PIC  X(01) VALUE "C".
      *
      *    ****    D  =  Detail line
      * 	   C  =  Condensed print
      * 	   E  =  Expanded print
      * 	   H  =  Header line
      * 	   X  =  Cancel expanded print
      * 	   1  =  10 Characters per inch
      * 	   2  =  12 Characters per inch
      * 	   3  =  17 Characters per inch
      * 	   6  =  6 Lines per inch
      * 	   8  =  8 Lines per inch
      *
	   03  W02-PRN-TYPE    PIC  X(01).
	   03  W02-PRN-LINE    PIC  X(136).
	   03  R-SL1 REDEFINES W02-PRN-LINE.
	       05  R-SLA       PIC  X(78).
	       05  FILLER      PIC  X(58).
	   03  R-SL2 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(58).
	       05  R-SLB       PIC  X(78).
000390	   03  REP-LINE1 REDEFINES W02-PRN-LINE.
000400	       05  REP-DETAIL1 PIC  X(140).
000410	   03  REP-LINE2 REDEFINES W02-PRN-LINE.
000420	       05  REP-DETAIL2 PIC  X(80).
000430	   03  REP-LINE3 REDEFINES W02-PRN-LINE.
000490	       05  REP-DATE    PIC  X(06).
000500	       05  REP-DTE     PIC  Z9/99/9999.
000460	       05  FILLER      PIC  X(03).
	       05  REP-PRC-HD.
		   07  FILLER  PIC  X(01).
000470		   07  REP-COMPANY  PIC	X(40).
		   07  FILLER  PIC  X(02).
000480	       05  FILLER      PIC  X(08).
000440	       05  REP-PAGE    PIC  X(06).
000450	       05  REP-P-NO    PIC  Z(03)9.
000430	   03  REP-LINE4 REDEFINES W02-PRN-LINE.
	       05  REP-HEAD    PIC  X(96).
	   03  REP-LINE5 REDEFINES W02-PRN-LINE.
	       05  REP-ITEM    PIC  X(18).
	       05  FILLER      PIC  X(01).
	       05  REP-DESC    PIC  X(30).
	       05  FILLER      PIC  X(01).
	       05  REP-TOT-LINE.
		 07  REP-QUANT PIC  Z(06)9.99-.
		 07  FILLER    PIC  X(01).
		 07  REP-COST  PIC  Z(08)9.99-.
	       05  FILLER      PIC  X(01).
	       05  REP-REMK    PIC  X(20).

       COPY W10.STK.

       COPY W12.WS.

       01  W15-DATES.
004120	   03  W15-START       PIC 9(08).
004130     03  W15-SDTE-DMY REDEFINES W15-START.
004140         05  W15-SDAY    PIC 9(02).
004150         05  W15-SMNTH   PIC 9(02).
	       05  W15-SCENT   PIC 9(02).
004160         05  W15-SYEAR   PIC 9(02).
004170	   03  W15-S-YMD       PIC 9(08).
004180     03  W15-STRT-YMD REDEFINES W15-S-YMD.
	       05  W15-SCC     PIC 9(02).
004190         05  W15-SYY     PIC 9(02).
004200         05  W15-SMM     PIC 9(02).
004210         05  W15-SDD     PIC 9(02).
004120	   03  W15-END	       PIC 9(08).
004130     03  W15-EDTE-DMY REDEFINES W15-END.
004140         05  W15-EDAY    PIC 9(02).
004150         05  W15-EMNTH   PIC 9(02).
	       05  W15-ECENT   PIC 9(02).
004160         05  W15-EYEAR   PIC 9(02).
004170	   03  W15-E-YMD       PIC 9(08).
004180     03  W15-END-YMD REDEFINES W15-E-YMD.
	       05  W15-ECC     PIC 9(02).
004190         05  W15-EYY     PIC 9(02).
004200         05  W15-EMM     PIC 9(02).
004210         05  W15-EDD     PIC 9(02).

       COPY W20.WS.

002850 01  W25-CALCS.
002860	   03  W25-RESULT      PIC  9(05)V99.
002870     03  W25-RESULT1 REDEFINES W25-RESULT.
002880	       05  W25-WHOLE   PIC  9(05).
002890	       05  W25-DECIMAL PIC  9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
	       05  W25-KEY     PIC  9(04).
	       05  W25-SUB     PIC  9(01).
	       05  FILLER      PIC  9(02).
002900     03  W25-TOTAL       PIC S9(09)V99 COMP-3.
002910     03  W25-VALUE       PIC S9(09)V99 COMP-3.

       01  W35-HEADINGS.
	   02  W35-PERIOD.
	   03  FILLER	 PIC  X(35)
			 VALUE "Stock Adjustments for the period:- ".
	   03  W35-START PIC  Z9/99/9999.
	   03  FILLER	 PIC  X(04) VALUE " to ".
	   03  W35-END	 PIC  Z9/99/9999.
	   03  W35-REP	 PIC  X(18).
	   02  W35-REFERENCE.
	   03  FILLER	 PIC  X(31)
			 VALUE "Stock Adjustments - Reference: ".
	   03  W35-REF	 PIC  X(08).
	   02  W35-HEADING.
	   03  FILLER	 PIC  X(86)
	       VALUE "Item Code          Stock Description
      - 	     "  Quantity         Value  Remarks".

       01  BLOCK-DETAIL1.
	   03  ORIGINAL-VID1.
	       05  ORIGINAL-CHAR1 PIC  X(01) OCCURS 80.
	   03  REVERSE-VID1.
	       05  REVERSE-CHAR1  PIC  X(01) OCCURS 80.

       01  SHADOW-DETAIL1.
	   03  SHAD1-ROW       PIC  9(02) COMP-X.
	   03  SHAD1-COL       PIC  9(02) COMP-X.
	   03  SHAD1-LINES     PIC  9(02) COMP-X.
	   03  SHAD1-WIDTH     PIC  9(02) COMP-X.
	   03  SHAD1-CHAR      PIC  X(01).

       01  CRT-DETAIL1.
	   03  TOP-ROW1       PIC  9(02) COMP-X.
	   03  BOTTOM-ROW1     PIC  9(02) COMP-X.
	   03  STRING-LENGTH1  PIC  9(04) COMP-X.
	   03  SCREEN-POSITION1.
	       05  SCREEN-LIN1 PIC  9(02) COMP-X.
	       05  SCREEN-COL1 PIC  9(02) COMP-X.

       01  BLOCK-DETAIL2.
	   03  ORIGINAL-VID2.
	       05  ORIGINAL-CHAR2 PIC  X(01) OCCURS 80.
	   03  REVERSE-VID2.
	       05  REVERSE-CHAR2  PIC  X(01) OCCURS 80.

       01  SHADOW-DETAIL2.
	   03  SHAD2-ROW       PIC  9(02) COMP-X.
	   03  SHAD2-COL       PIC  9(02) COMP-X.
	   03  SHAD2-LINES     PIC  9(02) COMP-X.
	   03  SHAD2-WIDTH     PIC  9(02) COMP-X.
	   03  SHAD2-CHAR      PIC  X(01).

       01  CRT-DETAIL2.
	   03  TOP-ROW2       PIC  9(02) COMP-X.
	   03  BOTTOM-ROW2     PIC  9(02) COMP-X.
	   03  STRING-LENGTH2  PIC  9(04) COMP-X.
	   03  SCREEN-POSITION2.
	       05  SCREEN-LIN2 PIC  9(02) COMP-X.
	       05  SCREEN-COL2 PIC  9(02) COMP-X.

       COPY W40.WS.

       COPY FUNCTION.WS.

003530 COPY W50.WS.

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
	   03  W75-DATE       PIC 9(08) OCCURS 16.

003750 01  W75-STORE.
003760	   03  W75-SNAME      PIC  X(08).
	   03  W75-SREF       PIC  X(08).
	   03  W75-SACN       PIC  X(06).
	   03  W75-SDTE       PIC  9(06).

004390 01  W95-STM.
004400     03  W95-COMP        PIC X(40).
004410     03  W95-V1.
004420         05  W95-T       PIC Z(04)9.99.
004430     03  W95-V2.
004440         05  W95-B       PIC Z(06)9.99-.
004450     03  W95-D.
004460	       05  W95-DTE     PIC  Z9/99/9999.
004470	   03  W95-POST        PIC  9(04).
004480	   03  W95-TEL	       PIC  X(08).

004490 01  W100-EDIT.
004500	   03  W100-COST       PIC  Z(06)9.99-.
	   03  W100-QNT	       PIC  Z(06)9.99-.
	   03  W100-USER.
	       05  W100-USE    PIC  ZZ9.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004580 SCREEN SECTION.

       COPY BLANK.CRT.

       01  MENU-INSTRUCT.
	   03  LINE 25 COLUMN  2 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE "Use ".
	   03	       COLUMN  6 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "".
	   03	       COLUMN  7 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " or ".
	   03	       COLUMN 11 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "".
	   03	       COLUMN 12 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " to point to an item, and ".
	   03	       COLUMN 38 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "<Enter>".
	   03	       COLUMN 45 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " to Select ".
	   03	       COLUMN 56 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "<Esc>".
	   03	       COLUMN 61 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " to Exit".

       01  S01.
	   03  LINE  2 COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
009360		       "ADJUSTMENT TRACKING".

       01  S02.
	   03  LINE  4 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE
				"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 50 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "¿".
	   03  LINE  5 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE
				"³   LOOKUP OPTIONS   ".
	   03	       COLUMN 50 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE  6 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³ 1 ".
	   03	       COLUMN 33 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE
				"Specified Period ³".
	   03  LINE  7 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³ 2 ".
	   03	       COLUMN 33 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE
				"Individual Ref.  ³".
	   03  LINE  8 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³ 0 ".
	   03	       COLUMN 33 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE
				"Exit	          ³".
	   03  LINE  9 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "À".
	   03	       COLUMN 30 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE
				 "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S03.
	   03  LINE  8 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC X(08)
				 USING WS-REFERENCE AUTO.

009340 01  S05D.
	   03  LINE  4 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  5 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"³    Transactions for Period.    ³".
	   03  LINE  6 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE  7 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"³   Starting Date :              ³".
	   03  LINE  8 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"³   Ending Date   :              ³".
	   03  LINE  9 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE 10 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"³Dates to be keyed in as ".
	   03	       COLUMN 49 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE "DDMMCCYY".
	   03	       COLUMN 57 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE "³".
	   03  LINE 11 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S05A.
	   03  LINE  7 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W15-START AUTO.
	   03  LINE  8 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W15-END AUTO.

009340 01  S10D.
009350	   03  LINE  2 COLUMN 25 FOREGROUND-COLOR 15 VALUE
009360		       "STOCK ADJUSTMENT TRACKING LOOKUP".
	   03  LINE  3 COLUMN  3 VALUE "Item Code.".

007910 01  S36.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(14) USING W10-ITEM AUTO.

       01  S36-ITM.
005540	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				  PIC X(14) USING W10-ITEM AUTO.
	   03  COLUMN 16 FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
005550				  PIC X(03) USING W10-ITM AUTO.

005530 01  S36-EXT.
005540	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				  PIC X(18) USING W10-EXT-ITEM AUTO.

005530 01  S36-3.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(03) USING W10-I4 AUTO.

005530 01  S36-4.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(04) USING W10-I4 AUTO.

005530 01  S36-5.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(05) USING W10-I5 AUTO.

005530 01  S36-6.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(06) USING W10-I6 AUTO.

005530 01  S36-7.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(07) USING W10-I7 AUTO.

005530 01  S36-8.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(08) USING W10-I8 AUTO.

005530 01  S36-9.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(09) USING W10-I9 AUTO.

005530 01  S36-10.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(10) USING W10-I10 AUTO.

005530 01  S36-11.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(11) USING W10-I11 AUTO.

005530 01  S36-12.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(12) USING W10-I12 AUTO.

005530 01  S36-13.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(13) USING W10-I13 AUTO.

005530 01  S36-14.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(14) USING W10-I14 AUTO.

009340 01  S-MOV.
009390     03  LINE  4 COLUMN  2 REVERSE-VIDEO VALUE 
009400	       "   Date    User  W/S  Type    Cost Price     Quantity
      -        "       Remarks        ".
009160	   03  LINE  5 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009160	   03	       COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÑÍÍÍÑÍÍÍÍÍÍÑÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
009160	   03  LINE 22 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009170	   03	       COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÏÍÍÍÏÍÍÍÍÍÍÏÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".

004870 COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
010250 AA000-MAIN	       SECTION.
010260 AA000-INIT.
003240	   IF LS0-STLEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
010270	     PERFORM ZA000-INIT.
	     DISPLAY S01.
      *    *************************************************************
      *    ****       C H O O S E   L O O K U P   O P T I O N	    ****
      *    *************************************************************
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 31		 TO SHADE-COL.
	     MOVE 20		 TO SHADE-WIDTH.
	     MOVE 5		 TO SHADE-LINES.
	     DISPLAY S02.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 5		 TO TOP-ROW.
	     MOVE 7		 TO BOTTOM-ROW.
	     MOVE 4		 TO SCREEN-LIN.
	     MOVE 31		 TO SCREEN-COL.
	     MOVE 18		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       AA05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       AA10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO AA15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO AA10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO AA10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO AA10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO AA10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO AA15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO AA05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO AA18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO AA05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       IF WS-OPTION < 0 OR > 2
		   CALL X"E5"
		   GO TO AA05
	       END-IF
	       GO TO AA16
	   ELSE
	       CALL X"E5"
	       GO TO AA05.

       AA15.
	   IF SCREEN-LIN < 5
	       CALL X"E5"
	       GO TO AA05
	   ELSE
	   IF SCREEN-LIN = 5
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 6
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 7
	       MOVE "0"		 TO WS-OPTION.
	   GO TO AA18.

       AA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = 0
	       MOVE 7		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 1
	       MOVE 5		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = 2
	       MOVE 6		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       AA18.
	   IF WS-OPTION = "0"
	       GO TO AA49.
	     PERFORM SAVE-SCREEN-2.
	     MOVE BLOCK-DETAIL	 TO BLOCK-DETAIL2.
	     MOVE SHADOW-DETAIL	 TO SHADOW-DETAIL2.
	     MOVE CRT-DETAIL	 TO CRT-DETAIL2.
	   IF WS-OPTION = "1"
	       MOVE "D" 	 TO WS-REP
	       PERFORM BA000
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "R" 	 TO WS-REP
	       PERFORM BB000.
	     PERFORM RESTORE-SCREEN-2.
	     MOVE BLOCK-DETAIL2	 TO BLOCK-DETAIL.
	     MOVE SHADOW-DETAIL2 TO SHADOW-DETAIL.
	     MOVE CRT-DETAIL2	 TO CRT-DETAIL.
	     GO TO AA05.

       AA49.
013150       EXIT PROGRAM.

       COPY AA50.LUP.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

011390 COPY LOCKED.RC2.

011410 COPY CLEAR.CRT.

       COPY DATE.CHK.

       COPY AA300.STK.

       COPY ABORT.PRN.

       COPY PRINTUTL.AS3.

      /
005810 AB000              SECTION.
005110 AB05.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    1 O   C P I   ( N O R M A L   P R I N T )	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
005120	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "H"		 TO W02-PRN-TYPE.
005120	     MOVE SPACES	 TO REP-DETAIL1.
005130	     MOVE "Page:"	 TO REP-PAGE.
005140	     MOVE "Date:"	 TO REP-DATE.
005150	     MOVE W95-COMP	 TO REP-COMPANY.
005160       ADD 1               TO WS-PAGE.
005170	     MOVE WS-PAGE	 TO REP-P-NO.
005180	     MOVE W12-TODAY	 TO REP-DTE.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
005200	     MOVE SPACES	 TO REP-DETAIL1.

005210 AB10.
	   IF WS-REP = "D"
005220	       MOVE W35-PERIOD	 TO REP-HEAD
	   ELSE
	       MOVE W35-REFERENCE
				 TO REP-HEAD.
	     MOVE 2		 TO WS-ADVANCE.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
005240	     MOVE SPACES	 TO REP-DETAIL1.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    1 2   C P I   ( 9 6   Characters per line )	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "H"		 TO W02-PRN-TYPE.
	     MOVE 2		 TO WS-ADVANCE.
	     MOVE W35-HEADING	 TO REP-HEAD.
	     PERFORM CALL-PRINTUTL.
005240	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE 1		 TO WS-ADVANCE.
	     MOVE "D"		 TO W02-PRN-TYPE.

       AB20.
	     EXIT.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       R E A D	 F I L E S   R O U T I N E S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
011650 AC000		  SECTION.

011670 COPY CONTROL.RD.

000140 COPY DEPART.RD.

011750 COPY PARAM.RD.

000300 COPY STKDEX.RD.

       COPY STOCK.RD.

      /    *************************************************************
      *    ****    M O V E M E N T   L O O K U P
      *    *************************************************************
000010 MOVEMENT-LOOKUP		 SECTION.

000030 MOVE-LUP-SCREEN.
000050	     DISPLAY S-MOV.
	     INITIALIZE STX-REC1.
	     MOVE W10-EXT-ITEM	 TO STX-EXT-CODE.
	     MOVE W15-S-YMD	 TO STX-DTE.

       MOVE-LUP-CLR-SCREEN.
000060       MOVE 0601           TO CPOS.
000080	     PERFORM MOVE-CRT-EDGE UNTIL CLIN = 22.

       MOVE-LUP-CONTINUE.
000180	     MOVE 6		 TO CLIN.
000200       MOVE 1              TO W75-S W75-S1.

       MOVE-LUP-TAB-LOOP.
	     MOVE ZERO		 TO W75-DATE (W75-S1).
	   IF W75-S1 < 16
               ADD 1             TO W75-S1
	       GO TO MOVE-LUP-TAB-LOOP.
             MOVE ZERO           TO W75-S1.

       MOVE-LUP-START-STDX.
	     MOVE SPACES	 TO WS-OPTION.
000750	     PERFORM START-AT-STDX-KEY THRU READ-STKDEX-EXIT.
000880	   IF WS-F-ERROR = 46
	       GO TO MOVE-NO-RECORD.

000810 MOVE-GET-STKDEX-REC.
           IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
	       GO TO MOVE-SKIP-STDX-READ.
000840	     PERFORM READ-STKDEX-NEXT THRU READ-STKDEX-EXIT.
	   IF WS-F-ERROR = 46
               SUBTRACT 1        FROM W75-S
	       GO TO MOVE-FULL-SCREEN.

       MOVE-LUP-DISP-ITEM.
	   IF NOT (STX-EXT-CODE = W10-EXT-ITEM)
	       GO TO MOVE-FULL-SCREEN.
	   IF (STX-DTE > W15-E-YMD)
	       GO TO MOVE-FULL-SCREEN.

       MOVE-SKIP-STDX-READ.
	     MOVE STX-DTE	 TO W22-DTE2.
	     MOVE W22-CC2	 TO W22-CC3.
	     MOVE W22-YY2	 TO W22-YY3.
	     MOVE W22-MM2	 TO W22-MM3.
	     MOVE W22-DD2	 TO W22-DD3.
	     MOVE W22-DTE3	 TO W95-DTE.
000900	     DISPLAY (CLIN, 2) W95-DTE WITH FOREGROUND-COLOR 11.
	     MOVE STX-USER	 TO W100-USE.
000920	     DISPLAY ( , 14) W100-USER WITH FOREGROUND-COLOR 11.
000920	     DISPLAY ( , 19) STX-WS WITH FOREGROUND-COLOR 11.
000920	     DISPLAY ( , 23) STX-PROG WITH FOREGROUND-COLOR 11.
	     COMPUTE W100-COST ROUNDED = STX-COST / STX-QNT.
	     MOVE STX-QNT	 TO W100-QNT.
000960	     DISPLAY ( , 31) W100-COST WITH FOREGROUND-COLOR 11.
	     DISPLAY ( , 45) W100-QNT WITH FOREGROUND-COLOR 11.
	     DISPLAY ( , 59) STX-REMARKS WITH FOREGROUND-COLOR 11.
000970     IF CLIN < 21
000980         ADD 1             TO CLIN W75-S
000990	       GO TO MOVE-GET-STKDEX-REC.
001000
001010 MOVE-FULL-SCREEN.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
001020	     DISPLAY " " AT 2303 WITH FOREGROUND-COLOR 14
001030		     " - " WITH FOREGROUND-COLOR 7
		     "PgUp" WITH FOREGROUND-COLOR 14
                     "/" WITH FOREGROUND-COLOR 7
		     "PgDn" WITH FOREGROUND-COLOR 14
		     " - " WITH FOREGROUND-COLOR 7
001170		     "Esc" WITH FOREGROUND-COLOR 14
		     "ape." WITH FOREGROUND-COLOR 7.

       MOVE-SAVE-FULL-SCREEN.
	     PERFORM SAVE-SCREEN-2.
001140	     MOVE 0		 TO W75-S1.
001150       MOVE 0601           TO CPOS.

001180 MOVE-CRT-ATTRIB.
	     MOVE 1920		 TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
001220	     MOVE 2000		 TO W41-LENGTH.

001240 MOVE-GET-USER-OPT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     DISPLAY CLEAR-L25.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
001320		     GO TO MOVE-ADJ-NO-KEYS
		 WHEN DOWN-KEY
001340		     GO TO MOVE-CHK-NO-KEYS
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO MOVE-GET-USER-OPT
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO MOVEMENT-LOOKUP-EXIT
		 WHEN PAGE-DOWN
001340		     GO TO MOVE-GET-NEXT-PAGE
		 WHEN PAGE-UP
001340		     GO TO MOVE-GET-PREV-PAGE
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO MOVE-GET-USER-OPT.
	     CALL X"E5".
001280	     GO TO MOVE-GET-USER-OPT.

001240 MOVE-GET-NEXT-PAGE.
001480     IF W75-S < 16
001490	       GO TO MOVE-GET-USER-OPT.

       MOVE-NEW-PAGE.
001500       MOVE 0601           TO CPOS.
001520	     PERFORM MOVE-CRT-EDGE UNTIL CLIN = 22.
001530       MOVE 6              TO CLIN.
001540       MOVE LOW-VALUES     TO W75-KEYS.
001550       MOVE 1              TO W75-S W75-S1.
	     PERFORM MOVE-LUP-TAB-LOOP.
001560	     GO TO MOVE-GET-STKDEX-REC.

       MOVE-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *    *************************************************************
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *    *************************************************************
	      MOVE "J"		  TO WS-OPTION.

       MOVE-GET-PREV-LOOP.
	     PERFORM READ-STKDEX-PREV THRU READ-STKDEX-EXIT.
	   IF WS-F-ERROR = 46
	       MOVE SPACE	 TO WS-OPTION
	       GO TO MOVE-NEW-PAGE.
	   IF NOT (STX-EXT-CODE = W10-EXT-ITEM)
	       PERFORM READ-STKDEX-NEXT THRU READ-STKDEX-EXIT
	       GO TO MOVE-NEW-PAGE.
	   IF STX-DTE < W15-START
	       PERFORM READ-STKDEX-NEXT THRU READ-STKDEX-EXIT
	       GO TO MOVE-NEW-PAGE.
	   IF WS-IXS1 < 32
	       ADD 1		 TO WS-IXS1
	       GO TO MOVE-GET-PREV-LOOP.
	     GO TO MOVE-NEW-PAGE.

001580 MOVE-NO-RECORD.
	     DISPLAY CLEAR-L25.
001590	     DISPLAY "No record - Press " AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
001600		     "ANY" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
001610		     " key"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
001770	     GO TO MOVEMENT-LOOKUP-EXIT.

001790 MOVE-ADJ-NO-KEYS.
001800     IF W75-S1 > 1
001810         SUBTRACT 1        FROM W75-S1 CLIN.
001820	     GO TO MOVE-HIGHLIGHT.

001840 MOVE-CHK-NO-KEYS.
001850     IF W75-S1 = ZERO
001860         MOVE 1            TO W75-S1
001870	       GO TO MOVE-HIGHLIGHT.
001880     IF W75-S1 < W75-S
001890         ADD 1             TO W75-S1 CLIN.

001910 MOVE-HIGHLIGHT.
001920	     PERFORM MOVE-CRT-ATTRIB.
001930	     DISPLAY W43-DET(CLIN) AT CPOS WITH REVERSE-VIDEO.
001940	     GO TO MOVE-GET-USER-OPT.

002070 MOVE-CRT-EDGE.
002130	     DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR 0
				      FOREGROUND-COLOR 3
		     "          ³     ³   ³      ³             ³
      - 	     "     ³                      "
		      WITH FOREGROUND-COLOR 3
		     "³" WITH BACKGROUND-COLOR 0
			      FOREGROUND-COLOR 3.
002090	     ADD 1		 TO CLIN.

002110 MOVEMENT-LOOKUP-EXIT.
002120       EXIT.

       COPY APAC.HLP.

      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   C O D E
      *
015170 CA000         SECTION.
015180 CA00.
013310	     MOVE 18		 TO W50-LENGTH.

       CA05.
	   IF WS-EXT-STK = "Y"
013320	       MOVE W10-EXT-ITEM TO W50-DATA
	   ELSE
	       MOVE W10-ITEM	 TO W50-DATA
	       MOVE W10-ITM	 TO W50-ITM.
	   IF W50-1 = SPACE
	       IF WS-EXT-STK = "Y"
		   MOVE W50-17	 TO W10-EXT-ITEM
	       ELSE
		   MOVE W50-13	 TO W10-ITEM
	       END-IF
               GO TO CA05.      
	     CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
013380	     MOVE W50-EXT-ITEM TO W10-EXT-ITEM.

       CA06.
013380	     MOVE W50-EXT-ITEM	 TO STK-CODE.

       CA10.
	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF WS-STATUS = "23"
	       MOVE 22		 TO WS-F-ERROR.

       CA99.
             EXIT.

      /    *************************************************************
      *    * E N Q U I R Y   ( A D J U S T M E N T   T R A C K I N G ) *
      *    *************************************************************
021000 BA000		       SECTION 50.
021010 BA00.
021020       DISPLAY CLR-SCREEN.
	     DISPLAY S01.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 26		 TO SHADE-COL.
	     MOVE 32		 TO SHADE-WIDTH.
	     MOVE 7		 TO SHADE-LINES.
	     DISPLAY S05D.
	     PERFORM SCREEN-SHADOW.
	     MOVE W12-TODAY	 TO W15-START W15-END.

       BA05.
	     DISPLAY "Press " AT 2552
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "ape to Exit"
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
	     ACCEPT S05A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA999
		 WHEN OTHER	 CALL X"E5"
		     GO TO BA05
	       END-EVALUATE.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       GO TO BA05.
	     DISPLAY CLEAR-L25.
	     MOVE W15-START	 TO W10-EDTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "Start date" TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA05.
             MOVE W20-DTE        TO W15-S-YMD.
	     MOVE W15-END	 TO W10-EDTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "End date"	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA05.
             MOVE W20-DTE        TO W15-E-YMD.
	   IF W15-E-YMD < W15-S-YMD
	       MOVE "End date is before start date"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA05.
	     MOVE W15-START	 TO W35-START.
	     MOVE W15-END	 TO W35-END.
	     DISPLAY "The Print format will only include transactions th
      - 	     "at were processed in the" AT 1804
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4.
	     DISPLAY "Stock Adjust Quantities (Option 1/1/2/8). The Disp
      - 	     "lay and Report formats  " AT 1904
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4.
	     DISPLAY "include all stock adjustments that change quantiti
      - 	     "es                      " AT 2004
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4.

       BA10.
	     MOVE "'P'rint, 'D'isplay or 'R'eport format  [D]"
				 TO WS-ERR-MES.
	     MOVE "D"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
020250	   IF NOT (WS-OPTION = "D" OR "P" OR "R")
	       GO TO BA10.
	     MOVE WS-OPTION	 TO WS-ENQ.
	   IF WS-ENQ = "P" OR "R"
021020	       DISPLAY CLR-SCREEN
	       DISPLAY S10D
	       GO TO BA55.

       BA15.
	     MOVE SPACES	 TO W10-EXT-ITEM.
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

021010 BA20.
021020       DISPLAY CLR-SCREEN.
	     DISPLAY WS-BLNK78 AT 0302
		     WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1.
	     DISPLAY S10D.
	   IF WS-USE-ITM = "Y"
	       DISPLAY "/" AT 0328
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.

       BA25.
	     MOVE WS-OPTION	 TO WS-ENQ.

       BA30.
020960	     PERFORM AA300.
      *    *************************************************************
      *    ****    G E T   S T O C K   C O D E
      *    *************************************************************
	   IF WS-EXT-STK = "Y"
	       ACCEPT S36-EXT AT 0314
	   ELSE
      *    *************************************************************
      *        ***** ALPHA-NUMERIC CODE *****
      *    *************************************************************
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   ACCEPT S36-ITM AT 0314
	       ELSE
021070		   ACCEPT S36 AT 0314
	       END-IF
	   ELSE
	   IF W10-SLNGTH = 3
	       ACCEPT S36-3 AT 0314
	   ELSE
	   IF W10-SLNGTH = 4
	       ACCEPT S36-4 AT 0314
	   ELSE
	   IF W10-SLNGTH = 5
	       ACCEPT S36-5 AT 0314
	   ELSE
	   IF W10-SLNGTH = 6
	       ACCEPT S36-6 AT 0314
	   ELSE
	   IF W10-SLNGTH = 7
	       ACCEPT S36-7 AT 0314
	   ELSE
	   IF W10-SLNGTH = 8
	       ACCEPT S36-8 AT 0314
	   ELSE
	   IF W10-SLNGTH = 9
	       ACCEPT S36-9 AT 0314
	   ELSE
	   IF W10-SLNGTH = 10
	       ACCEPT S36-10 AT 0314
	   ELSE
	   IF W10-SLNGTH = 11
	       ACCEPT S36-11 AT 0314
	   ELSE
	   IF W10-SLNGTH = 12
	       ACCEPT S36-12 AT 0314
	   ELSE
	   IF W10-SLNGTH = 13
	       ACCEPT S36-13 AT 0314
	   ELSE
	   IF W10-SLNGTH = 14
	       ACCEPT S36-14 AT 0314.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
018870		     DISPLAY CLEAR-L25
		     GO TO BA999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
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
		 WHEN F9-KEY
		     MOVE "W"	 TO W10-ETYPE
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BA30
	       END-EVALUATE
016560	       PERFORM AA50-LOOKUP
      *    *************************************************************
      *        *****   E X T E N D E D	 C O D E   *****
      *    *************************************************************
	       IF WS-EXT-STK = "Y"
		   DISPLAY S36-EXT AT 0314
	       ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
	       IF W10-STCK = "A"
		   IF WS-USE-ITM = "Y"
		       DISPLAY S36-ITM AT 0314
		   ELSE
021070		       DISPLAY S36 AT 0314.
	     DISPLAY CLEAR-L25.

       BA35.
021040	   IF W10-EXT-ITEM = SPACES
021060	       GO TO BA30.

       BA40.
021070       PERFORM CA000.
021080     IF WS-F-ERROR = 22
	       MOVE "Z" 	 TO W10-ETYPE
018620	       PERFORM AA50-LOOKUP
      *    *************************************************************
      *        *****   E X T E N D E D	 C O D E   *****
      *    *************************************************************
	       IF WS-EXT-STK = "Y"
		   DISPLAY S36-EXT AT 0314
	       ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
	       IF W10-STCK = "A"
		   IF WS-USE-ITM = "Y"
		       DISPLAY S36-ITM AT 0314
		   ELSE
021070		       DISPLAY S36 AT 0314
		   END-IF
	       END-IF
018640	       IF W10-EXT-ITEM = SPACES
018720		   GO TO BA30
               ELSE
		   GO TO BA40.
	     DISPLAY "Description" AT 0333.
	     DISPLAY STK-DESC AT 0345
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 3 HIGHLIGHT.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       GO TO BA30.

       BA45.
	     PERFORM MOVEMENT-LOOKUP.
	     MOVE SPACE 	 TO WS-OPTION.

       BA50.
	     MOVE "Change 'D'ates,'I'tem code or 'E'nd  [ ]"
				 TO WS-ERR-MES.
033910	     PERFORM OPT-MESSAGE.
014920	   IF NOT (WS-OPTION = "D" OR "E" OR "I")
014930	       GO TO BA50.
	   IF WS-OPTION = "I"
	       GO TO BA20
	   ELSE
	   IF WS-OPTION = "D"
	       GO TO BA00.
	     GO TO BA999.

       BA55.
	     MOVE ZERO		 TO WS-PAGE.
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.

       BA60.
	     MOVE "  !" 	 TO STX-EXT-CODE.
	     MOVE W15-S-YMD	 TO STX-DTE.
	     MOVE ZERO		 TO STX-TIME.
	     PERFORM START-AT-STDX-DTE-TME THRU READ-STKDEX-EXIT.
      *
      *      Before time was included in file
      *
      *      PERFORM START-AT-STDX-DATE THRU READ-STKDEX-EXIT.
000880	   IF WS-F-ERROR = 46
	       GO TO BA75.
	     MOVE ZERO		 TO WS-QTOT WS-VTOT.
	     PERFORM AB000.
	     DISPLAY "Processing Record:" AT 1212.

       BA65.
000840	     PERFORM READ-STKDEX-NEXT THRU READ-STKDEX-EXIT.
000880	   IF WS-F-ERROR = 46
	       GO TO BA70.
	   IF (STX-DTE > W15-E-YMD)
	       GO TO BA70.
	   IF WS-ENQ = "P"
	       IF NOT (STX-PROG = "STKADJ")
		   GO TO BA65.
	     DISPLAY STX-EXT-CODE AT 1231
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT.
	     MOVE STX-EXT-CODE	 TO REP-ITEM STK-CODE.
	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	     MOVE STK-DESC	 TO REP-DESC.
	     MOVE STX-QNT	 TO REP-QUANT.
	     MOVE STX-COST	 TO REP-COST.
	     MOVE STX-REMARKS	 TO REP-REMK.
	     ADD STX-QNT	 TO WS-QTOT.
	     ADD STX-COST	 TO WS-VTOT.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009740	       PERFORM AB000.
009750	     MOVE SPACES	 TO REP-DETAIL1.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
	       MOVE 0		 TO WS-ADVANCE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    1 O   C P I   ( N O R M A L   P R I N T )	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
	       MOVE 1		 TO WS-ADVANCE
010010	       MOVE SPACES	 TO REP-DETAIL1
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P" 	 TO WS-COMMAND
	       GO TO BA999.
	     GO TO BA65.

       BA70.
	     MOVE ALL "Ä"	 TO REP-TOT-LINE.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
009980	     MOVE SPACES	 TO REP-DETAIL1.
009940	     MOVE "**** TOTAL UNITS ****"
				 TO REP-DESC.
009950	     MOVE WS-QTOT	 TO REP-QUANT.
009960	     MOVE WS-VTOT	 TO REP-COST.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     GO TO BA80.

       BA75.
	     MOVE "No details for period"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.

       BA80.
009980	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

       BA85.
	     MOVE "Change 'D'ates or 'E'nd  [ ]"
				 TO WS-ERR-MES.
033910	     PERFORM OPT-MESSAGE.
014920	   IF NOT (WS-OPTION = "D" OR "E")
014930	       GO TO BA85.
	   IF WS-OPTION = "D"
	       GO TO BA00.

021720 BA999.
021730       EXIT.
      /    *************************************************************
      *    * E N Q U I R Y   ( A D J U S T M E N T   T R A C K I N G ) *
      *    *************************************************************
021000 BB000		       SECTION 51.
021010 BB00.
021020	     DISPLAY CLR-SCREEN.
	     DISPLAY S01.
	     MOVE 8		 TO SHADE-ROW.
	     MOVE 29		 TO SHADE-COL.
	     MOVE 25		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY "Ú" AT 0727
		      WITH FOREGROUND-COLOR 5 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "³" AT 0827
		      WITH FOREGROUND-COLOR 5 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     " Reference Code          "
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0927
		      WITH FOREGROUND-COLOR 5 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "Ù" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     PERFORM SCREEN-SHADOW.
	     MOVE SPACES	 TO WS-REFERENCE.

       BB05.
	     ACCEPT S03.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BB999
		 WHEN OTHER	 CALL X"E5"
		     GO TO BB05
	       END-EVALUATE.
	   IF WS-REFERENCE = SPACES
	       GO TO BB999.

       BB10.
	     MOVE "'C'ontinue or 'R'ekey Code  [C]"
				 TO WS-ERR-MES.
	     MOVE "C"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
020250	   IF NOT(WS-OPTION = "C" OR "R")
	       GO TO BB10.
	   IF WS-OPTION = "R"
	       GO TO BB05.
	     MOVE WS-REFERENCE	 TO STX-GRN.
	     MOVE ZERO		 TO STX-DTE STX-TIME.
	     PERFORM START-AT-STDX-GRN THRU READ-STKDEX-EXIT.
	   IF WS-F-ERROR = 46
	       GO TO BB75.
000840	     PERFORM READ-STKDEX-NEXT THRU READ-STKDEX-EXIT.
	   IF NOT(STX-GRN = WS-REFERENCE)
	       GO TO BB75.

       BB12.
	     MOVE STX-DTE	 TO W15-S-YMD.
	     MOVE W15-SCC	 TO W15-SDAY.
004190	     MOVE W15-SYY	 TO W15-SMNTH.
004200	     MOVE W15-SMM	 TO W15-SCENT.
004210	     MOVE W15-SDD	 TO W15-SYEAR.

       BB14.
	     PERFORM START-AT-STDX-GRN THRU READ-STKDEX-EXIT.
	     MOVE WS-REFERENCE	 TO W35-REF.
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	     MOVE ZERO		 TO WS-PAGE.
	     MOVE "C"		 TO W02-PRN-STATUS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	  Open the printer file - Allow for Screen option.     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE "O"		 TO WS-COMMAND.
	     MOVE "P"		 TO WS-PRINTER.
	     MOVE ZERO		 TO WS-ADVANCE
				    WS-REPORT
				    WS-SCREEN.
	     MOVE 3		 TO WS-REPORT.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
	     MOVE ZERO		 TO WS-QTOT WS-VTOT.
	     PERFORM AB000.
	     MOVE "R.Dte"	 TO REP-DATE.
	     MOVE W15-START	 TO REP-DTE.
	     DISPLAY "Processing Record:" AT 1212.

       BB15.
000840	     PERFORM READ-STKDEX-NEXT THRU READ-STKDEX-EXIT.
000880	   IF WS-F-ERROR = 46
	       GO TO BB20.
	   IF NOT(STX-GRN = WS-REFERENCE)
	       GO TO BB20.
	   IF NOT(STX-DTE = W15-S-YMD)
	       PERFORM BB20
	       IF W02-LINAGE < (W02-PRN-LENGTH - 6)
		   MOVE 1	 TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL
009780	       ELSE
		   MOVE 99	 TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL
009740		   PERFORM AB000.

	   IF NOT(WS-PRINTER = "S")
	       DISPLAY STX-EXT-CODE AT 1231
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT.
	     MOVE STX-EXT-CODE	 TO REP-ITEM STK-CODE.
	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	     MOVE STK-DESC	 TO REP-DESC.
	     MOVE STX-QNT	 TO REP-QUANT.
	     MOVE STX-COST	 TO REP-COST.
	     MOVE STX-REMARKS	 TO REP-REMK.
	     ADD STX-QNT	 TO WS-QTOT.
	     ADD STX-COST	 TO WS-VTOT.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009740	       PERFORM AB000.
009750	     MOVE SPACES	 TO REP-DETAIL1.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
	       MOVE 0		 TO WS-ADVANCE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    1 O   C P I   ( N O R M A L   P R I N T )	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
	       MOVE 1		 TO WS-ADVANCE
010010	       MOVE SPACES	 TO REP-DETAIL1
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P" 	 TO WS-COMMAND
	       GO TO BB999.
	     GO TO BB15.

       BB20.
	     MOVE ALL "Ä"	 TO REP-TOT-LINE.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
009980	     MOVE SPACES	 TO REP-DETAIL1.
009940	     MOVE "**** TOTAL UNITS ****"
				 TO REP-DESC.
009950	     MOVE WS-QTOT	 TO REP-QUANT.
009960	     MOVE WS-VTOT	 TO REP-COST.

       BB25.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     GO TO BB80.

       BB75.
	     MOVE "No details for Reference Code"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.
	     GO TO BB05.

       BB80.
009980	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

       BB85.
	     MOVE "'N'ext reference or 'E'nd  [ ]"
				 TO WS-ERR-MES.
033910	     PERFORM OPT-MESSAGE.
014920	   IF NOT(WS-OPTION = "N" OR "E")
014930	       GO TO BB85.
	   IF WS-OPTION = "N"
	       GO TO BB00.

       BB999.
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
				 TO TODAY-DDMMYY W12-TODAY.
	     MOVE LS-USUB	 TO WS-USUB.
042000	     DISPLAY CLR-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   S e t   u p	 t h e	 F U N C T I O N   k e y s     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		       ESC, F1 to F10 keys		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		    PAGE-UP and PAGE-DOWN keys		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   A C T I V A T E   M O U S E		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       Set MOUSE key to act as FUNCTION key	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    Setup the file ID's including path names           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *    *************************************************************
      *    **** 	  P A R A M E T E R   F I L E		    ****
      *    *************************************************************
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
      *    *************************************************************
      *    **** 	   D E P A R T M E N T	 F I L E	    ****
      *    *************************************************************
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "STKDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKDEX.
      *    *************************************************************
      *    **** 	       S T O C K   F I L E		    ****
      *    *************************************************************
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
042080	     MOVE 3		 TO WS-PARKEY.
042090       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF NOT (PAR-STOCK = "A" OR "N")
	       MOVE "A" 	 TO PAR-STOCK.
	     MOVE PAR-STOCK	 TO W10-STCK.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.
	   IF PAR-STOCK = "N"
	       MOVE PAR-SLNGTH	 TO W10-SLNGTH
	   ELSE
	       MOVE 14		 TO W10-SLNGTH.
	   IF W10-SLNGTH < 3
	       MOVE 3		 TO W10-SLNGTH
	   ELSE
	   IF W10-SLNGTH > 14
	       MOVE 14		 TO W10-SLNGTH.
	   IF LS-USER NUMERIC
	       IF LS-USE > ZERO AND < 110
		   MOVE LS-USE	 TO WS-USUB
                   GO TO ZA01A.
             MOVE 110            TO WS-USUB.

       ZA01A.
014240       ADD 2549 WS-USUB    GIVING W25-RESULT.
             MOVE W25-KEY        TO WS-PARKEY.
             ADD 1 W25-SUB       GIVING WS-S6.
014250       PERFORM READ-PARAM THRU READ-PARAM-EXIT.

042170 ZA02.
042720	     MOVE 1		 TO WS-PARKEY.
042730       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042740       MOVE PAR-COMPANY    TO W95-COMP.
042750       MOVE PAR-DMY        TO W12-TODAY.
042760       MOVE PAR-YMD        TO W12-T-YMD.
044780	     GO TO ZA999-EXIT.

039780 ZA49.
039790       DISPLAY "Too many files OPEN" AT 0812
039800                WITH FOREGROUND-COLOR 14.
039810       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
039820       DISPLAY "Press any key to continue" AT 1212.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
039840	     GO TO ZA205.
039850 ZA50.
041850	   IF WS-F-ERROR = 46
041860	       MOVE "ADJUSTMENT file"
				 TO WS-FILE.
039980       DISPLAY "Rebuild " AT 0812
039990                WS-FILE WITH FOREGROUND-COLOR 14.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040020 ZA51.
040030       EXIT PROGRAM.

       ZA55.
041220       MOVE 1              TO WS-S1.
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
021970	     MOVE WS-G4 	 TO WS-BCHR (8)  WS-BCHR (49)
021980                              WS-BCHR (61) WS-BCHR (72).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
042800 ZA200.
042810       DISPLAY "Files locked - Try later" AT 2312
042820                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
042830       DISPLAY "Press any key" AT 2512 
042840		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
042850	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
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
041630	       MOVE WS-NETKEY TO WS-KEY
056420	   ELSE
056430     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870         MOVE WS-PARKEY TO WS-KEY
056620	   ELSE
056630     IF WS-F-ERROR = 22
056640         MOVE W02-STOCKF   TO WS-FILE
056650         MOVE ZERO         TO WS-KEY
	       MOVE STK-CODE	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 46
041860	       MOVE W02-STKDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE STX-KEY	 TO WS-KEYX.
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
