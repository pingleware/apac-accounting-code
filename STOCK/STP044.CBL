      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	  ****	      ***	***    *
      *   **	**     **     **   **	 **  **      ****      ****    *
      *   **	       **     **   **	**    **    ** **     ** **    *
      *    ******      **     ******	**    **   **  **    **  **    *
      * 	**     **     **	**    **  ********  ********   *
      *   **	**     **     **	 **  **        **	 **    *
      *    ******      **     **	  ****	      ****	****   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *	  S T O C K   R E P O R T   -	S T O C K   T A K E   L I S T  *
      *                                                                *
      *     SPECIAL SPARES VERSION 1989 - WEEKLY TOTALS & X-REF        *
      * 							       *
      *     VERSION 8.13.09 - November 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STP044.
000030 AUTHOR.	       J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   JANUARY 1983.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2009
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

       COPY CONTROL.SL.

       COPY DEPART.SL.

000120 COPY PARAM.SL.

000180 COPY SPARTS.SL.

000140 COPY STOCK.SL.

      /
000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

       COPY DEPART.FDE.

000290 COPY PARAM.FDE.

000350 COPY SPARTS.FDE.

000310 COPY STOCK.FDE.

      /
003090 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
003100 77  WS-S1	   PIC	9(04)	 COMP-5.
003110 77  WS-S2	   PIC	9(04)	 COMP-5.
003120 77  WS-S3	   PIC	9(04)	 COMP-5.
003130 77  WS-S4	   PIC	9(04)	 COMP-5.
003140 77  WS-S5	   PIC	9(04)	 COMP-5.
003140 77  WS-S6	   PIC	9(04)	 COMP-5.
003140 77  WS-S7	   PIC	9(04)	 COMP-5.
003140 77  WS-S8	   PIC	9(04)	 COMP-5.
003150 77  WS-IXS	   PIC	9(04)	 COMP-5.
003150 77  WS-IXS1	   PIC	9(04)	 COMP-5.
003160 77  WS-IXS2	   PIC	9(04)	 COMP-5.
003170 77  WS-IXS3	   PIC	9(04)	 COMP-5.
003180 77  WS-IXS4	   PIC	9(04)	 COMP-5.
003210 77  WS-STKKEY	   PIC	9(04)	 COMP-5.
003220 77  WS-SPAKEY	   PIC	9(04)	 COMP-5.
003230 77  WS-PARKEY	   PIC	9(04)	 COMP-5.
003250 77  WS-NETKEY	   PIC	9(04)	 COMP-5.
003150 77  WS-PRN-NO	   PIC	X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC	X(01)	 COMP-X.
003270 77  WS-PAGE	   PIC	9(04)	 COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
003300 77  WS-PQNT	   PIC	X(01).
003310 77  WS-WS	   PIC	X(01) VALUE "N".
003310 77  WS-MEMO	   PIC	X(01) VALUE "N".
003310 77  WS-PRC	   PIC	X(01).
003310 77  WS-PRT	   PIC	X(01).
003320 77  WS-BIN	   PIC	X(01).
       77  WS-ETYPE	   PIC	X(01).
003330 77  WS-OPTION	   PIC	X(01).
003330 77  WS-PRICE	   PIC	X(01).
002180 77  WS-USE-CASES    PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
       77  WS-NUM	   PIC	ZZ9.
003350 77  WS-ERROR	   PIC	9(01).
003470 77  WS-ORD	   PIC S9(05)	 COMP-3.
003480 77  WS-TOTAL	   PIC S9(07)V99 COMP-3.
003490 77  WS-VALUE	   PIC S9(07)V99 COMP-3.
003500 77  WS-CCOST	   PIC S9(07)V99 COMP-3.
003510 77  WS-CSELL	   PIC S9(07)V99 COMP-3.
003520 77  WS-SCOST	   PIC S9(07)V99 COMP-3.
003530 77  WS-SSELL	   PIC S9(07)V99 COMP-3.
003540 77  WS-TCOST	   PIC S9(07)V99 COMP-3.
003550 77  WS-TSELL	   PIC S9(07)V99 COMP-3.
003560 77  WS-DEPT	   PIC	X(04).
001330 77  WS-DESC-H1	   PIC	X(14) VALUE "Description".
       77  WS-DESC-H2	   PIC	X(14) VALUE "Description 2".
       77  WS-RT-HD	   PIC	X(10) VALUE "-Retail   ".
       77  WS-WS-HD	   PIC	X(10) VALUE "-Wholesale".
       77  WS-CS-HD	   PIC	X(10) VALUE "-Cash sale".
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

003590 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003610 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

000360 COPY SPARTS.ID.

000300 COPY STOCK.ID.

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
000590	   03  REP-LINE6 REDEFINES W02-PRN-LINE.
000600	       05  REP-HEAD2   PIC  X(10).
000610	       05  REP-CNO     PIC  9(04).
000620	       05  REP-CATNO REDEFINES REP-CNO PIC X(04).
000630	       05  FILLER      PIC  X(01).
000640	       05  REP-CAT     PIC  X(30).
000650	       05  FILLER      PIC  X(35).
000850	   03  REP-LINE9 REDEFINES W02-PRN-LINE.
000860	       05  REP-PG      PIC  X(06).
000870	       05  REP-P-N     PIC  Z(03)9.
000880	       05  FILLER      PIC  X(22).
000890	       05  REP-COMP    PIC  X(40).
000900	       05  REP-REPORT  PIC  X(34).
000910	       05  FILLER      PIC  X(12).
000920	       05  REP-DAT     PIC  X(06).
000930	       05  REP-DT      PIC  Z9/99/99.
001080	   03  REP-LINE9B REDEFINES W02-PRN-LINE.
001090	       05  S-H1        PIC  X(19).
001100	       05  S-H2        PIC  X(30).
001110	       05  S-L1        PIC  X(01).
001120	       05  S-H3        PIC  X(06).
001130	       05  S-L2        PIC  X(01).
001140	       05  S-H4        PIC  X(08).
001150	       05  S-L3        PIC  X(01).
001160	       05  S-H5        PIC  X(09).
001170	       05  S-L4        PIC  X(01).
001180	       05  S-H6        PIC  X(09).
001190	       05  S-L5        PIC  X(01).
001200	       05  S-H7        PIC  X(09).
001210	       05  S-L6        PIC  X(01).
001220	   03  REP-LINE9C REDEFINES W02-PRN-LINE.
	       05  S-EXT-CDE.
001230		   07  S-CDE   PIC  X(14).
		   07  S-SL    PIC  X(01).
		   07  S-ITM   PIC  X(03).
001240	       05  FILLER      PIC  X(01).
001250	       05  S-DSC       PIC  X(30).
001260	       05  FILLER      PIC  X(01).
001270	       05  S-BN        PIC  X(06).
001280	       05  FILLER      PIC  X(01).
001290	       05  S-QNT       PIC  Z(05)9.9.
	       05  S-QNTN REDEFINES S-QNT
			       PIC  -(05)9.9.
001300	       05  FILLER      PIC  X(31).
001570	   03  REP-LINE11 REDEFINES W02-PRN-LINE.
001580	       05  FILLER      PIC  X(13).
001590	       05  REP-HEAD13  PIC  X(26).
001600	       05  FILLER      PIC  X(02).
001610	       05  REP-DCOST   PIC  Z(08)9.99.
001620	       05  FILLER      PIC  X(02).
001630	       05  REP-DSELL   PIC  Z(08)9.99.
001640	       05  FILLER      PIC  X(13).

003740 01  W05.
003750     03  W05-S OCCURS 300   PIC 9(04)     COMP-5.

       COPY W05.VAT.

       COPY W10.STK.

       COPY W12.WS.

       01  W15-BINS.
	   03  W15-START       PIC  X(05).
	   03  W15-END	       PIC  X(05).
	   03  W15-BIN	       PIC  X(01).

004310 01  W25-CALCS.
005040     03  W25-RESULT      PIC 9(05)V99.
004330     03  W25-RESULT1 REDEFINES W25-RESULT.
004340         05  W25-WHOLE   PIC 9(05).
004350         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
003790     03  W25-VALUE          PIC S9(07)V9(04) COMP-3.
003800     03  W25-TOTAL          PIC S9(07)V9(04) COMP-3.
003810 01  W30-COST-CODE.
003820     03  W30-CODE           PIC X(01) OCCURS 10.
003830 01  W35-COST.
003840     03  W35-CST            PIC 9(08)V99.
003850     03  W35-WHOLE  REDEFINES W35-CST
                                  PIC 9(10).
003860     03  W35-CPRINT.
003870         05  W35-CEDIT      PIC ZZZZZZ9999.
003880     03  W35-EOC REDEFINES W35-CPRINT.
003890         05  W35-CHAR       PIC X(01) OCCURS 10.
003900     03  W35-EON REDEFINES W35-CPRINT.
003910         05  W35-NUM        PIC 9(01) OCCURS 10.
           03  W35-CPRN REDEFINES W35-CPRINT.
               05  FILLER         PIC  X(01).
               05  W35-CSTPRN     PIC  X(09).
003920     03  W35-S1             PIC 9(02) COMP-5.
003930     03  W35-S2             PIC 9(02) COMP-5.

       01  W40-DATES.
	   03  W40-DATE1	  PIC  9(08).
           03  W40-DTE1 REDEFINES W40-DATE1.
	       05  W40-C1	  PIC  9(02).
	       05  W40-Y1	  PIC  9(02).
	       05  W40-M1	  PIC  9(02).
	       05  W40-D1	  PIC  9(02).
	   03  W40-DATE2	  PIC  9(08).
           03  W40-DTE2 REDEFINES W40-DATE2.
	       05  W40-C2	  PIC  9(02).
	       05  W40-Y2	  PIC  9(02).
	       05  W40-M2	  PIC  9(02).
	       05  W40-D2	  PIC  9(02).

       COPY W40.WS.

       COPY FUNCTION.WS.

003950 COPY W50.WS.

004040 COPY W70STOCK.WS.

004060 01  W70-KEYS.
004070     03  W70-BIND        PIC 9(05)     COMP-0 VALUE ZERO.
004080 01  W95-COMP	       PIC X(40).
005120 01  W100-EDIT.
003970     03  W100-V2.
003980         05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004090 SCREEN SECTION.

       COPY BLANK.CRT.

004440 01  S03.
004450	   03  LINE 18 COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
004460				 PIC X(14) FROM STK-ITEM.

004440 01  S03-ITM.
004450	   03  LINE 18 COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
004460				 PIC X(14) FROM STK-ITEM.
	   03	       COLUMN 40 FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
				 VALUE "/".
	   03	       COLUMN 41 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(03) FROM STK-ITM.

004440 01  S03-EXT.
004450	   03  LINE 18 COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
004460				 PIC X(18) FROM STK-CODE.

004470 01  S04.
004480	   03  LINE 10 COLUMN 20 FOREGROUND-COLOR 14
004490                           VALUE "Busy loading stock departments".

009340 01  S05B.
	   03  LINE  4 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  5 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"³  Stock Take per Bin Location.  ³".
	   03  LINE  6 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE  7 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"³   Starting Bin Code:           ³".
	   03  LINE  8 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"³   Ending Bin Code:             ³".
	   03  LINE  9 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S05A.
	   03  LINE  7 COLUMN 47 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(05) USING W15-START AUTO.
	   03  LINE  8 COLUMN 47 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(05) USING W15-END AUTO.

004690 01  S10A.
           03  LINE  2 COLUMN 32 FOREGROUND-COLOR 15
                                 VALUE "STOCK-TAKE REPORT".
004700     03  LINE  4 COLUMN 12 FOREGROUND-COLOR 14 VALUE
004710                           "Busy printing Stock-take report".
004720	   03  LINE  6 COLUMN 12 VALUE "Are you sure (".
	   03	       COLUMN 26 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Y".
	   03	       COLUMN 27 VALUE "/".
	   03	       COLUMN 28 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "N".
	   03	       COLUMN 29 VALUE ")".

       01  S10B.
	   03  LINE  8 COLUMN 12 VALUE "Include Priced Items (".
	   03	       COLUMN 34 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "Y".
	   03	       COLUMN 35 VALUE "/".
	   03	       COLUMN 36 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "N".
	   03	       COLUMN 37 VALUE ")".

       01  S11A.
004730	   03  LINE 10 COLUMN 12 VALUE "Print quantity (".
	   03	       COLUMN 28 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "Y".
	   03	       COLUMN 29 VALUE "/".
	   03	       COLUMN 30 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "N".
	   03	       COLUMN 31 VALUE ")".

       01  S11B.
004740	   03  LINE 12 COLUMN 12 VALUE "Bin sequence (".
	   03	       COLUMN 26 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "Y".
	   03	       COLUMN 27 VALUE "/".
	   03	       COLUMN 28 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "N".
	   03	       COLUMN 29 VALUE ")".

       01  S11C.
004750	   03  LINE 14 COLUMN 12 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "A".
	   03	       COLUMN 13 VALUE "ll or ".
	   03	       COLUMN 19 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "S".
	   03	       COLUMN 20 VALUE "eletive departments".

       01  S11D.
004770	   03  LINE 16 COLUMN 12 VALUE "Department code".

005040 01  S15.
005050	   03  LINE 18 COLUMN 12 VALUE "Stock record:".

       COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
005070 AA00M		 SECTION.
005080 AA00.
003240	   IF LS0-STLEV < 2
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
005090	     PERFORM ZA000-INIT.
	     PERFORM BE000.
	   IF WS-ERROR = 9
	       MOVE "No details in Department file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.
	     PERFORM DE00-REPORT.
      *
      *    ****   1 O	C P I	( N O R M A L	P R I N T )
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
010010	       MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

       AA49.
             EXIT PROGRAM.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP3.

      /
      *    ****    READ FILES ROUTINES
      *
008180 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY DEPART.RD.

       COPY PARAM.RD.

       COPY STOCK.RD.

       COPY SPARTS.RD.

       COPY STOCK.1ST.

      /
008500 BE000		SECTION.
008510 BE00.
	     MOVE "!"		 TO DPT-CODE.
	     PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       MOVE 9		 TO WS-ERROR
	   ELSE
	       MOVE ZERO	 TO WS-ERROR.

       BE09.
	     EXIT.

      /
013920 DE00-REPORT     SECTION 5.
013930 DE00.
013940       DISPLAY CLR-SCREEN.
013960       DISPLAY S10A.
013980	     MOVE "Y"		 TO WS-OPTION.
013990 DE02.
014000	     ACCEPT WS-OPTION AT 0632
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
014010			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
014070	   IF NOT (WS-OPTION = "Y" OR "N")
014080         GO TO DE02.
014090     IF WS-OPTION = "N"
014100	       GO TO DE999.
	     DISPLAY S10B.
	     MOVE "N"		 TO WS-PRC.

       DE03.
	     ACCEPT WS-PRC AT 0840
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO
	     CALL "CBL_TOUPPER" USING WS-PRC
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
014200	   IF NOT (WS-PRC = "N" OR "Y")
014210	       GO TO DE03.
013960       DISPLAY S11A.
014110	     MOVE "N"		 TO WS-PQNT.

014120 DE04.
014130	     ACCEPT WS-PQNT AT 1034
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-PQNT
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
014200	   IF NOT (WS-PQNT = "N" OR "Y")
014210         GO TO DE04.
013960       DISPLAY S11B.
014220	     MOVE "N"		 TO WS-BIN.

       DE05.
014230	     ACCEPT WS-BIN AT 1232
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-BIN
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-BIN = "N" OR "Y")
               GO TO DE05.
014300     IF WS-BIN = "Y"
013970	       DISPLAY S15
014320         GO TO DE108.
014330	     MOVE "S"		 TO WS-PRT.
013960       DISPLAY S11C.

014340 DE06.
014350	     ACCEPT WS-PRT AT 1440
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-PRT
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
014420	   IF NOT (WS-PRT = "S" OR "A")
014430         GO TO DE06.
014440     IF WS-PRT = "A"
014450	       MOVE SPACES	 TO WS-DEPT
014470         GO TO DE10.
013960       DISPLAY S11D.

014480 DE08.
014490	     ACCEPT WS-DEPT AT 1628
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
014500	   IF WS-DEPT = SPACES
014510         GO TO DE999.
009330	     MOVE WS-DEPT	 TO DPT-CODE.
	   IF (DPT-CODE = "XXXX" OR "YYYY") OR
	      (DPT-SCDE = "VAT")
014590         GO TO DE08.
010090       PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       MOVE "Invalid department code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
014590	       GO TO DE08.
	     DISPLAY DPT-DESC AT 1633
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

       DE09.
	     MOVE "'C'ontinue or 'R'ekey  [ ]"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "R" OR "C")
               GO TO DE09.
           IF WS-OPTION = "R"
               GO TO DE08.
013970	     DISPLAY S15.
	   IF WS-PRT = "S"
	       GO TO DE12.

014610 DE10.
	     PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       GO TO DE999.
	   IF (DPT-CODE = "XXXX" OR "YYYY") OR
	      (DPT-SCDE = "VAT")
		GO TO DE10.

014660 DE12.
014670	     MOVE SPACES	 TO REP-DETAIL1.
014680       PERFORM DE95 THRU DE999.
014690	     MOVE ZERO		 TO WS-PAGE.
014700     IF WS-PRT = "S"
014710         GO TO DE08.
014760	     GO TO DE10.

014770 DE15.
      *
      *    ****   1 0	C P I	( N O R M A L	P R I N T )
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
012960	       MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "H"		 TO W02-PRN-TYPE.
014780	     MOVE SPACES	 TO REP-DETAIL1.
014790	     MOVE "Page:"	 TO REP-PAGE.
014800	     MOVE "Date:"	 TO REP-DATE.
014810	     MOVE W95-COMP	 TO REP-COMPANY.
014820	     ADD 1		 TO WS-PAGE.
014830	     MOVE WS-PAGE	 TO REP-P-NO.
014840	     MOVE W12-TODAY	 TO REP-DTE.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
014860	     MOVE SPACES	 TO REP-DETAIL1.
           IF WS-BIN = "Y"
014870         MOVE "STOCK-TAKE - BIN SEQUENCE" TO REP-COMPANY
           ELSE
014870         MOVE "STOCK-TAKE BY DEPARTMENT" TO REP-COMPANY.
	     PERFORM CALL-PRINTUTL.
014890	     MOVE SPACES	 TO REP-DETAIL1.
014900     IF WS-BIN NOT = "Y"
014910	       MOVE "Category:"  TO REP-HEAD2
014920	       MOVE DPT-CODE	 TO REP-CATNO
014930	       MOVE DPT-DESC	 TO REP-CAT
014940	       PERFORM CALL-PRINTUTL.
014950	     MOVE SPACES	 TO REP-DETAIL1.
      *
      *    ****   1 7	C P I	( C O N D E N S E D   P R I N T )
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 3		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
012960	       MOVE SPACES	 TO REP-DETAIL1
	       MOVE "H" 	 TO W02-PRN-TYPE.
014960	     MOVE "Item Code"	 TO S-H1.
014970	     MOVE WS-DESC-H1	 TO S-H2.
014980	     MOVE "Bin" 	 TO S-H3.
014990	     MOVE "Quantity"	 TO S-H4.
015000	     MOVE " Count 1"	 TO S-H5.
015010	     MOVE " Count 2"	 TO S-H6.
015020	     MOVE " Accept"	 TO S-H7.
	     MOVE 1		 TO WS-ADVANCE.
015030	     PERFORM CALL-PRINTUTL.
015040	     MOVE ALL "Ä"	 TO REP-LINE9B.
015110	     MOVE "Â"		 TO S-L1 S-L2 S-L3 S-L4 S-L5.
	     MOVE "¿"		 TO S-L6.
015060	     PERFORM CALL-PRINTUTL.
015070	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "D"		 TO W02-PRN-TYPE.

015080 DE20.
015090       EXIT.

015100 DE25.
015110	     MOVE "Å"		 TO S-L1 S-L2 S-L3 S-L4 S-L5.
	     MOVE "´"		 TO S-L6.

015100 DE26.
015110	     MOVE "³"		 TO S-L1 S-L2 S-L3 S-L4 S-L5 S-L6.

015120 DE95.
015130	     MOVE 0		 TO WS-S1.
015160       PERFORM DE15.
	     MOVE DPT-CODE	 TO STK-LDG.
	     MOVE "!"		 TO STK-CODE.
015170       PERFORM START-AT-STOCK-LEDG THRU READ-STOCK-EXIT.
015180     IF WS-F-ERROR = 22
015190         GO TO DE105.

015200 DE100.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO DE999.
           IF WS-F-ERROR = 22
               GO TO DE105.
015230     IF WS-BIN NOT = "Y"
015240	       IF STK-LDG NOT = DPT-CODE
015250		   GO TO DE105.
	   IF WS-BIN = "Y"
	       IF W15-BIN = "S"
		   IF STK-BIN > W15-END
		       GO TO DE105.
	   IF NOT (WS-PRINTER = "S")
	       IF WS-USE-ITM = "Y"
		   DISPLAY S03-ITM
	       ELSE
	       IF WS-EXT-STK = "Y"
		   DISPLAY S03-EXT
	       ELSE
015270		   DISPLAY S03.
	   IF STK-IND > 1 AND < 8
	       GO TO DE100.
	   IF WS-PRC = "N"
	       IF STK-IND = 1 OR 9
		   GO TO DE100.
	   IF WS-USE-ITM = "Y"
	       MOVE STK-ITEM	 TO S-CDE
	       MOVE "/" 	 TO S-SL
	       MOVE STK-ITM	 TO S-ITM
	   ELSE
015280	       MOVE STK-CODE	 TO S-EXT-CDE.
015290	     MOVE STK-DESC	 TO S-DSC.
015300	     MOVE STK-BIN	 TO S-BN.
015310     IF WS-PQNT = "Y"
               IF STK-QUANT > 0.0
015320             MOVE STK-QUANT TO S-QNT
               ELSE
015320             MOVE STK-QUANT TO S-QNTN.
015330       PERFORM DE26.
	   IF WS-PQNT = "Y"
	       IF (STK-QPIC NOT = ZERO) OR
		  (STK-QJOB NOT = ZERO)
		   PERFORM DE110.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
015360	       MOVE ALL "Ä"	 TO REP-LINE9B
015370         PERFORM DE25
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
015390	   ELSE
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
015360	       MOVE ALL "Ä"	 TO REP-LINE9B
015110	       MOVE "Á" 	 TO S-L1 S-L2 S-L3 S-L4 S-L5
	       MOVE "Ù" 	 TO S-L6
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
015410	       PERFORM DE15.
015420	     MOVE SPACES	 TO REP-DETAIL1.

015430 DE102.
015480       GO TO DE100.

015490 DE105.
015500	   IF W02-LINAGE < (W02-PRN-LENGTH - 4)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
015520	   ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
015540	       PERFORM DE15.
      *
      *    ****   1 0	C P I	( N O R M A L	P R I N T )
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
012960	       MOVE SPACES	 TO REP-DETAIL1
	       MOVE "D" 	 TO W02-PRN-TYPE.
015550	   IF WS-BIN = "Y"
015560	       MOVE "***** END LIST *****"
				 TO REP-HEAD13
015570     ELSE
015580	       MOVE "***** END DEPARTMENT *****"
				 TO REP-HEAD13.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
015600	     MOVE SPACES	 TO REP-DETAIL1.
015610	     MOVE "Team ............./............."
				 TO REP-DETAIL1.
	     PERFORM CALL-PRINTUTL.
015630	     MOVE SPACES	 TO REP-DETAIL1.
015640	     MOVE "Signature ..............."
				 TO REP-DETAIL1.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
015660	     MOVE SPACES TO REP-DETAIL1.

015670 DE106.
015700       GO TO DE999.

015710 DE108.
	     MOVE "''A'll or 'S'elective Bins  [A]"
				 TO WS-ERR-MES.

       DE108A.
	     MOVE "A"		 TO WS-OPTION.
033910	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "A" OR "S")
	       GO TO DE108A.
	     MOVE WS-OPTION	 TO W15-BIN.
	   IF WS-OPTION = "A"
	       GO TO DE108C.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 26		 TO SHADE-COL.
	     MOVE 32		 TO SHADE-WIDTH.
	     MOVE 5		 TO SHADE-LINES.
	     DISPLAY S05B.
	     PERFORM SCREEN-SHADOW.

       DE108B.
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
		     PERFORM RESTORE-SCREEN-3
		     GO TO DE999
		 WHEN OTHER	 CALL X"E5"
		     GO TO DE108B
	       END-EVALUATE.
	     PERFORM CHECK-CORRECT.
	   IF W15-END < W15-START
	       MOVE "Ending Location < Start Location"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO DE108B.

       DE108C.
	   IF W15-BIN = "S"
	       MOVE W15-START	 TO STK-BIN
	   ELSE
	       MOVE "!"		 TO STK-BIN.
	     MOVE SPACES	 TO STK-CODE.
             PERFORM START-AT-STOCK-BIN THRU READ-STOCK-EXIT,
           IF WS-F-ERROR = 22
               GO TO DE999.
016070	     MOVE SPACES	 TO REP-DETAIL1.
016080       PERFORM DE15.
016120       PERFORM DE100 THRU DE999.
016130	     MOVE ZERO		 TO WS-PAGE.
       DE109.
015700       GO TO DE999.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	      Order (Picking Slip) and Job Quantities	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       DE110.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
015420	     MOVE SPACES	 TO REP-DETAIL1.
	     PERFORM DE26.
	   IF NOT (STK-QJOB = ZERO)
	       MOVE "     *** Job quantity ***"
				 TO S-DSC
	       IF STK-QJOB > 0.0
015320		   MOVE STK-QJOB  TO S-QNT
               ELSE
015320		   MOVE STK-QJOB  TO S-QNTN
	       END-IF
	       IF NOT (STK-QPIC = ZERO)
		   MOVE 1	  TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL
		   IF NOT (W02-LINAGE < W02-PRN-LENGTH)
015360		       MOVE ALL "Ä"
				 TO REP-LINE9B
015110		       MOVE "Á"	 TO S-L1 S-L2 S-L3 S-L4 S-L5
		       MOVE "Ù"	 TO S-L6
		       MOVE 99	 TO WS-ADVANCE
		       PERFORM CALL-PRINTUTL
015410		       PERFORM DE15
		       IF WS-USE-ITM = "Y"
			   MOVE STK-ITEM
				 TO S-CDE
			   MOVE "/"
				 TO S-SL
			   MOVE STK-ITM
				 TO S-ITM
		       ELSE
015280			   MOVE STK-CODE
				 TO S-EXT-CDE
		       END-IF
015290		       MOVE STK-DESC
				 TO S-DSC
015300		       MOVE STK-BIN
				 TO S-BN
		       MOVE 1	 TO WS-ADVANCE
		       PERFORM CALL-PRINTUTL
015420		       MOVE SPACES
				 TO REP-DETAIL1
		       PERFORM DE26
		   ELSE
		       PERFORM DE26.
	   IF NOT (STK-QPIC = ZERO)
	       MOVE "     *** P/S quantity ***"
				 TO S-DSC
	       IF STK-QPIC > 0.0
015320		   MOVE STK-QPIC  TO S-QNT
               ELSE
015320		   MOVE STK-QPIC  TO S-QNTN.

016140 DE999.
016150       EXIT.
      /
019370 ZA000-INIT	       SECTION 8.
019380 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
019390	     DISPLAY CLR-SCREEN.
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
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
019480	     MOVE 3		 TO WS-PARKEY.
044240       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-CASES	 TO WS-USE-CASES.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.

       ZA01B.
	     MOVE 6		 TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-WHOLESALE  TO WS-WS.
019880	     MOVE PAR-COST-CODE  TO W30-COST-CODE.
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
	     INSPECT WS-CS-HD REPLACING LEADING "-" BY " ".
	     INSPECT WS-WS-HD REPLACING LEADING "-" BY " ".
	     INSPECT WS-RT-HD REPLACING LEADING "-" BY " ".
019820	     MOVE 1		 TO WS-PARKEY.
019830       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
019840       MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W40-DATE1 W40-DATE2.
           IF W40-M2 > 3
               SUBTRACT 3        FROM W40-M2
           ELSE
               SUBTRACT 1        FROM W40-Y2
               ADD 9             TO W40-M2.
019850	     MOVE PAR-COMPANY	 TO W95-COMP.

       ZA05.
018380	     DISPLAY WS-BLNK78 AT 1402 WITH FOREGROUND-COLOR 3.
019900	     GO TO ZA999-EXIT.

055270 ZA49.
055280       DISPLAY "Too many files OPEN" AT 0812
055290                WITH FOREGROUND-COLOR 14.
055300       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
055310       DISPLAY "Press any key to continue" AT 1212.
055320       ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
055330       GO TO ZA205.

055340 ZA50.
055390     IF WS-F-ERROR = 22
055400         MOVE "STOCK file" TO WS-FILE
           ELSE
           IF WS-F-ERROR = 25
               MOVE "STOCK MEMO" TO WS-FILE.
055440       DISPLAY "Rebuild " AT 0812
055450                WS-FILE WITH FOREGROUND-COLOR 14.
055460       DISPLAY "Press any key to continue" AT 1012.
055470       ACCEPT WS-OPTION AT 1038 WITH FOREGROUND-COLOR 15.

055480 ZA51.
055490       EXIT PROGRAM.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
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

019910 ZA200.
019920       DISPLAY "Files locked - Try later" AT 2312
019930                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
019940       DISPLAY "Press any key" AT 2512 
019950		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
019960	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.

       ZA205.
             EXIT PROGRAM.

019990 ZA999-EXIT.
020000       EXIT.
      /
020010 ZB000-ERROR	       SECTION 8.
      
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
	   ELSE
041850     IF WS-F-ERROR = 21
041860         MOVE W02-SPARTS   TO WS-FILE
041870         MOVE WS-SPAKEY    TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 22
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
042170       STOP RUN.
