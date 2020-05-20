      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	  ****	      ***   ********   *
      *   **	**     **     **   **	 **  **      ****	 **    *
      *   **	       **     **   **	**    **    ** **	**     *
      *    ******      **     ******	**    **   **  **      **      *
      * 	**     **     **	**    **  ********    **       *
      *   **	**     **     **	 **  **        **     **       *
      *    ******      **     **	  ****	      ****    **       *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *   S T O C K   B I L L	O F   Q U A N T I T Y	P R O G R A M  *
      *                                                                *
      *     SPECIAL SPARES VERSION        WEEKLY TOTALS & X-REF        *
      *                                                                *
      *     VERSION 8.13.09 - November 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STP047.
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

000140 COPY STKMEM.SL.

000140 COPY STOCK.SL.

      /
000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

       COPY DEPART.FDE.

000290 COPY PARAM.FDE.

000350 COPY SPARTS.FDE.

000140 COPY STKMEM.FDE.

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
003190 77  WS-CAT	   PIC	9(04)	 COMP-5.
003200 77  WS-SCAT	   PIC	9(04)	 COMP-5.
003210 77  WS-STKKEY	   PIC	9(04)	 COMP-5.
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
003310 77  WS-PRT	   PIC	X(01).
003320 77  WS-BIN	   PIC	X(01).
       77  WS-ETYPE	   PIC	X(01).
003330 77  WS-OPTION	   PIC	X(01).
003330 77  WS-PRICE	   PIC	X(01).
001980 77  WS-ITM	   PIC	X(18).
002180 77  WS-USE-CASES    PIC	X(01).
002190 77  WS-USE-PACKS    PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
       77  WS-CARDEX	   PIC	X(01).
       77  WS-NUM	   PIC	ZZ9.
003350 77  WS-ERROR	   PIC	9(01).
003380 77  WS-KEY1	   PIC	9(04)	 COMP-5.
003390 77  WS-KSTORE	   PIC	9(04)	 COMP-5.
003470 77  WS-ORD	   PIC S9(05)	 COMP-3.
003480 77  WS-TOTAL	   PIC S9(07)V99 COMP-3.
003490 77  WS-VALUE	   PIC S9(07)V99 COMP-3.
003500 77  WS-CCOST	   PIC S9(07)V99 COMP-3.
003510 77  WS-CSELL	   PIC S9(07)V99 COMP-3.
003520 77  WS-SCOST	   PIC S9(07)V99 COMP-3.
003530 77  WS-SSELL	   PIC S9(07)V99 COMP-3.
003540 77  WS-TCOST	   PIC S9(09)V99 COMP-3.
003550 77  WS-TSELL	   PIC S9(09)V99 COMP-3.
003560 77  WS-DEPT	   PIC	9(03).
003570 77  WS-WEEK	   PIC	9(01).
       77  WS-LABCDE	   PIC	X(08).
001330 77  WS-DESC-H1	   PIC	X(14) VALUE "Description".
       77  WS-DESC-H2	   PIC	X(14) VALUE "Description 2".
       77  WS-RT-HD	   PIC	X(10) VALUE "-Retail   ".
       77  WS-WS-HD	   PIC	X(10) VALUE "-Wholesale".
       77  WS-CS-HD	   PIC	X(10) VALUE "-Cash sale".
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

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "STP".
	   03  WS-PROG	       PIC  X(03) VALUE "047".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003610 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

000360 COPY SPARTS.ID.

000140 COPY STKMEM.ID.

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
000510	   03  REP-LINE4 REDEFINES W02-PRN-LINE.
000520	       05  FILLER      PIC  X(30).
000530	       05  REP-HEAD1   PIC  X(20).
000540	       05  FILLER      PIC  X(30).
000550	   03  REP-LINE5 REDEFINES W02-PRN-LINE.
000560	       05  FILLER      PIC  X(09).
000570	       05  REP-DSC     PIC  X(30).
000580	       05  FILLER      PIC  X(93).
000590	   03  REP-LINE6 REDEFINES W02-PRN-LINE.
000600	       05  REP-HEAD2   PIC  X(10).
000610	       05  REP-CNO     PIC  9(03).
000620	       05  REP-CATNO REDEFINES REP-CNO PIC 9(03).
000630	       05  FILLER      PIC  X(01).
000640	       05  REP-CAT     PIC  X(30).
000650	       05  FILLER      PIC  X(36).
000850	   03  REP-LINE9 REDEFINES W02-PRN-LINE.
000860	       05  REP-PG      PIC  X(06).
000870	       05  REP-P-N     PIC  Z(03)9.
000880	       05  FILLER      PIC  X(22).
000890	       05  REP-COMP    PIC  X(40).
000900	       05  REP-REPORT  PIC  X(34).
000910	       05  FILLER      PIC  X(12).
000920	       05  REP-DAT     PIC  X(06).
000930	       05  REP-DT      PIC  Z9/99/99.
000940	   03  REP-LINE9A REDEFINES W02-PRN-LINE.
000950	       05  REP-HEAD6   PIC  X(13).
000960	       05  REP-HEAD6A  PIC  X(04).
000970	       05  REP-HEAD7   PIC  X(25).
000980	       05  REP-HEAD8   PIC  X(12).
000990	       05  REP-HEAD9   PIC  X(08).
001000	       05  REP-HEAD10  PIC  X(08).
001010	       05  REP-HEAD11  PIC  X(09).
001020	       05  REP-HEAD12  PIC  X(09).
001030	       05  REP-HEAD14  PIC  X(07).
001040	       05  REP-HEAD15  PIC  X(10).
001050	       05  REP-HEAD16  PIC  X(07).
001060	       05  REP-HEAD17  PIC  X(08).
001070	       05  REP-HEAD18  PIC  X(12).
001080	   03  REP-LINE9B REDEFINES W02-PRN-LINE.
001090	       05  S-H1        PIC  X(15).
001100	       05  S-H2        PIC  X(28).
001110	       05  S-L1        PIC  X(01).
001120	       05  S-H3        PIC  X(03).
001130	       05  S-L2        PIC  X(01).
001140	       05  S-H4        PIC  X(07).
001150	       05  S-L3        PIC  X(01).
001160	       05  S-H5        PIC  X(07).
001170	       05  S-L4        PIC  X(01).
001180	       05  S-H6        PIC  X(07).
001190	       05  S-L5        PIC  X(01).
001200	       05  S-H7        PIC  X(07).
001210	       05  S-L6        PIC  X(01).
001220	   03  REP-LINE9C REDEFINES W02-PRN-LINE.
001230	       05  S-CDE       PIC  X(14).
001240	       05  FILLER      PIC  X(01).
001250	       05  S-DSC       PIC  X(28).
001260	       05  FILLER      PIC  X(01).
001270	       05  S-BN        PIC  X(03).
001280	       05  FILLER      PIC  X(01).
001290	       05  S-QNT       PIC  Z(04)9.9.
	       05  S-QNTN REDEFINES S-QNT
			       PIC  -(04)9.9.
001300	       05  FILLER      PIC  X(25).
001570	   03  REP-LINE11 REDEFINES W02-PRN-LINE.
001580	       05  FILLER      PIC  X(13).
001590	       05  REP-HEAD13  PIC  X(26).
001600	       05  FILLER      PIC  X(02).
001610	       05  REP-DCOST   PIC  Z(08)9.99.
001620	       05  FILLER      PIC  X(02).
001630	       05  REP-DSELL   PIC  Z(08)9.99.
001640	       05  FILLER      PIC  X(13).
	   03  PRD-LINE1 REDEFINES W02-PRN-LINE.
	       05  PRD-H1      PIC  X(20).
	       05  PRD-H2      PIC  X(32).
	       05  PRD-H2A     PIC  X(28).
	   03  PRD-LINE2 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(12).
	       05  PRD-H3      PIC  X(09).
	       05  PRD-H4      PIC  X(19).
	       05  PRD-H5      PIC  X(32).
	       05  PRD-H6      PIC  X(14).
	       05  PRD-H7      PIC  X(10).
	   03  PRD-LINE3 REDEFINES W02-PRN-LINE.
	       05  PRD-EXT-ITEM.
		   07  PRD-ITEM
			       PIC  X(14).
		   07  PRD-SL  PIC  X(01).
		   07  PRD-ITM PIC  X(03).
	       05  FILLER      PIC  X(02).
	       05  PRD-DESC    PIC  X(30).
	       05  FILLER      PIC  X(02).
	       05  PRD-QUANT   PIC  ZZZZ9.
	       05  FILLER      PIC  X(25).
	   03  PRD-LINE4 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(10).
	       05  PRD-QNT     PIC  ZZZZ9.9999.
	       05  FILLER      PIC  X(01).
	       05  PRD-EXT-ITEM2.
		   07  PRD-ITEM2
			       PIC  X(14).
		   07  PRD-SL2 PIC  X(01).
		   07  PRD-ITM2
			       PIC  X(03).
	       05  FILLER      PIC  X(01).
	       05  PRD-DESC2   PIC  X(30).
	       05  FILLER      PIC  X(01).
	       05  PRD-SELL    PIC  Z(06)9.999.
	       05  FILLER      PIC  X(01).
	       05  PRD-TOTAL   PIC  Z(08)9.99.

003740 01  W05.
003750     03  W05-S OCCURS 300   PIC 9(04)     COMP-5.

       COPY W05.VAT.

       COPY W10.STK.

       COPY W12.WS.

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

       01  W44-SCREEN1.
           03  FILLER          PIC X(2000).

       01  W44-ATTRIB1.
           03  FILLER          PIC X(2000).

       COPY FUNCTION.WS.

003950 COPY W50.WS.

004040 COPY W70STOCK.WS.

004060 01  W70-KEYS.
004070     03  W70-BIND        PIC 9(05)     COMP-0 VALUE ZERO.

       COPY W75.STK.

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
004450	   03  LINE 16 COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
004460				 PIC X(14) FROM STK-CODE.

       01  S03-ITM.
005540	   03  LINE 16 COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC X(14) FROM STK-ITEM.
	   03	       COLUMN 40 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE "/".
	   03	       COLUMN 41 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC X(03) FROM STK-ITM.

005530 01  S03-EXT.
005540	   03  LINE 16 COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC X(18) FROM STK-CODE.

004470 01  S04.
004480	   03  LINE 10 COLUMN 20 FOREGROUND-COLOR 14
004490                           VALUE "Busy loading stock departments".

004440 01  S05.
004450	   03  LINE 12 COLUMN 23 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
004460				 PIC X(14) USING W10-EXT-ITEM AUTO.

       01  S05-ITM.
005540	   03  LINE 12 COLUMN 23 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC X(14) USING W10-ITEM AUTO.
	   03	       COLUMN 38 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC X(03) USING W10-ITM AUTO.

       01  S05-DSP.
	   03  LINE 12 COLUMN 37 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE "/".

005530 01  S05-EXT.
005540	   03  LINE 12 COLUMN 23 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC X(18) USING W10-EXT-ITEM AUTO.

005040 01  S15.
005050     03  LINE 16 COLUMN 12 VALUE "Stock record:".

004650 01  S16.
           03  LINE  2 COLUMN 32 FOREGROUND-COLOR 15
                                 VALUE "BILL OF QUANTITIES".
004660	   03  LINE  4 COLUMN 12 FOREGROUND-COLOR 14
004670                           VALUE "Printing bill of quantities".
004680	   03  LINE  6 COLUMN 12 VALUE "Are you sure (".
	   03	       COLUMN 26 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Y".
	   03	       COLUMN 27 VALUE "/".
	   03	       COLUMN 28 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "N".
	   03	       COLUMN 29 VALUE ")?".

       01  S17.
	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC ZZZZ9 USING W10-QUANT AUTO.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
005070 AA00M		 SECTION.
005080 AA00.
003240	   IF LS0-STLEV < 1
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
005090	     PERFORM ZA000-INIT.
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.
	     PERFORM GE000.
      *
      *      1 0   C P I
      *
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
      *
      *  ***  *     ***   **** ****   ****  ****  *** *   * ***** **** ****
      * *   * *    *   * *     *      *   * *	*  *  **  *   *   *    *   *
      * *     *    *   *  ***  ***    ****  ****   *  * * *   *   ***  ****
      * *   * *    *   *     * *      *     *	*  *  *  **   *   *    *   *
      *  ***  ****  ***  ****  ****   *     *	* *** *   *   *   **** *   *
      *
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

       AA49.
             EXIT PROGRAM.

       COPY AA50.LUP.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

       COPY OPTION.CRT.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP3.

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

       COPY STOCK.1ST.

       COPY APAC.HLP.

       COPY CA000.STK.

       COPY CA100.STK.

       COPY CA200.STK.

       COPY CA500.STK.

      /
019290 GE000	       SECTION 5.
019300 GE00.
019310       DISPLAY CLR-SCREEN.
019320       DISPLAY S16.
             MOVE 1              TO W10-QUANT.
017740       MOVE "Y"            TO WS-OPTION.

       GE02.
012710	     ACCEPT WS-OPTION AT 0632 WITH FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
012770	   IF NOT (WS-OPTION = "N" OR "Y")
012780         GO TO GE02. 
012770     IF WS-OPTION = "N"
012780	       GO TO GE999.
	     DISPLAY "Print " AT 0812
		     "C" WITH FOREGROUND-COLOR 6 HIGHLIGHT "ost, "
		     "S" WITH FOREGROUND-COLOR 6 HIGHLIGHT "elling or "
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT "o price".
	     MOVE "S"		 TO WS-PRICE.

       GE02A.
	     ACCEPT WS-PRICE AT 0844
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-PRICE = "C" OR "S" OR "N")
	       GO TO GE02A.
	     DISPLAY "Selective report (" AT 1012
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT ")".
             MOVE "N"            TO WS-OPTION.

       GE03.
012710	     ACCEPT WS-OPTION AT 1035
		    WITH FOREGROUND-COLOR 15 BACKGROUND-COLOR 5
                                           UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
012770	   IF NOT (WS-OPTION = "N" OR "Y")
012780         GO TO GE03. 
012770     IF WS-OPTION = "Y"
	       PERFORM SAVE-SCREEN-3
	       MOVE W43-SCREEN3  TO W44-SCREEN1
	       MOVE W42-ATTRIB3  TO W44-ATTRIB1
               GO TO GE40.
             DISPLAY S15.

       GE05.
	     MOVE 0		 TO WS-ADVANCE.
      *
      *      1 0   C P I
      *
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
019260	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "H"		 TO W02-PRN-TYPE.
018920       MOVE SPACES         TO REP-DETAIL1.
018930	     MOVE "Page:"	 TO REP-PAGE.
018940	     MOVE "Date:"	 TO REP-DATE.
018950       MOVE W95-COMP       TO REP-COMPANY.
018960       ADD 1               TO WS-PAGE.
018970       MOVE WS-PAGE        TO REP-P-NO.
018980       MOVE W12-TODAY      TO REP-DTE.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-DETAIL1.
018990       MOVE "BILL OF QUANTITIES REPORT" 
                                 TO REP-COMPANY.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
019010	     MOVE SPACES	 TO REP-DETAIL1.
019070	     MOVE "Item Code"	 TO PRD-H1.
019080	     MOVE WS-DESC-H1	 TO PRD-H2.
	     MOVE "Items"	 TO PRD-H2A.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
019130	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE 0		 TO WS-ADVANCE.
      *
      *      1 2   C P I
      *
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "H"		 TO W02-PRN-TYPE.
019150	     MOVE "Quantity"	 TO PRD-H3.
019150	     MOVE "Item Code"	 TO PRD-H4.
019150	     MOVE "Description"  TO PRD-H5.
	   IF WS-PRICE = "S"
019150	       MOVE "Sell.Price" TO PRD-H6
019150	       MOVE "Total Sell" TO PRD-H7
	   ELSE
	   IF WS-PRICE = "C"
	       MOVE "Cost Price" TO PRD-H6
019150	       MOVE "Total Cost" TO PRD-H7.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
019260	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "D"		 TO W02-PRN-TYPE.

       GE10.
	     MOVE "!"		 TO PRT-EXT-ITEM.
010110       PERFORM START-AT-PART-CODE THRU READ-SPARTS-EXIT.
010120     IF WS-F-ERROR = 21
010130         GO TO GE35.

010140 GE15.
             PERFORM READ-SPARTS-NEXT THRU READ-SPARTS-EXIT.
010150     IF WS-F-ERROR = 21
010160         GO TO GE35.
	     MOVE PRT-EXT-ITEM	 TO STK-CODE.
010190       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF NOT (WS-PRINTER = "S")
	       IF WS-USE-ITM = "Y"
		   DISPLAY S03-ITM
	       ELSE
	       IF WS-EXT-STK = "Y"
		   DISPLAY S03-EXT
	       ELSE
010200		   DISPLAY S03.

       GE16.
	     MOVE ZERO		 TO WS-ERROR.
	     MOVE 0		 TO WS-ADVANCE.
      *
      *      1 0   C P I
      *
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
019260	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "D"		 TO W02-PRN-TYPE.
	   IF WS-USE-ITM = "Y"
	       MOVE STK-ITEM	 TO PRD-ITEM
	       MOVE "/" 	 TO PRD-SL
	       MOVE STK-ITM	 TO PRD-ITM
	   ELSE
	       MOVE STK-CODE	 TO PRD-EXT-ITEM.
             MOVE STK-DESC       TO PRD-DESC.
             MOVE W10-QUANT      TO PRD-QUANT.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO PRD-LINE3.
	     MOVE 0		 TO WS-ADVANCE.
      *
      *      1 2   C P I
      *
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
019260	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE 1		 TO WS-S5.
             MOVE ZERO           TO WS-CSELL WS-SSELL WS-TSELL.

       GE20.                    
036760	   IF PRT-EXT-CODE(WS-S5) = SPACES
036770         GO TO GE25.
036780	     MOVE PRT-EXT-CODE(WS-S5)
				 TO STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
             COMPUTE W25-VALUE = PRT-QUANT (WS-S5) * W10-QUANT.
             MOVE W25-VALUE      TO PRD-QNT.
	   IF WS-USE-ITM = "Y"
	       MOVE STK-ITEM	 TO PRD-ITEM2
	       MOVE "/" 	 TO PRD-SL2
	       MOVE STK-ITM	 TO PRD-ITM2
	   ELSE
	       MOVE STK-CODE	 TO PRD-EXT-ITEM2.
             MOVE STK-DESC       TO PRD-DESC2.
	   IF WS-PRICE = "S"
	       MOVE STK-SELL	 TO PRD-SELL WS-CSELL
	   ELSE
	   IF WS-PRICE = "C"
	       MOVE STK-COST	 TO PRD-SELL WS-CSELL.
	   IF NOT (WS-PRICE = "N")
	       COMPUTE WS-SSELL = WS-CSELL * W25-VALUE
	       MOVE WS-SSELL	 TO PRD-TOTAL
	       ADD WS-SSELL	 TO WS-TSELL.
018690	   IF W02-LINAGE < (W02-PRN-LENGTH - 2)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM GE05.
	     MOVE SPACES	 TO PRD-LINE4.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE 9		 TO WS-ERROR
014990	       GO TO GE29.
           IF WS-S5 < 30
	       ADD 1		 TO WS-S5
               GO TO GE20.

       GE25.
	   IF NOT (WS-PRICE = "N")
	       MOVE ALL "Ä"	 TO PRD-H7
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO PRD-H7
	       MOVE WS-TSELL	 TO PRD-TOTAL
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO PRD-H7
	       MOVE ALL "Í"	 TO PRD-H7.
	   IF W02-LINAGE < (W02-PRN-LENGTH - 6)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM GE05.
	     MOVE SPACES	 TO PRD-LINE2.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970         MOVE "**** PRINT ABORTED BY OPERATOR ****" 
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE 9		 TO WS-ERROR
014990	       GO TO GE29.

       GE29.
	     EXIT.

       GE30.
018770       GO TO GE15.

018780 GE35.
018790	     MOVE SPACES	 TO PRD-LINE1.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
018810	     MOVE "****** END OF REPORT *****" TO PRD-LINE1.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
018830	     MOVE SPACES TO REP-DETAIL1.

       GE36.
             GO TO GE999.

       GE40.
	     DISPLAY "Item code" AT 1212.
	     MOVE SPACES	 TO W10-EXT-ITEM.

       GE45.
             PERFORM AA300.
	   IF WS-USE-ITM = "Y"
	       DISPLAY S05-DSP
	       ACCEPT S05-ITM
	   ELSE
	   IF WS-EXT-STK = "Y"
	       ACCEPT S05-EXT
	   ELSE
	       ACCEPT S05.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
                     GO TO GE999
                 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO GE45
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
                     GO TO GE45
               END-EVALUATE
	       PERFORM AA50-LOOKUP
	       IF WS-USE-ITM = "Y"
		   DISPLAY S05-DSP
		   DISPLAY S05-ITM
	       ELSE
	       IF WS-EXT-STK = "Y"
		   DISPLAY S05-EXT
	       ELSE
		   DISPLAY S05
	       END-IF
	       IF W10-EXT-ITEM = SPACES
                   GO TO GE45.
015830	   IF W10-EXT-ITEM = SPACE
015840         GO TO GE999.
	     DISPLAY CLEAR-L25.
015850       PERFORM CA000.
015860     IF WS-F-ERROR NOT = ZERO
	       MOVE "Invalid code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO GE45.
	     DISPLAY STK-DESC AT 1239
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF STK-IND = 4
	       MOVE "Header Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO GE45.
	     MOVE STK-CODE	 TO PRT-EXT-ITEM.
             PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
           IF WS-F-ERROR = 21
	       MOVE "This is not a produced item"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO GE45.
	     DISPLAY "Quantity required" AT 1412.
             MOVE 1              TO W10-QUANT.

       GE50.
             ACCEPT S17 AT 1431.
           IF W10-QUANT = ZERO
               GO TO GE45.

       GE55.
	     MOVE "'C'ontinue or 'R'ekey  [ ]"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
012770	   IF NOT (WS-OPTION = "C" OR "R")
012780         GO TO GE55. 
012770     IF WS-OPTION = "R"
               GO TO GE50.
             MOVE ZERO           TO WS-PAGE.
             PERFORM GE05.
	     PERFORM GE16 THRU GE29.
	   IF WS-ERROR = ZERO
	       PERFORM GE35.
             MOVE 1201 TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
	     MOVE W44-SCREEN1	 TO W43-SCREEN3.
	     MOVE W44-ATTRIB1	 TO W42-ATTRIB3.
	     PERFORM RESTORE-SCREEN-3.
	     GO TO GE40.

019350 GE999.
019360       EXIT.
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
	     MOVE "STKMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKMEM.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
019480	     MOVE 3		 TO WS-PARKEY.
044240       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-CRDX	 TO WS-CARDEX.
	     MOVE PAR-CASES	 TO WS-USE-CASES.
	     MOVE PAR-PACKS	 TO WS-USE-PACKS.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.

       ZA01B.
	     MOVE 6		 TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-WHOLESALE  TO WS-WS.
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
019620	     MOVE 3		 TO WS-NETKEY.
019630       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
019690	     MOVE STK-WEEK	 TO WS-WEEK.
             MOVE STK-LABCDE     TO WS-LABCDE.
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
019900       GO TO ZA999-EXIT.

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
041860	       MOVE W02-SPARTS	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE PRT-ITEM     TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 25
041860         MOVE W02-STKMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY.
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
