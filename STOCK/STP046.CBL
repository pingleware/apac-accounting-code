      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	  ****	      ***    ******    *
      *   **	**     **     **   **	 **  **      ****   **	  **   *
      *   **	       **     **   **	**    **    ** **   **	       *
      *    ******      **     ******	**    **   **  **   *******    *
      * 	**     **     **	**    **  ********  **	  **   *
      *   **	**     **     **	 **  **        **   **	  **   *
      *    ******      **     **	  ****	      ****   ******    *
      *                                                                *
      *     S T O C K	L O W - L E V E L S   R E P O R T	       *
      *                                                                *
      *     VERSION 8.13.09 - November 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STP046.
000030 AUTHOR.	       JW LEMMON (APAC).
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
003100 77  WS-S1	    PIC 9(04)	  COMP-5.
003110 77  WS-S2            PIC 9(04)     COMP-5.
003120 77  WS-S3            PIC 9(04)     COMP-5.
003130 77  WS-S4            PIC 9(04)     COMP-5.
003140 77  WS-S5            PIC 9(04)     COMP-5.
003140 77  WS-S6            PIC 9(04)     COMP-5.
003140 77  WS-S7	    PIC 9(04)	  COMP-5.
003140 77  WS-S8	    PIC 9(04)	  COMP-5.
003150 77  WS-IXS	    PIC 9(04)	  COMP-5.
003150 77  WS-IXS1          PIC 9(04)     COMP-5.
003160 77  WS-IXS2          PIC 9(04)     COMP-5.
003170 77  WS-IXS3          PIC 9(04)     COMP-5.
003180 77  WS-IXS4          PIC 9(04)     COMP-5.
003190 77  WS-CAT           PIC 9(04)     COMP-5.
003200 77  WS-SCAT          PIC 9(04)     COMP-5.
003210 77  WS-STKKEY        PIC 9(04)     COMP-5.
003220 77  WS-SPAKEY        PIC 9(04)     COMP-5.
003230 77  WS-PARKEY        PIC 9(04)     COMP-5.
003250 77  WS-NETKEY	    PIC 9(04)	  COMP-5.
003150 77  WS-PRN-NO	   PIC X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC X(01)	 COMP-X.
003270 77  WS-PAGE	    PIC 9(04)	  COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
003300 77  WS-PQNT	    PIC X(01).
003310 77  WS-WS            PIC X(01) VALUE "N".
003310 77  WS-MEMO          PIC X(01) VALUE "N".
003310 77  WS-PRT           PIC X(01).
003320 77  WS-BIN           PIC X(01).
       77  WS-ETYPE         PIC X(01).
003330 77  WS-OPTION	    PIC X(01).
003330 77  WS-PRICE         PIC X(01).
       77  WS-NUM	    PIC ZZ9.
003350 77  WS-ERROR         PIC 9(01).
003380 77  WS-KEY1          PIC 9(04)     COMP-5.
003390 77  WS-KSTORE        PIC 9(04)     COMP-5.
003470 77  WS-ORD	    PIC S9(05)	  COMP-3.
003480 77  WS-TOTAL	    PIC S9(07)V99 COMP-3.
003490 77  WS-VALUE         PIC S9(07)V99 COMP-3.
003500 77  WS-CCOST         PIC S9(07)V99 COMP-3.
003510 77  WS-CSELL         PIC S9(07)V99 COMP-3.
003520 77  WS-SCOST         PIC S9(07)V99 COMP-3.
003530 77  WS-SSELL         PIC S9(07)V99 COMP-3.
003540 77  WS-TCOST	    PIC S9(09)V99 COMP-3.
003550 77  WS-TSELL	    PIC S9(09)V99 COMP-3.
003560 77  WS-DEPT	    PIC X(04).
003570 77  WS-WEEK          PIC 9(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
       77  WS-LABCDE	   PIC	X(08).
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
000880	       05  FILLER      PIC  X(20).
000890	       05  REP-COMP    PIC  X(40).
000900	       05  REP-REPORT  PIC  X(34).
000910	       05  FILLER      PIC  X(12).
000920	       05  REP-DAT     PIC  X(06).
000930	       05  REP-DT      PIC  Z9/99/9999.
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
001570	   03  REP-LINE11 REDEFINES W02-PRN-LINE.
001580	       05  FILLER      PIC  X(13).
001590	       05  REP-HEAD13  PIC  X(26).
001600	       05  FILLER      PIC  X(02).
001610	       05  REP-DCOST   PIC  Z(08)9.99.
001620	       05  FILLER      PIC  X(02).
001630	       05  REP-DSELL   PIC  Z(08)9.99.
001640	       05  FILLER      PIC  X(13).
      * 					12CPI
002490	   03  ORD-LINE1 REDEFINES W02-PRN-LINE.
002500	       05  ORD-H1      PIC  X(19).
002510	       05  ORD-H2      PIC  X(07).
002520	       05  ORD-H3      PIC  X(31).
	       05  ORD-H3A     PIC  X(33).
      * 					12CPI

002700	   03  ORD-LINE2 REDEFINES W02-PRN-LINE.
	       05  ORD-EXT-ITEM.
002710		   07  ORD-ITEM
			       PIC  X(14).
		   07  ORD-SL  PIC  X(01).
		   07  ORD-ITM PIC  X(03).
002720	       05  FILLER      PIC  X(01).
002730	       05  ORD-BIN     PIC  X(04).
002740	       05  FILLER      PIC  X(01).
002750	       05  ORD-DESC    PIC  X(30).
002760	       05  FILLER      PIC  X(01).
002750	       05  ORD-DESC2   PIC  X(30).
	       05  FILLER      PIC  X(03).
      * 					17CPI
002700	   03  ORD-LINE3 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(19).
002530	       05  ORD-H4      PIC  X(12).
002540	       05  ORD-H5      PIC  X(09).
002550	       05  ORD-MOVE.
002560		   07  ORD-H6  PIC  X(28).
002570		   07  ORD-H7  PIC  X(10).
002580		   07  ORD-H8  PIC  X(29).
002590		   07  FILLER  PIC  X(02).
		   07  ORD-H9A PIC  X(07).
002600		   07  ORD-H9  PIC  X(09).
		   07  ORD-H9B PIC  X(07).
002610	       05  ORD-MOV REDEFINES ORD-MOVE.
		   07  FILLER  PIC  X(02).
002620		   07  ORD-H10 PIC  X(11).
002630		   07  ORD-H11 PIC  X(11).
002640		   07  ORD-H12 PIC  X(10).
002650		   07  ORD-H13 PIC  X(12).
002660		   07  ORD-H14 PIC  X(13).
002670		   07  ORD-H15 PIC  X(11).
002680		   07  ORD-H16 PIC  X(15).
002690		   07  ORD-H17 PIC  X(07).
002700	   03  ORD-LINE4 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(17).
002770	       05  ORD-QUANT   PIC  Z(05)9.9999-.
	       05  FILLER      PIC  X(01).
002780	       05  ORD-REORD   PIC  Z(05)9.99.
002790	       05  FILLER      PIC  X(01).
002800	       05  ORD-MTH1    PIC  Z(06)9.99.
002810	       05  FILLER      PIC  X(01).
002820	       05  ORD-MTH2    PIC  Z(06)9.99.
002830	       05  FILLER      PIC  X(01).
002840	       05  ORD-MTH3    PIC  Z(06)9.99.
002850	       05  FILLER      PIC  X(01).
002860	       05  ORD-MAVG    PIC  Z(06)9.99.
002870	       05  FILLER      PIC  X(01).
002880	       05  ORD-MTD     PIC  Z(06)9.99.
002890	       05  FILLER      PIC  X(01).
002900	       05  ORD-YTD     PIC  Z(08)9.99.
002910	       05  FILLER      PIC  X(02).
002920	       05  ORD-ORDER   PIC  Z(06)9.99.
002930	       05  FILLER      PIC  X(01).
002940	       05  ORD-COST    PIC  Z(08)9.99.

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
004450	   03  LINE 17 COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
004460				 PIC X(14) FROM STK-ITEM.

004440 01  S03-ITM.
004450	   03  LINE 17 COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
004460				 PIC X(14) FROM STK-ITEM.
	   03	       COLUMN 40 FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
				 VALUE "/".
	   03	       COLUMN 41 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(03) FROM STK-ITM.

004440 01  S03-EXT.
004450	   03  LINE 17 COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
004460				 PIC X(18) FROM STK-CODE.

004470 01  S04.
004480	   03  LINE 10 COLUMN 20 FOREGROUND-COLOR 14
004490                           VALUE "Busy loading stock departments".

004910 01  S14.
004920     03  LINE  2 COLUMN 25 FOREGROUND-COLOR 15 VALUE 
004930                           "STOCK LEVELS AND RE-ORDER REPORT".
004940     03  LINE  4 COLUMN 12 FOREGROUND-COLOR 14 VALUE
004950                           "Busy printing Stock Levels report".
004960	   03  LINE  6 COLUMN 12 VALUE "Are you sure (".
	   03	       COLUMN 26 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Y".
	   03	       COLUMN 27 VALUE "/".
	   03	       COLUMN 28 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "N".
	   03	       COLUMN 29 VALUE ")?".

       01  S14A.
004970     03  LINE  8 COLUMN 12 VALUE "A . . All stock".
004980     03  LINE  9 COLUMN 12 VALUE "M . . Movement current month".
004990     03  LINE 10 COLUMN 12 VALUE "L . . Low-level stock only".
005000	   03  LINE 11 COLUMN 12 VALUE "Select option".

       01  S14B.
004750	   03  LINE 13 COLUMN 12 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "A".
	   03	       COLUMN 13 VALUE "ll or ".
	   03	       COLUMN 19 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "S".
	   03	       COLUMN 20 VALUE "eletive departments".

       01  S14C.
004770	   03  LINE 15 COLUMN 12 VALUE "Department code".

005040 01  S15.
005050	   03  LINE 17 COLUMN 12 VALUE "Stock record:".

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
005070 AA00M		 SECTION.
005080 AA00.
003240	   IF LS0-STLEV < 3
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
	     PERFORM FE00-REPORT.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

       AA49.
             EXIT PROGRAM.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

       COPY OPTION.CRT.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP3.

      /
005810 AB000              SECTION.
      *
      *    ****   1 7	C P I	( C O N D E N S E D   P R I N T )
      *
       AB25.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 3		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
009860	       MOVE SPACES	 TO REP-DETAIL1.

       AB30.
             EXIT.
      *
      *    ****   1 O	C P I	( N O R M A L	P R I N T )
      *
       AB35.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
010010	       MOVE SPACES	 TO REP-DETAIL1.
	     MOVE ZERO		 TO WS-PAGE.

       AB40.
             EXIT.
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
017690 FE00-REPORT     SECTION 5.
017700 FE00.
017710       DISPLAY CLR-SCREEN.
017720       DISPLAY S14.
017740	     MOVE "Y"		 TO WS-OPTION.

017750 FE01.
017760	     ACCEPT WS-OPTION AT 0632
		    WITH FOREGROUND-COLOR 15
017770			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
017830	   IF NOT (WS-OPTION = "Y" OR "N")
017840         GO TO FE01.
017850     IF WS-OPTION = "N"
017860         GO TO FE999.
017720       DISPLAY S14A.
017870	     MOVE "L"		 TO WS-PQNT.

017880 FE02.
017890	     ACCEPT WS-PQNT AT 1126
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-PQNT
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
017960	   IF NOT (WS-PQNT = "A" OR "M" OR "L")
017970         GO TO FE02.
017720       DISPLAY S14B.
014330	     MOVE "S"		 TO WS-PRT.

014340 FE03.
014350	     ACCEPT WS-PRT AT 1341
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-PRT
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
014420	   IF NOT (WS-PRT = "S" OR "A")
014430         GO TO FE03.
014440     IF WS-PRT = "A"
	       PERFORM AB25
	       DISPLAY S15
014470         GO TO FE05.
018010	     MOVE SPACES	 TO WS-DEPT.
             DISPLAY S14C.
	     PERFORM SAVE-SCREEN-3.

018020 FE04.
	     PERFORM RESTORE-SCREEN-3.

       FE04A.
018030	     ACCEPT WS-DEPT AT 1528
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
014500	   IF WS-DEPT = SPACES
014510	       GO TO FE998.
009330	     MOVE WS-DEPT	 TO DPT-CODE.
	   IF (DPT-CODE = "XXXX" OR "YYYY") OR
	      (DPT-SCDE = "VAT")
014590	       GO TO FE04A.
010090       PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       MOVE "Invalid department code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
018080	       GO TO FE04A.
	     DISPLAY DPT-DESC AT 1543
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

       FE04B.
	     MOVE "'C'ontinue or 'R'ekey  [ ]"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     MOVE 17		 TO SLIN.
	     PERFORM OPT-SETUP THRU OPT-EXIT.
	   IF NOT (WS-OPTION = "R" OR "C")
	       GO TO FE04B.
           IF WS-OPTION = "R"
	       GO TO FE04A.
             PERFORM AB25.
017730       DISPLAY S15.
	     GO TO FE10.

018180 FE05.
	     PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       GO TO FE30.
	   IF (DPT-CODE = "XXXX" OR "YYYY") OR
	      (DPT-SCDE = "VAT")
		GO TO FE05.

018280 FE10.
018290	     MOVE 0		 TO WS-S1.
018300     IF WS-PAGE > ZERO
	       MOVE "S"		 TO WS-COMMAND
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND.
018340	     PERFORM FE35.
	     MOVE DPT-CODE	 TO STK-LDG.
	     MOVE "!"		 TO STK-CODE.
018360       PERFORM START-AT-STOCK-LEDG THRU READ-STOCK-EXIT.
018370     IF WS-F-ERROR = 22
018380         GO TO FE25.

018390 FE15.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
018400	   IF (WS-F-ERROR = 22) OR (STK-LDG NOT = DPT-CODE)
018410         GO TO FE25.
	   IF NOT (WS-PRINTER = "S")
	       IF WS-USE-ITM = "Y"
		   DISPLAY S03-ITM
	       ELSE
	       IF WS-EXT-STK = "Y"
		   DISPLAY S03-EXT
	       ELSE
015270		   DISPLAY S03.
	   IF STK-IND > 0
	       GO TO FE15.
018460	   IF (WS-PQNT = "M" AND STK-MTD = ZERO) AND
	      (STK-QUANT > STK-LEVEL)
018470         GO TO FE20.
018480     IF WS-PQNT = "L" AND STK-QUANT > STK-LEVEL
018490         GO TO FE20.
	   IF WS-USE-ITM = "Y"
	       MOVE STK-ITEM	 TO ORD-ITEM
	       MOVE "/" 	 TO ORD-SL
	       MOVE STK-ITM	 TO ORD-ITM
	   ELSE
015280	       MOVE STK-CODE	 TO ORD-EXT-ITEM.
018510	     MOVE STK-BIN	 TO ORD-BIN.
018540	     MOVE STK-DESC	 TO ORD-DESC.
018540	     MOVE STK-DESC2	 TO ORD-DESC2.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
018530	     MOVE SPACES	 TO REP-DETAIL1.
      *
      *    ****   1 7	C P I	( C O N D E N S E D   P R I N T )
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 3		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
009860	       MOVE SPACES	 TO REP-DETAIL1
	       MOVE "D" 	 TO W02-PRN-TYPE.
018550	     MOVE STK-QUANT	 TO ORD-QUANT.
018560	     MOVE STK-LEVEL	 TO ORD-REORD.
018570	     MOVE STK-MTH1	 TO ORD-MTH1.
018580	     MOVE STK-MTH2	 TO ORD-MTH2.
018590	     MOVE STK-MTH3	 TO ORD-MTH3.
018600	     MOVE STK-MAVG	 TO ORD-MAVG.
018610	     MOVE STK-MTD	 TO ORD-MTD.
018620	     MOVE STK-YTD	 TO ORD-YTD.
018630	     MOVE ZERO		 TO WS-ORD WS-VALUE.
018640     IF STK-QUANT NOT > STK-LEVEL
018650	       COMPUTE WS-ORD = (STK-MAVG * STK-LTIME * 2) - STK-QUANT
018660         COMPUTE WS-VALUE = WS-ORD * STK-COST.
018670	     MOVE WS-ORD	 TO ORD-ORDER.
018680	     MOVE WS-VALUE	 TO ORD-COST.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
018730	       PERFORM FE35.
018740	     MOVE SPACES	 TO REP-DETAIL1.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO FE999.

018750 FE20.
018770       GO TO FE15.

018780 FE25.
018790	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
018810	     MOVE "****** END OF DEPARTMENT *****"
				 TO REP-DETAIL1.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
018830	     MOVE SPACES	 TO REP-DETAIL1.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO FE999.
014440     IF WS-PRT = "A"
018840	       GO TO FE05.
	     GO TO FE04.

018850 FE30.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
009980	     MOVE SPACES	 TO REP-DETAIL1.
009990       PERFORM AB35.

018890 FE32.
018900       GO TO FE999.

018910 FE35.
	     MOVE "H"		 TO W02-PRN-TYPE.
018920	     MOVE SPACES	 TO REP-DETAIL1.
018930	     MOVE "Page:"	 TO REP-PG.
018940	     MOVE "Date:"	 TO REP-DAT.
018950	     MOVE W95-COMP	 TO REP-COMP.
018960	     ADD 1		 TO WS-PAGE.
018970	     MOVE WS-PAGE	 TO REP-P-N.
018980	     MOVE W12-TODAY	 TO REP-DT.
018990	     MOVE "STOCK LEVELS AND RE-ORDER REPORT"
				 TO REP-REPORT.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
019010	     MOVE SPACES	 TO REP-DETAIL1.
      *
      *    ****   1 2	C P I	( E L I T E   P R I N T )
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 2		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
012960	       MOVE SPACES	 TO REP-DETAIL1
	       MOVE "H" 	 TO W02-PRN-TYPE.
019020	     MOVE "Category:"	 TO REP-HEAD2.
019030	     MOVE DPT-CODE	 TO REP-CNO.
019040	     MOVE DPT-DESC	 TO REP-CAT.
	     PERFORM CALL-PRINTUTL.
019060	     MOVE SPACES	 TO REP-DETAIL1.
019070	     MOVE "Item Code"	 TO ORD-H1.
019080	     MOVE "Bin" 	 TO ORD-H2.
019140	     MOVE WS-DESC-H1	 TO ORD-H3.
019140	     MOVE WS-DESC-H2	 TO ORD-H3A.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *
      *    ****   1 7	C P I	( C O N D E N S E D   P R I N T )
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 3		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
009860	       MOVE SPACES	 TO REP-DETAIL1
	       MOVE "H" 	 TO W02-PRN-TYPE.
019090	     MOVE ALL "_"	 TO ORD-H6 ORD-H8 ORD-H9A ORD-H9B.
019100	     MOVE " Movement"	 TO ORD-H7.
019110	     MOVE " ReOrder "
				 TO ORD-H9.
	     PERFORM CALL-PRINTUTL.
019130	     MOVE SPACES	 TO REP-DETAIL1.
019150	     MOVE "Quantity"	 TO ORD-H4.
019160	     MOVE "ReOrder"	 TO ORD-H5.
019170	     MOVE "Month 1"	 TO ORD-H10.
019180	     MOVE "Month 2"	 TO ORD-H11.
019190	     MOVE "Month 3"	 TO ORD-H12.
019200	     MOVE "Mth-Avrg"	 TO ORD-H13.
019210	     MOVE "M.T.D."	 TO ORD-H14.
019220	     MOVE "Y.T.D."	 TO ORD-H15.
019230	     MOVE "Quantity"	 TO ORD-H16.
019240	     MOVE "Cost"	 TO ORD-H17.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
019260	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "D"		 TO W02-PRN-TYPE.

       FE998.
018300     IF WS-PAGE > ZERO
014970	       MOVE "****   E N D   O F   R E P O R T   ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.

019270 FE999.
019280       EXIT.
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
055320	     ACCEPT WS-OPTION AT 1238
		    WITH FOREGROUND-COLOR 15 BACKGROUND-COLOR 5.
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
055470	     ACCEPT WS-OPTION AT 1038
		    WITH FOREGROUND-COLOR 15 BACKGROUND-COLOR 5.

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
