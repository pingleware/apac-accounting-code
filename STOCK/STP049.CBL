      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	  ****	      ***    ******    *
      *   **	**     **     **   **	 **  **      ****   **	  **   *
      *   **	       **     **   **	**    **    ** **   **	  **   *
      *    ******      **     ******	**    **   **  **    *******   *
      * 	**     **     **	**    **  ********	  **   *
      *   **	**     **     **	 **  **        **   **	  **   *
      *    ******      **     **	  ****	      ****   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S T O C K	P R I C E   L A B E L	P R O G R A M	       *
      *                                                                *
      *     SPECIAL SPARES VERSION        WEEKLY TOTALS & X-REF        *
      *                                                                *
      *     VERSION 8.13.09 - November 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STP049.
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
003300 77  WS-PQNT	    PIC X(01).
003310 77  WS-WS            PIC X(01) VALUE "N".
003310 77  WS-MEMO          PIC X(01) VALUE "N".
003310 77  WS-PRT           PIC X(01).
003320 77  WS-BIN           PIC X(01).
       77  WS-ETYPE         PIC X(01).
003330 77  WS-OPTION	    PIC X(01).
003330 77  WS-PRICE         PIC X(01).
003330 77  WS-LAB	    PIC	X(01).
       77  WS-NUM	    PIC ZZ9.
003350 77  WS-ERROR         PIC 9(01).
003380 77  WS-KEY1          PIC 9(04)     COMP-5.
003390 77  WS-KSTORE        PIC 9(04)     COMP-5.
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
003470 77  WS-ORD	    PIC S9(05)	  COMP-3.
003480 77  WS-TOTAL	    PIC S9(07)V99 COMP-3.
003490 77  WS-VALUE         PIC S9(07)V99 COMP-3.
003500 77  WS-CCOST         PIC S9(07)V99 COMP-3.
003510 77  WS-CSELL         PIC S9(07)V99 COMP-3.
003520 77  WS-SCOST         PIC S9(07)V99 COMP-3.
003530 77  WS-SSELL         PIC S9(07)V99 COMP-3.
003540 77  WS-TCOST         PIC S9(07)V99 COMP-3.
003550 77  WS-TSELL         PIC S9(07)V99 COMP-3.
003560 77  WS-DEPT          PIC 9(03).
003570 77  WS-WEEK          PIC 9(01).
       77  WS-LABCDE        PIC X(08).
001330 77  WS-DESC-H1	    PIC	 X(14) VALUE "Description".
       77  WS-DESC-H2	    PIC	 X(14) VALUE "Description 2".
       77  WS-RT-HD	    PIC	 X(10) VALUE "-Retail   ".
       77  WS-WS-HD	    PIC	 X(10) VALUE "-Wholesale".
       77  WS-CS-HD	    PIC	 X(10) VALUE "-Cash sale".
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
	   03  WS-PROG	       PIC  X(03) VALUE "049".

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
	   03  LAB-L1 REDEFINES W02-PRN-LINE.
	       05  LAB-HEAD    PIC  X(06).
	       05  LAB-DESC    PIC  X(74).
	   03  LAB-L2 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(02).
	       05  LAB-ITEM    PIC  X(18).
	       05  FILLER      PIC  X(60).
	   03  LAB-L3 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(06).
	       05  LAB-COST    PIC  X(08).
	       05  FILLER      PIC  X(66).
	   03  LAB-L4 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(03).
	       05  LAB-SELL    PIC  Z(04)9.99 BLANK WHEN ZERO.
	       05  FILLER      PIC  X(69).
	   03  LB2-L1 REDEFINES W02-PRN-LINE.
	       05  LB2-DET OCCURS 3.
		   07  LB2-DATA.
		       09  LB2-H   PIC	X(16).
		       09  LB2-PRC PIC	ZZZZ9.99 BLANK WHEN ZERO.
		   07  FILLER	   PIC	X(04).
	   03  LB3-L1 REDEFINES W02-PRN-LINE.
	       05  LB3-DET OCCURS 4.
		   07  LB3-DATA.
		       09  LB3-H   PIC	X(10).
		       09  LB3-PRC PIC	ZZZZZZ9.99 BLANK WHEN ZERO.
		       09  FILLER  PIC	X(01).
		   07  FILLER	   PIC	X(04).
	   03  LB4-L1 REDEFINES W02-PRN-LINE.
	       05  LB4-DET OCCURS 3.
		   07  LB4-DATA.
		       09  LB4-CODE
				   PIC	X(16).
		   07  FILLER	   PIC	X(01).
	   03  LB4-L2 REDEFINES W02-PRN-LINE.
	       05  LB4-DATA2 OCCURS 3.
		   07  LB4-DESC    PIC	X(32).
		   07  FILLER	   PIC	X(02).
	   03  LB4-L3 REDEFINES W02-PRN-LINE.
	       05  LB4-DATA3 OCCURS 3.
		   07  LB4-H	   PIC	X(13).
		   07  LB4-LABCDE.
		       09  LB4-PRC PIC	ZZZZ9.99.
		   07  FILLER	   PIC	X(03).
		   07  LB4-DTE	   PIC	9(06).
		   07  FILLER	   PIC	X(04).
	   03  SAPDC-L1 REDEFINES W02-PRN-LINE.
	       05  SAPDC-DET.
		   07  SAPDC-CDE1  PIC	X(14).
		   07  FILLER	   PIC	X(04).
		   07  SAPDC-CDE2  PIC	X(14).
		   07  FILLER	   PIC	X(03).
		   07  SAPDC-CDE3  PIC	X(14).
		   07  FILLER	   PIC	X(04).
		   07  SAPDC-CDE4  PIC	X(14).
		   07  FILLER	   PIC	X(03).
	   03  SAPDC-L2 REDEFINES W02-PRN-LINE.
	       05  SAPDC-DATA2 OCCURS 4.
		   07  SAPDC-DESC  PIC	X(27).
		   07  FILLER	   PIC	X(08).
	   03  SAPDC-L3 REDEFINES W02-PRN-LINE.
	       05  SAPDC-DATA3 OCCURS 4.
		   07  SAPDC-H	   PIC	X(18).
		   07  SAPDC-PRC   PIC	ZZZZ9.99.
		   07  FILLER	   PIC	X(09).
	   03  SAPDC-L4 REDEFINES W02-PRN-LINE.
	       05  SAPDC-DET.
		   07  FILLER	   PIC	X(05).
		   07  SAPDC-PRC1  PIC	ZZZZ9.99.
		   07  FILLER	   PIC	X(10).
		   07  SAPDC-PRC2  PIC	ZZZZ9.99.
		   07  FILLER	   PIC	X(09).
		   07  SAPDC-PRC3  PIC	ZZZZ9.99.
		   07  FILLER	   PIC	X(10).
		   07  SAPDC-PRC4  PIC	ZZZZ9.99.
	       05  FILLER      PIC  X(04).
	   03  LB5-L1 REDEFINES W02-PRN-LINE.
	       05  LB5-DET OCCURS 4.
		   07  LB5-CODE    PIC	X(16).
		   07  FILLER	   PIC	X(01).
	   03  LB5-L2 REDEFINES W02-PRN-LINE.
	       05  LB5-DATA2 OCCURS 4.
		   07  LB5-DESC    PIC	X(30).
		   07  FILLER	   PIC	X(03).
	   03  LB5-L3 REDEFINES W02-PRN-LINE.
	       05  LB5-DATA3 OCCURS 4.
		   07  LB5-H	   PIC	X(18).
		   07  LB5-PRC	   PIC	ZZZZ9.99.
		   07  FILLER	   PIC	X(07).
	   03  LB5-L4 REDEFINES W02-PRN-LINE.
	       05  LB5-DATA4 OCCURS 4.
		   07  LB5-COST    PIC	X(08).
		   07  FILLER	   PIC	X(06).
		   07  LB5-LAB-SUPP.
		       09  LB5-LABCDE	PIC  X(08).
		       09  LB5-DTE	PIC  X(06).
		   07  LB5-LBSP REDEFINES LB5-LAB-SUPP.
		       09  LB5-H2	PIC  X(06).
		       09  LB5-SUPP	PIC  X(06).
		   07  FILLER	   PIC	X(05).
	   03  LB6-L1 REDEFINES W02-PRN-LINE.
	       05  LB6-DET OCCURS 4.
		   07  LB6-CODE    PIC	X(17).
		   07  FILLER	   PIC	X(01).
	   03  LB6-L2 REDEFINES W02-PRN-LINE.
	       05  LB6-DATA2 OCCURS 4.
		   07  LB6-DESC.
		       09  FILLER  PIC	X(28).
		       09  LB6-SPC PIC	X(02).
		   07  FILLER	   PIC	X(06).
	   03  LB6-L3 REDEFINES W02-PRN-LINE.
	       05  LB6-DATA3 OCCURS 4.
		   07  LB6-H	   PIC	X(18).
		   07  LB6-PRC	   PIC	ZZZZ9.99.
		   07  FILLER	   PIC	X(10).
	   03  LB6-L4 REDEFINES W02-PRN-LINE.
	       05  LB6-DATA4 OCCURS 4.
		   07  LB6-COST    PIC	X(08).
		   07  FILLER	   PIC	X(06).
		   07  LB6-LAB-SUPP.
		       09  LB6-LABCDE	PIC  X(08).
		       09  LB6-DTE	PIC  X(06).
		   07  LB6-LBSP REDEFINES LB6-LAB-SUPP.
		       09  LB6-H2	PIC  X(06).
		       09  LB6-SUPP	PIC  X(06).
		       09  FILLER	PIC  X(02).
		   07  FILLER	   PIC	X(08).

003740 01  W05.
003750     03  W05-S OCCURS 300   PIC 9(04)     COMP-5.

       COPY W05.VAT.

001270 01  W08-EXPD.
	   03  W08-CHAR        PIC  X(06).
001280	   03  W08-DEC	       PIC  X(01) COMP-X OCCURS 6.

       COPY W10.STK.

       COPY W12.WS.

       01  W15-LABELS.
           03  W15-DEPTH          OCCURS 2.
               05  W15-WIDTH      OCCURS 3.
		   07  W15-ITM	  PIC  X(18).
                   07  W15-DESC   PIC  X(30).
                   07  W15-PRC    PIC  9(05)V99.
                   07  W15-SUPP   PIC  X(05).
       01  W15-LAB-2 REDEFINES W15-LABELS.
           03  W15-LABS OCCURS 4.
	       05  W15-ITEM	  PIC  X(18).
               05  W15-DSC        PIC  X(22).
               05  W15-PRICE      PIC  9(05)V99.
               05  W15-SUPP2      PIC  X(05).
       01  W15-LAB-3 REDEFINES W15-LABELS.
           03  W15-LAB4 OCCURS 3.
	       05  W15-ITM4	  PIC  X(18).
               05  W15-DSC4       PIC  X(30).
               05  W15-PRC4       PIC  9(05)V99.
               05  W15-SUPP4      PIC  X(05).
	       05  W15-COST4	  PIC  X(08).
	       05  W15-INV4.
		   07  W15-INVN4  PIC  Z(08).
       01  W15-SAPDC REDEFINES W15-LABELS.
	   03  W15-LAB5 OCCURS 4.
	       05  W15-ITM5	  PIC  X(18).
	       05  W15-DSC5	  PIC  X(30).
               05  W15-PRC5       PIC  9(05)V99.
               05  W15-SUPP5      PIC  X(05).
	       05  W15-COST	  PIC  X(08).
	       05  W15-INV.
		   07  W15-INVN	  PIC  Z(08).

       01  W20-LABEL.
	   03  W20-CODE 	  PIC  X(18).
           03  W20-DESC           PIC  X(30).
           03  W20-COST.
	       05  W20-CST	  PIC  9(06)V99.
           03  W20-SELL           PIC  9(05)V99.
           03  W20-NO             PIC  9(05)    COMP-0.
           03  W20-SUPP           PIC  X(05).

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
	   03  W35-CPRNT REDEFINES W35-CPRINT.
	       05  FILLER	  PIC  X(02).
	       05  W35-LCST	  PIC  X(08).
003920     03  W35-S1             PIC 9(02) COMP-5.
003930     03  W35-S2             PIC 9(02) COMP-5.
003930	   03  W35-S3		  PIC 9(02) COMP-5.

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

000850 01  S-PROG.
004170	   03  LINE  2 COLUMN 31 FOREGROUND-COLOR 15
004180                           VALUE "STOCK LABELS CONTROL".
       01  S-MENU.
	   03  LINE  4 COLUMN 29 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 5 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 51 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  5 COLUMN 29 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 5 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "PRICE LABEL FORMATS".
	   03	       COLUMN 50 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 0
				 VALUE " ³".
	   03  LINE  6 COLUMN 29 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 5 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 32 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7
				 VALUE " Format 1          ".
	   03	       COLUMN 51 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "³".
	   03  LINE  7 COLUMN 29 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 5 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 32 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7
				 VALUE " Format 2          ".
	   03	       COLUMN 51 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "³".
001470	   03  LINE  8 COLUMN 29 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 5 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 32 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7
				 VALUE " Format 3          ".
	   03	       COLUMN 51 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "³".
001470	   03  LINE  9 COLUMN 29 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 5 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 32 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7
				 VALUE " Format 4          ".
	   03	       COLUMN 51 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "³".
001470	   03  LINE 10 COLUMN 29 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 5 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 32 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7
				 VALUE " Format 5          ".
	   03	       COLUMN 51 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "³".
001470	   03  LINE 11 COLUMN 29 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 5 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 32 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7
				 VALUE " Format 6          ".
	   03	       COLUMN 51 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "³".
001470	   03  LINE 12 COLUMN 29 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 5 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "7".
	   03	       COLUMN 32 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7
				 VALUE " Format 7          ".
	   03	       COLUMN 51 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "³".
001470	   03  LINE 13 COLUMN 29 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 5 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "8".
	   03	       COLUMN 32 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7
				 VALUE " Format 8 (SAPDC)  ".
	   03	       COLUMN 51 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "³".
001470	   03  LINE 14 COLUMN 29 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 5 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "9".
	   03	       COLUMN 32 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7
				 VALUE " Format 9          ".
	   03	       COLUMN 51 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "³".
001470	   03  LINE 15 COLUMN 29 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 5 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 32 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7
				 VALUE " Exit              ".
	   03	       COLUMN 51 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "³".
	   03  LINE 16 COLUMN 29 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 5 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 30 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

004470 01  S04.
004480	   03  LINE 10 COLUMN 20 FOREGROUND-COLOR 14
004490                           VALUE "Busy loading stock departments".

       01  S18.
           03  LINE  2 COLUMN 35 FOREGROUND-COLOR 15
                                 VALUE "PRICE LABELS".
017270     03  LINE  4 COLUMN 12 FOREGROUND-COLOR 14 VALUE 
                                 "Busy printing Stock Price Labels".
	   03  LINE  6 COLUMN 12 VALUE "Are you sure (".
	   03	       COLUMN 26 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Y".
	   03	       COLUMN 27 VALUE "/".
	   03	       COLUMN 28 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "N".
	   03	       COLUMN 29 VALUE ")?".

       01  S19.
	   03  LINE 12 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZZZ9 USING W20-NO AUTO.

004870 COPY S99.CRT.

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
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.
	     PERFORM BE00S.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

       AA49.
             EXIT PROGRAM.

       COPY AA50.LUP.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP8.

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

      /
       BE00S	       SECTION.
       BE62.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 1		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
008660	     DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 31		 TO SHADE-COL.
	     MOVE 21		 TO SHADE-WIDTH.
	     MOVE 12		 TO SHADE-LINES.
	     DISPLAY S-MENU.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
       BE63.
	     MOVE 5		 TO TOP-ROW.
	     MOVE 14		 TO BOTTOM-ROW.
	     MOVE 4		 TO SCREEN-LIN.
	     MOVE 31		 TO SCREEN-COL.
	     MOVE 19		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

008690 BE64.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

006660 BE65.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO BE66
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO BE65
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO BE65
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO BE65
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO BE65
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO BE66
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BE64
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO BE68
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BE64
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF (WS-OPTION < "0" OR > "9")
		   CALL X"E5"
		   GO TO BE64
	       END-IF
	       GO TO BE67
	   ELSE
	       CALL X"E5"
	       GO TO BE64.

       BE66.
	   IF SCREEN-LIN < 5
	       CALL X"E5"
	       GO TO BE64
	   ELSE
	   IF SCREEN-LIN = 5
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 6
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 7
	       MOVE "3"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 8
	       MOVE "4"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "5"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 10
	       MOVE "6"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 11
	       MOVE "7"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 12
	       MOVE "8"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 13
	       MOVE "9"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 14
	       MOVE "0"		 TO WS-OPTION.
	   GO TO BE68.

       BE67.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 14		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "1"
	       MOVE 5		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE 6		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE 7		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE 8		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE 10		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "7"
	       MOVE 11		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "8"
	       MOVE 12		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "9"
	       MOVE 13		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       BE68.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
008760	   IF WS-OPTION = "0"
008770	       GO TO BE999.
008780	   IF (WS-OPTION < "1" OR > "9")
	       GO TO BE64.
             MOVE WS-OPTION      TO WS-LAB.
008780	   IF WS-OPTION = "1" OR "9"
               PERFORM IE000
008790	       GO TO BE999.
008800     IF WS-OPTION = "2"
               PERFORM JE000
008810	       GO TO BE999.
008800     IF WS-OPTION = "3"
               PERFORM KE000
008810	       GO TO BE999.
008800     IF WS-OPTION = "4"
               PERFORM NE000
008810	       GO TO BE999.
008800	   IF WS-OPTION = "5"
	       PERFORM PE000
008810	       GO TO BE999.
008800	   IF WS-OPTION = "6"
	       PERFORM LE000
008810	       GO TO BE999.
008800	   IF WS-OPTION = "7"
	       PERFORM QE000
008810	       GO TO BE999.
008800	   IF WS-OPTION = "8"
               PERFORM OE000
008810	       GO TO BE999.
             GO TO BE63.
       BE999.
	     EXIT.

       COPY CA000.STK.

       COPY CA100.STK.

       COPY CA200.STK.

       COPY CA500.STK.

      /
018610 IE000	       SECTION 50.
018620 IE00.
018630       DISPLAY CLR-SCREEN.
             DISPLAY S18.
       IE05.
	     ACCEPT WS-OPTION AT 0632
		    WITH FOREGROUND-COLOR 15 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "N" OR "Y")
               GO TO IE05.
           IF WS-OPTION = "N"
               GO TO IE999.
	     DISPLAY "Omit price from label " AT 0812
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE "N"		 TO WS-PRICE.
       IE06.
	     ACCEPT WS-PRICE AT 0839
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-PRICE
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-PRICE = "N" OR "Y")
               GO TO IE06.
	     PERFORM SAVE-SCREEN-3.
	     MOVE W43-SCREEN3	 TO W44-SCREEN1.
	     MOVE W42-ATTRIB3	 TO W44-ATTRIB1.

       IE08.
	     DISPLAY "Item code" AT 1012.
	     MOVE SPACES	 TO W10-EXT-ITEM.

       IE10.
             PERFORM AA300.
021070	     ACCEPT W10-EXT-ITEM AT 1023
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
                     GO TO IE999
                 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO IE10
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
                     GO TO IE10
               END-EVALUATE
	       PERFORM AA50-LOOKUP
	       DISPLAY W10-EXT-ITEM AT 1023
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 5
	       IF W10-EXT-ITEM = SPACES
                   GO TO IE10.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
015830	   IF W10-EXT-ITEM = SPACE
015840         GO TO IE999.
015850       PERFORM CA000.
015860     IF WS-F-ERROR NOT = ZERO
	       MOVE "Invalid code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO IE10.
	     DISPLAY STK-DESC AT 1043
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF STK-IND = 4
	       MOVE "Header Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO IE10.
	     PERFORM CHECK-CORRECT.
011790	   IF WS-OPTION = "N"
011800         GO TO IE10.
	     DISPLAY "Number of Labels:" AT 1212.
	     MOVE ZERO		 TO W20-NO.

       IE15.
             ACCEPT S19.
           IF W20-NO = ZERO
               GO TO IE25.
	     PERFORM CHECK-CORRECT.
011790	   IF WS-OPTION = "N"
011800         GO TO IE15.
	     MOVE STK-CODE	 TO W20-CODE.
	     MOVE STK-DESC	 TO W20-DESC.
	     MOVE STK-COST	 TO W35-CST.
	     MOVE STK-SUPP	 TO W20-SUPP.
009540	     MOVE W35-WHOLE	 TO W35-CEDIT.
009550	     MOVE 3		 TO W35-S3.

009560 IE16.
	   IF W35-S3 < 6
	       IF W35-CHAR (W35-S3) = SPACE
		   MOVE SPACE	 TO W35-CHAR (W35-S3)
		   ADD 1	 TO W35-S3
		   GO TO IE16.
009570	   IF (W35-CHAR (W35-S3) NOT NUMERIC) OR
009590	      (W35-NUM (W35-S3) = 0)
009600	       MOVE 10		 TO W35-S2
009610     ELSE
009620	       MOVE W35-NUM (W35-S3)
				 TO W35-S2.
009630	     MOVE W30-CODE (W35-S2)
				 TO W35-CHAR (W35-S3).
009650	   IF W35-S3 < 10
009660	       ADD 1		 TO W35-S3
009670         GO TO IE16.
	     MOVE W35-LCST	 TO W20-COST.
	     MOVE W10-VSELL	 TO W20-SELL.
       IE20.
           IF W20-NO < 1
	       GO TO IE25.
	     MOVE SPACES	 TO REP-LINE1.
	   IF WS-LAB = "9"
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
	     MOVE "Item:"	 TO LAB-HEAD.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *
      *      S E T   E X P A N D E D   P R I N T
      *
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO REP-LINE1.
	     MOVE W20-CODE	 TO LAB-ITEM.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO REP-LINE1.
           IF WS-PRICE = "N"
	       MOVE "Prod:"	 TO LAB-HEAD
	       MOVE W20-COST	 TO LAB-COST.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-LINE1.
	     MOVE "Desc:"	 TO LAB-HEAD.
	     MOVE W20-DESC	 TO LAB-DESC.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-LINE1.
           IF WS-PRICE = "N"
	       MOVE "Price"	 TO LAB-HEAD
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
      *
      *      S E T   E X P A N D E D   P R I N T
      *
	       MOVE "E"		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
	       MOVE SPACES	 TO REP-LINE1
               MOVE W20-SELL TO LAB-SELL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-LINE1.
	   IF WS-PRICE = "N"
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
	       MOVE 0		 TO WS-ADVANCE
	       MOVE "X"		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-LINE1.
           IF NOT (W20-SUPP = SPACES OR LOW-VALUES)
	       MOVE "Supp:"	 TO LAB-HEAD
               MOVE W20-SUPP     TO LAB-DESC.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-LINE1.
	   IF WS-LAB = "9"
	       PERFORM CALL-PRINTUTL.
	     PERFORM PRINT-ABORT.

       IE24.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014990	       GO TO IE999.
	     SUBTRACT 1 	 FROM W20-NO.
             GO TO IE20.

       IE25.
	     MOVE "'E'xit / 'N'ext Label  [N]"
				 TO WS-ERR-MES.
	     MOVE "N"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.

       IE30.
011790	   IF NOT (WS-OPTION = "E" OR "N")
	       GO TO IE25.
011790     IF WS-OPTION = "N"
               MOVE 1001 TO CPOS
               PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23
	       MOVE W44-SCREEN1	 TO W43-SCREEN3
	       MOVE W44-ATTRIB1	 TO W42-ATTRIB3
	       PERFORM RESTORE-SCREEN-3
011800	       GO TO IE08.

018760 IE999.
018770       EXIT.

018610 JE000	       SECTION 51.
018620 JE00.
018630       DISPLAY CLR-SCREEN.
             DISPLAY S18.

       JE01.
             MOVE 1              TO W35-S1 W35-S2.
             INITIALIZE W15-LABELS.

       JE05.
	     ACCEPT WS-OPTION AT 0632
		    WITH FOREGROUND-COLOR 15 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "N" OR "Y")
               GO TO JE05.
           IF WS-OPTION = "N"
               GO TO JE999.
	     DISPLAY "Omit price from label " AT 0812
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE "N"		 TO WS-PRICE.

       JE06.
	     ACCEPT WS-PRICE AT 0839
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-PRICE
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-PRICE = "N" OR "Y")
               GO TO JE06.
	     PERFORM SAVE-SCREEN-3.
	     MOVE W43-SCREEN3	 TO W44-SCREEN1.
	     MOVE W42-ATTRIB3	 TO W44-ATTRIB1.

       JE08.
	     DISPLAY "Item code" AT 1012.
	     MOVE SPACES	 TO W10-EXT-ITEM.

       JE10.
             PERFORM AA300.
021070	     ACCEPT W10-EXT-ITEM AT 1023
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
                     GO TO JE25
                 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO JE10
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
                     GO TO JE10
               END-EVALUATE
	       PERFORM AA50-LOOKUP
	       DISPLAY W10-EXT-ITEM AT 1023
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 5
	       IF W10-EXT-ITEM = SPACES
                   GO TO JE10.
015830	   IF W10-EXT-ITEM = SPACE
015840         GO TO JE25.
015850       PERFORM CA000.
015860     IF WS-F-ERROR NOT = ZERO
	       MOVE "Invalid code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO JE10.
	     DISPLAY STK-DESC AT 1043
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF STK-IND = 4
	       MOVE "Header Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO JE10.
	     PERFORM CHECK-CORRECT.
011790	   IF WS-OPTION = "N"
011800         GO TO JE10.
	     DISPLAY "Number of Labels:" AT 1212.
	     MOVE ZERO		 TO W20-NO.

       JE15.
             ACCEPT S19.
           IF W20-NO = ZERO
               GO TO JE25.
	     PERFORM CHECK-CORRECT.
011790	   IF WS-OPTION = "N"
011800         GO TO JE15.

       JE16.
           IF W20-NO < 1
               GO TO JE25.
             MOVE STK-CODE       TO W15-ITM (W35-S1, W35-S2).
             MOVE STK-DESC       TO W15-DESC (W35-S1, W35-S2).
             MOVE STK-SUPP       TO W15-SUPP (W35-S1, W35-S2).
           IF WS-PRICE = "N"
               MOVE W10-VSELL    TO W15-PRC (W35-S1, W35-S2)
           ELSE
               MOVE ZERO         TO W15-PRC (W35-S1, W35-S2).
             SUBTRACT 1          FROM W20-NO.
           IF W35-S1 < 2
               ADD 1             TO W35-S1
               GO TO JE16.
           IF W35-S2 < 3
               MOVE 1            TO W35-S1
               ADD 1             TO W35-S2
               GO TO JE16.
             PERFORM JE18 THRU JE23.
           IF WS-OPTION = "Y"
014990         GO TO JE999.
             GO TO JE16.

       JE18.
             MOVE 1              TO W35-S1.

       JE19.
             MOVE 1              TO W35-S2.
	     MOVE SPACES	 TO REP-LINE1.

       JE20.
           IF W15-ITM (W35-S1, W35-S2) NOT = SPACES
               MOVE W15-ITM (W35-S1, W35-S2)
                                 TO LB2-DATA (W35-S2).
           IF W35-S2 < 3
               ADD 1             TO W35-S2
               GO TO JE20.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM JE19.

       JE21.
           IF W15-ITM (W35-S1, W35-S2) NOT = SPACES
               MOVE W15-DESC (W35-S1, W35-S2)
                                 TO LB2-DATA (W35-S2).
           IF W35-S2 < 3
               ADD 1             TO W35-S2
               GO TO JE21.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM JE19.

       JE22.
           IF W15-ITM (W35-S1, W35-S2) NOT = SPACES
               MOVE "VAT inclusive"
                                 TO LB2-H (W35-S2)
               MOVE W15-PRC (W35-S1, W35-S2)
                                 TO LB2-PRC (W35-S2).
           IF W35-S2 < 3
               ADD 1             TO W35-S2
               GO TO JE22.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM JE19.

       JE23.
           IF NOT (W15-SUPP (W35-S1, W35-S2) = SPACES OR LOW-VALUES)
               MOVE W15-SUPP (W35-S1, W35-S2)
                                 TO LB2-DATA (W35-S2).
           IF W35-S2 < 3
               ADD 1             TO W35-S2
               GO TO JE23.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM JE19.
           IF W35-S1 < 2
               ADD 1             TO W35-S1
               GO TO JE20.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM JE19.

       JE23E.
             PERFORM PRINT-ABORT.

       JE24.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014990	       GO TO JE999.
             PERFORM JE01.

       JE25.
	     MOVE "'E'xit / 'N'ext Label  [N]"
				 TO WS-ERR-MES.
	     MOVE "N"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.

       JE30.
011790	   IF NOT (WS-OPTION = "E" OR "N")
	       GO TO JE25.
011790     IF WS-OPTION = "N"
               MOVE 1001 TO CPOS
               PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23
	       MOVE W44-SCREEN1	 TO W43-SCREEN3
	       MOVE W44-ATTRIB1	 TO W42-ATTRIB3
	       PERFORM RESTORE-SCREEN-3
011800	       GO TO JE08.
           IF (W35-S1 > 1) OR (W35-S2 > 1)
	       PERFORM JE18 THRU JE23.

018760 JE999.
018770       EXIT.
      /
018610 KE000	       SECTION 52.
018620 KE00.
      *
      *    ****    1 2	 C P I
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO REP-LINE1.
018630	     DISPLAY CLR-SCREEN.
             DISPLAY S18.

       KE01.
             MOVE 1              TO W35-S1.
             INITIALIZE W15-LAB-2.

       KE05.
	     ACCEPT WS-OPTION AT 0632
		    WITH FOREGROUND-COLOR 15 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "N" OR "Y")
               GO TO KE05.
           IF WS-OPTION = "N"
               GO TO KE999.
	     DISPLAY "Omit price from label " AT 0812
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE "N"		 TO WS-PRICE.

       KE06.
	     ACCEPT WS-PRICE AT 0839
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-PRICE
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-PRICE = "N" OR "Y")
               GO TO KE06.
	     PERFORM SAVE-SCREEN-3.
	     MOVE W43-SCREEN3	 TO W44-SCREEN1.
	     MOVE W42-ATTRIB3	 TO W44-ATTRIB1.

       KE08.
	     DISPLAY "Item code" AT 1012.
	     MOVE SPACES	 TO W10-EXT-ITEM.

       KE10.
             PERFORM AA300.
021070	     ACCEPT W10-EXT-ITEM AT 1023
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
                     GO TO KE25
                 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO KE10
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
                     GO TO KE10
               END-EVALUATE
	       PERFORM AA50-LOOKUP
	       DISPLAY W10-EXT-ITEM AT 1023
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 5
	       IF W10-EXT-ITEM = SPACES
                   GO TO KE10.
015830	   IF W10-EXT-ITEM = SPACE
015840         GO TO KE25.
015850       PERFORM CA000.
015860     IF WS-F-ERROR NOT = ZERO
	       MOVE "Invalid code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO KE10.
	     DISPLAY STK-DESC AT 1043
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF STK-IND = 4
	       MOVE "Header Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO KE10.
	     PERFORM CHECK-CORRECT.
011790	   IF WS-OPTION = "N"
011800         GO TO KE10.
             DISPLAY "Number of Labels:" AT 1212.
	     MOVE ZERO		 TO W20-NO.

       KE15.
             ACCEPT S19.
           IF W20-NO = ZERO
               GO TO KE25.
	     PERFORM CHECK-CORRECT.
011790	   IF WS-OPTION = "N"
011800         GO TO KE15.

       KE16.
           IF W20-NO < 1
               GO TO KE25.
             MOVE STK-CODE       TO W15-ITEM (W35-S1).
             MOVE STK-DESC       TO W15-DSC (W35-S1).
             MOVE STK-SUPP       TO W15-SUPP2 (W35-S1).
	   IF WS-PRICE = "N"
               MOVE W10-VSELL    TO W15-PRICE (W35-S1)
           ELSE
               MOVE ZERO         TO W15-PRICE (W35-S1).
             SUBTRACT 1          FROM W20-NO.
           IF W35-S1 < 4
               ADD 1             TO W35-S1
               GO TO KE16.
             PERFORM KE18 THRU KE23.
           IF WS-OPTION = "Y"
014990         GO TO KE35.
             GO TO KE16.

       KE18.
             MOVE 1              TO W35-S1.
	     MOVE SPACES	 TO REP-LINE1.

       KE20.
           IF W15-ITEM (W35-S1) NOT = SPACES
               MOVE W15-ITEM (W35-S1)
                                 TO LB3-DATA (W35-S1).
           IF W35-S1 < 4
               ADD 1             TO W35-S1
               GO TO KE20.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KE18.
       KE21.
           IF W15-ITEM (W35-S1) NOT = SPACES
               MOVE W15-DSC (W35-S1)
                                 TO LB3-DATA (W35-S1).
           IF W35-S1 < 4
               ADD 1             TO W35-S1
               GO TO KE21.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KE18.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.

       KE22.
           IF W15-ITEM (W35-S1) NOT = SPACES
	       IF WS-PRICE = "N"
                   MOVE "VAT incl"   
                                 TO LB3-H (W35-S1)
                   MOVE W15-PRICE (W35-S1)
                                 TO LB3-PRC (W35-S1).
           IF W35-S1 < 4
               ADD 1             TO W35-S1
               GO TO KE22.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KE18.

       KE23.
           IF NOT (W15-SUPP2 (W35-S1) = SPACES OR LOW-VALUES)
               MOVE W15-SUPP2 (W35-S1)
                                 TO LB3-DATA (W35-S1).
           IF W35-S1 < 4
               ADD 1             TO W35-S1
               GO TO KE23.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KE18.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KE01.

       KE23E.
             PERFORM PRINT-ABORT.

       KE24.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014990	       GO TO KE35.

       KE25.
	     MOVE "'E'xit / 'N'ext Label  [N]"
				 TO WS-ERR-MES.
	     MOVE "N"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.

       KE30.
011790	   IF NOT (WS-OPTION = "E" OR "N")
	       GO TO KE25.
011790     IF WS-OPTION = "N"
               MOVE 1001 TO CPOS
               PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23
	       MOVE W44-SCREEN1	 TO W43-SCREEN3
	       MOVE W44-ATTRIB1	 TO W42-ATTRIB3
	       PERFORM RESTORE-SCREEN-3
011800	       GO TO KE08.
           IF W35-S1 > 1
	       PERFORM KE18 THRU KE23.

       KE35.
      *
      *    ****    1 0	 C P I
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.

018760 KE999.
018770       EXIT.
      /
      *    ****    F O R M A T	 6   -	 P R I C E   L A B E L S
      *
018610 LE000	       SECTION 54.
018620 LE00.
      *
      *    ****    1 2	 C P I
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO REP-LINE1.
018630	     DISPLAY CLR-SCREEN.
             DISPLAY S18.

       LE01.
             MOVE 1              TO W35-S1.
             INITIALIZE W15-LAB-3.

       LE05.
	     ACCEPT WS-OPTION AT 0632
		    WITH FOREGROUND-COLOR 15 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "N" OR "Y")
	       GO TO LE05.
           IF WS-OPTION = "N"
	       GO TO LE999.
	     DISPLAY "Omit price from label " AT 0812
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE "N"		 TO WS-PRICE.

       LE06.
	     ACCEPT WS-PRICE AT 0839
		    WITH FOREGROUND-COLOR 15
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-PRICE
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-PRICE = "N" OR "Y")
	       GO TO LE06.
	     PERFORM SAVE-SCREEN-3.
	     MOVE W43-SCREEN3	 TO W44-SCREEN1.
	     MOVE W42-ATTRIB3	 TO W44-ATTRIB1.

       LE08.
	     DISPLAY "Item code" AT 1012.
	     MOVE SPACES	 TO W10-EXT-ITEM.

       LE10.
             PERFORM AA300.
021070	     ACCEPT W10-EXT-ITEM AT 1023
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
		     GO TO LE25
                 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO LE10
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
		     GO TO LE10
               END-EVALUATE
	       PERFORM AA50-LOOKUP
	       DISPLAY W10-EXT-ITEM AT 1023
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 5
	       IF W10-EXT-ITEM = SPACES
		   GO TO LE10.
015830	   IF W10-EXT-ITEM = SPACE
015840	       GO TO LE25.
015850       PERFORM CA000.
015860     IF WS-F-ERROR NOT = ZERO
	       MOVE "Invalid code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO LE10.
	     DISPLAY STK-DESC AT 1043
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF STK-IND = 4
	       MOVE "Header Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO LE10.
	     PERFORM CHECK-CORRECT.
011790	   IF WS-OPTION = "N"
011800	       GO TO LE10.
             DISPLAY "Number of Labels:" AT 1212.
	     MOVE ZERO		 TO W20-NO.

       LE15.
             ACCEPT S19.
           IF W20-NO = ZERO
	       GO TO LE25.
	     PERFORM CHECK-CORRECT.
011790	   IF WS-OPTION = "N"
011800	       GO TO LE15.

       LE16.
           IF W20-NO < 1
	       GO TO LE25.
             MOVE STK-CODE       TO W15-ITM4 (W35-S1).
             MOVE STK-DESC       TO W15-DSC4 (W35-S1).
             MOVE STK-SUPP       TO W15-SUPP4 (W35-S1).
           IF WS-PRICE = "N"
               MOVE W10-VSELL    TO W15-PRC4 (W35-S1)
           ELSE
               MOVE ZERO         TO W15-PRC4 (W35-S1).
	     MOVE STK-COST	 TO W35-CST.
009540	     MOVE W35-WHOLE	 TO W35-CEDIT.
009550	     MOVE 3		 TO W35-S3.

009560 LE16A.
	   IF W35-S3 < 6
	       IF W35-CHAR (W35-S3) = SPACE
		   MOVE SPACE	 TO W35-CHAR (W35-S3)
		   ADD 1	 TO W35-S3
		   GO TO LE16A.
009570	   IF (W35-CHAR (W35-S3) NOT NUMERIC) OR
009590	      (W35-NUM (W35-S3) = 0)
009600	       MOVE 10		 TO W35-S2
009610     ELSE
009620	       MOVE W35-NUM (W35-S3)
				 TO W35-S2.
009630	     MOVE W30-CODE (W35-S2)
				 TO W35-CHAR (W35-S3).
009650	   IF W35-S3 < 10
009660	       ADD 1		 TO W35-S3
009670	       GO TO LE16A.
	     MOVE W35-LCST	 TO W20-COST.
	     MOVE W20-COST	 TO W15-COST4 (W35-S1).
             SUBTRACT 1          FROM W20-NO.
           IF W35-S1 < 3
               ADD 1             TO W35-S1
	       GO TO LE16.
	     PERFORM LE17 THRU LE24A.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014990	       GO TO LE35.
	     GO TO LE16.

       LE17.
      *
      *      S E T   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.

       LE18.
             MOVE 1              TO W35-S1.
	     MOVE SPACES	 TO REP-LINE1.

       LE20.
           IF W15-ITM4 (W35-S1) NOT = SPACES
               MOVE W15-ITM4 (W35-S1)
				 TO LB5-CODE (W35-S1).
           IF W35-S1 < 3
               ADD 1             TO W35-S1
	       GO TO LE20.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     PERFORM LE18.

       LE21.
           IF W15-ITM4 (W35-S1) NOT = SPACES
               MOVE W15-DSC4 (W35-S1)
				 TO LB5-DESC (W35-S1).
           IF W35-S1 < 3
               ADD 1             TO W35-S1
	       GO TO LE21.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM LE18.

       LE22.
	   IF W15-ITM4 (W35-S1) NOT = SPACES
               IF WS-PRICE = "N"
                   MOVE "VAT incl"
				 TO LB5-H (W35-S1)
		   MOVE W15-PRC4 (W35-S1)
				 TO LB5-PRC (W35-S1).
	   IF W35-S1 < 3
               ADD 1             TO W35-S1
	       GO TO LE22.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM LE18.

       LE24.
	   IF W15-ITM4 (W35-S1) NOT = SPACES
	       MOVE W15-COST4 (W35-S1)
				 TO LB5-COST (W35-S1)
	       MOVE WS-LABCDE	 TO LB5-LABCDE (W35-S1)
	       MOVE W12-MMYY	 TO LB5-DTE (W35-S1).
	   IF W35-S1 < 3
               ADD 1             TO W35-S1
	       GO TO LE24.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM LE18.

       LE24A.
           IF W15-ITM4 (W35-S1) NOT = SPACES
           IF NOT (W15-SUPP4 (W35-S1) = SPACES OR LOW-VALUES)
               MOVE "Supplier:"  TO LB4-H (W35-S1)
               MOVE W15-SUPP4 (W35-S1)
                                 TO LB4-LABCDE (W35-S1).
           IF W35-S1 < 3
               ADD 1             TO W35-S1
	       GO TO LE24A.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM LE18.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM LE01.

       LE25.
	     MOVE "'E'xit / 'N'ext Label  [N]"
				 TO WS-ERR-MES.
	     MOVE "N"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.

       LE30.
011790	   IF NOT (WS-OPTION = "E" OR "N")
	       GO TO LE25.
011790     IF WS-OPTION = "N"
               MOVE 1001 TO CPOS
               PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23
	       MOVE W44-SCREEN1	 TO W43-SCREEN3
	       MOVE W44-ATTRIB1	 TO W42-ATTRIB3
	       PERFORM RESTORE-SCREEN-3
011800	       GO TO LE08.
           IF W35-S1 > 1
	       PERFORM LE17 THRU LE24A.

       LE35.
      *
      *    ****    1 0	 C P I
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.

018760 LE999.
018770       EXIT.
      /
      *    ****    F O R M A T   4   -   P R I C E   L A B E L S
      *
018610 NE000	       SECTION 55.
018620 NE00.
      *
      *    ****    1 2	 C P I
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO REP-LINE1.
018630	     DISPLAY CLR-SCREEN.
             DISPLAY S18.

       NE01.
             MOVE 1              TO W35-S1.
             INITIALIZE W15-LAB-3.

       NE05.
	     ACCEPT WS-OPTION AT 0632
		    WITH FOREGROUND-COLOR 15 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "N" OR "Y")
               GO TO NE05.
           IF WS-OPTION = "N"
               GO TO NE999.
	     DISPLAY "Omit price from label " AT 0812
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE "N"		 TO WS-PRICE.

       NE06.
	     ACCEPT WS-PRICE AT 0839
		    WITH FOREGROUND-COLOR 15
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-PRICE
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-PRICE = "N" OR "Y")
               GO TO NE06.
	     PERFORM SAVE-SCREEN-3.
	     MOVE W43-SCREEN3	 TO W44-SCREEN1.
	     MOVE W42-ATTRIB3	 TO W44-ATTRIB1.

       NE08.
	     DISPLAY "Item code" AT 1012.
	     MOVE SPACES	 TO W10-EXT-ITEM.

       NE10.
             PERFORM AA300.
021070	     ACCEPT W10-EXT-ITEM AT 1023
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
		     GO TO NE25
                 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO NE10
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
		     GO TO NE10
               END-EVALUATE
	       PERFORM AA50-LOOKUP
	       DISPLAY W10-EXT-ITEM AT 1023
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 5
	       IF W10-EXT-ITEM = SPACES
                   GO TO NE10.
015830	   IF W10-EXT-ITEM = SPACE
015840         GO TO NE25.
015850       PERFORM CA000.
015860     IF WS-F-ERROR NOT = ZERO
	       MOVE "Invalid code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO NE10.
	     DISPLAY STK-DESC AT 1043
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF STK-IND = 4
	       MOVE "Header Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO NE10.
	     PERFORM CHECK-CORRECT.
011790	   IF WS-OPTION = "N"
011800         GO TO NE10.
             DISPLAY "Number of Labels:" AT 1212.
	     MOVE ZERO		 TO W20-NO.

       NE15.
             ACCEPT S19.
           IF W20-NO = ZERO
               GO TO NE25.
	     PERFORM CHECK-CORRECT.
011790	   IF WS-OPTION = "N"
011800         GO TO NE15.

       NE16.
           IF W20-NO < 1
               GO TO NE25.
             MOVE STK-CODE       TO W15-ITM4 (W35-S1).
             MOVE STK-DESC       TO W15-DSC4 (W35-S1).
             MOVE STK-SUPP       TO W15-SUPP4 (W35-S1).
           IF WS-PRICE = "N"
               MOVE W10-VSELL    TO W15-PRC4 (W35-S1)
           ELSE
               MOVE ZERO         TO W15-PRC4 (W35-S1).
             SUBTRACT 1          FROM W20-NO.
           IF W35-S1 < 3
               ADD 1             TO W35-S1
               GO TO NE16.
             PERFORM NE17 THRU NE23.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014990	       GO TO NE35.
             GO TO NE16.

       NE17.
      *
      *      S E T   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.

       NE18.
             MOVE 1              TO W35-S1.
	     MOVE SPACES	 TO REP-LINE1.

       NE20.
           IF W15-ITM4 (W35-S1) NOT = SPACES
               MOVE W15-ITM4 (W35-S1)
                                 TO LB4-CODE (W35-S1).
           IF W35-S1 < 3
               ADD 1             TO W35-S1
               GO TO NE20.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-LINE1.
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     PERFORM NE18.

       NE21.
           IF W15-ITM4 (W35-S1) NOT = SPACES
               MOVE W15-DSC4 (W35-S1)
                                 TO LB4-DESC (W35-S1).
           IF W35-S1 < 3
               ADD 1             TO W35-S1
               GO TO NE21.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM NE18.

       NE21A.
           IF W15-ITM4 (W35-S1) NOT = SPACES
           IF NOT (W15-SUPP4 (W35-S1) = SPACES OR LOW-VALUES)
               MOVE "Supplier:"  TO LB4-H (W35-S1)
               MOVE W15-SUPP4 (W35-S1)
                                 TO LB4-LABCDE (W35-S1).
           IF W35-S1 < 3
               ADD 1             TO W35-S1
               GO TO NE21A.
	     PERFORM CALL-PRINTUTL.
	     PERFORM NE18.

       NE22.
           IF W15-ITM4 (W35-S1) NOT = SPACES
               IF WS-PRICE = "N"
                   MOVE "VAT incl"
                                 TO LB4-H (W35-S1)
                   MOVE W15-PRC4 (W35-S1)
                                 TO LB4-PRC (W35-S1).
           IF W35-S1 < 3
               ADD 1             TO W35-S1
               GO TO NE22.
	     PERFORM CALL-PRINTUTL.
	     PERFORM NE18.

       NE23.
           IF W15-ITM4 (W35-S1) NOT = SPACES
               MOVE WS-LABCDE    TO LB4-LABCDE (W35-S1)
               MOVE W12-MMYY     TO LB4-DTE (W35-S1).
           IF W35-S1 < 3
               ADD 1             TO W35-S1
               GO TO NE23.
	     PERFORM CALL-PRINTUTL.
	     PERFORM NE18.
	     PERFORM CALL-PRINTUTL.
	     PERFORM NE01.

       NE25.
	     MOVE "'E'xit / 'N'ext Label  [N]"
				 TO WS-ERR-MES.
	     MOVE "N"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.

       NE30.
011790	   IF NOT (WS-OPTION = "E" OR "N")
	       GO TO NE25.
011790     IF WS-OPTION = "N"
               MOVE 1001 TO CPOS
               PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23
	       MOVE W44-SCREEN1	 TO W43-SCREEN3
	       MOVE W44-ATTRIB1	 TO W42-ATTRIB3
	       PERFORM RESTORE-SCREEN-3
011800	       GO TO NE08.
           IF W35-S1 > 1
               PERFORM NE17 THRU NE23.

       NE35.
      *
      *    ****    1 0	 C P I
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.

018760 NE999.
018770       EXIT.
      /
      *    ****    S A P D C   P R I C E   L A B E L S
      *
018610 OE000	       SECTION 56.
018620 OE00.
      *
      *    ****    8   L P I
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 8		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
      *
      *    ****    1 7	 C P I
      *
	     MOVE 3		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO REP-LINE1.
018630	     DISPLAY CLR-SCREEN.
             DISPLAY S18.

       OE01.
             MOVE 1              TO W35-S1.
             INITIALIZE W15-SAPDC.

       OE05.
	     ACCEPT WS-OPTION AT 0632
		    WITH FOREGROUND-COLOR 15 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "N" OR "Y")
               GO TO OE05.
           IF WS-OPTION = "N"
               GO TO OE999.
	     DISPLAY "Omit price from label " AT 0812
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE "N"		 TO WS-PRICE.

       OE06.
	     ACCEPT WS-PRICE AT 0839
		    WITH FOREGROUND-COLOR 15
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-PRICE
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-PRICE = "N" OR "Y")
               GO TO OE06.
	     PERFORM SAVE-SCREEN-3.
	     MOVE W43-SCREEN3	 TO W44-SCREEN1.
	     MOVE W42-ATTRIB3	 TO W44-ATTRIB1.

       OE08.
	     DISPLAY "Item code" AT 1012.
	     MOVE SPACES	 TO W10-EXT-ITEM.

       OE10.
             PERFORM AA300.
021070	     ACCEPT W10-EXT-ITEM AT 1023
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
		     GO TO OE25
                 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO OE10
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
		     GO TO OE10
               END-EVALUATE
	       PERFORM AA50-LOOKUP
	       DISPLAY W10-EXT-ITEM AT 1023
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 5
	       IF W10-EXT-ITEM = SPACES
                   GO TO OE10.
015830	   IF W10-EXT-ITEM = SPACE
015840         GO TO OE25.
015850       PERFORM CA000.
015860     IF WS-F-ERROR NOT = ZERO
	       MOVE "Invalid code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO OE10.
	     DISPLAY STK-DESC AT 1043
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF STK-IND = 4
	       MOVE "Header Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO OE10.
	     PERFORM CHECK-CORRECT.
011790	   IF WS-OPTION = "N"
011800         GO TO OE10.
             DISPLAY "Number of Labels:" AT 1212.
             MOVE ZERO TO W20-NO.

       OE15.
             ACCEPT S19.
           IF W20-NO = ZERO
               GO TO OE25.
	     PERFORM CHECK-CORRECT.
011790	   IF WS-OPTION = "N"
011800         GO TO OE15.

       OE16.
           IF W20-NO < 1
               GO TO OE25.
             MOVE STK-CODE       TO W15-ITM5 (W35-S1).
             MOVE STK-DESC       TO W15-DSC5 (W35-S1).
           IF WS-PRICE = "N"
               MOVE W10-VSELL    TO W15-PRC5 (W35-S1)
           ELSE
               MOVE ZERO         TO W15-PRC5 (W35-S1).
             SUBTRACT 1          FROM W20-NO.
           IF W35-S1 < 4
               ADD 1             TO W35-S1
               GO TO OE16.
             PERFORM OE17 THRU OE23.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014990	       GO TO OE35.
             GO TO OE16.

       OE17.
      *
      *      S E T   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.

       OE18.
             MOVE 1              TO W35-S1.
	     MOVE SPACES	 TO REP-LINE1.

       OE20.
           IF W15-ITM5 (W35-S1) NOT = SPACES
               IF W35-S1 = 1
                   MOVE W15-ITM5 (1)
                                 TO SAPDC-CDE1
               ELSE
               IF W35-S1 = 2
                   MOVE W15-ITM5 (2)
                                 TO SAPDC-CDE2
               ELSE
               IF W35-S1 = 3
                   MOVE W15-ITM5 (3)
                                 TO SAPDC-CDE3
               ELSE
                   MOVE W15-ITM5 (4)
                                 TO SAPDC-CDE4.
           IF W35-S1 < 4
               ADD 1             TO W35-S1
               GO TO OE20.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-LINE1.
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     PERFORM OE18.

       OE21.
           IF W15-ITM5 (W35-S1) NOT = SPACES
               MOVE W15-DSC5 (W35-S1)
                                 TO SAPDC-DESC (W35-S1).
           IF W35-S1 < 4
               ADD 1             TO W35-S1
               GO TO OE21.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM OE18.
	     PERFORM CALL-PRINTUTL.

       OE22.
           IF W15-ITM5 (W35-S1) NOT = SPACES
               IF WS-PRICE = "N"
                   MOVE "VAT incl"
				 TO SAPDC-H (W35-S1).
	   IF W35-S1 < 4
               ADD 1             TO W35-S1
               GO TO OE22.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *
      *      S E T   E X P A N D E D   P R I N T
      *
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO REP-LINE1.
	     PERFORM OE18.

       OE22A.
           IF W15-ITM5 (W35-S1) NOT = SPACES
               IF WS-PRICE = "N"
	       IF W35-S1 = 1
		   MOVE W15-PRC5 (1)
				 TO SAPDC-PRC1
               ELSE
               IF W35-S1 = 2
		   MOVE W15-PRC5 (2)
				 TO SAPDC-PRC2
               ELSE
               IF W35-S1 = 3
		   MOVE W15-PRC5 (3)
				 TO SAPDC-PRC3
               ELSE
		   MOVE W15-PRC5 (4)
				 TO SAPDC-PRC4.
	   IF W35-S1 < 4
               ADD 1             TO W35-S1
	       GO TO OE22A.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     PERFORM OE18.

       OE23.
	     MOVE 4		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM OE01.

       OE25.
	     MOVE "'E'xit / 'N'ext Label  [N]"
				 TO WS-ERR-MES.
	     MOVE "N"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.

       OE30.
011790	   IF NOT (WS-OPTION = "E" OR "N")
	       GO TO OE25.
011790     IF WS-OPTION = "N"
               MOVE 1001 TO CPOS
               PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23
	       MOVE W44-SCREEN1	 TO W43-SCREEN3
	       MOVE W44-ATTRIB1	 TO W42-ATTRIB3
	       PERFORM RESTORE-SCREEN-3
011800	       GO TO OE08.
           IF W35-S1 > 1
               PERFORM OE17 THRU OE23.

       OE35.
      *
      *    ****    6   L P I
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 6		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
      *
      *    ****    1 0	 C P I
      *
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO REP-LINE1.

018760 OE999.
018770       EXIT.
      /
      *    ****    F O R M A T	 5   P R I C E	 L A B E L S
      *
018610 PE000	       SECTION 57.
018620 PE00.
      *
      *    ****    1 2	 C P I
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO REP-LINE1.
018630	     DISPLAY CLR-SCREEN.
             DISPLAY S18.

       PE01.
             MOVE 1              TO W35-S1.
             INITIALIZE W15-SAPDC.

       PE05.
	     ACCEPT WS-OPTION AT 0632
		    WITH FOREGROUND-COLOR 15 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "N" OR "Y")
	       GO TO PE05.
           IF WS-OPTION = "N"
	       GO TO PE35.
	     DISPLAY "Omit price from label " AT 0812
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE "N"		 TO WS-PRICE.

       PE06.
	     ACCEPT WS-PRICE AT 0839
		    WITH FOREGROUND-COLOR 15
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-PRICE
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-PRICE = "N" OR "Y")
	       GO TO PE06.
	     PERFORM SAVE-SCREEN-3.
	     MOVE W43-SCREEN3	 TO W44-SCREEN1.
	     MOVE W42-ATTRIB3	 TO W44-ATTRIB1.

       PE08.
	     DISPLAY "Item code" AT 1012.
	     MOVE SPACES	 TO W10-EXT-ITEM.

       PE10.
             PERFORM AA300.
021070	     ACCEPT W10-EXT-ITEM AT 1023
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
		     GO TO PE25
                 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO PE10
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
		     GO TO PE10
               END-EVALUATE
	       PERFORM AA50-LOOKUP
	       DISPLAY W10-EXT-ITEM AT 1023
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 5
	       IF W10-EXT-ITEM = SPACES
		   GO TO PE10.
015830	   IF W10-EXT-ITEM = SPACE
015840	       GO TO PE25.
015850       PERFORM CA000.
015860     IF WS-F-ERROR NOT = ZERO
	       MOVE "Invalid code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO PE10.
	     DISPLAY STK-DESC AT 1043
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF STK-IND = 4
	       MOVE "Header Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO PE10.
	     PERFORM CHECK-CORRECT.
011790	   IF WS-OPTION = "N"
011800	       GO TO PE10.
             DISPLAY "Number of Labels:" AT 1212.
             MOVE ZERO TO W20-NO.

       PE15.
             ACCEPT S19.
           IF W20-NO = ZERO
	       GO TO PE25.
	     PERFORM CHECK-CORRECT.
011790	   IF WS-OPTION = "N"
011800	       GO TO PE15.

       PE16.
           IF W20-NO < 1
	       GO TO PE25.
             MOVE STK-CODE       TO W15-ITM5 (W35-S1).
             MOVE STK-DESC       TO W15-DSC5 (W35-S1).
           IF WS-PRICE = "N"
               MOVE W10-VSELL    TO W15-PRC5 (W35-S1)
           ELSE
               MOVE ZERO         TO W15-PRC5 (W35-S1).
	     MOVE STK-COST	 TO W35-CST.
009540	     MOVE W35-WHOLE	 TO W35-CEDIT.
009550	     MOVE 3		 TO W35-S3.

009560 PE16A.
	   IF W35-S3 < 6
	       IF W35-CHAR (W35-S3) = SPACE
		   MOVE SPACE	 TO W35-CHAR (W35-S3)
		   ADD 1	 TO W35-S3
		   GO TO PE16A.
009570	   IF (W35-CHAR (W35-S3) NOT NUMERIC) OR
009590	      (W35-NUM (W35-S3) = 0)
009600	       MOVE 10		 TO W35-S2
009610     ELSE
009620	       MOVE W35-NUM (W35-S3)
				 TO W35-S2.
009630	     MOVE W30-CODE (W35-S2)
				 TO W35-CHAR (W35-S3).
009650	   IF W35-S3 < 10
009660	       ADD 1		 TO W35-S3
009670	       GO TO PE16A.
	     MOVE W35-LCST	 TO W20-COST.
	     MOVE W20-COST	 TO W15-COST (W35-S1).
	     SUBTRACT 1 	 FROM W20-NO.
           IF W35-S1 < 4
               ADD 1             TO W35-S1
	       GO TO PE16.
	     PERFORM PE17 THRU PE24.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014990	       GO TO PE35.
	     GO TO PE16.

       PE17.
      *
      *      S E T   E X P A N D E D   P R I N T
      *
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.

       PE18.
             MOVE 1              TO W35-S1.
	     MOVE SPACES	 TO REP-LINE1.

       PE20.
           IF W15-ITM5 (W35-S1) NOT = SPACES
	       MOVE W15-ITM5 (W35-S1)
				 TO LB5-CODE (W35-S1).
	   IF W35-S1 < 4
               ADD 1             TO W35-S1
	       GO TO PE20.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     PERFORM PE18.

       PE21.
           IF W15-ITM5 (W35-S1) NOT = SPACES
               MOVE W15-DSC5 (W35-S1)
				 TO LB5-DESC (W35-S1).
           IF W35-S1 < 4
               ADD 1             TO W35-S1
	       GO TO PE21.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM PE18.
	     PERFORM CALL-PRINTUTL.

       PE22.
           IF W15-ITM5 (W35-S1) NOT = SPACES
               IF WS-PRICE = "N"
                   MOVE "VAT incl"
				 TO LB5-H (W35-S1)
                   MOVE W15-PRC5 (W35-S1)
				 TO LB5-PRC (W35-S1).
           IF W35-S1 < 4
               ADD 1             TO W35-S1
	       GO TO PE22.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM PE18.

       PE24.
	   IF W15-ITM5 (W35-S1) NOT = SPACES
	       MOVE W15-COST (W35-S1)
				 TO LB5-COST (W35-S1)
	       MOVE WS-LABCDE	 TO LB5-LABCDE (W35-S1)
	       MOVE W12-MMYY	 TO LB5-DTE (W35-S1).
	   IF W35-S1 < 4
               ADD 1             TO W35-S1
	       GO TO PE24.
	     PERFORM CALL-PRINTUTL.
	     PERFORM PE18.
	     PERFORM CALL-PRINTUTL.
	     PERFORM PE01.

       PE25.
	     MOVE "'E'xit / 'N'ext Label  [N]"
				 TO WS-ERR-MES.
	     MOVE "N"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.

       PE30.
011790	   IF NOT (WS-OPTION = "E" OR "N")
	       GO TO PE25.
011790     IF WS-OPTION = "N"
               MOVE 1001 TO CPOS
               PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23
	       MOVE W44-SCREEN1	 TO W43-SCREEN3
	       MOVE W44-ATTRIB1	 TO W42-ATTRIB3
	       PERFORM RESTORE-SCREEN-3
011800	       GO TO PE08.
           IF W35-S1 > 1
	       PERFORM PE17 THRU PE24.

       PE35.
      *
      *    ****    1 0	 C P I
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.

018760 PE999.
018770       EXIT.
      /
      *    ****    F O R M A T	 7   P R I C E	 L A B E L S
      *
018610 QE000	       SECTION 58.
018620 QE00.
      *
      *    ****    1 2	 C P I
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO REP-LINE1.
018630	     DISPLAY CLR-SCREEN.
             DISPLAY S18.

       QE01.
             MOVE 1              TO W35-S1.
             INITIALIZE W15-SAPDC.

       QE05.
	     ACCEPT WS-OPTION AT 0632
		    WITH FOREGROUND-COLOR 15 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "N" OR "Y")
	       GO TO QE05.
           IF WS-OPTION = "N"
	       GO TO QE35.
	     DISPLAY "Omit price from label " AT 0812
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE "N"		 TO WS-PRICE.

       QE06.
	     ACCEPT WS-PRICE AT 0839
		    WITH FOREGROUND-COLOR 15
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-PRICE
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-PRICE = "N" OR "Y")
	       GO TO QE06.
	     PERFORM SAVE-SCREEN-3.
	     MOVE W43-SCREEN3	 TO W44-SCREEN1.
	     MOVE W42-ATTRIB3	 TO W44-ATTRIB1.

       QE08.
	     DISPLAY "Item code" AT 1012.
	     MOVE SPACES	 TO W10-EXT-ITEM.

       QE10.
             PERFORM AA300.
021070	     ACCEPT W10-EXT-ITEM AT 1023
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
		     GO TO QE25
                 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO QE10
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
		     GO TO QE10
               END-EVALUATE
	       PERFORM AA50-LOOKUP
	       DISPLAY W10-EXT-ITEM AT 1023
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 5
	       IF W10-EXT-ITEM = SPACES
		   GO TO QE10.
015830	   IF W10-EXT-ITEM = SPACE
015840	       GO TO QE25.
015850       PERFORM CA000.
015860     IF WS-F-ERROR NOT = ZERO
	       MOVE "Invalid code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO QE10.
	     DISPLAY STK-DESC AT 1043
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF STK-IND = 4
	       MOVE "Header Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO QE10.
	     PERFORM CHECK-CORRECT.
011790	   IF WS-OPTION = "N"
011800	       GO TO QE10.
             DISPLAY "Number of Labels:" AT 1212.
             MOVE ZERO TO W20-NO.

       QE15.
             ACCEPT S19.
           IF W20-NO = ZERO
	       GO TO QE25.
	     PERFORM CHECK-CORRECT.
011790	   IF WS-OPTION = "N"
011800	       GO TO QE15.

       QE16.
           IF W20-NO < 1
	       GO TO QE25.
             MOVE STK-CODE       TO W15-ITM5 (W35-S1).
             MOVE STK-DESC       TO W15-DSC5 (W35-S1).
           IF WS-PRICE = "N"
               MOVE W10-VSELL    TO W15-PRC5 (W35-S1)
           ELSE
               MOVE ZERO         TO W15-PRC5 (W35-S1).
	     MOVE STK-COST	 TO W35-CST.
009540	     MOVE W35-WHOLE	 TO W35-CEDIT.
009550	     MOVE 3		 TO W35-S3.

009560 QE16A.
	   IF W35-S3 < 6
	       IF W35-CHAR (W35-S3) = SPACE
		   MOVE SPACE	 TO W35-CHAR (W35-S3)
		   ADD 1	 TO W35-S3
		   GO TO QE16A.
009570	   IF (W35-CHAR (W35-S3) NOT NUMERIC) OR
009590	      (W35-NUM (W35-S3) = 0)
009600	       MOVE 10		 TO W35-S2
009610     ELSE
009620	       MOVE W35-NUM (W35-S3)
				 TO W35-S2.
009630	     MOVE W30-CODE (W35-S2)
				 TO W35-CHAR (W35-S3).
009650	   IF W35-S3 < 10
009660	       ADD 1		 TO W35-S3
009670	       GO TO QE16A.
	     MOVE W35-LCST	 TO W20-COST.
	     MOVE W20-COST	 TO W15-COST (W35-S1).
	     SUBTRACT 1 	 FROM W20-NO.
           IF W35-S1 < 4
               ADD 1             TO W35-S1
	       GO TO QE16.
	     PERFORM QE17 THRU QE24.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014990	       GO TO QE35.
	     GO TO QE16.

       QE17.
      *
      *      S E T   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.

       QE18.
             MOVE 1              TO W35-S1.
	     MOVE SPACES	 TO REP-LINE1.

       QE20.
           IF W15-ITM5 (W35-S1) NOT = SPACES
	       MOVE W15-ITM5 (W35-S1)
				 TO LB6-CODE (W35-S1).
	   IF W35-S1 < 4
               ADD 1             TO W35-S1
	       GO TO QE20.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     PERFORM QE18.

       QE21.
           IF W15-ITM5 (W35-S1) NOT = SPACES
               MOVE W15-DSC5 (W35-S1)
				 TO LB6-DESC (W35-S1).
           IF W35-S1 < 4
               ADD 1             TO W35-S1
	       GO TO QE21.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM QE18.
	     PERFORM CALL-PRINTUTL.

       QE22.
           IF W15-ITM5 (W35-S1) NOT = SPACES
               IF WS-PRICE = "N"
                   MOVE "VAT incl"
				 TO LB6-H (W35-S1)
                   MOVE W15-PRC5 (W35-S1)
				 TO LB6-PRC (W35-S1).
           IF W35-S1 < 4
               ADD 1             TO W35-S1
	       GO TO QE22.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM QE18.

       QE24.
	   IF W15-ITM5 (W35-S1) NOT = SPACES
	       MOVE W15-COST (W35-S1)
				 TO LB6-COST (W35-S1)
	       MOVE WS-LABCDE	 TO LB6-LABCDE (W35-S1)
	       MOVE W12-MMYY	 TO LB6-DTE (W35-S1).
	   IF W35-S1 < 4
               ADD 1             TO W35-S1
	       GO TO QE24.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM QE18.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM QE01.

       QE25.
	     MOVE "'E'xit / 'N'ext Label  [N]"
				 TO WS-ERR-MES.
	     MOVE "N"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.

       QE30.
011790	   IF NOT (WS-OPTION = "E" OR "N")
	       GO TO QE25.
011790     IF WS-OPTION = "N"
               MOVE 1001 TO CPOS
               PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23
	       MOVE W44-SCREEN1	 TO W43-SCREEN3
	       MOVE W44-ATTRIB1	 TO W42-ATTRIB3
	       PERFORM RESTORE-SCREEN-3
011800	       GO TO QE08.
           IF W35-S1 > 1
	       PERFORM QE17 THRU QE24.

       QE35.
      *
      *    ****    1 0	 C P I
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.

018760 QE999.
018770       EXIT.
      /
019370 ZA000-INIT	       SECTION 80.
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
020010 ZB000-ERROR	       SECTION 81.
      
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
