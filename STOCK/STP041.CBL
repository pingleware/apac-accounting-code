      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	  ****	      ***      **      *
      *   **	**     **     **   **	 **  **      ****     ***      *
      *   **	       **     **   **	**    **    ** **      **      *
      *    ******      **     ******	**    **   **  **      **      *
      * 	**     **     **	**    **  ********     **      *
      *   **	**     **     **	 **  **        **      **      *
      *    ******      **     **	  ****	      ****   ******    *
      * 							       *
      *     ENGLISH                                                    *
      *                                                                *
      *     S T O C K	R E P O R T S	-   P R I C E	L I S T S      *
      *                                                                *
      *     SPECIAL SPARES VERSION 1989 - WEEKLY TOTALS & X-REF        *
      *                                                                *
      *     VERSION 8.13.09 - November 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STP041.
000030 AUTHOR.	       J.W.LEMMON. (APAC)
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
       77  WS-P1	   PIC	9(04)	 COMP-5.
003150 77  WS-IXS	   PIC	9(04)	 COMP-5.
003150 77  WS-IXS1	   PIC	9(04)	 COMP-5.
003160 77  WS-IXS2	   PIC	9(04)	 COMP-5.
003170 77  WS-IXS3	   PIC	9(04)	 COMP-5.
003180 77  WS-IXS4	   PIC	9(04)	 COMP-5.
003230 77  WS-PARKEY	   PIC	9(04)	 COMP-5.
003250 77  WS-NETKEY	   PIC	9(04)	 COMP-5.
003150 77  WS-PRN-NO	   PIC	X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC	X(01)	 COMP-X.
003270 77  WS-PAGE	   PIC	9(04)	 COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
003190 77  WS-CAT	   PIC	X(04).
003200 77  WS-SCAT	   PIC	X(04).
002190 77  WS-USE-PACKS    PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
003300 77  WS-PQNT	   PIC	X(01).
003310 77  WS-WS	   PIC	X(01) VALUE "N".
003310 77  WS-MEMO	   PIC	X(01) VALUE "N".
003310 77  WS-PRT	   PIC	X(01).
003320 77  WS-BIN	   PIC	X(01).
       77  WS-ETYPE	   PIC	X(01).
003330 77  WS-OPTION	   PIC	X(01).
003330 77  WS-PRICE	   PIC	X(01).
       77  WS-NUM	   PIC	ZZ9.
003350 77  WS-ERROR	   PIC	9(01).
003380 77  WS-KEY1	   PIC	9(04)	 COMP-5.
003390 77  WS-KSTORE	   PIC	9(04)	 COMP-5.
003430 77  WS-ART	   PIC	X(01).
003430 77  WS-CST	   PIC	X(01).
003430 77  WS-MAT	   PIC	X(01).
003430 77  WS-SEQ	   PIC	X(01).
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
003570 77  WS-WEEK	   PIC	9(01).
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
001390	       05  WS-TCH   PIC  X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
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
000610	       05  REP-CNO     PIC  X(04).
000620	       05  REP-CATNO REDEFINES REP-CNO PIC X(04).
000630	       05  FILLER      PIC  X(01).
000640	       05  REP-CAT     PIC  X(30).
000650	       05  FILLER      PIC  X(35).
000850	   03  REP-LINE9 REDEFINES W02-PRN-LINE.
000860	       05  REP-PG      PIC  X(06).
000870	       05  REP-P-N     PIC  Z(03)9.
000880	       05  FILLER      PIC  X(21).
000890	       05  REP-COMP    PIC  X(40).
000900	       05  REP-REPORT  PIC  X(34).
000910	       05  FILLER      PIC  X(11).
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
001600	       05  FILLER      PIC  X(04).
001610	       05  REP-DCOST   PIC  Z(06)9.99.
001620	       05  FILLER      PIC  X(02).
001630	       05  REP-DSELL   PIC  Z(06)9.99.
001640	       05  FILLER      PIC  X(15).
	   03  PRC-L1 REDEFINES W02-PRN-LINE.
	       05  PRC-H-ITEM-BIN.
		   07  PRC-H1  PIC  X(19).
		   07  PRC-H2  PIC  X(08).
	       05  PRC-H-ARTICLE REDEFINES PRC-H-ITEM-BIN.
		   07  FILLER  PIC  X(16).
		   07  PRC-H3  PIC  X(11).
	       05  PRC-H4      PIC  X(32).
	       05  PRC-H5.
		   07  FILLER  PIC  X(02).
		   07  PRC-H5A PIC  X(12).
	       05  PRC-H6.
		   07  PRC-H6A PIC  X(10).
	       05  PRC-H7.
		   07  FILLER  PIC  X(05).
		   07  PRC-H7A PIC  X(08).
	       05  FILLER      PIC  X(02).
	       05  PRC-H8.
		   07  FILLER  PIC  X(01).
		   07  PRC-H8A PIC  X(12).
	       05  PRC-H9.
		   07  PRC-H9A PIC  X(09).
	       05  PRC-H10.
		   07  FILLER  PIC  X(04).
		   07  PRC-H10A PIC X(08).
000730	   03  PRC-L2 REDEFINES W02-PRN-LINE.
	       05  PRC-ITM-AND-BIN.
		   07  PRC-ITEM PIC  X(18).
000750		   07  PRC-ITM REDEFINES PRC-ITEM
			       PIC  X(14)/X(03).
000760		   07  FILLER  PIC  X(01).
000770		   07  PRC-BIN PIC  X(06).
000780		   07  FILLER  PIC  X(01).
	       05  PRC-ARTICLE REDEFINES PRC-ITM-AND-BIN.
		   07  FILLER  PIC  X(06).
		   07  PRC-PER PIC  X(08).
		   07  FILLER  PIC  X(02).
000790		   07  PRC-CODE.
		     09	PRC-CCODE
			       PIC  Z(06)9.99.
000800	       05  FILLER      PIC  X(01).
000810	       05  PRC-DESC.
		   07  FILLER  PIC  X(12).
		   07  PRC-MRT PIC  ZZ9.99.
		   07  PRC-P   PIC  X(01).
		   07  FILLER  PIC  X(11).
000820	       05  FILLER      PIC  X(01).
	       05  PRC-E-WSALE PIC  Z(07)9.999.
	       05  FILLER      PIC  X(01).
000830	       05  PRC-E-CASH  PIC  Z(07)9.999.
000820	       05  FILLER      PIC  X(01).
	       05  PRC-E-RET   PIC  Z(07)9.999.
	       05  FILLER      PIC  X(01).
000830	       05  PRC-I-WSALE PIC  Z(07)9.99.
000840	       05  FILLER      PIC  X(01).
	       05  PRC-I-CASH  PIC  Z(07)9.99.
	       05  FILLER      PIC  X(01).
000830	       05  PRC-I-RET   PIC  Z(07)9.99.
	   03  PRI-L1 REDEFINES W02-PRN-LINE.
	       05  PRI-HEADINGS.
		 07  PRI-H1    PIC  X(19).
		 07  PRI-H2    PIC  X(07).
	       05  PRI-A-HEADING REDEFINES PRI-HEADINGS.
		 07  FILLER    PIC  X(18).
		 07  PRI-H3    PIC  X(08).
	       05  FILLER      PIC  X(01).
	       05  PRI-H4      PIC  X(34).
	       05  PRI-H5      PIC  X(16).
	       05  PRI-H6      PIC  X(10).
	       05  PRI-H7      PIC  X(09).
000730	   03  PRC-L2 REDEFINES W02-PRN-LINE.
	       05  PRI-ITM-BIN.
		 07  PRI-ITEM  PIC  X(18).
000750		 07  PRI-ITM REDEFINES PRI-ITEM
			       PIC  X(14)/X(03).
000760		 07  FILLER    PIC  X(01).
000770		 07  PRI-BIN   PIC  X(06).
000780		 07  FILLER    PIC  X(01).
	       05  PRI-ARTICLE REDEFINES PRI-ITM-BIN.
		 07  FILLER    PIC  X(07).
		 07  PRI-PER   PIC  X(08).
		 07  FILLER    PIC  X(02).
000790		 07  PRI-CCODE PIC  X(09).
000800	       05  FILLER      PIC  X(01).
000810	       05  PRI-DESC.
		   07  FILLER  PIC  X(12).
		   07  PRI-MRT PIC  ZZ9.99.
		   07  PRI-P   PIC  X(01).
		   07  FILLER  PIC  X(11).
000820	       05  FILLER      PIC  X(01).
	       05  PRI-EXCL    PIC  Z(07)9.999.
	       05  FILLER      PIC  X(02).
000830	       05  PRI-VAT     PIC  Z(07)9.99.
000820	       05  FILLER      PIC  X(02).
000830	       05  PRI-INCL    PIC  Z(07)9.99.
	   03  PRI-L3 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(42).
	       05  PRI-MEMO    PIC  X(60).
	   03  DEP-L1 REDEFINES W02-PRN-LINE.
	       05  DEP-H1      PIC  X(15).
	       05  DEP-H2      PIC  X(04).
	       05  DEP-H3      PIC  X(34).
	       05  DEP-H4      PIC  X(10).
	       05  DEP-H5      PIC  X(14).
	       05  DEP-H6      PIC  X(12).
	       05  DEP-H7      PIC  X(07).
	       05  DEP-H8.
		   07  FILLER  PIC  X(01).
		   07  DEP-H8A PIC  X(06).
	       05  DEP-H9.
		   07  FILLER  PIC  X(02).
		   07  DEP-H9A PIC  X(04).
	       05  DEP-H10.
		   07  FILLER  PIC  X(01).
		   07  DEP-H10A PIC X(06).
	       05  FILLER      PIC  X(02).
	       05  DEP-H11     PIC  X(09).
	       05  DEP-H12     PIC  X(05).
	   03  DEP-L2 REDEFINES W02-PRN-LINE.
	       05  DEP-ITM     PIC  X(14).
	       05  FILLER      PIC  X(01).
	       05  DEP-BIN     PIC  X(03).
	       05  FILLER      PIC  X(01).
	       05  DEP-DESC    PIC  X(30).
	       05  FILLER      PIC  X(01).
	       05  DEP-QNT     PIC  Z(06)9.999-.
	       05  DEP-COST    PIC  Z(06)9.999.
	       05  FILLER      PIC  X(01).
	       05  DEP-AVG     PIC  Z(05)9.999.
	       05  FILLER      PIC  X(01).
	       05  DEP-RET     PIC  Z(06)9.99.
	       05  FILLER      PIC  X(01).
	       05  DEP-MWS     PIC  ZZ9.99.
	       05  FILLER      PIC  X(01).
	       05  DEP-MCSH    PIC  ZZ9.99.
	       05  FILLER      PIC  X(01).
	       05  DEP-MRET    PIC  ZZ9.99.
	       05  FILLER      PIC  X(01).
	       05  DEP-ACT     PIC  Z9/99/99.
	       05  FILLER      PIC  X(01).
	       05  DEP-GP      PIC  ZZ9.99.

       COPY W05.VAT.

       COPY W10.STK.

       COPY W12.WS.

       01  W15-PRICE-NAMES.
	   03  W15-PNAME.
	       05  W15-CHECK   PIC  X(01).
	       05  W15-REST    PIC  X(09).

       01  W20-MATRIX.
	   03  W20-DISCOUNT OCCURS 10.
	       05  W20-RATE	 PIC  9(03)V99	COMP-3.
000140	       05  W20-SELL	 PIC S9(08)V999 COMP-3.
000150	       05  W20-CASH	 PIC S9(08)V999 COMP-3.
000160	       05  W20-WSALE	 PIC S9(08)V999 COMP-3.
000140	       05  W20-VSELL	 PIC S9(08)V999 COMP-3.
000150	       05  W20-VCASH	 PIC S9(08)V999 COMP-3.
000160	       05  W20-VWSALE	 PIC S9(08)V999 COMP-3.

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
000860	   03  LINE  2 COLUMN 32 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "STOCK PRICE LISTS".

       01  S-MENU.
	   03  LINE  4 COLUMN 27 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 4 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 54 BACKGROUND-COLOR 4 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  5 COLUMN 27 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 4 HIGHLIGHT
				 VALUE "³    ".
	   03	       COLUMN 32 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "PRICE LIST OPTIONS".
	   03	       COLUMN 50 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 0
				 VALUE "    ³".
	   03  LINE  6 COLUMN 27 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 4 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1 ".
	   03	       COLUMN 31 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 7
				 PIC  X(10) FROM WS-WS-HD.
	   03	       COLUMN 41 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 7
				 VALUE "	     ".
	   03	       COLUMN 54 BACKGROUND-COLOR 4 FOREGROUND-COLOR 0
				 VALUE "³".
	   03  LINE  7 COLUMN 27 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 4 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2 ".
	   03	       COLUMN 31 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 7
				 PIC  X(10) FROM WS-CS-HD.
	   03	       COLUMN 41 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 7
				 VALUE "	     ".
	   03	       COLUMN 54 BACKGROUND-COLOR 4 FOREGROUND-COLOR 0
				 VALUE "³".
001470	   03  LINE  8 COLUMN 27 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 4 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3 ".
	   03	       COLUMN 31 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 7
				 PIC  X(08) FROM WS-RT-HD.
	   03	       COLUMN 39 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 7
				 VALUE " 	       ".
	   03	       COLUMN 54 BACKGROUND-COLOR 4 FOREGROUND-COLOR 0
				 VALUE "³".
001470	   03  LINE  9 COLUMN 27 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 4 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 30 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 7
				 VALUE " List all three Prices  ".
	   03	       COLUMN 54 BACKGROUND-COLOR 4 FOREGROUND-COLOR 0
				 VALUE "³".
001470	   03  LINE 10 COLUMN 27 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 4 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 29 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 30 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 7
				 VALUE " Exit                   ".
	   03	       COLUMN 54 BACKGROUND-COLOR 4 FOREGROUND-COLOR 0
				 VALUE "³".
	   03  LINE 11 COLUMN 27 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 4 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 28 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 0 VALUE
				 "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

004440 01  S03.
004450	   03  LINE 16 COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
004460				 PIC X(14) FROM STK-ITEM.

004440 01  S03-ITM.
004450	   03  LINE 16 COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
004460				 PIC X(14) FROM STK-ITEM.
	   03	       COLUMN 40 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 5 VALUE "/".
	   03	       COLUMN 41 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
004460				 PIC X(03) FROM STK-ITM.

004440 01  S03-EXT.
004450	   03  LINE 16 COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
004460				 PIC X(18) FROM STK-CODE.

004500 01  S05.
           02  S05A.
           03  LINE  2 COLUMN 32 FOREGROUND-COLOR 15
                                 VALUE "STOCK PRICE LISTS".
           02  S05B.
004510	   03  LINE  8 COLUMN  8 VALUE
004520                           "Press 'ENTER' for all departments - De
004530-                          "partment code for specific".

004540 01  S06.
004550     03  LINE 12 COLUMN 20 FOREGROUND-COLOR 14
004560                           VALUE "Busy printing price list".

004570 01  S07.
004580     03  LINE  6 COLUMN 20 FOREGROUND-COLOR 14 VALUE 
004590                           "Busy printing departmental report".

005040 01  S15.
005050     03  LINE 16 COLUMN 12 VALUE "Stock record:".

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
	     PERFORM BE00S.
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

      *
      *    ****    Calculate the inclusive prices
      *
       AA500         SECTION.
       AA505.
           IF STK-WSALE = ZERO
               IF STK-CASH NOT = ZERO
                   MOVE STK-CASH TO STK-WSALE
               ELSE
                   MOVE STK-SELL TO STK-WSALE.
015630     IF STK-CASH = ZERO
015640         MOVE STK-SELL     TO STK-CASH.
	     MOVE ZERO		 TO W05-RATE.
	   IF STK-TAX > 0
               MOVE W05-VAT (STK-TAX)
                                 TO W05-RATE.
             COMPUTE W10-VCASH ROUNDED 
		   = STK-CASH + (STK-CASH * W05-RTE).
             COMPUTE W10-VWSALE ROUNDED 
		   = STK-WSALE + (STK-WSALE * W05-RTE).
             COMPUTE W10-VSELL ROUNDED 
		   = STK-SELL + (STK-SELL * W05-RTE).

015650 AA599.
015660       EXIT.
      /
005810 AB000              SECTION.
005110 AB05.
	     MOVE "H"		 TO W02-PRN-TYPE.
005120	     MOVE SPACES	 TO REP-DETAIL1.
005130	     MOVE "Page:"	 TO REP-PG.
005140	     MOVE "Date:"	 TO REP-DAT.
005150       MOVE W95-COMP       TO REP-COMP.
005160       ADD 1               TO WS-PAGE.
005170       MOVE WS-PAGE        TO REP-P-N.
005180       MOVE W12-TODAY      TO REP-DT.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
005200	     MOVE SPACES	 TO REP-DETAIL1.

005210 AB10.
005220       MOVE "S T O C K   P R I C E   L I S T" 
                                 TO REP-COMP.
	     PERFORM CALL-PRINTUTL.
005240	     MOVE SPACES	 TO REP-DETAIL1.
005250	     MOVE "Category:"	 TO REP-HEAD2.
005260	     MOVE DPT-CODE	 TO REP-CATNO.
005270       MOVE DPT-DESC       TO REP-CAT.
	     PERFORM CALL-PRINTUTL.
005290	     MOVE SPACES	 TO REP-DETAIL1.
005300	     MOVE "Item Code"	 TO PRC-H1.
005310	     MOVE "Bin" 	 TO PRC-H2.
005330	     MOVE "Description"  TO PRC-H4.
005340       MOVE ALL "_"        TO PRC-H5 PRC-H7 PRC-H8 PRC-H10.
005340	     MOVE "Exclusive"	 TO PRC-H6.
005340	     MOVE "Inclusive"	 TO PRC-H9.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-DETAIL1.
005320	     MOVE "Article"	 TO PRC-H3.
	     MOVE WS-WS-HD	 TO PRC-H5  PRC-H8.
	     MOVE WS-CS-HD	 TO PRC-H6  PRC-H9.
	     MOVE WS-RT-HD	 TO PRC-H7A PRC-H10A.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
005360	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE ALL "Í"	 TO PRC-L1.
	     PERFORM CALL-PRINTUTL.
005360	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "D"		 TO W02-PRN-TYPE.
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

       AB40.
             EXIT.
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

       BE00S	       SECTION.
009010 BE65.
	     MOVE SPACES	 TO WS-DEPT.
	   IF WS-PRINTER = "S"
	       MOVE "I" 	 TO WS-COMMAND
	       PERFORM CALL-PRINTUTL
	       MOVE "P" 	 TO WS-COMMAND.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 1		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
009030	     DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 29		 TO SHADE-COL.
	     MOVE 26		 TO SHADE-WIDTH.
	     MOVE 7		 TO SHADE-LINES.
	     DISPLAY S-MENU.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.

       BE65A.
	     MOVE 5		 TO TOP-ROW.
	     MOVE 9		 TO BOTTOM-ROW.
	     MOVE 4		 TO SCREEN-LIN.
	     MOVE 29		 TO SCREEN-COL.
	     MOVE 24		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       BE65B.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

006660 BE65C.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO BE65D
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO BE65C
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO BE65C
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO BE65C
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO BE65C
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO BE65D
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BE65B
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO BE65F
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BE65B
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF WS-OPTION < "0" OR > "4"
		   CALL X"E5"
		   GO TO BE65B
	       END-IF
	       GO TO BE65E
	   ELSE
	       CALL X"E5"
	       GO TO BE65B.

       BE65D.
	   IF SCREEN-LIN < 5
	       CALL X"E5"
	       GO TO BE65B
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
	       MOVE "0"		 TO WS-OPTION.
	   GO TO BE65F.

       BE65E.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 9		 TO SCREEN-LIN
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
	       MOVE 8		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       BE65F.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
006730	   IF WS-OPTION = "0"
006740	       GO TO BE999.
	     MOVE ZERO		 TO WS-PAGE.
008780	   IF WS-OPTION = "1" OR "2" OR "3"
	       PERFORM ME000
008790         GO TO BE65.
008800     IF WS-OPTION = "4"
               PERFORM AB25
               DISPLAY CLR-SCREEN 
	       GO TO BE65G.
             GO TO BE65A.

       BE65G.
	     DISPLAY "Print coded cost price " AT 0608
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE "Y"		 TO WS-ART.

       BE65H.
	     ACCEPT WS-ART AT 0635
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-ART
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-ART = "N" OR "Y")
	       GO TO BE65H.
	   IF WS-ART = "Y"
	       MOVE "N" 	 TO WS-CST
	       GO TO BE65HB.
	     DISPLAY "Print cost price " AT 0708
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE "Y"		 TO WS-CST.

       BE65HA.
	     ACCEPT WS-CST AT 0729
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-CST
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-CST = "N" OR "Y")
	       GO TO BE65HA.

       BE65HB.
	     DISPLAY "Print matrix prices " AT 0808
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE "N"		 TO WS-MAT.

       BE65HC.
	     ACCEPT WS-MAT AT 0832
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-MAT
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-MAT = "N" OR "Y")
	       GO TO BE65HC.

       BE65I.
009040	     DISPLAY S05.
009050	     ACCEPT WS-DEPT AT 0873
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BE65
		 WHEN OTHER	 CALL X"E5"
		     GO TO BE65I
	       END-EVALUATE.
009080	   IF WS-DEPT = SPACES
	       GO TO BE66A.
009330	     MOVE WS-DEPT	 TO DPT-CODE.
	   IF (DPT-CODE = "XXXX" OR "YYYY") OR
	      (DPT-SCDE = "VAT")
	       GO TO BE65I.
009340	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       MOVE "Invalid department code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BE65I.
	     DISPLAY DPT-DESC AT 1012
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

       BE66.
	     MOVE "'C'ontinue or 'R'ekey  [ ]"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "R" OR "C")
               GO TO BE66.
           IF WS-OPTION = "R"
	       GO TO BE65I.

       BE66A.
	     MOVE "'D'escription or 'C'ode sequence  [C]"
				 TO WS-ERR-MES.
	     MOVE "C"		 TO WS-OPTION.

       BE67.
	     PERFORM OPT-MESSAGE.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BE65I
		 WHEN OTHER	 CALL X"E5"
		     GO TO BE67
	       END-EVALUATE.
	     MOVE WS-OPTION	 TO WS-SEQ.
	   IF NOT (WS-SEQ = "D" OR "C")
	       GO TO BE67.

       BE68.
009110       DISPLAY S06.
009120       DISPLAY S15.
009200	   IF WS-DEPT > SPACES
009210	       GO TO BE75.
	     PERFORM BE000.

009170 BE70.
	     PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       GO TO BE72A.
	   IF (DPT-CODE = "XXXX" OR "YYYY") OR
	      (DPT-SCDE = "VAT")
	       GO TO BE72.
	     GO TO BE75.

009220 BE72.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO BE73.
009200	   IF WS-DEPT > SPACES
009210         GO TO BE65.
009250	     GO TO BE70.

       BE72A.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.

009270 BE73.
             PERFORM AB35.
009280	     GO TO BE65.

009290 BE75.
	   IF NOT (WS-PRINTER = "S")
	       DISPLAY DPT-DESC AT 1012
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
           IF WS-PAGE > ZERO
009700	       IF W02-LINAGE < (W02-PRN-LENGTH - 8)
		   MOVE 1	 TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL
005250		   MOVE "Category:"
                                 TO REP-HEAD2
005260		   MOVE DPT-CODE TO REP-CATNO
005270             MOVE DPT-DESC TO REP-CAT
		   MOVE 2	 TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL
005290		   MOVE SPACES	 TO REP-DETAIL1
               ELSE
005290             MOVE SPACES   TO REP-DETAIL1
		   MOVE 99	 TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL
009350		   PERFORM AB05 THRU AB10
           ELSE
009350         PERFORM AB05 THRU AB10.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO BE73.
	     MOVE DPT-CODE	 TO STK-LDG.
	   IF WS-SEQ = "C"
	       MOVE "!" 	 TO STK-CODE
009360	       PERFORM START-AT-STOCK-LEDG THRU READ-STOCK-EXIT
	   ELSE
	       MOVE "!" 	 TO STK-DESC
	       PERFORM START-AT-STOCK-DESC THRU READ-STOCK-EXIT.
009370     IF WS-F-ERROR = 22
009380	       GO TO BE72A.

009390 BE80.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
	   IF (WS-F-ERROR = 22)
009410	       IF (WS-DEPT > SPACES)
		   MOVE 99	 TO WS-ADVANCE
		   MOVE "E"	 TO WS-COMMAND
		   PERFORM CALL-PRINTUTL
		   MOVE "P"	 TO WS-COMMAND
009430		   GO TO BE73
009440	       ELSE
009450		   GO TO BE72.
	   IF (WS-SEQ = "C") AND
	      (DPT-CODE NOT = STK-LDG)
009410	       IF WS-DEPT > SPACES
		   MOVE 99	 TO WS-ADVANCE
		   MOVE "E"	 TO WS-COMMAND
		   PERFORM CALL-PRINTUTL
		   MOVE "P"	 TO WS-COMMAND
009430		   GO TO BE73
009440         ELSE
009450             GO TO BE72.
	   IF WS-SEQ = "D"
	       IF DPT-CODE NOT = STK-LDG
		   GO TO BE80.
	   IF NOT (WS-PRINTER = "S")
	       IF WS-USE-ITM = "Y"
		   DISPLAY S03-ITM
	       ELSE
	       IF WS-EXT-STK = "Y"
		   DISPLAY S03-EXT
	       ELSE
009490		   DISPLAY S03.
             PERFORM AA500.
	   IF WS-MAT = "Y"
	       PERFORM BG000.
	   IF WS-USE-ITM = "Y"
	       MOVE STK-CODE	 TO PRC-ITM
	   ELSE
009500	       MOVE STK-CODE	 TO PRC-ITEM.
009520	     MOVE STK-DESC	 TO PRC-DESC.
	   IF STK-IND = 4
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	   ELSE
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM AA500
	       PERFORM BE85 THRU BE95
009510	       MOVE STK-BIN	 TO PRC-BIN
	       IF (WS-ART = "Y") OR (WS-CST = "Y")
009680		   PERFORM CALL-PRINTUTL
		   MOVE SPACES TO REP-DETAIL1
		   IF WS-ART = "Y"
		       MOVE W35-CSTPRN
				 TO PRC-CODE
		   ELSE
009530		       MOVE STK-COST
				 TO PRC-CCODE.
	   IF NOT (STK-IND = 4)
009690	       MOVE STK-WSALE	 TO PRC-E-WSALE
009690	       MOVE STK-CASH	 TO PRC-E-CASH
009690	       MOVE STK-SELL	 TO PRC-E-RET
009690	       MOVE W10-VWSALE	 TO PRC-I-WSALE
009690	       MOVE W10-VCASH	 TO PRC-I-CASH
009690	       MOVE W10-VSELL	 TO PRC-I-RET.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009740	       PERFORM AB05 THRU AB10.
009750	     MOVE SPACES	 TO REP-DETAIL1.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO BE73.
	   IF STK-IND = 4
	       GO TO BE81.
	   IF WS-MAT = "N"
	       GO TO BE81.
	     MOVE 1		 TO WS-P1.

       BE80A.
	   IF W20-RATE(WS-P1) = ZERO
	       GO TO BE80B.
	     MOVE W20-RATE(WS-P1)
				 TO PRC-MRT.
	     MOVE "%"		 TO PRC-P.
	     MOVE W20-SELL(WS-P1)
				 TO PRC-E-RET.
000150	     MOVE W20-CASH(WS-P1)
				 TO PRC-E-CASH.
000160	     MOVE W20-WSALE(WS-P1)
				 TO PRC-E-WSALE.
000140	     MOVE W20-VSELL(WS-P1)
				 TO PRC-I-RET.
000150	     MOVE W20-VCASH(WS-P1)
				 TO PRC-I-CASH.
000160	     MOVE W20-VWSALE(WS-P1)
				 TO PRC-I-WSALE.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009740	       PERFORM AB05 THRU AB10.
009750	     MOVE SPACES	 TO REP-DETAIL1.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO BE73.

       BE80B.
	   IF WS-P1 < 10
	       ADD 1		 TO WS-P1
	       GO TO BE80A.

       BE81.
	   IF WS-USE-PACKS = "Y"
	       PERFORM BF000.

009760 BE82.
009780       GO TO BE80.

       BE85.
009530	     MOVE STK-COST	 TO W35-CST.
009540       MOVE W35-WHOLE      TO W35-CEDIT.
009550       MOVE 1              TO W35-S1.

009560 BE90.
009570     IF W35-CHAR (W35-S1) NOT NUMERIC
009580	       GO TO BE95.
009590     IF W35-NUM (W35-S1) = 0
009600         MOVE 10           TO W35-S2
009610     ELSE
009620         MOVE W35-NUM (W35-S1) 
                                 TO W35-S2.
009630       MOVE W30-CODE (W35-S2) 
                                 TO W35-CHAR (W35-S1).

009640 BE95.
009650     IF W35-S1 < 10
009660         ADD 1             TO W35-S1
009670	       GO TO BE90.

       BE99.
	     EXIT.

       BE999.
	     EXIT.

      /
      *    ****    P A C K S
      *
       BF000	       SECTION 50.
       BF00.
	   IF STK-USE-PACKS = "N"
	       GO TO BF999.
	     MOVE 2		 TO WS-S1.

       BF05.
	   IF STK-UNT(WS-S1) = ZERO
	       GO TO BF10.
	     COMPUTE W10-COST ROUNDED = STK-COST * STK-UNT(WS-S1).
	   IF WS-ART = "Y"
	       PERFORM BF15 THRU BF25
	       MOVE W35-CSTPRN	 TO PRC-CODE.
	     MOVE STK-PR(WS-S1)  TO PRC-PER.
	   IF WS-CST = "Y"
	       MOVE W10-COST	 TO PRC-CCODE.
	     MOVE STK-SL(WS-S1)  TO PRC-E-RET.
000150	     MOVE STK-CSH(WS-S1) TO PRC-E-CASH.
000160	     MOVE STK-WSL(WS-S1) TO PRC-E-WSALE.
	   IF STK-TAX > 0
               MOVE W05-VAT (STK-TAX)
				 TO W05-RATE
	       COMPUTE PRC-I-CASH ROUNDED
		   = STK-CSH(WS-S1) + (STK-CSH(WS-S1) * W05-RTE)
	       COMPUTE PRC-I-WSALE ROUNDED
		   = STK-WSL(WS-S1) + (STK-WSL(WS-S1) * W05-RTE)
	       COMPUTE PRC-I-RET ROUNDED
		   = STK-SL(WS-S1) + (STK-SL(WS-S1) * W05-RTE)
	   ELSE
	       MOVE STK-SL(WS-S1)
				 TO PRC-I-RET
	       MOVE STK-CSH(WS-S1)
				 TO PRC-I-CASH
	       MOVE STK-WSL(WS-S1)
				 TO PRC-I-WSALE.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009740	       PERFORM AB05 THRU AB10.
009750	     MOVE SPACES	 TO REP-DETAIL1.

       BF10.
	   IF WS-S1 < 4
	       ADD 1		 TO WS-S1
	       GO TO BF05.
	     GO TO BF999.

       BF15.
009530	     MOVE W10-COST	 TO W35-CST.
009540       MOVE W35-WHOLE      TO W35-CEDIT.
009550       MOVE 1              TO W35-S1.

009560 BF20.
009570     IF W35-CHAR (W35-S1) NOT NUMERIC
009580	       GO TO BF25.
009590     IF W35-NUM (W35-S1) = 0
009600         MOVE 10           TO W35-S2
009610     ELSE
009620         MOVE W35-NUM (W35-S1) 
                                 TO W35-S2.
009630       MOVE W30-CODE (W35-S2) 
                                 TO W35-CHAR (W35-S1).

009640 BF25.
009650     IF W35-S1 < 10
009660         ADD 1             TO W35-S1
009670	       GO TO BF20.

       BF999.
	     EXIT.
      /
      *    ****    M A T R I X	 P R I C E S
      *
       BG000	       SECTION 51.
       BG00.
	     MOVE 1		 TO WS-P1.

       BG05.
	     MOVE ZERO		 TO W20-RATE(WS-P1)
				    W20-SELL(WS-P1)
				    W20-CASH(WS-P1)
				    W20-WSALE(WS-P1)
				    W20-VSELL(WS-P1)
				    W20-VCASH(WS-P1)
				    W20-VWSALE(WS-P1).
	   IF WS-P1 < 10
	       ADD 1		 TO WS-P1
	       GO TO BG05.
	     MOVE 1		 TO WS-P1.

       BG10.
	   IF DPT-DSETT(WS-P1) = ZERO
	       GO TO BG15.
	     MOVE DPT-DSETT(WS-P1)
				 TO W20-RATE(WS-P1).
	     COMPUTE W20-SELL(WS-P1) ROUNDED = STK-SELL -
		    (STK-SELL * (W20-RATE(WS-P1) / 100.0000)).
	     COMPUTE W20-CASH(WS-P1) ROUNDED = STK-CASH -
		    (STK-CASH * (W20-RATE(WS-P1) / 100.0000)).
	     COMPUTE W20-WSALE(WS-P1) ROUNDED = STK-WSALE -
		    (STK-WSALE * (W20-RATE(WS-P1) / 100.0000)).
	     COMPUTE W20-VCASH(WS-P1) ROUNDED
		   = W20-CASH(WS-P1) + (W20-CASH(WS-P1) * W05-RTE).
	     COMPUTE W20-VWSALE(WS-P1) ROUNDED
		   = W20-WSALE(WS-P1) + (W20-WSALE(WS-P1) * W05-RTE).
	     COMPUTE W20-VSELL(WS-P1) ROUNDED
		   = W20-SELL(WS-P1) + (W20-SELL(WS-P1) * W05-RTE).

       BG15.
	   IF WS-P1 < 10
	       ADD 1		 TO WS-P1
	       GO TO BG10.

       BG999.
	   EXIT.
      /
      *    ****    P R I C E   L I S T S
      *
018610 ME000	       SECTION 60.
018620 ME00.
018630       DISPLAY CLR-SCREEN.
             MOVE WS-OPTION      TO WS-PRICE.

       ME05.
           IF WS-PRICE = "1"
	       DISPLAY WS-WS-HD AT 0231
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
		       " Price List"
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
           ELSE
           IF WS-PRICE = "2"
	       DISPLAY WS-CS-HD AT 0231
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
		       " Price List"
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
	   ELSE
	       DISPLAY WS-RT-HD AT 0232
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
		       " Price List"
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT.
             DISPLAY "Print memo details " AT 0408
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT.
             MOVE "N"            TO WS-MEMO.

       ME06.
             ACCEPT WS-MEMO AT 0431 
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO ME999
		 WHEN OTHER	 CALL X"E5"
		     GO TO ME06
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-MEMO
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-MEMO = "N" OR "Y")
               GO TO ME06.
	     DISPLAY "Print coded cost price " AT 0608
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE "Y"		 TO WS-ART.

       ME07.
	     ACCEPT WS-ART AT 0635
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-ART
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-ART = "N" OR "Y")
	       GO TO ME07.
004520	     DISPLAY "Press " AT 0808
		     "ENTER" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     " for all departments - Department "
		     "code" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     " for specific".

       ME08.
009050	     ACCEPT WS-DEPT AT 0871
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO ME05
		 WHEN OTHER	 CALL X"E5"
		     GO TO ME08
	       END-EVALUATE.
009080	   IF WS-DEPT = SPACES
	       GO TO ME12.
009330	     MOVE WS-DEPT	 TO DPT-CODE WS-CAT.
	   IF (DPT-CODE = "XXXX" OR "YYYY") OR
	      (DPT-SCDE = "VAT")
009070	       GO TO ME08.
009340	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       MOVE "Invalid department code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO ME08.
	     DISPLAY DPT-DESC AT 1012
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

       ME10.
	     MOVE "'C'ontinue or 'R'ekey  [ ]"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "R" OR "C")
               GO TO ME10.
           IF WS-OPTION = "R"
	       GO TO ME08.

       ME12.
	     MOVE "'D'escription or 'C'ode sequence  [C]"
				 TO WS-ERR-MES.
	     MOVE "C"		 TO WS-OPTION.

       ME14.
	     PERFORM OPT-MESSAGE.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO ME08
		 WHEN OTHER	 CALL X"E5"
		     GO TO ME14
	       END-EVALUATE.
	     MOVE WS-OPTION	 TO WS-SEQ.
	   IF NOT (WS-SEQ = "D" OR "C")
	       GO TO ME14.

       ME15.
009110       DISPLAY S06.
009120       DISPLAY S15.
009200	   IF WS-DEPT > SPACES
009210	       GO TO ME35.
	     PERFORM BE000.

009170 ME20.
	     PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       GO TO ME28.
	   IF (DPT-CODE = "XXXX" OR "YYYY") OR
	      (DPT-SCDE = "VAT")
	       GO TO ME25.
	     GO TO ME35.

009220 ME25.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO ME999.
009200	   IF WS-DEPT > SPACES
009210	       GO TO ME05.
009250	     GO TO ME20.

       ME28.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.

009270 ME30.
005120	     MOVE SPACES	 TO REP-DETAIL1.
009280       GO TO ME999.

009290 ME35.
	   IF NOT (WS-PRINTER = "S")
	       DISPLAY DPT-DESC AT 1012
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
           IF WS-PAGE > ZERO
009700	       IF W02-LINAGE < (W02-PRN-LENGTH - 8)
		   MOVE 1	 TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL
005250		   MOVE "Category:"
                                 TO REP-HEAD2
005260		   MOVE DPT-CODE TO REP-CATNO
005270             MOVE DPT-DESC TO REP-CAT
		   MOVE 2	 TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL
005290		   MOVE SPACES	 TO REP-DETAIL1
               ELSE
005290             MOVE SPACES   TO REP-DETAIL1
		   MOVE 99	 TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL
009350		   PERFORM ME60 THRU ME65
           ELSE
009350         PERFORM ME60 THRU ME65.
	     MOVE DPT-CODE	 TO STK-LDG.
	   IF WS-SEQ = "C"
	       MOVE "!" 	 TO STK-CODE
009360	       PERFORM START-AT-STOCK-LEDG THRU READ-STOCK-EXIT
	   ELSE
	       MOVE "!" 	 TO STK-DESC
	       PERFORM START-AT-STOCK-DESC THRU READ-STOCK-EXIT.
009370	   IF WS-F-ERROR = 22
009410         IF WS-DEPT > ZERO
		   MOVE 99	 TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL
009430		   GO TO ME30
009440         ELSE
009450             GO TO ME25.

009390 ME40.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO ME30.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
	   IF (WS-F-ERROR = 22)
009410	       IF (WS-DEPT > ZERO)
		   MOVE 99	 TO WS-ADVANCE
		   MOVE "E"	 TO WS-COMMAND
		   PERFORM CALL-PRINTUTL
		   MOVE "P"	 TO WS-COMMAND
009430		   GO TO ME30
009440	       ELSE
009450		   GO TO ME25.
	   IF (WS-SEQ = "C") AND
	      (DPT-CODE NOT = STK-LDG)
009410         IF WS-DEPT > ZERO
		   MOVE 99	 TO WS-ADVANCE
		   MOVE "E"	 TO WS-COMMAND
		   PERFORM CALL-PRINTUTL
		   MOVE "P"	 TO WS-COMMAND
009430		   GO TO ME30
009440         ELSE
009450		   GO TO ME25.
	   IF WS-SEQ = "D"
	       IF NOT (DPT-CODE = STK-LDG)
		   GO TO ME40.
	   IF NOT (WS-PRINTER = "S")
	       IF WS-USE-ITM = "Y"
		   DISPLAY S03-ITM
	       ELSE
	       IF WS-EXT-STK = "Y"
		   DISPLAY S03-EXT
	       ELSE
009490		   DISPLAY S03.
	     PERFORM AA500.
	   IF WS-MAT = "Y"
	       PERFORM BG000.
	   IF WS-USE-ITM = "Y"
	       MOVE STK-CODE	 TO PRI-ITM
	   ELSE
009500	       MOVE STK-CODE	 TO PRI-ITEM.
009520	     MOVE STK-DESC	 TO PRI-DESC.
	   IF STK-IND = 4
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       GO TO ME52.
009510       MOVE STK-BIN        TO PRI-BIN.
	   IF WS-ART = "Y"
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES TO REP-DETAIL1.
009530	     MOVE STK-COST	 TO W35-CST.
009540       MOVE W35-WHOLE      TO W35-CEDIT.
009550       MOVE 1              TO W35-S1.

009560 ME45.
009570     IF W35-CHAR (W35-S1) NOT NUMERIC
009580         GO TO ME50.
009590     IF W35-NUM (W35-S1) = 0
009600         MOVE 10           TO W35-S2
009610     ELSE
009620         MOVE W35-NUM (W35-S1) 
                                 TO W35-S2.
009630       MOVE W30-CODE (W35-S2) 
                                 TO W35-CHAR (W35-S1).

009640 ME50.
009650     IF W35-S1 < 10
009660         ADD 1             TO W35-S1
009670         GO TO ME45.
	   IF WS-ART = "Y"
009680	       MOVE W35-CSTPRN	 TO PRI-CCODE.
	   IF WS-PRICE = "1"
009690         MOVE STK-WSALE    TO PRI-EXCL
	       COMPUTE PRI-VAT ROUNDED = W10-VWSALE - STK-WSALE
009690         MOVE W10-VWSALE   TO PRI-INCL
           ELSE
           IF WS-PRICE = "2"
009690         MOVE STK-CASH     TO PRI-EXCL
	       COMPUTE PRI-VAT ROUNDED = W10-VCASH - STK-CASH
009690         MOVE W10-VCASH    TO PRI-INCL
           ELSE
009690         MOVE STK-SELL     TO PRI-EXCL
	       COMPUTE PRI-VAT ROUNDED = W10-VSELL - STK-SELL
009690         MOVE W10-VSELL    TO PRI-INCL.

       ME52.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009740	       PERFORM ME60 THRU ME65.
009750       MOVE SPACES TO REP-DETAIL1.
	   IF WS-USE-PACKS = "Y"
	       PERFORM MF000.
           IF WS-MEMO = "Y"
                PERFORM ME70 THRU ME85.

009760 ME55.
009780       GO TO ME40.

       ME60.
009990	     PERFORM AB35.
	     MOVE "H"		 TO W02-PRN-TYPE.
005120       MOVE SPACES         TO REP-DETAIL1.
005130	     MOVE "Page:"	 TO REP-PAGE.
005140	     MOVE "Date:"	 TO REP-DATE.
005150       MOVE W95-COMP       TO REP-COMPANY.
005160       ADD 1               TO WS-PAGE.
005170       MOVE WS-PAGE        TO REP-P-NO.
005180       MOVE W12-TODAY      TO REP-DTE.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
005200	     MOVE SPACES	 TO REP-DETAIL1.
	   IF WS-PRICE = "1"
	       STRING WS-WS-HD DELIMITED SIZE
005220		      "   P R I C E   L I S T" DELIMITED SIZE
				 INTO REP-COMPANY
           ELSE
           IF WS-PRICE = "2"
	       STRING WS-CS-HD DELIMITED SIZE
005220		      "   P R I C E   L I S T" DELIMITED SIZE
				 INTO REP-COMPANY
           ELSE
	       STRING WS-RT-HD DELIMITED SIZE
005220		      "   P R I C E   L I S T" DELIMITED SIZE
				 INTO REP-COMPANY.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
005200	     MOVE SPACES	 TO REP-DETAIL1.

005210 ME65.
      *
      *    ****    1 2	 C P I.
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 2		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
010010	     MOVE SPACES	 TO REP-DETAIL1.
005250	     MOVE "Category:"	 TO REP-HEAD2.
005260	     MOVE DPT-CODE	 TO REP-CATNO.
005270       MOVE DPT-DESC       TO REP-CAT.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
005290	     MOVE SPACES	 TO REP-DETAIL1.
005300	     MOVE "Item Code"	 TO PRI-H1.
005310	     MOVE "Bin" 	 TO PRI-H2.
005330	     MOVE "Description"  TO PRI-H4.
005340	     MOVE "Exclusive"	 TO PRI-H5.
             MOVE "V.A.T."       TO PRI-H6.
005340	     MOVE "Inclusive"	 TO PRI-H7.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
005360	     MOVE SPACES	 TO REP-DETAIL1.
	   IF WS-ART = "Y"
005320	       MOVE "Article"	 TO PRI-H3
	       PERFORM CALL-PRINTUTL
005360	       MOVE SPACES	 TO REP-DETAIL1.
	     MOVE ALL "Í"	 TO PRI-L1.
	     PERFORM CALL-PRINTUTL.
005360	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "D"		 TO W02-PRN-TYPE.

       ME70.
             MOVE STK-CODE       TO SME-CODE.
             MOVE 1              TO SME-SEQ.
             PERFORM READ-STKMEM THRU READ-STKMEM-EXIT.
           IF WS-F-ERROR NOT = ZERO
               GO TO ME85.
       ME75.
      *
      *    ****    1 7	 C P I.
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 3		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO REP-DETAIL2.
       ME80.
             MOVE SME-DATA       TO PRI-MEMO.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009740	       PERFORM ME60 THRU ME65
               PERFORM ME75.
010010       MOVE SPACES         TO REP-DETAIL1.
             PERFORM READ-STKMEM-NEXT THRU READ-STKMEM-EXIT.
           IF WS-F-ERROR = ZERO
               IF SME-CODE = STK-CODE
                   GO TO ME80.
       ME85.
      *
      *    ****    1 2	 C P I.
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 2		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
010010	     MOVE SPACES	 TO REP-DETAIL1.

       ME999.
             EXIT.
      /
      *    ****    P A C K S
      *
       MF000	       SECTION 60.
       MF00.
	   IF STK-USE-PACKS = "N"
	       GO TO MF999.
	     MOVE 2		 TO WS-S1.

       MF05.
	   IF STK-UNT(WS-S1) = ZERO
	       GO TO MF10.
	     COMPUTE W10-COST ROUNDED = STK-COST * STK-UNT(WS-S1).
	   IF WS-ART = "Y"
	       PERFORM MF15 THRU MF25
	       MOVE W35-CSTPRN	 TO PRI-CCODE.
	     MOVE STK-PR(WS-S1)  TO PRI-PER.
	   IF STK-TAX > 0
               MOVE W05-VAT (STK-TAX)
				 TO W05-RATE
	       COMPUTE W10-VCSH(WS-S1) ROUNDED
		     = STK-CSH(WS-S1) + (STK-CSH(WS-S1) * W05-RTE)
	       COMPUTE W10-VWSL(WS-S1) ROUNDED
		     = STK-WSL(WS-S1) + (STK-WSL(WS-S1) * W05-RTE)
	       COMPUTE W10-VSL(WS-S1) ROUNDED
		     = STK-SL(WS-S1) + (STK-SL(WS-S1) * W05-RTE)
	   ELSE
	       MOVE STK-SL(WS-S1)
				 TO W10-VSL(WS-S1)
	       MOVE STK-CSH(WS-S1)
				 TO W10-VCSH(WS-S1)
	       MOVE STK-WSL(WS-S1)
				 TO W10-VWSL(WS-S1).
	   IF WS-PRICE = "1"
009690	       MOVE STK-WSL(WS-S1)
				 TO PRI-EXCL
	       COMPUTE PRI-VAT ROUNDED
		     = W10-VWSL(WS-S1) - STK-WSL(WS-S1)
009690	       MOVE W10-VWSL(WS-S1)
				 TO PRI-INCL
           ELSE
           IF WS-PRICE = "2"
009690	       MOVE STK-CSH(WS-S1)
				 TO PRI-EXCL
	       COMPUTE PRI-VAT ROUNDED
		     = W10-VCSH(WS-S1) - STK-CSH(WS-S1)
009690	       MOVE W10-VCSH(WS-S1)
				 TO PRI-INCL
           ELSE
009690	       MOVE STK-SL(WS-S1)
				 TO PRI-EXCL
	       COMPUTE PRI-VAT ROUNDED
		     = W10-VSL(WS-S1) - STK-SL(WS-S1)
009690	       MOVE W10-VSL(WS-S1)
				 TO PRI-INCL.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009740	       PERFORM AB05 THRU AB10.
009750	     MOVE SPACES	 TO REP-DETAIL1.

       MF10.
	   IF WS-S1 < 4
	       ADD 1		 TO WS-S1
	       GO TO MF05.
	     GO TO MF999.

       MF15.
009530	     MOVE W10-COST	 TO W35-CST.
009540       MOVE W35-WHOLE      TO W35-CEDIT.
009550       MOVE 1              TO W35-S1.

009560 MF20.
009570     IF W35-CHAR (W35-S1) NOT NUMERIC
009580	       GO TO MF25.
009590     IF W35-NUM (W35-S1) = 0
009600         MOVE 10           TO W35-S2
009610     ELSE
009620         MOVE W35-NUM (W35-S1) 
                                 TO W35-S2.
009630       MOVE W30-CODE (W35-S2) 
                                 TO W35-CHAR (W35-S1).

009640 MF25.
009650     IF W35-S1 < 10
009660         ADD 1             TO W35-S1
009670	       GO TO MF20.

       MF999.
	     EXIT.
      /
019370 ZA000-INIT	       SECTION 81.
019380 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
019390	     DISPLAY CLR-SCREEN.
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
      *      CALL CBL_INIT_MOUSE USING MOUSE-HANDLE
      *				       MOUSE-BUTTONS
      *				 RETURNING MOUSE-STATUS.
      *    IF MOUSE-STATUS = ZERO
      *        MOVE "Y"		 TO MOUSE-ATTACHED.
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
      *	   *************************************************************
      *    **** 	  S T O C K   P A R T S   F I L E	    ****
      *    *************************************************************
	     MOVE "SPARTS"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SPARTS.
      *    *************************************************************
      *    **** 	   S T O C K   M E M O	 F I L E	    ****
      *    *************************************************************
	     MOVE "STKMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKMEM.
      *    *************************************************************
      *    **** 	       S T O C K   F I L E		    ****
      *    *************************************************************
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
019480	     MOVE 3		 TO WS-PARKEY.
044240       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.
	     MOVE PAR-PACKS	 TO WS-USE-PACKS.

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
	   IF WS-CS-HD = SPACES
	       MOVE "Cash"	 TO WS-CS-HD.
	   IF WS-WS-HD = SPACES
	       MOVE "Wholesale"	 TO WS-WS-HD.
	   IF WS-RT-HD = SPACES
	       MOVE "Retail"	 TO WS-RT-HD.
	     MOVE WS-CS-HD	 TO W15-PNAME.

       ZA05-NAME-LOOP.
	   IF W15-CHECK = SPACE
	       MOVE W15-REST	 TO W15-PNAME
	       GO TO ZA05-NAME-LOOP.

       ZA05-EXIT.
	     MOVE W15-PNAME	 TO WS-CS-HD.
	     MOVE WS-WS-HD	 TO W15-PNAME.
	     PERFORM ZA05-NAME-LOOP.
	     MOVE W15-PNAME	 TO WS-WS-HD.
	     MOVE WS-RT-HD	 TO W15-PNAME.
	     PERFORM ZA05-NAME-LOOP.
	     MOVE W15-PNAME	 TO WS-RT-HD.
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
041250	     MOVE WS-G1 	 TO WS-TCHR(WS-S1) WS-BCHR(WS-S1).
	     MOVE WS-G8 	 TO WS-BCH(WS-S1).
041260     IF WS-S1 < 80
041270	       ADD 1		 TO WS-S1
041280         GO TO ZA60.
	     MOVE WS-G11	 TO WS-BCH(1).
	     MOVE WS-G12	 TO WS-BCH(80).
	     MOVE WS-G14	 TO WS-TCHR(1)	WS-BCHR(1).
	     MOVE WS-G13	 TO WS-TCHR(80) WS-BCHR(80).
021930	     MOVE WS-G2 	 TO WS-TCHR(18) WS-TCHR(49)
021940				    WS-TCHR(60) WS-TCHR(70).
021950	     MOVE WS-G3 	 TO WS-MCHR(18) WS-MCHR(49)
021960				    WS-MCHR(60) WS-MCHR(70)
				    WS-MCHR(1)	WS-MCHR(80).
021970	     MOVE WS-G4 	 TO WS-BCHR(18) WS-BCHR(49)
021980				    WS-BCHR(60) WS-BCHR(70).
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
020010 ZB000-ERROR	       SECTION 82.
      
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
