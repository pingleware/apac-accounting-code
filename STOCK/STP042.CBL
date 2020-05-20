      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	  ****	      ***    ******    *
      *   **	**     **     **   **	 **  **      ****   **	  **   *
      *   **	       **     **   **	**    **    ** **	 **    *
      *    ******      **     ******	**    **   **  **      **      *
      * 	**     **     **	**    **  ********   **        *
      *   **	**     **     **	 **  **        **   **	       *
      *    ******      **     **	  ****	      ****  ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      * S T O C K   D E P A R M E N T A L   R E P O R T	  P R O G R A M*
      *                                                                *
      *     SPECIAL SPARES VERSION 1989 - WEEKLY TOTALS & X-REF        *
      * 							       *
      *     VERSION 8.13.09 - November 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STP042.
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
003190 77  WS-CAT	   PIC	9(04)	 COMP-5.
003200 77  WS-SCAT	   PIC	X(04).
003230 77  WS-PARKEY	   PIC	9(04)	 COMP-5.
003250 77  WS-NETKEY	   PIC	9(04)	 COMP-5.
003270 77  WS-PAGE	   PIC	9(04)	 COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
       77  WS-PRC-IND	   PIC	X(01).
003310 77  WS-WS	   PIC	X(01) VALUE "N".
003310 77  WS-MEMO	   PIC	X(01) VALUE "N".
003310 77  WS-PRT	   PIC	X(01) VALUE "Y".
003320 77  WS-BIN	   PIC	X(01).
       77  WS-ETYPE	   PIC	X(01).
003330 77  WS-OPTION	   PIC	X(01).
003330 77  WS-ZLT	   PIC	X(01) VALUE "Y".
003330 77  WS-ZQT	   PIC	X(01) VALUE "Y".
003330 77  WS-ROC	   PIC	X(01) VALUE "R".
       77  WS-FORMAT	   PIC	9(01) VALUE 1.
003350 77  WS-ERROR	   PIC	9(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
003470 77  WS-ORD	   PIC S9(05)	 COMP-3.
003480 77  WS-TOTAL	   PIC S9(09)V99 COMP-3.
003490 77  WS-VALUE	   PIC S9(09)V99 COMP-3.
       77  WS-QUANT	   PIC S9(09)V999 COMP-3.
003500 77  WS-CCOST	   PIC S9(09)V999 COMP-3.
003510 77  WS-CSELL	   PIC S9(09)V999 COMP-3.
003520 77  WS-SCOST	   PIC S9(09)V99 COMP-3.
003530 77  WS-SSELL	   PIC S9(09)V99 COMP-3.
       77  WS-TQUANT	   PIC S9(09)V99 COMP-3.
003540 77  WS-TCOST	   PIC S9(09)V99 COMP-3.
003550 77  WS-TSELL	   PIC S9(09)V99 COMP-3.
003560 77  WS-DEPT	   PIC	X(04).
003570 77  WS-WEEK	   PIC	9(01).
       77  WS-LABCDE	   PIC	X(08).
001330 77  WS-DESC-H1	    PIC	 X(14) VALUE "Description".
       77  WS-DESC-H2	    PIC	 X(14) VALUE "Description 2".
       77  WS-RT-HD	    PIC	 X(10) VALUE "-Retail   ".
       77  WS-WS-HD	    PIC	 X(10) VALUE "-Wholesale".
       77  WS-CS-HD	    PIC	 X(10) VALUE "-Cash sale".
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
000910	       05  FILLER      PIC  X(10).
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
001600	       05  FILLER      PIC  X(02).
001610	       05  REP-DCOST   PIC  Z(08)9.99.
001620	       05  FILLER      PIC  X(02).
001630	       05  REP-DSELL   PIC  Z(08)9.99.
001640	       05  FILLER      PIC  X(13).
	   03  PRC-L1 REDEFINES W02-PRN-LINE.
	       05  PRC-H1      PIC  X(15).
	       05  PRC-H2      PIC  X(06).
	       05  PRC-H3      PIC  X(09).
	       05  PRC-H4      PIC  X(32).
	       05  PRC-H5.
		   07  FILLER  PIC  X(01).
		   07  PRC-H5A PIC  X(12).
	       05  PRC-H6.
		   07  FILLER  PIC  X(05).
		   07  PRC-H6A PIC  X(04).
	       05  PRC-H7.
		   07  FILLER  PIC  X(06).
		   07  PRC-H7A PIC  X(06).
	       05  FILLER      PIC  X(02).
	       05  PRC-H8.
		   07  FILLER  PIC  X(01).
		   07  PRC-H8A PIC  X(12).
	       05  PRC-H9.
		   07  FILLER  PIC  X(05).
		   07  PRC-H9A PIC  X(04).
	       05  PRC-H10.
		   07  FILLER  PIC  X(06).
		   07  PRC-H10A PIC X(06).
000730	   03  PRC-L2 REDEFINES W02-PRN-LINE.
000750	       05  PRC-ITM     PIC  X(14).
000760	       05  FILLER      PIC  X(01).
000770	       05  PRC-BIN     PIC  X(03).
000780	       05  FILLER      PIC  X(01).
000790	       05  PRC-CCODE   PIC  Z(06)9.99.
000800	       05  FILLER      PIC  X(01).
000810	       05  PRC-DESC    PIC  X(30).
000820	       05  FILLER      PIC  X(01).
	       05  PRC-E-WSALE PIC  Z(07)9.99.
	       05  FILLER      PIC  X(01).
000830	       05  PRC-E-CASH  PIC  Z(07)9.99.
000820	       05  FILLER      PIC  X(01).
	       05  PRC-E-RET   PIC  Z(07)9.99.
	       05  FILLER      PIC  X(01).
000830	       05  PRC-I-WSALE PIC  Z(07)9.99.
000840	       05  FILLER      PIC  X(01).
	       05  PRC-I-CASH  PIC  Z(07)9.99.
	       05  FILLER      PIC  X(01).
000830	       05  PRC-I-RET   PIC  Z(07)9.99.
	   03  PRI-L1 REDEFINES W02-PRN-LINE.
	       05  PRI-H1      PIC  X(15).
	       05  PRI-H2      PIC  X(06).
	       05  PRI-H3      PIC  X(08).
	       05  PRI-H4      PIC  X(33).
	       05  PRI-H5      PIC  X(15).
	       05  PRI-H6      PIC  X(09).
	       05  PRI-H7      PIC  X(09).
000730	   03  PRC-L2 REDEFINES W02-PRN-LINE.
000750	       05  PRI-ITM     PIC  X(14).
000760	       05  FILLER      PIC  X(01).
000770	       05  PRI-BIN     PIC  X(03).
000780	       05  FILLER      PIC  X(01).
000790	       05  PRI-CCODE   PIC  X(09).
000800	       05  FILLER      PIC  X(01).
000810	       05  PRI-DESC    PIC  X(31).
000820	       05  FILLER      PIC  X(01).
	       05  PRI-EXCL    PIC  Z(07)9.99.
	       05  FILLER      PIC  X(01).
000830	       05  PRI-VAT     PIC  Z(07)9.99.
000820	       05  FILLER      PIC  X(01).
000830	       05  PRI-INCL    PIC  Z(07)9.99.
	   03  PRI-L3 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(42).
	       05  PRI-MEMO    PIC  X(60).
	   03  DEP-L1 REDEFINES W02-PRN-LINE.
	       05  DEP-H1      PIC  X(19).
	       05  DEP-H3      PIC  X(32).
	       05  DEP-H2      PIC  X(07).
	       05  DEP-H4      PIC  X(10).
	       05  DEP-H5      PIC  X(14).
	       05  DEP-H6      PIC  X(11).
	       05  DEP-H7      PIC  X(08).
	       05  DEP-H8.
		   07  FILLER  PIC  X(01).
		   07  DEP-H8A PIC  X(06).
	       05  DEP-H9.
		   07  FILLER  PIC  X(02).
		   07  DEP-H9A PIC  X(04).
	       05  DEP-H10.
		   07  DEP-H10A PIC X(08).
	       05  FILLER      PIC  X(02).
	       05  DEP-H11     PIC  X(08).
      *	       05  DEP-H12     PIC  X(05).
	   03  DEP-L2 REDEFINES W02-PRN-LINE.
	       05  DEP-EXT-ITEM.
		   07  DEP-ITEM
			       PIC  X(14).
		   07  DEP-SL  PIC  X(01).
		   07  DEP-ITM PIC  X(03).
	       05  FILLER      PIC  X(01).
	       05  DEP-DESC    PIC  X(28).
	       05  FILLER      PIC  X(01).
	       05  DEP-BIN     PIC  X(06).
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
	       05  DEP-ACT     PIC  Z9/99/9999 BLANK WHEN ZERO.

	   03  DEP-L3 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(19).
	       05  DEP-T-HEAD.
		   07  FILLER  PIC  X(18).
		   07  DEP-T-LEDG
			       PIC  X(04).
		   07  FILLER  PIC  X(08).
      *	       05  FILLER      PIC  X(11).
	       05  DEP-T-H1    PIC  X(06).
	       05  DEP-T-L1.
		   07  DEP-TQUANT
			       PIC  Z(08)9.999-.
	       05  FILLER      PIC  X(01).
	       05  DEP-T-H2    PIC  X(05).
	       05  DEP-T-L2.
		   07  DEP-TCOST
			       PIC  Z(08)9.999-.
	       05  FILLER      PIC  X(01).
	       05  DEP-T-H3    PIC  X(08).
	       05  DEP-T-L3.
		   07  DEP-TSELL
			       PIC  Z(08)9.999-.
      *	       05  FILLER      PIC  X(10).
	       05  FILLER      PIC  X(20).

	   03  DP2-L1 REDEFINES W02-PRN-LINE.
	       05  DP2-H1      PIC  X(19).
	       05  DP2-H2      PIC  X(30).
	       05  DP2-H3      PIC  X(07).
	       05  DP2-H4      PIC  X(11).
	       05  DP2-H5      PIC  X(10).
	       05  DP2-H6      PIC  X(12).
	       05  DP2-H7      PIC  X(17).
	       05  DP2-H8      PIC  X(17).
	       05  DP2-H9      PIC  X(08).

	   03  DP2-L2 REDEFINES W02-PRN-LINE.
	       05  DP2-EXT-ITEM.
		   07  DP2-ITEM
			       PIC  X(14).
		   07  DP2-SL  PIC  X(01).
		   07  DP2-ITM PIC  X(03).
	       05  FILLER      PIC  X(01).
	       05  DP2-DESC    PIC  X(28).
	       05  FILLER      PIC  X(01).
	       05  DP2-QNT     PIC  Z(06)9-.
	       05  DP2-COST    PIC  Z(05)9.99.
	       05  FILLER      PIC  X(01).
	       05  DP2-AVG     PIC  Z(05)9.99.
	       05  FILLER      PIC  X(01).
	       05  DP2-RET     PIC  Z(06)9.99.
	       05  FILLER      PIC  X(01).
	       05  DP2-MRKV    PIC  Z(05)9.99.
	       05  FILLER      PIC  X(01).
	       05  DP2-MRKP    PIC  ZZ9.99.
	       05  FILLER      PIC  X(01).
	       05  DP2-SYSV    PIC  Z(05)9.99.
	       05  FILLER      PIC  X(01).
	       05  DP2-SYSP    PIC  ZZ9.99.
	       05  FILLER      PIC  X(01).
	       05  DP2-ACT     PIC  Z9/99/9999 BLANK WHEN ZERO.

003740 01  W05.
003750     03  W05-S OCCURS 300   PIC 9(04)     COMP-5.

       COPY W05.VAT.

       COPY W10.STK.

       COPY W12.WS.

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
004460                           PIC X(14) USING STK-CODE.

004470 01  S04.
004480	   03  LINE 10 COLUMN 20 FOREGROUND-COLOR 14
004490                           VALUE "Busy loading stock departments".

004570 01  S07.
004580	   03  LINE 10 COLUMN 20 FOREGROUND-COLOR 14 VALUE
004590                           "Busy printing departmental report".

004600 01  S08.
004610	   03  LINE  8 COLUMN 20 VALUE "Select department".

004620 01  S09.
004630	   03  LINE 25 COLUMN 20  BACKGROUND-COLOR 3
				  FOREGROUND-COLOR 14 VALUE
004640                           "No details for department - ENTER".

005040 01  S15.
005050     03  LINE 16 COLUMN 12 VALUE "Stock record:".

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

       COPY DATE.CNV.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP3.

      /
005810 AB000              SECTION.
005370 AB15.
	     MOVE "H"		 TO W02-PRN-TYPE.
005380       MOVE SPACES         TO REP-DETAIL1.
005390	     MOVE "Page:"	 TO REP-PG.
005400	     MOVE "Date:"	 TO REP-DAT.
005410       MOVE W95-COMP       TO REP-COMP.
005420       ADD 1               TO WS-PAGE.
005430       MOVE WS-PAGE        TO REP-P-N.
005440       MOVE W12-TODAY      TO REP-DT.
005450       MOVE "STOCK BY DEPARTMENT" 
                                 TO REP-REPORT.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
005470	     MOVE SPACES	 TO REP-DETAIL1.
005480	     MOVE "Category:"	 TO REP-HEAD2.
005490       MOVE DPT-SALES-NO   TO REP-CATNO.
005500       MOVE DPT-DESC       TO REP-CAT.
	     PERFORM CALL-PRINTUTL.
005520	     MOVE SPACES	 TO REP-DETAIL1.
	   IF WS-FORMAT = 1
005530	       MOVE "Item Code"	 TO DEP-H1
005540	       MOVE "Bin"	 TO DEP-H2
005570	       MOVE WS-DESC-H1	 TO DEP-H3
005580	       MOVE "Quantity"	 TO DEP-H4
005590	       MOVE "Cost-Price" TO DEP-H5
005600	       MOVE "Average"	 TO DEP-H6
	       IF WS-ROC = "C"
		   MOVE "T-Avrg" TO DEP-H7
	       ELSE
005610		   MOVE WS-RT-HD TO DEP-H7
	       END-IF
	       MOVE ALL "_"	 TO DEP-H8 DEP-H10
005620	       MOVE "Markup"	 TO DEP-H9
005660	       MOVE "Active"	 TO DEP-H11
	   ELSE
	       MOVE "Item Code"  TO DP2-H1
005570	       MOVE WS-DESC-H1	 TO DP2-H2
	       MOVE "Quant."	 TO DP2-H3
	       MOVE "Cst-Price"  TO DP2-H4
	       MOVE "Average"	 TO DP2-H5
	       MOVE "Selling"	 TO DP2-H6
	       MOVE "Actual Markup"
				 TO DP2-H7
	       MOVE "System Markup"
				 TO DP2-H8
005660	       MOVE "Active"	 TO DP2-H9.
005650*      MOVE "GP-%"	 TO DEP-H12.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
005690	     MOVE SPACES	 TO REP-DETAIL1.
	   IF WS-FORMAT = 1
005640	       MOVE WS-WS-HD	 TO DEP-H8
005610	       MOVE WS-CS-HD	 TO DEP-H9
005620	       MOVE WS-RT-HD	 TO DEP-H10A
	   ELSE
	       MOVE "Value     %"
				 TO DP2-H7 DP2-H8.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
005690	     MOVE SPACES TO REP-DETAIL1.
	     MOVE "D"		 TO W02-PRN-TYPE.

005700 AB20.
005710       EXIT.
      *
      *    ****   1 7	C P I	( C O N D E N S E D   P R I N T )
      *
       AB25.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 3		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE.
009860	     MOVE SPACES	 TO REP-DETAIL1.

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
008500 BE00S            SECTION.
       BE00.
009800       DISPLAY CLR-SCREEN.
             DISPLAY "DEPARTMENTAL REPORT" AT 0231
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     PERFORM SAVE-SCREEN-2.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0606
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "¿" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "³    " AT 0706
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "S E L E C T   R E P O R T   F O R M A T   R E Q U
      -		     "I R E D    ³" WITH FOREGROUND-COLOR 7 HIGHLIGHT
					 BACKGROUND-COLOR 7.
	     DISPLAY "³                               " AT 0806
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "                                  ³"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "³    " AT 0906
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "Format 1,2    " WITH FOREGROUND-COLOR 5
					   BACKGROUND-COLOR 7
		     "                                               ³"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "³                               " AT 1006
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "                                  ³"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "³ All formats print: Item Code,Description & date
      -		     "last active.    " AT 1106
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "³" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "³ 1 = Bin,Quantity (3dec),Cost,Average,Selling,Mar
      -		     "kup 1,2 & 3.    " AT 1206
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "³" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "³ 2 = Quant (No dec),Cost,Average,Selling,Markup A
      -		     "ctual - Value & " AT 1306
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "³" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "³     percent,System Markup - Value & percent.
      -		     "                " AT 1406
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "³" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "³                               " AT 1506
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "                                  ³"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "³ Note - Additional formats may be added later.
      -		     "                " AT 1606
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "³" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "À" AT 1706
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 8		 TO SHADE-COL.
	     MOVE 65		 TO SHADE-WIDTH.
	     MOVE 11		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.
       BE02.
	     ACCEPT WS-FORMAT AT 0925
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF WS-FORMAT < 1 OR > 2
	       GO TO BE02.
	     PERFORM RESTORE-SCREEN-2.
010680	     MOVE ZERO		 TO WS-TCOST WS-TSELL WS-TQUANT.
	     DISPLAY "Print stock with Lead Time = 0.00" AT 0404.

       BE05.
	     ACCEPT WS-ZLT AT 0439
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-ZLT
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-ZLT = "Y" OR "N")
	       GO TO BE05.
	     DISPLAY "Print stock with zero quantity" AT 0604.

       BE10.
	     ACCEPT WS-ZQT AT 0635
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-ZQT
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-ZQT = "Y" OR "N")
	       GO TO BE10.
	   IF WS-FORMAT = 2
	       MOVE "R" 	 TO WS-ROC
	       GO TO BE50.
	     DISPLAY "Print " AT 0804
		     "R" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "etail price or total "
		     "C" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "ost".

       BE15.
	     ACCEPT WS-ROC AT 0837
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-ROC
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-ROC = "R" OR "C")
	       GO TO BE15.

       BE50.
	     DISPLAY "A" AT 1004 WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "ll or "
		     "S" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "elected Departments".
	     MOVE SPACE 	 TO WS-OPTION.

       BE55.
	     ACCEPT WS-OPTION AT 1033
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "A" OR "S")
	       GO TO BE55.
	   IF WS-OPTION = "S"
	       GO TO BE110.

009790 BE85.
009810       DISPLAY S07.
	     MOVE "'C'ontinue or 'E'xit  [ ]"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.

       BE86.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "E" OR "C")
	       GO TO BE85.
           IF WS-OPTION = "E"
	       GO TO BE999.
009820	     DISPLAY S15.
             PERFORM AB25.

009870 BE90.
010090	     PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 0
	       IF (DPT-CODE = "XXXX" OR "YYYY") OR
		  (DPT-SCDE = "VAT")
		   GO TO BE90
	       ELSE
009890		   GO TO BE95.

009900 BE92.
	   IF WS-OPTION = "X"
	       GO TO BE999.
	     MOVE ALL "Ä"	 TO DEP-T-L1
				    DEP-T-L2
				    DEP-T-L3.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
009980	     MOVE SPACES	 TO REP-DETAIL1.
009940	     MOVE "**** STOCK TOTAL ****"
				 TO DEP-T-HEAD.
	     MOVE WS-TQUANT	 TO DEP-TQUANT.
009950	     MOVE WS-TCOST	 TO DEP-TCOST.
009960	     MOVE WS-TSELL	 TO DEP-TSELL.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
009980	     MOVE SPACES	 TO REP-DETAIL1.
009990       PERFORM AB35.

010020 BE93.
010030	     GO TO BE999.

010040 BE95.
010050	     MOVE 0		 TO WS-S1.
	     MOVE "Y"		 TO WS-PRT.
	   IF NOT (WS-PRINTER = "S")
	       DISPLAY DPT-DESC AT 1212
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
	     MOVE DPT-CODE	 TO STK-LDG WS-SCAT.
	     MOVE "!"		 TO STK-CODE.
009360       PERFORM START-AT-STOCK-LEDG THRU READ-STOCK-EXIT.
009370     IF WS-F-ERROR = 22
	       MOVE "X" 	 TO WS-OPTION
010130         GO TO BE106.
010680	     MOVE ZERO		 TO WS-CCOST WS-CSELL WS-QUANT.

010140 BE100.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
	   IF (WS-F-ERROR = 22) OR (STK-LDG NOT = WS-SCAT)
010160         GO TO BE105.
	   IF WS-ZLT = "N"
	       IF STK-LTIME = ZERO
		   GO TO BE100.
	   IF WS-ZQT = "N"
	       IF STK-QUANT = ZERO
		   GO TO BE100.
	   IF WS-PRT = "Y"
010060	       IF WS-PAGE > ZERO
		   MOVE "S"	 TO WS-COMMAND
		   PERFORM CALL-PRINTUTL
		   MOVE "P"	 TO WS-COMMAND
	       END-IF
010100	       PERFORM AB15
	       MOVE "N"		 TO WS-PRT.
	   IF STK-IND > 2
	       MOVE ZERO	 TO STK-QUANT.
	   IF STK-IND = 1 AND WS-PRC-IND = "N"
		   MOVE ZERO	 TO STK-QUANT.
	   IF NOT (WS-PRINTER = "S")
010200	       DISPLAY S03.
	     PERFORM PRINT-ABORT.
           IF WS-OPTION = "Y"
014970         MOVE "**** PRINT ABORTED BY OPERATOR ****" 
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "X" 	 TO WS-OPTION
014990	       GO TO BE106.
	   IF WS-USE-ITM = "Y"
	       MOVE STK-ITEM	 TO DEP-ITEM
	       MOVE "/" 	 TO DEP-SL
	       MOVE STK-ITM	 TO DEP-ITM
	   ELSE
010210	       MOVE STK-CODE	 TO DEP-EXT-ITEM.
010250       MOVE STK-DESC       TO DEP-DESC.
	   IF STK-IND = 4
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       GO TO BE101.
	   IF NOT (WS-FORMAT = 1)
	       PERFORM BE103
	       GO TO BE101.
010220	     MOVE STK-BIN	 TO DEP-BIN.
010260	     MOVE STK-QUANT	 TO DEP-QNT.
010270       MOVE STK-COST       TO DEP-COST.
      *    ****    A V E R A G E   C O S T
	   IF STK-QUANT < 0.001
	       MOVE STK-COST	 TO STK-AVGE.
	   IF STK-IND < 2
016950	       IF STK-AVGE = ZERO
016960		   MOVE STK-COST TO STK-AVGE.
010290	     MOVE STK-AVGE	 TO DEP-AVG.
	   IF WS-ROC = "C"
	       COMPUTE DEP-RET ROUNDED = STK-QUANT * STK-AVGE
	   ELSE
010280	       MOVE STK-SELL	 TO DEP-RET.
010310	     COMPUTE DEP-MWS ROUNDED = ((STK-WSALE - STK-COST)
010320                                 * 100.000) / STK-COST. 
010310       COMPUTE DEP-MRET ROUNDED = ((STK-SELL - STK-COST)
010320                                  * 100.000) / STK-COST. 
010330       COMPUTE DEP-MCSH ROUNDED = ((STK-CASH - STK-COST)
010340                                  * 100.000) / STK-COST. 
010350	     MOVE STK-MTDV	 TO WS-SSELL.
010360	     MOVE STK-MTDC	 TO WS-SCOST.
010370     IF WS-SSELL = ZERO OR WS-SCOST = ZERO
010380	       MOVE STK-COST	 TO WS-SCOST
010390         COMPUTE WS-SSELL = (STK-SELL + STK-CASH) / 2.000 + .005.
010400*       COMPUTE DEP-GP ROUNDED = ((WS-SSELL - WS-SCOST)
010410* 			       * 100.000) / WS-SSELL.
021150	   IF STK-ACT = ZERO
021160	       MOVE STK-ACT	 TO DEP-ACT
021170     ELSE
021180	       MOVE STK-ACT	 TO W22-DTE2
	       PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT
	       MOVE W22-DTE3	 TO DEP-ACT.
010440	     MULTIPLY STK-AVGE BY STK-QUANT
				 GIVING WS-VALUE.
010450	     MULTIPLY STK-SELL BY STK-QUANT
				 GIVING WS-TOTAL.
	     ADD STK-QUANT	 TO WS-QUANT.
010460	     ADD WS-VALUE	 TO WS-CCOST.
010470	     ADD WS-TOTAL	 TO WS-CSELL.

       BE101.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
010520	       PERFORM AB15.
010530	     MOVE SPACES	 TO REP-DETAIL1.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "X" 	 TO WS-OPTION
014990         GO TO BE106.

010540 BE102.
010560       GO TO BE100.

       BE103.
010260	     MOVE STK-QUANT	 TO DP2-QNT.
010270	     MOVE STK-COST	 TO DP2-COST.
      *    ****    A V E R A G E   C O S T
	   IF STK-QUANT < 0.001
	       MOVE STK-COST	 TO STK-AVGE.
	   IF STK-IND < 2
016950	       IF STK-AVGE = ZERO
016960		   MOVE STK-COST TO STK-AVGE.
010290	     MOVE STK-AVGE	 TO DP2-AVG.
010280	     MOVE STK-SELL	 TO DP2-RET.
	     COMPUTE W25-VALUE = STK-SELL - STK-COST.
	     MOVE W25-VALUE	 TO DP2-MRKV.
010310	     COMPUTE DP2-MRKP ROUNDED = ((STK-SELL - STK-COST)
010320                                  * 100.000) / STK-COST. 
	     MOVE STK-MARKUP	 TO DP2-SYSP.
	     COMPUTE DP2-SYSV ROUNDED = STK-COST * STK-MARKUP / 100.000.
	     MOVE STK-MARKUP	 TO DP2-SYSP.
021150	   IF STK-ACT = ZERO
021160	       MOVE STK-ACT	 TO DP2-ACT
021170     ELSE
021180	       MOVE STK-ACT	 TO W22-DTE2
	       PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT
	       MOVE W22-DTE3	 TO DP2-ACT.
010440	     MULTIPLY STK-AVGE BY STK-QUANT
				 GIVING WS-VALUE.
010450	     MULTIPLY STK-SELL BY STK-QUANT
				 GIVING WS-TOTAL.
	     ADD STK-QUANT	 TO WS-QUANT.
010460	     ADD WS-VALUE	 TO WS-CCOST.
010470	     ADD WS-TOTAL	 TO WS-CSELL.

       BE104.
	     EXIT.

010570 BE105.
	   IF WS-PRT = "Y"
	       GO TO BE106.
010580	   IF W02-LINAGE > (W02-PRN-LENGTH - 4)
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
010600	       PERFORM AB15.
010610       MOVE SPACES         TO REP-DETAIL1.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
010630	     MOVE "DEPARTMENT TOTAL (    )"
				 TO DEP-T-HEAD.
	     MOVE DPT-CODE	 TO DEP-T-LEDG.
	     MOVE "QUANT"	 TO DEP-T-H1.
	     MOVE "AVRG"	 TO DEP-T-H2.
	     MOVE "SELLING"	 TO DEP-T-H3.
	     MOVE WS-QUANT	 TO DEP-TQUANT.
010640	     MOVE WS-CCOST	 TO DEP-TCOST.
010650	     MOVE WS-CSELL	 TO DEP-TSELL.
010660	     ADD WS-QUANT	 TO WS-TQUANT.
010660       ADD WS-CCOST        TO WS-TCOST.
010670       ADD WS-CSELL        TO WS-TSELL.
010680	     MOVE ZERO		 TO WS-CCOST WS-CSELL WS-QUANT.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
010700	     MOVE SPACES	 TO REP-DETAIL1.

       BE106.
             EXIT.

010710 BE108.
010720	     GO TO BE90.

010730 BE110.
010740       DISPLAY CLR-SCREEN.
             DISPLAY "SELECTIVE DEPARTMENTAL REPORT" AT 0226
                     WITH FOREGROUND-COLOR 15.
010750       DISPLAY S08.
010760       DISPLAY S15.
010770	     ACCEPT WS-DEPT AT 0838
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
010780	   IF WS-DEPT = SPACES
010790	       GO TO BE999.
010800	     MOVE WS-DEPT	 TO DPT-CODE.
	     DISPLAY CLEAR-L25.
010810	   IF (DPT-CODE = "XXXX" OR "YYYY") OR
	      (DPT-SCDE = "VAT")
	       GO TO BE110.
010090       PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
010820	       DISPLAY S09
010830	       ACCEPT WS-OPTION AT 2554
		      WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO
010840         GO TO BE110.
	     DISPLAY DPT-DESC AT 1212
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
       BE115.
	     MOVE "'C'ontinue or 'R'ekey  [ ]"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "R" OR "C")
               GO TO BE115.
           IF WS-OPTION = "R"
               GO TO BE110.
	     PERFORM AB25.
010880	     PERFORM BE95 THRU BE106.
	   IF WS-OPTION = "X"
	       GO TO BE999.
	     PERFORM AB35
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
010900	     MOVE ZERO		 TO WS-PAGE.
010910	     GO TO BE110.

       BE999.
	     EXIT.

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
	     MOVE PAR-PRICED-IND TO WS-PRC-IND.
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
	     INSPECT WS-RT-HD REPLACING LEADING "-" BY " ".
	     INSPECT WS-CS-HD REPLACING LEADING "-" BY " ".
	     INSPECT WS-WS-HD REPLACING LEADING "-" BY " ".
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
041870         MOVE ZERO         TO WS-KEY
               MOVE PRT-ITEM     TO WS-KEYX
041840	   ELSE
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
