      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	  ****	  ********   ******    *
      *   **	**     **     **   **	 **  **   **	    **	  **   *
      *   **	       **     **   **	**    **  *******	 **    *
      *    ******      **     ******	**    **	**     **      *
      * 	**     **     **	**    **	**   **        *
      *   **	**     **     **	 **  **   **	**  **	       *
      *    ******      **     **	  ****	   ******   ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     R E D U N D A N T	S T O C K   R E P O R T   P R O G      *
      *                                                                *
      *     SPECIAL SPARES VERSION        WEEKLY TOTALS & X-REF        *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STP052.
000030 AUTHOR.	       J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   MARCH 1990.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1990 - 2008
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
003200 77  WS-SCAT	   PIC	9(04)	 COMP-5.
003230 77  WS-PARKEY	   PIC	9(04)	 COMP-5.
003250 77  WS-NETKEY	   PIC	9(04)	 COMP-5.
003270 77  WS-PAGE	   PIC	9(04)	 COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
003300 77  WS-PQNT	   PIC	X(01).
003310 77  WS-WS	   PIC	X(01) VALUE "N".
003310 77  WS-MEMO	   PIC	X(01) VALUE "N".
003310 77  WS-PRT	   PIC	X(01).
003320 77  WS-BIN	   PIC	X(01).
       77  WS-ETYPE	   PIC	X(01).
003330 77  WS-OPTION	   PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
003330 77  WS-PRICE	   PIC	X(01).
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
001650	   03  INV-L1 REDEFINES W02-PRN-LINE.
001660	       05  INV-H1      PIC X(15).
001670	       05  INV-H2      PIC X(03).
001680	       05  INV-H3      PIC X(21).
001690	       05  INV-H4      PIC X(09).
001700	       05  INV-H5      PIC X(09).
001710	       05  INV-H6      PIC X(09).
001720	       05  INV-H7      PIC X(12).
001730	       05  INV-H8      PIC X(09).
001740	       05  INV-H9      PIC X(15).
001750	       05  INV-H10     PIC X(09).
001760	       05  INV-H11     PIC X(06).
001770	       05  INV-H12     PIC X(14).
001780	   03  INV-L2 REDEFINES W02-PRN-LINE.
001790	       05  INV-ITM     PIC X(14).
001800	       05  FILLER      PIC X(01).
001810	       05  INV-BIN     PIC X(03).
001820	       05  FILLER      PIC X(01).
001830	       05  INV-DESC    PIC X(20).
001840	       05  FILLER      PIC X(01).
001850	       05  INV-QNT     PIC ZZZZ9.99-.
001860	       05  INV-COST    PIC ZZZZ9.999.
001880	       05  INV-AVRG    PIC ZZZZ9.999.
001900	       05  INV-TCOST   PIC Z(06)9.99.
001910	       05  FILLER      PIC X(01).
001920	       05  INV-SELL    PIC ZZZZ9.99.
001930	       05  FILLER      PIC X(01).
001940	       05  INV-TSELL   PIC Z(06)9.99.
001950	       05  FILLER      PIC X(01).
001960	       05  INV-PROF    PIC Z(06)9.99.
001970	       05  FILLER      PIC X(01).
001980	       05  INV-GP      PIC Z(02)9.99.
001990	       05  FILLER      PIC X(01).
002000	       05  INV-XREF    PIC X(14).
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
002010	   03  MOV-LINE1 REDEFINES W02-PRN-LINE.
002020	       05  MOV-H1      PIC  X(15).
002030	       05  MOV-H2      PIC  X(04).
002040	       05  MOV-H3      PIC  X(23).
002050	       05  MOV-MONTH.
002060		   07  MOV-H4  PIC  X(16).
002070		   07  MOV-H5  PIC  X(08).
002080		   07  MOV-H6  PIC  X(16).
002090	       05  MOV-RMNTH REDEFINES MOV-MONTH.
002100		   07  MOV-H7  PIC  X(09).
002110		   07  MOV-H8  PIC  X(11).
002120		   07  MOV-H9  PIC  X(14).
002130		   07  MOV-H10 PIC  X(06).
002140	       05  FILLER      PIC  X(02).
002150	       05  MOV-YEAR.
002160		   07  MOV-H11 PIC  X(20).
002170		   07  MOV-H12 PIC  X(08).
002180		   07  MOV-H13 PIC  X(20).
002190	       05  MOV-RYR REDEFINES MOV-YEAR.
002200		   07  MOV-H14 PIC  X(09).
002210		   07  MOV-H15 PIC  X(11).
002220		   07  MOV-H16 PIC  X(15).
002230		   07  MOV-H17 PIC  X(07).
002240		   07  MOV-H18 PIC  X(06).
002250	   03  MOV-LINE2 REDEFINES W02-PRN-LINE.
002260	       05  MOV-ITEM    PIC  X(14).
002270	       05  FILLER      PIC  X(01).
002280	       05  MOV-BIN     PIC  X(03).
002290	       05  FILLER      PIC  X(01).
002300	       05  MOV-DESC    PIC  X(22).
002310	       05  FILLER      PIC  X(01).
002320	       05  MOV-MQTY    PIC  Z(04)9.99.
002330	       05  FILLER      PIC  X(01).
002340	       05  MOV-MCOST   PIC  Z(06)9.99.
002350	       05  FILLER      PIC  X(01).
002360	       05  MOV-MSELL   PIC  Z(06)9.99.
002370	       05  FILLER      PIC  X(01).
002380	       05  MOV-MPROF   PIC  Z(05)9.99.
002390	       05  FILLER      PIC  X(01).
002400	       05  MOV-YQTY    PIC  Z(05)9.99.
002410	       05  FILLER      PIC  X(01).
002420	       05  MOV-YCOST   PIC  Z(06)9.99.
002430	       05  FILLER      PIC  X(01).
002440	       05  MOV-YSELL   PIC  Z(06)9.99.
002450	       05  FILLER      PIC  X(01).
002460	       05  MOV-YPROF   PIC  Z(06)9.99.
002470	       05  FILLER      PIC  X(01).
002480	       05  MOV-GP      PIC  Z(02)9.99.
002490	   03  ORD-LINE1 REDEFINES W02-PRN-LINE.
002500	       05  ORD-H1      PIC  X(15).
002510	       05  ORD-H2      PIC  X(04).
002520	       05  ORD-H3      PIC  X(23).
002530	       05  ORD-H4      PIC  X(11).
002540	       05  ORD-H5      PIC  X(09).
002550	       05  ORD-MOVE.
002560		   07  ORD-H6  PIC  X(22).
002570		   07  ORD-H7  PIC  X(10).
002580		   07  ORD-H8  PIC  X(22).
002590		   07  FILLER  PIC  X(01).
002600		   07  ORD-H9  PIC  X(15).
002610	       05  ORD-MOV REDEFINES ORD-MOVE.
002620		   07  ORD-H10 PIC  X(09).
002630		   07  ORD-H11 PIC  X(09).
002640		   07  ORD-H12 PIC  X(08).
002650		   07  ORD-H13 PIC  X(11).
002660		   07  ORD-H14 PIC  X(11).
002670		   07  ORD-H15 PIC  X(07).
002680		   07  ORD-H16 PIC  X(11).
002690		   07  ORD-H17 PIC  X(04).
002700	   03  ORD-LINE2 REDEFINES W02-PRN-LINE.
002710	       05  ORD-ITM     PIC  X(14).
002720	       05  FILLER      PIC  X(01).
002730	       05  ORD-BIN     PIC  X(03).
002740	       05  FILLER      PIC  X(01).
002750	       05  ORD-DESC    PIC  X(22).
002760	       05  FILLER      PIC  X(01).
002770	       05  ORD-QUANT   PIC  Z(04)9.99-.
002780	       05  ORD-REORD   PIC  Z(04)9.99.
002790	       05  FILLER      PIC  X(01).
002800	       05  ORD-MTH1    PIC  Z(04)9.99.
002810	       05  FILLER      PIC  X(01).
002820	       05  ORD-MTH2    PIC  Z(04)9.99.
002830	       05  FILLER      PIC  X(01).
002840	       05  ORD-MTH3    PIC  Z(04)9.99.
002850	       05  FILLER      PIC  X(01).
002860	       05  ORD-MAVG    PIC  Z(04)9.99.
002870	       05  FILLER      PIC  X(01).
002880	       05  ORD-MTD     PIC  Z(04)9.99.
002890	       05  FILLER      PIC  X(01).
002900	       05  ORD-YTD     PIC  Z(04)9.99.
002910	       05  FILLER      PIC  X(01).
002920	       05  ORD-ORDER   PIC  Z(04)9.99.
002930	       05  FILLER      PIC  X(01).
002940	       05  ORD-COST    PIC  Z(05)9.99.
	   03  QTY-L1 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(02).
	       05  QTY-H1      PIC  X(20).
	       05  QTY-H2      PIC  X(34).
	       05  QTY-H3      PIC  X(15).
	       05  QTY-H4      PIC  X(09).
	   03  QTY-L2 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(02).
	       05  QTY-EXT-ITEM.
		   07  QTY-ITEM
			       PIC  X(14).
		   07  QTY-SL  PIC  X(01).
		   07  QTY-ITM PIC  X(03).
	       05  FILLER      PIC  X(02).
	       05  QTY-DESC    PIC  X(30).
	       05  FILLER      PIC  X(02).
	       05  QTY-QUANT   PIC  Z(05)9.99-.
	       05  FILLER      PIC  X(03).
	       05  QTY-PRC.
		   07  QTY-PRICE PIC Z(05)9.99.
		   07  FILLER  PIC  X(01).
	       05  QTY-DTE REDEFINES QTY-PRC.
		   07  QTY-DATE
			       PIC  Z9/99/9999.
	       05  FILLER      PIC  X(02).

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
	   03  W25-COST        PIC  9(07)V999	COMP-3.
003790	   03  W25-VALUE       PIC S9(07)V9(04) COMP-3.
003800	   03  W25-TOTAL       PIC S9(07)V9(04) COMP-3.

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

004470 01  S04.
004480	   03  LINE 10 COLUMN 20 FOREGROUND-COLOR 14
004490                           VALUE "Busy loading stock departments".

       01  S20.
	   03  LINE  2 COLUMN 34 FOREGROUND-COLOR 15
				 VALUE "REDUNDANT STOCK".
017270     03  LINE  4 COLUMN 12 FOREGROUND-COLOR 14 VALUE 
				 "Busy printing Redundant Stock".
	   03  LINE  6 COLUMN 12 VALUE "Are you sure (".
	   03	       COLUMN 26 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Y".
	   03	       COLUMN 27 VALUE "/".
	   03	       COLUMN 28 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "N".
	   03	       COLUMN 29 VALUE ")?".

       COPY ERROR.CRT.

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
	     PERFORM LE000.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

       AA49.
             EXIT PROGRAM.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

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
      *    ****    R E D U N D A N T   S T O C K
      *
018610 LE000	       SECTION 5.
018620 LE00.
018630       DISPLAY CLR-SCREEN.
             DISPLAY S20.
016210	     MOVE "Y"		 TO WS-OPTION.

       LE05.
	     ACCEPT WS-OPTION AT 0632
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
016240			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "N" OR "Y")
               GO TO LE05.
           IF WS-OPTION = "N"
               GO TO LE999.
	     MOVE ZERO		 TO WS-PAGE W25-TOTAL W25-VALUE.
	     MOVE "N"		 TO WS-PRT.

       LE10.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO LE999.

       LE12.
009340	     PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       GO TO LE15.
	   IF (DPT-CODE = "XXXX" OR "YYYY") OR
	      (DPT-SCDE = "VAT")
		GO TO LE12.
	     GO TO LE20.

       LE15.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "REDUNDANT STOCK   T O T A L"
				 TO QTY-H2.
	     MOVE W25-TOTAL	 TO QTY-PRICE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "****   E N D   R E P O R T   ****"
                                 TO QTY-L1.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
	     MOVE SPACES	 TO REP-DETAIL1.
             GO TO LE999.

       LE20.           
	   IF WS-PRT = "Y"
	       MOVE "X" 	 TO WS-PRT.
	   IF NOT (WS-PRINTER = "S")
	       DISPLAY WS-BLNK78 AT 0802 WITH FOREGROUND-COLOR 3
	       DISPLAY WS-BLNK78 AT 1002 WITH FOREGROUND-COLOR 3.
	   IF NOT (WS-PRINTER = "S")
	       DISPLAY "Department : " AT 0812
			DPT-DESC WITH FOREGROUND-COLOR 3 HIGHLIGHT
				      BACKGROUND-COLOR 5
	       DISPLAY "Stock Code : " AT 1012.
	     MOVE DPT-CODE	 TO STK-LDG.
	     MOVE "!"		 TO STK-CODE.
009360       PERFORM START-AT-STOCK-LEDG THRU READ-STOCK-EXIT.
009370     IF WS-F-ERROR = 22
	       GO TO LE12.

       LE25.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
           IF (WS-F-ERROR = 22) OR
	      (DPT-CODE NOT = STK-LDG)
009450	       GO TO LE40.
	   IF STK-IND = 4
	       GO TO LE25.
	   IF NOT (WS-PRINTER = "S")
009490	       DISPLAY STK-CODE AT 1025
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
	   IF STK-IND < 8
               GO TO LE25.
	   IF STK-QUANT = ZERO
               GO TO LE25.
	   IF WS-PRT = "X"
               MOVE "N"          TO WS-PRT
	       MOVE "S"		 TO WS-COMMAND
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND.
	   IF WS-PRT = "N"
               PERFORM LE35.
	   IF WS-USE-ITM = "Y"
	       MOVE STK-ITEM	 TO QTY-ITEM
	       MOVE "/" 	 TO QTY-SL
	       MOVE STK-ITM	 TO QTY-ITM
	   ELSE
015280	       MOVE STK-CODE	 TO QTY-EXT-ITEM.
009520	     MOVE STK-DESC	 TO QTY-DESC.
	     MOVE STK-QUANT	 TO QTY-QUANT.
	     COMPUTE W25-COST = STK-QUANT * STK-AVGE.
	     MOVE W25-COST	 TO QTY-PRICE.
	     ADD W25-COST	 TO W25-VALUE.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
018730	       PERFORM LE35.
018740	     MOVE SPACES	 TO REP-DETAIL1.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO LE999.
             GO TO LE25.

018910 LE35.
	     MOVE "H"		 TO W02-PRN-TYPE.
018920	     MOVE SPACES	 TO REP-DETAIL1.
018930	     MOVE "Page:"	 TO REP-PAGE.
018940	     MOVE "Date:"	 TO REP-DATE.
018950	     MOVE W95-COMP	 TO REP-COMPANY.
018960	     ADD 1		 TO WS-PAGE.
018970	     MOVE WS-PAGE	 TO REP-P-NO.
018980	     MOVE W12-TODAY	 TO REP-DTE.
018990	     MOVE "STOCK - REDUNDANT ITEMS REPORT"
				 TO REP-COMPANY.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
019010	     MOVE SPACES	 TO REP-DETAIL1.
019020	     MOVE "Category:"	 TO REP-HEAD2.
019030	     MOVE DPT-CODE	 TO REP-CATNO.
019040	     MOVE DPT-DESC	 TO REP-CAT.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
019060	     MOVE SPACES	 TO REP-DETAIL1.
019070	     MOVE "Item Code"	 TO QTY-H1.
019140	     MOVE WS-DESC-H1	 TO QTY-H2.
019150	     MOVE "Quantity"	 TO QTY-H3.
019160	     MOVE "Value"	 TO QTY-H4.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
019260	     MOVE SPACES	 TO REP-DETAIL1.
             MOVE "Y"            TO WS-PRT.

       LE40.
	   IF WS-PRT = "N" OR "X"
	       GO TO LE12.
018740	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D E P A R T M E N T   T O T A L"
				 TO QTY-H2.
	     ADD W25-VALUE	 TO W25-TOTAL.
	     MOVE W25-VALUE	 TO QTY-PRICE.
	     MOVE ZERO		 TO W25-VALUE.
	     PERFORM CALL-PRINTUTL.
018740	     MOVE SPACES	 TO REP-DETAIL1.
	     GO TO LE12.

019270 LE999.
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
041850	   IF WS-F-ERROR = 15
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
