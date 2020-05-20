      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	  ****	   ******    ******    *
      *   **	**     **     **   **	 **  **   **	**  **	  **   *
      *   **	       **     **   **	**    **  **	    **	       *
      *    ******      **     ******	**    **  ********  ********   *
      * 	**     **     **	**    **  **	**  **	  **   *
      *   **	**     **     **	 **  **   **	**  **	  **   *
      *    ******      **     **	  ****	   ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *    S T O C K   L O C A T I O N	 R E P O R T   P R O G R A M   *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STP066.
000030 AUTHOR.	       J.W.LEMMON SOFTWARE CC.
000040 DATE-WRITTEN.   APRIL 1996.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1996 - 2008
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

       COPY WARHSE.SL.

       COPY WSTOCK.SL.

      /
000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

       COPY DEPART.FDE.

000290 COPY PARAM.FDE.

000350 COPY SPARTS.FDE.

000310 COPY STOCK.FDE.

000280 COPY WARHSE.FDE.

000280 COPY WSTOCK.FDE.

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
003230 77  WS-PARKEY	   PIC	9(04)	 COMP-5.
003250 77  WS-NETKEY	   PIC	9(04)	 COMP-5.
003150 77  WS-PRN-NO	   PIC	X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC	X(01)	 COMP-X.
003270 77  WS-PAGE	   PIC	9(04)	  COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
003650 77  WS-COND	   PIC	X(06).
003660 77  WS-NORM	   PIC	X(06).
003070 77  WS-EXPP	   PIC	X(06).
003080 77  WS-ECAN	   PIC	X(06).
003090 77  WS-8LPI	   PIC	X(06).
003100 77  WS-6LPI	   PIC	X(06).
       77  WS-10CPI	   PIC	X(06).
       77  WS-12CPI	   PIC	X(06).
       77  WS-17CPI	   PIC	X(06).
003300 77  WS-PQNT	   PIC	X(01).
003310 77  WS-WS	   PIC	X(01) VALUE "N".
003310 77  WS-MEMO	   PIC	X(01) VALUE "N".
003310 77  WS-PRT	   PIC	X(01) VALUE "Y".
003320 77  WS-BIN	   PIC	X(01).
       77  WS-ETYPE	   PIC	X(01).
003330 77  WS-OPTION	   PIC	X(01).
003330 77  WS-ZLT	   PIC	X(01) VALUE "Y".
003330 77  WS-ZQT	   PIC	X(01) VALUE "Y".
       77  WS-NUM	   PIC	ZZ9.
003350 77  WS-ERROR	   PIC	9(01).
003380 77  WS-KEY1	   PIC	9(04)	 COMP-5.
003390 77  WS-KSTORE	   PIC	9(04)	 COMP-5.
003420 77  WS-INVF	   PIC	X(01)	 COMP-X.
003470 77  WS-ORD	   PIC S9(05)	 COMP-3.
003480 77  WS-TOTAL	   PIC S9(09)V99 COMP-3.
003490 77  WS-VALUE	   PIC S9(09)V99 COMP-3.
003500 77  WS-CCOST	   PIC S9(09)V999 COMP-3.
003510 77  WS-CSELL	   PIC S9(09)V99 COMP-3.
003520 77  WS-SCOST	   PIC S9(09)V99 COMP-3.
003530 77  WS-SSELL	   PIC S9(09)V99 COMP-3.
003540 77  WS-TCOST	   PIC S9(09)V999 COMP-3.
003550 77  WS-TSELL	   PIC S9(09)V99 COMP-3.
003560 77  WS-DEPT	   PIC	X(04).
003190 77  WS-CAT	   PIC	X(04).
003200 77  WS-SCAT	   PIC	X(04).
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

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "STP".
	   03  WS-PROG	       PIC  X(03) VALUE "066".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003610 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

000360 COPY SPARTS.ID.

000300 COPY STOCK.ID.

000360 COPY WARHSE.ID.

000360 COPY WSTOCK.ID.

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
000500	       05  REP-DTE     PIC  Z9/99/99.
000460	       05  FILLER      PIC  X(05).
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
001650	   03  INV-L1 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(04).
001660	       05  INV-H1      PIC  X(17).
001670	       05  INV-H2      PIC  X(33).
001680	       05  INV-H3      PIC  X(13).
001690	       05  INV-H4      PIC  X(12).
001700	       05  INV-H5      PIC  X(13).
001710	       05  INV-H6      PIC  X(14).
001710	       05  INV-H7      PIC  X(13).
001710	       05  INV-H8      PIC  X(10).
	       05  FILLER      PIC  X(03).
001780	   03  INV-L2 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(04).
001790	       05  INV-ITM     PIC  X(14).
001820	       05  FILLER      PIC  X(03).
001830	       05  INV-DESC    PIC  X(30).
001840	       05  FILLER      PIC  X(03).
	       05  INV-REF     PIC  X(10).
	       05  FILLER      PIC  X(04).
	       05  INV-PER     PIC  X(08).
	       05  FILLER      PIC  X(02).
001860	       05  INV-COST    PIC  Z(07)9.999.
001910	       05  FILLER      PIC  X(02).
001920	       05  INV-SELL    PIC  Z(07)9.99.
001930	       05  FILLER      PIC  X(02).
001940	       05  INV-CASH    PIC  Z(07)9.99.
001950	       05  FILLER      PIC  X(02).
001960	       05  INV-WSALE   PIC  Z(07)9.99.
	       05  FILLER      PIC  X(03).
000590	   03  INV-L3 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(04).
000600	       05  INV-HEAD2   PIC  X(10).
000610	       05  INV-CNO     PIC  9(03).
000620	       05  INV-CATNO REDEFINES INV-CNO PIC 9(03).
000630	       05  FILLER      PIC  X(01).
000640	       05  INV-CAT     PIC  X(30).
000650	       05  FILLER      PIC  X(84).
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
	       05  FILLER      PIC  X(03).
	       05  QTY-H1      PIC  X(18).
	       05  QTY-H2      PIC  X(35).
	       05  QTY-H3      PIC  X(14).
	       05  QTY-H4      PIC  X(10).
	   03  QTY-L2 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(03).
	       05  QTY-ITM     PIC  X(14).
	       05  FILLER      PIC  X(04).
	       05  QTY-DESC    PIC  X(30).
	       05  FILLER      PIC  X(04).
	       05  QTY-QUANT   PIC  Z(05)9.99-.
	       05  FILLER      PIC  X(03).
	       05  QTY-PRC.
		   07  QTY-PRICE PIC Z(05)9.99.
	       05  QTY-DTE REDEFINES QTY-PRC.
		   07  QTY-DATE PIC Z9/99/99.
		   07  FILLER  PIC  X(01).
	       05  FILLER      PIC  X(03).

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
		   07  DEP-H10A
			       PIC  X(06).
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

	   03  DEP-L3 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(19).
	       05  DEP-T-HEAD  PIC  X(30).
	       05  FILLER      PIC  X(11).
	       05  DEP-TCOST   PIC  Z(08)9.999.
	       05  FILLER      PIC  X(10).
	       05  DEP-TSELL   PIC  Z(08)9.99.
	       05  FILLER      PIC  X(37).

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
           03  W40-DATE1          PIC 9(06).
           03  W40-DTE1 REDEFINES W40-DATE1.
               05  W40-Y1         PIC 9(02).
               05  W40-M1         PIC 9(02).
               05  W40-D1         PIC 9(02).
           03  W40-DATE2          PIC 9(06).
           03  W40-DTE2 REDEFINES W40-DATE2.
               05  W40-Y2         PIC 9(02).
               05  W40-M2         PIC 9(02).
               05  W40-D2         PIC 9(02).

       COPY W40.WS.

002920 01  W40-WAREHOUSE.
002930     03  W40-NAME1.
002940	       05  W40-NAME    PIC  X(40).
003020	   03  W40-NAME2 REDEFINES W40-NAME1.
003030	       05  W40-NCHAR OCCURS 40 PIC X(01).
           03  W40-CONTACT     PIC  X(30).
	   03  W40-CODE	       PIC  X(06).
003070	   03  W40-ADD.
003080	       05  W40-ADDRESS1.
003090		   07  W40-ADD1	PIC X(30).
003100		   07  W40-ADD2	PIC X(30).
003110		   07  W40-ADD3	PIC X(30).
003120		   07  W40-ADD4	PIC X(30).
003130	       05  W40-ADDRESS2 REDEFINES W40-ADDRESS1.
003140		   07  W40-ACHAR OCCURS 120 PIC X(01).
003230     03  W40-POST        PIC 9(04).
003250     03  W40-TEL         PIC X(14).
003260     03  W40-FAX         PIC X(14).

       COPY FUNCTION.WS.

003950 COPY W50.WS.

004040 COPY W70STOCK.WS.

004060 01  W70-KEYS.
004070     03  W70-BIND        PIC 9(05)     COMP-0 VALUE ZERO.

       01  W75-KEYS.
	   03  W75-S	      PIC  9(02) COMP-5.
	   03  W75-S1	      PIC  9(02) COMP-5.
	   03  W75-S2	      PIC  9(02) COMP-5.
	   03  W75-WHOUSE OCCURS 18.
	       05  W75-WHCDE  PIC  X(06).

004080 01  W95-COMP	       PIC X(40).
005120 01  W100-EDIT.
003970     03  W100-V2.
003980         05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.
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
004090 SCREEN SECTION.

       COPY BLANK.CRT.

004440 01  S03.
004450	   03  LINE 18 COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
004460                           PIC X(14) USING STK-CODE.
004470 01  S04.
004480	   03  LINE 10 COLUMN 20 FOREGROUND-COLOR 14
004490                           VALUE "Busy loading stock departments".
004570 01  S07.
004580	   03  LINE  8 COLUMN 20 FOREGROUND-COLOR 14 VALUE
004590                           "Busy printing departmental report".

004600 01  S08.
004610	   03  LINE 10 COLUMN 20 VALUE "Select department".

005040 01  S15.
005050	   03  LINE 18 COLUMN 12 VALUE "Stock record:".

       COPY WARLUP.CRT.

       COPY S99.CRT.

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

       AA50-LOOKUP     SECTION.
       AA50.
	     PERFORM SAVE-SCREEN.
011080	     MOVE 4		 TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
	     PERFORM WARHSE-LOOKUP.
	     PERFORM RESTORE-SCREEN.
       AA99.
             EXIT.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

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
005450	     MOVE "STOCK LOCATION REPORT"
                                 TO REP-REPORT.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
005470	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "Location:"	 TO REP-HEAD2.
	     MOVE WAR-NAME	 TO REP-CAT.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
005520	     MOVE SPACES	 TO REP-DETAIL1.
005480	     MOVE "Category:"	 TO REP-HEAD2.
005490       MOVE DPT-SALES-NO   TO REP-CATNO.
005500       MOVE DPT-DESC       TO REP-CAT.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
005520	     MOVE SPACES	 TO REP-DETAIL1.
005530	     MOVE "Item Code"	 TO DEP-H1.
005540	     MOVE "Bin" 	 TO DEP-H2.
005570	     MOVE "Description"  TO DEP-H3.
005580	     MOVE "Quantity"	 TO DEP-H4.
005590	     MOVE "Cost-Price"	 TO DEP-H5.
005600	     MOVE "Average"	 TO DEP-H6.
005610	     MOVE "Retail"	 TO DEP-H7.
             MOVE ALL "_"        TO DEP-H8 DEP-H10.
005620	     MOVE "Markup"	 TO DEP-H9.
005660	     MOVE "Active"	 TO DEP-H11.
005650       MOVE "GP-%"         TO DEP-H12.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
005690	     MOVE SPACES	 TO REP-DETAIL1.
005640	     MOVE WS-WS-HD	 TO DEP-H8.
005610	     MOVE WS-CS-HD	 TO DEP-H9.
005620	     MOVE "Retail"	 TO DEP-H10A.
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

       COPY WARHSE.RD.

       COPY WSTOCK.RD.

       COPY STOCK.1ST.

       COPY WARHSE.LUP.

       COPY APAC.HLP.

      *
      *    ****   G E T   W A R E H O U S E   R E C O R D
      *
015170 CA000         SECTION.
015180 CA00.
013310	     MOVE 6		 TO W50-LENGTH.

       CA05.
013320	     MOVE W40-CODE	 TO W50-DATA.
           IF W50-1 = SPACE
	       MOVE W50-5	 TO W40-CODE
               GO TO CA05.      
	     CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
013380	     MOVE W50-DENT	 TO W40-CODE.

       CA06.
013380	     MOVE W50-DENT	 TO WAR-CODE.

       CA10.
	     PERFORM READ-WARHSE THRU READ-WARHSE-EXIT.

       CA99.
             EXIT.

      /
008500 BE000		SECTION.
008510 BE00.
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
	     DISPLAY "LOCATION STOCK REPORT" AT 0230
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     MOVE SPACES	 TO W40-CODE.
	     DISPLAY "Warehouse Code:" AT 0404.

       BE02.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Warehouse Lookup,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     ACCEPT W40-CODE AT 0420
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO BE999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
016560		     PERFORM AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BE02
	       END-EVALUATE
	       DISPLAY W40-CODE AT 0420
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 5
	       GO TO BE02.
019250	     DISPLAY CLEAR-L25.
015830	   IF W40-CODE = SPACE
015840	       GO TO BE999.

       BE03.
	     PERFORM CA000.
015860	   IF WS-F-ERROR = 51
017520	       MOVE "Z" 	 TO WS-ETYPE
018620	       PERFORM AA50
	       DISPLAY W40-CODE AT 0420
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 5
018640	       IF W40-CODE = SPACES
018720		   GO TO BE02
               ELSE
		   GO TO BE03.
	     DISPLAY WAR-NAME AT 0427
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 3 HIGHLIGHT.
	     PERFORM CHECK-CORRECT.
015980	   IF WS-OPTION = "N"
	       GO TO BE02.
	     DISPLAY "Print stock with Lead Time = 0.00" AT 0604.
       BE05.
	     ACCEPT WS-ZLT AT 0639
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-ZLT
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-ZLT = "Y" OR "N")
	       GO TO BE05.
	     DISPLAY "Print stock with zero quantity" AT 0804.
       BE10.
	     ACCEPT WS-ZQT AT 0835
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-ZQT
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-ZQT = "Y" OR "N")
	       GO TO BE10.
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
	     MOVE "!"		 TO DPT-CODE.
	     PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.
             PERFORM AB25.

009870 BE90.
	     PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       GO TO BE92.
	   IF NOT (DPT-CODE = "XXXX" OR "YYYY")
	       IF NOT (DPT-SCDE = "VAT")
		   GO TO BE95.
	     GO TO BE90.

009900 BE92.
009940	     MOVE "STOCK TOTAL"  TO DEP-T-HEAD.
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
	       DISPLAY DPT-DESC AT 1412
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
	     MOVE WS-CAT	 TO STK-LDG WS-SCAT.
	     MOVE "!"		 TO STK-CODE.
009360       PERFORM START-AT-STOCK-LEDG THRU READ-STOCK-EXIT.
009370     IF WS-F-ERROR = 22
	       MOVE "X" 	 TO WS-OPTION
010130         GO TO BE106.

010140 BE100.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
	   IF (WS-F-ERROR = 22) OR (STK-LDG NOT = WS-SCAT)
010160         GO TO BE105.
	   IF WS-ZLT = "N"
	       IF STK-LTIME = ZERO
		   GO TO BE100.
	     MOVE W40-CODE	 TO WST-WAR.
	     MOVE STK-CODE	 TO WST-EXT-CODE.
	     PERFORM READ-WSTOCK THRU READ-WSTOCK-EXIT.
	   IF WS-F-ERROR = 52
	       GO TO BE100.
	   IF WS-ZQT = "N"
	       IF WST-QUANT = ZERO
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
	       MOVE ZERO	 TO WST-QUANT.
	   IF STK-IND = 1
	       IF NOT (WS-INVF = 1)
		   MOVE ZERO	 TO WST-QUANT.
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
010210	     MOVE STK-CODE	 TO DEP-ITM.
010250       MOVE STK-DESC       TO DEP-DESC.
	   IF STK-IND = 4
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       GO TO BE101.
010220       MOVE STK-BIN        TO DEP-BIN.
010260	     MOVE WST-QUANT	 TO DEP-QNT.
010270       MOVE STK-COST       TO DEP-COST.
010280       MOVE STK-SELL       TO DEP-RET.
      *    ****    A V E R A G E   C O S T
	   IF WST-QUANT < 0.001
	       MOVE STK-COST	 TO STK-AVGE.
	   IF STK-IND < 2
016950	       IF STK-AVGE = ZERO
016960		   MOVE STK-COST TO STK-AVGE.
010290	     MOVE STK-AVGE	 TO DEP-AVG.
010310       COMPUTE DEP-MWS ROUNDED = ((STK-WSALE - STK-COST)
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
010400       COMPUTE DEP-GP ROUNDED = ((WS-SSELL - WS-SCOST)
010410                                * 100.000) / WS-SSELL. 
010420	     MOVE STK-ACT	 TO DEP-ACT.
010440	     MULTIPLY STK-AVGE BY WST-QUANT
				 GIVING WS-VALUE.
010450	     MULTIPLY STK-SELL BY WST-QUANT
				 GIVING WS-TOTAL.
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
014990	       GO TO BE106.

010540 BE102.
010560       GO TO BE100.

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
010630	     MOVE "DEPARTMENT TOTAL COST/SELL"
				 TO DEP-T-HEAD.
010640	     MOVE WS-CCOST	 TO DEP-TCOST.
010650	     MOVE WS-CSELL	 TO DEP-TSELL.
010660       ADD WS-CCOST        TO WS-TCOST.
010670       ADD WS-CSELL        TO WS-TSELL.
010680       MOVE ZERO           TO WS-CCOST WS-CSELL.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
010700	     MOVE SPACES	 TO REP-DETAIL1.

       BE106.
             EXIT.

010710 BE108.
	   IF WS-OPTION = "X"
	       GO TO BE999.
010720	     GO TO BE90.

010730 BE110.
010740       DISPLAY CLR-SCREEN.
             DISPLAY "SELECTIVE DEPARTMENTAL REPORT" AT 0226
                     WITH FOREGROUND-COLOR 15.
010750       DISPLAY S08.
010760       DISPLAY S15.
010770	     ACCEPT WS-DEPT AT 1038
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
010780	   IF WS-DEPT = SPACES
010790	       GO TO BE999.
010800	     MOVE WS-DEPT	 TO DPT-CODE WS-CAT.
	     DISPLAY CLEAR-L25.
010810	   IF (DPT-CODE = "XXXX" OR "YYYY") OR
	      (DPT-SCDE = "VAT")
	       MOVE "Invalid code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
010840         GO TO BE110.
010090       PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       MOVE "Invalid department code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
010840         GO TO BE110.
	     DISPLAY DPT-DESC AT 1412
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
010910       GO TO BE110.

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
	     MOVE "WARHSE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-WARHSE.
	     MOVE "WSTOCK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-WSTOCK.
019480	     MOVE 3		 TO WS-PARKEY.
044240       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-INV	 TO WS-INVF.

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
	   IF NOT (PAR-WS-HD = X"00000000000000000000")
	       MOVE PAR-WS-HD	 TO WS-WS-HD.
	   IF NOT (PAR-CS-HD = X"00000000000000000000")
	       MOVE PAR-CS-HD	 TO WS-CS-HD.
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
019900	     GO TO ZA999-EXIT.

055270 ZA49.
055280       DISPLAY "Too many files OPEN" AT 0812
055290                WITH FOREGROUND-COLOR 14.
055300       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
055310       DISPLAY "Press any key to continue" AT 1212.
055320       ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
055330       GO TO ZA205.
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
	       MOVE STK-CODE	 TO WS-KEYX
041840	   ELSE
041770	   IF WS-F-ERROR = 51
041780	       MOVE W02-WARHSE TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
	       MOVE WAR-CODE	 TO WS-KEYX
041840	   ELSE
041770	   IF WS-F-ERROR = 52
041780	       MOVE W02-WSTOCK TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
	       MOVE WST-KEY	 TO WS-KEYX.
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
