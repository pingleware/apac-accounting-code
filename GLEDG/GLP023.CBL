      $set LINKCOUNT"320"
      ******************************************************************
      *                                                                *
      *    ******   **	      *******	  ****	   ******    ******    *
      *   **	**  **	      **    **	 **  **   **	**  **	  **   *
      *   **	    **	      **    **	**    **       **	  **   *
      *   **	    **	      *******	**    **     **        ****    *
      *   **   ***  **	      **	**    **   **		  **   *
      *   **	**  **	      **	 **  **   **	    **	  **   *
      *    ******   ********  **	  ****	  ********   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     G / L E D G E R   O P E R A T I N G   S T A T E M E N T    *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	       GLP023.
000030 AUTHOR.                 J.W. LEMMON (APAC).
000040 DATE-WRITTEN.           JUNE 1983.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2008
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

000070 ENVIRONMENT DIVISION.
000080 CONFIGURATION SECTION.
000090 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000110 INPUT-OUTPUT SECTION.
000120 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CONTROL.SL.

000140 COPY PARAM.SL.

000160 COPY GACCNT.SL.

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000280 COPY PARAM.FDE.

000300 COPY GACCNT.FDE.

      /
004330 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
004340 77  WS-PARKEY	   PIC 9(04)	 COMP-5 VALUE 1.
004350 77  WS-GLKEY        PIC 9(04)     COMP-5 VALUE 1.
004360 77  WS-GTRKEY       PIC 9(04)     COMP-5 VALUE 1.
004370 77  WS-GINKEY       PIC 9(04)     COMP-5 VALUE 1.
004370 77  WS-NETKEY       PIC 9(04)     COMP-5 VALUE 1.
004380 77  WS-KSTORE       PIC 9(04)     COMP-5.
004390 77  WS-ISUB         PIC 9(04)     COMP-5.
004400 77  WS-CHEQUE       PIC 9(04)     COMP-5.
004410 77  WS-PAGE         PIC 9(04)     COMP-5.
004420 77  WS-LINES        PIC 9(02) VALUE 66.
004430 77  WS-S1           PIC 9(04)     COMP-5.
004440 77  WS-S2           PIC 9(04)     COMP-5.
004450 77  WS-S3           PIC 9(04)     COMP-5.
004460 77  WS-S4           PIC 9(04)     COMP-5.
004470 77  WS-S5           PIC 9(04)     COMP-5.
004480 77  WS-S6           PIC 9(04)     COMP-5.
004490 77  WS-S7           PIC 9(04)     COMP-5.
004500 77  WS-S8           PIC 9(04)     COMP-5.
004510 77  WS-S9           PIC 9(04)     COMP-5.
004520 77  WS-S10          PIC 9(04)     COMP-5.
004530 77  WS-S11          PIC 9(04)     COMP-5.
004540 77  WS-OPEN         PIC  9(02) VALUE ZERO.
004550 77  WS-EXS          PIC 9(01).
004560 77  WS-NUM          PIC  Z9.
004560 77  WS-SCH          PIC  X(01).
004560 77  WS-RECON        PIC  X(01).
004570 77  WS-ZERO         PIC  9(01).
004580 77  WS-MM           PIC  9(02) COMP-5 VALUE ZERO.
       77  WS-MTH          PIC  9(02) COMP-5.
004590 77  WS-F-YR         PIC 9(02).
004600 77  WS-E-YR         PIC 9(02).
004610 77  WS-OPTION       PIC X(01).
004630 77  WS-ERROR        PIC 9(01) VALUE ZERO.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
004710 77  WS-TNEXT	   PIC 9(04)	 COMP-5.
003150 77  WS-PRN-NO	   PIC X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC X(01)	 COMP-X.
004730 77  WS-H1	   PIC X(05) VALUE "DATE:".
004740 77  WS-H3           PIC X(06) VALUE "BATCH:".
004750 77  WS-H4           PIC X(05) VALUE "PAGE:".
004780 77  WS-ER1	   PIC X(07) VALUE "Account".
004790 77  WS-ER3          PIC X(04) VALUE "Date".
004800 77  WS-ER4          PIC X(05) VALUE "Value".
004810 77  WS-ER5          PIC X(13) VALUE "Cheque number".
004820 77  WS-ER6          PIC X(09) VALUE "No Record".
004830 77  WS-ER7          PIC X(13) VALUE "Group Heading".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
004840 01  WS-DB-LINE.
004850     03  WS-TOP-LNE.
004860         05  WS-TCHR PIC X(01) OCCURS 80.
004870     03  WS-T-LINE REDEFINES WS-TOP-LNE.
004880         05  FILLER  PIC X(01).
004890         05  WS-H-LINE
004900                     PIC X(78).
004910         05  FILLER  PIC X(01).
004920     03  WS-TOP-LNE2.
004930         05  WS-TCH  PIC X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
004940	   03  WS-MID-LNE.
004950         05  WS-MCHR PIC X(01) OCCURS 80.
	   03  WS-MDGL-LNE REDEFINES WS-MID-LNE.
	       05  WS-MGL1     PIC X(01).
	       05  WS-MGL78    PIC X(78).
	       05  WS-MGL80    PIC X(01).
004960     03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
004980	   03  WS-BOT-LNE.
004990         05  WS-BCHR PIC X(01) OCCURS 80.
005000     03  WS-B-LINE REDEFINES WS-BOT-LNE.
005010         05  FILLER  PIC X(01).
005020         05  WS-F-LINE
005030                     PIC X(78).
005040         05  FILLER  PIC X(01).
005050     03  WS-BOT-LNE2.
005060         05  WS-BCH  PIC X(01) OCCURS 80.

005080 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

005210 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

000300 COPY GACCNT.ID.

       COPY PARAM.ID.

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

000380	   03  B-L1 REDEFINES W02-PRN-LINE.
000390	       05  B-H1        PIC  X(06).
000400	       05  B-DTE       PIC  Z9/99/9999.
000410	       05  FILLER      PIC  X(05).
000420	       05  B-COMP      PIC  X(40).
000430	       05  FILLER      PIC  X(10).
000440	       05  B-H2        PIC  X(06).
000450	       05  B-PGE       PIC  ZZ9.

000460	   03  B-L2 REDEFINES W02-PRN-LINE.
000470	       05  FILLER      PIC  X(04).
000480	       05  B-NO1       PIC  9(04).
000490	       05  B-NO2       PIC  9(04).
000500	       05  FILLER      PIC  X(06).
000510	       05  B-NAME.
000520		   07  FILLER  PIC  X(06).
000530		   07  B-NME2.
000540		       09  FILLER
			       PIC  X(10).
000550		       09  B-H3
			       PIC  X(24).
000560	       05  B-SDE1      PIC  X(01).
000570	       05  B-LINE.
000580		   07  FILLER  PIC  X(01).
000590		   07  B-VAL   PIC  Z(07)9.99-.
000600		   07  FILLER  PIC  X(01).
000610	       05  B-SDE2      PIC  X(01).
000620	       05  FILLER      PIC  X(16).
000630	       05  B-SDE3      PIC  X(01).
000640	       05  B-LINE2.
000650		   07  FILLER  PIC  X(01).
000660		   07  B-VAL2  PIC  Z(07)9.99-.
000670		   07  FILLER  PIC  X(01).
000680	       05  B-SDE4      PIC  X(01).

000690	   03  R-L1 REDEFINES W02-PRN-LINE.
000700	       05  R-DET       PIC  X(136).

000710	   03  R-L2 REDEFINES W02-PRN-LINE.
000720	       05  R-H1        PIC  X(06).
000730	       05  R-DTE1      PIC  Z9/99/9999.
000740	       05  FILLER      PIC  X(30).
000750	       05  R-H2        PIC  X(40).
	       05  FILLER      PIC  X(01).
000760	       05  R-DESC      PIC  X(38).
000770	       05  R-H4        PIC  X(06).
000780	       05  R-PGE       PIC  ZZ9.

000790	   03  R-L3 REDEFINES W02-PRN-LINE.
000800	       05  FILLER      PIC  X(10).
000810	       05  R-AC1       PIC  9999/99.
000820	       05  FILLER      PIC  X(03).
000830	       05  R-NME1      PIC  X(30).
000840	       05  FILLER      PIC  X(53).
000850	       05  R-DT1       PIC  Z(07)9.99 BLANK WHEN ZERO.
000860	       05  FILLER      PIC  X(01).
000870	       05  R-CR1       PIC  Z(07)9.99 BLANK WHEN ZERO.
000880	       05  FILLER      PIC  X(06).

000890	   03  R-L4 REDEFINES W02-PRN-LINE.
000900	       05  FILLER      PIC  X(14).
000910	       05  R-AC2       PIC  9999/99.
000920	       05  FILLER      PIC  X(03).
000930	       05  R-NME2      PIC  X(30).
000940	       05  FILLER      PIC  X(27).
000950	       05  R-DT2       PIC  Z(07)9.99 BLANK WHEN ZERO.
000960	       05  FILLER      PIC  X(01).
000970	       05  R-CR2       PIC  Z(07)9.99 BLANK WHEN ZERO.
000980	       05  FILLER      PIC  X(28).

000990	   03  R-L5 REDEFINES W02-PRN-LINE.
001000	       05  FILLER      PIC  X(18).
001010	       05  R-AC3       PIC  9999/99.
001020	       05  FILLER      PIC  X(03).
001030	       05  R-NME3      PIC  X(30).
001040	       05  FILLER      PIC  X(01).
001050	       05  R-DT3       PIC  Z(07)9.99.
001060	       05  FILLER      PIC  X(01).
001070	       05  R-CR3       PIC  Z(07)9.99.
001080	       05  FILLER      PIC  X(50).

001090	   03  R-L6 REDEFINES W02-PRN-LINE.
001100	       05  FILLER      PIC  X(10).
001110	       05  R-BH1       PIC  X(08).
001120	       05  FILLER      PIC  X(01).
001130	       05  R-BH2       PIC  X(31).
001140	       05  R-BH3       PIC  X(08).
001150	       05  R-BHEAD.
001160		   07  FILLER  PIC  X(05).
001170		   07  R-BH4   PIC  X(10).
001180		   07  R-BH5   PIC  X(12).
001190	       05  R-BH6       PIC  X(10).
001200	       05  R-BH7       PIC  X(12).
001210	       05  R-BH8       PIC  X(10).
001220	       05  R-BH9       PIC  X(15).

001230	   03  SCH-L1 REDEFINES W02-PRN-LINE.
001240	       05  S-H1        PIC  X(14).
001250	       05  S-SUBGRP    PIC  X(30).
001260	       05  FILLER      PIC  X(07).
001270	       05  S-TL.
001280		   07  FILLER  PIC  X(04).
001290		   07  S-H2    PIC  X(15).
001300		   07  S-H3    PIC  X(08).
001310	       05  FILLER      PIC  X(02).

001320	   03  SCH-L2 REDEFINES W02-PRN-LINE.
001330	       05  S-H4.
001340		   07  S-ACNO  PIC  9(04)/99.
001350		   07  FILLER  PIC  X(03).
001360	       05  S-ANAME     PIC  X(30).
001370	       05  FILLER      PIC  X(11).
001380	       05  S-DT        PIC  Z(07)9.99 BLANK WHEN ZERO.
001390	       05  FILLER      PIC  X(05).
001400	       05  S-CR        PIC  Z(07)9.99 BLANK WHEN ZERO.
001410	       05  FILLER      PIC  X(02).

001420	   03  TRL-L1 REDEFINES W02-PRN-LINE.
001430	       05  FILLER      PIC  X(19).
001440	       05  TRL-H4      PIC  X(07).
001450	       05  FILLER      PIC  X(12).
001460	       05  TRL-H5      PIC  X(12).
001470	       05  FILLER      PIC  X(33).
001480	       05  TRL-H7      PIC  X(05).
001490	       05  FILLER      PIC  X(16).
001500	       05  TRL-H8      PIC  X(06).
001510	       05  FILLER      PIC  X(22).

001520	   03  TRL-L2 REDEFINES W02-PRN-LINE.
001530	       05  FILLER      PIC  X(19).
001540	       05  TRL-ACC     PIC  9(04)/99.
001550	       05  FILLER      PIC  X(12).
001560	       05  TRL-ANME    PIC  X(30).
001570	       05  FILLER      PIC  X(11).
001580	       05  TRL-DBT     PIC  Z(07)9.99.
001590	       05  FILLER      PIC  X(11).
001600	       05  TRL-CRD     PIC  Z(07)9.99-.
001610	       05  FILLER      PIC  X(19).

001620	   03  STM-L1 REDEFINES W02-PRN-LINE.
001630	       05  FILLER      PIC  X(40).
001640	       05  STM-H1      PIC  X(51).
001650	       05  FILLER      PIC  X(31).
001660	       05  STM-H2      PIC  X(06).
001670	       05  STM-PGE     PIC  ZZZ9.

001680	   03  STM-L2 REDEFINES W02-PRN-LINE.
001690	       05  FILLER      PIC  X(39).
001700	       05  STM-H3.
		 07  FILLER		       PIC  X(47).
		 07  STM-MTH5.
		   09  FILLER		       PIC  X(01).
		   09  STM-MTH6-7.
		     11  FILLER		       PIC  X(01).
		     11  STM-MTH3-4.
		       13  FILLER	       PIC  X(01).
		       13  STM-MTH8.
			 15  FILLER	       PIC  X(01).
			 15  STM-MTH1-10.
			   17  FILLER	       PIC  X(01).
			   17  STM-MTH2-11-12.
			     19  FILLER	       PIC  X(01).
001710	       05  FILLER      PIC  X(40).

001720	   03  STM-L3 REDEFINES W02-PRN-LINE.
001730	       05  FILLER      PIC  X(21).
001740	       05  STM-H4      PIC  X(12).
001750	       05  FILLER      PIC  X(73).
001760	       05  STM-H5      PIC  X(05).
001770	       05  FILLER      PIC  X(21).

001780	   03  STM-L4 REDEFINES W02-PRN-LINE.
001790	       05  STM-H6      PIC  X(47).
001800	       05  FILLER      PIC  X(01).
001810	       05  STM-H7      PIC  X(03).
001820	       05  FILLER      PIC  X(34).
001830	       05  STM-H8      PIC  X(47).

001840	   03  STM-L5 REDEFINES W02-PRN-LINE.
001850	       05  STM-H9      PIC  X(08).
001860	       05  FILLER      PIC  X(02).
001870	       05  STM-H10     PIC  X(03).
001880	       05  FILLER      PIC  X(01).
001890	       05  STM-H11     PIC  X(12).
001900	       05  FILLER      PIC  X(02).
001910	       05  STM-H12     PIC  X(03).
001920	       05  FILLER      PIC  X(01).
001930	       05  STM-H13     PIC  X(11).
001940	       05  FILLER      PIC  X(02).
001950	       05  STM-H14     PIC  X(03).
001960	       05  FILLER      PIC  X(01).
001970	       05  STM-H15     PIC  X(03).
001980	       05  FILLER      PIC  X(10).
001990	       05  STM-H16     PIC  X(11).
002000	       05  FILLER      PIC  X(13).
002010	       05  STM-H17     PIC  X(08).
002020	       05  FILLER      PIC  X(02).
002030	       05  STM-H18     PIC  X(03).
002040	       05  FILLER      PIC  X(01).
002050	       05  STM-H19     PIC  X(12).
002060	       05  FILLER      PIC  X(02).
002070	       05  STM-H20     PIC  X(03).
002080	       05  FILLER      PIC  X(01).
002090	       05  STM-H21     PIC  X(11).
002100	       05  FILLER      PIC  X(02).
002110	       05  STM-H22     PIC  X(03).

002120	   03  STM-L6 REDEFINES W02-PRN-LINE.
002130	       05  STM-V1      PIC  Z(08)-.
002140	       05  FILLER      PIC  X(01).
002150	       05  STM-P1      PIC  ZZZ	      BLANK WHEN ZERO.
002160	       05  FILLER      PIC  X(01).
002170	       05  STM-V2      PIC  Z(08)9.99- BLANK WHEN ZERO.
002180	       05  FILLER      PIC  X(01).
002190	       05  STM-P2      PIC  ZZZ	      BLANK WHEN ZERO.
002200	       05  FILLER      PIC  X(01).
002210	       05  STM-V3      PIC  Z(07)9.99- BLANK WHEN ZERO.
002220	       05  FILLER      PIC  X(01).
002230	       05  STM-P3      PIC  ZZZ	      BLANK WHEN ZERO.
002240	       05  FILLER      PIC  X(01).
002250	       05  STM-GP      PIC  ZZZ	      BLANK WHEN ZERO.
002260	       05  FILLER      PIC  X(01).
002270	       05  STM-DESC.
002280		   07  STM-AC  PIC  9(06).
002290		   07  FILLER  PIC  X(01).
002300		   07  STM-ANAME
			       PIC  X(25).
002310	       05  FILLER      PIC  X(01).
002320	       05  STM-V4      PIC  Z(08)-.
002330	       05  FILLER      PIC  X(01).
002340	       05  STM-P4      PIC  ZZZ	      BLANK WHEN ZERO.
002350	       05  FILLER      PIC  X(01).
002360	       05  STM-V5      PIC  Z(08)9.99- BLANK WHEN ZERO.
002370	       05  FILLER      PIC  X(01).
002380	       05  STM-P5      PIC  ZZZ	      BLANK WHEN ZERO.
002390	       05  FILLER      PIC  X(01).
002400	       05  STM-V6      PIC  Z(07)9.99- BLANK WHEN ZERO.
002410	       05  FILLER      PIC  X(01).
002420	       05  STM-P6      PIC  ZZZ	      BLANK WHEN ZERO.

002430	   03  LED-L1 REDEFINES W02-PRN-LINE.
002440	       05  LED-H1      PIC  X(07).
002450	       05  FILLER      PIC  X(02).
002460	       05  LED-H2      PIC  X(12).
002470	       05  FILLER      PIC  X(06).
	       05  LED-H2A     PIC  X(10).
	       05  FILLER      PIC  X(02).
002480	       05  LED-H3      PIC  X(04).
002490	       05  FILLER      PIC  X(04).
002500	       05  LED-H4      PIC  X(04).
	       05  FILLER      PIC  X(03).
002500	       05  LED-H4A     PIC  X(05).
002510	       05  FILLER      PIC  X(04).
002520	       05  LED-H5      PIC  X(05).
002530	       05  FILLER      PIC  X(02).
002540	       05  LED-H6      PIC  X(15).
002550	       05  FILLER      PIC  X(28).
002560	       05  LED-H7      PIC  X(05).
002570	       05  FILLER      PIC  X(06).
002580	       05  LED-H8      PIC  X(07).
002590	       05  FILLER      PIC  X(01).

002600	   03  LED-L2 REDEFINES W02-PRN-LINE.
002610	       05  LED-AC      PIC  9(04)/99.
002620	       05  FILLER      PIC  X(02).
002630	       05  LED-NME     PIC  X(24).
002640	       05  FILLER      PIC  X(04).
002650	       05  LED-TRAN.
002660		   07  LED-DTE PIC  Z9/99/9999.
002670		   07  FILLER  PIC  X(01).
002680		   07  LED-CDE PIC  X(04).
		   07  FILLER  PIC  X(01).
		   07  LED-BTCH
			       PIC  Z(06).
002690		   07  FILLER  PIC  X(02).
		   07  LED-REFER.
002700		   09  LED-REF PIC  Z(07).
002710		   07  FILLER  PIC  X(02).
002720		   07  LED-NAR PIC  X(36).
002730		   07  FILLER  PIC  X(01).
002740		   07  LED-VAL PIC  Z(07)9.99-.
002750		   07  FILLER  PIC  X(01).
002760		   07  LED-BAL PIC  Z(07)9.99-.

	   03  REC-L1 REDEFINES W02-PRN-LINE.
002610	       05  REC-AC      PIC  9(04)/99.
002620	       05  FILLER      PIC  X(02).
002630	       05  REC-NME     PIC  X(40).
	       05  FILLER      PIC  X(83).

	   03  REC-L2 REDEFINES W02-PRN-LINE.
002620	       05  FILLER      PIC X(32).
	       05  REC-REC     PIC X(01).
	       05  FILLER      PIC X(04).
002650	       05  REC-TRAN.
002660		   07  REC-DTE PIC  Z9/99/9999.
002670		   07  FILLER  PIC  X(01).
002680		   07  REC-CDE PIC  X(04).
002690		   07  FILLER  PIC  X(01).
		   07  REC-BTCH
			       PIC  Z(06).
002690		   07  FILLER  PIC  X(02).
002700		   07  REC-REF PIC  Z(07).
002710		   07  FILLER  PIC  X(02).
002720		   07  REC-NAR PIC  X(36).
002730		   07  FILLER  PIC  X(01).
002740		   07  REC-VAL PIC  Z(07)9.99-.
002750		   07  FILLER  PIC  X(01).
002760		   07  REC-BAL PIC  Z(07)9.99-.

002770	   03  CHQ-L1 REDEFINES W02-PRN-LINE.
002780	       05  FILLER      PIC  X(04).
002790	       05  CHQ-H2      PIC  X(04).
002800	       05  FILLER      PIC  X(04).
002810	       05  CHQ-H3      PIC  X(05).
002820	       05  FILLER      PIC  X(33).
002830	       05  CHQ-H4      PIC  X(08).
002840	       05  FILLER      PIC  X(30).
002850	       05  CHQ-H1      PIC  X(07).
002860	       05  FILLER      PIC  X(02).
002870	       05  CHQ-H5      PIC  X(07).
002880	       05  FILLER      PIC  X(07).
002890	       05  CHQ-H6      PIC  X(05).
002900	       05  FILLER      PIC  X(07).
002910	       05  CHQ-H7      PIC  X(06).
002920	       05  FILLER      PIC  X(03).

002930	   03  CHQ-L2 REDEFINES W02-PRN-LINE.
002940	       05  FILLER      PIC  X(01).
002950	       05  CHQ-DTE     PIC  Z9/99/9999.
002960	       05  FILLER      PIC  X(01).
002970	       05  CHQ-PAY     PIC  X(36).
002980	       05  FILLER      PIC  X(02).
002990	       05  CHQ-NAR     PIC  X(36).
003000	       05  FILLER      PIC  X(02).
003010	       05  CHQ-NO      PIC  Z(06)9.
003020	       05  FILLER      PIC  X(02).
003030	       05  CHQ-AC      PIC  9(04)/99.
003040	       05  FILLER      PIC  X(01).
003050	       05  CHQ-DT      PIC  Z(07)9.99.
003060	       05  FILLER      PIC  X(01).
003070	       05  CHQ-CR      PIC  Z(07)9.99-.
003080	       05  FILLER      PIC  X(02).

003090	   03  BAL-L1 REDEFINES W02-PRN-LINE.
003100	       05  FILLER      PIC  X(19).
003110	       05  BAL-H1      PIC  X(11).
003120	       05  BAL-H2      PIC  X(07).
003130	       05  BAL-H3      PIC  X(18).
003140	       05  BAL-H4.
003150		   07  FILLER  PIC  X(14).
003160		   07  BAL-H5  PIC  X(12).
003170	       05  BAL-H6      PIC  X(14).
003180	       05  BAL-H7      PIC  X(07).
003190	       05  BAL-H8      PIC  X(05).
003200	       05  BAL-H9      PIC  X(06).
003210	       05  FILLER      PIC  X(19).

003220	   03  BAL-L2 REDEFINES W02-PRN-LINE.
003230	       05  FILLER      PIC  X(20).
003240	       05  BAL-GRP     PIC  9(02).
003250	       05  FILLER      PIC  X(03).
003260	       05  BAL-GRNAME  PIC  X(24).
003270	       05  FILLER      PIC  X(83).

003280	   03  BAL-L3 REDEFINES W02-PRN-LINE.
003290	       05  FILLER      PIC  X(30).
003300	       05  BAL-AC      PIC  9(04).
003310	       05  FILLER      PIC  X(03).
003320	       05  BAL-ACNAME  PIC  X(24).
003330	       05  FILLER      PIC  X(02).
003340	       05  BAL-DEBT    PIC  Z(07)9.99.
003350	       05  FILLER      PIC  X(02).
003360	       05  BAL-CRED    PIC  Z(07)9.99-.
003370	       05  FILLER      PIC  X(01).
003380	       05  BAL-TDBT    PIC  Z(07)9.99.
003390	       05  FILLER      PIC  X(02).
003400	       05  BAL-TCRD    PIC  Z(07)9.99-.
003410	       05  FILLER      PIC  X(18).

003420	   03  DEP-L1 REDEFINES W02-PRN-LINE.
003430	       05  FILLER      PIC  X(22).
003440	       05  DEP-H1      PIC  X(22).
003450	       05  DEP-H2      PIC  X(14).
003460	       05  DEP-H3      PIC  X(08).
003470	       05  DEP-H4      PIC  X(14).
003480	       05  FILLER      PIC  X(19).
003490	       05  DEP-H5      PIC  X(33).

003500	   03  DEP-L2 REDEFINES W02-PRN-LINE.
003510	       05  FILLER      PIC  X(20).
003520	       05  DEP-DATE    PIC  Z9/99/9999.
003530	       05  FILLER      PIC  X(14).
003540	       05  DEP-NAR     PIC  X(36).
003550	       05  FILLER      PIC  X(14).
003560	       05  DEP-VAL     PIC  Z(07)9.99-.
003570	       05  FILLER      PIC  X(26).

003580	   03  P-L2 REDEFINES W02-PRN-LINE.
003590	       05  P-H1        PIC  X(06).
003600	       05  P-DTE1      PIC  Z9/99/9999.
003610	       05  FILLER      PIC  X(18).
003620	       05  P-H2        PIC  X(40).
003630	       05  P-HEAD      PIC  X(49).
003640	       05  P-H4        PIC  X(06).
003650	       05  P-PGE       PIC  ZZ9.

003660	   03  P-L3 REDEFINES W02-PRN-LINE.
003670	       05  FILLER      PIC  X(13).
003680	       05  P-HD1       PIC  X(06).
003690	       05  FILLER      PIC  X(33).
003700	       05  P-HD2       PIC  X(14).
003710	       05  FILLER      PIC  X(05).

003720	   03  P-L4 REDEFINES W02-PRN-LINE.
003730	       05  FILLER      PIC  X(02).
003740	       05  P-HD3       PIC  X(07).
003750	       05  P-HD4       PIC  X(07).
003760	       05  P-HD5       PIC  X(11).
003770	       05  P-HD6       PIC  X(07).
003780	       05  P-HD7       PIC  X(07).
003790	       05  P-HD8       PIC  X(08).
003800	       05  P-HD9       PIC  X(06).
003810	       05  P-HD10      PIC  X(24).
003820	       05  P-HD11      PIC  X(09).
003830	       05  P-HD12      PIC  X(07).
003840	       05  P-HD13      PIC  X(07).
003850	       05  P-HD14      PIC  X(11).
003860	       05  P-HD15      PIC  X(07).
003870	       05  P-HD16      PIC  X(07).
003880	       05  P-HD17      PIC  X(05).

003890	   03  P-L5 REDEFINES W02-PRN-LINE.
003900	       05  P-MTD1      PIC  Z(6)9- BLANK WHEN ZERO.
003910	       05  FILLER      PIC  X(01).
003920	       05  P-MGRP1     PIC  ZZ9.99 BLANK WHEN ZERO.
003930	       05  FILLER      PIC  X(01).
003940	       05  P-MTOT1     PIC  ZZ9.99 BLANK WHEN ZERO.
003950	       05  FILLER      PIC  X(03).
003960	       05  P-LN4.
003970		   07  P-LN6.
003980		       09  P-YTD1
			       PIC  Z(6)9- BLANK WHEN ZERO.
003990		   07  FILLER  PIC  X(01).
004000		   07  P-YGRP1 PIC  ZZ9.99 BLANK WHEN ZERO.
004010		   07  FILLER  PIC  X(01).
004020		   07  P-LN7.
004030		       09  P-YTOT1
			       PIC  ZZ9.99 BLANK WHEN ZERO.
004040		   07  FILLER  PIC  X(02).
004050		   07  P-ACNO1 PIC  9999.
004060		   07  FILLER  PIC  X(01).
004070		   07  P-DESC  PIC  X(24).
004080		   07  FILLER  PIC  X(01).
004090		   07  P-ACNO2 PIC  9999.
004100		   07  FILLER  PIC  X(02).
004110		   07  P-LN8.
004120		       09  P-MTD2
			       PIC  Z(6)9- BLANK WHEN ZERO.
004130		   07  FILLER  PIC  X(01).
004140		   07  P-MGRP2 PIC  ZZ9.99 BLANK WHEN ZERO.
004150		   07  FILLER  PIC  X(01).
004160		   07  P-LN9.
004170		       09  P-MTOT2
			       PIC  ZZ9.99 BLANK WHEN ZERO.
004180	       05  FILLER      PIC  X(03).
004190	       05  P-YTD2      PIC  Z(6)9- BLANK WHEN ZERO.
004200	       05  FILLER      PIC  X(01).
004210	       05  P-YGRP2     PIC  ZZ9.99 BLANK WHEN ZERO.
004220	       05  FILLER      PIC  X(01).
004230	       05  P-YTOT2     PIC  ZZ9.99 BLANK WHEN ZERO.

004240	   03  P-L6 REDEFINES W02-PRN-LINE.
004250	       05  P-LN10      PIC  X(22).
004260	       05  FILLER      PIC  X(03).
004270	       05  P-LN11      PIC  X(22).
004280	       05  FILLER      PIC  X(03).
004290	       05  P-LN2       PIC  X(34).
004300	       05  P-LN13      PIC  X(22).
004310	       05  FILLER      PIC  X(03).
004320	       05  P-LN14      PIC  X(22).

       COPY W05.GL.

005470 01  W10-H2.
005480     03  FILLER          PIC X(27).
005490     03  W10-HEAD        PIC X(13).

       COPY W15.GL.

005880 01  W20-TOTALS.
005890     03  W20-DEBIT       PIC S9(09)V99 COMP-3.
005900     03  W20-CREDIT      PIC S9(09)V99 COMP-3.
005910     03  W20-DR          PIC S9(09)V99 COMP-3.
005920     03  W20-CR          PIC S9(09)V99 COMP-3.
005930     03  W20-RESULT      PIC S9(09)V99.
005940     03  W20-RES1 REDEFINES W20-RESULT.
005950         05  W20-WHOLE   PIC 9(09).
005960         05  W20-DEC     PIC S9(02).
005970     03  W20-TOTAL       PIC S9(09)V99 COMP-3.

       COPY W20.WS.

002850 01  W25-CALCS.
002860     03  W25-RESULT      PIC 9(05)V99.
002870     03  W25-RESULT1 REDEFINES W25-RESULT.
002880         05  W25-WHOLE   PIC 9(05).
002890         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(04).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).

005980 01  W25-EDIT.
005990     03  W25-7.
006000         05  W25-1       PIC ZZZZZZZ9-.
006010     03  W25-S7V2.
006020         05  W25-V1      PIC Z(07)9.99CR.
003990	   03  W25-S8V2.
004000	       05  W25-VG1     PIC Z(07)9.99CR.
006030	   03  W25-DATE.
006040	       05  W25-DTE     PIC Z9/99/9999.
006050     03  W25-REF.
006060         05  W25-REFER   PIC Z(07).

006070 01  W30-INACTS.
006080     02  W30-TAB1.
006090     03  W30-INCOME-TABLE OCCURS 50.
006100         05  W30-INACNO  PIC 9(04).
006110         05  W30-INKEY   PIC 9(05)  COMP-0.
006120     02  W30-TAB2 REDEFINES W30-TAB1.
006130         05  W30-ING OCCURS 50.
006140             07 W30-INGRP PIC 9(02).
006150             07 W30-INSAC PIC 9(04).
006160     02  W30-TAB3 REDEFINES W30-TAB1.
006170         05  W30-INGS OCCURS 50.
006180             07  FILLER  PIC 9(01).
006190             07 W30-INGSUB PIC 9(03).
006200             07  FILLER  PIC 9(02).

006210 01  W30-COACTS.
006220     02  W30-TAB4.
006230     03  W30-COST-TABLE OCCURS 50.
006240         05  W30-COACNO  PIC 9(04).
006250         05  W30-COKEY   PIC 9(05)  COMP-0.
006260     02  W30-TAB5 REDEFINES W30-TAB4.
006270         05  W30-CNG OCCURS 50.
006280             07 W30-COGRP  PIC 9(02).
006290             07 W30-COSAC  PIC 9(04).
006300     02  W30-TAB6 REDEFINES W30-TAB4.
006310         05  W30-COGS OCCURS 50.
006320             07 FILLER     PIC 9(01).
006330             07 W30-COGSUB PIC 9(03).
006340             07 FILLER     PIC 9(02).
000550 01  W30-MONTHS.
000560     03  W30-MONTH-NAMES.
000570	       05  W30-ENG.
000580		   07  FILLER  PIC  X(09) VALUE "JANUARY  ".
		   07  FILLER  PIC  X(09) VALUE "FEBRUARY ".
		   07  FILLER  PIC  X(09) VALUE "MARCH	  ".
		   07  FILLER  PIC  X(09) VALUE "APRIL    ".
		   07  FILLER  PIC  X(09) VALUE "MAY      ".
		   07  FILLER  PIC  X(09) VALUE "JUNE     ".
		   07  FILLER  PIC  X(09) VALUE "JULY     ".
		   07  FILLER  PIC  X(09) VALUE "AUGUST   ".
		   07  FILLER  PIC  X(09) VALUE "SEPTEMBER".
		   07  FILLER  PIC  X(09) VALUE "OCTOBER  ".
		   07  FILLER  PIC  X(09) VALUE "NOVEMBER ".
		   07  FILLER  PIC  X(09) VALUE "DECEMBER ".
000590     03  W30-MONTH REDEFINES W30-MONTH-NAMES
000600			       PIC  X(09)  OCCURS 12.
	   03  W30-YEAR        PIC  9(04).

       COPY W40.WS.

       COPY FUNCTION.WS.

006560 01  W50-CALC.
006570     05  W50-ACNTS OCCURS 30.
006580         07  W50-ANO     PIC 9(06)     COMP-3.
006590         07  W50-PER     PIC 9(03)V99  COMP-3.
006600 01  W55-TOTALS.
006610     03  W55-VAL         PIC S9(09)    COMP-3 OCCURS 12.
006620 01  W55-TOT REDEFINES W55-TOTALS.
006630     03  W55-1           PIC S9(09)    COMP-3.
006640     03  W55-2           PIC S9(09)    COMP-3.
006650     03  W55-3           PIC S9(09)    COMP-3.
006660     03  W55-4           PIC S9(09)    COMP-3.
006670     03  W55-5           PIC S9(09)    COMP-3.
006680     03  W55-6           PIC S9(09)    COMP-3.
006690     03  W55-7           PIC S9(09)    COMP-3.
006700     03  W55-8           PIC S9(09)    COMP-3.
006710     03  W55-9           PIC S9(09)    COMP-3.
006720     03  W55-10          PIC S9(09)    COMP-3.
006730     03  W55-11          PIC S9(09)    COMP-3.
006740     03  W55-12          PIC S9(09)    COMP-3.
006750 01  W60-TOTALS.
006760     03  W60-VAL         PIC S9(09)    COMP-3 OCCURS 12.
006770 01  W65-TOTALS.
006780     03  W65-VAL         PIC S9(09)    COMP-3 OCCURS 12.
006790 01  W70-ACTS.
006800     03  W70-BRANCH      PIC 9(04)     COMP-5.

007000 COPY W70.WS.

007040 01  W75-KEYS.
007050     03  W75-S          PIC 9(02) COMP-5.
007060     03  W75-S1         PIC 9(02) COMP-5.
007070	   03  W75-GNO	      PIC 9(06) COMP-5
007080                        OCCURS 18.

007090 01  W80-HEAD.
007100     03  W80-H1          PIC X(22).
007110	   03  FILLER	       PIC X(17) VALUE "FOR THE MONTH OF".
	   03  W80-MONTH.
	       05  W80-CHAR    PIC  X(01) OCCURS 14.
	   03  W80-YEAR REDEFINES W80-MONTH.
	       05  FILLER		       PIC  X(04).
	       05  W80-MTH5.
		 07  FILLER		       PIC  X(01).
		 07  W80-MTH6-7.
		   09  FILLER		       PIC  X(01).
		   09  W80-MTH3-4.
		     11  FILLER 	       PIC  X(01).
		     11  W80-MTH8.
		       13  FILLER	       PIC  X(01).
		       13  W80-MTH1-10.
			 15  FILLER	       PIC  X(01).
			 15  W80-MTH2-11-12.
			   17  FILLER	       PIC  X(01).
			   17  W80-MTH9        PIC  X(04).

007130 01  W90-TOTALS.
007140     03  W90-ACCUM.
007150         05  W90-V1      PIC S9(09)V99 COMP-3.
007160         05  W90-V2      PIC S9(09)V99 COMP-3.
007170         05  W90-V3      PIC S9(09)V99 COMP-3.
007180         05  W90-V4      PIC S9(09)V99 COMP-3.
007190         05  W90-V5      PIC S9(09)V99 COMP-3.
007200         05  W90-V6      PIC S9(09)V99 COMP-3.
007210         05  W90-V7      PIC S9(09)V99 COMP-3.
007220         05  W90-V8      PIC S9(09)V99 COMP-3.
007230         05  W90-V9      PIC S9(09)V99 COMP-3.
007240         05  W90-V10     PIC S9(09)V99 COMP-3.
007250         05  W90-V11     PIC S9(09)V99 COMP-3.
007260         05  W90-V12     PIC S9(09)V99 COMP-3.
007270         05  W90-V13     PIC S9(09)V99 COMP-3.
007280         05  W90-V14     PIC S9(09)V99 COMP-3.
007290         05  W90-V15     PIC S9(09)V99 COMP-3.
007300         05  W90-V16     PIC S9(09)V99 COMP-3.
007310         05  W90-V17     PIC S9(09)V99 COMP-3.
007320         05  W90-V18     PIC S9(09)V99 COMP-3.
007330     03  W90-TOT  REDEFINES W90-ACCUM.
007340         05  W90-VAL     PIC S9(09)V99 COMP-3 OCCURS 18.
007350     03  W90-PER         PIC 9(03)V9(06) COMP-3.
007360 01  W90-COMP.
007370     03  W90-CNAME       PIC X(40).

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
007380 SCREEN SECTION.

       COPY BLANK.CRT.

       01  S-MTH.
	   03  FOREGROUND-COLOR 7 HIGHLIGHT
	       PIC 99 USING WS-MM AUTO.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
008530 AA000	       SECTION.
008540 AA00.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA35.
003240	   IF LS0-GLLEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA35.
008560	     PERFORM ZA000.
	     PERFORM DA000.

009030 AA35.
009040       EXIT PROGRAM.

009050 AA50.
	     MOVE "H"		 TO W02-PRN-TYPE.
009060	     MOVE SPACES	 TO R-DET.
009070	     MOVE WS-H1 	 TO R-H1.
009080	     MOVE W90-COMP	 TO R-H2.
009090	     MOVE WS-H4 	 TO R-H4.
009100	     ADD 1		 TO WS-PAGE.
009110	     MOVE WS-PAGE	 TO R-PGE.
009120	     MOVE W05-TODAY	 TO R-DTE1.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
009140	     MOVE SPACES	 TO R-DET.
009150 AA55.
009160       EXIT.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP6.

009820 AB000	       SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.

       COPY GACCNT.RD.

      /
010460 AC000           SECTION.

       COPY CONTROL.WR.

      /
014630 DA000	       SECTION 5.
014640 DA00.
014650       DISPLAY CLR-SCREEN.
014660       DISPLAY "OPERATING STATEMENT (Profit/Loss)" AT 0225
014670                WITH FOREGROUND-COLOR 15.
014680       DISPLAY "Are you sure (" AT 0412
014700		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
014720		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT ")?".
014740 DA01.
014750	     ACCEPT WS-OPTION AT 0432
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
014810	   IF NOT (WS-OPTION = "N" OR "Y")
014820         GO TO DA01.
014830     IF WS-OPTION = "N"
014840         GO TO DA999.
014850       DISPLAY "Have Totals been accumulated (" AT 0612
014870		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
014890		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT ")?".

014910 DA02.
014920	     ACCEPT WS-OPTION AT 0648
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
014980	   IF NOT (WS-OPTION = "N" OR "Y")
014990         GO TO DA02.
015000     IF WS-OPTION = "N"
015010         GO TO DA999.
015020       DISPLAY "Detailed print (" AT 0812
015040		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
015060		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT ")?".

015080 DA03.
015090	     ACCEPT WS-OPTION AT 0834
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
015150	   IF NOT (WS-OPTION = "N" OR "Y")
015160         GO TO DA03.
015170     IF NOT (WS-OPTION = "N")
015180	       MOVE ZERO	 TO WS-EXS
015190     ELSE
015200	       MOVE 1		 TO WS-EXS.
015210       DISPLAY "Print inactive accounts (" AT 1012
015230		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
015250		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT ")?".

015270 DA04.
015280	     ACCEPT WS-OPTION AT 1043
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
015340	   IF NOT (WS-OPTION = "N" OR "Y")
015350         GO TO DA04.
015360     IF NOT (WS-OPTION = "N")
015370         MOVE ZERO TO WS-ZERO
015380     ELSE
015390         MOVE 1 TO WS-ZERO.
	     MOVE W05-MM	 TO WS-MM.
	     MOVE W05-CY	 TO W30-YEAR.
	     DISPLAY "Key in month :" AT 1212.

015400 DA05.
	     ACCEPT S-MTH AT 1227.
	   IF WS-MM < 01 OR > 12
	       MOVE W05-MM	 TO WS-MM.
	   IF WS-MM > W05-MM
	       SUBTRACT 1	 FROM W30-YEAR.
015410	     DISPLAY "Account No :" AT 1412.

015420 DA15.
015430	     MOVE WS-MM		 TO WS-S7.
015440	   IF WS-S7 = 1
015450	       MOVE 12		 TO WS-S8
015460     ELSE
015470	       SUBTRACT 1 FROM WS-S7
				 GIVING WS-S8.
015480	     MOVE ZERO		 TO WS-PAGE.
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 3		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.

015510 DA25.
015520       PERFORM AA50 THRU AA55.
015530	     MOVE SPACES	 TO R-DET.
015540	     MOVE "OPERATING STATEMENT -"
				 TO W80-H1.
	     MOVE W30-MONTH (WS-MM)
				 TO W80-MONTH.
	   IF WS-MM = 5
	       MOVE W30-YEAR	 TO W80-MTH5
	   ELSE
	   IF WS-MM = 6 OR 7
	       MOVE W30-YEAR	 TO W80-MTH6-7
	   ELSE
	   IF WS-MM = 3 OR 4
	       MOVE W30-YEAR	 TO W80-MTH3-4
	   ELSE
	   IF WS-MM = 8
	       MOVE W30-YEAR	 TO W80-MTH8
	   ELSE
	   IF WS-MM = 1 OR 10
	       MOVE W30-YEAR	 TO W80-MTH1-10
	   ELSE
	   IF WS-MM = 2 OR 11 OR 12
	       MOVE W30-YEAR	 TO W80-MTH2-11-12
	   ELSE
	   IF WS-MM = 9
	       MOVE W30-YEAR	 TO W80-MTH9.
015560	     MOVE W80-HEAD	 TO STM-H3.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
015580	     MOVE ALL "-"	 TO STM-H3.
	   IF WS-MM = 5
	       MOVE SPACES	 TO STM-MTH5
	   ELSE
	   IF WS-MM = 6 OR 7
	       MOVE SPACES	 TO STM-MTH6-7
	   ELSE
	   IF WS-MM = 3 OR 4
	       MOVE SPACES	 TO STM-MTH3-4
	   ELSE
	   IF WS-MM = 8
	       MOVE SPACES	 TO STM-MTH8
	   ELSE
	   IF WS-MM = 1 OR 10
	       MOVE SPACES	 TO STM-MTH1-10
	   ELSE
	   IF WS-MM = 2 OR 11 OR 12
	       MOVE SPACES	 TO STM-MTH2-11-12.
	     PERFORM CALL-PRINTUTL.
015600	     MOVE SPACES	 TO R-DET.
015610       MOVE "YEAR TO DATE" TO STM-H4.
015620	     MOVE "MONTH"	 TO STM-H5.
	     PERFORM CALL-PRINTUTL.
015640	     MOVE ALL "-"	 TO STM-H6.
015650	     MOVE "PERC"	 TO STM-H7.
015660	     MOVE ALL "-"	 TO STM-H8.
	     PERFORM CALL-PRINTUTL.
015680	     MOVE SPACES	 TO R-DET.
015690	     MOVE "  TARGET"	 TO STM-H9 STM-H17.
015700	     MOVE " %"		 TO STM-H10 STM-H12 STM-H14
015710				    STM-H18 STM-H20 STM-H22.
015720	     MOVE "     ACTUAL"  TO STM-H11 STM-H19.
015730	     MOVE "  LAST YEAR"  TO STM-H13 STM-H21.
015740	     MOVE "YTD" 	 TO STM-H15.
015750	     MOVE "DESCRIPTION"  TO STM-H16.
	     PERFORM CALL-PRINTUTL.
015770	     MOVE ALL "-"	 TO STM-H15 STM-H16.
	     MOVE "D"		 TO W02-PRN-TYPE.

015780 DA32.
015790	     MOVE ALL "-"	 TO STM-H9  STM-H10 STM-H11 STM-H12
015800				    STM-H13 STM-H14 STM-H17 STM-H18
015810				    STM-H19 STM-H20 STM-H21 STM-H22.
	     PERFORM CALL-PRINTUTL.
015840	     MOVE SPACES	 TO R-DET.

015850 DA34.
	     MOVE 100000	 TO G-AC.
	     PERFORM START-AT-LEDG-AC THRU READ-GACCNT-EXIT.
	   IF WS-F-ERROR = 8
	       GO TO DA75.
015870	     MOVE ZERO		 TO W15-GR.
015880	     MOVE 1		 TO WS-S11.

015890 DA34A.
015900	     MOVE ZERO		 TO W90-VAL (WS-S11).
015910     IF WS-S11 < 18
015920	       ADD 1		 TO WS-S11
015930         GO TO DA34A.

015940 DA35.
	     PERFORM READ-GACCNT-NEXT THRU READ-GACCNT-EXIT.
014220	   IF WS-F-ERROR = 8
015960	       GO TO DA75.
	   IF NOT (WS-PRINTER = "S")
016020	       DISPLAY G-AC AT 1425
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
015970	   IF G-ASER NOT = ZERO
015980         IF (WS-ZERO = 1) AND (G-BAL = ZERO) AND 
015990            (G-VAL (WS-S8) = ZERO) 
016000             GO TO DA73.
016010	   IF W15-GRP = G-GRP
016030	       GO TO DA60.
016040     IF G-GRP > 3
016050         GO TO DA75.
016070     IF G-GRP = 1
016080	       MOVE "*** INCOME ***"
				 TO STM-ANAME
016090	       PERFORM CALL-PRINTUTL
016100	       MOVE SPACES	 TO R-DET
016110         GO TO DA60.
016120     IF W15-GRP = 1
016130         GO TO DA95.
016140     IF W15-GR = ZERO
016150         GO TO DA55.
016160     IF W15-GRP > 2
016170         GO TO DA60.

016180 DA40.
016190       PERFORM DA32.
016200	     MOVE "TOTAL COST OF SALES"
				 TO STM-DESC.
016210 DA45.
016220	     MOVE W90-V7	 TO STM-V1.
016230     IF W90-V1 NOT = ZERO
016240         COMPUTE W90-PER = W90-V7 / W90-V1 * 100
016250	       MOVE W90-PER	 TO STM-P1.
016260	     MOVE W90-V8	 TO STM-V2.
016270     IF W90-V2 NOT = ZERO
016280         COMPUTE W90-PER = W90-V8 / W90-V2 * 100
016290	       MOVE W90-PER	 TO STM-P2.
016300	     MOVE W90-V9	 TO STM-V3.
016310     IF W90-V3 NOT = ZERO
016320         COMPUTE W90-PER = W90-V9 / W90-V3 * 100
016330	       MOVE W90-PER	 TO STM-P3.
016340	     MOVE W90-V10	 TO STM-V4.
016350     IF W90-V4 NOT = ZERO
016360         COMPUTE W90-PER = W90-V10 / W90-V4 * 100
016370	       MOVE W90-PER	 TO STM-P4.
016380	     MOVE W90-V11	 TO STM-V5.
016390     IF W90-V5 NOT = ZERO
016400         COMPUTE W90-PER = W90-V11 / W90-V5 * 100
016410	       MOVE W90-PER	 TO STM-P5.
016420	     MOVE W90-V12	 TO STM-V6.
016430     IF W90-V6 NOT = ZERO
016440         COMPUTE W90-PER = W90-V12 / W90-V6 * 100
016450	       MOVE W90-PER	 TO STM-P6.
016460	     PERFORM CALL-PRINTUTL.
016470	     MOVE SPACES	 TO R-DET.
016480       PERFORM DA32.

016490 DA50.
016500       MOVE "GROSS PROFIT" TO STM-DESC.
016510	     ADD W90-V1 	 TO W90-V7.
016520	     ADD W90-V2 	 TO W90-V8.
016530	     ADD W90-V3 	 TO W90-V9.
016540	     ADD W90-V4 	 TO W90-V10.
016550	     ADD W90-V5 	 TO W90-V11.
016560	     ADD W90-V6 	 TO W90-V12.
016570       PERFORM DA45.

016580 DA55.
016590	     MOVE "*** EXPENSES ***"
				 TO STM-ANAME.
016600	     PERFORM CALL-PRINTUTL.
016610	     MOVE SPACES	 TO R-DET.

016620 DA60.
016630	     MOVE G-GR		 TO W15-GR.
016640     IF WS-EXS = 1
016650         IF G-ASER NOT = ZERO
016660             GO TO DA73.
016670	     MOVE G-NAME	 TO STM-ANAME.
016680	     MOVE G-AC		 TO STM-AC.
016690     IF WS-EXS = ZERO
016700         IF G-ASER = ZERO
016710             PERFORM DA100 THRU DA115
016720             GO TO DA72.
016730     IF WS-EXS = 1
016740         IF G-SR = ZERO
016750             IF NOT (G-GR = 10 OR 20 OR 30)
016760                 PERFORM DA100 THRU DA115
016770                 GO TO DA72
016780             ELSE
016790                 GO TO DA72.
016800	     MOVE 1		 TO WS-S1.
016810	     MOVE 2		 TO WS-S2.
016820	     MOVE 3		 TO WS-S3.
016830	     MOVE 4		 TO WS-S4.
016840	     MOVE 5		 TO WS-S5.
016850	     MOVE 6		 TO WS-S6.
016860     IF W15-GRP = 2
016870	       ADD 6		 TO WS-S1
016880	       ADD 6		 TO WS-S2
016890	       ADD 6		 TO WS-S3
016900	       ADD 6		 TO WS-S4
016910	       ADD 6		 TO WS-S5
016920	       ADD 6		 TO WS-S6
016930     ELSE
016940     IF W15-GRP = 3
016950	       ADD 12		 TO WS-S1
016960	       ADD 12		 TO WS-S2
016970	       ADD 12		 TO WS-S3
016980	       ADD 12		 TO WS-S4
016990	       ADD 12		 TO WS-S5
017000	       ADD 12		 TO WS-S6.
017010	     MOVE ZERO		 TO W20-DEBIT.
017020     IF WS-S7 < WS-F-YR
017030	       MOVE 1		 TO WS-S9
017040	       MOVE 12		 TO WS-S10
017050     ELSE
017060	       MOVE WS-S7	 TO WS-S10
017070	       MOVE WS-F-YR	 TO WS-S9
017080         GO TO DA70.

017090 DA65.
017100	     ADD G-BVAL (WS-S9)  TO W20-DEBIT.
017110     IF WS-S9 < WS-S7
017120	       ADD 1		 TO WS-S9
017130         GO TO DA65.
017140	     MOVE WS-F-YR	 TO WS-S9.

017150 DA70.
	   IF WS-MM = W05-MM
	       MOVE G-BAL	 TO G-VAL (WS-S7).
017160	     ADD G-BVAL (WS-S9)  TO W20-DEBIT.
017170     IF WS-S9 < WS-S10
017180	       ADD 1		 TO WS-S9
017190         GO TO DA70.
017200     IF W15-GRP = 1
017210	       MULTIPLY -1	 BY W20-DEBIT.
017220	     ADD W20-DEBIT	 TO W90-VAL (WS-S1).
017230	     MOVE W20-DEBIT	 TO STM-V1.
017240     IF W90-V1 NOT = ZERO
017250     IF W15-GRP = 1
017260	       MOVE 100 	 TO STM-P1
017270     ELSE
017280         COMPUTE W90-PER = W20-DEBIT / W90-V1 * 100
017290	       MOVE W90-PER	 TO STM-P1.
017300	     MOVE G-VAL (WS-S7)  TO STM-V2 W20-DEBIT W20-CREDIT.
017310	     ADD W20-DEBIT	 TO W90-VAL (WS-S2).
017320	   IF W90-V2 NOT = ZERO
017330     IF W15-GRP = 1
017340	       MOVE 100 	 TO STM-P2
017350     ELSE
017360         COMPUTE W90-PER = W20-DEBIT / W90-V2 * 100
017370	       MOVE W90-PER	 TO STM-P2.
017380       MOVE G-LVAL (WS-S7) TO STM-V3 W20-DEBIT.
017390	     ADD W20-DEBIT	 TO W90-VAL (WS-S3).
017400     IF W90-V3 NOT = ZERO
017410     IF W15-GRP = 1
017420	       MOVE 100 	 TO STM-P3
017430     ELSE
017440         COMPUTE W90-PER = W20-DEBIT / W90-V3 * 100
017450	       MOVE W90-PER	 TO STM-P3.
017460     IF W20-TOTAL NOT = ZERO
017470         COMPUTE W90-PER = W20-CREDIT / W20-TOTAL * 100
017480	       MOVE W90-PER	 TO STM-GP.
017490       MOVE G-BVAL (WS-S7) TO W20-DEBIT.
017500     IF W15-GRP = 1
017510	       MULTIPLY -1	 BY W20-DEBIT.
017520	     MOVE W20-DEBIT	 TO STM-V4.
017530	     ADD W20-DEBIT	 TO W90-VAL (WS-S4).
017540     IF W90-V4 NOT = ZERO
017550     IF W15-GRP = 1
017560	       MOVE 100 	 TO STM-P4
017570     ELSE
017580         COMPUTE W90-PER = W20-DEBIT / W90-V4 * 100
017590	       MOVE W90-PER	 TO STM-P4.
017600	     MOVE G-VAL (WS-S7)  TO W20-DEBIT.
017610	     SUBTRACT G-VAL (WS-S8)
				 FROM W20-DEBIT.
017620	     MOVE W20-DEBIT	 TO STM-V5.
017630	     ADD W20-DEBIT	 TO W90-VAL (WS-S5).
017640     IF W90-V5 NOT = ZERO
017650     IF W15-GRP = 1
017660	       MOVE 100 	 TO STM-P5
017670     ELSE
017680         COMPUTE W90-PER = W20-DEBIT / W90-V5 * 100
017690	       MOVE W90-PER	 TO STM-P5.
017700       MOVE G-LVAL (WS-S7) TO W20-DEBIT.
017710	     SUBTRACT G-LVAL (WS-S8)
				 FROM W20-DEBIT.
017720	     MOVE W20-DEBIT	 TO STM-V6.
017730	     ADD W20-DEBIT	 TO W90-VAL (WS-S6).
017740     IF W90-V6 NOT = ZERO
017750     IF W15-GRP = 1
017760	       MOVE 100 	 TO STM-P6
017770     ELSE
017780         COMPUTE W90-PER = W20-DEBIT / W90-V6 * 100
017790	       MOVE W90-PER	 TO STM-P6.

017800 DA72.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
017850	       PERFORM DA25 THRU DA32.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
				 TO R-DET
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       GO TO DA86.
017860	     MOVE SPACES	 TO R-DET.

017870 DA73.
017890       GO TO DA35.

017900 DA75.
017910       PERFORM DA32.
017920	     MOVE "TOTAL EXPENDITURE"
				 TO STM-DESC.
017930 DA80.
017940	     MOVE W90-V13	 TO STM-V1.
017950     IF W90-V1 NOT = ZERO
017960         COMPUTE W90-PER = W90-V13 / W90-V1 * 100
017970	       MOVE W90-PER	 TO STM-P1.
017980	     MOVE W90-V14	 TO STM-V2.
017990     IF W90-V2 NOT = ZERO
018000         COMPUTE W90-PER = W90-V14 / W90-V2 * 100
018010	       MOVE W90-PER	 TO STM-P2.
018020	     MOVE W90-V15	 TO STM-V3.
018030     IF W90-V3 NOT = ZERO
018040         COMPUTE W90-PER = W90-V15 / W90-V3 * 100
018050	       MOVE W90-PER	 TO STM-P3.
018060	     MOVE W90-V16	 TO STM-V4.
018070     IF W90-V4 NOT = ZERO
018080         COMPUTE W90-PER = W90-V16 / W90-V4 * 100
018090	       MOVE W90-PER	 TO STM-P4.
018100	     MOVE W90-V17	 TO STM-V5.
018110     IF W90-V5 NOT = ZERO
018120         COMPUTE W90-PER = W90-V17 / W90-V5 * 100
018130	       MOVE W90-PER	 TO STM-P5.
018140	     MOVE W90-V18	 TO STM-V6.
018150     IF W90-V6 NOT = ZERO
018160         COMPUTE W90-PER = W90-V18 / W90-V6 * 100
018170	       MOVE W90-PER	 TO STM-P6.
	     PERFORM CALL-PRINTUTL.
018190	     MOVE SPACES	 TO R-DET.
018200       PERFORM DA32.

018210 DA85.
018220	     ADD W90-V13	 TO W90-V7.
018230	     ADD W90-V14	 TO W90-V8.
018240	     ADD W90-V15	 TO W90-V9.
018250	     ADD W90-V16	 TO W90-V10.
018260	     ADD W90-V17	 TO W90-V11.
018270	     ADD W90-V18	 TO W90-V12.
018280	     MOVE "TOTAL PROFIT/LOSS THIS MONTH"
				 TO STM-DESC.
018290       PERFORM DA45.

       DA86.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
      *    *************************************************************
      *    ****   S T O R E   T H E   P R O F I T / L O S S
      * 	  The calculated PROFIT/LOSS is stored in record 4
      * 	  of the control file (G/L CONTROL RECORD).
      *    *************************************************************
018330 DA90.
018360       MOVE 4              TO WS-NETKEY.
011300       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
018380	     MOVE W90-V8	 TO G-PL G-PL-MTH (WS-S7).
011360       PERFORM REWRITE-CONTROL-UNLOCK THRU WRITE-CONTROL-EXIT.
018430       GO TO DA999.

018440 DA95.
018450       PERFORM DA32.
018460	     MOVE 100		 TO STM-P1 STM-P2 STM-P3 STM-P4
018470				    STM-P5 STM-P6.
018480	     MOVE W90-V1	 TO STM-V1.
018490	     MOVE W90-V2	 TO STM-V2.
018500	     MOVE W90-V3	 TO STM-V3.
018510	     MOVE W90-V4	 TO STM-V4.
018520	     MOVE W90-V5	 TO STM-V5.
018530	     MOVE W90-V6	 TO STM-V6.
018540	     MOVE "TOTAL SALES"  TO STM-DESC.
	     PERFORM CALL-PRINTUTL.
018560	     MOVE SPACES	 TO R-DET.
018570       PERFORM DA32.
018580     IF G-GRP = 3
018590         GO TO DA55.
018600	     MOVE "*** COSTS ***"
				 TO STM-ANAME.
	     PERFORM CALL-PRINTUTL.
018620	     MOVE SPACES	 TO R-DET.
018630       GO TO DA60.

018640 DA100.
018650     IF WS-EXS = 0
018660         IF G-SR = ZERO
018670             GO TO DA112.
018680	     MOVE ZERO		 TO W20-DEBIT.
018690     IF WS-S7 < WS-F-YR
018700	       MOVE 1		 TO WS-S9
018710	       MOVE 12		 TO WS-S10
018720     ELSE
018730	       MOVE WS-S7	 TO WS-S10
018740	       MOVE WS-F-YR	 TO WS-S9
018750         GO TO DA110.

018760 DA105.
018770	     ADD G-BVAL (WS-S9)  TO W20-DEBIT.
018780     IF WS-S9 < WS-S7
018790	       ADD 1		 TO WS-S9
018800         GO TO DA105.
018810	     MOVE WS-F-YR	 TO WS-S9.

018820 DA110.
018830	     ADD G-BVAL (WS-S9)  TO W20-DEBIT.
018840     IF WS-S9 < WS-S10
018850	       ADD 1		 TO WS-S9
018860         GO TO DA110.
018870     IF W15-GRP = 1
018880	       MULTIPLY -1	 BY W20-DEBIT.
018890	     MOVE W20-DEBIT	 TO STM-V1.
018900	     MOVE G-VAL (WS-S7)  TO STM-V2.
018910       MOVE G-LVAL (WS-S7) TO STM-V3.
018920       MOVE G-BVAL (WS-S7) TO W20-DEBIT.
018930     IF W15-GRP = 1
018940	       MULTIPLY -1	 BY W20-DEBIT.
018950	     MOVE W20-DEBIT	 TO STM-V4.
018960	     MOVE G-VAL (WS-S7)  TO W20-DEBIT.
018970	     SUBTRACT G-VAL (WS-S8)
				 FROM W20-DEBIT.
018980	     MOVE W20-DEBIT	 TO STM-V5.
018990       MOVE G-LVAL (WS-S7) TO W20-DEBIT.
019000	     SUBTRACT G-LVAL (WS-S8)
				 FROM W20-DEBIT.
019010	     MOVE W20-DEBIT	 TO STM-V6.

019020 DA112.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 1		 TO WS-ADVANCE.

019040 DA115.
019050       EXIT.

019060 DA999.
019070       EXIT.
      /    *************************************************************
      * 		I N I T I A L I S E   P R O G R A M
      *    *************************************************************
033920 ZA000	     SECTION 90.
033930 ZA00.
033940       PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
033950	     DISPLAY CLR-SCREEN.
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
	     MOVE "GACCNT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GACCNT.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.

042170 ZA02.
034370	     MOVE 1		 TO WS-PARKEY.
034380       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
034390	     MOVE PAR-COMPANY	 TO W90-CNAME.
034400	     MOVE PAR-DMY	 TO W05-TODAY.
034410	     MOVE PAR-YMD	 TO W05-TYMD.
	     MOVE 6		 TO WS-PARKEY.
034380       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-UNPROF     TO W70-UNPROF.             
             MOVE PAR-BNKGL      TO W70-BNKGL.
	     MOVE 4		 TO WS-NETKEY.
034450       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
034510	     MOVE G-OPEN	 TO WS-OPEN.
034520	     ADD 1 G-YEAR-END	 GIVING WS-F-YR.
034530     IF WS-F-YR > 12
034540	       SUBTRACT 12	 FROM WS-F-YR.
034550	     MOVE G-YEAR-END	 TO WS-E-YR.
034560	     MOVE 1		 TO WS-S1.

034570 ZA05.
034620	     MOVE ZERO		 TO WS-ERROR.
034630     IF W70-BNKGL = ZERO
034640         DISPLAY (20, 20) "Parameters not Set up - ENTER"
034650                           WITH FOREGROUND-COLOR 14
034660         ACCEPT (20, 50) WS-OPTION WITH FOREGROUND-COLOR 15 AUTO
	     DISPLAY SPACES AT 2525
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
034700	     GO TO ZA999.
      *    *************************************************************
      *    ****   F I L E   L I M I T	E X C E E D E D
      * 	  (Config.sys Dos/Win/Windows 95/98/NT/2000)
      * 	  (Config.nt Windows XP)
      * 	  (System.ini Windows.me)
      *    *************************************************************
039780 ZA49.
039790	     DISPLAY "Too many files OPEN" AT 0708
039800                WITH FOREGROUND-COLOR 14.
039810	     DISPLAY "Check the FILES option in CONFIG.SYS (Dos,Windows
      -		     "3/95/98/NT/2000)" AT 0908.
	     DISPLAY "CONFIG.NT  (Windows XP)" AT 1034.
	     DISPLAY "SYSTEM.INI (Windows ME)" AT 1134.
039820	     DISPLAY "Press any key to continue" AT 1308.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     GO TO ZA205.

034710 ZA55.
034720       MOVE 1 TO WS-S1.
034730	     MOVE SPACES TO WS-MID-LNE.

034740 ZA60.
034750       MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
034760       MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
034770     IF WS-S1 < 80
034780         ADD 1 TO WS-S1
034790         GO TO ZA60.
034800       MOVE WS-G9  TO WS-TCH  (1).
034810       MOVE WS-G10 TO WS-TCH  (80).
034820       MOVE WS-G11 TO WS-BCH  (1).
034830       MOVE WS-G12 TO WS-BCH  (80).
034840       MOVE WS-G14 TO WS-TCHR (1)  WS-BCHR (1).
034850       MOVE WS-G13 TO WS-TCHR (80) WS-BCHR (80).
034860       MOVE WS-G2  TO WS-TCHR (11) WS-TCHR (45)
034870                      WS-TCHR (63).
034880       MOVE WS-G3  TO WS-MCHR (11) WS-MCHR (45)
034890                      WS-MCHR (63)
034900			    WS-MCHR (1)  WS-MCHR (80).
034920       MOVE WS-G4  TO WS-BCHR (11) WS-BCHR (45)
034930                      WS-BCHR (63).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.

034940 ZA200.
034950       DISPLAY "Files locked - Try later" AT 2312
034960                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
034970       DISPLAY "Press any key" AT 2512 
034980		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
034990	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
035000 ZA205.
035010       EXIT PROGRAM.

035020 ZA999.
035030       EXIT.
      /
035040 ZB000	     SECTION 91.
      
041480 OPEN-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041480 READ-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041520 WRITE-ERROR.
041530       DISPLAY CLR-SCREEN.
041540       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK TO WS-FILE
041630         MOVE WS-NETKEY TO WS-KEY
041640     ELSE
041810     IF WS-F-ERROR = 8
041820	       MOVE W02-GACCNT	 TO WS-FILE
041830	       MOVE ZERO	 TO WS-KEY
	       MOVE G-AC	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870	       MOVE WS-PARKEY TO WS-KEY.
036720     IF WS-STATUS = "10"
036730         MOVE "End of FILE" TO WS-STAT-MESSAGE
036740     ELSE
036750     IF WS-STATUS = "22"
036760         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
036770     ELSE
036780     IF WS-STATUS = "23"
036790         MOVE "Invalid record number" TO WS-STAT-MESSAGE
036800     ELSE
036810     IF WS-STATUS = "24"
036820         MOVE "DISK full" TO WS-STAT-MESSAGE
036830     ELSE
036840     IF WS-STATUS = "30"
036850         MOVE "DISK error" TO WS-STAT-MESSAGE
036860     ELSE
036870     IF WS-STATUS = "94"
036880         MOVE "FILE locked" TO WS-STAT-MESSAGE.
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
