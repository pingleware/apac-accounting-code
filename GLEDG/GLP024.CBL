      $set LINKCOUNT"320"
      ******************************************************************
      *                                                                *
      *    ******   **	      *******	  ****	   ******	**     *
      *   **	**  **	      **    **	 **  **   **	**     ***     *
      *   **	    **	      **    **	**    **       **     * **     *
      *   **	    **	      *******	**    **     **      *	**     *
      *   **   ***  **	      **	**    **   **	    *******    *
      *   **	**  **	      **	 **  **   **		**     *
      *    ******   ********  **	  ****	  ********     ****    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      * 	 G / L E D G E R   D E T A I L E D   L I S T	       *
      *                                                                *
      *     VERSION 8.14.02 - January 2010			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	       GLP024.
000030 AUTHOR.		       James William Lemmon.
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

000180 COPY GLTRAN.SL.

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000280 COPY PARAM.FDE.

000300 COPY GACCNT.FDE.

000320 COPY GLTRAN.FDE.

      /
004330 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
004340 77  WS-PARKEY	   PIC	9(04)	 COMP-5 VALUE 1.
004350 77  WS-GLKEY	   PIC	9(04)	 COMP-5 VALUE 1.
004360 77  WS-GTRKEY	   PIC	9(04)	 COMP-5 VALUE 1.
004370 77  WS-GINKEY	   PIC	9(04)	 COMP-5 VALUE 1.
004370 77  WS-NETKEY	   PIC	9(04)	 COMP-5 VALUE 1.
004380 77  WS-KSTORE	   PIC	9(04)	 COMP-5.
004390 77  WS-ISUB	   PIC	9(04)	 COMP-5.
004400 77  WS-CHEQUE	   PIC	9(04)	 COMP-5.
004410 77  WS-PAGE	   PIC	9(04)	 COMP-5.
004420 77  WS-LINES	   PIC	9(02) VALUE 66.
004430 77  WS-S1	   PIC	9(04)	 COMP-5.
004440 77  WS-S2	   PIC	9(04)	 COMP-5.
004450 77  WS-S3	   PIC	9(04)	 COMP-5.
004460 77  WS-S4	   PIC	9(04)	 COMP-5.
004470 77  WS-S5	   PIC	9(04)	 COMP-5.
004480 77  WS-S6	   PIC	9(04)	 COMP-5.
004490 77  WS-S7	   PIC	9(04)	 COMP-5.
004500 77  WS-S8	   PIC	9(04)	 COMP-5.
004510 77  WS-S9	   PIC	9(04)	 COMP-5.
004520 77  WS-S10	   PIC	9(04)	 COMP-5.
004530 77  WS-S11	   PIC	9(04)	 COMP-5.
004530 77  WS-IXS1	   PIC	9(04)	 COMP-5.
004540 77  WS-OPEN	   PIC	9(02)	 VALUE ZERO.
004550 77  WS-EXS	   PIC	9(01).
004560 77  WS-NUM          PIC  Z9.
004560 77  WS-SCH          PIC  X(01).
004560 77  WS-RECON        PIC  X(01).
004570 77  WS-ZERO         PIC  9(01).
004580 77  WS-MM	   PIC	9(02)	 COMP-5 VALUE ZERO.
004590 77  WS-F-YR	   PIC	9(02).
004600 77  WS-E-YR	   PIC	9(02).
004610 77  WS-OPTION	   PIC	X(01).
004630 77  WS-ERROR	   PIC	9(01)	 VALUE ZERO.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
004710 77  WS-TNEXT	   PIC	9(04)	 COMP-5.
003150 77  WS-PRN-NO	   PIC	X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC	X(01)	 COMP-X.
004730 77  WS-H1	   PIC	X(05)	 VALUE "DATE:".
004740 77  WS-H3	   PIC	X(06)	 VALUE "BATCH:".
004750 77  WS-H4	   PIC	X(05)	 VALUE "PAGE:".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04)	 COMP-5.
      /
004840 01  WS-DB-LINE.
004850     03  WS-TOP-LNE.
004860	       05  WS-TCHR     PIC X(01) OCCURS 80.
004870     03  WS-T-LINE REDEFINES WS-TOP-LNE.
004880	       05  FILLER      PIC X(01).
004890	       05  WS-H-LINE   PIC X(78).
004910	       05  FILLER      PIC X(01).
004920     03  WS-TOP-LNE2.
004930	       05  WS-TCH      PIC X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
004940	   03  WS-MID-LNE.
004950	       05  WS-MCHR     PIC X(01) OCCURS 80.
	   03  WS-MDGL-LNE REDEFINES WS-MID-LNE.
	       05  WS-MGL1     PIC X(01).
	       05  WS-MGL78    PIC X(78).
	       05  WS-MGL80    PIC X(01).
004960     03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
004980	   03  WS-BOT-LNE.
004990	       05  WS-BCHR     PIC X(01) OCCURS 80.
005000     03  WS-B-LINE REDEFINES WS-BOT-LNE.
005010	       05  FILLER      PIC X(01).
005020	       05  WS-F-LINE   PIC X(78).
005040	       05  FILLER      PIC X(01).
005050     03  WS-BOT-LNE2.
005060	       05  WS-BCH      PIC X(01) OCCURS 80.

005080 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "GLP".
	   03  WS-PROG	       PIC  X(03) VALUE "024".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

005210 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

000300 COPY GACCNT.ID.

000320 COPY GLTRAN.ID.

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
000740	       05  FILLER      PIC  X(29).
000750	       05  R-H2        PIC  X(40).
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
001690	       05  FILLER      PIC  X(36).
001700	       05  STM-H3      PIC  X(96).

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
001890	       05  STM-H11     PIC  X(11).
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
002050	       05  STM-H19     PIC  X(11).
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
002170	       05  STM-V2      PIC  Z(07)9.99- BLANK WHEN ZERO.
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
002360	       05  STM-V5      PIC  Z(07)9.99- BLANK WHEN ZERO.
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

       01  W10-DATE.
	   03  W10-DTE	       PIC 99/99/9999.

       COPY W15.GL.

       01  W16-DATES.
004120	   03  W16-START       PIC 9(08).
004130	   03  W16-SDTE-DMY REDEFINES W16-START.
004140	       05  W16-SDAY    PIC 9(02).
004150	       05  W16-SMNTH   PIC 9(02).
004160	       05  W16-SCENT   PIC 9(02).
004160	       05  W16-SYEAR   PIC 9(02).
004170	   03  W16-S-YMD       PIC 9(08).
004180	   03  W16-STRT-YMD REDEFINES W16-S-YMD.
004190	       05  W16-SCC     PIC 9(02).
004190	       05  W16-SYY     PIC 9(02).
004200	       05  W16-SMM     PIC 9(02).
004210	       05  W16-SDD     PIC 9(02).
004120	   03  W16-END	       PIC 9(08).
004130	   03  W16-EDTE-DMY REDEFINES W16-END.
004140	       05  W16-EDAY    PIC 9(02).
004150	       05  W16-EMNTH   PIC 9(02).
004160	       05  W16-ECENT   PIC 9(02).
004160	       05  W16-EYEAR   PIC 9(02).
004170	   03  W16-E-YMD       PIC 9(08).
004180	   03  W16-END-YMD REDEFINES W16-E-YMD.
004190	       05  W16-ECC     PIC 9(02).
004190	       05  W16-EYY     PIC 9(02).
004200	       05  W16-EMM     PIC 9(02).
004210	       05  W16-EDD     PIC 9(02).

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
	       05  W25-KEY     PIC 9(04).
	       05  W25-SUB     PIC 9(01).
	       05  FILLER      PIC 9(02).

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
000570	       05  FILLER      PIC  X(36)    VALUE
000580			       "JANUARY  FEBRUARY MARCH    APRIL    ".
000570	       05  FILLER      PIC  X(36)    VALUE
000580			       "MAY      JUNE     JULY     AUGUST   ".
000570	       05  FILLER      PIC  X(36)    VALUE
000580			       "SEPTEMBEROCTOBER  NOVEMBER DECEMBER ".
000590	   03  W30-MONTH REDEFINES W30-MONTH-NAMES
000600			       PIC  X(09)  OCCURS 12.

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
007080			      OCCURS 18.

007090 01  W80-HEAD.
	   03  FILLER	       PIC  X(55) VALUE
	     "GENERAL LEDGER - DETAILED TRANSACTION LIST PER ACCOUNT".
           03  FILLER          PIC X(17) VALUE "For the period : ".
	   03  W80-DATE1       PIC Z9/99/9999.
           03  FILLER          PIC X(04) VALUE " to ".
	   03  W80-DATE2       PIC Z9/99/9999.

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

       01  S07.
	   03  LINE 06 COLUMN 25 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W16-START AUTO.
	   03  LINE 07 COLUMN 25 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W16-END AUTO.

008300 01  S17.
008310	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC 9(06) USING W15-ACCOUNT AUTO.

       COPY LEDLUP.CRT.

004870 COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
008530 AA000	       SECTION.
008540 AA00.
003240	   IF LS0-GLLEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA35.
008560	     PERFORM ZA000.
	     PERFORM EA000.

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

      /    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      * 	   S C R E E N ,   K E Y B O A R D   &	 M O U S E
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3      ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³			  SCREEN-SHADOW 		       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To routine is used to display a shadow down the right and ³
      *    ³ along the bottom of a pop-up box. The parameters that are ³
      *    ³ required:						       ³
      *	   ³	       SHADE-ROW   - Top line of the box + 1.	       ³
      *	   ³	       SHADE-COL   - Left line of box + 2.	       ³
      *	   ³	       SHADE-WIDTH - Width of the box - 2.	       ³
      *	   ³	       SHADE-LINES - Hight of box - 1.		       ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³			  CHECK-CORRECT 		       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine displays a pop-up window with the message    ³
      *    ³	      "Correct ENTER - N if incorrect"		       ³
      *    ³							       ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³							       ³
      *    ³ To start with the pop-up window higher or lower than row  ³
      *    ³ 13 (default); move another value to SLIN and PERFORM      ³
      *    ³ CHECK-SETUP THRU CHECK-EXIT.			       ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³			  ERROR-MESSAGE 		       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To display an error message with " - Press ANY key" at    ³
      *    ³ the end; use PERFORM ERROR-MESSAGE.		       ³
      *    ³							       ³
      *    ³ To display just the error message; use PERFORM	       ³
      *    ³ ERROR-LENGTH THRU ERROR-EXIT.			       ³
      *    ³							       ³
      *    ³ To display the error message higher or lower (default is  ³
      *    ³ line 13) firstly use PERFORM ERROR-SETUP THRU ERROR-POS   ³
      *    ³ or PERFORM ERROR-LENGTH THRU ERROR-POS. Move the line     ³
      *    ³ number to be used to SLIN and then PERFORM ERROR-DISPLAY  ³
      *    ³ THRU ERROR-EXIT.					       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY FUNCTION.CRT.

      *
      *   D A T E   C H E C K   R O U T I N E
      *
      *   PERFORM THIS ROUTINE WITH THE DATE IN W10-DATE (PIC 99/99/9999)
      *   WITH THE FORMAT AS DD/MM/CCYY
      *
      *   ON EXITING - WS-ERROR WILL BE ZERO (0) IF THE DATE IS CORRECT
      *                OR ONE (1) IF THE DATE IS INCORRECT.
      *
008380 CHECK-DATE         SECTION.
      
008390 DATE-CHECK.
008400       UNSTRING W10-DATE DELIMITED BY "/" OR " "
008410	       INTO W20-DD, W20-MM, W20-CCYY
008420         OVERFLOW GO TO DATE-ERROR.
008430       MOVE ZERO           TO WS-ERROR.
008440     IF W20-DATE NOT NUMERIC
008450         GO TO DATE-ERROR.
008470     IF W20-DAY < 01 OR > 31
008480         GO TO DATE-ERROR.
008490     IF W20-MONTH < 01 OR > 12
008500         GO TO DATE-ERROR.
008510     IF W20-MONTH = 01 OR 03 OR 05 OR 07 OR 08 OR 10 OR 12
008520         GO TO CHECK-DATE-EXIT.
008530     IF W20-MONTH = 02 AND W20-DAY > 28
008540         GO TO LEAP-YEAR.
008550     IF W20-DAY > 30
008560         GO TO DATE-ERROR.
008570       GO TO CHECK-DATE-EXIT.
      
008580 LEAP-YEAR.
008590     IF W20-DAY > 29
008600         GO TO DATE-ERROR.
008610	     DIVIDE W20-CNYR BY 4 GIVING W25-RESULT.
008620     IF W25-DECIMAL = 0
008630         GO TO CHECK-DATE-EXIT.
      
008640 DATE-ERROR.
008650       MOVE 1              TO WS-ERROR.
      
008660 CHECK-DATE-EXIT.
008670       EXIT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP6.

009320 AA350-LOOKUP    SECTION.
009540 AA350.
011050	     PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
	     PERFORM GACCNT-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.
009800 AA399.
009810       EXIT.

009820 AB000           SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.

       COPY GACCNT.RD1.

       COPY GLTRAN.RD.

      /
010460 AC000           SECTION.

       COPY CONTROL.WR.

       COPY GACCNT.LUP.

       COPY APAC.HLP.

      /
019080 EA000	     SECTION 5.
019090 EA00.
019100	     DISPLAY CLR-SCREEN.
019110       DISPLAY "LISTING OF G/L ACCOUNT/S IN DETAIL" AT 0224
019120                WITH FOREGROUND-COLOR 15.
	     MOVE 01		 TO W16-SDAY.
	     MOVE WS-OPEN	 TO W16-SMNTH.
	     MOVE W05-CC	 TO W16-SCENT.
	     MOVE W05-YY	 TO W16-SYEAR.
	   IF W05-MM < WS-OPEN
	       SUBTRACT 1	 FROM W16-SYEAR.
	     MOVE W05-TODAY	 TO W16-END.
	     DISPLAY "For the period." AT 0412.
	     DISPLAY "Start date :" AT 0612.
	     DISPLAY "End date   :" AT 0712.
	     DISPLAY "Dates to be keyed in as DDMMCCYY" AT 0912
                      WITH FOREGROUND-COLOR 11.

       EA02.
             ACCEPT S07.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE 9	 TO WS-ERROR
		     GO TO EA03
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO EA02
		 WHEN OTHER
		     CALL X"E5" CALL X"E5" CALL X"E5"
		     GO TO EA02
	       END-EVALUATE.
	     PERFORM CHECK-CORRECT.
018520	   IF WS-OPTION = "N"
018530	       GO TO EA02.
	     MOVE W16-START	 TO W10-DTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "Start date" TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
	       GO TO EA02.
	     MOVE W20-DTE	 TO W16-S-YMD W15-YMD.
	     MOVE 01		 TO W15-DY.
	     MOVE W16-END	 TO W10-DTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "End date"	 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
	       GO TO EA02.
	     MOVE W20-DTE	 TO W16-E-YMD.
	     MOVE W16-START	 TO W80-DATE1.
	     MOVE W16-END	 TO W80-DATE2.
	     MOVE 4		 TO WS-NETKEY.
034450       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
019250	     SUBTRACT 1 FROM W16-SMM
				 GIVING WS-S10.
019280     IF WS-S10 = ZERO
019290	       MOVE 12		 TO WS-S10.

       EA03.
	     EXIT.

       EA04.
	     PERFORM SAVE-SCREEN.
019300	     MOVE ZERO		 TO WS-PAGE.
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 3		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.

019330 EA05.
019340       PERFORM AA50 THRU AA55.
019350	     MOVE W80-HEAD	 TO STM-H3.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
019370	     MOVE ALL "-"	 TO STM-H3.
	     PERFORM CALL-PRINTUTL.
019390	     MOVE SPACES	 TO R-DET.
019400       MOVE "ACNT NO"      TO LED-H1.
019410       MOVE "ACCOUNT NAME" TO LED-H2.
019420       MOVE "DATE"         TO LED-H3.
019430       MOVE "CODE"         TO LED-H4.
             MOVE "BATCH"        TO LED-H4A.
019440       MOVE "REFER"        TO LED-H5.
019450       MOVE "PAYEE/NARRATIVE"
                                 TO LED-H6.
019460       MOVE "VALUE"        TO LED-H7.
019470       MOVE "BALANCE"      TO LED-H8.
	     PERFORM CALL-PRINTUTL.
019490	     MOVE ALL "-"	 TO LED-H1 LED-H2  LED-H3
                                    LED-H4 LED-H4A LED-H5 
019500                              LED-H6 LED-H7  LED-H8.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
019520	     MOVE SPACES	 TO R-DET.
	     MOVE "D"		 TO W02-PRN-TYPE.

019530 EA10.
	     PERFORM RESTORE-SCREEN.
019840	     DISPLAY WS-BLNK78 AT 1102 WITH FOREGROUND-COLOR 3.
	     DISPLAY "Account from:" AT 1112.
	     DISPLAY "      " AT 1126
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5.
	     DISPLAY "to:" AT 1222.
	     DISPLAY "	    " AT 1226
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5.
             MOVE 100000         TO W15-ACCOUNT.
             MOVE 999999         TO W15-ENDACC.

019720 EA12.
019730	     DISPLAY "F1" AT 2502
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "=Help," WITH FOREGROUND-COLOR 1
				  BACKGROUND-COLOR 3
		     "F2" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
019740		     " to do Account Lookup"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
019750	     ACCEPT S17 AT 1126.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE ZERO	 TO W15-ACCOUNT
		     GO TO EA50
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO EA12
		 WHEN F2-KEY
		     PERFORM AA350
		 WHEN OTHER
		     CALL X"E5"
		     GO TO EA12
	       END-EVALUATE
019780	       DISPLAY S17 AT 1126
019790         IF W15-ACCOUNT = ZERO
019800             GO TO EA12.
019810	     DISPLAY CLEAR-L25.
019820     IF W15-ACCOUNT = ZERO
019830	       GO TO EA50.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		 Store the starting account number.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE W15-ACCOUNT	 TO W15-BEGACC.
           IF W15-BEGACC < 100000
               MOVE 100000       TO W15-BEGACC.
	     MOVE W15-BEGACC	 TO G-AC.
	     PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
	   IF WS-F-ERROR = ZERO
	       DISPLAY G-NAME AT 1133
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
019840	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.

       EA13.
             MOVE W15-ENDACC     TO W15-ACCOUNT
019730	     DISPLAY "F1" AT 2502
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "=Help," WITH FOREGROUND-COLOR 1
				  BACKGROUND-COLOR 3
		     "F2" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
019740		     " to do Account Lookup"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
019750	     ACCEPT S17 AT 1226.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE W15-BEGACC
				 TO W15-ACCOUNT
		     GO TO EA12
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO EA13
		 WHEN F2-KEY
		     PERFORM AA350
		 WHEN OTHER
		     CALL X"E5"
		     GO TO EA13
	       END-EVALUATE
019780	       DISPLAY S17 AT 1226
019790         IF W15-ACCOUNT = ZERO
                   MOVE W15-BEGACC   TO W15-ENDACC
019800             GO TO EA13.
             MOVE W15-ACCOUNT    TO W15-ENDACC.
	     MOVE W15-ENDACC	 TO G-AC.
	     PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
	   IF WS-F-ERROR = ZERO
	       DISPLAY G-NAME AT 1233
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
019810	     DISPLAY CLEAR-L25.
019820     IF (W15-ENDACC = ZERO) OR
              (W15-ENDACC < W15-ACCOUNT)
               MOVE W15-ACCOUNT  TO W15-ENDACC
019830         GO TO EA13.
             MOVE W15-BEGACC     TO W15-ACCOUNT.
           IF W15-ACCOUNT = W15-ENDACC
019850         IF W15-SER = ZERO
019860		    MOVE "Group Heading"
				 TO WS-ERR-STRING
003260		    PERFORM ERROR-MESSAGE
019890              GO TO EA12.
           IF (W15-ACCOUNT NOT = 100000) OR
              (W15-ENDACC NOT = 999999)
               MOVE 1            TO WS-EXS
           ELSE
               MOVE ZERO         TO WS-EXS.
	     PERFORM SAVE-SCREEN.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       MOVE W15-BEGACC	 TO W15-ACCOUNT
	       GO TO EA12.
	   IF W15-ACCOUNT > 100000
019900	       MOVE W15-ACCOUNT  TO G-AC
	       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT
019910	       IF WS-F-ERROR = 8
019920		   MOVE "No Record"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
019930             GO TO EA12
               END-IF
	       GO TO EA18.
	     MOVE 100000	 TO G-AC.
	     PERFORM START-AT-LEDG-AC THRU READ-GACCNT-EXIT.
	   IF WS-F-ERROR = 8
	       GO TO EA50.

019990 EA15.
	     PERFORM READ-GACCNT-NEXT THRU READ-GACCNT-EXIT.
020000	   IF WS-F-ERROR = 8
020670         IF (WS-EXS = ZERO) OR
                  (W15-ACCOUNT = 100000)
020010             GO TO EA50
               ELSE
                   GO TO EA10.
       EA18.
	   IF NOT (WS-PRINTER = "S")
	       DISPLAY "Processing:" AT 1412
	       DISPLAY G-AC AT 1424
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5
	       DISPLAY G-NAME AT 1431
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5
	       DISPLAY "Transaction:" AT 1612.
           IF G-AC > W15-ENDACC
	       PERFORM RESTORE-SCREEN
	       PERFORM EA02 THRU EA03
	       IF WS-ERROR = 9
		   GO TO EA50
	       END-IF
	       GO TO EA10.

020020 EA20.
020030     IF G-ASER = ZERO
020040         GO TO EA45.
           IF G-BAL NOT = ZERO
               GO TO EA22.
             MOVE G-AC           TO T-AC.
	     MOVE W15-YMD	 TO T-DATE.
             PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
           IF WS-F-ERROR = 11
               GO TO EA45.              
020210       PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
020190     IF (WS-F-ERROR = 11) OR 
	      (G-AC NOT = T-AC) OR
	      (T-DATE > W16-E-YMD)
020200         GO TO EA45.
020080 EA22.
020090	     MOVE G-AC		 TO LED-AC.
020100	     MOVE G-NAME	 TO LED-NME.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
				 TO R-DET
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       GO TO EA55.
020120	     MOVE SPACES	 TO R-DET.
020130	     MOVE "BALANCE B/FWD"
				 TO LED-NAR.
	   IF WS-S10 = WS-E-YR
	       IF (G-OBAL NOT > 0.00) AND
		  (G-OBAL NOT < 0.00)
		   MOVE ZERO	 TO W20-TOTAL
	       ELSE
020140		   MOVE G-OBAL	 TO W20-TOTAL
	   ELSE
020140	       MOVE G-VAL (WS-S10)
				 TO W20-TOTAL.
             INITIALIZE T-REC.
	     MOVE G-AC		 TO T-AC.
	     MOVE W15-YMD	 TO T-DATE.
             PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
           IF WS-F-ERROR = 11
               GO TO EA35.              

       EA24.
020210	     PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
020190     IF (WS-F-ERROR = 11) OR 
	      (G-AC NOT = T-AC) OR
	      (T-DATE > W16-E-YMD)
	       MOVE W20-TOTAL	 TO LED-BAL
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-DET
020200         GO TO EA35.
020260     IF T-SUB = 5
020270	       GO TO EA24.
	   IF NOT (WS-PRINTER = "S")
	       DISPLAY T-KEY AT 1625
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
	   IF T-DATE < W16-S-YMD
	       ADD T-VAL	 TO W20-TOTAL
	       GO TO EA24.
020400       MOVE W20-TOTAL      TO LED-BAL.
	     PERFORM CALL-PRINTUTL.
020160	     MOVE SPACES	 TO R-DET.
	     GO TO EA26.

020180 EA25.
020210       PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
020190     IF (WS-F-ERROR = 11) OR 
	      (G-AC NOT = T-AC) OR
	      (T-DATE > W16-E-YMD)
020200	       GO TO EA35.
020260     IF T-SUB = 5
020270         GO TO EA30.
	     DISPLAY T-KEY AT 1625
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

       EA26.
020280	     MOVE T-DATE	 TO W15-SYMD.
	     MOVE W15-SCN	 TO W15-CC.
	     MOVE W15-SYR	 TO W15-YY.
	     MOVE W15-SMT	 TO W15-MM.
	     MOVE W15-SDY	 TO W15-DD.
020280       MOVE W15-DATE       TO LED-DTE.
020290     IF T-CODE = 01 OR 02
020300         MOVE "JNL"        TO LED-CDE
020310     ELSE
020320     IF T-CODE = 03 OR 04
020330	       MOVE "CHEQ"	 TO LED-CDE
020310	   ELSE
020320	   IF T-CODE = 05 OR 10
020330	       MOVE "BANK"	 TO LED-CDE
020340	   ELSE
020350     IF T-CODE = 06 OR 07
020360	       MOVE "DEP"	 TO LED-CDE
020340     ELSE
020350	   IF T-CODE = 08 OR 09
020360	       MOVE "EFT"	 TO LED-CDE
020340     ELSE
020350	   IF T-CODE = 11 OR 12
020360	       MOVE "PMT"	 TO LED-CDE
020340     ELSE
020350	   IF T-CODE = 13 OR 14
020360	       MOVE "RCPT"	 TO LED-CDE.
             MOVE T-BATCH        TO LED-BTCH.
	   IF T-REFER NUMERIC
020370	       MOVE T-REF	 TO LED-REF
	   ELSE
	       INSPECT T-REFER REPLACING ALL X"00" BY SPACE
	       MOVE T-REFER	 TO LED-REFER.
020380       MOVE T-VAL          TO LED-VAL.
020390       ADD T-VAL           TO W20-TOTAL.
020400       MOVE W20-TOTAL      TO LED-BAL.
020410 EA30.
020420       MOVE T-NARATIVE     TO LED-NAR.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-DET
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
020470	       PERFORM EA05
	       MOVE SPACES	 TO R-DET
020090	       MOVE G-AC	 TO LED-AC
020100	       MOVE G-NAME	 TO LED-NME.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
				 TO R-DET
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       GO TO EA55.
020500	     GO TO EA25.

020510 EA35.
	     DISPLAY WS-BLNK78 AT 0902 WITH FOREGROUND-COLOR 3.
	     DISPLAY WS-BLNK78 AT 1102 WITH FOREGROUND-COLOR 3.
020560	     MOVE "BALANCE C/FWD"
				 TO LED-NAR.
020520	   IF W16-E-YMD = W05-TYMD
020540	       MOVE G-BAL	 TO LED-BAL
020550     ELSE
020570	       MOVE W20-TOTAL	 TO LED-BAL.
020580	   IF W02-LINAGE > (W02-PRN-LENGTH - 6)
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
020600	       PERFORM EA05
020610         GO TO EA45.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
020630	     MOVE ALL "-"	 TO LED-TRAN.
	     PERFORM CALL-PRINTUTL.
020650	     MOVE SPACES	 TO R-DET.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
				 TO R-DET
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       GO TO EA55.

020660 EA45.
           IF W15-ACCOUNT = W15-ENDACC
	       PERFORM RESTORE-SCREEN
	       PERFORM EA02 THRU EA03
	       IF WS-ERROR = 9
		   GO TO EA50
	       END-IF
020690	       GO TO EA10.
020710       GO TO EA15.
020720 EA50.
020730	     MOVE "*****   E N D   R E P O R T   *****"
				 TO REC-NME.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

       EA55.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

020770 EA999.
020780       EXIT.
      /
033920 ZA000	     SECTION 9.
033930 ZA00.
033940       PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
033950	     DISPLAY CLR-SCREEN.
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
	     MOVE "GACCNT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GACCNT.
	     MOVE "GLTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GLTRAN.
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
	   IF NOT (G-OPEN = ZERO)
034510	       MOVE G-OPEN	 TO WS-OPEN
	   ELSE
	       MOVE 3		 TO WS-OPEN.
034520       ADD 1 G-YEAR-END GIVING WS-F-YR.
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
034670	       MOVE 1		 TO WS-ERROR.
	     DISPLAY SPACES AT 2525
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
034700	     GO TO ZA999.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
034710 ZA55.
034720       MOVE 1 TO WS-S1.
034730	     MOVE SPACES	 TO WS-MID-LNE.
034740 ZA60.
034750	     MOVE WS-G1 	 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
034760	     MOVE WS-G8 	 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
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
035040 ZB000	     SECTION 9.
      
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
041620	       MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041640     ELSE
041810     IF WS-F-ERROR = 8
041820	       MOVE W02-GACCNT	 TO WS-FILE
041830	       MOVE ZERO	 TO WS-KEY
	       MOVE G-AC	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 11
041860	       MOVE W02-GLTRAN	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
               MOVE T-KEY        TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY.
036720     IF WS-STATUS = "10"
036730         MOVE "End of FILE" TO WS-STAT-MESSAGE
036740     ELSE
036750     IF WS-STATUS = "22"
036760         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
036770     ELSE
036780     IF WS-STATUS = "23"
036790	       MOVE "Invalid record number"
				 TO WS-STAT-MESSAGE
036800     ELSE
036810     IF WS-STATUS = "24"
036820	       MOVE "DISK full"  TO WS-STAT-MESSAGE
036830     ELSE
036840     IF WS-STATUS = "30"
036850         MOVE "DISK error" TO WS-STAT-MESSAGE
036860     ELSE
036870     IF WS-STATUS = "94"
036880	       MOVE "FILE locked"
				 TO WS-STAT-MESSAGE.
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
