      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   **	      *******	  ****	    ****     ******    *
      *   **	**  **	      **    **	 **  **    **  **   **	  **   *
      *   **	    **	      **    **	**    **  **	**	  **   *
      *   **	    **	      *******	**    **  **	**     ****    *
      *   **   ***  **	      **	**    **  **	**	  **   *
      *   **	**  **	      **	 **  **    **  **   **	  **   *
      *    ******   ********  **	  ****	    ****     ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     G / L E D G E R   B U D G E T T I N G		       *
      *                                                                *
      *     VERSION 8.15.00 - November 2010			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	       GLP003.
000030 AUTHOR.                 J.W. LEMMON (APAC).
000040 DATE-WRITTEN.           JUNE 1983.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2011
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

000220 COPY GLJRN.SL.

       COPY LEDTRN.SL.

000240 COPY RECOVER.SL.

       COPY SHARED.SL.

       COPY TXTRAN.SL.

000270 DATA DIVISION.
000280 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000300 COPY PARAM.FDE.

000320 COPY GACCNT.FDE.

000340 COPY GLTRAN.FDE.

000380 COPY GLJRN.FD.

       COPY LEDTRN.FDE.

000400 COPY RECOVER.GFD.

       COPY SHARED.FDE.

       COPY TXTRAN.FDE.

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001700 77  WS-PARKEY	   PIC	9(04)	 COMP-5 VALUE 1.
001690 77  WS-LTRKEY	   PIC	9(06)	 COMP-5.
001690 77  WS-ENQPOS       PIC  9(06)    COMP-5.
001700 77  WS-ENQEND       PIC  9(06)    COMP-5.
001730 77  WS-NETKEY       PIC  9(04)    COMP-5 VALUE 1.
001740 77  WS-KEYSTR       PIC  9(06)    COMP-5.
001750 77  WS-KSTORE       PIC  9(06)    COMP-5.
001760 77  WS-SKSTORE      PIC  9(06)    COMP-5.
001770 77  WS-INKEY        PIC  9(06)    COMP-5.
001780 77  WS-COKEY        PIC  9(06)    COMP-5.
001790 77  WS-GLJSTR       PIC  9(04)    COMP-5.
001800 77  WS-GLJKEY       PIC  9(06)    COMP-5.
001810 77  WS-RECKEY       PIC  9(04)    COMP-5.
001820 77  WS-RECOVER      PIC  9(04)    COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
001830 77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
001840 77  WS-ISUB         PIC  9(04)    COMP-5.
001850 77  WS-CHEQUE       PIC  9(04)    COMP-5.
001860 77  WS-PAGE         PIC  9(04)    COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
001880 77  WS-SUB	   PIC	9(04)	 COMP-5.
001880 77  WS-IXS1	   PIC	9(04)	 COMP-5.
001880 77  WS-S            PIC  9(04)    COMP-5.
001880 77  WS-S1           PIC  9(04)    COMP-5.
001890 77  WS-S2           PIC  9(04)    COMP-5.
001890 77  WS-S3           PIC  9(04)    COMP-5.
001890 77  WS-S4	   PIC	9(04)	 COMP-5.
001890 77  WS-S5	   PIC	9(04)	 COMP-5.
001890 77  WS-S6           PIC  9(04)    COMP-5.
001890 77  WS-S7	   PIC	9(04)	 COMP-5.
001890 77  WS-S8	   PIC	9(04)	 COMP-5.
001890 77  WS-S9	   PIC	9(04)	  COMP-5.
001900 77  WS-Y-END        PIC  9(02) VALUE ZERO.
001900 77  WS-L-END	   PIC	9(04) VALUE ZERO.
001900 77  WS-OPEN         PIC  9(02) VALUE ZERO.
001910 77  WS-MS1          PIC  9(04)    COMP-5.
001920 77  WS-MS2          PIC  9(04)    COMP-5.
001930 77  WS-VALUE        PIC S9(09)V99 COMP-3.
001940 77  WS-EXS          PIC  9(01).
001950 77  WS-CONREC       PIC  9(01) VALUE ZERO.
001960 77  WS-INDG         PIC  9(01) VALUE ZERO.
001970 77  WS-OPTION       PIC  X(01).
001970 77  WS-SKIP         PIC  X(01) VALUE "N".
001980 77  WS-FIRST        PIC  9(02) VALUE ZERO.
001990 77  WS-LAST         PIC  9(02) VALUE ZERO.
002010 77  WS-ERROR        PIC  9(01) VALUE ZERO.
002020 77  WS-NUM          PIC  Z9.
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
002070 77  WS-BATCH	   PIC	9(05)	 COMP-3.
002080 77  WS-TNEXT        PIC  9(04)    COMP-5.
002080 77  WS-PERIOD       PIC  9(02)    COMP-5 VALUE ZERO.
003150 77  WS-PRN-NO	   PIC	X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC	X(01)	 COMP-X.
002100 77  WS-H1	   PIC	X(05) VALUE "DATE:".
002110 77  WS-H3           PIC  X(06) VALUE "BATCH:".
002120 77  WS-H4           PIC  X(05) VALUE "PAGE:".
002150 77  WS-ER1          PIC  X(07) VALUE "Account".
002160 77  WS-ER3          PIC  X(04) VALUE "Date".
002170 77  WS-ER4          PIC  X(05) VALUE "Value".
002180 77  WS-ER5          PIC  X(13) VALUE "Cheque Number".
002190 77  WS-ER6          PIC  X(09) VALUE "No Record".
002200 77  WS-ER7          PIC  X(13) VALUE "Group Heading".
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
	   03  WS-MDGL-LNE REDEFINES WS-MID-LNE.
	       05  WS-MGL1     PIC X(01).
	       05  WS-MGL78    PIC X(78).
	       05  WS-MGL80    PIC X(01).
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

002220 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "GLP".
	   03  WS-PROG	       PIC  X(03) VALUE "003".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002330 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

000320 COPY GACCNT.ID.

000340 COPY GLTRAN.ID.

000380 COPY GLJRN.ID.

       COPY LEDTRN.ID.

       COPY PARAM.ID.

000400 COPY RECOVER.ID.

       COPY SHARED.ID.

       COPY TXTRAN.ID.

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

000440	   03  R-L1 REDEFINES W02-PRN-LINE.
000450	       05  R-DET       PIC  X(132).

000440	   03  R-SL1 REDEFINES W02-PRN-LINE.
	       05  R-1ST       PIC  X(78).
	       05  FILLER      PIC  X(56).

000440	   03  R-SL2 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(56).
	       05  R-2ND       PIC  X(78).

000460	   03  R-L2 REDEFINES W02-PRN-LINE.
000470	       05  R-H1	       PIC  X(06).
000480	       05  R-DTE1      PIC  Z9/99/9999.
000490	       05  FILLER      PIC  X(23).
000500	       05  R-H2	       PIC  X(40).
000510	       05  FILLER      PIC  X(15).
000520	       05  R-H3	       PIC  X(07).
000530	       05  R-BTCH      PIC  ZZZZ9.
000540	       05  FILLER      PIC  X(17).
000550	       05  R-H4	       PIC  X(06).
000560	       05  R-PGE       PIC  ZZ9.

000570	   03  R-L3 REDEFINES W02-PRN-LINE.
000580	       05  FILLER      PIC  X(03).
000590	       05  R-H5	       PIC  X(11).
000600	       05  R-H6	       PIC  X(05).
000610	       05  FILLER      PIC  X(01).
000620	       05  R-H7	       PIC  X(13).
000630	       05  R-H8	       PIC  X(05).
000640	       05  FILLER      PIC  X(06).
000650	       05  R-H9	       PIC  X(09).
000660	       05  R-H10       PIC  X(12).
000670	       05  R-H11       PIC  X(13).
000680	       05  FILLER      PIC  X(01).
000690	       05  R-H12       PIC  X(09).
000700	       05  R-H13       PIC  X(13).
000710	       05  FILLER      PIC  X(08).
000720	       05  R-H14       PIC  X(14).
000730	       05  R-H15       PIC  X(09).

000740	   03  R-L4 REDEFINES W02-PRN-LINE.
000750	       05  FILLER      PIC  X(03).
000760	       05  R-ACC       PIC  9(04)/99.
000770	       05  FILLER      PIC  X(04).
000780	       05  R-ANME      PIC  X(24).
000790	       05  FILLER      PIC  X(03).
000800	       05  R-DTE2      PIC  Z9/99/9999.
000810	       05  FILLER      PIC  X(03).
000820	       05  R-REF       PIC  Z(07).
000830	       05  FILLER      PIC  X(04).
000840	       05  R-NAR       PIC  X(36).
000850	       05  FILLER      PIC  X(02).
000860	       05  R-DBT       PIC  Z(08)9.99.
000870	       05  FILLER      PIC  X(02).
000880	       05  R-CRD       PIC  Z(08)9.99-.
000890	       05  FILLER      PIC  X(02).

000900	   03  R-L5 REDEFINES W02-PRN-LINE.
000910	       05  FILLER      PIC  X(65).
000920	       05  R-H16       PIC  X(10).
000930	       05  FILLER      PIC  X(01).
000940	       05  R-H17       PIC  X(15).
000950	       05  R-H18       PIC  X(10).
000960	       05  FILLER      PIC  X(31).

000970	   03  R-L6 REDEFINES W02-PRN-LINE.
000980	       05  FILLER      PIC  X(08).
000990	       05  R-H19       PIC  X(11).
001000	       05  R-H20       PIC  X(05).
001010	       05  FILLER      PIC  X(01).
001020	       05  R-H21       PIC  X(13).
001030	       05  R-H22       PIC  X(05).
001040	       05  FILLER      PIC  X(04).
001050	       05  R-H23       PIC  X(06).
001060	       05  FILLER      PIC  X(01).
001070	       05  R-H24       PIC  X(12).
001080	       05  R-H25       PIC  X(05).
001090	       05  FILLER      PIC  X(06).
001100	       05  R-H26       PIC  X(14).
001110	       05  R-H27       PIC  X(10).
001120	       05  R-H28       PIC  X(15).
001130	       05  R-H29       PIC  X(16).

001140	   03  R-L7 REDEFINES W02-PRN-LINE.
001150	       05  FILLER      PIC  X(08).
001160	       05  R-DAC       PIC  9(04)/99.
001170	       05  FILLER      PIC  X(04).
001180	       05  R-ANAME     PIC  X(24).
001190	       05  FILLER      PIC  X(04).
001200	       05  R-BRANCH    PIC  X(24).
001210	       05  FILLER      PIC  X(02).
001220	       05  R-DTE3      PIC  Z9/99/9999.
001230	       05  FILLER      PIC  X(02).
001240	       05  R-DEBT      PIC  Z(08)9.99.
001250	       05  FILLER      PIC  X(04).
001260	       05  R-BNKAC     PIC  9(04)/99.
001270	       05  FILLER      PIC  X(02).
001280	       05  R-CRED      PIC  Z(08)9.99-.
001290	       05  FILLER      PIC  X(09).

001300	   03  R-BL2 REDEFINES W02-PRN-LINE.
001310	       05  FILLER      PIC  X(20).
001320	       05  R-BH1       PIC  X(40).
001330	       05  R-BH2       PIC  X(30).
001340	       05  R-BH3       PIC  X(42).

001350	   03  R-BL3 REDEFINES W02-PRN-LINE.
001360	       05  R-BH4.
001370		   07  R-BAC   PIC  9(06).
001380		   07  FILLER  PIC  X(01).
001390		   07  R-BDESC PIC  X(30).
001400	       05  FILLER      PIC  X(95).

001410	   03  R-BL3A REDEFINES W02-PRN-LINE.
001420	       05  FILLER      PIC  X(06).
001430	       05  R-BH5       PIC  X(10).
001440	       05  R-BH6       PIC  X(10).
001450	       05  R-BH7       PIC  X(10).
001460	       05  R-BH8       PIC  X(10).
001470	       05  R-BH9       PIC  X(10).
001480	       05  R-BH10      PIC  X(10).
001490	       05  R-BH11      PIC  X(10).
001500	       05  R-BH12      PIC  X(10).
001510	       05  R-BH13      PIC  X(10).
001520	       05  R-BH14      PIC  X(10).
001530	       05  R-BH15      PIC  X(10).
001540	       05  R-BH16      PIC  X(10).
001550	       05  R-BH17      PIC  X(06).

001560	   03  R-BL4 REDEFINES W02-PRN-LINE.
001570	       05  FILLER      PIC  X(02).
001580	       05  R-BH18      PIC  X(08).
001590	       05  R-BH19      PIC  X(18).
001600	       05  FILLER      PIC  X(104).

001610	   03  R-BL4A REDEFINES W02-PRN-LINE.
001620	       05  FILLER      PIC  X(02).
001630	       05  R-BVAL      PIC  Z(08)9- OCCURS 13.

001640	   03  R-BL5 REDEFINES W02-PRN-LINE.
001650	       05  FILLER      PIC  X(02).
001660	       05  R-BH20      OCCURS 13.
001670		   07  R-BH21  PIC  X(09).
001680		   07  FILLER  PIC  X(01).

       COPY W05.GL.

002630 01  W10-H2.
002640     03  FILLER      PIC  X(27).
002650     03  W10-HEAD    PIC  X(13).

       COPY W15.GL.

003060 01  W20-TOTALS.
003070     03  W20-DEBIT   PIC S9(09)V99 COMP-3.
003080     03  W20-CREDIT  PIC S9(09)V99 COMP-3.
003090     03  W20-RESULT  PIC S9(09)V99.
003100     03  W20-RES1    REDEFINES W20-RESULT.
003110         05  W20-WHOLE
003120                     PIC  9(09).
003130         05  W20-DEC PIC S9(02).
003140     03  W20-TOTAL   PIC S9(09)V99 COMP-3.
003150     03  W20-STOTAL  PIC S9(09)V99 COMP-3.
003160     03  W20-BTOTAL  PIC S9(09)    COMP-3.
003170     03  W20-INTOT   PIC S9(09)V99 COMP-3.
003180     03  W20-COTOT   PIC S9(09)V99 COMP-3.
003190     03  W20-GTOT.
003200         05  W20-GRPTOT
003210                     PIC S9(09)V99 COMP-3 OCCURS 12.
003220     03  W20-STOT.
003230         05  W20-SUBTOT
003240                     PIC S9(09)V99 COMP-3 OCCURS 12.
003250 01  W20-INPUT.
003260     03  W20-ACCOUNT PIC  9(06).
003270     03  W20-ACNO1   REDEFINES W20-ACCOUNT.
003280         05  W20-ACNO
003290                     PIC 9(04).
003300         05  W20-SER PIC  9(02).
003310     03  W20-ACNO2   REDEFINES W20-ACCOUNT.
003320         05  W20-GR  PIC  9(02).
003330         05  W20-SGRP
003340                     PIC 9(02).
003350         05  W20-ASER
003360                     PIC 9(02).
003370     03  W20-ACNO3   REDEFINES W20-ACCOUNT.
003380         05  W20-GROUP
003390                     PIC 9(02).
003400         05  W20-REST
003410                     PIC 9(04).

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

003420 01  W25-EDIT.
003430     03  W25-7.
003440         05  W25-1   PIC  ZZZZZZZZ9-.
003450     03  W25-S7V2.
003460         05  W25-V1  PIC  Z(07)9.99CR.
003990	   03  W25-S8V2.
004000	       05  W25-VG1     PIC Z(07)9.99CR.
003470	   03  W25-DATE.
003480	       05  W25-DTE PIC	Z9/99/9999.
003490     03  W25-REF.
003500         05  W25-REFER
003510                     PIC Z(07).
003520     03  W25-RECKEY  PIC  Z(04)9.
003530 01  W30-MONTH-NAMES.
003540     03  W30-MONTHS.
003550         05  FILLER  PIC  X(24) VALUE
003560                     "Jan:Feb:Mar:Apr:May:Jun:".
003570         05  FILLER  PIC X(24) VALUE
003580                     "Jul:Aug:Sep:Oct:Nov:Dec:".
003590     03  W30-MNTHS REDEFINES W30-MONTHS.
003600         05  W30-MONTH OCCURS 12.
003610             07  W30-MTH PIC X(03).
003620             07  FILLER  PIC X(01).

003630 01  W35-MONTH-NAMES.
003640     03  W35-MONTHS.
003650         05  W35-MONTH OCCURS 12.
003660             07  W35-MTH PIC X(03).
003670             07  FILLER  PIC X(01).
003680     03  W35-MTHS REDEFINES W35-MONTHS.
003690         05  W35-MTH1.
003700             07  W35-M1  PIC X(03).
003710             07  FILLER  PIC X(01).
003720         05  W35-MTH2.
003730             07  W35-M2  PIC X(03).
003740             07  FILLER  PIC X(01).
003750         05  W35-MTH3.
003760             07  W35-M3  PIC X(03).
003770             07  FILLER  PIC X(01).
003780         05  W35-MTH4.
003790             07  W35-M4  PIC X(03).
003800             07  FILLER  PIC X(01).
003810         05  W35-MTH5.
003820             07  W35-M5  PIC X(03).
003830             07  FILLER  PIC X(01).
003840         05  W35-MTH6.
003850             07  W35-M6  PIC X(03).
003860             07  FILLER  PIC X(01).
003870         05  W35-MTH7.
003880             07  W35-M7  PIC X(03).
003890             07  FILLER  PIC X(01).
003900         05  W35-MTH8.
003910             07  W35-M8  PIC X(03).
003920             07  FILLER  PIC X(01).
003930         05  W35-MTH9.
003940             07  W35-M9  PIC X(03).
003950             07  FILLER  PIC X(01).
003960         05  W35-MTH10.
003970             07  W35-M10 PIC X(03).
003980             07  FILLER  PIC X(01).
003990         05  W35-MTH11.
004000             07  W35-M11 PIC X(03).
004010             07  FILLER  PIC X(01).
004020         05  W35-MTH12.
004030             07  W35-M12 PIC X(03).
004040             07  FILLER  PIC X(01).

004050 01  W40-CUR-YEAR.
004060	   03  W40-YEAR    PIC	9(04) COMP-5 OCCURS 12.

       COPY W40.WS.

       COPY FUNCTION.WS.

       COPY W50.WS.

004070 01  W50-CALC.
004080     05  W50-ACNTS   OCCURS 30.
004090         07  W50-ANO PIC  9(06)    COMP-3.
004100         07  W50-PER PIC S9(03)V99 COMP-3.

004110 01  W55-TOTALS.
004120     03  W55-VAL     PIC S9(09)    COMP-3 OCCURS 12.

004130 01  W55-TOT         REDEFINES W55-TOTALS.
004140     03  W55-1       PIC S9(09)    COMP-3.
004150     03  W55-2       PIC S9(09)    COMP-3.
004160     03  W55-3       PIC S9(09)    COMP-3.
004170     03  W55-4       PIC S9(09)    COMP-3.
004180     03  W55-5       PIC S9(09)    COMP-3.
004190     03  W55-6       PIC S9(09)    COMP-3.
004200     03  W55-7       PIC S9(09)    COMP-3.
004210     03  W55-8       PIC S9(09)    COMP-3.
004220     03  W55-9       PIC S9(09)    COMP-3.
004230     03  W55-10      PIC S9(09)    COMP-3.
004240     03  W55-11      PIC S9(09)    COMP-3.
004250     03  W55-12      PIC S9(09)    COMP-3.

002930 01  W55-TOTA	       REDEFINES W55-TOTALS.
002940	   03  W55-1A	       PIC S9(07)V99 COMP-3.
002950	   03  W55-2A	       PIC S9(07)V99 COMP-3.
002960	   03  W55-3A	       PIC S9(07)V99 COMP-3.
002970	   03  W55-4A	       PIC S9(07)V99 COMP-3.
002980	   03  W55-5A	       PIC S9(07)V99 COMP-3.
002990	   03  W55-6A	       PIC S9(07)V99 COMP-3.
003000	   03  W55-7A	       PIC S9(07)V99 COMP-3.
003010	   03  W55-8A	       PIC S9(07)V99 COMP-3.
003020	   03  W55-9A	       PIC S9(07)V99 COMP-3.
003030	   03  W55-10A	       PIC S9(07)V99 COMP-3.
003040	   03  W55-11A	       PIC S9(07)V99 COMP-3.
003050	   03  W55-12A	       PIC S9(07)V99 COMP-3.

002930 01  W55-TOTB	       REDEFINES W55-TOTALS.
004120	   03  W55-VALA    PIC S9(07)V99 COMP-3 OCCURS 12.

004260 01  W60-TOTALS.
004270     03  W60-VAL     PIC S9(09)    COMP-3 OCCURS 12.

004280 01  W65-TOTALS.
004290     03  W65-VAL     PIC S9(09)    COMP-3 OCCURS 12.

004300 01  W70-ACTS.
004310     03  W70-BRANCH  PIC  9(04)    COMP-5.

004470 COPY W70.WS.

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
	   03  W75-GNO	      PIC 9(06) COMP-5
                              OCCURS 18.
004510 01  W80-H3.
004520     03  FILLER      PIC  X(13)      VALUE "BUDGET AS AT ".
004530	   03  W80-DTE	   PIC	99/99/9999.

004540 01  W90-COMP.
004550     03  W90-CNAME   PIC  X(40).

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004560 SCREEN SECTION.

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

005060 01  S03.
005070	   03  LINE  3 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
                                 VALUE "GENERAL LEDGER BUDGET".
005080     03  LINE  5 COLUMN 12 VALUE "Account Number".
005090     03          COLUMN 35 VALUE "Account Name".
005100     03  LINE  7 COLUMN  9 PIC X(04) USING W35-MTH1.
005110     03          COLUMN 25 PIC X(04) USING W35-MTH2.
005120     03          COLUMN 41 PIC X(04) USING W35-MTH3.
005130     03          COLUMN 57 PIC X(04) USING W35-MTH4.
005140     03  LINE  8 COLUMN  9 PIC X(04) USING W35-MTH5.
005150     03          COLUMN 25 PIC X(04) USING W35-MTH6.
005160     03          COLUMN 41 PIC X(04) USING W35-MTH7.
005170     03          COLUMN 57 PIC X(04) USING W35-MTH8.
005180     03  LINE  9 COLUMN  9 PIC X(04) USING W35-MTH9.
005190     03          COLUMN 25 PIC X(04) USING W35-MTH10.
005200     03          COLUMN 41 PIC X(04) USING W35-MTH11.
005210     03          COLUMN 57 PIC X(04) USING W35-MTH12.
005220     03  LINE 11 COLUMN  8 VALUE "Total".

005230 01  S04.
005240	   03  LINE  7 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005250                           PIC Z(08)9- USING W55-1 AUTO.
005260	   03	       COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005270                           PIC Z(08)9- USING W55-2 AUTO.
005280	   03	       COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005290                           PIC Z(08)9- USING W55-3 AUTO.
005300	   03	       COLUMN 62 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005310                           PIC Z(08)9- USING W55-4 AUTO.
005320	   03  LINE  8 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005330                           PIC Z(08)9- USING W55-5 AUTO.
005340	   03	       COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005350                           PIC Z(08)9- USING W55-6 AUTO.
005360	   03	       COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005370                           PIC Z(08)9- USING W55-7 AUTO.
005380	   03	       COLUMN 62 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005390                           PIC Z(08)9- USING W55-8 AUTO.
005400	   03  LINE  9 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005410                           PIC Z(08)9- USING W55-9 AUTO.
005420	   03	       COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005430                           PIC Z(08)9- USING W55-10 AUTO.
005440	   03	       COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005450                           PIC Z(08)9- USING W55-11 AUTO.
005460	   03	       COLUMN 62 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005470                           PIC Z(08)9- USING W55-12 AUTO.

000850 01  S-PROG.
000860	   03  LINE  2 COLUMN 26 FOREGROUND-COLOR 7
				 HIGHLIGHT
006220                           VALUE "GENERAL LEDGER BUDGET CONTROL".

       01  S-MENU.
	   03  LINE  6 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  7 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³    ".
	   03	       COLUMN 34 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "BUDGET OPTIONS".
	   03	       COLUMN 48 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "    ³".
	   03  LINE  8 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Create/Amend Budget³".
	   03  LINE  9 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Calculate Budgets  ³".
001470	   03  LINE 10 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Budget Report      ³".
001470	   03  LINE 11 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Clear Budgets      ³".
001570	   03  LINE 12 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Exit               ³".
	   03  LINE 13 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

006330 01  S13.
006340	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z9/99/9999 USING W15-DATE AUTO.
006350 01  S14.
006360	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(07)9.99CR USING W15-VAL AUTO.
006370 01  S15.
006380	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC ZZ9.99 USING W15-PER AUTO.
       01  S16.
002970	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(06)9 USING W15-REFER AUTO.
       01  S17.
002970	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC 9(06) USING W15-ACCOUNT AUTO.

004870 COPY S99.CRT.

       COPY LEDLUP.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
006540 AA000	       SECTION.
006550 AA00.
003240	   IF LS0-GLLEV < 2
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006560	     PERFORM ZA000.
	     PERFORM IA000.
	     CLOSE RECOVER.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

       AA49.
	     EXIT PROGRAM.

       COPY FUNCTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP6.

       AA350-LOOKUP    SECTION.
       AA350.
011050	     PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
	     PERFORM GACCNT-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.

       AA399.
             EXIT.

       COPY APAC.HLP.

007600 AB000	       SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.

       COPY GACCNT.RD.

       COPY GLTRAN.RD.

       COPY GLJRN.RD.

       COPY LEDTRN.RD.

       COPY SHARED.RD.

       COPY TXTRAN.RD.

      /
008790 AC000           SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

       COPY GACCNT.WR.

       COPY GLTRAN.WR.

       COPY GLJRN.WR.

       COPY LEDTRN.WR.

       COPY TXTRAN.WR.

       COPY GACCNT.LUP.

      /
      *       ****   *****   ***    ***   *   *  *****	****   *   *
      *       *   *  *	    *	*  *   *  *   *  *	*   *  *   *
      *       ****   ***    *	   *   *  *   *  ***	****	* *
      *       *   *  *	    *	*  *   *   * *	 *	*   *	 *
      *       *   *  *****   ***    ***     *	 *****	*   *	 *
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      * 	 THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *        ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *        ³ ORIGINAL ACTION (REC-TYPE)  ÄÄ  RECOVERY PROCESS ³
      *        ³ 0 = RECORD CHANGED	     ÄÄ     (REWRITE)	   ³
      *        ³ 1 = RECORD WAS ADDED	     ÄÄ     (DELETE)	   ³
      *        ³ 2 = RECORD WAS DELETED      ÄÄ     (WRITE)	   ³
      *        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
010530 AY000	       SECTION.
010540 AY01.
010550	     MOVE 01		 TO REC-FILE.
010560	     MOVE WS-PARKEY	 TO REC-KEY.
011710	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
010570	     MOVE PAR-RECORD1	 TO REC-PARAM.
010580       GO TO AY50.

010590 AY12.
010600	     MOVE 12		 TO REC-FILE.
010610	     MOVE ZERO		 TO REC-KEY.
	     MOVE WS-ACTION	 TO REC-TYPE.
           IF WS-SKIP = "N"
               PERFORM READ-GACCNT-LOCK THRU READ-GACCNT-EXIT
           ELSE
               MOVE "N"          TO WS-SKIP.
010620       MOVE G-REC          TO REC-GACCNT.
010630       GO TO AY50.

010640 AY13.
010650	     MOVE 13		 TO REC-FILE.
010660       MOVE ZERO           TO REC-KEY.
012000       MOVE WS-ACTION      TO REC-TYPE.
010670       MOVE T-REC          TO REC-GLTRAN.
010680       GO TO AY50.

011970 AY19.
011980       MOVE 19             TO REC-FILE.
011990       MOVE ZERO           TO REC-KEY.
012000       MOVE WS-ACTION      TO REC-TYPE.
012010       MOVE TAX-RECORD1    TO REC-TXTRAN.
012020       GO TO AY50.

012150 AY39.
012160       MOVE 39             TO REC-FILE.
012170       MOVE WS-NETKEY      TO REC-KEY.
012180       MOVE NET-RECORD     TO REC-NETWORK.
012190       GO TO AY50.

012040 AY40.
012050       MOVE 99             TO REC-FILE.
012060       MOVE ZERO           TO REC-KEY.
012070       MOVE SPACES         TO REC-DETAIL.
012080       PERFORM AY50.
012090       ADD 1               TO WS-TRANS.
012100     IF WS-RECOVER > 95
012110         CLOSE RECOVER
012120         OPEN OUTPUT RECOVER
012130         CLOSE RECOVER
012140         OPEN I-O RECOVER
012150         MOVE ZERO         TO WS-RECOVER.
012160       GO TO AY59.

012170 AY50.
012180       ADD 1               TO WS-RECOVER.
012190       MOVE WS-RECOVER     TO WS-RECKEY.
012200       MOVE WS-TRANS       TO REC-TRAN.
012210       WRITE REC-RECORD.
012220     IF WS-STATUS NOT = "00"
012230         DISPLAY "Write error Recovery file - Status " AT 2312
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			WS-STATUS
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
012250         STOP RUN.

012280 AY59.
012290       EXIT.
      *
      *    ****    Start processing transaction
      *
012470 AY60.
	   IF WS-SKIP = "Y" OR "A"
               GO TO AY999.
012690	     MOVE 1		 TO WS-COUNT.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      *	     MOVE SHR-STOCK	 TO WS-STOCK.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
012500	     MOVE 4		 TO WS-PARKEY.
012510       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
012520     IF PAR-USERS < 24
012530         MOVE 1            TO WS-SUB
	       MOVE ZERO	 TO WS-WAIT
012540         GO TO AY62.
      *
      *    ****   Q   F U L L  -  W A I T   F O R   4	S E C O N D S
      *
012550	     DISPLAY "WAITING" AT 2551
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 4 HIGHLIGHT.
012560	     COMMIT.
	     ACCEPT WS-STIME FROM TIME.
	     MOVE 400		 TO WS-WAIT.
012580	     PERFORM LOCK-REC-LOOP.
012590	     DISPLAY SPACE AT 2551
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.
012600       GO TO AY60.

012610 AY61.
012620	     MOVE "GL"		 TO PAR-PROG(WS-USUB).
012630	     MOVE LS-USER	 TO PAR-USR(WS-USUB).
012640       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the LEDGER control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
012010       MOVE 4              TO WS-NETKEY.
012680	     PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012690       PERFORM AY39 THRU AY59.
012740	     GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N	P R O G R E S S
      *
012750 AY62.
	   IF NOT (PAR-PROG(WS-SUB) = SPACES OR
		   PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R	O R   S T O C K   F I L E S
      * 	  B E I N G   U P D A T E D
      *
	       IF NOT (PAR-PROG(WS-SUB) = SPACES)
012760		   IF PAR-PROG(WS-SUB) = "CG" OR "GL"
      *
      *    ****   Y E S   -   S E T   W A I T	P E R I O D
      *
		       GO TO AY62-WAIT
		   ELSE
		       ADD 1	 TO WS-SUB
		       GO TO AY62
		   END-IF
	       ELSE
      *
      *    ****   I S	T H I S   P R O G R A M   I N	T H E  Q
      *
	       IF PAR-USR(WS-SUB) = LS-USER
      *
      *    ****   I S	I T   N E X T	I N   L I N E	T O   P R O C E S S
      *
		   IF WS-WAIT = ZERO
		       GO TO AY63
		   ELSE
		       GO TO AY62-WAIT
		   END-IF
	       ELSE
		   GO TO AY62-WAIT
	       END-IF
	   END-IF.
	     MOVE LS-USER	 TO PAR-USR(WS-SUB).
	     MOVE WS-SUB	 TO PAR-USERS.
	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
	     GO TO AY62-CHECK.
      *
      *    ****   S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
	     MOVE 300		 TO WS-WAIT.
	   IF NOT (PAR-USR(WS-SUB) = LS-USER)
	       IF WS-SUB < 24
		   ADD 1	 TO WS-SUB
		   GO TO AY62.

       AY62-CHECK.
	   IF WS-WAIT > ZERO
	       COMMIT
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 6 HIGHLIGHT BLINK
	       ACCEPT WS-STIME FROM TIME
	       PERFORM LOCK-REC-LOOP
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 6 HIGHLIGHT BLINK
	       GO TO AY60.
012880	     DISPLAY SPACE AT 2572
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.

012900 AY63.
012920	     MOVE WS-SUB	 TO WS-USUB.
012940	     GO TO AY61.

012970 AY70.
013030	     MOVE 4		 TO WS-PARKEY.
013040       PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      * 	   NETWORK record 2.
      *
013010	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
013020	     PERFORM AY40 THRU AY59.
	     MOVE 1		 TO WS-USUB.

       AY72.
	   IF NOT (PAR-USR(WS-USUB) = LS-USER)
	       ADD 1		 TO WS-USUB
	       GO TO AY72.

       AY75.
013050	     MOVE SPACES	 TO PAR-PROG(WS-USUB)
013060				    PAR-USR(WS-USUB).
	   IF WS-USUB < 24
	       ADD 1 WS-USUB	 GIVING WS-SUB
	   ELSE
	       GO TO AY80.
	   IF NOT (PAR-PROG(WS-SUB) = SPACES)
	       MOVE PAR-PROG(WS-SUB)
				 TO PAR-PROG(WS-USUB)
	       MOVE PAR-USR(WS-SUB)
				 TO PAR-USR(WS-USUB)
	       ADD 1		 TO WS-USUB
	       GO TO AY75.

       AY80.
013070	     SUBTRACT 1 FROM WS-USUB
				 GIVING PAR-USERS.
013080	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
013090	     COMMIT.

011860 AY999.
011870       EXIT.

      /
025220 IA000	     SECTION 5.
025230 IA00.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 1		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
	     DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 31		 TO SHADE-COL.
	     MOVE 22		 TO SHADE-WIDTH.
	     MOVE 7		 TO SHADE-LINES.
	     DISPLAY S-MENU.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.

       IA02.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 11		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 31		 TO SCREEN-COL.
	     MOVE 20		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       IA05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       IA10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO IA15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO IA10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO IA10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO IA10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO IA10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO IA15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO IA05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO IA18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO IA05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       IF WS-OPTION < 0 OR > 4
		   CALL X"E5"
		   GO TO IA05
	       END-IF
	       GO TO IA16
	   ELSE
	       CALL X"E5"
	       GO TO IA05.

       IA15.
	   IF SCREEN-LIN < 7
	       CALL X"E5"
	       GO TO IA05
	   ELSE
	   IF SCREEN-LIN = 7
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 8
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "3"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 10
	       MOVE "4"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 11
	       MOVE "0"		 TO WS-OPTION.
	   GO TO IA18.

       IA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 11		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "1"
	       MOVE 7		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE 8		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE 10		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       IA18.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
	   IF WS-OPTION = "0"
	       GO TO IA999.
	   IF WS-OPTION = "1"
	       PERFORM IB000
	   ELSE
	   IF WS-OPTION = "2"
	       PERFORM IC000
	   ELSE
	   IF WS-OPTION = "3"
	       PERFORM ID000
	   ELSE
	   IF WS-OPTION = "4"
	       PERFORM IE000.
	     GO TO IA00.

025410 IA999.
025420       EXIT.
      /
025430 IB000	     SECTION 5.
025440 IB00.
025450       DISPLAY CLR-SCREEN.
025460       DISPLAY S03.

025470 IB05.
025480	     MOVE ZERO		 TO W15-ACCOUNT.

025490 IB15.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		     " to do Account Lookup - "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		     "ape to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
025500       ACCEPT S17 AT 0527.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE ZERO	 TO W15-ACCOUNT
		     GO TO IB999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     PERFORM AA350
		 WHEN OTHER
		     CALL X"E5"
		     GO TO IB15
	       END-EVALUATE
	       DISPLAY S17 AT 0527
               IF W15-ACCOUNT = ZERO
015990		   GO TO IB15.
	     DISPLAY CLEAR-L25.
             MOVE 1401           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 23.
025530     IF W15-ACCOUNT = ZERO
025540         GO TO IB999.
025550     IF W15-ASER = ZERO
025560	       MOVE WS-ER7	 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
025570	       GO TO IB15.

025580 IB25.
	     MOVE W15-ACCOUNT	 TO G-AC.
008980	     PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
025600	   IF WS-F-ERROR = 8
025610	       MOVE WS-ER6	 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
025620	       GO TO IB15.
025630	     MOVE G-NAME	 TO W15-ACNME.
025640	     DISPLAY W15-ACNME AT 0548
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     PERFORM CHECK-CORRECT.
025720	   IF WS-OPTION = "N"
025730         GO TO IB00.
025740     IF G-IND = 1
025750         DISPLAY "Figures for this A/C are calculated" AT 1429
025760			WITH FOREGROUND-COLOR 6 HIGHLIGHT
025770         GO TO IB15.
             MOVE 1401           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 23.
025790	     MOVE 1		 TO WS-S1.
025800	     ADD 1 WS-Y-END	 GIVING WS-S2.

025810 IB26.
025820     IF WS-S2 > 12
025830	       SUBTRACT 12	 FROM WS-S2.
025840       MOVE G-BVAL (WS-S2) TO W55-VAL (WS-S1).
025850     IF WS-S1 < 12
025860	       ADD 1		 TO WS-S1 WS-S2
025870         GO TO IB26.

025880 IB30.
025890       ACCEPT S04.
025900	     MOVE 1		 TO WS-S1.
025910	     ADD 1 WS-Y-END	 GIVING WS-S2.

025920 IB32.
025930     IF WS-S2 > 12
025940	       SUBTRACT 12	 FROM WS-S2.
025950	     MOVE W55-VAL (WS-S1)
				 TO G-BVAL (WS-S2).
025960     IF WS-S1 < 12
025970	       ADD 1		 TO WS-S1 WS-S2
025980         GO TO IB32.
025990       COMPUTE W20-TOTAL = G-BVAL (1) + G-BVAL (2) + G-BVAL (3)
026000                          + G-BVAL (4) + G-BVAL (5) + G-BVAL (6)
026010                          + G-BVAL (7) + G-BVAL (8) + G-BVAL (9)
026020                          + G-BVAL (10) + G-BVAL (11) 
026030                          + G-BVAL (12).
026040	     MOVE W20-TOTAL	 TO W25-1.
026050	     DISPLAY W25-7 AT 1114
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     PERFORM CHECK-CORRECT.
026130	   IF WS-OPTION = "N"
026140         GO TO IB30.
026160       PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
026170       GO TO IB00.

026180 IB999.
026190       EXIT.
      /
026200 IC000	     SECTION 5.
026210 IC00.
026220       DISPLAY CLR-SCREEN.
026230	     DISPLAY "Busy with calcs - Please be patient" AT 0612
026240		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
026250	     MOVE 1		 TO WS-S2.

026260 IC03.
026270	     MOVE ZERO		 TO W55-VAL (WS-S2).
026280     IF WS-S2 < 12
026290	       ADD 1		 TO WS-S2
026300         GO TO IC03.
026310	     MOVE 1		 TO WS-S2.
	     MOVE 100000	 TO G-AC.
	     PERFORM START-AT-LEDG-AC THRU READ-GACCNT-EXIT.
	   IF WS-F-ERROR = 8
	       GO TO IC999.
	     DISPLAY "Account :" AT 0812.

026330 IC05.
	     PERFORM READ-GACCNT-NEXT THRU READ-GACCNT-EXIT.
026340	   IF WS-F-ERROR = 8
026350	       GO TO IC999.
	     DISPLAY G-AC AT 0822 WITH FOREGROUND-COLOR 3 HIGHLIGHT
				       BACKGROUND-COLOR 5.
026360	     MOVE G-AC		 TO W15-ACCOUNT.
026370     IF W15-ASER = ZERO
026380	       GO TO IC05.
026390	   IF W15-GRP = 1
026400         GO TO IC25.
026410     IF G-IND NOT = 1
026420	       GO TO IC05.

026440 IC10.
026460       MOVE G-BPER         TO W15-PER.
026530	     MOVE 1		 TO WS-S1.

026540 IC15.
026550       COMPUTE G-BVAL (WS-S1) = (W55-VAL (WS-S1) * W15-PER)
026560                                  / 100.
026570     IF WS-S1 < 12
026580	       ADD 1		 TO WS-S1
026590         GO TO IC15.
026600       PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
026630       GO TO IC05.

026640 IC25.
026650	     ADD G-BVAL (1)	 TO W55-VAL (1).
026660	     ADD G-BVAL (2)	 TO W55-VAL (2).
026670	     ADD G-BVAL (3)	 TO W55-VAL (3).
026680	     ADD G-BVAL (4)	 TO W55-VAL (4).
026690	     ADD G-BVAL (5)	 TO W55-VAL (5).
026700	     ADD G-BVAL (6)	 TO W55-VAL (6).
026710	     ADD G-BVAL (7)	 TO W55-VAL (7).
026720	     ADD G-BVAL (8)	 TO W55-VAL (8).
026730	     ADD G-BVAL (9)	 TO W55-VAL (9).
026740	     ADD G-BVAL (10)	 TO W55-VAL (10).
026750	     ADD G-BVAL (11)	 TO W55-VAL (11).
026760	     ADD G-BVAL (12)	 TO W55-VAL (12).
026770	     GO TO IC05.
026800 IC999.
026810       EXIT.
      /
026820 ID000	     SECTION 5.
026830 ID0.
	     MOVE 100000	 TO G-AC.
	     PERFORM START-AT-LEDG-AC THRU READ-GACCNT-EXIT.
	   IF WS-F-ERROR = 8
	       GO TO ID999.
	     MOVE "P"		 TO WS-COMMAND.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 3		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.

026860 ID00.
026870       DISPLAY CLR-SCREEN.
026880	     DISPLAY "BUDGET REPORT" AT 0233
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
026890	     DISPLAY "Busy printing Budget report" AT 0612
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     DISPLAY "Account :" AT 0812.

026900 ID05.
	     MOVE "H"		 TO W02-PRN-TYPE.
026910	     MOVE SPACES	 TO R-DET.
026920	     MOVE W90-CNAME	 TO R-BH1.
026930	     MOVE W05-TODAY	 TO W80-DTE.
026940	     MOVE W80-H3	 TO R-BH3.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
026960	     MOVE SPACES	 TO R-DET.
026970	     MOVE "ACC" 	 TO R-BH18.
026980	     MOVE "DESCRIPTION"  TO R-BH19.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
027000	     MOVE SPACES	 TO R-DET.
027010	     MOVE W35-M1	 TO R-BH5.
027020	     MOVE W35-M2	 TO R-BH6.
027030	     MOVE W35-M3	 TO R-BH7.
027040	     MOVE W35-M4	 TO R-BH8.
027050	     MOVE W35-M5	 TO R-BH9.
027060	     MOVE W35-M6	 TO R-BH10.
027070	     MOVE W35-M7	 TO R-BH11.
027080	     MOVE W35-M8	 TO R-BH12.
027090	     MOVE W35-M9	 TO R-BH13.
027100	     MOVE W35-M10	 TO R-BH14.
027110	     MOVE W35-M11	 TO R-BH15.
027120	     MOVE W35-M12	 TO R-BH16.
027130	     MOVE "TOTAL"	 TO R-BH17.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
027150	     MOVE SPACES	 TO R-DET.
	     MOVE "D"		 TO W02-PRN-TYPE.

027160 ID06.
027170	     MOVE 1		 TO WS-S2.

027180 ID08.
027190	     MOVE ZERO		 TO W55-VAL (WS-S2) W60-VAL (WS-S2)
027200				    W65-VAL (WS-S2).
027210     IF WS-S2 < 12
027220	       ADD 1		 TO WS-S2
027230         GO TO ID08.
027240	     MOVE ZERO		 TO W15-MONTH.

027260 ID10.
	     PERFORM READ-GACCNT-NEXT THRU READ-GACCNT-EXIT.
027270	   IF WS-F-ERROR = 8
027280         GO TO ID50.
	     DISPLAY G-AC AT 0822 WITH FOREGROUND-COLOR 3 HIGHLIGHT
				       BACKGROUND-COLOR 5.
027290	   IF W15-MONTH = G-GRP
027300         GO TO ID45.
027310     IF G-GRP > 3
027320         GO TO ID50.
027330     IF G-GRP = 1
027340	       MOVE "**** INCOME ****"
				 TO R-BH19
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
027360	       MOVE SPACES	 TO R-DET
027370         GO TO ID45.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
	       GO TO ID54.
027380	   IF W15-MONTH = 1
027390         GO TO ID55.
027400     IF W15-MONTH = ZERO
027410         GO TO ID40.
027420     IF W15-MONTH > 2
027430         GO TO ID45.

027440 ID15.
027450       MOVE ALL "-" TO R-BH21 (1) R-BH21 (2) R-BH21 (3) R-BH21 (4)
027460                       R-BH21 (5) R-BH21 (6) R-BH21 (7) R-BH21 (8)
027470                       R-BH21 (9) R-BH21 (10) R-BH21 (11)
027480                       R-BH21 (12) R-BH21 (13).
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
027500	     MOVE SPACES	 TO R-DET.

027510 ID20.
027520	     MOVE "TOTAL COST OF SALES"
				 TO R-BH4.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
027540	     MOVE SPACES	 TO R-DET.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
	       GO TO ID54.
027550	     MOVE 1		 TO WS-S1.

027560 ID25.
027570	     SUBTRACT W60-VAL (WS-S1)
				 FROM W55-VAL (WS-S1).
027580     IF WS-S1 < 12
027590	       ADD 1		 TO WS-S1
027600         GO TO ID25.

027610 ID30.
027620	     ADD 1 WS-Y-END	 GIVING WS-S2.
027630	     MOVE 1		 TO WS-S1.

027640 ID32.
027650     IF WS-S2 > 12
027660	       SUBTRACT 12	 FROM WS-S2.
027670	     MOVE W60-VAL (WS-S2)
				 TO R-BVAL (WS-S1).
027680     IF WS-S1 < 12
027690	       ADD 1		 TO WS-S1 WS-S2
027700         GO TO ID32.
027710	     MOVE W60-VAL (1)	 TO W20-TOTAL.
027720	     ADD W60-VAL (2)	 TO W20-TOTAL.
027730	     ADD W60-VAL (3)	 TO W20-TOTAL.
027740	     ADD W60-VAL (4)	 TO W20-TOTAL.
027750	     ADD W60-VAL (5)	 TO W20-TOTAL.
027760	     ADD W60-VAL (6)	 TO W20-TOTAL.
027770	     ADD W60-VAL (7)	 TO W20-TOTAL.
027780	     ADD W60-VAL (8)	 TO W20-TOTAL.
027790	     ADD W60-VAL (9)	 TO W20-TOTAL.
027800	     ADD W60-VAL (10)	 TO W20-TOTAL.
027810	     ADD W60-VAL (11)	 TO W20-TOTAL.
027820	     ADD W60-VAL (12)	 TO W20-TOTAL.
027830	     MOVE W20-TOTAL	 TO R-BVAL (13).
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
027850	     MOVE SPACES	 TO R-DET.
027860	     PERFORM ID15.

027870 ID35.
027880       PERFORM ID15.
027890	     MOVE W55-TOTALS	 TO W60-TOTALS.
027900	     MOVE "GROSS PROFIT/LOSS "
				 TO R-BH4.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
027920	     MOVE SPACES	 TO R-DET.
027930       PERFORM ID30 THRU ID32.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
	       GO TO ID54.

027940 ID40.
027950	     MOVE "**** EXPENSES ****"
				 TO R-BH19.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
027970	     MOVE SPACES	 TO R-DET.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
	       GO TO ID54.

027980 ID45.
027990	     MOVE G-GRP 	 TO W15-MONTH.
028000	     MOVE G-NAME	 TO R-BDESC.
028010	     MOVE G-AC		 TO R-BAC.
028020     IF G-ASER = ZERO
028030         GO TO ID46.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
028050	     MOVE SPACES	 TO R-DET.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
	       GO TO ID54.
028060	     ADD 1 WS-Y-END	 GIVING WS-S2.
028070	     MOVE 1		 TO WS-S1.

028080 ID45A.
028090     IF WS-S2 > 12
028100	       SUBTRACT 12	 FROM WS-S2.
028110       MOVE G-BVAL (WS-S2) TO R-BVAL (WS-S1).
028120     IF WS-S1 < 12
028130	       ADD 1		 TO WS-S1 WS-S2
028140         GO TO ID45A.
028150	     MOVE G-BVAL (1)	 TO W20-TOTAL.
028160	     ADD G-BVAL (2)	 TO W20-TOTAL.
028170	     ADD G-BVAL (3)	 TO W20-TOTAL.
028180	     ADD G-BVAL (4)	 TO W20-TOTAL.
028190	     ADD G-BVAL (5)	 TO W20-TOTAL.
028200	     ADD G-BVAL (6)	 TO W20-TOTAL.
028210	     ADD G-BVAL (7)	 TO W20-TOTAL.
028220	     ADD G-BVAL (8)	 TO W20-TOTAL.
028230	     ADD G-BVAL (9)	 TO W20-TOTAL.
028240	     ADD G-BVAL (10)	 TO W20-TOTAL.
028250	     ADD G-BVAL (11)	 TO W20-TOTAL.
028260	     ADD G-BVAL (12)	 TO W20-TOTAL.
028270	     MOVE W20-TOTAL	 TO R-BVAL (13).
028280     IF G-GRP = 1
028290	       ADD G-BVAL (1)	 TO W55-VAL (1)
028300	       ADD G-BVAL (2)	 TO W55-VAL (2)
028310	       ADD G-BVAL (3)	 TO W55-VAL (3)
028320	       ADD G-BVAL (4)	 TO W55-VAL (4)
028330	       ADD G-BVAL (5)	 TO W55-VAL (5)
028340	       ADD G-BVAL (6)	 TO W55-VAL (6)
028350	       ADD G-BVAL (7)	 TO W55-VAL (7)
028360	       ADD G-BVAL (8)	 TO W55-VAL (8)
028370	       ADD G-BVAL (9)	 TO W55-VAL (9)
028380	       ADD G-BVAL (10)	 TO W55-VAL (10)
028390	       ADD G-BVAL (11)	 TO W55-VAL (11)
028400	       ADD G-BVAL (12)	 TO W55-VAL (12)
028410         GO TO ID46.
028420     IF G-GRP = 2
028430	       ADD G-BVAL (1)	 TO W60-VAL (1)
028440	       ADD G-BVAL (2)	 TO W60-VAL (2)
028450	       ADD G-BVAL (3)	 TO W60-VAL (3)
028460	       ADD G-BVAL (4)	 TO W60-VAL (4)
028470	       ADD G-BVAL (5)	 TO W60-VAL (5)
028480	       ADD G-BVAL (6)	 TO W60-VAL (6)
028490	       ADD G-BVAL (7)	 TO W60-VAL (7)
028500	       ADD G-BVAL (8)	 TO W60-VAL (8)
028510	       ADD G-BVAL (9)	 TO W60-VAL (9)
028520	       ADD G-BVAL (10)	 TO W60-VAL (10)
028530	       ADD G-BVAL (11)	 TO W60-VAL (11)
028540	       ADD G-BVAL (12)	 TO W60-VAL (12)
028550         GO TO ID46.
028560	     ADD G-BVAL (1)	 TO W65-VAL (1).
028570	     ADD G-BVAL (2)	 TO W65-VAL (2).
028580	     ADD G-BVAL (3)	 TO W65-VAL (3).
028590	     ADD G-BVAL (4)	 TO W65-VAL (4).
028600	     ADD G-BVAL (5)	 TO W65-VAL (5).
028610	     ADD G-BVAL (6)	 TO W65-VAL (6).
028620	     ADD G-BVAL (7)	 TO W65-VAL (7).
028630	     ADD G-BVAL (8)	 TO W65-VAL (8).
028640	     ADD G-BVAL (9)	 TO W65-VAL (9).
028650	     ADD G-BVAL (10)	 TO W65-VAL (10).
028660	     ADD G-BVAL (11)	 TO W65-VAL (11).
028670	     ADD G-BVAL (12)	 TO W65-VAL (12).

028680 ID46.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
028730	       PERFORM ID05.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
	       GO TO ID54.
028740	     MOVE SPACES	 TO R-DET.
028770       GO TO ID10.

028780 ID50.
028790	     MOVE W65-TOTALS	 TO W60-TOTALS.
028800       PERFORM ID15.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
	       GO TO ID54.
028810	     MOVE "TOTAL EXPENDITURE ACCOUNTS"
				 TO R-BH4.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
	       GO TO ID54.
028830	     MOVE SPACES	 TO R-DET.
028840	     MOVE 1		 TO WS-S1.
028850       PERFORM ID25 THRU ID32.
028860       PERFORM ID15.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
	       GO TO ID54.
028870	     MOVE W55-TOTALS	 TO W60-TOTALS.
028880	     MOVE "TOTAL PROFIT/LOSS"
				 TO R-BH4.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
	       GO TO ID54.
028900	     MOVE SPACES	 TO R-DET.
028910       PERFORM ID30 THRU ID32.

       ID52.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 1		 TO W02-PRN-TYPE
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
028950	     GO TO ID999.

       ID54.
014970	     MOVE "**** PRINT ABORTED BY OPERATOR ****"
				 TO R-DET.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     GO TO ID52.

028960 ID55.
028970	     MOVE W55-TOTALS	 TO W60-TOTALS.
028980       PERFORM ID15.
028990       MOVE "TOTAL INCOME" TO R-BH4.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
029010	     MOVE SPACES	 TO R-DET.
029020       PERFORM ID30 THRU ID32.
029030	     MOVE 1		 TO WS-S2.

029040 ID56.
029050	     MOVE ZERO		 TO W60-VAL (WS-S2).
029060     IF WS-S2 < 12
029070	       ADD 1		 TO WS-S2
029080         GO TO ID56.

029090 ID60.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
029110	     GO TO ID45.

029120 ID999.
029130       EXIT.
      /
026200 IE000	     SECTION 5.
026210 IE00.
026220       DISPLAY CLR-SCREEN.
026230	     DISPLAY "Busy clearing bugets - Please be patient" AT 0612
026240		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
	     MOVE 100000	 TO G-AC.
	     PERFORM START-AT-LEDG-AC THRU READ-GACCNT-EXIT.
	   IF WS-F-ERROR = 8
	       GO TO IE999.
	     DISPLAY "Account :" AT 0812.
026330 IE05.
	     PERFORM READ-GACCNT-NEXT THRU READ-GACCNT-EXIT.
026340	   IF WS-F-ERROR = 8
026350	       GO TO IE999.
	     DISPLAY G-AC AT 0822 WITH FOREGROUND-COLOR 3 HIGHLIGHT
				       BACKGROUND-COLOR 5.
026360	     MOVE G-AC		 TO W15-ACCOUNT.
026370     IF W15-ASER = ZERO
026380	       GO TO IE05.
026440 IE10.
026530	     MOVE 1		 TO WS-S1.
026540 IE15.
026550	     MOVE ZERO		 TO G-BVAL (WS-S1).
026570     IF WS-S1 < 12
026580	       ADD 1		 TO WS-S1
026590	       GO TO IE15.
026600       PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
026630	     GO TO IE05.
026800 IE999.
026810       EXIT.
      /
035270 ZA000	     SECTION 9.
035280 ZA00.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
035290	     DISPLAY CLR-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     COPY FUNCTION.PRO.
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
      *    *************************************************************
      *    *** G E N E R A L   L E D G E R   A C C O U N T   F I L E ***
      *    *************************************************************
	     MOVE "GACCNT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GACCNT.
      *    *************************************************************
      *    *** G E N E R A L   L E D G E R   J O U R N A L   F I L E ***
      *    *************************************************************
	     MOVE "GLJRN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-GLJRN.
      *    *************************************************************
      *    ***	 G / L E D G E R   T R A N S A C T I O N   F I L E   ***
      *    *************************************************************
	     MOVE "GLTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GLTRAN.
	     MOVE "LEDTRN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-LEDTRN.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
      *    *************************************************************
      *    **** 	    R E C O V E R Y   F I L E		    ****
      *    *************************************************************
	     MOVE "RECOVER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-RECOVER.
      *    *************************************************************
      *    ****      N E T W O R K   (S H A R E D)   F I L E	    ****
      *    *************************************************************
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
	     MOVE "TXTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-TXTRAN.
	   IF LS-USER NUMERIC
	       IF LS-USE > ZERO AND < 110
		   MOVE LS-USE	 TO WS-USUB
		   GO TO ZA02.
             MOVE 110            TO WS-USUB.

042170 ZA02.
035650	     OPEN OUTPUT RECOVER GLJRN.
035660       CLOSE RECOVER GLJRN.
035670       OPEN I-O RECOVER GLJRN.
035680	     MOVE 1		 TO WS-PARKEY.
035690       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
035700	     MOVE PAR-COMPANY	 TO W90-CNAME.
035710	     MOVE PAR-DMY	 TO W05-TODAY.
035720	     MOVE PAR-YMD	 TO W05-TYMD.
	     MOVE 6		 TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DEBGL      TO W70-DEBGL.
             MOVE PAR-CREGL      TO W70-CREGL.
             MOVE PAR-INTGL      TO W70-INTGL.
             MOVE PAR-BNKGL      TO W70-BNKGL.
             MOVE PAR-UNPROF     TO W70-UNPROF.
             MOVE 4              TO WS-NETKEY.
035770       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
035780	     MOVE G-BATCH	 TO WS-BATCH.
035900     IF G-YEAR-END = ZERO
035910	       MOVE 2		 TO G-YEAR-END
035920	       PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
      *
      *    ****    S E T   U P   M O N T H S   ( Y E A R   E N D)
      *
035930	     MOVE 1		 TO WS-S1.
035940	     MOVE G-YEAR-END	 TO WS-S2 WS-Y-END.
035160	     MOVE G-LAST-YE	 TO WS-L-END.
	     MOVE G-OPEN	 TO WS-OPEN.
035950	     ADD 1		 TO WS-S2.

035960 ZA15.
035970     IF WS-S2 > 12
035980	       SUBTRACT 12	 FROM WS-S2.
035990	     MOVE W30-MONTH (WS-S2)
				 TO W35-MONTH (WS-S1).
036000     IF WS-S1 < 12
036010	       ADD 1		 TO WS-S1 WS-S2
036020         GO TO ZA15.
036030	     MOVE ZERO		 TO WS-ERROR.
036040     IF W70-BNKGL = ZERO
036050	       DISPLAY "Parameters not set up - " AT 2020
		       "ENTER" WITH FOREGROUND-COLOR 6 HIGHLIGHT
036070	       ACCEPT WS-OPTION AT 2050
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT AUTO
036080	       MOVE 1		 TO WS-ERROR.
036120	     MOVE WS-Y-END	 TO WS-S1.
036130	     MOVE W05-CY	 TO WS-S2.
036140     IF W05-MM < WS-Y-END
036150	       SUBTRACT 1	 FROM WS-S2.

036160 ZA20.
036170	     ADD 1		 TO WS-S1.
036180     IF WS-S1 > 12
036190	       MOVE 1		 TO WS-S1
036200	       ADD 1		 TO WS-S2.
036210	     MOVE WS-S2 	 TO W40-YEAR (WS-S1).
036220     IF WS-S1 NOT = WS-Y-END
036230         GO TO ZA20.
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.
036250	     GO TO ZA999.

       COPY ZA49.PRO.

       ZA55.
041220	     MOVE 1		 TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.

041240 ZA60.
041250	     MOVE WS-G1 	 TO WS-TCHR(WS-S1) WS-BCHR(WS-S1).
	     MOVE WS-G8 	 TO WS-TCH(WS-S1) WS-BCH(WS-S1).
041260     IF WS-S1 < 80
041270	       ADD 1		 TO WS-S1
041280         GO TO ZA60.
	     MOVE WS-G9 	 TO WS-TCH(1).
	     MOVE WS-G10	 TO WS-TCH(80).
	     MOVE WS-G11	 TO WS-BCH(1).
	     MOVE WS-G12	 TO WS-BCH(80).
	     MOVE WS-G14	 TO WS-TCHR(1)	WS-BCHR(1).
	     MOVE WS-G13	 TO WS-TCHR(80) WS-BCHR(80).
021930	     MOVE WS-G2 	 TO WS-TCHR(11) WS-TCHR(45)
021940				    WS-TCHR(63).
021950	     MOVE WS-G3 	 TO WS-MCHR(11) WS-MCHR(45)
021960				    WS-MCHR(63)
				    WS-MCHR(1)	WS-MCHR(80).
021970	     MOVE WS-G4 	 TO WS-BCHR(11) WS-BCHR(45)
021980				    WS-BCHR(63).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"
				 TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"
				 TO WS-WRKHD
	       MOVE LS-USER	 TO WS-WRKST.

036260 ZA200.

       COPY LOCKED.PRO.

       ZA205.
             EXIT PROGRAM.

036340 ZA999.
036350       EXIT.
      /
036360 ZB000	     SECTION 9.

       COPY ERRORS.PRO.

041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620	       MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041640     ELSE
041810     IF WS-F-ERROR = 8
041820	       MOVE W02-GACCNT	 TO WS-FILE
041830	       MOVE ZERO	 TO WS-KEY
	       MOVE G-AC	 TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 11
041860         MOVE W02-GLTRAN   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE T-KEY        TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 18
041860	       MOVE W02-RECOVER  TO WS-FILE
041870	       MOVE WS-RECKEY	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 26
041860	       MOVE W02-GLJRN	 TO WS-FILE
041870	       MOVE WS-GLJKEY	 TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 32
041860         MOVE W02-TXTRAN   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE TAX-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 39
041860         MOVE W02-LEDTRN   TO WS-FILE
041870         MOVE WS-LTRKEY    TO WS-KEY.

       COPY DISPERR.PRO.
