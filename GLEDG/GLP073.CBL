      $set LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *    ******   **	      *******	  ****	   ******    ******    *
      *   **	**  **	      **    **	 **  **   **	**  **	  **   *
      *   **	    **	      **    **	**    **  **		  **   *
      *   **	    **	      *******	**    **  *******	***    *
      *   **   ***  **	      **	**    **  **	**	  **   *
      *   **	**  **	      **	 **  **   **	**  **	  **   *
      *    ******   ********  **	  ****	   ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     G / L E D G E R   P R I N T   B A T C H		       *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	       GLP063.
000030 AUTHOR.                 J.W. LEMMON (APAC).
000040 DATE-WRITTEN.           OCTOBER 1992.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1992 - 2008
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

       COPY GLBTCH.SL.

000260     SELECT PRNREP ASSIGN W02-PRINTER
                         ORGANIZATION LINE SEQUENTIAL.
000270 DATA DIVISION.
000280 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000300 COPY PARAM.FDE.

000320 COPY GACCNT.FDE.

       COPY GLBTCH.FD.

      /
000420 FD  PRNREP    LABEL RECORD OMITTED
000430		     LINAGE IS WS-PGE-LENGTH.
000440 01  R-L1.
000450     03  R-DET       PIC  X(132).
000770 01  R-L2.
000780     03  R-H1        PIC X(06).
000790	   03  R-DTE1	   PIC Z9/99/9999.
000800	   03  FILLER	   PIC X(08).
000810     03  R-H2        PIC X(40).
000820     03  R-H2A       PIC X(27).
000830     03  FILLER      PIC X(10).
000840     03  R-H3        PIC X(07).
000850     03  R-BTCH      PIC Z(04)9.
000860     03  FILLER      PIC X(10). 
000870     03  R-H4        PIC X(06).
000880     03  R-PGE       PIC ZZ9.
000570 01  R-L3.
000580     03  FILLER      PIC  X(03).
000590     03  R-H5        PIC  X(11).
000600     03  R-H6        PIC  X(05).
000610     03  FILLER      PIC  X(01).
000620     03  R-H7        PIC  X(13).
000630     03  R-H8        PIC  X(05).
000640     03  FILLER      PIC  X(06).
000650     03  R-H9        PIC  X(09).
000660     03  R-H10       PIC  X(12).
000670     03  R-H11       PIC  X(13).
000680     03  FILLER      PIC  X(01).
000690     03  R-H12       PIC  X(09).
000700     03  R-H13       PIC  X(13).
000710     03  FILLER      PIC  X(08).
000720     03  R-H14       PIC  X(14).
000730     03  R-H15       PIC  X(09).
000740 01  R-L4.
000750     03  FILLER      PIC  X(03).
000760     03  R-ACC       PIC  9(04)/99.
000770     03  FILLER      PIC  X(04).
000780     03  R-ANME      PIC  X(24).
000790	   03  FILLER	   PIC	X(03).
000800	   03  R-DTE2	   PIC	Z9/99/9999.
000810	   03  FILLER	   PIC	X(03).
	   03  R-REFER.
000820	   05  R-REF	   PIC	Z(07).
000830     03  FILLER      PIC  X(04).
000840     03  R-NAR       PIC  X(36).
001170     03  FILLER      PIC X(01).
           03  R-DBRKO     PIC X(01).
001180     03  R-DBT       PIC Z(08)9.99.
           03  R-DBRKC     PIC X(01).
           03  R-CBRKO     PIC X(01).
001200     03  R-CRD       PIC Z(08)9.99-.
           03  R-CBRKC     PIC X(01).
001210     03  FILLER      PIC X(01).
000900 01  R-L5.
000910     03  FILLER      PIC  X(65).
000920     03  R-H16       PIC  X(10).
000930     03  FILLER      PIC  X(01).
000940     03  R-H17       PIC  X(15).
000950     03  R-H18       PIC  X(10).
000960     03  FILLER      PIC  X(31).
000970 01  R-L6.
000980     03  FILLER      PIC  X(08).
000990     03  R-H19       PIC  X(11).
001000     03  R-H20       PIC  X(05).
001010     03  FILLER      PIC  X(01).
001020     03  R-H21       PIC  X(13).
001030     03  R-H22       PIC  X(05).
001040     03  FILLER      PIC  X(04).
001050     03  R-H23       PIC  X(06).
001060     03  FILLER      PIC  X(01).
001070     03  R-H24       PIC  X(12).
001080     03  R-H25       PIC  X(05).
001090     03  FILLER      PIC  X(06).
001100     03  R-H26       PIC  X(14).
001110     03  R-H27       PIC  X(10).
001120     03  R-H28       PIC  X(15).
001130     03  R-H29       PIC  X(16).
001140 01  R-L7.
001150     03  FILLER      PIC  X(08).
001160     03  R-DAC       PIC  9(04)/99.
001170     03  FILLER      PIC  X(04).
001180     03  R-ANAME     PIC  X(24).
001190     03  FILLER      PIC  X(04).
001200     03  R-BRANCH    PIC  X(24).
001210	   03  FILLER	   PIC	X(02).
001220	   03  R-DTE3	   PIC	Z9/99/9999.
001230     03  FILLER      PIC  X(02).
001240     03  R-DEBT      PIC  Z(08)9.99.
001250     03  FILLER      PIC  X(04).
001260     03  R-BNKAC     PIC  9(04)/99.
001270     03  FILLER      PIC  X(02).
001280     03  R-CRED      PIC  Z(08)9.99-.
001290     03  FILLER      PIC  X(09).
001300 01  R-BL2.
001310     03  FILLER      PIC  X(20).
001320     03  R-BH1       PIC  X(34).
001330     03  R-BH2       PIC  X(36).
001340     03  R-BH3       PIC  X(42).
001350 01  R-BL3.
001360     03  R-BH4.
001370         05  R-BAC   PIC  9(06).
001380         05  FILLER  PIC  X(01).
001390         05  R-BDESC PIC  X(30).
001400     03  FILLER      PIC  X(95).
001410 01  R-BL3A.
001420     03  FILLER      PIC  X(06). 
001430     03  R-BH5       PIC  X(10).
001440     03  R-BH6       PIC  X(10).
001450     03  R-BH7       PIC  X(10).
001460     03  R-BH8       PIC  X(10).
001470     03  R-BH9       PIC  X(10).
001480     03  R-BH10      PIC  X(10).
001490     03  R-BH11      PIC  X(10).
001500     03  R-BH12      PIC  X(10).
001510     03  R-BH13      PIC  X(10).
001520     03  R-BH14      PIC  X(10).
001530     03  R-BH15      PIC  X(10).
001540     03  R-BH16      PIC  X(10).
001550     03  R-BH17      PIC  X(06).
001560 01  R-BL4.
001570     03  FILLER      PIC  X(02).
001580     03  R-BH18      PIC  X(08).
001590     03  R-BH19      PIC  X(18).
001600     03  FILLER      PIC  X(104).
001610 01 R-BL4A.
001620     03  FILLER      PIC  X(02). 
001630     03  R-BVAL      PIC  Z(08)9- OCCURS 13.
001640 01  R-BL5.
001650     03  FILLER      PIC  X(02).
001660     03  R-BH20      OCCURS 13.
001670         05  R-BH21  PIC   X(09).
001680         05  FILLER  PIC  X(01).
      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001700 77  WS-PARKEY	   PIC	9(04)	 COMP-5 VALUE 1.
001710 77  WS-GLKEY        PIC  9(06)    COMP-5 VALUE 1.
001720 77  WS-GTRKEY       PIC  9(06)    COMP-5 VALUE 1.
001720 77  WS-LTRKEY       PIC  9(06)    COMP-5 VALUE 0.
001730 77  WS-GINKEY       PIC  9(04)    COMP-5 VALUE 1.
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
001830 77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
001840 77  WS-ISUB         PIC  9(04)    COMP-5.
001850 77  WS-CHEQUE       PIC  9(04)    COMP-5.
001860 77  WS-PAGE         PIC  9(04)    COMP-5.
001870 77  WS-LINES        PIC  9(04)    COMP-5 VALUE 66.
001880 77  WS-SUB          PIC  9(04)    COMP-5.
001880 77  WS-S            PIC  9(04)    COMP-5.
001880 77  WS-S1           PIC  9(04)    COMP-5.
001890 77  WS-S2           PIC  9(04)    COMP-5.
001890 77  WS-S3           PIC  9(04)    COMP-5.
001890 77  WS-S4	   PIC	9(04)	 COMP-5.
001890 77  WS-S5	   PIC	9(04)	 COMP-5.
001890 77  WS-S6           PIC  9(04)    COMP-5.
001890 77  WS-S7	   PIC	9(04)	 COMP-5.
001890 77  WS-S8	   PIC	9(04)	 COMP-5.
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
001980 77  WS-FIRST        PIC  9(01) VALUE ZERO.
001990 77  WS-LAST         PIC  9(01) VALUE ZERO.
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
002090 77  WS-SELECT       PIC  X(13) VALUE "Select Option".
003150 77  WS-PRN-NO	   PIC X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC X(01)	 COMP-X.
002100 77  WS-H1	   PIC	X(05) VALUE "DATE:".
002110 77  WS-H3           PIC  X(06) VALUE "BATCH:".
002120 77  WS-H4           PIC  X(05) VALUE "PAGE:".
002130 77  WS-CHK          PIC  X(30) VALUE
002140                     "Correct ENTER - N if Incorrect".
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

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002330 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

000320 COPY GACCNT.ID.

000340 COPY GLBTCH.ID.

       COPY PARAM.ID.

       01  W02-FILE-IDS.
           03  W02-PRINTER.
	       05  W02-REPORT PIC X(07) VALUE "GLP063.".
               05  W02-USER   PIC X(05) VALUE SPACES.

       COPY W05.GL.

002370 01  W10-H2.
002380     03  FILLER      PIC X(02) VALUE "-".
002390     03  W10-HEAD    PIC X(25).

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
           03  W75-SKEY       PIC 9(04) COMP-5
                              OCCURS 18.

004540 01  W90-COMP.
004550     03  W90-CNAME   PIC  X(40).

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004560 SCREEN SECTION.

       COPY BLANK.CRT.

       01  S06.
005490	   03  LINE 3 COLUMN 26 FOREGROUND-COLOR 7 HIGHLIGHT
005500		      VALUE "G/L (JOURNAL) - PRINT A BATCH".
005510	   03  LINE 5 COLUMN  3 VALUE "Batch Id:".
	   03	      COLUMN 13 FOREGROUND-COLOR 3 HIGHLIGHT
				BACKGROUND-COLOR 5
				PIC  X(14) FROM W02-GLBTCH.
	   03  LINE 6 COLUMN  3 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
	       "Key in a one character batch reference code".

005570 01  S06A.
005510	   03  LINE 5 COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
				BACKGROUND-COLOR 5
				PIC  X(01) USING W02-BCH41 AUTO.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
006540 AA000	       SECTION.
006550 AA00.
003240	   IF LS0-GLLEV < 1
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA35.
006560	     PERFORM ZA000.
	     PERFORM DA000.

	     CLOSE GLBTCH
007210		   PRNREP.
007240 AA35.
             EXIT PROGRAM.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

007600 AB000	       SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.

       COPY GACCNT.RD.
 
       COPY GLBTCH.RD.

      /
008790 AC000           SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

       COPY GACCNT.WR.

       COPY GLBTCH.WR.

      /
      *    ****    P R I N T   B A T C H
      *
011620 DA000	       SECTION 50.
       DA0.
	     DISPLAY CLR-SCREEN.
	     DISPLAY S06.
       DA0A.
	     ACCEPT S06A.
	   IF W02-BCH41 = SPACE
	       MOVE "1" 	 TO W02-BCH41
	       GO TO DA0A.
044300	     OPEN I-O GLBTCH.
           IF WS-STATUS = "05"
               CLOSE GLBTCH
               OPEN OUTPUT GLBTCH
               CLOSE GLBTCH
               OPEN I-O GLBTCH.
       DA0B.
044310     IF RUNTIME-ERROR
               IF FLE-LOCKED
036270		   DISPLAY "Files locked - Try later" AT 2512
036280			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
				 " " WS-STATUS
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15
036290		   DISPLAY "Press any key" AT 2512
036300			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
036310		   ACCEPT WS-OPTION AT 2526
			  WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 7 HIGHLIGHT AUTO
040420		   GO TO DA999
               ELSE
               IF FLE-LIMIT
		   DISPLAY "Too many files OPEN" AT 0812
			    WITH FOREGROUND-COLOR 14
		   DISPLAY "Check the FILES option in CONFIG.SYS"
			    AT 1012
		   DISPLAY "Press any key to continue" AT 1212
		   ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15
040420		   GO TO DA999
	       ELSE
               IF IDX-CORRUPT
                   CLOSE GLBTCH
                   OPEN OUTPUT GLBTCH
                   CLOSE GLBTCH
                   OPEN I-O GLBTCH
		   GO TO DA0B.
050190     IF WS-STATUS NOT = "00"
050200         MOVE 41           TO WS-F-ERROR
050210         PERFORM OPEN-ERROR.
011670 DA00.
011680       DISPLAY CLR-SCREEN.
012910       DISPLAY "PRINT BATCH" AT 0234
012920                         WITH FOREGROUND-COLOR 15.
012930	     DISPLAY "Are you sure (" AT 0610
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT ")".
012950	     ACCEPT WS-OPTION AT 0629
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
013010	   IF WS-OPTION = "N"
013020         GO TO DA999.
             INITIALIZE GLB-REC.
             PERFORM START-AT-GLBTCH-KEY THRU READ-GLBTCH-EXIT.
           IF WS-F-ERROR = 41
	       MOVE "No entries to be posted"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO DA999.
013390       MOVE ZERO           TO WS-PAGE W20-DEBIT W20-CREDIT.
013400       MOVE "BATCH EDIT LIST" 
                                 TO W10-HEAD.
013410       MOVE W05-TODAY      TO W15-DATE.
013420       DISPLAY "Date     :" AT 1210.
013430       DISPLAY "Account  :" AT 1410.
013440       DISPLAY "Narrative:" AT 1610.
013440       DISPLAY "Value    :" AT 1810.
013460       MOVE WS-COND        TO R-DET.
013470       WRITE R-L1 BEFORE 0 LINES.
       DA10.
005460	     MOVE SPACES	 TO R-DET.
005470       MOVE WS-H1          TO R-H1.
005480       MOVE W90-COMP       TO R-H2.
005490       MOVE W10-H2         TO R-H2A.
005500       MOVE WS-H3          TO R-H3.
005510       MOVE WS-H4          TO R-H4.
005520       ADD 1               TO WS-PAGE.
005530       MOVE WS-PAGE        TO R-PGE.
005540       MOVE WS-BATCH       TO R-BTCH.
005550       MOVE W05-TODAY      TO R-DTE1.
005560       WRITE R-L1 BEFORE 2 LINES.
005570       MOVE SPACES         TO R-DET.
013500	     MOVE "ACCNT NO"	 TO R-H5.
013510       MOVE ALL "_"        TO R-H6.
013520       MOVE "ACCOUNT NAME" TO R-H7.
013530       MOVE ALL "_"        TO R-H8.
013540       MOVE "DATE"         TO R-H9.
013550       MOVE "REFERENCE"    TO R-H10.
013560       MOVE ALL "_"        TO R-H11.
013570       MOVE "NARRATION"    TO R-H12.
013580       MOVE ALL "_"        TO R-H13.
013590       MOVE "DEBIT"        TO R-H14.
013600       MOVE "CREDIT"       TO R-H15.
013610       WRITE R-L1 BEFORE 2 LINES.
013620       MOVE SPACES         TO R-DET.
013630 DA15.
             PERFORM READ-GLBTCH-NEXT THRU READ-GLBTCH-EXIT.
           IF WS-F-ERROR = 41
               GO TO DA20.  
      *
      *    ****    C H E C K   G / L   A C C O U N T   N O .
      *
             MOVE GLB-AC         TO W15-ACCOUNT.
	     MOVE GLB-REFER	 TO W15-REFER.
             MOVE GLB-DATE       TO W15-YMD.
             MOVE W15-YR         TO W15-YY.
             MOVE W15-MT         TO W15-MM.
             MOVE W15-DY         TO W15-DD.
             MOVE GLB-NAR        TO W15-NAR.
             MOVE GLB-VALUE      TO W15-VAL.
013940       MOVE ZERO           TO WS-ERROR.
013950	     DISPLAY W15-ACCOUNT AT 1422
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
008970	     MOVE W15-ACCOUNT	 TO G-AC.
008980       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
012040	   IF WS-F-ERROR = 8
014060	       PERFORM DA30 THRU DA40
014070         GO TO DA15.
             MOVE W15-DATE       TO W25-DTE.
013450	     DISPLAY W25-DATE AT 1221
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
014000	     DISPLAY G-NAME AT 1430
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     DISPLAY W15-NAR AT 1622
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
014110       MOVE W15-VAL        TO W25-V1.
014120	     DISPLAY W25-S7V2 AT 1822
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
      *
      *    ****    PRINT TRANSACTION
      *
014130       MOVE G-AC           TO R-ACC.
014140       MOVE G-NAME         TO R-ANME.
014150       MOVE W15-NAR        TO R-NAR.
014160	     MOVE W15-REFER	 TO R-REFER.
014170       MOVE W15-DATE       TO R-DTE2.
014180     IF W15-VAL < ZERO
014190         MOVE W15-VAL      TO R-CRD
014200     ELSE
014210         MOVE W15-VAL      TO R-DBT.
014430     IF W15-VAL < ZERO
014440         ADD W15-VAL       TO W20-CREDIT
014470     ELSE
014480         ADD W15-VAL       TO W20-DEBIT.
             MOVE W15-VAL        TO WS-VALUE.
014570	   IF LINAGE-COUNTER < WS-PRN-LENGTH
014580         WRITE R-L1 BEFORE 1 LINES
014590     ELSE
014600         WRITE R-L1 BEFORE PAGE
014610         PERFORM DA10.
014620       MOVE SPACES TO R-DET.
             GO TO DA15.
016960 DA20.
017020	   IF LINAGE-COUNTER < (WS-PRN-LENGTH - 1)
017030         WRITE R-L1 BEFORE 1 LINES
017040     ELSE
017050         WRITE R-L1 BEFORE PAGE
017060	       PERFORM DA10.
017070       MOVE "*****   T O T A L S   *****" 
                                 TO R-NAR.
017080       MOVE W20-DEBIT      TO R-DBT.
017090       MOVE W20-CREDIT     TO R-CRD.
017100       WRITE R-L1 BEFORE PAGE.
017110       MOVE WS-NORM        TO R-DET.
017120       WRITE R-L1 BEFORE 0 LINES.
             GO TO DA999.
       DA30.
             MOVE W15-ACCOUNT    TO R-ACC.
             MOVE "*** INVALID ACCOUNT NUMBER"
                                 TO R-ANME.
014150       MOVE W15-NAR        TO R-NAR.
014160	     MOVE W15-REFER	 TO R-REFER.
014170       MOVE W15-DATE       TO R-DTE2.
017620 DA35.
014180     IF GLB-VALUE < ZERO
014190         MOVE GLB-VALUE    TO R-CRD
               MOVE "["          TO R-CBRKO
               MOVE "]"          TO R-CBRKC
014200     ELSE
014210         MOVE GLB-VALUE    TO R-DBT
               MOVE "["          TO R-DBRKO
               MOVE "]"          TO R-DBRKC.
017630	   IF LINAGE-COUNTER < (WS-PRN-LENGTH - 1)
017640         WRITE R-L1 BEFORE 1 LINES
017650     ELSE
017660         WRITE R-L1 BEFORE PAGE
017670	       PERFORM DA10.
017680       MOVE SPACES TO R-DET.
017690       MOVE ZERO TO WS-ERROR.
017700 DA40.
017710       EXIT.
       DA999.
             EXIT.
      /
035270 ZA000	     SECTION 90.
035280 ZA00.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
049200	     DISPLAY CLR-SCREEN.
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
	     MOVE "GLCHEQ"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GLBTCH.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	   IF LS-USER NUMERIC
	       IF LS-USE > ZERO AND < 110
		   MOVE LS-USE	 TO WS-USUB
                   GO TO ZA01A.
             MOVE 110            TO WS-USUB.

       ZA01A.
014240       ADD 2549 WS-USUB    GIVING W25-RESULT.
             MOVE W25-KEY        TO WS-PARKEY.
             ADD 1 W25-SUB       GIVING WS-S6.
014250       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF TRM-STDP (WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
	       MOVE ZERO	 TO WS-PRN-NO
           ELSE
           IF TRM-STDP (WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
	       MOVE 1		 TO WS-PRN-NO
           ELSE
           IF TRM-STDP (WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
	       MOVE 2		 TO WS-PRN-NO
	   ELSE
           IF TRM-STDP (WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 9
	       MOVE LS-USER	 TO W02-USER.
             ADD 501 TRM-PRN3 (WS-S6)
                                 GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S6.
             ADD 1               TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE TRM-CONP (WS-S6)
                                 TO WS-COND.
             MOVE TRM-NORP (WS-S6)
                                 TO WS-NORM.
             MOVE TRM-EXPP (WS-S6)
                                 TO WS-EXPP.
             MOVE TRM-ECAN (WS-S6)
                                 TO WS-ECAN.
             MOVE TRM-8LPI (WS-S6)
                                 TO WS-8LPI.
             MOVE TRM-6LPI (WS-S6)
                                 TO WS-6LPI.
             MOVE TRM-10CPI (WS-S6)
                                 TO WS-10CPI.
             MOVE TRM-12CPI (WS-S6)
                                 TO WS-12CPI.
             MOVE TRM-17CPI (WS-S6)
                                 TO WS-17CPI.
	     MOVE TRM-LENGTH (WS-S6)
				 TO WS-PRN-LENGTH.
	     MOVE TRM-PAGE (WS-S6)
				 TO WS-PGE-LENGTH.
042170 ZA02.
035680	     MOVE 1		 TO WS-PARKEY.
035690       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
035700	     MOVE PAR-COMPANY	 TO W90-CNAME.
035710	     MOVE PAR-DMY	 TO W05-TODAY.
035720	     MOVE PAR-YMD	 TO W05-TYMD.
	     MOVE 6		 TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-SUSGL	 TO W70-SUSGL.
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
035920         PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
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
036050         DISPLAY "Parameters not set up - ENTER" AT 2020
036060                  WITH FOREGROUND-COLOR 14
036070         ACCEPT WS-OPTION AT 2050 WITH FOREGROUND-COLOR 15 AUTO
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
	   IF WS-PRN-NO < 3
	       PERFORM ZA210 THRU ZA215
	       IF WS-ERROR = 1
		   GO TO ZA205
	   ELSE
	       DISPLAY "Waiting for printer response" AT 1412
			WITH FOREGROUND-COLOR 14 BLINK.
036240	     OPEN OUTPUT PRNREP.
	     CALL X"91" USING X91-RES X91-FUN PRNREP.
036250       GO TO ZA999.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
       ZA50.
041850     IF WS-F-ERROR = 11
               MOVE "G/L TRAN file"
                                 TO WS-FILE
           ELSE
041850     IF WS-F-ERROR = 40
041860         MOVE "INTEGRATION"
                                 TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR 14.
             DISPLAY "Press any key to continue" AT 1012.
             ACCEPT WS-OPTION AT 1038 WITH FOREGROUND-COLOR 15.
       ZA51.
             EXIT PROGRAM.
       ZA55.
041220       MOVE 1 TO WS-S1.
021870	     MOVE SPACES TO WS-MID-LNE.
041240 ZA60.
041250       MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1 TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9  TO WS-TCH  (1).
             MOVE WS-G10 TO WS-TCH  (80).
             MOVE WS-G11 TO WS-BCH  (1).
             MOVE WS-G12 TO WS-BCH  (80).
             MOVE WS-G14 TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13 TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2  TO WS-TCHR (11) WS-TCHR (45)
021940                      WS-TCHR (63).
021950       MOVE WS-G3  TO WS-MCHR (11) WS-MCHR (45)
021960                      WS-MCHR (63)
			    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4  TO WS-BCHR (11) WS-BCHR (45)
021980                      WS-BCHR (63).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
036260 ZA200.
036270       DISPLAY "Files locked - Try later" AT 2512
036280		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " " WS-STATUS
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15.
036290       DISPLAY "Press any key" AT 2512 
036300		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
036310	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
       ZA210.
	     MOVE ZERO		 TO WS-ERROR
				    WS-S1 WS-S2 WS-S3 WS-S4
				    WS-S5 WS-S6 WS-S7 WS-S8.
	     CALL "PC_TEST_PRINTER" USING WS-PRN-NO
					  WS-PRN-STAT
				    RETURNING WS-STATUS.
	   IF WS-PRN-STAT > 127
	       MOVE 1		 TO WS-S1
	       SUBTRACT 128	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 63
	       MOVE 1		 TO WS-S2
	       SUBTRACT 64	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 31
	       MOVE 1		 TO WS-S3
	       SUBTRACT 32	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 15
	       MOVE 1		 TO WS-S4
	       SUBTRACT 16	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 7
	       MOVE 1		 TO WS-S5
	       SUBTRACT 8	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 3
	       MOVE 1		 TO WS-S6
	       SUBTRACT 4	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 1
	       MOVE 1		 TO WS-S7
	       SUBTRACT 2	 FROM WS-PRN-STAT.
	     MOVE WS-PRN-STAT	 TO WS-S8.
	   IF WS-S3 = 0 AND
	      WS-S5 = 0 AND
	      WS-S8 = 0
	       GO TO ZA215.
	     DISPLAY "Check printer - " AT 2304
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "R" WITH FOREGROUND-COLOR 7 HIGHLIGHT
		     "etry - "
		     "A" WITH FOREGROUND-COLOR 7 HIGHLIGHT
		     "bort".
       ZA212.
	     ACCEPT WS-OPTION AT 2334
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     IF NOT (WS-OPTION = "A" OR "R")
		 GO TO ZA212.
	     IF WS-OPTION = "A"
		 MOVE 1 	 TO WS-ERROR
		 GO TO ZA215.
	       GO TO ZA210.
       ZA215.
	     EXIT.
036340 ZA999.
036350       EXIT.
      /
036360 ZB000	     SECTION 91.
      
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
041820         MOVE W02-GACCNT TO WS-FILE
041830         MOVE WS-GLKEY TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870         MOVE WS-PARKEY TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 41
041860         MOVE W02-GLBTCH   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE GLB-KEY      TO WS-KEYX..
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
