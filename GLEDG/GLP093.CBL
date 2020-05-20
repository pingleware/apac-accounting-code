      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   **	      *******	  ****	   ******    ******    *
      *   **	**  **	      **    **	 **  **   **	**  **	  **   *
      *   **	    **	      **    **	**    **  **	**	  **   *
      *   **	    **	      *******	**    **   *******     ****    *
      *   **   ***  **	      **	**    **	**	  **   *
      *   **	**  **	      **	 **  **   **	**  **	  **   *
      *    ******   ********  **	  ****	   ******    ******    *
      * 							       *
      *     ENGLISH                                                    *
      *                                                                *
      *     G / L E D G E R   B A L A N C I N G   U T I L I T I E S    *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	       GLP093.
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

000180 COPY GLTRAN.SL.

000180 COPY GLTRNB.SL.

000110	   SELECT GLBTCH ASSIGN DISK
			 LOCK MANUAL
000220                   STATUS WS-STATUS
000230                   ACCESS DYNAMIC
000240                   ORGANIZATION INDEXED
000250			 RECORD B-KEY
			 ALTERNATE RECORD B-VAL DUPLICATES.

000270 DATA DIVISION.
000280 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000300 COPY PARAM.FDE.

000320 COPY GACCNT.FDE.

000340 COPY GLTRAN.FDE.

000340 COPY GLTRNB.FDE.

      /
002000 FD  GLBTCH    LABEL RECORD IS STANDARD
002010		     VALUE OF FILE-ID IS W02-GLBTCH.
002020 01  B-REC.
	   03  B-KEY.
002160	       05  B-AC        PIC  9(06).
002030	       05  B-DATE      PIC  9(08).
002040	       05  B-DTE REDEFINES B-DATE.
002050		   07  B-CC    PIC  9(02).
002050		   07  B-YY    PIC  9(02).
002060		   07  B-MM    PIC  9(02).
002070		   07  B-DD    PIC  9(02).
	       05  B-AKEY.
002120		   07  B-CODE  PIC  9(02).
		   07  B-REFER.
002110		       09  B-REF
                               PIC  9(07).
		   07  B-SEQ   PIC  9(02).
	       05  B-SUB       PIC  9(01).
002080	   03  B-NAR	       PIC  X(40).
002090	   03  B-DESC REDEFINES B-NAR.
002100	       05  B-NARATIVE  PIC  X(36).
               05  FILLER      PIC  9(07)    COMP-3.
002130	   03  B-VAL	       PIC S9(09)V99 COMP.
002170	   03  B-FLAG	       PIC  X(01).
	   03  B-BATCH	       PIC  9(06)    COMP.

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
001770 77  WS-INKEY	   PIC	9(06)	 COMP-5.
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
001890 77  WS-S9	   PIC	9(04)	  COMP-5.
001890 77  WS-IXS1	   PIC	9(04)	  COMP-5.
001900 77  WS-Y-END        PIC  9(02) VALUE ZERO.
001900 77  WS-L-END	   PIC	9(04) VALUE ZERO.
001900 77  WS-OPEN         PIC  9(02) VALUE ZERO.
001910 77  WS-MS1          PIC  9(04)    COMP-5.
001920 77  WS-MS2          PIC  9(04)    COMP-5.
001750 77  WS-START	   PIC	9(06)	 COMP-3.
001760 77  WS-END	   PIC	9(06)	 COMP-3.
001930 77  WS-VALUE	   PIC S9(09)V99 COMP-3.
001940 77  WS-ENDINP	   PIC	9(01).
001950 77  WS-CONREC       PIC  9(01) VALUE ZERO.
001960 77  WS-INDG         PIC  9(01) VALUE ZERO.
001970 77  WS-OPTION       PIC  X(01).
001970 77  WS-BAL-TYPE	   PIC	X(01).
001980 77  WS-FIRST        PIC  9(02) VALUE ZERO.
001990 77  WS-LAST         PIC  9(02) VALUE ZERO.
002010 77  WS-ERROR        PIC  9(01) VALUE ZERO.
002020 77  WS-NUM          PIC  Z9.
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
       77  WS-DATE	   PIC	9(06)	 COMP-3.
002070 77  WS-BATCH	   PIC	9(06)	 COMP-3.
002080 77  WS-TNEXT        PIC  9(04)    COMP-5.
002080 77  WS-PERIOD       PIC  9(02)    COMP-5 VALUE ZERO.
003150 77  WS-PRN-NO	   PIC	X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC	X(01)	 COMP-X.
002100 77  WS-H1	   PIC	X(05) VALUE "DATE:".
002110 77  WS-H3           PIC  X(06) VALUE "BATCH:".
002120 77  WS-H4           PIC  X(05) VALUE "PAGE:".
002150 77  WS-ER1	   PIC	X(07) VALUE "Account".
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
	       05  FILLER      PIC  X(01) VALUE "�".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "�".
	       05  FILLER      PIC  X(01) VALUE "�".
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
	   03  WS-PROG	       PIC  X(03) VALUE "093".

002020 01  WT-REC.
	   03  WT-KEY.
002160	       05  WT-AC	PIC  9(06).
002030	       05  WT-DATE	PIC  9(08).
002040	       05  WT-DTE REDEFINES WT-DATE.
002050		   07  WT-CC	PIC  9(02).
002050		   07  WT-YY	PIC  9(02).
002060		   07  WT-MM	PIC  9(02).
002070		   07  WT-DD	PIC  9(02).
	       05  WT-AKEY.
002120		   07  WT-CODE	PIC  9(02).
		   07  WT-REFER.
002110		       09  WT-REF
				PIC  9(07).
		   07  WT-SEQ	PIC  9(02).
	       05  WT-SUB	PIC  9(01).
002080	   03  WT-NAR		PIC  X(40).
002090	   03  WT-DESC REDEFINES WT-NAR.
002100	       05  WT-NARATIVE	PIC  X(36).
		05  FILLER	PIC  9(07)    COMP-3.
002130	   03  WT-VAL		PIC S9(09)V99 COMP-3.
002170	   03  WT-FLAG		PIC  X(01).
	   03  WT-BATCH	       PIC  9(06)    COMP-3.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002330 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

000320 COPY GACCNT.ID.

000340 COPY GLTRAN.ID.

000340 COPY GLTRNB.ID.

000970 COPY GLBATCH.ID.

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

000690	   03  R-L1 REDEFINES W02-PRN-LINE.
000700	       05  R-DET       PIC  X(136).

000710	   03  R-L2 REDEFINES W02-PRN-LINE.
000780	       05  R-H1	       PIC  X(06).
000790	       05  R-DTE1      PIC  Z9/99/9999.
000800	       05  FILLER      PIC  X(08).
000810	       05  R-H2	       PIC  X(40).
000820	       05  R-H2A       PIC  X(27).
000830	       05  FILLER      PIC  X(10).
000840	       05  R-H3	       PIC  X(07).
000850	       05  R-BTCH      PIC  Z(04)9.
000860	       05  FILLER      PIC  X(10).
000870	       05  R-H4	       PIC  X(06).
000880	       05  R-PGE       PIC  ZZ9.

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
001080	       05  R-ACC       PIC  9(04)/99.
001090	       05  FILLER      PIC  X(02).
001100	       05  R-ANME      PIC  X(24).
001110	       05  FILLER      PIC  X(02).
001120	       05  R-DTE2      PIC  Z9/99/9999.
001130	       05  FILLER      PIC  X(02).
002680	       05  R-CDE       PIC  X(04).
	       05  FILLER      PIC  X(02).
	       05  R-BATCH     PIC  Z(06).
002690	       05  FILLER      PIC  X(02).
	       05  R-REFER.
001140		   07  R-REF   PIC  Z(07).
001150	       05  FILLER      PIC  X(02).
001160	       05  R-NAR       PIC  X(36).
001170	       05  FILLER      PIC  X(01).
001180	       05  R-DBT       PIC  Z(08)9.99.
	       05  FILLER      PIC  X(01).
001200	       05  R-CRD       PIC  Z(07)9.99-.

000990	   03  R-L5 REDEFINES W02-PRN-LINE.
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
003260	   03  W20-ACCOUNT   PIC  9(06).
003270     03  W20-ACNO1   REDEFINES W20-ACCOUNT.
003280	       05  W20-ACNO  PIC	9(04).
003300	       05  W20-SER   PIC  9(02).
003310	   03  W20-ACNO2     REDEFINES W20-ACCOUNT.
003320	       05  W20-GR    PIC  9(02).
003330	       05  W20-SGRP  PIC 9(02).
003350	       05  W20-ASER  PIC 9(02).
003370	   03  W20-ACNO3     REDEFINES W20-ACCOUNT.
003380	       05  W20-GROUP PIC 9(02).
003400	       05  W20-REST  PIC 9(04).

       COPY W20.WS.

002850 01  W25-CALCS.
002860	   03  W25-RESULT      PIC  9(05)V99.
002870     03  W25-RESULT1 REDEFINES W25-RESULT.
002880	       05  W25-WHOLE   PIC  9(05).
002890	       05  W25-DECIMAL PIC  9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
	       05  W25-KEY     PIC  9(04).
	       05  W25-SUB     PIC  9(01).
	       05  FILLER      PIC  9(02).
003420 01  W25-EDIT.
003430     03  W25-7.
003440	       05  W25-1       PIC  ZZZZZZZZ9-.
003450     03  W25-S7V2.
003460	       05  W25-V1      PIC  Z(07)9.99CR.
003990	   03  W25-S8V2.
004000	       05  W25-VG1     PIC  Z(07)9.99CR.
003470	   03  W25-DATE.
003480	       05  W25-DTE     PIC  Z9/99/9999.
003490     03  W25-REF.
003500	       05  W25-REFER   PIC  Z(07).
003520	   03  W25-RECKEY      PIC  Z(04)9.
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

004540 01  W90-COMP.
004550     03  W90-CNAME   PIC  X(40).

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004560 SCREEN SECTION.

       COPY BLANK.CRT.

006150 01  S11.
006160	   03  LINE  2 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
006170				 VALUE "BALANCING UTILITIES".
006180	   03  LINE  4 COLUMN 12 VALUE "Balance an ".
	   03	       COLUMN 23 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "A".
	   03	       COLUMN 24 VALUE "ccount,".
	   03	       COLUMN 31 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "B".
	   03	       COLUMN 32 VALUE "atch or ".
	   03	       COLUMN 40 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "R".
	   03	       COLUMN 41 VALUE "ange of batches > < ".
	   03	       COLUMN 61 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Esc".
	   03	       COLUMN 64 VALUE "ape to Exit".

       01  S17.
002970	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC 9(06) USING W15-ACCOUNT AUTO.

       01  S18.
002970	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(05)9 USING WS-BATCH AUTO.

       01  S19.
002970	   03  LINE  6 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC Z(05)9 USING WS-START AUTO.
002970	   03  LINE  7 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC Z(05)9 USING WS-END AUTO.

004870 COPY S99.CRT.

       COPY LEDLUP.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
006540 AA000	       SECTION.
006550 AA00.
003240	   IF LS0-GLLEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006560	     PERFORM ZA000.
      * 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      * 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.
	     PERFORM HA000.
	     CLOSE GLBTCH.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

       AA49.
	     EXIT PROGRAM.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

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

007600 AB000           SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.

       COPY GACCNT.RD.

       COPY GLTRAN.RD.

      *
      *     ***  *     ****  *****  ***  *   *	  ***** *** *	  *****
      *    *   * *     *   *   *   *   * *   *	  *	 *  *	  *
      *    *	 *     *   *   *   *	 *   *	  *	 *  *	  *
      *    *	 *     ****    *   *	 *****	  ***	 *  *	  ***
      *    *  ** *     *   *   *   *	 *   *	  *	 *  *	  *
      *    *   * *     *   *   *   *   * *   *	  *	 *  *	  *
      *     ***  ***** ****    *    ***  *   *	  *	*** ***** *****
      *
008370 READ-GLBTCH.
008380	     READ GLBTCH
	       KEY IS B-KEY.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-GLBTCH-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 61		 TO WS-F-ERROR
008400	       GO TO READ-GLBTCH-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 61		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
008440     IF RECORD-LOCKED
	       MOVE W02-GLBTCH	 TO WS-FILE
008450         PERFORM LOCKED-RECORD
008460	       GO TO READ-GLBTCH.
008470	     MOVE 61		 TO WS-F-ERROR.
008480       PERFORM READ-ERROR.
	     GO TO READ-GLBTCH-EXIT.

008370 READ-GLBTCH-NEXT.
008380	     READ GLBTCH NEXT.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-GLBTCH-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 61		 TO WS-F-ERROR
008400	       GO TO READ-GLBTCH-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 61		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
008440     IF RECORD-LOCKED
	       MOVE W02-GLBTCH	 TO WS-FILE
008450         PERFORM LOCKED-RECORD
008460	       GO TO READ-GLBTCH-NEXT.
008470	     MOVE 61		 TO WS-F-ERROR.
008480       PERFORM READ-ERROR.
	     GO TO READ-GLBTCH-EXIT.

008370 READ-GLBTCH-PREV.
008380	     READ GLBTCH PREVIOUS.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-GLBTCH-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 61		 TO WS-F-ERROR
008400	       GO TO READ-GLBTCH-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 61		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
008440     IF RECORD-LOCKED
	       MOVE W02-GLBTCH	 TO WS-FILE
008450         PERFORM LOCKED-RECORD
008460	       GO TO READ-GLBTCH-PREV.
008470	     MOVE 61		 TO WS-F-ERROR.
008480       PERFORM READ-ERROR.
	     GO TO READ-GLBTCH-EXIT.

008370 START-AT-GLBTCH-KEY.
008380	     START GLBTCH
	       KEY >= B-KEY.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-GLBTCH-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 61		 TO WS-F-ERROR
008400	       GO TO READ-GLBTCH-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 61		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
	     GO TO READ-GLBTCH-EXIT.

008370 START-AT-GLBTCH-VAL.
008380	     START GLBTCH
	       KEY >= B-VAL.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-GLBTCH-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 61		 TO WS-F-ERROR
008400	       GO TO READ-GLBTCH-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 61		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.

008490 READ-GLBTCH-EXIT.
008500       EXIT.

      /
008790 AC000           SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

       COPY GACCNT.WR.

       COPY GLTRAN.WR.

009200 WRITE-GLBTCH.
009260	     WRITE B-REC.
009270     IF WS-STATUS = "22"
009330	       ADD 1		 TO B-SEQ
	       GO TO WRITE-GLBTCH.
009270     IF WS-STAT1 NOT = "0"
009330	       MOVE 61		 TO WS-F-ERROR
009340         PERFORM WRITE-ERROR.

009350 WRITE-GLBTCH-EXIT.
009360       EXIT.

       COPY GACCNT.LUP.

       COPY APAC.HLP.

      /
024350 HA000	     SECTION 5.
024360 HA00.
024370       DISPLAY CLR-SCREEN.
024380	     DISPLAY S11.
	     MOVE "B"		 TO WS-OPTION.

       HA02.
	     ACCEPT WS-OPTION AT 0458
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO HA999
		 WHEN F3-KEY
		     MOVE "P"	 TO WS-OPTION
		     MOVE "B"	 TO WS-BAL-TYPE
		     GO TO HA22
		 WHEN OTHER
		     CALL X"E5"
		     GO TO HA02
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "A" OR "B" OR "R")
	       GO TO HA02.
	   IF WS-OPTION = "B"
	       GO TO HA15.
	   IF WS-OPTION = "R"
	       MOVE ZERO	 TO WS-ENDINP
	       PERFORM HB000
	       GO TO HA00.
       HA03.
	     DISPLAY "Account Number :" AT 0612.
024390       MOVE ZERO TO W15-ACCOUNT.

024400 HA05.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Account Lookup - "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ape to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
024410	     ACCEPT S17 AT 0629.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE ZERO	 TO W15-ACCOUNT
		     GO TO HA999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     PERFORM AA350
		 WHEN OTHER
		     CALL X"E5"
		     GO TO HA05
	       END-EVALUATE
	       DISPLAY S17 AT 0629
               IF W15-ACCOUNT = ZERO
015990		   GO TO HA05.
	     DISPLAY CLEAR-L25.
024430     IF W15-ACCOUNT = ZERO
024440         GO TO HA999.
008970	     MOVE W15-ACCOUNT	 TO G-AC.
008980       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
024460	     DISPLAY CLEAR-L25.
021840	   IF WS-F-ERROR = 8
024480	       MOVE WS-ER6	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
024490         GO TO HA05.
024500	     DISPLAY G-NAME AT 0637 WITH FOREGROUND-COLOR 3 HIGHLIGHT
					 BACKGROUND-COLOR 5.
	     PERFORM CHECK-CORRECT.
024580	   IF WS-OPTION = "N"
024590	       GO TO HA03.
	     INITIALIZE T-REC.
	     MOVE G-AC		 TO T-AC.
	     PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
           IF WS-F-ERROR = 11
	       MOVE "No transactions for this account"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO HA00.
	     MOVE ZERO		 TO W20-DEBIT W20-CREDIT.
	     DISPLAY "Reading :" AT 0812.

       HA10.
020110	     PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
	     COMMIT.
           IF (WS-F-ERROR = 11) OR
              (T-AC NOT = G-AC)
	       MOVE "A" 	 TO WS-BAL-TYPE
	       GO TO HA35.
	     DISPLAY T-KEY AT 0822
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
      *
      *    ****    I G N O R E	 S E C O N D   N A R R A T I V E
      *
	   IF T-SUB = 5
	       GO TO HA10.
	     MOVE T-KEY 	 TO B-KEY.
002080	     MOVE T-NAR 	 TO B-NAR.
002130	     MOVE T-VAL 	 TO B-VAL.
002170	     MOVE T-FLAG	 TO B-FLAG.
	     MOVE T-BATCH	 TO B-BATCH.
	     PERFORM WRITE-GLBTCH THRU WRITE-GLBTCH-EXIT.
	   IF T-VAL < ZERO
	       ADD T-VAL	 TO W20-CREDIT
	   ELSE
	       ADD T-VAL	 TO W20-DEBIT.
	     GO TO HA10.

       HA15.
	     DISPLAY "Batch Number :" AT 0612.
024390	     MOVE ZERO		 TO WS-BATCH.
	     MOVE "B"		 TO WS-BAL-TYPE.
       HA20.
024410	     ACCEPT S18 AT 0627.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO HA999
		 WHEN OTHER
		     CALL X"E5"
		     GO TO HA20
	       END-EVALUATE.
	   IF WS-BATCH = ZERO
	       GO TO HA999.
	     PERFORM CHECK-CORRECT.
024580	   IF WS-OPTION = "N"
024590	       GO TO HA20.
	     DISPLAY WS-BLNK78 AT 1202 WITH FOREGROUND-COLOR 3.
       HA22.
	   IF WS-OPTION = "P"
	       PERFORM HA60.
	     INITIALIZE T-REC.
	     PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
           IF WS-F-ERROR = 11
	       MOVE "No transactions on file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO HA00.
	     MOVE ZERO		 TO W20-DEBIT W20-CREDIT.
	     DISPLAY "Reading :" AT 0812.

       HA25.
020110	     PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
	     COMMIT.
	   IF WS-F-ERROR = 11
	       GO TO HA30.
	     DISPLAY T-KEY AT 0822
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

      ****************************************************************

	   IF WS-OPTION = "P"
	       IF T-DATE < WS-DATE
		   MOVE T-REC	 TO WT-REC
		   PERFORM DELETE-GLTRAN-REC THRU WRITE-GLTRAN-EXIT
      *		   MOVE WS-DATE  TO T-DATE
      *		   MOVE 999999	 TO T-BATCH
      *		   PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT
		   MOVE WT-REC	 TO T-REC
		   PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT
		   GO TO HA25
	       ELSE
		   GO TO HA25.

      *****************************************************************

	   IF T-BATCH NOT = WS-BATCH
	       GO TO HA25.
      *
      *    ****    I G N O R E	 S E C O N D   N A R R A T I V E
      *
       HA26.
	   IF T-SUB = 5
	       GO TO HA25.
	     MOVE T-KEY 	 TO B-KEY.
002080	     MOVE T-NAR 	 TO B-NAR.
002130	     MOVE T-VAL 	 TO B-VAL.
002170	     MOVE T-FLAG	 TO B-FLAG.
	     MOVE T-BATCH	 TO B-BATCH.
	     PERFORM WRITE-GLBTCH THRU WRITE-GLBTCH-EXIT.
	   IF T-VAL < ZERO
	       ADD T-VAL	 TO W20-CREDIT
	   ELSE
	       ADD T-VAL	 TO W20-DEBIT.
	     GO TO HA25.

       HA30.
	   IF (W20-DEBIT + W20-CREDIT) = ZERO
	       MOVE "Batch is in balance-'P'rint,'N'ext or 'E'xit"
				 TO WS-ERR-MES
	       MOVE SPACE	 TO WS-OPTION
	       PERFORM OPT-MESSAGE
	       IF USER-FUNC
		   EVALUATE KEY-CODE-1
		     WHEN ESC-KEY
			  GO TO HA999
		     WHEN OTHER	 CALL X"E5"
		   END-EVALUATE
		   GO TO HA30
	       ELSE
	       IF DATA-8BIT
		   MOVE KEY-CODE-1X  TO WS-OPTION
		   IF NOT (WS-OPTION = "E" OR "N" OR "P")
		       CALL X"E5"
		       GO TO HA30
		   ELSE
		   DISPLAY CLEAR-L25
		   IF WS-OPTION = "E"
		       GO TO HA999
		   ELSE
		   IF WS-OPTION = "N"
		       GO TO HA15
		   ELSE
		       GO TO HA35
	       ELSE
		   CALL X"E5"
		   GO TO HA30.

       HA35.
013390	     MOVE ZERO		 TO WS-PAGE.
	     MOVE ZERO		 TO W15-ACCOUNT.
	   IF WS-BAL-TYPE = "B"
013400	       MOVE "G/L BATCH LISTING"
				 TO W10-HEAD
	   ELSE
	       MOVE "G/L ACCOUNT TRANSACTIONS"
				 TO W10-HEAD.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 3		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "V"		 TO WS-OPTION.
	     DISPLAY "D" AT 1012 WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "ate or " "V" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "alue sequence ".

       HA38.
	     ACCEPT WS-OPTION AT 1036
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
024580	   IF NOT (WS-OPTION = "D" OR "V")
	       GO TO HA38.

       HA40.
005460	     MOVE SPACES	 TO R-DET.
005470       MOVE WS-H1          TO R-H1.
005480       MOVE W90-COMP       TO R-H2.
005490       MOVE W10-H2         TO R-H2A.
005510       MOVE WS-H4          TO R-H4.
005520       ADD 1               TO WS-PAGE.
005530       MOVE WS-PAGE        TO R-PGE.
	   IF WS-BAL-TYPE = "B"
005500	       MOVE WS-H3	 TO R-H3
005540	       MOVE WS-BATCH	 TO R-BTCH.
005550       MOVE W05-TODAY      TO R-DTE1.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
009140	     MOVE SPACES	 TO R-DET.
019400	     MOVE "ACNT NO"	 TO LED-H1.
019410       MOVE "ACCOUNT NAME" TO LED-H2.
019420       MOVE "DATE"         TO LED-H3.
019430       MOVE "CODE"         TO LED-H4.
             MOVE "BATCH"        TO LED-H4A.
019440       MOVE "REFER"        TO LED-H5.
019450       MOVE "PAYEE/NARRATIVE"
                                 TO LED-H6.
019460	     MOVE "DEBIT"	 TO LED-H7.
019470	     MOVE " CREDIT"	 TO LED-H8.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
013620	     MOVE SPACES	 TO R-DET.
	   IF WS-BAL-TYPE = "A"
	       MOVE G-AC	 TO R-ACC
014140	       MOVE G-NAME	 TO R-ANME W15-ACCOUNT.

       HA45.
	     INITIALIZE B-REC.
	   IF WS-OPTION = "V"
	       MOVE ZERO	 TO B-VAL
	       PERFORM START-AT-GLBTCH-VAL THRU READ-GLBTCH-EXIT
	   ELSE
	       PERFORM START-AT-GLBTCH-KEY THRU READ-GLBTCH-EXIT.
	   IF WS-F-ERROR = 61
	       GO TO HA55.

       HA50.
	     PERFORM READ-GLBTCH-NEXT THRU READ-GLBTCH-EXIT.
	   IF WS-F-ERROR = 61
	       GO TO HA55.
	   IF WS-BAL-TYPE = "B"
	       IF B-AC NOT = W15-ACCOUNT
		   MOVE B-AC	 TO W15-ACCOUNT G-AC
008980		   PERFORM READ-GACCNT THRU READ-GACCNT-EXIT
		   MOVE G-AC	 TO R-ACC
		   MOVE G-NAME	 TO R-ANME.
014170	     MOVE B-DATE	 TO W15-YMD
	     MOVE W15-CN	 TO W15-CC.
	     MOVE W15-YR	 TO W15-YY.
             MOVE W15-MT         TO W15-MM.
             MOVE W15-DY         TO W15-DD.
020280	     MOVE W15-DATE	 TO R-DTE2.
020290	   IF B-CODE = 01 OR 02
020300	       MOVE "JNL"	 TO R-CDE
020310	   ELSE
020320	   IF B-CODE = 03 OR 04
020330	       MOVE "CHEQ"	 TO R-CDE
020310	   ELSE
020320	   IF B-CODE = 05 OR 10
020330	       MOVE "CHEQ"	 TO R-CDE
020340     ELSE
020350	   IF B-CODE = 06 OR 07
020360	       MOVE "DEP"	 TO R-CDE
020340     ELSE
020350	   IF B-CODE = 08 OR 09
020360	       MOVE "EFT"	 TO R-CDE
020340     ELSE
020350	   IF B-CODE = 11 OR 12
020360	       MOVE "PMT"	 TO R-CDE
020340     ELSE
020350	   IF B-CODE = 13 OR 14
020360	       MOVE "REC"	 TO R-CDE.
	   IF WS-BAL-TYPE = "A"
	       MOVE B-BATCH	 TO R-BATCH.
014150	     MOVE B-NAR		 TO R-NAR.
	   IF B-REFER NUMERIC
	       MOVE B-REF	 TO R-REF
	   ELSE
014160	      MOVE B-REFER	 TO R-REFER.
014180	   IF B-VAL < ZERO
014190	       MOVE B-VAL	 TO R-CRD
014200     ELSE
014210	       MOVE B-VAL	 TO R-DBT.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014610	       PERFORM HA40
	       IF WS-BAL-TYPE = "B"
		   MOVE ZERO	 TO W15-ACCOUNT.
014620	     MOVE SPACES	 TO R-DET.
	     GO TO HA50.

       HA55.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "****  E N D   R E P O R T  ****"
				 TO R-NAR.
	     MOVE W20-DEBIT	 TO R-DBT.
	     MOVE W20-CREDIT	 TO R-CRD.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.
	     CLOSE GLBTCH.
	     OPEN OUTPUT GLBTCH.
	     CLOSE GLBTCH.
	     OPEN I-O GLBTCH.

       HA58.
	     GO TO HA00.

       HA60.
	     DISPLAY "Date (YYMMDD) :" AT 0612.
	     DISPLAY "NB. Keep transactions from this date" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT.
024390	     MOVE ZERO		 TO WS-DATE.
	     ACCEPT WS-DATE AT 0628
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     PERFORM CHECK-CORRECT.
024580	   IF WS-OPTION = "N"
024590	       GO TO HA60.
	     MOVE "P"	       TO WS-OPTION.

       HA65.
	     EXIT.

025200 HA999.
025210       EXIT.

       HB000	     SECTION 6.
       HB00.
	     OPEN OUTPUT GLTRNB.
	     CLOSE GLTRNB.
	     OPEN I-O GLTRNB.
	     DISPLAY "Starting Batch :" AT 0612.
	     DISPLAY "Ending   Batch :" AT 0712.
024390	     MOVE ZERO		 TO WS-START WS-END.
	     MOVE "B"		 TO WS-BAL-TYPE.
       HB05.
	     ACCEPT S19.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO HB999
		 WHEN OTHER
		     CALL X"E5"
		     GO TO HB05
	       END-EVALUATE.
	   IF WS-START = ZERO
	       GO TO HB999.
	   IF WS-START > WS-END
	       MOVE "Start greater than End batch"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO HB05.
	     DISPLAY CLEAR-L25.
	     PERFORM CHECK-CORRECT.
024580	   IF WS-OPTION = "N"
024590	       GO TO HB05.
	     INITIALIZE T-REC.
	     PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
           IF WS-F-ERROR = 11
	       MOVE "No transactions on file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO HB999.
	     DISPLAY "Extracting :" AT 0912.
       HB10.
020110	     PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
	     COMMIT.
	   IF WS-F-ERROR = 11
	       GO TO HB15.
	     DISPLAY T-KEY AT 0925
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF T-BATCH < WS-START OR
	      T-BATCH > WS-END
	       GO TO HB10.
      *
      *    ****    I G N O R E	 S E C O N D   N A R R A T I V E
      *
	   IF T-SUB = 5
	       GO TO HB10.
	     MOVE T-REC 	 TO TB-REC.
	     WRITE TB-REC.
	     GO TO HB10.

       HB15.
	     MOVE ZERO		 TO WS-BATCH.
	     DISPLAY "Reading    :" AT 0912.
	     DISPLAY "Batch :" AT 0950.
	     INITIALIZE TB-REC.
008380	     START GLTRNB
	       KEY >= TB-BKEY.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO HB20.
008430	     GO TO HB900.

       HB20.
008380	     READ GLTRNB NEXT.
008390	   IF WS-STAT1 = "0"
	       DISPLAY TB-KEY AT 0925
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5
	       MOVE ZERO	 TO WS-F-ERROR
008400	       GO TO HB25.
008420	     MOVE 51		 TO WS-F-ERROR.

       HB25.
	   IF WS-F-ERROR = 51
	       MOVE 9		 TO WS-ENDINP
	       GO TO HB35.
	   IF NOT (TB-BATCH = WS-BATCH)
	       GO TO HB35.

       HB30.
	     MOVE TB-KEY	 TO B-KEY.
002080	     MOVE TB-NAR	 TO B-NAR.
002130	     MOVE TB-VAL	 TO B-VAL.
002170	     MOVE TB-FLAG	 TO B-FLAG.
	     MOVE TB-BATCH	 TO B-BATCH.
	     PERFORM WRITE-GLBTCH THRU WRITE-GLBTCH-EXIT.
	   IF TB-VAL < ZERO
	       ADD TB-VAL	 TO W20-CREDIT
	   ELSE
	       ADD TB-VAL	 TO W20-DEBIT.
	     GO TO HB20.

       HB35.
	   IF WS-BATCH = ZERO
	       GO TO HB50.
	   IF (W20-DEBIT + W20-CREDIT) = ZERO
	       MOVE "Batch in balance - 'P'rint,'N'ext or 'E'xit"
				 TO WS-ERR-MES
	       MOVE SPACE	 TO WS-OPTION
	       PERFORM OPT-MESSAGE
	       IF USER-FUNC
		   EVALUATE KEY-CODE-1
		     WHEN ESC-KEY
			  GO TO HB900
		     WHEN OTHER	 CALL X"E5"
		   END-EVALUATE
		   GO TO HB35
	       ELSE
	       IF DATA-8BIT
		   MOVE KEY-CODE-1X  TO WS-OPTION
		   IF NOT (WS-OPTION = "E" OR "N" OR "P")
		       CALL X"E5"
		       GO TO HB35
		   ELSE
		   DISPLAY CLEAR-L25
		   IF WS-OPTION = "E"
		       PERFORM HB45
		       GO TO HB900
		   ELSE
		   IF WS-OPTION = "N"
		       GO TO HB45
		   ELSE
		       GO TO HB40
	       ELSE
		   CALL X"E5"
		   GO TO HB35.

       HB40.
	     PERFORM HA35 THRU HA55.

       HB45.
	     CLOSE GLBTCH.
	     OPEN OUTPUT GLBTCH.
	     CLOSE GLBTCH.
	     OPEN I-O GLBTCH.

       HB50.
	   IF WS-ENDINP = 9
	       GO TO HB900.
	     MOVE ZERO		 TO W20-DEBIT W20-CREDIT.
	     MOVE TB-BATCH	 TO WS-BATCH.
	     DISPLAY WS-BATCH AT 0958
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 6 HIGHLIGHT.
	     GO TO HB30.

       HB900.
	     CLOSE GLTRNB.

       HB999.
	     EXIT.

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
	     MOVE "GLBATCH"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-GLBTCH.
	     MOVE "GLTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GLTRAN.
	     MOVE "GLTRNB"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-GLTRNB.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.

042170 ZA02.
	     OPEN OUTPUT GLBTCH.
035480	   IF RUNTIME-ERROR
               IF FLE-LIMIT
                   GO TO ZA49.
035500     IF WS-STATUS NOT = "00"
035510	       MOVE 61		 TO WS-F-ERROR
050480         PERFORM OPEN-ERROR.
	     CLOSE GLBTCH.
	     OPEN I-O GLBTCH.
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
035990	     MOVE W30-MONTH(WS-S2)
				 TO W35-MONTH(WS-S1).
036000     IF WS-S1 < 12
036010	       ADD 1		 TO WS-S1 WS-S2
036020         GO TO ZA15.
036030	     MOVE ZERO		 TO WS-ERROR.
036040     IF W70-BNKGL = ZERO
036050	       DISPLAY "Parameters not set up - " AT 2020
		       "ENTER" WITH FOREGROUND-COLOR 14
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
036210	     MOVE WS-S2 	 TO W40-YEAR(WS-S1).
036220     IF WS-S1 NOT = WS-Y-END
036230         GO TO ZA20.
	     DISPLAY SPACES AT 2525
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
036250	     GO TO ZA999.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.

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
	       MOVE "Supervisor�"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.

036260 ZA200.
036270	     DISPLAY "Files locked - Try later" AT 2502
036280		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " " WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     WS-STATUS
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15
036290		     " Press any key"
036300		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
036310	     ACCEPT WS-OPTION AT 2545
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
036360 ZB000	     SECTION 9.
      
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
041830	       MOVE WS-GLKEY	 TO WS-KEY
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
041850	   IF WS-F-ERROR = 61
041860	       MOVE W02-GLBTCH	 TO WS-FILE
041870	       MOVE B-KEY	 TO WS-KEYX.
036720     IF WS-STATUS = "10"
036730	       MOVE "End of FILE"
				 TO WS-STAT-MESSAGE
036740     ELSE
036750     IF WS-STATUS = "22"
036760	       MOVE "Duplicate record number"
				 TO WS-STAT-MESSAGE
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
