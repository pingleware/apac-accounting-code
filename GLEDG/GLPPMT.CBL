      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   **	      *******	*******   **	 **  ********  *
      *   **	**  **	      **    **	**    **  ***	***	**     *
      *   **	    **	      **    **	**    **  ** * * **	**     *
      *   **	    **	      *******	*******   **  *  **	**     *
      *   **   ***  **	      **	**	  **	 **	**     *
      *   **	**  **	      **	**	  **	 **	**     *
      *    ******   ********  **	**	  **	 **	**     *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     G / L E D G E R   P E T T Y	  C A S H   P A Y M E N T      *
      *                                                                *
      *     VERSION 8.13.08 - August 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	       GLPPMT.
000030 AUTHOR.                 J.W. LEMMON (APAC).
000040 DATE-WRITTEN.	       JULY 2007.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2007 - 2008
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

000120 COPY DEPART.SL.

000160 COPY GACCNT.SL.

000180 COPY GLTRAN.SL.

000220 COPY GLJRN.SL.

000140 COPY PARAM.SL.

000240 COPY RECOVER.SL.

       COPY SHARED.SL.

000180 COPY TXTRAN.SL.

000270 DATA DIVISION.
000280 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

       COPY DEPART.FDE.

000320 COPY GACCNT.FDE.

000340 COPY GLTRAN.FDE.

000380 COPY GLJRN.FD.

000300 COPY PARAM.FDE.

000400 COPY RECOVER.GFD.

       COPY SHARED.FDE.

000200 COPY TXTRAN.FDE.

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001790 77  WS-IXS	   PIC	9(04)	 COMP-5.
001800 77  WS-IXS1	   PIC	9(04)	 COMP-5.
001810 77  WS-IXS2	   PIC	9(04)	 COMP-5.
001820 77  WS-IXS3	   PIC	9(04)	 COMP-5.
001830 77  WS-IXS4	   PIC	9(04)	 COMP-5.
001700 77  WS-PARKEY	   PIC	9(04)	 COMP-5 VALUE 1.
001720 77  WS-LTRKEY       PIC  9(06)    COMP-5 VALUE 0.
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
001850 77  WS-EFT	   PIC	9(04)	 COMP-5.
001860 77  WS-PAGE         PIC  9(04)    COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
001870 77  WS-LINES	   PIC	9(04)	 COMP-5 VALUE 66.
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
001970 77  WS-TAX	   PIC	X(01).
001970 77  WS-SKIP         PIC  X(01) VALUE "N".
       77  WS-CHQ-PRN	   PIC	X(01) VALUE "N".
001980 77  WS-FIRST        PIC  9(02) VALUE ZERO.
001990 77  WS-LAST	   PIC	9(03) COMP-3 VALUE ZERO.
002010 77  WS-ERROR        PIC  9(01) VALUE ZERO.
002020 77  WS-NUM          PIC  Z9.
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
002070 77  WS-BATCH	   PIC	9(05)	 COMP-3.
002080 77  WS-TNEXT        PIC  9(04)    COMP-5.
002080 77  WS-PERIOD       PIC  9(02)    COMP-5 VALUE ZERO.
002090 77  WS-SPC	   PIC	X(36) VALUE SPACES.
003150 77  WS-PRN-NO	   PIC	X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC	X(01)	 COMP-X.
002100 77  WS-H1	   PIC	X(05) VALUE "DATE:".
002110 77  WS-H3           PIC  X(06) VALUE "BATCH:".
002120 77  WS-H4           PIC  X(05) VALUE "PAGE:".
002150 77  WS-ER1	   PIC	X(07) VALUE "Account".
002160 77  WS-ER3          PIC  X(04) VALUE "Date".
002170 77  WS-ER4          PIC  X(05) VALUE "Value".
002180 77  WS-ER5	   PIC	X(16) VALUE "Reference Number".
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
	   03  WS-PROG	       PIC  X(03) VALUE "011".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002330 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DEPART.ID.

000320 COPY GACCNT.ID.

000340 COPY GLTRAN.ID.

000380 COPY GLJRN.ID.

       COPY PARAM.ID.

000400 COPY RECOVER.ID.

       COPY SHARED.ID.

000200 COPY TXTRAN.ID.

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

	   03  R-L1 REDEFINES W02-PRN-LINE.
	       05  R-DET       PIC  X(136).

	   03  R-L2 REDEFINES W02-PRN-LINE.
	       05  R-H1        PIC  X(06).
	       05  R-DTE1      PIC  Z9/99/9999.
	       05  FILLER      PIC  X(23).
	       05  R-H2        PIC  X(40).
	       05  FILLER      PIC  X(15).
	       05  R-H3        PIC  X(07).
	       05  R-BTCH      PIC  ZZZZ9.
	       05  FILLER      PIC  X(17).
	       05  R-H4        PIC  X(06).
	       05  R-PGE       PIC  ZZ9.

	   03  R-L3 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(03).
	       05  R-H5        PIC  X(11).
	       05  R-H6        PIC  X(05).
	       05  FILLER      PIC  X(01).
	       05  R-H7        PIC  X(13).
	       05  R-H8        PIC  X(05).
	       05  FILLER      PIC  X(06).
	       05  R-H9        PIC  X(09).
	       05  R-H10       PIC  X(12).
	       05  R-H11       PIC  X(13).
	       05  FILLER      PIC  X(01).
	       05  R-H12       PIC  X(09).
	       05  R-H13       PIC  X(13).
	       05  FILLER      PIC  X(08).
	       05  R-H14       PIC  X(14).
	       05  R-H15       PIC  X(09).

	   03  R-L4 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(03).
	       05  R-ACC       PIC  9(04)/99.
	       05  FILLER      PIC  X(04).
	       05  R-ANME      PIC  X(24).
	       05  FILLER      PIC  X(03).
	       05  R-DTE2      PIC  Z9/99/9999.
	       05  FILLER      PIC  X(03).
	       05  R-REF       PIC  X(07).
	       05  FILLER      PIC  X(04).
	       05  R-NAR       PIC  X(36).
	       05  FILLER      PIC  X(02).
	       05  R-DBT       PIC  Z(08)9.99.
	       05  FILLER      PIC  X(02).
	       05  R-CRD       PIC  Z(08)9.99-.
	       05  FILLER      PIC  X(02).

	   03  R-L5 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(65).
	       05  R-H16       PIC  X(10).
	       05  FILLER      PIC  X(01).
	       05  R-H17       PIC  X(15).
	       05  R-H18       PIC  X(10).
	       05  FILLER      PIC  X(31).

	   03  R-L6 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(08).
	       05  R-H19       PIC  X(11).
	       05  R-H20       PIC  X(05).
	       05  FILLER      PIC  X(01).
	       05  R-H21       PIC  X(13).
	       05  R-H22       PIC  X(05).
	       05  FILLER      PIC  X(04).
	       05  R-H23       PIC  X(06).
	       05  FILLER      PIC  X(01).
	       05  R-H24       PIC  X(12).
	       05  R-H25       PIC  X(05).
	       05  FILLER      PIC  X(06).
	       05  R-H26       PIC  X(14).
	       05  R-H27       PIC  X(10).
	       05  R-H28       PIC  X(15).
	       05  R-H29       PIC  X(16).

	   03  R-L7 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(08).
	       05  R-DAC       PIC  9(04)/99.
	       05  FILLER      PIC  X(04).
	       05  R-ANAME     PIC  X(24).
	       05  FILLER      PIC  X(04).
	       05  R-BRANCH    PIC  X(24).
	       05  FILLER      PIC  X(02).
	       05  R-DTE3      PIC  Z9/99/9999.
	       05  FILLER      PIC  X(02).
	       05  R-DEBT      PIC  Z(08)9.99.
	       05  FILLER      PIC  X(04).
	       05  R-BNKAC     PIC  9(04)/99.
	       05  FILLER      PIC  X(02).
	       05  R-CRED      PIC  Z(08)9.99-.
	       05  FILLER      PIC  X(09).

	   03  R-BL2 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(20).
	       05  R-BH1       PIC  X(34).
	       05  R-BH2       PIC  X(36).
	       05  R-BH3       PIC  X(42).

	   03  R-BL3 REDEFINES W02-PRN-LINE.
	       05  R-BH4.
		   07  R-BAC   PIC  9(06).
		   07  FILLER  PIC  X(01).
		   07  R-BDESC PIC  X(30).
	       05  FILLER      PIC  X(95).

	   03  R-BL3A REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(06).
	       05  R-BH5       PIC  X(10).
	       05  R-BH6       PIC  X(10).
	       05  R-BH7       PIC  X(10).
	       05  R-BH8       PIC  X(10).
	       05  R-BH9       PIC  X(10).
	       05  R-BH10      PIC  X(10).
	       05  R-BH11      PIC  X(10).
	       05  R-BH12      PIC  X(10).
	       05  R-BH13      PIC  X(10).
	       05  R-BH14      PIC  X(10).
	       05  R-BH15      PIC  X(10).
	       05  R-BH16      PIC  X(10).
	       05  R-BH17      PIC  X(06).

	   03  R-BL4 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(02).
	       05  R-BH18      PIC  X(08).
	       05  R-BH19      PIC  X(18).
	       05  FILLER      PIC  X(104).

	   03 R-BL4A REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(02).
	       05  R-BVAL      PIC  Z(08)9- OCCURS 13.

	   03  R-BL5 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(02).
	       05  R-BH20      OCCURS 13.
		   07  R-BH21  PIC   X(09).
		   07  FILLER  PIC  X(01).

       COPY W05.GL.

       COPY W05.VAT.

002630 01  W10-H2.
002640	   03  FILLER	   PIC	X(20).
002650	   03  W10-HEAD    PIC	X(20).

       COPY W15.GL.

	   03  W15-TAXABLE	OCCURS 12.
006790	       05  W15-TAXVAL  PIC S9(09)V99 COMP-3.
	       05  W15-VATVAL  PIC S9(09)V99 COMP-3.

003060 01  W20-TOTALS.
003070     03  W20-DEBIT   PIC S9(09)V99 COMP-3.
003080     03  W20-CREDIT  PIC S9(09)V99 COMP-3.
003080	   03  W20-TAXABLE PIC S9(09)V99 COMP-3.
003080	   03  W20-VAT	   PIC S9(09)V99 COMP-3.
003090	   03  W20-RESULT  PIC S9(09)V99.
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

	   03  W70-PETGL      PIC 9(06)     COMP-3.

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

005630 01  S06.
005640	   03  LINE  2 COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT
005650				 VALUE "PETTY CASH PAYMENT".
005660	   03  LINE  4 COLUMN  4 VALUE "P/Cash G/L :".
	   03	       COLUMN 17 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(06) FROM WS-SPC.
	   03	       COLUMN 24 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC  X(30) FROM WS-SPC.
005670	   03	       COLUMN 57 VALUE "Balance :".
	   03	       COLUMN 67 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC  X(13) FROM WS-SPC.
005680	   03  LINE  5 COLUMN  4 VALUE "Date       :".
	   03	       COLUMN 17 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(10) FROM WS-SPC.
005690	   03  LINE  6 COLUMN  4 VALUE "Refer. Code:".
	   03	       COLUMN 17 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(08) FROM WS-SPC.
005700	   03  LINE  8 COLUMN  4 VALUE "Payee      :".
	   03	       COLUMN 17 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(36) FROM WS-SPC.
005710	   03  LINE  9 COLUMN  4 VALUE "Narrative  :".
	   03	       COLUMN 17 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(36) FROM WS-SPC.
005720	   03  LINE 11 COLUMN  4 VALUE "G/L Ac.No. :".
	   03	       COLUMN 17 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(06) FROM WS-SPC.
	   03	       COLUMN 24 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC  X(30) FROM WS-SPC.
005730	   03	       COLUMN 57 VALUE "Balance :".
	   03	       COLUMN 67 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC  X(13) FROM WS-SPC.
005710	   03  LINE 12 COLUMN  4 VALUE "Narrative  :".
	   03	       COLUMN 17 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(36) FROM WS-SPC.
005740	   03  LINE 13 COLUMN  4 VALUE "Value      :".
	   03	       COLUMN 17 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(13) FROM WS-SPC.
	   03  LINE 14 COLUMN  4 VALUE "V.A.T.     :".
	   03	       COLUMN 17 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(13) FROM WS-SPC.
	   03  LINE 15 COLUMN  4 VALUE "Exclusive  :".
	   03	       COLUMN 17 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(13) FROM WS-SPC.
005790	   03  LINE 17 COLUMN 46 VALUE "Net Amount Payable :".
005800	   03	       COLUMN 67 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(13) FROM WS-SPC.

005750 01  S07.
005760	   03  LINE 11 COLUMN  4 VALUE "G/L Ac.No. :".
	   03	       COLUMN 17 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(06) FROM WS-SPC.
	   03	       COLUMN 24 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC  X(30) FROM WS-SPC.
005770	   03	       COLUMN 57 VALUE "Balance :".
	   03	       COLUMN 67 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC  X(13) FROM WS-SPC.
005710	   03  LINE 12 COLUMN  4 VALUE "Narrative  :".
	   03	       COLUMN 17 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(36) FROM WS-SPC.
005780	   03  LINE 13 COLUMN  4 VALUE "Value      :".
	   03	       COLUMN 17 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(13) FROM WS-SPC.
	   03  LINE 14 COLUMN  4 VALUE "V.A.T.     :".
	   03	       COLUMN 17 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(13) FROM WS-SPC.
	   03  LINE 15 COLUMN  4 VALUE "Exclusive  :".
	   03	       COLUMN 17 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(13) FROM WS-SPC.
005790	   03  LINE 17 COLUMN 46 VALUE "Net Amount Payable :".
005800	   03	       COLUMN 67 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005810				 PIC Z(07)9.99CR USING W20-RESULT.

006330 01  S13.
006340	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z9/99/9999 USING W15-DATE AUTO.

006350 01  S14.
006360	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(07)9.99CR USING W15-TOT AUTO.

006350 01  S14V.
006360	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(07)9.99CR USING W15-VAT AUTO.

006350 01  S14T.
006360	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(07)9.99CR USING W15-VAL AUTO.

       01  S16.
002970	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(07) USING W15-REF AUTO.

       01  S17.
002970	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC 9(06) USING W15-ACCOUNT AUTO.

004870 COPY S99.CRT.

       COPY LEDLUP.CRT.

       COPY OPT.CRT.

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
	       GO TO AA49.
006560	     PERFORM ZA000.
      * ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      * ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.
	     PERFORM CA000.
	     CLOSE RECOVER.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

       AA49.
	     EXIT PROGRAM.

007340 AA50.
      *
      *    ****    Print the audit trail HEADING
      *
	     MOVE "H"		 TO W02-PRN-TYPE.
007350	     MOVE SPACES	 TO R-DET.
007360	     MOVE WS-H1 	 TO R-H1.
007370	     MOVE W90-CNAME	 TO R-H2.
007380	     MOVE WS-H3 	 TO R-H3.
007390	     MOVE WS-H4 	 TO R-H4.
007400	     ADD 1		 TO WS-PAGE.
007410	     MOVE WS-PAGE	 TO R-PGE.
007420	     MOVE WS-BATCH	 TO R-BTCH.
007430	     MOVE W05-TODAY	 TO R-DTE1.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
007450	     MOVE SPACES	 TO R-DET.
007460	     MOVE W10-H2	 TO R-H2.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
007480	     MOVE SPACES	 TO R-DET.

007490 AA55.
007500	     EXIT.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY PRINTUTL.AS6.

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

       COPY DEPART.RD.

       COPY GACCNT.RD.

       COPY GLTRAN.RD.

       COPY GLJRN.RD.

       COPY PARAM.RD.

       COPY SHARED.RD.

      /
008790 AC000           SECTION.

       COPY CONTROL.WR.

       COPY DEPART.WR.

       COPY GACCNT.WR.

       COPY GLTRAN.WR.

       COPY GLJRN.WR.

       COPY PARAM.WR.

000200 COPY TXTRAN.WR.

      /
009940 AE000           SECTION.
009950 AE00.
009960	     MOVE ZERO		 TO WS-ERROR.
009970     IF W15-DD < 01 OR > 31
009980         GO TO AE10.
009990     IF W15-MM < 01 OR > 12
010000         GO TO AE10.
010010     IF W15-MM = 01 OR 03 OR 05 OR 07 OR 08 OR 10 OR 12
010020         GO TO AE15.
010030     IF W15-DD > 30
010040         GO TO AE10.
010050     IF W15-MM = 02 AND W15-DD > 28
010060         GO TO AE05.
010070       GO TO AE15.

010080 AE05.
010090     IF W15-DD > 29
010100         GO TO AE10.
010110	     DIVIDE W15-CY BY 4  GIVING W20-RESULT.
010120     IF W20-DEC = ZERO
010130         GO TO AE15.

010140 AE10.
010150	     MOVE 1		 TO WS-ERROR.
             GO TO AE999.
      *
      *   ****    CALCULATE THE PERIOD INTO WHICH THIS DATE FALLS
      * 
       AE15.
	     MOVE W15-MM	 TO WS-PERIOD.
	   IF W15-CY > W05-CY
	       GO TO AE10.
	   IF W15-CY < W05-CY
               GO TO AE20.
	   IF W05-MM = W15-MM
	      GO TO AE999.
	   IF W15-MM > W05-MM
	       GO TO AE10.
	   IF W15-MM > WS-Y-END
               GO TO AE999.
	   IF W15-CY > WS-L-END
               GO TO AE999.
	     ADD 12		 TO WS-PERIOD
             GO TO AE999.

       AE20.
	   IF W15-CY > WS-L-END
               GO TO AE999.
	   IF (W15-CY = WS-L-END) AND
	      (W15-MM > WS-Y-END)
	       GO TO AE999.
	     ADD 12		 TO WS-PERIOD.

010160 AE999.
010170       EXIT.
      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ This routine updates the various months starting from the ³
      *    ³ transaction date up to the current month.		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AH000           SECTION.
       AH00.
	   IF WS-PERIOD = W05-MM
               GO TO AH999.
           IF WS-PERIOD > 12
	       SUBTRACT 13 FROM WS-PERIOD
				 GIVING WS-S3
               PERFORM AH10
	       ADD 1 WS-OPEN	 GIVING WS-MS1
           ELSE
	       MOVE WS-PERIOD	 TO WS-MS1.
       AH05.
             ADD WS-VALUE        TO G-VAL (WS-MS1).
             ADD 1               TO WS-MS1.
           IF WS-MS1 > 12
               MOVE 1            TO WS-MS1.
           IF WS-MS1 = W05-MM
               GO TO AH999.
             GO TO AH05.
       AH10.
             ADD 1               TO WS-S3.
           IF WS-S3 > 12
               MOVE 1            TO WS-S3.
             ADD WS-VALUE        TO G-LVAL (WS-S3).
           IF WS-S3 NOT = WS-OPEN
               GO TO AH10.
	     ADD WS-VALUE	 TO G-OBAL.
	   IF G-AC < 400000 AND W70-UNPROF > 400000
	       PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT
	       MOVE W70-UNPROF	 TO W15-ACCOUNT G-AC
008980	       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT
	       IF WS-F-ERROR = 0
		   MOVE G-AC	 TO T-AC
015130		   PERFORM AY12 THRU AY59
	       ELSE
		   MOVE GLJ-AC	 TO W15-ACCOUNT G-AC
		   PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
       AH15.
             EXIT.
       AH999.
             EXIT.

       COPY GACCNT.LUP.

       COPY APAC.HLP.

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
010530 AY000           SECTION.
010540 AY01.
010550       MOVE 01 TO REC-FILE.
010560       MOVE WS-PARKEY TO REC-KEY.
011710	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
010570	     MOVE PAR-RECORD1	 TO REC-PARAM.
010580	     GO TO AY50.

010590 AY12.
010600	     MOVE 12		 TO REC-FILE.
009710	     MOVE ZERO		 TO REC-KEY REC-TYPE.
	   IF WS-SKIP = "N"
               PERFORM READ-GACCNT-LOCK THRU READ-GACCNT-EXIT
           ELSE
               MOVE "N"          TO WS-SKIP.
010620       MOVE G-REC          TO REC-GACCNT.
010630       GO TO AY50.
010640 AY13.
010650       MOVE 13 TO REC-FILE.
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

       AY38.
             MOVE 38             TO REC-FILE.
008020	     MOVE ZERO		 TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
	     MOVE DPT-RECORD	 TO REC-DEPART.
             GO TO AY50.

012150 AY39.
012160       MOVE 39             TO REC-FILE.
012170       MOVE WS-NETKEY      TO REC-KEY.
012180       MOVE NET-RECORD     TO REC-NETWORK.
012190	     GO TO AY50.

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
                        WITH FOREGROUND-COLOR 14 
                        WS-STATUS WITH FOREGROUND-COLOR 15
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
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12.
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
			     FOREGROUND-COLOR 14 BLINK
	       ACCEPT WS-STIME FROM TIME
	       PERFORM LOCK-REC-LOOP
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 14 BLINK
	       GO TO AY60.
012880	     DISPLAY SPACE AT 2572
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.

012900 AY63.
012920	     MOVE WS-SUB	 TO WS-USUB.
012940	     GO TO AY61.

011260 AY65.
011270       MOVE 4              TO WS-NETKEY.
011300       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
011310       MOVE G-BATCH        TO WS-BATCH.
011320     IF G-BATCH = 99999
011330         MOVE 1            TO G-BATCH
011340     ELSE
011350         ADD 1             TO G-BATCH.
011360       PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
011380       GO TO AY999.

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
013450 CA000	       SECTION 50.
013460 CA0.
      *    *************************************************************
      *    ****   1 7	C P I	( C O N D E N S E D   P R I N T )
      *    *************************************************************
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 3		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
013750	     MOVE W05-TODAY	 TO W15-DATE.

013500 CA00.
013510       DISPLAY CLR-SCREEN.
013520       DISPLAY S06.

013530 CA02.
013540	     MOVE ZERO		 TO WS-PAGE W20-TOTAL WS-EFT.
013550	     MOVE "PETTY CASH PAYMENTS"
				 TO W10-HEAD.
	     GO TO CA09.

013560 CA05.
013570       PERFORM AA50.
013580       MOVE "ACCNT NO "    TO R-H5.
013590       MOVE ALL "_"        TO R-H6.
013600       MOVE "ACCOUNT NAME" TO R-H7.
013610       MOVE ALL "_"        TO R-H8.
013620       MOVE "DATE"         TO R-H9.
013630	     MOVE "REF. CODE"	 TO R-H10.
013640       MOVE ALL "_"        TO R-H16.
013650       MOVE "PAYEE/NARRATIVE"
                                 TO R-H17.
013660       MOVE ALL "_"        TO R-H18.
013670       MOVE "DEBIT"        TO R-H14.
013680       MOVE "CREDIT"       TO R-H15.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
013700	     MOVE SPACES	 TO R-DET.
	     MOVE "D"		 TO W02-PRN-TYPE.

       CA09.
013720	     MOVE W70-PETGL	 TO W15-PETTY.

013710 CA10.
013730	     MOVE SPACES	 TO W15-NAR W15-PAYEE
013740				    W15-REF.
013760       MOVE 1              TO WS-LAST.

013770 CA15.
013780	     MOVE W15-PETTY	 TO W15-ACCOUNT.
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
013790	     ACCEPT S17 AT 0417.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE ZERO	 TO W15-ACCOUNT
		     GO TO CA120
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     PERFORM AA350
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CA15
	       END-EVALUATE
	       DISPLAY S17 AT 0417
               IF W15-ACCOUNT = ZERO
015990		   GO TO CA15.
	     DISPLAY CLEAR-L25.
013810     IF W15-ACCOUNT = ZERO
013820         GO TO CA120.
013830     IF W15-ASER = ZERO
013840	       MOVE WS-ER7	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
013850         GO TO CA15.
013860	     MOVE W15-ACCOUNT	 TO W15-PETTY.

013870 CA20.
008970	     MOVE W15-ACCOUNT	 TO G-AC.
008980       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
021840	   IF WS-F-ERROR = 8
013900	       MOVE WS-ER6	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
013910         GO TO CA15.
013920	     DISPLAY G-NAME AT 0424
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
013930	     MOVE G-BAL 	 TO W25-V1.
013940	     DISPLAY W25-S7V2 AT 0467
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
013950 CA30.
013960	     MOVE G-AC		 TO WS-KSTORE.

013970 CA35.
013980	     ACCEPT S13 AT 0517.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO CA15
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CA35
	       END-EVALUATE.
013990	     PERFORM AE000.
014000	   IF WS-ERROR = 1
014010         GO TO CA35.
	   IF WS-PERIOD > 12
	       PERFORM DA000
	       IF WS-ERROR = 1
		   GO TO CA35.
014020	     DISPLAY CLEAR-L25.
      *    *************************************************************
      *    *	 Switch the transaction date into YYYYMMDD format      *
      *    *************************************************************
	     MOVE W15-CC	 TO W15-CN.
             MOVE W15-YY         TO W15-YR.
             MOVE W15-MM         TO W15-MT.
             MOVE W15-DD         TO W15-DY.
      *    *************************************************************
      *    ****    AMEND TO ALLOW FOR MULTIPLE VAT RATES
      *    *************************************************************
	   IF W15-YMD < WS-VAT-DATE(1)
	       MOVE 7		 TO WS-VAT-SUB
	   ELSE
	       MOVE 1		 TO WS-VAT-SUB.
	     MOVE W05-VAT(WS-VAT-SUB)
				 TO W05-RATE.

014030 CA40.
012300	     ACCEPT S16 AT 0617.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO CA35
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CA40
	       END-EVALUATE.
014060	   IF W15-REF = SPACES
014070         GO TO CA40.
	     MOVE 17		 TO SLIN.
	     PERFORM CHECK-SETUP THRU CHECK-EXIT.
014160	   IF WS-OPTION = "N"
014170         GO TO CA15.

014180 CA45.
014190	     ACCEPT W15-PAYEE AT 0817
014200		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO CA40
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CA45
	       END-EVALUATE.
014210	   IF W15-PAYEE = SPACES
014220         GO TO CA45.

014230 CA50.
014240	     ACCEPT W15-NAR AT 0917
014250		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO CA45
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CA50
	       END-EVALUATE.
014260	   IF W15-NAR = SPACES
014270         GO TO CA50.
	     MOVE W15-NAR	 TO W15-NAR2.
	     MOVE 17		 TO SLIN.
	     PERFORM CHECK-SETUP THRU CHECK-EXIT.
014360	   IF WS-OPTION = "N"
014370         GO TO CA45.
014380	     MOVE ZERO		 TO W20-DEBIT W20-CREDIT
				    W20-VAT   W20-TAXABLE.
	     MOVE 1		 TO WS-S9.

       CA53.
018660	     MOVE ZERO		 TO W15-VATVAL(WS-S9)
				    W15-TAXVAL(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO CA53.

014390 CA55.
014400	     MOVE ZERO		 TO W15-ACCOUNT W15-VAL
				    W15-TOT	W15-VAT.

014410 CA60.
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
014420	     ACCEPT S17 AT 1117.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE ZERO	 TO W15-ACCOUNT
014450		     SUBTRACT 1  FROM WS-LAST
		     GO TO CA82
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     PERFORM AA350
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CA60
	       END-EVALUATE
	       DISPLAY S17 AT 1117
               IF W15-ACCOUNT = ZERO
015990             GO TO CA60.
	     DISPLAY CLEAR-L25.
014440     IF W15-ACCOUNT = ZERO
014450	       SUBTRACT 1	 FROM WS-LAST
014460         GO TO CA82.
014470     IF W15-ASER = ZERO
014480	       MOVE WS-ER7	 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
014490         GO TO CA60.
008970	     MOVE W15-ACCOUNT	 TO G-AC.
008980       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
021840	   IF WS-F-ERROR = 8
014520	       MOVE WS-ER6	 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
014530         GO TO CA60.
014550	     DISPLAY G-NAME AT 1124
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
014560	     MOVE G-BAL 	 TO W25-V1.
014570	     DISPLAY W25-S7V2 AT 1167
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

       CA70.
014240	     ACCEPT W15-NAR2 AT 1217
014250		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO CA60
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CA70
	       END-EVALUATE.
014260	   IF W15-NAR2 = SPACES
014270	       GO TO CA70.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Is this amount VAT inclusive?	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE "N"		 TO WS-OPTION.

       CA75.
	     MOVE "Taxable: 'N'o or 'I'ncluded  [N]"
				 TO WS-ERR-MES.
	     PERFORM OPT-MESSAGE.
	     MOVE WS-OPTION	 TO WS-TAX.
027950	   IF NOT (WS-TAX = "N" OR "I")
	       GO TO CA75.

014580 CA80.
014590	     ACCEPT S14 AT 1317.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO CA60
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CA80
	       END-EVALUATE.
014600	   IF W15-TOT = ZERO
014610         GO TO CA80.
	   IF WS-TAX = "I"
	       COMPUTE W15-VAT ROUNDED =
		   ((W15-TOT / (100.00000 + W05-RATE)) * W05-RATE)
	       SUBTRACT W15-VAT FROM W15-TOT
				 GIVING W15-VAL
	   ELSE
	       MOVE W15-TOT	 TO W15-VAL.
	     DISPLAY S14V AT 1417.
	     DISPLAY S14T AT 1517.
	     MOVE 17		 TO SLIN.
	     PERFORM CHECK-SETUP THRU CHECK-EXIT.
014700	   IF WS-OPTION = "N"
	       MOVE ZERO	 TO W15-VAL W15-TOT W15-VAT
	       DISPLAY S14V AT 1417
	       DISPLAY S14T AT 1517
014710	       GO TO CA60.
014720	   IF W15-TOT > ZERO
014730	       ADD W15-TOT	 TO W20-TOTAL.
014740	     ADD W15-VAL	 TO W20-DEBIT.
	   IF NOT(W15-VAT = ZERO)
	       ADD W15-VAT	 TO W20-VAT
	       ADD W15-VAL	 TO W20-TAXABLE.
014750	     MOVE WS-LAST	 TO WS-GLJKEY.
      *
      *    ****   L O G   D E T A I L S   T O	W O R K   F I L E
      *
014760	     MOVE W15-ACCOUNT	 TO GLJ-AC.
014780	     MOVE W15-VAL	 TO GLJ-VAL.
	     MOVE W15-NAR2	 TO GLJ-NAR.
014790       PERFORM WRITE-GLJRN THRU WRITE-GLJRN-EXIT.
014800	     ADD 1		 TO WS-LAST.
	     MOVE 11		 TO CLIN.
	     MOVE 01		 TO CCOL.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 23.
	     ADD W20-DEBIT W20-VAT
				 GIVING W20-RESULT.
014820	     DISPLAY S07.
014830       GO TO CA55.

014840 CA82.
014850	   IF WS-LAST = ZERO
014860         GO TO CA115.
	   IF NOT(W20-VAT = ZERO)
014780	       ADD W20-VAT	 TO W20-DEBIT.
014870	     MOVE "'P'rocess Payment, 'C'ancel or 'M'ore  [ ]"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.

014890 CA82A.
	     MOVE 17		 TO SLIN.
	     PERFORM OPT-SETUP THRU OPT-EXIT.
014960	   IF NOT (WS-OPTION = "P" OR "C" OR "M")
014970	       GO TO CA82.
	   IF WS-OPTION = "M"
	       ADD 1		 TO WS-LAST
	       GO TO CA55.
014990	   IF WS-OPTION = "C"
	       SUBTRACT W20-DEBIT
				 FROM W20-TOTAL
015000         GO TO CA115.
015010	     DISPLAY "Processing P/Cash Pmt" AT 2520
015020		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
      *    *************************************************************
      *    ****   S T A R T   A U D I T   T R A I L
      *    *************************************************************
	   IF WS-PAGE = ZERO
013490	       PERFORM AY65 THRU AY999
015790	       PERFORM CA05.
015030	     MOVE 1		 TO WS-GLJKEY.
015040       PERFORM AY60 THRU AY999.

015050 CA83.
015060     IF WS-GLJKEY > WS-LAST
015070         GO TO CA100.
015080       PERFORM READ-GLJRN THRU READ-GLJRN-EXIT.
015090	     MOVE GLJ-AC	 TO W15-ACCOUNT G-AC.
015110	     MOVE GLJ-VAL	 TO W15-VAL.
	     MOVE GLJ-NAR	 TO W15-NAR2.
015130       PERFORM AY12 THRU AY59.
015150       INITIALIZE T-REC.

015300 CA85.
	     MOVE W15-CC	 TO W15-CN.
             MOVE W15-YY         TO W15-YR.
             MOVE W15-MM         TO W15-MT.
             MOVE W15-DD         TO W15-DY.
012680       MOVE W15-YMD        TO T-DATE.
015330	     MOVE W15-NAR2	 TO T-NARATIVE.
015340	     MOVE W15-REF	 TO T-REFER.
015360       MOVE G-AC           TO T-AC.
015370	     MOVE 11		 TO T-CODE.
015380     IF W15-VAL > ZERO
015390         ADD W15-VAL       TO G-DT
015400     ELSE
015410         ADD W15-VAL       TO G-CR.
015420       MOVE W15-VAL        TO T-VAL
                                    WS-VALUE.
             MOVE WS-BATCH       TO T-BATCH.
	     MOVE "N"		 TO T-FLAG.
             PERFORM AH000.
             ADD WS-VALUE        TO G-VAL (W05-MM).
015430       ADD W15-VAL         TO G-BAL.
015480       PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
015270       PERFORM AY13 THRU AY59.
015500       MOVE W15-PAYEE      TO T-NARATIVE.
015510       MOVE 5              TO T-SUB.
015530       PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
015270       PERFORM AY13 THRU AY59.
015640       PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.

015650 CA95.
015660	     MOVE W15-ACCOUNT	 TO R-ACC.
015670	     MOVE G-NAME	 TO R-ANME.
015680	     MOVE W15-DATE	 TO R-DTE2.
015690	     MOVE W15-REF	 TO R-REF.
015700	     MOVE W15-NAR2	 TO R-NAR.
015710     IF W15-VAL > ZERO
015720	       MOVE W15-VAL	 TO R-DBT
015730     ELSE
015740	       MOVE W15-VAL	 TO R-CRD.
015750	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
015790	       PERFORM CA05.
015800	     MOVE SPACES	 TO R-DET.
015810	     ADD 1		 TO WS-GLJKEY
015820       GO TO CA83.

015830 CA100.
015840	     DISPLAY CLEAR-L25.
015850     IF W20-DEBIT = ZERO
015860         GO TO CA115.
      *    ************************************************************
      *    *	 C H E C K   F O R   V A T   A N D   U P D A T E      *
      *    ************************************************************
	   IF NOT(W20-VAT = ZERO)
	       PERFORM EA000.
015870	     MOVE WS-KSTORE	 TO G-AC.
015890       PERFORM AY12 THRU AY59.
015910       INITIALIZE T-REC.

016060 CA105.
	     MOVE W15-CC	 TO W15-CN.
             MOVE W15-YY         TO W15-YR.
             MOVE W15-MM         TO W15-MT.
             MOVE W15-DD         TO W15-DY.
012680       MOVE W15-YMD        TO T-DATE.
016090       MOVE W15-NAR        TO T-NARATIVE.
016100	     MOVE W15-REF	 TO T-REFER.
016110       ADD 1               TO G-GLTRAN.
016130       MOVE G-AC           TO T-AC.
016140	     MOVE 12		 TO T-CODE.
016150       MULTIPLY -1 BY W20-DEBIT 
                                 GIVING W20-CREDIT.
015380	   IF W20-CREDIT > ZERO
015390	       ADD W20-CREDIT	 TO G-DT
015400     ELSE
015410	       ADD W20-CREDIT	 TO G-CR.
016160*	     ADD W20-CREDIT	 TO G-CR.
             MOVE W20-CREDIT     TO WS-VALUE.
             MOVE WS-BATCH       TO T-BATCH.
	     MOVE "N"		 TO T-FLAG.
             PERFORM AH000.
             ADD WS-VALUE        TO G-VAL (W05-MM).
016170       ADD W20-CREDIT      TO G-BAL.
016180       MOVE W20-CREDIT     TO T-VAL.
015970       PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
015920       PERFORM AY13 THRU AY59.
016220       MOVE W15-PAYEE      TO T-NARATIVE.
016230       MOVE 5              TO T-SUB.
016250       PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
015920       PERFORM AY13 THRU AY59.
016360       PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.

016370 CA110.
015750	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
016420	       PERFORM CA05.
016430	     MOVE G-AC		 TO R-ACC.
016440	     MOVE G-NAME	 TO R-ANME.
016450	     MOVE W15-DATE	 TO R-DTE2.
016460	     MOVE W15-REF	 TO R-REF.
016470	     MOVE W15-PAYEE	 TO R-NAR.
016480	     MOVE W20-CREDIT	 TO R-CRD.
015710	   IF W20-CREDIT > ZERO
015720	       MOVE W20-CREDIT	 TO R-DBT
015730     ELSE
015740	       MOVE W20-CREDIT	 TO R-CRD.
039710	   IF W02-LINAGE < (W02-PRN-LENGTH - 2)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
016530	       PERFORM CA05.
016540	     MOVE SPACES	 TO R-DET.
016560       PERFORM AY70 THRU AY999.
016580	     CLOSE GLJRN.
016590       OPEN OUTPUT GLJRN.
016600       CLOSE GLJRN.
016610	     OPEN I-O GLJRN.

016630 CA115.
029700	     MOVE "ENTER to continue - E to end  [ ]"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     MOVE 17		 TO SLIN.
	     PERFORM OPT-SETUP THRU OPT-EXIT.
016730	   IF NOT (WS-OPTION = "E")
016740         PERFORM CA00
016750         GO TO CA10.

016760 CA120.
016770	     MOVE "BATCH TOTAL"  TO R-NAR.
016780	     MOVE W20-TOTAL	 TO R-DBT.
016790	     MULTIPLY -1 BY W20-TOTAL
				 GIVING R-CRD.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *
      *    ****   1 O	C P I	( N O R M A L	P R I N T )
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
016830	     MOVE SPACES	 TO R-DET.

016840 CA999.
016850       EXIT.

       DA000	     SECTION 50.
       DA00.
	     MOVE "Date in a prior Fin Year -Continue Y/N  [Y]"
				 TO WS-ERR-MES.
	     MOVE "Y"		 TO WS-OPTION.
033910	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "Y" OR "N")
	       GO TO DA00.
	   IF WS-OPTION = "Y"
	       MOVE ZERO	 TO WS-ERROR
	   ELSE
	       MOVE 1		 TO WS-ERROR.

       DA99.
	     EXIT.

      *    *************************************************************
      *    *  U P D A T E   V A T   F I L E   A N D   G / L   A C N T  *
      *    *************************************************************
       EA000	     SECTION 50.
       EA00.
      *    *************************************************************
      *    ****    VAT Control
      * 			   MULTIPLE RATE TO BE INCLUDED
      *    *************************************************************
	     MOVE 1		 TO W05-V-RATE.
	     MOVE W20-VAT	 TO W15-VATVAL(WS-VAT-SUB).
	     MOVE W20-TAXABLE	 TO W15-TAXVAL(WS-VAT-SUB).
	     MOVE W05-VAT-CODE	 TO DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	     MOVE 0		 TO WS-ACTION.
027840	     PERFORM AY38 THRU AY59.
	     MOVE DPT-I-VAT-GL	 TO W15-ACCOUNT.
	   IF W15-ACCOUNT = ZERO
	       PERFORM EA100
	       MOVE W15-ACCOUNT  TO DPT-I-VAT-GL.
	   IF W20-VAT < ZERO
	       ADD W15-VATVAL(WS-VAT-SUB)
				 TO DPT-I-VRET-DAY
				    DPT-I-VRET-MTD
				    DPT-I-VRET-YTD
	       ADD W15-TAXVAL(WS-VAT-SUB)
				 TO DPT-V-PRET-DAY
				    DPT-V-PRET-MTD
				    DPT-V-PRET-YTD
	   ELSE
	       ADD W15-VATVAL(WS-VAT-SUB)
				 TO DPT-I-VAT-DAY
				    DPT-I-VAT-MTD
				    DPT-I-VAT-YTD
	       ADD W15-TAXVAL(WS-VAT-SUB)
				 TO DPT-V-PUR-DAY
				    DPT-V-PUR-MTD
				    DPT-V-PUR-YTD.
027880       PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
015090	     MOVE W15-ACCOUNT	 TO G-AC.
015110	     MOVE W20-VAT	 TO W15-VAL.
015130       PERFORM AY12 THRU AY59.
015150       INITIALIZE T-REC.
012680	     MOVE W15-YMD	 TO T-DATE.
015330       MOVE W15-NAR        TO T-NARATIVE.
015340	     MOVE W15-REF	 TO T-REFER.
015360       MOVE G-AC           TO T-AC.
015380	   IF W15-VAL > ZERO
012780	       MOVE 01		 TO T-CODE
015390         ADD W15-VAL       TO G-DT
015400     ELSE
012780         MOVE 02           TO T-CODE 
015410         ADD W15-VAL       TO G-CR.
015420       MOVE W15-VAL        TO T-VAL
                                    WS-VALUE.
             MOVE WS-BATCH       TO T-BATCH.
             PERFORM AH000.
             ADD WS-VALUE        TO G-VAL (W05-MM).
015430       ADD W15-VAL         TO G-BAL.
015480       PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
015270       PERFORM AY13 THRU AY59.
015640	     PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
      *    *************************************************************
      *    *	    P R I N T	V A T	T R A N S A C T I O N	       *
      *    *************************************************************
015660	     MOVE W15-ACCOUNT	 TO R-ACC.
015670	     MOVE G-NAME	 TO R-ANME.
015680	     MOVE W15-DATE	 TO R-DTE2.
015690	     MOVE W15-REF	 TO R-REF.
015700	     MOVE W15-NAR	 TO R-NAR.
015710     IF W15-VAL > ZERO
015720	       MOVE W15-VAL	 TO R-DBT
015730     ELSE
015740	       MOVE W15-VAL	 TO R-CRD.
015750	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
015790	       PERFORM CA05.
015800	     MOVE SPACES	 TO R-DET.
      *    *************************************************************
      *    *		   V A T   R E P O R T	 F I L E	       *
      *    *************************************************************
	     MOVE 5		 TO TAX-ACTYPE.
	   IF W20-VAT > ZERO
	       MOVE 01		 TO TAX-CODE
	       MOVE "1"		 TO TAX-TYPE
	   ELSE
	       MOVE 02		 TO TAX-CODE
	       MOVE "2"		 TO TAX-TYPE.
	     MOVE W15-YMD	 TO TAX-DATE.
	     MOVE W15-REF	 TO TAX-REF.
	     COMPUTE TAX-VALUE = (W20-VAT + W20-TAXABLE) * -1.
	     MOVE ZERO		 TO TAX-TAXFREE.
	     MOVE WS-KSTORE	 TO TAX-AC.
	     MOVE ZERO		 TO TAX-SEQ.
	     MOVE 1		 TO WS-S9.

       EA05.
	     MULTIPLY -1 BY W15-TAXVAL(WS-S9)
				 GIVING TAX-VAL(WS-S9).
	     MULTIPLY -1 BY W15-VATVAL(WS-S9)
				 GIVING TAX-VAT(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO EA05.
000890	     PERFORM WRITE-TXTRAN THRU WRITE-TXTRAN-EXIT.
000900	     MOVE 1		 TO WS-ACTION.
000910	     PERFORM AY19 THRU AY59.

       EA99.
	     EXIT.
      *    *************************************************************
      *    *   G E T   -   I N P U T   V A T   G / L   A C C O U N T   *
      *    *************************************************************
       EA100	     SECTION 50.
       EA105.
011050	     PERFORM SAVE-SCREEN-3.
	     MOVE 15		 TO SHADE-ROW.
	     MOVE 18		 TO SHADE-COL.
	     MOVE 47		 TO SHADE-WIDTH.
	     MOVE 3		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
		      AT 1416 WITH FOREGROUND-COLOR 0
				   BACKGROUND-COLOR 3
		     "¿" WITH FOREGROUND-COLOR 3 HIGHLIGHT
			      BACKGROUND-COLOR 3.
	     DISPLAY "³      " AT 1516
		      WITH FOREGROUND-COLOR 0
			   BACKGROUND-COLOR 3
		     "Key in the Input VAT account Number      "
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "³" WITH FOREGROUND-COLOR 3 HIGHLIGHT
			      BACKGROUND-COLOR 3.
	     DISPLAY "³ " AT 1616 WITH FOREGROUND-COLOR 0
				       BACKGROUND-COLOR 3
		     "A/C No.                                       "
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "³" WITH FOREGROUND-COLOR 3 HIGHLIGHT
			      BACKGROUND-COLOR 3.
	     DISPLAY "À" AT 1716 WITH FOREGROUND-COLOR 0
				      BACKGROUND-COLOR 3
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 3.
	     PERFORM SCREEN-SHADOW.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Account Lookup"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       EA110.
013790	     ACCEPT S17 AT 1626.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     PERFORM AA350
		 WHEN OTHER
		     CALL X"E5"
		     GO TO EA110
	       END-EVALUATE
	       DISPLAY S17 AT 1626
               IF W15-ACCOUNT = ZERO
015990		   GO TO EA110.
	     DISPLAY CLEAR-L25.
013810     IF W15-ACCOUNT = ZERO
013820	       GO TO EA110.
013830     IF W15-ASER = ZERO
013840	       MOVE WS-ER7	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
013850	       GO TO EA110.
008970	     MOVE W15-ACCOUNT	 TO G-AC.
008980       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
021840	   IF WS-F-ERROR = 8
013900	       MOVE WS-ER6	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
013910	       GO TO EA110.
013920	     DISPLAY G-NAME AT 1633
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT
			  BACKGROUND-COLOR 3.
	     PERFORM CHECK-CORRECT.
018520	   IF WS-OPTION = "N"
	       GO TO EA110.
	     PERFORM RESTORE-SCREEN-3.

       EA999.
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
035290	     DISPLAY CLR-SCREEN.
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
      *    *** G E N E R A L   L E D G E R   A C C O U N T   F I L E ***
      *    *************************************************************
	     MOVE "GACCNT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GACCNT.
      *    *************************************************************
      *    **	 G / L	 T R A N S A C T I O N	 W O R K   F I L E    **
      *    *************************************************************
	     MOVE "GLJRN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-GLJRN.
      *    *************************************************************
      *    G E N E R A L   L E D G E R	 T R A N S A C T I O N	 F I L E
      *    *************************************************************
	     MOVE "GLTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GLTRAN.
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
      *    *************************************************************
      *    ****      V A T   T R A N S A C T I O N S   F I L E	    ****
      *    *************************************************************
	     MOVE "TXTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-TXTRAN.

042170 ZA02.
035650	     OPEN OUTPUT RECOVER GLJRN.
035660       CLOSE RECOVER GLJRN.
035670       OPEN I-O RECOVER GLJRN.
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
	     MOVE PAR-PETGL	 TO W70-PETGL.
             MOVE 4              TO WS-NETKEY.
035770       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
035780	     MOVE G-BATCH	 TO WS-BATCH.
035900     IF G-YEAR-END = ZERO
035910	       MOVE 2		 TO G-YEAR-END
035920	       PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
      *    *************************************************************
      *    ****    S E T   U P	 M O N T H S   ( Y E A R   E N D)   ****
      *    *************************************************************
035930	     MOVE 1		 TO WS-S1.
035940	     MOVE G-YEAR-END	 TO WS-S2 WS-Y-END.
035160	     MOVE G-LAST-YE	 TO WS-L-END.
	   IF WS-L-END = ZERO
	       SUBTRACT 1 FROM W05-CY
				 GIVING WS-L-END.
	   IF G-OPEN = ZERO
	       MOVE WS-Y-END	 TO G-OPEN.
	     MOVE G-OPEN	 TO WS-OPEN.
035950	     ADD 1		 TO WS-S2.
035960 ZA15.
035970     IF WS-S2 > 12
035980	       SUBTRACT 12	 FROM WS-S2.
035990       MOVE W30-MONTH (WS-S2) TO W35-MONTH (WS-S1).
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
036210	     MOVE WS-S2 	 TO W40-YEAR (WS-S1).
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
041220	     MOVE 1		 TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.

041240 ZA60.
041250	     MOVE WS-G1 	 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
	     MOVE WS-G8 	 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270	       ADD 1		 TO WS-S1
041280         GO TO ZA60.
	     MOVE WS-G9 	 TO WS-TCH  (1).
	     MOVE WS-G10	 TO WS-TCH (80).
	     MOVE WS-G11	 TO WS-BCH  (1).
	     MOVE WS-G12	 TO WS-BCH (80).
	     MOVE WS-G14	 TO WS-TCHR  (1) WS-BCHR  (1).
	     MOVE WS-G13	 TO WS-TCHR (80) WS-BCHR (80).
021930	     MOVE WS-G2 	 TO WS-TCHR (11) WS-TCHR (45)
021940				    WS-TCHR (63).
021950	     MOVE WS-G3 	 TO WS-MCHR (11) WS-MCHR (45)
021960				    WS-MCHR (63)
				    WS-MCHR  (1) WS-MCHR (80).
021970	     MOVE WS-G4 	 TO WS-BCHR (11) WS-BCHR (45)
021980				    WS-BCHR (63).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
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
041620	       MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041800	   ELSE
041810     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041640	   ELSE
041810     IF WS-F-ERROR = 8
041820	       MOVE W02-GACCNT	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE G-AC	 TO WS-KEYX
041840	   ELSE
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
041870	       MOVE WS-SHARED	 TO WS-KEY.
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
