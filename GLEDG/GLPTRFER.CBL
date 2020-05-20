      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *  ******  **	  *******  ******** *******  **    ** ******** *
      * **    ** **	  **	**    **    **	  ** ***   ** **       *
      * **	 **	  **	**    **    **	  ** ****  ** **       *
      * **	 **	  *******     **    *******  ** ** ** ****     *
      * **   *** **	  **	      **    **	 **  **  **** **       *
      * **    ** **	  **	      **    **	  ** **   *** **       *
      *  ******  ******** **	      **    **	  ** **    ** **       *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     T R A N S F E R   G / L   T R A N S A C T I O N S	       *
      *                                                                *
      *     VERSION 8.14.02 - December 2009			       *
      *                                                                *
      ******************************************************************
      * NB -- To do list: (Audit)				       *
      *       Include a section to transfer selectively if all the     *
      *       transactions do not need to be transfered.	       *
      *ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ*
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	       GLPTRFER.
000030 AUTHOR.		       James William Lemmon.
000040 DATE-WRITTEN.	       December 2009.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2009 - 2010
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

000550	   SELECT PRNREP  ASSIGN W02-PRINTER
                          ORGANIZATION LINE SEQUENTIAL.

000270 DATA DIVISION.
000280 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000300 COPY PARAM.FDE.

000320 COPY GACCNT.FDE.

000340 COPY GLTRAN.FDE.

000730 FD  PRNREP    LABEL RECORD OMITTED
000430		     LINAGE IS WS-PGE-LENGTH.
000750 01  R-L1.
000760     03  R-DET       PIC X(132).
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
000890 01  R-L3.
002440	   03  LED-H1	       PIC  X(07).
002450     03  FILLER          PIC  X(02).
002460     03  LED-H2          PIC  X(12).
002470     03  FILLER          PIC  X(06).
           03  LED-H2A         PIC  X(10).
           03  FILLER          PIC  X(02).
002480     03  LED-H3          PIC  X(04).
002490     03  FILLER          PIC  X(04).
002500     03  LED-H4          PIC  X(04).
           03  FILLER          PIC  X(03).
002500     03  LED-H4A         PIC  X(05).
002510     03  FILLER          PIC  X(04).
002520     03  LED-H5          PIC  X(05).
002530     03  FILLER          PIC  X(02).
002540     03  LED-H6          PIC  X(15).
002550     03  FILLER          PIC  X(28).
002560     03  LED-H7          PIC  X(05).
002570     03  FILLER          PIC  X(06).
002580     03  LED-H8          PIC  X(07).
002590     03  FILLER          PIC  X(01).
001060 01  R-L4.
001080     03  R-ACC       PIC 9(04)/99.
001090	   03  FILLER	   PIC X(02).
001100     03  R-ANME      PIC X(24).
001110	   03  FILLER	   PIC X(02).
001120	   03  R-DTE2	   PIC Z9/99/9999.
001130	   03  FILLER	   PIC X(02).
002680	   03  R-CDE	   PIC X(04).
	   03  FILLER	   PIC X(02).
	   03  R-BATCH	   PIC Z(06).
002690	   03  FILLER	   PIC X(02).
	   03  R-REFER.
001140	   05  R-REF	   PIC Z(07).
001150	   03  FILLER	   PIC X(02).
001160     03  R-NAR       PIC X(36).
001170     03  FILLER      PIC X(01).
001180     03  R-DBT       PIC Z(08)9.99.
	   03  FILLER	   PIC X(01).
001200	   03  R-CRD	   PIC Z(07)9.99-.
001220 01  R-L5.
001230     03  RI-H1       PIC X(06).
001240     03  RI-H2       PIC X(40).
001250     03  RI-H3       PIC X(39).
001260     03  RI-H4       PIC X(42).
001270     03  RI-H5       PIC X(05).
001280 01  R-L6.
001290     03  RI-NO       PIC Z(04)9.
001300     03  FILLER      PIC X(01).
001310     03  RI-ACDT     PIC 9999/99.
001320     03  FILLER      PIC X(01).
001330     03  RI-DTNAME   PIC X(30).
001340     03  FILLER      PIC X(02).
001350     03  RI-ACCR     PIC 9999/99.
001360     03  FILLER      PIC X(01).
001370     03  RI-CRNAME   PIC X(30).
001380     03  FILLER      PIC X(01).
001390     03  RI-NAR      PIC X(36).
001400     03  RI-VAL      PIC Z(07)9.99.
      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001880 77  WS-S1	   PIC	9(04)	 COMP-5.
001890 77  WS-S2           PIC  9(04)    COMP-5.
001890 77  WS-S3           PIC  9(04)    COMP-5.
001890 77  WS-S4	   PIC	9(04)	 COMP-5.
001890 77  WS-S5	   PIC	9(04)	 COMP-5.
001890 77  WS-S6           PIC  9(04)    COMP-5.
001890 77  WS-S7	   PIC	9(04)	 COMP-5.
001890 77  WS-S8	   PIC	9(04)	 COMP-5.
001890 77  WS-S9	   PIC	9(04)	  COMP-5.
001890 77  WS-IXS1	   PIC	9(04)	  COMP-5.
001880 77  WS-SUB          PIC  9(04)    COMP-5.
001730 77  WS-NETKEY       PIC  9(04)    COMP-5 VALUE 1.
001700 77  WS-PARKEY	   PIC	9(04)	 COMP-5 VALUE 1.
001840 77  WS-ISUB         PIC  9(04)    COMP-5.
001860 77  WS-PAGE         PIC  9(04)    COMP-5.
001870 77  WS-LINES        PIC  9(04)    COMP-5 VALUE 66.
001880 77  WS-S            PIC  9(04)    COMP-5.
001900 77  WS-Y-END	   PIC	9(02) VALUE ZERO.
001900 77  WS-L-END	   PIC	9(04) VALUE ZERO.
001900 77  WS-OPEN         PIC  9(02) VALUE ZERO.
001910 77  WS-MS1          PIC  9(04)    COMP-5.
001920 77  WS-MS2          PIC  9(04)    COMP-5.
001930 77  WS-VALUE        PIC S9(09)V99 COMP-3.
001940 77  WS-EXS          PIC  9(01).
001950 77  WS-CONREC       PIC  9(01) VALUE ZERO.
001960 77  WS-INDG         PIC  9(01) VALUE ZERO.
001970 77  WS-OPTION       PIC  X(01).
001970 77  WS-BAL-TYPE	   PIC	X(01).
001980 77  WS-FIRST        PIC  9(02) VALUE ZERO.
001990 77  WS-LAST         PIC  9(02) VALUE ZERO.
002010 77  WS-ERROR        PIC  9(01) VALUE ZERO.
002020 77  WS-NUM          PIC  Z9.
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
       77  WS-TRAN-SHRT    PIC	X(03).
003650 77  WS-COND	   PIC	X(06).
003660 77  WS-NORM	   PIC	X(06).
003070 77  WS-EXPP	   PIC	X(06).
003080 77  WS-ECAN	   PIC	X(06).
003090 77  WS-8LPI	   PIC	X(06).
003100 77  WS-6LPI	   PIC	X(06).
       77  WS-10CPI	   PIC	X(06).
       77  WS-12CPI	   PIC	X(06).
       77  WS-17CPI	   PIC	X(06).
002080 77  WS-TNEXT	   PIC	9(04)	 COMP-5.
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
       77  WS-DATE-CHK	   PIC	9(08) COMP-5.
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
       77  WS-PASSWORD	   PIC	X(10) VALUE SPACES.
      /    *************************************************************
      *    ****     G / L   A C C O U N T   T E M P   S T O R E	    ****
      *    *************************************************************
000030 01  WG-REC.
000040	   03  WG-AC	       PIC  9(06).
000050	   03  WG-ACR	       REDEFINES WG-AC.
000060	       05  WG-GR       PIC  9(02).
000070	       05  WG-SR       PIC  9(04).
000080	       05  WG-SER      REDEFINES WG-SR.
000090		   07  WG-SGR  PIC  9(02).
000100		   07  WG-ASER PIC  9(02).
000110	   03  WG-ACR2	       REDEFINES WG-AC.
000120	       05  WG-GRP      PIC  9(01).
000130         05  FILLER      PIC  9(05).
000140	   03  WG-AC2	       REDEFINES WG-AC.
000150	       05  WG-ACNO     PIC  9(04).
000160         05  FILLER      PIC  9(02).
000170	   03  WG-NAME.
	       05  WG-NKEY     PIC  X(12).
	       05  FILLER      PIC  X(18).
      *
      *    ****   This indicator informs the system of the type of
      *           budget on this account.
      *           0 = Manual figures entered.
      *           1 = Percentage of Total income.
      *           2 = Percentage of Group income.
      *           3 = Percentage of Group/Subgroup income.
      *        
000180	   03  WG-IND	       PIC  9(01).
      *
      *    ****   This is the percentage for calculating the budget
      * 	  figures based on WG-IND.
      *
000190	   03  WG-BPER	       PIC S9(02)V99  COMP-3.
      *
      *    ****   Budget figures for 12 months (Jan to Dec).
      *
000200	   03  WG-BUD.
000210	       05  WG-BVAL     PIC S9(09)    COMP-3 OCCURS 12.
      *
      *    ****   Monthly Balance for each month of the current year.
      *
000220	   03  WG-MOVE.
000230	       05  WG-VAL      PIC S9(09)V99 COMP-3 OCCURS 12.
000240	   03  WG-DT	       PIC S9(09)V99 COMP-3.
000250	   03  WG-CR	       PIC S9(09)V99 COMP-3.
000260	   03  WG-BAL	       PIC S9(09)V99 COMP-3.
	   03  WG-OBAL	       PIC S9(09)V99 COMP-3.
      *
      *    ****   Monthly Balance for each month of the previous year.
      *
000290	   03  WG-LSTY.
000300	       05  WG-LVAL     PIC S9(09)V99 COMP-3 OCCURS 12.
      *
      *    ****   1 = Income,  2 = Costs,  3 = Expenses.
      * 	  4 = Capital, 5 = Assets, 6 = Liabilities.
      *
	   03  WG-TYPE	       PIC  X(01).
	   03  WG-SEC	       PIC  X(01).
      /    *************************************************************
      *    **** G / L	T R A N S A C T I O N	T E M P   S T O R E ****
      *    *************************************************************
002020 01  WT-REC.
	   03  WT-KEY.
002160	       05  WT-AC       PIC  9(06).
002030	       05  WT-DATE     PIC  9(08).
002040	       05  WT-DTE REDEFINES WT-DATE.
002050		   07  WT-CC   PIC  9(02).
002050		   07  WT-YY   PIC  9(02).
002060		   07  WT-MM   PIC  9(02).
002070		   07  WT-DD   PIC  9(02).
	       05  WT-AKEY.
      *
      *     01 OR 02 - Journal Entries
      *     03 OR 04 - Cheques
      *     06 OR 07 - Deposits
      *     08 OR 09 - Electronic Funds
      *
002120		   07  WT-CODE PIC  9(02).
		   07  WT-REFER.
002110		       09  WT-REF
                               PIC  9(07).
		   07  WT-SEQ  PIC  9(02).
	       05  WT-SUB      PIC  9(01).
002080	   03  WT-NAR	       PIC  X(40).
002090	   03  WT-DESC REDEFINES WT-NAR.
002100	       05  WT-NARATIVE PIC  X(36).
               05  FILLER      PIC  9(07)    COMP-3.
002130	   03  WT-VAL	       PIC S9(09)V99 COMP-3.
002170	   03  WT-FLAG	       PIC  X(01).
	   03  WT-BATCH	       PIC  9(06)    COMP-3.
	   03  FILLER	       PIC  X(03).

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
	       05  FILLER      PIC  X(01) VALUE "≥".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "∞".
	       05  FILLER      PIC  X(01) VALUE "≥".
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
	   03  WS-PROG	       PIC  X(03) VALUE "095".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002330 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

000320 COPY GACCNT.ID.

000340 COPY GLTRAN.ID.

       COPY PARAM.ID.

       01  W02-FILE-IDS.
           03  W02-PRINTER.
	       05  W02-REPORT PIC X(07) VALUE "GLPTPT.".
               05  W02-USER   PIC X(05) VALUE SPACES.

       COPY W05.GL.

002630 01  W10-H2.
002640     03  FILLER      PIC  X(27).
002650     03  W10-HEAD    PIC  X(13).

       01  W10-EDIT.
002910	   03  W10-DATE.
	       05  W10-EDTE	 PIC  99/99/9999.

       COPY W15.GL.

       COPY W20.WS.

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
	   03  W20-AC-NAME PIC	X(40).
003260	   03  W22-ACCOUNT PIC	9(06).
	   03  W22-AC-NAME PIC	X(40).

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

       01  S00.
	   03  LINE  6 COLUMN  5 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE
	   "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ
      -	   "ÕÕÕÕÕÕÕÕÕÕª".
	   03  LINE  7 COLUMN  5 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE
	   "∫ W A R N I N G  -  W A R N I N G  -  W A R N I N G  -  W A
      -    "R N I N G ∫".
	   03  LINE  8 COLUMN  5 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE
	   "ÃÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ
      -	   "ÕÕÕÕÕÕÕÕÕÕπ".
	   03  LINE  9 COLUMN  5 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE
	   "∫ This program must be used with caution. The program will a
      -    "llow you  ∫".
	   03  LINE 10 COLUMN  5 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE
	   "∫ to transfer all or selective General Ledger transactions,
      -    "without   ∫".
	   03  LINE 11 COLUMN  5 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE
	   "∫ updating the account. The index will be updated, but you m
      -    "ust run   ∫".
	   03  LINE 12 COLUMN  5 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE
	   "∫ a recalculate balances after you exit this program, if the
      -    "re were   ∫".
	   03  LINE 13 COLUMN  5 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE
	   "∫ any transfers performed.
      -    "          ∫".
	   03  LINE 14 COLUMN  5 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE
	   "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ
      -    "ÕÕÕÕÕÕÕÕÕÕº".
      *
      *	…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª
      *	∫ W A R N I N G  -  W A R N I N G  -  W A R N I N G  -	W A R N I N G ∫
      *	ÃÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕπ
      *	∫ This program must be used with caution. The program will allow you  ∫
      *	∫ to transfer all or selective General Ledger transactions, without   ∫
      *	∫ updating the account. The index will be updated, but you must run   ∫
      *	∫ a recalculate balances after you exit this program, if there were   ∫
      *	∫ any transfers performed.					      ∫
      *	»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº
      *
005910 01  S09.
006160	   03  LINE  2 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
006170				 VALUE "TRANSFER TRANSACTIONS".
005940	   03  LINE  4 COLUMN  4 VALUE "Account FROM".
	   03  LINE  5 COLUMN  4 VALUE "Ac.No.".
005950	   03	       COLUMN 20 VALUE "Name:".
	   03  LINE  7 COLUMN  4 VALUE "Account TO".
	   03  LINE  8 COLUMN  4 VALUE "Ac.No.".
005950	   03	       COLUMN 20 VALUE "Name:".

       01  S09A.
	   03  S09-AC-FROM.
	   03  LINE  6 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(06) USING W20-ACCOUNT AUTO.
	   03  S09-NM-FROM.
	   03  LINE  6 COLUMN 26 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(40) USING W20-AC-NAME AUTO.
	   03  S09-AC-TO.
	   03  LINE  8 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(06) USING W22-ACCOUNT AUTO.
	   03  S09-NM-TO.
	   03  LINE  8 COLUMN 26 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(40) USING W22-AC-NAME AUTO.

       01  S10.
	   03  S10-ACNO.
	       05  LINE  8 COLUMN 25 FOREGROUND-COLOR 0
				     BACKGROUND-COLOR 7
		   PIC	X(06) USING W15-ACCOUNT AUTO.
	   03  S10-ACNO-TO.
	       05  LINE  8 COLUMN 55 FOREGROUND-COLOR 1
				     BACKGROUND-COLOR 7
		   PIC	X(06) USING W15-ENDACC AUTO.
	   03  S10-DATE.
	       05  LINE  9 COLUMN 25 FOREGROUND-COLOR 0
				     BACKGROUND-COLOR 7
		   PIC	99/99/9999 USING W15-DATE AUTO.
	   03  S10-TYPE.
	       05  LINE 10 COLUMN 25 FOREGROUND-COLOR 0
				     BACKGROUND-COLOR 7
		   PIC	99 USING W15-TYPE AUTO.
	   03  S10-TYPE-DESC.
	       05  LINE 10 COLUMN 29 FOREGROUND-COLOR 0
				     BACKGROUND-COLOR 7
		   PIC	X(03) USING WS-TRAN-SHRT AUTO.
	   03  S10-REF.
	       05  LINE 11 COLUMN 25 FOREGROUND-COLOR 0
				     BACKGROUND-COLOR 7
020290		   PIC	X(08) USING W15-REF AUTO.
	   03  S10-NAR.
	       05  LINE 12 COLUMN 25 FOREGROUND-COLOR 0
				     BACKGROUND-COLOR 7
		   PIC	X(36) USING W15-NAR AUTO.
	   03  S10-VALUE.
	       05  LINE 13 COLUMN 25 FOREGROUND-COLOR 0
				     BACKGROUND-COLOR 7
		   PIC	ZZZZZZZ9.99CR USING W15-VAL AUTO.

006150 01  S11.
006160	   03  LINE  2 COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT
006170				 VALUE "AMEND TRANSACTION".
006180	   03  LINE  4 COLUMN 12 VALUE "Press ".
	   03	       COLUMN 18 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "C".
	   03	       COLUMN 19 VALUE "ontinue or ".
	   03	       COLUMN 30 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Esc".
	   03	       COLUMN 33 VALUE "ape to Exit".

       01  S17.
002970	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC 9(06) USING W15-ACCOUNT AUTO.

       01  S18.
002970	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC 99/99/9999 USING W15-DATE AUTO.

004870 COPY S99.CRT.

       COPY LEDLUP.CRT.

       COPY OPT.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
006540 AA000	       SECTION.
006550 AA00.
003240	   IF LS0-GLLEV < 9
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006560	     PERFORM ZA000.
	     PERFORM EA000.

      *
      *    ****    P R I N T I N G   T O   B E	 I N C L U D E D
      * 	   L A T E R   -   U S E   P R I N T   U T I L I T Y
      *

      *      CLOSE PRNREP.

       AA49.
	     EXIT PROGRAM.

      /    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      * 	   S C R E E N ,   K E Y B O A R D   &	 M O U S E
      *    *************************************************************
      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥	   SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3      ≥
      *    ∆ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕµ
      *    ≥			  SCREEN-SHADOW 		       ≥
      *    √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
      *    ≥ To routine is used to display a shadow down the right and ≥
      *    ≥ along the bottom of a pop-up box. The parameters that are ≥
      *    ≥ required:						       ≥
      *	   ≥	       SHADE-ROW   - Top line of the box + 1.	       ≥
      *	   ≥	       SHADE-COL   - Left line of box + 2.	       ≥
      *	   ≥	       SHADE-WIDTH - Width of the box - 2.	       ≥
      *	   ≥	       SHADE-LINES - Hight of box - 1.		       ≥
      *    ∆ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕµ
      *    ≥			  CHECK-CORRECT 		       ≥
      *    √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
      *    ≥ This routine displays a pop-up window with the message    ≥
      *    ≥	      "Correct ENTER - N if incorrect"		       ≥
      *    ≥							       ≥
      *    ≥ The response is returned in WS-OPTION (in upper case).    ≥
      *    ≥							       ≥
      *    ≥ To start with the pop-up window higher or lower than row  ≥
      *    ≥ 13 (default); move another value to SLIN and PERFORM      ≥
      *    ≥ CHECK-SETUP THRU CHECK-EXIT.			       ≥
      *    ∆ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕµ
      *    ≥			  ERROR-MESSAGE 		       ≥
      *    √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
      *    ≥ To display an error message with " - Press ANY key" at    ≥
      *    ≥ the end; use PERFORM ERROR-MESSAGE.		       ≥
      *    ≥							       ≥
      *    ≥ To display just the error message; use PERFORM	       ≥
      *    ≥ ERROR-LENGTH THRU ERROR-EXIT.			       ≥
      *    ≥							       ≥
      *    ≥ To display the error message higher or lower (default is  ≥
      *    ≥ line 13) firstly use PERFORM ERROR-SETUP THRU ERROR-POS   ≥
      *    ≥ or PERFORM ERROR-LENGTH THRU ERROR-POS. Move the line     ≥
      *    ≥ number to be used to SLIN and then PERFORM ERROR-DISPLAY  ≥
      *    ≥ THRU ERROR-EXIT.					       ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ

       COPY FUNCTION.CRT.

      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥			  OPT-MESSAGE			       ≥
      *    √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
      *    ≥ This routine is used to allow the OPERATOR to respond to  ≥
      *    ≥ a request for an option, so that the correct action can   ≥
      *    ≥ be performed by the program. The routine will display the ≥
      *    ≥ message in a pop-up window and allow the OPERATOR to      ≥
      *    ≥ respond to the 'question'. 			       ≥
      *    ≥							       ≥
      *    ≥ The option request must be placed in WS-ERR-MES and may   ≥
      *    ≥ not exceed 48 characters. The message will be centred in  ≥
      *    ≥ the window. An example of a message request follows:      ≥
      *    ≥							       ≥
      *    ≥   MOVE "Print transactions (Y/N) [ ]" TO WS-ERR-MES.      ≥
      *    ≥   PERFORM OPT-MESSAGE.				       ≥
      *    ≥							       ≥
      *    ≥ This would be displayed as:			       ≥
      *    ≥	⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø     ≥
      *    ≥	≥	   Print transactions (Y/N) [ ] 	 ≥∞∞   ≥
      *    ≥	¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ∞∞   ≥
      *    ≥	  ∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞   ≥
      *    ≥							       ≥
      *    ≥ The response is returned in WS-OPTION (in upper case).    ≥
      *    ≥							       ≥
      *    ≥ To display the request message higher or lower (default   ≥
      *    ≥ is line 13) move the line number to be used to SLIN and   ≥
      *    ≥ then PEFORM OPT-SETUP THRU OPT-EXIT.		       ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ

       COPY OPTION.CRT.

       COPY LOCKED.REC.

      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥	      ERASE SCREEN FROM SPECIFIED LOCATION	       ≥
      *    √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
      *    ≥ Uses CPOS (CLIN,CCOL) as starting position and increases  ≥
      *    ≥ CLIN by 1 until CLIN reaches the line allocated to exit.  ≥
      *    ≥ The screen is cleared with Column 1 and 80 containing "≥" ≥
      *    ≥ and columns 2 to 79 containing spaces.		       ≥
      *    ≥  eg.						       ≥
      *    ≥	  MOVE 0801	 TO CPOS.			       ≥
      *    ≥	  PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 19.	       ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ

       COPY CLEAR.CRT.

       AA350-LOOKUP    SECTION.
       AA350.
011050	     PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
	     PERFORM GACCNT-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.
       AA399.
             EXIT.

      /    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥	       R E A D	 F I L E S   R O U T I N E S	       ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
007600 AB000	       SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.

       COPY GACCNT.RD.

       COPY GLTRAN.RD.

      /    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥R E W R I T E   &	W R I T E   F I L E S	R O U T I N E S≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
008790 AC000	       SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

       COPY GACCNT.WR.

       COPY GLTRAN.WR.

       COPY GACCNT.LUP.

       COPY APAC.HLP.

      *
      *   D A T E   C H E C K   R O U T I N E
      *
      *   PERFORM THIS ROUTINE WITH THE DATE IN W10-DATE (PIC 99/99/9999)
      *   WITH THE FORMAT AS DD/MM/CCYY
      *
      *   ON EXITING - WS-ERROR WILL BE ZERO (0) IF THE DATE IS CORRECT
      *                OR ONE (1) IF THE DATE IS INCORRECT.
      *

       COPY DATE.CHK.

      /
024350 EA000	     SECTION 5.
024360 EA00.
024370       DISPLAY CLR-SCREEN.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 7		 TO SHADE-COL.
	     MOVE 69		 TO SHADE-WIDTH.
	     MOVE 8		 TO SHADE-LINES.
	     DISPLAY S00.
	     PERFORM SCREEN-SHADOW.
	     DISPLAY "Press any key to continue" AT 1727
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       EA05.
024370	     DISPLAY CLR-SCREEN.
019700	     DISPLAY S09.
019740	     MOVE ZERO		 TO W15-ACCOUNT
				    W15-ENDACC.

019750 EA10.
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
019760	     ACCEPT S17 AT 0513.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE ZERO	 TO W15-ACCOUNT
		     GO TO EA999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     PERFORM AA350
		 WHEN OTHER
		     CALL X"E5" CALL X"E5" CALL X"E5"
		     GO TO EA05
	       END-EVALUATE
	       DISPLAY S17 AT 0513
               IF W15-ACCOUNT = ZERO
015990		   GO TO EA05.
	     DISPLAY CLEAR-L25.
019780     IF W15-ACCOUNT = ZERO
019790         GO TO EA999.
013830	   IF W15-ASER = ZERO
013840	       MOVE WS-ER7	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
013850	       GO TO EA10.
008970	     MOVE W15-ACCOUNT	 TO G-AC.
008980       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
019810	   IF WS-F-ERROR = 8
019820	       MOVE WS-ER6	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
019830         GO TO EA05.
019850	     DISPLAY G-NAME AT 0526
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     PERFORM SAVE-SCREEN.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       GO TO EA10.
      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥ Move G/L account details to temporary store as the G-REC  ≥
      *    ≥ details may be changed while patching transaction details ≥
      *    ≥ if the account number is amended.			       ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
	     MOVE G-REC 	 TO WG-REC.
	     MOVE W15-ACCOUNT	 TO W15-ACNT W15-BEGACC.

       EA11.
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
019760	     ACCEPT S17 AT 0713.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE ZERO	 TO W15-ACCOUNT
		     GO TO EA10
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     PERFORM AA350
		 WHEN OTHER
		     CALL X"E5" CALL X"E5" CALL X"E5"
		     GO TO EA11
	       END-EVALUATE
	       DISPLAY S17 AT 0813
               IF W15-ACCOUNT = ZERO
015990		   GO TO EA10.
	     DISPLAY CLEAR-L25.
019780     IF W15-ACCOUNT = ZERO
019790	       GO TO EA05.
013830	   IF W15-ASER = ZERO
013840	       MOVE WS-ER7	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
013850	       GO TO EA11.
008970	     MOVE W15-ACCOUNT	 TO G-AC W15-ENDACC.
008980       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
019810	   IF WS-F-ERROR = 8
019820	       MOVE WS-ER6	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
019830	       GO TO EA11.
019850	     DISPLAY G-NAME AT 0826
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     PERFORM SAVE-SCREEN.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       GO TO EA11.
      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥ Read the account from which transactions are to be trans- ≥
      *    ≥ ferred from. As the account is used to access the trans-  ≥
      *    ≥ actions, it is necessary to read the account.	       ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
	     MOVE W15-BEGACC	 TO G-AC.
008980       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.

019860 EA15.
	     DISPLAY "Start Date" AT 1202.

       EA15A.
	     ACCEPT S18 AT 1302.
	     MOVE W15-DATE	 TO W10-DATE.
	     PERFORM CHECK-DATE.
027400	   IF WS-ERROR = 1
027410	       MOVE "Invalid date" TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
	       GO TO EA15A.
	     INITIALIZE T-REC.
	     MOVE WG-AC		 TO T-AC.
	     MOVE W15-CC	 TO W15-CN.
	     MOVE W15-YY	 TO W15-YR.
	     MOVE W15-MM	 TO W15-MT.
	     MOVE W15-DD	 TO W15-DY.
020170	     MOVE W15-YMD	 TO T-DATE.
             PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
           IF WS-F-ERROR = 11
	       GO TO EA190.

020080 EA20.
020110       PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
	     COMMIT.
           IF (WS-F-ERROR = 11) OR
	      (T-AC NOT = WG-AC)
	       GO TO EA05.

       EA20A.
020170	     MOVE T-DATE	 TO W15-YMD W15-SYMD.
	     MOVE W15-CN	 TO W15-CC.
	     MOVE W15-YR	 TO W15-YY.
	     MOVE W15-MT	 TO W15-MM.
	     MOVE W15-DY	 TO W15-DD.
	     MOVE W15-DATE	 TO W25-DTE WS-DATE-CHK.
020190	   IF T-CODE = 01 OR 02
	       MOVE "Jnl"	 TO WS-TRAN-SHRT
020210	   ELSE
020220	   IF T-CODE = 03 OR 04
	       MOVE "Chq"	 TO WS-TRAN-SHRT
020210	   ELSE
020220	   IF T-CODE = 05 OR 10
	       MOVE "Bnk"	 TO WS-TRAN-SHRT
020240	   ELSE
020250     IF T-CODE = 06 OR 07
	       MOVE "Dep"	 TO WS-TRAN-SHRT
020240	   ELSE
020250	   IF T-CODE = 08 OR 09
	       MOVE "Eft"	 TO WS-TRAN-SHRT
020270	   ELSE
020250	   IF T-CODE = 11 OR 12
	       MOVE "Pmt"	 TO WS-TRAN-SHRT
020270	   ELSE
020250	   IF T-CODE = 13 OR 14
	       MOVE "Rec"	 TO WS-TRAN-SHRT
020270	   ELSE
	       MOVE "   "	 TO WS-TRAN-SHRT.

020310 EA25.
	     MOVE T-AC		 TO W15-ACCOUNT.
	     MOVE T-CODE	 TO W15-TYPE.
	     MOVE T-REFER	 TO W15-REF.
	     MOVE T-NARATIVE	 TO W15-NAR.
	     MOVE T-VAL 	 TO W15-VAL.
	     PERFORM SAVE-SCREEN-3.
      *    *************************************************************
      *    ****		DISPLAY THE TRANSACTION PATCH SCREEN	    ****
      *    *************************************************************
      *		⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *		≥ Ac No 999999	    Transfer to Ac No 999999 ≥
      *		≥ Date	99/99/9999			     ≥
      *		≥ Code	99  XXX				     ≥
      *		≥ Ref	XXXXXXXX			     ≥
      *		≥ Nar	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ≥
      *		≥ Value ZZZZZZZ9.99CR			     ≥
      *		¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
      *
	     DISPLAY "⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
		      AT 0717 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				   BACKGROUND-COLOR 7
		     "ø" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "≥ Ac No             " AT 0817
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "Transfer to Ac No        "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "≥" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "≥ Date             " AT 0917
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "                          "
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 7
		     "≥" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "≥ Code             " AT 1017
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "                          "
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 7
		     "≥" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "≥ Ref              " AT 1117
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "                          "
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 7
		     "≥" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "≥ Nar                                        "
		      AT 1217 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				   BACKGROUND-COLOR 7
		     "≥" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "≥ Value                                      "
		      AT 1317 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				   BACKGROUND-COLOR 7
		     "≥" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "¿" AT 1417 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 7
		     "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     MOVE 8		 TO SHADE-ROW.
	     MOVE 19		 TO SHADE-COL.
	     MOVE 44		 TO SHADE-WIDTH.
	     MOVE 7		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.

020420 EA30.
	     DISPLAY S10.
023540	     MOVE "Transfer this transaction (Y/N)  [ ]"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     MOVE 16		 TO SLIN.
	     PERFORM OPT-SETUP THRU OPT-EXIT.

       EA35.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO EA05
		 WHEN OTHER
		     CALL X"E5" CALL X"E5" CALL X"E5"
		     GO TO EA35
	       END-EVALUATE.
	   IF NOT(WS-OPTION = "Y" OR "N")
	       CALL X"E5" CALL X"E5" CALL X"E5"
	       GO TO EA30.
	   IF WS-OPTION = "N"
	       GO TO EA20.
      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥ As the account number is being  changed, this transaction ≥
      *    ≥ must be deleted and the amended transaction  must then be ≥
      *    ≥ added. Store the transaction record before deleting, as a ≥
      *    ≥ restart pointer after adding the new record.	       ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
	     MOVE T-REC 	 TO WT-REC.
	     MOVE 1		 TO WS-INDG.
	     PERFORM DELETE-GLTRAN-REC THRU WRITE-GLTRAN-EXIT.
	     MOVE WT-REC	 TO T-REC.
      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥		    A C C O U N T   N U M B E R 	       ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
008970	     MOVE W15-ENDACC	 TO T-AC.
      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥	  Add transaction with the changed account number.     ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
	     PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥ Restart the transaction file from the last point that it  ≥
      *    ≥ read for the transferring from account.		       ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
	     MOVE WT-REC	 TO T-REC.
	     PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
	     PERFORM RESTORE-SCREEN-3.
	     GO TO EA20.

       EA190.
	     MOVE "No transactions on file"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.
	     GO TO EA05.

020610 EA999.
020620       EXIT.

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
      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥	   S e t   u p	 t h e	 F U N C T I O N   k e y s     ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥		       ESC, F1 to F10 keys		       ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥		    PAGE-UP and PAGE-DOWN keys		       ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥		      UP and DOWN ARROW keys		       ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
	     MOVE 2		 TO USER-ACTION.
	     MOVE 3		 TO USER-SETTING.
	     MOVE 5		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥		   A C T I V A T E   M O U S E		       ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
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
      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥	       Set MOUSE key to act as FUNCTION key	       ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥	    Setup the file ID's including path names           ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
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
      *    *** G E N E R A L   L E D G E R   A C C O U N T   F I L E ***
      *    *************************************************************
	     MOVE "GACCNT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GACCNT.
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
      *
      *    ****    Printing of an audit trail will be included later.
      * 	   U S E   P R I N T   U T I L I T Y
      *

		  GO TO ZA999.

	   IF WS-PRN-NO < 3
	       PERFORM ZA210 THRU ZA215
	       IF WS-ERROR = 1
		   GO TO ZA205
	   ELSE
	       DISPLAY "Waiting for printer response" AT 1412
			WITH FOREGROUND-COLOR 14 BLINK.
022530*	     OPEN OUTPUT PRNREP.
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
	       MOVE "Supervisorƒ"  TO WS-WRKHD
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
041870	       MOVE WS-PARKEY	 TO WS-KEY.
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
