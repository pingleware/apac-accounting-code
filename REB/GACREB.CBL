      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******     ****     ******	*******   *******  *******     *
      *   **	**   **  **   **    **	**    **  **	   **	 **    *
      *   **	    **	  **  **	**   **   **	   **	 **    *
      *   **	    ********  **	******	  *****    *******     *
      *   **   ***  **	  **  **	**   **   **	   **	 **    *
      *   **	**  **	  **  **    **	**    **  **	   **	 **    *
      *    ******   **	  **   ******	**    **  *******  *******     *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     G / L E D G E R   R E C R E A T E	I N D E X	       *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	       GACREB.
000030 AUTHOR.                 J.W. LEMMON (APAC).
000040 DATE-WRITTEN.	       OCTOBER 1991.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1991 - 2008
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

      $SET KEYCOMPRESS "7"
000160	   SELECT GACCNT ASSIGN DISK
			 LOCK EXCLUSIVE
000170                   STATUS WS-STATUS
000180			 ACCESS DYNAMIC
000260			 ORGANIZATION INDEXED
000270			 RECORD G-AC
			 ALTERNATE RECORD G-FSKEY =
			     G-TYPE G-AC.
      $SET NOKEYCOMPRESS

000160	   SELECT GACOLD ASSIGN DISK
			 LOCK EXCLUSIVE
000190			 STATUS WS-STATUS
000200			 ACCESS SEQUENTIAL
000210			 ORGANIZATION SEQUENTIAL.

000270 DATA DIVISION.
000280 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.IFD.

000300 COPY PARAM.IFD.

000320 COPY GACCNT.FD.

      /*****************************************************************
      *                                                                *
      *      G E N E R A L   L E D G E R   F I L E   L A Y O U T       *
      *                                                                *
      *      VALUE FIELDS ALLOW FOR 9 WHOLE NUMBERS.                   *
      *                                                                *
      ******************************************************************
000010 FD  GACOLD    LABEL RECORD IS STANDARD
000020		     VALUE OF FILE-ID IS W02-GACOLD
000220		     RECORDING MODE IS VARIABLE.
000030 01  GO-REC.
000040	   03  GO-AC		PIC  9(06).
000050	   03  GO-ACR		REDEFINES GO-AC.
000060	       05  GO-GR	PIC  9(02).
000070	       05  GO-SR	PIC  9(04).
000080	       05  GO-SER	REDEFINES GO-SR.
000090		   07  GO-SGR	PIC  9(02).
000100		   07  GO-ASER	PIC  9(02).
000110	   03  GO-ACR2		REDEFINES GO-AC.
000120	       05  GO-GRP	PIC  9(01).
000130	       05  FILLER	PIC  9(05).
000140	   03  GO-AC2		REDEFINES GO-AC.
000150	       05  GO-ACNO	PIC  9(04).
000160	       05  FILLER	PIC  9(02).
000170	   03  GO-NAME.
	       05  GO-NKEY	PIC  X(12).
	       05  FILLER	PIC  X(18).
      *
      *    ****   This indicator informs the system of the type of
      *           budget on this account.
      *           0 = Manual figures entered.
      *           1 = Percentage of Total income.
      *           2 = Percentage of Group income.
      *           3 = Percentage of Group/Subgroup income.
      *        
000180	   03  GO-IND		PIC  9(01).
      *
      *    ****   This is the percentage for calculating the budget
      * 	  figures based on GO-IND.
      *
000190	   03  GO-BPER		PIC S9(02)V99  COMP-3.
      *
      *    ****   Budget figures for 12 months (Jan to Dec).
      *
000200	   03  GO-BUD.
000210	       05  GO-BVAL	PIC S9(09)    COMP-3 OCCURS 12.
      *
      *    ****   Monthly Balance for each month of the current year.
      *
000220	   03  GO-MOVE.
000230	       05  GO-VAL	PIC S9(09)V99 COMP-3 OCCURS 12.
000240	   03  GO-DT		PIC S9(09)V99 COMP-3.
000250	   03  GO-CR		PIC S9(09)V99 COMP-3.
000260	   03  GO-BAL		PIC S9(09)V99 COMP-3.
	   03  GO-OBAL	       PIC S9(09)V99 COMP-3.
      *
      *    ****   Monthly Balance for each month of the previous year.
      *
000290	   03  GO-LSTY.
000300	       05  GO-LVAL	PIC S9(09)V99 COMP-3 OCCURS 12.
      *
      *    ****   1 = Income,  2 = Costs,  3 = Expenses.
      * 	  4 = Capital, 5 = Assets, 6 = Liabilities.
      *
	   03  GO-TYPE	       PIC  X(01).
	   03  GO-SEC	       PIC  X(01).
      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001700 77  WS-PARKEY	   PIC	9(04)	 COMP-5 VALUE 1.
001730 77  WS-NETKEY	   PIC	9(04)	 COMP-5 VALUE 1.
001840 77  WS-ISUB         PIC  9(04)    COMP-5.
001880 77  WS-S            PIC  9(04)    COMP-5.
001880 77  WS-S1           PIC  9(04)    COMP-5.
001890 77  WS-S2           PIC  9(04)    COMP-5.
001890 77  WS-S3           PIC  9(04)    COMP-5.
001890 77  WS-S4	   PIC	9(04)	 COMP-5.
001890 77  WS-S6           PIC  9(04)    COMP-5.
001890 77  WS-S9	   PIC	9(04)	  COMP-5.
001970 77  WS-OPTION       PIC  X(01).
002010 77  WS-ERROR        PIC  9(01) VALUE ZERO.
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

002240 01  W00-TREC.
002250     03  W00-DATE    PIC  9(06).
002260     03  W00-NAR     PIC  X(40).
002270     03  W00-CODE    PIC  9(02).
002280     03  W00-VAL     PIC S9(09)V99 COMP-3.
002290     03  W00-PREV    PIC  9(04)    COMP-5.
002300     03  W00-NXT     PIC  9(04)    COMP-5.
002310     03  W00-AC      PIC  9(06)    COMP-3.
002320     03  FILLER      PIC  X(01).

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002330 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

000320 COPY GACCNT.ID.

000970 COPY GACOLD.ID.

       COPY PARAM.ID.

       COPY W05.GL.

       01  W05-TAX-RATES.
000760     03  W05-VAT-RATES.
000770         05  W05-VAT     PIC S9(03)V99 COMP-3 OCCURS 6.
000780     03  W05-TAX-RATES REDEFINES W05-VAT-RATES.
000790         05  W05-VAT-R1  PIC S9(03)V99 COMP-3.
000800         05  W05-VAT-R2  PIC S9(03)V99 COMP-3.
000810         05  W05-VAT-R3  PIC S9(03)V99 COMP-3.
000820         05  W05-VAT-R4  PIC S9(03)V99 COMP-3.
000830         05  W05-VAT-R5  PIC S9(03)V99 COMP-3.
000840         05  W05-VAT-R6  PIC S9(03)V99 COMP-3.
           03  W05-RATE        PIC S9(03)V99 COMP-3.
           03  W05-RTE REDEFINES W05-RATE
                               PIC S9V9(04)  COMP-3.
002630 01  W10-H2.
002640     03  FILLER      PIC  X(27).
002650     03  W10-HEAD    PIC  X(13).
002660 01  W15-INPUT.
002670     03  W15-ACCOUNT PIC  9(06).
002680     03  W15-ACNO1   REDEFINES W15-ACCOUNT.
002690         05  W15-ACNO
002700                     PIC 9(04).
002710         05  W15-SER PIC  9(02).
002720     03  W15-ACNO2   REDEFINES W15-ACCOUNT.
002730         05  W15-GR  PIC 9(02).
002740         05  W15-SGRP
002750                     PIC 9(02).
002760         05  W15-ASER
002770                     PIC 9(02).
002780     03  W15-ACNO3   REDEFINES W15-ACCOUNT.
002790         05  W15-GROUP
002800                     PIC 9(02).
002810         05  W15-REST
002820                     PIC 9(04).
002830     03  W15-ACNO4   REDEFINES W15-ACCOUNT.
002840         05  W15-GRP PIC  9(01).
002850         05  FILLER  PIC  9(05).
002860     03  W15-GL1     PIC  9(06)     COMP-3.
002870     03  W15-GL2     PIC  9(06)     COMP-3.
002880     03  W15-LNK1    PIC  9(04)     COMP-5.
002890     03  W15-LNK2    PIC  9(04)     COMP-5.
002900     03  W15-KEY     PIC  9(04)     COMP-5.
002910     03  W15-KEY2    PIC  9(04)     COMP-5.
002920     03  W15-DATE    PIC  9(06).
002930     03  W15-RDATE   REDEFINES W15-DATE.
002940         05  W15-DD  PIC  9(02).
002950         05  W15-MM  PIC  9(02).
002960         05  W15-YY  PIC  9(02).
           03  W15-YMD     PIC  9(06).
           03  W15-RYMD REDEFINES W15-YMD.
               05  W15-YR  PIC  9(02).
               05  W15-MT  PIC  9(02).
               05  W15-DY  PIC  9(02).
002970     03  W15-REFER   PIC  9(07).
002980     03  W15-NAR     PIC  X(36).
002990     03  W15-VAL     PIC S9(08)V99.
003000     03  W15-BANK    PIC  9(06).
003010     03  W15-MONTH   PIC  9(02).
003020     03  W15-PAYEE   PIC  X(36).
003030     03  W15-PER     PIC S9(03)V99.
003040     03  W15-IND     PIC  9(01).
003050     03  W15-ACNME   PIC  X(30).
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
000430 01  W20-DATE.
000440     03  W20-DAY         PIC  9(02).
000450     03  W20-MONTH       PIC  9(02).
000460     03  W20-YEAR        PIC  9(02).
000470 01  W20-DTE1 REDEFINES W20-DATE.
000480     03  W20-DTE         PIC  9(06).
       01  W21-DATE.
           03  W21-YY          PIC  9(02).
           03  W21-MM          PIC  9(02).
           03  W21-DD          PIC  9(02).
       01  W21-DTE1 REDEFINES W21-DATE.
           03  W21-DTE         PIC  9(06).
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
003480         05  W25-DTE PIC  Z9/99/99.
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
004060     03  W40-YEAR    PIC  9(02) OCCURS 12.

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
           03  W75-SKEY       PIC 9(04) COMP-5
                              OCCURS 18.
004510 01  W80-H3.
004520     03  FILLER      PIC  X(13)      VALUE "BUDGET AS AT ".
004530     03  W80-DTE     PIC  99/99/99.
004540 01  W90-COMP.
004550     03  W90-CNAME   PIC  X(40).

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004560 SCREEN SECTION.

       COPY BLANK.CRT.

000860 01  S00.
000910     03  LINE  2 COLUMN 26 FOREGROUND-COLOR 15
000920		       VALUE "G/LEDGER FILE REBUILD PROGRAM".
000930     03  LINE  4 COLUMN  2 VALUE "Reading :".
000950     03  LINE  6 COLUMN  2 VALUE "Writing :".
001080 01  S04.
001090     03  LINE 12 COLUMN 12 FOREGROUND-COLOR 11
001100		       VALUE "Rebuilt of G/LEDGER file completed".
001110     03  LINE 14 COLUMN 12 FOREGROUND-COLOR 11
001120                 VALUE "Press any key to continue".
001130 01  S05.
001140     03  LINE 12 COLUMN 12 FOREGROUND-COLOR 11
001150                 VALUE "Rebuild unsuccessfull !".
001160     03  LINE 14 COLUMN 12 FOREGROUND-COLOR 11
001170                 VALUE "Reload files from last backup and rekey al
001180-                      "l updates and transactions".
001190     03  LINE 16 COLUMN 12 FOREGROUND-COLOR 11
001200                 VALUE "since creating the last backup disk/s".

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
006540 AA000	       SECTION.
006550 AA00.
001230 AA00.
041220       MOVE 1 TO WS-S1.
021870	     MOVE SPACES TO WS-MID-LNE.
041240 AA02.
041250       MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1 TO WS-S1
041280         GO TO AA02.
             MOVE WS-G9  TO WS-TCH  (1).
             MOVE WS-G10 TO WS-TCH  (80).
             MOVE WS-G11 TO WS-BCH  (1).
             MOVE WS-G12 TO WS-BCH  (80).
             MOVE WS-G14 TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13 TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2  TO WS-TCHR (16) WS-TCHR (41)
021940                      WS-TCHR (64) WS-TCHR (71).
021950       MOVE WS-G3  TO WS-MCHR (16) WS-MCHR (41)
021960                      WS-MCHR (64) WS-MCHR (71)
			    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4  TO WS-BCHR (16) WS-BCHR (41)
021980                      WS-BCHR (64) WS-BCHR (71).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
001250	     DISPLAY CLR-SCREEN.
006560	     PERFORM ZA000.
	   IF WS-ERROR = 9
               GO TO AA50.
001260       DISPLAY S00.

001340 AA05.
001350	     READ GACOLD
               AT END
               GO TO AA10.
	     DISPLAY GO-AC AT 0412 WITH FOREGROUND-COLOR 11
		     " " GO-NAME WITH FOREGROUND-COLOR 11.
001600       PERFORM AW000.
             GO TO AA05.
001450 AA10.
001830	     CLOSE GACCNT
		   GACOLD
                   PARAM
001870             NETWORK.
001910       DISPLAY S04.
             ACCEPT WS-OPTION AT 1439 WITH FOREGROUND-COLOR 15 AUTO.
004990	     CALL "CBL_DELETE_FILE" USING W02-GACOLD
005030				    RETURNING WS-STATUS.

       AA15.
001920       EXIT PROGRAM.

001930 AA50.
001940       DISPLAY S05.
001950       STOP RUN.
      /
      *    THIS ROUTINE WRITES THE DETAILS TO THE NEW FILE.
      *
001960 AW000           SECTION.
011840 AW01.
011870	     MOVE GO-REC	 TO G-REC.
	     DISPLAY G-AC AT 0612 WITH FOREGROUND-COLOR 14
		     " " G-NAME WITH FOREGROUND-COLOR 14.
002310	     WRITE G-REC.
           IF WS-STAT1 NOT = "0"
	       MOVE 8		 TO WS-F-ERROR
               PERFORM WRITE-ERROR.
002440 AW999.
002450       EXIT.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

      /
002460 ZA000	       SECTION.
002470 ZA00.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
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
	     MOVE "GACOLD"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GACOLD.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
002480	     OPEN I-O PARAM.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042130             GO TO ZA200.
002490     IF WS-STATUS NOT = "00"
002500         MOVE 15 TO WS-F-ERROR
002510         PERFORM OPEN-ERROR.
002520       MOVE 1 TO WS-PARKEY.
             READ PARAM.
002540     IF WS-STATUS NOT = "00"
002550         MOVE 15 TO WS-F-ERROR
002560         PERFORM READ-ERROR.
	     MOVE PAR-DMY	 TO W05-TODAY.
042140	     OPEN I-O NETWORK.
042150     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042160             GO TO ZA200.
003050	   IF NOT (WS-STATUS = "00" OR "41")
003060         MOVE 2 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
004990	     CALL "CBL_RENAME_FILE" USING W02-GACCNT
005000					  W02-GACOLD
005030				    RETURNING WS-STATUS.
	   IF NOT (WS-STATUS = X"0000")
	       MOVE "Unable to Rebuild G/LEDGER file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       MOVE 9		 TO WS-ERROR
	       GO TO ZA999-EXIT.
042240	     OPEN INPUT GACOLD.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042260             GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 58 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
042240	     OPEN OUTPUT GACCNT.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042260             GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 8 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
002960       GO TO ZA999-EXIT.
020110 ZA200.
020120       DISPLAY "Files locked - Try later" AT 2412
020130                WITH FOREGROUND-COLOR 14.
020140       DISPLAY "Press any key" AT 2512 
020150		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
020160	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
             MOVE 9 TO WS-ERROR.
003080 ZA999-EXIT.
003090       EXIT.
      /
003100 ZB000-ERROR           SECTION.
      
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
041650	   IF WS-F-ERROR = 8
041660	       MOVE W02-GACCNT TO WS-FILE
041670         MOVE ZERO       TO WS-KEY
041640     ELSE
041650	   IF WS-F-ERROR = 58
041660	       MOVE W02-GACOLD TO WS-FILE
041670         MOVE ZERO       TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870         MOVE WS-PARKEY TO WS-KEY.
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
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170       STOP RUN.
