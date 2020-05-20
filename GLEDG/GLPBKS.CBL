      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   **	      *******	*******   **	**   ******    *
      *   **	**  **	      **    **	**    **  **   **   **	  **   *
      *   **	    **	      **    **	**    **  **  **    **	       *
      *   **	    **	      *******	*******   *****      ******    *
      *   **   ***  **	      **	**    **  **  **	  **   *
      *   **	**  **	      **	**    **  **   **   **	  **   *
      *    ******   ********  **	*******	  **	**   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     G / L E D G E R   S E T U P   B A S I C   A C C O U N T S  *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	       GLPBKS.
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

000140 COPY DEPART.SL.

000140 COPY PARAM.SL.

000160 COPY GACCNT.SL.

000270 DATA DIVISION.
000280 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

       COPY DEPART.FDE.

000300 COPY PARAM.FDE.

000320 COPY GACCNT.FDE.

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001790 77  WS-IXS	    PIC  9(04)	  COMP-5.
001800 77  WS-IXS1          PIC  9(04)    COMP-5.
001810 77  WS-IXS2          PIC  9(04)    COMP-5.
001820 77  WS-IXS3          PIC  9(04)    COMP-5.
001830 77  WS-IXS4          PIC  9(04)    COMP-5.
001700 77  WS-PARKEY	   PIC	9(04)	 COMP-5 VALUE 1.
001730 77  WS-NETKEY       PIC  9(04)    COMP-5 VALUE 1.
001880 77  WS-S1           PIC  9(04)    COMP-5.
001880 77  WS-S2	   PIC	9(04)	 COMP-5.
001880 77  WS-S3	   PIC	9(04)	 COMP-5.
001880 77  WS-S4	   PIC	9(04)	 COMP-5.
001970 77  WS-LANG	   PIC	X(01).
001970 77  WS-OPTION       PIC  X(01).
002010 77  WS-ERROR        PIC  9(01) VALUE ZERO.
002090 77  WS-SPC	   PIC	X(30) VALUE SPACES.
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
	   03  WS-PROG	       PIC  X(03) VALUE "001".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002330 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DEPART.ID.

000320 COPY GACCNT.ID.

       COPY PARAM.ID.

       01  W02-FILE-IDS.
           03  W02-PRINTER.
               05  W02-REPORT PIC X(07) VALUE "GLP001.".
               05  W02-USER   PIC X(05) VALUE SPACES.

       COPY W05.GL.

002630 01  W10-H2.
002640     03  FILLER      PIC  X(27).
002650     03  W10-HEAD    PIC  X(13).

       COPY W15.GL.

       01  W20-TOTALS.
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
003520	   03  W25-RECKEY  PIC	Z(04)9.

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

004540 01  W90-COMP.
004550     03  W90-CNAME   PIC  X(40).

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004560 SCREEN SECTION.

       COPY BLANK.CRT.

006070 01  S10.
006080	   03  LINE  2 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
006090				 VALUE "SETUP BASIC G/L ACCOUNTS".
006100	   03  LINE  5 COLUMN 15 VALUE "Account Number :".
	   03	       COLUMN 32 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC  X(06) FROM WS-SPC.
006110	   03  LINE  6 COLUMN 15 VALUE "Account Name   :".
	   03	       COLUMN 32 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC  X(30) FROM WS-SPC.

       01  S17.
002970	   03  FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC 9(06) USING W15-ACCOUNT AUTO.

004870 COPY S99.CRT.

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
	     PERFORM GB000.

       AA49.
	     EXIT PROGRAM.

       COPY FUNCTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

007600 AB000	       SECTION.

       COPY CONTROL.RD.

       COPY DEPART.RD.

       COPY PARAM.RD.

       COPY GACCNT.RD.

      /
008790 AC000           SECTION.

       COPY CONTROL.WR.

       COPY DEPART.WR.

       COPY PARAM.WR.

       COPY GACCNT.WR.

       COPY APAC.HLP.

      /
022430 GB000	     SECTION 5.
022440 GB00.
022450       DISPLAY CLR-SCREEN.
	     DISPLAY "Generate Chart of Accounts" AT 0228
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
011270	     MOVE 4		 TO WS-NETKEY.
011300       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
	   IF NOT (G-ACCOUNTS = ZERO)
	       MOVE "Chart of accounts exists"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       UNLOCK NETWORK
	       GO TO GB999.
	     DISPLAY "Company Ownership" AT 1004
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "S" AT 1204 WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "hareholders, "
		     "M" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "embers or "
		     "P" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "artners".
	     MOVE "S"		 TO WS-OPTION.
       GB05.
	     ACCEPT WS-OPTION AT 1238
		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 7 HIGHLIGHT UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO GB999
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GB05
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "S" OR "M" OR "P")
	       GO TO GB05.
	     MOVE "E"		 TO WS-LANG.
      *	     DISPLAY "Language Preference" AT 1404
      *		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
      *	     DISPLAY "A" AT 1604 WITH FOREGROUND-COLOR 6 HIGHLIGHT
      *		     "frikaans or "
      *		     "E" WITH FOREGROUND-COLOR 6 HIGHLIGHT
      *		     "nglish".
       GB06.
      *	     ACCEPT WS-LANG AT 1625
      *		    WITH BACKGROUND-COLOR 5
      *			 FOREGROUND-COLOR 7 HIGHLIGHT UPDATE AUTO.
      *	     CALL "CBL_TOUPPER" USING WS-LANG
      *				BY VALUE WS-LENGTH
      *				RETURNING WS-STATUS.
      *	   IF NOT (WS-LANG = "A" OR "E")
      *	       GO TO GB06.
022450	     DISPLAY CLR-SCREEN.
	     DISPLAY "Generate Chart of Accounts" AT 0228
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
022470	     MOVE ZERO TO W15-ACCOUNT W15-IND W15-PER.
	     MOVE SPACE 	 TO W15-SUPER.
	     MOVE 100000	 TO W15-ACCOUNT.
      *	   IF WS-LANG = "A"
      *	       MOVE "INKOSTE REKENINGE"
      *				 TO W15-ACNME
      *	   ELSE
	       MOVE "INCOME ACCOUNTS"
				 TO W15-ACNME.
022460       DISPLAY S10.

022490 GB10.
022510	     DISPLAY S17 AT 0532.
022890	     DISPLAY W15-ACNME AT 0632
022900		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5 UPDATE AUTO.
             ADD 1               TO G-ACCOUNTS.
023350	     INITIALIZE G-REC.
008970	     MOVE W15-ACCOUNT	 TO G-AC.
023170	     MOVE W15-ACNME	 TO G-NAME.
023180	     MOVE W15-IND	 TO G-IND.
	     MOVE W15-SUPER	 TO G-SEC.
023200	     MOVE W15-PER	 TO G-BPER.
023930	     PERFORM WRITE-GACCNT THRU WRITE-GACCNT-EXIT.

       GB15.
	     MOVE 110000	 TO W15-ACCOUNT.
      *	   IF WS-LANG = "A"
      *	       MOVE "VERKOPE"	 TO W12-ACNME
      *	   ELSE
	       MOVE "SALES"	 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 111000	 TO W15-ACCOUNT.
      *	   IF WS-LANG = "A"
      *	       MOVE "VOORRAAD EN DIENSTE"
      *				 TO W12-ACNME
      *	   ELSE
	       MOVE "SALES OF STOCK AND SERVICES"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 111001	 TO W15-ACCOUNT.
      *	   IF WS-LANG = "A"
      *	       MOVE "-Voorraad Verkope"
      *				 TO W12-ACNME
      *	   ELSE
	       MOVE "-Stock Sales"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 111002	 TO W15-ACCOUNT.
      *	   IF WS-LANG = "A"
      *	       MOVE "-Dienste Gelewer"
      *				 TO W12-ACNME
      *	   ELSE
	       MOVE "-Services Rendered"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 200000	 TO W15-ACCOUNT.
      *	   IF WS-LANG = "A"
      *	       MOVE "KOSPRYS VAN VERKOPE REKENINGE"
      *				 TO W15-ACNME
      *	   ELSE
	       MOVE "COST OF SALES ACCOUNTS"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 210000	 TO W15-ACCOUNT.
      *	   IF WS-LANG = "A"
      *	       MOVE "KOSPRYS-VAN-VERKOPE"
      *				 TO W15-ACNME
      *	   ELSE
	       MOVE "COST-OF-SALES"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 211000	 TO W15-ACCOUNT.
      *	   IF WS-LANG = "A"
      *	       MOVE "KOSPRYS  - VOORRAAD VERKOPE"
      *				 TO W15-ACNME.
      *	   ELSE
	       MOVE "COSTS  - STOCK & SERVICE"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 211001	 TO W15-ACCOUNT.
	     MOVE "-Stock"	 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 211005	 TO W15-ACCOUNT.
	     MOVE "-Services (Labour)"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 300000	 TO W15-ACCOUNT.
	     MOVE "EXPENSES / OVERHEADS ACCOUNTS"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 310000	 TO W15-ACCOUNT.
	     MOVE "OPERATING EXPENSES"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 311000	 TO W15-ACCOUNT.
	     MOVE "ADMINISTRATION EXPENSES"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 311003	 TO W15-ACCOUNT.
	     MOVE "-Audit Fees"  TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 311006	 TO W15-ACCOUNT.
	     MOVE "-Bad Debts"	 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 311009	 TO W15-ACCOUNT.
	     MOVE "-Bank Charges"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 311012	 TO W15-ACCOUNT.
	     MOVE "-Cleaning Materials"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 311015	 TO W15-ACCOUNT.
	     MOVE "-Donations"	 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 311018	 TO W15-ACCOUNT.
	     MOVE "-Insurance"	 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 311021	 TO W15-ACCOUNT.
	     MOVE "-Legal Fees"	 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 311024	 TO W15-ACCOUNT.
	     MOVE "-Licences"	 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 311027	 TO W15-ACCOUNT.
	     MOVE "-Maintenance (Motor Vehicles)"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 311030	 TO W15-ACCOUNT.
	     MOVE "-Maintenance (Office Equipment)"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 311033	 TO W15-ACCOUNT.
	     MOVE "-Medical"	 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 311036	 TO W15-ACCOUNT.
	     MOVE "-Occupational Rent"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 311039	 TO W15-ACCOUNT.
	     MOVE "-Postage and Stamps"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 311042	 TO W15-ACCOUNT.
	     MOVE "-Rates and Taxes"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 311045	 TO W15-ACCOUNT.
	     MOVE "-Railage/Freight Charges"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 311048	 TO W15-ACCOUNT.
	     MOVE "-Regional Service Council"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 311051	 TO W15-ACCOUNT.
	     MOVE "-Salaries and Wages"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 311054	 TO W15-ACCOUNT.
	     MOVE "-Staff Refreshments"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 311057	 TO W15-ACCOUNT.
	     MOVE "-Stationery"  TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 311060	 TO W15-ACCOUNT.
	     MOVE "-Telephone and Fax"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 311063	 TO W15-ACCOUNT.
	     MOVE "-Unemployment Insurance"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 311066	 TO W15-ACCOUNT.
	     MOVE "-Water and Electricity"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 312000	 TO W15-ACCOUNT.
	     MOVE "SELLING EXPENSES"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 312003	 TO W15-ACCOUNT.
	     MOVE "-Advertising" TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 312006	 TO W15-ACCOUNT.
	     MOVE "-Commision"	 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 312009	 TO W15-ACCOUNT.
	     MOVE "-Discount Allowed"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 312012	 TO W15-ACCOUNT.
	     MOVE "-Salaries"	 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 312018	 TO W15-ACCOUNT.
	     MOVE "-Travelling Expenses"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 313000	 TO W15-ACCOUNT.
	     MOVE "DEPRECIATION" TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 313003	 TO W15-ACCOUNT.
	     MOVE "-Computer Equipment"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 313006	 TO W15-ACCOUNT.
	     MOVE "-Motor Vehicles"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 313009	 TO W15-ACCOUNT.
	     MOVE "-Office Equipment"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 313012	 TO W15-ACCOUNT.
	     MOVE "-Office Furniture"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 314000	 TO W15-ACCOUNT.
	     MOVE "OPERATING COSTS"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 314003	 TO W15-ACCOUNT.
	     MOVE "-Adjustments (Stock)"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 314006	 TO W15-ACCOUNT.
	     MOVE "-Stock Write-offs"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 390000	 TO W15-ACCOUNT.
	     MOVE "SUNDRY ACCOUNTS"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 391000	 TO W15-ACCOUNT.
	     MOVE "SUNDRY INCOME"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 391003	 TO W15-ACCOUNT.
	     MOVE "-Bad Debts Recovered"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 391006	 TO W15-ACCOUNT.
	     MOVE "-Discount Received"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 391009	 TO W15-ACCOUNT.
	     MOVE "-Interest Received"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 391012	 TO W15-ACCOUNT.
	     MOVE "-Rebates"	 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 391015	 TO W15-ACCOUNT.
	     MOVE "-Rent Received"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 400000	 TO W15-ACCOUNT.
	     MOVE "CAPITAL EMPLOYED"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 410000	 TO W15-ACCOUNT.
	   IF WS-OPTION = "S"
	       MOVE "SHAREHOLDERS CONTRIBUTIONS"
				 TO W15-ACNME
	   ELSE
	   IF WS-OPTION = "M"
	       MOVE "MEMBERS CONTRIBUTIONS"
				 TO W15-ACNME
	   ELSE
	       MOVE "PARTNERS CONTRIBUTIONS"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 411000	 TO W15-ACCOUNT.
	   IF WS-OPTION = "S"
	       MOVE "SHAREHOLDERS"
				 TO W15-ACNME
	   ELSE
	   IF WS-OPTION = "M"
	       MOVE "MEMBERS"	 TO W15-ACNME
	   ELSE
	       MOVE "PARTNERS"	 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 420000	 TO W15-ACCOUNT.
	     MOVE "LOAN CAPITAL" TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 421000	 TO W15-ACCOUNT.
	   IF WS-OPTION = "S"
	       MOVE "SHAREHOLDERS (Loan Accounts)"
				 TO W15-ACNME
	   ELSE
	   IF WS-OPTION = "M"
	       MOVE "MEMBERS (Loan Accounts)"
				 TO W15-ACNME
	   ELSE
	       MOVE "PARTNERS (Loan Accounts)"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 422000	 TO W15-ACCOUNT.
	     MOVE "LONG TERM LOANS"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 430000	 TO W15-ACCOUNT.
	   IF WS-OPTION = "S"
	       MOVE "SHAREHOLDERS INTERESTS"
				 TO W15-ACNME
	   ELSE
	   IF WS-OPTION = "M"
	       MOVE "MEMBERS INTERESTS"
				 TO W15-ACNME
	   ELSE
	       MOVE "PARTNERS INTERESTS"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 431000	 TO W15-ACCOUNT.
	     MOVE "RESERVES"	 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 431003	 TO W15-ACCOUNT.
	     MOVE "-Net Profit/Loss (Current)"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 431006	 TO W15-ACCOUNT.
	     MOVE "-Unapropriated Profit"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 431009	 TO W15-ACCOUNT.
	     MOVE "-Capital Reserve"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 500000	 TO W15-ACCOUNT.
	     MOVE "EMPLOYMENT OF CAPITAL"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 510000	 TO W15-ACCOUNT.
	     MOVE "ASSETS"	 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 511000	 TO W15-ACCOUNT.
	     MOVE "FIXED ASSETS" TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 512000	 TO W15-ACCOUNT.
	     MOVE "MOTOR VEHICLES"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 513000	 TO W15-ACCOUNT.
	     MOVE "FURNITURE AND FITTINGS"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 513003	 TO W15-ACCOUNT.
	     MOVE "-Office Furniture"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 514000	 TO W15-ACCOUNT.
	     MOVE "OFFICE MACHINES/COMPUTERS"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 514003	 TO W15-ACCOUNT.
	     MOVE "-Computers"	 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 514006	 TO W15-ACCOUNT.
	     MOVE "-Office Machines (Fax/Copiers)"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 520000	 TO W15-ACCOUNT.
	     MOVE "CURRENNT STOCK"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 521000	 TO W15-ACCOUNT.
	     MOVE "STOCK AT COST"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 521001	 TO W15-ACCOUNT.
	     MOVE "-Trading Stock"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 530000	 TO W15-ACCOUNT.
	     MOVE "CURRENT DEBTORS"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 531000	 TO W15-ACCOUNT.
	     MOVE "DEBTORS"	 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 531001	 TO W15-ACCOUNT.
	     MOVE "-Trade Debtors"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 531051	 TO W15-ACCOUNT.
	     MOVE "-Sundry Debtors"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 531099	 TO W15-ACCOUNT.
	     MOVE "-Debtors Suspense Account"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 540000	 TO W15-ACCOUNT.
	     MOVE "BANK ACCOUNTS AND FUNDS"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 541000	 TO W15-ACCOUNT.
	     MOVE "BANK CURRENT ACCOUNTS"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 541001	 TO W15-ACCOUNT.
	     MOVE "-Bank Name [Account No]"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 542000	 TO W15-ACCOUNT.
	     MOVE "BANK SAVINGS AND FIXED DEPOSITS"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 543000	 TO W15-ACCOUNT.
	     MOVE "INVESTMENTS"  TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 544000	 TO W15-ACCOUNT.
	     MOVE "CASH/CHEQUES ON HAND"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 544001	 TO W15-ACCOUNT.
	     MOVE "-Cash Sales"  TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 544011	 TO W15-ACCOUNT.
	     MOVE "-Cash Float"  TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 544021	 TO W15-ACCOUNT.
	     MOVE "-Debtor Receipts"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 544031	 TO W15-ACCOUNT.
	     MOVE "-Petty Cash"  TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 600000	 TO W15-ACCOUNT.
	     MOVE "LIABILITIES"  TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 610000	 TO W15-ACCOUNT.
	     MOVE "CREDITORS"	 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 611000	 TO W15-ACCOUNT.
	     MOVE "CURRENT CREDITORS"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 611001	 TO W15-ACCOUNT.
	     MOVE "-Trade Creditors"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 611051	 TO W15-ACCOUNT.
	     MOVE "-Sundry Creditors"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 611099	 TO W15-ACCOUNT.
	     MOVE "-Creditors Suspense Account"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 612000	 TO W15-ACCOUNT.
	     MOVE "RATES AND TAXES"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 612001	 TO W15-ACCOUNT.
	     MOVE "-Receiver of Revenue"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 612011	 TO W15-ACCOUNT.
	     MOVE "-City Council"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 612021	 TO W15-ACCOUNT.
	     MOVE "-Customs and Excise"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 613000	 TO W15-ACCOUNT.
	     MOVE "RATES AND TAXES"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 613001	 TO W15-ACCOUNT.
	     MOVE "-VAT Control Account"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 613002	 TO W15-ACCOUNT.
	     MOVE "-Input VAT"	 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 613003	 TO W15-ACCOUNT.
	     MOVE "-Output VAT"  TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 620000	 TO W15-ACCOUNT.
	     MOVE "PROVISION/SUSPENSE ACCOUNTS"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 621000	 TO W15-ACCOUNT.
	     MOVE "PROVISIONS"	 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 621001	 TO W15-ACCOUNT.
	     MOVE "-Audit Fees"  TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 621011	 TO W15-ACCOUNT.
	     MOVE "-Bad Debts"	 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 621021	 TO W15-ACCOUNT.
	     MOVE "-Cheques not Presented"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 621031	 TO W15-ACCOUNT.
	     MOVE "-Taxation"	 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 621035	 TO W15-ACCOUNT.
	     MOVE "-Telephone and Fax"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 621041	 TO W15-ACCOUNT.
	     MOVE "-Water and Electricity"
				 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 622000	 TO W15-ACCOUNT.
	     MOVE "SUSPENSE"	 TO W15-ACNME.
	     PERFORM GB10.
	     MOVE 622001	 TO W15-ACCOUNT.
	     MOVE "-Integration Suspense"
				 TO W15-ACNME.
	     PERFORM GB10.
	     DISPLAY "Updating Integration Links" AT 0815
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE 6		 TO WS-PARKEY.
019710	     PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
	     MOVE 531001	 TO PAR-DEBGL.
	     MOVE 611001	 TO PAR-CREGL.
	     MOVE 311006	 TO PAR-INTGL.
	     MOVE 541001	 TO PAR-BNKGL.
	     MOVE 431003	 TO PAR-UNPROF.
	     MOVE 311045	 TO PAR-RLGL.
	     MOVE 391006	 TO PAR-DSGL.
	     MOVE 314003	 TO PAR-ADJGL.
	     MOVE 314006	 TO PAR-REDGL.
	     MOVE 622001	 TO PAR-SUSGL.
	     MOVE 544031	 TO PAR-PETGL.
	     PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
	     MOVE 7		 TO WS-PARKEY.
019710	     PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
	     MOVE 544001	 TO PAR-CASH-GL.
	     PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
	     MOVE 11		 TO WS-PARKEY.
	     DISPLAY "Updating Debtor Integration Links" AT 1015
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.

       GB20.
	     PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
002790	   IF WS-PARKEY = 11
002810	       MOVE 111001	 TO PAR-T-GL (1)
002820	       MOVE 544021	 TO PAR-T-GL (2).
002900     IF WS-PARKEY = 12
002810	       MOVE 531099	 TO PAR-T-GL (1)
002820	       MOVE 531099	 TO PAR-T-GL (2).
002900	   IF WS-PARKEY = 13
002810	       MOVE 531099	 TO PAR-T-GL (1)
002820	       MOVE 531099	 TO PAR-T-GL (2).
	   IF WS-PARKEY = 17
002810	       MOVE 613001	 TO PAR-T-GL (1)
002820	       MOVE 613001	 TO PAR-T-GL (2).
002990	   IF WS-PARKEY = 18
002810	       MOVE 531099	 TO PAR-T-GL (1)
002820	       MOVE 544021	 TO PAR-T-GL (2).
003080	   IF WS-PARKEY = 19
002810	       MOVE 391009	 TO PAR-T-GL (1).
003170	   IF WS-PARKEY = 20
002810	       MOVE 312009	 TO PAR-T-GL (1)
002820	       MOVE 391009	 TO PAR-T-GL (2).
	     PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
003280	   IF WS-PARKEY < 20
003290	       ADD 1		 TO WS-PARKEY
003300	       GO TO GB20.
	     MOVE 191		 TO WS-PARKEY.
	     DISPLAY "Updating Creditor Integration Links" AT 1215
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.

       GB25.
	     PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
	   IF WS-PARKEY = 191
	       MOVE 611099	 TO PAR-C-GL (1)
	       MOVE 611099	 TO PAR-C-GL (2).
           IF WS-PARKEY = 192
	       MOVE 611099	 TO PAR-C-GL (1)
	       MOVE 611099	 TO PAR-C-GL (2).
           IF WS-PARKEY = 193
	       MOVE 611099	 TO PAR-C-GL (1)
	       MOVE 611099	 TO PAR-C-GL (2).
           IF WS-PARKEY = 194
	       MOVE 391006	 TO PAR-C-GL (2).
           IF WS-PARKEY = 195
	       MOVE 611099	 TO PAR-C-GL (1)
	       MOVE 611099	 TO PAR-C-GL (2).
	     PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
003280	   IF WS-PARKEY < 195
003290	       ADD 1		 TO WS-PARKEY
003300	       GO TO GB25.
	     MOVE " !"		 TO DPT-CODE.
	     DISPLAY "Updating Sales Ledger Integration Links" AT 1415
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       GO TO GB35.

       GB30.
	     PERFORM READ-DEPART-NEXT-LOCK THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       GO TO GB35.
	   IF DPT-CODE = "YYYY"
	       MOVE 312009	 TO DPT-DISC-GL
	   ELSE
	   IF DPT-SCDE = "VAT"
	       MOVE 613003	 TO DPT-O-VAT-GL
	       MOVE 613002	 TO DPT-I-VAT-GL
	   ELSE
	       MOVE 111001	 TO DPT-SALE-GL DPT-DISC-GL
	       MOVE 211001	 TO DPT-COST-GL
	       MOVE 521001	 TO DPT-PUR-GL.
	     PERFORM REWRITE-DEPART-UNLOCK THRU WRITE-DEPART-EXIT.
	     GO TO GB30.

       GB35.
011300	     PERFORM REWRITE-CONTROL-UNLOCK THRU WRITE-CONTROL-EXIT.
	     DISPLAY CLR-SCREEN.
	     DISPLAY "Must the System be Integrated (" AT 1212
		     "Y" WITH HIGHLIGHT "/"
		     "N" WITH HIGHLIGHT ")".
	     MOVE 6		 TO WS-PARKEY.
	     PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.

       GB40.
	     ACCEPT PAR-INTEGRATE AT 1248
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING PAR-INTEGRATE
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (PAR-INTEGRATE = "N" OR "Y")
	       GO TO GB40.
	     PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.

024330 GB999.
024340       EXIT.

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
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "GACCNT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GACCNT.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	   IF LS-USER NUMERIC
	       IF LS-USE > ZERO AND < 110
		   MOVE LS-USE	 TO WS-USUB.
042170 ZA02.
035680	     MOVE 1		 TO WS-PARKEY.
035690       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
035700	     MOVE PAR-COMPANY	 TO W90-CNAME.
035710	     MOVE PAR-DMY	 TO W05-TODAY.
035720	     MOVE PAR-YMD	 TO W05-TYMD.
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
036360 ZB000	     SECTION 9.
      
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
056420	   ELSE
056430     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041640	   ELSE
041810     IF WS-F-ERROR = 8
041820         MOVE W02-GACCNT TO WS-FILE
041830	       MOVE ZERO	 TO WS-KEY
	       MOVE G-AC	 TO WS-KEYX
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
