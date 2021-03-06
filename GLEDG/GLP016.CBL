      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   **	      *******	  ****	     **      ******    *
      *   **	**  **	      **    **	 **  **     ***     **	  **   *
      *   **	    **	      **    **	**    **     **     **	       *
      *   **	    **	      *******	**    **     **     *******    *
      *   **   ***  **	      **	**    **     **     **	  **   *
      *   **	**  **	      **	 **  **      **     **	  **   *
      *    ******   ********  **	  ****	   ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      * G / L E D G E R   R E C O N C I L E   T R A N S A C T I O N S  *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	       GLP016.
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

       COPY LEDTRN.SL.

000270 DATA DIVISION.
000280 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000300 COPY PARAM.FDE.

000320 COPY GACCNT.FDE.

000340 COPY GLTRAN.FDE.

       COPY LEDTRN.FDE.

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001700 77  WS-PARKEY	   PIC	9(04)	 COMP-5 VALUE 1.
001720 77  WS-GTRKEY       PIC  9(06)    COMP-5 VALUE 1.
001720 77  WS-LTRKEY       PIC  9(06)    COMP-5 VALUE 0.
001690 77  WS-ENQPOS       PIC  9(06)    COMP-5.
001700 77  WS-ENQEND       PIC  9(06)    COMP-5.
001730 77  WS-NETKEY       PIC  9(04)    COMP-5 VALUE 1.
001740 77  WS-KEYSTR       PIC  9(06)    COMP-5.
001750 77  WS-KSTORE       PIC  9(06)    COMP-5.
001760 77  WS-SKSTORE      PIC  9(06)    COMP-5.
001770 77  WS-INKEY        PIC  9(06)    COMP-5.
001780 77  WS-COKEY        PIC  9(06)    COMP-5.
001810 77  WS-RECKEY       PIC  9(04)    COMP-5.
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
001890 77  WS-S6           PIC  9(04)    COMP-5.
001890 77  WS-S9	   PIC	9(04)	  COMP-5.
001880 77  WS-IXS1	   PIC	9(04)	 COMP-5.
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
003050 77  WS-NORM         PIC  X(04).
003060 77  WS-COND         PIC  X(04).
003070 77  WS-EXPP         PIC  X(04).
003080 77  WS-ECAN         PIC  X(04).
003090 77  WS-8LPI       PIC  X(04).
003100 77  WS-6LPI       PIC  X(04).
       77  WS-10CPI        PIC  X(04).
       77  WS-12CPI        PIC  X(04).
       77  WS-17CPI        PIC  X(04).
       77  WS-DBLP         PIC  X(04).
       77  WS-DCAN         PIC  X(04).
002070 77  WS-BATCH        PIC  9(05)    COMP-3.
002080 77  WS-TNEXT        PIC  9(04)    COMP-5.
002080 77  WS-PERIOD	   PIC	9(02) VALUE ZERO.
002130 77  WS-SPC	   PIC	X(30) VALUE SPACES.
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
	       05  FILLER      PIC  X(01) VALUE "│".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "░".
	       05  FILLER      PIC  X(01) VALUE "│".
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
	   03  WS-PROG	       PIC  X(03) VALUE "016".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002330 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

000320 COPY GACCNT.ID.

000340 COPY GLTRAN.ID.

       COPY LEDTRN.ID.

       COPY PARAM.ID.

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

       01  S17.
002970	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC 9(06) USING W15-ACCOUNT AUTO.

       COPY LEDLUP.CRT.

       01  S20.
006250	   03  LINE  2 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
                                 VALUE "RECONCILIATION OF ACCOUNT".
	   03  LINE  4 COLUMN  4 VALUE "Account Number:".
	   03	       COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(06) FROM WS-SPC.
006190	   03  LINE  5 COLUMN  4 VALUE "Account Name  :".
	   03	       COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(30) FROM WS-SPC.
006020	   03  LINE  7 COLUMN  5 VALUE "Date".
006030	   03	       COLUMN 13 VALUE "Trn".
006040     03          COLUMN 18 VALUE "Reference".
006050	   03	       COLUMN 38 VALUE "Narrative/Payee".
006060	   03	       COLUMN 70 VALUE "Value".

       01  S23.
	   03  LINE 12 COLUMN 38 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		       VALUE "┌────────────────────────────────────┐".
	   03  LINE 13 COLUMN 38 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		       VALUE "│".
	   03	       COLUMN 39 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		       VALUE " Account Balance      ".
	   03	       COLUMN 61 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC ZZZZZZZZ9.99- FROM W20-TOTAL.
	   03	       COLUMN 74 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		       VALUE " │".
	   03  LINE 14 COLUMN 38 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		       VALUE "│".
	   03	       COLUMN 39 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		       VALUE " Unreconciled Debits  ".
	   03	       COLUMN 61 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC ZZZZZZZZ9.99 FROM W20-DEBIT.
	   03	       COLUMN 73 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		       VALUE "  │".
	   03  LINE 15 COLUMN 38 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		       VALUE "│".
	   03	       COLUMN 39 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		       VALUE " Unreconciled Credits ".
	   03	       COLUMN 61 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC ZZZZZZZZ9.99- FROM W20-CREDIT.
	   03	       COLUMN 74 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		       VALUE " │".
	   03  LINE 16 COLUMN 38 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		       VALUE "│".
	   03	       COLUMN 39 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		       VALUE " Reconciled Balance   ".
	   03	       COLUMN 61 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC ZZZZZZZZ9.99- FROM W20-RESULT.
	   03	       COLUMN 74 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		       VALUE " │".
	   03  LINE 17 COLUMN 38 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
		       VALUE "└────────────────────────────────────┘".

004870 COPY S99.CRT.

       COPY ERROR.CRT.

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
	     PERFORM NA000.

       AA49.
	     EXIT PROGRAM.

       COPY FUNCTION.CRT.

       COPY LOCKED.REC.

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

007600 AB000           SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.

       COPY GACCNT.RD.

       COPY GLTRAN.RD.

       COPY LEDTRN.RD.

      /
008790 AC000           SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

       COPY GACCNT.WR.

       COPY GLTRAN.WR.

       COPY LEDTRN.WR.

       COPY GACCNT.LUP.

       COPY APAC.HLP.

      /
034780 NA000	     SECTION 5.
034790 NA00.
034800       DISPLAY CLR-SCREEN.
011930       DISPLAY S20.
019710       SUBTRACT 1 FROM W05-MM GIVING WS-S2.
019720     IF WS-S2 = ZERO
019730         MOVE 12 TO WS-S2.
             MOVE W70-BNKGL      TO W15-ACCOUNT.
011940 NA05.
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
011950	     ACCEPT S17 AT 0420.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE ZERO	 TO W15-ACCOUNT
		     GO TO NA999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     PERFORM AA350
		 WHEN OTHER
		     CALL X"E5"
		     GO TO NA05
	       END-EVALUATE
	       DISPLAY S17 AT 0420
               IF W15-ACCOUNT = ZERO
015990		   GO TO NA05.
011970	   IF W15-ACCOUNT = ZERO
011980	       GO TO NA999.
011990     IF W15-ASER = ZERO
013840	       MOVE WS-ER7	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
012010	       GO TO NA05.
012020 NA10.
008970	     MOVE W15-ACCOUNT	 TO G-AC.
008980       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
021840	   IF WS-F-ERROR = 8
013900	       MOVE WS-ER6	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
012060	       GO TO NA05.
013920	     DISPLAY G-NAME AT 0520
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     DISPLAY CLEAR-L25.
	     PERFORM CHECK-CORRECT.
017350	   IF WS-OPTION = "N"
               GO TO NA00.
	     PERFORM NB000.
	   IF WS-ERROR = 1
	       MOVE "No transactions on file"
				 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
	       GO TO NA00.
	     MOVE 13		 TO SHADE-ROW.
	     MOVE 40		 TO SHADE-COL.
	     MOVE 36		 TO SHADE-WIDTH.
	     MOVE 5		 TO SHADE-LINES.
	     DISPLAY S23.
	     PERFORM SCREEN-SHADOW.
016640	     DISPLAY "ENTER" AT 2012
016650		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     " for all Transactions - "
		     "MM" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     " for specific Month".
020030 NA10A.
020040	     ACCEPT W15-MONTH AT 2063
		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 7 HIGHLIGHT AUTO.
020050     IF W15-MONTH > 12
020060         GO TO NA10A.
             INITIALIZE T-REC.
           IF W15-MONTH NOT = ZERO
               MOVE W15-MONTH    TO W15-MT
               MOVE 01           TO W15-DY
               IF W15-MONTH > W05-MM
		   SUBTRACT 1 FROM W05-CY
				 GIVING W15-CCYY
               ELSE
		   MOVE W05-CY	 TO W15-CCYY.
             MOVE G-AC           TO T-AC.
           IF W15-MONTH NOT = ZERO
               MOVE W15-YMD      TO T-DATE.
             PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
           IF WS-F-ERROR = ZERO
               GO TO NA12.

       NA11.
	     MOVE "No transactions on file"
				 TO WS-ERR-STRING.
	     PERFORM ERROR-MESSAGE.
	     GO TO NA05.

       NA12.
             PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
           IF (WS-F-ERROR = 11) OR
              (G-AC NOT = T-AC)
               GO TO NA11.
020120     IF W15-MONTH NOT = ZERO
020130         IF T-MM > W15-MONTH 
                   GO TO NA11.
             OPEN OUTPUT LEDTRN.
             CLOSE LEDTRN.
             OPEN I-O LEDTRN.
             MOVE ZERO           TO WS-LTRKEY.
             DISPLAY "Searching Transactions:" AT 2312.
             DISPLAY T-KEY AT 2336 WITH FOREGROUND-COLOR 11.
             GO TO NA18.

       NA15.
             PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
           IF (WS-F-ERROR = 11) OR
              (G-AC NOT = T-AC)
               GO TO NA20.
020120     IF W15-MONTH NOT = ZERO
020130         IF T-MM > W15-MONTH 
                   GO TO NA20.
             DISPLAY T-KEY AT 2336 WITH FOREGROUND-COLOR 11.

       NA18.
           IF (T-FLAG = "Y") OR (T-SUB = 5)
               GO TO NA15.
             ADD 1               TO WS-LTRKEY.
             MOVE T-REC          TO L-REC.
             MOVE "N"            TO L-FLAG.
             PERFORM WRITE-LEDTRN THRU WRITE-LEDTRN-EXIT.
             GO TO NA15.

       NA20.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
           IF WS-LTRKEY = ZERO
               CLOSE LEDTRN
               DISPLAY "All transactions have been reconciled" AT 2302
                        WITH FOREGROUND-COLOR 14
               GO TO NA05.
             MOVE WS-LTRKEY      TO WS-KEYSTR.
             MOVE 1              TO WS-LTRKEY.

       NA25.
	     DISPLAY WS-BLNK78 AT 0902 WITH FOREGROUND-COLOR 3.
020110       PERFORM READ-LEDTRN THRU READ-LEDTRN-EXIT.
020170       MOVE L-DATE         TO W15-YMD.
	     MOVE W15-CN	 TO W15-CC.
             MOVE W15-YR         TO W15-YY.
             MOVE W15-MT         TO W15-MM.
             MOVE W15-DY         TO W15-DD.
             MOVE W15-DATE       TO W25-DTE.
020180	     DISPLAY W25-DATE AT 0902
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
020190     IF L-CODE = 01 OR 02
020200	       DISPLAY "Jnl" AT 0913
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 5
020210     ELSE
020220     IF L-CODE = 03 OR 04
020230	       DISPLAY "Chq" AT 0913
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 5
020240     ELSE
020250     IF L-CODE = 06 OR 07
020260	       DISPLAY "Dep" AT 0913
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 5
020240     ELSE
020250	   IF L-CODE = 08 OR 09
020260	       DISPLAY "Eft" AT 0913
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 5.
	   IF L-REFER NUMERIC
020290	       MOVE L-REF	 TO W25-REFER
	   ELSE
	       MOVE L-REFER	 TO W25-REF.
020300	     DISPLAY W25-REF AT 0919
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
020310 NA30.
020320	     DISPLAY L-NARATIVE AT 0929
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
020340       MOVE L-VAL          TO W25-V1.
020350	     DISPLAY W25-S7V2 AT 0967
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     DISPLAY "Reconciled " AT 1102
                     "Y" WITH FOREGROUND-COLOR 11 "/"
		     "N" WITH FOREGROUND-COLOR 11
		     ">" L-FLAG WITH FOREGROUND-COLOR 15 "<"
		     " Press " " " WITH FOREGROUND-COLOR 11
                     " to view the transactions or "
                     "U" WITH FOREGROUND-COLOR 11
                     "pdate and exit".
	     DISPLAY "Esc" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to abandon"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
             MOVE L-FLAG         TO WS-OPTION.
020360 NA35.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO NA55
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO NA35
		 WHEN OTHER
		     CALL X"E5"
		     GO TO NA35
	       END-EVALUATE
	   ELSE
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		   IF WS-LTRKEY = 1
		       GO TO NA35
		   ELSE
		       SUBTRACT 1    FROM WS-LTRKEY
		       GO TO NA25
		   END-IF
		 WHEN DOWN-KEY
		   IF WS-LTRKEY NOT < WS-KEYSTR
		       GO TO NA35
		   ELSE
		       GO TO NA40
		   END-IF
	       END-EVALUATE
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION.

	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "Y" OR "N" OR "U")
               GO TO NA35.
           IF WS-OPTION = "U"
               GO TO NA42.
	   IF NOT (WS-OPTION NOT = L-FLAG)
	       IF L-FLAG = "Y"
                   IF L-VAL < ZERO
		       ADD L-VAL TO W20-CREDIT
                   ELSE
		       ADD L-VAL TO W20-DEBIT
		  END-IF
               ELSE
               IF L-VAL < ZERO
		   SUBTRACT L-VAL
				 FROM W20-CREDIT
               ELSE
		   SUBTRACT L-VAL
				 FROM W20-DEBIT.
	     COMPUTE W20-RESULT = W20-TOTAL - W20-DEBIT - W20-CREDIT.
	     DISPLAY S23.
             MOVE WS-OPTION      TO L-FLAG.
             PERFORM REWRITE-LEDTRN THRU WRITE-LEDTRN-EXIT.
       NA40.
           IF WS-LTRKEY < WS-KEYSTR
	       DISPLAY WS-BLNK78 AT 1102 WITH FOREGROUND-COLOR 3
               ADD 1             TO WS-LTRKEY
               GO TO NA25.
             GO TO NA35.
       NA42.
             MOVE 1              TO WS-LTRKEY.
             PERFORM READ-GACCNT-LOCK THRU READ-GACCNT-EXIT.
	     DISPLAY WS-BLNK78 AT 1102 WITH FOREGROUND-COLOR 3.
             DISPLAY "Updating transaction file" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
       NA45.
020110       PERFORM READ-LEDTRN THRU READ-LEDTRN-EXIT.
           IF L-FLAG = "N"
               GO TO NA50.
             MOVE L-KEY          TO T-KEY.
             PERFORM READ-GLTRAN THRU READ-GLTRAN-EXIT.
             MOVE "Y"            TO T-FLAG.
             PERFORM REWRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
       NA50.
           IF WS-LTRKEY < WS-KEYSTR
               ADD 1             TO WS-LTRKEY
               GO TO NA45.
             UNLOCK GACCNT.
       NA55.
             CLOSE LEDTRN.
             GO TO NA00.
       NA999.
             EXIT.

       NB000	       SECTION 5.
       NB00.
	     MOVE G-BAL 	 TO W20-TOTAL W20-RESULT.
	     MOVE ZERO		 TO W20-DEBIT W20-CREDIT.
	     INITIALIZE T-REC.
             MOVE G-AC           TO T-AC.
             PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
	   IF NOT (WS-F-ERROR = ZERO)
	       MOVE 1		 TO WS-ERROR
	       GO TO NB999.
       NB05.
             PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
           IF (WS-F-ERROR = 11) OR
              (G-AC NOT = T-AC)
	       GO TO NB05.
	     DISPLAY "Calculating:" AT 2312.
	     DISPLAY T-KEY AT 2325 WITH FOREGROUND-COLOR 11.
	     GO TO NB15.
       NB10.
             PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
           IF (WS-F-ERROR = 11) OR
              (G-AC NOT = T-AC)
	       GO TO NB20.
	     DISPLAY T-KEY AT 2325 WITH FOREGROUND-COLOR 11.
       NB15.
           IF (T-FLAG = "Y") OR (T-SUB = 5)
	       GO TO NB10.
	   IF T-VAL < ZERO
	       ADD T-VAL	 TO W20-CREDIT
           ELSE
	       ADD T-VAL	 TO W20-DEBIT.
	     SUBTRACT T-VAL	 FROM W20-RESULT.
	     GO TO NB10.

       NB20.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.

       NB999.
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
      *    ****    D O	 N O T	 D I S P L A Y	 E R R O R
      * 	   M E S S A G E S
      *
      *	     MOVE 44		 TO USER-NUMBER.
      *	     MOVE 1		 TO USER-SETTING.
      *	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
      *    PRE-DISPLAY ACCEPT FIELDS BEFORE DATA-ENTRY
      *
      *      MOVE 76		 TO USER-NUMBER.
      *      MOVE 3		 TO USER-SETTING.
      *      MOVE 1		 TO USER-KEYS.
      *      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
      *      CALL CBL_INIT_MOUSE USING MOUSE-HANDLE
      *				       MOUSE-BUTTONS
      *				 RETURNING MOUSE-STATUS.
      *    IF MOUSE-STATUS = ZERO
      *        MOVE "Y"		 TO MOUSE-ATTACHED.
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
	     MOVE "LEDTRN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-LEDTRN.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.

042170 ZA02.
035680       MOVE 1 TO WS-PARKEY.
035690       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
035700       MOVE PAR-COMPANY TO W90-CNAME.
035710       MOVE PAR-DMY TO W05-TODAY.
035720       MOVE PAR-YMD TO W05-TYMD.
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
035920         PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
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
	       MOVE "Supervisor─"  TO WS-WRKHD
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
041870	       MOVE ZERO	 TO WS-KEY
	       MOVE G-AC	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 11
041860         MOVE W02-GLTRAN   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE T-KEY        TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870         MOVE WS-PARKEY TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 39
041860         MOVE W02-LEDTRN   TO WS-FILE
041870         MOVE WS-LTRKEY    TO WS-KEY.
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
