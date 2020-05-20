      $SET LINKCOUNT"256"
      ******************************************************************
      *                                                                *
      *   *******   **	      *******	  ****	    ****     ******    *
      *   **	**  **	      **    **	 **  **    **  **   **	  **   *
      *   **	**  **	      **    **	**    **  **	**  **	       *
      *   **	**  **	      *******	**    **  **	**  *******    *
      *   **	**  **	      **	**    **  **	**  **	  **   *
      *   **	**  **	      **	 **  **    **  **   **	  **   *
      *   *******   ********  **	  ****	    ****     ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E L I V E R Y   R O U T E   M A I N T E N A N C E        *
      *                                                                *
      *     VERSION 8.14.03 - May 2010				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DLP006.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   OCTOBER 1990.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1990 - 2010
				     by James William Lemmon.

		   All rights reserved.

000050 SECURITY.   PROGRAM MATERIAL IS THE SOLE PROPERTY OF
000060		   James William Lemmon.(Id No. 4412165050082).

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080                 CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000100 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

000120 COPY PARAM.SL.

       COPY ROUTE.SL.

000160	   SELECT PRNREP  ASSIGN W02-PRINTER
                          ORGANIZATION LINE SEQUENTIAL.

000220 DATA DIVISION.
000230 FILE SECTION.

       COPY APACFIDS.FDE.

000250 COPY PARAM.FDE.

       COPY ROUTE.FDE.

000240 FD  PRNREP    LABEL RECORD OMITTED
000420		     LINAGE IS WS-PGE-LENGTH.
000260 01  REP-LINE1.
000270     03  REP-DETAIL1     PIC X(80).
000280 01  REP-LINE2.
000290     03  REP-DETAIL2     PIC X(80).
000300 01  REP-LINE3.
000310     03  REP-PAGE        PIC X(06).
000320     03  REP-P-NO        PIC Z(03)9.
000330     03  FILLER          PIC X(10).
000340     03  REP-COMPANY     PIC X(40).
000350     03  FILLER          PIC X(06).
000360     03  REP-DATE        PIC X(06).
000370     03  REP-DTE         PIC Z9/99/99.
000380 01  REP-LINE4.
000390	   03  FILLER	       PIC X(13).
000400	   03  REP-ROUTE       PIC X(08).
000410     03  FILLER          PIC X(06).
000420	   03  REP-DESC        PIC X(40).
000430	   03  FILLER	       PIC X(13).
      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   **
      *         **    *    **   **    **   **   **    **  **
      *         **   ***   **   **    **   ******     *****
      *          ** ** ** **    **    **   **   **    **  **
      *           ***   ***     **    **   **    **   **   **
      *            *     *       ******    **    **   **    **
      *
002780 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
003260 77  WS-S1	   PIC 9(04)	 COMP-5.
003270 77  WS-S2           PIC 9(04)     COMP-5.
003280 77  WS-S3           PIC 9(04)     COMP-5.
003290 77  WS-S4           PIC 9(04)     COMP-5.
003300 77  WS-S5           PIC 9(04)     COMP-5.
003310 77  WS-S6           PIC 9(04)     COMP-5.
003320 77  WS-S7           PIC 9(04)     COMP-5.
003330 77  WS-S8           PIC 9(04)     COMP-5.
002850 77  WS-IXS1	   PIC 9(04)	 COMP-5.
002860 77  WS-IXS2         PIC 9(04)     COMP-5.
002870 77  WS-IXS3         PIC 9(04)     COMP-5.
002880 77  WS-IXS4         PIC 9(04)     COMP-5.
002890 77  WS-MSUB         PIC 9(04)     COMP-5.
002900 77  WS-PARKEY       PIC 9(04)     COMP-5.
002960 77  WS-NTP          PIC 9(01).
002970 77  WS-LINES        PIC 9(02).
002980 77  WS-PAGE	   PIC 9(04)	 COMP-5.
002990 77  WS-OPTION       PIC X(01).
002990 77  WS-AGE          PIC X(01).
002990 77  WS-SEQ          PIC X(01).
003000 77  WS-CREDIT       PIC X(01).
002990 77  WS-ZERO         PIC X(01).
001820 77  WS-SKIP         PIC  X(01) VALUE "N".
       77  WS-NUM          PIC Z9.
003020 77  WS-ADDR         PIC X(01).
003030 77  WS-ERROR        PIC 9(01).
003040 77  WS-ERROR1       PIC 9(01).
003110 77  WS-EXPAND	   PIC X(01)	 VALUE " ".
003180 77  WS-COMP         PIC 9(01).
003180 77  WS-IND	   PIC 9(01).
003190 77  WS-TYPE         PIC X(01).
003200 77  WS-HEAD         PIC X(01).
003240 77  WS-MESS         PIC X(02).
003250 77  WS-ACNO         PIC X(06).
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
003390 77  WS-A 	   PIC X(19) VALUE "Any key to continue".
       77  TODAY-DDMMYY	   PIC	9(06) COMP-5.
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
	   03  WS-MDDT-LNE REDEFINES WS-MID-LNE.
	       05  WS-MDT1     PIC  X(01).
	       05  WS-MDT78    PIC  X(78).
	       05  WS-MDT80    PIC  X(01).
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
003240     03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
           03  WS-MM-LINE.
               05  FILLER  PIC  X(01) VALUE X"B3".
               05  FILLER  PIC  X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

003440 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "DLP".
	   03  WS-PROG	       PIC  X(03) VALUE "006".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003520 01  W02-FID.

       COPY APACFIDS.ID.

       COPY PARAM.ID.

       COPY ROUTE.ID.

	   03  W02-PRINTER.
	       05  W02-REPORT PIC X(07) VALUE "DLP006.".
               05  W02-USER   PIC X(05) VALUE SPACES.

003730 01  W12-DATE.
003740     03  W12-TODAY       PIC 9(06).
003750     03  W12-DATE-DMY REDEFINES W12-TODAY.
003760         05  W12-DAY     PIC 9(02).
003770         05  W12-MONTH   PIC 9(02).
003780         05  W12-YEAR    PIC 9(02).
003790     03  W12-T-YMD       PIC 9(06).
003800     03  W12-DATE-YMD REDEFINES W12-T-YMD.
003810         05  W12-YY      PIC 9(02).
003820         05  W12-MM      PIC 9(02).
003830         05  W12-DD      PIC 9(02).

003930 01  W25-CALCS.
002860     03  W25-RESULT      PIC 9(05)V99.
002870     03  W25-RESULT1 REDEFINES W25-RESULT.
002880         05  W25-WHOLE   PIC 9(05).
002890         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(04).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).
003980     03  W25-TOTAL       PIC S9(07)V99 COMP-3.
003990     03  W25-VALUE       PIC S9(07)V99 COMP-3.

000030 01  W30-RTE.
	   03  W30-ROUTE       PIC  X(08).
	   03  W30-DESC        PIC  X(40).

       COPY W40.WS.

       COPY FUNCTION.WS.

005170 COPY W50.WS.

005830 01  W75-KEYS.
005840     03  W75-S          PIC 9(02) COMP-5.
005850     03  W75-S1         PIC 9(02) COMP-5.
           03  W75-S2         PIC 9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
005860	   03  W75-DNO OCCURS 18.
	       05  W75-WHCDE  PIC  X(06).
           03  W75-ITEMS OCCURS 18.
	       05  W75-ROUTE  PIC  X(08).

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
005990 SCREEN SECTION.

       COPY BLANK.CRT.

       01  S01.
	   03  LINE  2 COLUMN 27 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
				"DELIVERY ROUTE MAINTENANCE".
	   03  LINE  4 COLUMN 10 VALUE "Code       ".
	   03  LINE  6 COLUMN 10 VALUE "Description".

       01  S02.
	   02  S02A.
	   03  LINE  4 COLUMN 22 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC X(08) USING W30-ROUTE AUTO.
	   02  S02B.
	   03  LINE  6 COLUMN 22 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC X(40) USING W30-DESC AUTO.

       01  S10.
003530	   03  LINE  2 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
003540				 VALUE "DELIVERY ROUTE OPTIONS".
	   03  LINE  4 COLUMN 12 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "M".
	   03	       COLUMN 13 VALUE "aintain, ".
	   03	       COLUMN 22 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "P".
	   03	       COLUMN 23 VALUE "rint or ".
	   03	       COLUMN 31 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "E".
	   03	       COLUMN 32 VALUE "xit ? > <".

       COPY ROUTE.CRT.

       COPY S99.CRT.

       COPY OPT.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
006900 AA00-MAIN	      SECTION.
006910 AA00-INIT.
003240	   IF LS0-PALEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006920	     PERFORM ZA00-INIT.
	     PERFORM BA000.
	     CLOSE ROUTE
013040		   PRNREP.

       AA49.
006950	     EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
       AA50.
	     PERFORM SAVE-SCREEN.
	     PERFORM ROUTE-LOOKUP.
	     PERFORM RESTORE-SCREEN.
       AA99.
             EXIT.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

      /
      *    ****    READ FILES ROUTINES
      *
007340 AC000-READ              SECTION.

       COPY PARAM.RD.

       COPY ROUTE.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010170 AD000             SECTION.

       COPY ROUTE.WR.

       COPY PARAM.WR.

       COPY ROUTE.LUP.

       COPY APAC.HLP.

       BA000	       SECTION.
       BA00.
007330	     DISPLAY CLR-SCREEN.
007340       DISPLAY S10.
	     MOVE "M"		 TO WS-OPTION.
       BA05.
	     ACCEPT WS-OPTION AT 0439
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA999
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BA05
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "E" OR "M" OR "P")
	       GO TO BA05.
	   IF WS-OPTION = "M"
	       PERFORM DA000
	   ELSE
	   IF WS-OPTION = "P"
	       PERFORM EA000
	   ELSE
	       GO TO BA999.
	     GO TO BA00.
       BA999.
	     EXIT.

      /
008930 CA155-GET-ROUTE	       SECTION 2.
008940 CA155-ROUTE.
012010	     MOVE W30-ROUTE	 TO RTE-ROUTE.
014330	     PERFORM READ-ROUTE THRU READ-ROUTE-EXIT.
	   IF WS-F-ERROR = 30
               MOVE 1            TO WS-ERROR
014380     ELSE
014390	   IF W30-ROUTE NOT = RTE-ROUTE
014400         MOVE 2            TO WS-ERROR
014410     ELSE
014420	       MOVE 0		 TO WS-ERROR.

009100 CA160-EXIT.
009110       EXIT.

      /
      *    ****    D E L I V E R Y   R O U T E	 M A I N T E N A N C E
      *
037050 DA000	       SECTION 50.
037060 DA00.
020320       DISPLAY CLR-SCREEN.
020330	     DISPLAY S01.
	     INITIALIZE W30-RTE.
	     DISPLAY S02.
020370	     MOVE SPACES	 TO W30-ROUTE.
020360 DA05.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Route Lookup "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
020380	     ACCEPT S02A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W30-ROUTE
		     GO TO DA999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
016560		     PERFORM AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA05
	       END-EVALUATE
016570	       DISPLAY S02A
016580	       IF W30-ROUTE = SPACES
016590		   GO TO DA05.
	     PERFORM CLEAR-L50.
020390	   IF W30-ROUTE = SPACES
020400	       GO TO DA999.
	     MOVE 8		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W30-ROUTE
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
020410	     PERFORM CA155-GET-ROUTE.
	   IF WS-ERROR = ZERO
	       MOVE ZERO	 TO WS-IND
	       MOVE RTE-RECORD1	 TO W30-RTE
	   ELSE
020420     IF WS-ERROR = 1
026950	       MOVE 1		 TO WS-IND
	   ELSE
020450     IF WS-ERROR = 2
026950	       MOVE "Route File corrupted"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
020470	       GO TO DA05.

       DA10.
	     ACCEPT S02B.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W30-ROUTE
		     GO TO DA00
	       END-EVALUATE.
	   IF W30-DESC = SPACES
	       GO TO DA00.

       DA15.
	   IF WS-IND = ZERO
014330	       PERFORM READ-ROUTE-LOCK THRU READ-ROUTE-EXIT.
	     MOVE W30-RTE	 TO RTE-RECORD1.
	   IF WS-IND = ZERO
	       PERFORM REWRITE-ROUTE-UNLOCK THRU WRITE-ROUTE-EXIT
	   ELSE
	       PERFORM WRITE-ROUTE THRU WRITE-ROUTE-EXIT.
	     GO TO DA00.

       DA999.
	     EXIT.

      /
      *    ****    D E L I V E R Y   R O U T E	 L I S T I N G
      *
037050 EA000	       SECTION 51.
037060 EA00.
020320       DISPLAY CLR-SCREEN.
	     DISPLAY "Busy Printing" AT 1212
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT BLINK.
	     MOVE ZERO		 TO WS-PAGE.
014740 EA05.
014750       MOVE SPACES         TO REP-DETAIL1.
014760       MOVE "PAGE:"        TO REP-PAGE.
             ADD  1              TO WS-PAGE.
014770       MOVE WS-PAGE        TO REP-P-NO.
014780       MOVE "DATE:"        TO REP-DATE.
014790       MOVE W12-TODAY      TO REP-DTE.
014800	     MOVE "DELIVERY ROUTES"
                                 TO REP-COMPANY.
014810       WRITE REP-LINE1 BEFORE 2 LINES.
014820       MOVE SPACES         TO REP-DETAIL2.
014830	     MOVE "CODE"	 TO REP-ROUTE.
014840	     MOVE "DESCRIPTION"  TO REP-DESC.
014850       WRITE REP-LINE1 BEFORE 2 LINES.
014860	     MOVE SPACES	 TO REP-DETAIL2.

       EA10.
	     MOVE "       !"	 TO RTE-ROUTE.
	     PERFORM START-AT-ROUTE THRU READ-ROUTE-EXIT.
	   IF WS-F-ERROR = 30
	       GO TO EA20.

       EA15.
	     PERFORM READ-ROUTE-NEXT THRU READ-ROUTE-EXIT.
	   IF WS-F-ERROR = 30
	       GO TO EA20.
	     MOVE RTE-ROUTE	 TO REP-ROUTE.
	     MOVE RTE-DESC	 TO REP-DESC.
	   IF LINAGE-COUNTER < (WS-PRN-LENGTH - 2)
014930         WRITE REP-LINE1 BEFORE 2 LINES
           ELSE
014930         WRITE REP-LINE1 BEFORE PAGE
	       PERFORM EA05.
             PERFORM PRINT-ABORT.
           IF WS-OPTION = "Y"
014970         MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL2
014980         WRITE REP-LINE1 BEFORE PAGE
014990	       GO TO EA999.
	     GO TO EA15.

       EA20.
014970	     MOVE "**** END REPORT ****"
                                 TO REP-DETAIL2.
014980       WRITE REP-LINE1 BEFORE PAGE.
       EA999.
	     EXIT.
      /
025020 ZA00-INIT	      SECTION 90.
025030 ZA00-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
042000	     DISPLAY CLR-SCREEN.
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



025140	     MOVE WS-PTH15	 TO W02-PTH30.

016200 ZA02.
	   IF LS-USER NUMERIC
	       IF LS-USE > ZERO AND < 110
		   MOVE LS-USE	 TO WS-USUB
		   GO TO ZA04.
             MOVE 110            TO WS-USUB.
       ZA04.
014240       ADD 2549 WS-USUB    GIVING W25-RESULT.
             MOVE W25-KEY        TO WS-PARKEY.
             ADD 1 W25-SUB       GIVING WS-S6.
014250       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF TRM-STDP (WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
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
	     DISPLAY "Waiting for printer response" AT 1412
		      WITH FOREGROUND-COLOR 14 BLINK.
016250	     OPEN OUTPUT PRNREP.
	     CALL X"91" USING X91-RES X91-FUN PRNREP.
	     MOVE WS-10CPI	 TO REP-DETAIL1.
	     WRITE REP-LINE1 BEFORE 0 LINES.
	     MOVE SPACES	 TO REP-DETAIL1.
053830	     OPEN I-O ROUTE.
	   IF WS-STATUS = "05"
	       CLOSE ROUTE
	       OPEN OUTPUT ROUTE
	       CLOSE ROUTE
	       OPEN I-O ROUTE.
053840	   IF RUNTIME-ERROR
053850         IF FLE-LOCKED
053860             GO TO ZA200
053870         ELSE
053880         IF FLE-LIMIT
053890		   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT OR
		 (WS-STATUS = "39")
		   MOVE 30	 TO WS-F-ERROR
		   GO TO ZA50.
053900     IF WS-STATUS NOT = "00"
053910	       MOVE 30		 TO WS-F-ERROR
053920         PERFORM OPEN-ERROR.
025620 ZA25.
025630	     MOVE 1		 TO WS-PARKEY.
025640       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
025660	     MOVE PAR-DMY	 TO W12-TODAY.
025670	     MOVE PAR-YMD	 TO W12-T-YMD.
025830       GO TO ZA999-EXIT.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.

039850 ZA50.
041850	   IF WS-F-ERROR = 30
041860	       MOVE "ROUTE file" TO WS-FILE.
039980       DISPLAY "Rebuild " AT 0812
039990                WS-FILE WITH FOREGROUND-COLOR 14.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040020 ZA51.
040030       EXIT PROGRAM.
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
041240 ZA60.
041250       MOVE WS-G1          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8          TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1             TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2          TO WS-TCHR (8)  WS-TCHR (49)
021940                              WS-TCHR (61) WS-TCHR (72).
021950       MOVE WS-G3          TO WS-MCHR (8)  WS-MCHR (49)
021960                              WS-MCHR (61) WS-MCHR (72)
021960				    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4          TO WS-BCHR (8)  WS-BCHR (49)
021980                              WS-BCHR (61) WS-BCHR (72).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
025840 ZA200.
025850       DISPLAY "Files locked - Try later" AT 2312
025860                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
025870       DISPLAY "Press any key" AT 2512
025880		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
025890	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
025920 ZA999-EXIT.
025930       EXIT.
      /
025940 ZB000		     SECTION 9.

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
041770	   IF WS-F-ERROR = 30
041780	       MOVE W02-ROUTE	 TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
	       MOVE RTE-ROUTE	 TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY.
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
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170       STOP RUN.
