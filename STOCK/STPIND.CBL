      $set LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	******	*     **   *******     *
      *   **	**     **     **    **	  **	**    **   **	 **    *
      *   **	       **     **    **	  **	***   **   **	 **    *
      *    ******      **     *******	  **	** *  **   **	 **    *
      * 	**     **     **	  **	**  * **   **	 **    *
      *   **	**     **     **	  **	**   ***   **	 **    *
      *    ******      **     **	******	**    **   *******     *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     F I X   T H E   P A C K E D   F I E L D   F O R	       *
      *     S I N G L E   I T E M S				       *
      *                                                                *
      *     VERSION 8.07 - January 2003				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STPIND.
000030 AUTHOR.         J.W.LEMMON (CSS).
000040 DATE-WRITTEN.   AUGUST 2001.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2001 - 2003
				     by James William Lemmon.

		   All rights reserved.

000050 SECURITY.   PROGRAM MATERIAL IS THE SOLE PROPERTY OF
000060		   James William Lemmon.

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090                 CONSOLE IS CRT
                       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

000140 COPY STOCK.SL.

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY APACFIDS.FDE.

000300 COPY STOCK.FDE.

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001710 77  WS-SUB	    PIC 9(04)	  COMP-5.
001710 77  WS-S1            PIC 9(04)     COMP-5.
001720 77  WS-S2            PIC 9(04)     COMP-5.
001730 77  WS-S3            PIC 9(04)     COMP-5.
001740 77  WS-S4            PIC 9(04)     COMP-5.
001750 77  WS-S5            PIC 9(04)     COMP-5.
001760 77  WS-S6            PIC 9(04)     COMP-5.
001770 77  WS-S7            PIC 9(04)     COMP-5.
001780 77  WS-S8            PIC 9(04)     COMP-5.
001790 77  WS-IXS           PIC 9(04)     COMP-5.
001800 77  WS-IXS1          PIC 9(04)     COMP-5.
001810 77  WS-IXS2          PIC 9(04)     COMP-5.
001820 77  WS-IXS3          PIC 9(04)     COMP-5.
001830 77  WS-IXS4          PIC 9(04)     COMP-5.
001920 77  WS-NETKEY	    PIC 9(04)	  COMP-5.
001920 77  WS-KEYSTR        PIC 9(04)     COMP-5.
001940 77  WS-MONTHS	    PIC S9(01)V99 COMP-3.
001960 77  WS-KEY1          PIC 9(04)     COMP-5.
001980 77  WS-ITM           PIC X(14).
       77  WS-ETYPE         PIC X(01).
002020 77  WS-OPTION	    PIC X(01).
002020 77  WS-SKIP          PIC X(01).
002040 77  WS-ADJ           PIC X(01).
002070 77  WS-ERROR         PIC 9(01).
002120 77  WS-AMEND	    PIC 9(01)	  VALUE 0.
       77  APAC-VERSION     PIC  X(04) VALUE "8.03".
       77  APAC-SYSTEM	    PIC  X(12) VALUE "-STK Utility".

      /
002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440         05  WS-TCHR PIC X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC  X(78).
               05  FILLER  PIC X(01).
002430     03  WS-TOP-LNE2.
002440         05  WS-TCH  PIC X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
               05  FILLER      PIC  X(39).
002450     03  WS-MID-LNE.
002460         05  WS-MCHR PIC X(01) OCCURS 80.
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
002450	   03  WS-MID-LNE2.
002460         05  WS-MCH  PIC X(01) OCCURS 80.
	   03  WS-MID78 REDEFINES WS-MID-LNE2.
	       05  FILLER      PIC  X(01).
	       05  WS-BLNK78   PIC  X(78).
	       05  FILLER      PIC  X(01).
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

002330 COPY WS.WS.

002330 COPY CHAIN.WS.

002350 01  W02-FID.

       COPY APACFIDS.ID.

000300 COPY STOCK.ID.

       COPY W10.STK.

       COPY W11.STK.

       COPY W12.WS.

       01  W15-INPUT.
	   03  W15-LTIME      PIC  9(01)V999 COMP.

       COPY W20.WS.

003700 01  W25-CALCS.
005040     03  W25-RESULT      PIC 9(05)V99.
004330     03  W25-RESULT1 REDEFINES W25-RESULT.
004340         05  W25-WHOLE   PIC 9(05).
004350         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
003750     03  W25-SCOST      PIC S9(09)V99 COMP-3.
003760     03  W25-PCOST      PIC S9(09)V99 COMP-3.
003760     03  W25-PCST       PIC S9(09)V99 COMP-3.
003760     03  W25-PTOT       PIC S9(09)V99 COMP-3.
003770     03  W25-TSELL      PIC S9(09)V99 COMP-3.
003770     03  W25-TOTAL      PIC S9(09)V99 COMP-3.
003780     03  W25-VALUE      PIC S9(09)V99 COMP-3.
003780     03  W25-ADJUST     PIC S9(09)V99 COMP-3.
001660	   03  W25-TIME        PIC  9(08).
001670     03  W25-TRED REDEFINES  W25-TIME.
001680	       05  FILLER      PIC  9(02).
001690	       05  W25-FACT    PIC  9(06).
	       05  W25-DATE REDEFINES W25-FACT.
		   07  W25-YY  PIC  9(02).
		   07  W25-MM  PIC  9(02).
		   07  W25-DD  PIC  9(02).
	   03  W25-CUR-CC      PIC  9(02) COMP-5.
	   03  W25-PRV-CC      PIC  9(02) COMP-5.
	   03  W25-NXT-CC      PIC  9(02) COMP-5.

       COPY W40.WS.

       COPY FUNCTION.WS.

003800 COPY W50.WS.

003930 01  W80-EDIT.
003940     03  W80-CNO        PIC 9(04).
003950     03  W80-V1.
003960         05  W80-S7V2   PIC Z(06)9.99-.
003970     03  W80-V2.
003980         05  W80-S5V2   PIC Z(04)9.99-.
003990     03  W80-DTE.
004000	       05  W80-DATE   PIC Z9/99/9999.
004010     03  W80-QNT.
004020         05  W80-5      PIC Z(04)9.99.
004030     03  W80-QNT1 REDEFINES W80-QNT.
004040         05  W80-3V2    PIC Z(02)9.9999.
004050     03  W80-NO.
004060         05  W80-5N     PIC 9(05).
004070         05  W80-REC REDEFINES W80-5N
004080                        PIC ZZZZ9.
004090     03  W80-DEL.
004100         05  W80-2N      PIC 9(02).

006270 01  W85-PASS.
006280     03  W85-SUPER       PIC  X(06)    OCCURS 9.
006290     03  W85-ENTRY       PIC  9(02)    COMP-3.
006300     03  W85-MARG        PIC S9(02)V99.

004110 01  W90-BAL             PIC S9(07)V99 COMP-3.

004120 01  W95-COMP            PIC X(40).

005120 01  W100-EDIT.
003970     03  W100-V2.
003980         05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.
      /
004130 SCREEN SECTION.

       COPY BLANK.CRT.

       COPY S99.CRT.

       COPY ERROR.CRT.

      /
007730 PROCEDURE DIVISION.
007740 AA000-MAIN              SECTION.
007750 AA000-INIT.
007760       PERFORM ZA000-INIT.
	   IF WS-ERROR = 9
	       GO TO AA99.
007780	     PERFORM BX000.

       AA49.
	     CLOSE STOCK.

       AA99.
	     STOP RUN.

       COPY FUNCTION.CRT.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

       COPY DATE.CHK.

      /
      *    ****    READ FILES ROUTINES
      *
008180 AC000-READ              SECTION.
      *
      *     ***  *****  ***   ***  *   *     ***** ***** *     *****
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *    *       *   *   * *     * *       *       *   *     *
      *     ***    *   *   * *     **        ***     *   *     ***
      *        *   *   *   * *     * *       *       *   *     *
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *     ***    *    ***   ***  *   *     *     ***** ***** *****
      *
000180 READ-STOCK-LOCK.
000190	     READ STOCK 	 WITH KEPT LOCK
000200         KEY IS STK-CODE.
000210     IF WS-STATUS = "00"
	       IF STK-DISC NOT = 9
000050		   MOVE ZERO	 TO WS-F-ERROR
000060		   GO TO READ-STOCK-EXIT
	       ELSE
		   MOVE 22	 TO WS-F-ERROR
000060		   GO TO READ-STOCK-EXIT.
000240	   IF WS-STATUS = "23"
000250         MOVE 22           TO WS-F-ERROR
000260         GO TO READ-STOCK-EXIT.
000270     IF WS-STAT1 = "2" OR "3" OR "4"
000280         MOVE 22           TO WS-F-ERROR
000290         PERFORM READ-ERROR.
000300     IF RECORD-LOCKED
	       MOVE W02-STOCKF	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000310         PERFORM LOCKED-RECORD
000320         GO TO READ-STOCK-LOCK.
000330     IF TOO-MANY-LOCKS
000340         UNLOCK STOCK
000350         GO TO READ-STOCK-LOCK.
000360       GO TO READ-STOCK-EXIT.

000550 READ-STOCK-NEXT-LOCK.
000560       READ STOCK NEXT WITH KEPT LOCK.
000570     IF WS-STATUS = "00"
	       IF STK-DISC NOT = 9
000050		   MOVE ZERO	 TO WS-F-ERROR
000060		   GO TO READ-STOCK-EXIT
	       ELSE
000060		   GO TO READ-STOCK-NEXT-LOCK.
000600	   IF (WS-STATUS = "23") OR
000610        (WS-STAT1 = "1")
000620         MOVE 22           TO WS-F-ERROR
000630         GO TO READ-STOCK-EXIT.
000640     IF WS-STAT1 = "2" OR "3" OR "4"
000650         MOVE 22           TO WS-F-ERROR
000660         PERFORM READ-ERROR.
000670     IF RECORD-LOCKED
	       MOVE W02-STOCKF	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000680         PERFORM LOCKED-RECORD
000690         GO TO READ-STOCK-NEXT-LOCK.
000700     IF TOO-MANY-LOCKS
000710         UNLOCK STOCK
000720         GO TO READ-STOCK-NEXT-LOCK.
000730       GO TO READ-STOCK-EXIT.

000750 START-AT-STOCK-CODE.
000760       START STOCK 
000770	       KEY >= STK-CODE.
	     GO TO STOCK-CHECK-STATUS.

       STOCK-CHECK-STATUS.
001480	   IF WS-STATUS = "00"
001490         MOVE ZERO         TO WS-F-ERROR
001500         GO TO READ-STOCK-EXIT.
001510     IF WS-STATUS = "23"
001520         MOVE 22           TO WS-F-ERROR
001530         GO TO READ-STOCK-EXIT.
001540     IF WS-STAT1 = "2" OR "3" OR "4"
001550         MOVE 22           TO WS-F-ERROR
001560         PERFORM READ-ERROR.

001580 READ-STOCK-EXIT.
001590       EXIT.
      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
009850 AD000-WRITE             SECTION.
      *
      *     ***  *****  ***   ***  *   *     ***** ***** *     *****
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *    *       *   *   * *     * *       *       *   *     *
      *     ***    *   *   * *     **        ***     *   *     ***
      *        *   *   *   * *     * *       *       *   *     *
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *     ***    *    ***   ***  *   *     *     ***** ***** *****
      *
000080 REWRITE-STOCK-UNLOCK.
000090       REWRITE STK-RECORD1.
	   IF WS-STAT1 = "0"
	       UNLOCK STOCK
	       GO TO WRITE-STOCK-EXIT.
	   IF WS-STAT1 = "9"
	       IF NET-ERROR OR COM-ERROR
000030		   DISPLAY SPACE AT 2550
000040		   DISPLAY "Network error " AT 2550
000050			    WITH FOREGROUND-COLOR 6 HIGHLIGHT
			    "STOCK" WITH FOREGROUND-COLOR 7 HIGHLIGHT
		   GO TO STOCK-ERROR-DELAY
	       ELSE
		   DISPLAY "I-O error " AT 2550
000050			    WITH FOREGROUND-COLOR 6 HIGHLIGHT
			    WS-STATUS  WITH FOREGROUND-COLOR 7 HIGHLIGHT
		   GO TO STOCK-ERROR-DELAY.
000130	     MOVE 22		 TO WS-F-ERROR.
000140       GO TO WRITE-STOCK-EXIT.
000150
       STOCK-ERROR-DELAY.
020280	     ACCEPT WS-STIME FROM TIME.

000160 STOCK-ERROR-LOOP.
020280	     ACCEPT WS-TIME FROM TIME.
020880	     SUBTRACT WS-STIME	 FROM WS-TIME.
	   IF WS-TIME < 100
	       GO TO STOCK-ERROR-LOOP.
000200       DISPLAY SPACE AT 2550.
000210	     GO TO REWRITE-STOCK-UNLOCK.

000320 WRITE-STOCK-EXIT.
000330       EXIT.

       COPY STOCK.1ST.

      /
      *    ****    C H A N G E	 L E A D   T I M E   A N D
      * 	   R E - O R D E R   L E V E L S
      *
035140 BX000	       SECTION.
035150 BX00.
035160       DISPLAY CLR-SCREEN.
	     MOVE 10		 TO SHADE-ROW.
	     MOVE 26		 TO SHADE-COL.
	     MOVE 32		 TO SHADE-WIDTH.
	     MOVE 4		 TO SHADE-LINES.
	     DISPLAY "旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�" AT 0924
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		     "�" WITH FOREGROUND-COLOR 5 HIGHLIGHT
			      BACKGROUND-COLOR 5.
	     DISPLAY "�" AT 1024
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		     " CHECK & CORRECT PACKED ITEMS.  "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "�" WITH FOREGROUND-COLOR 5 HIGHLIGHT
			      BACKGROUND-COLOR 5.
	     DISPLAY "�" AT 1124
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		     "컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴"
		      WITH FOREGROUND-COLOR 7 BACKGROUND-COLOR 5
		     "�" WITH FOREGROUND-COLOR 5 HIGHLIGHT
			      BACKGROUND-COLOR 5.
035170	     DISPLAY "�" AT 1224
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		     " Run amendment routine  (Y/N)   "
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "�" WITH FOREGROUND-COLOR 5 HIGHLIGHT
			      BACKGROUND-COLOR 5.
	     DISPLAY "�" AT 1324
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		     "컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�"
		      WITH FOREGROUND-COLOR 5 HIGHLIGHT
			   BACKGROUND-COLOR 5.
	     PERFORM SCREEN-SHADOW.

035180 BX02.
035190	     ACCEPT WS-OPTION AT 1255
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
035250	   IF NOT (WS-OPTION = "N" OR "Y")
035260         GO TO BX02.
035160       DISPLAY CLR-SCREEN.
035270     IF WS-OPTION = "N"
035280	       GO TO BX999.

035300 BX05.
035360       MOVE "    !"        TO STK-CODE.
035370       PERFORM START-AT-STOCK-CODE THRU READ-STOCK-EXIT.
035460     IF WS-F-ERROR = 22
035470	       GO TO BX999.
	     MOVE 10		 TO SHADE-ROW.
	     MOVE 25		 TO SHADE-COL.
	     MOVE 33		 TO SHADE-WIDTH.
	     MOVE 4		 TO SHADE-LINES.
	     DISPLAY "旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴" AT 0923
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "�" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "�" AT 1023
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "    BUSY UPDATING: STOCK FILE    "
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 7
		     "�" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "�" AT 1123
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�"
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 7
		     "�" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
035380	     DISPLAY "�" AT 1223
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     " Stock Record                    "
		      WITH FOREGROUND-COLOR 5 BACKGROUND-COLOR 7
		     "�" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "�" AT 1323
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     PERFORM SCREEN-SHADOW.

035390 BX10.
035450       PERFORM READ-STOCK-NEXT-LOCK THRU READ-STOCK-EXIT.
035460     IF WS-F-ERROR = 22
035470	       GO TO BX15.
035440	     DISPLAY STK-CODE AT 1238
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF STK-UNIT > 0
	       UNLOCK STOCK
	       GO TO BX10.
	     MOVE 1		 TO STK-UNIT.
035710	     PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
035720       GO TO BX10.

       BX15.
035160	     DISPLAY CLR-SCREEN.
	     DISPLAY "Amendments Complete" AT 1231
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4.

035730 BX999.
035740       EXIT.

      /
044130 ZA000-INIT	       SECTION.
044140 ZA000-OPEN.
	     ACCEPT W25-FACT FROM DATE.
	     MOVE W25-YY	 TO W12-YEAR.
	     MOVE W25-MM	 TO W12-MONTH.
	     MOVE W25-DD	 TO W12-DAY.
	   IF W25-YY < 50
	       MOVE 20		 TO W12-CENT
	   ELSE
	       MOVE 19		 TO W12-CENT.
	     MOVE W12-TODAY	 TO TODAY-DDMMYY.
003250	     MOVE W12-DAY	 TO W12-DD.
	     MOVE W12-MONTH	 TO W12-MM.
003260	     MOVE W12-CENT	 TO W12-CC.
003270	     MOVE W12-YEAR	 TO W12-YY.
	     PERFORM ZA55 THRU ZA60.
044150	     DISPLAY CLR-SCREEN.
      *
      *    ****    D O	 N O T	 D I S P L A Y	 E R R O R
      * 	   M E S S A G E S
      *
	     MOVE 44		 TO USER-NUMBER.
	     MOVE 1		 TO USER-SETTING.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
	     DISPLAY "ADJUST STOCK AVERAGE COST" AT 0228
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "旼컴컴컴컴컴컴컴컴컴컴컴컴컴�" AT 1126
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "� Company/Business Code     �" AT 1226
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "읕컴컴컴컴컴컴컴컴컴컴컴컴컴�" AT 1326
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     ACCEPT WS-SYS-ID AT 1250
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     MOVE WS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     OPEN I-O APACFIDS.
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
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.

       ZA05.
044300	     OPEN I-O STOCK.
044310     IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 22       TO WS-F-ERROR
                   GO TO ZA50.
044330     IF WS-STATUS NOT = "00"
044340         MOVE 22           TO WS-F-ERROR
050210	       PERFORM OPEN-ERROR.
	     GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
039850 ZA50.
	   IF WS-F-ERROR = 22
	       MOVE "STOCK file" TO WS-FILE.
039980	     DISPLAY "Rebuild " AT 0812
039990                WS-FILE WITH FOREGROUND-COLOR 14.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

040020 ZA51.
	     STOP RUN.

       ZA55.
041220       MOVE 1              TO WS-S1.
021870       MOVE SPACES         TO WS-MID-LNE.
041240 ZA60.
041250       MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1 TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2          TO WS-TCHR (18) WS-TCHR (49)
021940                              WS-TCHR (60) WS-TCHR (70).
021950       MOVE WS-G3          TO WS-MCHR (18) WS-MCHR (49)
021960                              WS-MCHR (60) WS-MCHR (70)
                                    WS-MCHR (1)  WS-MCHR (80) 
                                    WS-MCH  (1)  WS-MCH  (80).
021970       MOVE WS-G4          TO WS-BCHR (18) WS-BCHR (49)
021980                              WS-BCHR (60) WS-BCHR (70).

044790 ZA200.
044800       DISPLAY "Files locked - Try later" AT 2312
044810                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
044820       DISPLAY "Press any key" AT 2512 
044830		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
044840	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.

       ZA205.
	     STOP RUN.

044870 ZA999-EXIT.
044880       EXIT.
      /
044890 ZB000-ERROR	       SECTION.
      
041480 OPEN-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
      
041480 READ-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
      
041520 WRITE-ERROR.
041530       DISPLAY CLR-SCREEN.
041540       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041560 DISPLAY-FILE-NAME.
041850	   IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE STK-CODE	 TO WS-KEYX.
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
042170	     STOP RUN.
