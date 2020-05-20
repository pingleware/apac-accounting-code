      $set LINKCOUNT"256"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******   ********  **    **  *******    *
      *   **    **     **     **    **  **         **  **   **    **   *
      *   **           **     **    **  **          ****    **    **   *
      *    ******      **     *******   *****        **     *******    *
      *         **     **     **        **          ****    **         *
      *   **    **     **     **        **         **  **   **         *
      *    ******      **     **        ********  **    **  **         *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S T O C K	E X C E P T I O N   P R O G R A M	       *
      *                                                                *
      *     VERSION 8.06 - August 2002				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STPEXCEP.
000030 AUTHOR.         J W LEMMON (CSS).
000040 DATE-WRITTEN.   April 1995.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1995 - 2002
				     by James William Lemmon.

		   All rights reserved.

000050 SECURITY.   PROGRAM MATERIAL IS THE SOLE PROPERTY OF
000060		   James William Lemmon.(Id No. 4412165050082).

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

000200	   SELECT STKDET1 ASSIGN W02-STKDET1
000040                    STATUS WS-STATUS
                          ORGANIZATION LINE SEQUENTIAL.

000200	   SELECT STKDET2 ASSIGN W02-STKDET2
000040                    STATUS WS-STATUS
                          ORGANIZATION LINE SEQUENTIAL.

000200	   SELECT PRNREP  ASSIGN PRINTER
                          ORGANIZATION LINE SEQUENTIAL.

000220 DATA DIVISION.
000230 FILE SECTION.

       COPY APACFIDS.FDE.

       FD  STKDET1   LABEL RECORD OMITTED.
       01  EXP-REC1.
	   03  EXP-DATA     PIC  X(100).

       FD  STKDET2   LABEL RECORD OMITTED.
       01  EXP-REC2.
	   03  EXP-DATA2    PIC  X(100).

      /
000370 FD  PRNREP    LABEL RECORD OMITTED
000380               LINAGE 66.
000390 01  REP-LINE1.
000400     03  REP-DETAIL1     PIC X(132).
000410 01  REP-LINE2.
000420     03  REP-DETAIL2     PIC X(80).
000430 01  REP-LINE3.
000490     03  REP-DATE        PIC X(06).
000500	   03  REP-DTE	       PIC Z9/99/9999.
000460	   03  FILLER	       PIC X(04).
           03  REP-PRC-HD.
               05  FILLER      PIC  X(01).
000470         05  REP-COMPANY PIC  X(40).
               05  FILLER      PIC  X(02).
000480	   03  FILLER	       PIC X(07).
000440     03  REP-PAGE        PIC X(06).
000450     03  REP-P-NO        PIC Z(03)9.
000510 01  REP-LINE4.
	   03  FILLER	       PIC  X(11).
001090	   03  EXC-H1	       PIC  X(18).
001100	   03  EXC-H2	       PIC  X(20).
	   03  EXC-H3	       PIC  X(20).
	   03  FILLER	       PIC  X(11).
       01  REP-LINE5.
	   03  FILLER	       PIC  X(11).
	   03  EXC-ITEM        PIC  X(14).
	   03  FILLER	       PIC  X(06).
	   03  EXC-QNTY1       PIC  Z(06)9-.
	   03  FILLER	       PIC  X(11).
	   03  EXC-QNTY2       PIC  Z(06)9-.
	   03  FILLER	       PIC  X(22).
      /
000350 WORKING-STORAGE SECTION.
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
002010 77  WS-PAGE	    PIC 9(04)	  COMP-5 VALUE ZERO.
002020 77  WS-OPTION        PIC X(01).
002070 77  WS-ERROR         PIC 9(01).
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
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
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
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

002330 COPY WS.WS.

       01  W02-FILE-IDS.

       COPY APACFIDS.ID.

	  03  W02-STKDET1.
	       05  W02-EXPORT PIC X(64) VALUE SPACES.

	  03  W02-STKDET2.
	       05  W02-EXPORT2 PIC X(64) VALUE SPACES.

002550 01  W10-STOCK.
002570     03  W10-CODE1      PIC X(14).
002580     03  W10-COD1 REDEFINES W10-CODE1.
002590         05  W10-CD1    PIC X(02).
002600         05  W10-CDE1   PIC X(12).
002610     03  W10-CODE2      PIC X(14).
002620     03  W10-COD2 REDEFINES W10-CODE2.
002630         05  W10-CD2    PIC X(02).
002640         05  W10-CDE2   PIC X(12).
	   03  W10-ITM1       PIC  X(14).
	   03  W10-ITM2       PIC  X(14).
	   03  W10-QNT1       PIC  X(08) JUSTIFIED RIGHT.
	   03  W10-QNT2       PIC  X(08) JUSTIFIED RIGHT.
002820	   03  W10-QNTY1      PIC S9(07).
002820	   03  W10-QNTY2      PIC S9(07).
003120 01  W11-PARTS.
003130     03  W11-PART-DETAILS OCCURS 30.
003140         05  W11-CODE   PIC X(14).
003150         05  W11-DESC   PIC X(30).
003160         05  W11-QUANT  PIC S9(05)V9999 COMP-3.

       COPY W12.WS.

       COPY W40.WS.

       COPY FUNCTION.WS.

003800 COPY W50.WS.

004120 01  W95-COMP            PIC X(40).

      /
004130 SCREEN SECTION.

       COPY BLANK.CRT.

000850 01  S-PROG.
000860	   03  LINE  2 COLUMN 28 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "STOCK EXCEPTIONS PROGRAM".

       01  S07.
	   03  LINE 10 COLUMN  2 VALUE "Checking:".

       01  S08.
	   03  LINE 10 COLUMN 12 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(14) FROM W10-CODE1.

004870 01  S99.
004880	   03  LINE 25 COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
000830				 VALUE "Correct ".
	   03	       COLUMN 37 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6 HIGHLIGHT
004890				 VALUE "ENTER".
	   03	       COLUMN 42 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " - ".
	   03	       COLUMN 45 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "N".
	   03	       COLUMN 46 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " if incorrect".

       01  ERROR-LINE.
	   03  LINE 25 COLUMN  2 BACKGROUND-COLOR WS-TEMPBG
				 FOREGROUND-COLOR WS-FGRND-5
				 HIGHLIGHT
				 PIC  X(48) FROM WS-ERR-MES.
      /
001230 PROCEDURE DIVISION.
007740 AA000-MAIN	       SECTION.
007750 AA000-INIT.
007760	     PERFORM ZA000-INIT.
	     PERFORM BA000.

       AA49.
	     STOP RUN.

      *
      *    ****    G E T   T H E   E X P O R T   F I L E   I D 
      *
       AA100           SECTION.
       AA101.
	     DISPLAY "Keyin the <pathname>/<filename> " AT 0402.
	     DISPLAY "First file  :" AT 0502.
             MOVE ZERO           TO WS-ERROR.
       AA105.
	     ACCEPT W02-EXPORT AT 0516
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE 9	 TO WS-ERROR
		     GO TO AA199
		 WHEN OTHER
		     CALL X"E5"
		     GO TO AA105
	       END-EVALUATE.
	     MOVE 64		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W02-EXPORT
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     DISPLAY W02-EXPORT AT 0516
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5 UPDATE AUTO.
	     DISPLAY CLEAR-L25.
012180	     DISPLAY S99.
012190	     ACCEPT WS-OPTION AT 2560
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     DISPLAY CLEAR-L25.
015980	   IF WS-OPTION = "N"
               GO TO AA105.
	     OPEN INPUT STKDET1.
           IF WS-STATUS = "00"
	       GO TO AA108.
             DISPLAY "Invalid file name" AT 2512 
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		      GO TO AA105.
       AA108.
	     DISPLAY "Second file :" AT 0702.
             MOVE ZERO           TO WS-ERROR.
       AA110.
	     ACCEPT W02-EXPORT2 AT 0716
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE 9	 TO WS-ERROR
		     GO TO AA199
		 WHEN OTHER
		     CALL X"E5"
		     GO TO AA110
	       END-EVALUATE.
	     MOVE 64		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W02-EXPORT
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     DISPLAY W02-EXPORT2 AT 0716
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5 UPDATE AUTO.
	     DISPLAY CLEAR-L25.
012180	     DISPLAY S99.
012190	     ACCEPT WS-OPTION AT 2560
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     DISPLAY CLEAR-L25.
015980	   IF WS-OPTION = "N"
	       GO TO AA110.
	     OPEN INPUT STKDET2.
           IF WS-STATUS = "00"
	       OPEN OUTPUT PRNREP
	       DISPLAY S07
               GO TO AA199.
             DISPLAY "Invalid file name" AT 2512 
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		      GO TO AA110.
       AA199.
             EXIT.

       AA200	       SECTION.
       AA201.
	     MOVE SPACES	 TO REP-DETAIL1.
	     ADD 1		 TO WS-PAGE.
	     MOVE "Date:"	 TO REP-DATE.
	     MOVE TODAY-DDMMYY	 TO REP-DTE.
	     MOVE "M A R K S   M O T O R   S P A R E S"
				 TO REP-COMPANY.
	     MOVE "Page:"	 TO REP-PAGE.
	     MOVE WS-PAGE	 TO REP-P-NO.
	     WRITE REP-LINE1 BEFORE 1 LINES.
	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "STOCK 'QUANTITY' EXCEPTIONS REPORT"
				 TO REP-COMPANY.
	     WRITE REP-LINE1 BEFORE 2 LINES.
	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "ITEM CODE"	 TO EXC-H1.
	     MOVE "File 1 (Quantity)"
				 TO EXC-H2.
	     MOVE "File 2 (Quantity)"
				 TO EXC-H3.
	     WRITE REP-LINE1 BEFORE 1 LINES.
	     MOVE SPACES	 TO REP-DETAIL1.
       AA299.
	     EXIT.

       BA000           SECTION.
       BA00.
	     DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	     PERFORM AA100.
	   IF WS-ERROR = 9
	       STOP RUN.
	     PERFORM AA200.

       BA05.
	     READ STKDET1
		  AT END GO TO BA10.
	     READ STKDET2
		  AT END GO TO BA10.
	     UNSTRING EXP-DATA DELIMITED BY ","
			       INTO W10-CODE1 W10-QNTY1.
	     UNSTRING EXP-DATA2 DELIMITED BY ","
				INTO W10-CODE2 W10-QNTY2.
	     UNSTRING EXP-DATA DELIMITED BY ","
			       INTO W10-ITM1 W10-QNT1.
	     UNSTRING EXP-DATA2 DELIMITED BY ","
				INTO W10-ITM2 W10-QNT2.
	   IF NOT (W10-CODE1 = W10-CODE2)
	       DISPLAY "Files out of SYNC " AT 2502
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			     BACKGROUND-COLOR 3
			W10-CODE1 WITH FOREGROUND-COLOR 1
				       BACKGROUND-COLOR 3
		       " " WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
			W10-CODE2 WITH FOREGROUND-COLOR 5
				       BACKGROUND-COLOR 3
	       STOP RUN.
	     DISPLAY S08.
	     MOVE ZERO		 TO WS-SUB.
	     INSPECT W10-QNT1 TALLYING WS-SUB FOR ALL "-".
	   IF WS-SUB > ZERO
	       INSPECT W10-QNT1 REPLACING ALL " " BY "0"
					  ALL "-" BY "0"
	       MOVE W10-QNT1	 TO W10-QNTY1
	       MULTIPLY -1	 BY W10-QNTY1.
	     MOVE ZERO		 TO WS-SUB.
	     INSPECT W10-QNT2 TALLYING WS-SUB FOR ALL "-".
	   IF WS-SUB > ZERO
	       INSPECT W10-QNT2 REPLACING ALL " " BY "0"
					  ALL "-" BY "0"
	       MOVE W10-QNT2	 TO W10-QNTY2
	       MULTIPLY -1	 BY W10-QNTY2.
	   IF (W10-QNTY1 = W10-QNTY2) AND
	      (W10-QNTY1 NOT < ZERO)
	       GO TO BA05.
	     MOVE W10-CODE1	 TO EXC-ITEM.
	     MOVE W10-QNTY1	 TO EXC-QNTY1.
	     MOVE W10-QNTY2	 TO EXC-QNTY2.
	   IF LINAGE-COUNTER < 62
	       WRITE REP-LINE1 BEFORE 1 LINES
	   ELSE
	       WRITE REP-LINE1 BEFORE 2 LINES
	       PERFORM AA200.
	     MOVE SPACES	 TO REP-DETAIL1.
	     GO TO BA05.
       BA10.
	     MOVE "******  E N D   R E P O R T  ******"
				 TO REP-DETAIL1.
	     WRITE REP-LINE1 BEFORE PAGE.
	     DISPLAY "End of EXCEPTION report" AT 1202
		      WITH BACKGROUND-COLOR 4
			   FOREGROUND-COLOR 7 HIGHLIGHT.

       BA999.
	     EXIT.

      /
019370 ZA000-INIT	       SECTION 8.
019380 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     ACCEPT W12-T-YMD FROM DATE.
	     MOVE W12-YY	 TO W12-YEAR.
	     MOVE W12-MM	 TO W12-MONTH.
	     MOVE W12-DD	 TO W12-DAY.
	     MOVE W12-TODAY	 TO TODAY-DDMMYY.
019390	     DISPLAY CLR-SCREEN.
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




	     MOVE WS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.

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




019900	     GO TO ZA999-EXIT.
055270 ZA49.
055280       DISPLAY "Too many files OPEN" AT 0812
055290                WITH FOREGROUND-COLOR 14.
055300       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
055310       DISPLAY "Press any key to continue" AT 1212.
055320       ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
055330       GO TO ZA205.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
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
021950	     MOVE WS-G3 	 TO WS-MCHR (19) WS-MCHR (50)
021960                              WS-MCHR (60) WS-MCHR (70)
				    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4          TO WS-BCHR (18) WS-BCHR (49)
021980                              WS-BCHR (60) WS-BCHR (70).
019910 ZA200.
019920       DISPLAY "Files locked - Try later" AT 2312
019930                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
019940       DISPLAY "Press any key" AT 2512 
019950		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
019960	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
019990 ZA999-EXIT.
020000       EXIT.
      /
020010 ZB000-ERROR	       SECTION 8.
      
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
041510       GO TO DISPLAY-FILE-NAME.
      
041560 DISPLAY-FILE-NAME.
041850	   IF WS-F-ERROR = 99
041860	       MOVE W02-STKDET1   TO WS-FILE
041870	       MOVE ZERO	  TO WS-KEY.
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
