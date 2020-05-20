      ******************************************************************
      *                                                                *
      *    ****    *******    ******   **     **  **	    ********   *
      *   **  **   **	 **  **    **  **     **  **	    **	   **  *
      *  **    **  **	 **  **        **     **  **	    **	   **  *
      *  ********  *******   **	       *********  **	    ********   *
      *  **    **  **	     **        **     **  **	    **	       *
      *  **    **  **	     **    **  **     **  **	    **	       *
      *  **    **  **	      ******   **     **  *******   **	       *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     B U S I N E S S   S Y S T E M   H E L P  P R O G R A M     *
      *                                                                *
      *     VERSION 8.13.07 - January 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     APACHLP.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   AUGUST 1994.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1994 - 2002
				     by James William Lemmon.

		   All rights reserved.

000050 SECURITY.   PROGRAM MATERIAL IS THE SOLE PROPERTY OF
000060		   James William Lemmon.

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.  
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       SELECT APCHLP	 ASSIGN DISK
			 LOCK MANUAL
                         WITH LOCK ON MULTIPLE RECORDS
000240                   STATUS WS-STATUS
000250                   ACCESS DYNAMIC
000260                   ORGANIZATION INDEXED
000270			 RECORD HLP-KEY.

000330 DATA DIVISION.
000340 FILE SECTION.

       FD  APCHLP    IS EXTERNAL
		     LABEL RECORD STANDARD
000020		     VALUE OF FILE-ID "HLP\APACHELP.APC".
       01  HLP-REC.
	   03  HLP-KEY.
	       05  HLP-ID.
		   07  HLP-MODULE  PIC	X(03).
		   07  HLP-PROG	   PIC	X(03).
	       05  HLP-SEQ	   PIC	9(04).
	   03  HLP-DATA 	   PIC	X(36).

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   ** 
      *         **    *    **   **    **   **   **    **  **  
      *         **   ***   **   **    **   ******     *****   
      *          ** ** ** **    **    **   **   **    **  **  
      *           ***   ***     **    **   **    **   **   ** 
      *            *     *       ******    **    **   **    **
      *
000940 WORKING-STORAGE SECTION.
       77  WS-S1	       PIC  9(04)    COMP-5.
       77  WS-S2	       PIC  9(04)    COMP-5.
       77  WS-S3	       PIC  9(04)    COMP-5.
       77  WS-S4	       PIC  9(04)    COMP-5.
       77  WS-S5	       PIC  9(04)    COMP-5.
       77  WS-S6	       PIC  9(04)    COMP-5.
000950 77  WS-SEQ	       PIC  9(04)    COMP-5.
       77  WS-SPACES	       PIC  X(36) VALUE SPACES.

       COPY WS.WS.

       COPY FUNCTION.WS.

       01  W05-HELP.
	   03  W05-HLP	       PIC  X(2448).
	   03  W05-LINE	REDEFINES W05-HLP
			       PIC  X(36) OCCURS 68.

       LINKAGE SECTION.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE SPACES.
	   03  WS-PROG	       PIC  X(03) VALUE SPACES.

      /
      *   ******    ******   *******   ********  ********  **     ** 
      *  **    **  **    **  **    **  **        **        ***    ** 
      *  **        **        **    **  **        **        ** *   ** 
      *   ******   **        ******    *****     *****     **  *  ** 
      *        **  **        **   **   **        **        **   * ** 
      *  **    **  **    **  **    **  **        **        **    *** 
      *   ******    ******   **    **  ********  ********  **     ** 
      *                                                              
003150 SCREEN SECTION.

       01  S-HELP.
	   03  LINE  4 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE
	       "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  5 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE  6 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE  7 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE  8 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE  9 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 10 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 11 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 12 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 13 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 14 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 15 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 16 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 17 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 18 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 19 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 20 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 21 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 22 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE
	       "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  ERROR-LINE.
	   03  LINE 25 COLUMN  2 BACKGROUND-COLOR WS-TEMPBG
				 FOREGROUND-COLOR WS-FGRND-5
				 HIGHLIGHT
				 PIC  X(48) FROM WS-ERR-MES.
      /
      *        *******    *******     ******     ****** 
      *        **    **   **    **   **    **   **    **
      *        **    **   **   **    **    **   **
      *        *******    ******     **    **   **
      *        **         **   **    **    **   **
      *        **         **    **   **    **   **    **
      *        **         **    **    ******     ****** 
      *
001230 PROCEDURE DIVISION USING WS-HELP.
006220 AA000	       SECTION.
006230 AA00.
      *
      *    ****    D O	 N O T	 D I S P L A Y	 E R R O R
      * 	   M E S S A G E S
      *
	     MOVE 2		 TO USER-ACTION.
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
	     OPEN I-O APCHLP.
	   IF WS-STATUS NOT = "00"
	       DISPLAY "Help file corrupted or not on disk" AT 2502
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			     BACKGROUND-COLOR 3
		       " - Press ANY key to exit"
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 3
	       CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS
	       GO TO AA999.
	     PERFORM BA000.
	     CLOSE APCHLP.
       AA999.
	     EXIT PROGRAM.

       BA000	       SECTION.
       BA00.
	     MOVE WS-HELP      TO HLP-ID.
	     MOVE ZERO	       TO HLP-SEQ.
	     DISPLAY S-HELP.
006090	     START APCHLP
	       KEY >= HLP-KEY.
006100	   IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO BA05.
	     MOVE 99		 TO WS-F-ERROR.

       BA05.
	   IF WS-F-ERROR = 99
	       GO TO BA900.
	     MOVE 0442	       TO CPOS.
	     MOVE ZERO	       TO WS-S3.
	     INITIALIZE W05-HELP.

       BA10.
	     READ APCHLP NEXT.
	   IF WS-STAT1 NOT = "0"
	       GO TO BA15.
	   IF WS-HELP NOT = HLP-ID
	       GO TO BA15.
	     ADD 1	       TO WS-S3.
	     MOVE HLP-DATA     TO W05-LINE (WS-S3).
	     GO TO BA10.

       BA15.
	   IF WS-S3 = ZERO
	       GO TO BA900.
	   IF WS-S3 > 17
	       SUBTRACT 16 FROM WS-S3
			       GIVING WS-S5
	   ELSE
	       MOVE 1	       TO WS-S5.
	     MOVE 1	       TO WS-S1 WS-S4.

       BA20.
	     MOVE WS-S4        TO WS-S1.

       BA25.
	     ADD 1	       TO CLIN.
	     DISPLAY W05-LINE (WS-S1) AT CPOS
		     WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 7.
	   IF WS-S1 < WS-S3
	       IF CLIN < 21
		   ADD 1       TO WS-S1
		   GO TO BA25.
	     DISPLAY SPACES AT 2501
		     WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
	     DISPLAY "Use " AT 2502 WITH FOREGROUND-COLOR 1
					 BACKGROUND-COLOR 3
		     "" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			      BACKGROUND-COLOR 3
		     "/" WITH FOREGROUND-COLOR 1
			      BACKGROUND-COLOR 3
		     "" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			      BACKGROUND-COLOR 3
		     "/" WITH FOREGROUND-COLOR 1
			      BACKGROUND-COLOR 3
		     "PgDn" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 3
		     "/" WITH FOREGROUND-COLOR 1
			      BACKGROUND-COLOR 3
		     "PgUp" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 3
		     " or " WITH FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 3
		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "ape" WITH FOREGROUND-COLOR 1
				BACKGROUND-COLOR 3.
       BA30.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		   IF WS-S4 > 1
		       SUBTRACT 1  FROM WS-S4
		 WHEN DOWN-KEY
		   IF WS-S4 < WS-S5
		       ADD 1   TO WS-S4
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY GO TO BA999
		 WHEN PAGE-UP
		   IF WS-S4 > 17
		       SUBTRACT 17 FROM WS-S4
		   ELSE
		       MOVE 1	   TO WS-S4
		   END-IF
		 WHEN PAGE-DOWN
		   ADD 17      TO WS-S4
		   IF WS-S4 > WS-S5
		       MOVE WS-S5  TO WS-S4
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE.
	     MOVE 0442	       TO CPOS.
	     DISPLAY S-HELP.
	     GO TO BA20.

       BA900.
	     DISPLAY "No help available" AT 0949
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "Press ANY key to exit" AT 1149
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BA999.
	     EXIT.
