      $set linkcount"384"
      ******************************************************************
      *                                                                *
      *  ******   *******  ********  **   **  ******	  ***    ******  *
      * **    **  **	**    **     **   **  **       ** **   **   ** *
      * **        **	**    **     **   **  **      **   **  **   ** *
      * **        ******      **     *******  ****    **   **  **   ** *
      * **        **   **     **     **   **  **      *******  **   ** *
      * **    **  **	**    **     **   **  **      **   **  **   ** *
      *  ******   **	**    **     **   **  ******  **   **  ******  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S E T U P	S C R E E N   H E A D I N G S		           *
      *                                                                *
      *     VERSION 9.00.00 - August 2012		                       *
      * 						                             *
      ******************************************************************
      * 						                             *
      *  This program has been introduced to ensure that all screens   *
      *  in the system have the same format and reduce the maintenance *
      *  in future. The system has had a major change to the screen    *
      *  size from 25 lines to 50 lines and a format change which this *
      *  program will handle.					                 *
      *  All programs will now call this program to setup and display  *
      *  the screen layouts.					                 *
      * 							                       *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CRTHEAD.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  WS-COPYRIGHT    PIC  X(18) VALUE "aeWlimemnomLalismJ".
000610 77  WS-S1	         PIC  9(04) COMP-5.
000620 77  WS-S2           PIC  9(04) COMP-5.

       COPY "WS.WS".

       01  W05-HEADINGS.
	     03  W05-TIME.
	         05  W05-ETIME PIC  99/99/99.
	     03  W05-PROGRAM   PIC  X(15).
	     03  W05-USER      PIC  X(20).
	     03  W05-HEADING   PIC  X(40).
	     03  W05-COMPANY   PIC  X(40).
           03  W05-WORK-STATION.
               05  W05-WRKHD PIC  X(11).
               05  W05-WRKST PIC  X(03).

       01  W50-JUSTIFY.
000210     03  W50-DATA.
000220         05  W50-CCHAR   PIC  X(01)  OCCURS 40.
000230     03  W50-DATA-LEFT REDEFINES W50-DATA.
000240         05  FILLER      PIC  X(01).
000250         05  W50-D39     PIC  X(39).
000310     03  W50-DATA-15 REDEFINES W50-DATA.
000320         05  W50-15.
000330	       07  FILLER  PIC  X(01).
000340	       07  W50-14  PIC  X(14).
000350         05  FILLER      PIC  X(25).
000310     03  W50-DATA-20 REDEFINES W50-DATA.
000320         05  W50-20.
000330	       07  FILLER  PIC  X(01).
000340	       07  W50-19  PIC  X(19).
000350         05  FILLER      PIC  X(20).
000360     03  W50-DATA-30 REDEFINES W50-DATA.
000370         05  W50-30.
000380             07  FILLER  PIC  X(01).
000390             07  W50-29  PIC  X(29).
000400         05  FILLER      PIC  X(10).

       LINKAGE SECTION.

       COPY "CHAIN.LS".

       COPY "HEADING.CRT".

       SCREEN SECTION.

       COPY "Blank.crt".

       PROCEDURE DIVISION USING CRT-HEADINGS LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       A00-MAIN SECTION.
       AA00.
             ACCEPT WS-TIME FROM TIME.
             MOVE WS-TIME                TO W05-ETIME.
             INSPECT W05-TIME REPLACING ALL "/" BY ":".
             MOVE CRT-WRKHD              TO W05-WRKHD.
             MOVE CRT-WRKST              TO W05-WRKST.

       AA05.
             EVALUATE CRT-TYPE
               WHEN "C"
		        DISPLAY CLEAR-L50
               WHEN "E"
                    MOVE CRT-START       TO CLIN
                    MOVE 01	           TO CCOL
		        PERFORM TEST AFTER UNTIL CLIN = CRT-END
                        DISPLAY CLR-LINE AT CPOS
                        ADD 1            TO CLIN
		        END-PERFORM
                    DISPLAY CLR-COMPANY-HEAD
               WHEN "H"
		        PERFORM BA00 THRU BA09
		        DISPLAY CLR-SCREEN-HEAD
               WHEN "S"
		        PERFORM BA10 THRU BA19
		        PERFORM BA20 THRU BA29
		        DISPLAY CLR-SCREEN
             END-EVALUATE.

       AA09.
             EXIT-PROGRAM.

       BA000-JUSTIFY SECTION.
       BA00.
             MOVE CRT-HEADING	 TO W50-DATA.
             MOVE 40		 TO WS-S1.
      *    旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
      *    �                 FIND THE LENGTH OF THE HEADING	              �
      *    읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸
       BA05.
           IF W50-CCHAR(WS-S1) = SPACE
               SUBTRACT 1                FROM WS-S1
               GO TO BA05.
             SUBTRACT WS-S1 FROM 40      GIVING WS-COUNT.
           IF WS-COUNT > 2
               DIVIDE 2		           INTO WS-COUNT
               SUBTRACT WS-COUNT FROM 40 GIVING WS-S2
      *    旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
      *    �	       CENTRE THE HEADING IN THE DISPLAY WINDOW 	  �
      *    읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸
	       PERFORM TEST AFTER UNTIL WS-S1 = 1
               MOVE W50-CCHAR(WS-S1)     TO W50-CCHAR(WS-S2)
               MOVE SPACE                TO W50-CCHAR(WS-S1)
               SUBTRACT 1                FROM WS-S1 WS-S2
	       END-PERFORM.
      *    旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
      *    �		MOVE CENTERED HEADING TO DISPLAY FIELD		  �
      *    읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸
             MOVE W50-DATA               TO W05-HEADING.

       BA09.
	     EXIT.

       BA10.
	     MOVE CRT-COMPANY	 TO W50-DATA.
	     MOVE 40		 TO WS-S1.
      *    旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
      *    �		  FIND THE LENGTH OF THE COMPANY NAME		  �
      *    읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸
       BA15.
	   IF W50-CCHAR(WS-S1) = SPACE
	       SUBTRACT 1	 FROM WS-S1
	       GO TO BA15.
	     SUBTRACT WS-S1 FROM 40
				 GIVING WS-COUNT.
	   IF WS-COUNT > 2
	       DIVIDE 2		 INTO WS-COUNT
	       SUBTRACT WS-COUNT FROM 40
				 GIVING WS-S2
      *    旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
      *    �	       CENTRE COMPANY IN THE DISPLAY WINDOW     	  �
      *    읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸
	       PERFORM TEST AFTER UNTIL WS-S1 = 1
		   MOVE W50-CCHAR(WS-S1)	TO W50-CCHAR(WS-S2)
		   MOVE SPACE	 TO W50-CCHAR(WS-S1)
		   SUBTRACT 1	 FROM WS-S1 WS-S2
	       END-PERFORM.
	     MOVE W50-DATA	 TO W05-COMPANY.

       BA19.
	     EXIT.

       BA20.
	     MOVE CRT-PROGRAM	 TO W50-DATA.
	     MOVE 15		 TO WS-S1.

       BA25.
	   IF W50-CCHAR(WS-S1) = SPACE
	       SUBTRACT 1	 FROM WS-S1
	       GO TO BA25.
      *    旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
      *    �		      RIGHT JUSTIFY PROGRAM NAME                  �
      *    읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸
	   IF WS-S1 < 15
	       MOVE 15		 TO WS-S2
	       PERFORM TEST AFTER UNTIL WS-S1 = 1
		  MOVE W50-CCHAR(WS-S1) TO W50-CCHAR(WS-S2)
		  MOVE SPACE		TO W50-CCHAR(WS-S1)
		  SUBTRACT 1	 FROM WS-S1 WS-S2
	       END-PERFORM
	     MOVE W50-15	 TO W05-PROGRAM.

       BA29.
	     EXIT.
