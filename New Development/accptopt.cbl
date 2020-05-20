      $set linkcount"384"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.            acceptopt.
       AUTHOR.                JW Lemmon.
       DATE-WRITTEN.          FEB-2012.
       INSTALLATION.          APAC.

      ****
      *
      *  Used to display a request in a window and get a one character
      *  reponse from a User. The message, including the response
      *  may be up to 46 characters in length
      *
      *  Call this program with WS-ERR-MES containing the message, WS-MES-LINE either 0 or
      *  with a value indicating the top line of the message window. The program allows for
      *  different routines and options which must be set in WS-INSTR. The result will be
      *  returned to the calling program in WS-OPTION .
      *
      *  The value of WS-INSTR may be changed to include additional options or routines.
      *
      *  Current values for WS-INSTR:
      *                     C = Confirm input (Y or N)    Section - CHECK-CORRECT
      *                         This message has been changed from:
      *                                     "Correct ENTER - N if incorrect"
      *                                                    to
      *                                     "Press Y if correct - N if incorrect"
      *                         at the request of USERS. If WS-OPTION contains a Y on
      *                         entry, then pressing the ENTER key will leave WS-OPTION
      *                         with a value of Y.
      *
      *
      *
      ****

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WS-S1             PIC S9(04)    COMP-3.
       77  WS-S2             PIC S9(04)    COMP-3.
       77  WS-S3             PIC S9(04)    COMP-3.
       77  WS-S4             PIC S9(04)    COMP-3.

       COPY "HEADING.CRT".

       COPY "WS.WS".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       LINKAGE SECTION.

       COPY "CHAIN.LS".

       77  WS-OPTION         PIC  X(01).

       01  L-MESSAGE         PIC  X(48).
       01  L-LINE            PIC  9(02).

       SCREEN SECTION.

       01  OPT-LINE.
           02  BACKGROUND-COLOR Cyan.
               03	          COLUMN 16 VALUE "здддддддддддддддддддддддддддддддддддддддддддддддд" HIGHLIGHT.
               03	          COLUMN 65 VALUE "©"                                                 FOREGROUND-COLOR Black.
               03  LINE + 1 COLUMN 16 VALUE "Ё"                                                 HIGHLIGHT.
               03	          COLUMN 17 PIC  X(48) FROM WS-OPT-MES                                FOREGROUND-COLOR Brown HIGHLIGHT.
               03	          COLUMN 65 VALUE "Ё"                                                 FOREGROUND-COLOR Black.
               03  LINE + 1 COLUMN 16 VALUE "ю"                                                 HIGHLIGHT.
               03	          COLUMN 17 VALUE "дддддддддддддддддддддддддддддддддддддддддддддддды" FOREGROUND-COLOR Black.

       01  S99.
	     02  BACKGROUND-COLOR Cyan.
	         03	          COLUMN 19 VALUE "здддддддддддддддддддддддддддддддддддддддд" HIGHLIGHT.
	         03	          COLUMN 60 VALUE "©"                                         FOREGROUND-COLOR Black.
               03  LINE + 1 COLUMN 19 VALUE "Ё"                                         HIGHLIGHT.
	         03	          COLUMN 20 VALUE " Press "                                   FOREGROUND-COLOR Blue.
	         03	          COLUMN 27 VALUE "Y"                                         FOREGROUND-COLOR Brown HIGHLIGHT.
               03           COLUMN 28 VALUE " if correct - "                            FOREGROUND-COLOR Blue.
	         03	          COLUMN 42 VALUE "N"                                         FOREGROUND-COLOR Brown HIGHLIGHT.
	         03	          COLUMN 43 VALUE " if incorrect ["                           FOREGROUND-COLOR Blue.
               03           COLUMN 58 PIC X(01) USING WS-OPTION                         FOREGROUND-COLOR Grey  HIGHLIGHT.
               03           COLUMN 59 VALUE "] "                                        FOREGROUND-COLOR Blue.
	         03	          COLUMN 60 VALUE "Ё"                                         FOREGROUND-COLOR Black.
	         03  LINE + 1 COLUMN 19 VALUE "ю"                                         HIGHLIGHT.
	         03		    COLUMN 20 VALUE "дддддддддддддддддддддддддддддддддддддддды" FOREGROUND-COLOR Black.

       PROCEDURE DIVISION USING L-MESSAGE L-LINE WS-OPTION LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       A-MAIN SECTION.
       AA000.
             MOVE L-MESSAGE        TO WS-OPT-MES.
           IF NOT(L-LINE = ZERO)
               MOVE L-LINE         TO SLIN
           ELSE
               MOVE 20             TO SLIN
           END-IF.
           IF LS-INSTR = "C"
               PERFORM CHECK-CORRECT
           ELSE
               PERFORM OPT-MESSAGE.

       AA999.
             EXIT PROGRAM.

       COPY "FUNCTION.SCR".

       OPT-MESSAGE   SECTION.
       OPT-SETUP.
             MOVE 64               TO SCOL.
             MOVE 48               TO WS-S1.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Delete leading spaces from the message to ensure that the Ё
      *    Ё centering of the message is correct. Included procedure   Ё
      *    Ё to accomodate PERFORM UNTIL.	                             Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
           IF WS-MES-CHAR(1) = SPACE
               PERFORM WITH TEST AFTER UNTIL NOT(WS-MES-CHAR(1) = SPACE)
                   MOVE WS-CONT    TO WS-ERR-MES
	         END-PERFORM
           END-IF.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		Calculate the LENGTH of the MESSAGE            Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       OPT-LOOP.
           IF WS-MES-CHAR(WS-S1) = SPACE
               SUBTRACT 1        FROM WS-S1
               GO TO OPT-LOOP.
             SUBTRACT WS-S1 FROM 48 GIVING WS-COUNT.
           IF WS-COUNT < 3
               MOVE WS-S1          TO WS-COUNT
               GO TO OPT-POS.
             DIVIDE 2              INTO WS-COUNT.
             SUBTRACT WS-COUNT FROM 48 GIVING WS-S2.
             MOVE WS-S2            TO WS-COUNT.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	     Center the message in the DISPLAY WINDOW        Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       OPT-CENTRE.
             MOVE WS-MES-CHAR(WS-S1) TO WS-MES-CHAR(WS-S2).
             MOVE SPACE              TO WS-MES-CHAR(WS-S1).
           IF WS-S1 > 1
               SUBTRACT 1            FROM WS-S1 WS-S2
               GO TO OPT-CENTRE.

       OPT-POS.
             PERFORM SAVE-SCREEN.

       OPT-DISPLAY.
             ADD 1 SLIN            GIVING SHADE-ROW.
             MOVE 18               TO SHADE-COL.
             MOVE 48               TO SHADE-WIDTH.
             MOVE 2                TO SHADE-LINES.
             DISPLAY OPT-LINE AT LINE SLIN.
             PERFORM SCREEN-SHADOW.
             PERFORM MESSAGE-INST.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё If the character to be entered is contained within [ ],   Ё
      *    Ё then move WS-OPTION to that position within WS-ERR-MES    Ё
      *    Ё and display it as white (highlighted) on a magenta back-  Ё
      *    Ё ground. This change; moving WS-OPTION has been included   Ё
      *    Ё as using the CALL X"AF" function does not echo the value  Ё
      *    Ё of the key that has been pressed, which can confuse the   Ё
      *    Ё USER if an incorrect key is pressed.                      Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       OPT-REPLY.
             ADD 1                 TO SLIN.
             MOVE SLIN             TO L-LINE.
           IF WS-MES-CHAR(WS-COUNT) = "]"
               SUBTRACT 1          FROM WS-COUNT
               ADD 16 WS-COUNT     GIVING SCOL
               MOVE WS-OPTION      TO WS-MES-CHAR(WS-COUNT)
               DISPLAY WS-MES-CHAR(WS-COUNT) AT SCREEN-POS WITH FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
               ADD 1               TO WS-COUNT.
             PERFORM HIDE-THE-CURSOR.

       OPT-ACCEPT.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY
                      PERFORM RESTORE-SCREEN
                      IF SLIN > 3
                          SUBTRACT 2 FROM SLIN
                          MOVE SLIN  TO L-LINE
                      ELSE
                          MOVE 2     TO SLIN L-LINE
                      END-IF
                      GO TO OPT-DISPLAY
                 WHEN DOWN-KEY
                      PERFORM RESTORE-SCREEN
                      IF SLIN > 42
                          MOVE 42    TO SLIN L-LINE
                      END-IF
                      GO TO OPT-DISPLAY
                 WHEN ENTER-KEY GO TO OPT-END
                 WHEN OTHER CALL X"E5"
               END-EVALUATE
               GO TO OPT-REPLY
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X      TO WS-OPTION.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.

       OPT-END.
             DISPLAY OPT-LINE AT LINE SLIN.
             PERFORM DISPLAY-THE-CURSOR.
             PERFORM RESTORE-SCREEN.

       OPT-EXIT.
	     EXIT.

       CHECK-CORRECT   SECTION.
       CHECK-POS.
             MOVE SPACE	           TO WS-OPTION.
             MOVE 54               TO SCOL.
             PERFORM SAVE-SCREEN.

       CHECK-DISPLAY.
             ADD 1 SLIN	           GIVING SHADE-ROW.
             MOVE 21               TO SHADE-COL.
             MOVE 41               TO SHADE-WIDTH.
             MOVE 2                TO SHADE-LINES.
             DISPLAY S99 AT LINE SLIN.
             PERFORM SCREEN-SHADOW.
             PERFORM MESSAGE-INST.

       CHECK-REPLY.
             ADD 1                 TO SLIN.
             PERFORM HIDE-THE-CURSOR.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY
                      PERFORM RESTORE-SCREEN
                      IF SLIN > 3
                          SUBTRACT 2 FROM SLIN
                      ELSE
                          MOVE 2     TO SLIN
                      END-IF
                      GO TO CHECK-DISPLAY
                 WHEN DOWN-KEY
                      PERFORM RESTORE-SCREEN
                      IF SLIN > 42
                          MOVE 42    TO SLIN
                      END-IF
                      GO TO CHECK-DISPLAY
                 WHEN ENTER-KEY  GO TO CHECK-UPPER
                 WHEN OTHER	   CALL X"E5"
               END-EVALUATE
               GO TO CHECK-REPLY
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X  TO WS-OPTION.

       CHECK-UPPER.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
             DISPLAY S99 AT LINE SLIN.
           IF NOT(WS-OPTION = "Y" OR "N")
               GO TO CHECK-REPLY.
             PERFORM DISPLAY-THE-CURSOR.
             PERFORM RESTORE-SCREEN.

       CHECK-EXIT.
	     EXIT.
