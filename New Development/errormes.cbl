      $set linkcount"384"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.            ERRORMES.
       AUTHOR.                JW Lemmon.
       DATE-WRITTEN.          FEB-2012.
       INSTALLATION.          APAC.

      ****
      *
      *  Used to display an error message in a window at the line
      *  specified by the calling routine. The error message may be up
      *  to 46 characters in length
      *
      ****

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WS-S1             PIC S9(04)    COMP-3.
       77  WS-S2             PIC S9(04)    COMP-3.
       77  WS-S3             PIC S9(04)    COMP-3.
       77  WS-S4             PIC S9(04)    COMP-3.
       77  WS-OPTION         PIC  X(01).

       COPY "HEADING.CRT".

       COPY "WS.WS".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       LINKAGE SECTION.

       COPY "CHAIN.LS".

       01  L-MESSAGE         PIC  X(48).
       01  L-LINE            PIC  9(02).

       SCREEN SECTION.

       01  ERROR-LINE.
           03  BACKGROUND-COLOR Red.
               05		    COLUMN 16 VALUE "здддддддддддддддддддддддддддддддддддддддддддддддд" FOREGROUND-COLOR Red   HIGHLIGHT.
               05		    COLUMN 65 VALUE "©"                                                 FOREGROUND-COLOR Black.
               05  LINE + 1 COLUMN 16 VALUE "Ё"                                                 FOREGROUND-COLOR Red   HIGHLIGHT.
               05		    COLUMN 17 PIC  X(48) FROM WS-ERR-MES                                FOREGROUND-COLOR Grey  HIGHLIGHT.
               05		    COLUMN 65 VALUE "Ё"                                                 FOREGROUND-COLOR Black.
               05  LINE + 1 COLUMN 16 VALUE "Ё           Press any key to continue            " FOREGROUND-COLOR Grey.
               05           COLUMN 65 VALUE "Ё"                                                 FOREGROUND-COLOR Black.
               05  LINE + 1 COLUMN 16 VALUE "ю"                                                 FOREGROUND-COLOR Red   HIGHLIGHT.
               05		    COLUMN 17 VALUE "дддддддддддддддддддддддддддддддддддддддддддддддды" FOREGROUND-COLOR Black.

       PROCEDURE DIVISION USING L-MESSAGE L-LINE LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000 SECTION.
       AA00.
             MOVE L-MESSAGE          TO WS-ERR-MES
           IF NOT(L-LINE = ZERO)
               MOVE L-LINE           TO SLIN
           ELSE
               MOVE 20               TO SLIN
           END-IF.
             PERFORM ERROR-MESSAGE.

       AA999.
             EXIT PROGRAM.

       COPY "FUNCTION.SCR".

      *    *************************************************************
      *    ****   T H I S   R O U T I N E   I S   U S E D   T O
      *           D I S P L A Y   E R R O R   M E S S A G E S
      *    *************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                        ERROR-MESSAGE                      Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       ERROR-MESSAGE   SECTION.
       ERROR-LENGTH.
             MOVE SPACE              TO WS-OPTION
             MOVE 48                 TO WS-S1.
             MOVE 64                 TO SCOL.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		Calculate the LENGTH of the MESSAGE	           Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       ERROR-LOOP.
           IF WS-MES-CHAR (WS-S1) = SPACE
               SUBTRACT 1            FROM WS-S1
	         GO TO ERROR-LOOP.
             SUBTRACT WS-S1 FROM 48  GIVING WS-COUNT.
           IF WS-COUNT < 3
               GO TO ERROR-POS.
             DIVIDE 2                INTO WS-COUNT.
             SUBTRACT WS-COUNT FROM 48 GIVING WS-S2.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	     Center the message in the DISPLAY WINDOW	     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       ERROR-CENTRE.
             MOVE WS-MES-CHAR(WS-S1) TO WS-MES-CHAR(WS-S2).
             MOVE SPACE              TO WS-MES-CHAR(WS-S1).
           IF WS-S1 > 1
               SUBTRACT 1            FROM WS-S1 WS-S2
               GO TO ERROR-CENTRE.

       ERROR-POS.
             PERFORM SAVE-SCREEN.

       ERROR-DISPLAY.
             ADD 1 SLIN              GIVING SHADE-ROW.
             MOVE 18                 TO SHADE-COL.
             MOVE 48                 TO SHADE-WIDTH.
             MOVE 2                  TO SHADE-LINES.
             DISPLAY ERROR-LINE AT LINE SLIN.
             PERFORM SCREEN-SHADOW.
             PERFORM MESSAGE-INST.

       ERROR-REPLY.
             ADD 1                   TO SLIN.
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
                      GO TO ERROR-DISPLAY
                 WHEN DOWN-KEY
                      PERFORM RESTORE-SCREEN
                      IF SLIN > 42
                         MOVE 42     TO SLIN
                      END-IF
                      GO TO ERROR-DISPLAY
                 WHEN ENTER-KEY    GO TO ERROR-END
                 WHEN OTHER	     CALL X"E5"
               END-EVALUATE
               GO TO ERROR-REPLY
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X      TO WS-OPTION.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.

       ERROR-END.
             PERFORM DISPLAY-THE-CURSOR.
             PERFORM RESTORE-SCREEN.

       ERROR-EXIT.
             EXIT.
