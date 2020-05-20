      $set linkcount"384"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.            DISPMESS.
       AUTHOR.                JW Lemmon.
       DATE-WRITTEN.          FEB-2012.
       INSTALLATION.          APAC.

      ****
      *
      *  Used to display an information message in a window at the line
      *  specified by the calling routine. The information message may
      *  be up to 46 characters in length
      *
      ****

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WS-S1                   PIC S9(04)    COMP-3.
       77  WS-S2                   PIC S9(04)    COMP-3.
       77  WS-S3                   PIC S9(04)    COMP-3.
       77  WS-S4                   PIC S9(04)    COMP-3.
       77  WS-OPTION               PIC  X(01).

       COPY "HEADING.CRT".

       COPY "WS.WS".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       LINKAGE SECTION.

       COPY "CHAIN.LS".

       01  L-MESSAGE               PIC  X(48).
       01  L-LINE                  PIC  9(02).

       SCREEN SECTION.

       01  DISPLAY-LINE.
           03  BACKGROUND-COLOR Green.
               05		    COLUMN 16 VALUE "здддддддддддддддддддддддддддддддддддддддддддддддд" FOREGROUND-COLOR Green HIGHLIGHT.
               05		    COLUMN 65 VALUE "©"                                                 FOREGROUND-COLOR Black.
               05  LINE + 1 COLUMN 16 VALUE "Ё"                                                 FOREGROUND-COLOR Green HIGHLIGHT.
               05		    COLUMN 17 PIC  X(48) FROM WS-DSP-MES                                FOREGROUND-COLOR Grey  HIGHLIGHT.
               05		    COLUMN 65 VALUE "Ё"                                                 FOREGROUND-COLOR Black.
               05  LINE + 1 COLUMN 16 VALUE "Ё"                                                 FOREGROUND-COLOR Green HIGHLIGHT.
               05           COLUMN 17 VALUE "           Press any key to continue            "  FOREGROUND-COLOR Grey.
               05           COLUMN 65 VALUE "Ё"                                                 FOREGROUND-COLOR Black.
               05  LINE + 1 COLUMN 16 VALUE "ю"                                                 FOREGROUND-COLOR Green HIGHLIGHT.
               05		    COLUMN 17 VALUE "дддддддддддддддддддддддддддддддддддддддддддддддды" FOREGROUND-COLOR Black.

       PROCEDURE DIVISION USING L-MESSAGE L-LINE LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       A-MAIN SECTION.
       AA000.
             MOVE L-MESSAGE          TO WS-DSP-MES
           IF NOT(L-LINE = ZERO)
               MOVE L-LINE           TO SLIN
           ELSE
               MOVE 20               TO SLIN
           END-IF.
             PERFORM DISPLAY-MESSAGE.

       AA999.
             EXIT PROGRAM.

       COPY "FUNCTION.SCR".

      *    *************************************************************
      *    ****   T H I S   R O U T I N E   I S   U S E D   T O
      *                  D I S P L A Y   M E S S A G E S
      *    *************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                        DISPLAY-MESSAGE                      Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       DISPLAY-MESSAGE   SECTION.
       DISPLAY-LENGTH.
             MOVE SPACE              TO WS-OPTION
             MOVE 64                 TO SCOL.
             MOVE 48                 TO WS-S1.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		Calculate the LENGTH of the MESSAGE	           Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       DISPLAY-LOOP.
           IF WS-MES-CHAR(WS-S1) = SPACE
               SUBTRACT 1            FROM WS-S1
	         GO TO DISPLAY-LOOP.
             SUBTRACT WS-S1 FROM 48  GIVING WS-COUNT.
           IF WS-COUNT < 3
               GO TO DISPLAY-POS.
             DIVIDE 2                INTO WS-COUNT.
             SUBTRACT WS-COUNT FROM 48 GIVING WS-S2.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	     Center the message in the DISPLAY WINDOW	     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       DISPLAY-CENTRE.
             MOVE WS-MES-CHAR(WS-S1) TO WS-MES-CHAR(WS-S2).
             MOVE SPACE              TO WS-MES-CHAR(WS-S1).
           IF WS-S1 > 1
               SUBTRACT 1            FROM WS-S1 WS-S2
               GO TO DISPLAY-CENTRE.

       DISPLAY-POS.
             PERFORM SAVE-SCREEN.

       DISPLAY-MESS-WINDOW.
             ADD 1 SLIN              GIVING SHADE-ROW.
             MOVE 18                 TO SHADE-COL.
             MOVE 48                 TO SHADE-WIDTH.
             MOVE 2                  TO SHADE-LINES.
             DISPLAY DISPLAY-LINE AT LINE SLIN.
             PERFORM SCREEN-SHADOW.
             PERFORM MESSAGE-INST.

       DISPLAY-REPLY.
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
                      GO TO DISPLAY-MESS-WINDOW
                 WHEN DOWN-KEY
                      PERFORM RESTORE-SCREEN
                      IF SLIN > 42
                         MOVE 42     TO SLIN
                      END-IF
                      GO TO DISPLAY-MESS-WINDOW
                 WHEN ENTER-KEY    GO TO DISPLAY-END
                 WHEN OTHER	     CALL X"E5"
               END-EVALUATE
               GO TO DISPLAY-REPLY
           END-IF.

       DISPLAY-END.
             PERFORM DISPLAY-THE-CURSOR.
             PERFORM RESTORE-SCREEN.

       DISPLAY-EXIT.
             EXIT.
