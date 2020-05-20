      $set linkcount"384" GNT"ERRORMES.GNT"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.            ERRORMES.
       AUTHOR.                JW Lemmon.
       DATE-WRITTEN.          FEB-2012.

                COPYRIGHT NOTICE: COPYRIGHT (C) 2008 - 2016
                                  by James William Lemmon.
                                    (Id No. 4412165050082).

                All rights reserved.

                e-mail jwlemmon@gmail.com.

       SECURITY.
                This program is free software; you can redistribute
                it and/or modify it under the terms of the GNU General
                Public License as published by the Free Software
                Foundation; either version 3 of the License, or (at
                your option) any later version.

                This program is distributed in the hope that it will
                be useful, but WITHOUT ANY WARRANTY; without even the
                implied warranty of MERCHANTABILITY or FITNESS FOR A
                PARTICULAR PURPOSE. See the GNU General Public License
                for more details.

                You should have received a copy of the GNU General
                Public License along with this program. If not, see
                <http://www.gnu.org/licenses/>.

       INSTALLATION.          APAC.

      ****
      *
      *  Used to display an error message in a window at the line
      *  specified by the calling routine. The error message may be up
      *  to 48 characters in length.
      *  A one character reponse from the User will be returned and may
      *  be used by the calling program if required.
      *
      ****
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         CURSOR IS CSTART
                         CONSOLE IS CRT
                         CRT STATUS IS KEY-STATUS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WS-S1             PIC S9(04)    COMP-3.
       77  WS-S2             PIC S9(04)    COMP-3.
       77  WS-S3             PIC S9(04)    COMP-3.
       77  WS-S4             PIC S9(04)    COMP-3.
       77  WS-OPTION         PIC  X(01).
       77  TODAY-DDMMYY      PIC  9(08).

       COPY "HEADING.CRT".

       COPY "WS.WS".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       LINKAGE SECTION.

       77  LS-OPTION         PIC  X(01).
       01  L-MESSAGE         PIC  X(48).
       01  L-LINE            PIC  9(02).

       COPY "USER.LS".
       
       SCREEN SECTION.

       01  ERROR-LINE.
           03  BACKGROUND-COLOR Red FOREGROUND-COLOR Black.
               05           COLUMN 16 VALUE "                                                  ".
               05  LINE + 1 COLUMN 16 VALUE " ".
               05           COLUMN 17 PIC  X(48) FROM WS-ERR-MES                                 FOREGROUND-COLOR Grey  HIGHLIGHT.
               05           COLUMN 65 VALUE                                                  " ".
               05  LINE + 1 COLUMN 16 VALUE "            Press any key to continue             ".
               05  LINE + 1 COLUMN 16 VALUE "                                                  ".

       PROCEDURE DIVISION USING LS-OPTION L-MESSAGE L-LINE LS-USER-ID.
       AA000 SECTION.
       AA00.
           IF WS-OPTION = X"FF"
               MOVE "Security level - Insufficient" TO WS-ERR-MES
           ELSE
               MOVE L-MESSAGE        TO WS-ERR-MES.
             MOVE LS-OPTION          TO WS-OPTION.
           IF NOT(L-LINE = ZERO)
               MOVE L-LINE           TO SLIN
           ELSE
               MOVE 20               TO SLIN
           END-IF.
             PERFORM ERROR-MESSAGE.
             MOVE WS-OPTION          TO LS-OPTION.

       AA999.
             EXIT PROGRAM.

      *    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *           S C R E E N ,   K E Y B O A R D   &  M O U S E
      *    *************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё       SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3      Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё                       SCREEN-SHADOW                       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To routine is used to display a shadow down the right and Ё
      *    Ё along the bottom of a pop-up box. The parameters that are Ё
      *    Ё required:                                                 Ё
      *    Ё           SHADE-ROW   - Top line of the box + 1.          Ё
      *    Ё           SHADE-COL   - Left line of box + 2.             Ё
      *    Ё           SHADE-WIDTH - Width of the box - 2.             Ё
      *    Ё           SHADE-LINES - Hight of box - 1.                 Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "FUNCTION.SCR".

       COPY "CLEAR-L50.CRT".

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
      *    Ё            Calculate the LENGTH of the MESSAGE            Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       ERROR-LOOP.
           IF WS-MES-CHAR(WS-S1) = SPACE
               SUBTRACT 1            FROM WS-S1
               GO TO ERROR-LOOP.
             SUBTRACT WS-S1 FROM 48  GIVING WS-COUNT.
           IF WS-COUNT < 3
               GO TO ERROR-POS.
             DIVIDE 2                INTO WS-COUNT.
             SUBTRACT WS-COUNT FROM 48 GIVING WS-S2.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё          Center the message in the DISPLAY WINDOW         Ё
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
             MOVE SLIN               TO SHADE-ROW.
             MOVE 16                 TO SHADE-COL.
             MOVE 50                 TO SHADE-WIDTH.
             MOVE 4                  TO SHADE-LINES.
             DISPLAY ERROR-LINE AT LINE SLIN.
             PERFORM SCREEN-SHADOW.
             PERFORM MESSAGE-INST.

       ERROR-REPLY.
             ADD 1                   TO SLIN.
             PERFORM HIDE-THE-CURSOR.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     PERFORM RESTORE-SCREEN
                                 IF SLIN > 3
                                     SUBTRACT 2 FROM SLIN
                                 ELSE
                                     MOVE 2     TO SLIN
                                 END-IF
                                 GO TO ERROR-DISPLAY

                 WHEN DOWN-KEY   PERFORM RESTORE-SCREEN
                                 IF SLIN > 42
                                     MOVE 42     TO SLIN
                                 END-IF
                                 GO TO ERROR-DISPLAY

                 WHEN ENTER-KEY  GO TO ERROR-END

                 WHEN OTHER      CALL X"E5" CALL X"E5" CALL X"E5"
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
