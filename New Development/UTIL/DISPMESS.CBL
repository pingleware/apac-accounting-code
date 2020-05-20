      $set linkcount"384" GNT"DISPMESS.GNT"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.            DISPMESS.
       AUTHOR.                JW Lemmon.
       DATE-WRITTEN.          FEB-2012.

                COPYRIGHT NOTICE: COPYRIGHT (C) 2008 - 2018
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
      *  Used to display an information message in a window at the line
      *  specified by the calling routine. The information message may
      *  be up to 48 characters in length
      *
      ****
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         CURSOR IS CSTART
                         CONSOLE IS CRT
                         CRT STATUS IS KEY-STATUS.

       DATA DIVISION.
      *
      *         **        **    *****    ******    **   **
      *         **        **   **   **   **   **   **  **
      *         **        **   **   **   **  **    ** **
      *         **   **   **   **   **   *****     ****
      *          ** **** **    **   **   **  **    ** **
      *           ***  ***     **   **   **   **   **  **
      *            *    *       *****    **   **   **   **
      *
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

      *
      *    **       ******  **    **  **   **    ***     *****   ******
      *    **         **    ***   **  **  **    ** **   **   **  **
      *    **         **    ****  **  ** **    **   **  **       ** 
      *    **         **    ** ** **  ****     *******  **       *****
      *    **         **    **  ****  ** **    **   **  **  ***  **  
      *    **         **    **   ***  **  **   **   **  **   **  **
      *    *******  ******  **    **  **   **  **   **   *****   ******
      *
       LINKAGE SECTION.

       01  L-MESSAGE     PIC  X(48).
       01  L-LINE        PIC  9(02).

       COPY "USER.LS".

      * 
      *   *****    *****   ******   ******  ******  **    **
      *  **   **  **   **  **   **  **      **      ***   **
      *  **       **       **  **   **      **      ****  **
      *   *****   **       *****    *****   *****   ** ** **
      *       **  **       **  **   **      **      **  ****
      *  **   **  **   **  **   **  **      **      **   ***
      *   *****    *****   **   **  ******  ******  **    **
      *
       SCREEN SECTION.

       01  DISPLAY-LINE.
           03  BACKGROUND-COLOR Green FOREGROUND-COLOR Black.
               05           COLUMN 16 VALUE "                                                  ".
               05  LINE + 1 COLUMN 16 VALUE " ".
               05           COLUMN 17 PIC  X(48) FROM WS-DSP-MES                                FOREGROUND-COLOR Grey  HIGHLIGHT.
               05           COLUMN 65 VALUE                                                  " ".
               05  LINE + 1 COLUMN 16 VALUE "            Press any key to continue             ".
               05  LINE + 1 COLUMN 16 VALUE "                                                  ".

      *
      *      ******   ******    *****    *****   ******  ******   **   **  ******    ****** 
      *      **   **  **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **   **  **  **   **   **  **       **      **   **  **   **  **  **    **
      *      ******   *****    **   **  **       *****   **   **  **   **  *****     *****
      *      **       **  **   **   **  **       **      **   **  **   **  **  **    **
      *      **       **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **       **   **   *****    *****   ******  ******    *****   **   **   ******
      *
       PROCEDURE DIVISION USING L-MESSAGE L-LINE LS-USER-ID.
       A-MAIN SECTION.
       AA000.
             MOVE L-MESSAGE          TO WS-DSP-MES
           IF NOT(L-LINE = ZERO)
               MOVE L-LINE           TO SLIN
           ELSE
               MOVE 20               TO SLIN
           END-IF.
             PERFORM DISPLAY-MESSAGE.

       AA99.
             EXIT PROGRAM.

       COPY "AA900.ALM".

      *    
      *            ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *           S C R E E N ,   K E Y B O A R D   &   M O U S E
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё      SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3       Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё                      SCREEN-SHADOW                        Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To routine is used to display a shadow down the right and Ё
      *    Ё along the bottom of a pop-up box. The parameters that are Ё
      *    Ё required:                                                 Ё
      *    Ё          SHADE-ROW   - Top line of the box.               Ё
      *    Ё          SHADE-COL   - Left line of box.                  Ё
      *    Ё          SHADE-WIDTH - Width of the box.                  Ё
      *    Ё          SHADE-LINES - Height of box.                     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "FUNCTION.SCR".

       COPY "CLEAR-L50.CRT".

      *    *************************************************************
      *    ****   T H I S   R O U T I N E   I S   U S E D   T O
      *                  D I S P L A Y   M E S S A G E S
      *    *************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                      DISPLAY-MESSAGE                      Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       DISPLAY-MESSAGE   SECTION.
       DISPLAY-LENGTH.
             MOVE SPACE              TO WS-OPTION
             MOVE 64                 TO SCOL.
             MOVE 48                 TO WS-S1.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё            Calculate the LENGTH of the MESSAGE            Ё
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
      *    Ё          Center the message in the DISPLAY WINDOW         Ё
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
             MOVE SLIN               TO SHADE-ROW.
             MOVE 16                 TO SHADE-COL.
             MOVE 50                 TO SHADE-WIDTH.
             MOVE 4                  TO SHADE-LINES.
             DISPLAY DISPLAY-LINE AT LINE SLIN.
             PERFORM SCREEN-SHADOW.
             PERFORM MESSAGE-INST.

       DISPLAY-REPLY.
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
                                 GO TO DISPLAY-MESS-WINDOW
                                 
                 WHEN DOWN-KEY   PERFORM RESTORE-SCREEN
                                 IF SLIN > 42
                                     MOVE 42     TO SLIN
                                 END-IF
                                 GO TO DISPLAY-MESS-WINDOW
                                 
                 WHEN ENTER-KEY  GO TO DISPLAY-END
                 
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO DISPLAY-REPLY
           END-IF.

       DISPLAY-END.
             PERFORM DISPLAY-THE-CURSOR.
             PERFORM RESTORE-SCREEN.

       DISPLAY-EXIT.
             EXIT.
