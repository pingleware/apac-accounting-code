      $set linkcount"384" GNT"ACCPTOPT.GNT"
      ******************************************************************
      *                                                                *
      *    ***    ****   ****  ******  ******   *****  ******  ******  *
      *   ** **  **  ** **  ** **   **   **    **   ** **   **   **    *
      *  **   ** **     **     **   **   **    **   ** **   **   **    *
      *  ******* **     **     ******    **    **   ** ******    **    *
      *  **   ** **     **     **        **    **   ** **        **    *
      *  **   ** **  ** **  ** **        **    **   ** **        **    *
      *  **   **  ****   ****  **        **     *****  **        **    *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *  A P A C   S Y S T E M :  A C C E P T   U S E R   O P T I O N  *
      *                                                                *
      *       Version 9.04.05 - June 2018                              *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.            ACCPTOPT.
       AUTHOR.                JW Lemmon.
       DATE-WRITTEN.          FEB-2012.

                COPYRIGHT NOTICE: COPYRIGHT (C) 2012 - 2018
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
      *  This program is used to get an option from the User.
      *
      *  If character 48 of the message contains a value of X"FA" then do not display the 
      *  message in a window, only get the User reponse at the position specified by
      *  WS-MES-LINE and WS-MES-COL.
      *
      *  Used to display a request in a window and get a one character
      *  reponse from a User. The message, including the response
      *  may be up to 46 characters in length
      *
      *  Call this program with WS-OPT-MES containing the message, WS-MES-LINE either 0 or
      *  with a value indicating the top line of the message window. The program allows for
      *  different routines and options which must be set in WS-INSTR. The result will be
      *  returned to the calling program in WS-OPTION.
      *
      *  The value of WS-INSTR may be changed to include additional options or routines.
      *
      *            THE FOLLOWING LIST IS AN ALPHABETICAL LIST OF VALID OPTIONS
      *                       (1st, 2nd ... OF EACH SELECTION) 
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
      *                     C - See note above.
      *
      *                     Z = Any key is valid
      *
      *                     0 = "0" OR "1" OR "2" OR "3"
      *
      *         (Hex 6B)    k = "A" OR "1" OR "2" OR "3"
      *
      *                     A = "A" OR "C"
      *
      *         (Hex 62)    b = "A" OR "C" OR "3" OR "6" OR "7"
      *
      *                     D = "A", "C", "M" OR "P"
      *
      *                     J = "A" OR "D"
      *                          Allow for Escape key
      *
      *                     8 = "A", "D" OR "E"
      *
      *         (Hex 66)    f = "A" OR "E"
      *
      *                     T = "A" OR "E" OR "O"
      *                          Allow for Escape key
      *
      *                     H = "A" OR "M"
      *
      *                     R = "A" OR "N"
      *
      *                     N = "A" OR "R"
      *
      *         (Hex 65)    e = "A" OR "S"
      *
      *                     5 = "B", "N" OR "P"
      *
      *                     X = "B", "Q" OR "V"
      *
      *                     Y = "B" OR "S"
      *
      *         (Hex 69)    i = "C"
      *
      *         (Hex 68)    h = "C" "D" OR "E"
      *                          Allow for the Escape key.
      *
      *                     V = "C" OR "E"
      *
      *                     W = "C" "E" "L" "P" OR "R"
      *
      *                     Q = "C", "E" OR "M"
      *
      *                     F = "C", "E" OR "R"
      *                          Allow for the F9 function key.
      *
      *                     M = "C", "I", "K" OR "N"
      *
      *                     2 = "C", "I" OR "R"
      *
      *                     7 = "C", "M" OR "P"
      *
      *                     P = "C" OR "P"
      *
      *                     9 = "C" OR "R"
      *
      *         (Hex 61)    a = "C" OR "S"
      *
      *         (Hex 6D)    m = "D" OR "E" OR "N" OR "P" OR "X"
      *
      *         (Hex 6E)    n = "D" OR "E" OR "P"
      *
      *                     U = "D" OR "I"
      *
      *                     S = "D" OR "N"
      *
      *         (Hex 67)    g = "D" OR "S"
      *
      *                     E = "E"
      *
      *         (Hex 6C)    l = "E" OR "F"
      *
      *                     K = "E", "H" OR "S"
      *
      *                     4 = "E", "I" OR "N"
      *
      *                     G = "E", "I" OR "X"
      *                          Allow for Escape key
      *
      *         (Hex 64)    d = "E", "M" OR "P"
      *                          Allow for Escape key
      *
      *                     I = "E" OR "N"
      *
      *         (Hex 6A)    j = "E" OR "N"
      *                          Allow for Escape key
      *
      *                     B = "E", "N" OR "R"
      *
      *                     6 = "E", "N" OR "V"
      *
      *                     L = "E", "O", "R" OR "S"
      *
      *                     3 = "E" OR "R"
      *
      *                     O = "I" OR "O"
      *
      *                     1 = "N" OR "Y"
      *
      *         (Hex 63)    c = "P" OR "R"
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
       77  WS-S1                     PIC S9(04)    COMP-3.
       77  WS-S2                     PIC S9(04)    COMP-3.
       77  WS-S3                     PIC S9(04)    COMP-3.
       77  WS-S4                     PIC S9(04)    COMP-3.
       77  WS-ERROR                  PIC  9(01).
       77  WS-INSTR                  PIC  X(01).
       77  WS-OPTION                 PIC  X(01).
           88  OPT-0                               VALUES "0" "1" "2" "3".
           88  OPT-1                               VALUES "N" "Y".
           88  OPT-2                               VALUES "C" "I" "R".
           88  OPT-3                               VALUES "E" "R".
           88  OPT-4                               VALUES "E" "I" "N".
           88  OPT-5                               VALUES "B" "N" "P".
           88  OPT-6                               VALUES "E" "N" "V".
           88  OPT-7                               VALUES "C" "M" "P".
           88  OPT-8                               VALUES "A" "D" "E".
           88  OPT-9                               VALUES "C" "R".
           88  OPT-A                               VALUES "A" "C".
           88  OPT-B                               VALUES "E" "N" "R".
           88  OPT-D                               VALUES "A" "C" "M" "P".
           88  OPT-E                               VALUES "E" " ".
           88  OPT-F                               VALUES "C" "E" "R".
           88  OPT-G                               VALUES "E" "I" "X".
           88  OPT-H                               VALUES "A" "M".
           88  OPT-I                               VALUES "E" "N".
           88  OPT-J                               VALUES "A" "D".
           88  OPT-K                               VALUES "E" "H" "S".
           88  OPT-L                               VALUES "E" "O" "R" "S".
           88  OPT-M                               VALUES "C" "I" "K" "N".
           88  OPT-N                               VALUES "A" "R".
           88  OPT-O                               VALUES "O" "I".
           88  OPT-P                               VALUES "C" "P".
           88  OPT-Q                               VALUES "C" "E" "M".
           88  OPT-R                               VALUES "A" "N".
           88  OPT-S                               VALUES "D" "N".
           88  OPT-T                               VALUES "A" "E" "O".
           88  OPT-U                               VALUES "D" "I".
           88  OPT-V                               VALUES "C" "E".
           88  OPT-W                               VALUES "C" "E" "L" "P" "R".
           88  OPT-X                               VALUES "B" "Q" "V".
           88  OPT-Y                               VALUES "B" "S".
           88  OPT-61                              VALUES "C" "S".
           88  OPT-62                              VALUES "3" "6" "7" "A" "C".
           88  OPT-63                              VALUES "P" "R".
           88  OPT-64                              VALUES "E" "M" "P".
           88  OPT-65                              VALUES "A" "S".
           88  OPT-66                              VALUES "A" "E".
           88  OPT-67                              VALUES "D" "S".
           88  OPT-68                              VALUES "C" "D" "E".
           88  OPT-69                              VALUES "C".
           88  OPT-6A                              VALUES "E" "N".
           88  OPT-6B                              VALUES "A" "1" "2" "3".
           88  OPT-6C                              VALUES "E" "F".
           88  OPT-6D                              VALUES "D" "E" "N" "P" "X".
           88  OPT-6D                              VALUES "D" "E" "P".

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
       77  LWS-INSTR                 PIC  X(01).

       77  LS-OPTION                 PIC  X(01).

       01  L-MESSAGE.
           03  FILLER                PIC  X(47).
           03  L-C48                 PIC  X(01).
       01  L-LINE                    PIC  9(02).

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

       01  OPT-LINE.
           02  BACKGROUND-COLOR Cyan.
               03           COLUMN 16 VALUE "                                                  ".
               03  LINE + 1 COLUMN 16 VALUE " ".
               03           COLUMN 17 PIC  X(48) FROM WS-OPT-MES                                FOREGROUND-COLOR Brown HIGHLIGHT.
               03           COLUMN 65 VALUE                                                  " ".
               03  LINE + 1 COLUMN 16 VALUE "                                                  ".

       01  S99.
           02  BACKGROUND-COLOR Cyan.
               03           COLUMN 19 VALUE "                                          ".
               03  LINE + 1 COLUMN 19 VALUE "  Press "                                   FOREGROUND-COLOR Blue.
               03           COLUMN 27 VALUE         "Y"                                  FOREGROUND-COLOR Brown HIGHLIGHT.
               03           COLUMN 28 VALUE          " if correct - "                    FOREGROUND-COLOR Blue.
               03           COLUMN 42 VALUE                        "N"                   FOREGROUND-COLOR Brown HIGHLIGHT.
               03           COLUMN 43 VALUE                         " if incorrect ["    FOREGROUND-COLOR Blue.
               03           COLUMN 58 PIC X(01) USING WS-OPTION                          FOREGROUND-COLOR Grey  HIGHLIGHT.
               03           COLUMN 59 VALUE                                         "] " FOREGROUND-COLOR Blue.
               03  LINE + 1 COLUMN 19 VALUE "                                          ".

      *
      *      ******   ******    *****    *****   ******  ******   **   **  ******    ****** 
      *      **   **  **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **   **  **  **   **   **  **       **      **   **  **   **  **  **    **
      *      ******   *****    **   **  **       *****   **   **  **   **  *****     *****
      *      **       **  **   **   **  **       **      **   **  **   **  **  **    **
      *      **       **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **       **   **   *****    *****   ******  ******    *****   **   **   ******
      *
       PROCEDURE DIVISION USING LWS-INSTR LS-OPTION L-MESSAGE L-LINE LS-USER-ID.
       A-MAIN SECTION.
       AA000.
             MOVE LWS-INSTR          TO WS-INSTR.
             MOVE LS-OPTION          TO WS-OPTION.
             MOVE L-MESSAGE          TO WS-OPT-MES.
           IF NOT(L-LINE = ZERO)
               MOVE L-LINE           TO SLIN
           ELSE
               MOVE 20               TO SLIN
           END-IF.
      *
      *    Check to find out if only an ACCEPT is required
      *
           IF L-C48 = X"FA"
               PERFORM GET-INPUT
      *
      *    Clear character 48
      *
               MOVE SPACE            TO L-C48
           ELSE    
               EVALUATE WS-INSTR
                 WHEN "C"    PERFORM CHECK-CORRECT
                 WHEN OTHER  PERFORM OPT-MESSAGE
               END-EVALUATE.
             MOVE SPACE              TO LWS-INSTR.
             MOVE WS-OPTION          TO LS-OPTION.

       AA199.
             EXIT PROGRAM.

       COPY "AA900.ALM".

       CHANGE-TIME-EXIT.
             EXIT.
             
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                         CLEAR-L50                         ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    *************************************************************
      *           T H I S   R O U T I N E   I S   U S E D   T O
      *       C L E A R   L I N E   50   O N   T H E   S C R E E N
      *    *************************************************************
       CLEAR-L50   SECTION.
       CLEAR-SETUP.
             MOVE "C"            TO CRT-TYPE.
             CALL "UTP\CRTHEAD" USING CRT-HEADINGS LS-USER-ID.

       CLEAR-EXIT.
             EXIT.
             
      *    
      *            ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *           S C R E E N ,   K E Y B O A R D   &   M O U S E
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³      SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3       ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³                      SCREEN-SHADOW                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To routine is used to display a shadow down the right and ³
      *    ³ along the bottom of a pop-up box. The parameters that are ³
      *    ³ required:                                                 ³
      *    ³          SHADE-ROW   - Top line of the box.               ³
      *    ³          SHADE-COL   - Left line of box.                  ³
      *    ³          SHADE-WIDTH - Width of the box.                  ³
      *    ³          SHADE-LINES - Height of box.                     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.SCR".

       OPT-MESSAGE   SECTION.
       OPT-SETUP.
             MOVE 64                 TO SCOL.
             MOVE 48                 TO WS-S1.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Delete leading spaces from the message to ensure that the ³
      *    ³ centering of the message is correct. Included procedure   ³
      *    ³ to accomodate PERFORM UNTIL.                              ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF WS-MES-CHAR(1) = SPACE
               PERFORM WITH TEST AFTER UNTIL NOT(WS-MES-CHAR(1) = SPACE)
                 MOVE WS-CONT        TO WS-OPT-MES
               END-PERFORM
           END-IF.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³            Calculate the LENGTH of the MESSAGE            ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       OPT-LOOP.
           IF WS-MES-CHAR(WS-S1) = SPACE
               SUBTRACT 1            FROM WS-S1
               GO TO OPT-LOOP.
             SUBTRACT WS-S1 FROM 48  GIVING WS-COUNT.
           IF WS-COUNT < 3
               MOVE WS-S1            TO WS-COUNT
               GO TO OPT-POS.
             DIVIDE 2                INTO WS-COUNT.
             SUBTRACT WS-COUNT FROM 48 GIVING WS-S2.
             MOVE WS-S2              TO WS-COUNT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³           Center the message in the DISPLAY WINDOW        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       OPT-CENTRE.
             MOVE WS-MES-CHAR(WS-S1) TO WS-MES-CHAR(WS-S2).
             MOVE SPACE              TO WS-MES-CHAR(WS-S1).
           IF WS-S1 > 1
               SUBTRACT 1            FROM WS-S1 WS-S2
               GO TO OPT-CENTRE.

       OPT-POS.
             PERFORM SAVE-SCREEN.

       OPT-DISPLAY.
             MOVE SLIN               TO SHADE-ROW.
             MOVE 16                 TO SHADE-COL.
             MOVE 50                 TO SHADE-WIDTH.
             MOVE 3                  TO SHADE-LINES.
             DISPLAY OPT-LINE AT LINE SLIN.
             PERFORM SCREEN-SHADOW.
             PERFORM MESSAGE-INST.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the character to be entered is contained within [ ],   ³
      *    ³ then move WS-OPTION to that position within WS-OPT-MES    ³
      *    ³ and display it as white (highlighted) on a magenta back-  ³
      *    ³ ground. This change; moving WS-OPTION has been included   ³
      *    ³ as using the CALL X"AF" function does not echo the value  ³
      *    ³ of the key that has been pressed, which can confuse the   ³
      *    ³ USER if an incorrect key is pressed.                      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       OPT-REPLY.
             MOVE SLIN               TO L-LINE.
             ADD 1                   TO SLIN.
           IF WS-MES-CHAR(WS-COUNT) = "]"
               SUBTRACT 1            FROM WS-COUNT
               ADD 16 WS-COUNT       GIVING SCOL
               MOVE WS-OPTION        TO WS-MES-CHAR(WS-COUNT)
               DISPLAY WS-MES-CHAR(WS-COUNT) AT SCREEN-POS WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
               ADD 1                 TO WS-COUNT.
             PERFORM HIDE-THE-CURSOR.

       OPT-ACCEPT.
             MOVE ZERO               TO WS-ERROR.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN F9-KEY     IF WS-INSTR = "F" OR "Z"
                                     MOVE X"F9"  TO WS-OPTION
                                     GO TO OPT-END
                                 ELSE
                                     GO TO OPT-ACCEPT
                                 END-IF

                 WHEN UP-KEY     PERFORM RESTORE-SCREEN
                                 IF SLIN > 3
                                     SUBTRACT 2  FROM SLIN
                                     MOVE SLIN   TO L-LINE
                                 ELSE
                                     MOVE 2      TO SLIN L-LINE
                                 END-IF
                                 GO TO OPT-DISPLAY

                 WHEN DOWN-KEY   PERFORM RESTORE-SCREEN
                                 IF SLIN > 42
                                     MOVE 42     TO SLIN L-LINE
                                 END-IF
                                 GO TO OPT-DISPLAY

                 WHEN ENTER-KEY  GO TO OPT-VALID

                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO OPT-ACCEPT
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    IF WS-INSTR = "G" OR "J" OR "T" OR "Z" OR "d" OR "h" OR "j"
                                     MOVE X"EC"  TO WS-OPTION
                                     GO TO OPT-END
                                 ELSE
                                     GO TO OPT-ACCEPT
                                 END-IF
                                 
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO OPT-ACCEPT
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X      TO WS-OPTION.

       OPT-VALID.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
             EVALUATE WS-INSTR
               WHEN "0"    IF OPT-0  GO TO OPT-END
               WHEN "1"    IF OPT-1  GO TO OPT-END
               WHEN "2"    IF OPT-2  GO TO OPT-END
               WHEN "3"    IF OPT-3  GO TO OPT-END
               WHEN "4"    IF OPT-4  GO TO OPT-END
               WHEN "5"    IF OPT-5  GO TO OPT-END
               WHEN "6"    IF OPT-6  GO TO OPT-END
               WHEN "7"    IF OPT-7  GO TO OPT-END
               WHEN "8"    IF OPT-8  GO TO OPT-END
               WHEN "9"    IF OPT-9  GO TO OPT-END
               WHEN "A"    IF OPT-A  GO TO OPT-END
               WHEN "B"    IF OPT-B  GO TO OPT-END
               WHEN "D"    IF OPT-D  GO TO OPT-END
               WHEN "E"    IF OPT-E  GO TO OPT-END
               WHEN "F"    IF OPT-F  GO TO OPT-END
               WHEN "G"    IF OPT-G  GO TO OPT-END
               WHEN "H"    IF OPT-H  GO TO OPT-END
               WHEN "I"    IF OPT-I  GO TO OPT-END
               WHEN "J"    IF OPT-J  GO TO OPT-END
               WHEN "K"    IF OPT-K  GO TO OPT-END
               WHEN "L"    IF OPT-L  GO TO OPT-END
               WHEN "M"    IF OPT-M  GO TO OPT-END
               WHEN "O"    IF OPT-O  GO TO OPT-END
               WHEN "P"    IF OPT-P  GO TO OPT-END                 
               WHEN "Q"    IF OPT-Q  GO TO OPT-END                 
               WHEN "R"    IF OPT-R  GO TO OPT-END                 
               WHEN "S"    IF OPT-S  GO TO OPT-END
               WHEN "T"    IF OPT-T  GO TO OPT-END
               WHEN "U"    IF OPT-U  GO TO OPT-END
               WHEN "V"    IF OPT-V  GO TO OPT-END
               WHEN "W"    IF OPT-W  GO TO OPT-END
               WHEN "X"    IF OPT-X  GO TO OPT-END
               WHEN "Z"    GO TO OPT-END
               WHEN "a"    IF OPT-61 GO TO OPT-END
               WHEN "b"    IF OPT-62 GO TO OPT-END
               WHEN "c"    IF OPT-63 GO TO OPT-END
               WHEN "d"    IF OPT-64 GO TO OPT-END
               WHEN "e"    IF OPT-65 GO TO OPT-END
               WHEN "f"    IF OPT-66 GO TO OPT-END
               WHEN "g"    IF OPT-67 GO TO OPT-END
               WHEN "h"    IF OPT-68 GO TO OPT-END
               WHEN "i"    IF OPT-69 GO TO OPT-END
               WHEN "j"    IF OPT-6A GO TO OPT-END
               WHEN "k"    IF OPT-6B GO TO OPT-END
               WHEN "l"    IF OPT-6C GO TO OPT-END
               WHEN "m"    IF OPT-6D GO TO OPT-END
               WHEN "n"    IF OPT-6E GO TO OPT-END
               WHEN OTHER  GO TO OPT-END
             END-EVALUATE.
             MOVE L-LINE             TO SLIN.
             GO TO OPT-REPLY.

       OPT-END.
             DISPLAY OPT-LINE AT LINE SLIN.
             PERFORM DISPLAY-THE-CURSOR.
             PERFORM RESTORE-SCREEN.

       OPT-EXIT.
             EXIT.

       CHECK-CORRECT   SECTION.
       CHECK-POS.
             MOVE SPACE              TO WS-OPTION.
             MOVE 54                 TO SCOL.
             PERFORM SAVE-SCREEN.

       CHECK-DISPLAY.
             MOVE SLIN               TO SHADE-ROW.
             MOVE 19                 TO SHADE-COL.
             MOVE 42                 TO SHADE-WIDTH.
             MOVE 3                  TO SHADE-LINES.
             DISPLAY S99 AT LINE SLIN.
             PERFORM SCREEN-SHADOW.
             PERFORM MESSAGE-INST.

       CHECK-REPLY.
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
                                 GO TO CHECK-DISPLAY
                 WHEN DOWN-KEY   PERFORM RESTORE-SCREEN
                                 IF SLIN > 42
                                     MOVE 42    TO SLIN
                                 END-IF
                                 GO TO CHECK-DISPLAY
                 WHEN ENTER-KEY  GO TO CHECK-UPPER
                 WHEN OTHER      PERFORM AA900-ALARM
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

      *      Accept option from position specified in WS-MES-LINE and
      *      WS-MES-COL. Do not display the message in a window.
       GET-INPUT       SECTION.
       GET-POS.
             MOVE WS-MES-COL         TO SCOL.
             
       GET-ACCEPT.
             MOVE ZERO               TO WS-ERROR.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN F9-KEY     IF WS-INSTR = "F" OR "Z"
                                     MOVE X"F9"  TO WS-OPTION
                                     GO TO GET-END
                                 ELSE
                                     GO TO GET-ACCEPT
                                 END-IF

                 WHEN ENTER-KEY  GO TO GET-VALID

                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO GET-ACCEPT
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    IF WS-INSTR = "G" OR "J" OR "T" OR "Z" OR "d" OR "h" OR "j"
                                     MOVE X"EC"  TO WS-OPTION
                                     GO TO GET-END
                                 ELSE
                                     GO TO GET-ACCEPT
                                 END-IF
                                 
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO GET-ACCEPT
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X      TO WS-OPTION.

       GET-VALID.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
             EVALUATE WS-INSTR
               WHEN "0"    IF OPT-0  GO TO GET-END
               WHEN "1"    IF OPT-1  GO TO GET-END
               WHEN "2"    IF OPT-2  GO TO GET-END
               WHEN "3"    IF OPT-3  GO TO GET-END
               WHEN "4"    IF OPT-4  GO TO GET-END
               WHEN "5"    IF OPT-5  GO TO GET-END
               WHEN "6"    IF OPT-6  GO TO GET-END
               WHEN "7"    IF OPT-7  GO TO GET-END
               WHEN "8"    IF OPT-8  GO TO GET-END
               WHEN "9"    IF OPT-9  GO TO GET-END
               WHEN "A"    IF OPT-A  GO TO GET-END
               WHEN "B"    IF OPT-B  GO TO GET-END
               WHEN "D"    IF OPT-D  GO TO GET-END
               WHEN "E"    IF OPT-E  GO TO GET-END
               WHEN "F"    IF OPT-F  GO TO GET-END
               WHEN "G"    IF OPT-G  GO TO GET-END
               WHEN "H"    IF OPT-H  GO TO GET-END
               WHEN "I"    IF OPT-I  GO TO GET-END
               WHEN "J"    IF OPT-J  GO TO GET-END
               WHEN "K"    IF OPT-K  GO TO GET-END
               WHEN "L"    IF OPT-L  GO TO GET-END
               WHEN "M"    IF OPT-M  GO TO GET-END
               WHEN "O"    IF OPT-O  GO TO GET-END
               WHEN "P"    IF OPT-P  GO TO GET-END                 
               WHEN "Q"    IF OPT-Q  GO TO GET-END                 
               WHEN "R"    IF OPT-R  GO TO GET-END                 
               WHEN "S"    IF OPT-S  GO TO GET-END
               WHEN "T"    IF OPT-T  GO TO GET-END
               WHEN "U"    IF OPT-U  GO TO GET-END
               WHEN "V"    IF OPT-V  GO TO GET-END
               WHEN "W"    IF OPT-W  GO TO GET-END
               WHEN "X"    IF OPT-X  GO TO GET-END
               WHEN "Z"    GO TO GET-END
               WHEN "a"    IF OPT-61 GO TO GET-END
               WHEN "b"    IF OPT-62 GO TO GET-END
               WHEN "c"    IF OPT-63 GO TO GET-END
               WHEN "d"    IF OPT-64 GO TO GET-END
               WHEN "e"    IF OPT-65 GO TO GET-END
               WHEN "f"    IF OPT-66 GO TO GET-END
               WHEN "g"    IF OPT-67 GO TO GET-END
               WHEN "h"    IF OPT-68 GO TO GET-END
               WHEN "i"    IF OPT-69 GO TO GET-END
               WHEN "j"    IF OPT-6A GO TO GET-END
               WHEN "k"    IF OPT-6B GO TO GET-END
               WHEN "l"    IF OPT-6C GO TO GET-END
               WHEN "m"    IF OPT-6D GO TO GET-END
               WHEN "n"    IF OPT-6E GO TO GET-END
               WHEN OTHER  GO TO GET-END
             END-EVALUATE.
             GO TO GET-INPUT.

       GET-END.
           IF NOT(WS-OPTION = X"EC" OR X"F9")
               DISPLAY WS-OPTION AT SPOS WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             PERFORM DISPLAY-THE-CURSOR.
             
       GET-INPUT-EXIT.
             EXIT.
