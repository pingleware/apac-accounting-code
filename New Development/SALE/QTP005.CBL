      $set LINKCOUNT"512" GNT"QTP005.GNT"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******     ****      ****    ********   *
      *   **    **     **     **    **   **  **    **  **   **         *
      *   **    **     **     **    **  **    **  **    **  *******    *
      *   **    **     **     *******   **    **  **    **        **   *
      *   **   ***     **     **        **    **  **    **        **   *
      *    ******      **     **         **  **    **  **   **    **   *
      *         **     **     **          ****      ****     ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      * A M E N D   Q U O T A T I O N   H E A D E R   &   R E M A R K S*
      *                                                                *
      *       Version 9.00.82 - April 2016                             *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     QTP005.
       AUTHOR.         J.W.LEMMON (APAC).
       DATE-WRITTEN.   AUGUST 1991.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1991 - 2016
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

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
               CURSOR IS CSTART
                       CONSOLE IS CRT
                       CRT STATUS IS KEY-STATUS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CONTROL.SL.

       COPY PARAM.SL.

      /
       DATA DIVISION.
       FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

       COPY PARAM.FDE.

      /
       WORKING-STORAGE SECTION.
       77  WS-CHECK    PIC X(18)  VALUE
      "aeWlimemnomLalismJ".
       77  WS-S1    PIC 9(04)  COMP-5.
       77  WS-S2           PIC  9(04)    COMP-5.
       77  WS-S3           PIC  9(04)    COMP-5.
       77  WS-S4           PIC  9(04)    COMP-5.
       77  WS-PARKEY    PIC 9(04)  COMP-5.
       77  WS-NETKEY       PIC  9(04)    COMP-5.
       77  WS-PAR          PIC  9(01) VALUE 0.
       77  WS-INO    PIC 9(03) VALUE 0.
       77  WS-LINES        PIC  9(02) VALUE 0.
       77  WS-FIRST    PIC 9(03) VALUE 0.
       77  WS-LAST    PIC 9(03) VALUE 0.
       77  WS-LASTINV      PIC  9(04)    COMP-5.
       77  WS-LINE         PIC  9(04)    COMP-5.
       77  WS-PAGE         PIC  9(02)    COMP-5.
       77  WS-L1           PIC  9(04)    COMP-5.
       77  WS-OPTION    PIC X(01).
       77  WS-IN-OPT       PIC  X(01).
       77  WS-NUM          PIC  Z9.
       77  WS-ERROR        PIC  9(01).
       77  WS-CHAR1        PIC  X(01).
       77  WS-CHAR2        PIC  X(01).
       77  WS-COMP         PIC  9(01).
       77  WS-UPDATE       PIC  9(01) VALUE ZERO.
       77  WS-CONREC       PIC  9(01) VALUE ZERO.
       77  WS-HEAD         PIC  X(01).
       77  WS-TYPE         PIC  X(01).
       77  WS-AGE          PIC  X(01).
       77  WS-PRC          PIC  X(01).
       77  WS-SPACE    PIC X(70) VALUE SPACES.
       77  WS-PER          PIC  Z(02)9.99.
       77  WS-CR           PIC  X(01).
       77  WS-TAXRES       PIC  S9(07)V9(08) COMP-3.
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.
      /
       01  WS-DB-LINE.
           03  WS-TOP-LINE.
               05  WS-TCHAR PIC X(01) OCCURS 80.
           03  WS-TOP-LNE2.
               05  WS-TCR  PIC X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
              05  FILLER      PIC  X(23).
        05  WS-WRKHD    PIC  X(11).
              05  FILLER      PIC  X(01).
        05  WS-WRKST    PIC  X(03).
              05  FILLER      PIC  X(01).
          03  WS-MID-LINE.
               05  WS-MCHAR PIC X(01) OCCURS 80.
    03  WS-MINV REDEFINES WS-MID-LINE.
        05  WS-INV1     PIC  X(01).
        05  WS-INV78    PIC  X(78).
        05  WS-INV80    PIC  X(01).
          03  WS-MID-LNE2.
        05  FILLER      PIC  X(01) VALUE "³".
        05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
        05  FILLER      PIC  X(01) VALUE "³".
          03  WS-BOT-LINE.
               05  WS-BCHAR PIC X(01) OCCURS 80.
           03  WS-BOT-LNE2.
               05  WS-BCR  PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE2.
               05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
                  07  WS-SRT-LINE
                           PIC X(50).
                  07  FILLER
                           PIC X(10).
               05  FILLER  PIC X(19).
           03  WS-MM-LINE.
        05  FILLER  PIC X(01) VALUE X"B3".
        05  FILLER  PIC X(60) VALUE SPACES.
        05  FILLER  PIC X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".
           03  WS-TOP-LNE.
               05  WS-TCHR PIC X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
           03  WS-MID-LNE.
               05  WS-MCHR PIC X(01) OCCURS 80.
    03  WS-MST-LNE REDEFINES WS-MID-LNE.
        05  WS-MST1     PIC  X(01).
        05  WS-MST78    PIC  X(78).
        05  WS-MST80    PIC  X(01).
          03  WS-BOT-LNE.
               05  WS-BCHR PIC X(01) OCCURS 80.
           03  WS-TP-LNE.
               05  WS-TCH  PIC X(01) OCCURS 80.
           03  WS-MD-LNE.
               05  WS-MCH  PIC X(01) OCCURS 80.
    03  WS-MDDT-LNE REDEFINES WS-MD-LNE.
        05  WS-MDT1     PIC  X(01).
        05  WS-MDT78    PIC  X(78).
        05  WS-MDT80    PIC  X(01).
          03  WS-BT-LNE.
               05  WS-BCH  PIC X(01) OCCURS 80.
           03  WS-TP-LINE.
               05  WS-TCA  PIC X(01) OCCURS 80.
           03  WS-MD-LINE.
               05  WS-MCA  PIC X(01) OCCURS 80.
    03  WS-SO78-L REDEFINES WS-MD-LINE.
        05  WS-MSO1     PIC  X(01).
        05  WS-MSO78    PIC  X(78).
        05  WS-MSO80    PIC  X(01).
          03  WS-BT-LINE.
               05  WS-BCA  PIC X(01) OCCURS 80.

       COPY WS.WS.

       01  WS-HELP.
    03  WS-MODULE       PIC  X(03) VALUE "QTP".
    03  WS-PROG        PIC  X(03) VALUE "005".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY PARAM.ID.

       COPY W12.WS.

       01  W30-MESSAGE.
           03  W30-REMARKS OCCURS 4
                               PIC  X(63).

       01  W32-MESSAGE.
    03  W32-REMARKS OCCURS 10
          PIC  X(63).
       01  W34-HEADER.
    03  W34-FORMAT      PIC  X(01).
    03  W34-MARGIN      PIC  9(02) COMP-5.
    03  W34-SALUTATION  PIC  X(63).
    03  W34-SIGNATURE   PIC  X(20).

       COPY W40.WS.

       COPY FUNCTION.WS.

       COPY W50.WS.

       01  W95-STM.
           03  W95-COMP        PIC X(40).
           03  W95-ADD1        PIC X(30).
           03  W95-ADD2        PIC X(30).
           03  W95-ADD3        PIC X(30).
           03  W95-ADD4        PIC X(30).
           03  W95-PC.
               05  W95-POST    PIC 9(04).
           03  W95-TEL         PIC X(14).
           03  W95-V1.
               05  W95-T       PIC Z(04)9.99.
           03  W95-V2.
               05  W95-B       PIC Z(06)9.99-.
           03  W95-D.
               05  W95-DTE     PIC Z9/99/99.
           03  W95-TIME.
               05  W95-HH      PIC 99.
               05  FILLER      PIC X(01) VALUE ":".
               05  W95-MM      PIC 99.
               05  FILLER      PIC X(01) VALUE ":".
               05  W95-SS      PIC 99.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
       SCREEN SECTION.

       COPY BLANK.CRT.

       01  S42.
           03  LINE  2 COLUMN 28 FOREGROUND-COLOR 15
                                 VALUE "AMEND QUOTATION REMARKS".
    03  LINE  4 COLUMN  3 VALUE "Salutation".
    03        COLUMN 14 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(63) FROM WS-SPACE.
    03  LINE  5 COLUMN  3 VALUE "Headers".
    03        COLUMN 14 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(63) FROM WS-SPACE.
    03  LINE  6 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(63) FROM WS-SPACE.
    03  LINE  7 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(63) FROM WS-SPACE.
    03  LINE  8 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(63) FROM WS-SPACE.
    03  LINE  9 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(63) FROM WS-SPACE.
    03  LINE 10 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(63) FROM WS-SPACE.
    03  LINE 11 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(63) FROM WS-SPACE.
    03  LINE 12 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(63) FROM WS-SPACE.
    03  LINE 13 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(63) FROM WS-SPACE.
    03  LINE 14 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(63) FROM WS-SPACE.
    03  LINE 15 COLUMN  3 VALUE "Remarks".
    03        COLUMN 14 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(63) FROM WS-SPACE.
    03  LINE 16 COLUMN 14 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(63) FROM WS-SPACE.
    03  LINE 17 COLUMN 14 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(63) FROM WS-SPACE.
    03  LINE 18 COLUMN 14 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(63) FROM WS-SPACE.
    03  LINE 19 COLUMN  3 VALUE "Greeting".
    03        COLUMN 14 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(20) FROM WS-SPACE.
    03  LINE 20 COLUMN  3 VALUE "Format".
    03        COLUMN 14 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(01) FROM WS-SPACE.
    03        COLUMN 16 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "L".
    03        COLUMN 17 VALUE "etter head or ".
    03        COLUMN 31 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "C".
    03        COLUMN 32 VALUE "onsole paper".
    03  LINE 21 COLUMN  3 VALUE "Top Margin".
    03        COLUMN 14 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(02) FROM WS-SPACE.
    03        COLUMN 17 VALUE "Lines to skip before printing".

       01  S42A.
    03  LINE  4 COLUMN 14 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC X(63) USING W34-SALUTATION AUTO.
    03  LINE  5 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC X(63) USING W32-REMARKS (1) AUTO.
    03  LINE  6 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC X(63) USING W32-REMARKS (2) AUTO.
    03  LINE  7 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC X(63) USING W32-REMARKS (3) AUTO.
    03  LINE  8 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC X(63) USING W32-REMARKS (4) AUTO.
    03  LINE  9 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC X(63) USING W32-REMARKS (5) AUTO.
    03  LINE 10 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC X(63) USING W32-REMARKS (6) AUTO.
    03  LINE 11 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC X(63) USING W32-REMARKS (7) AUTO.
    03  LINE 12 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC X(63) USING W32-REMARKS (8) AUTO.
    03  LINE 13 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC X(63) USING W32-REMARKS (9) AUTO.
    03  LINE 14 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC X(63) USING W32-REMARKS (10) AUTO.
    03  LINE 15 COLUMN 14 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC X(63) USING W30-REMARKS (1) AUTO.
    03  LINE 16 COLUMN 14 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC X(63) USING W30-REMARKS (2) AUTO.
    03  LINE 17 COLUMN 14 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC X(63) USING W30-REMARKS (3) AUTO.
    03  LINE 18 COLUMN 14 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC X(63) USING W30-REMARKS (4) AUTO.
    03  LINE 19 COLUMN 14 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC X(20) USING W34-SIGNATURE AUTO.
    03  LINE 20 COLUMN 14 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC X(01) USING W34-FORMAT AUTO.
    03  LINE 21 COLUMN 14 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC 9(02) USING W34-MARGIN AUTO.

       01  S99.
          03  LINE 25 COLUMN 29 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 1
           VALUE "Correct ".
    03        COLUMN 37 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6 HIGHLIGHT
           VALUE "ENTER".
    03        COLUMN 42 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 1
     VALUE " - ".
    03        COLUMN 45 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "N".
    03        COLUMN 46 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 1
     VALUE " if incorrect".

       01  ERROR-LINE.
    03  LINE 25 COLUMN  2 BACKGROUND-COLOR WS-TEMPBG
     FOREGROUND-COLOR WS-FGRND-5
     HIGHLIGHT
     PIC  X(48) FROM WS-ERR-MES.
      /
       PROCEDURE DIVISION
   USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000-MAIN        SECTION.
       AA000-INIT.
          IF LS0-SALEV < 1
        PERFORM CLEAR-L50
              DISPLAY "Not Authorised - Press any key" AT 2512
         WITH BACKGROUND-COLOR 3
        FOREGROUND-COLOR 6 HIGHLIGHT
        CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS
        GO TO AA49.
            PERFORM ZA000-INIT.
            PERFORM EA000.
       AA49.
             EXIT PROGRAM.


       COPY FUNCTION.CRT.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

      /
      *    ****    READ FILES ROUTINES
      *
       AC000              SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.

      /
       AD000-WRITE             SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

      /
      *  ****  Q U O T A T I O N   R E M A R K S   A M E N D
      *
       EA000         SECTION 56.
       EA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S42.
       EA05.
             ACCEPT S42A.
            DISPLAY S99.
            ACCEPT WS-OPTION AT 2560
      WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
            PERFORM CLEAR-L50.
           IF WS-OPTION = "N"
               GO TO EA05.
             MOVE 266            TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
             MOVE W30-REMARKS (1)
                                 TO PAR-QTE1.
             MOVE W30-REMARKS (2)
                                 TO PAR-QTE2.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             ADD 1               TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
             MOVE W30-REMARKS (3)
                                 TO PAR-QTE1.
             MOVE W30-REMARKS (4)
                                 TO PAR-QTE2.
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      MOVE 275   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
      CALL "CBL_TOUPPER" USING W34-FORMAT
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
      MOVE W34-FORMAT  TO PAR-QTE-FMT.
      MOVE W34-MARGIN  TO PAR-QTE-MRG.
      MOVE W34-SALUTATION TO PAR-QTE-SAL.
      MOVE W34-SIGNATURE  TO PAR-QTE-SIG.
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      ADD 1   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
      MOVE W32-REMARKS (1)
     TO PAR-QHDR1.
      MOVE W32-REMARKS (2)
     TO PAR-QHDR2.
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      ADD 1   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
      MOVE W32-REMARKS (3)
     TO PAR-QHDR1.
      MOVE W32-REMARKS (4)
     TO PAR-QHDR2.
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      ADD 1   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
      MOVE W32-REMARKS (5)
     TO PAR-QHDR1.
      MOVE W32-REMARKS (6)
     TO PAR-QHDR2.
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      ADD 1   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
      MOVE W32-REMARKS (7)
     TO PAR-QHDR1.
      MOVE W32-REMARKS (8)
     TO PAR-QHDR2.
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      ADD 1   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
      MOVE W32-REMARKS (9)
     TO PAR-QHDR1.
      MOVE W32-REMARKS (10)
     TO PAR-QHDR2.
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.

       EA999.
             EXIT.

      /
       ZA000-INIT              SECTION 90.
       ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
      MOVE LS-PARID  TO WS-PARID.
      MOVE LS-L-OR-N  TO W02-L-OR-N.
      MOVE WS-SYS-ID  TO W02-SYSID.
      MOVE LS-TODAY-DDMMYY
     TO TODAY-DDMMYY.
      MOVE LS-USUB  TO WS-USUB.
            PERFORM ERASE-SCREEN.
      *
      *    ****    S E T   U P   T H E   F U N C T I O N   K E Y S
      *
             MOVE 1              TO USER-ACTION
                                    USER-SETTING.
      *
      *    ESC, F1 to F10 keys
      *
             MOVE ZERO           TO USER-NUMBER.
             MOVE 11             TO USER-KEYS.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    PAGE-UP AND PAGE-DOWN KEYS
      *
             MOVE 53             TO USER-NUMBER.
             MOVE 2              TO USER-KEYS.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    A C T I V A T E   M O U S E
      *
             MOVE 64             TO MOUSE-FUNC.
             MOVE 1              TO MOUSE-PARAM.
             CALL X"AF" USING MOUSE-FUNC
                              MOUSE-PARAM.
           IF MOUSE-FUNC NOT = 255
               MOVE "Y"          TO MOUSE-ATTACHED
               MOVE 66           TO MOUSE-FUNC
               MOVE 0            TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC
                                MOUSE-PARAM
           ELSE
               GO TO ZA00A.
      *
      *    ****    S E T   M O U S E   K E Y   T O   A C T
      *            A S   F U N C T I O N   K E Y
      *
             MOVE 3              TO USER-ACTION.
             MOVE 27             TO USER-NUMBER.
             MOVE 2              TO USER-KEYS.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    P A R A M E T E R   F I L E
      *
       ZA00A.
      MOVE "PARAM"  TO AFID-KEY.

       ZA00-READ-APACFIDS.
            READ APACFIDS WITH IGNORE LOCK
        KEY IS AFID-KEY.
    IF WS-STATUS = "00"
        GO TO ZA00-READ-APACFIDS-EXIT.
            STRING "Missing " DELIMITED SIZE
       AFID-KEY DELIMITED BY " "
       " file ID - Status " DELIMITED SIZE
       WS-STATUS DELIMITED SIZE
       INTO WS-ERR-MES.
      PERFORM ERROR-LENGTH THRU ERROR-EXIT.
            STOP RUN.

       ZA00-READ-APACFIDS-EXIT.
      EXIT.

       ZA00A-CONTINUE.
      MOVE AFID-PATH  TO W02-PARAM.
      MOVE "NETWORK"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-NETWORK.

       ZA01B.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
             MOVE PAR-COMPANY    TO W95-COMP.
             MOVE PAR-TELEPHONE  TO W95-TEL.
      MOVE 266   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-QTE1       TO W30-REMARKS (1).
             MOVE PAR-QTE2       TO W30-REMARKS (2).
             ADD 1               TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-QTE1       TO W30-REMARKS (3).
             MOVE PAR-QTE2       TO W30-REMARKS (4).
      MOVE 275   TO WS-PARKEY.
            READ PARAM IGNORE LOCK.
          IF NOT (WS-STATUS = "00")
              GO TO ZA10.
      MOVE PAR-QTE-FMT  TO W34-FORMAT.
      MOVE PAR-QTE-MRG  TO W34-MARGIN.
      MOVE PAR-QTE-SAL  TO W34-SALUTATION.
      MOVE PAR-QTE-SIG  TO W34-SIGNATURE.
      ADD 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-QHDR1  TO W32-REMARKS (1).
      MOVE PAR-QHDR2  TO W32-REMARKS (2).
      ADD 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-QHDR1  TO W32-REMARKS (3).
      MOVE PAR-QHDR2  TO W32-REMARKS (4).
      ADD 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-QHDR1  TO W32-REMARKS (5).
      MOVE PAR-QHDR2  TO W32-REMARKS (6).
      ADD 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-QHDR1  TO W32-REMARKS (7).
      MOVE PAR-QHDR2  TO W32-REMARKS (8).
      ADD 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-QHDR1  TO W32-REMARKS (9).
      MOVE PAR-QHDR2  TO W32-REMARKS (10).
             GO TO ZA999.

       ZA10.
      MOVE "C"   TO PAR-QTE-FMT W34-FORMAT.
      MOVE 6   TO PAR-QTE-MRG W34-MARGIN.
      MOVE "Dear Sir/Madam,"
     TO PAR-QTE-SAL W34-SALUTATION.
      MOVE "Yours sincerely"
     TO PAR-QTE-SIG W34-SIGNATURE.
            WRITE PAR-RECORD1.
      ADD 1   TO WS-PARKEY.
      MOVE SPACES  TO PAR-QHDR1 W32-REMARKS (1).
      MOVE SPACES  TO PAR-QHDR2 W32-REMARKS (2).
            WRITE PAR-RECORD1.
      ADD 1   TO WS-PARKEY.
      MOVE SPACES  TO PAR-QHDR1 W32-REMARKS (3).
      MOVE SPACES  TO PAR-QHDR2 W32-REMARKS (4).
            WRITE PAR-RECORD1.
      ADD 1   TO WS-PARKEY.
      MOVE SPACES  TO PAR-QHDR1 W32-REMARKS (5).
      MOVE SPACES  TO PAR-QHDR2 W32-REMARKS (6).
            WRITE PAR-RECORD1.
      ADD 1   TO WS-PARKEY.
      MOVE SPACES  TO PAR-QHDR1 W32-REMARKS (7).
      MOVE SPACES  TO PAR-QHDR2 W32-REMARKS (8).
            WRITE PAR-RECORD1.
      ADD 1   TO WS-PARKEY.
      MOVE SPACES  TO PAR-QHDR1 W32-REMARKS (9).
      MOVE SPACES  TO PAR-QHDR2 W32-REMARKS (10).
            WRITE PAR-RECORD1.
             GO TO ZA999.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
             MOVE 1 TO WS-S1.
             MOVE SPACES         TO WS-MID-LINE WS-MID-LNE
        WS-MD-LINE
                                    WS-MD-LNE.
       ZA60.
             MOVE WS-G1          TO WS-TCHAR (WS-S1) WS-BCHAR (WS-S1).
             MOVE WS-G1          TO WS-TCHR (WS-S1)  WS-BCHR (WS-S1).
             MOVE WS-G1          TO WS-TCH (WS-S1)   WS-BCH (WS-S1).
             MOVE WS-G1          TO WS-TCA (WS-S1)   WS-BCA (WS-S1).
             MOVE WS-G8          TO WS-TCR (WS-S1)   WS-BCR (WS-S1).
           IF WS-S1 < 80
               ADD 1 TO WS-S1
               GO TO ZA60.
             MOVE WS-G9          TO WS-TCR   (1).
             MOVE WS-G10         TO WS-TCR   (80).
             MOVE WS-G11         TO WS-BCR   (1).
             MOVE WS-G12         TO WS-BCR   (80).
             MOVE WS-G14         TO WS-TCHAR (1)  WS-BCHAR (1)
                                    WS-TCHR  (1)  WS-BCHR  (1)
                                    WS-TCA   (1)  WS-BCA   (1)
                                    WS-TCH   (1)  WS-BCH   (1).
             MOVE WS-G13         TO WS-TCHAR (80) WS-BCHAR (80)
                                    WS-TCHR  (80) WS-BCHR  (80)
                                    WS-TCA   (80) WS-BCA   (80)
                                    WS-TCH   (80) WS-BCH   (80).
             MOVE WS-G2          TO WS-TCHAR (4)  WS-TCHAR (19)
                                    WS-TCHAR (50) WS-TCHAR (59)
                                    WS-TCHAR (69)
                                    WS-TCHR  (18) WS-TCHR  (49)
                                    WS-TCHR  (60) WS-TCHR  (70)
                                    WS-TCA   (10) WS-TCA   (17)
                                    WS-TCA   (47) WS-TCA   (56)
                                    WS-TCA   (59) WS-TCA   (68)
                                    WS-TCH   (8)  WS-TCH   (49)
                                    WS-TCH   (61) WS-TCH   (72).
             MOVE WS-G3          TO WS-MCHAR (4)  WS-MCHAR (19)
                                    WS-MCHAR (50) WS-MCHAR (59)
                                    WS-MCHAR (69) WS-MCHAR (80)
                                    WS-MCHAR (1)
                                    WS-MCH   (8)  WS-MCH   (49)
                                    WS-MCH   (61) WS-MCH   (72)
                                    WS-MCA   (1)  WS-MCA   (80)
                                    WS-MCA   (10) WS-MCA   (17)
                                    WS-MCA   (47) WS-MCA   (56)
              WS-MCA   (59) WS-MCA   (68)
                                    WS-MCHR  (18) WS-MCHR  (49)
                                    WS-MCHR  (60) WS-MCHR  (70)
                                    WS-MCHR  (1)  WS-MCHR  (80)
                                    WS-MCH   (1)  WS-MCH   (80).
             MOVE WS-G4          TO WS-BCHAR (4)  WS-BCHAR (19)
                                    WS-BCHAR (50) WS-BCHAR (59)
                                    WS-BCHAR (69)
                                    WS-BCHR  (18) WS-BCHR  (49)
                                    WS-BCHR  (60) WS-BCHR  (70)
                                    WS-BCA   (10) WS-BCA   (17)
                                    WS-BCA   (47) WS-BCA   (56)
                                    WS-BCA   (59) WS-BCA   (68)
                                    WS-BCH   (8)  WS-BCH   (49)
                                    WS-BCH   (61) WS-BCH   (72).
      MOVE LS-COMPANY  TO WS-TOP-COMP.
    IF LS-USER = LS-SYS-ID
        MOVE "SupervisorÄ"  TO WS-WRKHD
    ELSE
        MOVE "Workstation"  TO WS-WRKHD
        MOVE LS-USER    TO WS-WRKST.
       ZA200.
             DISPLAY "Files locked - Try later" AT 2312
                      WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
             DISPLAY "Press any key" AT 2512
              WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
            ACCEPT WS-OPTION AT 2526
      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
       ZA999.
             EXIT.

       I-O-ERRORS      SECTION  91.
       OPEN-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
             PERFORM DISPLAY-FILE-NAME.
      
       READ-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
             PERFORM DISPLAY-FILE-NAME.
      
       WRITE-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
             PERFORM DISPLAY-FILE-NAME.
      

       DISPLAY-FILE-NAME.
           IF WS-F-ERROR = 2
               MOVE W02-NETWORK TO WS-FILE
               MOVE WS-NETKEY TO WS-KEY
          ELSE
           IF WS-F-ERROR = 15
               MOVE WS-PARID     TO WS-FILE
              MOVE WS-PARKEY  TO WS-KEY.
           IF WS-STATUS = "10"
               MOVE "End of FILE" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "22"
               MOVE "Duplicate record number" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "23"
               MOVE "Invalid record number" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "24"
               MOVE "DISK full" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "30"
               MOVE "DISK error" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "94"
               MOVE "FILE locked" TO WS-STAT-MESSAGE.
             DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
             DISPLAY "Status " AT 1212
                      WS-STATUS WITH FOREGROUND-COLOR 11
                     ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
           IF WS-STATUS NOT = "94"
               DISPLAY "Key    " AT 1412
                        WS-KEY WITH FOREGROUND-COLOR 11
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
               DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
               DISPLAY "Please make a note of these details" AT 1812.
             STOP RUN.
