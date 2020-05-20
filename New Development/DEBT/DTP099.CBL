      $SET LINKCOUNT"256" GNT"DTP099.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****     ******    ******    *
      *   **    **     **     **    **   **  **   **    **  **    **   *
      *   **    **     **     **    **  **    **  **    **  **    **   *
      *   **    **     **     *******   **    **   *******   *******   *
      *   **    **     **     **        **    **        **        **   *
      *   **    **     **     **         **  **   **    **  **    **   *
      *   *******      **     **          ****     ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *          D E B T O R   B A L A N C E   T A K E - O N           *
      *                                                                *
      *       Version 9.00.70 - March 2015                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  May 2006 - New  field included in Accounts Receivable  for    *
      *             recording number of outstanding Jobs linked to the *
      *             account. Additional  filler added  for  future     *
      *             expansion.                                         *
      *                                                                *
      *  Jan 2008 - New file (DEBALT)- Debtor alternate index included *
      *             for lookups, using any word contained in the       *
      *             Debtor name.                                       *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     DTP099.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   AUGUST 1984.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1984 - 2016
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

       COPY "CONTROL.SL".

       COPY "PARAM.SL".

       COPY "DBTRAN.SL".

       COPY "DEBTOR.SL".

       COPY "DEBMEM.SL".

       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FD".

       COPY "PARAM.FD".

       COPY "DBTRAN.FD".

       COPY "DEBTOR.FD".

       COPY "DEBMEM.FD".

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   **
      *         **    *    **   **    **   **   **    **  **
      *         **   ***   **   **    **   ******     *****
      *          ** ** ** **    **    **   **   **    **  **
      *           ***   ***     **    **   **    **   **   **
      *            *     *       ******    **    **   **    **
      *
       WORKING-STORAGE SECTION.
       77  WS-CHECK                PIC  X(18)    VALUE "aeWlimemnomLalismJ".
       77  WS-S1    PIC 9(04)  COMP-5.
       77  WS-S2           PIC 9(04)     COMP-5.
       77  WS-S3    PIC 9(04)  COMP-5.
       77  WS-S4    PIC 9(04)  COMP-5.
       77  WS-IXS1         PIC 9(04)     COMP-5.
       77  WS-PARKEY       PIC 9(04)     COMP-5.
       77  WS-NETKEY       PIC 9(04)     COMP-5.
       77  WS-OPTION    PIC X(01).
       77  WS-TAKEON       PIC X(01) VALUE "A".
       77  WS-INDD         PIC 9(01).
       77  WS-ERROR        PIC 9(01).
       77  WS-PASSW    PIC X(08).
       77  WS-ERR1    PIC X(22)   VALUE
                          "Invalid account number".
       77  WS-ERR2         PIC X(09)      VALUE "No record".
       77  PRG-DEBT-LUP            PIC  X(12)    VALUE "DTP\DTPLOOK".
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *      C = Clear comment line (50)
      *      E = Clear lines (any line or lines between 5 and 46)
      *       H = Change heading
      *       S = Clear the screen and display headings
      *
           03  CRT-TYPE            PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START           PIC  9(02).
           03  CRT-END             PIC  9(02).
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15) VALUE "DTP099".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "ACCOUNTS RECEIVABLE - BALANCE TAKE-ON".
      *
      *    Company Name
      *
           03  CRT-COMPANY         PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-WRKHD           PIC  X(11).
           03  CRT-WRKST           PIC  X(03).

       01  WS-DB-LINE.
           03  WS-TOP-LNE.
               05  WS-TCHR PIC X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
           03  WS-TOP-LNE2.
               05  WS-TCH  PIC X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
              05  FILLER      PIC  X(23).
        05  WS-WRKHD    PIC  X(11).
              05  FILLER      PIC  X(01).
        05  WS-WRKST    PIC  X(03).
              05  FILLER      PIC  X(01).
          03  WS-MID-LNE.
               05  WS-MCHR PIC X(01) OCCURS 80.
    03  WS-MDDT-LNE REDEFINES WS-MID-LNE.
        05  WS-MDT1     PIC  X(01).
        05  WS-MDT78    PIC  X(78).
        05  WS-MDT80    PIC  X(01).
          03  WS-MID-LNE2.
        05  FILLER      PIC  X(01) VALUE "³".
        05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
        05  FILLER      PIC  X(01) VALUE "³".
          03  WS-BOT-LNE.
               05  WS-BCHR PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC X(01).
               05  WS-F-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
           03  WS-BOT-LNE2.
               05  WS-BCH  PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE2.
               05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
                  07  WS-SRT-LINE
                           PIC X(50).
                  07  FILLER
                           PIC X(10).
               05  FILLER  PIC X(19).
           03  WS-MM-LINE.
               05  FILLER  PIC  X(01) VALUE X"B3".
               05  FILLER  PIC  X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

       COPY "WS.WS".

       01  WS-HELP.
    03  WS-MODULE       PIC  X(03) VALUE "DTP".
    03  WS-PROG        PIC  X(03) VALUE "001".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W10-SPARES.
           03  W10-DATE       PIC X(08).

      *
      *    Date routines for accepting checking and formatting have
      *    been removed from each program and the DateCheck program
      *    will be called to handle these routines.
      *
      *    The following copy 'DateVariables' now includes the 'W12.WS'
      *    and 'W20.WS' variables and is passed to DateCheck in the CALL
      *    statement.
      *
       COPY DateVariables.

       01  W25-CALCS.
           03  W25-TOTAL      PIC S9(09)V99 COMP-3.

       COPY "W40.DBT".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       01  W70-LINKS.
           03  W70-OPOUT      PIC S9(09)V99 COMP-3.
           03  W70-OPDUE      PIC S9(09)V99 COMP-3.
           03  W70-OPCUR      PIC S9(09)V99 COMP-3.
           03  W70-OP30       PIC S9(09)V99 COMP-3.
           03  W70-OP60       PIC S9(09)V99 COMP-3.
           03  W70-OP90       PIC S9(09)V99 COMP-3.
           03  W70-OP120      PIC S9(09)V99 COMP-3.
           03  W70-OPINT      PIC S9(09)V99 COMP-3.
       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-DNO        PIC X(06) OCCURS 18.
       01  W80-ACCOUNT.
           03  W80-ACNO       PIC X(06).
           03  W80-NUMD REDEFINES W80-ACNO.
               05  W80-SERIAL PIC 9(06).
           03  W80-TYPE       PIC 9(01).
       01  W90-TRAN.
           03  W90-120        PIC S9(09)V99 COMP-3.
           03  W90-90         PIC S9(09)V99 COMP-3.
           03  W90-60         PIC S9(09)V99 COMP-3.
           03  W90-30         PIC S9(09)V99 COMP-3.
           03  W90-CUR        PIC S9(09)V99 COMP-3.
           03  W90-BAL        PIC S9(09)V99 COMP-3.
           03  W90-MTD        PIC S9(09)V99 COMP-3.
           03  W90-INT        PIC S9(07)V99 COMP-3.
       01  W100-EDIT.
           03  W100-LBAL       PIC Z(06)9.99-.
           03  W100-LARR       PIC Z(06)9.99.
           03  W100-BAL        PIC Z(08)9.99-.
           03  W100-CUR        PIC Z(08)9.99-.
           03  W100-30         PIC Z(08)9.99-.
           03  W100-60         PIC Z(08)9.99-.
           03  W100-90         PIC Z(08)9.99-.
           03  W100-120        PIC Z(08)9.99-.
           03  W100-INT        PIC Z(08)9.99.
           03  W100-CRL        PIC Z(06)9.
           03  W100-KEY        PIC Z(05)9.
       01  W200-MEMO.
           03  W200-MEM-FLDS.
               05  W200-MEM1   PIC  X(60).
               05  W200-MEM2   PIC  X(60).
               05  W200-MEM3   PIC  X(60).
               05  W200-MEM4   PIC  X(60).
               05  W200-MEM5   PIC  X(60).
               05  W200-MEM6   PIC  X(60).
               05  W200-MEM7   PIC  X(60).
               05  W200-MEM8   PIC  X(60).
               05  W200-MEM9   PIC  X(60).
               05  W200-MEM10  PIC  X(60).
               05  W200-MEM11  PIC  X(60).
               05  W200-MEM12  PIC  X(60).
           03  W200-MEM-FOC REDEFINES W200-MEM-FLDS.
               05  W200-MEM    PIC  X(60) OCCURS 12.

       LINKAGE SECTION.

       COPY "CHAIN.LS".

       COPY "FILE.IDS".

      /
       SCREEN SECTION.

       COPY "DEBLUP.CRT".

       01  S026-TRANS.
          03  LINE 2  COLUMN 33 FOREGROUND-COLOR Grey HIGHLIGHT
           VALUE "DEBTOR BALANCE TAKE-ON".
           03  LINE 4  COLUMN 2  VALUE "Account No:".
           03  LINE 5  COLUMN 2  VALUE "Name:".
          03  LINE 6  COLUMN 2  VALUE "Balance ".
          03  LINE 7  COLUMN 2  VALUE "M.T.D.  ".
          03  LINE 8  COLUMN 2  VALUE " 30 Days".
          03  LINE 9  COLUMN 2  VALUE " 60 Days".
          03  LINE 10 COLUMN 2  VALUE " 90 Days".
          03  LINE 11 COLUMN 2  VALUE "120 Days".
          03  LINE 12 COLUMN 2  VALUE "Interest".

       01  S28.
    03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC X(06) USING W80-ACNO AUTO.

       01  S030-TAKE-ON.
          03  LINE  6 COLUMN 12 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta PIC Z(08)9.99-
                                 USING W90-BAL AUTO.
          03  LINE  7 COLUMN 12 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta PIC Z(08)9.99-
                                 USING W90-CUR AUTO.
          03  LINE  8 COLUMN 12 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta PIC Z(08)9.99-
                                 USING W90-30 AUTO.
          03  LINE  9 COLUMN 12 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta PIC Z(08)9.99-
                                 USING W90-60 AUTO.
          03  LINE 10 COLUMN 12 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta PIC Z(08)9.99-
                                 USING W90-90 AUTO.
          03  LINE 11 COLUMN 12 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta PIC Z(08)9.99-
                                 USING W90-120 AUTO.
          03  LINE 12 COLUMN 16 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta PIC Z(04)9.99-
                                 USING W90-INT AUTO.
       01  S-DEB-MEM.
           03  LINE  7 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey VALUE
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E
      -    "".
       01  S-MEM.
           03  LINE  6 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G9.
           03          COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G10.
           03  LINE  7 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G3.
           03          COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey VALUE
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E
      -    "".
           03          COLUMN 78 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G3.
           03  LINE  8 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G14.
           03          COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING WS-HME-LINE.
           03          COLUMN 78 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G13.
           03  LINE  9 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 10 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 11 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 12 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 13 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 14 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 15 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 16 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 17 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 18 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 19 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 20 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 21 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G11.
           03          COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G12.
       01  SA-MEM.
           03  LINE  9 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM1 AUTO.
           03  LINE 10 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM2 AUTO.
           03  LINE 11 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM3 AUTO.
           03  LINE 12 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM4 AUTO.
           03  LINE 13 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM5 AUTO.
           03  LINE 14 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM6 AUTO.
           03  LINE 15 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM7 AUTO.
           03  LINE 16 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM8 AUTO.
           03  LINE 17 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM9 AUTO.
           03  LINE 18 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM10 AUTO.
           03  LINE 19 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM11 AUTO.
           03  LINE 20 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM12 AUTO.

      /
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS.
       AA000-MAIN        SECTION.
       AA000-INIT.
           IF LS0-DBLEV < 9
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             GO TO CA000-OPTIONS.

       AA49.
      EXIT PROGRAM.

       AA50.
      PERFORM SAVE-SCREEN.
            MOVE 4   TO W44-FUNCTION.
            PERFORM SCREEN-CONTENTS.
      PERFORM DEBTOR-LOOKUP.
      PERFORM RESTORE-SCREEN.
       AA99.
             EXIT.

       COPY "FUNCTION.CRT".

       COPY "LOCKED.REC".

       AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "DEBTOR.RD".

       COPY "DEBMEM.RD".

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

       COPY "DEBTOR.WR".

       COPY "DEBTOR.LUP".

       COPY "APAC.HLP".

       AZ000-END        SECTION.
       AZ000-EOJ.
             CLOSE PARAM
                   DEBTOR
                   DEBMEM
                   NETWORK.
       AZ05.
             EXIT PROGRAM.
      /
       CA000-DEBTOR            SECTION 2.
       CA000-OPTIONS.
             PERFORM CE000-GET-PASSW.
           IF WS-ERROR = 1
                GO TO AZ000-EOJ.
             PERFORM CG000-GET-BAL.
             GO TO AZ000-EOJ.

       CA155-GET-DEBTOR        SECTION 2.
       CA155-ACCOUNT.
             MOVE W80-ACNO       TO DEB-ACNO.
             PERFORM READ-DEBTOR-LOCK THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               MOVE 1            TO WS-ERROR
           ELSE
           IF DEB-ACNO NOT = W80-ACNO
               MOVE 2            TO WS-ERROR
           ELSE
               MOVE 0            TO WS-ERROR.
       CA160-EXIT.
             EXIT.

       CE000-GET-PASSW          SECTION 2.
       CE00.
             MOVE 3              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE ZERO           TO WS-ERROR.
             DISPLAY "Password :" AT 0812.
            ACCEPT WS-PASSW AT 0824
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta NO-ECHO AUTO-SKIP.
            MOVE 8   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING WS-PASSW
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
            MOVE 1   TO WS-LENGTH.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO CE00.
          IF NOT (WS-PASSW = PAR-PASSWORD OR "DBALANCE")
               MOVE 1            TO WS-ERROR
               GO TO CE999.

       CE10.
             DISPLAY "Change password (" AT 1007
                     "Y" WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                     "/" "N" WITH FOREGROUND-COLOR Cyan HIGHLIGHT ")".
             MOVE "N"            TO WS-OPTION.

       CE15.
            ACCEPT WS-OPTION AT 1032
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-OPTION = "Y" OR "N")
               GO TO CE15.
            DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
           IF WS-OPTION = "N"
               GO TO CE25.

       CE20-CHANGE.
             DISPLAY "Password :" AT 1212.
            ACCEPT WS-PASSW AT 1224
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta.
      MOVE 8   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING WS-PASSW
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE  1   TO WS-LENGTH.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO CE20-CHANGE.
            DISPLAY WS-BLNK78 AT 1202 WITH FOREGROUND-COLOR Cyan.
            MOVE 3   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
            MOVE WS-PASSW  TO PAR-PASSWORD.
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.

       CE25.
             DISPLAY "R" AT 1212 WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     "eplace, " "A" WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     "dd to current balances".

       CE30.
             ACCEPT WS-TAKEON AT 1245
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-TAKEON
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-TAKEON = "R" OR "A")
               GO TO CE30.

       CE999.
             EXIT.

       CG000-GET-BAL          SECTION 2.
       CG00.
             PERFORM ERASE-SCREEN.
             DISPLAY S026-TRANS.
             MOVE SPACES         TO W80-ACNO.

       CG15.
      DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "=Help,"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "F2" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       " to do Debtor Lookup "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "Esc" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       " to exit"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             ACCEPT S28 AT 0414.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W80-ACNO
       GO TO CG999
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
             PERFORM AA50
   WHEN OTHER
       CALL X"E5"
       GO TO CG15
        END-EVALUATE
        DISPLAY S28 AT 0414
               IF W80-ACNO = SPACES
                   GO TO CG15.
      PERFORM CLEAR-L50.
           IF W80-ACNO = SPACES
             GO TO CG65.
            DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
             PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR = 1
               DISPLAY WS-ERR2 AT 2312 WITH FOREGROUND-COLOR Brown HIGHLIGHT
               GO TO CG15.
           IF WS-ERROR = 2
               DISPLAY WS-ERR1 AT 2312 WITH FOREGROUND-COLOR Brown HIGHLIGHT
               GO TO CG15.
    IF DEB-OI = "O"
        MOVE "Open Item - May not use balance take-on"
     TO WS-ERR-MES
              PERFORM ERROR-LENGTH THRU ERROR-EXIT
        GO TO CG15.
            DISPLAY DEB-NAME AT 0514
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO CG00.
           IF WS-TAKEON = "R"
               IF DEB-BAL NOT = DEB-STM
                   DISPLAY "May not change balances - 'ENTER'" AT 2312
                            WITH FOREGROUND-COLOR Brown HIGHLIGHT
                   ACCEPT WS-OPTION AT 2346
     WITH FOREGROUND-COLOR Grey HIGHLIGHT
          BACKGROUND-COLOR Magenta AUTO
                   GO TO CG00
               ELSE
                   MOVE DEB-BAL  TO W90-BAL
                   MOVE DEB-CUR  TO W90-CUR
                   MOVE DEB-30   TO W90-30
                   MOVE DEB-60   TO W90-60
                   MOVE DEB-90   TO W90-90
                   MOVE DEB-120  TO W90-120
                   MOVE DEB-INT  TO W90-INT
           ELSE
               MOVE ZERO         TO W90-BAL
                                    W90-CUR
                                    W90-30
                                    W90-60
                                    W90-90
                                    W90-120
                                    W90-INT.
      DISPLAY "Press " AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "Esc" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       " to CANCEL"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.

       CG25.
             ACCEPT S030-TAKE-ON.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO CG00
   WHEN OTHER
       CALL X"E5"
       GO TO CG25
        END-EVALUATE.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO CG25.
            MOVE ZERO TO WS-ERROR.
             COMPUTE W25-TOTAL = W90-CUR + W90-30  + W90-60 +
                                  W90-90  + W90-120 + W90-INT.
           IF W25-TOTAL NOT = W90-BAL
               GO TO CG25.

             MOVE 1              TO WS-NETKEY.
             PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
           IF WS-TAKEON = "R"
               SUBTRACT DEB-BAL  FROM DEB-OPEN
               SUBTRACT DEB-STM  FROM DEB-OUT
               SUBTRACT DEB-CUR  FROM DEB-MTD
               SUBTRACT DEB-30   FROM DEB-ONE
               SUBTRACT DEB-60   FROM DEB-TWO
               SUBTRACT DEB-90   FROM DEB-THREE
               SUBTRACT DEB-120  FROM DEB-FOUR
               SUBTRACT DEB-INT  FROM DEB-INTEREST.
             ADD W90-BAL         TO DEB-OPEN.
             ADD W90-BAL         TO DEB-OUT.
             ADD W90-CUR         TO DEB-MTD.
             ADD W90-30          TO DEB-ONE.
             ADD W90-60          TO DEB-TWO.
             ADD W90-90          TO DEB-THREE.
             ADD W90-120         TO DEB-FOUR.
             ADD W90-INT         TO DEB-INTEREST.
       CG35.
           IF WS-TAKEON = "R"
               MOVE W90-BAL      TO DEB-BAL DEB-STM
               MOVE W90-CUR      TO DEB-CUR
               MOVE W90-30       TO DEB-30
               MOVE W90-60       TO DEB-60
               MOVE W90-90       TO DEB-90
               MOVE W90-120      TO DEB-120
               MOVE W90-INT      TO DEB-INT
           ELSE
               ADD W90-BAL       TO DEB-BAL DEB-STM
               ADD W90-CUR       TO DEB-CUR
               ADD W90-30        TO DEB-30
               ADD W90-60        TO DEB-60
               ADD W90-90        TO DEB-90
               ADD W90-120       TO DEB-120
               ADD W90-INT       TO DEB-INT.
             PERFORM REWRITE-DEBTOR-UNLOCK THRU WRITE-DEBTOR-EXIT.
             PERFORM REWRITE-CONTROL-UNLOCK THRU WRITE-CONTROL-EXIT.
             GO TO CG00.
       CG65.
       CG999.
             EXIT.

       ZA000-INIT              SECTION 2.
       ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
      MOVE LS-PARID  TO WS-PARID.
      MOVE LS-L-OR-N  TO W02-L-OR-N.
      MOVE WS-SYS-ID  TO W02-SYSID.
      MOVE LS-TODAY-DDMMYY
     TO TODAY-DDMMYY.
      MOVE LS-USUB  TO WS-USUB.
            PERFORM ERASE-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

            OPEN I-O PARAM.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS NOT = "00"
               MOVE 15           TO WS-ERROR
               PERFORM OPEN-ERROR.
      OPEN I-O NETWORK.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS NOT = "00"
               MOVE 2            TO WS-F-ERROR
               PERFORM OPEN-ERROR.
             OPEN I-O DEBTOR.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 6        TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
               MOVE 6            TO WS-F-ERROR
               PERFORM OPEN-ERROR.
               OPEN I-O DEBMEM.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 27       TO WS-F-ERROR
                   GO TO ZA50.
             MOVE 1              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
             GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             GO TO ZA205.

       ZA50.
           IF WS-F-ERROR = 6
               MOVE "DEBTOR file"
                                 TO WS-FILE
           ELSE
           IF WS-F-ERROR = 27
               MOVE "DEBTOR MEMO"
                                 TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Press any key to continue" AT 1012.
             ACCEPT WS-OPTION AT 1038 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
       ZA51.
             EXIT PROGRAM.
       ZA55.
             MOVE 1              TO WS-S1.
            MOVE SPACES  TO WS-MID-LNE.
       ZA60.
             MOVE WS-G1          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8          TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
           IF WS-S1 < 80
               ADD 1             TO WS-S1
               GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G2          TO WS-TCHR (8)  WS-TCHR (49)
                                    WS-TCHR (61) WS-TCHR (72).
             MOVE WS-G3          TO WS-MCHR (8)  WS-MCHR (49)
                                    WS-MCHR (61) WS-MCHR (72)
              WS-MCHR (1)  WS-MCHR (80).
             MOVE WS-G4          TO WS-BCHR (8)  WS-BCHR (49)
                                    WS-BCHR (61) WS-BCHR (72).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
      MOVE LS-COMPANY  TO WS-TOP-COMP.
    IF LS-USER = LS-SYS-ID
        MOVE "SupervisorÄ"  TO WS-WRKHD
    ELSE
        MOVE "Workstation"  TO WS-WRKHD
        MOVE LS-USER    TO WS-WRKST.
       ZA200.
             DISPLAY "Files locked - Try later" AT 2312
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     " " WS-STATUS WITH FOREGROUND-COLOR Grey HIGHLIGHT.
            DISPLAY "Press any key" AT 5012
              WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT WS-OPTION AT 5026
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
       ZA205.
             EXIT PROGRAM.
       ZA999-EXIT.
             EXIT.
      /
       ZB000-ERROR           SECTION 2.

       OPEN-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             PERFORM DISPLAY-FILE-NAME.

       READ-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             PERFORM DISPLAY-FILE-NAME.

       WRITE-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             PERFORM DISPLAY-FILE-NAME.

       DISPLAY-FILE-NAME.
           IF WS-F-ERROR = 2
               MOVE W02-NETWORK  TO WS-FILE
               MOVE WS-NETKEY    TO WS-KEY
           ELSE
           IF WS-F-ERROR = 6
              MOVE W02-DEBTOR TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
          ELSE
           IF WS-F-ERROR = 15
               MOVE WS-PARID     TO WS-FILE
               MOVE WS-PARKEY    TO WS-KEY
           ELSE
           IF WS-F-ERROR = 27
              MOVE W02-DEBMEM  TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE DME-KEY  TO WS-KEYX.
           IF WS-STATUS = "10"
               MOVE "End of FILE"
                                 TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "22"
               MOVE "Duplicate record number"
                                 TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "23"
               MOVE "Invalid record number"
                                 TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "24"
               MOVE "DISK full"  TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "30"
               MOVE "DISK error" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "94"
               MOVE "FILE locked"
                                 TO WS-STAT-MESSAGE.
             DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY "Status " AT 1212
                      WS-STATUS WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                     ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
           IF WS-STATUS NOT = "94"
               DISPLAY "Key    " AT 1412
                        WS-KEY WITH FOREGROUND-COLOR Cyan HIGHLIGHT
               DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
               DISPLAY "Please make a note of these details" AT 1812.
             STOP RUN.
