      $SET LINKCOUNT"512" GNT"DTP095.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****     ******   ********   *
      *   **    **     **     **    **   **  **   **    **  **         *
      *   **    **     **     **    **  **    **  **    **  *******    *
      *   **    **     **     *******   **    **   *******        **   *
      *   **    **     **     **        **    **        **        **   *
      *   **    **     **     **         **  **   **    **  **    **   *
      *   *******      **     **          ****     ******    ******    *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *       D E B T O R   R E S E T   L A B E L   F L A G            *
      *                                                                *
      *       Version 9.00.70 - March 2015                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  May 2006 - New field included in Accounts Receivable          *
      *             for recording number of outstanding Jobs           *
      *             linked to the account. Additional filler           *
      *             added for future expansion.                        *
      *                                                                *
      *  Jan 2008 - New file (DEBALT)- Debtor alternate index included *
      *             for lookups, using any word contained in the       *
      *             Debtor name.                                       *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     DTP095.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   OCTOBER 1982.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1982 - 2016
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

       COPY "DEBTOR.SL".

       COPY "PARAM.SL".

       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "DEBTOR.FDE".

       COPY "PARAM.FDE".

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
       77  WS-SUB          PIC  9(04)  COMP-5.
       77  WS-S            PIC  9(04)    COMP-5.
       77  WS-S1           PIC  9(04)    COMP-5.
       77  WS-S2           PIC  9(04)    COMP-5.
       77  WS-S3           PIC  9(04)    COMP-5.
       77  WS-S4           PIC  9(04)    COMP-5.
       77  WS-S5           PIC  9(04)    COMP-5.
       77  WS-S6           PIC  9(04)    COMP-5.
       77  WS-S7           PIC  9(04)    COMP-5.
       77  WS-S8           PIC  9(04)    COMP-5.
       77  WS-S9           PIC  9(04)    COMP-5.
       77  WS-IXS1         PIC  9(04)    COMP-5.
       77  WS-IXS2         PIC  9(04)    COMP-5.
       77  WS-IXS3         PIC  9(04)    COMP-5.
       77  WS-IXS4         PIC  9(04)    COMP-5.
       77  WS-NETKEY       PIC  9(06)    COMP-5.
       77  WS-PARKEY       PIC  9(06)    COMP-5.
       77  WS-NTP          PIC  X(01).
       77  WS-KEYSTR    PIC  9(06)  COMP-5.
       77  WS-OPTION       PIC  X(01).
       77  WS-LABEL    PIC  X(01).
       77  WS-ERROR        PIC  9(01) VALUE ZERO.
       77  WS-EKEY         PIC  9(06)    COMP-5.
       77  WS-CHAR1        PIC  X(01).
       77  WS-CHAR2        PIC  X(01).
       77  WS-COMP         PIC  9(01).
       77  WS-ERR1         PIC  X(22) VALUE "Invalid Account Number".
       77  WS-ERR2         PIC  X(09) VALUE "No Record".
       77  WS-A            PIC  X(19) VALUE "Any key to continue".
       77  TODAY-DDMMYY    PIC  9(08) COMP-5.
       77  WS-USUB    PIC  9(04) COMP-5.

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
           03  CRT-PROGRAM         PIC  X(15) VALUE "DTP095".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "ACCOUNTS RECEIVABLE - REST LABEL FLAG".
      *
      *    Company Name
      *
           03  CRT-COMPANY         PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-WRKHD           PIC  X(11).
           03  CRT-WRKST           PIC  X(03).

      /
       01  WS-DB-LINE.
           03  WS-TOP-LNE.
               05  WS-TCHR         PIC X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER          PIC X(01).
               05  WS-H-LINE       PIC X(78).
               05  FILLER          PIC X(01).
           03  WS-TOP-LNE2.
               05  WS-TCH          PIC X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER          PIC  X(01).
               05  WS-TOP-COMP     PIC  X(40).
               05  FILLER          PIC  X(23).
               05  WS-WRKHD        PIC  X(11).
               05  FILLER          PIC  X(01).
               05  WS-WRKST        PIC  X(03).
               05  FILLER          PIC  X(01).
           03  WS-MID-LNE.
               05  WS-MCHR         PIC X(01) OCCURS 80.
           03  WS-MDDT-LNE REDEFINES WS-MID-LNE.
               05  WS-MDT1         PIC  X(01).
               05  WS-MDT78        PIC  X(78).
               05  WS-MDT80        PIC  X(01).
           03  WS-MID-LNE2.
               05  FILLER          PIC  X(01) VALUE "³".
               05  WS-BLNK78       PIC  X(78) VALUE ALL "°".
               05  FILLER          PIC  X(01) VALUE "³".
           03  WS-BOT-LNE.
               05  WS-BCHR         PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER          PIC X(01).
               05  WS-F-LINE       PIC X(78).
               05  FILLER          PIC X(01).
           03  WS-BOT-LNE2.
               05  WS-BCH          PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE2.
               05  FILLER          PIC X(01).
               05  WS-MEM-LINE.
                   07  WS-SRT-LINE PIC X(50).
                   07  FILLER      PIC X(10).
               05  FILLER          PIC X(19).
           03  WS-MM-LINE.
               05  FILLER          PIC  X(01) VALUE X"B3".
               05  FILLER          PIC  X(60) VALUE SPACES.
               05  FILLER          PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE         PIC  X(60) VALUE ALL X"CD".

       COPY "WS.WS".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       COPY "W12.WS".

       COPY "W40.DBT".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W80-ACCOUNT.
           03  W80-ACNO            PIC X(06).
           03  W80-TYPE            PIC 9(01).

       01  W95-STM.
           03  W95-COMP            PIC X(40).

       LINKAGE SECTION.

       COPY "CHAIN.LS".

       COPY "FILE.IDS".

       SCREEN SECTION.

       01  S36.
           03  FOREGROUND-COLOR Cyan HIGHLIGHT PIC X(06) USING W80-ACNO AUTO.

      /
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS.
       AA000         SECTION.
       AA00.
           IF LS0-DBLEV < 9
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM CM000.

       AA49.
             EXIT PROGRAM.

      /    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *     S C R E E N ,   K E Y B O A R D   &  M O U S E
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3    ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³                      SCREEN-SHADOW                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To routine is used to display a shadow down the right and ³
      *    ³ along the bottom of a pop-up box. The parameters that are ³
      *    ³ required:                                                 ³
      *    ³          SHADE-ROW   - Top line of the box + 1.           ³
      *    ³          SHADE-COL   - Left line of box + 2.              ³
      *    ³          SHADE-WIDTH - Width of the box - 2.              ³
      *    ³          SHADE-LINES - Hight of box - 1.                  ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³                      CHECK-CORRECT                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine displays a pop-up window with the message    ³
      *    ³           "Press Y if correct - N if incorrect"           ³
      *    ³                                                           ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³                                                           ³
      *    ³ To start with the pop-up window higher or lower than row  ³
      *    ³ 13 (default); move another value to SLIN and PERFORM      ³
      *    ³ CHECK-SETUP THRU CHECK-EXIT.                              ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ****   T H I S   R O U T I N E   I S   U S E D   T O
      *         D I S P L A Y   E R R O R   M E S S A G E S
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                      ERROR-MESSAGE                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To display the error message higher or lower (default is  ³
      *    ³ line 20) Move the line number which must be used as the   ³
      *    ³ top line to WS-MES-LINE. The message to be displayed must ³
      *    ³ be in WS-ERR-MES. PERFORM ERROR-MESSAGE.                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.CRT".

       COPY "LOCKED.REC".

       COPY "CLEAR.CRT".

      /
      *    ****    READ FILES ROUTINES
      *
       AC000              SECTION.

       COPY "CONTROL.RD".

       COPY "DEBTOR.RD".

       COPY "PARAM.RD".

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
       AD000             SECTION.

       COPY "CONTROL.WR".

       COPY "DEBTOR.WR".

       COPY "PARAM.WR".

      /
      *    ****    R E S E T   A D D R E S S   L A B E L   F L A G
      *
       CM000         SECTION 5.
       CM00.
             MOVE "S"                TO CRT-TYPE.
             CALL "UTP\CRTHEAD" USING CRT-HEADINGS LS-USER-ID.
             DISPLAY "Set address label flag to " AT 0812
           "Y"                          WITH FOREGROUND-COLOR Brown HIGHLIGHT
                      "/"
             "N"                        WITH FOREGROUND-COLOR Brown HIGHLIGHT
                        " or "
                  "S"                   WITH FOREGROUND-COLOR Brown HIGHLIGHT
                   "electively amend flag".
             MOVE SPACE              TO WS-LABEL.

       CM05.
             ACCEPT WS-LABEL AT 0868 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT UPDATE AUTO.
             CALL "CBL_TOUPPER" USING WS-LABEL BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-LABEL = "N" OR "Y" OR "S")
               GO TO CM05.

       CM-CHK.
             DISPLAY "Account :" AT 1012.
             MOVE "     !"           TO DEB-ACNO.
           IF WS-LABEL = "S"
               MOVE SPACES           TO W80-ACNO
               DISPLAY "Key in the account number from where to start" AT 1212 WITH FOREGROUND-COLOR Brown HIGHLIGHT
               ACCEPT S36 AT 1022
               MOVE 12               TO CRT-START CRT-END
               PERFORM ERASE-SCREEN
               IF W80-ACNO NOT = SPACES
                   MOVE W80-ACNO     TO DEB-ACNO.
             PERFORM START-AT-DEBT-AC THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               GO TO CM999.

       CM20.
             PERFORM READ-DEBTOR-NEXT-LOCK THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               GO TO CM999.
             DISPLAY DEB-ACNO AT 1022 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
           IF WS-LABEL = "S"
               PERFORM CM30 THRU CM40
               IF WS-LABEL = "X"
                   GO TO CM999
               END-IF
           ELSE
               MOVE WS-LABEL         TO DEB-LAB.
             PERFORM REWRITE-DEBTOR-UNLOCK THRU WRITE-DEBTOR-EXIT.
             GO TO CM20.

       CM30.
             DISPLAY DEB-NAME              AT 1029 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY "Address Label Flag:" AT 1212.
             DISPLAY "Esc"                 AT 1412 WITH FOREGROUND-COLOR Brown HIGHLIGHT
              "ape to exit".

       CM35.
             ACCEPT DEB-LAB AT 1232 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT UPDATE AUTO.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                      MOVE "X"       TO WS-LABEL
                      GO TO CM40
                 WHEN OTHER
                      CALL X"E5"
                      GO TO CM05
               END-EVALUATE.
             CALL "CBL_TOUPPER" USING DEB-LAB BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(DEB-LAB = "Y" OR "N")
               GO TO CM35.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO CM35.

       CM40.
      EXIT.

       CM999.
             EXIT.
      /
      *    ****    I N I T I A L I Z E   P R O G R A M
      *
       ZA000-INIT        SECTION 8.
       ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-L-OR-N          TO W02-L-OR-N.
             MOVE WS-SYS-ID          TO W02-SYSID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY.
             MOVE LS-USUB            TO WS-USUB.
             PERFORM ERASE-SCREEN.
      *
      *    ****    D O  N O T   D I S P L A Y   E R R O R
      *            M E S S A G E S
      *
             MOVE 44                 TO USER-NUMBER.
             MOVE 1                  TO USER-SETTING.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

       ZA00A-CONTINUE.
             MOVE 1                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY        TO W95-COMP.
             MOVE PAR-DMY            TO W12-TODAY.
             MOVE PAR-YMD            TO W12-T-YMD.
             GO TO ZA999-EXIT.

       ZA55.
             MOVE 1                  TO WS-S1.
             MOVE SPACES             TO WS-MID-LNE.
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
             MOVE LS-COMPANY         TO WS-TOP-COMP.
           IF LS-USER = LS-SYS-ID
               MOVE "SupervisorÄ"    TO WS-WRKHD
           ELSE
               MOVE "Workstation"    TO WS-WRKHD
               MOVE LS-USER          TO WS-WRKST.

       ZA999-EXIT.
             EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *
       ZB000-ERROR      SECTION 8.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
           IF WS-F-ERROR = 2
               MOVE W02-NETWORK      TO WS-FILE
               MOVE WS-NETKEY        TO WS-KEY
           ELSE
           IF WS-F-ERROR = 6
               MOVE W02-DEBTOR       TO WS-FILE
               MOVE ZERO             TO WS-KEY
               MOVE DEB-ACNO         TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 15
               MOVE WS-PARID         TO WS-FILE
               MOVE WS-PARKEY        TO WS-KEY.

       COPY "DISPERR.PRO".
