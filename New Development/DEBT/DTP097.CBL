      $SET LINKCOUNT"320" GNT"DTP097.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****     ******   ********   *
      *   **    **     **     **    **   **  **   **    **       **    *
      *   **    **     **     **    **  **    **  **    **      **     *
      *   **    **     **     *******   **    **   *******     **      *
      *   **    **     **     **        **    **        **    **       *
      *   **    **     **     **         **  **   **    **    **       *
      *   *******      **     **          ****     ******     **       *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *  D E B T O R   E X T R A C T   T R A N S A C T I O N S         *
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
       PROGRAM-ID.     DTP097.
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

       COPY "DBTRAN.SL".

       COPY "DEBTOR.SL".

       COPY "DEBTRN.SL".

       COPY "PARAM.SL".

       DATA DIVISION.
       FILE SECTION.

       COPY "DBTRAN.FDE".

       COPY "DEBTOR.FDE".

       COPY "DEBTRN.FDE".

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
       77  WS-SUB    PIC 9(04)  COMP-5.
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
       77  WS-DBTKEY       PIC  9(06)    COMP-5.
       77  WS-DEPKEY       PIC  9(06)    COMP-5.
       77  WS-ENQPOS       PIC  9(06)    COMP-5.
       77  WS-ENQEND       PIC  9(06)    COMP-5.
       77  WS-NETKEY       PIC  9(06)    COMP-5.
       77  WS-PARKEY       PIC  9(06)    COMP-5.
       77  WS-RECKEY       PIC  9(06)    COMP-5.
       77  WS-RECOVER      PIC  9(06)    COMP-5.
       77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
       77  WS-GST-RATE     PIC S9(03)V99 COMP-3.
       77  WS-FIN-RATE     PIC S9(03)V99 COMP-3.
       77  WS-PERIOD       PIC  9(02).
       77  WS-NTP          PIC  X(01).
       77  WS-KEYSTR    PIC 9(06)  COMP-5.
       77  WS-OPTION       PIC  X(01).
       77  WS-SKIP         PIC  X(01) VALUE "N".
       77  WS-TAX          PIC  X(01).
       77  WS-TYPE         PIC  X(01).
       77  WS-ERROR        PIC  9(01) VALUE ZERO.
       77  WS-EKEY         PIC  9(06)    COMP-5.
       77  WS-CHAR1        PIC  X(01).
       77  WS-CHAR2        PIC  X(01).
       77  WS-COMP         PIC  9(01).
       77  WS-BATCH        PIC  9(06) COMP-5.
       77  WS-ERR1    PIC X(22) VALUE "Invalid Account Number".
       77  WS-ERR2         PIC  X(09) VALUE "No Record".
       77  WS-A            PIC  X(19) VALUE "Any key to continue".
       77  WS-E            PIC  X(15) VALUE "**END** - ENTER".
       77  WS-AC           PIC  X(07) VALUE "ACCOUNT".
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "DTP097".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "ACCOUNTS RECEIVABLE- EXTRACT TRANSATIONS".
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

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W10-SPARES.
          03  W10-DATE     PIC X(10).
          03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.

       01  W10-REF.
           03  W10-NUM         PIC  9(08).

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

       01  W15-CHECK.
    03  W15-VCHK.
      05  W15-VALUE     PIC S9(09)V99 SIGN TRAILING SEPARATE.
    03  W15-VRED REDEFINES W15-VCHK.
      05  FILLER        PIC  X(04).
      05  W15-ERR       PIC  9(04).
      05  FILLER        PIC  X(04).
    03  W15-NVAL.
      05  W15-NVALUE    PIC S9(09)V99 SIGN TRAILING SEPARATE.
    03  W15-NRED REDEFINES W15-NVAL.
      05  FILLER        PIC  X(04).
      05  W15-NEW       PIC  9(04).
      05  FILLER        PIC  X(04).

       01  W25-CALCS.
           03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
               05  W25-WHOLE   PIC 9(05).
               05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(04).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).
           03  W25-TOTAL       PIC S9(09)V99 COMP-3.
           03  W25-VALUE       PIC S9(09)V99 COMP-3.
       01  W30-AGE-DATES.
           03  W30-120         PIC  9(06) COMP-3.
           03  W30-90          PIC  9(06) COMP-3.
           03  W30-60          PIC  9(06) COMP-3.
           03  W30-30          PIC  9(06) COMP-3.
           03  W30-CUR         PIC  9(06) COMP-3.

       COPY "W40.DBT".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W60-NME-ADD.
           03  W60-NAME.
               05  W60-NCHAR OCCURS 40 PIC X(01).
           03  W60-ADDRESS.
               05  W60-ACHAR OCCURS 70 PIC X(01).
           03  W60-ADDR REDEFINES W60-ADDRESS.
               05  W60-ADD     PIC X(60).
               05  FILLER      PIC X(10).
           03  W60-ADDRESS1.
               05  W60-PAD     PIC X(60).
               05  FILLER      PIC X(10).

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
           03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
           03  W75-DNO        PIC X(06) OCCURS 18.

       01  W75-TITLES.
           03  W75-DESCRIP.
               05  FILLER      PIC X(28)      VALUE
                           "Mr     Mnr    Mrs    Mev    ".
               05  FILLER      PIC X(28)      VALUE
                           "Miss   Mej    Ms     Ms     ".
               05  FILLER      PIC X(28)      VALUE
                           "Dr     Dr     Rev    Ds     ".
               05  FILLER      PIC X(28)      VALUE
                           "Prof   Prof   The HonSy Edel".
               05  FILLER      PIC X(28)      VALUE
                           "                            ".
           03  W75-CODES REDEFINES W75-DESCRIP.
               05  W75-TITLE  OCCURS 10.
                   07  W75-ETITLE  PIC X(07).
                   07  W75-ATITLE  PIC X(07).

       01  W80-ACCOUNT.
           03  W80-ACNO        PIC X(06).
           03  W80-TYPE        PIC 9(01).

       01  W85-PASS.
           03  W85-SUPER       PIC  X(06)    OCCURS 9.
           03  W85-ENTRY       PIC  9(02)    COMP-3.
           03  W85-MARG        PIC S9(02)V99.

       01  W90-TRAN.
           03  W90-120         PIC S9(09)V99 COMP-3.
           03  W90-90          PIC S9(09)V99 COMP-3.
           03  W90-60          PIC S9(09)V99 COMP-3.
           03  W90-30          PIC S9(09)V99 COMP-3.
           03  W90-CUR         PIC S9(09)V99 COMP-3.
           03  W90-BAL         PIC S9(09)V99 COMP-3.
          03  W90-IDTE        PIC 9(08)     COMP-3.
           03  W90-INT         PIC S9(09)V99 COMP-3.

       01  W95-STM.
           03  W95-COMP        PIC X(40).

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

       LINKAGE SECTION.

       COPY "CHAIN.LS".

       COPY "FILE.IDS".

       SCREEN SECTION.

       01  S27.
          03  LINE  5 COLUMN 12 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
                                 VALUE "This routine does not include a
      -                          "recovery procedure. Security".
          03  LINE  6 COLUMN 12 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
                                 VALUE "copies  of the data files should
      -                          " be made as a power  failure".
          03  LINE  7 COLUMN 12 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
                                 VALUE "or any other problem during this
      -                          " procedure would require the".
          03  LINE  8 COLUMN 12 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
                                 VALUE "DATA FILES from the previous bac
      -                          "kup to be restored !!!      ".
          03  LINE 10 COLUMN 12 VALUE "Do you wish to continue (".
    03        COLUMN 37 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "Y".
    03        COLUMN 38 VALUE "/".
    03        COLUMN 39 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "N".
    03        COLUMN 40 VALUE ")?".

       01  S28.
          03  LINE  4 COLUMN 40 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC Z9/99/9999 USING W90-IDTE AUTO.

      /
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS.
       AA000         SECTION.
       AA00.
           IF LS0-DBLEV < 9
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM CI000.
             CLOSE DEBTRN.
       AA49.
             EXIT PROGRAM.

       COPY "FUNCTION.SCR".

       COPY "ERROR.SCR".

       COPY "LOCKED.REC".

       COPY "CLEAR.CRT".

       COPY "DATE.CHK".

      /
      *    ****    READ FILES ROUTINES
      *
       AC000              SECTION.

       COPY "DBTRAN.RD1".

       COPY "DEBTOR.RD".

       COPY "DEBTRN.RD".

       COPY "PARAM.RD".

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
       AD000             SECTION.

       COPY "DBTRAN.WR".

       COPY "DEBTOR.WR".

       COPY "DEBTRN.WR".

       COPY "PARAM.WR".

      /
      *    ****    E X T R A C T   O R  L O A D   T R A N S
      *
       CI000         SECTION 5.
       CI00.
             PERFORM ERASE-SCREEN.
      DISPLAY "EXTRACT / LOAD DEBTOR TRANSACTIONS"
        AT 0224 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
      DISPLAY S27.
       CI-CHK.
            ACCEPT WS-OPTION AT 1043
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "Y" OR "N")
               GO TO CI-CHK.
           IF WS-OPTION = "N"
              GO TO CI999.
      DISPLAY "E" AT 1204 WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "xtract or " "L" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "oad transactions".
      MOVE SPACE TO WS-OPTION.
       CI05.
            ACCEPT WS-OPTION AT 1234
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "E" OR "L")
               GO TO CI-CHK.
    IF WS-OPTION = "E"
        OPEN OUTPUT DEBTRN
        IF WS-STAT1 NOT = "0"
     MOVE 31  TO WS-F-ERROR
     PERFORM OPEN-ERROR
        END-IF
        CLOSE DEBTRN.
      OPEN I-O DEBTRN.
    IF WS-STAT1 NOT = "0"
        MOVE 31   TO WS-F-ERROR
        PERFORM OPEN-ERROR.
      MOVE ZERO   TO WS-DBTKEY.
      PERFORM ERASE-SCREEN.
    IF WS-OPTION = "L"
        GO TO CI20.
      DISPLAY "EXTRACT DEBTOR TRANSACTIONS"
        AT 0227 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
      DISPLAY "Date from which to extract (DDMMYY)" AT 0404.
      MOVE W12-TODAY  TO W90-IDTE.
       CI10.
      ACCEPT S28.
      MOVE W90-IDTE  TO W10-EDTE.
      PERFORM CHECK-DATE.
    IF WS-ERROR NOT = ZERO
        GO TO CI10.
      MOVE SPACES  TO TRA-KEY.
      MOVE ZERO   TO TRA-DATE TRA-CODE.
      DISPLAY "Reading Record :" AT 0604.
      PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
    IF WS-F-ERROR = 5
        GO TO CI30.

       CI15.
      PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
      DISPLAY TRA-KEY AT 0621
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
    IF WS-F-ERROR = 5
        GO TO CI30.
    IF TRA-DATE < W22-DTE1
        GO TO CI15.
      MOVE TRA-VALUE  TO W15-VALUE.
      MOVE TRA-RECORD1  TO DBT-RECORD1.
    IF W15-ERR = 2050
        IF NOT (TRA-TAX = ZERO)
     MOVE TRA-VALUE
     TO W15-NVALUE
     COMPUTE W15-VALUE = (TRA-TAX / 14) * 114
     MOVE W15-ERR  TO W15-NEW
     MOVE W15-NVALUE
     TO DBT-VALUE.
      ADD 1   TO WS-DBTKEY.
      PERFORM WRITE-DEBTRN THRU WRITE-DEBTRN-EXIT.
      GO TO CI15.

       CI20.
      DISPLAY "LOAD DEBTOR TRANSACTIONS"
        AT 0228 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
      DISPLAY "Writing Record :" AT 0604.

       CI25.
      ADD 1   TO WS-DBTKEY.
      PERFORM READ-DEBTRN THRU READ-DEBTRN-EXIT.
    IF WS-F-ERROR = 31
        GO TO CI30.
      MOVE DBT-RECORD1  TO TRA-RECORD1.
      DISPLAY TRA-KEY AT 0621
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      MOVE TRA-AC  TO DEB-ACNO.
      PERFORM READ-DEBTOR-LOCK THRU READ-DEBTOR-EXIT.
    IF TRA-VALUE < ZERO
        ADD TRA-VALUE  TO DEB-CRED
    ELSE
        ADD TRA-VALUE  TO DEB-DEBT.
      SUBTRACT TRA-VALUE FROM DEB-STM.
      PERFORM WRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
      PERFORM REWRITE-DEBTOR-UNLOCK THRU WRITE-DEBTOR-EXIT.
      GO TO CI25.

       CI30.
      DISPLAY "Load/Extract Complete" AT 0804
        WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      DISPLAY "Press ANY key to continue" AT 1004
        WITH FOREGROUND-COLOR Brown HIGHLIGHT.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
       CI999.
             EXIT.
      /
      *    ****    I N I T I A L I Z E   P R O G R A M
      *
       ZA000-INIT        SECTION 8.
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

            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY    TO W95-COMP.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
            GO TO ZA999-EXIT.
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
       ZA999-EXIT.
             EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *
       ZB000-ERROR      SECTION 8.

       OPEN-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             GO TO DISPLAY-FILE-NAME.

       READ-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             GO TO DISPLAY-FILE-NAME.

       WRITE-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             GO TO DISPLAY-FILE-NAME.
       I-O-ERROR-EXIT.
             EXIT.

       DISPLAY-FILE-NAME.
           IF WS-F-ERROR = 5
               MOVE W02-DBTRAN TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE TRA-KEY      TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 6
               MOVE W02-DEBTOR TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
          ELSE
           IF WS-F-ERROR = 15
               MOVE WS-PARID TO WS-FILE
               MOVE WS-PARKEY TO WS-KEY
           ELSE
          IF WS-F-ERROR = 31
              MOVE W02-DEBTRN TO WS-FILE
              MOVE WS-RECKEY TO WS-KEY.
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
             DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY "Status " AT 1212
                      WS-STATUS WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                     ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
           IF WS-STATUS NOT = "94"
               DISPLAY "Key    " AT 1412
                        WS-KEY WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                        " " WS-KEYX WITH FOREGROUND-COLOR Cyan HIGHLIGHT
               DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
               DISPLAY "Please make a note of these details" AT 1812.
             STOP RUN.
