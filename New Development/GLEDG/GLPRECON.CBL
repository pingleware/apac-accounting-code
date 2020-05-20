      $set LINKCOUNT"512" GNT"GLPRECON.GNT"
      ******************************************************************
      *                                                                *
      *  ******   **     *******    **** **   ******** ******** *
      * **    **  **     **   **  **  **   ***   **       **       *
      * **   **     **   ** **    ** **   *******  **       *
      * **   **     *******  **    ** **    ** ******   *
      * **   ***  **     **      **    ** **    ** **       *
      * **    **  **     **       **  ** **   **    ** **       *
      *  ******   ********  **        ****   ******  ******  ******** *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     G / L E D G E R   S E T   R E C O N   I N D   O N A L L  *
      *     B A N K   T R A N S A C T I O N S          *
      *                                                                *
      *       Version 9.04.02 - June 2017                              *
      *               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        GLPRECON.
       AUTHOR.                 J.W. LEMMON (APAC).
       DATE-WRITTEN.        APRIL 2000.

                COPYRIGHT NOTICE: COPYRIGHT (C) 2000 - 2017
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

       COPY "GACCNT.SL".

       COPY "GLTRAN.SL".

       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "PARAM.FDE".

       COPY "GACCNT.FDE".

       COPY "GLTRAN.FDE".

      /
       WORKING-STORAGE SECTION.
       77  WS-CHECK    PIC X(18)  VALUE
      "aeWlimemnomLalismJ".
       77  WS-PARKEY    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-GTRKEY       PIC  9(06)    COMP-5 VALUE 1.
       77  WS-LTRKEY       PIC  9(06)    COMP-5 VALUE 0.
       77  WS-ENQPOS       PIC  9(06)    COMP-5.
       77  WS-ENQEND       PIC  9(06)    COMP-5.
       77  WS-NETKEY       PIC  9(04)    COMP-5 VALUE 1.
       77  WS-KEYSTR       PIC  9(06)    COMP-5.
       77  WS-KSTORE       PIC  9(06)    COMP-5.
       77  WS-SKSTORE      PIC  9(06)    COMP-5.
       77  WS-INKEY        PIC  9(06)    COMP-5.
       77  WS-COKEY        PIC  9(06)    COMP-5.
       77  WS-GLJSTR       PIC  9(04)    COMP-5.
       77  WS-GLJKEY       PIC  9(06)    COMP-5.
       77  WS-RECKEY       PIC  9(04)    COMP-5.
       77  WS-TRANS    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-ISUB         PIC  9(04)    COMP-5.
       77  WS-EFT    PIC 9(04)  COMP-5.
       77  WS-PAGE         PIC  9(04)    COMP-5.
       77  WS-LINES        PIC  9(04)    COMP-5 VALUE 66.
       77  WS-SUB          PIC  9(04)    COMP-5.
       77  WS-S            PIC  9(04)    COMP-5.
       77  WS-S1           PIC  9(04)    COMP-5.
       77  WS-S2           PIC  9(04)    COMP-5.
       77  WS-S3           PIC  9(04)    COMP-5.
       77  WS-S4    PIC 9(04)  COMP-5.
       77  WS-S6           PIC  9(04)    COMP-5.
       77  WS-S9    PIC 9(04)  COMP-5.
       77  WS-IXS1    PIC 9(04)  COMP-5.
       77  WS-Y-END        PIC  9(02) VALUE ZERO.
       77  WS-L-END    PIC 9(04) VALUE ZERO.
       77  WS-OPEN         PIC  9(02) VALUE ZERO.
       77  WS-MS1          PIC  9(04)    COMP-5.
       77  WS-MS2          PIC  9(04)    COMP-5.
       77  WS-VALUE        PIC S9(09)V99 COMP-3.
       77  WS-EXS          PIC  9(01).
       77  WS-CONREC       PIC  9(01) VALUE ZERO.
       77  WS-INDG         PIC  9(01) VALUE ZERO.
       77  WS-OPTION       PIC  X(01).
       77  WS-SKIP         PIC  X(01) VALUE "N".
       77  WS-FIRST        PIC  9(02) VALUE ZERO.
       77  WS-LAST    PIC 9(03) COMP-3 VALUE ZERO.
       77  WS-ERROR        PIC  9(01) VALUE ZERO.
       77  WS-NUM          PIC  Z9.
       77  WS-NORM         PIC  X(04).
       77  WS-COND         PIC  X(04).
       77  WS-EXPP         PIC  X(04).
       77  WS-ECAN         PIC  X(04).
       77  WS-8LPI       PIC  X(04).
       77  WS-6LPI       PIC  X(04).
       77  WS-10CPI        PIC  X(04).
       77  WS-12CPI        PIC  X(04).
       77  WS-17CPI        PIC  X(04).
       77  WS-DBLP         PIC  X(04).
       77  WS-DCAN         PIC  X(04).
       77  WS-BATCH        PIC  9(05)    COMP-3.
       77  WS-TNEXT        PIC  9(04)    COMP-5.
       77  WS-PERIOD    PIC 9(02) VALUE ZERO.
       77  WS-PASS    PIC X(08).
       77  WS-SPC    PIC X(30) VALUE SPACES.
       77  WS-ER6          PIC  X(09) VALUE "No Record".
       77  WS-ER7          PIC  X(13) VALUE "Group Heading".
       77  PRG-NAME                PIC  X(12)           VALUE SPACES.
       77  PRG-GLED-LUP            PIC  X(12)           VALUE "GLP\GLPLOOK".
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *         C =  Clear comment line (50)
      *         E =  Clear lines (any line or lines between 5 and 46)
      *         H =  Change heading
      *         S =  Clear the screen and display headings
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "GLPRECON".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "G/LEDGER -SET BANK TRANS TO RECONCILED".
      *
      *    Company Name
      *
           03  CRT-COMPANY         PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-TERMINAL.
               05  CRT-WRKHD       PIC  X(11).
               05  CRT-WRKST       PIC  X(03).

       COPY "WS.WS".

       01  WS-HELP.
    03  WS-MODULE       PIC  X(03) VALUE "GLP".
    03  WS-PROG        PIC  X(03) VALUE "015".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       COPY "W05.GL".

       01  W10-H2.
           03  FILLER      PIC  X(27).
           03  W10-HEAD    PIC  X(13).

       COPY "W15.GL".

       01  W20-TOTALS.
           03  W20-DEBIT   PIC S9(09)V99 COMP-3.
           03  W20-CREDIT  PIC S9(09)V99 COMP-3.
           03  W20-RESULT  PIC S9(09)V99.
           03  W20-RES1    REDEFINES W20-RESULT.
               05  W20-WHOLE
                           PIC  9(09).
               05  W20-DEC PIC S9(02).
           03  W20-TOTAL   PIC S9(09)V99 COMP-3.
           03  W20-STOTAL  PIC S9(09)V99 COMP-3.
           03  W20-BTOTAL  PIC S9(09)    COMP-3.
           03  W20-INTOT   PIC S9(09)V99 COMP-3.
           03  W20-COTOT   PIC S9(09)V99 COMP-3.
           03  W20-GTOT.
               05  W20-GRPTOT
                           PIC S9(09)V99 COMP-3 OCCURS 12.
           03  W20-STOT.
               05  W20-SUBTOT
                           PIC S9(09)V99 COMP-3 OCCURS 12.
       01  W20-INPUT.
           03  W20-ACCOUNT PIC  9(06).
           03  W20-ACNO1   REDEFINES W20-ACCOUNT.
               05  W20-ACNO
                           PIC 9(04).
               05  W20-SER PIC  9(02).
           03  W20-ACNO2   REDEFINES W20-ACCOUNT.
               05  W20-GR  PIC  9(02).
               05  W20-SGRP
                           PIC 9(02).
               05  W20-ASER
                           PIC 9(02).
           03  W20-ACNO3   REDEFINES W20-ACCOUNT.
               05  W20-GROUP
                           PIC 9(02).
               05  W20-REST
                           PIC 9(04).

       COPY "W20.WS".

       01  W25-CALCS.
           03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
               05  W25-WHOLE   PIC 9(05).
               05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(04).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).

       01  W25-EDIT.
           03  W25-7.
               05  W25-1   PIC  ZZZZZZZZ9-.
           03  W25-S7V2.
               05  W25-V1  PIC  Z(07)9.99CR.
          03  W25-S8V2.
              05  W25-VG1     PIC Z(07)9.99CR.
          03  W25-DATE.
              05  W25-DTE PIC Z9/99/9999.
           03  W25-REF.
               05  W25-REFER
                           PIC Z(07).
           03  W25-RECKEY  PIC  Z(04)9.

       01  W30-MONTH-NAMES.
           03  W30-MONTHS.
               05  FILLER  PIC  X(24) VALUE
                           "Jan:Feb:Mar:Apr:May:Jun:".
               05  FILLER  PIC X(24) VALUE
                           "Jul:Aug:Sep:Oct:Nov:Dec:".
           03  W30-MNTHS REDEFINES W30-MONTHS.
               05  W30-MONTH OCCURS 12.
                   07  W30-MTH PIC X(03).
                   07  FILLER  PIC X(01).

       01  W35-MONTH-NAMES.
           03  W35-MONTHS.
               05  W35-MONTH OCCURS 12.
                   07  W35-MTH PIC X(03).
                   07  FILLER  PIC X(01).
           03  W35-MTHS REDEFINES W35-MONTHS.
               05  W35-MTH1.
                   07  W35-M1  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH2.
                   07  W35-M2  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH3.
                   07  W35-M3  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH4.
                   07  W35-M4  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH5.
                   07  W35-M5  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH6.
                   07  W35-M6  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH7.
                   07  W35-M7  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH8.
                   07  W35-M8  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH9.
                   07  W35-M9  PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH10.
                   07  W35-M10 PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH11.
                   07  W35-M11 PIC X(03).
                   07  FILLER  PIC X(01).
               05  W35-MTH12.
                   07  W35-M12 PIC X(03).
                   07  FILLER  PIC X(01).

       01  W40-CUR-YEAR.
          03  W40-YEAR    PIC 9(04) COMP-5 OCCURS 12.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W50-CALC.
           05  W50-ACNTS   OCCURS 30.
               07  W50-ANO PIC  9(06)    COMP-3.
               07  W50-PER PIC S9(03)V99 COMP-3.

       01  W60-TOTALS.
           03  W60-VAL     PIC S9(09)    COMP-3 OCCURS 12.

       01  W65-TOTALS.
           03  W65-VAL     PIC S9(09)    COMP-3 OCCURS 12.

       01  W70-ACTS.
           03  W70-BRANCH  PIC  9(04)    COMP-5.

       COPY "W70.WS".

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
    03  W75-GNO       PIC 9(06) COMP-5
                              OCCURS 18.

       01  W90-COMP.
           03  W90-CNAME   PIC  X(40).

       LINKAGE SECTION.

       COPY "CHAIN.LS".

       COPY "FILE-GENL.IDS".

      /
       SCREEN SECTION.

       01  S13.
          03  FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z9/99/9999 USING W15-DATE AUTO.
       01  S14.
          03  FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z(07)9.99CR USING W15-VAL AUTO.
       01  S15.
          03  FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
      PIC ZZ9.99 USING W15-PER AUTO.
       01  S16.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC X(07) USING W15-REF AUTO.
       01  S17.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC 9(06) USING W15-ACCOUNT AUTO.

       COPY "LEDLUP.CRT".

       01  S20.
    03  LINE 11 COLUMN 20 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
         VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
          03  LINE 12 COLUMN 20 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
         VALUE "³ Enter Reconcile - Password          ³".
    03  LINE 13 COLUMN 20 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
         VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S20A.
          03  LINE 12 COLUMN 49 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5 PIC  X(08)
     USING WS-PASS AUTO SECURE.

       01  S22.
          03  LINE  2 COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT
     VALUE "RECONCILE ALL TRANSACTIONS".
    03  LINE  4 COLUMN  4 VALUE "Account Number:".
    03        COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(06) FROM WS-SPC.
          03  LINE  5 COLUMN  4 VALUE "Account Name  :".
    03        COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(30) FROM WS-SPC.
          03  LINE  7 COLUMN  5 VALUE "Date".
          03        COLUMN 13 VALUE "Trn".
          03        COLUMN 18 VALUE "Reference".
          03        COLUMN 38 VALUE "Narrative/Payee".
          03        COLUMN 70 VALUE "Value".

       01  S23.
    03  LINE 12 COLUMN 38 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
         VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
    03  LINE 13 COLUMN 38 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
         VALUE "³".
    03        COLUMN 39 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
         VALUE " Account Balance      ".
    03        COLUMN 61 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC ZZZZZZZZ9.99- FROM W20-TOTAL.
    03        COLUMN 74 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
         VALUE " ³".
    03  LINE 14 COLUMN 38 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
         VALUE "³".
    03        COLUMN 39 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
         VALUE " Unreconciled Debits  ".
    03        COLUMN 61 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC ZZZZZZZZ9.99 FROM W20-DEBIT.
    03        COLUMN 73 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
         VALUE "  ³".
    03  LINE 15 COLUMN 38 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
         VALUE "³".
    03        COLUMN 39 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
         VALUE " Unreconciled Credits ".
    03        COLUMN 61 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC ZZZZZZZZ9.99- FROM W20-CREDIT.
    03        COLUMN 74 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
         VALUE " ³".
    03  LINE 16 COLUMN 38 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
         VALUE "³".
    03        COLUMN 39 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
         VALUE " Reconciled Balance   ".
    03        COLUMN 61 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC ZZZZZZZZ9.99- FROM W20-RESULT.
    03        COLUMN 74 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
         VALUE " ³".
    03  LINE 17 COLUMN 38 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
         VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

      /
       PROCEDURE DIVISION
   USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000        SECTION.
       AA00.
          IF LS0-GLLEV < 4
        MOVE "Not Authorised"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO AA49.
            PERFORM ZA000.
      PERFORM OA000.

       AA49.
      EXIT PROGRAM.

       COPY "FUNCTION.CRT".

       COPY "OPTION.CRT".

       COPY "LOCKED.REC".

       COPY "CLEAR.CRT".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ This routine is used to call  the General Ledger Accounts ³
      *    ³ lookup program and cancel it when control is returned.    ³
      *    ³ The  relevant  error  message  will be  displayed  if  an ³
      *    ³ exception occurs. The usual  parameters are passed to the ³
      *    ³ lookup program as well W15-GLEDGER which will contain the ³
      *    ³ details of the account that has been selected when control³
      *    ³ is returned to the calling program.                       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AA350-LOOKUP    SECTION.
       AA350.
             PERFORM SAVE-SCREEN.
             MOVE 4              TO W44-FUNCTION.
             PERFORM SCREEN-CONTENTS.
             CALL PRG-GLED-LUP USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS W15-GLEDGER
                  ON EXCEPTION GO TO AA355
             END-CALL.
             CANCEL PRG-GLED-LUP.
             GO TO AA358.

       AA355.
             MOVE SPACE              TO WS-ERR-MES.
             STRING "Program- " DELIMITED SIZE PRG-GLED-LUP DELIMITED " " " not on disk, press ANY key" DELIMITED SIZE INTO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.

       AA358.
             PERFORM RESTORE-SCREEN.

       AA399.
             EXIT.

       COPY "AA900.ALM".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AB000           SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "GACCNT.RD".

       COPY "GLTRAN.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AC000           SECTION.

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

       COPY "GACCNT.WR".

       COPY "GLTRAN.WR".

       COPY "GACCNT.LUP".

       COPY "APAC.HLP".

      /
       OA000      SECTION 5.
       OA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S22.
    IF WS-SKIP = "Y"
        GO TO OA05.
      PERFORM SAVE-SCREEN-2.
            DISPLAY S20.
      MOVE 12   TO SHADE-ROW.
      MOVE 22   TO SHADE-COL.
      MOVE 37   TO SHADE-WIDTH.
      MOVE 2   TO SHADE-LINES.
      PERFORM SCREEN-SHADOW.
            ACCEPT S20A.
            MOVE 8   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING WS-PASS
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
            MOVE 1   TO WS-LENGTH.
          IF NOT (WS-PASS = "RECONALL")
              MOVE "Invalid password"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
              GO TO OA999.
      MOVE "Y"   TO WS-SKIP.
      PERFORM RESTORE-SCREEN-2.
      MOVE W70-BNKGL  TO W15-ACCOUNT.

       OA05.
      DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Help,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       " to do Account Lookup - "
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "ape to exit"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
            ACCEPT S17 AT 0420.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE ZERO  TO W15-ACCOUNT
       GO TO OA999
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA350
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO OA05
        END-EVALUATE
        DISPLAY S17 AT 0420
               IF W15-ACCOUNT = ZERO
           GO TO OA05.
           IF W15-ACCOUNT = ZERO
               GO TO OA999.
           IF W15-ASER = ZERO
              MOVE WS-ER7  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
              GO TO OA05.

       OA10.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
              MOVE WS-ER6  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
              GO TO OA05.
            DISPLAY G-NAME AT 0520
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      PERFORM CLEAR-L50.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO OA00.
      PERFORM OB000.
    IF WS-ERROR = 1
        MOVE "No transactions on file"
     TO WS-ERR-STRING
        PERFORM ERROR-MESSAGE
        GO TO OA00.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
      MOVE 13   TO SHADE-ROW.
      MOVE 40   TO SHADE-COL.
      MOVE 36   TO SHADE-WIDTH.
      MOVE 5   TO SHADE-LINES.
      DISPLAY S23.
      PERFORM SCREEN-SHADOW.

       OA15.
             INITIALIZE T-REC.
      MOVE G-AC   TO T-AC.

       OA20.
      PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
    IF WS-F-ERROR = ZERO
               GO TO OA28.

       OA25.
      MOVE "Not on file"  TO WS-ERR-STRING.
            PERFORM ERROR-MESSAGE
      GO TO OA00.

       OA28.
             PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
    IF (WS-F-ERROR = 11) OR
              (G-AC NOT = T-AC)
        GO TO OA25.

       OA29.
           IF T-FLAG NOT = "Y"
               MOVE "N"          TO T-FLAG.
             MOVE T-DATE         TO W15-YMD.
      MOVE W15-CN  TO W15-CC.
             MOVE W15-YR         TO W15-YY.
             MOVE W15-MT         TO W15-MM.
             MOVE W15-DY         TO W15-DD.
            DISPLAY S13 AT 0902.
    IF T-CODE = 01 OR 02
        DISPLAY "Jnl" AT 0913
   WITH FOREGROUND-COLOR 3 HIGHLIGHT
        BACKGROUND-COLOR 5
    ELSE
    IF T-CODE = 03 OR 04
        DISPLAY "Chq" AT 0913
   WITH FOREGROUND-COLOR 3 HIGHLIGHT
        BACKGROUND-COLOR 5
    ELSE
    IF T-CODE = 05 OR 10
        DISPLAY "Bnk" AT 0913
   WITH FOREGROUND-COLOR 3 HIGHLIGHT
        BACKGROUND-COLOR 5
    ELSE
    IF T-CODE = 06 OR 07
        DISPLAY "Dep" AT 0913
   WITH FOREGROUND-COLOR 3 HIGHLIGHT
        BACKGROUND-COLOR 5
    ELSE
    IF T-CODE = 08 OR 09
              DISPLAY "Eft" AT 0913
   WITH FOREGROUND-COLOR 3 HIGHLIGHT
        BACKGROUND-COLOR 5
    ELSE
    IF T-CODE = 11 OR 12
              DISPLAY "Pmt" AT 0913
   WITH FOREGROUND-COLOR 3 HIGHLIGHT
        BACKGROUND-COLOR 5
    ELSE
    IF T-CODE = 13 OR 14
              DISPLAY "Rec" AT 0913
   WITH FOREGROUND-COLOR 3 HIGHLIGHT
        BACKGROUND-COLOR 5.
            MOVE T-REF   TO W25-REF.
            DISPLAY S16 AT 0919.

       OA30.
            DISPLAY T-NARATIVE AT 0928
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
            MOVE T-VAL   TO W15-VAL.
            DISPLAY S14 AT 0966.
      MOVE T-FLAG  TO WS-OPTION.

       OA35.
      MOVE "Y"   TO WS-OPTION.
    IF NOT (T-FLAG = WS-OPTION)
        IF T-FLAG = "Y"
    IF T-VAL < ZERO
        ADD T-VAL  TO W20-CREDIT
    ELSE
        ADD T-VAL  TO W20-DEBIT
    END-IF
        ELSE
        IF T-VAL < ZERO
     SUBTRACT T-VAL
     FROM W20-CREDIT
        ELSE
     SUBTRACT T-VAL
     FROM W20-DEBIT.
      COMPUTE W20-RESULT = W20-TOTAL - W20-DEBIT - W20-CREDIT.
      DISPLAY S23.
      MOVE WS-OPTION  TO T-FLAG.
             PERFORM REWRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.

       OA40.
      PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
    IF (WS-F-ERROR = 11) OR
       (G-AC NOT = T-AC)
        MOVE "Transactions Reconciled -Press ANY key"
     TO WS-ERR-MES
        PERFORM OPT-MESSAGE
        GO TO OA00.
    IF T-SUB = 5
        GO TO OA40.
     GO TO OA29.

       OA999.
             EXIT.

       OB000        SECTION 5.
       OB00.
      MOVE G-BAL   TO W20-TOTAL W20-RESULT.
      MOVE ZERO   TO W20-DEBIT W20-CREDIT.
      INITIALIZE T-REC.
             MOVE G-AC           TO T-AC.
             PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
    IF NOT (WS-F-ERROR = ZERO)
        MOVE 1   TO WS-ERROR
        GO TO OB999.
       OB05.
             PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
           IF (WS-F-ERROR = 11) OR
              (G-AC NOT = T-AC)
        GO TO OB05.
      DISPLAY "Calculating:" AT 2312.
      DISPLAY T-KEY AT 2325 WITH FOREGROUND-COLOR 11.
      GO TO OB15.
       OB10.
             PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
           IF (WS-F-ERROR = 11) OR
              (G-AC NOT = T-AC)
        GO TO OB20.
      DISPLAY T-KEY AT 2325 WITH FOREGROUND-COLOR 11.
       OB15.
           IF (T-FLAG = "Y") OR (T-SUB = 5)
        GO TO OB10.
    IF T-VAL < ZERO
        ADD T-VAL  TO W20-CREDIT
           ELSE
        ADD T-VAL  TO W20-DEBIT.
      SUBTRACT T-VAL  FROM W20-RESULT.
      GO TO OB10.

       OB20.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.

       OB999.
      EXIT.

      /
       ZA000      SECTION 9.
       ZA00.
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

       ZA02.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-COMPANY  TO W90-CNAME.
            MOVE PAR-DMY  TO W05-TODAY.
            MOVE PAR-YMD  TO W05-TYMD.
      MOVE 6   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DEBGL      TO W70-DEBGL.
             MOVE PAR-CREGL      TO W70-CREGL.
             MOVE PAR-INTGL      TO W70-INTGL.
             MOVE PAR-BNKGL      TO W70-BNKGL.
             MOVE PAR-UNPROF     TO W70-UNPROF.
             MOVE 4              TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
            MOVE G-BATCH  TO WS-BATCH.
           IF G-YEAR-END = ZERO
              MOVE 2   TO G-YEAR-END
               PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
      *
      *    ****    S E T   U P   M O N T H S   ( Y E A R   E N D)
      *
            MOVE 1   TO WS-S1.
            MOVE G-YEAR-END  TO WS-S2 WS-Y-END.
            MOVE G-LAST-YE  TO WS-L-END.
      MOVE G-OPEN  TO WS-OPEN.
            ADD 1   TO WS-S2.
       ZA15.
           IF WS-S2 > 12
              SUBTRACT 12  FROM WS-S2.
            MOVE W30-MONTH(WS-S2)
     TO W35-MONTH(WS-S1).
           IF WS-S1 < 12
              ADD 1   TO WS-S1 WS-S2
               GO TO ZA15.
            MOVE ZERO   TO WS-ERROR.
           IF W70-BNKGL = ZERO
              DISPLAY "Parameters not set up - " AT 2020
         "ENTER" WITH FOREGROUND-COLOR 14
               ACCEPT WS-OPTION AT 2050 WITH FOREGROUND-COLOR 15 AUTO
              MOVE 1   TO WS-ERROR.
            MOVE WS-Y-END  TO WS-S1.
            MOVE W05-CY  TO WS-S2.
           IF W05-MM < WS-Y-END
              SUBTRACT 1  FROM WS-S2.
       ZA20.
            ADD 1   TO WS-S1.
           IF WS-S1 > 12
              MOVE 1   TO WS-S1
              ADD 1   TO WS-S2.
            MOVE WS-S2   TO W40-YEAR(WS-S1).
           IF WS-S1 NOT = WS-Y-END
               GO TO ZA20.
      DISPLAY SPACES AT 5025
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
            GO TO ZA999.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
       ZA55.
             MOVE 1 TO WS-S1.
            MOVE SPACES TO WS-MID-LNE.
       ZA60.
             MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
           IF WS-S1 < 80
               ADD 1 TO WS-S1
               GO TO ZA60.
             MOVE WS-G9  TO WS-TCH  (1).
             MOVE WS-G10 TO WS-TCH  (80).
             MOVE WS-G11 TO WS-BCH  (1).
             MOVE WS-G12 TO WS-BCH  (80).
             MOVE WS-G14 TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13 TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G2  TO WS-TCHR (11) WS-TCHR (45)
                            WS-TCHR (63).
             MOVE WS-G3  TO WS-MCHR (11) WS-MCHR (45)
                            WS-MCHR (63)
       WS-MCHR (1)  WS-MCHR (80).
             MOVE WS-G4  TO WS-BCHR (11) WS-BCHR (45)
                            WS-BCHR (63).
      MOVE LS-COMPANY  TO WS-TOP-COMP.
    IF LS-USER = LS-SYS-ID
        MOVE "SupervisorÄ"  TO WS-WRKHD
    ELSE
        MOVE "Workstation"  TO WS-WRKHD
        MOVE LS-USER    TO WS-WRKST.
       ZA200.
            DISPLAY "Files locked - Try later" AT 5002
              WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       " " WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       WS-STATUS
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15
             " Press any key"
              WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
            ACCEPT WS-OPTION AT 5045
      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
       ZA999.
             EXIT.
      /
       ZB000      SECTION 9.

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
           IF WS-F-ERROR = 8
               MOVE W02-GACCNT TO WS-FILE
              MOVE ZERO  TO WS-KEY
        MOVE G-AC  TO WS-KEYX
          ELSE
           IF WS-F-ERROR = 11
               MOVE W02-GLTRAN   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE T-KEY        TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 15
              MOVE WS-PARID  TO WS-FILE
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
