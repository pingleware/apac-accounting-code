      $set LINKCOUNT"512" GNT"GLPENQ.GNT"
      ******************************************************************
      *                                                                *
      *    ******   **       ******* ********  ** **   ******    *
      *   ** **  **       **    ** **   *** **  **   **   *
      *   **     **       **    ** **   **** **  **   **   *
      *   **     **       ******* ****   ** ** **  **   **   *
      *   **   ***  **       ** **   **  ****  **  ***   *
      *   ** **  **       ** **   **   ***   ******    *
      *    ******   ********  ** ********  ** **   **   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     G / L E D G E R   E N Q U I R I E S   P R O G R A M        *
      *                                                                *
      *       Version 9.04.02 - June 2017                              *
      *               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        GLPENQ.
       AUTHOR.                 J.W. LEMMON (APAC).
       DATE-WRITTEN.           JUNE 1983.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2017
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
       77  WS-CHECK                PIC  X(18)           VALUE "aeWlimemnomLalismJ".
       77  WS-PARKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-GLKEY                PIC  9(06)    COMP-5 VALUE 1.
       77  WS-GTRKEY               PIC  9(06)    COMP-5 VALUE 1.
       77  WS-LTRKEY               PIC  9(06)    COMP-5 VALUE 0.
       77  WS-GINKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-ENQPOS               PIC  9(06)    COMP-5.
       77  WS-ENQEND               PIC  9(06)    COMP-5.
       77  WS-NETKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-KEYSTR               PIC  9(06)    COMP-5.
       77  WS-KSTORE               PIC  9(06)    COMP-5.
       77  WS-SKSTORE              PIC  9(06)    COMP-5.
       77  WS-INKEY                PIC  9(06)    COMP-5.
       77  WS-COKEY                PIC  9(06)    COMP-5.
       77  WS-GLJSTR               PIC  9(04)    COMP-5.
       77  WS-GLJKEY               PIC  9(06)    COMP-5.
       77  WS-RECKEY               PIC  9(04)    COMP-5.
       77  WS-RECOVER              PIC  9(04)    COMP-5.
       77  WS-TRANS                PIC  9(04)    COMP-5 VALUE 1.
       77  WS-ISUB                 PIC  9(04)    COMP-5.
       77  WS-CHEQUE               PIC  9(04)    COMP-5.
       77  WS-PAGE                 PIC  9(04)    COMP-5.
       77  WS-LINES                PIC  9(04)    COMP-5 VALUE 66.
       77  WS-SUB                  PIC  9(04)    COMP-5.
       77  WS-S                    PIC  9(04)    COMP-5.
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S6                   PIC  9(04)    COMP-5.
       77  WS-S9                   PIC  9(04)    COMP-5.
       77  WS-IXS1                 PIC  9(04)    COMP-5.
       77  WS-Y-END                PIC  9(02)           VALUE ZERO.
       77  WS-L-END                PIC  9(04)           VALUE ZERO.
       77  WS-OPEN                 PIC  9(02)           VALUE ZERO.
       77  WS-MS1                  PIC  9(04)    COMP-5.
       77  WS-MS2                  PIC  9(04)    COMP-5.
       77  WS-VALUE                PIC S9(09)V99 COMP-3.
       77  WS-EXS                  PIC  9(01).
       77  WS-CONREC               PIC  9(01)           VALUE ZERO.
       77  WS-INDG                 PIC  9(01)           VALUE ZERO.
       77  WS-OPTION               PIC  X(01).
       77  WS-SKIP                 PIC  X(01)           VALUE "N".
       77  WS-FIRST                PIC  9(02)           VALUE ZERO.
       77  WS-LAST                 PIC  9(02)           VALUE ZERO.
       77  WS-ERROR                PIC  9(01)           VALUE ZERO.
       77  WS-NUM                  PIC  Z9.
       77  WS-NORM                 PIC  X(04).
       77  WS-COND                 PIC  X(04).
       77  WS-EXPP                 PIC  X(04).
       77  WS-ECAN                 PIC  X(04).
       77  WS-8LPI                 PIC  X(04).
       77  WS-6LPI                 PIC  X(04).
       77  WS-10CPI                PIC  X(04).
       77  WS-12CPI                PIC  X(04).
       77  WS-17CPI                PIC  X(04).
       77  WS-DBLP                 PIC  X(04).
       77  WS-DCAN                 PIC  X(04).
       77  WS-BATCH                PIC  9(05)    COMP-3.
       77  WS-TNEXT                PIC  9(04)    COMP-5.
       77  WS-PERIOD               PIC  9(02)           VALUE ZERO.
       77  WS-SPC                  PIC  X(30)           VALUE SPACES.
       77  WS-ER1                  PIC  X(07)           VALUE "Account".
       77  WS-ER3                  PIC  X(04)           VALUE "Date".
       77  WS-ER4                  PIC  X(05)           VALUE "Value".
       77  WS-ER5                  PIC  X(13)           VALUE "Cheque Number".
       77  WS-ER6                  PIC  X(09)           VALUE "No Record".
       77  WS-ER7                  PIC  X(13)           VALUE "Group Heading".
       77  PRG-NAME                PIC  X(12)           VALUE SPACES.
       77  PRG-GLED-LUP            PIC  X(12)           VALUE "GLP\GLPLOOK".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.

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
           03  CRT-PROGRAM         PIC  X(15) VALUE "GLPENQ".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "GENERAL LEDGER -ENQUIRY".
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
           03  WS-MODULE           PIC  X(03)    VALUE "GLP".
           03  WS-PROG             PIC  X(03)    VALUE "ENQ".

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

       COPY "W05.GL".

       01  W10-H2.
           03  FILLER              PIC  X(27).
           03  W10-HEAD            PIC  X(13).

       COPY "W15.GL".

       01  W20-TOTALS.
           03  W20-DEBIT           PIC S9(09)V99 COMP-3.
           03  W20-CREDIT          PIC S9(09)V99 COMP-3.
           03  W20-RESULT          PIC S9(09)V99.
           03  W20-RES1                          REDEFINES W20-RESULT.
               05  W20-WHOLE       PIC  9(09).
               05  W20-DEC         PIC S9(02).
           03  W20-TOTAL           PIC S9(09)V99 COMP-3.
           03  W20-STOTAL          PIC S9(09)V99 COMP-3.
           03  W20-BTOTAL          PIC S9(09)    COMP-3.
           03  W20-INTOT           PIC S9(09)V99 COMP-3.
           03  W20-COTOT           PIC S9(09)V99 COMP-3.
           03  W20-GTOT.
               05  W20-GRPTOT      PIC S9(09)V99 COMP-3 OCCURS 12.
           03  W20-STOT.
               05  W20-SUBTOT      PIC S9(09)V99 COMP-3 OCCURS 12.

       01  W20-INPUT.
           03  W20-ACCOUNT         PIC  9(06).
           03  W20-ACNO1                         REDEFINES W20-ACCOUNT.
               05  W20-ACNO        PIC  9(04).
               05  W20-SER         PIC  9(02).
           03  W20-ACNO2                         REDEFINES W20-ACCOUNT.
               05  W20-GR          PIC  9(02).
               05  W20-SGRP        PIC  9(02).
               05  W20-ASER        
                                   PIC  9(02).
           03  W20-ACNO3                         REDEFINES W20-ACCOUNT.
               05  W20-GROUP       PIC  9(02).
               05  W20-REST        PIC  9(04).

       COPY "W20.WS".

       01  W25-CALCS.
           03  W25-RESULT          PIC  9(05)V99.
           03  W25-RESULT1                       REDEFINES W25-RESULT.
               05  W25-WHOLE       PIC  9(05).
               05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2                       REDEFINES W25-RESULT.
               05  W25-KEY         PIC  9(04).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).

       01  W25-EDIT.
           03  W25-7.
               05  W25-1           PIC  ZZZZZZZZ9-.
           03  W25-S7V2.
               05  W25-V1          PIC  Z(07)9.99CR.
           03  W25-S8V2.
               05  W25-VG1         PIC  Z(07)9.99CR.
           03  W25-DATE.
               05  W25-DTE         PIC  Z9/99/9999.
           03  W25-REF.
               05  W25-REFER       PIC  Z(07).
           03  W25-RECKEY          PIC  Z(04)9.

       COPY "W30.GL".
       
       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       COPY "W50-70.GL".

       COPY "W70.WS".

       01  W90-COMP.
           03  W90-CNAME   PIC  X(40).

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

       COPY "CHAIN.LS".

       COPY "FILE-GENL.IDS".

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

       01  S09.
          03  LINE  2 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
                                 VALUE "GENERAL LEDGER ENQUIRY".
          03  LINE  4 COLUMN  4 VALUE "Account:".
    03        COLUMN 13 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(06) FROM WS-SPC.
          03        COLUMN 21 VALUE "Name:".
    03        COLUMN 27 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(30) FROM WS-SPC.
          03  LINE  6 COLUMN  6 VALUE "O/Balance".
          03        COLUMN 24 VALUE "Debits".
          03        COLUMN 39 VALUE "Credits".
          03        COLUMN 54 VALUE "C/Balance".
          03        COLUMN 69 VALUE "Budget".
          03  LINE  8 COLUMN 34 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "Transactions".
          03  LINE  9 COLUMN  5 VALUE "Date".
          03        COLUMN 14 VALUE "Trn".
           03          COLUMN 18 VALUE "Reference".
          03        COLUMN 38 VALUE "Narrative/Payee".
          03        COLUMN 70 VALUE "Value".

       01  S17.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC 9(06) USING W15-ACCOUNT AUTO.

       COPY "LEDLUP.CRT".

      *
      *      ******   ******    *****    *****   ******  ******   **   **  ******    ****** 
      *      **   **  **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **   **  **  **   **   **  **       **      **   **  **   **  **  **    **
      *      ******   *****    **   **  **       *****   **   **  **   **  *****     *****
      *      **       **  **   **   **  **       **      **   **  **   **  **  **    **
      *      **       **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **       **   **   *****    *****   ******  ******    *****   **   **   ******
      *
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS.
       AA000        SECTION.
       AA00.
             PERFORM ZA000.
             PERFORM EA000.

       AA49.
             EXIT PROGRAM.

       COPY "FUNCTION.SCR".

       COPY "ERROR.SCR".

       COPY "LOCKED.REC".

       COPY "CLEAR.CRT".

       COPY "APAC.HLP".

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

       COPY "GACCNT.LUP".

      /
       EA000     SECTION 5.
       EA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S09.
             SUBTRACT 1 FROM W05-MM GIVING WS-S2.
           IF WS-S2 = ZERO
              MOVE 12   TO WS-S2.
            MOVE ZERO   TO W15-ACCOUNT.
       EA05.
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
            ACCEPT S17 AT 0413.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE ZERO  TO W15-ACCOUNT
       GO TO EA999
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA350
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO EA05
        END-EVALUATE
        DISPLAY S17 AT 0413
               IF W15-ACCOUNT = ZERO
           GO TO EA05.
      PERFORM CLEAR-L50.
           IF W15-ACCOUNT = ZERO
               GO TO EA999.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
              MOVE WS-ER6  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
              GO TO EA05.
            PERFORM CLEAR-L50.
    IF G-SEC = "Y"
        IF NOT (LS0-GLLEV = 9)
     MOVE "Not Authorised"
     TO WS-ERR-STRING
           PERFORM ERROR-MESSAGE
     GO TO EA05.
            DISPLAY G-NAME AT 0427
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
       EA15.
            MOVE W05-MM  TO WS-S1.
            MOVE G-VAL (WS-S2)  TO W25-V1.
            DISPLAY W25-S7V2 AT 0704
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
            MOVE G-DT   TO W25-V1.
            DISPLAY W25-S7V2 AT 0720
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
            MOVE G-CR   TO W25-V1.
            DISPLAY W25-S7V2 AT 0736
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
            MOVE G-BAL   TO W25-V1.
            DISPLAY W25-S7V2 AT 0752
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
             MOVE G-BVAL (WS-S1) TO W25-1.
            DISPLAY W25-7 AT 0768
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
            MOVE 10   TO LIN.
            DISPLAY "ENTER" AT 5012
              WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       " for all Transactions - "
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "MM" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       " for specific Month"
              WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
       EA16.
            ACCEPT W15-MONTH AT 5063
      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 11 AUTO.
           IF W15-MONTH > 12
               GO TO EA16.
            PERFORM CLEAR-L50.
             INITIALIZE T-REC.
           IF W15-MONTH NOT = ZERO
               MOVE W15-MONTH    TO W15-MT
               MOVE 01           TO W15-DY
               IF W15-MONTH > W05-MM
     SUBTRACT 1 FROM W05-CY
     GIVING W15-CY
               ELSE
     MOVE W05-CY  TO W15-CY
        END-IF
              MOVE W15-CC  TO W15-CN
              MOVE W15-YY  TO W15-YR
        MOVE W15-YMD  TO T-DATE.
      MOVE G-AC   TO T-AC.
      PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
           IF WS-F-ERROR = 11
               GO TO EA50.
      MOVE ZERO   TO WS-ENQEND WS-ENQPOS.

       EA20.
             PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
      COMMIT.
           IF (WS-F-ERROR = 11) OR
              (T-AC NOT = G-AC)
               MOVE WS-ENQPOS    TO WS-ENQEND
               GO TO EA30.
             ADD 1               TO WS-ENQPOS.
       EA20A.
           IF W15-MONTH NOT = ZERO
               IF T-MM > W15-MONTH
                   SUBTRACT 1    FROM WS-ENQPOS
                   MOVE WS-ENQPOS
                                 TO WS-ENQEND
                   GO TO EA30.
           IF T-SUB NOT = 5
               MOVE T-DATE       TO W15-YMD
        MOVE W15-CN  TO W15-CC
               MOVE W15-YR       TO W15-YY
               MOVE W15-MT       TO W15-MM
               MOVE W15-DY       TO W15-DD
               MOVE W15-DATE     TO W25-DTE
              DISPLAY (LIN, 2) W25-DATE
    WITH FOREGROUND-COLOR 3 HIGHLIGHT
         BACKGROUND-COLOR 5
           ELSE
               GO TO EA25.
           IF T-CODE = 01 OR 02
              DISPLAY (LIN, 14) "Jnl"
      WITH FOREGROUND-COLOR 3 HIGHLIGHT
           BACKGROUND-COLOR 5
          ELSE
           IF T-CODE = 03 OR 04
              DISPLAY (LIN, 14) "Chq"
      WITH FOREGROUND-COLOR 3 HIGHLIGHT
           BACKGROUND-COLOR 5
          ELSE
          IF T-CODE = 05 OR 10
              DISPLAY (LIN, 14) "Bnk"
      WITH FOREGROUND-COLOR 3 HIGHLIGHT
           BACKGROUND-COLOR 5
           ELSE
          IF T-CODE = 06 OR 07
              DISPLAY (LIN, 14) "Dep"
      WITH FOREGROUND-COLOR 3 HIGHLIGHT
           BACKGROUND-COLOR 5
          ELSE
          IF T-CODE = 08 OR 09
              DISPLAY (LIN, 14) "Eft"
      WITH FOREGROUND-COLOR 3 HIGHLIGHT
           BACKGROUND-COLOR 5
          ELSE
          IF T-CODE = 11 OR 12
              DISPLAY (LIN, 14) "Pmt"
      WITH FOREGROUND-COLOR 3 HIGHLIGHT
           BACKGROUND-COLOR 5
          ELSE
          IF T-CODE = 13 OR 14
              DISPLAY (LIN, 14) "Rec"
      WITH FOREGROUND-COLOR 3 HIGHLIGHT
           BACKGROUND-COLOR 5
           ELSE
               GO TO EA25.
    IF T-REF NUMERIC
              MOVE T-REF  TO W25-REFER
    ELSE
        MOVE T-REFER  TO W25-REF.
            DISPLAY (LIN, 19) W25-REF
          WITH FOREGROUND-COLOR 3 HIGHLIGHT
        BACKGROUND-COLOR 5.
       EA25.
            DISPLAY (LIN, 28) T-NARATIVE
          WITH FOREGROUND-COLOR 3 HIGHLIGHT
        BACKGROUND-COLOR 5.
           IF T-SUB NOT = 5
               MOVE T-VAL TO W25-V1
              DISPLAY (LIN, 66) W25-S7V2
          WITH FOREGROUND-COLOR 3 HIGHLIGHT
        BACKGROUND-COLOR 5.
       EA26.
           IF LIN > 22
               GO TO EA30.
             ADD 1 TO LIN.
             GO TO EA20.
       EA30.
            DISPLAY "PgDn" AT 5002
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       ", " WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "PgUp" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       " or " WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "ape to exit"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
       EA32.
            ACCEPT WS-OPTION AT 5031
      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO EA00
   WHEN PAGE-UP
       IF WS-ENQPOS = 0 OR 1 OR 14
    GO TO EA30
       ELSE
    GO TO EA35
       END-IF
   WHEN PAGE-DOWN  GO TO EA33
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO EA32
        END-EVALUATE.

       EA33.
           IF WS-ENQPOS = WS-ENQEND
               GO TO EA32.
            PERFORM CLEAR-L50.
             MOVE 10             TO LIN.
             MOVE 1001           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
             GO TO EA20.
       EA35.
             MOVE 1              TO WS-S1.
       EA40.
             PERFORM READ-GLTRAN-PREV THRU READ-GLTRAN-EXIT.
           IF WS-F-ERROR = 11
               GO TO EA45.
    IF NOT (T-AC = G-AC)
        PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT
        COMMIT
        GO TO EA42.
             SUBTRACT 1          FROM WS-ENQPOS.
             ADD 1               TO WS-S1.
           IF WS-S1 < 28
        IF WS-ENQPOS > 0
     GO TO EA40.

       EA42.
      ADD 1   TO WS-ENQPOS.

       EA45.
            PERFORM CLEAR-L50.
             MOVE 10             TO LIN.
             MOVE 1001           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
             GO TO EA20A.
       EA50.
      MOVE "No transactions on file"
     TO WS-ERR-STRING.
            PERFORM ERROR-MESSAGE.
      GO TO EA00.
       EA999.
             EXIT.

      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000      SECTION 9.
       ZA00.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W05-TODAY.
             MOVE LS-TODAY-YYMMDD    TO W05-TYMD.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY W90-CNAME.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.PRO".

       ZA02.
             MOVE 6                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DEBGL          TO W70-DEBGL.
             MOVE PAR-CREGL          TO W70-CREGL.
             MOVE PAR-INTGL          TO W70-INTGL.
             MOVE PAR-BNKGL          TO W70-BNKGL.
             MOVE PAR-UNPROF         TO W70-UNPROF.
             MOVE 4                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
           IF G-YEAR-END = ZERO
               MOVE 2                TO G-YEAR-END.
           IF G-OPEN = ZERO
               ADD 1 G-YEAR-END      GIVING G-OPEN.
      *    *************************************************************
      *    ****    S E T   U P  M O N T H S   ( Y E A R   E N D)   ****
      *    *************************************************************
             MOVE 1                  TO WS-S1.
             MOVE G-YEAR-END         TO WS-S2 WS-Y-END.
             MOVE G-LAST-YE          TO WS-L-END.
             MOVE G-OPEN             TO WS-OPEN.
             MOVE ZERO               TO WS-ERROR.
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
            MOVE WS-S2   TO W40-YEAR (WS-S1).
           IF WS-S1 NOT = WS-Y-END
               GO TO ZA20.
      DISPLAY SPACES AT 5025
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
            GO TO ZA999.
      *
      *ZA200.
      *
      *COPY "LOCKED.PRO".
      *
      *ZA205.
      *      EXIT PROGRAM.

       ZA999.
             EXIT.
      *
      *     I / O   E R R O R   M E S S A G E S
      *
       ZB000      SECTION 9.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 2          MOVE W02-NETWORK  TO WS-FILE
                               MOVE WS-NETKEY    TO WS-KEY
               WHEN 8          MOVE W02-GACCNT   TO WS-FILE
                               MOVE G-AC         TO WS-KEYX
               WHEN 11         MOVE W02-GLTRAN   TO WS-FILE
                               MOVE T-KEY        TO WS-KEYX
               WHEN 15         MOVE WS-PARID     TO WS-FILE
                               MOVE WS-PARKEY    TO WS-KEY
               WHEN OTHER      MOVE "ERROR"      TO WS-FILE
                               MOVE "GLPENQ"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
