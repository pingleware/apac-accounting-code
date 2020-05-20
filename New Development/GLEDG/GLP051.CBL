      $set LINKCOUNT"384" GNT"GLP051.GNT"
      ******************************************************************
      *                                                                *
      *    ******   **       *******   ****   ********     **      *
      *   ** **  **       **    **  **  **   **       ***      *
      *   **     **       **    ** **    **  *******      **      *
      *   **     **       ******* **    ** **     **      *
      *   **   ***  **       ** **    ** **     **      *
      *   ** **  **       **  **  **   ** **     **      *
      *    ******   ********  **   ****    ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     G / L E D G E R   R E V I E W   I N T E G R A T I O N      *
      *                                                                *
      *       Version 9.04.02 - June 2017                              *
      *               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        GLP051.
       AUTHOR.                 J.W. LEMMON (APAC).
       DATE-WRITTEN.           OCTOBER 1992.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1992 - 2017
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

       COPY "LEDTRF.SL".

       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "PARAM.FDE".

       COPY "GACCNT.FDE".

       COPY "LEDTRF.FDE".

      /
       WORKING-STORAGE SECTION.
       77  WS-CHECK                  PIC  X(18)    VALUE "aeWlimemnomLalismJ".
       77  WS-PARKEY    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-GLKEY        PIC  9(06)    COMP-5 VALUE 1.
       77  WS-GTRKEY       PIC  9(06)    COMP-5 VALUE 1.
       77  WS-LTRKEY       PIC  9(06)    COMP-5 VALUE 0.
       77  WS-GINKEY       PIC  9(04)    COMP-5 VALUE 1.
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
       77  WS-ISUB         PIC  9(04)    COMP-5.
       77  WS-CHEQUE       PIC  9(04)    COMP-5.
       77  WS-SUB    PIC 9(04)  COMP-5.
       77  WS-S            PIC  9(04)    COMP-5.
       77  WS-S1           PIC  9(04)    COMP-5.
       77  WS-S2           PIC  9(04)    COMP-5.
       77  WS-S3           PIC  9(04)    COMP-5.
       77  WS-S4    PIC 9(04)  COMP-5.
       77  WS-S6           PIC  9(04)    COMP-5.
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
       77  WS-FIRST        PIC  9(01) VALUE ZERO.
       77  WS-LAST         PIC  9(01) VALUE ZERO.
       77  WS-ERROR        PIC  9(01) VALUE ZERO.
       77  WS-NUM          PIC  Z9.
       77  WS-BATCH    PIC 9(05)  COMP-3.
       77  WS-TNEXT        PIC  9(04)    COMP-5.
       77  WS-PERIOD       PIC  9(02)    COMP-5 VALUE ZERO.
       77  WS-ER1    PIC X(07) VALUE "Account".
       77  WS-ER3          PIC  X(04) VALUE "Date".
       77  WS-ER4          PIC  X(05) VALUE "Value".
       77  WS-ER5          PIC  X(13) VALUE "Cheque Number".
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "GLP051".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "GENERAL LEDGER -REVIEW INTEGRATION BATCH".
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
    03  WS-PROG        PIC  X(03) VALUE "002".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       COPY "W05.GL".

       01  W10-H2.
           03  FILLER      PIC X(02) VALUE "-".
           03  W10-HEAD    PIC X(25).

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

       01  W50-CALC.
           05  W50-ACNTS   OCCURS 30.
               07  W50-ANO PIC  9(06)    COMP-3.
               07  W50-PER PIC S9(03)V99 COMP-3.
       01  W55-TOTALS.
           03  W55-VAL     PIC S9(09)    COMP-3 OCCURS 12.
       01  W55-TOT         REDEFINES W55-TOTALS.
           03  W55-1       PIC S9(09)    COMP-3.
           03  W55-2       PIC S9(09)    COMP-3.
           03  W55-3       PIC S9(09)    COMP-3.
           03  W55-4       PIC S9(09)    COMP-3.
           03  W55-5       PIC S9(09)    COMP-3.
           03  W55-6       PIC S9(09)    COMP-3.
           03  W55-7       PIC S9(09)    COMP-3.
           03  W55-8       PIC S9(09)    COMP-3.
           03  W55-9       PIC S9(09)    COMP-3.
           03  W55-10      PIC S9(09)    COMP-3.
           03  W55-11      PIC S9(09)    COMP-3.
           03  W55-12      PIC S9(09)    COMP-3.
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

       01  S05.
          03  LINE 3 COLUMN 26 FOREGROUND-COLOR 7 HIGHLIGHT
              VALUE "G/L REVIEW INTEGRATION BATCH".
           03  LINE 5 COLUMN  3 VALUE "Account".
           03         COLUMN 13 VALUE "Date".
           03         COLUMN 20 VALUE "Reference".
           03         COLUMN 30 VALUE
                                "____________ Narrative _____________".
           03         COLUMN 73 VALUE "Value".

       01  S05A.
           03  LINE 18 COLUMN 20 VALUE  "Totals".
          03        COLUMN 29 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(07)9.99CR USING W20-DEBIT AUTO.
          03        COLUMN 46 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(07)9.99CR USING W20-CREDIT AUTO.

       01  S13.
          03  FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z9/99/9999 USING W15-DATE AUTO.

       01  S14.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z(07)9.99CR USING W15-VAL AUTO.

       01  S16.
          03  FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
      PIC X(07) USING W15-REF AUTO.

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
          IF LS0-GLLEV < 3
        MOVE "Not Authorised"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO AA35.
            PERFORM ZA000.
      PERFORM GA000.

       AA35.
             EXIT PROGRAM.

       COPY "FUNCTION.SCR".

       COPY "ERROR.SCR".

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
       AB000        SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "GACCNT.RD".

       COPY "LEDTRF.RD1".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AC000           SECTION.

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

       COPY "GACCNT.WR".

       COPY "LEDTRF.WR".

       COPY "GACCNT.LUP".

       COPY "APAC.HLP".

      /
      *    ****    R E V I E W   I N T E G R A T I O N   B A T C H
      *
       GA000        SECTION 5.
       GA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S05.
             MOVE ZERO           TO W20-DEBIT W20-CREDIT.
             MOVE ZERO           TO W15-REFER.
             INITIALIZE XFR-REC.
             PERFORM START-AT-LEDTRF-KEY THRU READ-LEDTRF-EXIT.
           IF WS-F-ERROR = 40
        MOVE "No batch outstanding"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO GA999.
      DISPLAY "Reading batch details" AT 5002
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
       GA05.
             PERFORM READ-LEDTRF-NEXT THRU READ-LEDTRF-EXIT.
           IF WS-F-ERROR = 40
               GO TO GA10.
           IF XFR-VALUE < ZERO
               ADD XFR-VALUE     TO W20-CREDIT
           ELSE
               ADD XFR-VALUE     TO W20-DEBIT.
             GO TO GA05.
       GA10.
             INITIALIZE XFR-REC.
             PERFORM START-AT-LEDTRF-KEY THRU READ-LEDTRF-EXIT.
      MOVE ZERO   TO WS-LAST.
             MOVE 1              TO WS-FIRST.

       GA15.
             DISPLAY S05A.
             PERFORM READ-LEDTRF-NEXT THRU READ-LEDTRF-EXIT.
           IF WS-F-ERROR = 40
               MOVE 1            TO WS-LAST
        IF WS-FIRST = 0
     GO TO GA25
        END-IF
    ELSE
        MOVE ZERO  TO WS-FIRST.
    IF WS-LAST = 1 AND WS-FIRST = 1
        GO TO GA00.

       GA18.
    IF XFR-MM > 9
        IF XFR-MM = 10
     MOVE "O"  TO W15-IMM
        ELSE
        IF XFR-MM = 11
     MOVE "N"  TO W15-IMM
        ELSE
     MOVE "D"  TO W15-IMM
        END-IF
    ELSE
        MOVE XFR-MM  TO W15-IMN.
             MOVE XFR-DD         TO W15-IDD.
             MOVE XFR-LEDG       TO W15-ILEDG.
             MOVE XFR-DATE       TO W15-YMD.
      MOVE W15-CN  TO W15-CC.
             MOVE W15-YR         TO W15-YY.
             MOVE W15-MT         TO W15-MM.
             MOVE W15-DY         TO W15-DD.
             MOVE XFR-AC         TO W15-ACCOUNT.
             MOVE XFR-NAR        TO W15-NAR.
             MOVE XFR-VALUE      TO W15-VAL.
            DISPLAY S17 AT 0703.
            DISPLAY S13 AT 0710.
             DISPLAY S16 AT 0721.
             DISPLAY W15-NAR AT 0730
             WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5 UPDATE AUTO.
             DISPLAY S14 AT 0767.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
       MOVE "*** INVALID ACCOUNT NUMBER"
                                 TO G-NAME
              MOVE ZERO          TO G-BAL.
            DISPLAY G-NAME AT 0912
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
             DISPLAY "Balance:" AT 1112.
             MOVE G-BAL TO W25-V1.
            DISPLAY W25-S7V2 AT 1121
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
       GA20.
      MOVE "Press   to view,'A'mend,'D'elete or 'Esc'ape"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-CURSOR THRU OPT-REPLY.
      PERFORM CLEAR-L50.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM OPT-END.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO GA999
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO GA20
        END-EVALUATE
    ELSE
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
     IF WS-FIRST = 1
         GO TO GA20
     ELSE
         GO TO GA25
     END-IF
   WHEN DOWN-KEY
     IF WS-LAST = 1
         GO TO GA20
     ELSE
         GO TO GA15
     END-IF
        END-EVALUATE
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-OPTION = "A" OR "D" OR "+")
               GO TO GA20.
      PERFORM CLEAR-L50.
           IF WS-OPTION = "D"
               GO TO GA30.
           IF WS-OPTION = "+"
               MULTIPLY -1       BY XFR-VALUE
               ADD XFR-VALUE     TO W20-CREDIT
                                    W20-DEBIT
               PERFORM REWRITE-LEDTRF THRU WRITE-LEDTRF-EXIT
               DISPLAY S05A
               GO TO GA18.
             GO TO GA35.
       GA25.
             PERFORM READ-LEDTRF-PREV THRU READ-LEDTRF-EXIT.
           IF WS-F-ERROR = 40
        MOVE 1   TO WS-FIRST
        GO TO GA15.
             MOVE ZERO           TO WS-LAST.
             GO TO GA18.
       GA30.
           IF XFR-VALUE < ZERO
               SUBTRACT XFR-VALUE
                                 FROM W20-CREDIT
           ELSE
               SUBTRACT XFR-VALUE
                                 FROM W20-DEBIT.
             PERFORM DELETE-LEDTRF-REC THRU WRITE-LEDTRF-EXIT.
           IF WS-FIRST = 1
               GO TO GA15.
             DISPLAY S05A.
             GO TO GA25.
       GA35.
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
            ACCEPT S17 AT 0703.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE ZERO  TO W15-ACCOUNT
       PERFORM CLEAR-L50
       GO TO GA50
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA350
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO GA35
        END-EVALUATE
        DISPLAY S17 AT 0703
               IF W15-ACCOUNT = ZERO
                   GO TO GA35.
      PERFORM CLEAR-L50.
           IF W15-ACCOUNT = ZERO
               GO TO GA50.
           IF W15-ASER = ZERO
              MOVE WS-ER7  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO GA35.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
       MOVE "*** INVALID ACCOUNT NUMBER"
                                 TO G-NAME
              MOVE ZERO          TO G-BAL.
            DISPLAY G-NAME AT 0912
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
             DISPLAY "Balance:" AT 1112.
             MOVE G-BAL TO W25-V1.
            DISPLAY W25-S7V2 AT 1121
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
    IF XFR-VALUE < ZERO
        SUBTRACT XFR-VALUE
                                 FROM W20-CREDIT
           ELSE
        SUBTRACT XFR-VALUE
                                 FROM W20-DEBIT.
            DISPLAY "Keyin (MINUS) - and Value for Credit" AT 1402
                      WITH FOREGROUND-COLOR 14.
             ACCEPT S14 AT 0767.
            MOVE W15-VAL  TO XFR-VALUE.
          IF W15-VAL < ZERO
               ADD W15-VAL       TO W20-CREDIT
           ELSE
               ADD W15-VAL       TO W20-DEBIT.
      DISPLAY WS-BLNK78 AT 1402 WITH FOREGROUND-COLOR 3.
             DISPLAY S05A.
       GA50.
             MOVE W15-ACCOUNT    TO XFR-AC.
            PERFORM REWRITE-LEDTRF THRU WRITE-LEDTRF-EXIT.
             GO TO GA20.
       GA999.
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
      MOVE PAR-SUSGL  TO W70-SUSGL.
             MOVE PAR-DEBGL      TO W70-DEBGL.
             MOVE PAR-CREGL      TO W70-CREGL.
             MOVE PAR-INTGL      TO W70-INTGL.
             MOVE PAR-BNKGL      TO W70-BNKGL.
             MOVE PAR-UNPROF     TO W70-UNPROF.
             MOVE 4              TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE G-BATCH TO WS-BATCH.
           IF G-YEAR-END = ZERO
               MOVE 2 TO G-YEAR-END
               PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
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
            MOVE W30-MONTH (WS-S2)
     TO W35-MONTH (WS-S1).
           IF WS-S1 < 12
              ADD 1   TO WS-S1 WS-S2
               GO TO ZA15.
            MOVE ZERO   TO WS-ERROR.
           IF W70-BNKGL = ZERO
               DISPLAY "Parameters not set up - ENTER" AT 2020
                        WITH FOREGROUND-COLOR 14
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
             GO TO ZA999.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
       ZA50.
           IF WS-F-ERROR = 11
               MOVE "G/L TRAN file"
                                 TO WS-FILE
           ELSE
           IF WS-F-ERROR = 40
               MOVE "INTEGRATION"
                                 TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR 14.
             DISPLAY "Press any key to continue" AT 1012.
             ACCEPT WS-OPTION AT 1038 WITH FOREGROUND-COLOR 15.
       ZA51.
             EXIT PROGRAM.
       ZA55.
            MOVE 1   TO WS-S1.
            MOVE SPACES  TO WS-MID-LNE.
       ZA60.
            MOVE WS-G1   TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
      MOVE WS-G8   TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
           IF WS-S1 < 80
              ADD 1   TO WS-S1
               GO TO ZA60.
      MOVE WS-G9   TO WS-TCH  (1).
      MOVE WS-G10  TO WS-TCH  (80).
      MOVE WS-G11  TO WS-BCH  (1).
      MOVE WS-G12  TO WS-BCH  (80).
      MOVE WS-G14  TO WS-TCHR (1)  WS-BCHR (1).
      MOVE WS-G13  TO WS-TCHR (80) WS-BCHR (80).
            MOVE WS-G2   TO WS-TCHR (11) WS-TCHR (45)
              WS-TCHR (63).
            MOVE WS-G3   TO WS-MCHR (11) WS-MCHR (45)
              WS-MCHR (63)
        WS-MCHR (1)  WS-MCHR (80).
            MOVE WS-G4   TO WS-BCHR (11) WS-BCHR (45)
              WS-BCHR (63).
      MOVE LS-COMPANY  TO WS-TOP-COMP.
    IF LS-USER = LS-SYS-ID
        MOVE "SupervisorÄ"  TO WS-WRKHD
    ELSE
        MOVE "Workstation"  TO WS-WRKHD
        MOVE LS-USER    TO WS-WRKST.
       ZA200.
            DISPLAY "Files locked - Try later" AT 5012
              WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       " " WS-STATUS
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15.
            DISPLAY "Press any key" AT 5012
              WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
            ACCEPT WS-OPTION AT 5026
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
              MOVE W02-GACCNT  TO WS-FILE
              MOVE ZERO  TO WS-KEY
              MOVE G-AC  TO WS-KEYX
          ELSE
           IF WS-F-ERROR = 15
               MOVE WS-PARID TO WS-FILE
               MOVE WS-PARKEY TO WS-KEY
          ELSE
           IF WS-F-ERROR = 40
               MOVE W02-LEDTRF   TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE XFR-KEY  TO WS-KEYX.
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
