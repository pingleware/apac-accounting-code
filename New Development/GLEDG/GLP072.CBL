      $set LINKCOUNT"384" GNT"GLP072.GNT"
      ******************************************************************
      *                                                                *
      *    ******   **       *******   ****   ********   ******    *
      *   ** **  **       **    **  **  **        **   **   **   *
      *   **     **       **    ** **    **      **  **    *
      *   **     **       ******* **    **     **        **      *
      *   **   ***  **       ** **    **    **      **        *
      *   ** **  **       **  **  **     **     **        *
      *    ******   ********  **   ****     **     ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     G / L E D G E R   R E V I E W   B A T C H         *
      *                                                                *
      *       Version 9.04.02 - June 2017                              *
      *               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        GLP072.
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

       COPY "GLBTCH.SL".

       COPY "GLJRN.SL".

       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "PARAM.FDE".

       COPY "GACCNT.FDE".

       COPY "GLCHEQ.FD".

       COPY "GLJRN.FD".

      /
       WORKING-STORAGE SECTION.
       77  WS-CHECK    PIC X(18)  VALUE
      "aeWlimemnomLalismJ".
       77  WS-PARKEY   PIC  9(04)    COMP-5 VALUE 1.
       77  WS-GLKEY    PIC  9(06)    COMP-5 VALUE 1.
       77  WS-ENQPOS   PIC  9(06)    COMP-5.
       77  WS-ENQEND   PIC  9(06)    COMP-5.
       77  WS-NETKEY   PIC  9(04)    COMP-5 VALUE 1.
       77  WS-KEYSTR   PIC  9(06)    COMP-5.
       77  WS-KSTORE   PIC  9(06)    COMP-5.
       77  WS-GLJSTR   PIC  9(04)    COMP-5.
       77  WS-GLJKEY   PIC  9(06)    COMP-5.
       77  WS-ISUB     PIC  9(04)    COMP-5.
       77  WS-PAGE     PIC  9(04)    COMP-5.
       77  WS-LINES    PIC  9(04)    COMP-5 VALUE 66.
       77  WS-SUB      PIC  9(04)    COMP-5.
       77  WS-S        PIC  9(04)    COMP-5.
       77  WS-S1       PIC  9(04)    COMP-5.
       77  WS-S2       PIC  9(04)    COMP-5.
       77  WS-S3       PIC  9(04)    COMP-5.
       77  WS-S4       PIC  9(04)    COMP-5.
       77  WS-S6       PIC  9(04)    COMP-5.
       77  WS-Y-END    PIC  9(02) VALUE ZERO.
       77  WS-L-END    PIC  9(04) VALUE ZERO.
       77  WS-OPEN     PIC  9(02) VALUE ZERO.
       77  WS-MS1      PIC  9(04)    COMP-5.
       77  WS-MS2      PIC  9(04)    COMP-5.
       77  WS-VALUE    PIC S9(09)V99 COMP-3.
       77  WS-REFER    PIC  9(07).
       77  WS-EXS      PIC  9(01).
       77  WS-CONREC   PIC  9(01) VALUE ZERO.
       77  WS-INDG     PIC  9(01) VALUE ZERO.
       77  WS-OPTION   PIC  X(01).
       77  WS-SKIP     PIC  X(01) VALUE "N".
       77  WS-FIRST    PIC  9(01) VALUE ZERO.
       77  WS-LAST     PIC  9(01) VALUE ZERO.
       77  WS-ERROR    PIC  9(01) VALUE ZERO.
       77  WS-NUM      PIC  Z9.
       77  WS-NORM     PIC  X(04).
       77  WS-COND     PIC  X(04).
       77  WS-EXPP     PIC  X(04).
       77  WS-ECAN     PIC  X(04).
       77  WS-8LPI     PIC  X(04).
       77  WS-6LPI     PIC  X(04).
       77  WS-10CPI    PIC  X(04).
       77  WS-12CPI    PIC  X(04).
       77  WS-17CPI    PIC  X(04).
       77  WS-DBLP     PIC  X(04).
       77  WS-DCAN     PIC  X(04).
       77  WS-BATCH    PIC  9(05)    COMP-3.
       77  WS-TNEXT    PIC  9(04)    COMP-5.
       77  WS-PERIOD   PIC  9(02)    COMP-5 VALUE ZERO.
       77  WS-H1       PIC  X(05) VALUE "DATE:".
       77  WS-H3       PIC  X(06) VALUE "BATCH:".
       77  WS-H4       PIC  X(05) VALUE "PAGE:".
       77  WS-ER1      PIC  X(07) VALUE "Account".
       77  WS-ER3      PIC  X(04) VALUE "Date".
       77  WS-ER4      PIC  X(05) VALUE "Value".
       77  WS-ER5      PIC  X(13) VALUE "Cheque Number".
       77  WS-ER6      PIC  X(09) VALUE "No Record".
       77  WS-ER7      PIC  X(13) VALUE "Group Heading".
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB     PIC  9(04) COMP-5.

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
           03  CRT-PROGRAM         PIC  X(15) VALUE "GLP072".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "GENERAL LEDGER -REVIEW CHEQUE BATCH".
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
           03  W75-SKEY       PIC 9(04) COMP-5
                              OCCURS 18.
       01  W80-H3.
           03  FILLER      PIC  X(13)      VALUE "BUDGET AS AT ".
           03  W80-DTE     PIC  99/99/99.
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
          03  LINE  3 COLUMN 26 FOREGROUND-COLOR 7 HIGHLIGHT
               VALUE "G/L (CHEQUES) - REVIEW A BATCH".
          03  LINE  5 COLUMN  4 VALUE "Bank G/L No:".
    03        COLUMN 17 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(06) FROM WS-SPC.
    03        COLUMN 24 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 3 HIGHLIGHT
     PIC  X(30) FROM WS-SPC.
          03        COLUMN 57 VALUE "Balance :".
    03        COLUMN 67 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 3 HIGHLIGHT
     PIC  X(13) FROM WS-SPC.
          03  LINE  6 COLUMN  4 VALUE "Date       :".
    03        COLUMN 17 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(08) FROM WS-SPC.
          03  LINE  7 COLUMN  4 VALUE "Cheque No  :".
    03        COLUMN 17 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(08) FROM WS-SPC.
          03  LINE  8 COLUMN  4 VALUE "Payee      :".
    03        COLUMN 17 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(36) FROM WS-SPC.
          03  LINE  9 COLUMN  4 VALUE "Narrative  :".
    03        COLUMN 17 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(36) FROM WS-SPC.
          03  LINE 11 COLUMN  4 VALUE "G/L Ac.No. :".
    03        COLUMN 17 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(06) FROM WS-SPC.
    03        COLUMN 24 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 3 HIGHLIGHT
     PIC  X(30) FROM WS-SPC.
          03        COLUMN 57 VALUE "Balance :".
    03        COLUMN 67 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 3 HIGHLIGHT
     PIC  X(13) FROM WS-SPC.
          03  LINE 12 COLUMN  4 VALUE "Narrative  :".
    03        COLUMN 17 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(36) FROM WS-SPC.
          03  LINE 13 COLUMN  4 VALUE "Value      :".
    03        COLUMN 17 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(13) FROM WS-SPC.
          03  LINE 15 COLUMN 46 VALUE "Net Amount Payable :".
          03        COLUMN 67 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(13) FROM WS-SPC.

       01  S05A.
           03  LINE 18 COLUMN 20 VALUE  "Totals".
          03        COLUMN 29 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(07)9.99CR USING W20-DEBIT AUTO.
          03        COLUMN 46 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(07)9.99CR USING W20-CREDIT AUTO.

       01  S06.
          03  LINE 3 COLUMN 25 FOREGROUND-COLOR 7 HIGHLIGHT
              VALUE "G/L (CHEQUES) - REVIEW A BATCH".
          03  LINE 5 COLUMN  3 VALUE "Batch Id:".
    03       COLUMN 13 FOREGROUND-COLOR 3 HIGHLIGHT
    BACKGROUND-COLOR 5
    PIC  X(14) FROM W02-GLCHEQ.
    03  LINE 6 COLUMN  3 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
        "Key in a one character batch reference code".

       01  S06A.
          03  LINE 5 COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5
    PIC  X(01) USING W02-BCH41 AUTO.

       01  S07.
          03  LINE 11 COLUMN  4 VALUE "G/L Ac.No. :".
    03        COLUMN 17 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(06) FROM WS-SPC.
    03        COLUMN 24 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 3 HIGHLIGHT
     PIC  X(30) FROM WS-SPC.
          03        COLUMN 57 VALUE "Balance :".
    03        COLUMN 67 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 3 HIGHLIGHT
     PIC  X(13) FROM WS-SPC.
          03  LINE 12 COLUMN  4 VALUE "Narrative  :".
    03        COLUMN 17 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(36) FROM WS-SPC.
          03  LINE 13 COLUMN  4 VALUE "Value      :".
    03        COLUMN 17 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(13) FROM WS-SPC.
          03  LINE 15 COLUMN 46 VALUE "Net Amount Payable :".
          03        COLUMN 67 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(13) FROM WS-SPC.

       01  S13.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z9/99/9999 USING W15-DATE AUTO.

       01  S14.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z(07)9.99CR USING W15-VAL AUTO.

       01  S16.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z(06)9 USING W15-REFER AUTO.

       01  S17.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC 9(06) USING W15-ACCOUNT AUTO.

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
          IF LS0-GLLEV < 1
        MOVE "Not Authorised"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO AA35.
            PERFORM ZA000.
      PERFORM CA000.

      CLOSE GLBTCH.
       AA35.
             EXIT PROGRAM.

       COPY "FUNCTION.SCR".

       COPY "ERROR.SCR".

       COPY "OPTION.CRT".

       COPY "LOCKED.REC".

       COPY "CLEAR.CRT".

       COPY "AA900.ALM".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AB000        SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "GACCNT.RD".

       COPY "GLCHEQ.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AC000           SECTION.

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

       COPY "GACCNT.WR".

       COPY "GLCHEQ.WR".

      /
       AE000           SECTION.
       AE00.
            MOVE ZERO   TO WS-ERROR.
           IF W15-DD < 01 OR > 31
               GO TO AE10.
           IF W15-MM < 01 OR > 12
               GO TO AE10.
           IF W15-MM = 01 OR 03 OR 05 OR 07 OR 08 OR 10 OR 12
               GO TO AE15.
           IF W15-DD > 30
               GO TO AE10.
           IF W15-MM = 02 AND W15-DD > 28
               GO TO AE05.
             GO TO AE15.

       AE05.
           IF W15-DD > 29
               GO TO AE10.
            DIVIDE W15-CY BY 4  GIVING W20-RESULT.
           IF W20-DEC = ZERO
               GO TO AE15.

       AE10.
            MOVE 1   TO WS-ERROR.
             GO TO AE999.
      *
      *   ****    CALCULATE THE PERIOD INTO WHICH THIS DATE FALLS
      *
       AE15.
      MOVE W15-MM  TO WS-PERIOD.
    IF W15-CY > W05-CY
        GO TO AE10.
    IF W15-CY < W05-CY
               GO TO AE20.
    IF W05-MM = W15-MM
       GO TO AE999.
    IF W15-MM > W05-MM
        GO TO AE10.
    IF W15-MM > WS-Y-END
               GO TO AE999.
    IF W15-CY > WS-L-END
               GO TO AE999.
      ADD 12   TO WS-PERIOD
             GO TO AE999.

       AE20.
    IF W15-CY > WS-L-END
               GO TO AE999.
      ADD 12   TO WS-PERIOD.

       AE999.
             EXIT.

      /
      *    ****    R E V I E W   B A T C H
      *
       CA000        SECTION 50.
       CA0.
      PERFORM ERASE-SCREEN.
      DISPLAY S06.
       CA0A.
      ACCEPT S06A.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO CA999
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO CA0A
        END-EVALUATE.
    IF W02-BCH41 = SPACE
        MOVE "1"   TO W02-BCH41
        GO TO CA0A.
      OPEN INPUT GLBTCH.
    IF NOT (WS-STATUS = "00")
        MOVE "File requested not on disk"
     TO WS-ERR-STRING
              PERFORM ERROR-LENGTH THRU ERROR-EXIT
        GO TO CA0A.
      CLOSE GLBTCH.
            OPEN I-O GLBTCH.
    IF NOT (WS-STATUS = "00")
        MOVE "Disk error or file corrupted"
     TO WS-ERR-STRING
              PERFORM ERROR-LENGTH THRU ERROR-EXIT
        GO TO CA0A.
       CA0B.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
           DISPLAY "Files locked - Try later" AT 5012
             WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
     " " WS-STATUS
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15
           DISPLAY "Press any key" AT 5012
             WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
           ACCEPT WS-OPTION AT 5026
     WITH BACKGROUND-COLOR 3
          FOREGROUND-COLOR 7 HIGHLIGHT AUTO
           GO TO CA999
               ELSE
               IF FLE-LIMIT
     DISPLAY "Too many files OPEN" AT 0812
       WITH FOREGROUND-COLOR 14
     DISPLAY "Check the FILES option in CONFIG.SYS"
       AT 1012
     DISPLAY "Press any key to continue" AT 1212
     ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15
           GO TO CA999
        ELSE
               IF IDX-CORRUPT
                   CLOSE GLBTCH
                   OPEN OUTPUT GLBTCH
                   CLOSE GLBTCH
                   OPEN I-O GLBTCH
     GO TO CA0B.
           IF WS-STATUS NOT = "00"
               MOVE 41           TO WS-F-ERROR
               PERFORM OPEN-ERROR.

       CA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S05.
             MOVE ZERO           TO W20-DEBIT W20-CREDIT.
            MOVE SPACES  TO W15-REF.
             INITIALIZE GLB-REC.
             PERFORM START-AT-GLBTCH-KEY THRU READ-GLBTCH-EXIT.
           IF WS-F-ERROR = 41
        MOVE "No batch outstanding"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO CA999.
      DISPLAY "Reading batch details" AT 5002
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.

       CA05.
             PERFORM READ-GLBTCH-NEXT THRU READ-GLBTCH-EXIT.
           IF WS-F-ERROR = 41
               GO TO CA10.
           IF GLB-VALUE < ZERO
               ADD GLB-VALUE     TO W20-CREDIT
           ELSE
               ADD GLB-VALUE     TO W20-DEBIT.
      DISPLAY S05A.
             GO TO CA05.

       CA10.
             INITIALIZE GLB-REC.
             PERFORM START-AT-GLBTCH-KEY THRU READ-GLBTCH-EXIT.
      MOVE ZERO   TO W15-REFER.
      MOVE 1   TO WS-LAST.
            MOVE ZERO   TO W20-DEBIT2 W20-CREDIT2.

       CA15.
      PERFORM READ-GLBTCH-NEXT THRU READ-GLBTCH-EXIT.
           IF WS-F-ERROR = 41
        MOVE ZERO  TO WS-REFER
        GO TO CA25.
    IF NOT (W15-REFER = ZERO)
        IF NOT (GLB-REF = W15-REFER)
    MOVE GLB-REFER TO WS-REFER
    GO TO CA25.

       CA20.
    IF W15-REFER = ZERO
        MOVE GLB-REF  TO W15-REFER
        MOVE GLB-DATE  TO W15-YMD
        MOVE W15-YR  TO W15-YY
        MOVE W15-MT  TO W15-MM
        MOVE W15-DY  TO W15-DD
        MOVE GLB-AC  TO WS-KSTORE W15-ACCOUNT
        MOVE GLB-NAR  TO W15-NAR
        MOVE GLB-PAY  TO W15-PAYEE
        MOVE GLB-VALUE  TO W20-CREDIT
        GO TO CA15.
      MOVE GLB-AC  TO GLJ-AC.
      MOVE GLB-NAR  TO GLJ-NAR.
      MOVE GLB-VALUE  TO GLJ-VAL.
      MOVE WS-LAST  TO WS-GLJKEY.
            PERFORM WRITE-GLJRN THRU WRITE-GLJRN-EXIT.
      ADD 1   TO WS-LAST.
      GO TO CA15.

       CA25.
            DISPLAY S05A.
            DISPLAY S17 AT 0517.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
              MOVE "G/L Account file corrupted"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
              GO TO      BA15.
            DISPLAY G-NAME AT 0524
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
            MOVE G-BAL   TO W25-V1.
            DISPLAY W25-S7V2 AT 0567
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      MOVE 1   TO WS-FIRST.

       CA3O.
             PERFORM READ-GLJRN THRU READ-GLJRN-EXIT.
            MOVE GLJ-AC  TO W15-ACCOUNT.
      MOVE GLJ-NAR  TO GLB-NAR2.
            MOVE GLJ-VAL  TO W15-VAL.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
              MOVE "G/L Account file corrupted"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
              GO TO         BA60.
            DISPLAY G-NAME AT 1124
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
            MOVE G-BAL   TO W25-V1.
            DISPLAY W25-S7V2 AT 1167
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
            DISPLAY W15-NAR2 AT 1217
            WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
            DISPLAY S14 AT 1317.

       CA55.
      MOVE " ,'A'mend,'D'elete,'N'ext cheque or 'Esc'ape"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      MOVE 17   TO SLIN
      PERFORM OPT-SETUP THRU OPT-REPLY.
      PERFORM CLEAR-L50.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM OPT-END.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO CA999
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO CA55
        END-EVALUATE
    ELSE
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
     IF WS-FIRST = 1
         GO TO CA55
     ELSE
         GO TO CA60
     END-IF
   WHEN DOWN-KEY
     IF WS-LAST = 1
         GO TO CA55
     ELSE
         GO TO CA15
     END-IF
        END-EVALUATE
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-OPTION = "A" OR "D")
        GO TO CA55.
    IF WS-OPTION = "D"
        GO TO CA65.
      GO TO CA70.
       CA60.
             PERFORM READ-GLBTCH-PREV THRU READ-GLBTCH-EXIT.
           IF WS-F-ERROR = 41
               MOVE 1 TO WS-FIRST
        GO TO CA15.
             MOVE ZERO           TO WS-LAST.
      GO TO       CA18.
       CA65.
           IF GLB-VALUE < ZERO
               SUBTRACT GLB-VALUE
                                 FROM W20-CREDIT
           ELSE
               SUBTRACT GLB-VALUE
                                 FROM W20-DEBIT.
             PERFORM DELETE-GLBTCH-REC THRU WRITE-GLBTCH-EXIT.
           IF WS-FIRST = 1
               GO TO CA15.
      GO TO CA60.
       CA70.
           IF GLB-VALUE < ZERO
               SUBTRACT GLB-VALUE
                                 FROM W20-CREDIT
           ELSE
               SUBTRACT GLB-VALUE
                                 FROM W20-DEBIT.
       CA75.
            ACCEPT S13 AT 0710.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO CA95
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO CA75
        END-EVALUATE.
             PERFORM AE000.
           IF WS-ERROR = 1
              MOVE WS-ER3  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
              GO TO CA75.
       CA80.
            ACCEPT S16 AT 0721.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO CA95
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO CA80
        END-EVALUATE.
       CA85.
            ACCEPT W15-NAR AT 0730
            WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO CA75
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO CA85
        END-EVALUATE.
            DISPLAY "Keyin (MINUS) - and Value for Credit" AT 0943
                      WITH FOREGROUND-COLOR 14.
       CA90.
             ACCEPT S14 AT 0767.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO CA85
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO CA90
        END-EVALUATE.
       CA95.
             MOVE W15-YY         TO W15-YR.
             MOVE W15-MM         TO W15-MT.
             MOVE W15-DD         TO W15-DY.
             MOVE W15-YMD        TO GLB-DATE.
      MOVE W15-REF  TO GLB-REFER.
             MOVE W15-NAR        TO GLB-NAR.
             MOVE W15-VAL        TO GLB-VALUE.
           IF W15-VAL < ZERO
               ADD W15-VAL       TO W20-CREDIT
           ELSE
               ADD W15-VAL       TO W20-DEBIT.
             PERFORM REWRITE-GLBTCH THRU WRITE-GLBTCH-EXIT.
      GO TO CA55.
       CA999.
             EXIT.
      /
       ZA000      SECTION 90.
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
             MOVE 5              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-VAT-RATES  TO W05-VAT-RATES.
             MOVE 6              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-SUSGL  TO W70-SUSGL.
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
             MOVE ZERO TO WS-ERROR.
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
       ZB000      SECTION 91.

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
               MOVE WS-GLKEY TO WS-KEY
          ELSE
           IF WS-F-ERROR = 15
               MOVE WS-PARID TO WS-FILE
               MOVE WS-PARKEY TO WS-KEY
          ELSE
           IF WS-F-ERROR = 41
               MOVE W02-GLBTCH   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE GLB-KEY      TO WS-KEYX..
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
