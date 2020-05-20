      $set LINKCOUNT"256" GNT"GLPEXPT.GNT"
      ******************************************************************
      *                                                                *
      *  ******  **   *******  ******* **   ** *******  ******** *
      * **    ** **   ** ** **     **  **  **    **  **    *
      * **  **   ** ** **      ** **   **    **  **    *
      * **  **   *******  *****      ***    *******  **    *
      * **   *** **   **    **      ** **   **   **    *
      * **    ** **   **    **     **  **  **   **    *
      *  ******  ******** **    ******* **   ** **   **    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     G E N E R A L   L E D G E R   E X P O R T   P R O G R A M  *
      *                                                                *
      *       Version 9.04.02 - June 2017                              *
      *               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     GLPEXPT.
       AUTHOR.        James William Lemmon.
       DATE-WRITTEN.   FEBRUARY 2006.

                COPYRIGHT NOTICE: COPYRIGHT (C) 2006 - 2017
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
                         CONSOLE IS CRT.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY "PARAM.SL".

       COPY "GACCNT.SL".

       COPY "GLTRAN.SL".

          SELECT GTRDET  ASSIGN W02-GTRDET
                          STATUS WS-STATUS
                          ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       COPY "PARAM.FDE".

       COPY "GACCNT.FDE".

       COPY "GLTRAN.FDE".

       FD  GTRDET    LABEL RECORD OMITTED.
       01  EXP-REC1.
    03  EXP-DATA     PIC  X(256).

      /
       WORKING-STORAGE SECTION.
       77  WS-CHECK    PIC X(18)  VALUE
      "aeWlimemnomLalismJ".
       77  WS-SUB     PIC 9(04)   COMP-5.
       77  WS-S1            PIC 9(04)     COMP-5.
       77  WS-S2            PIC 9(04)     COMP-5.
       77  WS-S3            PIC 9(04)     COMP-5.
       77  WS-S4            PIC 9(04)     COMP-5.
       77  WS-S5            PIC 9(04)     COMP-5.
       77  WS-S6            PIC 9(04)     COMP-5.
       77  WS-S7            PIC 9(04)     COMP-5.
       77  WS-S8            PIC 9(04)     COMP-5.
       77  WS-IXS           PIC 9(04)     COMP-5.
       77  WS-IXS1          PIC 9(04)     COMP-5.
       77  WS-IXS2          PIC 9(04)     COMP-5.
       77  WS-IXS3          PIC 9(04)     COMP-5.
       77  WS-IXS4          PIC 9(04)     COMP-5.
       77  WS-GLKEY     PIC 9(06)   COMP-5.
       77  WS-PARKEY     PIC 9(06)   COMP-5.
       77  WS-RECOVER     PIC 9(06)   COMP-5.
       77  WS-MONTHS     PIC S9(01)V99 COMP-3.
       77  WS-KEY1          PIC 9(04)     COMP-5.
       77  WS-KSTORE        PIC 9(04)     COMP-5.
       77  WS-OPEN         PIC  9(02) VALUE ZERO.
       77  WS-ETYPE     PIC X(01).
       77  WS-VAT-GST       PIC X(01).
       77  WS-OPTION     PIC X(01).
       77  WS-ADJ           PIC X(01).
       77  WS-TRAN     PIC  X(05).
       77  WS-ERROR         PIC 9(01).
       77  WS-IND           PIC 9(01)     VALUE 0.
       77  WS-INDB          PIC 9(01)     VALUE 0.
       77  WS-INDP          PIC 9(01)     VALUE 0.
       77  WS-INDG     PIC 9(01)   VALUE 0.
       77  WS-AMEND         PIC 9(01)     VALUE 0.
       77  WS-CONREC        PIC 9(01) VALUE ZERO.
       77  WS-COMP          PIC 9(01).
       77  WS-WEEK          PIC 9(01).
       77  WS-TYPE          PIC X(01).
       77  WS-BATCH         PIC 9(05).
       77  WS-PASSWORD      PIC X(08).
       77  WS-PRINT         PIC 9(01).
       77  WS-PASS          PIC X(08).
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "GLPEXPT".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "GENERAL LEDGER -EXPORT TRANSACTIONS".
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

       01  W02-FILE-IDS.
    03  W02-GTRDET.
        05  W02-EXPORT PIC X(64) VALUE "GLTRANS.TXT".

       01  W10-WORK.
          03  W10-DATE.
        05  W10-EDTE  PIC  99/99/9999.

       COPY "W12.WS".

       COPY "W15.WS".

       01  W15-RECORD1.
          03  W15-ACCOUNT PIC 9(06).
           03  W15-ACNO1   REDEFINES W15-ACCOUNT.
               05  W15-ACNO
                           PIC 9(04).
               05  W15-SER PIC  9(02).
           03  W15-ACNO2   REDEFINES W15-ACCOUNT.
               05  W15-GR  PIC 9(02).
               05  W15-SGRP
                           PIC 9(02).
               05  W15-ASER
                           PIC 9(02).
           03  W15-ACNO3   REDEFINES W15-ACCOUNT.
               05  W15-GROUP
                           PIC 9(02).
               05  W15-REST
                           PIC 9(04).
           03  W15-ACNO4   REDEFINES W15-ACCOUNT.
               05  W15-GRP PIC  9(01).
               05  FILLER  PIC  9(05).

       COPY "W20.WS".

       01  W25-CALCS.
          03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
              05  W25-WHOLE   PIC 9(05).
               05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
        05  W25-KEY PIC 9(04).
        05  W25-SUB PIC 9(01).
        05  FILLER  PIC 9(02).
          03  W25-SCOST   PIC S9(09)V99 COMP-3.
          03  W25-PCOST   PIC S9(09)V99 COMP-3.
          03  W25-PCST    PIC S9(09)V99 COMP-3.
          03  W25-PTOT    PIC S9(09)V99 COMP-3.
          03  W25-TSELL   PIC S9(09)V99 COMP-3.
          03  W25-TOTAL   PIC S9(09)V99 COMP-3.
          03  W25-VALUE   PIC S9(09)V99 COMP-3.
          03  W25-ADJUST  PIC S9(09)V99 COMP-3.

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

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W55-EXPORT.
           03  W55-S1          PIC  9(04) COMP-5.
           03  W55-EXP-DATA.
        05  W55-EXP     PIC  X(01) OCCURS 256.

       01  W55-HEADING.
    03  FILLER       PIC  X(22) VALUE "Exporting for period:".
    03  W55-START      PIC  99/99/9999.
    03  FILLER       PIC  X(03) VALUE " - ".
    03  W55-END       PIC  99/99/9999.

       01  W60-FIELDS.
           03  W60-S           PIC  9(02) COMP-5.
          03  W60-FNO        PIC  9(02) COMP-5 OCCURS 8.

       01  W65-FIELDS.
           03  W65-S           PIC  9(02) COMP-5.
    03  W65-FIELD       PIC  X(18).
           03  W65-FLDS.
              05  FILLER      PIC X(17) VALUE "Account Number".
              05  FILLER      PIC X(17) VALUE "Date".
              05  FILLER      PIC X(17) VALUE "Transaction Type".
              05  FILLER      PIC X(17) VALUE "Reference".
              05  FILLER      PIC X(17) VALUE "Narrative/Remarks".
              05  FILLER      PIC X(17) VALUE "Value".
              05  FILLER      PIC X(17) VALUE "Batch Number".
    03  W65-RFLDS REDEFINES W65-FLDS.
        05  W65-FLD     PIC  X(17) OCCURS 8.

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
    03  W75-GNO       PIC 9(06) COMP-5
                              OCCURS 18.

       01  W80-EDIT.
           03  W80-CNO        PIC 9(04).
           03  W80-V1.
               05  W80-S7V2   PIC Z(06)9.99-.
           03  W80-V2.
               05  W80-S5V2   PIC Z(04)9.99-.
           03  W80-DTE.
              05  W80-DATE   PIC Z9/99/9999.
           03  W80-QNT.
               05  W80-5      PIC Z(04)9.
           03  W80-QNT1 REDEFINES W80-QNT.
               05  W80-3V2    PIC Z(02)9.9999.
           03  W80-NO.
               05  W80-5N     PIC 9(05).
               05  W80-REC REDEFINES W80-5N
                              PIC ZZZZ9.
           03  W80-DEL.
               05  W80-2N      PIC 9(02).

       01  W90-BAL        PIC S9(07)V99 COMP-3.

       01  W95-COMP        PIC X(40).

       01  W100-EDIT.
           03  W100-1.
               05  W100-N      PIC  9(01).
           03  W100-2.
               05  W100-N2     PIC  9(02).
           03  W100-3.
               05  W100-NO1    PIC  9(03).
           03  W100-9.
               05  W100-NO2    PIC  Z(08)9.
           03  W100-S9.
        05  W100-NO3    PIC  -(09)9.
           03  W100-S9M REDEFINES W100-S9.
        05  W100-BUD    PIC  -(09)9.
           03  W100-PRICE.
               05  W100-S6V2   PIC  Z(05)9.99.
           03  W100-S9V2.
        05  W100-V1     PIC  -(09)9.99.
           03  W100-9V2.
               05  W100-V2     PIC  Z(08)9.99.
           03  W100-9V3.
               05  W100-V3     PIC  Z(08)9.999.
           03  W100-3V2.
               05  W100-V4     PIC  Z(02)9.99.
           03  W100-DATE.
        05  W100-DTE    PIC  9(08).

       LINKAGE SECTION.

       COPY "CHAIN.LS".

       COPY "FILE-GENL.IDS".

      /
       SCREEN SECTION.

       01  MENU-INSTRUCT.
    03  LINE 25 COLUMN  2 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 1
     VALUE "Use ".
    03        COLUMN  6 HIGHLIGHT
     BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6
     VALUE "".
    03        COLUMN  7 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 1
     VALUE " or ".
    03        COLUMN 11 HIGHLIGHT
     BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6
     VALUE "".
    03        COLUMN 12 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 1
     VALUE " to point to an item, and ".
    03        COLUMN 38 HIGHLIGHT
     BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6
     VALUE "<Enter>".
    03        COLUMN 45 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 1
     VALUE " to Select ".
    03        COLUMN 56 HIGHLIGHT
     BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6
     VALUE "<Esc>".
    03        COLUMN 61 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 1
     VALUE " to Exit".

       01  S-PROG.
          03  LINE  2 COLUMN 30 FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "GL-TRAN EXPORT PROGRAM".

       01  S-MENU.
    03  LINE  6 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
    03  LINE  7 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE "³    ".
    03        COLUMN 34 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "EXPORT OPTIONS".
    03        COLUMN 48 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE "    ³".
    03  LINE  8 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "1".
    03        COLUMN 32 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE " All Fields      ³".
    03  LINE  9 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "2".
    03        COLUMN 32 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE " Field Separator    ³".
          03  LINE 10 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "0".
    03        COLUMN 32 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE " Exit               ³".
    03  LINE 11 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S04.
           03  LINE  4 COLUMN  2 VALUE "Export all the fields".

       01  S05.
           03  LINE  4 COLUMN  2 VALUE "Select the fields required".
           03  LINE  6 COLUMN  2 VALUE "Field :".

       01  S05D.
    03  LINE  4 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 6 VALUE
    "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
    03  LINE  5 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 6 VALUE
    "³    Transactions for Period.    ³".
    03  LINE  6 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 6 VALUE
    "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
    03  LINE  7 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 6 VALUE
    "³   Starting Date :              ³".
    03  LINE  8 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 6 VALUE
    "³   Ending Date   :              ³".
    03  LINE  9 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 6 VALUE
    "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
    03  LINE 10 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 6 VALUE
    "³Dates to be keyed in as ".
    03        COLUMN 49 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 6 VALUE "DDMMCCYY".
    03        COLUMN 57 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 6 VALUE "³".
    03  LINE 11 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 6 VALUE
    "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S05A.
    03  LINE  7 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC 99/99/9999 USING W15-START AUTO.
    03  LINE  8 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC 99/99/9999 USING W15-END AUTO.

       01  S06.
           03  LINE  8 COLUMN  2 VALUE "Fields:".
    03        COLUMN 10 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (1).
    03        COLUMN 13 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (2).
    03        COLUMN 16 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (3).
    03        COLUMN 19 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (4).
    03        COLUMN 22 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (5).
    03        COLUMN 25 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (6).
    03        COLUMN 28 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (7).

       01  S07.
           03  LINE 16 COLUMN  2 VALUE "Exporting:".

       01  S08.
    03  LINE 16 COLUMN 13 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(06) FROM T-AC.
    03        COLUMN 20 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(08) FROM T-DATE.
       01  S09.
    03  LINE  6 COLUMN 10 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC 99 FROM W65-S.
    03        COLUMN 13 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(17) FROM W65-FIELD.

       01  S10.
    03  LINE  4 COLUMN  2 FOREGROUND-COLOR 3 HIGHLIGHT
     VALUE "Change Field Separator".
    03  LINE  6 COLUMN  2 VALUE "Character Used:".

       COPY "LEDLUP.CRT".

      /
       PROCEDURE DIVISION
   USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000-MAIN        SECTION.
       AA000-INIT.
          IF LS0-GLLEV < 9
        MOVE "Not Authorised"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO AA99.
            PERFORM ZA000-INIT.
           IF WS-STATUS NOT = "00"
               PERFORM OPEN-ERROR.
      PERFORM BA000.
             GO TO AZ000-EOJ.

       AA50.
            PERFORM SAVE-SCREEN.
             MOVE 4              TO W44-FUNCTION.
            PERFORM SCREEN-CONTENTS.
      PERFORM GACCNT-LOOKUP.
            PERFORM RESTORE-SCREEN.

       AA99.
             EXIT.

      /    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *     S C R E E N ,   K E Y B O A R D   &  M O U S E
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3      ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³     SCREEN-SHADOW          ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To routine is used to display a shadow down the right and ³
      *    ³ along the bottom of a pop-up box. The parameters that are ³
      *    ³ required:             ³
      *    ³        SHADE-ROW   - Top line of the box + 1.        ³
      *    ³        SHADE-COL   - Left line of box + 2.        ³
      *    ³        SHADE-WIDTH - Width of the box - 2.        ³
      *    ³        SHADE-LINES - Hight of box - 1.         ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³     CHECK-CORRECT          ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine displays a pop-up window with the message    ³
      *    ³       "Correct ENTER - N if incorrect"         ³
      *    ³              ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³              ³
      *    ³ To start with the pop-up window higher or lower than row  ³
      *    ³ 13 (default); move another value to SLIN and PERFORM      ³
      *    ³ CHECK-SETUP THRU CHECK-EXIT.          ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³     ERROR-MESSAGE          ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To display an error message with " - Press ANY key" at    ³
      *    ³ the end; use PERFORM ERROR-MESSAGE.         ³
      *    ³              ³
      *    ³ To display just the error message; use PERFORM        ³
      *    ³ ERROR-LENGTH THRU ERROR-EXIT.          ³
      *    ³              ³
      *    ³ To display the error message higher or lower (default is  ³
      *    ³ line 13) firstly use PERFORM ERROR-SETUP THRU ERROR-POS   ³
      *    ³ or PERFORM ERROR-LENGTH THRU ERROR-POS. Move the line     ³
      *    ³ number to be used to SLIN and then PERFORM ERROR-DISPLAY  ³
      *    ³ THRU ERROR-EXIT.            ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.CRT".

       COPY "LOCKED.RC2".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     OPT-MESSAGE          ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine is used to allow the OPERATOR to respond to  ³
      *    ³ a request for an option, so that the correct action can   ³
      *    ³ be performed by the program. The routine will display the ³
      *    ³ message in a pop-up window and allow the OPERATOR to      ³
      *    ³ respond to the 'question'.           ³
      *    ³              ³
      *    ³ The option request must be placed in WS-ERR-MES and may   ³
      *    ³ not exceed 48 characters. The message will be centred in  ³
      *    ³ the window. An example of a message request follows:      ³
      *    ³              ³
      *    ³   MOVE "Print transactions (Y/N) [ ]" TO WS-ERR-MES.      ³
      *    ³   PERFORM OPT-MESSAGE.           ³
      *    ³              ³
      *    ³ This would be displayed as:          ³
      *    ³ ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿     ³
      *    ³ ³    Print transactions (Y/N) [ ]   ³°°   ³
      *    ³ ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°   ³
      *    ³   °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°   ³
      *    ³              ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³              ³
      *    ³ To display the request message higher or lower (default   ³
      *    ³ is line 13) move the line number to be used to SLIN and   ³
      *    ³ then PEFORM OPT-SETUP THRU OPT-EXIT.         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "OPTION.CRT".

       COPY "CLEAR.CRT".

       COPY "DATE.CHK".

      *
      *    ****    G E T   T H E   E X P O R T   F I L E   I D
      *
       AA100           SECTION.
       AA101.
      DISPLAY "Keyin the <" AT 2202
       "pathname" WITH FOREGROUND-COLOR 6 HIGHLIGHT ">/<"
       "filename" WITH FOREGROUND-COLOR 6 HIGHLIGHT ">".
             DISPLAY "EXPORT file :" AT 2302.
             MOVE ZERO           TO WS-ERROR.
       AA105.
      ACCEPT W02-EXPORT AT 2316
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE 9  TO WS-ERROR
       GO TO AA199
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO AA105
        END-EVALUATE.
      MOVE 64   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W02-EXPORT
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
             DISPLAY W02-EXPORT AT 2316
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO AA105.
      PERFORM CLEAR-L50.
      OPEN OUTPUT GTRDET.
           IF WS-STATUS = "00"
        PERFORM AA600
               GO TO AA199.
      MOVE "Invalid file name"
     TO WS-ERR-STRING
            PERFORM ERROR-MESSAGE.
             GO TO AA105.
       AA199.
             EXIT.
      *
      *    ****    M O V E   F I E L D   T O   E X P O R T   A R E A
      *
      *            W50-DATA   : CONTAINS DATA TO BE INSERTED
      *            W50-LENGTH : LENGTH OF DATA
      *            W50-EXP    : FIELD TERMINATION CHARACTER
      *
       AA200           SECTION.
       AA201.
           IF W50-DATA = SPACES
               GO TO AA225.
             MOVE W50-LENGTH     TO W50-S1.
      *
      *    ****    D E T E R M I N E   E N D   O F   F I E L D
      *
      *            THIS LOOP EXCLUDES THE TRAILING SPACES
      *
       AA205.
           IF W50-CCHAR (W50-S1) = SPACE
               SUBTRACT 1        FROM W50-S1
               GO TO AA205.
      *
      *    ****    R E M O V E   L E A D I N G   S P A C E S
      *
       AA210.
           IF W50-1 = SPACE
               MOVE W50-D39      TO W50-DATA
               SUBTRACT 1        FROM W50-S1
               GO TO AA210.
             MOVE W50-S1         TO W50-S2.
             MOVE 1              TO W50-S1.
       AA220.
             MOVE W50-CCHAR (W50-S1)
                                 TO W55-EXP (W55-S1).
             ADD 1               TO W50-S1
                                    W55-S1.
           IF W50-S1 NOT > W50-S2
               GO TO AA220.
       AA225.
             MOVE W50-EXP        TO W55-EXP (W55-S1).
             ADD 1               TO W55-S1.
       AA299.
             EXIT.
      *
      *    ****    M O V E   I N D V I D U A L   F I E L D S
      *
      *            W50-BIN : FIELD NUMBER TO MOVE
      *
       AA300              SECTION.
       AA301.
    IF W50-BIN < 01 OR > 07
               GO TO AA399.
      *
           IF W50-BIN = 01
        MOVE T-AC  TO W50-DATA
        MOVE 6   TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 02
        IF T-DATE = ZERO
     MOVE SPACES  TO W50-DATA
        ELSE
     MOVE T-DATE  TO W100-DTE
     MOVE W100-DATE
     TO W50-DATA
        END-IF
        MOVE 8   TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 03
        MOVE WS-TRAN  TO W50-DATA
        MOVE 5   TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
    IF W50-BIN = 04
        MOVE T-REFER  TO W50-DATA
        MOVE 7   TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 05
        MOVE T-NAR  TO W50-DATA
        MOVE 40   TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 06
        IF T-VAL = ZERO
     MOVE SPACES  TO W50-DATA
        ELSE
     MOVE T-VAL  TO W100-V1
     MOVE W100-S9V2
     TO W50-DATA
        END-IF
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
               GO TO AA399.
      *
    IF W50-BIN = 07
        IF T-BATCH = ZERO
     MOVE SPACES  TO W50-DATA
        ELSE
     MOVE T-BATCH  TO W100-NO2
     MOVE W100-9  TO W50-DATA
        END-IF
        MOVE 9   TO W50-LENGTH
               PERFORM AA200
        GO TO AA399.

       AA399.
               EXIT.
      *
      *    ****    P R O C E S S   T H E   S E L E C T E D   F I E L D S
      *
       AA400           SECTION.
       AA401.
             MOVE 1              TO W60-S.
      *
      *    ****    C L E A R   E X P O R T   D A T A   A R E A
      *
             MOVE SPACES         TO W55-EXP-DATA.
      MOVE 1   TO W55-S1.

       AA405.
           IF W60-FNO (W60-S) = ZERO
               GO TO AA499.
             MOVE W60-FNO (W60-S)
                                 TO W50-BIN.
             PERFORM AA300.
    IF W60-S < 7
               ADD 1             TO W60-S
        GO TO AA405.

       AA499.
             EXIT.
      *
      *    ****    S E C U R I T Y   V I O L A T I O N
      *
       AA500              SECTION.
       AA501.
             DISPLAY "Insufficient security" AT 0412
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Press ENTER to continue" AT 0612
                      WITH FOREGROUND-COLOR 14.
            ACCEPT WS-OPTION AT 0636
      WITH FOREGROUND-COLOR 15 BACKGROUND-COLOR 5 AUTO.
       AA505.
             EXIT.
      *
      *    ****   H E A D E R S
      *
       AA600    SECTION.
       AA601.
      MOVE 1   TO W60-S.
      *
      *    ****    C L E A R   E X P O R T   D A T A   A R E A
      *
             MOVE SPACES         TO W55-EXP-DATA.
      MOVE 1   TO W55-S1.

       AA605.
    IF W60-FNO(W60-S) = ZERO
        MOVE W55-EXP-DATA TO EXP-REC1
        PERFORM WRITE-GTRDET THRU WRITE-GTRDET-EXIT
        GO TO AA699.
      MOVE W60-FNO(W60-S) TO W50-BIN.
      MOVE 21   TO W50-LENGTH.
      MOVE W65-FLD(W50-BIN)
     TO W50-DATA.
      PERFORM AA200.
    IF W60-S < 8
               ADD 1             TO W60-S
        GO TO AA605.

       AA699.
             EXIT.
      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S
      *
       AA700              SECTION.
       AA701.
      DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       " to do Account Lookup - "
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "ape to exit"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
       AA705.
             EXIT.

       COPY "AA900.ALM".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000-READ              SECTION.

       COPY "PARAM.RD".

       COPY "GACCNT.RD".

       COPY "GLTRAN.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AD000-WRITE             SECTION.

       WRITE-GTRDET.
             WRITE EXP-REC1.
           IF WS-STATUS NOT = "00"
               MOVE 99           TO WS-F-ERROR
               PERFORM WRITE-ERROR.
      
       WRITE-GTRDET-EXIT.
             EXIT.

       COPY "GACCNT.LUP".

      /
       AZ000-END               SECTION.
       AZ000-EOJ.
      MOVE ZERO   TO WS-S1.

       AZ010.
             EXIT PROGRAM.
      /
      *    ****    P R O G R A M   M E N U / C O N T R O L
      *
       BA000           SECTION.
       BA00.
    IF MOUSE-ATTACHED = "Y"
        MOVE 66   TO MOUSE-FUNC
        MOVE 1   TO MOUSE-PARAM
        CALL X"AF" USING MOUSE-FUNC
    MOUSE-PARAM.
      PERFORM ERASE-SCREEN.
      DISPLAY S-PROG.
      MOVE 7   TO SHADE-ROW.
      MOVE 31   TO SHADE-COL.
      MOVE 22   TO SHADE-WIDTH.
      MOVE 5   TO SHADE-LINES.
      DISPLAY S-MENU.
    IF LS0-LEV < 3
        DISPLAY "(N/A)" AT 1049
   WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
      DISPLAY MENU-INSTRUCT.
      PERFORM SCREEN-SHADOW.
       BA02.
      MOVE 7   TO TOP-ROW.
      MOVE 9   TO BOTTOM-ROW.
      MOVE 6   TO SCREEN-LIN.
      MOVE 31   TO SCREEN-COL.
      MOVE 20   TO STRING-LENGTH.
      PERFORM SAVE-ATTR.

       BA05.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BA10.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY  PERFORM MOVE-BLOCK-UP
   WHEN DOWN-KEY  PERFORM MOVE-BLOCK-DOWN
   WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
     IF MOUSE-ENTER = "Y"
         MOVE "N"  TO MOUSE-ENTER
         GO TO BA15
     END-IF
     IF MOUSE-Y = 24
         IF MOUSE-X = 5
      MOVE 5  TO KEY-CODE-1
      GO TO BA10
         ELSE
         IF MOUSE-X = 10
      MOVE 6  TO KEY-CODE-1
      GO TO BA10
         ELSE
         IF MOUSE-X > 36 AND < 44
      MOVE 0  TO KEY-CODE-1
      GO TO BA10
         ELSE
         IF MOUSE-X > 54 AND < 60
      MOVE 0  TO KEY-CODE-1
      MOVE 1  TO KEY-TYPE
      GO TO BA10
         END-IF
     END-IF
   WHEN ENTER-KEY  GO TO BA15
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO BA05
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE "0"  TO WS-OPTION
       GO TO BA18
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO BA05
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION
        IF WS-OPTION < 0 OR > 2
     PERFORM AA900-ALARM
     GO TO BA05
        END-IF
        GO TO BA16
    ELSE
        PERFORM AA900-ALARM
        GO TO BA05.

       BA15.
    IF SCREEN-LIN < 7
        PERFORM AA900-ALARM
        GO TO BA05
    ELSE
    IF SCREEN-LIN = 7
        MOVE "1"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 8
        MOVE "2"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 9
        MOVE "0"   TO WS-OPTION.
    GO TO BA18.

       BA16.
      PERFORM CLEAR-BLOCK.
    IF WS-OPTION = "0"
        MOVE 10   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "1"
        MOVE 7   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "2"
        MOVE 8   TO SCREEN-LIN.
      PERFORM SAVE-ATTR.
      PERFORM MARK-BLOCK.

       BA18.
    IF MOUSE-ATTACHED = "Y"
        MOVE 66   TO MOUSE-FUNC
        MOVE 0   TO MOUSE-PARAM
        CALL X"AF" USING MOUSE-FUNC
    MOUSE-PARAM.
    IF WS-OPTION = "0"
        GO TO BA999.
    IF WS-OPTION = "1"
        PERFORM DA000
    ELSE
    IF WS-OPTION = "2"
        PERFORM GA000.
      GO TO BA00.
       BA999.
      EXIT.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Get the period for which transactions are to be exported. ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       CA000        SECTION.
       CA00.
      PERFORM SAVE-SCREEN-3.
      MOVE 5   TO SHADE-ROW.
      MOVE 26   TO SHADE-COL.
      MOVE 32   TO SHADE-WIDTH.
      MOVE 7   TO SHADE-LINES.
      DISPLAY S05D.
      PERFORM SCREEN-SHADOW.
      MOVE W12-TODAY  TO W15-START W15-END.

       CA05.
      DISPLAY "Press " AT 5052
        WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
       "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
    BACKGROUND-COLOR 3
       "ape to Exit"
        WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
      ACCEPT S05A.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE 9  TO WS-ERROR
       GO TO CA999
   WHEN OTHER  PERFORM AA900-ALARM
       GO TO CA05
        END-EVALUATE.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO CA05.
      PERFORM CLEAR-L50.
      MOVE W15-START  TO W10-EDTE W55-START.
             PERFORM CHECK-DATE.
           IF WS-ERROR = 1
              MOVE "Start date" TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO CA05.
             MOVE W20-DTE        TO W15-S-YMD.
      MOVE W15-END  TO W10-EDTE W55-END.
             PERFORM CHECK-DATE.
           IF WS-ERROR = 1
              MOVE "End date"  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO CA05.
             MOVE W20-DTE        TO W15-E-YMD.
    IF W15-E-YMD < W15-S-YMD
        MOVE "End date is before start date"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO CA05.
      PERFORM RESTORE-SCREEN-3.

       CA999.
      EXIT.

      /
      *    ****    E X P O R T :
      *
      *         A L L   T H E   F I E L D S
      *
       DA000        SECTION 50.
       DA00.
             PERFORM ERASE-SCREEN.
      DISPLAY S-PROG.
      DISPLAY S04.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO DA999.
      INITIALIZE W60-FIELDS.
             MOVE 1              TO W60-S.

       DA01.
      MOVE W60-S   TO W60-FNO(W60-S).
    IF W60-S < 7
               ADD 1             TO W60-S
        GO TO DA01.
             DISPLAY S06.
             PERFORM AA100.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Get the period for which transactions are to be exported. ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      PERFORM CA000.
    IF WS-ERROR = 9
        GO TO DA999.

       DA02.
      DISPLAY S07.
      MOVE 100000  TO T-AC.
      MOVE W15-S-YMD  TO T-DATE
      PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
    IF WS-F-ERROR =11
        GO TO DA900.

       DA05.
      PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
          IF WS-F-ERROR =11
        GO TO DA900.
    IF T-DATE < W15-S-YMD OR
       T-DATE > W15-E-YMD
        GO TO DA05.
    IF T-SUB = 5
       MOVE ZERO   TO T-DATE T-CODE T-VAL T-BATCH
       MOVE "Payee"  TO WS-TRAN.
    IF T-CODE = 01 OR 02
        MOVE "Jnl"  TO WS-TRAN
    ELSE
    IF T-CODE = 03 OR 04
        MOVE "Chq"  TO WS-TRAN
    ELSE
    IF T-CODE = 05 OR 10
        MOVE "Bnk"  TO WS-TRAN
    ELSE
    IF T-CODE = 06 OR 07
        MOVE "Dep"  TO WS-TRAN
    ELSE
    IF T-CODE = 08 OR 09
        MOVE "Eft"  TO WS-TRAN
    ELSE
    IF T-CODE = 11 OR 12
        MOVE "Pmt"  TO WS-TRAN
    ELSE
    IF T-CODE = 13 OR 14
        MOVE "Rec"  TO WS-TRAN.
      DISPLAY S08.
             PERFORM AA400.
             MOVE W55-EXP-DATA   TO EXP-REC1.
      PERFORM WRITE-GTRDET THRU WRITE-GTRDET-EXIT.
      GO TO DA05.

       DA900.
      CLOSE GTRDET.

       DA999.
             EXIT.
      /
      *    ****    E X P O R T :
      *
      *                A L L   T H E   F I E L D S
      *
       EA000        SECTION 5.
       EA00.
      PERFORM ERASE-SCREEN.
      DISPLAY S-PROG.
             DISPLAY S04.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO EA999.
      INITIALIZE W60-FIELDS.
             MOVE 1              TO W60-S.
       EA05.
             MOVE W60-S          TO W60-FNO (W60-S).
    IF W60-S < 7
               ADD 1             TO W60-S
               GO TO EA05.
             DISPLAY S06.
             PERFORM AA100.
      DISPLAY S07.
      MOVE 100000  TO G-AC.
      PERFORM START-AT-LEDG-AC THRU READ-GACCNT-EXIT.
    IF WS-F-ERROR = 8
        GO TO EA900.
       EA10.
      PERFORM READ-GACCNT-NEXT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
        GO TO EA900.
      DISPLAY S08.
             PERFORM AA400.
             MOVE W55-EXP-DATA   TO EXP-REC1.
      PERFORM WRITE-GTRDET THRU WRITE-GTRDET-EXIT.
      GO TO EA10.
       EA900.
      CLOSE GTRDET.
       EA999.
             EXIT.
      /
      *    ****    E X P O R T :
      *
      *                S E L E C T E D   F I E L D S
      *
       FA000        SECTION 5.
       FA05.
      PERFORM ERASE-SCREEN.
      DISPLAY S-PROG.
             DISPLAY S05.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO FA999.
      INITIALIZE W60-FIELDS.
             MOVE 1              TO W60-S.
001020      DISPLAY " " AT 1803 WITH FOREGROUND-COLOR 14
       " to view the fields available "
                      WITH FOREGROUND-COLOR 7
                     "S" WITH FOREGROUND-COLOR 14
                     "elect/" WITH FOREGROUND-COLOR 7
                     "A" WITH FOREGROUND-COLOR 14
       "ccept - " WITH FOREGROUND-COLOR 7
                     "Esc" WITH FOREGROUND-COLOR 14
                     "ape" WITH FOREGROUND-COLOR 7.
             MOVE 1              TO W65-S.
       FA10.
             MOVE W65-FLD (W65-S)
                                 TO W65-FIELD.
             DISPLAY S09.
       FA15.
      DISPLAY " " AT 1851.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      DISPLAY SPACE AT 5041
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
       IF W65-S = 1
    GO TO FA15
       ELSE
    SUBTRACT 1  FROM W65-S
    GO TO FA10
   WHEN DOWN-KEY
       IF W65-S NOT < 43
    GO TO FA15
       ELSE
    ADD 1  TO W65-S
    GO TO FA10
   WHEN OTHER
       PERFORM AA900-ALARM
        END-EVALUATE
        GO TO FA15
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO FA05
   WHEN OTHER
       PERFORM AA900-ALARM
        END-EVALUATE
        GO TO FA15
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION
    ELSE
        CALL X"35"
        GO TO FA15.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
           IF NOT (WS-OPTION = "A" OR "S")
        PERFORM AA900-ALARM
               GO TO FA15.
           IF WS-OPTION = "A"
               GO TO FA30.
       FA18.
             MOVE 1              TO WS-S1.
       FA20.
           IF WS-S1 = W60-S
               GO TO FA25.
           IF W60-FNO (WS-S1) = W65-S
               GO TO FA15.
             ADD 1               TO WS-S1.
             GO TO FA20.
       FA25.
             MOVE W65-S          TO W60-FNO (W60-S).
             DISPLAY S06.
             ADD 1               TO W60-S.
    IF W65-S < 43
               ADD 1             TO W65-S.
             GO TO FA10.
       FA30.
             PERFORM AA100.
      DISPLAY S07.
       FA32.
      MOVE 100000  TO G-AC.
      PERFORM START-AT-LEDG-AC THRU READ-GACCNT-EXIT.
    IF WS-F-ERROR = 8
        GO TO FA900.
       FA35.
      PERFORM READ-GACCNT-NEXT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
        GO TO FA900.
      DISPLAY S08.
             PERFORM AA400.
             MOVE W55-EXP-DATA   TO EXP-REC1.
      PERFORM WRITE-GTRDET THRU WRITE-GTRDET-EXIT.
      GO TO FA35.
       FA900.
      CLOSE GTRDET.
       FA999.
             EXIT.
      /
      *    ****    F I E L D   S E P A R A T O R
      *
       GA000        SECTION 5.
       GA00.
             PERFORM ERASE-SCREEN.
      DISPLAY S-PROG.
      DISPLAY S10.
       GA05.
      ACCEPT W50-EXP AT 0618
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
       GA999.
      EXIT.

      /
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

            MOVE 5   TO WS-PARKEY.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-COMPANY TO W95-COMP.

       ZA05.
             MOVE ","            TO W50-EXP.
             GO TO ZA999-EXIT.

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
             MOVE 1              TO WS-S1.
             MOVE SPACES         TO WS-MID-LNE.

       ZA60.
             MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
           IF WS-S1 < 80
               ADD 1 TO WS-S1
               GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G2          TO WS-TCHR (18) WS-TCHR (49)
                                    WS-TCHR (60) WS-TCHR (70).
            MOVE WS-G3   TO WS-MCHR (11) WS-MCHR (45)
              WS-MCHR (63)
        WS-MCHR (1)  WS-MCHR (80).
             MOVE WS-G4          TO WS-BCHR (18) WS-BCHR (49)
                                    WS-BCHR (60) WS-BCHR (70).
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
            DISPLAY "Press any key" AT 5012
                      WITH FOREGROUND-COLOR 14.
            ACCEPT WS-OPTION AT 5026 WITH FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
       ZA999-EXIT.
             EXIT.
      /
       ZB000-ERROR        SECTION 8.

       OPEN-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
             GO TO DISPLAY-FILE-NAME.

       READ-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
             GO TO DISPLAY-FILE-NAME.

       WRITE-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
             GO TO DISPLAY-FILE-NAME.

       DISPLAY-FILE-NAME.
          IF WS-F-ERROR = 8
              MOVE W02-GACCNT  TO WS-FILE
              MOVE ZERO  TO WS-KEY
        MOVE G-AC  TO WS-KEYX
          ELSE
           IF WS-F-ERROR = 11
               MOVE W02-GLTRAN   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE T-KEY        TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 15
               MOVE WS-PARID     TO WS-FILE
              MOVE WS-PARKEY  TO WS-KEY
          ELSE
           IF WS-F-ERROR = 99
              MOVE W02-GTRDET  TO WS-FILE
               MOVE ZERO         TO WS-KEY.
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
               DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
               DISPLAY "Please make a note of these details" AT 1812.
             STOP RUN.
