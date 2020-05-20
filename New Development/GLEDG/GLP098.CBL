      $set LINKCOUNT"512" GNT"GLP098.GNT"
      ******************************************************************
      *                                                                *
      *    ******   **       *******   ****    ******    ******    *
      *   ** **  **       **    **  **  **   ** **  **   **   *
      *   **     **       **    ** **    **  ** **  **   **   *
      *   **     **       ******* **    **   *******   ******    *
      *   **   ***  **       ** **    ** **  **   **   *
      *   ** **  **       **  **  **   ** **  **   **   *
      *    ******   ********  **   ****    ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     A M E N D   T R A N S A C T I O N   B A T C H        *
      *                                                                *
      *       Version 9.04.02 - June 2017                              *
      *               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        GLP098.
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
00110 INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY "CONTROL.SL".

       COPY "PARAM.SL".

       COPY "GACCNT.SL".

       COPY "GLTRAN.SL".

          SELECT GLBTCH ASSIGN DISK
    LOCK MANUAL
                         STATUS WS-STATUS
                         ACCESS DYNAMIC
                         ORGANIZATION INDEXED
          RECORD B-KEY
    ALTERNATE RECORD B-VAL DUPLICATES.

          SELECT PRNREP  ASSIGN W02-PRINTER
                          ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "PARAM.FDE".

       COPY "GACCNT.FDE".

       COPY "GLTRAN.FDE".

      /
       FD  GLBTCH    LABEL RECORD IS STANDARD
             VALUE OF FILE-ID IS W02-GLBTCH.
       01  B-REC.
    03  B-KEY.
              05  B-AC        PIC  9(06).
              05  B-DATE      PIC  9(06).
              05  B-DTE REDEFINES B-DATE.
           07  B-YY    PIC  9(02).
           07  B-MM    PIC  9(02).
           07  B-DD    PIC  9(02).
        05  B-AKEY.
           07  B-CODE  PIC  9(02).
     07  B-REFER.
               09  B-REF
                               PIC  9(07).
     07  B-SEQ   PIC  9(02).
        05  B-SUB       PIC  9(01).
          03  B-NAR        PIC  X(40).
          03  B-DESC REDEFINES B-NAR.
              05  B-NARATIVE  PIC  X(36).
               05  FILLER      PIC  9(07)    COMP-3.
          03  B-VAL        PIC S9(09)V99 COMP.
          03  B-FLAG        PIC  X(01).
    03  B-BATCH        PIC  9(06)    COMP.

       FD  PRNREP    LABEL RECORD OMITTED
             LINAGE IS WS-PGE-LENGTH.
       01  R-L1.
           03  R-DET       PIC X(132).
       01  R-L2.
           03  R-H1        PIC X(06).
          03  R-DTE1    PIC Z9/99/9999.
          03  FILLER    PIC X(08).
           03  R-H2        PIC X(40).
           03  R-H2A       PIC X(27).
           03  FILLER      PIC X(10).
           03  R-H3        PIC X(07).
           03  R-BTCH      PIC Z(04)9.
           03  FILLER      PIC X(10).
           03  R-H4        PIC X(06).
           03  R-PGE       PIC ZZ9.
       01  R-L3.
          03  LED-H1        PIC  X(07).
           03  FILLER          PIC  X(02).
           03  LED-H2          PIC  X(12).
           03  FILLER          PIC  X(06).
           03  LED-H2A         PIC  X(10).
           03  FILLER          PIC  X(02).
           03  LED-H3          PIC  X(04).
           03  FILLER          PIC  X(04).
           03  LED-H4          PIC  X(04).
           03  FILLER          PIC  X(03).
           03  LED-H4A         PIC  X(05).
           03  FILLER          PIC  X(04).
           03  LED-H5          PIC  X(05).
           03  FILLER          PIC  X(02).
           03  LED-H6          PIC  X(15).
           03  FILLER          PIC  X(28).
           03  LED-H7          PIC  X(05).
           03  FILLER          PIC  X(06).
           03  LED-H8          PIC  X(07).
           03  FILLER          PIC  X(01).
       01  R-L4.
           03  R-ACC       PIC 9(04)/99.
          03  FILLER    PIC X(02).
           03  R-ANME      PIC X(24).
          03  FILLER    PIC X(02).
          03  R-DTE2    PIC Z9/99/9999.
          03  FILLER    PIC X(02).
          03  R-CDE    PIC X(04).
    03  FILLER    PIC X(02).
    03  R-BATCH    PIC Z(06).
          03  FILLER    PIC X(02).
    03  R-REFER.
          05  R-REF    PIC Z(07).
          03  FILLER    PIC X(02).
           03  R-NAR       PIC X(36).
           03  FILLER      PIC X(01).
           03  R-DBT       PIC Z(08)9.99.
    03  FILLER    PIC X(01).
          03  R-CRD    PIC Z(07)9.99-.
       01  R-L5.
           03  RI-H1       PIC X(06).
           03  RI-H2       PIC X(40).
           03  RI-H3       PIC X(39).
           03  RI-H4       PIC X(42).
           03  RI-H5       PIC X(05).
       01  R-L6.
           03  RI-NO       PIC Z(04)9.
           03  FILLER      PIC X(01).
           03  RI-ACDT     PIC 9999/99.
           03  FILLER      PIC X(01).
           03  RI-DTNAME   PIC X(30).
           03  FILLER      PIC X(02).
           03  RI-ACCR     PIC 9999/99.
           03  FILLER      PIC X(01).
           03  RI-CRNAME   PIC X(30).
           03  FILLER      PIC X(01).
           03  RI-NAR      PIC X(36).
           03  RI-VAL      PIC Z(07)9.99.

      /
       WORKING-STORAGE SECTION.
       77  WS-CHECK    PIC X(18)  VALUE
      "aeWlimemnomLalismJ".
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
       77  WS-RECOVER      PIC  9(04)    COMP-5.
       77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
       77  WS-ISUB         PIC  9(04)    COMP-5.
       77  WS-CHEQUE       PIC  9(04)    COMP-5.
       77  WS-PAGE         PIC  9(04)    COMP-5.
       77  WS-LINES        PIC  9(04)    COMP-5 VALUE 66.
       77  WS-SUB          PIC  9(04)    COMP-5.
       77  WS-S            PIC  9(04)    COMP-5.
       77  WS-S1           PIC  9(04)    COMP-5.
       77  WS-S2           PIC  9(04)    COMP-5.
       77  WS-S3           PIC  9(04)    COMP-5.
       77  WS-S4    PIC 9(04)  COMP-5.
       77  WS-S5    PIC 9(04)  COMP-5.
       77  WS-S6           PIC  9(04)    COMP-5.
       77  WS-S7    PIC 9(04)  COMP-5.
       77  WS-S8    PIC 9(04)  COMP-5.
       77  WS-S9    PIC 9(04)   COMP-5.
       77  WS-IXS     PIC  9(04)   COMP-5.
       77  WS-IXS1          PIC  9(04)    COMP-5.
       77  WS-IXS2          PIC  9(04)    COMP-5.
       77  WS-IXS3          PIC  9(04)    COMP-5.
       77  WS-IXS4          PIC  9(04)    COMP-5.
       77  WS-Y-END    PIC 9(02) VALUE ZERO.
       77  WS-L-END    PIC 9(04) VALUE ZERO.
       77  WS-OPEN         PIC  9(02) VALUE ZERO.
       77  WS-MS1          PIC  9(04)    COMP-5.
       77  WS-MS2          PIC  9(04)    COMP-5.
       77  WS-VALUE        PIC S9(09)V99 COMP-3.
       77  WS-EXS          PIC  9(01).
       77  WS-CONREC       PIC  9(01) VALUE ZERO.
       77  WS-INDG         PIC  9(01) VALUE ZERO.
       77  WS-OPTION       PIC  X(01).
       77  WS-BAL-TYPE    PIC X(01).
       77  WS-FIRST        PIC  9(02) VALUE ZERO.
       77  WS-LAST         PIC  9(02) VALUE ZERO.
       77  WS-ERROR        PIC  9(01) VALUE ZERO.
       77  WS-NUM          PIC  Z9.
       77  WS-PGE-LENGTH   PIC 9(02)  VALUE 66.
       77  WS-PRN-LENGTH   PIC 9(02)  VALUE 62.
       77  WS-COND    PIC X(06).
       77  WS-NORM    PIC X(06).
       77  WS-EXPP    PIC X(06).
       77  WS-ECAN    PIC X(06).
       77  WS-8LPI    PIC X(06).
       77  WS-6LPI    PIC X(06).
       77  WS-10CPI    PIC X(06).
       77  WS-12CPI    PIC X(06).
       77  WS-17CPI    PIC X(06).
       77  WS-BATCH    PIC 9(06)  COMP-3.
       77  WS-TNEXT        PIC  9(04)    COMP-5.
       77  WS-PERIOD       PIC  9(02)    COMP-5 VALUE ZERO.
       77  WS-PRN-NO    PIC X(01)  COMP-X VALUE 9.
       77  WS-PRN-STAT    PIC X(01)  COMP-X.
       77  WS-H1    PIC X(05) VALUE "DATE:".
       77  WS-H3           PIC  X(06) VALUE "BATCH:".
       77  WS-H4           PIC  X(05) VALUE "PAGE:".
       77  WS-CHK          PIC  X(30) VALUE
                           "Correct ENTER - N if Incorrect".
       77  WS-ER1          PIC  X(07) VALUE "Account".
       77  WS-ER3          PIC  X(04) VALUE "Date".
       77  WS-ER4          PIC  X(05) VALUE "Value".
       77  WS-ER5          PIC  X(13) VALUE "Cheque Number".
       77  WS-ER6          PIC  X(09) VALUE "No Record".
       77  WS-ER7          PIC  X(13) VALUE "Group Heading".
       77  WS-PASSWORD    PIC X(10) VALUE SPACES.
       77  PRG-NAME                PIC  X(12)           VALUE SPACES.
       77  PRG-GLED-LUP            PIC  X(12)           VALUE "GLP\GLPLOOK".
      *77  APAC-VERSION    PIC X(04) VALUE "8.00".
      *77  APAC-SYSTEM    PIC X(12) VALUE "- GL UTILITY".

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
           03  CRT-PROGRAM         PIC  X(15) VALUE "GLP098".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "GENERAL LEDGER -AMEND TRANSACTION BATCH".
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

       COPY "CHAIN.WS".

       01  W02-FILE-IDS.
           03  W02-PRINTER.
        05  W02-REPORT PIC X(07) VALUE "GLP097.".
               05  W02-USER   PIC X(05) VALUE SPACES.

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
       01  W55-TOTA        REDEFINES W55-TOTALS.
          03  W55-1A        PIC S9(07)V99 COMP-3.
          03  W55-2A        PIC S9(07)V99 COMP-3.
          03  W55-3A        PIC S9(07)V99 COMP-3.
          03  W55-4A        PIC S9(07)V99 COMP-3.
          03  W55-5A        PIC S9(07)V99 COMP-3.
          03  W55-6A        PIC S9(07)V99 COMP-3.
          03  W55-7A        PIC S9(07)V99 COMP-3.
          03  W55-8A        PIC S9(07)V99 COMP-3.
          03  W55-9A        PIC S9(07)V99 COMP-3.
          03  W55-10A        PIC S9(07)V99 COMP-3.
          03  W55-11A        PIC S9(07)V99 COMP-3.
          03  W55-12A        PIC S9(07)V99 COMP-3.
       01  W55-TOTB        REDEFINES W55-TOTALS.
          03  W55-VALA    PIC S9(07)V99 COMP-3 OCCURS 12.
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

       01  S05.
          03  LINE 12 COLUMN  3 VALUE "Account".
          03        COLUMN 13 VALUE "Date".
          03        COLUMN 20 VALUE "Reference".
          03        COLUMN 30 VALUE
                                "____________ Narrative _____________".
          03        COLUMN 73 VALUE "Value".

       01  S11.
          03  LINE  2 COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT
           VALUE "AMEND TRANSACTION BATCH".
          03  LINE  4 COLUMN 12 VALUE "Press ".
    03        COLUMN 18 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "C".
    03        COLUMN 19 VALUE "ontinue or ".
    03        COLUMN 30 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "Esc".
    03        COLUMN 33 VALUE "ape to Exit".

       01  S13.
          03  FOREGROUND-COLOR 3 HIGHLIGHT
      PIC Z9/99/9999 USING T-DATE AUTO.

       01  S14.
          03  FOREGROUND-COLOR 3 HIGHLIGHT
      PIC Z(07)9.99CR USING T-VAL AUTO.

       01  S15.
          03  FOREGROUND-COLOR 3 HIGHLIGHT
      PIC X(07) USING T-REFER AUTO.

       01  S16.
          03  FOREGROUND-COLOR 3 HIGHLIGHT
      PIC 9(06) USING W15-ACCOUNT AUTO.

       01  S17.
          03  FOREGROUND-COLOR 3 HIGHLIGHT
      PIC Z(06) USING WS-BATCH AUTO.

       COPY "LEDLUP.CRT".

      /
       PROCEDURE DIVISION
   USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000        SECTION.
       AA00.
          IF WS0-GLLEV < 3
        MOVE "Not Authorised"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO AA49.
            PERFORM ZA000.
      PERFORM HA000.
      CLOSE GLBTCH
     PRNREP.

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
      *     ***  *     ****  *****  ***  *   *   ***** *** *     *****
      *    *   * *     *   *   *   *   * *   *   *      *  *     *
      *    *     *     *   *   *   *     *   *   *      *  *     *
      *    *     *     ****    *   *     *****   ***    *  *     ***
      *    *  ** *     *   *   *   *     *   *   *      *  *     *
      *    *   * *     *   *   *   *   * *   *   *      *  *     *
      *     ***  ***** ****    *    ***  *   *   *     *** ***** *****
      *
       READ-GLBTCH.
            READ GLBTCH
        KEY IS B-KEY.
           IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
              GO TO READ-GLBTCH-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
              MOVE 61   TO WS-F-ERROR
              GO TO READ-GLBTCH-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 61   TO WS-F-ERROR
               PERFORM READ-ERROR.
           IF RECORD-LOCKED
        MOVE W02-GLBTCH  TO WS-FILE
               PERFORM LOCKED-RECORD
              GO TO READ-GLBTCH.
            MOVE 61   TO WS-F-ERROR.
             PERFORM READ-ERROR.
      GO TO READ-GLBTCH-EXIT.

       READ-GLBTCH-NEXT.
            READ GLBTCH NEXT.
           IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
              GO TO READ-GLBTCH-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
              MOVE 61   TO WS-F-ERROR
              GO TO READ-GLBTCH-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 61   TO WS-F-ERROR
               PERFORM READ-ERROR.
           IF RECORD-LOCKED
        MOVE W02-GLBTCH  TO WS-FILE
               PERFORM LOCKED-RECORD
              GO TO READ-GLBTCH-NEXT.
            MOVE 61   TO WS-F-ERROR.
             PERFORM READ-ERROR.
      GO TO READ-GLBTCH-EXIT.

       READ-GLBTCH-PREV.
            READ GLBTCH PREVIOUS.
           IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
              GO TO READ-GLBTCH-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
              MOVE 61   TO WS-F-ERROR
              GO TO READ-GLBTCH-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 61   TO WS-F-ERROR
               PERFORM READ-ERROR.
           IF RECORD-LOCKED
        MOVE W02-GLBTCH  TO WS-FILE
               PERFORM LOCKED-RECORD
              GO TO READ-GLBTCH-PREV.
            MOVE 61   TO WS-F-ERROR.
             PERFORM READ-ERROR.
      GO TO READ-GLBTCH-EXIT.

       START-AT-GLBTCH-KEY.
            START GLBTCH
        KEY >= B-KEY.
           IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
              GO TO READ-GLBTCH-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
              MOVE 61   TO WS-F-ERROR
              GO TO READ-GLBTCH-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 61   TO WS-F-ERROR
               PERFORM READ-ERROR.
      GO TO READ-GLBTCH-EXIT.

       START-AT-GLBTCH-VAL.
            START GLBTCH
        KEY >= B-VAL.
           IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
              GO TO READ-GLBTCH-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
              MOVE 61   TO WS-F-ERROR
              GO TO READ-GLBTCH-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 61   TO WS-F-ERROR
               PERFORM READ-ERROR.

       READ-GLBTCH-EXIT.
             EXIT.

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AC000           SECTION.

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

       COPY "GACCNT.WR".

       COPY "GLTRAN.WR".

       WRITE-GLBTCH.
            WRITE B-REC.
           IF WS-STATUS = "22"
              ADD 1   TO B-SEQ
        GO TO WRITE-GLBTCH.
           IF WS-STAT1 NOT = "0"
              MOVE 61   TO WS-F-ERROR
               PERFORM WRITE-ERROR.

       WRITE-GLBTCH-EXIT.
             EXIT.

       COPY "GACCNT.LUP".

      /
       HA000      SECTION 5.
       HA00.
             PERFORM ERASE-SCREEN.
            DISPLAY S11.
      MOVE " "   TO WS-OPTION.

       HA02.
      ACCEPT WS-OPTION AT 0445
      WITH FOREGROUND-COLOR 7 HIGHLIGHT UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO HA999
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HA02
        END-EVALUATE.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-OPTION = "C")
        GO TO HA02.
      DISPLAY "Password >          <" AT 0612.
       HA05.
      ACCEPT WS-PASSWORD AT 0622 WITH AUTO SECURE.
      MOVE 10   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
    IF WS-PASSWORD NOT = "AMENDATRAN"
        GO TO HA999.
       HA15.
      DISPLAY "Keyin batch number to amend  >        <" AT 0812.
            MOVE ZERO   TO WS-BATCH.
      MOVE "B"   TO WS-BAL-TYPE.
       HA20.
            ACCEPT S17 AT 0842.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO HA999
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HA20
        END-EVALUATE.
    IF WS-BATCH = ZERO
        GO TO HA999.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
              GO TO HA20.
      INITIALIZE T-REC.
      PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
           IF WS-F-ERROR = 11
        MOVE "No transactions on file"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO HA00.
      MOVE ZERO   TO W20-DEBIT W20-CREDIT.
      DISPLAY "Reading :" AT 1012.
      DISPLAY S05.

       HA22.
            PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
      COMMIT.
    IF WS-F-ERROR = 11
        GO TO HA35.
      DISPLAY T-KEY AT 1022 WITH FOREGROUND-COLOR 7 HIGHLIGHT.
    IF T-BATCH NOT = WS-BATCH
        GO TO HA22.
      *
      *    ****    I G N O R E  S E C O N D   N A R R A T I V E
      *
    IF T-SUB = 5
        GO TO HA23.
      MOVE T-AC   TO W15-ACCOUNT.
            PERFORM READ-GACCNT-LOCK THRU READ-GACCNT-EXIT.
      DISPLAY S16 AT 1403.
      DISPLAY S13 AT 1410.
      DISPLAY S15 AT 1421.
      DISPLAY T-NAR AT 1430 WITH HIGHLIGHT.
      DISPLAY S14 AT 1467.
      DISPLAY G-NAME AT 1503
       WITH FOREGROUND-COLOR 6 HIGHLIGHT.
      MOVE "Change value sign Y/N"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF WS-OPTION = "N"
              GO TO HA23.
      PERFORM CLEAR-L50.
      SUBTRACT T-VAL  FROM G-BAL.
      MULTIPLY -1  BY T-VAL.
      DISPLAY S14 AT 1467.
      ADD T-VAL   TO G-BAL.
    IF T-VAL < ZERO
        ADD T-VAL  TO W20-CREDIT
    ELSE
        ADD T-VAL  TO W20-DEBIT.
      PERFORM REWRITE-GACCNT-UNLOCK THRU WRITE-GACCNT-EXIT.

       HA23.
      MOVE T-KEY   TO B-KEY.
            MOVE T-NAR   TO B-NAR.
      MOVE T-SUB   TO B-SUB.
    IF T-SUB = 5
        MOVE ZERO  TO B-VAL
    ELSE
              MOVE T-VAL  TO B-VAL.
            MOVE T-FLAG  TO B-FLAG.
      MOVE T-BATCH  TO B-BATCH.
      PERFORM WRITE-GLBTCH THRU WRITE-GLBTCH-EXIT.
      PERFORM REWRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
      GO TO HA22.

       HA35.
            MOVE ZERO   TO WS-PAGE.
      MOVE ZERO   TO W15-ACCOUNT.
      MOVE "G/L AMENDED TRANSACTIONS"
     TO W10-HEAD.
             MOVE WS-COND        TO R-DET.
             WRITE R-L1 BEFORE 0 LINES.

       HA40.
            MOVE SPACES  TO R-DET.
             MOVE WS-H1          TO R-H1.
             MOVE W90-COMP       TO R-H2.
             MOVE W10-H2         TO R-H2A.
             MOVE WS-H4          TO R-H4.
             ADD 1               TO WS-PAGE.
             MOVE WS-PAGE        TO R-PGE.
             MOVE W05-TODAY      TO R-DTE1.
             WRITE R-L1 BEFORE 2 LINES.
             MOVE SPACES         TO R-DET.
            MOVE "ACNT NO"  TO LED-H1.
             MOVE "ACCOUNT NAME" TO LED-H2.
             MOVE "DATE"         TO LED-H3.
             MOVE "CODE"         TO LED-H4.
             MOVE "BATCH"        TO LED-H4A.
             MOVE "REFER"        TO LED-H5.
             MOVE "PAYEE/NARRATIVE"
                                 TO LED-H6.
            MOVE "DEBIT"  TO LED-H7.
            MOVE " CREDIT"  TO LED-H8.
            WRITE R-L1 BEFORE 2 LINES.
             MOVE SPACES         TO R-DET.

       HA45.
      INITIALIZE B-REC.
      PERFORM START-AT-GLBTCH-KEY THRU READ-GLBTCH-EXIT.
    IF WS-F-ERROR = 61
        GO TO HA55.

       HA50.
      PERFORM READ-GLBTCH-NEXT THRU READ-GLBTCH-EXIT.
    IF WS-F-ERROR = 61
        GO TO HA55.
    IF B-AC NOT = W15-ACCOUNT
        MOVE B-AC  TO W15-ACCOUNT
        MOVE G-AC  TO R-ACC
        MOVE G-NAME  TO R-ANME.
    IF B-SUB = 5
        MOVE B-NAR  TO R-NAR
        GO TO HA52.
            MOVE B-DATE  TO W15-YMD
      MOVE W15-YR  TO W15-YY.
             MOVE W15-MT         TO W15-MM.
             MOVE W15-DY         TO W15-DD.
            MOVE W15-DATE  TO R-DTE2.
          IF B-CODE = 01 OR 02
              MOVE "JNL"  TO R-CDE
           ELSE
          IF B-CODE = 03 OR 04
              MOVE "CHEQ"  TO R-CDE
           ELSE
          IF B-CODE = 06 OR 07
              MOVE "DEP"  TO R-CDE.
      MOVE B-BATCH  TO R-BATCH.
            MOVE B-NAR   TO R-NAR.
    IF B-REFER NUMERIC
              MOVE B-REF  TO R-REF
    ELSE
        MOVE B-REFER  TO R-REFER.
          IF B-VAL < ZERO
              MOVE B-VAL  TO R-CRD
           ELSE
              MOVE B-VAL  TO R-DBT.

       HA52.
          IF LINAGE-COUNTER < WS-PRN-LENGTH
               WRITE R-L1 BEFORE 1 LINES
           ELSE
               WRITE R-L1 BEFORE PAGE
              PERFORM HA40
        MOVE ZERO  TO W15-ACCOUNT.
            MOVE SPACES TO R-DET.
      GO TO HA50.

       HA55.
            WRITE R-L1 BEFORE 1 LINES
      MOVE "****  E N D   R E P O R T  ****"
     TO R-NAR.
      MOVE W20-DEBIT  TO R-DBT.
      MOVE W20-CREDIT  TO R-CRD.
            WRITE R-L1 BEFORE PAGE.
      MOVE SPACES  TO R-DET.
      CLOSE GLBTCH.
      OPEN OUTPUT GLBTCH.
      CLOSE GLBTCH.
      OPEN I-O GLBTCH.
      GO TO HA00.

       HA999.
             EXIT.

      /
       ZA000      SECTION 9.
       ZA00.
      MOVE LS-L-OR-N  TO W02-L-OR-N.
      MOVE WS-SYS-ID  TO W02-SYSID.
             PERFORM ZA55 THRU ZA60.
            PERFORM ERASE-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      COPY "FUNCTION.PRO".

    IF WS-USER NUMERIC
        IF WS-USE > ZERO AND < 110
     MOVE WS-USE  TO WS-USUB
                   GO TO ZA01A.
             MOVE 110            TO WS-USUB.
       ZA01A.
             ADD 2549 WS-USUB    GIVING W25-RESULT.
             MOVE W25-KEY        TO WS-PARKEY.
             ADD 1 W25-SUB       GIVING WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
    IF TRM-STDP (WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
        MOVE ZERO  TO WS-PRN-NO
           ELSE
           IF TRM-STDP (WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
        MOVE 1   TO WS-PRN-NO
           ELSE
           IF TRM-STDP (WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
        MOVE 2   TO WS-PRN-NO
    ELSE
           IF TRM-STDP (WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 9
        MOVE WS-USER  TO W02-USER.
             ADD 501 TRM-PRN3 (WS-S6)
                                 GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S6.
             ADD 1               TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE TRM-CONP (WS-S6)
                                 TO WS-COND.
             MOVE TRM-NORP (WS-S6)
                                 TO WS-NORM.
             MOVE TRM-EXPP (WS-S6)
                                 TO WS-EXPP.
             MOVE TRM-ECAN (WS-S6)
                                 TO WS-ECAN.
             MOVE TRM-8LPI (WS-S6)
                                 TO WS-8LPI.
             MOVE TRM-6LPI (WS-S6)
                                 TO WS-6LPI.
             MOVE TRM-10CPI (WS-S6)
                                 TO WS-10CPI.
             MOVE TRM-12CPI (WS-S6)
                                 TO WS-12CPI.
             MOVE TRM-17CPI (WS-S6)
                                 TO WS-17CPI.
      MOVE TRM-LENGTH (WS-S6)
     TO WS-PRN-LENGTH.
      MOVE TRM-PAGE (WS-S6)
     TO WS-PGE-LENGTH.
       ZA02.
      OPEN OUTPUT GLBTCH.
          IF RUNTIME-ERROR
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS NOT = "00"
              MOVE 61   TO WS-F-ERROR
               PERFORM OPEN-ERROR.
      CLOSE GLBTCH.
      OPEN I-O GLBTCH.
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
              PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
      *
      *    ****    S E T   U P   M O N T H S   ( Y E A R   E N D)
      *
             MOVE 1 TO WS-S1.
             MOVE G-YEAR-END TO WS-S2 WS-Y-END.
             MOVE G-LAST-YE TO WS-L-END.
             MOVE G-OPEN TO WS-OPEN.
             ADD 1 TO WS-S2.
       ZA15.
           IF WS-S2 > 12
               SUBTRACT 12 FROM WS-S2.
             MOVE W30-MONTH (WS-S2) TO W35-MONTH (WS-S1).
           IF WS-S1 < 12
               ADD 1 TO WS-S1 WS-S2
               GO TO ZA15.
             MOVE ZERO TO WS-ERROR.
           IF W70-BNKGL = ZERO
              DISPLAY "Parameters not set up - " AT 2020
         "ENTER" WITH FOREGROUND-COLOR 14
               ACCEPT WS-OPTION AT 2050 WITH FOREGROUND-COLOR 15 AUTO
              MOVE 1   TO WS-ERROR.
      DISPLAY SPACES AT 5025
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
    IF WS-PRN-NO < 3
        PERFORM ZA210 THRU ZA215
        IF WS-ERROR = 1
     GO TO ZA205
    ELSE
        DISPLAY "Waiting for printer response" AT 1412
   WITH FOREGROUND-COLOR 14 BLINK.
            OPEN OUTPUT PRNREP.
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
            MOVE SPACES TO WS-MID-LNE WS-MID-LNE2.
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
                            WS-MCHR (1)  WS-MCHR (80)
                            WS-MCH  (1)  WS-MCH  (80).
             MOVE WS-G4  TO WS-BCHR (11) WS-BCHR (45)
                            WS-BCHR (63).
      MOVE WS-COMPANY  TO WS-TOP-COMP.
    IF WS-USER = LS-SYS-ID
        MOVE "SupervisorÄ"  TO WS-WRKHD
    ELSE
        MOVE "Workstation"  TO WS-WRKHD
        MOVE WS-USER    TO WS-WRKST.
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
       ZA210.
      MOVE ZERO   TO WS-ERROR
        WS-S1 WS-S2 WS-S3 WS-S4
        WS-S5 WS-S6 WS-S7 WS-S8.
      CALL "PC_TEST_PRINTER" USING WS-PRN-NO
       WS-PRN-STAT
        RETURNING WS-STATUS.
    IF WS-PRN-STAT > 127
        MOVE 1   TO WS-S1
        SUBTRACT 128  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 63
        MOVE 1   TO WS-S2
        SUBTRACT 64  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 31
        MOVE 1   TO WS-S3
        SUBTRACT 32  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 15
        MOVE 1   TO WS-S4
        SUBTRACT 16  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 7
        MOVE 1   TO WS-S5
        SUBTRACT 8  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 3
        MOVE 1   TO WS-S6
        SUBTRACT 4  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 1
        MOVE 1   TO WS-S7
        SUBTRACT 2  FROM WS-PRN-STAT.
      MOVE WS-PRN-STAT  TO WS-S8.
    IF WS-S3 = 0 AND
       WS-S5 = 0 AND
       WS-S8 = 0
        GO TO ZA215.
      DISPLAY "Check printer - " AT 2304
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
       "R" WITH FOREGROUND-COLOR 7 HIGHLIGHT
       "etry - "
       "A" WITH FOREGROUND-COLOR 7 HIGHLIGHT
       "bort".
       ZA212.
      ACCEPT WS-OPTION AT 2334
      WITH FOREGROUND-COLOR 7 HIGHLIGHT AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      IF NOT (WS-OPTION = "A" OR "R")
   GO TO ZA212.
      IF WS-OPTION = "A"
   MOVE 1   TO WS-ERROR
   GO TO ZA215.
        GO TO ZA210.
       ZA215.
      EXIT.
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
              MOVE W02-NETWORK  TO WS-FILE
              MOVE WS-NETKEY  TO WS-KEY
           ELSE
           IF WS-F-ERROR = 8
              MOVE W02-GACCNT  TO WS-FILE
              MOVE WS-GLKEY  TO WS-KEY
           ELSE
           IF WS-F-ERROR = 11
               MOVE W02-GLTRAN   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE T-KEY        TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 15
              MOVE WS-PARID  TO WS-FILE
              MOVE WS-PARKEY  TO WS-KEY
           ELSE
          IF WS-F-ERROR = 61
              MOVE W02-GLBTCH  TO WS-FILE
              MOVE B-KEY  TO WS-KEYX.
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
