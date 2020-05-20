      $set LINKCOUNT"512" GNT"GLP093.GNT"
      ******************************************************************
      *                                                                *
      *    ******   **       *******   ****    ******    ******    *
      *   ** **  **       **    **  **  **   ** **  **   **   *
      *   **     **       **    ** **    **  ** **   **   *
      *   **     **       ******* **    **   *******     ****    *
      *   **   ***  **       ** **    ** **   **   *
      *   ** **  **       **  **  **   ** **  **   **   *
      *    ******   ********  **   ****    ******    ******    *
      *               *
      *     ENGLISH                                                    *
      *                                                                *
      *     G / L E D G E R   B A L A N C I N G   U T I L I T I E S    *
      *                                                                *
      *       Version 9.04.02 - June 2017                              *
      *               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        GLP093.
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

       COPY "GLTRNB.SL".

          SELECT GLBTCH ASSIGN DISK
    LOCK MANUAL
                         STATUS WS-STATUS
                         ACCESS DYNAMIC
                         ORGANIZATION INDEXED
          RECORD B-KEY
    ALTERNATE RECORD B-VAL DUPLICATES.

       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "PARAM.FDE".

       COPY "GACCNT.FDE".

       COPY "GLTRAN.FDE".

       COPY "GLTRNB.FDE".

      /
       FD  GLBTCH    LABEL RECORD IS STANDARD
             VALUE OF FILE-ID IS W02-GLBTCH.
       01  B-REC.
    03  B-KEY.
              05  B-AC        PIC  9(06).
              05  B-DATE      PIC  9(08).
              05  B-DTE REDEFINES B-DATE.
           07  B-CC    PIC  9(02).
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
       77  WS-INKEY    PIC 9(06)  COMP-5.
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
       77  WS-IXS1    PIC 9(04)   COMP-5.
       77  WS-Y-END        PIC  9(02) VALUE ZERO.
       77  WS-L-END    PIC 9(04) VALUE ZERO.
       77  WS-OPEN         PIC  9(02) VALUE ZERO.
       77  WS-MS1          PIC  9(04)    COMP-5.
       77  WS-MS2          PIC  9(04)    COMP-5.
       77  WS-START    PIC 9(06)  COMP-3.
       77  WS-END    PIC 9(06)  COMP-3.
       77  WS-VALUE    PIC S9(09)V99 COMP-3.
       77  WS-ENDINP    PIC 9(01).
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
       77  WS-DATE    PIC 9(06)  COMP-3.
       77  WS-BATCH    PIC 9(06)  COMP-3.
       77  WS-TNEXT        PIC  9(04)    COMP-5.
       77  WS-PERIOD       PIC  9(02)    COMP-5 VALUE ZERO.
       77  WS-PRN-NO    PIC X(01)  COMP-X VALUE 9.
       77  WS-PRN-STAT    PIC X(01)  COMP-X.
       77  WS-H1    PIC X(05) VALUE "DATE:".
       77  WS-H3           PIC  X(06) VALUE "BATCH:".
       77  WS-H4           PIC  X(05) VALUE "PAGE:".
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "GLP093".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "GENERAL LEDGER -BALANCING UTILITY".
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
    03  WS-PROG        PIC  X(03) VALUE "093".

       01  WT-REC.
    03  WT-KEY.
              05  WT-AC PIC  9(06).
              05  WT-DATE PIC  9(08).
              05  WT-DTE REDEFINES WT-DATE.
           07  WT-CC PIC  9(02).
           07  WT-YY PIC  9(02).
           07  WT-MM PIC  9(02).
           07  WT-DD PIC  9(02).
        05  WT-AKEY.
           07  WT-CODE PIC  9(02).
     07  WT-REFER.
               09  WT-REF
    PIC  9(07).
     07  WT-SEQ PIC  9(02).
        05  WT-SUB PIC  9(01).
          03  WT-NAR  PIC  X(40).
          03  WT-DESC REDEFINES WT-NAR.
              05  WT-NARATIVE PIC  X(36).
  05  FILLER PIC  9(07)    COMP-3.
          03  WT-VAL  PIC S9(09)V99 COMP-3.
          03  WT-FLAG  PIC  X(01).
    03  WT-BATCH        PIC  9(06)    COMP-3.

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-PRINTER-DETAILS.
    03  W02-PRINTER     PIC  X(12).
          03  W02-PGE-LENGTH  PIC  9(03).
          03  W02-PRN-LENGTH  PIC  9(03).
    03  W02-LINAGE      PIC  9(03).
    03  W02-PRN-STATUS  PIC  X(01) VALUE "C".
      *
      *    ****    D  =  Detail line
      *     C  =  Condensed print
      *     E  =  Expanded print
      *     H  =  Header line
      *     X  =  Cancel expanded print
      *     1  =  10 Characters per inch
      *     2  =  12 Characters per inch
      *     3  =  17 Characters per inch
      *     6  =  6 Lines per inch
      *     8  =  8 Lines per inch
      *
    03  W02-PRN-TYPE    PIC  X(01).
    03  W02-PRN-LINE    PIC  X(136).

          03  R-L1 REDEFINES W02-PRN-LINE.
              05  R-DET       PIC  X(136).

          03  R-L2 REDEFINES W02-PRN-LINE.
              05  R-H1        PIC  X(06).
              05  R-DTE1      PIC  Z9/99/9999.
              05  FILLER      PIC  X(08).
              05  R-H2        PIC  X(40).
              05  R-H2A       PIC  X(27).
              05  FILLER      PIC  X(10).
              05  R-H3        PIC  X(07).
              05  R-BTCH      PIC  Z(04)9.
              05  FILLER      PIC  X(10).
              05  R-H4        PIC  X(06).
              05  R-PGE       PIC  ZZ9.

          03  R-L3 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(10).
              05  R-AC1       PIC  9999/99.
              05  FILLER      PIC  X(03).
              05  R-NME1      PIC  X(30).
              05  FILLER      PIC  X(53).
              05  R-DT1       PIC  Z(07)9.99 BLANK WHEN ZERO.
              05  FILLER      PIC  X(01).
              05  R-CR1       PIC  Z(07)9.99 BLANK WHEN ZERO.
              05  FILLER      PIC  X(06).

          03  R-L4 REDEFINES W02-PRN-LINE.
              05  R-ACC       PIC  9(04)/99.
              05  FILLER      PIC  X(02).
              05  R-ANME      PIC  X(24).
              05  FILLER      PIC  X(02).
              05  R-DTE2      PIC  Z9/99/9999.
              05  FILLER      PIC  X(02).
              05  R-CDE       PIC  X(04).
        05  FILLER      PIC  X(02).
        05  R-BATCH     PIC  Z(06).
              05  FILLER      PIC  X(02).
        05  R-REFER.
           07  R-REF   PIC  Z(07).
              05  FILLER      PIC  X(02).
              05  R-NAR       PIC  X(36).
              05  FILLER      PIC  X(01).
              05  R-DBT       PIC  Z(08)9.99.
        05  FILLER      PIC  X(01).
              05  R-CRD       PIC  Z(07)9.99-.

          03  R-L5 REDEFINES W02-PRN-LINE.
              05  LED-H1      PIC  X(07).
              05  FILLER      PIC  X(02).
              05  LED-H2      PIC  X(12).
              05  FILLER      PIC  X(06).
        05  LED-H2A     PIC  X(10).
        05  FILLER      PIC  X(02).
              05  LED-H3      PIC  X(04).
              05  FILLER      PIC  X(04).
              05  LED-H4      PIC  X(04).
        05  FILLER      PIC  X(03).
              05  LED-H4A     PIC  X(05).
              05  FILLER      PIC  X(04).
              05  LED-H5      PIC  X(05).
              05  FILLER      PIC  X(02).
              05  LED-H6      PIC  X(15).
              05  FILLER      PIC  X(28).
              05  LED-H7      PIC  X(05).
              05  FILLER      PIC  X(06).
              05  LED-H8      PIC  X(07).
              05  FILLER      PIC  X(01).

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
          03  W20-ACCOUNT   PIC  9(06).
           03  W20-ACNO1   REDEFINES W20-ACCOUNT.
              05  W20-ACNO  PIC 9(04).
              05  W20-SER   PIC  9(02).
          03  W20-ACNO2     REDEFINES W20-ACCOUNT.
              05  W20-GR    PIC  9(02).
              05  W20-SGRP  PIC 9(02).
              05  W20-ASER  PIC 9(02).
          03  W20-ACNO3     REDEFINES W20-ACCOUNT.
              05  W20-GROUP PIC 9(02).
              05  W20-REST  PIC 9(04).

       COPY "W20.WS".

       01  W25-CALCS.
          03  W25-RESULT      PIC  9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
              05  W25-WHOLE   PIC  9(05).
              05  W25-DECIMAL PIC  9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
        05  W25-KEY     PIC  9(04).
        05  W25-SUB     PIC  9(01).
        05  FILLER      PIC  9(02).
       01  W25-EDIT.
           03  W25-7.
              05  W25-1       PIC  ZZZZZZZZ9-.
           03  W25-S7V2.
              05  W25-V1      PIC  Z(07)9.99CR.
          03  W25-S8V2.
              05  W25-VG1     PIC  Z(07)9.99CR.
          03  W25-DATE.
              05  W25-DTE     PIC  Z9/99/9999.
           03  W25-REF.
              05  W25-REFER   PIC  Z(07).
          03  W25-RECKEY      PIC  Z(04)9.
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

       01  S11.
          03  LINE  2 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
           VALUE "BALANCING UTILITIES".
          03  LINE  4 COLUMN 12 VALUE "Balance an ".
    03        COLUMN 23 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "A".
    03        COLUMN 24 VALUE "ccount,".
    03        COLUMN 31 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "B".
    03        COLUMN 32 VALUE "atch or ".
    03        COLUMN 40 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "R".
    03        COLUMN 41 VALUE "ange of batches > < ".
    03        COLUMN 61 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "Esc".
    03        COLUMN 64 VALUE "ape to Exit".

       01  S17.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC 9(06) USING W15-ACCOUNT AUTO.

       01  S18.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z(05)9 USING WS-BATCH AUTO.

       01  S19.
          03  LINE  6 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC Z(05)9 USING WS-START AUTO.
          03  LINE  7 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC Z(05)9 USING WS-END AUTO.

       COPY "LEDLUP.CRT".

      /
       PROCEDURE DIVISION
   USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000        SECTION.
       AA00.
          IF LS0-GLLEV < 3
        MOVE "Not Authorised"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO AA49.
            PERFORM ZA000.
      * ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      *  ***  ****  ***** *   *   **** ****  *** *   * ***** ***** ****
      * *   * *   * *   **  *   *   * *   *  *  **  *   *   *     * *
      * *   * ****  ***   * * *   **** ****   *  * * *   *   ***   ****
      * *   * *     *   *  **   * *   *  *  *  **   *   *     * *
      *  ***  *     ***** *   *   * *   * *** *   *   *   ***** * *
      * ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      PERFORM OPEN-PRINTER.
      MOVE "P"   TO WS-COMMAND.
      PERFORM HA000.
      CLOSE GLBTCH.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.

       AA49.
      EXIT PROGRAM.

       COPY "FUNCTION.CRT".

       COPY "OPTION.CRT".

       COPY "LOCKED.REC".

       COPY "CLEAR.CRT".

       COPY "ABORT.PRN".

       COPY "PRINTUTL.AP6".

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

       COPY "APAC.HLP".

      /
       HA000      SECTION 5.
       HA00.
             PERFORM ERASE-SCREEN.
            DISPLAY S11.
      MOVE "B"   TO WS-OPTION.

       HA02.
      ACCEPT WS-OPTION AT 0458
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO HA999
   WHEN F3-KEY
       MOVE "P"  TO WS-OPTION
       MOVE "B"  TO WS-BAL-TYPE
       GO TO HA22
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HA02
        END-EVALUATE.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-OPTION = "A" OR "B" OR "R")
        GO TO HA02.
    IF WS-OPTION = "B"
        GO TO HA15.
    IF WS-OPTION = "R"
        MOVE ZERO  TO WS-ENDINP
        PERFORM HB000
        GO TO HA00.
       HA03.
      DISPLAY "Account Number :" AT 0612.
             MOVE ZERO TO W15-ACCOUNT.

       HA05.
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
            ACCEPT S17 AT 0629.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE ZERO  TO W15-ACCOUNT
       GO TO HA999
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA350
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HA05
        END-EVALUATE
        DISPLAY S17 AT 0629
               IF W15-ACCOUNT = ZERO
           GO TO HA05.
      PERFORM CLEAR-L50.
           IF W15-ACCOUNT = ZERO
               GO TO HA999.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
            PERFORM CLEAR-L50.
          IF WS-F-ERROR = 8
              MOVE WS-ER6  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO HA05.
            DISPLAY G-NAME AT 0637 WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 5.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
              GO TO HA03.
      INITIALIZE T-REC.
      MOVE G-AC   TO T-AC.
      PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
           IF WS-F-ERROR = 11
        MOVE "No transactions for this account"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO HA00.
      MOVE ZERO   TO W20-DEBIT W20-CREDIT.
      DISPLAY "Reading :" AT 0812.

       HA10.
            PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
      COMMIT.
           IF (WS-F-ERROR = 11) OR
              (T-AC NOT = G-AC)
        MOVE "A"   TO WS-BAL-TYPE
        GO TO HA35.
      DISPLAY T-KEY AT 0822
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      *
      *    ****    I G N O R E  S E C O N D   N A R R A T I V E
      *
    IF T-SUB = 5
        GO TO HA10.
      MOVE T-KEY   TO B-KEY.
            MOVE T-NAR   TO B-NAR.
            MOVE T-VAL   TO B-VAL.
            MOVE T-FLAG  TO B-FLAG.
      MOVE T-BATCH  TO B-BATCH.
      PERFORM WRITE-GLBTCH THRU WRITE-GLBTCH-EXIT.
    IF T-VAL < ZERO
        ADD T-VAL  TO W20-CREDIT
    ELSE
        ADD T-VAL  TO W20-DEBIT.
      GO TO HA10.

       HA15.
      DISPLAY "Batch Number :" AT 0612.
            MOVE ZERO   TO WS-BATCH.
      MOVE "B"   TO WS-BAL-TYPE.
       HA20.
            ACCEPT S18 AT 0627.
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
      DISPLAY WS-BLNK78 AT 1202 WITH FOREGROUND-COLOR 3.
       HA22.
    IF WS-OPTION = "P"
        PERFORM HA60.
      INITIALIZE T-REC.
      PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
           IF WS-F-ERROR = 11
        MOVE "No transactions on file"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO HA00.
      MOVE ZERO   TO W20-DEBIT W20-CREDIT.
      DISPLAY "Reading :" AT 0812.

       HA25.
            PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
      COMMIT.
    IF WS-F-ERROR = 11
        GO TO HA30.
      DISPLAY T-KEY AT 0822
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.

      ****************************************************************

    IF WS-OPTION = "P"
        IF T-DATE < WS-DATE
     MOVE T-REC  TO WT-REC
     PERFORM DELETE-GLTRAN-REC THRU WRITE-GLTRAN-EXIT
      *     MOVE WS-DATE  TO T-DATE
      *     MOVE 999999  TO T-BATCH
      *     PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT
     MOVE WT-REC  TO T-REC
     PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT
     GO TO HA25
        ELSE
     GO TO HA25.

      *****************************************************************

    IF T-BATCH NOT = WS-BATCH
        GO TO HA25.
      *
      *    ****    I G N O R E  S E C O N D   N A R R A T I V E
      *
       HA26.
    IF T-SUB = 5
        GO TO HA25.
      MOVE T-KEY   TO B-KEY.
            MOVE T-NAR   TO B-NAR.
            MOVE T-VAL   TO B-VAL.
            MOVE T-FLAG  TO B-FLAG.
      MOVE T-BATCH  TO B-BATCH.
      PERFORM WRITE-GLBTCH THRU WRITE-GLBTCH-EXIT.
    IF T-VAL < ZERO
        ADD T-VAL  TO W20-CREDIT
    ELSE
        ADD T-VAL  TO W20-DEBIT.
      GO TO HA25.

       HA30.
    IF (W20-DEBIT + W20-CREDIT) = ZERO
        MOVE "Batch is in balance-'P'rint,'N'ext or 'E'xit"
     TO WS-ERR-MES
        MOVE SPACE  TO WS-OPTION
        PERFORM OPT-MESSAGE
        IF USER-FUNC
     EVALUATE KEY-CODE-1
       WHEN ESC-KEY
     GO TO HA999
       WHEN OTHER  PERFORM AA900-ALARM
     END-EVALUATE
     GO TO HA30
        ELSE
        IF DATA-8BIT
     MOVE KEY-CODE-1X  TO WS-OPTION
     IF NOT (WS-OPTION = "E" OR "N" OR "P")
         PERFORM AA900-ALARM
         GO TO HA30
     ELSE
     PERFORM CLEAR-L50
     IF WS-OPTION = "E"
         GO TO HA999
     ELSE
     IF WS-OPTION = "N"
         GO TO HA15
     ELSE
         GO TO HA35
        ELSE
     PERFORM AA900-ALARM
     GO TO HA30.

       HA35.
            MOVE ZERO   TO WS-PAGE.
      MOVE ZERO   TO W15-ACCOUNT.
    IF WS-BAL-TYPE = "B"
              MOVE "G/L BATCH LISTING"
     TO W10-HEAD
    ELSE
        MOVE "G/L ACCOUNT TRANSACTIONS"
     TO W10-HEAD.
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE 3   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
      MOVE "V"   TO WS-OPTION.
      DISPLAY "D" AT 1012 WITH FOREGROUND-COLOR 6 HIGHLIGHT
       "ate or " "V" WITH FOREGROUND-COLOR 6 HIGHLIGHT
       "alue sequence ".

       HA38.
      ACCEPT WS-OPTION AT 1036
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "D" OR "V")
        GO TO HA38.

       HA40.
            MOVE SPACES  TO R-DET.
             MOVE WS-H1          TO R-H1.
             MOVE W90-COMP       TO R-H2.
             MOVE W10-H2         TO R-H2A.
             MOVE WS-H4          TO R-H4.
             ADD 1               TO WS-PAGE.
             MOVE WS-PAGE        TO R-PGE.
    IF WS-BAL-TYPE = "B"
              MOVE WS-H3  TO R-H3
              MOVE WS-BATCH  TO R-BTCH.
             MOVE W05-TODAY      TO R-DTE1.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
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
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
    IF WS-BAL-TYPE = "A"
        MOVE G-AC  TO R-ACC
              MOVE G-NAME  TO R-ANME W15-ACCOUNT.

       HA45.
      INITIALIZE B-REC.
    IF WS-OPTION = "V"
        MOVE ZERO  TO B-VAL
        PERFORM START-AT-GLBTCH-VAL THRU READ-GLBTCH-EXIT
    ELSE
        PERFORM START-AT-GLBTCH-KEY THRU READ-GLBTCH-EXIT.
    IF WS-F-ERROR = 61
        GO TO HA55.

       HA50.
      PERFORM READ-GLBTCH-NEXT THRU READ-GLBTCH-EXIT.
    IF WS-F-ERROR = 61
        GO TO HA55.
    IF WS-BAL-TYPE = "B"
        IF B-AC NOT = W15-ACCOUNT
     MOVE B-AC  TO W15-ACCOUNT G-AC
           PERFORM READ-GACCNT THRU READ-GACCNT-EXIT
     MOVE G-AC  TO R-ACC
     MOVE G-NAME  TO R-ANME.
            MOVE B-DATE  TO W15-YMD
      MOVE W15-CN  TO W15-CC.
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
          IF B-CODE = 05 OR 10
              MOVE "CHEQ"  TO R-CDE
           ELSE
          IF B-CODE = 06 OR 07
              MOVE "DEP"  TO R-CDE
           ELSE
          IF B-CODE = 08 OR 09
              MOVE "EFT"  TO R-CDE
           ELSE
          IF B-CODE = 11 OR 12
              MOVE "PMT"  TO R-CDE
           ELSE
          IF B-CODE = 13 OR 14
              MOVE "REC"  TO R-CDE.
    IF WS-BAL-TYPE = "A"
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
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM HA40
        IF WS-BAL-TYPE = "B"
     MOVE ZERO  TO W15-ACCOUNT.
            MOVE SPACES  TO R-DET.
      GO TO HA50.

       HA55.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE "****  E N D   R E P O R T  ****"
     TO R-NAR.
      MOVE W20-DEBIT  TO R-DBT.
      MOVE W20-CREDIT  TO R-CRD.
      MOVE 99   TO WS-ADVANCE.
      MOVE "E"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO R-DET.
      CLOSE GLBTCH.
      OPEN OUTPUT GLBTCH.
      CLOSE GLBTCH.
      OPEN I-O GLBTCH.

       HA58.
      GO TO HA00.

       HA60.
      DISPLAY "Date (YYMMDD) :" AT 0612.
      DISPLAY "NB. Keep transactions from this date" AT 5002
        WITH BACKGROUND-COLOR 3
      FOREGROUND-COLOR 6 HIGHLIGHT.
            MOVE ZERO   TO WS-DATE.
      ACCEPT WS-DATE AT 0628
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
              GO TO HA60.
      MOVE "P"        TO WS-OPTION.

       HA65.
      EXIT.

       HA999.
             EXIT.

       HB000      SECTION 6.
       HB00.
      OPEN OUTPUT GLTRNB.
      CLOSE GLTRNB.
      OPEN I-O GLTRNB.
      DISPLAY "Starting Batch :" AT 0612.
      DISPLAY "Ending   Batch :" AT 0712.
            MOVE ZERO   TO WS-START WS-END.
      MOVE "B"   TO WS-BAL-TYPE.
       HB05.
      ACCEPT S19.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO HB999
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HB05
        END-EVALUATE.
    IF WS-START = ZERO
        GO TO HB999.
    IF WS-START > WS-END
        MOVE "Start greater than End batch"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO HB05.
      PERFORM CLEAR-L50.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
              GO TO HB05.
      INITIALIZE T-REC.
      PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
           IF WS-F-ERROR = 11
        MOVE "No transactions on file"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO HB999.
      DISPLAY "Extracting :" AT 0912.
       HB10.
            PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
      COMMIT.
    IF WS-F-ERROR = 11
        GO TO HB15.
      DISPLAY T-KEY AT 0925
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
    IF T-BATCH < WS-START OR
       T-BATCH > WS-END
        GO TO HB10.
      *
      *    ****    I G N O R E  S E C O N D   N A R R A T I V E
      *
    IF T-SUB = 5
        GO TO HB10.
      MOVE T-REC   TO TB-REC.
      WRITE TB-REC.
      GO TO HB10.

       HB15.
      MOVE ZERO   TO WS-BATCH.
      DISPLAY "Reading    :" AT 0912.
      DISPLAY "Batch :" AT 0950.
      INITIALIZE TB-REC.
            START GLTRNB
        KEY >= TB-BKEY.
           IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
              GO TO HB20.
            GO TO HB900.

       HB20.
            READ GLTRNB NEXT.
          IF WS-STAT1 = "0"
        DISPLAY TB-KEY AT 0925
         WITH FOREGROUND-COLOR 3 HIGHLIGHT
       BACKGROUND-COLOR 5
        MOVE ZERO  TO WS-F-ERROR
              GO TO HB25.
            MOVE 51   TO WS-F-ERROR.

       HB25.
    IF WS-F-ERROR = 51
        MOVE 9   TO WS-ENDINP
        GO TO HB35.
    IF NOT (TB-BATCH = WS-BATCH)
        GO TO HB35.

       HB30.
      MOVE TB-KEY  TO B-KEY.
            MOVE TB-NAR  TO B-NAR.
            MOVE TB-VAL  TO B-VAL.
            MOVE TB-FLAG  TO B-FLAG.
      MOVE TB-BATCH  TO B-BATCH.
      PERFORM WRITE-GLBTCH THRU WRITE-GLBTCH-EXIT.
    IF TB-VAL < ZERO
        ADD TB-VAL  TO W20-CREDIT
    ELSE
        ADD TB-VAL  TO W20-DEBIT.
      GO TO HB20.

       HB35.
    IF WS-BATCH = ZERO
        GO TO HB50.
    IF (W20-DEBIT + W20-CREDIT) = ZERO
        MOVE "Batch in balance - 'P'rint,'N'ext or 'E'xit"
     TO WS-ERR-MES
        MOVE SPACE  TO WS-OPTION
        PERFORM OPT-MESSAGE
        IF USER-FUNC
     EVALUATE KEY-CODE-1
       WHEN ESC-KEY
     GO TO HB900
       WHEN OTHER  PERFORM AA900-ALARM
     END-EVALUATE
     GO TO HB35
        ELSE
        IF DATA-8BIT
     MOVE KEY-CODE-1X  TO WS-OPTION
     IF NOT (WS-OPTION = "E" OR "N" OR "P")
         PERFORM AA900-ALARM
         GO TO HB35
     ELSE
     PERFORM CLEAR-L50
     IF WS-OPTION = "E"
         PERFORM HB45
         GO TO HB900
     ELSE
     IF WS-OPTION = "N"
         GO TO HB45
     ELSE
         GO TO HB40
        ELSE
     PERFORM AA900-ALARM
     GO TO HB35.

       HB40.
      PERFORM HA35 THRU HA55.

       HB45.
      CLOSE GLBTCH.
      OPEN OUTPUT GLBTCH.
      CLOSE GLBTCH.
      OPEN I-O GLBTCH.

       HB50.
    IF WS-ENDINP = 9
        GO TO HB900.
      MOVE ZERO   TO W20-DEBIT W20-CREDIT.
      MOVE TB-BATCH  TO WS-BATCH.
      DISPLAY WS-BATCH AT 0958
       WITH BACKGROUND-COLOR 5
     FOREGROUND-COLOR 6 HIGHLIGHT.
      GO TO HB30.

       HB900.
      CLOSE GLTRNB.

       HB999.
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
