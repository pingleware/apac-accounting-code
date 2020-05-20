      $set LINKCOUNT"512" GNT"GLPRCT.GNT"
      ******************************************************************
      *                                                                *
      *    ******   **       ******* *******    *****   ********    *
      *   ** **  **       **    ** **    **  **   **     **       *
      *   **     **       **    ** **    **  **       **       *
      *   **     **       ******* *******   **       **       *
      *   **   ***  **       ** **   **   **       **       *
      *   ** **  **       ** **    **  **   **     **       *
      *    ******   ********  ** **    **   *****      **       *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     G / L E D G E R   P E T T Y   C A S H   D E P O S I T S    *
      *                                                                *
      *       Version 9.04.02 - June 2017                              *
      *               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        GLPRCT.
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

       COPY "GLJRN.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "PARAM.FDE".

       COPY "GACCNT.FDE".

       COPY "GLTRAN.FDE".

       COPY "GLJRN.FD".

       COPY "RECOVER.FD".

       COPY "SHARED.FDE".

      /
       WORKING-STORAGE SECTION.
       77  WS-CHECK    PIC X(18)  VALUE
      "aeWlimemnomLalismJ".
       77  WS-PARKEY    PIC 9(04)  COMP-5 VALUE 1.
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
       77  WS-SHARED    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
       77  WS-ISUB         PIC  9(04)    COMP-5.
       77  WS-PAGE    PIC 9(04)  COMP-5.
       77  WS-SUB    PIC 9(04)  COMP-5.
       77  WS-S            PIC  9(04)    COMP-5.
       77  WS-S1           PIC  9(04)    COMP-5.
       77  WS-S2           PIC  9(04)    COMP-5.
       77  WS-S3           PIC  9(04)    COMP-5.
       77  WS-S4    PIC 9(04)  COMP-5.
       77  WS-S5    PIC 9(04)  COMP-5.
       77  WS-S6           PIC  9(04)    COMP-5.
       77  WS-S7    PIC 9(04)  COMP-5.
       77  WS-S8    PIC 9(04)  COMP-5.
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
       77  WS-PGE-LENGTH   PIC 9(02)  VALUE 66.
       77  WS-PRN-LENGTH   PIC 9(02)  VALUE 62.
       77  WS-BATCH    PIC 9(05)  COMP-3.
       77  WS-TNEXT        PIC  9(04)    COMP-5.
       77  WS-PERIOD       PIC  9(02)    COMP-5 VALUE ZERO.
       77  WS-PRN-NO    PIC X(01)  COMP-X VALUE 9.
       77  WS-PRN-STAT    PIC X(01)  COMP-X.
       77  WS-H1    PIC X(05) VALUE "DATE:".
       77  WS-H3           PIC  X(06) VALUE "BATCH:".
       77  WS-H4           PIC  X(05) VALUE "PAGE:".
       77  WS-SPC    PIC X(40) VALUE SPACES.
       77  WS-ER1          PIC  X(07) VALUE "Account".
       77  WS-ER3          PIC  X(04) VALUE "Date".
       77  WS-ER4          PIC  X(05) VALUE "Value".
       77  WS-ER6    PIC X(09) VALUE "No Record".
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "GLPRCT".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "GENERAL LEDGER -PETTY CASH RECEIPT".
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
    03  WS-PROG        PIC  X(03) VALUE "012".

       01  W00-REC.
          03  W00-NAR.
        05  W00-NME PIC X(30).
        05  FILLER  PIC X(10).

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
        05  FILLER      PIC  X(23).
        05  R-H2        PIC  X(40).
        05  FILLER      PIC  X(15).
        05  R-H3        PIC  X(07).
        05  R-BTCH      PIC  ZZZZ9.
        05  FILLER      PIC  X(17).
        05  R-H4        PIC  X(06).
        05  R-PGE       PIC  ZZ9.

    03  R-L3 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(03).
        05  R-H5        PIC  X(11).
        05  R-H6        PIC  X(05).
        05  FILLER      PIC  X(01).
        05  R-H7        PIC  X(13).
        05  R-H8        PIC  X(05).
        05  FILLER      PIC  X(06).
        05  R-H9        PIC  X(09).
        05  R-H10       PIC  X(12).
        05  R-H11       PIC  X(13).
        05  FILLER      PIC  X(01).
        05  R-H12       PIC  X(09).
        05  R-H13       PIC  X(13).
        05  FILLER      PIC  X(08).
        05  R-H14       PIC  X(14).
        05  R-H15       PIC  X(09).

    03  R-L4 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(03).
        05  R-ACC       PIC  9(04)/99.
        05  FILLER      PIC  X(04).
        05  R-ANME      PIC  X(24).
        05  FILLER      PIC  X(03).
        05  R-DTE2      PIC  Z9/99/9999.
        05  FILLER      PIC  X(03).
        05  R-REF       PIC  Z(07).
        05  FILLER      PIC  X(04).
        05  R-NAR       PIC  X(36).
        05  FILLER      PIC  X(02).
        05  R-DBT       PIC  Z(08)9.99.
        05  FILLER      PIC  X(02).
        05  R-CRD       PIC  Z(08)9.99-.
        05  FILLER      PIC  X(02).

    03  R-L5 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(65).
        05  R-H16       PIC  X(10).
        05  FILLER      PIC  X(01).
        05  R-H17       PIC  X(15).
        05  R-H18       PIC  X(10).
        05  FILLER      PIC  X(31).

    03  R-L6 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(08).
        05  R-H19       PIC  X(11).
        05  R-H20       PIC  X(05).
        05  FILLER      PIC  X(01).
        05  R-H21       PIC  X(13).
        05  R-H22       PIC  X(05).
        05  FILLER      PIC  X(04).
        05  R-H23       PIC  X(06).
        05  FILLER      PIC  X(01).
        05  R-H24       PIC  X(12).
        05  R-H25       PIC  X(05).
        05  FILLER      PIC  X(06).
        05  R-H26       PIC  X(14).
        05  R-H27       PIC  X(10).
        05  R-H28       PIC  X(15).
        05  R-H29       PIC  X(16).

    03  R-L7 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(08).
        05  R-DAC       PIC  9(04)/99.
        05  FILLER      PIC  X(04).
        05  R-ANAME     PIC  X(24).
        05  FILLER      PIC  X(04).
        05  R-BRANCH    PIC  X(24).
        05  FILLER      PIC  X(02).
        05  R-DTE3      PIC  Z9/99/9999.
        05  FILLER      PIC  X(02).
        05  R-DEBT      PIC  Z(08)9.99.
        05  FILLER      PIC  X(04).
        05  R-PETAC     PIC  9(04)/99.
        05  FILLER      PIC  X(02).
        05  R-CRED      PIC  Z(08)9.99-.
        05  FILLER      PIC  X(09).

    03  R-BL2 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(20).
        05  R-BH1       PIC  X(34).
        05  R-BH2       PIC  X(36).
        05  R-BH3       PIC  X(42).

    03  R-BL3 REDEFINES W02-PRN-LINE.
        05  R-BH4.
     07  R-BAC   PIC  9(06).
     07  FILLER  PIC  X(01).
     07  R-BDESC PIC  X(30).
        05  FILLER      PIC  X(95).

    03  R-BL3A REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(06).
        05  R-BH5       PIC  X(10).
        05  R-BH6       PIC  X(10).
        05  R-BH7       PIC  X(10).
        05  R-BH8       PIC  X(10).
        05  R-BH9       PIC  X(10).
        05  R-BH10      PIC  X(10).
        05  R-BH11      PIC  X(10).
        05  R-BH12      PIC  X(10).
        05  R-BH13      PIC  X(10).
        05  R-BH14      PIC  X(10).
        05  R-BH15      PIC  X(10).
        05  R-BH16      PIC  X(10).
        05  R-BH17      PIC  X(06).

    03  R-BL4 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(02).
        05  R-BH18      PIC  X(08).
        05  R-BH19      PIC  X(18).
        05  FILLER      PIC  X(104).

    03 R-BL4A REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(02).
        05  R-BVAL      PIC  Z(08)9- OCCURS 13.

    03  R-BL5 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(02).
        05  R-BH20      OCCURS 13.
     07  R-BH21  PIC   X(09).
     07  FILLER  PIC  X(01).

       COPY "W05.GL".

       01  W10-H2.
          03  FILLER    PIC X(21).
          03  W10-HEAD    PIC X(19).

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

    03  W70-PETGL      PIC 9(06)     COMP-3.

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
    03  W75-GNO       PIC 9(06) COMP-5
                              OCCURS 18.

       01  W90-COMP.
           03  W90-CNAME   PIC  X(40).
      /

       LINKAGE SECTION.

       COPY "CHAIN.LS".

       COPY "FILE-GENL.IDS".

       SCREEN SECTION.

       01  S08.
    02  S08A.
          03  LINE  3 COLUMN 32 FOREGROUND-COLOR 7 HIGHLIGHT
           VALUE "PETTY CASH RECEIPT".
          03  LINE  5 COLUMN 20 VALUE "P/Csh G/L:".
    03        COLUMN 31 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(06) FROM WS-SPC.
    03        COLUMN 38 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 3 HIGHLIGHT
     PIC  X(30) FROM WS-SPC.
          03  LINE  6 COLUMN 20 VALUE "Date     :".
    03        COLUMN 31 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(10) FROM WS-SPC.
          03  LINE  7 COLUMN 20 VALUE "Reference:".
    03        COLUMN 31 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(07) FROM WS-SPC.
          03  LINE  8 COLUMN 20 VALUE "Narrative:".
    03        COLUMN 31 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(36) FROM WS-SPC.
    02  S08B.
          03  LINE 10 COLUMN 20 VALUE "Account  :".
    03        COLUMN 31 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(06) FROM WS-SPC.
    03        COLUMN 38 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 3 HIGHLIGHT
     PIC  X(30) FROM WS-SPC.
          03  LINE 11 COLUMN 20 VALUE "Value    :".
    03        COLUMN 31 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(13) FROM WS-SPC.
    02  S08C.
          03  LINE 13 COLUMN 20 VALUE "Total    :".
    03        COLUMN 31 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 3 HIGHLIGHT
     PIC  X(13) FROM WS-SPC.

       01  S13.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z9/99/9999 USING W15-DATE AUTO.

       01  S14.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z(07)9.99CR USING W15-VAL AUTO.

       01  S16.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC X(07) USING W15-REF AUTO.

       01  S17.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC 9(06) USING W15-ACCOUNT AUTO.

       COPY "LEDLUP.CRT".

      /
       PROCEDURE DIVISION
   USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000        SECTION.
       AA00.
          IF LS0-GLLEV < 1
        MOVE "Not Authorised"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO AA49.
            PERFORM ZA000.
      *
      *  ***  ****  ***** *   *   **** ****  *** *   * ***** ***** ****
      * *   * *   * *   **  *   *   * *   *  *  **  *   *   *     * *
      * *   * ****  ***   * * *   **** ****   *  * * *   *   ***   ****
      * *   * *     *   *  **   * *   *  *  *  **   *   *     * *
      *  ***  *     ***** *   *   * *   * *** *   *   *   ***** * *
      *
      PERFORM OPEN-PRINTER.
      MOVE "P"   TO WS-COMMAND.
      PERFORM DA000.
      CLOSE RECOVER.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.

       AA49.
      EXIT PROGRAM.

       AA50.
      *
      *    ****    Print the audit trail HEADING
      *
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE SPACES  TO R-DET.
            MOVE WS-H1   TO R-H1.
            MOVE W90-CNAME  TO R-H2.
            MOVE WS-H3   TO R-H3.
            MOVE WS-H4   TO R-H4.
            ADD 1   TO WS-PAGE.
            MOVE WS-PAGE  TO R-PGE.
            MOVE WS-BATCH  TO R-BTCH.
            MOVE W05-TODAY  TO R-DTE1.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
            MOVE W10-H2  TO R-H2.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.

       AA55.
             EXIT.

       COPY "FUNCTION.CRT".

       COPY "OPTION.CRT".

       COPY "LOCKED.REC".

       COPY "CLEAR.CRT".

       COPY "PRINTUTL.AS6".

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

       COPY "GLJRN.RD".

       COPY "SHARED.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AC000           SECTION.

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

       COPY "GACCNT.WR".

       COPY "GLTRAN.WR".

       COPY "GLJRN.WR".

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
    IF (W15-CY = WS-L-END) AND
       (W15-MM > WS-Y-END)
        GO TO AE999.
      ADD 12   TO WS-PERIOD.

       AE999.
             EXIT.
      /
      *    THIS ROUTINE UPDATES THE VARIOUS MONTHS FROM TRANSACTION
      *    DATE UP TO THE CURRENT MONTH
      *
       AH000           SECTION.
       AH00.
           IF WS-PERIOD = W05-MM
               GO TO AH999.
           IF WS-PERIOD > 12
               SUBTRACT 13 FROM WS-PERIOD GIVING WS-S3
               PERFORM AH10
               ADD 1 WS-OPEN GIVING WS-MS1
           ELSE
               MOVE WS-PERIOD TO WS-MS1.
       AH05.
             ADD WS-VALUE        TO G-VAL (WS-MS1).
             ADD 1               TO WS-MS1.
           IF WS-MS1 > 12
               MOVE 1            TO WS-MS1.
           IF WS-MS1 = W05-MM
               GO TO AH999.
             GO TO AH05.
       AH10.
             ADD 1               TO WS-S3.
           IF WS-S3 > 12
               MOVE 1            TO WS-S3.
             ADD WS-VALUE        TO G-LVAL (WS-S3).
           IF WS-S3 NOT = WS-OPEN
               GO TO AH10.
      ADD WS-VALUE  TO G-OBAL.
    IF G-AC < 400000 AND W70-UNPROF > 400000
        PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT
        MOVE W70-UNPROF  TO W15-ACCOUNT G-AC
              PERFORM READ-GACCNT THRU READ-GACCNT-EXIT
        IF WS-F-ERROR = 0
     MOVE G-AC  TO T-AC
           PERFORM AY12 THRU AY59
        ELSE
     MOVE GLJ-AC  TO W15-ACCOUNT G-AC
     PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
       AH15.
             EXIT.
       AH999.
             EXIT.

       COPY "GACCNT.LUP".

       COPY "APAC.HLP".

      /
      *       ****   *****   ***    ***   *   *  ***** ****   *   *
      *       *   *  *     * *  *   *  *   *  * *   *  *   *
      *       ****   ***    *    *   *  *   *  *** **** * *
      *       *   *  *     * *  *   *   * *  * *   *  *
      *       *   *  *****   ***    ***     *  ***** *   *  *
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *   THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *        ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *        ³ ORIGINAL ACTION (REC-TYPE)  ÄÄ  RECOVERY PROCESS ³
      *        ³ 0 = RECORD CHANGED      ÄÄ     (REWRITE)    ³
      *        ³ 1 = RECORD WAS ADDED      ÄÄ     (DELETE)    ³
      *        ³ 2 = RECORD WAS DELETED      ÄÄ     (WRITE)    ³
      *        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY000        SECTION.
       AY01.
            MOVE 01   TO REC-FILE.
            MOVE WS-PARKEY  TO REC-KEY.
            PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-RECORD1  TO REC-PARAM.
             GO TO AY50.
       AY12.
            MOVE 12   TO REC-FILE.
            MOVE ZERO   TO REC-KEY REC-TYPE.
           IF WS-SKIP = "N"
               PERFORM READ-GACCNT-LOCK THRU READ-GACCNT-EXIT
           ELSE
               MOVE "N"          TO WS-SKIP.
             MOVE G-REC          TO REC-GACCNT.
             GO TO AY50.
       AY13.
            MOVE 13   TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
             MOVE T-REC          TO REC-GLTRAN.
             GO TO AY50.
       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.
       AY40.
             MOVE 99             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE SPACES         TO REC-DETAIL.
             PERFORM AY50.
             ADD 1               TO WS-TRANS.
           IF WS-RECOVER > 95
               CLOSE RECOVER
               OPEN OUTPUT RECOVER
               CLOSE RECOVER
               OPEN I-O RECOVER
               MOVE ZERO         TO WS-RECOVER.
             GO TO AY59.
       AY50.
             ADD 1               TO WS-RECOVER.
             MOVE WS-RECOVER     TO WS-RECKEY.
             MOVE WS-TRANS       TO REC-TRAN.
             WRITE REC-RECORD.
           IF WS-STATUS NOT = "00"
               DISPLAY "Write error Recovery file - Status " AT 2312
                        WITH FOREGROUND-COLOR 14
                        WS-STATUS WITH FOREGROUND-COLOR 15
               STOP RUN.
       AY59.
             EXIT.
      *
      *    ****    Start processing transaction
      *
       AY60.
    IF WS-SKIP = "Y" OR "A"
               GO TO AY999.
            MOVE 1   TO WS-COUNT.
            MOVE 5   TO WS-SHARED.
            PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
            MOVE 4   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-USERS < 24
               MOVE 1            TO WS-SUB
        MOVE ZERO  TO WS-WAIT
               GO TO AY62.
      *
      *    ****   Q   F U L L  -  W A I T   F O R   4 S E C O N D S
      *
            DISPLAY "WAITING" AT 5051
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12.
            COMMIT.
      ACCEPT WS-STIME FROM TIME.
      MOVE 400   TO WS-WAIT.
            PERFORM LOCK-REC-LOOP.
            DISPLAY SPACE AT 5051
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.
             GO TO AY60.

       AY61.
            MOVE "GL"   TO PAR-PROG(WS-USUB).
            MOVE LS-USER  TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the LEDGER control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
             MOVE 4              TO WS-NETKEY.
            PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             PERFORM AY39 THRU AY59.
            GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N P R O G R E S S
      *
       AY62.
    IF NOT (PAR-PROG(WS-SUB) = SPACES OR
     PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R O R   S T O C K   F I L E S
      *    B E I N G   U P D A T E D
      *
        IF NOT (PAR-PROG(WS-SUB) = SPACES)
           IF PAR-PROG(WS-SUB) = "CG" OR "GL"
      *
      *    ****   Y E S   -   S E T   W A I T P E R I O D
      *
         GO TO AY62-WAIT
     ELSE
         ADD 1  TO WS-SUB
         GO TO AY62
     END-IF
        ELSE
      *
      *    ****   I S T H I S   P R O G R A M   I N T H E  Q
      *
        IF PAR-USR(WS-SUB) = LS-USER
      *
      *    ****   I S I T   N E X T I N   L I N E T O   P R O C E S S
      *
     IF WS-WAIT = ZERO
         GO TO AY63
     ELSE
         GO TO AY62-WAIT
     END-IF
        ELSE
     GO TO AY62-WAIT
        END-IF
    END-IF.
      MOVE LS-USER  TO PAR-USR(WS-SUB).
      MOVE WS-SUB  TO PAR-USERS.
      PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
      GO TO AY62-CHECK.
      *
      *    ****   S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
      MOVE 300   TO WS-WAIT.
    IF NOT (PAR-USR(WS-SUB) = LS-USER)
        IF WS-SUB < 24
     ADD 1  TO WS-SUB
     GO TO AY62.

       AY62-CHECK.
    IF WS-WAIT > ZERO
        COMMIT
        DISPLAY "Waiting" AT 5072
   WITH BACKGROUND-COLOR 3
        FOREGROUND-COLOR 14 BLINK
        ACCEPT WS-STIME FROM TIME
        PERFORM LOCK-REC-LOOP
        DISPLAY "Waiting" AT 5072
   WITH BACKGROUND-COLOR 3
        FOREGROUND-COLOR 14 BLINK
        GO TO AY60.
            DISPLAY SPACE AT 5072
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.

       AY63.
            MOVE WS-SUB  TO WS-USUB.
            GO TO AY61.

       AY65.
             MOVE 4              TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE G-BATCH        TO WS-BATCH.
           IF G-BATCH = 99999
               MOVE 1            TO G-BATCH
           ELSE
               ADD 1             TO G-BATCH.
             PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
             GO TO AY999.

       AY70.
            MOVE 4   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *     NETWORK record 2.
      *
            PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
            PERFORM AY40 THRU AY59.
      MOVE 1   TO WS-USUB.

       AY72.
    IF NOT (PAR-USR(WS-USUB) = LS-USER)
        ADD 1   TO WS-USUB
        GO TO AY72.

       AY75.
            MOVE SPACES  TO PAR-PROG(WS-USUB)
              PAR-USR(WS-USUB).
    IF WS-USUB < 24
        ADD 1 WS-USUB  GIVING WS-SUB
    ELSE
        GO TO AY80.
    IF NOT (PAR-PROG(WS-SUB) = SPACES)
        MOVE PAR-PROG(WS-SUB)
     TO PAR-PROG(WS-USUB)
        MOVE PAR-USR(WS-SUB)
     TO PAR-USR(WS-USUB)
        ADD 1   TO WS-USUB
        GO TO AY75.

       AY80.
            SUBTRACT 1 FROM WS-USUB
     GIVING PAR-USERS.
            PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
            COMMIT.

       AY999.
             EXIT.

       CA000      SECTION 50.
       CA00.
      MOVE "Date in a prior Fin Year -Continue Y/N  [Y]"
     TO WS-ERR-MES.
      MOVE "Y"   TO WS-OPTION.
            PERFORM OPT-MESSAGE.
    IF NOT (WS-OPTION = "Y" OR "N")
        GO TO CA00.
    IF WS-OPTION = "Y"
        MOVE ZERO  TO WS-ERROR
    ELSE
        MOVE 1   TO WS-ERROR.

       CA99.
      EXIT.

      /
       DA000      SECTION 50.
       DA0.
      *
      *    ****   1 7 C P I ( C O N D E N S E D   P R I N T )
      *
      MOVE 0   TO WS-ADVANCE.
      MOVE 3   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.

       DA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S08.

       DA05.
            MOVE ZERO   TO WS-PAGE   W20-CREDIT
        W20-DEBIT W20-STOTAL.
            MOVE SPACES  TO W15-NAR W15-REF.
            MOVE "PETTY CASH RECEIPTS"
     TO W10-HEAD.
      GO TO DA12.

       DA10.
             PERFORM AA50.
            MOVE "ACNT NO"  TO R-H19.
            MOVE ALL "_"  TO R-H20.
            MOVE "ACCOUNT NAME" TO R-H21.
            MOVE ALL "_"  TO R-H22.
            MOVE "DATE"  TO R-H26.
            MOVE "DEBIT"  TO R-H27.
            MOVE "PETTY/C"  TO R-H28.
            MOVE "CREDIT"  TO R-H29.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
      MOVE "D"   TO W02-PRN-TYPE.

       DA12.
            MOVE W70-PETGL  TO W15-PETTY.

       DA15.
            MOVE W15-PETTY  TO W15-ACCOUNT.
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
            ACCEPT S17 AT 0531.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE ZERO  TO W15-ACCOUNT
       GO TO DA65
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA350
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO DA15
        END-EVALUATE
        DISPLAY S17 AT 0531
               IF W15-ACCOUNT = ZERO
           GO TO DA15.
      PERFORM CLEAR-L50.
           IF W15-ACCOUNT = ZERO
               GO TO DA65.
           IF W15-ASER = ZERO
              MOVE WS-ER7  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO DA15.
            MOVE W15-ACCOUNT  TO W15-PETTY.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
              MOVE WS-ER6  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO DA15.
      MOVE 20   TO CLIN.
      MOVE 01   TO CCOL.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 23.
            MOVE G-AC   TO WS-KSTORE.
            MOVE W15-ACCOUNT  TO W15-PETTY.
            MOVE G-NAME  TO W00-NAR.
            DISPLAY G-NAME AT 0538
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      MOVE 15   TO SLIN.
      PERFORM CHECK-SETUP THRU CHECK-EXIT.
          IF WS-OPTION = "N"
               GO TO DA15.
            MOVE ZERO   TO W15-ACCOUNT.
            MOVE ZERO   TO W20-TOTAL W15-VAL.
            MOVE W05-TODAY  TO W15-DATE.
            MOVE 1   TO WS-LAST.

       DA20.
             ACCEPT S13 AT 0631.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO DA15
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO DA20
        END-EVALUATE.
            PERFORM AE000.
           IF WS-ERROR = 1
              MOVE WS-ER3  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO DA20.
    IF WS-PERIOD > 12
        PERFORM CA000
        IF WS-ERROR = 1
     GO TO DA20.

       DA21.
            ACCEPT S16 AT 0731.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO DA20
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO DA21
        END-EVALUATE.
          IF W15-REF = SPACES
              GO TO DA21.
       DA22.
            ACCEPT W15-NAR AT 0831
            WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO DA21
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO DA22
        END-EVALUATE.
          IF W15-NAR = SPACES
               GO TO DA22.
      MOVE 15   TO SLIN.
      PERFORM CHECK-SETUP THRU CHECK-EXIT.
          IF WS-OPTION = "N"
               GO TO DA20.
       DA25.
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
            ACCEPT S17 AT 1031.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE ZERO  TO W15-ACCOUNT
             SUBTRACT 1  FROM WS-LAST
       GO TO DA52
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA350
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO DA25
        END-EVALUATE
        DISPLAY S17 AT 1031
               IF W15-ACCOUNT = ZERO
           GO TO DA25.
      PERFORM CLEAR-L50.
           IF W15-ACCOUNT = ZERO
               SUBTRACT 1        FROM WS-LAST
               GO TO DA52.
           IF W15-ASER = ZERO
              MOVE WS-ER7  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO DA25.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
              MOVE WS-ER6  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO DA25.
            DISPLAY G-NAME AT 1038
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
       DA45.
            ACCEPT S14 AT 1131.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO DA25
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO DA45
        END-EVALUATE.
            MULTIPLY -1  BY W15-VAL.
            DISPLAY S14 AT 1131.
           IF W15-VAL = ZERO
              MOVE WS-ER4  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO DA45.
       DA50.
      MOVE 15   TO SLIN.
      PERFORM CHECK-SETUP THRU CHECK-EXIT.
          IF WS-OPTION = "N"
              MULTIPLY -1  BY W15-VAL
               GO TO DA25.
            ADD W15-VAL  TO W20-CREDIT.
            SUBTRACT W15-VAL  FROM W20-DEBIT.
            SUBTRACT W15-VAL  FROM W20-STOTAL.
            MOVE W20-STOTAL  TO W25-V1.
            DISPLAY W25-S7V2 AT 1331
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
            MOVE WS-LAST  TO WS-GLJKEY.
            MOVE W15-ACCOUNT  TO GLJ-AC.
            MOVE W15-VAL  TO GLJ-VAL.
             PERFORM WRITE-GLJRN THRU WRITE-GLJRN-EXIT.
            ADD 1   TO WS-LAST.
            DISPLAY S08B.
            MOVE ZERO   TO W15-ACCOUNT W15-VAL.
            GO TO DA25.

       DA52.
           IF WS-LAST = ZERO
               GO TO DA62.
            MOVE "'P'rocess receipt, 'C'ancel or 'M'ore  [ ]"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.

       DA52A.
      MOVE 15   TO SLIN.
      PERFORM OPT-SETUP THRU OPT-EXIT.
          IF NOT (WS-OPTION = "P" OR "C" OR "M")
        ADD 1   TO WS-LAST
              GO TO DA52.
    IF WS-OPTION = "M"
        GO TO DA25.
           IF WS-OPTION = "C"
        ADD W20-STOTAL  TO W20-CREDIT
        SUBTRACT W20-STOTAL
     FROM W20-DEBIT
               GO TO DA62.
            DISPLAY "Processing Receipt" AT 5020 WITH
              BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
      *
      *    ****   S T A R T   A U D I T   T R A I L
      *
    IF WS-PAGE = ZERO
              PERFORM AY65 THRU AY999
              PERFORM DA10.
            MOVE 1   TO WS-GLJKEY.
             PERFORM AY60 THRU AY999.

       DA53.
           IF WS-GLJKEY > WS-LAST
               GO TO DA58.
             PERFORM READ-GLJRN THRU READ-GLJRN-EXIT.
            MOVE GLJ-AC  TO W15-ACCOUNT G-AC.
            MOVE GLJ-VAL  TO W15-VAL.
             PERFORM AY12 THRU AY59.
             INITIALIZE T-REC.

       DA55.
      MOVE W15-CC  TO W15-CN.
      MOVE W15-YY  TO W15-YR.
             MOVE W15-MM         TO W15-MT.
             MOVE W15-DD         TO W15-DY.
             MOVE W15-YMD        TO T-DATE.
      MOVE W15-REF  TO T-REFER.
             MOVE W00-NAR        TO T-NARATIVE.
            MOVE 13   TO T-CODE.
             MOVE W15-VAL        TO T-VAL.
             MOVE G-AC           TO T-AC.
             ADD W15-VAL         TO G-CR.
             MOVE W15-VAL        TO WS-VALUE.
             MOVE WS-BATCH       TO T-BATCH.
      MOVE "N"   TO T-FLAG.
             PERFORM AH000.
             ADD WS-VALUE        TO G-VAL (W05-MM).
             ADD W15-VAL         TO G-BAL.
             PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY13 THRU AY59.
             PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
             MOVE W15-ACCOUNT    TO R-DAC.
             MOVE W00-NAR        TO R-ANAME.
             MOVE W15-DATE       TO R-DTE3.
            MOVE W15-PETTY  TO R-PETAC.
             MOVE W15-VAL        TO R-CRED.
          IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM DA10.
             MOVE SPACES         TO R-DET.
             ADD 1               TO WS-GLJKEY.
             GO TO DA53.

       DA58.
            PERFORM CLEAR-L50.
            MOVE WS-KSTORE  TO G-AC.
             PERFORM AY12 THRU AY59.
             INITIALIZE T-REC.

       DA60.
      MOVE W15-CC  TO W15-CN.
             MOVE W15-YY         TO W15-YR.
             MOVE W15-MM         TO W15-MT.
             MOVE W15-DD         TO W15-DY.
             MOVE W15-YMD        TO T-DATE.
            MOVE 14   TO T-CODE.
      MOVE W15-REF  TO T-REFER.
             MOVE W15-NAR        TO T-NARATIVE.
             MOVE W20-STOTAL     TO T-VAL.
             MOVE G-AC           TO T-AC.
             ADD W20-STOTAL      TO G-DT.
             MOVE W20-STOTAL     TO WS-VALUE.
             MOVE WS-BATCH       TO T-BATCH.
      MOVE "N"   TO T-FLAG.
             PERFORM AH000.
             ADD WS-VALUE        TO G-VAL (W05-MM).
             ADD W20-STOTAL      TO G-BAL.
             PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY13 THRU AY59.
             PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
            MOVE W15-PETTY  TO R-DAC.
             MOVE W15-NAR        TO R-ANAME.
             MOVE W15-DATE       TO R-DTE3.
             MOVE W20-STOTAL     TO R-DEBT.
          IF W02-LINAGE < (W02-PRN-LENGTH - 1)
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM DA10.
             MOVE SPACES         TO R-L1.
             PERFORM AY70 THRU AY999.
            CLOSE GLJRN.
             OPEN OUTPUT GLJRN.
             CLOSE GLJRN.
            OPEN I-O GLJRN.

       DA62.
            MOVE "ENTER to continue - E to end  [ ]"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      MOVE 15   TO SLIN.
      PERFORM OPT-SETUP THRU OPT-EXIT.
          IF WS-OPTION = "E"
               GO TO DA65.
             PERFORM DA00.
            MOVE ZERO   TO W20-STOTAL.
             GO TO DA15.

       DA65.
    IF NOT (WS-PAGE = ZERO)
              MOVE "BATCH TOTAL"
     TO R-BRANCH
              MOVE W20-DEBIT  TO R-DEBT
              MOVE W20-CREDIT  TO R-CRED
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
      *
      *    ****   1 O C P I ( N O R M A L P R I N T )
      *
      MOVE 0   TO WS-ADVANCE.
      MOVE 1   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      MOVE "D"   TO W02-PRN-TYPE.
            MOVE SPACES  TO R-DET.

       DA999.
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
            OPEN OUTPUT RECOVER GLJRN.
             CLOSE RECOVER GLJRN.
             OPEN I-O RECOVER GLJRN.
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
      MOVE PAR-PETGL  TO W70-PETGL.
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
    IF WS-L-END = ZERO
        SUBTRACT 1 FROM W05-YY
     GIVING WS-L-END.
    IF G-OPEN = ZERO
        MOVE WS-Y-END  TO G-OPEN.
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

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.

       ZA55.
            MOVE 1   TO WS-S1.
            MOVE SPACES  TO WS-MID-LNE.

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
              MOVE WS-PARID  TO WS-FILE
              MOVE WS-PARKEY  TO WS-KEY
           ELSE
           IF WS-F-ERROR = 18
              MOVE W02-RECOVER  TO WS-FILE
              MOVE WS-RECKEY  TO WS-KEY
           ELSE
           IF WS-F-ERROR = 26
              MOVE W02-GLJRN  TO WS-FILE
              MOVE WS-GLJKEY  TO WS-KEY
          ELSE
          IF WS-F-ERROR = 37
              MOVE W02-SHARED  TO WS-FILE
              MOVE WS-SHARED  TO WS-KEY.
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
