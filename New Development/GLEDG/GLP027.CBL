      $set LINKCOUNT"320" GNT"GLP027.GNT"
      ******************************************************************
      *                                                                *
      *    ******   **        *******     ****     ******   ********   *
      *   **    **  **        **    **   **  **   **    **       **    *
      *   **        **        **    **  **    **       **       **     *
      *   **        **        *******   **    **     **        **      *
      *   **   ***  **        **        **    **   **         **       *
      *   **    **  **        **         **  **   **          **       *
      *    ******   ********  **          ****   ********     **       *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     G / L E D G E R   S U B / G R O U P   S C H E D U L E S    *
      *                                                                *
      *       Version 9.04.02 - June 2017                              *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        GLP027.
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

       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "PARAM.FDE".

       COPY "GACCNT.FDE".

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
       77  WS-GLKEY                PIC  9(04)    COMP-5 VALUE 1.
       77  WS-GTRKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-GINKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-NETKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-KSTORE               PIC  9(04)    COMP-5.
       77  WS-ISUB                 PIC  9(04)    COMP-5.
       77  WS-CHEQUE               PIC  9(04)    COMP-5.
       77  WS-PAGE                 PIC  9(04)    COMP-5.
       77  WS-LINES                PIC  9(02)           VALUE 66.
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S5                   PIC  9(04)    COMP-5.
       77  WS-S6                   PIC  9(04)    COMP-5.
       77  WS-S7                   PIC  9(04)    COMP-5.
       77  WS-S8                   PIC  9(04)    COMP-5.
       77  WS-S9                   PIC  9(04)    COMP-5.
       77  WS-S10                  PIC  9(04)    COMP-5.
       77  WS-S11                  PIC  9(04)    COMP-5.
       77  WS-IXS1                 PIC  9(04)    COMP-5.
       77  WS-OPEN                 PIC  9(02)           VALUE ZERO.
       77  WS-EXS                  PIC  9(01).
       77  WS-NUM                  PIC  Z9.
       77  WS-SCH                  PIC  X(01).
       77  WS-RECON                PIC  X(01).
       77  WS-ZERO                 PIC  9(01).
       77  WS-MM                   PIC  9(02)    COMP-5 VALUE ZERO.
       77  WS-MTH                  PIC  9(02)    COMP-5.
       77  WS-F-YR                 PIC  9(02).
       77  WS-E-YR                 PIC  9(02).
       77  WS-OPTION               PIC  X(01).
       77  WS-ERROR                PIC  9(01)           VALUE ZERO.
       77  WS-PGE-LENGTH           PIC  9(02)           VALUE 66.
       77  WS-PRN-LENGTH           PIC  9(02)           VALUE 62.
       77  WS-TNEXT                PIC  9(04)    COMP-5.
       77  WS-PRN-NO               PIC  X(01)    COMP-X VALUE 9.
       77  WS-PRN-STAT             PIC  X(01)    COMP-X.
       77  WS-H1                   PIC  X(05)           VALUE "DATE:".
       77  WS-H3                   PIC  X(06)           VALUE "BATCH:".
       77  WS-H4                   PIC  X(05)           VALUE "PAGE:".
       77  WS-ER1                  PIC  X(07)           VALUE "Account".
       77  WS-ER3                  PIC  X(04)           VALUE "Date".
       77  WS-ER4                  PIC  X(05)           VALUE "Value".
       77  WS-ER5                  PIC  X(13)           VALUE "Cheque number".
       77  WS-ER6                  PIC  X(09)           VALUE "No Record".
       77  WS-ER7                  PIC  X(13)           VALUE "Group Heading".
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "GLP027".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "GENERAL LEDGER -SUB/GROUP SCHEDULES".
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
    03  WS-PROG        PIC  X(03) VALUE "026".

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

          03  B-L1 REDEFINES W02-PRN-LINE.
              05  B-H1        PIC  X(06).
              05  B-DTE       PIC  Z9/99/9999.
              05  FILLER      PIC  X(05).
              05  B-COMP      PIC  X(40).
              05  FILLER      PIC  X(10).
              05  B-H2        PIC  X(06).
              05  B-PGE       PIC  ZZ9.

          03  B-L2 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(04).
              05  B-NO1       PIC  9(04).
              05  B-NO2       PIC  9(04).
              05  FILLER      PIC  X(06).
              05  B-NAME.
           07  FILLER  PIC  X(06).
           07  B-NME2.
               09  FILLER
          PIC  X(10).
               09  B-H3
          PIC  X(24).
              05  B-SDE1      PIC  X(01).
              05  B-LINE.
           07  FILLER  PIC  X(01).
           07  B-VAL   PIC  Z(07)9.99-.
           07  FILLER  PIC  X(01).
              05  B-SDE2      PIC  X(01).
              05  FILLER      PIC  X(16).
              05  B-SDE3      PIC  X(01).
              05  B-LINE2.
           07  FILLER  PIC  X(01).
           07  B-VAL2  PIC  Z(07)9.99-.
           07  FILLER  PIC  X(01).
              05  B-SDE4      PIC  X(01).

          03  R-L1 REDEFINES W02-PRN-LINE.
              05  R-DET       PIC  X(136).

          03  R-L2 REDEFINES W02-PRN-LINE.
              05  R-H1        PIC  X(06).
              05  R-DTE1      PIC  Z9/99/9999.
              05  FILLER      PIC  X(29).
              05  R-H2        PIC  X(40).
              05  R-DESC      PIC  X(38).
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
              05  FILLER      PIC  X(14).
              05  R-AC2       PIC  9999/99.
              05  FILLER      PIC  X(03).
              05  R-NME2      PIC  X(30).
              05  FILLER      PIC  X(27).
              05  R-DT2       PIC  Z(07)9.99 BLANK WHEN ZERO.
              05  FILLER      PIC  X(01).
              05  R-CR2       PIC  Z(07)9.99 BLANK WHEN ZERO.
              05  FILLER      PIC  X(28).

          03  R-L5 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(18).
              05  R-AC3       PIC  9999/99.
              05  FILLER      PIC  X(03).
              05  R-NME3      PIC  X(30).
              05  FILLER      PIC  X(01).
              05  R-DT3       PIC  Z(07)9.99.
              05  FILLER      PIC  X(01).
              05  R-CR3       PIC  Z(07)9.99.
              05  FILLER      PIC  X(50).

          03  R-L6 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(10).
              05  R-BH1       PIC  X(08).
              05  FILLER      PIC  X(01).
              05  R-BH2       PIC  X(31).
              05  R-BH3       PIC  X(08).
              05  R-BHEAD.
           07  FILLER  PIC  X(05).
           07  R-BH4   PIC  X(10).
           07  R-BH5   PIC  X(12).
              05  R-BH6       PIC  X(10).
              05  R-BH7       PIC  X(12).
              05  R-BH8       PIC  X(10).
              05  R-BH9       PIC  X(15).

          03  SCH-L1 REDEFINES W02-PRN-LINE.
              05  S-H1        PIC  X(14).
              05  S-SUBGRP    PIC  X(30).
              05  FILLER      PIC  X(07).
              05  S-TL.
           07  FILLER  PIC  X(04).
           07  S-H2    PIC  X(15).
           07  S-H3    PIC  X(08).
              05  FILLER      PIC  X(02).

          03  SCH-L2 REDEFINES W02-PRN-LINE.
              05  S-H4.
           07  S-ACNO  PIC  9(04)/99.
           07  FILLER  PIC  X(03).
              05  S-ANAME     PIC  X(30).
              05  FILLER      PIC  X(11).
              05  S-DT        PIC  Z(07)9.99 BLANK WHEN ZERO.
              05  FILLER      PIC  X(05).
              05  S-CR        PIC  Z(07)9.99 BLANK WHEN ZERO.
              05  FILLER      PIC  X(02).

          03  TRL-L1 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(19).
              05  TRL-H4      PIC  X(07).
              05  FILLER      PIC  X(12).
              05  TRL-H5      PIC  X(12).
              05  FILLER      PIC  X(33).
              05  TRL-H7      PIC  X(05).
              05  FILLER      PIC  X(16).
              05  TRL-H8      PIC  X(06).
              05  FILLER      PIC  X(22).

          03  TRL-L2 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(19).
              05  TRL-ACC     PIC  9(04)/99.
              05  FILLER      PIC  X(12).
              05  TRL-ANME    PIC  X(30).
              05  FILLER      PIC  X(11).
              05  TRL-DBT     PIC  Z(07)9.99.
              05  FILLER      PIC  X(11).
              05  TRL-CRD     PIC  Z(07)9.99-.
              05  FILLER      PIC  X(19).

          03  STM-L1 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(40).
              05  STM-H1      PIC  X(51).
              05  FILLER      PIC  X(31).
              05  STM-H2      PIC  X(06).
              05  STM-PGE     PIC  ZZZ9.

          03  STM-L2 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(40).
              05  STM-H3      PIC  X(52).
              05  FILLER      PIC  X(40).

          03  STM-L3 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(21).
              05  STM-H4      PIC  X(12).
              05  FILLER      PIC  X(73).
              05  STM-H5      PIC  X(05).
              05  FILLER      PIC  X(21).

          03  STM-L4 REDEFINES W02-PRN-LINE.
              05  STM-H6      PIC  X(47).
              05  FILLER      PIC  X(01).
              05  STM-H7      PIC  X(03).
              05  FILLER      PIC  X(34).
              05  STM-H8      PIC  X(47).

          03  STM-L5 REDEFINES W02-PRN-LINE.
              05  STM-H9      PIC  X(08).
              05  FILLER      PIC  X(02).
              05  STM-H10     PIC  X(03).
              05  FILLER      PIC  X(01).
              05  STM-H11     PIC  X(11).
              05  FILLER      PIC  X(02).
              05  STM-H12     PIC  X(03).
              05  FILLER      PIC  X(01).
              05  STM-H13     PIC  X(11).
              05  FILLER      PIC  X(02).
              05  STM-H14     PIC  X(03).
              05  FILLER      PIC  X(01).
              05  STM-H15     PIC  X(03).
              05  FILLER      PIC  X(10).
              05  STM-H16     PIC  X(11).
              05  FILLER      PIC  X(13).
              05  STM-H17     PIC  X(08).
              05  FILLER      PIC  X(02).
              05  STM-H18     PIC  X(03).
              05  FILLER      PIC  X(01).
              05  STM-H19     PIC  X(11).
              05  FILLER      PIC  X(02).
              05  STM-H20     PIC  X(03).
              05  FILLER      PIC  X(01).
              05  STM-H21     PIC  X(11).
              05  FILLER      PIC  X(02).
              05  STM-H22     PIC  X(03).

          03  STM-L6 REDEFINES W02-PRN-LINE.
              05  STM-V1      PIC  Z(08)-.
              05  FILLER      PIC  X(01).
              05  STM-P1      PIC  ZZZ       BLANK WHEN ZERO.
              05  FILLER      PIC  X(01).
              05  STM-V2      PIC  Z(07)9.99- BLANK WHEN ZERO.
              05  FILLER      PIC  X(01).
              05  STM-P2      PIC  ZZZ       BLANK WHEN ZERO.
              05  FILLER      PIC  X(01).
              05  STM-V3      PIC  Z(07)9.99- BLANK WHEN ZERO.
              05  FILLER      PIC  X(01).
              05  STM-P3      PIC  ZZZ       BLANK WHEN ZERO.
              05  FILLER      PIC  X(01).
              05  STM-GP      PIC  ZZZ       BLANK WHEN ZERO.
              05  FILLER      PIC  X(01).
              05  STM-DESC.
           07  STM-AC  PIC  9(06).
           07  FILLER  PIC  X(01).
           07  STM-ANAME
          PIC  X(25).
              05  FILLER      PIC  X(01).
              05  STM-V4      PIC  Z(08)-.
              05  FILLER      PIC  X(01).
              05  STM-P4      PIC  ZZZ       BLANK WHEN ZERO.
              05  FILLER      PIC  X(01).
              05  STM-V5      PIC  Z(07)9.99- BLANK WHEN ZERO.
              05  FILLER      PIC  X(01).
              05  STM-P5      PIC  ZZZ       BLANK WHEN ZERO.
              05  FILLER      PIC  X(01).
              05  STM-V6      PIC  Z(07)9.99- BLANK WHEN ZERO.
              05  FILLER      PIC  X(01).
              05  STM-P6      PIC  ZZZ       BLANK WHEN ZERO.

          03  LED-L1 REDEFINES W02-PRN-LINE.
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

          03  LED-L2 REDEFINES W02-PRN-LINE.
              05  LED-AC      PIC  9(04)/99.
              05  FILLER      PIC  X(02).
              05  LED-NME     PIC  X(24).
              05  FILLER      PIC  X(04).
              05  LED-TRAN.
           07  LED-DTE PIC  Z9/99/9999.
           07  FILLER  PIC  X(01).
           07  LED-CDE PIC  X(04).
     07  FILLER  PIC  X(01).
     07  LED-BTCH
          PIC  Z(06).
           07  FILLER  PIC  X(02).
           07  LED-REF PIC  Z(07).
           07  FILLER  PIC  X(02).
           07  LED-NAR PIC  X(36).
           07  FILLER  PIC  X(01).
           07  LED-VAL PIC  Z(07)9.99-.
           07  FILLER  PIC  X(01).
           07  LED-BAL PIC  Z(07)9.99-.

    03  REC-L1 REDEFINES W02-PRN-LINE.
              05  REC-AC      PIC  9(04)/99.
              05  FILLER      PIC  X(02).
              05  REC-NME     PIC  X(40).
        05  FILLER      PIC  X(83).

    03  REC-L2 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC X(32).
        05  REC-REC     PIC X(01).
        05  FILLER      PIC X(04).
              05  REC-TRAN.
           07  REC-DTE PIC  Z9/99/9999.
           07  FILLER  PIC  X(01).
           07  REC-CDE PIC  X(04).
           07  FILLER  PIC  X(01).
     07  REC-BTCH
          PIC  Z(06).
           07  FILLER  PIC  X(02).
           07  REC-REF PIC  Z(07).
           07  FILLER  PIC  X(02).
           07  REC-NAR PIC  X(36).
           07  FILLER  PIC  X(01).
           07  REC-VAL PIC  Z(07)9.99-.
           07  FILLER  PIC  X(01).
           07  REC-BAL PIC  Z(07)9.99-.

          03  CHQ-L1 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(04).
              05  CHQ-H2      PIC  X(04).
              05  FILLER      PIC  X(04).
              05  CHQ-H3      PIC  X(05).
              05  FILLER      PIC  X(33).
              05  CHQ-H4      PIC  X(08).
              05  FILLER      PIC  X(30).
              05  CHQ-H1      PIC  X(07).
              05  FILLER      PIC  X(02).
              05  CHQ-H5      PIC  X(07).
              05  FILLER      PIC  X(07).
              05  CHQ-H6      PIC  X(05).
              05  FILLER      PIC  X(07).
              05  CHQ-H7      PIC  X(06).
              05  FILLER      PIC  X(03).

          03  CHQ-L2 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(01).
              05  CHQ-DTE     PIC  Z9/99/9999.
              05  FILLER      PIC  X(01).
              05  CHQ-PAY     PIC  X(36).
              05  FILLER      PIC  X(02).
              05  CHQ-NAR     PIC  X(36).
              05  FILLER      PIC  X(02).
              05  CHQ-NO      PIC  Z(06)9.
              05  FILLER      PIC  X(02).
              05  CHQ-AC      PIC  9(04)/99.
              05  FILLER      PIC  X(01).
              05  CHQ-DT      PIC  Z(07)9.99.
              05  FILLER      PIC  X(01).
              05  CHQ-CR      PIC  Z(07)9.99-.
              05  FILLER      PIC  X(02).

          03  BAL-L1 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(19).
              05  BAL-H1      PIC  X(11).
              05  BAL-H2      PIC  X(07).
              05  BAL-H3      PIC  X(18).
              05  BAL-H4.
           07  FILLER  PIC  X(14).
           07  BAL-H5  PIC  X(12).
              05  BAL-H6      PIC  X(14).
              05  BAL-H7      PIC  X(07).
              05  BAL-H8      PIC  X(05).
              05  BAL-H9      PIC  X(06).
              05  FILLER      PIC  X(19).

          03  BAL-L2 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(20).
              05  BAL-GRP     PIC  9(02).
              05  FILLER      PIC  X(03).
              05  BAL-GRNAME  PIC  X(24).
              05  FILLER      PIC  X(83).

          03  BAL-L3 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(30).
              05  BAL-AC      PIC  9(04).
              05  FILLER      PIC  X(03).
              05  BAL-ACNAME  PIC  X(24).
              05  FILLER      PIC  X(02).
              05  BAL-DEBT    PIC  Z(07)9.99.
              05  FILLER      PIC  X(02).
              05  BAL-CRED    PIC  Z(07)9.99-.
              05  FILLER      PIC  X(01).
              05  BAL-TDBT    PIC  Z(07)9.99.
              05  FILLER      PIC  X(02).
              05  BAL-TCRD    PIC  Z(07)9.99-.
              05  FILLER      PIC  X(18).

          03  DEP-L1 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(22).
              05  DEP-H1      PIC  X(22).
              05  DEP-H2      PIC  X(14).
              05  DEP-H3      PIC  X(08).
              05  DEP-H4      PIC  X(14).
              05  FILLER      PIC  X(19).
              05  DEP-H5      PIC  X(33).

          03  DEP-L2 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(20).
              05  DEP-DATE    PIC  Z9/99/9999.
              05  FILLER      PIC  X(14).
              05  DEP-NAR     PIC  X(36).
              05  FILLER      PIC  X(14).
              05  DEP-VAL     PIC  Z(07)9.99-.
              05  FILLER      PIC  X(26).

          03  P-L2 REDEFINES W02-PRN-LINE.
              05  P-H1        PIC  X(06).
              05  P-DTE1      PIC  Z9/99/9999.
              05  FILLER      PIC  X(18).
              05  P-H2        PIC  X(40).
              05  P-HEAD      PIC  X(49).
              05  P-H4        PIC  X(06).
              05  P-PGE       PIC  ZZ9.

          03  P-L3 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(13).
              05  P-HD1       PIC  X(06).
              05  FILLER      PIC  X(33).
              05  P-HD2       PIC  X(14).
              05  FILLER      PIC  X(05).

          03  P-L4 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(02).
              05  P-HD3       PIC  X(07).
              05  P-HD4       PIC  X(07).
              05  P-HD5       PIC  X(11).
              05  P-HD6       PIC  X(07).
              05  P-HD7       PIC  X(07).
              05  P-HD8       PIC  X(08).
              05  P-HD9       PIC  X(06).
              05  P-HD10      PIC  X(24).
              05  P-HD11      PIC  X(09).
              05  P-HD12      PIC  X(07).
              05  P-HD13      PIC  X(07).
              05  P-HD14      PIC  X(11).
              05  P-HD15      PIC  X(07).
              05  P-HD16      PIC  X(07).
              05  P-HD17      PIC  X(05).

          03  P-L5 REDEFINES W02-PRN-LINE.
              05  P-MTD1      PIC  Z(6)9- BLANK WHEN ZERO.
              05  FILLER      PIC  X(01).
              05  P-MGRP1     PIC  ZZ9.99 BLANK WHEN ZERO.
              05  FILLER      PIC  X(01).
              05  P-MTOT1     PIC  ZZ9.99 BLANK WHEN ZERO.
              05  FILLER      PIC  X(03).
              05  P-LN4.
           07  P-LN6.
               09  P-YTD1
          PIC  Z(6)9- BLANK WHEN ZERO.
           07  FILLER  PIC  X(01).
           07  P-YGRP1 PIC  ZZ9.99 BLANK WHEN ZERO.
           07  FILLER  PIC  X(01).
           07  P-LN7.
               09  P-YTOT1
          PIC  ZZ9.99 BLANK WHEN ZERO.
           07  FILLER  PIC  X(02).
           07  P-ACNO1 PIC  9999.
           07  FILLER  PIC  X(01).
           07  P-DESC  PIC  X(24).
           07  FILLER  PIC  X(01).
           07  P-ACNO2 PIC  9999.
           07  FILLER  PIC  X(02).
           07  P-LN8.
               09  P-MTD2
          PIC  Z(6)9- BLANK WHEN ZERO.
           07  FILLER  PIC  X(01).
           07  P-MGRP2 PIC  ZZ9.99 BLANK WHEN ZERO.
           07  FILLER  PIC  X(01).
           07  P-LN9.
               09  P-MTOT2
          PIC  ZZ9.99 BLANK WHEN ZERO.
              05  FILLER      PIC  X(03).
              05  P-YTD2      PIC  Z(6)9- BLANK WHEN ZERO.
              05  FILLER      PIC  X(01).
              05  P-YGRP2     PIC  ZZ9.99 BLANK WHEN ZERO.
              05  FILLER      PIC  X(01).
              05  P-YTOT2     PIC  ZZ9.99 BLANK WHEN ZERO.

          03  P-L6 REDEFINES W02-PRN-LINE.
              05  P-LN10      PIC  X(22).
              05  FILLER      PIC  X(03).
              05  P-LN11      PIC  X(22).
              05  FILLER      PIC  X(03).
              05  P-LN2       PIC  X(34).
              05  P-LN13      PIC  X(22).
              05  FILLER      PIC  X(03).
              05  P-LN14      PIC  X(22).

       COPY "W05.GL".

       01  W10-H2.
           03  FILLER          PIC X(27).
           03  W10-HEAD        PIC X(13).

       COPY "W15.GL".

       01  W20-TOTALS.
           03  W20-DEBIT       PIC S9(09)V99 COMP-3.
           03  W20-CREDIT      PIC S9(09)V99 COMP-3.
           03  W20-DR          PIC S9(09)V99 COMP-3.
           03  W20-CR          PIC S9(09)V99 COMP-3.
           03  W20-RESULT      PIC S9(09)V99.
           03  W20-RES1 REDEFINES W20-RESULT.
               05  W20-WHOLE   PIC 9(09).
               05  W20-DEC     PIC S9(02).
           03  W20-TOTAL       PIC S9(09)V99 COMP-3.

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
               05  W25-1       PIC ZZZZZZZ9-.
           03  W25-S7V2.
               05  W25-V1      PIC Z(07)9.99CR.
          03  W25-S8V2.
              05  W25-VG1     PIC Z(07)9.99CR.
          03  W25-DATE.
              05  W25-DTE     PIC Z9/99/9999.
           03  W25-REF.
               05  W25-REFER   PIC Z(07).

       01  W30-INACTS.
           02  W30-TAB1.
           03  W30-INCOME-TABLE OCCURS 50.
               05  W30-INACNO  PIC 9(04).
               05  W30-INKEY   PIC 9(05)  COMP-0.
           02  W30-TAB2 REDEFINES W30-TAB1.
               05  W30-ING OCCURS 50.
                   07 W30-INGRP PIC 9(02).
                   07 W30-INSAC PIC 9(04).
           02  W30-TAB3 REDEFINES W30-TAB1.
               05  W30-INGS OCCURS 50.
                   07  FILLER  PIC 9(01).
                   07 W30-INGSUB PIC 9(03).
                   07  FILLER  PIC 9(02).

       01  W30-COACTS.
           02  W30-TAB4.
           03  W30-COST-TABLE OCCURS 50.
               05  W30-COACNO  PIC 9(04).
               05  W30-COKEY   PIC 9(05)  COMP-0.
           02  W30-TAB5 REDEFINES W30-TAB4.
               05  W30-CNG OCCURS 50.
                   07 W30-COGRP  PIC 9(02).
                   07 W30-COSAC  PIC 9(04).
           02  W30-TAB6 REDEFINES W30-TAB4.
               05  W30-COGS OCCURS 50.
                   07 FILLER     PIC 9(01).
                   07 W30-COGSUB PIC 9(03).
                   07 FILLER     PIC 9(02).
       01  W30-MONTHS.
           03  W30-MONTH-NAMES.
               05  W30-ENG     PIC  X(36)    VALUE
                               "JanFebMarAprMayJunJulAugSepOctNovDec".
           03  W30-MONTH REDEFINES W30-MONTH-NAMES
                               PIC  X(03)  OCCURS 12.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50-70.GL".

       COPY "W70.WS".

       01  W90-TOTALS.
           03  W90-ACCUM.
               05  W90-V1      PIC S9(09)V99 COMP-3.
               05  W90-V2      PIC S9(09)V99 COMP-3.
               05  W90-V3      PIC S9(09)V99 COMP-3.
               05  W90-V4      PIC S9(09)V99 COMP-3.
               05  W90-V5      PIC S9(09)V99 COMP-3.
               05  W90-V6      PIC S9(09)V99 COMP-3.
               05  W90-V7      PIC S9(09)V99 COMP-3.
               05  W90-V8      PIC S9(09)V99 COMP-3.
               05  W90-V9      PIC S9(09)V99 COMP-3.
               05  W90-V10     PIC S9(09)V99 COMP-3.
               05  W90-V11     PIC S9(09)V99 COMP-3.
               05  W90-V12     PIC S9(09)V99 COMP-3.
               05  W90-V13     PIC S9(09)V99 COMP-3.
               05  W90-V14     PIC S9(09)V99 COMP-3.
               05  W90-V15     PIC S9(09)V99 COMP-3.
               05  W90-V16     PIC S9(09)V99 COMP-3.
               05  W90-V17     PIC S9(09)V99 COMP-3.
               05  W90-V18     PIC S9(09)V99 COMP-3.
           03  W90-TOT  REDEFINES W90-ACCUM.
               05  W90-VAL     PIC S9(09)V99 COMP-3 OCCURS 18.
           03  W90-PER         PIC 9(03)V9(06) COMP-3.
       01  W90-COMP.
           03  W90-CNAME       PIC X(40).

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

       COPY "LEDLUP.CRT".

       01  S-MTH.
    03  FOREGROUND-COLOR 7 HIGHLIGHT
        BACKGROUND-COLOR 5 PIC 99 USING WS-MM AUTO.

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
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA35.
             PERFORM ZA000.
             PERFORM HA000.

       AA35.
             EXIT PROGRAM.

       COPY "FUNCTION.CRT".

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

       COPY "GACCNT.RD1".

       COPY "GACCNT.LUP".

       COPY "APAC.HLP".

      /
      *    ****    SUB-GROUP SCHEDULES
      *
       HA000      SECTION 5.
       HA00.
             PERFORM ERASE-SCREEN.
            DISPLAY "PRINTING SUB-GROUP SCHEDULES" AT 0227
              WITH FOREGROUND-COLOR 15.
             DISPLAY "Are you sure " AT 0606
                      WITH FOREGROUND-COLOR 7
             "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT
                     "/" WITH FOREGROUND-COLOR 7
             "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT
                     "?" WITH FOREGROUND-COLOR 7.
       HA01.
            ACCEPT WS-OPTION AT 0624
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "N" OR "Y")
               GO TO HA01.
           IF WS-OPTION = "N"
               GO TO HA999.
            MOVE W05-MM  TO WS-MM.
            DISPLAY "Which Month do you want to print" AT 0806.
       HA02.
            ACCEPT S-MTH AT 0839.
           IF WS-MM < 01 OR > 12
               GO TO HA02.
           IF WS-MM = 01
              MOVE "January"  TO W15-MNME
           ELSE
           IF WS-MM = 02
              MOVE "February"  TO W15-MNME
           ELSE
           IF WS-MM = 03
              MOVE "March"  TO W15-MNME
           ELSE
           IF WS-MM = 04
              MOVE "April"  TO W15-MNME
           ELSE
           IF WS-MM = 05
              MOVE "May"  TO W15-MNME
           ELSE
           IF WS-MM = 06
              MOVE "June"  TO W15-MNME
           ELSE
           IF WS-MM = 07
              MOVE "July"  TO W15-MNME
           ELSE
           IF WS-MM = 08
              MOVE "August"  TO W15-MNME
           ELSE
           IF WS-MM = 09
              MOVE "September"  TO W15-MNME
           ELSE
           IF WS-MM = 10
              MOVE "October"  TO W15-MNME
           ELSE
           IF WS-MM = 11
              MOVE "November"  TO W15-MNME
           ELSE
           IF WS-MM = 12
              MOVE "December"  TO W15-MNME.
            DISPLAY W15-MNME AT 0842 WITH FOREGROUND-COLOR 3 HIGHLIGHT.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO HA02.
            MOVE WS-MM   TO WS-S10.
           IF WS-S3 = 1
              MOVE 12   TO WS-S11
           ELSE
              SUBTRACT 1 FROM WS-MM
     GIVING WS-S11.

       HA05.
            DISPLAY "Sub-Group No" AT 1206.
            MOVE ZERO   TO W15-ACCOUNT.

       HA10.
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
            ACCEPT W15-ACNO AT 1220
            WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
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
       GO TO HA10
        END-EVALUATE
              DISPLAY W15-ACNO AT 1220
         WITH FOREGROUND-COLOR 7 HIGHLIGHT
       BACKGROUND-COLOR 5
               IF W15-ACCOUNT = ZERO
                   GO TO HA10.
            PERFORM CLEAR-L50.
           IF W15-ACNO = ZERO
               GO TO HA999.
      MOVE ZERO   TO W15-SER.
            MOVE W15-ACCOUNT  TO G-AC.
      PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
              MOVE "No Record"  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO HA10.
            DISPLAY G-NAME AT 1236
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO HA10.
            MOVE ZERO   TO WS-PAGE W20-DEBIT W20-CREDIT.
            MOVE G-NAME  TO W15-ACNME.
      PERFORM OPEN-PRINTER.
      MOVE "P"   TO WS-COMMAND.

       HA15.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE SPACES  TO B-L1.
            MOVE WS-H1   TO B-H1.
            MOVE W05-TODAY  TO B-DTE.
            MOVE W90-COMP  TO B-COMP.
            MOVE WS-H4   TO B-H2.
            ADD 1   TO WS-PAGE.
            MOVE WS-PAGE  TO B-PGE.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO B-L1.
            MOVE W15-ACNME  TO S-SUBGRP.
      MOVE 0   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE "SCHEDULE FOR:"
     TO S-H1.
            MOVE W15-MNME  TO R-H2.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO B-L1.
            MOVE "DEBIT"  TO S-H2.
            MOVE "CREDIT"  TO S-H3.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO B-L1.

       HA20.
            PERFORM READ-GACCNT-NEXT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
               GO TO HA25.
           IF G-ACNO NOT = W15-ACNO
               GO TO HA25.
    IF NOT (WS-PRINTER = "S")
              DISPLAY G-AC AT 1220
         WITH FOREGROUND-COLOR 7 HIGHLIGHT
       BACKGROUND-COLOR 5
              DISPLAY G-NAME AT 1236
         WITH FOREGROUND-COLOR 3 HIGHLIGHT
       BACKGROUND-COLOR 5.
            MOVE G-AC   TO S-ACNO.
            MOVE G-NAME  TO S-ANAME.
          IF G-VAL(WS-S10) = ZERO
              MOVE G-BAL  TO G-VAL(WS-S10).
    IF WS-S10 = WS-OPEN
              COMPUTE W20-TOTAL = G-VAL(WS-S10) - G-OBAL
    ELSE
              COMPUTE W20-TOTAL = G-VAL(WS-S10) - G-VAL(WS-S11).
           IF W20-TOTAL < ZERO
              MOVE W20-TOTAL  TO S-CR
              ADD W20-TOTAL  TO W20-CREDIT
           ELSE
              MOVE W20-TOTAL  TO S-DT
              ADD W20-TOTAL  TO W20-DEBIT.
          IF W02-LINAGE < (W02-PRN-LENGTH - 2)
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM HA15.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
        GO TO HA30.
            MOVE SPACES  TO SCH-L1.
             GO TO HA20.

       HA25.
             MOVE ALL "-"        TO S-TL.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO SCH-L1.
            MOVE W20-DEBIT  TO S-DT.
            MOVE W20-CREDIT  TO S-CR.
            MOVE "TOTAL"  TO S-H4.
      MOVE 99   TO WS-ADVANCE.
      MOVE "E"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
            GO TO HA05.

       HA30.
            MOVE "**** PRINT ABORTED BY OPERATOR ****"
     TO R-DET.
      MOVE 99   TO WS-ADVANCE.
      MOVE "E"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.

       HA999.
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
             MOVE PAR-UNPROF         TO W70-UNPROF.
             MOVE PAR-BNKGL          TO W70-BNKGL.
             MOVE 4                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
           IF G-YEAR-END = ZERO
               MOVE 2                TO G-YEAR-END.
           IF G-OPEN = ZERO
               ADD 1 G-YEAR-END      GIVING G-OPEN.
             MOVE G-OPEN             TO WS-OPEN.
             ADD 1 G-YEAR-END        GIVING WS-F-YR.
           IF WS-F-YR > 12
              SUBTRACT 12            FROM WS-F-YR.
             MOVE G-YEAR-END         TO WS-E-YR.
             MOVE 1                  TO WS-S1.

       ZA05.
             MOVE ZERO               TO WS-ERROR.
           IF W70-BNKGL = ZERO
               DISPLAY (20, 20) "Parameters not Set up - ENTER"
                                 WITH FOREGROUND-COLOR 14
               ACCEPT (20, 50) WS-OPTION WITH FOREGROUND-COLOR 15 AUTO
              MOVE 1   TO WS-ERROR.
             DISPLAY SPACES AT 5025
             WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
            GO TO ZA999.

      *COPY "ZA49.PRO".
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
      *    ****    I / O   E R R O R   M E S S A G E S
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
                               MOVE "GLP027"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
