      $set LINKCOUNT"320" GNT"GLP024.GNT"
      ******************************************************************
      *                                                                *
      *    ******   **       *******     ****      ******       **     *
      *   **    **  **       **    **   **  **    **    **     ***     *
      *   **        **       **    **  **    **        **     * **     *
      *   **        **       *******   **    **      **      *  **     *
      *   **   ***  **       **        **    **    **       *******    *
      *   **    **  **       **         **  **    **            **     *
      *    ******   ******   **          ****    ********      ****    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *   G / L E D G E R   D E T A I L E D   L I S T                  *
      *                                                                *
      *       Version 9.04.02 - June 2017                              *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.             GLP024.
       AUTHOR.                 James William Lemmon.
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
      *
      *          ********  ********  **        ******** 
      *          **           **     **        **
      *          **           **     **        ** 
      *          *****        **     **        *****
      *          **           **     **        **  
      *          **           **     **        **
      *          **        ********  ********  ********
      *
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "GLP024".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "GENERAL LEDGER -DETAILED ACCOUNT LIST".
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
    03  WS-PROG        PIC  X(03) VALUE "024".

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
              05  FILLER      PIC  X(36).
              05  STM-H3      PIC  X(96).

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
     07  LED-REFER.
           09  LED-REF PIC  Z(07).
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

       01  W10-DATE.
    03  W10-DTE        PIC 99/99/9999.

       COPY "W15.GL".

       01  W16-DATES.
          03  W16-START       PIC 9(08).
          03  W16-SDTE-DMY REDEFINES W16-START.
              05  W16-SDAY    PIC 9(02).
              05  W16-SMNTH   PIC 9(02).
              05  W16-SCENT   PIC 9(02).
              05  W16-SYEAR   PIC 9(02).
          03  W16-S-YMD       PIC 9(08).
          03  W16-STRT-YMD REDEFINES W16-S-YMD.
              05  W16-SCC     PIC 9(02).
              05  W16-SYY     PIC 9(02).
              05  W16-SMM     PIC 9(02).
              05  W16-SDD     PIC 9(02).
          03  W16-END        PIC 9(08).
          03  W16-EDTE-DMY REDEFINES W16-END.
              05  W16-EDAY    PIC 9(02).
              05  W16-EMNTH   PIC 9(02).
              05  W16-ECENT   PIC 9(02).
              05  W16-EYEAR   PIC 9(02).
          03  W16-E-YMD       PIC 9(08).
          03  W16-END-YMD REDEFINES W16-E-YMD.
              05  W16-ECC     PIC 9(02).
              05  W16-EYY     PIC 9(02).
              05  W16-EMM     PIC 9(02).
              05  W16-EDD     PIC 9(02).

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
        05  W25-KEY     PIC 9(04).
        05  W25-SUB     PIC 9(01).
        05  FILLER      PIC 9(02).

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
              05  FILLER      PIC  X(36)    VALUE
                "JANUARY  FEBRUARY MARCH    APRIL    ".
              05  FILLER      PIC  X(36)    VALUE
                "MAY      JUNE     JULY     AUGUST   ".
              05  FILLER      PIC  X(36)    VALUE
                "SEPTEMBEROCTOBER  NOVEMBER DECEMBER ".
          03  W30-MONTH REDEFINES W30-MONTH-NAMES
                PIC  X(09)  OCCURS 12.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50-70.GL".

       COPY "W70.WS".

       01  W80-HEAD.
    03  FILLER        PIC  X(55) VALUE
      "GENERAL LEDGER - DETAILED TRANSACTION LIST PER ACCOUNT".
           03  FILLER          PIC X(17) VALUE "For the period : ".
    03  W80-DATE1       PIC Z9/99/9999.
           03  FILLER          PIC X(04) VALUE " to ".
    03  W80-DATE2       PIC Z9/99/9999.

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

       COPY "FILE.IDS".

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

       01  S07.
    03  LINE 06 COLUMN 25 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC 99/99/9999 USING W16-START AUTO.
    03  LINE 07 COLUMN 25 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC 99/99/9999 USING W16-END AUTO.

       01  S17.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC 9(06) USING W15-ACCOUNT AUTO.

      *COPY "LEDLUP.CRT".

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
             PERFORM EA000.

       AA35.
             EXIT PROGRAM.

       AA50.
             MOVE "H"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DET.
             MOVE WS-H1              TO R-H1.
             MOVE W90-COMP           TO R-H2.
             MOVE WS-H4              TO R-H4.
             ADD 1                   TO WS-PAGE.
             MOVE WS-PAGE            TO R-PGE.
             MOVE W05-TODAY          TO R-DTE1.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.

       AA55.
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

      *
      *   D A T E   C H E C K   R O U T I N E
      *
      *   PERFORM THIS ROUTINE WITH THE DATE IN W10-DATE (PIC 99/99/9999)
      *   WITH THE FORMAT AS DD/MM/CCYY
      *
      *   ON EXITING - WS-ERROR WILL BE ZERO (0) IF THE DATE IS CORRECT
      *                OR ONE (1) IF THE DATE IS INCORRECT.
      *
       CHECK-DATE         SECTION.

       DATE-CHECK.
             UNSTRING W10-DATE DELIMITED BY "/" OR " "
              INTO W20-DD, W20-MM, W20-CCYY
               OVERFLOW GO TO DATE-ERROR.
             MOVE ZERO           TO WS-ERROR.
           IF W20-DATE NOT NUMERIC
               GO TO DATE-ERROR.
           IF W20-DAY < 01 OR > 31
               GO TO DATE-ERROR.
           IF W20-MONTH < 01 OR > 12
               GO TO DATE-ERROR.
           IF W20-MONTH = 01 OR 03 OR 05 OR 07 OR 08 OR 10 OR 12
               GO TO CHECK-DATE-EXIT.
           IF W20-MONTH = 02 AND W20-DAY > 28
               GO TO LEAP-YEAR.
           IF W20-DAY > 30
               GO TO DATE-ERROR.
             GO TO CHECK-DATE-EXIT.

       LEAP-YEAR.
           IF W20-DAY > 29
               GO TO DATE-ERROR.
            DIVIDE W20-CNYR BY 4 GIVING W25-RESULT.
           IF W25-DECIMAL = 0
               GO TO CHECK-DATE-EXIT.

       DATE-ERROR.
             MOVE 1              TO WS-ERROR.

       CHECK-DATE-EXIT.
             EXIT.

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

       COPY "GLTRAN.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AC000           SECTION.

       COPY "CONTROL.WR".

       COPY "GACCNT.LUP".

       COPY "APAC.HLP".

      /
       EA000      SECTION 5.
       EA00.
            PERFORM ERASE-SCREEN.
             DISPLAY "LISTING OF G/L ACCOUNT/S IN DETAIL" AT 0224
                      WITH FOREGROUND-COLOR Grey HIGHLIGHT.
      MOVE 01   TO W16-SDAY.
      MOVE WS-OPEN  TO W16-SMNTH.
      MOVE W05-CC  TO W16-SCENT.
      MOVE W05-YY  TO W16-SYEAR.
    IF W05-MM < WS-OPEN
        SUBTRACT 1  FROM W16-SYEAR.
      MOVE W05-TODAY  TO W16-END.
      DISPLAY "For the period." AT 0412.
      DISPLAY "Start date :" AT 0612.
      DISPLAY "End date   :" AT 0712.
      DISPLAY "Dates to be keyed in as DDMMCCYY" AT 0912
                      WITH FOREGROUND-COLOR Cyan HIGHLIGHT.

       EA02.
             ACCEPT S07.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE 9  TO WS-ERROR
       GO TO EA03
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
       GO TO EA02
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO EA02
        END-EVALUATE.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
              GO TO EA02.
      MOVE W16-START  TO W10-DTE.
             PERFORM CHECK-DATE.
           IF WS-ERROR = 1
              MOVE "Start date" TO WS-ERR-STRING
        PERFORM ERROR-MESSAGE
        GO TO EA02.
      MOVE W20-DTE  TO W16-S-YMD W15-YMD.
      MOVE 01   TO W15-DY.
      MOVE W16-END  TO W10-DTE.
             PERFORM CHECK-DATE.
           IF WS-ERROR = 1
              MOVE "End date"  TO WS-ERR-STRING
        PERFORM ERROR-MESSAGE
        GO TO EA02.
      MOVE W20-DTE  TO W16-E-YMD.
      MOVE W16-START  TO W80-DATE1.
      MOVE W16-END  TO W80-DATE2.
      MOVE 4   TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
            SUBTRACT 1 FROM W16-SMM
     GIVING WS-S10.
           IF WS-S10 = ZERO
              MOVE 12   TO WS-S10.

       EA03.
      EXIT.

       EA04.
      PERFORM SAVE-SCREEN.
            MOVE ZERO   TO WS-PAGE.
      PERFORM OPEN-PRINTER.
      MOVE "P"   TO WS-COMMAND.
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE 3   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.

       EA05.
             PERFORM AA50 THRU AA55.
            MOVE W80-HEAD  TO STM-H3.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE ALL "-"  TO STM-H3.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
             MOVE "ACNT NO"      TO LED-H1.
             MOVE "ACCOUNT NAME" TO LED-H2.
             MOVE "DATE"         TO LED-H3.
             MOVE "CODE"         TO LED-H4.
             MOVE "BATCH"        TO LED-H4A.
             MOVE "REFER"        TO LED-H5.
             MOVE "PAYEE/NARRATIVE"
                                 TO LED-H6.
             MOVE "VALUE"        TO LED-H7.
             MOVE "BALANCE"      TO LED-H8.
      PERFORM CALL-PRINTUTL.
            MOVE ALL "-"  TO LED-H1 LED-H2  LED-H3
                                    LED-H4 LED-H4A LED-H5
                                    LED-H6 LED-H7  LED-H8.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
      MOVE "D"   TO W02-PRN-TYPE.

       EA10.
      PERFORM RESTORE-SCREEN.
            DISPLAY WS-BLNK78 AT 1102 WITH FOREGROUND-COLOR Cyan.
      DISPLAY "Account from:" AT 1112.
      DISPLAY "      " AT 1126
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta.
      DISPLAY "to:" AT 1222.
      DISPLAY "     " AT 1226
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta.
             MOVE 100000         TO W15-ACCOUNT.
             MOVE 999999         TO W15-ENDACC.

       EA12.
            DISPLAY "F1" AT 5002
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Cyan
       "=Help," WITH FOREGROUND-COLOR Blue
      BACKGROUND-COLOR Cyan
       "F2" WITH BACKGROUND-COLOR Cyan
          FOREGROUND-COLOR Brown HIGHLIGHT
             " to do Account Lookup"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
            ACCEPT S17 AT 1126.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE ZERO  TO W15-ACCOUNT
       GO TO EA50
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
       GO TO EA12
   WHEN F2-KEY
       PERFORM AA350
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO EA12
        END-EVALUATE
              DISPLAY S17 AT 1126
               IF W15-ACCOUNT = ZERO
                   GO TO EA12.
            PERFORM CLEAR-L50.
           IF W15-ACCOUNT = ZERO
              GO TO EA50.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³   Store the starting account number.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      MOVE W15-ACCOUNT  TO W15-BEGACC.
           IF W15-BEGACC < 100000
               MOVE 100000       TO W15-BEGACC.
      MOVE W15-BEGACC  TO G-AC.
      PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
    IF WS-F-ERROR = ZERO
        DISPLAY G-NAME AT 1133
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta.
            DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.

       EA13.
             MOVE W15-ENDACC     TO W15-ACCOUNT
            DISPLAY "F1" AT 5002
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Cyan
       "=Help," WITH FOREGROUND-COLOR Blue
      BACKGROUND-COLOR Cyan
       "F2" WITH BACKGROUND-COLOR Cyan
          FOREGROUND-COLOR Brown HIGHLIGHT
             " to do Account Lookup"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
            ACCEPT S17 AT 1226.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE W15-BEGACC
     TO W15-ACCOUNT
       GO TO EA12
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
       GO TO EA13
   WHEN F2-KEY
       PERFORM AA350
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO EA13
        END-EVALUATE
              DISPLAY S17 AT 1226
               IF W15-ACCOUNT = ZERO
                   MOVE W15-BEGACC   TO W15-ENDACC
                   GO TO EA13.
             MOVE W15-ACCOUNT    TO W15-ENDACC.
      MOVE W15-ENDACC  TO G-AC.
      PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
    IF WS-F-ERROR = ZERO
        DISPLAY G-NAME AT 1233
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta.
            PERFORM CLEAR-L50.
           IF (W15-ENDACC = ZERO) OR
              (W15-ENDACC < W15-ACCOUNT)
               MOVE W15-ACCOUNT  TO W15-ENDACC
               GO TO EA13.
             MOVE W15-BEGACC     TO W15-ACCOUNT.
           IF W15-ACCOUNT = W15-ENDACC
               IF W15-SER = ZERO
            MOVE "Group Heading"
     TO WS-ERR-STRING
            PERFORM ERROR-MESSAGE
                    GO TO EA12.
           IF (W15-ACCOUNT NOT = 100000) OR
              (W15-ENDACC NOT = 999999)
               MOVE 1            TO WS-EXS
           ELSE
               MOVE ZERO         TO WS-EXS.
      PERFORM SAVE-SCREEN.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        MOVE W15-BEGACC  TO W15-ACCOUNT
        GO TO EA12.
    IF W15-ACCOUNT > 100000
              MOVE W15-ACCOUNT  TO G-AC
        PERFORM READ-GACCNT THRU READ-GACCNT-EXIT
              IF WS-F-ERROR = 8
           MOVE "No Record"
     TO WS-ERR-STRING
           PERFORM ERROR-MESSAGE
                   GO TO EA12
               END-IF
        GO TO EA18.
      MOVE 100000  TO G-AC.
      PERFORM START-AT-LEDG-AC THRU READ-GACCNT-EXIT.
    IF WS-F-ERROR = 8
        GO TO EA50.

       EA15.
      PERFORM READ-GACCNT-NEXT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
               IF (WS-EXS = ZERO) OR
                  (W15-ACCOUNT = 100000)
                   GO TO EA50
               ELSE
                   GO TO EA10.
       EA18.
    IF NOT (WS-PRINTER = "S")
        DISPLAY "Processing:" AT 1412
        DISPLAY G-AC AT 1424
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta
        DISPLAY G-NAME AT 1431
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta
        DISPLAY "Transaction:" AT 1612.
           IF G-AC > W15-ENDACC
        PERFORM RESTORE-SCREEN
        PERFORM EA02 THRU EA03
        IF WS-ERROR = 9
     GO TO EA50
        END-IF
        GO TO EA10.

       EA20.
           IF G-ASER = ZERO
               GO TO EA45.
           IF G-BAL NOT = ZERO
               GO TO EA22.
             MOVE G-AC           TO T-AC.
      MOVE W15-YMD  TO T-DATE.
             PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
           IF WS-F-ERROR = 11
               GO TO EA45.
             PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
           IF (WS-F-ERROR = 11) OR
       (G-AC NOT = T-AC) OR
       (T-DATE > W16-E-YMD)
               GO TO EA45.
       EA22.
            MOVE G-AC   TO LED-AC.
            MOVE G-NAME  TO LED-NME.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
     TO R-DET
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        GO TO EA55.
            MOVE SPACES  TO R-DET.
            MOVE "BALANCE B/FWD"
     TO LED-NAR.
    IF WS-S10 = WS-E-YR
        IF (G-OBAL NOT > 0.00) AND
    (G-OBAL NOT < 0.00)
     MOVE ZERO  TO W20-TOTAL
        ELSE
           MOVE G-OBAL  TO W20-TOTAL
    ELSE
              MOVE G-VAL (WS-S10)
     TO W20-TOTAL.
             INITIALIZE T-REC.
      MOVE G-AC   TO T-AC.
      MOVE W15-YMD  TO T-DATE.
             PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
           IF WS-F-ERROR = 11
               GO TO EA35.

       EA24.
            PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
           IF (WS-F-ERROR = 11) OR
       (G-AC NOT = T-AC) OR
       (T-DATE > W16-E-YMD)
        MOVE W20-TOTAL  TO LED-BAL
        PERFORM CALL-PRINTUTL
        MOVE SPACES  TO R-DET
               GO TO EA35.
           IF T-SUB = 5
              GO TO EA24.
    IF NOT (WS-PRINTER = "S")
        DISPLAY T-KEY AT 1625
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta.
    IF T-DATE < W16-S-YMD
        ADD T-VAL  TO W20-TOTAL
        GO TO EA24.
             MOVE W20-TOTAL      TO LED-BAL.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
      GO TO EA26.

       EA25.
             PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
           IF (WS-F-ERROR = 11) OR
       (G-AC NOT = T-AC) OR
       (T-DATE > W16-E-YMD)
              GO TO EA35.
           IF T-SUB = 5
               GO TO EA30.
      DISPLAY T-KEY AT 1625
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.

       EA26.
            MOVE T-DATE  TO W15-SYMD.
      MOVE W15-SCN  TO W15-CC.
      MOVE W15-SYR  TO W15-YY.
      MOVE W15-SMT  TO W15-MM.
      MOVE W15-SDY  TO W15-DD.
             MOVE W15-DATE       TO LED-DTE.
           IF T-CODE = 01 OR 02
               MOVE "JNL"        TO LED-CDE
           ELSE
           IF T-CODE = 03 OR 04
              MOVE "CHEQ"  TO LED-CDE
          ELSE
          IF T-CODE = 05 OR 10
              MOVE "BANK"  TO LED-CDE
          ELSE
           IF T-CODE = 06 OR 07
              MOVE "DEP"  TO LED-CDE
           ELSE
          IF T-CODE = 08 OR 09
              MOVE "EFT"  TO LED-CDE
           ELSE
          IF T-CODE = 11 OR 12
              MOVE "PMT"  TO LED-CDE
           ELSE
          IF T-CODE = 13 OR 14
              MOVE "RCPT"  TO LED-CDE.
             MOVE T-BATCH        TO LED-BTCH.
    IF T-REFER NUMERIC
              MOVE T-REF  TO LED-REF
    ELSE
        INSPECT T-REFER REPLACING ALL X"00" BY SPACE
        MOVE T-REFER  TO LED-REFER.
             MOVE T-VAL          TO LED-VAL.
             ADD T-VAL           TO W20-TOTAL.
             MOVE W20-TOTAL      TO LED-BAL.
       EA30.
             MOVE T-NARATIVE     TO LED-NAR.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        MOVE SPACES  TO R-DET
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM EA05
        MOVE SPACES  TO R-DET
              MOVE G-AC  TO LED-AC
              MOVE G-NAME  TO LED-NME.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
     TO R-DET
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        GO TO EA55.
            GO TO EA25.

       EA35.
      DISPLAY WS-BLNK78 AT 0902 WITH FOREGROUND-COLOR Cyan.
      DISPLAY WS-BLNK78 AT 1102 WITH FOREGROUND-COLOR Cyan.
            MOVE "BALANCE C/FWD"
     TO LED-NAR.
          IF W16-E-YMD = W05-TYMD
              MOVE G-BAL  TO LED-BAL
           ELSE
              MOVE W20-TOTAL  TO LED-BAL.
          IF W02-LINAGE > (W02-PRN-LENGTH - 6)
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM EA05
               GO TO EA45.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE ALL "-"  TO LED-TRAN.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
      MOVE 1   TO WS-ADVANCE.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
     TO R-DET
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        GO TO EA55.

       EA45.
           IF W15-ACCOUNT = W15-ENDACC
        PERFORM RESTORE-SCREEN
        PERFORM EA02 THRU EA03
        IF WS-ERROR = 9
     GO TO EA50
        END-IF
              GO TO EA10.
             GO TO EA15.
       EA50.
            MOVE "*****   E N D   R E P O R T   *****"
     TO REC-NME.
      MOVE 99   TO WS-ADVANCE.
      MOVE "E"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.

       EA55.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.

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
             MOVE PAR-UNPROF         TO W70-UNPROF.
             MOVE PAR-BNKGL          TO W70-BNKGL.
             MOVE 4                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
           IF NOT(G-OPEN = ZERO)
               MOVE G-OPEN           TO WS-OPEN
           ELSE
               MOVE 3                TO WS-OPEN.
             ADD 1 G-YEAR-END        GIVING WS-F-YR.
           IF WS-F-YR > 12
               SUBTRACT 12           FROM WS-F-YR.
             MOVE G-YEAR-END         TO WS-E-YR.
             MOVE 1                  TO WS-S1.

       ZA05.
             MOVE ZERO               TO WS-ERROR.
           IF W70-BNKGL = ZERO
               DISPLAY (20, 20) "Parameters not Set up - ENTER"
                                 WITH FOREGROUND-COLOR Brown HIGHLIGHT
               ACCEPT (20, 50) WS-OPTION WITH FOREGROUND-COLOR Grey HIGHLIGHT AUTO
               MOVE 1   TO WS-ERROR.
             DISPLAY SPACES AT 5025
                 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
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
                               MOVE "GLP024"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
