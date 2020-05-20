      $set LINKCOUNT"320" GNT"GLP026.GNT"
      ******************************************************************
      *                                                                *
      *    ******   **       *******     ****      ******    ******    *
      *   **    **  **       **    **   **  **    **    **  **    **   *
      *   **        **       **    **  **    **        **   **         *
      *   **        **       *******   **    **      **     *******    *
      *   **   ***  **       **        **    **    **       **    **   *
      *   **    **  **       **         **  **    **        **    **   *
      *    ******   ******   **          ****    ********    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     G / L E D G E R   C O S T   C E N T R E   P & L            *
      *                                                                *
      *       Version 9.04.02 - June 2017                              *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.             GLP026.
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
       77  WS-PARKEY               PIC  9(04)   COMP-5  VALUE 1.
       77  WS-GLKEY                PIC  9(04)   COMP-5  VALUE 1.
       77  WS-GTRKEY               PIC  9(04)   COMP-5  VALUE 1.
       77  WS-GINKEY               PIC  9(04)   COMP-5  VALUE 1.
       77  WS-NETKEY               PIC  9(04)   COMP-5  VALUE 1.
       77  WS-KSTORE               PIC  9(04)   COMP-5.
       77  WS-ISUB                 PIC  9(04)   COMP-5.
       77  WS-CHEQUE               PIC  9(04)   COMP-5.
       77  WS-PAGE                 PIC  9(04)   COMP-5.
       77  WS-LINES                PIC  9(02)           VALUE 66.
       77  WS-S1                   PIC  9(04)   COMP-5.
       77  WS-S2                   PIC  9(04)   COMP-5.
       77  WS-S3                   PIC  9(04)   COMP-5.
       77  WS-S4                   PIC  9(04)   COMP-5.
       77  WS-S5                   PIC  9(04)   COMP-5.
       77  WS-S6                   PIC  9(04)   COMP-5.
       77  WS-S7                   PIC  9(04)   COMP-5.
       77  WS-S8                   PIC  9(04)   COMP-5.
       77  WS-S9                   PIC  9(04)   COMP-5.
       77  WS-S10                  PIC  9(04)   COMP-5.
       77  WS-S11                  PIC  9(04)   COMP-5.
       77  WS-IXS1                 PIC  9(04)   COMP-5.
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "GLP026".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "GENERAL LEDGER -COST CENTRE PROF & LOSS".
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

       01  W80-HEAD.
           03  W80-H1          PIC X(22).
           03  FILLER          PIC X(18) VALUE "FOR PERIOD ENDING".
          03  W80-DATE        PIC Z9/99/9999.

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
             PERFORM GA000.

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
      *    ****    COST CENTRE PROFIT AND LOSS
      *
       GA000      SECTION 5.
       GA00.
             PERFORM ERASE-SCREEN.
             DISPLAY "PRINTING COST CENTRE PROFIT/LOSS" AT 0225
              WITH FOREGROUND-COLOR Grey HIGHLIGHT .
             DISPLAY "Are you sure " AT 0412
             "Y" WITH FOREGROUND-COLOR Brown HIGHLIGHT "/"
             "N" WITH FOREGROUND-COLOR Brown HIGHLIGHT "?".
       GA05.
            ACCEPT WS-OPTION AT 0430
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "N" OR "Y")
               GO TO GA05.
           IF WS-OPTION = "N"
               GO TO GA999.
             DISPLAY "Have Totals been accumulated (" AT 0612
             "Y" WITH FOREGROUND-COLOR Brown HIGHLIGHT "/"
             "N" WITH FOREGROUND-COLOR Brown HIGHLIGHT ")?".
       GA10.
            ACCEPT WS-OPTION AT 0648
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "N" OR "Y")
               GO TO GA10.
           IF WS-OPTION = "N"
               GO TO GA999.
             DISPLAY "Detailed print (" AT 0812
             "Y" WITH FOREGROUND-COLOR Brown HIGHLIGHT "/"
             "N" WITH FOREGROUND-COLOR Brown HIGHLIGHT ")?".
       GA15.
            ACCEPT WS-OPTION AT 0834
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "N" OR "Y")
               GO TO GA15.
           IF NOT (WS-OPTION = "N")
               MOVE ZERO         TO WS-EXS
           ELSE
               MOVE 1            TO WS-EXS.
             DISPLAY "Print inactive accounts (" AT 1012
             "Y" WITH FOREGROUND-COLOR Brown HIGHLIGHT "/"
             "N" WITH FOREGROUND-COLOR Brown HIGHLIGHT ")?".
       GA20.
            ACCEPT WS-OPTION AT 1043
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "N" OR "Y")
               GO TO GA20.
           IF NOT (WS-OPTION = "N")
               MOVE ZERO         TO WS-ZERO
           ELSE
               MOVE 1            TO WS-ZERO.
       GA25.
            DISPLAY "Cost Centre" AT 1206.
             DISPLAY "Range " AT 2312 WITH FOREGROUND-COLOR Grey
                     "1001" WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     " TO " WITH FOREGROUND-COLOR Grey
                     "1999" WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             MOVE ZERO           TO W15-ACCOUNT.
       GA30.
      DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "=Help,"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "F2" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       " to do Account Lookup - "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "Esc" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "ape to exit"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
            ACCEPT W15-ACNO AT 1219
            WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE ZERO  TO W15-ACCOUNT
       GO TO GA999
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA350
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO GA30
        END-EVALUATE
              DISPLAY W15-ACNO AT 1219
         WITH FOREGROUND-COLOR Grey HIGHLIGHT
       BACKGROUND-COLOR Magenta
               IF W15-ACCOUNT = ZERO
                   GO TO GA30.
            PERFORM CLEAR-L50.
           IF W15-ACNO = ZERO
               GO TO GA999.
           IF W15-ACNO < 1001 OR > 1999
              MOVE "Invalid number 1001 - 1999"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO GA30.
             MOVE ZERO           TO W15-SER.
             MOVE W15-ACNO       TO W15-START.
            MOVE W15-ACCOUNT  TO G-AC.
      PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
              MOVE "No Record"  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO GA30.
            DISPLAY G-NAME AT 1226
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO GA30.
       GA35.
            MOVE "'I'ncome/cost accnts, include 'E'xpenses  [I]"
     TO WS-ERR-MES.
      MOVE "I"   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
      MOVE WS-OPTION  TO WS-SCH.
          IF NOT (WS-SCH = "I" OR "E")
               GO TO GA35.
             DISPLAY "Account :" AT 1412.
             MOVE ZERO           TO WS-PAGE  W20-DEBIT
                                    WS-ERROR W20-CREDIT.
       GA40.
            MOVE W05-MM  TO WS-S7.
           IF WS-S7 = 1
              MOVE 12   TO WS-S8
           ELSE
              SUBTRACT 1 FROM WS-S7
     GIVING WS-S8.
      PERFORM OPEN-PRINTER.
      MOVE "P"   TO WS-COMMAND.
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE 3   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.

       GA45.
             PERFORM AA50 THRU AA55.
    IF WS-COMMAND = "Z"
        GO TO GA175.
            MOVE SPACES  TO R-DET.
             MOVE "COST CENTRE PROF/LOSS" TO W80-H1.
             MOVE W05-TODAY      TO W80-DATE.
             MOVE W80-HEAD       TO STM-H3.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
    IF WS-COMMAND = "Z"
        GO TO GA175.
            MOVE ALL "-"  TO STM-H3.
      PERFORM CALL-PRINTUTL.
    IF WS-COMMAND = "Z"
        GO TO GA175.
            MOVE SPACES  TO R-DET.
             MOVE "YEAR TO DATE" TO STM-H4.
             MOVE "MONTH"        TO STM-H5.
      PERFORM CALL-PRINTUTL.
    IF WS-COMMAND = "Z"
        GO TO GA175.
            MOVE ALL "-"  TO STM-H6.
             MOVE "PERC"         TO STM-H7.
             MOVE ALL "-"        TO STM-H8.
      PERFORM CALL-PRINTUTL.
    IF WS-COMMAND = "Z"
        GO TO GA175.
            MOVE SPACES  TO R-DET.
             MOVE "  TARGET"     TO STM-H9 STM-H17.
             MOVE " %"           TO STM-H10 STM-H12 STM-H14
                                    STM-H18 STM-H20 STM-H22.
             MOVE "     ACTUAL"  TO STM-H11 STM-H19.
             MOVE "  LAST YEAR"  TO STM-H13 STM-H21.
            MOVE "YTD"   TO STM-H15.
             MOVE "DESCRIPTION"  TO STM-H16.
      PERFORM CALL-PRINTUTL.
    IF WS-COMMAND = "Z"
        GO TO GA175.
            MOVE ALL "-"  TO STM-H15 STM-H16.
       GA50.
             MOVE ALL "-"        TO STM-H9  STM-H10 STM-H11 STM-H12
                                    STM-H13 STM-H14 STM-H17 STM-H18
                                    STM-H19 STM-H20 STM-H21 STM-H22.
      PERFORM CALL-PRINTUTL.
    IF WS-COMMAND = "Z"
        GO TO GA175.
            MOVE SPACES  TO R-DET.
      MOVE "D"   TO W02-PRN-TYPE.

       GA55.
             MOVE ZERO           TO W15-GR.
             MOVE 1              TO WS-S11.
       GA60.
             MOVE ZERO           TO W90-VAL (WS-S11).
           IF WS-S11 < 18
               ADD 1             TO WS-S11
               GO TO GA60.
       GA65.
      PERFORM READ-GACCNT-NEXT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
               GO TO GA115.
           IF G-ASER NOT = ZERO
               IF (WS-ZERO = 1) AND (G-BAL = ZERO) AND
                  (G-VAL (WS-S8) = ZERO)
           GO TO GA65.
      *
      *    I S   S T A R T I N G   S U B - G R O U P   Z E R O
      *
           IF W15-STSG = ZERO
      *
      *    T H E N   I S   T H E   G R O U P   E Q U A L
      *
               IF W15-GR = G-GR
     IF NOT (WS-PRINTER = "S")
               DISPLAY G-AC AT 1422
          WITH FOREGROUND-COLOR Cyan HIGHLIGHT
        BACKGROUND-COLOR Magenta
               DISPLAY G-NAME AT 1430
          WITH FOREGROUND-COLOR Cyan HIGHLIGHT
        BACKGROUND-COLOR Magenta
     END-IF
                   GO TO GA90
               ELSE
                   NEXT SENTENCE
           ELSE
      *
      *    T E S T   G R O U P   &   S U B - G R O U P
      *
           IF W15-ACNO = G-ACNO
        IF NOT (WS-PRINTER = "S")
           DISPLAY G-AC AT 1422
      WITH FOREGROUND-COLOR Cyan HIGHLIGHT
    BACKGROUND-COLOR Magenta
           DISPLAY G-NAME AT 1430
      WITH FOREGROUND-COLOR Cyan HIGHLIGHT
    BACKGROUND-COLOR Magenta
        END-IF
               GO TO GA90.
      *
      *    M U S T   T H E   E X P E N S E   A C C O U N T S   B E
      *    P R I N T E D
      *
           IF WS-SCH = "E"
               IF G-GRP > 3
                   GO TO GA115
               ELSE
                   NEXT SENTENCE
           ELSE
           IF G-GRP > 2
               GO TO GA70.
           IF W15-GRP = G-GRP
               PERFORM GA165 THRU GA170
               IF W15-GRP < 4
           SUBTRACT 1  FROM W15-GRP
                   GO TO GA65
               ELSE
      *
      *    M U S T   T H E   E X P E N S E   A C C O U N T S   B E
      *    P R I N T E D
      *
               IF WS-SCH = "E"
                   GO TO GA115
               ELSE
                   GO TO GA65.
    IF NOT (WS-PRINTER = "S")
              DISPLAY G-AC AT 1422
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta
              DISPLAY G-NAME AT 1430
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta.
           IF G-GRP = 1
              MOVE "*** INCOME ***"
     TO STM-ANAME
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        IF WS-COMMAND = "Z"
     GO TO GA175
        END-IF
              MOVE SPACES  TO R-DET
               GO TO GA90.
           IF W15-GRP = 1
               GO TO GA135.
           IF W15-GR = ZERO
               GO TO GA85.
           IF W15-GRP > 2
               GO TO GA90.

       GA70.
             PERFORM GA50.
            MOVE "TOTAL COSTS"  TO STM-DESC.

       GA75.
            MOVE W90-V7  TO STM-V1.
           IF W90-V1 NOT = ZERO
               COMPUTE W90-PER = W90-V7 / W90-V1 * 100
              MOVE W90-PER  TO STM-P1.
            MOVE W90-V8  TO STM-V2.
           IF W90-V2 NOT = ZERO
               COMPUTE W90-PER = W90-V8 / W90-V2 * 100
              MOVE W90-PER  TO STM-P2.
            MOVE W90-V9  TO STM-V3.
           IF W90-V3 NOT = ZERO
               COMPUTE W90-PER = W90-V9 / W90-V3 * 100
              MOVE W90-PER  TO STM-P3.
            MOVE W90-V10  TO STM-V4.
           IF W90-V4 NOT = ZERO
               COMPUTE W90-PER = W90-V10 / W90-V4 * 100
              MOVE W90-PER  TO STM-P4.
            MOVE W90-V11  TO STM-V5.
           IF W90-V5 NOT = ZERO
               COMPUTE W90-PER = W90-V11 / W90-V5 * 100
              MOVE W90-PER  TO STM-P5.
            MOVE W90-V12  TO STM-V6.
           IF W90-V6 NOT = ZERO
               COMPUTE W90-PER = W90-V12 / W90-V6 * 100
              MOVE W90-PER  TO STM-P6.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
    IF WS-COMMAND = "Z"
        GO TO GA175.
            MOVE SPACES  TO R-DET.
             PERFORM GA50.

       GA80.
             MOVE "GROSS PROFIT" TO STM-DESC.
            ADD W90-V1   TO W90-V7.
            ADD W90-V2   TO W90-V8.
            ADD W90-V3   TO W90-V9.
            ADD W90-V4   TO W90-V10.
            ADD W90-V5   TO W90-V11.
            ADD W90-V6   TO W90-V12.
             PERFORM GA75.
           IF WS-SCH = "I"
               GO TO GA126.
           IF W15-STGR < 30
               PERFORM GA165 THRU GA170.

       GA85.
            MOVE "*** EXPENSES ***"
     TO STM-ANAME.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
    IF WS-COMMAND = "Z"
        GO TO GA175.
            MOVE SPACES  TO R-DET.

       GA90.
             MOVE G-ACNO         TO W15-ACNO.
           IF WS-EXS = 1
               IF G-ASER NOT = ZERO
           GO TO GA65.
             MOVE G-NAME         TO STM-ANAME.
             MOVE G-AC           TO STM-AC.
           IF WS-EXS = ZERO
               IF G-ASER = ZERO
                   PERFORM GA140 THRU GA160
                   GO TO GA105.
           IF WS-EXS = 1
               IF G-SR = ZERO
                   IF NOT (G-GR = 10 OR 20 OR 30)
                       PERFORM GA140 THRU GA160
                       GO TO GA105
                   ELSE
                       GO TO GA105.
            MOVE 1   TO WS-S1.
            MOVE 2   TO WS-S2.
            MOVE 3   TO WS-S3.
            MOVE 4   TO WS-S4.
            MOVE 5   TO WS-S5.
            MOVE 6   TO WS-S6.
           IF W15-GRP = 2
              ADD 6   TO WS-S1
              ADD 6   TO WS-S2
              ADD 6   TO WS-S3
              ADD 6   TO WS-S4
              ADD 6   TO WS-S5
              ADD 6   TO WS-S6
           ELSE
           IF W15-GRP = 3
              ADD 12   TO WS-S1
              ADD 12   TO WS-S2
              ADD 12   TO WS-S3
              ADD 12   TO WS-S4
              ADD 12   TO WS-S5
              ADD 12   TO WS-S6.
            MOVE ZERO   TO W20-DEBIT.
           IF WS-S7 < WS-F-YR
              MOVE 1   TO WS-S9
              MOVE 12   TO WS-S10
           ELSE
              MOVE WS-S7  TO WS-S10
              MOVE WS-F-YR  TO WS-S9
               GO TO GA100.
       GA95.
            ADD G-BVAL (WS-S9)  TO W20-DEBIT.
           IF WS-S9 < WS-S7
              ADD 1   TO WS-S9
               GO TO GA95.
            MOVE WS-F-YR  TO WS-S9.
       GA100.
            ADD G-BVAL (WS-S9)  TO W20-DEBIT.
           IF WS-S9 < WS-S10
              ADD 1   TO WS-S9
               GO TO GA100.
           IF W15-GRP = 1
              MULTIPLY -1  BY W20-DEBIT.
            ADD W20-DEBIT  TO W90-VAL (WS-S1).
            MOVE W20-DEBIT  TO STM-V1.
           IF W90-V1 NOT = ZERO
           IF W15-GRP = 1
              MOVE 100   TO STM-P1
           ELSE
               COMPUTE W90-PER = W20-DEBIT / W90-V1 * 100
              MOVE W90-PER  TO STM-P1.
            MOVE G-BAL   TO STM-V2 W20-DEBIT W20-CREDIT.
            ADD W20-DEBIT  TO W90-VAL (WS-S2).
           IF W90-V2 NOT = ZERO
           IF W15-GRP = 1
              MOVE 100   TO STM-P2
           ELSE
               COMPUTE W90-PER = W20-DEBIT / W90-V2 * 100
              MOVE W90-PER  TO STM-P2.
             MOVE G-LVAL (WS-S7) TO STM-V3 W20-DEBIT.
            ADD W20-DEBIT  TO W90-VAL (WS-S3).
           IF W90-V3 NOT = ZERO
           IF W15-GRP = 1
              MOVE 100   TO STM-P3
           ELSE
               COMPUTE W90-PER = W20-DEBIT / W90-V3 * 100
              MOVE W90-PER  TO STM-P3.
           IF W20-TOTAL NOT = ZERO
               COMPUTE W90-PER = W20-CREDIT / W20-TOTAL * 100
              MOVE W90-PER  TO STM-GP.
             MOVE G-BVAL (WS-S7) TO W20-DEBIT.
           IF W15-GRP = 1
              MULTIPLY -1  BY W20-DEBIT.
            MOVE W20-DEBIT  TO STM-V4.
            ADD W20-DEBIT  TO W90-VAL (WS-S4).
           IF W90-V4 NOT = ZERO
           IF W15-GRP = 1
              MOVE 100   TO STM-P4
           ELSE
               COMPUTE W90-PER = W20-DEBIT / W90-V4 * 100
              MOVE W90-PER  TO STM-P4.
            MOVE G-BAL   TO W20-DEBIT.
            SUBTRACT G-VAL (WS-S8)
     FROM W20-DEBIT.
            MOVE W20-DEBIT  TO STM-V5.
            ADD W20-DEBIT  TO W90-VAL (WS-S5).
           IF W90-V5 NOT = ZERO
           IF W15-GRP = 1
              MOVE 100   TO STM-P5
           ELSE
               COMPUTE W90-PER = W20-DEBIT / W90-V5 * 100
              MOVE W90-PER  TO STM-P5.
             MOVE G-LVAL (WS-S7) TO W20-DEBIT.
            SUBTRACT G-LVAL (WS-S8)
     FROM W20-DEBIT.
            MOVE W20-DEBIT  TO STM-V6.
            ADD W20-DEBIT  TO W90-VAL (WS-S6).
           IF W90-V6 NOT = ZERO
           IF W15-GRP = 1
              MOVE 100   TO STM-P6
           ELSE
               COMPUTE W90-PER = W20-DEBIT / W90-V6 * 100
              MOVE W90-PER  TO STM-P6.

       GA105.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM GA45 THRU GA50.
            MOVE SPACES  TO R-DET.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
        GO TO GA175.
       GA110.
             GO TO GA65.

       GA115.
             PERFORM GA50.
            MOVE "TOTAL EXPENDITURE"
     TO STM-DESC.

       GA120.
            MOVE W90-V13  TO STM-V1.
           IF W90-V1 NOT = ZERO
               COMPUTE W90-PER = W90-V13 / W90-V1 * 100
              MOVE W90-PER  TO STM-P1.
            MOVE W90-V14  TO STM-V2.
           IF W90-V2 NOT = ZERO
               COMPUTE W90-PER = W90-V14 / W90-V2 * 100
              MOVE W90-PER  TO STM-P2.
            MOVE W90-V15  TO STM-V3.
           IF W90-V3 NOT = ZERO
               COMPUTE W90-PER = W90-V15 / W90-V3 * 100
              MOVE W90-PER  TO STM-P3.
            MOVE W90-V16  TO STM-V4.
           IF W90-V4 NOT = ZERO
               COMPUTE W90-PER = W90-V16 / W90-V4 * 100
              MOVE W90-PER  TO STM-P4.
            MOVE W90-V17  TO STM-V5.
           IF W90-V5 NOT = ZERO
               COMPUTE W90-PER = W90-V17 / W90-V5 * 100
              MOVE W90-PER  TO STM-P5.
            MOVE W90-V18  TO STM-V6.
           IF W90-V6 NOT = ZERO
               COMPUTE W90-PER = W90-V18 / W90-V6 * 100
              MOVE W90-PER  TO STM-P6.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
    IF WS-COMMAND = "Z"
        GO TO GA175.
            MOVE SPACES  TO R-DET.
             PERFORM GA50.

       GA125.
            ADD W90-V13  TO W90-V7.
            ADD W90-V14  TO W90-V8.
            ADD W90-V15  TO W90-V9.
            ADD W90-V16  TO W90-V10.
            ADD W90-V17  TO W90-V11.
            ADD W90-V18  TO W90-V12.
            MOVE "TOTAL PROFIT/LOSS THIS MONTH"
     TO STM-DESC.
             PERFORM GA75.

       GA126.
      MOVE 99   TO WS-ADVANCE.
      MOVE "E"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.

       GA130.
             GO TO GA999.

       GA135.
             PERFORM GA50.
            MOVE 100   TO STM-P1 STM-P2 STM-P3 STM-P4
              STM-P5 STM-P6.
            MOVE W90-V1  TO STM-V1.
            MOVE W90-V2  TO STM-V2.
            MOVE W90-V3  TO STM-V3.
            MOVE W90-V4  TO STM-V4.
            MOVE W90-V5  TO STM-V5.
            MOVE W90-V6  TO STM-V6.
             MOVE "TOTAL INCOME" TO STM-DESC.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
    IF WS-COMMAND = "Z"
        GO TO GA175.
            MOVE SPACES  TO R-DET.
             PERFORM GA50.
           IF G-GRP = 3
               GO TO GA85.
            MOVE "*** COSTS ***"
     TO STM-ANAME.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
    IF WS-COMMAND = "Z"
        GO TO GA175.
            MOVE SPACES  TO R-DET.
           IF W15-STGR < 20
               PERFORM GA165 THRU GA170.
             GO TO GA90.

       GA140.
           IF WS-EXS = 0
               IF G-SR = ZERO
                   GO TO GA155.
            MOVE ZERO   TO W20-DEBIT.
           IF WS-S7 < WS-F-YR
              MOVE 1   TO WS-S9
              MOVE 12   TO WS-S10
           ELSE
              MOVE WS-S7  TO WS-S10
              MOVE WS-F-YR  TO WS-S9
               GO TO GA150.

       GA145.
            ADD G-BVAL (WS-S9)  TO W20-DEBIT.
           IF WS-S9 < WS-S7
              ADD 1   TO WS-S9
               GO TO GA145.
            MOVE WS-F-YR  TO WS-S9.

       GA150.
            ADD G-BVAL (WS-S9)  TO W20-DEBIT.
           IF WS-S9 < WS-S10
              ADD 1   TO WS-S9
               GO TO GA150.
           IF W15-GRP = 1
              MULTIPLY -1  BY W20-DEBIT.
            MOVE W20-DEBIT  TO STM-V1.
            MOVE G-BAL   TO STM-V2.
             MOVE G-LVAL (WS-S7) TO STM-V3.
             MOVE G-BVAL (WS-S7) TO W20-DEBIT.
           IF W15-GRP = 1
              MULTIPLY -1  BY W20-DEBIT.
            MOVE W20-DEBIT  TO STM-V4.
            MOVE G-BAL   TO W20-DEBIT.
            SUBTRACT G-VAL (WS-S8)
     FROM W20-DEBIT.
            MOVE W20-DEBIT  TO STM-V5.
             MOVE G-LVAL (WS-S7) TO W20-DEBIT.
            SUBTRACT G-LVAL (WS-S8)
     FROM W20-DEBIT.
            MOVE W20-DEBIT  TO STM-V6.

       GA155.
      MOVE 0   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.

       GA160.
             EXIT.

       GA165.
             ADD 10              TO W15-STGR.
             MOVE W15-START      TO W15-ACNO.
           IF W15-STGR > 39
               GO TO GA170.
             MOVE ZERO           TO W15-SER.
            MOVE W15-ACCOUNT  TO G-AC.
      PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 0
              GO TO GA170.
             GO TO GA165.

       GA170.
             EXIT.

       GA175.
            MOVE "**** PRINT ABORTED BY OPERATOR ****"
     TO R-DET.
      MOVE 99   TO WS-ADVANCE.
      MOVE "E"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.

       GA180.
      EXIT PROGRAM.

       GA999.
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
             MOVE G-OPEN             TO WS-OPEN.
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
               MOVE 1 TO WS-ERROR.
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
               WHEN 15         MOVE WS-PARID     TO WS-FILE
                               MOVE WS-PARKEY    TO WS-KEY
               WHEN OTHER      MOVE "ERROR"      TO WS-FILE
                               MOVE "GLP026"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
