      $set LINKCOUNT"512" GNT"GLPEFT.GNT"
      ******************************************************************
      *                                                                *
      *    ******   **        *******   ********  ********  ********   *
      *   **    **  **        **    **  **        **           **      *
      *   **        **        **    **  **        **           **      *
      *   **        **        *******   *****     *****        **      *
      *   **   ***  **        **        **        **           **      *
      *   **    **  **        **        **        **           **      *
      *    ******   ********  **        ********  **           **      *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     G / L E D G E R   E L E C T R O N I C   F U N D S          *
      *                                                                *
      *       Version 9.04.02 - June 2017                              *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.             GLPEFT.
       AUTHOR.                 J.W. LEMMON (APAC).
       DATE-WRITTEN.           APRIL 2000.

                COPYRIGHT NOTICE: COPYRIGHT (C) 2000 - 2017
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

       COPY "DEPART.SL".

       COPY "GACCNT.SL".

       COPY "GLTRAN.SL".

       COPY "GLJRN.SL".

       COPY "PARAM.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

       COPY "TXTRAN.SL".

       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "DEPART.FDE".

       COPY "GACCNT.FDE".

       COPY "GLTRAN.FDE".

       COPY "GLJRN.FD".

       COPY "PARAM.FDE".

       COPY "RECOVER.FD".

       COPY "SHARED.FDE".

       COPY "TXTRAN.FDE".

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
       77  WS-IXS                  PIC  9(04)    COMP-5.
       77  WS-IXS1                 PIC  9(04)    COMP-5.
       77  WS-IXS2                 PIC  9(04)    COMP-5.
       77  WS-IXS3                 PIC  9(04)    COMP-5.
       77  WS-IXS4                 PIC  9(04)    COMP-5.
       77  WS-PARKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-LTRKEY               PIC  9(06)    COMP-5 VALUE 0.
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
       77  WS-SHARED               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-TRANS                PIC  9(04)    COMP-5 VALUE 1.
       77  WS-ISUB                 PIC  9(04)    COMP-5.
       77  WS-EFT                  PIC  9(04)    COMP-5.
       77  WS-PAGE                 PIC  9(04)    COMP-5.
       77  WS-LINES                PIC  9(04)    COMP-5 VALUE 66.
       77  WS-SUB                  PIC  9(04)    COMP-5.
       77  WS-S                    PIC  9(04)    COMP-5.
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S5                   PIC  9(04)    COMP-5.
       77  WS-S6                   PIC  9(04)    COMP-5.
       77  WS-S7                   PIC  9(04)    COMP-5.
       77  WS-S8                   PIC  9(04)    COMP-5.
       77  WS-S9                   PIC  9(04)    COMP-5.
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
       77  WS-TAX                  PIC  X(01).
       77  WS-SKIP                 PIC  X(01)           VALUE "N".
       77  WS-CHQ-PRN              PIC  X(01)           VALUE "N".
       77  WS-FIRST                PIC  9(02)           VALUE ZERO.
       77  WS-LAST                 PIC  9(03)    COMP-3 VALUE ZERO.
       77  WS-ERROR                PIC  9(01)           VALUE ZERO.
       77  WS-NUM                  PIC  Z9.
       77  WS-PGE-LENGTH           PIC  9(02)           VALUE 66.
       77  WS-PRN-LENGTH           PIC  9(02)           VALUE 62.
       77  WS-BATCH                PIC  9(05)    COMP-3.
       77  WS-TNEXT                PIC  9(04)    COMP-5.
       77  WS-PERIOD               PIC  9(02)    COMP-5 VALUE ZERO.
       77  WS-SPC                  PIC  X(36)           VALUE SPACES.
       77  WS-PRN-NO               PIC  X(01)    COMP-X VALUE 9.
       77  WS-PRN-STAT             PIC  X(01)    COMP-X.
       77  WS-H1                   PIC  X(05)           VALUE "DATE:".
       77  WS-H3                   PIC  X(06)           VALUE "BATCH:".
       77  WS-H4                   PIC  X(05)           VALUE "PAGE:".
       77  WS-ER1                  PIC  X(07)           VALUE "Account".
       77  WS-ER3                  PIC  X(04)           VALUE "Date".
       77  WS-ER4                  PIC  X(05)           VALUE "Value".
       77  WS-ER5                  PIC  X(16)           VALUE "Reference Number".
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "GLPEFT".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "GENERAL LEDGER -ELECTRONIC FUND TRANSFER".
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
    03  WS-PROG        PIC  X(03) VALUE "011".

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
        05  R-REF       PIC  X(07).
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
        05  R-BNKAC     PIC  9(04)/99.
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

       COPY "W05.VAT".

       01  W10-H2.
          03  FILLER    PIC X(20).
          03  W10-HEAD    PIC X(20).

       COPY "W15.GL".

    03  W15-TAXABLE OCCURS 12.
              05  W15-TAXVAL  PIC S9(09)V99 COMP-3.
        05  W15-VATVAL  PIC S9(09)V99 COMP-3.

       01  W20-TOTALS.
           03  W20-DEBIT   PIC S9(09)V99 COMP-3.
           03  W20-CREDIT  PIC S9(09)V99 COMP-3.
          03  W20-TAXABLE PIC S9(09)V99 COMP-3.
          03  W20-VAT    PIC S9(09)V99 COMP-3.
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

       01  S06.
          03  LINE  2 COLUMN 32 FOREGROUND-COLOR 7 HIGHLIGHT
           VALUE "ELECTRONIC FUNDS".
          03  LINE  4 COLUMN  4 VALUE "Bank G/L No:".
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
          03  LINE  5 COLUMN  4 VALUE "Date       :".
    03        COLUMN 17 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(10) FROM WS-SPC.
          03  LINE  6 COLUMN  4 VALUE "Refer. Code:".
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
    03  LINE 14 COLUMN  4 VALUE "V.A.T.     :".
    03        COLUMN 17 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(13) FROM WS-SPC.
    03  LINE 15 COLUMN  4 VALUE "Exclusive  :".
    03        COLUMN 17 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(13) FROM WS-SPC.
          03  LINE 17 COLUMN 46 VALUE "Net Amount Payable :".
          03        COLUMN 67 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(13) FROM WS-SPC.

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
    03  LINE 14 COLUMN  4 VALUE "V.A.T.     :".
    03        COLUMN 17 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(13) FROM WS-SPC.
    03  LINE 15 COLUMN  4 VALUE "Exclusive  :".
    03        COLUMN 17 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC  X(13) FROM WS-SPC.
          03  LINE 17 COLUMN 46 VALUE "Net Amount Payable :".
          03        COLUMN 67 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
           PIC Z(07)9.99CR USING W20-RESULT.

       01  S13.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z9/99/9999 USING W15-DATE AUTO.

       01  S14.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z(07)9.99CR USING W15-TOT AUTO.

       01  S14V.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z(07)9.99CR USING W15-VAT AUTO.

       01  S14T.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z(07)9.99CR USING W15-VAL AUTO.

       01  S16.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
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
           IF LS0-GLLEV < 1
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
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
             MOVE "P"                TO WS-COMMAND.
             PERFORM CA000.
             CLOSE RECOVER.
             MOVE "C"                TO WS-COMMAND.
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

       COPY "DEPART.RD".

       COPY "GACCNT.RD".

       COPY "GLTRAN.RD".

       COPY "GLJRN.RD".

       COPY "PARAM.RD".

       COPY "SHARED.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AC000           SECTION.

       COPY "CONTROL.WR".

       COPY "DEPART.WR".

       COPY "GACCNT.WR".

       COPY "GLTRAN.WR".

       COPY "GLJRN.WR".

       COPY "PARAM.WR".

       COPY "TXTRAN.WR".

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
      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ This routine updates the various months starting from the ³
      *    ³ transaction date up to the current month.         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AH000           SECTION.
       AH00.
    IF WS-PERIOD = W05-MM
               GO TO AH999.
           IF WS-PERIOD > 12
        SUBTRACT 13 FROM WS-PERIOD
     GIVING WS-S3
               PERFORM AH10
        ADD 1 WS-OPEN  GIVING WS-MS1
           ELSE
        MOVE WS-PERIOD  TO WS-MS1.
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

      *
      *       ****   *****  ***    ***   *   *  *****  ****   *   *
      *       *   *  *     *   *  *   *  *   *  *      *   *  *   *
      *       ****   ***   *      *   *  *   *  ***    ****    * *
      *       *   *  *     *   *  *   *   * *   *      *   *    *
      *       *   *  *****  ***    ***     *    *****  *   *    *
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *          THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *        ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *        ³ ORIGINAL ACTION (REC-TYPE)  ÄÄ  RECOVERY PROCESS ³
      *        ³ 0 = RECORD CHANGED          ÄÄ      (REWRITE)    ³
      *        ³ 1 = RECORD WAS ADDED        ÄÄ      (DELETE)     ³
      *        ³ 2 = RECORD WAS DELETED      ÄÄ      (WRITE)      ³
      *        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY000           SECTION.
      *    *************************************************************
      *    ****             P A R A M E T E R   F I L E             ****
      *    *************************************************************
      *AY01.
      *      MOVE 01                 TO REC-FILE.
      *      MOVE WS-PARKEY          TO REC-KEY.
      *      MOVE ZERO               TO REC-TYPE
      *      PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      *      MOVE PAR-RECORD1        TO REC-PARAM.
      *      GO TO AY50.

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
             MOVE 13 TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
             MOVE T-REC          TO REC-GLTRAN.
             GO TO AY50.

       AY19.
             MOVE 19             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
             MOVE TAX-RECORD1    TO REC-TXTRAN.
             GO TO AY50.

       AY38.
             MOVE 38             TO REC-FILE.
            MOVE ZERO   TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
      MOVE DPT-RECORD  TO REC-DEPART.
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
      *      MOVE SHR-STOCK  TO WS-STOCK.
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

      /
       CA000        SECTION 5.
       CA0.
      *    *************************************************************
      *    ****   1 7 C P I ( C O N D E N S E D   P R I N T )
      *    *************************************************************
      MOVE 0   TO WS-ADVANCE.
      MOVE 3   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
            MOVE W05-TODAY  TO W15-DATE.

       CA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S06.

       CA02.
            MOVE ZERO   TO WS-PAGE W20-TOTAL WS-EFT.
            MOVE "G/L ELECTRONIC FUNDS"
     TO W10-HEAD.
      GO TO CA09.

       CA05.
             PERFORM AA50.
             MOVE "ACCNT NO "    TO R-H5.
             MOVE ALL "_"        TO R-H6.
             MOVE "ACCOUNT NAME" TO R-H7.
             MOVE ALL "_"        TO R-H8.
             MOVE "DATE"         TO R-H9.
            MOVE "REF. CODE"  TO R-H10.
             MOVE ALL "_"        TO R-H16.
             MOVE "PAYEE/NARRATIVE"
                                 TO R-H17.
             MOVE ALL "_"        TO R-H18.
             MOVE "DEBIT"        TO R-H14.
             MOVE "CREDIT"       TO R-H15.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
      MOVE "D"   TO W02-PRN-TYPE.

       CA09.
            MOVE W70-BNKGL  TO W15-BANK.

       CA10.
            MOVE SPACES  TO W15-NAR W15-PAYEE
              W15-REF.
             MOVE 1              TO WS-LAST.

       CA15.
             MOVE W15-BANK       TO W15-ACCOUNT.
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
            ACCEPT S17 AT 0417.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE ZERO  TO W15-ACCOUNT
       GO TO CA120
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA350
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO CA15
        END-EVALUATE
        DISPLAY S17 AT 0417
               IF W15-ACCOUNT = ZERO
           GO TO CA15.
      PERFORM CLEAR-L50.
           IF W15-ACCOUNT = ZERO
               GO TO CA120.
           IF W15-ASER = ZERO
              MOVE WS-ER7  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO CA15.
             MOVE W15-ACCOUNT TO W15-BANK.

       CA20.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
              MOVE WS-ER6  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO CA15.
            DISPLAY G-NAME AT 0424
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
            MOVE G-BAL   TO W25-V1.
            DISPLAY W25-S7V2 AT 0467
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
       CA30.
            MOVE G-AC   TO WS-KSTORE.

       CA35.
            ACCEPT S13 AT 0517.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO CA15
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO CA35
        END-EVALUATE.
            PERFORM AE000.
          IF WS-ERROR = 1
               GO TO CA35.
    IF WS-PERIOD > 12
        PERFORM DA000
        IF WS-ERROR = 1
     GO TO CA35.
            PERFORM CLEAR-L50.
      *    *************************************************************
      *    *  Switch the transaction date into YYYYMMDD format      *
      *    *************************************************************
      MOVE W15-CC  TO W15-CN.
             MOVE W15-YY         TO W15-YR.
             MOVE W15-MM         TO W15-MT.
             MOVE W15-DD         TO W15-DY.
      *    *************************************************************
      *    ****    AMEND TO ALLOW FOR MULTIPLE VAT RATES
      *    *************************************************************
    IF W15-YMD < WS-VAT-DATE(1)
        MOVE 7   TO WS-VAT-SUB
    ELSE
        MOVE 1   TO WS-VAT-SUB.
      MOVE W05-VAT(WS-VAT-SUB)
     TO W05-RATE.

       CA40.
            ACCEPT S16 AT 0617.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO CA35
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO CA40
        END-EVALUATE.
          IF W15-REF = SPACES
               GO TO CA40.
      MOVE 17   TO SLIN.
      PERFORM CHECK-SETUP THRU CHECK-EXIT.
          IF WS-OPTION = "N"
               GO TO CA15.

       CA45.
            ACCEPT W15-PAYEE AT 0817
            WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO CA40
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO CA45
        END-EVALUATE.
          IF W15-PAYEE = SPACES
               GO TO CA45.

       CA50.
            ACCEPT W15-NAR AT 0917
            WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO CA45
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO CA50
        END-EVALUATE.
          IF W15-NAR = SPACES
               GO TO CA50.
      MOVE W15-NAR  TO W15-NAR2.
      MOVE 17   TO SLIN.
      PERFORM CHECK-SETUP THRU CHECK-EXIT.
          IF WS-OPTION = "N"
               GO TO CA45.
            MOVE ZERO   TO W20-DEBIT W20-CREDIT
        W20-VAT   W20-TAXABLE.
      MOVE 1   TO WS-S9.

       CA53.
            MOVE ZERO   TO W15-VATVAL(WS-S9)
        W15-TAXVAL(WS-S9).
    IF WS-S9 < 12
        ADD 1   TO WS-S9
        GO TO CA53.

       CA55.
            MOVE ZERO   TO W15-ACCOUNT W15-VAL
        W15-TOT W15-VAT.

       CA60.
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
            ACCEPT S17 AT 1117.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE ZERO  TO W15-ACCOUNT
             SUBTRACT 1  FROM WS-LAST
       GO TO CA82
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA350
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO CA60
        END-EVALUATE
        DISPLAY S17 AT 1117
               IF W15-ACCOUNT = ZERO
                   GO TO CA60.
      PERFORM CLEAR-L50.
           IF W15-ACCOUNT = ZERO
              SUBTRACT 1  FROM WS-LAST
               GO TO CA82.
           IF W15-ASER = ZERO
              MOVE WS-ER7  TO WS-ERR-STRING
        PERFORM ERROR-MESSAGE
               GO TO CA60.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
              MOVE WS-ER6  TO WS-ERR-STRING
        PERFORM ERROR-MESSAGE
               GO TO CA60.
            DISPLAY G-NAME AT 1124
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
            MOVE G-BAL   TO W25-V1.
            DISPLAY W25-S7V2 AT 1167
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.

       CA70.
            ACCEPT W15-NAR2 AT 1217
            WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO CA60
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO CA70
        END-EVALUATE.
          IF W15-NAR2 = SPACES
              GO TO CA70.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     Is this amount VAT inclusive?        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      MOVE "N"   TO WS-OPTION.

       CA75.
      MOVE "Taxable: 'N'o or 'I'ncluded  [N]"
     TO WS-ERR-MES.
      PERFORM OPT-MESSAGE.
      MOVE WS-OPTION  TO WS-TAX.
          IF NOT (WS-TAX = "N" OR "I")
        GO TO CA75.

       CA80.
            ACCEPT S14 AT 1317.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO CA60
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO CA80
        END-EVALUATE.
          IF W15-TOT = ZERO
               GO TO CA80.
    IF WS-TAX = "I"
        COMPUTE W15-VAT ROUNDED =
     ((W15-TOT / (100.00000 + W05-RATE)) * W05-RATE)
        SUBTRACT W15-VAT FROM W15-TOT
     GIVING W15-VAL
    ELSE
        MOVE W15-TOT  TO W15-VAL.
      DISPLAY S14V AT 1417.
      DISPLAY S14T AT 1517.
      MOVE 17   TO SLIN.
      PERFORM CHECK-SETUP THRU CHECK-EXIT.
          IF WS-OPTION = "N"
        MOVE ZERO  TO W15-VAL W15-TOT W15-VAT
        DISPLAY S14V AT 1417
        DISPLAY S14T AT 1517
              GO TO CA60.
          IF W15-TOT > ZERO
              ADD W15-TOT  TO W20-TOTAL.
            ADD W15-VAL  TO W20-DEBIT.
    IF NOT(W15-VAT = ZERO)
        ADD W15-VAT  TO W20-VAT
        ADD W15-VAL  TO W20-TAXABLE.
            MOVE WS-LAST  TO WS-GLJKEY.
      *
      *    ****   L O G   D E T A I L S   T O W O R K   F I L E
      *
            MOVE W15-ACCOUNT  TO GLJ-AC.
            MOVE W15-VAL  TO GLJ-VAL.
      MOVE W15-NAR2  TO GLJ-NAR.
             PERFORM WRITE-GLJRN THRU WRITE-GLJRN-EXIT.
            ADD 1   TO WS-LAST.
      MOVE 11   TO CLIN.
      MOVE 01   TO CCOL.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 23.
      ADD W20-DEBIT W20-VAT
     GIVING W20-RESULT.
            DISPLAY S07.
             GO TO CA55.

       CA82.
          IF WS-LAST = ZERO
               GO TO CA115.
    IF NOT(W20-VAT = ZERO)
              ADD W20-VAT  TO W20-DEBIT.

       CA82A.
            MOVE "'P'rocess E.F.T., 'C'ancel or 'M'ore  [ ]"
     TO WS-ERR-MES.
      MOVE "P"   TO WS-OPTION.
      MOVE 17   TO SLIN.
      PERFORM OPT-SETUP THRU OPT-EXIT.
          IF NOT (WS-OPTION = "P" OR "C" OR "M")
              GO TO CA82A.
    IF WS-OPTION = "M"
        ADD 1   TO WS-LAST
        GO TO CA55.
          IF WS-OPTION = "C"
        SUBTRACT W20-DEBIT
     FROM W20-TOTAL
               GO TO CA115.
            DISPLAY "Processing E.F.T." AT 5020
              WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
      *    *************************************************************
      *    ****   S T A R T   A U D I T   T R A I L
      *    *************************************************************
    IF WS-PAGE = ZERO
              PERFORM AY65 THRU AY999
              PERFORM CA05.
            MOVE 1   TO WS-GLJKEY.
             PERFORM AY60 THRU AY999.

       CA83.
           IF WS-GLJKEY > WS-LAST
               GO TO CA100.
             PERFORM READ-GLJRN THRU READ-GLJRN-EXIT.
            MOVE GLJ-AC  TO W15-ACCOUNT G-AC.
            MOVE GLJ-VAL  TO W15-VAL.
      MOVE GLJ-NAR  TO W15-NAR2.
             PERFORM AY12 THRU AY59.
             INITIALIZE T-REC.

       CA85.
      MOVE W15-CC  TO W15-CN.
             MOVE W15-YY         TO W15-YR.
             MOVE W15-MM         TO W15-MT.
             MOVE W15-DD         TO W15-DY.
             MOVE W15-YMD        TO T-DATE.
            MOVE W15-NAR2  TO T-NARATIVE.
            MOVE W15-REF  TO T-REFER.
             MOVE G-AC           TO T-AC.
            MOVE 08   TO T-CODE.
           IF W15-VAL > ZERO
               ADD W15-VAL       TO G-DT
           ELSE
               ADD W15-VAL       TO G-CR.
             MOVE W15-VAL        TO T-VAL
                                    WS-VALUE.
             MOVE WS-BATCH       TO T-BATCH.
      MOVE "N"   TO T-FLAG.
             PERFORM AH000.
             ADD WS-VALUE        TO G-VAL (W05-MM).
             ADD W15-VAL         TO G-BAL.
             PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY13 THRU AY59.
             MOVE W15-PAYEE      TO T-NARATIVE.
             MOVE 5              TO T-SUB.
             PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY13 THRU AY59.
             PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.

       CA95.
            MOVE W15-ACCOUNT  TO R-ACC.
            MOVE G-NAME  TO R-ANME.
            MOVE W15-DATE  TO R-DTE2.
            MOVE W15-REF  TO R-REF.
            MOVE W15-NAR2  TO R-NAR.
           IF W15-VAL > ZERO
              MOVE W15-VAL  TO R-DBT
           ELSE
              MOVE W15-VAL  TO R-CRD.
          IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM CA05.
            MOVE SPACES  TO R-DET.
            ADD 1   TO WS-GLJKEY
             GO TO CA83.

       CA100.
            PERFORM CLEAR-L50.
           IF W20-DEBIT = ZERO
               GO TO CA115.
      *    ************************************************************
      *    *  C H E C K   F O R   V A T   A N D   U P D A T E      *
      *    ************************************************************
    IF NOT(W20-VAT = ZERO)
        PERFORM EA000.
            MOVE WS-KSTORE  TO G-AC.
             PERFORM AY12 THRU AY59.
             INITIALIZE T-REC.

       CA105.
      MOVE W15-CC  TO W15-CN.
             MOVE W15-YY         TO W15-YR.
             MOVE W15-MM         TO W15-MT.
             MOVE W15-DD         TO W15-DY.
             MOVE W15-YMD        TO T-DATE.
             MOVE W15-NAR        TO T-NARATIVE.
            MOVE W15-REF  TO T-REFER.
             ADD 1               TO G-GLTRAN.
             MOVE G-AC           TO T-AC.
            MOVE 09   TO T-CODE.
             MULTIPLY -1 BY W20-DEBIT
                                 GIVING W20-CREDIT.
          IF W20-CREDIT > ZERO
              ADD W20-CREDIT  TO G-DT
           ELSE
              ADD W20-CREDIT  TO G-CR.
      *      ADD W20-CREDIT  TO G-CR.
             MOVE W20-CREDIT     TO WS-VALUE.
             MOVE WS-BATCH       TO T-BATCH.
      MOVE "N"   TO T-FLAG.
             PERFORM AH000.
             ADD WS-VALUE        TO G-VAL (W05-MM).
             ADD W20-CREDIT      TO G-BAL.
             MOVE W20-CREDIT     TO T-VAL.
             PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY13 THRU AY59.
             MOVE W15-PAYEE      TO T-NARATIVE.
             MOVE 5              TO T-SUB.
             PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY13 THRU AY59.
             PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.

       CA110.
          IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM CA05.
            MOVE G-AC   TO R-ACC.
            MOVE G-NAME  TO R-ANME.
            MOVE W15-DATE  TO R-DTE2.
            MOVE W15-REF  TO R-REF.
            MOVE W15-PAYEE  TO R-NAR.
            MOVE W20-CREDIT  TO R-CRD.
          IF W20-CREDIT > ZERO
              MOVE W20-CREDIT  TO R-DBT
           ELSE
              MOVE W20-CREDIT  TO R-CRD.
          IF W02-LINAGE < (W02-PRN-LENGTH - 2)
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM CA05.
            MOVE SPACES  TO R-DET.
             PERFORM AY70 THRU AY999.
            CLOSE GLJRN.
             OPEN OUTPUT GLJRN.
             CLOSE GLJRN.
            OPEN I-O GLJRN.

       CA115.
            MOVE "ENTER to continue - E to end  [ ]"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      MOVE 17   TO SLIN.
      PERFORM OPT-SETUP THRU OPT-EXIT.
          IF NOT (WS-OPTION = "E")
               PERFORM CA00
               GO TO CA10.

       CA120.
            MOVE "BATCH TOTAL"  TO R-NAR.
            MOVE W20-TOTAL  TO R-DBT.
            MULTIPLY -1 BY W20-TOTAL
     GIVING R-CRD.
      MOVE 99   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      *
      *    ****   1 O C P I ( N O R M A L P R I N T )
      *
      MOVE 0   TO WS-ADVANCE.
      MOVE 1   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      MOVE "D"   TO W02-PRN-TYPE.
            MOVE SPACES  TO R-DET.

       CA999.
             EXIT.

       DA000      SECTION 5.
       DA00.
      MOVE "Date in a prior Fin Year -Continue Y/N  [Y]"
     TO WS-ERR-MES.
      MOVE "Y"   TO WS-OPTION.
            PERFORM OPT-MESSAGE.
    IF NOT (WS-OPTION = "Y" OR "N")
        GO TO DA00.
    IF WS-OPTION = "Y"
        MOVE ZERO  TO WS-ERROR
    ELSE
        MOVE 1   TO WS-ERROR.

       DA99.
      EXIT.

      *    *************************************************************
      *    *  U P D A T E   V A T   F I L E   A N D   G / L   A C N T  *
      *    *************************************************************
       EA000      SECTION 5.
       EA00.
      *    *************************************************************
      *    ****    VAT Control
      *       MULTIPLE RATE TO BE INCLUDED
      *    *************************************************************
      MOVE 1   TO W05-V-RATE.
      MOVE W20-VAT  TO W15-VATVAL(WS-VAT-SUB).
      MOVE W20-TAXABLE  TO W15-TAXVAL(WS-VAT-SUB).
      MOVE W05-VAT-CODE  TO DPT-CODE.
      PERFORM READ-DEPART THRU READ-DEPART-EXIT.
      MOVE 0   TO WS-ACTION.
            PERFORM AY38 THRU AY59.
      MOVE DPT-I-VAT-GL  TO W15-ACCOUNT.
    IF W15-ACCOUNT = ZERO
        PERFORM EA100
        MOVE W15-ACCOUNT  TO DPT-I-VAT-GL.
    IF W20-VAT < ZERO
        ADD W15-VATVAL(WS-VAT-SUB)
     TO DPT-I-VRET-DAY
        DPT-I-VRET-MTD
        DPT-I-VRET-YTD
        ADD W15-TAXVAL(WS-VAT-SUB)
     TO DPT-V-PRET-DAY
        DPT-V-PRET-MTD
        DPT-V-PRET-YTD
    ELSE
        ADD W15-VATVAL(WS-VAT-SUB)
     TO DPT-I-VAT-DAY
        DPT-I-VAT-MTD
        DPT-I-VAT-YTD
        ADD W15-TAXVAL(WS-VAT-SUB)
     TO DPT-V-PUR-DAY
        DPT-V-PUR-MTD
        DPT-V-PUR-YTD.
             PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
            MOVE W15-ACCOUNT  TO G-AC.
            MOVE W20-VAT  TO W15-VAL.
             PERFORM AY12 THRU AY59.
             INITIALIZE T-REC.
            MOVE W15-YMD  TO T-DATE.
             MOVE W15-NAR        TO T-NARATIVE.
            MOVE W15-REF  TO T-REFER.
             MOVE G-AC           TO T-AC.
          IF W15-VAL > ZERO
              MOVE 01   TO T-CODE
               ADD W15-VAL       TO G-DT
           ELSE
               MOVE 02           TO T-CODE
               ADD W15-VAL       TO G-CR.
             MOVE W15-VAL        TO T-VAL
                                    WS-VALUE.
             MOVE WS-BATCH       TO T-BATCH.
             PERFORM AH000.
             ADD WS-VALUE        TO G-VAL (W05-MM).
             ADD W15-VAL         TO G-BAL.
             PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY13 THRU AY59.
            PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
      *    *************************************************************
      *    *     P R I N T V A T T R A N S A C T I O N        *
      *    *************************************************************
            MOVE W15-ACCOUNT  TO R-ACC.
            MOVE G-NAME  TO R-ANME.
            MOVE W15-DATE  TO R-DTE2.
            MOVE W15-REF  TO R-REF.
            MOVE W15-NAR  TO R-NAR.
           IF W15-VAL > ZERO
              MOVE W15-VAL  TO R-DBT
           ELSE
              MOVE W15-VAL  TO R-CRD.
          IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM CA05.
            MOVE SPACES  TO R-DET.
      *    *************************************************************
      *    *     V A T   R E P O R T  F I L E        *
      *    *************************************************************
      MOVE 4   TO TAX-ACTYPE.
    IF W20-VAT > ZERO
        MOVE 01   TO TAX-CODE
        MOVE "1"   TO TAX-TYPE
    ELSE
        MOVE 02   TO TAX-CODE
        MOVE "2"   TO TAX-TYPE.
      MOVE W15-YMD  TO TAX-DATE.
      MOVE W15-REF  TO TAX-REF.
      COMPUTE TAX-VALUE = (W20-VAT + W20-TAXABLE) * -1.
      MOVE ZERO   TO TAX-TAXFREE.
      MOVE WS-KSTORE  TO TAX-AC.
      MOVE ZERO   TO TAX-SEQ.
      MOVE 1   TO WS-S9.

       EA05.
      MULTIPLY -1 BY W15-TAXVAL(WS-S9)
     GIVING TAX-VAL(WS-S9).
      MULTIPLY -1 BY W15-VATVAL(WS-S9)
     GIVING TAX-VAT(WS-S9).
    IF WS-S9 < 12
        ADD 1   TO WS-S9
        GO TO EA05.
            PERFORM WRITE-TXTRAN THRU WRITE-TXTRAN-EXIT.
            MOVE 1   TO WS-ACTION.
            PERFORM AY19 THRU AY59.

       EA99.
      EXIT.
      *    *************************************************************
      *    *   G E T   -   I N P U T   V A T   G / L   A C C O U N T   *
      *    *************************************************************
       EA100      SECTION 5.
       EA105.
            PERFORM SAVE-SCREEN-3.
      MOVE 15   TO SHADE-ROW.
      MOVE 18   TO SHADE-COL.
      MOVE 47   TO SHADE-WIDTH.
      MOVE 3   TO SHADE-LINES.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
        AT 1416 WITH FOREGROUND-COLOR 0
       BACKGROUND-COLOR 3
       "¿" WITH FOREGROUND-COLOR 3 HIGHLIGHT
         BACKGROUND-COLOR 3.
      DISPLAY "³      " AT 1516
        WITH FOREGROUND-COLOR 0
      BACKGROUND-COLOR 3
       "Key in the Input VAT account Number      "
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
      BACKGROUND-COLOR 3
       "³" WITH FOREGROUND-COLOR 3 HIGHLIGHT
         BACKGROUND-COLOR 3.
      DISPLAY "³ " AT 1616 WITH FOREGROUND-COLOR 0
           BACKGROUND-COLOR 3
       "A/C No.                                       "
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
      BACKGROUND-COLOR 3
       "³" WITH FOREGROUND-COLOR 3 HIGHLIGHT
         BACKGROUND-COLOR 3.
      DISPLAY "À" AT 1716 WITH FOREGROUND-COLOR 0
          BACKGROUND-COLOR 3
       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 3.
      PERFORM SCREEN-SHADOW.
      DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Help,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       " to do Account Lookup"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       EA110.
            ACCEPT S17 AT 1626.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA350
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO EA110
        END-EVALUATE
        DISPLAY S17 AT 1626
               IF W15-ACCOUNT = ZERO
           GO TO EA110.
      PERFORM CLEAR-L50.
           IF W15-ACCOUNT = ZERO
              GO TO EA110.
           IF W15-ASER = ZERO
              MOVE WS-ER7  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
              GO TO EA110.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
              MOVE WS-ER6  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
              GO TO EA110.
            DISPLAY G-NAME AT 1633
       WITH FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 3.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO EA110.
      PERFORM RESTORE-SCREEN-3.

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
             OPEN OUTPUT RECOVER GLJRN.
             CLOSE RECOVER GLJRN.
             OPEN I-O RECOVER GLJRN.
             MOVE 1                  TO W05-V-RATE.

       ZA05-VAT.
             MOVE W05-VAT-CODE       TO DPT-CODE.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
             MOVE DPT-R-DATE         TO WS-VAT-DATE(W05-V-RATE).
             MOVE DPT-RATE           TO W05-VAT(W05-V-RATE).
             ADD 6 W05-V-RATE        GIVING WS-S1.
             MOVE DPT-P-RATE         TO W05-VAT(WS-S1).
           IF W05-V-RATE < 6
               ADD 1                 TO W05-V-RATE
               GO TO ZA05-VAT.
             MOVE 6   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DEBGL          TO W70-DEBGL.
             MOVE PAR-CREGL          TO W70-CREGL.
             MOVE PAR-INTGL          TO W70-INTGL.
             MOVE PAR-BNKGL          TO W70-BNKGL.
             MOVE PAR-UNPROF         TO W70-UNPROF.
             MOVE 4                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE G-BATCH            TO WS-BATCH.
           IF G-YEAR-END = ZERO
              MOVE 2   TO G-YEAR-END
              PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
      *    *************************************************************
      *    ****    S E T   U P  M O N T H S   ( Y E A R   E N D)   ****
      *    *************************************************************
            MOVE 1   TO WS-S1.
            MOVE G-YEAR-END  TO WS-S2 WS-Y-END.
            MOVE G-LAST-YE  TO WS-L-END.
    IF WS-L-END = ZERO
        SUBTRACT 1 FROM W05-CY
     GIVING WS-L-END.
    IF G-OPEN = ZERO
        MOVE WS-Y-END  TO G-OPEN.
      MOVE G-OPEN  TO WS-OPEN.
            ADD 1   TO WS-S2.
       ZA15.
           IF WS-S2 > 12
              SUBTRACT 12  FROM WS-S2.
             MOVE W30-MONTH (WS-S2) TO W35-MONTH (WS-S1).
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
               WHEN 7          MOVE W02-DEPART   TO WS-FILE
                               MOVE DPT-CODE     TO WS-KEYX
               WHEN 8          MOVE W02-GACCNT   TO WS-FILE
                               MOVE G-AC         TO WS-KEYX
               WHEN 11         MOVE W02-GLTRAN   TO WS-FILE
                               MOVE T-KEY        TO WS-KEYX
               WHEN 15         MOVE WS-PARID     TO WS-FILE
                               MOVE WS-PARKEY    TO WS-KEY
               WHEN 18         MOVE W02-RECOVER  TO WS-FILE
                               MOVE WS-RECKEY    TO WS-KEY
               WHEN 26         MOVE W02-GLJRN    TO WS-FILE
                               MOVE WS-GLJKEY    TO WS-KEY
               WHEN 32         MOVE W02-TXTRAN   TO WS-FILE
                               MOVE TAX-KEY      TO WS-KEYX
               WHEN 37         MOVE W02-SHARED   TO WS-FILE
                               MOVE WS-SHARED    TO WS-KEY
               WHEN OTHER      MOVE "ERROR"      TO WS-FILE
                               MOVE "GLPEFT"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
