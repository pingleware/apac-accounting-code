      $set LINKCOUNT"512" GNT"GLPTRFER.GNT"
      ******************************************************************
      *                                                                *
      *  ******  **       *******  ******** *******  **    ** ******** *
      * **    ** **       **    **    **    **    ** ***   ** **       *
      * **       **       **    **    **    **    ** ****  ** **       *
      * **       **       *******     **    *******  ** ** ** ****     *
      * **   *** **       **          **    **   **  **  **** **       *
      * **    ** **       **          **    **    ** **   *** **       *
      *  ******  ******** **          **    **    ** **    ** **       *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     T R A N S F E R   G / L   T R A N S A C T I O N S          *
      *                                                                *
      *       Version 9.04.02 - June 2017                              *
      *                                                                *
      ******************************************************************
      * NB -- To do list: (Audit)                                      *
      *       Include a section to transfer selectively if all the     *
      *       transactions do not need to be transfered.               *
      *ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        GLPTRFER.
       AUTHOR.         James William Lemmon.
       DATE-WRITTEN.        December 2009.

                COPYRIGHT NOTICE: COPYRIGHT (C) 2009 - 2017
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

          SELECT PRNREP  ASSIGN W02-PRINTER
                          ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "PARAM.FDE".

       COPY "GACCNT.FDE".

       COPY "GLTRAN.FDE".

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
       77  WS-CHECK    PIC X(18)  VALUE
      "aeWlimemnomLalismJ".
       77  WS-S1    PIC 9(04)  COMP-5.
       77  WS-S2           PIC  9(04)    COMP-5.
       77  WS-S3           PIC  9(04)    COMP-5.
       77  WS-S4    PIC 9(04)  COMP-5.
       77  WS-S5    PIC 9(04)  COMP-5.
       77  WS-S6           PIC  9(04)    COMP-5.
       77  WS-S7    PIC 9(04)  COMP-5.
       77  WS-S8    PIC 9(04)  COMP-5.
       77  WS-S9    PIC 9(04)   COMP-5.
       77  WS-IXS1    PIC 9(04)   COMP-5.
       77  WS-SUB          PIC  9(04)    COMP-5.
       77  WS-NETKEY       PIC  9(04)    COMP-5 VALUE 1.
       77  WS-PARKEY    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-ISUB         PIC  9(04)    COMP-5.
       77  WS-PAGE         PIC  9(04)    COMP-5.
       77  WS-LINES        PIC  9(04)    COMP-5 VALUE 66.
       77  WS-S            PIC  9(04)    COMP-5.
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
       77  WS-TRAN-SHRT    PIC X(03).
       77  WS-COND    PIC X(06).
       77  WS-NORM    PIC X(06).
       77  WS-EXPP    PIC X(06).
       77  WS-ECAN    PIC X(06).
       77  WS-8LPI    PIC X(06).
       77  WS-6LPI    PIC X(06).
       77  WS-10CPI    PIC X(06).
       77  WS-12CPI    PIC X(06).
       77  WS-17CPI    PIC X(06).
       77  WS-TNEXT    PIC 9(04)  COMP-5.
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
       77  WS-DATE-CHK    PIC 9(08) COMP-5.
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.
       77  WS-PASSWORD    PIC X(10) VALUE SPACES.
      /    *************************************************************
      *    ****     G / L   A C C O U N T   T E M P   S T O R E     ****
      *    *************************************************************
       01  WG-REC.
          03  WG-AC        PIC  9(06).
          03  WG-ACR        REDEFINES WG-AC.
              05  WG-GR       PIC  9(02).
              05  WG-SR       PIC  9(04).
              05  WG-SER      REDEFINES WG-SR.
           07  WG-SGR  PIC  9(02).
           07  WG-ASER PIC  9(02).
          03  WG-ACR2        REDEFINES WG-AC.
              05  WG-GRP      PIC  9(01).
               05  FILLER      PIC  9(05).
          03  WG-AC2        REDEFINES WG-AC.
              05  WG-ACNO     PIC  9(04).
               05  FILLER      PIC  9(02).
          03  WG-NAME.
        05  WG-NKEY     PIC  X(12).
        05  FILLER      PIC  X(18).
      *
      *    ****   This indicator informs the system of the type of
      *           budget on this account.
      *           0 = Manual figures entered.
      *           1 = Percentage of Total income.
      *           2 = Percentage of Group income.
      *           3 = Percentage of Group/Subgroup income.
      *
          03  WG-IND        PIC  9(01).
      *
      *    ****   This is the percentage for calculating the budget
      *    figures based on WG-IND.
      *
          03  WG-BPER        PIC S9(02)V99  COMP-3.
      *
      *    ****   Budget figures for 12 months (Jan to Dec).
      *
          03  WG-BUD.
              05  WG-BVAL     PIC S9(09)    COMP-3 OCCURS 12.
      *
      *    ****   Monthly Balance for each month of the current year.
      *
          03  WG-MOVE.
              05  WG-VAL      PIC S9(09)V99 COMP-3 OCCURS 12.
          03  WG-DT        PIC S9(09)V99 COMP-3.
          03  WG-CR        PIC S9(09)V99 COMP-3.
          03  WG-BAL        PIC S9(09)V99 COMP-3.
    03  WG-OBAL        PIC S9(09)V99 COMP-3.
      *
      *    ****   Monthly Balance for each month of the previous year.
      *
          03  WG-LSTY.
              05  WG-LVAL     PIC S9(09)V99 COMP-3 OCCURS 12.
      *
      *    ****   1 = Income,  2 = Costs,  3 = Expenses.
      *    4 = Capital, 5 = Assets, 6 = Liabilities.
      *
    03  WG-TYPE        PIC  X(01).
    03  WG-SEC        PIC  X(01).
      /    *************************************************************
      *    **** G / L T R A N S A C T I O N T E M P   S T O R E ****
      *    *************************************************************
       01  WT-REC.
    03  WT-KEY.
              05  WT-AC       PIC  9(06).
              05  WT-DATE     PIC  9(08).
              05  WT-DTE REDEFINES WT-DATE.
           07  WT-CC   PIC  9(02).
           07  WT-YY   PIC  9(02).
           07  WT-MM   PIC  9(02).
           07  WT-DD   PIC  9(02).
        05  WT-AKEY.
      *
      *     01 OR 02 - Journal Entries
      *     03 OR 04 - Cheques
      *     06 OR 07 - Deposits
      *     08 OR 09 - Electronic Funds
      *
           07  WT-CODE PIC  9(02).
     07  WT-REFER.
               09  WT-REF
                               PIC  9(07).
     07  WT-SEQ  PIC  9(02).
        05  WT-SUB      PIC  9(01).
          03  WT-NAR        PIC  X(40).
          03  WT-DESC REDEFINES WT-NAR.
              05  WT-NARATIVE PIC  X(36).
               05  FILLER      PIC  9(07)    COMP-3.
          03  WT-VAL        PIC S9(09)V99 COMP-3.
          03  WT-FLAG        PIC  X(01).
    03  WT-BATCH        PIC  9(06)    COMP-3.
    03  FILLER        PIC  X(03).

      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥ Used by calling program (WORKING-STORAGE) and called program ≥
      *    ≥ (LINKAGE SECTION) for Main screen layout and headings.       ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "GLPTRFER".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "GENERAL LEDGER -TRANSFER TRANSACTIONS".
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
    03  WS-PROG        PIC  X(03) VALUE "095".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-FILE-IDS.
           03  W02-PRINTER.
        05  W02-REPORT PIC X(07) VALUE "GLPTPT.".
               05  W02-USER   PIC X(05) VALUE SPACES.

       COPY "W05.GL".

       01  W10-H2.
           03  FILLER      PIC  X(27).
           03  W10-HEAD    PIC  X(13).

       01  W10-EDIT.
          03  W10-DATE.
        05  W10-EDTE  PIC  99/99/9999.

       COPY "W15.GL".

       COPY "W20.WS".

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
    03  W20-AC-NAME PIC X(40).
          03  W22-ACCOUNT PIC 9(06).
    03  W22-AC-NAME PIC X(40).

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

       01  S00.
    03  LINE  6 COLUMN  5 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE
    "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ
      -    "ÕÕÕÕÕÕÕÕÕÕª".
    03  LINE  7 COLUMN  5 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE
    "∫ W A R N I N G  -  W A R N I N G  -  W A R N I N G  -  W A
      -    "R N I N G ∫".
    03  LINE  8 COLUMN  5 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE
    "ÃÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ
      -    "ÕÕÕÕÕÕÕÕÕÕπ".
    03  LINE  9 COLUMN  5 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE
    "∫ This program must be used with caution. The program will a
      -    "llow you  ∫".
    03  LINE 10 COLUMN  5 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE
    "∫ to transfer all or selective General Ledger transactions,
      -    "without   ∫".
    03  LINE 11 COLUMN  5 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE
    "∫ updating the account. The index will be updated, but you m
      -    "ust run   ∫".
    03  LINE 12 COLUMN  5 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE
    "∫ a recalculate balances after you exit this program, if the
      -    "re were   ∫".
    03  LINE 13 COLUMN  5 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE
    "∫ any transfers performed.
      -    "          ∫".
    03  LINE 14 COLUMN  5 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE
    "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº".
      *
      * …ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª
      * ∫ W A R N I N G  -  W A R N I N G  -  W A R N I N G  - W A R N I N G ∫
      * ÃÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕπ
      * ∫ This program must be used with caution. The program will allow you  ∫
      * ∫ to transfer all or selective General Ledger transactions, without   ∫
      * ∫ updating the account. The index will be updated, but you must run   ∫
      * ∫ a recalculate balances after you exit this program, if there were   ∫
      * ∫ any transfers performed.           ∫
      * »ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº
      *
       01  S09.
          03  LINE  2 COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
           VALUE "TRANSFER TRANSACTIONS".
          03  LINE  4 COLUMN  4 VALUE "Account FROM".
    03  LINE  5 COLUMN  4 VALUE "Ac.No.".
          03        COLUMN 20 VALUE "Name:".
    03  LINE  7 COLUMN  4 VALUE "Account TO".
    03  LINE  8 COLUMN  4 VALUE "Ac.No.".
          03        COLUMN 20 VALUE "Name:".

       01  S09A.
    03  S09-AC-FROM.
    03  LINE  6 COLUMN 12 FOREGROUND-COLOR Grey HIGHLIGHT
     PIC  X(06) USING W20-ACCOUNT AUTO.
    03  S09-NM-FROM.
    03  LINE  6 COLUMN 26 FOREGROUND-COLOR Grey HIGHLIGHT
     PIC  X(40) USING W20-AC-NAME AUTO.
    03  S09-AC-TO.
    03  LINE  8 COLUMN 12 FOREGROUND-COLOR Grey HIGHLIGHT
     PIC  X(06) USING W22-ACCOUNT AUTO.
    03  S09-NM-TO.
    03  LINE  8 COLUMN 26 FOREGROUND-COLOR Grey HIGHLIGHT
     PIC  X(40) USING W22-AC-NAME AUTO.

       01  S10.
    03  S10-ACNO.
        05  LINE  8 COLUMN 25 FOREGROUND-COLOR Black
         BACKGROUND-COLOR Grey
     PIC X(06) USING W15-ACCOUNT AUTO.
    03  S10-ACNO-TO.
        05  LINE  8 COLUMN 55 FOREGROUND-COLOR Blue HIGHLIGHT
         BACKGROUND-COLOR Grey
     PIC X(06) USING W15-ENDACC AUTO.
    03  S10-DATE.
        05  LINE  9 COLUMN 25 FOREGROUND-COLOR Black
         BACKGROUND-COLOR Grey
     PIC 99/99/9999 USING W15-DATE AUTO.
    03  S10-TYPE.
        05  LINE 10 COLUMN 25 FOREGROUND-COLOR Black
         BACKGROUND-COLOR Grey
     PIC 99 USING W15-TYPE AUTO.
    03  S10-TYPE-DESC.
        05  LINE 10 COLUMN 29 FOREGROUND-COLOR Black
         BACKGROUND-COLOR Grey
     PIC X(03) USING WS-TRAN-SHRT AUTO.
    03  S10-REF.
        05  LINE 11 COLUMN 25 FOREGROUND-COLOR Black
         BACKGROUND-COLOR Grey
           PIC X(08) USING W15-REF AUTO.
    03  S10-NAR.
        05  LINE 12 COLUMN 25 FOREGROUND-COLOR Black
         BACKGROUND-COLOR Grey
     PIC X(36) USING W15-NAR AUTO.
    03  S10-VALUE.
        05  LINE 13 COLUMN 25 FOREGROUND-COLOR Black
         BACKGROUND-COLOR Grey
     PIC ZZZZZZZ9.99CR USING W15-VAL AUTO.

       01  S11.
          03  LINE  2 COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
           VALUE "AMEND TRANSACTION".
          03  LINE  4 COLUMN 12 VALUE "Press ".
    03        COLUMN 18 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "C".
    03        COLUMN 19 VALUE "ontinue or ".
    03        COLUMN 30 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "Esc".
    03        COLUMN 33 VALUE "ape to Exit".

       01  S17.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC 9(06) USING W15-ACCOUNT AUTO.

       01  S18.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC 99/99/9999 USING W15-DATE AUTO.

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
       PROCEDURE DIVISION
   USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000        SECTION.
       AA00.
          IF LS0-GLLEV < 9
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
        GO TO AA49.
            PERFORM ZA000.
      PERFORM EA000.

      *
      *    ****    P R I N T I N G   T O   B E  I N C L U D E D
      *     L A T E R   -   U S E   P R I N T   U T I L I T Y
      *

      *      CLOSE PRNREP.

       AA49.
      EXIT PROGRAM.

       COPY "AA900.ALM".

      /    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *     S C R E E N ,   K E Y B O A R D   &  M O U S E
      *    *************************************************************
      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥    SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3      ≥
      *    ∆ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕµ
      *    ≥     SCREEN-SHADOW          ≥
      *    √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
      *    ≥ To routine is used to display a shadow down the right and ≥
      *    ≥ along the bottom of a pop-up box. The parameters that are ≥
      *    ≥ required:             ≥
      *    ≥        SHADE-ROW   - Top line of the box + 1.        ≥
      *    ≥        SHADE-COL   - Left line of box + 2.        ≥
      *    ≥        SHADE-WIDTH - Width of the box - 2.        ≥
      *    ≥        SHADE-LINES - Hight of box - 1.         ≥
      *    ∆ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕµ
      *    ≥     CHECK-CORRECT          ≥
      *    √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
      *    ≥ This routine displays a pop-up window with the message    ≥
      *    ≥       "Correct ENTER - N if incorrect"         ≥
      *    ≥              ≥
      *    ≥ The response is returned in WS-OPTION (in upper case).    ≥
      *    ≥              ≥
      *    ≥ To start with the pop-up window higher or lower than row  ≥
      *    ≥ 13 (default); move another value to SLIN and PERFORM      ≥
      *    ≥ CHECK-SETUP THRU CHECK-EXIT.          ≥
      *    ∆ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕµ
      *    ≥     ERROR-MESSAGE          ≥
      *    √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
      *    ≥ To display an error message with " - Press ANY key" at    ≥
      *    ≥ the end; use PERFORM ERROR-MESSAGE.         ≥
      *    ≥              ≥
      *    ≥ To display just the error message; use PERFORM        ≥
      *    ≥ ERROR-LENGTH THRU ERROR-EXIT.          ≥
      *    ≥              ≥
      *    ≥ To display the error message higher or lower (default is  ≥
      *    ≥ line 13) firstly use PERFORM ERROR-SETUP THRU ERROR-POS   ≥
      *    ≥ or PERFORM ERROR-LENGTH THRU ERROR-POS. Move the line     ≥
      *    ≥ number to be used to SLIN and then PERFORM ERROR-DISPLAY  ≥
      *    ≥ THRU ERROR-EXIT.            ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ

       COPY "FUNCTION.CRT".

      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥                        OPT-MESSAGE                        ≥
      *    √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
      *    ≥ This routine is used to allow the OPERATOR to respond to  ≥
      *    ≥ a request for an option, so that the correct action can   ≥
      *    ≥ be performed by the program. The routine will display the ≥
      *    ≥ message in a pop-up window and allow the OPERATOR to      ≥
      *    ≥ respond to the 'question'.                                ≥
      *    ≥                                                           ≥
      *    ≥ The option request must be placed in WS-OPT-MES and may   ≥
      *    ≥ not exceed 48 characters. The message will be centred in  ≥
      *    ≥ the window. An example of a message request follows:      ≥
      *    ≥                                                           ≥
      *    ≥   MOVE "Print transactions (Y/N) [ ]" TO WS-OPT-MES.      ≥
      *    ≥   MOVE Instruction    TO WS-INSTR.                        ≥
      *    ≥       [see Accptopt for instruction values]               ≥
      *    ≥   PERFORM OPT-MESSAGE.                                    ≥
      *    ≥                                                           ≥
      *    ≥ This would be displayed as:                               ≥
      *    ≥      ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø   ≥
      *    ≥      ≥          Print transactions (Y/N) [ ]          ≥∞∞ ≥
      *    ≥      ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ∞∞ ≥
      *    ≥        ∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞ ≥
      *    ≥                                                           ≥
      *    ≥ The response is returned in WS-OPTION (in upper case).    ≥
      *    ≥                                                           ≥
      *    ≥ The system will display the message box with the top line ≥
      *    ≥ as the value of WS-MES-LINE. If WS-MES-LINE is zero the   ≥
      *    ≥ default will be 20.                                       ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ

       COPY "OPTION.CRT".

       COPY "LOCKED.REC".

      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥           ERASE SCREEN FROM SPECIFIED LOCATION            ≥
      *    √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
      *    ≥ Uses CRT-START as starting line and increases and CRT-END ≥
      *    ≥ as the ending line. The screen is cleared with Column 1   ≥
      *    ≥ and 80 containing "≥" and columns 2 to 79 containing      ≥
      *    ≥ spaces.                                                   ≥
      *    ≥                                                           ≥
      *    ≥  eg.                                                      ≥
      *    ≥   MOVE 8         TO CRT-START.                            ≥
      *    ≥   MOVE 28        TO CRT-END.                              ≥
      *    ≥   PERFORM ERASE-SCREEN.                                   ≥
      *    ≥                                                           ≥
      *    ≥ To clear the entire screen and Display a new screen with  ≥
      *    ≥ headings (program/date/time/company):                     ≥
      *    ≥                                                           ≥
      *    ≥    CRT-PROGRAM (calling program)                          ≥
      *    ≥    CRT-HEADING (Screen heading)                           ≥
      *    ≥    CRT-COMPANY (Company name)                             ≥
      *    ≥    CRT-TERMINAL (consists of two fields:                  ≥
      *    ≥                  CRT-WRKHD and CRT-WRKST. See CRTHEAD)    ≥
      *    ≥    These fields should be populated at the start of the   ≥
      *    ≥    program.                                               ≥
      *    ≥    PERFORM HEADING-AND-CLEAR-SCREEN.                      ≥
      *    ≥                                                           ≥
      *    ≥ To change the screen heading:                             ≥
      *    ≥    MOVE "The new heading" TO CRT-HEADING.                 ≥
      *    ≥    PERFORM CHANGE-HEADING.                                ≥
      *    ≥                                                           ≥
      *    ≥ To change the time on the screen:                         ≥
      *    ≥    PERFORM CHANGE-TIME                                    ≥
      *    ≥                                                           ≥
      *    ≥ To clear line 50:                                         ≥
      *    ≥    PERFORM CLEAR-L50                                      ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ

       COPY "CLEAR.CRT".

      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥ This routine is used to call  the General Ledger Accounts ≥
      *    ≥ lookup program and cancel it when control is returned.    ≥
      *    ≥ The  relevant  error  message  will be  displayed  if  an ≥
      *    ≥ exception occurs. The usual  parameters are passed to the ≥
      *    ≥ lookup program as well W15-GLEDGER which will contain the ≥
      *    ≥ details of the account that has been selected when control≥
      *    ≥ is returned to the calling program.                       ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
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

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AB000        SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "GACCNT.RD".

       COPY "GLTRAN.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AC000        SECTION.

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

       COPY "GACCNT.WR".

       COPY "GLTRAN.WR".

       COPY "GACCNT.LUP".

       COPY "APAC.HLP".

      *
      *   D A T E   C H E C K   R O U T I N E
      *
      *   PERFORM THIS ROUTINE WITH THE DATE IN W10-DATE (PIC 99/99/9999)
      *   WITH THE FORMAT AS DD/MM/CCYY
      *
      *   ON EXITING - WS-ERROR WILL BE ZERO (0) IF THE DATE IS CORRECT
      *                OR ONE (1) IF THE DATE IS INCORRECT.
      *

       COPY "DATE.CHK".

      /
       EA000      SECTION 5.
       EA00.
             PERFORM ERASE-SCREEN.
      MOVE 7   TO SHADE-ROW.
      MOVE 7   TO SHADE-COL.
      MOVE 69   TO SHADE-WIDTH.
      MOVE 8   TO SHADE-LINES.
      DISPLAY S00.
      PERFORM SCREEN-SHADOW.
      DISPLAY "Press any key to continue" AT 1727
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       EA05.
            PERFORM ERASE-SCREEN.
            DISPLAY S09.
            MOVE ZERO   TO W15-ACCOUNT
        W15-ENDACC.

       EA10.
      DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "=Help,"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT
       "F2" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       " to do Account Lookup - "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT
       "Esc" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "ape to exit"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT.
            ACCEPT S17 AT 0513.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE ZERO  TO W15-ACCOUNT
       GO TO EA999
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA350
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO EA05
        END-EVALUATE
        DISPLAY S17 AT 0513
               IF W15-ACCOUNT = ZERO
           GO TO EA05.
      PERFORM CLEAR-L50.
           IF W15-ACCOUNT = ZERO
               GO TO EA999.
          IF W15-ASER = ZERO
              MOVE WS-ER7  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO EA10.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
              MOVE WS-ER6  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO EA05.
            DISPLAY G-NAME AT 0526
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      PERFORM SAVE-SCREEN.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO EA10.
      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥ Move G/L account details to temporary store as the G-REC  ≥
      *    ≥ details may be changed while patching transaction details ≥
      *    ≥ if the account number is amended.          ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
      MOVE G-REC   TO WG-REC.
      MOVE W15-ACCOUNT  TO W15-ACNT W15-BEGACC.

       EA11.
      DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "=Help,"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT
       "F2" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       " to do Account Lookup - "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT
       "Esc" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "ape to exit"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT.
            ACCEPT S17 AT 0713.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE ZERO  TO W15-ACCOUNT
       GO TO EA10
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA350
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO EA11
        END-EVALUATE
        DISPLAY S17 AT 0813
               IF W15-ACCOUNT = ZERO
           GO TO EA10.
      PERFORM CLEAR-L50.
           IF W15-ACCOUNT = ZERO
              GO TO EA05.
          IF W15-ASER = ZERO
              MOVE WS-ER7  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO EA11.
            MOVE W15-ACCOUNT  TO G-AC W15-ENDACC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
              MOVE WS-ER6  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO EA11.
            DISPLAY G-NAME AT 0826
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      PERFORM SAVE-SCREEN.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO EA11.
      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥ Read the account from which transactions are to be trans- ≥
      *    ≥ ferred from. As the account is used to access the trans-  ≥
      *    ≥ actions, it is necessary to read the account.        ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
      MOVE W15-BEGACC  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.

       EA15.
      DISPLAY "Start Date" AT 1202.

       EA15A.
      ACCEPT S18 AT 1302.
      MOVE W15-DATE  TO W10-DATE.
      PERFORM CHECK-DATE.
          IF WS-ERROR = 1
              MOVE "Invalid date" TO WS-ERR-MES
        PERFORM ERROR-MESSAGE
        GO TO EA15A.
      INITIALIZE T-REC.
      MOVE WG-AC   TO T-AC.
      MOVE W15-CC  TO W15-CN.
      MOVE W15-YY  TO W15-YR.
      MOVE W15-MM  TO W15-MT.
      MOVE W15-DD  TO W15-DY.
            MOVE W15-YMD  TO T-DATE.
             PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
           IF WS-F-ERROR = 11
        GO TO EA190.

       EA20.
             PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
      COMMIT.
           IF (WS-F-ERROR = 11) OR
       (T-AC NOT = WG-AC)
        GO TO EA05.

       EA20A.
            MOVE T-DATE  TO W15-YMD W15-SYMD.
      MOVE W15-CN  TO W15-CC.
      MOVE W15-YR  TO W15-YY.
      MOVE W15-MT  TO W15-MM.
      MOVE W15-DY  TO W15-DD.
      MOVE W15-DATE  TO W25-DTE WS-DATE-CHK.
          IF T-CODE = 01 OR 02
        MOVE "Jnl"  TO WS-TRAN-SHRT
          ELSE
          IF T-CODE = 03 OR 04
        MOVE "Chq"  TO WS-TRAN-SHRT
          ELSE
          IF T-CODE = 05 OR 10
        MOVE "Bnk"  TO WS-TRAN-SHRT
          ELSE
           IF T-CODE = 06 OR 07
        MOVE "Dep"  TO WS-TRAN-SHRT
          ELSE
          IF T-CODE = 08 OR 09
        MOVE "Eft"  TO WS-TRAN-SHRT
          ELSE
          IF T-CODE = 11 OR 12
        MOVE "Pmt"  TO WS-TRAN-SHRT
          ELSE
          IF T-CODE = 13 OR 14
        MOVE "Rec"  TO WS-TRAN-SHRT
          ELSE
        MOVE "   "  TO WS-TRAN-SHRT.

       EA25.
      MOVE T-AC   TO W15-ACCOUNT.
      MOVE T-CODE  TO W15-TYPE.
      MOVE T-REFER  TO W15-REF.
      MOVE T-NARATIVE  TO W15-NAR.
      MOVE T-VAL   TO W15-VAL.
      PERFORM SAVE-SCREEN-3.
      *    *************************************************************
      *    ****  DISPLAY THE TRANSACTION PATCH SCREEN     ****
      *    *************************************************************
      *  ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *  ≥ Ac No 999999     Transfer to Ac No 999999 ≥
      *  ≥ Date 99/99/9999        ≥
      *  ≥ Code 99  XXX         ≥
      *  ≥ Ref XXXXXXXX        ≥
      *  ≥ Nar XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ≥
      *  ≥ Value ZZZZZZZ9.99CR        ≥
      *  ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
      *
      DISPLAY "⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
        AT 0717 WITH FOREGROUND-COLOR Grey HIGHLIGHT
       BACKGROUND-COLOR Grey
       "ø" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "≥ Ac No             " AT 0817
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Grey
       "Transfer to Ac No        "
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Grey
       "≥" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "≥ Date             " AT 0917
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Grey
       "                          "
        WITH FOREGROUND-COLOR Blue HIGHLIGHT BACKGROUND-COLOR Grey
       "≥" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "≥ Code             " AT 1017
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Grey
       "                          "
        WITH FOREGROUND-COLOR Blue HIGHLIGHT BACKGROUND-COLOR Grey
       "≥" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "≥ Ref              " AT 1117
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Grey
       "                          "
        WITH FOREGROUND-COLOR Blue HIGHLIGHT BACKGROUND-COLOR Grey
       "≥" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "≥ Nar                                        "
        AT 1217 WITH FOREGROUND-COLOR Grey HIGHLIGHT
       BACKGROUND-COLOR Grey
       "≥" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "≥ Value                                      "
        AT 1317 WITH FOREGROUND-COLOR Grey HIGHLIGHT
       BACKGROUND-COLOR Grey
       "≥" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "¿" AT 1417 WITH FOREGROUND-COLOR Grey HIGHLIGHT
          BACKGROUND-COLOR Grey
       "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ"
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      MOVE 8   TO SHADE-ROW.
      MOVE 19   TO SHADE-COL.
      MOVE 44   TO SHADE-WIDTH.
      MOVE 7   TO SHADE-LINES.
      PERFORM SCREEN-SHADOW.

       EA30.
      DISPLAY S10.
            MOVE "Transfer this transaction (Y/N)  [ ]" TO WS-OPT-MES.
      MOVE SPACE   TO WS-OPTION.
      MOVE 16   TO SLIN.
      PERFORM OPT-SETUP THRU OPT-EXIT.

       EA35.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO EA05
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO EA35
        END-EVALUATE.
    IF NOT(WS-OPTION = "Y" OR "N")
        PERFORM AA900-ALARM
        GO TO EA30.
    IF WS-OPTION = "N"
        GO TO EA20.
      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥ As the account number is being  changed, this transaction ≥
      *    ≥ must be deleted and the amended transaction  must then be ≥
      *    ≥ added. Store the transaction record before deleting, as a ≥
      *    ≥ restart pointer after adding the new record.        ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
      MOVE T-REC   TO WT-REC.
      MOVE 1   TO WS-INDG.
      PERFORM DELETE-GLTRAN-REC THRU WRITE-GLTRAN-EXIT.
      MOVE WT-REC  TO T-REC.
      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥      A C C O U N T   N U M B E R         ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
            MOVE W15-ENDACC  TO T-AC.
      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥   Add transaction with the changed account number.     ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
      PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥ Restart the transaction file from the last point that it  ≥
      *    ≥ read for the transferring from account.         ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
      MOVE WT-REC  TO T-REC.
      PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
      PERFORM RESTORE-SCREEN-3.
      GO TO EA20.

       EA190.
      MOVE "No transactions on file" TO WS-ERR-MES.
            PERFORM ERROR-MESSAGE.
      GO TO EA05.

       EA999.
             EXIT.

      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000      SECTION 9.
       ZA00.
      MOVE LS-PARID  TO WS-PARID.
      MOVE LS-L-OR-N  TO W02-L-OR-N.
      MOVE WS-SYS-ID  TO W02-SYSID.
      MOVE LS-TODAY-DDMMYY
     TO TODAY-DDMMYY.
      MOVE LS-USUB  TO WS-USUB.
            PERFORM ERASE-SCREEN.
      *    ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
      *    ≥  Setup various keys and the mouse for use with menus and  ≥
      *    ≥  other required user interface functions and procedures.  ≥
      *    ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ

      COPY "FUNCTION.PRO".

    IF LS-USER NUMERIC
        IF LS-USE > ZERO AND < 110
     MOVE LS-USE  TO WS-USUB
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
        MOVE LS-USER  TO W02-USER.
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
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-COMPANY  TO W90-CNAME.
            MOVE PAR-DMY  TO W05-TODAY.
            MOVE PAR-YMD  TO W05-TYMD.
      *
      *    ****    Printing of an audit trail will be included later.
      *     U S E   P R I N T   U T I L I T Y
      *

    GO TO ZA999.

    IF WS-PRN-NO < 3
        PERFORM ZA210 THRU ZA215
        IF WS-ERROR = 1
     GO TO ZA205
    ELSE
        DISPLAY "Waiting for printer response" AT 1412
   WITH FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
      *      OPEN OUTPUT PRNREP.
      DISPLAY SPACES AT 5025
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT.
            GO TO ZA999.

       COPY "ZA49.PRO".

       ZA200.

       COPY "LOCKED.PRO".

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
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "R" WITH FOREGROUND-COLOR Grey HIGHLIGHT
       "etry - "
       "A" WITH FOREGROUND-COLOR Grey HIGHLIGHT
       "bort".
       ZA212.
      ACCEPT WS-OPTION AT 2334
      WITH FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      IF NOT(WS-OPTION = "A" OR "R")
   GO TO ZA212.
      IF WS-OPTION = "A"
   MOVE 1   TO WS-ERROR
   GO TO ZA215.
        GO TO ZA210.
       ZA215.
      EXIT.
       ZA999.
             EXIT.
      *
      *    ****    I / O   E R R O R   M E S S A G E S
      *
       ZB000      SECTION 9.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
           IF WS-F-ERROR = 2
              MOVE W02-NETWORK  TO WS-FILE
              MOVE WS-NETKEY  TO WS-KEY
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
              MOVE WS-PARKEY  TO WS-KEY.
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
             DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY "Status " AT 1212
                      WS-STATUS WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                     ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
           IF WS-STATUS NOT = "94"
               DISPLAY "Key    " AT 1412
                        WS-KEY WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                        " " WS-KEYX WITH FOREGROUND-COLOR Cyan HIGHLIGHT
               DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
               DISPLAY "Please make a note of these details" AT 1812.
             STOP RUN.
