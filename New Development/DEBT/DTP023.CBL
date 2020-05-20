      $SET LINKCOUNT"512" GNT"DTP023.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****     ******    ******    *
      *   **    **     **     **    **   **  **   **    **  **    **   *
      *   **    **     **     **    **  **    **       **         **   *
      *   **    **     **     *******   **    **     **         ***    *
      *   **    **     **     **        **    **   **             **   *
      *   **    **     **     **         **  **   **        **    **   *
      *   *******      **     **          ****   ********    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R   A D D R E S S   L A B E L S   P R O G R A M  *
      *                                                                *
      *       Version 9.00.82 - April 2016                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  May 2006 - New field included in Accounts Receivable          *
      *             for recording number of outstanding Jobs           *
      *             linked to the account. Additional filler           *
      *             added for future expansion.                        *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     DTP023.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   November 1983.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2016
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

       COPY "DEBTOR.SL".

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

       COPY "DEBTOR.FDE".

      *
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   **
      *         **    *    **   **    **   **   **    **  **
      *         **   ***   **   **    **   ******     *****
      *          ** ** ** **    **    **   **   **    **  **
      *           ***   ***     **    **   **    **   **   **
      *            *     *       ******    **    **   **    **
      *
       WORKING-STORAGE SECTION.
       77  WS-CHECK                PIC  X(18)     VALUE "aeWlimemnomLalismJ".
       77  WS-SUB-1                PIC  9(04)     COMP-5.
       77  WS-SUB-2                PIC  9(04)     COMP-5.
       77  WS-IXS1                 PIC  9(04)     COMP-5.
       77  WS-IXS2                 PIC  9(04)     COMP-5.
       77  WS-IXS3                 PIC  9(04)     COMP-5.
       77  WS-IXS4                 PIC  9(04)     COMP-5.
       77  WS-MSUB                 PIC  9(04)     COMP-5.
       77  WS-PARKEY               PIC  9(04)     COMP-5.
       77  WS-NETKEY               PIC  9(06)     COMP-5.
       77  WS-WRKEY                PIC  9(04)     COMP-5.
       77  WS-NTP                  PIC  9(01).
       77  WS-LINES                PIC  9(02).
       77  WS-PAGE                 PIC  9(04)     COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-SEQ                  PIC  X(01).
       77  WS-ERROR                PIC  9(01).
       77  WS-ERROR1               PIC  9(01).
       77  WS-ACNO                 PIC  X(06).
       77  WS-S1                   PIC  9(04)     COMP-5.
       77  WS-S2                   PIC  9(04)     COMP-5.
       77  WS-S3                   PIC  9(04)     COMP-5.
       77  WS-S4                   PIC  9(04)     COMP-5.
       77  WS-S5                   PIC  9(04)     COMP-5.
       77  WS-S6                   PIC  9(04)     COMP-5.
       77  WS-S7                   PIC  9(04)     COMP-5.
       77  WS-S8                   PIC  9(04)     COMP-5.
       77  WS-ERR1                 PIC  X(22)     VALUE "Invalid account number".
       77  WS-ERR2                 PIC  X(09)     VALUE "No record".
       77  WS-AC                   PIC  X(07)     VALUE "Account".
       77  TODAY-DDMMYY            PIC  9(08)     COMP-5.
       77  WS-USUB                 PIC  9(04)     COMP-5.
      *
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *         C = Clear comment line (50)
      *         E = Clear lines (any line or lines between 5 and 46)
      *         H = Change heading
      *         S = Clear the screen and display headings
      *
           03  CRT-TYPE            PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START           PIC  9(02) VALUE 5.
           03  CRT-END             PIC  9(02) VALUE 46.
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15) VALUE "DTP023".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "ACCOUNTS RECEIVABLE: ADDRESS LABELS".
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

       01  W00-TRAN-CODES.
           03  W00-TRAN-TYPES  OCCURS 100.
               05  W00-TRAN-CODE PIC 9(02)   COMP-5.
               05  W00-ACTION  PIC 9(02)     COMP-5.
               05  W00-E-DESC  PIC X(12).
               05  W00-A-DESC  PIC X(12).

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-PRINTER-DETAILS.
           03  W02-PRINTER           PIC  X(12).
           03  W02-PGE-LENGTH        PIC  9(03).
           03  W02-PRN-LENGTH        PIC  9(03).
           03  W02-LINAGE            PIC  9(03).
           03  W02-PRN-STATUS        PIC  X(01)    VALUE "C".
      *
      *    ****    D  =  Detail line
      *            C  =  Condensed print
      *            E  =  Expanded print
      *            H  =  Header line
      *            X  =  Cancel expanded print
      *            1  =  10 Characters per inch
      *            2  =  12 Characters per inch
      *            3  =  17 Characters per inch
      *            6  =  6 Lines per inch
      *            8  =  8 Lines per inch
      *
           03  W02-PRN-TYPE        PIC  X(01).
           03  W02-PRN-LINE        PIC  X(136).

           03  REP-LINE1                          REDEFINES W02-PRN-LINE.
               05  REP-DETAIL1     PIC  X(136).
           
           03  LAB-L1                             REDEFINES W02-PRN-LINE.
               05  LAB-DETAIL                     OCCURS 2.
                   07  FILLER      PIC  X(02).
                   07  LAB-DET     PIC  X(36).
                   07  FILLER      PIC  X(02).

       01  W05-LIST.
           03  W05-OPT         PIC 9(03).
           03  W05-TYPES REDEFINES W05-OPT.
               05  W05-COMP    PIC 9(01).
               05  W05-INDV    PIC 9(01).
               05  W05-INT     PIC 9(01).

      * 01  W10-SPARES.
      *    03  W10-DATE        PIC X(10).
      *     03 W10-RDATE REDEFINES W10-DATE.
      *        05  W10-DTE     PIC 99/99/9999.

      *
      *    Date routines for accepting checking and formatting have
      *    been removed from each program and the DateCheck program
      *    will be called to handle these routines.
      *
      *    The following copy 'DateVariables' now includes the 'W12.WS'
      *    and 'W20.WS' variables and is passed to DateCheck in the CALL
      *    statement.
      *
       COPY "DateVariables.cpy".

       01  W15-OPTIONS.
           03  W15-OPTS.
               05  W15-CAT         PIC  X(01).
               05  W15-ALL         PIC  X(01).
               05  W15-ZERO        PIC  X(01).
               05  W15-L30         PIC  X(01).
               05  W15-L60         PIC  X(01).
               05  W15-L90         PIC  X(01).
               05  W15-30          PIC  X(01).
               05  W15-60          PIC  X(01).
               05  W15-90          PIC  X(01).
               05  W15-90P         PIC  X(01).
           03  W15-REPEAT                         REDEFINES W15-OPTS.
               05  W15-OPT         PIC  X(01)     OCCURS 10.

       01  W24-MONTHS.
           03  W24-MONTH-NAMES.
               05  FILLER          PIC  X(09)     VALUE " January ".
               05  FILLER          PIC  X(09)     VALUE " February".
               05  FILLER          PIC  X(09)     VALUE "  March  ".
               05  FILLER          PIC  X(09)     VALUE "  April  ".
               05  FILLER          PIC  X(09)     VALUE "   May   ".
               05  FILLER          PIC  X(09)     VALUE "   June  ".
               05  FILLER          PIC  X(09)     VALUE "   July  ".
               05  FILLER          PIC  X(09)     VALUE "  August ".
               05  FILLER          PIC  X(09)     VALUE "September".
               05  FILLER          PIC  X(09)     VALUE " October ".
               05  FILLER          PIC  X(09)     VALUE " November".
               05  FILLER          PIC  X(09)     VALUE " December".
           03  W24-EACH-MONTH                     REDEFINES W24-MONTH-NAMES.
               05  W24-MONTH       PIC  X(09)     OCCURS 12.

       01  W25-CALCS.
           03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
               05  W25-WHOLE   PIC 9(05).
               05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(04).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).
           03  W25-TOTAL       PIC S9(07)V99 COMP-3.
           03  W25-VALUE       PIC S9(07)V99 COMP-3.

       01  W40-COMPANY.
           02  W40-NAME1.
               03  W40-NAME.
                   05  W40-ENTRY PIC X(07).
                   05  FILLER  PIC X(33).
               03  FILLER      PIC X(32).
           02  W40-NAME2 REDEFINES W40-NAME1.
               03  W40-SURNAME PIC X(24).
               03  W40-FNAME   PIC X(24).
               03  W40-SNAME   PIC X(24).
           02  W40-NAME3 REDEFINES W40-NAME1.
               03  W40-NCHAR OCCURS 72 PIC X(01).
           02  W40-ADDRESS1.
               03  W40-ADD1    PIC X(30).
               03  W40-ADD2    PIC X(30).
               03  W40-ADD3    PIC X(30).
               03  W40-ADD4    PIC X(30).
           02  W40-ADDRESS2 REDEFINES W40-ADDRESS1.
               03  W40-ACHAR OCCURS 120 PIC X(01).
           02  W40-POST        PIC 9(04).
           02  W40-TEL         PIC X(14).
           02  W40-HTEL        PIC X(14).
           02  W40-TAX REDEFINES W40-HTEL.
               03  W40-TAXNO   PIC 9(14)     COMP-3.
           02  W40-TITLE       PIC 9(04)     COMP-5.
           02  W40-LANG        PIC X(01).

       COPY "W40.WS".

       01  W45-TRAN.
           03  W45-CODE        PIC 9(02).
           03  W45-ENG         PIC X(12).
           03  W45-AFR         PIC X(12).
           03  W45-ACT         PIC X(01).
      *
      *    STORAGE FOR PRINTING OF LABELS
      *
       01  W46-LABELS.
           03  W46-LABEL OCCURS 2.
               05  W46-NAME    PIC X(40).
               05  W46-ADD1    PIC X(30).
               05  W46-ADD2    PIC X(30).
               05  W46-ADD3    PIC X(30).
               05  W46-ADD4    PIC X(30).
               05  W46-POST.
                   07  W46-PC  PIC 9(04).

       COPY "FUNCTION.WS".

      *COPY "W50.WS".

       01  W55-STATS.
           03  W55-H1         PIC  X(09).
           03  W55-H2         PIC  X(09).
           03  W55-H3         PIC  X(09).
           03  W55-H4         PIC  X(09).
           03  W55-H5         PIC  X(09).
           03  W55-H6         PIC  X(09).
           03  W55-MONTHS.
               05  W55-MTH1   PIC  9(02) COMP-5.
               05  W55-MTH2   PIC  9(02) COMP-5.
               05  W55-MTH3   PIC  9(02) COMP-5.
               05  W55-MTH4   PIC  9(02) COMP-5.
               05  W55-MTH5   PIC  9(02) COMP-5.
               05  W55-MTH6   PIC  9(02) COMP-5.
           03  W55-VALUES.
               05  W55-VAL1   PIC S9(09)V99 COMP-3.
               05  W55-VAL2   PIC S9(09)V99 COMP-3.
               05  W55-VAL3   PIC S9(09)V99 COMP-3.
               05  W55-VAL4   PIC S9(09)V99 COMP-3.
               05  W55-VAL5   PIC S9(09)V99 COMP-3.
               05  W55-VAL6   PIC S9(09)V99 COMP-3.

       01  W60-NME-ADD.
           03  W60-NAME.
               05  W60-NCHAR OCCURS 40 PIC X(01).
           03  W60-ADDRESS.
               05  W60-ACHAR OCCURS 60 PIC X(01).
       01  W65-ENTRY.
           03  W65-FOUR.
               05  W65-CHAR OCCURS 8 PIC X(01).
           03  W65-PREFIX REDEFINES W65-FOUR.
               05  W65-PREF    PIC X(02).
               05  FILLER      PIC X(06).
       01  W65-INDEX.
           03  W65-INPUT.
               05  W65-ICHAR OCCURS 8 PIC X(01).

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-DNO        PIC X(06) OCCURS 18.

       COPY "W75.WS".

       01  W80-ACCOUNT.
           03  W80-ACNO        PIC X(06).
           03  W80-NUMD REDEFINES W80-ACNO.
               05  W80-SERIAL  PIC 9(06).
           03  W80-TYPE        PIC 9(01).
       01  W85-BATCHES.
               05  W85-BATCH   PIC 9(05).
       01  W90-TRAN.
          03  W90-DATE        PIC X(10).
           03  W90-DTE REDEFINES W90-DATE
                PIC 99/99/9999.
          03  W90-CODE        PIC 9(02).
           03  W90-REF         PIC X(08).
           03  W90-DB          PIC S9(07)V99 COMP-3.
           03  W90-CR          PIC S9(07)V99 COMP-3.
           03  W90-BDT         PIC S9(07)V99 COMP-3.
           03  W90-BCR         PIC S9(07)V99 COMP-3.
           03  W90-DESC        PIC X(12).
           03  W90-120         PIC S9(07)V99 COMP-3.
           03  W90-90          PIC S9(07)V99 COMP-3.
           03  W90-60          PIC S9(07)V99 COMP-3.
           03  W90-30          PIC S9(07)V99 COMP-3.
           03  W90-CUR         PIC S9(07)V99 COMP-3.
           03  W90-MTD         PIC S9(07)V99 COMP-3.
           03  W90-BAL         PIC S9(07)V99 COMP-3.
           03  W90-IN          PIC 9(04)     COMP-5.
           03  W90-IV          PIC S9(07)V99 COMP-3.
           03  W90-RN          PIC 9(04)     COMP-5.
           03  W90-RV          PIC S9(07)V99 COMP-3.
           03  W90-DN          PIC 9(04)     COMP-5.
           03  W90-DV          PIC S9(07)V99 COMP-3.
           03  W90-CN          PIC 9(04)     COMP-5.
           03  W90-CV          PIC S9(07)V99 COMP-3.
           03  W90-INT         PIC S9(07)V99 COMP-3.
           03  W90-RESULT      PIC  9(04)    COMP-5.
           03  W90-CATKEY      PIC  9(04)    COMP-5.
           03  W90-CS          PIC  9(02)    COMP-5.

       01  W95-STM.
           03  W95-COMP        PIC X(40).

       01  W100-HEAD.
           03  FILLER          PIC X(26)    VALUE
                             "DEBTOR ACCOUNTS LISTING -".
           03  W100-DESC       PIC X(11).
       01  W100-EDIT.
           03  W100-LBAL       PIC Z(06)9.99-.
           03  W100-LARR       PIC Z(06)9.99-.
           03  W100-BAL        PIC Z(08)9.99-.
           03  W100-CUR        PIC Z(08)9.99-.
           03  W100-30         PIC Z(08)9.99-.
           03  W100-60         PIC Z(08)9.99-.
           03  W100-90         PIC Z(08)9.99-.
           03  W100-120        PIC Z(08)9.99-.
           03  W100-INT        PIC Z(08)9.99.
           03  W100-CRL        PIC Z(06)9.
           03  W100-KEY        PIC Z(05)9.

      *
      *    **        ********  **    **  **    **    ****     ******   ********
      *    **           **     ***   **  **   **    **  **   **    **  **
      *    **           **     ****  **  **  **    **    **  **        ** 
      *    **           **     ** ** **  *****     ********  **        *****
      *    **           **     **  ****  **  **    **    **  **   ***  **  
      *    **           **     **   ***  **   **   **    **  **    **  **
      *    ********  ********  **    **  **    **  **    **   ******   ********
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

       01  S03.
    02  S03-1.
    03  LINE  2 COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     VALUE "DEBTORS - ADDRESS LABELS".
    02  S03-2.
          03  LINE  4 COLUMN 12 FOREGROUND-COLOR Grey HIGHLIGHT
                                 VALUE "Printing Client labels".
      *
      *     ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *     ³      F I L T E R   O P T I O N S     ³°°
      *     ³                                  Y/N ³°°
      *     ³ Selective per Category/Area      [ ] ³°°
      *     ³                                      ³°°
      *     ³ Include: 1 All accounts          [ ] ³°°
      *     ³          2 Zero balance accounts [ ] ³°°
      *     ³          3 Accounts < 30 days    [ ] ³°°
      *     ³          4 Accounts < 60 days    [ ] ³°°
      *     ³          5 Accounts < 90 days    [ ] ³°°
      *     ³          6 Only 30 day accounts  [ ] ³°°
      *     ³          7 Only 60 day accounts  [ ] ³°°
      *     ³          8 Only 90 day accounts  [ ] ³°°
      *     ³          9 Only 90+ day ccounts  [ ] ³°°
      *     ³ 1 & 2 = Y, include all accounts, 2 = ³°°
      *     ³ Y, only accounts with zero balances. ³°°
      *     ³ If any of 3 - 9 = Y, then all others ³°°
      *     ³ from 1 - 9 must contain an N.        ³°°
      *     ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°
      *       °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
      *
       01  S04.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE  6 COLUMN 21 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"  FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 60 VALUE                                        "¿".
               05  LINE  7 COLUMN 21 VALUE "³     F I L T E R   O P T I O N S      "  FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 60 VALUE                                        "³".
               05  LINE  8 COLUMN 21 VALUE "³                                  Y/N "  FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 60 VALUE                                        "³".
               05  LINE  9 COLUMN 21 VALUE "³ "                                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 23 VALUE   "Selective per Category/Area      [ ] " .
               05          COLUMN 60 VALUE                                        "³".
               05  LINE 10 COLUMN 21 VALUE "³                                      "  FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 60 VALUE                                        "³".
               05  LINE 11 COLUMN 21 VALUE "³ "                                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 23 VALUE   "Include: 1 All accounts          [ ] " .
               05          COLUMN 60 VALUE                                        "³".
               05  LINE 12 COLUMN 21 VALUE "³          "                              FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 32 VALUE            "2 Zero balance accounts [ ] " .
               05          COLUMN 60 VALUE                                        "³".         
               05  LINE 13 COLUMN 21 VALUE "³          "                              FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 32 VALUE            "3 Accounts < 30 days    [ ] " .
               05          COLUMN 60 VALUE "³"                                       .        
               05  LINE 14 COLUMN 21 VALUE "³          "                              FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 32 VALUE            "4 Accounts < 60 days    [ ] " .
               05          COLUMN 60 VALUE "³"                                       .         
               05  LINE 15 COLUMN 21 VALUE "³          "                              FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 32 VALUE            "5 Accounts < 90 days    [ ] " .
               05          COLUMN 60 VALUE "³"                                       .         
               05  LINE 16 COLUMN 21 VALUE "³          "                              FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 32 VALUE            "6 Only 30 day accounts  [ ] " .
               05          COLUMN 60 VALUE "³"                                       .         
               05  LINE 17 COLUMN 21 VALUE "³          "                              FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 32 VALUE            "7 Only 60 day accounts  [ ] " .
               05          COLUMN 60 VALUE "³"                                       .         
               05  LINE 18 COLUMN 21 VALUE "³          "                              FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 32 VALUE            "8 Only 90 day accounts  [ ] " .
               05          COLUMN 60 VALUE                                        "³".
               05  LINE 19 COLUMN 21 VALUE "³          "                              FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 32 VALUE            "9 Only 90+ day accounts [ ] " .
               05          COLUMN 60 VALUE                                        "³".
               05  LINE 20 COLUMN 21 VALUE "³ "                                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 23 VALUE   "1 & 2 = Y, include all accounts, 2 = "  FOREGROUND-COLOR Magenta.
               05          COLUMN 60 VALUE                                        "³".
               05  LINE 21 COLUMN 21 VALUE "³ "                                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 23 VALUE   "Y, only accounts with zero balances. "  FOREGROUND-COLOR Magenta.
               05          COLUMN 60 VALUE                                        "³".
               05  LINE 22 COLUMN 21 VALUE "³ "                                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 23 VALUE   "If any of 3 - 9 = Y, then all others "  FOREGROUND-COLOR Magenta.
               05          COLUMN 60 VALUE                                        "³".
               05  LINE 23 COLUMN 21 VALUE "³ "                                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 23 VALUE   "from 1 - 9 must contain an N.        "  FOREGROUND-COLOR Magenta.
               05          COLUMN 60 VALUE                                        "³".
               05  LINE 24 COLUMN 21 VALUE "À"                                        FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 22 VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S04-A.
           03  LINE  9 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey PIC  X(01) USING W15-CAT AUTO.
           03  LINE 11 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey PIC  X(01) USING W15-ALL AUTO.
           03  LINE 12 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey PIC  X(01) USING W15-ZERO AUTO.
           03  LINE 13 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey PIC  X(01) USING W15-L30 AUTO.
           03  LINE 14 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey PIC  X(01) USING W15-L60 AUTO.
           03  LINE 15 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey PIC  X(01) USING W15-L90 AUTO.
           03  LINE 16 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey PIC  X(01) USING W15-30 AUTO.
           03  LINE 17 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey PIC  X(01) USING W15-60 AUTO.
           03  LINE 18 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey PIC  X(01) USING W15-90 AUTO.
           03  LINE 19 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey PIC  X(01) USING W15-90P AUTO.

       01  S05.
          03  LINE 10 COLUMN 12 VALUE "Key in Category code".

      *
      *        *******   *******    ******    ******   ********  *******   **     **  *******    ******** 
      *        **    **  **    **  **    **  **    **  **        **    **  **     **  **    **   **
      *        **    **  **   **   **    **  **        **        **    **  **     **  **   **    **
      *        *******   ******    **    **  **        *****     **    **  **     **  ******     *****
      *        **        **   **   **    **  **        **        **    **  **     **  **   **    **
      *        **        **    **  **    **  **    **  **        **    **  **     **  **    **   **
      *        **        **    **   ******    ******   ********  *******    *******   **    **   ********
      *
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS.
       AA00-MAIN       SECTION.
       AA00-INIT.
    IF LS-DSKDRV = "L"
        MOVE "Not available on Local drive"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO AA49.
           IF LS0-DBLEV < 2
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA00-INIT.
             PERFORM CH000.

       AA49.
            EXIT PROGRAM.

      *    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *           S C R E E N ,   K E Y B O A R D   &   M O U S E
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³      SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3       ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³                      SCREEN-SHADOW                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To routine is used to display a shadow down the right and ³
      *    ³ along the bottom of a pop-up box. The parameters that are ³
      *    ³ required:                                                 ³
      *    ³          SHADE-ROW   - Top line of the box.               ³
      *    ³          SHADE-COL   - Left line of box.                  ³
      *    ³          SHADE-WIDTH - Width of the box.                  ³
      *    ³          SHADE-LINES - Height of box.                     ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³                      CHECK-CORRECT                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine displays a pop-up window with the message    ³
      *    ³           "Press Y if correct - N if incorrect"           ³
      *    ³                                                           ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³                                                           ³
      *    ³ To start with the pop-up window higher or lower than row  ³
      *    ³ 20 (default); move another value to WS-MES-LINE.          ³
      *    *************************************************************
      *            T H I S   R O U T I N E   I S   U S E D   T O
      *       D I S P L A Y   I N F O R M A T I O N   M E S S A G E S
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Move the message to WS-DSP-MES and the top line of the   ³
      *    ³  message box to WS-MES-LINE. If WS-MES-LINE is zero the   ³
      *    ³  default will be 20.                                      ³
      *    ³                  PERFORM DISPLAY-MESSAGE                  ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ****   T H I S   R O U T I N E   I S   U S E D   T O
      *            D I S P L A Y   E R R O R   M E S S A G E S
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                      ERROR-MESSAGE                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To display the error message higher or lower (default is  ³
      *    ³ line 20) Move the line number which must be used as the   ³
      *    ³ top line to WS-MES-LINE. The message to be displayed must ³
      *    ³ be in WS-ERR-MES. PERFORM ERROR-MESSAGE.                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.CRT".

       COPY "LOCKED.REC".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³           ERASE SCREEN FROM SPECIFIED LOCATION            ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ Uses CRT-START as starting line and increases and CRT-END ³
      *    ³ as the ending line. The screen is cleared with Column 1   ³
      *    ³ and 80 containing "³" and columns 2 to 79 containing      ³
      *    ³ spaces.                                                   ³
      *    ³                                                           ³
      *    ³  eg.                                                      ³
      *    ³   MOVE 8         TO CRT-START.                            ³
      *    ³   MOVE 28        TO CRT-END.                              ³
      *    ³   PERFORM ERASE-SCREEN.                                   ³
      *    ³                                                           ³
      *    ³ To clear the entire screen and Display a new screen with  ³
      *    ³ headings (program/date/time/company):                     ³
      *    ³                                                           ³
      *    ³    CRT-PROGRAM (calling program)                          ³
      *    ³    CRT-HEADING (Screen heading)                           ³
      *    ³    CRT-COMPANY (Company name)                             ³
      *    ³    CRT-TERMINAL (consists of two fields:                  ³
      *    ³                  CRT-WRKHD and CRT-WRKST. See CRTHEAD)    ³
      *    ³    These fields should be populated at the start of the   ³
      *    ³    program.                                               ³
      *    ³    PERFORM HEADING-AND-CLEAR-SCREEN.                      ³
      *    ³                                                           ³
      *    ³ To change the screen heading:                             ³
      *    ³    MOVE "The new heading" TO CRT-HEADING.                 ³
      *    ³    PERFORM CHANGE-HEADING.                                ³
      *    ³                                                           ³
      *    ³ To change the time on the screen:                         ³
      *    ³    PERFORM CHANGE-TIME                                    ³
      *    ³                                                           ³
      *    ³ To clear lines 46 and 50:                                 ³
      *    ³    PERFORM CLEAR-L46-AND-50                               ³
      *    ³                                                           ³
      *    ³ To clear line 50:                                         ³
      *    ³    PERFORM CLEAR-L50                                      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "CLEAR.CRT".

       COPY "ABORT.PRN".

       COPY "PRINTUTL.AP8".

      /
      *    ****    READ FILES ROUTINES
      *
       AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "DEBTOR.RD1".

      /
       CA155-GET-DEBTOR        SECTION 2.
       CA155-ACCOUNT.
             MOVE W80-ACNO       TO DEB-ACNO.
             PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               MOVE 1            TO WS-ERROR
           ELSE
           IF DEB-ACNO NOT = W80-ACNO
               MOVE 2            TO WS-ERROR
           ELSE
               MOVE 0            TO WS-ERROR
               MOVE DEB-TYPE     TO W80-TYPE.
       CA160-EXIT.
             EXIT.

       CA165-NAME              SECTION 2.
       CA165-EXTRACT.
            MOVE ZERO   TO WS-ERROR.
             UNSTRING W60-NAME DELIMITED BY "*" OR "!"
           INTO W40-SURNAME W40-FNAME W40-SNAME
               OVERFLOW
           MOVE 1  TO WS-ERROR.
       CA170-EXIT.
             EXIT.

       CA175-ADDRESS           SECTION 2.
       CA175-EXTRACT.
            MOVE 0   TO WS-ERROR.
             UNSTRING W60-ADDRESS DELIMITED BY "*"
           INTO W40-ADD1 W40-ADD2
          W40-ADD3 W40-ADD4
               OVERFLOW
           MOVE 1  TO WS-ERROR.
       CA180-EXIT.
             EXIT.
      /
      *    ****    C L I E N T   L A B E L S
      *
       CH000        SECTION 5.
       CH00.
             PERFORM ERASE-SCREEN.
             DISPLAY S03.
             MOVE "Are you sure Y/N" TO WS-OPT-MES.
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
          IF WS-OPTION = "N"
               GO TO CH999.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *   **  *   *   * *   *  *  **  *   *   *     * *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *   *  **   *     *   *  *  *  **   *   *     * *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** * *
      *
      PERFORM OPEN-PRINTER.
      MOVE "P"   TO WS-COMMAND.
      MOVE "D"   TO W02-PRN-TYPE.
            MOVE SPACES  TO REP-DETAIL1.
    IF WS-PRINTER = "D" OR "S"
        MOVE 1   TO WS-S6
        GO TO CH30.
      *    *************************************************************
      *    ****   S E T U P   T E S T L A B E L S   T O   B E
      *    P R I N T E D
      *    *************************************************************
            MOVE ALL "X"  TO W46-LABELS.
            MOVE "9999"  TO W46-PC (1) W46-PC (2).
      *    *************************************************************
      *    ****   S E T U P   L A B E L - 1   ( L E F T )  T O  B E
      *    P R I N T E D
      *    *************************************************************
       CH05.
           IF W46-ADD2 (1) = SPACES
               MOVE W46-POST (1) TO W46-ADD2 (1)
              MOVE SPACES  TO W46-POST (1)
              GO TO CH10.
           IF W46-ADD3 (1) = SPACES
               MOVE W46-POST (1) TO W46-ADD3 (1)
              MOVE SPACES  TO W46-POST (1)
              GO TO CH10.
           IF W46-ADD4 (1) = SPACES
               MOVE W46-POST (1) TO W46-ADD4 (1)
              MOVE SPACES  TO W46-POST (1).
      *    *************************************************************
      *    ****   S E T U P   L A B E L - 2   ( R I G H T )  T O   B E
      *    P R I N T E D
      *    *************************************************************
       CH10.
           IF W46-ADD2 (2) = SPACES
               MOVE W46-POST (2) TO W46-ADD2 (2)
              MOVE SPACES  TO W46-POST (2)
              GO TO CH15.
           IF W46-ADD3 (2) = SPACES
               MOVE W46-POST (2) TO W46-ADD3 (2)
              MOVE SPACES  TO W46-POST (2)
              GO TO CH15.
           IF W46-ADD4 (2) = SPACES
               MOVE W46-POST (2) TO W46-ADD4 (2)
              MOVE SPACES  TO W46-POST (2).
      *    *************************************************************
      *    ****    P R I N T   L A B E L / S
      *    *************************************************************
       CH15.
            MOVE SPACES  TO LAB-L1.
            MOVE W46-NAME (1)  TO LAB-DET (1).
            MOVE W46-NAME (2)  TO LAB-DET (2).
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      *    *************************************************************
      *    ****   H A S   T H E   U S E R   A B O R T E D   T H E
      *    P R O G R A M   ?
      *    *************************************************************
    IF WS-COMMAND = "Z"
        GO TO CH20.
            MOVE W46-ADD1 (1)  TO LAB-DET (1).
            MOVE W46-ADD1 (2)  TO LAB-DET (2).
      PERFORM CALL-PRINTUTL.
      *    *************************************************************
      *    ****   H A S   T H E   U S E R   A B O R T E D   T H E
      *    P R O G R A M   ?
      *    *************************************************************
    IF WS-COMMAND = "Z"
        GO TO CH20.
            MOVE W46-ADD2 (1)  TO LAB-DET (1).
            MOVE W46-ADD2 (2)  TO LAB-DET (2).
      PERFORM CALL-PRINTUTL.
      *    *************************************************************
      *    ****   H A S   T H E   U S E R   A B O R T E D   T H E
      *    P R O G R A M   ?
      *    *************************************************************
    IF WS-COMMAND = "Z"
        GO TO CH20.
            MOVE W46-ADD3 (1)  TO LAB-DET (1).
            MOVE W46-ADD3 (2)  TO LAB-DET (2).
      PERFORM CALL-PRINTUTL.
      *    *************************************************************
      *    ****   H A S   T H E   U S E R   A B O R T E D   T H E
      *    P R O G R A M   ?
      *    *************************************************************
    IF WS-COMMAND = "Z"
        GO TO CH20.
            MOVE W46-ADD4 (1)  TO LAB-DET (1).
            MOVE W46-ADD4 (2)  TO LAB-DET (2).
      PERFORM CALL-PRINTUTL.
      *    *************************************************************
      *    ****   H A S   T H E   U S E R   A B O R T E D   T H E
      *    P R O G R A M   ?
      *    *************************************************************
    IF WS-COMMAND = "Z"
        GO TO CH20.
            MOVE W46-POST (1)  TO LAB-DET (1).
            MOVE W46-POST (2)  TO LAB-DET (2).
      PERFORM CALL-PRINTUTL.
      *    *************************************************************
      *    ****   H A S   T H E   U S E R   A B O R T E D   T H E
      *    P R O G R A M   ?
      *    *************************************************************
    IF WS-COMMAND = "Z"
        GO TO CH20.
            MOVE SPACES  TO LAB-L1.
      MOVE 3   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      *    *************************************************************
      *    ****   H A S   T H E   U S E R   A B O R T E D   T H E
      *    P R O G R A M   ?
      *    *************************************************************
    IF WS-COMMAND = "Z"
        GO TO CH20.
            MOVE 1   TO WS-S6.

       CH20.
      EXIT.
      *    *************************************************************
      *    ****   H A S   T H E   U S E R   A B O R T E D   T H E
      *    P R O G R A M   ?
      *    *************************************************************
       CH25.
    IF WS-COMMAND = "Z"
        GO TO CH130.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
              GO TO CH15.
      *    *************************************************************
      *    ****    D I S P L A Y   O P T I O N S
      *    *************************************************************
       CH30.
             MOVE 7                  TO SHADE-ROW.
             MOVE 23                 TO SHADE-COL.
             MOVE 38                 TO SHADE-WIDTH.
             MOVE 18                 TO SHADE-LINES.
             DISPLAY S04.
             PERFORM SCREEN-SHADOW.
             MOVE "Y"                TO W15-ALL W15-ZERO.
             MOVE "N"                TO W15-L30 W15-L60 W15-L90 W15-30  W15-60  W15-90 W15-90P W15-CAT.
      *    *************************************************************
      *    ****    G E T   U S E R   O P T I O N S
      *    *************************************************************
       CH35.
             ACCEPT S04-A.
             MOVE 1                  TO WS-S5.
             MOVE ZERO               TO WS-S4.
      *    *************************************************************
      *     C O N V E R T   A L L   I N P U T T O   U P P E R C A S E
      *    *************************************************************
       CH40.
      CALL "CBL_TOUPPER" USING W15-OPT(WS-S5)
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      *    *************************************************************
      *    ****  I S   T H E   O P T I O N   V A L I D  ( Y OR N ) ?
      *    *************************************************************
    IF NOT (W15-OPT(WS-S5) = "Y" OR "N")
        GO TO CH45.
      *    *************************************************************
      *    **** A C C U M U L A T E   N U M B E R   O F   O P T I O N S
      *  ( 2  T O  1 0 ) T H A T   A R E   ' Y ' .
      *    *************************************************************
    IF (WS-S5 > 1) AND (W15-OPT(WS-S5) = "Y")
        ADD 1   TO WS-S4.
    IF WS-S5 < 10
        ADD 1   TO WS-S5
        GO TO CH40.
      *    *************************************************************
      *    ****  M O R E   T H A N   T W O   O P T I O N S   =  Y ?
      *    *************************************************************
    IF WS-S4 > 2
        GO TO CH40-ERR.
      *    *************************************************************
      *    ****      O N L Y   T W O   O P T I O N S   =   Y ?
      *    *************************************************************
    IF WS-S4 = 2
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ The system only allows for option W15-ALL and W15-ZERO to ³
      *    ³ both contain a 'Y' to make a selection. For all other     ³
      *    ³ selections, only one option may contain a 'Y'.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
        IF (W15-ALL = "Y") AND
    (W15-ZERO = "Y")
     GO TO CH50
        ELSE
    GO TO CH40-ERR.
      GO TO CH50.
      *    *************************************************************
      *    ****   I N V A L I D  S E L E C T I O N
      *    *************************************************************
       CH40-ERR.
      MOVE "Invalid selection"
     TO WS-ERR-MES
            PERFORM ERROR-MESSAGE.
      GO TO CH35.
      *    *************************************************************
      *    **** A   C H A R A C T E R O T H E R   T H A N   A N  'N'
      *  O R   A   'Y' H A S B E E N   K E Y E D.
      *    *************************************************************
       CH45.
      MOVE "Selections must be Y or N"
     TO WS-ERR-MES
            PERFORM ERROR-MESSAGE.
      GO TO CH35.

       CH50.
            PERFORM ERASE-SCREEN.
            DISPLAY S03-1.
      *    *************************************************************
      *    ****  H A S  T H E  P E R  C A T E G O R Y   O P T I O N
      *   B E E N   S E L E C T E D ?
      *    *************************************************************
          IF NOT (W15-CAT = "Y")
               MOVE ZERO TO W45-CODE
              GO TO CH60.
             DISPLAY S05.

       CH55.
            ACCEPT W45-CODE AT 1034
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
           IF W45-CODE = ZERO
              GO TO CH115.
           IF W45-CODE > 80
              MOVE "Invalid code"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO CH55.
            DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
             ADD 121 W45-CODE    GIVING W90-RESULT.
             DIVIDE W90-RESULT BY 2
                                 GIVING W90-CATKEY
                                 REMAINDER W90-CS.
             ADD 1               TO W90-CS.
             MOVE W90-CATKEY     TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            DISPLAY PAR-CDESC(W90-CS) AT 1039
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
              GO TO CH55.

       CH60.
             DISPLAY "A" AT 1212 WITH FOREGROUND-COLOR Cyan HIGHLIGHT 
                     "ccount or " WITH FOREGROUND-COLOR Cyan
                     "N" WITH FOREGROUND-COLOR Cyan HIGHLIGHT 
                     "ame sequence" WITH FOREGROUND-COLOR Cyan.
             MOVE "N"            TO WS-SEQ.

       CH65.
      ACCEPT WS-SEQ AT 1237
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-SEQ
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-SEQ = "A" OR "N")
        GO TO CH65.
      DISPLAY "Is this a restart " AT 1412
       "Y" WITH FOREGROUND-COLOR Brown HIGHLIGHT "/"
       "N" WITH FOREGROUND-COLOR Brown HIGHLIGHT.
      MOVE "N"   TO WS-OPTION.

       CH70.
      ACCEPT WS-OPTION AT 1434
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-OPTION = "N" OR "Y")
        GO TO CH70.
           IF WS-OPTION = "Y"
        GO TO CH120.
             MOVE " !"           TO DEB-NAME DEB-ACNO.

       CH75.
             DISPLAY "Debtor account:" AT 1612.
           IF WS-SEQ = "A"
               PERFORM START-AT-DEBT-AC THRU READ-DEBTOR-EXIT
           ELSE
               PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
              GO TO CH135.

       CH80.
             PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
              GO TO CH115.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the account is an internal account then read the next  ³
      *    ³ account. (Do not print labels for internal accounts).     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
    IF DEB-TYPE = 3
        GO TO CH80.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the option for the output is the screen then do not    ³
      *    ³ display the Debtor account number on the screen.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       CH85.
    IF NOT (WS-PRINTER = "S")
        DISPLAY DEB-ACNO AT 1628
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the selection is per category code then if the account ³
      *    ³ is not for the selected category then read next account.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
          IF W15-CAT = "Y"
               IF W45-CODE NOT = DEB-CAT
           GO TO CH80.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the account balance is zero and the user has selected  ³
      *    ³ not to include zero accounts then read next account.      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
    IF DEB-BAL = ZERO
        IF W15-ZERO = "N"
     GO TO CH80.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the user has selected only zero balance accounts and   ³
      *    ³ the balance is not zero then read next account.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
    IF (W15-ALL = "N")
        IF (W15-ZERO = "Y")
     IF NOT (DEB-BAL = ZERO)
         GO TO CH80.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the user has selected only accounts with a balance less³
      *    ³ than 30 days and the account has an amount outstanding for³
      *    ³ more than 30 days then read the next account.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
    IF (W15-L30 = "Y") AND
       ((DEB-30 > ZERO) OR (DEB-60 > ZERO) OR
        (DEB-90 > ZERO) OR (DEB-120 > ZERO))
        GO TO CH80.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the user has selected only accounts with a balance less³
      *    ³ than 60 days and the account has an amount outstanding for³
      *    ³ more than 60 days then read the next account.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
    IF (W15-L60 = "Y") AND
       ((DEB-60 > ZERO) OR (DEB-90 > ZERO) OR (DEB-120 > ZERO))
        GO TO CH80.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the user has selected only accounts with a balance less³
      *    ³ than 90 days and the account has an amount outstanding for³
      *    ³ more than 90 days then read the next account.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
    IF (W15-L90 = "Y") AND
       ((DEB-90 > ZERO) OR (DEB-120 > ZERO))
        GO TO CH80.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the user has selected only 30 day accounts and the     ³
      *    ³ account is not a 30 day account or has amounts outstanding³
      *    ³ for more than 30 days then read the next account.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
    IF (W15-30 = "Y")
        IF (DEB-30 = ZERO) OR (DEB-60 > ZERO) OR (DEB-90 > ZERO)
     OR (DEB-120 > ZERO)
     GO TO CH80.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the user has selected only 60 day accounts and the     ³
      *    ³ account is not a 60 day account or has amounts outstanding³
      *    ³ for more than 60 days then read the next account.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
    IF (W15-60 = "Y")
        IF (DEB-60 = ZERO) OR (DEB-90 > ZERO) OR (DEB-120 > ZERO)
     GO TO CH80.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the user has selected only 90 day accounts and the     ³
      *    ³ account is not a 90 day account or has amounts outstanding³
      *    ³ for more than 60 days then read the next account.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
    IF (W15-90 = "Y")
        IF (DEB-90 = ZERO) OR (DEB-120 > ZERO)
     GO TO CH80.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the user has selected only 90+ day accounts and the    ³
      *    ³ account is not a 90+ day account then read the next       ³
      *    ³ account.             ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
    IF (W15-90P = "Y")
        IF (DEB-120 = ZERO)
     GO TO CH80.
          IF NOT (DEB-TYPE = 1)
              MOVE DEB-NAME  TO W40-NAME
              GO TO CH110.
            MOVE DEB-NAME  TO W60-NAME.
             PERFORM CA165-NAME.
           IF DEB-LANG = "A"
              MOVE W75-ATITLE (DEB-TITLE)
     TO W60-NAME
           ELSE
              MOVE W75-ETITLE (DEB-TITLE)
     TO W60-NAME.
            MOVE 7   TO WS-S8.

       CH90.
           IF W60-NCHAR (WS-S8) NOT = SPACE
              GO TO CH95.
           IF WS-S8 > 0
              SUBTRACT 1  FROM WS-S8
              GO TO CH90.

       CH95.
            MOVE 1   TO WS-S7.
           IF WS-S8 > 0
              ADD 2   TO WS-S8
           ELSE
              MOVE 1   TO WS-S8.
             MOVE W40-NCHAR (25) TO W60-NCHAR (WS-S8).
            ADD 1   TO WS-S8.
            MOVE "."   TO W60-NCHAR (WS-S8).
           IF W40-NCHAR (49) = SPACE
              GO TO CH100.
            ADD 1   TO WS-S8.
             MOVE W40-NCHAR (49) TO W60-NCHAR (WS-S8).
            ADD 1   TO WS-S8.
            MOVE "."   TO W60-NCHAR (WS-S8).

       CH100.
            ADD 2   TO WS-S8.
       CH105.
            MOVE W40-NCHAR (WS-S7)
     TO W60-NCHAR (WS-S8).
           IF WS-S7 < 24
              ADD 1   TO WS-S7
              ADD 1   TO WS-S8
              GO TO CH105.
            MOVE W60-NAME  TO W40-NAME.
       CH110.
            MOVE DEB-ADDRESS  TO W60-ADDRESS.
             PERFORM CA175-ADDRESS.
            MOVE W40-NAME  TO W46-NAME (WS-S6).
            MOVE W40-ADD1  TO W46-ADD1 (WS-S6).
            MOVE W40-ADD2  TO W46-ADD2 (WS-S6).
            MOVE W40-ADD3  TO W46-ADD3 (WS-S6).
            MOVE W40-ADD4  TO W46-ADD4 (WS-S6).
            MOVE DEB-POST  TO W46-PC (WS-S6).
            ADD 1   TO WS-S6.
           IF WS-S6 > 2
              PERFORM CH05 THRU CH20
              MOVE SPACES  TO W46-LABELS.
             PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              GO TO CH130.
      GO TO CH80.
       CH115.
           IF WS-S6 > 1
              PERFORM CH05 THRU CH20.
      *    *************************************************************
      *    ****   H A S   T H E   U S E R   A B O R T E D   T H E
      *    P R O G R A M   ?
      *    *************************************************************
    IF WS-COMMAND = "Z"
        GO TO CH130.
          IF W45-CODE NOT = ZERO
              GO TO CH55.
      GO TO CH135.

       CH120.
             MOVE SPACES         TO W80-ACNO.
      MOVE 1801   TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
            DISPLAY "Account No:" AT 1812.
            ACCEPT W80-ACNO AT 1824
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
    IF W80-ACNO = SPACES
        GO TO CH70.
             MOVE W80-ACNO       TO DEB-ACNO.
           IF WS-SEQ = "A"
        GO TO CH75.
             PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
             PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.

       CH125.
             PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
           IF W80-ACNO NOT = DEB-ACNO
        GO TO CH125.
             DISPLAY "Debtor account:" AT 1612.
      GO TO CH85.

       CH130.
            MOVE "**** PRINT ABORTED BY OPERATOR ****"
     TO REP-DETAIL1.
      MOVE 99   TO WS-ADVANCE.
      MOVE "E"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.

       CH135.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.

       CH999.
             EXIT.

      /
       ZA00-INIT       SECTION 9.
       ZA00-OPEN.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

       ZA02.
            MOVE 11   TO WS-PARKEY.
            MOVE 1   TO WS-SUB-2.
       ZA15-READ-TRANS.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE 1   TO WS-SUB-1.
       ZA20-TRAN-LOOP.
            MOVE PAR-T-CODE (WS-SUB-1)
     TO W00-TRAN-CODE (WS-SUB-2).
            MOVE PAR-E-DESC (WS-SUB-1)
     TO W00-E-DESC (WS-SUB-2).
            MOVE PAR-A-DESC (WS-SUB-1)
     TO W00-A-DESC (WS-SUB-2).
            MOVE PAR-ACTION (WS-SUB-1)
     TO W00-ACTION (WS-SUB-2).
            ADD 1   TO WS-SUB-2.
           IF WS-SUB-1 < 2
              ADD 1   TO WS-SUB-1
               GO TO ZA20-TRAN-LOOP.
           IF WS-PARKEY < 60
              ADD 1   TO WS-PARKEY
               GO TO ZA15-READ-TRANS.

       ZA25.
             MOVE 1                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY        TO W95-COMP.
             MOVE PAR-DMY            TO W12-TODAY.
             MOVE PAR-YMD            TO W12-T-YMD.
             MOVE 1                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             GO TO ZA999-EXIT.

       ZA200.

       COPY "LOCKED.PRO".

       ZA205.
             EXIT PROGRAM.
       ZA999-EXIT.
             EXIT.
      /
       ZB000       SECTION 9.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 2          MOVE W02-NETWORK   TO WS-FILE
                               MOVE WS-NETKEY     TO WS-KEY
               WHEN 6          MOVE W02-DEBTOR    TO WS-FILE
                               MOVE DEB-ACNO      TO WS-KEYX
               WHEN 15         MOVE WS-PARID     TO WS-FILE
                               MOVE WS-PARKEY    TO WS-KEY
               WHEN OTHER      MOVE "ERROR"       TO WS-FILE
                               MOVE "DTP023"      TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
