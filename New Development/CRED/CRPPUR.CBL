      $SET LINKCOUNT"512" GNT"CRPPUR.GNT"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******   *******   **    **  *******    *
      *   **    **  **    **  **    **  **    **  **    **  **    **   *
      *   **        **    **  **    **  **    **  **    **  **    **   *
      *   **        ******    *******   *******   **    **  ******     *
      *   **        **   **   **        **        **    **  **   **    *
      *   **    **  **    **  **        **        **    **  **    **   *
      *    ******   **    **  **        **         ******   **    **   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     P U R C H A C E S   T R A C K I N G   P R O G R A M        *
      *                                                                *
      *       Version 9.04.05 - June 2018                              *
      *                                                                *
      ******************************************************************
      *                                                                *
      * June 2018 - Added program CRPRDWR to handle IO for Creditor    *
      *             file and COPY file CA155.CRP to call and handlw IO *
      *             for the CREDITOR file. SEE CA155.CRP and CRPRDWR   *
      *             for details.                                       *
      *                                                                *
      ******************************************************************
      * NB -- To do list:                                              *
      *       Include a print of the various lookups. Currently all    *
      *       tracking displayed in a screen layout.                   *
      *ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     CRPPUR.
       AUTHOR.        James William Lemmon.
       DATE-WRITTEN.   OCTOBER 2005.

                COPYRIGHT NOTICE: COPYRIGHT (C) 2005 - 2017
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

       COPY "CREDIT.SL".

       COPY "CRDMEM.SL".

       COPY "DEPART.SL".

       COPY "PARAM.SL".

       COPY "PURDEX.SL".

       COPY "STOCK.SL".

       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "CREDIT.FDE".

       COPY "CRDMEM.FDE".

       COPY "DEPART.FDE".

       COPY "PARAM.FDE".

       COPY "PURDEX.FDE".

       COPY "STOCK.FDE".

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   **
      *         **    *    **   **    **   **   **    **  **
      *         **   ***   **   **    **   ******     *****
      *          ** ** ** **    **    **   **   **    **  **
      *           ***   ***     **    **   **    **   **   **
      *            *     *       ******    **    **   **    **
      *
       WORKING-STORAGE SECTION.
       77  WS-CHECK                PIC  X(18)           VALUE "aeWlimemnomLalismJ".
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
       77  WS-IXS1                 PIC  9(04)    COMP-5.
       77  WS-ENQPOS               PIC  9(06)    COMP-5.
       77  WS-ENQEND               PIC  9(06)    COMP-5.
       77  WS-NETKEY               PIC  9(06)    COMP-5.
       77  WS-PARKEY               PIC  9(06)    COMP-5.
       77  WS-KEYSTR               PIC  9(06)    COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-USE-ITM              PIC  X(01).
       77  WS-EXT-STK              PIC  X(01).
       77  WS-ENQ                  PIC  X(01).
       77  WS-MONTH                PIC  9(02).
       77  WS-ETYPE                PIC  X(01).
       77  WS-DESC-H1              PIC  X(14)           VALUE "Description".
       77  WS-DESC-H2              PIC  X(14)           VALUE "Description 2".
       77  WS-RT-HD                PIC  X(10)           VALUE "-Retail   ".
       77  WS-WS-HD                PIC  X(10)           VALUE "-Wholesale".
       77  WS-CS-HD                PIC  X(10)           VALUE "-Cash sale".
       77  WS-ERROR                PIC  9(01)           VALUE ZERO.
       77  WS-ER1                  PIC  X(09)           VALUE "No record".
       77  PRG-NAME                PIC  X(12)           VALUE SPACES.
       77  PRG-CRED-LUP            PIC  X(12)           VALUE "CRP\CRPLOOK".
       77  TODAY-DDMMYY            PIC  9(08) COMP-5.
       77  WS-USUB                 PIC  9(04) COMP-5.
  
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
           03  CRT-START           PIC  9(02).
           03  CRT-END             PIC  9(02).
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15)           VALUE "CRPPUR".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40)           VALUE "CREDITOR - PURCHASE TRACKING".
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
    03  WS-MODULE       PIC  X(03) VALUE "DTP".
    03  WS-PROG        PIC  X(03) VALUE "ENQ".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  BLOCK-DETAIL1.
    03  ORIGINAL-VID1.
        05  ORIGINAL-CHAR1 PIC  X(01) OCCURS 80.
    03  REVERSE-VID1.
        05  REVERSE-CHAR1  PIC  X(01) OCCURS 80.

       01  SHADOW-DETAIL1.
    03  SHAD1-ROW       PIC  9(02) COMP-X.
    03  SHAD1-COL       PIC  9(02) COMP-X.
    03  SHAD1-LINES     PIC  9(02) COMP-X.
    03  SHAD1-WIDTH     PIC  9(02) COMP-X.
    03  SHAD1-CHAR      PIC  X(01).

       01  CRT-DETAIL1.
    03  TOP-ROW1       PIC  9(02) COMP-X.
    03  BOTTOM-ROW1     PIC  9(02) COMP-X.
    03  STRING-LENGTH1  PIC  9(04) COMP-X.
    03  SCREEN-POSITION1.
        05  SCREEN-LIN1 PIC  9(02) COMP-X.
        05  SCREEN-COL1 PIC  9(02) COMP-X.

       01  BLOCK-DETAIL2.
    03  ORIGINAL-VID2.
        05  ORIGINAL-CHAR2 PIC  X(01) OCCURS 80.
    03  REVERSE-VID2.
        05  REVERSE-CHAR2  PIC  X(01) OCCURS 80.

       01  SHADOW-DETAIL2.
    03  SHAD2-ROW       PIC  9(02) COMP-X.
    03  SHAD2-COL       PIC  9(02) COMP-X.
    03  SHAD2-LINES     PIC  9(02) COMP-X.
    03  SHAD2-WIDTH     PIC  9(02) COMP-X.
    03  SHAD2-CHAR      PIC  X(01).

       01  CRT-DETAIL2.
    03  TOP-ROW2       PIC  9(02) COMP-X.
    03  BOTTOM-ROW2     PIC  9(02) COMP-X.
    03  STRING-LENGTH2  PIC  9(04) COMP-X.
    03  SCREEN-POSITION2.
        05  SCREEN-LIN2 PIC  9(02) COMP-X.
        05  SCREEN-COL2 PIC  9(02) COMP-X.

       COPY "W05.VAT".

       COPY "W10.STK".

      *
      *    Date routines for accepting checking and formatting have
      *    been removed from each program and the DateCheck program
      *    will be called to handle these routines.
      *
      *    The following copy 'DateVariables' now includes the 'W12.WS'
      *    and 'W20.WS' variables and is passed to DateCheck in the CALL
      *    statement.
      *
       COPY "DateVariables".

       01  W15-INPUT-AREA.
          03  W15-ACNO        PIC X(06).
           03  W15-SNO         REDEFINES W15-ACNO.
               05  W15-APREF   PIC X(01).
              05  W15-SERIAL  PIC X(05).
          03  W15-NAME.
              05  W15-NCHAR   PIC  X(01) OCCURS 40.
          03  W15-INVN        PIC  X(08).
          03  W15-INVD        PIC  9(08)     COMP-3.
    03  W15-DESC        PIC  X(20).
       01  W15-DATES.
          03  W15-START       PIC 9(08).
           03  W15-SDTE-DMY REDEFINES W15-START.
               05  W15-SDAY    PIC 9(02).
               05  W15-SMNTH   PIC 9(02).
        05  W15-SCENT   PIC 9(02).
               05  W15-SYEAR   PIC 9(02).
          03  W15-S-YMD       PIC 9(08).
           03  W15-STRT-YMD REDEFINES W15-S-YMD.
        05  W15-SCC     PIC 9(02).
               05  W15-SYY     PIC 9(02).
               05  W15-SMM     PIC 9(02).
               05  W15-SDD     PIC 9(02).
          03  W15-END        PIC 9(08).
           03  W15-EDTE-DMY REDEFINES W15-END.
               05  W15-EDAY    PIC 9(02).
               05  W15-EMNTH   PIC 9(02).
        05  W15-ECENT   PIC 9(02).
               05  W15-EYEAR   PIC 9(02).
          03  W15-E-YMD       PIC 9(08).
           03  W15-END-YMD REDEFINES W15-E-YMD.
        05  W15-ECC     PIC 9(02).
               05  W15-EYY     PIC 9(02).
               05  W15-EMM     PIC 9(02).
               05  W15-EDD     PIC 9(02).

       01  W25-CALCS.
           03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
               05  W25-WHOLE   PIC 9(05).
               05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
        05  W25-KEY    PIC  9(04).
        05  W25-SUB    PIC  9(01).
        05  FILLER     PIC  9(02).
          03  W25-TOTAL      PIC S9(09)V99 COMP-3.
          03  W25-VALUE      PIC S9(09)V99 COMP-3.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W75-KEYS.
    03  W75-S       PIC  9(02) COMP-5.
    03  W75-S1       PIC  9(02) COMP-5.
    03  W75-S2       PIC  9(02)    COMP-5.
    03  W75-CNO       PIC  X(06) OCCURS 18.

       01  W75-STORE.
          03  W75-SNAME      PIC  X(08).
    03  W75-SREF       PIC  X(08).
    03  W75-SACN       PIC  X(06).
    03  W75-SDTE       PIC  9(06).

       01  W95-STM.
           03  W95-COMP        PIC  X(40).
           03  W95-D.
               05  W95-DTE     PIC  Z9/99/9999.
           03  W95-PERIOD.
               05  W95-SDTE    PIC  Z9/99/9999.
               05  FILLER      PIC  X(03) VALUE " - ".
               05  W95-EDTE    PIC  Z9/99/9999.

       01  W100-EDIT.
           03  W100-COST       PIC  Z(06)9.99.
           03  W100-TCOST      PIC  Z(06)9.99.
           03  W100-QNT        PIC  Z(04)9.99.
           03  W100-QNT1       PIC  Z(04)9.

       01  W200-MEMO.
           03  W200-MEM-FLDS.
               05  W200-MEM1   PIC  X(60).
               05  W200-MEM2   PIC  X(60).
               05  W200-MEM3   PIC  X(60).
               05  W200-MEM4   PIC  X(60).
               05  W200-MEM5   PIC  X(60).
               05  W200-MEM6   PIC  X(60).
               05  W200-MEM7   PIC  X(60).
               05  W200-MEM8   PIC  X(60).
               05  W200-MEM9   PIC  X(60).
               05  W200-MEM10  PIC  X(60).
               05  W200-MEM11  PIC  X(60).
               05  W200-MEM12  PIC  X(60).
           03  W200-MEM-FOC REDEFINES W200-MEM-FLDS.
               05  W200-MEM    PIC  X(60) OCCURS 12.

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

       01  S-MENU.
          03  LINE  2 COLUMN 32 FOREGROUND-COLOR Grey HIGHLIGHT
     VALUE "PURCHASE TRACKING".
    03  LINE  4 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
    03  LINE  5 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "PURCHASE TRACKING OPTIONS".
    03        COLUMN 56 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " ³".
    03  LINE  6 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "1".
    03        COLUMN 32 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " Selective per Account   ³".
    03  LINE  7 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "2".
    03        COLUMN 32 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " Selective per Invoice   ³".
          03  LINE  8 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "3".
    03        COLUMN 32 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " All - Specified Period  ³".
          03  LINE  9 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "0".
    03        COLUMN 32 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " Exit                    ³".
    03  LINE 10 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  MENU-INSTRUCT.
    03  LINE 25 COLUMN  2 BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Blue
     VALUE "Use ".
    03        COLUMN  6 HIGHLIGHT
     BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Brown
     VALUE "".
    03        COLUMN  7 BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Blue
     VALUE " or ".
    03        COLUMN 11 HIGHLIGHT
     BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Brown
     VALUE "".
    03        COLUMN 12 BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Blue
     VALUE " to point to an item, and ".
    03        COLUMN 38 HIGHLIGHT
     BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Brown
     VALUE "<Enter>".
    03        COLUMN 45 BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Blue
     VALUE " to Select ".
    03        COLUMN 56 HIGHLIGHT
     BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Brown
     VALUE "<Esc>".
    03        COLUMN 61 BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Blue
     VALUE " to Exit".

       01  S02D.
    03  LINE  8 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
    03  LINE  9 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "³    Transactions for Period.    ³".
    03  LINE 10 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
    03  LINE 11 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "³   Starting Date :              ³".
    03  LINE 12 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "³   Ending Date   :              ³".
    03  LINE 13 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
    03  LINE 14 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "³Dates to be keyed in as ".
    03        COLUMN 49 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE "DDMMCCYY".
    03        COLUMN 57 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE "³".
    03  LINE 15 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S02A.
    03  LINE 11 COLUMN 44 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC 99/99/9999 USING W15-START AUTO.
    03  LINE 12 COLUMN 44 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC 99/99/9999 USING W15-END AUTO.

       01  S05D.
          03  LINE  2 COLUMN 32 FOREGROUND-COLOR Grey HIGHLIGHT VALUE
               "PURCHASE TRACKING".
    03  LINE  4 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
    03  LINE  5 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "³    Transactions for Period.    ³".
    03  LINE  6 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
    03  LINE  7 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "³   Starting Date :              ³".
    03  LINE  8 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "³   Ending Date   :              ³".
    03  LINE  9 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
    03  LINE 10 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "³Dates to be keyed in as ".
    03        COLUMN 49 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE "DDMMCCYY".
    03        COLUMN 57 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE "³".
    03  LINE 11 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S05A.
    03  LINE  7 COLUMN 44 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC 99/99/9999 USING W15-START AUTO.
    03  LINE  8 COLUMN 44 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC 99/99/9999 USING W15-END AUTO.

       01  S10H.
          03  LINE  2 COLUMN 26 FOREGROUND-COLOR Grey HIGHLIGHT VALUE
               "PURCHASE TRACKING PER SUPPLIER".

       01  S12H.
          03  LINE  2 COLUMN 23 FOREGROUND-COLOR Grey HIGHLIGHT VALUE
               "PURCHASE TRACKING SUPPLIER/REFERENCE".

       01  S14H.
          03  LINE  2 COLUMN 22 FOREGROUND-COLOR Grey HIGHLIGHT VALUE
               "PURCHASE TRACKING FOR SPECIFIED PERIOD".

       01  S-MOV.
           03  LINE  4 COLUMN  2 REVERSE-VIDEO VALUE
              "Supply Ref. No.    Date        Item Code      Quantity Cost Price Total Cost T".
           03  LINE  5 COLUMN  1 BACKGROUND-COLOR Black VALUE "Æ".
           03        COLUMN  2 VALUE "ÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍ".
           03        COLUMN 80 BACKGROUND-COLOR Black VALUE "µ".
           03  LINE 22 COLUMN  1 BACKGROUND-COLOR Black VALUE "Æ".
           03        COLUMN  2 VALUE "ÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍ".
           03        COLUMN 80 BACKGROUND-COLOR Black VALUE "µ".

       01  S55.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
               05  LINE 50 COLUMN  2 VALUE "F1"                                           FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN  4 VALUE   "=Help,".
               05          COLUMN 10 VALUE         "F2"                                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 12 VALUE           " to do Creditor Lookup ".
               05          COLUMN 35 VALUE                                  "Esc"         FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 38 VALUE                                     " to exit".

       01  S-SUP-1.
           03  LINE  3 COLUMN  2 VALUE "Supplier".
           03          COLUMN 35 VALUE "Name".

       01  S-SUP-D1.
    03  S-SUP-1A.
    05  LINE  3 COLUMN 11 FOREGROUND-COLOR Cyan HIGHLIGHT
     PIC  X(06) USING W15-ACNO AUTO.
    03  S-SUP-1B.
    05  LINE  3 COLUMN 40 FOREGROUND-COLOR Cyan HIGHLIGHT
     PIC  X(40) FROM W15-NAME.

       01  S-SUP-2.
          03  LINE  4 COLUMN  2 REVERSE-VIDEO VALUE
              "     Item Code     Description           Refer  Quant Cost Price Total Cost T".
          03  LINE  5 COLUMN  1 BACKGROUND-COLOR Black VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍµdd/mm/yyyy - dd/mm/yyyyÆÍÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍ".
    03        COLUMN 80 BACKGROUND-COLOR Black VALUE "µ".
          03  LINE 22 COLUMN  1 BACKGROUND-COLOR Black VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍ".
    03        COLUMN 80 BACKGROUND-COLOR Black VALUE "µ".

       01  S-REF-1.
    03  LINE  3 COLUMN  2 VALUE "A/C".
    03        COLUMN 13 VALUE "Name".
    03        COLUMN 52 VALUE "Ref".
    03        COLUMN 65 VALUE "Date".

       01  S-REF-D1.
    03  S-REF-1A.
    05  LINE  3 COLUMN  6 FOREGROUND-COLOR Cyan HIGHLIGHT
     PIC  X(06) USING W15-ACNO AUTO.
    03  S-REF-1B.
    05  LINE  3 COLUMN 18 FOREGROUND-COLOR Cyan HIGHLIGHT
     PIC  X(32) FROM W15-NAME.
    03  S-REF-1C.
    05  LINE  3 COLUMN 56 FOREGROUND-COLOR Cyan HIGHLIGHT
     PIC  X(08) USING W15-INVN AUTO.
    03  S-REF-1D.
    05  LINE  3 COLUMN 70 FOREGROUND-COLOR Cyan HIGHLIGHT
     PIC  Z9/99/9999 FROM W15-INVD.

       01  S-REF-2.
           03  LINE  4 COLUMN  2 REVERSE-VIDEO VALUE
              "     Item Code     Description                  Quantity Cost Price Total Cost".
          03  LINE  5 COLUMN  1 BACKGROUND-COLOR Black VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR Black VALUE "µ".
          03  LINE 22 COLUMN  1 BACKGROUND-COLOR Black VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR Black VALUE "µ".

       COPY "CRDLUP.CRT".

       01  S-MEM.
           03  LINE  6 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G9.
           03          COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G10.
           03  LINE  7 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G3.
           03          COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey VALUE
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E  ".
           03          COLUMN 78 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G3.
           03  LINE  8 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G14.
           03          COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING WS-HME-LINE.
           03          COLUMN 78 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G13.
           03  LINE  9 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 10 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 11 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 12 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 13 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 14 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 15 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 16 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 17 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 18 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 19 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 20 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 21 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G11.
           03          COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G12.
       01  SA-MEM.
           03  LINE  9 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM1 AUTO.
           03  LINE 10 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM2 AUTO.
           03  LINE 11 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM3 AUTO.
           03  LINE 12 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM4 AUTO.
           03  LINE 13 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM5 AUTO.
           03  LINE 14 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM6 AUTO.
           03  LINE 15 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM7 AUTO.
           03  LINE 16 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM8 AUTO.
           03  LINE 17 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM9 AUTO.
           03  LINE 18 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM10 AUTO.
           03  LINE 19 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM11 AUTO.
           03  LINE 20 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM12 AUTO.

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
       AA000-MAIN        SECTION.
       AA000-INIT.
           IF LS0-STLEV < 3
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
            PERFORM ZA000-INIT.
      PERFORM BA000.

       AA49.
             EXIT PROGRAM.

       COPY "AA50C.LUP".
       
       COPY "AA900.ALM".

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

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                        OPT-MESSAGE                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine is used to allow the OPERATOR to respond to  ³
      *    ³ a request for an option, so that the correct action can   ³
      *    ³ be performed by the program. The routine will display the ³
      *    ³ message in a pop-up window and allow the OPERATOR to      ³
      *    ³ respond to the 'question'.                                ³
      *    ³                                                           ³
      *    ³ The option request must be placed in WS-ERR-MES and may   ³
      *    ³ not exceed 48 characters. The message will be centred in  ³
      *    ³ the window. An example of a message request follows:      ³
      *    ³                                                           ³
      *    ³   MOVE "Print transactions (Y/N) [ ]" TO WS-ERR-MES.      ³
      *    ³   MOVE Instruction    TO WS-INSTR.                        ³
      *    ³       [see Accptopt for instruction values]               ³
      *    ³   PERFORM OPT-MESSAGE.                                    ³
      *    ³                                                           ³
      *    ³ This would be displayed as:                               ³
      *    ³      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿   ³
      *    ³      ³          Print transactions (Y/N) [ ]          ³°° ³
      *    ³      ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°° ³
      *    ³        °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° ³
      *    ³                                                           ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³                                                           ³
      *    ³ The system will display the message box with the top line ³
      *    ³ as the value of WS-MES-LINE. If WS-MES-LINE is zero the   ³
      *    ³ default will be 20.                                       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "OPTION.CRT".

       COPY "LOCKED.RC2".

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

      *    *************************************************************
      *    ****        D A T E  C H E C K   R O U T I N E
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Perform this routine with the date in W10-DATE (PIC      ³
      *    ³  99/99/9999) with the format as DD/MM/CCYY         ³
      *    ³              ³
      *    ³  On exiting - WS-ERROR will be zero (0) if the date is    ³
      *    ³  correct or one (1) if the date is incorrect.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "DATE.CHK".

       COPY "AA300.STK".

      /
      *    ****    READ FILES ROUTINES
      *
       AC000              SECTION.

       COPY "CONTROL.RD".

       COPY "CREDIT.RD".

       COPY "CRDMEM.RD".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "PURDEX.RD".

       COPY "STOCK.RD".

      *COPY "CREDIT.LUP".

       COPY "APAC.HLP".

      /    *************************************************************
      *    ****   M E N U   ( P U R C H A S E T R A C K I N G )
      *    *************************************************************
      *
      *       ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *       ³ PURCHASE TRACKING OPTIONS ³°°
      *       ³  1 Selective per Account  ³°°
      *       ³  2 Selective per Invoice  ³°°
      *       ³  3 All - Specified Period ³°°
      *       ³  0 Exit                   ³°°
      *       ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°
      *         °°°°°°°°°°°°°°°°°°°°°°°°°°°°°
      *
       BA000         SECTION.
       BA00.
             MOVE 5                  TO SHADE-ROW.
             MOVE 31                 TO SHADE-COL.
             MOVE 27                 TO SHADE-WIDTH.
             MOVE 6                  TO SHADE-LINES.
             DISPLAY S-MENU.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
             MOVE 5                  TO TOP-ROW.
             MOVE 8                  TO BOTTOM-ROW.
             MOVE 4                  TO SCREEN-LIN.
             MOVE 31                 TO SCREEN-COL.
             MOVE 24                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       BA05.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BA10.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     PERFORM MOVE-BLOCK-UP
                 WHEN DOWN-KEY   PERFORM MOVE-BLOCK-DOWN
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
               IF WS-OPTION < 0 OR > 3
                   PERFORM AA900-ALARM
                   GO TO BA05
               END-IF
               GO TO BA16
           ELSE
               PERFORM AA900-ALARM
               GO TO BA05.

       BA15.
           IF SCREEN-LIN < 5
               PERFORM AA900-ALARM
               GO TO BA05
           ELSE
           IF SCREEN-LIN = 5
               MOVE "1"   TO WS-OPTION
           ELSE
           IF SCREEN-LIN = 6
               MOVE "2"   TO WS-OPTION
           ELSE
           IF SCREEN-LIN = 7
               MOVE "3"   TO WS-OPTION
           ELSE
           IF SCREEN-LIN = 8
               MOVE "0"   TO WS-OPTION.
             GO TO BA18.

       BA16.
             PERFORM CLEAR-BLOCK.
           IF WS-OPTION = 0
               MOVE 8   TO SCREEN-LIN
           ELSE
           IF WS-OPTION = 1
               MOVE 5   TO SCREEN-LIN
           ELSE
           IF WS-OPTION = 2
               MOVE 6   TO SCREEN-LIN
           ELSE
           IF WS-OPTION = 3
               MOVE 7   TO SCREEN-LIN.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       BA18.
           IF WS-OPTION = "0"
               GO TO BA999.
           IF WS-OPTION = "1"
               PERFORM DA000
           ELSE
           IF WS-OPTION = "2"
               PERFORM EA000
           ELSE
           IF WS-OPTION = "3"
               PERFORM FA000.
             PERFORM ERASE-SCREEN.
             GO TO BA00.

       BA999.
             EXIT.
      *
      *    ****    Read/Write Creditor record
      *
      *    This routine is performed and the following are the variables
      *    that are required to have been set up.
      *
      *          VARIABLES USED FOR CRPRDWR as instructions and data
      *
      *    03  W15-INSTR             PIC  X(01).
      *
      *        88  ADD-ACCOUNT                     VALUE "A".
      *
      *            W15-CREDREC needs to contain details for the account 
      *                        that must be added
      *
      *        88  DELETE-ACT                      VALUE "D".
      *
      *            W15-CREDREC needs to contain the details for the 
      *                        account to be deleted
      *
      *        88  GET-ACCOUNT                     VALUE "G".
      *
      *            W15-LINCOL must have the screen location of where the
      *                       User will key in the account number. 
      *                            eg. 0617 = Line 6 Column 17.
      *
      *            W15-BGRND  The Background colour that must be used.
      *            W15-FGRND  The Foreground colour that must be used.
      *                       The folloeing values are available in the
      *                       COPY file "WS.WS" that is included in the
      *                       WORKING-STORAGE SECTION.
      *
      *                           78  Black     value 0.
      *                           78  Blue      value 1.
      *                           78  Green     value 2.
      *                           78  Cyan      value 3.
      *                           78  Red       value 4.
      *                           78  Magenta   value 5.
      *                           78  Brown     value 6.
      *                           78  Grey      value 7.
      *
      *                       If the screen colours for the calling 
      *                       program are Black on White(Grey) then
      *                       that would be setup as follows:
      *
      *                           MOVE Grey  TO W15-BGRND.
      *                           MOVE Black TO W15-FGRND.
      *
      *        88  READ-LOCK                       VALUE "L".
      *
      *            W15-ACNO must contain the Account Number of the 
      *                     account record to be read and locked.
      *
      *        88  READ-RECORD                     VALUE "R".
      *
      *            W15-ACNO must contain the Account Number of the 
      *                     account record to be read.
      *
      *        88  UPDATE-ACT                      VALUE "U".
      *
      *            W15-CREDREC needs to contain the details for the 
      *                        account record to be updated.
      *
      *        88  UPDATE-UNL                      VALUE "X".
      *
      *            W15-CREDREC must contain the the details for the  
      *                        account record to be updated and unlocked.
      *
      *    This variable informs the called program to write details of
      *    the recoosd to the RECOVERY file.
      * 
      *    03  W15-RECOVER.
      *        05  W15-RECV        PIC  X(01)      VALUE "N".
      *            88  ADD-TO-RECV                 VALUE "Y".
      * 
      *    If the record must be written to the RECOVERY file; the 
      *    calling program must MOVE "Y" TO W15-RECV before calling 
      *    CRPRDWR. CRPRDWR will always change it back to "N" before 
      *    returning control to the calling program.
      *
      *    When a read CREDITOR account has been requested WITH W15-RECV
      *    havinge a value of "N" then the program will exit with the
      *    account record in W15-CREDREC. If W15_RECV = "Y" then 
      *    W15-CREDREC remains unchanged.
      *
      *    On exiting W15-ERROR will be 0 if successful or contain and 
      *    error code for unsuccessful requests:
      *               0 = successful
      *               1 = No Record
      *               2 = account number on File not the same as requested
      *               9 = CRPRDWR called with incorrect instruction
      *
              
       COPY "CA155.CRP".

       COPY "CA000.STK".

       COPY "CA100.STK".

       COPY "CA200.STK".

       COPY "CA500.STK".

       CA600        SECTION.
       CA601.
             MOVE PRD-EXT-CODE       TO STK-CODE.

       CA610.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
           IF WS-STATUS = "23"
               MOVE "INVALID ITEM CODE ??" TO W10-DESC
           ELSE
               MOVE STK-DESC         TO W10-DESC.

       CA699.
             EXIT.
      /
      *      ( P U R C H A S E   T R A C K I N G ) Selective per Account
      *
       DA000         SECTION 5.
       DA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S10H.

       DA05.
             DISPLAY S-SUP-1.
             DISPLAY S-SUP-2.
             MOVE 0601   TO CPOS.
             PERFORM DA900-EDGE UNTIL CLIN = 22.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Allow user to key in the Creditor account number.     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      MOVE SPACES  TO W15-ACNO.

       DA10.
             DISPLAY S55.
             ACCEPT S-SUP-1A.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY GO TO DA999
                 WHEN F1-KEY  PERFORM HELP-ROUTINE
                 WHEN F2-KEY  PERFORM AA50-LOOKUP
                 WHEN OTHER   PERFORM AA900-ALARM
                              GO TO DA10
               END-EVALUATE
               DISPLAY S-SUP-1A
               IF W15-ACNO = SPACES
                  GO TO DA10.
             PERFORM CLEAR-L50.
           IF W15-ACNO = SPACES
               GO TO DA999.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W15-ACNO BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             PERFORM CA155-GET-CREDITOR.
           IF WS-ERROR NOT = ZERO
               MOVE WS-ER1           TO WS-ERR-STRING
               PERFORM ERROR-MESSAGE
               GO TO DA10.
             MOVE P-NAME             TO W15-NAME.
             DISPLAY S-SUP-1B.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO DA10.
             PERFORM DA905 THRU DA919.
           IF WS-ERROR = 9
               GO TO DA00.
             DISPLAY W95-PERIOD AT 0529 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             MOVE W15-ACNO           TO PRD-SUPP.
             MOVE W15-S-YMD          TO PRD-DTE.

       DA25.
            MOVE 6   TO CLIN.
            MOVE 1   TO W75-S.

       DA30.
      MOVE ZERO   TO W75-S1.

       DA35.
            MOVE SPACES  TO WS-OPTION PRD-EXT-CODE PRD-TRAN.
      MOVE ZERO   TO PRD-SEQ.
      MOVE W15-ACNO  TO PRD-SUPP.
      MOVE W15-S-YMD  TO PRD-DTE.
      PERFORM START-AT-PRDX-CDTE THRU READ-PURDEX-EXIT.
    IF WS-F-ERROR = 56
              GO TO DA95.

       DA40.
           IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
        GO TO DA50.
            PERFORM READ-PURDEX-NEXT THRU READ-PURDEX-EXIT.
    IF WS-F-ERROR = 56
               SUBTRACT 1        FROM W75-S
        GO TO DA55.

       DA45.
    IF NOT (PRD-SUPP = W15-ACNO)
        GO TO DA55.
    IF PRD-DTE > W15-E-YMD
        GO TO DA55.

       DA50.
      PERFORM CA600.
      MOVE W10-DESC  TO W15-DESC.
      DISPLAY PRD-EXT-CODE AT LINE CLIN COLUMN 2
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            DISPLAY W15-DESC AT LINE CLIN COLUMN 21
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      DISPLAY PRD-REF AT LINE CLIN COLUMN 42
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      COMPUTE W100-COST ROUNDED = PRD-COST / PRD-QNT.
      MOVE PRD-COST  TO W100-TCOST.
      MOVE PRD-QNT  TO W100-QNT1.
      DISPLAY W100-QNT1 AT LINE CLIN COLUMN 51
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            DISPLAY W100-COST AT LINE CLIN COLUMN 57
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      DISPLAY W100-TCOST AT LINE CLIN COLUMN 68
       WITH FOREGROUND-COLOR Brown HIGHLIGHT.
      DISPLAY PRD-TRAN AT LINE CLIN COLUMN 79
       WITH FOREGROUND-COLOR Grey HIGHLIGHT.
          IF CLIN < 21
               ADD 1             TO CLIN W75-S
              GO TO DA40.
      
       DA55.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
001020      DISPLAY " " AT 2303 WITH FOREGROUND-COLOR Brown HIGHLIGHT
             " - " WITH FOREGROUND-COLOR Grey
       "PgUp" WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     "/" WITH FOREGROUND-COLOR Grey
       "PgDn" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       " - " WITH FOREGROUND-COLOR Grey
             "Esc" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "ape." WITH FOREGROUND-COLOR Grey.

       DA60.
      PERFORM SAVE-SCREEN-2.
            MOVE 0   TO W75-S1.
             MOVE 0601           TO CPOS.

       DA65.
             MOVE 3920               TO W41-LENGTH.
             CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS W42-ATTRIB2 W41-LENGTH RETURNING WS-STATUS.
             MOVE 4000   TO W41-LENGTH.

       DA70.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM CLEAR-L50.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
             GO TO DA100
   WHEN DOWN-KEY
             GO TO DA105
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO DA70
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO DA999
   WHEN PAGE-DOWN
             GO TO DA75
   WHEN PAGE-UP
             GO TO DA85
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO DA70.
      PERFORM AA900-ALARM.
            GO TO DA70.

       DA75.
           IF W75-S < 16
              GO TO DA70.

       DA80.
             MOVE 0601           TO CPOS.
            PERFORM DA900-EDGE UNTIL CLIN = 22.
             MOVE 6              TO CLIN.
             MOVE LOW-VALUES     TO W75-KEYS.
            MOVE 1   TO W75-S.
      MOVE ZERO   TO W75-S1.
            GO TO DA40.

       DA85.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
       MOVE "J"    TO WS-OPTION.

       DA90.
      PERFORM READ-PURDEX-PREV THRU READ-PURDEX-EXIT.
    IF WS-F-ERROR = 56
        MOVE SPACE  TO WS-OPTION
        GO TO DA80.
    IF NOT (PRD-SUPP = W15-ACNO)
        PERFORM READ-PURDEX-NEXT THRU READ-PURDEX-EXIT
        GO TO DA80.
    IF WS-IXS1 < 32
        ADD 1   TO WS-IXS1
        GO TO DA90.
      GO TO DA80.

       DA95.
      PERFORM CLEAR-L50.
            DISPLAY "No record - Press " AT 5012
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "ANY" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             " key"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
            GO TO DA999.

       DA100.
           IF W75-S1 > 1
               SUBTRACT 1        FROM W75-S1 CLIN.
            GO TO DA110.

       DA105.
           IF W75-S1 = ZERO
               MOVE 1            TO W75-S1
              GO TO DA110.
           IF W75-S1 < W75-S
               ADD 1             TO W75-S1 CLIN.

       DA110.
            PERFORM DA65.
             DISPLAY W43-DET (CLIN) AT CPOS WITH REVERSE-VIDEO.
            GO TO DA70.

       DA900-EDGE.
            DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR Black
          FOREGROUND-COLOR Cyan
       "                  ³                    ³        ³
      -       "    ³          ³          ³ "
        WITH FOREGROUND-COLOR Cyan
       "³" WITH BACKGROUND-COLOR Black
         FOREGROUND-COLOR Cyan.
            ADD 1   TO CLIN.

       DA905.
      PERFORM SAVE-SCREEN-3.
      MOVE ZERO   TO WS-ERROR.
      *
      *
      *
      *   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *   ³    Transactions for Period.  ³
      *   ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´°°
      *   ³   Starting Date :   ³°°
      *   ³   Ending Date   :   ³°°
      *   ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´°°
      *   ³Dates to be keyed in as DDMMCCYY³°°
      *   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°
      *     °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
      *
      MOVE 9   TO SHADE-ROW.
      MOVE 26   TO SHADE-COL.
      MOVE 32   TO SHADE-WIDTH.
      MOVE 7   TO SHADE-LINES.
      DISPLAY S02D.
      PERFORM SCREEN-SHADOW.
      MOVE W12-TODAY  TO W15-START W15-END.

       DA910.
      DISPLAY "Press " AT 5052
        WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan
       "Esc" WITH FOREGROUND-COLOR Brown HIGHLIGHT
    BACKGROUND-COLOR Cyan
       "ape to Exit"
        WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan.
      ACCEPT S02A.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE 9  TO WS-ERROR
       GO TO DA915
   WHEN OTHER  PERFORM AA900-ALARM
       GO TO DA910
        END-EVALUATE.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO DA910.
      PERFORM CLEAR-L50.
      MOVE W15-START  TO W10-EDTE W95-SDTE.
             PERFORM CHECK-DATE.
           IF WS-ERROR = 1
              MOVE "Start date" TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO DA910.
             MOVE W20-DTE        TO W15-S-YMD.
      MOVE W15-END  TO W10-EDTE W95-EDTE.
             PERFORM CHECK-DATE.
           IF WS-ERROR = 1
              MOVE "End date"  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO DA910.
             MOVE W20-DTE        TO W15-E-YMD.
    IF W15-E-YMD < W15-S-YMD
        MOVE "End date is before start date"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO DA910.

       DA915.
      PERFORM RESTORE-SCREEN-3.

       DA919.
      EXIT.

       DA999.
      EXIT.
      /
      *      ( P U R C H A S E  T R A C K I N G ) Selective per Invoice
      *
       EA000         SECTION 5.
       EA00.
            PERFORM ERASE-SCREEN.
      DISPLAY S12H.

       EA05.
      DISPLAY S-REF-1.
      DISPLAY S-REF-2.
            MOVE 0601   TO CPOS.
            PERFORM EA900-EDGE UNTIL CLIN = 22.
      MOVE SPACES  TO W15-ACNO W15-INVN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Allow user to key in the Creditor account number.     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       EA10.
      DISPLAY S55.
      ACCEPT S-REF-1A.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO EA999
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
             PERFORM AA50-LOOKUP
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO EA10
        END-EVALUATE
        DISPLAY S-REF-1A
               IF W15-ACNO = SPACES
           GO TO EA10.
      PERFORM CLEAR-L50.
          IF W15-ACNO = SPACES
              GO TO EA999.
      MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W15-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
            PERFORM CA155-GET-CREDITOR.
           IF WS-ERROR NOT = ZERO
              MOVE WS-ER1  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
              GO TO EA10.
            MOVE P-NAME  TO W15-NAME.
      DISPLAY S-REF-1B.
            PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
              GO TO EA10.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Allow user to key in the Document reference number.    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       EA15.
      ACCEPT S-REF-1C.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO EA10
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO EA10
        END-EVALUATE
        IF W15-INVN = SPACES
           GO TO EA15.
      MOVE 8   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W15-INVN
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
      DISPLAY S-REF-1C.
      MOVE W15-ACNO  TO PRD-SUPP.
      MOVE W15-INVN  TO PRD-REF.
      MOVE ZERO   TO PRD-DTE PRD-SEQ.

       EA20.
      PERFORM START-AT-PRDX-SREF THRU READ-PURDEX-EXIT.
    IF WS-F-ERROR = 56
              GO TO EA95.
            PERFORM READ-PURDEX-NEXT THRU READ-PURDEX-EXIT.
    IF NOT (PRD-SUPP = W15-ACNO)
        GO TO EA95.
    IF (PRD-REF > W15-INVN)
        GO TO EA95.
      MOVE PRD-DTE  TO W22-DTE2.
            MOVE W22-CC2  TO W22-CC3.
            MOVE W22-YY2  TO W22-YY3.
            MOVE W22-MM2  TO W22-MM3.
            MOVE W22-DD2  TO W22-DD3.
      MOVE W22-DTE3  TO W15-INVD.
      DISPLAY S-REF-1D.

       EA25.
            MOVE 6   TO CLIN.
            MOVE 1   TO W75-S.

       EA30.
      MOVE ZERO   TO W75-S1.

       EA35.
      MOVE SPACES  TO WS-OPTION.
      PERFORM START-AT-PRDX-SREF THRU READ-PURDEX-EXIT.
    IF WS-F-ERROR = 56
              GO TO EA95.

       EA40.
           IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
        GO TO EA50.
            PERFORM READ-PURDEX-NEXT THRU READ-PURDEX-EXIT.
    IF WS-F-ERROR = 56
               SUBTRACT 1        FROM W75-S
        GO TO EA55.

       EA45.
    IF NOT (PRD-SUPP = W15-ACNO)
        GO TO EA55.
    IF (PRD-REF > W15-INVN)
        GO TO EA55.

       EA50.
      PERFORM CA600.
      DISPLAY PRD-EXT-CODE AT LINE CLIN COLUMN 2
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            DISPLAY W10-SDSC AT LINE CLIN COLUMN 21
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      COMPUTE W100-COST ROUNDED = PRD-COST / PRD-QNT.
      MOVE PRD-COST  TO W100-TCOST.
      MOVE PRD-QNT  TO W100-QNT.
      DISPLAY W100-QNT AT LINE CLIN COLUMN 50
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            DISPLAY W100-COST AT LINE CLIN COLUMN 59
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      DISPLAY W100-TCOST AT LINE CLIN COLUMN 70
       WITH FOREGROUND-COLOR Brown HIGHLIGHT.
          IF CLIN < 21
               ADD 1             TO CLIN W75-S
              GO TO EA40.
      
       EA55.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
001020      DISPLAY " " AT 2303 WITH FOREGROUND-COLOR Brown HIGHLIGHT
             " - " WITH FOREGROUND-COLOR Grey
       "PgUp" WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     "/" WITH FOREGROUND-COLOR Grey
       "PgDn" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       " - " WITH FOREGROUND-COLOR Grey
             "Esc" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "ape." WITH FOREGROUND-COLOR Grey.

       EA60.
      PERFORM SAVE-SCREEN-2.
            MOVE 0   TO W75-S1.
             MOVE 0601           TO CPOS.

       EA65.
             MOVE 3920               TO W41-LENGTH.
             CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS W42-ATTRIB2 W41-LENGTH RETURNING WS-STATUS.
             MOVE 4000               TO W41-LENGTH.

       EA70.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM CLEAR-L50.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
             GO TO EA100
   WHEN DOWN-KEY
             GO TO EA105
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO EA70
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO EA999
   WHEN PAGE-DOWN
             GO TO EA75
   WHEN PAGE-UP
             GO TO EA85
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO EA70.
      PERFORM AA900-ALARM.
            GO TO EA70.

       EA75.
           IF W75-S < 16
              GO TO EA70.

       EA80.
             MOVE 0601           TO CPOS.
            PERFORM EA900-EDGE UNTIL CLIN = 22.
             MOVE 6              TO CLIN.
             MOVE LOW-VALUES     TO W75-KEYS.
             MOVE 1              TO W75-S W75-S1.
      PERFORM EA30.
            GO TO EA40.

       EA85.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
       MOVE "J"    TO WS-OPTION.

       EA90.
      PERFORM READ-PURDEX-PREV THRU READ-PURDEX-EXIT.
    IF WS-F-ERROR = 56
        MOVE SPACE  TO WS-OPTION
        GO TO EA80.
    IF NOT (PRD-SUPP = W15-ACNO)
        PERFORM READ-PURDEX-NEXT THRU READ-PURDEX-EXIT
        GO TO EA80.
    IF NOT (PRD-REF = W15-INVN)
        PERFORM READ-PURDEX-NEXT THRU READ-PURDEX-EXIT
        GO TO EA80.
    IF WS-IXS1 < 32
        ADD 1   TO WS-IXS1
        GO TO EA90.
      GO TO EA80.

       EA95.
      PERFORM CLEAR-L50.
            DISPLAY "No record - Press " AT 5012
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "ANY" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             " key"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
            GO TO EA999.

       EA100.
           IF W75-S1 > 1
               SUBTRACT 1        FROM W75-S1 CLIN.
            GO TO EA110.

       EA105.
           IF W75-S1 = ZERO
               MOVE 1            TO W75-S1
              GO TO EA110.
           IF W75-S1 < W75-S
               ADD 1             TO W75-S1 CLIN.

       EA110.
            PERFORM EA65.
             DISPLAY W43-DET (CLIN) AT CPOS WITH REVERSE-VIDEO.
            GO TO EA70.

       EA900-EDGE.
            DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR Black
          FOREGROUND-COLOR Cyan
       "                  ³                            ³
      -       "      ³          ³          "
        WITH FOREGROUND-COLOR Cyan
       "³" WITH BACKGROUND-COLOR Black
         FOREGROUND-COLOR Cyan.
            ADD 1   TO CLIN.

       EA999.
      EXIT.
      /
      *      ( P U R C H A S E  T R A C K I N G ) Selective for Period
      *
       FA000         SECTION 5.
       FA00.
             PERFORM ERASE-SCREEN.
      DISPLAY S14H.

       FA05.
      *
      *    PURCHASE TRACKING
      *
      *   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *   ³    Transactions for Period.  ³
      *   ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´°°
      *   ³   Starting Date :   ³°°
      *   ³   Ending Date   :   ³°°
      *   ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´°°
      *   ³Dates to be keyed in as DDMMCCYY³°°
      *   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°
      *     °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
      *
      MOVE 5   TO SHADE-ROW.
      MOVE 26   TO SHADE-COL.
      MOVE 32   TO SHADE-WIDTH.
      MOVE 7   TO SHADE-LINES.
      DISPLAY S05D.
      PERFORM SCREEN-SHADOW.
      MOVE W12-TODAY  TO W15-START W15-END.

       FA10.
      DISPLAY "Press " AT 5052
        WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan
       "Esc" WITH FOREGROUND-COLOR Brown HIGHLIGHT
    BACKGROUND-COLOR Cyan
       "ape to Exit"
        WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan.
      ACCEPT S05A.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO FA999
   WHEN OTHER  PERFORM AA900-ALARM
       GO TO FA10
        END-EVALUATE.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO FA10.
      PERFORM CLEAR-L50.
      MOVE W15-START  TO W10-EDTE.
             PERFORM CHECK-DATE.
           IF WS-ERROR = 1
              MOVE "Start date" TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO FA10.
             MOVE W20-DTE        TO W15-S-YMD.
      MOVE W15-END  TO W10-EDTE.
             PERFORM CHECK-DATE.
           IF WS-ERROR = 1
              MOVE "End date"  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO FA10.
             MOVE W20-DTE        TO W15-E-YMD.
    IF W15-E-YMD < W15-S-YMD
        MOVE "End date is before start date"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO FA10.

       FA15.
            DISPLAY S-MOV.
      INITIALIZE PRD-REC1.
      MOVE SPACES  TO PRD-EXT-CODE.
      MOVE W15-S-YMD  TO PRD-DTE.
      MOVE ZERO   TO PRD-SEQ.

       FA20.
             MOVE 0601           TO CPOS.
            PERFORM FA900-EDGE UNTIL CLIN = 22.

       FA25.
            MOVE 6   TO CLIN.
            MOVE 1   TO W75-S.

       FA30.
      MOVE ZERO   TO W75-S1.

       FA35.
      MOVE SPACES  TO WS-OPTION.
            PERFORM START-AT-PRDX-DATE THRU READ-PURDEX-EXIT.
          IF WS-F-ERROR = 56
        GO TO FA95.

       FA40.
           IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
        GO TO FA50.
            PERFORM READ-PURDEX-NEXT THRU READ-PURDEX-EXIT.
    IF WS-F-ERROR = 56
               SUBTRACT 1        FROM W75-S
        GO TO FA55.

       FA45.
    IF (PRD-DTE > W15-E-YMD)
        GO TO FA55.

       FA50.
            DISPLAY PRD-SUPP AT LINE CLIN COLUMN 2
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            DISPLAY PRD-REF AT LINE CLIN COLUMN 9
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      MOVE PRD-DTE  TO W22-DTE2.
      MOVE W22-CC2  TO W22-CC3.
      MOVE W22-YY2  TO W22-YY3.
      MOVE W22-MM2  TO W22-MM3.
      MOVE W22-DD2  TO W22-DD3.
      MOVE W22-DTE3  TO W95-DTE.
            DISPLAY W95-DTE AT LINE CLIN COLUMN 18
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      DISPLAY PRD-EXT-CODE AT LINE CLIN COLUMN 29
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      MOVE PRD-COST  TO W100-TCOST.
      COMPUTE W100-COST ROUNDED = PRD-COST / PRD-QNT.
      MOVE PRD-QNT  TO W100-QNT.
      DISPLAY W100-QNT AT LINE CLIN COLUMN 48
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            DISPLAY W100-COST AT LINE CLIN COLUMN 57
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      DISPLAY W100-TCOST AT LINE CLIN COLUMN 68
       WITH FOREGROUND-COLOR Brown HIGHLIGHT.
      DISPLAY PRD-TRAN AT LINE CLIN COLUMN 79
       WITH FOREGROUND-COLOR Brown HIGHLIGHT.
           IF CLIN < 21
               ADD 1             TO CLIN W75-S
              GO TO FA40.
      
       FA55.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
001020      DISPLAY " " AT 2303 WITH FOREGROUND-COLOR Brown HIGHLIGHT
             " - " WITH FOREGROUND-COLOR Grey
       "PgUp" WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     "/" WITH FOREGROUND-COLOR Grey
       "PgDn" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       " - " WITH FOREGROUND-COLOR Grey
             "Esc" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "ape." WITH FOREGROUND-COLOR Grey.

       FA60.
      PERFORM SAVE-SCREEN-2.
            MOVE 0   TO W75-S1.
             MOVE 0601           TO CPOS.

       FA65.
             MOVE 3920               TO W41-LENGTH.
             CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS W42-ATTRIB2 W41-LENGTH RETURNING WS-STATUS.
             MOVE 4000               TO W41-LENGTH.

       FA70.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM CLEAR-L50.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
             GO TO FA100
   WHEN DOWN-KEY
             GO TO FA105
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO FA70
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO FA999
   WHEN PAGE-DOWN
             GO TO FA75
   WHEN PAGE-UP
             GO TO FA85
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO FA70.
      PERFORM AA900-ALARM.
            GO TO FA70.

       FA75.
           IF W75-S < 16
              GO TO FA70.

       FA80.
             MOVE 0601           TO CPOS.
            PERFORM FA900-EDGE UNTIL CLIN = 22.
             MOVE 6              TO CLIN.
             MOVE LOW-VALUES     TO W75-KEYS.
            MOVE 1   TO W75-S.
      MOVE ZERO   TO W75-S1.
            GO TO FA40.

       FA85.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
       MOVE "J"    TO WS-OPTION.

       FA90.
      PERFORM READ-PURDEX-PREV THRU READ-PURDEX-EXIT.
    IF WS-F-ERROR = 56
        MOVE SPACE  TO WS-OPTION
        GO TO FA80.
    IF NOT (PRD-EXT-CODE = W10-EXT-ITEM)
        PERFORM READ-PURDEX-NEXT THRU READ-PURDEX-EXIT
        GO TO FA80.
    IF PRD-DTE < W15-START
        PERFORM READ-PURDEX-NEXT THRU READ-PURDEX-EXIT
        GO TO FA80.
    IF WS-IXS1 < 32
        ADD 1   TO WS-IXS1
        GO TO FA90.
      GO TO FA80.

       FA95.
      PERFORM CLEAR-L50.
            DISPLAY "No record - Press " AT 5012
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "ANY" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             " key"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
            GO TO FA999.

       FA100.
           IF W75-S1 > 1
               SUBTRACT 1        FROM W75-S1 CLIN.
            GO TO FA110.

       FA105.
           IF W75-S1 = ZERO
               MOVE 1            TO W75-S1
              GO TO FA110.
           IF W75-S1 < W75-S
               ADD 1             TO W75-S1 CLIN.

       FA110.
            PERFORM FA65.
             DISPLAY W43-DET (CLIN) AT CPOS WITH REVERSE-VIDEO.
            GO TO FA70.

       FA900-EDGE.
            DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR Black
          FOREGROUND-COLOR Cyan
       "      ³        ³          ³                  ³
      -       "    ³          ³          ³ "
        WITH FOREGROUND-COLOR Cyan
       "³" WITH BACKGROUND-COLOR Black
         FOREGROUND-COLOR Cyan.
            ADD 1   TO CLIN.

       FA999.
             EXIT.

      *    *************************************************************
      *                 I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT    SECTION 8.
       ZA000-START.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W12-TODAY.
             MOVE LS-TODAY-YYMMDD    TO W12-T-YMD.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.PRO".

             MOVE 3                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF NOT(PAR-STOCK = "A" OR "N")
               MOVE "A"              TO PAR-STOCK.
               MOVE PAR-STOCK        TO W10-STCK.
           IF PAR-STOCK = "N"
               MOVE PAR-SLNGTH       TO W10-SLNGTH
           ELSE
               MOVE 14               TO W10-SLNGTH.
           IF W10-SLNGTH < 3
               MOVE 3                TO W10-SLNGTH
           ELSE
           IF W10-SLNGTH > 14
               MOVE 14               TO W10-SLNGTH.
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
       ZA02.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY    TO W95-COMP.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
      MOVE 1   TO W05-V-RATE.

       ZA05-VAT.
      MOVE W05-VAT-CODE  TO DPT-CODE.
      PERFORM READ-DEPART THRU READ-DEPART-EXIT.
      MOVE DPT-R-DATE  TO WS-VAT-DATE(W05-V-RATE).
      MOVE DPT-RATE  TO W05-VAT(W05-V-RATE).
      ADD 6 W05-V-RATE  GIVING WS-S1.
      MOVE DPT-P-RATE  TO W05-VAT(WS-S1).
    IF W05-V-RATE < 6
        ADD 1   TO W05-V-RATE
        GO TO ZA05-VAT.
            GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
            GO TO ZA205.
       ZA50.
          IF WS-F-ERROR = 56
              MOVE "CARDEX file"
     TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Press any key to continue" AT 1012.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
       ZA51.
             EXIT PROGRAM.

       ZA55.
             MOVE 1              TO WS-S1.
       ZA60.
             MOVE WS-G1          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8          TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
           IF WS-S1 < 80
               ADD 1             TO WS-S1
               GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G2          TO WS-TCHR (8)  WS-TCHR (49)
                                    WS-TCHR (61) WS-TCHR (72).
            MOVE WS-G3   TO WS-MCR  (8)  WS-MCR (50)
                                    WS-MCR  (66) WS-MCR (1)
        WS-MCR  (80).
            MOVE WS-G4   TO WS-BCHR (8)  WS-BCHR (49)
                                    WS-BCHR (61) WS-BCHR (72).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
      MOVE LS-COMPANY  TO WS-TOP-COMP.
    IF LS-USER = LS-SYS-ID
        MOVE "SupervisorÄ"  TO WS-WRKHD
    ELSE
        MOVE "Workstation"  TO WS-WRKHD
        MOVE LS-USER    TO WS-WRKST.
       ZA200.
             DISPLAY "Files locked - Try later" AT 2312
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     " " WS-STATUS WITH FOREGROUND-COLOR Grey HIGHLIGHT.
            DISPLAY "Press any key" AT 5012
              WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT WS-OPTION AT 5026
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
       ZA205.
             EXIT PROGRAM.
       ZA999-EXIT.
             EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *
       ZB000-ERROR      SECTION 8.

       OPEN-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             GO TO DISPLAY-FILE-NAME.

       READ-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             GO TO DISPLAY-FILE-NAME.

       WRITE-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             GO TO DISPLAY-FILE-NAME.
       I-O-ERROR-EXIT.
             EXIT.

       DISPLAY-FILE-NAME.
           IF WS-F-ERROR = 2
               MOVE W02-NETWORK TO WS-FILE
              MOVE WS-NETKEY TO WS-KEY
          ELSE
           IF WS-F-ERROR = 3
               MOVE W02-CREDIT TO WS-FILE
              MOVE ZERO  TO WS-KEY
        MOVE P-NUMBER  TO WS-KEYX
          ELSE
           IF WS-F-ERROR = 7
              MOVE W02-DEPART  TO WS-FILE
              MOVE ZERO  TO WS-KEY
              MOVE DPT-CODE  TO WS-KEYX
          ELSE
           IF WS-F-ERROR = 15
               MOVE WS-PARID TO WS-FILE
               MOVE WS-PARKEY TO WS-KEY
          ELSE
           IF WS-F-ERROR = 22
               MOVE W02-STOCKF   TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE STK-CODE  TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 24
              MOVE W02-CRDMEM  TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE CME-KEY  TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 56
              MOVE W02-PURDEX  TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE PRD-KEY  TO WS-KEYX.
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
