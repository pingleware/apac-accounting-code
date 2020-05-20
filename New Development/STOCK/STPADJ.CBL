      $SET LINKCOUNT"512" GNT"STPADJ.GNT"
      *
      *       ONLY DELETE WHEN ALL BELOW IS COMPLETED  
      *     ===========================================
      *
      *  Screen co-ordinates must be revised for new layout
      *
      *  Screen Section                            Completed  
      *                                               ___
      *                    Layout screen co-ordinates|   |
      *                                               ---
      *                                               ___
      *                    Input screen co-ordinates |   |
      *                                               ---
      *                                               ___
      *                    Output screen co-ordinates|   |
      *                                               ---
      *  Procedure Division 
      *                                               ___
      *                    Screen Heading Format     |   |
      *                                               ---
      *                                               ___
      *                    Error Messages            |   |
      *                                               ---
      *                                               ___
      *                    User information messages |   |
      *                                               ---
      *                                               ___
      *                    Accept User options       |   |
      *                                               ---
      *                                               ___
      *                    Display co-ordinates      |   |
      *                                               ---
      *                                               ___
      *                    Accept co-ordinates       |   |
      *                                               ---
      *  Accept and Display of Stk-code allows
      *                                               ___
      *         Display ITM, EXT and 14 Characters   |   |
      *                                               ---
      *                                               ___
      *         Accept ITM, EXT, 14 Characters and   |   |
      *            all the Numeric Formats 3 to 14    ---
      *
      *  Stock file
      *                                               ___       
      *         Use the new layout and W10.STK       |   |
      *                                               ---
      ******************************************************************
      *                                                                *
      *    ******   ********  *******     ****    *******   ********   *
      *   **    **     **     **    **   **  **   **    **     **      *
      *   **           **     **    **  **    **  **    **     **      *
      *    ******      **     *******   ********  **    **     **      *
      *         **     **     **        **    **  **    **     **      *
      *   **    **     **     **        **    **  **    **  ** **      *
      *    ******      **     **        **    **  *******    ***       *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     A D J U S T M E N T   T R A C K I N G   P R O G R A M      *
      *                                                                *
      *       Version 9.04.03 - September 2017                         *

      *                                                                *
      ******************************************************************
      *                                                                *
      * August 2008   - New file (STKALT) - Stock alternate index      *
      *                 included for lookups, using any word con-      *
      *                 tained in the Stock description.               *
      *                                                                *
      * November 2009 - Include words from Description 2 and from      *
      *                 the Item code (Some item codes are comprised   *
      *                 of individual words and these will be          *
      *                 included in the alternate Index)               *
      *                                                                *
      * Jan 2010  - Allow for two discounts for a Debtor on the same   *
      *             Sales Ledger code. Included two additional Debtor  *
      *             discount codes (11 and 12) each with a field to    *
      *             define which debtor discount code uses this as an  *
      *             alternate discount. The relevant Stock Item will   *
      *             have an indicator field to instruct the system to  *
      *             use the alternate discount. The alternate discount *
      *             is only applicable if the discount code for which  *
      *             this alternate discount is applicable is the code  *
      *             on the Debtor account record.                      *
      *                                                                *
      * Feb 2017 - Increased the number of Memo entries, of 60 bytes,  *
      *            allowed from 12 to 34 per stock record.             *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     STPADJ.
       AUTHOR.         J.W.LEMMON.
       DATE-WRITTEN.        APRIL 1999.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1999 - 2018
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

       COPY "PARAM.SL".

       COPY "STKDEX.SL".

       COPY "STOCK.SL".

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

       COPY "DEPART.FDE".

       COPY "PARAM.FDE".

       COPY "STKDEX.FDE".

       COPY "STOCK.FDE".

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
       77  WS-SUB    PIC 9(04)  COMP-5.
       77  WS-S            PIC  9(04)    COMP-5.
       77  WS-S1           PIC  9(04)    COMP-5.
       77  WS-S2           PIC  9(04)    COMP-5.
       77  WS-S3           PIC  9(04)    COMP-5.
       77  WS-S4           PIC  9(04)    COMP-5.
       77  WS-S5           PIC  9(04)    COMP-5.
       77  WS-S6           PIC  9(04)    COMP-5.
       77  WS-S7           PIC  9(04)    COMP-5.
       77  WS-S8           PIC  9(04)    COMP-5.
       77  WS-S9           PIC  9(04)    COMP-5.
       77  WS-IXS1         PIC  9(04)    COMP-5.
       77  WS-PAGE    PIC 9(04)  COMP-5.
       77  WS-ENQPOS    PIC 9(06)  COMP-5.
       77  WS-ENQEND       PIC  9(06)    COMP-5.
       77  WS-NETKEY    PIC 9(06)  COMP-5.
       77  WS-PARKEY       PIC  9(06)    COMP-5.
       77  WS-KEYSTR       PIC  9(06)    COMP-5.
       77  WS-OPTION       PIC  X(01).
       77  WS-USE-ITM    PIC X(01).
       77  WS-EXT-STK    PIC X(01).
       77  WS-ENQ    PIC X(01).
       77  WS-REP    PIC X(01).
       77  WS-MONTH    PIC 9(02).
       77  WS-ETYPE    PIC X(01).
       77  WS-REFERENCE    PIC X(08).
       77  WS-DESC-H1    PIC X(14) VALUE "Description".
       77  WS-DESC-H2    PIC X(14) VALUE "Description 2".
       77  WS-RT-HD    PIC X(10) VALUE "-Retail   ".
       77  WS-WS-HD    PIC X(10) VALUE "-Wholesale".
       77  WS-CS-HD    PIC X(10) VALUE "-Cash sale".
       77  WS-ERROR    PIC 9(01) VALUE ZERO.
       77  WS-ERR2         PIC  X(09) VALUE "No Record".
       77  PRG-NAME    PIC X(12) VALUE "STP\STPLOOK".
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.
       77  WS-QTOT    PIC S9(07)V99 COMP-3.
       77  WS-VTOT    PIC S9(09)V99 COMP-3.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *       C = Clear comment line (50)
      *       E = Clear lines (any line or lines between 5 and 46)
      *       H = Change heading
      *       S = Clear the screen and display headings
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STPADJ".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY -ADJUSTMENT TRACKING".
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
           03  WS-MODULE           PIC  X(03) VALUE "STP".
           03  WS-PROG             PIC  X(03) VALUE "ADJ".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-PRINTER-DETAILS.
           03  W02-PRINTER         PIC  X(12).
           03  W02-PGE-LENGTH      PIC  9(03).
           03  W02-PRN-LENGTH      PIC  9(03).
           03  W02-LINAGE          PIC  9(03).
           03  W02-PRN-STATUS      PIC  X(01)           VALUE "C".
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

    03  R-SL1 REDEFINES W02-PRN-LINE.
        05  R-SLA       PIC  X(78).
        05  FILLER      PIC  X(58).
    03  R-SL2 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(58).
        05  R-SLB       PIC  X(78).
          03  REP-LINE1 REDEFINES W02-PRN-LINE.
              05  REP-DETAIL1 PIC  X(140).
          03  REP-LINE2 REDEFINES W02-PRN-LINE.
              05  REP-DETAIL2 PIC  X(80).
          03  REP-LINE3 REDEFINES W02-PRN-LINE.
              05  REP-DATE    PIC  X(06).
              05  REP-DTE     PIC  Z9/99/9999.
              05  FILLER      PIC  X(03).
        05  REP-PRC-HD.
     07  FILLER  PIC  X(01).
           07  REP-COMPANY  PIC X(40).
     07  FILLER  PIC  X(02).
              05  FILLER      PIC  X(08).
              05  REP-PAGE    PIC  X(06).
              05  REP-P-NO    PIC  Z(03)9.
          03  REP-LINE4 REDEFINES W02-PRN-LINE.
        05  REP-HEAD    PIC  X(96).
    03  REP-LINE5 REDEFINES W02-PRN-LINE.
        05  REP-ITEM    PIC  X(18).
        05  FILLER      PIC  X(01).
        05  REP-DESC    PIC  X(30).
        05  FILLER      PIC  X(01).
        05  REP-TOT-LINE.
   07  REP-QUANT PIC  Z(06)9.99-.
   07  FILLER    PIC  X(01).
   07  REP-COST  PIC  Z(08)9.99-.
        05  FILLER      PIC  X(01).
        05  REP-REMK    PIC  X(20).

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
       COPY "DateVariables.cpy".

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

       COPY "W25.STK".

       01  W35-HEADINGS.
    02  W35-PERIOD.
    03  FILLER  PIC  X(35)
    VALUE "Stock Adjustments for the period:- ".
    03  W35-START PIC  Z9/99/9999.
    03  FILLER  PIC  X(04) VALUE " to ".
    03  W35-END  PIC  Z9/99/9999.
    03  W35-REP  PIC  X(18).
    02  W35-REFERENCE.
    03  FILLER  PIC  X(31)
    VALUE "Stock Adjustments - Reference: ".
    03  W35-REF  PIC  X(08).
    02  W35-HEADING.
    03  FILLER  PIC  X(86)
        VALUE "Item Code          Stock Description
      -       "  Quantity         Value  Remarks".

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

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
    03  W75-DATE       PIC 9(08) OCCURS 16.

       01  W75-STORE.
          03  W75-SNAME      PIC  X(08).
    03  W75-SREF       PIC  X(08).
    03  W75-SACN       PIC  X(06).
    03  W75-SDTE       PIC  9(06).

       01  W95-STM.
           03  W95-COMP        PIC X(40).
           03  W95-V1.
               05  W95-T       PIC Z(04)9.99.
           03  W95-V2.
               05  W95-B       PIC Z(06)9.99-.
           03  W95-D.
              05  W95-DTE     PIC  Z9/99/9999.
          03  W95-POST        PIC  9(04).
          03  W95-TEL        PIC  X(08).

       01  W100-EDIT.
          03  W100-COST       PIC  Z(06)9.99-.
    03  W100-QNT        PIC  Z(06)9.99-.
    03  W100-USER.
        05  W100-USE    PIC  ZZ9.

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

       01  S01.
    03  LINE  2 COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT VALUE
               "ADJUSTMENT TRACKING".

       01  S02.
    03  LINE  4 COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE
    "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
    03        COLUMN 50 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "¿".
    03  LINE  5 COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE
    "³   LOOKUP OPTIONS   ".
    03        COLUMN 50 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE  6 COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE "³ 1 ".
    03        COLUMN 33 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE
    "Specified Period ³".
    03  LINE  7 COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE "³ 2 ".
    03        COLUMN 33 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE
    "Individual Ref.  ³".
    03  LINE  8 COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE "³ 0 ".
    03        COLUMN 33 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE
    "Exit           ³".
    03  LINE  9 COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE "À".
    03        COLUMN 30 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE
     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S03.
    03  LINE  8 COLUMN 44 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta PIC X(08)
     USING WS-REFERENCE AUTO.

       01  S05D.
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

       01  S10D.
          03  LINE  2 COLUMN 25 FOREGROUND-COLOR Grey HIGHLIGHT VALUE
               "STOCK ADJUSTMENT TRACKING LOOKUP".
    03  LINE  3 COLUMN  3 VALUE "Item Code.".

       01  S36.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC X(14) USING W10-ITEM AUTO.

       01  S36-ITM.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
            PIC X(14) USING W10-ITEM AUTO.
    03  COLUMN 16 FOREGROUND-COLOR Black BACKGROUND-COLOR Magenta
            PIC X(03) USING W10-ITM AUTO.

       01  S36-EXT.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
            PIC X(18) USING W10-EXT-ITEM AUTO.

       01  S36-3.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(03) USING W10-I4 AUTO.

       01  S36-4.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(04) USING W10-I4 AUTO.

       01  S36-5.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(05) USING W10-I5 AUTO.

       01  S36-6.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(06) USING W10-I6 AUTO.

       01  S36-7.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(07) USING W10-I7 AUTO.

       01  S36-8.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(08) USING W10-I8 AUTO.

       01  S36-9.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(09) USING W10-I9 AUTO.

       01  S36-10.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(10) USING W10-I10 AUTO.

       01  S36-11.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(11) USING W10-I11 AUTO.

       01  S36-12.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(12) USING W10-I12 AUTO.

       01  S36-13.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(13) USING W10-I13 AUTO.

       01  S36-14.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(14) USING W10-I14 AUTO.

       01  S-MOV.
           03  LINE  4 COLUMN  2 REVERSE-VIDEO VALUE
              "   Date    User  W/S  Type    Cost Price     Quantity       Remarks        ".
          03  LINE  5 COLUMN  1 BACKGROUND-COLOR Black VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÑÍÍÍÑÍÍÍÍÍÍÑÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR Black VALUE "µ".
          03  LINE 22 COLUMN  1 BACKGROUND-COLOR Black VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÏÍÍÍÏÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR Black VALUE "µ".

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
      *
      *   **** Check the User's security level for Stock/Inventory
      *
          IF LS0-STLEV < 3
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
        GO TO AA49.
            PERFORM ZA000-INIT.
      DISPLAY S01.
      *    *************************************************************
      *    ****       C H O O S E   L O O K U P   O P T I O N     ****
      *    *************************************************************
      MOVE 5   TO SHADE-ROW.
      MOVE 31   TO SHADE-COL.
      MOVE 20   TO SHADE-WIDTH.
      MOVE 5   TO SHADE-LINES.
      DISPLAY S02.
      DISPLAY MENU-INSTRUCT.
      PERFORM SCREEN-SHADOW.
      MOVE 5   TO TOP-ROW.
      MOVE 7   TO BOTTOM-ROW.
      MOVE 4   TO SCREEN-LIN.
      MOVE 31   TO SCREEN-COL.
      MOVE 18   TO STRING-LENGTH.
      PERFORM SAVE-ATTR.

       AA05.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       AA10.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY  PERFORM MOVE-BLOCK-UP
   WHEN DOWN-KEY  PERFORM MOVE-BLOCK-DOWN
   WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
     IF MOUSE-ENTER = "Y"
         MOVE "N"  TO MOUSE-ENTER
         GO TO AA15
     END-IF
     IF MOUSE-Y = 24
         IF MOUSE-X = 5
      MOVE 5  TO KEY-CODE-1
      GO TO AA10
         ELSE
         IF MOUSE-X = 10
      MOVE 6  TO KEY-CODE-1
      GO TO AA10
         ELSE
         IF MOUSE-X > 36 AND < 44
      MOVE 0  TO KEY-CODE-1
      GO TO AA10
         ELSE
         IF MOUSE-X > 54 AND < 60
      MOVE 0  TO KEY-CODE-1
      MOVE 1  TO KEY-TYPE
      GO TO AA10
         END-IF
     END-IF
   WHEN ENTER-KEY  GO TO AA15
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO AA05
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE "0"  TO WS-OPTION
       GO TO AA18
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO AA05
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION
        IF WS-OPTION < 0 OR > 2
     PERFORM AA900-ALARM
     GO TO AA05
        END-IF
        GO TO AA16
    ELSE
        PERFORM AA900-ALARM
        GO TO AA05.

       AA15.
    IF SCREEN-LIN < 5
        PERFORM AA900-ALARM
        GO TO AA05
    ELSE
    IF SCREEN-LIN = 5
        MOVE "1"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 6
        MOVE "2"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 7
        MOVE "0"   TO WS-OPTION.
    GO TO AA18.

       AA16.
      PERFORM CLEAR-BLOCK.
    IF WS-OPTION = 0
        MOVE 7   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = 1
        MOVE 5   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = 2
        MOVE 6   TO SCREEN-LIN.
      PERFORM SAVE-ATTR.
      PERFORM MARK-BLOCK.

       AA18.
    IF WS-OPTION = "0"
        GO TO AA49.
      PERFORM SAVE-SCREEN-2.
      MOVE BLOCK-DETAIL  TO BLOCK-DETAIL2.
      MOVE SHADOW-DETAIL  TO SHADOW-DETAIL2.
      MOVE CRT-DETAIL  TO CRT-DETAIL2.
    IF WS-OPTION = "1"
        MOVE "D"   TO WS-REP
        PERFORM BA000
    ELSE
    IF WS-OPTION = "2"
        MOVE "R"   TO WS-REP
        PERFORM BB000.
      PERFORM RESTORE-SCREEN-2.
      MOVE BLOCK-DETAIL2  TO BLOCK-DETAIL.
      MOVE SHADOW-DETAIL2 TO SHADOW-DETAIL.
      MOVE CRT-DETAIL2  TO CRT-DETAIL.
      GO TO AA05.

       AA49.
             EXIT PROGRAM.

       COPY "AA50.LUP".

       COPY "AA900.ALM".

      *    
      *            ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *           S C R E E N ,   K E Y B O A R D   &   M O U S E
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
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *            T H I S   R O U T I N E   I S   U S E D   T O
      *       D I S P L A Y   I N F O R M A T I O N   M E S S A G E S
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Move the message to WS-DSP-MES and the top line of the   ³
      *    ³  message box to WS-MES-LINE. If WS-MES-LINE is zero the   ³
      *    ³  default will be 20.                                      ³
      *    ³                  PERFORM DISPLAY-MESSAGE                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *           T H I S   R O U T I N E   I S   U S E D   T O
      *            D I S P L A Y   E R R O R   M E S S A G E S
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                      ERROR-MESSAGE                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To display the error message higher or lower (default is  ³
      *    ³ line 20) Move the line number which must be used as the   ³
      *    ³ top line to WS-MES-LINE. The message to be displayed must ³
      *    ³ be in WS-ERR-MES. PERFORM ERROR-MESSAGE.                  ³
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
      *    ³ The option request must be placed in WS-OPT-MES and may   ³
      *    ³ not exceed 48 characters. The message will be centred in  ³
      *    ³ the window. An example of a message request follows:      ³
      *    ³                                                           ³
      *    ³   MOVE "Print transactions (Y/N) [ ]" TO WS-OPT-MES.      ³
      *    ³   MOVE Instruction    TO WS-INSTR.                        ³
      *    ³       [see Accptopt for instruction values]               ³
      *    ³   PERFORM OPT-MESSAGE.                                    ³
      *    ³                                                           ³
      *    ³ This would be displayed as:                               ³
      *    ³   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿      ³
      *    ³   ³           Print transactions (Y/N) [ ]         ³°°    ³
      *    ³   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°    ³
      *    ³     °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°    ³
      *    ³                                                           ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³                                                           ³
      *    ³ To display the request message higher or lower (default   ³
      *    ³ is line 20) move the line number to be used to            ³
      *    ³ WS-MES-LINE.                                              ³
      *    ³                                                           ³
      *    ³ If character 48 of the message contains a value of X"FA"  ³
      *    ³ then do not display the message in a window, only get the ³
      *    ³ User reponse at the position specified by WS-MES-LINE and ³
      *    ³ WS-MES-COL.                                               ³
      *    ³                                                           ³
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

       COPY "DATE.CHK".

       COPY "DATE.CNV".

       COPY "AA300.STK".

       COPY "ABORT.PRN".

       COPY "PRINTUTL.AS3".

      /
       AB000              SECTION.
       AB05.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     1 O   C P I   ( N O R M A L   P R I N T )        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
            MOVE SPACES  TO REP-DETAIL1.
      MOVE 0   TO WS-ADVANCE.
      MOVE 1   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE SPACES  TO REP-DETAIL1.
            MOVE "Page:"  TO REP-PAGE.
            MOVE "Date:"  TO REP-DATE.
            MOVE W95-COMP  TO REP-COMPANY.
             ADD 1               TO WS-PAGE.
            MOVE WS-PAGE  TO REP-P-NO.
            MOVE W12-TODAY  TO REP-DTE.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.

       AB10.
    IF WS-REP = "D"
              MOVE W35-PERIOD  TO REP-HEAD
    ELSE
        MOVE W35-REFERENCE
     TO REP-HEAD.
      MOVE 2   TO WS-ADVANCE.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     1 2   C P I   ( 9 6   Characters per line )        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      MOVE 0   TO WS-ADVANCE.
      MOVE 2   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      MOVE "H"   TO W02-PRN-TYPE.
      MOVE 2   TO WS-ADVANCE.
      MOVE W35-HEADING  TO REP-HEAD.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE 1   TO WS-ADVANCE.
      MOVE "D"   TO W02-PRN-TYPE.

       AB20.
      EXIT.

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000    SECTION.

       COPY "CONTROL.RD".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "STKDEX.RD".

       COPY "STOCK.RD".

      *
      *   R E W R I T E   &    W R I T E   F I L E S   R O U T I N E S
      *
       MOVEMENT-LOOKUP   SECTION.

       MOVE-LUP-SCREEN.
            DISPLAY S-MOV.
      INITIALIZE STX-REC1.
      MOVE W10-EXT-ITEM  TO STX-EXT-CODE.
      MOVE W15-S-YMD  TO STX-DTE.

       MOVE-LUP-CLR-SCREEN.
             MOVE 0601           TO CPOS.
            PERFORM MOVE-CRT-EDGE UNTIL CLIN = 22.

       MOVE-LUP-CONTINUE.
            MOVE 6   TO CLIN.
             MOVE 1              TO W75-S W75-S1.

       MOVE-LUP-TAB-LOOP.
      MOVE ZERO   TO W75-DATE (W75-S1).
    IF W75-S1 < 16
               ADD 1             TO W75-S1
        GO TO MOVE-LUP-TAB-LOOP.
             MOVE ZERO           TO W75-S1.

       MOVE-LUP-START-STDX.
      MOVE SPACES  TO WS-OPTION.
            PERFORM START-AT-STDX-KEY THRU READ-STKDEX-EXIT.
          IF WS-F-ERROR = 46
        GO TO MOVE-NO-RECORD.

       MOVE-GET-STKDEX-REC.
           IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
        GO TO MOVE-SKIP-STDX-READ.
            PERFORM READ-STKDEX-NEXT THRU READ-STKDEX-EXIT.
    IF WS-F-ERROR = 46
               SUBTRACT 1        FROM W75-S
        GO TO MOVE-FULL-SCREEN.

       MOVE-LUP-DISP-ITEM.
    IF NOT (STX-EXT-CODE = W10-EXT-ITEM)
        GO TO MOVE-FULL-SCREEN.
    IF (STX-DTE > W15-E-YMD)
        GO TO MOVE-FULL-SCREEN.

       MOVE-SKIP-STDX-READ.
             MOVE STX-DTE            TO W22-DTE2.
             PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT.
             MOVE W22-DTE3           TO W95-DTE.
            DISPLAY (CLIN, 2) W95-DTE WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      MOVE STX-USER  TO W100-USE.
            DISPLAY ( , 14) W100-USER WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            DISPLAY ( , 19) STX-WS WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            DISPLAY ( , 23) STX-PROG WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      COMPUTE W100-COST ROUNDED = STX-COST / STX-QNT.
      MOVE STX-QNT  TO W100-QNT.
            DISPLAY ( , 31) W100-COST WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      DISPLAY ( , 45) W100-QNT WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      DISPLAY ( , 59) STX-REMARKS WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
           IF CLIN < 21
               ADD 1             TO CLIN W75-S
              GO TO MOVE-GET-STKDEX-REC.
      
       MOVE-FULL-SCREEN.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
001020      DISPLAY " " AT 2303 WITH FOREGROUND-COLOR Brown HIGHLIGHT
             " - " WITH FOREGROUND-COLOR Grey
       "PgUp" WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     "/" WITH FOREGROUND-COLOR Grey
       "PgDn" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       " - " WITH FOREGROUND-COLOR Grey
             "Esc" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "ape." WITH FOREGROUND-COLOR Grey.

       MOVE-SAVE-FULL-SCREEN.
      PERFORM SAVE-SCREEN-2.
            MOVE 0   TO W75-S1.
             MOVE 0601           TO CPOS.

       MOVE-CRT-ATTRIB.
      MOVE 3920   TO W41-LENGTH.
      CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
           W42-ATTRIB2
           W41-LENGTH
     RETURNING WS-STATUS.
            MOVE 4000   TO W41-LENGTH.

       MOVE-GET-USER-OPT.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM CLEAR-L50.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
             GO TO MOVE-ADJ-NO-KEYS
   WHEN DOWN-KEY
             GO TO MOVE-CHK-NO-KEYS
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO MOVE-GET-USER-OPT
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO MOVEMENT-LOOKUP-EXIT
   WHEN PAGE-DOWN
             GO TO MOVE-GET-NEXT-PAGE
   WHEN PAGE-UP
             GO TO MOVE-GET-PREV-PAGE
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO MOVE-GET-USER-OPT.
      PERFORM AA900-ALARM.
            GO TO MOVE-GET-USER-OPT.

       MOVE-GET-NEXT-PAGE.
           IF W75-S < 16
              GO TO MOVE-GET-USER-OPT.

       MOVE-NEW-PAGE.
             MOVE 0601           TO CPOS.
            PERFORM MOVE-CRT-EDGE UNTIL CLIN = 22.
             MOVE 6              TO CLIN.
             MOVE LOW-VALUES     TO W75-KEYS.
             MOVE 1              TO W75-S W75-S1.
      PERFORM MOVE-LUP-TAB-LOOP.
            GO TO MOVE-GET-STKDEX-REC.

       MOVE-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *    *************************************************************
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *    *************************************************************
       MOVE "J"    TO WS-OPTION.

       MOVE-GET-PREV-LOOP.
      PERFORM READ-STKDEX-PREV THRU READ-STKDEX-EXIT.
    IF WS-F-ERROR = 46
        MOVE SPACE  TO WS-OPTION
        GO TO MOVE-NEW-PAGE.
    IF NOT (STX-EXT-CODE = W10-EXT-ITEM)
        PERFORM READ-STKDEX-NEXT THRU READ-STKDEX-EXIT
        GO TO MOVE-NEW-PAGE.
    IF STX-DTE < W15-START
        PERFORM READ-STKDEX-NEXT THRU READ-STKDEX-EXIT
        GO TO MOVE-NEW-PAGE.
    IF WS-IXS1 < 32
        ADD 1   TO WS-IXS1
        GO TO MOVE-GET-PREV-LOOP.
      GO TO MOVE-NEW-PAGE.

       MOVE-NO-RECORD.
      PERFORM CLEAR-L50.
            DISPLAY "No record - Press " AT 5012
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "ANY" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             " key"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
            GO TO MOVEMENT-LOOKUP-EXIT.

       MOVE-ADJ-NO-KEYS.
           IF W75-S1 > 1
               SUBTRACT 1        FROM W75-S1 CLIN.
            GO TO MOVE-HIGHLIGHT.

       MOVE-CHK-NO-KEYS.
           IF W75-S1 = ZERO
               MOVE 1            TO W75-S1
              GO TO MOVE-HIGHLIGHT.
           IF W75-S1 < W75-S
               ADD 1             TO W75-S1 CLIN.

       MOVE-HIGHLIGHT.
            PERFORM MOVE-CRT-ATTRIB.
            DISPLAY W43-DET(CLIN) AT CPOS WITH REVERSE-VIDEO.
            GO TO MOVE-GET-USER-OPT.

       MOVE-CRT-EDGE.
            DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR Black
          FOREGROUND-COLOR Cyan
       "          ³     ³   ³      ³             ³
      -       "     ³                      ""
        WITH FOREGROUND-COLOR Cyan
       "³" WITH BACKGROUND-COLOR Black
         FOREGROUND-COLOR Cyan.
            ADD 1   TO CLIN.

       MOVEMENT-LOOKUP-EXIT.
             EXIT.

       COPY "APAC.HLP".

      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   C O D E
      *
       CA000         SECTION.
       CA00.
            MOVE 18   TO W50-LENGTH.

       CA05.
    IF WS-EXT-STK = "Y"
              MOVE W10-EXT-ITEM TO W50-DATA
    ELSE
        MOVE W10-ITEM  TO W50-DATA
        MOVE W10-ITM  TO W50-ITM.
    IF W50-1 = SPACE
        IF WS-EXT-STK = "Y"
     MOVE W50-17  TO W10-EXT-ITEM
        ELSE
     MOVE W50-13  TO W10-ITEM
        END-IF
               GO TO CA05.
      CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
            MOVE W50-EXT-ITEM TO W10-EXT-ITEM.

       CA06.
            MOVE W50-EXT-ITEM  TO STK-CODE.

       CA10.
      PERFORM READ-STOCK THRU READ-STOCK-EXIT.
    IF WS-STATUS = "23"
        MOVE 22   TO WS-F-ERROR.

       CA99.
             EXIT.

      /    *************************************************************
      *    * E N Q U I R Y   ( A D J U S T M E N T   T R A C K I N G ) *
      *    *************************************************************
       BA000         SECTION 50.
       BA00.
             PERFORM ERASE-SCREEN.
      DISPLAY S01.
      MOVE 5   TO SHADE-ROW.
      MOVE 26   TO SHADE-COL.
      MOVE 32   TO SHADE-WIDTH.
      MOVE 7   TO SHADE-LINES.
      DISPLAY S05D.
      PERFORM SCREEN-SHADOW.
      MOVE W12-TODAY  TO W15-START W15-END.

       BA05.
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
       GO TO BA999
   WHEN OTHER  PERFORM AA900-ALARM
       GO TO BA05
        END-EVALUATE.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO BA05.
      PERFORM CLEAR-L50.
             MOVE W15-START          TO W22-DATE.
             PERFORM DATE-CHECK THRU CHECK-DATE-EXIT.
           IF NOT(W22-ERROR = 0)
               EVALUATE W22-ERROR
                 WHEN 1  MOVE "Invalid Start DATE"  TO WS-ERR-MES
                 WHEN 2  MOVE "Invalid Start DAY"   TO WS-ERR-MES
                 WHEN 3  MOVE "Invalid Start MONTH" TO WS-ERR-MES
               END-EVALUATE
               PERFORM ERROR-MESSAGE
               GO TO BA05.
             MOVE W20-DTE            TO W15-S-YMD.
             MOVE W15-END            TO W22-DATE.
             PERFORM DATE-CHECK THRU CHECK-DATE-EXIT.
           IF NOT(W22-ERROR = 0)
               EVALUATE W22-ERROR
                 WHEN 1  MOVE "Invalid End DATE"  TO WS-ERR-MES
                 WHEN 2  MOVE "Invalid End DAY"   TO WS-ERR-MES
                 WHEN 3  MOVE "Invalid End MONTH" TO WS-ERR-MES
               END-EVALUATE
               PERFORM ERROR-MESSAGE
               GO TO BA05.
             MOVE W20-DTE        TO W15-E-YMD.
    IF W15-E-YMD < W15-S-YMD
        MOVE "End date is before start date"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BA05.
      MOVE W15-START  TO W35-START.
      MOVE W15-END  TO W35-END.
      DISPLAY "The Print format will only include transactions that were processed in the" AT 1804
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Red.
      DISPLAY "Stock Adjust Quantities (Option 1/1/2/8). The Display and Report formats  " AT 1904
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Red.
      DISPLAY "include all stock adjustments that change quantities                      " AT 2004
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Red.

       BA10.
      MOVE "'P'rint, 'D'isplay or 'R'eport format  [D]"
     TO WS-ERR-MES.
      MOVE "D"   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "D" OR "P" OR "R")
        GO TO BA10.
      MOVE WS-OPTION  TO WS-ENQ.
    IF WS-ENQ = "P" OR "R"
              PERFORM ERASE-SCREEN
        DISPLAY S10D
        GO TO BA55.

       BA15.
             MOVE SPACES             TO W10-EXT-ITEM.
           IF W10-STCK = "N"
               MOVE ZERO             TO W10-I14.
      *        EVALUATE W10-SLNGTH
      *          WHEN 3   MOVE ZERO  TO W10-I3            
      *          WHEN 4   MOVE ZERO  TO W10-I4            
      *          WHEN 5   MOVE ZERO  TO W10-I5            
      *          WHEN 6   MOVE ZERO  TO W10-I6            
      *          WHEN 7   MOVE ZERO  TO W10-I7            
      *          WHEN 8   MOVE ZERO  TO W10-I8            
      *          WHEN 9   MOVE ZERO  TO W10-I9            
      *          WHEN 10  MOVE ZERO  TO W10-I10            
      *          WHEN 11  MOVE ZERO  TO W10-I11            
      *          WHEN 12  MOVE ZERO  TO W10-I12            
      *          WHEN 13  MOVE ZERO  TO W10-I13            
      *          WHEN 14  MOVE ZERO  TO W10-I14
      *        END-EVALUATE.

       BA20.
             PERFORM ERASE-SCREEN.
      DISPLAY WS-BLNK78 AT 0302
       WITH FOREGROUND-COLOR Cyan BACKGROUND-COLOR Blue.
      DISPLAY S10D.
    IF WS-USE-ITM = "Y"
        DISPLAY "/" AT 0328
   WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.

       BA25.
      MOVE WS-OPTION  TO WS-ENQ.

       BA30.
            PERFORM AA300.
      *    *************************************************************
      *                     G E T   S T O C K   C O D E
      *    *************************************************************
      *    
      *      Is an 18 character stock/inventory item code being used?  
      *    
           IF WS-EXT-STK = "Y"
               ACCEPT S36-EXT AT 0314
           ELSE
      *   
      *       Is an ALPHA-NUMERIC code being used?
      *    
           IF W10-STCK = "A"
      *    
      *     Is a 3 character extension code being used in addition to 
      *     the 14 character stock/inventory item code?               
      *    
               IF WS-USE-ITM = "Y"
                   ACCEPT S36-ITM AT 0314
               ELSE
                   ACCEPT S36 AT 0314
               END-IF
           ELSE
      *    *************************************************************
      *                         NUMERIC CODES APPLY 
      *    *************************************************************
      *    
      *     Check the length of the numeric Stock/Inventory item code 
      *     being used and allow the User to key in the code.         
      *    
               EVALUATE W10-SLNGTH
      *                     3 Digit numeric Stock/Inventory item code
                 WHEN 3         ACCEPT S36-3 AT 0314
      *                     4 Digit numeric Stock/Inventory item code
                 WHEN 4         ACCEPT S36-4 AT 0314
      *                     5 Digit numeric Stock/Inventory item code
                 WHEN 5         ACCEPT S36-5 AT 0314
      *                     6 Digit numeric Stock/Inventory item code
                 WHEN 6         ACCEPT S36-6 AT 0314
      *                     7 Digit numeric Stock/Inventory item code
                 WHEN 7         ACCEPT S36-7 AT 0314
      *                     8 Digit numeric Stock/Inventory item code
                 WHEN 8         ACCEPT S36-8 AT 0314
      *                     9 Digit numeric Stock/Inventory item code
                 WHEN 9         ACCEPT S36-9 AT 0314
      *                    10 Digit numeric Stock/Inventory item code
                 WHEN 10        ACCEPT S36-10 AT 0314
      *                    11 Digit numeric Stock/Inventory item code
                 WHEN 11        ACCEPT S36-11 AT 0314
      *                    12 Digit numeric Stock/Inventory item code
                 WHEN 12        ACCEPT S36-12 AT 0314
      *                    13 Digit numeric Stock/Inventory item code
                 WHEN 13        ACCEPT S36-13 AT 0314
      *                    14 Digit numeric Stock/Inventory item code
                 WHEN 14        ACCEPT S36-14 AT 0314
               END-EVALUATE.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
             PERFORM CLEAR-L50
       GO TO BA999
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       MOVE "I"  TO W10-ETYPE
   WHEN F3-KEY
       MOVE "A"  TO W10-ETYPE
   WHEN F4-KEY
       MOVE "D"  TO W10-ETYPE
   WHEN F5-KEY
       MOVE "2"  TO W10-ETYPE
   WHEN F6-KEY
       MOVE "X"  TO W10-ETYPE
   WHEN F9-KEY
       MOVE "W"  TO W10-ETYPE
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO BA30
        END-EVALUATE
              PERFORM AA50-LOOKUP
      *    *************************************************************
      *        *****   E X T E N D E D  C O D E   *****
      *    *************************************************************
        IF WS-EXT-STK = "Y"
     DISPLAY S36-EXT AT 0314
        ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
        IF W10-STCK = "A"
     IF WS-USE-ITM = "Y"
         DISPLAY S36-ITM AT 0314
     ELSE
               DISPLAY S36 AT 0314.
      PERFORM CLEAR-L50.

       BA35.
          IF W10-EXT-ITEM = SPACES
              GO TO BA30.

       BA40.
             PERFORM CA000.
           IF WS-F-ERROR = 22
        MOVE "Z"   TO W10-ETYPE
              PERFORM AA50-LOOKUP
      *    *************************************************************
      *        *****   E X T E N D E D  C O D E   *****
      *    *************************************************************
        IF WS-EXT-STK = "Y"
     DISPLAY S36-EXT AT 0314
        ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
        IF W10-STCK = "A"
     IF WS-USE-ITM = "Y"
         DISPLAY S36-ITM AT 0314
     ELSE
               DISPLAY S36 AT 0314
     END-IF
        END-IF
              IF W10-EXT-ITEM = SPACES
           GO TO BA30
               ELSE
     GO TO BA40.
      DISPLAY "Description" AT 0333.
      DISPLAY STK-DESC AT 0345
       WITH BACKGROUND-COLOR Magenta
     FOREGROUND-COLOR Cyan HIGHLIGHT.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO BA30.

       BA45.
      PERFORM MOVEMENT-LOOKUP.
      MOVE SPACE   TO WS-OPTION.

       BA50.
      MOVE "Change 'D'ates,'I'tem code or 'E'nd  [ ]"
     TO WS-ERR-MES.
            PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "D" OR "E" OR "I")
              GO TO BA50.
    IF WS-OPTION = "I"
        GO TO BA20
    ELSE
    IF WS-OPTION = "D"
        GO TO BA00.
      GO TO BA999.

       BA55.
      MOVE ZERO   TO WS-PAGE.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *     **  *   *   * *   *  *  **  *   *   *     *   *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *     *  **   *     *   *  *  *  **   *   *     *  *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** *   *
      *
             PERFORM OPEN-PRINTER.
             MOVE "P"                TO WS-COMMAND.

       BA60.
             MOVE "  !"              TO STX-EXT-CODE.
             MOVE W15-S-YMD          TO STX-DTE.
             MOVE ZERO               TO STX-TIME.
             PERFORM START-AT-STDX-DTE-TME THRU READ-STKDEX-EXIT.
      *
      *      Before time was included in file
      *
      *      PERFORM START-AT-STDX-DATE THRU READ-STKDEX-EXIT.
           IF WS-F-ERROR = 46
               GO TO BA75.
             MOVE ZERO               TO WS-QTOT WS-VTOT.
             PERFORM AB000.
             DISPLAY "Processing Record:" AT 1212.

       BA65.
             PERFORM READ-STKDEX-NEXT THRU READ-STKDEX-EXIT.
           IF WS-F-ERROR = 46
               GO TO BA70.
           IF (STX-DTE > W15-E-YMD)
               GO TO BA70.
           IF WS-ENQ = "P"
               IF NOT(STX-PROG = "STKADJ")
                   GO TO BA65.
             DISPLAY STX-EXT-CODE AT 1231 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE STX-EXT-CODE       TO REP-ITEM STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
             MOVE STK-DESC           TO REP-DESC.
             MOVE STX-QNT            TO REP-QUANT.
             MOVE STX-COST           TO REP-COST.
             MOVE STX-REMARKS        TO REP-REMK.
             ADD STX-QNT             TO WS-QTOT.
             ADD STX-COST            TO WS-VTOT.
           IF W02-LINAGE < W02-PRN-LENGTH
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM AB000.
             MOVE SPACES             TO REP-DETAIL1.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE 0                TO WS-ADVANCE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³          1 O   C P I   ( N O R M A L   P R I N T )        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               MOVE 1                TO W02-PRN-TYPE
               PERFORM CALL-PRINTUTL
               MOVE "D"              TO W02-PRN-TYPE
               MOVE 1                TO WS-ADVANCE
               MOVE SPACES           TO REP-DETAIL1
               MOVE "**** PRINT ABORTED BY OPERATOR ****" TO REP-DETAIL1
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE "P"              TO WS-COMMAND
               GO TO BA999.
             GO TO BA65.

       BA70.
      MOVE ALL "Ä"  TO REP-TOT-LINE.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
            MOVE "**** TOTAL UNITS ****"
     TO REP-DESC.
            MOVE WS-QTOT  TO REP-QUANT.
            MOVE WS-VTOT  TO REP-COST.
      MOVE 99   TO WS-ADVANCE.
      MOVE "E"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
      GO TO BA80.

       BA75.
      MOVE "No details for period"
     TO WS-ERR-MES.
            PERFORM ERROR-MESSAGE.

       BA80.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.

       BA85.
      MOVE "Change 'D'ates or 'E'nd  [ ]"
     TO WS-ERR-MES.
            PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "D" OR "E")
              GO TO BA85.
    IF WS-OPTION = "D"
        GO TO BA00.

       BA999.
             EXIT.
      /    *************************************************************
      *    * E N Q U I R Y   ( A D J U S T M E N T   T R A C K I N G ) *
      *    *************************************************************
       BB000         SECTION 51.
       BB00.
            PERFORM ERASE-SCREEN.
      DISPLAY S01.
      MOVE 8   TO SHADE-ROW.
      MOVE 29   TO SHADE-COL.
      MOVE 25   TO SHADE-WIDTH.
      MOVE 2   TO SHADE-LINES.
      DISPLAY "Ú" AT 0727
        WITH FOREGROUND-COLOR Magenta HIGHLIGHT
      BACKGROUND-COLOR Magenta
       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Magenta.
      DISPLAY "³" AT 0827
        WITH FOREGROUND-COLOR Magenta HIGHLIGHT
      BACKGROUND-COLOR Magenta
       " Reference Code          "
        WITH FOREGROUND-COLOR Cyan HIGHLIGHT
      BACKGROUND-COLOR Magenta
       "³" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Magenta.
      DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0927
        WITH FOREGROUND-COLOR Magenta HIGHLIGHT
      BACKGROUND-COLOR Magenta
       "Ù" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Magenta.
      PERFORM SCREEN-SHADOW.
      MOVE SPACES  TO WS-REFERENCE.

       BB05.
      ACCEPT S03.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BB999
   WHEN OTHER  PERFORM AA900-ALARM
       GO TO BB05
        END-EVALUATE.
    IF WS-REFERENCE = SPACES
        GO TO BB999.

       BB10.
      MOVE "'C'ontinue or 'R'ekey Code  [C]"
     TO WS-ERR-MES.
      MOVE "C"   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT(WS-OPTION = "C" OR "R")
        GO TO BB10.
    IF WS-OPTION = "R"
        GO TO BB05.
      MOVE WS-REFERENCE  TO STX-GRN.
      MOVE ZERO   TO STX-DTE STX-TIME.
      PERFORM START-AT-STDX-GRN THRU READ-STKDEX-EXIT.
    IF WS-F-ERROR = 46
        GO TO BB75.
            PERFORM READ-STKDEX-NEXT THRU READ-STKDEX-EXIT.
    IF NOT(STX-GRN = WS-REFERENCE)
        GO TO BB75.

       BB12.
      MOVE STX-DTE  TO W15-S-YMD.
      MOVE W15-SCC  TO W15-SDAY.
            MOVE W15-SYY  TO W15-SMNTH.
            MOVE W15-SMM  TO W15-SCENT.
            MOVE W15-SDD  TO W15-SYEAR.

       BB14.
      PERFORM START-AT-STDX-GRN THRU READ-STKDEX-EXIT.
      MOVE WS-REFERENCE  TO W35-REF.
      *
      *  ***  ****  ***** *   *   **** ****  *** *   * ***** ***** ****
      * *   * *   * *   **  *   *   * *   *  *  **  *   *   *     * *
      * *   * ****  ***   * * *   **** ****   *  * * *   *   ***   ****
      * *   * *     *   *  **   * *   *  *  *  **   *   *     * *
      *  ***  *     ***** *   *   * *   * *** *   *   *   ***** * *
      *
      MOVE ZERO   TO WS-PAGE.
      MOVE "C"   TO W02-PRN-STATUS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³   Open the printer file - Allow for Screen option.     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      MOVE "O"   TO WS-COMMAND.
      MOVE "P"   TO WS-PRINTER.
      MOVE ZERO   TO WS-ADVANCE
        WS-REPORT
        WS-SCREEN.
      MOVE 3   TO WS-REPORT.
      PERFORM CALL-PRINTUTL.
      MOVE "P"   TO WS-COMMAND.
      MOVE ZERO   TO WS-QTOT WS-VTOT.
      PERFORM AB000.
      MOVE "R.Dte"  TO REP-DATE.
      MOVE W15-START  TO REP-DTE.
      DISPLAY "Processing Record:" AT 1212.

       BB15.
            PERFORM READ-STKDEX-NEXT THRU READ-STKDEX-EXIT.
          IF WS-F-ERROR = 46
        GO TO BB20.
    IF NOT(STX-GRN = WS-REFERENCE)
        GO TO BB20.
    IF NOT(STX-DTE = W15-S-YMD)
        PERFORM BB20
        IF W02-LINAGE < (W02-PRN-LENGTH - 6)
     MOVE 1  TO WS-ADVANCE
     PERFORM CALL-PRINTUTL
              ELSE
     MOVE 99  TO WS-ADVANCE
     PERFORM CALL-PRINTUTL
           PERFORM AB000.

    IF NOT(WS-PRINTER = "S")
        DISPLAY STX-EXT-CODE AT 1231
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      MOVE STX-EXT-CODE  TO REP-ITEM STK-CODE.
      PERFORM READ-STOCK THRU READ-STOCK-EXIT.
      MOVE STK-DESC  TO REP-DESC.
      MOVE STX-QNT  TO REP-QUANT.
      MOVE STX-COST  TO REP-COST.
      MOVE STX-REMARKS  TO REP-REMK.
      ADD STX-QNT  TO WS-QTOT.
      ADD STX-COST  TO WS-VTOT.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM AB000.
            MOVE SPACES  TO REP-DETAIL1.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
        MOVE 0   TO WS-ADVANCE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     1 O   C P I   ( N O R M A L   P R I N T )        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
        MOVE 1   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
        MOVE "D"   TO W02-PRN-TYPE
        MOVE 1   TO WS-ADVANCE
              MOVE SPACES  TO REP-DETAIL1
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        MOVE "P"   TO WS-COMMAND
        GO TO BB999.
      GO TO BB15.

       BB20.
      MOVE ALL "Ä"  TO REP-TOT-LINE.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
            MOVE "**** TOTAL UNITS ****"
     TO REP-DESC.
            MOVE WS-QTOT  TO REP-QUANT.
            MOVE WS-VTOT  TO REP-COST.

       BB25.
      MOVE 99   TO WS-ADVANCE.
      MOVE "E"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
      GO TO BB80.

       BB75.
      MOVE "No details for Reference Code"
     TO WS-ERR-MES.
            PERFORM ERROR-MESSAGE.
      GO TO BB05.

       BB80.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.

       BB85.
      MOVE "'N'ext reference or 'E'nd  [ ]"
     TO WS-ERR-MES.
            PERFORM OPT-MESSAGE.
          IF NOT(WS-OPTION = "N" OR "E")
              GO TO BB85.
    IF WS-OPTION = "N"
        GO TO BB00.

       BB999.
      EXIT.
      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT        SECTION 8.
       ZA000-OPEN.
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

       ZA00A-CONTINUE.
            MOVE 3   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
    IF NOT (PAR-STOCK = "A" OR "N")
        MOVE "A"   TO PAR-STOCK.
      MOVE PAR-STOCK  TO W10-STCK.
      MOVE PAR-USE-ITM  TO WS-USE-ITM.
      MOVE PAR-EXT-STK  TO WS-EXT-STK.
    IF PAR-STOCK = "N"
        MOVE PAR-SLNGTH  TO W10-SLNGTH
    ELSE
        MOVE 14   TO W10-SLNGTH.
    IF W10-SLNGTH < 3
        MOVE 3   TO W10-SLNGTH
    ELSE
    IF W10-SLNGTH > 14
        MOVE 14   TO W10-SLNGTH.
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
            GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
            GO TO ZA205.
       ZA50.
          IF WS-F-ERROR = 46
              MOVE "ADJUSTMENT file"
     TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Press any key to continue" AT 1012.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
       ZA51.
             EXIT PROGRAM.

       ZA200.

       COPY "LOCKED.PRO".

       ZA205.
             EXIT PROGRAM.

       ZA999-EXIT.
             EXIT.
      *
      *    ****    I / O   E R R O R   M E S S A G E S
      *
       ZB000-ERROR      SECTION 8.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 2     MOVE W02-NETWORK  TO WS-FILE
                          MOVE WS-NETKEY    TO WS-KEY
               WHEN 7     MOVE W02-DEPART   TO WS-FILE
                          MOVE DPT-CODE     TO WS-KEYX
               WHEN 15    MOVE WS-PARID     TO WS-FILE
                          MOVE WS-PARKEY    TO WS-KEY
               WHEN 22    MOVE W02-STOCKF   TO WS-FILE
                          MOVE STK-CODE     TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 46
              MOVE W02-STKDEX  TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE STX-KEY  TO WS-KEYX
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STPADJ"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
