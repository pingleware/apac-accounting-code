      $set LINKCOUNT"576" GNT"STPMENU.GNT"
      ******************************************************************
      *                                                                *
      *   ******   ********  *******   **     **  ********  **     **  *
      *  **    **     **     **    **  ***   ***  **        ***    **  *
      *  **           **     **    **  ** * * **  **        ** *   **  *
      *   ******      **     *******   **  *  **  *****     **  *  **  *
      *        **     **     **        **     **  **        **   * **  *
      *  **    **     **     **        **     **  **        **    ***  *
      *   ******      **     **        **     **  *******   **     **  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S T O C K   M O D U L E   M E N U   P R O G R A M          *
      *                                                                *
      *       Version 9.04.01 - May 2016                               *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  Aug 2008 - New file (STKALT) - Stock alternate index          *
      *             included for lookups, using any word con-          *
      *             tained in the Stock description.                   *
      *                                                                *
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                VERSIONS OF THE APAC SYSTEM                Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё While developing this system the requirements and budgets Ё
      *    Ё (originally this was a propriety package) for various     Ё
      *    Ё businesses necessitated that at least 4 separate systems  Ё
      *    Ё be produced. As the maintenance and amendments to the 4   Ё
      *    Ё different systems would require man hours that would not  Ё
      *    Ё enable me to do much development, it was decided to use   Ё
      *    Ё one system with the various requirements controlled by    Ё
      *    Ё the various menu programs using a version indicator. This Ё
      *    Ё indicator is located in the second record of the SYSTEM   Ё
      *    Ё file SEC-LEV, which is in the SEC-MODULES. This indicator Ё
      *    Ё together with all the other indicators in SEC-MODULES, is Ё
      *    Ё moved to WS0-MODULES in BUP000 or BUP001 and then passed  Ё
      *    Ё via the LINKAGE SECTION from program to program in the    Ё
      *    Ё LS0-MODULES group. The other indicators in this group are Ё
      *    Ё used to show which modules of the system are installed.   Ё
      *    фмммммммммммммммммммямммммммммммммммммммяммммммммммммммммммм╣
      *    Ё    SYSTEM FILE    Ё      CHAIN.WS     Ё     CHAIN.LS      Ё
      *    цдддддддддддддддддддедддддддддддддддддддеддддддддддддддддддд╢
      *    Ё03  SEC-MODULES.   Ё03  WS0-MODULES.   Ё03 LS0-MODULES.    Ё
      *    Ё  05  SEC-DTP 9(01)Ё  05  WS0-DTP 9(01)Ё  05  LS0-DTP 9(01)Ё
      *    Ё  05  SEC-STP 9(01)Ё  05  WS0-STP 9(01)Ё  05  LS0-STP 9(01)Ё
      *    Ё  05  SEC-HPD 9(01)Ё  05  WS0-HPD 9(01)Ё  05  LS0-HPD 9(01)Ё
      *    Ё  05  SEC-CRP 9(01)Ё  05  WS0-CRP 9(01)Ё  05  LS0-CRP 9(01)Ё
      *    Ё  05  SEC-GLP 9(01)Ё  05  WS0-GLP 9(01)Ё  05  LS0-GLP 9(01)Ё
      *    Ё  05  SEC-POS 9(01)Ё  05  WS0-POS 9(01)Ё  05  LS0-POS 9(01)Ё
      *    Ё  05  SEC-JCP 9(01)Ё  05  WS0-JCP 9(01)Ё  05  LS0-JCP 9(01)Ё
      *    Ё  05  SEC-VHP 9(01)Ё  05  WS0-VHP 9(01)Ё  05  LS0-VHP 9(01)Ё
      *    Ё  05  SEC-WAG 9(01)Ё  05  WS0-WAG 9(01)Ё  05  LS0-WAG 9(01)Ё
      *    Ё  05  SEC-SAL 9(01)Ё  05  WS0-SAL 9(01)Ё  05  LS0-SAL 9(01)Ё
      *    Ё  05  SEC-LEV 9(01)Ё  05  WS0-LEV 9(01)Ё  05  LS0-LEV 9(01)Ё
      *    Ё  05  SEC-WHS 9(01)Ё  05  WS0-WHS 9(01)Ё  05  LS0-WHS 9(01)Ё
      *    Ё  05  SEC-RTE 9(01)Ё  05  WS0-RTE 9(01)Ё  05  LS0-RTE 9(01)Ё
      *    Ё  05  FILLER  X(07)Ё  05  FILLER  X(07)Ё  05  FILLER  X(07)Ё
      *    юдддддддддддддддддддадддддддддддддддддддаддддддддддддддддддды
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     STPMENU.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   JANUARY 1983.

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

       COPY "CARDEX.SL".

       COPY "CONTROL.SL".

       COPY "CREDIT.SL".

       COPY "LEDTRF.SL".

       COPY "PARAM.SL".

       COPY "PORDER.SL".

       COPY "PURDEX.SL".

       COPY "SPARTS.SL".

       COPY "STKALT.SL".

       COPY "STKDEX.SL".

       COPY "STKMEM.SL".

       COPY "STOCK.SL".

       COPY "WARHSE.SL".

       COPY "WSTOCK.SL".

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

       COPY "CARDEX.FDE".

       COPY "CONTROL.FDE".

       COPY "CREDIT.FDE".

       COPY "LEDTRF.FDE".

       COPY "PARAM.FDE".

       COPY "PORDER.FDE".

       COPY "PURDEX.FDE".

       COPY "SPARTS.FDE".

       COPY "STKALT.FDE".

       COPY "STKDEX.FDE".

       COPY "STKMEM.FDE".

       COPY "STOCK.FDE".

       COPY "WARHSE.FDE".

       COPY "WSTOCK.FDE".

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
       77  WS-CHECK                  PIC  X(18)    VALUE "aeWlimemnomLalismJ".
       77  WS-S1                     PIC  9(04)    COMP-5.
       77  WS-S2                     PIC  9(04)    COMP-5.
       77  WS-S3                     PIC  9(04)    COMP-5.
       77  WS-S4                     PIC  9(04)    COMP-5.
       77  WS-NETKEY                 PIC  9(04)    COMP-5.
       77  WS-PARKEY                 PIC  9(04)    COMP-5.
       77  WS-TEMP-LIN               PIC  9(02)    COMP-5.
       77  WS-INSTR                  PIC  X(01).
       77  WS-OPTION                 PIC  X(01).
       77  WS-REOPEN                 PIC  X(01)    VALUE SPACE.
       77  WS-PASSWORD               PIC  X(08).
       77  WS-PASS                   PIC  X(08).
       77  PRG-NAME                  PIC  X(12)    VALUE SPACES.
       77  TODAY-DDMMYY              PIC  9(08)    COMP-5.

      *    здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Used by calling program (WORKING-STORAGE) and called program Ё
      *    Ё (LINKAGE SECTION) for Main screen layout and headings.   Ё
      *    юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *       C = Clear comment line (50)
      *       E = Clear lines (any line or lines between 5 and 46)
      *       H = Change heading
      *       S = Clear the screen and display headings
      *
           03  CRT-TYPE              PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START             PIC  9(02).
           03  CRT-END               PIC  9(02).
      *
      *    Calling Program
      *
           03  CRT-PROGRAM           PIC  X(15) VALUE "STPMENU".
      *
      *    Screen Heading
      *
           03  CRT-HEADING           PIC  X(40) VALUE "STOCK (INVENTORY) CONTROL MODULE".
      *
      *    Company Name
      *
           03  CRT-COMPANY           PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-TERMINAL.
               05  CRT-WRKHD         PIC  X(11).
               05  CRT-WRKST         PIC  X(03).

       COPY "WS.WS".

       01  WS-PARID.
           03  WS-SYS-ID       PIC  X(03).

       01  BLOCK-DETAIL1.
           03  ORIGINAL-VID1.
               05  ORIGINAL-CHAR1    PIC  X(01) OCCURS 80.
           03  REVERSE-VID1.
               05  REVERSE-CHAR1     PIC  X(01) OCCURS 80.

       01  SHADOW-DETAIL1.
           03  SHAD1-ROW             PIC  9(02) COMP-X.
           03  SHAD1-COL             PIC  9(02) COMP-X.
           03  SHAD1-LINES           PIC  9(02) COMP-X.
           03  SHAD1-WIDTH           PIC  9(02) COMP-X.
           03  SHAD1-CHAR            PIC  X(01).

       01  CRT-DETAIL1.
           03  TOP-ROW1              PIC  9(02) COMP-X.
           03  BOTTOM-ROW1           PIC  9(02) COMP-X.
           03  STRING-LENGTH1        PIC  9(04) COMP-X.
           03  SCREEN-POSITION1.
               05  SCREEN-LIN1       PIC  9(02) COMP-X.
               05  SCREEN-COL1       PIC  9(02) COMP-X.

       01  BLOCK-DETAIL2.
           03  ORIGINAL-VID2.
               05  ORIGINAL-CHAR2    PIC  X(01) OCCURS 80.
           03  REVERSE-VID2.
               05  REVERSE-CHAR2     PIC  X(01) OCCURS 80.

       01  SHADOW-DETAIL2.
           03  SHAD2-ROW             PIC  9(02) COMP-X.
           03  SHAD2-COL             PIC  9(02) COMP-X.
           03  SHAD2-LINES           PIC  9(02) COMP-X.
           03  SHAD2-WIDTH           PIC  9(02) COMP-X.
           03  SHAD2-CHAR            PIC  X(01).

       01  CRT-DETAIL2.
           03  TOP-ROW2              PIC  9(02) COMP-X.
           03  BOTTOM-ROW2           PIC  9(02) COMP-X.
           03  STRING-LENGTH2        PIC  9(04) COMP-X.
           03  SCREEN-POSITION2.
               05  SCREEN-LIN2       PIC  9(02) COMP-X.
               05  SCREEN-COL2       PIC  9(02) COMP-X.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

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

       COPY "MENUINST.CRT".

       01  S02.
           03  BACKGROUND-COLOR Green FOREGROUND-COLOR Black.
               05  LINE  8 COLUMN  7 VALUE "                    ".
               05  LINE  9 COLUMN  7 VALUE "  STOCK PROGRAMS    " FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 10 COLUMN  7 VALUE " 1"                   FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Maintenance      ".
               05  LINE 11 COLUMN  7 VALUE " 2"                   FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Reports          ".
               05  LINE 12 COLUMN  7 VALUE " 3"                   FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Export Options   ".
               05  LINE 13 COLUMN  7 VALUE " 4"                   FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Enquiries        ".
               05  LINE 14 COLUMN  7 VALUE " 5"                   FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Login/Logout     ".
               05  LINE 15 COLUMN  7 VALUE " 6"                   FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Enquiries (Ord)  ".
               05  LINE 16 COLUMN  7 VALUE " 7"                   FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Sales Tracking   ".
               05  LINE 17 COLUMN  7 VALUE " 8"                   FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Purch. Tracking  ".
               05  LINE 18 COLUMN  7 VALUE " A"                   FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Adjust. Tracking ".
               05  LINE 19 COLUMN  7 VALUE " B"                   FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Price Labels     ".
               05  LINE 20 COLUMN  7 VALUE " C"                   FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Barcode Labels   ".
               05  LINE 21 COLUMN  7 VALUE " D"                   FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Barcode System   ".
               05  LINE 22 COLUMN  7 VALUE " E"                   FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Stock Options    ".
               05  LINE 23 COLUMN  7 VALUE " F"                   FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Promotions/Spec. ".
               05  LINE 24 COLUMN  7 VALUE " G"                   FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Utilities        ".
               05  LINE 25 COLUMN  7 VALUE " 0"                   FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Exit             ".
               05  LINE 26 COLUMN  7 VALUE "                    ".

       01  S03.
           03   BACKGROUND-COLOR Cyan FOREGROUND-COLOR Black.
               05  LINE 10 COLUMN 26 VALUE "                        ".
               05  LINE 11 COLUMN 26 VALUE "   STOCK MAINTENANCE    " FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 12 COLUMN 26 VALUE " 1"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Record Maintenance   ".
               05  LINE 13 COLUMN 26 VALUE " 2"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Price/Quant Adjust   ".
               05  LINE 14 COLUMN 26 VALUE " 3"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Production Routines  ".
               05  LINE 15 COLUMN 26 VALUE " 4"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Month-End Routine    ".
               05  LINE 16 COLUMN 26 VALUE " 5"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Enquiries            ".
               05  LINE 17 COLUMN 26 VALUE " 6"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Enquiries (Orders)   ".
               05  LINE 18 COLUMN 26 VALUE " 7"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Descript/Price Names ".
               05  LINE 19 COLUMN 26 VALUE " 8"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " W/House Maintenance  ".
               05  LINE 20 COLUMN 26 VALUE " 0"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Exit                 ".
               05  LINE 21 COLUMN 26 VALUE "                        ".

       01  S04.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE 12 COLUMN 48 VALUE "                       ".
               05  LINE 13 COLUMN 48 VALUE "  RECORD MAINTENANCE   " FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 14 COLUMN 48 VALUE " 1"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 50 VALUE   " Create New Record   ".
               05  LINE 15 COLUMN 48 VALUE " 2"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 50 VALUE   " Amend Existing Rec. ".
               05  LINE 16 COLUMN 48 VALUE " 3"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 50 VALUE   " Change Item Code    ".
               05  LINE 17 COLUMN 48 VALUE " 4"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 50 VALUE   " Delete Stock Record ".
               05  LINE 18 COLUMN 48 VALUE " 5"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 50 VALUE   " Flag Redundant Item ".
               05  LINE 19 COLUMN 48 VALUE " 6"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 50 VALUE   " Amend Supplier Code ".
               05  LINE 20 COLUMN 48 VALUE " 7"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 50 VALUE   " Enquiries           ".
               05  LINE 21 COLUMN 48 VALUE " 0"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 50 VALUE   " Exit                ".
               05  LINE 22 COLUMN 48 VALUE "                       ".

       01  S05.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE 12 COLUMN 48 VALUE "                         ".
               05  LINE 13 COLUMN 48 VALUE " PRICE/QUANT ADJUSTMENTS " FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 14 COLUMN 48 VALUE "  1"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 51 VALUE    " Price Changes        ".
               05  LINE 15 COLUMN 48 VALUE "  2"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 51 VALUE    " Price Changes (Dept) ".
               05  LINE 16 COLUMN 48 VALUE "  3"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 51 VALUE    " Price Adjust (Dept)  ".
               05  LINE 17 COLUMN 48 VALUE "  4"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 51 VALUE    " Stock Quantities     ".
               05  LINE 18 COLUMN 48 VALUE "  5"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 51 VALUE    " Quantities per Dept. ".
               05  LINE 19 COLUMN 48 VALUE "  6"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 51 VALUE    " Lead-Time/ReOrd Adj  ".
               05  LINE 20 COLUMN 48 VALUE "  7"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 51 VALUE    " Enquiries            ".
               05  LINE 21 COLUMN 48 VALUE "  8"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 51 VALUE    " Adjust Quantities    ".
               05  LINE 22 COLUMN 48 VALUE "  0"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 51 VALUE    " Exit                 ".
               05  LINE 23 COLUMN 48 VALUE "                         ".

       01  S06.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE 12 COLUMN 48 VALUE "                       ".
               05  LINE 13 COLUMN 48 VALUE "  PRODUCTION ROUTINES  " FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 14 COLUMN 48 VALUE " 1"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 50 VALUE   " Change to Produced  ".
               05  LINE 15 COLUMN 48 VALUE " 2"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 50 VALUE   " Recorded Details    ".
               05  LINE 16 COLUMN 48 VALUE " 3"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 50 VALUE   " Details - all Items ".
               05  LINE 17 COLUMN 48 VALUE " 4"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 50 VALUE   " Details - per Item  ".
               05  LINE 18 COLUMN 48 VALUE " 5"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 50 VALUE   " Update Prod Prices  ".
               05  LINE 19 COLUMN 48 VALUE " 6"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 50 VALUE   " Enquiries           ".
               05  LINE 20 COLUMN 48 VALUE " 7"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 50 VALUE   " Change to NOT Prod. ".
               05  LINE 21 COLUMN 48 VALUE " 0"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 50 VALUE   " Exit                ".
               05  LINE 22 COLUMN 48 VALUE "                       ".

       01  S07.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE 10 COLUMN 26 VALUE "                        ".
               05  LINE 11 COLUMN 26 VALUE "       UTILITIES        " FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 12 COLUMN 26 VALUE " 1"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Create ALT DESC Idx  ".
               05  LINE 13 COLUMN 26 VALUE " 2"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Clear Guarantees     ".
               05  LINE 14 COLUMN 26 VALUE " 3"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Clear Quantities     ".
               05  LINE 15 COLUMN 26 VALUE " 4"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Clr Quantities -Dept ".
               05  LINE 16 COLUMN 26 VALUE " 5"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Delete Item Range    ".
               05  LINE 17 COLUMN 26 VALUE " 6"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Price Label Code     ".
               05  LINE 18 COLUMN 26 VALUE " 7"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " ReCalculate Prices   ".
               05  LINE 19 COLUMN 26 VALUE " 8"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Enquiries            ".
               05  LINE 20 COLUMN 26 VALUE " A"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Fix Average Prices   ".
               05  LINE 21 COLUMN 26 VALUE " 0"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Exit                 ".
               05  LINE 22 COLUMN 26 VALUE "                        ".

       01  S08.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Black.
               05  LINE 10 COLUMN 26 VALUE "                        ".
               05  LINE 11 COLUMN 26 VALUE "    STOCK REPORTING     " FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 12 COLUMN 26 VALUE " 1"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Price Lists          ".
               05  LINE 13 COLUMN 26 VALUE " 2"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Departmental Lists   ".
               05  LINE 14 COLUMN 26 VALUE " 3"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " List - Item Code Seq ".
               05  LINE 15 COLUMN 26 VALUE " 4"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Stock Take List      ".
               05  LINE 16 COLUMN 26 VALUE " 5"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Movement Report      ".
               05  LINE 17 COLUMN 26 VALUE " 6"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " LowLevels / ReOrder  ".
               05  LINE 18 COLUMN 26 VALUE " 7"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Bill of Quantities   ".
               05  LINE 19 COLUMN 26 VALUE " 8"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Quantity/Price List  ".
               05  LINE 20 COLUMN 26 VALUE " A"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Price Labels         ".
               05  LINE 21 COLUMN 26 VALUE " B"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Slow Moving Stock    ".
               05  LINE 22 COLUMN 26 VALUE " C"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Exceptions           ".
               05  LINE 23 COLUMN 26 VALUE " D"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Redundant Stock      ".
               05  LINE 24 COLUMN 26 VALUE " E"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Stock per Supplier   ".
               05  LINE 25 COLUMN 26 VALUE " F"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Price Catalogue      ".
               05  LINE 26 COLUMN 26 VALUE " 0"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Exit                 ".
               05  LINE 27 COLUMN 26 VALUE "                        ".

       01  S09.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Black.
               05  LINE 10 COLUMN 26 VALUE "                        ".
               05  LINE 11 COLUMN 26 VALUE "  STOCK EXPORT/IMPORT   " FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 12 COLUMN 26 VALUE " 1"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Transfers In         ".
               05  LINE 13 COLUMN 26 VALUE " 2"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Transfers Out        ".
               05  LINE 14 COLUMN 26 VALUE " 3"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Prices - Branch (In) ".
               05  LINE 15 COLUMN 26 VALUE " 4"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Prices - Branch(Out) ".
               05  LINE 16 COLUMN 26 VALUE " 5"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Export Options       ".
               05  LINE 17 COLUMN 26 VALUE " 6"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Import Options       ".
               05  LINE 18 COLUMN 26 VALUE " 0"                       FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Exit                 ".
               05  LINE 19 COLUMN 26 VALUE "                        ".

       01  S10.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE 12 COLUMN 48 VALUE "                       ".
               05  LINE 13 COLUMN 48 VALUE "  WAREHOUSE ROUTINES   " FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 14 COLUMN 48 VALUE " 1"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 50 VALUE   " Add a new Location  ".
               05  LINE 15 COLUMN 48 VALUE " 2"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 50 VALUE   " Amend a Location    ".
               05  LINE 16 COLUMN 48 VALUE " 3"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 50 VALUE   " Delete a Location   ".
               05  LINE 17 COLUMN 48 VALUE " 4"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 50 VALUE   " Stock Transfers     ".
               05  LINE 18 COLUMN 48 VALUE " 5"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 50 VALUE   " Add Stock Records   ".
               05  LINE 19 COLUMN 48 VALUE " 6"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 50 VALUE   " Location Report     ".
               05  LINE 20 COLUMN 48 VALUE " 7"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 50 VALUE   " Enquiries           ".
               05  LINE 21 COLUMN 48 VALUE " 0"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 50 VALUE   " Exit                ".
               05  LINE 22 COLUMN 48 VALUE "                       ".

       01  SD-PW.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 15 COLUMN 28 VALUE "                         ". 
               05  LINE 16 COLUMN 28 VALUE " Enter Password          ".
               05  LINE 17 COLUMN 28 VALUE "                         ".
           
       01  SD-NPW.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 15 COLUMN 29 VALUE "                       ".
               05  LINE 16 COLUMN 29 VALUE " New Password          ".
               05  LINE 17 COLUMN 29 VALUE "                       ".
   
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
             PERFORM ZA000-INIT.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66               TO MOUSE-FUNC
               MOVE 1                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM
               END-IF.
           IF LS0-STP NOT = 2
               MOVE "Not Installed"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
           ELSE
           IF LS0-STOCK = "N"
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
           ELSE
               PERFORM BA000.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66               TO MOUSE-FUNC
               MOVE 0                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
             CLOSE CARDEX CREDIT LEDTRF PORDER PURDEX SPARTS STKALT STKDEX STKMEM STOCK WARHSE WSTOCK.
       AA99.
             EXIT PROGRAM.

       AA100        SECTION.
       AA101.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66               TO MOUSE-FUNC
               MOVE 0                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM
           END-IF.
             PERFORM SAVE-SCREEN-3.
             MOVE SPACES             TO LS-PRG-NAME.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Strip the path from the program name and put the program  Ё
      *    Ё name into LS-PRG-NAME.                                    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
             UNSTRING PRG-NAME DELIMITED BY "\" OR " " INTO WS-STAT-MESSAGE LS-PRG-NAME.
             CALL PRG-NAME USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS
                  ON EXCEPTION GO TO AA110
             END-CALL.
             CANCEL PRG-NAME.

       AA105.
             PERFORM RESTORE-SCREEN-3.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66               TO MOUSE-FUNC
               MOVE 1                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM
           END-IF.
             GO TO AA199.

       AA110.
             MOVE SPACE              TO WS-ERR-MES.
             STRING "Program- " DELIMITED SIZE LS-PRG-NAME DELIMITED " " " not on disk, press ANY key" DELIMITED SIZE INTO WS-ERR-MES.
             MOVE W43-SCREEN         TO W43-SCREEN2.
             MOVE W42-ATTRIB         TO W42-ATTRIB2.
             MOVE 30                 TO WS-MES-LINE.
             PERFORM ERROR-MESSAGE.
             MOVE W43-SCREEN2        TO W43-SCREEN.
             MOVE W42-ATTRIB2        TO W42-ATTRIB.
             GO TO AA105.

       AA199.
             EXIT.

       COPY "AA900.ALM".

      *    
      *            ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *           S C R E E N ,   K E Y B O A R D   &   M O U S E
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё      SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3       Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё                      SCREEN-SHADOW                        Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To routine is used to display a shadow down the right and Ё
      *    Ё along the bottom of a pop-up box. The parameters that are Ё
      *    Ё required:                                                 Ё
      *    Ё          SHADE-ROW   - Top line of the box.               Ё
      *    Ё          SHADE-COL   - Left line of box.                  Ё
      *    Ё          SHADE-WIDTH - Width of the box.                  Ё
      *    Ё          SHADE-LINES - Height of box.                     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "FUNCTION.SCR".

      *           T H I S   R O U T I N E   I S   U S E D   T O
      *            D I S P L A Y   E R R O R   M E S S A G E S
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                      ERROR-MESSAGE                        Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To display the error message higher or lower (default is  Ё
      *    Ё line 20) Move the line number which must be used as the   Ё
      *    Ё top line to WS-MES-LINE. The message to be displayed must Ё
      *    Ё be in WS-ERR-MES. PERFORM ERROR-MESSAGE.                  Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё                  ERROR-MESSAGE (2 LINES)                  Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё See above for details of the first line and the second    Ё
      *    Ё line of the message must be in WS-ERR-MES-2.              Ё
      *    Ё               PERFORM ERROR-MESSAGE-2.                    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "ERROR.SCR".

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё           ERASE SCREEN FROM SPECIFIED LOCATION            Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё Uses CRT-START as starting line and increases and CRT-END Ё
      *    Ё as the ending line. The screen is cleared with Column 1   Ё
      *    Ё and 80 containing "Ё" and columns 2 to 79 containing      Ё
      *    Ё spaces.                                                   Ё
      *    Ё                                                           Ё
      *    Ё  eg.                                                      Ё
      *    Ё   MOVE 8         TO CRT-START.                            Ё
      *    Ё   MOVE 28        TO CRT-END.                              Ё
      *    Ё   PERFORM ERASE-SCREEN.                                   Ё
      *    Ё                                                           Ё
      *    Ё To clear the entire screen and Display a new screen with  Ё
      *    Ё headings (program/date/time/company):                     Ё
      *    Ё                                                           Ё
      *    Ё    CRT-PROGRAM (calling program)                          Ё
      *    Ё    CRT-HEADING (Screen heading)                           Ё
      *    Ё    CRT-COMPANY (Company name)                             Ё
      *    Ё    CRT-TERMINAL (consists of two fields:                  Ё
      *    Ё                  CRT-WRKHD and CRT-WRKST. See CRTHEAD)    Ё
      *    Ё    These fields should be populated at the start of the   Ё
      *    Ё    program.                                               Ё
      *    Ё    PERFORM HEADING-AND-CLEAR-SCREEN.                      Ё
      *    Ё                                                           Ё
      *    Ё To change the screen heading:                             Ё
      *    Ё    MOVE "The new heading" TO CRT-HEADING.                 Ё
      *    Ё    PERFORM CHANGE-HEADING.                                Ё
      *    Ё                                                           Ё
      *    Ё To change the time on the screen:                         Ё
      *    Ё    PERFORM CHANGE-TIME                                    Ё
      *    Ё                                                           Ё
      *    Ё To clear lines 46 and 50:                                 Ё
      *    Ё    PERFORM CLEAR-L46-AND-50                               Ё
      *    Ё                                                           Ё
      *    Ё To clear line 50:                                         Ё
      *    Ё    PERFORM CLEAR-L50                                      Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "CLEAR.CRT".

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                        OPT-MESSAGE                        Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё This routine is used to allow the OPERATOR to respond to  Ё
      *    Ё a request for an option, so that the correct action can   Ё
      *    Ё be performed by the program. The routine will display the Ё
      *    Ё message in a pop-up window and allow the OPERATOR to      Ё
      *    Ё respond to the 'question'.                                Ё
      *    Ё                                                           Ё
      *    Ё The option request must be placed in WS-OPT-MES and may   Ё
      *    Ё not exceed 48 characters. The message will be centred in  Ё
      *    Ё the window. An example of a message request follows:      Ё
      *    Ё                                                           Ё
      *    Ё   MOVE "Print transactions (Y/N) [ ]" TO WS-OPT-MES.      Ё
      *    Ё   MOVE Instruction    TO WS-INSTR.                        Ё
      *    Ё       [see Accptopt for instruction values]               Ё
      *    Ё   PERFORM OPT-MESSAGE.                                    Ё
      *    Ё                                                           Ё
      *    Ё This would be displayed as:                               Ё
      *    Ё      здддддддддддддддддддддддддддддддддддддддддддддддд©   Ё
      *    Ё      Ё          Print transactions (Y/N) [ ]          Ё╟╟ Ё
      *    Ё      юдддддддддддддддддддддддддддддддддддддддддддддддды╟╟ Ё
      *    Ё        ╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟ Ё
      *    Ё                                                           Ё
      *    Ё The response is returned in WS-OPTION (in upper case).    Ё
      *    Ё                                                           Ё
      *    Ё The system will display the message box with the top line Ё
      *    Ё as the value of WS-MES-LINE. If WS-MES-LINE is zero the   Ё
      *    Ё default will be 20.                                       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "OPTION.CRT".

      *    *************************************************************
      *    ****    M A I N   S T O C K  P R O G R A M  M E N U    ****
      *    *************************************************************
       BA000        SECTION 5.
       BA00.
             MOVE 8                  TO SHADE-ROW.
             MOVE 7                  TO SHADE-COL.
             MOVE 20                 TO SHADE-WIDTH.
             MOVE 19                 TO SHADE-LINES.
             DISPLAY S02.
           IF LS0-LEV < 2
               DISPLAY ".(N/A)"           AT 1221 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Green.
           IF LS0-LEV < 3
               DISPLAY "Enq. (Ord) (N/A)" AT 1510 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Green
               DISPLAY ".(N/A)"           AT 1620 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Green
               DISPLAY ".N/A"             AT 1722 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Green
               DISPLAY "(N/A)"            AT 2021 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Green
               DISPLAY "(N/A)"            AT 2121 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Green
               DISPLAY ".N/A"             AT 2222 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Green
               DISPLAY " (N/A)"           AT 2320 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Green.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.

       BA02.
             MOVE 9                  TO TOP-ROW.
             MOVE 24                 TO BOTTOM-ROW.
             MOVE 8                  TO SCREEN-LIN.
             MOVE 8                  TO SCREEN-COL.
             MOVE 17                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       BA05.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
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
                                 IF MOUSE-Y = 49
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
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO BA05
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE "0"  TO WS-OPTION
                                 GO TO BA18
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO BA05
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X      TO WS-OPTION
               CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS
               IF (WS-OPTION < 0 OR > 8) AND (WS-OPTION < "A" OR > "G")
                   PERFORM AA900-ALARM
                   GO TO BA05
               END-IF
               GO TO BA16
           ELSE
               PERFORM AA900-ALARM
               GO TO BA05.

       BA15.
             EVALUATE SCREEN-LIN
               WHEN 9     MOVE "1"   TO WS-OPTION
               WHEN 10    MOVE "2"   TO WS-OPTION
               WHEN 11    MOVE "3"   TO WS-OPTION
               WHEN 12    MOVE "4"   TO WS-OPTION
               WHEN 13    MOVE "5"   TO WS-OPTION
               WHEN 14    MOVE "6"   TO WS-OPTION
               WHEN 15    MOVE "7"   TO WS-OPTION
               WHEN 16    MOVE "8"   TO WS-OPTION
               WHEN 17    MOVE "A"   TO WS-OPTION
               WHEN 18    MOVE "B"   TO WS-OPTION
               WHEN 19    MOVE "C"   TO WS-OPTION
               WHEN 20    MOVE "D"   TO WS-OPTION
               WHEN 21    MOVE "E"   TO WS-OPTION
               WHEN 22    MOVE "F"   TO WS-OPTION
               WHEN 23    MOVE "G"   TO WS-OPTION
               WHEN 24    MOVE "0"   TO WS-OPTION
               WHEN OTHER PERFORM AA900-ALARM
                          GO TO BA05
             END-EVALUATE.
             GO TO BA18.

       BA16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "0"  MOVE 24     TO SCREEN-LIN
               WHEN "1"  MOVE 9      TO SCREEN-LIN
               WHEN "2"  MOVE 10     TO SCREEN-LIN
               WHEN "3"  MOVE 11     TO SCREEN-LIN
               WHEN "4"  MOVE 12     TO SCREEN-LIN
               WHEN "5"  MOVE 13     TO SCREEN-LIN
               WHEN "6"  MOVE 14     TO SCREEN-LIN
               WHEN "7"  MOVE 15     TO SCREEN-LIN
               WHEN "8"  MOVE 16     TO SCREEN-LIN
               WHEN "A"  MOVE 17     TO SCREEN-LIN
               WHEN "B"  MOVE 18     TO SCREEN-LIN
               WHEN "C"  MOVE 19     TO SCREEN-LIN
               WHEN "D"  MOVE 20     TO SCREEN-LIN
               WHEN "E"  MOVE 21     TO SCREEN-LIN
               WHEN "F"  MOVE 22     TO SCREEN-LIN
               WHEN "G"  MOVE 23     TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       BA18.
             PERFORM SAVE-SCREEN.
             MOVE BLOCK-DETAIL       TO BLOCK-DETAIL1.
             MOVE SHADOW-DETAIL      TO SHADOW-DETAIL1.
             MOVE CRT-DETAIL         TO CRT-DETAIL1.
             EVALUATE WS-OPTION
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                          E X I T                          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "0"  MOVE "N"   TO LS-OK
                         GO TO BA999
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                        Maintenance                        Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "1"  PERFORM CA000
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                          Reports                          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "2"  PERFORM DA000
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                      Export Options                       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "3"  PERFORM EA000
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                        Enquiries                          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "4"  PERFORM FA000
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                       Login/Logout                        Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "5"  PERFORM GA000
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                    Enquiries (Orders)                     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "6"  PERFORM CG000
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                      Sales Tracking                       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "7"  PERFORM CH000
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                     Purchase Tracking                     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "8"  PERFORM CL000
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                    Adjustment Tracking                    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "A"  PERFORM CP000
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                       Price Labels                        Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "B"  PERFORM CK000
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                      Barcode Labels                       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "C"  PERFORM CM000
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                      Barcode System                       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "D"  PERFORM CN000
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                       Stock Options                       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "E"  PERFORM CO000
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                    Promotions/Specials                    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "F"  PERFORM CQ000
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                         Utilities                         Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "G"  PERFORM CR000
             END-EVALUATE.
             PERFORM RESTORE-SCREEN.
             MOVE BLOCK-DETAIL1      TO BLOCK-DETAIL.
             MOVE SHADOW-DETAIL1     TO SHADOW-DETAIL.
             MOVE CRT-DETAIL1        TO CRT-DETAIL.
           IF LS-OK = "Y"
               DISPLAY "Logged In" AT 0502 WITH FOREGROUND-COLOR Brown HIGHLIGHT BLINK
           ELSE
               DISPLAY "         " AT 0502 WITH FOREGROUND-COLOR Cyan BACKGROUND-COLOR Blue.
             DISPLAY " " AT 5069 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             GO TO BA05.

       BA999.
             EXIT.
      *    *************************************************************
      *    ****          S T O C K   M A I N T E N A N C E          ****
      *    *************************************************************
       CA000        SECTION 5.
       CA00.
           IF LS-DSKDRV = "L"
               MOVE "Not available on Local drive" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CA999.
             MOVE 10                 TO SHADE-ROW.
             MOVE 26                 TO SHADE-COL.
             MOVE 24                 TO SHADE-WIDTH.
             MOVE 12                 TO SHADE-LINES.
             DISPLAY S03.
           IF LS0-LEV < 3
               DISPLAY "Production (N/A)   " AT 1429 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Cyan
               DISPLAY "Enq. (Orders) (N/A)" AT 1729 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Cyan
               DISPLAY "Chg Prc Names (N/A)" AT 1829 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Cyan.
           IF LS0-LEV < 4
               DISPLAY "W/House Maint (N/A)" AT 1929 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Cyan.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
             MOVE 11                 TO TOP-ROW.
             MOVE 19                 TO BOTTOM-ROW.
             MOVE 10                 TO SCREEN-LIN.
             MOVE 27                 TO SCREEN-COL.
             MOVE 21                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       CA05.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       CA10.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     PERFORM MOVE-BLOCK-UP
                 WHEN DOWN-KEY   PERFORM MOVE-BLOCK-DOWN
                 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
                                 IF MOUSE-ENTER = "Y"
                                     MOVE "N"        TO MOUSE-ENTER
                                     GO TO CA15
                                 END-IF
                                 IF MOUSE-Y = 49
                                     IF MOUSE-X = 5
                                         MOVE 5      TO KEY-CODE-1
                                         GO TO CA10
                                     ELSE
                                     IF MOUSE-X = 10
                                         MOVE 6      TO KEY-CODE-1
                                         GO TO CA10
                                     ELSE
                                     IF MOUSE-X > 36 AND < 44
                                         MOVE 0      TO KEY-CODE-1
                                         GO TO CA10
                                     ELSE
                                     IF MOUSE-X > 54 AND < 60
                                         MOVE 0  TO KEY-CODE-1
                                         MOVE 1  TO KEY-TYPE
                                         GO TO CA10
                                     END-IF
                                 END-IF
                 WHEN ENTER-KEY  GO TO CA15
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO CA05
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE "0"  TO WS-OPTION
                                 GO TO CA18
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO CA05
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X      TO WS-OPTION
               IF WS-OPTION < 0 OR > 8
                   PERFORM AA900-ALARM
                   GO TO CA05
               END-IF
               GO TO CA16
           ELSE
               PERFORM AA900-ALARM
               GO TO CA05.

       CA15.
             EVALUATE SCREEN-LIN
               WHEN 11     MOVE "1"  TO WS-OPTION
               WHEN 12     MOVE "2"  TO WS-OPTION
               WHEN 13     MOVE "3"  TO WS-OPTION
               WHEN 14     MOVE "4"  TO WS-OPTION
               WHEN 15     MOVE "5"  TO WS-OPTION
               WHEN 16     MOVE "6"  TO WS-OPTION
               WHEN 17     MOVE "7"  TO WS-OPTION
               WHEN 18     MOVE "8"  TO WS-OPTION
               WHEN 19     MOVE "0"  TO WS-OPTION
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO CA05
             END-EVALUATE.
             GO TO CA18.

       CA16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "0"    MOVE 19   TO SCREEN-LIN
               WHEN "1"    MOVE 11   TO SCREEN-LIN
               WHEN "2"    MOVE 12   TO SCREEN-LIN
               WHEN "3"    MOVE 13   TO SCREEN-LIN
               WHEN "4"    MOVE 14   TO SCREEN-LIN
               WHEN "5"    MOVE 15   TO SCREEN-LIN
               WHEN "6"    MOVE 16   TO SCREEN-LIN
               WHEN "7"    MOVE 17   TO SCREEN-LIN
               WHEN "8"    MOVE 18   TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       CA18.
             PERFORM SAVE-SCREEN-2.
             MOVE BLOCK-DETAIL  TO BLOCK-DETAIL2.
             MOVE SHADOW-DETAIL  TO SHADOW-DETAIL2.
             MOVE CRT-DETAIL  TO CRT-DETAIL2.
             EVALUATE WS-OPTION
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                          E X I T                          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "0"    GO TO CA999
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                     Record Maintenance                    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "1"    PERFORM CB000
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                     Price/Quant Adjust                    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "2"    PERFORM CC000
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                     Production Routines                   Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "3"    PERFORM CD000
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                         Month-End                         Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "4"    PERFORM CE000
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                         Enquiries                         Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "5"    PERFORM CF000
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                     Enquiries (Orders)                    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "6"    PERFORM CG000
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                  Description/Price Names                  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "7"    PERFORM CI000
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                   Warehouse Maintenance                   Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "8"    PERFORM CJ000
             END-EVALUATE.
             PERFORM RESTORE-SCREEN-2.
             MOVE BLOCK-DETAIL2      TO BLOCK-DETAIL.
             MOVE SHADOW-DETAIL2     TO SHADOW-DETAIL.
             MOVE CRT-DETAIL2        TO CRT-DETAIL.
             GO TO CA05.

       CA999.
             EXIT.
      *    *************************************************************
      *    ****  R E C O R D   M A I N T E N A N C E     ****
      *    *************************************************************
       CB000        SECTION 5.
       CB00.
             MOVE 12                 TO SHADE-ROW.
             MOVE 48                 TO SHADE-COL.
             MOVE 23                 TO SHADE-WIDTH.
             MOVE 11                 TO SHADE-LINES.
             DISPLAY S04.
           IF LS0-LEV < 4
               DISPLAY "N/A" AT 1668 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
           IF LS0-LEV < 3
               DISPLAY "N/A " AT 1866 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
             MOVE 13                 TO TOP-ROW.
             MOVE 20                 TO BOTTOM-ROW.
             MOVE 12                 TO SCREEN-LIN.
             MOVE 49                 TO SCREEN-COL.
             MOVE 20                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       CB05.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       CB10.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     PERFORM MOVE-BLOCK-UP
                 WHEN DOWN-KEY   PERFORM MOVE-BLOCK-DOWN
                 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
                                 IF MOUSE-ENTER = "Y"
                                     MOVE "N"      TO MOUSE-ENTER
                                     GO TO CB15
                                 END-IF
                                 IF MOUSE-Y = 49
                                     IF MOUSE-X = 5
                                         MOVE 5    TO KEY-CODE-1
                                         GO TO CB10
                                     ELSE
                                     IF MOUSE-X = 10
                                         MOVE 6    TO KEY-CODE-1
                                         GO TO CB10
                                     ELSE
                                     IF MOUSE-X > 36 AND < 44
                                         MOVE 0    TO KEY-CODE-1
                                         GO TO CB10
                                     ELSE
                                     IF MOUSE-X > 54 AND < 60
                                         MOVE 0    TO KEY-CODE-1
                                         MOVE 1    TO KEY-TYPE
                                         GO TO CB10
                                     END-IF
                                 END-IF
                 WHEN ENTER-KEY  GO TO CB15
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO CB05
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE "0"  TO WS-OPTION
                                 GO TO CB18
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO CB05
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X  TO WS-OPTION
               IF WS-OPTION < 0 OR > 7
                   PERFORM AA900-ALARM
                   GO TO CB05
               END-IF
               GO TO CB16
           ELSE
               PERFORM AA900-ALARM
               GO TO CB05.

       CB15.
             EVALUATE SCREEN-LIN
               WHEN 13     MOVE "1"   TO WS-OPTION
               WHEN 14     MOVE "2"   TO WS-OPTION
               WHEN 15     MOVE "3"   TO WS-OPTION
               WHEN 16     MOVE "4"   TO WS-OPTION
               WHEN 17     MOVE "5"   TO WS-OPTION
               WHEN 18     MOVE "6"   TO WS-OPTION
               WHEN 19     MOVE "7"   TO WS-OPTION
               WHEN 20     MOVE "0"   TO WS-OPTION
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO CB05
             END-EVALUATE.
             GO TO CB18.

       CB16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "0"    MOVE 20   TO SCREEN-LIN
               WHEN "1"    MOVE 13   TO SCREEN-LIN
               WHEN "2"    MOVE 14   TO SCREEN-LIN
               WHEN "3"    MOVE 15   TO SCREEN-LIN
               WHEN "4"    MOVE 16   TO SCREEN-LIN
               WHEN "5"    MOVE 17   TO SCREEN-LIN
               WHEN "6"    MOVE 18   TO SCREEN-LIN
               WHEN "7"    MOVE 19   TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       CB18.
             EVALUATE WS-OPTION
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                          E X I T                          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "0"    GO TO CB999
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                     Create New Record                     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "1"    MOVE "STP\STP001"      TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                   Amend Existing Record                   Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "2"    MOVE "STP\STP002"      TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                      Change Item Code                     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "3"    IF LS0-LEV < 4
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Not available in this version               Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
                               MOVE "MNU\NOPMENU" TO PRG-NAME
                           ELSE
                               MOVE "STP\STP003"  TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                    Delete Stock Record                    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "4"    MOVE "STP\STP004"      TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                    Flag Redundant Item                    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "5"    IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                Not available in this version              Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
                               MOVE "MNU\NOPMENU" TO PRG-NAME
                           ELSE
                               MOVE "STP\STP005"  TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                    Amend Supplier Code                    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "6"    MOVE "STP\STP006"      TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                         Enquiries                         Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "7"    MOVE "STP\STPENQ"      TO PRG-NAME
             END-EVALUATE.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.
             GO TO CB05.

       CB999.
             EXIT.
      *    *************************************************************
      *    *  P R I C E   &   Q U A N T I T Y A D J U S T M E N T S  *
      *    *************************************************************
       CC000        SECTION 5.
       CC00.
             MOVE 12                 TO SHADE-ROW.
             MOVE 48                 TO SHADE-COL.
             MOVE 25                 TO SHADE-WIDTH.
             MOVE 12                 TO SHADE-LINES.
             DISPLAY S05.
           IF LS0-LEV < 3
               DISPLAY "Quant per Dept. N/A " AT 1852 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
               DISPLAY "N/A"                  AT 1968 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
             MOVE 13                 TO TOP-ROW.
             MOVE 21                 TO BOTTOM-ROW.
             MOVE 12                 TO SCREEN-LIN.
             MOVE 50                 TO SCREEN-COL.
             MOVE 21                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       CC05.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       CC10.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     PERFORM MOVE-BLOCK-UP
                 WHEN DOWN-KEY   PERFORM MOVE-BLOCK-DOWN
                 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
                                 IF MOUSE-ENTER = "Y"
                                     MOVE "N"      TO MOUSE-ENTER
                                     GO TO CC15
                                 END-IF
                                 IF MOUSE-Y = 49
                                     IF MOUSE-X = 5
                                         MOVE 5    TO KEY-CODE-1
                                         GO TO CC10
                                     ELSE
                                     IF MOUSE-X = 10
                                         MOVE 6    TO KEY-CODE-1
                                         GO TO CC10
                                     ELSE
                                     IF MOUSE-X > 36 AND < 44
                                         MOVE 0    TO KEY-CODE-1
                                         GO TO CC10
                                     ELSE
                                     IF MOUSE-X > 54 AND < 60
                                         MOVE 0    TO KEY-CODE-1
                                         MOVE 1    TO KEY-TYPE
                                         GO TO CC10
                                     END-IF
                                 END-IF
                 WHEN ENTER-KEY  GO TO CC15
                 WHEN OTHER  PERFORM AA900-ALARM
               END-EVALUATE
               GO TO CC05
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE "0"  TO WS-OPTION
                                 GO TO CC18
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO CC05
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X      TO WS-OPTION
               IF WS-OPTION < 0 OR > 8
                   PERFORM AA900-ALARM
                   GO TO CC05
               END-IF
               GO TO CC16
           ELSE
               PERFORM AA900-ALARM
               GO TO CC05.

       CC15.
             EVALUATE SCREEN-LIN
               WHEN 13     MOVE "1"  TO WS-OPTION
               WHEN 14     MOVE "2"  TO WS-OPTION
               WHEN 15     MOVE "3"  TO WS-OPTION
               WHEN 16     MOVE "4"  TO WS-OPTION
               WHEN 17     MOVE "5"  TO WS-OPTION
               WHEN 18     MOVE "6"  TO WS-OPTION
               WHEN 19     MOVE "7"  TO WS-OPTION
               WHEN 20     MOVE "8"  TO WS-OPTION
               WHEN 21     MOVE "0"  TO WS-OPTION
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO CC05
             END-EVALUATE.
             GO TO CC18.

       CC16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "0"    MOVE 21   TO SCREEN-LIN
               WHEN "1"    MOVE 13   TO SCREEN-LIN
               WHEN "2"    MOVE 14   TO SCREEN-LIN
               WHEN "3"    MOVE 15   TO SCREEN-LIN
               WHEN "4"    MOVE 16   TO SCREEN-LIN
               WHEN "5"    MOVE 17   TO SCREEN-LIN
               WHEN "6"    MOVE 18   TO SCREEN-LIN
               WHEN "7"    MOVE 19   TO SCREEN-LIN
               WHEN "8"    MOVE 20   TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       CC18.
             EVALUATE WS-OPTION
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                          E X I T                          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "0"    GO TO CC999
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                       Price Changes                       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "1"    MOVE "STP\STP011"       TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                 Price Changes (Department)                Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "2"    MOVE "STP\STP012"       TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                  Price Adjust (Department)                Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "3"    MOVE "STP\STP013"       TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                      Stock Quantities                     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "4"    MOVE "STP\STP014"       TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                  Quantities per Department                Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "5"    IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                Not available in this version              Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
                               MOVE "MNU\NOPMENU"  TO PRG-NAME
                           ELSE
                               MOVE "STP\STP015"   TO PRG-NAME
                           END-IF
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Adjust Lead-Time/ReOrder levels             Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "6"    IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                Not available in this version              Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
                               MOVE "MNU\NOPMENU"  TO PRG-NAME
                           ELSE
                               MOVE "STP\STP016"   TO PRG-NAME
                           END-IF
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                         Enquiries                         Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "7"    MOVE "STP\STPENQ"       TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                      Adjust Quantities                    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "8"    MOVE "STP\STP018"       TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                       I N V A L I D                       Ё
      *    Ё   Added test as a precaution. It should never happen.     Ё   
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO CC05
             END-EVALUATE.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.
             GO TO CC05.

       CC999.
             EXIT.
      *    *************************************************************
      *    ****  P R O D U C T I O N   O P T I O N S     ****
      *    *************************************************************
       CD000        SECTION 5.
       CD00.
           IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Not available in this version               Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               MOVE "MNU\NOPMENU"    TO PRG-NAME
               PERFORM AA100
               DISPLAY MENU-INSTRUCT
               GO TO CD999.
             MOVE 12                 TO SHADE-ROW.
             MOVE 48                 TO SHADE-COL.
             MOVE 23                 TO SHADE-WIDTH.
             MOVE 11                 TO SHADE-LINES.
             DISPLAY S06.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
             MOVE 13                 TO TOP-ROW.
             MOVE 20                 TO BOTTOM-ROW.
             MOVE 12                 TO SCREEN-LIN.
             MOVE 49                 TO SCREEN-COL.
             MOVE 20                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       CD05.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       CD10.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     PERFORM MOVE-BLOCK-UP
                 WHEN DOWN-KEY   PERFORM MOVE-BLOCK-DOWN
                 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
                                 IF MOUSE-ENTER = "Y"
                                     MOVE "N"  TO MOUSE-ENTER
                                     GO TO CD15
                                 END-IF
                                 IF MOUSE-Y = 49
                                     IF MOUSE-X = 5
                                         MOVE 5  TO KEY-CODE-1
                                         GO TO CD10
                                     ELSE
                                     IF MOUSE-X = 10
                                         MOVE 6  TO KEY-CODE-1
                                         GO TO CD10
                                     ELSE
                                     IF MOUSE-X > 36 AND < 44
                                         MOVE 0  TO KEY-CODE-1
                                         GO TO CD10
                                     ELSE
                                     IF MOUSE-X > 54 AND < 60
                                         MOVE 0  TO KEY-CODE-1
                                         MOVE 1  TO KEY-TYPE
                                         GO TO CD10
                                     END-IF
                                 END-IF
                 WHEN ENTER-KEY  GO TO CD15
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO CD05
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE "0"  TO WS-OPTION
                                 GO TO CD18
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO CD05
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X      TO WS-OPTION
               IF WS-OPTION < 0 OR > 7
                   PERFORM AA900-ALARM
                   GO TO CD05
               END-IF
               GO TO CD16
           ELSE
               PERFORM AA900-ALARM
               GO TO CD05.

       CD15.
             EVALUATE SCREEN-LIN
               WHEN 13     MOVE "1"  TO WS-OPTION
               WHEN 14     MOVE "2"  TO WS-OPTION
               WHEN 15     MOVE "3"  TO WS-OPTION
               WHEN 16     MOVE "4"  TO WS-OPTION
               WHEN 17     MOVE "5"  TO WS-OPTION
               WHEN 18     MOVE "6"  TO WS-OPTION
               WHEN 19     MOVE "7"  TO WS-OPTION
               WHEN 20     MOVE "0"  TO WS-OPTION
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO CD05
             END-EVALUATE.
             GO TO CD18.

       CD16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "0"    MOVE 20   TO SCREEN-LIN
               WHEN "1"    MOVE 13   TO SCREEN-LIN
               WHEN "2"    MOVE 14   TO SCREEN-LIN
               WHEN "3"    MOVE 15   TO SCREEN-LIN
               WHEN "4"    MOVE 16   TO SCREEN-LIN
               WHEN "5"    MOVE 17   TO SCREEN-LIN
               WHEN "6"    MOVE 18   TO SCREEN-LIN
               WHEN "7"    MOVE 19   TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       CD18.
             EVALUATE WS-OPTION
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                          E X I T                          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "0"    GO TO CD999
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                    Change to Produced                     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "1"    MOVE "STP\STP021" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё              Use Recorded Details (Template)              Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "2"    MOVE "STP\STP022" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                Key the Details - all Items                Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "3"    MOVE "STP\STP023" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Key in the Details - per Item               Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "4"    MOVE "STP\STP024" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                 Update Production Prices                  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "5"    MOVE "STP\STP025" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                         Enquiries                         Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "6"    MOVE "STP\STPENQ" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                 Change to NOT Produced                    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "7"    MOVE "STP\STP026" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                       I N V A L I D                       Ё
      *    Ё   Added test as a precaution. It should never happen.     Ё   
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO CD05
             END-EVALUATE.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.
             GO TO CD05.

       CD999.
             EXIT.
      *    *************************************************************
      *    ****                  U T I L I T I E S                  ****
      *    *************************************************************
       CE000        SECTION 5.
       CE00.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                     Month-end Routine                     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
             MOVE "STP\STP091"       TO PRG-NAME.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.

       CE999.
             EXIT.
      *    *************************************************************
      *    ****                  E N Q U I R I E S                  ****
      *    *************************************************************
       CF000        SECTION 5.
       CF00.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                         Enquiries                         Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
             MOVE "STP\STPENQ"       TO PRG-NAME.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.

       CF999.
             EXIT.
      *    *************************************************************
      *    ****         E N Q U I R I E S   ( O R D E R S )         ****
      *    *************************************************************
       CG000        SECTION 5.
       CG00.
           IF LS-DSKDRV = "L"
               MOVE "Not available on Local drive" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CG999.
           IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Not available in this version               Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               MOVE "MNU\NOPMENU"    TO PRG-NAME
               PERFORM AA100
               DISPLAY MENU-INSTRUCT
               GO TO CG999.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                    Enquiries (Orders)                     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
             MOVE "STP\STPORD"       TO PRG-NAME.
             PERFORM AA100.
             CLOSE CREDIT.
             OPEN I-O CREDIT.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Merge and Print Purchase Orders             Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
             MOVE "STP\ORDPRN"       TO PRG-NAME.
             PERFORM AA100.
             CLOSE CREDIT.
             OPEN INPUT CREDIT.
             DISPLAY MENU-INSTRUCT.

       CG999.
             EXIT.
      *    *************************************************************
      *    ****             S A L E S   T R A C K I N G             ****
      *    *************************************************************
       CH000        SECTION 5.
       CH00.
           IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                Not available in this version              Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               MOVE "MNU\NOPMENU"    TO PRG-NAME
           ELSE
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                       Sales Tracking                      Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               MOVE "STP\STPSAL"     TO PRG-NAME.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.

       CH999.
             EXIT.
      *    *************************************************************
      *    ****    D E S C R I P T I O N / P R I C E   N A M E S    ****
      *    *************************************************************
       CI000        SECTION 5.
       CI00.
           IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                Not available in this version              Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               MOVE "MNU\NOPMENU"    TO PRG-NAME
           ELSE
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                  Description/Price Names                  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               MOVE "STP\STP017"     TO PRG-NAME.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.

       CI999.
             EXIT.
      *    *************************************************************
      *    ****          W A R E H O U S E   O P T I O N S          ****
      *    *************************************************************
       CJ000        SECTION 5.
       CJ00.
           IF LS0-LEV < 4
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                Not available in this version              Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               MOVE "MNU\NOPMENU"    TO PRG-NAME
               PERFORM AA100
               DISPLAY MENU-INSTRUCT
               GO TO CJ999.
             MOVE 12                 TO SHADE-ROW.
             MOVE 48                 TO SHADE-COL.
             MOVE 23                 TO SHADE-WIDTH.
             MOVE 11                 TO SHADE-LINES.
             DISPLAY S10.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
             MOVE 13                 TO TOP-ROW.
             MOVE 20                 TO BOTTOM-ROW.
             MOVE 12                 TO SCREEN-LIN.
             MOVE 49                 TO SCREEN-COL.
             MOVE 20                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       CJ05.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       CJ10.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     PERFORM MOVE-BLOCK-UP
                 WHEN DOWN-KEY   PERFORM MOVE-BLOCK-DOWN
                 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
                                 IF MOUSE-ENTER = "Y"
                                     MOVE "N"  TO MOUSE-ENTER
                                     GO TO CJ15
                                 END-IF
                                 IF MOUSE-Y = 49
                                     IF MOUSE-X = 5
                                         MOVE 5  TO KEY-CODE-1
                                         GO TO CJ10
                                     ELSE
                                     IF MOUSE-X = 10
                                         MOVE 6  TO KEY-CODE-1
                                         GO TO CJ10
                                     ELSE
                                     IF MOUSE-X > 36 AND < 44
                                         MOVE 0  TO KEY-CODE-1
                                         GO TO CJ10
                                     ELSE
                                     IF MOUSE-X > 54 AND < 60
                                         MOVE 0  TO KEY-CODE-1
                                         MOVE 1  TO KEY-TYPE
                                         GO TO CJ10
                                     END-IF
                                 END-IF
                 WHEN ENTER-KEY  GO TO CJ15
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO CJ05
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE "0"  TO WS-OPTION
                                 GO TO CJ18
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO CJ05
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X  TO WS-OPTION
               IF WS-OPTION < 0 OR > 7
                   PERFORM AA900-ALARM
                   GO TO CJ05
               END-IF
               GO TO CJ16
           ELSE
               PERFORM AA900-ALARM
               GO TO CJ05.

       CJ15.
             EVALUATE SCREEN-LIN
               WHEN 13     MOVE "1"  TO WS-OPTION
               WHEN 14     MOVE "2"  TO WS-OPTION
               WHEN 15     MOVE "3"  TO WS-OPTION
               WHEN 16     MOVE "4"  TO WS-OPTION
               WHEN 17     MOVE "5"  TO WS-OPTION
               WHEN 18     MOVE "6"  TO WS-OPTION
               WHEN 19     MOVE "7"  TO WS-OPTION
               WHEN 20     MOVE "0"  TO WS-OPTION
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO CJ05
             END-EVALUATE.
             GO TO CJ18.

       CJ16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "0"    MOVE 20   TO SCREEN-LIN
               WHEN "1"    MOVE 13   TO SCREEN-LIN
               WHEN "2"    MOVE 14   TO SCREEN-LIN
               WHEN "3"    MOVE 15   TO SCREEN-LIN
               WHEN "4"    MOVE 16   TO SCREEN-LIN
               WHEN "5"    MOVE 17   TO SCREEN-LIN
               WHEN "6"    MOVE 18   TO SCREEN-LIN
               WHEN "7"    MOVE 19   TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       CJ18.
             EVALUATE WS-OPTION
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                          E X I T                          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "0"    GO TO CJ999
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Add a new Location (Warehouse)              Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "1"    MOVE "STP\STP061" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                     Amend a Location                      Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "2"    MOVE "STP\STP062" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                     Delete a Location                     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "3"    MOVE "STP\STP063" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                Stock Transfers (Warehouse)                Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "4"    MOVE "STP\STP064" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё              Add Stock Records to Warehouse               Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "5"    MOVE "STP\STP065" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                Location Report (Warehouse)                Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "6"    MOVE "STP\STP066" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                        Enquiries                          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "7"    MOVE "STP\STPENQ" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                       I N V A L I D                       Ё
      *    Ё   Added test as a precaution. It should never happen.     Ё   
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO CJ05
             END-EVALUATE.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.
             GO TO CJ05.

       CJ999.
             EXIT.

      *    *************************************************************
      *    ****               P R I C E   L A B E L S               ****
      *    *************************************************************
       CK000        SECTION 5.
       CK00.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                       Price Labels                        Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
             MOVE "STP\STP049"       TO PRG-NAME.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.

       CK999.
             EXIT.
      *    *************************************************************
      *    ****           P U R C H A S E   T R A C K I N G         ****
      *    *************************************************************
       CL000        SECTION 5.
       CL00.
           IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё              Not available in this version                Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               MOVE "MNU\NOPMENU"    TO PRG-NAME
           ELSE
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                      Purchase Tracking                    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               MOVE "STP\STPPUR"     TO PRG-NAME.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.

       CL999.
             EXIT.
      *    *************************************************************
      *    ****              B A R C O D E   L A B E L S            ****
      *    *************************************************************
       CM000        SECTION 5.
       CM00.
           IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё              Not available in this version                Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               MOVE "MNU\NOPMENU"    TO PRG-NAME
           ELSE
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                      Barcode Labels                       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               MOVE "STP\STPBAR"     TO PRG-NAME.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.

       CM999.
             EXIT.
      *    *************************************************************
      *    ****             B A R C O D E   S T Y L E S             ****
      *    *************************************************************
       CN000        SECTION 5.
       CN00.
           IF LS-DSKDRV = "L"
               MOVE "Not available on Local drive" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CN999.
           IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё              Not available in this version                Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               MOVE "MNU\NOPMENU"    TO PRG-NAME
           ELSE
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                      Barcode Styles                       Ё
      *    Ё   UPC-A,UPC-E,JAN13/EAN13,JAN8/EAN8,CODE39,ITF,CODABAR    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               MOVE "UTP\BCSTYLE"    TO PRG-NAME.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.

       CN999.
             EXIT.
      *    *************************************************************
      *    ****              S T O C K   O P T I O N S              ****
      *    *************************************************************
       CO000        SECTION 5.
       CO00.
           IF LS-DSKDRV = "L"
               MOVE "Not available on Local drive" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CO999.
           IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Not available in this version               Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               MOVE "MNU\NOPMENU"    TO PRG-NAME
           ELSE
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Set the various Stock Options               Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               MOVE "STP\STP080"     TO PRG-NAME.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.

       CO999.
             EXIT.
      *    *************************************************************
      *    ****        A D J U S T M E N T   T R A C K I N G        ****
      *    *************************************************************
       CP000        SECTION 5.
       CP00.
           IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё              Not available in this version                Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               MOVE "MNU\NOPMENU"    TO PRG-NAME
           ELSE
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                   Adjustment Tracking                     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               MOVE "STP\STPADJ"     TO PRG-NAME.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.

       CP999.
             EXIT.
      *    *************************************************************
      *    ****    P R O M O T I O N S   A N D   S P E C I A L S    ****
      *    *************************************************************
       CQ000        SECTION 5.
       CQ00.
           IF LS-DSKDRV = "L"
               MOVE "Not available on Local drive" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CQ999.
           IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё              Not available in this version                Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               MOVE "MNU\NOPMENU"    TO PRG-NAME
           ELSE
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                    Promotions/Specials                    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               MOVE "STP\STP085"     TO PRG-NAME.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.

       CQ999.
             EXIT.
      *    *************************************************************
      *    ****                   U T I L I T I E S                 ****
      *    *************************************************************
       CR000        SECTION 5.
       CR00.
           IF LS-DSKDRV = "L"
               MOVE "Not available on Local drive" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CR999.
             MOVE 10                 TO SHADE-ROW.
             MOVE 26                 TO SHADE-COL.
             MOVE 24                 TO SHADE-WIDTH.
             MOVE 13                 TO SHADE-LINES.
             DISPLAY S07.
           IF LS0-LEV < 3
               DISPLAY "N/A"                  AT 0347 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
               DISPLAY "N/A"                  AT 1447 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
               DISPLAY "Clr. Qnt -Dept N/A  " AT 1530 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
               DISPLAY "Del. Itm Range N/A"   AT 1630 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
             MOVE 11                 TO TOP-ROW.
             MOVE 20                 TO BOTTOM-ROW.
             MOVE 10                 TO SCREEN-LIN.
             MOVE 27                 TO SCREEN-COL.
             MOVE 21                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       CR05.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       CR10.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     PERFORM MOVE-BLOCK-UP
                 WHEN DOWN-KEY   PERFORM MOVE-BLOCK-DOWN
                 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
                                 IF MOUSE-ENTER = "Y"
                                     MOVE "N"    TO MOUSE-ENTER
                                     GO TO CR15
                                 END-IF
                                 IF MOUSE-Y = 49
                                     IF MOUSE-X = 5
                                         MOVE 5  TO KEY-CODE-1
                                         GO TO CR10
                                     ELSE
                                     IF MOUSE-X = 10
                                         MOVE 6  TO KEY-CODE-1
                                         GO TO CR10
                                     ELSE
                                     IF MOUSE-X > 36 AND < 44
                                         MOVE 0  TO KEY-CODE-1
                                         GO TO CR10
                                     ELSE
                                     IF MOUSE-X > 54 AND < 60
                                         MOVE 0  TO KEY-CODE-1
                                         MOVE 1  TO KEY-TYPE
                                         GO TO CR10
                                     END-IF
                                 END-IF
                 WHEN ENTER-KEY  GO TO CR15
                 WHEN OTHER  PERFORM AA900-ALARM
               END-EVALUATE
               GO TO CR05
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE "0"  TO WS-OPTION
                                 GO TO CR18
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO CR05
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X      TO WS-OPTION
               CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS
               IF (WS-OPTION < 0 OR > 8) AND (WS-OPTION NOT = "A") AND (WS-OPTION NOT = ">") AND (WS-OPTION NOT = "<") AND (WS-OPTION NOT = "/") AND (WS-OPTION NOT = "!")
                   PERFORM AA900-ALARM
                   GO TO CR05
               END-IF
               GO TO CR16
           ELSE
               PERFORM AA900-ALARM
               GO TO CR05.

       CR15.
             EVALUATE SCREEN-LIN
               WHEN 11     MOVE "1"  TO WS-OPTION
               WHEN 12     MOVE "2"  TO WS-OPTION
               WHEN 13     MOVE "3"  TO WS-OPTION
               WHEN 14     MOVE "4"  TO WS-OPTION
               WHEN 15     MOVE "5"  TO WS-OPTION
               WHEN 16     MOVE "6"  TO WS-OPTION
               WHEN 17     MOVE "7"  TO WS-OPTION
               WHEN 18     MOVE "8"  TO WS-OPTION
               WHEN 19     MOVE "A"  TO WS-OPTION
               WHEN 20     MOVE "0"  TO WS-OPTION
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO CR05
             END-EVALUATE.
             GO TO CR18.

       CR16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "0"    MOVE 20   TO SCREEN-LIN
               WHEN "1"    MOVE 11   TO SCREEN-LIN
               WHEN "2"    MOVE 12   TO SCREEN-LIN
               WHEN "3"    MOVE 13   TO SCREEN-LIN
               WHEN "4"    MOVE 14   TO SCREEN-LIN
               WHEN "5"    MOVE 15   TO SCREEN-LIN
               WHEN "6"    MOVE 16   TO SCREEN-LIN
               WHEN "7"    MOVE 17   TO SCREEN-LIN
               WHEN "8"    MOVE 18   TO SCREEN-LIN
               WHEN "A"
               WHEN ">"
               WHEN "<"
               WHEN "/"
               WHEN "!"    MOVE 19   TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       CR18.
             EVALUATE WS-OPTION
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                          E X I T                          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "0"    GO TO CR999
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё             Create Alternate Description Index            Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "1"    MOVE "Y"                TO WS-REOPEN
                           CLOSE STKALT STOCK NETWORK PARAM
                           MOVE "UTP\STAREB"       TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                   Clear Guarantee Period                  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "2"    IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Not available in this version               Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
                               MOVE "MNU\NOPMENU"  TO PRG-NAME
                           ELSE
                               MOVE "STP\STP092"   TO PRG-NAME
                           END-IF
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                      Clear Quantities                     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "3"    IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Not available in this version               Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
                               MOVE "MNU\NOPMENU"  TO PRG-NAME
                           ELSE
                               MOVE "STP\STP093"   TO PRG-NAME
                           END-IF
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё              Clear Quantities per Department              Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "4"    IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Not available in this version               Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
                               MOVE "MNU\NOPMENU"  TO PRG-NAME
                           ELSE
                               MOVE "STP\STP094"   TO PRG-NAME
                           END-IF
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                    Delete an Item Range                   Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "5"    IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Not available in this version               Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
                               MOVE "MNU\NOPMENU"  TO PRG-NAME
                           ELSE
                               MOVE "STP\STP095"   TO PRG-NAME
                           END-IF
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                      Price Label Code                     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "6"    MOVE "STP\STP096"       TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                    Re-Calculate Prices                    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "7"    MOVE "STP\STP097"       TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё           Average cost Adjustment (Not on Menu)           Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN ">"    MOVE "STP\STP098"       TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Patch Program  (Not on Menu)                Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "<"    MOVE "STP\STP099"       TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                          Enquiries                        Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "8"    MOVE "STP\STPENQ"       TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё             Varify and correct AVERAGE prices             Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "A"    MOVE "STP\STPCSTAV"     TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё      Picking Slip Quantity recalculate (Not on Menu)      Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "/"   MOVE "STP\STPPIC"        TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё   Create Alternate code for Mica Dbn North (Not on Menu)  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "!"   MOVE "STP\STPALT"        TO PRG-NAME
             END-EVALUATE.
             PERFORM AA100.
           IF WS-REOPEN = "Y"
               MOVE SPACE            TO WS-REOPEN
               OPEN I-O STKALT STOCK NETWORK PARAM.
             DISPLAY MENU-INSTRUCT.
             GO TO CR05.

       CR999.
             EXIT.
      *    *************************************************************
      *    ****       S T O C K   R E P O R T S      ****
      *    *************************************************************
       DA000        SECTION 5.
       DA00.
             MOVE 10                 TO SHADE-ROW.
             MOVE 26                 TO SHADE-COL.
             MOVE 24                 TO SHADE-WIDTH.
             MOVE 18                 TO SHADE-LINES.
             DISPLAY S08.
           IF LS0-LEV < 3
               DISPLAY ". (N/A)" AT 1842 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Cyan
               DISPLAY "(N/A)"   AT 2240 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Cyan
               DISPLAY "(N/A)"   AT 2344 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Cyan
               DISPLAY "(N/A)"   AT 5044 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Cyan.
           IF LS0-LEV < 2
               DISPLAY " (N/A)"  AT 2443 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Cyan.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
             MOVE 11                 TO TOP-ROW.
             MOVE 25                 TO BOTTOM-ROW.
             MOVE 10                 TO SCREEN-LIN.
             MOVE 27                 TO SCREEN-COL.
             MOVE 21                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       DA05.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       DA10.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     PERFORM MOVE-BLOCK-UP
                 WHEN DOWN-KEY   PERFORM MOVE-BLOCK-DOWN
                 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
                                 IF MOUSE-ENTER = "Y"
                                     MOVE "N"  TO MOUSE-ENTER
                                     GO TO DA15
                                 END-IF
                                 IF MOUSE-Y = 49
                                     IF MOUSE-X = 5
                                         MOVE 5  TO KEY-CODE-1
                                         GO TO DA10
                                     ELSE
                                     IF MOUSE-X = 10
                                         MOVE 6  TO KEY-CODE-1
                                         GO TO DA10
                                     ELSE
                                     IF MOUSE-X > 36 AND < 44
                                         MOVE 0  TO KEY-CODE-1
                                         GO TO DA10
                                     ELSE
                                     IF MOUSE-X > 54 AND < 60
                                         MOVE 0  TO KEY-CODE-1
                                         MOVE 1  TO KEY-TYPE
                                         GO TO DA10
                                     END-IF
                                 END-IF
                 WHEN ENTER-KEY  GO TO DA15
                 WHEN OTHER  PERFORM AA900-ALARM
               END-EVALUATE
               GO TO DA05
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE "0"  TO WS-OPTION
                                 GO TO DA18
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO DA05
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X  TO WS-OPTION
               CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS
               IF (WS-OPTION < 0 OR > 8) AND (WS-OPTION < "A" OR > "F")
                   PERFORM AA900-ALARM
                   GO TO DA05
               END-IF
               GO TO DA16
           ELSE
               PERFORM AA900-ALARM
               GO TO DA05.

       DA15.
             EVALUATE SCREEN-LIN
               WHEN 11     MOVE "1"  TO WS-OPTION
               WHEN 12     MOVE "2"  TO WS-OPTION
               WHEN 13     MOVE "3"  TO WS-OPTION
               WHEN 14     MOVE "4"  TO WS-OPTION
               WHEN 15     MOVE "5"  TO WS-OPTION
               WHEN 16     MOVE "6"  TO WS-OPTION
               WHEN 17     MOVE "7"  TO WS-OPTION
               WHEN 18     MOVE "8"  TO WS-OPTION
               WHEN 19     MOVE "A"  TO WS-OPTION
               WHEN 20     MOVE "B"  TO WS-OPTION
               WHEN 21     MOVE "C"  TO WS-OPTION
               WHEN 22     MOVE "D"  TO WS-OPTION
               WHEN 23     MOVE "E"  TO WS-OPTION
               WHEN 24     MOVE "F"  TO WS-OPTION
               WHEN 25     MOVE "0"  TO WS-OPTION
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO DA05
             END-EVALUATE.
             GO TO DA18.

       DA16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "0"    MOVE 25   TO SCREEN-LIN
               WHEN "1"    MOVE 11   TO SCREEN-LIN
               WHEN "2"    MOVE 12   TO SCREEN-LIN
               WHEN "3"    MOVE 13   TO SCREEN-LIN
               WHEN "4"    MOVE 14   TO SCREEN-LIN
               WHEN "5"    MOVE 15   TO SCREEN-LIN
               WHEN "6"    MOVE 16   TO SCREEN-LIN
               WHEN "7"    MOVE 17   TO SCREEN-LIN
               WHEN "8"    MOVE 18   TO SCREEN-LIN
               WHEN "A"    MOVE 19   TO SCREEN-LIN
               WHEN "B"    MOVE 20   TO SCREEN-LIN
               WHEN "C"    MOVE 21   TO SCREEN-LIN
               WHEN "D"    MOVE 22   TO SCREEN-LIN
               WHEN "E"    MOVE 23   TO SCREEN-LIN
               WHEN "F"    MOVE 24   TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       DA18.
             EVALUATE WS-OPTION
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                          E X I T                          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "0"    GO TO DA999
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                        Price Lists                        Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "1"    MOVE "STP\STP041" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                     Departmental Lists                    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "2"    MOVE "STP\STP042" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Stock List - Item Code Sequence             Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "3"    MOVE "STP\STP043" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                       Stock Take List                     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "4"    MOVE "STP\STP044" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                       Movement Report                     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "5"    MOVE "STP\STP045" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                 Low-Levels / Re-Order report              Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "6"    MOVE "STP\STP046" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                     Bill of Quantities                    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "7"    IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Not available in this version               Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
                               MOVE "MNU\NOPMENU" TO PRG-NAME
                           ELSE
                               MOVE "STP\STP047"  TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                     Quantity/Price List                   Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "8"    MOVE "STP\STP048" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                       Price Labels                        Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "A"    MOVE "STP\STP049" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                     Slow Moving Stock                     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "B"    MOVE "STP\STP050" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                        Exceptions                         Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "C"    IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Not available in this version               Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
                               MOVE "MNU\NOPMENU" TO PRG-NAME
                           ELSE
                               MOVE "STP\STP051"  TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                      Redundant Stock                      Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "D"    IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Not available in this version               Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
                               MOVE "MNU\NOPMENU" TO PRG-NAME
                           ELSE
                               MOVE "STP\STP052"  TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                     Stock per Supplier                    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "E"    IF LS0-LEV < 2
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Not available in this version               Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
                               MOVE "MNU\NOPMENU" TO PRG-NAME
                           ELSE
                               MOVE "STP\STP053"  TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                       Price Catalogue                     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "F"    IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Not available in this version               Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
                               MOVE "MNU\NOPMENU" TO PRG-NAME
                           ELSE
                               MOVE "STP\STP054"  TO PRG-NAME
             END-EVALUATE.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.
             GO TO DA05.

       DA999.
             EXIT.
      *    *************************************************************
      *    ****       S T O C K   E X P O R T  /  I M P O R T     ****
      *    *************************************************************
       EA000        SECTION 5.
       EA00.
           IF LS-DSKDRV = "L"
               MOVE "Not available on Local drive" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO EA999.
           IF LS0-LEV < 2
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё     Not available in this version        Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               MOVE "MNU\NOPMENU"    TO PRG-NAME
               PERFORM AA100
               DISPLAY MENU-INSTRUCT
               GO TO EA999.
             MOVE 10                 TO SHADE-ROW.
             MOVE 26                 TO SHADE-COL.
             MOVE 24                 TO SHADE-WIDTH.
             MOVE 10                 TO SHADE-LINES.
             DISPLAY S09.
           IF LS0-LEV < 3
               DISPLAY "(N/A)"         AT 1242 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Cyan
               DISPLAY "(N/A)"         AT 1343 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Cyan
               DISPLAY "(In) (N/A)   " AT 1436 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Cyan
               DISPLAY "(Out) (N/A)  " AT 1536 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Cyan
               DISPLAY "(N/A)"         AT 1744 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Cyan.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
             MOVE 11                 TO TOP-ROW.
             MOVE 17                 TO BOTTOM-ROW.
             MOVE 10                 TO SCREEN-LIN.
             MOVE 27                 TO SCREEN-COL.
             MOVE 21                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       EA05.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       EA10.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     PERFORM MOVE-BLOCK-UP
                 WHEN DOWN-KEY   PERFORM MOVE-BLOCK-DOWN
                 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
                                 IF MOUSE-ENTER = "Y"
                                     MOVE "N"  TO MOUSE-ENTER
                                     GO TO EA15
                                 END-IF
                                 IF MOUSE-Y = 49
                                     IF MOUSE-X = 5
                                         MOVE 5  TO KEY-CODE-1
                                         GO TO EA10
                                     ELSE
                                     IF MOUSE-X = 10
                                         MOVE 6  TO KEY-CODE-1
                                         GO TO EA10
                                     ELSE
                                     IF MOUSE-X > 36 AND < 44
                                         MOVE 0  TO KEY-CODE-1
                                         GO TO EA10
                                     ELSE
                                     IF MOUSE-X > 54 AND < 60
                                         MOVE 0  TO KEY-CODE-1
                                         MOVE 1  TO KEY-TYPE
                                         GO TO EA10
                                     END-IF
                                 END-IF
                 WHEN ENTER-KEY  GO TO EA15
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO EA05
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE "0"  TO WS-OPTION
                                 GO TO EA18
                 WHEN OTHER  PERFORM AA900-ALARM
               END-EVALUATE
               GO TO EA05
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X  TO WS-OPTION
               IF WS-OPTION < 0 OR > 6
                   PERFORM AA900-ALARM
                   GO TO EA05
               END-IF
               GO TO EA16
           ELSE
               PERFORM AA900-ALARM
               GO TO EA05.

       EA15.
             EVALUATE SCREEN-LIN
               WHEN 11     MOVE "1"  TO WS-OPTION
               WHEN 12     MOVE "2"  TO WS-OPTION
               WHEN 13     MOVE "3"  TO WS-OPTION
               WHEN 14     MOVE "4"  TO WS-OPTION
               WHEN 15     MOVE "5"  TO WS-OPTION
               WHEN 16     MOVE "6"  TO WS-OPTION
               WHEN 17     MOVE "0"  TO WS-OPTION
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO EA05
             END-EVALUATE.
             GO TO EA18.

       EA16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "0"    MOVE 17   TO SCREEN-LIN
               WHEN "1"    MOVE 11   TO SCREEN-LIN
               WHEN "2"    MOVE 12   TO SCREEN-LIN
               WHEN "3"    MOVE 13   TO SCREEN-LIN
               WHEN "4"    MOVE 14   TO SCREEN-LIN
               WHEN "5"    MOVE 15   TO SCREEN-LIN
               WHEN "6"    MOVE 16   TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       EA18.
             EVALUATE WS-OPTION
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                          E X I T                          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "0"    GO TO EA999
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                    Branch Transfers In                    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "1"    IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Not available in this version               Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
                               MOVE "MNU\NOPMENU" TO PRG-NAME
                           ELSE
                               MOVE "STP\STP031"  TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                    Branch Transfers Out                   Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "2"    IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Not available in this version               Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
                               MOVE "MNU\NOPMENU" TO PRG-NAME
                           ELSE
                               MOVE "STP\STP032"  TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                  Prices - Branch (Import)                 Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "3"    IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Not available in this version               Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
                               MOVE "MNU\NOPMENU" TO PRG-NAME
                           ELSE
                               MOVE "STP\STP033"  TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                  Prices - Branch (Export)                 Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "4"    IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Not available in this version               Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
                               MOVE "MNU\NOPMENU" TO PRG-NAME
                           ELSE
                               MOVE "STP\STP034"  TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                       Export Options                      Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "5"    MOVE "STP\STPEXP"      TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                       Import Options                      Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "6"    IF LS0-LEV < 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Not available in this version               Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
                               MOVE "MNU\NOPMENU" TO PRG-NAME
                           ELSE
                               MOVE "STP\STPIMP"  TO PRG-NAME
             END-EVALUATE.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.
             GO TO EA05.

       EA999.
             EXIT.
      *    *************************************************************
      *    ****                  E N Q U I R I E S                  ****
      *    *************************************************************
       FA000        SECTION 5.
       FA00.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                         Enquiries                         Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
             MOVE "STP\STPENQ"       TO PRG-NAME.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.

       FA999.
             EXIT.
      *    *************************************************************
      *    ****                L O G I N / L O G O U T              ****
      *    *************************************************************
       GA000        SECTION 5.
       GA00.
           IF LS-OK = "Y"
               GO TO GA30.
             MOVE 3                  TO WS-NETKEY.
       GA05.
             READ NETWORK.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 2                TO WS-F-ERROR
               PERFORM READ-ERROR.
           IF RECORD-LOCKED
               GO TO GA05.
            MOVE STK-PASSWORD        TO WS-PASSWORD.

       GA10.
             MOVE SPACES             TO WS-PASS.
             MOVE 15                 TO SHADE-ROW.
             MOVE 28                 TO SHADE-COL.
             MOVE 25                 TO SHADE-WIDTH.
             MOVE 3                  TO SHADE-LINES.
             DISPLAY SD-PW.
             PERFORM SCREEN-SHADOW.
             ACCEPT WS-PASS AT 1644 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT AUTO NO-ECHO.
             MOVE 8                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING WS-PASS BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             PERFORM RESTORE-SCREEN.
           IF NOT(WS-PASS = "@#$" OR WS-PASSWORD)
               MOVE "Inalid password"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO GA999.
             MOVE "Y"                TO LS-OK.
             MOVE 1                  TO WS-INSTR.
             MOVE "Do you want to change the password (Y/N) [ ]"  TO WS-OPT-MES.
             MOVE "N"                TO WS-OPTION.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "N"
               GO TO GA999.
             MOVE 15                 TO SHADE-ROW.
             MOVE 29                 TO SHADE-COL.
             MOVE 23                 TO SHADE-WIDTH.
             MOVE 3                  TO SHADE-LINES.
             DISPLAY SD-NPW.
             PERFORM SCREEN-SHADOW.

       GA20.
             ACCEPT WS-PASS AT 1643 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
             MOVE 8                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING WS-PASS BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             MOVE "'C'orrect or 'R'ekey  [C]" TO WS-OPT-MES
             MOVE 9                  TO WS-INSTR
             MOVE "C"                TO WS-OPTION.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "R"
               GO TO GA20.
       GA25.
             READ NETWORK WITH KEPT LOCK.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 2                TO WS-F-ERROR
               PERFORM READ-ERROR.
           IF RECORD-LOCKED
               GO TO GA25.
             MOVE WS-PASS            TO STK-PASSWORD.
             REWRITE NET-RECORD.
           IF WS-STATUS NOT = "00"
               MOVE 2                TO WS-F-ERROR
               PERFORM WRITE-ERROR.
             UNLOCK NETWORK.
             GO TO GA999.
       GA30.
             MOVE "N"                TO LS-OK.
       GA999.
             EXIT.
      *    *************************************************************
      *    ****  I N I T I A L I S E   P R O G R A M     ****
      *    *************************************************************
       ZA000-INIT        SECTION 9.
       ZA000-OPEN.
             MOVE LS-COMPANY         TO CRT-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY.
             PERFORM CHANGE-HEADING.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Setup various keys and the mouse for use with menus and  Ё
      *    Ё  other required user interface functions and procedures.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

             COPY "FUNCTION.PRO".

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё     Setup the file ID's including path names           Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *    *************************************************************
      *    ****    P A R A M E T E R   F I L E      ****
      *    *************************************************************
       ZA00A-CONTINUE.
             OPEN I-O CARDEX.
           IF WS-STATUS = "05"
               CLOSE CARDEX OPEN OUTPUT CARDEX CLOSE CARDEX OPEN I-O CARDEX.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 43           TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
               MOVE 43               TO WS-F-ERROR
               IF WS-STATUS = "39"
                   GO TO ZA50
               ELSE
               IF NOT(WS-STATUS = "41")
                   PERFORM OPEN-ERROR.
             OPEN INPUT CREDIT.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 3            TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
               MOVE 3                TO WS-F-ERROR
               IF WS-STATUS = "39"
                   GO TO ZA50
               ELSE
               IF NOT(WS-STATUS = "41")
                   PERFORM OPEN-ERROR.
             OPEN I-O PURDEX.
           IF WS-STATUS = "05"
               CLOSE PURDEX OPEN OUTPUT PURDEX CLOSE PURDEX OPEN I-O PURDEX.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT OR (WS-STATUS = "39")
                   MOVE 56           TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
               MOVE 56               TO WS-F-ERROR
           IF NOT(WS-STATUS = "41")
               PERFORM OPEN-ERROR.
             OPEN I-O STKALT.
           IF WS-STATUS = "05"
               CLOSE STKALT OPEN OUTPUT STKALT CLOSE STKALT OPEN I-O STKALT.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 49           TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
               MOVE 49               TO WS-F-ERROR
               IF WS-STATUS = "39"
                   GO TO ZA50
               ELSE
               IF NOT(WS-STATUS = "41")
                   PERFORM OPEN-ERROR.
             OPEN I-O STKDEX.
           IF WS-STATUS = "05"
               CLOSE STKDEX OPEN OUTPUT STKDEX CLOSE STKDEX OPEN I-O STKDEX.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 46           TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
               MOVE 46               TO WS-F-ERROR
               IF WS-STATUS = "39"
                  GO TO ZA50
               ELSE
               IF NOT(WS-STATUS = "41")
                   PERFORM OPEN-ERROR.
             OPEN I-O STKMEM.
           IF WS-STATUS = "05"
               CLOSE STKMEM OPEN OUTPUT STKMEM CLOSE STKMEM OPEN I-O STKMEM.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 25           TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
               MOVE 25               TO WS-F-ERROR
               IF WS-STATUS = "39"
                   GO TO ZA50
               ELSE
               IF NOT(WS-STATUS = "41")
                   PERFORM OPEN-ERROR.
             OPEN I-O STOCK.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 22           TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
               MOVE 22               TO WS-F-ERROR
               IF WS-STATUS = "39"
                   GO TO ZA50
               ELSE
               IF NOT(WS-STATUS = "41")
                   PERFORM OPEN-ERROR.
             OPEN I-O SPARTS.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 21           TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
               MOVE 21               TO WS-F-ERROR
               IF WS-STATUS = "39"
                   GO TO ZA50
               ELSE
               IF NOT(WS-STATUS = "41")
                   PERFORM OPEN-ERROR.
             OPEN I-O PORDER.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 16           TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
               MOVE 16               TO WS-F-ERROR
               IF WS-STATUS = "39"
                   GO TO ZA50
               ELSE
               IF NOT(WS-STATUS = "41")
                   PERFORM OPEN-ERROR.
             OPEN I-O LEDTRF.
           IF WS-STATUS = "05"
               CLOSE LEDTRF OPEN OUTPUT LEDTRF CLOSE LEDTRF OPEN I-O LEDTRF.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 40           TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
               MOVE 40               TO WS-F-ERROR
               IF WS-STATUS = "39"
                   GO TO ZA50
               ELSE
               IF NOT(WS-STATUS = "41")
                   PERFORM OPEN-ERROR.
             OPEN I-O WARHSE.
           IF WS-STATUS = "05"
               CLOSE WARHSE OPEN OUTPUT WARHSE CLOSE WARHSE OPEN I-O WARHSE.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 51           TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
               MOVE 51               TO WS-F-ERROR
               IF WS-STATUS = "39"
                   GO TO ZA50
               ELSE
               IF NOT(WS-STATUS = "41")
                   PERFORM OPEN-ERROR.
             OPEN I-O WSTOCK.
           IF WS-STATUS = "05"
               CLOSE WSTOCK OPEN OUTPUT WSTOCK CLOSE WSTOCK OPEN I-O WSTOCK.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 52           TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
               MOVE 52               TO WS-F-ERROR
               IF WS-STATUS = "39"
                   GO TO ZA50
               ELSE
               IF NOT(WS-STATUS = "41")
                   PERFORM OPEN-ERROR.
      *    IF LS-DEM = 5
      *        MOVE "STP\STP093"     TO PRG-NAME
      *        PERFORM AA100.
             GO TO ZA999.
            
       COPY "ZA49.PRO".

       ZA50.
             EVALUATE WS-F-ERROR
               WHEN 3          MOVE "CREDITOR"        TO WS-FILE
               WHEN 16         MOVE "PURCHASE ORDER"  TO WS-FILE
               WHEN 21         MOVE "PRODUCTION"      TO WS-FILE
               WHEN 22         MOVE "STOCK file"      TO WS-FILE
               WHEN 25         MOVE "STOCK MEMO"      TO WS-FILE
               WHEN 40         MOVE "INTEGRATION"     TO WS-FILE
               WHEN 43         MOVE "CARDEX (SALES)"  TO WS-FILE
               WHEN 46         MOVE "CARDEX (ADJUST)" TO WS-FILE
               WHEN 49         MOVE "STOCK DESC INDX" TO WS-FILE
               WHEN 51         MOVE "WAREHOUSE file"  TO WS-FILE
               WHEN 52         MOVE "STOCK LOCATION"  TO WS-FILE
               WHEN 56         MOVE "CARDEX (PURCH)"  TO WS-FILE
             END-EVALUATE.
             DISPLAY "Rebuild " AT 0812 WS-FILE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Press any key to continue" AT 1012.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             
       ZA51.
             EXIT PROGRAM.

       ZA200.

       COPY "LOCKED.PRO".

       ZA205.
             EXIT PROGRAM.
       ZA999.
             EXIT.

       I-O-ERRORS      SECTION  9.
      
       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 2          MOVE W02-NETWORK  TO WS-FILE
                               MOVE WS-NETKEY    TO WS-KEY
               WHEN 3          MOVE W02-CREDIT   TO WS-FILE
                               MOVE P-NUMBER     TO WS-KEYX
               WHEN 16         MOVE W02-PORDER   TO WS-FILE
                               MOVE ORD-KEY      TO WS-KEYX
               WHEN 21         MOVE W02-SPARTS   TO WS-FILE
                               MOVE PRT-ITEM     TO WS-KEYX
               WHEN 22         MOVE W02-STOCKF   TO WS-FILE
                               MOVE STK-CODE     TO WS-KEYX
               WHEN 25         MOVE W02-STKMEM   TO WS-FILE
                               MOVE SME-KEY      TO WS-KEYX
               WHEN 40         MOVE W02-LEDTRF   TO WS-FILE
                               MOVE XFR-KEY      TO WS-KEYX
               WHEN 43         MOVE W02-CARDEX   TO WS-FILE
                               MOVE CRD-KEY      TO WS-KEYX
               WHEN 46         MOVE W02-STKDEX   TO WS-FILE
                               MOVE STX-KEY      TO WS-KEYX
               WHEN 49         MOVE W02-STKALT   TO WS-FILE
                               MOVE STKA-KEY     TO WS-KEYX
               WHEN 51         MOVE W02-WARHSE   TO WS-FILE
                               MOVE WAR-CODE     TO WS-KEYX
               WHEN 52         MOVE W02-WSTOCK   TO WS-FILE
                               MOVE WST-KEY      TO WS-KEYX
               WHEN 56         MOVE W02-PURDEX   TO WS-FILE
                               MOVE PRD-KEY      TO WS-KEYX
               WHEN OTHER      MOVE "ERROR"      TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
