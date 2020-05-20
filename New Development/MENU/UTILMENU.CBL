      $set LINKCOUNT"512" GNT"UTILMENU.GNT"
      ******************************************************************
      *                                                                *
      *  **    **  ********  **        **     **  ********  **     **  *
      *  **    **     **     **        ***   ***  **        ***    **  *
      *  **    **     **     **        ** * * **  **        ** *   **  *
      *  **    **     **     **        **  *  **  *****     **  *  **  *
      *  **    **     **     **        **     **  **        **   * **  *
      *  **    **     **     **        **     **  **        **    ***  *
      *   ******      **     ********  **     **  *******   **     **  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     U T I L I T I E S   M E N U   P R O G R A M                *
      *                                                                *
      *    Version 9.04.01 - May 2016                                  *
      *                                                                *
      ******************************************************************
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
      *    Ё via the LINKAGE SECTION from program to program in  the   Ё
      *    Ё LS0-MODULES group. The other indicators in this group are Ё
      *    Ё used to show which modules of the system are installed.   Ё
      *    фмммммммммммммммммммямммммммммммммммммммяммммммммммммммммммм╣
      *    Ё    SYSTEM FILE    Ё      CHAIN.WS     Ё     CHAIN.LS      Ё
      *    цдддддддддддддддддддедддддддддддддддддддеддддддддддддддддддд╢
      *    Ё03 SEC-MODULES. Ё03  WS0-MODULES.      Ё03 LS0-MODULES.    Ё
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
       PROGRAM-ID.     UTILMENU.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   OCTOBER 1982.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1982 - 2016
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

       COPY "TXTRAN.SL".

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

       COPY "TXTRAN.FDE".

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
       77  WS-CHECK                  PIC  X(18)    VALUE "aeWlimemnomLalismJ".
       77  WS-S1    PIC 9(04)  COMP-5.
       77  WS-S2           PIC  9(04)    COMP-5.
       77  WS-S3           PIC  9(04)    COMP-5.
       77  WS-S4           PIC  9(04)    COMP-5.
       77  WS-TEMP-LIN    PIC 9(02)  COMP-5.
       77  WS-OPTION    PIC X(01).
       77  PRG-NAME    PIC X(12) VALUE SPACES.
       77  WS-PASSWORD    PIC X(08) VALUE SPACES.
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.

      *    здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Used by calling program (WORKING-STORAGE) and called program Ё
      *    Ё (LINKAGE SECTION) for Main screen layout and headings.       Ё
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
           03  CRT-PROGRAM           PIC  X(15) VALUE "UTILMENU".
      *
      *    Screen Heading
      *
           03  CRT-HEADING           PIC  X(40) VALUE "UTILITIES/PARAMETERS CONTROL MODULE".
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
      *   ******    ******   *******   ********  ********  **     **
      *  **    **  **    **  **    **  **        **        ***    **
      *  **        **        **    **  **        **        ** *   **
      *   ******   **        ******    *****     *****     **  *  **
      *        **  **        **   **   **        **        **   * **
      *  **    **  **    **  **    **  **        **        **    ***
      *   ******    ******   **    **  ********  ********  **     **
      *
       SCREEN SECTION.

       COPY "MENUINST.CRT".

       01  S02.
           03  BACKGROUND-COLOR Green FOREGROUND-COLOR Black.
               05  LINE  8 COLUMN  7 VALUE "                     ".
               05  LINE  9 COLUMN  7 VALUE "   UTILITY CONTROL   " FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 10 COLUMN  7 VALUE " 1"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Parameters        ".
               05  LINE 11 COLUMN  7 VALUE " 2"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Rebuild Files     ".
               05  LINE 12 COLUMN  7 VALUE " 3"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Delivery Routes   ".
               05  LINE 13 COLUMN  7 VALUE " 4"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Clear USER Log    ".
               05  LINE 14 COLUMN  7 VALUE " 5"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Clear Values      ".
               05  LINE 15 COLUMN  7 VALUE " 6"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Purge Cardex File ".
               05  LINE 16 COLUMN  7 VALUE " 0"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN  9 VALUE   " Exit              ".
               05  LINE 17 COLUMN  7 VALUE "                     ".

       01  S03.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Black.
               05  LINE 10 COLUMN 26 VALUE "                  ".
               05  LINE 11 COLUMN 26 VALUE "  REBUILD FILES   " FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 12 COLUMN 26 VALUE " 1"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Creditors      ".
               05  LINE 13 COLUMN 26 VALUE " 2"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Debtors        ".
               05  LINE 14 COLUMN 26 VALUE " 3"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " General Ledger ".
               05  LINE 15 COLUMN 26 VALUE " 4"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Job Costing    ".
               05  LINE 16 COLUMN 26 VALUE " 5"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Stock          ".
               05  LINE 17 COLUMN 26 VALUE " 6"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Sundry         ".
               05  LINE 18 COLUMN 26 VALUE " 0"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Exit           ".
               05  LINE 19 COLUMN 26 VALUE "                  ".

       01  S04.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE 12 COLUMN 44 VALUE "                   ".
               05  LINE 13 COLUMN 44 VALUE "  CREDITOR FILES   " FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 14 COLUMN 44 VALUE " 1"                  FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Account File    ".
               05  LINE 15 COLUMN 44 VALUE " 2"                  FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Purchase Orders ".
               05  LINE 16 COLUMN 44 VALUE " 3"                  FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " PD Cheques      ".
               05  LINE 17 COLUMN 44 VALUE " 4"                  FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Transactions    ".
               05  LINE 18 COLUMN 44 VALUE " 5"                  FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Memo/Remarks    ".
               05  LINE 19 COLUMN 44 VALUE " 6"                  FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Cardex (Purch)  ".
               05  LINE 20 COLUMN 44 VALUE " 0"                  FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Exit            ".
               05  LINE 21 COLUMN 44 VALUE "                   ".

       01  S05.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE 12 COLUMN 44 VALUE "                  ".
               05  LINE 13 COLUMN 44 VALUE "   DEBTOR FILES   " FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 14 COLUMN 44 VALUE " 1"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Account File   ".
               05  LINE 15 COLUMN 44 VALUE " 2"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Invoice File   ".
               05  LINE 16 COLUMN 44 VALUE " 3"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Memo/Remarks   ".
               05  LINE 17 COLUMN 44 VALUE " 4"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Sales Orders   ".
               05  LINE 18 COLUMN 44 VALUE " 5"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Transactions   ".
               05  LINE 19 COLUMN 44 VALUE " 6"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Rec.Transact's ".
               05  LINE 20 COLUMN 44 VALUE " 7"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " ALT NAME Index ".
               05  LINE 21 COLUMN 44 VALUE " 0"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Exit           ".
               05  LINE 22 COLUMN 44 VALUE "                  ".

       01  S06.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE 12 COLUMN 44 VALUE "                  ".
               05  LINE 13 COLUMN 44 VALUE "  GENERAL LEDGER  " FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 14 COLUMN 44 VALUE " 1"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Account File   ".
               05  LINE 15 COLUMN 44 VALUE " 2"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Batch File     ".
               05  LINE 16 COLUMN 44 VALUE " 3"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Integration    ".
               05  LINE 17 COLUMN 44 VALUE " 4"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Transactions   ".
               05  LINE 18 COLUMN 44 VALUE " 5"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Cheque Batch   ".
               05  LINE 19 COLUMN 44 VALUE " 0"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Exit           ".
               05  LINE 20 COLUMN 44 VALUE "                  ".

       01  S07.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE 12 COLUMN 44 VALUE "                  ".
               05  LINE 13 COLUMN 44 VALUE "   JOB COSTING    " FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 14 COLUMN 44 VALUE " 1"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Job File       ".
               05  LINE 15 COLUMN 44 VALUE " 2"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Job Types      ".
               05  LINE 16 COLUMN 44 VALUE " 3"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Requisitions   ".
               05  LINE 17 COLUMN 44 VALUE " 4"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Rev. Warranty  ".
               05  LINE 18 COLUMN 44 VALUE " 0"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Exit           ".
               05  LINE 19 COLUMN 44 VALUE "                  ".

       01  S08.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE 12 COLUMN 44 VALUE "                  ".
               05  LINE 13 COLUMN 44 VALUE "   STOCK FILES    " FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 14 COLUMN 44 VALUE " 1"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Memo/Remarks   ".
               05  LINE 15 COLUMN 44 VALUE " 2"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Production     ".
               05  LINE 16 COLUMN 44 VALUE " 3"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Stock File     ".
               05  LINE 17 COLUMN 44 VALUE " 4"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Cardex File    ".
               05  LINE 18 COLUMN 44 VALUE " 5"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Warehouse File ".
               05  LINE 19 COLUMN 44 VALUE " 6"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " W/H Stock File ".
               05  LINE 20 COLUMN 44 VALUE " 0"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Exit           ".
               05  LINE 21 COLUMN 44 VALUE "                  ".

       01  S09.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE 12 COLUMN 44 VALUE "                  ".
               05  LINE 13 COLUMN 44 VALUE "   SUNDRY FILES   " FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 14 COLUMN 44 VALUE " 1"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Cheques        ".
               05  LINE 15 COLUMN 44 VALUE " 2"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Guarantees     ".
               05  LINE 16 COLUMN 44 VALUE " 3"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Tax (VAT)      ".
               05  LINE 17 COLUMN 44 VALUE " 4"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Cash Slips     ".
               05  LINE 18 COLUMN 44 VALUE " 5"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Department     ".
               05  LINE 19 COLUMN 44 VALUE " 0"                 FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE   " Exit           ".
               05  LINE 20 COLUMN 44 VALUE "                  ".

       01  S11.
           03  S11A.
               05  LINE 23 COLUMN 12 VALUE "Password:" FOREGROUND-COLOR Brown HIGHLIGHT.
           03  S11B.
               05  LINE 23 COLUMN 22 PIC  X(08) USING WS-PASSWORD SECURE AUTO.

       01  S12.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Black.
               05  LINE 10 COLUMN 26 VALUE "                      ".
               05  LINE 11 COLUMN 26 VALUE "      PARAMETERS      " FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 12 COLUMN 26 VALUE " 1"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Company/Staff Data ".
               05  LINE 13 COLUMN 26 VALUE " 2"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Transaction Codes  ".
               05  LINE 14 COLUMN 26 VALUE " 3"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Printer Details    ".
               05  LINE 15 COLUMN 26 VALUE " 4"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Tax/Int/Disc Rates ".
               05  LINE 16 COLUMN 26 VALUE " 5"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Account Numbers    ".
               05  LINE 17 COLUMN 26 VALUE " 6"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Sundry Parameters  ".
               05  LINE 18 COLUMN 26 VALUE " 7"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Cheque Printing    ".
               05  LINE 19 COLUMN 26 VALUE " 8"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Inhouse Bar Codes  ".
               05  LINE 20 COLUMN 26 VALUE " A"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Bar Code Labels    ".
               05  LINE 21 COLUMN 26 VALUE " B"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Stock Conv. Factor ".
               05  LINE 22 COLUMN 26 VALUE " 0"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 28 VALUE   " Exit               ".
               05  LINE 23 COLUMN 26 VALUE "                      ".

       01  S13.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE 12 COLUMN 47 VALUE "                     ".
               05  LINE 13 COLUMN 47 VALUE " COMPANY/STAFF DATA  " FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 14 COLUMN 47 VALUE " 1"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Company Details   ".
               05  LINE 15 COLUMN 47 VALUE " 2"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Debtor Categories ".
               05  LINE 16 COLUMN 47 VALUE " 3"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Sales Ledgers     ".
               05  LINE 17 COLUMN 47 VALUE " 4"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Sales Staff Data  ".
               05  LINE 18 COLUMN 47 VALUE " 5"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Technicians Data  ".
               05  LINE 19 COLUMN 47 VALUE " 6"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Branch Details    ".
               05  LINE 20 COLUMN 47 VALUE " 7"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Bank Details      ".
               05  LINE 21 COLUMN 47 VALUE " 0"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Exit              ".
               05  LINE 22 COLUMN 47 VALUE "                     ".

       01  S14.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE 12 COLUMN 47 VALUE "                   ".
               05  LINE 13 COLUMN 47 VALUE " TRANSACTION CODES " FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 14 COLUMN 47 VALUE " 1"                  FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Creditor Codes  ".
               05  LINE 15 COLUMN 47 VALUE " 2"                  FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Debtor Codes    ".
               05  LINE 16 COLUMN 47 VALUE " 0"                  FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Exit            ".
               05  LINE 17 COLUMN 47 VALUE "                   ".

       01  S15.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE 12 COLUMN 47 VALUE "                          ".
               05  LINE 13 COLUMN 47 VALUE "   PRINTERS/STATIONERY    " FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 14 COLUMN 47 VALUE " 1"                         FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Printer Specifications ".
               05  LINE 15 COLUMN 47 VALUE " 2"                         FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Supervisor Printers    ".
               05  LINE 16 COLUMN 47 VALUE " 3"                         FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Work Station Printers  ".
               05  LINE 17 COLUMN 47 VALUE " 4"                         FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Cash Slip Headings     ".
               05  LINE 18 COLUMN 47 VALUE " 5"                         FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Cash Slip/Drawer Cont. ".
               05  LINE 19 COLUMN 47 VALUE " 6"                         FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Statement/Inv. Formats ".
               05  LINE 20 COLUMN 47 VALUE " 7"                         FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Statement/Inv. Heading ".
               05  LINE 21 COLUMN 47 VALUE " 8"                         FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Consol. Statements     " .
               05  LINE 22 COLUMN 47 VALUE " A"                         FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Stm Det. Lines USB/WIN ".
               05  LINE 23 COLUMN 47 VALUE " 0"                         FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Exit                   ".
               05  LINE 24 COLUMN 47 VALUE "                          ".

       01  S16.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE 12 COLUMN 47 VALUE "                      ".
               05  LINE 13 COLUMN 47 VALUE "  TAX/INT/DISC RATES  " FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 14 COLUMN 47 VALUE " 1"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Value Added Tax    ".
               05  LINE 15 COLUMN 47 VALUE " 2"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Ad-Valorem Tax     ".
               05  LINE 16 COLUMN 47 VALUE " 3"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Arrear Interest    ".
               05  LINE 17 COLUMN 47 VALUE " 4"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Settlement Discnt. ".
               05  LINE 18 COLUMN 47 VALUE " 0"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Exit               ".
               05  LINE 19 COLUMN 47 VALUE "                      ".

       01  S17.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE 12 COLUMN 47 VALUE "                   ".
               05  LINE 13 COLUMN 47 VALUE "  ACCOUNT NUMBERS  " FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 14 COLUMN 47 VALUE " 1"                  FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Cash Sales      ".
               05  LINE 15 COLUMN 47 VALUE " 2"                  FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " General Ledger  ".
               05  LINE 16 COLUMN 47 VALUE " 0"                  FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Exit            ".
               05  LINE 17 COLUMN 47 VALUE "                   ".

       01  S18.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE 12 COLUMN 47 VALUE "                      ".
               05  LINE 13 COLUMN 47 VALUE "  SUNDRY PARAMETERS   " FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 14 COLUMN 47 VALUE " 1"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Starting Numbers   ".
               05  LINE 15 COLUMN 47 VALUE " 2"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Price Rounding     ".
               05  LINE 16 COLUMN 47 VALUE " 3"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " System Passwords   ".
               05  LINE 17 COLUMN 47 VALUE " 4"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Minimum Margin     ".
               05  LINE 18 COLUMN 47 VALUE " 5"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Cost Codes         ".
               05  LINE 19 COLUMN 47 VALUE " 6"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Debtor Title Codes ".
               05  LINE 20 COLUMN 47 VALUE " 7"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Special Routines   ".
               05  LINE 21 COLUMN 47 VALUE " 8"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Cash Voucher Code  ".
               05  LINE 22 COLUMN 47 VALUE " A"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Batch/Realtime POS ".
               05  LINE 23 COLUMN 47 VALUE " B"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Cash Slip Rounding ".
               05  LINE 24 COLUMN 47 VALUE " C"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Article 11/1g(VAT) ".
               05  LINE 25 COLUMN 47 VALUE " D"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Stock Conv.Factor  ".
               05  LINE 26 COLUMN 47 VALUE " 0"                     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 49 VALUE   " Exit               ".
               05  LINE 27 COLUMN 47 VALUE "                      ".

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
       AA000        SECTION.
       AA00.
           IF LS0-PALEV < 1 AND LS0-SALEV < 4
               MOVE "Not Authorised" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO AA99.
             PERFORM ZA000-INIT.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66               TO MOUSE-FUNC
               MOVE 1                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM
               END-IF.
             PERFORM BA000.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66   TO MOUSE-FUNC
               MOVE 0   TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
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
             MOVE 29                 TO WS-MES-LINE.
             PERFORM ERROR-MESSAGE.
             MOVE W43-SCREEN2        TO W43-SCREEN.
             MOVE W42-ATTRIB2        TO W42-ATTRIB.
             GO TO AA105.

       AA199.
             EXIT.

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
      *    Ё To clear line 50:                                         Ё
      *    Ё    PERFORM CLEAR-L50                                      Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "CLEAR.CRT".

       COPY "AA900.ALM".

       BA000        SECTION.
       BA00.
             MOVE 8                  TO SHADE-ROW.
             MOVE 7                  TO SHADE-COL.
             MOVE 21                 TO SHADE-WIDTH.
             MOVE 10                 TO SHADE-LINES.
             DISPLAY S02.
           IF (LS0-LEV < 4) OR (LS0-RTE = 0)
               DISPLAY " Del. Routes (N/A)Ё" AT 0810 WITH BACKGROUND-COLOR Green FOREGROUND-COLOR Black.
           IF (LS0-LEV < 3)
               DISPLAY " Purge Cardex(N/A)Ё" AT 1110 WITH BACKGROUND-COLOR Green FOREGROUND-COLOR Black.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
       BA02.
             MOVE 9                  TO TOP-ROW.
             MOVE 15                 TO BOTTOM-ROW.
             MOVE 8                  TO SCREEN-LIN.
             MOVE 8                  TO SCREEN-COL.
             MOVE 18                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.
           IF LS-OK = "Z"
               MOVE SPACE  TO LS-OK
               MOVE 1                TO WS-OPTION
               GO TO BA16.

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
               MOVE KEY-CODE-1X  TO WS-OPTION
               CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS
               IF (WS-OPTION < 0 OR > 6) AND (WS-OPTION NOT = "S")
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
               WHEN 15    MOVE "0"   TO WS-OPTION
               WHEN OTHER PERFORM AA900-ALARM
                          GO TO BA05
             END-EVALUATE.
             GO TO BA18.

       BA16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "0"  MOVE 15     TO SCREEN-LIN
               WHEN "1"  MOVE 9      TO SCREEN-LIN
               WHEN "2"  MOVE 10     TO SCREEN-LIN
               WHEN "3"  MOVE 11     TO SCREEN-LIN
               WHEN "4"  MOVE 12     TO SCREEN-LIN
               WHEN "5"  MOVE 13     TO SCREEN-LIN
               WHEN "6"  MOVE 14     TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       BA18.
             PERFORM SAVE-SCREEN.
             MOVE BLOCK-DETAIL       TO BLOCK-DETAIL1.
             MOVE SHADOW-DETAIL      TO SHADOW-DETAIL1.
             MOVE CRT-DETAIL         TO CRT-DETAIL1.
             EVALUATE WS-OPTION
               WHEN "0"  GO TO BA999
               WHEN "1"  PERFORM GA000
               WHEN "2"  PERFORM CA000
               WHEN "3"  PERFORM DA000
               WHEN "4"  PERFORM EA000
               WHEN "5"  PERFORM FA000
               WHEN "6"  PERFORM IA000
               WHEN "S"  PERFORM HA000
             END-EVALUATE.
             PERFORM RESTORE-SCREEN.
             MOVE BLOCK-DETAIL1      TO BLOCK-DETAIL.
             MOVE SHADOW-DETAIL1     TO SHADOW-DETAIL.
             MOVE CRT-DETAIL1        TO CRT-DETAIL.
             GO TO BA05.
       BA999.
             EXIT.

      *
      *    ****    R E B U I L D   F I L E S
      *
       CA000        SECTION 5.
       CA00.
             MOVE SPACES  TO WS-PASSWORD.
             DISPLAY S11A.
             ACCEPT S11B.
             MOVE 8                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING WS-PASSWORD BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             MOVE 23                 TO CRT-START CRT-END.
             PERFORM ERASE-SCREEN.
           IF WS-PASSWORD NOT = "APACFILE"
               GO TO CA999.
             MOVE 10                 TO SHADE-ROW.
             MOVE 26                 TO SHADE-COL.
             MOVE 18                 TO SHADE-WIDTH.
             MOVE 10                 TO SHADE-LINES.
             DISPLAY S03.
           IF LS0-LEV < 1
               DISPLAY ".N/A" AT 1041 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Black.
           IF (LS0-LEV < 4) AND (LS0-JCP = 0)
               DISPLAY "N/A" AT 1142 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Black.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
             MOVE 11                 TO TOP-ROW.
             MOVE 17                 TO BOTTOM-ROW.
             MOVE 10                 TO SCREEN-LIN.
             MOVE 27                 TO SCREEN-COL.
             MOVE 15                 TO STRING-LENGTH.
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
                          MOVE "N"  TO MOUSE-ENTER
                          GO TO CA15
                      END-IF
                      IF MOUSE-Y = 49
                          IF MOUSE-X = 5
                              MOVE 5  TO KEY-CODE-1
                              GO TO CA10
                          ELSE
                          IF MOUSE-X = 10
                              MOVE 6  TO KEY-CODE-1
                              GO TO CA10
                          ELSE
                          IF MOUSE-X > 36 AND < 44
                              MOVE 0  TO KEY-CODE-1
                              GO TO CA10
                          ELSE
                          IF MOUSE-X > 54 AND < 60
                              MOVE 0  TO KEY-CODE-1
                              MOVE 1  TO KEY-TYPE
                              GO TO CA10
                          END-IF
                      END-IF
                 WHEN ENTER-KEY  GO TO BA15
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE "0"  TO WS-OPTION
                                 GO TO CA18
                 WHEN OTHER  PERFORM AA900-ALARM
               END-EVALUATE
               GO TO CA05
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X  TO WS-OPTION
               IF WS-OPTION < 0 OR > 6
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
               WHEN 17     MOVE "0"  TO WS-OPTION
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO CA05
             END-EVALUATE.
             GO TO CA18.

       CA16.
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

       CA18.
             PERFORM SAVE-SCREEN-2.
             MOVE BLOCK-DETAIL       TO BLOCK-DETAIL2.
             MOVE SHADOW-DETAIL      TO SHADOW-DETAIL2.
             MOVE CRT-DETAIL         TO CRT-DETAIL2.
             EVALUATE WS-OPTION
               WHEN "0"    GO TO CA999
               WHEN "1"    PERFORM CB000
               WHEN "2"    PERFORM CC000
               WHEN "3"    PERFORM CD000
               WHEN "4"    PERFORM CE000
               WHEN "5"    PERFORM CF000
               WHEN "6"    PERFORM CG000
             END-EVALUATE.
             PERFORM RESTORE-SCREEN-2.
             MOVE BLOCK-DETAIL2      TO BLOCK-DETAIL.
             MOVE SHADOW-DETAIL2     TO SHADOW-DETAIL.
             MOVE CRT-DETAIL2        TO CRT-DETAIL.
             GO TO CA05.

       CA999.
             EXIT.
      *
      *    ****    R E B U I L D   C R E D I T O R   F I L E S
      *
       CB000        SECTION 5.
       CB0.
          IF LS0-CRED = "N" OR LS0-CRLEV < 9
              MOVE "Not Authorised"  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO CB999.

       CB00.
             MOVE 12  TO SHADE-ROW.
             MOVE 44   TO SHADE-COL.
             MOVE 19   TO SHADE-WIDTH.
             MOVE 10  TO SHADE-LINES.
             DISPLAY S04.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
             MOVE 13  TO TOP-ROW.
             MOVE 19   TO BOTTOM-ROW.
             MOVE 12  TO SCREEN-LIN.
             MOVE 45   TO SCREEN-COL.
             MOVE 16   TO STRING-LENGTH.
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
               IF WS-OPTION < 0 OR > 6
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
               WHEN 19     MOVE "0"   TO WS-OPTION
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO CB05
             END-EVALUATE.
             GO TO CB18.

       CB16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "0"    MOVE 19   TO SCREEN-LIN
               WHEN "1"    MOVE 13   TO SCREEN-LIN
               WHEN "2"    MOVE 14   TO SCREEN-LIN
               WHEN "3"    MOVE 15   TO SCREEN-LIN
               WHEN "4"    MOVE 16   TO SCREEN-LIN
               WHEN "5"    MOVE 17   TO SCREEN-LIN
               WHEN "6"    MOVE 18   TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       CB18.
             EVALUATE WS-OPTION
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                          E X I T                          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "0"    GO TO CB999
               WHEN "1"    MOVE "UTP\CRDREB" TO PRG-NAME
               WHEN "2"    MOVE "UTP\PURREB" TO PRG-NAME
               WHEN "3"    MOVE "UTP\PDCREB" TO PRG-NAME
               WHEN "4"    MOVE "UTP\CRTREB" TO PRG-NAME
               WHEN "5"    MOVE "UTP\CRMREB" TO PRG-NAME
               WHEN "6"    MOVE "UTP\PRDREB" TO PRG-NAME
             END-EVALUATE.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.
             GO TO CB05.

       CB999.
             EXIT.
      *
      *    ****    R E B U I L D   D E B T O R  F I L E S
      *
       CC000        SECTION 5.
       CC0.
          IF LS0-DEBT = "N" OR LS0-DBLEV < 9
              MOVE "Not Authorised"  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO CC999.

       CC00.
             MOVE 12                 TO SHADE-ROW.
             MOVE 44                 TO SHADE-COL.
             MOVE 18                 TO SHADE-WIDTH.
             MOVE 11                 TO SHADE-LINES.
             DISPLAY S05.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
             MOVE 13                 TO TOP-ROW.
             MOVE 20                 TO BOTTOM-ROW.
             MOVE 12                 TO SCREEN-LIN.
             MOVE 45                 TO SCREEN-COL.
             MOVE 15                 TO STRING-LENGTH.
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
               IF WS-OPTION < 0 OR > 7
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
               WHEN 20     MOVE "0"  TO WS-OPTION
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO CC05
             END-EVALUATE.
             GO TO CC18.

       CC16.
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

       CC18.
             EVALUATE WS-OPTION
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                          E X I T                          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "0"    GO TO CC999
               WHEN "1"    MOVE "UTP\DEBREB" TO PRG-NAME
               WHEN "2"    MOVE "UTP\INVREB" TO PRG-NAME
               WHEN "3"    MOVE "UTP\DBMREB" TO PRG-NAME
               WHEN "4"    MOVE "UTP\SALREB" TO PRG-NAME
               WHEN "5"    MOVE "UTP\DBTREB" TO PRG-NAME
               WHEN "6"    MOVE "UTP\DBFREB" TO PRG-NAME
               WHEN "7"    MOVE "UTP\DALREB" TO PRG-NAME
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
      *
      *    ****    R E B U I L D   G / L E D G E R   F I L E S
      *
       CD000        SECTION 5.
       CD0.
           IF LS0-LEV < 1
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Not available in this version               Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               MOVE "MNU\NOPMENU"    TO PRG-NAME
               PERFORM AA100
               DISPLAY MENU-INSTRUCT
               GO TO CD999.

       CD00.
             MOVE 12                 TO SHADE-ROW.
             MOVE 44                 TO SHADE-COL.
             MOVE 18                 TO SHADE-WIDTH.
             MOVE 9                  TO SHADE-LINES.
             DISPLAY S06.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
             MOVE 13                 TO TOP-ROW.
             MOVE 18                 TO BOTTOM-ROW.
             MOVE 12                 TO SCREEN-LIN.
             MOVE 45                 TO SCREEN-COL.
             MOVE 15                 TO STRING-LENGTH.
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
               IF WS-OPTION < 0 OR > 5
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
               WHEN 18     MOVE "0"  TO WS-OPTION
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO CD05
             END-EVALUATE.
             GO TO CD18.

       CD16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "0"    MOVE 18   TO SCREEN-LIN
               WHEN "1"    MOVE 13   TO SCREEN-LIN
               WHEN "2"    MOVE 14   TO SCREEN-LIN
               WHEN "3"    MOVE 15   TO SCREEN-LIN
               WHEN "4"    MOVE 16   TO SCREEN-LIN
               WHEN "5"    MOVE 17   TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       CD18.
             EVALUATE WS-OPTION
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                          E X I T                          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "0"    GO TO CD999
               WHEN "1"    MOVE "UTP\GACREB" TO PRG-NAME
               WHEN "2"    MOVE "UTP\GBHREB" TO PRG-NAME
               WHEN "3"    MOVE "UTP\LEDREB" TO PRG-NAME
               WHEN "4"    MOVE "UTP\GTRREB" TO PRG-NAME
               WHEN "5"    MOVE "UTP\CHBREB" TO PRG-NAME
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
      *
      *    ****    R E B U I L D   J O B - C O S T   F I L E S
      *
       CE000        SECTION 5.
       CE0.
           IF (LS0-LEV < 4) AND (LS0-JCP = 0)
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               Not available in this version               Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               MOVE "MNU\NOPMENU"    TO PRG-NAME
               PERFORM AA100
               DISPLAY MENU-INSTRUCT
               GO TO CE999.
           IF LS0-JOBS = "N" OR LS0-JBLEV < 9
               MOVE "Not Authorised"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CE999.

       CE00.
             MOVE 12                 TO SHADE-ROW.
             MOVE 44                 TO SHADE-COL.
             MOVE 18                 TO SHADE-WIDTH.
             MOVE 8                  TO SHADE-LINES.
             DISPLAY S07.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
             MOVE 13                 TO TOP-ROW.
             MOVE 17                 TO BOTTOM-ROW.
             MOVE 12                 TO SCREEN-LIN.
             MOVE 45                 TO SCREEN-COL.
             MOVE 15                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       CE05.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       CE10.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     PERFORM MOVE-BLOCK-UP
                 WHEN DOWN-KEY   PERFORM MOVE-BLOCK-DOWN
                 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
                                 IF MOUSE-ENTER = "Y"
                                     MOVE "N"    TO MOUSE-ENTER
                                     GO TO CE15
                                 END-IF
                                 IF MOUSE-Y = 49
                                     IF MOUSE-X = 5
                                         MOVE 5  TO KEY-CODE-1
                                         GO TO CE10
                                     ELSE
                                     IF MOUSE-X = 10
                                         MOVE 6  TO KEY-CODE-1
                                         GO TO CE10
                                     ELSE
                                     IF MOUSE-X > 36 AND < 44
                                         MOVE 0  TO KEY-CODE-1
                                         GO TO CE10
                                     ELSE
                                     IF MOUSE-X > 54 AND < 60
                                         MOVE 0  TO KEY-CODE-1
                                         MOVE 1  TO KEY-TYPE
                                         GO TO CE10
                                     END-IF
                                 END-IF
                 WHEN ENTER-KEY  GO TO CE15
                 WHEN OTHER  PERFORM AA900-ALARM
               END-EVALUATE
               GO TO CE05
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE "0"  TO WS-OPTION
                                 GO TO CE18
                 WHEN OTHER  PERFORM AA900-ALARM
               END-EVALUATE
               GO TO CE05
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X      TO WS-OPTION
               IF WS-OPTION < "0" OR > "4"
                   PERFORM AA900-ALARM
                   GO TO CE05
               END-IF
               GO TO CE16
           ELSE
               PERFORM AA900-ALARM
               GO TO CE05.

       CE15.
             EVALUATE SCREEN-LIN
               WHEN 13     MOVE "1"  TO WS-OPTION
               WHEN 14     MOVE "2"  TO WS-OPTION
               WHEN 15     MOVE "3"  TO WS-OPTION
               WHEN 16     MOVE "4"  TO WS-OPTION
               WHEN 17     MOVE "0"  TO WS-OPTION
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO CE05
             END-EVALUATE.
             GO TO CE18.

       CE16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "0"    MOVE 17   TO SCREEN-LIN
               WHEN "1"    MOVE 13   TO SCREEN-LIN
               WHEN "2"    MOVE 14   TO SCREEN-LIN
               WHEN "3"    MOVE 15   TO SCREEN-LIN
               WHEN "4"    MOVE 16   TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       CE18.
             EVALUATE WS-OPTION
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                          E X I T                          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "0"    GO TO CE999
               WHEN "1"    MOVE "UTP\JOBREB" TO PRG-NAME
               WHEN "2"    MOVE "UTP\JTPREB" TO PRG-NAME
               WHEN "3"    MOVE "UTP\JRQREB" TO PRG-NAME
               WHEN "4"    MOVE "UTP\JWAREB" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                       I N V A L I D                       Ё
      *    Ё   Added test as a precaution. It should never happen.     Ё   
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO CE05
             END-EVALUATE.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.
             GO TO CE05.

       CE999.
             EXIT.
      *
      *    ****    R E B U I L D   S T O C K   F I L E S
      *
       CF000        SECTION 5.
       CF0.
           IF LS0-STOCK = "N" OR LS0-STLEV < 9
               MOVE "Not Authorised"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CF999.

       CF00.
             MOVE 12                 TO SHADE-ROW.
             MOVE 44                 TO SHADE-COL.
             MOVE 18                 TO SHADE-WIDTH.
             MOVE 10                 TO SHADE-LINES.
             DISPLAY S08.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
             MOVE 13                 TO TOP-ROW.
             MOVE 19                 TO BOTTOM-ROW.
             MOVE 12                 TO SCREEN-LIN.
             MOVE 45                 TO SCREEN-COL.
             MOVE 15                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       CF05.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       CF10.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     PERFORM MOVE-BLOCK-UP
                 WHEN DOWN-KEY   PERFORM MOVE-BLOCK-DOWN
                 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
                                 IF MOUSE-ENTER = "Y"
                                     MOVE "N"    TO MOUSE-ENTER
                                     GO TO CF15
                                 END-IF
                                 IF MOUSE-Y = 49
                                     IF MOUSE-X = 5
                                         MOVE 5  TO KEY-CODE-1
                                         GO TO CF10
                                     ELSE
                                     IF MOUSE-X = 10
                                         MOVE 6  TO KEY-CODE-1
                                         GO TO CF10
                                     ELSE
                                     IF MOUSE-X > 36 AND < 44
                                         MOVE 0  TO KEY-CODE-1
                                         GO TO CF10
                                     ELSE
                                     IF MOUSE-X > 54 AND < 60
                                         MOVE 0  TO KEY-CODE-1
                                         MOVE 1  TO KEY-TYPE
                                         GO TO CF10
                                     END-IF
                                 END-IF
                 WHEN ENTER-KEY  GO TO CF15
                 WHEN OTHER  PERFORM AA900-ALARM
               END-EVALUATE
               GO TO CE05
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE "0"  TO WS-OPTION
                                 GO TO CF18
                 WHEN OTHER  PERFORM AA900-ALARM
               END-EVALUATE
               GO TO CE05
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X      TO WS-OPTION
               IF WS-OPTION < "0" OR > "6"
                   PERFORM AA900-ALARM
                   GO TO CF05
               END-IF
               GO TO CF16
           ELSE
               PERFORM AA900-ALARM
               GO TO CF05.

       CF15.
             EVALUATE SCREEN-LIN
               WHEN 13     MOVE "1"  TO WS-OPTION
               WHEN 14     MOVE "2"  TO WS-OPTION
               WHEN 15     MOVE "3"  TO WS-OPTION
               WHEN 16     MOVE "4"  TO WS-OPTION
               WHEN 17     MOVE "5"  TO WS-OPTION
               WHEN 18     MOVE "6"  TO WS-OPTION
               WHEN 19     MOVE "0"  TO WS-OPTION
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO CF05
             END-EVALUATE.
             GO TO CF18.

       CF16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "0"    MOVE 19   TO SCREEN-LIN
               WHEN "1"    MOVE 13   TO SCREEN-LIN
               WHEN "2"    MOVE 14   TO SCREEN-LIN
               WHEN "3"    MOVE 15   TO SCREEN-LIN
               WHEN "4"    MOVE 16   TO SCREEN-LIN
               WHEN "5"    MOVE 17   TO SCREEN-LIN
               WHEN "6"    MOVE 18   TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       CF18.
             EVALUATE WS-OPTION
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                          E X I T                          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "0"    GO TO CF999
               WHEN "1"    MOVE "UTP\STMREB" TO PRG-NAME
               WHEN "2"    MOVE "UTP\PROREB" TO PRG-NAME
               WHEN "3"    MOVE "UTP\STKREB" TO PRG-NAME
               WHEN "4"    MOVE "UTP\CARREB" TO PRG-NAME
                           PERFORM AA100
                           MOVE "UTP\PRDREB" TO PRG-NAME
                           PERFORM AA100
                           MOVE "UTP\SRDREB" TO PRG-NAME
               WHEN "5"    MOVE "UTP\WARREB" TO PRG-NAME
               WHEN "6"    MOVE "UTP\WSTREB" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                       I N V A L I D                       Ё
      *    Ё   Added test as a precaution. It should never happen.     Ё   
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO CF05
             END-EVALUATE.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.
             GO TO CF05.

       CF999.
             EXIT.
      *
      *    ****    R E B U I L D   S U N D R Y  F I L E S
      *
       CG000        SECTION 5.
       CG0.
          IF LS0-CRLEV < 9 AND LS0-DBLEV < 9 AND LS0-GLLEV < 9 AND LS0-STLEV < 9 AND LS0-JBLEV < 9 AND LS0-PALEV < 9 AND LS0-SALEV < 9
              MOVE "Not Authorised"  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO CG999.

       CG00.
             MOVE 12                 TO SHADE-ROW.
             MOVE 44                 TO SHADE-COL.
             MOVE 18                 TO SHADE-WIDTH.
             MOVE 9                  TO SHADE-LINES.
             DISPLAY S09.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
             MOVE 13                 TO TOP-ROW.
             MOVE 18                 TO BOTTOM-ROW.
             MOVE 12                 TO SCREEN-LIN.
             MOVE 45                 TO SCREEN-COL.
             MOVE 15                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       CG05.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       CG10.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     PERFORM MOVE-BLOCK-UP
                 WHEN DOWN-KEY   PERFORM MOVE-BLOCK-DOWN
                 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
                                 IF MOUSE-ENTER = "Y"
                                     MOVE "N"    TO MOUSE-ENTER
                                     GO TO CG15
                                 END-IF
                                 IF MOUSE-Y = 49
                                     IF MOUSE-X = 5
                                         MOVE 5  TO KEY-CODE-1
                                         GO TO CG10
                                     ELSE
                                     IF MOUSE-X = 10
                                         MOVE 6  TO KEY-CODE-1
                                         GO TO CG10
                                     ELSE
                                     IF MOUSE-X > 36 AND < 44
                                         MOVE 0  TO KEY-CODE-1
                                         GO TO CG10
                                     ELSE
                                     IF MOUSE-X > 54 AND < 60
                                         MOVE 0  TO KEY-CODE-1
                                         MOVE 1  TO KEY-TYPE
                                         GO TO CG10
                                     END-IF
                                 END-IF
                 WHEN ENTER-KEY  GO TO CG15
                 WHEN OTHER  PERFORM AA900-ALARM
               END-EVALUATE
               GO TO CG05
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE "0"  TO WS-OPTION
                                 GO TO CG18
                 WHEN OTHER  PERFORM AA900-ALARM
               END-EVALUATE
               GO TO CG05
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X      TO WS-OPTION
               IF WS-OPTION < "0" OR > "5"
                   PERFORM AA900-ALARM
                   GO TO CG05
               END-IF
               GO TO CG16
           ELSE
               PERFORM AA900-ALARM
               GO TO CG05.

       CG15.
             EVALUATE SCREEN-LIN
               WHEN 13     MOVE "1"  TO WS-OPTION
               WHEN 14     MOVE "2"  TO WS-OPTION
               WHEN 15     MOVE "3"  TO WS-OPTION
               WHEN 16     MOVE "4"  TO WS-OPTION
               WHEN 17     MOVE "5"  TO WS-OPTION
               WHEN 18     MOVE "0"  TO WS-OPTION
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO CG05
             END-EVALUATE.
             GO TO CG18.

       CG16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "0"    MOVE 18   TO SCREEN-LIN
               WHEN "1"    MOVE 13   TO SCREEN-LIN
               WHEN "2"    MOVE 14   TO SCREEN-LIN
               WHEN "3"    MOVE 15   TO SCREEN-LIN
               WHEN "4"    MOVE 16   TO SCREEN-LIN
               WHEN "5"    MOVE 17   TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       CG18.
             EVALUATE WS-OPTION
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                          E X I T                          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "0"    GO TO CG999
               WHEN "1"    MOVE "UTP\CHQREB" TO PRG-NAME
               WHEN "2"    MOVE "UTP\GARREB" TO PRG-NAME
               WHEN "3"    MOVE "UTP\TAXREB" TO PRG-NAME
                           CLOSE TXTRAN
               WHEN "4"    MOVE "UTP\SLPREB" TO PRG-NAME
               WHEN "5"    MOVE "UTP\DPTREB" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                       I N V A L I D                       Ё
      *    Ё   Added test as a precaution. It should never happen.     Ё   
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO CG05
             END-EVALUATE.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.
           IF PRG-NAME = "UTP\TAXREB"
               OPEN I-O TXTRAN.
             GO TO CG05.

       CG999.
             EXIT.
      *
      *    ****    D E L I V E R Y   R O U T E S
      *
       DA000        SECTION 6.
       DA00.
           IF LS-DSKDRV = "L"
               MOVE "Not available on Local drive" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO DA999.
           IF (LS0-LEV < 4) OR (LS0-RTE = 0)
               MOVE "MNU\NOPMENU"    TO PRG-NAME
           ELSE
               MOVE "SLP\DLP006"     TO PRG-NAME.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.
       DA999.
             EXIT.
      *
      *    ****    C L E A R   U S E R  L O G
      *
       EA000        SECTION 6.
       EA00.
             MOVE "LOG001"           TO PRG-NAME.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.

       EA999.
             EXIT.
      *
      *    ****    C L E A R   V A L U E S
      *
       FA000        SECTION 6.
       FA00.
           IF LS-DSKDRV = "L"
               MOVE "Not available on Local drive" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO FA999.
             MOVE SPACES             TO WS-PASSWORD.
             DISPLAY S11A.
             ACCEPT S11B.
             MOVE 8                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING WS-PASSWORD BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             MOVE 23                 TO CRT-START CRT-END.
             PERFORM ERASE-SCREEN.
           IF WS-PASSWORD NOT = "APACZERO"
               GO TO FA999.

       FA999.
             EXIT.
      *
      *    ****    P A R A M E T E R S
      *
       GA000        SECTION 7.
       GA00.
           IF LS-DSKDRV = "L"
               MOVE "Not available on Local drive" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO GA999.
           IF LS0-PARM = "N"
               MOVE "Not Authorised"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO GA999.

       GA02.
             MOVE 10                 TO SHADE-ROW.
             MOVE 26                 TO SHADE-COL.
             MOVE 22                 TO SHADE-WIDTH.
             MOVE 14                 TO SHADE-LINES.
             DISPLAY S12.
           IF LS0-LEV < 3
               DISPLAY "N/A"        AT 1446 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Black
               DISPLAY "/Codes N/A" AT 1539 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Black
               DISPLAY "N/A"        AT 1546 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Black
               DISPLAY "N/A"        AT 1746 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Black.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
             MOVE 11                 TO TOP-ROW.
             MOVE 21                 TO BOTTOM-ROW.
             MOVE 10                 TO SCREEN-LIN.
             MOVE 27                 TO SCREEN-COL.
             MOVE 19                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       GA05.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       GA10.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     PERFORM MOVE-BLOCK-UP
                 WHEN DOWN-KEY   PERFORM MOVE-BLOCK-DOWN
                 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
                                 IF MOUSE-ENTER = "Y"
                                     MOVE "N"  TO MOUSE-ENTER
                                     GO TO GA15
                                 END-IF
                                 IF MOUSE-Y = 49
                                     IF MOUSE-X = 5
                                         MOVE 5  TO KEY-CODE-1
                                         GO TO GA10
                                     ELSE
                                     IF MOUSE-X = 10
                                         MOVE 6  TO KEY-CODE-1
                                         GO TO GA10
                                     ELSE
                                     IF MOUSE-X > 36 AND < 44
                                         MOVE 0  TO KEY-CODE-1
                                         GO TO GA10
                                     ELSE
                                     IF MOUSE-X > 54 AND < 60
                                         MOVE 0  TO KEY-CODE-1
                                         MOVE 1  TO KEY-TYPE
                                         GO TO GA10
                                     END-IF
                                 END-IF
                 WHEN ENTER-KEY  GO TO GA15
                 WHEN OTHER  PERFORM AA900-ALARM
               END-EVALUATE
               GO TO GA05
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE "0"  TO WS-OPTION
                                 GO TO GA18
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO GA05
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X  TO WS-OPTION
               CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS
               IF (WS-OPTION < 0 OR > 8) AND (WS-OPTION < "A" OR > "B")
                   PERFORM AA900-ALARM
                   GO TO GA05
               END-IF
               GO TO GA16
           ELSE
               PERFORM AA900-ALARM
               GO TO GA05.

       GA15.
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
               WHEN 21     MOVE "0"  TO WS-OPTION
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO GA05
             END-EVALUATE.
             GO TO GA18.

       GA16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "0"    MOVE 21   TO SCREEN-LIN
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
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       GA18.
             PERFORM SAVE-SCREEN-2.
             MOVE BLOCK-DETAIL       TO BLOCK-DETAIL2.
             MOVE SHADOW-DETAIL      TO SHADOW-DETAIL2.
             MOVE CRT-DETAIL         TO CRT-DETAIL2.
             EVALUATE WS-OPTION
               WHEN "0"    GO TO GA999
               WHEN "1"    PERFORM GB000
               WHEN "2"    PERFORM GC000
               WHEN "3"    PERFORM GD000
               WHEN "4"    PERFORM GE000
               WHEN "5"    PERFORM GF000
               WHEN "6"    PERFORM GG000
               WHEN "7"    PERFORM GH000
               WHEN "8"    PERFORM GI000
               WHEN "A"    PERFORM GK000
               WHEN "B"    PERFORM GJ000
             END-EVALUATE.
             PERFORM RESTORE-SCREEN-2.
             MOVE BLOCK-DETAIL2      TO BLOCK-DETAIL.
             MOVE SHADOW-DETAIL2     TO SHADOW-DETAIL.
             MOVE CRT-DETAIL2        TO CRT-DETAIL.
             GO TO GA05.

       GA999.
             EXIT.

       GB000        SECTION 7.
       GB00.
             MOVE 12                 TO SHADE-ROW.
             MOVE 47                 TO SHADE-COL.
             MOVE 21                 TO SHADE-WIDTH.
             MOVE 11                 TO SHADE-LINES.
             DISPLAY S13.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
             MOVE 13                 TO TOP-ROW.
             MOVE 20                 TO BOTTOM-ROW.
             MOVE 12                 TO SCREEN-LIN.
             MOVE 48                 TO SCREEN-COL.
             MOVE 18                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       GB05.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       GB10.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     PERFORM MOVE-BLOCK-UP
                 WHEN DOWN-KEY   PERFORM MOVE-BLOCK-DOWN
                 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
                                 IF MOUSE-ENTER = "Y"
                                     MOVE "N"  TO MOUSE-ENTER
                                     GO TO GB15
                                 END-IF
                                 IF MOUSE-Y = 49
                                     IF MOUSE-X = 5
                                         MOVE 5  TO KEY-CODE-1
                                         GO TO GB10
                                     ELSE
                                     IF MOUSE-X = 10
                                         MOVE 6  TO KEY-CODE-1
                                         GO TO GB10
                                     ELSE
                                     IF MOUSE-X > 36 AND < 44
                                         MOVE 0  TO KEY-CODE-1
                                         GO TO GB10
                                     ELSE
                                     IF MOUSE-X > 54 AND < 60
                                         MOVE 0  TO KEY-CODE-1
                                         MOVE 1  TO KEY-TYPE
                                         GO TO GB10
                                     END-IF
                                 END-IF
                 WHEN ENTER-KEY  GO TO GB15
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO GB05
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE "0"  TO WS-OPTION
                                 GO TO GB18
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO GB05
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X  TO WS-OPTION
               IF WS-OPTION < 0 OR > 7
                   PERFORM AA900-ALARM
                   GO TO GB05
               END-IF
               GO TO GB16
           ELSE
               PERFORM AA900-ALARM
               GO TO GB05.

       GB15.
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
                           GO TO GB05
             END-EVALUATE.
             GO TO GB18.

       GB16.
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

       GB18.
             EVALUATE WS-OPTION
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                          E X I T                          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "0"    GO TO GB999
               WHEN "1"    MOVE "UTP\PRP001" TO PRG-NAME
               WHEN "2"    MOVE "UTP\PRP002" TO PRG-NAME
               WHEN "3"    MOVE "UTP\PRP003" TO PRG-NAME
               WHEN "4"    MOVE "UTP\PRP004" TO PRG-NAME
               WHEN "5"    MOVE "UTP\PRP005" TO PRG-NAME
               WHEN "6"    MOVE "UTP\PRP006" TO PRG-NAME
               WHEN "7"    MOVE "UTP\PRP007" TO PRG-NAME
             END-EVALUATE.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.
             GO TO GB05.

       GB999.
             EXIT.

       GC000        SECTION 7.
       GC00.
             MOVE 12                 TO SHADE-ROW.
             MOVE 47                 TO SHADE-COL.
             MOVE 19                 TO SHADE-WIDTH.
             MOVE 6                  TO SHADE-LINES.
             DISPLAY S14.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
             MOVE 13                 TO TOP-ROW.
             MOVE 15                 TO BOTTOM-ROW.
             MOVE 12                 TO SCREEN-LIN.
             MOVE 48                 TO SCREEN-COL.
             MOVE 16                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       GC05.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       GC10.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     PERFORM MOVE-BLOCK-UP
                 WHEN DOWN-KEY   PERFORM MOVE-BLOCK-DOWN
                 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
                                 IF MOUSE-ENTER = "Y"
                                     MOVE "N"  TO MOUSE-ENTER
                                     GO TO GC15
                                 END-IF
                                 IF MOUSE-Y = 49
                                     IF MOUSE-X = 5
                                         MOVE 5  TO KEY-CODE-1
                                         GO TO GC10
                                     ELSE
                                     IF MOUSE-X = 10
                                         MOVE 6  TO KEY-CODE-1
                                         GO TO GC10
                                     ELSE
                                     IF MOUSE-X > 36 AND < 44
                                         MOVE 0  TO KEY-CODE-1
                                         GO TO GC10
                                     ELSE
                                     IF MOUSE-X > 54 AND < 60
                                         MOVE 0  TO KEY-CODE-1
                                         MOVE 1  TO KEY-TYPE
                                         GO TO GC10
                                     END-IF
                                 END-IF
                 WHEN ENTER-KEY  GO TO GC15
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO GC05
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE "0"  TO WS-OPTION
                                 GO TO GC18
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO GC05
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X  TO WS-OPTION
               IF WS-OPTION < 0 OR > 2
                   PERFORM AA900-ALARM
                   GO TO GC05
               END-IF
               GO TO GC16
           ELSE
               PERFORM AA900-ALARM
               GO TO GC05.

       GC15.
             EVALUATE SCREEN-LIN
               WHEN 13     MOVE "1"  TO WS-OPTION
               WHEN 14     MOVE "2"  TO WS-OPTION
               WHEN 15     MOVE "0"  TO WS-OPTION
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO GC05
             END-EVALUATE.
             GO TO GC18.

       GC16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "0"    MOVE 15   TO SCREEN-LIN
               WHEN "1"    MOVE 13   TO SCREEN-LIN
               WHEN "2"    MOVE 14   TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       GC18.
             EVALUATE WS-OPTION
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                          E X I T                          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "0"    GO TO GC999
               WHEN "1"    MOVE "UTP\PRP010" TO PRG-NAME
               WHEN "2"    MOVE "UTP\PRP011" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                       I N V A L I D                       Ё
      *    Ё   Added test as a precaution. It should never happen.     Ё   
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO GC05
             END-EVALUATE.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.
             GO TO GC05.

       GC999.
             EXIT.

       GD000        SECTION 7.
       GD00.
             MOVE 12                 TO SHADE-ROW.
             MOVE 47                 TO SHADE-COL.
             MOVE 26                 TO SHADE-WIDTH.
             MOVE 13                 TO SHADE-LINES.
             DISPLAY S15.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
             MOVE 13                 TO TOP-ROW.
             MOVE 22                 TO BOTTOM-ROW.
             MOVE 12                 TO SCREEN-LIN.
             MOVE 48                 TO SCREEN-COL.
             MOVE 23                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       GD05.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       GD10.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     PERFORM MOVE-BLOCK-UP
                 WHEN DOWN-KEY   PERFORM MOVE-BLOCK-DOWN
                 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
                                 IF MOUSE-ENTER = "Y"
                                     MOVE "N"  TO MOUSE-ENTER
                                     GO TO GD15
                                 END-IF
                                 IF MOUSE-Y = 49
                                     IF MOUSE-X = 5
                                         MOVE 5  TO KEY-CODE-1
                                         GO TO GD10
                                     ELSE
                                     IF MOUSE-X = 10
                                         MOVE 6  TO KEY-CODE-1
                                         GO TO GD10
                                     ELSE
                                     IF MOUSE-X > 36 AND < 44
                                         MOVE 0  TO KEY-CODE-1
                                         GO TO GD10
                                     ELSE
                                     IF MOUSE-X > 54 AND < 60
                                         MOVE 0  TO KEY-CODE-1
                                         MOVE 1  TO KEY-TYPE
                                         GO TO GD10
                                     END-IF
                                 END-IF
                 WHEN ENTER-KEY  GO TO GD15
                 WHEN OTHER  PERFORM AA900-ALARM
               END-EVALUATE
               GO TO GD05
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE "0"  TO WS-OPTION
                                 GO TO GD18
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO GD05
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X  TO WS-OPTION
               CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS
               IF (WS-OPTION < 0 OR > 8) AND (WS-OPTION NOT = "A")
                   PERFORM AA900-ALARM
                   GO TO GD05
               END-IF
               GO TO GD16
           ELSE
               PERFORM AA900-ALARM
               GO TO GD05.

       GD15.
             EVALUATE SCREEN-LIN
               WHEN 13     MOVE "1"  TO WS-OPTION
               WHEN 14     MOVE "2"  TO WS-OPTION
               WHEN 15     MOVE "3"  TO WS-OPTION
               WHEN 16     MOVE "4"  TO WS-OPTION
               WHEN 17     MOVE "5"  TO WS-OPTION
               WHEN 18     MOVE "6"  TO WS-OPTION
               WHEN 19     MOVE "7"  TO WS-OPTION
               WHEN 20     MOVE "8"  TO WS-OPTION
               WHEN 21     MOVE "A"  TO WS-OPTION
               WHEN 22     MOVE "0"  TO WS-OPTION
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO GD05
             END-EVALUATE.
             GO TO GD18.

       GD16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "0"    MOVE 22   TO SCREEN-LIN
               WHEN "1"    MOVE 13   TO SCREEN-LIN
               WHEN "2"    MOVE 14   TO SCREEN-LIN
               WHEN "3"    MOVE 15   TO SCREEN-LIN
               WHEN "4"    MOVE 16   TO SCREEN-LIN
               WHEN "5"    MOVE 17   TO SCREEN-LIN
               WHEN "6"    MOVE 18   TO SCREEN-LIN
               WHEN "7"    MOVE 19   TO SCREEN-LIN
               WHEN "8"    MOVE 20   TO SCREEN-LIN
               WHEN "A"    MOVE 21   TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       GD18.
             EVALUATE WS-OPTION
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                          E X I T                          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "0"    GO TO GD999
               WHEN "1"    MOVE "UTP\PRP020" TO PRG-NAME
               WHEN "2"    MOVE "UTP\PRP021" TO PRG-NAME
               WHEN "3"    MOVE "UTP\PRP022" TO PRG-NAME
               WHEN "4"    MOVE "UTP\PRP023" TO PRG-NAME
               WHEN "5"    MOVE "UTP\PRP024" TO PRG-NAME
               WHEN "6"    MOVE "UTP\PRP025" TO PRG-NAME
               WHEN "7"    MOVE "UTP\PRP026" TO PRG-NAME
               WHEN "8"    MOVE "UTP\PRP027" TO PRG-NAME
               WHEN "A"    MOVE "UTP\PRP028" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                       I N V A L I D                       Ё
      *    Ё   Added test as a precaution. It should never happen.     Ё   
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO GD05
             END-EVALUATE.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.
             GO TO GD05.

       GD999.
             EXIT.

       GE000        SECTION 7.
       GE00.
             MOVE 12                 TO SHADE-ROW.
             MOVE 47                 TO SHADE-COL.
             MOVE 22                 TO SHADE-WIDTH.
             MOVE 8                  TO SHADE-LINES.
             DISPLAY S16.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
             MOVE 13                 TO TOP-ROW.
             MOVE 17                 TO BOTTOM-ROW.
             MOVE 12                 TO SCREEN-LIN.
             MOVE 48                 TO SCREEN-COL.
             MOVE 19                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       GE05.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       GE10.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     PERFORM MOVE-BLOCK-UP
                 WHEN DOWN-KEY   PERFORM MOVE-BLOCK-DOWN
                 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
                                 IF MOUSE-ENTER = "Y"
                                     MOVE "N"    TO MOUSE-ENTER
                                     GO TO GE15
                                 END-IF
                                 IF MOUSE-Y = 49
                                     IF MOUSE-X = 5
                                         MOVE 5  TO KEY-CODE-1
                                         GO TO GE10
                                     ELSE
                                     IF MOUSE-X = 10
                                         MOVE 6  TO KEY-CODE-1
                                         GO TO GE10
                                     ELSE
                                     IF MOUSE-X > 36 AND < 44
                                         MOVE 0  TO KEY-CODE-1
                                         GO TO GE10
                                     ELSE
                                     IF MOUSE-X > 54 AND < 60
                                         MOVE 0  TO KEY-CODE-1
                                         MOVE 1  TO KEY-TYPE
                                         GO TO GE10
                                     END-IF
                                 END-IF
                 WHEN ENTER-KEY  GO TO GE15
                 WHEN OTHER  PERFORM AA900-ALARM
               END-EVALUATE
               GO TO GE05
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE "0"  TO WS-OPTION
                                 GO TO GE18
                 WHEN OTHER  PERFORM AA900-ALARM
               END-EVALUATE
               GO TO GE05
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X      TO WS-OPTION
               IF WS-OPTION < "0" OR > "4"
                   PERFORM AA900-ALARM
                   GO TO GE05
               END-IF
               GO TO GE16
           ELSE
               PERFORM AA900-ALARM
               GO TO GE05.

       GE15.
             EVALUATE SCREEN-LIN
               WHEN 13     MOVE "1"  TO WS-OPTION
               WHEN 14     MOVE "2"  TO WS-OPTION
               WHEN 15     MOVE "3"  TO WS-OPTION
               WHEN 16     MOVE "4"  TO WS-OPTION
               WHEN 17     MOVE "0"  TO WS-OPTION
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO GE05
             END-EVALUATE.
             GO TO GE18.

       GE16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "0"    MOVE 17   TO SCREEN-LIN
               WHEN "1"    MOVE 13   TO SCREEN-LIN
               WHEN "2"    MOVE 14   TO SCREEN-LIN
               WHEN "3"    MOVE 15   TO SCREEN-LIN
               WHEN "4"    MOVE 16   TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       GE18.
             EVALUATE WS-OPTION
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                          E X I T                          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "0"    GO TO GE999
               WHEN "1"    MOVE "UTP\PRP030" TO PRG-NAME
               WHEN "2"    MOVE "UTP\PRP031" TO PRG-NAME
               WHEN "3"    MOVE "UTP\PRP032" TO PRG-NAME
               WHEN "4"    MOVE "UTP\PRP033" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                       I N V A L I D                       Ё
      *    Ё   Added test as a precaution. It should never happen.     Ё   
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO GE05
             END-EVALUATE.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.
             GO TO GE05.

       GE999.
             EXIT.

       GF000        SECTION 7.
       GF00.
             MOVE 12                 TO SHADE-ROW.
             MOVE 47                 TO SHADE-COL.
             MOVE 19                 TO SHADE-WIDTH.
             MOVE 6                  TO SHADE-LINES.
             DISPLAY S17.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
             MOVE 13                 TO TOP-ROW.
             MOVE 15                 TO BOTTOM-ROW.
             MOVE 12                 TO SCREEN-LIN.
             MOVE 48                 TO SCREEN-COL.
             MOVE 16                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       GF05.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       GF10.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     PERFORM MOVE-BLOCK-UP
                 WHEN DOWN-KEY   PERFORM MOVE-BLOCK-DOWN
                 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
                                 IF MOUSE-ENTER = "Y"
                                     MOVE "N"  TO MOUSE-ENTER
                                     GO TO GF15
                                 END-IF
                                 IF MOUSE-Y = 49
                                     IF MOUSE-X = 5
                                         MOVE 5  TO KEY-CODE-1
                                         GO TO GF10
                                     ELSE
                                     IF MOUSE-X = 10
                                         MOVE 6  TO KEY-CODE-1
                                         GO TO GF10
                                     ELSE
                                     IF MOUSE-X > 36 AND < 44
                                         MOVE 0  TO KEY-CODE-1
                                         GO TO GF10
                                     ELSE
                                     IF MOUSE-X > 54 AND < 60
                                         MOVE 0  TO KEY-CODE-1
                                         MOVE 1  TO KEY-TYPE
                                         GO TO GF10
                                     END-IF
                                 END-IF
                 WHEN ENTER-KEY  GO TO GF15
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO GF05
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE "0"  TO WS-OPTION
                                 GO TO GF18
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO GF05
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X  TO WS-OPTION
               IF WS-OPTION < 0 OR > 2
                   PERFORM AA900-ALARM
                   GO TO GF05
               END-IF
               GO TO GF16
           ELSE
               PERFORM AA900-ALARM
               GO TO GF05.

       GF15.
             EVALUATE SCREEN-LIN
               WHEN 13     MOVE "1"  TO WS-OPTION
               WHEN 14     MOVE "2"  TO WS-OPTION
               WHEN 15     MOVE "0"  TO WS-OPTION
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO GF05
             END-EVALUATE.
             GO TO GF18.

       GF16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "0"    MOVE 15   TO SCREEN-LIN
               WHEN "1"    MOVE 13   TO SCREEN-LIN
               WHEN "2"    MOVE 14   TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       GF18.
             EVALUATE WS-OPTION
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                          E X I T                          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "0"    GO TO GF999
               WHEN "1"    MOVE "UTP\PRP040" TO PRG-NAME
               WHEN "2"    MOVE "UTP\PRP041" TO PRG-NAME
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                       I N V A L I D                       Ё
      *    Ё   Added test as a precaution. It should never happen.     Ё   
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO GF05
             END-EVALUATE.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.
             GO TO GF05.

       GF999.
             EXIT.

       GG000        SECTION 7.
       GG00.
             MOVE 12                 TO SHADE-ROW.
             MOVE 47                 TO SHADE-COL.
             MOVE 22                 TO SHADE-WIDTH.
             MOVE 16                 TO SHADE-LINES.
             DISPLAY S18.
           IF LS0-LEV < 3
               DISPLAY "N/A" AT 1966 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black
               DISPLAY "N/A" AT 2065 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black
               DISPLAY "N/A" AT 2166 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
             MOVE 13                 TO TOP-ROW.
             MOVE 25                 TO BOTTOM-ROW.
             MOVE 12                 TO SCREEN-LIN.
             MOVE 48                 TO SCREEN-COL.
             MOVE 19                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       GG05.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       GG10.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     PERFORM MOVE-BLOCK-UP
                 WHEN DOWN-KEY   PERFORM MOVE-BLOCK-DOWN
                 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
                                 IF MOUSE-ENTER = "Y"
                                     MOVE "N"  TO MOUSE-ENTER
                                     GO TO GG15
                                 END-IF
                                 IF MOUSE-Y = 49
                                     IF MOUSE-X = 5
                                         MOVE 5  TO KEY-CODE-1
                                         GO TO GG10
                                     ELSE
                                     IF MOUSE-X = 10
                                         MOVE 6  TO KEY-CODE-1
                                         GO TO GG10
                                     ELSE
                                     IF MOUSE-X > 36 AND < 44
                                         MOVE 0  TO KEY-CODE-1
                                         GO TO GG10
                                     ELSE
                                     IF MOUSE-X > 54 AND < 60
                                         MOVE 0  TO KEY-CODE-1
                                         MOVE 1  TO KEY-TYPE
                                         GO TO GG10
                                     END-IF
                                 END-IF
                 WHEN ENTER-KEY  GO TO GG15
                 WHEN OTHER  PERFORM AA900-ALARM
               END-EVALUATE
               GO TO GG05
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE "0"  TO WS-OPTION
                                 GO TO GG18
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO GG05
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X  TO WS-OPTION
               CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS
               IF (WS-OPTION < 0 OR > 8) AND (WS-OPTION < "A" OR > "D")
                   PERFORM AA900-ALARM
                   GO TO GG05
               END-IF
               GO TO GG16
           ELSE
               PERFORM AA900-ALARM
               GO TO GG05.

       GG15.
             EVALUATE SCREEN-LIN
               WHEN 13     MOVE "1"  TO WS-OPTION
               WHEN 14     MOVE "2"  TO WS-OPTION
               WHEN 15     MOVE "3"  TO WS-OPTION
               WHEN 16     MOVE "4"  TO WS-OPTION
               WHEN 17     MOVE "5"  TO WS-OPTION
               WHEN 18     MOVE "6"  TO WS-OPTION
               WHEN 19     MOVE "7"  TO WS-OPTION
               WHEN 20     MOVE "8"  TO WS-OPTION
               WHEN 21     MOVE "A"  TO WS-OPTION
               WHEN 22     MOVE "B"  TO WS-OPTION
               WHEN 23     MOVE "C"  TO WS-OPTION
               WHEN 24     MOVE "D"  TO WS-OPTION
               WHEN 25     MOVE "0"  TO WS-OPTION
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO CC05
             END-EVALUATE.
             GO TO GG18.

       GG16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "0"    MOVE 25   TO SCREEN-LIN
               WHEN "1"    MOVE 13   TO SCREEN-LIN
               WHEN "2"    MOVE 14   TO SCREEN-LIN
               WHEN "3"    MOVE 15   TO SCREEN-LIN
               WHEN "4"    MOVE 16   TO SCREEN-LIN
               WHEN "5"    MOVE 17   TO SCREEN-LIN
               WHEN "6"    MOVE 18   TO SCREEN-LIN
               WHEN "7"    MOVE 19   TO SCREEN-LIN
               WHEN "8"    MOVE 20   TO SCREEN-LIN
               WHEN "A"    MOVE 21   TO SCREEN-LIN
               WHEN "B"    MOVE 22   TO SCREEN-LIN
               WHEN "C"    MOVE 23   TO SCREEN-LIN
               WHEN "D"    MOVE 24   TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       GG18.
             EVALUATE WS-OPTION
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                          E X I T                          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN "0"    GO TO GG999
               WHEN "1"    MOVE "UTP\PRP050" TO PRG-NAME
               WHEN "2"    MOVE "UTP\PRP051" TO PRG-NAME
               WHEN "3"    MOVE "UTP\PRP052" TO PRG-NAME
               WHEN "4"    MOVE "UTP\PRP053" TO PRG-NAME
               WHEN "5"    MOVE "UTP\PRP054" TO PRG-NAME
               WHEN "6"    MOVE "UTP\PRP055" TO PRG-NAME
               WHEN "7"    MOVE "UTP\PRP056" TO PRG-NAME
               WHEN "8"    MOVE "UTP\PRP057" TO PRG-NAME
               WHEN "A"    MOVE "UTP\PRP058" TO PRG-NAME
               WHEN "B"    IF LS0-LEV < 3
                               MOVE "MNU\NOPMENU" TO PRG-NAME
                           ELSE
                               MOVE "UTP\PRP061"  TO PRG-NAME
                           END-IF
               WHEN "C"    IF LS0-LEV < 3
                               MOVE "MNU\NOPMENU" TO PRG-NAME
                           ELSE
                               MOVE "UTP\PRP062"  TO PRG-NAME
                           END-IF
               WHEN "D"    IF LS0-LEV < 3
                               MOVE "MNU\NOPMENU" TO PRG-NAME
                           ELSE
                               MOVE "UTP\PRP063"  TO PRG-NAME
                           END-IF
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                       I N V A L I D                       Ё
      *    Ё   Added test as a precaution. It should never happen.     Ё   
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO GG05
             END-EVALUATE.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.
             GO TO GG05.

       GG999.
             EXIT.

       GH000        SECTION 7.
       GH00.
           IF LS0-LEV < 3
               MOVE "MNU\NOPMENU"    TO PRG-NAME
           ELSE
               MOVE "UTP\PRP059"     TO PRG-NAME.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.
       GH999.
             EXIT.

       GI000        SECTION 7.
       GI00.
           IF LS0-LEV < 3
               MOVE "MNU\NOPMENU"    TO PRG-NAME
           ELSE
               MOVE "UTP\PRP060"     TO PRG-NAME.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.
       GI999.
             EXIT.

       GJ000        SECTION 7.
       GJ00.
           IF LS0-LEV < 3
               MOVE "MNU\NOPMENU"    TO PRG-NAME
           ELSE
               MOVE "UTP\PRP063"     TO PRG-NAME.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.
       GJ999.
             EXIT.

       GK000        SECTION 7.
       GK00.
           IF LS0-LEV < 3
               MOVE "MNU\NOPMENU"    TO PRG-NAME
           ELSE
               MOVE "UTP\PRP064"     TO PRG-NAME.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.
       GK999.
             EXIT.

       HA000        SECTION 7.
       HA00.
           IF LS-DSKDRV = "L"
               MOVE "Not available on Local drive" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO HA999.
             MOVE "UTP\AMDSTAT"      TO PRG-NAME.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.
       HA999.
             EXIT.
      *
      *    ****    D E L I V E R Y   R O U T E S
      *
       IA000        SECTION 7.
       IA00.
           IF LS-DSKDRV = "L"
               MOVE "Not available on Local drive" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO IA999.
           IF (LS0-LEV < 3)
               MOVE "MNU\NOPMENU"    TO PRG-NAME
           ELSE
               MOVE "UTP\CRDPRG"     TO PRG-NAME.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.
       IA999.
             EXIT.
      *
       ZA000-INIT              SECTION 9.
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

       ZA00A-CONTINUE.

       ZA999.
             EXIT.

       I-O-ERRORS      SECTION  9.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
       
      *    IF WS-F-ERROR = 15
      *        MOVE WS-PARID  TO WS-FILE
      *        MOVE WS-PARKEY  TO WS-KEY.

       COPY "DISPERR.PRO".
