      $set LINKCOUNT"256" GNT"BUP000.GNT"
      ******************************************************************
      *                                                                *
      *   *******   **    **  *******     ****      ****      ****     *
      *   **    **  **    **  **    **   **  **    **  **    **  **    *
      *   **    **  **    **  **    **  **    **  **    **  **    **   *
      *   *******   **    **  *******   **    **  **    **  **    **   *
      *   **    **  **    **  **        **    **  **    **  **    **   *
      *   **    **  **    **  **         **  **    **  **    **  **    *
      *   *******    ******   **          ****      ****      ****     *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     ( A P A C )   S T A R T   &   I N I T    P R O G R A M     *
      *                                                                *
      *       Version 9.04.04 - February/March 2018                    *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  Feb 2007 - New file (DEBDEP/AR_REST.DAT) - Debtor sales       *
      *             restrictions file has been included which          *
      *             will allow for departments to de excluded or       *
      *             only selected departments allowed. This will       *
      *             allow the user the abillity to allow or bar        *
      *             sales of selected departments.                     *
      *                                                                *
      *  Jan 2008 - New file (DEBALT) - Debtor alternate index         *
      *             included for lookups, using any word con-          *
      *             tained in the Debtor name.                         *
      *                                                                *
      *  Aug 2008 - New file (ADJREF) - Stock adjustments file         *
      *             included for lookups of references used for        *
      *             receiving stock via adjustment program.            *
      *                                                                *
      *           - New file (STKALT) - Stock alternate index          *
      *             included for lookups, using any word con-          *
      *             tained in the Stock description.                   *
      *                                                                *
      *  Mar 2010 - Include facility to print to a USB printer         *
      *             via the Windows print using the relavent           *
      *             print API's supplied with MF COBOL. Allow          *
      *             BMP files in APACFILE (INVBMP & STMBMP).           *
      *                                                                *
      *  May 2010 - Include facility to print sales by depart-         *
      *             ment for Debtors accounts. New work files          *
      *             "DEALER" & "DEATOT" included for extracting        *
      *             the data from the Cardex file.                     *
      *                                                                *
      *  Jul 2013 to                                                   *
      *  Jul 2015 - System includes new fields in Debtors, Creditors,  *
      *             Recovery,                                          *
      *             Introduced linking file identifications rather     *
      *             each program having to read the path file to get   *
      *             them.                                              *
      *             Consolodation of routines into separate programs   *
      *             which are called by the individual programs.       *
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
      *    Ё03 SEC-MODULES.    Ё03  WS0-MODULES.   Ё03 LS0-MODULES.    Ё
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
       PROGRAM-ID.     BUP000.
       AUTHOR.         J.W.LEMMON.
       DATE-WRITTEN.   OCTOBER 1982.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1982 - 2018
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

       COPY "APACFILE.SL".

       COPY "APCCOM.SL".

       COPY "PARAM.SL".

       COPY "PRNTCONT.SL".

       COPY "SYSTEM.SL".

           SELECT PRNREP  ASSIGN W02-PRINTER
                          ORGANIZATION LINE SEQUENTIAL.

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

       COPY "APACFILE.FD".

       COPY "APCCOM.FD".

       COPY "PARAM.FD".

       COPY "PRNTCONT.FD".

       COPY "SYSTEM.FD".

      *
       FD  PRNREP      LABEL RECORD OMITTED
                       LINAGE WS-PGE-LENGTH.
       01  REP-L1.
           03  REP-DETAIL            PIC  X(132).
       01  REP-L2.
           03  FILLER                PIC  X(28).
           03  REP-HEAD              PIC  X(24).
           03  FILLER                PIC  X(28).
       01  REP-L3.
           03  REP-GRPH              OCCURS 4.
               05  FILLER            PIC  X(01).
               05  REP-H1            PIC  X(06).
               05  REP-H2            PIC  X(06).
               05  REP-H3            PIC  X(06).
               05  REP-DIV           PIC  X(01).
       01  REP-L4.
           03  REP-ASCII             OCCURS 4.
               05  FILLER            PIC  X(01).
               05  REP-DEC           PIC  Z99.
               05  FILLER            PIC  X(03).
               05  REP-HEX           PIC  X(02).
               05  FILLER            PIC  X(04).
               05  REP-GRAPH.
                   07  FILLER        PIC  X(02).
                   07  REP-ALPH      PIC  X(01).
                   07  FILLER        PIC  X(04).
       01  SYS-L1.
           03  FILLER                PIC  X(05).
           03  SYS-H1                PIC  X(09).
           03  FILLER                PIC  X(03).
           03  SYS-H2                PIC  X(05).
           03  FILLER                PIC  X(03).
           03  SYS-H3                PIC  X(09).
           03  FILLER                PIC  X(03).
           03  SYS-H4                PIC  X(07).
           03  FILLER                PIC  X(36).
       01  SYS-L2.
           03  FILLER                PIC  X(07).
           03  SYS-FILE              PIC  X(06).
           03  FILLER                PIC  X(06).
           03  SYS-DRIVE             PIC  X(01).
           03  FILLER                PIC  X(08).
           03  SYS-EXT               PIC  X(03).
           03  FILLER                PIC  X(07).
           03  SYS-SIZE              PIC  Z(04)9.
           03  FILLER                PIC  X(37).
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
       77  WS-CHECK                  PIC  X(18)          VALUE "aeWlimemnomLalismJ".
       77  WS-S1                     PIC  9(04)    COMP-5.
       77  WS-S2                     PIC  9(04)    COMP-5.
       77  WS-S3                     PIC  9(04)    COMP-5.
       77  WS-S4                     PIC  9(04)    COMP-5.
       77  WS-S6                     PIC  9(04)    COMP-5.
       77  WS-SUB-6                  PIC  9(04)    COMP-5.
       77  WS-C1                     PIC  9(04)    COMP-5.
       77  WS-C2                     PIC  9(04)    COMP-5.
       77  WS-C3                     PIC  9(04)    COMP-5.
       77  WS-C4                     PIC  9(04)    COMP-5.
       77  WS-IXS1                   PIC  9(04)    COMP-5.
       77  WS-SECKEY                 PIC  9(04)    COMP-5.
       77  WS-PARKEY                 PIC  9(04)    COMP-5.
       77  WS-PRNKEY                 PIC  9(04)    COMP-5.
       77  WS-REG                    PIC  9(02)    COMP-5.
       77  WS-DTE                    PIC  9(08)    COMP-3.
       77  WS-LEV                    PIC  9(01).
       77  WS-SERIAL                 PIC  X(07).
       77  WS-TERMS                  PIC  9(04)    COMP-5.
       77  WS-NUM                    PIC  Z(04)9.
       77  WS-CONTRACT               PIC  X(01)          VALUE SPACE.
       77  WS-OPTION                 PIC  X(01).
       77  WS-INIT                   PIC  9(01)          VALUE ZERO.
       77  WS-ERROR                  PIC  9(01)          VALUE ZERO.
       77  WS-PGE-LENGTH             PIC  9(02)          VALUE 66.
       77  WS-PRN-LENGTH             PIC  9(02)          VALUE 62.
       77  WS-RESULT                 PIC  9(02)    COMP-X.
       77  WS-FUNCTION               PIC  9(02)    COMP-X.
       77  WS-CHAR                   PIC  X(01).
       77  WS-PAR                    PIC  9(01)          VALUE 0.
       77  WS-START                  PIC  9(08).
       77  WS-END                    PIC  9(08).
       77  WS-XXX                    PIC  X(07)          VALUE "XXXXXXX".
       77  WS-PACK                   PIC  X(08).
      *    здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Used by calling program (WORKING-STORAGE) and called program Ё
      *    Ё (LINKAGE SECTION) for Main screen layout and headings.       Ё
      *    юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "BUP000".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "SUPERVISOR: STARTUP/INITIALIZE PROGRAM".
      *
      *    Company Name
      *
           03  CRT-COMPANY         PIC  X(40) VALUE "NOT SELLECTED YET".
      *
      *    WorkStation / Supervisor
      *
           03  CRT-TERMINAL.
               05  CRT-WRKHD       PIC  X(11) VALUE "Supervisor ".
               05  CRT-WRKST       PIC  X(03) VALUE "   ".

       01  W00-ALPHA-SER.
           03  W00-SERIAL            PIC  X(07).

       COPY "WS.WS".

       COPY "CHAIN.WS".

       COPY "FILE-BUP.IDL".

           03  W02-APACFILE.
               05  W02-N-OR-L        PIC  X(08) VALUE "SYSTEMS\".
               05  W02-SYSPTH        PIC  X(03).
               05  FILLER            PIC  X(10) VALUE "_FILE.APC".

           03  W02-PRINTER.
               05  W02-REPORT        PIC  X(07)    VALUE "BUP000.".
               05  W02-USER          PIC  X(05)    VALUE SPACES.

           03  W02-RENAME-FILE       PIC  X(64).

           03  W02-NEW-FILE          PIC  X(64).

           03  W02-WORK-PTH.
               05  W02-COMP          PIC  X(03).
               05  FILLER            PIC  X(05)    VALUE "_WORK".
               05  FILLER            PIC  X(01)    VALUE X"00".

           03  W02-CHECK-SYS.
               05  W02-CHK-ID        PIC  X(03)    VALUE SPACES.
               05  FILLER            PIC  X(16)    VALUE "_DATA\SYSTEM.DAT".
               05  FILLER            PIC  X(01)    VALUE X"00".

           03  W02-OFFLINE.
               05  FILLER            PIC  X(07)    VALUE "OFFLINE".
               05  FILLER            PIC  X(01)    VALUE X"00".

       01  W05-COMMAND.
           03  W05-CHAR              PIC  X(01)    OCCURS 128.

       01  W05-DETAIL.
           03  W05-DC                PIC  X(01)    OCCURS 2.

       01  W05-COM                                 REDEFINES W05-DETAIL.
           03  W05-DRW.
               05  W05-DRAW          PIC  9(02).

       01  W10-FILE-PATHS.
           03  W10-CURRENT-PATH      PIC  X(52).

           03  W10-CURR-PATH                       REDEFINES W10-CURRENT-PATH.
               05  W10-CURR-CHAR     PIC  X(01)    OCCURS 52.
           03  W10-WRK-PATH          PIC  X(52).

           03  W10-WK-PATH                         REDEFINES W10-WRK-PATH.
               05  W10-WRK-CHAR      PIC  X(01)    OCCURS 52.
           03  W10-PATH              PIC  X(09).

           03  W10-COM-PATH                        REDEFINES W10-PATH.
               05  W10-COM-CHAR      PIC  X(01)    OCCURS 9.

      *COPY "W12.WS".
      *
      *    Date routines for accepting checking and formatting have
      *    been removed from each program and the DateCheck program
      *    will be called to handle these routines.
      *
      *    The following copy 'DateVariables' now includes the 'W12.WS'
      *    and 'W20.WS' variables and is passed to DateCheck in the CALL
      *    statement.
      *
       COPY "DateVariables.CPY".

       01  W15-HEX.
           03  W15-ASCII             PIC  X(01)    OCCURS 256.

       01  W25-CALCS.
           03  W25-RESULT            PIC  9(09)V99.

           03  W25-RESULT1                         REDEFINES W25-RESULT.
               05  W25-DAYS          PIC  9(03).
               05  W25-WHOLE         PIC  9(06).
               05  W25-DECIMAL       PIC  9(02).

           03  W25-RESULT2                         REDEFINES W25-RESULT.
               05  FILLER            PIC  9(02).
               05  W25-KEY           PIC  9(04).
               05  W25-SUB           PIC  9(01).
               05  FILLER            PIC  9(02).
           03  W25-PASS.
               05  W25-PASS1         PIC  9(03).
               05  W25-PASS2         PIC  9(03).
           03  W25-TIME              PIC  9(08).

           03  W25-TRED                            REDEFINES  W25-TIME.
               05  W25-FACT          PIC  9(08).

               05  W25-DATE                        REDEFINES W25-FACT.
                   07  W25-CC        PIC  9(02).
                   07  W25-YY        PIC  9(02).
                   07  W25-MM        PIC  9(02).
                   07  W25-DD        PIC  9(02).

       01  W30-MONTHS.
           03  W30-MONTH-NAMES.
               05  W30-ENG           PIC  X(36)    VALUE "JanFebMarAprMayJunJulAugSepOctNovDec".

           03  W30-MONTH                           REDEFINES W30-MONTH-NAMES PIC X(03) OCCURS 12.

       01  W31-DAYS.
           03  W31-DAY-NUMBERS.
               05  W31-DAYNO         PIC  X(36)    VALUE "000031059090120151181212243273303334".

               05  W31-DN                          REDEFINES W31-DAYNO.
                   07  W31-NO        PIC  9(03)    OCCURS 12.
           03  W31-DTE1              PIC  9(06)    COMP-5.
           03  W31-DTE2              PIC  9(06)    COMP-5.

       01  W35-PASS.
           03  W35-PASS1             PIC  9(03).
           03  W35-PASS2             PIC  9(03).

       01  W35-PASSW                               REDEFINES W35-PASS.
           03  W35-PNUM              PIC  9(06).

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       01  W75-KEYS.
           03  W75-S                 PIC  9(02)    COMP-5.
           03  W75-S1                PIC  9(02)    COMP-5.
           03  W75-S2                PIC  9(02)    COMP-5.
           03  W75-COM               PIC  X(03)    OCCURS 18.

       01  W85-PASS.
           03  W85-PW                PIC  X(02).
           03  W85-SUPER             PIC  X(06)    OCCURS 9.
           03  W85-ENTRY             PIC  9(02)    COMP-0.
           03  W85-MARG              PIC  9(02)V99.
           03  FILLER                PIC  X(02).

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

       01  L05-COMMAND.
           03  L05-CHAR              PIC  X(128).

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

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                    A P A C   L O G O                      Ё
      *    Ё This is the logo displayed using graphics (ASCII) char-   Ё
      *    Ё acters and will later be changed to use a bitmat when the Ё
      *    Ё menus are changed to GUI.                                 Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "LOGO.CRT".

       COPY "MENUINST.CRT".

       01  S01.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Blue. 
               05  LINE  8 COLUMN  6 VALUE "                                                                    ".
               05  LINE  9 COLUMN  6 VALUE " APAC - Business System (Version 2016).                             " FOREGROUND-COLOR Red  HIGHLIGHT.
               05  LINE 10 COLUMN  6 VALUE " Copyright (C) 1982 - 2018  JW Lemmon.                              " FOREGROUND-COLOR Black.
               05  LINE 11 COLUMN  6 VALUE " This SOFTWARE is used under licence to JW Lemmon.                  " HIGHLIGHT.
               05  LINE 12 COLUMN  6 VALUE " It is a contravention of the copyright law to:                     " FOREGROUND-COLOR Black.
               05  LINE 13 COLUMN  6 VALUE "   USE this SOFTWARE without registering it.                        " HIGHLIGHT.
               05  LINE 14 COLUMN  6 VALUE "   HAVE additional copies other than your WORKING & SECURITY disks. " HIGHLIGHT.
               05  LINE 15 COLUMN  6 VALUE "   USE the SOFTWARE on more than one computer or for more than one  " HIGHLIGHT.
               05  LINE 16 COLUMN  6 VALUE "   Business.                                                        " HIGHLIGHT.
               05  LINE 17 COLUMN  6 VALUE " The SOFTWARE remains the property of James W Lemmon.               " FOREGROUND-COLOR Black.
               05  LINE 18 COLUMN  6 VALUE "                                                                    ".

           03  LINE 21 COLUMN  7 VALUE "Do you accept these conditions (Y/N)  " BACKGROUND-COLOR Blue FOREGROUND-COLOR Brown HIGHLIGHT.

       01  S-MENU.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE  8 COLUMN 30 VALUE "                     ".
               05  LINE  9 COLUMN 30 VALUE "   SYSTEM CONTROL    " FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 10 COLUMN 30 VALUE " 1"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 32 VALUE   " Run the System    ".
               05  LINE 11 COLUMN 30 VALUE " 2"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 32 VALUE   " Special Routines  ".
               05  LINE 12 COLUMN 30 VALUE " 9"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 32 VALUE   " Exit to Op-System ".
               05  LINE 13 COLUMN 30 VALUE "                     ".

       01  S-MENU2.
           02  BACKGROUND-COLOR Green FOREGROUND-COLOR Black.
               03  LINE  8 COLUMN 30 VALUE "                     ".
               03  LINE  9 COLUMN 30 VALUE "  SPECIAL ROUTINES   " FOREGROUND-COLOR Grey HIGHLIGHT.
               03  LINE 10 COLUMN 30 VALUE " 1"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 32 VALUE   " Offline Location  ".
               03  LINE 11 COLUMN 30 VALUE " 2"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 32 VALUE   " Initialize Files  ".
               03  LINE 12 COLUMN 30 VALUE " 3"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 32 VALUE   " Register System   ".
               03  LINE 13 COLUMN 30 VALUE " 4"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 32 VALUE   " Run the System    ".
               03  LINE 14 COLUMN 30 VALUE " 5"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 32 VALUE   " Print ASCII Table ".
               03  LINE 15 COLUMN 30 VALUE " 6"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 32 VALUE   " Clear W/Stat. Log ".
               03  LINE 16 COLUMN 30 VALUE " 7"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 32 VALUE   " Amend No of W/St  ".
               03  LINE 17 COLUMN 30 VALUE " 8"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 32 VALUE   " Renew Contract    ".
               03  LINE 18 COLUMN 30 VALUE " 9"                    FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 32 VALUE   " Exit to Op-System ".
               03  LINE 19 COLUMN 30 VALUE "                     ".

       01  S-COM.
           05  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE 10 COLUMN 29 VALUE "                                              ".
               05  LINE 11 COLUMN 29 VALUE "     C O M P A N Y   C O D E   L O O K U P    ".
               05  LINE 12 COLUMN 29 VALUE " дддбдддддддддддддддддддддддддддддддддддддддд ".
               05  LINE 13 COLUMN 29 VALUE "    Ё                                         ".
               05  LINE 14 COLUMN 29 VALUE "    Ё                                         ".
               05  LINE 15 COLUMN 29 VALUE "    Ё                                         ".
               05  LINE 16 COLUMN 29 VALUE "    Ё                                         ".
               05  LINE 17 COLUMN 29 VALUE "    Ё                                         ".
               05  LINE 18 COLUMN 29 VALUE "    Ё                                         ".
               05  LINE 19 COLUMN 29 VALUE "    Ё                                         ".
               05  LINE 20 COLUMN 29 VALUE "    Ё                                         ".
               05  LINE 21 COLUMN 29 VALUE "    Ё                                         ".
               05  LINE 22 COLUMN 29 VALUE "    Ё                                         ".
               05  LINE 23 COLUMN 29 VALUE "    Ё                                         ".
               05  LINE 24 COLUMN 29 VALUE "    Ё                                         ".
               05  LINE 25 COLUMN 29 VALUE "    Ё                                         ".
               05  LINE 26 COLUMN 29 VALUE "    Ё                                         ".
               05  LINE 27 COLUMN 29 VALUE "    Ё                                         ".
               05  LINE 28 COLUMN 29 VALUE "    Ё                                         ".
               05  LINE 29 COLUMN 29 VALUE "    Ё                                         ".
               05  LINE 30 COLUMN 29 VALUE "    Ё                                         ".
               05  LINE 31 COLUMN 29 VALUE "                                              ".

       01  SD-COM.
           03          COLUMN 30 PIC X(03) FROM COM-CODE FOREGROUND-COLOR Magenta BACKGROUND-COLOR Grey.
           03          COLUMN 34 PIC X(40) FROM COM-NAME FOREGROUND-COLOR Blue    BACKGROUND-COLOR Grey.

       01  S03.
           03  LINE 23 COLUMN  7 VALUE "Company Identity Code ?" FOREGROUND-COLOR Brown HIGHLIGHT.

       01  S04.
           03  LINE 23 COLUMN 31 PIC X(03) USING WS-SYS-ID AUTO FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S06.
           03  BACKGROUND-COLOR Red FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE  8 COLUMN 12 VALUE "Enter Password >      <".
               05  LINE 16 COLUMN  8 VALUE " NB. ALL terminals MUST have signed off before this routine ".
               05  LINE 17 COLUMN  8 VALUE " can be run.  If this  routine  is run  while terminals are ".
               05  LINE 18 COLUMN  8 VALUE " still attached, the  results  are  unpredictable and could ".
               05  LINE 19 COLUMN  8 VALUE " result in the LOSS OF DATA or worse if a recovery is run.  ".

       01  S07.
           03  LINE  8 COLUMN 28 PIC  X(06) TO W40-CSHPASS AUTO SECURE FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S08.
           03  LINE 17 COLUMN 32 PIC  X(07) TO W00-SERIAL AUTO SECURE FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S10.
           03  LINE  5 COLUMN 27 PIC Z9/99/9999 USING SEC-DATE AUTO FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S11.
           03  LINE 27 COLUMN  7 VALUE "Work Station id " FOREGROUND-COLOR Brown HIGHLIGHT.

       01  S12.
           03  LINE 27 COLUMN 23 PIC X(03) USING WS-USER AUTO FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S13.
           02  S13A.
               03  LINE  2 COLUMN 26 VALUE "OFFLINE FILE LOCATIONS"                         FOREGROUND-COLOR Grey HIGHLIGHT.
           02  S13B.
               03  LINE 12 COLUMN 18 VALUE "This routine will set up the offline locations" FOREGROUND-COLOR Brown HIGHLIGHT.
               03  LINE 14 COLUMN 18 VALUE "Press ANY key to continue"                      FOREGROUND-COLOR Brown HIGHLIGHT.

       01  S18.
       02  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03  LINE  6 COLUMN 16 VALUE "                                                 ".
           03  LINE  7 COLUMN 16 VALUE " Packaging and Marketing:                        " FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE  8 COLUMN 16 VALUE "        WORKGROUP DATA SOLUTIONS.                ".
           03  LINE  9 COLUMN 16 VALUE "        18 Milford Crest, North Riding, JHB.     ".
           03  LINE 10 COLUMN 16 VALUE "        PO Box 68416,                            ".
           03  LINE 11 COLUMN 16 VALUE "        BRYANSTON. 2021                          ".
           03  LINE 12 COLUMN 16 VALUE " Support:                                        " FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE 13 COLUMN 16 VALUE "   See the registration card, supplied with the  ".
           03  LINE 14 COLUMN 16 VALUE "   system, for details of the telephone and fax  ".
           03  LINE 15 COLUMN 16 VALUE "   numbers of the APAC support line.             ".
           03  LINE 16 COLUMN 16 VALUE "   For local support in your area please contact " FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE 17 COLUMN 16 VALUE "   the support line, who will in turn give you   " FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE 18 COLUMN 16 VALUE "   details of your local support centre.         " FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE 19 COLUMN 16 VALUE "                                                 ".

           03  LINE 22 COLUMN 17 VALUE "Please key in todays date (DDMMCCYY)" BACKGROUND-COLOR Blue FOREGROUND-COLOR Brown HIGHLIGHT.

       01  S20.
           03  BACKGROUND-COLOR Blue FOREGROUND-COLOR Brown HIGHLIGHT.
               05  LINE 24 COLUMN 29 VALUE "Todays date - ".
               05          COLUMN 43 PIC  Z9    FROM      W12-DD                FOREGROUND-COLOR Grey.
               05          COLUMN 45 VALUE                  " ". 
               05          COLUMN 46 PIC  X(03) FROM          W30-MONTH(W12-MM) FOREGROUND-COLOR Grey.
               05          COLUMN 49 VALUE                     ","              FOREGROUND-COLOR Grey. 
               05          COLUMN 50 PIC  9(04) FROM              W12-CY        FOREGROUND-COLOR Grey.
           
       01  S25.
           03  LINE  8 COLUMN  4 VALUE "Work Stations :" FOREGROUND-COLOR Brown HIGHLIGHT.
           03  LINE 10 COLUMN 10 VALUE "Package: ".
           03          COLUMN 20 PIC  X(08) FROM WS-PACK FOREGROUND-COLOR Cyan HIGHLIGHT.
           03  S25A.
               05  LINE 8 COLUMN 20 PIC ZZ9 USING WS-TERMS AUTO FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S30.
           03  LINE 12 COLUMN 50 PIC ZZ9 USING WS-TERMS AUTO FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.

       01  S-LEGAL.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red HIGHLIGHT.
               05  LINE 50 COLUMN  2 VALUE "This package must be registered within ".
               05          COLUMN 42 PIC Z9 USING WS-REG FOREGROUND-COLOR Grey HIGHLIGHT BLINK.
               05          COLUMN 44 VALUE " days".
               
      *
      *      ******   ******    *****    *****   ******  ******   **   **  ******    ****** 
      *      **   **  **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **   **  **  **   **   **  **       **      **   **  **   **  **  **    **
      *      ******   *****    **   **  **       *****   **   **  **   **  *****     *****
      *      **       **  **   **   **  **       **      **   **  **   **  **  **    **
      *      **       **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **       **   **   *****    *****   ******  ******    *****   **   **   ******
      *
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS L05-COMMAND DATA-FILE-IDS.
       AA000-MAIN              SECTION.
       AA00.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё This routine has now been upgraded to the latest COBOL    Ё
      *    Ё syntax regarding date.                                    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
             ACCEPT W12-T-YMD FROM DATE YYYYMMDD.
             MOVE W12-CC             TO W12-CENT.
             MOVE W12-YY             TO W12-YEAR.
             MOVE W12-MM             TO W12-MONTH.
             MOVE W12-DD             TO W12-DAY.
             MOVE W12-TODAY          TO TODAY-DDMMYY.
             PERFORM AL000.
             PERFORM DISPLAY-THE-CURSOR.
             MOVE W05-DRAW           TO WS-DRAW.

       AA02.
             PERFORM ERASE-SCREEN.
             MOVE 8                  TO SHADE-ROW.
             MOVE 6                  TO SHADE-COL.
             MOVE 68                 TO SHADE-WIDTH.
             MOVE 11                 TO SHADE-LINES.
             DISPLAY S01.
             PERFORM SCREEN-SHADOW.
             PERFORM AD000.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё              Is a Data File recovery required?            Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
           IF WS-PAR = 9
               MOVE PAR-PROG(WS-S1)   TO WS0-RECV
               CLOSE PARAM SECUR
               CALL "MNU\APACRECOVER" USING WS-PARID WS-USER-ID WS0-PROGRAMS WS0-SECURITY DATA-FILE-IDS
               CANCEL "MNU\APACRECOVER"
               OPEN I-O PARAM
               OPEN I-O SECUR
               GO TO AA02.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66               TO MOUSE-FUNC
               MOVE 1                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
             MOVE 8                  TO SHADE-ROW.
             MOVE 30                 TO SHADE-COL.
             MOVE 21                 TO SHADE-WIDTH.
             MOVE 6                  TO SHADE-LINES.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                   The System Start Menu                   Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
             DISPLAY S-MENU.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
             MOVE 9                  TO TOP-ROW.
             MOVE 11                 TO BOTTOM-ROW.
             MOVE 8                  TO SCREEN-LIN.
             MOVE 31                 TO SCREEN-COL.
             MOVE 18                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.
             MOVE 2                  TO WS-SECKEY.
             READ SECUR.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё    C H E C K   T O   S E E   I F   T H E   A N N U A L    Ё
      *    Ё    C O N T R A C T   M U S T   B E   R E N E W E D        Ё
      *    Ё        Applies to Users with a maintenance Contract       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
           IF SEC-AGREE = "R"
               IF NOT(SEC-DAYS = 255)
                   MOVE "Contract must be renewed" TO WS-DSP-MES
                   IF SEC-DAYS > 15
                       MOVE SPACES      TO WS-DSP-MES-2
                       MOVE SEC-ANDATE  TO W20-DTE
                       STRING "By - " DELIMITED SIZE W20-DD DELIMITED SIZE " " DELIMITED SIZE
                               W30-MONTH(W12-MM) DELIMITED SIZE "," DELIMITED SIZE W12-CC DELIMITED SIZE
                               W12-YY DELIMITED SIZE INTO WS-DSP-MES-2
                       PERFORM DISPLAY-MESSAGE-2
                   ELSE
                       PERFORM DISPLAY-MESSAGE.

       AA05.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       AA10.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     PERFORM MOVE-BLOCK-UP
                 
                 WHEN DOWN-KEY   PERFORM MOVE-BLOCK-DOWN
                 
                 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
                                 IF MOUSE-ENTER = "Y"
                                     MOVE "N"    TO MOUSE-ENTER
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
                 
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO AA05
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X      TO WS-OPTION
               IF NOT(WS-OPTION = 1 OR 2 OR 9)
                   PERFORM AA900-ALARM
                   GO TO AA05
               END-IF
               GO TO AA16
           ELSE
               PERFORM AA900-ALARM
               GO TO AA05.

       AA15.
             EVALUATE SCREEN-LIN
               WHEN 9            MOVE "1"        TO WS-OPTION
               
               WHEN 10           MOVE "2"        TO WS-OPTION
               
               WHEN 11           MOVE "9"        TO WS-OPTION
               WHEN OTHER        GO TO AA05
             END-EVALUATE.
             GO TO AA18.

       AA16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "9"          MOVE 11         TO SCREEN-LIN
               
               WHEN "1"          MOVE 9          TO SCREEN-LIN
               
               WHEN "2"          MOVE 10         TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       AA18.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66               TO MOUSE-FUNC
               MOVE 0                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
           IF WS-OPTION = "9"
               MOVE 6                TO WS-ERROR
               GO TO AZ000-EOJ.
           IF WS-OPTION = "1"
               PERFORM AI000
      *
      *    ****  Was the Escape key pressed?
      *         
               IF (W22-INSTR = X"EC")
                   MOVE "E"          TO LS-INSTR
                   GO TO AZ99
               ELSE    
               IF WS-ERROR = ZERO
                   GO TO AZ000-EOJ.

       AA99.
             EXIT.
      *
      *    THE FOLLOWING SECTION IS NOT PERFORMED - The above section
      *    drops through to AB000
      *
       AB000     SECTION.
       AB00.
             MOVE 2                  TO WS-SECKEY.
             READ SECUR.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66               TO MOUSE-FUNC
               MOVE 1                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
             MOVE "SUPERVISOR: STARTUP/INITIALIZE PROGRAM" TO CRT-HEADING.
             PERFORM HEADING-AND-CLEAR-SCREEN.
             MOVE 8                  TO SHADE-ROW.
             MOVE 30                 TO SHADE-COL.
             MOVE 21                 TO SHADE-WIDTH.
             MOVE 12                 TO SHADE-LINES.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё              The System Special Routines Menu             Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
             DISPLAY S-MENU2.
             DISPLAY MENU-INSTRUCT.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё If the USER of the system is not on an annual contract    Ё
      *    Ё then change the 'Renew Contract' menu item to Not Applic- Ё
      *    Ё able and set WS-CONTRACT to 'N'.                          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
           IF NOT(SEC-AGREE = "R") OR (SEC-AGREE = "R" AND SEC-DAYS = 255)
               MOVE "N"              TO WS-CONTRACT
               DISPLAY " (N/A)" AT 1744 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Green.
             PERFORM SCREEN-SHADOW.

       AB02.
             MOVE 9                  TO TOP-ROW.
             MOVE 17                 TO BOTTOM-ROW.
             MOVE 8                  TO SCREEN-LIN.
             MOVE 31                 TO SCREEN-COL.
             MOVE 18                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       AB05.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       AB10.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     PERFORM MOVE-BLOCK-UP
                 
                 WHEN DOWN-KEY   PERFORM MOVE-BLOCK-DOWN
                 
                 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
                                 IF MOUSE-ENTER = "Y"
                                     MOVE "N"    TO MOUSE-ENTER
                                     GO TO AB15
                                     END-IF
                                 IF MOUSE-Y = 24
                                     IF MOUSE-X = 5
                                         MOVE 5  TO KEY-CODE-1
                                         GO TO AB10
                                     ELSE
                                     IF MOUSE-X = 10
                                         MOVE 6  TO KEY-CODE-1
                                         GO TO AB10
                                     ELSE
                                     IF MOUSE-X > 36 AND < 44
                                         MOVE 0  TO KEY-CODE-1
                                         GO TO AB10
                                     ELSE
                                     IF MOUSE-X > 54 AND < 60
                                         MOVE 0  TO KEY-CODE-1
                                         MOVE 1  TO KEY-TYPE
                                         GO TO AB10
                                     END-IF
                                 END-IF
                                 
                 WHEN ENTER-KEY  GO TO AB15
                 
                 WHEN OTHER      PERFORM AA900-ALARM
          END-EVALUATE
          GO TO AB05
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X  TO WS-OPTION
               IF (WS-OPTION < 1 OR > 9)
                   PERFORM AA900-ALARM
                   GO TO AB05
          END-IF
          GO TO AB16
           ELSE
               PERFORM AA900-ALARM
               GO TO AB05.

       AB15.
             EVALUATE SCREEN-LIN
               WHEN 9            MOVE "1"        TO WS-OPTION
               
               WHEN 10           MOVE "2"        TO WS-OPTION
               
               WHEN 11           MOVE "3"        TO WS-OPTION
               
               WHEN 12           MOVE "4"        TO WS-OPTION
               
               WHEN 13           MOVE "5"        TO WS-OPTION
               
               WHEN 14           MOVE "6"        TO WS-OPTION
               
               WHEN 15           MOVE "7"        TO WS-OPTION
               
               WHEN 16           MOVE "8"        TO WS-OPTION
               
               WHEN 17           MOVE "9"        TO WS-OPTION
               WHEN OTHER        GO TO AB05
             END-EVALUATE.
             GO TO AB18.

       AB16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "9"          MOVE 17         TO SCREEN-LIN
               
               WHEN "1"          MOVE 9          TO SCREEN-LIN
               
               WHEN "2"          MOVE 10         TO SCREEN-LIN
               
               WHEN "3"          MOVE 11         TO SCREEN-LIN
               
               WHEN "4"          MOVE 12         TO SCREEN-LIN
               
               WHEN "5"          MOVE 13         TO SCREEN-LIN
               
               WHEN "6"          MOVE 14         TO SCREEN-LIN
               
               WHEN "7"          MOVE 15         TO SCREEN-LIN
               
               WHEN "8"          MOVE 16         TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       AB18.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66               TO MOUSE-FUNC
               MOVE 0                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
             EVALUATE WS-OPTION
               WHEN "9"          MOVE 6    TO WS-ERROR
                                 GO TO AZ000-EOJ

               WHEN "1"          CALL "UTP\OFFLFILE" USING WS-PARID WS-USER-ID WS0-PROGRAMS WS0-SECURITY
                                 CANCEL "UTP\OFFLFILE"

               WHEN "2"          CALL "MNU\APACINITIAL" USING WS-PARID WS-USER-ID WS0-PROGRAMS WS0-SECURITY BUP-FILE-IDS WS-ERROR W12-DATE
                                 CANCEL "MNU\APACINITIAL"
                                 IF WS-ERROR = 9
                                     GO TO AZ000-EOJ

               WHEN "3"          CALL "MNU\APACREGISTER" USING WS-PARID WS-USER-ID WS0-PROGRAMS WS0-SECURITY BUP-FILE-IDS WS-LEV WS-SERIAL
                                 CANCEL "MNU\APACREGISTER"

               WHEN "4"          PERFORM AI000
                                 IF WS-ERROR = ZERO
                                     GO TO AZ000-EOJ

               WHEN "5"          CALL "UTP\ASCIIREP" USING WS-PARID WS-USER-ID WS0-PROGRAMS WS0-SECURITY
                                 CANCEL "UTP\ASCIIREP"

               WHEN "6"          PERFORM AM000

               WHEN "7"          PERFORM AN000

               WHEN "8"          IF NOT(WS-CONTRACT = "N")
                                     PERFORM AO000

               WHEN OTHER        GO TO AB05
             END-EVALUATE.
             GO TO AB00.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё       D A T E   V A L I D A T I O N   R O U T I N E       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *AB000-DATE              SECTION.
      *AB000-VALID-DATE.
      *      MOVE ZERO               TO WS-ERROR.
      *    IF W20-DAY < 01 OR > 31
      *        GO TO AB010-ERROR.
      *    IF W20-MONTH < 01 OR > 12
      *        GO TO AB010-ERROR.
      *    IF W20-MONTH = 01 OR 03 OR 05 OR 07 OR 08 OR 10 OR 12
      *        GO TO AB999-EXIT.
      *    IF W20-MONTH = 02 AND W20-DAY > 28
      *        GO TO AB005-FEB.
      *    IF W20-DAY > 30
      *        GO TO AB010-ERROR.
      *      GO TO AB999-EXIT.

      *AB005-FEB.
      *    IF W20-DAY > 29
      *        GO TO AB010-ERROR.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё    Check for a leap year if February and the day is 29    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *      DIVIDE W20-CY BY 4  GIVING W25-RESULT.
      *    IF W25-DECIMAL = 0
      *        GO TO AB999-EXIT.*

      *AB010-ERROR.
      *      MOVE 1                  TO WS-ERROR.

      *AB999-EXIT.
      *      EXIT.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               E N D   O F F   P R O G R A M               Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       AZ000-END        SECTION.
       AZ000-EOJ.
           IF WS-DEM > 2
               MOVE 2                TO WS-SECKEY
               READ SECUR
               MOVE 2                TO SEC-DEM
               MOVE ZERO             TO SEC-DATE
               REWRITE SEC-REC2.
           IF WS-INIT NOT = 2
               GO TO AZ05.
             CLOSE PARAM.

       AZ05.
             CLOSE SECUR.
             PERFORM ERASE-SCREEN.
             MOVE WS-ERROR           TO WS-RET.
             MOVE WS-PARID           TO LS-PARID.
             MOVE WS-USER-ID         TO LS-USER-ID.
             MOVE WS0-PROGRAMS       TO LS0-PROGRAMS.
           IF WS-ERROR = ZERO
               MOVE "C"              TO LS-INSTR
               MOVE SPACES           TO LS-NEXT-PRG
           ELSE
           IF WS-ERROR = 9
               MOVE "I"              TO LS-INSTR
               MOVE "UTP\APACINST"   TO LS-NEXT-PRG
           ELSE
               MOVE "E"              TO LS-INSTR.

       AZ99.
               EXIT PROGRAM.

       COPY "AA900.ALM".
             
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
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "CLEARW.CRT".

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
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё                      CHECK-CORRECT                        Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё This routine displays a pop-up window with the message    Ё
      *    Ё           "Press Y if correct - N if incorrect"           Ё
      *    Ё                                                           Ё
      *    Ё The response is returned in WS-OPTION (in upper case).    Ё
      *    Ё                                                           Ё
      *    Ё To start with the pop-up window higher or lower than row  Ё
      *    Ё 20 (default); move another value to WS-MES-LINE.          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *            T H I S   R O U T I N E   I S   U S E D   T O
      *       D I S P L A Y   I N F O R M A T I O N   M E S S A G E S
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Move the message to WS-DSP-MES and the top line of the   Ё
      *    Ё  message box to WS-MES-LINE. If WS-MES-LINE is zero the   Ё
      *    Ё  default will be 20.                                      Ё
      *    Ё                  PERFORM DISPLAY-MESSAGE                  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *           T H I S   R O U T I N E   I S   U S E D   T O
      *            D I S P L A Y   E R R O R   M E S S A G E S
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                      ERROR-MESSAGE                        Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To display the error message higher or lower (default is  Ё
      *    Ё line 20) Move the line number which must be used as the   Ё
      *    Ё top line to WS-MES-LINE. The message to be displayed must Ё
      *    Ё be in WS-ERR-MES. PERFORM ERROR-MESSAGE.                  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "FUNCTION.CRT".

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
      *    Ё                                                           Ё
      *    Ё If character 48 of the message contains a value of X"FA"  Ё
      *    Ё then do not display the message in a window, only get the Ё
      *    Ё User reponse at the position specified by WS-MES-LINE and Ё
      *    Ё WS-MES-COL.                                               Ё
      *    Ё                                                           Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "OPTION.CRT".

      *    *************************************************************
      *    ***** I N I T I A L I Z E   P R O G R A M    *****
      *    *************************************************************
       AD000        SECTION 5.
       AD00.
           IF LS-INSTR = "X"
               MOVE 21               TO CRT-START CRT-END
               PERFORM ERASE-SCREEN
               MOVE SPACE            TO LS-INSTR
               GO TO AD05.
             MOVE 21                 TO WS-MES-LINE.
             MOVE 44                 TO WS-MES-COL.
      *
      *      ACCEPT the option at the position specified above
      *
             MOVE X"FA"              TO WS-MES-CHAR(48).
             MOVE "1"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "N"
               STOP RUN.
             MOVE SPACES             TO WS-SYS-ID.
      *    *************************************************************
      *    ****       G E T   S Y S T E M   E X T E N S I O N   ****
      *    *************************************************************
       AD05.
             DISPLAY S03.
             DISPLAY "F2"                       AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     " to view existing codes"          WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             ACCEPT S04.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN F2-KEY OPEN INPUT APCCOM
                             PERFORM COMPANY-LOOKUP
                             CLOSE APCCOM
                             GO TO AD05

                 WHEN OTHER  PERFORM AA900-ALARM
                             GO TO AD05
               END-EVALUATE.
             PERFORM CLEAR-L50.
           IF WS-SYS-ID = SPACES
               GO TO AD05.
             MOVE 3                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING WS-SYS-ID BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.

       AD05-APAC.
             PERFORM APACFILES.
           IF NOT(WS-ERROR = 9)
               OPEN I-O SECUR.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   MOVE "Files locked - try later - Press ENTER" TO WS-ERR-MES
                   PERFORM ERROR-MESSAGE
                   STOP RUN.
      *    IF (WS-STAT1 = "9") AND (WS-STAT2H = 009 OR 199) AND (WS-DSKDRV = "N")
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё           F I L E S   N O T   A V A I L A B L E           Ё
      *    Ё  Check if this is a Network and if off-line files exist.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *        PERFORM AD200
      *        IF WS-ERROR = 0
      *            PERFORM AD100
      *            IF WS-OPTION = "Y"
      *                MOVE "OFFLINE\"   TO W02-N-OR-L W02-L-OR-N WS-L-OR-N
      *                MOVE "L"          TO WS-DSKDRV
      *                GO TO AD05-APAC
      *            ELSE
      *                MOVE "System Aborted- No network drive" TO WS-ERR-MES
      *                PERFORM ERROR-MESSAGE
      *                STOP RUN.
           IF WS-STATUS NOT = "00"
               MOVE "Invalid or file not on disk - 'E'xit or 'R'ekey" TO WS-OPT-MES
               MOVE 3                 TO LS-INSTR
               PERFORM OPT-MESSAGE
               IF WS-OPTION = "R"
                   GO TO AD05
               ELSE
                   STOP RUN.
             MOVE 2                  TO WS-SECKEY.

       AD005.
             READ SECUR.
           IF WS-STATUS = "00"
               GO TO AD005A.
           IF RUNTIME-ERROR
               IF RECORD-LOCKED
                   GO TO AD005.
             STRING "Invalid or file not on disk - " DELIMITED SIZE WS-STATUS DELIMITED SIZE INTO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.
             STOP RUN.

       AD005A.
             MOVE SEC-COMP           TO LS-COMPANY.
             DISPLAY SEC-COMP AT 2507 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
           IF (SEC-TERMS < 2 OR > 100)
               DISPLAY " Single USER System" AT 2547 WITH FOREGROUND-COLOR Grey HIGHLIGHT
           ELSE
               ADD 1 SEC-TERMS       GIVING WS-NUM
               DISPLAY " " AT 2547 WS-NUM WITH FOREGROUND-COLOR Grey HIGHLIGHT " WORK Stations" WITH FOREGROUND-COLOR Grey HIGHLIGHT.
               MOVE 27               TO WS-MES-LINE.
               PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               CLOSE SECUR
               GO TO AD05.
             MOVE SEC-COMP           TO CRT-COMPANY WS-COMPANY.
             MOVE SEC-SYS            TO APAC-SYSTEM WS-APAC-SYSTEM.
           IF NOT(SEC-VER = "2016")
               MOVE "2016"           TO SEC-VER
               REWRITE SEC-REC2.
             MOVE SEC-VER            TO APAC-VERSION WS-APAC-VERSION.
             PERFORM CLEAR-L50.
             PERFORM CHANGE-HEADING.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё           G E T   O P E R A T O R   N U M B E R           Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       AD05B.
             DISPLAY S11.

       AD05U.
             MOVE WS-SYS-ID          TO WS-USER.
             ACCEPT S12.
             MOVE 3                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING WS-USER BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             MOVE SEC-PMODS          TO WS0-PMODS.
           IF WS-USER = WS-SYS-ID
               MOVE "Supervisor"     TO CRT-WRKHD
           ELSE
               MOVE "Workstation"    TO CRT-WRKHD
               MOVE WS-USER          TO CRT-WRKST.
             MOVE CRT-TERMINAL       TO WS-TERMINAL.  
             MOVE SEC-DEM            TO WS-DEM.
           IF (SEC-DTP NOT = 2) AND (SEC-STP NOT = 2) AND (SEC-CRP NOT = 2) AND (SEC-GLP NOT = 2) AND
              (SEC-POS NOT = 2) AND (SEC-HPD NOT = 2) AND (SEC-JCP NOT = 2) AND (SEC-VHP NOT = 2)
               MOVE "LPT1"           TO W02-PRINTER
               GO TO AD08.
             OPEN I-O PARAM.
           IF RUNTIME-ERROR AND FLE-LOCKED
               MOVE SPACES           TO WS-ERR-MES
               STRING "Files locked - try later - " DELIMITED SIZE WS-PARID DELIMITED SIZE INTO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               STOP RUN.
             MOVE 2                  TO WS-INIT.
           IF WS-STATUS NOT = "00"
               GO TO AD06.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё             CHECK IF LAST RUN ENDED CORRECTLY             Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
             MOVE 4                  TO WS-PARKEY.

       AD05-PAR.
             READ PARAM LOCK.
           IF RUNTIME-ERROR
               IF RECORD-LOCKED
                   GO TO AD05-PAR.
           IF PAR-USERS = 0
               MOVE 1                TO WS-PAR PAR-LOGON
               GO TO AD07U.
             MOVE 1                  TO WS-S1.
             MOVE 0                  TO WS-S2 WS-S3.

       AD05-CHECK.
           IF PAR-USR(WS-S1) = WS-USER
               IF PAR-PROG(WS-S1) = SPACES
                   MOVE SPACES       TO PAR-USR(WS-S1)
                   SUBTRACT 1        FROM PAR-USERS
               ELSE
                   GO TO AD06.
           IF WS-S1 < 24
               ADD 1                 TO WS-S1
               GO TO AD05-CHECK.
             MOVE 1                  TO PAR-LOGON.
             GO TO AD07U.

       AD06.
             PERFORM ERASE-SCREEN.
             DISPLAY "Last run not ended off correctly"                            AT 0814 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
           IF PAR-PROG (WS-S1) = "99"
               DISPLAY "Reload files and redo any updating done since last BACKUP" AT 1014 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Red HIGHLIGHT
               STOP RUN.
             DISPLAY "Recovery procedure will now be initiated"                    AT 1014 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Press ANY key to continue"                                   AT 1214 WITH FOREGROUND-COLOR Brown HIGHLIGHT.

       AD07.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             MOVE 9                  TO WS-PAR.
             UNLOCK PARAM.
             GO TO AD99.

       AD07U.
             REWRITE PAR-RECORD4.
             UNLOCK PARAM.

       AD07W.
             MOVE 1              TO WS-PARKEY.
             READ PARAM.
           IF RUNTIME-ERROR
               IF RECORD-LOCKED
                   GO TO AD07W.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё C H E C K  I F   D E M O N S T R A T I O N   S Y S T E M Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
           IF (SEC-DEM = 1) OR (PAR-STAT = 1)
               IF SEC-COUNT < 75
                   PERFORM AD15
               END-IF
           ELSE
               MOVE ZERO             TO SEC-COUNT.
             MOVE SEC-COUNT          TO WS-DAYS.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё              C H E C K   F O R   E X P I R Y              Ё
      *    Ё      Systems installed with anual service contracts       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
           IF SEC-COUNT > 65
               COMPUTE WS-REG = 15 - (WS-DAYS - 65)
               DISPLAY S-LEGAL.

       AD07X.
             MOVE 3                  TO WS-PARKEY.
             READ PARAM.
           IF RUNTIME-ERROR
               IF RECORD-LOCKED
                   GO TO AD07X.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  A D D I T I O N A L   S E C U R I T Y   I N C L U D E D  Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё          New Systems - already correct                Ё
      *    Ё     (This ROUTINE updating older versions)           Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
           IF NOT(PAR-VER = "2013")
               MOVE "2013"           TO PAR-VER
               REWRITE PAR-RECORD3.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё      S E R I A L   N U M B E R   A N D   S Y S T E M      Ё
      *    Ё           F R O M   P A R A M E T E R   F I L E           Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
             MOVE PAR-LEV            TO WS-LEV.
             MOVE PAR-SERIAL         TO WS-SERIAL.
      *    *************************************************************
      *      Following code changed to remarks - No longer required
      *
      *      MOVE PAR-CUR-YR         TO WS-CUR-YR.
      *      MOVE PAR-CUR-CEN        TO WS-CUR-CEN.
      *      MOVE PAR-PRV-CEN        TO WS-PRV-CEN.
      *      MOVE PAR-NXT-CEN        TO WS-NXT-CEN.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё S U P E R V I S O R   P R I N T E R   D E T A I L S    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
             MOVE 110                TO WS-USUB.
             ADD 2549 WS-USUB        GIVING W25-RESULT.
             MOVE W25-KEY            TO WS-PARKEY.
             ADD 1 W25-SUB           GIVING WS-S6.
             READ PARAM.
           IF RUNTIME-ERROR
               IF RECORD-LOCKED
                   GO TO AD07X.
             ADD 501 TRM-PRN4(WS-S6) GIVING WS-SUB-6.
             EVALUATE TRM-STDP(WS-S6)
               WHEN 1            MOVE "LPT1"     TO W02-PRINTER
               
               WHEN 2            MOVE "LPT2"     TO W02-PRINTER
               
               WHEN 3            MOVE "LPT3"     TO W02-PRINTER
               
               WHEN 4            MOVE "COM1"     TO W02-PRINTER
               
               WHEN 5            MOVE "COM2"     TO W02-PRINTER
               
               WHEN 9            MOVE LS-USER    TO W02-USER
             END-EVALUATE.
             ADD 501 TRM-PRN3(WS-S6) GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2  GIVING WS-PARKEY REMAINDER WS-S6.
             ADD 1                   TO WS-S6.
             READ PARAM.
           IF RUNTIME-ERROR
               IF RECORD-LOCKED
                   GO TO AD07X.
             MOVE TRM-LENGTH(WS-S6)  TO WS-PRN-LENGTH.
             MOVE TRM-PAGE(WS-S6)    TO WS-PGE-LENGTH.

       AD08.
             MOVE 1                  TO WS-SECKEY.
             READ SECUR.
           IF RUNTIME-ERROR
               IF RECORD-LOCKED
                   GO TO AD08.
           IF WS-STATUS NOT = "00"
               MOVE "Disk error - AD08"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               STOP RUN.
             MOVE 1                  TO WS-S1.

       AD10.
             MOVE SEC-ASCII(WS-S1)   TO W15-ASCII (WS-S1).
           IF WS-S1 < 256
               ADD 1                 TO WS-S1
               GO TO AD10.
             MOVE 5                  TO WS-PARKEY.

       AD12.
             READ PARAM.
           IF RUNTIME-ERROR
               IF RECORD-LOCKED
                   GO TO AD12.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё D E F A U L T   S E T T I N G S   F O R   B A R C O D E   Ё
      *    Ё P R I N T I N G.                                          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
           IF NOT(PAR-BC = "Y")
               MOVE "Y"              TO PAR-BC
               MOVE 80               TO PAR-BC-HGHT
               MOVE 4                TO PAR-COLUMN
               MOVE 45               TO PAR-ROW
               MOVE 2                TO PAR-NAR
               MOVE 2                TO PAR-WIDE
               MOVE 2                TO PAR-FONT
               MOVE 160              TO PAR-F-ROW
               MOVE ZERO             TO PAR-F-COLUMN
               MOVE 125              TO PAR-F-ROW2
               MOVE ZERO             TO PAR-F-COL2
               MOVE "Y"              TO PAR-SPLIT
               REWRITE PAR-RECORD5.
             GO TO AD99.
       AD15.
             MOVE W12-T-YMD          TO WS-DTE.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  N U M B E R   O F   D A Y S   F R O M   S T A R T   O F  Ё
      *    Ё  P R E V I O U S   C E N T U R Y   -   D A T E   1        Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
             COMPUTE W31-DTE1 = (W12-CY - (WS-PRV-CEN*100)) * 365.25.
             ADD W31-NO(W12-MM)      TO W31-DTE1.
             ADD W12-DD              TO W31-DTE1.
             MOVE WS-DTE             TO W12-T-YMD.
           IF W12-T-YMD < SEC-INITIAL
               MOVE W12-T-YMD        TO SEC-INITIAL.
             MOVE SEC-INITIAL        TO W20-DTE.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  N U M B E R   O F   D A Y S   F R O M   S T A R T   O F  Ё
      *    Ё  P R E V I O U S   C E N T U R Y   -   D A T E   2        Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
             COMPUTE W31-DTE2 = (W20-CNYR - (WS-PRV-CEN*100)) * 365.25.
             ADD W31-NO(W20-MONTH)   TO W31-DTE2.
             ADD W20-DAY             TO W31-DTE2.
           IF (W31-DTE1 - W31-DTE2) < 75
               SUBTRACT W31-DTE2 FROM W31-DTE1 GIVING SEC-COUNT
           ELSE
               MOVE 75               TO SEC-COUNT.
             REWRITE SEC-REC2.

       AD99.
             EXIT.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё   N E T W O R K   D R I V E   N O T   A V A I L A B L E   Ё
      *    Ё   M U S T   S Y S T E M   R U N   O F F - L I N E         Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       AD100        SECTION 5.
       AD105.
             PERFORM SAVE-SCREEN-3.
             DISPLAY "зддддддддддддддддддддддддддддддддддддддд" AT 1120 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Black            
                     "©"                                                WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Magenta HIGHLIGHT.
             DISPLAY "Ё"                                        AT 1220 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Black            
                     "Network files not available at present!"          WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan    HIGHLIGHT
                     "Ё"                                                WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Magenta HIGHLIGHT.
             DISPLAY "Ё"                                        AT 1320 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Black            
                     "Work off-line (Y/N)                    "          WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan    HIGHLIGHT
                     "Ё"                                                WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Magenta HIGHLIGHT.
             DISPLAY "ю"                                        AT 1420 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Black            
                     "ддддддддддддддддддддддддддддддддддддддды"         WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Magenta HIGHLIGHT.
             MOVE 11                 TO SHADE-ROW.
             MOVE 20                 TO SHADE-COL.
             MOVE 41                 TO SHADE-WIDTH.
             MOVE 4                  TO SHADE-LINES.
             PERFORM SCREEN-SHADOW.
             MOVE "N"                TO WS-OPTION.

       AD110.
             ACCEPT WS-OPTION AT 1341 WITH HIGHLIGHT BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey UPDATE AUTO.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-OPTION = "N" OR "Y")
               GO TO AD110.
             PERFORM RESTORE-SCREEN-3.

       AD199.
             EXIT.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё           F I L E S   N O T   A V A I L A B L E           Ё
      *    Ё  Check if this is a Network and if off-line files exist.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *AD200        SECTION 5.
      *AD205.
      *      MOVE ZERO               TO WS-ERROR.
      *      MOVE "OFFLINE\"         TO W02-N-OR-L.
      *      MOVE WS-SYS-ID          TO W02-SYSPTH.
      *      CALL "CBL_CHECK_FILE_EXIST" USING W02-APACFILE FILE-DETAILS RETURNING WS-STATUS.
      *    IF WS-STAT1H = 1
      *        MOVE 9                TO WS-ERROR.
      *      MOVE "SYSTEMS\"         TO W02-N-OR-L.

       AD999.
             EXIT.
      *    *************************************************************
      *    *****              S T A R T   S Y S T E M              *****
      *    *************************************************************
       AI000                   SECTION 5.
       AI0.
             MOVE 2                  TO WS-SECKEY.

       AI0-READ.
             READ SECUR.
           IF RUNTIME-ERROR
               IF RECORD-LOCKED
                   GO TO AI0-READ.
           IF WS-STATUS NOT = "00"
               MOVE "Disk error - AI00 (SYSTEM)" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               STOP RUN.

       AI00.
             MOVE 1                  TO WS-ERROR.
           IF (WS0-DTP NOT = 2) AND (WS0-STP NOT = 2) AND
              (WS0-CRP NOT = 2) AND (WS0-GLP NOT = 2) AND
              (WS0-HPD NOT = 2) AND (WS0-POS NOT = 2) AND
              (WS0-JCP NOT = 2) AND (WS0-VHP NOT = 2)
               MOVE "Files not yet initialized"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO AI999.
           IF SEC-AGREE = "R" AND SEC-DAYS = ZERO
               MOVE "Contract must be renewed!"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO AI999.
           IF WS0-POS = 2
               MOVE "Y"              TO WS-CSS.

       AI02.
             MOVE 3                  TO WS-PARKEY.
             READ PARAM.
           IF RUNTIME-ERROR
               IF RECORD-LOCKED
                   GO TO AI02.
      *      MOVE PAR-CUR-YR         TO WS-CUR-YR.
      *      MOVE PAR-CUR-CEN        TO WS-CUR-CEN.
      *      MOVE PAR-PRV-CEN        TO WS-PRV-CEN.
      *      MOVE PAR-NXT-CEN        TO WS-NXT-CEN.
             MOVE 1                  TO WS-PARKEY.
      
       AI05.
             READ PARAM.
           IF RUNTIME-ERROR
               IF RECORD-LOCKED
                   GO TO AI05.
           IF WS-STATUS NOT = "00"
               MOVE "Disk error - AI00 (PARAM)"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               STOP RUN.

       AI10.
           IF (PAR-COMPANY NOT = SEC-COMP) OR
              (WS-SERIAL NOT = SEC-NUM) OR
              (WS-LEV NOT = SEC-LEV)
               MOVE "Contact your supplier"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               STOP RUN.
      *    *************************************************************
      *    ****   U S E D   T O   C H E C K   F O R   C L E A R
      *           O N   D E M O N S T R A T I O N   S Y S T E M
      *    *************************************************************
             MOVE SEC-DATE           TO WS-DTE.

       AI25.
             MOVE ZERO               TO WS-ERROR.
             PERFORM ERASE-SCREEN.
             MOVE 6                  TO SHADE-ROW.
             MOVE 16                 TO SHADE-COL.
             MOVE 49                 TO SHADE-WIDTH.
             MOVE 14                 TO SHADE-LINES.
             DISPLAY S18.
             PERFORM SCREEN-SHADOW.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё This routine has now been upgraded to the latest COBOL    Ё
      *    Ё syntax regarding date.                                    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
             ACCEPT W12-T-YMD FROM DATE YYYYMMDD.
             MOVE W12-CC             TO W12-CENT.
             MOVE W12-YY             TO W12-YEAR.
             MOVE W12-MM             TO W22-MONTH W12-MONTH.
             MOVE W12-DD             TO W22-DAY   W12-DAY.
             MOVE W12-CY             TO W22-CCYY.  

       AI30.
             MOVE 2254               TO DATEPOS.
             MOVE "A"                TO W22-INSTR.
             CALL "UTP\DateCheck" USING W-DATES.
      *
      *    If the Escape key is pressed, the program exits with the 
      *    value X"EC" in W22-INSTR.
      *
      *                            OR
      *
      *    On exiting, the valid date will be in W20-DATE [9(08)] and
      *    W22-DATE-YMD [9999/99/99] in CCYYMMDD format, while W21-DATE
      *    [9(08)] and W22-DATE-DMY [99/99/9999] will have the date in
      *    DDMMCCYY format.
      *
           IF (W22-INSTR = X"EC")
               GO TO AI999.
             MOVE W22-DTE            TO W12-TODAY TODAY-DDMMYY SEC-DATE.
             MOVE W20-DTE            TO W12-T-YMD TODAY-YYMMDD.
             MOVE 23                 TO CRT-START.
             MOVE 25                 TO CRT-END.
             PERFORM ERASE-SCREEN.
             DISPLAY S20.
             DISPLAY "Is date correct (Y/N) ?" AT 2629 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             MOVE SPACE              TO WS-OPTION.
             PERFORM TEST AFTER UNTIL WS-OPTION = "Y" OR "N"
                 ACCEPT WS-OPTION AT 2653 WITH FOREGROUND-COLOR Grey HIGHLIGHT UPDATE AUTO
                 CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS
             END-PERFORM.
           IF WS-OPTION = "N"
               GO TO AI30.
             MOVE W12-TODAY          TO PAR-DMY.
             MOVE W12-T-YMD          TO PAR-YMD.
             REWRITE PAR-RECORD1.
           IF SEC-AGREE = "R"
               IF NOT(W12-T-YMD < SEC-EXPDATE)
                   MOVE ZERO           TO SEC-DAYS
               ELSE
                   COMPUTE W31-DTE1 = W12-YY * 365.25
                   ADD W31-NO(W12-MM)  TO W31-DTE1
                   ADD W12-DD          TO W31-DTE1
                   MOVE SEC-EXPDATE    TO W20-DTE
                   COMPUTE W31-DTE2 = W20-YEAR * 365.25
                   ADD W31-NO(W20-MONTH) TO W31-DTE2
                   ADD W20-DAY          TO W31-DTE2
                   SUBTRACT W31-DTE2 FROM W31-DTE1 GIVING W25-DAYS
                   IF W25-DAYS < 46
                       MOVE W25-DAYS   TO SEC-DAYS.
             MOVE SEC-DEM            TO WS-DEM.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё T E S T   F O R   D E M O N S T R A T I O N   S Y S T E M Ё
      *    Ё             Is the run date still the same ?              Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
           IF SEC-DATE = WS-DTE
      *    *************************************************************
      *    ****   M U S T   I N D I C A T O R   B E   R E S E T   ?
      *    *************************************************************
               IF WS-DEM > 2
                   MOVE 2            TO WS-DEM SEC-DEM
               END-IF
           ELSE
      *    *************************************************************
      *    ****   M U S T   I N D I C A T O R   B E   S E T    ?
      *    *************************************************************
           IF WS-DEM = 2
               MOVE ZERO             TO W31-DTE1
               ADD W31-NO(W12-MM)    TO W31-DTE1
               ADD W12-DD            TO W31-DTE1
               DIVIDE W31-DTE1 BY 13 GIVING W31-DTE2 REMAINDER WS-C1
               IF WS-C1 = 3 OR 6 OR 9 OR 12
                   DIVIDE 3          INTO WS-C1
                   ADD 2 WS-C1       GIVING WS-DEM
                   MOVE WS-DEM       TO SEC-DEM.
             REWRITE SEC-REC2.
      *******************************************************************
      *
      *    **** The following code is no longer required and was used
      *         for the Y2K switch over. Now changed to remarks
      *
      *   IF W12-YY = WS-CUR-YR
      *       GO TO AI999.

       AI40.
      *     MOVE 3   TO WS-PARKEY.
      *      READ PARAM.
      *     IF RUNTIME-ERROR
      *         IF RECORD-LOCKED
      *     GO TO AI40.
      *     MOVE W12-YY  TO PAR-CUR-YR.
      *   IF W12-YY < (WS-CUR-YR - 70)
      *       ADD 1   TO PAR-CUR-CEN
      *        PAR-PRV-CEN
      *        PAR-NXT-CEN
      *   ELSE
      *   IF W12-YY > (WS-CUR-YR + 70)
      *       SUBTRACT 1  FROM PAR-CUR-CEN
      *          PAR-PRV-CEN
      *          PAR-NXT-CEN.
      *      REWRITE PAR-RECORD3.
      ******************************************************************
       AI999.
             EXIT.
      *    *************************************************************
      *    *****            A P A C   MARKETING   SCREEN            ****
      *    *************************************************************
       AL000      SECTION 5.
       AL00.
             ACCEPT WS-START FROM TIME.
             MOVE 223                TO W40-DEC.
           IF NOT(LS-INSTR = "X")
               DISPLAY APAC
               PERFORM HIDE-THE-CURSOR
               DISPLAY " " AT 5001.
             MOVE ZERO               TO WS-S1 WS-S2.
             MOVE L05-COMMAND        TO W05-COMMAND.
             MOVE 1                  TO WS-S1 WS-S2.
             MOVE ZERO               TO W05-DRAW.

      *
      *    **** Check if data entered on the command line
      *
       AL10.
           IF W05-CHAR(WS-S1) = "*"
               GO TO AL15.
           IF WS-S1 < 117
               ADD 1                 TO WS-S1
               GO TO AL10.
             GO TO AL30.
      *
      *    **** Parse command line data
      *
       AL15.
             ADD 2                   TO WS-S1.

       AL20.
           IF W05-CHAR (WS-S1) = "," OR " "
               GO TO AL25.
             MOVE W05-CHAR(WS-S1)    TO W05-DC(WS-S2).
           IF WS-S2 < 2
               ADD 1                 TO WS-S2
           ELSE
               GO TO AL30.

       AL25.
           IF WS-S1 < 128
               ADD 1                 TO WS-S1
               GO TO AL20.
      *
      *    **** Check if valid Drawer no?
      *
       AL30.
           IF W05-DRW NOT NUMERIC
               MOVE 0                TO W05-DRAW
           ELSE
           IF W05-DRAW > 39
               MOVE 40               TO W05-DRAW.
             MOVE ZERO               TO WS-S1.

       AL35.
             CALL X"D9" USING WS-CONSOLE.
           IF NOT(WS-CONSOLE = ZERO)
               ACCEPT WS-OPTION WITH AUTO
               GO TO AL40.
             ADD 1                   TO WS-S1 WS-S2.
             ACCEPT WS-END FROM TIME.
             SUBTRACT WS-START       FROM WS-END.
           IF LS-INSTR = "X"
               GO TO AL999.
           IF WS-END < 400
               GO TO AL35.

       AL40.
             PERFORM HEADING-AND-CLEAR-SCREEN.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё D O   N O T   D I S P L A Y   E R R O R   M E S S A G E S Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
             MOVE 44                 TO USER-NUMBER.
             MOVE 1                  TO USER-SETTING.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Setup various keys and the mouse for use with menus and  Ё
      *    Ё  other required user interface functions and procedures.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

             COPY "FUNCTION.PRO".

       AL999.
           EXIT.
      *    *************************************************************
      *    *****              CLEAR  THE  USER  LOG                *****
      *    *************************************************************
       AM000                   SECTION 5.
       AM00.
             PERFORM ERASE-SCREEN.
           IF WS-INIT NOT = 2
               GO TO AM999.
             DISPLAY "Clear the Work Station Log"      AT 0729 WITH FOREGROUND-COLOR Grey HIGHLIGHT .
             MOVE SPACES             TO W40-CSHPASS.
             MOVE 16                 TO SHADE-ROW.
             MOVE 8                  TO SHADE-COL.
             MOVE 60                 TO SHADE-WIDTH.
             MOVE 4                  TO SHADE-LINES.
             DISPLAY S06.
             PERFORM SCREEN-SHADOW.
             MOVE "Do you wish to continue Y/N [ ]" TO WS-OPT-MES.
             MOVE 1                  TO LS-INSTR.
             PERFORM OPT-MESSAGE.
          IF WS-OPTION = "N"
               GO TO AM999.

       AM05.
             ACCEPT S07.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W40-CSHPASS BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             MOVE 5                  TO WS-PARKEY.

       AM06.
             READ PARAM.
           IF RUNTIME-ERROR
               IF RECORD-LOCKED
                   GO TO AM06.
           IF WS-STATUS NOT = "00"
               MOVE "Disk error - AM05 (PARAM)" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               STOP RUN.
             MOVE 1                  TO WS-S1.

       AM07.
             MOVE PAR-SUPER(WS-S1)   TO W85-SUPER(WS-S1).
           IF WS-S1 < 9
               ADD 1                 TO WS-S1
               GO TO AM07.
           IF NOT(W40-CSHPASS = W85-SUPER(1) OR W85-SUPER(2) OR W85-SUPER(3) OR W85-SUPER(4) OR W85-SUPER(5) OR W85-SUPER(6) OR W85-SUPER(7) OR W85-SUPER(8) OR W85-SUPER(9) OR "!@#$%^")
               MOVE "Invalid password" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO AM999.
             MOVE 4                  TO WS-PARKEY.

       AM08.
             READ PARAM LOCK.
           IF RUNTIME-ERROR
               IF RECORD-LOCKED
                   GO TO AM08.
             MOVE 1                  TO WS-S1.
             MOVE 0                  TO WS-S2.
           IF PAR-USERS = 0
               GO TO AM20.
             DISPLAY "Recoveries required for W/St" AT 1212.
             MOVE 1241               TO CPOS.

       AM10.
           IF PAR-USR(WS-S1) = WS-USER
               ADD 1                 TO PAR-USERS
               GO TO AM15.
           IF PAR-USR(WS-S1) = SPACES
               GO TO AM15.
           IF PAR-PROG(WS-S1) NOT = SPACES
               ADD 1                 TO PAR-USERS
               DISPLAY PAR-USR(WS-S1) AT CPOS WITH FOREGROUND-COLOR Brown HIGHLIGHT
               ADD 4                 TO CCOL
           ELSE
               MOVE SPACES           TO PAR-USR(WS-S1).

       AM15.
           IF WS-S1 < 24
              ADD 1   TO WS-S1
               GO TO AM10.

       AM18.
             MOVE "Recoveries must be done before continuing" TO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.
             MOVE SPACES             TO PAR-USR (WS-USUB).
             SUBTRACT 1              FROM PAR-USERS.
             MOVE ZERO               TO PAR-LOGON.
             GO TO AM25.

       AM20.
             MOVE "Work Station Log cleared" TO WS-DSP-MES.
             PERFORM DISPLAY-MESSAGE.

       AM25.
             REWRITE PAR-RECORD4.
           IF PAR-LOGON = ZERO
               STOP RUN.

       AM999.
             EXIT.
      *    *************************************************************
      *    *****             AMEND NUMBER OF W/STATIONS            *****
      *    *************************************************************
       AN000         SECTION 6.
       AN00.
             MOVE "AMEND NO. OF WORK STATIONS" TO CRT-HEADING.
             PERFORM HEADING-AND-CLEAR-SCREEN.
           IF (WS0-DTP NOT = 2) AND (WS0-STP NOT = 2) AND (WS0-CRP NOT = 2) AND (WS0-GLP NOT = 2) AND (WS0-HPD NOT = 2) AND (WS0-POS NOT = 2) AND (WS0-JCP NOT = 2) AND (WS0-VHP NOT = 2)
               MOVE "Files not yet initialized"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO AN999.
             MOVE 2                  TO WS-SECKEY.

       AN02.
             READ SECUR LOCK.
           IF RUNTIME-ERROR
               IF RECORD-LOCKED
               GO TO AN02.
           IF WS-STATUS NOT = "00"
               MOVE "Disk error - AN00"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               STOP RUN.
           IF SEC-DEM > ZERO
               MOVE "System is not registered" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO AN999.
           IF WS0-LEV < 2
               MOVE 2                TO WS-S2
           ELSE
           IF WS0-LEV < 3
               MOVE 4                TO WS-S2
           ELSE
           IF WS0-LEV < 4
               MOVE 10               TO WS-S2
           ELSE
               MOVE 100              TO WS-S2.
           IF NOT(SEC-TERMS < WS-S2)
               MOVE "Maximum W/Stations already installed" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO AN999.
             MOVE "N"                TO WS-OPTION.
             MOVE "Are you sure (Y/N)? [ ]"    TO WS-OPT-MES.
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "N"
               GO TO AN999.
             DISPLAY "Key in date (DDMMCCYY)" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
       AN05.
             MOVE 0835               TO DATEPOS.
             MOVE "A"                TO W22-INSTR.
             CALL "UTP\DateCheck" USING W-DATES.
      *
      *    If the Escape key is pressed, the program exits with the 
      *    value X"EC" in W22-INSTR.
      *
      *                            OR
      *
      *    On exiting, the valid date will be in W20-DATE [9(08)] and
      *    W22-DATE-YMD [9999/99/99] in CCYYMMDD format, while W21-DATE
      *    [9(08)] and W22-DATE-DMY [99/99/9999] will have the date in
      *    DDMMCCYY format.
      *
           IF (W22-INSTR = X"EC")
               GO TO AN05.
             PERFORM SAVE-SCREEN-3.
             MOVE 11                 TO SHADE-ROW.
             MOVE 24                 TO SHADE-COL.
             MOVE 31                 TO SHADE-WIDTH.
             MOVE 3                  TO SHADE-LINES.
             DISPLAY "зддддддддддддддддддддддддддддд©" AT 1124 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
             DISPLAY "Ё Number of Work Stations     Ё" AT 1224 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
             DISPLAY "юддддддддддддддддддддддддддддды" AT 1324 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
             PERFORM SCREEN-SHADOW.
             MOVE SEC-TERMS          TO WS-TERMS.

       AN08.
             ACCEPT S30.
           IF WS-TERMS > WS-S2
               MOVE WS-S2            TO WS-TERMS.
             EVALUATE WS0-LEV
               WHEN 0      MOVE "JUNIOR"   TO WS-PACK

               WHEN 1      MOVE "ENTRY"     TO WS-PACK

               WHEN 2      MOVE "BASIC"     TO WS-PACK

               WHEN 3      MOVE "STANDARD"  TO WS-PACK

               WHEN 4      MOVE "PREMIUM"   TO WS-PACK

               WHEN 5      MOVE "G O L D"   TO WS-PACK

               WHEN OTHER  MOVE "S.MANAG."  TO WS-PACK
             END-EVALUATE.
             PERFORM RESTORE-SCREEN-3.
             DISPLAY S25.
             DIVIDE SEC-DATE BY 2 GIVING W25-RESULT.
             ADD 13                  TO W25-RESULT.
             MULTIPLY 2.136          BY W25-RESULT.
             ADD SEC-PASS            TO W25-RESULT.
             ACCEPT W25-TIME         FROM TIME.
             ADD W25-TIME            TO W25-RESULT.
             DISPLAY W25-RESULT AT 1204.
             DIVIDE 1.97             INTO W25-RESULT.
             MOVE W25-WHOLE          TO W25-PASS.
             MOVE W25-PASS1          TO W35-PASS2.
             MOVE W25-PASS2          TO W35-PASS1.
             MOVE W35-PNUM           TO W25-RESULT.
           IF W25-RESULT < 100000
               ADD 701010            TO W25-RESULT
           ELSE
               ADD 77                TO W25-RESULT.
             ADD 10112               TO W25-RESULT.
             ADD WS-TERMS            TO W25-RESULT.
             ADD WS0-LEV             TO W25-RESULT.
             MOVE W25-WHOLE          TO SEC-PASS.
             DISPLAY "Password" AT 1404.
             DISPLAY S25.
             MOVE ZERO               TO W25-WHOLE.
             ACCEPT W25-WHOLE AT 1413 WITH FOREGROUND-COLOR Brown HIGHLIGHT NO-ECHO AUTO.
             MOVE W12-T-YMD          TO SEC-INITIAL.
      *    *************************************************************
      *    ****     O V E R R I D E   P A S S W O R D
      *            F O R   A D D   F O U R   U S E R S
      *                Y O U R  E Y E S   O N L Y
      *           ----- NOT TO BE GIVEN TO ANYONE -----
      *    *************************************************************
           IF W25-WHOLE NOT = SEC-PASS
               IF W25-WHOLE NOT = 112233
                    GO TO AN15.

       AN10.
             MOVE 3                  TO WS-PARKEY.
             READ PARAM.
           IF RUNTIME-ERROR
               IF RECORD-LOCKED
                   GO TO AN10.
             MOVE SEC-NUM            TO PAR-SERIAL WS-SERIAL.
             MOVE SEC-LEV            TO PAR-LEV    WS-LEV.
             REWRITE PAR-RECORD3.
             MOVE WS-TERMS           TO SEC-TERMS.
             REWRITE SEC-REC2.
             UNLOCK SECUR.
           IF WS-STATUS NOT = "00"
               MOVE "Disk error - AN10"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               STOP RUN.
             GO TO AN999.

       AN15.
             MOVE "Invalid password" TO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.

       AN999.
             EXIT.
      *    *************************************************************
      *    *****              R E N E W   C O N T R A C T          *****
      *    *************************************************************
       AO000         SECTION 6.
       AO00.
             MOVE "RENEW CONTRACT"   TO CRT-HEADING.
             PERFORM HEADING-AND-CLEAR-SCREEN.
             MOVE 2                  TO WS-SECKEY.

       AO02.
             READ SECUR LOCK.
           IF RUNTIME-ERROR
               IF RECORD-LOCKED
                   GO TO AO02.
           IF WS-STATUS NOT = "00"
               MOVE "Disk error - AO00"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               STOP RUN.
             MOVE "N"                TO WS-OPTION.
             MOVE "Are you sure (Y/N)? [ ]" TO WS-OPT-MES.
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "N"
              GO TO AO999.
             DISPLAY "Key in date (DDMMCCYY)" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
       AO05.
             MOVE 0835               TO DATEPOS.
             CALL "UTP\DateCheck" USING W-DATES.
      *
      *    If the Escape key is pressed, the program exits with the 
      *    value X"EC" in W22-INSTR.
      *
      *                            OR
      *
      *    On exiting, the valid date will be in W20-DATE [9(08)] and
      *    W22-DATE-YMD [9999/99/99] in CCYYMMDD format, while W21-DATE
      *    [9(08)] and W22-DATE-DMY [99/99/9999] will have the date in
      *    DDMMCCYY format.
      *
           IF (W22-INSTR = X"EC")
               GO TO AO05.
             EVALUATE WS0-LEV
               WHEN 0      MOVE "JUNIOR"   TO WS-PACK

               WHEN 1      MOVE "ENTRY"     TO WS-PACK

               WHEN 2      MOVE "BASIC"     TO WS-PACK

               WHEN 3      MOVE "STANDARD"  TO WS-PACK

               WHEN 4      MOVE "PREMIUM"   TO WS-PACK

               WHEN 5      MOVE "G O L D"   TO WS-PACK

               WHEN OTHER  MOVE "S.MANAG."  TO WS-PACK
             END-EVALUATE.
             DISPLAY S25.
             DIVIDE SEC-DATE BY 2    GIVING W25-RESULT.
             ADD 13                  TO W25-RESULT.
             MULTIPLY 2.136          BY W25-RESULT.
             ADD SEC-PASS            TO W25-RESULT.
             ACCEPT W25-TIME         FROM TIME.
             ADD W25-TIME            TO W25-RESULT.
             DISPLAY W25-RESULT AT 1204.
             DIVIDE 1.97             INTO W25-RESULT.
             MOVE W25-WHOLE          TO W25-PASS.
             MOVE W25-PASS1          TO W35-PASS2.
             MOVE W25-PASS2          TO W35-PASS1.
             MOVE W35-PNUM           TO W25-RESULT.
           IF W25-RESULT < 100000
               ADD 701010            TO W25-RESULT
           ELSE
               ADD 77                TO W25-RESULT.
             ADD 10112               TO W25-RESULT.
             ADD WS-TERMS            TO W25-RESULT.
             ADD WS0-LEV             TO W25-RESULT.
             MOVE W25-WHOLE          TO SEC-PASS.
             DISPLAY "Password" AT 1404.
             DISPLAY S25.
             MOVE ZERO               TO W25-WHOLE.
             ACCEPT W25-WHOLE AT 1413 WITH FOREGROUND-COLOR Brown HIGHLIGHT NO-ECHO AUTO.
      *    *************************************************************
      *    ****     O V E R R I D E   P A S S W O R D
      *            F O R   R E N E W   C O N T R A C T
      *                Y O U R  E Y E S   O N L Y
      *           ----- NOT TO BE GIVEN TO ANYONE -----
      *    *************************************************************
           IF W25-WHOLE NOT = SEC-PASS
                IF W25-WHOLE NOT = 112233
                     GO TO AO15.

       AO10.
             MOVE SEC-ANDATE         TO W20-DTE.
             ADD 1                   TO W20-YEAR.
             MOVE W20-DTE            TO SEC-ANDATE.
             MOVE SEC-EXPDATE        TO W20-DTE.
             ADD 1                   TO W20-YEAR.
             MOVE W20-DTE            TO SEC-EXPDATE.
             MOVE 255                TO SEC-DAYS
             REWRITE SEC-REC2.
             UNLOCK SECUR.
           IF WS-STATUS NOT = "00"
               MOVE "Disk error - AO10"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               STOP RUN.
             GO TO AO999.

       AO15.
             MOVE "Invalid password" TO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.

       AO999.
             EXIT.

       COMPANY-LOOKUP  SECTION 7.
       COMPANY-LUP-SCREEN.
             PERFORM SAVE-SCREEN.
             DISPLAY S-COM.

       COMPANY-LUP-CLR-SCREEN.
             MOVE 10                 TO SHADE-ROW.
             MOVE 29                 TO SHADE-COL.
             MOVE 47                 TO SHADE-WIDTH.
             MOVE 22                 TO SHADE-LINES.
             PERFORM SCREEN-SHADOW.
             PERFORM CLEAR-L50.
             MOVE 13                 TO CLIN.
             MOVE 1                  TO W75-S W75-S1.

       COMPANY-LUP-TAB-LOOP.
             MOVE SPACES  TO W75-COM(W75-S1).
           IF W75-S1 < 18
               ADD 1                 TO W75-S1
               GO TO COMPANY-LUP-TAB-LOOP.
             MOVE ZERO               TO W75-S1.
             MOVE "  !"              TO COM-CODE.
             START APCCOM KEY >= COM-CODE.
           IF NOT(WS-STATUS = "00")
               GO TO COMPANY-NO-RECORD.

       COMPANY-GET-COM-RECORD.
           IF WS-OPTION = "J"
               MOVE SPACE  TO WS-OPTION
               GO TO COMPANY-SKIP-COM-READ.
             READ APCCOM NEXT  WITH IGNORE LOCK.
           IF NOT(WS-STATUS = "00")
               SUBTRACT 1  FROM W75-S
               GO TO COMPANY-FULL-SCREEN.

       COMPANY-SKIP-COM-READ.
             MOVE COM-CODE  TO W75-COM (W75-S).
             DISPLAY SD-COM AT LINE CLIN.
           IF CLIN < 30
               ADD 1 TO CLIN W75-S
               GO TO COMPANY-GET-COM-RECORD.

       COMPANY-FULL-SCREEN.
             DISPLAY " "                     AT 5003 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                        " highlight - "                    WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue           
                                     "PgUp"                WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                         "/"               WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue           
                                          "PgDn"           WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                              " - "        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue           
                                                 "Esc"     WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                                    "ape"  WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue           .

       COMPANY-SAVE-FULL-SCREEN.
             PERFORM SAVE-SCREEN-2.
             MOVE 0                  TO W75-S1.
             MOVE 1330               TO CPOS.

       COMPANY-CRT-ATTRIB.
             MOVE 3920               TO W41-LENGTH.
             CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS W42-ATTRIB2 W41-LENGTH RETURNING WS-STATUS.
             MOVE 4000   TO W41-LENGTH.

       COMPANY-GET-USER-OPT.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             PERFORM CLEAR-L50.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     GO TO COMPANY-ADJ-NO-KEYS
                 
                 WHEN DOWN-KEY   GO TO COMPANY-CHK-NO-KEYS
                 
                 WHEN ENTER-KEY
                 
                 WHEN CR-KEY     GO TO COMPANY-RESTORE-SCREEN
                 
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO COMPANY-GET-USER-OPT
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    GO TO COMPANY-RESTORE-SCREEN
                 
                 WHEN PAGE-DOWN  GO TO COMPANY-GET-NEXT-PAGE
                 
                 WHEN PAGE-UP    GO TO COMPANY-GET-PREV-PAGE
                 
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO COMPANY-GET-USER-OPT.
             PERFORM AA900-ALARM.
             GO TO COMPANY-GET-USER-OPT.

       COMPANY-GET-NEXT-PAGE.
           IF W75-S < 12
               GO TO COMPANY-GET-USER-OPT.

       COMPANY-NEW-PAGE.
             DISPLAY S-COM.
             MOVE 13                 TO CLIN.
             MOVE LOW-VALUES         TO W75-KEYS.
             MOVE 1                  TO W75-S W75-S1.
             PERFORM COMPANY-LUP-TAB-LOOP.
             GO TO COMPANY-GET-COM-RECORD.

       COMPANY-GET-PREV-PAGE.
             MOVE 1                  TO WS-IXS1.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё             SET OPTION TO BYPASS THE READ NEXT            Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
             MOVE "J"                TO WS-OPTION.

       COMPANY-GET-PREV-LOOP.
             READ APCCOM PREVIOUS WITH IGNORE LOCK.
           IF NOT(WS-STATUS = "00")
               MOVE SPACE            TO WS-OPTION
               GO TO COMPANY-NEW-PAGE.
           IF WS-IXS1 < 24
               ADD 1                 TO WS-IXS1
               GO TO COMPANY-GET-PREV-LOOP.
             GO TO COMPANY-NEW-PAGE.

       COMPANY-NO-RECORD.
             MOVE "No company codes" TO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.
             GO TO COMPANY-RESTORE-SCREEN.

       COMPANY-ADJ-NO-KEYS.
           IF W75-S1 > 1
               SUBTRACT 1            FROM W75-S1 CLIN.
             GO TO COMPANY-HIGHLIGHT.

       COMPANY-CHK-NO-KEYS.
           IF W75-S1 = ZERO
               MOVE 1                TO W75-S1
               GO TO COMPANY-HIGHLIGHT.
           IF W75-S1 < W75-S
               ADD 1                 TO W75-S1 CLIN.

       COMPANY-HIGHLIGHT.
             PERFORM COMPANY-CRT-ATTRIB.
             DISPLAY W43-DET3(CLIN) AT CPOS WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey.
             GO TO COMPANY-GET-USER-OPT.

       COMPANY-RESTORE-SCREEN.
           IF NOT(W75-S1 = ZERO)
               MOVE W75-COM(W75-S1)  TO WS-SYS-ID.
             PERFORM RESTORE-SCREEN.

       COMPANY-LOOKUP-EXIT.
             EXIT.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Set the DEFAULT file PATHS as if  they are present on the Ё
      *    Ё same hard drive as  the  programs.  This will allow for a Ё
      *    Ё single USER system  or  for  a  network system, where the Ё
      *    Ё programs are run from  the Network drive.  Where programs Ё
      *    Ё are run from a local  drive  on  network  use the program Ё
      *    Ё FILEUTIL to change the paths.                             Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       APACFILES       SECTION 7.
       APAC-FILES-INIT.
             MOVE WS-SYS-ID          TO W02-SYSPTH W02-CHK-ID LS-SYS-ID.
             MOVE ZERO               TO WS-ERROR.
             OPEN INPUT APACFILE.
           IF NOT(WS-STATUS = "00")
               MOVE 9                TO WS-ERROR
               GO TO APACFILES-EXIT.
             CLOSE APACFILE.
             CALL "UTP\FILEIDS" USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS.
             CANCEL "UTP\FILEIDS".
             MOVE W02-PARAM          TO L02-PARAM.
             MOVE W02-SYSTEM         TO L02-SYSTEM.

       APACFILES-EXIT.
             EXIT.
