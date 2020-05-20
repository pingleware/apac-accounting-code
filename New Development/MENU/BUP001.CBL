      $SET  GNT"BUP001.GNT"
      ******************************************************************
      *                                                                *
      *    *******   **    **  *******     ****      ****      **      *
      *    **    **  **    **  **    **   **  **    **  **    ***      *
      *    **    **  **    **  **    **  **    **  **    **    **      *
      *    *******   **    **  *******   **    **  **    **    **      *
      *    **    **  **    **  **        **    **  **    **    **      *
      *    **    **  **    **  **         **  **    **  **     **      *
      *    *******    ******   **          ****      ****    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     ( A P A C )   T E R M I N A L   S T A R T   &   I N I T    *
      *                                                                *
      *       Version 9.04.04 - February/March 2018                    *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  May 2010 - Include the updating of new files that are added   *
      *             to the system to the Work stations as the file     *
      *             containing the paths could be on a local drive and *
      *             not the Network drive.                             *
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
      *    Ё    SYSTEM FILE    Ё      CHAIN.WS     Ё      CHAIN.LS     Ё
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
       PROGRAM-ID.     BUP001.
       AUTHOR.         J.W.LEMMON.
       DATE-WRITTEN.   OCTOBER 1982.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1990 - 2018
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

      *COPY "APACFIDS.SL".

       COPY "APACFILE.SL".

       COPY "APCCOM.SL".

       COPY "SYSTEM.SL".

       COPY "PARAM.SL".

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

      *COPY "APACFIDS.FD".

       COPY "APACFILE.FD".

       COPY "APCCOM.FD".

       COPY "SYSTEM.FD".

       COPY "PARAM.FD".

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
       77  WS-CHECK                PIC  X(18)     VALUE    "aeWlimemnomLalismJ".
       77  WS-S1                   PIC  9(04)     COMP-5.
       77  WS-S2                   PIC  9(04)     COMP-5.
       77  WS-S3                   PIC  9(04)     COMP-5.
       77  WS-S4                   PIC  9(04)     COMP-5.
       77  WS-IXS1                 PIC  9(04)     COMP-5.
       77  WS-TERMS                PIC  9(04)     COMP-5.
       77  WS-PARKEY               PIC  9(04)     COMP-5.
       77  WS-SECKEY               PIC  9(04)     COMP-5.
       77  WS-START                PIC  9(08).
       77  WS-END                  PIC  9(08).
       77  WS-DATE                 PIC  Z9/99/9999.
       77  WS-OPTION               PIC  X(01).
       77  WS-REPLY                PIC  X(01).
       77  WS-PAR                  PIC  9(01)     VALUE 0.
       77  WS-ERROR                PIC  9(01)     VALUE 0.

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
           03  CRT-PROGRAM         PIC  X(15) VALUE "BUP001".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "WORK STATION: STARTUP/INITIALIZE PROGRAM".
      *
      *    Company Name
      *
           03  CRT-COMPANY         PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-TERMINAL.
               05  CRT-WRKHD       PIC  X(11) VALUE "Supervisor ".
               05  CRT-WRKST       PIC  X(03) VALUE "   ".

       COPY "WS.WS".

       COPY "CHAIN.WS".

       01  W05-COMMAND.
           03  W05-COM-LINE.
               05  W05-CHAR        PIC  X(01)    OCCURS 128.
           03  W05-COM-L                         REDEFINES W05-COM-LINE.
               05  W05-SHORT       PIC  X(28).
               05  FILLER          PIC  X(100).

       01  W05-DETAIL.
           03  W05-DC              PIC  X(01) OCCURS 15.

       01  W05-STRINGS.
           03  W05-DSK             PIC  X(01).
           03  W05-EX              PIC  X(03).
           03  W05-TR              PIC  X(03).
           03  W05-DR              PIC  X(02).
           03  W05-CS              PIC  X(01).

       01  W05-COM.
           03  W05-DISK            PIC  X(01).
           03  W05-EXT             PIC  X(03).
           03  W05-TRM             PIC  X(03).
           03  W05-DRW.
               05  W05-DRAW        PIC  9(02).
           03  W05-CSS             PIC  X(01).

       01  W05-TEMP.
           03  W05-NUM             PIC  X(03).
           03  W05-NCH1                          REDEFINES W05-NUM
               05  W05-NCHR        PIC  X(01)    OCCURS 3.
           03  W05-NUM2            PIC  X(03).
           03  W05-NCH2                          REDEFINES W05-NUM2
               05  W05-NCHR2       PIC  X(01)    OCCURS 3.
           03  W05-RESULT                        REDEFINES W05-NUM2.
               05  W05-NTRM        PIC  9(03).
           03  W05-REST                          REDEFINES W05-NUM2.
               05  FILLER          PIC  X(01).
               05  W05-NDRW        PIC  9(02).

       01  W10-FILE-PATHS.
           03  W10-CURRENT-PATH    PIC  X(52).
           03  W10-CURR-PATH                     REDEFINES W10-CURRENT-PATH.
               05  W10-CURR-CHAR   PIC  X(01)    OCCURS 52.
           03  W10-WRK-PATH        PIC  X(52).
           03  W10-WK-PATH                       REDEFINES W10-WRK-PATH.
               05  W10-WRK-CHAR    PIC  X(01)    OCCURS 52.
           03  W10-PATH            PIC  X(09).
           03  W10-COM-PATH                      REDEFINES W10-PATH.
               05  W10-COM-CHAR    PIC  X(01)    OCCURS 9.

       01  W10-USER-FIELDS.
           03  W10-USER            PIC  9(03)    VALUE ZERO.
           03  W10-PASS            PIC  X(08)    VALUE SPACES.

       COPY "W12.WS".

       01  W20-DATE.
           03  W20-DAY             PIC  9(02).
           03  W20-MONTH           PIC  9(02).
           03  W20-CENT            PIC  9(02).
           03  W20-YEAR            PIC  9(02).
       01  W20-DTE1                              REDEFINES W20-DATE.
           03  W20-DTE             PIC  9(08).
       01  W20-DTE2                              REDEFINES W20-DATE.
           03  W20-CC              PIC  9(02).
           03  W20-YY              PIC  9(02).
           03  W20-MM              PIC  9(02).
           03  W20-DD              PIC  9(02).
       01  W20-LEAP                              REDEFINES W20-DATE.
           03  FILLER              PIC  9(04).
           03  W20-CY              PIC  9(04).

       01  W25-CALCS.
           03  W25-RESULT          PIC  9(09)V99.
           03  W25-RESULT1                       REDEFINES W25-RESULT.
               05  FILLER          PIC  9(03).
               05  W25-WHOLE       PIC  9(06).
               05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2                       REDEFINES W25-RESULT.
               05  FILLER          PIC  9(02).
               05  W25-KEY         PIC  9(04).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).
           03  W25-PASS.
               05  W25-PASS1       PIC  9(03).
               05  W25-PASS2       PIC  9(03).
           03  W25-TIME            PIC  9(08).
           03  W25-TRED                          REDEFINES  W25-TIME.
               05  FILLER          PIC  9(02).
               05  W25-FACT        PIC  9(06).
               05  W25-DATE                      REDEFINES W25-FACT.
                   07  W25-YY      PIC  9(02).
                   07  W25-MM      PIC  9(02).
                   07  W25-DD      PIC  9(02).
           03  W25-CUR-CC          PIC  9(02)    COMP-5.
           03  W25-PRV-CC          PIC  9(02)    COMP-5.
           03  W25-NXT-CC          PIC  9(02)    COMP-5.
                                                 
       01  W30-MONTHS.
           03  W30-MONTH-NAMES.
               05  W30-ENG           PIC  X(36)    VALUE "JanFebMarAprMayJunJulAugSepOctNovDec".

           03  W30-MONTH                           REDEFINES W30-MONTH-NAMES PIC X(03) OCCURS 12.


       COPY "W40.WS".

       COPY "FUNCTION.WS".

       01  W75-KEYS.
           03  W75-S               PIC  9(02)    COMP-5.
           03  W75-S1              PIC  9(02)    COMP-5.
           03  W75-S2              PIC  9(02)    COMP-5.
           03  W75-COM             PIC  X(03)    OCCURS 18.

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
           03  L05-CHAR            PIC  X(128).

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

       COPY "LOGO.CRT".

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

       01  S19.
           03  LINE 16 COLUMN 17 VALUE "Please key in todays date (DDMMCCYY)" FOREGROUND-COLOR Brown HIGHLIGHT.

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
       AA000-MAIN        SECTION.
       AA00.
             DISPLAY SPACE AT 0101.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-USER-ID         TO WS-USER-ID.
             MOVE LS0-PROGRAMS       TO WS0-PROGRAMS.
             PERFORM AL000.
             PERFORM ERASE-SCREEN.
             MOVE W05-EXT            TO WS-SYS-ID W02-SYSID W02-SYSPTH.

       AA00-APAC.
             PERFORM APACFILES.
           IF NOT(WS-ERROR = 9)
               OPEN I-O SECUR.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   MOVE "Files locked - try later - Press ENTER" TO WS-ERR-MES
                   PERFORM ERROR-MESSAGE
                   STOP RUN.
           IF NOT(WS-STATUS = "00")
               MOVE "Invalid or file not on disk" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               STOP RUN.

       AA00A-CONTINUE.
             PERFORM AD000.
             PERFORM AE000.
             MOVE 2                  TO WS-SECKEY.

       AA02.
             READ SECUR.
           IF RUNTIME-ERROR
               IF RECORD-LOCKED
                   GO TO AA02.
           IF SEC-AGREE = "R" AND SEC-DAYS = ZERO
               MOVE "Contract must be renewed!" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               STOP RUN.
           IF WS-TERMS NOT > 0
               MOVE "Networking not installed" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               STOP RUN.
           IF WS-USE > WS-TERMS
               MOVE "Workstation number exceeds users allowed" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               STOP RUN.
             MOVE 5                  TO SHADE-ROW.
             MOVE 18                 TO SHADE-COL.
             MOVE 48                 TO SHADE-WIDTH.
             MOVE 13                 TO SHADE-LINES.
             DISPLAY S18.
             PERFORM SCREEN-SHADOW.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      *      ACCEPT WS-OPTION AT 1942 WITH AUTO-SKIP.
             MOVE "C"                TO LS-INSTR.
             MOVE SPACES             TO LS-NEXT-PRG.
             MOVE WS-PARID           TO LS-PARID.
             MOVE WS-USER-ID         TO LS-USER-ID.
             MOVE WS0-PROGRAMS       TO LS0-PROGRAMS.

       AA05.
             EXIT PROGRAM.
      *    *************************************************************
      *    *** N E T W O R K D R I V E   N O T   A V A I L A B L E
      *        M U S T   S Y S T E M   R U N   O F F - L I N E
      *    *************************************************************
      *AA100        SECTION.
      *AA105.
      *       PERFORM SAVE-SCREEN-3.
      *       DISPLAY "зддддддддддддддддддддддддддддддддддддддд" AT 1120 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Black
      *                                                       "©"        WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Magenta HIGHLIGHT.
      *       DISPLAY "Ё"                                        AT 1220 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Black
      *                "Network files not available at present!"         WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT
      *                                                       "Ё"        WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Magenta HIGHLIGHT.
      *       DISPLAY "Ё"                                        AT 1320 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Black
      *                "Work off-line (Y/N)                    "         WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT
      *                                                       "Ё"        WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Magenta HIGHLIGHT.
      *       DISPLAY "ю"                                        AT 1420 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Black
      *                "ддддддддддддддддддддддддддддддддддддддды"        WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Magenta HIGHLIGHT.
      *       MOVE 12                TO SHADE-ROW.
      *       MOVE 22                TO SHADE-COL.
      *       MOVE 40                TO SHADE-WIDTH.
      *       MOVE 3                 TO SHADE-LINES.
      *       PERFORM SCREEN-SHADOW.
      *       MOVE "N"               TO WS-OPTION.
      *
      *AA110.
      *      PERFORM WITH TEST AFTER UNTIL WS-OPTION = "N" OR "Y"
      *        ACCEPT WS-OPTION AT 1341 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO
      *        CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS
      *      END-PERFORM.
      *      PERFORM RESTORE-SCREEN-3.
      *
      *AA199.
      *      EXIT.
      *    *************************************************************
      *    *** F I L E S   N O T   A V A I L A B L E
      *        Check if this is a Network and if off-line files exist.
      *    *************************************************************
      *AA200        SECTION.
      *AA205.
      *      MOVE ZERO               TO WS-ERROR.
      *      MOVE "OFFLINE\"         TO W02-L-OR-N.
      *      MOVE WS-SYS-ID          TO W02-SYSID.
      *      CALL "CBL_CHECK_FILE_EXIST" USING W02-APACFIDS FILE-DETAILS RETURNING WS-STATUS.
      *    IF WS-STAT1H = 1
      *        MOVE 9                TO WS-ERROR.
      *      MOVE "SYSTEMS\"         TO W02-L-OR-N.
      *
      *AA999.
      *      EXIT.

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

      *
      *    *****    DATE VALIDATION ROUTINE    *****
      *
      *AB000-DATE              SECTION.
      *AB000-VALID-DATE.
      *     MOVE ZERO   TO WS-ERROR.
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
      *
      *AB005-FEB.
      *    IF W20-DAY > 29
      *        GO TO AB010-ERROR.
      *      DIVIDE W20-CY BY 4      GIVING W25-RESULT.
      *    IF W25-DECIMAL = 0
      *        GO TO AB999-EXIT.
      *
      *AB010-ERROR.
      *      MOVE 1                  TO WS-ERROR.
      *
      *AB999-EXIT.
      *      EXIT.

       AD000         SECTION.
       AD00.
           IF LS-INSTR = "X"
               MOVE W05-EXT          TO WS-SYS-ID
               MOVE "X"              TO W05-CSS
               MOVE WS-USER          TO WS-WRKST
               GO TO AD01.
             MOVE W05-DRAW           TO WS-DRAW.
             MOVE W05-TRM            TO WS-USER.
             MOVE WS-USER            TO WS-WRKST.
             MOVE W05-EXT            TO WS-SYS-ID.
           IF W05-CSS = "X"
               MOVE "Y"              TO WS-CSS
           ELSE
           IF W05-CSS = "Z"
               MOVE "N"              TO WS-CSS
           ELSE
               MOVE W05-CSS          TO WS-CSS.

       AD01.
             OPEN I-O PARAM.
           IF WS-STAT1 = "9"
               MOVE "Files locked - try later - Press ENTER" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               STOP RUN.
           IF WS-STATUS NOT = "00"
               GO TO AD06.
             MOVE 3                  TO WS-PARKEY.

       AD01-DATE.
             READ PARAM LOCK.
           IF RUNTIME-ERROR
               IF RECORD-LOCKED
                   GO TO AD01-DATE.
           IF WS-STATUS NOT = "00"
               MOVE "Disk error - AD01 (PARAM)" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               STOP RUN.
      *      MOVE PAR-CUR-YR         TO WS-CUR-YR.
      *      MOVE PAR-CUR-CEN        TO WS-CUR-CEN.
      *      MOVE PAR-PRV-CEN        TO WS-PRV-CEN.
      *      MOVE PAR-NXT-CEN        TO WS-NXT-CEN.
      *
      *    ****    G E T   R U N   D A T E
      *
             MOVE 1                  TO WS-PARKEY.
             READ PARAM IGNORE LOCK.
             MOVE PAR-DMY            TO W12-TODAY WS-DATE TODAY-DDMMYY.
             MOVE PAR-YMD            TO W12-T-YMD.
      *
      *    ****    CHECK IF LAST RUN ENDED CORRECTLY
      *
            MOVE 4   TO WS-PARKEY.
       AD02.
             READ PARAM LOCK.
           IF WS-STAT1 = "9"
               IF RECORD-LOCKED
                   GO TO AD02.
          IF PAR-LOGON NOT = 1
              DISPLAY "Supervisor not signed on" AT 1004
         WITH FOREGROUND-COLOR Brown HIGHLIGHT
        DISPLAY "Run Date is " AT 1204
   WS-DATE WITH FOREGROUND-COLOR Brown HIGHLIGHT
        DISPLAY "C" AT 1404 WITH FOREGROUND-COLOR Brown HIGHLIGHT
         "ontinue,"
         "N" WITH FOREGROUND-COLOR Brown HIGHLIGHT
         "ew date or "
         "E" WITH FOREGROUND-COLOR Brown HIGHLIGHT
         "xit"
        PERFORM AD10 THRU AD99
              PERFORM ERASE-SCREEN
        IF WS-OPTION = "E"
           UNLOCK PARAM
           STOP RUN.
           IF PAR-USERS = 0
              MOVE 1   TO WS-PAR
               GO TO AD07U.
            MOVE 1   TO WS-S1.
            MOVE 0   TO WS-S2 WS-S3.

       AD05-CHECK.
          IF PAR-USR(WS-S1) = WS-USER
              IF PAR-PROG(WS-S1) = SPACES
     MOVE SPACES  TO PAR-USR(WS-S1)
     SUBTRACT 1  FROM PAR-USERS
               ELSE
                   GO TO AD06.
           IF WS-S1 < 24
              ADD 1   TO WS-S1
               GO TO AD05-CHECK.
    IF NOT(LS-INSTR = "X")
              GO TO AD07U.
    IF PAR-LOGON = 1
        MOVE "Y"   TO WS-REPLY
    ELSE
        MOVE "N"   TO WS-REPLY.
             REWRITE PAR-RECORD1.
            GO TO AD07U.

       AD06.
             DISPLAY "Last run not ended off correctly" AT 2014
              WITH FOREGROUND-COLOR Brown HIGHLIGHT
             DISPLAY
                   "Recovery procedure must be initiated by Supervisor"
            AT 2114 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
        DISPLAY WS-STATUS AT 2502
         WITH BACKGROUND-COLOR Cyan
       FOREGROUND-COLOR Grey HIGHLIGHT
         " " WITH BACKGROUND-COLOR Cyan
         WS-PARID WITH BACKGROUND-COLOR Cyan
         FOREGROUND-COLOR Brown HIGHLIGHT.

       AD07.
            MOVE 9   TO WS-PAR.
             UNLOCK PARAM.
             STOP RUN.

       AD07U.
             REWRITE PAR-RECORD1.
             UNLOCK PARAM.
             CLOSE PARAM.
           IF NOT WS-STAT1 = "0"
               MOVE "User file error" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               STOP RUN.
             GO TO AD999.

       AD10.
             PERFORM WITH TEST AFTER UNTIL WS-OPTION = "C" OR "N" OR "E"
               ACCEPT WS-OPTION AT 1430 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Brown HIGHLIGHT AUTO
               CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH
                                        RETURNING WS-STATUS
             END-PERFORM.
           IF NOT(WS-OPTION = "N")
               GO TO AD99.
             MOVE 1                  TO WS-PARKEY.

       AD12.
             READ PARAM.
           IF RUNTIME-ERROR
               IF RECORD-LOCKED
                   GO TO AD12.
             MOVE 2                  TO WS-SECKEY.

       AD14.
             READ SECUR.
           IF RUNTIME-ERROR
               IF RECORD-LOCKED
                   GO TO AD14.

       AD15.
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
             DISPLAY S19.

       AD20.
             MOVE 1654               TO DATEPOS.
             MOVE SPACE              TO W22-INSTR.
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
               MOVE "E"              TO WS-OPTION
               GO TO AD99.
             MOVE W22-DTE            TO W12-TODAY TODAY-DDMMYY SEC-DATE.
             MOVE W20-DTE            TO W12-T-YMD TODAY-YYMMDD.
             MOVE 18                 TO CRT-START.
             MOVE 20                 TO CRT-END.
             PERFORM ERASE-SCREEN.
             DISPLAY "Todays date - " AT 1829 W12-DD " " W30-MONTH(W12-MM) "," W12-CC W12-YY.
             DISPLAY "Is date correct (Y/N) ?" AT 2029 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             MOVE SPACE              TO WS-OPTION.

       AD25.
             PERFORM WITH TEST AFTER UNTIL WS-OPTION = "N" OR "Y"
                 ACCEPT WS-OPTION AT 2053 WITH FOREGROUND-COLOR Grey HIGHLIGHT UPDATE AUTO
                 CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS
             END-PERFORM.
           IF WS-OPTION = "N"
               GO TO AD20.
             MOVE W12-TODAY          TO PAR-DMY.
             MOVE W12-T-YMD          TO PAR-YMD.
             REWRITE PAR-RECORD1.

       AD99.
             EXIT.

       AD999.
             EXIT.

       AE000           SECTION.
       AE00.
            MOVE 2                   TO WS-SECKEY.

       AE02.
             READ SECUR.
           IF WS-STAT1 = "9"
               IF RECORD-LOCKED
                   GO TO AE02.
           IF WS-STATUS NOT = "00"
               MOVE "Disk error - AE00" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               STOP RUN.
             MOVE SEC-COMP           TO WS-COMPANY.
             MOVE SEC-SYS            TO APAC-SYSTEM WS-APAC-SYSTEM.
      *    IF NOT(SEC-VER = "2010" OR "2011")
      *        PERFORM AF000
      *        MOVE "2011"           TO SEC-VER.
             MOVE SEC-VER            TO APAC-VERSION WS-APAC-VERSION.
             MOVE 40                 TO WS-S1.

       AE05.
           IF WS-COMPCHR(WS-S1) = SPACE
               MOVE WS-G8            TO WS-COMPCHR(WS-S1)
               IF WS-S1 > 1
                   SUBTRACT 1        FROM WS-S1
                   GO TO AE05.
             MOVE WS-COMPANY         TO WS-TOP-COMP.
           IF WS-USER = WS-SYS-ID
               MOVE "Supervisorд"    TO WS-WRKHD
           ELSE
               MOVE "Workstation"    TO WS-WRKHD
               MOVE WS-USER          TO WS-WRKST.
             MOVE SEC-PMODS          TO WS0-PMODS.
             MOVE SEC-DEM            TO WS-DEM.
             MOVE SEC-TERMS          TO WS-TERMS.
             MOVE CRT-TERMINAL       TO WS-TERMINAL.
             CLOSE SECUR.

       AE999.
             EXIT.

      *
      *    The following code is now handled by program FILEIDS and will
      *    be deleted, once testing of the system has been completed.
      *
      *    Now been commented out.
      *
      *AF000        SECTION.
      *AF00.
      *      OPEN I-O APACFILE.
      *      CLOSE APACFIDS.
      *
      *  Get the path used for the existing files.
      *
      *      MOVE "PARAM"            TO APAC-KEY.
      *      READ APACFILE WITH IGNORE LOCK
      *                    KEY IS APAC-KEY.
      *      MOVE APAC-PATH          TO W10-CURRENT-PATH.
      *      MOVE "CORDER"           TO APAC-KEY.
      *      READ APACFILE WITH IGNORE LOCK KEY IS APAC-KEY.
      *      MOVE APAC-PATH          TO W10-WRK-PATH.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё CHECK FOR THE PRESENCE OF NEW FILES THAT ARE ADDED TO THE Ё
      *    Ё SYSTEM AND UPDATE THE PATH FILE WITH MISSING FILE NAMES.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *    *************************************************************
      *    ****   C R E D I T O R S   M E M O W O R K   F I L E   ****
      *         Version 8.15.01
      *    *************************************************************
      *      MOVE "MEMCRD"           TO APAC-KEY.
      *      READ APACFILE WITH IGNORE LOCK KEY IS APAC-KEY.
      *    IF NOT(WS-STATUS = "00")
      *        MOVE "MEMCRD"         TO APAC-KEY
      *        MOVE "CRDM_XXX.WRK"   TO APAC-NAME
      *        MOVE ZERO             TO APAC-FILE
      *        MOVE W10-WRK-PATH     TO APAC-PATH
      *        WRITE APAC-RECORD1
      *        MOVE WS-SYS-ID        TO W02-SYSID
      *        OPEN I-O APACFIDS
      *        MOVE APAC-KEY         TO AFID-KEY
      *        MOVE SPACES           TO AFID-PATH
      *        STRING APAC-PATH DELIMITED BY " " APAC-NAME DELIMITED BY " " INTO AFID-PATH
      *        WRITE AFID-RECORD1
      *        CLOSE APACFIDS.
      *    *************************************************************
      *    ****        I N V O I C E   B I T M A P   F I L E       ****
      *                         Version 8.14.03
      *    *************************************************************
      *      MOVE "INVBMP"  TO APAC-KEY.
      *      READ APACFILE WITH IGNORE LOCK 
      *                    KEY IS APAC-KEY.
      *    IF NOT(WS-STATUS = "00")
      *        STRING WS-SYS-ID DELIMITED SIZE "_INV.BMP" DELIMITED SIZE INTO APAC-NAME
      *        MOVE W10-CURRENT-PATH TO APAC-PATH
      *        WRITE APAC-RECORD1
      *        MOVE WS-SYS-ID        TO W02-SYSID
      *        OPEN I-O APACFIDS
      *        MOVE APAC-KEY         TO AFID-KEY
      *        MOVE SPACES           TO AFID-PATH
      *        STRING APAC-PATH DELIMITED BY " " APAC-NAME DELIMITED BY " " INTO AFID-PATH
      *        WRITE AFID-RECORD1
      *        CLOSE APACFIDS.
      *    *************************************************************
      *    ****      S T A T E M E N T   B I T M A P   F I L E      ****
      *                        Version 8.14.03
      *    *************************************************************
      *      MOVE "STMBMP"  TO APAC-KEY.
      *      READ APACFILE WITH IGNORE LOCK
      *                    KEY IS APAC-KEY.
      *    IF NOT(WS-STATUS = "00")
      *        MOVE "STMBMP"         TO APAC-KEY
      *        MOVE SPACES           TO APAC-NAME
      *        STRING WS-SYS-ID DELIMITED SIZE "_STM.BMP" DELIMITED SIZE INTO APAC-NAME
      *        MOVE W10-CURRENT-PATH TO APAC-PATH
      *        WRITE APAC-RECORD1
      *        MOVE WS-SYS-ID        TO W02-SYSID
      *        OPEN I-O APACFIDS
      *        MOVE APAC-KEY         TO AFID-KEY
      *        MOVE SPACES           TO AFID-PATH
      *        STRING APAC-PATH DELIMITED BY " " APAC-NAME DELIMITED BY " " INTO AFID-PATH
      *        WRITE AFID-RECORD1
      *        CLOSE APACFIDS.
      *    *************************************************************
      *                    Version 8.14.03
      *    *************************************************************
      *      MOVE "DEALER"           TO APAC-KEY.
      *      READ APACFILE WITH IGNORE LOCK
      *                    KEY IS APAC-KEY.
      *    IF NOT(WS-STATUS = "00")
      *        MOVE "DEALER"         TO APAC-KEY
      *        MOVE "DEAL_XXX.WRK"   TO APAC-NAME
      *        MOVE W10-WRK-PATH     TO APAC-PATH
      *        WRITE APAC-RECORD1
      *        MOVE WS-SYS-ID        TO W02-SYSID
      *        OPEN I-O APACFIDS
      *        MOVE APAC-KEY         TO AFID-KEY
      *        MOVE SPACES           TO AFID-PATH
      *        STRING APAC-PATH DELIMITED BY " " APAC-NAME DELIMITED BY " " INTO AFID-PATH
      *        WRITE AFID-RECORD1
      *        CLOSE APACFIDS.
      *      MOVE "DEATOT"           TO APAC-KEY.
      *      READ APACFILE WITH IGNORE LOCK
      *                    KEY IS APAC-KEY.
      *    IF NOT(WS-STATUS = "00")
      *        MOVE "DEATOT"         TO APAC-KEY
      *        MOVE "DEAT_XXX.WRK"   TO APAC-NAME
      *        MOVE W10-WRK-PATH     TO APAC-PATH
      *        WRITE APAC-RECORD1
      *        MOVE WS-SYS-ID        TO W02-SYSID
      *        OPEN I-O APACFIDS
      *        MOVE APAC-KEY         TO AFID-KEY
      *        MOVE SPACES           TO AFID-PATH
      *        STRING APAC-PATH DELIMITED BY " " APAC-NAME DELIMITED BY " " INTO AFID-PATH
      *        WRITE AFID-RECORD1
      *        CLOSE APACFIDS.
      *    *************************************************************
      *    ****    D E B T O R   N A M E   I N D E X   F I L E     ****
      *                      Version 8.13.8
      *    *************************************************************
      *      MOVE "DEBALT"           TO APAC-KEY.
      *      READ APACFILE WITH IGNORE LOCK
      *                    KEY IS APAC-KEY.
      *    IF NOT(WS-STATUS = "00")
      *        MOVE "DEBALT"         TO APAC-KEY
      *        MOVE "AR_NAME.DAT"    TO APAC-NAME
      *        MOVE W10-CURRENT-PATH TO APAC-PATH
      *        WRITE APAC-RECORD1
      *        OPEN I-O APACFIDS
      *        MOVE APAC-KEY         TO AFID-KEY
      *        MOVE SPACES           TO AFID-PATH
      *        STRING APAC-PATH DELIMITED BY " " APAC-NAME DELIMITED BY " " INTO AFID-PATH
      *        WRITE AFID-RECORD1
      *        CLOSE APACFIDS.
      *      CLOSE APACFILE.
      *      OPEN INPUT APACFIDS.
      *
      *AF999.
      *      EXIT.

       AL000           SECTION.
       AL00.
             ACCEPT WS-START FROM TIME.
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

       AL05B.
           IF LS-INSTR = "X"
               GO TO AL45.
      *
      *    Parse the command line
      *
             MOVE L05-COMMAND        TO W05-COMMAND.
             MOVE 1                  TO WS-S1 WS-S2.

       AL10.
           IF W05-CHAR(WS-S1) = "*"
               GO TO AL15.
           IF WS-S1 < 117
               ADD 1                 TO WS-S1
               GO TO AL10.
             MOVE 1                  TO WS-ERROR.
             GO TO AL30.

       AL15.
             ADD 2                   TO WS-S1.

       AL20.
           IF W05-CHAR(WS-S1) = " "
               GO TO AL25.
             MOVE W05-CHAR(WS-S1)    TO W05-DC(WS-S2).
           IF WS-S2 < 15
               ADD 1                 TO WS-S2
           ELSE
               GO TO AL26.

       AL25.
           IF WS-S1 < 128
               ADD 1                 TO WS-S1
               GO TO AL20.

       AL26.
             MOVE SPACES             TO W05-STRINGS.
             MOVE ","                TO W05-DC(WS-S2).
             UNSTRING W05-DETAIL DELIMITED BY "," INTO W05-DSK W05-EX W05-TR W05-DR W05-CS
                 OVERFLOW MOVE 0     TO WS-ERROR.
             MOVE W05-TR             TO W05-NUM.
             PERFORM AL27 THRU AL28.
             MOVE W05-NTRM           TO W05-TRM.
           IF W05-TRM = ZERO
               MOVE 2                TO WS-ERROR
               GO TO AL30.
             MOVE W05-DR             TO W05-NUM.
             PERFORM AL27 THRU AL28.
             MOVE W05-NDRW           TO W05-DRAW.
             MOVE W05-DSK            TO W05-DISK.
             MOVE W05-EX             TO W05-EXT.
             CALL "CBL_TOUPPER" USING W05-CS BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE W05-CS             TO W05-CSS.
             GO TO AL30.

       AL27.
             MOVE 3                  TO WS-S1 WS-S2.
             MOVE ZERO               TO W05-NUM2.

       AL28.
           IF W05-NCHR(WS-S1) NUMERIC
               MOVE W05-NCHR(WS-S1)  TO W05-NCHR2(WS-S2)
               SUBTRACT 1            FROM WS-S1 WS-S2
           ELSE
               SUBTRACT 1            FROM WS-S1.
           IF WS-S1 > ZERO
               GO TO AL28.

       AL30.
           IF WS-ERROR = ZERO
               IF W05-DRW NOT NUMERIC
                   MOVE 2            TO WS-ERROR
               ELSE
               IF W05-DRAW > 39
                   MOVE 3            TO WS-ERROR
               ELSE
               IF W05-TRM NOT NUMERIC
                   MOVE 4            TO WS-ERROR
               ELSE
               IF W05-TRM > 100
                   MOVE 5            TO WS-ERROR.
             MOVE ZERO               TO WS-S1 WS-S2.

       AL35.
             CALL X"D9" USING WS-CONSOLE.
           IF WS-CONSOLE NOT = ZERO
               ACCEPT WS-OPTION WITH AUTO-SKIP
               GO TO AL40.
             ADD 1                   TO WS-S1 WS-S2.
             ACCEPT WS-END FROM TIME.
             SUBTRACT WS-START       FROM WS-END.
           IF WS-END < 400
               GO TO AL35.
       AL40.
           IF WS-ERROR = 0
               GO TO AL999.
             PERFORM ERASE-SCREEN.
           IF WS-ERROR = 2 OR 3
               MOVE "Cash drawer not in range 00 to 39"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
           ELSE
           IF WS-ERROR = 4 OR 5
               MOVE "Terminal not in range 001 to 100"   TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
           ELSE
               MOVE "Invalid Parameters in command line" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE.
             STOP RUN.

       AL45.
             PERFORM ERASE-SCREEN.
             DISPLAY "Key in Company code"       AT 1212.
             DISPLAY "F2"                        AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                       " to view existing codes"         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             MOVE WS-SYS-ID          TO W05-EXT.

       AL50.
             ACCEPT W05-EXT AT 1232 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT UPDATE AUTO.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN F2-KEY   OPEN INPUT APCCOM
                               PERFORM COMPANY-LOOKUP
                               CLOSE APCCOM
                               GO TO AL50
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO AL50
               END-EVALUATE.
                   MOVE 3            TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W05-EXT BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
           IF W05-EXT = SPACES
               MOVE WS-SYS-ID        TO W05-EXT.

       AL999.
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
