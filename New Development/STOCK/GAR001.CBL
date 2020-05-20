      $SET LINKCOUNT"256" GNT"GAR001.GNT"
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
      *    ******      **     *******     ****      ****       **      *
      *   **    **    ****    **    **   **  **    **  **     ***      *
      *   **         **  **   **   **   **    **  **    **     **      *
      *   **        **    **  ******    **    **  **    **     **      *
      *   **   ***  ********  **   **   **    **  **    **     **      *
      *   **    **  **    **  **    **   **  **    **  **      **      *
      *    ******   **    **  **    **    ****      ****     ******    *
      *                                                                *
      *   ENGLISH                                                      *
      *                                                                *
      *   G U A R A N T E E   M O D U L E                              *
      *                                                                *
      *       Version 9.04.03 - September 2017                         *

      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     GAR001.
       AUTHOR.         J.W.LEMMON.
       DATE-WRITTEN.   OCTOBER 1991.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1991 - 2018
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

       COPY "GARTEE.SL".

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

       COPY "CONTROL.FD".

       COPY "GARTEE.FD".

       COPY "PARAM.FD".

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
       77  WS-SUB                  PIC  9(04)    COMP-5.
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
       77  WS-IXS2                 PIC  9(04)    COMP-5.
       77  WS-PARKEY               PIC  9(04)    COMP-5.
       77  WS-NETKEY               PIC  9(04)    COMP-5.
       77  WS-ETYPE                PIC  X(01).
       77  WS-USE-CASES            PIC  X(01).
       77  WS-USE-PACKS            PIC  X(01).
       77  WS-USE-ITM              PIC  X(01).
       77  WS-EXT-STK              PIC  X(01).
       77  WS-CARDEX               PIC  X(01).
       77  WS-LINES                PIC  9(02)           VALUE 0.
       77  WS-PAGE                 PIC  9(02)    COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-NUM                  PIC  Z9.
       77  WS-ERROR                PIC  9(01).
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *         4 = Clear line 46 and line 50
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "GAR001".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY GUARANTEE MODULE".
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
           03  WS-MODULE           PIC  X(03)           VALUE "GAR".
           03  WS-PROG             PIC  X(03)           VALUE "001".

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

       01  W02-FILE-IDS.
           03  W02-PRINTER.
               05  W02-REPORT      PIC  X(07)           VALUE "GAR001.".
               05  W02-USER        PIC  X(05)           VALUE SPACES.

       01  W10-GUARANTEE.
           03  W10-EXT-ITEM.
               05  W10-ITEM        PIC  X(14).
               05  W10-ITM         PIC  X(03).
               05  FILLER          PIC  X(01).
           03  W10-SERNO           PIC  X(18).
           03  W10-DESC.
               05  W10-SDSC.
                   07  W10-DLUP    PIC  X(20).
                   07  FILLER      PIC  X(10).
           03  W10-NAME.
               05  W10-NLUP.
                   07  W10-NMELUP  PIC  X(10).
                   07  FILLER      PIC  X(10).
               05  FILLER          PIC  X(20).
           03  W10-DATE1           PIC  9(08)    COMP-3.
           03  W10-DATE2           PIC  9(08)    COMP-3.
           03  W10-PERIOD          PIC  9(02)    COMP-5.
           03  W10-INV             PIC  X(08).

       01  W10-SPARES.
           03  W10-DATE            PIC  X(10).
           03  W10-EDTE            PIC 99/99/9999 REDEFINES W10-DATE.

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

       COPY "W20.WS".

       COPY "W25.STK".

       01  W30-GUAR.
           03  W30-KEY.
               05  W30-EXT-CODE.
                   07  W30-CODE    PIC  X(14).
                   07  W30-ITM     PIC  X(03).
                   07  FILLER      PIC  X(01).
              05  W30-SERIAL       PIC  X(18).
           03  W30-DESC            PIC  X(30).
           03  W30-NAME.
               05  W30-NKEY        PIC  X(12).
               05  FILLER          PIC  X(28).
           03  W30-DATE1           PIC  9(08)    COMP.
           03  W30-DATE2           PIC  9(08)    COMP.
           03  W30-PERIOD          PIC  9(02)    COMP-5.
           03  W30-INV             PIC  X(08).
           03  FILLER              PIC  X(15).

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W75-KEYS.
           03  W75-S               PIC 9(02)     COMP-5.
           03  W75-S1              PIC 9(02)     COMP-5.
           03  W75-S2              PIC 9(02)     COMP-5.
           03  W75-ITEMS                                OCCURS 18.
               05  W75-CODE        PIC X(18).
               05  W75-SERNO       PIC X(18).

       01  W85-PASS.
           03  W85-SUPER           PIC  X(06)           OCCURS 9.
           03  W85-ENTRY           PIC  9(02)    COMP-3.
           03  W85-MARG            PIC S9(02)V99.

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

       01  S-PROG.
           03  LINE  6 COLUMN 28 VALUE "GUARANTEE PROGRAM CONTROL" FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S-MENU.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan.
           05  LINE 10 COLUMN 26 VALUE "                              ".
           05  LINE 11 COLUMN 26 VALUE "      GUARANTEE OPTIONS       " FOREGROUND-COLOR Grey HIGHLIGHT.
           05  LINE 12 COLUMN 26 VALUE " 1"                             FOREGROUND-COLOR Grey HIGHLIGHT.
           05          COLUMN 28 VALUE   " Amend Guarantee Record     ".
           05  LINE 13 COLUMN 26 VALUE " 2"                             FOREGROUND-COLOR Grey HIGHLIGHT.
           05          COLUMN 28 VALUE   " Enquiry                    ".
           05  LINE 14 COLUMN 26 VALUE " 3"                             FOREGROUND-COLOR Grey HIGHLIGHT.
           05          COLUMN 28 VALUE   " Delete a Guarantee Record  ".
           05  LINE 15 COLUMN 26 VALUE " 4"                             FOREGROUND-COLOR Grey HIGHLIGHT.
           05          COLUMN 28 VALUE   " Amend a Serial Number      ".
           05  LINE 16 COLUMN 26 VALUE " 5"                             FOREGROUND-COLOR Grey HIGHLIGHT.
           05          COLUMN 28 VALUE   " Replace an Item            ".
           05  LINE 17 COLUMN 26 VALUE " 6"                             FOREGROUND-COLOR Grey HIGHLIGHT.
           05          COLUMN 28 VALUE   " Delete Expired Records     ".
           05  LINE 18 COLUMN 26 VALUE " 0"                             FOREGROUND-COLOR Grey HIGHLIGHT.
           05          COLUMN 28 VALUE   " Exit                       ".
           05  LINE 19 COLUMN 26 VALUE "                              ".

       01  S02.
           03  LINE 12 COLUMN 12 VALUE "Enter Password >      <".

       01  S03.
          03  LINE 12 COLUMN 28 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta PIC X(06)
                                 USING W40-CSHPASS AUTO SECURE.
       01  S04.
          03  LINE  2 COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
                                 VALUE "AMEND GUARANTEE RECORD".
       01  S05.
    03  LINE  4 COLUMN  6 VALUE "Item Code       :".
    03        COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(14) USING WS-SPACES.
    03  LINE  5 COLUMN  6 VALUE "Serial Number   :".
    03        COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(18) USING WS-SPACES.
    03  LINE  6 COLUMN  6 VALUE "Item Description:".
    03        COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(30) USING WS-SPACES.
    03  LINE  7 COLUMN  6 VALUE "Client Name     :".
    03        COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(40) USING WS-SPACES.
    03  LINE  8 COLUMN  6 VALUE "Date Purchased  :".
    03        COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(10) USING WS-SPACES.
    03  LINE  9 COLUMN  6 VALUE "Expiry Date     :".
    03        COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(10) USING WS-SPACES.
    03  LINE 10 COLUMN  6 VALUE "Guarantee Period:".
    03        COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(02) USING WS-SPACES.
    03        COLUMN 27 VALUE "(Months)".
    03  LINE 11 COLUMN  6 VALUE "Invoice Number  :".
    03        COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(08) USING WS-SPACES.

       01  S05-ITM.
    03  LINE  4 COLUMN 38 FOREGROUND-COLOR Black BACKGROUND-COLOR Magenta
     VALUE "/".
    03        COLUMN 39 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(03) USING WS-SPACES.

       01  S05-EXT.
    03  LINE  4 COLUMN 38 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(04) USING WS-SPACES.

       01  S06.
          03  LINE  2 COLUMN 32 FOREGROUND-COLOR Grey HIGHLIGHT
                                 VALUE "GUARANTEE ENQUIRY".

       01  S07.
          03  LINE  2 COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
                                 VALUE "DELETE GUARANTEE RECORD".

       01  S08.
          03  LINE  2 COLUMN 30 FOREGROUND-COLOR Grey HIGHLIGHT
                                 VALUE "AMEND SERIAL NUMBER".

       01  S09.
          03  LINE  2 COLUMN 26 FOREGROUND-COLOR Grey HIGHLIGHT
                                 VALUE "REPLACE AN ITEM UNDER WARRANTY".

       01  S10.
          03  LINE  2 COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
                                 VALUE "DELETE EXPIRED RECORDS".
       01  S11.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               05  S11A.
                   07  LINE  4 COLUMN 24     PIC  X(14)      USING W10-ITEM   AUTO.
                   07  LINE  5 COLUMN 24     PIC  X(18)      USING W10-SERNO  AUTO.
               05  S11B.
                   07  LINE  6 COLUMN 24     PIC  X(30)      USING W10-SDSC   AUTO.
                   07  S11C.
                       09  LINE  7 COLUMN 24 PIC  X(40)      USING W10-NAME   AUTO.
                       09  LINE  8 COLUMN 24 PIC  99/99/9999 USING W10-DATE1  AUTO.
                   07  LINE  9 COLUMN 24     PIC  99/99/9999 USING W10-DATE2  AUTO.
                   07  LINE 10 COLUMN 24     PIC  Z9         USING W10-PERIOD AUTO.
                   07  LINE 11 COLUMN 24     PIC  X(08)      USING W10-INV    AUTO.

       01  S11-ITM.
    03  LINE  4 COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(14) USING W10-ITEM AUTO.
    03        COLUMN 38 FOREGROUND-COLOR Black BACKGROUND-COLOR Magenta
     VALUE "/".
    03        COLUMN 39 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(03) USING W10-ITM AUTO.
    03  LINE  5 COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(18) USING W10-SERNO AUTO.

       01  S11-EXT.
    03  LINE  4 COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(18) USING W10-EXT-ITEM AUTO.
    03  LINE  5 COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(18) USING W10-SERNO AUTO.

       01  S12.
    03  LINE 15 COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(18) USING W10-EXT-ITEM AUTO.
    03  LINE 16 COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(18) USING W10-SERNO AUTO.

       COPY GARLUP.CRT.

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
           IF LS0-SALEV < 1
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM BA000.
             GO TO AZ000-EOJ.

       AA49.
             EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
       AA50.
             PERFORM SAVE-SCREEN.
             MOVE 4                  TO W44-FUNCTION.
             PERFORM SCREEN-CONTENTS.
             PERFORM GUAR-LOOKUP.
             PERFORM RESTORE-SCREEN.

       AA99.
             EXIT.
      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S
      *
       AA300              SECTION.
       AA301.
             DISPLAY "F1"                                                                  AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                       "=Help,"                                                                    WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                             "F2"                                                                  WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                               "=Item code & Ser.No./ "                                            WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                                     "F3"                                          WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                                       "=Item code/ "                              WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                                                   "F4"                            WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                                                     "=Serial No./ "               WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                                                                  "F5"             WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                                                                    "=Client name" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
       AA305.
             EXIT.
      *
      *    ****   G E T   G U A R A N T E E   R E C O R D   U S I N G
      *           I T E M   C O D E   &   S E R I A L   N U M B E R
      *
       AA500         SECTION.
       AA501.
             MOVE 18                 TO W50-LENGTH.
           IF W10-EXT-ITEM = SPACES
               MOVE "!"              TO W10-EXT-ITEM.
       AA505.
             MOVE W10-EXT-ITEM       TO W50-DATA.
      *
      *    ****    R E M O V E  L E A D I N G  S P A C E S
      *
           IF W50-1 = SPACE
               MOVE W50-17           TO W10-EXT-ITEM
               GO TO AA505.
      *
      *    ****    C O N V E R T   I T E M   C O D E   T O   U P P E R
      *     C A S E   C H A R A C T E R S
      *
             CALL "CBL_TOUPPER" USING W50-DATA BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
             MOVE W50-EXT-ITEM       TO W10-EXT-ITEM.

       AA510.
             MOVE W50-EXT-ITEM       TO GUA-EXT-CODE.

       AA512.
             MOVE 18                 TO W50-LENGTH.
           IF W10-SERNO = SPACE
               MOVE "!"              TO W10-SERNO.

       AA515.
             MOVE W10-SERNO          TO W50-DATA.
      *
      *    ****    R E M O V E  L E A D I N G  S P A C E S
      *
           IF W50-1 = SPACE
               MOVE W50-17           TO W10-SERNO
               GO TO AA515.
      *
      *    ****    C O N V E R T   S E R I A L  N O   T O   U P P E R
      *                      C A S E   C H A R A C T E R S
      *
             CALL "CBL_TOUPPER" USING W50-DATA BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
             MOVE W50-EX-CDE         TO W10-SERNO.

       AA520.
             MOVE W10-SERNO          TO GUA-SERIAL.

       AA525.
             PERFORM READ-GARTEE THRU READ-GARTEE-EXIT.
           IF WS-F-ERROR NOT = 0
               GO TO AA599.
             MOVE GUA-DESC           TO W10-DESC.
             MOVE GUA-NAME           TO W10-NAME.
             MOVE GUA-DATE1          TO W22-DTE2.
             PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT
             MOVE W22-DTE3           TO W10-DATE1.
             MOVE GUA-DATE2          TO W22-DTE2.
             PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT
             MOVE W22-DTE3           TO W10-DATE2.
             MOVE GUA-PERIOD         TO W10-PERIOD.
             MOVE GUA-INV            TO W10-INV.

       AA599.
             EXIT.

       COPY "AA900.ALM".

       COPY "LOCKED.RC2".

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

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000              SECTION.

       COPY "CONTROL.RD".

       COPY "GARTEE.RD".

       COPY "PARAM.RD".

      *
      *   R E W R I T E   &    W R I T E   F I L E S   R O U T I N E S
      *
       AD000-WRITE             SECTION.

       COPY "CONTROL.WR".

       COPY "GARTEE.WR".

       COPY "PARAM.WR".

       COPY GARTEE.LUP.

       COPY "APAC.HLP".

      /
       AZ000-END               SECTION.
       AZ000-EOJ.
            CLOSE GARTEE NETWORK PARAM.

       AZ100.
             EXIT PROGRAM.

      /
       BA000           SECTION.
       BA00.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66               TO MOUSE-FUNC
               MOVE 1                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
             PERFORM HEADING-AND-CLEAR-SCREEN.
             DISPLAY S-PROG.
             MOVE 10                 TO SHADE-ROW.
             MOVE 26                 TO SHADE-COL.
             MOVE 30                 TO SHADE-WIDTH.
             MOVE 10                 TO SHADE-LINES.
             DISPLAY S-MENU.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
           IF LS-DAYS > 65
               COMPUTE WS-NUM = 15 - (LS-DAYS - 65)
               MOVE SPACES           TO WS-ERR-MES
               STRING "This package must be registered within " DELIMITED SIZE 
               WS-NUM DELIMITED SIZE " days" DELIMITED SIZE INTO WS-ERR-MES
               PERFORM ERROR-MESSAGE.

       BA02.
             MOVE 11                 TO TOP-ROW.
             MOVE 17                 TO BOTTOM-ROW.
             MOVE 10                 TO SCREEN-LIN.
             MOVE 27                 TO SCREEN-COL.
             MOVE 27                 TO STRING-LENGTH.
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
               IF WS-OPTION < 0 OR > 6
                   PERFORM AA900-ALARM
                   GO TO BA05
               END-IF
               GO TO BA16
           ELSE
               PERFORM AA900-ALARM
               GO TO BA05.

       BA15.
             EVALUATE SCREEN-LIN
               WHEN 11     MOVE "1"  TO WS-OPTION
               WHEN 12     MOVE "2"  TO WS-OPTION
               WHEN 13     MOVE "3"  TO WS-OPTION
               WHEN 14     MOVE "4"  TO WS-OPTION
               WHEN 15     MOVE "5"  TO WS-OPTION
               WHEN 16     MOVE "6"  TO WS-OPTION
               WHEN 17     MOVE "0"  TO WS-OPTION
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO BA05
             END-EVALUATE.
             GO TO BA18.

       BA16.
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

       BA18.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66               TO MOUSE-FUNC
               MOVE 0                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
             EVALUATE WS-OPTION
               WHEN "0"    GO TO BA999.
               WHEN "1"    PERFORM CA000
               WHEN "2"    PERFORM DA000
               WHEN "3"    PERFORM EA000
               WHEN "4"    PERFORM FA000
               WHEN "5"    PERFORM GA000
               WHEN "6"    PERFORM HA000
             END-EVALUATE.
             GO TO BA00.

       BA999.
             EXIT.

      *
      *    ****    A M E N D   E X I S T I N G   G U A R A N T E E
      *
       CA000        SECTION 5.
       CA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S04.
             DISPLAY S02.

       CA01.
             ACCEPT S03.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W40-CSHPASS BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 5                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-PW             TO W85-PASS.
           IF NOT(W40-CSHPASS = W85-SUPER(1) OR W85-SUPER(4) OR ")&^%$!")
               MOVE "Invalid password" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CA999.

       CA02.
             PERFORM ERASE-SCREEN.
             DISPLAY S04.
             DISPLAY S05.
           IF WS-USE-ITM = "Y"
               DISPLAY S05-ITM
           ELSE
           IF WS-EXT-STK = "Y"
               DISPLAY S05-EXT.
             INITIALIZE W10-GUARANTEE.

       CA05.
             PERFORM AA300.
           IF WS-USE-ITM = "Y"
               ACCEPT S11-ITM
           ELSE
           IF WS-EXT-STK = "Y"
               ACCEPT S11-EXT
           ELSE
               ACCEPT S11A.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY   MOVE SPACES TO W10-EXT-ITEM
                                GO TO CA999
                 WHEN F1-KEY    PERFORM HELP-ROUTINE
                 WHEN F2-KEY    MOVE "K"    TO WS-ETYPE
                 WHEN F3-KEY    MOVE "I"    TO WS-ETYPE
                 WHEN F4-KEY    MOVE "S"    TO WS-ETYPE
                 WHEN F5-KEY    MOVE "N"    TO WS-ETYPE
                 WHEN OTHER     PERFORM AA900-ALARM
                                GO TO CA05
               END-EVALUATE
               PERFORM AA50
               IF WS-USE-ITM = "Y"
                   DISPLAY S11-ITM
               ELSE
               IF WS-EXT-STK = "Y"
                   DISPLAY S11-EXT
               ELSE
                   DISPLAY S11A
               END-IF
               IF W10-EXT-ITEM = SPACES
                   GO TO CA05.
             PERFORM CLEAR-L50.
           IF W10-EXT-ITEM = SPACE
                GO TO CA999.

       CA10.
             PERFORM AA500.
           IF WS-F-ERROR = 9
               MOVE "Z"              TO WS-ETYPE
               PERFORM AA50
               IF WS-USE-ITM = "Y"
                   DISPLAY S11-ITM
               ELSE
               IF WS-EXT-STK = "Y"
                   DISPLAY S11-EXT
               ELSE
                   DISPLAY S11A
               END-IF
               IF W10-EXT-ITEM = SPACES
                    GO TO CA05
               ELSE
                   GO TO CA10.
             DISPLAY S11B.

       CA15.
             ACCEPT S11C.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO CA15.
             MOVE W10-DATE1          TO W22-DATE.
             PERFORM DATE-CHECK THRU CHECK-DATE-EXIT.
           IF NOT(W22-ERROR = 0)
               EVALUATE W22-ERROR
                 WHEN 1  MOVE "Invalid DATE"  TO WS-ERR-MES
                 WHEN 2  MOVE "Invalid DAY"   TO WS-ERR-MES
                 WHEN 3  MOVE "Invalid MONTH" TO WS-ERR-MES
               END-EVALUATE
               PERFORM ERROR-MESSAGE
               GO TO CA15.
             PERFORM READ-GARTEE-LOCK THRU READ-GARTEE-EXIT.
             MOVE W10-NAME           TO GUA-NAME.
           IF W22-DTE1 NOT = GUA-DATE1
               PERFORM CA20 THRU CA25.
             PERFORM REWRITE-GARTEE-UNLOCK THRU WRITE-GARTEE-EXIT.
             GO TO CA02.

       CA20.
             MOVE W22-DTE2           TO GUA-DATE1.
             ADD GUA-PERIOD          TO W20-MONTH.

       CA25.
           IF W20-MONTH > 12
               SUBTRACT 12           FROM W20-MONTH
               IF W20-YEAR = 99
                   MOVE ZERO         TO W20-YEAR
                   ADD 1             TO W20-CENT
                   GO TO CA25
               ELSE
                   ADD 1             TO W20-YEAR
                   GO TO CA25.
             MOVE W20-DTE            TO GUA-DATE2.

       CA999.
             EXIT.
      *
      *    ****    G U A R A N T E E   E N Q U I R Y
      *
       DA000        SECTION 5.
       DA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S06.
             DISPLAY S05.
    IF WS-USE-ITM = "Y"
        DISPLAY S05-ITM
    ELSE
    IF WS-EXT-STK = "Y"
        DISPLAY S05-EXT.
      INITIALIZE W10-GUARANTEE.

       DA05.
             PERFORM AA300.
    IF WS-USE-ITM = "Y"
        ACCEPT S11-ITM
    ELSE
    IF WS-EXT-STK = "Y"
        ACCEPT S11-EXT
    ELSE
              ACCEPT S11A.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
                     GO TO DA999
                 WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       MOVE "K"  TO WS-ETYPE
   WHEN F3-KEY
                     MOVE "I"    TO WS-ETYPE
   WHEN F4-KEY
                     MOVE "S"    TO WS-ETYPE
   WHEN F5-KEY
                     MOVE "N"    TO WS-ETYPE
                 WHEN OTHER
                     PERFORM AA900-ALARM
                     GO TO DA05
               END-EVALUATE
        PERFORM AA50
        IF WS-USE-ITM = "Y"
     DISPLAY S11-ITM
        ELSE
        IF WS-EXT-STK = "Y"
     DISPLAY S11-EXT
        ELSE
           DISPLAY S11A
        END-IF
        IF W10-EXT-ITEM = SPACES
                   GO TO DA05.
            PERFORM CLEAR-L50.
          IF W10-EXT-ITEM = SPACE
               GO TO DA999.
       DA10.
             PERFORM AA500.
           IF WS-F-ERROR = 9
               MOVE "Z"          TO WS-ETYPE
              PERFORM AA50
        IF WS-USE-ITM = "Y"
     DISPLAY S11-ITM
        ELSE
        IF WS-EXT-STK = "Y"
     DISPLAY S11-EXT
        ELSE
           DISPLAY S11A
        END-IF
              IF W10-EXT-ITEM = SPACES
                   GO TO DA05
               ELSE
                   GO TO DA10.
             DISPLAY S11B.
            MOVE "ENTER to continue"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                     GO TO DA999
               END-EVALUATE.
      GO TO DA00.
       DA999.
             EXIT.
      /
      *    ****    D E L E T E   E X I S T I N G   G U A R A N T E E
      *
       EA000        SECTION 5.
       EA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S07.
             DISPLAY S02.
       EA01.
             ACCEPT S03.
             MOVE 6              TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W40-CSHPASS
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
            MOVE 5   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-PW         TO W85-PASS.
           IF NOT (W40-CSHPASS = W85-SUPER(1)  OR
                                 W85-SUPER(4)  OR
                                 ")&^%$!")
              MOVE "Invalid password"
     TO WS-ERR-MES
        PERFORM ERROR-MESSAGE
               GO TO EA999.
       EA02.
             PERFORM ERASE-SCREEN.
             DISPLAY S07.
             DISPLAY S05.
    IF WS-USE-ITM = "Y"
        DISPLAY S05-ITM
    ELSE
    IF WS-EXT-STK = "Y"
        DISPLAY S05-EXT.
      INITIALIZE W10-GUARANTEE.
       EA05.
             PERFORM AA300.
    IF WS-USE-ITM = "Y"
        ACCEPT S11-ITM
    ELSE
    IF WS-EXT-STK = "Y"
        ACCEPT S11-EXT
    ELSE
              ACCEPT S11A.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
                     GO TO EA999
                 WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       MOVE "K"  TO WS-ETYPE
   WHEN F3-KEY
                     MOVE "I"    TO WS-ETYPE
   WHEN F4-KEY
                     MOVE "S"    TO WS-ETYPE
   WHEN F5-KEY
                     MOVE "N"    TO WS-ETYPE
                 WHEN OTHER
                     PERFORM AA900-ALARM
                     GO TO EA05
               END-EVALUATE
        PERFORM AA50
        IF WS-USE-ITM = "Y"
     DISPLAY S11-ITM
        ELSE
        IF WS-EXT-STK = "Y"
     DISPLAY S11-EXT
        ELSE
           DISPLAY S11A
        END-IF
        IF W10-EXT-ITEM = SPACES
                   GO TO EA05.
            PERFORM CLEAR-L50.
          IF W10-EXT-ITEM = SPACE
               GO TO EA999.
       EA10.
             PERFORM AA500.
           IF WS-F-ERROR = 9
               MOVE "Z"          TO WS-ETYPE
               PERFORM AA50
        IF WS-USE-ITM = "Y"
     DISPLAY S11-ITM
        ELSE
        IF WS-EXT-STK = "Y"
     DISPLAY S11-EXT
        ELSE
           DISPLAY S11A
        END-IF
              IF W10-EXT-ITEM = SPACES
                   GO TO EA05
               ELSE
                   GO TO EA10.
             DISPLAY S11B.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO EA02.
             PERFORM READ-GARTEE-LOCK THRU READ-GARTEE-EXIT.
      PERFORM DELETE-GARTEE-REC THRU WRITE-GARTEE-EXIT.
             GO TO EA02.
       EA999.
             EXIT.
      /
      *    ****    A M E N D   A   S E R I A L   N U M B E R
      *
       FA000        SECTION 5.
       FA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S08.
             DISPLAY S02.
       FA01.
             ACCEPT S03.
             MOVE 6              TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W40-CSHPASS
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
            MOVE 5   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-PW         TO W85-PASS.
           IF NOT (W40-CSHPASS = W85-SUPER(1)  OR
                                 W85-SUPER(4)  OR
                                 ")&^%$!")
              MOVE "Invalid password"
     TO WS-ERR-MES
        PERFORM ERROR-MESSAGE
               GO TO FA999.
       FA02.
             PERFORM ERASE-SCREEN.
      DISPLAY S08.
             DISPLAY S05.
    IF WS-USE-ITM = "Y"
        DISPLAY S05-ITM
    ELSE
    IF WS-EXT-STK = "Y"
        DISPLAY S05-EXT.
      INITIALIZE W10-GUARANTEE.
       FA05.
             PERFORM AA300.
    IF WS-USE-ITM = "Y"
        ACCEPT S11-ITM
    ELSE
    IF WS-EXT-STK = "Y"
        ACCEPT S11-EXT
    ELSE
              ACCEPT S11A.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
                     GO TO FA999
                 WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       MOVE "K"  TO WS-ETYPE
   WHEN F3-KEY
                     MOVE "I"    TO WS-ETYPE
   WHEN F4-KEY
                     MOVE "S"    TO WS-ETYPE
   WHEN F5-KEY
                     MOVE "N"    TO WS-ETYPE
                 WHEN OTHER
                     PERFORM AA900-ALARM
                     GO TO FA05
               END-EVALUATE
        PERFORM AA50
        IF WS-USE-ITM = "Y"
     DISPLAY S11-ITM
        ELSE
        IF WS-EXT-STK = "Y"
     DISPLAY S11-EXT
        ELSE
           DISPLAY S11A
        END-IF
        IF W10-EXT-ITEM = SPACES
                   GO TO FA05.
            PERFORM CLEAR-L50.
          IF W10-EXT-ITEM = SPACE
               GO TO FA999.
       FA10.
             PERFORM AA500.
           IF WS-F-ERROR = 9
               MOVE "Z"          TO WS-ETYPE
              PERFORM AA50
        IF WS-USE-ITM = "Y"
     DISPLAY S11-ITM
        ELSE
        IF WS-EXT-STK = "Y"
     DISPLAY S11-EXT
        ELSE
           DISPLAY S11A
        END-IF
              IF W10-EXT-ITEM = SPACES
                   GO TO FA05
               ELSE
                   GO TO FA10.
             DISPLAY S11B.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO FA02.
             MOVE GUA-RECORD1    TO W30-GUAR.
      DISPLAY "New Serial No.  :" AT 1306.
             MOVE SPACES         TO W10-SERNO.
       FA15.
             ACCEPT W10-SERNO AT 1324
      WITH FOREGROUND-COLOR Brown HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
      PERFORM CLEAR-L50.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                     GO TO FA02
               END-EVALUATE.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO FA15.
             MOVE W10-SERNO      TO GUA-SERIAL.
             PERFORM READ-GARTEE THRU READ-GARTEE-EXIT.
           IF WS-F-ERROR = ZERO
        MOVE "Serial number duplicated"
     TO WS-ERR-MES
        PERFORM ERROR-MESSAGE
               GO TO FA15.
      MOVE ZERO   TO WS-F-ERROR.
             PERFORM WRITE-GARTEE THRU WRITE-GARTEE-EXIT.
      MOVE W30-GUAR  TO GUA-RECORD1.
             PERFORM READ-GARTEE-LOCK THRU READ-GARTEE-EXIT.
             PERFORM DELETE-GARTEE-REC THRU WRITE-GARTEE-EXIT.
             GO TO FA02.
       FA999.
             EXIT.
      /
      *    ****   R E P L A C E   A N   I T E M
      *
       GA000        SECTION 5.
       GA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S09.
             DISPLAY S05.
    IF WS-USE-ITM = "Y"
        DISPLAY S05-ITM
    ELSE
    IF WS-EXT-STK = "Y"
        DISPLAY S05-EXT.
      INITIALIZE W10-GUARANTEE.
       GA05.
             PERFORM AA300.
    IF WS-USE-ITM = "Y"
        ACCEPT S11-ITM
    ELSE
    IF WS-EXT-STK = "Y"
        ACCEPT S11-EXT
    ELSE
              ACCEPT S11A.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
                     GO TO GA999
                 WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       MOVE "K"  TO WS-ETYPE
   WHEN F3-KEY
                     MOVE "I"    TO WS-ETYPE
   WHEN F4-KEY
                     MOVE "S"    TO WS-ETYPE
   WHEN F5-KEY
                     MOVE "N"    TO WS-ETYPE
                 WHEN OTHER
                     PERFORM AA900-ALARM
                     GO TO GA05
               END-EVALUATE
        PERFORM AA50
        IF WS-USE-ITM = "Y"
     DISPLAY S11-ITM
        ELSE
        IF WS-EXT-STK = "Y"
     DISPLAY S11-EXT
        ELSE
           DISPLAY S11A
        END-IF
        IF W10-EXT-ITEM = SPACES
                   GO TO GA05.
            PERFORM CLEAR-L50.
          IF W10-EXT-ITEM = SPACE
               GO TO GA999.
       GA10.
             PERFORM AA500.
           IF WS-F-ERROR = 9
               MOVE "Z"          TO WS-ETYPE
              PERFORM AA50
        IF WS-USE-ITM = "Y"
     DISPLAY S11-ITM
        ELSE
        IF WS-EXT-STK = "Y"
     DISPLAY S11-EXT
        ELSE
           DISPLAY S11A
        END-IF
              IF W10-EXT-ITEM = SPACES
                   GO TO GA05
               ELSE
                   GO TO GA10.
             DISPLAY S11B.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO GA00.
             MOVE GUA-RECORD1    TO W30-GUAR.
             DISPLAY "REPLACEMENT DETAILS" AT 1306
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
      DISPLAY "Item Code        :" AT 1506.
      DISPLAY "Serial Number    :" AT 1606.
      MOVE SPACES  TO W10-EXT-ITEM W10-SERNO.
       GA15.
             ACCEPT S12.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                     GO TO GA00
               END-EVALUATE.
      PERFORM CLEAR-L50.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO GA15.
    IF W10-EXT-ITEM = SPACES OR
       W10-SERNO = SPACES
        MOVE "Data Incomplete"
     TO WS-ERR-MES
        PERFORM ERROR-MESSAGE
        GO TO GA15.
      MOVE W10-EXT-ITEM  TO GUA-EXT-CODE.
             MOVE W10-SERNO      TO GUA-SERIAL.
             PERFORM READ-GARTEE THRU READ-GARTEE-EXIT.
           IF WS-F-ERROR = ZERO
        MOVE "Code & Serial number duplicated"
     TO WS-ERR-MES
        PERFORM ERROR-MESSAGE
               GO TO GA15.
             PERFORM WRITE-GARTEE THRU WRITE-GARTEE-EXIT.
             MOVE W30-GUAR       TO GUA-RECORD1.
             PERFORM READ-GARTEE-LOCK THRU READ-GARTEE-EXIT.
             PERFORM DELETE-GARTEE-REC THRU WRITE-GARTEE-EXIT.
             GO TO GA00.
       GA999.
             EXIT.
      /
      *    ****   D E L E T E   E X P I R E D   R E C O R D S
      *
       HA000        SECTION 5.
       HA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S10.
             DISPLAY "Are you sure Y/N?" AT 0506
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.

       HA05.
            ACCEPT WS-OPTION AT 0524
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "Y" OR "N")
               GO TO HA05.
           IF WS-OPTION = "N"
               GO TO HA999.
             MOVE W12-T-YMD      TO W22-DTE2.
           IF W22-MM2 < 4
                ADD 12           TO W22-MM2
  SUBTRACT 1  FROM W22-CY2.
             SUBTRACT 3          FROM W22-MM2.
             DISPLAY "Reading record :" AT 0706.
      MOVE "!"   TO GUA-EXT-CODE GUA-SERIAL.
             PERFORM START-AT-GARTEE-KEY THRU READ-GARTEE-EXIT.
           IF WS-F-ERROR = 9
               GO TO HA999.

       HA10.
             PERFORM READ-GARTEE-NEXT THRU READ-GARTEE-EXIT.
           IF WS-F-ERROR = 9
               GO TO HA999.
             DISPLAY GUA-KEY AT 0723 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
           IF GUA-DATE2 < W22-DTE2
               PERFORM DELETE-GARTEE-REC THRU WRITE-GARTEE-EXIT.
             GO TO HA10.

       HA999.
             EXIT.
      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT        SECTION 9.
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
      MOVE AFID-PATH  TO W02-PARAM.
      MOVE "GARTEE"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-GARTEE.
      MOVE "NETWORK"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-NETWORK.
            OPEN I-O PARAM.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS NOT = "00"
               MOVE 15           TO WS-F-ERROR
               PERFORM OPEN-ERROR.
             MOVE 3              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-CRDX  TO WS-CARDEX.
      MOVE PAR-CASES  TO WS-USE-CASES.
      MOVE PAR-PACKS  TO WS-USE-PACKS.
      MOVE PAR-USE-ITM  TO WS-USE-ITM.
      MOVE PAR-EXT-STK  TO WS-EXT-STK.
            OPEN I-O NETWORK.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS NOT = "00"
               MOVE 2            TO WS-F-ERROR
               PERFORM OPEN-ERROR.
             OPEN I-O GARTEE.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 9        TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
               MOVE 9            TO WS-F-ERROR
               PERFORM OPEN-ERROR.
             MOVE 1              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
             GO TO ZA999.

       COPY "ZA49.PRO".

       ZA50.
           IF WS-F-ERROR = 9
               MOVE "GUARANTEE file"
                                 TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Press any key to continue" AT 1012.
             ACCEPT WS-OPTION AT 1038 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
       ZA51.
             EXIT PROGRAM.

       ZA200.
       
       COPY "LOCKED.PRO".

       ZA205.
             EXIT PROGRAM.

      ZA999.
             EXIT.

       I-O-ERRORS      SECTION 9.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 9     MOVE W02-GARTEE   TO WS-FILE
                          MOVE GUA-KEY      TO WS-KEYX
               WHEN 15    MOVE WS-PARID     TO WS-FILE
                          MOVE WS-PARKEY    TO WS-KEY
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "GAR001"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
