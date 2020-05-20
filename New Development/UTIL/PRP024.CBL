      $SET LINKCOUNT"320" GNT"PRP024.GNT"
      ******************************************************************
      *                                                                *
      *   *******   *******   *******     ****     ******       **     *
      *   **    **  **    **  **    **   **  **   **    **     ***     *
      *   **    **  **    **  **    **  **    **        **    * **     *
      *   *******   ******    *******   **    **      **     *  **     *
      *   **        **   **   **        **    **    **      *******    *
      *   **        **    **  **         **  **    **           **     *
      *   **        **    **  **          ****    ********     ****    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *    P A R A M E T E R   C A S H  S L I P   C O N T R O L S     *
      *                                                                *
      *       Version 9.04.01 - May 2016                               *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PRP024.
       AUTHOR.         J.W.LEMMON (APAC).
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

       COPY "CONTROL.SL".

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

       COPY "CONTROL.FDE".

       COPY "PARAM.FDE".

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
       77  WS-CHECK                PIC  X(18)    VALUE "aeWlimemnomLalismJ".
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-PARKEY    PIC 9(04)  COMP-5.
       77  WS-NETKEY    PIC 9(04)  COMP-5.
       77  WS-SECKEY    PIC 9(04)  COMP-5.
       77  WS-OPTION    PIC X(01).
       77  WS-INSTR                PIC  X(01).
       77  WS-ERROR    PIC 9(01).
       77  WS-COMP    PIC 9(01).
       77  WS-TYPE    PIC X(01).
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.
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
      *         S =  Clear the screen
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "PRP024".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "UTILITIES - CASH SLIP PRINTER CONTROLS".
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

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W05-PRINT.
           03  W05-ESC             PIC  9(02) COMP-X.
           03  W05-ESCR                       REDEFINES W05-ESC.
               05  W05-E           PIC  X(01).
           03  W05-DBL             PIC  9(02) COMP-X.
           03  W05-DBLR                       REDEFINES W05-DBL.
               05  W05-D           PIC  X(01).
           03  W05-CDBL            PIC  9(02) COMP-X.
           03  W05-CDBLR                      REDEFINES W05-CDBL.
               05  W05-CD          PIC  X(01).
           03  W05-SI              PIC  9(02) COMP-X.
           03  W05-SIR                        REDEFINES W05-SI.
               05  W05-I           PIC  X(01).
           03  W05-CSI             PIC  9(02) COMP-X.
           03  W05-CSIR                       REDEFINES W05-CSI.
               05  W05-CI          PIC  X(01).
           03  W05-SO              PIC  9(02) COMP-X.
           03  W05-SOR                        REDEFINES W05-SO.
               05  W05-O           PIC  X(01).
           03  W05-CSO             PIC  9(02) COMP-X.
           03  W05-CSOR                       REDEFINES W05-CSO.
               05  W05-CO          PIC  X(01).
           03  W05-6LPI            PIC  9(02) COMP-X.
           03  W05-6LPIR                      REDEFINES W05-6LPI.
               05  W05-6           PIC  X(01).
           03  W05-8LPI            PIC  9(02) COMP-X.
           03  W05-8LPIR                      REDEFINES W05-8LPI.
               05  W05-8           PIC  X(01).
           03  W05-NUL             PIC  9(02) COMP-X.
           03  W05-NULR                       REDEFINES W05-NUL.
               05  W05-N           PIC  X(01).
           03  W05-CSHDRW          PIC  9(02).

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

       COPY "W40.WS".

       COPY "FUNCTION.WS".

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

       01  S19.
           03  LINE  3 COLUMN 28 FOREGROUND-COLOR Grey HIGHLIGHT
                                 VALUE "CASH SLIP PRINTER CONTROLS".
          03  LINE  4 COLUMN 12 VALUE "Cash Draw No    ".
    03        COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE "  ".
          03  LINE  6 COLUMN  2 FOREGROUND-COLOR Cyan HIGHLIGHT
                                 VALUE "Decimal values to be entered".
          03  LINE  8 COLUMN 12 VALUE "Escape          ".
    03        COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE "   ".
          03  LINE  9 COLUMN 12 VALUE "Double Print    ".
    03        COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE "   ".
          03  LINE 10 COLUMN 12 VALUE "Cancel Double   ".
    03        COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE "   ".
          03  LINE 11 COLUMN 12 VALUE "Shift In        ".
    03        COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE "   ".
          03  LINE 12 COLUMN 12 VALUE "Cancel Shift In ".
    03        COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE "   ".
          03  LINE 13 COLUMN 12 VALUE "Shift Out       ".
    03        COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE "   ".
          03  LINE 14 COLUMN 12 VALUE "Cancel Shift Out".
    03        COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE "   ".
          03  LINE 15 COLUMN 12 VALUE "6 Lines per inch".
    03        COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE "   ".
          03  LINE 16 COLUMN 12 VALUE "8 Lines per inch".
    03        COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE "   ".
    03  LINE 17 COLUMN 12 VALUE "Lines to advance".
    03        COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE "  ".
    03  LINE 18 COLUMN 12 VALUE "Printer Number  ".
    03        COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE " ".
    03        COLUMN 31 VALUE "1 to 5".
    03  LINE  8 COLUMN 41 VALUE "Draw type       ".
    03        COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE " ".
    03        COLUMN 59 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "N".
    03        COLUMN 60 VALUE "o, ".
    03        COLUMN 63 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "P".
    03        COLUMN 64 VALUE "ort or ".
    03        COLUMN 71 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "K".
    03        COLUMN 72 VALUE "ick".
    03  LINE  9 COLUMN 41 VALUE "COM port number ".
    03        COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE " ".
    03        COLUMN 59 VALUE "1 to 4".
    03  LINE 10 COLUMN 41 VALUE "Customer Dislay ".
    03        COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE " ".
    03        COLUMN 59 VALUE "Y or N".
    03  LINE 11 COLUMN 41 VALUE "COM port number ".
    03        COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE " ".
    03        COLUMN 59 VALUE "1 to 4".
    03  LINE 12 COLUMN 41 VALUE "Digital Scale   ".
    03        COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE " ".
    03        COLUMN 59 VALUE "Y or N".
    03  LINE 13 COLUMN 41 VALUE "COM port number ".
    03        COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE " ".
    03        COLUMN 59 VALUE "1 to 4".
    03  LINE 14 COLUMN 41 VALUE "Scale Type      ".
    03        COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE " ".
    03        COLUMN 59 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "A".
    03        COLUMN 60 VALUE "very/".
    03        COLUMN 65 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "D".
    03        COLUMN 66 VALUE "S640/".
    03        COLUMN 71 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "O".
    03        COLUMN 72 VALUE "ther".
    03  LINE 15 COLUMN 41 VALUE "Kitchen Printer ".
    03        COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE " ".
    03        COLUMN 59 VALUE "Y or N".
    03  LINE 16 COLUMN 41 VALUE "Printer Number  ".
    03        COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE " ".
    03        COLUMN 59 VALUE "1 to 5".
    03  LINE 17 COLUMN 41 VALUE "Qnty: use 3 dec.".
    03        COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red VALUE " ".
    03        COLUMN 59 VALUE "Y or N".


       01  S20.
          03  LINE  8 COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
                                 PIC ZZ9 USING W05-ESC AUTO.
          03  LINE  9 COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
                                 PIC ZZ9 USING W05-DBL AUTO.
          03  LINE 10 COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
                                 PIC ZZ9 USING W05-CDBL AUTO.
          03  LINE 11 COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
                                 PIC ZZ9 USING W05-SI AUTO.
          03  LINE 12 COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
                                 PIC ZZ9 USING W05-CSI AUTO.
          03  LINE 13 COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
                                 PIC ZZ9 USING W05-SO AUTO.
          03  LINE 14 COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
                                 PIC ZZ9 USING W05-CSO AUTO.
          03  LINE 15 COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
                                 PIC ZZ9 USING W05-6LPI AUTO.
          03  LINE 16 COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
                                 PIC ZZ9 USING W05-8LPI AUTO.
          03  LINE 17 COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
                                 PIC Z9 USING PAR-ADVANCE AUTO.
          03  LINE 18 COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
                                 PIC 9 USING PAR-CPRN AUTO.
          03  LINE  8 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
           PIC X USING W05-DRAWER AUTO.
    03  LINE  9 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
                                 PIC 9 USING PAR-PORT AUTO.
          03  LINE 10 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
           PIC X USING PAR-CUSDISP AUTO.
    03  LINE 11 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
     PIC 9 USING PAR-CUSPORT AUTO.
          03  LINE 12 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
           PIC X USING PAR-SCALE AUTO.
    03  LINE 13 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
     PIC 9 USING PAR-SCALPORT AUTO.
    03  LINE 14 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
     PIC X USING PAR-SCALE-TYPE AUTO.
          03  LINE 15 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
           PIC X USING PAR-KTCHPRN AUTO.
    03  LINE 16 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
     PIC 9 USING PAR-KCHPORT AUTO.
    03  LINE 17 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
     PIC X USING PAR-USE-3 AUTO.

       01  S34.
          03  LINE  4 COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red PIC Z9
                                 USING W05-CSHDRW AUTO.

       01  S35.
    03  LINE  4 COLUMN 38 BACKGROUND-COLOR Red
     FOREGROUND-COLOR Grey HIGHLIGHT
     PIC 999 USING PAR-KC1 AUTO.
    03        COLUMN 42 BACKGROUND-COLOR Red
     FOREGROUND-COLOR Grey HIGHLIGHT
     PIC 999 USING PAR-KC2 AUTO.
    03        COLUMN 46 BACKGROUND-COLOR Red
     FOREGROUND-COLOR Grey HIGHLIGHT
     PIC 999 USING PAR-KC3 AUTO.
    03        COLUMN 50 BACKGROUND-COLOR Red
     FOREGROUND-COLOR Grey HIGHLIGHT
     PIC 999 USING PAR-KC4 AUTO.
    03        COLUMN 54 BACKGROUND-COLOR Red
     FOREGROUND-COLOR Grey HIGHLIGHT
     PIC 999 USING PAR-KC5 AUTO.

       01  S-HELP.
    03  LINE  4 COLUMN 41 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE
        "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
    03  LINE  5 COLUMN 41 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 42 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE
        "        CASH DRAWER CONTROLS        ".
    03        COLUMN 78 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE  6 COLUMN 41 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 42 FOREGROUND-COLOR Blue
     BACKGROUND-COLOR Grey VALUE
        "Drawer no: In the range 01 to 40    ".
    03        COLUMN 78 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE  7 COLUMN 41 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 42 FOREGROUND-COLOR Blue
     BACKGROUND-COLOR Grey VALUE
        "See the printer reference MANUAL for".
    03        COLUMN 78 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE  8 COLUMN 41 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 42 FOREGROUND-COLOR Blue
     BACKGROUND-COLOR Grey VALUE
        "details of the control codes reques-".
    03        COLUMN 78 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE  9 COLUMN 41 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 42 FOREGROUND-COLOR Blue
     BACKGROUND-COLOR Grey VALUE
        "ed. Please note that the decimal    ".
    03        COLUMN 78 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE 10 COLUMN 41 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 42 FOREGROUND-COLOR Blue
     BACKGROUND-COLOR Grey VALUE
        "value of the code must be keyed in. ".
    03        COLUMN 78 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE 11 COLUMN 41 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 42 FOREGROUND-COLOR Blue
     BACKGROUND-COLOR Grey VALUE
        "Codes are usually specified using   ".
    03        COLUMN 78 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE 12 COLUMN 41 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 42 FOREGROUND-COLOR Blue
     BACKGROUND-COLOR Grey VALUE
        "three formats:-                     ".
    03        COLUMN 78 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE 13 COLUMN 41 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 42 FOREGROUND-COLOR Blue
     BACKGROUND-COLOR Grey VALUE
        "ASCII/GRAPHIC, HEXIDECIMAL & DECIMAL".
    03        COLUMN 78 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE 14 COLUMN 41 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 42 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE
        "EXAMPLE:                            ".
    03        COLUMN 78 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE 15 COLUMN 41 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 42 FOREGROUND-COLOR Blue
     BACKGROUND-COLOR Grey VALUE
        " ASCII/GRAPHIC - ESC SO  SI  DC2 :  ".
    03        COLUMN 78 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE 16 COLUMN 41 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 42 FOREGROUND-COLOR Blue
     BACKGROUND-COLOR Grey VALUE
        " HEXIDECIMAL   - 1B  0E  0F  12  3A ".
    03        COLUMN 78 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE 17 COLUMN 41 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 42 FOREGROUND-COLOR Blue
     BACKGROUND-COLOR Grey VALUE
        " DECIMAL       - 027 014 015 018 058".
    03        COLUMN 78 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE 18 COLUMN 41 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 42 FOREGROUND-COLOR Blue
     BACKGROUND-COLOR Grey VALUE
        "Lines to advance: Allow for tear off".
    03        COLUMN 78 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE 19 COLUMN 41 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 42 FOREGROUND-COLOR Blue
     BACKGROUND-COLOR Grey VALUE
        "Draw type: Most drawers are type F  ".
    03        COLUMN 78 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE 20 COLUMN 41 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 42 FOREGROUND-COLOR Blue
     BACKGROUND-COLOR Grey VALUE
        "           Use P if no drawer       ".
    03        COLUMN 78 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 78 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE 21 COLUMN 41 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 42 FOREGROUND-COLOR Blue
     BACKGROUND-COLOR Grey VALUE
        "COM port: Port used for cash drawer ".
    03        COLUMN 78 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE 22 COLUMN 41 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE
        "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

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
          IF LS0-PALEV < 3
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM YA000.
       AA49.
             EXIT PROGRAM.

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

       COPY "LOCKED.RC2".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                                                           ³
      *    ³           R E A D   F I L E S   R O U T I N E S           ³
      *    ³                                                           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD2".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                                                           ³
      *    ³R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S³
      *    ³                                                           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AD000-WRITE             SECTION.

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

      /
      *    *****    CASH SLIP / DRAWER CONTROLS    *****
      *
       YA000         SECTION 5.
       YA310-PRN.
             PERFORM ERASE-SCREEN.
             DISPLAY S19.
      DISPLAY "F1" AT 2502 WITH BACKGROUND-COLOR Cyan
           FOREGROUND-COLOR Brown HIGHLIGHT
       "=Help" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
            MOVE ZERO TO W05-CSHDRW.
       YA315.
             ACCEPT S34.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN NOT F1-KEY GO TO YA318
        END-EVALUATE
    ELSE
        GO TO YA318.
      PERFORM SAVE-SCREEN.
      MOVE 5   TO SHADE-ROW.
      MOVE 43   TO SHADE-COL.
      MOVE 36   TO SHADE-WIDTH.
      MOVE 18   TO SHADE-LINES.
            DISPLAY S-HELP.
      PERFORM SCREEN-SHADOW.
            PERFORM CLEAR-L50.
            DISPLAY "ENTER to continue" AT 2524
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT WS-OPTION AT 2542
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      PERFORM RESTORE-SCREEN.
      GO TO YA315.
       YA318.
      PERFORM CLEAR-L50.
          IF W05-CSHDRW = ZERO
              GO TO YA999.
           IF W05-CSHDRW > 40
              MOVE "Invalid Cash Draw Number" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO YA315.
      PERFORM CLEAR-L50.
            ADD 150 W05-CSHDRW  GIVING WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
             MOVE ZERO           TO W05-ESC
                                    W05-DBL
                                    W05-CDBL
                                    W05-SI
                                    W05-CSI
                                    W05-SO
                                    W05-CSO
                                    W05-6LPI
                                    W05-8LPI
                                    W05-NUL.
             MOVE PAR-ESC        TO W05-E.
             MOVE PAR-DBL        TO W05-D.
             MOVE PAR-CDBL       TO W05-CD.
             MOVE PAR-SI         TO W05-I.
             MOVE PAR-CSI        TO W05-CI.
             MOVE PAR-SO         TO W05-O.
             MOVE PAR-CSO        TO W05-CO.
             MOVE PAR-6LPI       TO W05-6.
             MOVE PAR-8LPI       TO W05-8.
           IF NOT(PAR-PORT = 1 OR 2 OR 3 OR 4)
               MOVE 1            TO PAR-PORT.
    IF PAR-DRAWER = "F"
        MOVE "P"   TO W05-DRAWER
    ELSE
    IF PAR-DRAWER = "P"
        MOVE "N"   TO W05-DRAWER
    ELSE
        MOVE PAR-DRAWER  TO W05-DRAWER.
      DISPLAY "F1" AT 2502 WITH BACKGROUND-COLOR Cyan
           FOREGROUND-COLOR Brown HIGHLIGHT
       "=Help" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
       YA320.
            ACCEPT S20.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN NOT F1-KEY GO TO YA325
        END-EVALUATE
    ELSE
        GO TO YA325.
      PERFORM SAVE-SCREEN.
      MOVE 5   TO SHADE-ROW.
      MOVE 43   TO SHADE-COL.
      MOVE 36   TO SHADE-WIDTH.
      MOVE 18   TO SHADE-LINES.
            DISPLAY S-HELP.
      PERFORM SCREEN-SHADOW.
            PERFORM CLEAR-L50.
            DISPLAY "ENTER to continue" AT 2524
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT WS-OPTION AT 2542
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      PERFORM RESTORE-SCREEN.
      GO TO YA320.
       YA325.
      PERFORM CLEAR-L50.
          IF 255 < W05-ESC OR W05-DBL OR W05-CDBL OR W05-SI OR
                    W05-CSI OR W05-SO OR W05-CSO OR W05-6LPI OR W05-8LPI
              MOVE "Decimal values may not be > 255" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO YA320.
    IF PAR-ADVANCE > 15
        MOVE 15   TO PAR-ADVANCE.
           IF PAR-CPRN = 0
               MOVE 1            TO PAR-CPRN
           ELSE
           IF PAR-CPRN NOT = 9
               IF (PAR-CPRN < 1 OR > 5)
     MOVE "Printer number MUST be in the range 1 to 5" TO WS-ERR-MES
     PERFORM ERROR-LENGTH THRU ERROR-EXIT
     GO TO YA320.
      CALL "CBL_TOUPPER" USING W05-DRAWER
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT(W05-DRAWER = "N" OR "P" OR "K")
        MOVE "Drawer type must be K, N or P" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO YA320.
           IF NOT(PAR-PORT = 1 OR 2 OR 3 OR 4)
        MOVE "COM port must be in the range 1 to 4" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO YA320.
      CALL "CBL_TOUPPER" USING PAR-CUSDISP
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT(PAR-CUSDISP = "Y" OR "N")
        MOVE "Customer Display must be Y or N" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO YA320.
    IF NOT(PAR-CUSPORT = 1 OR 2 OR 3 OR 4)
        MOVE "DISP port must be in the range 1 to 4" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO YA320.
      CALL "CBL_TOUPPER" USING PAR-SCALE
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT(PAR-SCALE = "Y" OR "N")
        MOVE "Digital Scale must be Y or N" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO YA320.
    IF NOT(PAR-SCALPORT = 1 OR 2 OR 3 OR 4)
        MOVE "SCALE port must be in the range 1 to 4" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO YA320.
      CALL "CBL_TOUPPER" USING PAR-SCALE-TYPE
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT(PAR-SCALE-TYPE = "A" OR "D" OR "O")
        MOVE "Scale type must be A, D, or O" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO YA320.
      CALL "CBL_TOUPPER" USING PAR-KTCHPRN
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT(PAR-KTCHPRN = "Y" OR "N")
        MOVE "Kitchen printer must be Y or N" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO YA320.
    IF PAR-KCHPORT = 0
        MOVE 1   TO PAR-KCHPORT
           ELSE
    IF PAR-KCHPORT NOT = 9
        IF (PAR-KCHPORT < 1 OR > 5)
     MOVE "Kitch.Prn no. MUST be in the range 1 to 5" TO WS-ERR-MES
     PERFORM ERROR-LENGTH THRU ERROR-EXIT
     GO TO YA320.
      CALL "CBL_TOUPPER" USING PAR-USE-3
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT(PAR-USE-3 = "Y" OR "N")
        MOVE "Use 3 decimals must be Y or N" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO YA320.
            PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO YA320.
             MOVE W05-E          TO PAR-ESC.
             MOVE W05-D          TO PAR-DBL.
             MOVE W05-CD         TO PAR-CDBL.
             MOVE W05-I          TO PAR-SI.
             MOVE W05-CI         TO PAR-CSI.
             MOVE W05-O          TO PAR-SO.
             MOVE W05-CO         TO PAR-CSO.
             MOVE W05-6          TO PAR-6LPI.
             MOVE W05-8          TO PAR-8LPI.
             MOVE W05-N          TO PAR-NUL.
    IF PAR-DRAWER = "K"
              PERFORM ERASE-SCREEN
        DISPLAY "PRINTER 'KICK' DRAWER CONTROLS" AT 0226
   WITH FOREGROUND-COLOR Grey HIGHLIGHT
        DISPLAY "Keyin control characters:" AT 0412
        ACCEPT S35.
    IF W05-DRAWER = "N"
        MOVE "P"   TO PAR-DRAWER
    ELSE
    IF W05-DRAWER = "P"
        MOVE "F"   TO PAR-DRAWER
    ELSE
        MOVE W05-DRAWER  TO PAR-DRAWER.
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
    IF NOT(PAR-CUSDISP = "Y")
        GO TO YA310-PRN.
      MOVE 7   TO WS-PARKEY.
            PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
             PERFORM ERASE-SCREEN.
      DISPLAY "CUSTOMER DISPLAY HEADING" AT 0329
        WITH FOREGROUND-COLOR Grey HIGHLIGHT.
      DISPLAY "Line 1" AT 0512.
      DISPLAY "Line 2" AT 0612.
       YA330.
      ACCEPT PAR-DSP-L1 AT 0519
      WITH BACKGROUND-COLOR Red
    FOREGROUND-COLOR Grey HIGHLIGHT UPDATE AUTO.
      ACCEPT PAR-DSP-L2 AT 0619
      WITH BACKGROUND-COLOR Red
    FOREGROUND-COLOR Grey HIGHLIGHT UPDATE AUTO.
            PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
              GO TO YA330.
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
            GO TO YA310-PRN.
       YA999.
      EXIT.
      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT        SECTION 8.
       ZA000-OPEN.
             MOVE LS-COMPANY         TO CRT-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W12-TODAY.
             MOVE LS-USUB            TO WS-USUB.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

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
               WHEN 15         MOVE WS-PARID      TO WS-FILE
                               MOVE WS-PARKEY     TO WS-KEY
               WHEN OTHER      MOVE "ERROR"       TO WS-FILE
                               MOVE "PRP024"      TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
