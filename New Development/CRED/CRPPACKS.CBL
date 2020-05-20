      $set LINKCOUNT"384" GNT"CRPPACKS.GNT"
      ******************************************************************
      *                                                                *
      *  ******  *******  *******  *******    ****    ******  **   **  *
      * **    ** **    ** **    ** **    **  **  **  **    ** **  **   *
      * **       **   **  **    ** **    ** **    ** **       ** **    *
      * **       ******   *******  *******  ******** **       ***      *
      * **       **   **  **       **       **    ** **       ** **    *
      * **    ** **    ** **       **       **    ** **    ** **  **   *
      *  ******  **    ** **       **       **    **  ******  **   **  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *      S T O C K   P A C K S                                     *
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
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     CRPPACKS.
       AUTHOR.        J.W.LEMMON.
       DATE-WRITTEN.   JANUARY 1999.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1999 - 2017
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

       COPY "DEPART.SL".

       COPY "PARAM.SL".

       DATA DIVISION.
       FILE SECTION.

       COPY "DEPART.FDE".

       COPY "PARAM.FDE".

      /
       WORKING-STORAGE SECTION.
       77  WS-COPYRIGHT    PIC X(18)  VALUE
      "aeWlimemnomLalismJ".
       77  WS-P1     PIC  9(02)    COMP-5.
       77  WS-S1     PIC  9(04)    COMP-5.
       77  WS-S2     PIC  9(04)    COMP-5.
       77  WS-S3     PIC  9(04)    COMP-5.
       77  WS-S4     PIC  9(04)    COMP-5.
       77  WS-SUB     PIC  9(04)    COMP-5.
       77  WS-PARKEY     PIC  9(06)    COMP-5.
       77  WS-USE-CASES     PIC  X(01).
       77  WS-USE-PACKS     PIC  X(01).
       77  WS-PACKS     PIC  X(01) VALUE "N".
       77  WS-OPTION     PIC  X(01).
       77  WS-RT-HD     PIC  X(10) VALUE "-Retail   ".
       77  WS-WS-HD     PIC  X(10) VALUE "-Wholesale".
       77  WS-CS-HD     PIC  X(10) VALUE "-Cash sale".
       77  TODAY-DDMMYY     PIC  9(08) COMP-5.

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
           03  CRT-START           PIC  9(02).
           03  CRT-END             PIC  9(02).
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15)           VALUE "CRPPACKS".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40)           VALUE "CREDITOR - STOCK/INVENTORY PACKS".
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

       COPY "W05.VAT".

       COPY "W05.RND".

       01  W20-PRICES.
    03  W20-EXCLUSIVE OCCURS 4.
              05  W20-SL    PIC S9(08)V999   COMP-3.
              05  W20-CSH   PIC S9(08)V999   COMP-3.
              05  W20-WSL   PIC S9(08)V999   COMP-3.

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

       01  LS-PARID.
          03  LS-SYS-ID       PIC  X(03).

       01  LS-USER-ID.
          03  LS-USER.
        05  LS-USE      PIC  9(03).
          03  LS-COMPANY      PIC  X(40).
    03  LS-INSTR        PIC  X(01).
    03  LS-STOCK-CODE.
        05  LS-NEXT-PRG PIC  X(12).
        05  LS-ACNO     PIC  X(06).
    03  LS-DAYS        PIC  9(02) COMP-5.
    03  LS-DEM        PIC  9(02) COMP-5.
          03  LS-USUB        PIC  9(04) COMP-5.
          03  LS-DRAW        PIC  9(02).
          03  LS-CSS        PIC  X(01).
          03  LS-OK        PIC  X(01).
          03  LS-RET        PIC  X(01).
    03  LS-TODAY-DDMMYY PIC  9(08) COMP-5.
    03  LS-CUR-YR       PIC  X(01) COMP-X.
    03  LS-CUR-CEN      PIC  X(01) COMP-X.
    03  LS-PRV-CEN      PIC  X(01) COMP-X.
    03  LS-NXT-CEN      PIC  X(01) COMP-X.
    03  LS-SLS-ORDNO.
      05  LS-SLS-JOBNO  PIC  X(08) VALUE SPACES.
    03  LS-CARDEX       PIC  X(01).
    03  LS-CRDMTH       PIC  X(01).
    03  APAC-SYSTEM     PIC  X(12).
    03  APAC-VERSION    PIC  X(04).
    03  LS-L-OR-N       PIC  X(08) VALUE "SYSTEMS\".

       COPY "W10.CRP".

       01  W10-SLS-ETC.
          03  W10-CCOST  PIC S9(07)V9(6)  COMP-3.
          03  W10-CSELL  PIC S9(07)V9(6)  COMP-3.
    03  W10-RSELL  PIC S9(09)V99.
           03  W10-RSR1 REDEFINES W10-RSELL.
        05  W10-RSR  PIC S9(09).
               05  W10-RSRANDS REDEFINES W10-RSR.
     07  W10-RSR8  PIC S9(08).
     07  W10-RSR9  PIC  9(01).
        05  W10-RSC  PIC  9(02).
           03  W10-RSRC2 REDEFINES W10-RSELL.
        05  FILLER  PIC S9(08).
        05  W10-RSRCC  PIC  9V99.

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

       01  S05.
    03  LINE  7 COLUMN 10 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE
         "зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд".
    03        COLUMN 70 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE "©".
    03  LINE  8 COLUMN 10 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
     VALUE "Ё".
    03        COLUMN 11 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE
           "           P A C K S :   S A L E S   D E T A I L S            ".
    03        COLUMN 70 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE "Ё".
    03  LINE  9 COLUMN 10 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
     VALUE "ц".
    03        COLUMN 11 FOREGROUND-COLOR Red BACKGROUND-COLOR Grey
         VALUE "дддддддддддддддддддддддд╢Pack 1цддддд╢Pack 2цддддд╢Pack 3цддд".
    03        COLUMN 70 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE "╢".
    03  LINE 10 COLUMN 10 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
     VALUE "Ё".
    03        COLUMN 11 FOREGROUND-COLOR Red HIGHLIGHT
     BACKGROUND-COLOR Grey
     VALUE " Priced per        ".
    03        COLUMN 31 FOREGROUND-COLOR Red BACKGROUND-COLOR Grey
         VALUE "                                       ".
    03        COLUMN 70 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE "Ё".
    03  LINE 11 COLUMN 10 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
     VALUE "Ё".
    03        COLUMN 11 FOREGROUND-COLOR Red HIGHLIGHT
     BACKGROUND-COLOR Grey
     VALUE " Units per pack    ".
    03        COLUMN 31 FOREGROUND-COLOR Red BACKGROUND-COLOR Grey
         VALUE "                                       ".
    03        COLUMN 70 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE "Ё".
    03  LINE 12 COLUMN 10 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
     VALUE "Ё".
    03        COLUMN 11 FOREGROUND-COLOR Red HIGHLIGHT
     BACKGROUND-COLOR Grey
     VALUE " Exclusive ".
    03        COLUMN 22 FOREGROUND-COLOR Red HIGHLIGHT
     BACKGROUND-COLOR Grey
     PIC  X(11) FROM WS-RT-HD.
    03        COLUMN 33 FOREGROUND-COLOR Red BACKGROUND-COLOR Grey
         VALUE "                                       ".
    03        COLUMN 72 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE "Ё".
    03  LINE 13 COLUMN 10 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
     VALUE "Ё".
    03        COLUMN 11 FOREGROUND-COLOR Red HIGHLIGHT
     BACKGROUND-COLOR Grey
     VALUE "           ".
    03        COLUMN 22 FOREGROUND-COLOR Red HIGHLIGHT
     BACKGROUND-COLOR Grey
     PIC  X(11) FROM WS-CS-HD.
    03        COLUMN 33 FOREGROUND-COLOR Red BACKGROUND-COLOR Grey
         VALUE "                                       ".
    03        COLUMN 72 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE "Ё".
    03  LINE 14 COLUMN 10 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
     VALUE "Ё".
    03        COLUMN 11 FOREGROUND-COLOR Red HIGHLIGHT
     BACKGROUND-COLOR Grey
     VALUE "           ".
    03        COLUMN 22 FOREGROUND-COLOR Red HIGHLIGHT
     BACKGROUND-COLOR Grey
     PIC  X(11) FROM WS-WS-HD.
    03        COLUMN 33 FOREGROUND-COLOR Red BACKGROUND-COLOR Grey
         VALUE "                                       ".
    03        COLUMN 72 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE "Ё".
    03  LINE 15 COLUMN 10 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
     VALUE "Ё".
    03        COLUMN 11 FOREGROUND-COLOR Red HIGHLIGHT
     BACKGROUND-COLOR Grey
     VALUE " Inclusive ".
    03        COLUMN 22 FOREGROUND-COLOR Red HIGHLIGHT
     BACKGROUND-COLOR Grey
     PIC  X(11) FROM WS-RT-HD.
    03        COLUMN 33 FOREGROUND-COLOR Red BACKGROUND-COLOR Grey
         VALUE "                                       ".
    03        COLUMN 72 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE "Ё".
    03  LINE 16 COLUMN 10 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
     VALUE "Ё".
    03        COLUMN 11 FOREGROUND-COLOR Red HIGHLIGHT
     BACKGROUND-COLOR Grey
     VALUE "           ".
    03        COLUMN 22 FOREGROUND-COLOR Red HIGHLIGHT
     BACKGROUND-COLOR Grey
     PIC  X(11) FROM WS-CS-HD.
    03        COLUMN 33 FOREGROUND-COLOR Red BACKGROUND-COLOR Grey
         VALUE "                                       ".
    03        COLUMN 72 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE "Ё".
    03  LINE 17 COLUMN 10 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
     VALUE "Ё".
    03        COLUMN 11 FOREGROUND-COLOR Red HIGHLIGHT
     BACKGROUND-COLOR Grey
     VALUE "           ".
    03        COLUMN 22 FOREGROUND-COLOR Red HIGHLIGHT
     BACKGROUND-COLOR Grey
     PIC  X(11) FROM WS-WS-HD.
    03        COLUMN 33 FOREGROUND-COLOR Red BACKGROUND-COLOR Grey
         VALUE "                                       ".
    03        COLUMN 72 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE "Ё".
    03  LINE 18 COLUMN 10 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
     VALUE "ю".
    03        COLUMN 11 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE
         "ддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды".

       01  S06.
    02  S06-P1.
    03  LINE 10 COLUMN 33 FOREGROUND-COLOR Black
     PIC X(08) FROM W10-PER-2 AUTO.
    03  LINE 11 COLUMN 33 FOREGROUND-COLOR Black
     PIC ZZZ9.999 FROM W10-UNIT-2 AUTO.
    03  LINE 10 COLUMN 46 FOREGROUND-COLOR Black
     PIC X(08) FROM W10-PER-3 AUTO.
    03  LINE 11 COLUMN 46 FOREGROUND-COLOR Black
     PIC ZZZ9.999 FROM W10-UNIT-3 AUTO.
    03  LINE 10 COLUMN 57 FOREGROUND-COLOR Black
     PIC X(08) FROM W10-PER-4 AUTO.
    03  LINE 11 COLUMN 57 FOREGROUND-COLOR Black
     PIC ZZZ9.999 FROM W10-UNIT-4 AUTO.
    02  S06-P2.
    03  LINE 12 COLUMN 33 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZ9.999 USING W10-SELL-2 AUTO.
    03  LINE 13 COLUMN 33 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZ9.999 USING W10-CASH-2 AUTO.
    03  LINE 14 COLUMN 33 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZ9.999 USING W10-WSALE-2
     AUTO.
    03  LINE 12 COLUMN 46 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZ9.999 USING W10-SELL-3 AUTO.
    03  LINE 13 COLUMN 46 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZ9.999 USING W10-CASH-3 AUTO.
    03  LINE 14 COLUMN 46 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZ9.999 USING W10-WSALE-3
     AUTO.
    03  LINE 12 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZ9.999 USING W10-SELL-4 AUTO.
    03  LINE 13 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZ9.999 USING W10-CASH-4 AUTO.
    03  LINE 14 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZ9.999 USING W10-WSALE-4
     AUTO.
    02  S06-P3.
    03  LINE 15 COLUMN 33 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZZ9.99 USING W10-VSELL-2
     AUTO.
    03  LINE 16 COLUMN 33 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZZ9.99 USING W10-VCASH-2
     AUTO.
    03  LINE 17 COLUMN 33 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZZ9.99 USING W10-VWSALE-2
     AUTO.
    03  LINE 15 COLUMN 46 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZZ9.99 USING W10-VSELL-3
     AUTO.
    03  LINE 16 COLUMN 46 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZZ9.99 USING W10-VCASH-3
     AUTO.
    03  LINE 17 COLUMN 46 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZZ9.99 USING W10-VWSALE-3
     AUTO.
    03  LINE 15 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZZ9.99 USING W10-VSELL-4
     AUTO.
    03  LINE 16 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZZ9.99 USING W10-VCASH-4
     AUTO.
    03  LINE 17 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZZ9.99 USING W10-VWSALE-4
     AUTO.

      *
      *      ******   ******    *****    *****   ******  ******   **   **  ******    ****** 
      *      **   **  **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **   **  **  **   **   **  **       **      **   **  **   **  **  **    **
      *      ******   *****    **   **  **       *****   **   **  **   **  *****     *****
      *      **       **  **   **   **  **       **      **   **  **   **  **  **    **
      *      **       **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **       **   **   *****    *****   ******  ******    *****   **   **   ******
      *
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID W10-STOCK DATA-FILE-IDS.
       AA000-MAIN        SECTION.
       AA000-INIT.
            PERFORM ZA000-INIT.
      PERFORM SAVE-SCREEN-3.
      PERFORM BB000.

       AA05.
      EXIT PROGRAM.

       COPY "AA900.ALM".

      /
      *    ****    READ FILES ROUTINES
      *
       AC000-READ              SECTION.

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "LOCKED.RC2".

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
      *    Ё The option request must be placed in WS-ERR-MES and may   Ё
      *    Ё not exceed 48 characters. The message will be centred in  Ё
      *    Ё the window. An example of a message request follows:      Ё
      *    Ё                                                           Ё
      *    Ё   MOVE "Print transactions (Y/N) [ ]" TO WS-ERR-MES.      Ё
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

      /
      *    R O U N D I N G   O F   S E L L I N G   P R I C E S
      *
      *    This routine will round the selling price as follows:
      *
      *    If the rounding factor is less than 50 cents or greater
      *    than R1.00 but less than five rand, then the selling price
      *    (.CC or R.CC portion) will be rounded to a multiple of the
      *    rounding factor.
      *
      *    If the rounding factor is R1.00, then if the selling price
      *    is greater than 79 cents it will be rounded to the next rand.
      *
      *    If the rounding factor is greater than 49 cents and less than
      *    R1.00 or greater than R4.99 then the selling price (.CC or
      *    R.CC portion) will be changed to the rounding value. If the
      *    rounding portion of the selling price is greater than the
      *    factor then the system will amend these figures and change
      *    the adjoining value.
      *
       CB100         SECTION.
       CB101.
      *
      *    ****    I F  T H E  V A L U E   I S   Z E R O  -  E X I T
      *
    IF W10-RSELL = ZERO
        GO TO CB199.
      *
      *    ROUND TO THE NEXT RAND
      *
           IF W05-ROUND-VAL = 1.00
               IF W10-RSC = ZERO
                   GO TO CB199
               ELSE
      *
      *    SELLING PRICE LESS THAN ONE RAND
      *
               IF W10-RSELL < 1.00
      *
      *    SELLING PRICE IS MORE THAN 79 CENTS - CHANGE TO R1.00
      *
                   IF W10-RSC > 79
                       MOVE 1.00 TO W10-RSELL
                       GO TO CB199
                   ELSE
      *
      *    IF THE SELLING PRICE IS LESS THAN 80 CENTS DO NOT ROUND UP
      *
                       GO TO CB199
      *
      *    SELLING PRICE MORE THAN ONE RAND
      *
               ELSE
      *
      *    CENTS PORTION OF PRICE IS LESS THAN TEN CENTS - DROP CENTS
      *
                   IF W10-RSC < 10
                       MOVE ZERO TO W10-RSC
                       GO TO CB199
                   ELSE
      *
      *    ROUND UP TO NEXT R1.00
      *
                       MOVE ZERO TO W10-RSC
                       ADD 1     TO W10-RSR
                       GO TO CB199.
      *
      *    ROUNDING CENTS ONLY
      *
           IF W05-ROUND-VAL < 1.00
      *
      *    ROUNDING VALUE AND CENTS PORTION ARE EQUAL - EXIT
      *
               IF W10-RSC = W05-CENTS
                   GO TO CB199
               ELSE
      *
      *    ROUNDING VALUE IS 50 CENTS OR MORE
      *
               IF W05-ROUND-VAL > .50
      *
      *    IF CENTS PORTION TO BE ROUNDED IS LESS THAN ROUNDING VALUE
      *
                   IF W10-RSC < W05-CENTS
                       MOVE W05-CENTS
                                 TO W10-RSC
                       GO TO CB199
      *
      *    IF CENTS PORTION TO BE ROUNDED IS MORE THAN ROUNDING VALUE
      *
                   ELSE
                       MOVE W05-CENTS
                                 TO W10-RSC
                       ADD 1     TO W10-RSR
                       GO TO CB199
      *
      *    ROUNDING VALUE IS LESS THAN 50 CENTS
      *
               ELSE
                   GO TO CB120.
      *
      *    IF ROUNDING VALUE EQUALS THE R.CC OF VALUE TO BE ROUNDED - EXIT
      *
           IF W10-RSRCC = W05-ROUND-VAL
               GO TO CB199.
      *
      *    SELLING PRICE IS NOT MORE THAN HALF THE ROUNDING FACTOR - EXIT
      *
           IF W10-RSELL NOT > W05-HLF-VAL
               GO TO CB199.
           IF W05-ROUND-VAL > 5.00
               GO TO CB110.
      *
      *    ROUNDING FACTOR IS LESS THAN FIVE RAND
      *
      *    R.CC OF VALUE TO BE ROUNDED IS LESS THAN ROUNDING FACTOR
      *
           IF W10-RSRCC < W05-ROUND-VAL
               MOVE W05-ROUND-VAL
                                 TO W10-RSRCC
               GO TO CB199.
             MOVE W05-ROUND-VAL  TO W05-CALC-VAL.
       CB105.
           IF W10-RSRCC = W05-CALC-VAL
               GO TO CB199.
           IF W10-RSRCC < W05-CALC-VAL
               MOVE W05-CALC-VAL TO W10-RSRCC
               GO TO CB199.
             ADD W05-ROUND-VAL   TO W05-CALC-VAL.
           IF W05-CALC-VAL < 10.00
               GO TO CB105.
             ADD 1               TO W10-RSR8.
             MOVE W05-RVRCC      TO W10-RSRCC.
             GO TO CB199.
      *
      *    ROUNDING FACTOR GREATER THAN R5.00
      *
      *    SELLING PRICE IS NOT MORE THAN 75% OF ROUNDING FACTOR - EXIT
      *
       CB110.
           IF W10-RSELL NOT > W05-TQTR-VAL
               GO TO CB199.
       CB115.
           IF W10-RSRCC < W05-ROUND-VAL
               MOVE W05-ROUND-VAL
                                 TO W10-RSRCC
               GO TO CB199.
             ADD 1               TO W10-RSR8.
             MOVE W05-ROUND-VAL  TO W10-RSRCC.
             GO TO CB199.
      *
      *    ROUND UP VALUES WHERE ROUNDING FACTOR IS LESS THAN 50 CENTS
      *
       CB120.
    IF W05-CENTS = 2 OR 4 OR 5 OR 10 OR 20 OR 25
        MOVE ZERO  TO WS-SUB
    ELSE
        MOVE W05-CENTS  TO WS-SUB.
       CB125.
           IF W10-RSC = WS-SUB
               GO TO CB199.
           IF W10-RSC < WS-SUB
               MOVE WS-SUB       TO W10-RSC
               GO TO CB199.
             ADD W05-CENTS       TO WS-SUB.
           IF WS-SUB < 100
               GO TO CB125.
             ADD 1               TO W10-RSR.
             SUBTRACT 100 FROM WS-SUB
                                 GIVING W10-RSC.
       CB199.
             EXIT.
      *
      *    ****    C A L C U L A T E   E X C L U S I V E   P R I C E
      *              F R O M   T H E   I N C L U S I V E   P R I C E
      *
       CB200         SECTION.
       CB201.
    IF W10-TAX NOT = ZERO
        COMPUTE W10-SL(WS-P1) ROUNDED = (W10-VSL(WS-P1) /
    (100.00000 + W05-RATE)) * 100.00
        COMPUTE W10-WSL(WS-P1) ROUNDED = (W10-VWSL(WS-P1) /
     (100.00000 + W05-RATE)) * 100.00
        COMPUTE W10-CSH(WS-P1) ROUNDED = (W10-VCSH(WS-P1) /
    (100.00000 + W05-RATE)) * 100.00
           ELSE
        MOVE W10-VWSL(WS-P1)
     TO W10-WSALE
        MOVE W10-VCSH(WS-P1)
     TO W10-CASH
        MOVE W10-VSL(WS-P1)
     TO W10-SELL.
    IF W10-TAX NOT = ZERO
        COMPUTE W10-CHECK ROUNDED
     = W10-SL(WS-P1) + (W10-SL(WS-P1) * W05-RTE)
        IF W10-CHECK > W10-VSL(WS-P1)
     SUBTRACT 0.01 FROM W10-SL(WS-P1)
        ELSE
        IF W10-CHECK < W10-VSL(WS-P1)
     ADD 0.01  TO W10-SL(WS-P1)
        END-IF
        COMPUTE W10-CHECK ROUNDED
     = W10-CSH(WS-P1) + (W10-CSH(WS-P1) * W05-RTE)
        IF W10-CHECK > W10-VCSH(WS-P1)
     SUBTRACT 0.01 FROM W10-CSH(WS-P1)
        ELSE
        IF W10-CHECK < W10-VCSH(WS-P1)
     ADD 0.01  TO W10-CSH(WS-P1)
        END-IF
        COMPUTE W10-CHECK ROUNDED
     = W10-WSL(WS-P1) + (W10-WSL(WS-P1) * W05-RTE)
        IF W10-CHECK > W10-VWSL(WS-P1)
     SUBTRACT 0.01 FROM W10-WSL(WS-P1)
        ELSE
        IF W10-CHECK < W10-VWSL(WS-P1)
     ADD 0.01  TO W10-WSL(WS-P1).

       CB299.
             EXIT.
      *
      *    ****    P A C K S
      *
       BB000        SECTION 5.
       BB00.
      *
      *    ***** SKIP SINGLE PRICES *****
      *
      MOVE 2   TO WS-P1.
      *
      *    ***** STORE PACK PRICES *****
      *
       BB02.
      MOVE W10-SL(WS-P1)  TO W20-SL(WS-P1).
      MOVE W10-CSH(WS-P1) TO W20-CSH(WS-P1).
      MOVE W10-WSL(WS-P1) TO W20-WSL(WS-P1).
    IF WS-P1 < 4
        ADD 1   TO WS-P1
        GO TO BB02.
      MOVE 8   TO SHADE-ROW.
      MOVE 12   TO SHADE-COL.
      MOVE 61   TO SHADE-WIDTH.
      MOVE 11   TO SHADE-LINES.
      DISPLAY S05.
      PERFORM SCREEN-SHADOW.

       BB05.
      *
      *    ***** SKIP SINGLE PRICES *****
      *
      MOVE 2   TO WS-P1.
      *
      *      ***** CHECK FOR END OF PACKS *****
      *
       BB10.
    IF W10-UNT(WS-P1) = ZERO
        GO TO BB15.
            MULTIPLY W10-UNT(WS-P1) BY W10-COST
     GIVING W10-CCOST.
          IF W10-MKUP(WS-P1) NOT > ZERO
              PERFORM BB55.
      *
      *      ***** CALCULATE NEW SELLING PRICES *****
      *
            COMPUTE W10-CSELL ROUNDED = (W10-CCOST +
           ((W10-CCOST * W10-MKUP(WS-P1)) / 100)).
            MOVE W10-CSELL  TO W10-SL(WS-P1).
          IF W10-CMKUP(WS-P1) NOT > ZERO
              PERFORM BB60.
            MOVE W10-CSH(WS-P1) TO W10-CSELL.
            COMPUTE W10-CSELL ROUNDED = (W10-CCOST +
           ((W10-CCOST * W10-CMKUP(WS-P1)) / 100)).
            MOVE W10-CSELL  TO W10-CSH(WS-P1).
          IF W10-WMKUP(WS-P1) NOT > ZERO
              PERFORM BB65.
            MOVE W10-WSL(WS-P1) TO W10-CSELL.
            COMPUTE W10-CSELL ROUNDED = (W10-CCOST +
           ((W10-CCOST * W10-WMKUP(WS-P1)) / 100)).
            MOVE W10-CSELL  TO W10-WSL(WS-P1).
      *
      *    ****    C H E C K   I F   E X C L U S I V E  R O U N D I N G
      *     R E Q U I R E D
      *
           IF W05-ROUND = "E"
        MOVE W10-SL(WS-P1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-SL(WS-P1)
        MOVE W10-WSL(WS-P1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-WSL(WS-P1)
        MOVE W10-CSH(WS-P1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-CSH(WS-P1).
    IF W10-TAX NOT = ZERO
      *
      *    ***** CALCULATE INCLUSIVE PRICES *****
      *
        MOVE W05-VAT(W10-TAX)
     TO W05-RATE
        COMPUTE W10-VSL(WS-P1) ROUNDED = W10-SL(WS-P1) +
     (W10-SL(WS-P1) * W05-RTE)
        COMPUTE W10-VWSL(WS-P1) ROUNDED = W10-WSL(WS-P1) +
     (W10-WSL(WS-P1) * W05-RTE)
        COMPUTE W10-VCSH(WS-P1) ROUNDED = W10-CSH(WS-P1) +
     (W10-CSH(WS-P1) * W05-RTE)
           ELSE
      *
      *    ***** NON TAXABLE ITEMS *****
      *
        MOVE W10-WSL(WS-P1)
     TO W10-VWSL(WS-P1)
        MOVE W10-CSH(WS-P1)
     TO W10-VCSH(WS-P1)
        MOVE W10-SL(WS-P1)
     TO W10-VSL(WS-P1).
      *
      *    ****    C H E C K   I F   I N C L U S I V E  R O U N D I N G
      *     R E Q U I R E D
      *
    IF W05-ROUND = "I"
        MOVE W10-VSL(WS-P1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-VSL(WS-P1)
        MOVE W10-VWSL(WS-P1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-VWSL(WS-P1)
        MOVE W10-VCSH(WS-P1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-VCSH(WS-P1)
               PERFORM CB200.
      MOVE ZERO   TO W05-RATE.
           IF W10-TAX > 0
        MOVE W05-VAT(W10-TAX)
     TO W05-RATE.
      COMPUTE W10-VCSH(WS-P1) ROUNDED
     = W10-CSH(WS-P1) + (W10-CSH(WS-P1) * W05-RTE).
      COMPUTE W10-VWSL(WS-P1) ROUNDED
     = W10-WSL(WS-P1) + (W10-WSL(WS-P1) * W05-RTE).
      COMPUTE W10-VSL(WS-P1) ROUNDED
     = W10-SL(WS-P1) + (W10-SL(WS-P1) * W05-RTE).
      *
      *      ***** CHECK FOR END OF PACKS *****
      *
       BB15.
    IF WS-P1 < 4
        ADD 1   TO WS-P1
        GO TO BB10.
      *
      *    ***** DISPLAY PACK DETAILS *****
      *
      DISPLAY S06.
      *
      *    ***** CHECK IF SELLING PRICES (PACK 2) THE SAME *****
      *
    IF (W10-SL(2) = W20-SL(2)) AND
       (W10-CSH(2) = W20-CSH(2)) AND
       (W10-WSL(2) = W20-WSL(2))
        GO TO BB25.
      MOVE SPACE   TO WS-OPTION.
      *
      *    *** ALLOW USER TO DECIDE IF NEW PRICES ARE TO BE APPLIED ***
      *
       BB20.
      MOVE "Use new selling prices Y/N  [ ]"
     TO WS-ERR-MES.
      MOVE 19   TO SLIN.
      PERFORM OPT-SETUP THRU OPT-EXIT.
    IF NOT (WS-OPTION = "Y" OR "N")
        GO TO BB20.
           IF WS-OPTION = "Y"
      *
      *    ***** USE NEW PRICES *****
      *
              GO TO BB25.
      *
      *    ***** USE OLD PRICES *****
      *
      PERFORM BB75 THRU BB85.
      *
      *    ***** SKIP SINGLE PRICES *****
      *
      MOVE 2   TO WS-P1.

       BB22.
    IF W10-TAX NOT = ZERO
      *
      *    ***** CALCULATE THE INCLUSIVE PRICES *****
      *
        MOVE W05-VAT(W10-TAX)
     TO W05-RATE
      *
      *    ***** RETAIL PRICE (price 1) *****
      *
        COMPUTE W10-VSL(WS-P1) ROUNDED = W10-SL(WS-P1) +
     (W10-SL(WS-P1) * W05-RTE)
      *
      *    ***** WHOLESALE PRICE (price 3) *****
      *
        COMPUTE W10-VWSL(WS-P1) ROUNDED = W10-WSL(WS-P1) +
     (W10-WSL(WS-P1) * W05-RTE)
      *
      *    ***** CASH PRICE (price 2) *****
      *
        COMPUTE W10-VCSH(WS-P1) ROUNDED = W10-CSH(WS-P1) +
     (W10-CSH(WS-P1) * W05-RTE)
           ELSE
      *
      *    ***** NON-TAXABLE *****
      *
        MOVE W10-WSL(WS-P1)
     TO W10-VWSL(WS-P1)
        MOVE W10-CSH(WS-P1)
     TO W10-VCSH(WS-P1)
        MOVE W10-SL(WS-P1)
     TO W10-VSL(WS-P1).
    IF WS-P1 < 4
        ADD 1   TO WS-P1
        GO TO BB22.
      DISPLAY S06.
      *
      *    ***** IS ITEM SUBJECT TO VAT *****
      *
       BB25.
    IF NOT (W10-TAX = ZERO)
        GO TO BB35.
      *
      *    ***** ASK USER IF PRICES ARE TO BE ADJUSTED *****
      *
       BB30.
      MOVE "Adjust selling prices Y/N  [N]"
     TO WS-ERR-MES.
      MOVE "N"   TO WS-OPTION.
      MOVE 19   TO SLIN.
      PERFORM OPT-SETUP THRU OPT-EXIT.
    IF NOT (WS-OPTION = "Y" OR "N")
        GO TO BB30.
    IF WS-OPTION = "N"
        GO TO BB990.
      *
      *    ***** ASK THE USER WHICH PRICES ARE TO BE ADJUSTED *****
      *
       BB35.
      MOVE "Adjust 'E'x/'I'nclusive price or 'N'one  [E]"
     TO WS-ERR-MES.
      MOVE "E"   TO WS-OPTION.
      MOVE 16   TO SLIN.
      PERFORM OPT-SETUP THRU OPT-EXIT.
    IF NOT (WS-OPTION = "E" OR "I" OR "N")
        GO TO BB35.

    IF WS-OPTION = "N"
      *
      *    ***** DO NOT ADJUST THE PRICES *****
      *
        GO TO BB990.
    IF WS-OPTION = "E"
        GO TO BB45.
      *
      *    ***** ALLOW USER TO ADJUST INCLUSIVE PRICES *****
      *
       BB40.
      ACCEPT S06-P3.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
       GO TO BB35
   WHEN OTHER
                     PERFORM AA900-ALARM
       GO TO BB40
        END-EVALUATE.
      *
      *    ***** CALCULATE EXCLUSIVE PRICES *****
      *
      MOVE 2   TO WS-S1.
      MOVE W05-VAT(W10-TAX)
     TO W05-RATE.

       BB42.
      *
      *    ***** RETAIL PRICE (price 1) *****
      *
    IF NOT (W10-VSL(WS-S1) = ZERO)
        COMPUTE W10-SL(WS-S1) ROUNDED = (W10-VSL(WS-S1) /
        (100.000 + W05-RATE)) * 100.000.
      *
      *    ***** CASH PRICE (price 2) *****
      *
    IF NOT (W10-VCSH(WS-S1) = ZERO)
        COMPUTE W10-CSH(WS-S1) ROUNDED = (W10-VCSH(WS-S1) /
        (100.000 + W05-RATE)) * 100.000.
      *
      *    ***** WHOLESALE PRICE (price 3) *****
      *
    IF NOT (W10-VWSL(WS-S1) = ZERO)
        COMPUTE W10-WSL(WS-S1) ROUNDED = (W10-VWSL(WS-S1) /
        (100.000 + W05-RATE)) * 100.000.
    IF WS-S1 < 4
        ADD 1   TO WS-S1
        GO TO BB42.
      MOVE 20   TO SLIN.
      PERFORM CHECK-SETUP THRU CHECK-EXIT.
    IF WS-OPTION = "N"
        GO TO BB40.
      GO TO BB990.
      *
      *    ***** ALLOW USER TO ADJUST EXCLUSIVE PRICES *****
      *
       BB45.
      ACCEPT S06-P2.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
       GO TO BB35
   WHEN OTHER
                     PERFORM AA900-ALARM
       GO TO BB45
        END-EVALUATE.
      *
      *    ***** CALCULATE INCLUSIVE PRICES *****
      *
      MOVE 2   TO WS-S1.
      MOVE W05-VAT(W10-TAX)
     TO W05-RATE.

       BB50.
      *
      *    ***** RETAIL PRICE (price 1) *****
      *
    IF NOT (W10-SL(WS-S1) = ZERO)
        COMPUTE W10-VSL(WS-P1) ROUNDED = W10-SL(WS-P1) +
     (W10-SL(WS-P1) * W05-RTE).
      *
      *    ***** WHOLESALE PRICE (price 3) *****
      *
    IF NOT (W10-WSL(WS-S1) = ZERO)
        COMPUTE W10-VWSL(WS-P1) ROUNDED = W10-WSL(WS-P1) +
     (W10-WSL(WS-P1) * W05-RTE).
      *
      *    ***** CASH PRICE (price 2) *****
      *
    IF NOT (W10-CSH(WS-S1) = ZERO)
        COMPUTE W10-VCSH(WS-P1) ROUNDED = W10-CSH(WS-P1) +
     (W10-CSH(WS-P1) * W05-RTE).
      MOVE 20   TO SLIN.
      PERFORM CHECK-SETUP THRU CHECK-EXIT.
    IF WS-OPTION = "N"
        GO TO BB45.
      GO TO BB990.
      *
      *    ****    D E F A U L T   M A R K U P
      *
       BB55.
    IF (W10-SL(WS-P1) > ZERO) AND (W10-CCOST > ZERO)
        MOVE W10-SL(WS-P1)
     TO W10-CSELL
        COMPUTE W10-MKUP(WS-P1) ROUNDED =
      (((W10-CSELL - W10-CCOST) / W10-CCOST) * 100).

       BB56.
      EXIT.
      *
      *    ****    D E F A U L T   C A S H   M A R K U P
      *
       BB60.
    IF (W10-CSH(WS-P1) > ZERO) AND (W10-CCOST > ZERO)
        MOVE W10-CSH(WS-P1)
     TO W10-CSELL
        COMPUTE W10-CMKUP(WS-P1) ROUNDED =
      (((W10-CSELL - W10-CCOST) / W10-CCOST) * 100).

       BB61.
      EXIT.
      *
      *    ****    D E F A U L T   W H O L E S A L E   M A R K U P
      *
       BB65.
    IF (W10-WSL(WS-P1) > ZERO) AND (W10-CCOST > ZERO)
        MOVE W10-WSL(WS-P1)
     TO W10-CSELL
        COMPUTE W10-WMKUP(WS-P1) ROUNDED =
      (((W10-CSELL - W10-CCOST) / W10-CCOST) * 100).

       BB66.
      EXIT.
      *
      *    ****    R E S T O R E   O L D   P R I C E S
      *
       BB75.
      *
      *    ***** SKIP SINGLE PRICES *****
      *
      MOVE 2   TO WS-P1.

       BB80.
      MOVE W20-SL(WS-P1)  TO W10-SL(WS-P1).
      MOVE W20-CSH(WS-P1) TO W10-CSH(WS-P1).
      MOVE W20-WSL(WS-P1) TO W10-WSL(WS-P1).
    IF WS-P1 < 4
        ADD 1   TO WS-P1
        GO TO BB80.

       BB85.
      EXIT.

       BB990.
      PERFORM RESTORE-SCREEN-3.

       BB999.
      EXIT.

      /
       ZA000-INIT        SECTION 8.
       ZA000-OPEN.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-L-OR-N          TO W02-L-OR-N.
             MOVE WS-SYS-ID          TO W02-SYSID.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Setup various keys and the mouse for use with menus and  Ё
      *    Ё  other required user interface functions and procedures.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

             COPY "FUNCTION.PRO".

            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-DMY  TO TODAY-DDMMYY.
             MOVE 3              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-CASES  TO WS-USE-CASES.
      MOVE PAR-PACKS  TO WS-USE-PACKS.
            MOVE 5   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-ROUND  TO W05-ROUND.
             MOVE PAR-ROUND-VAL  TO W05-ROUND-VAL.
           IF W05-ROUND-VAL > 0.01
               COMPUTE W05-HLF-VAL = W05-ROUND-VAL / 2
           ELSE
               MOVE ZERO         TO W05-HLF-VAL.
           IF W05-HLF-VAL > 0.01
               COMPUTE W05-TQTR-VAL = W05-HLF-VAL * 1.5
           ELSE
               MOVE ZERO         TO W05-TQTR-VAL.
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
            MOVE 8   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
    IF NOT (PAR-WS-HD = X"00000000000000000000")
        MOVE PAR-WS-HD  TO WS-WS-HD.
    IF NOT (PAR-CS-HD = X"00000000000000000000")
        MOVE PAR-CS-HD  TO WS-CS-HD.
    IF NOT (PAR-RT-HD = X"00000000000000000000")
        MOVE PAR-RT-HD  TO WS-RT-HD.

       ZA999-EXIT.
             EXIT.
      /
       ZB000-ERROR        SECTION 8.

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
             PERFORM DISPLAY-FILE-NAME.

       DISPLAY-FILE-NAME.
          IF WS-F-ERROR = 7
              MOVE W02-DEPART  TO WS-FILE
              MOVE ZERO  TO WS-KEY
              MOVE DPT-CODE  TO WS-KEYX
    ELSE
          IF WS-F-ERROR = 15
               MOVE WS-PARID     TO WS-FILE
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
