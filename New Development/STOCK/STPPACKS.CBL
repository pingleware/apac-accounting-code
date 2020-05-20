      $set LINKCOUNT"468"
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
      *  ******  ******** *******  *******    ****    ******  **   **  *
      * **    **    **   **     ** **    **  **  **  **    ** **  **   *
      * **          **   **     ** **    ** **    ** **       ** **    *
      *  ******     **   *******   *******  ******** **       ***      *
      *       **    **   **        **       **    ** **       ** **    *
      * **    **    **   **        **       **    ** **    ** **  **   *
      *  ******     **   **        **       **    **  ******  **   **  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S T O C K   P A C K S   A N D   C A S E S                  *
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
       PROGRAM-ID.     STPPACKS.
       AUTHOR.         J.W.LEMMON.
       DATE-WRITTEN.   JULY 1998.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1998 - 2018
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

       COPY "DEPART.FDE".

       COPY "PARAM.FDE".

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
       77  WS-P1                   PIC  9(02)    COMP-5.
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-SUB                  PIC  9(04)    COMP-5.
       77  WS-PARKEY               PIC  9(06)    COMP-5.
       77  WS-USE-CASES            PIC  X(01).
       77  WS-USE-PACKS            PIC  X(01).
       77  WS-PACKS                PIC  X(01)           VALUE "N".
       77  WS-OPTION               PIC  X(01).
       77  WS-RT-HD                PIC  X(10)           VALUE "-Retail   ".
       77  WS-WS-HD                PIC  X(10)           VALUE "-Wholesale".
       77  WS-CS-HD                PIC  X(10)           VALUE "-Cash sale".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.

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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STPPACKS".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY -PACKS AND CASES".
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
           03  WS-SYS-ID           PIC  X(03).

       COPY "W05.VAT".

       COPY "W05.RND".

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

       COPY "W10.STK".

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

       01  S01.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey.
               05  LINE 10 COLUMN 30 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"  FOREGROUND-COLOR Black. 
               05          COLUMN 50 VALUE                     "¿" HIGHLIGHT.
               05  LINE 11 COLUMN 30 VALUE "³"                     FOREGROUND-COLOR Black.
               05          COLUMN 31 VALUE  " Use cases ("         FOREGROUND-COLOR Red.
               05          COLUMN 43 VALUE              "Y"        FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 44 VALUE               "/"       FOREGROUND-COLOR Red.
               05          COLUMN 45 VALUE                "N"      FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 46 VALUE                 ")   "  FOREGROUND-COLOR Red.
               05          COLUMN 50 VALUE                     "³" HIGHLIGHT.
               05  LINE 12 COLUMN 30 VALUE "À"                     FOREGROUND-COLOR Black.
               05          COLUMN 31 VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" HIGHLIGHT.

       01  S02.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey.
               05  LINE 10 COLUMN 26 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"  FOREGROUND-COLOR Black.
               05          COLUMN 54 VALUE                             "¿" HIGHLIGHT.
               05  LINE 11 COLUMN 26 VALUE "³"                             FOREGROUND-COLOR Black.
               05          COLUMN 27 VALUE  " Number of Cases           "  FOREGROUND-COLOR Red.
               05          COLUMN 54 VALUE                             "³" HIGHLIGHT.
               05  LINE 12 COLUMN 26 VALUE "À"                             FOREGROUND-COLOR Black.
               05          COLUMN 27 VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" HIGHLIGHT.

       01  S03.
           03  LINE 11 COLUMN 44 PIC ZZZZZZZZ9 USING W10-CASES BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT AUTO.

       01  S04.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey.
               05  LINE 10 COLUMN 30 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"  FOREGROUND-COLOR Black.
               05          COLUMN 50 VALUE                     "¿" HIGHLIGHT.
               05  LINE 11 COLUMN 30 VALUE "³"                     FOREGROUND-COLOR Black.
               05          COLUMN 31 VALUE  " Use packs ("         FOREGROUND-COLOR Red.
               05          COLUMN 43 VALUE              "Y"        FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 44 VALUE               "/"       FOREGROUND-COLOR Red.
               05          COLUMN 45 VALUE                "N"      FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 46 VALUE                 ")   "  FOREGROUND-COLOR Red.
               05          COLUMN 50 VALUE                     "³" HIGHLIGHT.
               05  LINE 12 COLUMN 30 VALUE "À"                     FOREGROUND-COLOR Black.
               05          COLUMN 31 VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" HIGHLIGHT.

       01  S05.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey.
               05  LINE  7 COLUMN 11 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"  FOREGROUND-COLOR Black.
               05          COLUMN 70 VALUE                                                            "¿" HIGHLIGHT.
               05  LINE  8 COLUMN 11 VALUE "³"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  "          P A C K S :   S A L E S   D E T A I L S         "  HIGHLIGHT.
               05          COLUMN 70 VALUE                                                            "³" HIGHLIGHT.
               05  LINE  9 COLUMN 11 VALUE "Ã"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´Pack 1ÃÄÄÄÄÄ´Pack 2ÃÄÄÄÄÄ´Pack 3ÃÄÄÄ"  FOREGROUND-COLOR Red.
               05          COLUMN 70 VALUE                                                            "´" HIGHLIGHT.
               05  LINE 10 COLUMN 11 VALUE "³"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  " Priced per        "                                         FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 31 VALUE                     "                                       "  FOREGROUND-COLOR Red.
               05          COLUMN 70 VALUE                                                            "³" HIGHLIGHT.
               05  LINE 11 COLUMN 11 VALUE "³"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  " Units per pack    "                                         FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 31 VALUE                     "                                       "  FOREGROUND-COLOR Red.
               05          COLUMN 70 VALUE                                                            "³" HIGHLIGHT.
               05  LINE 12 COLUMN 11 VALUE "³"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  " Markup "                                                    FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 20 PIC  X(11) FROM WS-RT-HD                                             FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 31 VALUE                     "                                       "  FOREGROUND-COLOR Red.
               05          COLUMN 70 VALUE                                                            "³" HIGHLIGHT.
               05  LINE 13 COLUMN 11 VALUE "³"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  "        "                                                    FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 20 PIC  X(11) FROM WS-CS-HD                                             FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 31 VALUE                     "                                       "  FOREGROUND-COLOR Red.
               05          COLUMN 70 VALUE                                                            "³" HIGHLIGHT.
               05  LINE 14 COLUMN 11 VALUE "³"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  "        "                                                    FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 20 PIC  X(11) FROM WS-WS-HD                                             FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 31 VALUE                     "                                       "  FOREGROUND-COLOR Red.
               05          COLUMN 70 VALUE                                                            "³" HIGHLIGHT.
               05  LINE 15 COLUMN 11 VALUE "³"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  " Price  "                                                    FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 20 PIC  X(11) FROM WS-RT-HD                                             FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 31 VALUE                     "                                       "  FOREGROUND-COLOR Red.
               05          COLUMN 70 VALUE                                                            "³" HIGHLIGHT.
               05  LINE 16 COLUMN 11 VALUE "³"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  "        "                                                    FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 20 PIC  X(11) FROM WS-CS-HD                                             FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 31 VALUE                     "                                       "  FOREGROUND-COLOR Red.
               05          COLUMN 70 VALUE                                                            "³" HIGHLIGHT.
               05  LINE 17 COLUMN 11 VALUE "³"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  "        "                                                    FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 20 PIC  X(11) FROM WS-WS-HD                                             FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 31 VALUE                     "                                       "  FOREGROUND-COLOR Red.
               05          COLUMN 70 VALUE                                                            "³" HIGHLIGHT.
               05  LINE 18 COLUMN 11 VALUE "À"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" HIGHLIGHT.

       01  S06.
           02  S06-P1 BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 10 COLUMN 31 PIC X(08)        USING W10-PER-2 AUTO.
               05  LINE 11 COLUMN 31 PIC ZZZ9.999     USING W10-UNIT-2 AUTO.
               05  LINE 12 COLUMN 31 PIC ZZ9.9999     USING W10-MARKUP-2 AUTO.
               05  LINE 13 COLUMN 31 PIC ZZ9.9999     USING W10-CMARKUP-2 AUTO.
               05  LINE 14 COLUMN 31 PIC ZZ9.9999     USING W10-WMARKUP-2 AUTO.
               05  LINE 15 COLUMN 31 PIC ZZZZZZZ9.999 USING W10-SELL-2 AUTO.
               05  LINE 16 COLUMN 31 PIC ZZZZZZZ9.999 USING W10-CASH-2 AUTO.
               05  LINE 17 COLUMN 31 PIC ZZZZZZZ9.999 USING W10-WSALE-2 AUTO.

           02  S06-P2 BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               03  LINE 10 COLUMN 44 PIC X(08)        USING W10-PER-3 AUTO.
               03  LINE 11 COLUMN 44 PIC ZZZ9.999     USING W10-UNIT-3 AUTO.
               03  LINE 12 COLUMN 44 PIC ZZ9.9999     USING W10-MARKUP-3 AUTO.
               03  LINE 13 COLUMN 44 PIC ZZ9.9999     USING W10-CMARKUP-3 AUTO.
               03  LINE 14 COLUMN 44 PIC ZZ9.9999     USING W10-WMARKUP-3 AUTO.
               03  LINE 15 COLUMN 44 PIC ZZZZZZZ9.999 USING W10-SELL-3 AUTO.
               03  LINE 16 COLUMN 44 PIC ZZZZZZZ9.999 USING W10-CASH-3 AUTO.
               03  LINE 17 COLUMN 44 PIC ZZZZZZZ9.999 USING W10-WSALE-3 AUTO.

           02  S06-P3 BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               03  LINE 10 COLUMN 57 PIC X(08)        USING W10-PER-4 AUTO.
               03  LINE 11 COLUMN 57 PIC ZZZ9.999     USING W10-UNIT-4 AUTO.
               03  LINE 12 COLUMN 57 PIC ZZ9.9999     USING W10-MARKUP-4 AUTO.
               03  LINE 13 COLUMN 57 PIC ZZ9.9999     USING W10-CMARKUP-4 AUTO.
               03  LINE 14 COLUMN 57 PIC ZZ9.9999     USING W10-WMARKUP-4 AUTO.
               03  LINE 15 COLUMN 57 PIC ZZZZZZZ9.999 USING W10-SELL-4 AUTO.
               03  LINE 16 COLUMN 57 PIC ZZZZZZZ9.999 USING W10-CASH-4 AUTO.
               03  LINE 17 COLUMN 57 PIC ZZZZZZZ9.999 USING W10-WSALE-4 AUTO.

      *
      *      ******   ******    *****    *****   ******  ******   **   **  ******    ****** 
      *      **   **  **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **   **  **  **   **   **  **       **      **   **  **   **  **  **    **
      *      ******   *****    **   **  **       *****   **   **  **   **  *****     *****
      *      **       **  **   **   **  **       **      **   **  **   **  **  **    **
      *      **       **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **       **   **   *****    *****   ******  ******    *****   **   **   ******
      *
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS W10-STOCK.
       AA000-MAIN        SECTION.
       AA000-INIT.
             PERFORM ZA000-INIT.
             PERFORM SAVE-SCREEN-3.
           IF WS-USE-CASES = "Y"
               PERFORM BA000.
           IF WS-USE-PACKS = "Y"
               PERFORM BB000.

       AA05.
             EXIT PROGRAM.

       COPY "AA900.ALM".

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

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000-READ              SECTION.

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "STOCK.RD".

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

      *
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
                       MOVE 1.00     TO W10-RSELL
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
                       MOVE ZERO     TO W10-RSC
                       GO TO CB199
                   ELSE
      *
      *    ROUND UP TO NEXT R1.00
      *
                       MOVE ZERO     TO W10-RSC
                       ADD 1         TO W10-RSR
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
                       MOVE W05-CENTS TO W10-RSC
                       GO TO CB199
      *
      *    IF CENTS PORTION TO BE ROUNDED IS MORE THAN ROUNDING VALUE
      *
                   ELSE
                       MOVE W05-CENTS TO W10-RSC
                       ADD 1          TO W10-RSR
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
               MOVE W05-ROUND-VAL    TO W10-RSRCC
               GO TO CB199.
             MOVE W05-ROUND-VAL      TO W05-CALC-VAL.
       CB105.
           IF W10-RSRCC = W05-CALC-VAL
               GO TO CB199.
           IF W10-RSRCC < W05-CALC-VAL
               MOVE W05-CALC-VAL     TO W10-RSRCC
               GO TO CB199.
             ADD W05-ROUND-VAL       TO W05-CALC-VAL.
           IF W05-CALC-VAL < 10.00
               GO TO CB105.
             ADD 1                   TO W10-RSR8.
             MOVE W05-RVRCC          TO W10-RSRCC.
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
               MOVE W05-ROUND-VAL    TO W10-RSRCC
               GO TO CB199.
             ADD 1                   TO W10-RSR8.
             MOVE W05-ROUND-VAL      TO W10-RSRCC.
             GO TO CB199.
      *
      *    ROUND UP VALUES WHERE ROUNDING FACTOR IS LESS THAN 50 CENTS
      *
       CB120.
           IF W05-CENTS = 2 OR 4 OR 5 OR 10 OR 20 OR 25
               MOVE ZERO             TO WS-SUB
           ELSE
               MOVE W05-CENTS        TO WS-SUB.

       CB125.
           IF W10-RSC = WS-SUB
               GO TO CB199.
           IF W10-RSC < WS-SUB
               MOVE WS-SUB           TO W10-RSC
               GO TO CB199.
             ADD W05-CENTS           TO WS-SUB.
           IF WS-SUB < 100
               GO TO CB125.
             ADD 1                   TO W10-RSR.
             SUBTRACT 100 FROM WS-SUB GIVING W10-RSC.

       CB199.
             EXIT.
      *
      *    ****    C A L C U L A T E   E X C L U S I V E   P R I C E
      *              F R O M   T H E   I N C L U S I V E   P R I C E
      *
       CB200         SECTION.
       CB201.
           IF W10-TAX NOT = ZERO
               COMPUTE W10-SL(WS-P1)  ROUNDED = (W10-VSL(WS-P1)  / (100.00000 + W05-RATE)) * 100.00
               COMPUTE W10-WSL(WS-P1) ROUNDED = (W10-VWSL(WS-P1) / (100.00000 + W05-RATE)) * 100.00
               COMPUTE W10-CSH(WS-P1) ROUNDED = (W10-VCSH(WS-P1) / (100.00000 + W05-RATE)) * 100.00
           ELSE
               MOVE W10-VWSL(WS-P1)  TO W10-WSALE
               MOVE W10-VCSH(WS-P1)  TO W10-CASH
               MOVE W10-VSL(WS-P1)   TO W10-SELL.
           IF W10-TAX NOT = ZERO
               COMPUTE W10-CHECK ROUNDED = W10-SL(WS-P1) + (W10-SL(WS-P1) * W05-RTE)
               IF W10-CHECK > W10-VSL(WS-P1)
                   SUBTRACT 0.01     FROM W10-SL(WS-P1)
               ELSE
               IF W10-CHECK < W10-VSL(WS-P1)
                   ADD 0.01          TO W10-SL(WS-P1)
               END-IF
               COMPUTE W10-CHECK ROUNDED = W10-CSH(WS-P1) + (W10-CSH(WS-P1) * W05-RTE)
               IF W10-CHECK > W10-VCSH(WS-P1)
                   SUBTRACT 0.01     FROM W10-CSH(WS-P1)
               ELSE
               IF W10-CHECK < W10-VCSH(WS-P1)
                   ADD 0.01          TO W10-CSH(WS-P1)
               END-IF
               COMPUTE W10-CHECK ROUNDED = W10-WSL(WS-P1) + (W10-WSL(WS-P1) * W05-RTE)
               IF W10-CHECK > W10-VWSL(WS-P1)
                   SUBTRACT 0.01     FROM W10-WSL(WS-P1)
               ELSE
               IF W10-CHECK < W10-VWSL(WS-P1)
                   ADD 0.01          TO W10-WSL(WS-P1).

       CB299.
             EXIT.
      *
      *    ****    C A S E S
      *
       BA000        SECTION 5.
       BA00.
             MOVE 10                 TO SHADE-ROW.
             MOVE 30                 TO SHADE-COL.
             MOVE 21                 TO SHADE-WIDTH.
             MOVE 3                  TO SHADE-LINES.
             DISPLAY S01.
             PERFORM SCREEN-SHADOW.
             MOVE SPACE              TO W10-USE-CASES.

       BA05.
             MOVE 11                 TO WS-MES-LINE.
             MOVE 48                 TO WS-MES-COL.
      *
      *      ACCEPT the option at the position specified above
      *
             MOVE X"FA"              TO WS-MES-CHAR(48).
             MOVE "1"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
             DISPLAY WS-OPTION AT 1148 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.
             MOVE WS-OPTION          TO W10-USE-CASES.
             PERFORM RESTORE-SCREEN-3.
           IF W10-USE-CASES = "N"
               MOVE ZERO             TO W10-CASES
               GO TO BA999.
             MOVE 10                 TO SHADE-ROW.
             MOVE 26                 TO SHADE-COL.
             MOVE 29                 TO SHADE-WIDTH.
             MOVE 3                  TO SHADE-LINES.
             DISPLAY S02.
             PERFORM SCREEN-SHADOW.

       BA10.
             ACCEPT S03.
             PERFORM CHECK-CORRECT.
             PERFORM RESTORE-SCREEN-3.
           IF WS-OPTION = "N"
               GO TO BA00.

       BA999.
             EXIT.
      *
      *    ****    P A C K S
      *
       BB000        SECTION 5.
       BB00.
           IF W10-USE-PACKS = "Y"
               MOVE "Y"              TO WS-PACKS
               PERFORM BB08
               DISPLAY S06
               GO TO BB90
           ELSE
               MOVE "N"              TO WS-PACKS.
             MOVE 10                 TO SHADE-ROW.
             MOVE 30                 TO SHADE-COL.
             MOVE 21                 TO SHADE-WIDTH.
             MOVE 3                  TO SHADE-LINES.
             DISPLAY S04.
             PERFORM SCREEN-SHADOW.
             MOVE SPACE              TO W10-USE-PACKS.

       BB05.
             MOVE 11                 TO WS-MES-LINE.
             MOVE 48                 TO WS-MES-COL.
      *
      *      ACCEPT the option at the position specified above
      *
             MOVE X"FA"              TO WS-MES-CHAR(48).
             MOVE "1"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
             DISPLAY WS-OPTION AT 1148 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.
             MOVE WS-OPTION          TO W10-USE-PACKS.
             PERFORM RESTORE-SCREEN-3.
           IF W10-USE-PACKS = "N"
               PERFORM BB75 THRU BB85
               GO TO BB999.

       BB08.
             MOVE 7                  TO SHADE-ROW.
             MOVE 11                 TO SHADE-COL.
             MOVE 60                 TO SHADE-WIDTH.
             MOVE 12                 TO SHADE-LINES.
             DISPLAY S05.
             PERFORM SCREEN-SHADOW.

       BB09.
             DISPLAY "Esc"      AT 5012 WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Cyan
                        "ape to cancel" WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan.

       BB10.
             ACCEPT S06.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  PERFORM BB75 THRU BB85
                               GO TO BB990
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO BB10
               END-EVALUATE.

       BB15.
             PERFORM CLEAR-L50.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO BB25.
             MOVE W10-LEDG           TO DPT-CODE.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
           IF (WS-F-ERROR = 7) OR (DPT-DESC = SPACES)
               PERFORM BB60 THRU BB70.
               MOVE 2                TO WS-P1.

       BB20.
           IF W10-UNT(WS-P1) = ZERO
               GO TO BB22.
             MULTIPLY W10-UNT(WS-P1) BY W10-COST GIVING W10-CCOST.
           IF W10-MKUP(WS-P1) NOT > ZERO
               PERFORM BB50.
             MOVE W10-SL(WS-P1)      TO W10-CSELL.
           IF W10-SL(WS-P1) = ZERO
               IF (W10-CCOST NOT = ZERO) AND (W10-MKUP(WS-P1) NOT = ZERO)
                   COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST * W10-MKUP(WS-P1)) / 100)).
           IF W10-CSELL = ZERO
               MOVE W10-CCOST        TO W10-CSELL.
             MOVE W10-CSELL          TO W10-SL(WS-P1).
           IF W10-CMKUP(WS-P1) NOT > ZERO
               PERFORM BB52.
             MOVE W10-CSH(WS-P1)     TO W10-CSELL.
           IF W10-CSH(WS-P1) = ZERO
               IF (W10-CCOST NOT = ZERO) AND (W10-CMKUP(WS-P1) NOT = ZERO)
                   COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST * W10-CMKUP(WS-P1)) / 100)).
           IF W10-CSELL = ZERO
               MOVE W10-CCOST        TO W10-CSELL.
             MOVE W10-CSELL          TO W10-CSH(WS-P1).
           IF W10-WMKUP(WS-P1) NOT > ZERO
               PERFORM BB54.
             MOVE W10-WSL(WS-P1)     TO W10-CSELL.
           IF W10-WSL(WS-P1) = ZERO
               IF (W10-CCOST NOT = ZERO) AND (W10-WMKUP(WS-P1) NOT = ZERO)
                   COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST * W10-WMKUP(WS-P1)) / 100)).
           IF W10-CSELL = ZERO
               MOVE W10-CCOST        TO W10-CSELL.
             MOVE W10-CSELL          TO W10-WSL(WS-P1).
      *
      *    ****    C H E C K   I F   E X C L U S I V E   R O U N D I N G
      *                            R E Q U I R E D
      *
           IF W05-ROUND = "E"
               MOVE W10-SL(WS-P1)    TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-SL(WS-P1)
               MOVE W10-WSL(WS-P1)   TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-WSL(WS-P1)
               MOVE W10-CSH(WS-P1)   TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-CSH(WS-P1).
            IF W10-TAX NOT = ZERO
                MOVE W05-VAT(W10-TAX) TO W05-RATE
                COMPUTE W10-VSL(WS-P1)  ROUNDED = W10-SL(WS-P1)  + (W10-SL(WS-P1)  * W05-RTE)
                COMPUTE W10-VWSL(WS-P1) ROUNDED = W10-WSL(WS-P1) + (W10-WSL(WS-P1) * W05-RTE)
                COMPUTE W10-VCSH(WS-P1) ROUNDED = W10-CSH(WS-P1) + (W10-CSH(WS-P1) * W05-RTE)
            ELSE
                MOVE W10-WSL(WS-P1)  TO W10-VWSL(WS-P1)
                MOVE W10-CSH(WS-P1)  TO W10-VCSH(WS-P1)
                MOVE W10-SL(WS-P1)   TO W10-VSL(WS-P1).
           IF W05-ROUND = "I"
               MOVE W10-VSL(WS-P1)   TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-VSL(WS-P1)
               MOVE W10-VWSL(WS-P1)  TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-VWSL(WS-P1)
               MOVE W10-VCSH(WS-P1)  TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-VCSH(WS-P1)
               PERFORM CB200.
             MOVE ZERO               TO W05-RATE.
           IF W10-TAX > 0
               MOVE W05-VAT(W10-TAX) TO W05-RATE.
             COMPUTE W10-VCSH(WS-P1) ROUNDED = W10-CSH(WS-P1) + (W10-CSH(WS-P1) * W05-RTE).
             COMPUTE W10-VWSL(WS-P1) ROUNDED = W10-WSL(WS-P1) + (W10-WSL(WS-P1) * W05-RTE).
             COMPUTE W10-VSL(WS-P1)  ROUNDED = W10-SL(WS-P1)  + (W10-SL(WS-P1)  * W05-RTE).

       BB22.
           IF WS-P1 < 4
               ADD 1                 TO WS-P1
               GO TO BB20.
             DISPLAY S06.
             PERFORM CHECK-CORRECT.
           IF NOT (WS-OPTION = "N")
               GO TO BB990.

       BB25.
             MOVE "Re-key: 'A'll,pack '1','2','3' [A]" TO WS-ERR-MES.
             MOVE "A"                TO WS-OPTION.
             MOVE 20                 TO SLIN.
             PERFORM OPT-SETUP THRU OPT-EXIT.
           IF NOT(WS-OPTION = "A" OR "1" OR "2" OR "3")
               GO TO BB25.
           IF WS-OPTION = "A"
               PERFORM RESTORE-SCREEN-3
               GO TO BB00.
             DISPLAY "Esc"      AT 5012 WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Cyan
                        "ape to cancel" WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan.
           IF WS-OPTION = "2"
               GO TO BB35
           ELSE
           IF WS-OPTION = "3"
               GO TO BB40.
      *
      *    ****    P A C K   1
      *
       BB30.
             ACCEPT S06-P1.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY   PERFORM BB75 THRU BB85
                                GO TO BB990
                 WHEN OTHER     PERFORM AA900-ALARM
                                GO TO BB30
               END-EVALUATE.
             GO TO BB15.
      *
      *    ****    P A C K   2
      *
       BB35.
             ACCEPT S06-P2.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY   PERFORM BB75 THRU BB85
                                GO TO BB990
                 WHEN OTHER     PERFORM AA900-ALARM
                                GO TO BB35
               END-EVALUATE.
             GO TO BB15.
      *
      *    ****    P A C K   3
      *
       BB40.
             ACCEPT S06-P3.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY   PERFORM BB75 THRU BB85
                                GO TO BB990
                 WHEN OTHER     PERFORM AA900-ALARM
                                GO TO BB40
               END-EVALUATE.
             GO TO BB15.
      *
      *    ****  D E F A U L T   M A R K U P
      *
       BB50.
           IF (W10-SL(WS-P1) > ZERO) AND (W10-CCOST > ZERO)
               MOVE W10-SL(WS-P1)    TO W10-CSELL
               COMPUTE W10-MKUP(WS-P1) ROUNDED = (((W10-CSELL - W10-CCOST) / W10-CCOST) * 100)
           ELSE
               MOVE DPT-MKUP(WS-P1)  TO W10-MKUP(WS-P1).

       BB51.
             EXIT.
      *
      *    ****  D E F A U L T   C A S H   M A R K U P
      *
       BB52.
           IF (W10-CSH(WS-P1) > ZERO) AND (W10-CCOST > ZERO)
               MOVE W10-CSH(WS-P1)   TO W10-CSELL
               COMPUTE W10-CMKUP(WS-P1) ROUNDED = (((W10-CSELL - W10-CCOST) / W10-CCOST) * 100)
           ELSE
               MOVE DPT-CMKUP(WS-P1) TO W10-CMKUP(WS-P1).

       BB53.
             EXIT.
      *
      *    ****  D E F A U L T   W H O L E S A L E   M A R K U P
      *
       BB54.
           IF (W10-WSL(WS-P1) > ZERO) AND (W10-CCOST > ZERO)
               MOVE W10-WSL(WS-P1)   TO W10-CSELL
               COMPUTE W10-WMKUP(WS-P1) ROUNDED = (((W10-CSELL - W10-CCOST) / W10-CCOST) * 100)
           ELSE
               MOVE DPT-WMKUP(WS-P1) TO W10-WMKUP(WS-P1).

       BB55.
             EXIT.
      *
      *    ****    N O  D E P A R T M E N T:  Z E R O  M A R K - U P S
      *
       BB60.
             MOVE 1                  TO WS-S1.

       BB65.
             MOVE ZERO               TO DPT-MKUP(WS-S1) DPT-CMKUP(WS-S1) DPT-WMKUP(WS-S1).
           IF WS-S1 < 4
               ADD 1                 TO WS-S1
               GO TO BB65.

       BB70.
             EXIT.
      *
      *    ****    C A N C E L  P A C K S   I N P U T
      *
       BB75.
             MOVE 2                  TO WS-P1.
           IF WS-PACKS = "Y"
               MOVE W10-EXT-ITEM     TO STK-CODE
               PERFORM READ-STOCK THRU READ-STOCK-EXIT
               GO TO BB82.
             MOVE "N"                TO W10-USE-PACKS.

       BB80.
             MOVE ZERO               TO W10-UNT(WS-P1) W10-MKUP(WS-P1) W10-CMKUP(WS-P1) W10-WMKUP(WS-P1)
                                        W10-SL(WS-P1)  W10-CSH(WS-P1)  W10-WSL(WS-P1).
           IF WS-P1 < 4
               ADD 1                 TO WS-P1
               GO TO BB80.
             GO TO BB85.

       BB82.
             MOVE STK-UNT(WS-P1)     TO W10-UNT(WS-P1).
             MOVE STK-PR(WS-P1)      TO W10-PR(WS-P1).
             MOVE STK-MKUP(WS-P1)    TO W10-MKUP(WS-P1).
             MOVE STK-CMKUP(WS-P1)   TO W10-CMKUP(WS-P1).
             MOVE STK-WMKUP(WS-P1)   TO W10-WMKUP(WS-P1).
             MOVE STK-SL(WS-P1)      TO W10-SL(WS-P1).
             MOVE STK-CSH(WS-P1)     TO W10-CSH(WS-P1).
             MOVE STK-WSL(WS-P1)     TO W10-WSL(WS-P1).
           IF WS-P1 < 4
               ADD 1                 TO WS-P1
               GO TO BB82.

       BB85.
           EXIT.

       BB90.
             MOVE "'A'mend,'D'elete or 'E'xit  [ ]" TO WS-ERR-MES.
             MOVE "A"                TO WS-OPTION.
             MOVE 20                 TO SLIN.
             PERFORM OPT-SETUP THRU OPT-EXIT.
           IF NOT(WS-OPTION = "A" OR "D" OR "E")
               GO TO BB90.
           IF WS-OPTION = "E"
               GO TO BB990.
           IF WS-OPTION = "A"
               GO TO BB09.
             MOVE "N"                TO WS-PACKS.
             PERFORM BB75 THRU BB85.

       BB990.
             PERFORM RESTORE-SCREEN-3.

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
             PERFORM CHANGE-HEADING.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

       ZA00A-CONTINUE.
             MOVE 3                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-CASES          TO WS-USE-CASES.
             MOVE PAR-PACKS          TO WS-USE-PACKS.
             MOVE 5                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-ROUND          TO W05-ROUND.
             MOVE PAR-ROUND-VAL      TO W05-ROUND-VAL.
           IF W05-ROUND-VAL > 0.01
               COMPUTE W05-HLF-VAL = W05-ROUND-VAL / 2
           ELSE
               MOVE ZERO             TO W05-HLF-VAL.
           IF W05-HLF-VAL > 0.01
               COMPUTE W05-TQTR-VAL = W05-HLF-VAL * 1.5
           ELSE
               MOVE ZERO             TO W05-TQTR-VAL.
             MOVE 1                  TO W05-V-RATE.

       ZA05-VAT.
             MOVE W05-VAT-CODE       TO DPT-CODE.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
             MOVE DPT-R-DATE         TO WS-VAT-DATE(W05-V-RATE).
             MOVE DPT-RATE           TO W05-VAT(W05-V-RATE).
             ADD 6 W05-V-RATE        GIVING WS-S1.
             MOVE DPT-P-RATE         TO W05-VAT(WS-S1).
           IF W05-V-RATE < 6
               ADD 1                 TO W05-V-RATE
               GO TO ZA05-VAT.
             MOVE 8                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF NOT(PAR-WS-HD = X"00000000000000000000")
               MOVE PAR-WS-HD        TO WS-WS-HD.
           IF NOT(PAR-CS-HD = X"00000000000000000000")
               MOVE PAR-CS-HD        TO WS-CS-HD.
           IF NOT(PAR-RT-HD = X"00000000000000000000")
               MOVE PAR-RT-HD        TO WS-RT-HD.

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
               WHEN 7     MOVE W02-DEPART   TO WS-FILE
                          MOVE DPT-CODE     TO WS-KEYX
               WHEN 15    MOVE WS-PARID     TO WS-FILE
                          MOVE WS-PARKEY    TO WS-KEY
               WHEN 22    MOVE W02-STOCKF   TO WS-FILE
                          MOVE STK-CODE     TO WS-KEYX
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STPPACKS"   TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
