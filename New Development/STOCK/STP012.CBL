      $set LINKCOUNT"468" GNT"STP012.GNT"
      *
      *       ONLY DELETE WHEN ALL BELOW IS COMPLETED  
      *     ===========================================
      *
      *  Screen co-ordinates must be revised for new layout
      *
      *  Screen Section                            Completed  
      *                                               ___
      *                    Layout screen co-ordinates| X |
      *                                               ---
      *                                               ___
      *                    Input screen co-ordinates | X |
      *                                               ---
      *                                               ___
      *                    Output screen co-ordinates| X |
      *                                               ---
      *  Procedure Division 
      *                                               ___
      *                    Screen Heading Format     | X |
      *                                               ---
      *                                               ___
      *                    Error Messages            | X |
      *                                               ---
      *                                               ___
      *                    User information messages | X |
      *                                               ---
      *                                               ___
      *                    Accept User options       | X |
      *                                               ---
      *                                               ___
      *                    Display co-ordinates      | X |
      *                                               ---
      *                                               ___
      *                    Accept co-ordinates       | X |
      *                                               ---
      *  Accept and Display of Stk-code allows
      *                                               ___
      *         Display ITM, EXT and 14 Characters   | X |
      *                                               ---
      *                                               ___
      *         Accept ITM, EXT, 14 Characters and   | X |
      *            all the Numeric Formats 3 to 14    ---
      *
      *  Stock file
      *                                               ___       
      *         Use the new layout and W10.STK       | X |
      *                                               ---
      ******************************************************************
      *                                                                *
      *    ******   ********  *******     ****       **      ******    *
      *   **    **     **     **    **   **  **     ***     **    **   *
      *   **           **     **    **  **    **     **          **    *
      *    ******      **     *******   **    **     **        **      *
      *         **     **     **        **    **     **      **        *
      *   **    **     **     **         **  **      **     **         *
      *    ******      **     **          ****    ********  ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *   P R I C E   C H A N G E S   P E R   S A L E S   L E D G E R  *
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
       PROGRAM-ID.     STP012.
       AUTHOR.         J.W.LEMMON.
       DATE-WRITTEN.   JANUARY 1983.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2018
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

       COPY "LEDTRF.SL".

       COPY "PARAM.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

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

       COPY "LEDTRF.FDE".

       COPY "RECOVER.FD".

       COPY "PARAM.FDE".

       COPY "SHARED.FDE".

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
       77  WS-DISC                 PIC  999V99   COMP-3.
       77  WS-SUB                  PIC  9(04)    COMP-5.
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S5                   PIC  9(04)    COMP-5.
       77  WS-S6                   PIC  9(04)    COMP-5.
       77  WS-S7                   PIC  9(04)    COMP-5.
       77  WS-S8                   PIC  9(04)    COMP-5.
       77  WS-IXS                  PIC  9(04)    COMP-5.
       77  WS-IXS1                 PIC  9(04)    COMP-5.
       77  WS-IXS2                 PIC  9(04)    COMP-5.
       77  WS-IXS3                 PIC  9(04)    COMP-5.
       77  WS-IXS4                 PIC  9(04)    COMP-5.
       77  WS-P1                   PIC  9(02)    COMP-5.
       77  WS-PARKEY               PIC  9(06)    COMP-5.
       77  WS-NETKEY               PIC  9(06)    COMP-5.
       77  WS-RECKEY               PIC  9(06)    COMP-5.
       77  WS-RECOVER              PIC  9(06)    COMP-5.
       77  WS-RECORDS              PIC  9(06)    COMP-5.
       77  WS-SHARED               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-TRANS                PIC  9(06)    COMP-5 VALUE 1.
       77  WS-STOCK                PIC  X(01).
       77  WS-KEYSTR               PIC  9(04)    COMP-5.
       77  WS-MONTHS               PIC S9(01)V99 COMP-3.
       77  WS-KEY1                 PIC  9(04)    COMP-5.
       77  WS-ITM                  PIC  X(18).
       77  WS-USE-CASES            PIC  X(01).
       77  WS-USE-PACKS            PIC  X(01).
       77  WS-USE-ITM              PIC  X(01).
       77  WS-EXT-STK              PIC  X(01).
       77  WS-ETYPE                PIC  X(01).
       77  WS-OPTION               PIC  X(01).
       77  WS-SKIP                 PIC  X(01).
       77  WS-ADJ                  PIC  X(01).
       77  WS-ERROR                PIC  9(01).
       77  WS-AMEND                PIC  9(01)    VALUE 0.
       77  WS-CONREC               PIC  9(01)    VALUE ZERO.
       77  WS-LEDG                 PIC  X(04).
       77  WS-LEDG1                PIC  X(04).
       77  WS-COMP                 PIC  9(01).
       77  WS-WEEK                 PIC  9(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-PASSWORD             PIC  X(08).
       77  WS-PPASSWORD            PIC  X(06).
       77  WS-PRINT                PIC  9(01).
       77  WS-PASS                 PIC  X(08).
       77  WS-DESC-H1              PIC  X(14)    VALUE "Description".
       77  WS-DESC-H2              PIC  X(14)    VALUE "Description 2".
       77  WS-RT-HD                PIC  X(10)    VALUE "-Retail   ".
       77  WS-WS-HD                PIC  X(10)    VALUE "-Wholesale".
       77  WS-CS-HD                PIC  X(10)    VALUE "-Cash sale".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STP012".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY CHANGE PRICES PER S/L.".
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

       COPY "LEDTRF.WS".

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

       COPY "W05.VAT".

       COPY "W05.RND".

       01  W09-STOCK.
           03  W09-MARKUP          PIC S9(03)V9999 COMP-3.
           03  W09-CMARKUP         PIC S9(03)V9999 COMP-3.
           03  W09-WMARKUP         PIC S9(03)V9999 COMP-3.

       COPY "W10.STK".

       COPY "W11.STK".

      *
      *    Date routines for accepting checking and formatting have
      *    been removed from each program and the DateCheck program
      *    will be called to handle these routines.
      *
      *    The following copy 'DateVariables.CPY' now includes the
      *    'W12.WS'and 'W20.WS' variables and is passed to DateCheck
      *    in the CALL statement.
      *
       COPY "DateVariables.cpy".

       COPY "W15.STK".

       COPY "W25.STK".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W60-ITMCODE.
           03  W60-ITM.
               05  W60-ICHAR       PIC  X(01)    OCCURS 8.

       01  W65-ENTRY.
           03  W65-FOUR.
               05  W65-CHAR        PIC  X(01)    OCCURS 2.

       01  W80-EDIT.
           03  W80-CNO             PIC  9(04).
           03  W80-V1.
               05  W80-S7V2        PIC  Z(06)9.99-.
           03  W80-V2.
               05  W80-S5V2        PIC  Z(04)9.99-.
           03  W80-DTE.
               05  W80-DATE        PIC  Z9/99/9999.
           03  W80-QNT.
               05  W80-5           PIC  Z(04)9.99.
           03  W80-QNT1                          REDEFINES W80-QNT.
               05  W80-3V2         PIC  Z(02)9.9999.
           03  W80-NO.
               05  W80-5N          PIC  9(05).
               05  W80-REC         PIC  ZZZZ9    REDEFINES W80-5N.
           03  W80-DEL.
               05  W80-2N          PIC 9(02).

       01  W85-PASS.
           03  W85-SUPER           PIC  X(06)    OCCURS 9.
           03  W85-ENTRY           PIC  9(02)    COMP-3.
           03  W85-MARG            PIC S9(02)V99.

       01  W90-BAL                 PIC S9(07)V99 COMP-3.

       01  W95-COMP                PIC  X(40).

       01  W100-EDIT.
           03  W100-V2.
               05  W100-S6V2       PIC  Z(05)9.99-.
           03  W100-V3                           REDEFINES W100-V2.
               05  W100-PRICE      PIC  X(09).
               05  FILLER          PIC  X(01).
           03  W100-V5                           REDEFINES W100-V2.
               05  W100-QNT        PIC  Z(05)9.99-.

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

       01  S05.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey.
      *                                    1         2         3         4         5         6         7         8
      *                           12345678901234567890123456789012345678901234567890123456789012345678901234567890
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
               05          COLUMN 20 PIC X(11) FROM  WS-RT-HD                                             FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 31 VALUE                     "                                       "  FOREGROUND-COLOR Red.
               05          COLUMN 70 VALUE                                                            "³" HIGHLIGHT.
               05  LINE 13 COLUMN 11 VALUE "³"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  "        "                                                    FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 20 PIC X(11) FROM  WS-CS-HD                                             FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 31 VALUE                     "                                       "  FOREGROUND-COLOR Red.
               05          COLUMN 70 VALUE                                                            "³" HIGHLIGHT.
               05  LINE 14 COLUMN 11 VALUE "³"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  "        "                                                    FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 20 PIC X(11) FROM  WS-WS-HD                                             FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 31 VALUE                     "                                       "  FOREGROUND-COLOR Red.
               05          COLUMN 70 VALUE                                                            "³" HIGHLIGHT VALUE "³".
               05  LINE 15 COLUMN 11 VALUE "³"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  " Price  "                                                    FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 20 PIC X(11) FROM  WS-RT-HD                                             FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 31 VALUE                     "                                       "  FOREGROUND-COLOR Red.
               05          COLUMN 70 VALUE                                                            "³" HIGHLIGHT.
               05  LINE 16 COLUMN 11 VALUE "³"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  "        "                                                    FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 20 PIC X(11) FROM  WS-CS-HD                                             FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 31 VALUE                     "                                       "  FOREGROUND-COLOR Red.
               05          COLUMN 70 VALUE                                                            "³" HIGHLIGHT.
               05  LINE 17 COLUMN 11 VALUE "³"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  "        "                                                    FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 20 PIC X(11) FROM  WS-WS-HD                                             FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 31 VALUE                     "                                       "  FOREGROUND-COLOR Red.
               05          COLUMN 70 VALUE                                                            "³" HIGHLIGHT VALUE "³".
               05  LINE 18 COLUMN 11 VALUE "À"                                                            FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" HIGHLIGHT.

       01  S06.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               05  S06-P1.
                   07  LINE 10 COLUMN 31 PIC X(08)       FROM  W10-PER-2.
                   07  LINE 11 COLUMN 31 PIC ZZZ9.999    FROM  W10-UNIT-2.
                   07  LINE 12 COLUMN 31 PIC ZZ9.99      USING W10-MARKUP-2 AUTO.
                   07  LINE 13 COLUMN 31 PIC ZZ9.99      USING W10-CMARKUP-2 AUTO.
                   07  LINE 14 COLUMN 31 PIC ZZ9.99      USING W10-WMARKUP-2 AUTO.
                   07  LINE 15 COLUMN 31 PIC ZZZZZZZ9.99 USING W10-SELL-2 AUTO.
                   07  LINE 16 COLUMN 31 PIC ZZZZZZZ9.99 USING W10-CASH-2 AUTO.
                   07  LINE 17 COLUMN 31 PIC ZZZZZZZ9.99 USING W10-WSALE-2 AUTO.
               05  S06-P2.
                   07  LINE 10 COLUMN 44 PIC X(08)       FROM  W10-PER-3.
                   07  LINE 11 COLUMN 44 PIC ZZZ9.999    FROM  W10-UNIT-3.
                   07  LINE 12 COLUMN 44 PIC ZZ9.99      USING W10-MARKUP-3 AUTO.
                   07  LINE 13 COLUMN 44 PIC ZZ9.99      USING W10-CMARKUP-3 AUTO.
                   07  LINE 14 COLUMN 44 PIC ZZ9.99      USING W10-WMARKUP-3 AUTO.
                   07  LINE 15 COLUMN 44 PIC ZZZZZZZ9.99 USING W10-SELL-3 AUTO.
                   07  LINE 16 COLUMN 44 PIC ZZZZZZZ9.99 USING W10-CASH-3 AUTO.
                   07  LINE 17 COLUMN 44 PIC ZZZZZZZ9.99 USING W10-WSALE-3 AUTO.
               05  S06-P3.
                   07  LINE 10 COLUMN 57 PIC X(08)       FROM  W10-PER-4.
                   07  LINE 11 COLUMN 57 PIC ZZZ9.999    FROM  W10-UNIT-4.
                   07  LINE 12 COLUMN 57 PIC ZZ9.99      USING W10-MARKUP-4 AUTO.
                   07  LINE 13 COLUMN 57 PIC ZZ9.99      USING W10-CMARKUP-4 AUTO.
                   07  LINE 14 COLUMN 57 PIC ZZ9.99      USING W10-WMARKUP-4 AUTO.
                   07  LINE 15 COLUMN 57 PIC ZZZZZZZ9.99 USING W10-SELL-4 AUTO.
                   07  LINE 16 COLUMN 57 PIC ZZZZZZZ9.99 USING W10-CASH-4 AUTO.
                   07  LINE 17 COLUMN 57 PIC ZZZZZZZ9.99 USING W10-WSALE-4 AUTO.

       01  S20.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE 11 COLUMN 20 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
               05  LINE 12 COLUMN 20 VALUE "³ Enter price change - Password        ³".
               05  LINE 13 COLUMN 20 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S20A.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 12 COLUMN 52 PIC X(06) USING WS-PPASSWORD AUTO SECURE.

      *
      *    ****    PRICE CHANGE INPUT SCREEN
      *
       01  S26.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               05  S26AB.
                   07  S26A.
                       09  LINE 15 COLUMN  3 FOREGROUND-COLOR Cyan PIC X(14) FROM W10-EXT-ITEM.
                       09          COLUMN 22 FOREGROUND-COLOR Cyan PIC X(30) FROM W10-DESC.

                   07  S26B.
                       09  LINE 17 COLUMN 66 PIC Z(07)9.999 USING W10-COST    AUTO.
                       09  LINE 18 COLUMN 71 PIC ZZ9.9999   USING W10-WMARKUP AUTO.
                       09  LINE 19 COLUMN 66 PIC Z(07)9.999 USING W10-WSALE   AUTO.
                       09  LINE 20 COLUMN 71 PIC ZZ9.9999   USING W10-CMARKUP AUTO.
                       09  LINE 21 COLUMN 66 PIC Z(07)9.999 USING W10-CASH    AUTO.
                       09  LINE 22 COLUMN 71 PIC ZZ9.9999   USING W10-MARKUP  AUTO.
                       09  LINE 23 COLUMN 66 PIC Z(07)9.999 USING W10-SELL    AUTO.

               05  S26-DESC2.
                   07  LINE 16 COLUMN 22 PIC X(30) FROM W10-DESC2 FOREGROUND-COLOR Cyan.

               05  S26-ITM.
                   07  LINE 15 COLUMN  3 PIC X(14) FROM W10-ITEM.
                   07          COLUMN 17 VALUE "/" FOREGROUND-COLOR Black.
                   07          COLUMN 18 PIC X(03) FROM W10-ITM.

               05  S26-EXT.
                   07  LINE 15 COLUMN  3 PIC X(18) FROM W10-EXT-ITEM.

      *
      *    ****    PRICE CHANGE PER SALES LEDGER DISPLAY SCREEN
      *
       01  S31.
      *                                             1         2         3         4         5         6         7         8
      *                                    12345678901234567890123456789012345678901234567890123456789012345678901234567890
           03  S31A.
               05  LINE  6 COLUMN 31 VALUE                              "STOCK PRICE CHANGES"                             FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE  8 COLUMN  2 VALUE "This option allows you to change the prices on all the items in the S/Ledger".
               05  LINE  9 COLUMN  2 VALUE "that has been selected. Each of the items in the S/Ledger will be  displayed".
               05  LINE 10 COLUMN  2 VALUE "and the price can then be adjusted manually.".
               05  LINE 13 COLUMN  3 VALUE "Sales Ledger".
               05  LINE 14 COLUMN  3 VALUE "Item code".
               05          COLUMN 22 VALUE                     "Description".

           03  S31B.
               05  LINE 17 COLUMN 53 VALUE                                                    "Cost Price  ".
               05  LINE 18 COLUMN 53 VALUE                                                    "W/sale Mrkup".
               05  LINE 19 COLUMN 53 VALUE                                                    "W/sale Price".
               05  LINE 20 COLUMN 53 VALUE                                                    "Cash Markup ".
               05  LINE 21 COLUMN 53 VALUE                                                    "Cash Price  ".
               05  LINE 22 COLUMN 53 VALUE                                                    "Ret. Markup ".
               05  LINE 23 COLUMN 53 VALUE                                                    "Retail Price".

       01  S40.
           03  PIC X(04) USING WS-LEDG FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.

       01  S-ESC.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT.
               05  LINE 50 COLUMN 12 VALUE "Esc"               FOREGROUND-COLOR Brown.
               05          COLUMN 14 VALUE    "ape to cancel".
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
           IF LS-OK = "N"
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM BO000.
             CLOSE RECOVER.

       AA49.
             EXIT PROGRAM.

      *
      *    ****    S E C U R I T Y   V I O L A T I O N
      *
      *AA500              SECTION.
      *AA501.
      *      PERFORM SAVE-SCREEN.
      *      MOVE 7                 TO SHADE-ROW.
      *      MOVE 26                TO SHADE-COL.
      *      MOVE 29                TO SHADE-WIDTH.
      *      MOVE 4                 TO SHADE-LINES.
      *      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0726 WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Red  HIGHLIGHT
      *                                          "¿"        WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Black.
      *      DISPLAY "³"                            AT 0826 WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Red  HIGHLIGHT
      *               " Insufficient security     "         WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Grey HIGHLIGHT
      *                                          "³"        WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Black.
      *      DISPLAY "³"                            AT 0926 WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Red  HIGHLIGHT
      *               " Press ENTER to continue   "         WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Grey HIGHLIGHT
      *                                          "³"        WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Black.
      *      DISPLAY "À"                            AT 1026 WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Red  HIGHLIGHT
      *               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"        WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Black.
      *      PERFORM SCREEN-SHADOW.
      *      ACCEPT WS-OPTION AT 0752 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey AUTO.
      *      PERFORM RESTORE-SCREEN.
      *
      *AA505.
      *      EXIT.

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

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "DEPART.RD".

       COPY "LEDTRF.RD".

       COPY "PARAM.RD".

       COPY "SHARED.RD".

       COPY "STOCK.RD".

      *
      *   R E W R I T E   &    W R I T E   F I L E S   R O U T I N E S
      *
       AD000-WRITE             SECTION.

       COPY "CONTROL.WR".

       COPY "DEPART.WR".

       COPY "LEDTRF.WR".

       COPY "PARAM.WR".

       COPY "SHARED.WR".

       COPY "STOCK.WR".

       COPY "STOCK.1ST".

      *
      *       ****   *****  ***    ***   *   *  *****  ****   *   *
      *       *   *  *     *   *  *   *  *   *  *      *   *  *   *
      *       ****   ***   *      *   *  *   *  ***    ****    * *
      *       *   *  *     *   *  *   *   * *   *      *   *    *
      *       *   *  *****  ***    ***     *    *****  *   *    *
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *          THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *        ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *        ³ ORIGINAL ACTION (REC-TYPE)  ÄÄ  RECOVERY PROCESS ³
      *        ³ 0 = RECORD CHANGED          ÄÄ     (REWRITE)     ³
      *        ³ 1 = RECORD WAS ADDED        ÄÄ     (DELETE)      ³
      *        ³ 2 = RECORD WAS DELETED      ÄÄ     (WRITE)       ³
      *        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY000           SECTION.
      *    *************************************************************
      *    ****             P A R A M E T E R   F I L E             ****
      *    *************************************************************
       AY01.
             MOVE 01                 TO REC-FILE.
             MOVE WS-PARKEY          TO REC-KEY.
             MOVE PAR-RECORD1        TO REC-PARAM.
             GO TO AY50.
      *    *************************************************************
      *    ****                 S T O C K   F I L E                 ****
      *    *************************************************************
       AY05.
             MOVE 05                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
           IF WS-ACTION = 0 OR 2
               PERFORM READ-STOCK-LOCK THRU READ-STOCK-EXIT.
           IF WS-ACTION = 9
               MOVE 0                TO REC-TYPE
           ELSE
               MOVE WS-ACTION        TO REC-TYPE.
             MOVE STK-RECORD1        TO REC-STOCKF.
             GO TO AY50.
      *    *************************************************************
      *    ****       N E T W O R K   (S H A R E D)   F I L E       ****
      *    *************************************************************
       AY37.
             MOVE 37                 TO REC-FILE.
             MOVE WS-SHARED          TO REC-KEY.
             MOVE SHR-RECORD         TO REC-NETWORK.
             GO TO AY50.
      *    *************************************************************
      *    ****            D E P A R T M E N T   F I L E            ****
      *    *************************************************************
       AY38.
             MOVE 38                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
             MOVE DPT-RECORD         TO REC-DEPART.
             GO TO AY50.
      *    *************************************************************
      *    ****               C O N T R O L   F I L E               ****
      *    *************************************************************
       AY39.
             MOVE 39                 TO REC-FILE.
             MOVE WS-NETKEY          TO REC-KEY.
             MOVE NET-RECORD         TO REC-NETWORK.
             GO TO AY50.

       AY40.
             MOVE 40                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
             MOVE XFR-REC            TO REC-LEDTRF.
             GO TO AY50.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³              End transaction in recovery log              ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY49.
             MOVE 99                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE SPACES             TO REC-DETAIL.
             PERFORM AY50 THRU AY59.
             ADD 1                   TO WS-TRANS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If recovery records exceed 95 - Clear (Close and Open     ³
      *    ³ new). This allows for quicker recoveries when required.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF WS-RECOVER > 95
               CLOSE RECOVER
               OPEN OUTPUT RECOVER
               CLOSE RECOVER
               OPEN I-O RECOVER
               MOVE ZERO             TO WS-RECOVER.
             GO TO AY59.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³         W R I T E   R E C O V E R Y   R E C O R D         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY50.
             ADD 1                   TO WS-RECOVER.
             MOVE WS-RECOVER         TO WS-RECKEY.
             MOVE WS-TRANS           TO REC-TRAN.
             WRITE REC-RECORD.
           IF WS-STATUS NOT = "00"
               DISPLAY "Write error Recovery file - Status " AT 2212 WITH FOREGROUND-COLOR Brown HIGHLIGHT WS-STATUS WITH FOREGROUND-COLOR Grey HIGHLIGHT
               STOP RUN.
       AY59.
             EXIT.
      *    *************************************************************
      *    ****        Start processing transaction
      *    *************************************************************
       AY60.
             MOVE 1                  TO WS-COUNT.
             MOVE 5                  TO WS-SHARED.
             PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
             MOVE SHR-STOCK          TO WS-STOCK.
      *    *************************************************************
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *    *************************************************************
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-USERS < 24
               MOVE 1                TO WS-SUB
               MOVE ZERO             TO WS-WAIT
               GO TO AY62.
      *    *************************************************************
      *     Q   F U L L  -  W A I T   F O R   A  C O U N T  O F   400
      *    *************************************************************
             DISPLAY "WAITING" AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red HIGHLIGHT.
             COMMIT.
             ACCEPT WS-STIME FROM TIME.
             MOVE 400                TO WS-WAIT.
             PERFORM LOCK-REC-LOOP.
             DISPLAY SPACE AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.
             GO TO AY60.

       AY61.
             MOVE "ST"               TO PAR-PROG(WS-USUB).
             MOVE LS-USER            TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the STOCK control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
           IF WS-STOCK = "Y"
               MOVE 3                TO WS-SHARED
               PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT
               PERFORM AY37 THRU AY59
               MOVE SHR-RECORD       TO NET-RECORD
           ELSE
               MOVE 3                TO WS-NETKEY
               PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT
               PERFORM AY39 THRU AY59.
             GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N P R O G R E S S
      *
       AY62.
           IF NOT(PAR-PROG(WS-SUB) = SPACES OR PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R   O R   S T O C K   F I L E S
      *           B E I N G   U P D A T E D
      *
               IF NOT(PAR-PROG(WS-SUB) = SPACES)
                   IF PAR-PROG(WS-SUB) = "DB" OR "DS" OR "CS" OR "ST"
      *
      *    ****   Y E S   -   S E T   W A I T   P E R I O D
      *
                       GO TO AY62-WAIT
                   ELSE
                       ADD 1         TO WS-SUB
                       GO TO AY62
                   END-IF
               ELSE
      *
      *    ****   I S   T H I S   P R O G R A M   I N   T H E   Q
      *
               IF PAR-USR(WS-SUB) = LS-USER
      *
      *    ****   I S   I T   N E X T   I N   L I N E   T O   P R O C E S S
      *
                   IF WS-WAIT = ZERO
                       GO TO AY63
                   ELSE
                       GO TO AY62-WAIT
                   END-IF
               ELSE
                   GO TO AY62-WAIT
               END-IF
           END-IF.
             MOVE LS-USER            TO PAR-USR(WS-SUB).
             MOVE WS-SUB             TO PAR-USERS.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             GO TO AY62-CHECK.
      *
      *    ****   S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
             MOVE 300                TO WS-WAIT.
           IF NOT(PAR-USR(WS-SUB) = LS-USER)
               IF WS-SUB < 24
                   ADD 1             TO WS-SUB
                   GO TO AY62.

       AY62-CHECK.
           IF WS-WAIT > ZERO
               COMMIT
               DISPLAY "Waiting" AT 5072 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK
               ACCEPT WS-STIME FROM TIME
               PERFORM LOCK-REC-LOOP
               DISPLAY "Waiting" AT 5072 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK
               GO TO AY60.
             DISPLAY SPACE AT 5072 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.

       AY63.
             MOVE WS-SUB             TO WS-USUB.
             GO TO AY61.

       AY70.
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 3.
      *
           IF WS-STOCK = "Y"
               MOVE NET-RECORD       TO SHR-RECORD
               PERFORM REWRITE-SHARED THRU WRITE-SHARED-EXIT
           ELSE
               PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
               PERFORM AY49 THRU AY59.
             MOVE 1                  TO WS-USUB.

       AY72.
           IF NOT(PAR-USR(WS-USUB) = LS-USER)
               IF WS-USUB < 24
                   ADD 1             TO WS-USUB
                   GO TO AY72
               ELSE
                   GO TO AY85.

       AY75.
             MOVE SPACES             TO PAR-PROG(WS-USUB) PAR-USR(WS-USUB).
           IF WS-USUB < 24
               ADD 1 WS-USUB         GIVING WS-SUB
           ELSE
               GO TO AY80.
           IF NOT(PAR-PROG(WS-SUB) = SPACES)
               MOVE PAR-PROG(WS-SUB) TO PAR-PROG(WS-USUB)
               MOVE PAR-USR(WS-SUB)  TO PAR-USR(WS-USUB)
               ADD 1                 TO WS-USUB
               GO TO AY75.

       AY80.
             SUBTRACT 1 FROM WS-USUB GIVING PAR-USERS.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.

       AY85.
           IF LS0-POS NOT = 2
               CLOSE RECOVER
               OPEN I-O RECOVER.
             COMMIT.

       AY999.
             EXIT.

       COPY "CA000.STK".

       COPY "CA100.STK".

       COPY "CA200.STK".

       COPY "CA500.STK".

       COPY "ROUND.PRC".

       COPY "LEDTRF.UPD".

      *
      *    ****  P R I C E   C H A N G E S
      *
       BO000         SECTION 50.
       BO0.
             PERFORM ERASE-SCREEN.
             DISPLAY S31.
             PERFORM SAVE-SCREEN-2.
             DISPLAY S20.
             MOVE 11                 TO SHADE-ROW.
             MOVE 20                 TO SHADE-COL.
             MOVE 40                 TO SHADE-WIDTH.
             MOVE 3                  TO SHADE-LINES.
             PERFORM SCREEN-SHADOW.
             ACCEPT S20A.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING WS-PPASSWORD BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             MOVE 5                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-PW             TO W85-PASS.
             PERFORM RESTORE-SCREEN-2.
           IF NOT(WS-PPASSWORD = W85-SUPER(1) OR W85-SUPER(9) OR "CECIRP")
               MOVE "Invalid password" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BO999.

       BO00.
             PERFORM ERASE-SCREEN.
             DISPLAY S31.
             MOVE SPACES             TO WS-LEDG.

       BO05.
            ACCEPT S40 AT 1316.
          IF WS-LEDG = SPACES
               GO TO BO999.
             MOVE WS-LEDG            TO DPT-CODE.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
           IF DPT-DESC NOT = SPACES
               GO TO BO07.

       BO06.
             MOVE "Invalid or No record" TO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.
             GO TO BO05.

       BO07.
             DISPLAY DPT-DESC AT 1321 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO BO05.
             MOVE WS-LEDG            TO STK-LDG.
             MOVE "!"                TO STK-CODE.
             PERFORM START-AT-STOCK-LEDG THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               MOVE "No record"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BO05.

       BO10.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO BO45.
           IF STK-LDG NOT = WS-LEDG
               UNLOCK STOCK
               GO TO BO45.
           IF STK-IND = 4
               GO TO BO10.
             MOVE STK-CODE           TO W10-EXT-ITEM.
             MOVE STK-DESC           TO W10-DESC.
             MOVE STK-DESC2          TO W10-DESC2.
             MOVE STK-COST           TO W10-COST.
             MOVE STK-SELL           TO W10-SELL.
             MOVE STK-MARKUP         TO W10-MARKUP.
             MOVE STK-CMARKUP        TO W10-CMARKUP.
             MOVE STK-WMARKUP        TO W10-WMARKUP.
             MOVE STK-CASH           TO W10-CASH.
             MOVE STK-WSALE          TO W10-WSALE.
             DISPLAY S26AB.
           IF W10-STCK = "A"
               IF WS-EXT-STK = "Y"
                   DISPLAY S26-EXT
               ELSE
               IF WS-USE-ITM = "Y"
                   DISPLAY S26-ITM.
           IF NOT(W10-DESC2 = SPACES)
               DISPLAY S26-DESC2.

       BO35.
             ACCEPT S26B.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
                GO TO BO35.
             PERFORM AY60 THRU AY999.
             MOVE 0                  TO WS-ACTION.
             PERFORM AY05 THRU AY59.
             MOVE STK-LDG            TO DPT-CODE.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
           IF W10-MARKUP = ZERO
               PERFORM BO50.

       BO40.
             MOVE W10-COST           TO W10-CCOST.
             MOVE W10-SELL           TO W10-CSELL.
           IF W10-COST = ZERO
               IF W10-SELL NOT = ZERO
                   COMPUTE W10-CCOST = (W10-CSELL / (100 + W10-MARKUP) * 100) + .005.
           IF W10-SELL = ZERO
               IF W10-COST NOT = ZERO
                   COMPUTE W10-CSELL = (W10-CCOST + ((W10-CCOST * W10-MARKUP) / 100)) + .005.
             MOVE W10-CCOST          TO W10-COST.
             MOVE W10-CSELL          TO W10-SELL.
             MOVE W10-COST           TO STK-COST.
             MOVE W10-MARKUP         TO STK-MARKUP.
           IF W10-CMARKUP = ZERO
               PERFORM BO52.
           IF (W10-CMARKUP = ZERO) AND (W10-CASH = ZERO)
               MOVE W10-MARKUP       TO W10-CMARKUP
               MOVE W10-SELL         TO W10-CASH
               GO TO BO42.
           IF W10-CMARKUP = ZERO
               MOVE W10-CASH         TO W10-CSELL
               COMPUTE W10-CMARKUP = (((W10-CSELL - W10-CCOST) / W10-CCOST) * 100) + .005.
           IF W10-CASH = ZERO
               COMPUTE W10-CSELL = (W10-CCOST + ((W10-CCOST * W10-CMARKUP) / 100)) + .005
               MOVE W10-CSELL        TO W10-CASH.
       BO42.
             MOVE W10-CMARKUP        TO STK-CMARKUP.
           IF W10-WMARKUP = ZERO
               PERFORM BO54.
           IF (W10-WMARKUP = ZERO) AND (W10-WSALE = ZERO)
               MOVE W10-CMARKUP      TO W10-WMARKUP
               MOVE W10-CASH         TO W10-WSALE
               GO TO BO43.
           IF W10-WMARKUP = ZERO
               MOVE W10-WSALE        TO W10-CSELL
               COMPUTE W10-WMARKUP = (((W10-CSELL - W10-CCOST) / W10-CCOST) * 100) + .005.
           IF W10-WSALE = ZERO
               COMPUTE W10-CSELL = (W10-CCOST + ((W10-CCOST * W10-WMARKUP) / 100)) + .005
               MOVE W10-CSELL       TO W10-WSALE.
       BO43.
             MOVE W10-WMARKUP       TO STK-WMARKUP.
           IF W05-ROUND = "E"
               MOVE W10-SELL        TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL       TO W10-SELL
               MOVE W10-WSALE       TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL       TO W10-WSALE
               MOVE W10-CASH        TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL       TO W10-CASH.
           IF STK-TAX NOT = ZERO
               MOVE W05-VAT(STK-TAX) TO W05-RATE
               COMPUTE W10-VSELL ROUNDED = W10-SELL + (W10-SELL * W05-RTE)
               COMPUTE W10-VWSALE ROUNDED = W10-WSALE + (W10-WSALE * W05-RTE)
               COMPUTE W10-VCASH ROUNDED = W10-CASH + (W10-CASH * W05-RTE)
           ELSE
               MOVE W10-WSALE        TO W10-VWSALE
               MOVE W10-CASH         TO W10-VCASH
               MOVE W10-SELL         TO W10-VSELL.
           IF W05-ROUND = "I"
               MOVE W10-VSELL        TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-VSELL
               MOVE W10-VWSALE       TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-VWSALE
               MOVE W10-VCASH        TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-VCASH
               PERFORM CB200.
             MOVE W10-SELL           TO STK-SELL.
             MOVE W10-CASH           TO STK-CASH.
             MOVE W10-WSALE          TO STK-WSALE.
             PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
             PERFORM AY70 THRU AY999.
      *
      *    ****    P A C K   P R I C E S
      *
           IF STK-USE-PACKS = "Y"
               PERFORM BP000
      *
      *    ****   R E - S T A R T   R E C O V E R Y   L O G I N G
      *
               PERFORM AY60 THRU AY999
               MOVE 0                TO WS-ACTION
               PERFORM AY05 THRU AY59
               PERFORM BO60 THRU BO70
               PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT
               PERFORM AY70 THRU AY999.
             MOVE "ENTER to continue - 'E' to end  [ ]" TO WS-OPT-MES.
             MOVE SPACE              TO WS-OPTION.
             MOVE "Z"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
             MOVE 17                 TO CRT-START.
             PERFORM ERASE-SCREEN.
           IF WS-OPTION = "E"
               GO TO BO00.
             PERFORM START-AT-STOCK-LEDG THRU READ-STOCK-EXIT.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
             DISPLAY S31B.
             GO TO BO10.
       BO45.
             MOVE "End of category - ENTER for next - 'E' to end" TO WS-OPT-MES.
             MOVE SPACE              TO WS-OPTION.
             MOVE "Z"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
          IF WS-OPTION = "E"
               GO TO BO999.
             GO TO BO00.
      *
      *    ****  D E F A U L T   M A R K U P
      *
       BO50.
           IF W10-SELL > ZERO AND W10-COST > ZERO
               MOVE W10-SELL         TO W10-CSELL
               MOVE W10-COST         TO W10-CCOST
               COMPUTE W10-MARKUP = (((W10-CSELL - W10-CCOST) / W10-COST) * 100) + .005
           ELSE
               MOVE DPT-MARKUP       TO W10-MARKUP.
       BO51.
             EXIT.
      *
      *    ****  D E F A U L T   C A S H   M A R K U P
      *
       BO52.
           IF W10-CASH > ZERO AND W10-COST > ZERO
               MOVE W10-CASH         TO W10-CSELL
               MOVE W10-COST         TO W10-CCOST
               COMPUTE W10-CMARKUP = (((W10-CSELL - W10-CCOST) / W10-COST) * 100) + .005
           ELSE
               MOVE DPT-CMRKUP       TO W10-CMARKUP.
       BO53.
             EXIT.
      *
      *    ****  D E F A U L T   W H O L E S A L E   M A R K U P
      *
       BO54.
           IF W10-WSALE > ZERO AND W10-COST > ZERO
               MOVE W10-WSALE        TO W10-CSELL
               MOVE W10-COST         TO W10-CCOST
               COMPUTE W10-WMARKUP = (((W10-CSELL - W10-CCOST) / W10-COST) * 100) + .005
           ELSE
               MOVE DPT-CMRKUP       TO W10-WMARKUP.
       BO55.
             EXIT.

       BO60.
             MOVE 2                  TO WS-S1.
      *
      *    ****   P A C K   D E T A I L S
      *
       BO65.
             MOVE W10-MKUP(WS-S1)    TO STK-MKUP(WS-S1).
             MOVE W10-CMKUP(WS-S1)   TO STK-CMKUP(WS-S1).
             MOVE W10-WMKUP(WS-S1)   TO STK-WMKUP(WS-S1).
             MOVE W10-SL(WS-S1)      TO STK-SL(WS-S1).
             MOVE W10-CSH(WS-S1)     TO STK-CSH(WS-S1).
             MOVE W10-WSL(WS-S1)     TO STK-WSL(WS-S1).
           IF WS-S1 < 4
               ADD 1                 TO WS-S1
               GO TO BO65.

       BO70.
             EXIT.

       BO999.
             EXIT.
      /
       BP000        SECTION 50.
       BP00.
             PERFORM SAVE-SCREEN-3.
      *
      *    ****    U N I T S   A N D   M A R K U P S
      *
             MOVE 1                  TO WS-S1.

       BP05.
             MOVE STK-UNT(WS-S1)     TO W10-UNT(WS-S1).
             MOVE STK-PR(WS-S1)      TO W10-PR(WS-S1).
             MOVE STK-MKUP(WS-S1)    TO W10-MKUP(WS-S1).
             MOVE STK-CMKUP(WS-S1)   TO W10-CMKUP(WS-S1).
             MOVE STK-WMKUP(WS-S1)   TO W10-WMKUP(WS-S1).
             MOVE STK-SL(WS-S1)      TO W10-SL(WS-S1).
             MOVE STK-CSH(WS-S1)     TO W10-CSH(WS-S1).
             MOVE STK-WSL(WS-S1)     TO W10-WSL(WS-S1).
           IF WS-S1 < 4
               ADD 1                 TO WS-S1
               GO TO BP05.
             MOVE 7                  TO SHADE-ROW.
             MOVE 11                 TO SHADE-COL.
             MOVE 60                 TO SHADE-WIDTH.
             MOVE 12                 TO SHADE-LINES.
             DISPLAY S05.
             PERFORM SCREEN-SHADOW.
             DISPLAY S-ESC.
             DISPLAY S06.

       BP10.
             ACCEPT S06.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  GO TO BP990
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO BP10
               END-EVALUATE.

       BP15.
             PERFORM CLEAR-L50.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO BP25.
             MOVE 2                  TO WS-P1.

       BP20.
           IF W10-UNT(WS-P1) = ZERO
               GO TO BP22.
             MULTIPLY W10-UNT(WS-P1) BY W10-COST GIVING W10-CCOST.
           IF NOT(W10-MKUP(WS-P1) > ZERO)
               PERFORM BP50.
             MOVE W10-SL(WS-P1)      TO W10-CSELL.
           IF W10-SL(WS-P1) = ZERO
               IF (W10-CCOST NOT = ZERO) AND (W10-MKUP(WS-P1) NOT = ZERO)
                   COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST * W10-MKUP(WS-P1)) / 100)).
           IF W10-CSELL = ZERO
               MOVE W10-CCOST        TO W10-CSELL.
             MOVE W10-CSELL          TO W10-SL(WS-P1).
           IF W10-CMKUP(WS-P1) NOT > ZERO
               PERFORM BP52.
             MOVE W10-CSH(WS-P1)     TO W10-CSELL.
           IF W10-CSH(WS-P1) = ZERO
               IF (W10-CCOST NOT = ZERO) AND (W10-CMKUP(WS-P1) NOT = ZERO)
                   COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST * W10-CMKUP(WS-P1)) / 100)).
           IF W10-CSELL = ZERO
               MOVE W10-CCOST        TO W10-CSELL.
             MOVE W10-CSELL          TO W10-CSH(WS-P1).
           IF NOT(W10-WMKUP(WS-P1) > ZERO)
               PERFORM BP54.
             MOVE W10-WSL(WS-P1)     TO W10-CSELL.
           IF W10-WSL(WS-P1) = ZERO
               IF (W10-CCOST NOT = ZERO) AND (W10-WMKUP(WS-P1) NOT = ZERO)
                   COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST * W10-WMKUP(WS-P1)) / 100)).
           IF W10-CSELL = ZERO
               MOVE W10-CCOST        TO W10-CSELL.
             MOVE W10-CSELL          TO W10-WSL(WS-P1).
      *
      *    ****    C H E C K   I F   E X C L U S I V E  R O U N D I N G
      *     R E Q U I R E D
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
               MOVE W10-RSELL  TO W10-CSH(WS-P1).
           IF W10-TAX NOT = ZERO
               MOVE W05-VAT(W10-TAX) TO W05-RATE
               COMPUTE W10-VSL(WS-P1)  ROUNDED = W10-SL(WS-P1)  + (W10-SL(WS-P1)  * W05-RTE)
               COMPUTE W10-VWSL(WS-P1) ROUNDED = W10-WSL(WS-P1) + (W10-WSL(WS-P1) * W05-RTE)
               COMPUTE W10-VCSH(WS-P1) ROUNDED = W10-CSH(WS-P1) + (W10-CSH(WS-P1) * W05-RTE)
           ELSE
               MOVE W10-WSL(WS-P1)   TO W10-VWSL(WS-P1)
               MOVE W10-CSH(WS-P1)   TO W10-VCSH(WS-P1)
               MOVE W10-SL(WS-P1)    TO W10-VSL(WS-P1).
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
               MOVE W05-VAT (W10-TAX) TO W05-RATE.
             COMPUTE W10-VCSH(WS-P1) ROUNDED = W10-CSH(WS-P1) + (W10-CSH(WS-P1) * W05-RTE).
             COMPUTE W10-VWSL(WS-P1) ROUNDED = W10-WSL(WS-P1) + (W10-WSL(WS-P1) * W05-RTE).
             COMPUTE W10-VSL(WS-P1)  ROUNDED = W10-SL(WS-P1)  + (W10-SL(WS-P1)  * W05-RTE).

       BP22.
           IF WS-P1 < 4
               ADD 1                 TO WS-P1
               GO TO BP20.
             DISPLAY S06.
             PERFORM CHECK-CORRECT.
           IF NOT(WS-OPTION = "N")
               GO TO BP990.

       BP25.
             MOVE "Re-key: 'A'll,pack '1','2','3' [A]" TO WS-OPT-MES.
             MOVE "A"                TO WS-OPTION.
             MOVE "k"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "A"
               PERFORM RESTORE-SCREEN-3
               GO TO BP00.
             DISPLAY S-ESC.
           IF WS-OPTION = "2"
               GO TO BP35
           ELSE
           IF WS-OPTION = "3"
               GO TO BP40.
      *
      *    ****    P A C K   1
      *
       BP30.
             ACCEPT S06-P1.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  GO TO BP990
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO BP30
               END-EVALUATE.
             GO TO BP15.
      *
      *    ****    P A C K   2
      *
       BP35.
             ACCEPT S06-P2.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  GO TO BP990
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO BP35
               END-EVALUATE.
             GO TO BP15.
      *
      *    ****    P A C K   3
      *
       BP40.
             ACCEPT S06-P3.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  GO TO BP990
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO BP40
               END-EVALUATE.
             GO TO BP15.
      *
      *    ****  D E F A U L T   M A R K U P
      *
       BP50.
           IF (W10-SL(WS-P1) > ZERO) AND (W10-CCOST > ZERO)
               MOVE W10-SL(WS-P1)    TO W10-CSELL
               COMPUTE W10-MKUP(WS-P1) ROUNDED = (((W10-CSELL - W10-CCOST) / W10-CCOST) * 100)
           ELSE
               MOVE DPT-MKUP(WS-P1)  TO W10-MKUP(WS-P1).

       BP51.
             EXIT.
      *
      *    ****  D E F A U L T   C A S H   M A R K U P
      *
       BP52.
           IF (W10-CSH(WS-P1) > ZERO) AND (W10-CCOST > ZERO)
               MOVE W10-CSH(WS-P1)   TO W10-CSELL
               COMPUTE W10-CMKUP(WS-P1) ROUNDED = (((W10-CSELL - W10-CCOST) / W10-CCOST) * 100)
           ELSE
               MOVE DPT-CMKUP(WS-P1) TO W10-CMKUP(WS-P1).

       BP53.
             EXIT.
      *
      *    ****  D E F A U L T   W H O L E S A L E   M A R K U P
      *
       BP54.
           IF (W10-WSL(WS-P1) > ZERO) AND (W10-CCOST > ZERO)
               MOVE W10-WSL(WS-P1)   TO W10-CSELL
               COMPUTE W10-WMKUP(WS-P1) ROUNDED = (((W10-CSELL - W10-CCOST) / W10-CCOST) * 100)
           ELSE
               MOVE DPT-WMKUP(WS-P1) TO W10-WMKUP(WS-P1).

       BP55.
             EXIT.

       BP990.
             PERFORM RESTORE-SCREEN-3.

       BP999.
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
             MOVE 3                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF NOT(PAR-STOCK = "A" OR "N")
               MOVE "A"              TO PAR-STOCK.
             MOVE PAR-STOCK          TO W10-STCK.
           IF PAR-STOCK = "N"
               MOVE PAR-SLNGTH       TO W10-SLNGTH
           ELSE
               MOVE 14               TO W10-SLNGTH.
           IF W10-SLNGTH < 3
               MOVE 3                TO W10-SLNGTH
           ELSE
           IF W10-SLNGTH > 14
               MOVE 14               TO W10-SLNGTH.
             MOVE PAR-CASES          TO WS-USE-CASES.
             MOVE PAR-PACKS          TO WS-USE-PACKS.
             MOVE PAR-USE-ITM        TO WS-USE-ITM.
             MOVE PAR-EXT-STK        TO WS-EXT-STK.

       ZA01B.
             OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
             MOVE 1                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      *      MOVE PAR-DMY            TO W12-TODAY.
      *      MOVE PAR-YMD            TO W12-T-YMD.
             MOVE PAR-COMPANY        TO W95-COMP.
             MOVE 3                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE STK-WEEK           TO WS-WEEK.
             MOVE STK-RECORDS        TO WS-RECORDS.
             MOVE STK-PASSWORD       TO WS-PASSWORD.
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
             MOVE PAR-PW             TO W85-PASS.
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
             MOVE 6                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF PAR-INTEGRATE NOT = "Y"
               MOVE "N"              TO WX-INT
           ELSE
               MOVE "Y"              TO WX-INT.
             MOVE PAR-DEBGL          TO WX-DEBGL.
             MOVE PAR-CREGL          TO WX-CREGL.
             MOVE PAR-INTGL          TO WX-INTGL.
             MOVE PAR-BNKGL          TO WX-BNKGL.
             MOVE PAR-UNPROF         TO WX-UNPROF.
             MOVE PAR-REDGL          TO WX-REDGL.
             MOVE PAR-ADJGL          TO WX-ADJGL.
             MOVE PAR-RLGL           TO WX-RLGL.
             MOVE PAR-DSGL           TO WX-DSGL.
             MOVE 8                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF NOT(PAR-DESC-H1 = X"0000000000000000000000000000")
               MOVE PAR-DESC-H1      TO WS-DESC-H1.
           IF NOT(PAR-DESC-H2 = X"0000000000000000000000000000")
               MOVE PAR-DESC-H2      TO WS-DESC-H2.
           IF NOT(PAR-WS-HD = X"00000000000000000000")
               MOVE PAR-WS-HD        TO WS-WS-HD.
           IF NOT(PAR-CS-HD = X"00000000000000000000")
               MOVE PAR-CS-HD        TO WS-CS-HD.
           IF NOT(PAR-RT-HD = X"00000000000000000000")
               MOVE PAR-RT-HD        TO WS-RT-HD.
             GO TO ZA999-EXIT.

       COPY "ZA49.PRO".

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
               WHEN 2          MOVE W02-NETWORK TO WS-FILE
                               MOVE WS-NETKEY   TO WS-KEY
               WHEN 7          MOVE W02-DEPART  TO WS-FILE
                               MOVE DPT-CODE    TO WS-KEYX
               WHEN 15         MOVE WS-PARID    TO WS-FILE
                               MOVE WS-PARKEY   TO WS-KEY
               WHEN 18         MOVE W02-RECOVER TO WS-FILE
                               MOVE WS-RECKEY   TO WS-KEY
               WHEN 22         MOVE W02-STOCKF  TO WS-FILE
                               MOVE STK-CODE    TO WS-KEYX
               WHEN 37         MOVE W02-SHARED  TO WS-FILE
                               MOVE WS-SHARED   TO WS-KEY
               WHEN 40         MOVE W02-LEDTRF  TO WS-FILE
                               MOVE XFR-KEY     TO WS-KEYX
               WHEN OTHER      MOVE "ERROR"     TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
