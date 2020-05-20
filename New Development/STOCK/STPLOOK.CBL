      $set LINKCOUNT"640" GNT"STPLOOK.GNT"
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
      *  ******  ******** *******  **        ******   ******  **   **  *
      * **    **    **   **     ** **       **    ** **    ** **  **   *
      * **          **   **     ** **       **    ** **    ** ** **    *
      *  ******     **   *******   **       **    ** **    ** ****     *
      *       **    **   **        **       **    ** **    ** ** **    *
      * **    **    **   **        **       **    ** **    ** **  **   *
      *  ******     **   **        ********  ******   ******  **   **  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S T O C K   L O O K - U P S                                *
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
       PROGRAM-ID.     STPLOOK.
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

       COPY "APACFIDS.SL".

       COPY "CREDIT.SL".

       COPY "DEPART.SL".

       COPY "PARAM.SL".

       COPY "SPARTS.SL".

       COPY "STKALT.SL".

       COPY "STOCK.SL".

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

       COPY "CREDIT.FDE".

       COPY "DEPART.FDE".

       COPY "PARAM.FDE".

       COPY "SPARTS.FDE".

       COPY "STKALT.FDE".

       COPY "STOCK.FDE".

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
       77  WS-CHECK                PIC  X(18)           VALUE "aeWlimemnomLalismJ".
       77  WS-P1                   PIC  9(02)    COMP-5.
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S5                   PIC  9(04)    COMP-5.
       77  WS-S6                   PIC  9(04)    COMP-5.
       77  WS-S7                   PIC  9(04)    COMP-5.
       77  WS-S8                   PIC  9(04)    COMP-5.
       77  WS-S9                   PIC  9(04)    COMP-5.
       77  WS-IXS                  PIC  9(04)    COMP-5.
       77  WS-IXS1                 PIC  9(04)    COMP-5.
       77  WS-IXS2                 PIC  9(04)    COMP-5.
       77  WS-IXS3                 PIC  9(04)    COMP-5.
       77  WS-IXS4                 PIC  9(04)    COMP-5.
       77  WS-NETKEY               PIC  9(06)    COMP-5.
       77  WS-PARKEY               PIC  9(06)    COMP-5.
       77  WS-USE-CASES            PIC  X(01).
       77  WS-USE-PACKS            PIC  X(01).
       77  WS-USE-ITM              PIC  X(01).
       77  WS-EXT-STK              PIC  X(01).
       77  WS-PRICE                PIC  X(01).
       77  WS-OPTION               PIC  X(01).
       77  WS-WSTOCK               PIC  X(01).
       77  WS-WH-QUANT             PIC S9(08)V999 COMP-3.
       77  WS-NODEP                PIC  X(30)    VALUE "NO SUCH   D E P A R T M E N T".
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
      *         4 = Clear line 46 and line 50
      *         C =  Clear comment line (50)
      *         E =  Clear lines (any line or lines between 5 and 46)
      *         H =  Change heading
      *         S =  Clear the screen and display headings
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STPLOOK".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY -LOOKUP".
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
           03  WS-SYS-ID             PIC  X(03).

       COPY "W05.VAT".

       01  W09-STOCK.
           03  W09-S-MARKUPS.
               05  W09-MARKUP        PIC S9(03)V9999  COMP-3.
               05  W09-CMARKUP       PIC S9(03)V9999  COMP-3.
               05  W09-WMARKUP       PIC S9(03)V9999  COMP-3.
               05  W09-MARKUP-2      PIC S9(03)V9999  COMP-3.
               05  W09-CMARKUP-2     PIC S9(03)V9999  COMP-3.
               05  W09-WMARKUP-2     PIC S9(03)V9999  COMP-3.
               05  W09-MARKUP-3      PIC S9(03)V9999  COMP-3.
               05  W09-CMARKUP-3     PIC S9(03)V9999  COMP-3.
               05  W09-WMARKUP-3     PIC S9(03)V9999  COMP-3.
               05  W09-MARKUP-4      PIC S9(03)V9999  COMP-3.
               05  W09-CMARKUP-4     PIC S9(03)V9999  COMP-3.
               05  W09-WMARKUP-4     PIC S9(03)V9999  COMP-3.
           03  W09-ST-MARKUPS REDEFINES W09-S-MARKUPS.
               05  W09-STK-MARK OCCURS 4.
                   07  W09-MKUP      PIC S9(03)V9999  COMP-3.
                   07  W09-CMKUP     PIC S9(03)V9999  COMP-3.
                   07  W09-WMKUP     PIC S9(03)V9999  COMP-3.

       01  W10.
           03  W10-ALT-DISC          PIC  X(01).

       COPY "W20.WS".

       01  W30.
           03  W30-ITEM.
               05  W30-CODE          PIC  X(14).
               05  FILLER            PIC  X(04).
           03  W30-ITM-CODE REDEFINES W30-ITEM PIC  X(14)/X(03).
           03  W30-EXT-CODE REDEFINES W30-ITEM PIC  X(18).

       COPY "W40.WS".
      *
      *    ****    U S E D   F O R   W O R D   L O O K U P
      *
       01  W40-DESCRIPTION.
           03  W40-ADESC.
               05  W40-ALT-LUP.
                   07  W40-ALT-CHAR  PIC X(01) OCCURS 12.
               05  FILLER            PIC  X(18).
           03  W40-ALT-KEY.
               05  W40-DESC-CH       PIC X(01) OCCURS 12.

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W75-KEYS.
           03  W75-P                 PIC  9(02) COMP-5.
           03  W75-S                 PIC  9(02) COMP-5.
           03  W75-S1                PIC  9(02) COMP-5.
           03  W75-S2                PIC  9(02) COMP-5.
           03  W75-PACK              PIC  9(02) COMP-5.
           03  W75-ITEMS OCCURS 36.
               05  W75-EXT-CODE.
                   07  W75-CODE      PIC  X(14).
                   07  W75-ITM       PIC  X(03).
                   07  FILLER        PIC  X(01).
               05  W75-PRICES.
                   07  W75-PER       PIC  X(08).
                   07  W75-SELL      PIC  9(08)V999 COMP-3.
                   07  W75-VSELL     PIC  9(09)V99  COMP-3.
                   07  W75-CASH      PIC  9(08)V999 COMP-3.
                   07  W75-VCASH     PIC  9(09)V99  COMP-3.
                   07  W75-WSALE     PIC  9(08)V999 COMP-3.
                   07  W75-VWSALE    PIC  9(09)V99  COMP-3.
                   07  W75-PER-2     PIC  X(08).
                   07  W75-SELL-2    PIC  9(08)V999 COMP-3.
                   07  W75-VSELL-2   PIC  9(09)V99  COMP-3.
                   07  W75-CASH-2    PIC  9(08)V999 COMP-3.
                   07  W75-VCASH-2   PIC  9(09)V99  COMP-3.
                   07  W75-WSALE-2   PIC  9(08)V999 COMP-3.
                   07  W75-VWSALE-2  PIC  9(09)V99  COMP-3.
                   07  W75-PER-3     PIC  X(08).
                   07  W75-SELL-3    PIC  9(08)V999 COMP-3.
                   07  W75-VSELL-3   PIC  9(09)V99  COMP-3.
                   07  W75-CASH-3    PIC  9(08)V999 COMP-3.
                   07  W75-VCASH-3   PIC  9(09)V99  COMP-3.
                   07  W75-WSALE-3   PIC  9(08)V999 COMP-3.
                   07  W75-VWSALE-3  PIC  9(09)V99  COMP-3.
                   07  W75-PER-4     PIC  X(08).
                   07  W75-SELL-4    PIC  9(08)V999 COMP-3.
                   07  W75-VSELL-4   PIC  9(09)V99  COMP-3.
                   07  W75-CASH-4    PIC  9(08)V999 COMP-3.
                   07  W75-VCASH-4   PIC  9(09)V99  COMP-3.
                   07  W75-WSALE-4   PIC  9(08)V999 COMP-3.
                   07  W75-VWSALE-4  PIC  9(09)V99  COMP-3.
               05  W75-DSP-PRICES    REDEFINES W75-PRICES.
                   07  W75-DSP-PRC   OCCURS 4.
                       09  W75-PR    PIC  X(08).
                       09  W75-SL    PIC  9(08)V999 COMP-3.
                       09  W75-VSL   PIC  9(09)V99  COMP-3.
                       09  W75-CSH   PIC  9(08)V999 COMP-3.
                       09  W75-VCSH  PIC  9(09)V99  COMP-3.
                       09  W75-WSL   PIC  9(08)V999 COMP-3.
                       09  W75-VWSL  PIC  9(09)V99  COMP-3.
           03  W75-SLEDG             PIC  X(04) OCCURS 10.

       01  W100-EDIT.
           03  W100-V2.
               05  W100-S6V2         PIC  Z(05)9.99-.
           03  W100-V3               REDEFINES W100-V2.
               05  W100-PRICE        PIC  X(09).
               05  FILLER            PIC  X(01).
           03  W100-V5               REDEFINES W100-V2.
               05  W100-QNT          PIC  Z(05)9.99.
               05  FILLER            PIC  X(01).
           03  W100-V6               REDEFINES W100-V2.
               05  W100-NQNT         PIC  -----9.99.
               05  FILLER            PIC  X(01).
           03  W100-V7               REDEFINES W100-V2.
               05  W100-LQNT         PIC  Z(06)9.9.
               05  FILLER            PIC  X(01).
           03  W100-V8               REDEFINES W100-V2.
               05  W100-QUANT        PIC  X(09).
               05  FILLER            PIC  X(01).
           03  W100-Q1.
               05  W100-S7V3         PIC Z(06)9.999-.

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

       COPY "W10.STK".

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

       COPY "STOCK.CRT".

       01  S04C.
           03  LINE  2 COLUMN 34 VALUE "STOCK ENQUIRY" FOREGROUND-COLOR Grey HIGHLIGHT.

       COPY "STKLUP.CRT".

       01  S15.
           03  BACKGROUND-COLOR Grey.
           03  LINE  7 COLUMN 11 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" FOREGROUND-COLOR Black.
           03          COLUMN 70 VALUE "¿"                                                           FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE  8 COLUMN 11 VALUE "³"                                                           FOREGROUND-COLOR Black.
           03          COLUMN 12 VALUE "          P A C K S :   S A L E S   D E T A I L S          " FOREGROUND-COLOR Grey HIGHLIGHT.
           03          COLUMN 70 VALUE "³"                                                           FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE  9 COLUMN 11 VALUE "Ã"                                                           FOREGROUND-COLOR Black.
           03          COLUMN 12 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´Pack 1ÃÄÄÄÄÄ´Pack 2ÃÄÄÄÄÄ´Pack 3ÃÄÄÄ"  FOREGROUND-COLOR Red.
           03          COLUMN 70 VALUE "´"                                                           FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE 10 COLUMN 11 VALUE "³"                                                           FOREGROUND-COLOR Black.
           03          COLUMN 12 VALUE " Priced per        "                                         FOREGROUND-COLOR Red HIGHLIGHT.
           03          COLUMN 31 VALUE "                                       "                     FOREGROUND-COLOR Red.
           03          COLUMN 70 VALUE "³"                                                           FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE 11 COLUMN 11 VALUE "³"                                                           FOREGROUND-COLOR Black.
           03          COLUMN 12 VALUE " Units per pack    "                                         FOREGROUND-COLOR Red HIGHLIGHT.
           03          COLUMN 31 VALUE "                                       "                     FOREGROUND-COLOR Red.
           03          COLUMN 70 VALUE "³"                                                           FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE 12 COLUMN 11 VALUE "³"                                                           FOREGROUND-COLOR Black.
           03          COLUMN 12 VALUE " Markup "                                                    FOREGROUND-COLOR Red HIGHLIGHT.
           03          COLUMN 20 PIC  X(11) FROM WS-RT-HD                                            FOREGROUND-COLOR Red HIGHLIGHT.
           03          COLUMN 31 VALUE "                                       "                     FOREGROUND-COLOR Red.
           03          COLUMN 70 VALUE "³"                                                           FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE 13 COLUMN 11 VALUE "³"                                                           FOREGROUND-COLOR Black.
           03          COLUMN 12 VALUE "        "                                                    FOREGROUND-COLOR Red HIGHLIGHT.
           03          COLUMN 20 PIC  X(11) FROM WS-CS-HD                                            FOREGROUND-COLOR Red HIGHLIGHT.
           03          COLUMN 31 VALUE "                                       "                     FOREGROUND-COLOR Red.
           03          COLUMN 70 VALUE "³"                                                           FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE 14 COLUMN 11 VALUE "³"                                                           FOREGROUND-COLOR Black.
           03          COLUMN 12 VALUE "        "                                                    FOREGROUND-COLOR Red HIGHLIGHT.
           03          COLUMN 20 PIC  X(11) FROM WS-WS-HD                                            FOREGROUND-COLOR Red HIGHLIGHT.
           03          COLUMN 31 VALUE "                                       "                     FOREGROUND-COLOR Red.
           03          COLUMN 70 VALUE "³"                                                           FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE 15 COLUMN 11 VALUE "³"                                                           FOREGROUND-COLOR Black.
           03          COLUMN 12 VALUE " Price  "                                                    FOREGROUND-COLOR Red HIGHLIGHT.
           03          COLUMN 20 PIC  X(11) FROM WS-RT-HD                                            FOREGROUND-COLOR Red HIGHLIGHT.
           03          COLUMN 31 VALUE "                                       "                     FOREGROUND-COLOR Red.
           03          COLUMN 70 VALUE "³"                                                           FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE 16 COLUMN 11 VALUE "³"                                                           FOREGROUND-COLOR Black.
           03          COLUMN 12 VALUE "        "                                                    FOREGROUND-COLOR Red HIGHLIGHT.
           03          COLUMN 20 PIC  X(11) FROM WS-CS-HD                                            FOREGROUND-COLOR Red HIGHLIGHT.
           03          COLUMN 31 VALUE "                                       "                     FOREGROUND-COLOR Red.
           03          COLUMN 70 VALUE "³"                                                           FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE 17 COLUMN 11 VALUE "³"                                                           FOREGROUND-COLOR Black.
           03          COLUMN 12 VALUE "        "                                                    FOREGROUND-COLOR Red HIGHLIGHT.
           03          COLUMN 20 PIC  X(11) FROM WS-WS-HD                                            FOREGROUND-COLOR Red HIGHLIGHT.
           03          COLUMN 31 VALUE "                                       "                     FOREGROUND-COLOR Red.
           03          COLUMN 70 VALUE "³"                                                           FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE 18 COLUMN 11 VALUE "À"                                                           FOREGROUND-COLOR Black.
           03          COLUMN 12 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S16.
           03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.
               05  S16-P1.
                   07  LINE 10 COLUMN 34 PIC X(08)       USING W10-PER-2 AUTO.
                   07  LINE 11 COLUMN 34 PIC ZZZ9.999    USING W10-UNIT-2 AUTO.
                   07  LINE 12 COLUMN 36 PIC ZZ9.99      USING W09-MARKUP-2 AUTO.
                   07  LINE 13 COLUMN 36 PIC ZZ9.99      USING W09-CMARKUP-2 AUTO.
                   07  LINE 14 COLUMN 36 PIC ZZ9.99      USING W09-WMARKUP-2 AUTO.
                   07  LINE 15 COLUMN 31 PIC ZZZZZZZ9.99 USING W10-SELL-2 AUTO.
                   07  LINE 16 COLUMN 31 PIC ZZZZZZZ9.99 USING W10-CASH-2 AUTO.
                   07  LINE 17 COLUMN 31 PIC ZZZZZZZ9.99 USING W10-WSALE-2 AUTO.
               05  S16-P2.
                   07  LINE 10 COLUMN 47 PIC X(08)       USING W10-PER-3 AUTO.
                   07  LINE 11 COLUMN 47 PIC ZZZ9.999    USING W10-UNIT-3 AUTO.
                   07  LINE 12 COLUMN 49 PIC ZZ9.99      USING W09-MARKUP-3 AUTO.
                   07  LINE 13 COLUMN 49 PIC ZZ9.99      USING W09-CMARKUP-3 AUTO.
                   07  LINE 14 COLUMN 49 PIC ZZ9.99      USING W09-WMARKUP-3 AUTO.
                   07  LINE 15 COLUMN 44 PIC ZZZZZZZ9.99 USING W10-SELL-3 AUTO.
                   07  LINE 16 COLUMN 44 PIC ZZZZZZZ9.99 USING W10-CASH-3 AUTO.
                   07  LINE 17 COLUMN 44 PIC ZZZZZZZ9.99 USING W10-WSALE-3 AUTO.
               05  S16-P3.
                   07  LINE 10 COLUMN 60 PIC X(08)       USING W10-PER-4 AUTO.
                   07  LINE 11 COLUMN 60 PIC ZZZ9.999    USING W10-UNIT-4 AUTO.
                   07  LINE 12 COLUMN 62 PIC ZZ9.99      USING W09-MARKUP-4 AUTO.
                   07  LINE 13 COLUMN 62 PIC ZZ9.99      USING W09-CMARKUP-4 AUTO.
                   07  LINE 14 COLUMN 62 PIC ZZ9.99      USING W09-WMARKUP-4 AUTO.
                   07  LINE 15 COLUMN 57 PIC ZZZZZZZ9.99 USING W10-SELL-4 AUTO.
                   07  LINE 16 COLUMN 57 PIC ZZZZZZZ9.99 USING W10-CASH-4 AUTO.
                   07  LINE 17 COLUMN 57 PIC ZZZZZZZ9.99 USING W10-WSALE-4 AUTO.

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
           IF W10-ETYPE = "W"
               PERFORM STOCK-A-LOOKUP
           ELSE
               PERFORM STOCK-LOOKUP.

       AA49.
           IF WS-WSTOCK = "Y"
               CLOSE WSTOCK
               MOVE "N"              TO WS-WSTOCK.
             EXIT PROGRAM.
      *
      *    ****    D I S P L A Y   D E P A R T M E N T  N A M E
      *
       AA400              SECTION.
       AA401.
             MOVE W10-LEDG           TO DPT-CODE.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
           IF (WS-F-ERROR = 7) OR (DPT-DESC = SPACES)
               MOVE WS-NODEP         TO W10-DEPT
           ELSE
               MOVE DPT-DESC         TO W10-DEPT.
             DISPLAY W10-DEPT AT 0623 WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Magenta.
       AA405.
             EXIT.

       COPY "AA900.ALM".

      *           T H I S   R O U T I N E   I S   U S E D   T O
      *            D I S P L A Y   E R R O R   M E S S A G E S
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                      ERROR-MESSAGE                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To display the error message higher or lower (default is  ³
      *    ³ line 20) Move the line number which must be used as the   ³
      *    ³ top line to WS-MES-LINE. The message to be displayed must ³
      *    ³ be in WS-ERR-MES. PERFORM ERROR-MESSAGE.                  ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³                  ERROR-MESSAGE (2 LINES)                  ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ See above for details of the first line and the second    ³
      *    ³ line of the message must be in WS-ERR-MES-2.              ³
      *    ³               PERFORM ERROR-MESSAGE-2.                    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "ERROR.SCR".

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
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.SCR".

       COPY "LOCKED.RC2".

       COPY "DATE.CNV".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                        STOCK-MEMO                         ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ The item code  of  the  Stock  record  for which the MEMO ³
      *    ³ details are to be displayed must be in W10-EXT-ITEM.      ³
      *    ³                    PERFORM STOCK-MEMO.                    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "STKMEM.PRO".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
        AC000-READ        SECTION.

       COPY "CREDIT.RD".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "STKMEM.RD".

       COPY "STKALT.RD".

       COPY "STOCK.RD".

       COPY "SPARTS.RD".

       COPY "WSTOCK.RD".

       COPY "CA000X.STK".

       COPY "CA100.STK".

       COPY "CA200.STK".

       COPY "CA500.STK".

      *
      *    ****    S T O C K   L O O K U P   R O U T I N E
      *
       STOCK-LOOKUP    SECTION.
       STOCK-LUP-SCREEN.
             PERFORM ERASE-SCREEN.
             DISPLAY S-STK.
             DISPLAY WS-RT-HD AT 0654 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY "Single" AT 0668 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE 1                  TO WS-PRICE.
             EVALUATE W10-ETYPE
               WHEN "A"          DISPLAY S-STKA
               WHEN "X"          DISPLAY S-STKC
               WHEN "D"          DISPLAY S-STKD
               WHEN "2"          DISPLAY S-STKD2
             END-EVALUATE.
             MOVE SPACE              TO WS-OPTION.

       STOCK-LUP-CLR-SCREEN.
             MOVE 0901               TO CPOS.
             PERFORM STOCK-CRT-EDGE UNTIL CLIN = 45.
             MOVE SPACES             TO W10-ACODE W10-DESC W10-DESC2 W10-EXT-CODE2 W10-EXT-CODE1.

       STOCK-LUP-DATA.
             EVALUATE W10-ETYPE
               WHEN "I"          ACCEPT SA-STK
               WHEN "A"          ACCEPT SA-STKA
               WHEN "X"          ACCEPT SA-STKC
               WHEN "D"          ACCEPT SA-STKD
               WHEN "2"          ACCEPT SA-STKD2
             END-EVALUATE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE SPACES TO W10-EXT-ITEM
                                 GO TO STOCK-LOOKUP-EXIT
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO STOCK-LUP-DATA.

       STOCK-START-LOOKUP.
             PERFORM CLEAR-L46-AND-50.
             MOVE 9                  TO CLIN.
             MOVE 1                  TO W75-S W75-S1 WS-P1 W75-P.

       STOCK-LUP-TAB-LOOP.
             MOVE SPACES             TO W75-EXT-CODE(W75-S1).
           IF W75-S1 < 36
               ADD 1                 TO W75-S1
               GO TO STOCK-LUP-TAB-LOOP.
             MOVE ZERO               TO W75-S1.

       STOCK-LUP-ENTRY-TYPE.
             EVALUATE W10-ETYPE
               WHEN "I"
               WHEN "Z"          IF W10-EXT-ITEM = SPACES
                                     MOVE X"1F"      TO W10-EXT-ITEM
                                 END-IF
                                 PERFORM CA00 THRU CA06
                                 MOVE W10-EXT-ITEM   TO STK-CODE
                                 PERFORM START-AT-STOCK-CODE THRU READ-STOCK-EXIT
                                     
               WHEN "A"          IF W10-ACODE = SPACES
                                     MOVE X"1F"      TO W10-ACODE
                                 END-IF
                                 PERFORM CA101 THRU CA105
                                 MOVE W10-ACODE      TO STK-ACODE
                                 MOVE SPACES         TO STK-CODE
                                 PERFORM START-AT-ALT-CODE THRU READ-STOCK-EXIT

               WHEN "X"          IF W10-EXT-CODE2 = SPACES
                                     MOVE X"1F"      TO W10-EXT-CODE2
                                 END-IF
                                 PERFORM CA201 THRU CA205
                                 MOVE W10-EXT-CODE2  TO STK-XREF
                                 PERFORM START-AT-STOCK-XREF THRU READ-STOCK-EXIT
                                 
               WHEN "D"          IF W10-DESC = SPACES
                                     MOVE X"1F"      TO W10-DESC
                                 END-IF
                                 MOVE W10-DESC       TO W50-DATA
                                 MOVE 30             TO W50-LENGTH
                                 CALL "CBL_TOUPPER" USING W50-30 BY VALUE W50-LENGTH RETURNING WS-STATUS
                                 MOVE W50-30         TO W10-DESC
                                 MOVE W10-DESC       TO STK-DESC
                                 PERFORM START-AT-STOCK-DESC THRU READ-STOCK-EXIT
                                 
               WHEN "2"          IF W10-DESC2 = SPACES
                                     MOVE X"1F"      TO W10-DESC2
                                 END-IF
                                 MOVE W10-DESC2      TO W50-DATA
                                 MOVE 30             TO W50-LENGTH
                                 CALL "CBL_TOUPPER" USING W50-30 BY VALUE W50-LENGTH RETURNING WS-STATUS
                                 MOVE W50-30         TO W10-DESC2
                                 MOVE W10-DESC2      TO STK-DESC2
                                 PERFORM START-AT-STOCK-DESC2 THRU READ-STOCK-EXIT
             END-EVALUATE.
           IF WS-F-ERROR = 22
               GO TO STOCK-NO-RECORD.

       STOCK-GET-STOCK-REC.
           IF WS-OPTION = "J"
               MOVE SPACE            TO WS-OPTION
               GO TO STOCK-SKIP-STOCK-READ.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               SUBTRACT 1            FROM W75-S
      *
      *    ****   U S E R   O P T I O N S
      *
               PERFORM STOCK-FULL-SCREEN
               GO TO STOCK-POSITION-CURSOR.
      *
      *    ****   S T O R E   S T O C K   R E C O R D   P R I C E S
      *
       STOCK-SKIP-STOCK-READ.
             MOVE STK-CODE           TO W75-EXT-CODE(W75-S).
             MOVE 1                  TO W75-PACK.

       STOCK-PRICES-LOOP.
             MOVE STK-PR(W75-PACK)   TO W75-PR(W75-S, W75-PACK).
             MOVE STK-SL(W75-PACK)   TO W75-SL(W75-S, W75-PACK).
             MOVE STK-CSH(W75-PACK)  TO W75-CSH(W75-S, W75-PACK).
             MOVE STK-WSL(W75-PACK)  TO W75-WSL(W75-S, W75-PACK).
           IF STK-TAX NOT = ZERO
               MOVE W05-VAT (STK-TAX)  TO W05-RATE
               COMPUTE W75-VSL(W75-S, W75-PACK) ROUNDED = W75-SL(W75-S, W75-PACK) + (W75-SL(W75-S, W75-PACK) * W05-RTE)
               COMPUTE W75-VCSH(W75-S, W75-PACK) ROUNDED = W75-CSH(W75-S, W75-PACK) + (W75-CSH(W75-S, W75-PACK) * W05-RTE)
               COMPUTE W75-VWSL(W75-S, W75-PACK) ROUNDED = W75-WSL(W75-S, W75-PACK) + (W75-WSL(W75-S, W75-PACK) * W05-RTE)
           ELSE
               MOVE W75-SL(W75-S, W75-PACK)  TO W75-VSL(W75-S, W75-PACK)
               MOVE W75-CSH(W75-S, W75-PACK) TO W75-VCSH(W75-S, W75-PACK)
               MOVE W75-WSL(W75-S, W75-PACK) TO W75-VWSL(W75-S, W75-PACK).
      *
      *    ****    H A V E   A L L   P R I C E S   B E E N
      *            S T O R E D ?
      *
           IF W75-PACK < 4
               ADD 1                 TO W75-PACK
               GO TO STOCK-PRICES-LOOP.
      *
      *    ****    U S E   D I F F E R E N T   C O L O U R S   F O R
      *            E A C H   S T O C K   T Y P E
      *
       STOCK-FB-COL.
             EVALUATE STK-IND
      *               C O N T R O L E D
               WHEN 0            MOVE "HIGHLIGHT FOREGROUND-COLOR Cyan"    TO  WS-CONT
      *               P R I C E D   I T E M
               WHEN 1            MOVE "HIGHLIGHT FOREGROUND-COLOR Red"     TO  WS-CONT
      *               B U Y - O U T
               WHEN 2            MOVE "HIGHLIGHT FOREGROUND-COLOR Grey"    TO  WS-CONT
      *               H E A D I N G
               WHEN 4            MOVE "HIGHLIGHT FOREGROUND-COLOR Green"   TO  WS-CONT
      *               L A B O U R
               WHEN OTHER        MOVE "HIGHLIGHT FOREGROUND-COLOR Magenta" TO  WS-CONT.

       STOCK-DISP-DET.
           IF WS-USE-ITM = "Y"
               MOVE STK-CODE         TO W30-ITM-CODE
           ELSE
           IF WS-EXT-STK = "Y"
               MOVE STK-CODE         TO W30-EXT-CODE
           ELSE
               MOVE STK-ITEM         TO W30-ITEM.
             DISPLAY STK-CODE AT LINE CLIN COLUMN 2 WITH CONTROL WS-CONT.
             MOVE STK-DESC           TO W10-DESC.
             DISPLAY W10-SDSC AT LINE CLIN COLUMN 21 WITH CONTROL WS-CONT.
           IF STK-QUANT < ZERO
               MOVE STK-QUANT        TO W100-NQNT
           ELSE
           IF STK-QUANT > 999999.99
               MOVE STK-QUANT        TO W100-LQNT
           ELSE
               MOVE STK-QUANT        TO W100-QNT.
           IF NOT(STK-IND = 4)
               DISPLAY W100-QUANT AT LINE CLIN COLUMN 51 WITH CONTROL WS-CONT
               MOVE W75-SL(W75-S, W75-P)   TO W100-S6V2
               DISPLAY W100-PRICE AT LINE CLIN COLUMN 61 WITH CONTROL WS-CONT
               MOVE W75-VSL(W75-S, W75-P)  TO W100-S6V2
               DISPLAY W100-PRICE AT LINE CLIN COLUMN 71 WITH CONTROL WS-CONT.
           IF CLIN < 44
               ADD 1                 TO CLIN W75-S
               GO TO STOCK-GET-STOCK-REC.
      *
      *    ****   U S E R   O P T I O N S
      *
       STOCK-FULL-SCREEN.
            DISPLAY ""                                                                 AT 4602 WITH FOREGROUND-COLOR Brown HIGHLIGHT
                      X"1B1A"                                                                        WITH FOREGROUND-COLOR Brown HIGHLIGHT
                         ","                                                                         WITH FOREGROUND-COLOR Grey
                          "PgUp"                                                                     WITH FOREGROUND-COLOR Brown HIGHLIGHT
                              ","                                                                    WITH FOREGROUND-COLOR Grey
                               "PgDn"                                                                WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                   ","                                                               WITH FOREGROUND-COLOR Grey
                                    "ENTER"                                                          WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                         ","                                                         WITH FOREGROUND-COLOR Grey
                                          "Esc"                                                      WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                             ","                                                     WITH FOREGROUND-COLOR Grey
                                              "F2"                                                   WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                                "=View,"                                             WITH FOREGROUND-COLOR Grey
                                                      "F5"                                           WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                                        "=Packs,"                                    WITH FOREGROUND-COLOR Grey
                                                               "F6"                                  WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                                                 "=Dsc2,"                            WITH FOREGROUND-COLOR Grey
                                                                       "F7"                          WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                                                         "=Bin,"                     WITH FOREGROUND-COLOR Grey
                                                                              "F8"                   WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                                                                "=Xref/alt,"         WITH FOREGROUND-COLOR Grey
                                                                                          "F9"       WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                                                                            "=remks" WITH FOREGROUND-COLOR Grey.
             DISPLAY "1"                       AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                      " "                              WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                        WS-RT-HD                       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                               ","                     WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                "2"                    WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                 " "                   WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                   WS-CS-HD            WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                          ","          WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                           "3"         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                            " "        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                              WS-WS-HD WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.

       STOCK-POSITION-CURSOR.
             PERFORM SAVE-SCREEN-2.
             MOVE 0                  TO W75-S1.
             MOVE 0901               TO CPOS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³               Restore the screen attributes.              ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       STOCK-CRT-ATTRIB.
             MOVE 3920               TO W41-LENGTH.
             CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS W42-ATTRIB2 W41-LENGTH RETURNING WS-STATUS.
             MOVE 4000               TO W41-LENGTH.

       STOCK-GET-USER-OPT.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             DISPLAY SPACE AT 5040 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     GO TO STOCK-ADJ-NO-KEYS
                 WHEN DOWN-KEY   GO TO STOCK-CHK-NO-KEYS
                 WHEN LEFT-KEY   IF W75-P > 1
                                     SUBTRACT 1  FROM W75-P
                                     GO TO STOCK-CHK-PACKS
                 WHEN RIGHT-KEY  IF W75-P < 4
                                     ADD 1      TO W75-P
                                     GO TO STOCK-CHK-PACKS
                 WHEN ENTER-KEY
                 WHEN CR-KEY     GO TO STOCK-CHK-SELECT
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO STOCK-GET-USER-OPT
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE SPACES     TO W10-EXT-ITEM
                                 GO TO STOCK-LOOKUP-EXIT
                 WHEN PAGE-DOWN  GO TO STOCK-GET-NEXT-PAGE
                 WHEN PAGE-UP    GO TO STOCK-GET-PREV-PAGE
                 WHEN F2-KEY     PERFORM BA000
                                 GO TO STOCK-GET-USER-OPT
                 WHEN F5-KEY     PERFORM BB000
                                 GO TO STOCK-GET-USER-OPT
                 WHEN F6-KEY     MOVE WS1-CURS   TO CRT-LINE
                                 PERFORM STOCK-DESC2 THRU STOCK-DESC2-EXIT
                                 MOVE CRT-LINE   TO WS1-CURS
                                 GO TO STOCK-GET-USER-OPT
                 WHEN F7-KEY     MOVE WS1-CURS   TO CRT-LINE
                                 PERFORM STOCK-BIN THRU STOCK-BIN-EXIT
                                 MOVE CRT-LINE   TO WS1-CURS
                                 GO TO STOCK-GET-USER-OPT
                 WHEN F8-KEY     MOVE WS1-CURS   TO CRT-LINE
                                 PERFORM STOCK-CODES THRU STOCK-CODES-EXIT
                                 MOVE CRT-LINE   TO WS1-CURS
                                 GO TO STOCK-GET-USER-OPT
                 WHEN F9-KEY     PERFORM STOCK-MEM
      *
      *    ****   U S E R   O P T I O N S
      *
                                 PERFORM STOCK-FULL-SCREEN
                                 GO TO STOCK-GET-USER-OPT
                 WHEN OTHER  PERFORM AA900-ALARM
               END-EVALUATE
               GO TO STOCK-GET-USER-OPT
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X      TO WS-OPTION
               GO TO STOCK-CHK-PRICES.
             PERFORM AA900-ALARM.
             GO TO STOCK-GET-USER-OPT.

       STOCK-CHK-PACKS.
             EVALUATE W75-P
                 WHEN 1     DISPLAY "Single" AT 0668 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                 WHEN 2     DISPLAY "Pack 1" AT 0668 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                 WHEN 3     DISPLAY "Pack 2" AT 0668 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                 WHEN 4     DISPLAY "Pack 3" AT 0668 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
             END-EVALUATE.
             GO TO STOCK-DISP-PRICES.

       STOCK-CHK-PRICES.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-OPTION = "1" OR "2" OR "3")
               GO TO STOCK-GET-USER-OPT.
             MOVE WS-OPTION          TO WS-PRICE.

       STOCK-DISP-PRICES.
             EVALUATE WS-PRICE
                 WHEN "2"   GO TO STOCK-CASH-PRICES
                 WHEN "1"   GO TO STOCK-RETAIL-PRICES
                 WHEN "3"   GO TO STOCK-WSALE-PRICES.
                 WHEN OTHER PERFORM AA900-ALARM
             END-EVALUATE.
             GO TO STOCK-GET-USER-OPT.

       STOCK-GET-NEXT-PAGE.
           IF W75-S < 16
               GO TO STOCK-GET-USER-OPT.

       STOCK-NEW-PAGE.
             MOVE 0901               TO CPOS.
             PERFORM STOCK-CRT-EDGE UNTIL CLIN = 45.
             MOVE 9                  TO CLIN.
             MOVE LOW-VALUES         TO W75-KEYS.
             MOVE 1                  TO W75-S W75-S1 W75-P.
             PERFORM STOCK-LUP-TAB-LOOP.
             DISPLAY WS-RT-HD AT 0654 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             GO TO STOCK-GET-STOCK-REC.

       STOCK-GET-PREV-PAGE.
             MOVE 1                  TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
             MOVE "J"                TO WS-OPTION.

       STOCK-GET-PREV-LOOP.
             PERFORM READ-STOCK-PREV THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               MOVE SPACE            TO WS-OPTION
               PERFORM STOCK-LUP-SCREEN THRU STOCK-LUP-CLR-SCREEN
               GO TO STOCK-START-LOOKUP.
           IF WS-IXS1 < 71
               ADD 1                 TO WS-IXS1
               GO TO STOCK-GET-PREV-LOOP.
             GO TO STOCK-NEW-PAGE.

       STOCK-CASH-PRICES.
             DISPLAY WS-CS-HD AT 0654 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE 1                  TO W75-S2.
             MOVE 09                 TO LIN.

       STOCK-CASH-LOOP.
           IF NOT(W75-CSH(W75-S2, W75-P) = ZERO)
               MOVE W75-CSH(W75-S2, W75-P)   TO W100-S6V2
               DISPLAY W100-PRICE AT LINE LIN COLUMN 61 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
               MOVE W75-VCSH(W75-S2, W75-P)  TO W100-S6V2
               DISPLAY W100-PRICE AT LINE LIN COLUMN 71 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
           ELSE
           IF W75-P > 1
               DISPLAY "   N/A   " AT LINE LIN COLUMN 61 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
               DISPLAY "         " AT LINE LIN COLUMN 71 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
           ELSE
               DISPLAY "         " AT LINE LIN COLUMN 61 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
           IF W75-S2 < W75-S
               ADD 1                 TO W75-S2 LIN
               GO TO STOCK-CASH-LOOP.
             GO TO STOCK-SAVE-FULL-SCREEN.

       STOCK-RETAIL-PRICES.
             DISPLAY WS-RT-HD AT 0654 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE 1              TO W75-S2.
             MOVE 09             TO LIN.

       STOCK-RETAIL-LOOP.
           IF NOT(W75-SL(W75-S2, W75-P) = ZERO)
               MOVE W75-SL(W75-S2, W75-P)  TO W100-S6V2
               DISPLAY W100-PRICE AT LINE LIN COLUMN 61 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
               MOVE W75-VSL(W75-S2, W75-P) TO W100-S6V2
               DISPLAY W100-PRICE AT LINE LIN COLUMN 71 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
           ELSE
           IF W75-P > 1
               DISPLAY "   N/A   " AT LINE LIN COLUMN 61 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
               DISPLAY "         " AT LINE LIN COLUMN 71 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
           ELSE
               DISPLAY "         " AT LINE LIN COLUMN 61 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
           IF W75-S2 < W75-S
               ADD 1                 TO W75-S2 LIN
               GO TO STOCK-RETAIL-LOOP.
             GO TO STOCK-SAVE-FULL-SCREEN.

       STOCK-WSALE-PRICES.
             DISPLAY WS-WS-HD AT 0654 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE 1              TO W75-S2.
             MOVE 09             TO LIN.

       STOCK-WSALE-LOOP.
           IF NOT(W75-WSL(W75-S2, W75-P) = ZERO)
               MOVE W75-WSL(W75-S2, W75-P)   TO W100-S6V2
               DISPLAY W100-PRICE AT LINE LIN COLUMN 61 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
               MOVE W75-VWSL(W75-S2, W75-P)  TO W100-S6V2
               DISPLAY W100-PRICE AT LINE LIN COLUMN 71 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
           ELSE
           IF W75-P > 1
               DISPLAY "   N/A   " AT LINE LIN COLUMN 61 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
               DISPLAY "         " AT LINE LIN COLUMN 71 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
           ELSE
               DISPLAY "         " AT LINE LIN COLUMN 61 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
           IF W75-S2 < W75-S
               ADD 1                 TO W75-S2 LIN
               GO TO STOCK-WSALE-LOOP.
             GO TO STOCK-SAVE-FULL-SCREEN.

       STOCK-SAVE-FULL-SCREEN.
             DISPLAY " " AT 5072 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             PERFORM STOCK-CRT-ATTRIB.
             PERFORM SCREEN-DATA.
           IF W75-S1 > 0
               GO TO STOCK-HIGHLIGHT.
             GO TO STOCK-GET-USER-OPT.

       STOCK-NO-RECORD.
             DISPLAY "No record - "         AT 5012 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                 "N"                WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                  "ext or "         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                         "Q"        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                          "uit > <" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.

       STOCK-GET-USER-OPT-2.
             ACCEPT WS-OPTION AT 5038 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-OPTION = "N" OR "Q")
               GO TO STOCK-GET-USER-OPT-2.
           IF WS-OPTION = "N"
               GO TO STOCK-LUP-SCREEN.
             MOVE SPACES             TO W10-EXT-ITEM.
             GO TO STOCK-LOOKUP-EXIT.

       STOCK-ADJ-NO-KEYS.
           IF W75-S1 > 1
               SUBTRACT 1            FROM W75-S1 CLIN.
             GO TO STOCK-HIGHLIGHT.

       STOCK-CHK-NO-KEYS.
           IF W75-S1 = ZERO
               MOVE 1                TO W75-S1
               GO TO STOCK-HIGHLIGHT.
           IF W75-S1 < W75-S
               ADD 1                 TO W75-S1 CLIN.

       STOCK-HIGHLIGHT.
             PERFORM STOCK-CRT-ATTRIB.
             DISPLAY W43-DET (CLIN) AT CPOS WITH REVERSE-VIDEO.
             GO TO STOCK-GET-USER-OPT.

       STOCK-CHK-SELECT.
           IF W75-S1 = ZERO
               GO TO STOCK-GET-USER-OPT.
             MOVE CSTART               TO WS1-CURSOR.
             MOVE W75-EXT-CODE(W75-S1) TO W10-EXT-ITEM STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
      *
      *    ****   To avoid non stock items, labour and free format items
      *           having the last item on screens description when the
      *           lookup program returns control to the calling program
      *           the stock description is moved to the W10- Link area.
      *
             MOVE STK-DESC           TO W10-DESC.
             MOVE ZERO               TO COL-AND-LINE.
             GO TO STOCK-LOOKUP-EXIT.

       STOCK-MEM.
           IF NOT(W75-S1 = ZERO)
               MOVE W75-EXT-CODE(W75-S1)  TO W10-EXT-ITEM
               PERFORM STOCK-MEMO.

       STOCK-MEM-EXIT.
             EXIT.

       
       STOCK-CODES.
             PERFORM SAVE-SCREEN-3.
             MOVE 1                  TO W75-S2.
             MOVE 06                 TO CLIN LIN.
             MOVE 51                 TO CCOL
             DISPLAY S-CODES.
             PERFORM STOCK-CDE-EDGE UNTIL CLIN = 45.
             PERFORM CLEAR-L46-AND-50.
      *
      *    ****    D I S P L A Y   X - R E F   A N D   A L T   C O D E S
      *
       STOCK-CDE-DISP.
             MOVE W75-EXT-CODE(W75-S2) TO STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
             PERFORM STOCK-FB-COL.
           IF NOT STK-IND = 4
               DISPLAY STK-XREF  AT LINE LIN COLUMN 51 WITH CONTROL WS-CONT
               DISPLAY STK-ACODE AT LINE LIN COLUMN 70 WITH CONTROL WS-CONT.
           IF W75-S2 < W75-S
               ADD 1             TO W75-S2 LIN
               GO TO STOCK-CDE-DISP.
             DISPLAY "ENTER to continue" AT 4655 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             PERFORM RESTORE-SCREEN-3.

       STOCK-CODES-EXIT.
             EXIT.
      *
      *    ****    D I S P L A Y   D E S C R I P T I O N   2
      *
       STOCK-DESC2.
             PERFORM SAVE-SCREEN-3.
             MOVE 1                  TO W75-S2.
             MOVE 06                 TO LIN.
             DISPLAY "Description-2" AT 0421 WITH REVERSE-VIDEO.
             PERFORM CLEAR-L50.

       STOCK-DSC-DISP.
             MOVE W75-EXT-CODE(W75-S2) TO STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
             PERFORM STOCK-FB-COL.
           IF STK-DESC2 = SPACES
               MOVE STK-DESC         TO W10-DESC
           ELSE
               MOVE STK-DESC2        TO W10-DESC.
           IF NOT STK-IND = 4
               DISPLAY W10-SDSC AT LINE LIN COLUMN 21 WITH CONTROL WS-CONT.
           IF W75-S2 < W75-S
               ADD 1                 TO W75-S2 LIN
               GO TO STOCK-DSC-DISP.
             DISPLAY "ENTER to continue" AT 5055 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             PERFORM RESTORE-SCREEN-3.

       STOCK-DESC2-EXIT.
             EXIT.

       STOCK-CDE-EDGE.
             DISPLAY "                  ³          " AT CPOS WITH FOREGROUND-COLOR Cyan.
             ADD 1                   TO CLIN.

       STOCK-BIN.
             PERFORM SAVE-SCREEN-3.
             MOVE 1                  TO W75-S2.
             MOVE 06                 TO CLIN LIN.
             MOVE 51                 TO CCOL
             DISPLAY S-BIN.
             PERFORM STOCK-BIN-EDGE UNTIL CLIN = 45.
             PERFORM CLEAR-L46-AND-50.

       STOCK-BIN-DISP.
             MOVE W75-EXT-CODE(W75-S2) TO STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
             PERFORM STOCK-FB-COL.
           IF NOT STK-IND = 4
               DISPLAY STK-BIN AT LINE LIN COLUMN 53 WITH CONTROL WS-CONT.
           IF W75-S2 < W75-S
               ADD 1             TO W75-S2 LIN
               GO TO STOCK-BIN-DISP.
             DISPLAY "ENTER to continue" AT 4655 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             PERFORM RESTORE-SCREEN-3.

       STOCK-BIN-EXIT.
             EXIT.

       STOCK-BIN-EDGE.
             DISPLAY "         " AT CPOS WITH FOREGROUND-COLOR Cyan.
             ADD 1                   TO CLIN.

       STOCK-CRT-EDGE.
             DISPLAY "³"                                                                        AT CPOS WITH BACKGROUND-COLOR Black FOREGROUND-COLOR Cyan
                      "                  ³                             ³         ³         ³         "  WITH FOREGROUND-COLOR Cyan
                                                                                                    "³" WITH BACKGROUND-COLOR Black FOREGROUND-COLOR Cyan.
             ADD 1                   TO CLIN.

       STOCK-LOOKUP-EXIT.
             EXIT.
      *    *************************************************************
      *    ****    S T O C K   L O O K U P   -  S T K A L T     ****
      *    *************************************************************
      *                                                                *
      *     PERFORMED FROM AA50                                        *
      *           (P R O C E S S   F U N C T I O N   K E Y S)          *
      *                                                                *
      *  Aug 2008 - New file (STKALT) - Stock alternate index included *
      *             for lookups, using any word contained in the Stock *
      *             description.                                       *
      *                                                                *
      *  Nov 2009 - Include words from Description 2 and from the Item *
      *             code (Some item codes are comprised of individual  *
      *             words and these will be included in the alternate  *
      *             Index)                                             *
      *                                                                *
      ******************************************************************
       STOCK-A-LOOKUP  SECTION.
       STOCK-A-SAVE-LIN.
             MOVE LIN                TO SLIN.

       STCKA-LUP-SCREEN.
             PERFORM ERASE-SCREEN.
             DISPLAY S-STK.
             DISPLAY "STOCK LOOKUP"        AT 0530 WITH FOREGROUND-COLOR Grey  HIGHLIGHT
                     " (ANY WORD)"                 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY WS-RT-HD              AT 0654 WITH FOREGROUND-COLOR Cyan  HIGHLIGHT.
             DISPLAY "Single"              AT 0668 WITH FOREGROUND-COLOR Cyan  HIGHLIGHT.
             DISPLAY "Find Word or Part :" AT 0602.
             MOVE 1                  TO WS-PRICE.
             MOVE SPACE              TO WS-OPTION.

       STCKA-LUP-CLR-SCREEN.
             MOVE 0601               TO CPOS.
             PERFORM STOCK-CRT-EDGE UNTIL CLIN = 45.
             MOVE SPACES             TO W10-ACODE W10-DESC W10-DESC2 W10-EXT-CODE2 W10-EXT-CODE1.

       STCKA-LUP-DATA.
             ACCEPT SA-STKD.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE SPACES TO W10-EXT-ITEM
                                 GO TO STOCK-A-LOOKUP-EXIT
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO STCKA-LUP-DATA.

       STCKA-START-LOOKUP.
             PERFORM CLEAR-L46-AND-50.
             MOVE 6                  TO CLIN.
             MOVE 1                  TO W75-S W75-S1 WS-P1 W75-P.

       STCKA-LUP-TAB-LOOP.
             MOVE SPACES             TO W75-EXT-CODE(W75-S1).
           IF W75-S1 < 36
               ADD 1                 TO W75-S1
               GO TO STCKA-LUP-TAB-LOOP.
             MOVE ZERO               TO W75-S1.

       STCKA-LUP-ENTRY-TYPE.
      *
      *    ****    D E S C R I P T I O N   S E Q U E N C E
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If no description has been entered, inform the User that  ³
      *    ³ to view the stock in description sequence, starting from  ³
      *    ³ the first description in the file, the F2 key lookup must ³
      *    ³ be used.                                                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF W10-DESC = SPACES
               DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1217 WITH FOREGROUND-COLOR Red   HIGHLIGHT BACKGROUND-COLOR Red
                                                                     "¿"        WITH FOREGROUND-COLOR Black HIGHLIGHT BACKGROUND-COLOR Red
                       "³"                                              AT 1317 WITH FOREGROUND-COLOR Red   HIGHLIGHT BACKGROUND-COLOR Red
                        "Use "                                                  WITH FOREGROUND-COLOR Grey  HIGHLIGHT BACKGROUND-COLOR Red
                            "F2"                                                WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Red
                              " for all descriptions  -  "                      WITH FOREGROUND-COLOR Grey  HIGHLIGHT BACKGROUND-COLOR Red
                                                        "Press ANY key"         WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Red
                                                                     "³"        WITH FOREGROUND-COLOR Black HIGHLIGHT BACKGROUND-COLOR Red
                       "À"                                              AT 1417 WITH FOREGROUND-COLOR Red   HIGHLIGHT BACKGROUND-COLOR Red
                        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"        WITH FOREGROUND-COLOR Black HIGHLIGHT BACKGROUND-COLOR Red
               MOVE 13               TO SHADE-ROW
               MOVE 19               TO SHADE-COL
               MOVE 45               TO SHADE-WIDTH
               MOVE 2                TO SHADE-LINES
               PERFORM SCREEN-SHADOW
               PERFORM HIDE-THE-CURSOR
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
               CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS
               PERFORM DISPLAY-THE-CURSOR
               GO TO STOCK-A-LOOKUP-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Convert the description to upper case, as all keys in the ³
      *    ³ system are stored as upper case.                          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             MOVE W10-DESC           TO W50-DATA.
             MOVE 30                 TO W50-LENGTH.
             CALL "CBL_TOUPPER" USING W50-30 BY VALUE W50-LENGTH RETURNING WS-STATUS.
             MOVE W50-30             TO W10-DESC.
             MOVE 1                  TO WS-LENGTH.
             MOVE W10-DESC           TO W40-ADESC.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ WS-SW4 - Will be equal to the number of characters that   ³
      *    ³ have been keyed in by the User.                           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *      MOVE 1                  TO WS-S4.
      *      PERFORM UNTIL W40-ALT-CHAR(WS-S4) = SPACE OR WS-S4 > 12
      *      ADD 1                   TO WS-S4.
      *      SUBTRACT 1              FROM WS-S4.
             MOVE 12                 TO WS-S4.
             PERFORM UNTIL W40-ALT-CHAR(WS-S4) NOT = SPACE OR WS-S4 < 2
                 SUBTRACT 1          FROM WS-S4.
             MOVE W10-DESC           TO STKA-DKEY.
             PERFORM START-AT-STKA-DESC THRU READ-STKALT-EXIT.
           IF WS-F-ERROR = 49
               GO TO STCKA-NO-RECORD.

       STCKA-GET-STOCK-REC.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³      Must the READ-STKALT-NEXT statement be skipped?      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF WS-OPTION = "J"
               MOVE SPACE            TO WS-OPTION
               GO TO STCKA-SKIP-STCKA-READ.
             PERFORM READ-STKALT-NEXT THRU READ-STKALT-EXIT.
           IF WS-F-ERROR = 49
               SUBTRACT 1            FROM W75-S
      *
      *    ****   U S E R   O P T I O N S
      *
               PERFORM STOCK-FULL-SCREEN
               GO TO STCKA-POSITION-CURSOR.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Check that the details (word) of the desc that have been  ³
      *    ³ read, match the characters that have been entered by the  ³
      *    ³ User. Number of characters that were entered are stored   ³
      *    ³ in WS-S4.                                                 ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             MOVE 1                  TO WS-S3.
             MOVE SPACES             TO W40-ALT-KEY.
             PERFORM UNTIL WS-S3 > WS-S4
                 MOVE STKA-DCHAR(WS-S3)  TO W40-DESC-CH(WS-S3)
                 ADD 1               TO WS-S3.
           IF W40-ALT-KEY > W40-ALT-LUP
               SUBTRACT 1            FROM W75-S
      *
      *    ****   U S E R   O P T I O N S
      *
               PERFORM STOCK-FULL-SCREEN
               GO TO STCKA-POSITION-CURSOR.

       STCKA-SKIP-STCKA-READ.
             MOVE STKA-CODE          TO W75-EXT-CODE(W75-S) STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
             MOVE 1                  TO W75-PACK.

       STCKA-PRICES-LOOP.
             MOVE STK-PR(W75-PACK)   TO W75-PR(W75-S, W75-PACK).
             MOVE STK-SL(W75-PACK)   TO W75-SL(W75-S, W75-PACK).
             MOVE STK-CSH(W75-PACK)  TO W75-CSH(W75-S, W75-PACK).
             MOVE STK-WSL(W75-PACK)  TO W75-WSL(W75-S, W75-PACK).
           IF STK-TAX NOT = ZERO
               MOVE W05-VAT (STK-TAX)  TO W05-RATE
               COMPUTE W75-VSL(W75-S, W75-PACK) ROUNDED = W75-SL(W75-S, W75-PACK) + (W75-SL(W75-S, W75-PACK) * W05-RTE)
               COMPUTE W75-VCSH(W75-S, W75-PACK) ROUNDED = W75-CSH(W75-S, W75-PACK) + (W75-CSH(W75-S, W75-PACK) * W05-RTE)
               COMPUTE W75-VWSL(W75-S, W75-PACK) ROUNDED = W75-WSL(W75-S, W75-PACK) + (W75-WSL(W75-S, W75-PACK) * W05-RTE)
           ELSE
               MOVE W75-SL(W75-S, W75-PACK)  TO W75-VSL(W75-S, W75-PACK)
               MOVE W75-CSH(W75-S, W75-PACK) TO W75-VCSH(W75-S, W75-PACK)
               MOVE W75-WSL(W75-S, W75-PACK) TO W75-VWSL(W75-S, W75-PACK).
      *
      *    ****    H A V E   A L L   P R I C E S   B E E N
      *     S T O R E D  ?
      *
           IF W75-PACK < 4
               ADD 1                 TO W75-PACK
               GO TO STCKA-PRICES-LOOP.
      *
      *    ****    U S E   D I F F E R E N T   C O L O U R S   F O R
      *     E A C H   S T O C K  T Y P E
      *
       STCKA-FB-COL.
             EVALUATE STK-IND
      *      C O N T R O L E D
               WHEN 0            MOVE "HIGHLIGHT FOREGROUND-COLOR  3"  TO  WS-CONT
      *      P R I C E D   I T E M
               WHEN 1            MOVE "HIGHLIGHT FOREGROUND-COLOR  6"  TO  WS-CONT
      *      B U Y - O U T
               WHEN 2            MOVE "HIGHLIGHT FOREGROUND-COLOR  7"  TO  WS-CONT
      *      H E A D I N G
               WHEN 4            MOVE "HIGHLIGHT FOREGROUND-COLOR  2"  TO  WS-CONT
      *      L A B O U R
               WHEN OTHER        MOVE "HIGHLIGHT FOREGROUND-COLOR  5"  TO  WS-CONT
             END-EVALUATE.

       STCKA-DISP-DET.
           IF WS-USE-ITM = "Y"
               MOVE STK-CODE         TO W30-ITM-CODE
           ELSE
           IF WS-EXT-STK = "Y"
               MOVE STK-CODE         TO W30-EXT-CODE
           ELSE
               MOVE STK-ITEM         TO W30-ITEM.
             DISPLAY STK-CODE AT LINE CLIN COLUMN 2 WITH CONTROL WS-CONT.
             MOVE STK-DESC           TO W10-DESC.
             DISPLAY W10-SDSC AT LINE CLIN COLUMN 21 WITH CONTROL WS-CONT.
           IF STK-QUANT < ZERO
               MOVE STK-QUANT        TO W100-NQNT
           ELSE
           IF STK-QUANT > 999999.99
             MOVE STK-QUANT          TO W100-LQNT
           ELSE
             MOVE STK-QUANT          TO W100-QNT.
           IF NOT(STK-IND = 4)
              DISPLAY W100-QUANT AT LINE CLIN COLUMN 51 WITH CONTROL WS-CONT
              MOVE W75-SL(W75-S, W75-P)  TO W100-S6V2
              DISPLAY W100-PRICE AT LINE CLIN COLUMN 61 WITH CONTROL WS-CONT
              MOVE W75-VSL(W75-S, W75-P) TO W100-S6V2
              DISPLAY W100-PRICE AT LINE CLIN COLUMN 71 WITH CONTROL WS-CONT.
           IF CLIN < 44
              ADD 1                  TO CLIN W75-S
              GO TO STCKA-GET-STOCK-REC.
      *
      *    ****   U S E R   O P T I O N S
      *
             PERFORM STOCK-FULL-SCREEN.

       STCKA-POSITION-CURSOR.
             PERFORM SAVE-SCREEN-2.
             MOVE 0                  TO W75-S1.
             MOVE 0901               TO CPOS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³              Restore the screen attributes.               ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       STCKA-CRT-ATTRIB.
             MOVE 3920   TO W41-LENGTH.
             CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS W42-ATTRIB2 W41-LENGTH RETURNING WS-STATUS.
             MOVE 4000   TO W41-LENGTH.

       STCKA-GET-USER-OPT.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             DISPLAY SPACE AT 5040 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY      GO TO STCKA-ADJ-NO-KEYS
                 WHEN DOWN-KEY    GO TO STCKA-CHK-NO-KEYS
                 WHEN LEFT-KEY    IF W75-P > 1
                                      SUBTRACT 1  FROM W75-P
                                      GO TO STCKA-CHK-PACKS
                 WHEN RIGHT-KEY   IF W75-P < 4
                                      ADD 1       TO W75-P
                                      GO TO STCKA-CHK-PACKS
                 WHEN ENTER-KEY
                 WHEN CR-KEY      GO TO STCKA-CHK-SELECT
                 WHEN OTHER       PERFORM AA900-ALARM
               END-EVALUATE
               GO TO STCKA-GET-USER-OPT
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY     MOVE SPACES TO W10-EXT-ITEM
                                  GO TO STOCK-A-LOOKUP-EXIT
                 WHEN PAGE-DOWN   GO TO STCKA-GET-NEXT-PAGE
                 WHEN PAGE-UP     GO TO STCKA-GET-PREV-PAGE
                 WHEN F2-KEY      PERFORM BA000
                                  GO TO STCKA-GET-USER-OPT
                 WHEN F5-KEY      PERFORM BB000
                                  GO TO STCKA-GET-USER-OPT
                 WHEN F6-KEY      MOVE WS1-CURS   TO CRT-LINE
                                  PERFORM STOCK-DESC2 THRU STOCK-DESC2-EXIT
                                  MOVE CRT-LINE   TO WS1-CURS
                                  GO TO STCKA-GET-USER-OPT
                 WHEN F7-KEY      MOVE WS1-CURS   TO CRT-LINE
                                  PERFORM STOCK-BIN THRU STOCK-BIN-EXIT
                                  MOVE CRT-LINE   TO WS1-CURS
                                  GO TO STCKA-GET-USER-OPT
                 WHEN F8-KEY      MOVE WS1-CURS   TO CRT-LINE
                                  PERFORM STOCK-CODES THRU STOCK-CODES-EXIT
                                  MOVE CRT-LINE   TO WS1-CURS
                                  GO TO STCKA-GET-USER-OPT
                 WHEN F9-KEY      PERFORM STOCK-MEM
      *
      *    ****   U S E R   O P T I O N S
      *
                                  PERFORM STOCK-FULL-SCREEN
                                  GO TO STCKA-GET-USER-OPT
                 WHEN OTHER       PERFORM AA900-ALARM
               END-EVALUATE
               GO TO STCKA-GET-USER-OPT
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X       TO WS-OPTION
               GO TO STCKA-CHK-PRICES.
             PERFORM AA900-ALARM.
             GO TO STCKA-GET-USER-OPT.

       STCKA-CHK-PACKS.
             EVALUATE W75-P
                 WHEN 1          DISPLAY "Single" AT 0368 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                 WHEN 2          DISPLAY "Pack 1" AT 0368 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                 WHEN 3          DISPLAY "Pack 2" AT 0368 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                 WHEN 4          DISPLAY "Pack 3" AT 0368 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
             END-EVALUATE.
             GO TO STCKA-DISP-PRICES.

       STCKA-CHK-PRICES.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-OPTION = "1" OR "2" OR "3")
               GO TO STCKA-GET-USER-OPT.
             MOVE WS-OPTION  TO WS-PRICE.

       STCKA-DISP-PRICES.
             EVALUATE WS-PRICE
                 WHEN "1"        GO TO STCKA-RETAIL-PRICES
                 WHEN "2"        GO TO STCKA-CASH-PRICES
                 WHEN "3"        GO TO STCKA-WSALE-PRICES
             END-EVALUATE.
             PERFORM AA900-ALARM.
             GO TO STCKA-GET-USER-OPT.

       STCKA-GET-NEXT-PAGE.
           IF W75-S < 16
               GO TO STCKA-GET-USER-OPT.

       STCKA-NEW-PAGE.
             MOVE 0601               TO CPOS.
             PERFORM STOCK-CRT-EDGE UNTIL CLIN = 45.
             MOVE 6                  TO CLIN.
             MOVE LOW-VALUES         TO W75-KEYS.
             MOVE 1                  TO W75-S W75-S1 W75-P.
             PERFORM STCKA-LUP-TAB-LOOP.
             DISPLAY WS-RT-HD AT 0354 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             GO TO STCKA-GET-STOCK-REC.

       STCKA-GET-PREV-PAGE.
             MOVE 1                  TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
             MOVE "J"                TO WS-OPTION.

       STCKA-GET-PREV-LOOP.
             PERFORM READ-STKALT-PREV THRU READ-STKALT-EXIT.
           IF WS-F-ERROR = 22
               MOVE SPACE            TO WS-OPTION
               PERFORM STCKA-LUP-SCREEN THRU STCKA-LUP-CLR-SCREEN
               MOVE W40-ADESC        TO STKA-DKEY
               PERFORM START-AT-STKA-DESC THRU READ-STKALT-EXIT
               GO TO STCKA-NEW-PAGE.
           IF WS-IXS1 < 31
               ADD 1                 TO WS-IXS1
               GO TO STCKA-GET-PREV-LOOP.
             GO TO STCKA-NEW-PAGE.

       STCKA-CASH-PRICES.
             DISPLAY WS-CS-HD AT 0354 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE 1                  TO W75-S2.
             MOVE 06                 TO LIN.
  
       STCKA-CASH-LOOP.
           IF NOT(W75-CSH(W75-S2, W75-P) = ZERO)
               MOVE W75-CSH(W75-S2, W75-P)  TO W100-S6V2
               DISPLAY W100-PRICE AT LINE LIN COLUMN 61 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
               MOVE W75-VCSH(W75-S2, W75-P) TO W100-S6V2
               DISPLAY W100-PRICE AT LINE LIN COLUMN 71 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
           ELSE
           IF W75-P > 1
               DISPLAY "   N/A   " AT LINE LIN COLUMN 61 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
               DISPLAY "         " AT LINE LIN COLUMN 71 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
           ELSE
               DISPLAY "         " AT LINE LIN COLUMN 61 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
           IF W75-S2 < W75-S
               ADD 1                 TO W75-S2 LIN
               GO TO STCKA-CASH-LOOP.
             GO TO STCKA-SAVE-FULL-SCREEN.

       STCKA-RETAIL-PRICES.
             DISPLAY WS-RT-HD AT 0354 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE 1                  TO W75-S2.
             MOVE 06                 TO LIN.

       STCKA-RETAIL-LOOP.
           IF NOT(W75-SL(W75-S2, W75-P) = ZERO)
               MOVE W75-SL(W75-S2, W75-P)  TO W100-S6V2
               DISPLAY W100-PRICE AT LINE LIN COLUMN 61 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
               MOVE W75-VSL(W75-S2, W75-P) TO W100-S6V2
               DISPLAY W100-PRICE AT LINE LIN COLUMN 71 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
           ELSE
           IF W75-P > 1
               DISPLAY "   N/A   " AT LINE LIN COLUMN 61 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
               DISPLAY "         " AT LINE LIN COLUMN 71 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
           ELSE
               DISPLAY "         " AT LINE LIN COLUMN 61 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
           IF W75-S2 < W75-S
               ADD 1                 TO W75-S2 LIN
               GO TO STCKA-RETAIL-LOOP.
             GO TO STCKA-SAVE-FULL-SCREEN.

       STCKA-WSALE-PRICES.
             DISPLAY WS-WS-HD AT 0354 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE 1                  TO W75-S2.
             MOVE 06                 TO LIN.

       STCKA-WSALE-LOOP.
           IF NOT(W75-WSL(W75-S2, W75-P) = ZERO)
               MOVE W75-WSL(W75-S2, W75-P)  TO W100-S6V2
               DISPLAY W100-PRICE AT LINE LIN COLUMN 61 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
               MOVE W75-VWSL(W75-S2, W75-P) TO W100-S6V2
               DISPLAY W100-PRICE AT LINE LIN COLUMN 71 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
           ELSE
           IF W75-P > 1
               DISPLAY "   N/A   " AT LINE LIN COLUMN 61 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
               DISPLAY "         " AT LINE LIN COLUMN 71 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
           ELSE
               DISPLAY "         " AT LINE LIN COLUMN 61 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
           IF W75-S2 < W75-S
               ADD 1                 TO W75-S2 LIN
               GO TO STCKA-WSALE-LOOP.
             GO TO STCKA-SAVE-FULL-SCREEN.

       STCKA-SAVE-FULL-SCREEN.
             DISPLAY " " AT 5072 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             PERFORM STCKA-CRT-ATTRIB.
             PERFORM SCREEN-DATA.
           IF W75-S1 > 0
               GO TO STCKA-HIGHLIGHT.
             GO TO STCKA-GET-USER-OPT.

       STCKA-NO-RECORD.
             DISPLAY "No record - "         AT 5012 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                 "N"                WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                  "ext or "         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                         "Q"        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                          "uit > <" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.

       STCKA-GET-USER-OPT-2.
             ACCEPT WS-OPTION AT 5038 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-OPTION = "N" OR "Q")
               GO TO STCKA-GET-USER-OPT-2.
           IF WS-OPTION = "N"
               GO TO STCKA-LUP-SCREEN.
             MOVE SPACES             TO W10-EXT-ITEM.
             GO TO STOCK-A-LOOKUP-EXIT.

       STCKA-ADJ-NO-KEYS.
           IF W75-S1 > 1
               SUBTRACT 1            FROM W75-S1 CLIN.
             GO TO STCKA-HIGHLIGHT.

       STCKA-CHK-NO-KEYS.
           IF W75-S1 = ZERO
               MOVE 1                TO W75-S1
               GO TO STCKA-HIGHLIGHT.
           IF W75-S1 < W75-S
               ADD 1                 TO W75-S1 CLIN.

       STCKA-HIGHLIGHT.
             PERFORM STCKA-CRT-ATTRIB.
             DISPLAY W43-DET (CLIN) AT CPOS WITH REVERSE-VIDEO.
             GO TO STCKA-GET-USER-OPT.

       STCKA-CHK-SELECT.
           IF W75-S1 = ZERO
              GO TO STCKA-GET-USER-OPT.
             MOVE CSTART             TO WS1-CURSOR.
             MOVE W75-EXT-CODE(W75-S1) TO W10-EXT-ITEM STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
      *
      *    ****   To avoid non stock items labour and free format items
      *    having the last item on screens description when the
      *    lookup program returns control to the calling program
      *    the stock description is moved to the W10- Link area.
      *
             MOVE STK-DESC           TO W10-DESC.
             MOVE ZERO               TO COL-AND-LINE.

       STOCK-A-LOOKUP-EXIT.
             EXIT.
      *
      *  ****  E N Q U I R Y   U S I N G   S T O C K   C O D E
      *
       BA000         SECTION 5.
       BA000-ENQUIRY.
           IF W75-S1 = ZERO
               GO TO BA999.
             MOVE W75-EXT-CODE(W75-S1) TO W10-EXT-ITEM.
             PERFORM SAVE-SCREEN-3.
             PERFORM ERASE-SCREEN.
             DISPLAY S04C.
             DISPLAY S04.
           IF WS-USE-ITM = "Y"
               DISPLAY "/ITM" AT 0312 "/" AT 0332.
           IF W10-ART11 = "Y"
               DISPLAY "VAT - Article 11/1g" AT 0849.
             PERFORM CA000.
           IF WS-EXT-STK = "Y"
               DISPLAY S05-EXT
           ELSE
           IF WS-USE-ITM = "Y"
               DISPLAY S05-ITM
           ELSE
               DISPLAY S05.
             MOVE ZERO   TO WS-WH-QUANT.
           IF LS0-WHS = 2
               PERFORM BA30 THRU BA40.
             MOVE STK-XREF           TO W10-EXT-CODE2.
             MOVE STK-BIN            TO W10-BIN.
             MOVE STK-DESC           TO W10-DESC.
             MOVE STK-DESC2          TO W10-DESC2.
             MOVE STK-TAX            TO W10-TAX.
             MOVE STK-EXEMPT         TO W10-EXEMPT.
             MOVE STK-MARKUP         TO W10-MARKUP.
             MOVE STK-CMARKUP        TO W10-CMARKUP.
             MOVE STK-WMARKUP        TO W10-WMARKUP.
             MOVE STK-IND            TO W10-IND.
             MOVE STK-QUANT          TO W10-QUANT.
             MOVE STK-PER            TO W10-PER.
           IF W10-PER = SPACES OR LOW-VALUES
               MOVE "Each"           TO W10-PER.
             MOVE STK-PRC            TO W10-PRC.
             MOVE STK-LEVEL          TO W10-REORD.
             MOVE STK-LTIME          TO W10-LTIME.
             MOVE STK-COST           TO W10-COST.
             MOVE STK-SELL           TO W10-SELL.
             MOVE STK-CASH           TO W10-CASH.
             MOVE STK-WSALE          TO W10-WSALE.
             MOVE STK-LDG            TO W10-LEDG.
             MOVE STK-GUAR           TO W10-GUAR.
             MOVE STK-ORD            TO W10-ORDER.
             MOVE STK-DTE            TO W22-DTE2.
             PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT.
             MOVE W22-DTE3           TO W10-ODTE.
             MOVE STK-PUR            TO W22-DTE2.
             PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT.
             MOVE W22-DTE3           TO W10-PURCH.
           IF STK-IND < 2
               IF STK-AVGE = ZERO
                   MOVE STK-COST     TO STK-AVGE.
             MOVE STK-AVGE           TO W10-AVRG.
             MOVE STK-ACODE          TO W10-ACODE.
             COMPUTE W09-WMARKUP ROUNDED = ((STK-WSALE - STK-COST) * 100.000) / STK-COST.
             COMPUTE W09-MARKUP ROUNDED  = ((STK-SELL  - STK-COST) * 100.000) / STK-COST.
             COMPUTE W09-CMARKUP ROUNDED = ((STK-CASH  - STK-COST) * 100.000) / STK-COST.
             DISPLAY S06C.
           IF STK-ACT = ZERO
               MOVE SPACE            TO W10-ACTIVE
           ELSE
               MOVE STK-ACT          TO W22-DTE2
               PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT
               MOVE W22-DTE3         TO W10-ADATE.
             MOVE STK-MTDC  TO W10-MTDC.
             MOVE STK-YTDC  TO W10-YTDC.
           IF LS-OK = "N"
               MOVE ZERO             TO W10-COST W10-MARKUP W10-AVRG W10-CMARKUP W10-MTDC W10-WMARKUP W10-YTDC
           ELSE
               MOVE W09-MARKUP       TO W10-MARKUP
               MOVE W09-CMARKUP      TO W10-CMARKUP
               MOVE W09-WMARKUP      TO W10-WMARKUP.
           IF LS0-STLEV < 2
               DISPLAY S06A
               DISPLAY S06C
           ELSE
               DISPLAY S06.
           IF W10-ART11 = "Y"
               DISPLAY W10-EXEMPT AT 0869 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             PERFORM AA400.
           IF NOT(STK-SUPP = SPACES OR LOW-VALUES)
               MOVE STK-SUPP         TO P-NUMBER
               PERFORM READ-CREDIT THRU READ-CREDIT-EXIT
               IF WS-F-ERROR = ZERO
                   DISPLAY "Supplier      "      AT 4603
                                          P-NAME AT 4618 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
           IF NOT(WS-WH-QUANT = ZERO)
               ADD W10-QUANT WS-WH-QUANT GIVING W100-S7V3
               DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"  AT 2054 WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Black
                                               "¿"         WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Red  HIGHLIGHT
               DISPLAY "³"                         AT 2154 WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Black
                        "Including all warehouse"          WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Grey HIGHLIGHT
                                               "³"         WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Red  HIGHLIGHT
               DISPLAY "³"                         AT 2254 WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Black
                        "quantities "                      WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Grey HIGHLIGHT
                                    W100-Q1                WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Grey HIGHLIGHT
                                               "³"         WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Red  HIGHLIGHT
               DISPLAY "À"                         AT 2354 WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Black
                        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"         WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Red  HIGHLIGHT.
               DISPLAY "Press ANY key to continue" AT 5050 WITH FOREGROUND-COLOR Brown HIGHLIGHT.

       BA05.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN F9-KEY IF STK-IND < 2
                                 IF STK-AVGE = ZERO
                                     MOVE STK-COST TO STK-AVGE
                                 END-IF
                             END-IF
                             MOVE STK-AVGE         TO W10-AVRG
                             MOVE STK-COST         TO W10-COST
                             DISPLAY S06A-C
                 WHEN OTHER  PERFORM AA900-ALARM
               END-EVALUATE
               GO TO BA05.
             PERFORM RESTORE-SCREEN-3.
             GO TO BA999.

       BA30.
             MOVE W10-EXT-ITEM       TO WST-EXT-CODE.
             MOVE SPACES             TO WST-ITM WST-WAR.
             PERFORM START-AT-WH-ITEM THRU READ-WSTOCK-EXIT.
           IF WS-F-ERROR = 52
               GO TO BA40.

       BA35.
             PERFORM READ-WSTOCK-NEXT THRU READ-WSTOCK-EXIT.
           IF WS-F-ERROR = 52
               GO TO BA40.
           IF NOT(W10-EXT-ITEM = WST-EXT-CODE)
               GO TO BA40.
             ADD WST-QUANT           TO WS-WH-QUANT.
             GO TO BA35.

       BA40.
             EXIT.

       BA999.
             EXIT.
      /
      *    ****    D I S P L A Y   P A C K   D E T A I L S
      *
       BB000        SECTION 5.
       BB00.
           IF W75-S1 = ZERO
               DISPLAY "No Item selected - Press ANY key" AT 5041 WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Grey HIGHLIGHT
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
               CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS
               DISPLAY "                                " AT 5041 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
               GO TO BB999.
             MOVE W75-EXT-CODE(W75-S1) TO W10-EXT-ITEM.
             PERFORM SAVE-SCREEN-3.
             PERFORM CA000.
             MOVE 8                  TO SHADE-ROW.
             MOVE 13                 TO SHADE-COL.
             MOVE 58                 TO SHADE-WIDTH.
             MOVE 11                 TO SHADE-LINES.
             DISPLAY S15.
             PERFORM SCREEN-SHADOW.
             MOVE 2                  TO WS-P1.

       BB05.
             MOVE STK-UNT(WS-P1)     TO W10-UNT(WS-P1).
             MOVE STK-PR(WS-P1)      TO W10-PR(WS-P1).
             MOVE STK-SL(WS-P1)      TO W10-SL(WS-P1).
             MOVE STK-CSH(WS-P1)     TO W10-CSH(WS-P1).
             MOVE STK-WSL(WS-P1)     TO W10-WSL(WS-P1).
           IF WS-P1 < 4
               ADD 1                 TO WS-P1
               GO TO BB05.
             PERFORM BC000.
             DISPLAY S16.
             DISPLAY "Press any key to continue" AT 5050 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             PERFORM RESTORE-SCREEN-3.

       BB999.
             EXIT.
      *
      *    ****    C A L C U L A T E   M A R K U P S
      *
       BC000        SECTION 5.
       BC00.
             MOVE 2                  TO WS-P1.
             MOVE STK-COST           TO W10-ICOST.

       BC05-MKUPS.
           IF LS-OK = "Y"
               COMPUTE W09-WMKUP(WS-P1) ROUNDED = ((STK-WSL(WS-P1) - W10-ICOST) * 100.000) / W10-ICOST
               COMPUTE W09-MKUP(WS-P1)  ROUNDED = ((STK-SL(WS-P1)  - W10-ICOST) * 100.000) / W10-ICOST
               COMPUTE W09-CMKUP(WS-P1) ROUNDED = ((STK-CSH(WS-P1) - W10-ICOST) * 100.000) / W10-ICOST
           ELSE
               MOVE ZERO             TO W09-WMKUP(WS-P1) W09-MKUP(WS-P1) W09-CMKUP(WS-P1).
           IF WS-P1 < 4
               ADD 1                 TO WS-P1
               GO TO BC05-MKUPS.

       BC999.
             EXIT.
      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT        SECTION 9.
       ZA000-OPEN.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

             MOVE 3                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-VAT-REG        TO W05-VATNO.
             MOVE PAR-CASES          TO WS-USE-CASES.
             MOVE PAR-PACKS          TO WS-USE-PACKS.
             MOVE PAR-USE-ITM        TO WS-USE-ITM.
             MOVE PAR-EXT-STK        TO WS-EXT-STK.
           IF NOT(PAR-ART11 = "Y" OR "N")
               MOVE "N"              TO PAR-ART11.
             MOVE PAR-ART11          TO W10-ART11.
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
             MOVE "N"                TO WS-WSTOCK.
             MOVE SPACES             TO WST-EXT-CODE WST-WAR.
             START WSTOCK KEY >= WST-IKEY.
           IF WS-STATUS = "47"
               OPEN I-O WSTOCK
               MOVE "Y"              TO WS-WSTOCK.

       ZA999.
             EXIT.

      *
      *    ****    I / O   E R R O R   M E S S A G E S
      *
       I-O-ERRORS      SECTION 9.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 3     MOVE W02-CREDIT   TO WS-FILE
                          MOVE P-NUMBER     TO WS-KEYX
               WHEN 7     MOVE W02-DEPART   TO WS-FILE
                          MOVE DPT-CODE     TO WS-KEYX
               WHEN 15    MOVE WS-PARID     TO WS-FILE
                          MOVE WS-PARKEY    TO WS-KEY
               WHEN 21    MOVE W02-SPARTS   TO WS-FILE
                          MOVE PRT-ITEM     TO WS-KEYX
               WHEN 22    MOVE W02-STOCKF   TO WS-FILE
                          MOVE STK-CODE     TO WS-KEYX
               WHEN 25    MOVE W02-STKMEM   TO WS-FILE
                          MOVE SME-KEY      TO WS-KEYX
               WHEN 49    MOVE W02-STKALT   TO WS-FILE
                          MOVE STKA-KEY     TO WS-KEYX
               WHEN 52    MOVE W02-WSTOCK   TO WS-FILE
                          MOVE WST-KEY      TO WS-KEYX
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STPLOOK"    TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
