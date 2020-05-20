      $set LINKCOUNT"512" GNT"STP018.GNT"
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
      *    ******   ********  *******     ****       **      ******    *
      *   **    **     **     **    **   **  **     ***     **    **   *
      *   **           **     **    **  **    **     **     **    **   *
      *    ******      **     *******   **    **     **      ******    *
      *         **     **     **        **    **     **     **    **   *
      *   **    **     **     **         **  **      **     **    **   *
      *    ******      **     **          ****    ********   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *        S T O C K   -   A D J U S T   Q U A N T I T I E S       *
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
       PROGRAM-ID.     STP018.
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

       COPY "ADJREF.SL".

       COPY "CONTROL.SL".

       COPY "DEPART.SL".

       COPY "LEDTRF.SL".

       COPY "PARAM.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

       COPY "STKDEX.SL".

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

       COPY "ADJREF.FDE".

       COPY "CONTROL.FDE".

       COPY "DEPART.FDE".

       COPY "LEDTRF.FDE".

       COPY "RECOVER.FD".

       COPY "PARAM.FDE".

       COPY "SHARED.FDE".

       COPY "STKDEX.FDE".

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
       77  WS-DISC    PIC 999V99   COMP-3.
       77  WS-SUB    PIC 9(04)   COMP-5.
       77  WS-S1    PIC 9(04)   COMP-5.
       77  WS-S2    PIC 9(04)   COMP-5.
       77  WS-S3    PIC 9(04)   COMP-5.
       77  WS-S4    PIC 9(04)   COMP-5.
       77  WS-S5    PIC 9(04)   COMP-5.
       77  WS-S6    PIC 9(04)   COMP-5.
       77  WS-S7    PIC 9(04)   COMP-5.
       77  WS-S8    PIC 9(04)   COMP-5.
       77  WS-S9    PIC 9(04)   COMP-5.
       77  WS-IXS    PIC 9(04)   COMP-5.
       77  WS-IXS1    PIC 9(04)   COMP-5.
       77  WS-IXS2    PIC 9(04)   COMP-5.
       77  WS-IXS3    PIC 9(04)   COMP-5.
       77  WS-IXS4    PIC 9(04)   COMP-5.
       77  WS-TRFKEY    PIC 9(06)   COMP-5.
       77  WS-PRCKEY    PIC 9(06)   COMP-5.
       77  WS-PARKEY    PIC 9(06)   COMP-5.
       77  WS-NETKEY    PIC 9(06)   COMP-5.
       77  WS-RECKEY    PIC 9(06)   COMP-5.
       77  WS-RECOVER    PIC 9(06)   COMP-5.
       77  WS-RECORDS    PIC 9(06)   COMP-5.
       77  WS-SHARED    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
       77  WS-STOCK    PIC X(01).
       77  WS-KEYSTR    PIC 9(04)   COMP-5.
       77  WS-KEY1    PIC 9(04)   COMP-5.
       77  WS-ITM    PIC X(18).
       77  WS-CARDEX    PIC X(01).
       77  WS-USE-CASES    PIC X(01).
       77  WS-USE-PACKS    PIC X(01).
       77  WS-USE-ITM    PIC X(01).
       77  WS-EXT-STK    PIC X(01).
       77  WS-ETYPE    PIC X(01).
       77  WS-OPTION    PIC X(01).
       77  WS-SKIP    PIC X(01).
       77  WS-ERROR    PIC 9(01).
       77  WS-AMEND    PIC 9(01)  VALUE 0.
       77  WS-COMP    PIC 9(01).
       77  WS-WEEK    PIC 9(01).
       77  WS-TYPE    PIC X(01).
       77  WS-REFERENCE    PIC X(08).
       77  WS-BATCH    PIC 9(05).
       77  WS-PASSWORD    PIC X(08).
       77  WS-QPASSWORD    PIC X(06).
       77  WS-PRINT    PIC 9(01).
       77  WS-LABCDE    PIC X(08).
       77  WS-PASS    PIC X(08).
       77  WS-ER2    PIC X(25)  VALUE
                           "May not be less than cost".
       77  WS-DESC-H1    PIC X(14) VALUE "Description".
       77  WS-DESC-H2    PIC X(14) VALUE "Description 2".
       77  WS-RT-HD    PIC X(10) VALUE "-Retail   ".
       77  WS-WS-HD    PIC X(10) VALUE "-Wholesale".
       77  WS-CS-HD    PIC X(10) VALUE "-Cash sale".
       77  WS-NODEP    PIC X(30) VALUE
      "NO  SUCH   D E P A R T M E N T".
       77  PRG-NAME    PIC X(12) VALUE "STP\STPLOOK".
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STP018".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK ADJUSTMENTS - QUANTITY".
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
           03  WS-MODULE       PIC  X(03) VALUE "STP".
           03  WS-PROG        PIC  X(03) VALUE "018".

       COPY "LEDTRF.WS".

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

       COPY "W05.RND".

       COPY "W05.VAT".

       01  W09-STOCK.
           03  W09-MARKUP          PIC S9(03)V9999 COMP-3.
           03  W09-CMARKUP         PIC S9(03)V9999 COMP-3.
           03  W09-WMARKUP         PIC S9(03)V9999 COMP-3.

       COPY "W10.STK".

          03  W10-TCOST            PIC S9(09)V99   COMP-3.
          03  W10-CSHPR            PIC S9(07)V9(6) COMP-3.
          03  W10-WSPRC            PIC S9(07)V9(6) COMP-3.
          03  W10-CCASH            PIC S9(07)V9(6) COMP-3.
          03  W10-CWSALE           PIC S9(07)V9(6) COMP-3.
          03  W10-MCASH            PIC S9(07)V9999 COMP-3.
          03  W10-MWSALE           PIC S9(07)V9999 COMP-3.

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

           03  W15-RSELL           PIC S9(09)V99.
           03  W15-RSR1                          REDEFINES W15-RSELL.
               05  W15-RSR         PIC S9(09).
               05  W15-RSRANDS                   REDEFINES W15-RSR.
                   07  W15-RSR8    PIC S9(08).
                   07  W15-RSR9    PIC  9(01).
               05  W15-RSC         PIC  9(02).
           03  W15-RSRC2                         REDEFINES W15-RSELL.
               05  FILLER          PIC S9(08).
               05  W15-RSRCC       PIC  9V99.

       COPY "W25.STK".

           03  W25-DEBIT       PIC S9(09)V99    COMP-3.
           03  W25-CREDIT      PIC S9(09)V99    COMP-3.
           03  W25-QUANT       PIC S9(09)V99    COMP-3.
           03  W25-QNT         PIC S9(09)V99    COMP-3.
           03  W25-AQUANT      PIC S9(09)V9(04) COMP-3.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       COPY "W75.STK".

       01  W80-EDIT.
           03  W80-CNO        PIC 9(04).
           03  W80-V1.
               05  W80-S7V2   PIC Z(06)9.99-.
           03  W80-V2.
               05  W80-S5V2   PIC Z(04)9.99-.
           03  W80-DTE.
              05  W80-DATE   PIC Z9/99/9999.
           03  W80-QNT.
               05  W80-5      PIC Z(04)9.99.
           03  W80-QNT1 REDEFINES W80-QNT.
               05  W80-3V2    PIC Z(02)9.9999.
           03  W80-NO.
               05  W80-5N     PIC 9(05).
               05  W80-REC REDEFINES W80-5N
               PIC ZZZZ9.
    03  W80-NO3 REDEFINES W80-NO.
        05  W80-3N     PIC ZZ9.
        05  FILLER     PIC X(02).
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

      *
      *    The following group level is only used to specify the colours
      *    The 05 levels are used to ACCEPT/DISPLAY data.
      *
       01  S05-DATA.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               05  S05.
                   07  LINE  7 COLUMN 27 PIC X(14) USING W10-ITEM AUTO.
               
               05  S05-ITM.
                   07  LINE  7 COLUMN 27 PIC X(14) USING W10-ITEM AUTO.
                   07          COLUMN 42 PIC X(03) USING W10-ITM AUTO.
               
               05  S05-EXT.
                   07  LINE  7 COLUMN 27 PIC X(18) USING W10-EXT-ITEM AUTO.
               
               05  S05-3.
                   07  LINE  7 COLUMN 27 PIC Z(03) USING W10-I3 AUTO.
               
               05  S05-4.
                   07  LINE  7 COLUMN 27 PIC Z(04) USING W10-I4 AUTO.
               
               05  S05-5.
                   07  LINE  7 COLUMN 27 PIC Z(05) USING W10-I5 AUTO.
               
               05  S05-6.
                   07  LINE  7 COLUMN 27 PIC Z(06) USING W10-I6 AUTO.
               
               05  S05-7.
                   07  LINE  7 COLUMN 27 PIC Z(07) USING W10-I7 AUTO.
               
               05  S05-8.
                   07  LINE  7 COLUMN 27 PIC Z(08) USING W10-I8 AUTO.
               
               05  S05-9.
                   07  LINE  7 COLUMN 27 PIC Z(09) USING W10-I9 AUTO.
               
               05  S05-10.
                   07  LINE  7 COLUMN 27 PIC Z(10) USING W10-I10 AUTO.
               
               05  S05-11.
                   07  LINE  7 COLUMN 27 PIC Z(11) USING W10-I11 AUTO.
               
               05  S05-12.
                   07  LINE  7 COLUMN 27 PIC Z(12) USING W10-I12 AUTO.
               
               05  S05-13.
                   07  LINE  7 COLUMN 27 PIC Z(13) USING W10-I13 AUTO.
               
               05  S05-14.
                   07  LINE  7 COLUMN 27 PIC Z(14) USING W10-I14 AUTO.

       01  S20.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE 11 COLUMN 20 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
               05  LINE 12 COLUMN 20 VALUE "³ Enter quantity Amend Password        ³".
               05  LINE 13 COLUMN 20 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S20A.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 12 COLUMN 52 PIC  X(06) USING WS-QPASSWORD AUTO SECURE.

      *    *************************************************************
      *    ****      STOCK ADJUST 'QUANTITIES' DISPLAY SCREEN     ****
      *    *************************************************************
       01  S27.
           03  LINE  2 COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
           VALUE "STOCK-ADJUST QUANTITIES".
           03  LINE  4 COLUMN  2 VALUE "This option allows you to adjust the quantity or value on a stock record.    ".
           03  LINE  5 COLUMN  2 VALUE "Use the minus key to reduce and no sign to increase the quantity or value    ".
           03  LINE  7 COLUMN 12 VALUE "Item Code     ".
           03  LINE  8 COLUMN 12 PIC X(14) FROM WS-DESC-H1.
           03  LINE  9 COLUMN 12 PIC X(14) FROM WS-DESC-H2.
           03  LINE 10 COLUMN 12 VALUE "Type          ".
           03          COLUMN 29 VALUE                  "A"                   FOREGROUND-COLOR Grey HIGHLIGHT.
           03          COLUMN 30 VALUE                   "djust,".
           03          COLUMN 36 VALUE                         "R"            FOREGROUND-COLOR Grey HIGHLIGHT.
           03          COLUMN 37 VALUE                          "eceive,".
           03          COLUMN 44 VALUE                                 "V"    FOREGROUND-COLOR Grey HIGHLIGHT.
           03          COLUMN 45 VALUE                                  "alue".
      *    *************************************************************
      *    ** R E F E R E N C E   N U M B E R - R E C E I V I N G   **
      *    *************************************************************
           03          COLUMN 50 VALUE                                      "Reference".
           03  LINE 11 COLUMN 31 VALUE                   "Current".
           03          COLUMN 44 VALUE                                "Movement".
           03          COLUMN 58 VALUE                                              "Move. Total".
           03  LINE 12 COLUMN 12 VALUE "Quantity      ".
           03  LINE 13 COLUMN 12 VALUE "Cost Price    ".
           03  LINE 14 COLUMN 12 VALUE "Sell. prices  ".
           03          COLUMN 28 PIC X(10) FROM WS-WS-HD.
           03          COLUMN 43 PIC X(10) FROM WS-CS-HD.
           03          COLUMN 58 PIC X(10) FROM WS-RT-HD.
           03  LINE 15 COLUMN 12 VALUE "VAT exclusive ".
           03  LINE 16 COLUMN 12 VALUE "VAT inclusive ".
           03  LINE 17 COLUMN 12 VALUE "Cur exclusive ".
      *
      *    ****    STOCK ADJUSTMENTS - DISPLAY SCREEN
      *
       01  S29.
           03  S29A BACKGROUND-COLOR Magenta.
               05  LINE  8 COLUMN 27 PIC X(30) FROM W10-DESC  FOREGROUND-COLOR Cyan HIGHLIGHT .
               05  LINE  9 COLUMN 27 PIC X(30) FROM W10-DESC2 FOREGROUND-COLOR Brown HIGHLIGHT.
               05  LINE 10 COLUMN 27 PIC X(01) FROM WS-TYPE   FOREGROUND-COLOR Brown HIGHLIGHT.
      *    *************************************************************
      *    ** R E F E R E N C E   N U M B E R - R E C E I V I N G   **
      *    *************************************************************
               05          COLUMN 60 PIC  X(08)         FROM WS-REFERENCE FOREGROUND-COLOR Brown HIGHLIGHT.
               05  LINE 12 COLUMN 27 PIC  Z(06)9.9(03)- FROM W10-QUANT    FOREGROUND-COLOR Cyan HIGHLIGHT .
               05  LINE 13 COLUMN 27 PIC  Z(06)9.9(04)  FROM W10-CCOST    FOREGROUND-COLOR Cyan HIGHLIGHT .
               05  LINE 15 COLUMN 27 PIC  Z(06)9.9(04)  FROM W10-CWSALE   FOREGROUND-COLOR Grey HIGHLIGHT .
               05          COLUMN 42 PIC  Z(06)9.9(04)  FROM W10-CCASH    FOREGROUND-COLOR Grey HIGHLIGHT .
               05          COLUMN 57 PIC  Z(06)9.9(04)  FROM W10-CSELL    FOREGROUND-COLOR Grey HIGHLIGHT .
               05  LINE 16 COLUMN 27 PIC  Z(08)9.9(02)  FROM W10-VWSALE   FOREGROUND-COLOR Grey HIGHLIGHT .
               05          COLUMN 42 PIC  Z(08)9.9(02)  FROM W10-VCASH    FOREGROUND-COLOR Grey HIGHLIGHT .
               05          COLUMN 57 PIC  Z(08)9.9(02)  FROM W10-VSELL    FOREGROUND-COLOR Grey HIGHLIGHT .
               05  LINE 17 COLUMN 27 PIC  Z(06)9.9(04)  FROM W10-CWSALE   FOREGROUND-COLOR Cyan HIGHLIGHT .
               05          COLUMN 42 PIC  Z(06)9.9(04)  FROM W10-CCASH    FOREGROUND-COLOR Cyan HIGHLIGHT .
               05          COLUMN 57 PIC  Z(06)9.9(04)  FROM W10-CSELL    FOREGROUND-COLOR Cyan HIGHLIGHT .

       01  S29-CASES.
           03  LINE 18 COLUMN 53 VALUE "Curr Cases".
           03          COLUMN 68 VALUE "Adj. Cases".

       01  SA29-CASES.
           03  BACKGROUND-COLOR Magenta.
               05  LINE 19 COLUMN 53 PIC Z(08)9- FROM W10-CASES       FOREGROUND-COLOR Cyan HIGHLIGHT.
               05          COLUMN 68 PIC Z(08)9- USING W25-CASES AUTO FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S29-TYPE.
           03  LINE 10 COLUMN 27 PIC X(01) USING WS-TYPE BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      *    *************************************************************
      *    ** R E F E R E N C E   N U M B E R - R E C E I V I N G   **
      *    *************************************************************
       01  S29-REFERENCE.
           03  LINE 10 COLUMN 60 PIC X(08) USING WS-REFERENCE BACKGROUND-COLOR Magenta FOREGROUND-COLOR Brown HIGHLIGHT AUTO.

       01  S29-INPUT.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               05  S29-DSP1.
                   07  S29-IA.
                       09  LINE 12 COLUMN 42 PIC Z(06)9.9(03)- USING W10-MQUANT.
                   07  S29-IB.
                       09  LINE 13 COLUMN 42 PIC Z(07)9.9(04) USING W10-MCOST.
                   07  S29-IC.
                       09  LINE 13 COLUMN 57 PIC Z(08)9.9(02)- USING W10-TCOST.
               05  S29-DSP2.
                   07  S29-ID.
                       09  LINE 15 COLUMN 27 PIC Z(06)9.9(04) USING W10-MWSALE.
                   07  S29-IE.
                       09  LINE 15 COLUMN 42 PIC Z(06)9.9(04) USING W10-MCASH.
                   07  S29-IF.
                       09  LINE 15 COLUMN 57 PIC Z(06)9.9(04) USING W10-MSELL.
                   07  S29-IG.
                       09  LINE 16 COLUMN 27 PIC Z(08)9.9(02) USING W10-VWSALE.
                   07  S29-IH.
                       09  LINE 16 COLUMN 42 PIC Z(08)9.9(02) USING W10-VCASH.
                   07  S29-II.
                       09  LINE 16 COLUMN 57 PIC Z(08)9.9(02) USING W10-VSELL.

       01  S30.
           05  LINE 16 COLUMN 50 PIC Z(06)9.9(03)- USING W10-MQUANT BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
       
       01  S35.
           05  LINE 13 COLUMN 51 PIC X(08) USING WS-REFERENCE BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT AUTO.

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
             PERFORM BM000.
             CLOSE RECOVER.

       AA49.
             EXIT PROGRAM.

       COPY  "AA50.LUP".

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

       COPY "DATE.CNV".

       COPY "AA300.STK".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000-READ        SECTION.

       COPY "CONTROL.RD".

       COPY "DEPART.RD".

       COPY "LEDTRF.RD".

       COPY "PARAM.RD".

       COPY "SHARED.RD".

       COPY "STKDEX.RD".

       COPY "STOCK.RD".

      *
      *   R E W R I T E   &    W R I T E   F I L E S   R O U T I N E S
      *
       AD000-WRITE        SECTION.

       COPY "CONTROL.WR".

       COPY "DEPART.WR".

       COPY "LEDTRF.WR".

       COPY "PARAM.WR".

       COPY "SHARED.WR".

       COPY "STKDEX.WR".

       COPY "STOCK.WR".

       COPY "APAC.HLP".

      /
      *       ****   *****   ***    ***   *   *  *****  ****   *   *
      *       *   *  *      *   *  *   *  *   *  *      *   *  *   *
      *       ****   ***    *      *   *  *   *  ***    ****    * *
      *       *   *  *      *   *  *   *   * *   *      *   *    *
      *       *   *  *****   ***    ***     *    *****  *   *    *
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *           THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
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
      *    *************************************************************
      *    ****          S T O C K   C A R D E X   F I L E          ****
      *    *************************************************************
       AY46.
             MOVE 46                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
             MOVE STX-REC1           TO REC-STKDEX.
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

      /
      *    ****    S T O C K   T A K E   Q U A N T I T I E S
      *
       BM000        SECTION 50.
       BM0.
             PERFORM ERASE-SCREEN.
             DISPLAY S27.
             PERFORM SAVE-SCREEN-2.
             DISPLAY S20.
             MOVE 12                 TO SHADE-ROW.
             MOVE 22                 TO SHADE-COL.
             MOVE 38                 TO SHADE-WIDTH.
             MOVE 2                  TO SHADE-LINES.
             PERFORM SCREEN-SHADOW.
             ACCEPT S20A.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING WS-QPASSWORD BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             MOVE 5                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-PW             TO W85-PASS.
             PERFORM RESTORE-SCREEN-2.
           IF NOT(WS-QPASSWORD = W85-SUPER(1) OR W85-SUPER(7) OR "ATNAUQ")
               MOVE "Invalid password" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BM999.
             MOVE ZERO               TO W25-DEBIT W25-CREDIT W25-QUANT W25-QNT.
             MOVE SPACE              TO WS-TYPE WS-REFERENCE.

       BM00.
             PERFORM ERASE-SCREEN.
             DISPLAY S27.
           IF WS-USE-ITM = "Y"
               DISPLAY "/Itm" AT 0721.
             MOVE SPACES             TO W10-EXT-ITEM.
           IF W10-STCK = "N"
               MOVE ZERO             TO W10-I14.
      *        EVALUATE W10-SLNGTH
      *          WHEN 3   MOVE ZERO  TO W10-I3            
      *          WHEN 4   MOVE ZERO  TO W10-I4            
      *          WHEN 5   MOVE ZERO  TO W10-I5            
      *          WHEN 6   MOVE ZERO  TO W10-I6            
      *          WHEN 7   MOVE ZERO  TO W10-I7            
      *          WHEN 8   MOVE ZERO  TO W10-I8            
      *          WHEN 9   MOVE ZERO  TO W10-I9            
      *          WHEN 10  MOVE ZERO  TO W10-I10            
      *          WHEN 11  MOVE ZERO  TO W10-I11            
      *          WHEN 12  MOVE ZERO  TO W10-I12            
      *          WHEN 13  MOVE ZERO  TO W10-I13            
      *          WHEN 14  MOVE ZERO  TO W10-I14
      *        END-EVALUATE.
       BM05.
             PERFORM AA300.
      *    *************************************************************
      *                     G E T   S T O C K   C O D E
      *    *************************************************************
      *    
      *      Is an 18 character stock/inventory item code being used?  
      *    
           IF WS-EXT-STK = "Y"
               ACCEPT S05-EXT
           ELSE
      *   
      *       Is an ALPHA-NUMERIC code being used?
      *    
           IF W10-STCK = "A"
      *    
      *     Is a 3 character extension code being used in addition to 
      *     the 14 character stock/inventory item code?               
      *    
               IF WS-USE-ITM = "Y"
                   ACCEPT S05-ITM
               ELSE
                   ACCEPT S05
               END-IF
      *    *************************************************************
      *                         NUMERIC CODES APPLY 
      *    *************************************************************
      *    
      *     Check the length of the numeric Stock/Inventory item code 
      *     being used and allow the User to key in the code.         
      *    
           ELSE
               EVALUATE W10-SLNGTH
      *                     3 Digit numeric Stock/Inventory item code
                 WHEN 3         ACCEPT S05-3
      *                     4 Digit numeric Stock/Inventory item code
                 WHEN 4         ACCEPT S05-4
      *                     5 Digit numeric Stock/Inventory item code
                 WHEN 5         ACCEPT S05-5
      *                     6 Digit numeric Stock/Inventory item code
                 WHEN 6         ACCEPT S05-6
      *                     7 Digit numeric Stock/Inventory item code
                 WHEN 7         ACCEPT S05-7
      *                     8 Digit numeric Stock/Inventory item code
                 WHEN 8         ACCEPT S05-8
      *                     9 Digit numeric Stock/Inventory item code
                 WHEN 9         ACCEPT S05-9
      *                    10 Digit numeric Stock/Inventory item code
                 WHEN 10        ACCEPT S05-10
      *                    11 Digit numeric Stock/Inventory item code
                 WHEN 11        ACCEPT S05-11
      *                    12 Digit numeric Stock/Inventory item code
                 WHEN 12        ACCEPT S05-12
      *                    13 Digit numeric Stock/Inventory item code
                 WHEN 13        ACCEPT S05-13
      *                    14 Digit numeric Stock/Inventory item code
                 WHEN 14        ACCEPT S05-14
               END-EVALUATE.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY MOVE SPACES TO W10-EXT-ITEM
                              GO TO BM999
                 WHEN F1-KEY  PERFORM HELP-ROUTINE
                              GO TO BM05
                 WHEN F2-KEY  MOVE "I"    TO W10-ETYPE
                 WHEN F3-KEY  MOVE "A"    TO W10-ETYPE
                 WHEN F4-KEY  MOVE "D"    TO W10-ETYPE
                 WHEN F5-KEY  MOVE "2"    TO W10-ETYPE
                 WHEN F6-KEY  MOVE "X"    TO W10-ETYPE
                 WHEN F9-KEY  MOVE "W"    TO W10-ETYPE
                 WHEN OTHER   PERFORM AA900-ALARM
                              GO TO BM05
               END-EVALUATE
               PERFORM AA50-LOOKUP
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
               IF W10-STCK = "A"
                   IF WS-EXT-STK = "Y"
                       DISPLAY S05-EXT
                   ELSE
                   IF WS-USE-ITM = "Y"
                       DISPLAY S05-ITM
                   ELSE
                       DISPLAY S05
                   END-IF
               ELSE
                   DISPLAY S05
               END-IF
               IF W10-EXT-ITEM = SPACES
                   GO TO BM05.
             DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
             PERFORM CLEAR-L50.
           IF W10-EXT-ITEM = SPACES
               GO TO BM999.
             MOVE ZERO               TO W25-VALUE W25-TOTAL.
             PERFORM CA000.
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
           IF W10-STCK = "A"
               IF WS-EXT-STK = "Y"
                   DISPLAY S05-EXT
               ELSE
               IF WS-USE-ITM = "Y"
                   DISPLAY S05-ITM
               ELSE
                  DISPLAY S05
               END-IF
           ELSE
               DISPLAY S05.
           IF WS-F-ERROR = 22
               MOVE "Z"              TO W10-ETYPE
               PERFORM AA50-LOOKUP
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
                IF W10-STCK = "A"
                    IF WS-EXT-STK = "Y"
                        DISPLAY S05-EXT
                    ELSE
                    IF WS-USE-ITM = "Y"
                        DISPLAY S05-ITM
                    ELSE
                        DISPLAY S05
                    END-IF
                ELSE
                    DISPLAY S05
                END-IF
                IF W10-EXT-ITEM = SPACES
                    GO TO BM999
                ELSE
                    GO TO BM05.

       BM10.
           IF STK-IND > 1
               MOVE "Not a Controlled or Priced item" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BM05.
             MOVE STK-LDG            TO DPT-CODE.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
           IF WS-F-ERROR = 7
               MOVE "Invalid Sales Ledger/Department Code" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BM00.
             MOVE ZERO               TO W25-CASES W10-MQUANT W10-MSELL W10-MCOST W10-TCOST.
           IF (WS-USE-CASES = "Y") AND (STK-USE-CASES = "Y")
               MOVE STK-CASES        TO W10-CASES
               DISPLAY S29-CASES.
             MOVE STK-CODE           TO W10-EXT-ITEM.
             MOVE STK-DESC           TO W10-DESC.
             MOVE STK-DESC2          TO W10-DESC2.
             MOVE STK-QUANT          TO W10-QUANT.
             MOVE STK-COST           TO W10-CCOST.
             MOVE STK-SELL           TO W10-CSELL  W10-MSELL.
             MOVE STK-CASH           TO W10-CCASH  W10-MCASH.
             MOVE STK-WSALE          TO W10-CWSALE W10-MWSALE.
             DISPLAY S29.
           IF (WS-USE-CASES = "Y") AND (STK-USE-CASES = "Y")
               DISPLAY SA29-CASES.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Allow the User to select the type which may be one of the ³
      *    ³ following: Adjust Quantity, Receive Stock or amend Value. ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BM15.
             ACCEPT S29-TYPE.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY GO TO BM05
                 WHEN OTHER   PERFORM AA900-ALARM
                              GO TO BM15
               END-EVALUATE
               GO TO BM15.
             CALL "CBL_TOUPPER" USING WS-TYPE BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-TYPE = "A" OR "R" OR "V")
               GO TO BM15.
           IF WS-TYPE = "V"
               GO TO BM500.
      *
      *    ****   A D J U S T M E N T ?
      *
           IF WS-TYPE = "A"
               GO TO BM20.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ For receiving stock, allow for a reference number to be   ³
      *    ³ captured. Used for consolodation purposes.         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BM18.
            DISPLAY "F7"                                                   AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                      "=View Reference codes for today,"                           WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                                      "F8"                         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                                        "=View specific Reference" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             ACCEPT S29-REFERENCE.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY GO TO BM15
                 WHEN F7-KEY  MOVE "D"  TO WS-ETYPE
                              PERFORM BM600 THRU BM699
                 WHEN F8-KEY  MOVE "R"  TO WS-ETYPE
                              PERFORM BM600 THRU BM699
                 WHEN OTHER   PERFORM AA900-ALARM
               END-EVALUATE
               GO TO BM18.
      *    *************************************************************
      *    ****     Q U A N T I T Y      ****
      *    *************************************************************
       BM20.
             ACCEPT S29-IA.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY GO TO BM18
      *
      *    ****    Before reference number inserted
      *
      *       GO TO BM15
                 WHEN OTHER   PERFORM AA900-ALARM
                              GO TO BM20
               END-EVALUATE
               GO TO BM20.
           IF NOT(W10-MQUANT = ZERO)
               IF WS-TYPE = "A"
                   MOVE W10-CCOST    TO W10-MCOST.
      *     IF W10-MQUANT < ZERO
      *         COMPUTE W10-MCOST = W10-MCOST * -1.
      *    *************************************************************
      *    ****                  C O S T  P R I C E                 ****
      *    *************************************************************
       BM25.
             ACCEPT S29-IB.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY GO TO BM15
                 WHEN OTHER   PERFORM AA900-ALARM
                              GO TO BM25
               END-EVALUATE
               GO TO BM25.
           IF ((W10-MCOST NOT = ZERO) AND (W10-MQUANT NOT = ZERO))
               COMPUTE W10-TCOST ROUNDED = (W10-MQUANT * W10-MCOST)
               DISPLAY S29-IC
               GO TO BM35.
      *    *************************************************************
      *    ** I S   C O S T   P R I C E   B E I N G   A M E N D E D ? **
      *    *************************************************************
           IF NOT(W10-MCOST = ZERO)
               GO TO BM35.
      *    *************************************************************
      *    ****                 T O T A L   C O S T                 ****
      *    *************************************************************
       BM30.
             ACCEPT S29-IC.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY GO TO BM15
                 WHEN OTHER   PERFORM AA900-ALARM
                              GO TO BM30
               END-EVALUATE
               GO TO BM30.
           IF W10-TCOST = ZERO
               GO TO BM25.
      *    *************************************************************
      *    ****   C A L C U L A T E   U N I T C O S T   P R I C E
      *    *************************************************************
           IF NOT(W10-MQUANT = ZERO)
               COMPUTE W10-MCOST ROUNDED = (W10-TCOST / W10-MQUANT).
             DISPLAY S29-IB.

       BM35.
           IF STK-MARKUP = ZERO
               PERFORM BN000.
      *    *************************************************************
      *    ****    A V E R A G E   C O S T
      *    *************************************************************
           IF STK-QUANT < 0.001
               MOVE STK-COST         TO STK-AVGE.
           IF STK-IND = 0
               IF STK-QUANT > ZERO
                  COMPUTE W25-VALUE = W25-VALUE + (STK-QUANT * STK-AVGE)
                  END-IF
                  COMPUTE W25-TOTAL = W25-TOTAL + W10-TCOST.
           IF (W10-MQUANT = ZERO) OR (W10-MCOST = ZERO)
               MOVE STK-AVGE         TO W10-AVRG
               GO TO BM40.
             ADD W10-MQUANT STK-QUANT GIVING W25-AQUANT.
           IF W25-VALUE = ZERO
               MOVE W10-MCOST        TO W10-AVRG
           ELSE
               COMPUTE W10-AVRG = (W25-VALUE + W25-TOTAL) / W25-AQUANT.
      *    *************************************************************
      *    ****   H A S   C O S T   P R I C E C H A N G E D ?
      *    *************************************************************
       BM40.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
           IF W10-MCOST = STK-COST
               GO TO BM50.
      *    *************************************************************
      *      ***** CALCULATE NEW SELLING PRICE *****
      *    *************************************************************
           IF W10-AVRG > W10-MCOST
               COMPUTE W10-MSELL ROUNDED  = (W10-AVRG + (W10-AVRG * STK-MARKUP / 100.00000))
               COMPUTE W10-MWSALE ROUNDED = (W10-AVRG + (W10-AVRG * STK-WMARKUP / 100.00000))
               COMPUTE W10-MCASH ROUNDED  = (W10-AVRG + (W10-AVRG * STK-CMARKUP / 100.00000))
           ELSE
               COMPUTE W10-MSELL ROUNDED  = (W10-MCOST + (W10-MCOST * STK-MARKUP / 100.00000))
               COMPUTE W10-MWSALE ROUNDED = (W10-MCOST + (W10-MCOST * STK-WMARKUP / 100.00000))
               COMPUTE W10-MCASH ROUNDED  = (W10-MCOST + (W10-MCOST * STK-CMARKUP / 100.00000)).
           IF W05-ROUND = "E"
               MOVE W10-MSELL        TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL        TO W10-MSELL
               MOVE W10-MWSALE       TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL        TO W10-MWSALE
               MOVE W10-MCASH        TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL        TO W10-MCASH.
             DISPLAY S29-DSP2.

       BM45.
           IF STK-TAX NOT = ZERO
               COMPUTE W10-VSELL ROUNDED  = W10-MSELL + (W10-MSELL * W05-RTE)
               COMPUTE W10-VWSALE ROUNDED = W10-MWSALE + (W10-MWSALE * W05-RTE)
               COMPUTE W10-VCASH ROUNDED  = W10-MCASH + (W10-MCASH * W05-RTE)
           ELSE
               MOVE W10-MWSALE       TO W10-VWSALE
               MOVE W10-MCASH        TO W10-VCASH
               MOVE W10-MSELL        TO W10-VSELL.
           IF W05-ROUND = "I"
               MOVE W10-VSELL        TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL        TO W10-VSELL
               MOVE W10-VWSALE       TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL        TO W10-VWSALE
               MOVE W10-VCASH        TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL        TO W10-VCASH
               PERFORM CB200
               MOVE W10-WSALE        TO W10-MWSALE
               MOVE W10-CASH         TO W10-MCASH
               MOVE W10-SELL         TO W10-MSELL.
             DISPLAY S29-DSP2.

       BM50.
           IF W10-MSELL = STK-SELL
               GO TO BM60.
             MOVE SPACE   TO WS-OPTION.

       BM55.
             MOVE "Use new selling prices Y/N  [ ]" TO WS-ERR-MES.
             MOVE 17                 TO SLIN.
             PERFORM OPT-SETUP THRU OPT-EXIT.
           IF NOT(WS-OPTION = "Y" OR "N")
               GO TO BM55.
           IF WS-OPTION = "Y"
               GO TO BM60.
           IF NOT(STK-WSALE = ZERO)
               MOVE STK-WSALE        TO W10-MWSALE.
           IF NOT(STK-CASH = ZERO)
               MOVE STK-CASH         TO W10-MCASH.
           IF NOT(STK-SELL = ZERO)
               MOVE STK-SELL         TO W10-MSELL.
             PERFORM BM45.

       BM60.
           IF NOT(STK-TAX = ZERO)
               GO TO BM70.

       BM65.
             MOVE "Adjust selling prices Y/N  [N]" TO WS-ERR-MES.
             MOVE "N"                TO WS-OPTION.
             MOVE 16                 TO SLIN.
             PERFORM OPT-SETUP THRU OPT-EXIT.
           IF NOT(WS-OPTION = "Y" OR "N")
               GO TO BM65.
           IF WS-OPTION = "N"
               GO TO BM105.
             GO TO BM90.

       BM70.
             MOVE "Adjust 'E'x/'I'nclusive price or 'N'one  [E]" TO WS-ERR-MES.
             MOVE "E"                TO WS-OPTION.
             MOVE 16                 TO SLIN.
             PERFORM OPT-SETUP THRU OPT-EXIT.
           IF NOT(WS-OPTION = "E" OR "I" OR "N")
               GO TO BM70.
           IF WS-OPTION = "N"
               GO TO BM105.
           IF WS-OPTION = "E"
               GO TO BM90.

       BM75.
             ACCEPT S29-IG.
           IF W10-VWSALE = ZERO
               GO TO BM75.
             COMPUTE W10-MWSALE ROUNDED = (W10-VWSALE / (100.000 + W05-RATE)) * 100.000.
           IF W10-MWSALE < W10-MCOST
               MOVE WS-ER2           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BM75.

       BM80.
             ACCEPT S29-IH.
           IF W10-VCASH = ZERO
               GO TO BM80.
             COMPUTE W10-MCASH ROUNDED = (W10-VCASH / (100.000 + W05-RATE)) * 100.000.
           IF W10-MCASH < W10-MCOST
               MOVE WS-ER2           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BM80.

       BM85.
             ACCEPT S29-II.
           IF W10-VSELL = ZERO
               GO TO BM80.
             COMPUTE W10-MSELL ROUNDED = (W10-VSELL / (100.000 + W05-RATE)) * 100.000.
           IF W10-MSELL < W10-MCOST
               MOVE WS-ER2           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BM85.
             GO TO BM105.

       BM90.
             ACCEPT S29-ID.
           IF W10-MWSALE < W10-MCOST
               MOVE WS-ER2           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BM90.

       BM95.
             ACCEPT S29-IE.
           IF W10-MCASH < W10-MCOST
               MOVE WS-ER2           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BM95.

       BM100.
             ACCEPT S29-IF.
           IF W10-MSELL < W10-MCOST
               MOVE WS-ER2           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BM100.
              PERFORM BM45.

       BM105.
             EXIT.

       BM110.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO BM20.
           IF (WS-USE-CASES = "Y") AND (STK-USE-CASES = "Y")
               GO TO BM125
           ELSE
               GO TO BM130.
      *    *************************************************************
      *    ****            U P D A T E   C A S E S
      *    *************************************************************
       BM125.
             ACCEPT SA29-CASES.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO BM125.
           IF W25-CASES = ZERO
               IF W10-MQUANT = ZERO
                   GO TO BM15.

       BM130.
           IF NOT(STK-USE-CASES = "Y")
               IF W10-MQUANT = ZERO
                   GO TO BM15.
             MOVE ZERO               TO W25-VALUE W25-TOTAL.
      *    *************************************************************
      *    ****             S T A R T   U P D A T E
      *    *************************************************************
             PERFORM AY60 THRU AY999.
             MOVE 0                  TO WS-ACTION.
             PERFORM AY05 THRU AY59.
      *    *************************************************************
      *    ****             A V E R A G E   C O S T
      *    *************************************************************
           IF STK-QUANT < 0.001
               MOVE STK-COST         TO STK-AVGE.
           IF STK-IND = 0
               IF STK-QUANT > ZERO
                   COMPUTE W25-VALUE = W25-VALUE + (STK-QUANT * STK-AVGE)
               END-IF
               COMPUTE W25-TOTAL = W25-TOTAL + W10-TCOST.
             ADD W10-MQUANT          TO STK-QUANT.
           IF NOT(W10-MCOST < ZERO)
               MOVE  W10-MCOST       TO STK-COST.
           IF W25-VALUE = ZERO
               MOVE W10-MCOST        TO STK-AVGE
           ELSE
               COMPUTE STK-AVGE = (W25-VALUE + W25-TOTAL) / STK-QUANT.
           IF (WS-USE-CASES = "Y") AND (STK-USE-CASES = "Y")
               ADD W25-CASES         TO STK-CASES.
           IF WS-TYPE = "R"
               MOVE W12-T-YMD        TO STK-PUR.
             MOVE W10-MSELL          TO STK-SELL.
             MOVE W10-MCASH          TO STK-CASH.
             MOVE W10-MWSALE         TO STK-WSALE.
             MOVE W25-TOTAL          TO W25-ADJUST.
           IF NOT(W25-ADJUST = ZERO)
               MOVE STK-LDG          TO DPT-CODE
               PERFORM READ-DEPART-LOCK THRU READ-DEPART-EXIT
               IF WS-F-ERROR = 7
                   MOVE "Invalid Sales Ledger Code" TO WS-ERR-MES
                   PERFORM ERROR-MESSAGE
                   PERFORM AY70 THRU AY999
                   GO TO BM00
               END-IF
               PERFORM AY38 THRU AY59
               IF WS-TYPE = "A"
                   ADD W25-ADJUST TO DPT-ADJ-DAY DPT-ADJ-MTD DPT-ADJ-YTD STK-ADJMTD STK-ADJYTD
               ELSE
               IF W25-ADJUST > ZERO
                   ADD W25-ADJUST TO DPT-PUR-DAY DPT-PUR-MTD DPT-PUR-YTD
               ELSE
                   ADD W25-ADJUST TO DPT-PRET-MTD DPT-PRET-YTD DPT-PRET-DAY
               END-IF
           END-IF
           PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT
           IF WX-INT = "Y"
               MOVE W12-T-YMD        TO WX-DATE
               MOVE STK-LDG          TO WX-LEDG
               MOVE DPT-PUR-GL       TO WX-AC
               MOVE W25-ADJUST       TO WX-VALUE
               IF WS-TYPE = "A"
                   MOVE 28           TO WX-TYPE
                   MOVE "Stock Adjust" TO WX-NAR
                   PERFORM GL-TRF-UPDATE
                   MOVE 27           TO WX-TYPE
                   MOVE WX-ADJGL     TO WX-AC
                   MULTIPLY -1       BY WX-VALUE
                   PERFORM GL-TRF-UPDATE
               ELSE
               IF W25-ADJUST > ZERO
                   MOVE 06           TO WX-TYPE
                   MOVE "Stock Received" TO WX-NAR
                   PERFORM GL-TRF-UPDATE
                   MOVE 02           TO WX-TYPE
                   MOVE "RECV"       TO WX-LEDG
                   MOVE WX-CREGL     TO WX-AC
                   MULTIPLY -1       BY WX-VALUE
                   PERFORM GL-TRF-UPDATE
               ELSE
                   MOVE 07           TO WX-TYPE
                   MOVE "Stock Returned" TO WX-NAR
                   PERFORM GL-TRF-UPDATE
                   MOVE 02           TO WX-TYPE
                   MOVE "RTRN"       TO WX-LEDG
                   MOVE WX-CREGL     TO WX-AC
                   MULTIPLY -1       BY WX-VALUE
                   PERFORM GL-TRF-UPDATE.
           IF WS-CARDEX = "Y"
               IF NOT(W10-MQUANT = ZERO)
                   INITIALIZE STX-REC1
                   ACCEPT WS-STIME FROM TIME
                   MOVE W10-EXT-ITEM TO STX-EXT-CODE
                   MOVE WS-REFERENCE TO STX-GRN
                   MOVE W12-T-YMD    TO STX-DTE
                   MOVE WS-STIME     TO STX-TIME
                   MOVE ZERO         TO STX-SEQ
                   MOVE LS0-NO       TO STX-USER
                   MOVE "STKADJ"     TO STX-PROG
                   MOVE LS-USER      TO STX-WS
                   MOVE W10-MQUANT   TO STX-QNT
                   COMPUTE STX-COST ROUNDED = (W10-MCOST * STX-QNT)
                   COMPUTE STX-SELL ROUNDED = (W10-MSELL * STX-QNT)
                   IF WS-TYPE = "A"
                       MOVE "Stock Adjust"   TO STX-REMARKS
                       MOVE "A"              TO STX-TYPE
                   ELSE
                   IF W25-ADJUST > ZERO
                       MOVE "Stock Received" TO STX-REMARKS
                       MOVE "F"              TO STX-TYPE
                   ELSE
                       MOVE "Stock Returned" TO STX-REMARKS
                       MOVE "G"              TO STX-TYPE
                   END-IF
               END-IF
               IF STX-GRN = SPACES
                   MOVE STK-ADJREF   TO STX-GRN
                   IF STK-ADJREF < 99999999
                       ADD 1         TO STK-ADJREF
                   ELSE
                       MOVE 1        TO STK-ADJREF
                   END-IF
               END-IF
               PERFORM WRITE-STKDEX THRU WRITE-STKDEX-EXIT
               MOVE 1                TO WS-ACTION
               PERFORM AY46 THRU AY59.
               PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
           IF W25-ADJUST < ZERO
               ADD W25-ADJUST        TO W25-CREDIT
               ADD W10-MQUANT        TO W25-QNT
           ELSE
               ADD W25-ADJUST        TO W25-DEBIT
               ADD W10-MQUANT        TO W25-QUANT.
             ADD W25-ADJUST          TO STK-TOTAL.
             PERFORM AY70 THRU AY999.

       BM135.
             ADD W25-QNT W25-QUANT    GIVING W80-5.
             ADD W25-DEBIT W25-CREDIT GIVING W80-S7V2.
             DISPLAY "Net Adjustment - "                           AT 2104 WITH FOREGROUND-COLOR Cyan  HIGHLIGHT
                                      "Units"                              WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                             W80-QNT               AT 2127 WITH FOREGROUND-COLOR Cyan  HIGHLIGHT BACKGROUND-COLOR Magenta
                                                     "Value"       AT 2136 WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                                            W80-V1 AT 2142 WITH FOREGROUND-COLOR Cyan  HIGHLIGHT BACKGROUND-COLOR Magenta.
             MOVE "N"                TO WS-OPTION.

       BM140.
             MOVE SPACES             TO WS-ERR-MES.
             STRING "'N'ext, 'E'xit  [" DELIMITED SIZE WS-OPTION DELIMITED SIZE "]" DELIMITED SIZE INTO WS-ERR-MES.
             PERFORM OPT-MESSAGE.
           IF NOT(WS-OPTION = "E" OR "N")
               GO TO BM140.
           IF WS-OPTION = "N"
               GO TO BM00.
             GO TO BM999.

       BM500.
             ACCEPT S29-IC.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY GO TO BM15
                 WHEN OTHER   PERFORM AA900-ALARM
                              GO TO BM500
               END-EVALUATE
               GO TO BM500.
           IF W10-TCOST = ZERO
               GO TO BM15.
             PERFORM SAVE-SCREEN-3.
             MOVE 16                 TO SHADE-ROW.
             MOVE 21                 TO SHADE-COL.
             MOVE 43                 TO SHADE-WIDTH.
             MOVE 2                  TO SHADE-LINES.
             DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1519 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black
                                                                 "¿"        WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "³ Apply adjustment to Quantity              " AT 1619 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black
                                                                 "³"        WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "À"                                            AT 1719 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black
                      "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"        WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
             PERFORM SCREEN-SHADOW.

       BM505.
             ACCEPT S30.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY GO TO BM15
                 WHEN OTHER   PERFORM AA900-ALARM
                              GO TO BM505
               END-EVALUATE.
           IF W10-MQUANT = ZERO
               PERFORM RESTORE-SCREEN-3
               GO TO BM15.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO BM505.
             PERFORM RESTORE-SCREEN-3.
      *    *************************************************************
      *    ****    S T A R T   U P D A T E
      *    *************************************************************
             PERFORM AY60 THRU AY999.
             MOVE 0                  TO WS-ACTION.
             PERFORM AY05 THRU AY59.
             COMPUTE W25-SCOST = (STK-QUANT * STK-AVGE) + W10-TCOST.
             COMPUTE W10-AVRG = W25-SCOST / STK-QUANT.
             COMPUTE W10-MCOST ROUNDED = W10-TCOST / W10-MQUANT.
             MOVE STK-LDG            TO DPT-CODE.
             PERFORM READ-DEPART-LOCK THRU READ-DEPART-EXIT.
           IF WS-F-ERROR = 7
               MOVE "Invalid Sales Ledger Code" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               PERFORM AY70 THRU AY999
               GO TO BM00.
             PERFORM AY38 THRU AY59.
             ADD W10-TCOST           TO DPT-ADJ-DAY DPT-ADJ-MTD DPT-ADJ-YTD STK-ADJMTD STK-ADJYTD.
           IF WX-INT = "Y"
               MOVE W12-T-YMD        TO WX-DATE
               MOVE STK-LDG          TO WX-LEDG
               MOVE DPT-PUR-GL       TO WX-AC
               MOVE W10-TCOST        TO WX-VALUE
               MOVE 28               TO WX-TYPE
               MOVE "Stock Adjust"   TO WX-NAR
               PERFORM GL-TRF-UPDATE
               MOVE 27               TO WX-TYPE
               MOVE WX-ADJGL         TO WX-AC
               MULTIPLY -1           BY WX-VALUE
               PERFORM GL-TRF-UPDATE.
           IF WS-CARDEX = "Y"
               INITIALIZE STX-REC1
               ACCEPT WS-STIME FROM TIME
               MOVE W10-EXT-ITEM     TO STX-EXT-CODE
               MOVE W12-T-YMD        TO STX-DTE
               MOVE WS-STIME         TO STX-TIME
               MOVE ZERO             TO STX-SEQ
               MOVE LS0-NO           TO STX-USER
               MOVE "STKADJ"         TO STX-PROG
               MOVE LS-USER          TO STX-WS
               MOVE ZERO             TO STX-QNT STX-SELL
               MOVE W10-TCOST        TO STX-COST
               MOVE "Stock Adjust Value" TO STX-REMARKS
               MOVE "B"              TO STX-TYPE
               PERFORM WRITE-STKDEX THRU WRITE-STKDEX-EXIT
               MOVE 1                TO WS-ACTION
               PERFORM AY46 THRU AY59.
             MOVE W10-AVRG           TO STK-AVGE.
             ADD W10-MCOST           TO STK-COST.
             PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
           IF W10-TCOST < ZERO
               ADD W10-TCOST         TO W25-CREDIT
           ELSE
               ADD W10-TCOST         TO W25-DEBIT.
               ADD W10-TCOST         TO STK-TOTAL.
               PERFORM AY70 THRU AY999.
             GO TO BM135.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Extract details of stock received using stock adjustments ³
      *    ³ procedure for thr current day.                            ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BM600.
             OPEN OUTPUT ADJREF.
             CLOSE ADJREF.
             OPEN I-O ADJREF.
           IF WS-ETYPE = "R"
               GO TO BM610.
             MOVE W12-T-YMD          TO STX-DTE.
             MOVE ZERO               TO STX-TIME.
             MOVE SPACES             TO STX-EXT-CODE.
             PERFORM START-AT-STDX-DATE THRU READ-STKDEX-EXIT.
           IF WS-F-ERROR = 46
               MOVE "No details for today" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BM699.
            PERFORM SAVE-SCREEN.

       BM605.
             PERFORM READ-STKDEX-NEXT THRU READ-STKDEX-EXIT.
           IF WS-F-ERROR = 46
               GO TO BM620.
      *    *************************************************************
      *    ****         STOP WHEN REFERENCE NUMBER CHANGES          ****
      *    *************************************************************
           IF WS-ETYPE = "R"
               IF NOT(STX-GRN = WS-REFERENCE)
                   GO TO BM620.
      *    *************************************************************
      *    ****         Check if this is stock received.            ****
      *    *************************************************************
           IF NOT Received
               GO TO BM605.
             MOVE STX-GRN            TO ADJ-CODE.
             MOVE STX-DTE            TO ADJ-DATE.
             READ ADJREF KEY IS ADJ-DTEREF.
           IF NOT(WS-STATUS = "00")
               INITIALIZE ADJ-RECORD
               MOVE STX-GRN          TO ADJ-CODE
               MOVE STX-DTE          TO ADJ-DATE
               MOVE ZERO             TO ADJ-ENTRIES
               WRITE ADJ-RECORD
               IF NOT(WS-STAT1 = "0")
                   GO TO BM685.
             ADD 1                   TO ADJ-ENTRIES.
             REWRITE ADJ-RECORD.
           IF NOT(WS-STAT1 = "0")
               GO TO BM685.
             GO TO BM605.

       BM610.
             PERFORM SAVE-SCREEN.
             DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 1220 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "³                                      ³" AT 1320 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY  "Keyin the reference required:"           AT 1321 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Blue HIGHLIGHT.
             DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 1420 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.

       BM615.
             ACCEPT S35.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY GO TO BM690
                 WHEN OTHER   PERFORM AA900-ALARM
               END-EVALUATE
               GO TO BM615.
           IF WS-REFERENCE = SPACES
               GO TO BM615.
             MOVE WS-REFERENCE       TO STX-GRN.
             MOVE ZERO               TO STX-DTE STX-TIME.
             PERFORM START-AT-STDX-GRN THRU READ-STKDEX-EXIT.
           IF WS-F-ERROR = 46
               MOVE "No details for reference requested" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BM615.
             GO TO BM605.

       BM620.
             DISPLAY "ÚÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄ¿" AT 0613 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey.
             DISPLAY "³Ref. No.³   Date   ³Entries³Ref. No.³   Date   ³Entries³" AT 0713 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey.
             DISPLAY "ÃÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄ´" AT 0813 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey.
             MOVE 0913               TO CPOS.
             PERFORM VARYING CLIN FROM 9 BY 1 UNTIL CLIN > 19
               DISPLAY "³        ³          ³       ³        ³          ³       ³" AT CPOS WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey
             END-PERFORM.
             DISPLAY "ÀÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÙ" AT 2013 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey.

       BM625.
             MOVE SPACES             TO ADJ-CODE.
             MOVE ZERO               TO ADJ-DATE.
             MOVE 0914               TO CPOS.
             START ADJREF KEY >= ADJ-DTEREF.
           IF NOT(WS-STATUS = "00")
               GO TO BM685.

       BM630.
             READ ADJREF NEXT.
           IF (WS-STATUS = "23") OR (WS-STAT1 = "1")
               GO TO BM640.
             DISPLAY ADJ-CODE AT CPOS WITH FOREGROUND-COLOR Magenta BACKGROUND-COLOR Grey.
             ADD 9                   TO CCOL.
             MOVE ADJ-DATE           TO W22-DTE2.
             PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT.
             MOVE W22-DTE3           TO W80-DATE.
             DISPLAY W80-DTE AT CPOS WITH FOREGROUND-COLOR Magenta BACKGROUND-COLOR Grey.
             ADD 13                  TO CCOL.
             MOVE ADJ-ENTRIES        TO W80-3N.
             DISPLAY W80-3N AT CPOS WITH FOREGROUND-COLOR Magenta BACKGROUND-COLOR Grey.
           IF CCOL < 40
               ADD 6                 TO CCOL
           ELSE
               ADD 1                 TO CLIN
               MOVE 14               TO CCOL.
           IF CLIN < 20
               GO TO BM630.
             PERFORM CLEAR-L50.
             DISPLAY "Press any key to continue" AT 5002 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             MOVE 0914               TO CPOS.
             GO TO BM630.

       BM640.
             PERFORM CLEAR-L50.
             DISPLAY "Press any key to continue" AT 5002 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             GO TO BM690.

       BM685.
             MOVE "Error while extracting details" TO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.

       BM690.
             PERFORM RESTORE-SCREEN.
             CLOSE ADJREF.

       BM699.
             EXIT.

       BM999.
             EXIT.
      *    *************************************************************
      *    ****    I N S E R T   M A R K U P   P E R C E N T A G E S
      *    *************************************************************
       BN000      SECTION 51.
       BN00.
             MOVE STK-LDG            TO DPT-CODE.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
             MOVE 1                  TO WS-S9.

       BN05.
             MOVE DPT-MKUP(WS-S9)    TO STK-MKUP(WS-S9).
             MOVE DPT-CMKUP(WS-S9)   TO STK-CMKUP(WS-S9).
             MOVE DPT-WMKUP(WS-S9)   TO STK-WMKUP(WS-S9).
           IF WS-S9 < 4
               ADD 1                 TO WS-S9
               GO TO BN05.
             PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.

       BN99.
            EXIT.
      *    *************************************************************
      *      *****          CALCULATE NEW SELLING PRICE          *****
      *    *************************************************************
       BN100        SECTION 52.
       BN101.
           IF W10-AVRG > W10-MCOST
               COMPUTE W10-MSELL ROUNDED = (W10-AVRG + (W10-AVRG * STK-MARKUP / 100.00000))
               COMPUTE W10-MWSALE ROUNDED  = (W10-AVRG + (W10-AVRG * STK-WMARKUP / 100.00000))
               COMPUTE W10-MCASH ROUNDED = (W10-AVRG + (W10-AVRG * STK-CMARKUP / 100.00000))
           ELSE
               COMPUTE W10-MSELL ROUNDED = (W10-MCOST + (W10-MCOST * STK-MARKUP / 100.00000))
               COMPUTE W10-MWSALE ROUNDED = (W10-MCOST + (W10-MCOST * STK-WMARKUP / 100.00000))
               COMPUTE W10-MCASH ROUNDED = (W10-MCOST + (W10-MCOST * STK-CMARKUP / 100.00000)).
           IF W05-ROUND = "E"
               MOVE W10-MSELL        TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL        TO W10-MSELL
               MOVE W10-MWSALE       TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL        TO W10-MWSALE
               MOVE W10-MCASH        TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL        TO W10-MCASH.

       BN102A.
           IF STK-TAX NOT = ZERO
               COMPUTE W10-VSELL ROUNDED = W10-MSELL + (W10-MSELL * W05-RTE)
               COMPUTE W10-VWSALE ROUNDED = W10-MWSALE + (W10-MWSALE * W05-RTE)
               COMPUTE W10-VCASH ROUNDED = W10-MCASH + (W10-MCASH * W05-RTE)
           ELSE
               MOVE W10-MWSALE       TO W10-VWSALE
               MOVE W10-MCASH        TO W10-VCASH
               MOVE W10-MSELL        TO W10-VSELL.
           IF W05-ROUND = "I"
               MOVE W10-VSELL        TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL        TO W10-VSELL
               MOVE W10-VWSALE       TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL        TO W10-VWSALE
               MOVE W10-VCASH        TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL        TO W10-VCASH
               PERFORM CB200.
             DISPLAY S29-DSP2.

       BN102B.
           IF W10-MSELL = STK-SELL
               GO TO BN102D.
             MOVE SPACE              TO WS-OPTION.

       BN102C.
             MOVE "Use new selling prices Y/N  [ ]" TO WS-ERR-MES.
             MOVE 17                 TO SLIN.
             PERFORM OPT-SETUP THRU OPT-EXIT.
           IF NOT(WS-OPTION = "Y" OR "N")
               GO TO BN102C.
           IF WS-OPTION = "Y"
               GO TO BN102D.
           IF NOT(STK-WSALE = ZERO)
               MOVE STK-WSALE        TO W10-MWSALE.
           IF NOT(STK-CASH = ZERO)
               MOVE STK-CASH         TO W10-MCASH.
           IF NOT(STK-SELL = ZERO)
               MOVE STK-SELL          TO W10-MSELL.
             PERFORM BN102A.

       BN102D.
           IF NOT(STK-TAX = ZERO)
               GO TO BN102E.

       BN102DA.
             MOVE "Adjust selling prices Y/N  [N]" TO WS-ERR-MES.
             MOVE "N"                TO WS-OPTION.
             MOVE 16                 TO SLIN.
             PERFORM OPT-SETUP THRU OPT-EXIT.
           IF NOT(WS-OPTION = "Y" OR "N")
               GO TO BN102E.
           IF WS-OPTION = "N"
               GO TO BN103B-1.
             GO TO BN103.

       BN102E.
             MOVE "Adjust 'E'x/'I'nclusive price or 'N'one  [E]" TO WS-ERR-MES.
             MOVE "E"                TO WS-OPTION.
             MOVE 16                 TO SLIN.
             PERFORM OPT-SETUP THRU OPT-EXIT.
           IF NOT(WS-OPTION = "E" OR "I" OR "N")
               GO TO BN102E.
           IF WS-OPTION = "N"
               GO TO BN103B-1.
           IF WS-OPTION = "E"
               GO TO BN103.

       BN102F.
             ACCEPT S29-IG.
           IF W10-VWSALE = ZERO
               GO TO BN102F.
             COMPUTE W10-MWSALE ROUNDED = (W10-VWSALE / (100.000 + W05-RATE)) * 100.000
           IF W10-MWSALE < W10-MCOST
               MOVE WS-ER2           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BN102F.

       BN102G.
             ACCEPT S29-IH.
           IF W10-VCASH = ZERO
               GO TO BN102G.
             COMPUTE W10-MCASH ROUNDED = (W10-VCASH / (100.000 + W05-RATE)) * 100.000
           IF W10-MCASH < W10-MCOST
               MOVE WS-ER2           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BN102G.

       BN102H.
             ACCEPT S29-II.
           IF W10-VSELL = ZERO
               GO TO BN102G.
             COMPUTE W10-MSELL ROUNDED = (W10-VSELL / (100.000 + W05-RATE)) * 100.000
           IF W10-MSELL < W10-MCOST
               MOVE WS-ER2           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BN102H.
             GO TO BN103B-1.

       BN103.
            ACCEPT S29-ID.
          IF W10-MWSALE < W10-MCOST
        MOVE WS-ER2  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BN103.

       BN103A.
             ACCEPT S29-IE.
           IF W10-MCASH < W10-MCOST
               MOVE WS-ER2           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BN103A.

       BN103B.
             ACCEPT S29-IF.
           IF W10-MSELL < W10-MCOST
               MOVE WS-ER2  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BN103B.
             PERFORM BN102A.

       BN103B-1.
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
            MOVE 6   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF PAR-INTEGRATE NOT = "Y"
               MOVE "N"          TO WX-INT
           ELSE
               MOVE "Y"          TO WX-INT.
             MOVE PAR-DEBGL      TO WX-DEBGL.
             MOVE PAR-CREGL      TO WX-CREGL.
             MOVE PAR-INTGL      TO WX-INTGL.
             MOVE PAR-BNKGL      TO WX-BNKGL.
             MOVE PAR-UNPROF     TO WX-UNPROF.
             MOVE PAR-REDGL      TO WX-REDGL.
             MOVE PAR-ADJGL      TO WX-ADJGL.
             MOVE PAR-RLGL       TO WX-RLGL.
             MOVE PAR-DSGL       TO WX-DSGL.
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
               WHEN 46         MOVE W02-STKDEX  TO WS-FILE
                               MOVE STX-KEY     TO WS-KEYX
               WHEN OTHER      MOVE "ERROR"     TO WS-FILE
                               MOVE "STP018"    TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
