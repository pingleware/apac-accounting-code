      $SET LINKCOUNT"512" GNT"STPENQ.GNT"
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
      *    ******   ********  *******   ********  **    **   ******    *
      *   **    **     **     **    **  **        ***   **  **    **   *
      *   **           **     **    **  **        ****  **  **    **   *
      *    ******      **     *******   ****      ** ** **  **    **   *
      *         **     **     **        **        **  ****  **    **   *
      *   **    **     **     **        **        **   ***  **   ***   *
      *    ******      **     **        ********  **    **   ******    *
      *                                                           **   *
      *     ENGLISH                                                    *
      *                                                                *
      *     S T O C K   E N Q U I R Y   P R O G R A M                  *
      *                                                                *
      *       Version 9.04.01 - September 2016                         *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  September 2005 - F8 function introduced to view both the      *
      *                   last supplier as well as the alternate       *
      *                   supplier.                                    *
      *                 - See also STP006: Amend Supplier              *
      *                                                                *
      *  September 2005 - Details of JOBS introduced. New sort key     *
      *                   JOB-DETAILS = JOB-TYPE JOB-EXT-ITEM JOB-REF  *
      *                                                                *
      *  May 2006       - New sort key (Lookup from Stock Item)        *
      *                   JOB-CODE = JOB-EXT-ITEM JOB-REF              *
      *                                                                *
      *  Aug 2008       - New file (STKALT) - Stock alternate index    *
      *                   included for lookups, using any word con-    *
      *                   tained in the Stock description.             *
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
       PROGRAM-ID.     STPENQ.
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

       COPY "CONTROL.SL".

       COPY "CREDIT.SL".

       COPY "DEPART.SL".

       COPY "PARAM.SL".

       COPY "SPARTS.SL".

       COPY "STKMEM.SL".

       COPY "STOCK.SL".

       COPY "WARHSE.SL".

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

       COPY "CONTROL.FDE".

       COPY "CREDIT.FDE".

       COPY "DEPART.FDE".

       COPY "PARAM.FDE".

       COPY "SPARTS.FDE".

       COPY "STKMEM.FDE".

       COPY "STOCK.FDE".

       COPY "WARHSE.FDE".

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
       77  WS-PARKEY               PIC  9(06)    COMP-5.
       77  WS-NETKEY               PIC  9(06)    COMP-5.
       77  WS-KEYSTR               PIC  9(04)    COMP-5.
       77  WS-GST-RATE             PIC S9(03)V99 COMP-3.
       77  WS-MONTHS               PIC S9(01)V99 COMP-3.
       77  WS-KEY1                 PIC  9(04)    COMP-5.
       77  WS-KSTORE               PIC  9(04)    COMP-5.
       77  WS-ETYPE                PIC  X(01).
       77  WS-VAT-GST              PIC  X(01).
       77  WS-OPTION               PIC  X(01).
       77  WS-SKIP                 PIC  X(01).
       77  WS-ADJ                  PIC  X(01).
       77  WS-ERROR                PIC  9(01).
       77  WS-AMEND                PIC  9(01)    VALUE 0.
       77  WS-CONREC               PIC  9(01)    VALUE ZERO.
       77  WS-COMP                 PIC  9(01).
       77  WS-WEEK                 PIC  9(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-USE-CASES            PIC  X(01).
       77  WS-USE-PACKS            PIC  X(01).
       77  WS-USE-ITM              PIC  X(01).
       77  WS-EXT-STK              PIC  X(01).
       77  WS-BATCH                PIC  9(05).
       77  WS-PASSWORD             PIC  X(08).
       77  WS-PRINT                PIC  9(01).
       77  WS-WH-QUANT             PIC S9(08)V999 COMP-3.
       77  WS-PASS                 PIC  X(08).
       77  WS-DESC-H1              PIC  X(14)    VALUE "Description".
       77  WS-DESC-H2              PIC  X(14)    VALUE "Description 2".
       77  WS-RT-HD                PIC  X(10)    VALUE "-Retail   ".
       77  WS-WS-HD                PIC  X(10)    VALUE "-Wholesale".
       77  WS-CS-HD                PIC  X(10)    VALUE "-Cash sale".
       77  WS-NODEP                PIC  X(30)    VALUE "NO SUCH   D E P A R T M E N T".
       77  PRG-NAME                PIC  X(12)    VALUE "STP\STPLOOK".
       77  PRG-NAME2               PIC  X(12)    VALUE "STP\STPMOVE".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.

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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STPENQ".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY -ENQUIRY".
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
           03  WS-MODULE           PIC  X(03) VALUE "STP".
           03  WS-PROG             PIC  X(03) VALUE "ENQ".

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

       01  W02-FILE-IDS.
           03  W02-PRINTER.
               05  W02-REPORT      PIC X(07) VALUE "STPENQ.".
               05  W02-USER        PIC X(05) VALUE SPACES.

       COPY "W05.VAT".

       COPY "W05.RND".

       01  W09-STOCK.
           03  W09-S-MARKUPS.
               05  W09-MARKUP      PIC S9(03)V9999 COMP-3.
               05  W09-CMARKUP     PIC S9(03)V9999 COMP-3.
               05  W09-WMARKUP     PIC S9(03)V9999 COMP-3.
               05  W09-MARKUP-2    PIC S9(03)V9999 COMP-3.
               05  W09-CMARKUP-2   PIC S9(03)V9999 COMP-3.
               05  W09-WMARKUP-2   PIC S9(03)V9999 COMP-3.
               05  W09-MARKUP-3    PIC S9(03)V9999 COMP-3.
               05  W09-CMARKUP-3   PIC S9(03)V9999 COMP-3.
               05  W09-WMARKUP-3   PIC S9(03)V9999 COMP-3.
               05  W09-MARKUP-4    PIC S9(03)V9999 COMP-3.
               05  W09-CMARKUP-4   PIC S9(03)V9999 COMP-3.
               05  W09-WMARKUP-4   PIC S9(03)V9999 COMP-3.
           03  W09-ST-MARKUPS                             REDEFINES W09-S-MARKUPS.
               05  W09-STK-MARK                           OCCURS 4.
                   07  W09-MKUP    PIC S9(03)V9999 COMP-3.
                   07  W09-CMKUP   PIC S9(03)V9999 COMP-3.
                   07  W09-WMKUP   PIC S9(03)V9999 COMP-3.

       COPY "W10.STK".

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

       COPY "W15.STK".

       01  W16-CALL.
           03  W16-EXT-ITEM.
               05  W16-ITEM        PIC  X(14).
               05  W16-ITM         PIC  X(03).
               05  FILLER          PIC  X(01).
           03  W16-CRED            PIC  X(06).
           03  W16-COST            PIC S9(07)V99 COMP-3.
           03  W16-CANCEL          PIC  X(01).

       COPY "W25.STK".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       COPY "W75.STK".

       01  W80-EDIT.
           03  W80-Q1.
               05  W80-S7V4        PIC  Z(06)9.9999-.

       01  W95-COMP                PIC  X(40).

       01  W100-EDIT.
           03  W100-V2.
               05  W100-S6V2       PIC  Z(05)9.99-.
           03  W100-V3                           REDEFINES W100-V2.
               05  W100-PRICE      PIC  X(09).
               05  FILLER          PIC  X(01).
           03  W100-V5                           REDEFINES W100-V2.
               05  W100-QNT        PIC  Z(05)9.99-.
           03  W100-V6.
               05  W100-S7V3       PIC  Z(06)9.999-.

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

       01  S04C.
          03  LINE  2 COLUMN 34 FOREGROUND-COLOR Grey HIGHLIGHT
                                 VALUE "STOCK ENQUIRY".

       COPY "STOCK.CRT".

       01  S15.
           03  LINE  2 COLUMN 33 FOREGROUND-COLOR Grey HIGHLIGHT
                                 VALUE "STOCK PRODUCTION".
           03  LINE  4 COLUMN  2 VALUE "Item Code/Description".
           03          COLUMN 36 VALUE "Quantity".
           03          COLUMN 47 VALUE "Unit cost".
           03          COLUMN 59 VALUE "Unit sell".
           03          COLUMN 70 VALUE "Total cost".

      *
      *    ****    PRODUCTION DETAILS DISPLAY SCREEN
      *
       01  S32.
           03  LINE 5 COLUMN 21 FOREGROUND-COLOR Grey HIGHLIGHT VALUE
                             "Details of Manufacture (Include Labour)".
           03  LINE 6 COLUMN  8 VALUE "Comments".
           03  LINE 7 COLUMN  8 VALUE "Item code".
           03         COLUMN 28 VALUE "Description".
           03         COLUMN 66 VALUE "Quantity".

       01  S38.
           03  FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC ZZZZ9.9999 USING W10-MQNT AUTO.

       01  S-PACKS.
           03  LINE  7 COLUMN 11 VALUE "здддддддддддддддддддддддддддддддддддддддддддддддддддддддддд"  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03          COLUMN 70 VALUE                                                            "©" BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey  HIGHLIGHT.
           03  LINE  8 COLUMN 11 VALUE "Ё"                                                            BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03          COLUMN 12 VALUE "          P A C K S :   S A L E S   D E T A I L S          "  BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey  HIGHLIGHT.
           03          COLUMN 70 VALUE                                                            "Ё" BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey  HIGHLIGHT.
           03  LINE  9 COLUMN 11 VALUE "ц"                                                            BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03          COLUMN 12 VALUE  "ддддддддддддддддддддд╢Pack 1цддддд╢Pack 2цддддд╢Pack 3цддд"  BACKGROUND-COLOR Grey FOREGROUND-COLOR Red.
           03          COLUMN 70 VALUE                                                            "╢" BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey  HIGHLIGHT.
           03  LINE 10 COLUMN 11 VALUE "Ё"                                                            BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03          COLUMN 12 VALUE  " Priced per        "                                         BACKGROUND-COLOR Grey FOREGROUND-COLOR Red   HIGHLIGHT.
           03          COLUMN 31 VALUE                     "                                       "  BACKGROUND-COLOR Grey FOREGROUND-COLOR Red.
           03          COLUMN 70 VALUE                                                            "Ё" BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey  HIGHLIGHT.
           03  LINE 11 COLUMN 11 VALUE "Ё"                                                            BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03          COLUMN 12 VALUE  " Units per pack    "                                         BACKGROUND-COLOR Grey FOREGROUND-COLOR Red   HIGHLIGHT.
           03          COLUMN 31 VALUE                     "                                       "  BACKGROUND-COLOR Grey FOREGROUND-COLOR Red.
           03          COLUMN 70 VALUE                                                            "Ё" BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey  HIGHLIGHT.
           03  LINE 12 COLUMN 11 VALUE "Ё"                                                            BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03          COLUMN 12 VALUE  " Markup "                                                    BACKGROUND-COLOR Grey FOREGROUND-COLOR Red   HIGHLIGHT.
           03          COLUMN 20 BACKGROUND-COLOR Grey FOREGROUND-COLOR Red HIGHLIGHT PIC  X(11) FROM WS-RT-HD.
           03          COLUMN 31 BACKGROUND-COLOR Grey FOREGROUND-COLOR Red VALUE "                                       ".
           03          COLUMN 70 BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT VALUE "Ё".
           03  LINE 13 COLUMN 11 BACKGROUND-COLOR Grey FOREGROUND-COLOR Black VALUE "Ё".
           03          COLUMN 12 BACKGROUND-COLOR Grey FOREGROUND-COLOR Red HIGHLIGHT VALUE "        ".
           03          COLUMN 20 BACKGROUND-COLOR Grey FOREGROUND-COLOR Red HIGHLIGHT PIC  X(11) FROM WS-CS-HD.
           03          COLUMN 31 BACKGROUND-COLOR Grey FOREGROUND-COLOR Red VALUE "                                       ".
           03          COLUMN 70 BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT VALUE "Ё".
           03  LINE 14 COLUMN 11 BACKGROUND-COLOR Grey FOREGROUND-COLOR Black VALUE "Ё".
           03          COLUMN 12 BACKGROUND-COLOR Grey FOREGROUND-COLOR Red HIGHLIGHT VALUE "        ".
           03          COLUMN 20 BACKGROUND-COLOR Grey FOREGROUND-COLOR Red HIGHLIGHT PIC  X(11) FROM WS-WS-HD.
           03          COLUMN 31 BACKGROUND-COLOR Grey FOREGROUND-COLOR Red VALUE "                                       ".
           03          COLUMN 70 BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT VALUE "Ё".
           03  LINE 15 COLUMN 11 BACKGROUND-COLOR Grey FOREGROUND-COLOR Black VALUE "Ё".
           03          COLUMN 12 BACKGROUND-COLOR Grey FOREGROUND-COLOR Red HIGHLIGHT VALUE " Price  ".
           03          COLUMN 20 BACKGROUND-COLOR Grey FOREGROUND-COLOR Red HIGHLIGHT PIC  X(11) FROM WS-RT-HD.
           03          COLUMN 31 BACKGROUND-COLOR Grey FOREGROUND-COLOR Red VALUE "                                       ".
           03          COLUMN 70 BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT Grey VALUE "Ё".
           03  LINE 16 COLUMN 11 BACKGROUND-COLOR Grey FOREGROUND-COLOR Black VALUE "Ё".
           03          COLUMN 12 BACKGROUND-COLOR Grey FOREGROUND-COLOR Red HIGHLIGHT VALUE "        ".
           03          COLUMN 20 BACKGROUND-COLOR Grey FOREGROUND-COLOR Red HIGHLIGHT PIC  X(11) FROM WS-CS-HD.
           03          COLUMN 31 BACKGROUND-COLOR Grey FOREGROUND-COLOR Red VALUE "                                       ".
           03          COLUMN 70 BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT VALUE "Ё".
           03  LINE 17 COLUMN 11 BACKGROUND-COLOR Grey FOREGROUND-COLOR Black VALUE "Ё".
           03          COLUMN 12 BACKGROUND-COLOR Grey FOREGROUND-COLOR Red HIGHLIGHT VALUE "        ".
           03          COLUMN 20 BACKGROUND-COLOR Grey FOREGROUND-COLOR Red HIGHLIGHT PIC  X(11) FROM WS-WS-HD.
           03          COLUMN 31 BACKGROUND-COLOR Grey FOREGROUND-COLOR Red VALUE "                                       ".
           03          COLUMN 70 BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT VALUE "Ё".
           03  LINE 18 COLUMN 11 BACKGROUND-COLOR Grey FOREGROUND-COLOR Black VALUE "ю".
           03          COLUMN 12 BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT VALUE "дддддддддддддддддддддддддддддддддддддддддддддддддддддддддды".

       01  SA-PACKS.
    02  SA-PACK-1.
    03  LINE 10 COLUMN 34 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC X(08) USING W10-PER-2 AUTO.
    03  LINE 11 COLUMN 34 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZ9.999 USING W10-UNIT-2 AUTO.
    03  LINE 12 COLUMN 36 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ9.99 USING W09-MARKUP-2 AUTO.
    03  LINE 13 COLUMN 36 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ9.99 USING W09-CMARKUP-2 AUTO.
    03  LINE 14 COLUMN 36 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ9.99 USING W09-WMARKUP-2 AUTO.
    03  LINE 15 COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZ9.99 USING W10-SELL-2 AUTO.
    03  LINE 16 COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZ9.99 USING W10-CASH-2 AUTO.
    03  LINE 17 COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZ9.99 USING W10-WSALE-2 AUTO.
    02  SA-PACK-2.
    03  LINE 10 COLUMN 47 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC X(08) USING W10-PER-3 AUTO.
    03  LINE 11 COLUMN 47 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZ9.999 USING W10-UNIT-3 AUTO.
    03  LINE 12 COLUMN 49 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ9.99 USING W09-MARKUP-3 AUTO.
    03  LINE 13 COLUMN 49 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ9.99 USING W09-CMARKUP-3 AUTO.
    03  LINE 14 COLUMN 49 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ9.99 USING W09-WMARKUP-3 AUTO.
    03  LINE 15 COLUMN 44 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZ9.99 USING W10-SELL-3 AUTO.
    03  LINE 16 COLUMN 44 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZ9.99 USING W10-CASH-3 AUTO.
    03  LINE 17 COLUMN 44 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZ9.99 USING W10-WSALE-3 AUTO.
    02  SA-PACK-3.
    03  LINE 10 COLUMN 60 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC X(08) USING W10-PER-4 AUTO.
    03  LINE 11 COLUMN 60 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZ9.999 USING W10-UNIT-4 AUTO.
    03  LINE 12 COLUMN 62 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ9.99 USING W09-MARKUP-4 AUTO.
    03  LINE 13 COLUMN 62 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ9.99 USING W09-CMARKUP-4 AUTO.
    03  LINE 14 COLUMN 62 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ9.99 USING W09-WMARKUP-4 AUTO.
    03  LINE 15 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZ9.99 USING W10-SELL-4 AUTO.
    03  LINE 16 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZ9.99 USING W10-CASH-4 AUTO.
    03  LINE 17 COLUMN 57 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZ9.99 USING W10-WSALE-4 AUTO.

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
       AA00.
             PERFORM ZA000-INIT.
      MOVE "N"   TO W16-CANCEL.
      PERFORM BD000.
    IF W16-CANCEL = "Y"
        CANCEL "STP\STPPOR".

       AA49.
      EXIT PROGRAM.

       COPY "AA50.LUP".

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
      *    Ё   здддддддддддддддддддддддддддддддддддддддддддддддд©      Ё
      *    Ё   Ё           Print transactions (Y/N) [ ]         Ё╟╟    Ё
      *    Ё   юдддддддддддддддддддддддддддддддддддддддддддддддды╟╟    Ё
      *    Ё     ╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟    Ё
      *    Ё                                                           Ё
      *    Ё The response is returned in WS-OPTION (in upper case).    Ё
      *    Ё                                                           Ё
      *    Ё To display the request message higher or lower (default   Ё
      *    Ё is line 20) move the line number to be used to            Ё
      *    Ё WS-MES-LINE.                                              Ё
      *    Ё                                                           Ё
      *    Ё If character 48 of the message contains a value of X"FA"  Ё
      *    Ё then do not display the message in a window, only get the Ё
      *    Ё User reponse at the position specified by WS-MES-LINE and Ё
      *    Ё WS-MES-COL.                                               Ё
      *    Ё                                                           Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "OPTION.CRT".

       COPY "LOCKED.RC2".

       COPY "DATE.CNV".

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
      *    Ё To clear lines 46 and 50:                                 Ё
      *    Ё    PERFORM CLEAR-L46-AND-50                               Ё
      *    Ё                                                           Ё
      *    Ё To clear line 50:                                         Ё
      *    Ё    PERFORM CLEAR-L50                                      Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "CLEAR.CRT".

       COPY "AA300.STK".

      *    *************************************************************
      *    ****    D I S P L A Y   D E P A R T M E N T  N A M E
      *    *************************************************************
       AA400              SECTION.
       AA401.
      MOVE W10-LEDG  TO DPT-CODE.
      PERFORM READ-DEPART THRU READ-DEPART-EXIT.
    IF (WS-F-ERROR = 7) OR
       (DPT-DESC = SPACES)
        MOVE WS-NODEP  TO W10-DEPT
    ELSE
        MOVE DPT-DESC  TO W10-DEPT.
      DISPLAY W10-DEPT AT 0623
       WITH FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Magenta.
       AA405.
             EXIT.

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000-READ        SECTION.

       COPY "CONTROL.RD".

       COPY "CREDIT.RD".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "STKMEM.RD".

       COPY "STOCK.RD".

       COPY "SPARTS.RD".

       COPY "WARHSE.RD".

       COPY "WSTOCK.RD".

      /    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Get the FIRST STOCK RECORD via the ITEM CODE INDEX     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "STOCK.1ST".

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  View Warehouse CODES and QUANTITIES        Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY WHQUANT.LUP.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё    View the APAC HELP details for this procedure       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "APAC.HLP".

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё     Retrieve a STOCK RECORD using the Item Code        Ё
      *    Ё     No record - Check           Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "CA000.STK".

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Retrieve a STOCK RECORD using the ALTERNATE CODE      Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "CA100.STK".

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Retrieve a STOCK RECORD using the X-REF/BAR CODE      Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "CA200.STK".

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё       Calculate the STOCK INCLUSIVE PRICES        Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "CA500.STK".

      /    *************************************************************
      *    * E N Q U I R Y   U S I N G  S T O C K   I T E M   C O D E *
      *    *************************************************************
       BD000         SECTION 5.
       BD000-ENQUIRY.
             PERFORM ERASE-SCREEN.
             DISPLAY S04C.
             DISPLAY S04.
    IF WS-USE-ITM = "Y"
        DISPLAY "/ITM" AT 0312
         "/" AT 0332.
    IF W10-ART11 = "Y"
        DISPLAY "VAT - Article 11/1g" AT 0849.
             MOVE SPACE              TO W10-EXT-ITEM.
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

       BD005.
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
      *    *************************************************************
      *    ** Check if the USER has pressed a function key and if so  **
      *    ** determine which key was selected.                       **
      *    *************************************************************
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  MOVE SPACES TO W10-EXT-ITEM
                               GO TO BD999
                 WHEN F1-KEY   PERFORM HELP-ROUTINE
                               GO TO BD005
                 WHEN F2-KEY   MOVE "I"  TO W10-ETYPE
                 WHEN F3-KEY   MOVE "A"  TO W10-ETYPE
                 WHEN F4-KEY   MOVE "D"  TO W10-ETYPE
                 WHEN F5-KEY   MOVE "2"  TO W10-ETYPE
                 WHEN F6-KEY   MOVE "X"  TO W10-ETYPE
                 WHEN F9-KEY   MOVE "W"  TO W10-ETYPE
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO BD005
               END-EVALUATE
               PERFORM AA50-LOOKUP
      *    *************************************************************
      *        *****       E X T E N D E D C O D E        *****
      *    *************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Is an 18 character stock/inventory item code being used?  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               IF WS-EXT-STK = "Y"
                   DISPLAY S05-EXT
               ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
               IF W10-STCK = "A"
                   IF WS-USE-ITM = "Y"
                       DISPLAY S05-ITM
                   ELSE
                       DISPLAY S05
                   END-IF
               END-IF
               IF W10-EXT-ITEM = SPACES
                   GO TO BD005.
             PERFORM CLEAR-L50.
           IF W10-EXT-ITEM = SPACE
                GO TO BD999.

       BD05.
             PERFORM CA000.
             DISPLAY S05.
           IF WS-F-ERROR = 22
               MOVE "Z"              TO W10-ETYPE
               PERFORM AA50-LOOKUP
               DISPLAY S05
               IF W10-EXT-ITEM = SPACES
                   GO TO BD005
               ELSE
                   GO TO BD05.
             MOVE ZERO               TO WS-WH-QUANT.
             MOVE STK-CODE           TO W10-EXT-ITEM W10-EXT-CODE1.
           IF LS0-WHS = 2
               PERFORM BD30 THRU BD40.
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
                   MOVE STK-AVGE     TO W10-AVRG.
                 MOVE STK-ACODE      TO W10-ACODE.
                 MOVE 1              TO WS-S1.

       BD05-MKUPS.
             MOVE STK-UNT(WS-S1)     TO W10-QNT.
           IF WS-S1 = 1
               MOVE W10-COST         TO W10-ICOST
           ELSE
               COMPUTE W10-ICOST ROUNDED = W10-COST * W10-QNT.
             COMPUTE W09-WMKUP(WS-S1) ROUNDED = ((STK-WSL(WS-S1) - W10-ICOST) * 100.000) / W10-ICOST.
             COMPUTE W09-MKUP(WS-S1)  ROUNDED = ((STK-SL(WS-S1)  - W10-ICOST) * 100.000) / W10-ICOST.
             COMPUTE W09-CMKUP(WS-S1) ROUNDED = ((STK-CSH(WS-S1) - W10-ICOST) * 100.000) / W10-ICOST.
           IF WS-S1 < 4
               ADD 1                 TO WS-S1
               GO TO BD05-MKUPS.
             DISPLAY S06C.
           IF STK-ACT = ZERO
               MOVE SPACE            TO W10-ACTIVE
           ELSE
               MOVE STK-ACT          TO W22-DTE2
               PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT
               MOVE W22-DTE3         TO W10-ADATE.
             MOVE STK-MTDC           TO W10-MTDC.
             MOVE STK-YTDC           TO W10-YTDC.
           IF LS-OK = "N"
               MOVE ZERO             TO W10-COST W10-MARKUP W10-AVRG W10-CMARKUP W10-MTDC W10-WMARKUP W10-YTDC
           ELSE
               MOVE W09-MARKUP       TO W10-MARKUP
               MOVE W09-CMARKUP      TO W10-CMARKUP
               MOVE W09-WMARKUP      TO W10-WMARKUP.
           IF LS0-STLEV < 1
               DISPLAY S06A
               DISPLAY S06C
           ELSE
               DISPLAY S06.
           IF W10-ART11 = "Y"
               DISPLAY W10-EXEMPT AT 0869 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             PERFORM AA400.
      *    *************************************************************
      *    ** As the F8 function has been introduced to view the last **
      *    ** Supplier and the Alternate Supplier, the following code **
      *    ** has been commented out. If there should again be a need **
      *    ** to view the last Supplier without using a function key, **
      *    ** then this code can be reintroduced.        **
      *    *************************************************************
      *    IF NOT (STK-SUPP = SPACES OR LOW-VALUES)
      *        MOVE STK-SUPP   TO P-NUMBER
      *        PERFORM READ-CREDIT THRU READ-CREDIT-EXIT
      *        IF WS-F-ERROR = ZERO
      *     DISPLAY "Supplier      " AT 2303
      *       P-NAME AT 2318
      *       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
      *     BACKGROUND-COLOR Magenta.
    IF NOT (WS-WH-QUANT = ZERO)
        ADD W10-QUANT WS-WH-QUANT
     GIVING W100-S7V3
        DISPLAY "зддддддддддддддддддддддд" AT 2054
   WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red
         "©" WITH FOREGROUND-COLOR Red HIGHLIGHT
    BACKGROUND-COLOR Red
        DISPLAY "Ё" AT 2154
   WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red
         "Including all warehouse"
   WITH FOREGROUND-COLOR Grey HIGHLIGHT
        BACKGROUND-COLOR Red
         "Ё" WITH FOREGROUND-COLOR Red HIGHLIGHT
    BACKGROUND-COLOR Red
        DISPLAY "Ё" AT 2254
   WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red
         "quantities "
   WITH FOREGROUND-COLOR Grey HIGHLIGHT
        BACKGROUND-COLOR Red
   W100-V6 WITH FOREGROUND-COLOR Grey HIGHLIGHT
         BACKGROUND-COLOR Red
         "Ё" WITH FOREGROUND-COLOR Red HIGHLIGHT
    BACKGROUND-COLOR Red
        DISPLAY "ю" AT 2354
   WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red
         "ддддддддддддддддддддддды"
   WITH FOREGROUND-COLOR Red HIGHLIGHT
        BACKGROUND-COLOR Red.
    IF STK-USE-CASES = "Y"
        MOVE STK-CASES  TO W10-CASES
        DISPLAY S06F.

       BD06.
      DISPLAY "" AT 5002
        WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Brown HIGHLIGHT
       "," WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "F5" WITH BACKGROUND-COLOR Cyan
          FOREGROUND-COLOR Brown HIGHLIGHT
       "=Job Det," WITH BACKGROUND-COLOR Cyan
          FOREGROUND-COLOR Blue
       "F6" WITH BACKGROUND-COLOR Cyan
          FOREGROUND-COLOR Brown HIGHLIGHT
       "=W/H Qnt," WITH BACKGROUND-COLOR Cyan
          FOREGROUND-COLOR Blue
       "F7" WITH BACKGROUND-COLOR Cyan
          FOREGROUND-COLOR Brown HIGHLIGHT
       "=Packs," WITH BACKGROUND-COLOR Cyan
         FOREGROUND-COLOR Blue
       "F8" WITH BACKGROUND-COLOR Cyan
          FOREGROUND-COLOR Brown HIGHLIGHT
       "=Suppl," WITH BACKGROUND-COLOR Cyan
       FOREGROUND-COLOR Blue
       "F9" WITH BACKGROUND-COLOR Cyan
          FOREGROUND-COLOR Brown HIGHLIGHT
       "=Memo," WITH BACKGROUND-COLOR Cyan
       FOREGROUND-COLOR Blue
       "F10" WITH BACKGROUND-COLOR Cyan
    FOREGROUND-COLOR Brown HIGHLIGHT
       "=P.Hist," WITH BACKGROUND-COLOR Cyan
           FOREGROUND-COLOR Blue
             "ANY" WITH BACKGROUND-COLOR Cyan
    FOREGROUND-COLOR Brown HIGHLIGHT
       "other key=cont"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
             GO TO BD45
   WHEN DOWN-KEY
             GO TO BD50
   WHEN OTHER GO TO BD08
        END-EVALUATE
    ELSE
    IF USER-FUNC
               EVALUATE KEY-CODE-1
   WHEN F5-KEY
              MOVE STK-CODE
     TO W16-EXT-ITEM
        PERFORM SAVE-SCREEN-3
        CALL "STP\STPJOB" USING
       LS-PARID LS-USER-ID LS0-PROGRAMS
       LS0-SECURITY W16-CALL
        PERFORM RESTORE-SCREEN-3
        GO TO BD06
   WHEN F6-KEY
                      PERFORM WH-VIEW-QUANT
                      GO TO BD06
   WHEN F7-KEY
        PERFORM DA000
        GO TO BD06
   WHEN F8-KEY
        PERFORM DB000
        GO TO BD06
   WHEN F9-KEY GO TO BD06A
   WHEN F10-KEY
              MOVE STK-CODE
     TO W16-EXT-ITEM
        PERFORM SAVE-SCREEN-3
        CALL "STP\STPPOR" USING
       LS-PARID LS-USER-ID LS0-PROGRAMS
       LS0-SECURITY W16-CALL
        PERFORM RESTORE-SCREEN-3
        GO TO BD06
   WHEN OTHER GO TO BD08
        END-EVALUATE
    ELSE
        GO TO BD08.

       BD06A.
      INITIALIZE SME-RECORD1.
      MOVE STK-CODE  TO SME-CODE.
             MOVE 1              TO SME-SEQ.
             PERFORM READ-STKMEM-LOCK THRU READ-STKMEM-EXIT.
           IF WS-F-ERROR NOT = ZERO
        MOVE "No additional specifications"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO BD06.
             INITIALIZE W200-MEM-FLDS.

       BD07.
             MOVE SME-SEQ        TO WS-S1.
      MOVE SME-DATA  TO W200-MEM(WS-S1).
             PERFORM READ-STKMEM-NEXT-LOCK THRU READ-STKMEM-EXIT.
           IF WS-F-ERROR = ZERO
               IF SME-CODE = STK-CODE
                   GO TO BD07.
      PERFORM SAVE-SCREEN.
             MOVE 7              TO SHADE-ROW.
      MOVE 19   TO SHADE-COL.
      MOVE 60   TO SHADE-WIDTH.
             MOVE 15             TO SHADE-LINES.
             DISPLAY S-MEM.
             PERFORM SCREEN-SHADOW.
      DISPLAY SA-MEM.
             DISPLAY "ENTER to continue" AT 2324
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT WS-OPTION AT 2342
      WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.
      PERFORM RESTORE-SCREEN.
      GO TO BD06.

       BD08.
      MOVE STK-CODE  TO PRT-EXT-ITEM.
             PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
           IF WS-F-ERROR = 21
               GO TO BD000-ENQUIRY.
      *
      *    ****  P R O D U C T I O N   D E T A I L S
      *
             MOVE 0501           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
             DISPLAY S32.
            DISPLAY PRT-COMMENT AT 0618
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             MOVE 1              TO WS-S6.

       BD10.
             MOVE 8              TO LIN.
             MOVE 64             TO COL.

       BD15.
          IF PRT-EXT-CODE(WS-S6) = SPACES
               ADD 1             TO WS-S6
               GO TO BD20.
            MOVE PRT-EXT-CODE(WS-S6)
     TO W10-EXT-CODE2 W10-EXT-ITEM.
            MOVE PRT-QUANT(WS-S6)
                                 TO W10-MQNT.
             PERFORM CA000.
           IF WS-F-ERROR = 22
               ADD 1             TO WS-S6
               GO TO BD20.
            DISPLAY (LIN, 8) W10-EXT-CODE2
        WITH FOREGROUND-COLOR Cyan HIGHLIGHT
      BACKGROUND-COLOR Magenta.
            DISPLAY (LIN, 28) STK-DESC
        WITH FOREGROUND-COLOR Cyan HIGHLIGHT
      BACKGROUND-COLOR Magenta.
             DISPLAY S38 AT LINE LIN COLUMN COL.
             ADD 1 TO WS-S6.
           IF LIN < 22
               ADD 1 TO LIN
           ELSE
               GO TO BD25.
      *
      *    ****   C H E C K   F O R   E N D   O F   P R O D U C T I O N
      *    D E T A I L S
      *
       BD20.
           IF WS-S6 < 31
               GO TO BD15.
            MOVE "ENTER to continue"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
            GO TO BD000-ENQUIRY.

       BD25.
            MOVE "ENTER to continue"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
    IF PRT-EXT-CODE(WS-S6) = SPACES
               GO TO BD000-ENQUIRY.
             MOVE 0801           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
             GO TO BD10.

       BD30.
      MOVE W10-EXT-ITEM  TO WST-EXT-CODE.
      MOVE SPACES  TO WST-WAR.
      PERFORM START-AT-WH-ITEM THRU READ-WSTOCK-EXIT.
    IF WS-F-ERROR = 52
        GO TO BD40.

       BD35.
      PERFORM READ-WSTOCK-NEXT THRU READ-WSTOCK-EXIT.
    IF WS-F-ERROR = 52
        GO TO BD40.
    IF NOT (W10-EXT-ITEM = WST-EXT-CODE)
        GO TO BD40.
      ADD WST-QUANT  TO WS-WH-QUANT.
      GO TO BD35.

       BD40.
      EXIT.
      *
      *    ****   P R E V I O U S   I T E M
      *
       BD45.
      MOVE W10-EXT-CODE1  TO STK-CODE.
      PERFORM READ-STOCK THRU READ-STOCK-EXIT.
            PERFORM ERASE-SCREEN.
             DISPLAY S04C.
             DISPLAY S04.
    IF WS-USE-ITM = "Y"
        DISPLAY "/ITM" AT 0312
         "/" AT 0332.
      PERFORM READ-STOCK-PREV THRU READ-STOCK-EXIT.
    IF WS-F-ERROR = 22
        GO TO BD55.
      GO TO BD60.
      *
      *    ****    N E X T   I T E M
      *
       BD50.
      MOVE W10-EXT-CODE1  TO STK-CODE.
      PERFORM READ-STOCK THRU READ-STOCK-EXIT.
            PERFORM ERASE-SCREEN.
             DISPLAY S04C.
             DISPLAY S04.
    IF WS-USE-ITM = "Y"
        DISPLAY "/ITM" AT 0312
         "/" AT 0332.
      PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
    IF WS-F-ERROR = 22
        GO TO BD55.
      GO TO BD60.

       BD55.
      MOVE W10-EXT-CODE1  TO STK-CODE.
      PERFORM READ-STOCK THRU READ-STOCK-EXIT.

       BD60.
      MOVE STK-CODE  TO W10-EXT-ITEM.
    IF WS-EXT-STK = "Y"
        DISPLAY S05-EXT
    ELSE
    IF WS-USE-ITM = "Y"
        DISPLAY S05-ITM
    ELSE
              DISPLAY S05.
      GO TO BD05.

       BD999.
             EXIT.
      /
      *    ****   D I S P L A Y   P A C K S
      *
       DA000        SECTION 5.
       DA00.
      PERFORM SAVE-SCREEN-3.
            PERFORM CLEAR-L50.
      MOVE 8   TO SHADE-ROW.
      MOVE 13   TO SHADE-COL.
      MOVE 58   TO SHADE-WIDTH.
      MOVE 11   TO SHADE-LINES.
      DISPLAY S-PACKS.
      PERFORM SCREEN-SHADOW.
      MOVE 1   TO WS-S1.

       DA05.
      MOVE STK-UNT(WS-S1) TO W10-UNT(WS-S1).
      MOVE STK-PR(WS-S1)  TO W10-PR(WS-S1).
      MOVE STK-MKUP(WS-S1)
     TO W10-MKUP(WS-S1).
      MOVE STK-CMKUP(WS-S1)
     TO W10-CMKUP(WS-S1).
      MOVE STK-WMKUP(WS-S1)
     TO W10-WMKUP(WS-S1).
      MOVE STK-SL(WS-S1)  TO W10-SL(WS-S1).
            MOVE STK-CSH(WS-S1) TO W10-CSH(WS-S1).
            MOVE STK-WSL(WS-S1) TO W10-WSL(WS-S1).
    IF WS-S1 < 4
        ADD 1   TO WS-S1
        GO TO DA05.
      DISPLAY SA-PACKS.
      DISPLAY "Press any key to continue" AT 5012
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Cyan.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM RESTORE-SCREEN-3.

       DA999.
      EXIT.
      /
      *    ****   D I S P L A Y   S U P P L I E R S
      *
      * здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      * Ё Supplier     XXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX Ё
      * Ё Alt Supplier XXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX Ё
      * юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       DB000        SECTION 5.
       DB00.
      PERFORM SAVE-SCREEN-3.
            PERFORM CLEAR-L50.
      MOVE 13   TO SHADE-ROW.
      MOVE 11   TO SHADE-COL.
      MOVE 62   TO SHADE-WIDTH.
      MOVE 3   TO SHADE-LINES.
      DISPLAY "зддддддддддддддддддддддддддддддддддддддддддддддддд
      -       "ддддддддддддд" AT 1209
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
       "©" WITH FOREGROUND-COLOR Grey HIGHLIGHT
         BACKGROUND-COLOR Grey.
      DISPLAY "Ё Supplier
      -       "             " AT 1309
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
       "Ё" WITH FOREGROUND-COLOR Grey HIGHLIGHT
         BACKGROUND-COLOR Grey.
      DISPLAY "Ё Alt Supplier
      -       "             " AT 1409
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
       "Ё" WITH FOREGROUND-COLOR Grey HIGHLIGHT
         BACKGROUND-COLOR Grey.
      DISPLAY "ю" AT 1509
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
       "дддддддддддддддддддддддддддддддддддддддддддддддддд
      -       "дддддддддддды"
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Grey.
      PERFORM SCREEN-SHADOW.

       DB05.
    IF NOT (STK-SUPP = SPACES OR LOW-VALUES)
               MOVE STK-SUPP     TO P-NUMBER
               PERFORM READ-CREDIT THRU READ-CREDIT-EXIT
               IF WS-F-ERROR = ZERO
     DISPLAY P-NUMBER AT 1324
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Black
     DISPLAY P-NAME AT 1331
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Black
        END-IF
    ELSE
        DISPLAY "No Supplier recorded for this Item" AT 1331
   WITH FOREGROUND-COLOR Brown HIGHLIGHT
        BACKGROUND-COLOR Black.
    IF NOT (STK-PSUPP = SPACES OR LOW-VALUES)
        MOVE STK-PSUPP  TO P-NUMBER
               PERFORM READ-CREDIT THRU READ-CREDIT-EXIT
        IF WS-F-ERROR = ZERO
     DISPLAY P-NUMBER AT 1424
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Black
     DISPLAY P-NAME AT 1431
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Black
        END-IF
    ELSE
        DISPLAY "No Alt Supplier recorded for this Item" AT 1331
   WITH FOREGROUND-COLOR Brown HIGHLIGHT
        BACKGROUND-COLOR Black.
      DISPLAY "Press any key to continue" AT 5012
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Cyan.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM RESTORE-SCREEN-3.

       DB999.
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
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Setup various keys and the mouse for use with menus and  Ё
      *    Ё  other required user interface functions and procedures.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

             COPY "FUNCTION.PRO".

       ZA00A-CONTINUE.
            MOVE 3   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
    IF NOT (PAR-STOCK = "A" OR "N")
        MOVE "A"   TO PAR-STOCK.
      MOVE PAR-STOCK  TO W10-STCK.
    IF PAR-STOCK = "N"
        MOVE PAR-SLNGTH  TO W10-SLNGTH
    ELSE
        MOVE 14   TO W10-SLNGTH.
    IF W10-SLNGTH < 3
        MOVE 3   TO W10-SLNGTH
    ELSE
    IF W10-SLNGTH > 14
        MOVE 14   TO W10-SLNGTH.
      MOVE PAR-CASES  TO WS-USE-CASES.
      MOVE PAR-PACKS  TO WS-USE-PACKS.
      MOVE PAR-USE-ITM  TO WS-USE-ITM.
      MOVE PAR-EXT-STK  TO WS-EXT-STK.
    IF NOT (PAR-ART11 = "Y" OR "N")
        MOVE "N"   TO PAR-ART11.
      MOVE PAR-ART11  TO W10-ART11.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
             MOVE PAR-COMPANY    TO W95-COMP.
             MOVE 3              TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE STK-WEEK       TO WS-WEEK.
             MOVE STK-PASSWORD   TO WS-PASSWORD.
      MOVE 1   TO W05-V-RATE.

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
             GO TO ZA999-EXIT.

      *COPY "ZA49.PRO".
      
       ZA999-EXIT.
             EXIT.
      *
      *    ****    I / O   E R R O R   M E S S A G E S
      *
       ZB000-ERROR        SECTION 9.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 2     MOVE W02-NETWORK  TO WS-FILE
                          MOVE WS-NETKEY    TO WS-KEY
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
          ELSE
           IF WS-F-ERROR = 51
              MOVE W02-WARHSE  TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE WAR-CODE     TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 52
              MOVE W02-WSTOCK  TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE WST-KEY  TO WS-KEYX
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STPENQ"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
