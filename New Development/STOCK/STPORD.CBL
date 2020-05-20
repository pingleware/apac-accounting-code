      $SET LINKCOUNT"576" GNT"STPORD.GNT"
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
      *    ******   ********  *******    ******   *******   *******    *
      *   **    **     **     **    **  **    **  **    **  **    **   *
      *   **           **     **    **  **    **  **    **  **    **   *
      *    ******      **     *******   **    **  *******   **    **   *
      *         **     **     **        **    **  **   **   **    **   *
      *   **    **     **     **        **    **  **    **  **    **   *
      *    ******      **     **         ******   **    **  *******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S T O C K   E N Q U I R Y   P R O G R A M   (ORDERS)       *
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
       PROGRAM-ID.     STPORD.
       AUTHOR.         J.W.LEMMON.
       DATE-WRITTEN.   MARCH 1995.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1995 - 2018
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

       COPY "CORDER.SL".

       COPY "CREDIT.SL".

       COPY "DEPART.SL".

       COPY "PARAM.SL".

       COPY "PORDER.SL".

       COPY "STKMEM.SL".

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

       COPY "CORDER.FDE".

       COPY "CREDIT.FDE".

       COPY "DEPART.FDE".

       COPY "PARAM.FDE".

       COPY "PORDER.FDE".

       COPY "STKMEM.FDE".

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
       77  WS-DISC    PIC 999V99  COMP-3.
       77  WS-SUB    PIC 9(04)  COMP-5.
       77  WS-S1    PIC 9(04)  COMP-5.
       77  WS-S2    PIC 9(04)  COMP-5.
       77  WS-S3    PIC 9(04)  COMP-5.
       77  WS-S4    PIC 9(04)  COMP-5.
       77  WS-S5    PIC 9(04)  COMP-5.
       77  WS-S6    PIC 9(04)  COMP-5.
       77  WS-S7    PIC 9(04)  COMP-5.
       77  WS-S8    PIC 9(04)  COMP-5.
       77  WS-IXS    PIC 9(04)  COMP-5.
       77  WS-IXS1    PIC 9(04)  COMP-5.
       77  WS-IXS2    PIC 9(04)  COMP-5.
       77  WS-IXS3    PIC 9(04)  COMP-5.
       77  WS-IXS4    PIC 9(04)  COMP-5.
       77  WS-STKKEY    PIC 9(06)  COMP-5.
       77  WS-SPAKEY    PIC 9(06)  COMP-5.
       77  WS-PARKEY    PIC 9(06)  COMP-5.
       77  WS-NETKEY    PIC 9(06)  COMP-5.
       77  WS-SINKEY    PIC 9(06)  COMP-5.
       77  WS-RECKEY    PIC 9(06)  COMP-5.
       77  WS-RECOVER    PIC 9(06)  COMP-5.
       77  WS-TRANS    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-KEYSTR    PIC 9(04)  COMP-5.
       77  WS-MONTHS    PIC S9(01)V99 COMP-3.
       77  WS-ITM    PIC X(18).
       77  WS-ETYPE    PIC X(01).
       77  WS-PAGE    PIC 9(04)  COMP-5.
       77  WS-OPTION    PIC X(01).
       77  WS-SKIP    PIC X(01).
       77  WS-ADJ    PIC X(01).
       77  WS-ERROR    PIC 9(01).
       77  WS-IND    PIC 9(01) VALUE 0.
       77  WS-USE-ITM    PIC X(01).
       77  WS-EXT-STK    PIC X(01).
       77  WS-AMEND    PIC 9(01) VALUE 0.
       77  WS-CONREC    PIC 9(01) VALUE ZERO.
       77  WS-LEDG    PIC X(04).
       77  WS-LEDG1    PIC X(04).
       77  WS-COMP    PIC 9(01).
       77  WS-WEEK    PIC 9(01).
       77  WS-TYPE    PIC X(01).
       77  WS-PASSWORD    PIC X(08).
       77  WS-PRINT    PIC 9(01).
       77  WS-PASS    PIC X(08).
       77  WS-DESC-H1    PIC X(14) VALUE "Description".
       77  WS-DESC-H2    PIC X(14) VALUE "Description 2".
       77  WS-RT-HD    PIC X(10) VALUE "-Retail   ".
       77  WS-WS-HD    PIC X(10) VALUE "-Wholesale".
       77  WS-CS-HD    PIC X(10) VALUE "-Cash sale".
       77  WS-NODEP    PIC X(30) VALUE
      "NO  SUCH   D E P A R T M E N T".
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.

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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STPORD".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY -ORDER ENQUIRY".
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
    03  WS-PROG        PIC  X(03) VALUE "ORD".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-FILE-IDS.
           03  W02-PRINTER.
        05  W02-REPORT PIC X(07) VALUE "STPORD.".
               05  W02-USER   PIC X(05) VALUE SPACES.

       COPY "W05.VAT".

       COPY "W05.RND".

       01  W09-STOCK.
           03  W09-S-MARKUPS.
               05  W09-MARKUP      PIC S9(03)V9999  COMP-3.
               05  W09-CMARKUP     PIC S9(03)V9999  COMP-3.
               05  W09-WMARKUP     PIC S9(03)V9999  COMP-3.
               05  W09-MARKUP-2    PIC S9(03)V9999  COMP-3.
               05  W09-CMARKUP-2   PIC S9(03)V9999  COMP-3.
               05  W09-WMARKUP-2   PIC S9(03)V9999  COMP-3.
               05  W09-MARKUP-3    PIC S9(03)V9999  COMP-3.
               05  W09-CMARKUP-3   PIC S9(03)V9999  COMP-3.
               05  W09-WMARKUP-3   PIC S9(03)V9999  COMP-3.
               05  W09-MARKUP-4    PIC S9(03)V9999  COMP-3.
               05  W09-CMARKUP-4   PIC S9(03)V9999  COMP-3.
               05  W09-WMARKUP-4   PIC S9(03)V9999  COMP-3.
           03  W09-ST-MARKUPS                       REDEFINES W09-S-MARKUPS.
               05  W09-STK-MARK                     OCCURS 4.
                   07  W09-MKUP    PIC S9(03)V9999  COMP-3.
                   07  W09-CMKUP   PIC S9(03)V9999  COMP-3.
                   07  W09-WMKUP   PIC S9(03)V9999  COMP-3.

       COPY "W10.STK".

           03  W10-CONTACT         PIC  X(30).
           03  W10-SUPP            PIC  X(06).
           03  W10-SNAME           PIC  X(40).
           03  W10-STERMS.
               05  W10-30          PIC  9(01).
               05  W10-60          PIC  9(01).
               05  W10-90          PIC  9(01).
               05  W10-120         PIC  9(01).
               05  W10-150         PIC  9(01).
               05  W10-180         PIC  9(01).
           03  W10-TERMS           PIC  9(06) REDEFINES W10-STERMS.
           03  W10-TEL             PIC  X(14).

       COPY "W11.STK".

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

       01  W15-INPUT-AREA.
           03  W15-NAME.
               05  W15-NCHAR PIC  X(01) OCCURS 40.
           03  W15-N         REDEFINES W15-NAME.
               05  W15-DESC.
                   07  W15-ENTRY
                             PIC  X(08).
                   07  FILLER
                             PIC  X(16).
               05  FILLER    PIC  X(16).
    03  W15-LDG.
              05  W15-LEDGN   PIC  9(04).
    03  W15-LG REDEFINES W15-LDG.
        05  FILLER      PIC  X(01).
        05  W15-LEDG    PIC  9(03).

       01  W16-CALL.
    03  W16-ITEM      PIC  X(18).
    03  W16-CRED      PIC  X(06).
    03  W16-COST      PIC S9(07)V99 COMP-3.
    03  W16-CANCEL    PIC  X(01).

       COPY "W25.STK".

       COPY "W40.WS".

       01  W42-ATTRIB5.
           03  FILLER          PIC X(2000).

       01  W42-ATTRIB6.
           03  FILLER          PIC X(2000).

       01  W43-SCREEN5.
           03  FILLER          PIC X(2000).

       01  W43-SCREEN6.
           03  FILLER          PIC X(2000).

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       COPY "W75.STK".

       01  W75-CKEYS.
    03  W75-CS      PIC  9(02) COMP-5.
    03  W75-CS1      PIC  9(02) COMP-5.
    03  W75-CNO      PIC  X(06) OCCURS 18.

       01  W95-COMP            PIC X(40).

       01  W100-EDIT.
           03  W100-V2.
               05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V5 REDEFINES W100-V2.
        05  W100-QNT    PIC  Z(03)9-.
        05  W100-NO     PIC  Z(03)9.
        05  FILLER      PIC  X(01).

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
          03  LINE  2 COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
           VALUE "STOCK ORDER ENQUIRY".

       COPY "STOCK.CRT".

       01  S07.
    02  S07A.
    03  LINE  7 COLUMN 18 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
    "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
    03  LINE  8 COLUMN 18 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
    "³      A D D   I T E M   T O   O R D E R   F I L E      ³".
    03  LINE  9 COLUMN 18 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
    "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
    03  LINE 10 COLUMN 18 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
    "³Stock code/Ext ".
    03        COLUMN 34 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(14) FROM W10-ITEM.
    03        COLUMN 48 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE "/".
    03        COLUMN 49 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(03) FROM W10-ITM.
    03        COLUMN 52 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
     "   ³".
    03  LINE 11 COLUMN 18 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
    "³Description    ".
    03        COLUMN 34 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(30) FROM W10-DESC.
    03        COLUMN 64 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
     "     ³".
    02  S07B.
    03  LINE 12 COLUMN 18 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
    "³Supplier     ".
    03        COLUMN 34 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(06) USING W10-SUPP AUTO.
    03        COLUMN 40 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
     "               ³".
    03  LINE 13 COLUMN 18 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
    "³      ".
    03        COLUMN 34 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(40) FROM W10-SNAME.
    03        COLUMN 74 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
     "³".
    02  S07C.
    03  LINE 14 COLUMN 18 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
    "³Contact     ".
    03        COLUMN 34 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(30) USING W10-CONTACT AUTO.
    03        COLUMN 64 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
     "     ³".
    02  S07D.
    03  LINE 15 COLUMN 18 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
    "³Telephone      ".
    03        COLUMN 34 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(14) USING W10-TEL AUTO.
    03        COLUMN 48 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
     "                          ³".
    02  S07E.
    03  LINE 16 COLUMN 18 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
    "³Quantity     ".
    03        COLUMN 34 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  Z(06)9.99 USING W10-ORDER AUTO.
    03        COLUMN 44 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
     "                         ³".
    02  S07F.
    03  LINE 17 COLUMN 18 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
    "³Unit cost     ".
    03        COLUMN 34 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  Z(05)9.999 USING W10-COST AUTO.
    03        COLUMN 44 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
     "               ³".
    02  S07G.
    03  LINE 18 COLUMN 18 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
    "³Terms  30 ".
    03        COLUMN 34 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  9 USING W10-30 AUTO.
    03        COLUMN 35 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE "  60 ".
    03        COLUMN 40 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  9 USING W10-60 AUTO.
    03        COLUMN 41 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE "  90 ".
    03        COLUMN 46 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  9 USING W10-90 AUTO.
    03        COLUMN 47 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE " 120 ".
    03        COLUMN 52 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  9 USING W10-120 AUTO.
    03        COLUMN 53 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE " 150 ".
    03        COLUMN 58 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  9 USING W10-150 AUTO.
    03        COLUMN 59 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE " 180 ".
    03        COLUMN 64 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  9 USING W10-180 AUTO.
    03        COLUMN 65 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE "     ³".
    02  S07H.
    03  LINE 19 COLUMN 18 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
    "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01 S07-EXT.
    03  LINE 10 COLUMN 18 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
    "³Stock code     ".
    03        COLUMN 34 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(18) FROM W10-EXT-ITEM.
    03        COLUMN 52 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
     "   ³".

       01  S-STK.
           03  LINE  2 COLUMN 35 FOREGROUND-COLOR Grey HIGHLIGHT
           VALUE "STOCK ORDERS".
          03  LINE  3 COLUMN  2 "Start Code/Desc :".
    03        COLUMN 34 "/".
           03  LINE  4 COLUMN  2 REVERSE-VIDEO VALUE
              " Item/Bar Code     Description                    Qty  M
      -        "TD M/AV  Ord    Cost  ".
          03  LINE  5 COLUMN  1 BACKGROUND-COLOR Black VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÑÍÍÍÍÑÍÍÍÍÑÍÍÍÍÑÍÍÍÍÍÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR Black VALUE "µ".
          03  LINE 22 COLUMN  1 BACKGROUND-COLOR Black VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÏÍÍÍÍÏÍÍÍÍÏÍÍÍÍÏÍÍÍÍÍÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR Black VALUE "µ".
           03  LINE 23 COLUMN  2 FOREGROUND-COLOR Grey VALUE
             "Key in at least the first character of the code or press".
           03          COLUMN 59 FOREGROUND-COLOR Brown HIGHLIGHT VALUE "Esc".
           03          COLUMN 62 FOREGROUND-COLOR Grey VALUE " to exit".
          03  LINE 25 COLUMN  2 BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Blue VALUE "Pressing '".
          03        COLUMN 12 BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Brown HIGHLIGHT VALUE "ENTER".
          03        COLUMN 17 BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Blue VALUE
                                 "' key only, will start from the beginning of the stock file".

       01  S-STKS.
          03  LINE  4 COLUMN 74 REVERSE-VIDEO VALUE "Sell".

       01  S-STKI.
          03  LINE  3 COLUMN  2 "Start Item Code :".
       01  S-STKC.
           03  LINE  3 COLUMN  2 "Start XRef Code :".
       01  S-STKD.
           03  LINE  3 COLUMN  2 "Start Description :".
           03  LINE 23 COLUMN  2 FOREGROUND-COLOR Grey VALUE
             "Key in at least the first character of the description or press".
           03          COLUMN 66 FOREGROUND-COLOR Brown HIGHLIGHT VALUE "Esc".
           03          COLUMN 69 FOREGROUND-COLOR Grey VALUE " to exit".

       01  SA-STK-CODE.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               05  SA-STK.
                   07  LINE  3 COLUMN 20  PIC  X(14) USING W10-ITEM AUTO.

               05  SA-STK-ITM.
                   07  LINE  3 COLUMN 20 PIC X(14) USING W10-ITEM AUTO.
                   07          COLUMN 35 PIC X(03) USING W10-ITM AUTO.
               
               05  SA-STK-EXT.
                   07  LINE  3 COLUMN 20 PIC X(18) USING W10-EXT-ITEM AUTO.
               
               05  SA-STK-3.
                   07  LINE  3 COLUMN 20 PIC Z(03) USING W10-I3 AUTO.
	           
               05  SA-STK-4.
                   07  LINE  3 COLUMN 20 PIC Z(04) USING W10-I4 AUTO.
	           
               05  SA-STK-5.
                   07  LINE  3 COLUMN 20 PIC Z(05) USING W10-I5 AUTO.
	           
               05  SA-STK-6.
                   07  LINE  3 COLUMN 20 PIC Z(06) USING W10-I6 AUTO.
	           
               05  SA-STK-7.
                   07  LINE  3 COLUMN 20 PIC Z(07) USING W10-I7 AUTO.
	           
               05  SA-STK-8.
                   07  LINE  3 COLUMN 20 PIC Z(08) USING W10-I8 AUTO.
	           
               05  SA-STK-9.
                   07  LINE  3 COLUMN 20 PIC Z(09) USING W10-I9 AUTO.
	           
               05  SA-STK-10.
                   07  LINE  3 COLUMN 20 PIC Z(10) USING W10-I10 AUTO.
	           
               05  SA-STK-11.
                   07  LINE  3 COLUMN 20 PIC Z(11) USING W10-I11 AUTO.
	           
               05  SA-STK-12.
                   07  LINE  3 COLUMN 20 PIC Z(12) USING W10-I12 AUTO.
	           
               05  SA-STK-13.
                   07  LINE  3 COLUMN 20 PIC Z(13) USING W10-I13 AUTO.
	           
               05  SA-STK-14.
                   07  LINE  3 COLUMN 20 PIC Z(14) USING W10-I14 AUTO.

       01  SA-STKA.
          03  LINE  3 COLUMN 25 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
                                 PIC  X(10) USING W10-ACODE AUTO.

       01  SA-STKC.
          03  LINE  3 COLUMN 20 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC  X(18) USING W10-EXT-CODE2 AUTO.

       01  SA-STKD.
           03  LINE  3 COLUMN 35 PIC  X(30) USING W10-DESC FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.

       01  S-CRD.
           03  LINE  2 COLUMN 33 VALUE                                 "CREDITOR LOOKUP"                                  FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE  3 COLUMN  2 VALUE  "Starting Name >".
           03          COLUMN 57 VALUE                                                         "<".
           03  LINE  4 COLUMN  2 VALUE  "Ac/No               Supplier name                  Telephone        Balance   "  REVERSE-VIDEO.
           03  LINE  5 COLUMN  1 VALUE "Æ"                                                                                BACKGROUND-COLOR Black.
           03          COLUMN  2 VALUE  "ÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           03          COLUMN 80 VALUE                                                                                "µ" BACKGROUND-COLOR Black.
           03  LINE 22 COLUMN  1 VALUE "Æ"                                                                                BACKGROUND-COLOR Black.
           03          COLUMN  2 VALUE  "ÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           03          COLUMN 80 VALUE                                                                                "µ" BACKGROUND-COLOR Black.
           03  LINE 23 COLUMN  2 VALUE  "Key in at least the first character of the name or press "                       FOREGROUND-COLOR Grey.
           03          COLUMN 59 VALUE                                                           "Esc"                    FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 62 VALUE                                                              " to exit          "  FOREGROUND-COLOR Grey.
           03  LINE 25 COLUMN  2 VALUE  "Pressing '"                                                                      BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
           03          COLUMN 12 VALUE            "ENTER"                                                                 BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 17 VALUE                 "' key only, will start from the beginning of the creditor file"   BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.

       01  SA-CRD.
           03  LINE  3 COLUMN 17 FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta PIC  X(40) USING W15-NAME AUTO.

       01  SD-CRD.
           03  COLUMN  2 FOREGROUND-COLOR Cyan HIGHLIGHT PIC X(06) USING P-NUMBER.
           03  COLUMN  9 FOREGROUND-COLOR Cyan HIGHLIGHT PIC X(40) USING P-NAME.
           03  COLUMN 51 FOREGROUND-COLOR Cyan HIGHLIGHT PIC X(14) USING P-TEL.
           03  COLUMN 68 FOREGROUND-COLOR Cyan HIGHLIGHT PIC Z(07)9.99- USING P-BAL.

       COPY "DEPART.CRT".

       01  S-LEDG.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
               05  LINE 50 COLUMN 02 VALUE "F1"                             FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 04 VALUE   "=Help,".
               05          COLUMN 10 VALUE         "F6"                     FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 12 VALUE           "=Sales Ledger Lookup".
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
       AA00.
      *
      *   **** Check the User's security level for Stock/Inventory
      *
      *    IF LS0-STLEV < 3
      *        MOVE "Not Authorised"
      *     TO WS-ERR-MES
      *        PERFORM ERROR-MESSAGE
      *        GO TO AA49.
            PERFORM ZA000-INIT.
      MOVE "N"   TO W16-CANCEL.
      PERFORM BA000.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If STPPOR was called, then cancel the program to free RAM ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
    IF W16-CANCEL = "Y"
        CANCEL "STP\STPPOR".
      CLOSE CORDER.

       AA49.
      EXIT PROGRAM.

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

       SAVE-SCREEN-5.
      CALL "CBL_READ_SCR_CHATTRS" USING SAVE-POS
            W43-SCREEN5
            W42-ATTRIB5
            W41-LENGTH
      RETURNING WS-STATUS.

       RESTORE-SCREEN-5.
      MOVE 2000   TO W41-LENGTH.
      CALL "CBL_WRITE_SCR_CHATTRS" USING SAVE-POS
      W43-SCREEN5
      W42-ATTRIB5
      W41-LENGTH
       RETURNING WS-STATUS.

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

       COPY "DATE.CNV".

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

      *    *************************************************************
      *    ****    D I S P L A Y   D E P A R T M E N T  N A M E    ****
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

       COPY "CORDER.RD".

       COPY "CREDIT.RD".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "PORDER.RD".

       COPY "STKMEM.RD".

       COPY "STOCK.RD".

       COPY "DEPART.LUP".

       COPY "STOCK.1ST".

      *
      *   R E W R I T E   &    W R I T E   F I L E S   R O U T I N E S
      *
       AD000   SECTION.

       COPY "CORDER.WR".

       COPY "APAC.HLP".

      /
      *    ****    S T O C K   L O O K U P   R O U T I N E
      *
      *     O R D E R S
      *
       STOCK-LOOKUP                   SECTION.

       STOCK-LUP-SCREEN.
             PERFORM ERASE-SCREEN.
            DISPLAY S-STK.
          IF LS0-STLEV < 3
        DISPLAY S-STKS.
             MOVE SPACE          TO WS-OPTION.

       STOCK-LUP-CLR-SCREEN.
             MOVE 0601               TO CPOS.
             PERFORM STOCK-CRT-EDGE UNTIL CLIN = 22.
             MOVE SPACES             TO W10-ACODE W10-DESC W10-EXT-CODE2 W10-EXT-CODE1 W10-EXT-ITEM.
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
             MOVE "I"                TO WS-ETYPE.

       STOCK-LUP-DATA.
      *    *************************************************************
      *                     G E T   S T O C K   C O D E
      *    *************************************************************
      *    
      *      Is an 18 character stock/inventory item code being used?  
      *    
           IF WS-EXT-STK = "Y"
               ACCEPT SA-STK-EXT
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
                   ACCEPT SA-STK-ITM
               ELSE
                   ACCEPT SA-STK
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
                 WHEN 3         ACCEPT SA-STK-3
      *                     4 Digit numeric Stock/Inventory item code
                 WHEN 4         ACCEPT SA-STK-4
      *                     5 Digit numeric Stock/Inventory item code
                 WHEN 5         ACCEPT SA-STK-5
      *                     6 Digit numeric Stock/Inventory item code
                 WHEN 6         ACCEPT SA-STK-6
      *                     7 Digit numeric Stock/Inventory item code
                 WHEN 7         ACCEPT SA-STK-7
      *                     8 Digit numeric Stock/Inventory item code
                 WHEN 8         ACCEPT SA-STK-8
      *                     9 Digit numeric Stock/Inventory item code
                 WHEN 9         ACCEPT SA-STK-9
      *                    10 Digit numeric Stock/Inventory item code
                 WHEN 10        ACCEPT SA-STK-10
      *                    11 Digit numeric Stock/Inventory item code
                 WHEN 11        ACCEPT SA-STK-11
      *                    12 Digit numeric Stock/Inventory item code
                 WHEN 12        ACCEPT SA-STK-12
      *                    13 Digit numeric Stock/Inventory item code
                 WHEN 13        ACCEPT SA-STK-13
      *                    14 Digit numeric Stock/Inventory item code
                 WHEN 14        ACCEPT SA-STK-14
               END-EVALUATE.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
              MOVE SPACES TO W10-EXT-ITEM
              GO TO STOCK-RESET-LIN
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO STOCK-LUP-DATA.
    IF NOT (W10-EXT-ITEM = SPACES)
        GO TO STOCK-LUP-CONTINUE.
    IF WS-LEDG1 > SPACES
        GO TO STOCK-LUP-CONTINUE.
      ACCEPT SA-STKD.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
              MOVE SPACES TO W10-EXT-ITEM
              GO TO STOCK-RESET-LIN
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO STOCK-LUP-DATA.

       STOCK-LUP-CONTINUE.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
            PERFORM CLEAR-L50.
            MOVE 6   TO CLIN.
            MOVE 1   TO W75-S W75-S1.

       STOCK-LUP-TAB-LOOP.
            MOVE SPACES  TO W75-CODE(W75-S1).
           IF W75-S1 < 18
              ADD 1   TO W75-S1
               GO TO STOCK-LUP-TAB-LOOP.
            MOVE ZERO   TO W75-S1.

       STOCK-LUP-ENTRY-TYPE.
          IF (W10-EXT-ITEM = SPACES) AND (W10-DESC = SPACES)
              MOVE "!"   TO W10-EXT-ITEM
    ELSE
    IF (W10-EXT-ITEM = SPACES) AND (WS-LEDG1 = SPACES)
              MOVE "D"   TO WS-ETYPE
              MOVE W10-DESC  TO W50-DATA
              MOVE 30   TO W50-LENGTH
        CALL "CBL_TOUPPER" USING W50-30
      BY VALUE W50-LENGTH
      RETURNING WS-STATUS
              MOVE W50-30 TO W10-DESC.
    IF WS-ETYPE = "D"
              MOVE W10-DESC  TO STK-DESC
              PERFORM START-AT-STOCK-DESC THRU READ-STOCK-EXIT
    ELSE
    IF WS-LEDG1 > SPACES
        MOVE WS-LEDG1  TO STK-LDG
              PERFORM CA00 THRU CA06
              MOVE W10-EXT-ITEM TO STK-CODE
        PERFORM START-AT-STOCK-LEDG THRU READ-STOCK-EXIT
    ELSE
              PERFORM CA00 THRU CA06
              MOVE W10-EXT-ITEM TO STK-CODE
              PERFORM START-AT-STOCK-CODE THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO STOCK-NO-RECORD.

       STOCK-GET-STOCK-REC.
           IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
               GO TO STOCK-SKIP-STOCK-READ.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
              SUBTRACT 1  FROM W75-S
               GO TO STOCK-FULL-SCREEN.
    IF (WS-LEDG1 > SPACES) AND (STK-LDG > WS-LEDG1)
              SUBTRACT 1  FROM W75-S
               GO TO STOCK-FULL-SCREEN.
    IF STK-IND > 1
        GO TO STOCK-GET-STOCK-REC.
    IF WS-TYPE = "C" OR "L"
        IF STK-IND > 0
     GO TO STOCK-GET-STOCK-REC.
    IF WS-TYPE = "C" OR "A"
        GO TO STOCK-SKIP-STOCK-READ.
      MOVE ZERO   TO W10-ORDER.
    IF STK-IND = ZERO
        IF NOT (STK-QUANT > STK-LEVEL)
     COMPUTE W10-ORDER =
         (STK-MAVG * STK-LTIME * 2) - STK-QUANT
     ADD 0.99  TO W10-ORDER.
    IF W10-ORDER < 1
        GO TO STOCK-GET-STOCK-REC.

       STOCK-SKIP-STOCK-READ.
            MOVE STK-CODE  TO W75-CODE(W75-S).
    IF STK-IND = 0
        MOVE "HIGHLIGHT FOREGROUND-COLOR  3"
     TO  WS-CONT
    ELSE
    IF STK-IND = 1
        MOVE "HIGHLIGHT FOREGROUND-COLOR  6"
     TO  WS-CONT
    ELSE
    IF STK-IND = 2
        MOVE "HIGHLIGHT FOREGROUND-COLOR  7"
     TO  WS-CONT
    ELSE
        MOVE "HIGHLIGHT FOREGROUND-COLOR  5"
     TO  WS-CONT.
            DISPLAY (CLIN, 2) STK-CODE WITH CONTROL WS-CONT.
    IF NOT (STK-ORD = ZERO)
        DISPLAY ( , 20) "*" WITH FOREGROUND-COLOR Black
     BACKGROUND-COLOR Cyan.
      MOVE STK-DESC  TO W10-DESC.
            DISPLAY ( , 21) W10-SDSC WITH CONTROL WS-CONT.
            MOVE STK-QUANT  TO W100-QNT.
            DISPLAY ( , 50) W100-QNT WITH CONTROL WS-CONT.
            MOVE STK-MTD  TO W100-NO.
            DISPLAY ( , 56) W100-NO WITH CONTROL WS-CONT.
            MOVE STK-MAVG  TO W100-NO.
            DISPLAY ( , 61) W100-NO WITH CONTROL WS-CONT.
          IF LS0-STLEV < 3
        MOVE STK-SELL  TO W100-S6V2
    ELSE
        MOVE STK-COST  TO W100-S6V2.
            DISPLAY ( , 71) W100-PRICE WITH CONTROL WS-CONT.
      MOVE ZERO   TO W100-NO.
    IF STK-IND = ZERO
        IF NOT (STK-QUANT > STK-LEVEL)
     COMPUTE W10-ORDER =
         (STK-MAVG * STK-LTIME * 2) - STK-QUANT
     ADD 0.99  TO W10-ORDER
     MOVE W10-ORD-UNITS
     TO W100-NO.
            DISPLAY ( , 66) W100-NO WITH CONTROL WS-CONT.
           IF CLIN < 21
              ADD 1   TO CLIN W75-S
               GO TO STOCK-GET-STOCK-REC.

       STOCK-FULL-SCREEN.
001020      DISPLAY " " AT 2303 WITH FOREGROUND-COLOR Brown HIGHLIGHT
             "," WITH FOREGROUND-COLOR Grey
       "PgUp" WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     "/" WITH FOREGROUND-COLOR Grey
       "PgDn" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "," WITH FOREGROUND-COLOR Grey
             "ENTER" WITH FOREGROUND-COLOR Brown HIGHLIGHT
             "=view," WITH FOREGROUND-COLOR Grey
       "F8" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "=order," WITH FOREGROUND-COLOR Grey
       "F9" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "=memo," WITH FOREGROUND-COLOR Grey
       "F10" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "=purch.history,"
        WITH FOREGROUND-COLOR Grey
             "Esc" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "ape" WITH FOREGROUND-COLOR Grey.

       STOCK-POSITION-CURSOR.
      PERFORM SAVE-SCREEN-2.
            MOVE 0   TO W75-S1.
             MOVE 0601           TO CPOS.

       STOCK-CRT-ATTRIB.
      MOVE 3920   TO W41-LENGTH.
      CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
           W42-ATTRIB2
           W41-LENGTH
     RETURNING WS-STATUS.
            MOVE 4000   TO W41-LENGTH.

       STOCK-GET-USER-OPT.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      DISPLAY SPACE AT 5040
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
              GO TO STOCK-ADJ-NO-KEYS
   WHEN DOWN-KEY
              GO TO STOCK-CHK-NO-KEYS
   WHEN ENTER-KEY
   WHEN CR-KEY
        PERFORM BD000
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO STOCK-GET-USER-OPT
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
        MOVE SPACES TO W10-EXT-ITEM
        GO TO STOCK-RESET-LIN
   WHEN PAGE-DOWN
              GO TO STOCK-GET-NEXT-PAGE
   WHEN PAGE-UP
              GO TO STOCK-GET-PREV-PAGE
   WHEN F8-KEY
              IF LS0-STLEV < 3
     PERFORM AA900-ALARM
        ELSE
     MOVE W42-ATTRIB2
     TO W42-ATTRIB6
     MOVE W43-SCREEN2
     TO W43-SCREEN6
     PERFORM BE000
     MOVE W42-ATTRIB6
     TO W42-ATTRIB2
     MOVE W43-SCREEN6
     TO W43-SCREEN2
        END-IF
   WHEN F9-KEY
        PERFORM STOCK-ADD-SPECS THRU STOCK-ADD-EXIT
        PERFORM STOCK-FULL-SCREEN
   WHEN F10-KEY
              IF NOT (W75-S1 = ZERO)
           MOVE W75-CODE(W75-S1)
     TO W16-ITEM
     PERFORM SAVE-SCREEN-5
     CALL "STP\STPPOR" USING
    LS-PARID LS-USER-ID LS0-PROGRAMS
    LS0-SECURITY W16-CALL
     PERFORM RESTORE-SCREEN-5
         END-IF
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE.
            GO TO STOCK-GET-USER-OPT.

       STOCK-GET-NEXT-PAGE.
           IF W75-S < 16
               GO TO STOCK-GET-USER-OPT.

       STOCK-NEW-PAGE.
             MOVE 0601           TO CPOS.
             PERFORM STOCK-CRT-EDGE UNTIL CLIN = 22.
             MOVE 6              TO CLIN.
             MOVE LOW-VALUES TO W75-KEYS.
            MOVE 1   TO W75-S W75-S1.
             PERFORM STOCK-LUP-TAB-LOOP.
             GO TO STOCK-GET-STOCK-REC.

       STOCK-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
             MOVE "J"            TO WS-OPTION.

       STOCK-GET-PREV-LOOP.
             PERFORM READ-STOCK-PREV THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
        MOVE SPACE  TO WS-OPTION
               GO TO STOCK-NEW-PAGE.
    IF (WS-LEDG1 > SPACES) AND (STK-LDG < WS-LEDG1)
              PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT
               GO TO STOCK-NEW-PAGE.
    IF STK-IND > 1
        GO TO STOCK-GET-PREV-LOOP.
    IF WS-TYPE = "C"
        IF STK-IND > 0
     GO TO STOCK-GET-PREV-LOOP.
           IF WS-IXS1 < 32
               ADD 1             TO WS-IXS1
               GO TO STOCK-GET-PREV-LOOP.
             GO TO STOCK-NEW-PAGE.

       STOCK-SAVE-FULL-SCREEN.
      DISPLAY " " AT 5072 WITH BACKGROUND-COLOR Cyan
          FOREGROUND-COLOR Blue.
             PERFORM STOCK-CRT-ATTRIB.
            MOVE 0   TO W44-FUNCTION.
            CALL X"B7" USING W44-FUNCTION W41-SAVE W43-SCREEN2.
           IF W75-S1 > 0
               GO TO STOCK-HIGHLIGHT.
             GO TO STOCK-GET-USER-OPT.

       STOCK-NO-RECORD.
            DISPLAY "No record - " AT 5012
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "N" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             "ext or "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "Q" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             "uit > <"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.

       STOCK-GET-USER-OPT-2.
            ACCEPT WS-OPTION AT 5038
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "N" OR "Q")
               GO TO STOCK-GET-USER-OPT-2.
           IF WS-OPTION = "N"
               GO TO STOCK-LUP-SCREEN.
            MOVE SPACES  TO W10-EXT-ITEM.
            GO TO STOCK-RESET-LIN.

       STOCK-ADJ-NO-KEYS.
           IF W75-S1 > 1
               SUBTRACT 1        FROM W75-S1 CLIN.
             GO TO STOCK-HIGHLIGHT.

       STOCK-CHK-NO-KEYS.
           IF W75-S1 = ZERO
               MOVE 1            TO W75-S1
               GO TO STOCK-HIGHLIGHT.
           IF W75-S1 < W75-S
               ADD 1             TO W75-S1 CLIN.

       STOCK-HIGHLIGHT.
             PERFORM STOCK-CRT-ATTRIB.
            DISPLAY W43-DET(CLIN) AT CPOS WITH REVERSE-VIDEO.
             GO TO STOCK-GET-USER-OPT.

       STOCK-ADD-SPECS.
           IF W75-S1 = ZERO
               GO TO STOCK-ADD-EXIT.
            MOVE W75-CODE(W75-S1)
     TO SME-CODE W10-EXT-ITEM.
             MOVE 1              TO SME-SEQ.
             PERFORM READ-STKMEM THRU READ-STKMEM-EXIT.
           IF WS-F-ERROR NOT = ZERO
        DISPLAY "No add. specifications" AT 5045
   WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
        CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS
               GO TO STOCK-ADD-EXIT.
             INITIALIZE W200-MEM-FLDS.

       STOCK-ADD-LOOP.
             MOVE SME-SEQ        TO WS-S1.
      MOVE SME-DATA  TO W200-MEM(WS-S1).
             PERFORM READ-STKMEM-NEXT THRU READ-STKMEM-EXIT.
           IF WS-F-ERROR = ZERO
        IF SME-CODE = W10-EXT-ITEM
                   GO TO STOCK-ADD-LOOP.
      PERFORM SAVE-SCREEN-3.
      MOVE 7   TO SHADE-ROW.
      MOVE 19   TO SHADE-COL.
      MOVE 60   TO SHADE-WIDTH.
      MOVE 15   TO SHADE-LINES.
      DISPLAY S-MEM.
      PERFORM SCREEN-SHADOW.
      DISPLAY SA-MEM.
             DISPLAY "ENTER to continue" AT 2355
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM RESTORE-SCREEN-3.

       STOCK-ADD-EXIT.
             EXIT.

       STOCK-CRT-EDGE.
            DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR Black
          FOREGROUND-COLOR Cyan
       "                  ³                            ³
      -       "   ³    ³    ³    ³         "
        WITH FOREGROUND-COLOR Cyan
       "³" WITH BACKGROUND-COLOR Black
         FOREGROUND-COLOR Cyan.
            ADD 1   TO CLIN.

       STOCK-RESET-LIN.
             MOVE STORE-LIN      TO LIN.

       STOCK-LOOKUP-EXIT.
             EXIT.

      /
      *    ****    C R E D I T O R   L O O K U P   R O U T I N E
      *
      *            (P R O C E S S   F U N C T I O N   K E Y S)
      *
       CREDITOR-LOOKUP         SECTION.

       CREDIT-LUP-SCREEN.
             PERFORM ERASE-SCREEN.
             DISPLAY S-CRD.
             MOVE SPACE          TO WS-OPTION.

       CREDIT-LUP-CLR-SCREEN.
             MOVE 0601           TO CPOS.
             PERFORM CREDIT-CRT-EDGE UNTIL CLIN = 22.
      MOVE SPACES  TO W15-NAME.

       CREDIT-LUP-DATA.
             ACCEPT SA-CRD.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
              MOVE SPACES TO W10-SUPP
              GO TO CREDIT-LOOKUP-EXIT
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO CREDIT-LUP-DATA.
      MOVE SPACES  TO WS-BLNK78.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
      DISPLAY SPACE AT 5001
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
      MOVE ALL "°"  TO WS-BLNK78.
      MOVE 6   TO CLIN.
      MOVE 1   TO W75-CS W75-CS1.

       CREDIT-LUP-TAB-LOOP.
      MOVE SPACES  TO W75-CNO(W75-CS1).
    IF W75-CS1 < 18
        ADD 1   TO W75-CS1
               GO TO CREDIT-LUP-TAB-LOOP.
      MOVE ZERO   TO W75-CS1.

       CREDIT-LUP-ENTRY-TYPE.
           IF W15-NAME = SPACES
               MOVE "!"          TO W15-NAME.
             MOVE 40             TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W15-NAME
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
             MOVE W15-NAME       TO P-NAME.
             PERFORM START-AT-CRED-NAME THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
               GO TO CREDIT-NO-RECORD.

       CREDIT-GET-CRED-RECORD.
           IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
               GO TO CREDIT-SKIP-CRED-READ.
             PERFORM READ-CREDIT-NEXT THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
        SUBTRACT 1  FROM W75-CS
               GO TO CREDIT-FULL-SCREEN.

       CREDIT-SKIP-CRED-READ.
      MOVE P-NUMBER  TO W75-CNO(W75-CS).
      DISPLAY SD-CRD AT LINE CLIN.
          IF CLIN < 21
              ADD 1   TO CLIN W75-CS
               GO TO CREDIT-GET-CRED-RECORD.

       CREDIT-FULL-SCREEN.
001020      DISPLAY " " AT 2303 WITH FOREGROUND-COLOR Brown HIGHLIGHT
             " highlight - " WITH FOREGROUND-COLOR Grey
       "PgUp" WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     "/" WITH FOREGROUND-COLOR Grey
       "PgDn" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       " - " WITH FOREGROUND-COLOR Grey
             "ENTER" WITH FOREGROUND-COLOR Brown HIGHLIGHT
             " select - " WITH FOREGROUND-COLOR Grey
             "Esc" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "ape" WITH FOREGROUND-COLOR Grey.

       CREDIT-SAVE-FULL-SCREEN.
      PERFORM SAVE-SCREEN-2.
      MOVE 0   TO W75-CS1.
             MOVE 0601           TO CPOS.

       CREDIT-CRT-ATTRIB.
             MOVE 3920           TO W41-LENGTH.
      CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
           W42-ATTRIB2
           W41-LENGTH
     RETURNING WS-STATUS.
      MOVE 4000   TO W41-LENGTH.

       CREDIT-GET-USER-OPT.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM CLEAR-L50.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
        GO TO CREDIT-ADJ-NO-KEYS
   WHEN DOWN-KEY
              GO TO CREDIT-CHK-NO-KEYS
   WHEN ENTER-KEY
   WHEN CR-KEY
        GO TO CREDIT-CHK-SELECT
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO CREDIT-GET-USER-OPT
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
        MOVE SPACES TO W10-SUPP
        GO TO CREDIT-LOOKUP-EXIT
   WHEN PAGE-DOWN
              GO TO CREDIT-GET-NEXT-PAGE
   WHEN PAGE-UP
              GO TO CREDIT-GET-PREV-PAGE
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO CREDIT-GET-USER-OPT.
      PERFORM AA900-ALARM.
            GO TO CREDIT-GET-USER-OPT.

       CREDIT-GET-NEXT-PAGE.
    IF W75-CS < 16
               GO TO CREDIT-GET-USER-OPT.

       CREDIT-NEW-PAGE.
             MOVE 0601           TO CPOS.
             PERFORM CREDIT-CRT-EDGE UNTIL CLIN = 22.
             MOVE 6              TO CLIN.
      MOVE LOW-VALUES TO W75-CKEYS.
      MOVE 1   TO W75-CS W75-CS1.
             PERFORM CREDIT-LUP-TAB-LOOP.
             GO TO CREDIT-GET-CRED-RECORD.

       CREDIT-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
             MOVE "J"            TO WS-OPTION.

       CREDIT-GET-PREV-LOOP.
             PERFORM READ-CREDIT-PREV THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
        MOVE SPACE  TO WS-OPTION
               GO TO CREDIT-NEW-PAGE.
           IF WS-IXS1 < 32
               ADD 1             TO WS-IXS1
               GO TO CREDIT-GET-PREV-LOOP.
             GO TO CREDIT-NEW-PAGE.

       CREDIT-NO-RECORD.
            DISPLAY "No record - " AT 5012
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "N" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "ext or "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "Q" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "uit > <"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.

       CREDIT-GET-USER-OPT-2.
            ACCEPT WS-OPTION AT 5038
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-OPTION = "N" OR "Q")
               GO TO CREDIT-GET-USER-OPT-2.
           IF WS-OPTION = "N"
               GO TO CREDIT-LUP-SCREEN.
      MOVE SPACES  TO W10-SUPP.
             GO TO CREDIT-LOOKUP-EXIT.

       CREDIT-ADJ-NO-KEYS.
    IF W75-CS1 > 1
        SUBTRACT 1  FROM W75-CS1 CLIN.
             GO TO CREDIT-HIGHLIGHT.

       CREDIT-CHK-NO-KEYS.
    IF W75-CS1 = ZERO
        MOVE 1   TO W75-CS1
               GO TO CREDIT-HIGHLIGHT.
    IF W75-CS1 < W75-CS
        ADD 1   TO W75-CS1 CLIN.

       CREDIT-HIGHLIGHT.
             PERFORM CREDIT-CRT-ATTRIB.
      DISPLAY W43-DET(CLIN) AT CPOS WITH REVERSE-VIDEO.
             GO TO CREDIT-GET-USER-OPT.

       CREDIT-CHK-SELECT.
    IF W75-CS1 = ZERO
               GO TO CREDIT-GET-USER-OPT.
      MOVE CSTART  TO WS1-CURSOR.
      MOVE W75-CNO(W75-CS1)
     TO P-NUMBER.
             PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
      MOVE ZERO   TO COL-AND-LINE.
      MOVE P-NUMBER  TO W10-SUPP.
             GO TO CREDIT-LOOKUP-EXIT.

       CREDIT-CRT-EDGE.
            DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR Black
          FOREGROUND-COLOR Cyan
       "      ³                                         ³
      -       "             ³            "
        WITH FOREGROUND-COLOR Cyan
       "³" WITH BACKGROUND-COLOR Black
         FOREGROUND-COLOR Cyan.
            ADD 1 TO CLIN.

       CREDIT-LOOKUP-EXIT.
             EXIT.

       COPY "CA000.STK".

       COPY "CA100.STK".

       COPY "CA200.STK".

       COPY "CA500.STK".

      /
      *  ****  E N Q U I R Y   ( O R D E R S )
      *
       BA000         SECTION 5.
       BA000-ENQUIRY.
             PERFORM ERASE-SCREEN.
            DISPLAY S04C.
      DISPLAY "C" AT 0404
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "ontroled or "
       "A" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "ll (Includes Priced only) or "
       "L" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "ow level Stock".
      MOVE "A"   TO WS-TYPE.

       BA05.
      ACCEPT WS-TYPE AT 0463
      WITH BACKGROUND-COLOR Magenta
    FOREGROUND-COLOR Grey HIGHLIGHT UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO BA999
        END-EVALUATE.
      CALL "CBL_TOUPPER" USING WS-TYPE
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-TYPE = "A" OR "C" OR "L")
        GO TO BA05.
      MOVE SPACES  TO WS-LEDG1.
      DISPLAY "Press " AT 0604
       "ENTER"
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
       " for all or key in Sales Ledger required".
            DISPLAY S-LEDG.

       BA08.
      ACCEPT WS-LEDG1 AT 0656
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO BA999
   WHEN F1-KEY
        PERFORM HELP-ROUTINE
        GO TO BA08
   WHEN F6-KEY
              PERFORM SAVE-SCREEN
        PERFORM DEPART-VIEW
              PERFORM RESTORE-SCREEN
        GO TO BA08
   WHEN OTHER
        PERFORM AA900-ALARM
        GO TO BA08
        END-EVALUATE.
      MOVE WS-LEDG1  TO DPT-CODE.
    IF (DPT-CODE = "XXXX" OR "YYYY") OR
       (DPT-SCDE = "VAT")
        GO TO BA08.
    IF WS-LEDG1 = SPACES
        MOVE "All Departments"
     TO W10-DEPT.
    IF NOT (WS-LEDG1 = SPACES)
        PERFORM READ-DEPART THRU READ-DEPART-EXIT
        IF WS-F-ERROR = 7
     GO TO BA08
        ELSE
     MOVE DPT-DESC TO W10-DEPT.
      DISPLAY W10-DEPT AT 0819
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      PERFORM CHECK-CORRECT.
    IF WS-OPTION = "N"
        GO TO BA08.
             PERFORM ERASE-SCREEN.
      PERFORM STOCK-LOOKUP.
             PERFORM ERASE-SCREEN.
            DISPLAY S04C.
            DISPLAY "Esc" AT 1210 WITH BACKGROUND-COLOR Magenta
     FOREGROUND-COLOR Brown HIGHLIGHT
       "ape or " WITH BACKGROUND-COLOR Magenta
        FOREGROUND-COLOR Cyan
       "N" WITH BACKGROUND-COLOR Magenta
         FOREGROUND-COLOR Brown HIGHLIGHT
       "ext range" WITH BACKGROUND-COLOR Magenta
          FOREGROUND-COLOR Cyan.
            MOVE SPACE   TO W10-EXT-ITEM.

       BA10.
            ACCEPT WS-OPTION AT 5031
      WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
        MOVE SPACES TO W10-EXT-ITEM
        GO TO BA999
                 WHEN OTHER
        PERFORM AA900-ALARM
        GO TO BA10
        END-EVALUATE.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "N")
        GO TO BA10.
      GO TO BA000-ENQUIRY.

       BA999.
             EXIT.

      /
      *  ****  D I S P L A Y   S T O C K   D E T A I L S
      *
       BD000         SECTION 5.
       BD000-ENQUIRY.
          IF W75-S1 = ZERO
              GO TO BD999.
            PERFORM SAVE-SCREEN.
             MOVE 4              TO W44-FUNCTION.
             PERFORM SCREEN-CONTENTS.
            PERFORM ERASE-SCREEN.
             DISPLAY S04.
    IF WS-USE-ITM = "Y"
        DISPLAY "/ITM" AT 0312
         "/" AT 0332.
    IF W10-ART11 = "Y"
        DISPLAY "VAT - Article 11/1g" AT 0849.
            MOVE W75-CODE(W75-S1)
     TO W10-EXT-ITEM.

       BD05.
             PERFORM CA000.
            MOVE STK-CODE  TO W10-EXT-ITEM.
            MOVE STK-XREF  TO W10-EXT-CODE2.
             MOVE STK-BIN        TO W10-BIN.
             MOVE STK-DESC       TO W10-DESC.
            MOVE STK-DESC2  TO W10-DESC2.
             MOVE STK-TAX        TO W10-TAX.
            MOVE STK-EXEMPT  TO W10-EXEMPT.
          IF LS0-STLEV < 3
        MOVE ZERO  TO W10-MARKUP W10-CMARKUP W10-WMARKUP
    ELSE
              MOVE STK-MARKUP  TO W10-MARKUP
              MOVE STK-CMARKUP  TO W10-CMARKUP
              MOVE STK-WMARKUP  TO W10-WMARKUP.
             MOVE STK-IND        TO W10-IND.
             MOVE STK-QUANT      TO W10-QUANT.
      MOVE STK-PER  TO W10-PER.
    IF W10-PER = SPACES OR LOW-VALUES
        MOVE "Each"  TO W10-PER.
      MOVE STK-PRC  TO W10-PRC.
            MOVE STK-LEVEL  TO W10-REORD.
             MOVE STK-LTIME      TO W10-LTIME.
          IF LS0-STLEV < 3
        MOVE ZERO  TO W10-COST
    ELSE
              MOVE STK-COST  TO W10-COST.
             MOVE STK-SELL       TO W10-SELL.
             MOVE STK-CASH       TO W10-CASH.
             MOVE STK-WSALE      TO W10-WSALE.
            MOVE STK-LDG  TO W10-LEDG.
             MOVE STK-GUAR       TO W10-GUAR.
             MOVE STK-ORD        TO W10-ORDER.
             MOVE STK-DTE  TO W22-DTE2.
             PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT.
             MOVE W22-DTE3       TO W10-ODTE.
            MOVE STK-PUR  TO W22-DTE2.
      PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT.
      MOVE W22-DTE3  TO W10-PURCH.
    IF STK-IND < 2
              IF STK-AVGE = ZERO
           MOVE STK-COST TO STK-AVGE.
          IF LS0-STLEV < 3
        MOVE ZERO  TO W10-AVRG
    ELSE
              MOVE STK-AVGE  TO W10-AVRG.
             MOVE STK-ACODE      TO W10-ACODE.
      MOVE 1   TO WS-S1.

       BD05-MKUPS.
      MOVE STK-UNT(WS-S1) TO W10-QNT.
    IF WS-S1 = 1
        MOVE W10-COST  TO W10-ICOST
    ELSE
        COMPUTE W10-ICOST ROUNDED = W10-COST * W10-QNT.
            COMPUTE W09-WMKUP(WS-S1) ROUNDED =
     ((STK-WSL(WS-S1) - W10-ICOST) * 100.000) / W10-ICOST.
            COMPUTE W09-MKUP(WS-S1) ROUNDED =
     ((STK-SL(WS-S1) - W10-ICOST) * 100.000) / W10-ICOST.
            COMPUTE W09-CMKUP(WS-S1) ROUNDED =
     ((STK-CSH(WS-S1) - W10-ICOST) * 100.000) / W10-ICOST.
    IF WS-S1 < 4
        ADD 1   TO WS-S1
        GO TO BD05-MKUPS.
      DISPLAY S06C.
    IF WS-EXT-STK = "Y"
        DISPLAY S05-EXT
    ELSE
    IF W10-STCK = "A"
        IF WS-USE-ITM = "Y"
     DISPLAY S05-ITM
        ELSE
           DISPLAY S05
        END-IF.
          IF STK-ACT = ZERO
              MOVE SPACE  TO W10-ACTIVE
           ELSE
              MOVE STK-ACT  TO W22-DTE2
        PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT
        MOVE W22-DTE3  TO W10-ADATE.
            MOVE STK-MTDC  TO W10-MTDC.
            MOVE STK-YTDC  TO W10-YTDC.
          IF (LS0-STLEV < 9) AND
             (LS-OK = "N")
              MOVE ZERO  TO W10-COST W10-MARKUP
                                    W10-AVRG W10-CMARKUP
                                    W10-MTDC W10-WMARKUP
        W10-YTDC
    ELSE
       MOVE W09-MARKUP  TO W10-MARKUP
       MOVE W09-CMARKUP  TO W10-CMARKUP
       MOVE W09-WMARKUP  TO W10-WMARKUP.
             DISPLAY S06.
             PERFORM AA400.
           IF NOT (STK-SUPP = SPACES OR LOW-VALUES)
               MOVE STK-SUPP     TO P-NUMBER
               PERFORM READ-CREDIT THRU READ-CREDIT-EXIT
               IF WS-F-ERROR = ZERO
     DISPLAY "Supplier      " AT 2303
       P-NAME AT 2318
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
    IF NOT (STK-ORD = ZERO)
        MOVE STK-CODE  TO ORD-EXT-ITEM
        PERFORM START-AT-ORD-ITEM THRU READ-PORDER-EXIT
        IF WS-F-ERROR = ZERO
     PERFORM READ-PORDER-NEXT THRU READ-PORDER-EXIT
     IF ORD-EXT-ITEM = STK-CODE
         MOVE ORD-CRED  TO P-NUMBER
         PERFORM READ-CREDIT THRU READ-CREDIT-EXIT
         IF WS-F-ERROR = ZERO
      DISPLAY "Supp:" AT 1533
        P-NAME AT 1539
        WITH FOREGROUND-COLOR Cyan HIGHLIGHT
      BACKGROUND-COLOR Magenta
      DISPLAY "OrNo:" AT 1633
        ORD-ORD AT 1639
        WITH FOREGROUND-COLOR Cyan HIGHLIGHT
      BACKGROUND-COLOR Magenta
       "/" WITH FOREGROUND-COLOR Brown HIGHLIGHT
         BACKGROUND-COLOR Magenta
        ORD-LINE WITH FOREGROUND-COLOR Cyan
        HIGHLIGHT
        BACKGROUND-COLOR Magenta.

       BD10.
      DISPLAY "F9" AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     " to show additional specifications"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             DISPLAY "Press any other key to continue" AT 5040
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT WS-OPTION AT 5072
      WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
            PERFORM CLEAR-L50.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
   WHEN NOT F9-KEY GO TO BD20
        END-EVALUATE
    ELSE
        GO TO BD20.
      MOVE STK-CODE  TO SME-CODE.
             MOVE 1              TO SME-SEQ.
             PERFORM READ-STKMEM-LOCK THRU READ-STKMEM-EXIT.
           IF WS-F-ERROR NOT = ZERO
        MOVE W42-ATTRIB  TO W42-ATTRIB6
        MOVE W43-SCREEN  TO W43-SCREEN6
        MOVE "No additional specifications"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        MOVE W42-ATTRIB6  TO W42-ATTRIB
        MOVE W43-SCREEN6  TO W43-SCREEN
        GO TO BD10.
             INITIALIZE W200-MEM-FLDS.

       BD15.
             MOVE SME-SEQ        TO WS-S1.
      MOVE SME-DATA  TO W200-MEM(WS-S1).
             PERFORM READ-STKMEM-NEXT-LOCK THRU READ-STKMEM-EXIT.
           IF WS-F-ERROR = ZERO
               IF SME-CODE = STK-CODE
     GO TO BD15.
      PERFORM SAVE-SCREEN-3.
      MOVE 7   TO SHADE-ROW.
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
      PERFORM RESTORE-SCREEN-3.
      GO TO BD10.

       BD20.
      MOVE 18   TO W75-S.

       BD25.
    IF W75-CODE(W75-S) = SPACES
        SUBTRACT 1  FROM W75-S
        GO TO BD25.
            MOVE W75-CODE(W75-S)
     TO W10-EXT-ITEM.
             PERFORM CA000.
    IF WS-ETYPE = "I"
        GO TO BD35.
            PERFORM START-AT-STOCK-DESC THRU READ-STOCK-EXIT.
       BD30.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
    IF NOT (W10-EXT-ITEM = STK-CODE)
        GO TO BD30.

       BD35.
            PERFORM RESTORE-SCREEN.

       BD999.
             EXIT.

       BE000        SECTION 5.
       BE00.
          IF W75-S1 = ZERO
              GO TO BE999.
      MOVE WS1-CURS  TO CRT-LINE.
            PERFORM SAVE-SCREEN.
            MOVE W75-CODE(W75-S1)
     TO W10-EXT-ITEM.
             PERFORM CA000.
      MOVE STK-DESC  TO W10-DESC.
      MOVE STK-COST  TO W10-COST.
      MOVE ZERO   TO W10-ORDER W10-TERMS.
      MOVE SPACES  TO W10-SUPP  W10-SNAME W10-TEL.
    IF STK-IND = ZERO
        IF NOT (STK-QUANT > STK-LEVEL)
     COMPUTE W10-QUANT ROUNDED =
         (STK-MAVG * STK-LTIME * 2) - STK-QUANT
     MOVE W10-QUANT
     TO W10-ORDER.
    IF NOT (STK-SUPP = SPACES OR LOW-VALUES)
               MOVE STK-SUPP     TO P-NUMBER
               PERFORM READ-CREDIT THRU READ-CREDIT-EXIT
               IF WS-F-ERROR = ZERO
     MOVE P-NUMBER TO W10-SUPP
     MOVE P-NAME  TO W10-SNAME
     MOVE P-CONTACT
     TO W10-CONTACT
     MOVE P-TEL  TO W10-TEL
     MOVE P-TERMS  TO W10-STERMS.
            DISPLAY S07.
    IF WS-EXT-STK = "Y"
        DISPLAY S07-EXT.
      MOVE 8   TO SHADE-ROW.
      MOVE 20   TO SHADE-COL.
      MOVE 55   TO SHADE-WIDTH.
      MOVE 12   TO SHADE-LINES.
             PERFORM SCREEN-SHADOW.

       BE05.
            DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "=Help,"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "F2" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             "=Creditor Lookup,"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "F3" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             "=View Suppliers,"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "F10" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             "=Purchase History"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
      ACCEPT S07B.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
        MOVE SPACES TO W10-SUPP
        GO TO BE15
         WHEN F1-KEY
        PERFORM HELP-ROUTINE
   WHEN F2-KEY
              PERFORM SAVE-SCREEN-5
              MOVE 4  TO W44-FUNCTION
              PERFORM SCREEN-CONTENTS
              PERFORM CREDITOR-LOOKUP
              PERFORM RESTORE-SCREEN-5
   WHEN F3-KEY
        PERFORM BF000
        GO TO BE05
   WHEN F10-KEY
              MOVE W10-EXT-ITEM
     TO W16-ITEM
        MOVE W10-SUPP
     TO W16-CRED
        MOVE W10-COST
     TO W16-COST
        PERFORM SAVE-SCREEN-5
        CALL "STP\STPPOR" USING
       LS-PARID LS-USER-ID LS0-PROGRAMS
       LS0-SECURITY W16-CALL
        PERFORM RESTORE-SCREEN-5
        MOVE W16-CRED
     TO W10-SUPP
        MOVE W16-COST
     TO W10-COST
        DISPLAY S07
   WHEN OTHER
        PERFORM AA900-ALARM
        GO TO BE05
        END-EVALUATE
              DISPLAY W10-SUPP AT 1234
         WITH FOREGROUND-COLOR Grey HIGHLIGHT
       BACKGROUND-COLOR Magenta
              IF W10-SUPP = SPACES
           GO TO BE05.
          IF W10-SUPP = SPACES
              GO TO BE15.
      MOVE 5   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W10-SUPP
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
      MOVE W10-SUPP  TO P-NUMBER.
            PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
              MOVE 1   TO WS-ERROR
           ELSE
          IF P-NUMBER NOT = W10-SUPP
              MOVE 2   TO WS-ERROR
           ELSE
              MOVE 0   TO WS-ERROR.
          IF WS-ERROR NOT = ZERO
              MOVE "No Record"  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BE05.
      MOVE P-NAME  TO W10-SNAME.
      MOVE P-NUMBER  TO COR-CRED.
      MOVE W10-EXT-ITEM  TO COR-ITEM.
      MOVE W12-T-YMD  TO COR-DATE.
      PERFORM READ-CORDER THRU READ-CORDER-EXIT.
    IF WS-F-ERROR = 48
        MOVE P-CONTACT  TO W10-CONTACT
        MOVE P-TERMS  TO W10-STERMS
        DISPLAY S07B
        MOVE ZERO  TO WS-IND
    ELSE
        MOVE 1   TO WS-IND
        MOVE COR-CONTACT  TO W10-CONTACT
        MOVE COR-TERMS  TO W10-TERMS
        MOVE COR-QUANT  TO W10-ORDER
        MOVE COR-UCOST  TO W10-COST
        DISPLAY S07.
            PERFORM CLEAR-L50.
      ACCEPT S07C.
      ACCEPT S07E.
    IF W10-ORDER = ZERO
        IF WS-IND = 1
     PERFORM BE40
        END-IF
        GO TO BE15.

       BE10.
      ACCEPT S07F.
    IF W10-COST = ZERO
        GO TO BE10.
      ACCEPT S07G.
      MOVE W42-ATTRIB  TO W42-ATTRIB5.
      MOVE W43-SCREEN  TO W43-SCREEN5.
      PERFORM CHECK-CORRECT.
      MOVE W42-ATTRIB5  TO W42-ATTRIB.
      MOVE W43-SCREEN5  TO W43-SCREEN.
          IF WS-OPTION = "N"
              GO TO BE05.
      MOVE W10-SUPP  TO COR-CRED.
      MOVE W10-EXT-ITEM  TO COR-ITEM.
      MOVE W12-T-YMD  TO COR-DATE.
      MOVE W10-ORDER  TO COR-QUANT.
      MOVE W10-COST  TO COR-UCOST.
      MOVE W10-TERMS  TO COR-TERMS.
      MOVE W10-CONTACT  TO COR-CONTACT.
      MOVE LS0-NO  TO COR-DELIV.
    IF WS-IND = ZERO
        PERFORM WRITE-CORDER THRU WRITE-CORDER-EXIT
    ELSE
        PERFORM REWRITE-CORDER THRU WRITE-CORDER-EXIT.

       BE15.
      MOVE 18   TO W75-S.

       BE20.
    IF W75-CODE(W75-S) = SPACES
        SUBTRACT 1  FROM W75-S
        GO TO BE20.
            MOVE W75-CODE(W75-S)
     TO W10-EXT-ITEM.
             PERFORM CA000.
    IF WS-ETYPE = "I"
        GO TO BE35.
            PERFORM START-AT-STOCK-DESC THRU READ-STOCK-EXIT.

       BE30.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
    IF NOT (W10-EXT-ITEM = STK-CODE)
        GO TO BE30.

       BE35.

            PERFORM RESTORE-SCREEN.
      MOVE CRT-LINE  TO WS1-CURS.
      GO TO BE999.

       BE40.
      MOVE "Must Item be deleted from Order Y/N  [Y]"
     TO WS-ERR-MES.
      MOVE "Y"   TO WS-OPTION.
            PERFORM OPT-MESSAGE.
    IF NOT (WS-OPTION = "Y" OR "N")
        GO TO BE40.
    IF WS-OPTION = "Y"
        PERFORM DELETE-CORDER-REC THRU WRITE-CORDER-EXIT.

       BE999.
      EXIT.
      /
      *    ****   D I S P L A Y   S U P P L I E R S
      *
      * ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      * ³ Supplier     XXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX ³
      * ³ Alt Supplier XXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX ³
      * ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BF000        SECTION 5.
       BF00.
      PERFORM SAVE-SCREEN-5.
            PERFORM CLEAR-L50.
      MOVE 13   TO SHADE-ROW.
      MOVE 11   TO SHADE-COL.
      MOVE 62   TO SHADE-WIDTH.
      MOVE 3   TO SHADE-LINES.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -       "ÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1209
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
       "¿" WITH FOREGROUND-COLOR Grey HIGHLIGHT
         BACKGROUND-COLOR Grey.
      DISPLAY "³ Supplier
      -       "             " AT 1309
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
       "³" WITH FOREGROUND-COLOR Grey HIGHLIGHT
         BACKGROUND-COLOR Grey.
      DISPLAY "³ Alt Supplier
      -       "             " AT 1409
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
       "³" WITH FOREGROUND-COLOR Grey HIGHLIGHT
         BACKGROUND-COLOR Grey.
      DISPLAY "À" AT 1509
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -       "ÄÄÄÄÄÄÄÄÄÄÄÄÙ"
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Grey.
      PERFORM SCREEN-SHADOW.

       BF05.
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
      PERFORM RESTORE-SCREEN-5.

       BF999.
      EXIT.
      *    *************************************************************
      *                 I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT    SECTION 8.
       ZA000-START.
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

            MOVE 3   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-USE-ITM  TO WS-USE-ITM.
      MOVE PAR-EXT-STK  TO WS-EXT-STK.
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
            OPEN I-O CORDER.
    IF NOT (WS-STATUS = "41")
        IF WS-STATUS = "05" OR "39"
     CLOSE CORDER
     OPEN OUTPUT CORDER
     CLOSE CORDER
     OPEN I-O CORDER
        END-IF
              IF RUNTIME-ERROR
     IF FLE-LOCKED
               GO TO ZA200
     ELSE
     IF FLE-LIMIT
         GO TO ZA49
     ELSE
     IF IDX-CORRUPT
         MOVE 48  TO WS-F-ERROR
         GO TO ZA50.
      GO TO ZA999-EXIT.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             GO TO ZA205.
       ZA50.
          IF WS-F-ERROR = 48
              MOVE "ORDER file"
     TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Press any key to continue" AT 1012.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
       ZA51.
             EXIT PROGRAM.

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
               WHEN 2     MOVE W02-NETWORK  TO WS-FILE
                          MOVE WS-NETKEY    TO WS-KEY
               WHEN 3     MOVE W02-CREDIT   TO WS-FILE
                          MOVE P-NUMBER     TO WS-KEYX
               WHEN 7     MOVE W02-DEPART   TO WS-FILE
                          MOVE DPT-CODE     TO WS-KEYX
               WHEN 15    MOVE WS-PARID     TO WS-FILE
                          MOVE WS-PARKEY    TO WS-KEY
               WHEN 16    MOVE W02-PORDER   TO WS-FILE
                          MOVE ZERO         TO WS-KEY
               WHEN 22    MOVE W02-STOCKF   TO WS-FILE
                          MOVE STK-CODE     TO WS-KEYX
               WHEN 25    MOVE W02-STKMEM   TO WS-FILE
                          MOVE SME-KEY      TO WS-KEYX
               WHEN 48    MOVE W02-CORDER   TO WS-FILE
                          MOVE COR-KEY      TO WS-KEYX
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STPORD"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
