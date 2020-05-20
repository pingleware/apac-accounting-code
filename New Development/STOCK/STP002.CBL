      $set LINKCOUNT"512" GNT"STP002.GNT"
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
      *  Memo File
      *                                               ___ 
      *         Remove STKMEM copies                 | x |
      *                                               ---
      *                                               ___ 
      *         Remove all variables for Memo        |   |
      *                                               ---
      *                                               ___ 
      *         Remove screen layouts for Memo       |   |
      *                                               ---
      *                                               ___ 
      *         Include COPY "STKMEM.PRO" for        | x |
      *                          memo lookups         ---
      *                                               ___ 
      *         Exchange Memo lookup to CALL         |   |
      *                                 STPMEMO       ---
      *                                               ___ 
      *         Exchange Memo maintenance to         |   |
      *                             CALL STP007       ---
      *
      ******************************************************************
      *                                                                *
      *    ******   ********  *******     ****      ****     ******    *
      *   **    **     **     **    **   **  **    **  **   **    **   *
      *   **           **     **    **  **    **  **    **       **    *
      *    ******      **     *******   **    **  **    **     **      *
      *         **     **     **        **    **  **    **   **        *
      *   **    **     **     **         **  **    **  **   **         *
      *    ******      **     **          ****      ****    ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S T O C K A M E N D   E X I S T I N G   R E C O R D        *
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
      *  Jan 2010 - Allow for two discounts for a Debtor on the same   *
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
       PROGRAM-ID.     STP002.
       AUTHOR.         J.W.LEMMON.
       DATE-WRITTEN.   JANUARY 1982.

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

       COPY "DEPART.SL".

       COPY "LEDTRF.SL".

       COPY "PARAM.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

       COPY "SPARTS.SL".

       COPY "STKALT.SL".

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

       COPY "CONTROL.FDE".

       COPY "DEPART.FDE".

       COPY "LEDTRF.FDE".

      *
      *   Using External file as the called program STPALT also updates
      *   the Recovery file while generating the entries for the
      *   Alternate Index file.
      *
       COPY "RECOVER.FDE".

       COPY "PARAM.FDE".

       COPY "SHARED.FDE".

       COPY "SPARTS.FDE".

       COPY "STKALT.FDE".

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
       77  WS-CARDEX               PIC  X(01).
       77  WS-ETYPE                PIC  X(01).
       77  WS-OPTION               PIC  X(01).
       77  WS-INSTR                PIC  X(01).
       77  WS-SKIP                 PIC  X(01).
       77  WS-ADJ                  PIC  X(01).
       77  WS-ERROR                PIC  9(01).
       77  WS-IND                  PIC  9(01)    VALUE 0.
       77  WS-INDB                 PIC  9(01)    VALUE 0.
       77  WS-INDP                 PIC  9(01)    VALUE 0.
       77  WS-INDS                 PIC  9(01)    VALUE 0.
       77  WS-AMEND                PIC  9(01)    VALUE 0.
       77  WS-CONREC               PIC  9(01)    VALUE ZERO.
       77  WS-LEDG                 PIC  X(04).
       77  WS-LEDG1                PIC  X(04).
       77  WS-USE-CASES            PIC  X(01).
       77  WS-USE-PACKS            PIC  X(01).
       77  WS-USE-ITM              PIC  X(01).
       77  WS-EXT-STK              PIC  X(01).
       77  WS-CON-FACT             PIC  X(01).
       77  WS-COMP                 PIC  9(01).
       77  WS-WEEK                 PIC  9(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-BATCH                PIC  9(05).
       77  WS-PASSWORD             PIC  X(08)    VALUE SPACES.
       77  WS-QPASSWORD            PIC  X(06)    VALUE SPACES.
       77  WS-PRINT                PIC  9(01).
       77  WS-LABCDE               PIC  X(08).
       77  WS-PASS                 PIC  X(08)    VALUE SPACES.
       77  WS-DESC-H1              PIC  X(14)    VALUE "Description".
       77  WS-DESC-H2              PIC  X(14)    VALUE "Description 2".
       77  WS-RT-HD                PIC  X(10)    VALUE "-Retail   ".
       77  WS-WS-HD                PIC  X(10)    VALUE "-Wholesale".
       77  WS-CS-HD                PIC  X(10)    VALUE "-Cash sale".
       77  WS-NODEP                PIC  X(30)    VALUE "NO SUCH   D E P A R T M E N T".
       77  PRG-NAME                PIC  X(12)    VALUE SPACES.
       77  PRG-STOCK-LUP           PIC  X(12)    VALUE "STP\STPLOOK".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.

       COPY "WS.WS".

       01  WS-HELP.
           03  WS-MODULE           PIC  X(03)    VALUE "STP".
           03  WS-PROG             PIC  X(03)    VALUE "002".

       COPY "LEDTRF.WS".

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.   ³
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
           03  CRT-START           PIC  9(02).
           03  CRT-END             PIC  9(02).
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15)    VALUE "STP002".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40)    VALUE "STOCK/INVENTORY AMENDMENT".
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

       01  W02-FILE-IDS.
           03  W02-PRINTER.
               05  W02-REPORT      PIC  X(07)    VALUE "STP002.".
               05  W02-USER        PIC  X(05)    VALUE SPACES.

       COPY "W05.VAT".

       COPY "W05.RND".

       COPY "W10.STK".

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

       COPY "W15.STK".

       COPY "W20.WS".

       COPY "W25.STK".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W75-KEYS.
           03  W75-S               PIC  9(02) COMP-5.
           03  W75-S1              PIC  9(02) COMP-5.
           03  W75-S2              PIC  9(02) COMP-5.
           03  W75-SLEDG           PIC  X(04) OCCURS 10.

       01  W80-EDIT.
           03  W80-CNO             PIC  9(04).
           03  W80-V1.
               05  W80-S7V2        PIC  Z(06)9.99-.
           03  W80-V2.
               05  W80-S5V2        PIC  Z(04)9.99-.
           03  W80-DTE.
              05  W80-DATE         PIC  Z9/99/9999.
           03  W80-QNT.
               05  W80-5           PIC  Z(04)9.99.
           03  W80-QNT1                          REDEFINES W80-QNT.
               05  W80-3V2         PIC  Z(02)9.9999.
           03  W80-NO.
              05  W80-REC          PIC  ZZZZ9.
           03  W80-DEL.
              05  W80-2NM          PIC  9(02).
      *
      *    ****   T A B L E   F O R   P R I C E   C H E C K   D I G I T
      *           C A L C U L A T I O N S
      *
       01  W80-CHECK-DIGIT.
           03  W80-VALUES.
               05  FILLER            PIC  9(09)  VALUE 516273849.
               05  FILLER            PIC  9(09)  VALUE 594837261.
               05  FILLER            PIC  9(09)  VALUE 246891357.
           03  W80-TABLE-VALUES                  REDEFINES W80-VALUES.
               05  W80-5P            PIC  9(01)  OCCURS 9.
               05  W80-5N            PIC  9(01)  OCCURS 9.
               05  W80-2N            PIC  9(01)  OCCURS 9.
           03  W80-SUB               PIC  9(02)  COMP-5.
           03  W80-RESULT            PIC  9(02).
           03  W80-RES-DIG                       REDEFINES W80-RESULT.
               05  W80-R-DIG         PIC  9(01)  OCCURS 2.
           03  W80-COMP                          REDEFINES W80-RESULT.
               05  W80-CD            PIC  9(01).
               05  W80-UNIT          PIC  9(01).
           03  W80-WEIGHTS.
               05  W80-WGHTS         PIC  9(05).
           03  W80-WGHT                          REDEFINES W80-WEIGHTS.
               05  W80-WEIGHT        PIC  9(01)  OCCURS 5.
           03  W80-BAR-CODE.
               05  W80-XREF.
               07  W80-GROUP         PIC  9(02).
               07  W80-ITM-CODE      PIC  9(04).
               05  W80-PRC-CD        PIC  9(01).
               05  W80-PRICE         PIC  9(03)V99.
               05  W80-PRC-DIGR                  REDEFINES W80-PRICE.
                   07  W80-PRC-DIG   PIC  9(01)  OCCURS 5.
               05  W80-BAR-CD        PIC  9(01).
           03  W80-BAR-CODE-CHR                  REDEFINES W80-BAR-CODE.
               05  W80-BAR-CODE-CH   PIC  9(01)  COMP OCCURS 13.
           03  W80-BAR-CODE-EAN13                REDEFINES W80-BAR-CODE.
               05  W80-BAR-CODE-12   PIC  9(12)  COMP.
               05  W80-BAR-CODE-CHK-DIG
                                     PIC  9(01)  COMP.
           03  W80-BAR-CODE-RESULT-1 PIC  9(04)  COMP.
           03  W80-BAR-CODE-RESULT-2 PIC  9(04)  COMP.
           03  W80-BAR-CODE-RESULT-3 PIC  9(04)  COMP.
           03  W80-BAR-CODE-CALC                 REDEFINES W80-BAR-CODE-RESULT-3.
               05  W80-BAR-CODE-TEN  PIC  9(03)  COMP.
               05  W80-BAR-CODE-UNIT PIC  9(01)  COMP.

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

       01  S02.
           03  LINE  5 COLUMN  2 VALUE "Logged In"      FOREGROUND-COLOR Cyan HIGHLIGHT BLINK.

       01  S04B.
          03  LINE  2 COLUMN 33 FOREGROUND-COLOR Grey HIGHLIGHT
                                 VALUE "STOCK AMENDMENT".

       COPY "STOCK.CRT".

       01  S20.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE 11 COLUMN 20 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
               05  LINE 12 COLUMN 20 VALUE "³ Enter quantity Amend Password        ³".
               05  LINE 13 COLUMN 20 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S20A.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 12 COLUMN 52 PIC  X(06) USING WS-QPASSWORD AUTO SECURE.

       COPY "DEPART.CRT".

      *
      *    ****    PRODUCTION DETAILS DISPLAY SCREEN
      *
       01  S32.
           03  LINE  5 COLUMN 21 VALUE "Details of Manufacture (Include Labour)" FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE  6 COLUMN  8 VALUE "Comments".
           03  LINE  7 COLUMN  4 VALUE "Stock code".
           03          COLUMN 19 VALUE "ITM".
           03          COLUMN 28 VALUE "Description".
           03          COLUMN 66 VALUE "Quantity".

       01  S32-ITEM-CODE.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               05  S32A.
                   07  COLUMN  4 PIC  X(14)/X(03) USING W10-EXT-CODE2 AUTO.

               05  S32B.
                   07  COLUMN  4 PIC  X(18) USING W10-EXT-CODE2 AUTO.

               05  S32C.
                   07  COLUMN  4 PIC  X(14) USING W10-EXT-CODE2 AUTO.
                   
               05  S32-3.
                   07  COLUMN  4 PIC Z(03) USING W10-I3 AUTO.
               
               05  S32-4.
                   07  COLUMN  4 PIC Z(04) USING W10-I4 AUTO.
               
               05  S32-5.
                   07  COLUMN  4 PIC Z(05) USING W10-I5 AUTO.
               
               05  S32-6.
                   07  COLUMN  4 PIC Z(06) USING W10-I6 AUTO.
               
               05  S32-7.
                   07  COLUMN  4 PIC Z(07) USING W10-I7 AUTO.
               
               05  S32-8.
                   07  COLUMN  4 PIC Z(08) USING W10-I8 AUTO.
               
               05  S32-9.
                   07  COLUMN  4 PIC Z(09) USING W10-I9 AUTO.
               
               05  S32-10.
                   07  COLUMN  4 PIC Z(10) USING W10-I10 AUTO.
               
               05  S32-11.
                   07  COLUMN  4 PIC Z(11) USING W10-I11 AUTO.
               
               05  S32-12.
                   07  COLUMN  4 PIC Z(12) USING W10-I12 AUTO.
               
               05  S32-13.
                   07  COLUMN  4 PIC Z(13) USING W10-I13 AUTO.
               
               05  S32-14.
                   07  COLUMN  4 PIC Z(14) USING W10-I14 AUTO.

       01  S38.
           03                    PIC ZZZZ9.9999 USING W10-MQNT AUTO  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.

       01  S-CONV.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black
               05  LINE 10 COLUMN 25 VALUE  "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
               05          COLUMN    VALUE                                 "¿" FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 11 COLUMN    VALUE  "³".                               
               05          COLUMN    VALUE   " Convertion Factor "             FOREGROUND-COLOR Red.
               05          COLUMN    VALUE                      "          "   FOREGROUND-COLOR Cyan HIGHLIGHT.
               05          COLUMN    VALUE                                " "  FOREGROUND-COLOR Grey.
               05          COLUMN    VALUE                                 "³" FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 12 COLUMN    VALUE  "À".                               
               05          COLUMN    VALUE   "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" FOREGROUND-COLOR Grey HIGHLIGHT.

       01  SA-FACT.
           03  LINE 11 COLUMN 45 PIC ZZZZZ9.999 USING W10-FACT AUTO BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.

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
       AA000-INIT.
      *
      *   **** Check the User's security level for Stock/Inventory
      *
           IF LS0-STLEV < 2
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM BC000.
             CLOSE RECOVER.

       AA49.
             EXIT PROGRAM.

       COPY "AA50.LUP".

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
      *    ³                      OPT-MESSAGE                          ³
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
      *    ³      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿   ³
      *    ³      ³          Print transactions (Y/N) [ ]          ³°° ³
      *    ³      ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°° ³
      *    ³        °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° ³
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

       COPY "AA300.STK".

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
             DISPLAY W10-DEPT AT 1226 WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Magenta.

       AA405.
             EXIT.
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

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                        STOCK-MEMO                         ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ The item code  of  the  Stock  record  for which the MEMO ³
      *    ³ details are to be displayed must be in W10-EXT-ITEM.      ³
      *    ³                    PERFORM STOCK-MEMO.                    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *
      *COPY "STKMEM.PRO".
      
       COPY "DATE.CHK".

       COPY "DATE.CNV".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000-READ        SECTION.

       COPY "CONTROL.RD".

       COPY "DEPART.RD".

       COPY "LEDTRF.RD".

       COPY "PARAM.RD".

       COPY "SHARED.RD".

       COPY "STKALT.RD".

       COPY "STOCK.RD".

       COPY "SPARTS.RD".

      *
      *   R E W R I T E   &    W R I T E   F I L E S   R O U T I N E S
      *
       AD000-WRITE        SECTION.

       COPY "CONTROL.WR".

       COPY "DEPART.WR".

       COPY "LEDTRF.WR".

       COPY "PARAM.WR".

       COPY "SHARED.WR".

       COPY "STKALT.WR".

       COPY "STKDEX.WR".

       COPY "STOCK.WR".

       COPY "SPARTS.WR".

       COPY "STOCK.1ST".

       COPY "DEPART.LUP".

       COPY "APAC.HLP".

      *
      *       ****   *****   ***    ***   *   *  *****  ****   *   *
      *       *   *  *      *   *  *   *  *   *  *      *   *  *   *
      *       ****   ***    *      *   *  *   *  ***    ****    * *
      *       *   *  *      *   *  *   *   * *   *      *   *    *
      *       *   *  *****   ***    ***     *    *****  *   *    *
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *   THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *        ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *        ³ ORIGINAL ACTION (REC-TYPE)  ÄÄ  RECOVERY PROCESS ³
      *        ³ 0 = RECORD CHANGED          ÄÄ     (REWRITE)     ³
      *        ³ 1 = RECORD WAS ADDED        ÄÄ     (DELETE)      ³
      *        ³ 2 = RECORD WAS DELETED      ÄÄ     (WRITE)       ³
      *        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY000        SECTION.
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
      *    ****   S T O C K   P A R T S   F I L E   (MANUFACTURING) ****
      *    *************************************************************
       AY20.
             MOVE 20                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
             MOVE PRT-REC1           TO REC-SPARTS.
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
      *    *************************************************************
      *    **** S T O C K   A L T E R N A T E   I N D E X   F I L E ****
      *    *************************************************************
      *AY49S.
      *      MOVE 49                 TO REC-FILE.
      *      MOVE ZERO               TO REC-KEY.
      *      MOVE WS-ACTION          TO REC-TYPE
      *      MOVE STKA-RECORD1       TO REC-STKALT.
      *      GO TO AY50.
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
      *    ****   A R E   A N Y   U P D A T E S   I N   P R O G R E S S
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

      *COPY "CA100.STK".

      *COPY "CA200.STK".

      *COPY "CA500.STK".

       COPY "ROUND.PRC".

       COPY "LEDTRF.UPD".

      *
      *    ****    B A R C O D E   C A L C U L A T I O N
      *
       BA000        SECTION 5.
       BA100.
             MOVE W10-XREF           TO W80-XREF.
             MOVE W10-CASH           TO W80-PRICE.
             MOVE ZERO               TO W80-WGHTS.
           IF W80-PRC-DIG(1) > ZERO
               MOVE W80-PRC-DIG(1)   TO W80-SUB
               MOVE W80-5P(W80-SUB)  TO W80-WEIGHT(1).
           IF W80-PRC-DIG(2) > ZERO
               MOVE W80-PRC-DIG(2)   TO W80-SUB
               MOVE W80-2N(W80-SUB)  TO W80-WEIGHT(2).
           IF W80-PRC-DIG(3) > ZERO
               MOVE W80-PRC-DIG(3)   TO W80-SUB
               MOVE W80-5N(W80-SUB)  TO W80-WEIGHT(3).
           IF W80-PRC-DIG(4) > ZERO
               MOVE W80-PRC-DIG(4)   TO W80-SUB
               MOVE W80-5P(W80-SUB)  TO W80-WEIGHT(4).
           IF W80-PRC-DIG(5) > ZERO
               MOVE W80-PRC-DIG(5)   TO W80-SUB
               MOVE W80-2N(W80-SUB)  TO W80-WEIGHT(5).
             ADD W80-WEIGHT(1) W80-WEIGHT(2) W80-WEIGHT(3) W80-WEIGHT(4) W80-WEIGHT(5) GIVING W80-RESULT.
           IF W80-UNIT > ZERO
               SUBTRACT W80-UNIT FROM 10 GIVING W80-SUB
           ELSE
               MOVE ZERO             TO W80-SUB.
           IF W80-SUB = ZERO
               MOVE ZERO             TO W80-CD
               GO TO BA110-PRC-CD-FOUND.
             MOVE 1                  TO W80-CD.
           
       BA105-FIND-PRC-CD.
           IF W80-SUB = W80-5N(W80-CD)
               GO TO BA110-PRC-CD-FOUND.
             ADD 1                   TO W80-CD.
           IF W80-CD < 9
               GO TO BA105-FIND-PRC-CD.
           
       BA110-PRC-CD-FOUND.
             MOVE W80-CD             TO W80-PRC-CD.
      *
      *    ****    C A L C U L A T E   T H E   B A R - C O D E
      *     C H E C K   D I G I T
      *
       BA115-CALC-CHECK-DIGIT.
             ADD W80-BAR-CODE-CH(1) W80-BAR-CODE-CH(3) W80-BAR-CODE-CH(5)
                 W80-BAR-CODE-CH(7) W80-BAR-CODE-CH(9) W80-BAR-CODE-CH(11)
                                     GIVING W80-BAR-CODE-RESULT-1.
             ADD W80-BAR-CODE-CH(2) W80-BAR-CODE-CH(4) W80-BAR-CODE-CH(6)
                 W80-BAR-CODE-CH(8) W80-BAR-CODE-CH(10) W80-BAR-CODE-CH(12)
                                     GIVING W80-BAR-CODE-RESULT-2.
             MULTIPLY 3              BY W80-BAR-CODE-RESULT-2.
             ADD W80-BAR-CODE-RESULT-1 W80-BAR-CODE-RESULT-2 GIVING W80-BAR-CODE-RESULT-3.
           IF W80-BAR-CODE-UNIT > ZERO
               SUBTRACT W80-BAR-CODE-UNIT FROM 10 GIVING W80-BAR-CODE-CHK-DIG
           ELSE
               MOVE ZERO             TO W80-BAR-CODE-CHK-DIG.
             MOVE W80-BAR-CODE       TO W10-XREF.

       BA199.
             EXIT.

      *
      *    ****  A M E N D   E X S I S T I N G   S T O C K
      *
       BC000        SECTION 5.
       BC000-AMEND.
             PERFORM ERASE-SCREEN.
             DISPLAY S04B.
             DISPLAY S04-N.
             MOVE ZERO               TO WS-INDB.
           IF WS-USE-ITM = "Y"
               DISPLAY "/ITM" AT 0612 "/" AT 0635.
           IF NOT(W10-STCK = "N")
               MOVE SPACE            TO W10-EXT-ITEM
           ELSE
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
       BC005.
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
                              GO TO BC999
                 WHEN F1-KEY  PERFORM HELP-ROUTINE
                              GO TO BC005
                 WHEN F2-KEY  MOVE "I"    TO W10-ETYPE
                 WHEN F3-KEY  MOVE "A"    TO W10-ETYPE
                 WHEN F4-KEY  MOVE "D"    TO W10-ETYPE
                 WHEN F5-KEY  MOVE "2"    TO W10-ETYPE
                 WHEN F6-KEY  MOVE "X"    TO W10-ETYPE
                 WHEN F9-KEY  MOVE "W"    TO W10-ETYPE
                 WHEN OTHER   PERFORM AA900-ALARM
                              GO TO BC005
               END-EVALUATE
               PERFORM AA50-LOOKUP
      *    
      *                      E X T E N D E D   C O D E
      *    
      *      Is an 18 character stock/inventory item code being used?  
      *    
               IF WS-EXT-STK = "Y"
                   DISPLAY S05-EXT
               ELSE
      *   
      *     Is an ALPHA-NUMERIC code being used?
      *    
               IF (W10-STCK = "A") AND (WS-USE-ITM = "Y")
      *    
      *     A 3 character extension code is being used in addition to 
      *     the 14 character stock/inventory item code.              
      *    
                   DISPLAY S05-ITM
               ELSE
      *    
      *     A 1 TO 14 character Alpha-Numeric or Numeric code is being 
      *     used.               
      *    
                   DISPLAY S05
               END-IF
               IF W10-EXT-ITEM = SPACES
                   GO TO BC005
               ELSE
                   DISPLAY S05
               END-IF.
             PERFORM CLEAR-L50.
           IF W10-EXT-ITEM = SPACE
               GO TO BC999.

       BC05.
      *
      *     Read Stock record using Item Code and Lock record, if read
      *     is successful.
      *
             MOVE "I"                TO W10-INSTR.
             MOVE "L"                TO W10-INSTR2.
             PERFORM CA000.
      *    
      *                      E X T E N D E D   C O D E
      *    
           IF WS-EXT-STK = "Y"
               DISPLAY S05-EXT
           ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
           IF (W10-STCK = "A") AND (WS-USE-ITM = "Y")
               DISPLAY S05-ITM
           ELSE
               DISPLAY S05.
           IF WS-F-ERROR = 22
               MOVE "Z"              TO W10-ETYPE
               PERFORM AA50-LOOKUP
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
               IF WS-EXT-STK = "Y"
                   DISPLAY S05-EXT
               ELSE    
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
               IF (W10-STCK = "A") AND (WS-USE-ITM = "Y")
                   DISPLAY S05-ITM
               ELSE
                   DISPLAY S05
               END-IF
               IF W10-EXT-ITEM = SPACES
                   GO TO BC005
               ELSE
                   GO TO BC05.

       BC012-DET.
      *      MOVE STK-CODE           TO W10-EXT-ITEM.
      *      MOVE STK-XREF           TO W10-EXT-CODE2.
      *      MOVE STK-BIN            TO W10-BIN.
      *      MOVE STK-DESC           TO W10-DESC.
      *      MOVE STK-DESC2          TO W10-DESC2.
      *      MOVE STK-TAX            TO W10-TAX.
      *      MOVE STK-IND            TO W10-IND.
      *      MOVE STK-QUANT          TO W10-QUANT.
      *      MOVE STK-PRC            TO W10-PRC.
      *      MOVE STK-LEVEL          TO W10-REORD.
      *      MOVE STK-LTIME          TO W10-LTIME.
      *      MOVE STK-COST           TO W10-COST.
      *      MOVE STK-LDG            TO W10-LEDG.
      *      MOVE STK-GUAR           TO W10-GUAR.
      *      MOVE STK-ORD            TO W10-ORDER.
      *      MOVE STK-DTE            TO W22-DTE2.
      *      PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT.
      *      MOVE W22-DTE3           TO W10-ODTE.
      *      MOVE STK-PUR            TO W22-DTE2.
      *      PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT.
      *      MOVE W22-DTE3           TO W10-PURCH.
      *
      *    If the average cost in the record is zero, make it equal
      *    to the current cost.
      *
      *    IF STK-IND < 2
      *        IF STK-AVGE = ZERO
      *            MOVE STK-COST     TO STK-AVGE.
      *      MOVE STK-AVGE           TO W10-AVRG.
      *      MOVE STK-ALT-DISC       TO W10-ALT-DISC.
      *      MOVE STK-ACODE          TO W10-ACODE.
      *      MOVE STK-USE-PACKS      TO W10-USE-PACKS.
      *      MOVE STK-EXEMPT         TO W10-EXEMPT.
      *      MOVE STK-FACT           TO W10-FACT.
      *
      *    ****    U N I T S   A N D   M A R K U P S
      *
             MOVE 1                  TO WS-S1.
             PERFORM BC45.
      *
      *    ****    P R I C E S  -   I N C L U S I V E   A N D
      *                             E X C L U S I V E
      *            S E E   S T P R E A D
      *
             DISPLAY S06C.

       BC015-AMEND-DET.
           IF LS-OK = "N"
               MOVE ZERO             TO W10-MARKUP W10-COST W10-CMARKUP W10-AVGE W10-WMARKUP.
             PERFORM AA400.
             DISPLAY S-LEDG.

       BC16-AMEND-DET.
             ACCEPT S06A.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY MOVE SPACES TO W10-EXT-ITEM
                              UNLOCK STOCK
                              GO TO BC999
                 WHEN F1-KEY  PERFORM HELP-ROUTINE
                              GO TO BC16-AMEND-DET
                 WHEN F6-KEY  PERFORM SAVE-SCREEN
                              PERFORM DEPART-VIEW
                              PERFORM RESTORE-SCREEN
                              GO TO BC015-AMEND-DET
                 WHEN OTHER   PERFORM AA900-ALARM
                              GO TO BC16-AMEND-DET
               END-EVALUATE
               GO TO BC16-AMEND-DET.
      *
      *    If User not authorised, reset the following fields to the 
      *    original contents at time of reading the record.
      *
           IF LS-OK = "N"
               MOVE STK-MARKUP       TO W10-MARKUP
               MOVE STK-CMARKUP      TO W10-CMARKUP
               MOVE STK-WMARKUP      TO W10-WMARKUP
               MOVE STK-GUAR         TO W10-GUAR
               MOVE STK-COST         TO W10-COST
               MOVE STK-SELL         TO W10-SELL
               MOVE STK-CASH         TO W10-CASH
               MOVE STK-WSALE        TO W10-WSALE
               MOVE STK-QUANT        TO W10-QUANT
               MOVE STK-AVGE         TO W10-AVGE.
      *
      *    If the quantity has been changed a password is required.
      *
           IF NOT(STK-QUANT = W10-QUANT)
               PERFORM SAVE-SCREEN-2
               DISPLAY S20
               MOVE 12               TO SHADE-ROW
               MOVE 22               TO SHADE-COL
               MOVE 38               TO SHADE-WIDTH
               MOVE 2                TO SHADE-LINES
               PERFORM SCREEN-SHADOW
               ACCEPT S20A
               MOVE 6                TO WS-LENGTH
               CALL "CBL_TOUPPER" USING WS-QPASSWORD BY VALUE WS-LENGTH RETURNING WS-STATUS
               MOVE 1                TO WS-LENGTH
               MOVE 5                TO WS-PARKEY
               PERFORM READ-PARAM THRU READ-PARAM-EXIT
               MOVE PAR-PW  TO W85-PASS
               IF NOT(WS-QPASSWORD = W85-SUPER(1) OR W85-SUPER(7) OR "YTITNQ")
                   MOVE STK-QUANT    TO W10-QUANT
               END-IF
               PERFORM RESTORE-SCREEN-2.
             PERFORM AA400.
             MOVE ZERO               TO W05-RATE.
           IF W10-TAX > 0
               MOVE W05-VAT(W10-TAX) TO W05-RATE.
             COMPUTE W10-VCASH  ROUNDED = W10-CASH  + (W10-CASH * W05-RTE).
             COMPUTE W10-VWSALE ROUNDED = W10-WSALE + (W10-WSALE * W05-RTE).
             COMPUTE W10-VSELL  ROUNDED = W10-SELL  + (W10-SELL * W05-RTE).
             PERFORM BC21D.
             DISPLAY S06C.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO BC015-AMEND-DET.
           IF ((W10-IND < 3) OR (W10-IND = 8 OR 9)) AND (W10-DESC = SPACES)
               MOVE "Invalid description (required)" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BC015-AMEND-DET.
             MOVE W10-LEDG  TO DPT-CODE.
           IF (W10-LEDG = SPACES OR "0000" OR "XXXX" OR "YYYY") OR (DPT-SCDE = "VAT") OR (WS-NODEP = W10-DEPT)
               MOVE "Invalid ledger code" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BC015-AMEND-DET.
           IF W10-BIN = SPACES
               MOVE "Invalid bin number (required)" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BC015-AMEND-DET.
           IF NOT(W10-ALT-DISC = "Y")
               MOVE "N"              TO W10-ALT-DISC.
           IF W10-TAX > 6
               MOVE "Invalid VAT code (1 - 6)" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BC015-AMEND-DET.
      *
      *    ****    TEMPORARY PATCH - ONLY 1 RATE APPLICABLE
      *
           IF W10-TAX > 1
               MOVE 1                TO W10-TAX.
           IF W10-ODTE = ZERO
      *
      *      Zero to date in Stock record in working storage
      *
               MOVE ZERO             TO W10-DTE
               GO TO BC18.
             MOVE W10-ODTE           TO W22-DATE.
             PERFORM DATE-CHECK THRU CHECK-DATE-EXIT.
           IF NOT(W22-ERROR = 0)
               EVALUATE W22-ERROR
                 WHEN 1  MOVE "Invalid Order DATE"  TO WS-ERR-MES
                 WHEN 2  MOVE "Invalid Order DAY"   TO WS-ERR-MES
                 WHEN 3  MOVE "Invalid Order MONTH" TO WS-ERR-MES
               END-EVALUATE
               PERFORM ERROR-MESSAGE
               GO TO BC015-AMEND-DET.
      *
      *      Date as CCYYMMDD to Stock record in working storage
      *
             MOVE W22-DTE1           TO W10-DTE.

       BC18.
           IF W10-PURCH = ZERO
      *
      *      Zero to date in Stock record in working storage
      *
               MOVE ZERO             TO W10-PUR
               GO TO BC020-AMEND.
             MOVE W10-PURCH          TO W22-DATE.
             PERFORM DATE-CHECK THRU CHECK-DATE-EXIT.
           IF NOT(W22-ERROR = 0)
               EVALUATE W22-ERROR
                 WHEN 1  MOVE "Invalid Received DATE"  TO WS-ERR-MES
                 WHEN 2  MOVE "Invalid Received DAY"   TO WS-ERR-MES
                 WHEN 3  MOVE "Invalid Received MONTH" TO WS-ERR-MES
               END-EVALUATE
               PERFORM ERROR-MESSAGE
               GO TO BC015-AMEND-DET.
      *
      *      Date as CCYYMMDD to Stock record in working storage
      *
             MOVE W22-DTE1           TO W10-PUR.

       BC020-AMEND.
             MOVE W10-EXT-ITEM       TO WS-ITM.
           IF W10-ART11 = "N"
               GO TO BC20-CHECK.

       BC20-ART11.
             PERFORM SAVE-SCREEN-3.
             MOVE 12                 TO SHADE-ROW.
             MOVE 17                 TO SHADE-COL.
             MOVE 49                 TO SHADE-WIDTH.
             MOVE 2                  TO SHADE-LINES.
             DISPLAY S-ART11.
             PERFORM SCREEN-SHADOW.

       BC20-EXEMPT.
             ACCEPT W10-EXEMPT AT 1263 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT UPDATE AUTO.
             CALL "CBL_TOUPPER" USING W10-EXEMPT BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(W10-EXEMPT = "N" OR "Y")
               GO TO BC20-EXEMPT.
             PERFORM RESTORE-SCREEN-3.

       BC20-CHECK.
           IF LS-OK = "N"
               GO TO BC20A.
             MOVE 2                  TO WS-INSTR.
           IF W10-TAX = ZERO
               MOVE "'C'ontinue or 'R'ekey  [C]" TO WS-OPT-MES
           ELSE
               MOVE "'C'ontinue, amend 'I'nclusive or 'R'ekey  [C]" TO WS-OPT-MES.
             MOVE "C"                TO WS-OPTION.
             MOVE 16                 TO SLIN.
             PERFORM OPT-MESSAGE.
      *      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
           IF WS-OPTION = "R"
               GO TO BC015-AMEND-DET.
           IF NOT(W10-TAX = ZERO) AND (WS-OPTION = "I")
               ACCEPT S06C
               COMPUTE W10-CASH = (W10-VCASH / (100 + W05-RATE)) * 100
               COMPUTE W10-WSALE ROUNDED = (W10-VWSALE / (100 + W05-RATE)) * 100
               COMPUTE W10-SELL  ROUNDED = (W10-VSELL  / (100 + W05-RATE)) * 100
               DISPLAY S06E
               GO TO BC20-CHECK.

       BC20A.
             PERFORM AY60 THRU AY999.
             MOVE 0                  TO WS-ACTION.
             PERFORM AY05 THRU AY59.
             MOVE ZERO               TO W25-TOTAL W25-VALUE.
           IF STK-IND = 0 OR 1
               MULTIPLY STK-QUANT BY STK-AVGE GIVING W25-VALUE.
           IF W10-IND = 0 OR 8 OR 9
               MULTIPLY W10-QUANT BY W10-AVGE GIVING W25-TOTAL.
             SUBTRACT W25-VALUE FROM W25-TOTAL GIVING W25-ADJUST.
           IF NOT(W25-ADJUST = ZERO)
               MOVE W10-LEDG         TO DPT-CODE
               PERFORM READ-DEPART THRU READ-DEPART-EXIT
               PERFORM AY38 THRU AY59
               ADD W25-ADJUST        TO DPT-ADJ-DAY DPT-ADJ-MTD DPT-ADJ-YTD STK-ADJMTD STK-ADJYTD
               PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT
               IF WX-INT = "Y"
                   MOVE W12-T-YMD    TO WX-DATE
                   MOVE 28           TO WX-TYPE
                   MOVE W10-LEDG     TO WX-LEDG
                   MOVE DPT-PUR-GL   TO WX-AC
                   MOVE "Stock Amended" TO WX-NAR
                   MOVE W25-ADJUST   TO WX-VALUE
                   PERFORM GL-TRF-UPDATE
                   MOVE 27           TO WX-TYPE
                   MOVE WX-ADJGL     TO WX-AC
                   MULTIPLY -1       BY WX-VALUE
                   PERFORM GL-TRF-UPDATE.
           IF WS-CARDEX = "Y"
               IF NOT(W25-ADJUST = ZERO) OR (STK-QUANT NOT = W10-QUANT)
                   INITIALIZE STX-REC1
                   MOVE W10-EXT-ITEM TO STX-EXT-CODE
                   MOVE W12-T-YMD    TO STX-DTE
                   MOVE ZERO         TO STX-SEQ
                   MOVE LS0-NO       TO STX-USER
                   MOVE "AMEND"      TO STX-PROG
                   MOVE LS-USER      TO STX-WS
                   MOVE "Quantity amended" TO STX-REMARKS
                   MOVE "C"          TO STX-TYPE
                   SUBTRACT STK-QUANT FROM W10-QUANT GIVING STX-QNT
                   MOVE W25-ADJUST   TO STX-COST
                   COMPUTE STX-SELL ROUNDED = (W25-ADJUST + ((W25-ADJUST * W10-MARKUP) / 100))
                   PERFORM WRITE-STKDEX THRU WRITE-STKDEX-EXIT
                   MOVE 1            TO WS-ACTION
                   PERFORM AY46 THRU AY59.

       BC21.
      *      MOVE W10-EXT-ITEM       TO STK-CODE WS-ITM.
      *      MOVE W10-BIN            TO STK-BIN.
           IF W10-DESC = SPACES
               GO TO BC21B.
             MOVE 30                 TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W10-DESC BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.

       BC21A.
             MOVE W10-DESC           TO W50-DATA.
           IF W50-1 = SPACE
               MOVE W50-29           TO W10-DESC
               GO TO BC21A.

       BC21B.
           IF NOT(W10-DESC = STK-DESC)
               MOVE 1                TO WS-INDB.
      *      MOVE W10-DESC           TO STK-DESC.
           IF W10-DESC2 = SPACES
               GO TO BC21BC.
             MOVE 30                 TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W10-DESC2 BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.

       BC21BA.
             MOVE W10-DESC2          TO W50-DATA.
           IF W50-1 = SPACE
               MOVE W50-29           TO W10-DESC2
               GO TO BC21BA.

       BC21BC.
           IF NOT(W10-DESC2 = STK-DESC2)
               MOVE 1                TO WS-INDB.
      *      MOVE W10-DESC2          TO STK-DESC2.
             MOVE 10                 TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W10-ACODE BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 18                 TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W10-EXT-CODE2 BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
      *      MOVE W10-TAX            TO STK-TAX.
      *
      *    Test for controlled and priced items
      *
           IF (W10-IND < 3) OR (W10-IND = 8 OR 9)
               IF NOT(W10-MARKUP > ZERO)
                   PERFORM BC50.
      *      MOVE W10-MARKUP         TO STK-MARKUP.
      *      MOVE W10-QUANT          TO STK-QUANT.
      *      MOVE W10-PER            TO STK-PER.
      *      MOVE W10-PRC            TO STK-PRC.
      *      MOVE W10-REORD          TO STK-LEVEL.
      *      MOVE W10-LTIME          TO STK-LTIME.
      *      MOVE W10-COST           TO W10-CCOST.
      *      MOVE W10-SELL           TO W10-CSELL.
           IF W10-COST = ZERO
               IF NOT(W10-SELL = ZERO)
                   IF NOT(W10-MARKUP = ZERO)
                       COMPUTE W10-CCOST ROUNDED = (W10-CSELL / (100 + W10-MARKUP) * 100).
           IF W10-SELL = ZERO
               IF NOT(W10-COST = ZERO)
                   IF NOT(W10-MARKUP = ZERO)
                       COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST * W10-MARKUP) / 100)).
           IF W10-CSELL = ZERO
               MOVE W10-CCOST        TO W10-CSELL.
      *      MOVE W10-CCOST          TO W10-COST.
      *      MOVE W10-CSELL          TO W10-SELL.
      *      MOVE W10-COST           TO STK-COST.
      *      MOVE W10-LEDG           TO STK-LDG.
      *      MOVE W10-GUAR           TO STK-GUAR.
      *      MOVE W10-ORDER          TO STK-ORD.
      *      MOVE W22-DTE2           TO STK-DTE.
      *      MOVE W10-PURCH          TO W22-DTE3.
      *      PERFORM SWITCH-DATE-TO-CYMD THRU SWITCH-DATE-EXIT.
      *      MOVE W22-DTE2           TO STK-PUR.
           IF W10-AVGE = ZERO
               MOVE W10-COST         TO W10-AVGE.
      *      MOVE W10-ALT-DISC       TO STK-ALT-DISC.
      *      MOVE W10-IND            TO STK-IND.
      *
      *    Test for controlled and priced items
      *
           IF (W10-IND < 3) OR (W10-IND = 8 OR 9)
               IF NOT(W10-CMARKUP > ZERO)
                   PERFORM BC52.
           IF W10-IND = 2
               MOVE "-LAB-"          TO W10-SUPP.
           IF (W10-CMARKUP = ZERO) AND (W10-CASH = ZERO)
               MOVE W10-MARKUP       TO W10-CMARKUP
               MOVE W10-SELL         TO W10-CASH
               GO TO BC21C.
           IF W10-CMARKUP = ZERO
               MOVE W10-CASH         TO W10-CSELL
               COMPUTE W10-CMARKUP ROUNDED = (((W10-CSELL - W10-CCOST) / W10-CCOST) * 100).
           IF W10-CASH = ZERO
               COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST * W10-CMARKUP) / 100))
               MOVE W10-CSELL        TO W10-CASH.

       BC21C.
      *      MOVE W10-CMARKUP        TO STK-CMARKUP.
      *
      *    Test for controlled and priced items
      *
           IF (W10-IND < 3) OR (W10-IND = 8 OR 9)
               IF W10-WMARKUP NOT > ZERO
                   PERFORM BC54.
           IF (W10-WMARKUP = ZERO) AND (W10-WSALE = ZERO)
               MOVE W10-CMARKUP      TO W10-WMARKUP
               MOVE W10-CASH         TO W10-WSALE
               GO TO BC21D.
           IF W10-WMARKUP = ZERO
               MOVE W10-WSALE        TO W10-CSELL
               COMPUTE W10-WMARKUP ROUNDED = (((W10-CSELL - W10-CCOST) / W10-CCOST) * 100).
           IF W10-WSALE = ZERO
               COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST * W10-WMARKUP) / 100))
               MOVE W10-CSELL        TO W10-WSALE.

       BC21D.
           IF W05-ROUND = "E"
               MOVE W10-SELL         TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-SELL
               MOVE W10-WSALE        TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-WSALE
               MOVE W10-CASH         TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-CASH.
           IF NOT(STK-TAX = ZERO)
               MOVE W05-VAT(STK-TAX) TO W05-RATE
               COMPUTE W10-VSELL  ROUNDED = W10-SELL  + (W10-SELL * W05-RTE)
               COMPUTE W10-VWSALE ROUNDED = W10-WSALE + (W10-WSALE * W05-RTE)
               COMPUTE W10-VCASH  ROUNDED = W10-CASH  + (W10-CASH * W05-RTE)
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
               MOVE ZERO             TO W05-RATE.
           IF W10-TAX > 0
               MOVE W05-VAT(W10-TAX) TO W05-RATE.
             COMPUTE W10-VCASH  ROUNDED = W10-CASH  + (W10-CASH  * W05-RTE).
             COMPUTE W10-VWSALE ROUNDED = W10-WSALE + (W10-WSALE * W05-RTE).
             COMPUTE W10-VSELL  ROUNDED = W10-SELL  + (W10-SELL  * W05-RTE).
 
       BC21E.
      *      MOVE W10-WMARKUP        TO STK-WMARKUP.
      *      MOVE W10-SELL           TO STK-SELL.
           IF NOT(W10-CASH = STK-CASH)
      *        MOVE W10-CASH         TO STK-CASH
               PERFORM BA000.
      *      MOVE W10-WSALE          TO STK-WSALE.
      *      MOVE W10-EXT-CODE2      TO STK-XREF.
      *      MOVE W10-ACODE          TO STK-ACODE.
      *      MOVE W10-EXEMPT         TO STK-EXEMPT.
           IF W10-IND = 2
               MOVE ZERO             TO W10-COST W10-MARKUP W10-CMARKUP W10-WMARKUP.
           IF (LS0-LEV > 3) AND (W10-IND < 2)
               MOVE 2                TO WS-S1
               PERFORM BC35.
           IF WS-CON-FACT = "Y"
               PERFORM BD000.
      *      MOVE W10-FACT           TO STK-FACT.
             MOVE W10-STK-RECORD     TO STK-RECORD1 W15-RECORD1.
             PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
             SUBTRACT W25-VALUE      FROM STK-TOTAL.
             ADD W25-TOTAL           TO STK-TOTAL.
             PERFORM AY70 THRU AY999.
           IF LS-OK = "Y"
               DISPLAY S06E
               DISPLAY S06C.
             MOVE "A"                TO W10-INSTR.
             CALL "STP\STP007" USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS W10-STOCK.               

       BC24C.
             MOVE STK-CODE           TO PRT-EXT-ITEM.
             PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
           IF NOT(WS-F-ERROR = ZERO)
               GO TO BC30.
             MOVE 1                  TO WS-S6.

       BC25.
             MOVE "Must production details be updated Y/N  [N]" TO WS-OPT-MES.
             MOVE "N"                TO WS-OPTION.
             MOVE 1                  TO WS-INSTR.

       BC26.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "N"
               GO TO BC30.

       BC27.
            MOVE PRT-EXT-CODE(WS-S6) TO W11-EXT-CODE(WS-S6).
            MOVE SPACES              TO W11-DESC(WS-S6).
            MOVE PRT-QUANT(WS-S6)    TO W11-QUANT(WS-S6).
           IF WS-S6 < 30
               ADD 1                 TO WS-S6
               GO TO BC27.
             MOVE PRT-COMMENT        TO W10-COMMENT.
             MOVE 1                  TO WS-AMEND.
             MOVE ZERO               TO W10-MCOST.
             PERFORM BC63 THRU BC99.
           IF WS-INDB = 1
               MOVE "A"              TO LS-INSTR
               CALL "STP\STPALT" USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY W02-STKALT W02-RECOVER WS-RECOVER WS-TRANS STK-CODE STK-DESC STK-DESC2 WS-ERROR.
               IF WS-ERROR = 9
                   MOVE "Error with Description WORD index update" TO WS-ERR-MES
                   PERFORM ERROR-MESSAGE
                   STOP RUN.
               CANCEL "STP\STPALT".
      *
      *    ****    C H E C K   F O R   C A S E S   O R   P A C K S
      *            A N D   A L L O W   A M E N D M E N T S
      *
       BC30.
           IF (LS0-LEV > 3) AND (W10-IND < 2)
               IF (WS-USE-PACKS = "Y") OR (WS-USE-CASES = "Y")
                   PERFORM CLEAR-L50
                   PERFORM SAVE-SCREEN
                   CALL "STP\STPPACKS" USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS W10-STOCK
                       ON EXCEPTION
                           MOVE "Program- STPPACKS not on disk, press ANY key" TO WS-ERR-MES
                           PERFORM ERROR-MESSAGE
                           GO TO BC22
                   END-CALL
                   CANCEL "STP\STPPACKS"
                   PERFORM RESTORE-SCREEN
                   GO TO BC32.
             GO TO BC000-AMEND.

       BC32.
             MOVE W15-RECORD1        TO STK-RECORD1.
      *
      *    ****   R E - S T A R T   R E C O V E R Y   L O G I N G
      *
             PERFORM AY60 THRU AY999.
             MOVE 0                  TO WS-ACTION.
             PERFORM AY05 THRU AY59.
             MOVE W10-USE-CASES      TO STK-USE-CASES.
             MOVE W10-CASES          TO STK-CASES.
             MOVE W10-USE-PACKS      TO STK-USE-PACKS.
             MOVE 2                  TO WS-S1.
      *
      *    ****   P A C K   D E T A I L S
      *
       BC35.
             MOVE W10-UNT(WS-S1)     TO STK-UNT(WS-S1).
             MOVE W10-PR(WS-S1)      TO STK-PR(WS-S1).
             MOVE W10-MKUP(WS-S1)    TO STK-MKUP(WS-S1).
             MOVE W10-CMKUP(WS-S1)   TO STK-CMKUP(WS-S1).
             MOVE W10-WMKUP(WS-S1)   TO STK-WMKUP(WS-S1).
             MOVE W10-SL(WS-S1)      TO STK-SL(WS-S1).
             MOVE W10-CSH(WS-S1)     TO STK-CSH(WS-S1).
             MOVE W10-WSL(WS-S1)     TO STK-WSL(WS-S1).
           IF WS-S1 < 4
               ADD 1                 TO WS-S1
               GO TO BC35.

       BC40.
             PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
             PERFORM AY70 THRU AY999.
             GO TO BC000-AMEND.

       BC45.
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
               GO TO BC45.
      *
      *    ****  D E F A U L T   M A R K U P
      *
       BC50.
           IF W10-SELL > ZERO AND W10-COST > ZERO
               MOVE W10-SELL         TO W10-CSELL
               MOVE W10-COST         TO W10-CCOST
               COMPUTE W10-MARKUP ROUNDED = (((W10-CSELL - W10-CCOST) / W10-COST) * 100)
           ELSE
               MOVE DPT-MARKUP       TO W10-MARKUP.

       BC51.
             EXIT.
      *
      *    ****  D E F A U L T   C A S H   M A R K U P
      *
       BC52.
           IF W10-CASH > ZERO AND W10-COST > ZERO
               MOVE W10-CASH         TO W10-CSELL
               MOVE W10-COST         TO W10-CCOST
               COMPUTE W10-CMARKUP = (((W10-CSELL - W10-CCOST) / W10-COST) * 100)
           ELSE
               MOVE DPT-CMRKUP       TO W10-CMARKUP.

       BC53.
             EXIT.
      *
      *    ****  D E F A U L T   W H O L E S A L E   M A R K U P
      *
       BC54.
           IF W10-WSALE > ZERO AND W10-COST > ZERO
               MOVE W10-WSALE        TO W10-CSELL
               MOVE W10-COST         TO W10-CCOST
               COMPUTE W10-WMARKUP = (((W10-CSELL - W10-CCOST) / W10-COST) * 100)
           ELSE
               MOVE DPT-WMRKUP       TO W10-WMARKUP.

       BC55.
             EXIT.

       BC63.
             MOVE 0501               TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
             DISPLAY S32.

       BC64.
             ACCEPT W10-COMMENT AT 0618 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO BC64.

       BC65.
             MOVE 1                  TO WS-S6 WS-S7.
             MOVE 8                  TO LIN.

       BC70.
             MOVE SPACES             TO W10-EXT-CODE2.
             MOVE ZERO               TO W10-MQNT.
           IF W11-EXT-CODE(WS-S6) = SPACES
               GO TO BC75.
             MOVE W11-EXT-CODE(WS-S6) TO W10-EXT-CODE2.
             MOVE W11-QUANT(WS-S6)   TO W10-MQNT.

       BC75.
             PERFORM AA300.
      *    *************************************************************
      *                     G E T   S T O C K   C O D E
      *    *************************************************************
      *    
      *      Is an 18 character stock/inventory item code being used?  
      *    
           IF WS-EXT-STK = "Y"
               ACCEPT S32B AT LINE LIN
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
                   ACCEPT S32A AT LINE LIN
               ELSE
                   ACCEPT S32C AT LINE LIN
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
                 WHEN 3         ACCEPT S32-3 AT LINE LIN
      *                     4 Digit numeric Stock/Inventory item code
                 WHEN 4         ACCEPT S32-4 AT LINE LIN
      *                     5 Digit numeric Stock/Inventory item code
                 WHEN 5         ACCEPT S32-5 AT LINE LIN
      *                     6 Digit numeric Stock/Inventory item code
                 WHEN 6         ACCEPT S32-6 AT LINE LIN
      *                     7 Digit numeric Stock/Inventory item code
                 WHEN 7         ACCEPT S32-7 AT LINE LIN
      *                     8 Digit numeric Stock/Inventory item code
                 WHEN 8         ACCEPT S32-8 AT LINE LIN
      *                     9 Digit numeric Stock/Inventory item code
                 WHEN 9         ACCEPT S32-9 AT LINE LIN
      *                    10 Digit numeric Stock/Inventory item code
                 WHEN 10        ACCEPT S32-10 AT LINE LIN
      *                    11 Digit numeric Stock/Inventory item code
                 WHEN 11        ACCEPT S32-11 AT LINE LIN
      *                    12 Digit numeric Stock/Inventory item code
                 WHEN 12        ACCEPT S32-12 AT LINE LIN
      *                    13 Digit numeric Stock/Inventory item code
                 WHEN 13        ACCEPT S32-13 AT LINE LIN
      *                    14 Digit numeric Stock/Inventory item code
                 WHEN 14        ACCEPT S32-14 AT LINE LIN
               END-EVALUATE.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY MOVE SPACES  TO W10-EXT-CODE2
                              GO TO BC75A
                 WHEN F1-KEY  PERFORM HELP-ROUTINE
                              GO TO BC75
                 WHEN F2-KEY  MOVE "I"     TO W10-ETYPE
                 WHEN F3-KEY  MOVE "A"     TO W10-ETYPE
                 WHEN F4-KEY  MOVE "D"     TO W10-ETYPE
                 WHEN F5-KEY  MOVE "2"     TO W10-ETYPE
                 WHEN F6-KEY  MOVE "X"     TO W10-ETYPE
                 WHEN F9-KEY  MOVE "W"     TO W10-ETYPE
                 WHEN OTHER   PERFORM AA900-ALARM
                              GO TO BC75
               END-EVALUATE
               PERFORM AA50-LOOKUP
               MOVE W10-EXT-ITEM     TO W10-EXT-CODE2
      *    
      *     Is an 18 character stock/inventory item code being used?  
      *    
               IF WS-EXT-STK = "Y"
                   DISPLAY S32B AT LINE LIN
               ELSE
      *   
      *     Is an ALPHA-NUMERIC code being used?
      *    
               IF W10-STCK = "A"
      *    
      *     Is a 3 character extension code being used in addition to 
      *     the 14 character stock/inventory item code?               
      *    
                   IF WS-USE-ITM = "Y"
                       DISPLAY S32A AT LINE LIN
                   ELSE
                       DISPLAY S32C AT LINE LIN
                   END-IF
               ELSE
      *    *************************************************************
      *                         NUMERIC CODES APPLY 
      *    *************************************************************
                   DISPLAY S32C AT LINE LIN
               END-IF
               END-IF    
               IF W10-EXT-CODE2 = SPACES
                   GO TO BC75.
             MOVE 18                 TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W10-EXT-CODE2 BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.

       BC75A.
             PERFORM CLEAR-L50.
          IF W10-EXT-CODE2 = SPACE
               IF WS-AMEND = 1
                   GO TO BC95
               ELSE
                   GO TO BC85.
             MOVE W10-EXT-CODE2      TO W10-EXT-ITEM.

       BC76.
      *
      *     Read Stock record using Item Code.
      *
             MOVE "I"                 TO W10-INSTR.
             PERFORM CA000.
           IF WS-F-ERROR = 22
               MOVE "Z"              TO W10-ETYPE
               PERFORM AA50-LOOKUP
               MOVE W10-EXT-ITEM     TO W10-EXT-CODE2
               IF WS-EXT-STK = "Y"
                   DISPLAY S32B AT LINE LIN
               ELSE
               IF WS-USE-ITM = "Y"
                   DISPLAY S32A AT LINE LIN
               ELSE
                   DISPLAY S32C AT LINE LIN
               END-IF
           END-IF
           IF W10-EXT-CODE2 = SPACES
               GO TO BC75
           ELSE
               GO TO BC76.
             DISPLAY (LIN, 28) STK-DESC WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO BC75.

       BC80.
             MOVE 64                 TO COL.
             ACCEPT S38 AT LINE LIN COLUMN COL.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO BC80.
           IF W10-MQNT = ZERO
               GO TO BC85.
             MOVE W10-EXT-CODE2      TO W11-EXT-CODE(WS-S6).
             MOVE STK-DESC           TO W11-DESC(WS-S6).
             MOVE W10-MQNT           TO W11-QUANT(WS-S6).
             COMPUTE W10-MCOST = W10-MCOST + (W10-MQNT * STK-AVGE).
           IF WS-S6 < 30
               ADD 1                 TO WS-S6
           ELSE
               GO TO BC85.
           IF LIN < 22
               ADD 1                 TO LIN
               GO TO BC70.
             ADD 1                   TO WS-S7.
             MOVE 8                  TO LIN.
             MOVE WS-S7              TO WS-S8.

       BC81.
             MOVE W11-EXT-CODE(WS-S8) TO W10-EXT-CODE2.
           IF WS-EXT-STK = "Y"
               DISPLAY S32B AT LINE LIN
           ELSE
           IF WS-USE-ITM = "Y"
               DISPLAY S32A AT LINE LIN
           ELSE
               DISPLAY S32C AT LINE LIN.
             MOVE W11-DESC(WS-S8)    TO W10-DESC.
             DISPLAY (LIN, 28) W10-DESC WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE W11-QUANT(WS-S8)   TO W10-MQNT.
             DISPLAY S38 AT LINE LIN COLUMN COL.
           IF LIN < 22
               ADD 1                 TO LIN WS-S8
               GO TO BC81.
            DISPLAY WS-BLNK78 AT 2202 WITH FOREGROUND-COLOR Cyan.
             GO TO BC70.

       BC85.
             DISPLAY "Input details complete" AT 4624 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             PERFORM CHECK-CORRECT.
      *
      *    ****    C L E A R   L I N E   4 6
      *
             MOVE 46                 TO CRT-START CRT-END.
             PERFORM ERASE-SCREEN.
           IF WS-OPTION = "N"
               MOVE ZERO             TO W10-MCOST
               MOVE 1                TO WS-S6
               MOVE 8                TO LIN
               GO TO BC70.
             PERFORM AY60 THRU AY999.
           IF WS-AMEND = 1
               MOVE 0                TO WS-ACTION
               PERFORM AY20 THRU AY59.

       BC88.
             INITIALIZE PRT-REC1.
             MOVE WS-ITM             TO PRT-EXT-ITEM.
             MOVE W15-RECORD1        TO STK-RECORD1.
             COMPUTE STK-COST ROUNDED = W10-MCOST * 1.
             MOVE STK-COST           TO W10-CCOST.
             COMPUTE W10-SELL ROUNDED = (W10-CCOST + ((W10-CCOST * STK-MARKUP) / 100)).
             COMPUTE W10-CASH ROUNDED = (W10-CCOST + ((W10-CCOST * STK-CMARKUP) / 100)).
             COMPUTE W10-WSALE ROUNDED = (W10-CCOST + ((W10-CCOST * STK-WMARKUP) / 100)).
           IF W05-ROUND = "E"
               MOVE W10-SELL         TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-SELL
               MOVE W10-WSALE        TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-WSALE
               MOVE W10-CASH         TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-CASH.
           IF NOT(STK-TAX = ZERO)
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
           IF W10-TAX > 0
               MOVE W05-VAT(W10-TAX) TO W05-RATE.
             COMPUTE W10-VCASH ROUNDED  = W10-CASH  + (W10-CASH  * W05-RTE).
             COMPUTE W10-VWSALE ROUNDED = W10-WSALE + (W10-WSALE * W05-RTE).
             COMPUTE W10-VSELL ROUNDED  = W10-SELL  + (W10-SELL  * W05-RTE).

       BC89.
             MOVE "Change the prices Y/N  [Y]" TO WS-OPT-MES.
             MOVE "Y"                TO WS-OPTION.
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF LS-OK = "Y" AND WS-OPTION = "Y"
               MOVE W10-SELL         TO STK-SELL
               MOVE W10-CASH         TO STK-CASH
               MOVE W10-WSALE        TO STK-WSALE
               PERFORM BC100 THRU BC199
           ELSE
               MOVE W15-RECORD1  TO STK-RECORD1.
             PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
             MOVE 1                  TO WS-S6.
             MOVE W10-COMMENT        TO PRT-COMMENT.

       BC90.
             MOVE W11-EXT-CODE(WS-S6)  TO PRT-EXT-CODE(WS-S6).
             MOVE W11-QUANT(WS-S6)   TO PRT-QUANT(WS-S6).
           IF WS-S6 < 30
               ADD 1                 TO WS-S6
               GO TO BC90.
           IF WS-AMEND = 0
               PERFORM WRITE-SPARTS THRU WRITE-SPARTS-EXIT
               MOVE 1                TO WS-ACTION
               PERFORM AY20 THRU AY59
           ELSE
               PERFORM REWRITE-SPARTS THRU WRITE-SPARTS-EXIT.
               GO TO BC99.

       BC95.
           IF W11-EXT-CODE(WS-S6) = SPACES
               GO TO BC85.
             ADD 1 WS-S6             GIVING WS-S8.
             MOVE SPACES             TO W11-EXT-CODE(WS-S6) W11-DESC(WS-S6).
             MOVE ZERO               TO W11-QUANT(WS-S6).
           IF WS-S8 > 29
               GO TO BC85.

       BC96.
             MOVE W11-EXT-CODE(WS-S8) TO W11-EXT-CODE(WS-S6).
             MOVE W11-DESC(WS-S8)    TO W11-DESC(WS-S6).
             MOVE W11-QUANT(WS-S8)   TO W11-QUANT(WS-S6).
             MOVE SPACES             TO W11-EXT-CODE(WS-S8) W11-DESC(WS-S8).
             MOVE ZERO               TO W11-QUANT(WS-S8).
           IF WS-S8 < 30
               ADD 1                 TO WS-S6 WS-S8
               GO TO BC96.
             GO TO BC70.

       BC99.
             EXIT.

       BC100.
             MOVE 0301               TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
             DISPLAY S04B.
             DISPLAY S04-N.
             PERFORM BC012-DET.
             MOVE STK-GUAR           TO W10-GUAR.
             PERFORM AA400.

       BC105.
             MOVE ZERO               TO W05-RATE.
           IF W10-TAX > 0
               MOVE W05-VAT(W10-TAX) TO W05-RATE.

       BC110.
             COMPUTE W10-VCASH ROUNDED  = W10-CASH  + (W10-CASH  * W05-RTE).
             COMPUTE W10-VWSALE ROUNDED = W10-WSALE + (W10-WSALE * W05-RTE).
             COMPUTE W10-VSELL ROUNDED  = W10-SELL  + (W10-SELL  * W05-RTE).
             DISPLAY S06A.
             DISPLAY S06C.

       BC115.
             ACCEPT S06D.
             MOVE W10-COST           TO W10-MCOST W10-CCOST.
             COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST * STK-MARKUP) / 100)).
             MOVE W10-CSELL          TO W10-SELL.
             COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST * STK-CMARKUP) / 100)).
             MOVE W10-CSELL          TO W10-CASH.
             COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST * STK-WMARKUP) / 100)).
             MOVE W10-CSELL          TO W10-WSALE.
             DISPLAY S06E.
             PERFORM BC105 THRU BC110.
             ACCEPT S06E.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO BC115.
             MOVE W10-COST           TO STK-COST.
             MOVE W10-SELL           TO STK-SELL.
             MOVE W10-CASH           TO STK-CASH.
             MOVE W10-WSALE          TO STK-WSALE.

       BC199.
             EXIT.
      *
      *    ****    C H E C K   T H E   A L T E R N A T E   I N D E X
      *
       BC200.
             MOVE ZERO               TO WS-ERROR.
             MOVE STK-CODE           TO STKA-CODE.
             MOVE ZERO               TO STKA-NO.
             PERFORM READ-STKALT THRU READ-STKALT-EXIT.
           IF WS-F-ERROR = 49
               MOVE 9                TO WS-ERROR
               GO TO BC210.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If  either of the two descriptions or Item code has  been ³
      *    ³ amended, then  the existing Alternate Index  entries will ³
      *    ³ be deleted and new entries generated.         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BC205.
             PERFORM DELETE-STKALT-REC THRU WRITE-STKALT-EXIT.
             PERFORM READ-STKALT-NEXT THRU READ-STKALT-EXIT.
           IF NOT(WS-F-ERROR = 49)
               IF (STKA-CODE = STK-CODE)
                   GO TO BC205.
             PERFORM AY60 THRU AY999.
             PERFORM STOCK-DESC-INDEX.
             PERFORM AY70 THRU AY999.

       BC210.
             EXIT.

       BC999.
             EXIT.
      *
      *    ****    C O N V E R T I O N  F A C T O R
      *
       BD000        SECTION 5.
       BD00.
             PERFORM SAVE-SCREEN-3.
             MOVE 10                 TO SHADE-ROW.
             MOVE 25                 TO SHADE-COL.
             MOVE 32                 TO SHADE-WIDTH.
             MOVE 3                  TO SHADE-LINES.
             DISPLAY S-CONV.
             PERFORM SCREEN-SHADOW.

       BD05.
             ACCEPT SA-FACT.
             PERFORM CHECK-CORRECT.
             PERFORM RESTORE-SCREEN-3.
           IF WS-OPTION = "N"
               GO TO BD00.

       BD999.
             EXIT.
      *
      *    ****  A L T E R N A T E   S T O C K  I N D E X
      *

      COPY "STOCK.DIX".

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
             MOVE PAR-CRDX           TO WS-CARDEX.
             MOVE PAR-CASES          TO WS-USE-CASES.
             MOVE PAR-PACKS          TO WS-USE-PACKS.
             MOVE PAR-USE-ITM        TO WS-USE-ITM.
             MOVE PAR-EXT-STK        TO WS-EXT-STK.
           IF NOT(PAR-ART11 = "Y" OR "N")
               MOVE "N"              TO PAR-ART11.
             MOVE PAR-ART11          TO W10-ART11.

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
             MOVE STK-CON-FACT       TO WS-CON-FACT.
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
               WHEN 2     MOVE W02-NETWORK  TO WS-FILE
                          MOVE WS-NETKEY    TO WS-KEY
               WHEN 7     MOVE W02-DEPART   TO WS-FILE
                          MOVE DPT-CODE     TO WS-KEYX
               WHEN 15    MOVE WS-PARID     TO WS-FILE
                          MOVE WS-PARKEY    TO WS-KEY
               WHEN 18    MOVE W02-RECOVER  TO WS-FILE
                          MOVE WS-RECKEY    TO WS-KEY
               WHEN 21    MOVE W02-SPARTS   TO WS-FILE
                          MOVE PRT-ITEM     TO WS-KEYX
               WHEN 22    MOVE W02-STOCKF   TO WS-FILE
                          MOVE STK-CODE     TO WS-KEYX
               WHEN 25    MOVE W02-STKMEM   TO WS-FILE
                          MOVE SME-KEY      TO WS-KEYX
               WHEN 37    MOVE W02-SHARED   TO WS-FILE
                          MOVE WS-SHARED    TO WS-KEY
               WHEN 40    MOVE W02-LEDTRF   TO WS-FILE
                          MOVE XFR-KEY      TO WS-KEYX
               WHEN 46    MOVE W02-STKDEX   TO WS-FILE
                          MOVE STX-KEY      TO WS-KEYX
               WHEN 49    MOVE W02-STKALT   TO WS-FILE
                          MOVE STKA-KEY     TO WS-KEYX
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STP002"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
