      $set LINKCOUNT"578" GNT"STP023.GNT"
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
      *    ******   ********  *******     ****     ******    ******    *
      *   **    **     **     **    **   **  **   **    **  **    **   *
      *   **           **     **    **  **    **       **         **   *
      *    ******      **     *******   **    **     **         ***    *
      *         **     **     **        **    **   **             **   *
      *   **    **     **     **         **  **   **        **    **   *
      *    ******      **     **          ****    ********   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     P R O D U C T I O N   D E T A I L S   A L L   I T E M S    *
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
       PROGRAM-ID.     STP023.
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

       COPY "AUDIT.SL".

       COPY "CONTROL.SL".

       COPY "DEPART.SL".

       COPY "PARAM.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

       COPY "SPARTS.SL".

       COPY "STKDEX.SL".

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

       COPY "AUDIT.FDE".

       COPY "CONTROL.FDE".

       COPY "DEPART.FDE".

       COPY "RECOVER.FD".

       COPY "PARAM.FDE".

       COPY "SHARED.FDE".

       COPY "SPARTS.FDE".

       COPY "STKDEX.FDE".

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
       77  WS-DISC                 PIC  9(03)V99 COMP-3.
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
       77  WS-AUDKEY    PIC 9(04)  COMP-5.
       77  WS-TRFKEY    PIC 9(06)  COMP-5.
       77  WS-PRCKEY    PIC 9(06)  COMP-5.
       77  WS-PARKEY    PIC 9(06)  COMP-5.
       77  WS-NETKEY    PIC 9(06)  COMP-5.
       77  WS-RECKEY    PIC 9(06)  COMP-5.
       77  WS-RECOVER    PIC 9(06)  COMP-5.
       77  WS-RECORDS    PIC 9(06)  COMP-5.
       77  WS-SHARED    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
       77  WS-STOCK    PIC X(01).
       77  WS-KEYSTR    PIC 9(04)  COMP-5.
       77  WS-GST-RATE    PIC S9(03)V99 COMP-3.
       77  WS-MONTHS    PIC S9(01)V99 COMP-3.
       77  WS-KEY1    PIC 9(04)  COMP-5.
       77  WS-ITM    PIC X(18).
       77  WS-USE-CASES    PIC X(01).
       77  WS-USE-PACKS    PIC X(01).
       77  WS-USE-ITM    PIC X(01).
       77  WS-EXT-STK    PIC X(01).
       77  WS-CARDEX    PIC X(01).
       77  WS-ETYPE    PIC X(01).
       77  WS-PAGE    PIC 9(04)  COMP-5.
       77  WS-OPTION    PIC X(01).
       77  WS-SKIP    PIC X(01).
       77  WS-ADJ    PIC X(01).
       77  WS-ERROR    PIC 9(01).
       77  WS-AMEND    PIC 9(01)  VALUE 0.
       77  WS-CONREC    PIC 9(01) VALUE ZERO.
       77  WS-LEDG    PIC X(04).
       77  WS-LEDG1    PIC X(04).
       77  WS-COMP    PIC 9(01).
       77  WS-WEEK    PIC 9(01).
       77  WS-CST    PIC X(01).
       77  WS-TYPE    PIC X(01).
       77  WS-BATCH    PIC 9(05).
       77  WS-PASSWORD    PIC X(08).
       77  WS-PRINT    PIC 9(01).
       77  WS-LABCDE    PIC X(08).
       77  WS-PASS    PIC X(08).
       77  WS-DESC-H1    PIC X(14) VALUE "Description".
       77  WS-DESC-H2    PIC X(14) VALUE "Description 2".
       77  WS-RT-HD    PIC X(10) VALUE "-Retail   ".
       77  WS-WS-HD    PIC X(10) VALUE "-Wholesale".
       77  WS-CS-HD    PIC X(10) VALUE "-Cash sale".
       77  WS-NODEP    PIC X(30) VALUE
      "NO SUCH   D E P A R T M E N T".
       77  PRG-NAME    PIC X(12) VALUE "STP\STPLOOK".
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.
       77  WS-DRAW    PIC 9(02).

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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STP023".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENT -PRODUCE: DETAILS ALL ITEMS".
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
    03  WS-PROG        PIC  X(03) VALUE "023".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-PRINTER-DETAILS.
           03  W02-PRINTER         PIC  X(12).
           03  W02-PGE-LENGTH      PIC  9(03).
           03  W02-PRN-LENGTH      PIC  9(03).
           03  W02-LINAGE          PIC  9(03).
           03  W02-PRN-STATUS      PIC  X(01)           VALUE "C".
      *
      *    ****    D  =  Detail line
      *            C  =  Condensed print
      *            E  =  Expanded print
      *            H  =  Header line
      *            X  =  Cancel expanded print
      *            1  =  10 Characters per inch
      *            2  =  12 Characters per inch
      *            3  =  17 Characters per inch
      *            6  =  6 Lines per inch
      *            8  =  8 Lines per inch
      *
           03  W02-PRN-TYPE        PIC  X(01).
           03  W02-PRN-LINE        PIC  X(136).

    03  R-SL1 REDEFINES W02-PRN-LINE.
        05  R-SLA       PIC  X(78).
        05  FILLER      PIC  X(58).
    03  R-SL2 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(58).
        05  R-SLB       PIC  X(78).
          03  REP-LINE1 REDEFINES W02-PRN-LINE.
              05  REP-DETAIL1 PIC  X(140).
          03  REP-LINE2 REDEFINES W02-PRN-LINE.
              05  REP-DETAIL2 PIC  X(80).
          03  REP-LINE3 REDEFINES W02-PRN-LINE.
              05  REP-DATE    PIC  X(06).
              05  REP-DTE     PIC  Z9/99/9999.
              05  FILLER      PIC  X(03).
        05  REP-PRC-HD.
     07  FILLER  PIC  X(01).
           07  REP-COMPANY  PIC X(40).
     07  FILLER  PIC  X(02).
              05  FILLER      PIC  X(08).
              05  REP-PAGE    PIC  X(06).
              05  REP-P-NO    PIC  Z(03)9.
          03  REP-LINE4 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(30).
              05  REP-HEAD1   PIC  X(20).
              05  FILLER      PIC  X(30).
          03  REP-LINE5 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(09).
              05  REP-DSC     PIC  X(30).
              05  FILLER      PIC  X(93).
          03  REP-LINE6 REDEFINES W02-PRN-LINE.
              05  REP-HEAD2   PIC  X(10).
              05  REP-CNO     PIC  X(04).
              05  REP-CATNO REDEFINES REP-CNO PIC X(04).
              05  FILLER      PIC  X(01).
              05  REP-CAT     PIC  X(30).
              05  FILLER      PIC  X(35).
          03  REP-LINE4 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(32).
              05  REP-HEAD10  PIC  X(16).
              05  FILLER      PIC  X(32).
          03  REP-LINE9 REDEFINES W02-PRN-LINE.
              05  REP-PG      PIC  X(06).
              05  REP-P-N     PIC  Z(03)9.
              05  FILLER      PIC  X(21).
              05  REP-COMP    PIC  X(40).
              05  REP-REPORT  PIC  X(34).
              05  FILLER      PIC  X(11).
              05  REP-DAT     PIC  X(06).
              05  REP-DT      PIC  Z9/99/9999.
          03  ORD-LINE1 REDEFINES W02-PRN-LINE.
              05  ORD-H1    PIC X(19).
              05  ORD-H2    PIC X(35).
              05  ORD-H3    PIC X(11).
              05  ORD-H4    PIC X(11).
              05  ORD-H5    PIC X(10).
              05  ORD-H6    PIC X(10).
          03  ORD-LINE2 REDEFINES W02-PRN-LINE.
              05  ORD-EXT-ITEM.
     07  ORD-ITEM    PIC X(14).
     07  ORD-SL    PIC X(01).
     07  ORD-ITM    PIC X(03).
              05  FILLER    PIC X(01).
              05  ORD-DESC    PIC X(30).
              05  FILLER    PIC X(01).
              05  ORD-QUANT    PIC Z(06)9.9999.
              05  FILLER    PIC X(01).
              05  ORD-COST    PIC Z(06)9.999.
              05  FILLER    PIC X(01).
              05  ORD-SELL    PIC Z(06)9.99.
              05  FILLER    PIC X(01).
              05  ORD-VALUE    PIC Z(06)9.99.

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
      *    The following copy 'DateVariables' now includes the 'W12.WS'
      *    and 'W20.WS' variables and is passed to DateCheck in the CALL
      *    statement.
      *
       COPY "DateVariables.cpy".

       COPY "W15.STK".

       COPY "W25.STK".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       COPY W70DEBT.IWS.

       COPY "W75.STK".

       01  W80-EDIT.
           03  W80-CNO        PIC 9(04).
           03  W80-V1.
              05  W80-7V3    PIC Z(06)9.999.
           03  W80-V2.
               05  W80-S5V2   PIC Z(04)9.99-.
           03  W80-DTE.
               05  W80-DATE   PIC Z9/99/99.
           03  W80-QNT.
               05  W80-5      PIC Z(04)9.99.
           03  W80-QNT1 REDEFINES W80-QNT.
               05  W80-3V2    PIC Z(02)9.9999.
           03  W80-NO.
               05  W80-5N     PIC 9(05).
               05  W80-REC REDEFINES W80-5N
                              PIC ZZZZ9.
           03  W80-DEL.
               05  W80-2N      PIC 9(02).
       01  W85-PASS.
           03  W85-SUPER       PIC  X(06)    OCCURS 9.
           03  W85-ENTRY       PIC  9(02)    COMP-3.
           03  W85-MARG        PIC S9(02)V99.
       01  W90-BAL             PIC S9(07)V99 COMP-3.
       01  W95-COMP            PIC X(40).
       01  W100-EDIT.
           03  W100-V2.
               05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.

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
    03  LINE  6 COLUMN  2 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(14) USING W10-EXT-ITEM AUTO.

       01  S05-ITM.
    03  LINE  6 COLUMN  2 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(14)/X(03)
     USING W10-EXT-ITEM AUTO.

       01  S05-EXT.
    03  LINE 11 COLUMN  2 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(18) USING W10-EXT-ITEM AUTO.

       01  S07.
    03  LINE 11 COLUMN  2 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(14) USING W10-EXT-ITEM AUTO.

       01  S07-ITM.
    03  LINE 11 COLUMN  2 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(14)/X(03)
     USING W10-EXT-ITEM AUTO.

       01  S07-EXT.
    03  LINE 11 COLUMN  2 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(18) USING W10-EXT-ITEM AUTO.

       01  S15.
          03  LINE  2 COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
           VALUE "PRODUCTION PROCESSING".
    03  LINE  3 COLUMN 26 FOREGROUND-COLOR Cyan HIGHLIGHT
         VALUE "Key in total items/labour used".
           03  LINE  4 COLUMN  2 VALUE "Item Code/Description".
           03          COLUMN 36 VALUE "Quantity".
           03          COLUMN 47 VALUE "Unit cost".
           03          COLUMN 59 VALUE "Unit sell".
           03          COLUMN 70 VALUE "Total cost".

       01  S35.
    03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC Z(05)9.9999 USING W10-QUANT AUTO.

       01  S36.
    03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC Z(07)9.999 USING W10-SELL AUTO.

       01  S37.
    03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC Z(05)9.999 USING W10-COST AUTO.

       01  S38.
    03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC ZZZZ9.9999 USING W10-MQNT AUTO.

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
             MOVE ZERO               TO WS-PAGE.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *     **  *   *   * *   *  *  **  *   *   *     *   *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *     *  **   *     *   *  *  *  **   *   *     *  *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** *   *
      *
             PERFORM OPEN-PRINTER.
             MOVE "P"                TO WS-COMMAND.
             PERFORM BQ000.
             PERFORM AB35.
             MOVE "C"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             CLOSE AUDIT RECOVER.

       AA49.
             EXIT PROGRAM.

       AB000        SECTION.
      *
      *    ****   1 2 C P I ( E L I T E   P R I N T )
      *
       AB25.
             MOVE 0                  TO WS-ADVANCE.
             MOVE 2                  TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             MOVE SPACES             TO REP-DETAIL1.

       AB30.
             EXIT.
      *
      *    ****   1 O C P I ( N O R M A L P R I N T )
      *
       AB35.
      MOVE 0   TO WS-ADVANCE.
      MOVE 1   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      MOVE "D"   TO W02-PRN-TYPE.
            MOVE SPACES  TO REP-DETAIL1.

       AB40.
             EXIT.

       COPY "AA50.LUP".

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

       COPY "ABORT.PRN".

       COPY "PRINTUTL.AS3".

       COPY "AA300.STK".

       COPY "DATE.CHK".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000-READ        SECTION.

       COPY "AUDIT.RD".

       COPY "CONTROL.RD".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "SHARED.RD".

       COPY "STKMEM.RD".

       COPY "STOCK.RD".

       COPY "SPARTS.RD".

      *
      *   R E W R I T E   &    W R I T E   F I L E S   R O U T I N E S
      *
       AD000-WRITE        SECTION.

       COPY "AUDIT.WR".

       COPY "CONTROL.WR".

       COPY "DEPART.WR".

       COPY "PARAM.WR".

       COPY "SHARED.WR".

       COPY "STKDEX.WR".

       COPY "STKMEM.WR".

       COPY "STOCK.WR".

       COPY "SPARTS.WR".

       COPY "STOCK.1ST".

       COPY "APAC.HLP".

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
       AY000           SECTION.
       AY01.
             MOVE 01                 TO REC-FILE.
             MOVE WS-PARKEY          TO REC-KEY.
             MOVE PAR-RECORD1        TO REC-PARAM.
             GO TO AY50.

       AY05.
             MOVE 05             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
           IF WS-ACTION = 0 OR 2
               PERFORM READ-STOCK-LOCK THRU READ-STOCK-EXIT.
           IF WS-ACTION = 9
               MOVE 0                TO REC-TYPE
           ELSE
               MOVE WS-ACTION        TO REC-TYPE.
             MOVE STK-RECORD1        TO REC-STOCKF.
             GO TO AY50.

       AY16.
             MOVE 16             TO REC-FILE.
             MOVE WS-AUDKEY      TO REC-KEY.
             MOVE AUD-REC1       TO REC-AUDITF.
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

       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.

       AY46.
            MOVE 46   TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
            MOVE STX-REC1  TO REC-STKDEX.
             GO TO AY50.

       AY49.
             MOVE 99             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE SPACES         TO REC-DETAIL.
             PERFORM AY50 THRU AY59.
             ADD 1               TO WS-TRANS.
           IF WS-RECOVER > 95
               CLOSE RECOVER
               OPEN OUTPUT RECOVER
               CLOSE RECOVER
               OPEN I-O RECOVER
               MOVE ZERO         TO WS-RECOVER.
             GO TO AY59.

       AY50.
            ADD 1   TO WS-RECOVER.
             MOVE WS-RECOVER     TO WS-RECKEY.
             MOVE WS-TRANS       TO REC-TRAN.
             WRITE REC-RECORD.
           IF WS-STATUS NOT = "00"
               DISPLAY "Write error Recovery file - Status " AT 2212
                        WITH FOREGROUND-COLOR Brown HIGHLIGHT WS-STATUS
                        WITH FOREGROUND-COLOR Grey HIGHLIGHT
               STOP RUN.

       AY59.
             EXIT.
      *
      *    ****    Start processing transaction
      *
       AY60.
            MOVE 1   TO WS-COUNT.
            MOVE 5   TO WS-SHARED.
            PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      MOVE SHR-STOCK  TO WS-STOCK.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
            MOVE 4   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-USERS < 24
               MOVE 1            TO WS-SUB
        MOVE ZERO  TO WS-WAIT
               GO TO AY62.
      *
      *     Q   F U L L  -  W A I T   F O R   A  C O U N T  O F   400
      *
            DISPLAY "WAITING" AT 5051
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red HIGHLIGHT.
            COMMIT.
      ACCEPT WS-STIME FROM TIME.
      MOVE 400   TO WS-WAIT.
            PERFORM LOCK-REC-LOOP.
            DISPLAY SPACE AT 5051
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.
             GO TO AY60.

       AY61.
            MOVE "ST"   TO PAR-PROG(WS-USUB).
            MOVE LS-USER  TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
            MOVE 1   TO WS-NETKEY.
             PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             PERFORM AY39 THRU AY59.
             MOVE NET-DEBTOR     TO W70-DEBT.
      *
      *    ****    Read the STOCK control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
    IF WS-STOCK = "Y"
              MOVE 3   TO WS-SHARED
              PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT
        PERFORM AY37 THRU AY59
        MOVE SHR-RECORD  TO NET-RECORD
    ELSE
              MOVE 3   TO WS-NETKEY
              PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT
              PERFORM AY39 THRU AY59.
            GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N P R O G R E S S
      *
       AY62.
    IF NOT (PAR-PROG(WS-SUB) = SPACES OR
     PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R O R   S T O C K   F I L E S
      *    B E I N G   U P D A T E D
      *
        IF NOT (PAR-PROG(WS-SUB) = SPACES)
           IF PAR-PROG(WS-SUB) = "DB" OR "DS" OR "CS" OR "ST"
      *
      *    ****   Y E S   -   S E T   W A I T P E R I O D
      *
         GO TO AY62-WAIT
     ELSE
         ADD 1  TO WS-SUB
         GO TO AY62
     END-IF
        ELSE
      *
      *    ****   I S T H I S   P R O G R A M   I N T H E  Q
      *
        IF PAR-USR(WS-SUB) = LS-USER
      *
      *    ****   I S I T   N E X T I N   L I N E T O   P R O C E S S
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
      MOVE LS-USER  TO PAR-USR(WS-SUB).
      MOVE WS-SUB  TO PAR-USERS.
      PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
      GO TO AY62-CHECK.
      *
      *    ****   S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
      MOVE 300   TO WS-WAIT.
    IF NOT (PAR-USR(WS-SUB) = LS-USER)
        IF WS-SUB < 24
     ADD 1  TO WS-SUB
     GO TO AY62.

       AY62-CHECK.
    IF WS-WAIT > ZERO
        COMMIT
        DISPLAY "Waiting" AT 5072
   WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT BLINK
        ACCEPT WS-STIME FROM TIME
        PERFORM LOCK-REC-LOOP
        DISPLAY "Waiting" AT 5072
   WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT BLINK
        GO TO AY60.
            DISPLAY SPACE AT 5072
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.

       AY63.
            MOVE WS-SUB  TO WS-USUB.
            GO TO AY61.

       AY70.
            MOVE 4   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 3.
      *
    IF WS-STOCK = "Y"
        MOVE NET-RECORD  TO SHR-RECORD
              PERFORM REWRITE-SHARED THRU WRITE-SHARED-EXIT
    ELSE
              PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
            MOVE 1   TO WS-NETKEY.
             MOVE W70-DEBT       TO NET-DEBTOR.
            PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
            PERFORM AY49 THRU AY59.
      MOVE 1   TO WS-USUB.

       AY72.
    IF NOT (PAR-USR(WS-USUB) = LS-USER)
        IF WS-USUB < 24
     ADD 1  TO WS-USUB
     GO TO AY72
        ELSE
     GO TO AY85.

       AY75.
            MOVE SPACES  TO PAR-PROG(WS-USUB)
              PAR-USR(WS-USUB).
    IF WS-USUB < 24
        ADD 1 WS-USUB  GIVING WS-SUB
    ELSE
        GO TO AY80.
    IF NOT (PAR-PROG(WS-SUB) = SPACES)
        MOVE PAR-PROG(WS-SUB)
     TO PAR-PROG(WS-USUB)
        MOVE PAR-USR(WS-SUB)
     TO PAR-USR(WS-USUB)
        ADD 1   TO WS-USUB
        GO TO AY75.

       AY80.
            SUBTRACT 1 FROM WS-USUB
     GIVING PAR-USERS.
            PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.

       AY85.
    IF LS0-POS NOT = 2
               CLOSE RECOVER
               OPEN I-O RECOVER.
             COMMIT.

       AY999.
             EXIT.

      /
       BG00-TRAN               SECTION.
       BG80.
             MOVE W70-AUDIT      TO WS-AUDKEY.
             MOVE LOW-VALUES     TO AUD-REC1.
             PERFORM AY16 THRU AY59.
            MOVE STK-CODE  TO AUD-EXT-CODE.
             MOVE 2              TO AUD-TYPE.
            MOVE WS-DRAW  TO AUD-SUB.
             PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
             ADD 1               TO W70-AUDIT.
             MOVE 1              TO WS-AUDKEY.
             PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
             PERFORM AY16 THRU AY59.
             MOVE 1              TO AUD-REP2.
             PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
       BG85.
             EXIT.

       COPY "CA000.STK".

       COPY "CA100.STK".

       COPY "CA200.STK".

       COPY "CA500.STK".

       COPY "ROUND.PRC".

       CB300      SECTION.
       CB301.
    IF W10-TAX NOT = ZERO
        COMPUTE W10-SL(WS-S1) ROUNDED = (W10-VSL(WS-S1) /
    (100.00000 + W05-RATE)) * 100.00
        COMPUTE W10-WSL(WS-S1) ROUNDED = (W10-VWSL(WS-S1) /
     (100.00000 + W05-RATE)) * 100.00
        COMPUTE W10-CSH(WS-S1) ROUNDED = (W10-VCSH(WS-S1) /
    (100.00000 + W05-RATE)) * 100.00
           ELSE
        MOVE W10-VWSL(WS-S1)
     TO W10-WSALE
        MOVE W10-VCSH(WS-S1)
     TO W10-CASH
        MOVE W10-VSL(WS-S1)
     TO W10-SELL.
    IF W10-TAX NOT = ZERO
        COMPUTE W10-CHECK ROUNDED
     = W10-SL(WS-S1) + (W10-SL(WS-S1) * W05-RTE)
        IF W10-CHECK > W10-VSL(WS-S1)
     SUBTRACT 0.01 FROM W10-SL(WS-S1)
        ELSE
        IF W10-CHECK < W10-VSL(WS-S1)
     ADD 0.01  TO W10-SL(WS-S1)
        END-IF
        COMPUTE W10-CHECK ROUNDED
     = W10-CSH(WS-S1) + (W10-CSH(WS-S1) * W05-RTE)
        IF W10-CHECK > W10-VCSH(WS-S1)
     SUBTRACT 0.01 FROM W10-CSH(WS-S1)
        ELSE
        IF W10-CHECK < W10-VCSH(WS-S1)
     ADD 0.01  TO W10-CSH(WS-S1)
        END-IF
        COMPUTE W10-CHECK ROUNDED
     = W10-WSL(WS-S1) + (W10-WSL(WS-S1) * W05-RTE)
        IF W10-CHECK > W10-VWSL(WS-S1)
     SUBTRACT 0.01 FROM W10-WSL(WS-S1)
        ELSE
        IF W10-CHECK < W10-VWSL(WS-S1)
     ADD 0.01  TO W10-WSL(WS-S1).

       CB399.
             EXIT.

      /
      *    ****    PRODUCTION PROCESSING
      *
       BQ000         SECTION 6.
       BQ0.
             PERFORM ERASE-SCREEN.
      DISPLAY "PRODUCTION PROCESSING" AT 0230
        WITH FOREGROUND-COLOR Grey HIGHLIGHT.
      MOVE "S"   TO WS-TYPE.
    IF LS0-STLEV < 3
               GO TO BQ0D.
      DISPLAY "Print the " AT 0412 WITH FOREGROUND-COLOR Grey
                     "C" WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     "ost, " WITH FOREGROUND-COLOR Grey
                     "S" WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     "elling or " WITH FOREGROUND-COLOR Grey
                     "B" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "oth prices" WITH FOREGROUND-COLOR Grey.

       BQ0C.
      ACCEPT WS-TYPE AT 0452
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-TYPE
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-TYPE = "B" OR "C" OR "S")
               GO TO BQ0C.

       BQ0D.
      DISPLAY "Use the " AT  0612 WITH FOREGROUND-COLOR Grey
       "A" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "verage or " WITH FOREGROUND-COLOR Grey
       "C" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "ost prices" WITH FOREGROUND-COLOR Grey.
      MOVE "A"   TO WS-CST.

       BQ0E.
      ACCEPT WS-CST AT 0644
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-CST
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-CST = "A" OR "C")
              GO TO BQ0E.
             MOVE ZERO           TO W25-PCOST WS-PAGE
                                    W25-TSELL W25-TOTAL.
      DISPLAY WS-BLNK78 AT 1202 WITH FOREGROUND-COLOR Cyan.

       BQ00.
             PERFORM ERASE-SCREEN.
             DISPLAY S15.

       BQ05.
      PERFORM AB35.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE SPACES  TO REP-DETAIL1.
             MOVE "PAGE:"        TO REP-PAGE.
             MOVE "DATE:"        TO REP-DATE.
             MOVE W95-COMP       TO REP-COMPANY.
             ADD 1               TO WS-PAGE.
             MOVE WS-PAGE        TO REP-P-NO.
             MOVE W12-TODAY      TO REP-DTE.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
             MOVE "PRODUCTION"   TO REP-HEAD10.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      PERFORM AB25.
            MOVE "Item Code"  TO ORD-H1.
            MOVE WS-DESC-H1  TO ORD-H2.
            MOVE "Quantity"  TO ORD-H3.
           IF WS-TYPE = "C" OR "B"
              MOVE "Unit Cost"  TO ORD-H4.
           IF WS-TYPE = "S" OR "B"
              MOVE "Unit Sell"  TO ORD-H5.
           IF WS-TYPE = "C" OR "B"
              MOVE "Total Cost" TO ORD-H6
           ELSE
              MOVE "Total Sell" TO ORD-H6.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE "D"   TO W02-PRN-TYPE.

       BQ10.
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
            MOVE ZERO   TO W10-QUANT W25-SCOST.
             MOVE 0701           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
      GO TO BQ45.

       BQ32.
           IF WS-TYPE = "B" OR "C"
              DISPLAY W80-5 AT 0749
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta.
    IF STK-MARKUP = ZERO
        MOVE STK-LDG  TO DPT-CODE
        PERFORM READ-DEPART THRU READ-DEPART-EXIT
              MOVE DPT-MARKUP  TO STK-MARKUP
              MOVE DPT-CMRKUP  TO STK-CMARKUP
              MOVE DPT-WMRKUP  TO STK-WMARKUP.
            COMPUTE W10-SELL ROUNDED = (W10-COST +
             ((W10-COST * STK-MARKUP) / 100)).
             MOVE W10-SELL       TO W10-MSELL.
           IF STK-CMARKUP NOT > ZERO
               MOVE STK-MARKUP   TO STK-CMARKUP.
            COMPUTE W10-CASH ROUNDED = (W10-COST +
             ((W10-COST * STK-CMARKUP) / 100)).
           IF STK-WMARKUP NOT > ZERO
               MOVE STK-CMARKUP  TO STK-WMARKUP.
            COMPUTE W10-WSALE ROUNDED = (W10-COST +
             ((W10-COST * STK-WMARKUP) / 100)).
             MULTIPLY W10-COST BY W10-QUANT
                                 GIVING W25-VALUE.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
           IF W05-ROUND = "E"
               MOVE W10-MSELL    TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-SELL W10-MSELL
               MOVE W10-WSALE    TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-WSALE
               MOVE W10-CASH     TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-CASH.
           IF STK-TAX NOT = ZERO
        MOVE W05-VAT (STK-TAX)
     TO W05-RATE
               COMPUTE W10-VSELL ROUNDED = W10-SELL +
                                           (W10-SELL * W05-RTE)
               COMPUTE W10-VWSALE ROUNDED = W10-WSALE +
                                            (W10-WSALE * W05-RTE)
               COMPUTE W10-VCASH ROUNDED = W10-CASH +
                                           (W10-CASH * W05-RTE)
           ELSE
               MOVE W10-WSALE    TO W10-VWSALE
               MOVE W10-CASH     TO W10-VCASH
               MOVE W10-SELL     TO W10-VSELL.
           IF W05-ROUND = "I"
               MOVE W10-VSELL    TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-VSELL
               MOVE W10-VWSALE   TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-VWSALE
               MOVE W10-VCASH    TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-VCASH
               PERFORM CB200.
      *
      *    ****   C H E C K   I F   S E L L I N G   P R I C E
      *    H A S   C H A N G E D
      *
      MOVE "N"   TO WS-SKIP.
    IF W10-MSELL > STK-SELL
        PERFORM BQ40 THRU BQ44
        IF WS-SKIP = "Y"
     MOVE STK-SELL TO W10-MSELL
     MOVE STK-CASH TO W10-CASH
     MOVE STK-WSALE
     TO W10-WSALE.
      MOVE W10-MSELL  TO W10-SELL.
      DISPLAY S36 AT 0758.

       BQ33.
    IF WS-SKIP = "N"
              ACCEPT S36 AT 0758
        IF W10-SELL < W10-WSALE
     MOVE "Selling price margin too small"
     TO WS-ERR-MES
           PERFORM ERROR-MESSAGE
     MOVE W10-WSALE
     TO W10-SELL
     GO TO BQ33
        END-IF
        MOVE W10-SELL  TO W10-MSELL.
           IF WS-TYPE = "B" OR "C"
               MOVE W25-VALUE    TO ORD-VALUE
              DISPLAY ORD-VALUE AT 0769
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta.
        MOVE ZERO  TO W90-BAL.
    IF STK-IND = 0
        IF STK-QUANT > ZERO
           MULTIPLY STK-QUANT BY STK-AVGE
     GIVING W90-BAL.
            ADD W25-SCOST  TO W25-PCOST W25-PCST.
             ADD W25-VALUE       TO W25-TOTAL W25-PTOT.
             COMPUTE W25-TSELL = W25-TSELL + (W10-MSELL * W10-QUANT).
             ADD W25-VALUE       TO W90-BAL.
    IF STK-IND = 0
              ADD W10-QUANT  TO STK-QUANT
        IF WS-CARDEX = "Y"
     INITIALIZE STX-REC1
     MOVE STK-CODE TO STX-EXT-CODE
     MOVE W12-T-YMD
     TO STX-DTE
     MOVE ZERO  TO STX-SEQ
     MOVE LS0-NO  TO STX-USER
     MOVE "PRODCT" TO STX-PROG
     MOVE LS-USER  TO STX-WS
     MOVE "Production" TO STX-REMARKS
     MOVE "E"          TO STX-TYPE
     MOVE W10-QUANT
     TO STX-QNT
     MOVE W25-VALUE
     TO STX-COST
     COMPUTE STX-SELL ROUNDED = (W25-VALUE +
      ((W25-VALUE * STK-MARKUP) / 100))
     PERFORM WRITE-STKDEX THRU WRITE-STKDEX-EXIT
     MOVE 1  TO WS-ACTION
     PERFORM AY46 THRU AY59.
            MOVE W10-QUANT  TO ORD-QUANT.
    IF (STK-IND = 0) AND
       (STK-QUANT > W10-QUANT)
              DIVIDE W90-BAL BY STK-QUANT
     GIVING STK-AVGE
    ELSE
        MOVE W10-COST  TO STK-AVGE.
          IF STK-AVGE < ZERO
               COMPUTE STK-AVGE = STK-AVGE * -1.
            MOVE W10-COST  TO STK-COST.
           IF WS-TYPE = "B" OR "C"
               MOVE W10-COST     TO ORD-COST.
      *
      *    ****   C H E C K   I F   S E L L I N G   P R I C E
      *    H A S   C H A N G E D
      *
    IF WS-SKIP = "N"
              IF W10-MSELL > STK-SELL
           MOVE W10-MSELL
     TO STK-SELL
        END-IF
              IF W10-CASH > STK-CASH
           MOVE W10-CASH TO STK-CASH
        END-IF
              IF W10-WSALE > STK-WSALE
           MOVE W10-WSALE
     TO STK-WSALE.
             MOVE STK-DESC       TO ORD-DESC.
    IF WS-USE-ITM = "Y"
        MOVE STK-ITEM  TO ORD-ITEM
        MOVE "/"   TO ORD-SL
        MOVE STK-ITM  TO ORD-ITM
    ELSE
              MOVE STK-CODE  TO ORD-EXT-ITEM.
    IF WS-TYPE = "B" OR "S"
               MOVE W10-MSELL    TO ORD-SELL
               COMPUTE ORD-VALUE = W10-MSELL * W10-QUANT.
      MOVE 0   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM BQ05.
            MOVE SPACES  TO REP-DETAIL1.
    IF (WS-USE-PACKS = "Y") AND
       (STK-USE-PACKS = "Y")
        PERFORM BQ90 THRU BQ105.
             PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
             SUBTRACT W25-PCST   FROM STK-TOTAL.
             ADD W25-PTOT        TO STK-TOTAL.
             PERFORM AY70 THRU AY999.

       BQ35.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE "TOTAL"  TO ORD-DESC.
           IF WS-TYPE = "S"
               MOVE W25-TSELL    TO ORD-VALUE
           ELSE
               MOVE W25-TOTAL    TO ORD-VALUE.
      PERFORM CALL-PRINTUTL.
            MOVE "***** END REPORT *****" TO REP-DETAIL1.
      MOVE 99   TO WS-ADVANCE.
      MOVE "E"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
      MOVE "P"   TO WS-COMMAND.
      MOVE SPACES  TO REP-DETAIL1.
             GO TO BQ999.

       BQ40.
      MOVE "N"   TO WS-SKIP.
      PERFORM SAVE-SCREEN.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1022
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Grey
       "¿" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "³    Selling prices have changed    " AT 1122
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Grey
       "³" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "³       " AT 1222
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Grey
       "Current       "
        WITH FOREGROUND-COLOR Magenta BACKGROUND-COLOR Grey
       "New     "
        WITH FOREGROUND-COLOR Red BACKGROUND-COLOR Grey
       "³" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "³ " AT 1322 WITH FOREGROUND-COLOR Grey HIGHLIGHT
           BACKGROUND-COLOR Grey
       WS-RT-HD WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
       "                         " WITH BACKGROUND-COLOR Grey
       "³" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "³ " AT 1422 WITH FOREGROUND-COLOR Grey HIGHLIGHT
           BACKGROUND-COLOR Grey
       WS-CS-HD WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
       "                         " WITH BACKGROUND-COLOR Grey
       "³" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "³ " AT 1522 WITH FOREGROUND-COLOR Grey HIGHLIGHT
           BACKGROUND-COLOR Grey
       WS-WS-HD WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
       "                         " WITH BACKGROUND-COLOR Grey
       "³" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "À" AT 1622
    WITH FOREGROUND-COLOR Grey HIGHLIGHT
         BACKGROUND-COLOR Grey
       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
         WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      MOVE 11   TO SHADE-ROW.
      MOVE 24   TO SHADE-COL.
      MOVE 36   TO SHADE-WIDTH.
      MOVE 6   TO SHADE-LINES.
      PERFORM SCREEN-SHADOW.
      MOVE STK-SELL  TO W80-7V3.
      DISPLAY W80-V1 AT 1335
       WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Grey.
      MOVE STK-CASH  TO W80-7V3.
      DISPLAY W80-V1 AT 1435
       WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Grey.
      MOVE STK-WSALE  TO W80-7V3.
      DISPLAY W80-V1 AT 1535
       WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Grey.
      MOVE W10-MSELL  TO W80-7V3.
      DISPLAY W80-V1 AT 1347
       WITH FOREGROUND-COLOR Magenta BACKGROUND-COLOR Grey.
      MOVE W10-CASH  TO W80-7V3.
      DISPLAY W80-V1 AT 1447
       WITH FOREGROUND-COLOR Magenta BACKGROUND-COLOR Grey.
      MOVE W10-WSALE  TO W80-7V3.
      DISPLAY W80-V1 AT 1547
       WITH FOREGROUND-COLOR Magenta BACKGROUND-COLOR Grey.
      MOVE "Use the new selling prices Y/N  [N]"
     TO WS-ERR-MES.
      MOVE "N"   TO WS-OPTION.

       BQ42.
      MOVE 18   TO SLIN.
      PERFORM OPT-SETUP THRU OPT-EXIT.
    IF NOT (WS-OPTION = "Y" OR "N")
        GO TO BQ42.
    IF WS-OPTION = "Y"
        MOVE "N"   TO WS-SKIP
    ELSE
        MOVE "Y"   TO WS-SKIP.

       BQ44.
      EXIT.

       BQ45.
             DISPLAY "Items Used" AT 0902 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "Item Code/Description" AT 1002.
             DISPLAY "Quantity" AT 1036.
       BQ50.
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
   WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
       GO TO BQ85
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
       GO TO BQ50
   WHEN F2-KEY
       MOVE "I"  TO W10-ETYPE
   WHEN F3-KEY
       MOVE "A"  TO W10-ETYPE
   WHEN F4-KEY
       MOVE "D"  TO W10-ETYPE
   WHEN F5-KEY
       MOVE "2"  TO W10-ETYPE
   WHEN F6-KEY
       MOVE "X"  TO W10-ETYPE
   WHEN F9-KEY
       MOVE "W"  TO W10-ETYPE
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO BQ50
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
                   GO TO BQ50.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
            PERFORM CLEAR-L50.
          IF W10-EXT-ITEM = SPACES
               GO TO BQ85.

       BQ51.
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
        MOVE "Z"   TO W10-ETYPE
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
                   GO TO BQ50
               ELSE
                   GO TO BQ51.
    IF STK-IND = 4
        MOVE "Header Record"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BQ50.
      MOVE STK-CODE  TO PRT-EXT-ITEM.
             PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
           IF WS-F-ERROR = ZERO
              MOVE "Item is manufactured or assembled"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO BQ50.
            DISPLAY STK-DESC AT 0702
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
    IF NOT (STK-DESC2 = SPACES)
        DISPLAY STK-DESC2 AT 0802
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO BQ50.
      *
      *    ****    Q U A N T I T Y
      *
       BQ55.
             ACCEPT S35 AT 0733.
           IF W10-QUANT = ZERO
               GO TO BQ50.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO BQ55.
      PERFORM AY60 THRU AY999.
             MOVE 0              TO WS-ACTION.
             PERFORM AY05 THRU AY59.
      *
      *    ****    S A V E   S T O C K  R E C O R D
      *
            MOVE STK-RECORD1  TO W15-RECORD1.
            MOVE 1   TO WS-S6.

       BQ60.
            MOVE SPACES  TO W11-EXT-CODE(WS-S6)
        W11-DESC(WS-S6).
      MOVE ZERO   TO W11-QUANT(WS-S6).
           IF WS-S6 < 30
               ADD 1             TO WS-S6
               GO TO BQ60.
             MOVE 1              TO WS-S6.
             MOVE ZERO           TO W10-MCOST W10-MSELL
                                    W25-PCST  W25-PTOT.

       BQ65.
            MOVE SPACES  TO W10-EXT-ITEM.
             MOVE ZERO           TO W10-MQNT.
             MOVE 1101           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 13.

       BQ70.
             PERFORM AA300.
    IF W10-STCK = "A"
        IF WS-EXT-STK = "Y"
     ACCEPT S07-EXT
        ELSE
        IF WS-USE-ITM = "Y"
     ACCEPT S07-ITM
        ELSE
           ACCEPT S07
        END-IF
    ELSE
    IF W10-SLNGTH = 3
              ACCEPT W10-I3 AT 1102
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta UPDATE AUTO
    ELSE
    IF W10-SLNGTH = 4
              ACCEPT W10-I4 AT 1102
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta UPDATE AUTO
    ELSE
    IF W10-SLNGTH = 5
              ACCEPT W10-I5 AT 1102
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta UPDATE AUTO
    ELSE
    IF W10-SLNGTH = 6
              ACCEPT W10-I6 AT 1102
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta UPDATE AUTO
    ELSE
    IF W10-SLNGTH = 7
              ACCEPT W10-I7 AT 1102
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta UPDATE AUTO
    ELSE
    IF W10-SLNGTH = 8
              ACCEPT W10-I8 AT 1102
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta UPDATE AUTO
    ELSE
    IF W10-SLNGTH = 9
              ACCEPT W10-I9 AT 1102
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta UPDATE AUTO
    ELSE
    IF W10-SLNGTH = 10
              ACCEPT W10-I10 AT 1102
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta UPDATE AUTO
    ELSE
    IF W10-SLNGTH = 11
              ACCEPT W10-I11 AT 1102
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta UPDATE AUTO
    ELSE
    IF W10-SLNGTH = 12
              ACCEPT W10-I12 AT 1102
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta UPDATE AUTO
    ELSE
    IF W10-SLNGTH = 13
              ACCEPT W10-I13 AT 1102
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta UPDATE AUTO
    ELSE
    IF W10-SLNGTH = 14
              ACCEPT W10-I14 AT 1102
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
       GO TO BQ80
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
       GO TO BQ70
   WHEN F2-KEY
       MOVE "I"  TO W10-ETYPE
   WHEN F3-KEY
       MOVE "A"  TO W10-ETYPE
   WHEN F4-KEY
       MOVE "D"  TO W10-ETYPE
   WHEN F5-KEY
       MOVE "2"  TO W10-ETYPE
   WHEN F6-KEY
       MOVE "X"  TO W10-ETYPE
   WHEN F9-KEY
       MOVE "W"  TO W10-ETYPE
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO BQ70
        END-EVALUATE
        PERFORM AA50-LOOKUP
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-EXT-STK = "Y"
         DISPLAY S07-EXT
     ELSE
     IF WS-USE-ITM = "Y"
         DISPLAY S07-ITM
     ELSE
         DISPLAY S07
     END-IF
        ELSE
     DISPLAY S07
        END-IF
        IF W10-EXT-ITEM = SPACES
                   GO TO BQ70.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
      PERFORM CLEAR-L50.
          IF W10-EXT-ITEM = SPACES
               GO TO BQ80.
            MOVE W10-EXT-ITEM  TO W10-EXT-CODE2.

       BQ71.
             PERFORM CA000.
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
    IF W10-STCK = "A"
        IF WS-EXT-STK = "Y"
     DISPLAY S07-EXT
        ELSE
        IF WS-USE-ITM = "Y"
     DISPLAY S07-ITM
        ELSE
           DISPLAY S07
        END-IF
    ELSE
        DISPLAY S07.
          IF WS-F-ERROR = 22
        MOVE "Z"   TO W10-ETYPE
              PERFORM AA50-LOOKUP
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-EXT-STK = "Y"
         DISPLAY S07-EXT
     ELSE
     IF WS-USE-ITM = "Y"
         DISPLAY S07-ITM
     ELSE
         DISPLAY S07
     END-IF
        ELSE
     DISPLAY S07
        END-IF
              IF W10-EXT-ITEM = SPACES
                   GO TO BQ70
               ELSE
                   GO TO BQ71.
    IF STK-IND = 4
        MOVE "Header Record"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BQ70.
            DISPLAY STK-DESC AT 1202
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO BQ70.

       BQ75.
             ACCEPT S38 AT 1233.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO BQ70.
           IF W10-MQNT = ZERO
               GO TO BQ70.
    IF NOT (STK-IND = 0)
        GO TO BQ78.
    IF W10-MQNT NOT > STK-QUANT
        GO TO BQ78.
      DISPLAY "Quantity will be negative " AT 5002
        WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Blue
       "A" WITH BACKGROUND-COLOR Cyan
         FOREGROUND-COLOR Brown HIGHLIGHT
       "ccept or " WITH BACKGROUND-COLOR Cyan
          FOREGROUND-COLOR Blue
       "R" WITH BACKGROUND-COLOR Cyan
         FOREGROUND-COLOR Brown HIGHLIGHT
       "ekey Item code" WITH BACKGROUND-COLOR Cyan
            FOREGROUND-COLOR Blue.
      MOVE SPACE   TO WS-OPTION.

       BQ76.
      ACCEPT WS-OPTION AT 5055
      WITH BACKGROUND-COLOR Cyan
    FOREGROUND-COLOR Grey HIGHLIGHT
    UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-OPTION = "A" OR "R")
        GO TO BQ76.
      PERFORM CLEAR-L50.
    IF WS-OPTION = "R"
        GO TO BQ70.

       BQ78.
      MOVE 0   TO WS-ACTION.
             PERFORM AY05 THRU AY59.
            MOVE W10-EXT-CODE2  TO W11-EXT-CODE(WS-S6).
            MOVE STK-DESC  TO W11-DESC(WS-S6).
            MOVE W10-MQNT  TO W11-QUANT(WS-S6).
           IF STK-IND = 3
              DISPLAY "Enter ITEM cost price of Buy-Out:" AT 2312
                       WITH FOREGROUND-COLOR Brown HIGHLIGHT
              MOVE ZERO          TO W10-COST STK-AVGE
              ACCEPT S37 AT 2346
              DISPLAY "Enter ITEM selling price of Buy-Out:" AT 5012
         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
              MOVE ZERO          TO W10-SELL
       ACCEPT S36 AT 5049
       MOVE W10-COST  TO STK-COST STK-AVGE
              MOVE W10-SELL      TO STK-SELL.
          IF WS-CST = "C"
              COMPUTE W10-COST = (W11-QUANT(WS-S6) * STK-COST)
           ELSE
              COMPUTE W10-COST = (W11-QUANT(WS-S6) * STK-AVGE).
           IF STK-IND = 0 OR 1
               COMPUTE W25-SCOST = W25-SCOST + (W10-COST * W10-QUANT)
           ELSE
              ADD W10-COST  TO W25-SCOST.
             COMPUTE W10-MCOST = W10-MCOST + W10-COST.
    IF STK-IND = 0
              SUBTRACT W11-QUANT(WS-S6)
     FROM STK-QUANT
        IF WS-CARDEX = "Y"
     INITIALIZE STX-REC1
     MOVE STK-CODE TO STX-EXT-CODE
     MOVE W12-T-YMD
     TO STX-DTE
     MOVE ZERO  TO STX-SEQ
     MOVE LS0-NO  TO STX-USER
     MOVE "PRODCT" TO STX-PROG
     MOVE LS-USER  TO STX-WS
     MOVE "Used for Production" TO STX-REMARKS
     MOVE "J"          TO STX-TYPE
     MULTIPLY -1 BY W11-QUANT(WS-S6)
     GIVING STX-QNT
     MULTIPLY -1 BY W10-COST
     GIVING STX-COST
     COMPUTE STX-SELL ROUNDED = (W10-COST +
      ((W10-COST * STK-MARKUP) / 100)) * -1
     PERFORM WRITE-STKDEX THRU WRITE-STKDEX-EXIT
     MOVE 1  TO WS-ACTION
     PERFORM AY46 THRU AY59
        END-IF
              IF NOT (STK-DISC > ZERO)
      *
      *    ****    C H E C K   R E - O R D E R   L E V E L   A N D
      *            R E P O R T   L O W - L E V E L S
      *
           IF STK-QUANT NOT > STK-LEVEL
               MOVE 1  TO STK-DISC
               PERFORM BG80.
            ADD W11-QUANT(WS-S6)
     TO STK-MTD STK-YTD.
          IF WS-CST = "C"
              COMPUTE W10-COST = (W11-QUANT(WS-S6) * STK-COST)
           ELSE
              COMPUTE W10-COST = (W11-QUANT(WS-S6) * STK-AVGE).
            ADD W10-COST  TO STK-MTDC STK-MTDV STK-YTDC STK-YTDV.
             MOVE SPACES         TO REP-LINE1.
             MOVE W10-MQNT       TO ORD-QUANT.
             MOVE STK-DESC       TO ORD-DESC.
    IF WS-USE-ITM = "Y"
        MOVE STK-ITEM  TO ORD-ITEM
        MOVE "/"   TO ORD-SL
        MOVE STK-ITM  TO ORD-ITM
    ELSE
              MOVE STK-CODE  TO ORD-EXT-ITEM.
    IF WS-TYPE = "B" OR "C"
        COMPUTE ORD-COST = (W10-COST / W11-QUANT(WS-S6))
               MOVE W10-COST     TO ORD-VALUE.
           IF WS-TYPE = "B" OR "S"
               MOVE STK-SELL     TO ORD-SELL.
           IF WS-TYPE = "S"
        COMPUTE ORD-VALUE = STK-SELL * W11-QUANT(WS-S6).
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM BQ05.
            MOVE SPACES  TO REP-DETAIL1.
             PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
           IF WS-S6 < 30
               ADD 1             TO WS-S6
               GO TO BQ65.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
      PERFORM CLEAR-L50.

       BQ80.
             MOVE W15-RECORD1    TO STK-RECORD1.
            COMPUTE W10-COST ROUNDED = (W10-MCOST / W10-QUANT).
            MOVE W10-COST  TO W80-5.
             PERFORM BQ32 THRU BQ33.

       BQ85.
      MOVE "ENTER to continue - E to exit"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF WS-OPTION = "E"
               GO TO BQ35.
            GO TO BQ10.

       BQ90.
      MOVE 2   TO WS-S1.

       BQ95.
    IF STK-UNT(WS-S1) = ZERO
        GO TO BQ100.
            MULTIPLY STK-UNT(WS-S1) BY STK-COST
     GIVING W10-CCOST.
            MOVE STK-SL(WS-S1)  TO W10-CSELL.
          IF STK-SL(WS-S1) < W10-CCOST
              MOVE ZERO  TO STK-MKUP(WS-S1)
    ELSE
              COMPUTE STK-MKUP(WS-S1) ROUNDED =
       ((W10-CSELL - W10-CCOST) / W10-CCOST) * 100.
            MOVE STK-CSH(WS-S1) TO W10-CSELL.
          IF STK-CSH(WS-S1) < W10-CCOST
              MOVE ZERO  TO STK-CMKUP(WS-S1)
    ELSE
              COMPUTE STK-CMKUP(WS-S1) ROUNDED =
       ((W10-CSELL - W10-CCOST) / W10-CCOST) * 100.
            MOVE STK-WSL(WS-S1) TO W10-CSELL.
          IF STK-WSL(WS-S1) < W10-CCOST
        MOVE ZERO  TO STK-WMKUP(WS-S1)
    ELSE
              COMPUTE STK-WMKUP(WS-S1) ROUNDED =
       ((W10-CSELL - W10-CCOST) / W10-CCOST) * 100.
      *
      *    ****    C H E C K   I F   E X C L U S I V E  R O U N D I N G
      *     R E Q U I R E D
      *
           IF W05-ROUND = "E"
        MOVE STK-SL(WS-S1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-SL(WS-S1)
        MOVE STK-WSL(WS-S1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-WSL(WS-S1)
        MOVE STK-CSH(WS-S1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-CSH(WS-S1).
    IF STK-TAX NOT = ZERO
        MOVE STK-TAX  TO W10-TAX
        MOVE W05-VAT (W10-TAX)
     TO W05-RATE
        COMPUTE W10-VSL(WS-S1) ROUNDED = W10-SL(WS-S1) +
     (W10-SL(WS-S1) * W05-RTE)
        COMPUTE W10-VWSL(WS-S1) ROUNDED = W10-WSL(WS-S1) +
     (W10-WSL(WS-S1) * W05-RTE)
        COMPUTE W10-VCSH(WS-S1) ROUNDED = W10-CSH(WS-S1) +
     (W10-CSH(WS-S1) * W05-RTE)
           ELSE
        MOVE W10-WSL(WS-S1)
     TO W10-VWSL(WS-S1)
        MOVE W10-CSH(WS-S1)
     TO W10-VCSH(WS-S1)
        MOVE W10-SL(WS-S1)
     TO W10-VSL(WS-S1).
           IF W05-ROUND = "I"
        MOVE W10-VSL(WS-S1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-VSL(WS-S1)
        MOVE W10-VWSL(WS-S1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-VWSL(WS-S1)
        MOVE W10-VCSH(WS-S1)
     TO W10-RSELL
               PERFORM CB100
        MOVE W10-RSELL  TO W10-VCSH(WS-S1)
        PERFORM CB300.
      MOVE W10-SL(WS-S1)  TO STK-SL(WS-S1).
      MOVE W10-CSH(WS-S1) TO STK-CSH(WS-S1).
      MOVE W10-WSL(WS-S1) TO STK-WSL(WS-S1).

       BQ100.
    IF WS-S1 < 4
        ADD 1   TO WS-S1
        GO TO BQ95.

       BQ105.
      EXIT.

       BQ999.
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
            OPEN I-O AUDIT.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS NOT = "00"
        IF NOT (WS-STATUS = "41")
           MOVE 1  TO WS-F-ERROR
           PERFORM OPEN-ERROR.
            OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
             MOVE 1              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
             MOVE PAR-COMPANY    TO W95-COMP.
             MOVE 3              TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE STK-WEEK       TO WS-WEEK.
             MOVE STK-RECORDS    TO WS-RECORDS.
             MOVE STK-PASSWORD   TO WS-PASSWORD.
             MOVE 5              TO WS-PARKEY.
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
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             GO TO ZA205.

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
               WHEN 1     MOVE W02-AUDITF   TO WS-FILE
                          MOVE WS-AUDKEY    TO WS-KEY
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
               WHEN 46    MOVE W02-STKDEX   TO WS-FILE
                          MOVE STX-KEY      TO WS-KEYX
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STP023"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
