      $set LINKCOUNT"512" GNT"STP047.GNT"
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
      *    ******   ********  *******     ****        ***   ********   *
      *   **    **     **     **    **   **  **      ****        **    *
      *   **           **     **    **  **    **    ** **       **     *
      *    ******      **     ******    **    **   **  **      **      *
      *         **     **     **        **    **  ********    **       *
      *   **    **     **     **         **  **        **     **       *
      *    ******      **     **          ****        ****    **       *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *   S T O C K   B I L L   O F   Q U A N T I T Y   P R O G R A M  *
      *                                                                *
      *     SPECIAL SPARES VERSION        WEEKLY TOTALS & X-REF        *
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
       PROGRAM-ID.     STP047.
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

       COPY "PARAM.SL".

       COPY "SPARTS.SL".

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

       COPY "DEPART.FDE".

       COPY "PARAM.FDE".

       COPY "SPARTS.FDE".

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
       77  WS-CAT    PIC 9(04)  COMP-5.
       77  WS-SCAT    PIC 9(04)  COMP-5.
       77  WS-STKKEY    PIC 9(04)  COMP-5.
       77  WS-PARKEY    PIC 9(04)  COMP-5.
       77  WS-NETKEY    PIC 9(04)  COMP-5.
       77  WS-PRN-NO    PIC X(01)  COMP-X VALUE 9.
       77  WS-PRN-STAT    PIC X(01)  COMP-X.
       77  WS-PAGE    PIC 9(04)  COMP-5.
       77  WS-PGE-LENGTH   PIC 9(02)  VALUE 66.
       77  WS-PRN-LENGTH   PIC 9(02)  VALUE 62.
       77  WS-PQNT    PIC X(01).
       77  WS-WS    PIC X(01) VALUE "N".
       77  WS-MEMO    PIC X(01) VALUE "N".
       77  WS-PRT    PIC X(01).
       77  WS-BIN    PIC X(01).
       77  WS-ETYPE    PIC X(01).
       77  WS-OPTION    PIC X(01).
       77  WS-PRICE    PIC X(01).
       77  WS-ITM    PIC X(18).
       77  WS-USE-CASES    PIC X(01).
       77  WS-USE-PACKS    PIC X(01).
       77  WS-USE-ITM    PIC X(01).
       77  WS-EXT-STK    PIC X(01).
       77  WS-CARDEX    PIC X(01).
       77  WS-NUM    PIC ZZ9.
       77  WS-ERROR    PIC 9(01).
       77  WS-KEY1    PIC 9(04)  COMP-5.
       77  WS-KSTORE    PIC 9(04)  COMP-5.
       77  WS-ORD    PIC S9(05)  COMP-3.
       77  WS-TOTAL    PIC S9(07)V99 COMP-3.
       77  WS-VALUE    PIC S9(07)V99 COMP-3.
       77  WS-CCOST    PIC S9(07)V99 COMP-3.
       77  WS-CSELL    PIC S9(07)V99 COMP-3.
       77  WS-SCOST    PIC S9(07)V99 COMP-3.
       77  WS-SSELL    PIC S9(07)V99 COMP-3.
       77  WS-TCOST    PIC S9(09)V99 COMP-3.
       77  WS-TSELL    PIC S9(09)V99 COMP-3.
       77  WS-DEPT    PIC 9(03).
       77  WS-WEEK    PIC 9(01).
       77  WS-LABCDE    PIC X(08).
       77  WS-DESC-H1    PIC X(14) VALUE "Description".
       77  WS-DESC-H2    PIC X(14) VALUE "Description 2".
       77  WS-RT-HD    PIC X(10) VALUE "-Retail   ".
       77  WS-WS-HD    PIC X(10) VALUE "-Wholesale".
       77  WS-CS-HD    PIC X(10) VALUE "-Cash sale".
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STP047".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY -BILL OF QUANTITIES".
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
    03  WS-PROG        PIC  X(03) VALUE "047".

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
              05  REP-CNO     PIC  9(03).
              05  REP-CATNO REDEFINES REP-CNO PIC 9(03).
              05  FILLER      PIC  X(01).
              05  REP-CAT     PIC  X(30).
              05  FILLER      PIC  X(36).
          03  REP-LINE9 REDEFINES W02-PRN-LINE.
              05  REP-PG      PIC  X(06).
              05  REP-P-N     PIC  Z(03)9.
              05  FILLER      PIC  X(22).
              05  REP-COMP    PIC  X(40).
              05  REP-REPORT  PIC  X(34).
              05  FILLER      PIC  X(12).
              05  REP-DAT     PIC  X(06).
              05  REP-DT      PIC  Z9/99/99.
          03  REP-LINE9A REDEFINES W02-PRN-LINE.
              05  REP-HEAD6   PIC  X(13).
              05  REP-HEAD6A  PIC  X(04).
              05  REP-HEAD7   PIC  X(25).
              05  REP-HEAD8   PIC  X(12).
              05  REP-HEAD9   PIC  X(08).
              05  REP-HEAD10  PIC  X(08).
              05  REP-HEAD11  PIC  X(09).
              05  REP-HEAD12  PIC  X(09).
              05  REP-HEAD14  PIC  X(07).
              05  REP-HEAD15  PIC  X(10).
              05  REP-HEAD16  PIC  X(07).
              05  REP-HEAD17  PIC  X(08).
              05  REP-HEAD18  PIC  X(12).
          03  REP-LINE9B REDEFINES W02-PRN-LINE.
              05  S-H1        PIC  X(15).
              05  S-H2        PIC  X(28).
              05  S-L1        PIC  X(01).
              05  S-H3        PIC  X(03).
              05  S-L2        PIC  X(01).
              05  S-H4        PIC  X(07).
              05  S-L3        PIC  X(01).
              05  S-H5        PIC  X(07).
              05  S-L4        PIC  X(01).
              05  S-H6        PIC  X(07).
              05  S-L5        PIC  X(01).
              05  S-H7        PIC  X(07).
              05  S-L6        PIC  X(01).
          03  REP-LINE9C REDEFINES W02-PRN-LINE.
              05  S-CDE       PIC  X(14).
              05  FILLER      PIC  X(01).
              05  S-DSC       PIC  X(28).
              05  FILLER      PIC  X(01).
              05  S-BN        PIC  X(03).
              05  FILLER      PIC  X(01).
              05  S-QNT       PIC  Z(04)9.9.
        05  S-QNTN REDEFINES S-QNT
          PIC  -(04)9.9.
              05  FILLER      PIC  X(25).
          03  REP-LINE11 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(13).
              05  REP-HEAD13  PIC  X(26).
              05  FILLER      PIC  X(02).
              05  REP-DCOST   PIC  Z(08)9.99.
              05  FILLER      PIC  X(02).
              05  REP-DSELL   PIC  Z(08)9.99.
              05  FILLER      PIC  X(13).
    03  PRD-LINE1 REDEFINES W02-PRN-LINE.
        05  PRD-H1      PIC  X(20).
        05  PRD-H2      PIC  X(32).
        05  PRD-H2A     PIC  X(28).
    03  PRD-LINE2 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(12).
        05  PRD-H3      PIC  X(09).
        05  PRD-H4      PIC  X(19).
        05  PRD-H5      PIC  X(32).
        05  PRD-H6      PIC  X(14).
        05  PRD-H7      PIC  X(10).
    03  PRD-LINE3 REDEFINES W02-PRN-LINE.
        05  PRD-EXT-ITEM.
     07  PRD-ITEM
          PIC  X(14).
     07  PRD-SL  PIC  X(01).
     07  PRD-ITM PIC  X(03).
        05  FILLER      PIC  X(02).
        05  PRD-DESC    PIC  X(30).
        05  FILLER      PIC  X(02).
        05  PRD-QUANT   PIC  ZZZZ9.
        05  FILLER      PIC  X(25).
    03  PRD-LINE4 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(10).
        05  PRD-QNT     PIC  ZZZZ9.9999.
        05  FILLER      PIC  X(01).
        05  PRD-EXT-ITEM2.
     07  PRD-ITEM2
          PIC  X(14).
     07  PRD-SL2 PIC  X(01).
     07  PRD-ITM2
          PIC  X(03).
        05  FILLER      PIC  X(01).
        05  PRD-DESC2   PIC  X(30).
        05  FILLER      PIC  X(01).
        05  PRD-SELL    PIC  Z(06)9.999.
        05  FILLER      PIC  X(01).
        05  PRD-TOTAL   PIC  Z(08)9.99.

       01  W05.
           03  W05-S OCCURS 300   PIC 9(04)     COMP-5.

       COPY "W05.VAT".

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

       COPY "W25.STK".

       01  W40-DATES.

           03  W40-DATE1           PIC  9(08).
           03  W40-DTE1                          REDEFINES W40-DATE1.
               05  W40-C1          PIC  9(02).
               05  W40-Y1          PIC  9(02).
               05  W40-M1          PIC  9(02).
               05  W40-D1          PIC  9(02).
           03  W40-DATE2           PIC  9(08).
           03  W40-DTE2                          REDEFINES W40-DATE2.
               05  W40-C2          PIC  9(02).
               05  W40-Y2          PIC  9(02).
               05  W40-M2          PIC  9(02).
               05  W40-D2          PIC  9(02).

       COPY "W40.WS".

       01  W44-SCREEN1.
           03  FILLER          PIC X(2000).

       01  W44-ATTRIB1.
           03  FILLER          PIC X(2000).

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       COPY W70STOCK.WS.

       01  W70-KEYS.
           03  W70-BIND        PIC 9(05)     COMP-0 VALUE ZERO.

       COPY "W75.STK".

       01  W95-COMP        PIC X(40).

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

       01  S03.
          03  LINE 16 COLUMN 26 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC X(14) FROM STK-CODE.

       01  S03-ITM.
          03  LINE 16 COLUMN 26 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC X(14) FROM STK-ITEM.
    03        COLUMN 40 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Magenta VALUE "/".
    03        COLUMN 41 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC X(03) FROM STK-ITM.

       01  S03-EXT.
          03  LINE 16 COLUMN 26 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC X(18) FROM STK-CODE.

       01  S04.
          03  LINE 10 COLUMN 20 FOREGROUND-COLOR Brown HIGHLIGHT
                                 VALUE "Busy loading stock departments".

       01  S05.
          03  LINE 12 COLUMN 23 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC X(14) USING W10-EXT-ITEM AUTO.

       01  S05-ITM.
          03  LINE 12 COLUMN 23 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC X(14) USING W10-ITEM AUTO.
    03        COLUMN 38 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC X(03) USING W10-ITM AUTO.

       01  S05-DSP.
    03  LINE 12 COLUMN 37 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Magenta VALUE "/".

       01  S05-EXT.
          03  LINE 12 COLUMN 23 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC X(18) USING W10-EXT-ITEM AUTO.

       01  S15.
           03  LINE 16 COLUMN 12 VALUE "Stock record:".

       01  S16.
           03  LINE  2 COLUMN 32 FOREGROUND-COLOR Grey HIGHLIGHT
                                 VALUE "BILL OF QUANTITIES".
          03  LINE  4 COLUMN 12 FOREGROUND-COLOR Brown HIGHLIGHT
                                 VALUE "Printing bill of quantities".
          03  LINE  6 COLUMN 12 VALUE "Are you sure (".
    03        COLUMN 26 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "Y".
    03        COLUMN 27 VALUE "/".
    03        COLUMN 28 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "N".
    03        COLUMN 29 VALUE ")?".

       01  S17.
    03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC ZZZZ9 USING W10-QUANT AUTO.

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
       AA00M   SECTION.
       AA00.
      *
      *   **** Check the User's security level for Stock/Inventory
      *
          IF LS0-STLEV < 1
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
            PERFORM ZA000-INIT.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *     **  *   *   * *   *  *  **  *   *   *     *   *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *     *  **   *     *   *  *  *  **   *   *     *  *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** *   *
      *
             PERFORM OPEN-PRINTER.
             MOVE "P"                TO WS-COMMAND.
             PERFORM GE000.
      *
      *      1 0   C P I
      *
             MOVE 1                  TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
      *
      *  ***  *     ***   **** ****   ****  ****  *** *   * ***** **** ****
      * *   * *    *   * *     *      *   * *   *  *  **  *   *   *    *   *
      * *     *    *   *  ***  ***    ****  ****   *  * * *   *   ***  ****
      * *   * *    *   *     * *      *     *   *  *  *  **   *   *    *   *
      *  ***  ****  ***  ****  ****   *     *   * *** *   *   *   **** *   *
      *
             MOVE "C"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.

       AA49.
             EXIT PROGRAM.

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
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.SCR".

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

       COPY "PRINTUTL.AP3".

       COPY "AA300.STK".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "STKMEM.RD".

       COPY "STOCK.RD".

       COPY "SPARTS.RD".

       COPY "STOCK.1ST".

       COPY "APAC.HLP".

       COPY "CA000.STK".

       COPY "CA100.STK".

       COPY "CA200.STK".

       COPY "CA500.STK".

      /
       GE000        SECTION 5.
       GE00.
             PERFORM ERASE-SCREEN.
             DISPLAY S16.
             MOVE 1              TO W10-QUANT.
             MOVE "Y"            TO WS-OPTION.

       GE02.
            ACCEPT WS-OPTION AT 0632 WITH FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "N" OR "Y")
               GO TO GE02.
           IF WS-OPTION = "N"
              GO TO GE999.
      DISPLAY "Print " AT 0812
       "C" WITH FOREGROUND-COLOR Brown HIGHLIGHT "ost, "
       "S" WITH FOREGROUND-COLOR Brown HIGHLIGHT "elling or "
       "N" WITH FOREGROUND-COLOR Brown HIGHLIGHT "o price".
      MOVE "S"   TO WS-PRICE.

       GE02A.
      ACCEPT WS-PRICE AT 0844
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
    IF NOT (WS-PRICE = "C" OR "S" OR "N")
        GO TO GE02A.
      DISPLAY "Selective report (" AT 1012
       "Y" WITH FOREGROUND-COLOR Brown HIGHLIGHT "/"
       "N" WITH FOREGROUND-COLOR Brown HIGHLIGHT ")".
             MOVE "N"            TO WS-OPTION.

       GE03.
            ACCEPT WS-OPTION AT 1035
      WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
                                           UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "N" OR "Y")
               GO TO GE03.
           IF WS-OPTION = "Y"
        PERFORM SAVE-SCREEN-3
        MOVE W43-SCREEN3  TO W44-SCREEN1
        MOVE W42-ATTRIB3  TO W44-ATTRIB1
               GO TO GE40.
             DISPLAY S15.

       GE05.
      MOVE 0   TO WS-ADVANCE.
      *
      *      1 0   C P I
      *
      MOVE 1   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE "H"   TO W02-PRN-TYPE.
             MOVE SPACES         TO REP-DETAIL1.
            MOVE "Page:"  TO REP-PAGE.
            MOVE "Date:"  TO REP-DATE.
             MOVE W95-COMP       TO REP-COMPANY.
             ADD 1               TO WS-PAGE.
             MOVE WS-PAGE        TO REP-P-NO.
             MOVE W12-TODAY      TO REP-DTE.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL1.
             MOVE "BILL OF QUANTITIES REPORT"
                                 TO REP-COMPANY.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
            MOVE "Item Code"  TO PRD-H1.
            MOVE WS-DESC-H1  TO PRD-H2.
      MOVE "Items"  TO PRD-H2A.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE 0   TO WS-ADVANCE.
      *
      *      1 2   C P I
      *
      MOVE 2   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE "Quantity"  TO PRD-H3.
            MOVE "Item Code"  TO PRD-H4.
            MOVE "Description"  TO PRD-H5.
    IF WS-PRICE = "S"
              MOVE "Sell.Price" TO PRD-H6
              MOVE "Total Sell" TO PRD-H7
    ELSE
    IF WS-PRICE = "C"
        MOVE "Cost Price" TO PRD-H6
              MOVE "Total Cost" TO PRD-H7.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE "D"   TO W02-PRN-TYPE.

       GE10.
      MOVE "!"   TO PRT-EXT-ITEM.
             PERFORM START-AT-PART-CODE THRU READ-SPARTS-EXIT.
           IF WS-F-ERROR = 21
               GO TO GE35.

       GE15.
             PERFORM READ-SPARTS-NEXT THRU READ-SPARTS-EXIT.
           IF WS-F-ERROR = 21
               GO TO GE35.
      MOVE PRT-EXT-ITEM  TO STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
    IF NOT (WS-PRINTER = "S")
        IF WS-USE-ITM = "Y"
     DISPLAY S03-ITM
        ELSE
        IF WS-EXT-STK = "Y"
     DISPLAY S03-EXT
        ELSE
           DISPLAY S03.

       GE16.
      MOVE ZERO   TO WS-ERROR.
      MOVE 0   TO WS-ADVANCE.
      *
      *      1 0   C P I
      *
      MOVE 1   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE "D"   TO W02-PRN-TYPE.
    IF WS-USE-ITM = "Y"
        MOVE STK-ITEM  TO PRD-ITEM
        MOVE "/"   TO PRD-SL
        MOVE STK-ITM  TO PRD-ITM
    ELSE
        MOVE STK-CODE  TO PRD-EXT-ITEM.
             MOVE STK-DESC       TO PRD-DESC.
             MOVE W10-QUANT      TO PRD-QUANT.
      MOVE 0   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO PRD-LINE3.
      MOVE 0   TO WS-ADVANCE.
      *
      *      1 2   C P I
      *
      MOVE 2   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE "D"   TO W02-PRN-TYPE.
      MOVE 1   TO WS-S5.
             MOVE ZERO           TO WS-CSELL WS-SSELL WS-TSELL.

       GE20.
          IF PRT-EXT-CODE(WS-S5) = SPACES
               GO TO GE25.
            MOVE PRT-EXT-CODE(WS-S5)
     TO STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
             COMPUTE W25-VALUE = PRT-QUANT (WS-S5) * W10-QUANT.
             MOVE W25-VALUE      TO PRD-QNT.
    IF WS-USE-ITM = "Y"
        MOVE STK-ITEM  TO PRD-ITEM2
        MOVE "/"   TO PRD-SL2
        MOVE STK-ITM  TO PRD-ITM2
    ELSE
        MOVE STK-CODE  TO PRD-EXT-ITEM2.
             MOVE STK-DESC       TO PRD-DESC2.
    IF WS-PRICE = "S"
        MOVE STK-SELL  TO PRD-SELL WS-CSELL
    ELSE
    IF WS-PRICE = "C"
        MOVE STK-COST  TO PRD-SELL WS-CSELL.
    IF NOT (WS-PRICE = "N")
        COMPUTE WS-SSELL = WS-CSELL * W25-VALUE
        MOVE WS-SSELL  TO PRD-TOTAL
        ADD WS-SSELL  TO WS-TSELL.
          IF W02-LINAGE < (W02-PRN-LENGTH - 2)
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        PERFORM GE05.
      MOVE SPACES  TO PRD-LINE4.
             PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        MOVE 9   TO WS-ERROR
              GO TO GE29.
           IF WS-S5 < 30
        ADD 1   TO WS-S5
               GO TO GE20.

       GE25.
    IF NOT (WS-PRICE = "N")
        MOVE ALL "Ä"  TO PRD-H7
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        MOVE SPACES  TO PRD-H7
        MOVE WS-TSELL  TO PRD-TOTAL
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        MOVE SPACES  TO PRD-H7
        MOVE ALL "Í"  TO PRD-H7.
    IF W02-LINAGE < (W02-PRN-LENGTH - 6)
        MOVE 2   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        PERFORM GE05.
      MOVE SPACES  TO PRD-LINE2.
             PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        MOVE 9   TO WS-ERROR
              GO TO GE29.

       GE29.
      EXIT.

       GE30.
             GO TO GE15.

       GE35.
            MOVE SPACES  TO PRD-LINE1.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE "****** END OF REPORT *****" TO PRD-LINE1.
      MOVE 99   TO WS-ADVANCE.
      MOVE "E"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
      MOVE "P"   TO WS-COMMAND.
            MOVE SPACES TO REP-DETAIL1.

       GE36.
             GO TO GE999.

       GE40.
      DISPLAY "Item code" AT 1212.
      MOVE SPACES  TO W10-EXT-ITEM.

       GE45.
             PERFORM AA300.
    IF WS-USE-ITM = "Y"
        DISPLAY S05-DSP
        ACCEPT S05-ITM
    ELSE
    IF WS-EXT-STK = "Y"
        ACCEPT S05-EXT
    ELSE
        ACCEPT S05.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
                     GO TO GE999
                 WHEN F1-KEY
       PERFORM HELP-ROUTINE
       GO TO GE45
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
   WHEN OTHER
                     PERFORM AA900-ALARM
                     GO TO GE45
               END-EVALUATE
        PERFORM AA50-LOOKUP
        IF WS-USE-ITM = "Y"
     DISPLAY S05-DSP
     DISPLAY S05-ITM
        ELSE
        IF WS-EXT-STK = "Y"
     DISPLAY S05-EXT
        ELSE
     DISPLAY S05
        END-IF
        IF W10-EXT-ITEM = SPACES
                   GO TO GE45.
          IF W10-EXT-ITEM = SPACE
               GO TO GE999.
      PERFORM CLEAR-L50.
             PERFORM CA000.
           IF WS-F-ERROR NOT = ZERO
        MOVE "Invalid code"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO GE45.
      DISPLAY STK-DESC AT 1239
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
    IF STK-IND = 4
        MOVE "Header Record"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO GE45.
      MOVE STK-CODE  TO PRT-EXT-ITEM.
             PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
           IF WS-F-ERROR = 21
        MOVE "This is not a produced item"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO GE45.
      DISPLAY "Quantity required" AT 1412.
             MOVE 1              TO W10-QUANT.

       GE50.
             ACCEPT S17 AT 1431.
           IF W10-QUANT = ZERO
               GO TO GE45.

       GE55.
      MOVE "'C'ontinue or 'R'ekey  [ ]"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "C" OR "R")
               GO TO GE55.
           IF WS-OPTION = "R"
               GO TO GE50.
             MOVE ZERO           TO WS-PAGE.
             PERFORM GE05.
      PERFORM GE16 THRU GE29.
    IF WS-ERROR = ZERO
        PERFORM GE35.
             MOVE 1201 TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
      MOVE W44-SCREEN1  TO W43-SCREEN3.
      MOVE W44-ATTRIB1  TO W42-ATTRIB3.
      PERFORM RESTORE-SCREEN-3.
      GO TO GE40.

       GE999.
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
             MOVE PAR-CRDX           TO WS-CARDEX.
             MOVE PAR-CASES          TO WS-USE-CASES.
             MOVE PAR-PACKS          TO WS-USE-PACKS.
             MOVE PAR-USE-ITM        TO WS-USE-ITM.
             MOVE PAR-EXT-STK        TO WS-EXT-STK.

       ZA01B.
             MOVE 6                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-WHOLESALE      TO WS-WS.
             MOVE PAR-COST-CODE      TO W30-COST-CODE.
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
            MOVE 3   TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
            MOVE STK-WEEK  TO WS-WEEK.
             MOVE STK-LABCDE     TO WS-LABCDE.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W40-DATE1 W40-DATE2.
           IF W40-M2 > 3
               SUBTRACT 3        FROM W40-M2
           ELSE
               SUBTRACT 1        FROM W40-Y2
               ADD 9             TO W40-M2.
            MOVE PAR-COMPANY  TO W95-COMP.

       ZA05.
            DISPLAY WS-BLNK78 AT 1402 WITH FOREGROUND-COLOR Cyan.
             GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             GO TO ZA205.

       ZA50.
           IF WS-F-ERROR = 22
               MOVE "STOCK file" TO WS-FILE
           ELSE
           IF WS-F-ERROR = 25
               MOVE "STOCK MEMO" TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Press any key to continue" AT 1012.
             ACCEPT WS-OPTION AT 1038 WITH FOREGROUND-COLOR Grey HIGHLIGHT.

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
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STP047"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
