      $set LINKCOUNT"468" GNT"STPBAR.GNT"
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
      *    ******   ********  *******   *******     ****    *******    *
      *   **    **     **     **    **  **    **   **  **   **    **   *
      *   **           **     **    **  **    **  **    **  **   **    *
      *    ******      **     *******   *******   ********  ******     *
      *         **     **     **        **    **  **    **  **   **    *
      *   **    **     **     **        **    **  **    **  **    **   *
      *    ******      **     **        *******   **    **  **    **   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S T O C K -   B A R C O D E   L A B E L S   P R I N T    *
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
       PROGRAM-ID.     STPBAR.
       AUTHOR.         J W LEMMON (CSS).
       DATE-WRITTEN.   April 1995.

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

       COPY "BARCOD.SL".

       COPY "CONTROL.SL".

       COPY "DEPART.SL".

       COPY "PARAM.SL".

       COPY "STOCK.SL".

          SELECT PRNREP  ASSIGN W02-PRINTER
                          ORGANIZATION LINE SEQUENTIAL.

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

       COPY "BARCOD.FD".

       COPY "CONTROL.FDE".

       COPY "DEPART.FDE".

       COPY "PARAM.FDE".

       COPY "STOCK.FDE".

       FD  PRNREP    LABEL RECORD OMITTED.
       01  REP-L1.
          03  REP-DET.
        05  REP-CONT    PIC  X(12).
        05  REP-CODE    PIC  X(14).
        05  FILLER      PIC  X(24).
       01  REP-L2.
    03  REP-CHAR        PIC  X(01) OCCURS 50.
       01  REP-L3.
    03  FILLER        PIC  X(15).
    03  REP-DTE        PIC  Z9/99.
    03  FILLER        PIC  X(01).
    03  REP-PRICE       PIC  ZZ9.99.
    03  FILLER        PIC  X(23).
       01  REP-L4.
    03  REP-CIT-CONT    PIC  X(12).
    03  REP-CIT-BCDE    PIC  X(38).
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
       77  WS-SUB    PIC 9(04)  COMP-5.
       77  WS-S1    PIC 9(04)  COMP-5.
       77  WS-S2    PIC 9(04)  COMP-5.
       77  WS-S3    PIC 9(04)  COMP-5.
       77  WS-S4    PIC 9(04)  COMP-5.
       77  WS-S5    PIC 9(04)  COMP-5.
       77  WS-S6    PIC 9(04)  COMP-5.
       77  WS-S7    PIC 9(04)  COMP-5.
       77  WS-S8    PIC 9(04)  COMP-5.
       77  WS-IXS1         PIC  9(04)    COMP-5.
       77  WS-PARKEY       PIC  9(04)    COMP-5.
       77  WS-NETKEY       PIC  9(04)    COMP-5.
       77  WS-PAGE    PIC 9(04)  COMP-5 VALUE ZERO.
       77  WS-OPTION    PIC X(01).
       77  WS-SFILE    PIC X(01) VALUE "N".
       77  WS-SELECT    PIC X(01).
       77  WS-PRINTER-TYPE PIC X(01) VALUE "E".
       77  WS-PRINTER-NAME PIC X(20).
       77  WS-WHICH-CODE   PIC X(01).
    88  USE-XREF    VALUE "X".
    88  USE-ITEM    VALUE "I".
       77  WS-BC-HEIGHT    PIC X(01).
       77  WS-BC-STYLE    PIC X(01).
       77  WS-USE-ITM    PIC X(01).
       77  WS-EXT-STK    PIC X(01).
       77  WS-SOH    PIC X(01) VALUE X"01".
       77  WS-STX    PIC X(01) VALUE X"02".
       77  WS-ERROR    PIC 9(01).
       77  WS-ETYPE        PIC  X(01) VALUE "I".
       77  WS-PRN-NO    PIC X(01)  COMP-X VALUE 9.
       77  WS-PRN-STAT    PIC X(01)  COMP-X.
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STPBAR".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY -PRINT BAR CODE LABELS".
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
    03  WS-PROG        PIC  X(03) VALUE "BAR".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-FILE-IDS.
    03  W02-PRINTER     PIC X(04) VALUE "COM1".

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

       01  W15-LABELS.
    03  W15-ITEM        PIC  X(18).
    03  W15-CODE        PIC  X(14).
          03  W15-DESC.
        05  W15-DSC     PIC  X(17).
        05  W15-DSC2    PIC  X(13).
    03  W15-QNT        PIC  9(04).
    03  W15-PRICE       PIC  9(03)V99 COMP.

       01  W16-ITEM.
    03  W16-CODE.
        05  W16-CHAR    PIC  X(01) OCCURS 14.
    03  W16-LENGTH.
        05  W16-COUNT   PIC  X(01) COMP-X.

       01  W20-PARAMETERS.
    03  W20-PIXELS      PIC  9(02).
    03  W20-LENGTH      PIC  9(04).
      *      ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      *
      *       Inserted 21/09/2001        (W25-LABELS)
      *
      *      ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
       01  W25-LABELS.
      *
      *        ****   N a r r o w   B a r   W i d t h
      *
    03  W25-NAR        PIC  9(02)    COMP-5.
      *
      *        ****   W i d e B a r W i d t h
      *
    03  W25-WIDE        PIC  9(02)    COMP-5.
      *
      *        ****   Y   A X I S
      *
    03  W25-COLUMN      PIC  9(04)    COMP-5.
      *
      *        ****   X   A X I S
      *
    03  W25-ROW        PIC  9(04)    COMP-5.
      *
      *        ****   B a r   C o d e H e i g h t
      *
    03  W25-BC-HGHT     PIC  9(02)    COMP-5.
      *
      *        ****   F o n t S e l e c t i o n
      *
    03  W25-FONT        PIC  9(01).
      *
      *        ****   Y   A X I S
      *
    03  W25-F-COLUMN    PIC  9(04)    COMP-5.
      *
      *        ****   X   A X I S
      *
    03  W25-F-ROW       PIC  9(04)    COMP-5.
      *
      *        ****   Y   A X I S
      *
    03  W25-F-COL2      PIC  9(04)    COMP-5.
      *
      *        ****   X   A X I S
      *
    03  W25-F-ROW2      PIC  9(04)    COMP-5.
      *
      *        ****   S P L I T   D E S C R I P T I O N
      *
    03  W25-SPLIT       PIC  X(01).
      *
      *    ****   C I T I Z E N   C L P   S E R I E S L A B E L   &
      *    B A R   C O D E   P R I N T E R S
      *
      *    ****   T H E   C O N T R O L S   A N D   P A R A M E T E R S
      *    A R E   C U R R E N T L Y   S T A T I C.
      *
      *    Only the bar code type, the bar code, the description,
      *    the price and the number of labels will be changed.
      *
       01  W30-LABELS.
    03  W30-SETUP.
        05  W30-STX1    PIC  X(01) VALUE X"02".
        05  W30-UNIT    PIC  X(01) VALUE "m".
        05  W30-STX2    PIC  X(01) VALUE X"02".
        05  W30-M-LNGTH PIC  9(04) VALUE 350.
        05  W30-STX3    PIC  X(01) VALUE X"02".
        05  W30-L-FMT   PIC  X(01) VALUE "L".
        05  W30-PIXELS  PIC  X(01) VALUE "D".
        05  W30-HOR     PIC  9(01) VALUE 1.
        05  W30-VER     PIC  9(01) VALUE 1.

    03  W30-BAR-CODE.
        05  W30-ANGLE   PIC  9(01) VALUE 1.
        05  W30-BC-TYPE PIC  X(01).
        05  W30-W-BAR   PIC  9(01) VALUE 2.
        05  W30-N-BAR   PIC  9(01) VALUE 2.
        05  W30-HEIGHT  PIC  9(03) VALUE 080.
        05  W30-ROW     PIC  9(04) VALUE 0004.
        05  W30-COL     PIC  9(04) VALUE 0045.
        05  W30-BAR-CDE PIC  X(14).

    03  W30-DESCRIPTION.
        05  W30-ANG2    PIC  9(01) VALUE 1.
        05  W30-FONT    PIC  9(01) VALUE 2.
        05  W30-HOR2    PIC  9(01) VALUE 1.
        05  W30-VER2    PIC  9(01) VALUE 1.
        05  W30-EXP     PIC  9(03) VALUE 000.
        05  W30-ROW2    PIC  9(04) VALUE 0160.
        05  W30-COL2    PIC  9(04) VALUE 0000.
        05  W30-DESC.
     07  W30-SD  PIC  X(17).
     07  FILLER  PIC  X(13).

    03  W30-DESCRIPTION2.
        05  W30-AN2     PIC  9(01) VALUE 1.
        05  W30-FNT     PIC  9(01) VALUE 2.
        05  W30-HR2     PIC  9(01) VALUE 1.
        05  W30-VR2     PIC  9(01) VALUE 1.
        05  W30-EX      PIC  9(03) VALUE 000.
        05  W30-RW2     PIC  9(04) VALUE 0125.
        05  W30-CL2     PIC  9(04) VALUE 0000.
        05  W30-DESC2   PIC  X(17).

    03  W30-PRICE.
        05  W30-ANG3    PIC  9(01) VALUE 4.
        05  W30-FONT3   PIC  9(01) VALUE 2.
        05  W30-HOR3    PIC  9(01) VALUE 1.
        05  W30-VER3    PIC  9(01) VALUE 1.
        05  W30-EXP3    PIC  9(03) VALUE 000.
        05  W30-ROW3    PIC  9(04) VALUE 0010.
        05  W30-COL3    PIC  9(04) VALUE 0350.
        05  W30-VALUE   PIC  ZZZ9.99.

    03  W30-PRICE2.
        05  W30-ANG4    PIC  9(01) VALUE 4.
        05  W30-FONT4   PIC  9(01) VALUE 3.
        05  W30-HOR4    PIC  9(01) VALUE 1.
        05  W30-VER4    PIC  9(01) VALUE 1.
        05  W30-EXP4    PIC  9(03) VALUE 000.
        05  W30-ROW4    PIC  9(04) VALUE 0005.
        05  W30-COL4    PIC  9(04) VALUE 0350.
        05  FILLER      PIC  X(01) VALUE "R".
        05  W30-VALUE2  PIC  Z9.99.

    03  W30-PRINT.
        05  W30-STX4    PIC  X(01) VALUE X"02".
        05  W30-SETNO   PIC  X(01) VALUE "E".
        05  W30-NUMBER  PIC  9(04).
        05  W30-STX5    PIC  X(01) VALUE X"02".
        05  W30-PROCEED PIC  X(01) VALUE "G".

      *
      *    ****   E L T R O N T L P &   L P +   S E R I E S
      *    L A B E L   &   B A R   C O D E   P R I N T E R S
      *
       01  W32-LABELS.
    03  W32-SETUP.
        05  W32-CLEAR   PIC  X(01) VALUE "N".

    03  W32-DESCRIPTION.
        05  W32-CMND    PIC  X(01) VALUE "A".
        05  W32-COL     PIC  9(03) VALUE 220.
        05  W32-ROW     PIC  9(02) VALUE 10.
        05  W32-ANG     PIC  9(01) VALUE 0.
        05  W32-FONT    PIC  9(01) VALUE 3.
        05  W32-HOR     PIC  9(01) VALUE 1.
        05  W32-VER     PIC  9(01) VALUE 1.
        05  W32-IMG     PIC  X(01) VALUE "N".
        05  W32-DESC.
     07  W32-SD  PIC  X(17).
     07  FILLER  PIC  X(13).

    03  W32-DESCRIPTION2.
        05  W32-CMD     PIC  X(01) VALUE "A".
        05  W32-CL2     PIC  9(03) VALUE 220.
        05  W32-RW2     PIC  9(02) VALUE 10.
        05  W32-ANG2    PIC  9(01) VALUE 0.
        05  W32-FNT     PIC  9(01) VALUE 3.
        05  W32-HR2     PIC  9(01) VALUE 1.
        05  W32-VR2     PIC  9(01) VALUE 1.
        05  W32-IM2     PIC  X(01) VALUE "N".
        05  W32-DESC2   PIC  X(17).

    03  W32-BAR-CODE.
        05  W32-CMND2   PIC  X(01) VALUE "B".
        05  W32-COL2    PIC  9(03) VALUE 220.
        05  W32-ROW2    PIC  9(02) VALUE 40.
        05  W32-ANG3    PIC  9(01) VALUE 0.
        05  W32-BC-TYPE PIC  X(03).
        05  W32-W-BAR   PIC  9(01) VALUE 2.
        05  W32-N-BAR   PIC  9(01) VALUE 2.
        05  W32-HEIGHT  PIC  9(03) VALUE 080.
        05  W32-PRN-CDE PIC  X(01) VALUE "B".
        05  W32-BAR-CDE PIC  X(14).

    03  W32-PRINT.
        05  W32-CMD3    PIC  X(01) VALUE "P".
        05  W32-NUMBER  PIC  9(04).

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       COPY "W75.STK".

      *
      *    ****   T A B L E   F O R   P R I C E   C H E C K   D I G I T
      *    C A L C U L A T I O N S
      *
       01  W80-CHECK-DIGIT.
    03  W80-VALUES.
        05  FILLER PIC  9(09) VALUE 516273849.
        05  FILLER PIC  9(09) VALUE 594837261.
        05  FILLER PIC  9(09) VALUE 246891357.
    03  W80-TABLE-VALUES REDEFINES W80-VALUES.
        05  W80-5P PIC  9(01) OCCURS 9.
        05  W80-5N PIC  9(01) OCCURS 9.
        05  W80-2N PIC  9(01) OCCURS 9.
    03  W80-SUB  PIC  9(02) COMP-5.
    03  W80-RESULT PIC  9(02).
    03  W80-RES-DIG REDEFINES W80-RESULT.
        05  W80-R-DIG PIC  9(01) OCCURS 2.
    03  W80-COMP REDEFINES W80-RESULT.
        05  W80-CD PIC  9(01).
        05  W80-UNIT PIC  9(01).
    03  W80-WEIGHTS.
        05  W80-WGHTS PIC  9(05).
    03  W80-WGHT REDEFINES W80-WEIGHTS.
        05  W80-WEIGHT PIC  9(01) OCCURS 5.
    03  W80-BAR-CODE.
        05  W80-XREF.
        07  W80-GROUP PIC  9(02).
        07  W80-ITM-CODE PIC  9(04).
        05  W80-PRC-CD PIC  9(01).
        05  W80-PRICE PIC  9(03)V99.
        05  W80-PRC-DIG REDEFINES W80-PRICE
    PIC  9(01) OCCURS 5.
        05  W80-BAR-CD PIC  9(01).
    03  W80-BAR-CODE-CH REDEFINES W80-BAR-CODE
         PIC  9(01) COMP OCCURS 13.
    03  W80-BAR-CODE-EAN13 REDEFINES W80-BAR-CODE.
        05  W80-BAR-CODE-12   PIC  9(12) COMP.
        05  W80-BAR-CODE-CHK-DIG
         PIC  9(01) COMP.
    03  W80-BAR-CODE-RESULT-1 PIC  9(04) COMP.
    03  W80-BAR-CODE-RESULT-2 PIC  9(04) COMP.
    03  W80-BAR-CODE-RESULT-3 PIC  9(04) COMP.
    03  W80-BAR-CODE-CALC REDEFINES W80-BAR-CODE-RESULT-3.
        05  W80-BAR-CODE-TEN  PIC  9(03) COMP.
        05  W80-BAR-CODE-UNIT PIC  9(01) COMP.

       01  W95-COMP        PIC X(40).

       01  W100-EDIT.
          03  W100-V2.
               05  W100-S6V2   PIC Z(05)9.99-.
          03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
          03  W100-V4 REDEFINES W100-V2.
               05  W100-EXT    PIC Z(06)9.99.
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

       01  S-PROG.
          03  LINE  2 COLUMN 33 FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "BARCODE PRINTING".

       01  S01.
    03  LINE  4 COLUMN 17 FOREGROUND-COLOR Green HIGHLIGHT
     BACKGROUND-COLOR Green VALUE
       "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
    03        COLUMN 62 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "¿".
    03  LINE  5 COLUMN 17 FOREGROUND-COLOR Green HIGHLIGHT
     BACKGROUND-COLOR Green VALUE
       "³     ".
    03        COLUMN 23 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Green VALUE
       "S E L E C T   S T O C K   C O D E      ".
    03        COLUMN 62 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³".
    03        COLUMN 63 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Black VALUE "°°".
    03  LINE  6 COLUMN 17 FOREGROUND-COLOR Green HIGHLIGHT
     BACKGROUND-COLOR Green VALUE
       "³     ".
    03        COLUMN 23 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Green VALUE
       "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ      ".
    03        COLUMN 62 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³".
    03        COLUMN 63 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Black VALUE "°°".
    03  LINE  7 COLUMN 17 FOREGROUND-COLOR Green HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³ ".
    03        COLUMN 19 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
       "Item Code                                  ".
    03        COLUMN 62 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³".
    03        COLUMN 63 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Black VALUE "°°".
    03  LINE  8 COLUMN 17 FOREGROUND-COLOR Green HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³ ".
    03        COLUMN 19 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
       "Description                                ".
    03        COLUMN 62 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³".
    03        COLUMN 63 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Black VALUE "°°".
    03  LINE  9 COLUMN 17 FOREGROUND-COLOR Green HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³ ".
    03        COLUMN 19 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
       "Barcode                                    ".
    03        COLUMN 62 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³".
    03        COLUMN 63 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Black VALUE "°°".
    03  LINE 10 COLUMN 17 FOREGROUND-COLOR Green HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³ ".
    03        COLUMN 19 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
       "Quantity                                   ".
    03        COLUMN 62 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³".
    03        COLUMN 63 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Black VALUE "°°".
    03  LINE 11 COLUMN 17 FOREGROUND-COLOR Green HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³ ".
    03        COLUMN 19 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE
       "Price                                      ".
    03        COLUMN 62 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³".
    03        COLUMN 63 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Black VALUE "°°".
    03  LINE 12 COLUMN 17 FOREGROUND-COLOR Green HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "À".
    03        COLUMN 18 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green VALUE
         "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
    03        COLUMN 63 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Black VALUE "°°".
    03  LINE 13 COLUMN 19 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Black VALUE
         "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°".

       01  S02.
    03  LINE  6 COLUMN 29 FOREGROUND-COLOR Green HIGHLIGHT
     BACKGROUND-COLOR Green VALUE
     "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
    03        COLUMN 52 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "¿".
    03  LINE  7 COLUMN 29 FOREGROUND-COLOR Green HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³".
    03        COLUMN 30 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Green VALUE
     "SELECT BARCODE PRINTER".
    03        COLUMN 52 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³".
    03  LINE  8 COLUMN 29 FOREGROUND-COLOR Green HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³ ".
    03        COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "C".
    03        COLUMN 32 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green
     VALUE " - Citizen CLP Series".
    03        COLUMN 52 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³".
    03  LINE  9 COLUMN 29 FOREGROUND-COLOR Green HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³  ".
    03        COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "E".
    03        COLUMN 32 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green
     VALUE " - Epson TLP Series  ".
    03        COLUMN 52 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³".
    03  LINE 10 COLUMN 29 FOREGROUND-COLOR Green HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³ ".
    03        COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "L".
    03        COLUMN 32 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green
     VALUE " - Eltron 2642/3642  ".
    03        COLUMN 52 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³".
    03  LINE 11 COLUMN 29 FOREGROUND-COLOR Green HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "À".
    03        COLUMN 30 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green VALUE
     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S02B.
    03  LINE  6 COLUMN 29 FOREGROUND-COLOR Green HIGHLIGHT
     BACKGROUND-COLOR Green VALUE
     "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
    03        COLUMN 52 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "¿".
    03  LINE  7 COLUMN 29 FOREGROUND-COLOR Green HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³".
    03        COLUMN 30 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Green VALUE
     " SELECT PRINTER PORT  ".
    03        COLUMN 52 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³".
    03  LINE  8 COLUMN 29 FOREGROUND-COLOR Green HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³  ".
    03        COLUMN 32 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "1".
    03        COLUMN 33 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE " = LPT1  ".
    03        COLUMN 42 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "5".
    03        COLUMN 43 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE " = COM1  ".
    03        COLUMN 52 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³".
    03  LINE  9 COLUMN 29 FOREGROUND-COLOR Green HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³  ".
    03        COLUMN 32 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "2".
    03        COLUMN 33 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE " = LPT2  ".
    03        COLUMN 42 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "6".
    03        COLUMN 43 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE " = COM2  ".
    03        COLUMN 52 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³".
    03  LINE 10 COLUMN 29 FOREGROUND-COLOR Green HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³  ".
    03        COLUMN 32 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "3".
    03        COLUMN 33 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE " = LPT3  ".
    03        COLUMN 42 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "7".
    03        COLUMN 43 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE " = COM3  ".
    03        COLUMN 52 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³".
    03  LINE 11 COLUMN 29 FOREGROUND-COLOR Green HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³  ".
    03        COLUMN 32 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "4".
    03        COLUMN 33 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE " = LPT4  ".
    03        COLUMN 42 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "8".
    03        COLUMN 43 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Green VALUE " = COM4  ".
    03        COLUMN 52 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "³".
    03  LINE 12 COLUMN 29 FOREGROUND-COLOR Green HIGHLIGHT
     BACKGROUND-COLOR Green VALUE "À".
    03        COLUMN 30 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green VALUE
     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S07.
          03  LINE  7 COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC X(14) USING W10-ITEM AUTO.

       01  S07-ITM.
          03  LINE  7 COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC X(14) USING W10-ITEM AUTO.
    03        COLUMN 46 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC X(03) USING W10-ITM AUTO.

       01  S07-EXT.
          03  LINE  7 COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC X(18) USING W10-EXT-ITEM AUTO.

       01  S07-3.
          03  LINE  7 COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC Z(03) USING W10-I4 AUTO.

       01  S07-4.
          03  LINE  7 COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC Z(04) USING W10-I4 AUTO.

       01  S07-5.
          03  LINE  7 COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC Z(05) USING W10-I5 AUTO.

       01  S07-6.
          03  LINE  7 COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC Z(06) USING W10-I6 AUTO.

       01  S07-7.
          03  LINE  7 COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC Z(07) USING W10-I7 AUTO.

       01  S07-8.
          03  LINE  7 COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC Z(08) USING W10-I8 AUTO.

       01  S07-9.
          03  LINE  7 COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC Z(09) USING W10-I9 AUTO.

       01  S07-10.
          03  LINE  7 COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC Z(10) USING W10-I10 AUTO.

       01  S07-11.
          03  LINE  7 COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC Z(11) USING W10-I11 AUTO.

       01  S07-12.
          03  LINE  7 COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC Z(12) USING W10-I12 AUTO.

       01  S07-13.
          03  LINE  7 COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC Z(13) USING W10-I13 AUTO.

       01  S07-14.
          03  LINE  7 COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC Z(14) USING W10-I14 AUTO.

       01  S08.
    03  LINE  8 COLUMN 31 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(30) FROM W15-DESC.
    03  S08-BC.
    05  LINE  9 COLUMN 31 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(13) FROM W15-CODE.
    03  S08-Q.
    05  LINE 10 COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  ZZZ9 USING W15-QNT AUTO.
    03  LINE 11 COLUMN 31 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZZZZZZZ9.99 FROM W10-VSELL.

       01  S09.
    05  LINE  9 COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(13) USING W15-CODE.

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
          IF LS0-STLEV < 1
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
        GO TO AA49.
            PERFORM ZA000-INIT.
      PERFORM BA000.

       AA49.
      EXIT PROGRAM.

       COPY "AA50.LUP".

       COPY "AA300.STK".

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

       COPY "APAC.HLP".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000              SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "DEPART.RD".

       COPY "STOCK.RD".

      *
      *   R E W R I T E   &    W R I T E   F I L E S   R O U T I N E S
      *
       AD000-WRITE             SECTION.

       COPY "STOCK.WR".

       BA000        SECTION.
       BA00.
      PERFORM ERASE-SCREEN.
      DISPLAY S-PROG.

       BA05.
      MOVE "'M'anual, 'P're-selected or 'E'xit  [ ]"
     TO WS-ERR-MES.
      MOVE " "   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "M" OR "P" OR "E")
        GO TO BA05.
      MOVE WS-OPTION  TO WS-SELECT.
    IF WS-SELECT = "E"
        GO TO BA999.
    IF (WS-SFILE = "N") AND (WS-SELECT = "P")
        MOVE "No Pre_selected items on file"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BA05.
      MOVE 7   TO SHADE-ROW.
      MOVE 31   TO SHADE-COL.
      MOVE 22   TO SHADE-WIDTH.
      MOVE 5   TO SHADE-LINES.
      DISPLAY S02.
      PERFORM SCREEN-SHADOW.
      PERFORM SAVE-SCREEN-3.

       BA06.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BA08.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
     GO TO BA999
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO BA06
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
        IF NOT (WS-OPTION = "C" OR "E" OR "L")
     PERFORM AA900-ALARM
     GO TO BA06
        END-IF
    ELSE
        PERFORM AA900-ALARM
        GO TO BA06.
      MOVE WS-OPTION  TO WS-PRINTER-TYPE.
    IF WS-PRINTER-TYPE = "C"
        MOVE "Citizen CLP Series"
     TO WS-PRINTER-NAME
    ELSE
    IF WS-PRINTER-TYPE = "E"
        MOVE "Epson TLP Series"
     TO WS-PRINTER-NAME
    ELSE
        MOVE "Eltron LP2642/LP3642"
     TO WS-PRINTER-NAME.
      PERFORM RESTORE-SCREEN-3.
      MOVE 7   TO SHADE-ROW.
      MOVE 31   TO SHADE-COL.
      MOVE 22   TO SHADE-WIDTH.
      MOVE 6   TO SHADE-LINES.
      DISPLAY S02B.
      PERFORM SCREEN-SHADOW.
      DISPLAY WS-PRINTER-NAME AT 0430
       WITH FOREGROUND-COLOR Brown HIGHLIGHT.

       BA10.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BA15.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
     GO TO BA999
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO BA10
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION
        IF (WS-OPTION < 1 OR > 8) AND
    (WS-OPTION NOT = "X")
     PERFORM AA900-ALARM
     GO TO BA10
        END-IF
    ELSE
        PERFORM AA900-ALARM
        GO TO BA10.
    IF WS-OPTION = "1"
        MOVE ZERO  TO WS-PRN-NO
        MOVE "LPT1"  TO W02-PRINTER
    ELSE
    IF WS-OPTION = "2"
        MOVE 1   TO WS-PRN-NO
        MOVE "LPT2"  TO W02-PRINTER
    ELSE
    IF WS-OPTION = "3"
        MOVE 2   TO WS-PRN-NO
        MOVE "LPT3"  TO W02-PRINTER
    ELSE
    IF WS-OPTION = "4"
        MOVE "LPT4"  TO W02-PRINTER
    ELSE
    IF WS-OPTION = "5"
        MOVE "COM1"  TO W02-PRINTER
    ELSE
    IF WS-OPTION = "6"
        MOVE "COM2"  TO W02-PRINTER
    ELSE
    IF WS-OPTION = "7"
        MOVE "COM3"  TO W02-PRINTER
    ELSE
    IF WS-OPTION = "8"
        MOVE "COM4"  TO W02-PRINTER
    ELSE
        MOVE "BCDE"  TO W02-PRINTER.
    IF WS-PRN-NO < 3
        PERFORM CHECK-FOR-PRINTER
        IF WS-ERROR = 1
     GO TO BA999
        END-IF
    ELSE
        MOVE "Waiting for printer response"
     TO WS-ERR-MES
        PERFORM OPT-CURSOR THRU OPT-REPLY.
            OPEN OUTPUT PRNREP.
      CALL X"91" USING X91-RES X91-FUN PRNREP.
      PERFORM ERASE-SCREEN.
      DISPLAY S-PROG.
    IF WS-SELECT = "M"
        GO TO BA30.

       BA20.
      READ BARCOD.
    IF NOT (WS-STATUS = "00")
        CLOSE BARCOD
        OPEN OUTPUT BARCOD
        CLOSE BARCOD
        GO TO BA990.
      MOVE BAR-REC  TO W15-LABELS.
    IF WS-PRINTER-TYPE = "E"
        PERFORM BA25 THRU BA29
    ELSE
    IF WS-PRINTER-TYPE = "C"
        PERFORM BA200 THRU BA299
    ELSE
        PERFORM BA300 THRU BA399.
      GO TO BA20.

       BA25.
      MOVE SPACES  TO REP-L1.
    IF NOT (W15-ITEM = W15-CODE)
        MOVE W15-ITEM  TO REP-L1
        MOVE W12-DDMM  TO REP-DTE
        IF W15-PRICE > ZERO
     MOVE W15-PRICE
     TO REP-PRICE
        END-IF
        WRITE REP-L1 BEFORE 1 LINES.
      MOVE W15-DESC  TO REP-L1.
      WRITE REP-L1 BEFORE 1 LINES.
      *
      *    ****    I F  P R O C E D U R E   2  I S   U S E D  T O
      *     P R I N T   A   B A R   C O D E   T H E   N U M B E R
      *     O F  C H A R A C T E R S   M U S T  B E   S E N T
      *     T O  T H E  P R I N T E R
      *
      MOVE 14   TO W16-COUNT.
      MOVE W15-CODE  TO W16-CODE.

       BA26.
    IF W16-CHAR (W16-COUNT) = SPACE
        SUBTRACT 1  FROM W16-COUNT
        GO TO BA26.
      MOVE SPACES  TO REP-L1.
      MOVE X"1D"   TO REP-CHAR(1) .
      MOVE "h"   TO REP-CHAR(2) .
      MOVE WS-BC-HEIGHT  TO REP-CHAR(3) .
      MOVE X"1D"   TO REP-CHAR(4) .
      MOVE "f"   TO REP-CHAR(5) .
      MOVE X"00"   TO REP-CHAR(6) .
      MOVE X"1D"   TO REP-CHAR (7).
      MOVE "H"   TO REP-CHAR (8).
      MOVE X"02"   TO REP-CHAR (9).
      MOVE X"1D"   TO REP-CHAR (10).
      MOVE "k"   TO REP-CHAR (11).
    IF WS-BC-STYLE = "1"
        MOVE X"00"  TO REP-CHAR (12)
    ELSE
    IF WS-BC-STYLE = "2"
        MOVE X"01"  TO REP-CHAR (12)
    ELSE
    IF WS-BC-STYLE = "3"
        MOVE X"02"  TO REP-CHAR (12)
    ELSE
    IF WS-BC-STYLE = "4"
        MOVE X"03"  TO REP-CHAR (12)
    ELSE
    IF WS-BC-STYLE = "5"
        MOVE X"04"  TO REP-CHAR (12)
    ELSE
    IF WS-BC-STYLE = "6"
        MOVE X"05"  TO REP-CHAR (12)
    ELSE
        MOVE X"06"  TO REP-CHAR (12).
      MOVE W15-CODE  TO REP-CODE.
      MOVE 26   TO WS-S1.
      MOVE 25   TO WS-S2.

       BA28.
    IF NOT (REP-CHAR (WS-S2) = SPACE)
        MOVE X"00"  TO REP-CHAR (WS-S1)
    ELSE
        SUBTRACT 1  FROM WS-S1 WS-S2
        GO TO BA28.
      ADD 1   TO WS-S1.
      MOVE X"0C"   TO REP-CHAR (WS-S1).
      WRITE REP-L1 BEFORE 0 LINES.
      SUBTRACT 1   FROM W15-QNT.
    IF W15-QNT > ZERO
        GO TO BA25.

       BA29.
      EXIT.

       BA30.
      DISPLAY S01.
             MOVE SPACES             TO W10-DESC W10-EXT-ITEM.
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
             MOVE ZERO               TO W10-SELL.
             MOVE 1                  TO W15-QNT.

       BA35.
             PERFORM AA300.
      *    *************************************************************
      *                     G E T   S T O C K   C O D E
      *    *************************************************************
      *    
      *      Is an 18 character stock/inventory item code being used?  
      *    
           IF WS-EXT-STK = "Y"
               ACCEPT S07-EXT AT LINE LIN
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
                   ACCEPT S07-ITM AT LINE LIN
               ELSE
                   ACCEPT S07 AT LINE LIN
               END-IF
           ELSE
      *    *************************************************************
      *                         NUMERIC CODES APPLY 
      *    *************************************************************
      *    
      *     Check the length of the numeric Stock/Inventory item code 
      *     being used and allow the User to key in the code.         
      *    
               EVALUATE W10-SLNGTH
      *                     3 Digit numeric Stock/Inventory item code
                 WHEN  3   ACCEPT S07-3 AT LINE LIN
      *                     4 Digit numeric Stock/Inventory item code
                 WHEN  4   ACCEPT S07-4 AT LINE LIN
      *                     5 Digit numeric Stock/Inventory item code
                 WHEN  5   ACCEPT S07-5 AT LINE LIN
      *                     6 Digit numeric Stock/Inventory item code
                 WHEN  6   ACCEPT S07-6 AT LINE LIN
      *                     7 Digit numeric Stock/Inventory item code
                 WHEN  7   ACCEPT S07-7 AT LINE LIN
      *                     8 Digit numeric Stock/Inventory item code
                 WHEN  8   ACCEPT S07-8 AT LINE LIN
      *                     9 Digit numeric Stock/Inventory item code
                 WHEN  9   ACCEPT S07-9 AT LINE LIN
      *                    10 Digit numeric Stock/Inventory item code
                 WHEN 10   ACCEPT S07-10 AT LINE LIN
      *                    11 Digit numeric Stock/Inventory item code
                 WHEN 11   ACCEPT S07-11 AT LINE LIN
      *                    12 Digit numeric Stock/Inventory item code
                 WHEN 12   ACCEPT S07-12 AT LINE LIN
      *                    13 Digit numeric Stock/Inventory item code
                 WHEN 13   ACCEPT S07-13 AT LINE LIN
      *                    14 Digit numeric Stock/Inventory item code
                 WHEN 14   ACCEPT S07-14 AT LINE LIN
               END-EVALUATE.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
       GO TO BA990
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
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
       GO TO BA35
        END-EVALUATE
              PERFORM AA50-LOOKUP
      *
      *        *****   E X T E N D E D  C O D E   *****
      *
        IF WS-EXT-STK = "Y"
     DISPLAY S07-EXT
        ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF (W10-STCK = "A") AND
    (WS-USE-ITM = "Y")
         DISPLAY S07-ITM
     ELSE
               DISPLAY S07.

       BA40.
          IF W10-EXT-ITEM = SPACES
              GO TO BA35.

       BA45.
             PERFORM CA000.
           IF WS-F-ERROR = 22
        MOVE "Z"   TO W10-ETYPE
              PERFORM AA50-LOOKUP
      *
      *        *****   E X T E N D E D  C O D E   *****
      *
        IF WS-EXT-STK = "Y"
     DISPLAY S07-EXT
        ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF (W10-STCK = "A") AND
    (WS-USE-ITM = "Y")
         DISPLAY S07-ITM
     ELSE
               DISPLAY S07
     END-IF
        END-IF
              IF W10-EXT-ITEM = SPACES
           GO TO BA35
               ELSE
     GO TO BA45.
    IF STK-IND = 4
        MOVE "Header Record"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BA35.
    IF STK-LDG = "98  "
        MOVE 98   TO W10-CD2
        MOVE STK-CODE  TO W10-CDE2
        MOVE W10-CODE2  TO W10-ITEM
        MOVE W10-ITEM  TO STK-XREF
        MOVE STK-CASH  TO W15-PRICE
        PERFORM BA100 THRU BA199.
      *    ELSE
      *        MOVE W10-VSELL  TO W15-PRICE.
      MOVE STK-CODE  TO W15-ITEM.
    IF USE-ITEM
        MOVE STK-CODE  TO W15-CODE
    ELSE
    IF STK-XREF = SPACES
        MOVE STK-CODE  TO W15-CODE
    ELSE
        MOVE STK-XREF  TO W15-CODE.
      MOVE STK-DESC  TO W15-DESC.
            PERFORM CLEAR-L50.
      DISPLAY S08.

       BA50.
      DISPLAY "Esc" AT 5003
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Cyan
       "ape to re-key"
        WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan.
      ACCEPT S08-Q.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BA30
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO BA50
        END-EVALUATE.
    IF W15-QNT = ZERO
        GO TO BA50.
            PERFORM CLEAR-L50.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO BA50.
    IF WS-PRINTER-TYPE = "E"
        PERFORM BA25 THRU BA29
    ELSE
    IF WS-PRINTER-TYPE = "C"
        PERFORM BA200 THRU BA299
    ELSE
        PERFORM BA300 THRU BA399.
      GO TO BA30.

       BA100.
      MOVE STK-XREF  TO W80-XREF.
      MOVE STK-CASH  TO W80-PRICE.
      MOVE ZERO   TO W80-WGHTS.
    IF W80-PRC-DIG(1) > ZERO
        MOVE W80-PRC-DIG(1)
     TO W80-SUB
        MOVE W80-5P(W80-SUB)
     TO W80-WEIGHT(1).
    IF W80-PRC-DIG(2) > ZERO
        MOVE W80-PRC-DIG(2)
     TO W80-SUB
        MOVE W80-2N(W80-SUB)
     TO W80-WEIGHT(2).
    IF W80-PRC-DIG(3) > ZERO
        MOVE W80-PRC-DIG(3)
     TO W80-SUB
        MOVE W80-5N(W80-SUB)
     TO W80-WEIGHT(3).
    IF W80-PRC-DIG(4) > ZERO
        MOVE W80-PRC-DIG(4)
     TO W80-SUB
        MOVE W80-5P(W80-SUB)
     TO W80-WEIGHT(4).
    IF W80-PRC-DIG(5) > ZERO
        MOVE W80-PRC-DIG(5)
     TO W80-SUB
        MOVE W80-2N(W80-SUB)
     TO W80-WEIGHT(5).
      ADD W80-WEIGHT(1) W80-WEIGHT(2) W80-WEIGHT(3)
   W80-WEIGHT(4) W80-WEIGHT(5) GIVING W80-RESULT.
    IF W80-UNIT > ZERO
        SUBTRACT W80-UNIT FROM 10
     GIVING W80-SUB
    ELSE
        MOVE ZERO  TO W80-SUB.
    IF W80-SUB = ZERO
        MOVE ZERO  TO W80-CD
        GO TO BA110-PRC-CD-FOUND.
      MOVE 1   TO W80-CD.

       BA105-FIND-PRC-CD.
    IF W80-SUB = W80-5N(W80-CD)
        GO TO BA110-PRC-CD-FOUND.
      ADD 1   TO W80-CD.
    IF W80-CD < 9
        GO TO BA105-FIND-PRC-CD.

       BA110-PRC-CD-FOUND.
      MOVE W80-CD  TO W80-PRC-CD.
      *
      *    ****    C A L C U L A T E   T H E   B A R - C O D E
      *     C H E C K   D I G I T
      *
       BA115-CALC-CHECK-DIGIT.
      ADD W80-BAR-CODE-CH(1) ,
   W80-BAR-CODE-CH(3) ,
   W80-BAR-CODE-CH(5) ,
   W80-BAR-CODE-CH (7),
   W80-BAR-CODE-CH (9),
   W80-BAR-CODE-CH (11),
     GIVING W80-BAR-CODE-RESULT-1.
      ADD W80-BAR-CODE-CH(2) ,
   W80-BAR-CODE-CH(4) ,
   W80-BAR-CODE-CH(6) ,
   W80-BAR-CODE-CH (8),
   W80-BAR-CODE-CH (10),
   W80-BAR-CODE-CH (12)
     GIVING W80-BAR-CODE-RESULT-2.
      MULTIPLY 3   BY W80-BAR-CODE-RESULT-2.
      ADD W80-BAR-CODE-RESULT-1
   W80-BAR-CODE-RESULT-2
     GIVING W80-BAR-CODE-RESULT-3.
    IF W80-BAR-CODE-UNIT > ZERO
        SUBTRACT W80-BAR-CODE-UNIT FROM 10
     GIVING W80-BAR-CODE-CHK-DIG
    ELSE
        MOVE ZERO  TO W80-BAR-CODE-CHK-DIG.
    IF NOT (W80-BAR-CODE = STK-XREF)
        PERFORM READ-STOCK-LOCK THRU READ-STOCK-EXIT
        MOVE W80-BAR-CODE TO STK-XREF
        PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.

       BA199.
      EXIT.
      *
      *   C I T I Z E N   C P L   B A R   C O D E   P R I N T I N G
      *
      *   STX X
      *   ³   À Command (see below)
      *   ÀÄÄÄÄ [02] All commands used in this routine will start with
      *       an STX (Start of text) character.
      *
      *   COMMANDS: E9999 (number of prints for edited mode)
      *      G   Print edited format
      *      L   Set label format
      *      M9999 Maximum label length
      *      m   Change units to metric
      *      n   Change units to inches
      *
      *   9 X 9 9 999 9999 9999 XXXXXXXXXXXXXX
      *   ³ ³ ³ ³  ³   ³    ³ À Print characters (bar code)
      *   ³ ³ ³ ³  ³   ³    ÀÄÄÄÄ Column address (distance right)
      *   ³ ³ ³ ³  ³   ÀÄÄÄÄÄÄÄÄÄ Row address (distance upward)
      *   ³ ³ ³ ³  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄ Bar code height
      *   ³ ³ ³ ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ Narrow bar width
      *   ³ ³ ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ Wide bar width
      *   ³ ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ Bar code selection (see below)
      *   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ Printing angle (1=0,2=90,3=180,4=270)
      *
      *   Bar codes: A - 3 OF 9    M - UPC 2 DIG ADD
      *       B - UPC-A    N - UPC 5 DIG ADD
      *       C - UPC-E    O - CODE 93
      *       D - 12 OF 5   p - ZIP
      *       E - CODE 128 (B)   Q - UCC/EAN 128
      *       F - EAN-13    R - UCC/EAN 128 (KMART)
      *       G - EAN-8    S - UCC/EAN 128 (RANDOM)
      *       H - HIBC    T - TELEPEN
      *       I - CODEBAR   u - UPS MAXICODE
      *       J - 12 OF 5 W/BARS   v - FIM
      *       K - PLESSEY   z - PDF417
      *       L - CASECODE
      *
       BA200.
      *      ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      *
      *       Inserted 21/09/2001        (W25-LABELS)
      *
      *      ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      MOVE W25-NAR  TO W30-N-BAR.
      MOVE W25-WIDE  TO W30-W-BAR.
      MOVE W25-COLUMN  TO W30-COL.
      MOVE W25-ROW  TO W30-ROW.
      MOVE W25-BC-HGHT  TO W30-HEIGHT.
      MOVE W25-FONT  TO W30-FONT W30-FNT.
      MOVE W25-F-COLUMN  TO W30-COL2.
      MOVE W25-F-ROW  TO W30-ROW2.
      MOVE W25-F-COL2  TO W30-CL2.
      MOVE W25-F-ROW2  TO W30-RW2.
      *      ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      MOVE SPACES  TO REP-L1.
      MOVE W15-CODE  TO W30-BAR-CDE.
    IF W15-PRICE > 99.99
        MOVE W15-PRICE  TO W30-VALUE
    ELSE
        MOVE W15-PRICE  TO W30-VALUE2.
      *      ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      *
      *       Inserted 21/09/2001        (W25-LABELS)
      *
      *      ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
    IF W25-SPLIT = "Y"
        MOVE SPACES  TO W30-DESC
        MOVE W15-DSC  TO W30-SD
        MOVE W15-DSC2  TO W30-DESC2
    ELSE
        MOVE W15-DESC  TO W30-DESC.
      MOVE W30-SETUP  TO REP-CIT-CONT.
    IF WS-BC-STYLE = "1"
        MOVE "B"   TO W30-BC-TYPE
    ELSE
    IF WS-BC-STYLE = "2"
        MOVE "C"   TO W30-BC-TYPE
    ELSE
    IF WS-BC-STYLE = "3"
        MOVE "F"   TO W30-BC-TYPE
    ELSE
    IF WS-BC-STYLE = "4"
        MOVE "G"   TO W30-BC-TYPE
    ELSE
    IF WS-BC-STYLE = "5"
        MOVE 3   TO W30-W-BAR
        MOVE 1   TO W30-N-BAR
        MOVE "A"   TO W30-BC-TYPE
    ELSE
    IF WS-BC-STYLE = "6"
        MOVE 5   TO W30-W-BAR
        MOVE 2   TO W30-N-BAR
        MOVE "D"   TO W30-BC-TYPE
    ELSE
        MOVE 3   TO W30-W-BAR
        MOVE 1   TO W30-N-BAR
        MOVE "I"   TO W30-BC-TYPE.
      MOVE W30-BAR-CODE  TO REP-CIT-BCDE.
      WRITE REP-L1 BEFORE 1 LINES.
      MOVE W30-DESCRIPTION
     TO REP-L4.
      WRITE REP-L1 BEFORE 1 LINES.
      *      ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      *
      *       Inserted 21/09/2001        (W25-LABELS)
      *
      *      ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
    IF W25-SPLIT = "Y"
        MOVE W30-DESCRIPTION2
     TO REP-L4
        WRITE REP-L1 BEFORE 1 LINES.
      *    IF W15-PRICE > 99.99
      *        MOVE W30-PRICE  TO REP-L4
      *    ELSE
      *        MOVE W30-PRICE2  TO REP-L4.
      *      WRITE REP-L1 BEFORE 1 LINES.
      MOVE "X"   TO REP-L4.
      WRITE REP-L1 BEFORE 1 LINES.
      MOVE W15-QNT  TO W30-NUMBER.
      MOVE W30-PRINT  TO REP-L4.
      WRITE REP-L1 BEFORE 1 LINES.

       BA299.
      EXIT.
      *
      *   E L T R O N T L P  &  L P +   B A R   C O D E
      *   P R I N T I N G
      *
      *   X
      *   À Command (see below)
      *
      *   COMMANDS: A   Print ASCII text
      *      B   Print Bar code
      *      P   Select number of labels to print.
      *
      *   B999,99,9,XXX,9,9,999,X,"XXXXXXXXXXXXXX"
      *   ³ ³  ³  ³  ³ ³ ³  ³ ³  À Print characters (bar code)
      *   ³ ³  ³  ³  ³ ³ ³  ³ ÀÄÄÄ Print readable code
      *   ³ ³  ³  ³  ³ ³ ³  ÀÄÄÄÄÄÄ Bar code height
      *   ³ ³  ³  ³  ³ ³ ÀÄÄÄÄÄÄÄÄÄ Wide bar width
      *   ³ ³  ³  ³  ³ ÀÄÄÄÄÄÄÄÄÄÄÄ Narrow bar width
      *   ³ ³  ³  ³  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄ Bar code selection (see below)
      *   ³ ³  ³  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ Printing angle (0=0,1=90,2=180,3=270)
      *   ³ ³  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ Row address (distance down)
      *   ³ ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ Column address (distance right)
      *   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ Select bar code print
      *
      *   Bar codes: 3  - 3 OF 9     9 - CODE 93
      *       3C  - 3 OF 9 with CD   2U - UPC Inter 2 OF 5
      *       UA0 - UPC-A     UE0 - UPC-E
      *       UA2 - UPC-A 2 DIG ADD  UE2 - UPC-E 2 DIG ADD
      *       UA5 - UPC-A 5 DIG ADD  UE5 - UPC-E 5 DIG ADD
      *       0  - CODE 128 UCC     1E - UCC/EAN 128
      *       1  - CODE 128 A,B,C   P - Postnet 5,6,8 & 11
      *       E30 - EAN-13     E80 - EAN-8
      *       E32 - EAN-13 2 DIG ADD E82 - EAN-8 2 DIG ADD
      *       E35 - EAN-13 5 DIG ADD E85 - EAN-8 5 DIG ADD
      *       K  - CODEBAR     2D - Inter 2 OF 5 CD read
      *       2  - Inter 2 OF 5     2G - German Postcode
      *       2C  - Inter 2 OF 5 CD
      *
       BA300.
      MOVE W25-NAR  TO W32-N-BAR.
      MOVE W25-WIDE  TO W32-W-BAR.
      MOVE W25-COLUMN  TO W32-COL2.
      MOVE W25-ROW  TO W32-ROW2.
      MOVE W25-BC-HGHT  TO W32-HEIGHT.
      MOVE W25-FONT  TO W32-FONT.
      MOVE W25-F-COLUMN  TO W32-COL.
      MOVE W25-F-ROW  TO W32-ROW.
      MOVE SPACES  TO REP-L1.
      MOVE W15-CODE  TO W32-BAR-CDE.
      *     MOVE W15-PRICE  TO W32-VALUE.
    IF W25-SPLIT = "Y"
        MOVE SPACES  TO W32-DESC
        MOVE W15-DSC  TO W32-SD
        MOVE W15-DSC2  TO W32-DESC2
    ELSE
        MOVE W15-DESC  TO W32-DESC.
      MOVE W32-SETUP  TO REP-L1.
      WRITE REP-L1 BEFORE 1 LINES.
      MOVE SPACES  TO REP-L4.
      STRING W32-CMND DELIMITED SIZE
      W32-COL DELIMITED SIZE
      "," DELIMITED SIZE
      W32-ROW DELIMITED SIZE
      "," DELIMITED SIZE
      W32-ANG DELIMITED SIZE
      "," DELIMITED SIZE
      W32-FONT DELIMITED SIZE
      "," DELIMITED SIZE
      W32-HOR DELIMITED SIZE
      "," DELIMITED SIZE
      W32-VER DELIMITED SIZE
      "," DELIMITED SIZE
      W32-IMG DELIMITED SIZE
      "," DELIMITED SIZE
      """" DELIMITED SIZE
      W32-DESC DELIMITED SIZE
      INTO REP-L2.
      MOVE 49   TO WS-S8.

       BA305.
    IF NOT (REP-CHAR(WS-S8) = SPACE)
        ADD 1   TO WS-S8
        MOVE QUOTE  TO REP-CHAR(WS-S8)
    ELSE
        SUBTRACT 1  FROM WS-S8
        GO TO BA305.
      WRITE REP-L1 BEFORE 1 LINES.
    IF NOT (W25-SPLIT = "Y")
        GO TO BA315.
      MOVE SPACES  TO REP-L4.
      STRING W32-CMD DELIMITED SIZE
      W32-CL2 DELIMITED SIZE
      "," DELIMITED SIZE
      W32-RW2 DELIMITED SIZE
      "," DELIMITED SIZE
      W32-ANG2 DELIMITED SIZE
      "," DELIMITED SIZE
      W32-FNT DELIMITED SIZE
      "," DELIMITED SIZE
      W32-HR2 DELIMITED SIZE
      "," DELIMITED SIZE
      W32-VR2 DELIMITED SIZE
      "," DELIMITED SIZE
      W32-IM2 DELIMITED SIZE
      "," DELIMITED SIZE
      """" DELIMITED SIZE
      W32-DESC2 DELIMITED SIZE
      INTO REP-L2.
      MOVE 49   TO WS-S8.

       BA310.
    IF NOT (REP-CHAR(WS-S8) = SPACE)
        ADD 1   TO WS-S8
        MOVE QUOTE  TO REP-CHAR(WS-S8)
    ELSE
        SUBTRACT 1  FROM WS-S8
        GO TO BA310.
      WRITE REP-L1 BEFORE 1 LINES.

       BA315.
    IF WS-BC-STYLE = "1"
        MOVE "UA0"  TO W32-BC-TYPE
    ELSE
    IF WS-BC-STYLE = "2"
        MOVE "UE0"  TO W32-BC-TYPE
    ELSE
    IF WS-BC-STYLE = "3"
        MOVE "E30"  TO W32-BC-TYPE
    ELSE
    IF WS-BC-STYLE = "4"
        MOVE "E80"  TO W32-BC-TYPE
    ELSE
    IF WS-BC-STYLE = "5"
        MOVE "3"   TO W32-BC-TYPE
    ELSE
    IF WS-BC-STYLE = "6"
        MOVE "2"   TO W32-BC-TYPE
    ELSE
        MOVE "K"   TO W32-BC-TYPE.
      MOVE SPACES  TO REP-L1.
      STRING W32-CMND2 DELIMITED SIZE
      W32-COL2 DELIMITED SIZE
      "," DELIMITED SIZE
      W32-ROW2 DELIMITED SIZE
      "," DELIMITED SIZE
      W32-ANG3 DELIMITED SIZE
      "," DELIMITED SIZE
      W32-BC-TYPE DELIMITED SIZE
      "," DELIMITED SIZE
      W32-W-BAR DELIMITED SIZE
      "," DELIMITED SIZE
      W32-N-BAR DELIMITED SIZE
      "," DELIMITED SIZE
      W32-HEIGHT DELIMITED SIZE
      "," DELIMITED SIZE
      W32-PRN-CDE DELIMITED SIZE
      "," DELIMITED SIZE
      """" DELIMITED SIZE
      W32-BAR-CDE DELIMITED SIZE
      INTO REP-L2.
      MOVE 39   TO WS-S8.

       BA320.
    IF NOT (REP-CHAR(WS-S8) = SPACE)
        ADD 1   TO WS-S8
        MOVE QUOTE  TO REP-CHAR(WS-S8)
    ELSE
        SUBTRACT 1  FROM WS-S8
        GO TO BA320.
      WRITE REP-L1 BEFORE 1 LINES.
      *    IF W15-PRICE > 99.99
      *        MOVE W32-PRICE  TO REP-L4
      *    ELSE
      *        MOVE W32-PRICE2  TO REP-L4.
      *      WRITE REP-L1 BEFORE 1 LINES.
      MOVE "X"   TO REP-L4.
      WRITE REP-L1 BEFORE 1 LINES.
      MOVE W15-QNT  TO W32-NUMBER.
      MOVE W32-PRINT  TO REP-L4.
      WRITE REP-L1 BEFORE 1 LINES.

       BA399.
      EXIT.

       BA990.
      CLOSE PRNREP.

       BA999.
      EXIT.

       COPY "CA000.STK".

       COPY "CA100.STK".

       COPY "CA200.STK".

       COPY "CA500.STK".

       CHECK-FOR-PRINTER       SECTION 89.
       CHECK-PRINTER.
      MOVE ZERO   TO WS-ERROR
        WS-S1 WS-S2 WS-S3 WS-S4
        WS-S5 WS-S6 WS-S7 WS-S8.
      CALL "PC_TEST_PRINTER" USING WS-PRN-NO
       WS-PRN-STAT
        RETURNING WS-STATUS.
    IF WS-PRN-STAT > 127
        MOVE 1   TO WS-S1
        SUBTRACT 128  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 63
        MOVE 1   TO WS-S2
        SUBTRACT 64  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 31
        MOVE 1   TO WS-S3
        SUBTRACT 32  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 15
        MOVE 1   TO WS-S4
        SUBTRACT 16  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 7
        MOVE 1   TO WS-S5
        SUBTRACT 8  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 3
        MOVE 1   TO WS-S6
        SUBTRACT 4  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 1
        MOVE 1   TO WS-S7
        SUBTRACT 2  FROM WS-PRN-STAT.
      MOVE WS-PRN-STAT  TO WS-S8.
    IF WS-S3 = 0 AND
       WS-S5 = 0 AND
       WS-S8 = 0
        GO TO CHECK-PRINTER-EXIT.

       PRINTER-NOT-READY.
      MOVE "Check printer! - 'R'etry or 'A'bort  [ ] "
     TO WS-ERR-MES.
      PERFORM OPT-MESSAGE.
    IF NOT (WS-OPTION = "R" OR "A")
        GO TO PRINTER-NOT-READY.
      IF WS-OPTION = "A"
   MOVE 1   TO WS-ERROR
   GO TO CHECK-PRINTER-EXIT.
        GO TO CHECK-PRINTER.

       CHECK-PRINTER-EXIT.
      EXIT.
      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT        SECTION 9.
       ZA000-OPEN.
            MOVE LS-COMPANY  TO WS-TOP-COMP.
    IF LS-USER = LS-SYS-ID
        MOVE "SupervisorÄ"  TO WS-WRKHD
    ELSE
        MOVE "Workstation"  TO WS-WRKHD
        MOVE LS-USER    TO WS-WRKST.
      MOVE LS-PARID  TO WS-PARID.
      MOVE LS-L-OR-N  TO W02-L-OR-N.
      MOVE WS-SYS-ID  TO W02-SYSID.
      MOVE LS-TODAY-DDMMYY
     TO TODAY-DDMMYY W12-TODAY.
      MOVE LS-USUB  TO WS-USUB.
            PERFORM ERASE-SCREEN.
      *
      *    ****    S E T   U P   T H E   F U N C T I O N   K E Y S
      *
      MOVE 1   TO USER-ACTION
        USER-SETTING.
      *
      *    ESC, F1 to F10 keys
      *
      MOVE ZERO   TO USER-NUMBER.
      MOVE 11   TO USER-KEYS.
      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    PAGE-UP AND PAGE-DOWN KEYS
      *
      MOVE 53   TO USER-NUMBER.
      MOVE 2   TO USER-KEYS.
      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    A C T I V A T E   M O U S E
      *
      MOVE 64   TO MOUSE-FUNC.
      MOVE 1   TO MOUSE-PARAM.
      CALL X"AF" USING MOUSE-FUNC
         MOUSE-PARAM.
    IF MOUSE-FUNC NOT = 255
        MOVE "Y"   TO MOUSE-ATTACHED
        MOVE 66   TO MOUSE-FUNC
        MOVE 0   TO MOUSE-PARAM
        CALL X"AF" USING MOUSE-FUNC
    MOUSE-PARAM
    ELSE
        GO TO ZA00A.
      *
      *    ****    S E T   M O U S E   K E Y   T O   A C T
      *     A S  F U N C T I O N   K E Y
      *
      MOVE 3   TO USER-ACTION.
      MOVE 27   TO USER-NUMBER.
      MOVE 2   TO USER-KEYS.
      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    P A R A M E T E R   F I L E
      *
       ZA00A.
      MOVE "PARAM"  TO AFID-KEY.

       ZA00-READ-APACFIDS.
            READ APACFIDS WITH IGNORE LOCK
        KEY IS AFID-KEY.
    IF WS-STATUS = "00"
        GO TO ZA00-READ-APACFIDS-EXIT.
            STRING "Missing " DELIMITED SIZE
       AFID-KEY DELIMITED BY " "
       " file ID - Status " DELIMITED SIZE
       WS-STATUS DELIMITED SIZE
       INTO WS-ERR-MES.
      PERFORM ERROR-LENGTH THRU ERROR-EXIT.
            STOP RUN.

       ZA00-READ-APACFIDS-EXIT.
      EXIT.

       ZA00A-CONTINUE.
      MOVE AFID-PATH  TO W02-PARAM.
      MOVE "BARCOD"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-BARCOD.
      MOVE "DEPART"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-DEPART.
      MOVE "NETWORK"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-NETWORK.
      MOVE "STOCKF"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-STOCKF.
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
      MOVE PAR-BAR-CODE  TO WS-WHICH-CODE.
    IF NOT (WS-WHICH-CODE = "I")
        MOVE "X"   TO WS-WHICH-CODE.
    IF PAR-BCSIZE < 50
        MOVE 50   TO PAR-BCSIZE.
      MOVE PAR-BC-HEIGHT  TO WS-BC-HEIGHT.
      MOVE PAR-BC-STYLE  TO WS-BC-STYLE.
      MOVE PAR-USE-ITM  TO WS-USE-ITM.
      MOVE PAR-EXT-STK  TO WS-EXT-STK.
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
            MOVE 5   TO WS-PARKEY.
            PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-NAR  TO W25-NAR.
      MOVE PAR-WIDE  TO W25-WIDE.
      MOVE PAR-COLUMN  TO W25-COLUMN.
      MOVE PAR-ROW  TO W25-ROW.
      MOVE PAR-BC-HGHT  TO W25-BC-HGHT.
      MOVE PAR-FONT  TO W25-FONT.
      MOVE PAR-F-COLUMN  TO W25-F-COLUMN.
      MOVE PAR-F-ROW  TO W25-F-ROW.
      MOVE PAR-F-COL2  TO W25-F-COL2.
      MOVE PAR-F-ROW2  TO W25-F-ROW2.
      MOVE PAR-SPLIT  TO W25-SPLIT.
            OPEN I-O BARCOD.
    IF WS-STATUS = "05"
        CLOSE BARCOD
        GO TO ZA999-EXIT.
          IF RUNTIME-ERROR
               IF FLE-LIMIT
           GO TO ZA49
        END-IF
        GO TO ZA999-EXIT.
      READ BARCOD.
    IF WS-STATUS = "00"
        MOVE "Y"   TO WS-SFILE
        CLOSE BARCOD
        OPEN INPUT BARCOD.
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
               WHEN 2     MOVE W02-NETWORK  TO WS-FILE
                          MOVE WS-NETKEY    TO WS-KEY
               WHEN 7     MOVE W02-DEPART   TO WS-FILE
                          MOVE DPT-CODE     TO WS-KEYX
               WHEN 15    MOVE WS-PARID     TO WS-FILE
                          MOVE WS-PARKEY    TO WS-KEY
               WHEN 22    MOVE W02-STOCKF   TO WS-FILE
                          MOVE STK-CODE     TO WS-KEYX
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STPBAR"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
