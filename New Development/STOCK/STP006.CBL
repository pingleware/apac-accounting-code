      $set LINKCOUNT"512" GNT"STP006.GNT"
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
      *    ******   ********  *******     ****      ****     ******    *
      *   **    **     **     **    **   **  **    **  **   **    **   *
      *   **           **     **    **  **    **  **    **  **         *
      *    ******      **     *******   **    **  **    **  *******    *
      *         **     **     **        **    **  **    **  **    **   *
      *   **    **     **     **         **  **    **  **   **    **   *
      *    ******      **     **          ****      ****     ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *        A M E N D   T H E   S U P P L I E R   C O D E           *
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
       PROGRAM-ID.     STP006.
       AUTHOR.         J.W.LEMMON.
       DATE-WRITTEN.   SEPTEMBER 2005.

                COPYRIGHT NOTICE: COPYRIGHT (C) 2005 - 2018
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

       COPY "CONTROL.SL".

       COPY "CREDIT.SL".

       COPY "DEPART.SL".

       COPY "PARAM.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

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

       COPY "APACFIDS.FDE".

       COPY "CONTROL.FDE".

       COPY "CREDIT.FDE".

       COPY "DEPART.FDE".

       COPY "PARAM.FDE".

       COPY "RECOVER.FD".

       COPY "SHARED.FDE".

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
       77  WS-TRFKEY               PIC  9(06)    COMP-5.
       77  WS-PRCKEY               PIC  9(06)    COMP-5.
       77  WS-PARKEY               PIC  9(06)    COMP-5.
       77  WS-NETKEY               PIC  9(06)    COMP-5.
       77  WS-RECKEY               PIC  9(06)    COMP-5.
       77  WS-RECOVER              PIC  9(06)    COMP-5.
       77  WS-RECORDS              PIC  9(06)    COMP-5.
       77  WS-SHARED               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-TRANS                PIC  9(06)    COMP-5 VALUE 1.
       77  WS-STOCK                PIC  X(01).
       77  WS-KEYSTR               PIC  9(04)    COMP-5.
       77  WS-GST-RATE             PIC S9(03)V99 COMP-3.
       77  WS-MONTHS               PIC S9(01)V99 COMP-3.
       77  WS-KEY1                 PIC  9(04)    COMP-5.
       77  WS-ITM                  PIC  X(14).
       77  WS-ETYPE                PIC  X(01).
       77  WS-PAGE                 PIC  9(04)    COMP-5.
       77  WS-NORM                 PIC  X(04).
       77  WS-COND                 PIC  X(04).
       77  WS-EXPP                 PIC  X(04).
       77  WS-ECAN                 PIC  X(04).
       77  WS-10CPI                PIC  X(04).
       77  WS-12CPI                PIC  X(04).
       77  WS-17CPI                PIC  X(04).
       77  WS-OPTION               PIC  X(01).
       77  WS-SKIP                 PIC  X(01).
       77  WS-ADJ                  PIC  X(01).
       77  WS-ERROR                PIC  9(01).
       77  WS-USE-CASES            PIC  X(01).
       77  WS-USE-PACKS            PIC  X(01).
       77  WS-USE-ITM              PIC  X(01).
       77  WS-EXT-STK              PIC  X(01).
       77  WS-IND                  PIC  9(01)    VALUE 0.
       77  WS-INDB                 PIC  9(01)    VALUE 0.
       77  WS-INDP                 PIC  9(01)    VALUE 0.
       77  WS-INDS                 PIC  9(01)    VALUE 0.
       77  WS-AMEND                PIC  9(01)    VALUE 0.
       77  WS-CONREC               PIC  9(01)    VALUE ZERO.
       77  WS-LEDG                 PIC  X(04).
       77  WS-LEDG1                PIC  X(04).
       77  WS-COMP                 PIC  9(01).
       77  WS-WEEK                 PIC  9(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-BATCH                PIC  9(05).
       77  WS-PASSWORD             PIC  X(08).
       77  WS-PRINT                PIC  9(01).
       77  WS-LABCDE               PIC  X(08).
       77  WS-PASS                 PIC  X(08).
       77  WS-DESC-H1              PIC  X(14)    VALUE "Description".
       77  WS-DESC-H2              PIC  X(14)    VALUE "Description 2".
       77  WS-RT-HD                PIC  X(10)    VALUE "-Retail   ".
       77  WS-WS-HD                PIC  X(10)    VALUE "-Wholesale".
       77  WS-CS-HD                PIC  X(10)    VALUE "-Cash sale".
       77  WS-NODEP                PIC  X(30)    VALUE "NO SUCH - D E P A R T M E N T".
       77  PRG-CRED-LUP            PIC  X(12)    VALUE "DTP\CRPLOOK".
       77  PRG-STOCK-LUP           PIC  X(12)    VALUE "STP\STPLOOK".
       77  PRG-NAME                PIC  X(12)    VALUE SPACES.
       77  WS-WHSE                 PIC  X(06).
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STP006".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY AMEND SUPPLIER CODE".
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
    03  WS-PROG        PIC  X(03) VALUE "005".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

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
           03  W09-ST-MARKUPS                      REDEFINES W09-S-MARKUPS.
               05  W09-STK-MARK                    OCCURS 4.
                   07  W09-MKUP    PIC S9(03)V9999 COMP-3.
                   07  W09-CMKUP   PIC S9(03)V9999 COMP-3.
                   07  W09-WMKUP   PIC S9(03)V9999 COMP-3.

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

      * COPY "W15.STK".

       01  W15-INPUT-AREA.
           03  W15-DATE            PIC  9(08).
           03  W15-DTE                           REDEFINES W15-DATE.
               05  W15-DATE-CHAR   PIC  X(01)    OCCURS 8.
           03  W15-DATE1                         REDEFINES W15-DATE.
               05  W15-DD          PIC  9(02).
               05  W15-MM          PIC  9(02).
               05  W15-CC          PIC  9(02).
               05  W15-YY          PIC  9(02).
           03  W15-DATE2                         REDEFINES W15-DATE.
               05  FILLER          PIC  9(04).
               05  W15-CCYY        PIC  9(04).
           03  W15-ACNO            PIC  X(06).
           03  W15-SNO                           REDEFINES W15-ACNO.
               05  W15-APREF       PIC  X(01).
               05  W15-SERIAL      PIC  X(05).
           03  W15-NAME.           
               05  W15-NCHAR       PIC  X(01)    OCCURS 40.
           03  W15-N                             REDEFINES W15-NAME.
               05  W15-NIX.
                   07  W15-ENTRY   PIC  X(08).
                   07  FILLER      PIC  X(16).
               05  FILLER          PIC  X(16).
           03  W15-ADDRESS         PIC  X(120).
           03  W15-ADD                           REDEFINES W15-ADDRESS.
               05  W15-ADDR1       PIC  X(30).
               05  W15-ADDR2       PIC  X(30).
               05  W15-ADDR3       PIC  X(30).
               05  W15-ADDR4       PIC  X(30).
           03  W15-ADD2                          REDEFINES W15-ADDRESS.
               05  W15-ACHAR       PIC  X(01)    OCCURS 120.
           03  W15-PCODE           PIC  9(04).
           03  W15-CONTACT         PIC  X(30).
           03  W15-STYLE           PIC  X(08).
           03  W15-TEL             PIC  X(14).
           03  W15-FAX             PIC  X(14).
           03  W15-TAX             PIC  X(01).
           03  W15-TERMS           PIC  X(01).
           03  W15-UN              PIC  9(05)V99.
           03  W15-UNIT                          REDEFINES W15-UN.
               05  FILLER          PIC  9(01).
               05  W15-UNT         PIC  9(04).
               05  W15-UDEC        PIC  9(02).
           03  W15-ORD             PIC  9(10).
           03  W15-ONO                           REDEFINES W15-ORD.
               05  W15-OPREF       PIC  9(08).
               05  W15-OSUB        PIC  9(02).
          03  W15-DESCR.
              05  W15-DESC         PIC  X(18).
              05  FILLER           PIC  X(12).
           03  W15-CURRENCY        PIC  X(10).
           03  W15-OCOST           PIC S9(09)V999 COMP-3.
           03  W15-RATE            PIC S9(03)V999 COMP-3.
           03  W15-COST            PIC S9(09)V999 COMP-3.
           03  W15-SELL            PIC S9(09)V999 COMP-3.
           03  W15-SETT            PIC  9(06).
           03  W15-SET                           REDEFINES W15-SETT.
               05  W15-30          PIC  9(01).
               05  W15-60          PIC  9(01).
               05  W15-90          PIC  9(01).
               05  W15-120         PIC  9(01).
               05  W15-150         PIC  9(01).
               05  W15-180         PIC  9(01).
           03  W15-PAY                           REDEFINES W15-SETT.
               05  W15-MONTH       PIC  9(01)    OCCURS 6.
           03  W15-SET-SHIFT                     REDEFINES W15-SETT.
               05  W15-F5          PIC  9(05).
               05  FILLER          PIC  9(01).
           03  W15-SET-SHFT                      REDEFINES W15-SETT.
               05  FILLER          PIC  9(01).
               05  W15-L5          PIC  9(05).
           03  W15-DEL             PIC  9(08).
           03  W15-DEL-DMY                       REDEFINES W15-DEL.
               05  W15-DEL-DD      PIC  9(02).
               05  W15-DEL-MM      PIC  9(02).
               05  W15-DEL-CC      PIC  9(02).
               05  W15-DEL-YY      PIC  9(02).
           03  W15-LEDG.
               05  W15-CAT         PIC  9(02).
               05  FILLER          PIC  X(02).
           03  W15-GRNN            PIC  X(08).
           03  W15-GRND            PIC  9(08)    COMP-3.
           03  W15-INVN            PIC  X(08).
           03  W15-INVD            PIC  9(08)    COMP-3.
           03  W15-TCOST           PIC S9(09)V99 COMP-3.
           03  W15-BAL             PIC S9(09)V99 COMP-3.
           03  W15-TSELL           PIC S9(09)V99 COMP-3.
           03  W15-QUANT           PIC S9(05)V9999 COMP-3.
           03  W15-XQNT            PIC S9(09)V99 COMP-3.
           03  W15-UCOST           PIC S9(09)V999 COMP-3.
           03  W15-USELL           PIC S9(09)V999 COMP-3.
           03  W15-UCASH           PIC S9(09)V999 COMP-3.
           03  W15-UWSALE          PIC S9(09)V999 COMP-3.
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
           03  W15-RAIL            PIC S9(09)V99 COMP-3.
           03  W15-DISCOUNT        PIC S9(09)V99 COMP-3.
           03  W15-DISC1           PIC  99V999.
           03  W15-DSC1R                         REDEFINES W15-DISC1.
               05  W15-DSC1        PIC  V9(05).
           03  W15-DISC2           PIC  99V999.
           03  W15-DSC2R                         REDEFINES W15-DISC2.
               05  W15-DSC2        PIC  V9(05).
           03  W15-TAXABLE         OCCURS 12.
               05  W15-TAXVAL      PIC S9(09)V99  COMP-3.
               05  W15-VATVAL      PIC S9(09)V99  COMP-3.
           03  W15-TXBL            PIC S9(09)V999 COMP-3.
           03  W15-INCVAL          PIC S9(09)V99  COMP-3.
           03  W15-IVAT            PIC S9(07)V99  COMP-3.
           03  W15-U-VAT           PIC S9(07)V999 COMP-3.
           03  W15-U-PNETT         PIC S9(07)V999 COMP-3.
           03  W15-U-NETT          PIC S9(07)V999 COMP-3.
           03  W15-U-TXBL          PIC S9(07)V999 COMP-3.
           03  W15-U-NTXBL         PIC S9(07)V999 COMP-3.
           03  W15-U-INCVAL        PIC S9(07)V999 COMP-3.
           03  W15-VAT             PIC S9(07)V99  COMP-3.
           03  W15-TOTVAT          PIC S9(09)V99  COMP-3.
           03  W15-TOTAL           PIC S9(09)V99  COMP-3.
           03  W15-IUN             PIC S9(08)V999 COMP-3.
           03  W15-PUN             PIC S9(07)V9(04) COMP-3.
           03  W15-AVRG            PIC S9(08)V9999 COMP-3.
           03  W15-ICOST           PIC S9(08)V9999 COMP-3.
           03  W15-INCOST          PIC S9(09)V999 COMP-3.
           03  W15-ILCOST          PIC S9(09)V999 COMP-3.
           03  W15-PCOST           PIC S9(09)V999 COMP-3.
           03  W15-RMKS            PIC  X(08)    VALUE "Account".
           03  W15-PMT             PIC  X(08)    VALUE "Deliver".
           03  W15-PSLIP           PIC  X(08).
           03  W15-PMNT            PIC  X(12).
           03  W15-STM             PIC  X(01).
           03  W15-WHSE            PIC  X(06)    VALUE SPACES.
           03  W15-WQUANT          PIC S9(08)V9999 COMP-3.

       COPY "W25.STK".

       01  W30-SUPPLIER.
           03  W30-ACNO            PIC  X(06).
           03  W30-PACNO           PIC  X(06).
           03  W30-NAME.
               05  W30-NCHAR       PIC  X(01)    OCCURS 40.
           03  W30-PNAME.
               05  W30-PNCHAR      PIC  X(01)    OCCURS 40.

       01  W31-SUPPLIER.
           03  W31-ACNO            PIC  X(06).
           03  W31-PACNO           PIC  X(06).

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       COPY "W75.STK".
           03  W75-CNO                           OCCURS 18.
               05  W75-WHCDE.
                   07  W75-PKEY    PIC  9(05).
                   07  FILLER      PIC  X(01).

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
               05  W80-5N          PIC  9(05).
               05  W80-REC                       REDEFINES W80-5N
                                   PIC  ZZZZ9.
           03  W80-DEL.
               05  W80-2N          PIC  9(02).
       01  W85-PASS.
           03  W85-SUPER           PIC  X(06)    OCCURS 9.
           03  W85-ENTRY           PIC  9(02)    COMP-3.
           03  W85-MARG            PIC S9(02)V99.
       01  W90-BAL                 PIC S9(07)V99 COMP-3.
       01  W95-COMP                PIC X(40).
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

       01  S04S.
          03  LINE  2 COLUMN 32 FOREGROUND-COLOR Grey HIGHLIGHT
           VALUE "AMEND THE SUPPLIER".

       01  S04-S.
    03  LINE 19 COLUMN 18 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC X(06) USING W30-ACNO AUTO.

       01  S04-AS.
    03  LINE 20 COLUMN 18 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC X(06) USING W30-PACNO AUTO.

       01  S55.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
      *                                             1         2         3         4      
      *                                    1234567890123456789012345678901234567890123456
      *
               05  LINE 50 COLUMN  2 VALUE "F1"                                           FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN  4 VALUE   "=Help,".
               05          COLUMN 10 VALUE         "F2"                                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 12 VALUE           " to do Creditor Lookup ".
               05          COLUMN 35 VALUE                                  "Esc"         FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 38 VALUE                                     " to exit".

       COPY "STOCK.CRT".

       COPY "CRDLUP.CRT".

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
             PERFORM BA000.
             CLOSE RECOVER.

       AA49.
             EXIT PROGRAM.

       COPY "AA50CS.LUP".

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
      *    ³ The system will display the message box with the top line ³
      *    ³ as the value of WS-MES-LINE. If WS-MES-LINE is zero the   ³
      *    ³ default will be 20.                                       ³
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
      *    ****    D I S P L A Y   D E P A R T M E N T  N A M E
      *
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

       COPY "DATE.CHK".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
        AC000-READ        SECTION.

       COPY "CONTROL.RD".

       COPY "CREDIT.RD".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "SHARED.RD".

       COPY "STKMEM.RD".

       COPY "STOCK.RD".

      *
      *   R E W R I T E   &    W R I T E   F I L E S   R O U T I N E S
      *
       AD000-WRITE        SECTION.

       COPY "CONTROL.WR".

       COPY "DEPART.WR".

       COPY "PARAM.WR".

       COPY "SHARED.WR".

       COPY "STKMEM.WR".

       COPY "STOCK.WR".

       COPY "STOCK.1ST".

       COPY "APAC.HLP".

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

      /
      *    ****    A M E N D   T H E   S U P P L I E R
      *
       BA000     SECTION 50.
       BA00.
             PERFORM ERASE-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Has the User logged in to do more restrictive amendments  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF LS-OK = "N"
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO BA999.
             DISPLAY S04S.
             DISPLAY S04-N.
           IF WS-USE-ITM = "Y"
               DISPLAY "/ITM" AT 0612 "/" AT 0635.
           IF W10-ART11 = "Y"
               DISPLAY "VAT - Article 11/1g" AT 0849.
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
       BA05.
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
                              GO TO BA999
                 WHEN F1-KEY  PERFORM HELP-ROUTINE
                              GO TO BA05
                 WHEN F2-KEY  MOVE "I"    TO W10-ETYPE
                 WHEN F3-KEY  MOVE "A"    TO W10-ETYPE
                 WHEN F4-KEY  MOVE "D"    TO W10-ETYPE
                 WHEN F5-KEY  MOVE "2"    TO W10-ETYPE
                 WHEN F6-KEY  MOVE "X"    TO W10-ETYPE
                 WHEN F9-KEY  MOVE "W"    TO W10-ETYPE
                 WHEN OTHER   PERFORM AA900-ALARM
                              GO TO BA05
               END-EVALUATE
               PERFORM AA50 THRU AA59
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
                   END-IF
               ELSE
                   DISPLAY S05
               END-IF
               IF W10-EXT-ITEM = SPACES
                   GO TO BA05
           ELSE
               DISPLAY S05
           END-IF.


      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
           IF W10-EXT-ITEM = SPACE
               GO TO BA999.

       BA06.
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
               END-IF
           ELSE
               DISPLAY S05.
           IF WS-F-ERROR = 22
               MOVE "Z"              TO W10-ETYPE
               PERFORM AA50 THRU AA59
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
                   END-IF
               ELSE
                   DISPLAY S05
               END-IF
               IF W10-EXT-ITEM = SPACES
                   GO TO BA05
               ELSE
                   GO TO BA06.
           IF STK-IND = 2
               MOVE "Text Record"    TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BA00.
           IF STK-IND = 3
               MOVE "Labour Record"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BA00.
           IF STK-IND = 4
               MOVE "Header Record"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BA00.
           IF STK-IND = 8 OR 9
               MOVE "Redundant Item" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BA00.

       BA10.
             MOVE STK-CODE           TO W10-EXT-ITEM.
             MOVE STK-XREF           TO W10-EXT-CODE2.
             MOVE STK-BIN            TO W10-BIN.
             MOVE STK-DESC           TO W10-DESC.
             MOVE STK-DESC2          TO W10-DESC2.
             MOVE STK-TAX            TO W10-TAX.
             MOVE STK-IND            TO W10-IND.
             MOVE STK-QUANT          TO W10-QUANT.
             MOVE STK-PER            TO W10-PER.
             MOVE STK-PRC            TO W10-PRC.
             MOVE STK-LEVEL          TO W10-REORD.
             MOVE STK-LTIME          TO W10-LTIME.
             MOVE STK-COST           TO W10-COST.
             MOVE STK-MARKUP         TO W10-MARKUP.
             MOVE STK-CMARKUP        TO W10-CMARKUP.
             MOVE STK-WMARKUP        TO W10-WMARKUP.
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
             MOVE STK-AVGE           TO W10-AVRG.
             MOVE STK-ACODE          TO W10-ACODE.
             MOVE STK-EXEMPT         TO W10-EXEMPT.
             MOVE STK-FACT           TO W10-FACT.
             MOVE 1                  TO WS-S1.

       BA12-MKUPS.
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
               GO TO BA12-MKUPS.
           IF STK-ACT = ZERO
               MOVE SPACE            TO W10-ACTIVE
           ELSE
               MOVE STK-ACT          TO W22-DTE2
               PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT
               MOVE W22-DTE3         TO W10-ADATE.
             MOVE STK-MTDC           TO W10-MTDC.
             MOVE STK-YTDC           TO W10-YTDC.
          IF LS-OK = "N"
               MOVE ZERO             TO W10-COST W10-MARKUP W10-AVRG W10-CMARKUP W10-YTDC W10-WMARKUP W10-MTDC.
             DISPLAY S06A.
             DISPLAY "Supplier      " AT 1903.
             DISPLAY "Alternate Supp" AT 2003.
             PERFORM AA400.

       BA15.
             MOVE SPACES             TO W30-ACNO W30-PACNO W31-ACNO W31-PACNO W30-NAME W30-PNAME.
           IF NOT(STK-SUPP = SPACES OR LOW-VALUES)
               MOVE STK-SUPP         TO P-NUMBER
               PERFORM READ-CREDIT THRU READ-CREDIT-EXIT
               IF WS-F-ERROR = ZERO
                   MOVE P-NUMBER     TO W30-ACNO W31-ACNO
                   MOVE P-NAME       TO W30-NAME.
           IF NOT(STK-PSUPP = SPACES OR LOW-VALUES)
               MOVE STK-PSUPP        TO P-NUMBER
               PERFORM READ-CREDIT THRU READ-CREDIT-EXIT
               IF WS-F-ERROR = ZERO
                   MOVE P-NUMBER     TO W30-PACNO W31-PACNO
                   MOVE P-NAME       TO W30-PNAME.
             DISPLAY W30-ACNO  AT 1918 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY W30-NAME  AT 1925 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY W30-PACNO AT 2018 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY W30-PNAME AT 2025 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.

       BA20.
             DISPLAY S55.

       BA25.
             ACCEPT S04-S.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY MOVE W31-ACNO TO W30-ACNO
                              GO TO BA00
                 WHEN F1-KEY  PERFORM HELP-ROUTINE
                 WHEN F2-KEY  PERFORM AA50C THRU AA59
                              MOVE W15-ACNO TO W30-ACNO
                 WHEN OTHER   PERFORM AA900-ALARM
                              GO TO BA25
               END-EVALUATE
               DISPLAY S04-S
               IF W30-ACNO = SPACES
                   MOVE SPACES       TO W30-NAME.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W30-ACNO BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             PERFORM CLEAR-L50.
           IF NOT(W30-ACNO = SPACES)
               MOVE W30-ACNO         TO P-NUMBER
               PERFORM READ-CREDIT THRU READ-CREDIT-EXIT
               IF WS-F-ERROR = 3
                   MOVE "Invalid Supplier account number" TO WS-ERR-MES
                   PERFORM ERROR-MESSAGE
                   GO TO BA20
               END-IF
               MOVE P-NAME           TO W30-NAME.
             DISPLAY W30-ACNO AT 1918 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY W30-NAME AT 1925 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             PERFORM BA20.

       BA30.
             ACCEPT S04-AS.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY MOVE W31-PACNO TO W30-PACNO
                              GO TO BA00
                 WHEN F1-KEY  PERFORM HELP-ROUTINE
                 WHEN F2-KEY  PERFORM AA50C THRU AA59
                              MOVE W15-ACNO TO W30-PACNO
                 WHEN OTHER   PERFORM AA900-ALARM
                              GO TO BA30
               END-EVALUATE
               DISPLAY S04-S
               IF W30-PACNO = SPACES
                   MOVE SPACES       TO W30-PNAME.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W30-PACNO BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             PERFORM CLEAR-L50.
           IF NOT(W30-PACNO = SPACES)
               MOVE W30-PACNO        TO P-NUMBER
               PERFORM READ-CREDIT THRU READ-CREDIT-EXIT
               IF WS-F-ERROR = 3
                   MOVE "Invalid Alternate Supplier A/C" TO WS-ERR-MES
                   PERFORM ERROR-MESSAGE
                   GO TO BA30
               END-IF
               MOVE P-NAME  TO W30-PNAME.
             DISPLAY W30-PACNO AT 2018 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY W30-PNAME AT 2025 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO BA20.
             PERFORM AY60 THRU AY999.
             MOVE 0                  TO WS-ACTION.
             PERFORM AY05 THRU AY59.
             MOVE W30-ACNO           TO STK-SUPP.
             MOVE W30-PACNO          TO STK-PSUPP.
             PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
             PERFORM AY70 THRU AY999.
             GO TO BA00.

       BA999.
             EXIT.
      /
      *    ****    C R E D I T O R   L O O K U P   R O U T I N E
      *
      *            PERFORMED FROM AA50
      *            (P R O C E S S   F U N C T I O N   K E Y S)
      *
       CREDITOR-LOOKUP SECTION 64.

       CREDIT-LUP-SCREEN.
             PERFORM ERASE-SCREEN.
             DISPLAY S-CRD.
             MOVE SPACE          TO WS-OPTION.

       CREDIT-LUP-CLR-SCREEN.
             MOVE 0601           TO CPOS.
             PERFORM CREDIT-CRT-EDGE UNTIL CLIN = 22.
             MOVE SPACES TO W15-NAME.

       CREDIT-LUP-DATA.
             ACCEPT SA-CRD.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
             MOVE SPACES TO W15-ACNO
             GO TO CREDIT-LOOKUP-EXIT
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO CREDIT-LUP-DATA.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
      DISPLAY SPACE AT 5001
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT.
      MOVE 6   TO CLIN.
      MOVE 1   TO W75-S W75-S1.

       CREDIT-LUP-TAB-LOOP.
             MOVE SPACES         TO W75-CNO (W75-S1).
           IF W75-S1 < 18
               ADD 1             TO W75-S1
               GO TO CREDIT-LUP-TAB-LOOP.
             MOVE ZERO           TO W75-S1.

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
               SUBTRACT 1        FROM W75-S
               GO TO CREDIT-FULL-SCREEN.

       CREDIT-SKIP-CRED-READ.
             MOVE P-NUMBER       TO W75-CNO (W75-S).
      DISPLAY SD-CRD AT LINE CLIN.
          IF CLIN < 21
               ADD 1 TO CLIN W75-S
               GO TO CREDIT-GET-CRED-RECORD.

       CREDIT-FULL-SCREEN.
            DISPLAY " " AT 2303 WITH FOREGROUND-COLOR Brown HIGHLIGHT
             " highlight - " WITH FOREGROUND-COLOR Grey
       "PgUp" WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     "/" WITH FOREGROUND-COLOR Grey
       "PgDn" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       " - " WITH FOREGROUND-COLOR Grey
             "ENTER" WITH FOREGROUND-COLOR Brown HIGHLIGHT
             " select - " WITH FOREGROUND-COLOR Grey
             "Esc" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "ape" WITH FOREGROUND-COLOR Grey.
      PERFORM SAVE-SCREEN-2.
      MOVE 0   TO W75-S1.
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
       MOVE SPACES TO W15-ACNO
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
           IF W75-S < 16
               GO TO CREDIT-GET-USER-OPT.

       CREDIT-NEW-PAGE.
             MOVE 0601           TO CPOS.
             PERFORM CREDIT-CRT-EDGE UNTIL CLIN = 22.
             MOVE 6              TO CLIN.
             MOVE LOW-VALUES TO W75-KEYS.
             MOVE 1 TO W75-S W75-S1.
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
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT
       "N" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "ext or "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT
       "Q" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "uit > <"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT.

       CREDIT-GET-USER-OPT-2.
            ACCEPT WS-OPTION AT 5038
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT(WS-OPTION = "N" OR "Q")
               GO TO CREDIT-GET-USER-OPT-2.
           IF WS-OPTION = "N"
               GO TO CREDIT-LUP-SCREEN.
             MOVE SPACES         TO W15-ACNO.
             GO TO CREDIT-LOOKUP-EXIT.

       CREDIT-ADJ-NO-KEYS.
           IF W75-S1 > 1
               SUBTRACT 1 FROM W75-S1 CLIN.
             GO TO CREDIT-HIGHLIGHT.

       CREDIT-CHK-NO-KEYS.
           IF W75-S1 = ZERO
               MOVE 1 TO W75-S1
               GO TO CREDIT-HIGHLIGHT.
           IF W75-S1 < W75-S
               ADD 1 TO W75-S1 CLIN.

       CREDIT-HIGHLIGHT.
             PERFORM CREDIT-CRT-ATTRIB.
             DISPLAY W43-DET (CLIN) AT CPOS WITH REVERSE-VIDEO.
             GO TO CREDIT-GET-USER-OPT.

       CREDIT-CHK-SELECT.
           IF W75-S1 = ZERO
               GO TO CREDIT-GET-USER-OPT.
             MOVE CSTART TO WS1-CURSOR.
             MOVE W75-CNO (W75-S1) TO P-NUMBER.
             PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
             MOVE ZERO   TO COL-AND-LINE.
             MOVE P-NUMBER       TO W15-ACNO.
             GO TO CREDIT-LOOKUP-EXIT.

       CREDIT-CRT-EDGE.
            DISPLAY WS-MCR1 AT CPOS WITH BACKGROUND-COLOR Black
       FOREGROUND-COLOR Cyan
       WS-MCR78 WITH FOREGROUND-COLOR Cyan
       WS-MCR80 WITH BACKGROUND-COLOR Black
       FOREGROUND-COLOR Cyan.
            ADD 1   TO CLIN.

       CREDIT-LOOKUP-EXIT.
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

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     Setup the file ID's including path names           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *    *************************************************************
      *    ****    P A R A M E T E R   F I L E      ****
      *    *************************************************************
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
       WS-STATUS DELIMITED SIZE INTO WS-ERR-MES.
      PERFORM ERROR-LENGTH THRU ERROR-EXIT.
            STOP RUN.

       ZA00-READ-APACFIDS-EXIT.
      EXIT.

       ZA00A-CONTINUE.
      MOVE AFID-PATH  TO W02-PARAM.
      *    *************************************************************
      *    *** C R E D I T O R  A C C O U N T  ( A / P )   F I L E ***
      *    *************************************************************
      MOVE "CREDIT"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-CREDIT.
      *    *************************************************************
      *    ****     D E P A R T M E N T  F I L E     ****
      *    *************************************************************
      MOVE "DEPART"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-DEPART.
      *    *************************************************************
      *    ****      C O N T R O L   F I L E      ****
      *    *************************************************************
      MOVE "NETWORK"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-NETWORK.
      *    *************************************************************
      *    ****      R E C O V E R Y   F I L E      ****
      *    *************************************************************
      MOVE "RECOVER"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      INSPECT AFID-PATH REPLACING FIRST "XXX"
          BY LS-USER.
      MOVE AFID-PATH  TO W02-RECOVER.
      *    *************************************************************
      *    ****      N E T W O R K   (S H A R E D)   F I L E     ****
      *    *************************************************************
      MOVE "SHARED"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-SHARED.
      *    *************************************************************
      *    *** S T O C K   ( I V E N T O R Y )   M E M O   F I L E  ***
      *    *************************************************************
      MOVE "STKMEM"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-STKMEM.
      *    *************************************************************
      *    ****      S T O C K  ( I V E N T O R Y )   F I L E     ****
      *    *************************************************************
      MOVE "STOCKF"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-STOCKF.
            MOVE 3   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
    IF NOT(PAR-STOCK = "A" OR "N")
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
    IF NOT(PAR-ART11 = "Y" OR "N")
        MOVE "N"   TO PAR-ART11.
      MOVE PAR-ART11  TO W10-ART11.

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
               WHEN 2          MOVE W02-NETWORK  TO WS-FILE
                               MOVE WS-NETKEY    TO WS-KEY
               WHEN 3          MOVE W02-CREDIT   TO WS-FILE
                               MOVE P-NUMBER     TO WS-KEYX
               WHEN 7          MOVE W02-DEPART   TO WS-FILE
                               MOVE DPT-CODE     TO WS-KEYX
               WHEN 15         MOVE WS-PARID     TO WS-FILE
                               MOVE WS-PARKEY    TO WS-KEY
               WHEN 18         MOVE W02-RECOVER  TO WS-FILE
                               MOVE WS-RECKEY    TO WS-KEY
               WHEN 22         MOVE W02-STOCKF   TO WS-FILE
                               MOVE STK-CODE     TO WS-KEYX
               WHEN 25         MOVE W02-STKMEM   TO WS-FILE
                               MOVE SME-KEY      TO WS-KEYX
               WHEN 37         MOVE W02-SHARED   TO WS-FILE
                               MOVE WS-SHARED    TO WS-KEY
             END-EVALUATE.

       COPY "DISPERR.PRO".
