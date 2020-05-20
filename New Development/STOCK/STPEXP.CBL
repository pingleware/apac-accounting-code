      $set LINKCOUNT"384" GNT"STPEXP.GNT"
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
      *    ******   ********  *******   ********  **    **  *******    *
      *   **    **     **     **    **  **         **  **   **    **   *
      *   **           **     **    **  **          ****    **    **   *
      *    ******      **     *******   *****        **     *******    *
      *         **     **     **        **          ****    **         *
      *   **    **     **     **        **         **  **   **         *
      *    ******      **     **        ********  **    **  **         *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S T O C K   E X P O R T   P R O G R A M                    *
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
       PROGRAM-ID.     STPEXP.
       AUTHOR.         J W LEMMON (CSS).
       DATE-WRITTEN.   SEPTEMBER 1992.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1992 - 2018
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

       COPY "DEPART.SL".

       COPY "PARAM.SL".

       COPY "STOCK.SL".

           SELECT STKDET  ASSIGN W02-STKDET
                          STATUS WS-STATUS
                          ORGANIZATION LINE SEQUENTIAL.

           SELECT STKSEL  ASSIGN DISK
                          STATUS WS-STATUS
                          ACCESS RANDOM
                          ORGANIZATION RELATIVE
                          RELATIVE WS-SELKEY.

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

       COPY "DEPART.FDE".

       COPY "PARAM.FDE".

       COPY "STOCK.FDE".

       FD  STKDET    LABEL RECORD OMITTED.
       01  EXP-REC1.
    03  EXP-DATA     PIC  X(650).

       FD  STKSEL    LABEL RECORD STANDARD
                     VALUE OF FILE-ID W02-STKSEL.
       01  SEL-REC.
    03  SEL-CODE     PIC  X(18).

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
       77  WS-IXS    PIC 9(04)  COMP-5.
       77  WS-IXS1    PIC 9(04)  COMP-5.
       77  WS-IXS2    PIC 9(04)  COMP-5.
       77  WS-IXS3    PIC 9(04)  COMP-5.
       77  WS-IXS4    PIC 9(04)  COMP-5.
       77  WS-SELKEY    PIC 9(06)  COMP-5.
       77  WS-SPAKEY    PIC 9(06)  COMP-5.
       77  WS-PARKEY    PIC 9(06)  COMP-5.
       77  WS-NETKEY    PIC 9(06)  COMP-5.
       77  WS-SINKEY    PIC 9(06)  COMP-5.
       77  WS-RECKEY    PIC 9(06)  COMP-5.
       77  WS-RECOVER    PIC 9(06)  COMP-5.
       77  WS-TRANS    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-KEYSTR    PIC 9(04)  COMP-5.
       77  WS-MONTHS    PIC S9(01)V99 COMP-3.
       77  WS-KEY1    PIC 9(04)  COMP-5.
       77  WS-KSTORE    PIC 9(04)  COMP-5.
       77  WS-ITM    PIC X(14).
       77  WS-ETYPE    PIC X(01).
       77  WS-PAGE    PIC 9(04)  COMP-5.
       77  WS-NORM         PIC  X(04).
       77  WS-COND         PIC  X(04).
       77  WS-EXPP         PIC  X(04).
       77  WS-ECAN         PIC  X(04).
       77  WS-8LPI         PIC  X(04).
       77  WS-6LPI         PIC  X(04).
       77  WS-10CPI        PIC  X(04).
       77  WS-12CPI    PIC X(04).
       77  WS-17CPI    PIC X(04).
       77  WS-DBLP    PIC X(04).
       77  WS-DCAN    PIC X(04).
       77  WS-OPTION    PIC X(01).
       77  WS-SKIP    PIC X(01).
       77  WS-ADJ    PIC X(01).
       77  WS-ERROR    PIC 9(01).
       77  WS-IND    PIC 9(01)   VALUE 0.
       77  WS-AMEND    PIC 9(01)   VALUE 0.
       77  WS-CONREC    PIC 9(01) VALUE ZERO.
       77  WS-LEDG    PIC X(04).
       77  WS-LEDG1    PIC X(04).
       77  WS-COMP    PIC 9(01).
       77  WS-WEEK    PIC 9(01).
       77  WS-TYPE    PIC X(01).
       77  WS-USE-PACKS    PIC X(01).
       77  WS-USE-ITM    PIC X(01).
       77  WS-EXT-STK    PIC X(01).
       77  WS-BATCH    PIC 9(05).
       77  WS-PASSWORD    PIC X(08).
       77  WS-PRINT    PIC 9(01).
       77  WS-PASS    PIC X(08).
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STPEXP".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY -EXPORT DATA".
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
    03  WS-PROG        PIC  X(03) VALUE "EXP".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-FILE-IDS.
           03  W02-STKDET.
               05  W02-EXPORT      PIC X(64)     VALUE "STOCK".

       COPY "W05.VAT".

       COPY "W10.STK".

           03  W10-VTAX                          OCCURS 4.
               05  W10-SVAT        PIC S9(07)V99 COMP-3.
               05  W10-CVAT        PIC S9(07)V99 COMP-3.
               05  W10-WVAT        PIC S9(07)V99 COMP-3.

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

       01  W20-DATE.
           03  W20-YY.
               05  W20-YEAR   PIC 9(02).
           03  W20-MM.
               05  W20-MONTH  PIC 9(02).
           03  W20-DD.
               05  W20-DAY    PIC 9(02).
       01  W20-DTE1 REDEFINES W20-DATE.
           03  W20-DTE        PIC 9(06).
       01  W22-DATES.
           03  W22-DTE1        PIC 9(06).
           03  W22-DTE2        PIC 9(06).
           03  W22-DATE2 REDEFINES W22-DTE2.
               05  W22-YY2     PIC 9(02).
               05  W22-MM2     PIC 9(02).
               05  W22-DD2     PIC 9(02).
           03  W22-DTE3        PIC 9(06).
           03  W22-DATE3 REDEFINES W22-DTE3.
               05  W22-DD3     PIC 9(02).
               05  W22-MM3     PIC 9(02).
               05  W22-YY3     PIC 9(02).

       COPY "W25.STK".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W55-EXPORT.
           03  W55-S1          PIC  9(04) COMP-5.
           03  W55-EXP-DATA.
               05  W55-EXP     PIC  X(01) OCCURS 650.

       01  W60-FIELDS.
           03  W60-S           PIC  9(02) COMP-5.
           03  W60-FNO         PIC  9(02) COMP-5 OCCURS 90.

       01  W65-FIELDS.
           03  W65-S           PIC  9(02) COMP-5.
           03  W65-FIELD       PIC  X(22).
           03  W65-FLDS.
      *    Field 01
               05  FILLER      PIC X(21) VALUE "Item Code".
      *    Field 02
               05  FILLER      PIC X(21) VALUE "Alternate Code".
      *    Field 03
               05  FILLER      PIC X(21) VALUE "Bin Location".
      *    Field 04
               05  FILLER      PIC X(21) VALUE "Sales Ledger".
      *    Field 05
               05  FILLER      PIC X(21) VALUE "Description".
      *    Field 06
              05  FILLER      PIC X(21) VALUE "Description 2".
      *    Field 07
               05  FILLER      PIC X(21) VALUE "Quantity".
      *    Field 08
               05  FILLER      PIC X(21) VALUE "Re-order Level".
      *    Field 09
               05  FILLER      PIC X(21) VALUE "Cost Price".
      *    Field 10
        05  FILLER      PIC X(21) VALUE "Price Per (Pack 1)".
      *    Field 11
        05  FILLER      PIC X(21) VALUE "Units 1".
      *    Field 12
              05  FILLER      PIC X(21) VALUE "Wholesale Markup 1".
      *    Field 13
              05  FILLER      PIC X(21) VALUE "Cash Markup 1".
      *    Field 14
              05  FILLER      PIC X(21) VALUE "Retail Markup 1".
      *    Field 15
              05  FILLER      PIC X(21) VALUE "Wholesale Exclusive 1".
      *    Field 16
              05  FILLER      PIC X(21) VALUE "Wholesale Vat 1".
      *    Field 17
              05  FILLER      PIC X(21) VALUE "Wholesale Inclusive 1".
      *    Field 18
              05  FILLER      PIC X(21) VALUE "Cash Exclusive 1".
      *    Field 19
              05  FILLER      PIC X(21) VALUE "Cash Vat 1".
      *    Field 20
              05  FILLER      PIC X(21) VALUE "Cash Inclusive 1".
      *    Field 21
              05  FILLER      PIC X(21) VALUE "Retail Exclusive 1".
      *    Field 22
              05  FILLER      PIC X(21) VALUE "Retail Vat 1".
      *    Field 23
              05  FILLER      PIC X(21) VALUE "Retail Inclusive 1".
      *    Field 24
        05  FILLER      PIC X(21) VALUE "Price Per (Pack 2)".
      *    Field 25
        05  FILLER      PIC X(21) VALUE "Units 2".
      *    Field 26
              05  FILLER      PIC X(21) VALUE "Wholesale Markup 2".
      *    Field 27
              05  FILLER      PIC X(21) VALUE "Cash Markup 2".
      *    Field 28
              05  FILLER      PIC X(21) VALUE "Retail Markup 2".
      *    Field 29
              05  FILLER      PIC X(21) VALUE "Wholesale Exclusive 2".
      *    Field 30
              05  FILLER      PIC X(21) VALUE "Wholesale Vat 2".
      *    Field 31
              05  FILLER      PIC X(21) VALUE "Wholesale Inclusive 2".
      *    Field 32
              05  FILLER      PIC X(21) VALUE "Cash Exclusive 2".
      *    Field 33
              05  FILLER      PIC X(21) VALUE "Cash Vat 2".
      *    Field 34
              05  FILLER      PIC X(21) VALUE "Cash Inclusive 2".
      *    Field 35
              05  FILLER      PIC X(21) VALUE "Retail Exclusive 2".
      *    Field 36
              05  FILLER      PIC X(21) VALUE "Retail Vat 2".
      *    Field 37
              05  FILLER      PIC X(21) VALUE "Retail Inclusive 2".
      *    Field 38
        05  FILLER      PIC X(21) VALUE "Price Per (Pack 3)".
      *    Field 39
        05  FILLER      PIC X(21) VALUE "Units 3".
      *    Field 40
              05  FILLER      PIC X(21) VALUE "Wholesale Markup 3".
      *    Field 41
              05  FILLER      PIC X(21) VALUE "Cash Markup 3".
      *    Field 42
              05  FILLER      PIC X(21) VALUE "Retail Markup 3".
      *    Field 43
              05  FILLER      PIC X(21) VALUE "Wholesale Exclusive 3".
      *    Field 44
              05  FILLER      PIC X(21) VALUE "Wholesale Vat 3".
      *    Field 45
              05  FILLER      PIC X(21) VALUE "Wholesale Inclusive 3".
      *    Field 46
              05  FILLER      PIC X(21) VALUE "Cash Exclusive 3".
      *    Field 47
              05  FILLER      PIC X(21) VALUE "Cash Vat 3".
      *    Field 48
              05  FILLER      PIC X(21) VALUE "Cash Inclusive 3".
      *    Field 49
              05  FILLER      PIC X(21) VALUE "Retail Exclusive 3".
      *    Field 50
              05  FILLER      PIC X(21) VALUE "Retail Vat 3".
      *    Field 51
              05  FILLER      PIC X(21) VALUE "Retail Inclusive 3".
      *    Field 52
        05  FILLER      PIC X(21) VALUE "Price Per (Pack 4)".
      *    Field 53
        05  FILLER      PIC X(21) VALUE "Units 4".
      *    Field 54
              05  FILLER      PIC X(21) VALUE "Wholesale Markup 4".
      *    Field 55
              05  FILLER      PIC X(21) VALUE "Cash Markup 4".
      *    Field 56
              05  FILLER      PIC X(21) VALUE "Retail Markup 4".
      *    Field 57
              05  FILLER      PIC X(21) VALUE "Wholesale Exclusive 4".
      *    Field 58
              05  FILLER      PIC X(21) VALUE "Wholesale Vat 4".
      *    Field 59
              05  FILLER      PIC X(21) VALUE "Wholesale Inclusive 4".
      *    Field 60
              05  FILLER      PIC X(21) VALUE "Cash Exclusive 4".
      *    Field 61
              05  FILLER      PIC X(21) VALUE "Cash Vat 4".
      *    Field 62
              05  FILLER      PIC X(21) VALUE "Cash Inclusive 4".
      *    Field 63
              05  FILLER      PIC X(21) VALUE "Retail Exclusive 4".
      *    Field 64
              05  FILLER      PIC X(21) VALUE "Retail Vat 4".
      *    Field 65
              05  FILLER      PIC X(21) VALUE "Retail Inclusive 4".
      *    Field 66
              05  FILLER      PIC X(21) VALUE "Average Cost".
      *    Field 67
               05  FILLER      PIC X(21) VALUE "Quantity Ordered".
      *    Field 68
               05  FILLER      PIC X(21) VALUE "Lead Time".
      *    Field 69
               05  FILLER      PIC X(21) VALUE "Quantity MTD".
      *    Field 70
               05  FILLER      PIC X(21) VALUE "Cost MTD".
      *    Field 71
               05  FILLER      PIC X(21) VALUE "Selling MTD".
      *    Field 72
               05  FILLER      PIC X(21) VALUE "Quantity YTD".
      *    Field 73
               05  FILLER      PIC X(21) VALUE "Cost YTD".
      *    Field 74
               05  FILLER      PIC X(21) VALUE "Selling YTD".
      *    Field 75
               05  FILLER      PIC X(21) VALUE "Quantity Month 1".
      *    Field 76
               05  FILLER      PIC X(21) VALUE "Quantity Month 2".
      *    Field 77
               05  FILLER      PIC X(21) VALUE "Quantity Month 3".
      *    Field 78
               05  FILLER      PIC X(21) VALUE "Average Monthly Quant".
      *    Field 79
              05  FILLER      PIC X(21) VALUE "Date Last Active".
      *    Field 80
               05  FILLER      PIC X(21) VALUE "Date Last Purchased".
      *    Field 81
               05  FILLER      PIC X(21) VALUE "Date Ordered".
      *    Field 82
               05  FILLER      PIC X(21) VALUE "Low Level Indicator".
      *    Field 83
               05  FILLER      PIC X(21) VALUE "Type Indicator".
      *    Field 84
               05  FILLER      PIC X(21) VALUE "Guarantee Period".
      *    Field 85
               05  FILLER      PIC X(21) VALUE "Tax Rate Code".
      *    Field 86
               05  FILLER      PIC X(21) VALUE "XRef Code".
      *    Field 87
               05  FILLER      PIC X(21) VALUE "Supplier Code".
      *    Field 88
              05  FILLER      PIC X(21) VALUE "Article 11 VAT".
      *    Field 89
              05  FILLER      PIC X(21) VALUE "Number of Cases".
      *    Field 90
              05  FILLER      PIC X(21) VALUE "Convertion Factor".
           03  W65-RFLDS REDEFINES W65-FLDS.
        05  W65-FLD     PIC  X(21) OCCURS 90.

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
               05  W80-5      PIC Z(04)9.
           03  W80-QNT1 REDEFINES W80-QNT.
               05  W80-3V2    PIC Z(02)9.9999.
           03  W80-NO.
               05  W80-5N     PIC 9(05).
               05  W80-REC REDEFINES W80-5N
                              PIC ZZZZ9.
           03  W80-DEL.
               05  W80-2N      PIC 9(02).

       01  W90-BAL             PIC S9(07)V99 COMP-3.

       01  W95-COMP            PIC X(40).

       01  W100-EDIT.
           03  W100-1.
               05  W100-N      PIC  9(01).
           03  W100-2.
               05  W100-N2     PIC  9(02).
           03  W100-3.
               05  W100-NO1    PIC  9(03).
           03  W100-9.
               05  W100-NO2    PIC  Z(08)9.
           03  W100-S9.
        05  W100-NO3    PIC  -(09)9.
    03  W100-S9Q.
        05  W100-QNT    PIC  -(09)9.9999.
           03  W100-PRICE.
               05  W100-S6V2   PIC  Z(05)9.99.
           03  W100-S9V2.
        05  W100-V1     PIC  -(09)9.99.
           03  W100-9V2.
               05  W100-V2     PIC  Z(08)9.99.
    03  W100-9V3.
               05  W100-V3     PIC  Z(08)9.999.
    03  W100-9V4.
        05  W100-V3B    PIC  Z(08)9.9999.
           03  W100-3V2.
               05  W100-V4     PIC  Z(02)9.99.
           03  W100-DATE.
        05  W100-DTE    PIC  9(08) BLANK WHEN ZERO.

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

       01  MENU-INSTRUCT.
    03  LINE 25 COLUMN  2 BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Blue
     VALUE "Use ".
    03        COLUMN  6 HIGHLIGHT
     BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Brown
     VALUE "".
    03        COLUMN  7 BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Blue
     VALUE " or ".
    03        COLUMN 11 HIGHLIGHT
     BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Brown
     VALUE "".
    03        COLUMN 12 BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Blue
     VALUE " to point to an item, and ".
    03        COLUMN 38 HIGHLIGHT
     BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Brown
     VALUE "<Enter>".
    03        COLUMN 45 BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Blue
     VALUE " to Select ".
    03        COLUMN 56 HIGHLIGHT
     BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Brown
     VALUE "<Esc>".
    03        COLUMN 61 BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Blue
     VALUE " to Exit".

       01  S-PROG.
          03  LINE  2 COLUMN 30 FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "STOCK EXPORT PROGRAM".

       01  S-MENU.
    03  LINE  6 COLUMN 25 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
         VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
    03  LINE  7 COLUMN 25 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³        ".
    03        COLUMN 34 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "EXPORT OPTIONS".
    03        COLUMN 48 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "        ³".
    03  LINE  8 COLUMN 25 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 27 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "1".
    03        COLUMN 28 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " Code, Description & Prices ³".
    03  LINE  9 COLUMN 25 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 27 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "2".
    03        COLUMN 28 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " All Fields              ³".
          03  LINE 10 COLUMN 25 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 27 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "3".
    03        COLUMN 28 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " Selected Fields            ³".
          03  LINE 11 COLUMN 25 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 27 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "4".
    03        COLUMN 28 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " Field Separator            ³".
          03  LINE 12 COLUMN 25 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 27 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "5".
    03        COLUMN 28 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " Maintain Selected Items    ³".
          03  LINE 13 COLUMN 25 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 27 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "0".
    03        COLUMN 28 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " Exit                       ³".
    03  LINE 14 COLUMN 25 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
         VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".


       01  S02.
           03  LINE  2 COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
                                 VALUE "STOCK EXPORT PROGRAM".
       01  S03.
           03  LINE  4 COLUMN  2 VALUE
                      "Export: Item code, Description and Prices".

       01  S04.
           03  LINE  4 COLUMN  2 VALUE "Export all the fields".

       01  S05.
           03  LINE  4 COLUMN  2 VALUE "Select the fields required".
           03  LINE  6 COLUMN  2 VALUE "Field :".

       01  S06.
           03  LINE  8 COLUMN  2 VALUE "Fields:".
    03        COLUMN 10 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(1).
    03        COLUMN 13 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(2).
    03        COLUMN 16 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(3).
    03        COLUMN 19 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(4).
    03        COLUMN 22 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(5).
    03        COLUMN 25 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(6).
    03        COLUMN 28 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(7).
    03        COLUMN 31 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(8).
    03        COLUMN 34 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(9).
    03        COLUMN 37 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(10).
    03        COLUMN 40 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(11).
    03        COLUMN 43 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(12).
    03        COLUMN 46 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(13).
    03        COLUMN 49 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(14).
    03        COLUMN 52 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(15).
    03        COLUMN 55 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(16).
    03        COLUMN 58 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(17).
    03        COLUMN 61 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(18).
    03        COLUMN 64 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(19).
    03        COLUMN 67 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(20).
    03        COLUMN 70 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(21).
    03        COLUMN 73 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(22).
    03  LINE  9 COLUMN 10 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(23).
    03        COLUMN 13 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(24).
    03        COLUMN 16 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(25).
    03        COLUMN 19 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(26).
    03        COLUMN 22 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(27).
    03        COLUMN 25 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(28).
    03        COLUMN 28 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(29).
    03        COLUMN 31 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(30).
    03        COLUMN 34 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(31).
    03        COLUMN 37 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(32).
    03        COLUMN 40 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(33).
    03        COLUMN 43 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(34).
    03        COLUMN 46 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(35).
    03        COLUMN 49 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(36).
    03        COLUMN 52 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(37).
    03        COLUMN 55 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(38).
    03        COLUMN 58 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(39).
    03        COLUMN 61 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(40).
    03        COLUMN 64 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(41).
    03        COLUMN 67 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(42).
    03        COLUMN 70 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(43).
    03        COLUMN 73 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(44).
    03  LINE 10 COLUMN 10 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(45).
    03        COLUMN 13 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(46).
    03        COLUMN 16 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(47).
    03        COLUMN 19 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(48).
    03        COLUMN 22 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(49).
    03        COLUMN 25 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(50).
    03        COLUMN 28 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(51).
    03        COLUMN 31 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(52).
    03        COLUMN 34 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(53).
    03        COLUMN 37 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(54).
    03        COLUMN 40 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(55).
    03        COLUMN 43 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(56).
    03        COLUMN 46 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(57).
    03        COLUMN 49 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(58).
    03        COLUMN 52 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(59).
    03        COLUMN 55 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(60).
    03        COLUMN 58 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(61).
    03        COLUMN 61 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(62).
    03        COLUMN 64 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(63).
    03        COLUMN 67 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(64).
    03        COLUMN 70 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(65).
    03        COLUMN 73 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(66).
    03  LINE 11 COLUMN 10 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(67).
    03        COLUMN 13 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(68).
    03        COLUMN 16 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(69).
    03        COLUMN 19 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(70).
    03        COLUMN 22 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(71).
    03        COLUMN 25 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(72).
    03        COLUMN 28 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(73).
    03        COLUMN 31 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(74).
    03        COLUMN 34 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(75).
    03        COLUMN 37 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(76).
    03        COLUMN 40 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(77).
    03        COLUMN 43 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(78).
    03        COLUMN 46 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(79).
    03        COLUMN 49 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(80).
    03        COLUMN 52 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(81).
    03        COLUMN 55 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(82).
    03        COLUMN 58 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(83).
    03        COLUMN 61 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(84).
    03        COLUMN 64 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(85).
    03        COLUMN 67 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(86).
    03        COLUMN 70 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(87).
    03        COLUMN 73 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(88).
    03  LINE 12 COLUMN 10 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(89).
    03        COLUMN 13 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ FROM W60-FNO(90).

       01  S07.
           03  LINE 16 COLUMN  2 VALUE "Exporting:".

       01  S08.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
               05  LINE 16 COLUMN 13 PIC  X(14) FROM STK-ITEM.
               05          COLUMN 28 PIC  X(30) FROM           STK-DESC.

       01  S08-ITM-CODE.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
               05  S08-ITM.
                   07  LINE 16 COLUMN 13 PIC  X(14) FROM STK-ITEM.
                   07          COLUMN 27 VALUE                  "/"                FOREGROUND-COLOR Black.
                   07          COLUMN 28 PIC  X(03) FROM          STK-ITM.
                   07          COLUMN 32 PIC  X(30) FROM                  STK-DESC.

               05  S08-EXT.
                   07  LINE 16 COLUMN 13 PIC  X(14) FROM STK-CODE.
                   07          COLUMN 32 PIC  X(30) FROM                  STK-DESC.

       01  S09.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
               05  LINE  6 COLUMN 10 PIC  99    FROM W65-S.
               05          COLUMN 13 PIC  X(21) FROM       W65-FIELD FOREGROUND-COLOR Grey HIGHLIGHT.
       01  S10.
           03  LINE  4 COLUMN  2 VALUE "Select the Items required".
           03  LINE  6 COLUMN  2 VALUE "Item :".

       01  S11-ITEM-CODE.
      *
      *    ****   Set the colours to be used for these screens 
      * 
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               05  S11.
                   07  PIC X(14) USING W10-ITEM AUTO.
               
               05  S11-ITM.
                   07  LINE  9 COLUMN 20 PIC X(14) USING W10-ITEM AUTO.
                   07          COLUMN 35 PIC X(03) USING W10-ITM AUTO.
               
               05  S11-EXT.
                   07  PIC X(18) USING W10-EXT-ITEM AUTO.
               
               05  S11-3.
                   07  PIC Z(03) USING W10-I4 AUTO.
               
               05  S11-4.
                   07  PIC Z(04) USING W10-I4 AUTO.
               
               05  S11-5.
                   07  PIC Z(05) USING W10-I5 AUTO.
               
               05  S11-6.
                   07  PIC Z(06) USING W10-I6 AUTO.
               
               05  S11-7.
                   07  PIC Z(07) USING W10-I7 AUTO.
               
               05  S11-8.
                   07  PIC Z(08) USING W10-I8 AUTO.
               
               05  S11-9.
                   07  PIC Z(09) USING W10-I9 AUTO.
               
               05  S11-10.
                   07  PIC Z(10) USING W10-I10 AUTO.
               
               05  S11-11.
                   07  PIC Z(11) USING W10-I11 AUTO.
               
               05  S11-12.
                   07  PIC Z(12) USING W10-I12 AUTO.
               
               05  S11-13.
                   07  PIC Z(13) USING W10-I13 AUTO.
               
               05  S11-14.
                   07  PIC Z(14) USING W10-I14 AUTO.

      *01  S11.
      *    03  LINE  6 COLUMN  9 FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta PIC X(14) USING W10-EXT-ITEM AUTO.

      *01  S11-ITM.
      *    03  LINE  6 COLUMN  9 FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta PIC X(14) USING W10-ITEM AUTO.
      *    03          COLUMN 23 FOREGROUND-COLOR Black BACKGROUND-COLOR Magenta VALUE "/".
      *    03          COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta PIC X(03) USING W10-ITM AUTO.

      *01  S11-EXT.
      *   03  LINE  6 COLUMN  9 PIC X(18) USING W10-EXT-ITEM AUTO.

      *01  S11-3.
      *   03  LINE  6 COLUMN  9 PIC Z(03) USING W10-I3 AUTO.

      *01  S11-4.
      *   03  LINE  6 COLUMN  9 PIC Z(04) USING W10-I4 AUTO.

      *01  S11-5.
      *   03  LINE  6 COLUMN  9 PIC Z(05) USING W10-I5 AUTO.

      *01  S11-6.
      *   03  LINE  6 COLUMN  9 PIC Z(06) USING W10-I6 AUTO.

      *01  S11-7.
      *   03  LINE  6 COLUMN  9 PIC Z(07) USING W10-I7 AUTO.

      *01  S11-8.
      *   03  LINE  6 COLUMN  9 PIC Z(08) USING W10-I8 AUTO.

      *01  S11-9.
      *   03  LINE  6 COLUMN  9 PIC Z(09) USING W10-I9 AUTO.

      *01  S11-10.
      *   03  LINE  6 COLUMN  9 PIC Z(10) USING W10-I10 AUTO.

      *01  S11-11.
      *   03  LINE  6 COLUMN  9 PIC Z(11) USING W10-I11 AUTO.

      *01  S11-12.
      *   03  LINE  6 COLUMN  9 PIC Z(12) USING W10-I12 AUTO.

      *01  S11-13.
      *   03  LINE  6 COLUMN  9 PIC Z(13) USING W10-I13 AUTO.

      *01  S11-14.
      *   03  LINE  6 COLUMN  9 PIC Z(14) USING W10-I14 AUTO.

       01  S12.
    03  LINE  4 COLUMN  2 FOREGROUND-COLOR Cyan HIGHLIGHT
     VALUE "Change Field Separator".
    03  LINE  6 COLUMN  2 VALUE "Character Used:".

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
          IF LS0-STLEV < 9
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
            PERFORM ZA000-INIT.
           IF WS-STATUS NOT = "00"
               PERFORM OPEN-ERROR.
             PERFORM BA000.

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

      *
      *    ****    G E T   T H E   E X P O R T   F I L E   I D
      *
       AA100           SECTION.
       AA101.
             DISPLAY "Keyin the <pathname>/<filename> " AT 2202.
             DISPLAY "EXPORT file :" AT 2302.
             MOVE ZERO               TO WS-ERROR.
       AA105.
             ACCEPT W02-EXPORT AT 2316 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                     MOVE 9          TO WS-ERROR
                     GO TO AA199
                 WHEN OTHER
                     PERFORM AA900-ALARM
                     GO TO AA105
               END-EVALUATE.
             MOVE 64                 TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W02-EXPORT BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             DISPLAY W02-EXPORT AT 2316 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO AA105.
             OPEN OUTPUT STKDET.
           IF WS-STATUS = "00"
               PERFORM AA800
               GO TO AA199.
             MOVE "Invalid file name" TO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.
             GO TO AA105.
       AA199.
             EXIT.
      *
      *    ****    M O V E   F I E L D   T O   E X P O R T   A R E A
      *
      *            W50-DATA   : CONTAINS DATA TO BE INSERTED
      *            W50-LENGTH : LENGTH OF DATA
      *            W50-EXP    : FIELD TERMINATION CHARACTER
      *
       AA200           SECTION.
       AA201.
           IF W50-DATA = SPACES
               GO TO AA225.
             MOVE W50-LENGTH         TO W50-S1.
      *
      *    ****    D E T E R M I N E   E N D   O F   F I E L D
      *
      *            THIS LOOP EXCLUDES THE TRAILING SPACES
      *
       AA205.
           IF W50-CCHAR(W50-S1) = SPACE
               SUBTRACT 1            FROM W50-S1
               GO TO AA205.
      *
      *    ****    R E M O V E   L E A D I N G   S P A C E S
      *
       AA210.
           IF W50-1 = SPACE
               MOVE W50-D39          TO W50-DATA
               SUBTRACT 1            FROM W50-S1
               GO TO AA210.
             MOVE W50-S1             TO W50-S2.
             MOVE 1                  TO W50-S1.
      *
      *    ****   M O V E   D A T A   T O   E X P O R T   A R E A
      *
       AA220.
             MOVE W50-CCHAR(W50-S1)  TO W55-EXP(W55-S1).
             ADD 1                   TO W50-S1 W55-S1.
           IF W50-S1 NOT > W50-S2
               GO TO AA220.
      *
      *    ****   M O V E   F I E L D   T E R M I N A T O R   T O
      *                       E X P O R T   A R E A
      *
       AA225.
             MOVE W50-EXP            TO W55-EXP(W55-S1).
             ADD 1                   TO W55-S1.

       AA299.
             EXIT.
      *
      *    ****    M O V E   I N D V I D U A L   F I E L D S
      *
      *            W50-BIN : FIELD NUMBER TO MOVE
      *
       AA300              SECTION.
       AA301.
           IF W50-BIN < 01 OR > 90
               GO TO AA399.
      *
             EVALUATE W60-BIN
               WHEN 01   MOVE STK-CODE     TO W50-DATA
                         MOVE 18           TO W50-LENGTH
      *
               WHEN 02   MOVE STK-ACODE    TO W50-DATA
                         MOVE 10           TO W50-LENGTH
      *
               WHEN 03   MOVE STK-BIN      TO W50-DATA
                         MOVE 6            TO W50-LENGTH
      *
               WHEN 04   MOVE STK-LDG      TO W50-DATA
                         MOVE 4            TO W50-LENGTH
      *
               WHEN 05   MOVE STK-DESC     TO W50-DATA
                         MOVE 30           TO W50-LENGTH
      *
               WHEN 06   MOVE STK-DESC2    TO W50-DATA
                         MOVE 30           TO W50-LENGTH
      *
               WHEN 07   MOVE STK-QUANT    TO W100-QNT
                         MOVE W100-S9Q     TO W50-DATA
                         MOVE 15           TO W50-LENGTH
      *
               WHEN 08   MOVE STK-LEVEL    TO W100-NO2
                         MOVE W100-9       TO W50-DATA
                         MOVE 9            TO W50-LENGTH
      *
               WHEN 09   MOVE STK-COST     TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 10   MOVE STK-PR(1)    TO W50-DATA
                         MOVE 8            TO W50-LENGTH
      *
               WHEN 11   MOVE STK-UNT(1)   TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 12   MOVE STK-WMKUP(1) TO W100-V4
                         MOVE W100-3V2     TO W50-DATA
                         MOVE 6            TO W50-LENGTH
      *
               WHEN 13   MOVE STK-CMKUP(1) TO W100-V4
                         MOVE W100-3V2     TO W50-DATA
                         MOVE 6            TO W50-LENGTH
      *
               WHEN 14   MOVE STK-MKUP(1)  TO W100-V4
                         MOVE W100-3V2     TO W50-DATA
                         MOVE 6            TO W50-LENGTH
      *
               WHEN 15   MOVE STK-WSL(1)   TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 16   MOVE W10-WVAT(1)  TO W100-V2
                         MOVE W100-9V2     TO W50-DATA
                         MOVE 12           TO W50-LENGTH
      *
               WHEN 17   MOVE W10-VWSL(1)  TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 18   MOVE STK-CSH(1)   TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 19   MOVE W10-CVAT(1)  TO W100-V2
                         MOVE W100-9V2     TO W50-DATA
                         MOVE 12           TO W50-LENGTH
      *
               WHEN 20   MOVE W10-VCSH(1)  TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 21   MOVE STK-SL(1)    TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 22   MOVE W10-SVAT(1)  TO W100-V2
                         MOVE W100-9V2     TO W50-DATA
                         MOVE 12           TO W50-LENGTH
      *
               WHEN 23   MOVE W10-VSL(1)   TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 24   MOVE STK-PR(2)    TO W50-DATA
                         MOVE 8            TO W50-LENGTH
      *
               WHEN 25   MOVE STK-UNT(2)   TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 26   MOVE STK-WMKUP(2) TO W100-V4
                         MOVE W100-3V2     TO W50-DATA
                         MOVE 6            TO W50-LENGTH
      *
               WHEN 27   MOVE STK-CMKUP(2) TO W100-V4
                         MOVE W100-3V2     TO W50-DATA
                         MOVE 6            TO W50-LENGTH
      *
               WHEN 28   MOVE STK-MKUP(2)  TO W100-V4
                         MOVE W100-3V2     TO W50-DATA
                         MOVE 6            TO W50-LENGTH
      *
               WHEN 29   MOVE STK-WSL(2)   TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 30   MOVE W10-WVAT(2)  TO W100-V2
                         MOVE W100-9V2     TO W50-DATA
                         MOVE 12           TO W50-LENGTH
      *
               WHEN 31   MOVE W10-VWSL(2)  TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 32   MOVE STK-CSH(2)   TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 33   MOVE W10-CVAT(2)  TO W100-V2
                         MOVE W100-9V2     TO W50-DATA
                         MOVE 12           TO W50-LENGTH
      *
               WHEN 34   MOVE W10-VCSH(2)  TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 35   MOVE STK-SL(2)    TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 36   MOVE W10-SVAT(2)  TO W100-V2
                         MOVE W100-9V2     TO W50-DATA
                         MOVE 12           TO W50-LENGTH
      *
               WHEN 37   MOVE W10-VSL(2)   TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 38   MOVE STK-PR(3)    TO W50-DATA
                         MOVE 8            TO W50-LENGTH
      *
               WHEN 39   MOVE STK-UNT(3)   TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 40   MOVE STK-WMKUP(3) TO W100-V4
                         MOVE W100-3V2     TO W50-DATA
                         MOVE 6            TO W50-LENGTH
      *
               WHEN 41   MOVE STK-CMKUP(3) TO W100-V4
                         MOVE W100-3V2     TO W50-DATA
                         MOVE 6            TO W50-LENGTH
      *
               WHEN 42   MOVE STK-MKUP(3)  TO W100-V4
                         MOVE W100-3V2     TO W50-DATA
                         MOVE 6            TO W50-LENGTH
      *
               WHEN 43   MOVE STK-WSL(3)   TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 44   MOVE W10-WVAT(3)  TO W100-V2
                         MOVE W100-9V2     TO W50-DATA
                         MOVE 12           TO W50-LENGTH
      *
               WHEN 45   MOVE W10-VWSL(3)  TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 46   MOVE STK-CSH(3)   TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 47   MOVE W10-CVAT(3)  TO W100-V2
                         MOVE W100-9V2     TO W50-DATA
                         MOVE 12           TO W50-LENGTH
      *
               WHEN 48   MOVE W10-VCSH(3)  TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 49   MOVE STK-SL(3)    TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 50   MOVE W10-SVAT(3)  TO W100-V2
                         MOVE W100-9V2     TO W50-DATA
                         MOVE 12           TO W50-LENGTH
      *
               WHEN 51   MOVE W10-VSL(3)   TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 52   MOVE STK-PR(4)    TO W50-DATA
                         MOVE 8            TO W50-LENGTH
      *
               WHEN 53   MOVE STK-UNT(4)   TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 54   MOVE STK-WMKUP(4) TO W100-V4
                         MOVE W100-3V2     TO W50-DATA
                         MOVE 6            TO W50-LENGTH
      *
               WHEN 55   MOVE STK-CMKUP(4) TO W100-V4
                         MOVE W100-3V2     TO W50-DATA
                         MOVE 6            TO W50-LENGTH
      *
               WHEN 56   MOVE STK-MKUP(4)  TO W100-V4
                         MOVE W100-3V2     TO W50-DATA
                         MOVE 6            TO W50-LENGTH
      *
               WHEN 57   MOVE STK-WSL(4)   TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 58   MOVE W10-WVAT(4)  TO W100-V2
                         MOVE W100-9V2     TO W50-DATA
                         MOVE 12           TO W50-LENGTH
      *
               WHEN 59   MOVE W10-VWSL(4)  TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 60   MOVE STK-CSH(4)   TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 61   MOVE W10-CVAT(4)  TO W100-V2
                         MOVE W100-9V2     TO W50-DATA
                         MOVE 12           TO W50-LENGTH
      *
               WHEN 62   MOVE W10-VCSH(4)  TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 63   MOVE STK-SL(4)    TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 64   MOVE W10-SVAT(4)  TO W100-V2
                         MOVE W100-9V2     TO W50-DATA
                         MOVE 12           TO W50-LENGTH
      *
               WHEN 65   MOVE W10-VSL(4)   TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 66   MOVE STK-AVGE     TO W100-V3B
                         MOVE W100-9V4     TO W50-DATA
                         MOVE 14           TO W50-LENGTH
      *
               WHEN 67   MOVE STK-ORD      TO W100-NO2
                         MOVE W100-9       TO W50-DATA
                         MOVE 9            TO W50-LENGTH
      *
               WHEN 68   MOVE STK-LTIME    TO W100-V4
                         MOVE W100-3V2     TO W50-DATA
                         MOVE 6            TO W50-LENGTH
      *
               WHEN 69   MOVE STK-MTD      TO W100-NO3
                         MOVE W100-S9      TO W50-DATA
                         MOVE 10           TO W50-LENGTH
      *
               WHEN 70   MOVE STK-MTDC     TO W100-V1
                         MOVE W100-S9V2    TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 71   MOVE STK-MTDV     TO W100-V1
                         MOVE W100-S9V2    TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 72   MOVE STK-YTD      TO W100-NO3
                         MOVE W100-S9      TO W50-DATA
                         MOVE 10           TO W50-LENGTH
      *
               WHEN 73   MOVE STK-YTDC     TO W100-V1
                         MOVE W100-S9V2    TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 74   MOVE STK-YTDV     TO W100-V1
                         MOVE W100-S9V2    TO W50-DATA
                         MOVE 13           TO W50-LENGTH
      *
               WHEN 75   MOVE STK-MTH1     TO W100-NO3
                         MOVE W100-S9      TO W50-DATA
                         MOVE 10           TO W50-LENGTH
      *
               WHEN 76   MOVE STK-MTH2     TO W100-NO3
                         MOVE W100-S9      TO W50-DATA
                         MOVE 10           TO W50-LENGTH
      *
               WHEN 77   MOVE STK-MTH3     TO W100-NO3
                         MOVE W100-S9      TO W50-DATA
                         MOVE 10           TO W50-LENGTH
      *
               WHEN 78   MOVE STK-MAVG     TO W100-NO3
                         MOVE W100-S9      TO W50-DATA
                         MOVE 10           TO W50-LENGTH
      *
               WHEN 79   MOVE STK-ACT      TO W100-DTE
                         MOVE W100-DATE    TO W50-DATA
                         MOVE 8            TO W50-LENGTH
      *
               WHEN 80   MOVE STK-PUR      TO W100-DTE
                         MOVE W100-DATE    TO W50-DATA
                         MOVE 8            TO W50-LENGTH
      *
               WHEN 81   MOVE STK-DTE      TO W100-DTE
                         MOVE W100-DATE    TO W50-DATA
                         MOVE 8            TO W50-LENGTH
      *
               WHEN 82   MOVE STK-DISC     TO W50-DATA
                         MOVE 1            TO W50-LENGTH
      *
               WHEN 83   MOVE STK-IND      TO W100-N
                         MOVE W100-1       TO W50-DATA
                         MOVE 1            TO W50-LENGTH
      *
               WHEN 84   MOVE STK-GUAR     TO W100-N2
                         MOVE W100-2       TO W50-DATA
                         MOVE 2            TO W50-LENGTH
      *
               WHEN 85   MOVE STK-TAX      TO W100-N
                         MOVE W100-1       TO W50-DATA
                         MOVE 1            TO W50-LENGTH
      *
               WHEN 86   MOVE STK-XREF     TO W50-DATA
                         MOVE 18           TO W50-LENGTH
      *
               WHEN 87   MOVE STK-SUPP     TO W50-DATA
                         MOVE 6            TO W50-LENGTH
      *
               WHEN 88   MOVE STK-EXEMPT   TO W50-DATA
                         MOVE 1            TO W50-LENGTH
      *
               WHEN 89   MOVE STK-CASES    TO W100-NO2
                         MOVE W100-9       TO W50-DATA
                         MOVE 9            TO W50-LENGTH
      *
               WHEN 90   MOVE STK-FACT     TO W100-V3
                         MOVE W100-9V3     TO W50-DATA
                         MOVE 13           TO W50-LENGTH
             END-EVALUATE.
             PERFORM AA200.

       AA399.
               EXIT.
      *
      *    ****    P R O C E S S   T H E   S E L E C T E D   F I E L D S
      *
       AA400           SECTION.
       AA401.
             MOVE 1                  TO W60-S.
      *
      *    ****    C L E A R   E X P O R T   D A T A   A R E A
      *
             MOVE SPACES             TO W55-EXP-DATA.
             MOVE 1                  TO W55-S1.
       AA405.
           IF W60-FNO(W60-S) = ZERO
               GO TO AA499.
             MOVE W60-FNO(W60-S)     TO W50-BIN.
             PERFORM AA300.
           IF W60-S < 90
               ADD 1                 TO W60-S
               GO TO AA405.
       AA499.
             EXIT.
      *
      *    ****    S E C U R I T Y   V I O L A T I O N
      *
       AA500              SECTION.
       AA501.
             DISPLAY "Insufficient security" AT 0412
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Press ENTER to continue" AT 0612
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT WS-OPTION AT 0636
      WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.
       AA505.
             EXIT.
      *
      *    ****    Calculate the inclusive prices
      *
       AA600         SECTION.
      *
      *    ****    U N I T S   A N D   M A R K U P S
      *
       AA601.
      MOVE 1   TO WS-S1.

       AA602.
      MOVE ZERO   TO W10-VSL(WS-S1)
        W10-VCSH(WS-S1)
        W10-VWSL(WS-S1)
        W10-SVAT(WS-S1)
        W10-CVAT(WS-S1)
        W10-WVAT(WS-S1).
    IF WS-S1 < 4
        ADD 1   TO WS-S1
        GO TO AA602.
      MOVE 1   TO WS-S1.

       AA605.
           IF STK-WSALE = ZERO
               IF STK-CASH NOT = ZERO
                   MOVE STK-CASH TO STK-WSALE
               ELSE
                   MOVE STK-SELL TO STK-WSALE.
           IF STK-CASH = ZERO
               MOVE STK-SELL     TO STK-CASH.
      MOVE ZERO   TO W05-RATE.
    IF STK-TAX > 0
        MOVE W05-VAT(STK-TAX)
                                 TO W05-RATE.

       AA610.
      COMPUTE W10-VCSH(WS-S1) ROUNDED
     = STK-CSH(WS-S1) + (STK-CSH(WS-S1) * W05-RTE).
      COMPUTE W10-VWSL(WS-S1) ROUNDED
     = STK-WSL(WS-S1) + (STK-WSL(WS-S1) * W05-RTE).
      COMPUTE W10-VSL(WS-S1) ROUNDED
     = STK-SL(WS-S1) + (STK-SL(WS-S1) * W05-RTE).
      COMPUTE W10-SVAT(WS-S1) = W10-VSL(WS-S1) - STK-SL(WS-S1).
      COMPUTE W10-CVAT(WS-S1) = W10-VCSH(WS-S1) - STK-CSH(WS-S1).
      COMPUTE W10-WVAT(WS-S1) = W10-VWSL(WS-S1) - STK-WSL(WS-S1).
    IF WS-S1 < 4
        ADD 1   TO WS-S1
        GO TO AA610.
    IF STK-IND < 2
              IF STK-AVGE = ZERO
           MOVE STK-COST TO STK-AVGE.

       AA699.
             EXIT.
      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S
      *
       AA700              SECTION.
       AA701.
            DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "=Help,"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "F2" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             " Item code/ "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "F3" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             " Alt code/ "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "F4" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             " Desc/ "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "F5" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             " Desc2/ "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "F6" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             " X/Ref Lookup"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
       AA705.
             EXIT.
      *
      *    ****   H E A D E R S
      *
       AA800    SECTION.
       AA801.
      MOVE 1   TO W60-S.
      *
      *    ****    C L E A R   E X P O R T   D A T A   A R E A
      *
             MOVE SPACES         TO W55-EXP-DATA.
             MOVE 1              TO W55-S1.
       AA805.
    IF W60-FNO(W60-S) = ZERO
        MOVE W55-EXP-DATA TO EXP-REC1
        PERFORM WRITE-STKDET THRU WRITE-STKDET-EXIT
        GO TO AA899.
      MOVE W60-FNO(W60-S) TO W50-BIN.
      MOVE 21   TO W50-LENGTH.
      MOVE W65-FLD(W50-BIN)
     TO W50-DATA.
      PERFORM AA200.
    IF W60-S < 90
               ADD 1             TO W60-S
        GO TO AA805.
       AA899.
             EXIT.

       COPY "AA900.ALM".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000-READ              SECTION.

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "STOCK.RD".

      *
      *   R E W R I T E   &    W R I T E   F I L E S   R O U T I N E S
      *
       AD000-WRITE             SECTION.

       WRITE-STKDET.
             WRITE EXP-REC1.
           IF WS-STATUS NOT = "00"
               MOVE 99           TO WS-F-ERROR
               PERFORM WRITE-ERROR.
      
       WRITE-STKDET-EXIT.
             EXIT.

       COPY "APAC.HLP".

      /
      *    ****    P R O G R A M   M E N U / C O N T R O L
      *
       BA000           SECTION.
       BA00.
    IF MOUSE-ATTACHED = "Y"
        MOVE 66   TO MOUSE-FUNC
        MOVE 1   TO MOUSE-PARAM
        CALL X"AF" USING MOUSE-FUNC
    MOUSE-PARAM.
      PERFORM ERASE-SCREEN.
      DISPLAY S-PROG.
      MOVE 7   TO SHADE-ROW.
      MOVE 27   TO SHADE-COL.
      MOVE 30   TO SHADE-WIDTH.
      MOVE 8   TO SHADE-LINES.
      DISPLAY S-MENU.
    IF LS0-LEV < 3
        DISPLAY "(N/A)" AT 1045
   WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Green
        DISPLAY "N/A" AT 1253
   WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Green.
      DISPLAY MENU-INSTRUCT.
      PERFORM SCREEN-SHADOW.
       BA02.
      MOVE 7   TO TOP-ROW.
      MOVE 12   TO BOTTOM-ROW.
      MOVE 6   TO SCREEN-LIN.
      MOVE 27   TO SCREEN-COL.
      MOVE 28   TO STRING-LENGTH.
      PERFORM SAVE-ATTR.

       BA05.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BA10.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY  PERFORM MOVE-BLOCK-UP
   WHEN DOWN-KEY  PERFORM MOVE-BLOCK-DOWN
   WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
     IF MOUSE-ENTER = "Y"
         MOVE "N"  TO MOUSE-ENTER
         GO TO BA15
     END-IF
     IF MOUSE-Y = 24
         IF MOUSE-X = 5
      MOVE 5  TO KEY-CODE-1
      GO TO BA10
         ELSE
         IF MOUSE-X = 10
      MOVE 6  TO KEY-CODE-1
      GO TO BA10
         ELSE
         IF MOUSE-X > 36 AND < 44
      MOVE 0  TO KEY-CODE-1
      GO TO BA10
         ELSE
         IF MOUSE-X > 54 AND < 60
      MOVE 0  TO KEY-CODE-1
      MOVE 1  TO KEY-TYPE
      GO TO BA10
         END-IF
     END-IF
   WHEN ENTER-KEY  GO TO BA15
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO BA05
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE "0"  TO WS-OPTION
       GO TO BA18
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO BA05
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION
        IF WS-OPTION < 0 OR > 5
     PERFORM AA900-ALARM
     GO TO BA05
        END-IF
        GO TO BA16
    ELSE
        PERFORM AA900-ALARM
        GO TO BA05.

       BA15.
    IF SCREEN-LIN < 7
        PERFORM AA900-ALARM
        GO TO BA05
    ELSE
    IF SCREEN-LIN = 7
        MOVE "1"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 8
        MOVE "2"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 9
        MOVE "3"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 10
        MOVE "4"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 11
        MOVE "5"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 12
        MOVE "0"   TO WS-OPTION.
    GO TO BA18.

       BA16.
      PERFORM CLEAR-BLOCK.
    IF WS-OPTION = "0"
        MOVE 12   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "1"
        MOVE 7   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "2"
        MOVE 8   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "3"
        MOVE 9   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "4"
        MOVE 10   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "5"
        MOVE 11   TO SCREEN-LIN.
      PERFORM SAVE-ATTR.
      PERFORM MARK-BLOCK.

       BA18.
    IF MOUSE-ATTACHED = "Y"
        MOVE 66   TO MOUSE-FUNC
        MOVE 0   TO MOUSE-PARAM
        CALL X"AF" USING MOUSE-FUNC
    MOUSE-PARAM.
          IF WS-OPTION = "0"
               GO TO BA999.
           IF WS-OPTION = "1"
               PERFORM DA000
          ELSE
           IF WS-OPTION = "2"
               PERFORM EA000
          ELSE
          IF WS-OPTION = "3" AND
       LS0-LEV > 2
              PERFORM FA000
          ELSE
           IF WS-OPTION = "4"
        PERFORM HA000
          ELSE
    IF WS-OPTION = "5" AND
       LS0-LEV > 2
        PERFORM GA000.
      GO TO BA00.
       BA999.
             EXIT.

       COPY "CA000.STK".

       COPY "CA100.STK".

       COPY "CA200.STK".

       COPY "CA500.STK".

      /
      *    ****    E X P O R T :    (PACK 1)
      *
      *                I T E M   C O D E
      *                D E S C R I P T I O N
      *                D I S T R I B U T O R   P R I C E
      *                       E X C L U S I V E
      *                       V A T
      *                       I N C L U S I V E
      *                D E A L E R   P R I C E
      *                       E X C L U S I V E
      *                       V A T
      *                       I N C L U S I V E
      *                B U S I N E S S   P R I C E
      *                       E X C L U S I V E
      *                       V A T
      *                       I N C L U S I V E
      *
       DA000        SECTION 5.
       DA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S02.
             DISPLAY S03.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO DA999.
      INITIALIZE W60-FIELDS.
      MOVE 01   TO W60-FNO(1).
      MOVE 05   TO W60-FNO(2).
      MOVE 15   TO W60-FNO(3).
      MOVE 16   TO W60-FNO(4).
      MOVE 17   TO W60-FNO(5).
      MOVE 18   TO W60-FNO(6).
      MOVE 19   TO W60-FNO(7).
      MOVE 20   TO W60-FNO(8).
      MOVE 21   TO W60-FNO(9).
      MOVE 22   TO W60-FNO(10).
      MOVE 23   TO W60-FNO(11).
             DISPLAY S06.
             PERFORM AA100.
       DA02.
      MOVE "'L'edger,'I'tem sequence,'S'eleted items  [I]"
     TO WS-ERR-MES.
      MOVE "I"   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
    IF NOT (WS-OPTION = "I" OR "L" OR "S")
               GO TO DA02.
    IF WS-OPTION = "S"
               GO TO DA10.
             DISPLAY S07.
             MOVE "!"            TO STK-CODE.
           IF WS-OPTION = "I"
               PERFORM START-AT-STOCK-CODE THRU READ-STOCK-EXIT
           ELSE
        MOVE "!"   TO STK-LDG
               PERFORM START-AT-STOCK-LEDG THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO DA900.

       DA05.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO DA900.
    IF STK-IND = 4
        GO TO DA05.
      *    ****    A V E R A G E   C O S T
    IF STK-QUANT < 0.001
        MOVE STK-COST  TO STK-AVGE.
    IF WS-USE-ITM = "Y"
        DISPLAY S08-ITM
    ELSE
    IF WS-EXT-STK = "Y"
        DISPLAY S08-EXT
    ELSE
        DISPLAY S08.
             PERFORM AA600.
             PERFORM AA400.
             MOVE W55-EXP-DATA   TO EXP-REC1.
             PERFORM WRITE-STKDET THRU WRITE-STKDET-EXIT.
             GO TO DA05.
       DA10.
             OPEN I-O STKSEL.
           IF WS-STATUS NOT = "00"
               DISPLAY W02-STKSEL AT 2302 WITH FOREGROUND-COLOR Brown HIGHLIGHT
                       " has not been created!" with FOREGROUND-COLOR Grey
               GO TO DA02.
             MOVE 1              TO WS-SELKEY.
       DA15.
             READ STKSEL.
           IF WS-STATUS NOT = "00"
               CLOSE STKSEL
               GO TO DA900.
           IF SEL-CODE = SPACES
               GO TO DA20.
             MOVE SEL-CODE       TO STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO DA15.
    IF STK-IND = 4
        GO TO DA15.
      *    ****    A V E R A G E   C O S T
    IF STK-QUANT < 0.001
        MOVE STK-COST  TO STK-AVGE.
    IF WS-USE-ITM = "Y"
        DISPLAY S08-ITM
    ELSE
    IF WS-EXT-STK = "Y"
        DISPLAY S08-EXT
    ELSE
        DISPLAY S08.
      PERFORM AA600.
             PERFORM AA400.
             MOVE W55-EXP-DATA   TO EXP-REC1.
             PERFORM WRITE-STKDET THRU WRITE-STKDET-EXIT.
       DA20.
             ADD 1               TO WS-SELKEY.
             GO TO DA15.
       DA900.
      CLOSE STKDET.

       DA999.
             EXIT.
      /
      *    ****    E X P O R T :
      *
      *                A L L   T H E   F I E L D S
      *
       EA000        SECTION 5.
       EA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S02.
             DISPLAY S04.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO EA999.
      INITIALIZE W60-FIELDS.
             MOVE 1              TO W60-S.
       EA05.
      MOVE W60-S   TO W60-FNO(W60-S).
    IF W60-S < 90
               ADD 1             TO W60-S
               GO TO EA05.
             DISPLAY S06.
             PERFORM AA100.
       EA06.
      MOVE "'S'ales ledger or 'I'tem sequence  [I]"
     TO WS-ERR-MES.
      MOVE "I"   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
    IF NOT(WS-OPTION = "I" OR "S")
               GO TO EA06.
             DISPLAY S07.
             MOVE "!"            TO STK-CODE.
           IF WS-OPTION = "I"
               PERFORM START-AT-STOCK-CODE THRU READ-STOCK-EXIT
           ELSE
        MOVE "!"   TO STK-LDG
               PERFORM START-AT-STOCK-LEDG THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO EA900.
       EA10.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO EA900.
    IF STK-IND = 4
        GO TO EA10.
      *    ****    A V E R A G E   C O S T
    IF STK-QUANT < 0.001
        MOVE STK-COST  TO STK-AVGE.
    IF WS-USE-ITM = "Y"
        DISPLAY S08-ITM
    ELSE
    IF WS-EXT-STK = "Y"
        DISPLAY S08-EXT
    ELSE
        DISPLAY S08.
      PERFORM AA600.
             PERFORM AA400.
             MOVE W55-EXP-DATA   TO EXP-REC1.
             PERFORM WRITE-STKDET THRU WRITE-STKDET-EXIT.
             GO TO EA10.
       EA900.
             CLOSE STKDET.
       EA999.
             EXIT.
      /
      *    ****    E X P O R T :
      *
      *                S E L E C T E D   F I E L D S
      *
       FA000        SECTION 5.
       FA05.
             PERFORM ERASE-SCREEN.
             DISPLAY S02.
             DISPLAY S05.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO FA999.
      INITIALIZE W60-FIELDS.
             MOVE 1              TO W60-S.
            DISPLAY " " AT 1803 WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     " to view the fields available "
                      WITH FOREGROUND-COLOR Grey
                     "S" WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     "elect/" WITH FOREGROUND-COLOR Grey
                     "A" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "ccept - " WITH FOREGROUND-COLOR Grey
                     "Esc" WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     "ape" WITH FOREGROUND-COLOR Grey.
             MOVE 1              TO W65-S.
       FA10.
      MOVE W65-FLD(W65-S) TO W65-FIELD.
             DISPLAY S09.
       FA15.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      DISPLAY SPACE AT 5041
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
       IF W65-S = 1
    GO TO FA15
       ELSE
    SUBTRACT 1  FROM W65-S
    GO TO FA10
       END-IF
   WHEN DOWN-KEY
       IF W65-S NOT < 90
    GO TO FA15
       ELSE
    ADD 1  TO W65-S
    GO TO FA10
       END-IF
   WHEN ENTER-KEY
   WHEN CR-KEY
       GO TO FA18
   WHEN OTHER
       PERFORM AA900-ALARM
        END-EVALUATE
        GO TO FA15
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO FA05
   WHEN OTHER
       PERFORM AA900-ALARM
        END-EVALUATE
        GO TO FA15
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
           IF NOT (WS-OPTION = "A" OR "S")
               GO TO FA15.
           IF WS-OPTION = "A"
               GO TO FA30.
       FA18.
             MOVE 1              TO WS-S1.
       FA20.
           IF WS-S1 = W60-S
               GO TO FA25.
    IF W60-FNO(WS-S1) = W65-S
               GO TO FA15.
             ADD 1               TO WS-S1.
             GO TO FA20.
       FA25.
      MOVE W65-S   TO W60-FNO(W60-S).
             DISPLAY S06.
             ADD 1               TO W60-S.
    IF W65-S < 90
               ADD 1             TO W65-S.
             GO TO FA10.
       FA30.
             PERFORM AA100.
             DISPLAY S07.
       FA32.
      MOVE "'S'ales ledger or 'I'tem sequence  [I]"
     TO WS-ERR-MES.
      MOVE "I"   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
    IF NOT (WS-OPTION = "I" OR "S")
               GO TO FA32.
             MOVE "!"            TO STK-CODE.
           IF WS-OPTION = "I"
               PERFORM START-AT-STOCK-CODE THRU READ-STOCK-EXIT
           ELSE
        MOVE "!"   TO STK-LDG
               PERFORM START-AT-STOCK-LEDG THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO FA900.
       FA35.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO FA900.
    IF STK-IND = 4
        GO TO FA35.
      *    ****    A V E R A G E   C O S T
    IF STK-QUANT < 0.001
        MOVE STK-COST  TO STK-AVGE.
    IF WS-USE-ITM = "Y"
        DISPLAY S08-ITM
    ELSE
    IF WS-EXT-STK = "Y"
        DISPLAY S08-EXT
    ELSE
        DISPLAY S08.
      PERFORM AA600.
             PERFORM AA400.
             MOVE W55-EXP-DATA   TO EXP-REC1.
             PERFORM WRITE-STKDET THRU WRITE-STKDET-EXIT.
             GO TO FA35.
       FA900.
             CLOSE STKDET.
       FA999.
             EXIT.
      *
      *    ****    SELECT ITEMS TO EXTRACT
      *
       GA000        SECTION 5.
       GA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S02.
       GA05.
      MOVE "'A'mend existing, 'C'reate new file  [C]"
     TO WS-ERR-MES.
             MOVE "C"            TO WS-OPTION.
      PERFORM OPT-MESSAGE.
    IF NOT (WS-OPTION = "A" OR "C")
               GO TO GA05.
             MOVE WS-OPTION      TO WS-SKIP.
      DISPLAY S10.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO GA999.
    IF WS-SKIP = "C"
               OPEN OUTPUT STKSEL
               CLOSE STKSEL.
             OPEN I-O STKSEL.
             MOVE 1              TO WS-SELKEY.
           IF WS-SKIP = "A"
               GO TO GA25.
       GA10.
      DISPLAY WS-BLNK78 AT 0602 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY S10.
            MOVE SPACE   TO W10-EXT-ITEM.
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

       GA15.
             PERFORM AA700.
    IF W10-STCK = "A"
        IF WS-EXT-STK = "Y"
     ACCEPT S11-EXT
        ELSE
        IF WS-USE-ITM = "Y"
     ACCEPT S11-ITM
        ELSE
           ACCEPT S11
        END-IF
    ELSE
    IF W10-SLNGTH = 3
        ACCEPT S11-3
    ELSE
    IF W10-SLNGTH = 4
        ACCEPT S11-4
    ELSE
    IF W10-SLNGTH = 5
        ACCEPT S11-5
    ELSE
    IF W10-SLNGTH = 6
        ACCEPT S11-6
    ELSE
    IF W10-SLNGTH = 7
        ACCEPT S11-7
    ELSE
    IF W10-SLNGTH = 8
        ACCEPT S11-8
    ELSE
    IF W10-SLNGTH = 9
        ACCEPT S11-9
    ELSE
    IF W10-SLNGTH = 10
        ACCEPT S11-10
    ELSE
    IF W10-SLNGTH = 11
        ACCEPT S11-11
    ELSE
    IF W10-SLNGTH = 12
        ACCEPT S11-12
    ELSE
    IF W10-SLNGTH = 13
        ACCEPT S11-13
    ELSE
    IF W10-SLNGTH = 14
        ACCEPT S11-14.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-CODE2
       GO TO GA25
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
       GO TO GA15
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
       GO TO GA15
        END-EVALUATE
        PERFORM AA50-LOOKUP
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-EXT-STK = "Y"
         DISPLAY S11-EXT
     ELSE
     IF WS-USE-ITM = "Y"
         DISPLAY S11-ITM
     ELSE
         DISPLAY S11
     END-IF
        ELSE
     DISPLAY S11
        END-IF
        IF W10-EXT-ITEM = SPACES
                   GO TO GA15.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
            PERFORM CLEAR-L50.
          IF W10-EXT-ITEM = SPACE
               GO TO GA99.
            MOVE 18   TO W50-LENGTH.
       GA20.
            MOVE W10-EXT-ITEM   TO W50-DATA.
           IF W50-1 = SPACE
        MOVE W50-17  TO W10-EXT-ITEM
               GO TO GA20.
      CALL "CBL_TOUPPER" USING W50-DATA
    BY VALUE W50-LENGTH
    RETURNING W50-STATUS.
            MOVE W50-EXT-ITEM  TO W10-EXT-ITEM
                                    STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
              MOVE "No record"  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO GA15.
      DISPLAY STK-DESC AT 0624
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
    IF STK-IND = 4
        MOVE "Header Record"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO GA15.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO GA15.
      MOVE STK-CODE  TO SEL-CODE.
             WRITE SEL-REC.
             ADD 1               TO WS-SELKEY.
             GO TO GA10.
       GA25.
             READ STKSEL.
           IF WS-STATUS NOT = "00"
               MOVE 1            TO WS-IND
               GO TO GA10.
             MOVE WS-SELKEY      TO WS-KEYSTR.
      DISPLAY SEL-CODE AT 0609
       WITH FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Magenta.
           IF SEL-CODE = SPACES
        DISPLAY "No details" AT 0624 WITH FOREGROUND-COLOR Brown HIGHLIGHT
               GO TO GA30.
             MOVE SEL-CODE       TO STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               DISPLAY "Invalid item code" AT 0624
                        WITH FOREGROUND-COLOR Brown HIGHLIGHT
               GO TO GA30.
      DISPLAY STK-DESC AT 0624
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
       GA30.
             DISPLAY X"182019" AT 2303 WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     " view selection, " WITH FOREGROUND-COLOR Grey
                     "A" WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     "mend/" WITH FOREGROUND-COLOR Grey
                     "D" WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     "elete item, " WITH FOREGROUND-COLOR Grey
                     "E" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "nd" WITH FOREGROUND-COLOR Grey.

       GA35.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
       IF WS-SELKEY = 1
    GO TO GA35
       ELSE
    SUBTRACT 1  FROM WS-SELKEY
    GO TO GA25
       END-IF
   WHEN DOWN-KEY
       IF WS-SELKEY NOT < WS-KEYSTR
    IF WS-IND = 1
        GO TO GA35
    END-IF
    ADD 1   TO WS-SELKEY
    GO TO GA25
       END-IF
       PERFORM AA900-ALARM
        END-EVALUATE
        GO TO GA35
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO GA99
   WHEN OTHER
       PERFORM AA900-ALARM
        END-EVALUATE
        GO TO GA35
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
           IF NOT (WS-OPTION = "A" OR "D")
               GO TO GA35.
           IF WS-OPTION = "A"
               GO TO GA40.
             MOVE SPACES         TO SEL-CODE
             GO TO GA55.
       GA40.
            MOVE SEL-CODE  TO W10-EXT-ITEM.
       GA45.
             PERFORM AA700.
             ACCEPT S11.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-CODE2
       GO TO GA25
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
       GO TO GA45
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
       GO TO GA45
        END-EVALUATE
        PERFORM AA50-LOOKUP
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-EXT-STK = "Y"
         DISPLAY S11-EXT
     ELSE
     IF WS-USE-ITM = "Y"
         DISPLAY S11-ITM
     ELSE
         DISPLAY S11
     END-IF
        ELSE
     DISPLAY S11
        END-IF
        IF W10-EXT-ITEM = SPACES
                   GO TO GA45.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
            PERFORM CLEAR-L50.
          IF W10-EXT-ITEM = SPACE
               GO TO GA25.
            MOVE 18   TO W50-LENGTH.
       GA50.
            MOVE W10-EXT-ITEM   TO W50-DATA.
           IF W50-1 = SPACE
        MOVE W50-17  TO W10-EXT-ITEM
               GO TO GA50.
      CALL "CBL_TOUPPER" USING W50-DATA
    BY VALUE W50-LENGTH
    RETURNING W50-STATUS.
            MOVE W50-EXT-ITEM  TO W10-EXT-ITEM
                                    STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
              MOVE "No record"  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO GA45.
      DISPLAY STK-DESC AT 0624
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
    IF STK-IND = 4
        MOVE "Header Record"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO GA45.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO GA45.
      MOVE STK-CODE  TO SEL-CODE.
       GA55.
             REWRITE SEL-REC.
             ADD 1               TO WS-SELKEY.
             GO TO GA25.
       GA99.
             CLOSE STKSEL.
       GA999.
             EXIT.
      /
      *    ****    F I E L D   S E P A R A T O R
      *
       HA000        SECTION 5.
       HA00.
             PERFORM ERASE-SCREEN.
      DISPLAY S-PROG.
      DISPLAY S12.
       HA05.
      ACCEPT W50-EXP AT 0618
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
       HA999.
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
            MOVE 3   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-USE-ITM  TO WS-USE-ITM.
      MOVE PAR-EXT-STK  TO WS-EXT-STK.
      MOVE PAR-PACKS  TO WS-USE-PACKS.
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
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DMY        TO W12-TODAY.
            MOVE PAR-COMPANY  TO W95-COMP.
       ZA05.
             MOVE ","            TO W50-EXP.
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
               WHEN 7     MOVE W02-DEPART   TO WS-FILE
                          MOVE DPT-CODE     TO WS-KEYX
               WHEN 15    MOVE WS-PARID     TO WS-FILE
                          MOVE WS-PARKEY    TO WS-KEY
               WHEN 22    MOVE W02-STOCKF   TO WS-FILE
                          MOVE STK-CODE     TO WS-KEYX
          ELSE
           IF WS-F-ERROR = 99
               MOVE W02-STKDET   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STPEXP"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
