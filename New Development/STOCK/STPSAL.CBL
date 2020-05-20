      $SET LINKCOUNT"576" GNT"STPSAL.GNT"
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
      *    ******   ********  *******    ******     ****    **         *
      *   **    **     **     **    **  **    **   **  **   **         *
      *   **           **     **    **  **        **    **  **         *
      *    ******      **     *******    ******   ********  **         *
      *         **     **     **              **  **    **  **         *
      *   **    **     **     **        **    **  **    **  **         *
      *    ******      **     **         ******   **    **  ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S A L E S   T R A C K I N G   P R O G R A M                *
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
       PROGRAM-ID.     STPSAL.
       AUTHOR.         J.W.LEMMON.
       DATE-WRITTEN.   OCTOBER 1989.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1989 - 2018
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

       COPY "CARDEX.SL".

       COPY "CONTROL.SL".

       COPY "DEPART.SL".

       COPY "INVOIC.SL".

       COPY "PARAM.SL".

       COPY "STOCK.SL".

       COPY "STKMEM.SL".

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

       COPY "CARDEX.FDE".

       COPY "CONTROL.FDE".

       COPY "DEPART.FDE".

       COPY "INVOIC.FDE".

       COPY "PARAM.FDE".

       COPY "STOCK.FDE".

       COPY "STKMEM.FDE".

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
       77  WS-S            PIC  9(04)    COMP-5.
       77  WS-S1           PIC  9(04)    COMP-5.
       77  WS-S2           PIC  9(04)    COMP-5.
       77  WS-S3           PIC  9(04)    COMP-5.
       77  WS-S4           PIC  9(04)    COMP-5.
       77  WS-S5           PIC  9(04)    COMP-5.
       77  WS-S6           PIC  9(04)    COMP-5.
       77  WS-S7           PIC  9(04)    COMP-5.
       77  WS-S8           PIC  9(04)    COMP-5.
       77  WS-S9           PIC  9(04)    COMP-5.
       77  WS-IXS1         PIC  9(04)    COMP-5.
       77  WS-ENQPOS    PIC 9(06)  COMP-5.
       77  WS-ENQEND       PIC  9(06)    COMP-5.
       77  WS-NETKEY    PIC 9(06)  COMP-5.
       77  WS-PARKEY       PIC  9(06)    COMP-5.
       77  WS-KEYSTR       PIC  9(06)    COMP-5.
       77  WS-OPTION       PIC  X(01).
       77  WS-USE-ITM    PIC X(01).
       77  WS-EXT-STK    PIC X(01).
       77  WS-FORMAT    PIC X(01).
       77  WS-PAGE    PIC 9(04)  COMP-5 VALUE ZERO.
       77  WS-ITMHDG    PIC X(01).
       77  WS-MONTH    PIC 9(02).
       77  WS-ETYPE    PIC X(01).
       77  WS-DESC-H1     PIC  X(14) VALUE "Description".
       77  WS-DESC-H2     PIC  X(14) VALUE "Description 2".
       77  WS-RT-HD     PIC  X(10) VALUE "-Retail   ".
       77  WS-WS-HD     PIC  X(10) VALUE "-Wholesale".
       77  WS-CS-HD     PIC  X(10) VALUE "-Cash sale".
       77  WS-ERROR    PIC 9(01) VALUE ZERO.
       77  WS-ERR2         PIC  X(09) VALUE "No Record".
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STPSAL".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY -SALES TRACKING".
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
    03  WS-MODULE       PIC  X(03) VALUE "DTP".
    03  WS-PROG        PIC  X(03) VALUE "ENQ".

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

    03  R-L1 REDEFINES W02-PRN-LINE.
        05  R-DETAIL.
     07  R-HL    PIC X(96).
     07  FILLER    PIC X(36).

    03  R-L2 REDEFINES W02-PRN-LINE.
        05  R-PGE    PIC X(06).
        05  R-PNO    PIC Z(03)9.
        05  FILLER    PIC X(06).
        05  R-HEAD.
     07  FILLER    PIC X(03).
     07  R-CPY    PIC X(42).
        05  FILLER    PIC X(03).
        05  R-DATE    PIC X(06).
        05  R-DTE    PIC Z9/99/9999.

    03  SRP-L1 REDEFINES W02-PRN-LINE.
        05  SRP-H1    PIC X(06).
        05  SRP-EXT-ITEM.
     07  SRP-ITEM    PIC X(14).
     07  SRP-SL    PIC X(01).
     07  SRP-ITM    PIC X(03).
        05  FILLER    PIC X(01).
        05  SRP-DESC    PIC X(30).
        05  FILLER    PIC X(25).

    03  SRP-L2 REDEFINES W02-PRN-LINE.
        05  SRP-H2    PIC X(51).
        05  SRP-H3    PIC X(29).

    03  SRP-L4 REDEFINES W02-PRN-LINE.
        05  SRP-H4    PIC X(11).
        05  SRP-H5    PIC X(07).
        05  SRP-H6    PIC X(07).
        05  SRP-H7    PIC X(32).
        05  SRP-H8    PIC X(13).
        05  SRP-H9    PIC X(13).
        05  SRP-H10    PIC X(13).

    03  SRP-L5 REDEFINES W02-PRN-LINE.
        05  SRP-REF    PIC X(08).
        05  FILLER    PIC X(01).
        05  SRP-DATE-2    PIC Z9/99/99.
        05  FILLER    PIC X(01).
        05  SRP-ACNO    PIC X(06).
        05  FILLER    PIC X(01).
        05  SRP-NAME    PIC X(30).
        05  FILLER    PIC X(01).
        05  SRP-COST    PIC Z(07)9.99-.
        05  FILLER    PIC X(01).
        05  SRP-SELL    PIC Z(07)9.99-.
        05  FILLER    PIC X(01).
        05  SRP-QUANT    PIC Z(05)9.99-.
        05  FILLER    PIC X(01).
        05  SRP-TYPE    PIC X(01).

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

       01  W15-DATES.
          03  W15-START       PIC 9(08).
           03  W15-SDTE-DMY REDEFINES W15-START.
               05  W15-SDAY    PIC 9(02).
               05  W15-SMNTH   PIC 9(02).
        05  W15-SCENT   PIC 9(02).
               05  W15-SYEAR   PIC 9(02).
          03  W15-S-YMD       PIC 9(08).
           03  W15-STRT-YMD REDEFINES W15-S-YMD.
        05  W15-SCC     PIC 9(02).
               05  W15-SYY     PIC 9(02).
               05  W15-SMM     PIC 9(02).
               05  W15-SDD     PIC 9(02).
          03  W15-END        PIC 9(08).
           03  W15-EDTE-DMY REDEFINES W15-END.
               05  W15-EDAY    PIC 9(02).
               05  W15-EMNTH   PIC 9(02).
        05  W15-ECENT   PIC 9(02).
               05  W15-EYEAR   PIC 9(02).
          03  W15-E-YMD       PIC 9(08).
           03  W15-END-YMD REDEFINES W15-E-YMD.
        05  W15-ECC     PIC 9(02).
               05  W15-EYY     PIC 9(02).
               05  W15-EMM     PIC 9(02).
               05  W15-EDD     PIC 9(02).

       01  W15-ALLOCATIONS.
           03  W15-DETAIL.
             05  W15-TRAN OCCURS 15.
               07  W15-VALUE   PIC  Z(06)9.99- BLANK WHEN ZERO.
               07  W15-ALLOC   PIC  Z(06)9.99- BLANK WHEN ZERO.
        07  W15-REC     PIC  X(08).
           03  W15-DISPLAY REDEFINES W15-DETAIL.
               05  W15-BAL1    PIC  X(11).
               05  W15-ALL1    PIC  X(11).
        05  W15-REC1    PIC  X(08).
               05  W15-BAL2    PIC  X(11).
               05  W15-ALL2    PIC  X(11).
        05  W15-REC2    PIC  X(08).
               05  W15-BAL3    PIC  X(11).
               05  W15-ALL3    PIC  X(11).
        05  W15-REC3    PIC  X(08).
               05  W15-BAL4    PIC  X(11).
               05  W15-ALL4    PIC  X(11).
        05  W15-REC4    PIC  X(08).
               05  W15-BAL5    PIC  X(11).
               05  W15-ALL5    PIC  X(11).
        05  W15-REC5    PIC  X(08).
               05  W15-BAL6    PIC  X(11).
               05  W15-ALL6    PIC  X(11).
        05  W15-REC6    PIC  X(08).
               05  W15-BAL7    PIC  X(11).
               05  W15-ALL7    PIC  X(11).
        05  W15-REC7    PIC  X(08).
               05  W15-BAL8    PIC  X(11).
               05  W15-ALL8    PIC  X(11).
        05  W15-REC8    PIC  X(08).
               05  W15-BAL9    PIC  X(11).
               05  W15-ALL9    PIC  X(11).
        05  W15-REC9    PIC  X(08).
               05  W15-BAL10   PIC  X(11).
               05  W15-ALL10   PIC  X(11).
        05  W15-REC10   PIC  X(08).
               05  W15-BAL11   PIC  X(11).
               05  W15-ALL11   PIC  X(11).
        05  W15-REC11   PIC  X(08).
               05  W15-BAL12   PIC  X(11).
               05  W15-ALL12   PIC  X(11).
        05  W15-REC12   PIC  X(08).
               05  W15-BAL13   PIC  X(11).
               05  W15-ALL13   PIC  X(11).
        05  W15-REC13   PIC  X(08).
               05  W15-BAL14   PIC  X(11).
               05  W15-ALL14   PIC  X(11).
        05  W15-REC14   PIC  X(08).
               05  W15-BAL15   PIC  X(11).
               05  W15-ALL15   PIC  X(11).
        05  W15-REC15   PIC  X(08).

       01  W16-DATES-6.
    03  W16-DATE.
        05  W16-DD      PIC  9(02).
        05  W16-MM      PIC  9(02).
        05  W16-YY      PIC  9(02).
    03  W16-DTE REDEFINES W16-DATE
          PIC  9(06).

       COPY "W25.STK".

       COPY "W40.DBT".
       
       01  W40-BALANCES.
           03  W40-A-MTD       PIC S9(09)V99 COMP-3.
           03  W40-A-CUR       PIC S9(09)V99 COMP-3.
           03  W40-A-30        PIC S9(09)V99 COMP-3.
           03  W40-A-60        PIC S9(09)V99 COMP-3.
           03  W40-A-90        PIC S9(09)V99 COMP-3.
           03  W40-A-120       PIC S9(09)V99 COMP-3.
           03  W40-A-INT       PIC S9(09)V99 COMP-3.
           03  W40-D-MTD       PIC S9(09)V99 COMP-3.
           03  W40-D-CUR       PIC S9(09)V99 COMP-3.
           03  W40-D-30        PIC S9(09)V99 COMP-3.
           03  W40-D-60        PIC S9(09)V99 COMP-3.
           03  W40-D-90        PIC S9(09)V99 COMP-3.
           03  W40-D-120       PIC S9(09)V99 COMP-3.
           03  W40-D-INT       PIC S9(09)V99 COMP-3.
           03  W40-T-MTD       PIC S9(09)V99 COMP-3.
           03  W40-T-CUR       PIC S9(09)V99 COMP-3.
           03  W40-T-30        PIC S9(09)V99 COMP-3.
           03  W40-T-60        PIC S9(09)V99 COMP-3.
           03  W40-T-90        PIC S9(09)V99 COMP-3.
           03  W40-T-120       PIC S9(09)V99 COMP-3.
           03  W40-T-INT       PIC S9(09)V99 COMP-3.

       COPY "W40.WS".

       01  W45-TRAN.
           03  W45-CODE        PIC 9(02).
           03  W45-ENG         PIC X(12).
           03  W45-AFR         PIC X(12).
           03  W45-ACT         PIC X(01).

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W55-HEADING.
    03  FILLER       PIC  X(22) VALUE "Sales for the period:".
    03  W55-START      PIC  99/99/9999.
    03  FILLER       PIC  X(03) VALUE " - ".
    03  W55-END       PIC  99/99/9999.

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
    03  W75-S2       PIC 9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
          03  W75-DNO       PIC X(06) OCCURS 18.
    03  W75-RNO       PIC X(08) OCCURS 16.
    03  W75-DATE       PIC 9(06) OCCURS 16.
    03  W75-SM       PIC 9(02) OCCURS 16.

       01  W75-STORE.
          03  W75-SNAME      PIC  X(08).
    03  W75-SREF       PIC  X(08).
    03  W75-SACN       PIC  X(06).
    03  W75-SDTE       PIC  9(06).
    03  W75-SSM       PIC  9(02).

       01  W80-ACCOUNT.
           03  W80-ACNO        PIC X(06).
           03  W80-TYPE        PIC 9(01).

       01  W95-STM.
           03  W95-COMP        PIC X(40).
           03  W95-V1.
               05  W95-T       PIC Z(04)9.99.
           03  W95-V2.
               05  W95-B       PIC Z(06)9.99-.
           03  W95-D.
               05  W95-DTE     PIC Z9/99/99.
           03  W95-POST        PIC 9(04).
           03  W95-TEL         PIC X(08).

       01  W100-EDIT.
          03  W100-COST       PIC Z(06)9.99.
          03  W100-SELL       PIC Z(06)9.99.
           03  W100-CUR        PIC Z(08)9.99-.
           03  W100-30         PIC Z(08)9.99-.
           03  W100-60         PIC Z(08)9.99-.
           03  W100-90         PIC Z(08)9.99-.
           03  W100-120        PIC Z(08)9.99-.
           03  W100-INT        PIC Z(08)9.99.
           03  W100-CRL        PIC Z(06)9.
           03  W100-KEY        PIC Z(05)9.
    03  W100-SER        PIC ZZ9.
          03  W100-PRC        PIC Z(06)9.99-.
          03  W100-VAL        PIC Z(08)9.99-.
           03  W100-V2.
               05  W100-S6V2   PIC Z(05)9.99-.
          03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
          03  W100-V4 REDEFINES W100-V2.
               05  W100-EXT    PIC Z(06)9.99.
           03  W100-V5 REDEFINES W100-V2.
        05  FILLER      PIC  X(02).
        05  W100-QNT    PIC Z(04)9.99.

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

       01  S05D.
    03  LINE  4 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
    03  LINE  5 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "³    Transactions for Period.    ³".
    03  LINE  6 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
    03  LINE  7 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "³   Starting Date :              ³".
    03  LINE  8 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "³   Ending Date   :              ³".
    03  LINE  9 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
    03  LINE 10 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "³Dates to be keyed in as ".
    03        COLUMN 49 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE "DDMMCCYY".
    03        COLUMN 57 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE "³".
    03  LINE 11 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S05A.
    03  LINE  7 COLUMN 44 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC 99/99/9999 USING W15-START AUTO.
    03  LINE  8 COLUMN 44 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC 99/99/9999 USING W15-END AUTO.

       01  S10D.
          03  LINE  2 COLUMN 21 FOREGROUND-COLOR Grey HIGHLIGHT VALUE
               "SALES TRACKING MOVEMENT/REFERENCE LOOKUP".
    03  LINE  3 COLUMN  3 VALUE "Item Code.".

       01  S36.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC X(14) USING W10-ITEM AUTO.

       01  S36-ITM.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC X(14)/X(03)
      USING W10-EXT-ITEM AUTO.

       01  S36-EXT.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC X(18) USING W10-EXT-ITEM AUTO.

       01  S36-3.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(03) USING W10-I4 AUTO.

       01  S36-4.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(04) USING W10-I4 AUTO.

       01  S36-5.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(05) USING W10-I5 AUTO.

       01  S36-6.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(06) USING W10-I6 AUTO.

       01  S36-7.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(07) USING W10-I7 AUTO.

       01  S36-8.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(08) USING W10-I8 AUTO.

       01  S36-9.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(09) USING W10-I9 AUTO.

       01  S36-10.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(10) USING W10-I10 AUTO.

       01  S36-11.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(11) USING W10-I11 AUTO.

       01  S36-12.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(12) USING W10-I12 AUTO.

       01  S36-13.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(13) USING W10-I13 AUTO.

       01  S36-14.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(14) USING W10-I14 AUTO.

       COPY STKLUP.CRT.

       01  S-MOV.
           03  LINE  4 COLUMN  2 REVERSE-VIDEO VALUE
              "Ref. No.   Date   Ac/No  SP Cost Price Sell.Price Quantity Customer Name      ".
          03  LINE  5 COLUMN  1 BACKGROUND-COLOR Black VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÑÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR Black VALUE "µ".
          03  LINE 22 COLUMN  1 BACKGROUND-COLOR Black VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÏÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR Black VALUE "µ".

       01  SA-ACT.
          03  LINE  3 COLUMN 17 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC  X(06) USING W80-ACNO AUTO.

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
      *    *************************************************************
      *      To control Sales Tracking security it is necessary to use
      *      two of the levels in the System.
      *      A Stock level of 3 will allow sales tracking.
      *      Where the stock level is less than 3, then by giving
      *      the user a sales level of 3, sales tracking will be
      *      enabled.
      *    *************************************************************
          IF (LS0-STLEV < 3) AND (LS0-SALEV < 3)
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
        GO TO AA49.
            PERFORM ZA000-INIT.
      MOVE "S"   TO WS-OPTION.

       AA05.
      MOVE "'S'creen,'R'eport format or 'E'xit  [S]"
     TO WS-ERR-MES.
      PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "S" OR "R" OR "E")
        GO TO AA05.
    IF WS-OPTION = "E"
        GO TO AA49.
      MOVE WS-OPTION  TO WS-FORMAT.
      PERFORM BA000.

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

      *    *************************************************************
      *    ****        D A T E  C H E C K   R O U T I N E
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Perform this routine with the date in W10-DATE (PIC      ³
      *    ³  99/99/9999) with the format as DD/MM/CCYY         ³
      *    ³              ³
      *    ³  On exiting - WS-ERROR will be zero (0) if the date is    ³
      *    ³  correct or one(1)  if the date is incorrect.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "DATE.CHK".

       COPY "ABORT.PRN".

       COPY "PRINTUTL.AP3".

       COPY "AA300.STK".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000              SECTION.

       COPY "CARDEX.RD".

       COPY "CONTROL.RD".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "INVOIC.RD".

       COPY "STOCK.RD".

       COPY "STKMEM.RD".

      *    *************************************************************
      *    ****   E N Q U I R Y   ( S A L E S   T R A C K I N G )
      *    *************************************************************
       BA000         SECTION.
       BA00.
             PERFORM ERASE-SCREEN.
             PERFORM SAVE-SCREEN-3.
             MOVE 5                  TO SHADE-ROW.
             MOVE 26                 TO SHADE-COL.
             MOVE 32                 TO SHADE-WIDTH.
             MOVE 7                  TO SHADE-LINES.
             DISPLAY S05D.
             PERFORM SCREEN-SHADOW.
             MOVE W12-TODAY  TO W15-START W15-END.

       BA05.
      DISPLAY "Press " AT 5052
        WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan
       "Esc" WITH FOREGROUND-COLOR Brown HIGHLIGHT
    BACKGROUND-COLOR Cyan
       "ape to Exit"
        WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan.
      ACCEPT S05A.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BA49
   WHEN OTHER  PERFORM AA900-ALARM
       GO TO BA05
        END-EVALUATE.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO BA05.
      PERFORM CLEAR-L50.
             MOVE W15-START          TO W22-DATE W55-START.
             PERFORM DATE-CHECK THRU CHECK-DATE-EXIT.
           IF NOT(W22-ERROR = 0)
               EVALUATE W22-ERROR
                 WHEN 1  MOVE "Invalid Start DATE"  TO WS-ERR-MES
                 WHEN 2  MOVE "Invalid Start DAY"   TO WS-ERR-MES
                 WHEN 3  MOVE "Invalid Start MONTH" TO WS-ERR-MES
               END-EVALUATE
               PERFORM ERROR-MESSAGE
               GO TO BA05.
             MOVE W20-DTE            TO W15-S-YMD.
             MOVE W15-END            TO W22-DATE W55-END.
             PERFORM DATE-CHECK THRU CHECK-DATE-EXIT.
           IF NOT(W22-ERROR = 0)
               EVALUATE W22-ERROR
                 WHEN 1  MOVE "Invalid End DATE"  TO WS-ERR-MES
                 WHEN 2  MOVE "Invalid End DAY"   TO WS-ERR-MES
                 WHEN 3  MOVE "Invalid End MONTH" TO WS-ERR-MES
               END-EVALUATE
               PERFORM ERROR-MESSAGE
               GO TO BA05.
             MOVE W20-DTE            TO W15-E-YMD.
           IF W15-E-YMD < W15-S-YMD
               MOVE "End date is before start date" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BA05.
             PERFORM RESTORE-SCREEN-3.

       BA10.
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

       BA15.
             PERFORM ERASE-SCREEN.
      DISPLAY WS-BLNK78 AT 0302
       WITH FOREGROUND-COLOR Cyan BACKGROUND-COLOR Blue.
      DISPLAY S10D.

       BA20.
      MOVE W12-MONTH  TO WS-MONTH.

       BA25.
            PERFORM AA300.
      *    *************************************************************
      *                     G E T   S T O C K   C O D E
      *    *************************************************************
      *    
      *      Is an 18 character stock/inventory item code being used?  
      *    
           IF WS-EXT-STK = "Y"
               ACCEPT S36-EXT AT 0314
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
                   ACCEPT S36-ITM AT 0314
               ELSE
                   ACCEPT S36 AT 0314
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
                 WHEN 3         ACCEPT S36-3 AT 0314
      *                     4 Digit numeric Stock/Inventory item code
                 WHEN 4         ACCEPT S36-4 AT 0314
      *                     5 Digit numeric Stock/Inventory item code
                 WHEN 5         ACCEPT S36-5 AT 0314
      *                     6 Digit numeric Stock/Inventory item code
                 WHEN 6         ACCEPT S36-6 AT 0314
      *                     7 Digit numeric Stock/Inventory item code
                 WHEN 7         ACCEPT S36-7 AT 0314
      *                     8 Digit numeric Stock/Inventory item code
                 WHEN 8         ACCEPT S36-8 AT 0314
      *                     9 Digit numeric Stock/Inventory item code
                 WHEN 9         ACCEPT S36-9 AT 0314
      *                    10 Digit numeric Stock/Inventory item code
                 WHEN 10        ACCEPT S36-10 AT 0314
      *                    11 Digit numeric Stock/Inventory item code
                 WHEN 11        ACCEPT S36-11 AT 0314
      *                    12 Digit numeric Stock/Inventory item code
                 WHEN 12        ACCEPT S36-12 AT 0314
      *                    13 Digit numeric Stock/Inventory item code
                 WHEN 13        ACCEPT S36-13 AT 0314
      *                    14 Digit numeric Stock/Inventory item code
                 WHEN 14        ACCEPT S36-14 AT 0314
               END-EVALUATE.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
             PERFORM CLEAR-L50
       GO TO BA49
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
       GO TO BA25
        END-EVALUATE
              PERFORM AA50-LOOKUP
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
        IF W10-STCK = "A"
     IF WS-EXT-STK = "Y"
         DISPLAY S36-EXT AT 0314
     ELSE
     IF WS-USE-ITM = "Y"
         DISPLAY S36-ITM AT 0314
     ELSE
         DISPLAY S36 AT 0314
     END-IF
        ELSE
     DISPLAY S36 AT 0314.

       BA30.
      PERFORM CLEAR-L50.
          IF W10-EXT-ITEM = SPACES
              GO TO BA25.

       BA35.
             PERFORM CA000.
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
    IF W10-STCK = "A"
        IF WS-EXT-STK = "Y"
     DISPLAY S36-EXT AT 0314
        ELSE
        IF WS-USE-ITM = "Y"
     DISPLAY S36-ITM AT 0314
        ELSE
           DISPLAY S36 AT 0314
        END-IF
    ELSE
        DISPLAY S36 AT 0314.
          IF WS-F-ERROR = 22
        MOVE "Z"   TO W10-ETYPE
              PERFORM AA50-LOOKUP
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
        IF W10-STCK = "A"
     IF WS-EXT-STK = "Y"
         DISPLAY S36-EXT AT 0314
     ELSE
     IF WS-USE-ITM = "Y"
         DISPLAY S36-ITM AT 0314
     ELSE
         DISPLAY S36 AT 0314
     END-IF
        ELSE
     DISPLAY S36 AT 0314
        END-IF
              IF W10-EXT-ITEM = SPACES
           GO TO BA25
               ELSE
     GO TO BA35.
      DISPLAY "Description" AT 0334.
      DISPLAY STK-DESC AT 0346
       WITH BACKGROUND-COLOR Magenta
     FOREGROUND-COLOR Cyan HIGHLIGHT.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO BA25.

       BA40.
    IF WS-FORMAT = "R"
        PERFORM MOVEMENT-REPORT
        IF WS-OPTION = "X"
     GO TO BA55
        END-IF
    ELSE
        PERFORM MOVEMENT-LOOKUP.
      MOVE SPACES  TO W80-ACNO.
      MOVE SPACE   TO WS-OPTION.

       BA45.
      MOVE "Change 'D'ates,'I'tem code or 'E'nd  [ ]"
     TO WS-ERR-MES.
            PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "D" OR "E" OR "I")
              GO TO BA45.
    IF WS-OPTION = "I"
        GO TO BA15
    ELSE
    IF WS-OPTION = "D"
        GO TO BA00.

       BA49.
    IF NOT (WS-FORMAT = "R")
        GO TO BA999.

       BA50.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE "******   E N D   O F   R E P O R T   ******"
     TO R-DETAIL.
      MOVE 99   TO WS-ADVANCE.
      MOVE "E"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.

       BA55.
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE 6   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
      MOVE "D"   TO W02-PRN-TYPE.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.

       BA999.
             EXIT.

      /    *************************************************************
      *    ****    M O V E M E N T   L O O K U P
      *    *************************************************************
       MOVEMENT-LOOKUP   SECTION 50.

       MOVE-LUP-SCREEN.
            DISPLAY S-MOV.
      INITIALIZE CRD-REC1.
      MOVE W10-EXT-ITEM  TO CRD-EXT-CODE.
      MOVE W15-S-YMD  TO CRD-DTE.

       MOVE-LUP-CLR-SCREEN.
             MOVE 0601           TO CPOS.
            PERFORM MOVE-CRT-EDGE UNTIL CLIN = 22.

       MOVE-LUP-CONTINUE.
            MOVE 6   TO CLIN.
             MOVE 1              TO W75-S W75-S1.

       MOVE-LUP-TAB-LOOP.
      MOVE SPACES  TO W75-DNO (W75-S1)
        W75-RNO (W75-S1).
      MOVE ZERO   TO W75-DATE (W75-S1)
        W75-SM (W75-S1).
    IF W75-S1 < 16
               ADD 1             TO W75-S1
        GO TO MOVE-LUP-TAB-LOOP.
             MOVE ZERO           TO W75-S1.

       MOVE-LUP-START-CRDX.
      MOVE SPACES  TO WS-OPTION.
            PERFORM START-AT-CRDX-SDTE THRU READ-CARDEX-EXIT.
          IF WS-F-ERROR = 43
        GO TO MOVE-NO-RECORD.

       MOVE-GET-CARDEX-REC.
           IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
        IF NOT (CRD-EXT-CODE = W10-EXT-ITEM)
     GO TO MOVE-FULL-SCREEN
        ELSE
     GO TO MOVE-SKIP-CRDX-READ.
            PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
    IF WS-F-ERROR = 43
               SUBTRACT 1        FROM W75-S
        GO TO MOVE-FULL-SCREEN.

       MOVE-LUP-DISP-ITEM.
    IF NOT (CRD-EXT-CODE = W10-EXT-ITEM)
        GO TO MOVE-FULL-SCREEN.
    IF (CRD-DTE > W15-E-YMD)
        GO TO MOVE-FULL-SCREEN.

       MOVE-SKIP-CRDX-READ.
            DISPLAY (CLIN, 2) CRD-REF WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      MOVE CRD-DTE  TO W22-DTE2.
      MOVE W22-YY2  TO W16-YY.
      MOVE W22-MM2  TO W16-MM.
      MOVE W22-DD2  TO W16-DD.
      MOVE W16-DTE  TO W95-DTE.
            DISPLAY ( , 11) W95-DTE WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            DISPLAY ( , 20) CRD-AC WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            DISPLAY ( , 27) CRD-SMAN WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      MOVE CRD-COST  TO W100-COST.
      MOVE CRD-SELL  TO W100-SELL.
      MOVE CRD-QNT  TO W100-QNT.
          IF LS-OK = "Y"
              DISPLAY ( , 30) W100-COST WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      DISPLAY ( , 41) W100-SELL WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      DISPLAY ( , 52) W100-QNT WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      DISPLAY ( , 61) CRD-NME WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
    IF CRD-SELL < ZERO
        DISPLAY ( , 78) " C" WITH FOREGROUND-COLOR Brown HIGHLIGHT
    ELSE
        DISPLAY ( , 78) " I" WITH FOREGROUND-COLOR Brown HIGHLIGHT.
           IF CLIN < 21
               ADD 1             TO CLIN W75-S
              GO TO MOVE-GET-CARDEX-REC.
      
       MOVE-FULL-SCREEN.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
001020      DISPLAY " " AT 2303 WITH FOREGROUND-COLOR Brown HIGHLIGHT
             " - " WITH FOREGROUND-COLOR Grey
       "PgUp" WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     "/" WITH FOREGROUND-COLOR Grey
       "PgDn" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       " - " WITH FOREGROUND-COLOR Grey
             "Esc" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "ape." WITH FOREGROUND-COLOR Grey.

       MOVE-SAVE-FULL-SCREEN.
      PERFORM SAVE-SCREEN-2.
            MOVE 0   TO W75-S1.
             MOVE 0601           TO CPOS.

       MOVE-CRT-ATTRIB.
      MOVE 3920   TO W41-LENGTH.
      CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
           W42-ATTRIB2
           W41-LENGTH
     RETURNING WS-STATUS.
            MOVE 4000   TO W41-LENGTH.

       MOVE-GET-USER-OPT.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM CLEAR-L50.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
             GO TO MOVE-ADJ-NO-KEYS
   WHEN DOWN-KEY
             GO TO MOVE-CHK-NO-KEYS
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO MOVE-GET-USER-OPT
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO MOVEMENT-LOOKUP-EXIT
   WHEN PAGE-DOWN
             GO TO MOVE-GET-NEXT-PAGE
   WHEN PAGE-UP
             GO TO MOVE-GET-PREV-PAGE
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO MOVE-GET-USER-OPT.
      PERFORM AA900-ALARM.
            GO TO MOVE-GET-USER-OPT.

       MOVE-GET-NEXT-PAGE.
           IF W75-S < 16
              GO TO MOVE-GET-USER-OPT.

       MOVE-NEW-PAGE.
             MOVE 0601           TO CPOS.
            PERFORM MOVE-CRT-EDGE UNTIL CLIN = 22.
             MOVE 6              TO CLIN.
             MOVE LOW-VALUES     TO W75-KEYS.
             MOVE 1              TO W75-S W75-S1.
      PERFORM MOVE-LUP-TAB-LOOP.
            GO TO MOVE-GET-CARDEX-REC.

       MOVE-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *    *************************************************************
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *    *************************************************************
       MOVE "J"    TO WS-OPTION.

       MOVE-GET-PREV-LOOP.
      PERFORM READ-CARDEX-PREV THRU READ-CARDEX-EXIT.
    IF WS-F-ERROR = 43
        MOVE SPACE  TO WS-OPTION
        GO TO MOVE-NEW-PAGE.
    IF NOT (CRD-EXT-CODE = W10-EXT-ITEM)
        PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT
        GO TO MOVE-NEW-PAGE.
    IF CRD-DTE < W15-START
        PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT
        GO TO MOVE-NEW-PAGE.
    IF WS-IXS1 < 32
        ADD 1   TO WS-IXS1
        GO TO MOVE-GET-PREV-LOOP.
      GO TO MOVE-NEW-PAGE.

       MOVE-NO-RECORD.
      PERFORM CLEAR-L50.
            DISPLAY "No record - Press " AT 5012
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "ANY" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             " key"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
            GO TO MOVEMENT-LOOKUP-EXIT.

       MOVE-ADJ-NO-KEYS.
           IF W75-S1 > 1
               SUBTRACT 1        FROM W75-S1 CLIN.
            GO TO MOVE-HIGHLIGHT.

       MOVE-CHK-NO-KEYS.
           IF W75-S1 = ZERO
               MOVE 1            TO W75-S1
              GO TO MOVE-HIGHLIGHT.
           IF W75-S1 < W75-S
               ADD 1             TO W75-S1 CLIN.

       MOVE-HIGHLIGHT.
            PERFORM MOVE-CRT-ATTRIB.
             DISPLAY W43-DET (CLIN) AT CPOS WITH REVERSE-VIDEO.
            GO TO MOVE-GET-USER-OPT.

       MOVE-CRT-EDGE.
            DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR Black
          FOREGROUND-COLOR Cyan
       "        ³        ³      ³  ³          ³          ³
      -       "        ³                   "
        WITH FOREGROUND-COLOR Cyan
       "³" WITH BACKGROUND-COLOR Black
         FOREGROUND-COLOR Cyan.
            ADD 1   TO CLIN.


       MOVEMENT-LOOKUP-EXIT.
             EXIT.
      /    *************************************************************
      *    ****    M O V E M E N T   R E P O R T
      *    *************************************************************
       MOVEMENT-REPORT   SECTION 51.

       MOVE-REP-ITEM.
      MOVE W10-EXT-ITEM  TO CRD-EXT-CODE.
      MOVE W15-S-YMD  TO CRD-DTE.
      MOVE "Y"   TO WS-ITMHDG.

       MOVE-REP-START-CRDX.
      MOVE SPACES  TO WS-OPTION.
            PERFORM START-AT-CRDX-SDTE THRU READ-CARDEX-EXIT.
          IF WS-F-ERROR = 43
        GO TO MOVE-REP-NO-RECORD.

       MOVE-REP-GET-CARDEX-REC.
            PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
    IF WS-F-ERROR = 43
        GO TO MOVEMENT-REPORT-PAUSE.

       MOVE-REP-CHECK-ITEM.
    IF NOT (CRD-EXT-CODE = W10-EXT-ITEM)
        GO TO MOVEMENT-REPORT-EXIT.
    IF (CRD-DTE > W15-E-YMD)
        GO TO MOVEMENT-REPORT-EXIT.
    IF WS-PAGE = ZERO
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *     **  *   *   * *   *  *  **  *   *   *     *   *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *     *  **   *     *   *  *  *  **   *   *     *  *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** *   *
      *
        MOVE 8   TO W02-PRN-TYPE
        PERFORM OPEN-PRINTER
        MOVE "P"   TO WS-COMMAND
        PERFORM BB000.
    IF WS-ITMHDG = "Y"
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        PERFORM BB05.
            MOVE SPACES  TO R-DETAIL.

       MOVE-PRINT-ITEM-READ.
            MOVE CRD-REF  TO SRP-REF.
      MOVE CRD-DTE  TO W22-DTE2.
      MOVE W22-YY2  TO W16-YY.
      MOVE W22-MM2  TO W16-MM.
      MOVE W22-DD2  TO W16-DD.
      MOVE W16-DTE  TO SRP-DATE-2.
            MOVE CRD-AC  TO SRP-ACNO.
      MOVE CRD-NAME  TO SRP-NAME.
          IF LS-OK = "Y"
        MOVE CRD-COST  TO SRP-COST.
      MOVE CRD-SELL  TO SRP-SELL.
      MOVE CRD-QNT  TO SRP-QUANT.
    IF CRD-SELL < ZERO
        MOVE "C"   TO SRP-TYPE
    ELSE
        MOVE "I"   TO SRP-TYPE.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM BB000.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
     TO R-DETAIL
        MOVE 99   TO WS-ADVANCE
        MOVE "X"   TO WS-OPTION
        PERFORM CALL-PRINTUTL
              GO TO MOVEMENT-REPORT-EXIT.
            MOVE SPACES  TO R-DETAIL.
      GO TO MOVE-REP-GET-CARDEX-REC.
      
       MOVE-REP-NO-RECORD.
      PERFORM CLEAR-L50.
            DISPLAY "No record - Press " AT 5012
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "ANY" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             " key"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
            GO TO MOVEMENT-REPORT-EXIT.

       MOVEMENT-REPORT-PAUSE.
    IF WS-ITMHDG = "Y"
        GO TO MOVE-REP-NO-RECORD.
      PERFORM CLEAR-L50.
            DISPLAY "Press " AT 5012
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "ANY" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             " key"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM CLEAR-L50.

       MOVEMENT-REPORT-EXIT.
             EXIT.

       BB000        SECTION 51.
       BB00.
      *    *************************************************************
      *    ****   1 O C P I ( N O R M A L P R I N T )
      *    *************************************************************
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE 1   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE SPACES  TO R-DETAIL.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE "PAGE:"  TO R-PGE.
             MOVE "DATE:"        TO R-DATE.
             ADD 1               TO WS-PAGE.
             MOVE WS-PAGE        TO R-PNO.
             MOVE W12-TODAY      TO R-DTE.
             MOVE W95-COMP       TO R-CPY.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DETAIL.
            MOVE W55-HEADING  TO R-HEAD.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DETAIL.
      *    *************************************************************
      *    ****   1 2 C P I
      *    *************************************************************
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE 2   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE SPACES  TO R-DETAIL.
      MOVE ALL "-"  TO R-HL.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DETAIL.
            MOVE "Ref. No."  TO SRP-H4.
            MOVE "Date"  TO SRP-H5.
      MOVE "Ac.No."  TO SRP-H6.
      MOVE "Customer Name"
     TO SRP-H7.
      MOVE "Cost Price"  TO SRP-H8.
      MOVE "Sell.Price"  TO SRP-H9.
      MOVE "Quantity"  TO SRP-H10.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE ALL "-"  TO R-HL.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DETAIL.
      *    *************************************************************
      *    ****   1 O C P I ( N O R M A L P R I N T )
      *    *************************************************************
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE 1   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
      MOVE "D"   TO W02-PRN-TYPE.
      *    *************************************************************
      *    ****    I T E M   H E A D I N G
      *    *************************************************************
       BB05.
            MOVE SPACES  TO R-DETAIL.
      MOVE "Item:"  TO SRP-H1.
      MOVE STK-CODE  TO SRP-EXT-ITEM.
      MOVE STK-DESC  TO SRP-DESC.
      MOVE 0   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DETAIL.
      *    *************************************************************
      *    ****   1 2 C P I
      *    *************************************************************
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE 2   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
      MOVE "N"   TO WS-ITMHDG.

       BB999.
      EXIT.

       COPY "APAC.HLP".

       COPY "CA000.STK".

       COPY "CA100.STK".

       COPY "CA200.STK".

       COPY "CA500.STK".

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
    IF NOT (PAR-STOCK = "A" OR "N")
        MOVE "A"   TO PAR-STOCK.
      MOVE PAR-STOCK  TO W10-STCK.
      MOVE PAR-USE-ITM  TO WS-USE-ITM.
      MOVE PAR-EXT-STK  TO WS-EXT-STK.
    IF PAR-STOCK = "N"
        MOVE PAR-SLNGTH  TO W10-SLNGTH
    ELSE
        MOVE 14   TO W10-SLNGTH.
    IF W10-SLNGTH < 3
        MOVE 3   TO W10-SLNGTH
    ELSE
    IF W10-SLNGTH > 14
        MOVE 14   TO W10-SLNGTH.
             MOVE 110            TO WS-USUB.
    IF LS-USER NUMERIC
        IF LS-USE > ZERO AND < 110
     MOVE LS-USE  TO WS-USUB.

       ZA02.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY    TO W95-COMP.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
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
            GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
            GO TO ZA205.
       ZA50.
          IF WS-F-ERROR = 43
              MOVE "CARDEX file"
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
               WHEN 7     MOVE W02-DEPART   TO WS-FILE
                          MOVE DPT-CODE     TO WS-KEYX
               WHEN 6     MOVE W02-DEBTOR   TO WS-FILE
                          MOVE DEB-ACNO     TO WS-KEYX
               WHEN 15    MOVE WS-PARID     TO WS-FILE
                          MOVE WS-PARKEY    TO WS-KEY
               WHEN 22    MOVE W02-STOCKF   TO WS-FILE
                          MOVE STK-CODE     TO WS-KEYX
               WHEN 25    MOVE W02-STKMEM   TO WS-FILE
                          MOVE SME-KEY      TO WS-KEYX
               WHEN 43    MOVE W02-CARDEX   TO WS-FILE
                          MOVE CRD-KEY      TO WS-KEYX
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STPSAL"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
