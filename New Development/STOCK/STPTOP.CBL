      $SET LINKCOUNT"384" GNT"STPTOP.GNT"
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
      *    ******   ********  *******   ********   ******   *******    *
      *   **    **     **     **    **     **     **    **  **    **   *
      *   **           **     **    **     **     **    **  **    **   *
      *    ******      **     *******      **     **    **  *******    *
      *         **     **     **           **     **    **  **         *
      *   **    **     **     **           **     **    **  **         *
      *    ******      **     **           **      ******   **         *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S A L E S   P E R F O R M A N C E   P R O G R A M          *
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
       PROGRAM-ID.     STPTOP.
       AUTHOR.         J.W.LEMMON.
       DATE-WRITTEN.   OCTOBER 1992.

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

       COPY "CARDEX.SL".

       COPY "DEPART.SL".

       COPY "PARAM.SL".

       COPY "STOCK.SL".

      $SET KEYCOMPRESS "7"
           SELECT SALES   ASSIGN DISK
                          STATUS WS-STATUS
                          ACCESS DYNAMIC
                          ORGANIZATION INDEXED
                          RECORD SAL-EXT-CODE
                          ALTERNATE RECORD SAL-QUANT DUPLICATES
                          ALTERNATE RECORD SAL-TSELL DUPLICATES
                          ALTERNATE RECORD SAL-GP DUPLICATES.
      $SET NOKEYCOMPRESS

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

       COPY "DEPART.FDE".

       COPY "PARAM.FDE".

       COPY "STOCK.FDE".

      /
       FD  SALES     IS EXTERNAL
       LABEL RECORD STANDARD
             VALUE OF FILE-ID W02-SALES.
       01  SAL-REC1.
          03  SAL-KEY.
        05  SAL-EXT-CODE.
     07  SAL-CODE PIC  X(14).
     07  SAL-ITM PIC  X(03).
     07  FILLER PIC  X(01).
    03  SAL-QUANT.
              05  SAL-QNT     PIC S9(07)V99 COMP.
    03  SAL-COST        PIC S9(09)V99 COMP.
    03  SAL-TSELL.
        05  SAL-SELL    PIC S9(09)V99 COMP.
    03  SAL-GP.
        05  SAL-PROFIT  PIC S9(09)V99 COMP.
    03  SAL-DESC        PIC  X(30).
    03  SAL-XREF        PIC  X(18).
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
       77  WS-S7    PIC 9(04)  COMP-5.
       77  WS-S8           PIC  9(04)    COMP-5.
       77  WS-S9           PIC  9(04)    COMP-5.
       77  WS-IXS1         PIC  9(04)    COMP-5.
       77  WS-CRDX    PIC 9(04)  COMP-5.
       77  WS-PAGE    PIC 9(04)  COMP-5.
       77  WS-ENQPOS    PIC 9(06)  COMP-5.
       77  WS-ENQEND       PIC  9(06)    COMP-5.
       77  WS-NETKEY       PIC  9(06)    COMP-5.
       77  WS-PARKEY       PIC  9(06)    COMP-5.
       77  WS-KEYSTR       PIC  9(06)    COMP-5.
       77  WS-OPTION       PIC  X(01).
       77  WS-LEDG    PIC X(04).
       77  WS-ENQ    PIC X(01).
       77  WS-MONTH    PIC 9(02).
       77  WS-ETYPE    PIC X(01).
       77  WS-DESC-H1     PIC  X(14) VALUE "Description".
       77  WS-DESC-H2     PIC  X(14) VALUE "Description 2".
       77  WS-RT-HD     PIC  X(10) VALUE "-Retail   ".
       77  WS-WS-HD     PIC  X(10) VALUE "-Wholesale".
       77  WS-CS-HD     PIC  X(10) VALUE "-Cash sale".
       77  WS-ERROR    PIC 9(01) VALUE ZERO.
       77  WS-ERR2         PIC  X(09) VALUE "No Record".
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STPTOP".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY -SALES PERFORMANCE".
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
    03  WS-PROG        PIC  X(03) VALUE "TOP".

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
        05  FILLER      PIC  X(01).
        05  REP-HEAD    PIC  X(94).
        05  FILLER      PIC  X(01).

       COPY "W05.VAT".

       COPY "W10.STK".

       COPY "W20.WS".

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

       COPY "W25.STK".

       01  W30-LINE.
    03  W30-DISPLAY.
        05  W30-CODE    PIC  X(19).
        05  W30-DESC    PIC  X(31).
        05  W30-QUANT   PIC  Z(06)9.99-.
        05  W30-VALUE   PIC  Z(08)9.99-.
    03  W30-EXTRA.
        05  W30-SPACE   PIC  X(01).
        05  W30-XREF    PIC  X(18).

       01  W35-HEADINGS.
    02  W35-PERIOD.
    03  FILLER  PIC  X(35)
    VALUE "Sales Performance for the period:- ".
    03  W35-START PIC  Z9/99/9999.
    03  FILLER  PIC  X(04) VALUE " to ".
    03  W35-END  PIC  Z9/99/9999.
    03  W35-REP  PIC  X(18).
    02  W35-HEADING.
    03  FILLER  PIC  X(86)
        VALUE "Item Code          Stock Description  Quantity        Value  X-Ref Code".

       01  W40-INDIC.
          03  W40-EDIT.
               05  W40-KEY     PIC ZZZZ9.
               05  W40-PERC    PIC ZZ9.99-.
           03  W40-EDIT2 REDEFINES W40-EDIT.
               05  FILLER      PIC X(01).
               05  W40-AMOUNT  PIC Z(06)9.99-.

       COPY "W40.WS".

       01  W45-TRAN.
           03  W45-CODE        PIC 9(02).
           03  W45-ENG         PIC X(12).
           03  W45-AFR         PIC X(12).
           03  W45-ACT         PIC X(01).

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W60-NME-ADD.
           03  W60-NAME.
               05  W60-NCHAR OCCURS 40 PIC X(01).
           03  W60-ADDRESS.
               05  W60-ACHAR OCCURS 70 PIC X(01).
           03  W60-ADDR REDEFINES W60-ADDRESS.
               05  W60-ADD     PIC X(60).
               05  FILLER      PIC X(10).
           03  W60-ADDRESS1.
               05  W60-PAD     PIC X(60).
               05  FILLER      PIC X(10).

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
    03  W75-S2       PIC 9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
           03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
           03  W75-DNO        PIC X(06) OCCURS 18.
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

       01  W200-TRAN.
           03  W200-ACCUM.
               05  W200-DT     PIC S9(07)V99 COMP-3 OCCURS 300.
               05  W200-CR     PIC S9(07)V99 COMP-3 OCCURS 300.
               05  W200-DTOT   PIC S9(07)V99 COMP-3.
               05  W200-CTOT   PIC S9(07)V99 COMP-3.
               05  W200-COST   PIC S9(07)V99 COMP-3.
               05  W200-SELL   PIC S9(07)V99 COMP-3.
               05  W200-NET    PIC S9(07)V99 COMP-3.
               05  W200-ADJ    PIC S9(07)V99 COMP-3.

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
          03  LINE  2 COLUMN 27 FOREGROUND-COLOR Grey
     HIGHLIGHT VALUE
               "SALES PERFORMANCE REPORTING".

       01  S-MENU.
    03  LINE  4 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
    03  LINE  5 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "SALES STATISTICS OPTIONS".
    03        COLUMN 55 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " ³".
    03  LINE  6 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "1".
    03        COLUMN 32 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " Stock Sales - Quantity ³".
    03  LINE  7 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "2".
    03        COLUMN 32 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "             - Value    ³".
          03  LINE  8 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "3".
    03        COLUMN 32 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "             - Profit   ³".
          03  LINE  9 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "4".
    03        COLUMN 32 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " Stock Gross Profit     ³".
          03  LINE 10 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "5".
    03        COLUMN 32 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " Stock Gross Profit-MTD ³".
          03  LINE 11 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "0".
    03        COLUMN 32 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " Exit                   ³".
    03  LINE 12 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S05D.
          03  LINE  2 COLUMN 27 FOREGROUND-COLOR Grey
     HIGHLIGHT VALUE
               "SALES PERFORMANCE REPORTING".
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

       01  S06.
    03  LINE  4 COLUMN  2 FOREGROUND-COLOR Grey HIGHLIGHT
         VALUE "Sales Performance for the period:- ".
    03        COLUMN 37 FOREGROUND-COLOR Brown HIGHLIGHT
     PIC Z9/99/9999 FROM W15-START.
    03        COLUMN 47 FOREGROUND-COLOR Grey HIGHLIGHT
     VALUE " to ".
    03        COLUMN 51 FOREGROUND-COLOR Brown HIGHLIGHT
     PIC Z9/99/9999 FROM W15-END.
               03  LINE  5 COLUMN  4 FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan
        VALUE "Item Code          Stock Description                       Quantity        Value ".

       01  S36.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC X(14) USING W10-ITEM AUTO.

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
           IF LS0-SALEV < 3
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
            PERFORM ZA000-INIT.
      PERFORM BA000.
      CLOSE SALES.
    IF WS-ENQ = "P"
        MOVE "C"   TO WS-COMMAND
        PERFORM CALL-PRINTUTL.

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

       COPY "DATE.CHK".

       COPY "ABORT.PRN".

       COPY "PRINTUTL.AS3".

      /
       AB000              SECTION.
       AB05.
      *
      *    ****   1 O C P I ( N O R M A L P R I N T )
      *
            MOVE SPACES  TO REP-DETAIL1.
      MOVE 0   TO WS-ADVANCE.
      MOVE 1   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE SPACES  TO REP-DETAIL1.
            MOVE "Page:"  TO REP-PAGE.
            MOVE "Date:"  TO REP-DATE.
            MOVE W95-COMP  TO REP-COMPANY.
             ADD 1               TO WS-PAGE.
            MOVE WS-PAGE  TO REP-P-NO.
            MOVE W12-TODAY  TO REP-DTE.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.

       AB10.
            MOVE W35-PERIOD  TO REP-HEAD.
      MOVE 2   TO WS-ADVANCE.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      *
      *    ****   1 2 C P I ( 9 6 Characters per line )
      *
      MOVE 0   TO WS-ADVANCE.
      MOVE 2   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      MOVE "H"   TO W02-PRN-TYPE.
      MOVE 2   TO WS-ADVANCE.
      MOVE W35-HEADING  TO REP-HEAD.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE 1   TO WS-ADVANCE.
      MOVE "D"   TO W02-PRN-TYPE.

       AB20.
      EXIT.

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000              SECTION.

       COPY "CARDEX.RD".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "STOCK.RD".

       READ-SALES.
            READ SALES   WITH IGNORE LOCK
              KEY IS SAL-EXT-CODE.
          IF WS-STATUS = "00"
              MOVE ZERO  TO WS-F-ERROR
              GO TO READ-SALES-EXIT.
          IF WS-STATUS = "23"
              MOVE 44   TO WS-F-ERROR
              GO TO READ-SALES-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 44   TO WS-F-ERROR
               PERFORM READ-ERROR.
            GO TO READ-SALES-EXIT.

       READ-SALES-NEXT.
            READ SALES NEXT  WITH IGNORE LOCK.
           IF WS-STATUS = "00"
              MOVE ZERO  TO WS-F-ERROR
              GO TO READ-SALES-EXIT.
          IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
              MOVE 44   TO WS-F-ERROR
              GO TO READ-SALES-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 44   TO WS-F-ERROR
               PERFORM READ-ERROR.
            GO TO READ-SALES-EXIT.

       READ-SALES-PREV.
            READ SALES PREVIOUS WITH IGNORE LOCK.
           IF WS-STATUS = "00"
              MOVE ZERO  TO WS-F-ERROR
              GO TO READ-SALES-EXIT.
          IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
              MOVE 44   TO WS-F-ERROR
              GO TO READ-SALES-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 44   TO WS-F-ERROR
               PERFORM READ-ERROR.
            GO TO READ-SALES-EXIT.

       START-AT-SALES-CODE.
            START SALES
              KEY >= SAL-EXT-CODE.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
              GO TO READ-SALES-EXIT.
           IF WS-STATUS = "23"
              MOVE 44   TO WS-F-ERROR
              GO TO READ-SALES-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 44   TO WS-F-ERROR
               PERFORM READ-ERROR.
            GO TO READ-SALES-EXIT.

       START-AT-SALES-QNT.
            START SALES
              KEY <= SAL-QUANT.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
              GO TO READ-SALES-EXIT.
           IF WS-STATUS = "23"
              MOVE 44   TO WS-F-ERROR
              GO TO READ-SALES-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 44   TO WS-F-ERROR
               PERFORM READ-ERROR.
            GO TO READ-SALES-EXIT.

       START-AT-SALES-TSELL.
             START SALES KEY <= SAL-TSELL.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
              GO TO READ-SALES-EXIT.
           IF WS-STATUS = "23"
              MOVE 44   TO WS-F-ERROR
              GO TO READ-SALES-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 44   TO WS-F-ERROR
               PERFORM READ-ERROR.
             GO TO READ-SALES-EXIT.

       START-AT-SALES-GP.
            START SALES
              KEY <= SAL-GP.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
              GO TO READ-SALES-EXIT.
           IF WS-STATUS = "23"
              MOVE 44   TO WS-F-ERROR
              GO TO READ-SALES-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 44   TO WS-F-ERROR
               PERFORM READ-ERROR.

       READ-SALES-EXIT.
             EXIT.

      *
      *   R E W R I T E   &    W R I T E   F I L E S   R O U T I N E S
      *
       AD000-WRITE             SECTION.

       REWRITE-SALES.
            REWRITE SAL-REC1.
           IF WS-STAT1 NOT = "0"
               MOVE 44               TO WS-F-ERROR
               PERFORM WRITE-ERROR.
             GO TO WRITE-SALES-EXIT.

       WRITE-SALES.
             WRITE SAL-REC1.
           IF WS-STAT1 NOT = "0"
               IF WS-STATUS = "22"
                   GO TO REWRITE-SALES
               ELSE
                   MOVE 44           TO WS-F-ERROR
                   PERFORM WRITE-ERROR.

       WRITE-SALES-EXIT.
             EXIT.

       COPY "APAC.HLP".

      *
      *  ****  E N Q U I R Y   ( S A L E S   P E R F O R M A N C E )
      *
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
             MOVE W12-TODAY          TO W15-START W15-END.

       BA05.
             DISPLAY "Press "       AT 5052 WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan
                           "Esc"            WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Cyan
                              "ape to Exit" WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan.
             ACCEPT S05A.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY GO TO BA999
                 WHEN OTHER   PERFORM AA900-ALARM
                              GO TO BA05
               END-EVALUATE.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO BA05.
             PERFORM CLEAR-L50.
             MOVE W15-START          TO W22-DATE.
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
             MOVE W15-END            TO W22-DATE.
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
        MOVE "End date is before start date"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BA05.
      MOVE W15-START  TO W22-DTE1 W35-START.
      MOVE W15-END  TO W22-DTE2 W35-END.
      DISPLAY WS-BLNK78 AT 0902
       WITH FOREGROUND-COLOR Cyan BACKGROUND-COLOR Blue.
      DISPLAY WS-BLNK78 AT 1102
       WITH FOREGROUND-COLOR Cyan BACKGROUND-COLOR Blue.
            PERFORM RESTORE-SCREEN-3.
      DISPLAY "Extracting details requested" AT 0912.
      DISPLAY "Reading:" AT 1112.
      MOVE W15-S-YMD  TO CRD-DTE.
      MOVE SPACES  TO CRD-EXT-CODE.
      PERFORM START-AT-CRDX-DSTK THRU READ-CARDEX-EXIT.
          IF WS-F-ERROR = 43
        GO TO BA16.
      MOVE SPACES  TO SAL-EXT-CODE.

       BA10.
            PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
    IF WS-F-ERROR = 43
        GO TO BA16.
    IF CRD-DTE > W15-E-YMD
        GO TO BA16.
      DISPLAY CRD-EXT-CODE AT 1121
       WITH BACKGROUND-COLOR Magenta
     FOREGROUND-COLOR Cyan HIGHLIGHT.
    IF CRD-EXT-CODE = SAL-EXT-CODE
        GO TO BA15.
      MOVE CRD-EXT-CODE  TO SAL-EXT-CODE STK-CODE.
      PERFORM READ-STOCK THRU READ-STOCK-EXIT.
    IF WS-F-ERROR = 22
        MOVE "No Stock Details for this Code"
     TO STK-DESC.
      PERFORM READ-SALES THRU READ-SALES-EXIT.
    IF WS-F-ERROR = 44
        MOVE STK-DESC  TO SAL-DESC
        MOVE STK-XREF  TO SAL-XREF
        MOVE CRD-QNT  TO SAL-QNT
        MOVE ZERO  TO SAL-COST SAL-SELL SAL-PROFIT
        IF CRD-QNT < ZERO
     COMPUTE SAL-COST = SAL-COST - (CRD-QNT * CRD-COST)
     COMPUTE SAL-SELL = SAL-SELL - (CRD-QNT * CRD-SELL)
        ELSE
     COMPUTE SAL-COST = SAL-COST + (CRD-QNT * CRD-COST)
     COMPUTE SAL-SELL = SAL-SELL + (CRD-QNT * CRD-SELL)
        END-IF
        SUBTRACT SAL-COST FROM SAL-SELL
     GIVING SAL-PROFIT
        PERFORM WRITE-SALES THRU WRITE-SALES-EXIT
        GO TO BA10.

       BA15.
      ADD CRD-QNT  TO SAL-QNT.
    IF CRD-QNT < ZERO
        COMPUTE SAL-COST = SAL-COST - (CRD-QNT * CRD-COST)
        COMPUTE SAL-SELL = SAL-SELL - (CRD-QNT * CRD-SELL)
    ELSE
        COMPUTE SAL-COST = SAL-COST + (CRD-QNT * CRD-COST)
        COMPUTE SAL-SELL = SAL-SELL + (CRD-QNT * CRD-SELL).
      SUBTRACT SAL-COST FROM SAL-SELL
     GIVING SAL-PROFIT.
      PERFORM REWRITE-SALES THRU WRITE-SALES-EXIT.
      GO TO BA10.

       BA16.
      MOVE "'P'rint or 'D'isplay format  [D]"
     TO WS-ERR-MES.
      MOVE "D"   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "D" OR "P")
        GO TO BA16.
      MOVE WS-OPTION  TO WS-ENQ.
    IF WS-ENQ = "D"
        GO TO BA20.
      MOVE ZERO   TO WS-PAGE.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *     **  *   *   * *   *  *  **  *   *   *     *   *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *     *  **   *     *   *  *  *  **   *   *     *  *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** *   *
      *
             PERFORM OPEN-PRINTER.
             MOVE "P"   TO WS-COMMAND.

       BA20.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66               TO MOUSE-FUNC
               MOVE 1                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
             PERFORM ERASE-SCREEN.
             DISPLAY S-PROG.
             MOVE 5                  TO SHADE-ROW.
             MOVE 31                 TO SHADE-COL.
             MOVE 26                 TO SHADE-WIDTH.
             MOVE 8                  TO SHADE-LINES.
             DISPLAY S-MENU.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
       BA25.
             MOVE 5                  TO TOP-ROW.
             MOVE 10                 TO BOTTOM-ROW.
             MOVE 4                  TO SCREEN-LIN.
             MOVE 31                 TO SCREEN-COL.
             MOVE 24                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       BA30.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BA35.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY  PERFORM MOVE-BLOCK-UP
   WHEN DOWN-KEY  PERFORM MOVE-BLOCK-DOWN
   WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
     IF MOUSE-ENTER = "Y"
         MOVE "N"  TO MOUSE-ENTER
         GO TO BA40
     END-IF
     IF MOUSE-Y = 24
         IF MOUSE-X = 5
      MOVE 5  TO KEY-CODE-1
      GO TO BA35
         ELSE
         IF MOUSE-X = 10
      MOVE 6  TO KEY-CODE-1
      GO TO BA35
         ELSE
         IF MOUSE-X > 36 AND < 44
      MOVE 0  TO KEY-CODE-1
      GO TO BA35
         ELSE
         IF MOUSE-X > 54 AND < 60
      MOVE 0  TO KEY-CODE-1
      MOVE 1  TO KEY-TYPE
      GO TO BA35
         END-IF
     END-IF
   WHEN ENTER-KEY  GO TO BA40
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO BA30
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE "0"  TO WS-OPTION
       GO TO BA50
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO BA30
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
        IF WS-OPTION < "0" OR > "5"
     PERFORM AA900-ALARM
     GO TO BA30
        END-IF
        GO TO BA45
    ELSE
        PERFORM AA900-ALARM
        GO TO BA30.

       BA40.
    IF SCREEN-LIN < 5
        PERFORM AA900-ALARM
        GO TO BA30
    ELSE
    IF SCREEN-LIN = 5
        MOVE "1"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 6
        MOVE "2"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 7
        MOVE "3"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 8
        MOVE "4"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 9
        MOVE "5"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 10
        MOVE "0"   TO WS-OPTION.
    GO TO BA50.

       BA45.
      PERFORM CLEAR-BLOCK.
    IF WS-OPTION = "0"
        MOVE 10   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "1"
        MOVE 5   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "2"
        MOVE 6   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "3"
        MOVE 7   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "4"
        MOVE 8   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "5"
        MOVE 9   TO SCREEN-LIN.
      PERFORM SAVE-ATTR.
      PERFORM MARK-BLOCK.

       BA50.
    IF MOUSE-ATTACHED = "Y"
        MOVE 66   TO MOUSE-FUNC
        MOVE 0   TO MOUSE-PARAM
        CALL X"AF" USING MOUSE-FUNC
    MOUSE-PARAM.
          IF WS-OPTION = "0"
              GO TO BA999.
           IF WS-OPTION = "1"
              PERFORM BB000
           ELSE
           IF WS-OPTION = "2"
              PERFORM BC000
           ELSE
           IF WS-OPTION = "3"
              PERFORM BD000
           ELSE
          IF WS-OPTION = "4"
              PERFORM BE000
           ELSE
          IF WS-OPTION = "5"
              PERFORM JA000.
      MOVE "P"   TO WS-COMMAND.
            GO TO BA20.

       BA999.
             EXIT.

       BB000        SECTION 50.
       BB00.
      PERFORM ERASE-SCREEN.
      DISPLAY S-PROG.
    IF WS-ENQ = "P"
        MOVE " (Quantity Seq.)"
     TO W35-REP
        MOVE ZERO  TO WS-PAGE
        PERFORM AB000
    ELSE
        DISPLAY S06
        DISPLAY " (Quantity Seq.)  " AT 0462
   WITH FOREGROUND-COLOR Grey HIGHLIGHT.
      MOVE "999999999"  TO SAL-QUANT.
      PERFORM START-AT-SALES-QNT THRU READ-SALES-EXIT.
    IF WS-F-ERROR = 44
        MOVE "No data on file"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BB999.

       BB05.
      MOVE 0604   TO CPOS.

       BB10.
    IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
        GO TO BB12.
      PERFORM READ-SALES-PREV THRU READ-SALES-EXIT.
    IF WS-F-ERROR = 44
        DISPLAY "End - Press ANY key" AT 5012
   WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT
        CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS
        GO TO BB999.

       BB12.
      MOVE SAL-EXT-CODE  TO W30-CODE.
      MOVE SAL-DESC  TO W30-DESC.
      MOVE SAL-QNT  TO W30-QUANT.
      MOVE SAL-SELL  TO W30-VALUE.
      MOVE SPACE   TO W30-SPACE.
      MOVE SAL-XREF  TO W30-XREF.
    IF WS-ENQ = "P"
        GO TO BB40.
      DISPLAY W30-DISPLAY AT CPOS
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
    IF CLIN < 23
        ADD 1   TO CLIN
        GO TO BB10.
            DISPLAY "PgUp" AT 5002 WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Cyan
       "/" WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan
       "PgDn" WITH FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Cyan
       " - " WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan
             "Esc" WITH FOREGROUND-COLOR Brown HIGHLIGHT
    BACKGROUND-COLOR Cyan
       "ape." WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan.

       BB15.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM CLEAR-L50.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BB999
   WHEN PAGE-DOWN
             GO TO BB20
   WHEN PAGE-UP
             GO TO BB30
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO BB15.
      PERFORM AA900-ALARM.
            GO TO BB15.

       BB20.
          IF CLIN < 23
              GO TO BB15.
            MOVE 0602   TO CPOS.

       BB25.
      DISPLAY WS-BLNK78 AT CPOS
       WITH FOREGROUND-COLOR Cyan BACKGROUND-COLOR Blue.
    IF CLIN < 23
        ADD 1   TO CLIN
        GO TO BB25.
            GO TO BB05.

       BB30.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
       MOVE "J"    TO WS-OPTION.

       BB35.
      PERFORM READ-SALES-NEXT THRU READ-SALES-EXIT.
     IF WS-F-ERROR = 44
        MOVE SPACE  TO WS-OPTION
              MOVE 0602  TO CPOS
        GO TO BB25.
     IF WS-IXS1 < 36
  ADD 1    TO WS-IXS1
  GO TO BB35.
      MOVE 0602    TO CPOS.
      GO TO BB25.

       BB40.
      MOVE W30-LINE   TO REP-HEAD.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM AB000.
            MOVE SPACES  TO REP-DETAIL1.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
        MOVE 0   TO WS-ADVANCE
        MOVE 1   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
        MOVE "D"   TO W02-PRN-TYPE
        MOVE 1   TO WS-ADVANCE
              MOVE SPACES  TO REP-DETAIL1
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        MOVE "P"   TO WS-COMMAND
        GO TO BB999.
      PERFORM READ-SALES-PREV THRU READ-SALES-EXIT.
    IF WS-F-ERROR = 44
        MOVE 0   TO WS-ADVANCE
        MOVE 1   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
        MOVE "D"   TO W02-PRN-TYPE
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              MOVE "**** END OF REPORT ****"
                                 TO REP-DETAIL1
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        GO TO BB999.
      GO TO BB12.

       BB999.
      EXIT.

       BC000        SECTION 51.
       BC00.
      PERFORM ERASE-SCREEN.
      DISPLAY S-PROG.
    IF WS-ENQ = "P"
        MOVE " (Value Sold Seq.)"
     TO W35-REP
        MOVE ZERO  TO WS-PAGE
        PERFORM AB000
    ELSE
        DISPLAY S06
        DISPLAY " (Value Sold Seq.)" AT 0462
   WITH FOREGROUND-COLOR Grey HIGHLIGHT.
      MOVE "99999999999"  TO SAL-TSELL.
      PERFORM START-AT-SALES-TSELL THRU READ-SALES-EXIT.
    IF WS-F-ERROR = 44
        MOVE "No data on file"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BC999.

       BC05.
      MOVE 0604   TO CPOS.

       BC10.
    IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
        GO TO BC12.
      PERFORM READ-SALES-PREV THRU READ-SALES-EXIT.
    IF WS-F-ERROR = 44
        DISPLAY "End - Press ANY key" AT 5012
   WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT
        CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS
        GO TO BC999.

       BC12.
      MOVE SAL-EXT-CODE  TO W30-CODE.
      MOVE SAL-DESC  TO W30-DESC.
      MOVE SAL-QNT  TO W30-QUANT.
      MOVE SAL-SELL  TO W30-VALUE.
      MOVE SPACE   TO W30-SPACE.
      MOVE SAL-XREF  TO W30-XREF.
    IF WS-ENQ = "P"
        GO TO BC40.
      DISPLAY W30-DISPLAY AT CPOS
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
    IF CLIN < 23
        ADD 1   TO CLIN
        GO TO BC10.
            DISPLAY "PgUp" AT 5002 WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Cyan
       "/" WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan
       "PgDn" WITH FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Cyan
       " - " WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan
             "Esc" WITH FOREGROUND-COLOR Brown HIGHLIGHT
    BACKGROUND-COLOR Cyan
       "ape." WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan.

       BC15.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM CLEAR-L50.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BC999
   WHEN PAGE-DOWN
             GO TO BC20
   WHEN PAGE-UP
             GO TO BC30
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO BC15.
      PERFORM AA900-ALARM.
            GO TO BC15.

       BC20.
          IF CLIN < 23
              GO TO BC15.
            MOVE 0602   TO CPOS.

       BC25.
      DISPLAY WS-BLNK78 AT CPOS
       WITH FOREGROUND-COLOR Cyan BACKGROUND-COLOR Blue.
    IF CLIN < 23
        ADD 1   TO CLIN
        GO TO BC25.
            GO TO BC05.

       BC30.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
       MOVE "J"    TO WS-OPTION.

       BC35.
      PERFORM READ-SALES-NEXT THRU READ-SALES-EXIT.
     IF WS-F-ERROR = 44
        MOVE SPACE  TO WS-OPTION
              MOVE 0602  TO CPOS
        GO TO BC25.
     IF WS-IXS1 < 36
  ADD 1    TO WS-IXS1
  GO TO BC35.
      MOVE 0602    TO CPOS.
      GO TO BC25.

       BC40.
      MOVE W30-LINE   TO REP-HEAD.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM AB000.
            MOVE SPACES  TO REP-DETAIL1.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
        MOVE 0   TO WS-ADVANCE
        MOVE 1   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
        MOVE "D"   TO W02-PRN-TYPE
        MOVE 1   TO WS-ADVANCE
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        MOVE "P"   TO WS-COMMAND
        GO TO BC999.
      PERFORM READ-SALES-PREV THRU READ-SALES-EXIT.
    IF WS-F-ERROR = 44
        PERFORM CALL-PRINTUTL
        MOVE 0   TO WS-ADVANCE
        MOVE 1   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
        MOVE "D"   TO W02-PRN-TYPE
        MOVE 1   TO WS-ADVANCE
              MOVE "**** END OF REPORT ****"
                                 TO REP-DETAIL1
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        GO TO BC999.
      GO TO BC12.

       BC999.
      EXIT.

       BD000        SECTION 52.
       BD00.
      PERFORM ERASE-SCREEN.
      DISPLAY S-PROG.
    IF WS-ENQ = "P"
        MOVE " (G.Profit Seq.)"
     TO W35-REP
        MOVE ZERO  TO WS-PAGE
        PERFORM AB000
    ELSE
        DISPLAY S06
        DISPLAY " (G.Profit Seq.)  " AT 0462
         WITH FOREGROUND-COLOR Grey HIGHLIGHT.
      DISPLAY "Profit" AT 0569
        WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan.
      MOVE "99999999999"  TO SAL-GP.
      PERFORM START-AT-SALES-GP THRU READ-SALES-EXIT.
    IF WS-F-ERROR = 44
        MOVE "No data on file"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BD999.

       BD05.
      MOVE 0604   TO CPOS.

       BD10.
    IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
        GO TO BD12.
      PERFORM READ-SALES-PREV THRU READ-SALES-EXIT.
    IF WS-F-ERROR = 44
        DISPLAY "End - Press ANY key" AT 5012
   WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT
        CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS
        GO TO BD999.

       BD12.
      MOVE SAL-EXT-CODE  TO W30-CODE.
      MOVE SAL-DESC  TO W30-DESC.
      MOVE SAL-QNT  TO W30-QUANT.
      MOVE SAL-PROFIT  TO W30-VALUE.
      MOVE SPACE   TO W30-SPACE.
      MOVE SAL-XREF  TO W30-XREF.
    IF WS-ENQ = "P"
        GO TO BD40.
      DISPLAY W30-DISPLAY AT CPOS
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
    IF CLIN < 23
        ADD 1   TO CLIN
        GO TO BD10.
            DISPLAY "PgUp" AT 5002 WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Cyan
       "/" WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan
       "PgDn" WITH FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Cyan
       " - " WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan
             "Esc" WITH FOREGROUND-COLOR Brown HIGHLIGHT
    BACKGROUND-COLOR Cyan
       "ape." WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan.

       BD15.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM CLEAR-L50.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BD999
   WHEN PAGE-DOWN
             GO TO BD20
   WHEN PAGE-UP
             GO TO BD30
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO BD15.
      PERFORM AA900-ALARM.
            GO TO BD15.

       BD20.
          IF CLIN < 23
              GO TO BD15.
            MOVE 0602   TO CPOS.

       BD25.
      DISPLAY WS-BLNK78 AT CPOS
       WITH FOREGROUND-COLOR Cyan BACKGROUND-COLOR Blue.
    IF CLIN < 23
        ADD 1   TO CLIN
        GO TO BD25.
            GO TO BD05.

       BD30.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
       MOVE "J"    TO WS-OPTION.

       BD35.
      PERFORM READ-SALES-NEXT THRU READ-SALES-EXIT.
     IF WS-F-ERROR = 44
        MOVE SPACE  TO WS-OPTION
              MOVE 0602  TO CPOS
        GO TO BD25.
     IF WS-IXS1 < 36
  ADD 1    TO WS-IXS1
        GO TO BD35.
      MOVE 0602    TO CPOS.
      GO TO BD25.

       BD40.
      MOVE W30-LINE   TO REP-HEAD.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM AB000.
            MOVE SPACES  TO REP-DETAIL1.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
        MOVE 0   TO WS-ADVANCE
        MOVE 1   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
        MOVE "D"   TO W02-PRN-TYPE
        MOVE 1   TO WS-ADVANCE
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        MOVE "P"   TO WS-COMMAND
        GO TO BD999.
      PERFORM READ-SALES-PREV THRU READ-SALES-EXIT.
    IF WS-F-ERROR = 44
        MOVE 0   TO WS-ADVANCE
        MOVE 1   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
        MOVE "D"   TO W02-PRN-TYPE
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              MOVE "**** END OF REPORT ****"
                                 TO REP-DETAIL1
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        GO TO BD999.
      GO TO BD12.

       BD999.
      EXIT.

       BE000        SECTION 53.
       BE00.
      PERFORM ERASE-SCREEN.
      DISPLAY S-PROG.
      MOVE " !"   TO SAL-EXT-CODE.
      PERFORM START-AT-SALES-CODE THRU READ-SALES-EXIT.
    IF WS-F-ERROR = 44
        MOVE "No data on file"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BE999.
            MOVE ZERO   TO W200-COST W200-SELL W200-NET.
      *
      *         ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *         ³  Extracting Gross Profit Data  ³°°
      *         ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´°°
      *         ³ Sales Record xxxxxxxxxxxxxxxxxx ³°°
      *         ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°
      *   °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
      *
      PERFORM SAVE-SCREEN-3.
      MOVE 7   TO SHADE-ROW.
      MOVE 24   TO SHADE-COL.
      MOVE 33   TO SHADE-WIDTH.
      MOVE 4   TO SHADE-LINES.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0622
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Grey
       "¿" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "³  Extracting Gross Profit Data   " AT 0722
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Grey
       "³" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "Ã" AT 0822 WITH FOREGROUND-COLOR Grey HIGHLIGHT
          BACKGROUND-COLOR Grey
       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "³ Sales Record" AT 0922
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Grey
       "                    ³"
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "À" AT 1022 WITH FOREGROUND-COLOR Grey HIGHLIGHT
          BACKGROUND-COLOR Grey
       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      PERFORM SCREEN-SHADOW.

       BE05.
      PERFORM READ-SALES-NEXT THRU READ-SALES-EXIT.
    IF WS-F-ERROR = 44
        GO TO BE10.
      DISPLAY SAL-EXT-CODE AT 0937
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      ADD SAL-COST  TO W200-COST.
      ADD SAL-SELL  TO W200-SELL.
      ADD SAL-PROFIT  TO W200-NET.
      GO TO BE05.

       BE10.
            PERFORM RESTORE-SCREEN-3.
      MOVE 7   TO SHADE-ROW.
      MOVE 21   TO SHADE-COL.
      MOVE 39   TO SHADE-WIDTH.
      MOVE 7   TO SHADE-LINES.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0619
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Grey
       "¿" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "³For the Period " AT 0719
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Grey
        W35-START WITH FOREGROUND-COLOR Grey HIGHLIGHT
         BACKGROUND-COLOR Grey
       " to " WITH FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey
        W35-END WITH FOREGROUND-COLOR Grey HIGHLIGHT
       BACKGROUND-COLOR Grey
       "³" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "Ã" AT 0819 WITH FOREGROUND-COLOR Grey HIGHLIGHT
          BACKGROUND-COLOR Grey
       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "³   Total Sales " AT 0919
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Grey
       "                      ³"
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "³         Cost    " AT 1019
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Grey
       "                      ³"
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "³                 ÄÄÄÄÄÄÄÄÄÄÄ" AT 1119
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Grey
       "           ³"
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "³   Gross Profit  " AT 1219
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Grey
       "                      ³"
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "À" AT 1319 WITH FOREGROUND-COLOR Grey HIGHLIGHT
          BACKGROUND-COLOR Grey
       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      PERFORM SCREEN-SHADOW.

      DISPLAY "%" AT 1257
        WITH FOREGROUND-COLOR Cyan HIGHLIGHT
      BACKGROUND-COLOR Magenta.
      MOVE W200-SELL  TO W40-AMOUNT.
            DISPLAY W40-AMOUNT AT 0937
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      MOVE W200-COST  TO W40-AMOUNT.
            DISPLAY W40-AMOUNT AT 1037
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      MOVE W200-NET  TO W40-AMOUNT.
            DISPLAY W40-AMOUNT AT 1237
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            COMPUTE W200-ADJ = (W200-NET / W200-SELL) * 100.
            MOVE W200-ADJ  TO W40-PERC.
            DISPLAY W40-PERC AT 1250
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      DISPLAY "Press ANY key to continue" AT 1527.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BE999.
      EXIT.

      /
      *    ****    STOCK SALES & GROSS PROFIT ENQUIRY
      *
       JA000        SECTION 54.
       JA00.
             PERFORM ERASE-SCREEN.
             DISPLAY "STOCK SALES & GP ENQUIRY" AT 0227
              WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "Are you sure Y/N?" AT 0620
              WITH FOREGROUND-COLOR Brown HIGHLIGHT.

       JA02.
            ACCEPT WS-OPTION AT 0638
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "Y" OR "N")
               GO TO JA02.
           IF WS-OPTION = "N"
               GO TO JA999.
            DISPLAY "Accumulating totals" AT 0812.
            DISPLAY "Sales Ledger :" AT 1012.
            MOVE " !"   TO DPT-CODE.
            MOVE 1   TO WS-S6.
      PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.
    IF WS-F-ERROR = 7
        MOVE "No departments loaded"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO JA999.

       JA03.
            MOVE ZERO   TO W200-DT (WS-S6) W200-CR (WS-S6).
          IF WS-S6 < 297
              ADD 1   TO WS-S6
               GO TO JA03.
            MOVE ZERO   TO W200-DTOT W200-CTOT
              W200-COST W200-SELL W200-NET.

       JA05.
            PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
    IF WS-F-ERROR = 7
        GO TO JA10.
    IF (DPT-CODE = "XXXX" OR "YYYY") OR
       (DPT-SCDE = "VAT")
        GO TO JA05.
            DISPLAY DPT-CODE AT 1027
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
           IF DPT-SALES-MTD < ZERO
        MULTIPLY -1  BY DPT-SALES-MTD
               COMPUTE DPT-SALES-YTD = DPT-SALES-YTD +
        (DPT-SALES-MTD * 2).
            DISPLAY DPT-DESC WITH FOREGROUND-COLOR Cyan HIGHLIGHT AT 1033.
             ADD DPT-SALES-MTD   TO W200-DT(1) .
             ADD DPT-SALES-YTD   TO W200-DT(2) .
             ADD DPT-RET-MTD     TO W200-DT(3) .
             ADD DPT-RET-YTD     TO W200-DT(4) .
             ADD DPT-COST-MTD    TO W200-DT (7).
             ADD DPT-COST-YTD    TO W200-DT (8).
             ADD DPT-CRET-MTD    TO W200-DT (9).
             ADD DPT-CRET-YTD    TO W200-DT (10).
             ADD DPT-DISC-MTD    TO W200-DT(5) .
             ADD DPT-DISC-YTD    TO W200-DT(6) .
            GO TO JA05.

       JA10.
             MOVE 0601           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
            DISPLAY "M.T.D. Sales   :" AT 0412.
            DISPLAY "       Returns :" AT 0512.
            DISPLAY "-----------" AT 0630
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Magenta.
            DISPLAY "       Gross   :" AT 0712.
            DISPLAY " Less Discount :" AT 0812.
            DISPLAY "-----------" AT 0930
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Magenta.
            DISPLAY "Nett Sales     :" AT 1012.
            DISPLAY "     Cost      :" AT 1112.
            DISPLAY "Gross Profit   :" AT 1212.
            DISPLAY "===========" AT 1330
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Magenta.
            DISPLAY "Y.T.D. Sales   :" AT 1412.
            DISPLAY "       Returns :" AT 1512.
            DISPLAY "-----------" AT 1630
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Magenta.
            DISPLAY "       Gross   :" AT 1712.
            DISPLAY " Less Discount :" AT 1812.
            DISPLAY "-----------" AT 1930
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Magenta.
            DISPLAY "Nett Sales     :" AT 2012.
            DISPLAY "     Cost      :" AT 2112.
            DISPLAY "Gross Profit   :" AT 2212.
            DISPLAY "===========" AT 2330
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Magenta.
            DISPLAY "%" AT 1248
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Magenta.
            DISPLAY "%" AT 2248
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Magenta.
             MOVE W200-DT(1)  TO W40-AMOUNT W200-CTOT.
            DISPLAY W40-AMOUNT AT 0430
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             MOVE W200-DT(3)  TO W40-AMOUNT W200-DTOT.
            DISPLAY W40-AMOUNT AT 0530
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             SUBTRACT W200-DTOT FROM W200-CTOT.
             MOVE W200-CTOT TO W40-AMOUNT.
            DISPLAY W40-AMOUNT AT 0730
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             MOVE W200-DT(5)  TO W40-AMOUNT W200-DTOT.
            DISPLAY W40-AMOUNT AT 0830
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             SUBTRACT W200-DTOT FROM W200-CTOT.
             MOVE W200-CTOT TO W40-AMOUNT W200-SELL.
            DISPLAY W40-AMOUNT AT 1030
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             SUBTRACT W200-DT (9) FROM W200-DT (7) GIVING W200-COST.
             MOVE W200-COST TO W40-AMOUNT.
            DISPLAY W40-AMOUNT AT 1130
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             COMPUTE W200-DTOT = W200-SELL - W200-COST.
             MOVE W200-DTOT TO W40-AMOUNT.
            DISPLAY W40-AMOUNT AT 1230
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             COMPUTE W200-COST = (W200-DTOT / W200-SELL) * 100.
             MOVE W200-COST TO W40-PERC.
            DISPLAY W40-PERC AT 1241
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             MOVE W200-DT(2)  TO W40-AMOUNT W200-CTOT.
            DISPLAY W40-AMOUNT AT 1430
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             MOVE W200-DT(4)  TO W40-AMOUNT W200-DTOT.
            DISPLAY W40-AMOUNT AT 1530
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             SUBTRACT W200-DTOT FROM W200-CTOT.
             MOVE W200-CTOT TO W40-AMOUNT.
            DISPLAY W40-AMOUNT AT 1730
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             MOVE W200-DT(6)  TO W40-AMOUNT W200-DTOT.
            DISPLAY W40-AMOUNT AT 1830
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             SUBTRACT W200-DTOT FROM W200-CTOT.
             MOVE W200-CTOT TO W40-AMOUNT W200-SELL.
            DISPLAY W40-AMOUNT AT 2030
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             SUBTRACT W200-DT (10) FROM W200-DT (8) GIVING W200-COST.
             MOVE W200-COST TO W40-AMOUNT.
            DISPLAY W40-AMOUNT AT 2130
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             COMPUTE W200-DTOT = W200-SELL - W200-COST.
             MOVE W200-DTOT TO W40-AMOUNT.
            DISPLAY W40-AMOUNT AT 2230
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             COMPUTE W200-COST = (W200-DTOT / W200-SELL) * 100.
             MOVE W200-COST TO W40-PERC.
            DISPLAY W40-PERC AT 2241
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            DISPLAY "Any KEY to continue" AT 5012
              WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT WS-OPTION AT 5032
      WITH BACKGROUND-COLOR Cyan
    FOREGROUND-COLOR Grey HIGHLIGHT AUTO.

       JA999.
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

       ZA00A-CONTINUE.
            MOVE 3   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
    IF LS-USER NUMERIC
        IF LS-USE > ZERO AND < 110
     MOVE LS-USE  TO WS-USUB
                   GO TO ZA01A.
             MOVE 110            TO WS-USUB.

       ZA01A.
             ADD 2549 WS-USUB    GIVING W25-RESULT.
             MOVE W25-KEY        TO WS-PARKEY.
             ADD 1 W25-SUB       GIVING WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.

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
            OPEN OUTPUT SALES.
          IF RUNTIME-ERROR
               IF FLE-LIMIT
           GO TO ZA49.
           IF WS-STATUS NOT = "00"
              MOVE 44   TO WS-F-ERROR
               PERFORM OPEN-ERROR.
      CLOSE SALES.
      OPEN I-O SALES.
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
               WHEN 7     MOVE W02-DEPART   TO WS-FILE
                          MOVE DPT-CODE     TO WS-KEYX
               WHEN 15    MOVE WS-PARID     TO WS-FILE
                          MOVE WS-PARKEY    TO WS-KEY
               WHEN 22    MOVE W02-STOCKF   TO WS-FILE
                          MOVE STK-CODE     TO WS-KEYX
               WHEN 43    MOVE W02-CARDEX   TO WS-FILE
                          MOVE CRD-KEY      TO WS-KEYX
               WHEN 44    MOVE W02-SALES    TO WS-FILE
                          MOVE SAL-EXT-CODE TO WS-KEYX
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STPTOP"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
