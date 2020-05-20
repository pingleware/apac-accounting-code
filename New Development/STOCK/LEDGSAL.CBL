      $SET LINKCOUNT"512" GNT"LEDGSAL.GNT"
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
      *  **       *******  ******    *****    *****     **    **       *
      *  **       **       **   **  **   **  **   **   ****   **       *
      *  **       **       **   **  **       **       **  **  **       *
      *  **       *****    **   **  **        *****   **  **  **       *
      *  **       **       **   **  **  ***       **  ******  **       *
      *  **       **       **   **  **   **  **   **  **  **  **       *
      *  *******  *******  ******    *****    *****   **  **  *******  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S A L E S   P E R   S A L E S   L E D G E R   F O R   A    *
      *     S P E C I F I C   P E R I O D                              *
      *                                                                *
      *       Version 9.04.03 - September 2017                         *

      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     LEDGSAL.
       AUTHOR.         J.W.LEMMON.
       DATE-WRITTEN.   January 1997.

           This program prints a list of stock items that have been sold
           during a specified period, for the sales ledger that has been
           requested.

           The program reads the cardex file to extract the sales for 
           each stock item. Invoices are accumulated for the sales and 
           credit notes are deducted.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1997 - 2018
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

       COPY "PARAM.SL".

       COPY "STOCK.SL".

      $SET KEYCOMPRESS "7"
           SELECT MOVEMENT ASSIGN DISK
                           STATUS WS-STATUS
                           ACCESS DYNAMIC
                           ORGANIZATION INDEXED
                           RECORD SAL-KEY
                           ALTERNATE RECORD SAL-CODE DUPLICATES
                           ALTERNATE RECORD SAL-QUANT DUPLICATES
                           ALTERNATE RECORD SAL-TSELL DUPLICATES
                           ALTERNATE RECORD SAL-GP DUPLICATES.
      $SET NOKEYCOMPRESS

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

       COPY "CARDEX.FDE".

       COPY "CONTROL.FDE".

       COPY "DEPART.FDE".

       COPY "PARAM.FDE".

       COPY "STOCK.FDE".

      /
       FD  MOVEMENT  IS EXTERNAL
       LABEL RECORD STANDARD
             VALUE OF FILE-ID W02-MOVEMENT.
       01  SAL-REC1.
          03  SAL-KEY.
        05  SAL-LEDG    PIC  X(03).
        05  SAL-CODE    PIC  X(14).
    03  SAL-QUANT.
              05  SAL-QNT     PIC S9(07)V99 COMP.
    03  SAL-COST        PIC S9(09)V99 COMP.
    03  SAL-TSELL.
        05  SAL-SELL    PIC S9(09)V99 COMP.
    03  SAL-GP.
        05  SAL-PROFIT  PIC S9(09)V99 COMP.
    03  SAL-DESC        PIC  X(30).

      /
       FD  PRNREP    LABEL RECORD OMITTED
             LINAGE WS-PGE-LENGTH.
       01  R-L1.
          03  R-DETAIL.
        05  R-HL        PIC  X(96).
        05  FILLER      PIC  X(36).
       01  R-L2.
           03  R-PGE           PIC X(06).
           03  R-PNO           PIC Z(03)9.
           03  FILLER          PIC X(10).
          03  R-CPY        PIC X(41).
          03  FILLER        PIC X(05).
           03  R-DATE          PIC X(06).
           03  R-DTE           PIC Z9/99/99.

       01  SRP-L1.
          03  SRP-H1        PIC X(14).
          03  SRP-NO        PIC Z9.
           03  FILLER          PIC X(01).
          03  SRP-SMAN        PIC X(30).
          03  FILLER        PIC X(33).
       01  SRP-L2.
    03  SRP-H2        PIC  X(51).
    03  SRP-H3        PIC  X(29).
       01  SRP-L3.
    03  SRP-ACT        PIC  X(06).
    03  FILLER        PIC  X(02).
    03  SRP-NAME        PIC  X(40).
    03  FILLER        PIC  X(01).
    03  SRP-DATE        PIC  Z9/99/99.
    03  FILLER        PIC  X(01).
    03  SRP-REF        PIC  X(08).
    03  FILLER        PIC  X(30).
       01  SRP-L4.
    03  SRP-H4        PIC  X(15).
    03  SRP-H5        PIC  X(33).
    03  SRP-H6        PIC  X(11).
    03  SRP-H7        PIC  X(13).
    03  SRP-H8        PIC  X(15).
    03  SRP-H9        PIC  X(10).
       01  SRP-L5.
          03  SRP-ITEM        PIC  X(14).
          03  FILLER        PIC  X(01).
          03  SRP-DESC        PIC  X(30).
          03  FILLER        PIC  X(01).
          03  SRP-QUANT       PIC  Z(06)9.99-.
          03  FILLER        PIC  X(01).
          03  SRP-COST        PIC  Z(07)9.99-.
          03  FILLER        PIC  X(01).
          03  SRP-SELL        PIC  Z(07)9.99-.
          03  FILLER        PIC  X(01).
          03  SRP-EXTEND      PIC  Z(07)9.99-.
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
       77  WS-ENQPOS    PIC 9(06)  COMP-5.
       77  WS-ENQEND       PIC  9(06)    COMP-5.
       77  WS-DEPKEY    PIC 9(06)  COMP-5.
       77  WS-NETKEY       PIC  9(06)    COMP-5.
       77  WS-PARKEY       PIC  9(06)    COMP-5.
       77  WS-KEYSTR       PIC  9(06)    COMP-5.
       77  WS-LINES    PIC 9(02)  VALUE 66.
       77  WS-PAGE    PIC 9(04)  COMP-5.
       77  WS-NUM    PIC ZZ9.
       77  WS-PGE-LENGTH   PIC 9(02)  VALUE 66.
       77  WS-PRN-LENGTH   PIC 9(02)  VALUE 62.
       77  WS-NORM    PIC X(06).
       77  WS-COND    PIC X(06).
       77  WS-EXPP    PIC X(06).
       77  WS-ECAN    PIC X(06).
       77  WS-8LPI    PIC X(06).
       77  WS-6LPI    PIC X(06).
       77  WS-10CPI    PIC X(06).
       77  WS-12CPI    PIC X(06).
       77  WS-17CPI    PIC X(06).
       77  WS-OPTION    PIC X(01).
       77  WS-LEDG         PIC  9(03).
       77  WS-ENQ    PIC X(01).
       77  WS-MONTH    PIC 9(02).
       77  WS-ETYPE    PIC X(01).
       77  WS-ERROR        PIC  9(01) VALUE ZERO.
       77  WS-ERR2         PIC  X(09) VALUE "No Record".
       77  TODAY-DDMMYY    PIC 9(06) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.

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
           03  CRT-PROGRAM         PIC  X(15) VALUE "LEDGSAL".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY SALES PER LEDGER".
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
           03  WS-MODULE           PIC  X(03) VALUE "LED".
           03  WS-PROG             PIC  X(03) VALUE "SAL".

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

    03  W02-PRINTER.
        05  W02-REPORT PIC X(07) VALUE "SALMAN.".
               05  W02-USER   PIC X(05) VALUE SPACES.

       01  W05-CARDEX.
           03  W05-CRDX-ID.
               05  W05-CID         PIC  X(01)           OCCURS 12.

       01  W05.
           03  W05-S               PIC  9(04)    COMP-5 OCCURS 300.

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

       COPY "W20.WS".

       COPY "W25.STK".

       01  W30-DISPLAY.
    03  W30-CODE        PIC  X(15).
    03  W30-DESC        PIC  X(31).
    03  W30-QUANT       PIC  Z(06)9.99-.
    03  W30-VALUE       PIC  Z(08)9.99-.

       01  W40-COMPANY.
           03  W40-NAME1.
               05  W40-NAME.
                   07  W40-ENTRY PIC X(08).
                   07  FILLER  PIC X(32).
              05  FILLER      PIC X(32).
           03  W40-NAME2 REDEFINES W40-NAME1.
               05  W40-SURNAME PIC X(24).
               05  W40-FNAME   PIC X(24).
               05  W40-SNAME   PIC X(24).
           03  W40-NAME3 REDEFINES W40-NAME1.
               05  W40-NCHAR OCCURS 72 PIC X(01).
           03  W40-ANAME       PIC X(40).
           03  W40-CONTACT     PIC  X(30).
           03  W40-DEBTREC.
               05  W40-ACNO      PIC X(06).
               05  FILLER        PIC X(456).
           03  W40-ADD REDEFINES W40-DEBTREC.
             05  W40-ADDRESS1.
               07  W40-ADD1    PIC X(30).
               07  W40-ADD2    PIC X(30).
               07  W40-ADD3    PIC X(30).
               07  W40-ADD4    PIC X(30).
             05  W40-ADDRESS2 REDEFINES W40-ADDRESS1.
               07  W40-ACHAR OCCURS 120 PIC X(01).
             05  W40-PADDRESS.
               07  W40-PADD1   PIC X(30).
               07  W40-PADD2   PIC X(30).
               07  W40-PADD3   PIC X(30).
               07  W40-PADD4   PIC X(30).
             05  W40-PADDR2   REDEFINES W40-PADDRESS.
               07  W40-PCHAR OCCURS 120 PIC X(01).
             05  W40-SPACES    PIC X(30).
             05  W40-TOP.
               07  W40-T1      PIC  X(01).
               07  W40-T2-27   PIC  X(26).
               07  W40-T28     PIC  X(01).
             05  W40-MID.
               07  W40-M1      PIC  X(01).
               07  W40-M2-27   PIC  X(26).
               07  W40-M28     PIC  X(01).
             05  W40-BOT.
               07  W40-B1      PIC  X(01).
               07  W40-B2-27   PIC  X(26).
               07  W40-B28     PIC  X(01).
             05  FILLER        PIC X(111).
           03  W40-POST        PIC 9(04).
           03  W40-PPOST       PIC 9(04).
           03  W40-TEL         PIC X(14).
           03  W40-HTEL        PIC X(14).
           03  W40-FAX         PIC X(14).
           03  W40-ID.
               05  W40-IDNO    PIC X(14).
           03  W40-TAX REDEFINES W40-ID.
               05  W40-TAXNO   PIC X(14).
           03  W40-TITLE       PIC 9(02).
           03  W40-LANG        PIC X(01).
           03  W40-CAT         PIC 9(02).
           03  W40-OCAT        PIC 9(02).
           03  W40-LEDG        PIC 9(03).
           03  W40-FIN         PIC X(01).
           03  W40-CRL         PIC S9(07)    COMP-3.
           03  W40-ONE         PIC S9(09)V99 COMP-3.
           03  W40-MTD         PIC S9(09)V99 COMP-3.
           03  W40-CUR         PIC S9(09)V99 COMP-3.
           03  W40-30          PIC S9(09)V99 COMP-3.
           03  W40-60          PIC S9(09)V99 COMP-3.
           03  W40-90          PIC S9(09)V99 COMP-3.
           03  W40-120         PIC S9(09)V99 COMP-3.
           03  W40-INT         PIC S9(09)V99 COMP-3.
           03  W40-TERMS       PIC X(01).
           03  W40-INV-TYPE    PIC X(01).
           03  W40-SDISC       PIC 9(02).
           03  W40-DISC        PIC 9(02).
           03  W40-SMAN        PIC 9(02).
           03  W40-LAB         PIC  X(01).
           03  W40-OI          PIC  X(01).
           03  W40-OOI         PIC  X(01).

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

       01  W40-INDIC.
      *     03 W40-SMAN.
      *  05  W40-SREP PIC 9(01) COMP-5 OCCURS 50.
      *     03 W40-DRAW.
      *  05  W40-DREP PIC 9(01) COMP-5 OCCURS 40.
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

       01  W55-HEADING.
    03  FILLER       PIC  X(22) VALUE "Sales for the period:".
    03  W55-START      PIC  99/99/99.
    03  FILLER       PIC  X(03) VALUE " - ".
    03  W55-END       PIC  99/99/99.

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
    03  W75-S2       PIC 9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
           03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
           03  W75-DNO        PIC X(06) OCCURS 18.
           03  W75-ITEMS OCCURS 18.
               05  W75-CODE   PIC X(14).
               05  W75-SELL   PIC 9(07)V99 COMP-3.
               05  W75-VSELL  PIC 9(07)V99 COMP-3.
               05  W75-CASH   PIC 9(07)V99 COMP-3.
               05  W75-VCASH  PIC 9(07)V99 COMP-3.
               05  W75-WSALE  PIC 9(07)V99 COMP-3.
               05  W75-VWSALE PIC 9(07)V99 COMP-3.
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
          03  LINE  2 COLUMN 25 FOREGROUND-COLOR Grey HIGHLIGHT VALUE
               "SALES PER SALES LEDGER REPORTING".

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
     VALUE "0".
    03        COLUMN 32 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " Exit                   ³".
    03  LINE 11 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S04.
          03  LINE 10 COLUMN 20 FOREGROUND-COLOR Brown HIGHLIGHT
                                 VALUE "Busy loading stock departments".

       01  S05D.
          03  LINE  2 COLUMN 27 FOREGROUND-COLOR Grey HIGHLIGHT VALUE
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
    03  LINE  5 COLUMN  6 FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan
        VALUE "Item Code      Stock Description                Quantity        Value ".

       01  S07.
    03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC ZZ9 USING WS-LEDG AUTO.

       01  S36.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC X(14) USING W10-ITEM AUTO.

       01  S40.
    03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC ZZ9 USING WS-LEDG AUTO.

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
          IF LS0-SALEV < 9
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             DISPLAY S-PROG.
             PERFORM BE000.
             PERFORM BA000.
           IF NOT(W05-CID(W12-MONTH) = W02-MTH43)
               CLOSE CARDEX
               MOVE W05-CID(W12-MONTH) TO W02-MTH43
               OPEN I-O CARDEX.
             PERFORM BB000.
             CLOSE MOVEMENT PRNREP.

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

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000              SECTION.

       COPY "CARDEX.RD".

       COPY "CONTROL.RD".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "STOCK.RD".

       READ-MOVEMENT.
            READ MOVEMENT  WITH IGNORE LOCK
              KEY IS SAL-KEY.
          IF WS-STATUS = "00"
              MOVE ZERO  TO WS-F-ERROR
              GO TO READ-MOVEMENT-EXIT.
          IF WS-STATUS = "23"
              MOVE 44   TO WS-F-ERROR
              GO TO READ-MOVEMENT-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 44   TO WS-F-ERROR
               PERFORM READ-ERROR.
            GO TO READ-MOVEMENT-EXIT.

       READ-MOVEMENT-NEXT.
            READ MOVEMENT NEXT  WITH IGNORE LOCK.
           IF WS-STATUS = "00"
              MOVE ZERO  TO WS-F-ERROR
              GO TO READ-MOVEMENT-EXIT.
          IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
              MOVE 44   TO WS-F-ERROR
              GO TO READ-MOVEMENT-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 44   TO WS-F-ERROR
               PERFORM READ-ERROR.
            GO TO READ-MOVEMENT-EXIT.

       READ-MOVEMENT-PREV.
            READ MOVEMENT PREVIOUS WITH IGNORE LOCK.
           IF WS-STATUS = "00"
              MOVE ZERO  TO WS-F-ERROR
              GO TO READ-MOVEMENT-EXIT.
          IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
              MOVE 44   TO WS-F-ERROR
              GO TO READ-MOVEMENT-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 44   TO WS-F-ERROR
               PERFORM READ-ERROR.
            GO TO READ-MOVEMENT-EXIT.

       START-AT-MOVEMENT-CODE.
            START MOVEMENT
              KEY >= SAL-KEY.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
              GO TO READ-MOVEMENT-EXIT.
           IF WS-STATUS = "23"
              MOVE 44   TO WS-F-ERROR
              GO TO READ-MOVEMENT-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 44   TO WS-F-ERROR
               PERFORM READ-ERROR.
            GO TO READ-MOVEMENT-EXIT.

       START-AT-MOVEMENT-QNT.
            START MOVEMENT
              KEY <= SAL-QUANT.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
              GO TO READ-MOVEMENT-EXIT.
           IF WS-STATUS = "23"
              MOVE 44   TO WS-F-ERROR
              GO TO READ-MOVEMENT-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 44   TO WS-F-ERROR
               PERFORM READ-ERROR.
            GO TO READ-MOVEMENT-EXIT.

       START-AT-MOVEMENT-TSELL.
            START MOVEMENT
              KEY <= SAL-TSELL.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
              GO TO READ-MOVEMENT-EXIT.
           IF WS-STATUS = "23"
              MOVE 44   TO WS-F-ERROR
              GO TO READ-MOVEMENT-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 44   TO WS-F-ERROR
               PERFORM READ-ERROR.
            GO TO READ-MOVEMENT-EXIT.

       START-AT-MOVEMENT-GP.
            START MOVEMENT
              KEY <= SAL-GP.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
              GO TO READ-MOVEMENT-EXIT.
           IF WS-STATUS = "23"
              MOVE 44   TO WS-F-ERROR
              GO TO READ-MOVEMENT-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 44   TO WS-F-ERROR
               PERFORM READ-ERROR.

       READ-MOVEMENT-EXIT.
             EXIT.

      *
      *   R E W R I T E   &    W R I T E   F I L E S   R O U T I N E S
      *
       AD000-WRITE             SECTION.

       REWRITE-MOVEMENT.
            REWRITE SAL-REC1.
           IF WS-STAT1 NOT = "0"
              MOVE 44   TO WS-F-ERROR
               PERFORM WRITE-ERROR.
            GO TO WRITE-MOVEMENT-EXIT.

       WRITE-MOVEMENT.
            WRITE SAL-REC1.
           IF WS-STAT1 NOT = "0"
              IF WS-STATUS = "22"
           GO TO REWRITE-MOVEMENT
               ELSE
           MOVE 44  TO WS-F-ERROR
                   PERFORM WRITE-ERROR.

       WRITE-MOVEMENT-EXIT.
             EXIT.

       COPY "APAC.HLP".

      /
      *  ****  E N Q U I R Y   ( S A L E S   P E R   L E D G E R )
      *
       BA000        SECTION 50.
       BA00.
             PERFORM ERASE-SCREEN.
      PERFORM SAVE-SCREEN-3.
      MOVE 5   TO SHADE-ROW.
      MOVE 27   TO SHADE-COL.
      MOVE 30   TO SHADE-WIDTH.
      MOVE 7   TO SHADE-LINES.
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
       GO TO BA999
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
             MOVE W15-SMM            TO WS-CRDX.
           IF NOT(W05-CID(WS-CRDX) = W02-MTH43)
               CLOSE CARDEX
               MOVE W05-CID(WS-CRDX) TO W02-MTH43
               OPEN I-O CARDEX
               IF WS-STATUS = "05"
                   CLOSE CARDEX
                   OPEN OUTPUT CARDEX
                   CLOSE CARDEX
                   OPEN I-O CARDEX.
             MOVE W15-START          TO W22-DTE1.
             MOVE W15-END            TO W22-DTE2.
      DISPLAY WS-BLNK78 AT 1302
       WITH FOREGROUND-COLOR Cyan BACKGROUND-COLOR Blue.
      DISPLAY WS-BLNK78 AT 1502
       WITH FOREGROUND-COLOR Cyan BACKGROUND-COLOR Blue.
      DISPLAY "Extracting details requested" AT 1312.
      DISPLAY "Reading:" AT 1512.
      MOVE W15-S-YMD  TO CRD-DTE.
      MOVE SPACES  TO CRD-CODE.
      PERFORM START-AT-CRDX-SDTE THRU READ-CARDEX-EXIT.
          IF WS-F-ERROR = 43
        MOVE "No data for time period selected"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BA00.
      MOVE SPACES  TO SAL-CODE.
      PERFORM SAVE-SCREEN-2.

       BA10.
            PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
    IF WS-F-ERROR = 43
        GO TO BA16.
    IF CRD-DTE > W15-E-YMD
        GO TO BA20.
      DISPLAY CRD-CODE AT 1521
       WITH BACKGROUND-COLOR Magenta
     FOREGROUND-COLOR Cyan HIGHLIGHT.
    IF CRD-CODE = SAL-CODE
        GO TO BA15.
      MOVE CRD-CODE  TO SAL-CODE STK-CODE.
      PERFORM READ-STOCK THRU READ-STOCK-EXIT.
    IF WS-F-ERROR = 22
        MOVE "No Stock Details for this Code"
     TO STK-DESC.
      MOVE STK-LEDG  TO SAL-LEDG.
      PERFORM READ-MOVEMENT THRU READ-MOVEMENT-EXIT.
    IF WS-F-ERROR = 44
        MOVE STK-DESC  TO SAL-DESC
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
        PERFORM WRITE-MOVEMENT THRU WRITE-MOVEMENT-EXIT
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
      PERFORM REWRITE-MOVEMENT THRU WRITE-MOVEMENT-EXIT.
      GO TO BA10.

       BA16.
    IF WS-CRDX = W15-EMM
        GO TO BA20.
      ADD 1   TO WS-CRDX.
    IF WS-CRDX > 12
        MOVE 1   TO WS-CRDX.
      CLOSE CARDEX.
      MOVE W05-CID (WS-CRDX)
     TO W02-MTH43.
      OPEN I-O CARDEX.
    IF WS-STATUS = "05"
        CLOSE CARDEX
        OPEN OUTPUT CARDEX
        CLOSE CARDEX
        OPEN I-O CARDEX.
      MOVE W15-S-YMD  TO CRD-DTE.
      MOVE SPACES  TO CRD-CODE.
      PERFORM START-AT-CRDX-SDTE THRU READ-CARDEX-EXIT.
          IF WS-F-ERROR = 43
        GO TO BA20.
      GO TO BA10.

       BA20.
      DISPLAY WS-BLNK78 AT 1302
       WITH FOREGROUND-COLOR Cyan BACKGROUND-COLOR Blue.
      DISPLAY WS-BLNK78 AT 1502
       WITH FOREGROUND-COLOR Cyan BACKGROUND-COLOR Blue.
      DISPLAY "Sales Ledger Required:" AT 1312.
      ACCEPT S07 AT 1335.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE ZERO  TO WS-LEDG
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO BA20
        END-EVALUATE.
          IF WS-LEDG > 296
              GO TO BA20.
          IF WS-LEDG = ZERO
        GO TO BA999.
          IF W05-S (WS-LEDG) = ZERO
              GO TO BA20.
            MOVE WS-LEDG  TO WS-DEPKEY.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
      DISPLAY DPT-DESC AT 1435
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
       BA25.
      MOVE "'C'ontinue or 'R'ekey  [ ]"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      MOVE 16   TO SLIN.
      PERFORM OPT-SETUP THRU OPT-EXIT.
    IF NOT (WS-OPTION = "R" OR "C")
        GO TO BA25.
           IF WS-OPTION = "R"
        GO TO BA20.
      MOVE WS-LEDG  TO SAL-LEDG.
      MOVE "    !"  TO SAL-CODE.
      PERFORM START-AT-MOVEMENT-CODE THRU READ-MOVEMENT-EXIT.
    IF WS-F-ERROR = 44
        GO TO BA35.
      MOVE ZERO   TO WS-PAGE.

       BA30.
      PERFORM READ-MOVEMENT-NEXT THRU READ-MOVEMENT-EXIT.
    IF WS-F-ERROR = 44
        IF WS-PAGE = ZERO
     GO TO BA35
        END-IF
        GO TO BA40.
    IF NOT (WS-LEDG = SAL-LEDG)
        IF WS-PAGE = ZERO
     GO TO BA35
        END-IF
        GO TO BA40.
    IF WS-PAGE = ZERO
        PERFORM BB000.
      MOVE SAL-CODE  TO SRP-ITEM.
      MOVE SAL-DESC  TO SRP-DESC.
      MOVE SAL-QNT  TO SRP-QUANT.
      MOVE SAL-COST  TO SRP-COST.
      MOVE SAL-SELL  TO SRP-SELL.
      COMPUTE SRP-EXTEND ROUNDED = SAL-SELL / SAL-QNT.
    IF LINAGE-COUNTER < WS-PRN-LENGTH
              WRITE R-L1 BEFORE 1 LINES
           ELSE
              WRITE R-L1 BEFORE PAGE
              PERFORM BB000.
      PERFORM PRINT-ABORT.
    IF WS-OPTION = "Y"
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
     TO R-DETAIL
        WRITE R-L1 BEFORE PAGE
              GO TO BA45.
            MOVE SPACES  TO R-DETAIL.
      GO TO BA30.

       BA35.
      MOVE "No details for period selected"
     TO WS-ERR-MES.
            PERFORM ERROR-MESSAGE.
      GO TO BA20.

       BA40.
      MOVE "******   E N D   O F   R E P O R T   ******"
     TO R-DETAIL.
            WRITE R-L1 BEFORE PAGE.
            MOVE SPACES  TO R-DETAIL.
       BA45.
      PERFORM RESTORE-SCREEN-2.
      GO TO BA20.

       BA999.
             EXIT.

       BB000        SECTION 50.
       BB00.
      *
      *    ****   1 O C P I ( N O R M A L P R I N T )
      *
      MOVE WS-10CPI  TO R-DETAIL.
      WRITE R-L1 BEFORE 0 LINES.
            MOVE SPACES  TO R-DETAIL.
            WRITE R-L1 BEFORE 1 LINES.
            MOVE "PAGE:"  TO R-PGE.
             MOVE "DATE:"        TO R-DATE.
             ADD 1               TO WS-PAGE.
             MOVE WS-PAGE        TO R-PNO.
             MOVE W12-TODAY      TO R-DTE.
             MOVE W95-COMP       TO R-CPY.
            WRITE R-L1 BEFORE 1 LINES.
            MOVE SPACES  TO R-DETAIL.
            MOVE W55-HEADING  TO R-CPY.
            WRITE R-L1 BEFORE 1 LINES.
            MOVE SPACES  TO R-DETAIL.
            MOVE "SALES LEDGER:"
     TO SRP-H1.
            MOVE WS-LEDG  TO SRP-NO.
            MOVE DPT-DESC  TO SRP-SMAN.
            WRITE R-L1 BEFORE 1 LINES.
      *
      *    ****   1 2 C P I
      *
      MOVE WS-12CPI  TO R-DETAIL.
      WRITE R-L1 BEFORE 0 LINES.
            MOVE SPACES  TO R-DETAIL.
      MOVE ALL "-"  TO R-HL.
            WRITE R-L1 BEFORE 1 LINES.
            MOVE SPACES  TO R-DETAIL.
            MOVE "Item Code"  TO SRP-H4.
            MOVE "Description"  TO SRP-H5.
      MOVE "Quantity"  TO SRP-H6.
      MOVE "Total Cost"  TO SRP-H7.
      MOVE "Total Sell"  TO SRP-H8.
      MOVE "Avg Sell"  TO SRP-H9.
            WRITE R-L1 BEFORE 1 LINES.
      MOVE ALL "-"  TO R-HL.
            WRITE R-L1 BEFORE 2 LINES.
            MOVE SPACES  TO R-DETAIL.

       BB999.
      EXIT.

      /
       BE000  SECTION 51.
       BE00.
             DISPLAY S04.
            MOVE 1   TO WS-S2 WS-S3 WS-DEPKEY.
       BE20.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
      MOVE WS-DEPKEY  TO WS-NUM.
      DISPLAY WS-NUM AT 1220
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      DISPLAY DPT-DESC AT 1224
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
       BE25.
           IF DPT-DESC NOT = SPACES
              MOVE WS-S2  TO W05-S (WS-S2).
          IF WS-S2 < 296
              ADD 1   TO WS-S2
              ADD 1   TO WS-DEPKEY
               GO TO BE20.
       BE99.
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
             MOVE 110            TO WS-USUB.

       ZA01A.
             ADD 2549 WS-USUB    GIVING W25-RESULT.
             MOVE W25-KEY        TO WS-PARKEY.
             ADD 1 W25-SUB       GIVING WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF TRM-STDP (WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 9
        MOVE LS-USER  TO W02-USER.
             ADD 501 TRM-PRN3 (WS-S6)
                                 GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S6.
             ADD 1               TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE TRM-CONP (WS-S6)
                                 TO WS-COND.
             MOVE TRM-NORP (WS-S6)
                                 TO WS-NORM.
             MOVE TRM-EXPP (WS-S6)
                                 TO WS-EXPP.
             MOVE TRM-ECAN (WS-S6)
                                 TO WS-ECAN.
             MOVE TRM-8LPI (WS-S6)
                                 TO WS-8LPI.
             MOVE TRM-6LPI (WS-S6)
                                 TO WS-6LPI.
             MOVE TRM-10CPI (WS-S6)
                                 TO WS-10CPI.
             MOVE TRM-12CPI (WS-S6)
                                 TO WS-12CPI.
             MOVE TRM-17CPI (WS-S6)
                                 TO WS-17CPI.
      MOVE TRM-LENGTH (WS-S6)
     TO WS-PRN-LENGTH.
      MOVE TRM-PAGE (WS-S6)
     TO WS-PGE-LENGTH.
       ZA01B.
      *
      *    ****    SET PRINTER TO 8 LPI
      *
      COMPUTE WS-PGE-LENGTH = WS-PGE-LENGTH / 6 * 8.
    IF WS-PGE-LENGTH > 88
        SUBTRACT 7 FROM WS-PGE-LENGTH
     GIVING WS-PRN-LENGTH
    ELSE
        SUBTRACT 6 FROM WS-PGE-LENGTH
     GIVING WS-PRN-LENGTH.
             OPEN OUTPUT PRNREP.
      CALL X"91" USING X91-RES X91-FUN PRNREP.
      MOVE WS-8LPI  TO R-L1.
            WRITE R-L1 BEFORE 0 LINES.
      MOVE WS-10CPI  TO R-L1.
            WRITE R-L1 BEFORE 0 LINES.
      MOVE SPACES  TO R-L1.

       ZA02.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY    TO W95-COMP.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
      MOVE 1   TO WS-COMP.

       ZA05-VAT.
      STRING "VAT" DELIMITED SIZE
       WS-COMP DELIMITED SIZE
     INTO DPT-CODE.
      PERFORM READ-DEPART THRU READ-DEPART-EXIT.
      MOVE DPT-R-DATE  TO WS-VAT-DATE(WS-COMP).
      MOVE DPT-RATE  TO W05-VAT(WS-COMP).
      ADD 6 WS-COMP  GIVING WS-S1.
      MOVE DPT-P-RATE  TO W05-VAT(WS-S1).
    IF WS-COMP < 6
        ADD 1   TO WS-COMP
        GO TO ZA05-VAT.
            OPEN OUTPUT MOVEMENT.
          IF RUNTIME-ERROR
               IF FLE-LIMIT
           GO TO ZA49.
           IF WS-STATUS NOT = "00"
              MOVE 44   TO WS-F-ERROR
               PERFORM OPEN-ERROR.
      CLOSE MOVEMENT.
      OPEN I-O MOVEMENT.
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
               WHEN 15    MOVE WS-PARID     TO WS-FILE
                          MOVE WS-PARKEY    TO WS-KEY
               WHEN 22    MOVE W02-STOCKF   TO WS-FILE
                          MOVE STK-CODE     TO WS-KEYX
               WHEN 43    MOVE W02-CARDEX   TO WS-FILE
                          MOVE CRD-KEY      TO WS-KEYX
               WHEN 44    MOVE W02-MOVEMENT TO WS-FILE
                          MOVE SAL-KEY      TO WS-KEYX
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "LEDGSAL"    TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
