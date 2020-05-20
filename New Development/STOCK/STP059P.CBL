      $set LINKCOUNT"512" GNT"STP059P.GNT"
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
      *  ******  ******** *******    ****   ********  ******  *******  *
      * **    **    **    **    **  **  **  **       **    ** **    ** *
      * **          **    **    ** **    ** *******  **    ** **    ** *
      *  ******     **    ******   **    **       **  ******* ******   *
      *       **    **    **       **    **       **       ** **       *
      * **    **    **    **        **  **  **    ** **    ** **       *
      *  ******     **    **         ****    ******   ******  **       *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *   S T O C K   O R D E R   S H E E T   -   P A N O R A M A      *
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
       PROGRAM-ID.     STP059P.
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

       COPY "APACFIDS.SL".

       COPY "CONTROL.SL".

       COPY "DEBTOR.SL".

       COPY "DEBMEM.SL".

       COPY "DEPART.SL".

       COPY "PARAM.SL".

       COPY "STKORD.SL".

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

       COPY "DEBTOR.FDE".

       COPY "DEBMEM.FDE".

       COPY "DEPART.FDE".

       COPY "PARAM.FDE".

       COPY "STKORD.FDE".

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
       77  WS-STKKEY    PIC 9(04)  COMP-5.
       77  WS-SPAKEY    PIC 9(04)  COMP-5.
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
       77  WS-PRC    PIC X(01).
       77  WS-PRT    PIC X(01).
       77  WS-BIN    PIC X(01).
       77  WS-ETYPE    PIC X(01).
       77  WS-OPTION    PIC X(01).
       77  WS-PRICE    PIC X(01).
       77  WS-USE-CASES    PIC X(01).
       77  WS-USE-ITM    PIC X(01).
       77  WS-EXT-STK    PIC X(01).
       77  WS-NUM    PIC ZZ9.
       77  WS-ERROR    PIC 9(01).
       77  WS-ORD    PIC S9(05)  COMP-3.
       77  WS-TOTAL    PIC S9(07)V99 COMP-3.
       77  WS-VALUE    PIC S9(07)V99 COMP-3.
       77  WS-CCOST    PIC S9(07)V99 COMP-3.
       77  WS-CSELL    PIC S9(07)V99 COMP-3.
       77  WS-SCOST    PIC S9(07)V99 COMP-3.
       77  WS-SSELL    PIC S9(07)V99 COMP-3.
       77  WS-TCOST    PIC S9(07)V99 COMP-3.
       77  WS-TSELL    PIC S9(07)V99 COMP-3.
       77  WS-DEPT    PIC X(04).
       77  WS-ERR1    PIC X(22) VALUE "Invalid Account Number".
       77  WS-ERR2         PIC  X(09) VALUE "No Record".
       77  WS-DESC-H1    PIC X(14) VALUE "Description".
       77  WS-DESC-H2    PIC X(14) VALUE "Description 2".
       77  WS-RT-HD    PIC X(10) VALUE "-Retail   ".
       77  WS-WS-HD    PIC X(10) VALUE "-Wholesale".
       77  WS-CS-HD    PIC X(10) VALUE "-Cash sale".
       77  PRG-NAME                PIC  X(12)           VALUE SPACES.
       77  PRG-DEBT-LUP            PIC  X(12)           VALUE "DTP\DTPLOOK".
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STP059P".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY ORDER SHEET REPORT".
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
           03  WS-MODULE           PIC  X(03)           VALUE "STP".
           03  WS-PROG             PIC  X(03)           VALUE "059".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-PRINTER-DETAILS.
           03  W02-PRINTER         PIC  X(12).
           03  W02-PGE-LENGTH      PIC  9(02).
           03  W02-PRN-LENGTH      PIC  9(02).
           03  W02-LINAGE          PIC  9(02).
           03  W02-PRN-STATUS      PIC  X(01)           VALUE "C".
      *
      *    ****    D  =  Detail line
      *            C  =  Condensed print
      *            E  =  Expanded print
      *            H  =  Header line
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
        05  REP-HEADINGS.
     07  FILLER  PIC  X(14).
     07  REP-AC-NAME
          PIC  X(40).
        05  REP-HEADINGS-2 REDEFINES REP-HEADINGS.
     07  FILLER  PIC  X(08).
     07  REP-P-DATE
          PIC  Z9/99/9999.
     07  FILLER  PIC  X(36).
        05  FILLER      PIC  X(26).
          03  REP-LINE6 REDEFINES W02-PRN-LINE.
              05  REP-HEAD2   PIC  X(10).
              05  REP-CNO     PIC  9(04).
              05  REP-CATNO REDEFINES REP-CNO PIC X(04).
              05  FILLER      PIC  X(01).
              05  REP-CAT     PIC  X(30).
              05  FILLER      PIC  X(35).
          03  REP-LINE9 REDEFINES W02-PRN-LINE.
              05  REP-PG      PIC  X(06).
              05  REP-P-N     PIC  Z(03)9.
              05  FILLER      PIC  X(22).
              05  REP-COMP    PIC  X(40).
              05  REP-REPORT  PIC  X(34).
              05  FILLER      PIC  X(10).
              05  REP-DAT     PIC  X(06).
              05  REP-DT      PIC  Z9/99/9999.
          03  REP-LINE9B REDEFINES W02-PRN-LINE.
              05  S-H1        PIC  X(04).
              05  FILLER      PIC  X(02).
              05  S-H2        PIC  X(30).
              05  FILLER      PIC  X(02).
              05  S-H3        PIC  X(08).
              05  FILLER      PIC  X(02).
              05  S-H4        PIC  X(09).
              05  FILLER      PIC  X(02).
              05  S-H5        PIC  X(09).
              05  FILLER      PIC  X(02).
              05  S-L1        PIC  X(01).
              05  S-H6        PIC  X(06).
              05  S-L2        PIC  X(01).
              05  S-H7        PIC  X(06).
              05  S-L3        PIC  X(01).
              05  S-H8        PIC  X(06).
              05  S-L4        PIC  X(01).
              05  S-H9        PIC  X(06).
              05  S-L5        PIC  X(01).
              05  S-H10       PIC  X(06).
              05  S-L6        PIC  X(01).
              05  S-H11       PIC  X(06).
              05  S-L7        PIC  X(01).
              05  S-H12       PIC  X(06).
              05  S-L8        PIC  X(01).
              05  S-H13       PIC  X(12).
          03  REP-LINE9C REDEFINES W02-PRN-LINE.
        05  S-CDE       PIC  X(04).
              05  FILLER      PIC  X(02).
              05  S-DSC       PIC  X(30).
              05  FILLER      PIC  X(02).
              05  S-PER       PIC  X(08).
              05  FILLER      PIC  X(02).
              05  S-COST      PIC  Z(05)9.99.
              05  FILLER      PIC  X(02).
              05  S-SELL      PIC  Z(05)9.99.
              05  FILLER      PIC  X(02).
        05  S-LINE.
           07  FILLER  PIC  X(01).
           07  S-SUN   PIC  X(06).
           07  FILLER  PIC  X(01).
           07  S-MON   PIC  X(06).
           07  FILLER  PIC  X(01).
           07  S-TUE   PIC  X(06).
           07  FILLER  PIC  X(01).
           07  S-WED   PIC  X(06).
           07  FILLER  PIC  X(01).
           07  S-THU   PIC  X(06).
           07  FILLER  PIC  X(01).
           07  S-FRI   PIC  X(06).
           07  FILLER  PIC  X(01).
           07  S-SAT   PIC  X(06).
           07  FILLER  PIC  X(01).
              05  S-TOT       PIC  X(12).
          03  REP-LINE11 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(13).
              05  REP-HEAD13  PIC  X(26).
              05  FILLER      PIC  X(02).
              05  REP-DCOST   PIC  Z(08)9.99.
              05  FILLER      PIC  X(02).
              05  REP-DSELL   PIC  Z(08)9.99.
              05  FILLER      PIC  X(13).

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

       01  W30-COST-CODE.
           03  W30-CODE           PIC X(01) OCCURS 10.

       01  W35-COST.
           03  W35-CST            PIC 9(08)V99.
           03  W35-WHOLE  REDEFINES W35-CST
                                  PIC 9(10).
           03  W35-CPRINT.
               05  W35-CEDIT      PIC ZZZZZZ9999.
           03  W35-EOC REDEFINES W35-CPRINT.
               05  W35-CHAR       PIC X(01) OCCURS 10.
           03  W35-EON REDEFINES W35-CPRINT.
               05  W35-NUM        PIC 9(01) OCCURS 10.
           03  W35-CPRN REDEFINES W35-CPRINT.
               05  FILLER         PIC  X(01).
               05  W35-CSTPRN     PIC  X(09).
           03  W35-S1             PIC 9(02) COMP-5.
           03  W35-S2             PIC 9(02) COMP-5.

       COPY "W40.DBT".

       01  W40-DATES.
    03  W40-DATE1   PIC  9(08).
           03  W40-DTE1 REDEFINES W40-DATE1.
        05  W40-C1   PIC  9(02).
        05  W40-Y1   PIC  9(02).
        05  W40-M1   PIC  9(02).
        05  W40-D1   PIC  9(02).
    03  W40-DATE2   PIC  9(08).
           03  W40-DTE2 REDEFINES W40-DATE2.
        05  W40-C2   PIC  9(02).
        05  W40-Y2   PIC  9(02).
        05  W40-M2   PIC  9(02).
        05  W40-D2   PIC  9(02).

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W60-NME-ADD.
           03  W60-NAME.
               05  W60-NCHAR OCCURS 40 PIC X(01).

       COPY "W70STOCK.WS".

       01  W70-KEYS.
           03  W70-BIND        PIC 9(05)     COMP-0 VALUE ZERO.

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
           03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
           03  W75-DNO        PIC X(06) OCCURS 18.

       01  W80-ACCOUNT.
           03  W80-ACNO        PIC X(06).
           03  W80-TYPE        PIC 9(01).

       01  W95-COMP        PIC X(40).

       01  W100-EDIT.
           03  W100-V2.
               05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.
           03  W100-LBAL       PIC Z(06)9.99-.
           03  W100-LARR       PIC Z(06)9.99.
           03  W100-BAL        PIC Z(08)9.99-.
           03  W100-CUR        PIC Z(08)9.99-.
           03  W100-30         PIC Z(08)9.99-.
           03  W100-60         PIC Z(08)9.99-.
           03  W100-90         PIC Z(08)9.99-.
           03  W100-120        PIC Z(08)9.99-.
           03  W100-INT        PIC Z(08)9.99.
           03  W100-CRL        PIC Z(06)9.
           03  W100-KEY        PIC Z(05)9.

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
          03  LINE 10 COLUMN 22 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC X(14) FROM STK-ITEM.

       01  S03-ITM.
          03  LINE 10 COLUMN 22 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC X(14) FROM STK-ITEM.
    03        COLUMN 36 FOREGROUND-COLOR Black BACKGROUND-COLOR Magenta
     VALUE "/".
    03        COLUMN 37 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(03) FROM STK-ITM.

       01  S03-EXT.
          03  LINE 10 COLUMN 22 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC X(18) FROM STK-CODE.

       01  S10A.
           03  LINE  2 COLUMN 32 FOREGROUND-COLOR Grey HIGHLIGHT
     VALUE "STOCK ORDER SHEET".
          03  LINE  4 COLUMN  8 FOREGROUND-COLOR Brown HIGHLIGHT VALUE
           "Busy printing Stock-order sheet".
          03  LINE  6 COLUMN  8 VALUE "Are you sure (".
    03        COLUMN 22 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "Y".
    03        COLUMN 23 VALUE "/".
    03        COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "N".
    03        COLUMN 25 VALUE ")".

       01  S11A.
    03  LINE  8 COLUMN  8 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "Extracting details".

       01  S15.
          03  LINE 10 COLUMN  8 VALUE "Stock record:".

       01  S36.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC X(06) USING W80-ACNO AUTO.

       COPY "DEBLUP.CRT".

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
           IF LS0-STLEV < 2
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM BE000.
           IF WS-ERROR = 9
               MOVE "No details in Department file" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO AA49.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *     **  *   *   * *   *  *  **  *   *   *     *   *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *     *  **   *     *   *  *  *  **   *   *     *  *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** *   *
      *
             PERFORM OPEN-PRINTER.
             MOVE "P"                TO WS-COMMAND.
             PERFORM DE00-REPORT.
      *
      *    ****   1 O C P I ( N O R M A L P R I N T )
      *
           IF NOT(WS-PRINTER = "S")
               MOVE 0                TO WS-ADVANCE
               MOVE 1                TO W02-PRN-TYPE
               PERFORM CALL-PRINTUTL
               MOVE "D"              TO W02-PRN-TYPE
               MOVE SPACES           TO REP-DETAIL1.
             MOVE "C"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             CLOSE DEBTOR DEBMEM STKORD.

       AA49.
             EXIT PROGRAM.

       COPY "AA50D.LUP".

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

       COPY "ABORT.PRN".

       COPY "PRINTUTL.AP3".

       COPY "DEBTOR.ILU".

       COPY "APAC.HLP".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "DEBTOR.RD".

       COPY "DEBMEM.RD".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "STOCK.RD".

       COPY "STOCK.1ST".

      /
       BE000  SECTION.
       BE00.
      MOVE "!"   TO DPT-CODE.
      PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.
    IF WS-F-ERROR = 7
        MOVE 9   TO WS-ERROR
    ELSE
        MOVE ZERO  TO WS-ERROR.

       BE09.
      EXIT.
      *
      *    ****    Read debtor record - using account number.
      *
       CA155-GET-DEBTOR  SECTION 2.
       CA155.
             MOVE W80-ACNO       TO DEB-ACNO.
             PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               MOVE 1            TO WS-ERROR
           ELSE
           IF DEB-ACNO NOT = W80-ACNO
               MOVE 2            TO WS-ERROR
           ELSE
               MOVE 0            TO WS-ERROR
               MOVE DEB-TYPE     TO W80-TYPE.

       CA160-EXIT.
             EXIT.
      /
       DE00-REPORT     SECTION 5.
       DE00.
             PERFORM ERASE-SCREEN.
             DISPLAY S10A.
            MOVE "Y"   TO WS-OPTION.

       DE05.
            ACCEPT WS-OPTION AT 0632
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
          BACKGROUND-COLOR Magenta UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
          IF NOT(WS-OPTION = "Y" OR "N")
              GO TO DE05.
           IF WS-OPTION = "N"
              GO TO DE999.
      DISPLAY S11A.
      MOVE "98"   TO STK-LDG.
      MOVE "!"   TO STK-CODE.
             PERFORM START-AT-STOCK-LEDG THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
              GO TO DE20.
            DISPLAY S15.
      MOVE ZERO   TO WS-S8.

       DE10.
      PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
    IF WS-F-ERROR = 22
        GO TO DE15.
    IF NOT(STK-LDG = "98  ")
        GO TO DE15.
    IF STK-IND > 1
        GO TO DE10.
    IF WS-USE-ITM = "Y"
        DISPLAY S03-ITM
    ELSE
    IF WS-EXT-STK = "Y"
        DISPLAY S03-EXT
    ELSE
              DISPLAY S03.
      MOVE STK-RECORD1  TO SOR-RECORD1.
      WRITE SOR-RECORD1.
      ADD 1   TO WS-S8.
      GO TO DE10.

       DE15.
    IF WS-S8 = ZERO
        GO TO DE20.
      MOVE "!"   TO SOR-DESC.
            START STKORD
              KEY >= SOR-DKEY.
          IF WS-STATUS = "00"
        PERFORM SAVE-SCREEN-3
        GO TO DE25.

       DE20.
      MOVE "No details for report" TO WS-ERR-MES.
            PERFORM ERROR-MESSAGE.
      GO TO DE999.
      *
      *    ****   G E T   T H E   D E B T O R A C C O U N T
      *    N U M B E R F O R W H I C H   T H E
      *    L I S T   M U S T   B E   P R I N T E D
      *
       DE25.
            DISPLAY WS-BLNK78 AT 1002
       WITH FOREGROUND-COLOR Cyan BACKGROUND-COLOR Blue HIGHLIGHT.
            DISPLAY "Account No:" AT 1008.
      DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "=Help,"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT
       "F2" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       " to do Debtor Lookup "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT
       "Esc" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       " to exit"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT.
      MOVE SPACES  TO W80-ACNO.

       DE30.
            ACCEPT W80-ACNO AT 1020
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W80-ACNO
       GO TO DE999
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA50
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO DE30
        END-EVALUATE
        DISPLAY S36 AT 1020
               IF W80-ACNO = SPACES
           GO TO DE30.
            DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
      PERFORM CLEAR-L50.
           IF W80-ACNO = SPACES
              GO TO DE999.
             MOVE 6              TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W80-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
             MOVE W80-ACNO       TO DEB-ACNO.
             PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR = 2
              MOVE WS-ERR1  TO WS-ERR-MES
        PERFORM ERROR-MESSAGE
              GO TO DE30.
           IF WS-ERROR = 1
              MOVE WS-ERR2  TO WS-ERR-MES
        PERFORM ERROR-MESSAGE
              GO TO DE30.
            MOVE DEB-NAME  TO W60-NAME.
          IF W80-TYPE > 1
              GO TO DE35.
             INSPECT W60-NAME REPLACING ALL "*" BY " ".
             INSPECT W60-NAME REPLACING FIRST "!" BY " ".

       DE35.
      DISPLAY W60-NAME AT 1027
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO DE25.

       DE40.
            MOVE SPACES  TO REP-DETAIL1.
            PERFORM DE95 THRU DE999.
            MOVE ZERO   TO WS-PAGE.
            GO TO DE110.

       DE45.
      *
      *    ****   1 0 C P I ( N O R M A L P R I N T )
      *
    IF NOT(WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE 1   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
              MOVE SPACES  TO REP-DETAIL1.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE "Customer Order Guide"
     TO REP-DETAIL1.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE "Customer Code"
     TO REP-DETAIL1.
      MOVE W80-ACNO  TO REP-AC-NAME.
      PERFORM CALL-PRINTUTL.
      MOVE "Customer Name"
     TO REP-DETAIL1.
      MOVE W60-NAME  TO REP-AC-NAME.
      PERFORM CALL-PRINTUTL.
      MOVE "Printed"  TO REP-DETAIL1.
      MOVE TODAY-DDMMYY  TO REP-P-DATE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      *
      *    ****   1 7 C P I ( C O N D E N S E D   P R I N T )
      *
    IF NOT(WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE 3   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
              MOVE SPACES  TO REP-DETAIL1
        MOVE "H"   TO W02-PRN-TYPE.
            MOVE ALL "Ä"  TO REP-LINE9B.
      MOVE 1   TO WS-ADVANCE.
            PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
            MOVE "Code"  TO S-H1.
            MOVE "Description"  TO S-H2.
            MOVE "Size"  TO S-H3.
            MOVE "     Cost"  TO S-H4.
            MOVE "  Selling"  TO S-H5.
            MOVE " Sun"  TO S-H6.
            MOVE " Mon"  TO S-H7.
            MOVE " Tue"  TO S-H8.
            MOVE " Wed"  TO S-H9.
            MOVE " Thu"  TO S-H10.
            MOVE " Fri"  TO S-H11.
            MOVE " Sat"  TO S-H12.
            MOVE "Total Order"  TO S-H13.
            PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
            MOVE ALL "Í"  TO REP-LINE9B.
            MOVE "Ñ"   TO S-L1 S-L2 S-L3 S-L4
        S-L5 S-L6 S-L7 S-L8.
            PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE "D"   TO W02-PRN-TYPE.

       DE50.
             EXIT.

       DE55.
            MOVE "³"   TO S-L1 S-L2 S-L3 S-L4
        S-L5 S-L6 S-L7 S-L8.

       DE95.
            MOVE 0   TO WS-S1.
            PERFORM DE45.
      MOVE "!"   TO SOR-DESC.
            START STKORD
              KEY >= SOR-DKEY.

       DE100.
            READ STKORD NEXT  WITH IGNORE LOCK.
             PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              GO TO DE999.
    IF NOT(WS-STAT1 = "0")
               GO TO DE105.
    IF NOT(WS-PRINTER = "S")
        MOVE SOR-CODE  TO STK-CODE
        IF WS-USE-ITM = "Y"
     DISPLAY S03-ITM
        ELSE
        IF WS-EXT-STK = "Y"
     DISPLAY S03-EXT
        ELSE
           DISPLAY S03.
            MOVE SOR-CODE  TO S-CDE.
            MOVE SOR-DESC  TO S-DSC.
      MOVE SOR-PER  TO S-PER.
      MOVE SOR-SELL  TO S-COST.
      MOVE SOR-CASH  TO S-SELL.
            PERFORM DE55.
      MOVE ZERO   TO WS-ADVANCE.
            PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE ALL "_"  TO S-LINE.
      MOVE 1   TO WS-ADVANCE.
    IF W02-LINAGE < W02-PRN-LENGTH
        PERFORM CALL-PRINTUTL
          ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM DE45.
            MOVE SPACES  TO REP-DETAIL1.

       DE102.
             GO TO DE100.

       DE105.
             MOVE ALL "Í"            TO REP-LINE9B.
             MOVE 99                 TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
      *
      *    ****   1 0 C P I ( N O R M A L P R I N T )
      *
           IF NOT(WS-PRINTER = "S")
               MOVE 0                TO WS-ADVANCE
               MOVE 1                TO W02-PRN-TYPE
               PERFORM CALL-PRINTUTL
               MOVE SPACES           TO REP-DETAIL1
               MOVE "D"              TO W02-PRN-TYPE.
             GO TO DE999.

       DE110.
             MOVE "'N'ext or 'E'xit  [N]" TO WS-OPT-MES.
             MOVE "N"                TO WS-OPTION.
             MOVE "I"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "N"
               PERFORM RESTORE-SCREEN-3
               GO TO DE25.
             MOVE "E"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             MOVE "P"                TO WS-COMMAND.
             MOVE SPACES             TO REP-DETAIL1.

       DE999.
             EXIT.
      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT        SECTION 8.
       ZA000-OPEN.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY.
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
      MOVE PAR-CASES  TO WS-USE-CASES.
      MOVE PAR-USE-ITM  TO WS-USE-ITM.
      MOVE PAR-EXT-STK  TO WS-EXT-STK.
            OPEN OUTPUT STKORD.
            CLOSE STKORD.
            OPEN I-O STKORD.

       ZA01B.
      MOVE 6   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-WHOLESALE  TO WS-WS.
            MOVE PAR-COST-CODE  TO W30-COST-CODE.
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
             INSPECT WS-CS-HD REPLACING LEADING "-" BY " ".
             INSPECT WS-WS-HD REPLACING LEADING "-" BY " ".
             INSPECT WS-RT-HD REPLACING LEADING "-" BY " ".
             MOVE 1                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W40-DATE1 W40-DATE2.
           IF W40-M2 > 3
               SUBTRACT 3        FROM W40-M2
           ELSE
               SUBTRACT 1        FROM W40-Y2
               ADD 9             TO W40-M2.
            MOVE PAR-COMPANY  TO W95-COMP.
            OPEN I-O DEBTOR.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
        IF IDX-CORRUPT
                   MOVE 6        TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
              MOVE 6   TO WS-F-ERROR
        IF WS-STATUS = "39"
           GO TO ZA50
        ELSE
        IF NOT(WS-STATUS = "41")
           PERFORM OPEN-ERROR.
            OPEN I-O DEBMEM.
           IF WS-STATUS = "05"
               CLOSE DEBMEM
               OPEN OUTPUT DEBMEM
               CLOSE DEBMEM
               OPEN I-O DEBMEM.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
        IF IDX-CORRUPT
                   MOVE 27       TO WS-F-ERROR
                   GO TO ZA50.
    IF WS-STATUS NOT = "00"
        MOVE 27   TO WS-F-ERROR
        IF WS-STATUS = "39"
           GO TO ZA50
        ELSE
        IF NOT(WS-STATUS = "41")
     PERFORM OPEN-ERROR.

       ZA05.
            DISPLAY WS-BLNK78 AT 1402 WITH FOREGROUND-COLOR Cyan.
            GO TO ZA999-EXIT.

       COPY "ZA49.PRO".

       ZA50.
          IF WS-F-ERROR = 6
               MOVE "DEBTOR file"
                                 TO WS-FILE
           ELSE
          IF WS-F-ERROR = 22
              MOVE "STOCK file" TO WS-FILE.
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
               WHEN 6     MOVE W02-DEBTOR   TO WS-FILE
                          MOVE DEB-ACNO     TO WS-KEYX
               WHEN 7     MOVE W02-DEPART   TO WS-FILE
                          MOVE DPT-CODE     TO WS-KEYX
               WHEN 15    MOVE WS-PARID     TO WS-FILE
                          MOVE WS-PARKEY    TO WS-KEY
               WHEN 22    MOVE W02-STOCKF   TO WS-FILE
                          MOVE STK-CODE     TO WS-KEYX
               WHEN 27    MOVE W02-DEBMEM   TO WS-FILE
                          MOVE DME-KEY      TO WS-KEY
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STP059"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
