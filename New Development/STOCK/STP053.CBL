      $set LINKCOUNT"512" GNT"STP053.GNT"
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
      *    ******   ********  *******     ****    ********   ******    *
      *   **    **     **     **    **   **  **   **        **    **   *
      *   **           **     **    **  **    **  *******         **   *
      *    ******      **     ******    **    **        **      ***    *
      *         **     **     **        **    **        **        **   *
      *   **    **     **     **         **  **   **    **  **    **   *
      *    ******      **     **          ****     ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S T O C K   B Y   S U P P L I E R   R E P O R T   P R O G  *
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
       PROGRAM-ID.     STP053.
       AUTHOR.         J.W.LEMMON.
       DATE-WRITTEN.   APRIL 1990.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1990 - 2018
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

       COPY "CRDMEM.SL".

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

       COPY "APACFIDS.FDE".

       COPY "CONTROL.FDE".

       COPY "CREDIT.FDE".

       COPY "CRDMEM.FDE".

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
       77  WS-S1     PIC 9(04)   COMP-5.
       77  WS-S2            PIC 9(04)     COMP-5.
       77  WS-S3            PIC 9(04)     COMP-5.
       77  WS-S4            PIC 9(04)     COMP-5.
       77  WS-S5            PIC 9(04)     COMP-5.
       77  WS-S6            PIC 9(04)     COMP-5.
       77  WS-S7     PIC 9(04)   COMP-5.
       77  WS-S8     PIC 9(04)   COMP-5.
       77  WS-IXS           PIC 9(04)     COMP-5.
       77  WS-IXS1          PIC 9(04)     COMP-5.
       77  WS-IXS2          PIC 9(04)     COMP-5.
       77  WS-IXS3          PIC 9(04)     COMP-5.
       77  WS-IXS4          PIC 9(04)     COMP-5.
       77  WS-CAT           PIC 9(04)     COMP-5.
       77  WS-SCAT          PIC 9(04)     COMP-5.
       77  WS-PARKEY     PIC 9(04)   COMP-5.
       77  WS-NETKEY     PIC 9(04)   COMP-5.
       77  WS-PAGE     PIC 9(04)   COMP-5.
       77  WS-PQNT     PIC X(01).
       77  WS-WS            PIC X(01) VALUE "N".
       77  WS-MEMO          PIC X(01) VALUE "N".
       77  WS-PRT           PIC X(01).
       77  WS-BIN           PIC X(01).
       77  WS-ETYPE         PIC X(01).
       77  WS-OPTION     PIC X(01).
       77  WS-PRICE         PIC X(01).
       77  WS-USE-ITM    PIC X(01).
       77  WS-EXT-STK    PIC X(01).
       77  WS-NUM     PIC ZZ9.
       77  WS-ERROR         PIC 9(01).
       77  WS-KEY1          PIC 9(04)     COMP-5.
       77  WS-KSTORE        PIC 9(04)     COMP-5.
       77  WS-ART     PIC X(01).
       77  WS-SEQ     PIC X(01).
       77  WS-ORD     PIC S9(05)   COMP-3.
       77  WS-TOTAL     PIC S9(07)V99 COMP-3.
       77  WS-VALUE         PIC S9(07)V99 COMP-3.
       77  WS-CCOST         PIC S9(07)V99 COMP-3.
       77  WS-CSELL         PIC S9(07)V99 COMP-3.
       77  WS-SCOST         PIC S9(07)V99 COMP-3.
       77  WS-SSELL         PIC S9(07)V99 COMP-3.
       77  WS-EXTEND     PIC S9(09)V99 COMP-3.
       77  WS-TCOST     PIC S9(09)V99 COMP-3.
       77  WS-TSELL     PIC S9(09)V99 COMP-3.
       77  PRG-NAME                PIC  X(12)           VALUE SPACES.
       77  PRG-CRED-LUP            PIC  X(12)           VALUE "CRP\CRPLOOK".
       77  WS-DEPT     PIC X(04).
       77  WS-WEEK          PIC 9(01).
       77  WS-LABCDE        PIC X(08).
       77  WS-DESC-H1     PIC  X(14) VALUE "Description".
       77  WS-DESC-H2     PIC  X(14) VALUE "Description 2".
       77  WS-RT-HD     PIC  X(10) VALUE "-Retail   ".
       77  WS-WS-HD     PIC  X(10) VALUE "-Wholesale".
       77  WS-CS-HD     PIC  X(10) VALUE "-Cash sale".
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STP053".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY ITEMS BY SUPPLIER REPORT".
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
           03  WS-PROG             PIC  X(03)           VALUE "053".

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

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
              05  FILLER      PIC  X(10).
              05  REP-DAT     PIC  X(06).
              05  REP-DT      PIC  Z9/99/9999.
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
    03  DEP-L1 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(02).
        05  DEP-H1      PIC  X(19).
        05  DEP-H2      PIC  X(07).
        05  DEP-H3      PIC  X(34).
        05  DEP-H4      PIC  X(14).
        05  DEP-H5      PIC  X(15).
        05  DEP-H6      PIC  X(14).
        05  DEP-H7      PIC  X(10).
        05  DEP-H8      PIC  X(10).
        05  DEP-H9      PIC  X(07).
    03  DEP-L2 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(02).
        05  DEP-EXT-ITEM.
     07  DEP-ITEM
          PIC  X(14).
     07  DEP-SL  PIC  X(01).
     07  DEP-ITM PIC  X(03).
        05  FILLER      PIC  X(01).
        05  DEP-BIN     PIC  X(06).
        05  FILLER      PIC  X(01).
        05  DEP-DESC    PIC  X(30).
        05  FILLER      PIC  X(01).
        05  DEP-QNT     PIC  Z(07)9.9999-.
        05  FILLER      PIC  X(01).
        05  DEP-COST    PIC  Z(06)9.9999.
        05  FILLER      PIC  X(01).
        05  DEP-EXT     PIC  Z(08)9.99-.
        05  FILLER      PIC  X(01).
        05  DEP-RET     PIC  Z(06)9.999.
        05  FILLER      PIC  X(01).
        05  DEP-ACT     PIC  Z9/99/9999.
        05  FILLER      PIC  X(01).
        05  DEP-GP      PIC  ZZ9.99.
        05  FILLER      PIC  X(02).
          03  MOV-LINE1 REDEFINES W02-PRN-LINE.
              05  MOV-H1      PIC  X(15).
              05  MOV-H2      PIC  X(04).
              05  MOV-H3      PIC  X(23).
              05  MOV-MONTH.
           07  MOV-H4  PIC  X(16).
           07  MOV-H5  PIC  X(08).
           07  MOV-H6  PIC  X(16).
              05  MOV-RMNTH REDEFINES MOV-MONTH.
           07  MOV-H7  PIC  X(09).
           07  MOV-H8  PIC  X(11).
           07  MOV-H9  PIC  X(14).
           07  MOV-H10 PIC  X(06).
              05  FILLER      PIC  X(02).
              05  MOV-YEAR.
           07  MOV-H11 PIC  X(20).
           07  MOV-H12 PIC  X(08).
           07  MOV-H13 PIC  X(20).
              05  MOV-RYR REDEFINES MOV-YEAR.
           07  MOV-H14 PIC  X(09).
           07  MOV-H15 PIC  X(11).
           07  MOV-H16 PIC  X(15).
           07  MOV-H17 PIC  X(07).
           07  MOV-H18 PIC  X(06).
          03  MOV-LINE2 REDEFINES W02-PRN-LINE.
              05  MOV-ITEM    PIC  X(14).
              05  FILLER      PIC  X(01).
              05  MOV-BIN     PIC  X(03).
              05  FILLER      PIC  X(01).
              05  MOV-DESC    PIC  X(22).
              05  FILLER      PIC  X(01).
              05  MOV-MQTY    PIC  Z(04)9.99.
              05  FILLER      PIC  X(01).
              05  MOV-MCOST   PIC  Z(06)9.99.
              05  FILLER      PIC  X(01).
              05  MOV-MSELL   PIC  Z(06)9.99.
              05  FILLER      PIC  X(01).
              05  MOV-MPROF   PIC  Z(05)9.99.
              05  FILLER      PIC  X(01).
              05  MOV-YQTY    PIC  Z(05)9.99.
              05  FILLER      PIC  X(01).
              05  MOV-YCOST   PIC  Z(06)9.99.
              05  FILLER      PIC  X(01).
              05  MOV-YSELL   PIC  Z(06)9.99.
              05  FILLER      PIC  X(01).
              05  MOV-YPROF   PIC  Z(06)9.99.
              05  FILLER      PIC  X(01).
              05  MOV-GP      PIC  Z(02)9.99.
          03  ORD-LINE1 REDEFINES W02-PRN-LINE.
              05  ORD-H1      PIC  X(15).
              05  ORD-H2      PIC  X(04).
              05  ORD-H3      PIC  X(23).
              05  ORD-H4      PIC  X(11).
              05  ORD-H5      PIC  X(09).
              05  ORD-MOVE.
           07  ORD-H6  PIC  X(22).
           07  ORD-H7  PIC  X(10).
           07  ORD-H8  PIC  X(22).
           07  FILLER  PIC  X(01).
           07  ORD-H9  PIC  X(15).
              05  ORD-MOV REDEFINES ORD-MOVE.
           07  ORD-H10 PIC  X(09).
           07  ORD-H11 PIC  X(09).
           07  ORD-H12 PIC  X(08).
           07  ORD-H13 PIC  X(11).
           07  ORD-H14 PIC  X(11).
           07  ORD-H15 PIC  X(07).
           07  ORD-H16 PIC  X(11).
           07  ORD-H17 PIC  X(04).
          03  ORD-LINE2 REDEFINES W02-PRN-LINE.
              05  ORD-ITM     PIC  X(14).
              05  FILLER      PIC  X(01).
              05  ORD-BIN     PIC  X(03).
              05  FILLER      PIC  X(01).
              05  ORD-DESC    PIC  X(22).
              05  FILLER      PIC  X(01).
              05  ORD-QUANT   PIC  Z(04)9.99-.
              05  ORD-REORD   PIC  Z(04)9.99.
              05  FILLER      PIC  X(01).
              05  ORD-MTH1    PIC  Z(04)9.99.
              05  FILLER      PIC  X(01).
              05  ORD-MTH2    PIC  Z(04)9.99.
              05  FILLER      PIC  X(01).
              05  ORD-MTH3    PIC  Z(04)9.99.
              05  FILLER      PIC  X(01).
              05  ORD-MAVG    PIC  Z(04)9.99.
              05  FILLER      PIC  X(01).
              05  ORD-MTD     PIC  Z(04)9.99.
              05  FILLER      PIC  X(01).
              05  ORD-YTD     PIC  Z(04)9.99.
              05  FILLER      PIC  X(01).
              05  ORD-ORDER   PIC  Z(04)9.99.
              05  FILLER      PIC  X(01).
              05  ORD-COST    PIC  Z(05)9.99.
    03  QTY-L1 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(03).
        05  QTY-H1      PIC  X(18).
        05  QTY-H2      PIC  X(35).
        05  QTY-H3      PIC  X(14).
        05  QTY-H4      PIC  X(10).
    03  QTY-L2 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(03).
        05  QTY-ITM     PIC  X(14).
        05  FILLER      PIC  X(04).
        05  QTY-DESC    PIC  X(30).
        05  FILLER      PIC  X(04).
        05  QTY-QUANT   PIC  Z(05)9.99-.
        05  FILLER      PIC  X(03).
        05  QTY-PRC.
     07  QTY-PRICE PIC Z(05)9.99.
     07  FILLER  PIC  X(01).
        05  QTY-DTE REDEFINES QTY-PRC.
     07  QTY-DATE PIC Z9/99/9999.
        05  FILLER      PIC  X(03).

       01  W05.
           03  W05-S OCCURS 300   PIC 9(04)     COMP-5.

       COPY "W05.VAT".

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

       01  W15-INPUT-AREA.
          03  W15-ACNO      PIC  X(06).
          03  W15-NAME.
               05  W15-NCHAR PIC  X(01) OCCURS 40.

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

       COPY "W70STOCK.WS".

       01  W70-KEYS.
           03  W70-BIND        PIC 9(05)     COMP-0 VALUE ZERO.

       01  W75-KEYS.
           03  W75-S         PIC  9(02) COMP-5.
           03  W75-S1        PIC  9(02) COMP-5.
           03  W75-CNO       PIC  X(05) OCCURS 18.

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
                                 PIC X(14) USING STK-CODE.

       01  S03-ITM.
          03  LINE 16 COLUMN 26 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC X(14)/X(03) FROM STK-CODE.

       01  S03-EXT.
          03  LINE 16 COLUMN 26 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC X(18) FROM STK-CODE.

       01  S04.
          03  LINE 10 COLUMN 20 FOREGROUND-COLOR Brown HIGHLIGHT
                                 VALUE "Busy loading stock departments".

       01  S05.
           02  S05A.
    03  LINE  2 COLUMN 30 FOREGROUND-COLOR Grey HIGHLIGHT
     VALUE "STOCK LIST BY SUPPLIER".
           02  S05B.
          03  LINE  6 COLUMN  8 VALUE "Supplier Code:".

       01  S06.
           02  S06A.
    03  LINE  6 COLUMN 23 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC X(06) USING W15-ACNO AUTO.

       01  S07.
          03  LINE  8 COLUMN 20 FOREGROUND-COLOR Brown HIGHLIGHT VALUE
           "Busy printing Supplier Stock list".

       01  S15.
           03  LINE 16 COLUMN 12 VALUE "Stock record:".

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

       COPY "CRDLUP.CRT".

       COPY "ERROR.CRT".

       COPY "OPT.CRT".

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
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *     **  *   *   * *   *  *  **  *   *   *     *   *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *     *  **   *     *   *  *  *  **   *   *     *  *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** *   *
      *
             PERFORM OPEN-PRINTER.
             MOVE "P"                TO WS-COMMAND.
             PERFORM AB25.
             PERFORM BE00S.
             PERFORM AB35.
             MOVE "C"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             CLOSE CRDMEM.

       AA49.
             EXIT PROGRAM.

       COPY "AA50C.LUP".
       
       COPY "AA900.ALM".

       COPY "DATE.CNV".

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

      /
       AB000              SECTION.
       AB05.
      MOVE "H"   TO W02-PRN-TYPE.
             MOVE SPACES         TO REP-DETAIL1.
            MOVE "Page:"  TO REP-PG.
            MOVE "Date:"  TO REP-DAT.
             MOVE W95-COMP       TO REP-COMP.
             ADD 1               TO WS-PAGE.
             MOVE WS-PAGE        TO REP-P-N.
             MOVE W12-TODAY      TO REP-DT.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.

       AB10.
            MOVE "Supplier:"  TO REP-HEAD2.
            MOVE W15-NAME  TO REP-CAT.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
            MOVE "S T O C K   L I S T"
                                 TO REP-COMP.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
            MOVE "Item Code"  TO DEP-H1.
            MOVE "Bin"   TO DEP-H2.
            MOVE WS-DESC-H1  TO DEP-H3.
            MOVE "Quantity"  TO DEP-H4.
            MOVE "Avrg-Price"  TO DEP-H5.
            MOVE "Extended"  TO DEP-H6.
            MOVE WS-RT-HD  TO DEP-H7.
            MOVE "Active"  TO DEP-H8.
            MOVE "GP-%"  TO DEP-H9.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE "D"   TO W02-PRN-TYPE.

       AB20.
             EXIT.
      *
      *    ****   1 7 C P I ( C O N D E N S E D   P R I N T )
      *
       AB25.
    IF NOT(WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE 3   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
        MOVE "D"   TO W02-PRN-TYPE
              MOVE SPACES  TO REP-DETAIL1.

       AB30.
             EXIT.
      *
      *    ****   1 O C P I ( N O R M A L P R I N T )
      *
       AB35.
    IF NOT(WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE 1   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
        MOVE "D"   TO W02-PRN-TYPE
              MOVE SPACES  TO REP-DETAIL1.
      MOVE ZERO   TO WS-PAGE.

       AB40.
             EXIT.
      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
        AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "CREDIT.RD".

       COPY "CRDMEM.RD".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "STKMEM.RD".

       COPY "STOCK.RD".

       COPY "SPARTS.RD".

       COPY "STOCK.1ST".

      *COPY "CREDIT.LUP".

       COPY "APAC.HLP".

       BE00S        SECTION.
       BE30.
             PERFORM ERASE-SCREEN.
             DISPLAY S05.
             MOVE SPACES             TO W15-ACNO.

       BE35.
             DISPLAY S55.
             ACCEPT S06A.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  MOVE SPACES TO W15-ACNO
                               GO TO BE999
                 WHEN F1-KEY   PERFORM HELP-ROUTINE
                 WHEN F2-KEY   PERFORM AA50
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO BE35
               END-EVALUATE
               DISPLAY S06A
               IF W15-ACNO = SPACES
                  GO TO BE35.
             PERFORM CLEAR-L50.
           IF W15-ACNO = SPACES
               GO TO BE999.
             MOVE 5                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W15-ACNO BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             MOVE W15-ACNO           TO P-NUMBER.
             PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
               MOVE 1                TO WS-ERROR
           ELSE
           IF P-NUMBER NOT = W15-ACNO
               MOVE 2                TO WS-ERROR
           ELSE
               MOVE 0                TO WS-ERROR.
           IF NOT(WS-ERROR = ZERO)
               MOVE "No Record"      TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BE35.

       BE40.
             PERFORM CLEAR-L50.
             MOVE P-NAME             TO W15-NAME.
             DISPLAY W15-NAME AT 0630 WITH HIGHLIGHT BACKGROUND-COLOR Magenta.

       BE45.
             MOVE "'C'ontinue or 'R'ekey" TO WS-OPT-MES.
             MOVE SPACE              TO WS-OPTION.
             MOVE "9"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "R"
               GO TO BE30.
             MOVE ZERO               TO WS-PAGE WS-TCOST WS-TSELL.

       BE50.
             MOVE "'D'escription or 'C'ode sequence  [C]" TO WS-OPT-MES.
             MOVE "C"                TO WS-OPTION.
             MOVE "h"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = X"EC"
               GO TO BE30.
             MOVE WS-OPTION          TO WS-SEQ.

       BE55.
             DISPLAY S06.
             DISPLAY S15.

       BE75.
            PERFORM AB05 THRU AB10.
             PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              GO TO BE30.
    IF WS-SEQ = "C"
        MOVE "!"   TO STK-CODE
              PERFORM START-AT-STOCK-CODE THRU READ-STOCK-EXIT
    ELSE
        MOVE "!"   TO STK-DESC
        PERFORM START-AT-STOCK-DESC THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
        MOVE 99   TO WS-ADVANCE
        MOVE "E"   TO WS-COMMAND
        PERFORM CALL-PRINTUTL
        MOVE "P"   TO WS-COMMAND
              GO TO BE30.

       BE80.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
    IF (WS-F-ERROR = 22)
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              MOVE "STOCK TOTAL -POSITIVE QUANTITY"
     TO DEP-DESC
              MOVE WS-TCOST  TO DEP-EXT
        PERFORM CALL-PRINTUTL
              MOVE SPACES  TO REP-DETAIL1
        IF NOT(WS-TSELL = ZERO)
           MOVE "STOCK TOTAL -NEGATIVE QUANTITY"
     TO DEP-DESC
           MOVE WS-TSELL TO DEP-EXT
        END-IF
        MOVE 99   TO WS-ADVANCE
        MOVE "E"   TO WS-COMMAND
        PERFORM CALL-PRINTUTL
        MOVE "P"   TO WS-COMMAND
              GO TO BE30.
    IF STK-IND = 4
        GO TO BE80.
    IF NOT(W15-ACNO = STK-SUPP OR STK-PSUPP)
              GO TO BE80.
    IF NOT(WS-PRINTER = "S")
        IF WS-USE-ITM = "Y"
     DISPLAY S03-ITM
        ELSE
        IF WS-EXT-STK = "Y"
     DISPLAY S03-EXT
        ELSE
           DISPLAY S03.
    IF WS-USE-ITM = "Y"
        MOVE STK-ITEM  TO DEP-ITEM
        MOVE "/"   TO DEP-SL
        MOVE STK-ITM  TO DEP-ITM
    ELSE
              MOVE STK-CODE  TO DEP-EXT-ITEM.
            MOVE STK-BIN  TO DEP-BIN.
             MOVE STK-DESC       TO DEP-DESC.
             MOVE STK-QUANT      TO DEP-QNT.
            MOVE STK-SELL  TO DEP-RET.
      *    ****    A V E R A G E   C O S T
    IF STK-QUANT < 0.001
        MOVE STK-COST  TO STK-AVGE.
    IF STK-IND < 2
              IF STK-AVGE = ZERO
           MOVE STK-COST TO STK-AVGE.
            MOVE STK-AVGE  TO DEP-COST.
      COMPUTE WS-EXTEND ROUNDED = (STK-AVGE * STK-QUANT).
      MOVE WS-EXTEND  TO DEP-EXT.
    IF WS-EXTEND < ZERO
        ADD WS-EXTEND  TO WS-TSELL
    ELSE
        ADD WS-EXTEND  TO WS-TCOST.
            MOVE STK-MTDV  TO WS-SSELL.
            MOVE STK-MTDC  TO WS-SCOST.
           IF WS-SSELL = ZERO OR WS-SCOST = ZERO
              MOVE STK-COST  TO WS-SCOST
               COMPUTE WS-SSELL = (STK-SELL + STK-CASH) / 2.000 + .005.
             COMPUTE DEP-GP ROUNDED = ((WS-SSELL - WS-SCOST)
                                      * 100.000) / WS-SSELL.
             MOVE STK-ACT            TO W22-DTE2.
             PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT.
      MOVE W22-DTE3  TO DEP-ACT.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM AB05 THRU AB10.
            MOVE SPACES  TO REP-DETAIL1.
             PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              GO TO BE30.

       BE82.
             GO TO BE80.

       BE999.
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
       WS-STATUS DELIMITED SIZE INTO WS-ERR-MES.
      PERFORM ERROR-LENGTH THRU ERROR-EXIT.
            STOP RUN.

       ZA00-READ-APACFIDS-EXIT.
      EXIT.

       ZA00A-CONTINUE.
      MOVE AFID-PATH  TO W02-PARAM.
      MOVE "CRDMEM"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-CRDMEM.
      MOVE "CREDIT"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-CREDIT.
      MOVE "DEPART"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-DEPART.
      MOVE "NETWORK"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-NETWORK.
      MOVE "SPARTS"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-SPARTS.
      MOVE "STKMEM"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-STKMEM.
      MOVE "STOCKF"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-STOCKF.
            MOVE 3   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-USE-ITM  TO WS-USE-ITM.
      MOVE PAR-EXT-STK  TO WS-EXT-STK.
            OPEN I-O CRDMEM.
    IF WS-STATUS = "05"
        CLOSE CRDMEM
        OPEN OUTPUT CRDMEM
        CLOSE CRDMEM
        OPEN I-O CRDMEM.
          IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
        IF IDX-CORRUPT OR
   (WS-STATUS = "39")
     MOVE 14  TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
              MOVE 14   TO WS-F-ERROR
               PERFORM OPEN-ERROR.

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
            MOVE 3   TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
            MOVE STK-WEEK  TO WS-WEEK.
             MOVE STK-LABCDE     TO WS-LABCDE.
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

       ZA05.
            DISPLAY WS-BLNK78 AT 1402 WITH FOREGROUND-COLOR Cyan.
             GO TO ZA999-EXIT.

       COPY "ZA49.PRO".

       ZA50.
    IF WS-F-ERROR = 14
        MOVE "CREDIT MEMO"
     TO WS-FILE.
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
               WHEN 3     MOVE W02-CREDIT   TO WS-FILE
                          MOVE P-NUMBER     TO WS-KEYX
               WHEN 7     MOVE W02-DEPART   TO WS-FILE
                          MOVE DPT-CODE     TO WS-KEYX
               WHEN 14    MOVE W02-CRDMEM   TO WS-FILE
                          MOVE CME-KEY      TO WS-KEYX
               WHEN 15    MOVE WS-PARID     TO WS-FILE
                          MOVE WS-PARKEY    TO WS-KEY
               WHEN 21    MOVE W02-SPARTS   TO WS-FILE
                          MOVE PRT-ITEM     TO WS-KEYX
               WHEN 22    MOVE W02-STOCKF   TO WS-FILE
                          MOVE STK-CODE     TO WS-KEYX
               WHEN 25    MOVE W02-STKMEM   TO WS-FILE
                          MOVE SME-KEY      TO WS-KEYX
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STP053"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
