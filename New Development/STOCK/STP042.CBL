      $set LINKCOUNT"512" GNT"STP042.GNT"
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
      *    ******   ********  *******     ****        ***    ******    *
      *   **    **     **     **    **   **  **      ****   **    **   *
      *   **           **     **    **  **    **    ** **        **    *
      *    ******      **     ******    **    **   **  **      **      *
      *         **     **     **        **    **  ********   **        *
      *   **    **     **     **         **  **        **   **         *
      *    ******      **     **          ****        ****  ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      * S T O C K   D E P A R M E N T A L   R E P O R T   P R O G R A M*
      *                                                                *
      *     SPECIAL SPARES VERSION 1989 - WEEKLY TOTALS & X-REF        *
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
       PROGRAM-ID.     STP042.
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

       COPY "STOCK.FDE".

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   **
      *         **    *    **   **    **   **   **    **  **
      *         **   ***   **   **    **   ******     *****
      *          ** ** ** **    **    **   **   **    **  **
      *           ***   ***     **    **   **    **   **   **
      *            *     *       ******    **    **   **    **
      *
       WORKING-STORAGE SECTION.
       77  WS-CHECK                PIC  X(18)           VALUE "aeWlimemnomLalismJ".
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
       77  WS-CAT                  PIC  9(04)    COMP-5.
       77  WS-SCAT                 PIC  X(04).
       77  WS-PARKEY               PIC  9(04)    COMP-5.
       77  WS-NETKEY               PIC  9(04)    COMP-5.
       77  WS-PAGE                 PIC  9(04)    COMP-5.
       77  WS-PRC-IND              PIC  X(01).
       77  WS-WS                   PIC  X(01)           VALUE "N".
       77  WS-MEMO                 PIC  X(01)           VALUE "N".
       77  WS-PRT                  PIC  X(01)           VALUE "Y".
       77  WS-BIN                  PIC  X(01).
       77  WS-ETYPE                PIC  X(01).
       77  WS-OPTION               PIC  X(01).
       77  WS-ZLT                  PIC  X(01)           VALUE "Y".
       77  WS-ZQT                  PIC  X(01)           VALUE "Y".
       77  WS-ROC                  PIC  X(01)           VALUE "R".
       77  WS-FORMAT               PIC  9(01)           VALUE 1.
       77  WS-ERROR                PIC  9(01).
       77  WS-USE-ITM              PIC  X(01).
       77  WS-EXT-STK              PIC  X(01).
       77  WS-ORD                  PIC S9(05)    COMP-3.
       77  WS-TOTAL                PIC S9(09)V99 COMP-3.
       77  WS-VALUE                PIC S9(09)V99 COMP-3.
       77  WS-QUANT                PIC S9(09)V999 COMP-3.
       77  WS-CCOST                PIC S9(09)V999 COMP-3.
       77  WS-CSELL                PIC S9(09)V999 COMP-3.
       77  WS-SCOST                PIC S9(09)V99 COMP-3.
       77  WS-SSELL                PIC S9(09)V99 COMP-3.
       77  WS-TQUANT               PIC S9(09)V99 COMP-3.
       77  WS-TCOST                PIC S9(09)V99 COMP-3.
       77  WS-TSELL                PIC S9(09)V99 COMP-3.
       77  WS-DEPT                 PIC  X(04).
       77  WS-WEEK                 PIC  9(01).
       77  WS-LABCDE               PIC  X(08).
       77  WS-DESC-H1              PIC  X(14)           VALUE "Description".
       77  WS-DESC-H2              PIC  X(14)           VALUE "Description 2".
       77  WS-RT-HD                PIC  X(10)           VALUE "-Retail   ".
       77  WS-WS-HD                PIC  X(10)           VALUE "-Wholesale".
       77  WS-CS-HD                PIC  X(10)           VALUE "-Cash sale".
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STP042".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY DEPARTMENTAL REPORT".
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

           03  R-SL1                               REDEFINES W02-PRN-LINE.
               05  R-SLA             PIC  X(78).
               05  FILLER            PIC  X(58).
           03  R-SL2                               REDEFINES W02-PRN-LINE.
               05  FILLER            PIC  X(58).
               05  R-SLB             PIC  X(78).
           03  REP-LINE1                           REDEFINES W02-PRN-LINE.
               05  REP-DETAIL1       PIC  X(140).
           03  REP-LINE2                           REDEFINES W02-PRN-LINE.
               05  REP-DETAIL2       PIC  X(80).
           03  REP-LINE3                           REDEFINES W02-PRN-LINE.
               05  REP-DATE          PIC  X(06).
               05  REP-DTE           PIC  Z9/99/9999.
               05  FILLER            PIC  X(03).
               05  REP-PRC-HD.
                   07  FILLER        PIC  X(01).
                   07  REP-COMPANY   PIC  X(40).
                   07  FILLER        PIC  X(02).
               05  FILLER            PIC  X(08).
               05  REP-PAGE          PIC  X(06).
               05  REP-P-NO          PIC  Z(03)9.
           03  REP-LINE4                           REDEFINES W02-PRN-LINE.
               05  FILLER            PIC  X(30).
               05  REP-HEAD1         PIC  X(20).
               05  FILLER            PIC  X(30).
           03  REP-LINE5                           REDEFINES W02-PRN-LINE.
               05  FILLER            PIC  X(09).
               05  REP-DSC           PIC  X(30).
               05  FILLER            PIC  X(93).
           03  REP-LINE6                           REDEFINES W02-PRN-LINE.
               05  REP-HEAD2         PIC  X(10).
               05  REP-CNO           PIC  9(04).
               05  REP-CATNO         PIC  X(04)    REDEFINES REP-CNO.
               05  FILLER            PIC  X(01).
               05  REP-CAT           PIC  X(30).
               05  FILLER            PIC  X(35).
           03  REP-LINE9                           REDEFINES W02-PRN-LINE.
               05  REP-PG            PIC  X(06).
               05  REP-P-N           PIC  Z(03)9.
               05  FILLER            PIC  X(22).
               05  REP-COMP          PIC  X(40).
               05  REP-REPORT        PIC  X(34).
               05  FILLER            PIC  X(10).
               05  REP-DAT           PIC  X(06).
               05  REP-DT            PIC  Z9/99/9999.
           03  REP-LINE9A                           REDEFINES W02-PRN-LINE.
               05  REP-HEAD6         PIC  X(13).
               05  REP-HEAD6A        PIC  X(04).
               05  REP-HEAD7         PIC  X(25).
               05  REP-HEAD8         PIC  X(12).
               05  REP-HEAD9         PIC  X(08).
               05  REP-HEAD10        PIC  X(08).
               05  REP-HEAD11        PIC  X(09).
               05  REP-HEAD12        PIC  X(09).
               05  REP-HEAD14        PIC  X(07).
               05  REP-HEAD15        PIC  X(10).
               05  REP-HEAD16        PIC  X(07).
               05  REP-HEAD17        PIC  X(08).
               05  REP-HEAD18        PIC  X(12).
           03  REP-LINE9B                          REDEFINES W02-PRN-LINE.
               05  S-H1              PIC  X(15).
               05  S-H2              PIC  X(28).
               05  S-L1              PIC  X(01).
               05  S-H3              PIC  X(03).
               05  S-L2              PIC  X(01).
               05  S-H4              PIC  X(07).
               05  S-L3              PIC  X(01).
               05  S-H5              PIC  X(07).
               05  S-L4              PIC  X(01).
               05  S-H6              PIC  X(07).
               05  S-L5              PIC  X(01).
               05  S-H7              PIC  X(07).
               05  S-L6              PIC  X(01).
           03  REP-LINE9C                          REDEFINES W02-PRN-LINE.
               05  S-CDE             PIC  X(14).
               05  FILLER            PIC  X(01).
               05  S-DSC             PIC  X(28).
               05  FILLER            PIC  X(01).
               05  S-BN              PIC  X(03).
               05  FILLER            PIC  X(01).
               05  S-QNT             PIC  Z(04)9.9.
               05  S-QNTN            PIC  -(04)9.9 REDEFINES S-QNT.
               05  FILLER            PIC  X(25).
           03  REP-LINE11                          REDEFINES W02-PRN-LINE.
               05  FILLER            PIC  X(13).
               05  REP-HEAD13        PIC  X(26).
               05  FILLER            PIC  X(02).
               05  REP-DCOST         PIC  Z(08)9.99.
               05  FILLER            PIC  X(02).
               05  REP-DSELL         PIC  Z(08)9.99.
               05  FILLER            PIC  X(13).
           03  PRC-L1                              REDEFINES W02-PRN-LINE.
               05  PRC-H1            PIC  X(15).
               05  PRC-H2            PIC  X(06).
               05  PRC-H3            PIC  X(09).
               05  PRC-H4            PIC  X(32).
               05  PRC-H5.
                   07  FILLER        PIC  X(01).
                   07  PRC-H5A       PIC  X(12).
               05  PRC-H6.
                   07  FILLER        PIC  X(05).
                   07  PRC-H6A       PIC  X(04).
               05  PRC-H7.
                   07  FILLER        PIC  X(06).
                   07  PRC-H7A       PIC  X(06).
               05  FILLER            PIC  X(02).
               05  PRC-H8.
                   07  FILLER        PIC  X(01).
                   07  PRC-H8A       PIC  X(12).
               05  PRC-H9.
                   07  FILLER        PIC  X(05).
                   07  PRC-H9A       PIC  X(04).
               05  PRC-H10.
                   07  FILLER        PIC  X(06).
                   07  PRC-H10A      PIC X(06).
           03  PRC-L2                              REDEFINES W02-PRN-LINE.
               05  PRC-ITM           PIC  X(14).
               05  FILLER            PIC  X(01).
               05  PRC-BIN           PIC  X(03).
               05  FILLER            PIC  X(01).
               05  PRC-CCODE         PIC  Z(06)9.99.
               05  FILLER            PIC  X(01).
               05  PRC-DESC          PIC  X(30).
               05  FILLER            PIC  X(01).
               05  PRC-E-WSALE       PIC  Z(07)9.99.
               05  FILLER            PIC  X(01).
               05  PRC-E-CASH        PIC  Z(07)9.99.
               05  FILLER            PIC  X(01).
               05  PRC-E-RET         PIC  Z(07)9.99.
               05  FILLER            PIC  X(01).
               05  PRC-I-WSALE       PIC  Z(07)9.99.
               05  FILLER            PIC  X(01).
               05  PRC-I-CASH        PIC  Z(07)9.99.
               05  FILLER            PIC  X(01).
               05  PRC-I-RET         PIC  Z(07)9.99.
           03  PRI-L1                              REDEFINES W02-PRN-LINE.
               05  PRI-H1            PIC  X(15).
               05  PRI-H2            PIC  X(06).
               05  PRI-H3            PIC  X(08).
               05  PRI-H4            PIC  X(33).
               05  PRI-H5            PIC  X(15).
               05  PRI-H6            PIC  X(09).
               05  PRI-H7            PIC  X(09).
           03  PRC-L2                              REDEFINES W02-PRN-LINE.
               05  PRI-ITM           PIC  X(14).
               05  FILLER            PIC  X(01).
               05  PRI-BIN           PIC  X(03).
               05  FILLER            PIC  X(01).
               05  PRI-CCODE         PIC  X(09).
               05  FILLER            PIC  X(01).
               05  PRI-DESC          PIC  X(31).
               05  FILLER            PIC  X(01).
               05  PRI-EXCL          PIC  Z(07)9.99.
               05  FILLER            PIC  X(01).
               05  PRI-VAT           PIC  Z(07)9.99.
               05  FILLER            PIC  X(01).
               05  PRI-INCL          PIC  Z(07)9.99.
           03  PRI-L3                              REDEFINES W02-PRN-LINE.
               05  FILLER            PIC  X(42).
               05  PRI-MEMO          PIC  X(60).
           03  DEP-L1                              REDEFINES W02-PRN-LINE.
               05  DEP-H1            PIC  X(19).
               05  DEP-H3            PIC  X(32).
               05  DEP-H2            PIC  X(07).
               05  DEP-H4            PIC  X(10).
               05  DEP-H5            PIC  X(14).
               05  DEP-H6            PIC  X(11).
               05  DEP-H7            PIC  X(08).
               05  DEP-H8.
                   07  FILLER        PIC  X(01).
                   07  DEP-H8A       PIC  X(06).
               05  DEP-H9.
                   07  FILLER        PIC  X(02).
                   07  DEP-H9A       PIC  X(04).
               05  DEP-H10.
                   07  DEP-H10A      PIC  X(08).
               05  FILLER            PIC  X(02).
               05  DEP-H11           PIC  X(08).
      *        05  DEP-H12           PIC  X(05).
           03  DEP-L2                              REDEFINES W02-PRN-LINE.
               05  DEP-EXT-ITEM.
                   07  DEP-ITEM      PIC  X(14).
                   07  DEP-SL        PIC  X(01).
                   07  DEP-ITM       PIC  X(03).
               05  FILLER            PIC  X(01).
               05  DEP-DESC          PIC  X(28).
               05  FILLER            PIC  X(01).
               05  DEP-BIN           PIC  X(06).
               05  FILLER            PIC  X(01).
               05  DEP-QNT           PIC  Z(06)9.999-.
               05  DEP-COST          PIC  Z(06)9.999.
               05  FILLER            PIC  X(01).
               05  DEP-AVG           PIC  Z(05)9.999.
               05  FILLER            PIC  X(01).
               05  DEP-RET           PIC  Z(06)9.99.
               05  FILLER            PIC  X(01).
               05  DEP-MWS           PIC  ZZ9.99.
               05  FILLER            PIC  X(01).
               05  DEP-MCSH          PIC  ZZ9.99.
               05  FILLER            PIC  X(01).
               05  DEP-MRET          PIC  ZZ9.99.
               05  FILLER            PIC  X(01).
               05  DEP-ACT           PIC  Z9/99/9999 BLANK WHEN ZERO.

           03  DEP-L3                              REDEFINES W02-PRN-LINE.
               05  FILLER            PIC  X(19).
               05  DEP-T-HEAD.
                   07  FILLER        PIC  X(18).
                   07  DEP-T-LEDG          PIC  X(04).
                   07  FILLER        PIC  X(08).
      *        05  FILLER            PIC  X(11).
               05  DEP-T-H1          PIC  X(06).
               05  DEP-T-L1.
                   07  DEP-TQUANT    PIC  Z(08)9.999-.
               05  FILLER            PIC  X(01).
               05  DEP-T-H2          PIC  X(05).
               05  DEP-T-L2.
                   07  DEP-TCOST     PIC  Z(08)9.999-.
               05  FILLER            PIC  X(01).
               05  DEP-T-H3          PIC  X(08).
               05  DEP-T-L3.
                   07  DEP-TSELL     PIC  Z(08)9.999-.
      *        05  FILLER            PIC  X(10).
               05  FILLER            PIC  X(20).

           03  DP2-L1                              REDEFINES W02-PRN-LINE.
               05  DP2-H1            PIC  X(19).
               05  DP2-H2            PIC  X(30).
               05  DP2-H3            PIC  X(07).
               05  DP2-H4            PIC  X(11).
               05  DP2-H5            PIC  X(10).
               05  DP2-H6            PIC  X(12).
               05  DP2-H7            PIC  X(17).
               05  DP2-H8            PIC  X(17).
               05  DP2-H9            PIC  X(08).

           03  DP2-L2                              REDEFINES W02-PRN-LINE.
               05  DP2-EXT-ITEM.
                   07  DP2-ITEM      PIC  X(14).
                   07  DP2-SL        PIC  X(01).
                   07  DP2-ITM       PIC  X(03).
               05  FILLER            PIC  X(01).
               05  DP2-DESC          PIC  X(28).
               05  FILLER            PIC  X(01).
               05  DP2-QNT           PIC  Z(06)9-.
               05  DP2-COST          PIC  Z(05)9.99.
               05  FILLER            PIC  X(01).
               05  DP2-AVG           PIC  Z(05)9.99.
               05  FILLER            PIC  X(01).
               05  DP2-RET           PIC  Z(06)9.99.
               05  FILLER            PIC  X(01).
               05  DP2-MRKV          PIC  Z(05)9.99.
               05  FILLER            PIC  X(01).
               05  DP2-MRKP          PIC  ZZ9.99.
               05  FILLER            PIC  X(01).
               05  DP2-SYSV          PIC  Z(05)9.99.
               05  FILLER            PIC  X(01).
               05  DP2-SYSP          PIC  ZZ9.99.
               05  FILLER            PIC  X(01).
               05  DP2-ACT           PIC  Z9/99/9999 BLANK WHEN ZERO.

       01  W05.
           03  W05-S OCCURS 300      PIC 9(04)     COMP-5.

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
           03  W30-CODE              PIC  X(01)    OCCURS 10.
       01  W35-COST.
           03  W35-CST               PIC  9(08)V99.
           03  W35-WHOLE             PIC  9(10)    REDEFINES W35-CST.
           03  W35-CPRINT.
               05  W35-CEDIT         PIC  ZZZZZZ9999.
           03  W35-EOC                             REDEFINES W35-CPRINT.
               05  W35-CHAR          PIC  X(01)    OCCURS 10.
           03  W35-EON                             REDEFINES W35-CPRINT.
               05  W35-NUM           PIC  9(01)    OCCURS 10.
           03  W35-CPRN                            REDEFINES W35-CPRINT.
               05  FILLER            PIC  X(01).
               05  W35-CSTPRN        PIC  X(09).
           03  W35-S1                PIC  9(02)    COMP-5.
           03  W35-S2                PIC  9(02)    COMP-5.

       01  W40-DATES.
           03  W40-DATE1             PIC  9(08).
           03  W40-DTE1                            REDEFINES W40-DATE1.
               05  W40-C1            PIC  9(02).
               05  W40-Y1            PIC  9(02).
               05  W40-M1            PIC  9(02).
               05  W40-D1            PIC  9(02).
           03  W40-DATE2             PIC  9(08).
           03  W40-DTE2                            REDEFINES W40-DATE2.
               05  W40-C2            PIC  9(02).
               05  W40-Y2            PIC  9(02).
               05  W40-M2            PIC  9(02).
               05  W40-D2            PIC  9(02).

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       COPY "W70STOCK.WS".

       01  W70-KEYS.
           03  W70-BIND              PIC  9(05)    COMP-0 VALUE ZERO.

       COPY "W75.STK".

       01  W95-COMP                  PIC  X(40).
       01  W100-EDIT.
           03  W100-V2.
               05  W100-S6V2         PIC Z(05)9.99-.
           03  W100-V3                             REDEFINES W100-V2.
               05  W100-PRICE        PIC X(09).
               05  FILLER            PIC X(01).
           03  W100-V5                             REDEFINES W100-V2.
               05  W100-QNT          PIC Z(05)9.99-.

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
      *   ******    ******   *******   ********  ********  **     **
      *  **    **  **    **  **    **  **        **        ***    **
      *  **        **        **   **   **        **        ** *   **
      *   ******   **        ******    *****     *****     **  *  **
      *        **  **        **   **   **        **        **   * **
      *  **    **  **    **  **    **  **        **        **    ***
      *   ******    ******   **    **  ********  ********  **     **
      *
       SCREEN SECTION.

       01  S03.
          03  LINE 16 COLUMN 26 PIC X(14) USING STK-CODE                  FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.

       01  S04.
          03  LINE 10 COLUMN 20 VALUE "Busy loading stock departments"    FOREGROUND-COLOR Brown HIGHLIGHT.

       01  S07.
          03  LINE 10 COLUMN 20 VALUE "Busy printing departmental report" FOREGROUND-COLOR Brown HIGHLIGHT.

       01  S08.
          03  LINE  8 COLUMN 20 VALUE "Select department".

       01  S09.
          03  LINE 25 COLUMN 20 VALUE "No details for department - ENTER" BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.

       01  S15.
           03  LINE 16 COLUMN 12 VALUE "Stock record:".

       COPY "ERROR.CRT".

       COPY "COPT.CRT".

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
           IF LS0-STLEV < 3
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
            PERFORM ZA000-INIT.
            PERFORM BE000.
           IF WS-ERROR = 9
               MOVE "No details in Department file"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO AA49.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *     **  *   *   * *   *  *  **  *   *   *     *   *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *     *  **   *     *   *  *  *  **   *   *     *   *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** *   *
      *
            PERFORM OPEN-PRINTER.
            MOVE "P"                 TO WS-COMMAND.
            PERFORM BE00S.
            MOVE "C"                 TO WS-COMMAND.
            PERFORM CALL-PRINTUTL.

       AA49.
             EXIT PROGRAM.

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

       COPY "DATE.CNV".

       COPY "ABORT.PRN".

       COPY "PRINTUTL.AP3".

      /
       AB000              SECTION.
       AB15.
             MOVE "H"                TO W02-PRN-TYPE.
             MOVE SPACES             TO REP-DETAIL1.
             MOVE "Page:"            TO REP-PG.
             MOVE "Date:"            TO REP-DAT.
             MOVE W95-COMP           TO REP-COMP.
             ADD 1                   TO WS-PAGE.
             MOVE WS-PAGE            TO REP-P-N.
             MOVE W12-TODAY          TO REP-DT.
             MOVE "STOCK BY DEPARTMENT"  TO REP-REPORT.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO REP-DETAIL1.
             MOVE "Category:"        TO REP-HEAD2.
             MOVE DPT-CODE           TO REP-CATNO.
             MOVE DPT-DESC           TO REP-CAT.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO REP-DETAIL1.
           IF WS-FORMAT = 1
               MOVE "Item Code"      TO DEP-H1
               MOVE "Bin"            TO DEP-H2
               MOVE WS-DESC-H1       TO DEP-H3
               MOVE "Quantity"       TO DEP-H4
               MOVE "Cost-Price"     TO DEP-H5
               MOVE "Average"        TO DEP-H6
               IF WS-ROC = "C"
                   MOVE "T-Avrg"     TO DEP-H7
               ELSE
                   MOVE WS-RT-HD     TO DEP-H7
               END-IF
               MOVE ALL "_"          TO DEP-H8 DEP-H10
               MOVE "Markup"         TO DEP-H9
               MOVE "Active"         TO DEP-H11
           ELSE
               MOVE "Item Code"      TO DP2-H1
               MOVE WS-DESC-H1       TO DP2-H2
               MOVE "Quant."         TO DP2-H3
               MOVE "Cst-Price"      TO DP2-H4
               MOVE "Average"        TO DP2-H5
               MOVE "Selling"        TO DP2-H6
               MOVE "Actual Markup"  TO DP2-H7
               MOVE "System Markup"  TO DP2-H8
               MOVE "Active"         TO DP2-H9.
      *      MOVE "GP-%"  TO DEP-H12.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO REP-DETAIL1.
           IF WS-FORMAT = 1
               MOVE WS-WS-HD         TO DEP-H8
               MOVE WS-CS-HD         TO DEP-H9
               MOVE WS-RT-HD         TO DEP-H10A
           ELSE
               MOVE "Value     %"    TO DP2-H7 DP2-H8.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO REP-DETAIL1.
             MOVE "D"                TO W02-PRN-TYPE.

       AB20.
             EXIT.
      *
      *    ****   1 7 C P I ( C O N D E N S E D   P R I N T )
      *
       AB25.
           IF NOT (WS-PRINTER = "S")
               MOVE 0                TO WS-ADVANCE
               MOVE 3                TO W02-PRN-TYPE
               PERFORM CALL-PRINTUTL
               MOVE "D"              TO W02-PRN-TYPE.
             MOVE SPACES             TO REP-DETAIL1.

       AB30.
             EXIT.
      *
      *    ****   1 O C P I ( N O R M A L P R I N T )
      *
       AB35.
           IF NOT(WS-PRINTER = "S")
               MOVE 0                TO WS-ADVANCE
               MOVE 1                TO W02-PRN-TYPE
               PERFORM CALL-PRINTUTL
               MOVE "D"              TO W02-PRN-TYPE
               MOVE SPACES           TO REP-DETAIL1.
             MOVE ZERO               TO WS-PAGE.

       AB40.
             EXIT.
      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "STOCK.RD".

       COPY "SPARTS.RD".

       COPY "STOCK.1ST".

      /
       BE000  SECTION.
       BE00.
             MOVE "!"                TO DPT-CODE.
             PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.
           IF WS-F-ERROR = 7
               MOVE 9                TO WS-ERROR
           ELSE
               MOVE ZERO             TO WS-ERROR.

       BE09.
      EXIT.

      /
       BE00S            SECTION.
       BE00.
             PERFORM ERASE-SCREEN.
             DISPLAY "DEPARTMENTAL REPORT" AT 0231 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             PERFORM SAVE-SCREEN-2.
             DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0606 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black
                                                                                       "¿"        WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "³    "                                                              AT 0706 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black
                          "S E L E C T   R E P O R T   F O R M A T   R E Q U I R E D    ³"        WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "³                                                                 " AT 0806 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black
                                                                                       "³"        WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "³    "                                                              AT 0906 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black
                          "Format 1,2    "                                                        WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Magenta
                                        "                                               ³"        WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "³                                                                 " AT 1006 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black
                                                                                       "³"        WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "³ All formats print: Item Code,Description & date last active.    " AT 1106 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black
                                                                                       "³"        WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "³ 1 = Bin,Quantity (3dec),Cost,Average,Selling,Markup 1,2 & 3.    " AT 1206 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black
                                                                                       "³"        WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "³ 2 = Quant (No dec),Cost,Average,Selling,Markup Actual - Value & " AT 1306 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black
                                                                                       "³"        WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "³     percent,System Markup - Value & percent.                    " AT 1406 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black
                                                                                       "³"        WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "³                                                                 " AT 1506 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black
                                                                                       "³"        WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "³ Note - Additional formats may be added later.                   " AT 1606 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black
                                                                                       "³"        WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "À"                                                                  AT 1706 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black
                      "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"        WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
             MOVE 7                  TO SHADE-ROW.
             MOVE 8                  TO SHADE-COL.
             MOVE 65                 TO SHADE-WIDTH.
             MOVE 11                 TO SHADE-LINES.
             PERFORM SCREEN-SHADOW.

       BE02.
             ACCEPT WS-FORMAT AT 0925 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO.
           IF WS-FORMAT < 1 OR > 2
               GO TO BE02.
             PERFORM RESTORE-SCREEN-2.
             MOVE ZERO               TO WS-TCOST WS-TSELL WS-TQUANT.
             DISPLAY "Print stock with Lead Time = 0.00" AT 0404.

       BE05.
             ACCEPT WS-ZLT AT 0439 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO.
             CALL "CBL_TOUPPER" USING WS-ZLT BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-ZLT = "Y" OR "N")
               GO TO BE05.
             DISPLAY "Print stock with zero quantity" AT 0604.

       BE10.
             ACCEPT WS-ZQT AT 0635 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO.
             CALL "CBL_TOUPPER" USING WS-ZQT BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-ZQT = "Y" OR "N")
               GO TO BE10.
           IF WS-FORMAT = 2
               MOVE "R"              TO WS-ROC
               GO TO BE50.
             DISPLAY "Print "                           AT 0804
                           "R"                                  WITH FOREGROUND-COLOR Brown HIGHLIGHT
                            "etail price or total "
                                                 "C"            WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                                  "ost".

       BE15.
             ACCEPT WS-ROC AT 0837 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO.
             CALL "CBL_TOUPPER" USING WS-ROC BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT (WS-ROC = "R" OR "C")
               GO TO BE15.

       BE50.
             DISPLAY "A"                          AT 1004 WITH FOREGROUND-COLOR Brown HIGHLIGHT
                      "ll or "
                            "S"                           WITH FOREGROUND-COLOR Brown HIGHLIGHT
                             "elected Departments".
             MOVE SPACE              TO WS-OPTION.

       BE55.
             ACCEPT WS-OPTION AT 1033 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT (WS-OPTION = "A" OR "S")
               GO TO BE55.
           IF WS-OPTION = "S"
               GO TO BE110.

       BE85.
             DISPLAY S07.
             MOVE "'C'ontinue or 'E'xit  [ ]"  TO WS-ERR-MES.
             MOVE SPACE              TO WS-OPTION.

       BE86.
             PERFORM OPT-MESSAGE.
           IF NOT(WS-OPTION = "E" OR "C")
               GO TO BE85.
           IF WS-OPTION = "E"
               GO TO BE999.
             DISPLAY S15.
             PERFORM AB25.

       BE90.
            PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
           IF WS-F-ERROR = 0
               IF (DPT-CODE = "XXXX" OR "YYYY") OR (DPT-SCDE = "VAT")
                   GO TO BE90
               ELSE
                   GO TO BE95.

       BE92.
           IF WS-OPTION = "X"
               GO TO BE999.
             MOVE ALL "Ä"            TO DEP-T-L1 DEP-T-L2 DEP-T-L3.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO REP-DETAIL1.
             MOVE "**** STOCK TOTAL ****" TO DEP-T-HEAD.
             MOVE WS-TQUANT          TO DEP-TQUANT.
             MOVE WS-TCOST           TO DEP-TCOST.
             MOVE WS-TSELL           TO DEP-TSELL.
             MOVE 99                 TO WS-ADVANCE.
             MOVE "E"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             MOVE "P"                TO WS-COMMAND.
             MOVE SPACES             TO REP-DETAIL1.
             PERFORM AB35.

       BE93.
             GO TO BE999.

       BE95.
             MOVE 0                  TO WS-S1.
             MOVE "Y"                TO WS-PRT.
           IF NOT(WS-PRINTER = "S")
               DISPLAY DPT-DESC AT 1212 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             MOVE DPT-CODE           TO STK-LDG WS-SCAT.
             MOVE "!"                TO STK-CODE.
             PERFORM START-AT-STOCK-LEDG THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               MOVE "X"              TO WS-OPTION
               GO TO BE106.
            MOVE ZERO                TO WS-CCOST WS-CSELL WS-QUANT.

       BE100.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
           IF (WS-F-ERROR = 22) OR (STK-LDG NOT = WS-SCAT)
               GO TO BE105.
           IF WS-ZLT = "N"
               IF STK-LTIME = ZERO
                   GO TO BE100.
           IF WS-ZQT = "N"
               IF STK-QUANT = ZERO
                   GO TO BE100.
           IF WS-PRT = "Y"
               IF WS-PAGE > ZERO
                   MOVE "S"          TO WS-COMMAND
                   PERFORM CALL-PRINTUTL
                   MOVE "P"          TO WS-COMMAND
              END-IF
              PERFORM AB15
              MOVE "N"               TO WS-PRT.
           IF STK-IND > 2
               MOVE ZERO             TO STK-QUANT.
           IF STK-IND = 1 AND WS-PRC-IND = "N"
               MOVE ZERO             TO STK-QUANT.
           IF NOT(WS-PRINTER = "S")
               DISPLAY S03.
             PERFORM PRINT-ABORT.
           IF WS-OPTION = "Y"
               MOVE "**** PRINT ABORTED BY OPERATOR ****" TO REP-DETAIL1
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE "X"              TO WS-OPTION
               GO TO BE106.
           IF WS-USE-ITM = "Y"
               MOVE STK-ITEM         TO DEP-ITEM
               MOVE "/"              TO DEP-SL
               MOVE STK-ITM          TO DEP-ITM
           ELSE
               MOVE STK-CODE         TO DEP-EXT-ITEM.
             MOVE STK-DESC           TO DEP-DESC.
           IF STK-IND = 4
               MOVE 0                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               GO TO BE101.
           IF NOT(WS-FORMAT = 1)
               PERFORM BE103
               GO TO BE101.
             MOVE STK-BIN            TO DEP-BIN.
             MOVE STK-QUANT          TO DEP-QNT.
             MOVE STK-COST           TO DEP-COST.
      *    ****    A V E R A G E   C O S T
           IF STK-QUANT < 0.001
               MOVE STK-COST         TO STK-AVGE.
           IF STK-IND < 2
               IF STK-AVGE = ZERO
                   MOVE STK-COST     TO STK-AVGE.
             MOVE STK-AVGE           TO DEP-AVG.
           IF WS-ROC = "C"
               COMPUTE DEP-RET ROUNDED = STK-QUANT * STK-AVGE
           ELSE
               MOVE STK-SELL         TO DEP-RET.
             COMPUTE DEP-MWS ROUNDED = ((STK-WSALE - STK-COST) * 100.000) / STK-COST.
             COMPUTE DEP-MRET ROUNDED = ((STK-SELL - STK-COST) * 100.000) / STK-COST.
             COMPUTE DEP-MCSH ROUNDED = ((STK-CASH - STK-COST) * 100.000) / STK-COST.
             MOVE STK-MTDV           TO WS-SSELL.
             MOVE STK-MTDC           TO WS-SCOST.
           IF WS-SSELL = ZERO OR WS-SCOST = ZERO
               MOVE STK-COST         TO WS-SCOST
               COMPUTE WS-SSELL = (STK-SELL + STK-CASH) / 2.000 + .005.
      *        COMPUTE DEP-GP ROUNDED = ((WS-SSELL - WS-SCOST) * 100.000) / WS-SSELL.
           IF STK-ACT = ZERO
               MOVE STK-ACT          TO DEP-ACT
           ELSE
               MOVE STK-ACT          TO W22-DTE2
               PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT
               MOVE W22-DTE3         TO DEP-ACT.
            MULTIPLY STK-AVGE BY STK-QUANT GIVING WS-VALUE.
            MULTIPLY STK-SELL BY STK-QUANT GIVING WS-TOTAL.
            ADD STK-QUANT            TO WS-QUANT.
            ADD WS-VALUE             TO WS-CCOST.
            ADD WS-TOTAL             TO WS-CSELL.

       BE101.
           IF W02-LINAGE < W02-PRN-LENGTH
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM AB15.
             MOVE SPACES             TO REP-DETAIL1.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****" TO REP-DETAIL1
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE "X"              TO WS-OPTION
               GO TO BE106.

       BE102.
             GO TO BE100.

       BE103.
             MOVE STK-QUANT  TO DP2-QNT.
             MOVE STK-COST  TO DP2-COST.
      *    ****    A V E R A G E   C O S T
           IF STK-QUANT < 0.001
               MOVE STK-COST         TO STK-AVGE.
           IF STK-IND < 2
               IF STK-AVGE = ZERO
                   MOVE STK-COST     TO STK-AVGE.
             MOVE STK-AVGE           TO DP2-AVG.
             MOVE STK-SELL           TO DP2-RET.
             COMPUTE W25-VALUE = STK-SELL - STK-COST.
             MOVE W25-VALUE          TO DP2-MRKV.
             COMPUTE DP2-MRKP ROUNDED = ((STK-SELL - STK-COST) * 100.000) / STK-COST.
             MOVE STK-MARKUP         TO DP2-SYSP.
             COMPUTE DP2-SYSV ROUNDED = STK-COST * STK-MARKUP / 100.000.
             MOVE STK-ACT            TO W22-DTE2.
             PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT.
             MOVE W22-DTE3           TO DP2-ACT.
             MULTIPLY STK-AVGE BY STK-QUANT GIVING WS-VALUE.
             MULTIPLY STK-SELL BY STK-QUANT GIVING WS-TOTAL.
             ADD STK-QUANT           TO WS-QUANT.
             ADD WS-VALUE            TO WS-CCOST.
             ADD WS-TOTAL            TO WS-CSELL.

       BE104.
             EXIT.

       BE105.
           IF WS-PRT = "Y"
               GO TO BE106.
           IF W02-LINAGE > (W02-PRN-LENGTH - 4)
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM AB15.
             MOVE SPACES             TO REP-DETAIL1.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE "DEPARTMENT TOTAL (    )"  TO DEP-T-HEAD.
             MOVE DPT-CODE           TO DEP-T-LEDG.
             MOVE "QUANT"            TO DEP-T-H1.
             MOVE "AVRG"             TO DEP-T-H2.
             MOVE "SELLING"          TO DEP-T-H3.
             MOVE WS-QUANT           TO DEP-TQUANT.
             MOVE WS-CCOST           TO DEP-TCOST.
             MOVE WS-CSELL           TO DEP-TSELL.
             ADD WS-QUANT            TO WS-TQUANT.
             ADD WS-CCOST            TO WS-TCOST.
             ADD WS-CSELL            TO WS-TSELL.
             MOVE ZERO               TO WS-CCOST WS-CSELL WS-QUANT.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO REP-DETAIL1.

       BE106.
             EXIT.

       BE108.
             GO TO BE90.

       BE110.
             PERFORM ERASE-SCREEN.
             DISPLAY "SELECTIVE DEPARTMENTAL REPORT" AT 0226 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY S08.
             DISPLAY S15.
             ACCEPT WS-DEPT AT 0838 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.
           IF WS-DEPT = SPACES
               GO TO BE999.
             MOVE WS-DEPT            TO DPT-CODE.
             PERFORM CLEAR-L50.
           IF (DPT-CODE = "XXXX" OR "YYYY") OR (DPT-SCDE = "VAT")
               GO TO BE110.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
           IF WS-F-ERROR = 7
               DISPLAY S09
               ACCEPT WS-OPTION AT 5054 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT AUTO
               GO TO BE110.
             DISPLAY DPT-DESC AT 1212 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.

       BE115.
             MOVE "'C'ontinue or 'R'ekey  [ ]" TO WS-ERR-MES.
             MOVE SPACE              TO WS-OPTION.
             PERFORM OPT-MESSAGE.
           IF NOT(WS-OPTION = "R" OR "C")
               GO TO BE115.
           IF WS-OPTION = "R"
               GO TO BE110.
             PERFORM AB25.
             PERFORM BE95 THRU BE106.
           IF WS-OPTION = "X"
               GO TO BE999.
             PERFORM AB35
             MOVE 99                 TO WS-ADVANCE.
             MOVE "E"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             MOVE "P"                TO WS-COMMAND.
             MOVE ZERO               TO WS-PAGE.
             GO TO BE110.

       BE999.
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
             MOVE PAR-PRICED-IND     TO WS-PRC-IND.
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
             MOVE 3                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE STK-WEEK           TO WS-WEEK.
             MOVE STK-LABCDE         TO WS-LABCDE.
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
             INSPECT WS-RT-HD REPLACING LEADING "-" BY " ".
             INSPECT WS-CS-HD REPLACING LEADING "-" BY " ".
             INSPECT WS-WS-HD REPLACING LEADING "-" BY " ".
             MOVE 1                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DMY            TO W12-TODAY.
             MOVE PAR-YMD            TO W40-DATE1 W40-DATE2.
           IF W40-M2 > 3
               SUBTRACT 3            FROM W40-M2
           ELSE
               SUBTRACT 1            FROM W40-Y2
               ADD 9                 TO W40-M2.
             MOVE PAR-COMPANY        TO W95-COMP.
       ZA05.
             DISPLAY WS-BLNK78 AT 1402 WITH FOREGROUND-COLOR Cyan.
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
                               
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STP042"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
