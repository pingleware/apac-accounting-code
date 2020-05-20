      $SET LINKCOUNT"256" GNT"REP004.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****      ****        **     *
      *   **    **  **        **    **   **  **    **  **      ***     *
      *   **    **  **        **    **  **    **  **    **    * **     *
      *   ******    *****     *******   **    **  **    **   *  **     *
      *   **   **   **        **        **    **  **    **  *******    *
      *   **    **  **        **         **  **    **  **       **     *
      *   **    **  ********  **          ****      ****       ****    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D A Y - E N D   S A L E S   R E P O R T   P R O G R A M    *
      *                                                                *
      *       Version 9.04.04 - February/March 2018                    *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     REP004.
       AUTHOR.         James William Lemmon.
       DATE-WRITTEN.   AUGUST 1982.

               COPYRIGHT NOTICE: COPYRIGHT (C) 1982 - 2018
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

       COPY "DEPART.SLE".

       COPY "PARAM.SLE".

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

       COPY "DEPART.FD".

       COPY "PARAM.FD".

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
       77  WS-CHECK                PIC  X(18)         VALUE "aeWlimemnomLalismJ".
       77  WS-CLEAR                PIC  9(01)         VALUE ZERO.
       77  WS-AUD                  PIC  9(06)  COMP-5 VALUE ZERO.
       77  WS-S1                   PIC  9(04)  COMP-5.
       77  WS-S2                   PIC  9(04)  COMP-5.
       77  WS-S3                   PIC  9(04)  COMP-5.
       77  WS-S4                   PIC  9(04)  COMP-5.
       77  WS-S5                   PIC  9(04)  COMP-5.
       77  WS-S6                   PIC  9(04)  COMP-5.
       77  WS-S7                   PIC  9(04)  COMP-5.
       77  WS-NO                   PIC  9(04)  COMP-5.
       77  WS-PARKEY               PIC  9(04)  COMP-5.
       77  WS-NETKEY               PIC  9(04)  COMP-5.
       77  WS-AUDKEY               PIC  9(06)  COMP-5.
       77  WS-USE-ITM              PIC  X(01).
       77  WS-PAGE                 PIC  9(04)  COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-STATOP               PIC  X(01).
       77  WS-ERROR                PIC  9(01).
       77  WS-REP1                 PIC  9(01)  VALUE 0.
       77  WS-REP2                 PIC  9(01)  VALUE 0.
       77  WS-REP3                 PIC  9(01)  VALUE 0.
       77  WS-REP4                 PIC  9(01)  VALUE 0.
       77  WS-REP5                 PIC  9(01)  VALUE 0.
       77  WS-REP6                 PIC  9(01)  VALUE 0.
       77  WS-REP7                 PIC  9(01)  VALUE 0.
       77  WS-REP8                 PIC  9(01)  VALUE 0.
       77  WS-REP9                 PIC  9(01)  VALUE 0.
       77  WS-REP10                PIC  9(01)  VALUE 0.
       77  WS-REP11                PIC  9(01)  VALUE 0.
       77  WS-H1                   PIC  X(18)  VALUE "*** END REPORT ***".
       77  WS-H2                   PIC  X(06)  VALUE "M.T.D.".
       77  WS-H3                   PIC  X(06)  VALUE "Y.T.D.".
       77  WS-H4                   PIC  X(05)  VALUE "TOTAL".
       77  WS-H5                   PIC  X(06)  VALUE "CREDIT".
       77  WS-H6                   PIC  X(05)  VALUE "DEBIT".
       77  WS-H7                   PIC  X(09)  VALUE "ITEM CODE".
       77  WS-H8                   PIC  X(11)  VALUE "DESCRIPTION".
       77  WS-H9                   PIC  X(05)  VALUE "QUANT".
       77  WS-H10                  PIC  X(04)  VALUE "CASH".
       77  WS-H11                  PIC  X(05)  VALUE "VALUE".
       77  WS-H12                  PIC  X(03)  VALUE "NO.".
       77  WS-H13                  PIC  X(03)  VALUE "MTD".
       77  WS-H14                  PIC  X(03)  VALUE "YTD".
       77  TODAY-DDMMYY            PIC  9(08)  COMP-5.
       77  WS-USUB                 PIC  9(04)  COMP-5.

       COPY "WS.WS".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADING.
      *
      *    TYPE:
      *         C =  Clear comment line (50)
      *         E =  Clear lines (any line or lines between 5 and 46)
      *         H =  Change heading
      *         S =  Clear the screen
      *
           03  CRT-TYPE            PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START           PIC  9(02).
           03  CRT-END             PIC  9(02).
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15) VALUE "REP004".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "SALES ANALYSIS REPORTS".
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

       01  W02-FID.

       COPY "APACFIDS.ID".

       COPY "AUDIT.ID".

       COPY "CONTROL.ID".

       COPY "DEPART.ID".

       COPY "DEBTOR.ID".

       COPY "PARAM.ID".

       COPY "STOCK.ID".

       01  W02-PRINTER-DETAILS.
           03  W02-PRINTER         PIC  X(12).
           03  W02-PGE-LENGTH      PIC  9(03).
           03  W02-PRN-LENGTH      PIC  9(03).
           03  W02-LINAGE          PIC  9(03).
           03  W02-PRN-STATUS      PIC  X(01)    VALUE "C".
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

           03  R-L1                              REDEFINES W02-PRN-LINE.
               05  R-DETAIL        PIC  X(136).

           03  R-L2                              REDEFINES W02-PRN-LINE.
               05  R-PGE           PIC  X(06).
               05  R-PNO           PIC  Z(03)9.
               05  FILLER          PIC  X(09).
               05  R-CPY           PIC  X(40).
               05  FILLER          PIC  X(05).
               05  R-DATE          PIC  X(06).
               05  R-DTE           PIC  Z9/99/9999.

           03  R-L3                              REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(30).
               05  R-HEAD          PIC  X(20).
               05  FILLER          PIC  X(30).

           03  R-L4                              REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(02).
               05  R-H1            PIC  X(17).
               05  R-H2            PIC  X(27).
               05  R-H3            PIC  X(09).
               05  R-H4            PIC  X(17).
               05  R-H5            PIC  X(08).

           03  R-L5                              REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(02).
               05  R-I1            PIC  X(15).
               05  FILLER          PIC  X(02).
               05  R-DESC1         PIC  X(24).
               05  FILLER          PIC  X(01).
               05  R-Q1            PIC  Z(06)9.
               05  FILLER          PIC  X(02).
               05  R-UP1           PIC  Z(07)9.99.
               05  FILLER          PIC  X(02).
               05  R-V1            PIC  Z(08)9.99.
               05  FILLER          PIC  X(02).

           03  R-L6                              REDEFINES W02-PRN-LINE.
               05  R-H6            PIC  X(19).
               05  R-H7            PIC  X(31).
               05  R-H8            PIC  X(07).
               05  R-H9            PIC  X(09).
               05  R-H10           PIC  X(08).
               05  R-H11           PIC  X(11).
               05  R-H12           PIC  X(07).
               05  R-H13           PIC  X(04).

           03  R-L7                              REDEFINES W02-PRN-LINE.
               05  R-I2.
                   07  R-ITM       PIC  X(14).
                   07  R-SL        PIC  X(01).
                   07  R-EXT       PIC  X(03).
               05  FILLER          PIC  X(01).
               05  R-DESC2         PIC  X(29).
               05  FILLER          PIC  X(01).
               05  R-LEV           PIC  Z(05)9.
               05  FILLER          PIC  X(01).
               05  R-Q2            PIC  Z(05)9.
               05  FILLER          PIC  X(01).
               05  R-COST1         PIC  Z(05)9.99.
               05  FILLER          PIC  X(01).
               05  R-AVGE          PIC  Z(05)9.99.
               05  FILLER          PIC  X(01).
               05  R-NMTD          PIC  Z(05)9.
               05  FILLER          PIC  X(01).
               05  R-NYTD          PIC  Z(05)9.

           03  R-L8                              REDEFINES W02-PRN-LINE.
               05  R-H14           PIC  X(07).
               05  R-H14A          PIC  X(41).
               05  R-H15           PIC  X(11).
               05  R-H16           PIC  X(13).
               05  R-H17           PIC  X(08).

           03  R-L9                              REDEFINES W02-PRN-LINE.
               05  R-AC            PIC  X(06).
               05  FILLER          PIC  X(01).
               05  R-DNME          PIC  X(39).
               05  FILLER          PIC  X(01).
               05  R-REF           PIC  X(08).
               05  FILLER          PIC  X(01).
               05  R-DEBT          PIC  Z(06)9.99-.
               05  FILLER          PIC  X(02).
               05  R-CASH          PIC  Z(06)9.99-.

           03  R-L10                             REDEFINES W02-PRN-LINE.
               05  R-H18           PIC  X(06).
               05  R-H19           PIC  X(32).
               05  R-H20           PIC  X(12).
               05  R-H21           PIC  X(12).
               05  R-H22           PIC  X(12).
               05  R-H23           PIC  X(06).

           03  R-L11                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(46).
               05  R-H24           PIC  X(21).
               05  R-H25           PIC  X(13).

           03  R-L12                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(01).
               05  R-LEDG          PIC  9(03).
               05  FILLER          PIC  X(01).
               05  R-SDESC         PIC  X(27).
               05  FILLER          PIC  X(01).
               05  R-DMTD          PIC  -(07)9.99.
               05  FILLER          PIC  X(01).
               05  R-DYTD          PIC  -(07)9.99.
               05  FILLER          PIC  X(01).
               05  R-CMTD          PIC  -(07)9.99.
               05  FILLER          PIC  X(01).
               05  R-CYTD          PIC  -(07)9.99.

           03  R-L13                             REDEFINES W02-PRN-LINE.
               05  R-H26           PIC  X(20).
               05  FILLER          PIC  X(02).
               05  R-H27           PIC  X(05).
               05  R-H28           PIC  X(06).
               05  R-H29           PIC  X(09).
               05  R-H30           PIC  X(06).
               05  R-H31           PIC  X(05).
               05  R-H32           PIC  X(10).
               05  R-H33           PIC  X(05).
               05  R-H34           PIC  X(06).
               05  R-H35           PIC  X(06).

           03  R-L14                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(05).
               05  R-H36           PIC  X(15).
               05  R-N1            PIC  Z(04)9.
               05  R-VAT           PIC  Z9.99    REDEFINES R-N1.
               05  R-PERC          PIC  X(01).
               05  FILLER          PIC  X(01).
               05  R-V5            PIC  Z(08)9.99-.
               05  FILLER          PIC  X(01).
               05  R-N2            PIC  Z(04)9.
               05  FILLER          PIC  X(01).
               05  R-V6            PIC  Z(08)9.99-.
               05  FILLER          PIC  X(01).
               05  R-N3            PIC  Z(04)9.
               05  FILLER          PIC  X(01).
               05  R-V7            PIC  Z(08)9.99-.

           03  REP-LINE3                         REDEFINES W02-PRN-LINE.
               05  REP-COMPANY     PIC  X(40).
               05  REP-DATE        PIC  X(06).
               05  REP-DTE         PIC  Z9/99/9999.
               05  FILLER          PIC  X(16).

           03  REP-LINE4                         REDEFINES W02-PRN-LINE.
               05  REP-REMK        PIC  X(20).
               05  FILLER          PIC  X(01).
               05  REP-VAL-NO.
                   07  REP-VAL     PIC  Z(06)9.99-.
               05  REP-VAL-NO-R                  REDEFINES REP-VAL-NO.
                   07  REP-NO      PIC  Z(06)9.
                   07  FILLER      PIC  X(04).
               05  FILLER          PIC  X(24).

           03  SAL-L1                            REDEFINES W02-PRN-LINE.
               05  SAL-H1          PIC  X(10).
               05  SAL-NO          PIC  ZZ9.
               05  FILLER          PIC  X(01).
               05  SAL-SMAN        PIC  X(30).
               05  FILLER          PIC  X(36).

           03  SAL-L2                            REDEFINES W02-PRN-LINE.
               05  SAL-H2          PIC  X(15).
               05  SAL-H3          PIC  X(36).
               05  SAL-H4          PIC  X(09).
               05  SAL-H5          PIC  X(11).
               05  SAL-H6          PIC  X(08).
               05  SAL-H6A         PIC  X(09).
               05  SAL-H6B         PIC  X(06).

           03  SAL-L3                            REDEFINES W02-PRN-LINE.
               05  SAL-ITEM        PIC  X(14).
               05  FILLER          PIC  X(01).
               05  SAL-DESC        PIC  X(28).
               05  FILLER          PIC  X(01).
               05  SAL-COST        PIC  Z(07)9.99-.
               05  FILLER          PIC  X(01).
               05  SAL-SELL        PIC  Z(07)9.99-.
               05  FILLER          PIC  X(01).
               05  SAL-GP          PIC  ZZ9.99-.
               05  FILLER          PIC  X(02).
               05  SAL-MARKUP      PIC  ZZ9.99-.
               05  FILLER          PIC  X(01).
               05  SAL-NOCHARGE.
                   07  FILLER      PIC  X(01).
                   07  SAL-SUPER   PIC  ZZ.
               05  FILLER          PIC  X(01).
               05  SAL-PAY         PIC  X(03).

           03  SAL-L4                            REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(02).
               05  SAL-H7          PIC  X(14).
               05  SAL-REF         PIC  X(08).
               05  FILLER          PIC  X(02).
               05  SAL-H8          PIC  X(22).
               05  FILLER          PIC  X(30).

           03  SAL-L5                            REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(02).
               05  SAL-H9          PIC  X(10).
               05  SAL-H10         PIC  X(20).
               05  SAL-H11         PIC  X(12).
               05  SAL-H12         PIC  X(16).
               05  SAL-H13         PIC  X(13).
               05  SAL-H14         PIC  X(07).

           03  SAL-L6                            REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(01).
               05  SAL-RF          PIC  X(08).
               05  FILLER          PIC  X(03).
               05  SAL-DSC         PIC  X(11).
               05  FILLER          PIC  X(02).
               05  SAL-CST         PIC  Z(08)9.99-.
               05  FILLER          PIC  X(02).
               05  SAL-SEL         PIC  Z(08)9.99-.
               05  FILLER          PIC  X(02).
               05  SAL-PRF         PIC  Z(08)9.99-.
               05  FILLER          PIC  X(04).
               05  SAL-GPR         PIC  ZZ9.99.
               05  FILLER          PIC  X(02).

       COPY "W12.WS".

       COPY "W15.WS".

       01  W25-CALCS.
           03  W25-CALCULATIONS.
               05  W25-RESULT      PIC  9(05)V99.
               05  W25-RESULT1                   REDEFINES W25-RESULT.
                   07  W25-WHOLE   PIC  9(05).
                   07  W25-DECIMAL PIC  9(02).
               05  W25-RESULT2                   REDEFINES W25-RESULT.
                   07  W25-KEY     PIC  9(04).
                   07  W25-SUB     PIC  9(01).
                   07  FILLER      PIC  9(02).
               05  W25-TOTAL       PIC S9(09)V99 COMP-3.
               05  W25-VALUE       PIC S9(09)V99 COMP-3.
               05  W25-COST1       PIC S9(09)V99 COMP-3.
               05  W25-COST2       PIC S9(09)V99 COMP-3.
               05  W25-COST3       PIC S9(09)V99 COMP-3.
               05  W25-SELL1       PIC S9(09)V99 COMP-3.
               05  W25-SELL2       PIC S9(09)V99 COMP-3.
               05  W25-SELL3       PIC S9(09)V99 COMP-3.
               05  W25-CASH        PIC S9(09)V99 COMP-3.
               05  W25-REFF        PIC S9(09)V99 COMP-3.
               05  W25-INV         PIC S9(09)V99 COMP-3.
               05  W25-CRN         PIC S9(09)V99 COMP-3.

       01  W40-INDIC.
           03  W40-SMAN.
               05  W40-SREP        PIC 9(01)     COMP-5 OCCURS 50.
           03  W40-DRAW.
               05  W40-DREP        PIC 9(01)     COMP-5 OCCURS 40.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       01  W70-WORK.
           03  W70-REP             PIC 9(04)     COMP-5.

       01  W80-ACCOUNT.
           03  W80-ACNO            PIC 9(05).

       01  W90-TRAN.
           03  W90-DATE            PIC X(10).
           03  W90-DTE             PIC 99/99/9999 REDEFINES W90-DATE
               .
           03  W90-CODE            PIC  9(02).
           03  W90-REF             PIC  X(06).
          03  W90-DB               PIC  9(09)V99 COMP-3.
          03  W90-CR               PIC  9(09)V99 COMP-3.
          03  W90-BDT              PIC S9(09)V99 COMP-3.
          03  W90-BCR              PIC S9(09)V99 COMP-3.
           03  W90-DESC            PIC  X(12).
          03  W90-90               PIC S9(09)V99 COMP-3.
          03  W90-60               PIC S9(09)V99 COMP-3.
          03  W90-30               PIC S9(09)V99 COMP-3.
          03  W90-CUR              PIC S9(09)V99 COMP-3.
          03  W90-BAL              PIC S9(09)V99 COMP-3.

       01  W200-TRAN.
           03  W200-ACCUM.
        05  W200-LEDG              PIC  X(04)    OCCURS 300.
              05  W200-DT          PIC S9(09)V99 COMP-3 OCCURS 300.
              05  W200-CR          PIC S9(09)V99 COMP-3 OCCURS 300.
              05  W200-DTOT        PIC S9(09)V99 COMP-3.
              05  W200-CTOT        PIC S9(09)V99 COMP-3.
              05  W200-COST        PIC S9(09)V99 COMP-3.
              05  W200-SELL        PIC S9(09)V99 COMP-3.
              05  W200-NET         PIC S9(09)V99 COMP-3.
              05  W200-ADJ         PIC S9(09)V99 COMP-3.

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

       COPY "FILE-PEND.IDS".

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

       01  SD-LOCKED.
           03  BACKGROUND-COLOR Red FOREGROUND-COLOUR Black.
               05  LINE 12 COLUMN  8 VALUE "This program can only be run when no other workstations are logged".
               05  LINE 13 COLUMN  8 VALUE "into the accounting system.  All other  workstations should return".
               05  LINE 14 COLUMN  8 VALUE "to the program  control  menu or exit  from  the accounting system".
               05  LINE 15 COLUMN  8 VALUE "before you may continue.                                          ".
               05  LINE 16 COLUMN  8 VALUE "Files locked - Try later     Status                               ".
               05  LINE 16 COLUMN 44 PIC  X(02) FROM                            WS-STATUS                        FOREGROUND-COLOR Grey HIGHLIGHT.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Red HIGHLIGHT.
               05  LINE 19 COLUMN 32 VALUE "               ".
               05  LINE 20 COLUMN 32 VALUE " Press any key ".
               05  LINE 21 COLUMN 32 VALUE "               ".
      *
      *      ******   ******    *****    *****   ******  ******   **   **  ******    ****** 
      *      **   **  **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **   **  **  **   **   **  **       **      **   **  **   **  **  **    **
      *      ******   *****    **   **  **       *****   **   **  **   **  *****     *****
      *      **       **  **   **   **  **       **      **   **  **   **  **  **    **
      *      **       **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **       **   **   *****    *****   ******  ******    *****   **   **   ******
      *
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY PEND-FILE-IDS.
       AA00M    SECTION.
       AA00.
            PERFORM ZA000-INIT.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *     **  *   *   * *   *  *  **  *   *   *     *   *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *     *  **   *     *   *  *  *  **   *   *     *  *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** *   *
      *
             CLOSE PARAM.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ As the parameter file has been assigned exclusive in this ³
      *    ³ program, it is  necessary to close it.  W02-LINAGE is set ³
      *    ³ to 99 which informs the PRINTUTL program to open the file ³
      *    ³ and close it again on exit.                               ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             MOVE 99                 TO W02-LINAGE.
      *
      *    ****     S E T   T O   8   L I N E S   P E R   I N C H
      *
            MOVE 8                   TO W02-PRN-TYPE.
            PERFORM OPEN-PRINTER.
            MOVE ZERO                TO W02-LINAGE.
            OPEN I-O PARAM LOCK.
            MOVE "P"                 TO WS-COMMAND.
            MOVE "D"                 TO W02-PRN-TYPE.
            MOVE SPACES              TO R-L1.

       AA01.
             PERFORM CG00R.
             CLOSE DEPART PARAM.
      *    *************************************************************
      *    ****                     6   L P I
      *    *************************************************************
           IF NOT(WS-PRINTER = "S")
               MOVE 0                TO WS-ADVANCE
               MOVE 6                TO W02-PRN-TYPE
               PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             MOVE "C"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.

       AA49.
             EXIT PROGRAM.

      *
      * ****  ***** ****   ***  ****  *****    *   * *****  ***  ****  *** *   *  ***  
      * *   * *     *   * *   * *   *   *      *   * *     *   * *   *  *  **  * *     
      * ****  ***   ****  *   * ****    *      ***** ***   ***** *   *  *  * * * *  ** 
      * *   * *     *     *   * *   *   *      *   * *     *   * *   *  *  *  ** *   * 
      * *   * ***** *      ***  *   *   *      *   * ***** *   * ****  *** *   *  ***  
      *
       AA60.
             MOVE "H"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DETAIL.
             MOVE "PAGE:"            TO RC-PGE.
             MOVE "DATE:"            TO RC-DATE.
             ADD 1                   TO WS-PAGE.
             MOVE WS-PAGE            TO RC-PNO.
             MOVE W12-TODAY          TO RC-DTE.
             MOVE W95-COMP           TO RC-CPY.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.

       AA65.
             EXIT.

       COPY "AA900.ALM".

      * COPY "DATE.CHK".

       COPY "LOCKED.REC".

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
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "OPTION.CRT".

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
      *    ³ To clear line 50:                                         ³
      *    ³    PERFORM CLEAR-L50                                      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "CLEAR.CRT".

       COPY "ABORT.PRN".

       COPY "PRINTUTL.AP6".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³        R E A D  F I L E S   R O U T I N E S        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AN00-READ        SECTION.
      *
      *    ****  ***** ****    *   ****  *****    ***** *** *     *****
      *    *   * *     *   *  * *  *   *   *      *      *  *     *
      *    *   * *     *   * *   * *   *   *      *      *  *     *
      *    *   * ***   ****  ***** ****    *      ***    *  *     ***
      *    *   * *     *     *   * *  *    *      *      *  *     *
      *    *   * *     *     *   * *   *   *      *      *  *     *
      *    ****  ***** *     *   * *   *   *      *     *** ***** *****
      *
       READ-DEPART.
            READ DEPART  WITH IGNORE LOCK KEY IS DPT-CODE.
             GO TO READ-DEPART-STATUS.

       READ-DEPART-NEXT.
             READ DEPART NEXT.
           IF RECORD-LOCKED
               PERFORM LOCKED-RECORD
               GO TO READ-DEPART-NEXT.
           IF WS-STAT1 = "1"
               MOVE 7                TO WS-F-ERROR
               GO TO READ-DEPART-EXIT.
             GO TO READ-DEPART-STATUS.

       START-AT-DEPT-CODE.
            START DEPART KEY >= DPT-CODE.

       READ-DEPART-STATUS.
           IF WS-STATUS = "00"
               MOVE ZERO             TO WS-F-ERROR
               GO TO READ-DEPART-EXIT.
           IF WS-STATUS = "23"
               MOVE 7                TO WS-F-ERROR
               GO TO READ-DEPART-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 7                TO WS-F-ERROR
               PERFORM READ-ERROR.
      
       READ-DEPART-EXIT.
             EXIT.
      *
      *    ****    *   ****    *    *     *     ***** *** *     *****
      *    *   *  * *  *   *  * *   **   **     *      *  *     *
      *    *   * *   * *   * *   *  * * * *     *      *  *     *
      *    ****  ***** ****  *****  *  *  *     ***    *  *     ***
      *    *     *   * *  *  *   *  *     *     *      *  *     *
      *    *     *   * *   * *   *  *     *     *      *  *     *
      *    *     *   * *   * *   *  *     *     *     *** ***** *****
      *
       READ-PARAM.
             READ PARAM.
           IF WS-STATUS = "00"
               MOVE ZERO             TO WS-F-ERROR
               GO TO READ-PARAM-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 15               TO WS-F-ERROR
               PERFORM READ-ERROR.
           IF RECORD-LOCKED
               PERFORM LOCKED-RECORD
               GO TO READ-PARAM.
             MOVE 15                 TO WS-F-ERROR.
             PERFORM READ-ERROR.

       READ-PARAM-EXIT.
             EXIT.

      *
      *    ****    SALES ANALYSIS REPORT    ****
      *
       CG00R                  SECTION 5.
       CG00.
             PERFORM ERASE-SCREEN.
             DISPLAY "Busy printing Sales Analysis Report" AT 1020 WITH FOREGROUND-COLOR Grey  HIGHLIGHT.
             DISPLAY "Are you sure Y/N?"                   AT 1220 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
       CG02.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-OPTION = "Y" OR "N")
               GO TO CG02.
           IF WS-OPTION = "N"
               GO TO CG999.
             MOVE ZERO               TO WS-PAGE.
      *
      *    ****   1 7 C P I ( C O N D E N S E D   P R I N T )
      *
           IF NOT(WS-PRINTER = "S")
               MOVE 0                TO WS-ADVANCE
               MOVE 3                TO W02-PRN-TYPE
               PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             MOVE 1                  TO WS-S6.

       CG03.
             MOVE SPACES             TO W200-LEDG(WS-S6).
             MOVE ZERO               TO W200-DT(WS-S6) W200-CR(WS-S6).
           IF WS-S6 < 300
               ADD 1                 TO WS-S6
               GO TO CG03.
             MOVE ZERO               TO W200-DTOT W200-CTOT W200-COST W200-SELL W200-NET.

       CG05.
             PERFORM AA60 THRU AA65.
             MOVE "SALES ANALYSIS"   TO RC-HEAD.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE WS-H6              TO RS-H26.
             MOVE WS-H5              TO RS-H27.
             MOVE "DISCOUNT"         TO RS-H27A.
             MOVE WS-H15             TO RS-H28.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "NO"               TO RS-H18.
             MOVE WS-H8              TO RS-H19.
             MOVE WS-H2              TO RS-H20 RS-H22 RS-H23A.
             MOVE WS-H3              TO RS-H21 RS-H23 RS-H23B.
             MOVE "INCOME"           TO RS-H24.
             MOVE "DISCOUNT"         TO RS-H25.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "D"                TO W02-PRN-TYPE.

       CG10.
             MOVE " !"               TO DPT-CODE.
             PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.
           IF WS-F-ERROR = 7
               GO TO CG20.

       CG15.
             PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
           IF WS-F-ERROR = 7
               GO TO CG20.
           IF (DPT-SCDE = "VAT")
               GO TO CG15.
             MOVE DPT-CODE           TO RS-LEDG.
             MOVE DPT-DESC           TO RS-SDESC.
             MULTIPLY DPT-SALES-MTD BY -1 GIVING RS-CMTD.
             MULTIPLY DPT-SALES-YTD BY -1 GIVING RS-CYTD.
             MOVE DPT-RET-MTD        TO RS-DMTD.
             MOVE DPT-RET-YTD        TO RS-DYTD.
             MOVE DPT-SALE-GL        TO RS-GL1.
             SUBTRACT DPT-SALES-MTD  FROM W200-CR(1).
             SUBTRACT DPT-SALES-YTD  FROM W200-CR(2).
             ADD DPT-RET-MTD         TO W200-DT(1).
             ADD DPT-RET-YTD         TO W200-DT(2).
             MOVE DPT-DISC-MTD       TO RS-DSMT.
             MOVE DPT-DISC-YTD       TO RS-DSYT.
             MOVE DPT-DISC-GL        TO RS-GL2.
             ADD DPT-DISC-MTD        TO W200-DT(3).
             ADD DPT-DISC-YTD        TO W200-DT(4).

       CG16.
           IF W02-LINAGE < W02-PRN-LENGTH
               MOVE 2                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM CG05.
             MOVE SPACES             TO R-DETAIL.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****" TO R-L1
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE "P"              TO WS-COMMAND
               GO TO CG999.
             GO TO CG15.

       CG20.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE WS-H4              TO RS-H19.
             MOVE W200-DT(1)         TO RS-DMTD.
             MOVE W200-DT(2)         TO RS-DYTD.
             MOVE W200-CR(1)         TO RS-CMTD.
             MOVE W200-CR(2)         TO RS-CYTD.
             MOVE W200-DT(3)         TO RS-DSMT.
             MOVE W200-DT(4)         TO RS-DSYT.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE WS-H1              TO R-DETAIL.
             MOVE 99                 TO WS-ADVANCE.
             MOVE "E"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             MOVE "P"                TO WS-COMMAND.
           IF NOT(WS-PRINTER = "S")
               MOVE 0                TO WS-ADVANCE
               MOVE 1                TO W02-PRN-TYPE
               PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.

       CG999.
             EXIT.
      *    *************************************************************
      *                 I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT       SECTION 8.
       ZA00-OPEN.
             MOVE LS-COMPANY         TO CRT-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W12-TODAY.
             MOVE LS-TODAY-YYMMDD    TO W12-T-YMD.
             MOVE LS-USUB            TO WS-USUB.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

       ZA00A-CONTINUE.
      *
      *     ****   ***  ****   ***  *   *    ***** *** *     *****
      *     *   * *   * *   * *   * ** **    *      *  *     *
      *     *   * *   * *   * *   * * * *    *      *  *     *
      *     ****  ***** ****  ***** *   *    ***    *  *     ***
      *     *     *   * *   * *   * *   *    *      *  *     *
      *     *     *   * *   * *   * *   *    *      *  *     *
      *     *     *   * *   * *   * *   *    *     *** ***** *****
      *
             OPEN I-O PARAM LOCK.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS NOT = "00"
               MOVE 15               TO WS-F-ERROR
               PERFORM OPEN-ERROR.
      *
      *    ****  ***** ****    *   ****  *****    ***** *** *     *****
      *    *   * *     *   *  * *  *   *   *      *      *  *     *
      *    *   * *     *   * *   * *   *   *      *      *  *     *
      *    *   * ***   ****  ***** ****    *      ***    *  *     ***
      *    *   * *     *     *   * *  *    *      *      *  *     *
      *    *   * *     *     *   * *   *   *      *      *  *     *
      *    ****  ***** *     *   * *   *   *      *     *** ***** *****
      *
             OPEN I-O DEPART.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS NOT = "00"
               MOVE 7                TO WS-F-ERROR
               PERFORM OPEN-ERROR.
       ZA10.
             MOVE 1                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      *      MOVE PAR-DMY            TO W12-TODAY.
      *      MOVE PAR-YMD            TO W12-T-YMD.
             MOVE PAR-COMPANY        TO CRT-COMP.
      *
      *   As this version of the system includes all modules the
      *   following tests are no longer required. 
      *   These will be removed with the next cleanup of code.
      *
      *   IF LS0-DTP NOT = 2
      *        GO TO ZA999.
      *   IF LS0-STP NOT = 2
      *        GO TO ZA999.
             GO TO ZA999.

       COPY "ZA49.PRO".

       ZA50.
           IF WS-F-ERROR = 3
               MOVE "CREDITOR file1" TO WS-FILE
           ELSE
           IF WS-F-ERROR = 6
               MOVE "DEBTOR file"    TO WS-FILE
           ELSE
           IF WS-F-ERROR = 22
               MOVE "STOCK FILE"     TO WS-FILE.
             DISPLAY "Rebuild "                  AT 0812 WS-FILE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Press any key to continue" AT 1012.
             ACCEPT WS-OPTION                    AT 1038 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.

       ZA51.
             EXIT PROGRAM.

       ZA200.
             MOVE 12                 TO SHADE-ROW.
             MOVE 8                  TO SHADE-COL.
             MOVE 66                 TO SHADE-WIDTH.
             MOVE 5                  TO SHADE-LINES.
             DISPLAY SD-LOCKED.
             PERFORM SCREEN-SHADOW.
             MOVE 19                 TO SHADE-ROW.
             MOVE 32                 TO SHADE-COL.
             MOVE 15                 TO SHADE-WIDTH.
             MOVE 3                  TO SHADE-LINES.
             PERFORM SCREEN-SHADOW.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       ZA205.
             EXIT PROGRAM.

       ZA999.
             EXIT.
      *
      *    ****    I / O   E R R O R   M E S S A G E S
      *
       ZB000-ERROR      SECTION 8.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 7      MOVE W02-DEPART    TO WS-FILE
                           MOVE DPT-CODE      TO WS-KEYX
               WHEN 15     MOVE WS-PARID      TO WS-FILE
                           MOVE WS-PARKEY     TO WS-KEY
               WHEN OTHER  MOVE "ERROR"       TO WS-FILE
                           MOVE "REP004"      TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
