      $SET LINKCOUNT"256" GNT"REP001.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****      ****       **      *
      *   **    **  **        **    **   **  **    **  **     ***      *
      *   **    **  **        **    **  **    **  **    **     **      *
      *   ******    *****     *******   **    **  **    **     **      *
      *   **   **   **        **        **    **  **    **     **      *
      *   **    **  **        **         **  **    **  **      **      *
      *   **    **  ********  **          ****      ****     ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D A Y - E N D   R E P O R T S   P R O G R A M              *
      *                                                                *
      *       Version 9.04.04 - February/March 2018                    *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     REP001.
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

       COPY "AUDIT.SLE".

       COPY "CONTROL.SLE".

       COPY "DEBTOR.SLE".

       COPY "DEPART.SLE".

       COPY "PARAM.SLE".

       COPY "STOCK.SLE".

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

       COPY "AUDIT.FD".

       COPY "CONTROL.FD".

       COPY "DEBTOR.FD".

       COPY "DEPART.FD".

       COPY "PARAM.FD".

       COPY "STOCK.FD".

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
       77  WS-CHECK                PIC  X(18)    VALUE "aeWlimemnomLalismJ".
       77  WS-CLEAR                PIC  9(01)    VALUE ZERO.
       77  WS-AUD                  PIC  9(06)    COMP-5 VALUE ZERO.
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S5                   PIC  9(04)    COMP-5.
       77  WS-S6                   PIC  9(04)    COMP-5.
       77  WS-S7                   PIC  9(04)    COMP-5.
       77  WS-NO                   PIC  9(04)    COMP-5.
       77  WS-PARKEY               PIC  9(04)    COMP-5.
       77  WS-NETKEY               PIC  9(04)    COMP-5.
       77  WS-AUDKEY               PIC  9(06)    COMP-5.
       77  WS-USE-ITM              PIC  X(01).
       77  WS-LINES                PIC  9(02)    VALUE 66.
       77  WS-PAGE                 PIC  9(04)    COMP-5.
       77  WS-PGE-LENGTH           PIC  9(02)    VALUE 66.
       77  WS-PRN-LENGTH           PIC  9(02)    VALUE 62.
       77  WS-GST-RATE             PIC  9(02)V99 COMP-3.
       77  WS-OPTION               PIC  X(01).
       77  WS-STATOP               PIC  X(01).
       77  WS-VATTYPE              PIC  X(01).
       77  WS-REPLY                PIC  X(01).
       77  WS-ERROR                PIC  9(01).
       77  WS-CHAR1                PIC  X(01).
       77  WS-CHAR2                PIC  X(01).
       77  WS-COMP                 PIC  9(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-LEDG                 PIC  9(03).
       77  WS-NUM                  PIC  Z9.
       77  WS-REF                  PIC  X(08).
       77  WS-BATCH                PIC  9(05)    COMP-3.
       77  WS-REP1                 PIC  9(01)    VALUE 0.
       77  WS-REP2                 PIC  9(01)    VALUE 0.
       77  WS-REP3                 PIC  9(01)    VALUE 0.
       77  WS-REP4                 PIC  9(01)    VALUE 0.
       77  WS-REP5                 PIC  9(01)    VALUE 0.
       77  WS-REP6                 PIC  9(01)    VALUE 0.
       77  WS-REP7                 PIC  9(01)    VALUE 0.
       77  WS-REP8                 PIC  9(01)    VALUE 0.
       77  WS-REP9                 PIC  9(01)    VALUE 0.
       77  WS-REP10                PIC  9(01)    VALUE 0.
       77  WS-REP11                PIC  9(01)    VALUE 0.
       77  WS-H1                   PIC  X(18)    VALUE "*** END REPORT ***".
       77  WS-H2                   PIC  X(06)    VALUE "M.T.D.".
       77  WS-H3                   PIC  X(06)    VALUE "Y.T.D.".
       77  WS-H4                   PIC  X(05)    VALUE "TOTAL".
       77  WS-H5                   PIC  X(06)    VALUE "CREDIT".
       77  WS-H6                   PIC  X(05)    VALUE "DEBIT".
       77  WS-H7                   PIC  X(09)    VALUE "ITEM CODE".
       77  WS-H8                   PIC  X(11)    VALUE "DESCRIPTION".
       77  WS-H9                   PIC  X(05)    VALUE "QUANT".
       77  WS-H10                  PIC  X(04)    VALUE "CASH".
       77  WS-H11                  PIC  X(05)    VALUE "VALUE".
       77  WS-H12                  PIC  X(03)    VALUE "NO.".
       77  WS-H13                  PIC  X(03)    VALUE "MTD".
       77  WS-H14                  PIC  X(03)    VALUE "YTD".
       77  WS-H15                  PIC  X(08)    VALUE "G/LEDGER".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
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
           03  CRT-PROGRAM         PIC  X(15)    VALUE "REP001".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40)    VALUE "DAY-END REPORTS".
      *
      *   COMPany Name
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
          03  WS-SYS-ID            PIC  X(03).

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

           03  R-L2A                             REDEFINES W02-PRN-LINE.
               05  RC-PGE          PIC  X(06).
               05  RC-PNO          PIC  Z(03)9.
               05  FILLER          PIC  X(33).
               05  RC-CPY          PIC  X(40).
               05  FILLER          PIC  X(33).
               05  RC-DATE         PIC  X(06).
               05  RC-DTE          PIC  Z9/99/9999.

           03  R-L3                              REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(30).
               05  R-HEAD          PIC  X(20).
               05  FILLER          PIC  X(30).

           03  R-L3A                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(56).
               05  RC-HEAD         PIC  X(20).
               05  FILLER          PIC  X(56).

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

           03  R-L10-TO                          REDEFINES W02-PRN-LINE.
               05  RT-H18          PIC  X(05).
               05  RT-H19          PIC  X(35).
               05  RT-H20          PIC  X(14).
               05  RT-H21          PIC  X(10).
               05  RT-H22          PIC  X(10).
               05  RT-H23          PIC  X(06).

           03  R-L10A                            REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(04).
               05  RC-H18          PIC  X(09).
               05  RC-H19          PIC  X(38).
               05  RC-H20          PIC  X(14).
               05  RC-H21          PIC  X(14).
               05  RC-H22          PIC  X(14).
               05  RC-H23          PIC  X(12).
               05  RC-H24          PIC  X(13).
               05  RC-H25          PIC  X(14).

           03  R-L10B                            REDEFINES W02-PRN-LINE.
               05  RS-H18          PIC  X(04).
               05  RS-H19          PIC  X(36).
               05  RS-H20          PIC  X(13).
               05  RS-H21          PIC  X(13).
               05  RS-H22          PIC  X(13).
               05  RS-H23          PIC  X(13).
               05  RS-H23A         PIC  X(13).
               05  RS-H23B         PIC  X(11).
               05  RS-H24          PIC  X(08).
               05  RS-H25          PIC  X(08).

           03  R-L10C                            REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(01).
               05  RP-H18          PIC  X(08).
               05  RP-H19          PIC  X(37).
               05  RP-H20          PIC  X(13).
               05  RP-H21          PIC  X(13).
               05  RP-H22          PIC  X(13).
               05  RP-H23          PIC  X(09).
               05  RP-H23A         PIC  X(15).
               05  RP-H24          PIC  X(13).
               05  RP-H25          PIC  X(10).

           03  R-L11                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(46).
               05  R-H24           PIC  X(21).
               05  R-H25           PIC  X(13).

           03  R-L11A                            REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(47).
               05  RS-H26          PIC  X(25).
               05  RS-H27          PIC  X(26).
               05  RS-H27A         PIC  X(22).
               05  RS-H28          PIC  X(12).

           03  R-L11B                            REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(56).
               05  RC-H26          PIC  X(28).
               05  RC-H27          PIC  X(27).
               05  RC-H28          PIC  X(21).

           03  R-L11C                            REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(51).
               05  RP-H26          PIC  X(26).
               05  RP-H27          PIC  X(37).
               05  RP-H28          PIC  X(18).

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

           03  R-L12-TO                          REDEFINES W02-PRN-LINE.
               05  RT-LEDG         PIC  X(04).
               05  FILLER          PIC  X(01).
               05  RT-SDESC        PIC  X(27).
               05  FILLER          PIC  X(01).
               05  RT-DMTD         PIC  -(09)9.99.
               05  FILLER          PIC  X(01).
               05  RT-DYTD         PIC  -(09)9.99.
               05  FILLER          PIC  X(01).
               05  RT-MUP          PIC  -(05)9.99.
               05  FILLER          PIC  X(01).
               05  RT-GP           PIC  -(05)9.99.

           03  R-L12A                            REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(04).
               05  RC-LEDG         PIC  X(04).
               05  FILLER          PIC  X(05).
               05  RC-SDESC        PIC  X(32).
               05  RC-DMTD         PIC  Z(08)9.99.
               05  FILLER          PIC  X(02).
               05  RC-DYTD         PIC  Z(08)9.99.
               05  FILLER          PIC  X(02).
               05  RC-CMTD         PIC  Z(08)9.99.
               05  FILLER          PIC  X(02).
               05  RC-CYTD         PIC  Z(08)9.99.
               05  FILLER          PIC  X(07).
               05  RC-GL1          PIC  9999/99.
               05  FILLER          PIC  X(06).
               05  RC-GL2          PIC  9999/99.
               05  FILLER          PIC  X(06).

           03  R-L12B                            REDEFINES W02-PRN-LINE.
               05  RS-LEDG         PIC  X(04).
               05  FILLER          PIC  X(01).
               05  RS-SDESC        PIC  X(28).
               05  FILLER          PIC  X(01).
               05  RS-DMTD         PIC  Z(08)9.99.
               05  FILLER          PIC  X(01).
               05  RS-DYTD         PIC  Z(08)9.99.
               05  FILLER          PIC  X(01).
               05  RS-CMTD         PIC  Z(08)9.99-.
               05  FILLER          PIC  X(01).
               05  RS-CYTD         PIC  Z(08)9.99-.
               05  FILLER          PIC  X(01).
               05  RS-DSMT         PIC  Z(08)9.99-.
               05  FILLER          PIC  X(01).
               05  RS-DSYT         PIC  Z(08)9.99-.
               05  FILLER          PIC  X(01).
               05  RS-GL1          PIC  9999/99.
               05  FILLER          PIC  X(01).
               05  RS-GL2          PIC  9999/99.

           03  R-L12C                            REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(03).
               05  RP-LEDG         PIC  X(04).
               05  FILLER          PIC  X(02).
               05  RP-SDESC        PIC  X(31).
               05  RP-DMTD         PIC  Z(08)9.99.
               05  FILLER          PIC  X(01).
               05  RP-DYTD         PIC  Z(08)9.99.
               05  FILLER          PIC  X(01).
               05  RP-CMTD         PIC  Z(08)9.99.
               05  FILLER          PIC  X(01).
               05  RP-CYTD         PIC  Z(08)9.99.
               05  FILLER          PIC  X(01).
               05  RP-ADJ          PIC  Z(08)9.99-.
               05  FILLER          PIC  X(03).
               05  RP-GL1          PIC  9999/99.
               05  FILLER          PIC  X(06).
               05  RP-GL2          PIC  9999/99.
               05  FILLER          PIC  X(04).

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
               05  R-N1R                         REDEFINES R-N1.
                   07  R-VAT       PIC  Z9.99.
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

           03  VAT-L1                            REDEFINES W02-PRN-LINE.
               05  VAT-DAT         PIC  99/99/9999.
               05  FILLER          PIC  X(01).
               05  VAT-REF         PIC  X(08).
               05  FILLER          PIC  X(01).
               05  VAT-TRAN        PIC  X(11).
               05  FILLER          PIC  X(01).
               05  VAT-NAME.
                   07  FILLER      PIC  X(04).
                   07  VAT-CDE     PIC  9(01).
                   07  FILLER      PIC  X(03).
                   07  VAT-RTE     PIC  Z9.99.
                   07  VAT-PERC    PIC  X(01).
               05  FILLER          PIC  X(01).
               05  VAT-TRAN-AMT    PIC  Z(07)9.99-.
               05  FILLER          PIC  X(01).
               05  VAT-NO-TAX      PIC  Z(06)9.99-.
               05  FILLER          PIC  X(01).
               05  VAT-TXBL-AMT    PIC  Z(07)9.99-.
               05  FILLER          PIC  X(01).
               05  VAT-VAT-AMT     PIC  Z(06)9.99-.

           03  VAT-L2                            REDEFINES W02-PRN-LINE.
               05  VAT-H1          PIC  X(11).
               05  VAT-H2A.
                   07  VAT-H2      PIC  X(09).
                   07  VAT-H3      PIC  X(12).
               05  VAT-H3A         PIC  X(15).
               05  VAT-H4          PIC  X(13).
               05  VAT-H5          PIC  X(16).
               05  VAT-H6          PIC  X(09).
               05  VAT-H7          PIC  X(11).

           03  VAT-L3                            REDEFINES W02-PRN-LINE.
               05  V-PGE           PIC  X(06).
               05  V-PNO           PIC  Z(03)9.
               05  V-HEADING.
                   07  FILLER      PIC  X(18).
                   07  V-CPY.
                       09 FILLER   PIC  X(08).
                       09 V-HEAD   PIC  X(32).
                   07  V-RATE      PIC  Z9.99.
                   07  V-PERC      PIC  X(01).
                   07  FILLER      PIC  X(06).
               05  V-DATE          PIC  X(06).
               05  V-DTE           PIC  Z9/99/9999.

           03  GP-L1                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(10).
               05  GP-HEAD         PIC  X(17).
               05  GP-HEAD2.
                   07  GP-VALUE    PIC  Z(08)9.99-.
               05  FILLER          PIC  X(01).
               05  GP-PERC         PIC  ZZ9.99-.
               05  GP-P            PIC  X(01).
               05  FILLER          PIC  X(31).

       COPY "W05.VAT".

       01  W10-DATE.
           03  W10-DTE             PIC  99/99/9999.

       01  W10-TAX-KEY.
      *
      *    The first record on the file is the control record
      *    and will have the type "C", taxtype 0 and the date
      *    will contain the start date for the period.
      *
      *    TYPE: "1" = Input Tax
      *          "2" = Output Tax
      *          "3" = Input Payments
      *          "4" = Output Payments
      *
           03  W10-TAX-TYPE        PIC  X(01).
      *
      *    ACTYPE: 1 = Debtors        (Sales / Returns)
      *            2 = Cash           (Sales / Refunds)
      *            3 = Creditors      (Purchases)
      *            4 = General Ledger (Rates, Vehicles ETC)
      *            5 = Petty Cash     (Payments / Refunds)
      *
           03  W10-TAX-ACTYPE      PIC  9(01).
      *
      *    CODE:  This is the transaction code
      *
           03  W10-TAX-CODE        PIC  9(02).
           03  W10-TAX-DATE        PIC  9(08).
           03  W10-TAX-REF         PIC  X(08).
           03  W10-TAX-SEQ         PIC  9(04).
       01  W15-VAT-TOTALS.
           03  W15-TRAN-AMT-1      PIC S9(09)V99 COMP-3.
           03  W15-TRAN-AMT-2      PIC S9(09)V99 COMP-3.
           03  W15-TRAN-AMT-3      PIC S9(09)V99 COMP-3.
           03  W15-TRAN-AMT-I      PIC S9(09)V99 COMP-3.
           03  W15-TRAN-AMT-O      PIC S9(09)V99 COMP-3.
           03  W15-NON-TXBL-1      PIC S9(09)V99 COMP-3.
           03  W15-NON-TXBL-2      PIC S9(09)V99 COMP-3.
           03  W15-NON-TXBL-3      PIC S9(09)V99 COMP-3.
           03  W15-NON-TXBL-I      PIC S9(09)V99 COMP-3.
           03  W15-NON-TXBL-O      PIC S9(09)V99 COMP-3.
           03  W15-VAT-CODES                             OCCURS 12.
               05  W15-TXBL-AMT-1  PIC S9(09)V99 COMP-3.
               05  W15-TXBL-AMT-2  PIC S9(09)V99 COMP-3.
               05  W15-TXBL-AMT-3  PIC S9(09)V99 COMP-3.
               05  W15-TXBL-AMT-I  PIC S9(09)V99 COMP-3.
               05  W15-TXBL-AMT-O  PIC S9(09)V99 COMP-3.
               05  W15-VAT-AMT-1   PIC S9(09)V99 COMP-3.
               05  W15-VAT-AMT-2   PIC S9(09)V99 COMP-3.
               05  W15-VAT-AMT-3   PIC S9(09)V99 COMP-3.
               05  W15-VAT-AMT-I   PIC S9(09)V99 COMP-3.
               05  W15-VAT-AMT-O   PIC S9(09)V99 COMP-3.
       01  W15-HEADINGS.
           03  W15-VAT-TYPE        PIC  X(15).
           03  W15-DESC            PIC  X(15).

       COPY "W15.WS".

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

       01  W24-PERIOD.
           03  FILLER              PIC  X(16)    VALUE "For the period: ".
           03  W24-DTE1            PIC  Z9/99/9999.
           03  FILLER              PIC  X(04)    VALUE " to ".
           03  W24-DTE2            PIC  Z9/99/9999.

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

           03  W25-RECALC                        REDEFINES W25-CALCULATIONS.
               05  W25-FILLER1     PIC 9(05)V99.
               05  W25-R-CASH      PIC S9(09)V99 COMP-3.
               05  W25-R-CHQ       PIC S9(09)V99 COMP-3.
               05  W25-R-CARD      PIC S9(09)V99 COMP-3.
               05  W25-R-SPEED     PIC S9(09)V99 COMP-3.
               05  W25-R-VOUCH     PIC S9(09)V99 COMP-3.
               05  W25-R-CRNOTE    PIC S9(09)V99 COMP-3.
               05  W25-R-TRFRS     PIC S9(09)V99 COMP-3.
               05  W25-R-RECEIPTS  PIC S9(09)V99 COMP-3.
               05  W25-FILLER2     PIC S9(09)V99 COMP-3.
               05  W25-FILLER3     PIC S9(09)V99 COMP-3.
               05  W25-FILLER4     PIC S9(09)V99 COMP-3.

       01  W30-REFERENCE.
           03  W30-START-REF       PIC  9(08).
           03  W30-END-REF         PIC  9(08).

       01  W30-HEADING.
           03  FILLER              PIC  X(12)    VALUE "CASH TAKINGS".
           03  FILLER              PIC  X(07)    VALUE SPACES.
           03  W30-DATE            PIC  Z9/99/9999.

       01  W40-INDIC.
           03  W40-SMAN.
               05  W40-SREP        PIC  9(01)    COMP-5 OCCURS 50.
           03  W40-DRAW.
               05  W40-DREP        PIC  9(01)    COMP-5 OCCURS 40.
           03  W40-EDIT.
               05  W40-KEY         PIC  ZZZZ9.
               05  W40-PERC        PIC  ZZ9.99-.
           03  W40-EDIT2                         REDEFINES W40-EDIT.
               05  FILLER          PIC  X(01).
              05  W40-AMOUNT       PIC  Z(08)9.99-.

       COPY "W40.DBT".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       01  W70-WORK.
           03  W70-REP             PIC 9(04)     COMP-5.

       01  W80-ACCOUNT.
           03  W80-ACNO            PIC 9(05).

       01  W90-TRAN.
           03  W90-DATE            PIC  X(10).
           03  W90-DTE             PIC  99/99/9999 REDEFINES W90-DATE.
           03  W90-CODE            PIC  9(02).
           03  W90-REF             PIC  X(06).
           03  W90-DB              PIC  9(09)V99 COMP-3.
           03  W90-CR              PIC  9(09)V99 COMP-3.
           03  W90-BDT             PIC S9(09)V99 COMP-3.
           03  W90-BCR             PIC S9(09)V99 COMP-3.
           03  W90-DESC            PIC  X(12).
           03  W90-90              PIC S9(09)V99 COMP-3.
           03  W90-60              PIC S9(09)V99 COMP-3.
           03  W90-30              PIC S9(09)V99 COMP-3.
           03  W90-CUR             PIC S9(09)V99 COMP-3.
           03  W90-BAL             PIC S9(09)V99 COMP-3.

       01  W95-STM.
           03  W95-COMP            PIC  X(40).
           03  W95-ADD1            PIC  X(30).
           03  W95-ADD2            PIC  X(30).
           03  W95-ADD3            PIC  X(30).
           03  W95-ADD4            PIC  X(30).
           03  W95-POST            PIC  9(04)    COMP-5.
           03  W95-TEL             PIC  X(08).

       01  W100-DAYS.
           03  W100-VAL.
               05  FILLER          PIC  9(12)    VALUE 000031059090.
               05  FILLER          PIC  9(12)    VALUE 120151181212.
               05  FILLER          PIC  9(12)    VALUE 243273304334.
           03  W100-MONTH                        REDEFINES W100-VAL.
               05  W100-DAY        PIC  9(03)    OCCURS 12.
           03  W100-DAY1           PIC  9(05)    COMP-3.
           03  W100-DAY2           PIC  9(05)    COMP-3.
           03  W100-DATE.
               05  W100-DD         PIC  9(02).
               05  W100-MM         PIC  9(02).
               05  W100-CC         PIC  9(02).
               05  W100-YY         PIC  9(02).
           03  W100-DTE            PIC  9(08)    REDEFINES W100-DATE.
           03  W100-RESULT         PIC  9(05)V99.
           03  W100-R1                           REDEFINES W100-RESULT.
               05  W100-WHOLE      PIC 9(05).
               05  W100-DEC        PIC 9(02).

       01  W200-TRAN.
           03  W200-ACCUM.
               05  W200-LEDG       PIC  X(04)           OCCURS 300.
               05  W200-DT         PIC S9(09)V99 COMP-3 OCCURS 300.
               05  W200-CR         PIC S9(09)V99 COMP-3 OCCURS 300.
               05  W200-DTOT       PIC S9(09)V99 COMP-3.
               05  W200-CTOT       PIC S9(09)V99 COMP-3.
               05  W200-COST       PIC S9(09)V99 COMP-3.
               05  W200-SELL       PIC S9(09)V99 COMP-3.
               05  W200-NET        PIC S9(09)V99 COMP-3.
               05  W200-ADJ        PIC S9(09)V99 COMP-3.

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

       COPY "MENUINST.CRT".

       01  S03.
           03  LINE  5 COLUMN 32 VALUE "DAY-END REPORTS"          FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S04.
           03  BACKGROUND-COLOR Green FOREGROUND-COLOR Black.
               05  LINE  7 COLUMN 29 VALUE "                      ".
               05  LINE  8 COLUMN 29 VALUE "  STATISTICS REPORTS  ".
               05  LINE  9 COLUMN 29 VALUE " 1"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 31 VALUE   " Debtors            ".
               05  LINE 10 COLUMN 29 VALUE " 2"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 31 VALUE   " Creditors          ".
               05  LINE 11 COLUMN 29 VALUE " 3"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 31 VALUE   " Stock              ".
               05  LINE 12 COLUMN 29 VALUE " 4"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 31 VALUE   " All the above      ".
               05  LINE 13 COLUMN 29 VALUE " 0"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 31 VALUE   " Period-End Control ".
               05  LINE 14 COLUMN 29 VALUE "                      ".

       01  S06.
           02  BACKGROUND-COLOR Green FOREGROUND-COLOR Green.
               03  LINE  5 COLUMN 30 VALUE "DAY-END REPORT OPTIONS"           FOREGROUND-COLOR Grey HIGHLIGHT.
               03  LINE  8 COLUMN 24 VALUE "                                " FOREGROUND-COLOR Black.
               03  LINE  9 COLUMN 24 VALUE " 1"                               FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 26 VALUE   " Stock Re-Order               " FOREGROUND-COLOR Black.
               03  LINE 10 COLUMN 24 VALUE " 2"                               FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 26 VALUE   " Debtor Transactions          " FOREGROUND-COLOR Black.
               03  LINE 11 COLUMN 24 VALUE " 3"                               FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 26 VALUE   " Cash Drawers                 " FOREGROUND-COLOR Black.
               03  LINE 12 COLUMN 24 VALUE " 4"                               FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 26 VALUE   " Salesman                     " FOREGROUND-COLOR Black.
               03  LINE 13 COLUMN 24 VALUE " 5"                               FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 26 VALUE   " Turnover and G.P.            " FOREGROUND-COLOR Black.
               03  LINE 14 COLUMN 24 VALUE " 6"                               FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 26 VALUE   " Turnover and G.P. (Detailed) " FOREGROUND-COLOR Black.
               03  LINE 15 COLUMN 24 VALUE " 7"                               FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 26 VALUE   " Debtor Receipts (POS)        " FOREGROUND-COLOR Black.
               03  LINE 16 COLUMN 24 VALUE " 8"                               FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 26 VALUE   " Discounted Sales             " FOREGROUND-COLOR Black.
               03  LINE 17 COLUMN 24 VALUE " 0"                               FOREGROUND-COLOR Grey HIGHLIGHT.
               03          COLUMN 26 VALUE   " Exit from this MENU          " FOREGROUND-COLOR Black.
               03  LINE 18 COLUMN 24 VALUE "                                " FOREGROUND-COLOR Black.

       01  S07.
           03  LINE 11 COLUMN 33 PIC 99/99/9999 USING W15-START      AUTO FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.
           03  LINE 12 COLUMN 33 PIC 99/99/9999 USING W15-END        AUTO FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.

       01  S08.
           03  LINE  8 COLUMN 67 PIC 99/99/9999 USING W15-END        AUTO FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.

       01  S09.
           03  S09S.
               05  LINE  9 COLUMN 45 PIC  Z(07)9 USING W30-START-REF AUTO FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Brown.
           03  S09E.
               05  LINE 10 COLUMN 45 PIC  Z(07)9 USING W30-END-REF   AUTO FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Brown.

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
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS.
       AA00M    SECTION.
       AA00.
           IF LS0-PALEV < 3 AND LS0-SALEV < 9
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AY005.
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

             MOVE 99               TO W02-LINAGE.
      *
      *    ****     S E T   T O   8   L I N E S   P E R   I N C H
      *
             MOVE 8                TO W02-PRN-TYPE.
             PERFORM OPEN-PRINTER.
             MOVE ZERO             TO W02-LINAGE.
             OPEN I-O PARAM LOCK.
             MOVE "P"              TO WS-COMMAND.
             MOVE "D"              TO W02-PRN-TYPE.
             MOVE SPACES           TO R-L1.

       AA01.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66             TO MOUSE-FUNC
               MOVE 1              TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
             PERFORM BA000D.
             CLOSE AUDIT NETWORK DEPART DEBTOR PARAM STOCK.
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
      *    Printer Headings
      *
       AA50.
             MOVE "H"              TO W02-PRN-TYPE.
             MOVE SPACES           TO R-DETAIL.
             MOVE "PAGE:"          TO R-PGE.
             MOVE "DATE:"          TO R-DATE.
             ADD 1                 TO WS-PAGE.
             MOVE WS-PAGE          TO R-PNO.
             MOVE W12-TODAY        TO R-DTE.
             MOVE W95-COMP         TO R-CPY.
             MOVE 2                TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES  TO R-DETAIL.

       AA55.
             EXIT.
      *
      *    Printer Headings
      *
       AA60.
             MOVE "H"              TO W02-PRN-TYPE.
             MOVE SPACES           TO R-DETAIL.
             MOVE "PAGE:"          TO RC-PGE.
             MOVE "DATE:"          TO RC-DATE.
             ADD 1                 TO WS-PAGE.
             MOVE WS-PAGE          TO RC-PNO.
             MOVE W12-TODAY        TO RC-DTE.
             MOVE W95-COMP         TO RC-CPY.
             MOVE 2                TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES           TO R-DETAIL.

       AA65.
             EXIT.

       COPY "AA900.ALM".

       COPY "DATE.CHK".

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
      *    ³ 20 (default); move another VALUE to WS-MES-LINE.          ³
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
      *    ³       [see Accptopt for instruction VALUEs]               ³
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
      *    ³ as the VALUE of WS-MES-LINE. If WS-MES-LINE is zero the   ³
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
      *    ³           R E A D   F I L E S   R O U T I N E S           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AN00-READ        SECTION.
      *
      *      *   *   * ****  *** *****     ***** *** *     *****
      *     * *  *   * *   *  *    *       *      *  *     *
      *    *   * *   * *   *  *    *       *      *  *     *
      *    ***** *   * *   *  *    *       ***    *  *     ***
      *    *   * *   * *   *  *    *       *      *  *     *
      *    *   * *   * *   *  *    *       *      *  *     *
      *    *   *  ***  ****  ***   *       *     *** ***** *****
      *
       READ-AUDIT.
             READ AUDIT.
           IF WS-STATUS = "00"
               MOVE ZERO             TO WS-F-ERROR
               GO TO READ-AUDIT-EXIT.
           IF WS-STATUS = 23
               IF (WS-AUDKEY NOT = 1) AND (WS-AUDKEY < W70-REP)
                   ADD 1             TO WS-AUD WS-AUDKEY
                   GO TO READ-AUDIT.
             MOVE ZERO               TO WS-AUD.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 1                 TO WS-F-ERROR
              PERFORM READ-ERROR.
           IF RECORD-LOCKED
               PERFORM LOCKED-RECORD
               GO TO READ-AUDIT.
             MOVE 1                  TO WS-F-ERROR.
             PERFORM READ-ERROR.

       READ-AUDIT-EXIT.
             EXIT.
      *
      *          ***   ***  *   * ***** ****   ***  *             *
      *         *   * *   * *   *   *   *   * *   * *            *
      *         *     *   * **  *   *   *   * *   * *           *
      *         *     *   * * * *   *   ****  *   * *          *
      *         *     *   * *  **   *   *   * *   * *         *
      *         *   * *   * *   *   *   *   * *   * *        *
      *          ***   ***  *   *   *   *   *  ***  *****   *
      *
      *  *   * ***** ***** *   *  ***  ****  *   *     ***** *** *     *****
      *  *   * *       *   *   * *   * *   * *  *      *      *  *     *
      *  **  * *       *   *   * *   * *   * * *       *      *  *     *
      *  * * * ***     *   * * * *   * ****  **        ***    *  *     ***
      *  *  ** *       *   * * * *   * *   * * *       *      *  *     *
      *  *   * *       *   * * * *   * *   * *  *      *      *  *     *
      *  *   * *****   *    * *   ***  *   * *   *     *     *** ***** *****
      *
       READ-CONTROL.
             READ NETWORK.
           IF WS-STATUS = "00"
               MOVE ZERO             TO WS-F-ERROR
               GO TO READ-CONTROL-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 2                TO WS-F-ERROR
               PERFORM READ-ERROR.
           IF RECORD-LOCKED
               PERFORM LOCKED-RECORD
               GO TO READ-CONTROL.
             MOVE 2                  TO WS-F-ERROR.
             PERFORM READ-ERROR.

       READ-CONTROL-EXIT.
             EXIT.
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
      *    ****  ***** ****  *****  ***  ****     ***** *** *     *****
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *    *   * ***   ****    *   *   * ****     ***    *  *     ***
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *    ****  ***** ****    *    ***  *   *    *     *** ***** *****
      *
       READ-DEBTOR.
             READ DEBTOR WITH IGNORE LOCK KEY IS DEB-ACNO.
             GO TO READ-DEBTOR-STATUS.

       READ-DEBTOR-NEXT.
             READ DEBTOR NEXT WITH IGNORE LOCK.
             GO TO READ-DEBTOR-STATUS.

       START-AT-DEBT-AC.
             START DEBTOR KEY >= DEB-ACNO.
             GO TO READ-DEBTOR-STATUS.

       READ-DEBTOR-STATUS.
           IF WS-STATUS = "00"
               MOVE ZERO             TO WS-F-ERROR
               GO TO READ-DEBTOR-EXIT.
           IF (WS-STATUS = "23") OR (WS-STAT1 = "1")
               MOVE 6                TO WS-F-ERROR
               GO TO READ-DEBTOR-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 6                TO WS-F-ERROR
               PERFORM READ-ERROR.

       READ-DEBTOR-EXIT.
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
      *     ***  *****  ***   ***  *   *     ***** ***** *     *****
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *    *       *   *   * *     * *       *       *   *     *
      *     ***    *   *   * *     **        ***     *   *     ***
      *        *   *   *   * *     * *       *       *   *     *
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *     ***    *    ***   ***  *   *     *     ***** ***** *****
      *
       READ-STOCK.
             READ STOCK WITH IGNORE LOCK KEY IS STK-CODE.
           IF WS-STATUS = "00"
               IF STK-DISC NOT = 9
                   MOVE ZERO         TO WS-F-ERROR
                   GO TO READ-STOCK-EXIT
               ELSE
                   MOVE 22           TO WS-F-ERROR
                   GO TO READ-STOCK-EXIT.
             GO TO STOCK-CHECK-STATUS.

       READ-STOCK-NEXT.
             READ STOCK NEXT WITH IGNORE LOCK.
           IF WS-STATUS = "00"
               IF STK-DISC NOT = 9
                   MOVE ZERO         TO WS-F-ERROR
                   GO TO READ-STOCK-EXIT
               ELSE
                   GO TO READ-STOCK-NEXT.
             GO TO STOCK-CHECK-STATUS.

       START-AT-STOCK-CODE.
             START STOCK KEY >= STK-CODE.
             GO TO STOCK-CHECK-STATUS.

       STOCK-CHECK-STATUS.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
               GO TO READ-STOCK-EXIT.
           IF (WS-STATUS = "23") OR (WS-STAT1 = "1")
               MOVE 22           TO WS-F-ERROR
               GO TO READ-STOCK-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 22           TO WS-F-ERROR
               PERFORM READ-ERROR.

       READ-STOCK-EXIT.
             EXIT.
      *
      *
      *
       BA000D        SECTION 5.
       BA00C.
             PERFORM ERASE-SCREEN.
             MOVE 1                  TO WS-S6.

       BA00C-LOOP.
            MOVE ZERO                TO W40-SREP(WS-S6) W40-DREP(WS-S6).
           IF WS-S6 < 40
               ADD 1                 TO WS-S6
               GO TO BA00C-LOOP.
             MOVE 41                 TO WS-S6.
             DISPLAY S03.

       BA00C-LP.
             MOVE ZERO               TO W40-SREP(WS-S6).
           IF WS-S6 < 50
               ADD 1                 TO WS-S6
               GO TO BA00C-LP.
             DISPLAY "Do you wish to clear daily statistics after printing Y/N" AT 0404.
             ACCEPT WS-OPTION AT 0461 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF WS-OPTION = "Y"
               MOVE "C"              TO LS-CLR.
             PERFORM SAVE-SCREEN-3.
             PERFORM BX000.
           IF WS-REP1 = 1
               PERFORM BA00R.

       BA00C-MENU.
             PERFORM RESTORE-SCREEN-3.
             MOVE 8                  TO SHADE-ROW.
             MOVE 24                 TO SHADE-COL.
             MOVE 32                 TO SHADE-WIDTH.
             MOVE 11                 TO SHADE-LINES.
             DISPLAY S06.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.

       BA00D.
             MOVE 8                  TO TOP-ROW.
             MOVE 16                 TO BOTTOM-ROW.
             MOVE 8                  TO SCREEN-LIN.
             MOVE 25                 TO SCREEN-COL.
             MOVE 29                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       BA00D-ACCEPT.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BA00D-10.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY       PERFORM MOVE-BLOCK-UP
                 WHEN DOWN-KEY     PERFORM MOVE-BLOCK-DOWN
                 WHEN MOUSE-KEY    PERFORM POSITION-MOUSE
                                   IF MOUSE-ENTER = "Y"
                                       MOVE "N"    TO MOUSE-ENTER
                                       GO TO BA00D-15
                                   END-IF
                                   IF MOUSE-Y = 49
                                       IF MOUSE-X = 5
                                           MOVE 5  TO KEY-CODE-1
                                           GO TO BA00D-10
                                       ELSE
                                       IF MOUSE-X = 10
                                           MOVE 6  TO KEY-CODE-1
                                           GO TO BA00D-10
                                       ELSE
                                       IF MOUSE-X > 36 AND < 44
                                           MOVE 0  TO KEY-CODE-1
                                           GO TO BA00D-10
                                       ELSE
                                       IF MOUSE-X > 54 AND < 60
                                           MOVE 0  TO KEY-CODE-1
                                           MOVE 1  TO KEY-TYPE
                                           GO TO BA00D-10
                                       END-IF
                                   END-IF
                 WHEN ENTER-KEY    GO TO BA00D-15
               END-EVALUATE
               GO TO BA00D-ACCEPT
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY   MOVE "0" TO WS-OPTION
                                GO TO BA00DF
               END-EVALUATE
               GO TO BA00D-ACCEPT
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X  TO WS-OPTION
               CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS
               IF WS-OPTION < "0" OR > "8"
                   GO TO BA00D-ACCEPT
               END-IF
               GO TO BA00D-16
           ELSE
               GO TO BA00D-ACCEPT.

       BA00D-15.
             EVALUATE SCREEN-LIN
               WHEN 8      MOVE "1"  TO WS-OPTION
               WHEN 9      MOVE "2"  TO WS-OPTION
               WHEN 10     MOVE "3"  TO WS-OPTION
               WHEN 11     MOVE "4"  TO WS-OPTION
               WHEN 12     MOVE "5"  TO WS-OPTION
               WHEN 13     MOVE "6"  TO WS-OPTION
               WHEN 14     MOVE "7"  TO WS-OPTION
               WHEN 15     MOVE "8"  TO WS-OPTION
               WHEN 16     MOVE "0"  TO WS-OPTION
               WHEN OTHER  GO TO BA00D-ACCEPT
             END-EVALUATE.
             GO TO BA00DF.

       BA00D-16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "0"    MOVE 16   TO SCREEN-LIN
               WHEN "1"    MOVE 8    TO SCREEN-LIN
               WHEN "2"    MOVE 9    TO SCREEN-LIN
               WHEN "3"    MOVE 10   TO SCREEN-LIN
               WHEN "4"    MOVE 11   TO SCREEN-LIN
               WHEN "5"    MOVE 12   TO SCREEN-LIN
               WHEN "6"    MOVE 13   TO SCREEN-LIN
               WHEN "7"    MOVE 14   TO SCREEN-LIN
               WHEN "8"    MOVE 15   TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       BA00DF.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66               TO MOUSE-FUNC
               MOVE 0                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
             DISPLAY CLEAR-L50.
             EVALUATE WS-OPTION
               WHEN "0"    GO TO BA00E
               WHEN "1"    IF WS-REP2 = 1
                               PERFORM BB00R
                           ELSE
                               MOVE "No re-orders to report"        TO WS-ERR-MES
                               PERFORM ERROR-MESSAGE
               WHEN "2"    IF WS-REP3 = 1 OR WS-REP4 = 1
                               PERFORM BC00R
                           ELSE
                               MOVE "No Transactions to report"      TO WS-ERR-MES
                               PERFORM ERROR-MESSAGE
               WHEN "3"    IF WS-REP4 = 1
                               PERFORM BE00R
                           ELSE
                               MOVE "No Cash draw details to report" TO WS-ERR-MES
                               PERFORM ERROR-MESSAGE
               WHEN "4"    IF WS-REP3 = 1 OR WS-REP4 = 1
                               PERFORM BF00R
                           ELSE
                               MOVE "No Salesman details to report"  TO WS-ERR-MES
                               PERFORM ERROR-MESSAGE
               WHEN "5"    IF WS-REP3 = 1 OR WS-REP4 = 1
                               PERFORM BG00R
                           ELSE
                               MOVE "No Turnover to report"          TO WS-ERR-MES
                               PERFORM ERROR-MESSAGE
               WHEN "6"    IF WS-REP3 = 1 OR WS-REP4 = 1
                               PERFORM BH00R
                           ELSE
                               MOVE "No Turnover to report"          TO WS-ERR-MES
                               PERFORM ERROR-MESSAGE
               WHEN "7"    IF WS-REP11 = 1
                               PERFORM BI00R
                           ELSE
                               MOVE "No Debtor receipts to report"   TO WS-ERR-MES
                               PERFORM ERROR-MESSAGE
               WHEN "8"    IF WS-REP3 = 1
                               PERFORM BJ00R
                           ELSE
                               MOVE "No Transactions to report"      TO WS-ERR-MES
                               PERFORM ERROR-MESSAGE
             END-EVALUATE.
             GO TO BA00D-ACCEPT.

       BA00E.
           IF WS-PRINTER = "S"
               PERFORM RESTORE-SCREEN-3
               MOVE "I"              TO WS-COMMAND
               PERFORM CALL-PRINTUTL
               MOVE "P"              TO WS-COMMAND.
             PERFORM RESTORE-SCREEN-3.
             MOVE 6                  TO SHADE-ROW.
             MOVE 29                 TO SHADE-COL.
             MOVE 22                 TO SHADE-WIDTH.
             MOVE 8                  TO SHADE-LINES.
             DISPLAY S04.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.

       BA00E-2.
             MOVE 8                  TO TOP-ROW.
             MOVE 12                 TO BOTTOM-ROW.
             MOVE 7                  TO SCREEN-LIN.
             MOVE 30                 TO SCREEN-COL.
             MOVE 19                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       BA00E-5.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BA00E-10.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     PERFORM MOVE-BLOCK-UP
                 WHEN DOWN-KEY   PERFORM MOVE-BLOCK-DOWN
                 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
                                 IF MOUSE-ENTER = "Y"
                                     MOVE "N"  TO MOUSE-ENTER
                                     GO TO BA00E-15
                                 END-IF
                                 IF MOUSE-Y = 49
                                     IF MOUSE-X = 5
                                         MOVE 5  TO KEY-CODE-1
                                         GO TO BA00E-10
                                     ELSE
                                     IF MOUSE-X = 10
                                         MOVE 6  TO KEY-CODE-1
                                         GO TO BA00E-10
                                     ELSE
                                     IF MOUSE-X > 36 AND < 44
                                         MOVE 0  TO KEY-CODE-1
                                         GO TO BA00E-10
                                     ELSE
                                     IF MOUSE-X > 54 AND < 60
                                         MOVE 0  TO KEY-CODE-1
                                         MOVE 1  TO KEY-TYPE
                                         GO TO BA00E-10
                                     END-IF
                                 END-IF
                 WHEN ENTER-KEY  GO TO BA00E-15
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO BA00E-5
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY   MOVE "0"  TO WS-OPTION
                                GO TO BA00F
                 WHEN OTHER     PERFORM AA900-ALARM
               END-EVALUATE
               GO TO BA00E-5
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X      TO WS-OPTION
               IF WS-OPTION < "0" OR > "4"
                   PERFORM AA900-ALARM
                   GO TO BA00E-5
               END-IF
               GO TO BA00E-16
           ELSE
               PERFORM AA900-ALARM
               GO TO BA00E-5.
          
       BA00E-15.
             EVALUATE SCREEN-LIN
               WHEN 7     MOVE "1"   TO WS-OPTION
               WHEN 8     MOVE "2"   TO WS-OPTION
               WHEN 9     MOVE "3"   TO WS-OPTION
               WHEN 10    MOVE "4"   TO WS-OPTION
               WHEN 11    MOVE "0"   TO WS-OPTION
               WHEN OTHER
             GO TO BA00E-5
             END-EVALUATE.
             GO TO BA00F.

       BA00E-16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "0"    MOVE 11   TO SCREEN-LIN
               WHEN "1"    MOVE 7    TO SCREEN-LIN
               WHEN "2"    MOVE 8    TO SCREEN-LIN
               WHEN "3"    MOVE 9    TO SCREEN-LIN
               WHEN "4"    MOVE 10   TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       BA00F.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66               TO MOUSE-FUNC
               MOVE 0                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
           IF WS-OPTION = "0"
               GO TO BA99.
             MOVE WS-OPTION          TO WS-STATOP.
             PERFORM BZ00R.
             PERFORM RESTORE-SCREEN-3.
             GO TO BA00E.

       BA99.
             EXIT.
      /
       BA00R          SECTION 5.
       BA00.
             MOVE 0701               TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
             DISPLAY "Busy printing Lost Sales Report" AT 1220 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             MOVE ZERO               TO W25-TOTAL.
             MOVE 2                  TO WS-AUDKEY.
             MOVE ZERO               TO WS-PAGE.

       BA05.
             PERFORM AA50 THRU AA55.
             MOVE "LOST SALES"       TO R-HEAD.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE WS-H7              TO R-H1.
             MOVE WS-H8              TO R-H2.
             MOVE WS-H9              TO R-H3.
             MOVE "UNIT PRICE"       TO R-H4.
             MOVE WS-H11             TO R-H5.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "D"                TO W02-PRN-TYPE.

       BA10.
             PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
           IF NOT(WS-F-ERROR = ZERO)
               MOVE ZERO             TO WS-F-ERROR
               GO TO BA15.
           IF AUD-TYPE NOT = 1
               GO TO BA15.
             MOVE AUD-EXT-CODE       TO STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               MOVE "Item deleted or code changed"    TO STK-DESC.
             MOVE STK-CODE           TO R-I1.
             MOVE STK-DESC           TO R-DESC1.
             MOVE AUD-QNT            TO R-Q1.
             MOVE STK-SELL           TO R-UP1.
             MULTIPLY STK-SELL BY AUD-QNT GIVING W25-VALUE.
             MOVE W25-VALUE          TO R-V1.
             ADD W25-VALUE           TO W25-TOTAL.
           IF W02-LINAGE < W02-PRN-LENGTH
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM BA05.
             MOVE SPACES             TO R-DETAIL.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****"  TO R-DETAIL
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE "P"              TO WS-COMMAND
               GO TO BA999.
       BA15.
           IF WS-AUDKEY < W70-REP
               ADD 1                 TO WS-AUDKEY
               GO TO BA10.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE "TOTAL LOST SALES" TO R-DESC1.
             MOVE W25-TOTAL          TO R-V1.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE WS-H1              TO R-DETAIL.
             MOVE 99                 TO WS-ADVANCE.
             MOVE "E"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             MOVE "P"                TO WS-COMMAND.
             MOVE SPACES             TO R-DETAIL.

       BA999.
             EXIT.
      /
       BB00R          SECTION 50.
       BB00.
             PERFORM RESTORE-SCREEN-3.
             DISPLAY "Printing Stock ReOrder Report" AT 1220 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             MOVE ZERO               TO WS-PAGE.
             MOVE 2                  TO WS-AUDKEY.

       BB05.
      *
      *    ****    SET PRINTER TO 10 CPI
      *
           IF NOT(WS-PRINTER = "S")
               MOVE 0                TO WS-ADVANCE
               MOVE 1                TO W02-PRN-TYPE
               PERFORM CALL-PRINTUTL.
             PERFORM AA50 THRU AA55.
             MOVE "STOCK LOW LEVELS" TO R-HEAD.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
      *
      *    ****    SET PRINTER TO 12 CPI
      *
           IF NOT(WS-PRINTER = "S")
               MOVE 0                TO WS-ADVANCE
               MOVE 2                TO W02-PRN-TYPE
               PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE WS-H7              TO R-H6.
             MOVE WS-H8              TO R-H7.
             MOVE "LEVEL"            TO R-H8.
             MOVE WS-H9              TO R-H9.
             MOVE "COST"             TO R-H10.
             MOVE "AVERAGE"          TO R-H11.
             MOVE WS-H13             TO R-H12.
             MOVE WS-H14             TO R-H13.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "D"                TO W02-PRN-TYPE.

       BB10.
             PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
           IF NOT(WS-F-ERROR = ZERO)
               MOVE ZERO             TO WS-F-ERROR
               GO TO BB15.
           IF AUD-TYPE NOT = 02
               GO TO BB15.
             MOVE AUD-EXT-CODE       TO STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               MOVE "Item deleted or code changed" TO STK-DESC.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Is a 3 character extension code being used in addition to ³
      *    ³ the 14 character stock/inventory item code?        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF WS-USE-ITM = "Y"
               MOVE STK-ITEM         TO R-ITM
               MOVE "/"              TO R-SL
               MOVE STK-ITM          TO R-EXT
           ELSE
               MOVE STK-CODE         TO R-I2.
             MOVE STK-DESC           TO R-DESC2.
             MOVE STK-LEVEL          TO R-LEV.
             MOVE STK-QUANT          TO R-Q2.
             MOVE STK-COST           TO R-COST1.
             MOVE STK-AVGE           TO R-AVGE.
             MOVE STK-MTD            TO R-NMTD.
             MOVE STK-YTD            TO R-NYTD.
           IF W02-LINAGE < W02-PRN-LENGTH
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM BB05.
             MOVE SPACES             TO R-DETAIL.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****"  TO R-DETAIL
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE "P"              TO WS-COMMAND
               GO TO BB900.
       BB15.
           IF WS-AUDKEY < W70-REP
               ADD 1                 TO WS-AUDKEY
               GO TO BB10.
             MOVE WS-H1              TO R-DETAIL.
             MOVE 99                 TO WS-ADVANCE.
             MOVE "E"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             MOVE "P"                TO WS-COMMAND.
             MOVE SPACES             TO R-DETAIL.

       BB900.
      *
      *    ****    RESET THE PRINTER CONTROLS
      *
           IF NOT(WS-PRINTER = "S")
               MOVE 0                TO WS-ADVANCE
               MOVE 1                TO W02-PRN-TYPE
               PERFORM CALL-PRINTUTL.

       BB999.
             EXIT.
      /
       BC00R          SECTION 5.
       BC00.
             PERFORM RESTORE-SCREEN-3.
             DISPLAY "Printing Sales Transactions" AT 1220 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             MOVE 1                  TO WS-S6.

       BC02.
             MOVE SPACES             TO W200-LEDG(WS-S6).
             MOVE ZERO               TO W200-DT(WS-S6) W200-CR(WS-S6).
           IF WS-S6 < 300
               ADD 1                 TO WS-S6
               GO TO BC02.
             MOVE ZERO               TO W200-DTOT W200-CTOT W200-COST W200-SELL W200-NET.
             MOVE ZERO               TO WS-PAGE.
             MOVE 2                  TO WS-AUDKEY.

       BC05.
             PERFORM AA50 THRU AA55.
             MOVE "DEBTOR TRANSACTIONS" TO R-HEAD.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "AC-NO"            TO R-H14.
             MOVE "DEBTOR NAME"      TO R-H14A.
             MOVE "REF. NO."         TO R-H15.
             MOVE "INVOICE"          TO R-H16.
             MOVE "C/NOTE"           TO R-H17.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "D"                TO W02-PRN-TYPE.

       BC10.
             PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
           IF NOT(WS-F-ERROR = ZERO)
               MOVE ZERO             TO WS-F-ERROR
               GO TO BC15.
           IF NOT(AUD-TYPE = 03)
               GO TO BC15.
             MOVE AUD-AC             TO R-AC W80-ACNO DEB-ACNO.
             PERFORM BK000
             MOVE W40-DSP-NAME       TO R-DNME.
             MOVE AUD-REF            TO R-REF.
           IF AUD-VAL < ZERO
               MOVE AUD-VAL          TO R-CASH
               ADD AUD-VAL           TO W200-CTOT
           ELSE
               ADD AUD-VAL           TO W200-DTOT
               MOVE AUD-VAL          TO R-DEBT.
           IF W02-LINAGE < W02-PRN-LENGTH
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM BC05.
             MOVE SPACES             TO R-DETAIL.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****" TO R-DETAIL
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE "P"              TO WS-COMMAND
               GO TO BC999.
       BC15.
           IF WS-AUDKEY < W70-REP
               ADD 1                 TO WS-AUDKEY
               GO TO BC10.
           IF W02-LINAGE < W02-PRN-LENGTH
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM BC05.
             MOVE WS-H4              TO R-H15.
             MOVE W200-DTOT          TO R-DEBT.
             MOVE W200-CTOT          TO R-CASH.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE WS-H1              TO R-DETAIL.
             MOVE 99                 TO WS-ADVANCE.
             MOVE "E"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             MOVE "P"                TO WS-COMMAND.
             MOVE SPACES             TO R-DETAIL.

       BC999.
             EXIT.
      /
       BE00R           SECTION 5.
       BE00.
            PERFORM RESTORE-SCREEN-3.
            DISPLAY "Busy printing Cash Draw Report" AT 1220 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
            MOVE ZERO                TO W25-COST1 W25-COST2 W25-COST3 W25-SELL1 W25-SELL2 W25-SELL3 W25-TOTAL.
      *
      *    ****    SET PRINTER TO 12 CPI
      *
           IF NOT(WS-PRINTER = "S")
               MOVE 0                TO WS-ADVANCE
               MOVE 2                TO W02-PRN-TYPE
               PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DETAIL.
             MOVE ZERO               TO WS-PAGE.
             MOVE 1                  TO WS-NO.
       BE05.
           IF W40-DREP(WS-NO) = 1
               GO TO BE10.
       BE06.
           IF WS-NO < 40
               ADD 1                 TO WS-NO
               GO TO BE05.
      *
      *    ****    P R I N T   G R A N D   T O T A L
      *
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE "******"           TO SAL-H7.
             MOVE "TOTAL FOR ALL DRAWS" TO SAL-H8.
             MOVE W25-COST3          TO SAL-COST.
             MOVE W25-SELL3          TO SAL-SELL.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "  **  GROSS PROFIT" TO SAL-H8.
             COMPUTE W25-SELL1 = W25-SELL3 - W25-COST3.
             COMPUTE W25-COST1 = (W25-SELL1 / W25-SELL3) * 100.000 + 0.005.
             MOVE W25-SELL1          TO SAL-SELL.
             MOVE W25-COST1          TO SAL-GP.
             COMPUTE W25-COST1 = (W25-SELL1 / W25-COST3) * 100.000 + 0.005.
             MOVE W25-COST1          TO SAL-MARKUP.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "NETT SALES + TAX" TO SAL-H8.
             MOVE W25-TOTAL          TO SAL-SELL.
             MOVE 99                 TO WS-ADVANCE.
             MOVE "E"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             MOVE "P"                TO WS-COMMAND.
             MOVE SPACES             TO R-DETAIL.
      *
      *    ****    RESET THE PRINTER CONTROLS
      *
           IF NOT(WS-PRINTER = "S")
               MOVE 0                TO WS-ADVANCE
               MOVE 1                TO W02-PRN-TYPE
               PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DETAIL.
             GO TO BE999.

       BE10.
           IF NOT(WS-PRINTER = "S")
               IF WS-PAGE > ZERO
                   MOVE SPACES       TO R-DETAIL.
                   MOVE 99           TO WS-ADVANCE
                   PERFORM CALL-PRINTUTL.
             MOVE 2                  TO WS-AUDKEY.

       BE15.
             PERFORM AA50 THRU AA55.
             MOVE "C A S H   D R A W   R E P O R T" TO R-CPY.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "CASHDRAW:"        TO SAL-H1.
             MOVE WS-NO              TO SAL-NO.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "ITEM CODE"        TO SAL-H2.
             MOVE "DESCRIPTION"      TO SAL-H3.
             MOVE "COST"             TO SAL-H4.
             MOVE "SELLING"          TO SAL-H5.
             MOVE "G.P.%"            TO SAL-H6.
             MOVE "MARKUP"           TO SAL-H6A.
             MOVE "PW"               TO SAL-H6B.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "D"                TO W02-PRN-TYPE.

       BE20.
             PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
           IF NOT(WS-F-ERROR = ZERO)
               MOVE ZERO             TO WS-F-ERROR
               GO TO BE30.
           IF NOT(AUD-TYPE = 4 OR 6)
               GO TO BE30.
           IF AUD-SUB NOT = WS-NO
               GO TO BE30.
           IF AUD-TYPE = 4
               GO TO BE25.
      *
      *    ****    P R I N T   I N D V I D U A L   I T E M S
      *
             MOVE AUD-EXT-STOCK  TO STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               MOVE "Item deleted or code changed" TO STK-DESC.
             MOVE STK-CODE           TO SAL-ITEM.
             MOVE STK-DESC           TO SAL-DESC.
             MOVE AUD-CST            TO SAL-COST.
             MOVE AUD-SEL            TO SAL-SELL.
             COMPUTE SAL-GP = ((AUD-SEL - AUD-CST) / AUD-SEL) * 100.000 + 0.005.
             COMPUTE SAL-MARKUP = ((AUD-SEL - AUD-CST) / AUD-CST) * 100.000 + 0.005.
             ADD AUD-CST             TO W25-COST1 W25-COST2 W25-COST3.
             ADD AUD-SEL             TO W25-SELL1 W25-SELL2 W25-SELL3.
           IF AUD-SUPER = 99
               MOVE "N/C"            TO SAL-NOCHARGE
           ELSE
               MOVE AUD-SUPER        TO SAL-SUPER.
           IF W02-LINAGE < W02-PRN-LENGTH
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM BE15.
             MOVE SPACES             TO R-DETAIL.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****" TO SAL-L1
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE "P"              TO WS-COMMAND
               GO TO BE999.
             GO TO BE30.
      *
      *    ****    P R I N T   S A L E   T O T A L
      *
       BE25.
             MOVE "REFERENCE NO:"    TO SAL-H7.
             MOVE AUD-REF  TO SAL-REF.
             MOVE "CASH SALE TOTAL"  TO SAL-H8.
             MOVE W25-COST1          TO SAL-COST.
             MOVE W25-SELL1          TO SAL-SELL.
             COMPUTE SAL-GP = ((W25-SELL1 - W25-COST1) / W25-SELL1) * 100.000 + 0.005.
             COMPUTE SAL-MARKUP = ((W25-SELL1 - W25-COST1) / W25-COST1) * 100.000 + 0.005.
             EVALUATE AUD-PAYMENT 
               WHEN  "C"   MOVE "CSH"  TO SAL-PAY
               WHEN  "D"   MOVE "CRD"  TO SAL-PAY
               WHEN  "N"   MOVE "CRN"  TO SAL-PAY
               WHEN  "Q"   MOVE "CHQ"  TO SAL-PAY
               WHEN  "S"   MOVE "SPD"  TO SAL-PAY
               WHEN  "V"   MOVE "VCH"  TO SAL-PAY
             END-EVALUATE.
             MOVE ZERO               TO W25-SELL1 W25-COST1.
           IF W02-LINAGE < W02-PRN-LENGTH
               MOVE 2                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM BE15.
             MOVE SPACES             TO R-DETAIL.
      *
      *    ****    C H E C K   E N D   O F   A U D I T   F I L E
      *
       BE30.
           IF WS-AUDKEY < W70-REP
               ADD 1                 TO WS-AUDKEY
               GO TO BE20.
             ADD 150 WS-NO           GIVING WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             ADD PAR-CSH             TO W25-TOTAL.
             ADD PAR-VATVAL          TO W25-TOTAL.
      *
      *    ****    P R I N T   C A S H   D R A W   T O T A L
      *
           IF W02-LINAGE < (W02-PRN-LENGTH - 6)
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM BE15.
             MOVE SPACES             TO R-DETAIL.
             MOVE "   ***"           TO SAL-H7.
             MOVE "CASH DRAW TOTAL"  TO SAL-H8.
             MOVE W25-COST2          TO SAL-COST.
             MOVE W25-SELL2          TO SAL-SELL.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "  GROSS PROFIT"   TO SAL-H8.
             COMPUTE W25-SELL1 = W25-SELL2 - W25-COST2.
             COMPUTE W25-COST1 = (W25-SELL1 / W25-SELL2) * 100.000 + 0.005.
             MOVE W25-SELL1          TO SAL-SELL.
             MOVE W25-COST1          TO SAL-GP.
             COMPUTE W25-COST1 = (W25-SELL1 / W25-COST2) * 100.000 + 0.005.
             MOVE W25-COST1          TO SAL-MARKUP.
             PERFORM CALL-PRINTUTL.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****"  TO SAL-L1
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE "P"              TO WS-COMMAND
               GO TO BE999.
             MOVE SPACES             TO R-DETAIL.
             MOVE ZERO               TO W25-SELL1 W25-SELL2 W25-COST1 W25-COST2.
             GO TO BE06.

       BE999.
             EXIT.
      /
       BF00R           SECTION 50.
       BF00.
             PERFORM RESTORE-SCREEN-3.
             DISPLAY "Busy printing Salesman Report" AT 1220 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             MOVE ZERO               TO W25-COST1 W25-COST2 W25-COST3 W25-SELL1 W25-SELL2 W25-SELL3.
             MOVE ZERO               TO WS-PAGE.
             MOVE 1                  TO WS-NO.

       BF05.
           IF W40-SREP(WS-NO) = 1
               GO TO BF10.
       BF06.
           IF WS-NO < 50
               ADD 1                 TO WS-NO
               GO TO BF05.
      *
      *    ****    P R I N T   G R A N D   T O T A L
      *
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "  TOTAL - ALL SALESMEN" TO SAL-L5.
             MOVE W25-COST3          TO SAL-CST.
             MOVE W25-SELL3          TO SAL-SEL.
             COMPUTE W25-SELL1 = W25-SELL3 - W25-COST3.
             COMPUTE W25-COST1 = (W25-SELL1 / W25-SELL3) * 100.000 + 0.005.
             MOVE W25-SELL1          TO SAL-PRF.
             MOVE W25-COST1          TO SAL-GPR.
             MOVE 99                 TO WS-ADVANCE.
             MOVE "E"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             MOVE "P"                TO WS-COMMAND.
             MOVE SPACES             TO R-DETAIL.
             GO TO BF999.

       BF10.
           IF NOT(WS-PRINTER = "S")
               IF WS-PAGE > ZERO
                   MOVE SPACES       TO R-DETAIL
                   MOVE 99           TO WS-ADVANCE
                   PERFORM CALL-PRINTUTL.
             MOVE 2                  TO WS-AUDKEY.
             ADD 100 WS-NO           GIVING WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE ZERO               TO W25-CASH W25-REFF W25-INV W25-CRN.

       BF15.
             PERFORM AA50 THRU AA55.
             MOVE "S A L E S M A N   R E P O R T" TO R-CPY.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "SALESMAN:"        TO SAL-H1.
             MOVE WS-NO              TO SAL-NO.
             MOVE PAR-SALESMAN       TO SAL-SMAN.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "REF NO"           TO SAL-H9.
             MOVE "DESCRIPTION"      TO SAL-H10.
             MOVE " COST"            TO SAL-H11.
             MOVE " SELLING"         TO SAL-H12.
             MOVE " PROFIT"          TO SAL-H13.
             MOVE "G.P.%"            TO SAL-H14.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "D"                TO W02-PRN-TYPE.

       BF20.
             PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
           IF NOT(WS-F-ERROR = ZERO)
               MOVE ZERO             TO WS-F-ERROR
               GO TO BF30.
           IF NOT(AUD-TYPE = 5 OR 7)
               GO TO BF30.
           IF AUD-SMAN NOT = WS-NO
               GO TO BF30.
      *
      *    ****    P R I N T   I N D V I D U A L   S A L E S
      *
             MOVE AUD-CSH  TO SAL-RF.
           IF AUD-TYPE = 5
               IF AUD-SELL < ZERO
                   MOVE "CASH REFUND" TO SAL-DSC
                   ADD AUD-SELL      TO W25-REFF
               ELSE
                  MOVE "CASH SALE"   TO SAL-DSC
                  ADD AUD-SELL       TO W25-CASH
               END-IF
           ELSE
           IF AUD-SELL < ZERO
               MOVE "CREDIT NOTE"    TO SAL-DSC
               ADD AUD-SELL          TO W25-CRN
           ELSE
               MOVE "INVOICE"        TO SAL-DSC
               ADD AUD-SELL          TO W25-INV.
             MOVE AUD-COST           TO SAL-CST.
             MOVE AUD-SELL           TO SAL-SEL.
             COMPUTE SAL-PRF = AUD-SELL - AUD-COST.
             COMPUTE SAL-GPR = ((AUD-SELL - AUD-COST) / AUD-SELL) * 100.000 + 0.005.
             ADD AUD-COST            TO W25-COST2 W25-COST3.
             ADD AUD-SELL            TO W25-SELL2 W25-SELL3.
           IF W02-LINAGE < W02-PRN-LENGTH
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM BF15.
             MOVE SPACES             TO R-DETAIL.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****" TO SAL-L1
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE "P"              TO WS-COMMAND
               GO TO BF999.

       BF30.
           IF WS-AUDKEY < W70-REP
               ADD 1                 TO WS-AUDKEY
               GO TO BF20.
      *
      *    ****    P R I N T   S A L E S M A N   T O T A L
      *
          IF W02-LINAGE < (W02-PRN-LENGTH - 4)
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM BE15.
             MOVE SPACES             TO R-DETAIL.
             MOVE "  SALESMAN TOTAL" TO SAL-L5.
             MOVE W25-COST2          TO SAL-CST.
             MOVE W25-SELL2          TO SAL-SEL.
             COMPUTE W25-SELL1 = W25-SELL2 - W25-COST2.
             COMPUTE W25-COST1 = (W25-SELL1 / W25-SELL2) * 100.000 + 0.005.
             MOVE W25-SELL1          TO SAL-PRF.
             MOVE W25-COST1          TO SAL-GPR.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "SALES"            TO SAL-H11.
             MOVE " RETURNS"         TO SAL-H12.
             MOVE "   NETT"          TO SAL-H13.
             MOVE "RTN %"            TO SAL-H14.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "CASH SALES"       TO SAL-DSC.
             MOVE W25-CASH           TO SAL-CST.
             MOVE W25-REFF           TO SAL-SEL.
             ADD W25-CASH W25-REFF   GIVING SAL-PRF.
             COMPUTE SAL-GPR ROUNDED = ((W25-REFF * -1) / W25-CASH) * 100.0000.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "INVOICING"        TO SAL-DSC.
             MOVE W25-INV            TO SAL-CST.
             MOVE W25-CRN            TO SAL-SEL.
             ADD W25-INV W25-CRN     GIVING SAL-PRF.
             COMPUTE SAL-GPR ROUNDED = ((W25-CRN * -1) / W25-INV) * 100.0000.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****" TO SAL-L1
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE "P"              TO WS-COMMAND
               GO TO BF999.
             MOVE ZERO               TO W25-SELL1 W25-SELL2 W25-COST1 W25-COST2.
             GO TO BF06.

       BF999.
             EXIT.
      /
       BG00R           SECTION 5.
       BG00.
             PERFORM RESTORE-SCREEN-3.
             DISPLAY "Busy printing Turnover Report" AT 1220 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             MOVE ZERO               TO W25-COST1 W25-COST2 W25-COST3 W25-SELL1 W25-SELL2 W25-SELL3.
             MOVE ZERO               TO WS-PAGE.
             MOVE 2                  TO WS-AUDKEY.

       BG05.
             PERFORM AA50 THRU AA55.
             MOVE "T U R N O V E R   R E P O R T" TO R-CPY.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "REF NO"           TO SAL-H9.
             MOVE "DESCRIPTION"      TO SAL-H10.
             MOVE " COST"            TO SAL-H11.
             MOVE " SELLING"         TO SAL-H12.
             MOVE " PROFIT"          TO SAL-H13.
             MOVE "G.P.%"            TO SAL-H14.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "D"                TO W02-PRN-TYPE.

       BG10.
             PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
           IF NOT(WS-F-ERROR = ZERO)
               MOVE ZERO             TO WS-F-ERROR
               GO TO BG15.
           IF NOT(AUD-TYPE = 5 OR 7)
               GO TO BG15.
      *
      *    ****    P R I N T   I N D V I D U A L   S A L E S
      *
             MOVE AUD-CSH            TO SAL-RF.
           IF AUD-TYPE = 5
               MOVE "CASH SALE"      TO SAL-DSC
           ELSE
           IF AUD-SELL < ZERO
               MOVE "CREDIT NOTE"    TO SAL-DSC
           ELSE
               MOVE "INVOICE"        TO SAL-DSC.
             MOVE AUD-COST           TO SAL-CST.
             MOVE AUD-SELL           TO SAL-SEL.
             COMPUTE SAL-PRF = AUD-SELL - AUD-COST.
             COMPUTE SAL-GPR = ((AUD-SELL - AUD-COST) / AUD-SELL) * 100.000 + 0.005.
             ADD AUD-COST            TO W25-COST2 W25-COST3.
             ADD AUD-SELL            TO W25-SELL2 W25-SELL3.
           IF W02-LINAGE < (W02-PRN-LENGTH - 2)
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM BG05.
             MOVE SPACES             TO R-DETAIL.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****" TO SAL-L1
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE "P"              TO WS-COMMAND
               GO TO BG999.
       BG15.
           IF WS-AUDKEY < W70-REP
               ADD 1                 TO WS-AUDKEY
               GO TO BG10.
      *
      *    ****    P R I N T   G R A N D   T O T A L
      *
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "  TOTAL TURNOVER AND GP" TO SAL-L5.
             MOVE W25-COST3          TO SAL-CST.
             MOVE W25-SELL3          TO SAL-SEL.
             COMPUTE W25-SELL1 = W25-SELL3 - W25-COST3.
             COMPUTE W25-COST1 = (W25-SELL1 / W25-SELL3) * 100.000 + 0.005.
             MOVE W25-SELL1          TO SAL-PRF.
             MOVE W25-COST1          TO SAL-GPR.
             MOVE 99                 TO WS-ADVANCE.
             MOVE "E"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             MOVE "P"                TO WS-COMMAND.
             MOVE SPACES             TO R-DETAIL.

       BG999.
             EXIT.
      /
       BH00R           SECTION 50.
       BH00.
             PERFORM RESTORE-SCREEN-3.
             DISPLAY "Busy printing detailed Turnover Report" AT 1220 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             MOVE ZERO               TO W25-COST1 W25-COST2 W25-COST3 W25-SELL1 W25-SELL2 W25-SELL3.
             MOVE ZERO               TO WS-PAGE.
             MOVE 2                  TO WS-AUDKEY.

       BH15.
             PERFORM AA50 THRU AA55.
             MOVE "T U R N O V E R   R E P O R T" TO R-CPY.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "ITEM CODE"        TO SAL-H2.
             MOVE "DESCRIPTION"      TO SAL-H3.
             MOVE "COST"             TO SAL-H4.
             MOVE "SELLING"          TO SAL-H5.
             MOVE "G.P.%"            TO SAL-H6.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "D"                TO W02-PRN-TYPE.

       BH20.
             PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
           IF NOT(WS-F-ERROR = ZERO)
               MOVE ZERO             TO WS-F-ERROR
               GO TO BH30.
           IF NOT(AUD-TYPE = 3 OR 4 OR 6 OR 8)
               GO TO BH30.
           IF AUD-TYPE = 3 OR 4
               GO TO BH25.
      *
      *    ****    P R I N T   I N D V I D U A L   I T E M S
      *
             MOVE AUD-EXT-STOCK      TO STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               MOVE "Item deleted or code changed" TO STK-DESC.
             MOVE STK-CODE           TO SAL-ITEM.
             MOVE STK-DESC           TO SAL-DESC.
             MOVE AUD-CST            TO SAL-COST.
             MOVE AUD-SEL            TO SAL-SELL.
             COMPUTE SAL-GP = ((AUD-SEL - AUD-CST) / AUD-SEL) * 100.000 + 0.005.
             ADD AUD-CST             TO W25-COST1 W25-COST2 W25-COST3.
             ADD AUD-SEL             TO W25-SELL1 W25-SELL2 W25-SELL3.
           IF AUD-SUPER = 99
               MOVE "N/C"            TO SAL-NOCHARGE
           ELSE
               MOVE AUD-SUPER        TO SAL-SUPER.
           IF W02-LINAGE < (W02-PRN-LENGTH - 2)
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM BH15.
            MOVE SPACES  TO R-DETAIL.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****" TO SAL-L1
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE "P"              TO WS-COMMAND
               GO TO BH999.
             GO TO BH30.
      *
      *    ****    P R I N T   S A L E   T O T A L
      *
       BH25.
           IF AUD-TYPE = 4
               IF W25-SELL1 < ZERO
                   MOVE "CASH REFUND TOTAL" TO SAL-H8
               ELSE
                   MOVE "CASH SALE TOTAL"   TO SAL-H8
               END-IF
           ELSE
               MOVE AUD-AC           TO R-AC W80-ACNO DEB-ACNO
               PERFORM BK000
               MOVE W40-DSP-NAME     TO R-DNME        
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE SPACES           TO R-DETAIL
               IF W25-SELL1 < ZERO
                   MOVE "CREDIT NOTE TOTAL" TO SAL-H8
               ELSE
                   MOVE "INVOICE TOTAL" TO SAL-H8.
             MOVE "REFERENCE NO:"    TO SAL-H7.
             MOVE AUD-REF            TO SAL-REF.
             MOVE W25-COST1          TO SAL-COST.
             MOVE W25-SELL1          TO SAL-SELL.
             COMPUTE SAL-GP = ((W25-SELL1 - W25-COST1) / W25-SELL1) * 100.000 + 0.005.
             MOVE ZERO               TO W25-SELL1 W25-COST1.
             MOVE ZERO               TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
           IF W02-LINAGE < (W02-PRN-LENGTH - 3)
               MOVE 2                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM BH15.
             MOVE SPACES             TO R-DETAIL.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****" TO SAL-L1
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE "P"              TO WS-COMMAND
               GO TO BH999.
      *
      *    ****    C H E C K   E N D   O F   A U D I T   F I L E
      *
       BH30.
           IF WS-AUDKEY < W70-REP
               ADD 1                 TO WS-AUDKEY
               GO TO BH20.
      *
      *    ****    P R I N T   G R A N D   T O T A L
      *
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "******"           TO SAL-H7.
             MOVE "TOTAL TURNOVER"   TO SAL-H8.
             MOVE W25-COST3          TO SAL-COST.
             MOVE W25-SELL3          TO SAL-SELL.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "** GROSS PROFIT"  TO SAL-H8.
             COMPUTE W25-SELL1 = W25-SELL3 - W25-COST3.
             COMPUTE W25-COST1 = (W25-SELL1 / W25-SELL3) * 100.000 + 0.005.
             MOVE W25-SELL1          TO SAL-SELL.
             MOVE W25-COST1          TO SAL-GP.
             MOVE 99                 TO WS-ADVANCE.
             MOVE "E"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             MOVE "P"                TO WS-COMMAND.
             MOVE SPACES             TO R-DETAIL.

       BH999.
             EXIT.
      /
       BI00R       SECTION 50.
       BI00.
             PERFORM RESTORE-SCREEN-3.
             DISPLAY "Printing Debtor Receipts" AT 1220 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             MOVE 1                  TO WS-S6.

       BI02.
             MOVE ZERO               TO W200-CR(WS-S6).
           IF WS-S6 < 300
               ADD 1                 TO WS-S6
               GO TO BI02.
             MOVE ZERO               TO W200-DTOT W200-CTOT W200-COST W200-SELL W200-NET.
             MOVE ZERO               TO WS-PAGE.
             MOVE 2                  TO WS-AUDKEY.
       BI05.
             PERFORM AA50 THRU AA55.
             MOVE "DEBTOR RECEIPTS"  TO R-HEAD.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "AC-NO"            TO R-H14.
             MOVE "DEBTOR NAME"      TO R-H14A.
             MOVE "REF. NO."         TO R-H15.
             MOVE "  VALUE"          TO R-H16.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "D"                TO W02-PRN-TYPE.

       BI10.
             PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
           IF NOT(WS-F-ERROR = ZERO)
               MOVE ZERO             TO WS-F-ERROR
               GO TO BI15.
           IF NOT(AUD-TYPE = 11)
               GO TO BI15.
             MOVE AUD-AC             TO R-AC W80-ACNO DEB-ACNO.
             PERFORM BK000
             MOVE W40-DSP-NAME       TO R-DNME.
             MOVE AUD-REF            TO R-REF.
             MOVE AUD-VAL            TO R-DEBT.
             ADD AUD-VAL             TO W200-CTOT.
           IF W02-LINAGE < W02-PRN-LENGTH
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM BI05.
             MOVE SPACES             TO R-DETAIL.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****" TO R-DETAIL
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE "P"              TO WS-COMMAND
               GO TO BI999.

       BI15.
           IF WS-AUDKEY < W70-REP
               ADD 1                 TO WS-AUDKEY
               GO TO BI10.
           IF W02-LINAGE < W02-PRN-LENGTH
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM BI05.
             MOVE WS-H4              TO R-H15.
             MOVE W200-CTOT          TO R-DEBT.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE WS-H1              TO R-DETAIL.
             MOVE 99                 TO WS-ADVANCE.
             MOVE "E"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             MOVE "P"                TO WS-COMMAND.
             MOVE SPACES             TO R-DETAIL.

       BI999.
             EXIT.
      /
       BJ00R       SECTION 50.
       BJ00.
             PERFORM RESTORE-SCREEN-3.
             DISPLAY "Printing Discounted Sales" AT 1220 WITH FOREGROUND-COLOR Grey HIGHLIGHT.

       BJ999.
             EXIT.

       BK000           SECTION 5.
       BK00.
             PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
      *
      *    ****    Move the Debtor Record to working storage area
      *
             MOVE DEB-RECORD1        TO W40-DEBTREC.
      *    *************************************************************
      *    ****         T E S T   F O R   I N D I V I D U A L       ****
      *    *************************************************************
           IF W40-TYPE = 1
               MOVE "D"              TO W40-INSTR
               CALL "DTP\DTPNAME" USING W40-COMPANY LS-USER-ID
           ELSE
               MOVE W40-NAME         TO W40-DSP-NAME.
 
       BK999.
             EXIT.
       
      *
      *
      *
      *
      *
      *
      *
      
       BX000           SECTION 50.
       BX00.
             DISPLAY "Analyzing audit file " AT 1212 WITH FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
           IF W70-REP < 2
               MOVE ZERO             TO WS-REP1 WS-REP2 WS-REP3 WS-REP4 WS-REP5 WS-REP6 WS-REP7 WS-REP8 WS-REP9 WS-REP10
               GO TO BX999.
             MOVE 2                  TO WS-AUDKEY.

       BX05.
             PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
           IF NOT(WS-F-ERROR = ZERO)
               MOVE ZERO             TO WS-F-ERROR
               GO TO BX10.
           IF AUD-TYPE = 5
               IF AUD-SUB > 0 AND < 41
                   MOVE 1            TO W40-DREP(AUD-SUB).
           IF AUD-TYPE = 5 OR 7
               IF AUD-SMAN > 0 AND < 51
                   MOVE 1            TO W40-SREP(AUD-SMAN).

       BX10.
           IF WS-AUDKEY < W70-REP
               ADD 1                 TO WS-AUDKEY
               GO TO BX05.

       BX999.
             EXIT.
      /
       BZ00R           SECTION 5.
       BZ00.
             DISPLAY CLEAR-L50.
             MOVE ZERO               TO WS-PAGE.
           IF NOT(WS-STATOP = "1" OR "8")
               PERFORM BZ05
               GO TO BZ15.
      *
      *   As this version of the system includes all modules the
      *   following tests are no longer required. 
      *   These will be removed with the next cleanup of code.
      *
      *    IF LS0-DTP NOT = 2
      *        MOVE "Not Installed"    TO WS-ERR-MES
      *        PERFORM ERROR-MESSAGE
      *        IF WS-STATOP = "1"
      *            GO TO BZ999
      *        ELSE
      *            PERFORM BZ05
      *            GO TO BZ15.
             DISPLAY "Busy printing Debtors Statistics      " AT 2212 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             MOVE 1         TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.

       BZ05.
             PERFORM AA50 THRU AA55.
             MOVE "STATISTICS REPORT" TO R-HEAD.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.

       BZ10.
             MOVE "DEBTORS"          TO R-H26.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "TODAY"            TO R-H28.
             MOVE WS-H13             TO R-H31.
             MOVE WS-H14             TO R-H34.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "TRANSACTIONS"     TO R-H26.
             MOVE WS-H12             TO R-H27 R-H30 R-H33.
             MOVE WS-H11             TO R-H29 R-H32 R-H35.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "D"                TO W02-PRN-TYPE.

       BZ10A.
             MOVE 11                 TO WS-PARKEY.

       BZ11.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE 1                  TO WS-S1.

       BZ12.
           IF PAR-E-DESC(WS-S1) = SPACES
               GO TO BZ14.
             MOVE PAR-E-DESC(WS-S1)  TO R-H36.
             MOVE PAR-T-DAY(WS-S1)   TO R-N1.
             MOVE PAR-T-VAL(WS-S1)   TO R-V5.
             MOVE PAR-T-MTD(WS-S1)   TO R-N2.
             MOVE PAR-T-MTDV(WS-S1)  TO R-V6.
             MOVE PAR-T-YTD(WS-S1)   TO R-N3.
             MOVE PAR-T-YTDV(WS-S1)  TO R-V7.

       BZ13.
           IF W02-LINAGE < W02-PRN-LENGTH
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM BZ05 THRU BZ10.
             MOVE SPACES             TO R-DETAIL.

       BZ14.
           IF WS-S1 < 2
               ADD 1             TO WS-S1
               GO TO BZ12.
           IF WS-PARKEY < 60
               ADD 1             TO WS-PARKEY
               GO TO BZ11.
             PERFORM BZ13.
             MOVE 9              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      *
      *  P R I N T   A L L   V A T   R A T E S
      *
      *    **** INSERT A LOOP TO PRINT EACH VAT RATE
      *
             MOVE "VAT"              TO DPT-SCDE.
             MOVE 1                  TO DPT-V-RTE.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
             MOVE "VAT SALES"        TO R-H36.
             MOVE PAR-DVAT-DAY       TO R-V5.
             MOVE PAR-DVAT-MTD       TO R-V6.
             MOVE PAR-DVAT-YTD       TO R-V7.
             PERFORM BZ13.
             MOVE "VAT"              TO R-H36.
             MOVE DPT-O-VAT-DAY      TO R-V5.
             MOVE DPT-O-VAT-MTD      TO R-V6.
             MOVE DPT-O-VAT-YTD      TO R-V7.
             PERFORM BZ13.
             MOVE "NON VAT SALES"    TO R-H36.
             MOVE PAR-DNGS-DAY       TO R-V5.
             MOVE PAR-DNGS-MTD       TO R-V6.
             MOVE PAR-DNGS-YTD       TO R-V7.
             PERFORM BZ13.
             MOVE "VAT RETURNS"  TO R-H36.
             MULTIPLY -1 BY PAR-CVAT-DAY GIVING R-V5.
             MULTIPLY -1 BY PAR-CVAT-MTD GIVING R-V6.
             MULTIPLY -1 BY PAR-CVAT-YTD GIVING R-V7.
             PERFORM BZ13.
             MOVE "VAT REVERSED"     TO R-H36.
             MOVE DPT-O-VRET-DAY     TO R-V5.
             MOVE DPT-O-VRET-MTD     TO R-V6.
             MOVE DPT-O-VRET-YTD     TO R-V7.
             PERFORM BZ13.
             MOVE "NET VAT - DAY"    TO R-H36.
             COMPUTE R-V5 = DPT-O-VAT-DAY + DPT-O-VRET-DAY.
             PERFORM BZ13.
             MOVE "NON VAT RETURNS"  TO R-H36.
             MULTIPLY -1 BY PAR-CNGS-DAY GIVING R-V5.
             MULTIPLY -1 BY PAR-CNGS-MTD GIVING R-V6.
             MULTIPLY -1 BY PAR-CNGS-YTD GIVING R-V7.
           IF W02-LINAGE < W02-PRN-LENGTH
               MOVE 3                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM BZ05 THRU BZ10.
             MOVE SPACES             TO R-DETAIL.
             MOVE "ACCOUNT RECORDS"  TO R-H26.
             PERFORM BZ13.
             MOVE "DEBTOR ACCOUNTS"  TO R-H36.
             MOVE DEB-ACCOUNTS       TO R-N2.
             PERFORM BZ13 3 TIMES.
             MOVE "THIS MONTH"       TO R-H26.
             PERFORM BZ13.
             MOVE "OPENING BALANCE"  TO R-H36.
             MOVE DEB-OPEN           TO R-V6.
             PERFORM BZ13.
             MOVE "DEBITS"           TO R-H36.
             MOVE DEB-DT             TO R-V6.
             PERFORM BZ13.
             MOVE "CREDITS"          TO R-H36.
             MULTIPLY -1 BY DEB-CR   GIVING R-V6.
             PERFORM BZ13.
             MOVE "CLOSING BALANCE"  TO R-H36.
             MOVE DEB-OUT            TO R-V6.
             PERFORM BZ13 3 TIMES.
             MOVE SPACES             TO R-DETAIL.
             MOVE "AGE ANALYSIS"     TO R-H26.
             PERFORM BZ13.
             MOVE DEB-ONE            TO W25-TOTAL.
             ADD DEB-TWO             TO W25-TOTAL.
             ADD DEB-THREE           TO W25-TOTAL.
             ADD DEB-INTEREST        TO W25-TOTAL.
             SUBTRACT W25-TOTAL FROM DEB-OUT GIVING R-V6.
             MOVE "CURRENT"          TO R-H36.
             PERFORM BZ13.
             MOVE "30 DAYS"          TO R-H36.
             MOVE DEB-ONE            TO R-V6.
             PERFORM BZ13.
             MOVE "60 DAYS"          TO R-H36.
             MOVE DEB-TWO            TO R-V6.
             PERFORM BZ13.
             MOVE "90 DAYS"          TO R-H36.
             MOVE DEB-THREE          TO R-V6.
             PERFORM BZ13.
             MOVE "INTEREST"         TO R-H36.
             MOVE DEB-INTEREST       TO R-V6.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****" TO R-L1
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE "P"              TO WS-COMMAND
               GO TO BZ999.
      *
      *   As this version of the system includes all modules the
      *   following tests are no longer required. 
      *   These will be removed with the next cleanup of code.
      *
      *    IF LS0-STP NOT = 2
      *       MOVE 99                TO WS-ADVANCE
      *       PERFORM CALL-PRINTUTL.
            DISPLAY "Printed" AT 0935 WITH FOREGROUND-COLOR Brown HIGHLIGHT.

       BZ15.
           IF NOT(WS-STATOP = "1" OR "3" OR "4" OR "8")
               GO TO BZ20.
      *
      *   As this version of the system includes all modules the
      *   following tests are no longer required. 
      *   These will be removed with the next cleanup of code.
      *
      *    IF LS0-STP NOT = 2
      *        MOVE "Not Installed"  TO WS-ERR-MES
      *        PERFORM ERROR-MESSAGE
      *        IF WS-STATOP = "8"
      *            GO TO BZ20
      *        ELSE
      *            GO TO BZ999.
             DISPLAY "Busy printing Stock Statistics       " AT 2212 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             MOVE 3                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE "STOCK"            TO R-H26.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
           IF (WS-STATOP = "4") OR (LS0-DTP NOT = 2)
               MOVE SPACES           TO R-DETAIL
               MOVE "TODAY"          TO R-H28
               MOVE WS-H13           TO R-H31
               MOVE WS-H14           TO R-H34
               MOVE 2                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "PURCHASES"        TO R-H36.
             MOVE STK-CMTD           TO R-V6.
             MOVE STK-CYTD           TO R-V7.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE "ADJUSTMENTS"      TO R-H36.
             MOVE STK-ADJMTD         TO R-V6.
             MOVE STK-ADJYTD         TO R-V7.
             PERFORM CALL-PRINTUTL.
             MOVE "SALES"            TO R-H36.
             MOVE STK-SMTD           TO R-V6.
             MOVE STK-SYTD           TO R-V7.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "STOCK ON HAND"    TO R-H36.
             MOVE STK-TOTAL          TO R-V5.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "COST OF SALES"    TO R-H26.
             MOVE 8                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-CSHT           TO R-V5.
             MOVE PAR-CMTD           TO R-V6.
             MOVE PAR-CYTD           TO R-V7.
             MOVE 99                 TO WS-ADVANCE.
             MOVE "E"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             MOVE "P"                TO WS-COMMAND.
             MOVE SPACES             TO R-DETAIL.
             DISPLAY "Printed" AT 1235 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****" TO R-L1
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE "P"              TO WS-COMMAND
               GO TO BZ999.
       BZ20.
           IF NOT(WS-STATOP = "2" OR "8")
               GO TO BZ999.
      *
      *   As this version of the system includes all modules the
      *   following tests are no longer required. 
      *   These will be removed with the next cleanup of code.
      *
      *    IF WS-STATOP = "8"
      *       IF LS0-DTP = 2 OR LS0-STP = 2
      *           IF LS0-CRP = 2
      *               PERFORM BZ05
      *           ELSE
      *               GO TO BZ999.
      *    IF LS0-CRP NOT = 2
      *        MOVE "Not Installed"  TO WS-ERR-MES
      *        PERFORM ERROR-MESSAGE
      *        GO TO BZ999.
             DISPLAY "Busy printing Creditors Statistics   " AT 2212 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             MOVE 2                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE 6                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****" TO R-L1
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE "P"              TO WS-COMMAND
               GO TO BZ999.
             MOVE "CREDITORS"        TO R-H26.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "TODAY"            TO R-H28.
             MOVE WS-H13             TO R-H31.
             MOVE WS-H14             TO R-H34.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.
             MOVE "TRANSACTIONS"     TO R-H26.
             MOVE WS-H12             TO R-H27 R-H30 R-H33.
             MOVE WS-H11             TO R-H29 R-H32 R-H35.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DETAIL.

       BZ20A.
             MOVE 191                TO WS-PARKEY.

       BZ21.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE 1                  TO WS-S1.
       BZ22.
           IF PAR-C-DESC(WS-S1) = SPACES
               GO TO BZ24.
             MOVE PAR-C-DESC(WS-S1)  TO R-H36.
             MOVE PAR-C-DAY(WS-S1)   TO R-N1.
             MOVE PAR-C-VAL(WS-S1    TO R-V5.
             MOVE PAR-C-MTD(WS-S1)   TO R-N2.
             MOVE PAR-C-MTDV(WS-S1)  TO R-V6.
             MOVE PAR-C-YTD(WS-S1)   TO R-N3.
             MOVE PAR-C-YTDV(WS-S1)  TO R-V7.
             PERFORM BZ13.
       BZ24.
           IF WS-S1 < 2
               ADD 1                 TO WS-S1
               GO TO BZ22.
           IF WS-PARKEY < 200
               ADD 1                 TO WS-PARKEY
               GO TO BZ21.
             MOVE 6                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             PERFORM BZ13.
             MOVE "RAILAGE"          TO R-H36.
             MOVE PAR-RAILD          TO R-V5.
             MOVE PAR-RAILM          TO R-V6.
             PERFORM BZ13.
             MOVE "DISCOUNT"         TO R-H36.
             MOVE PAR-DISCD          TO R-V5.
             MOVE PAR-DISCM          TO R-V6.
             PERFORM BZ13 3 TIMES.
             MOVE "THIS MONTH"       TO R-H26.
             PERFORM BZ13.
             MOVE "OPENING BALANCE"  TO R-H36.
             MOVE P-OPEN             TO R-V6.
             PERFORM BZ13.
             MOVE "DEBITS"           TO R-H36.
             MOVE P-DEBT             TO R-V6.
             PERFORM BZ13.
             MOVE "CREDITS"          TO R-H36.
             MOVE P-CRED             TO R-V6.
             PERFORM BZ13.
             MOVE "CLOSING BALANCE"  TO R-H36.
             MOVE P-CLOSE            TO R-V6.
             MOVE 99                 TO WS-ADVANCE.
             MOVE "E"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             MOVE "P"                TO WS-COMMAND.
             MOVE SPACES             TO R-DETAIL.
             DISPLAY "Printed" AT 1035 WITH FOREGROUND-COLOR Brown HIGHLIGHT.

       BZ999.
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
             MOVE LS-USUB            TO WS-USUB.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

      *
      *    ****    *   ****    *    *     *     ***** *** *     *****
      *    *   *  * *  *   *  * *   **   **     *      *  *     *
      *    *   * *   * *   * *   *  * * * *     *      *  *     *
      *    ****  ***** ****  *****  *  *  *     ***    *  *     ***
      *    *     *   * *  *  *   *  *     *     *      *  *     *
      *    *     *   * *   * *   *  *     *     *      *  *     *
      *    *     *   * *   * *   *  *     *     *     *** ***** *****
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
      *          ***   ***  *   * ***** ****   ***  *             *
      *         *   * *   * *   *   *   *   * *   * *            *
      *         *     *   * **  *   *   *   * *   * *           *
      *         *     *   * * * *   *   ****  *   * *          *
      *         *     *   * *  **   *   *   * *   * *         *
      *         *   * *   * *   *   *   *   * *   * *        *
      *          ***   ***  *   *   *   *   *  ***  *****   *
      *
      *  *   * ***** ***** *   *  ***  ****  *   *   ***** *** *     *****
      *  *   * *       *   *   * *   * *   * *  *    *      *  *     *
      *  **  * *       *   *   * *   * *   * * *     *      *  *     *
      *  * * * ***     *   * * * *   * ****  **      ***    *  *     ***
      *  *  ** *       *   * * * *   * *   * * *     *      *  *     *
      *  *   * *       *   * * * *   * *   * *  *    *      *  *     *
      *  *   * *****   *    * *   ***  *   * *   *   *     *** ***** *****
      *
             OPEN I-O NETWORK.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS NOT = "00"
               MOVE 2                TO WS-F-ERROR
               PERFORM OPEN-ERROR.
             MOVE 1                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             SUBTRACT 1 FROM DEB-AUDIT GIVING W70-REP.
             DISPLAY CLEAR-L50.
      *
      *      *   *   * ****  *** *****     ***** *** *     *****
      *     * *  *   * *   *  *    *       *      *  *     *
      *    *   * *   * *   *  *    *       *      *  *     *
      *    ***** *   * *   *  *    *       ***    *  *     ***
      *    *   * *   * *   *  *    *       *      *  *     *
      *    *   * *   * *   *  *    *       *      *  *     *
      *    *   *  ***  ****  ***   *       *     *** ***** *****
      *
             OPEN I-O AUDIT.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS NOT = "00"
               MOVE 1                TO WS-F-ERROR
               PERFORM OPEN-ERROR.
             MOVE ZERO               TO WS-AUDKEY WS-AUD.
             DISPLAY "Checking AUDIT file" AT 5012 WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Cyan BLINK.
       ZA03.
             ADD 1                   TO WS-AUDKEY.
             READ AUDIT.
           IF WS-STATUS NOT = "00"
               IF WS-AUDKEY < W70-REP
                   GO TO ZA03
               ELSE
               IF WS-AUD < 1000
                   ADD 1             TO WS-AUD
                   GO TO ZA03
               END-IF
           ELSE
               MOVE ZERO             TO WS-AUD
               IF WS-AUDKEY > W70-REP
                   MOVE WS-AUDKEY    TO W70-REP
                   GO TO ZA03
               ELSE
                   GO TO ZA03.
      *
      *     ***  *****  ***   ***  *   *     ***** ***** *     *****
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *    *       *   *   * *     * *       *       *   *     *
      *     ***    *   *   * *     **        ***     *   *     ***
      *        *   *   *   * *     * *       *       *   *     *
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *     ***    *    ***   ***  *   *     *     ***** ***** *****
      *
             OPEN I-O STOCK.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 22           TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
               MOVE 22               TO WS-F-ERROR
               PERFORM OPEN-ERROR.
       ZA05.
      *
      *    ****  ***** ****  *****  ***  ****     ***** *** *     *****
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *    *   * ***   ****    *   *   * ****     ***    *  *     ***
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *    ****  ***** ****    *    ***  *   *    *     *** ***** *****
      *
             OPEN I-O DEBTOR.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 6            TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
               MOVE 6                TO WS-F-ERROR
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
             MOVE 3                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-USE-ITM        TO WS-USE-ITM.
      *
      *   As this version of the system includes all modules the
      *   following tests are no longer required. 
      *   These will be removed with the next cleanup of code.
      *
      *   IF LS0-DTP NOT = 2
      *        GO TO ZA999.
      *   IF LS0-STP NOT = 2
      *        GO TO ZA999.
             MOVE 1                  TO WS-AUDKEY.
             PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
             MOVE AUD-REP1           TO WS-REP1.
             MOVE AUD-REP2           TO WS-REP2.
             MOVE AUD-REP3           TO WS-REP3.
             MOVE AUD-REP4           TO WS-REP4.
             MOVE AUD-REP5           TO WS-REP5.
             MOVE AUD-REP6           TO WS-REP6.
             MOVE AUD-REP7           TO WS-REP7.
             MOVE AUD-REP8           TO WS-REP8.
             MOVE AUD-REP9           TO WS-REP9.
             MOVE AUD-REP10          TO WS-REP10.
             MOVE AUD-REP11          TO WS-REP11.
             GO TO ZA999.

       COPY "ZA49.PRO".

       ZA50.
             EVALUATE WS-F-ERROR
               WHEN 3     MOVE "CREDITOR file"  TO WS-FILE
               WHEN 6     MOVE "DEBTOR file"    TO WS-FILE
               WHEN 20    MOVE "SALES ORDER"    TO WS-FILE
               WHEN 22    MOVE "STOCK FILE"     TO WS-FILE
               WHEN OTHER MOVE "INVALID FILE REF" TO WS-FILE
             END-EVALUATE.
             MOVE SPACES             TO WS-ERR-MES.
             STRING "Rebuild " DELIMITED SIZE WS-FILE INTO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.
             
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
               WHEN 1      MOVE W02-AUDITF   TO WS-FILE
                           MOVE WS-AUDKEY    TO WS-KEY
               WHEN 2      MOVE W02-NETWORK  TO WS-FILE
                           MOVE WS-NETKEY    TO WS-KEY
               WHEN 6      MOVE W02-DEBTOR   TO WS-FILE
                           MOVE DEB-ACNO     TO WS-KEYX
               WHEN 7      MOVE W02-DEPART   TO WS-FILE
                           MOVE DPT-CODE     TO WS-KEYX
               WHEN 15     MOVE WS-PARID     TO WS-FILE
                           MOVE WS-PARKEY    TO WS-KEY
               WHEN 22     MOVE W02-STOCKF   TO WS-FILE
                           MOVE STK-CODE     TO WS-KEYX
               WHEN OTHER  MOVE "ERROR"      TO WS-FILE
                           MOVE "REP001"     TO WS-KEYX
             END EVALUATE.

       COPY "DISPERR.PRO".
