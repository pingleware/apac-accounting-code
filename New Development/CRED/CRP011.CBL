      $set DATALIT LINKCOUNT"596" GNT"CRP011.GNT"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******     ****       **        **      *
      *   **    **  **    **  **    **   **  **     ***       ***      *
      *   **        **    **  **    **  **    **     **        **      *
      *   **        ******    *******   **    **     **        **      *
      *   **        **   **   **        **    **     **        **      *
      *   **    **  **    **  **         **  **      **        **      *
      *    ******   **    **  **          ****     ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R   I N V O I C E S   ( S T O C K )          *
      *                                                                *
      *       Version 9.04.03 - September 2017                         *
      *                                                                *
      ******************************************************************
      *                                                                *
      * September 2005 - Alternate/Previous Supplier included on the   *
      *                  stock record and now updated when receiving   *
      *                  new stock                                     *
      *                - See STPENQ (Stock Enquiry)                    *
      *                                                                *
      * June 2018 - Added program CRPRDWR to handle IO for Creditor    *
      *             file and COPY file CA155.CRP to call and handlw IO *
      *             for the CREDITOR file. SEE CA155.CRP and CRPRDWR   *
      *             for details.                                       *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        CRP011.
       AUTHOR.            James William Lemmon.
       DATE-WRITTEN.      JUNE, 1983.

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

       COPY "BARCOD.SL".

       COPY "CONTROL.SL".

       COPY "CREDIT.SL".

       COPY "CRTRAN.SL".

       COPY "DEPART.SL".

       COPY "LEDTRF.SL".

       COPY "PARAM.SL".

       COPY "PORDER.SL".

       COPY "PRCLAB.SL".

       COPY "PURCH.SL".

       COPY "PURDEX.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

       COPY "STOCK.SL".

       COPY "TXTRAN.SL".

       COPY "WARHSE.SL".

       COPY "WSTOCK.SL".

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

       COPY "BARCOD.FD".

       COPY "CONTROL.FDE".

       COPY "CREDIT.FDE".

       COPY "CRTRAN.FDE".

       COPY "DEPART.FDE".

       COPY "LEDTRF.FDE".

       COPY "PARAM.FDE".

       COPY "PORDER.FDE".

       COPY "PRCLAB.FD".

       COPY "PURCH.FD".

       COPY "PURDEX.FDE".

       COPY "RECOVER.FD".

       COPY "SHARED.FDE".

       COPY "STOCK.FDE".

       COPY "TXTRAN.FDE".

       COPY "WARHSE.FDE".

       COPY "WSTOCK.FDE".

      *
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
       77  WS-SUB                  PIC  9(04)    COMP-5.
       77  WS-IXS1                 PIC  9(04)    COMP-5.
       77  WS-IXS2                 PIC  9(04)    COMP-5.
       77  WS-IXS3                 PIC  9(04)    COMP-5.
       77  WS-IXS4                 PIC  9(04)    COMP-5.
       77  WS-PARKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-RECKEY               PIC  9(04)    COMP-5.
       77  WS-NETKEY               PIC  9(04)    COMP-5.
       77  WS-PRCKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-PURKEY               PIC  9(04)    COMP-5.
       77  WS-KEYSTR               PIC  9(04)    COMP-5.
       77  WS-RECOVER              PIC  9(04)    COMP-5.
       77  WS-SHARED               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-STOCK                PIC  X(01).
       77  WS-PURLAST              PIC  9(04)    COMP-5.
       77  WS-VIEWKEY              PIC  9(04)    COMP-5.
       77  WS-V-FIRST              PIC  9(04)    COMP-5.
       77  WS-V-LAST               PIC  9(04)    COMP-5.
       77  WS-TRANS                PIC  9(04)    COMP-5 VALUE 1.
       77  WS-ERROR                PIC  9(01)           VALUE 0.
       77  WS-ETYPE                PIC  X(01).
       77  WS-LOOP                 PIC  X(01)           VALUE SPACE.
       77  WS-DEL                  PIC  X(01).
       77  WS-NUM                  PIC  Z9.
       77  WS-S                    PIC  9(04)    COMP-5.
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S9                   PIC  9(04)    COMP-5.
       77  WS-ORD                  PIC  X(01).
       77  WS-OPTION               PIC  X(01).
       77  WS-LAB                  PIC  X(01).
       77  WS-PRICE                PIC  X(01).
       77  WS-SKIP                 PIC  X(01)           VALUE "Y".
       77  WS-LOOK                 PIC  X(01)           VALUE "S".
       77  WS-CHAR1                PIC  X(01).
       77  WS-USE-3                PIC  X(01).
       77  WS-USE-CASES            PIC  X(01).
       77  WS-USE-PACKS            PIC  X(01).
       77  WS-USE-ITM              PIC  X(01).
       77  WS-EXT-STK              PIC  X(01).
       77  WS-BAR-CODE             PIC  X(01).
       77  WS-IND                  PIC  9(01)           VALUE 0.
       77  WS-SKEY                 PIC  9(05).
       77  WS-KEY1                 PIC  9(04)    COMP-5.
       77  WS-PAGE                 PIC  9(04)    COMP-5 VALUE 0.
       77  WS-ER1                  PIC  X(09)           VALUE "No record".
       77  WS-ER2                  PIC  X(25)           VALUE "May not be less than cost".
       77  WS-ER8                  PIC  X(09)           VALUE "No record".
       77  WS-HDTE                 PIC  X(04)           VALUE "DATE".
       77  WS-HACNO                PIC  X(04)           VALUE "ACNO".
       77  WS-HCRED                PIC  X(09)           VALUE "CREDITOR:".
       77  WS-HQNT                 PIC  X(05)           VALUE "QUANT".
       77  WS-HDESC                PIC  X(11)           VALUE "DESCRIPTION".
       77  WS-HCOST                PIC  X(10)           VALUE "COST PRICE".
       77  WS-HSELL                PIC  X(10)           VALUE "SELL PRICE".
       77  WS-HNAME                PIC  X(04)           VALUE "NAME".
       77  WS-HAGE                 PIC  X(12)           VALUE "AGE 30 - 180".
       77  WS-HCHQ                 PIC  X(06)           VALUE "CHEQUE".
       77  WS-HSETT                PIC  X(22)           VALUE "30  60  90 120 150 180".
       77  WS-H1                   PIC  X(05)           VALUE "Date:".
       77  WS-H3                   PIC  X(05)           VALUE "Page:".
       77  PRG-NAME                PIC  X(12)           VALUE SPACES.
       77  PRG-CRED-LUP            PIC  X(12)           VALUE "CRP\CRPLOOK".
       77  PRG-STOCK-LUP           PIC  X(12)           VALUE "STP\STPLOOK".
       77  PRG-WARHS-LUP           PIC  X(12)           VALUE "STP\WARLOOK".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.
      /
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
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
           03  CRT-START           PIC  9(02).
           03  CRT-END             PIC  9(02).
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15) VALUE "CRP011".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "CREDITOR INVOICE (STOCK)".
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
           03  WS-MODULE           PIC  X(03) VALUE "CRP".
           03  WS-PROG             PIC  X(03) VALUE "011".

       COPY "LEDTRF.WS".

       01  W00-TRAN-CODES.
           03  W00-C-ABR           PIC  X(03).
           03  W00-C-DESC          PIC  X(15).
           03  W00-C-ACT           PIC  9(01).
           03  W00-C-CODE          PIC  9(02)    COMP-5.
           03  W00-C-GL            PIC  9(06)    COMP-3.
           03  W00-C-DAY           PIC  9(06)    COMP-5.
           03  W00-C-VAL           PIC S9(09)V99 COMP-3.
           03  W00-C-MTD           PIC  9(06)    COMP-5.
           03  W00-C-MTDV          PIC S9(09)V99 COMP-3.
           03  W00-C-YTD           PIC  9(06)    COMP-5.
           03  W00-C-YTDV          PIC S9(09)V99 COMP-3.

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

       01  W02-FID.

       01  W02-PRINTER-DETAILS.
           03  W02-PRINTER         PIC  X(12).
           03  W02-PGE-LENGTH      PIC  9(03).
           03  W02-PRN-LENGTH      PIC  9(03).
           03  W02-LINAGE          PIC  9(03).
           03  W02-PRN-STATUS      PIC  X(01) VALUE "C".
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

           03  R-SL1                             REDEFINES W02-PRN-LINE.
               05  R-SLA           PIC  X(78).
               05  FILLER          PIC  X(58).

           03  R-SL2                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(58).
               05  R-SLB           PIC  X(78).

           03  R-L1                              REDEFINES W02-PRN-LINE.
               05  R-DET           PIC  X(140).

           03  R-L2                              REDEFINES W02-PRN-LINE.
               05  R-H1            PIC  X(06).
               05  R-DTE1          PIC  Z9/99/9999.
               05  FILLER          PIC  X(14).
               05  R-H2            PIC  X(40).
               05  FILLER          PIC  X(17).
               05  R-H3            PIC  X(06).
               05  R-PGE           PIC  ZZ9.

           03  R-L2A                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(38).
               05  R-HEAD          PIC  X(20).
               05  FILLER          PIC  X(38).

           03  R-L3                              REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(02).
               05  R-ORD           PIC  Z(07).
               05  FILLER          PIC  X(01).
               05  R-ITM           PIC  X(18).
               05  FILLER          PIC  X(01).
               05  R-XREF          PIC  X(18).
               05  FILLER          PIC  X(01).
               05  R-DESC          PIC  X(30).
               05  FILLER          PIC  X(01).
               05  R-BIN           PIC  X(06).
               05  FILLER          PIC  X(01).
               05  R-QNT           PIC  Z(04)9.99.
               05  FILLER          PIC  X(01).
               05  R-COST          PIC  Z(07)9.9999.
               05  FILLER          PIC  X(01).
               05  R-SELL          PIC  Z(07)9.99.
               05  FILLER          PIC  X(01).
               05  R-EXT           PIC  Z(07)9.99-.
               05  FILLER          PIC  X(03).

           03  R-L4                              REDEFINES W02-PRN-LINE.
               05  G-H1            PIC  X(10).
               05  G-SUPP          PIC  X(06).
               05  FILLER          PIC  X(01).
               05  G-NAME          PIC  X(40).
               05  FILLER          PIC  X(23).

           03  R-L4A                             REDEFINES W02-PRN-LINE.
               05  G-H3            PIC  X(04).
               05  FILLER          PIC  X(01).
               05  G-INVN          PIC  X(08).
               05  FILLER          PIC  X(01).
               05  G-H4            PIC  X(04).
               05  FILLER          PIC  X(01).
               05  G-INVD          PIC  Z9/99/9999.
               05  FILLER          PIC  X(01).
               05  G-H5            PIC  X(05).
               05  FILLER          PIC  X(01).
               05  G-RAIL          PIC  Z(04)9.99-.
               05  FILLER          PIC  X(01).
               05  G-H6            PIC  X(04).
               05  FILLER          PIC  X(01).
               05  G-DISC          PIC  Z(04)9.99-.
               05  G-T-HD.
                   07  FILLER      PIC  X(01).
                   07  G-H6A       PIC  X(04).
                   07  FILLER      PIC  X(01).
                   07  G-VAT       PIC  Z(05)9.99-.
               05  FILLER          PIC  X(01).
               05  G-H7            PIC  X(06).
               05  G-TOTAL         PIC  Z(07)9.99-.

           03  R-L9                             REDEFINES W02-PRN-LINE.
               05  REP-PG          PIC  X(06).
               05  REP-P-N         PIC  Z(03)9.
               05  FILLER          PIC  X(16).
               05  REP-COMP        PIC  X(40).
               05  FILLER          PIC  X(02).
               05  REP-REPORT      PIC  X(40).
               05  FILLER          PIC  X(08).
               05  REP-DAT         PIC  X(06).
               05  REP-DT          PIC  Z9/99/9999.

       01  W03-HEADING.
           03  FILLER              PIC  X(48)    VALUE "  ORD.NO. ITEM CODE          XREF CODE          ".
           03  W03-H3              PIC  X(31).
           03  FILLER              PIC  X(57)    VALUE "BIN    QUANTITY   UNIT-COST     UNIT-SELL    EXTENDED    ".
       
       COPY "W05.VAT".

       01  W05-EXTRA.
           03  W05-SRTE            PIC S9(03)V99 COMP-3.
           03  W05-ROUND           PIC  X(01).
           03  W05-ROUND-VAL       PIC  9V9(02).
           03  W05-RVAL              REDEFINES W05-ROUND-VAL.
               05  W05-RAND        PIC  9(01).
               05  W05-CENTS       PIC  9(02).
           03  W05-RVAL2             REDEFINES W05-ROUND-VAL.
               05  FILLER          PIC  9(01).
               05  W05-RVC1        PIC  9(01).
               05  W05-RVC2        PIC  9(01).
           03  W05-HLF-VAL         PIC  9(01)V99.
           03  W05-TQTR-VAL        PIC  9(01)V99.
           03  W05-CALC-VAL        PIC  9(02)V99.
           03  W05-CALC-RRCC         REDEFINES W05-CALC-VAL.
               05  W05-RVR1        PIC  9(01).
               05  W05-RVRCC       PIC  9(01)V99.

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

       COPY "W10.CRP".

       COPY "W15.CRP".

       01  W16-TOTALS.
           03  FILLER              PIC  X(12)           VALUE "TOTAL COST: ".
           03  W16-COST            PIC  Z(08)9.99DB.
           03  FILLER              PIC  X(12)           VALUE " TOTAL VAT: ".
           03  W16-VAT             PIC  Z(08)9.99DB.
           03  FILLER              PIC  X(08)           VALUE " TOTAL: ".
           03  W16-TOTAL           PIC  Z(08)9.99DB.

       01  W20-CALCS.
           03  W20-RESULT          PIC  9(09)V99.
           03  W20-RESULT-1                      REDEFINES W20-RESULT.
               05  W20-WHOLE       PIC  9(09).
               05  W20-DECIMAL     PIC  9(02).
           03  W20-ORD             PIC  9(07)    COMP-3.

       01  W25-CALCS.
           03  W25-RESULT          PIC  9(05)V99.
           03  W25-RESULT2                       REDEFINES W25-RESULT.
               05  W25-KEY         PIC  9(04).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).
           03  W25-INVD            PIC  9(08).

           03  W25-PDTE                          REDEFINES W25-INVD.
               05  W25-DD          PIC  9(02).
               05  W25-MM          PIC  9(02).
               05  W25-CC          PIC  9(02).
               05  W25-YY          PIC  9(02).

       01  W25-VALUES.
           03  W25-CFLOW           PIC S9(09)V99 COMP-3 OCCURS 6.
           03  W25-V1              PIC S9(09)V99 COMP-3.
           03  W25-V2              PIC S9(09)V99 COMP-3.

       01  W25-VALUES-2.
           03  W25-CFLW2           PIC S9(09)V99 COMP-3 OCCURS 6.
           03  W25-V1-2            PIC S9(09)V99 COMP-3.
           03  W25-V2-2            PIC S9(09)V99 COMP-3.

       01  W25-EDIT.
           03  W25-5.
               05  W25-1           PIC  ZZZZ9.
           03  W25-3V2.
               05  W25-2           PIC  ZZ9.99.
           03  W25-7V2.
               05  W25-3           PIC  Z(06)9.99.
           03  W25-6V2.
               05  W25-3A          PIC  Z(06)9.99.
           03  W25-3N.
               05  W25-4           PIC  ZZ9.
           03  W25-S7V2A.
               05  W25-CVAL        PIC  Z(06)9.99-.
           03  W25-DTE.
              05  W25-DATE         PIC  99/99/9999.
           03  W25-6.
               05  W25-NO          PIC  Z(07)9.
           03  W25-S5V2.
               05  W25-SVAL        PIC  Z(07)9.99DB.
           03  W25-S7V2.
               05  W25-VAL2        PIC  Z(06)9.99DB.

           03  W25-5V2                           REDEFINES W25-S7V2.
               05  W25-VAL         PIC  Z(07)9.99-.
           03  W25-72.
               05  W25-VAL3        PIC S9(07)V99.
           03  W25-PRC.
               05  W25-IPRC        PIC  Z(06)9.999.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W50-H4.
           03  W50-HEAD            PIC  X(20).

       01  W55-CALCS.
           03  W55-VAT             PIC S9(07)V99 COMP-3.

       01  W60-H2.
           03  W60-COMPANY         PIC  X(40).
           03  W60-DESC            PIC  X(21)           VALUE "CREDITORS AUDIT TRAIL".

       01  W65-CREDITOR.
           03  W65-CFLOW           PIC S9(09)V99 COMP-3 OCCURS 6.
           03  W65-V1              PIC S9(09)V99 COMP-3.
           03  W65-V2              PIC S9(09)V99 COMP-3.

       COPY "W70CRED.WS".

       01  W75-KEYS.
           03  W75-S               PIC  9(02)    COMP-5.
           03  W75-S1              PIC  9(02)    COMP-5.
           03  W75-S2              PIC  9(02)    COMP-5.
           03  W75-CNO                           OCCURS 34.
               05  W75-WHCDE.
                   07  W75-PKEY    PIC  9(05).
                   07  FILLER      PIC  X(01).
           03  W75-ITEMS                         OCCURS 34.
               05  W75-CODE        PIC  X(14).
               05  W75-XCODE       PIC  X(14).
               05  W75-POR         PIC  9(10).

       01  W75-STORE.
           03  W75-X-C             PIC  X(01).

       01  W90-TRAN.
           03  W90-CODE            PIC  9(02).
           03  W90-RESULT          PIC  9(04)    COMP-5.
           03  W90-TRNKEY          PIC  9(04)    COMP-5.
           03  W90-TS              PIC  9(02)    COMP-5.

       01  W100-EDIT.
           03  W100-V2.
               05  W100-S6V2       PIC  Z(05)9.99-.
           03  W100-V3                           REDEFINES W100-V2.
               05  W100-PRICE      PIC  X(09).
               05  FILLER          PIC  X(01).
           03  W100-V4                           REDEFINES W100-V2.
               05  W100-EXT        PIC  Z(06)9.99.
           03  W100-V5                           REDEFINES W100-V2.
               05  W100-QNT        PIC  Z(05)9.99-.
           03  W100-S8V2           PIC  Z(07)9.999.
           03  W100-S8V3           PIC  Z(06)9.9999-.

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
      *                                         1         2         3         4         5         6         7         8
      *                                12345678901234567890123456789012345678901234567890123456789012345678901234567890
      *
           03  LINE  6 COLUMN  2 VALUE "Creditor Ac. :".
           03  LINE  7 COLUMN  2 VALUE "Invoice no   :".
           03          COLUMN 56 VALUE                                                       "Invoice date:".
           03  LINE  8 COLUMN  2 VALUE "Order number :".
           03  LINE  9 COLUMN  2 VALUE "Item code    :".
           03  LINE 10 COLUMN  2 VALUE "Quantity(Pur):".
           03          COLUMN 31 VALUE                              "Sell(Units):".
           03  LINE 11 COLUMN  2 VALUE "Unit cost    :".
      *
      *    Future development to allow for exchange rate and other currencies
      *
      *    03          COLUMN 31 VALUE "Rate       :".
      *    03          COLUMN 58 VALUE "Currency :".
      *    03  LINE 12 COLUMN  2 VALUE "Rand cost    :".
           03          COLUMN 31 VALUE                              "Current    :".
           03  LINE 13 COLUMN  2 VALUE "Discount-1   :".
           03          COLUMN 31 VALUE                              "Discount-2 :".
           03  LINE 14 COLUMN  2 VALUE "Unit nett Pur:".
           03          COLUMN 31 VALUE                              "VAT        :".
           03          COLUMN 56 VALUE                                                       "New average :".
           03  LINE 15 COLUMN  2 VALUE "Sell. prices -".
           03          COLUMN 17 VALUE                " Wholesale".
           03          COLUMN 38 VALUE                                     " Cash".
           03          COLUMN 52 VALUE                                                   " Retail".
           03  LINE 16 COLUMN  2 VALUE "VAT exclusive:".
           03  LINE 17 COLUMN  2 VALUE "VAT inclusive:".
           03  LINE 18 COLUMN  2 VALUE "Cur exclusive:".
           03  LINE 19 COLUMN  2 VALUE "VAT          :".
           03  LINE 20 COLUMN  2 VALUE "Excl - cost  :".
           03          COLUMN 54 VALUE                                                     "Sub total :".
           03  LINE 21 COLUMN  2 VALUE "Incl - cost  :".
           03          COLUMN 54 VALUE                                                     "Freight   :".
           03  LINE 22 COLUMN 54 VALUE                                                     "Discount  :".
           03  LINE 23 COLUMN 54 VALUE                                                     "VAT       :".
           03  LINE 24 COLUMN 54 VALUE                                                     "Total     :".

       01  S03-VIEW.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE  8 COLUMN  1 VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ´".
               05  LINE  9 COLUMN  1 VALUE "³    Item Code     ³       Description        ³ Quantity ³Unit Price³ Extended ³".
               05  LINE 10 COLUMN  1 VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄ´".
               05  LINE 12 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 13 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 14 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 15 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 16 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 17 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 18 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 19 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 20 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 21 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 22 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 23 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 24 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 25 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 26 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 27 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 28 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 29 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 30 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 31 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 32 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 33 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 34 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 35 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 36 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 37 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 38 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 39 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 40 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 41 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 42 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 43 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 44 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 45 COLUMN  1 VALUE "³                  ³                          ³          ³          ³          ³".
               05  LINE 46 COLUMN  1 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÙ".

       01  S03-VIEW-LINE.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE CLIN COLUMN  2 PIC  X(18)     FROM PUR-EXT-STOCK.
               05            COLUMN 21 PIC  X(26)     FROM STK-DSC26.
               05            COLUMN 48 PIC  Z(06)9.99 FROM PUR-IUN.
               05            COLUMN 59 PIC  Z(06)9.99 FROM PUR-ICOST.
               05            COLUMN 70 PIC  Z(06)9.99 FROM PUR-COST.

       01  S03-USER-OPT.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
               05  LINE 50 COLUMN  3 VALUE " "                                                FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN  6 VALUE    " highlight - ".
               05          COLUMN 19 VALUE                 "PgUp"                                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 23 VALUE                     "/".
               05          COLUMN 24 VALUE                      "PgDn"                              FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 28 VALUE                          " - "
               05          COLUMN 31 VALUE                             "ENTER"                      FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 36 VALUE                                  " Select - ".
               05          COLUMN 46 VALUE                                            "Esc"         FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 49 VALUE                                               " to Exit".
       
       01  S-ITEM-CODE.
      *
      *    ****   Set the colours to be used for these screens 
      * 
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  S07.
                   07  PIC X(14) USING W10-ITEM AUTO.
               
               05  S07-ITM.
                   07  PIC X(14) USING W10-ITEM AUTO.
                   07  PIC X(03) USING W10-ITM AUTO.
               
               05  S07-EXT.
                   07  PIC X(18) USING W10-EXT-ITEM AUTO.
               
               05  S07-3.
                   07  PIC Z(03) USING W10-I4 AUTO.
               
               05  S07-4.
                   07  PIC Z(04) USING W10-I4 AUTO.
               
               05  S07-5.
                   07  PIC Z(05) USING W10-I5 AUTO.
               
               05  S07-6.
                   07  PIC Z(06) USING W10-I6 AUTO.
               
               05  S07-7.
                   07  PIC Z(07) USING W10-I7 AUTO.
               
               05  S07-8.
                   07  PIC Z(08) USING W10-I8 AUTO.
               
               05  S07-9.
                   07  PIC Z(09) USING W10-I9 AUTO.
               
               05  S07-10.
                   07  PIC Z(10) USING W10-I10 AUTO.
               
               05  S07-11.
                   07  PIC Z(11) USING W10-I11 AUTO.
               
               05  S07-12.
                   07  PIC Z(12) USING W10-I12 AUTO.
               
               05  S07-13.
                   07  PIC Z(13) USING W10-I13 AUTO.
               
               05  S07-14.
                   07  PIC Z(14) USING W10-I14 AUTO.

       01  S08.
           02  BACKGROUND-COLOR Grey.
           03  LINE 10 COLUMN 26 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"  FOREGROUND-COLOR Black.
           03          COLUMN 54 VALUE                             "¿" FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE 11 COLUMN 26 VALUE "³"                             FOREGROUND-COLOR Black.
           03          COLUMN 27 VALUE  " Number of Cases           "  FOREGROUND-COLOR Red.
           03          COLUMN 54 VALUE                             "³" FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE 12 COLUMN 26 VALUE "À"                             FOREGROUND-COLOR Black.
           03          COLUMN 27 VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S09.
           03  LINE 11 COLUMN 44 PIC ZZZZZZZZ9 USING W10-CASES AUTO FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.

       01  S-ACCEPT-FIELDS.
           03  FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
               05  S19.
                   07  LINE  6 COLUMN 17 PIC X(06)       USING W15-ACNO AUTO.

               05  S21.
                   07  LINE  8 COLUMN 17 PIC Z(07)9      USING W15-OPREF AUTO.

               05  S22.
                   07  LINE  8 COLUMN 26 PIC 99          USING W15-OSUB AUTO.

               05  S29.
                   07  LINE  7 COLUMN 17 PIC X(08)       USING W15-INVN AUTO.

               05  S30.
                   07  LINE  7 COLUMN 70 PIC Z9/99/9999  USING W15-INVD AUTO.

               05  S33.
                   07  LINE 21 COLUMN 66 PIC Z(07)9.99DB USING W15-RAIL AUTO.

               05  S34.
                   07  LINE 22 COLUMN 66 PIC Z(07)9.99DB USING W15-DISCOUNT AUTO.

               05  S35.
                   07  LINE 13 COLUMN 23 PIC Z9.99       USING W15-DISC1 AUTO.

               05  S36.
                   07  LINE 13 COLUMN 50 PIC Z9.99       USING W15-DISC2 AUTO.

      *
      *    ****    F O R   C O N V E R T I O N S
      *            PURCHASE AND SELLING UNITS ARE DIFFERENT
      *            BUY PER CASE - SELL SINGLES
      *            BUY PER TON - SELL PER KG
      *
               05  S37.
                   07  LINE 10 COLUMN 17 PIC Z(06)9.9(04) USING W15-PUN AUTO.

               05  S37S.
                   07  LINE 10 COLUMN 44 PIC Z(06)9.999   USING W15-IUN AUTO.

      *        05  S37-3.
      *            07            PIC Z(06)9.999   USING W15-IUN AUTO.

               05  S38.
                   07  LINE 11 COLUMN 17 PIC Z(06)9.9999  USING W15-PCOST AUTO.

               05  S40.
                   07  LINE 20 COLUMN 17 PIC Z(07)9.999   USING W15-COST AUTO.

       01  S41.
           03  FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
               05  S41A-C.
                   07  S41A.
                       09  LINE 16 COLUMN 17 PIC Z(07)9.999 USING W15-UWSALE AUTO.
                   07  S41B.
                       00  LINE 16 COLUMN 33 PIC Z(07)9.999 USING W15-UCASH AUTO.
                   07  S41C.
                       09  LINE 16 COLUMN 49 PIC Z(07)9.999 USING W15-USELL AUTO.
               05  S41D.
                   07  S41D-1.
                       09  LINE 17 COLUMN 17 PIC Z(07)9.999 USING W10-VWSALE AUTO.
                   07  S41D-2.
                       09  LINE 17 COLUMN 33 PIC Z(07)9.999 USING W10-VCASH AUTO.
                   07  S41D-3.
                       09  LINE 17 COLUMN 49 PIC Z(07)9.999 USING W10-VSELL AUTO.

       01  S42.
           03  FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
               05  S42A.
                   07  LINE  6 COLUMN 17 PIC  X(06) FROM WS-SPC.
                   07          COLUMN 24 PIC  X(40) FROM WS-SPC.
                   07  LINE  7 COLUMN 17 PIC  X(08) FROM WS-SPC.
                   07          COLUMN 70 PIC  X(10) FROM WS-SPC.
               05  S42B.
                   07  LINE  8 COLUMN 17 PIC  X(11) FROM WS-SPC.
                   07  LINE  9 COLUMN 17 PIC  X(14) FROM WS-SPC.
                   07          COLUMN 36 PIC  X(30) FROM WS-SPC.
                   07  LINE 10 COLUMN 17 PIC  X(12) FROM WS-SPC.
                   07          COLUMN 44 PIC  X(11) FROM WS-SPC.
                   07  LINE 11 COLUMN 17 PIC  X(12) FROM WS-SPC.
                   07          COLUMN 44 PIC  X(12) FROM WS-SPC BACKGROUND-COLOR Blue.
                   07  LINE 13 COLUMN 23 PIC  X(05) FROM WS-SPC.
                   07          COLUMN 50 PIC  X(05) FROM WS-SPC.
                   07  LINE 14 COLUMN 17 PIC  X(12) FROM WS-SPC.
                   07          COLUMN 44 PIC  X(11) FROM WS-SPC.
                   07          COLUMN 70 PIC  X(10) FROM WS-SPC.
                   07  LINE 16 COLUMN 17 PIC  X(12) FROM WS-SPC.
                   07          COLUMN 33 PIC  X(12) FROM WS-SPC.
                   07          COLUMN 49 PIC  X(12) FROM WS-SPC.
                   07  LINE 17 COLUMN 17 PIC  X(12) FROM WS-SPC.
                   07          COLUMN 33 PIC  X(12) FROM WS-SPC.
                   07          COLUMN 49 PIC  X(12) FROM WS-SPC.
                   07  LINE 18 COLUMN 17 PIC  X(12) FROM WS-SPC BACKGROUND-COLOR Blue.
                   07          COLUMN 33 PIC  X(12) FROM WS-SPC BACKGROUND-COLOR Blue.
                   07          COLUMN 49 PIC  X(12) FROM WS-SPC BACKGROUND-COLOR Blue.
                   07  LINE 19 COLUMN 17 PIC  X(11) FROM WS-SPC.
                   07  LINE 20 COLUMN 17 PIC  X(12) FROM WS-SPC.
                   07  LINE 21 COLUMN 17 PIC  X(11) FROM WS-SPC.
               05  S42C.
                   07  LINE 20 COLUMN 66 PIC  X(13) FROM WS-SPC.
                   07  LINE 21 COLUMN 66 PIC  X(13) FROM WS-SPC.
                   07  LINE 22 COLUMN 66 PIC  X(13) FROM WS-SPC.
                   07  LINE 23 COLUMN 66 PIC  X(13) FROM WS-SPC.
                   07  LINE 24 COLUMN 66 PIC  X(13) FROM WS-SPC.

       01  S-TOTAL-FIELDS.
           03  FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
               05  S43.
                   07  LINE 21 COLUMN 66 PIC Z(07)9.99DB USING W15-VAT AUTO.

               05  S45.
                   07  LINE 19 COLUMN 17 PIC Z(07)9.99   USING W15-IVAT AUTO.

               05  S46.
                   07  LINE 21 COLUMN 17 PIC Z(07)9.99   USING W15-INCOST AUTO.

               05  S47.
                   07  LINE 14 COLUMN 17 PIC Z(07)9.999  USING W15-U-PNETT AUTO.

               05  S48.
                   07  LINE 14 COLUMN 44 PIC Z(07)9.99   USING W15-U-VAT AUTO.

       01  S55.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
               05  LINE 50 COLUMN  2 VALUE "F1"                                           FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN  4 VALUE   "=Help,".
               05          COLUMN 10 VALUE         "F2"                                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 12 VALUE           " to do Creditor Lookup ".
               05          COLUMN 35 VALUE                                  "Esc"         FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 38 VALUE                                     " to exit".

       01  S56.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
               05  LINE 50 COLUMN  2 VALUE "F1"                                                             FOREGROUND-COLOR Brown HIGHLIGHT
               05  LINE    COLUMN  4 VALUE   "=Help,".
               05  LINE    COLUMN 10 VALUE         "F2"                                                     FOREGROUND-COLOR Brown HIGHLIGHT
               05  LINE    COLUMN 12 VALUE           "=Item/".
               05  LINE    COLUMN 18 VALUE                 "F3"                                             FOREGROUND-COLOR Brown HIGHLIGHT
               05  LINE    COLUMN 20 VALUE                   "=Alt/".
               05  LINE    COLUMN 25 VALUE                        "F4"                                      FOREGROUND-COLOR Brown HIGHLIGHT
               05  LINE    COLUMN 27 VALUE                          "=Desc/".
               05  LINE    COLUMN 33 VALUE                                "F5"                              FOREGROUND-COLOR Brown HIGHLIGHT
               05  LINE    COLUMN 35 VALUE                                  "=Desc2/".
               05  LINE    COLUMN 42 VALUE                                         "F6"                     FOREGROUND-COLOR Brown HIGHLIGHT
               05  LINE    COLUMN 44 VALUE                                           "=X/Ref Find,".
               05  LINE    COLUMN 55 VALUE                                                       "F8"       FOREGROUNDCOLOR Red HIGHLIGHT
               05  LINE    COLUMN 57 VALUE                                                         " Create".

       01  S-DISPLAY-FIELDS.
           03  FOREGROUND-COLOR Magenta BACKGROUND-COLOR Grey.
               05  SD-XREF.
                   07  COLUMN 22 PIC X(14) FROM STK-XREF.

               05  SD-ITEM.
                   07  COLUMN 22 PIC X(14) FROM W75-CODE(W75-S2).

               05  SD-XCDE.
                   07  COLUMN 22 PIC X(14) FROM W75-XCODE(W75-S2).

      *COPY "CRDLUP.CRT".

       01  S-CRED.
           03  LINE 23 COLUMN  2 VALUE "Balance :".
           03          COLUMN 12 PIC Z(08)9.99DB USING P-BAL FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
           03          COLUMN 28 VALUE "Due :".
           03          COLUMN 34 PIC Z(08)9.99DB USING P-OUT FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.

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
       AA000-START-UP  SECTION.
       AA000.
           IF LS0-CRLEV < 1
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *     **  *   *   * *   *  *  **  *   *   *     *   *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *     *  **   *     *   *  *  *  **   *   *     *   *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** *   *
      *
             PERFORM OPEN-PRINTER.
             MOVE "P"                TO WS-COMMAND.
             MOVE 0                  TO WS-ADVANCE.
             MOVE 2                  TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.

       AA05.
             PERFORM BA000.
           IF NOT(WS-OPTION = "X")
               PERFORM BA036
               MOVE "Y"              TO WS-LOOP
               GO TO AA05.
             MOVE 0                  TO WS-ADVANCE.
             MOVE 1                  TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "C"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             CLOSE BARCOD PRCLAB PURCH RECOVER.

       AA49.
             EXIT PROGRAM.

       COPY "AA50CSW.LUP".

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

       COPY "LOCKED.REC".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                        OPT-MESSAGE                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine is used to allow the OPERATOR to respond to  ³
      *    ³ a request for an option, so that the correct action can   ³
      *    ³ be performed by the program. The routine will display the ³
      *    ³ message in a pop-up window and allow the OPERATOR to      ³
      *    ³ respond to the 'question'.                                ³
      *    ³                                                           ³
      *    ³ The option request must be placed in WS-ERR-MES and may   ³
      *    ³ not exceed 48 characters. The message will be centred in  ³
      *    ³ the window. An example of a message request follows:      ³
      *    ³                                                           ³
      *    ³   MOVE "Print transactions (Y/N) [ ]" TO WS-ERR-MES.      ³
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

      *
      *    ****    P R I N T   U T I L I T Y   -   A L L O W S
      *              F O R   P R I N T E R   A N D   D I S K
      *
       COPY "PRINTUTL.AS4".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AB00-READ      SECTION.

       COPY "CONTROL.RD".

       COPY "CRTRAN.RD".

      *
      *     ***  ****  ***** ****  *** *****     ***** *** *     *****
      *    *   * *   * *     *   *  *    *       *      *  *     *
      *    *     *   * *     *   *  *    *       *      *  *     *
      *    *     ****  ***   *   *  *    *       ***    *  *     ***
      *    *     *   * *     *   *  *    *       *      *  *     *
      *    *   * *   * *     *   *  *    *       *      *  *     *
      *     ***  *   * ***** ****  ***   *       *     *** ***** *****
      *
       READ-CREDIT.
             READ CREDIT
               KEY IS P-NUMBER.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
               GO TO READ-CREDIT-EXIT.
           IF WS-STATUS = "23"
               MOVE 3            TO WS-F-ERROR
               GO TO READ-CREDIT-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 3            TO WS-F-ERROR
               PERFORM READ-ERROR.
           IF RECORD-LOCKED
               PERFORM LOCKED-RECORD
               GO TO READ-CREDIT.
             GO TO READ-CREDIT-EXIT.

       READ-CREDIT-LOCK.
             READ CREDIT WITH KEPT LOCK
               KEY IS P-NUMBER.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
               GO TO READ-CREDIT-EXIT.
           IF WS-STATUS = "23"
               MOVE 3            TO WS-F-ERROR
               GO TO READ-CREDIT-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 3            TO WS-F-ERROR
               PERFORM READ-ERROR.
           IF RECORD-LOCKED
               PERFORM LOCKED-RECORD
               GO TO READ-CREDIT-LOCK.
           IF TOO-MANY-LOCKS
               UNLOCK CREDIT
               GO TO READ-CREDIT-LOCK.
             GO TO READ-CREDIT-EXIT.

       READ-CREDIT-NEXT.
             READ CREDIT NEXT.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
               GO TO READ-CREDIT-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
               MOVE 3            TO WS-F-ERROR
               GO TO READ-CREDIT-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 3            TO WS-F-ERROR
               PERFORM READ-ERROR.
           IF RECORD-LOCKED
               PERFORM LOCKED-RECORD
               GO TO READ-CREDIT-NEXT.
             GO TO READ-CREDIT-EXIT.

       READ-CREDIT-PREV.
             READ CREDIT PREVIOUS.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
               GO TO READ-CREDIT-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
               MOVE 3            TO WS-F-ERROR
               GO TO READ-CREDIT-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 3            TO WS-F-ERROR
               PERFORM READ-ERROR.
           IF RECORD-LOCKED
               PERFORM LOCKED-RECORD
               GO TO READ-CREDIT-PREV.
             GO TO READ-CREDIT-EXIT.

       START-AT-CRED-NAME.
             START CREDIT
               KEY >= P-NKEY.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
               GO TO READ-CREDIT-EXIT.
           IF WS-STATUS = "23"
               MOVE 3            TO WS-F-ERROR
               GO TO READ-CREDIT-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 3            TO WS-F-ERROR
               PERFORM READ-ERROR.

       READ-CREDIT-EXIT.
             EXIT.

       COPY "DEPART.RD".

       COPY "LEDTRF.RD".

       COPY "PARAM.RD".

       COPY "PORDER.RD".

       COPY "PURCH.RD".

       COPY "SHARED.RD".

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

       READ-STOCK-PREV.
             READ STOCK PREVIOUS WITH IGNORE LOCK.
           IF WS-STATUS = "00"
               IF STK-DISC NOT = 9
                   MOVE ZERO         TO WS-F-ERROR
                   GO TO READ-STOCK-EXIT
               ELSE
                   GO TO READ-STOCK-PREV.
             GO TO STOCK-CHECK-STATUS.

       START-AT-STOCK-CODE.
             START STOCK KEY >= STK-CODE.
             GO TO STOCK-CHECK-STATUS.

       START-AT-ALT-CODE.
             START STOCK KEY >= STK-ACODE.
             GO TO STOCK-CHECK-STATUS.

       START-AT-STOCK-DESC.
             START STOCK KEY >= STK-DKEY.
             GO TO STOCK-CHECK-STATUS.

       START-AT-STOCK-XREF.
             START STOCK KEY >= STK-XREF.

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

       COPY "WARHSE.RD".

       COPY "WSTOCK.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AC000-WRITE   SECTION.

       COPY "CONTROL.WR".

       COPY "CRTRAN.WR".

       COPY "CREDIT.WR".

       COPY "DEPART.WR".

       COPY "LEDTRF.WR".

       COPY "PARAM.WR".

       COPY "PORDER.WR".

       COPY "PRCLAB.WR".

       COPY "PURCH.WR".

       COPY "PURDEX.WR".

       COPY "SHARED.WR".

       COPY "STOCK.WR".

       COPY "TXTRAN.WR".

       COPY "WSTOCK.WR".

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
      *        ³ 0 = RECORD CHANGED          ÄÄ      (REWRITE)    ³
      *        ³ 1 = RECORD WAS ADDED        ÄÄ      (DELETE)     ³
      *        ³ 2 = RECORD WAS DELETED      ÄÄ      (WRITE)      ³
      *        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY000        SECTION.
      *    *************************************************************
      *    ****             P A R A M E T E R   F I L E             ****
      *    *************************************************************
       AY01.
             MOVE 01                 TO REC-FILE.
             MOVE WS-PARKEY          TO REC-KEY.
             MOVE ZERO               TO REC-TYPE
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-RECORD1        TO REC-PARAM.
             GO TO AY50.
      *    *************************************************************
      *    ****                 S T O C K   F I L E                 ****
      *    *************************************************************
       AY05.
             MOVE 05                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE ZERO               TO REC-TYPE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
             MOVE STK-RECORD1        TO REC-STOCKF.
             GO TO AY50.
      *    *************************************************************
      *    ****              C R E D I T O R   F I L E              ****
      *    *************************************************************
       AY07.
             MOVE 07                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-CREDIT-LOCK THRU READ-CREDIT-EXIT.
             MOVE P-REC              TO REC-CREDIT.
             GO TO AY50.
      *    *************************************************************
      *    ****  C R E D I T O R   T R A N S A C T I O N   F I L E  ****
      *    *************************************************************
       AY08.
             MOVE 08                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
             MOVE CT-REC             TO REC-CRTRAN.
             GO TO AY50.
      *    *************************************************************
      *    ****        P U R C H A S E   O R D E R   F I L E        ****
      *    *************************************************************
       AY10.
             MOVE 10                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-PORDER THRU READ-PORDER-EXIT
               IF WS-F-ERROR = 16
                   GO TO AY59.
             MOVE ORD-RECORD1        TO REC-PORDER.
             GO TO AY50.
      *    *************************************************************
      *    ****       V A L U E   A D D E D   T A X   F I L E       ****
      *    *************************************************************
       AY19.
             MOVE 19                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE.
             MOVE TAX-RECORD1        TO REC-TXTRAN.
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
             MOVE ZERO               TO REC-TYPE.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
           IF WS-F-ERROR = 7
               MOVE "XXXX"           TO DPT-CODE
               GO TO AY38.
             MOVE DPT-RECORD         TO REC-DEPART.
             GO TO AY50.
      *    *************************************************************
      *    ****               C O N T R O L   F I L E               ****
      *    *************************************************************
       AY39.
             MOVE 39                 TO REC-FILE.
             MOVE WS-NETKEY          TO REC-KEY.
             MOVE ZERO               TO REC-TYPE.
             MOVE NET-RECORD         TO REC-NETWORK.
             GO TO AY50.
      *    *************************************************************
      *    ****           I N T E G R A T I O N   F I L E           ****
      *    *************************************************************
       AY40.
             MOVE 40                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
             MOVE XFR-REC            TO REC-LEDTRF.
             GO TO AY50.

       AY056.
             MOVE 56                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
             MOVE PRD-REC1           TO REC-PURDEX.
             GO TO AY50.

       AY052.
             MOVE 52                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-WSTOCK THRU READ-WSTOCK-EXIT.
             MOVE WST-RECORD1        TO REC-WSTOCK.
             GO TO AY50.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³              End transaction in recovery log              ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY49.
             MOVE 99                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE SPACES             TO REC-DETAIL.
             PERFORM AY50.
             ADD 1                   TO WS-TRANS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If recovery records exceed 195 - Clear (Close and Open    ³
      *    ³ new). This allows for quicker recoveries when required.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF WS-RECOVER > 195
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
               DISPLAY "Write error Recovery file - Status " AT 2212 WITH FOREGROUND-COLOR Brown HIGHLIGHT
               DISPLAY WS-STATUS                             AT 2247 WITH FOREGROUND-COLOR Grey HIGHLIGHT
               STOP RUN.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
       AY59.
             EXIT.
      *
      *    ****    Start processing transaction
      *
       AY60.
             MOVE 1                  TO WS-COUNT.
             MOVE 5                  TO WS-SHARED.
             PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
             MOVE SHR-STOCK          TO WS-STOCK.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-USERS < 24
               MOVE 1                TO WS-SUB
               MOVE ZERO             TO WS-WAIT
               GO TO AY62.
      *
      *    ****   Q   F U L L  -  W A I T   F O R   4   S E C O N D S
      *
             DISPLAY "WAITING" AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red HIGHLIGHT.
             COMMIT.
             ACCEPT WS-STIME FROM TIME.
             MOVE 400                TO WS-WAIT.
             PERFORM LOCK-REC-LOOP.
             DISPLAY SPACE AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.
             GO TO AY60.

       AY61.
      *
      *    ****    Updating Creditor and Stock Files
      *
             MOVE "CS"               TO PAR-PROG(WS-USUB).
             MOVE LS-USER            TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the CREDITOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
             MOVE 2                  TO WS-NETKEY.
             PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             PERFORM AY39 THRU AY59.
             MOVE NET-CREDITOR       TO W70-CREDCON.
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
      *    ****   A R E   A N Y   U P D A T E S   I N   P R O G R E S S
      *
       AY62.
           IF NOT(PAR-PROG(WS-SUB) = SPACES OR PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   C R E D I T O R   O R   S T O C K   F I L E S
      *           B E I N G   U P D A T E D
      *
               IF NOT(PAR-PROG(WS-SUB) = SPACES)
                   IF PAR-PROG(WS-SUB) = "CR" OR "CS" OR "CG" OR "ST" OR "DS"
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
      *
      *    ****    End transaction
      *
       AY70.
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 2.
      *
           IF WS-STOCK = "Y"
               MOVE NET-RECORD       TO SHR-RECORD
               PERFORM REWRITE-SHARED THRU WRITE-SHARED-EXIT
           ELSE
               PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
             MOVE 2                  TO WS-NETKEY.
             MOVE W70-CREDCON        TO NET-CREDITOR.
             PERFORM REWRITE-CONTROL-UNLOCK THRU WRITE-CONTROL-EXIT.
             PERFORM AY49 THRU AY59.
             MOVE 1                  TO WS-USUB.

       AY72.
           IF NOT(PAR-USR(WS-USUB) = LS-USER)
               ADD 1                 TO WS-USUB
               GO TO AY72.

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
            SUBTRACT 1 FROM WS-USUB  GIVING PAR-USERS.
            PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
            COMMIT.

       AY999.
            EXIT.

       COPY "CA000.STK".

      *    *************************************************************
      *    ****   G E T   S T O C K   R E C O R D   U S I N G
      *           A L T E R N A T E   C O D E
      *    *************************************************************
       CA100      SECTION.
       CA101.
             MOVE 10                 TO W50-LENGTH.

       CA105.
             MOVE W10-ACODE          TO W50-DATA.
           IF W50-1 = SPACE
               MOVE W50-9            TO W10-ACODE
               GO TO CA105.
             CALL "CBL_TOUPPER" USING W50-DATA BY VALUE W50-LENGTH RETURNING W50-STATUS.
             MOVE W50-ACODE          TO STK-ACODE W10-ACODE.

       CA119.
             EXIT.

      *
      *    ****    Read/Write Creditor record
      *
      *    This routine is performed and the following are the variables
      *    that are required to have been set up.
      *
      *          VARIABLES USED FOR CRPRDWR as instructions and data
      *
      *    03  W15-INSTR             PIC  X(01).
      *
      *        88  ADD-ACCOUNT                     VALUE "A".
      *
      *            W15-CREDREC needs to contain details for the account 
      *                        that must be added
      *
      *        88  DELETE-ACT                      VALUE "D".
      *
      *            W15-CREDREC needs to contain the details for the 
      *                        account to be deleted
      *
      *        88  GET-ACCOUNT                     VALUE "G".
      *
      *            W15-LINCOL must have the screen location of where the
      *                       User will key in the account number.
      *                            eg. 0617 = Line 6 Column 17.
      *
      *            W15-BGRND  The Background colour that must be used.
      *            W15-FGRND  The Foreground colour that must be used.
      *                       The folloeing values are available in the
      *                       COPY file "WS.WS" that is included in the
      *                       WORKING-STORAGE SECTION.
      *
      *                           78  Black     value 0.
      *                           78  Blue      value 1.
      *                           78  Green     value 2.
      *                           78  Cyan      value 3.
      *                           78  Red       value 4.
      *                           78  Magenta   value 5.
      *                           78  Brown     value 6.
      *                           78  Grey      value 7.
      *
      *                       If the screen colours for the calling 
      *                       program are Black on White(Grey) then
      *                       that would be setup as follows:
      *
      *                           MOVE Grey  TO W15-BGRND.
      *                           MOVE Black TO W15-FGRND.
      *
      *        88  READ-LOCK                       VALUE "L".
      *
      *            W15-ACNO must contain the Account Number of the 
      *                     account record to be read and locked.
      *
      *        88  READ-RECORD                     VALUE "R".
      *
      *            W15-ACNO must contain the Account Number of the 
      *                     account record to be read.
      *
      *        88  UPDATE-ACT                      VALUE "U".
      *
      *            W15-CREDREC needs to contain the details for the 
      *                        account record to be updated.
      *
      *        88  UPDATE-UNL                      VALUE "X".
      *
      *            W15-CREDREC must contain the the details for the  
      *                        account record to be updated and unlocked.
      *
      *    This variable informs the called program to write details of
      *    the recoosd to the RECOVERY file.
      * 
      *    03  W15-RECOVER.
      *        05  W15-RECV        PIC  X(01)      VALUE "N".
      *            88  ADD-TO-RECV                 VALUE "Y".
      * 
      *    If the record must be written to the RECOVERY file; the 
      *    calling program must MOVE "Y" TO W15-RECV before calling 
      *    CRPRDWR. CRPRDWR will always change it back to "N" before 
      *    returning control to the calling program.
      *
      *    When a read CREDITOR account has been requested WITH W15-RECV
      *    havinge a value of "N" then the program will exit with the
      *    account record in W15-CREDREC. If W15_RECV = "Y" then 
      *    W15-CREDREC remains unchanged.
      *
      *    On exiting W15-ERROR will be 0 if successful or contain and 
      *    error code for unsuccessful requests:
      *               0 = successful
      *               1 = No Record
      *               2 = account number on File not the same as requested
      *               9 = CRPRDWR called with incorrect instruction
      *
              
       COPY "CA155.CRP".

       COPY "CA200.STK".

       COPY "CA500.STK".

       COPY "LEDTRF.UPD".

      *
      *            ***   ***   ***  ****   *** 
      *           *   * *   * *   * *   * *    
      *           *     *   * *   * *   *  *** 
      *           *  ** *   * *   * *   *     *
      *            ***   ***   ***  ****   *** 
      *
      *  ****  *****  ***  ***** ***** *   * ***** **** 
      *  *   * *     *   * *       *   *   * *     *   *
      *  ****  ***   *     ***     *   *   * ***   *   *
      *  *  *  *     *   * *       *    * *  *     *   *
      *  *   * *****  ***  ***** *****   *   ***** **** 
      *
       BA000      SECTION 5.
       BA00A.
           IF WS-LOOP = "Y"
               GO TO BA00.
             MOVE "GOODS RECEIVED"   TO W50-HEAD.
             MOVE ZERO               TO W15-TOTAL WS-PAGE.

       BA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S03.
             DISPLAY S42.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Is an 18 character stock/inventory item code being used?  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF WS-EXT-STK = "Y"
               DISPLAY "    " AT 0931 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black
           ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Is a 3 character extension code being used in addition to ³
      *    ³ the 14 character stock/inventory item code?               ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF WS-USE-ITM = "Y"
               DISPLAY "/"   AT 0931 WITH BACKGROUNDCOLOR Blue FOREGROUNDCOLOR Brown HIGHLIGHT
                        "   "        WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
             MOVE 1                  TO WS-PURLAST.
             MOVE SPACES             TO W15-ACNO.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     Allow user to key in the Creditor account number.     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA05.
             DISPLAY S55.
             MOVE 0617               TO 
             ACCEPT S19.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  MOVE SPACES TO W15-ACNO
                               GO TO BA06
                 WHEN F1-KEY   PERFORM HELP-ROUTINE
                 WHEN F2-KEY   PERFORM AA50C THRU AA59
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO BA05
               END-EVALUATE
               DISPLAY S19
               IF W15-ACNO = SPACES
                   GO TO BA05.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W15-ACNO BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.

       BA06.
             PERFORM CLEAR-L50.
           IF W15-ACNO = SPACES
               MOVE 1   TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE "TOTAL"          TO G-T-HD
               MOVE W15-TOTAL        TO G-TOTAL
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE SPACES           TO R-DET
               MOVE "X"              TO WS-OPTION
               GO TO BA35-EXIT.


             PERFORM CA155-IO-CREDITOR.


           IF WS-ERROR NOT = ZERO
               MOVE WS-ER1           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BA05.
             MOVE P-NAME             TO W15-NAME.
             DISPLAY P-NAME          AT 0624 WITH BACKGROUND-COLOR Grey FOREGROUNDCOLOR Blue.
           IF P-TAX = "E"
               DISPLAY "(Exclusive)" AT 0665 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
           ELSE
           IF P-TAX = "I"
               DISPLAY "(Inclusive)" AT 0665 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
           ELSE
               DISPLAY "(No VAT)"    AT 0665 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY S-CRED.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO BA05.
             MOVE SPACES             TO W15-INVN.
             MOVE ZERO               TO W15-INVD.
             MOVE 1                  TO WS-S.

       BA10.
             MOVE ZERO               TO W65-CFLOW(WS-S).
           IF WS-S < 6
               ADD 1                 TO WS-S
               GO TO BA10.

       BA12.
             ACCEPT S29.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  GO TO BA05
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO BA12
               END-EVALUATE.
           IF W15-INVN = SPACES
               GO TO BA12.
             MOVE 8                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W15-INVN BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.

       BA13.
             ACCEPT S30.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  GO TO BA12
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO BA13
               END-EVALUATE.
             MOVE W15-INVD           TO W22-DATE W25-INVD.
             PERFORM DATE-CHECK THRU CHECK-DATE-EXIT.
           IF NOT(W22-ERROR = 0)
               EVALUATE W22-ERROR
                 WHEN 1  MOVE "Invalid Invoice DATE"  TO WS-ERR-MES
                 WHEN 2  MOVE "Invalid Invoice DAY"   TO WS-ERR-MES
                 WHEN 3  MOVE "Invalid Invoice MONTH" TO WS-ERR-MES
               END-EVALUATE
               PERFORM ERROR-MESSAGE
               GO TO BA13.
           IF WS-OPTION = "N"
               GO TO BA05.
             MOVE SPACES             TO W15-WHSE.
      *    *************************************************************
      *    ****   W A R E H O U S I N G   A P P L I C A B L E ?
      *    *************************************************************
           IF NOT(LS0-WHS = 2)
               GO TO BA14-A.

       BA14.
             MOVE "Warehouse purchases Y/N  [N]" TO WS-OPT-MES.
             MOVE "N"                TO WS-OPTION.
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
      *    IF USER-FUNC
      *        EVALUATE KEY-CODE-1
      *          WHEN ESC-KEY
      *               GO TO BA13
      *        END-EVALUATE.
           IF NOT(WS-OPTION = "N" OR "Y")
               GO TO BA14.
           IF WS-OPTION = "Y"
               PERFORM HA000.

       BA14-A.
             MOVE ZERO               TO W15-QUANT W15-TCOST W15-DISC2  W15-RAIL W15-DISCOUNT
                                        W15-TSELL W15-DISC1 W15-INCVAL W15-TXBL W15-VAT W15-IVAT .
             MOVE SPACES             TO WS-ORD.
             MOVE 1                  TO WS-S9.

       BA14-B.
             MOVE ZERO               TO W15-VATVAL(WS-S9) W15-TAXVAL(WS-S9).
           IF WS-S9 < 12
               ADD 1                 TO WS-S9
               GO TO BA14-B.

       BA15.
             MOVE ZERO               TO W15-ORD.
             MOVE "Order no (Y/N)?  [N]" TO WS-OPT-MES.
             MOVE "N"                TO WS-OPTION.
             MOVE 1                  TO WS-INSTR.

       BA16.
             PERFORM OPT-MESSAGE.
             MOVE WS-OPTION          TO WS-ORD.
             PERFORM CLEAR-L50.

       BA20.
           IF WS-ORD = "N"
               GO TO BA23A.
             DISPLAY  "F2"                        AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUNDCOLOR Brown HIGHLIGHT
                        "=View Orders or "                WITH BACKGROUND-COLOR Cyan FOREGROUNDCOLOR Blue
                                        "Esc"             WITH BACKGROUND-COLOR Cyan FOREGROUNDCOLOR Brown HIGHLIGHT
                                            "ape to exit" WITH BACKGROUND-COLOR Cyan FOREGROUNDCOLOR Blue.
             ACCEPT S21.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  MOVE ZERO  TO W15-ORD
                               DISPLAY "        " AT 0817 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black
                               GO TO BA15
                 WHEN F2-KEY   PERFORM PORDER-LOOKUP
                               IF W15-ORD = ZERO
                                   GO TO BA20
                               END-IF
                               DISPLAY S21
                               DISPLAY "/" AT 0825
                               DISPLAY S22
                               GO TO BA22
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO BA20
               END-EVALUATE.
           IF W15-OPREF = ZERO
               SUBTRACT 1        FROM WS-PURLAST
               GO TO BA35.
             DISPLAY "/" AT 0825.

       BA21.
             ACCEPT S22.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY   MOVE ZERO   TO W15-ORD
                                DISPLAY "        " AT 0817
                                SUBTRACT 1  FROM WS-PURLAST
                                GO TO BA35
                 WHEN OTHER     PERFORM AA900-ALARM
                                GO TO BA21
               END-EVALUATE.
             PERFORM CLEAR-L50.

       BA22.
             MOVE W15-ORD            TO ORD-KEY.
             PERFORM READ-PORDER THRU READ-PORDER-EXIT.
           IF WS-F-ERROR = 16
               MOVE WS-ER1           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BA20.
             MOVE ZERO               TO W10-CASES.
             GO TO BA24.

       BA23A.
             MOVE SPACES             TO W10-EXT-ITEM W15-DESCR.
             MOVE ZERO              TO ORD-TERMS W15-ICOST W10-CASES W15-PCOST.
           IF W10-STCK = "N"
               EVALUATE W10-SLNGTH
                 WHEN 3    MOVE ZERO  TO W10-I3
               EVALUATE W10-SLNGTH
                 WHEN 3   MOVE ZERO  TO W10-I3            
                 WHEN 4   MOVE ZERO  TO W10-I4            
                 WHEN 5   MOVE ZERO  TO W10-I5            
                 WHEN 6   MOVE ZERO  TO W10-I6            
                 WHEN 7   MOVE ZERO  TO W10-I7            
                 WHEN 8   MOVE ZERO  TO W10-I8            
                 WHEN 9   MOVE ZERO  TO W10-I9            
                 WHEN 10  MOVE ZERO  TO W10-I10            
                 WHEN 11  MOVE ZERO  TO W10-I11            
                 WHEN 12  MOVE ZERO  TO W10-I12            
                 WHEN 13  MOVE ZERO  TO W10-I13            
                 WHEN 14  MOVE ZERO  TO W10-I14
               END-EVALUATE.

       BA23B.
      *    *************************************************************
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S
      *    *************************************************************
             DISPLAY S56.
      *    *************************************************************
      *    ****    G E T   S T O C K   C O D E
      *    *************************************************************
           IF WS-EXT-STK = "Y"
               ACCEPT S07-EXT AT 0917
           ELSE
      *    *************************************************************
      *                    ***** ALPHA-NUMERIC CODE *****
      *    *************************************************************
           IF W10-STCK = "A"
               IF WS-USE-ITM = "Y"
                        ACCEPT S07-ITM AT 0917
               ELSE
                        ACCEPT S07 AT 0917
               END-IF
           ELSE
               EVALUATE W10-SLNGTH
      *                     3 Digit numeric Stock/Inventory item code
                 WHEN  3   ACCEPT S07-3 AT 0917
      *                     4 Digit numeric Stock/Inventory item code
                 WHEN  4   ACCEPT S07-4 AT 0917
      *                     5 Digit numeric Stock/Inventory item code
                 WHEN  5   ACCEPT S07-5 AT 0917
      *                     6 Digit numeric Stock/Inventory item code
                 WHEN  6   ACCEPT S07-6 AT 0917
      *                     7 Digit numeric Stock/Inventory item code
                 WHEN  7   ACCEPT S07-7 AT 0917
      *                     8 Digit numeric Stock/Inventory item code
                 WHEN  8   ACCEPT S07-8 AT 0917
      *                     9 Digit numeric Stock/Inventory item code
                 WHEN  9   ACCEPT S07-9 AT 0917
      *                    10 Digit numeric Stock/Inventory item code
                 WHEN 10   ACCEPT S07-10 AT 0917
      *                    11 Digit numeric Stock/Inventory item code
                 WHEN 11   ACCEPT S07-11 AT 0917
      *                    12 Digit numeric Stock/Inventory item code
                 WHEN 12   ACCEPT S07-12 AT 0917
      *                    13 Digit numeric Stock/Inventory item code
                 WHEN 13   ACCEPT S07-13 AT 0917
      *                    14 Digit numeric Stock/Inventory item code
                 WHEN 14   ACCEPT S07-14 AT 0917
               END-EVALUATE.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY   MOVE SPACES TO W10-EXT-ITEM
                                GO TO BA23BE
                 WHEN F1-KEY    PERFORM HELP-ROUTINE
                 WHEN F2-KEY    MOVE "I"  TO W10-ETYPE
                 WHEN F3-KEY    MOVE "A"  TO W10-ETYPE
                 WHEN F4-KEY    MOVE "D"  TO W10-ETYPE
                 WHEN F5-KEY    MOVE "2"  TO W10-ETYPE
                 WHEN F6-KEY    MOVE "X"  TO W10-ETYPE
                 WHEN F8-KEY    PERFORM BD100
                                MOVE LS-STOCK-CODE TO W10-EXT-ITEM
                                MOVE SPACES        TO LS-STOCK-CODE
                                GO TO BA23B
                 WHEN OTHER     PERFORM AA900-ALARM
                                GO TO BA23B
               END-EVALUATE
               MOVE "S"              TO WS-LOOK
               PERFORM AA50 THRU AA59


               PERFORM CA155-IO-CREDITOR


      *    *************************************************************
      *              *****   E X T E N D E D   C O D E   *****
      *    *************************************************************
               IF WS-EXT-STK = "Y"
                   DISPLAY S07-EXT AT 0917
               ELSE
      *    *************************************************************
      *         *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
               IF W10-STCK = "A"
                   IF WS-USE-ITM = "Y"
                       DISPLAY S07-ITM AT 0917
                   ELSE
                       DISPLAY S07 AT 0917.
           IF W10-EXT-ITEM = SPACES
               GO TO BA23B.

       BA23BE.
             PERFORM CLEAR-L50.
           IF W10-EXT-ITEM = SPACES
               SUBTRACT 1            FROM WS-PURLAST
               GO TO BA35.

       BA23-GET-STOCK.
             MOVE W05-RATE           TO W05-SRTE.
             PERFORM CA000.
           IF STK-PRC = ZERO
               MOVE 1                TO STK-PRC.
           IF STK-IND < 2
               IF (STK-PCOST = ZERO) OR (STK-PRC = 1)
                   MOVE STK-COST     TO STK-PCOST.
             MOVE W05-SRTE           TO W05-RATE.
           IF NOT(WS-F-ERROR = ZERO)
               MOVE "Z"              TO W10-ETYPE
               PERFORM AA50 THRU AA59


               PERFORM CA155-IO-CREDITOR


      *    *************************************************************
      *               *****   E X T E N D E D  C O D E   *****
      *    *************************************************************
               IF WS-EXT-STK = "Y"
                   DISPLAY S07-EXT AT 0917
               ELSE
      *    *************************************************************
      *         *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
               IF W10-STCK = "A"
                   IF WS-USE-ITM = "Y"
                       DISPLAY S07-ITM AT 0917
                   ELSE
                       DISPLAY S07 AT 0917
                   END-IF
               END-IF
           END-IF
           IF W10-EXT-ITEM = SPACES
                GO TO BA23B
            ELSE
                GO TO BA23-GET-STOCK.
           IF STK-IND = 4
               MOVE "Header Record"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BA23B.
             MOVE ZERO               TO W15-WQUANT.
      *    *************************************************************
      *    ****   W A R E H O U S I N G   A P P L I C A B L E ?
      *    *************************************************************
           IF LS0-WHS = 2
               PERFORM IA000.

       BA23C.
             MOVE ZERO               TO W15-IUN W15-PUN.
             MOVE W12-TODAY          TO W15-DEL.
             GO TO BA24A.

       BA24.
             MOVE ORD-EXT-ITEM       TO W10-EXT-ITEM.
             MOVE ORD-QUANT          TO W15-PUN.
             MOVE ORD-DELIV          TO W15-DEL.
             MOVE ORD-UCOST          TO W15-PCOST.
      *      MOVE ORD-CURRENCY       TO W15-CURRENCY.
      *      MOVE ORD-DRATE          TO W15-RATE.
             DISPLAY W10-EXT-ITEM AT 0917 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
             MOVE W05-RATE           TO W05-SRTE.
             PERFORM CA000.
             MOVE W05-SRTE           TO W05-RATE.
           IF WS-F-ERROR NOT = ZERO
               MOVE "Stock record not on file" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BA20.
             MOVE ZERO               TO W15-WQUANT.
      *    *************************************************************
      *    ****   W A R E H O U S I N G   A P P L I C A B L E  ?
      *    *************************************************************
           IF LS0-WHS = 2
               PERFORM IA000.

       BA24A.
             DISPLAY STK-DESC AT 0936 WITH FOREGROUNDCOLOR Blue BACKGROUND-COLOR Grey.
      *    *************************************************************
      *    ****    GET THE APPLICABLE VAT RATE (W05-RATE, WS-VAT-SUB)
      *    *************************************************************
             MOVE STK-TAX            TO WS-VAT-SUB.
      *    *************************************************************
      *    ****            DOES THE PREVIOUS RATE APPLY
      *    *************************************************************
           IF (WS-VAT-SUB > 0 AND < 7)
               IF W20-DTE < WS-VAT-DATE(WS-VAT-SUB)
                   ADD 6             TO WS-VAT-SUB.
           IF STK-TAX NOT = ZERO
               MOVE W05-VAT(WS-VAT-SUB) TO W05-RATE
           ELSE
               MOVE ZERO             TO W05-RATE.
           IF STK-PCOST = ZERO
               MOVE STK-COST         TO STK-PCOST.
           IF WS-ORD = "N"
               IF STK-IND > 1
                   MOVE ZERO         TO W15-PCOST W100-S8V3
               ELSE
                   MOVE STK-PCOST    TO W15-PCOST W100-S8V3
               END-IF
           ELSE
               MOVE STK-COST         TO W100-S8V3.
      *    *************************************************************
      *    ****    C A L C U L A T E   I N C L U S I V E   C O S T
      *    *************************************************************
           IF P-TAX = "I"
               COMPUTE W15-PCOST ROUNDED = W15-PCOST + (W15-PCOST * W05-RTE)
               COMPUTE W100-S8V3 ROUNDED = STK-PCOST + (STK-PCOST * W05-RTE).
      *      DISPLAY W10-EXT-ITEM AT 0917 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      *    *************************************************************
      *    ****           I S  I T   A   T E X T   I T E M
      *    *************************************************************
           IF STK-IND = 3
               ACCEPT W15-DESCR AT 0936 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black AUTO
           ELSE
               MOVE STK-DESC         TO W15-DESCR
               DISPLAY STK-DESC AT 0936 WITH BACKGROUND-COLOR Grey FOREGROUNDCOLOR Blue.
            PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO BA20.
             DISPLAY W100-S8V3 AT 1144 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             MOVE STK-WSALE      TO W100-S8V2.
             DISPLAY W100-S8V2 AT 1817 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             MOVE STK-CASH       TO W100-S8V2.
             DISPLAY W100-S8V2 AT 1833 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             MOVE STK-SELL       TO W100-S8V2.
             DISPLAY W100-S8V2 AT 1849 WITH FOREGROUND-COLOR Brown HIGHLIGHT.

       BA25.
      *    IF WS-USE-3 = "Y"
      *        ACCEPT S37-3 AT 0817
      *    ELSE
             ACCEPT S37.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY GO TO BA23B
                 WHEN OTHER   PERFORM AA900-ALARM
                              GO TO BA25
               END-EVALUATE.
      *    *************************************************************
      *    ****    C O N V E R T I O N   F R O M   P U R C H A S E
      *                U N I T   T O   S E L L I N G   U N I T
      *    *************************************************************
           IF W15-PUN = ZERO
               GO TO BA25.
           IF STK-PRC = ZERO
               MOVE 1                TO STK-PRC.

       BA25B.
             MULTIPLY W15-PUN BY STK-PRC GIVING W15-IUN.
             ACCEPT S37S.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  GO TO BA25
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO BA25B
               END-EVALUATE.
           IF W15-IUN = ZERO
               GO TO BA25B.
      *    *************************************************************
      *    ****         A R E   C A S E S   C O N T R O L L E D
      *    *************************************************************
           IF WS-USE-CASES= "Y"
               IF STK-USE-CASES = "Y"
      *         GET THE NUMBER OF CASES RECEIVED
                   PERFORM BF000.

       BA26.
             ACCEPT S38.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  GO TO BA25
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO BA26
               END-EVALUATE.
           IF W15-PCOST = ZERO
               GO TO BA26.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³         C O S T   P E R   S E L L I N G   U N I T         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             COMPUTE W15-ICOST ROUNDED = (W15-PCOST * W15-PUN) / W15-IUN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ The following procedure is being developed to handle multi³
      *    ³ currencies and is commented out until completed.          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *      MOVE W15-OCOST  TO W15-ICOST.
      * BA26A.
      *      ACCEPT S23R AT 0949.
      *    IF USER-FUNC
      *       EVALUATE KEY-CODE-1
      *         WHEN ESC-KEY  GO TO BA20
      *         WHEN OTHER    PERFORM AA900-ALARM
      *                       GO TO BA26A
      *       END-EVALUATE.
      *     IF W15-RATE = ZERO
      *        GO TO BA26A.
      * BA26B.
      *    IF W15-RATE = 1.00
      *        MOVE W15-OCOST        TO W15-ICOST
      *        MOVE "Rand"           TO W15-CURRENCY
      *        DISPLAY S23C AT 0970
      *        GO TO BA26D.
      *BA26C.
      *      ACCEPT S23C AT 0970.
      *    IF USER-FUNC
      *       EVALUATE KEY-CODE-1
      *         WHEN ESC-KEY  GO TO BA20
      *         WHEN OTHER    PERFORM AA900-ALARM
      *                       GO TO BA26C
      *       END-EVALUATE.
      *    IF W15-CURRENCY = SPACES
      *        GO TO BA26C.
      *      COMPUTE W15-ICOST ROUNDED = W15-OCOST * W15-RATE.
      *BA26D.
      *      DISPLAY S38 AT 1017.

       BA28.
             MOVE ZERO               TO W15-U-TXBL W15-U-NTXBL W15-U-VAT.
           IF STK-TAX NOT = ZERO
               IF P-TAX = "N"
                   MOVE W15-PCOST    TO W15-U-NTXBL W15-U-INCVAL W15-U-PNETT
               ELSE
               IF P-TAX = "I"
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³              I N C L U S I V E   V A L U E                ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

                   MOVE W15-PCOST    TO W15-U-INCVAL

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³              E X C L U S I V E   V A L U E                ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

                   COMPUTE W15-U-TXBL ROUNDED = W15-U-INCVAL - ((W15-U-INCVAL / (100.00000 + W05-RATE)) * W05-RATE)
                   MOVE W15-U-TXBL   TO W15-U-PNETT

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³         V A L U E   A D D E D   T A X   V A L U E         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

                   SUBTRACT W15-U-TXBL FROM W15-U-INCVAL GIVING W15-U-VAT
               ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³              E X C L U S I V E   V A L U E                ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

                   MOVE W15-PCOST    TO W15-U-TXBL W15-U-PNETT

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³         V A L U E   A D D E D   T A X   V A L U E         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

                   COMPUTE W15-U-VAT ROUNDED = W15-U-TXBL * W05-RTE

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³              I N C L U S I V E   V A L U E                ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

                   ADD W15-U-TXBL W15-U-VAT GIVING W15-U-INCVAL
           ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                    N O N   T A X A B L E                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

               MOVE W15-PCOST        TO W15-U-NTXBL W15-U-INCVAL W15-U-PNETT.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                U N I T   N E T T   C O S T                ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COMPUTE W15-U-NETT ROUNDED = (W15-PCOST * W15-PUN) / W15-IUN.

      *    *************************************************************
      *    ****   L I N E   C O S T
      *    *************************************************************

           IF W15-U-TXBL NOT = ZERO
               MULTIPLY W15-PUN BY W15-U-TXBL  GIVING W15-COST ROUNDED
           ELSE
               MULTIPLY W15-PUN BY W15-U-NTXBL GIVING W15-COST ROUNDED.

      *    *************************************************************
      *    ****   L I N E   I N C L U S I V E   C O S T
      *    *************************************************************

           IF (P-TAX = "E") AND (STK-TAX NOT = ZERO)
               COMPUTE W15-INCOST ROUNDED = (W15-COST + (W15-COST * W05-RTE))
           ELSE
               MULTIPLY W15-PUN BY W15-U-INCVAL GIVING W15-INCOST ROUNDED.
             SUBTRACT W15-COST FROM W15-INCOST  GIVING W15-IVAT ROUNDED.
             MULTIPLY W15-PUN BY W15-PCOST      GIVING W15-ILCOST ROUNDED.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                U N I T   N E T T   C O S T                ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             DISPLAY S47.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                  V A T   P E R   U N I T                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             DISPLAY S48.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                     T O T A L   V A T                     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             DISPLAY S45.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³            C O S T   E X C L U D I N G   V A T            ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             DISPLAY S40.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³            C O S T   I N C L U D I N G   V A T            ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             DISPLAY S46.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³      C A L C U L A T E   N E W   C O S T   P R I C E      ³
      *    ³                        1st DISCOUNT                       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA29.
             ACCEPT S35.
           IF W15-DISC1 = ZERO
               GO TO BA30A.
             COMPUTE W25-V1 ROUNDED    = (W15-ILCOST - (W15-ILCOST * W15-DSC1)).
             COMPUTE W15-PCOST ROUNDED = W25-V1 / W15-PUN.
             COMPUTE W15-ICOST ROUNDED = (W15-PCOST * W15-PUN) / W15-IUN.
             PERFORM BA28.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                        2nd DISCOUNT                       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             ACCEPT S36.
           IF W15-DISC2 = ZERO
               GO TO BA30A.
             COMPUTE W25-V1 ROUNDED    = (W15-ILCOST - (W15-ILCOST * W15-DSC2)).
             COMPUTE W15-PCOST ROUNDED = W25-V1 / W15-PUN.
             COMPUTE W15-ICOST ROUNDED = (W15-PCOST * W15-PUN) / W15-IUN.
             PERFORM BA28.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³           CHECK IF AVERAGE COST TO BE CALCULATED          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA30A.
           IF STK-IND > 1
               COMPUTE W15-AVRG ROUNDED = W15-COST / W15-IUN
               GO TO BA31.
           IF (STK-QUANT + W15-WQUANT) < 0.01
               COMPUTE W15-AVRG ROUNDED = W15-COST / W15-IUN
           ELSE
               COMPUTE W25-V1 = (STK-QUANT + W15-WQUANT) * STK-AVGE
               ADD W15-COST      TO W25-V1
               COMPUTE W15-AVRG ROUNDED = W25-V1 / (STK-QUANT + W15-WQUANT + W15-IUN).

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                   N E W   A V E R A G E                   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA31.
             MOVE W15-AVRG  TO W100-S6V2.
             DISPLAY W100-V2 AT 1470 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.

       BA32.
           IF STK-MARKUP = ZERO
               PERFORM BA100.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³   C A L C U L A T E   N E W   S E L L I N G   P R I C E   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF W15-AVRG > W15-U-NETT
               COMPUTE W15-USELL ROUNDED   = (W15-AVRG + (W15-AVRG * STK-MARKUP / 100.00000))
               COMPUTE W15-UWSALE ROUNDED  = (W15-AVRG + (W15-AVRG * STK-WMARKUP / 100.00000))
               COMPUTE W15-UCASH ROUNDED   = (W15-AVRG + (W15-AVRG * STK-CMARKUP / 100.00000))
           ELSE
               COMPUTE W15-USELL ROUNDED   = (W15-U-NETT + (W15-U-NETT * STK-MARKUP / 100.00000))
               COMPUTE W15-UWSALE ROUNDED  = (W15-U-NETT + (W15-U-NETT * STK-WMARKUP / 100.00000))
               COMPUTE W15-UCASH ROUNDED   = (W15-U-NETT + (W15-U-NETT * STK-CMARKUP / 100.00000)).
           IF W05-ROUND = "E"
               MOVE W15-USELL        TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL        TO W15-USELL
               MOVE W15-UWSALE       TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL        TO W15-UWSALE
               MOVE W15-UCASH        TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL        TO W15-UCASH.

       BA32A.
           IF STK-TAX NOT = ZERO
               COMPUTE W10-VSELL ROUNDED  = W15-USELL + (W15-USELL * W05-RTE)
               COMPUTE W10-VWSALE ROUNDED = W15-UWSALE + (W15-UWSALE * W05-RTE)
               COMPUTE W10-VCASH ROUNDED  = W15-UCASH + (W15-UCASH * W05-RTE)
           ELSE
               MOVE W15-UWSALE       TO W10-VWSALE
               MOVE W15-UCASH        TO W10-VCASH
               MOVE W15-USELL        TO W10-VSELL.
           IF W05-ROUND = "I"
               MOVE W10-VSELL        TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL        TO W10-VSELL
               MOVE W10-VWSALE       TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL        TO W10-VWSALE
               MOVE W10-VCASH        TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL        TO W10-VCASH
               PERFORM CB200.
             DISPLAY S41.

       BA32B.
           IF W15-USELL = STK-SELL
               GO TO BA32D.
             MOVE SPACE              TO WS-OPTION.

       BA32C.
             MOVE "Use new selling prices Y/N  [ ]" TO WS-OPT-MES.
             MOVE 1                  TO WS-INSTR.
             MOVE 17                 TO WS-MES-LINE.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "Y"
               GO TO BA32D.
           IF NOT(STK-WSALE = ZERO)
               MOVE STK-WSALE        TO W15-UWSALE.
           IF NOT(STK-CASH = ZERO)
               MOVE STK-CASH         TO W15-UCASH.
           IF NOT(STK-SELL = ZERO)
               MOVE STK-SELL         TO W15-USELL.
             PERFORM BA32A.

       BA32D.
           IF NOT(STK-TAX = ZERO)
               GO TO BA32E.

       BA32DA.
             MOVE "Adjust selling prices Y/N  [N]"  TO WS-OPT-MES.
             MOVE 1                  TO WS-INSTR.
             MOVE "N"                TO WS-OPTION.
             MOVE 16                 TO WS-MES-LINE.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "N"
               GO TO BA33B-1.
             GO TO BA33.

       BA32E.
             MOVE "Adjust 'E'x/'I'nclusive price or 'N'one  [E]"  TO WS-OPT-MES.
             MOVE "E"                TO WS-OPTION.
             MOVE 4                  TO WS-INSTR.
             MOVE 16                 TO WS-MES-LINE.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "N"
               GO TO BA33B-1.
           IF WS-OPTION = "E"
               GO TO BA33.

       BA32F.
             ACCEPT S41D-1.
           IF W10-VWSALE = ZERO
               GO TO BA32F.
             COMPUTE W15-UWSALE ROUNDED = (W10-VWSALE / (100.000 + W05-RATE)) * 100.000
           IF W15-UWSALE < W15-ICOST
               MOVE WS-ER2  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BA32F.

       BA32G.
             ACCEPT S41D-2.
           IF W10-VCASH = ZERO
               GO TO BA32G.
             COMPUTE W15-UCASH ROUNDED = (W10-VCASH / (100.000 + W05-RATE)) * 100.000
           IF W15-UCASH < W15-ICOST
               MOVE WS-ER2  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BA32G.

       BA32H.
             ACCEPT S41D-3.
           IF W10-VSELL = ZERO
               GO TO BA32G.
             COMPUTE W15-USELL ROUNDED = (W10-VSELL / (100.000 + W05-RATE)) * 100.000
           IF W15-USELL < W15-ICOST
               MOVE WS-ER2           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BA32H.
             GO TO BA33B-1.

       BA33.
             ACCEPT S41A.
           IF W15-UWSALE < W15-ICOST
               MOVE WS-ER2           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BA33.

       BA33A.
             ACCEPT S41B.
           IF W15-UCASH < W15-ICOST
               MOVE WS-ER2           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BA33A.

       BA33B.
             ACCEPT S41C.
           IF W15-USELL < W15-ICOST
               MOVE WS-ER2           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BA33B.
             PERFORM BA32A.

       BA33B-1.
             EXIT.

       BA33B-PACKS.
           IF (WS-USE-PACKS = "Y") AND (STK-USE-PACKS = "Y")
               PERFORM BG000
               IF WS-ERROR = 9
                   MOVE "X"          TO WS-OPTION
                   GO TO BA35-EXIT.

       BA33B-1A.
             MULTIPLY W15-USELL BY W15-IUN GIVING W15-SELL.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               DISPLAY S42B
               GO TO BA20.
           IF NOT(P-TAX = "N") AND (STK-TAX > ZERO)
               ADD W15-COST          TO W15-TXBL.
             ADD W15-INCOST          TO W15-INCVAL.
             ADD W15-IUN             TO W15-QUANT.
             ADD W15-COST            TO W15-TCOST.
             ADD W15-SELL            TO W15-TSELL.
           IF NOT(WS-VAT-SUB = ZERO)
               ADD W15-COST          TO W15-TAXVAL(WS-VAT-SUB)
               ADD W15-IVAT          TO W15-VATVAL(WS-VAT-SUB).
             MOVE WS-PURLAST         TO WS-PURKEY.

       BA33C.
             MOVE "'P'rice,'B'arcode,'N'o labels  [ ]"  TO WS-OPT-MES.
             MOVE SPACE              TO WS-OPTION.
             MOVE 5                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
             INITIALIZE PUR-RECORD1.
           IF W15-ORD NOT = ZERO
               MOVE W15-ORD          TO PUR-ORD.
             MOVE W10-EXT-ITEM       TO PUR-EXT-STOCK.
             MOVE W15-PUN            TO PUR-PUN.
             MOVE W15-IUN            TO PUR-IUN.
             MOVE W15-ICOST          TO PUR-ICOST.
             MOVE W15-PCOST          TO PUR-PCOST.
             MOVE W15-UWSALE         TO PUR-UWSALE.
             MOVE W15-UCASH          TO PUR-UCASH.
             MOVE W15-USELL          TO PUR-USELL.
             MOVE W15-COST           TO PUR-COST.
             MOVE W15-INCOST         TO PUR-INCVAL.
             MOVE W15-SELL           TO PUR-SELL.
             MOVE WS-OPTION          TO PUR-LAB.
             MOVE W15-IVAT           TO PUR-IVAT.
             MOVE WS-VAT-SUB         TO PUR-VAT-SUB.
           IF WS-USE-PACKS = "Y"
               MOVE W10-WSALE-2      TO PUR-WSALE-2
               MOVE W10-CASH-2       TO PUR-CASH-2
               MOVE W10-SELL-2       TO PUR-SELL-2
               MOVE W10-WSALE-3      TO PUR-WSALE-3
               MOVE W10-CASH-3       TO PUR-CASH-3
               MOVE W10-SELL-3       TO PUR-SELL-3
               MOVE W10-WSALE-4      TO PUR-WSALE-4
               MOVE W10-CASH-4       TO PUR-CASH-4
               MOVE W10-SELL-4       TO PUR-SELL-4.
           IF WS-USE-CASES = "Y"
               MOVE W10-CASES        TO PUR-CASES.
               PERFORM WRITE-PURCH THRU WRITE-PURCH-EXIT.

       BA33D.
             MOVE W15-TCOST          TO W25-SVAL.
             DISPLAY W25-S5V2 AT 1866 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
             MOVE 1                  TO WS-S9.
             MOVE ZERO               TO W15-VAT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                    Accumulate the tax                     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA33-TAX.
             ADD W15-VATVAL(WS-S9)   TO W15-VAT.
           IF WS-S9 < 12
               ADD 1                 TO WS-S9
               GO TO BA33-TAX.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                   Display the total VAT                   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             DISPLAY S43.
             COMPUTE W25-SVAL = W15-TCOST + W15-VAT.
             DISPLAY W25-S5V2 AT 2266 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
             DISPLAY S42B.

       BA34-CHK.
             MOVE "'N'ext Item, 'V'iew or 'E'nd  [N]"  TO WS-OPT-MES.
             MOVE "N"                TO WS-OPTION.
             MOVE 6                  TO WS-INSTR.

       BA34A.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "E"
               GO TO BA35.
           IF WS-OPTION = "V"
               PERFORM BE000
               GO TO BA33D.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                     Get the next item                     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
            ADD 1                    TO WS-PURLAST.
            GO TO BA15.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                  Were there any entries?                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA35.
           IF WS-PURLAST = ZERO
               GO TO BA00.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³            Users last chance to amend or cancel           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA35A.
             MOVE "P"                TO WS-OPTION.
             MOVE "'P'rocess, 'C'ancel or 'M'ore items  [P]" TO WS-OPT-MES.
             MOVE 7                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "C"
               GO TO BA00.
           IF WS-OPTION = "M"
              GO TO BA34-CHK.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³              Transport / Railage / Freight                ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA35B.
             ACCEPT S33.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  GO TO BA34-CHK
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO BA35B
               END-EVALUATE.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                    Calculate Sub-Total                    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             COMPUTE W25-SVAL = W15-TCOST + W15-RAIL + W15-VAT.
             DISPLAY W25-S5V2 AT 2066 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
             DISPLAY "Press '-' key for negative" AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                  Discount / Adjustment                    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA35C.
             ACCEPT S34.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  GO TO BA34-CHK
                 WHEN OTHER
              PERFORM AA900-ALARM
              GO TO BA35C
               END-EVALUATE.
             PERFORM CLEAR-L50.
             MOVE 1                  TO WS-S9.
             MOVE ZERO               TO W15-VAT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                    Accumulate the tax                     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA35-TAX.
             ADD W15-VATVAL(WS-S9)   TO W15-VAT.
           IF WS-S9 < 12
               ADD 1                 TO WS-S9
               GO TO BA35-TAX.
             MOVE W15-VAT            TO W55-VAT.
             COMPUTE W25-SVAL = W15-TCOST + W15-RAIL + W15-VAT + W15-DISCOUNT.
             DISPLAY W25-S5V2 AT 2366 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           IF P-TAX = "E"
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³       Accept the total VAT (Allow for adjustments)        ³
      *    ³       E X C L U S I V E                                   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               ACCEPT S43
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    If VAT has been adjusted -                             ³
      *    ³    Re-allocate to the various VAT percentages             ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               IF NOT(W15-VAT = W55-VAT)
                   PERFORM BA00C
               END-IF
           ELSE
      *
      *    ***** No VAT *****
      *
           IF NOT(P-TAX = "I")
      *        COMPUTE W15-VAT ROUNDED = W15-VAT +
      *       ((W15-RAIL + W15-DISCOUNT) * W05-RTE)
      *    ELSE
               MOVE ZERO         TO W15-VAT.
             COMPUTE W25-SVAL = W15-TCOST + W15-RAIL + W15-VAT + W15-DISCOUNT.
             DISPLAY W25-S5V2 AT 2466 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
             DISPLAY "Esc" AT 5062    WITH BACKGROUND-COLOR Cyan FOREGROUNDCOLOR Brown HIGHLIGHT
                        "ape to exit" WITH BACKGROUND-COLOR Cyan FOREGROUNDCOLOR Blue.
       BA35D.
             PERFORM CHECK-CORRECT.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  GO TO BA00
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO BA35D
               END-EVALUATE.
             PERFORM CLEAR-L50.
           IF WS-OPTION = "N"
               GO TO BA35B.

       BA35-EXIT.
             EXIT.
      *
      *     ****    P R O C E S S   T H E   I N V O I C E
      *
       BA036        SECTION 5.
       BA36-START.
             MOVE ZERO   TO WS-PURKEY.
             DISPLAY "Processing Invoice" AT 5012 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
           IF WS-PAGE = ZERO
               PERFORM BA00B.
             INITIALIZE PRD-REC1.
             MOVE ZERO               TO PRD-SEQ.
      *
      *    ****    Start Recovery Logging.
      *
             PERFORM AY60 THRU AY999.
      *
      *    ****    Lock Creditor record and Log to Recovery file
      *
             MOVE 0                  TO WS-ACTION.
             MOVE W15-ACNO           TO P-NUMBER.
             PERFORM AY07 THRU AY59.

       BA36.
           IF WS-PURKEY NOT < WS-PURLAST
               GO TO BA70.
             ADD 1                   TO WS-PURKEY.
             PERFORM READ-PURCH THRU READ-PURCH-EXIT.
             MOVE ZERO               TO W15-ORD.
             MOVE "N"                TO WS-ORD.
      *
      *    ****    If Order - Read Order record. Log
      *            the order record to the Recovery file.
      *

           IF PUR-ORD NOT = ZERO
               MOVE PUR-ORD          TO ORD-KEY
               MOVE ZERO             TO WS-ACTION
               PERFORM AY10 THRU AY59
               IF WS-F-ERROR = 0
                   MOVE "Y"          TO WS-ORD
                   MOVE PUR-ORD      TO W15-ORD.
      *
      *    ****    Lock Stock record and Log to Recovery file
      *
             MOVE PUR-EXT-STOCK      TO STK-CODE.
             MOVE ZERO               TO WS-ACTION.
             PERFORM AY05 THRU AY59.
      *
      *    ****    W A R E H O U S E   S T O C K
      *
           IF NOT(W15-WHSE = SPACES)
               MOVE W15-WHSE         TO WST-WAR
               MOVE PUR-EXT-STOCK    TO WST-EXT-CODE
      *        MOVE SPACES           TO WST-ITM
               PERFORM READ-WSTOCK THRU READ-WSTOCK-EXIT
               IF NOT(WS-STATUS = "00")
      *
      *    ****    C R E A T E  W / H  S T O C K   R E C O R D
      *
                   MOVE W15-WHSE     TO WST-WAR
                   MOVE PUR-EXT-STOCK TO WST-EXT-CODE
                   MOVE SPACES       TO WST-BIN
                   MOVE ZERO         TO WST-MTD WST-MTDV WST-QUANT WST-YTD WST-YTDV
                   PERFORM WRITE-WSTOCK THRU WRITE-WSTOCK-EXIT
                   MOVE 1            TO WS-ACTION
               END-IF
               PERFORM AY052 THRU AY59.
               MOVE W05-RATE         TO W05-SRTE.
               PERFORM CA500.
             MOVE W05-SRTE           TO W05-RATE.
             MOVE PUR-PUN            TO W15-PUN.
             MOVE PUR-IUN            TO W15-IUN.
             MOVE PUR-ICOST          TO W15-ICOST.
             MOVE PUR-PCOST          TO W15-PCOST.
             MOVE PUR-UWSALE         TO W15-UWSALE
             MOVE PUR-UCASH          TO W15-UCASH.
             MOVE PUR-USELL          TO W15-USELL.
             MOVE PUR-COST           TO W15-COST.
             MOVE PUR-SELL           TO W15-SELL.
             MOVE PUR-INCVAL         TO W15-INCOST.
             MOVE PUR-IVAT           TO W15-IVAT.
             MOVE PUR-VAT-SUB        TO WS-VAT-SUB.
             MOVE PUR-WSALE-2        TO W10-WSALE-2.
             MOVE PUR-CASH-2         TO W10-CASH-2.
             MOVE PUR-SELL-2         TO W10-SELL-2.
             MOVE PUR-WSALE-3        TO W10-WSALE-3.
             MOVE PUR-CASH-3         TO W10-CASH-3.
             MOVE PUR-SELL-3         TO W10-SELL-3.
             MOVE PUR-WSALE-4        TO W10-WSALE-4.
             MOVE PUR-CASH-4         TO W10-CASH-4.
             MOVE PUR-SELL-4         TO W10-SELL-4.
             MOVE PUR-CASES          TO W10-CASES.
             MOVE STK-CODE           TO W10-EXT-ITEM.
             MOVE STK-DESC           TO W15-DESCR.
           IF PUR-LAB = "P"
               MOVE W10-EXT-ITEM     TO PLB-ITEM
               MOVE PUR-IUN          TO PLB-QUANT
               MOVE ZERO             TO PLB-FORM
               MOVE W15-INVN         TO PLB-INV
               PERFORM WRITE-PRCLAB THRU WRITE-PRCLAB-EXIT
               ADD 1                 TO WS-PRCKEY
           ELSE
           IF PUR-LAB = "B"
               MOVE W10-EXT-ITEM     TO BAR-ITEM BAR-CODE
               MOVE PUR-IUN          TO BAR-QNT
               MOVE STK-DESC         TO BAR-DESC
               IF WS-BAR-CODE = "X"
                   MOVE STK-XREF     TO BAR-CODE
               END-IF
               WRITE BAR-REC.
           IF WS-ORD = "N"
               MOVE W15-COST         TO W20-RESULT
               GO TO BA37.
           IF W15-IUN > ORD-QUANT
               SUBTRACT ORD-QUANT FROM W15-IUN GIVING W15-XQNT
           ELSE
               MOVE ZERO             TO W15-XQNT.
             MOVE W12-TODAY          TO W15-DATE.
           IF W15-MM NOT = W15-DEL-MM
               PERFORM BB000.
           IF W15-COST > ORD-COST
               MOVE ORD-COST         TO W20-RESULT
           ELSE
               MOVE W15-COST         TO W20-RESULT.
             MOVE ZERO               TO W20-RESULT.
           IF W15-IUN NOT > ORD-QUANT
               SUBTRACT W15-IUN      FROM ORD-QUANT
           ELSE
               MOVE ZERO             TO ORD-QUANT.
           IF W15-COST NOT > ORD-COST
               SUBTRACT W15-COST     FROM ORD-COST
           ELSE
               SUBTRACT ORD-COST FROM W15-COST GIVING W20-RESULT
               MOVE ZERO             TO ORD-COST.
      *
      *    ***** UPDATE THE STOCK RECORD *****
      *
       BA37.
             COMPUTE W15-UCOST ROUNDED = (W15-COST / W15-IUN).
             MOVE W15-UCOST          TO STK-COST.
             MOVE W15-PCOST          TO STK-PCOST.
             MOVE STK-AVGE           TO W15-AVRG.
             MOVE W15-UWSALE         TO STK-WSALE.
             MOVE W15-UCASH          TO STK-CASH.
             MOVE W15-USELL          TO STK-SELL.
      *
      *    ***** UPDATE THE PACK PRICES IF NECESSARY *****
      *
           IF STK-USE-PACKS = "Y"
               MOVE W10-WSALE-2      TO STK-WSALE-2
               MOVE W10-CASH-2       TO STK-CASH-2
               MOVE W10-SELL-2       TO STK-SELL-2
               MOVE W10-WSALE-3      TO STK-WSALE-3
               MOVE W10-CASH-3       TO STK-CASH-3
               MOVE W10-SELL-3       TO STK-SELL-3
               MOVE W10-WSALE-4      TO STK-WSALE-4
               MOVE W10-CASH-4       TO STK-CASH-4
               MOVE W10-SELL-4       TO STK-SELL-4.
             ADD W10-CASES           TO STK-CASES.
           IF NOT(P-NUMBER = STK-SUPP)
               MOVE STK-SUPP         TO STK-PSUPP
               MOVE P-NUMBER         TO STK-SUPP.
      *
      *      ***** CALCULATE AVERAGE COST *****
      *
           IF STK-IND > 1
               GO TO BA38.
           IF (STK-QUANT + W15-WQUANT) < 0.01
               COMPUTE STK-AVGE = W15-COST / W15-IUN
           ELSE
               COMPUTE W25-V1 = (STK-QUANT + W15-WQUANT) * STK-AVGE
               ADD W15-COST          TO W25-V1
               COMPUTE STK-AVGE ROUNDED = W25-V1 / (STK-QUANT + W15-WQUANT + W15-IUN).
           IF STK-AVGE < ZERO
               COMPUTE STK-AVGE = STK-AVGE * -1.
           IF NOT(W15-WHSE = SPACES)
               ADD W15-IUN           TO WST-QUANT
           ELSE
               ADD W15-IUN           TO STK-QUANT.
           IF STK-ORD > ZERO
               SUBTRACT W15-IUN      FROM STK-ORD.

       BA38.
             MOVE W12-T-YMD          TO STK-PUR.
           IF STK-ORD < ZERO
               MOVE ZERO             TO STK-ORD.
           IF STK-ORD = ZERO
               MOVE ZERO             TO STK-DTE STK-DISC.
      *
      *      ***** UPDATE DEPARTMENT COST *****
      *
             MOVE STK-LDG  TO DPT-CODE.
             MOVE 0                  TO WS-ACTION.
             PERFORM AY38 THRU AY59.
             ADD W15-COST            TO DPT-PUR-DAY DPT-PUR-MTD DPT-PUR-YTD.
           IF WX-INT = "Y"
               MOVE W20-DTE          TO WX-DATE
               MOVE STK-LDG          TO WX-LEDG
               MOVE "PURCHASES - CREDITORS" TO WX-NAR
               MOVE W15-COST         TO WX-VALUE
               MOVE DPT-PUR-GL       TO WX-AC
               MOVE 06               TO WX-TYPE
               PERFORM GL-TRF-UPDATE.
             PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
           IF WS-ORD = "Y"
               IF W12-MONTH = W15-DEL-MM
                   GO TO BA65.
           IF W20-RESULT = ZERO
               GO TO BA65.

       BA40.
           IF ORD-TERMS = ZERO
               MOVE P-TERMS          TO ORD-TERMS.
             MOVE ORD-TERMS          TO W15-SETT.
             MOVE 0                  TO WS-S.
             MOVE 1                  TO WS-S2.

       BA45.
            MOVE ZERO   TO W25-CFLOW(WS-S2).
          IF W15-MONTH(WS-S2) > 0
              ADD 1   TO WS-S.
           IF WS-S2 < 6
               ADD 1                 TO WS-S2
               GO TO BA45.
             MOVE ZERO               TO W25-V1 W25-V2.
             MOVE 1                  TO WS-S3.
           IF WS-S = 0
               MOVE 1                TO WS-S
               GO TO BA54.
      *        W15-30.
           IF WS-S = 1
               MOVE W20-RESULT       TO W25-V2
               GO TO BA50.
             DIVIDE W20-RESULT BY WS-S GIVING W25-V1 ROUNDED.
             SUBTRACT 1              FROM WS-S.
             COMPUTE W25-V2 = W20-RESULT - (W25-V1 * WS-S).
             ADD 1                   TO WS-S.

       BA50.
          IF W15-MONTH(WS-S3) = ZERO
               ADD 1                 TO WS-S3
               GO TO BA50.
           IF WS-S > 1
               MOVE W25-V1           TO W25-CFLOW(WS-S3)
               SUBTRACT 1            FROM WS-S
               ADD 1                 TO WS-S3
               GO TO BA50.
             MOVE W25-V2             TO W25-CFLOW(WS-S3).

       BA54.
             MOVE 1                  TO WS-S2.

       BA55.
          IF W25-CFLOW(WS-S2) = ZERO
               GO TO BA60.
             ADD W12-MONTH WS-S2     GIVING WS-S3.
           IF WS-S3 > 12
               SUBTRACT 12           FROM WS-S3.

       BA60.
           IF WS-S2 < 6
               ADD 1                 TO WS-S2
               GO TO BA55.

       BA65.
           IF WS-ORD = "Y"
               IF ORD-QUANT = ZERO
                   MOVE 2            TO WS-ACTION
                   PERFORM AY10 THRU AY59
                   PERFORM DELETE-PORDER-REC THRU WRITE-PORDER-EXIT
               ELSE
                   PERFORM REWRITE-PORDER THRU WRITE-PORDER-EXIT.
             MOVE W15-ORD            TO R-ORD.
             MOVE W10-EXT-ITEM       TO R-ITM.
             MOVE STK-XREF           TO R-XREF.
             MOVE W15-DESCR          TO R-DESC.
             MOVE STK-BIN            TO R-BIN.
             MOVE W15-IUN            TO R-QNT.
             MOVE W15-UCOST          TO R-COST.
             MOVE W15-USELL          TO R-SELL.
             MOVE W15-COST           TO R-EXT.
          IF W02-LINAGE < W02-PRN-LENGTH
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM BA00B.
             MOVE SPACES             TO R-DET.
           IF NOT(W15-WHSE = SPACES)
               PERFORM REWRITE-WSTOCK THRU WRITE-WSTOCK-EXIT.
             PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
             MOVE P-NUMBER           TO PRD-SUPP.
             MOVE W10-EXT-ITEM       TO PRD-EXT-CODE.
             MOVE W12-T-YMD          TO PRD-DTE.
             ADD 1                   TO PRD-SEQ.
             MOVE "I"                TO PRD-TRAN.
             MOVE W15-INVN           TO PRD-REF.
             MOVE W15-IUN            TO PRD-QNT.
             MOVE W15-COST           TO PRD-COST.
             MOVE W15-NAME           TO PRD-NAME.
             MOVE W15-WHSE           TO PRD-WHSE.
             PERFORM WRITE-PURDEX THRU WRITE-PURDEX-EXIT
             MOVE 1                  TO WS-ACTION
             PERFORM AY056 THRU AY59.
             GO TO BA36.

       BA70.
      *
      *    ****    U P D A T E   S T O C K   C O N T R O L
      *
             ADD W15-TCOST           TO STK-TOTAL.
             ADD W15-TCOST           TO STK-CMTD.
             ADD W15-TCOST           TO STK-CYTD.
      *
      *    ****    I N V O I C E   T O T A L
      *
             ADD W15-RAIL            TO W15-TCOST.
             ADD W15-DISCOUNT        TO W15-TCOST.
             ADD W15-VAT             TO W15-TCOST.

       BA75.
             MOVE 6                  TO WS-PARKEY.
             PERFORM AY01 THRU AY59.

       BA80.
             ADD W15-RAIL            TO PAR-RAILD.
             ADD W15-RAIL            TO PAR-RAILM.
             ADD W15-DISCOUNT        TO PAR-DISCD.
             ADD W15-DISCOUNT        TO PAR-DISCM.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.

       BA85.
             INITIALIZE CT-REC.
             MOVE W12-T-YMD          TO CT-DTE.
             MOVE W15-INVN           TO CT-SINV CT-REF.
             MOVE W20-DTE            TO CT-SDTE.
             MOVE "U"                TO CT-TYPE.
             MOVE "I"                TO CT-PJ.
           IF W15-OPREF = ZERO
               MOVE SPACES           TO CT-ORD
           ELSE
               MOVE W15-OPREF        TO CT-ORD.
             MOVE W15-TCOST          TO CT-COST.
             MOVE W15-ACNO           TO CT-CRED.
             MOVE 01                 TO CT-CODE.
             MOVE ZERO               TO CT-AGE.
             MOVE W15-TSELL          TO CT-SELL.
             MOVE W15-VAT            TO CT-TAX.
             PERFORM BG600.
             PERFORM BG700.
           IF WX-INT = "Y"
               MOVE W20-DTE          TO WX-DATE
               MOVE "  CR"           TO WX-LEDG
               MOVE W90-CODE         TO WX-TRN
               MOVE W00-C-DESC       TO WX-DESC
               MOVE "- CREDITORS"    TO WX-BOOK
               COMPUTE WX-VALUE = CT-COST * -1
               MOVE WX-CREGL         TO WX-AC
               MOVE 02               TO WX-TYPE
               PERFORM GL-TRF-UPDATE
               IF W15-RAIL NOT = ZERO
                   MOVE W15-RAIL     TO WX-VALUE
                   MOVE WX-RLGL      TO WX-AC
                   MOVE 21           TO WX-TYPE
                   PERFORM GL-TRF-UPDATE
               END-IF
               IF W15-DISCOUNT NOT = ZERO
                   MOVE W15-DISCOUNT TO WX-VALUE
                   MOVE WX-DSGL      TO WX-AC
                   MOVE 23           TO WX-TYPE
                   PERFORM GL-TRF-UPDATE.
             MOVE 1                  TO WS-S WS-S2.
             MOVE W15-TCOST          TO W20-RESULT.
             PERFORM BA40 THRU BA54.
             MOVE 1                  TO WS-S2.

       BA86.
             MOVE W25-CFLOW(WS-S2)   TO W65-CFLOW(WS-S2).
           IF WS-S2 < 6
               ADD 1                 TO WS-S2
               GO TO BA86.
             MOVE 1                  TO WS-S2.

       BA90.
           IF W65-CFLOW(WS-S) NOT = ZERO
               ADD WS-S2             TO CT-AGE.
           IF WS-S < 6
               ADD 1                 TO WS-S
               MULTIPLY 2            BY WS-S2
               GO TO BA90.
             ADD W15-TCOST           TO P-BAL.
             ADD W15-TCOST           TO P-CR.
             ADD W65-CFLOW(6)        TO P-180D.
             ADD W65-CFLOW(5)        TO P-150D.
             ADD W65-CFLOW(4)        TO P-120D.
             ADD W65-CFLOW(3)        TO P-90D.
             ADD W65-CFLOW(2)        TO P-60D.
             ADD W65-CFLOW(1)        TO P-30D.
           IF CT-AGE = ZERO
               ADD W15-TCOST         TO P-OUT W70-DUE-NOW.
             PERFORM WRITE-CRTRAN THRU WRITE-CRTRAN-EXIT.
             MOVE 1                  TO WS-ACTION.
             PERFORM AY08 THRU AY59.
             PERFORM REWRITE-CREDIT-UNLOCK THRU WRITE-CREDIT-EXIT.
             ADD W15-TCOST           TO W70-CLOSE.
             ADD W15-TCOST           TO W70-CRED.
             ADD W65-CFLOW(6)        TO W70-DUE-180.
             ADD W65-CFLOW(5)        TO W70-DUE-150.
             ADD W65-CFLOW(4)        TO W70-DUE-120.
             ADD W65-CFLOW(3)        TO W70-DUE-90.
             ADD W65-CFLOW(2)        TO W70-DUE-60.
             ADD W65-CFLOW(1)        TO W70-DUE-30.
      *      ADD W65-V2              TO W70-DUE-NOW.
      *
      *    ****    VAT Control
      *
             MOVE 1                  TO W05-V-RATE.

       BA92.
             ADD 6 W05-V-RATE        GIVING WS-VAT-SUB.
           IF (W15-VATVAL(W05-V-RATE) = ZERO) AND (W15-VATVAL(WS-VAT-SUB) = ZERO)
               GO TO BA93.
             MOVE W05-VAT-CODE       TO DPT-CODE.
             MOVE 0                  TO WS-ACTION.
             PERFORM AY38 THRU AY59.
             ADD W15-VATVAL(W05-V-RATE) TO DPT-I-VAT-DAY DPT-I-VAT-MTD DPT-I-VAT-YTD.
             ADD W15-VATVAL(WS-VAT-SUB) TO DPT-I-VAT-DAY DPT-I-VAT-MTD DPT-I-VAT-YTD.
             ADD W15-TAXVAL(W05-V-RATE) TO DPT-V-PUR-DAY DPT-V-PUR-MTD DPT-V-PUR-YTD.
             ADD W15-TAXVAL(WS-VAT-SUB) TO DPT-V-PUR-DAY DPT-V-PUR-MTD DPT-V-PUR-YTD.
             PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.

       BA93.
           IF W05-V-RATE < 6
               ADD 1                 TO W05-V-RATE
               GO TO BA92.
           IF (WX-INT = "Y") AND (W15-VAT NOT = ZERO)
               MOVE W15-VAT          TO WX-VALUE
               MOVE DPT-I-VAT-GL     TO WX-AC
               MOVE 13               TO WX-TYPE
               PERFORM GL-TRF-UPDATE.
      *
      *    ****    End Recovery Logging.
      *
       BA95.
           IF WS-PRCKEY > 2
               MOVE WS-PRCKEY        TO WS-KEYSTR
               MOVE 1                TO WS-PRCKEY
               INITIALIZE PLB-REC1
               MOVE WS-KEYSTR        TO PLB-QUANT
               PERFORM REWRITE-PRCLAB THRU WRITE-PRCLAB-EXIT
               MOVE WS-KEYSTR        TO WS-PRCKEY.
               CLOSE CREDIT.
               OPEN I-O CREDIT.
               PERFORM AY70 THRU AY999.
           IF W02-LINAGE < (W02-PRN-LENGTH - 2)
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM BA00B.
             MOVE SPACES             TO R-DET.
      *
      *      ***** CREDITOR INVOICE TOTAL *****
      *
             MOVE WS-HCRED           TO G-H1.
             MOVE W15-ACNO           TO G-SUPP.
             MOVE W15-NAME           TO G-NAME.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE "INV:"             TO G-H3.
             MOVE "DTE:"             TO G-H4.
             MOVE "RAIL:"            TO G-H5.
             MOVE "DSC:"             TO G-H6.
             MOVE "VAT:"             TO G-H6A.
             MOVE "TOTAL:"           TO G-H7.
             MOVE W15-INVN           TO G-INVN.
             MOVE W25-INVD           TO G-INVD.
             MOVE W15-RAIL           TO G-RAIL.
             MOVE W15-DISCOUNT       TO G-DISC.
             MOVE W15-VAT            TO G-VAT.
             MOVE W15-TCOST          TO G-TOTAL.
             ADD W15-TCOST           TO W15-TOTAL.
           IF W02-LINAGE < (W02-PRN-LENGTH - 2)
               MOVE 2                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM BA00B.
             MOVE SPACES             TO R-DET.
             CLOSE PURCH.
             OPEN OUTPUT PURCH.
             CLOSE PURCH.
             OPEN I-O PURCH.

       BA99.
             EXIT.
      *
      *    ****    I N S E R T   M A R K U P   P E R C E N T A G E S
      *
       BA100      SECTION 5.
       BA101.
             MOVE STK-LDG            TO DPT-CODE.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
             MOVE 0                  TO WS-S9.
             PERFORM TEST AFTER UNTIL WS-S9 = 4
               ADD 1                 TO WS-S9
               MOVE DPT-MKUP(WS-S9)  TO STK-MKUP(WS-S9)
               MOVE DPT-CMKUP(WS-S9) TO STK-CMKUP(WS-S9)
               MOVE DPT-WMKUP(WS-S9) TO STK-WMKUP(WS-S9)
             END-PERFORM.
             PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.

       BA199.
             EXIT.

       BA999.
             EXIT.
      *
      *    ****    Print the audit trail HEADING
      *
       BA00B      SECTION 5.
       BA0B.
             MOVE 0                  TO WS-ADVANCE.
             MOVE 2                  TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "H"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DET.
             MOVE WS-H1              TO R-H1.
             MOVE W60-COMPANY        TO R-H2.
             MOVE WS-H3              TO R-H3.
             ADD 1                   TO WS-PAGE.
             MOVE W12-TODAY          TO R-DTE1.
             MOVE WS-PAGE            TO R-PGE.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE W60-DESC           TO R-H2.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE W50-H4             TO R-HEAD.
             PERFORM CALL-PRINTUTL.
             MOVE 0                  TO WS-ADVANCE.
             MOVE 3                  TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "H"                TO W02-PRN-TYPE.
             MOVE 2                  TO WS-ADVANCE.
             MOVE WS-HDESC           TO W03-H3.
             MOVE W03-HEADING        TO R-DET.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE "D"                TO W02-PRN-TYPE.

       BA999B.
             EXIT.
      *
      *    R E - A L L O C A T E   V A T   T O   V A R I O U S   R A T E S
      *
       BA00C      SECTION 5.
       BA0C.
             MOVE 12                 TO WS-S9.
             MOVE W15-VAT            TO W55-VAT.

       BA05C.
           IF NOT(W15-VATVAL(WS-S9) = ZERO)
               IF W15-VATVAL(WS-S9) > W55-VAT
                   MOVE W55-VAT      TO W15-VATVAL(WS-S9)
               END-IF
               SUBTRACT W15-VATVAL(WS-S9) FROM W55-VAT
               MOVE WS-S9            TO WS-S4.
           IF W55-VAT > ZERO
               IF WS-S9 > 1
                   SUBTRACT 1        FROM WS-S9
                   GO TO BA05C
               ELSE
                   ADD W55-VAT       TO W15-VATVAL(WS-S4)
                   SUBTRACT W15-VATVAL(WS-S9) FROM W55-VAT.

       BA10C.
           IF WS-S9 > 1
               SUBTRACT 1            FROM WS-S9
               MOVE ZERO             TO W15-VATVAL(WS-S9)
               GO TO BA10C.

       BA999C.
             EXIT.
      /
       BB000      SECTION 5.
       BB00.
             MOVE W15-COST           TO W20-RESULT.
             PERFORM BA40 THRU BA60.
           IF W15-COST NOT > ORD-COST
               GO TO BB05.
             MOVE ORD-COST           TO W20-RESULT.
             PERFORM BA40 THRU BA54.

       BB05.
             MOVE ORD-DELIV          TO W12-MONTH.
             MOVE 1                  TO WS-S2.

       BB10.
            MULTIPLY -1              BY W25-CFLOW(WS-S2).
           IF WS-S2 < 6
               ADD 1                 TO WS-S2
               GO TO BB10.
             MOVE 1                  TO WS-S2.
             PERFORM BA55 THRU BA60.
             MOVE W12-MM      TO W12-MONTH.

       BB099.
             EXIT.

      *
      *    ****    C A L L   S T O C K   C R E A T I O N
      *
       BD100        SECTION 5.
       BD102.
             PERFORM SAVE-SCREEN.
             CLOSE RECOVER.
             CALL "STP\STP010" USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
             CANCEL "STP010".
             OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
             MOVE ZERO               TO WS-RECOVER.
             PERFORM RESTORE-SCREEN.

       BD199.
             EXIT.
      *
      *    ****    V I E W   T R A N S A C T I O N   D E T A I L S
      *
       BE000        SECTION 5.
       BE00.
             PERFORM SAVE-SCREEN-3.
             MOVE 1                  TO WS-VIEWKEY WS-V-FIRST WS-V-LAST W75-S1 W75-S.

       BE02.
             MOVE ZERO               TO W75-PKEY(W75-S1).
           IF W75-S1 < 34
               ADD 1                 TO W75-S1
               GO TO BE02.
             MOVE ZERO               TO W75-S1.

       BE05.
             DISPLAY S03-VIEW.

       BE10.
             PERFORM CLEAR-L50.

       BE15.
             MOVE WS-VIEWKEY         TO WS-PURKEY.
             PERFORM READ-PURCH THRU READ-PURCH-EXIT.
             MOVE 1202               TO CPOS.

       BE20.
             MOVE PUR-EXT-STOCK      TO STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
             DISPLAY S03-VIEW-LINE.
             MOVE WS-PURKEY          TO W75-PKEY(W75-S) WS-VIEWKEY.
           IF CLIN > 44
               GO TO BE25.
      *
      *    ****    T E S T   F O R   E N D   O F   I N P U T   I T E M S
      *
       BE22.
           IF WS-PURKEY NOT < WS-PURLAST
               GO TO BE25.
             ADD 1                   TO WS-PURKEY W75-S CLIN.
             PERFORM READ-PURCH THRU READ-PURCH-EXIT.
             GO TO BE20.
      *
      *    ****    D I S P L A Y   I N S T R U C T I O N S   O N
      *             L I N E   5 0   W H E N   P A G E   F U L L
      *
       BE25.
             DISPLAY S03-USER-OPT.
      *
      *    ****    S A V E   C U R R E N T   P A G E
      *
       BE30.
             PERFORM SAVE-SCREEN-2.
             MOVE 0                  TO W75-S1.
             MOVE 1202               TO CPOS.
      *
      *    ****    S C R E E N   A T T R I B U T E S
      *
       BE35.
             MOVE 3920               TO W41-LENGTH.
             CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS W42-ATTRIB2 W41-LENGTH RETURNING WS-STATUS.
             MOVE 4000               TO W41-LENGTH.
      *
      *    ****    G E T   U S E R   O P T I O N
      *
       BE40.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             PERFORM CLEAR-L50.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY    GO TO BE45
                 WHEN DOWN-KEY  GO TO BE50
                 WHEN ENTER-KEY
                 WHEN CR-KEY    GO TO BE60
                 WHEN OTHER     PERFORM AA900-ALARM
               END-EVALUATE
               GO TO BE40
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY   GO TO BE995
                 WHEN PAGE-DOWN GO TO BE65
                 WHEN PAGE-UP   GO TO BE75
                 WHEN OTHER     PERFORM AA900-ALARM
               END-EVALUATE
               GO TO BE40.
             PERFORM AA900-ALARM.
             GO TO BE40.
      *
      *    ****    U P   K E Y
      *
       BE45.
           IF W75-S1 > 1
               SUBTRACT 1            FROM W75-S1 CLIN.
               GO TO BE55.
      *
      *    ****    D O W N   K E Y
      *
       BE50.
           IF W75-S1 = ZERO
               MOVE 1                TO W75-S1
               GO TO BE55.
                  IF W75-S1 < W75-S
                      ADD 1          TO W75-S1 CLIN.
      *
      *    ****    H I G H L I G H T   C U R R E N T   L I N E
      *
       BE55.
            PERFORM BE35.
            DISPLAY W43-DET(CLIN) AT CPOS WITH BACKGROUND-COLOR Black FOREGROUND-COLOR Grey.
            GO TO BE40.
      *
      *    ****    G E T   S E L E C T E D   R E C O R D
      *
       BE60.
             PERFORM RESTORE-SCREEN-3.
           IF W75-S1 = ZERO
               GO TO BE40.
             MOVE W75-PKEY(W75-S1)   TO WS-PURKEY.
             PERFORM READ-PURCH THRU READ-PURCH-EXIT.
             GO TO BE80.
      *
      *    ****    P A G E   D O W N
      *
       BE65.
           IF W75-S < 34
               GO TO BE35.
      *
      *    ****    N E W   P A G E
      *
       BE70.
             MOVE 1202               TO CPOS.
             PERFORM BE10.
             MOVE 12                 TO CLIN.
             MOVE LOW-VALUES         TO W75-KEYS.
             MOVE 1                  TO W75-S W75-S1.
             PERFORM BE02.
             MOVE WS-VIEWKEY         TO WS-PURKEY.
             GO TO BE22.
      *
      *    ****    P A G E   U P
      *
       BE75.
           IF WS-VIEWKEY > 67
               SUBTRACT 68           FROM WS-VIEWKEY
           ELSE
               MOVE ZERO             TO WS-VIEWKEY.
             GO TO BE70.

       BE80.
             MOVE PUR-EXT-STOCK      TO STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.

       BE85A.
             MOVE PUR-EXT-STOCK      TO W10-EXT-ITEM.
             MOVE PUR-PUN            TO W15-PUN.
             MOVE PUR-IUN            TO W15-IUN.
             MOVE PUR-PCOST          TO W15-PCOST.
             MOVE PUR-ICOST          TO W15-ICOST.
             MOVE PUR-UWSALE         TO W15-UWSALE.
             MOVE PUR-UCASH          TO W15-UCASH.
             MOVE PUR-USELL          TO W15-USELL.
             MOVE PUR-COST           TO W15-COST.
             MOVE PUR-INCVAL         TO W15-INCOST.
             MOVE PUR-SELL           TO W15-SELL.
             MOVE PUR-IVAT           TO W15-IVAT.
             MOVE PUR-VAT-SUB        TO WS-VAT-SUB.
             MOVE PUR-WSALE-2        TO W10-WSALE-2.
             MOVE PUR-CASH-2         TO W10-CASH-2.
             MOVE PUR-SELL-2         TO W10-SELL-2.
             MOVE PUR-WSALE-3        TO W10-WSALE-3.
             MOVE PUR-CASH-3         TO W10-CASH-3.
             MOVE PUR-SELL-3         TO W10-SELL-3.
             MOVE PUR-WSALE-4        TO W10-WSALE-4.
             MOVE PUR-CASH-4         TO W10-CASH-4.
             MOVE PUR-SELL-4         TO W10-SELL-4.
             MOVE PUR-CASES          TO W10-CASES.
           IF NOT(WS-VAT-SUB = ZERO)
               MOVE W05-VAT(WS-VAT-SUB) TO W05-RATE
           ELSE
               MOVE ZERO  TO W05-RATE.
      *
      *    ****    R E S T O R E   O R I G I N A L   S C R E E N
      *
       BE85B.
             PERFORM RESTORE-SCREEN-3.
      *
      *    ****    R E D U C E  R U N N I N G  T O T A L
      *
           IF P-TAX NOT = "N"
               IF STK-TAX > ZERO
                   SUBTRACT W15-COST FROM W15-TXBL.
             SUBTRACT W15-COST       FROM W15-TCOST.
             SUBTRACT W15-INCOST     FROM W15-INCVAL.
             SUBTRACT W15-IUN        FROM W15-QUANT.
             SUBTRACT W15-SELL       FROM W15-TSELL.
             SUBTRACT W15-IVAT       FROM W15-VATVAL(WS-VAT-SUB).
             SUBTRACT W15-COST       FROM W15-TAXVAL(WS-VAT-SUB).
      *
      *    ****    D I S P L A Y   S E L E C T E D   I T E M
      *              D E T A I L S   A S  C A P T U R E D
      *
           IF WS-EXT-STK = "Y"
               DISPLAY S07-EXT AT 0717
           ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
           IF W10-STCK = "A"
               IF WS-USE-ITM = "Y"
                   DISPLAY S07-ITM AT 0717
               ELSE
                   DISPLAY S07 AT 0717.
             DISPLAY STK-DESC AT 0736 WITH FOREGROUNDCOLOR Blue BACKGROUND-COLOR Grey.
       BE85C.
      *    IF WS-USE-3 = "Y"
      *        DISPLAY S37-3 AT 1017
      *    ELSE
               DISPLAY S37.
           IF STK-PRC = ZERO
               MOVE 1                TO STK-PRC.
             MULTIPLY W15-PUN BY STK-PRC GIVING W15-IUN.
             DISPLAY S37S.
             DISPLAY S38.
             MOVE W15-ICOST  TO W100-S8V3.
             DISPLAY W100-S8V3 AT 0944 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
      *
      *    ****    C A L C U L A T E   T A X A B L E / N O N -
      *     T A X A B L E   A M O U N T
      *
       BE86.
             MOVE ZERO               TO W15-U-TXBL W15-U-NTXBL W15-U-VAT.
           IF STK-TAX NOT = ZERO
      *
      *    ****    N O T   A   T A X   I N V O I C E
      *
               IF P-TAX = "N"
                   MOVE W15-PCOST    TO W15-U-NTXBL W15-U-INCVAL W15-U-PNETT
               ELSE
      *
      *    ****    T A X   I N V O I C E   ( I N C L U S I V E )
      *
               IF P-TAX = "I"
                   MOVE W15-PCOST    TO W15-U-INCVAL
                   COMPUTE W15-U-TXBL ROUNDED = W15-U-INCVAL - ((W15-U-INCVAL / (100.00000 + W05-RATE)) * W05-RATE)
                   MOVE W15-U-TXBL   TO W15-U-PNETT
                   SUBTRACT W15-U-TXBL FROM W15-U-INCVAL GIVING W15-U-VAT
               ELSE
      *
      *    ****    T A X   I N V O I C E   ( E X C L U S I V E )
      *
                   MOVE W15-PCOST    TO W15-U-TXBL W15-U-PNETT
                   COMPUTE W15-U-VAT ROUNDED = W15-U-TXBL * W05-RTE
                   ADD W15-U-TXBL W15-U-VAT GIVING W15-U-INCVAL
               END-IF
           ELSE
      *
      *    ****    N O N   T A X A B L E   I T E M
      *
               MOVE W15-PCOST        TO W15-U-NTXBL W15-U-INCVAL W15-U-PNETT.
             COMPUTE W15-U-NETT ROUNDED = (W15-PCOST * W15-PUN) / W15-IUN.
           IF W15-U-TXBL NOT = ZERO
               MULTIPLY W15-PUN BY W15-U-TXBL GIVING W15-COST ROUNDED
           ELSE
               MULTIPLY W15-PUN BY W15-U-NTXBL GIVING W15-COST ROUNDED.
      *
      *    ****    C A L C U L A T E   V A T   /   I N C L U S I V E
      *
           IF (P-TAX = "E") AND (STK-TAX NOT = ZERO)
               COMPUTE W15-INCOST ROUNDED = (W15-COST + (W15-COST * W05-RTE))
           ELSE
               MULTIPLY W15-PUN BY W15-U-INCVAL GIVING W15-INCOST ROUNDED.
             SUBTRACT W15-COST FROM W15-INCOST GIVING W15-IVAT ROUNDED.
             MULTIPLY W15-PUN BY W15-PCOST GIVING W15-ILCOST ROUNDED.
      *
      *      ***** UNIT NETT COST *****
      *
             DISPLAY S47.
      *
      *      ***** VAT PER UNIT *****
      *
             DISPLAY S48.
      *
      *      ***** SELLING PRICES *****
      *
             DISPLAY S41A-C.
      *      MOVE W15-UWSALE         TO W100-S8V2.
      *      DISPLAY W100-S8V2 AT 1617 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
      *      MOVE W15-UCASH          TO W100-S8V2.
      *      DISPLAY W100-S8V2 AT 1633 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
      *      MOVE W15-USELL          TO W100-S8V2.
      *      DISPLAY W100-S8V2 AT 1649 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
      *
      *      ***** TOTAL VAT *****
      *
             DISPLAY S45.
      *
      *      ***** COST EXCLUDING VAT *****
      *
             DISPLAY S40.
      *
      *      ***** COST INCLUDING VAT *****
      *
             DISPLAY S46.
       BE88.
      *
      *      ***** EXCLUSIVE (RUNNING TOTAL) *****
      *
             MOVE W15-TCOST          TO W25-SVAL.
             DISPLAY W25-S5V2 AT 1866 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
             MOVE 1                  TO WS-S9.
             MOVE ZERO               TO W15-VAT.
      *
      *    ***** Accumulate the tax *****
      *
       BE88-TAX.
             ADD W15-VATVAL(WS-S9)   TO W15-VAT.
           IF WS-S9 < 12
               ADD 1                 TO WS-S9
               GO TO BE88-TAX.
      *
      *      ***** V A T   (RUNNING TOTAL)
      *
             DISPLAY S43.
      *
      *      ***** INCLUSIVE (RUNNING TOTAL) *****
      *
            COMPUTE W25-SVAL = W15-TCOST + W15-VAT.
            DISPLAY W25-S5V2 AT 2266 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
      *
      *    ****    A L L O W   A M E N D M E N T S   T O
      *                  S E L E C T E D   L I N E
      *
       BE89.
             MOVE "'A'mend/'D'elete or 'E'xit" TO WS-OPT-MES.
             MOVE SPACE              TO WS-OPTION.
             MOVE 8                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "E"
               ADD W15-IVAT          TO W15-VATVAL(WS-VAT-SUB)
               ADD W15-COST          TO W15-TAXVAL(WS-VAT-SUB)
               GO TO BE990.
           IF WS-OPTION = "D"
               GO TO BE105.
      *
      *    ****    A L L O W   Q U A N T I T Y  A M E N D M E N T
      *
       BE90.
      *    IF WS-USE-3 = "Y"
      *        ACCEPT S37-3 AT 0817
      *    ELSE
              ACCEPT S37.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  PERFORM BE85A
                               GO TO BE990
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO BE90
               END-EVALUATE.
      *    IF W15-IUN = ZERO
      *        GO TO BE90.
      *
      *    ****    C O N V E R T I O N   F R O M   P U R C H A S E
      *                U N I T   T O   S E L L I N G   U N I T
      *
           IF W15-PUN = ZERO
               GO TO BE90.
           IF STK-PRC = ZERO
               MOVE 1                TO STK-PRC.

       BE90B.
             MULTIPLY W15-PUN BY STK-PRC GIVING W15-IUN.
             ACCEPT S37S.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  GO TO BE90
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO BE90B
               END-EVALUATE.
           IF W15-IUN = ZERO
               GO TO BE90B.
      *
      *    ****   A R E   C A S E S   C O N T R O L L E D
      *
           IF WS-USE-CASES= "Y"
               IF STK-USE-CASES = "Y"
      *         GET THE NUMBER OF CASES RECEIVED
                   PERFORM BF000.
      *
      *    ****   A L L O W   P U R C H A S E - U N I T
      *                C O S T   A M E N D M E N T
      *
       BE95.
             ACCEPT S38.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  PERFORM BE85A
                               GO TO BE990
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO BE95
               END-EVALUATE.
           IF W15-PCOST = ZERO
               GO TO BE95.
             COMPUTE W15-ICOST ROUNDED = (W15-PCOST * W15-PUN) / W15-IUN.
             PERFORM BE86.
             PERFORM BA29 THRU BA33B-1.
             MULTIPLY W15-USELL BY W15-IUN GIVING W15-SELL.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               PERFORM BE85A
               GO TO BE85C.
           IF NOT(WS-VAT-SUB = ZERO)
               ADD W15-COST          TO W15-TAXVAL(WS-VAT-SUB)
               ADD W15-IVAT          TO W15-VATVAL(WS-VAT-SUB).

       BE100.
             MOVE "'P'rice,'B'arcode,'N'o labels  [ ]" TO WS-OPT-MES.
             MOVE SPACE              TO WS-OPTION.
             MOVE 5                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
             MOVE W15-PUN            TO PUR-PUN.
             MOVE W15-IUN            TO PUR-IUN.
             MOVE W15-ICOST          TO PUR-ICOST.
             MOVE W15-PCOST          TO PUR-PCOST.
             MOVE W15-UWSALE         TO PUR-UWSALE.
             MOVE W15-UCASH          TO PUR-UCASH.
             MOVE W15-USELL          TO PUR-USELL.
             MOVE W15-COST           TO PUR-COST.
             MOVE W15-IVAT           TO PUR-IVAT.
             MOVE W15-INCOST         TO PUR-INCVAL.
             MOVE W15-SELL           TO PUR-SELL.
             MOVE WS-OPTION          TO PUR-LAB.
             PERFORM REWRITE-PURCH THRU WRITE-PURCH-EXIT.
             GO TO BE990.
      *
      *    ****    D E L E T E  A   R E C O R D
      *
       BE105.
             MOVE WS-PURKEY          TO WS-V-FIRST.
             ADD 1 WS-V-FIRST        GIVING WS-V-LAST.
      *
      *    ****    S H I F T   R E C O R D S   U P
      *
       BE110.
           IF NOT(WS-V-LAST > WS-PURLAST)
               MOVE WS-V-LAST        TO WS-PURKEY
               PERFORM READ-PURCH THRU READ-PURCH-EXIT
               MOVE WS-V-FIRST       TO WS-PURKEY
               PERFORM REWRITE-PURCH THRU WRITE-PURCH-EXIT
               ADD 1                 TO WS-V-FIRST WS-V-LAST
               GO TO BE110.
             MOVE WS-V-FIRST         TO WS-PURKEY.
             PERFORM READ-PURCH THRU READ-PURCH-EXIT.
             DELETE PURCH.
             SUBTRACT 1              FROM WS-PURLAST.
             GO TO BE995.

       BE990.
           IF P-TAX NOT = "N"
               IF STK-TAX > ZERO
                   ADD W15-COST      TO W15-TXBL.
             ADD W15-INCOST          TO W15-INCVAL.
             ADD W15-IUN             TO W15-QUANT.
             ADD W15-COST            TO W15-TCOST.
             ADD W15-SELL            TO W15-TSELL.

       BE995.
             PERFORM RESTORE-SCREEN-3.
             MOVE CSTART             TO WS1-CURSOR.
             MOVE ZERO               TO COL-AND-LINE.
             MOVE WS-PURLAST         TO WS-PURKEY.

       BE999.
             EXIT.
      *
      *    ****    C A S E S
      *
       BF000        SECTION 5.
       BF00.
             PERFORM SAVE-SCREEN-3.
             MOVE 11                 TO SHADE-ROW.
             MOVE 28                 TO SHADE-COL.
             MOVE 27                 TO SHADE-WIDTH.
             MOVE 2                  TO SHADE-LINES.
             DISPLAY S08.
             PERFORM SCREEN-SHADOW.

       BF10.
             ACCEPT S09.
           IF W10-CASES = ZERO
               MOVE "Cases may not be zero" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BF10.
             PERFORM CHECK-CORRECT.
             PERFORM RESTORE-SCREEN-3.
           IF WS-OPTION = "N"
               GO TO BF00.

       BF999.
             EXIT.
      *
      *    ****    P A C K S
      *
       BG000        SECTION 5.
       BG00.
             MOVE ZERO               TO WS-ERROR.
             MOVE 2                  TO WS-S9.

       BG05.
             MOVE STK-UNT(WS-S9)     TO W10-UNT(WS-S9).
             MOVE STK-PR(WS-S9)      TO W10-PR(WS-S9).
             MOVE STK-SL(WS-S9)      TO W10-SL(WS-S9).
             MOVE STK-CSH(WS-S9)     TO W10-CSH(WS-S9).
             MOVE STK-WSL(WS-S9)     TO W10-WSL(WS-S9).
           IF STK-TAX NOT = ZERO
               COMPUTE W10-VSL(WS-S9)  ROUNDED = W10-SL(WS-S9)  + (W10-SL(WS-S9)  * W05-RTE)
               COMPUTE W10-VCSH(WS-S9) ROUNDED = W10-CSH(WS-S9) + (W10-CSH(WS-S9) * W05-RTE)
               COMPUTE W10-VWSL(WS-S9) ROUNDED = W10-WSL(WS-S9) + (W10-WSL(WS-S9) * W05-RTE).
             MOVE STK-MKUP(WS-S9)    TO W10-MKUP(WS-S9).
             MOVE STK-CMKUP(WS-S9)   TO W10-CMKUP(WS-S9).
             MOVE STK-WMKUP(WS-S9)   TO W10-WMKUP(WS-S9).
           IF WS-S9 < 4
               ADD 1                 TO WS-S9
               GO TO BG05.
             PERFORM SAVE-SCREEN.
             CALL "CRP\CRPPACKS" USING WS-PARID LS-USER-ID W10-STOCK
                ON EXCEPTION
                   MOVE "Program- CRPPACKS not on disk, press ANY key" TO WS-ERR-MES
                   PERFORM ERROR-MESSAGE
                   MOVE 9            TO WS-ERROR
                   PERFORM RESTORE-SCREEN
                   GO TO BG999.
              CANCEL "CRP\CRPPACKS".
              PERFORM RESTORE-SCREEN.

       BG999.
             EXIT.
      /
       PORDER-LOOKUP        SECTION 5.
       PORDER-LUP-SCREEN.
             MOVE "C"                TO W75-X-C.
             PERFORM SAVE-SCREEN.
             DISPLAY S-POR.

       PORDER-LUP-CLR-SCREEN.
             MOVE 7                  TO SHADE-ROW.
             MOVE 11                 TO SHADE-COL.
             MOVE 66                 TO SHADE-WIDTH.
             MOVE 15                 TO SHADE-LINES.
             PERFORM SCREEN-SHADOW.
             PERFORM CLEAR-L50.
             MOVE 9                  TO CLIN.
             MOVE 1                  TO W75-S W75-S1.

       PORDER-LUP-TAB-LOOP.
             MOVE ZERO               TO W75-POR(W75-S1).
             MOVE SPACES             TO W75-CODE(W75-S1) W75-XCODE(W75-S1).
           IF W75-S1 < 18
               ADD 1                 TO W75-S1
               GO TO PORDER-LUP-TAB-LOOP.
             MOVE ZERO               TO W75-S1.

       PORDER-LUP-START.
      *      MOVE P-NUMBER           TO ORD-CRED.
             MOVE W15-ACNO           TO ORD-CRED.
             MOVE SPACES             TO ORD-EXT-ITEM.
             PERFORM START-AT-ORD-CRED THRU READ-PORDER-EXIT.
           IF WS-F-ERROR = 16
               GO TO PORDER-NO-RECORD.

       PORDER-GET-ORD-RECORD.
           IF WS-OPTION = "J"
               MOVE SPACE            TO WS-OPTION
               GO TO PORDER-SKIP-ORD-READ.
             PERFORM READ-PORDER-NEXT THRU READ-PORDER-EXIT.
           IF WS-F-ERROR = 16
               SUBTRACT 1            FROM W75-S
               GO TO PORDER-FULL-SCREEN.
           IF NOT(ORD-CRED = P-NUMBER)
               SUBTRACT 1            FROM W75-S
               GO TO PORDER-FULL-SCREEN.

       PORDER-SKIP-ORD-READ.
             MOVE ORD-KEY            TO W75-POR(W75-S).
             MOVE ORD-EXT-ITEM       TO STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               MOVE ORD-EXT-ITEM     TO STK-XREF
               PERFORM START-AT-STOCK-XREF THRU READ-STOCK-EXIT
               IF WS-F-ERROR = 22
                   GO TO PORDER-GET-ORD-RECORD
               END-IF
               PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT
               IF NOT(STK-XREF = ORD-EXT-ITEM)
                   GO TO PORDER-GET-ORD-RECORD
               ELSE
                   MOVE STK-CODE     TO ORD-EXT-ITEM
                   PERFORM REWRITE-PORDER THRU WRITE-PORDER-EXIT
                   GO TO PORDER-LUP-CLR-SCREEN
               END-IF.
             MOVE STK-CODE           TO W75-CODE(W75-S).
             MOVE STK-XREF           TO W75-XCODE(W75-S).
             DISPLAY SD-POR AT LINE CLIN.
           IF (W75-X-C = "X")
               IF NOT(STK-XREF = SPACES)
                   DISPLAY SD-XREF AT LINE CLIN.
                   IF CLIN < 20
                       ADD 1         TO CLIN W75-S
                       GO TO PORDER-GET-ORD-RECORD.
             MOVE 12                 TO W75-S.

       PORDER-FULL-SCREEN.
             DISPLAY S03-POR-OPT.

       PORDER-SAVE-FULL-SCREEN.
             PERFORM SAVE-SCREEN-2.
             MOVE 0                  TO W75-S1.
             MOVE 0910               TO CPOS.

       PORDER-CRT-ATTRIB.
             MOVE 3920               TO W41-LENGTH.
             CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS W42-ATTRIB2 W41-LENGTH RETURNING WS-STATUS.
             MOVE 4000               TO W41-LENGTH.

       PORDER-GET-USER-OPT.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             PERFORM CLEAR-L50.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY    GO TO PORDER-ADJ-NO-KEYS
                 WHEN DOWN-KEY  GO TO PORDER-CHK-NO-KEYS
                 WHEN ENTER-KEY
                 WHEN CR-KEY    GO TO PORDER-CHK-SELECT
                 WHEN OTHER     PERFORM AA900-ALARM
               END-EVALUATE
               GO TO PORDER-GET-USER-OPT
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN F8-KEY    PERFORM PORDER-XREF THRU PORDER-XREF-LOOP
                 WHEN ESC-KEY   GO TO PORDER-RESTORE-SCREEN
                 WHEN PAGE-DOWN GO TO PORDER-GET-NEXT-PAGE
                 WHEN PAGE-UP   GO TO PORDER-GET-PREV-PAGE
                 WHEN OTHER     PERFORM AA900-ALARM
               END-EVALUATE
               GO TO PORDER-GET-USER-OPT.
             PERFORM AA900-ALARM.
             GO TO PORDER-GET-USER-OPT.

       PORDER-XREF.
             MOVE CLIN               TO STORE-LIN.
           IF W75-X-C = "C"
               MOVE "X"              TO W75-X-C
               DISPLAY "XRef" AT 0827 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
           ELSE
               MOVE "C"   TO W75-X-C
               DISPLAY "Item" AT 0827 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
             MOVE 1                  TO W75-S2.
             MOVE 9                  TO CLIN.

       PORDER-XREF-LOOP.
             DISPLAY SD-ITEM AT LINE CLIN.
           IF (W75-X-C = "X")
               IF NOT(W75-XCODE(W75-S2) = SPACES)
                   DISPLAY SD-XCDE AT LINE CLIN.
           IF W75-S2 < W75-S
               ADD 1                 TO CLIN W75-S2
               GO TO PORDER-XREF-LOOP.
             MOVE STORE-LIN          TO CLIN.

       PORDER-GET-NEXT-PAGE.
           IF W75-S < 12
               GO TO PORDER-CRT-ATTRIB.

       PORDER-NEW-PAGE.
             DISPLAY S-POR-DET.
             MOVE 9                  TO CLIN.
             MOVE LOW-VALUES         TO W75-KEYS.
             MOVE 1                  TO W75-S W75-S1.
             PERFORM PORDER-LUP-TAB-LOOP.
             GO TO PORDER-GET-ORD-RECORD.

       PORDER-GET-PREV-PAGE.
             MOVE 1                  TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
             MOVE "J"                TO WS-OPTION.

       PORDER-GET-PREV-LOOP.
             PERFORM READ-PORDER-PREV THRU READ-PORDER-EXIT.
           IF WS-F-ERROR = 16
               MOVE SPACE            TO WS-OPTION
               GO TO PORDER-NEW-PAGE.
           IF NOT(ORD-CRED = P-NUMBER)
               MOVE SPACE            TO WS-OPTION
               GO TO PORDER-NEW-PAGE.
           IF WS-IXS1 < 24
               ADD 1                 TO WS-IXS1
               GO TO PORDER-GET-PREV-LOOP.
             GO TO PORDER-NEW-PAGE.

       PORDER-NO-RECORD.
             DISPLAY "No outstandig orders - Press " AT 5012 WITH BACKGROUND-COLOR Cyan FOREGROUNDCOLOR Blue
                                                  "ANY"      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                                     " key"  WITH BACKGROUND-COLOR Cyan FOREGROUNDCOLOR Blue.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             GO TO PORDER-RESTORE-SCREEN.

       PORDER-ADJ-NO-KEYS.
           IF W75-S1 > 1
               SUBTRACT 1            FROM W75-S1 CLIN.
               GO TO PORDER-HIGHLIGHT.

       PORDER-CHK-NO-KEYS.
           IF W75-S1 = ZERO
               MOVE 1                TO W75-S1
               GO TO PORDER-HIGHLIGHT.
           IF W75-S1 < W75-S
               ADD 1                 TO W75-S1 CLIN.

       PORDER-HIGHLIGHT.
             PERFORM PORDER-CRT-ATTRIB.
             DISPLAY W43-DET2(CLIN) AT CPOSS-POR-DET FOREGROUND-COLOR Grey.
             GO TO PORDER-GET-USER-OPT.

       PORDER-CHK-SELECT.
           IF W75-S1 = ZERO
               GO TO PORDER-GET-USER-OPT.
             MOVE CSTART             TO WS1-CURSOR.
             MOVE W75-POR(W75-S1)    TO ORD-KEY.
             PERFORM READ-PORDER THRU READ-PORDER-EXIT.
             MOVE ZERO               TO COL-AND-LINE.
             MOVE ORD-KEY            TO W15-ORD.
             GO TO PORDER-RESTORE-SCREEN.

       PORDER-RESTORE-SCREEN.
             PERFORM RESTORE-SCREEN.

       PORDER-LOOKUP-EXIT.
             EXIT.

       BG600      SECTION 6.
       BG601.
             MOVE CT-CODE            TO W90-CODE.
             PERFORM GET-CREDITOR-TRAN-RECORD.
             SUBTRACT CT-COST        FROM W00-C-VAL W00-C-MTDV W00-C-YTDV.
             ADD 1                   TO W00-C-DAY W00-C-MTD W00-C-YTD.
             PERFORM UPDATE-TRAN-RECORD.

       BG699.
             EXIT.

       BG700      SECTION 6.
       BG701.
             MOVE 3                  TO TAX-ACTYPE.
             MOVE CT-CODE            TO TAX-CODE.
             MOVE W20-DTE            TO TAX-DATE.
             MOVE CT-REF             TO TAX-REF.
             MULTIPLY -1 BY CT-COST  GIVING TAX-VALUE.
             MULTIPLY -1 BY CT-TAXFREE GIVING TAX-TAXFREE.
      *      MULTIPLY -1 BY CT-TAX   GIVING TAX-VAT.
             MOVE CT-CRED            TO TAX-AC.
           IF W00-C-ACT = 1
               IF CT-CODE = 03
                   MOVE "3"          TO TAX-TYPE
               ELSE
                   MOVE "2"          TO TAX-TYPE
           ELSE
               MOVE "1"              TO TAX-TYPE.
             MOVE ZERO               TO TAX-SEQ.
             MOVE 1                  TO WS-S9.

       BG705.
             MULTIPLY -1 BY W15-TAXVAL(WS-S9) GIVING TAX-VAL(WS-S9).
             MULTIPLY -1 BY W15-VATVAL(WS-S9) GIVING TAX-VAT(WS-S9).
           IF WS-S9 < 12
               ADD 1                 TO WS-S9
               GO TO BG705.
             PERFORM WRITE-TXTRAN THRU WRITE-TXTRAN-EXIT.
             MOVE 1                  TO WS-ACTION.
             PERFORM AY19 THRU AY59.

       BG799.
             EXIT.

       COPY "CREDIT56.TRN".

      *
      *    R O U N D I N G   O F   S E L L I N G   P R I C E S
      *
      *    This routine will round the selling price as follows:
      *
      *    If the rounding factor is less than 50 cents or greater
      *    than R1.00 but less than five rand, then the selling price
      *    (.CC or R.CC portion) will be rounded to a multiple of the
      *    rounding factor.
      *
      *    If the rounding factor is R1.00, then if the selling price
      *    is greater than 79 cents it will be rounded to the next rand.
      *
      *    If the rounding factor is greater than 49 cents and less than
      *    R1.00 or greater than R4.99 then the selling price (.CC or
      *    R.CC portion) will be changed to the rounding value. If the
      *    rounding portion of the selling price is greater than the
      *    factor then the system will amend these figures and change
      *    the adjoining value.
      *
       CB100      SECTION 6.
       CB101.
      *
      *    ROUND TO THE NEXT RAND
      *
           IF W05-ROUND-VAL = 1.00
               IF W15-RSC = ZERO
                   GO TO CB199
               ELSE
      *
      *    SELLING PRICE LESS THAN ONE RAND
      *
               IF W15-RSELL < 1.00
      *
      *    SELLING PRICE IS MORE THAN 79 CENTS - CHANGE TO R1.00
      *
                   IF W15-RSC > 79
                       MOVE 1.00     TO W15-RSELL
                       GO TO CB199
                   ELSE
      *
      *    IF THE SELLING PRICE IS LESS THAN 80 CENTS DO NOT ROUND UP
      *
                       GO TO CB199
      *
      *    SELLING PRICE MORE THAN ONE RAND
      *
               ELSE
      *
      *    CENTS PORTION OF PRICE IS LESS THAN TEN CENTS - DROP CENTS
      *
                   IF W15-RSC < 10
                       MOVE ZERO     TO W15-RSC
                       GO TO CB199
                   ELSE
      *
      *    ROUND UP TO NEXT R1.00
      *
                       MOVE ZERO     TO W15-RSC
                       ADD 1         TO W15-RSR
                       GO TO CB199.
      *
      *    ROUNDING CENTS ONLY
      *
           IF W05-ROUND-VAL < 1.00
      *
      *    ROUNDING VALUE AND CENTS PORTION ARE EQUAL - EXIT
      *
               IF W15-RSC = W05-CENTS
                   GO TO CB199
               ELSE
      *
      *    ROUNDING VALUE IS 50 CENTS OR MORE
      *
               IF W05-ROUND-VAL > .50
      *
      *    IF CENTS PORTION TO BE ROUNDED IS LESS THAN ROUNDING VALUE
      *
                   IF W15-RSC < W05-CENTS
                       MOVE W05-CENTS TO W15-RSC
                       GO TO CB199
      *
      *    IF CENTS PORTION TO BE ROUNDED IS MORE THAN ROUNDING VALUE
      *
                   ELSE
                       MOVE W05-CENTS TO W15-RSC
                       ADD 1          TO W15-RSR
                       GO TO CB199
      *
      *    ROUNDING VALUE IS LESS THAN 50 CENTS
      *
               ELSE
                   GO TO CB120.
      *
      *    IF ROUNDING VALUE EQUALS THE R.CC OF VALUE TO BE ROUNDED - EXIT
      *
           IF W15-RSRCC = W05-ROUND-VAL
               GO TO CB199.
      *
      *    SELLING PRICE IS NOT MORE THAN HALF THE ROUNDING FACTOR - EXIT
      *
           IF W15-RSELL NOT > W05-HLF-VAL
               GO TO CB199.
           IF W05-ROUND-VAL > 5.00
               GO TO CB110.
      *
      *    ROUNDING FACTOR IS LESS THAN FIVE RAND
      *
      *    R.CC OF VALUE TO BE ROUNDED IS LESS THAN ROUNDING FACTOR
      *
           IF W15-RSRCC < W05-ROUND-VAL
               MOVE W05-ROUND-VAL    TO W15-RSRCC
               GO TO CB199.
             MOVE W05-ROUND-VAL      TO W05-CALC-VAL.

       CB105.
           IF W15-RSRCC = W05-CALC-VAL
               GO TO CB199.
           IF W15-RSRCC < W05-CALC-VAL
               MOVE W05-CALC-VAL     TO W15-RSRCC
               GO TO CB199.
             ADD W05-ROUND-VAL       TO W05-CALC-VAL.
           IF W05-CALC-VAL < 10.00
               GO TO CB105.
             ADD 1                   TO W15-RSR8.
             MOVE W05-RVRCC          TO W15-RSRCC.
             GO TO CB199.
      *
      *    ROUNDING FACTOR GREATER THAN R5.00
      *
      *    SELLING PRICE IS NOT MORE THAN 75% OF ROUNDING FACTOR - EXIT
      *
       CB110.
           IF W15-RSELL NOT > W05-TQTR-VAL
               GO TO CB199.

       CB115.
           IF W15-RSRCC < W05-ROUND-VAL
               MOVE W05-ROUND-VAL    TO W15-RSRCC
               GO TO CB199.
             ADD 1                   TO W15-RSR8.
             MOVE W05-ROUND-VAL      TO W15-RSRCC.
             GO TO CB199.
      *
      *    ROUND UP VALUES WHERE ROUNDING FACTOR IS LESS THAN 50 CENTS
      *
       CB120.
           IF W05-CENTS = 2 OR 4 OR 5 OR 10 OR 20 OR 25
               MOVE ZERO             TO WS-SUB
           ELSE
               MOVE W05-CENTS        TO WS-SUB.

       CB125.
           IF W15-RSC = WS-SUB
               GO TO CB199.
           IF W15-RSC < WS-SUB
               MOVE WS-SUB           TO W15-RSC
               GO TO CB199.
             ADD W05-CENTS           TO WS-SUB.
           IF WS-SUB < 100
               GO TO CB125.
             ADD 1                   TO W15-RSR.
             SUBTRACT 100 FROM WS-SUB GIVING W15-RSC.

       CB199.
             EXIT.
      *
      *    ****    C A L C U L A T E   E X C L U S I V E   P R I C E
      *              F R O M   T H E   I N C L U S I V E   P R I C E
      *
       CB200      SECTION 6.
       CB201.
           IF STK-TAX NOT = ZERO
               COMPUTE W15-USELL ROUNDED = (W10-VSELL   / (100.00000 + W05-RATE)) * 100.00
               COMPUTE W15-UWSALE ROUNDED = (W10-VWSALE / (100.00000 + W05-RATE)) * 100.00
               COMPUTE W15-UCASH ROUNDED = (W10-VCASH   / (100.00000 + W05-RATE)) * 100.00
           ELSE
               MOVE W10-VWSALE       TO W15-UWSALE
               MOVE W10-VCASH        TO W15-UCASH
               MOVE W10-VSELL        TO W15-USELL.
           IF STK-TAX NOT = ZERO
               COMPUTE W10-CHECK ROUNDED = W15-USELL + (W15-USELL * W05-RTE)
               IF W10-CHECK > W10-VSELL
                   SUBTRACT 0.01     FROM W15-USELL
               ELSE
               IF W10-CHECK < W10-VSELL
                   ADD 0.01          TO W15-USELL
               END-IF
               COMPUTE W10-CHECK ROUNDED = W15-UCASH + (W15-UCASH * W05-RTE)
               IF W10-CHECK > W10-VCASH
                   SUBTRACT 0.01     FROM W15-UCASH
               ELSE
               IF W10-CHECK < W10-VCASH
                   ADD 0.01          TO W15-UCASH
               END-IF
               COMPUTE W10-CHECK ROUNDED = W15-UWSALE + (W15-UWSALE * W05-RTE)
               IF W10-CHECK > W10-VWSALE
                   SUBTRACT 0.01     FROM W15-UWSALE
               ELSE
               IF W10-CHECK < W10-VWSALE
                   ADD 0.01          TO W15-UWSALE.

       CB299.
             EXIT.
      *
      *    ****    W A R E H O U S E
      *
       HA000        SECTION 6.
       HA00.
             PERFORM SAVE-SCREEN-3.
             MOVE 11                 TO SHADE-ROW.
             MOVE 30                 TO SHADE-COL.
             MOVE 24                 TO SHADE-WIDTH.
             MOVE 2                  TO SHADE-LINES.
             DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 1028 WITH Black BACKGROUND-COLOR Grey FOREGROUND-COLOR.
             DISPLAY "³ Warehouse Code:        ³" AT 1128 WITH Black BACKGROUND-COLOR Grey FOREGROUND-COLOR.
             DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 1228 WITH Black BACKGROUND-COLOR Grey FOREGROUND-COLOR.
             PERFORM SCREEN-SHADOW.
             MOVE SPACES             TO W15-WHSE.

       HA05.
             DISPLAY S55.
             ACCEPT W15-WHSE AT 1146 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black UPDATE AUTO.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  MOVE SPACES TO W15-WHSE
                               PERFORM RESTORE-SCREEN-3
                               GO TO HA999
                 WHEN F1-KEY   PERFORM HELP-ROUTINE
                               GO TO HA05
                 WHEN F2-KEY   PERFORM AA50W THRU AA59
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO HA05
               END-EVALUATE
               DISPLAY W15-WHSE AT 1146 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black
               IF W15-WHSE = SPACE
                   GO TO HA05.
             PERFORM CLEAR-L50.
           IF W15-WHSE = SPACES
               PERFORM RESTORE-SCREEN-3
               GO TO HA999.
             MOVE 6                  TO W50-LENGTH.

       HA10.
             MOVE W15-WHSE           TO W50-DATA.
           IF W50-1 = SPACE
               MOVE W50-5            TO W15-WHSE
               GO TO HA10.
             CALL "CBL_TOUPPER" USING W50-DATA BY VALUE W50-LENGTH RETURNING W50-STATUS.
             MOVE W50-DENT           TO W15-WHSE.
             MOVE W50-DENT           TO WAR-CODE.
             PERFORM READ-WARHSE THRU READ-WARHSE-EXIT.
           IF WS-F-ERROR = 51
               MOVE "Z"              TO W10-ETYPE
               PERFORM AA50W THRU AA59
               DISPLAY W15-WHSE AT 1146 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black
               IF W15-WHSE = SPACES
                   GO TO HA05
               ELSE
                   GO TO HA10.
             PERFORM RESTORE-SCREEN-3.
             DISPLAY "W/H " AT 0629 BACKGROUND-COLOR Grey.
             DISPLAY WAR-SALE AT 0640 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Cyan.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               PERFORM RESTORE-SCREEN-3
               GO TO HA00.

       HA999.
             EXIT.
      *
      *    ****    W A R E H O U S E   Q U A N T I T I E S
      *
       IA000        SECTION 6.
       IA00.
             MOVE ZERO               TO W15-WQUANT.
             MOVE W10-EXT-ITEM       TO WST-EXT-CODE.
             MOVE SPACES             TO WST-WAR.
             PERFORM START-AT-WH-ITEM THRU READ-WSTOCK-EXIT.
           IF WS-F-ERROR = 52
               GO TO IA999.

       IA05.
             PERFORM READ-WSTOCK-NEXT THRU READ-WSTOCK-EXIT.
           IF WS-F-ERROR = 52
               GO TO IA999.
           IF NOT(WST-EXT-CODE = W10-EXT-ITEM)
               GO TO IA999.
           IF NOT(WST-QUANT < 0.01)
               ADD WST-QUANT         TO W15-WQUANT.
             GO TO IA05.

       IA999.
             EXIT.
      *    *************************************************************
      *                 I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT    SECTION 9.
       ZA000-START.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W12-TODAY.
             MOVE LS-TODAY-YYMMDD    TO W12-T-YMD.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY W60-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

             MOVE 3                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF NOT(PAR-STOCK = "A" OR "N")
               MOVE "A"              TO PAR-STOCK.
               MOVE PAR-STOCK        TO W10-STCK.
           IF PAR-STOCK = "N"
               MOVE PAR-SLNGTH       TO W10-SLNGTH
           ELSE
               MOVE 14               TO W10-SLNGTH.
           IF W10-SLNGTH < 3
               MOVE 3                TO W10-SLNGTH
           ELSE
           IF W10-SLNGTH > 14
               MOVE 14               TO W10-SLNGTH.
             MOVE PAR-USE-3-DEC      TO WS-USE-3.
           IF NOT(PAR-USE-3-DEC = "Y")
               MOVE "N"              TO WS-USE-3.
             MOVE PAR-BAR-CODE       TO WS-BAR-CODE.
             MOVE PAR-CASES          TO WS-USE-CASES.
             MOVE PAR-PACKS          TO WS-USE-PACKS.
             MOVE PAR-USE-ITM        TO WS-USE-ITM.
             MOVE PAR-EXT-STK        TO WS-EXT-STK.
             OPEN I-O PRCLAB.
           IF NOT(WS-STATUS = "00")
               CLOSE PRCLAB
               OPEN OUTPUT PRCLAB
               INITIALIZE PLB-REC1
               MOVE 2                TO PLB-QUANT
               PERFORM WRITE-PRCLAB THRU WRITE-PRCLAB-EXIT
               CLOSE PRCLAB
               OPEN I-O PRCLAB.
             READ PRCLAB.
             MOVE PLB-QUANT          TO WS-PRCKEY.
             OPEN EXTEND BARCOD.
             OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
             OPEN OUTPUT PURCH.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
             CLOSE PURCH.
             OPEN I-O PURCH.
             MOVE 5                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-ROUND          TO W05-ROUND.
             MOVE PAR-ROUND-VAL      TO W05-ROUND-VAL.
           IF W05-ROUND-VAL > 0.01
               COMPUTE W05-HLF-VAL = W05-ROUND-VAL / 2
           ELSE
               MOVE ZERO             TO W05-HLF-VAL.
           IF W05-HLF-VAL > 0.01
               COMPUTE W05-TQTR-VAL = W05-HLF-VAL * 1.5
           ELSE
               MOVE ZERO             TO W05-TQTR-VAL.
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
             MOVE 6                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF PAR-INTEGRATE NOT = "Y"
               MOVE "N"              TO WX-INT
           ELSE
               MOVE "Y"              TO WX-INT.
             MOVE PAR-DEBGL          TO WX-DEBGL.
             MOVE PAR-CREGL          TO WX-CREGL.
             MOVE PAR-INTGL          TO WX-INTGL.
             MOVE PAR-BNKGL          TO WX-BNKGL.
             MOVE PAR-UNPROF         TO WX-UNPROF.
             MOVE PAR-REDGL          TO WX-REDGL.
             MOVE PAR-ADJGL          TO WX-ADJGL.
             MOVE PAR-RLGL           TO WX-RLGL.
             MOVE PAR-DSGL           TO WX-DSGL.
             PERFORM ERASE-SCREEN.
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
       ZB000-I-O-ERROR SECTION 9.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 2          MOVE W02-NETWORK TO WS-FILE
                               MOVE WS-NETKEY   TO WS-KEY
               WHEN 3          MOVE W02-CREDIT  TO WS-FILE
                               MOVE P-NUMBER    TO WS-KEYX
               WHEN 4          MOVE W02-CRTRAN  TO WS-FILE
                               MOVE CT-KEY      TO WS-KEYX
               WHEN 7          MOVE W02-DEPART  TO WS-FILE
                               MOVE DPT-CODE    TO WS-KEYX
               WHEN 15         MOVE WS-PARID    TO WS-FILE
                               MOVE WS-PARKEY   TO WS-KEY
               WHEN 16         MOVE W02-PORDER  TO WS-FILE
                               MOVE ZERO        TO WS-KEY
               WHEN 17         MOVE W02-PURCH   TO WS-FILE
                               MOVE WS-PURKEY   TO WS-KEY
               WHEN 18         MOVE W02-RECOVER TO WS-FILE
                               MOVE WS-RECKEY   TO WS-KEY
               WHEN 22         MOVE W02-STOCKF  TO WS-FILE
                               MOVE STK-CODE    TO WS-KEYX
               WHEN 32         MOVE W02-TXTRAN  TO WS-FILE
                               MOVE TAX-KEY     TO WS-KEYX
               WHEN 37         MOVE W02-SHARED  TO WS-FILE
                               MOVE WS-SHARED   TO WS-KEY
               WHEN 40         MOVE W02-LEDTRF  TO WS-FILE
                               MOVE XFR-KEY     TO WS-KEYX
               WHEN 51         MOVE W02-WARHSE  TO WS-FILE
                               MOVE WAR-CODE    TO WS-KEYX
               WHEN 52         MOVE W02-WSTOCK  TO WS-FILE
                               MOVE WST-KEY     TO WS-KEYX
               WHEN 56         MOVE W02-PURDEX  TO WS-FILE
                               MOVE PRD-KEY     TO WS-KEYX
               WHEN 64         MOVE W02-PRCLAB  TO WS-FILE
                               MOVE WS-PRCKEY   TO WS-KEY
               WHEN OTHER      MOVE "ERROR"     TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
