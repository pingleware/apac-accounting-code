      $set LINKCOUNT"596" GNT"CRP012.GNT"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******     ****       **      ******    *
      *   **    **  **    **  **    **   **  **     ***     **    **   *
      *   **        **    **  **    **  **    **     **          **    *
      *   **        ******    *******   **    **     **        **      *
      *   **        **   **   **        **    **     **      **        *
      *   **    **  **    **  **         **  **      **     **         *
      *    ******   **    **  **          ****     ******   ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R   D E B I T   N O T E S   ( S T O C K )    *
      *                                                                *
      *       Version 9.04.05 - June 2018                              *
      *                                                                *
      ******************************************************************
      *                                                                *
      * June 2018 - Added program CRPRDWR to handle IO for Creditor    *
      *             file and COPY file CA155.CRP to call and handlw IO *
      *             for the CREDITOR file. SEE CA155.CRP and CRPRDWR   *
      *             for details.                                       *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        CRP012.
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

       COPY "CONTROL.SL".

       COPY "CREDIT.SL".

       COPY "CRETRN.SL".

       COPY "CRTRAN.SL".

       COPY "DEPART.SL".

       COPY "LEDTRF.SL".

       COPY "PARAM.SL".

       COPY "PURCH.SL".

       COPY "PURDEX.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

       COPY "STOCK.SL".

       COPY "TXTRAN.SL".

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

       COPY "CREDIT.FDE".

       COPY "CRETRN.FDE".

       COPY "CRTRAN.FDE".

       COPY "DEPART.FDE".

       COPY "LEDTRF.FDE".

       COPY "PARAM.FDE".

       COPY "PURCH.FD".

       COPY "PURDEX.FDE".

       COPY "RECOVER.FD".

       COPY "SHARED.FDE".

       COPY "STOCK.FDE".

       COPY "TXTRAN.FDE".

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
       77  WS-PURKEY               PIC  9(04)    COMP-5.
       77  WS-KEYSTR               PIC  9(04)    COMP-5.
       77  WS-RECOVER              PIC  9(04)    COMP-5.
       77  WS-SHARED               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-STOCK                PIC  X(01).
       77  WS-PURLAST              PIC  9(04)    COMP-5.
       77  WS-TRANS                PIC  9(04)    COMP-5 VALUE 1.
       77  WS-ERROR                PIC  9(01)           VALUE 0.
       77  WS-ETYPE                PIC  X(01).
       77  WS-DEL                  PIC  X(01).
       77  WS-S                    PIC  9(04)    COMP-5.
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S9                   PIC  9(04)    COMP-5.
       77  WS-CRETRN               PIC  9(04)    COMP-5.
       77  WS-VAT                  PIC S9(07)V99 COMP-3.
       77  WS-IVAT                 PIC S9(07)V99 COMP-3.
       77  WS-ORD                  PIC  X(01).
       77  WS-OPTION               PIC  X(01).
       77  WS-ALLOCATE             PIC  X(01).
       77  WS-SKIP                 PIC  X(01)           VALUE "Y".
       77  WS-LOOK                 PIC  X(01)           VALUE "S".
       77  WS-USE-CASES            PIC  X(01).
       77  WS-USE-PACKS            PIC  X(01).
       77  WS-USE-ITM              PIC  X(01).
       77  WS-EXT-STK              PIC  X(01).
       77  WS-CHAR1                PIC  X(01).
       77  WS-SKEY                 PIC  9(05).
       77  WS-KEY1                 PIC  9(04)    COMP-5.
       77  WS-LINES                PIC  9(04)    COMP-5 VALUE 0.
       77  WS-PAGE                 PIC  9(04)    COMP-5 VALUE 0.
       77  WS-ER1                  PIC  X(09)           VALUE "No record".
       77  WS-HDTE                 PIC  X(04)           VALUE "DATE".
       77  WS-HACNO                PIC  X(04)           VALUE "ACNO".
       77  WS-HCRED                PIC  X(09)           VALUE "CREDITOR:".
       77  WS-HQNT                 PIC  X(05)           VALUE "QUANT".
       77  WS-HDESC                PIC  X(11)           VALUE "DESCRIPTION".
       77  WS-HCOST                PIC  X(10)           VALUE "COST PRICE".
       77  WS-HSELL                PIC  X(10)           VALUE "SELL PRICE".
       77  WS-HNAME                PIC  X(04)           VALUE "NAME".
       77  WS-HAGE                 PIC  X(12)           VALUE "AGE 30 - 180".
       77  WS-HSETT                PIC  X(22)           VALUE "30  60  90 120 150 180".
       77  WS-SDTE                 PIC  X(04)           VALUE "Date".
       77  WS-SACNO                PIC  X(04)           VALUE "Acno".
       77  WS-SCRED                PIC  X(09)           VALUE "Creditor:".
       77  WS-SQNT                 PIC  X(05)           VALUE "Quant".
       77  WS-SDESC                PIC  X(11)           VALUE "Description".
       77  WS-SCOST                PIC  X(10)           VALUE "Cost price".
       77  WS-SSELL                PIC  X(10)           VALUE "Sell price".
       77  WS-SNAME                PIC  X(04)           VALUE "Name".
       77  WS-SAGE                 PIC  X(12)           VALUE "Age 30 - 180".
       77  WS-SSETT                PIC  X(22)           VALUE "30  60  90 120 150 180".
       77  WS-H1                   PIC  X(05)           VALUE "DATE:".
       77  WS-H3                   PIC  X(05)           VALUE "PAGE:".
       77  PRG-NAME                PIC  X(12)           VALUE SPACES.
       77  PRG-CRED-LUP            PIC  X(12)           VALUE "CRP\CRPLOOK".
       77  PRG-STOCK-LUP           PIC  X(12)           VALUE "STP\STPLOOK".
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
      *       C = Clear comment line (50)
      *       E = Clear lines (any line or lines between 5 and 46)
      *       H = Change heading
      *       S = Clear the screen and display headings
      *
           03  CRT-TYPE              PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START             PIC  9(02).
           03  CRT-END               PIC  9(02).
      *
      *    Calling Program
      *
           03  CRT-PROGRAM           PIC  X(15) VALUE "CRP012".
      *
      *    Screen Heading
      *
           03  CRT-HEADING           PIC  X(40) VALUE "CREDITOR DEBIT NOTE (STOCK)".
      *
      *    Company Name
      *
           03  CRT-COMPANY           PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-TERMINAL.
               05  CRT-WRKHD       PIC  X(11).
               05  CRT-WRKST       PIC  X(03).


       COPY "WS.WS".

       01  WS-HELP.
           03  WS-MODULE           PIC  X(03)    VALUE "CRP".
           03  WS-PROG             PIC  X(03)    VALUE "012".

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

           03  R-L3A                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(02).
               05  I-H1            PIC  X(08).
               05  I-H2            PIC  X(19).
               05  I-H2A           PIC  X(19).
               05  I-H3            PIC  X(31).
               05  I-H3A           PIC  X(07).
               05  I-H4            PIC  X(11).
               05  I-H5            PIC  X(14).
               05  I-H6            PIC  X(13).
               05  I-H7            PIC  X(12).

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

           03  R-L9                              REDEFINES W02-PRN-LINE.
               05  REP-PG          PIC  X(06).
               05  REP-P-N         PIC  Z(03)9.
               05  FILLER          PIC  X(16).
               05  REP-COMP        PIC  X(40).
               05  FILLER          PIC  X(02).
               05  REP-REPORT      PIC  X(40).
               05  FILLER          PIC  X(08).
               05  REP-DAT         PIC  X(06).
               05  REP-DT          PIC  Z9/99/9999.

           03  DS-L1                             REDEFINES W02-PRN-LINE.
               05  DS-H1           PIC  X(06).
               05  DS-DATE         PIC  Z9/99/9999.
               05  FILLER          PIC  X(25).
               05  DS-COMP         PIC  X(41).
               05  DS-H2           PIC  X(40).
               05  DS-H3           PIC  X(06).
               05  DS-PAGE         PIC  ZZZ9.

           03  DS-L2                             REDEFINES W02-PRN-LINE.
               05  DS-H4           PIC  X(09).
               05  DS-ACNO         PIC  X(06).
               05  FILLER          PIC  X(01).
               05  DS-NAME         PIC  X(40).
               05  FILLER          PIC  X(02).
               05  DS-H5           PIC  X(11).
               05  DS-DNO          PIC  X(08).
               05  FILLER          PIC  X(02).
               05  DS-H6           PIC  X(05).
               05  DS-DNDTE        PIC  Z9/99/9999.
               05  FILLER          PIC  X(02).
               05  DS-H7           PIC  X(11).
               05  DS-INV          PIC  X(08).
               05  FILLER          PIC  X(01).
               05  DS-H8           PIC  X(06).
               05  DS-IDTE         PIC  Z9/99/9999.

           03  DS-L3                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(01).
               05  DS-H9           PIC  X(16).
               05  DS-H10          PIC  X(29).
               05  DS-H11          PIC  X(13).
               05  DS-H12          PIC  X(13).
               05  DS-H13          PIC  X(12).
               05  DS-H14          PIC  X(15).
               05  DS-H15          PIC  X(11).
               05  DS-H16          PIC  X(22).

           03  DS-L4                             REDEFINES W02-PRN-LINE.
               05  DS-ITEM         PIC  X(14).
               05  FILLER          PIC  X(03).
               05  DS-DESC         PIC  X(24).
               05  FILLER          PIC  X(04).
               05  DS-UCOST        PIC  Z(05)9.999.
               05  FILLER          PIC  X(04).
               05  DS-USELL        PIC  Z(05)9.99.
               05  FILLER          PIC  X(02).
               05  DS-QNT          PIC  Z(05)9.99.
               05  FILLER          PIC  X(03).
               05  DS-COST         PIC  Z(06)9.999.
               05  FILLER          PIC  X(04).
               05  DS-SELL         PIC  Z(07)9.99.
               05  FILLER          PIC  X(02).
               05  DS-30           PIC  X(01).
               05  FILLER          PIC  X(03).
               05  DS-60           PIC  X(01).
               05  FILLER          PIC  X(03).
               05  DS-90           PIC  X(01).
               05  FILLER          PIC  X(03).
               05  DS-120          PIC  X(01).
               05  FILLER          PIC  X(03).
               05  DS-150          PIC  X(01).
               05  FILLER          PIC  X(03).
               05  DS-180          PIC  X(01).
               05  FILLER          PIC  X(01).

       COPY "W05.VAT".

       COPY "W10.CRP".

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

       01  W30-AGE.
           03  W30-CUR         PIC S9(09)V99 COMP-3.
           03  W30-30          PIC S9(09)V99 COMP-3.
           03  W30-60          PIC S9(09)V99 COMP-3.
           03  W30-90          PIC S9(09)V99 COMP-3.
           03  W30-120         PIC S9(09)V99 COMP-3.
           03  W30-150         PIC S9(09)V99 COMP-3.
           03  W30-180         PIC S9(09)V99 COMP-3.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W50-H4.
           03  W50-HEAD        PIC X(20).

       01  W55-DEL.
           03  W55-MNTH        PIC X(03).
           03  FILLER          PIC X(01)     VALUE ",".
           03  W55-YY          PIC 9(02).

       01  W60-H2.
           03  W60-COMPANY     PIC X(40).
           03  W60-DESC        PIC X(21)     VALUE "CREDITORS AUDIT TRAIL".

       01  W65-CREDITOR.
           03  W65-CFLOW       PIC S9(09)V99 COMP-3 OCCURS 6.
           03  W65-V1          PIC S9(09)V99 COMP-3.
           03  W65-V2          PIC S9(09)V99 COMP-3.

       COPY "W70CRED.WS".

       01  W75-KEYS.
           03  W75-S               PIC  9(02)    COMP-5.
           03  W75-S1              PIC  9(02)    COMP-5.
           03  W75-S2              PIC  9(02)    COMP-5.
           03  W75-KEY             PIC  9(06)    COMP-5.
           03  W75-ALLOCATED       PIC S9(09)V99 COMP-3.
           03  W75-BALANCE         PIC S9(09)V99 COMP-3.
           03  W75-CNO                           OCCURS 18.
               05  W75-WHCDE.
                   07  W75-PKEY    PIC  9(05).
                   07  FILLER      PIC  X(01).
           03  W75-ITEMS OCCURS 18.
               05  W75-CODE        PIC  X(14).
               05  W75-XCODE       PIC  X(14).
               05  W75-POR         PIC  9(10).

       01  W90-TRAN.
           03  W90-CODE            PIC  9(02).
           03  W90-RESULT          PIC  9(04)    COMP-5.
           03  W90-TRNKEY          PIC  9(04)    COMP-5.
           03  W90-TS              PIC  9(02)    COMP-5.
           03  W90-DB              PIC S9(09)V99 COMP-3.
           03  W90-CR              PIC S9(09)V99 COMP-3.
           03  W90-ALLOCATED       PIC S9(09)V99 COMP-3.
           03  W90-DISCOUNT        PIC S9(09)V99 COMP-3.
           03  W90-DEBIT           PIC S9(09)V99 COMP-3.
           03  W90-CREDIT          PIC S9(09)V99 COMP-3.

       01  W100-EDIT.
           03  W100-V2.
               05  W100-S6V2   PIC  Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V4 REDEFINES W100-V2.
               05  W100-EXT    PIC Z(06)9.99.
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.
           03  W100-S8V2       PIC  Z(07)9.99DB.

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

       01  S06.
           02  S06A.
      *                                             1         2         3         4         5         6         7         8
      *                                    12345678901234567890123456789012345678901234567890123456789012345678901234567890
      *
               03  LINE  6 COLUMN  2 VALUE "Creditor Ac. :".
               03  LINE  7 COLUMN  2 VALUE "D/N no:".
               03          COLUMN 20 VALUE                   "D/N date:".
               03          COLUMN 42 VALUE                                         "Inv no:".
               03          COLUMN 60 VALUE                                                           "Inv date:".
               03  LINE  9 COLUMN  2 VALUE "Item code    :".
               03  LINE 10 COLUMN  2 VALUE "Description  :".
               03  LINE 11 COLUMN  2 VALUE "Quantity     :".
               03  LINE 12 COLUMN  2 VALUE "Unit cost    :".
               03  LINE 13 COLUMN  2 VALUE "Discount-1   :".
               03  LINE 14 COLUMN  2 VALUE "Discount-2   :".
               03  LINE 15 COLUMN  2 VALUE "Unit nett    :".
               03  LINE 16 COLUMN  2 VALUE "Total cost   :".
               03  LINE 18 COLUMN  2 VALUE "Unit retail  :".
               03  LINE 19 COLUMN  2 VALUE "Total retail :".
           02  S06B.
               03  LINE 21 COLUMN  2 VALUE "Allocation - 30:".
               03          COLUMN 22 VALUE                     "60:".
               03          COLUMN 29 VALUE                            "90:".
               03          COLUMN 36 VALUE                                   "120:".
               03          COLUMN 44 VALUE                                           "150:".
               03          COLUMN 52 VALUE                                                   "180:".
               03  LINE 22 COLUMN 54 VALUE                                                     "Sub total :".
               03  LINE 23 COLUMN 54 VALUE                                                     "VAT       :".
               03  LINE 24 COLUMN 54 VALUE                                                     "Totals    :".

       01  S-ITEM-CODE.
      *
      *    ****   Set the colours to be used for these screens 
      * 
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  S07.
                   07  PIC X(14) USING W10-ITEM AUTO.
               
               05  S07-ITM.
                   07  LINE  9 COLUMN 20 PIC X(14) USING W10-ITEM AUTO.
                   07          COLUMN 35 PIC X(03) USING W10-ITM AUTO.
               
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

       01  S19-S40.
      *
      *    ****   Set the colours to be used for these screens 
      * 
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  S19.
                   07   PIC X(06) USING W15-ACNO AUTO.

               05  S27.
                   07   PIC X(08) USING W15-GRNN AUTO.

               05  S28.
                   07  PIC Z9/99/9999 USING W15-GRND AUTO.

               05  S29.
                   07  PIC X(08) USING W15-INVN AUTO.

               05  S30.
                   07  PIC Z9/99/9999 USING W15-INVD AUTO.

               05  S32.
                   07  PIC Z(07)9.99 USING W15-USELL AUTO.

               05  S35.
                   07  IC Z9.99 USING W15-DISC1 AUTO.

               05  S36.
                   07  PIC Z9.99 USING W15-DISC2 AUTO.

               05  S37.
                   07  PIC Z(07)9.99 USING W15-IUN AUTO.

               05  S38.
                   07  PIC Z(07)9.999 USING W15-ICOST AUTO.

               05  S40.
                   07  PIC Z(07)9.99 USING W15-COST AUTO.

       01  S42.
           03  BACKGROUND-COLOR Grey  FOREGROUND-COLOR Black.
               05  S42A.
      *                                         1         2         3         4         5         6         7         8
      *                                12345678901234567890123456789012345678901234567890123456789012345678901234567890
      *
                   07  LINE  6 COLUMN 17 VALUE        "      ".
                   07          COLUMN 24 VALUE               "                                        ".
                   07  LINE  7 COLUMN 10 VALUE "        ".
                   07          COLUMN 30 VALUE                     "          ".
                   07          COLUMN 50 VALUE                                         "        ".
                   07          COLUMN 70 VALUE                                                             "          ".
               05  S42B.
      *                                         1         2         3         4         5         6         7         8
      *                                12345678901234567890123456789012345678901234567890123456789012345678901234567890
      *
                   07  LINE  9 COLUMN 17 VALUE        "              ".
                   07  LINE 10 COLUMN 17 VALUE        "                              ".
                   07  LINE 11 COLUMN 17 VALUE        "           ".
                   07  LINE 12 COLUMN 17 VALUE        "            ".
                   07  LINE 13 COLUMN 23 VALUE              "     ".
                   07  LINE 14 COLUMN 23 VALUE              "     ".
                   07  LINE 15 COLUMN 17 VALUE        "            ".
                   07  LINE 16 COLUMN 17 VALUE        "           ".
                   07  LINE 18 COLUMN 17 VALUE        "           ".
                   07  LINE 19 COLUMN 17 VALUE        "           ".
                   07  LINE 21 COLUMN 19 VALUE          " ".
                   07          COLUMN 26 VALUE                 " ".
                   07          COLUMN 33 VALUE                        " ".
                   07          COLUMN 41 VALUE                                " ".
                   07          COLUMN 49 VALUE                                        " ".
                   07          COLUMN 57 VALUE                                                " ".
               05  S42C.
      *                                         1         2         3         4         5         6         7         8
      *                                12345678901234567890123456789012345678901234567890123456789012345678901234567890
      *
                   03  LINE 22 COLUMN 66 VALUE                                                         "             ".
                   03  LINE 23 COLUMN 66 VALUE                                                         "             ".
                   03  LINE 24 COLUMN 66 VALUE                                                         "             ".

       01  S43.
           03  PIC Z(07)9.99DB USING W15-VAT AUTO BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.

       01  S44.
           03  PIC Z(07)9.99DB USING W15-TCOST AUTO BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.

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

       01  S56.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
      *                                             1         2         3         4          5         6   
      *                                    1234567890123456789012345678901234567890123456678901234567890123
      *
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

       01  S-CRED.
      *                                         1         2         3         4    
      *                                12345678901234567890123456789012345678901234
      *
           03  LINE 23 COLUMN  2 VALUE "Balance :".
           03          COLUMN 12 PIC              Z(08)9.99DB                       USING P-BAL BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03          COLUMN 28 VALUE                           "Due :".
           03          COLUMN 34 PIC                                    Z(08)9.99DB USING P-OUT BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.

       01  S-ALLOC.
      *                             1         2         3         4         5         6         7 
      *                    12345678901234567890123456789012345678901234567890123456789012345678901
      *
           03  LINE 21 COLUMN 14 VALUE "Un-allocated balance : ".
           03          COLUMN 37 PIC                           Z(08)9.99                           FROM W75-BALANCE BACKGROUND-COLOR Magenta FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 51 VALUE                                      "Allocated : ".
           03          COLUMN 63 PIC                                                     Z(08)9.99 FROM W75-ALLOCATED BACKGROUND-COLOR Magenta FOREGROUND-COLOR Brown HIGHLIGHT.

       01  S-CTRN.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT. 
      *                              1         2         3         4         5         6         7   
      *                     1234567890123456789012345678901234567890123456789012345678901234567890123
      *
               05  COLUMN  6 PIC X(08)                                                                 FROM CRT-REF.
               05  COLUMN 15 PIC          9999/99/99                                                   FROM CRT-SDTE.
               05  COLUMN 26 PIC                     X(12)                                             FROM W00-C-DESC.
               05  COLUMN 39 PIC                                  Z(08)9.99                            FROM CRT-VALUE.
               05  COLUMN 52 PIC                                               Z(08)9.99               FROM CRT-OUTSTD.
               05  COLUMN 65 PIC                                                            Z(08)9.99  USING CRT-ALLOC AUTO.

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
      * *   * *     *     *  **   *     *   *  *  *  **   *   *     *  *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** *   *
      *
             PERFORM OPEN-PRINTER.
             MOVE "P"                TO WS-COMMAND.
             MOVE 0                  TO WS-ADVANCE.
             MOVE 3                  TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             PERFORM EA000.
             MOVE "C"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             CLOSE PURCH RECOVER.

       AA49.
             EXIT PROGRAM.

       COPY "AA50CS.LUP".

       AA050-UTILS     SECTION.
      *
      *    ****    C A L L   S T O C K   C R E A T I O N
      *
       AA100.
             PERFORM SAVE-SCREEN.
             CALL "STP\STP010" USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
             CANCEL "STP010".
             OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
             MOVE ZERO               TO WS-RECOVER.
             PERFORM RESTORE-SCREEN.

       AA199.
             EXIT.

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

       COPY "LOCKED.REC".

       COPY "PRINTUTL.AS4".

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

       COPY "CREDITOR.TRN".

      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S
      *
       AA300              SECTION.
       AA301.
             DISPLAY S56.

       AA305.
             EXIT.
             
       COPY "AA900.ALM".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AB00-READ      SECTION.

       COPY "CONTROL.RD".

       COPY "CRETRN.RD".

       COPY "CRTRAN.RD".

       COPY "CREDIT.RD".

       COPY "DEPART.RD".

       COPY "LEDTRF.RD".

       COPY "PARAM.RD".

       COPY "PURCH.RD".

       COPY "SHARED.RD".

       COPY "STOCK.RD".

       COPY "TXTRAN.RD".

      *
      *   R E W R I T E   &    W R I T E   F I L E S   R O U T I N E S
      *
       AC000-WRITE   SECTION.

       COPY "CONTROL.WR".

       COPY "CRETRN.WR".

       COPY "CRTRAN.WR".

       COPY "CREDIT.WR".

       COPY "DEPART.WR".

       COPY "LEDTRF.WR".

       COPY "PARAM.WR".

       COPY "PURCH.WR".

       COPY "PURDEX.WR".

       COPY "SHARED.WR".

       COPY "STOCK.WR".

       COPY "TXTRAN.WR".

       COPY "DATE.CHK".

      /
       AD000         SECTION.
       AD00.
             MOVE ZERO           TO WS-ERROR.
           IF W15-DD < 01 OR > 31
               GO TO AD15.
           IF W15-MM < 01 OR > 12
               GO TO AD15.
           IF W15-MM = 01 OR 03 OR 05 OR 07 OR 08 OR 10 OR 12
               GO TO AD999.
           IF W15-MM = 02 AND W15-DD > 28
               GO TO AD05.
           IF W15-DD > 30
               GO TO AD15.
             GO TO AD999.

       AD05.
           IF W15-DD > 29
               GO TO AD15.
            DIVIDE W15-CCYY BY 4
     GIVING W20-RESULT.
           IF W20-DECIMAL > ZERO
               GO TO AD15.
             GO TO AD999.

       AD15.
             MOVE 1              TO WS-ERROR.
       AD999.
             EXIT.

       COPY "CREDIT.ILU".

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
            MOVE 56   TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
            MOVE PRD-REC1  TO REC-PURDEX.
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
           IF NOT (PAR-USR(WS-USUB) = LS-USER)
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

       CA600      SECTION.
       CA601.
             MOVE CT-CODE        TO W90-CODE.
             PERFORM GET-CREDITOR-TRAN-RECORD.
             SUBTRACT CT-COST    FROM W00-C-VAL
                                      W00-C-MTDV
                                      W00-C-YTDV.
             ADD 1               TO W00-C-DAY
                                    W00-C-MTD
                                    W00-C-YTD.
             PERFORM UPDATE-TRAN-RECORD.

       CA699.
             EXIT.

       CA700         SECTION.
       CA701.
             MOVE 3                  TO TAX-ACTYPE.
             MOVE CT-CODE            TO TAX-CODE.
             MOVE W20-DTE            TO TAX-DATE.
             MOVE CT-REF             TO TAX-REF.
             MULTIPLY -1 BY CT-COST  GIVING TAX-VALUE.
             MULTIPLY -1 BY CT-TAXFREE GIVING TAX-TAXFREE.
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

       CA705.
             MOVE W15-TAXVAL(WS-S9)  TO TAX-VAL(WS-S9).
             MOVE W15-VATVAL(WS-S9)  TO TAX-VAT(WS-S9).
           IF WS-S9 < 12
               ADD 1                 TO WS-S9
               GO TO CA705.
             PERFORM WRITE-TXTRAN THRU WRITE-TXTRAN-EXIT.
             MOVE 1                  TO WS-ACTION.
             PERFORM AY19 THRU AY59.

       CA799.
             EXIT.

       COPY "LEDTRF.UPD".

       COPY "CREDIT.ALL".

      /
       EA000      SECTION 5.
       EA00.
           IF LS0-STP NOT = 2
               PERFORM ERASE-SCREEN
               MOVE "Stock not installed" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO EA999.
             MOVE ZERO               TO W15-TOTAL WS-PAGE W15-TOTVAT.
      *
      *    ****    Print the audit trail HEADING
      *
       EA05.
             MOVE "H"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DET.
             MOVE WS-H1              TO DS-H1.
             MOVE W60-COMPANY        TO DS-COMP.
             MOVE "- DEBIT NOTES"    TO DS-H2.
             MOVE WS-H3              TO DS-H3.
             ADD 1                   TO WS-PAGE.
             MOVE W12-TODAY          TO DS-DATE.
             MOVE WS-PAGE            TO DS-PAGE.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE "D"                TO W02-PRN-TYPE.
      *
      *    ****    S C R E E N   L A Y O U T .
      *
       EA10.
             PERFORM ERASE-SCREEN.
             DISPLAY S06.
             DISPLAY S42.
             MOVE 1                  TO WS-PURLAST.
      *
      *                                ENTER CREDIT ACCOUNT NUMBER.
      *
             MOVE SPACES             TO W15-ACNO.

       EA15.
             DISPLAY S55.
             ACCEPT S19 AT 0617.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  MOVE SPACES TO W15-ACNO
                               GO TO EA16
                 WHEN F1-KEY   PERFORM HELP-ROUTINE
                 WHEN F2-KEY   PERFORM AA50C THRU AA59
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO EA15
               END-EVALUATE
               DISPLAY S19 AT 0617
               IF W15-ACNO = SPACES
                  GO TO EA15.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W15-ACNO BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.

       EA16.
             PERFORM CLEAR-L50.
           IF W15-ACNO = SPACES
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE W15-TOTAL        TO W16-COST
               MOVE W15-TOTVAT       TO W16-VAT
               ADD W15-TOTAL W15-TOTVAT GIVING W16-TOTAL
               MOVE W16-TOTALS       TO DS-L1
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE SPACES           TO R-DET
               GO TO EA999.
             PERFORM CA155-GET-CREDITOR.
           IF WS-ERROR NOT = ZERO
               MOVE WS-ER1           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO EA15.
             MOVE P-NAME             TO W15-NAME.
             DISPLAY P-NAME AT 0624 WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Grey.
           IF P-TAX = "E"
               DISPLAY "(Exclusive)" AT 0665 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
           ELSE
           IF P-TAX = "I"
               DISPLAY "(Inclusive)" AT 0665 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
           ELSE
               DISPLAY "(No VAT)" AT 0965 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY S-CRED.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO EA15.

       EA19.
             MOVE ZERO               TO W30-CUR W30-30 W30-120 W30-60 W30-150 W30-90 W30-180 W15-TXBL.
             MOVE ZERO               TO W15-GRND W15-INVD.
             MOVE SPACES             TO W15-GRNN W15-INVN.
           IF W02-LINAGE NOT < (W02-PRN-LENGTH - 8)
               PERFORM EA05.

       EA20.
             ACCEPT S27 AT 0710.
           IF W15-GRNN = SPACES
               GO TO EA20.
             MOVE 8                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W15-GRNN BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.

       EA25.
             ACCEPT S28 AT 0730.
             MOVE W15-GRND           TO W22-DATE.
             PERFORM DATE-CHECK THRU CHECK-DATE-EXIT.
           IF NOT(W22-ERROR = 0)
               EVALUATE W22-ERROR
                 WHEN 1  MOVE "Invalid D/N DATE"  TO WS-ERR-MES
                 WHEN 2  MOVE "Invalid D/N DAY"   TO WS-ERR-MES
                 WHEN 3  MOVE "Invalid D/N MONTH" TO WS-ERR-MES
               END-EVALUATE
               PERFORM ERROR-MESSAGE
               GO TO EA25.
             MOVE W22-DTE            TO W25-INVD

       EA30.
             ACCEPT S29 AT 0750.
           IF W15-INVN = SPACES
               GO TO EA30.
             MOVE 8                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W15-INVN BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.

       EA35.
             ACCEPT S30 AT 0770.
             MOVE W15-INVD           TO W22-DATE.
             PERFORM DATE-CHECK THRU CHECK-DATE-EXIT.
           IF NOT(W22-ERROR = 0)
               EVALUATE W22-ERROR
                 WHEN 1  MOVE "Invalid Inv DATE"  TO WS-ERR-MES
                 WHEN 2  MOVE "Invalid Inv DAY"   TO WS-ERR-MES
                 WHEN 3  MOVE "Invalid Inv MONTH" TO WS-ERR-MES
               END-EVALUATE
               PERFORM ERROR-MESSAGE
               GO TO EA35.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO EA20.
             PERFORM CLEAR-L50.
             MOVE W22-DTE-YMD        TO W21-DTE.            
      *      MOVE W15-DD  TO W21-DD.
      *      MOVE W15-MM  TO W21-MM.
      *      MOVE W15-CC  TO W21-CC.
      *      MOVE W15-YY  TO W21-YY.
      
             MOVE ZERO               TO W15-QUANT W15-TCOST W15-DISCOUNT W15-DISC1 W15-DISC2 W15-INCVAL W15-TSELL W15-TXBL W15-VAT W15-IVAT W15-RAIL.
             MOVE 1                  TO WS-S9.

       EA36.
             MOVE ZERO               TO W15-VATVAL(WS-S9) W15-TAXVAL(WS-S9).
           IF WS-S9 < 12
               ADD 1                 TO WS-S9
               GO TO EA36.

       EA38.
             MOVE SPACES             TO W10-EXT-ITEM W15-DESCR.
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

       EA40.
             PERFORM AA300.

      *    *************************************************************
      *                     G E T   S T O C K   C O D E
      *    *************************************************************
      *    
      *      Is an 18 character stock/inventory item code being used?  
      *    
           IF WS-EXT-STK = "Y"
               ACCEPT S07-EXT AT 0917
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
                   ACCEPT S07-ITM AT 0917
               ELSE
                   ACCEPT S07 AT 0917
               END-IF
      *    *************************************************************
      *                         NUMERIC CODES APPLY 
      *    *************************************************************
      *    
      *     Check the length of the numeric Stock/Inventory item code 
      *     being used and allow the User to key in the code.         
      *    
           ELSE
               EVALUATE W10-SLNGTH
      *                     3 Digit numeric Stock/Inventory item code
                 WHEN 3         ACCEPT S07-3 AT 0917
      *                     4 Digit numeric Stock/Inventory item code
                 WHEN 4         ACCEPT S07-4 AT 0917
      *                     5 Digit numeric Stock/Inventory item code
                 WHEN 5         ACCEPT S07-5 AT 0917
      *                     6 Digit numeric Stock/Inventory item code
                 WHEN 6         ACCEPT S07-6 AT 0917
      *                     7 Digit numeric Stock/Inventory item code
                 WHEN 7         ACCEPT S07-7 AT 0917
      *                     8 Digit numeric Stock/Inventory item code
                 WHEN 8         ACCEPT S07-8 AT 0917
      *                     9 Digit numeric Stock/Inventory item code
                 WHEN 9         ACCEPT S07-9 AT 0917
      *                    10 Digit numeric Stock/Inventory item code
                 WHEN 10        ACCEPT S07-10 AT 0917
      *                    11 Digit numeric Stock/Inventory item code
                 WHEN 11        ACCEPT S07-11 AT 0917
      *                    12 Digit numeric Stock/Inventory item code
                 WHEN 12        ACCEPT S07-12 AT 0917
      *                    13 Digit numeric Stock/Inventory item code
                 WHEN 13        ACCEPT S07-13 AT 0917
      *                    14 Digit numeric Stock/Inventory item code
                 WHEN 14        ACCEPT S07-14 AT 0917
               END-EVALUATE.

           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  MOVE SPACES TO W10-EXT-ITEM
                               GO TO EA40A
                 WHEN F1-KEY   PERFORM HELP-ROUTINE
                 WHEN F2-KEY   MOVE "I"  TO W10-ETYPE
                 WHEN F3-KEY   MOVE "A"  TO W10-ETYPE
                 WHEN F4-KEY   MOVE "D"  TO W10-ETYPE
                 WHEN F5-KEY   MOVE "2"  TO W10-ETYPE
                 WHEN F6-KEY   MOVE "X"  TO W10-ETYPE
                 WHEN F8-KEY   PERFORM AA100
                               GO TO EA40
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO EA38
               END-EVALUATE
               MOVE "S"        TO WS-LOOK
               PERFORM AA50 THRU AA59
      *    *************************************************************
      *        *****   E X T E N D E D   C O D E   *****
      *    *************************************************************
               IF WS-EXT-STK = "Y"
                   DISPLAY S07-EXT AT 0917
               ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
               IF W10-STCK = "A"
                   IF WS-USE-ITM = "Y"
                       DISPLAY S07-ITM AT 0917
                   ELSE
                       DISPLAY S07 AT 0917.
           IF W10-EXT-ITEM = SPACES
               GO TO EA38.

       EA40A.
           IF W10-EXT-ITEM = SPACES
               SUBTRACT 1            FROM WS-PURLAST
               GO TO EA72.

       EA41.
             PERFORM CA000.
           IF WS-F-ERROR NOT = ZERO
               MOVE "Z"              TO W10-ETYPE
               PERFORM AA50 THRU AA59
      *    *************************************************************
      *            *****   E X T E N D E D  C O D E   *****
      *    *************************************************************
               IF WS-EXT-STK = "Y"
                   DISPLAY S07-EXT AT 0917
               ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
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
               GO TO EA40
           ELSE
               GO TO EA41.
            PERFORM CLEAR-L50.
      *
      *    ****    GET THE APPLICABLE VAT RATE (W05-RATE, WS-VAT-SUB)
      *
             MOVE STK-TAX            TO WS-VAT-SUB.
      *
      *    ****    DOES THE PREVIOUS RATE APPLY
      *
           IF (WS-VAT-SUB > 0 AND < 7)
               IF W21-DTE < WS-VAT-DATE(WS-VAT-SUB)
                   ADD 6             TO WS-VAT-SUB.
           IF STK-TAX NOT = ZERO
               MOVE W05-VAT(WS-VAT-SUB) TO W05-RATE
           ELSE
               MOVE ZERO             TO W05-RATE.
          IF STK-IND = 3
               MOVE ZERO             TO W15-ICOST W15-USELL
           ELSE
               MOVE STK-COST         TO W15-ICOST
               MOVE STK-SELL         TO W15-USELL.
      *
      *    ****    I S  I T   A   T E X T   I T E M
      *
           IF STK-IND = 3
               ACCEPT W15-DESCR AT 1017 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey AUTO
           ELSE
               MOVE STK-DESC         TO W15-DESCR
               DISPLAY STK-DESC AT 1017 WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Grey.
             MOVE ZERO               TO W15-IUN.
      *
      *    ****    Q U A N T I T Y
      *
       EA45.
             ACCEPT S37 AT 1117.
           IF W15-IUN = ZERO
               GO TO EA40.
      *
      *    ****    C A L C U L A T E   I N C L U S I V E   C O S T
      *
           IF (P-TAX = "I") AND (STK-TAX > ZERO)
               COMPUTE W15-ICOST ROUNDED = W15-ICOST + (W15-ICOST * W05-RTE).
      *
      *    ****    U N I T   C O S T
      *
       EA50.
             ACCEPT S38 AT 1217.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  GO TO EA45
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO EA50
               END-EVALUATE.
           IF W15-ICOST = ZERO
               GO TO EA50.
             DISPLAY S38 AT 1517.
             MOVE ZERO               TO W15-U-TXBL W15-U-NTXBL W15-U-VAT.
           IF STK-TAX NOT = ZERO
               IF P-TAX = "N"
                   MOVE W15-ICOST    TO W15-U-NTXBL W15-U-INCVAL W15-U-NETT
               ELSE
               IF P-TAX = "I"
      *
      *    ****    I N C L U S I V E   V A L U E
      *
                   MOVE W15-ICOST    TO W15-U-INCVAL
      *
      *    ****    E X C L U S I V E   V A L U E
      *
                   COMPUTE W15-U-TXBL ROUNDED = W15-U-INCVAL - ((W15-U-INCVAL / (100.00000 + W05-RATE)) * W05-RATE)
                   MOVE W15-U-TXBL   TO W15-U-NETT
      *
      *    ****    V A T   V A L U E
      *
                   SUBTRACT W15-U-TXBL FROM W15-U-INCVAL GIVING W15-U-VAT
               ELSE
      *
      *    ****    E X C L U S I V E   V A L U E
      *
                   MOVE W15-ICOST    TO W15-U-TXBL W15-U-NETT
      *
      *    ****    V A T   V A L U E
      *
                   COMPUTE W15-U-VAT ROUNDED = W15-U-TXBL * W05-RTE
      *
      *    ****    I N C L U S I V E   V A L U E
      *
                   ADD W15-U-TXBL W15-U-VAT GIVING W15-U-INCVAL
           ELSE
      *
      *    ****    N O N   T A X A B L E
      *
               MOVE W15-ICOST        TO W15-U-NTXBL W15-U-INCVAL W15-U-NETT.
      *
      *    ****    T O T A L   C O S T  ( L I N E )
      *                       (W15-COST)
      *
             MULTIPLY W15-IUN BY W15-ICOST GIVING W15-COST ROUNDED.
             DISPLAY S40 AT 1617.
      *
      *      ***** CALCULATE COST (IN/EXCLUSIVE) PRICE AFTER DISCOUNT
      *                       (W15-ICOST)
      *
             ACCEPT S35 AT 1323.
           IF W15-DISC1 = ZERO
               GO TO EA55.
             COMPUTE W25-V1 ROUNDED = (W15-COST - (W15-COST * W15-DSC1)).
             COMPUTE W15-ICOST ROUNDED = W25-V1 / W15-IUN.
             DISPLAY S38 AT 1517.
             MOVE W25-V1             TO W15-COST.
             DISPLAY S40 AT 1617.
      *
      *            2nd DISCOUNT
      *
             ACCEPT S36 AT 1423.
           IF W15-DISC2 = ZERO
               GO TO EA55.
             COMPUTE W25-V1 ROUNDED = (W15-COST - (W15-COST * W15-DSC2)).
             COMPUTE W15-ICOST ROUNDED = W25-V1 / W15-IUN.
             DISPLAY S38 AT 1517.
             MOVE W25-V1             TO W15-COST.
             DISPLAY S40 AT 1617.
      *
      *    ****    C A L C U L A T E   E X C L U S I V E   C O S T
      *                         (W15-UCOST)
      *
       EA55.
           IF (P-TAX = "I") AND (STK-TAX > ZERO)
               COMPUTE W15-UCOST ROUNDED = W15-ICOST - (W15-ICOST / (100.0000 + W05-RATE) * W05-RATE)
           ELSE
               MOVE W15-ICOST        TO W15-UCOST.

       EA55A.
             ACCEPT S32 AT 1817.
           IF W15-USELL = ZERO OR W15-USELL < W15-UCOST
               MOVE "Retail price < cost price" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO EA55A.
             MULTIPLY W15-IUN BY W15-USELL GIVING W15-SELL.
             MOVE W15-SELL           TO W25-SVAL.
             DISPLAY W25-S5V2 AT 1717 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.

       EA65.
             MOVE P-TERMS            TO W15-SET.
           IF NOT (W21-MM = W12-MONTH)
               IF W21-CY < W12-CCYY
                   ADD 12 W12-MONTH  GIVING WS-IXS4
                   SUBTRACT W21-MM   FROM WS-IXS4
               ELSE
                   SUBTRACT W21-MM FROM W12-MONTH GIVING WS-IXS4
               END-IF
               PERFORM EA71 THRU EA71E.
             ACCEPT W15-30  AT 1919 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black UPDATE AUTO.
             ACCEPT W15-60  AT 1926 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black UPDATE AUTO.
             ACCEPT W15-90  AT 1933 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black UPDATE AUTO.
             ACCEPT W15-120 AT 1941 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black UPDATE AUTO.
             ACCEPT W15-150 AT 1949 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black UPDATE AUTO.
             ACCEPT W15-180 AT 1957 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black UPDATE AUTO.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO EA40.
             MOVE ZERO               TO W15-IVAT.
           IF STK-TAX > ZERO
               IF P-TAX = "I"
      *
      *    ****    I N C L U S I V E
      *                   (W15-INCVAL)
      *
                   ADD W15-COST      TO W15-INCVAL
                   COMPUTE W15-IVAT ROUNDED = W15-COST / (100.0000 + W05-RATE) * W05-RATE
                   COMPUTE W15-TXBL ROUNDED = W15-TXBL + (W15-COST - W15-IVAT)
               ELSE
      *
      *    ****    E X C L U S I V E
      *                   (W15-TXBL)
      *
                   COMPUTE W15-IVAT ROUNDED = W15-COST * W05-RTE
                   ADD W15-COST      TO W15-TXBL.
      *
      *    ****    T O T A L   C O S T  ( D / N O T E )
      *                   (W15-TCOST)
      *
           IF NOT (WS-VAT-SUB = ZERO)
               ADD W15-TXBL          TO W15-TAXVAL(WS-VAT-SUB)
               ADD W15-IVAT          TO W15-VATVAL(WS-VAT-SUB).
             ADD W15-COST            TO W15-TCOST.
             MOVE WS-PURLAST         TO WS-PURKEY.
             INITIALIZE PUR-RECORD1.
             MOVE W15-SETT           TO PUR-TERM.
             MOVE W10-EXT-ITEM       TO PUR-EXT-STOCK.
             MOVE W15-IUN            TO PUR-IUN.
      *      MOVE W15-ICOST          TO PUR-ICOST.
             MOVE W15-UCOST          TO PUR-ICOST.
             MOVE W15-ICOST          TO PUR-INCVAL.
             MOVE W15-USELL          TO PUR-USELL.
             MOVE W15-COST           TO PUR-COST.
             MOVE W15-SELL           TO PUR-SELL.
             PERFORM WRITE-PURCH THRU WRITE-PURCH-EXIT.
      *                                DISPLAY RUNNING TOTAL.
             DISPLAY S44 AT 2266.

       EA70.
             DISPLAY S42B.

       EA70-CHK.
             MOVE "'N'ext Item or 'E'nd  [N]" TO WS-OPT-MES.
             MOVE "N"                TO WS-OPTION.
             MOVE "I"                TO WS-INSTR
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "E"
               GO TO EA72.
             ADD 1                   TO WS-PURLAST.
             GO TO EA38.

       EA71.
             MOVE W15-L5             TO W15-F5.
             MOVE ZERO               TO W15-180.
             SUBTRACT 1              FROM WS-IXS4.
           IF WS-IXS4 > ZERO
               GO TO EA71.

       EA71E.
             EXIT.

       EA72.
             PERFORM CLEAR-L50.
           IF WS-PURLAST = ZERO
               GO TO EA10.
             MOVE "P"                TO WS-OPTION.
             MOVE "'P'rocess, 'C'ancel or 'M'ore items  [P]" TO WS-OPT-MES.
             MOVE "7"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "C"
               GO TO EA10.
           IF WS-OPTION = "M"
               GO TO EA70-CHK.

       EA74.
             MOVE 1                  TO WS-S9.
             MOVE ZERO               TO W15-VAT.

       EA74-TAX.
             ADD W15-VATVAL(WS-S9) TO W15-VAT.
           IF WS-S9 < 12
               ADD 1                 TO WS-S9
               GO TO EA74-TAX.
             MOVE W15-VAT            TO WS-VAT.
      *      COMPUTE W15-VAT ROUNDED = W15-TXBL * W05-RTE.
           IF P-TAX = "E"
               ACCEPT S43 AT 2366.
           IF P-TAX = "E"
               ADD W15-TCOST W15-VAT GIVING W25-SVAL
           ELSE
               MOVE W15-TCOST        TO W25-SVAL.
             DISPLAY W25-S5V2 AT 2266 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO EA74.
             MOVE ZERO               TO WS-PURKEY.
             DISPLAY "Processing Debit Note" AT 5012 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
      *
      *    ****    Start Recovery Logging.
      *
             PERFORM AY60 THRU AY999.
      *
      *    ****    Lock Creditor record and Log to Recovery file
      *
             MOVE 0                  TO WS-ACTION.
             PERFORM AY07 THRU AY59.
      *                                PRINT DEBIT NOTE HEADING.
       EA75.
             MOVE WS-HCRED           TO DS-H4.
             MOVE "D/NOTE NO:"       TO DS-H5.
             MOVE WS-H1              TO DS-H6 DS-H8.
             MOVE "INVOICE NO:"      TO DS-H7.
             MOVE W15-ACNO           TO DS-ACNO.
             MOVE W15-NAME           TO DS-NAME.
             MOVE W15-GRNN           TO DS-DNO.
             MOVE W15-GRND           TO DS-DNDTE.
             MOVE W15-INVN           TO DS-INV.
             MOVE W15-INVD           TO DS-IDTE.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE "ITEM CODE"        TO DS-H9.
             MOVE "DESCRIPTION"      TO DS-H10.
             MOVE "UNT COST"         TO DS-H11.
             MOVE "UNT-SELL"         TO DS-H12.
             MOVE "QUANT"            TO DS-H13.
             MOVE "TOT COST"         TO DS-H14.
             MOVE "TOT SELL"         TO DS-H15.
             MOVE "30  60  90 120 150 180" TO DS-H16.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.

       EA80.
           IF WS-PURKEY NOT < WS-PURLAST
               GO TO EA125.
             ADD 1                   TO WS-PURKEY.
             PERFORM READ-PURCH THRU READ-PURCH-EXIT.
      *
      *    ****    Lock Stock record and Log to Recovery file
      *
             MOVE PUR-EXT-STOCK      TO STK-CODE.
             MOVE 0                  TO WS-ACTION.
             PERFORM AY05 THRU AY59.
             MOVE PUR-IUN            TO W15-IUN.
             MOVE PUR-INCVAL         TO W15-ICOST.
             MOVE PUR-ICOST          TO W15-UCOST.
             MOVE PUR-USELL          TO W15-USELL.
             MOVE PUR-COST           TO W15-COST.
             MOVE PUR-SELL           TO W15-SELL.
             MOVE STK-CODE           TO W10-EXT-ITEM.
             MOVE STK-DESC           TO W15-DESC.
             MOVE PUR-TERM           TO W15-SETT.
      *
      *    ****    U P D A T E   S T O C K   R E C O R D
      *
             MULTIPLY W15-IUN BY W15-UCOST GIVING W25-V2 ROUNDED.
           IF STK-IND > 1
               GO TO EA85.
             MULTIPLY STK-QUANT BY STK-AVGE GIVING W25-V1.
             SUBTRACT W15-IUN        FROM STK-QUANT.
           IF STK-QUANT > ZERO
               SUBTRACT W25-V2       FROM W25-V1
               DIVIDE W25-V1 BY STK-QUANT GIVING STK-AVGE.
             PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
             MOVE P-NUMBER           TO PRD-SUPP.
             MOVE W10-EXT-ITEM       TO PRD-EXT-CODE.
             MOVE W12-T-YMD          TO PRD-DTE.
             ADD 1                   TO PRD-SEQ.
             MOVE "D"                TO PRD-TRAN.
           IF W15-GRNN NOT = SPACES
               MOVE W15-GRNN         TO PRD-REF
           ELSE
               MOVE W15-INVN         TO PRD-REF.
             MULTIPLY -1 BY W15-IUN  GIVING PRD-QNT.
             MULTIPLY -1 BY W15-COST GIVING PRD-COST.
             MOVE W15-NAME           TO PRD-NAME.
      *      MOVE W15-WHS            TO PRD-WHSE.
             PERFORM WRITE-PURDEX THRU WRITE-PURDEX-EXIT
             MOVE 1                  TO WS-ACTION
             PERFORM AY056 THRU AY59.
      *
      *    ****    U P D A T E   D E P A R T M E N T   C O S T
      *
       EA85.
             MOVE STK-LDG            TO DPT-CODE.
             PERFORM AY38 THRU AY59.
             ADD W25-V2              TO DPT-PRET-MTD DPT-PRET-YTD DPT-PRET-DAY.
           IF WX-INT = "Y"
               MOVE W20-DTE          TO WX-DATE
               MOVE STK-LDG          TO WX-LEDG
               MOVE "RETURNS - CREDITORS" TO WX-NAR
               COMPUTE WX-VALUE = W25-V2 * -1
               MOVE DPT-PUR-GL       TO WX-AC
               MOVE 07               TO WX-TYPE
               PERFORM GL-TRF-UPDATE.
             PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
      *
      *    ****    U P D A T E   S T O C K   C O N T R O L
      *
             SUBTRACT W25-V2         FROM STK-TOTAL.
             MOVE ZERO               TO WS-S.
             MOVE 1                  TO WS-S2.
      *                                CALCULATE AGEING.
       EA90.
             MOVE ZERO               TO W25-CFLOW(WS-S2).
           IF W15-MONTH(WS-S2) > ZERO
               ADD 1                 TO WS-S.
           IF WS-S2 < 6
               ADD 1                 TO WS-S2
               GO TO EA90.
             MOVE ZERO               TO W25-V1 W25-V2.
             MOVE 1                  TO WS-S3.
           IF WS-S < 2
               MOVE W15-COST         TO W25-V2
               GO TO EA95.
             MOVE W15-COST           TO W20-RESULT.
             DIVIDE W20-RESULT BY WS-S GIVING W25-V1 ROUNDED.
             SUBTRACT 1              FROM WS-S.
             COMPUTE W25-V2 = W20-RESULT - (W25-V1 * WS-S).
             ADD 1                   TO WS-S.

       EA95.
             MULTIPLY -1             BY W25-V1.
             MULTIPLY -1             BY W25-V2.
           IF WS-S = ZERO
               GO TO EA120.

       EA100.
           IF W15-MONTH(WS-S3) = ZERO
               ADD 1                 TO WS-S3
               GO TO EA100.
           IF WS-S > 1
               MOVE W25-V1           TO W25-CFLOW(WS-S3)
               SUBTRACT 1            FROM WS-S
               ADD 1                 TO WS-S3
               GO TO EA100.
             MOVE W25-V2             TO W25-CFLOW(WS-S3).
             MOVE ZERO               TO W25-V1 W25-V2.
             MOVE 1                  TO WS-S2.

       EA105.
           IF W25-CFLOW(WS-S2) = ZERO
               GO TO EA110.
             ADD W12-MONTH WS-S2     GIVING WS-S3.
           IF WS-S3 > 12
               SUBTRACT 12           FROM WS-S3.

       EA110.
           IF WS-S2 < 6
               ADD 1                 TO WS-S2
               GO TO EA105.
           IF W25-V2 NOT = ZERO
               GO TO EA120.
             MOVE 1                  TO WS-S WS-S2.

       EA115.
           IF W15-MONTH(WS-S) NOT = ZERO
               ADD WS-S2             TO CT-AGE.
           IF WS-S < 6
               MULTIPLY 2            BY WS-S2
               ADD 1                 TO WS-S
               GO TO EA115.
      *                                ACCUMULATE TOTALS.
       EA120.
             ADD W15-SELL            TO W15-TSELL.
             ADD W25-CFLOW(1)        TO W30-30.
             ADD W25-CFLOW(2)        TO W30-60.
             ADD W25-CFLOW(3)        TO W30-90.
             ADD W25-CFLOW(4)        TO W30-120.
             ADD W25-CFLOW(5)        TO W30-150.
             ADD W25-CFLOW(6)        TO W30-180.
             ADD W25-V2              TO W30-CUR.
      *
      *      ****    P R I N T   I T E M   D E T A I L S
      *
             MOVE W10-EXT-ITEM       TO DS-ITEM.
             MOVE W15-DESC           TO DS-DESC.
             MOVE W15-ICOST          TO DS-UCOST.
             MOVE W15-USELL          TO DS-USELL.
             MOVE W15-IUN            TO DS-QNT.
             MOVE W15-COST           TO DS-COST.
             MOVE W15-SELL           TO DS-SELL.
           IF W15-MONTH(1) > ZERO
               MOVE "Y"              TO DS-30
           ELSE
               MOVE "N"              TO DS-30.
           IF W15-MONTH(2) > ZERO
               MOVE "Y"              TO DS-60
           ELSE
               MOVE "N"              TO DS-60.
           IF W15-MONTH(3) > ZERO
               MOVE "Y"              TO DS-90
           ELSE
               MOVE "N"              TO DS-90.
           IF W15-MONTH(4) > ZERO
               MOVE "Y"              TO DS-120
           ELSE
               MOVE "N"              TO DS-120.
           IF W15-MONTH(5) > ZERO
               MOVE "Y"              TO DS-150
           ELSE
               MOVE "N"              TO DS-150.
           IF W15-MONTH(6) > ZERO
               MOVE "Y"              TO DS-180
           ELSE
               MOVE "N"              TO DS-180.
           IF W02-LINAGE < (W02-PRN-LENGTH - 2)
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM EA05
               PERFORM EA75.
             MOVE SPACES             TO R-DET.
             GO TO EA80.
      *                                DEBIT NOTE TOTAL.
       EA125.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE "SUB-TOTAL"        TO DS-H13.
             MOVE W15-TCOST          TO DS-COST.
             MOVE W15-TSELL          TO DS-SELL.
           IF W02-LINAGE < W02-PRN-LENGTH
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE "VAT"              TO DS-H13.
             MOVE W15-VAT            TO DS-COST.
           IF W02-LINAGE < W02-PRN-LENGTH
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             ADD W15-TCOST           TO W15-TOTAL.
             ADD W15-VAT             TO W15-TOTVAT.
           IF P-TAX = "I"
               SUBTRACT W15-VAT      FROM W15-TOTAL.
           IF P-TAX = "E"
               ADD W15-VAT           TO W15-TCOST.
             MOVE "TOTAL"            TO DS-H13.
             MOVE W15-TCOST          TO DS-COST.
           IF W02-LINAGE < (W02-PRN-LENGTH - 10)
               MOVE 3                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
      *
      *    ****    G E N E R A T E   T R A N S A C T I O N
      *
       EA130.
             INITIALIZE CT-REC.
             MOVE W15-GRND           TO W15-DEL.
             MOVE W15-DEL-DD         TO W21-DD.
             MOVE W15-DEL-MM         TO W21-MM.
             MOVE W15-DEL-CC         TO W21-CC.
             MOVE W15-DEL-YY         TO W21-YY.
             MOVE W21-DTE            TO CT-DTE.
           IF W15-GRNN NOT = SPACES
               MOVE W15-GRNN         TO CT-REF
           ELSE
               MOVE W15-INVN         TO CT-REF.
             MOVE W15-INVN           TO CT-SINV.
             MOVE W15-INVD           TO W15-DEL.
             MOVE W15-DEL-DD         TO W21-DD.
             MOVE W15-DEL-MM         TO W21-MM.
             MOVE W15-DEL-CC         TO W21-CC.
             MOVE W15-DEL-YY         TO W21-YY.
             MOVE W21-DTE            TO CT-SDTE.
             MOVE "U"                TO CT-TYPE.
             MOVE "I"                TO CT-PJ.
             MOVE W15-ACNO           TO CT-CRED.
             MOVE SPACES             TO CT-ORD.
             MOVE ZERO               TO CT-PAID CT-SEQ.
             MULTIPLY -1 BY W15-TCOST GIVING CT-COST.
             MULTIPLY -1 BY W15-TSELL GIVING CT-SELL.
             MULTIPLY -1 BY W15-VAT  GIVING CT-TAX
             MOVE 02                 TO CT-CODE.
             MOVE ZERO               TO CT-AGE W15-SETT.
             COMPUTE CT-TAXFREE = CT-COST - (CT-TAX + (W15-TXBL * -1)).
             MOVE SPACES             TO CT-DREF.
             PERFORM CA600.
           IF WX-INT = "Y"
               MOVE W20-DTE          TO WX-DATE
               MOVE "  CR"           TO WX-LEDG
               MOVE W90-CODE         TO WX-TRN
               MOVE W00-C-DESC       TO WX-DESC
               MOVE "- CREDITORS"    TO WX-BOOK
               COMPUTE WX-VALUE = CT-COST * -1
               MOVE WX-CREGL         TO WX-AC
               MOVE 02               TO WX-TYPE
               PERFORM GL-TRF-UPDATE.
             PERFORM CA700.
           IF W30-30 NOT = ZERO
               MOVE 1                TO W15-30.
           IF W30-60 NOT = ZERO
               MOVE 1                TO W15-60.
           IF W30-90 NOT = ZERO
               MOVE 1                TO W15-90.
           IF W30-120 NOT = ZERO
               MOVE 1                TO W15-120.
           IF W30-150 NOT = ZERO
               MOVE 1                TO W15-150.
           IF W30-180 NOT = ZERO
               MOVE 1                TO W15-180.
             MOVE 1                  TO WS-S WS-S2.

       EA140.
          IF W15-MONTH(WS-S) NOT = ZERO
               ADD WS-S2             TO CT-AGE.
           IF WS-S < 6
               ADD 1                 TO WS-S
               MULTIPLY 2            BY WS-S2
               GO TO EA140.

       EA145.
             ADD CT-COST             TO P-BAL.
             ADD CT-COST             TO P-DT.
             ADD W30-180             TO P-180D.
           IF P-180D < ZERO
               SUBTRACT P-180D       FROM W30-180
               ADD P-180D            TO W30-150
               MOVE ZERO             TO P-180D.
             ADD W30-150             TO P-150D.
           IF P-150D < ZERO
               SUBTRACT P-150D       FROM W30-150
               ADD P-150D            TO W30-120
               MOVE ZERO             TO P-150D.
             ADD W30-120             TO P-120D.
           IF P-120D < ZERO
               SUBTRACT P-120D       FROM W30-120
               ADD P-120D            TO W30-90
               MOVE ZERO             TO P-120D.
             ADD W30-90              TO P-90D.
           IF P-90D < ZERO
               SUBTRACT P-90D        FROM W30-90
               ADD P-90D             TO W30-60
               MOVE ZERO             TO P-90D.
             ADD W30-60              TO P-60D.
           IF P-60D < ZERO
               SUBTRACT P-60D        FROM W30-60
               ADD P-60D             TO W30-30
               MOVE ZERO             TO P-60D.
             ADD W30-30              TO P-30D.
           IF P-30D < ZERO
               SUBTRACT P-30D        FROM W30-30
               ADD P-30D             TO W30-CUR
               MOVE ZERO             TO P-30D.
             ADD W30-CUR             TO P-OUT.
             PERFORM WRITE-CRTRAN THRU WRITE-CRTRAN-EXIT.
             MOVE 1                  TO WS-ACTION.
             PERFORM AY08 THRU AY59.
             PERFORM REWRITE-CREDIT THRU WRITE-CREDIT-EXIT.
      *
      *    ****    U P D A T E   C R E D I T O R   C O N T R O L
      *
             MULTIPLY -1             BY W15-TCOST.
             ADD W15-TCOST           TO W70-CLOSE.
             ADD W15-TCOST           TO W70-DEBT.
             ADD W30-180             TO W70-DUE-180.
             ADD W30-150             TO W70-DUE-150.
             ADD W30-120             TO W70-DUE-120.
             ADD W30-90              TO W70-DUE-90.
             ADD W30-60              TO W70-DUE-60.
             ADD W30-30              TO W70-DUE-30.
             ADD W30-CUR             TO W70-DUE-NOW.
      *
      *    ****    VAT Control
      *
             MOVE 1                  TO W05-V-RATE.

       EA146.
             ADD 6 W05-V-RATE  GIVING WS-VAT-SUB.
           IF (W15-VATVAL(W05-V-RATE) = ZERO) AND (W15-VATVAL(WS-VAT-SUB) = ZERO)
               GO TO EA147.
             MOVE W05-VAT-CODE       TO DPT-CODE.
             MOVE 0                  TO WS-ACTION.
             PERFORM AY38 THRU AY59.
             ADD W15-VATVAL(W05-V-RATE) TO DPT-I-VRET-DAY DPT-I-VRET-MTD DPT-I-VRET-YTD.
             ADD W15-VATVAL(WS-VAT-SUB) TO DPT-I-VRET-DAY DPT-I-VRET-MTD DPT-I-VRET-YTD.
             ADD W15-TAXVAL(W05-V-RATE) TO DPT-V-PRET-DAY DPT-V-PRET-MTD DPT-V-PRET-YTD.
             ADD W15-TAXVAL(WS-VAT-SUB) TO DPT-V-PRET-DAY DPT-V-PRET-MTD DPT-V-PRET-YTD.
             PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.

       EA147.
           IF W05-V-RATE < 6
               ADD 1                 TO W05-V-RATE
               GO TO EA146.
           IF (WX-INT = "Y") AND (W15-VAT NOT = ZERO)
               COMPUTE WX-VALUE = W15-VAT * -1
               MOVE DPT-I-VAT-GL     TO WX-AC
               MOVE 12               TO WX-TYPE
               PERFORM GL-TRF-UPDATE.
      *
      *    ****    End Recovery Logging.
      *
       EA148.
             CLOSE CREDIT.
             OPEN I-O CREDIT.
             PERFORM AY70 THRU AY999.
             CLOSE PURCH.
             OPEN OUTPUT PURCH.
             CLOSE PURCH.
             OPEN I-O PURCH.
             GO TO EA10.

       EA999.
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
             MOVE PAR-CASES          TO WS-USE-CASES.
             MOVE PAR-PACKS          TO WS-USE-PACKS.
             MOVE PAR-USE-ITM        TO WS-USE-ITM.
             MOVE PAR-EXT-STK        TO WS-EXT-STK.
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
             MOVE 5              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE 1              TO W05-V-RATE.

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
               WHEN 18         MOVE W02-RECOVER TO WS-FILE
                               MOVE WS-RECKEY   TO WS-KEY
               WHEN 22         MOVE W02-STOCKF  TO WS-FILE
                               MOVE STK-CODE    TO WS-KEYX
               WHEN 32         MOVE W02-TXTRAN  TO WS-FILE
                               MOVE TAX-KEY     TO WS-KEYX
               WHEN 35         MOVE W02-CRETRN  TO WS-FILE
                               MOVE CRT-KEY  TO WS-KEYX
               WHEN 37         MOVE W02-SHARED  TO WS-FILE
                               MOVE WS-SHARED   TO WS-KEY
               WHEN 40         MOVE W02-LEDTRF  TO WS-FILE
                               MOVE XFR-KEY     TO WS-KEYX
               WHEN 56         MOVE W02-PURDEX  TO WS-FILE
                               MOVE PRD-KEY     TO WS-KEYX
               WHEN OTHER      MOVE "ERROR"     TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
