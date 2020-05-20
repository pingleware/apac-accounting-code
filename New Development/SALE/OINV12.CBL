      $set LINKCOUNT"772" GNT"OINV12.GNT"
      ******************************************************************
      *                                                                *
      *    ******   ********  **     **  **    **     **     ******    *
      *   **    **     **     ***    **  **    **    ***    **    **   *
      *   **    **     **     ** *   **  **    **     **         **    *
      *   **    **     **     **  *  **  **    **     **       **      *
      *   **    **     **     **   * **   **  **      **     **        *
      *   **    **     **     **    ***    ****       **    **         *
      *    ******   ********  **     **     **      ******  ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     P R I N T   I N V O I C E   -   ( C O N S O L )            *
      *                                                                *
      *       Version 9.04.05 - June 2018                              *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  Jun 2011 - Include user number and name on the invoice        *
      *             header record to enable lookup of person           *
      *             who captured the Invoice/Credit note.              *
      *                                                                *
      *  Nov 2012 - Changed DEB-ADDRESS and DEB-PADDR from group       *
      *             fields with 70 characters to group fields          *
      *             containing DEB-ADD-L1/2/3/4 and DEB-PADD-L1/2/3/4  *
      *             respectively. Each of these new fields are defined *
      *             as PIC X(30). As new PC's and Servers contain huge *
      *             amounts of disk space this has been changed, which *
      *             also results in simplifying the code for handling  *
      *             addresses in Data capture, enquiries, reports etc. *
      *             This also removes the restriction on the number of *
      *             characters that an address may contain.            *
      *                                                                *
      *  Jan 2013 - Program DTPMEMO introduced to handle memo lookups  *
      *             reducing code in all programs and removing file    *
      *             DEBMEM from programs. Copy file 'DEBMEM.PRO' now   *
      *             copied into all programs that require lookups for  *
      *             memo/remarks on a Debtor account. The Debtor code/ *
      *             account number is moved to W80-ACCOUNT and then    *
      *             PERFORM DEBTOR-MEMO.                               *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     OINV12.
       AUTHOR.         J.W.LEMMON (APAC).
       DATE-WRITTEN.   AUGUST 1991.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1991 - 2018
                                  by James William Lemmon.
                                     (Id No. 4412165050082).
	            
                All rights reserved.
	            
                e-mail jwlemmon@gmail.com.

       SECURITY.
                This program is free software; you can redistribute
                it and/or modify it under the terms of the GNU General
                Public License as published by the Free Software
                Foundation; either version 2 of the License, or (at
                your option) any later version.

                This program is distributed in the hope that it will
                be useful, but WITHOUT ANY WARRANTY; without even the
                implied warranty of MERCHANTABILITY or FITNESS FOR A
                PARTICULAR PURPOSE. See the GNU General Public License
                for more details.

                You should have received a copy of the GNU General
                Public License along with this program; if not, write
                to the Free Software Foundation, Inc., 59 Temple Place
                - Suite 330, Boston, MA 02111-1307, USA.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                       CURSOR IS CSTART
                       CONSOLE IS CRT.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY "AUDIT.SL".

       COPY "CARDEX.SL".

       COPY "CONTROL.SL".

       COPY "DBTRAN.SL".

       COPY "DEBTOR.SL".

       COPY "DEPART.SL".

       COPY "GARTEE.SL".

       COPY "INVOIC.SL".

       COPY "LEDTRF.SL".

       COPY "PARAM.SL".

       COPY "RECOVER.SL".

       COPY "SALE.SL".

       COPY "SHARED.SL".

       COPY "SORDER.SL".

       COPY "STOCK.SL".

       COPY "TXTRAN.SL".

           SELECT CSHDRW  ASSIGN DISK
                          ORGANIZATION SEQUENTIAL
                          ACCESS SEQUENTIAL.

      /
       DATA DIVISION.
       FILE SECTION.

       COPY "AUDIT.FDE".

       COPY "CARDEX.FDE".

       COPY "CONTROL.FDE".

       COPY "DBTRAN.FDE".

       COPY "DEBTOR.FDE".

       COPY "DEPART.FDE".

       COPY "GARTEE.FDE".

       COPY "INVOIC.FDE".

       COPY "LEDTRF.FDE".

       COPY "PARAM.FDE".

       COPY "RECOVER.FD".

       COPY "SALE.FD".

       COPY "SHARED.FDE".

       COPY "SORDER.FDE".

       COPY "STOCK.FDE".

       COPY "TXTRAN.FDE".

      *
      *    O L D   S T Y L E   C A S H   D R A W E R
      *
       FD  CSHDRW    LABEL RECORD STANDARD
                     VALUE OF FILE-ID "COM1".
       01  CSH-REC.
           03  CSH-OPEN     PIC  X(06).

      /
       WORKING-STORAGE SECTION.
       77  WS-CHECK                PIC  X(18)            VALUE "aeWlimemnomLalismJ".
       77  WS-DISC                 PIC  99V99    COMP-3.
       77  WS-TDISC                PIC  99V99    COMP-3.
       77  WS-CASH-AC              PIC  X(06).
       77  WS-SUB                  PIC  9(04)    COMP-5.
       77  WS-SUPER                PIC  9(02)    COMP-5.
       77  WS-P1                   PIC  9(04)    COMP-5.
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S5                   PIC  9(04)    COMP-5.
       77  WS-S6                   PIC  9(04)    COMP-5.
       77  WS-S7                   PIC  9(04)    COMP-5.
       77  WS-S8                   PIC  9(04)    COMP-5.
       77  WS-S9                   PIC  9(04)    COMP-5.
       77  WS-IXS                  PIC  9(04)    COMP-5.
       77  WS-IXS1                 PIC  9(04)    COMP-5.
       77  WS-IXS2                 PIC  9(04)    COMP-5.
       77  WS-IXS3                 PIC  9(04)    COMP-5.
       77  WS-IXS4                 PIC  9(04)    COMP-5.
       77  WS-PARKEY               PIC  9(04)    COMP-5.
       77  WS-KEYSTR               PIC  9(06)    COMP-5.
       77  WS-SALSTR               PIC  9(04)    COMP-5.
       77  WS-SALKEY               PIC  9(04)    COMP-5.
       77  WS-NETKEY               PIC  9(04)    COMP-5.
       77  WS-AUDKEY               PIC  9(06)    COMP-5.
       77  WS-RECKEY               PIC  9(06)    COMP-5.
       77  WS-RECOVER              PIC  9(06)    COMP-5.
       77  WS-SHARED               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-TRANS                PIC  9(06)    COMP-5 VALUE 1.
       77  WS-STOCK                PIC  X(01).
       77  WS-CARDEX               PIC  X(01).
       77  WS-PRC-IND              PIC  X(01).
       77  WS-INV                  PIC  9(01).
           88  STD-INV                                   VALUE 1.
           88  COND-INV                                  VALUE 2.
           88  ALT-INV                                   VALUE 3.
       77  WS-GAR                  PIC  9(01)            VALUE 0.
       77  WS-PRN1                 PIC  9(01)            VALUE 0.
       77  WS-PRN2                 PIC  9(01)            VALUE 0.
       77  WS-START                PIC  9(08).
       77  WS-END                  PIC  9(08).
       77  WS-TME2                 PIC  9(08).
       77  WS-PORT                 PIC  9(05)            VALUE 259.
      * 1004.
       77  WS-DRAW1                PIC  9(05)            VALUE 260.
       77  WS-OPEN                 PIC  X(01)            VALUE X"01".
       77  WS-CLOSE                PIC  X(01)            VALUE X"00".
       77  WS-PAR                  PIC  9(01)            VALUE 0.
       77  WS-INO                  PIC  9(02)            VALUE 0.
       77  WS-LINES                PIC  9(02)            VALUE 0.
       77  WS-FIRST                PIC  9(03)            VALUE 0.
       77  WS-LAST                 PIC  9(03)            VALUE 0.
       77  WS-LASTINV              PIC  9(04)    COMP-5.
       77  WS-DET-LINES            PIC  9(04)    COMP-5 VALUE 31.
       77  WS-LINE                 PIC  9(04)    COMP-5.
       77  WS-PAGE                 PIC  9(02)    COMP-5.
       77  WS-L1                   PIC  9(04)    COMP-5.
       77  WS-PGE-LENGTH           PIC  9(02)            VALUE 66.
       77  WS-PRN-LENGTH           PIC  9(02)            VALUE 62.
       77  WS-PRN-TYPE             PIC  X(01)            VALUE " ".
       77  WS-OPTION               PIC  X(01).
       77  WS-SINGLE               PIC  X(01)            VALUE "S".
       77  WS-SKIP                 PIC  X(01)            VALUE " ".
       77  WS-LOOK                 PIC  X(01)            VALUE "S".
       77  WS-ETYPE                PIC  X(01)            VALUE "I".
       77  WS-ERROR                PIC  9(01).
       77  WS-CHAR1                PIC  X(01).
       77  WS-CHAR2                PIC  X(01).
       77  WS-COMP                 PIC  9(01).
       77  WS-UPDATE               PIC  9(01)            VALUE ZERO.
       77  WS-CONREC               PIC  9(01)            VALUE ZERO.
       77  WS-HEAD                 PIC  X(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-AGE                  PIC  X(01).
       77  WS-PRC                  PIC  X(01).
       77  WS-NOSELL               PIC  X(01)            VALUE "N".
       77  WS-BATCH                PIC  9(05).
       77  WS-A                    PIC  X(19)            VALUE "Any key to continue".
       77  WS-E                    PIC  X(15)            VALUE "**END** - ENTER".
       77  WS-ERR1                 PIC  X(22)            VALUE "Invalid Account Number".
       77  WS-ERR2                 PIC  X(09)            VALUE "No Record".
       77  WS-PER                  PIC  Z(02)9.99.
       77  WS-CR                   PIC  X(01)            VALUE "I".
       77  WS-TAXRES               PIC S9(07)V9(08) COMP-3.
       77  TODAY-DDMMYY            PIC  9(06)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.
       77  WS-DRAW                 PIC  9(02).
      *    ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
      *    º       V A T   C A L C U L A T I O N   P R O B L E M       º
      *    ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Value Added Tax (VAT) is calculated per item and because  ³
      *    ³ some rates cause the calculation of tax to have more than ³
      *    ³ two decimals, results must be rounded per tax percentage. ³
      *    ³ Some Users require more detailed analysis of their sales  ³
      *    ³ and split their Inventory accross more than one category. ³
      *    ³                                                           ³
      *    ³ As the integration of sales into the General Ledger from  ³
      *    ³ the sales modules is processed per Sales Ledger, which    ³
      *    ³ requires the tax to be rounded per integration entry and  ³
      *    ³ rounding per sales integration entry which on occasion    ³
      *    ³ will result in the entries for the transaction to be out  ³
      *    ³ of balance.                                               ³
      *    ³                                                           ³
      *    ³ To minimise this problem the system stores the exclusive  ³
      *    ³ price with three decimals. This only serves to minimise   ³
      *    ³ the amount by which the calculation may vary, which when  ³
      *    ³ incorrect is normally 0.01 too much or too little.        ³
      *    ³                                                           ³
      *    ³ EXAMPLE:                                                  ³
      *    ³   VAT rate = 14%.                                         ³
      *    ³   Item one has an exclusive price of:              14.25  ³
      *    ³                   sales category    :              0001   ³
      *    ³   This would result in the inclusive price being calcula- ³
      *    ³   ed as:          14.25 + VAT 1.995 =              16.245 ³
      *    ³   As inclusive prices may only have two decimals the      ³
      *    ³   system will round this price to 16.25, when the item is ³
      *    ³   first loaded or if this was the last price, resulting   ³
      *    ³   in an exclusive price of                         14.254 ³
      *    ³   Item two has an inclusive price of:             151.90  ³
      *    ³                   sales category    :              0002   ³
      *    ³   This item will have an exclusive price recorded as      ³
      *    ³   133.246 after being rounded to three decimals.          ³
      *    ³                                                           ³
      *    ³   1. Invoice for 1 of item one.                           ³
      *    ³      Using the inclusive price, calculate the VAT and     ³
      *    ³      the other values as follows:                         ³
      *    ³      Inclusive price ö (100 + VAT Rate) x VAT rate        ³
      *    ³                16.25 ö (100 + 14) x 14  = 1.9956          ³
      *    ³      Round VAT to 2 decimals            =  2.00           ³
      *    ³      Subtract VAT from inclusive price  = 14.25 (excl)    ³
      *    ³                                              Dt     Cr    ³
      *    ³      Integration entries are:  Debtor Cont  16.25         ³
      *    ³                                Income              14.25- ³
      *    ³                                VAT                  2.00- ³
      *    ³                                            ______________ ³
      *    ³                                Totals       16.25  16.25- ³
      *    ³                                                           ³
      *    ³   2. Invoice for 1 of item one and 1 of item two.         ³
      *    ³      VAT is calculated as the total on both items and     ³
      *    ³      rounded to two decimals: This can be done, by using  ³
      *    ³      the total inclusive or excluse value.                ³
      *    ³      Exclusive: 14.254 + 133.246 = 147.50 x 14% = 20.65   ³
      *    ³      Inclusive: 16.25 + 151.90 = 168.15 ö (100 + 14)      ³
      *    ³                 x 14 = 20.65.                             ³
      *    ³      As the two items are from different catergories, two ³
      *    ³      integration entries are required.                    ³
      *    ³      These entries must be rounded to two decimals.       ³
      *    ³                 14.254 rounded =  14.25                   ³
      *    ³                133.246 rounded = 133.25                   ³
      *    ³                                              Dt     Cr    ³
      *    ³      Integration entries are:  Debtor Cont 168.15         ³
      *    ³                                Income 1            14.25- ³
      *    ³                                Income 2           133.25- ³
      *    ³                                VAT                 20.65- ³
      *    ³                                            ______________ ³
      *    ³                                Totals      168.15 168.15- ³
      *    ³                                                           ³
      *    ³      From these examples, it is clear that the procedure  ³
      *    ³      being used is correct. However, if both items had    ³
      *    ³      either the first or second price the result would be ³
      *    ³      as follows:                                          ³
      *    ³      1 - price 16.25 inclusive - VAT calculated as 3.99   ³
      *    ³      Both items would have their integration entries      ³
      *    ³      rounded to 14.25.                                    ³
      *    ³                                              Dt     Cr    ³
      *    ³      Integration entries are:  Debtor Cont  32.50         ³
      *    ³                                Income 1            14.25- ³
      *    ³                                Income 2            14.25- ³
      *    ³                                VAT                  3.99- ³
      *    ³                                            ______________ ³
      *    ³                                Totals       32.50  32.49- ³
      *    ³                                                           ³
      *    ³      2 - price 151.90 inclusive - VAT calculated as 37.31 ³
      *    ³      Both items would have their integration entries      ³
      *    ³      rounded to 133.25.                                   ³
      *    ³                                              Dt     Cr    ³
      *    ³      Integration entries are:  Debtor Cont 303.80         ³
      *    ³                                Income 1           133.25- ³
      *    ³                                Income 2           133.25- ³
      *    ³                                VAT                 37.31- ³
      *    ³                                            ______________ ³
      *    ³                                Totals      303.80 303.81- ³
      *    ³                                                           ³
      *    ³ The system can either, if required, process a balancing   ³
      *    ³ entry to a suspense account at the end of each day or     ³
      *    ³ balance each set of integration entries. The suspense     ³
      *    ³ account would need to have manual adjustment entries pro- ³
      *    ³ cessed and as the system already processes balancing      ³
      *    ³ entries if there has been a problem, it would not be a    ³
      *    ³ straight-forward procedure.                               ³
      *    ³                                                           ³
      *    ³ From the above examples, it would seem that when the cre- ³
      *    ³ dit side is more, then the income should be reduced and   ³
      *    ³ when the debit side is more, the income should be         ³
      *    ³ increased.                                                ³
      *    ³                                                           ³
      *    ³ The system will have to choose one of the income entries  ³
      *    ³ randomly when income needs to be adjusted. This will need ³
      *    ³ the details of the first sales (income) integration entry ³
      *    ³ to be stored and add sales (income) values of subsequent  ³
      *    ³ sales entries and VAT (tax) values to this value and on   ³
      *    ³ completing the transaction this balancing value will be   ³
      *    ³ checked and if not zero it will be used as the balancing  ³
      *    ³ entry by changing the sign.                               ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           77  WS-BAL-ENTRY        PIC  X(01)            VALUE "N".
               88  BALANCING-ENTRY-STORED                VALUE "Y".

       01  WXT-LEDTRF.
           03  WXT-DATE            PIC  9(08)    COMP-3.
           03  WXT-TYPE            PIC  9(02)    COMP-5.
           03  WXT-LEDG.
               05  WXT-TRN         PIC  9(02).
               05  FILLER          PIC  X(02).
           03  WXT-AC              PIC  9(06)    COMP-3.
           03  WXT-NAR.
               05  WXT-DESC        PIC  X(13).
               05  WXT-BOOK        PIC  X(27).
           03  WXT-VALUE           PIC S9(09)V99 COMP-3.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *          C = Clear comment line (50)
      *          E = Clear lines (any line or lines between 5 and 46)
      *          H = Change heading
      *          S = Clear the screen and display headings
      *
           03  CRT-TYPE              PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START             PIC  9(02) VALUE 5.
           03  CRT-END               PIC  9(02) VALUE 46.
      *
      *    Calling Program
      *
           03  CRT-PROGRAM           PIC  X(15) VALUE "OINV12".
      *
      *    Screen Heading
      *
           03  CRT-HEADING           PIC  X(40) VALUE "ACCOUNTS RECEIVABLE - ORDER INVOICING".
      *
      *    Company Name
      *
           03  CRT-COMPANY           PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-TERMINAL.
               05  CRT-WRKHD         PIC  X(11).
               05  CRT-WRKST         PIC  X(03).

       COPY "WS.WS".

       COPY "LEDTRF.WS".

       01  W00-TRAN-CODES.
           03  W00-T-VAT             PIC S9(09)V99 COMP-3.
           03  W00-E-DESC            PIC  X(12).
           03  W00-A-DESC            PIC  X(12).
           03  W00-ACTION            PIC  9(01).
           03  W00-T-CODE            PIC  9(02)    COMP-5.
           03  W00-T-GL              PIC  9(06)    COMP-3.
           03  W00-T-DAY             PIC  9(06)    COMP-5.
           03  W00-T-VAL             PIC S9(09)V99 COMP-3.
           03  W00-T-MTD             PIC  9(06)    COMP-5.
           03  W00-T-MTDV            PIC S9(09)V99 COMP-3.
           03  W00-T-YTD             PIC  9(06)    COMP-5.
           03  W00-T-YTDV            PIC S9(09)V99 COMP-3.

       01  WS-PARID.
           03  WS-SYS-ID             PIC  X(03).

       01  W02-FILE-IDS.
           03  W02-PRINTER.
               05  W02-REPORT PIC X(07) VALUE "OINV12.".
               05  W02-USER   PIC X(05) VALUE SPACES.

       01  W02-PRINTER-DETAILS.
           03  W02-PRINTER         PIC  X(12).
           03  W02-PGE-LENGTH      PIC  9(02).
           03  W02-PRN-LENGTH      PIC  9(02).
           03  W02-LINAGE          PIC  9(02).
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

           03  REP-LINE1                         REDEFINES W02-PRN-LINE.
               05  REP-DETAIL1     PIC  X(132).

           03  REP-L1                            REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(10).
               05  BAL-REP         PIC  X(60).
               05  FILLER          PIC  X(10).

           03  REP-LINE2                         REDEFINES W02-PRN-LINE.
               05  REP-DETAIL2     PIC X(132).

           03  INV-L1                            REDEFINES W02-PRN-LINE.
               05  INV-EXP         PIC  X(01).
               05  FILLER          PIC  X(03).
               05  INV-EXPNAME     PIC  X(40).
               05  FILLER          PIC  X(88).

           03  INV-L2                            REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(06).
               05  INV-NADD        PIC  X(44).
               05  INV-RADD                      REDEFINES INV-NADD.
                   07  INV-PC      PIC  X(04).
                   07  FILLER      PIC  X(40).
               05  FILLER          PIC  X(40).
               05  INV-H2          PIC  X(16).
               05  INV-TEL         PIC  X(14).
               05  INV-GNO                       REDEFINES INV-TEL PIC X(14).
               05  FILLER          PIC  X(12).

           03  INV-L3                            REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(06).
               05  INV-NAME        PIC  X(40).
               05  INV-DADD                      REDEFINES INV-NAME.
                   07  INV-VATH.
                       09  INV-PC1 PIC  X(04).
                       09  FILLER  PIC  X(03).
                   07  INV-VATNO.
                       09  FILLER  PIC  X(33).
               05  FILLER          PIC  X(06).
               05  INV-H3          PIC  X(08).
               05  INV-DADD2.
                   07  INV-PC2     PIC  X(30).
               05  FILLER          PIC  X(10).
               05  INV-H4          PIC  X(19).
               05  INV-ORD.
                   07  INV-TERMS.
                       09  FILLER  PIC  X(05).
                       09  INV-AC.
                           11  FILLER  PIC  X(03).
                           11  INV-PGE PIC ZZ9.
                   07  FILLER          PIC  X(01).
               05  FILLER          PIC  X(01).

           03  INV-L4                            REDEFINES W02-PRN-LINE.
               05  INV-H5          PIC  X(21).
               05  INV-H6          PIC  X(32).
               05  INV-H7          PIC  X(14).
               05  INV-H8          PIC  X(13).
               05  INV-H9          PIC  X(11).
               05  INV-H10         PIC  X(15).
               05  INV-H11         PIC  X(14).
               05  INV-H12         PIC  X(12).

           03  INV-L5                            REDEFINES W02-PRN-LINE.
               05  INV-ITM-MES.
                   07  INV-EXT-ITM.
                       09  INV-ITM PIC  X(14).
                       09  INV-SL  PIC  X(01).
                       09  INV-EXT PIC  X(03).
                   07  FILLER      PIC  X(03).
                   07  INV-MES.
                       09  INV-DESC PIC  X(30).
               05  INV-REMARKS                   REDEFINES INV-ITM-MES.
                   07  FILLER      PIC  X(11).
                   07  INV-RMK     PIC  X(40).
               05  FILLER          PIC  X(03).
               05  INV-QNT         PIC  Z(06)9.99-.
               05  FILLER          PIC  X(02).
               05  INV-PRICE       PIC  Z(06)9.99-.
               05  FILLER          PIC  X(03).
               05  INV-DSC         PIC Z9.99.
               05  FILLER          PIC  X(03).
               05  INV-EXTEND      PIC  Z(08)9.99-.
               05  FILLER          PIC  X(02).
               05  INV-TAX         PIC  Z(06)9.99- BLANK WHEN ZERO.
               05  FILLER          PIC  X(03).
               05  INV-VAL         PIC  Z(09)9.99-.

           03  INV-L6                            REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(09).
               05  INV-HD          PIC  X(10).
               05  INV-SERNOS.
                   07  FILLER      PIC  X(01).
                   07  INV-SERNO   PIC  X(10).
                   07  FILLER      PIC  X(25).
               05  FILLER          PIC  X(77).

           03  INV-L7                            REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(08).
               05  INV-REMKS.
                   07  FILLER      PIC  X(10).
                   07  INV-RMKS.
                       09  FILLER  PIC  X(40).
                   07  FILLER      PIC  X(13).
               05  FILLER          PIC  X(04).
               05  INV-H13         PIC  X(12).
               05  INV-LINE        PIC  X(45).

           03  INV-L8                            REDEFINES W02-PRN-LINE.
               05  INV-EXP2        PIC  X(01).
               05  FILLER          PIC  X(16).
               05  INV-INVOICE.
                   07  FILLER      PIC  X(03).
                   07  INV-CRNOTE.
                       09  FILLER  PIC  X(06).
                       09  INV-COPY PIC  X(12).
                       09  FILLER  PIC  X(06).
                   07  FILLER      PIC  X(03).
               05  FILLER          PIC  X(02).
               05  INV-H14         PIC  X(08).
               05  INV-REF.
                   07  INV-DTE     PIC Z9/99/9999.

       COPY "W05.VAT".

       01  W08-EXPD.
           03  W08-CHAR            PIC  X(06).
           03  W08-CHARO                         REDEFINES W08-CHAR.
               05  W08-DEC         PIC  X(01)    COMP-X OCCURS 6.

       COPY "W10.SAL".

       01  W10-REF.
           03  W10-NUM         PIC  9(08).

       COPY "W12.WS".

       01  W15-DISCOUNT.
           03  W15-DISC        PIC S9(03)V99 COMP-3 OCCURS 10.

       COPY "W20.WS".

       01  W25-CALCS.
           03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
               05  W25-WHOLE   PIC 9(03).
               05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
           03  W25-TOTAL       PIC 9(07)V99  COMP-3.
           03  W25-VALUE       PIC S9(07)V99 COMP-3.
           03  W25-CREDIT      PIC S9(07)V99 COMP-3.

       01  W30-MESSAGE.
           03  W30-PERC        PIC Z9.99.
           03  FILLER          PIC  X(40) VALUE
               "% Settlement discount if paid in 30 DAYS".

       01  W30-MESSAGE-2.
    03  W30-RMKS15 OCCURS 6
          PIC  X(63).

       01  W32-MESSAGE.
    03  W32-REMARKS OCCURS 10
          PIC  X(63).
       01  W34-HEADER.
    03  W34-FORMAT      PIC  X(01).
    03  W34-MARGIN      PIC  9(02) COMP-5.
    03  W34-SALUTATION  PIC  X(63).

       COPY "W40.DBT".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W60-NME-ADD.
           03  W60-NAME.
               05  W60-NCHAR OCCURS 40 PIC X(01).
           03  W60-ADDRESS.
               05  W60-ACHAR OCCURS 60 PIC X(01).
           03  W60-ADDRESS1    PIC X(60).

       COPY "W70DEBT.IWS".

       01  W75-TITLES.
           03  W75-DESCRIP.
               05  FILLER      PIC X(28)      VALUE
                           "Mr     Mnr    Mrs    Mev    ".
               05  FILLER      PIC X(28)      VALUE
                           "Miss   Mej    Ms     Ms     ".
               05  FILLER      PIC X(28)      VALUE
                           "Dr     Dr     Rev    Ds     ".
               05  FILLER      PIC X(28)      VALUE
                           "Prof   Prof   The HonSy Edel".
               05  FILLER      PIC X(28)      VALUE
                           "                            ".
           03  W75-CODES REDEFINES W75-DESCRIP.
               05  W75-TITLE  OCCURS 10.
                   07  W75-ETITLE PIC X(07).
                   07  W75-ATITLE PIC X(07).

       01  W80-ACCOUNT.
           03  W80-ACNO        PIC X(06).
           03  W80-CRED REDEFINES W80-ACNO.
               05  W80-CNO     PIC 9(05).
               05  W80-CAC REDEFINES W80-CNO.
                   07  W80-CTYPE PIC 9(01).
                   07  W80-SER   PIC 9(04).
               05  FILLER        PIC 9(01).
           03  W80-TYPE        PIC 9(01).

       COPY W90.SAL.

       01  W95-STM.
           03  W95-COMP        PIC X(40).
           03  W95-ADD1        PIC X(30).
           03  W95-ADD2        PIC X(30).
           03  W95-ADD3        PIC X(30).
           03  W95-ADD4        PIC X(30).
           03  W95-PC.
               05  W95-POST    PIC 9(04).
           03  W95-TEL         PIC X(14).
           03  W95-V1.
               05  W95-T       PIC Z(04)9.99.
           03  W95-V2.
               05  W95-B       PIC Z(06)9.99-.
           03  W95-D.
              05  W95-DTE     PIC Z9/99/9999.

       01  W100-EDIT.
           03  W100-LBAL       PIC Z(06)9.99-.
           03  W100-LARR       PIC Z(06)9.99.
           03  W100-BAL        PIC Z(08)9.99-.
           03  W100-CUR        PIC Z(08)9.99-.
           03  W100-30         PIC Z(08)9.99-.
           03  W100-60         PIC Z(08)9.99-.
           03  W100-90         PIC Z(08)9.99-.
           03  W100-120        PIC Z(08)9.99-.
           03  W100-INT        PIC Z(08)9.99-.
           03  W100-CRL        PIC Z(06)9.
           03  W100-SER        PIC ZZ9.
           03  W100-V2.
               05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V4 REDEFINES W100-V2.
               05  W100-EXT    PIC Z(06)9.99.

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

       01  S02.
          03  LINE 2  COLUMN 27 FOREGROUND-COLOR Grey HIGHLIGHT
                                 VALUE "SALES/ORDER PROGRAM CONTROL".

       COPY "INVOICE.CRT".

       01  S30.
           03  LINE  8 COLUMN 15 FOREGROUND-COLOR Brown HIGHLIGHT PIC X(01) USING WS-G9.
           03          COLUMN 16 FOREGROUND-COLOR Brown HIGHLIGHT PIC X(50) USING WS-SRT-LINE.
           03          COLUMN 66 FOREGROUND-COLOR Brown HIGHLIGHT PIC X(01) USING WS-G10.
           03  LINE  9 COLUMN 15 FOREGROUND-COLOR Brown HIGHLIGHT PIC X(01) USING WS-G3.
           03          COLUMN 34 FOREGROUND-COLOR Grey VALUE "CLIENT DETAILS".
           03          COLUMN 66 FOREGROUND-COLOR Brown HIGHLIGHT PIC X(01) USING WS-G3.
           03  LINE 10 COLUMN 15 FOREGROUND-COLOR Brown HIGHLIGHT PIC X(01) USING WS-G3.
           03          COLUMN 16 FOREGROUND-COLOR Grey VALUE "Name    >".
           03          COLUMN 65 FOREGROUND-COLOR Grey VALUE "<".
           03          COLUMN 66 FOREGROUND-COLOR Brown HIGHLIGHT PIC X(01) USING WS-G3.
           03  LINE 11 COLUMN 15 FOREGROUND-COLOR Brown HIGHLIGHT PIC X(01) USING WS-G3.
           03          COLUMN 16 FOREGROUND-COLOR Grey VALUE "Address >".
           03          COLUMN 55 FOREGROUND-COLOR Grey VALUE "<".
           03          COLUMN 66 FOREGROUND-COLOR Brown HIGHLIGHT PIC X(01) USING WS-G3.
           03  LINE 12 COLUMN 15 FOREGROUND-COLOR Brown HIGHLIGHT PIC X(01) USING WS-G3.
           03          COLUMN 24 FOREGROUND-COLOR Grey VALUE ">".
           03          COLUMN 55 FOREGROUND-COLOR Grey VALUE "<".
           03          COLUMN 66 FOREGROUND-COLOR Brown HIGHLIGHT PIC X(01) USING WS-G3.
           03  LINE 13 COLUMN 15 FOREGROUND-COLOR Brown HIGHLIGHT PIC X(01) USING WS-G3.
           03          COLUMN 24 FOREGROUND-COLOR Grey VALUE ">".
           03          COLUMN 55 FOREGROUND-COLOR Grey VALUE "<".
           03          COLUMN 66 FOREGROUND-COLOR Brown HIGHLIGHT PIC X(01) USING WS-G3.
           03  LINE 14 COLUMN 15 FOREGROUND-COLOR Brown HIGHLIGHT PIC X(01) USING WS-G3.
           03          COLUMN 24 FOREGROUND-COLOR Grey VALUE ">".
           03          COLUMN 55 FOREGROUND-COLOR Grey VALUE "<".
           03          COLUMN 66 FOREGROUND-COLOR Brown HIGHLIGHT PIC X(01) USING WS-G3.
           03  LINE 15 COLUMN 15 FOREGROUND-COLOR Brown HIGHLIGHT PIC X(01) USING WS-G3.
           03          COLUMN 24 FOREGROUND-COLOR Grey VALUE ">".
           03          COLUMN 29 FOREGROUND-COLOR Grey VALUE "<".
           03          COLUMN 66 FOREGROUND-COLOR Brown HIGHLIGHT PIC X(01) USING WS-G3.
           03  LINE 16 COLUMN 15 FOREGROUND-COLOR Brown HIGHLIGHT PIC X(01) USING WS-G3.
           03          COLUMN 16 FOREGROUND-COLOR Grey VALUE "Tel.    >".
           03          COLUMN 39 FOREGROUND-COLOR Grey VALUE "<".
          03        COLUMN 66 FOREGROUND-COLOR Brown HIGHLIGHT
                                 PIC X(01) USING WS-G3.
          03  LINE 17 COLUMN 15 FOREGROUND-COLOR Brown HIGHLIGHT
                                 PIC X(01) USING WS-G3.
           03          COLUMN 16 FOREGROUND-COLOR Grey
                                 VALUE "Id.No.  >".
           03          COLUMN 39 FOREGROUND-COLOR Grey
                                 VALUE "<".
          03        COLUMN 66 FOREGROUND-COLOR Brown HIGHLIGHT
                                 PIC X(01) USING WS-G3.
    03  LINE 18 COLUMN 15 FOREGROUND-COLOR Brown HIGHLIGHT
                                 PIC X(01) USING WS-G11.
    03        COLUMN 16 FOREGROUND-COLOR Brown HIGHLIGHT
                                 PIC X(50) USING WS-SRT-LINE.
    03        COLUMN 66 FOREGROUND-COLOR Brown HIGHLIGHT
                                 PIC X(01) USING WS-G12.
       01  S31.
    03  LINE 10 COLUMN 25 FOREGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(40) USING W40-NM1 AUTO.
    03  LINE 11 COLUMN 25 FOREGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(30) USING W40-AD1 AUTO.
    03  LINE 12 COLUMN 25 FOREGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(30) USING W40-AD2 AUTO.
    03  LINE 13 COLUMN 25 FOREGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(30) USING W40-AD3 AUTO.
    03  LINE 14 COLUMN 25 FOREGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(30) USING W40-AD4 AUTO.
    03  LINE 15 COLUMN 25 FOREGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(04) USING W40-PC AUTO.
    03  LINE 16 COLUMN 25 FOREGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(14) USING W40-HTEL AUTO.
    03  LINE 17 COLUMN 25 FOREGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(14) USING W40-ID AUTO.
       01  S36.
          03  FOREGROUND-COLOR Cyan HIGHLIGHT
      PIC X(06) USING W80-ACNO AUTO.

       COPY "ERROR.CRT".

      /
       PROCEDURE DIVISION USING LS-PARID
    LS-USER-ID
    LS0-PROGRAMS
    W40-COMPANY
    W90-TRAN.
       AA000-MAIN        SECTION.
       AA000-INIT.
            PERFORM ZA000-INIT.
            GO TO BA00.

       COPY "ERROR.SCR".

       COPY "FUNCTION.SCR".

       COPY "LOCKED.RC2".

       COPY "CLEAR.CRT".

       COPY "DEBTOR.TRN".

       COPY PRINTUTL.AS2.

      /
      *    ****    READ FILES ROUTINES
      *
       AC000              SECTION.

       COPY "AUDIT.RD".

       COPY "CONTROL.RD".

       COPY "DBTRAN.RD1".

       COPY "DEBTOR.RD".

       COPY "DEPART.RD".

       COPY "GARTEE.RD".

       COPY "INVOIC.RD".

       COPY "LEDTRF.RD".

       COPY "PARAM.RD".

       COPY "SALE.RD".

       COPY "SHARED.RD".

       COPY "SORDER.RD".

       COPY "STOCK.RD".

       COPY "TXTRAN.RD".

      /
       AD000-WRITE             SECTION.

       COPY "AUDIT.WR".

       COPY "CARDEX.WR".

       COPY "CONTROL.WR".

       COPY "DBTRAN.WR".

       COPY "DEBTOR.WR".

       COPY "DEPART.WR".

       COPY "GARTEE.WR".

       COPY "INVOIC.WR".

       COPY "LEDTRF.WR".

       COPY "PARAM.WR".

       COPY "SALE.WR".

       COPY "SHARED.WR".

       COPY "SORDER.WR".

       COPY "STOCK.WR".

       COPY "TXTRAN.WR".

      *COPY CASHDRAW.OP2.

      /
      *       ****   *****   ***    ***   *   *  ***** ****   *   *
      *       *   *  *     * *  *   *  *   *  * *   *  *   *
      *       ****   ***    *    *   *  *   *  *** **** * *
      *       *   *  *     * *  *   *   * *  * *   *  *
      *       *   *  *****   ***    ***     *  ***** *   *  *
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *   THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *        ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *        ³ ORIGINAL ACTION (REC-TYPE)  ÄÄ  RECOVERY PROCESS ³
      *        ³ 0 = RECORD CHANGED      ÄÄ     (REWRITE)    ³
      *        ³ 1 = RECORD WAS ADDED      ÄÄ     (DELETE)    ³
      *        ³ 2 = RECORD WAS DELETED      ÄÄ     (WRITE)    ³
      *        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY000        SECTION.
       AY01.
             MOVE 01             TO REC-FILE.
             MOVE WS-PARKEY      TO REC-KEY.
             MOVE ZERO           TO REC-TYPE
            PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-RECORD1    TO REC-PARAM.
             GO TO AY50.
       AY02.
             MOVE 02             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE ZERO           TO REC-TYPE
             PERFORM READ-DEBTOR-LOCK THRU READ-DEBTOR-EXIT.
             MOVE DEB-RECORD1    TO REC-DEBTOR.
             GO TO AY50.
       AY04.
             MOVE 04             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
             MOVE TRA-RECORD1    TO REC-DBTRAN.
             GO TO AY50.
       AY05.
             MOVE 05             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE ZERO           TO REC-TYPE.
            PERFORM READ-STOCK THRU READ-STOCK-EXIT.
             MOVE STK-RECORD1    TO REC-STOCKF.
             GO TO AY50.
       AY09.
             MOVE 09             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
             MOVE GUA-RECORD1    TO REC-GARTEE.
             GO TO AY50.

       AY16.
             MOVE 16             TO REC-FILE.
             MOVE WS-AUDKEY      TO REC-KEY.
      MOVE WS-ACTION  TO REC-TYPE
             MOVE AUD-REC1       TO REC-AUDITF.
             GO TO AY50.

       AY17.
             MOVE 17             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
             MOVE SOR-REC1       TO REC-SORDER.
             GO TO AY50.

       AY19.
             MOVE 19             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
             MOVE TAX-RECORD1    TO REC-TXTRAN.
             GO TO AY50.
       AY22.
             MOVE 22             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
             MOVE DOC-REC1       TO REC-INVOIC.
             GO TO AY50.
       AY37.
            MOVE 37   TO REC-FILE.
            MOVE WS-SHARED  TO REC-KEY.
            MOVE SHR-RECORD  TO REC-NETWORK.
             GO TO AY50.
       AY38.
             MOVE 38             TO REC-FILE.
            MOVE ZERO   TO REC-KEY.
      MOVE ZERO   TO REC-TYPE.
            PERFORM READ-DEPART THRU READ-DEPART-EXIT.
    IF WS-F-ERROR = 7
        MOVE "XXXX"  TO DPT-CODE
        GO TO AY38.
            MOVE DPT-RECORD  TO REC-DEPART.
             GO TO AY50.
       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.
       AY40.
             MOVE 40             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
             MOVE XFR-REC        TO REC-LEDTRF.
             GO TO AY50.
       AY43.
            MOVE 43   TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
            MOVE CRD-REC1  TO REC-CARDEX.
             GO TO AY50.
       AY49.
             MOVE 99             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE SPACES         TO REC-DETAIL.
             PERFORM AY50.
             ADD 1               TO WS-TRANS.
           IF WS-RECOVER > 95
               CLOSE RECOVER
               OPEN OUTPUT RECOVER
               CLOSE RECOVER
               OPEN I-O RECOVER
               MOVE ZERO         TO WS-RECOVER.
             GO TO AY59.
       AY50.
             ADD 1               TO WS-RECOVER.
             MOVE WS-RECOVER     TO WS-RECKEY.
             MOVE WS-TRANS       TO REC-TRAN.
             WRITE REC-RECORD.
           IF WS-STATUS NOT = "00"
               DISPLAY "Write error Recovery file - Status " AT 2212
         WITH FOREGROUND-COLOR Brown HIGHLIGHT
         WS-STATUS WITH FOREGROUND-COLOR Grey HIGHLIGHT
               STOP RUN.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
       AY59.
             EXIT.
      *
      *    ****    Start processing transaction
      *
       AY60.
          IF WS-SKIP = "Y" OR "A"
               GO TO AY999.
            MOVE 1   TO WS-COUNT.
            MOVE 5   TO WS-SHARED.
            PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      MOVE SHR-STOCK  TO WS-STOCK.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
            MOVE 4   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-USERS < 24
               MOVE 1            TO WS-SUB
        MOVE ZERO  TO WS-WAIT
               GO TO AY62.
      *
      *    ****   Q   F U L L  -  W A I T   F O R   4 S E C O N D S
      *
            DISPLAY "WAITING" AT 2551
        WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Red HIGHLIGHT.
            COMMIT.
      ACCEPT WS-STIME FROM TIME.
      MOVE 400   TO WS-WAIT.
            PERFORM LOCK-REC-LOOP.
            DISPLAY SPACE AT 2551
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.
             GO TO AY60.

       AY61.
            MOVE "DS"   TO PAR-PROG(WS-USUB).
            MOVE LS-USER  TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
            MOVE 1   TO WS-NETKEY.
             PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             PERFORM AY39 THRU AY59.
             MOVE NET-DEBTOR     TO W70-DEBT.
    IF WS-STOCK = "Y"
              MOVE 3   TO WS-SHARED
              PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT
        PERFORM AY37 THRU AY59
        MOVE SHR-RECORD  TO NET-RECORD
    ELSE
              MOVE 3   TO WS-NETKEY
              PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT
              PERFORM AY39 THRU AY59.
             GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N P R O G R E S S
      *
       AY62.
    IF NOT(PAR-PROG(WS-SUB) = SPACES OR
    PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R O R   S T O C K   F I L E S
      *    B E I N G   U P D A T E D
      *
        IF NOT(PAR-PROG(WS-SUB) = SPACES)
           IF PAR-PROG(WS-SUB) = "DB" OR "DS" OR "CS" OR "ST"
      *
      *    ****   Y E S   -   S E T   W A I T P E R I O D
      *
         GO TO AY62-WAIT
     ELSE
         ADD 1  TO WS-SUB
         GO TO AY62
     END-IF
        ELSE
      *
      *    ****   I S T H I S   P R O G R A M   I N T H E  Q
      *
        IF PAR-USR(WS-SUB) = LS-USER
      *
      *    ****   I S I T   N E X T I N   L I N E T O   P R O C E S S
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
      MOVE LS-USER  TO PAR-USR(WS-SUB).
      MOVE WS-SUB  TO PAR-USERS.
      PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
      GO TO AY62-CHECK.
      *
      *    ****   S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
      MOVE 300   TO WS-WAIT.
    IF NOT(PAR-USR(WS-SUB) = LS-USER)
        IF WS-SUB < 24
     ADD 1  TO WS-SUB
     GO TO AY62.

       AY62-CHECK.
    IF WS-WAIT > ZERO
        COMMIT
        DISPLAY "Waiting" AT 2572
   WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT BLINK
        ACCEPT WS-STIME FROM TIME
        PERFORM LOCK-REC-LOOP
        DISPLAY "Waiting" AT 2572
   WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT BLINK
        GO TO AY60.
            DISPLAY SPACE AT 2572
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.

       AY63.
            MOVE WS-SUB  TO WS-USUB.
            GO TO AY61.

       AY70.
            MOVE 4   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 1.
      *
    IF WS-STOCK = "Y"
        MOVE NET-RECORD  TO SHR-RECORD
              PERFORM REWRITE-SHARED THRU WRITE-SHARED-EXIT
    ELSE
              PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
             MOVE 1              TO WS-NETKEY.
             MOVE W70-DEBT       TO NET-DEBTOR.
            PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
             PERFORM AY49 THRU AY59.
      MOVE 1   TO WS-USUB.

       AY72.
    IF NOT(PAR-USR(WS-USUB) = LS-USER)
        ADD 1   TO WS-USUB
        GO TO AY72.

       AY75.
            MOVE SPACES  TO PAR-PROG(WS-USUB)
              PAR-USR(WS-USUB).
    IF WS-USUB < 24
        ADD 1 WS-USUB  GIVING WS-SUB
    ELSE
        GO TO AY80.
    IF NOT(PAR-PROG(WS-SUB) = SPACES)
        MOVE PAR-PROG(WS-SUB)
     TO PAR-PROG(WS-USUB)
        MOVE PAR-USR(WS-SUB)
     TO PAR-USR(WS-USUB)
        ADD 1   TO WS-USUB
        GO TO AY75.

       AY80.
            SUBTRACT 1 FROM WS-USUB
     GIVING PAR-USERS.
            PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
    IF LS0-POS NOT = 2
               CLOSE RECOVER
               OPEN I-O RECOVER.
             COMMIT.

       AY999.
             EXIT.
      /
       AZ000-END               SECTION.
       AZ000-EOJ.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
            CLOSE RECOVER
     SALE.
       AZ100.
             EXIT PROGRAM.
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
      *
       CA165-NAME        SECTION 2.
       CA165.
             UNSTRING W60-NAME DELIMITED BY "*" OR "!"
                                 INTO W40-SURNAME
                                      W40-FNAME
                                      W40-SNAME
             OVERFLOW
                 MOVE 1          TO WS-ERROR.
       CA170-EXIT.
             EXIT.
      *
       CA175-ADDRESS     SECTION 2.
       CA175.
             UNSTRING W60-ADDRESS DELIMITED BY "*"
                                 INTO W40-ADD1
                                      W40-ADD2
                                      W40-ADD3
                                      W40-ADD4
             OVERFLOW
                 MOVE 1          TO WS-ERROR.
       CA175-EXIT.
             EXIT.
      *
       CA185-ADDRESS     SECTION 2.
       CA185.
             UNSTRING W60-ADDRESS1 DELIMITED BY "*"
                                 INTO W40-PADD1
                                      W40-PADD2
                                      W40-PADD3
                                      W40-PADD4
             OVERFLOW
                 MOVE 1          TO WS-ERROR.
       CA185-EXIT.
             EXIT.
      *
      *    ****   G E T   S O R D E R   R E C O R D   U S I N G   SOR-KEY.
      *
      *           WS-ERROR:
      *
      *                    1 = NO RECORD
      *                    2 = NOT ORDER RECORD (QUOTE OR INVOICE)
      *                    3 = ORDER DETAILS INCOMPLETE
      *                    4 = INVALID ACCOUNT NUMBER
      *
       GET-SORDER           SECTION.

       GET-SORD-ORDNO.
      MOVE 1   TO WS-S7.
             MOVE SPACES         TO W40-NM1 W40-AD1 W40-AD2
                                    W40-AD3 W40-AD4 W40-PC.
             MOVE W10-SORD       TO SOR-REF W90-RF.
             MOVE 100            TO SOR-SQN.
             MOVE ZERO           TO WS-ERROR.
      PERFORM READ-SORDER THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
               MOVE 1            TO WS-ERROR
               GO TO GET-SORDER-EXIT.
           IF SOR-DOC NOT = "O"
               MOVE 2            TO WS-ERROR
               GO TO GET-SORDER-EXIT.
             MOVE 1              TO WS-LAST WS-SALKEY.
             GO TO GET-SORD-TEST-TYPE.

       GET-SORD-NEXT.
      PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
               MOVE 3            TO WS-ERROR
               GO TO GET-SORDER-EXIT.

       GET-SORD-TEST-TYPE.
      *                      H E A D E R   R E C O R D
           IF SOR-TYPE = 0
               MOVE SOR-AC       TO W80-ACNO DEB-ACNO
               PERFORM CA155-GET-DEBTOR
               IF WS-ERROR NOT = ZERO
                   MOVE 4        TO WS-ERROR
                   GO TO GET-SORDER-EXIT
               END-IF
               MOVE SOR-OREF     TO W90-REG
               MOVE SOR-NAME     TO W40-NAME W40-NM1
               MOVE SOR-PICNO    TO W90-PICK
               MOVE SOR-ORD      TO W90-ORD
               MOVE SOR-SMAN     TO W10-SMAN
               MOVE SOR-EXP      TO W90-EXP
               MOVE SOR-PRC      TO WS-PRC
               MOVE SOR-CONTACT  TO W40-CONTACT
               MOVE SOR-TEL      TO W40-TEL
           ELSE
      *                      I T E M   D E T A I L S
           IF SOR-TYPE = 1
               INITIALIZE SAL-RECORD1
               MOVE SOR-EXT-ITEM     TO SAL-EXT-ITEM
               MOVE SOR-DESC         TO SAL-DESC
               MOVE SOR-QNT          TO SAL-QNT
               MOVE SOR-COST         TO SAL-COST
               MOVE SOR-SELL         TO SAL-SELL
               MOVE SOR-VAT          TO SAL-VAT
               MOVE SOR-VAT-SUB      TO SAL-VAT-SUB
               MOVE SOR-VAL          TO SAL-VAL
               MOVE SOR-INCTAX       TO SAL-INCTAX
               MOVE SOR-NONTAX       TO SAL-NONTAX
               MOVE SOR-TAXVAL       TO SAL-TAXVAL
               MOVE SOR-DSC          TO SAL-DSC
               MOVE SOR-DISC         TO SAL-DISC
               MOVE SOR-PACK         TO SAL-PACK
               MOVE SOR-SQN          TO SAL-SQN
               IF SAL-VAT-SUB > ZERO
                   ADD SOR-TAXVAL    TO W90-TAXVAL(SAL-VAT-SUB)
                   MOVE W05-VAT(SAL-VAT-SUB) TO W05-RATE
                   COMPUTE W90-VAT(SAL-VAT-SUB) ROUNDED = (W90-TAXVAL(SAL-VAT-SUB) * W05-RTE)
                   COMPUTE SAL-VAT ROUNDED = SAL-TAXVAL * W05-RTE
               END-IF
               PERFORM GET-SORD-WRITE-SALE
           ELSE
      *                      P A R T S   D E T A I L S
           IF SOR-TYPE = 2
               INITIALIZE SAL-RECORD1
               MOVE SOR-EXT-ITEM     TO SAL-EXT-ITEM
               MOVE SOR-DESC         TO SAL-DESC
               MOVE SOR-QNT          TO SAL-QNT
               MOVE SOR-SERNO        TO SAL-SERNO
               MOVE SOR-SQN          TO SAL-SQN
               PERFORM GET-SORD-WRITE-SALE
           ELSE
      *                      R E M A R K S
           IF SOR-TYPE = 3
               INITIALIZE SAL-RECORD1
               MOVE SOR-SERIAL       TO SAL-MSER
               MOVE SOR-SQN          TO SAL-SQN
               PERFORM GET-SORD-WRITE-SALE
           ELSE
      *                      I N T E R N A L   N O T E S
           IF SOR-TYPE = 4
               GO TO GET-SORD-NEXT
           ELSE
      *                      A D D R E S S
           IF SOR-TYPE = 5
               MOVE SOR-AD1          TO W40-AD1
               MOVE SOR-AD2          TO W40-AD2
               MOVE SOR-AD3          TO W40-AD3
               MOVE SOR-AD4          TO W40-AD4
               MOVE SOR-PC           TO W40-PC
           ELSE
      *        S A L U T A T I O N
           IF SOR-TYPE = 7
               MOVE SOR-TEXT         TO W34-SALUTATION
           ELSE
      *        T E X T
           IF SOR-TYPE = 8
               MOVE SOR-TEXT         TO W32-REMARKS(WS-S7)
               ADD 1                 TO WS-S7
           ELSE
      *                      T R A I L E R
               MOVE SOR-SUBTOT       TO W90-DB
               MULTIPLY -1 BY SOR-DSCNT GIVING W90-TDISC
               MOVE W90-TDISC        TO W90-LDISC
               MOVE SOR-NON-TXBL     TO W90-NONTAX
               MOVE SOR-TAXABLE      TO W90-EXCTOT
               MOVE SOR-TAX          TO W90-TTAX
               MOVE SOR-TOT          TO W90-TOTAL
               MOVE SOR-BAL          TO W90-BAL
               MOVE SOR-INSTRUCT     TO W90-INSTRUCT
               MOVE SOR-PMETH        TO W90-PMETH
               MOVE SOR-STATUS       TO W40-STATUS
               MOVE SOR-HTEL         TO W40-HTEL
               MOVE SOR-ID           TO W40-ID
               GO TO GET-SORDER-EXIT.
             GO TO GET-SORD-NEXT.

       GET-SORD-WRITE-SALE.
             MOVE WS-LAST        TO SAL-CODE.
             PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
             ADD 1               TO WS-LAST WS-SALKEY.

       GET-SORDER-EXIT.
             EXIT.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    GL-TRF-UPDATE          ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ GENERAL LEDGER:            ³
      *    ³ This routine is used to UPDATE the INTEGRATION RECORDS    ³
      *    ³ for posting to the G/L ACCOUNTS.          ³
      *    ³              ³
      *    ³ THE KEY FOR EACH OF THE RECORDS IS:         ³
      *    ³              ³
      *    ³ RUN DATE (CCYYMMDD)  - WX-YMD          ³
      *    ³              ³
      *    ³ TYPE - WX-TYPE            ³
      *    ³  01 : DEBTORS           ³
      *    ³  02 : CREDITORS           ³
      *    ³  03 : BAD DEBTS           ³
      *    ³  04 : SALES           ³
      *    ³  05 : SALES (RETURNS)          ³
      *    ³  06 : PURCHASES           ³
      *    ³  07 : PURCHASES (RETURNS)         ³
      *    ³  08 : COST-OF-SALES          ³
      *    ³  09 : COST-OF-SALES (RETURNS)         ³
      *    ³  10 : OUTPUT VAT (DEBTORS)         ³
      *    ³  11 : INPUT VAT (DEBTORS)         ³
      *    ³  12 : OUTPUT VAT (CREDITORS)         ³
      *    ³  13 : INPUT VAT (CREDITORS)         ³
      *    ³  14 : DEBTOR TRANSACTIONS         ³
      *    ³              ³
      *    ³ TRANSACTION CODE / SALES LEDGER NO - WX-LEDG        ³
      *    ³              ³
      *    ³ OTHER DETAILS ARE:            ³
      *    ³              ³
      *    ³ ACCOUNT NUMBER -  WX-AC           ³
      *    ³              ³
      *    ³ NARRATIVE - WX-NAR           ³
      *    ³              ³
      *    ³ TRANSACTION VALUE - WX-VAL          ³
      *    ³              ³
      *    ³ The routine will first read the file using the DETAILS    ³
      *    ³ ABOVE as the KEY. If the record exists the routine WILL   ³
      *    ³ UPDATE the record. If no record exists a NEW record WILL  ³
      *    ³ be CREATED.            ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "LEDTRF.UPD".

      /
      *    ****    G E N E R A T E   A   D E B T O R
      *            T R A N S A C T I O N
      *
       GENERATE-DEBTOR-TRAN SECTION 2.
      
       DEBTOR-TRAN.
      
       LINK-DEBTOR-TRANS.
             INITIALIZE TRA-RECORD1.
             MOVE W90-DISC       TO TRA-DISC.
             MOVE W20-DTE        TO TRA-DATE.
             MOVE W20-DTE        TO DEB-ACTIVE.
             MOVE W90-CODE       TO TRA-CODE.
             MOVE W90-REF        TO TRA-REF.
             MOVE W90-REG        TO TRA-REG.
             MOVE W90-ORD        TO TRA-ORD.
             MOVE DEB-ACNO       TO TRA-AC.
           IF W90-DB NOT = ZERO
               MOVE W90-DB       TO TRA-VALUE
           ELSE
               MOVE W90-CR       TO TRA-VALUE.
           IF W00-ACTION = 1
               MULTIPLY -1 BY W90-NONTAX
                                 GIVING TRA-TAXFREE
           ELSE
               MOVE W90-NONTAX   TO TRA-TAXFREE.
             MOVE W90-TTAX       TO TRA-TAX.
             ADD TRA-TAX         TO W00-T-VAT.
            PERFORM WRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY04 THRU AY59.
             MOVE 1              TO TAX-ACTYPE.
             MOVE TRA-CODE       TO TAX-CODE.
             MOVE TRA-DATE       TO TAX-DATE.
             MOVE TRA-REF        TO TAX-REF.
             MOVE TRA-VALUE      TO TAX-VALUE.
             MOVE TRA-TAXFREE    TO TAX-TAXFREE.
            MOVE TRA-AC  TO TAX-AC.
             IF W90-CR NOT = ZERO
                 MOVE "1"        TO TAX-TYPE
             ELSE
                 MOVE "2"        TO TAX-TYPE.
             MOVE ZERO           TO TAX-SEQ.
      MOVE 1   TO WS-S9.

       DEBTOR-TAX.
      MOVE W90-TAXVAL(WS-S9)
     TO TAX-VAL(WS-S9).
      MOVE W90-VAT(WS-S9) TO TAX-VAT(WS-S9).
    IF WS-S9 < 12
        ADD 1   TO WS-S9
        GO TO DEBTOR-TAX.
            PERFORM WRITE-TXTRAN THRU WRITE-TXTRAN-EXIT.
            MOVE 1   TO WS-ACTION.
            PERFORM AY19 THRU AY59.
            ADD TRA-VALUE  TO W00-T-VAL W00-T-MTDV W00-T-YTDV.
             ADD 1               TO W00-T-DAY W00-T-MTD  W00-T-YTD.
    IF WX-INT = "Y"
        PERFORM DEBTOR-INTEGRATE.
            PERFORM UPDATE-TRAN-RECORD.
           IF W90-CR NOT = ZERO
               GO TO DEBTOR-CR-TRANS.
            ADD W90-DB   TO DEB-DEBT
                                    DEB-BAL
                                    DEB-PURYTD
        DEB-MTHPUR(W20-MONTH).
            GO TO DEBTOR-TRAN-EXIT.

       DEBTOR-INTEGRATE.
             MOVE W20-DTE        TO WX-DATE.
      MOVE "  DT"  TO WX-LEDG.
      MOVE W90-CODE  TO WX-TRN.
             MOVE W00-E-DESC     TO WX-DESC.
             MOVE "- DEBTORS"    TO WX-BOOK.
             MOVE TRA-VALUE      TO WX-VALUE.
             MOVE WX-DEBGL       TO WX-AC.
             MOVE 01             TO WX-TYPE.
      *
      *    ****    Balancing the Integration entries
      *     DEBTOR/ACCOUNTS RECEIVABLE
      *
      ADD WX-VALUE  TO WXT-VALUE
      PERFORM GL-TRF-UPDATE.
      
       DEBTOR-CR-TRANS.
            SUBTRACT W90-CR  FROM DEB-CRED
          DEB-MTHPUR(W20-MONTH).
             ADD W90-CR          TO DEB-PURYTD.
             ADD W90-CR          TO DEB-BAL.
    IF OPEN-ITEM
        GO TO DEBTOR-TRAN-EXIT.
          IF WS-AGE = "M"
               GO TO DEBTOR-TRAN-EXIT.
             MOVE ZERO           TO W90-CUR W90-30 W90-60 W90-90.
             MOVE W90-CR         TO W90-VALUE.
           IF WS-AGE = "1"
               ADD W90-VALUE     TO DEB-120
               MOVE W90-VALUE    TO W90-120.
           IF DEB-120 < ZERO
               SUBTRACT DEB-120  FROM W90-120
               MOVE DEB-120      TO W90-VALUE
               MOVE ZERO         TO DEB-120
               MOVE "9"          TO WS-AGE.
           IF WS-AGE = "9"
               ADD W90-VALUE     TO DEB-90
               MOVE W90-VALUE    TO W90-90.
           IF DEB-90 < ZERO
               SUBTRACT DEB-90   FROM W90-90
               MOVE DEB-90       TO W90-VALUE
               MOVE ZERO         TO DEB-90
               MOVE "6"          TO WS-AGE.
           IF WS-AGE = "6"
               ADD W90-VALUE     TO DEB-60
               MOVE W90-VALUE    TO W90-60.
           IF DEB-60 < ZERO
               SUBTRACT DEB-60   FROM W90-60
               MOVE DEB-60       TO W90-VALUE
               MOVE ZERO         TO DEB-60
               MOVE "3"          TO WS-AGE.
           IF WS-AGE = "3"
               ADD W90-VALUE     TO DEB-30
               MOVE W90-VALUE    TO W90-30.
           IF DEB-30 < ZERO
               SUBTRACT DEB-30   FROM W90-30
               MOVE DEB-30       TO W90-VALUE
               MOVE ZERO         TO DEB-30
               MOVE "C"          TO WS-AGE.
           IF WS-AGE = "C"
               ADD W90-VALUE     TO DEB-CUR
               MOVE W90-VALUE    TO W90-CUR.
           IF DEB-CUR < ZERO
               SUBTRACT DEB-CUR  FROM W90-CUR
               MOVE ZERO         TO DEB-CUR.

       DEBTOR-TRAN-EXIT.
             EXIT.
      /
       BA000   SECTION 50.
       BA00.
      PERFORM BC000.
             GO TO AZ000-EOJ.
       BA80.
             MOVE W70-AUDIT      TO WS-AUDKEY.
             MOVE LOW-VALUES     TO AUD-REC1.
      MOVE 1   TO WS-ACTION.
             PERFORM AY16 THRU AY59.
            MOVE STK-CODE  TO AUD-EXT-CODE.
            MOVE 2   TO AUD-TYPE.
             MOVE ZERO           TO AUD-SUB.
             PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
             ADD 1               TO W70-AUDIT.
             MOVE 1              TO WS-AUDKEY.
      MOVE 0   TO WS-ACTION.
             PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
             PERFORM AY16 THRU AY59.
             MOVE 1              TO AUD-REP2.
             PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.

       BA90.
             MOVE W70-AUDIT      TO WS-AUDKEY.
             MOVE LOW-VALUES     TO AUD-REC1.
      MOVE 1   TO WS-ACTION.
            PERFORM AY16 THRU AY59.
            MOVE 7   TO AUD-TYPE.
            MOVE WS-DRAW  TO AUD-SUB.
            MOVE W90-REF  TO AUD-CSH.
            MOVE W90-EXCTOT  TO AUD-SELL.
            MOVE W90-TCOST  TO AUD-COST.
            MOVE W10-SMAN  TO AUD-SMAN.
            PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
            ADD 1   TO W70-AUDIT.
            MOVE 1   TO WS-AUDKEY.
      MOVE 0   TO WS-ACTION.
            PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
            PERFORM AY16 THRU AY59.
            MOVE 1        TO AUD-REP7.
            PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.

       BA95.
             MOVE W70-AUDIT      TO WS-AUDKEY.
            MOVE LOW-VALUES  TO AUD-REC1.
      MOVE 1   TO WS-ACTION.
            PERFORM AY16 THRU AY59.
            MOVE 3   TO AUD-TYPE.
            MOVE DEB-ACNO  TO AUD-AC.
            MOVE WS-DRAW  TO AUD-SUB.
            MOVE W90-REF  TO AUD-REF.
            MOVE W90-TTAX  TO AUD-TAX.
            MOVE W90-TOTAL  TO AUD-VAL.
            PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
            ADD 1   TO W70-AUDIT.
            MOVE 1   TO WS-AUDKEY.
      MOVE 0   TO WS-ACTION.
            PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
            PERFORM AY16 THRU AY59.
            MOVE 1   TO AUD-REP3.
            PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.

       BA100.
             MOVE W70-AUDIT      TO WS-AUDKEY.
             MOVE LOW-VALUES     TO AUD-REC1.
      MOVE 1   TO WS-ACTION.
            PERFORM AY16 THRU AY59.
            MOVE 8   TO AUD-TYPE.
            MOVE WS-DRAW  TO AUD-SUB.
            MOVE W10-SMAN  TO AUD-SMAN.
            MOVE W90-REF  TO AUD-CSH.
            MOVE W90-VALUE  TO AUD-SEL.
            MOVE W90-ICST  TO AUD-CST.
            MOVE STK-CODE  TO AUD-EXT-STOCK.
      MOVE WS-SUPER  TO AUD-SUPER.
            PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
            ADD 1   TO W70-AUDIT.
            MOVE 1   TO WS-AUDKEY.
      MOVE 0   TO WS-ACTION.
            PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
            PERFORM AY16 THRU AY59.
            MOVE 1   TO AUD-REP8.
            PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
      /
       BB050     SECTION 50.
       BB50.

            MOVE SPACES  TO REP-DETAIL2.
    IF W02-PGE-LENGTH < 67
        MOVE 2   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
    IF WS-PRN-TYPE = "H"
      *
      *      ****    E X P A N D E D   P R I N T
      *
        MOVE "E"   TO W02-PRN-TYPE
        MOVE 0   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        MOVE SPACES  TO REP-DETAIL2
        MOVE X"00"  TO INV-EXP2
    ELSE
        MOVE X"0E"  TO INV-EXP2.
      MOVE "TAX INVOICE/BELASTING FAKTUUR"
     TO INV-INVOICE.
      MOVE 0   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL2.
    IF WS-PRN-TYPE = "H"
        MOVE X"00"  TO INV-EXP2
    ELSE
        MOVE X"0E"  TO INV-EXP2.
      MOVE "DELIVERY NOTE/AFLEWERINGSNOTA"
     TO INV-INVOICE.
      MOVE 0   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE 3   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL2.
    IF WS-PRN-TYPE = "H"
        MOVE X"00"  TO INV-EXP
    ELSE
        MOVE X"0E"  TO INV-EXP.
            MOVE W95-COMP  TO INV-EXPNAME.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      *
      *      ****   C A N C E L   E X P A N D E D   P R I N T
      *
      MOVE "X"   TO W02-PRN-TYPE.
      MOVE 0   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL2.
      MOVE "D"   TO W02-PRN-TYPE.
            MOVE W95-ADD1  TO INV-NADD.
          IF W05-VATNO NOT = SPACES
              MOVE "VAT No/BTW Nr :"
     TO INV-H2
               MOVE W05-VATNO    TO INV-GNO.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL2.
             MOVE W95-ADD2       TO INV-NADD.
          IF W95-TEL NOT = SPACES
              MOVE "TELEPHONE/FOON:"
     TO INV-H2
               MOVE W95-TEL      TO INV-TEL.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL2.
             MOVE W95-ADD3       TO INV-NADD.
      PERFORM CALL-PRINTUTL.
            MOVE W95-ADD4  TO INV-NADD.
      PERFORM CALL-PRINTUTL.
       BB55.
             MOVE SPACES         TO REP-DETAIL2.
             MOVE W95-PC         TO INV-PC.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL2.
       BB60.
             EXIT.
      /
       BB070     SECTION 50.
       BB70.
             INITIALIZE GUA-RECORD1.
             MOVE ZERO           TO WS-F-ERROR.
      MOVE STK-CODE  TO GUA-EXT-CODE.
             MOVE W10-SERNO      TO GUA-SERIAL.
           IF WS-GAR = 1
               GO TO BB80.
             MOVE STK-DESC       TO GUA-DESC.
             MOVE W40-NAME       TO GUA-NAME.
             MOVE W12-T-YMD      TO GUA-DATE1 W20-DTE.
             MOVE STK-GUAR       TO GUA-PERIOD.
             ADD GUA-PERIOD      TO W20-MONTH.
       BB75.
           IF W20-MONTH > 12
               SUBTRACT 12       FROM W20-MONTH
               IF W20-YEAR = 99
                   MOVE ZERO     TO W20-YEAR
     ADD 1  TO W20-CENT
                   GO TO BB75
               ELSE
                   ADD 1         TO W20-YEAR
                   GO TO BB75.
             MOVE W90-REF        TO GUA-INV.
             MOVE W20-DTE        TO GUA-DATE2.
             PERFORM WRITE-GARTEE THRU WRITE-GARTEE-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY09 THRU AY59.
             GO TO BB99.
       BB80.
             PERFORM READ-GARTEE THRU READ-GARTEE-EXIT.
           IF WS-F-ERROR = 9
               GO TO BB99.
             MOVE 2              TO WS-ACTION.
             PERFORM AY09 THRU AY59.
             PERFORM DELETE-GARTEE-REC THRU WRITE-GARTEE-EXIT.
       BB99.
             EXIT.
      /
      *    ****    P R O D U C E   I N V O I C E
      *
       BC000      SECTION 50.
       BC00.
      *
      *    ****    I N V O I C E   S C R E E N
      *
             PERFORM ERASE-SCREEN.
             DISPLAY S26.
             MOVE 8              TO LIN.
             PERFORM BC115 UNTIL LIN > 18.
             MOVE W12-TODAY      TO W90-DTE.
            DISPLAY W90-DTE AT 0570
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            MOVE ZERO   TO W90-BDT W90-LDISC W90-NONTAX
              W90-BAL W90-SALES W90-EXCTOT
        W90-CUR W90-PMETH W40-STATUS
        W90-CR  W90-DB.
             MOVE 1              TO WS-FIRST WS-LAST.
            MOVE SPACES  TO W90-TAX W90-INSTRUCT.
      MOVE 1   TO WS-S9.

       BC08.
            MOVE ZERO   TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
    IF WS-S9 < 12
        ADD 1   TO WS-S9
        GO TO BC08.
      MOVE LS-SLS-ORDNO  TO W10-SORD.
             PERFORM GET-SORDER.
           IF WS-ERROR NOT = ZERO
               DISPLAY "Database corrupted - Press ANY KEY" AT 2502
   WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT
        ACCEPT WS-OPTION AT 2537
        WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Grey HIGHLIGHT AUTO
               GO TO BC599.
             MOVE W10-SORD       TO W90-REG.
           IF W90-EXP = "E" OR "X"
               MOVE W05-VATNO    TO W90-TAX
           ELSE
               MOVE SPACES       TO W90-TAX.
             MOVE 1              TO WS-FIRST.
             DISPLAY S36 AT 0308
            DISPLAY W40-NAME AT 0320
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            DISPLAY W90-ORD AT 0368
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            DISPLAY W10-SMAN AT 0411
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
           IF W10-SMAN NOT = ZERO
               ADD 100 W10-SMAN    GIVING WS-PARKEY
               PERFORM READ-PARAM THRU READ-PARAM-EXIT
              DISPLAY PAR-SALESMAN AT 0420
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
               MOVE PAR-SALESMAN TO W40-ASSIST
           ELSE
               MOVE SPACES       TO W40-ASSIST.
      DISPLAY W40-CONTACT AT 0529
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            DISPLAY W90-REG AT 0554
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE 1              TO WS-SALKEY.
             SUBTRACT 1          FROM WS-LAST.
       BC15.
             MOVE W90-DB         TO W90-TOTAL.
             SUBTRACT W90-LDISC  FROM W90-TOTAL.
             MOVE W90-DB         TO W100-BAL.
            DISPLAY W100-BAL AT 2015
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MULTIPLY -1 BY W90-LDISC
                                 GIVING W100-BAL.
            DISPLAY W100-BAL AT 2115
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      *
      *    ****    C A L C U L A T E   T A X   ( RUNNING TOTAL )
      *
          IF W90-TAX = SPACES
               GO TO BC20.
      MOVE 1   TO WS-S1.
      MOVE ZERO   TO W25-TOTAL.

       BC18.
          IF W90-TAXVAL(WS-S1) NOT = ZERO
              MOVE W05-VAT(WS-S1)
           TO W05-RATE
              COMPUTE W25-TOTAL ROUNDED = W25-TOTAL +
        (W90-TAXVAL(WS-S1) * W05-RTE).
    IF WS-S1 < 6
        ADD 1   TO WS-S1
        GO TO BC18.
            MOVE W25-TOTAL  TO W100-BAL.
            ADD W25-TOTAL  TO W90-TOTAL.
            DISPLAY W100-BAL AT 2215
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
       BC20.
            MOVE W90-TOTAL  TO W100-BAL.
            DISPLAY W100-BAL AT 2315
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             GO TO BC185.
       BC115.
            DISPLAY WS-MIV1 AT LINE LIN COLUMN 1
         WITH BACKGROUND-COLOR Black
       FOREGROUND-COLOR Cyan
       WS-MIV78 WITH FOREGROUND-COLOR Cyan
       WS-MIV80 WITH BACKGROUND-COLOR Black
       FOREGROUND-COLOR Cyan.
            ADD 1   TO LIN.

       BC185.
      *
      *    ****    M U S T   T H E   V A L U E   B E   A G E D
      *
            PERFORM CLEAR-L50.
            MOVE "M"   TO WS-AGE.
       BC186.
            PERFORM CLEAR-L50.
      *
      *    ****    P R O C E S S   T H E   S A L E
      *
       BC188.
           IF DEB-TERMS NOT = "3"
               DISPLAY "Processing Invoice" AT 2520 WITH
         BACKGROUND-COLOR Cyan
   FOREGROUND-COLOR Brown HIGHLIGHT BLINK
           ELSE
               DISPLAY "Processing COD sale" AT 2520 WITH
         BACKGROUND-COLOR Cyan
   FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
             MOVE 1              TO WS-SALKEY.
             PERFORM AY60 THRU AY999.
             PERFORM AY02 THRU AY59.
             MOVE W70-INVOICE    TO W90-REF.
            DISPLAY W90-REF AT 0510
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
          IF W70-INVOICE = 9999999
              MOVE 1   TO W70-INVOICE
          ELSE
              ADD 1   TO W70-INVOICE.
      *
      *    ****    T E S T   F O R   I N D I V I D U A L
      *
           IF W80-TYPE NOT = 1
               GO TO BC210.
             MOVE DEB-NAME       TO W60-NAME.
             PERFORM CA165-NAME.
      *
      *    ****    T I T L E
      *
           IF DEB-LANG = "A"
              MOVE W75-ATITLE(DEB-TITLE)
                                 TO W60-NAME
           ELSE
              MOVE W75-ETITLE(DEB-TITLE)
                                 TO W60-NAME.
             MOVE 7              TO WS-S6.
       BC190.
          IF NOT(W60-NCHAR(WS-S6) = SPACE)
               GO TO BC195.
             SUBTRACT 1          FROM WS-S6.
           IF WS-S6 > 0
               GO TO BC190.
       BC195.
      *
      *    ****    F I R S T   I N I T I A L
      *
             MOVE 1              TO WS-S5.
             ADD 2               TO WS-S6.
            MOVE W40-NCHAR(25) TO W60-NCHAR(WS-S6).
             ADD 1               TO WS-S6.
            MOVE "."   TO W60-NCHAR(WS-S6).
          IF W40-NCHAR(49) = SPACE
               GO TO BC200.
      *
      *    ****    S E C O N D   I N I T I A L
      *
             ADD 1               TO WS-S6.
            MOVE W40-NCHAR(49)  TO W60-NCHAR(WS-S6).
             ADD 1               TO WS-S6.
            MOVE "."   TO W60-NCHAR(WS-S6).
       BC200.
             ADD 2               TO WS-S6.
       BC205.
      *
      *    ****    S U R N A M E
      *
            MOVE W40-NCHAR(WS-S5)
           TO W60-NCHAR(WS-S6).
           IF WS-S5 < 24
               ADD 1             TO WS-S5
               ADD 1             TO WS-S6
               GO TO BC205.
             MOVE W60-NAME       TO W40-NAME.
       BC210.
             MOVE ZERO           TO WS-PAGE.
             MOVE SPACES         TO W40-PC1 W40-PC2.
             MOVE DEB-ADDRESS    TO W60-ADDRESS.
             PERFORM CA175-ADDRESS.
          IF NOT(DEB-POST = ZERO OR 9999)
               MOVE DEB-POST     TO W40-PC1.
           IF W40-ADD4 = SPACES
               MOVE W40-PC1      TO W40-ADD4
               MOVE SPACES       TO W40-PC1.
           IF W40-ADD3 = SPACES
               MOVE W40-ADD4     TO W40-ADD3
               MOVE SPACES       TO W40-ADD4.
           IF W40-ADD2 = SPACES
               MOVE W40-ADD3     TO W40-ADD2
               MOVE SPACES       TO W40-ADD3.
           IF W40-ADD1 = SPACES
               MOVE W40-ADD2     TO W40-ADD1
               MOVE SPACES       TO W40-ADD2.
             MOVE DEB-PADDR      TO W60-ADDRESS1.
             PERFORM CA185-ADDRESS.
          IF NOT(DEB-PPOST = ZERO OR 9999)
               MOVE DEB-PPOST    TO W40-PC2.
           IF W40-PADD4 = SPACES
               MOVE W40-PC2      TO W40-PADD4
               MOVE SPACES       TO W40-PC2.
           IF W40-PADD3 = SPACES
               MOVE W40-PADD4    TO W40-PADD3
               MOVE SPACES       TO W40-PADD4.
           IF W40-PADD2 = SPACES
               MOVE W40-PADD3    TO W40-PADD2
               MOVE SPACES       TO W40-PADD3.
           IF W40-PADD1 = SPACES
               MOVE W40-PADD2    TO W40-PADD1
               MOVE SPACES       TO W40-PADD2.
              DISPLAY "Processing Invoice" AT 2520 WITH
         BACKGROUND-COLOR Cyan
   FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
       BC215.
      *
      *    ****    P R I N T   I N V O I C E   H E A D I N G S
      *
            PERFORM BB050.
            MOVE SPACES  TO REP-DETAIL2.
    IF WS-PRN-TYPE = "H"
      *
      *      ****    E X P A N D E D   P R I N T
      *
        MOVE "E"   TO W02-PRN-TYPE
        MOVE 0   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        MOVE SPACES  TO REP-DETAIL2
        MOVE X"00"  TO INV-EXP
    ELSE
        MOVE X"0E"  TO INV-EXP.
      MOVE "REF No:"  TO INV-H14.
             MOVE W90-REF        TO INV-REF.
      MOVE 0   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL2.
    IF WS-PRN-TYPE = "H"
        MOVE X"00"  TO INV-EXP
    ELSE
        MOVE X"0E"  TO INV-EXP.
      MOVE "DATE  :"  TO INV-H14.
            MOVE W12-TODAY  TO INV-DTE W40-DTE.
      MOVE 0   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      *
      *      ****   C A N C E L   E X P A N D E D   P R I N T
      *
      MOVE "X"   TO W02-PRN-TYPE.
      MOVE 0   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL2.
      MOVE "DELIVERY / AFLEWRING"
     TO INV-DADD2.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL2.
      MOVE "D"   TO W02-PRN-TYPE.
             MOVE W40-NAME       TO INV-NAME.
      MOVE "PAGE / BLADSY"
     TO INV-H4.
      ADD 1   TO WS-PAGE.
      MOVE WS-PAGE  TO INV-PGE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL2.
             MOVE W40-ADD1       TO INV-NAME.
            MOVE W40-PADD1  TO INV-DADD2.
      PERFORM CALL-PRINTUTL.
            MOVE W40-ADD2  TO INV-NAME.
            MOVE W40-PADD2  TO INV-DADD2.
      MOVE "ACCOUNT / REKENING"
     TO INV-H4.
            MOVE W80-ACNO  TO INV-AC.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL2.
             MOVE W40-ADD3       TO INV-NAME.
            MOVE W40-PADD3  TO INV-DADD2.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL2.
             MOVE W40-ADD4       TO INV-NAME.
            MOVE W40-PADD4  TO INV-DADD2.
      MOVE "ORDER / BESTELLING"
     TO INV-H4.
             MOVE W90-ORD        TO INV-ORD.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL2.
             MOVE W40-PC1        TO INV-PC1.
            MOVE W40-PC2  TO INV-PC2.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL2.
      *
      *    ****    C L I E N T  V A T  N U M B E R
      *
    IF NOT (W40-TAX = SPACES)
        MOVE "Vat No"  TO INV-VATH
        MOVE W40-TAX  TO INV-VATNO.
      *
      *    ****    P R I N T   T E R M S   O N   T H E   I N V O I C E
      *
      MOVE "TERMS / TERME"
     TO INV-H4.
          IF DEB-TERMS = "0"
              MOVE "7 Days/Dae" TO INV-TERMS
          ELSE
          IF DEB-TERMS = "2"
              MOVE "60 Days/Dae"
     TO INV-TERMS
          ELSE
          IF DEB-TERMS = "1"
              MOVE "30 Days/Dae"
     TO INV-TERMS
          ELSE
              MOVE "COD/KBA"  TO INV-TERMS.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL2.
    IF W10-SMAN NOT = ZERO
        MOVE "Assist :"  TO INV-H3
        MOVE W40-ASSIST  TO INV-DADD2.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL2.
      MOVE "  QUANTITY"  TO INV-H7.
      MOVE "DISCOUNT"  TO INV-H9.
      MOVE "EXCLUSIVE"  TO INV-H10.
      MOVE "NETT AMOUNT"  TO INV-H12.
      MOVE 0   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL2.
      MOVE "ITEM CODE/KODE"
     TO INV-H5.
      MOVE "DESCRIPTION/BESKRYWING"
     TO INV-H6.
      MOVE "HOEVEELHEID"  TO INV-H7.
      MOVE "PRICE/PRYS"  TO INV-H8.
      MOVE " AFSLAG"  TO INV-H9.
      MOVE "EKSKLUSIEF"  TO INV-H10.
      MOVE "VAT/BTW"  TO INV-H11.
      MOVE "NETO BEDRAG"  TO INV-H12.
      MOVE 0   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
       BC216.
      *
      *    ****    S T A R T   I N V O I C E   L O G G I N G   ( RE-PRINT )
      *
            MOVE SPACES  TO REP-DETAIL2.
            MOVE WS-DET-LINES  TO WS-LINE.

       BC220.
             EXIT.

       BC222.
             MOVE WS-PAGE        TO DOC-PAGE.
             MOVE W80-ACNO       TO DOC-AC.
             MOVE 100            TO DOC-SQN.
             MOVE ZERO           TO DOC-TYPE.
             MOVE W40-NAME       TO DOC-NAME.
             MOVE ZERO           TO DOC-PICNO.
             MOVE WS-CR          TO DOC-DOC.
             MOVE W90-EXP        TO DOC-EXP.
             MOVE WS-PRC         TO DOC-PRC.
             MOVE W90-ORD        TO DOC-ORD.
             MOVE W90-REF        TO DOC-REF.
             MOVE W90-REG        TO DOC-OREF.
             MOVE W12-T-YMD      TO DOC-DTE.
             MOVE W10-SMAN       TO DOC-SMAN.
             MOVE W90-TOTAL      TO DOC-VALUE.
      *
      *    ****    L O G   R E C O R D   T O   I N V O I C E   F I L E
      *
       BC225.
      PERFORM WRITE-INVOIC THRU WRITE-INVOIC-EXIT.
            MOVE 1   TO WS-ACTION.
      PERFORM AY22 THRU AY59.
       BC235.
      *
      *    ****    R E A D   T H E   S A L E   F I L E   A N D
      *            P R I N T   T H E   I N V O I C E
      *
           IF WS-SALKEY > WS-LAST
               GO TO BC255.
             PERFORM READ-SALE THRU READ-SALE-EXIT.
           IF WS-LINE > 1
               SUBTRACT 1        FROM WS-LINE
           ELSE
               PERFORM BC250.
          IF SAL-EXT-ITEM = SPACES
               GO TO BC247.
            MOVE SAL-EXT-ITEM   TO STK-CODE.
             PERFORM AY05 THRU AY59.
      *
      *    ****    N E X T   E N T R Y   O N   I N V O I C E   L O G
      *            R E C O R D
      *
             MOVE W80-ACNO           TO CRD-AC.
             MOVE ZERO               TO CRD-SEQ.
             MOVE W10-SMAN           TO CRD-SMAN.
             MOVE W90-REF            TO CRD-REF.
             MOVE STK-SUPP           TO CRD-SUPP.
             MOVE W12-T-YMD          TO CRD-DTE.
             MOVE W40-NAME           TO CRD-NAME.
             MOVE SPACES             TO CRD-WHS.
             MOVE STK-LDG            TO CRD-DEPT.
             MOVE 1                  TO DOC-TYPE.
             INITIALIZE DOC-DET2.
             MOVE SAL-EXT-ITEM       TO DOC-EXT-ITEM CRD-EXT-CODE.
             MOVE SAL-DESC           TO DOC-DESC.
             MOVE SAL-QNT            TO W10-QUANT DOC-QNT.
             MOVE SAL-PACK           TO WS-P1.
             MULTIPLY STK-UNT(WS-P1) BY W10-QUANT GIVING W10-QNT.
             MOVE W10-QNT            TO CRD-QNT.
             MOVE SAL-COST           TO W10-COST DOC-COST.
             COMPUTE CRD-COST = SAL-COST / STK-UNT(WS-P1).
             MOVE SAL-VAT            TO DOC-VAT.
             MOVE SAL-SELL           TO W10-SELL DOC-SELL.
             MOVE SAL-VAL            TO W25-VALUE DOC-VAL.
             MOVE SAL-EXT-ITEM       TO INV-EXT-ITM.
             MOVE SAL-DESC           TO INV-DESC.
             MOVE SAL-QNT            TO INV-QNT.
             MOVE SAL-VAT            TO INV-TAX.
             MOVE SAL-SELL           TO INV-PRICE.
             MOVE SAL-DSC            TO WS-DISC INV-DSC.
             MOVE SAL-DISC           TO W90-CDISC W90-EXCDIS.
             SUBTRACT W90-CDISC FROM W25-VALUE GIVING W90-VALUE.
             MOVE SAL-TAXVAL         TO DOC-TAXVAL.
             MOVE SAL-NONTAX         TO DOC-NONTAX.
             MOVE SAL-SERNO          TO W10-SERNO DOC-SERNO.
             MOVE SAL-SUPER          TO WS-SUPER.
             MOVE SAL-VAT-SUB        TO WS-VAT-SUB DOC-VAT-SUB.
           IF WS-VAT-SUB = ZERO
               MOVE ZERO             TO DOC-V-PER
           ELSE
               MOVE W05-VAT(WS-VAT-SUB) TO DOC-V-PER.
          IF W90-TAX = SPACES
              IF W90-EXP NOT = "X"
                  IF WS-VAT-SUB > ZERO
                      MOVE W05-VAT(WS-VAT-SUB) TO W05-RATE
                      COMPUTE W90-VALUE ROUNDED = W90-VALUE - (((W90-VALUE / (W05-RATE + 100.00000)) * W05-RATE))
                      COMPUTE W90-EXCDIS ROUNDED = W90-CDISC - (((W90-CDISC / (W05-RATE + 100.00000)) * W05-RATE)).
             MOVE W90-VALUE          TO INV-EXTEND.
             ADD W90-VALUE SAL-VAT   GIVING INV-VAL.
      *      ADD W90-VALUE  TO W90-EXCTOT.
             COMPUTE CRD-SELL ROUNDED = W90-VALUE / SAL-QNT.
           IF (WS-CARDEX = "Y")
               PERFORM WRITE-CARDEX THRU WRITE-CARDEX-EXIT
               MOVE 1                TO WS-ACTION
               PERFORM AY43 THRU AY59.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO REP-DETAIL2.
             ADD W25-VALUE           TO W90-STOTAL.
      *
      *    ****    H A S   L I N E   D I S C O U N T   B E E N
      *            A P P L I E D
      *
           IF W90-CDISC = ZERO
               GO TO BC240.
      *
      *    ****    P R I N T   A N D   L O G   T H E   L I N E
      *            D I S C O U N T
      *
             MOVE WS-DISC        TO DOC-DSC.
             MULTIPLY -1         BY W90-CDISC
                                 GIVING DOC-DISC.
       BC240.
      *
      *    ****    U P D A T E   S A L E S   A N A L Y S I S
      *
       BC242.
            MOVE STK-LDG  TO DPT-CODE.
            PERFORM AY38 THRU AY59.
            ADD W90-VALUE  TO DPT-SALES-DAY
              DPT-SALES-MTD
              DPT-SALES-YTD.
           IF WX-INT = "Y"
               MOVE W12-T-YMD    TO WX-DATE
        MOVE STK-LDG  TO WX-LEDG
               MOVE "SALES - DEBTORS"
                                 TO WX-NAR
        COMPUTE WX-VALUE = W90-VALUE * -1
        MOVE DPT-SALE-GL  TO WX-AC
        MOVE 04   TO WX-TYPE
      *
      *    ****    Balancing the Integration entries
      *     SALES LEDGER/CATERGORY
      *
        IF BALANCING-ENTRY-STORED
     ADD WX-VALUE  TO WXT-VALUE
        ELSE
      *    *************************************************************
      *    *  Store first Income Integration entry        *
      *    *  As items are selected randomly, storing the first income *
      *    *  entry each time will mean that INCOME ACCOUNTS are also  *
      *    *  randomly adjusted.           *
      *    *************************************************************
     MOVE WX-LEDTRF
     TO WXT-LEDTRF
     MOVE "Y"  TO WS-BAL-ENTRY
        END-IF
     PERFORM GL-TRF-UPDATE.
      *
      *    ****    U P D A T E   C O S T - O F - S A L E S
      *
             COMPUTE W90-BCR ROUNDED = (1.000 * W10-QUANT * W10-COST).
             ADD W90-BCR         TO DPT-COST-DAY
                                    DPT-COST-MTD
                                    DPT-COST-YTD.
           IF (WX-INT = "Y") AND (W90-BCR NOT = ZERO)
        MOVE W90-BCR  TO WX-VALUE
               MOVE DPT-COST-GL  TO WX-AC
               MOVE 08           TO WX-TYPE
               PERFORM GL-TRF-UPDATE
               COMPUTE WX-VALUE = W90-BCR * -1
               MOVE DPT-PUR-GL   TO WX-AC
               MOVE 19           TO WX-TYPE
               PERFORM GL-TRF-UPDATE.
      *
      *    ****    U P D A T E   D I S C O U N T   G I V E N
      *
            ADD W90-EXCDIS  TO DPT-DISC-DAY
              DPT-DISC-MTD
              DPT-DISC-YTD
              DPT-SALES-DAY
              DPT-SALES-MTD
              DPT-SALES-YTD.
            PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
             MOVE W12-T-YMD      TO STK-ACT.
      *
      *    ****    C H E C K   I F   S T O C K   C O N T R O L L E D
      *
    IF WS-PRC-IND = "Y"
              IF STK-IND > 1
           GO TO BC245
        END-IF
    ELSE
    IF STK-IND > ZERO
              GO TO BC245.
            SUBTRACT W10-QNT  FROM STK-QPIC.
    IF STK-USE-CASES = "Y"
        SUBTRACT SAL-CASES
     FROM STK-CASES.
          IF STK-IND = 1
               GO TO BC245.
          IF STK-DISC > ZERO
               GO TO BC245.
      *
      *    ****    C H E C K   R E - O R D E R   L E V E L   A N D
      *            R E P O R T   L O W - L E V E L S
      *
           IF STK-QUANT NOT > STK-LEVEL
               MOVE 1            TO STK-DISC
               PERFORM BA80.
       BC245.
             ADD W90-BCR         TO W90-BDT STK-MTDC STK-YTDC.
            ADD W10-QNT  TO STK-MTD STK-YTD.
            ADD W90-VALUE  TO STK-MTDV W90-SALES STK-YTDV.
            PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
      *
      *    ****    L O G   I N D I V I D U A L   I T E M S   T O   T H E
      *            A U D I T   F I L E
      *
             MOVE W90-BCR        TO W90-ICST.
             PERFORM BA100.
           IF W10-SERNO NOT = SPACES
               MOVE ZERO         TO WS-GAR
               PERFORM BB070.
             ADD 1               TO DOC-SEQ.
             MOVE ZERO           TO DOC-SUB.
             PERFORM BC225.
      *
      *    ****    G O   A N D   G E T   T H E   N E X T   I T E M
      *
       BC246.
             ADD 1               TO WS-SALKEY.
             GO TO BC235.
       BC247.
             MOVE 3              TO DOC-TYPE.
             INITIALIZE DOC-DET2.
      *
      *     C H E C K   F O R   S E R I A L   N U M B E R S
      *
           IF SAL-SUB NOT = ZERO
               MOVE 1            TO WS-S1
               PERFORM BC248
               MOVE "SERIAL NO:" TO INV-HD
        MOVE SAL-MSER  TO INV-SERNOS DOC-MSER
               ADD 1             TO DOC-SUB
           ELSE
               MOVE ZERO         TO DOC-SUB
               ADD 1             TO DOC-SEQ
              MOVE SAL-DESC  TO INV-MES DOC-DESC.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL2.
             PERFORM BC225.
             GO TO BC246.
       BC248.
      MOVE SAL-SNO(WS-S1) TO W10-SERNO.
             MOVE ZERO           TO WS-GAR.
             PERFORM BB070.
             ADD 1               TO WS-S1.
    IF WS-S1 < 3
        IF NOT(SAL-SNO(WS-S1) = SPACES)
                   GO TO BC248.
       BC250.
      *
      *    ****    C A R R I E D   F O R W A R D   -   N E X T   P A G E
      *
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL2.
            MOVE "C/FWD - OORGEDRA"
           TO INV-DADD2.
            MOVE W90-STOTAL  TO INV-VAL.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL2.
    IF W30-RMKS15(1) = SPACES
        MOVE "Please check your invoice."
     TO INV-RMKS
    ELSE
        MOVE W30-RMKS15(1)
     TO INV-REMKS
    END-IF.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL2.
    IF W30-RMKS15(2) = SPACES
        MOVE "Gaan asseblief u faktuur na."
     TO INV-RMKS
    ELSE
        MOVE W30-RMKS15(2)
     TO INV-REMKS
    END-IF.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL2.
      MOVE W30-RMKS15(3)  TO INV-REMKS.
      MOVE 3   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL2.
            PERFORM BC215 THRU BC220.
            MOVE "B/FWD - O/B"  TO INV-DESC.
            MOVE W90-STOTAL  TO INV-VAL.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL2.
             SUBTRACT 2          FROM WS-LINE.
       BC255.
      *
      *    ****    I N V O I C E   T O T A L S
      *
      MOVE WS-LINE  TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL2.
            MOVE ALL "-"  TO INV-LINE.
      *
      *    ****    P R I N T   S U B - T O T A L
      *
    IF W30-RMKS15(1) = SPACES
        MOVE "Please check your invoice."
     TO INV-RMKS
    ELSE
        MOVE W30-RMKS15(1)
     TO INV-REMKS.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL2.
      MOVE "TOTAL/TOTAAL" TO INV-H13.
    IF W30-RMKS15(2) = SPACES
        MOVE "Gaan aseblief u faktuur na."
     TO INV-RMKS
    ELSE
        MOVE W30-RMKS15(2)
     TO INV-REMKS.
       BC256.
      MOVE 9   TO DOC-TYPE.
             MOVE 99900          TO DOC-SQN.
             INITIALIZE DOC-DET3.
             MOVE W90-DB         TO DOC-SUBTOT.
      MOVE DEB-TERMS  TO DOC-TERMS.
            MOVE SPACES  TO REP-DETAIL2.
      *
      *    ****    P R I N T   D I S C O U N T   A N D   U P D A T E
      *            T H E   S U N D R Y   D I S C O U N T   T O T A L S
      *
          IF W90-TDISC NOT = ZERO
               MULTIPLY -1 BY W90-TDISC
                                 GIVING DOC-DSCNT
              IF W90-LDISC = ZERO
                   IF W90-ORG = "I"
               MOVE "YYYY"
     TO DPT-CODE
               PERFORM AY38 THRU AY59
                       ADD W90-TDISC
                                 TO DPT-DISC-DAY
                                    DPT-DISC-MTD
                                    DPT-DISC-YTD
                       IF WX-INT = "Y"
                           MOVE W90-TDISC    TO WX-VALUE
                           MOVE DPT-DISC-GL  TO WX-AC
      MOVE DPT-CODE     TO WX-LEDG
      MOVE 15      TO WX-TYPE
                           PERFORM GL-TRF-UPDATE
                       END-IF
               PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
      *
      *    ****    P R I N T   T H E   T A X   F I G U R E S   A N D
      *            U P D A T E   T A X   T O T A L S
      *
      *     TAX CALCULATED IN SLS003
      *         W90-NONTAX   =TOTAL NON TAXABLE VALUE
      *         W90-TTAX     =TOTAL TAX (ALL RATES)
      *         W90-TAXVAL(?)=TOTAL TAXABLE PER RATE
      *         W90-VAT(?)   =TOTAL VAT PER RATE
      *         W90-TOTAL    =TOTAL FOR CREDIT NOTE
      *
            MOVE W05-RATE  TO WS-PER.
       BC258.
      MOVE W90-EXCTOT  TO INV-EXTEND.
            MOVE W90-TTAX  TO DOC-TAX.
          IF W90-TAX NOT = SPACES
              MOVE W90-TTAX    TO INV-TAX.
       BC260.
      *
      *    ****    I N V O I C E   T O T A L   (P R I N T   &   L O G)
      *
             MOVE W90-TOTAL      TO W90-DB.
             MOVE W90-BDT        TO W90-TCOST.
             MOVE W90-DB         TO DOC-TOT.
      MOVE W90-EXCTOT  TO DOC-TAXABLE.
             MOVE W90-NONTAX     TO DOC-NON-TXBL.
             MOVE W90-BAL        TO DOC-BAL.
      MOVE W40-CONTACT  TO DOC-CONTACT.
      MOVE W40-TEL  TO DOC-TEL.
      PERFORM BC225.
            MOVE W90-DB  TO INV-VAL.
      MOVE 0   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE 1   TO WS-ADVANCE.
      MOVE SPACES  TO INV-REMKS.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL2.
      MOVE W30-RMKS15(3)  TO INV-REMKS.
      MOVE ALL "="  TO INV-LINE.
          IF NOT(DEB-SDISC = ZERO) AND
       (W30-RMKS15(3) = SPACES)
              MOVE W15-DISC(DEB-SDISC)
                                 TO W30-PERC
              MOVE W30-MESSAGE  TO INV-RMKS.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL2.
      MOVE "Processed by APAC Accounting - (011) 803-2572"
     TO REP-DETAIL2
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL2.
            MOVE W12-T-YMD  TO W20-DTE.
      *
      *    ****    U P D A T E   T H E   D E B T O R   A C C O U N T
      *
             MOVE 1              TO W90-CODE.
             PERFORM GET-DEBTOR-TRAN-RECORD.
             PERFORM GENERATE-DEBTOR-TRAN.
            MOVE 9   TO WS-PARKEY.
             PERFORM AY01 THRU AY59.
      MOVE 1   TO W05-V-RATE.

       BC262-VAT.
    IF W90-VAT(W05-V-RATE) = ZERO
        GO TO BC262-CHECK-END.
      MOVE W05-VAT-CODE  TO DPT-CODE.
            PERFORM AY38 THRU AY59.
      *
      *    ****    V A T   S T A T I S T I C S
      *
            ADD W90-VAT(W05-V-RATE)
     TO DPT-O-VAT-DAY
              DPT-O-VAT-MTD
              DPT-O-VAT-YTD.
      ADD W90-TAXVAL(W05-V-RATE)
     TO DPT-V-SALES-DAY
        DPT-V-SALES-MTD
        DPT-V-SALES-YTD.
    IF (WX-INT = "Y") AND (W90-VAT(W05-V-RATE) NOT = ZERO)
  COMPUTE WX-VALUE = W90-VAT(W05-V-RATE) * -1
  MOVE DPT-O-VAT-GL
     TO WX-AC
  MOVE DPT-CODE  TO WX-LEDG
  MOVE 10   TO WX-TYPE
  PERFORM GL-TRF-UPDATE.
            PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
      ADD W90-TAXVAL(W05-V-RATE)
     TO PAR-DVAT-DAY
                                    PAR-DVAT-MTD
                                    PAR-DVAT-YTD.

       BC262-CHECK-END.
    IF W05-V-RATE < 6
        ADD 1   TO W05-V-RATE
        GO TO BC262-VAT.
      ADD W90-NONTAX  TO PAR-DNGS-DAY
                                    PAR-DNGS-MTD
                                    PAR-DNGS-YTD.
      PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
            MOVE ZERO   TO W90-CUR W90-30 W90-60
                                    W90-120 W90-90.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  V A T   C A L C U L A T I O N   P R O B L E M  - F I X  ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³ Check if the Integration entries are in balance. If they  ³
      *    ³ are out of balance, process a balancing entry.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
    IF WX-INT = "Y"
        IF NOT(WXT-VALUE = ZERO)
     MOVE WXT-LEDTRF
     TO WX-LEDTRF
      *
      *    ****  Change the sign of the amount that the entries are
      *   out of balance and process.
      *
     COMPUTE WX-VALUE = WX-VALUE * -1
     PERFORM GL-TRF-UPDATE
     MOVE "N"  TO WS-BAL-ENTRY.
      *
      *    ****    L O G   T H E   S A L E S M A N   &   I N V O I C E -
      *            A U D I T   F I L E   D E T A I L S
      *
             PERFORM BA90.
             PERFORM BA95.
      *
      *    ****    U P D A T E   T H E   D E B T O R   C A T E G O R Y
      *
             ADD 121 DEB-CAT     GIVING W90-RESULT.
             DIVIDE W90-RESULT BY 2
                                 GIVING W90-CATKEY
                                 REMAINDER W90-CS.
             ADD 1               TO W90-CS.
             MOVE W90-CATKEY     TO WS-PARKEY.
             PERFORM AY01 THRU AY59.
            ADD W90-DB   TO PAR-CDRMTD(W90-CS).
            ADD W90-DB   TO PAR-CDRYTD(W90-CS).
            PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
      *
      *    ****    U P D A T E   T H E   S A L E S M A N   R E C O R D
      *
           IF W10-SMAN NOT = ZERO
               ADD 100 W10-SMAN  GIVING WS-PARKEY
               PERFORM AY01 THRU AY59
               ADD W90-TCOST     TO PAR-CT-MTD PAR-CT-YTD
               ADD W90-EXCTOT    TO PAR-SL-MTD PAR-SL-YTD
              PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
      *
      *    ****    R E W R I T E   T H E   D E B T O R   R E C O R D
      *
            PERFORM REWRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.
      *
      *    ****    U P D A T E   T H E   D E B T O R   C O N T R O L
      *            R E C O R D
      *
            ADD W90-DB   TO W70-OUT
              W70-DT.
            ADD W90-CUR  TO W70-MTD.
            ADD W90-30   TO W70-ONE.
            ADD W90-60   TO W70-TWO.
            ADD W90-90   TO W70-THREE.
            ADD W90-120  TO W70-FOUR.
      *
      *    ****    M U S T   T H E   C A S H   D R A W   B E
      *            U P D A T E D
      *
      *    IF W90-ORG = "C"
      *        PERFORM BC268.
      *
      *    ****    U P D A T E   T H E   S T O C K   C O N T R O L
      *            R E C O R D
      *
             SUBTRACT W90-BAL    FROM STK-TOTAL.
            ADD W90-SALES  TO STK-SMTD STK-SYTD.

       BC266.
      *
      *    ****    U P D A T E   T H E   C O S T - O F - S A L E S
      *
             MOVE 8              TO WS-PARKEY.
             PERFORM AY01 THRU AY59.
             ADD W90-BDT         TO PAR-CSHT PAR-CMTD PAR-CYTD.
            PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.

       BC267.
      *
      *    ****    U P D A T E   T H E   O R D E R   F I L E
      *
             MOVE W10-SORD       TO SOR-REF.
             MOVE 100            TO SOR-SQN.
      PERFORM READ-SORDER THRU READ-SORDER-EXIT.
             PERFORM AY17 THRU AY59.
             MOVE "I"            TO SOR-DOC.
      PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT.
             MOVE 99900          TO SOR-SQN.
      PERFORM READ-SORDER THRU READ-SORDER-EXIT.
             PERFORM AY17 THRU AY59.
             MOVE W90-REF        TO SOR-INVOICE.
      PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT.
      *
      *    ****    E N D   O F F   T H E   R E C O V E R Y   L O G
      *
             PERFORM AY70 THRU AY999.
      *
      *    ****    C L O S E   F I L E S
      *            ( FORCE SYSTEM TO WRITE DETAILS TO DISK )
      *
            CLOSE PARAM
                   DEPART
                   DEBTOR
                   AUDIT
                   STOCK
                   INVOICE
           SALE.
             OPEN OUTPUT SALE.
             CLOSE SALE.
             OPEN I-O PARAM
                      DEPART
                      DEBTOR
                      AUDIT
                      STOCK
                      INVOICE
                      SALE.
            PERFORM CLEAR-L50.
             GO TO BC599.
      *
      *    ****    U P D A T E   T H E   C A S H   D R A W
      *
      *BC268.
      *    IF WS0-POS = 2
      *      PERFORM OPEN-CASH-DRAWER THRU OPEN-CASH-DRAWER-EXIT.
      *      ADD 150 WS-DRAW  GIVING WS-PARKEY.
      *      PERFORM AY01 THRU AY59.
      *      ADD W90-CASH  TO PAR-CASH.
      *      ADD W90-CHQ  TO PAR-CHEQUES.
      *      ADD W90-CARD  TO PAR-CARDS.
      *      SUBTRACT W90-CHANGE FROM PAR-CASH.
      *      ADD W90-TTAX  TO PAR-VATVAL.
      *      ADD W90-TAXVAL  TO PAR-TAXVAL.
      *      COMPUTE PAR-CSH = PAR-CSH + W90-TAXVAL + W90-NONTAX.
      *      COMPUTE PAR-CSHMTD = PAR-CSHMTD + W90-TAXVAL + W90-NONTAX.
      *      COMPUTE PAR-CSHYTD = PAR-CSHYTD + W90-TAXVAL + W90-NONTAX.
      *      PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
       BC599.
             EXIT.
      /
       ZA000-INIT        SECTION 90.
       ZA000-OPEN.
      MOVE LS-COMPANY  TO WS-TOP-COMP.
      MOVE LS-PARID  TO WS-PARID.
      MOVE LS-L-OR-N  TO W02-L-OR-N.
      MOVE WS-SYS-ID  TO W02-SYSID.
      MOVE LS-TODAY-DDMMYY
     TO TODAY-DDMMYY W12-TODAY.
      MOVE LS-USUB  TO WS-USUB.
      MOVE LS-DRAW  TO WS-DRAW.
            DISPLAY WS-BLNK78 AT 1202 WITH FOREGROUND-COLOR Cyan.
             DISPLAY "PRINT INVOICE PROGRAM" AT 1231
              WITH FOREGROUND-COLOR Grey HIGHLIGHT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     Setup the file ID's including path names           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *    *************************************************************
      *    ****    P A R A M E T E R   F I L E      ****
      *    *************************************************************
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
       WS-STATUS DELIMITED SIZE
       INTO WS-ERR-MES.
      PERFORM ERROR-LENGTH THRU ERROR-EXIT.
            STOP RUN.

       ZA00-READ-APACFIDS-EXIT.
      EXIT.

       ZA00A-CONTINUE.
      MOVE AFID-PATH  TO W02-PARAM.
      *    *************************************************************
      *    ****  D A I L Y   - A U D I T   F I L E     ****
      *    *************************************************************
      MOVE "AUDITF"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-AUDITF.
      *    *************************************************************
      *    ****       S A L E S   -   C A R D E X   F I L E     ****
      *    *************************************************************
      MOVE "CARDEX"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-CARDEX.
      *    *************************************************************
      *    ****    D E B T O R  T R A N S A C T I O N  F I L E    ****
      *    *************************************************************
      MOVE "DBTRAN"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-DBTRAN.
      *    *************************************************************
      *    ****  D E B T O R   A C C O U N T   ( A / R )   F I L E  ****
      *    *************************************************************
      MOVE "DEBTOR"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-DEBTOR.
      *    *************************************************************
      *    ****     D E P A R T M E N T  F I L E     ****
      *    *************************************************************
      MOVE "DEPART"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-DEPART.
      MOVE "GARTEE"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-GARTEE.
      *    *************************************************************
      *    ****        D E B T O R   I N V O I C E   F I L E     ****
      *    *************************************************************
      MOVE "INVOIC"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-INVOIC.
      *    *************************************************************
      *    ****    I N T E G R A T I O N   F I L E     ****
      *    *************************************************************
      MOVE "LEDTRF"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-LEDTRF.
      *    *************************************************************
      *    ****      C O N T R O L   F I L E      ****
      *    *************************************************************
      MOVE "NETWORK"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-NETWORK.
      *    *************************************************************
      *    ****      R E C O V E R Y   F I L E      ****
      *    *************************************************************
      MOVE "RECOVER"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      INSPECT AFID-PATH REPLACING FIRST "XXX"
          BY LS-USER.
      MOVE AFID-PATH  TO W02-RECOVER.
      MOVE "SALE"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      INSPECT AFID-PATH REPLACING FIRST "XXX"
          BY LS-USER.
      MOVE AFID-PATH  TO W02-SALE.
      *    *************************************************************
      *    ****      N E T W O R K   (S H A R E D)   F I L E     ****
      *    *************************************************************
      MOVE "SHARED"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-SHARED.
      MOVE "SORDER"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-SORDER.
      *    *************************************************************
      *    ****         S T O C K   F I L E      ****
      *    *************************************************************
      MOVE "STOCKF"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-STOCKF.
      MOVE "TXTRAN"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-TXTRAN.

       ZA01.
             MOVE 3              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-INV  TO WS-INV.
             MOVE PAR-VAT-REG    TO W05-VATNO.
      MOVE PAR-CRDX  TO WS-CARDEX.
      MOVE PAR-PRICED-IND TO WS-PRC-IND.

       ZA01B.
      *
      *  ***  ****  ***** *   *   **** ****  *** *   * ***** ***** ****
      * *   * *   * *   **  *   *   * *   *  *  **  *   *   *     * *
      * *   * ****  ***   * * *   **** ****   *  * * *   *   ***   ****
      * *   * *     *   *  **   * *   *  *  *  **   *   *     * *
      *  ***  *     ***** *   *   * *   * *** *   *   *   ***** * *
      *
      PERFORM OPEN-PRINTER.
      MOVE WS-PAPER  TO WS-SINGLE.
      MOVE "P"   TO WS-COMMAND.
    IF WS-SCREEN-LIN = 77
        MOVE "H"   TO WS-PRN-TYPE
        MOVE ZERO  TO WS-SCREEN-LIN.
      *
      *    ****   1 7 C P I ( C O N D E N S E D   P R I N T )
      *
      MOVE 0   TO WS-ADVANCE.
      MOVE 3   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
    IF NOT (W02-PGE-LENGTH = 66)
        COMPUTE WS-DET-LINES = WS-DET-LINES
        + (W02-PGE-LENGTH - 66).
    IF WS-DET-LINES > 31
        SUBTRACT 2  FROM WS-DET-LINES.
            MOVE SPACES  TO REP-DETAIL2.
            OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
            OPEN OUTPUT SALE.
             CLOSE SALE.
             OPEN I-O SALE.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
             MOVE PAR-COMPANY    TO W95-COMP.
             MOVE PAR-TELEPHONE  TO W95-TEL.
           IF PAR-STMINV = "I" OR "X"
               MOVE "N"          TO WS-HEAD
           ELSE
               MOVE "Y"          TO WS-HEAD.
             ADD 1               TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-ADDRESS1   TO W95-ADD1.
             MOVE PAR-ADDRESS2   TO W95-ADD2.
             MOVE PAR-ADDRESS3   TO W95-ADD3.
             MOVE PAR-ADDRESS4   TO W95-ADD4.
             MOVE PAR-POST-CDE   TO W95-POST.
           IF W95-ADD4 = SPACES
               MOVE W95-PC       TO W95-ADD4
               MOVE SPACES       TO W95-PC.
           IF W95-ADD3 = SPACES
               MOVE W95-ADD4     TO W95-ADD3
               MOVE SPACES       TO W95-ADD4.
           IF W95-ADD2 = SPACES
               MOVE W95-ADD3     TO W95-ADD2
               MOVE SPACES       TO W95-ADD3.
           IF W95-ADD1 = SPACES
               MOVE W95-ADD2     TO W95-ADD1
               MOVE SPACES       TO W95-ADD2.
             MOVE 5              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-DISCOUNT  TO W15-DISCOUNT.
      MOVE 1   TO W05-V-RATE.

       ZA05-VAT.
      MOVE W05-VAT-CODE  TO DPT-CODE.
      PERFORM READ-DEPART THRU READ-DEPART-EXIT.
      MOVE DPT-R-DATE  TO WS-VAT-DATE(W05-V-RATE).
      MOVE DPT-RATE  TO W05-VAT(W05-V-RATE).
      ADD 6 W05-V-RATE  GIVING WS-S1.
      MOVE DPT-P-RATE  TO W05-VAT(WS-S1).
    IF W05-V-RATE < 6
        ADD 1   TO W05-V-RATE
        GO TO ZA05-VAT.
            MOVE 6   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF PAR-INTEGRATE NOT = "Y"
               MOVE "N"          TO WX-INT
           ELSE
               MOVE "Y"          TO WX-INT.
             MOVE PAR-DEBGL      TO WX-DEBGL.
             MOVE PAR-CREGL      TO WX-CREGL.
             MOVE PAR-INTGL      TO WX-INTGL.
             MOVE PAR-BNKGL      TO WX-BNKGL.
             MOVE PAR-UNPROF     TO WX-UNPROF.
             MOVE PAR-REDGL      TO WX-REDGL.
             MOVE PAR-ADJGL      TO WX-ADJGL.
             MOVE PAR-RLGL       TO WX-RLGL.
             MOVE PAR-DSGL       TO WX-DSGL.
            GO TO ZA999.

       COPY "ZA49.PRO".

       ZA50.
           IF WS-F-ERROR = 5
               MOVE "DEBTOR TRANS"
                                 TO WS-FILE
           ELSE
           IF WS-F-ERROR = 6
               MOVE "DEBTOR file"
                                 TO WS-FILE
           ELSE
           IF WS-F-ERROR = 9
               MOVE "GUARANTEE file"
                                 TO WS-FILE
           ELSE
           IF WS-F-ERROR = 12
               MOVE "INVOICE file"
                                 TO WS-FILE
           ELSE
           IF WS-F-ERROR = 20
               MOVE "SALES ORDER" TO WS-FILE
           ELSE
           IF WS-F-ERROR = 22
               MOVE "STOCK file" TO WS-FILE
           ELSE
           IF WS-F-ERROR = 32
               MOVE "TAX file"   TO WS-FILE
           ELSE
           IF WS-F-ERROR = 40
               MOVE "INTEGRATION"
                                 TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
        WS-FILE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Press any key to continue" AT 1012.
      ACCEPT WS-OPTION AT 1038
      WITH FOREGROUND-COLOR Grey HIGHLIGHT.

       ZA51.
      EXIT PROGRAM.

       ZA200.

       COPY "LOCKED.PRO".

       ZA205.
            EXIT PROGRAM.

       ZA999.
             EXIT.

       I-O-ERRORS      SECTION 91.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
           IF WS-F-ERROR = 1
               MOVE W02-AUDITF TO WS-FILE
               MOVE WS-AUDKEY TO WS-KEY
           ELSE
           IF WS-F-ERROR = 2
               MOVE W02-NETWORK TO WS-FILE
               MOVE WS-NETKEY TO WS-KEY
           ELSE
           IF WS-F-ERROR = 5
               MOVE W02-DBTRAN   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE TRA-KEY      TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 6
               MOVE W02-DEBTOR   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 7
              MOVE W02-DEPART  TO WS-FILE
              MOVE ZERO  TO WS-KEY
              MOVE DPT-CODE  TO WS-KEYX
    ELSE
           IF WS-F-ERROR = 9
               MOVE W02-GARTEE   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE GUA-KEY      TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 12
               MOVE W02-INVOIC   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE DOC-KEY      TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 15
               MOVE WS-PARID     TO WS-FILE
               MOVE WS-PARKEY    TO WS-KEY
           ELSE
           IF WS-F-ERROR = 18
               MOVE W02-RECOVER  TO WS-FILE
               MOVE WS-RECKEY    TO WS-KEY
           ELSE
           IF WS-F-ERROR = 19
               MOVE W02-SALE TO WS-FILE
               MOVE WS-SALKEY TO WS-KEY
           ELSE
           IF WS-F-ERROR = 20
               MOVE W02-SORDER   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE SOR-KEY      TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 22
               MOVE W02-STOCKF   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 32
               MOVE W02-TXTRAN   TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE TAX-KEY  TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 37
              MOVE W02-SHARED  TO WS-FILE
              MOVE WS-SHARED  TO WS-KEY
          ELSE
          IF WS-F-ERROR = 40
               MOVE W02-LEDTRF   TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE XFR-KEY  TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 43
              MOVE W02-CARDEX  TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE CRD-KEY  TO WS-KEYX.

       COPY "DISPERR.PRO".
