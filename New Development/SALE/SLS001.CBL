      $set LINKCOUNT"712" GNT"SLS001.GNT"
      ******************************************************************
      *                                                                *
      *    ******   **         ******      ****      ****       **     *
      *   **    **  **        **    **    **  **    **  **     ***     *
      *   **        **        **         **    **  **    **     **     *
      *    ******   **         ******    **    **  **    **     **     *
      *         **  **              **   **    **  **    **     **     *
      *   **    **  **        **    **    **  **    **  **      **     *
      *    ******   ********   ******      ****      ****     ******   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *      S A L E S   O R D E R S   P R O G R A M                   *
      *                                                                *
      *       Version 9.04.05 - June 2018                              *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  Feb 2007     - New file (DEBDEP/AR_REST.DAT) - Debtor sales   *
      *                 restrictions file has been included which      *
      *                 will allow for departments to be excluded or   *
      *                 only selected departments allowed. This will   *
      *                 allow the user the abillity to allow or bar    *
      *                 sales of selected departments.                 *
      *                                                                *
      * August 2008   - New file (STKALT) - Stock alternate index      *
      *                 included for lookups, using any word con-      *
      *                 tained in the Stock description.               *
      *                                                                *
      * November 2009 - Include words from Description 2 and from      *
      *                 the Item code (Some item codes are comprised   *
      *                 of individual words and these will be          *
      *                 included in the alternate Index).              *
      *                                                                *
      *  Jan 2010 - Allow for two discounts for a Debtor on the same   *
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
       PROGRAM-ID.     SLS001.
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

       COPY "DEBALT.SL".

       COPY "DEBDEP.SL".

       COPY "DEBTOR.SL".

       COPY "DEPART.SL".

       COPY "PARAM.SL".

       COPY "RECOVER.SL".

       COPY "SALE.SL".

       COPY "SHARED.SL".

       COPY "SORDER.SL".

       COPY "SPARTS.SL".

       COPY "STKMEM.SL".

       COPY "STOCK.SL".

       COPY "SYSUSE.SL".

           SELECT PRNDSK  ASSIGN W02-PRNDISK
                          ORGANIZATION LINE SEQUENTIAL.

           SELECT PRNREP  ASSIGN W02-PRINTER
                          ORGANIZATION LINE SEQUENTIAL.

      /
       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "DEBALT.FDE".

       COPY "DEBDEP.FDE".

       COPY "DEBTOR.FDE".

       COPY "DEPART.FDE".

       COPY "PARAM.FDE".

       COPY "RECOVER.FD".

       COPY "SALE.FD".

       COPY "SHARED.FDE".

       COPY "SORDER.FDE".

       COPY "SPARTS.FDE".

       COPY "STKMEM.FDE".

       COPY "STOCK.FDE".

       COPY "SYSUSE.FDE".

       FD  PRNDSK    LABEL RECORD OMITTED
                     LINAGE IS WS-PGE-LENGTH.
       01  REP-L1.
           03  REP-PG.
               05  REP-LN          PIC  9(04).
           03  REP-DETAIL2         PIC  X(96).

       01  REP-L2.
           03  FILLER              PIC  X(04).
           03  REP-DETAIL1         PIC  X(80).

       01  REP-L3.
           03  FILLER              PIC  X(04).
           03  REP-AC              PIC  X(10).
           03  REP-ACNO            PIC  X(06).
           03  FILLER              PIC  X(01).
           03  REP-NAME            PIC  X(40).
           03  FILLER              PIC  X(06).
           03  REP-DATE            PIC  X(06).
           03  REP-TIME.
               05  REP-DTE         PIC  Z9/99/9999.

       01  REP-L3A.
           03  FILLER              PIC  X(04).
           03  FILLER              PIC  X(10).
           03  REP-TEL             PIC  X(14).
           03  REP-CNTCT           PIC  X(10).
           03  REP-CONTACT         PIC  X(30).
           03  FILLER              PIC  X(32).

       01  REP-L4.
           03  FILLER              PIC  X(04).
           03  REP-H1              PIC  X(29).
           03  REP-REF             PIC  9(08)/99.
           03  FILLER              PIC  X(40).

       01  REP-L5.
           03  FILLER              PIC  X(04).
           03  REP-H2              PIC  X(20).
           03  REP-H3              PIC  X(32).
           03  REP-H4              PIC  X(12).
           03  REP-H5              PIC  X(16).

       01  REP-L6.
           03  FILLER              PIC  X(04).
           03  REP-EXT-ITEM.
               05  REP-ITM1        PIC  X(14).
               05  REP-SL          PIC  X(01).
               05  REP-ITM         PIC  X(03).
           03  FILLER              PIC  X(02).
           03  REP-DESC            PIC  X(30).
           03  FILLER              PIC  X(02).
           03  REP-QTY             PIC  Z(05)9.999 BLANK WHEN ZERO.
           03  FILLER              PIC  X(02).
           03  REP-PRICE           PIC  Z(06)9.999.
           03  FILLER              PIC  X(05).

       01  REP-L7.
           03  FILLER              PIC  X(24).
           03  REP-EXT-ITEM2.
               05  REP-ITM2        PIC  X(14).
               05  REP-SL2         PIC  X(01).
               05  REP-ITM3        PIC  X(03).
           03  FILLER              PIC  X(01).
           03  REP-DSCR            PIC  X(30).
           03  FILLER              PIC  X(01).
           03  REP-QNT             PIC  Z(05)9.999.
           03  FILLER              PIC  X(12).

       FD  PRNREP    LABEL RECORD OMITTED
             LINAGE IS WS-PGE-LENGTH.
       01  PRN-L1.
           03  PRN-DETAIL2         PIC X(96).
       01  PRN-L2.
           03  PRN-DETAIL1         PIC X(80).

      /
       WORKING-STORAGE SECTION.
       77  WS-CHECK                PIC  X(18)    VALUE "aeWlimemnomLalismJ".
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
       77  WS-DBTKEY               PIC  9(06)    COMP-5.
       77  WS-SALSTR               PIC  9(04)    COMP-5.
       77  WS-SALKEY               PIC  9(04)    COMP-5.
       77  WS-ENQPOS               PIC  9(06)    COMP-5.
       77  WS-ENQEND               PIC  9(06)    COMP-5.
       77  WS-NETKEY               PIC  9(04)    COMP-5.
       77  WS-AUDKEY               PIC  9(06)    COMP-5.
       77  WS-RECKEY               PIC  9(06)    COMP-5.
       77  WS-RECOVER              PIC  9(06)    COMP-5.
       77  WS-SHARED               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-TRANS                PIC  9(06)    COMP-5 VALUE 1.
       77  WS-PRN1                 PIC  9(01)    VALUE 0.
       77  WS-PRN2                 PIC  9(01)    VALUE 0.
       77  WS-USE-PACKS            PIC  X(01).
       77  WS-USE-ITM              PIC  X(01).
       77  WS-EXT-STK              PIC  X(01).
       77  WS-PIC-PRC              PIC  X(01).
       77  WS-START                PIC  9(08).
       77  WS-END                  PIC  9(08).
       77  WS-TME2                 PIC  9(08).
       77  WS-PORT                 PIC  9(05)    VALUE 1004.
       77  WS-OPEN                 PIC  X(01)    VALUE X"01".
       77  WS-CLOSE                PIC  X(01)    VALUE X"00".
       77  WS-PAR                  PIC  9(01)    VALUE 0.
       77  WS-INO                  PIC  9(03)    VALUE 0.
       77  WS-LINES                PIC  9(02)    VALUE 0.
       77  WS-FIRST                PIC  9(03)    VALUE 0.
       77  WS-SALPOS               PIC  9(03)    VALUE 0.
       77  WS-LAST                 PIC  9(03)    VALUE 0.
       77  WS-LASTINV              PIC  9(04)    COMP-5.
       77  WS-LINE                 PIC  9(04)    COMP-5.
       77  WS-PAGE                 PIC  9(02)    COMP-5.
       77  WS-L1                   PIC  9(04)    COMP-5.
       77  WS-PGE-LENGTH           PIC  9(02)    VALUE 66.
       77  WS-PRN-LENGTH           PIC  9(02)    VALUE 62.
       77  WS-COND                 PIC  X(06).
       77  WS-NORM                 PIC  X(06).
       77  WS-EXPP                 PIC  X(06).
       77  WS-ECAN                 PIC  X(06).
       77  WS-8LPI                 PIC  X(06).
       77  WS-6LPI                 PIC  X(06).
       77  WS-10CPI                PIC  X(06).
       77  WS-12CPI                PIC  X(06).
       77  WS-17CPI                PIC  X(06).
       77  WS-OPTION               PIC  X(01).
       77  WS-PGADV                PIC  X(01)    VALUE "Y".
       77  WS-SKIP                 PIC  X(01)    VALUE " ".
       77  WS-LOOK                 PIC  X(01)    VALUE "S".
       77  WS-ETYPE                PIC  X(01)    VALUE "I".
       77  WS-IN-OPT               PIC  X(01)    VALUE "N".
       77  WS-NUM                  PIC  Z9.
       77  WS-ERROR                PIC  9(01).
       77  WS-PRN-NO               PIC  X(01)    COMP-X VALUE 9.
       77  WS-PRN-STAT             PIC  X(01)    COMP-X.
       77  WS-COMP                 PIC  9(01).
       77  WS-UPDATE               PIC  9(01)    VALUE ZERO.
       77  WS-CONREC               PIC  9(01)    VALUE ZERO.
       77  WS-DELIV                PIC  X(01).
       77  WS-ITMPRC               PIC  X(01).
       77  WS-RT-HD                PIC  X(10)    VALUE "-Retail   ".
       77  WS-WS-HD                PIC  X(10)    VALUE "-Wholesale".
       77  WS-CS-HD                PIC  X(10)    VALUE "-Cash sale".
       77  WS-TYP                  PIC  X(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-AGE                  PIC  X(01).
       77  WS-PRC                  PIC  X(01).
       77  WS-RESTRICT             PIC  X(01).
       77  WS-RESTCODE             PIC  X(01).
       77  WS-MEASURE              PIC  X(01)    VALUE "N".
       77  PRG-NAME                PIC  X(12)    VALUE SPACES.
       77  PRG-DEBT-LUP            PIC  X(12)    VALUE "DTP\DTPLOOK".
       77  PRG-SORD-LUP            PIC  X(12)    VALUE "SLP\SORLOOK".
       77  PRG-STOCK-LUP           PIC  X(12)    VALUE "STP\STPLOOK".
       77  WS-A                    PIC  X(19)    VALUE "Any key to continue".
       77  WS-E                    PIC  X(15)    VALUE "**END** - ENTER".
       77  WS-ERR1                 PIC  X(22)    VALUE "Invalid Account Number".
       77  WS-ERR2                 PIC  X(09)    VALUE "No Record".
       77  WS-PER                  PIC  Z(02)9.99.
       77  WS-CR                   PIC  X(01)    VALUE "I".
       77  WS-TAXRES               PIC S9(07)V9(08) COMP-3.
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.
      /

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
           03  CRT-PROGRAM         PIC  X(15) VALUE "SLS001".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "SALES - ADD/CHANGE/DELETE AN ORDER".
      *
      *    Company Name
      *
           03  CRT-COMPANY         PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-WRKHD           PIC  X(11).
           03  CRT-WRKST           PIC  X(03).

       01  WS-DB-LINE.
           03  WS-TOP-LINE.
               05  WS-TCHAR        PIC  X(80) VALUE "ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍµ".
           03  WS-TOP-LNE2.
               05  WS-TCR          PIC  X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER          PIC  X(01).
               05  WS-TOP-COMP     PIC  X(40).
               05  FILLER          PIC  X(23).
               05  WS-WRKHD        PIC  X(11).
               05  FILLER          PIC  X(01).
               05  WS-WRKST        PIC  X(03).
               05  FILLER          PIC  X(01).
           03  WS-MID-LINE.
               05  WS-MCHAR        PIC  X(80) VALUE "³              ³   ³                             ³        ³         ³          ³".
           03  WS-MINV REDEFINES WS-MID-LINE.
               05  WS-INV1         PIC  X(01).
               05  WS-INV78        PIC  X(78).
               05  WS-INV80        PIC  X(01).
           03  WS-MID-LNE2.
               05  FILLER          PIC  X(01) VALUE "³".
               05  WS-BLNK78       PIC  X(78) VALUE ALL "°".
               05  FILLER          PIC  X(01) VALUE "³".
           03  WS-BOT-LINE.
               05  WS-BCHAR        PIC  X(80) VALUE "ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍµ".
           03  WS-BOT-LNE2.
               05  WS-BCR          PIC  X(80) VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
           03  WS-B-LINE REDEFINES WS-BOT-LNE2.
               05  FILLER          PIC  X(01).
               05  WS-MEM-LINE.
                   07  WS-SRT-LINE PIC  X(50).
                   07  FILLER      PIC  X(10).
               05  FILLER          PIC  X(19).
           03  WS-MM-LINE.
               05  FILLER          PIC  X(01) VALUE "³".
               05  WS-SPACE        PIC  X(60) VALUE SPACES.
               05  FILLER          PIC  X(01) VALUE "³".
           03  WS-HME-LINE         PIC  X(60) VALUE ALL "Í".
           03  WS-TOP-LNE.
               05  WS-TCHR         PIC  X(80) VALUE "ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍµ".
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER          PIC  X(01).
               05  WS-H-LINE       PIC  X(78).
               05  FILLER          PIC  X(01).
           03  WS-MID-LNE.
               05  WS-MCHR         PIC  X(80) VALUE "³                ³                              ³          ³         ³         ³".
           03  WS-MST-LNE REDEFINES WS-MID-LNE.
               05  WS-MST1         PIC  X(01).
               05  WS-MST78        PIC  X(78).
               05  WS-MST80        PIC  X(01).
           03  WS-BOT-LNE.
               05  WS-BCHR         PIC  X(80) VALUE "ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍµ".
           03  WS-TP-LNE.
               05  WS-TCH          PIC  X(80) VALUE "ÆÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍµ".
           03  WS-MD-LNE.
               05  WS-MCH          PIC  X(80) VALUE "³      ³                                        ³           ³          ³       ³".
           03  WS-MDDT-LNE REDEFINES WS-MD-LNE.
               05  WS-MDT1         PIC  X(01).
               05  WS-MDT78        PIC  X(78).
               05  WS-MDT80        PIC  X(01).
           03  WS-BT-LNE.
               05  WS-BCH          PIC  X(80) VALUE "ÆÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍµ".
           03  WS-TP-LINE.
               05  WS-TCA          PIC  X(01) OCCURS 80.
           03  WS-MD-LINE.
               05  WS-MCA          PIC  X(01) OCCURS 80.
           03  WS-SO78-L REDEFINES WS-MD-LINE.
               05  WS-MSO1         PIC  X(01).
               05  WS-MSO78        PIC  X(78).
               05  WS-MSO80        PIC  X(01).
           03  WS-BT-LINE.
               05  WS-BCA          PIC  X(01) OCCURS 80.

       01  WS-ENTRY.
           03  WS-ENT0             PIC  X(08).
           03  WS-TYPE0            PIC  X(01).
           03  WS-KEY0             PIC  9(06)    COMP-5.
       01  WS-ENTRY1.
           03  WS-ENT1             PIC  X(08).
           03  WS-TYPE1            PIC  X(01).
           03  WS-KEY1             PIC  9(06)    COMP-5.

       COPY "WS.WS".

       01  WS-HELP.
           03  WS-MODULE           PIC  X(03) VALUE "SLS".
           03  WS-PROG             PIC  X(03) VALUE "001".

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

       01  WS-LOCAL-ID.
           03  WS-LOCAL-DRIVE      PIC  X(01).
           03  FILLER              PIC  X(14) VALUE ":\APACSHR\SORD".
           03  WS-USER-ID          PIC  X(03).
           03  FILLER              PIC  X(04) VALUE ".ORD".

       01  WS-LOCAL-ID2.
           03  WS-LOCAL-DRIVE2     PIC  X(01).
           03  FILLER              PIC  X(19) VALUE ":\APACSHR\USERS.DAT".

       01  WS-SORDER-STR.
           03  FILLER              PIC  X(64).

       01  W00-TRAN-CODES.
           03  W00-T-VAT           PIC S9(09)V99 COMP-3.
           03  W00-E-DESC          PIC  X(12).
           03  W00-A-DESC          PIC  X(12).
           03  W00-ACTION          PIC  9(01).
           03  W00-T-CODE          PIC  9(02)    COMP-5.
           03  W00-T-GL            PIC  9(06)    COMP-3.
           03  W00-T-DAY           PIC  9(06)    COMP-5.
           03  W00-T-VAL           PIC S9(09)V99 COMP-3.
           03  W00-T-MTD           PIC  9(06)    COMP-5.
           03  W00-T-MTDV          PIC S9(09)V99 COMP-3.
           03  W00-T-YTD           PIC  9(06)    COMP-5.
           03  W00-T-YTDV          PIC S9(09)V99 COMP-3.

       01  W02-FID.

       COPY "PRNDISK.ID".

       01  W02-FILE-IDS.
           03  W02-PRINTER.
               05  W02-REPORT      PIC  X(07) VALUE "SLS001.".
               05  W02-USER        PIC  X(05) VALUE SPACES.

       COPY "W05.VAT".

       COPY "W10.SAL".

       01  W11-PARTS.
           03  W11-PART-DETAILS OCCURS 30.
               05  W11-CODE        PIC  X(14).
               05  W11-DESC        PIC  X(30).
               05  W11-QUANT       PIC S9(05)V9999 COMP-3.

       COPY "W12.WS".

       01  W14-ORDER.
           03  W14-REF             PIC  Z(08).
           03  W14-AC              PIC  X(06).
           03  W14-NAME.
               05  W14-NME         PIC  X(27).
               05  FILLER          PIC  X(13).
           03  W14-DATE            PIC  Z9/99/9999.
           03  W14-PS              PIC  9(02).
           03  W14-DOC             PIC  X(01).
           03  W14-DDOC.
               05  W14-INV         PIC  Z(08).
           03  W14-VALUE           PIC  Z(07)9.99.

       01  W14-DELETE.
           03  W14-SUB             PIC  9(03)     COMP-3.
           03  W14-SQN             PIC  9(05)     COMP-3 OCCURS 200.

       01  W15-DISCOUNT.
           03  W15-DISC            PIC S9(03)V99  COMP-3 OCCURS 10.

       01  W15-PRICE-NAMES.
           03  W15-PNAME.
               05  W15-CHECK       PIC  X(01).
               05  W15-REST        PIC  X(09).

       01  W15-ALLOCATIONS.
           03  W15-DETAIL.
               05  W15-TRAN OCCURS 15.
                   07  W15-VALUE   PIC  Z(06)9.99- BLANK WHEN ZERO.
                   07  W15-ALLOC   PIC  Z(06)9.99- BLANK WHEN ZERO.
           03  W15-DISPLAY                        REDEFINES W15-DETAIL.
               05  W15-BAL1        PIC  X(11).
               05  W15-ALL1        PIC  X(11).
               05  W15-BAL2        PIC  X(11).
               05  W15-ALL2        PIC  X(11).
               05  W15-BAL3        PIC  X(11).
               05  W15-ALL3        PIC  X(11).
               05  W15-BAL4        PIC  X(11).
               05  W15-ALL4        PIC  X(11).
               05  W15-BAL5        PIC  X(11).
               05  W15-ALL5        PIC  X(11).
               05  W15-BAL6        PIC  X(11).
               05  W15-ALL6        PIC  X(11).
               05  W15-BAL7        PIC  X(11).
               05  W15-ALL7        PIC  X(11).
               05  W15-BAL8        PIC  X(11).
               05  W15-ALL8        PIC  X(11).
               05  W15-BAL9        PIC  X(11).
               05  W15-ALL9        PIC  X(11).
               05  W15-BAL10       PIC  X(11).
               05  W15-ALL10       PIC  X(11).
               05  W15-BAL11       PIC  X(11).
               05  W15-ALL11       PIC  X(11).
               05  W15-BAL12       PIC  X(11).
               05  W15-ALL12       PIC  X(11).
               05  W15-BAL13       PIC  X(11).
               05  W15-ALL13       PIC  X(11).
               05  W15-BAL14       PIC  X(11).
               05  W15-ALL14       PIC  X(11).
               05  W15-BAL15       PIC  X(11).
               05  W15-ALL15       PIC  X(11).

       COPY "W20.WS".

       01  W25-CALCS.
           03  W25-RESULT          PIC  9(05)V99.
           03  W25-RESULT1                        REDEFINES W25-RESULT.
               05  W25-WHOLE       PIC  9(05).
               05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2                        REDEFINES W25-RESULT.
               05  W25-KEY         PIC  9(04).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).
           03  W25-TOTAL           PIC  9(07)V99  COMP-3.
           03  W25-VALUE           PIC S9(07)V99  COMP-3.
           03  W25-CREDIT          PIC S9(07)V99  COMP-3.
           03  W25-TIME            PIC  9(08).
           03  W25-TRED                           REDEFINES  W25-TIME.
               05  W25-HH          PIC  9(02).
               05  W25-MM          PIC  9(02).
               05  W25-SS          PIC  9(02).
               05  FILLER          PIC  9(02).

       01  W32-MESSAGE.
           03  W32-REMARKS         PIC  X(63) OCCURS 10.
       01  W34-HEADER.
           03  W34-FORMAT          PIC  X(01).
           03  W34-MARGIN          PIC  9(02) COMP-5.
           03  W34-SALUTATION      PIC  X(63).

       01  W35-DETKEY.
           03  W35-KEY.
               05  W35-REF         PIC  X(08).
               05  W35-SEQN.
                   07  W35-SEQ     PIC  9(03).
                   07  W35-SUB     PIC  9(02).
               05  W35-SQN REDEFINES W35-SEQN PIC  9(05).

       COPY "W40.DBT".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W60-NME-ADD.
           03  W60-NAME.
               05  W60-NCHAR       PIC  X(01) OCCURS 40.
           03  W60-ADDRESS.
               05  W60-ACHAR       PIC  X(01) OCCURS 70.
           03  W60-ADDR                       REDEFINES W60-ADDRESS.
               05  W60-ADD         PIC  X(60).
               05  FILLER          PIC  X(10).
           03  W60-ADDRESS1.
               05  W60-PAD         PIC  X(60).
               05  FILLER          PIC  X(10).

       COPY "W70DEBT.IWS".

       01  W75-KEYS.
           03  W75-S               PIC  9(02) COMP-5.
           03  W75-S1              PIC  9(02) COMP-5.
           03  W75-S2              PIC  9(02) COMP-5.
           03  W75-DNO             PIC  X(06) OCCURS 18.
           03  W75-SORD            PIC  9(08) OCCURS 18.

       COPY "W75.WS".

       01  W80-ACCOUNT.
           03  W80-ACNO            PIC  X(06).
           03  W80-CRED                       REDEFINES W80-ACNO.
               05  W80-CNO         PIC 9(05).
               05  W80-CAC                    REDEFINES W80-CNO.
                   07  W80-CTYPE   PIC 9(01).
                   07  W80-SER     PIC 9(04).
               05  FILLER          PIC 9(01).
           03  W80-TYPE            PIC 9(01).

       01  W85-PASS.
           03  W85-SUPER           PIC  X(06)    OCCURS 9.
           03  W85-ENTRY           PIC  9(02)    COMP-3.
           03  W85-MARG            PIC S9(02)V99.

       01  W85-BATCHES.
               05  W85-BATCH       PIC  9(05).

       COPY "W90.SAL".

       01  W95-STM.
           03  W95-COMP            PIC  X(40).
           03  W95-ADD1            PIC  X(30).
           03  W95-ADD2            PIC  X(30).
           03  W95-ADD3            PIC  X(30).
           03  W95-ADD4            PIC  X(30).
           03  W95-PC.
               05  W95-POST        PIC  9(04).
           03  W95-TEL             PIC  X(14).
           03  W95-V1.
               05  W95-T           PIC  Z(04)9.99.
           03  W95-V2.
               05  W95-B           PIC  Z(06)9.99-.
           03  W95-D.
              05  W95-DTE          PIC  Z9/99/9999.
           03  W95-TIME.
               05  W95-HH          PIC  99.
               05  FILLER          PIC  X(01) VALUE ":".
               05  W95-MM          PIC  99.
               05  FILLER          PIC  X(01) VALUE ":".
               05  W95-SS          PIC  99.
       01  W100-EDIT.
           03  W100-LBAL           PIC  Z(06)9.99-.
           03  W100-LNGTH                        REDEFINES W100-LBAL.
               05  FILLER          PIC  X(02).
               05  W100-LENGTH     PIC  ZZZ9.9999.
           03  W100-LARR           PIC  Z(06)9.99.
           03  W100-BRDTH                        REDEFINES W100-LARR.
               05  FILLER          PIC  X(01).
               05  W100-BREADTH    PIC  ZZZ9.9999.
           03  W100-BAL            PIC  Z(08)9.99-.
           03  W100-CUR            PIC  Z(08)9.99-.
           03  W100-30             PIC  Z(08)9.99-.
           03  W100-60             PIC  Z(08)9.99-.
           03  W100-90             PIC  Z(08)9.99-.
           03  W100-120            PIC  Z(08)9.99-.
           03  W100-INT            PIC  Z(08)9.99-.
           03  W100-CRL            PIC  Z(06)9.
           03  W100-NMB                          REDEFINES W100-CRL.
               05  FILLER          PIC  X(02).
               05  W100-NUMBER     PIC  ZZZ9.
           03  W100-V2.
               05  W100-S6V2       PIC  Z(05)9.99-.
           03  W100-V3                           REDEFINES W100-V2.
               05  W100-PRICE      PIC  X(09).
               05  FILLER          PIC  X(01).
           03  W100-V4                           REDEFINES W100-V2.
               05  W100-EXT        PIC  Z(06)9.99.
           03  W100-V5                           REDEFINES W100-V2.
               05  W100-QNT        PIC  Z(05)9.99-.

       LINKAGE SECTION.

       COPY "CHAIN.LS".

       COPY "FILE-DEBT.IDS".

      /
       SCREEN SECTION.

       01  S02.
           03  LINE 2  COLUMN 27 VALUE "SALES/ORDER PROGRAM CONTROL" FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S07.
           03        COLUMN  2 PIC X(14) USING W10-ITEM FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Black AUTO.

       01  S07-ITM.
           03        COLUMN  2 PIC X(14) USING W10-ITEM FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Black AUTO.
           03        COLUMN 17 PIC X(03) USING W10-ITM  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Black AUTO.

       01  S07-EXT.
           03        COLUMN  2 PIC X(18) USING W10-EXT-ITEM FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Black AUTO.

       01  S07-3.
           03        COLUMN  2 PIC Z(03) USING W10-I4   FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Black AUTO.

       01  S07-4.
           03        COLUMN  2 PIC Z(04) USING W10-I4   FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Black AUTO.

       01  S07-5.
           03        COLUMN  2 PIC Z(05) USING W10-I5   FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Black AUTO.

       01  S07-6.
           03        COLUMN  2 PIC Z(06) USING W10-I6   FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Black AUTO.

       01  S07-7.
           03        COLUMN  2 PIC Z(07) USING W10-I7   FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Black AUTO.

       01  S07-8.
           03        COLUMN  2 PIC Z(08) USING W10-I8   FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Black AUTO.

       01  S07-9.
           03        COLUMN  2 PIC Z(09) USING W10-I9   FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Black AUTO.

       01  S07-10.
           03        COLUMN  2 PIC Z(10) USING W10-I10  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Black AUTO.

       01  S07-11.
           03        COLUMN  2 PIC Z(11) USING W10-I11  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Black AUTO.

       01  S07-12.
           03        COLUMN  2 PIC Z(12) USING W10-I12  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Black AUTO.

       01  S07-13.
           03        COLUMN  2 PIC Z(13) USING W10-I13  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Black AUTO.

       01  S07-14.
           03        COLUMN  2 PIC Z(14) USING W10-I14  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Black AUTO.

       COPY "SORLUP.CRT".

       COPY "DEBLUP.CRT".

       01  S25.
           02  BACKGROUND-COLOR Green.
           03  LINE 11 COLUMN 15 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" FOREGROUND-COLOR Brown HIGHLIGHT.
           03  LINE 12 COLUMN 15 VALUE "³Addresses:                 "                                FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 49 VALUE "  Balance  "                                                 FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 60                      PIC Z(08)9.99- USING DEB-BAL                   FOREGROUND-COLOR Grey  HIGHLIGHT.
           03          COLUMN 73 VALUE "³"                                                           FOREGROUND-COLOR Brown HIGHLIGHT.
           03  LINE 13 COLUMN 15 VALUE "³ 1 "                                                        FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 19                      PIC  X(30) USING W40-ADD1                      FOREGROUND-COLOR Grey  HIGHLIGHT.
           03          COLUMN 49 VALUE "  MTD      "                                                 FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 60                      PIC Z(08)9.99- USING W100-CUR                  FOREGROUND-COLOR Grey  HIGHLIGHT.
           03          COLUMN 73 VALUE "³"                                                           FOREGROUND-COLOR Brown HIGHLIGHT.
           03  LINE 14 COLUMN 15 VALUE "³   "                                                        FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 19                      PIC  X(30) USING W40-ADD2                      FOREGROUND-COLOR Grey  HIGHLIGHT.
           03          COLUMN 49 VALUE "  Current  "                                                 FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 60                      PIC Z(08)9.99- USING DEB-CUR                   FOREGROUND-COLOR Grey  HIGHLIGHT.
           03          COLUMN 73 VALUE "³"                                                           FOREGROUND-COLOR Brown HIGHLIGHT.
           03  LINE 15 COLUMN 15 VALUE "³   "                                                        FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 19                      PIC  X(30) USING W40-ADD3                      FOREGROUND-COLOR Grey  HIGHLIGHT.
           03          COLUMN 49 VALUE "   30 Days "                                                 FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 60                      PIC Z(08)9.99- USING DEB-30                    FOREGROUND-COLOR Grey  HIGHLIGHT.
           03          COLUMN 73 VALUE "³"                                                           FOREGROUND-COLOR Brown HIGHLIGHT.
           03  LINE 16 COLUMN 15 VALUE "³   "                                                        FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 19                      PIC  X(30) USING W40-ADD4                      FOREGROUND-COLOR Grey  HIGHLIGHT.
           03          COLUMN 49 VALUE "   60 Days "                                                 FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 60                      PIC Z(08)9.99- USING DEB-60                    FOREGROUND-COLOR Grey  HIGHLIGHT.
           03          COLUMN 73 VALUE "³"                                                           FOREGROUND-COLOR Brown HIGHLIGHT.
           03  LINE 17 COLUMN 15 VALUE "³   "                                                        FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 19                      PIC  X(30) USING W40-PC1                       FOREGROUND-COLOR Grey  HIGHLIGHT.
           03          COLUMN 49 VALUE "   90 Days "                                                 FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 60                      PIC Z(08)9.99- USING DEB-90                    FOREGROUND-COLOR Grey  HIGHLIGHT.
           03          COLUMN 73 VALUE "³"                                                           FOREGROUND-COLOR Brown HIGHLIGHT.
           03  LINE 18 COLUMN 15 VALUE "³ 2 "                                                        FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 19                      PIC  X(30) USING W40-PADD1                     FOREGROUND-COLOR Grey  HIGHLIGHT.
           03          COLUMN 49 VALUE "  120 Days "                                                 FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 60                      PIC Z(08)9.99- USING DEB-120                   FOREGROUND-COLOR Grey  HIGHLIGHT.
           03          COLUMN 73 VALUE "³"                                                           FOREGROUND-COLOR Brown HIGHLIGHT.
           03  LINE 19 COLUMN 15 VALUE "³   "                                                        FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 19                      PIC  X(30) USING W40-PADD2                     FOREGROUND-COLOR Grey  HIGHLIGHT.
           03          COLUMN 49 VALUE "  Interest "                                                 FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 60                      PIC Z(08)9.99- USING DEB-INT                   FOREGROUND-COLOR Grey  HIGHLIGHT.
           03          COLUMN 73 VALUE "³"                                                           FOREGROUND-COLOR Brown HIGHLIGHT.
           03  LINE 20 COLUMN 15 VALUE "³   "                                                        FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 19                      PIC  X(30) USING W40-PADD3                     FOREGROUND-COLOR Grey  HIGHLIGHT.
           03          COLUMN 49 VALUE "  Cr Limit "                                                 FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 60                      PIC Z(08)9 USING DEB-CRL                       FOREGROUND-COLOR Grey  HIGHLIGHT.
           03          COLUMN 69 VALUE "    ³"                                                       FOREGROUND-COLOR Brown HIGHLIGHT.
           03  LINE 21 COLUMN 15 VALUE "³   "                                                        FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 19                      PIC  X(30) USING W40-PADD4                     FOREGROUND-COLOR Grey  HIGHLIGHT.
           03          COLUMN 49 VALUE "  Last Pmt "                                                 FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 60                      PIC 9999/99/99 USING DEB-PMT                   FOREGROUND-COLOR Grey  HIGHLIGHT.
           03          COLUMN 70 VALUE "   ³"                                                        FOREGROUND-COLOR Brown HIGHLIGHT.
           03  LINE 22 COLUMN 15 VALUE "³   "                                                        FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 19                      PIC  X(30) USING W40-PC2                       FOREGROUND-COLOR Grey  HIGHLIGHT.
           03          COLUMN 49 VALUE "       Amt "                                                 FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 60                      PIC Z(08)9.99- USING DEB-LAST-PMT              FOREGROUND-COLOR Grey  HIGHLIGHT.
           03          COLUMN 73 VALUE "³"                                                           FOREGROUND-COLOR Brown HIGHLIGHT.
           03  LINE 23 COLUMN 15 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" FOREGROUND-COLOR Brown HIGHLIGHT.

       01  S26-QNT.
           03          COLUMN 51 PIC Z(04)9.99  USING W10-QUANT FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Black   AUTO.

       01  S26-PRC.
           03          COLUMN 60 PIC Z(05)9.99  USING W10-SELL  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Black   AUTO.

       01  S26-DSC.
           03          COLUMN 30 PIC Z9.99      USING WS-DISC   FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.

       01  S26-TDSC.
           03          COLUMN 15 PIC Z9.99      USING WS-TDISC  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Black   AUTO.

       01  S26-AVGE.
           03          COLUMN 28 PIC Z(04)9.999 USING STK-AVGE  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.

       01  S28.
           03  LINE 25 COLUMN 65 PIC ZZZ9       USING WS-SALKEY FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.

       COPY "SORDER.CRT".

       01  S30A.
           03  PIC Z(08) USING W10-SORD FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.

       01  S31.
           03  LINE  2 COLUMN 32 VALUE "AMEND SALES ORDER" FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S31A.
           03  LINE  2 COLUMN 32 VALUE "DELETE SALES ORDER" FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S36.
           03  PIC X(06) USING W80-ACNO FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.

       01  S040-TRANS.
           03  LINE  6 COLUMN 12 VALUE "C"              FOREGROUND-COLOR Grey HIGHLIGHT.
           03          COLUMN 13 VALUE "reate, ".
           03          COLUMN 20 VALUE "A"              FOREGROUND-COLOR Grey HIGHLIGHT.
           03          COLUMN 21 VALUE "mend or ".
           03          COLUMN 29 VALUE "D"              FOREGROUND-COLOR Grey HIGHLIGHT.
           03          COLUMN 30 VALUE "elete Order - ".
           03          COLUMN 44 VALUE "E"              FOREGROUND-COLOR Grey HIGHLIGHT.
           03          COLUMN 45 VALUE "xit or ".
           03          COLUMN 52 VALUE "Esc"            FOREGROUND-COLOR Grey HIGHLIGHT.
           03          COLUMN 55 VALUE "ape".

       01  S40.
           03  BACKGROUND-COLOR Green.
               05  LINE  8 COLUMN 15 VALUE "Ú"                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 16 PIC X(50) USING WS-SRT-LINE FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 66 VALUE "¿"                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05  LINE  9 COLUMN 15 VALUE "³"                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 16 PIC  X(18) FROM WS-SPACE.
               05          COLUMN 34 VALUE "CLIENT DETAILS"      FOREGROUND-COLOR Grey.
               05          COLUMN 48 PIC  X(18) FROM WS-SPACE.
               05          COLUMN 66 VALUE "³"                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05  LINE 10 COLUMN 15 VALUE "³"                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 16 VALUE "Name    >"           FOREGROUND-COLOR Grey.
               05          COLUMN 65 VALUE "<"                   FOREGROUND-COLOR Grey.
               05          COLUMN 66 VALUE "³"                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05  LINE 11 COLUMN 15 VALUE "³"                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 16 VALUE "Address >"           FOREGROUND-COLOR Grey.
               05          COLUMN 55 VALUE "<"                   FOREGROUND-COLOR Grey.
               05          COLUMN 56 PIC  X(10) FROM WS-SPACE.
               05          COLUMN 66 VALUE "³"                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05  LINE 12 COLUMN 15 VALUE "³"                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 16 PIC  X(08) FROM WS-SPACE.
               05          COLUMN 24 VALUE ">"                   FOREGROUND-COLOR Grey.
               05          COLUMN 55 VALUE "<"                   FOREGROUND-COLOR Grey.
               05          COLUMN 56 PIC  X(10) FROM WS-SPACE.
               05          COLUMN 66 VALUE "³"                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05  LINE 13 COLUMN 15 VALUE "³"                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 16 PIC  X(08) FROM WS-SPACE.
               05          COLUMN 24 VALUE ">"                   FOREGROUND-COLOR Grey.
               05          COLUMN 55 VALUE "<"                   FOREGROUND-COLOR Grey.
               05          COLUMN 56 PIC  X(10) FROM WS-SPACE.
               05          COLUMN 66 VALUE "³"                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05  LINE 14 COLUMN 15 VALUE "³"                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 16 PIC  X(08) FROM WS-SPACE.
               05          COLUMN 24 VALUE ">"                   FOREGROUND-COLOR Grey.
               05          COLUMN 55 VALUE "<"                   FOREGROUND-COLOR Grey.
               05          COLUMN 56 PIC  X(10) FROM WS-SPACE.
               05          COLUMN 66 VALUE "³"                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05  LINE 15 COLUMN 15 VALUE "³"                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 16 PIC  X(08) FROM WS-SPACE.
               05          COLUMN 24 VALUE ">"                   FOREGROUND-COLOR Grey.
               05          COLUMN 29 VALUE "<"                   FOREGROUND-COLOR Grey.
               05          COLUMN 30 PIC  X(36) FROM WS-SPACE.
               05          COLUMN 66 VALUE "³"                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05  LINE 16 COLUMN 15 VALUE "³"                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 16 VALUE "Tel.    >"           FOREGROUND-COLOR Grey.
               05          COLUMN 39 VALUE "<"                   FOREGROUND-COLOR Grey.
               05          COLUMN 40 PIC  X(26) FROM WS-SPACE.
               05          COLUMN 66 VALUE "³"                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05  LINE 17 COLUMN 15 VALUE "³"                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 16 VALUE "Id.No.  >"           FOREGROUND-COLOR Grey.
               05          COLUMN 39 VALUE "<"                   FOREGROUND-COLOR Grey.
               05          COLUMN 40 PIC  X(26) FROM WS-SPACE.
               05          COLUMN 66 VALUE "³"                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05  LINE 18 COLUMN 15 VALUE "À"                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 16 PIC X(50) USING WS-SRT-LINE FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 66 VALUE "Ù"                   FOREGROUND-COLOR Brown HIGHLIGHT.

       01  S41.
           03  BACKGROUND-COLOR Green FOREGROUND-COLOR Grey HIGHLIGHT.
               05 LINE 10 COLUMN 25 PIC X(40) USING W40-NM1  AUTO.
               05 LINE 11 COLUMN 25 PIC X(30) USING W40-AD1  AUTO.
               05 LINE 12 COLUMN 25 PIC X(30) USING W40-AD2  AUTO.
               05 LINE 13 COLUMN 25 PIC X(30) USING W40-AD3  AUTO.
               05 LINE 14 COLUMN 25 PIC X(30) USING W40-AD4  AUTO.
               05 LINE 15 COLUMN 25 PIC X(04) USING W40-PC   AUTO.
               05 LINE 16 COLUMN 25 PIC X(14) USING W40-HTEL AUTO.
               05 LINE 17 COLUMN 25 PIC X(14) USING W40-ID   AUTO.

      /
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000-MAIN        SECTION.
       AA000-INIT.
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *              Check the security level of the USER.
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
           IF LS0-SALEV < 1
               MOVE "Not Authorised" TO WS-ERR-STRING
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  If the system is using seperate order numbers per user - ³
      *    ³  check that the files that are being used may be updated  ³
      *    ³  or if update from local/network is required first.       ³
      *    ³  LS-DSKDRV is used to indicate which drive is in use for  ³
      *    ³  the current session. L=Local drive and N=Network drive.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF W70-ORD-SYS = "Y"
               IF LS-DSKDRV = "N"
      *    *************************************************************
      *    As local drives are not always C, the following test has been
      *    replaced with the instruction above which will be more reli-
      *    able if the workstation happens to have apac on a local drive
      *    other than C.
      *    *************************************************************
      *        IF NOT(WS-LOCAL-DRIVE = "C")
                   OPEN I-O SYSUSER
               END-IF
               MOVE LS0-NO  TO USE-NO
               PERFORM READ-SYSUSER THRU READ-SYSUSER-EXIT
               IF (WS-F-ERROR = 64) OR (USE-SEQ NOT = 1)
                   MOVE "Please UPDATE files (Orders Spec)"  TO WS-ERR-STRING
                   PERFORM ERROR-MESSAGE
                   IF LS-DSKDRV = "N"
      *    *************************************************************
      *    As local drives are not always C, the following test has been
      *    replaced with the instruction above which will be more reli-
      *    able if the workstation happens to have apac on a local drive
      *    other than C.
      *    *************************************************************
      *    IF NOT(WS-LOCAL-DRIVE = "C")
                       CLOSE SYSUSER
                   END-IF
                   GO TO AZ000-EOJ.
           IF LS-DSKDRV = "N"
      *    *************************************************************
      *    As local drives are not always C, the following test has been
      *    replaced with the instruction above which will be more reli-
      *    able if the workstation happens to have apac on a local drive
      *    other than C.
      *    *************************************************************
      *    IF NOT(WS-LOCAL-DRIVE = "C")
               CLOSE SYSUSER.
             CLOSE DEBDEP.
             GO TO BA00.

       AA49.
             EXIT PROGRAM.

       COPY "AA50DSO.LUP".

      /    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *     S C R E E N ,   K E Y B O A R D   &  M O U S E
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3    ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³                      SCREEN-SHADOW                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To routine is used to display a shadow down the right and ³
      *    ³ along the bottom of a pop-up box. The parameters that are ³
      *    ³ required:                                                 ³
      *    ³          SHADE-ROW   - Top line of the box + 1.           ³
      *    ³          SHADE-COL   - Left line of box + 2.              ³
      *    ³          SHADE-WIDTH - Width of the box - 2.              ³
      *    ³          SHADE-LINES - Hight of box - 1.                  ³
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
      *    *************************************************************
      *            T H I S   R O U T I N E   I S   U S E D   T O
      *    D I S P L A Y   I N F O R M A T I O N   M E S S A G E S
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Move the message to WS-DSP-MES and the top line of the   ³
      *    ³  message box to WS-MES-LINE. If WS-MES-LINE is zero the   ³
      *    ³  default will be 20.                                      ³
      *    ³                  PERFORM DISPLAY-MESSAGE                  ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ****   T H I S   R O U T I N E   I S   U S E D   T O
      *         D I S P L A Y   E R R O R   M E S S A G E S
      *    *************************************************************
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
      *    ³ To display the request message higher or lower (default   ³
      *    ³ is line 20) move the line number to be used to            ³
      *    ³ WS-MES-LINE.                                              ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "OPTION.CRT".

       COPY "LOCKED.RC2".

       COPY "CLEAR.CRT".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                       DEBTOR-MEMO                         ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ The account number of the Debtor record for which the MEMO³
      *    ³ details are to be displayed must be in W80-ACNO.          ³
      *    ³ PERFORM DEBTOR-MEMO.                                      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "DEBMEM.PRO".

       COPY "DEBTOR.TRN".

      *    *************************************************************
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S
      *    *************************************************************
       AA300              SECTION.
       AA301.
            DISPLAY "F1"     AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                        "=Help,"     WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                    "F2"             WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                        "=Item,"     WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                    "F3"             WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                        "=Alt,"      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                    "F4"             WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                        "=Desc,"     WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                    "F5"             WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                        "=Desc2,"    WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                    "F6"             WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                        "=X/Ref,"    WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                    "F9"             WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                        "=Word,"     WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                    "Esc"            WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                        "ape to end" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
       AA305.
             EXIT.
      *    *************************************************************
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S
      *    *************************************************************
       AA400              SECTION.
       AA401.
            DISPLAY "F1"                  AT 2502 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                    "=Help,"                      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                    "F2"                          WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                    " Order no./ "                WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                    "F3"                          WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                    " Account no./ "              WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                    "F4"                          WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                    " From specified date Lookup" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
       AA405.
             EXIT.
      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³        R E A D  F I L E S   R O U T I N E S        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AC000    SECTION.

       COPY "CONTROL.RD".

       COPY "DEBALT.RD".

       COPY "DEBDEP.RD".

       COPY "DEBTOR.RD".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "SALE.RD".

       COPY "SHARED.RD".

       COPY "SORDER.RD".

       COPY "SPARTS.RD".

       COPY "STKMEM.RD".

       COPY "STOCK.RD".

       COPY "SYSUSE.RD".

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AD000-WRITE        SECTION.

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

       COPY "SALE.WR".

       COPY "SORDER.WR".

       COPY "STOCK.WR".

       COPY "SYSUSE.WR".

       COPY "DEBTOR.ILU".

       COPY "SORDER.LUP".

       COPY "APAC.HLP".

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY000        SECTION.
       AY05.
             MOVE 05                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
           IF WS-ACTION = 0 OR 2
               PERFORM READ-STOCK-LOCK THRU READ-STOCK-EXIT.
           IF WS-ACTION = 9
               MOVE 0                TO REC-TYPE
           ELSE
               MOVE WS-ACTION        TO REC-TYPE.
             MOVE STK-RECORD1        TO REC-STOCKF.
             GO TO AY50.

       AY17.
             MOVE 17                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE.
             MOVE SOR-REC1           TO REC-SORDER.
             GO TO AY50.
       AY39.
             MOVE 39                 TO REC-FILE.
             MOVE WS-NETKEY          TO REC-KEY.
             MOVE NET-RECORD         TO REC-NETWORK.
             GO TO AY50.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³             End transaction in recovery log               ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY49.
             MOVE 99                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE SPACES             TO REC-DETAIL.
             PERFORM AY50.
             ADD 1                   TO WS-TRANS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If recovery records exceed 95 - Clear (Close and Open     ³
      *    ³ new). This allows for quicker recoveries when required.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF WS-RECOVER > 95
               CLOSE RECOVER
               OPEN OUTPUT RECOVER
               CLOSE RECOVER
               OPEN I-O RECOVER
               MOVE ZERO             TO WS-RECOVER.
             GO TO AY59.
       AY50.
             ADD 1                   TO WS-RECOVER.
             MOVE WS-RECOVER         TO WS-RECKEY.
             MOVE WS-TRANS           TO REC-TRAN.
             WRITE REC-RECORD.
           IF WS-STATUS NOT = "00"
               DISPLAY "Write error Recovery file - Status " AT 2212 WITH FOREGROUND-COLOR Brown HIGHLIGHT WS-STATUS WITH FOREGROUND-COLOR Grey HIGHLIGHT
               STOP RUN.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
       AY59.
             EXIT.
      *    *************************************************************
      *    ****    Start processing transaction
      *    *************************************************************
       AY60.
           IF WS-SKIP = "Y" OR "A"
               GO TO AY999.
             MOVE 1                  TO WS-COUNT.
             MOVE 5                  TO WS-SHARED.
             PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      *    *************************************************************
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *    *************************************************************
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-USERS < 24
               MOVE 1                TO WS-SUB
               MOVE ZERO             TO WS-WAIT
               GO TO AY62.
      *    *************************************************************
      *    ****   Q   F U L L  -  W A I T   F O R   4 S E C O N D S
      *    *************************************************************
             DISPLAY "WAITING" AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red HIGHLIGHT.
             COMMIT.
             ACCEPT WS-STIME FROM TIME.
             MOVE 400                TO WS-WAIT.
             PERFORM LOCK-REC-LOOP.
             DISPLAY SPACE AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.
             GO TO AY60.

       AY61.
             MOVE "DS"               TO PAR-PROG(WS-USUB).
             MOVE LS-USER            TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *    *************************************************************
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *    *************************************************************
             MOVE 1                  TO WS-NETKEY.
             PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             PERFORM AY39 THRU AY59.
             MOVE NET-DEBTOR         TO W70-DEBT.
             GO TO AY999.
      *    *************************************************************
      *    ****   A R E   A N Y   U P D A T E S   I N P R O G R E S S
      *    *************************************************************
       AY62.
           IF NOT(PAR-PROG(WS-SUB) = SPACES OR PAR-USR(WS-SUB) = SPACES)
      *    *************************************************************
      *    ****   A R E   D E B T O R O R   S T O C K   F I L E S
      *    B E I N G   U P D A T E D
      *    *************************************************************
               IF NOT(PAR-PROG(WS-SUB) = SPACES)
                   IF PAR-PROG(WS-SUB) = "DB" OR "DS" OR "CS" OR "ST"
      *    *************************************************************
      *    ****   Y E S   -   S E T   W A I T P E R I O D
      *    *************************************************************
                       GO TO AY62-WAIT
                   ELSE
                       ADD 1         TO WS-SUB
                       GO TO AY62
                   END-IF
               ELSE
      *    *************************************************************
      *    ****   I S T H I S   P R O G R A M   I N T H E  Q
      *    *************************************************************
               IF PAR-USR(WS-SUB) = LS-USER
      *    *************************************************************
      *    * I S   I T  N E X T   I N  L I N E   T O  P R O C E S S *
      *    *************************************************************
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
      *    *************************************************************
      *    ****    S E T   W A I T   P E R I O D      ****
      *    *************************************************************
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

       AY70.
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *    *************************************************************
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 1.
      *    *************************************************************
             MOVE 1                  TO WS-NETKEY.
             MOVE W70-DEBT           TO NET-DEBTOR.
             PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
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
             SUBTRACT 1 FROM WS-USUB GIVING PAR-USERS.
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
             CLOSE RECOVER SALE.
           IF W70-ORD-SYS = "Y"
               IF LS-DSKDRV = "L"
      *    *************************************************************
      *    As local drives are not always C, the following test has been
      *    replaced with the instruction above which will be more reli-
      *    able if the workstation happens to have apac on a local drive
      *    other than C.
      *    *************************************************************
      *        IF WS-LOCAL-DRIVE = "C"
                   CLOSE SORDER SYSUSER
                   MOVE WS-SORDER-STR  TO W02-SORDER
                   OPEN I-O SORDER.

       AZ100.
             EXIT PROGRAM.

       COPY "CA000.STK".

       COPY "CA100.STK".

      *
      *    ****    Pack the delivery address for printing
      *
       CA110-ADDRESS       SECTION 2.
       CA110.
             MOVE ZERO               TO WS-ERROR.
             MOVE 1                  TO WS-S1 WS-S2.
             MOVE 30                 TO WS-S3.
             MOVE SPACES             TO W60-ADDRESS.
       CA115.
          IF W40-PCHAR(WS-S3) NOT = SPACE
               GO TO CA120.
           IF WS-S3 > WS-S2
               SUBTRACT 1            FROM WS-S3
               GO TO CA115.
       CA120.
             MOVE W40-PCHAR(WS-S2)   TO W60-ACHAR(WS-S1).
           IF WS-S2 = WS-S3
               GO TO CA125.
             ADD 1                   TO WS-S1.
             ADD 1                   TO WS-S2.
           IF WS-S1 > 70
               MOVE 1                TO WS-ERROR
           ELSE
               GO TO CA120.
       CA125.
           IF WS-ERROR = 1
               GO TO CA130.
           IF WS-S1 < 70
               ADD 1                 TO WS-S1
               MOVE "*"              TO W60-ACHAR(WS-S1).
       CA130.
             ADD 1                   TO WS-S1.
       CA135.
             MOVE 31                 TO WS-S2.
             MOVE 60                 TO WS-S3.
             PERFORM CA115 THRU CA130.
           IF WS-ERROR = 1
               GO TO CA140-EXIT.
             MOVE 61                 TO WS-S2.
             MOVE 90                 TO WS-S3.
             PERFORM CA115 THRU CA130.
           IF WS-ERROR = 1
               GO TO CA140-EXIT.
             MOVE 91                 TO WS-S2.
             MOVE 120                TO WS-S3.
             PERFORM CA115 THRU CA130.
       CA140-EXIT.
             MOVE ZERO               TO WS-S6.
       CA140.
             EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³         Read debtor record - using account number.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       CA155-GET-DEBTOR  SECTION 2.
       CA155.
             MOVE W80-ACNO           TO DEB-ACNO.
             PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               MOVE 1                TO WS-ERROR
           ELSE
           IF DEB-ACNO NOT = W80-ACNO
               MOVE 2                TO WS-ERROR
           ELSE
               MOVE 0                TO WS-ERROR
               MOVE DEB-TYPE         TO W80-TYPE.
           IF DEB-CONAC = LOW-VALUES
               MOVE SPACES           TO DEB-CONAC.

       CA160-EXIT.
             EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Unpack the postal address into line 1, 2, 3 and 4     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       CA175-ADDRESS  SECTION 2.
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Unpack the physical address into line 1, 2, 3 and 4    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       CA185-ADDRESS  SECTION 2.
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

       COPY "CA200.STK".

       COPY "CA500.STK".

      *    *************************************************************
      *    *** G E T   S O R D E R   R E C O R D   U S I N G   SOR-KEY.
      *
      *           WS-ERROR:
      *
      *                    1 = NO RECORD
      *                    2 = NOT ORDER RECORD (QUOTE OR INVOICE)
      *                    3 = ORDER DETAILS INCOMPLETE
      *                    4 = INVALID ACCOUNT NUMBER
      *    *************************************************************
       GET-SORDER           SECTION.

       GET-SORD-ORDNO.
             INITIALIZE W14-DELETE.
             MOVE 1                  TO W14-SUB WS-S7.
             MOVE SPACES             TO W40-NM1 W40-AD1 W40-AD2 W40-AD3 W40-AD4 W40-PC W35-KEY.
             MOVE W10-SORD           TO SOR-REF W90-RF.
             MOVE 100                TO SOR-SQN.
             MOVE ZERO               TO WS-ERROR.
             PERFORM READ-SORDER THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
               MOVE 1                TO WS-ERROR
               GO TO GET-SORDER-EXIT.
           IF SOR-DOC NOT = "O"
               MOVE 2                TO WS-ERROR
               GO TO GET-SORDER-EXIT.
             MOVE 1                  TO WS-LAST WS-SALKEY.
             GO TO GET-SORD-TEST-TYPE.

       GET-SORD-NEXT.
             PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
               MOVE 3                TO WS-ERROR
               GO TO GET-SORDER-EXIT.

       GET-SORD-TEST-TYPE.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                 H E A D E R   R E C O R D                 ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF SOR-TYPE = 0
               MOVE SOR-AC           TO W80-ACNO DEB-ACNO
               PERFORM CA155-GET-DEBTOR
               IF WS-ERROR NOT = ZERO
                   MOVE 4            TO WS-ERROR
                   GO TO GET-SORDER-EXIT
               END-IF
               MOVE SOR-OREF         TO W90-REG
               MOVE SOR-NAME         TO W40-NAME W40-NM1
               MOVE SOR-PICNO        TO W90-PICK
               MOVE SOR-ORD          TO W90-ORD
               MOVE SOR-SMAN         TO W10-SMAN
               MOVE SOR-EXP          TO W90-EXP
               MOVE SOR-PRC          TO WS-PRC
               MOVE SOR-CONTACT      TO W40-CONTACT
               MOVE SOR-TEL          TO W40-TEL
           ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                  I T E M   D E T A I L S                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF SOR-TYPE = 1
               INITIALIZE SAL-RECORD1
               MOVE SOR-EXT-ITEM     TO SAL-EXT-ITEM
               MOVE SOR-DESC         TO SAL-MSER
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
               END-IF
               MOVE "Y"              TO SAL-INSTRUCT
               PERFORM GET-SORD-WRITE-SALE
           ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                 P A R T S   D E T A I L S                 ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF SOR-TYPE = 2
               INITIALIZE SAL-RECORD1
               MOVE SOR-EXT-ITEM     TO SAL-EXT-ITEM
               MOVE SOR-DESC         TO SAL-MSER
               MOVE SOR-QNT          TO SAL-QNT
               MOVE SOR-SQN          TO SAL-SQN
           ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                       R E M A R K S                       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF SOR-TYPE = 3
               INITIALIZE SAL-RECORD1
               MOVE SOR-SERIAL       TO SAL-MSER
               MOVE SOR-SQN          TO SAL-SQN
               PERFORM GET-SORD-WRITE-SALE
           ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                I N T E R N A L   N O T E S                ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF SOR-TYPE = 4
               INITIALIZE SAL-RECORD1
               MOVE SOR-DESC         TO SAL-MSER
               MOVE SOR-SQN          TO SAL-SQN
               MOVE 1                TO SAL-SUPER
               PERFORM GET-SORD-WRITE-SALE
           ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                       A D D R E S S                       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF SOR-TYPE = 5
               MOVE SOR-KEY          TO W35-KEY
               MOVE SOR-AD1          TO W40-AD1
               MOVE SOR-AD2          TO W40-AD2
               MOVE SOR-AD3          TO W40-AD3
               MOVE SOR-AD4          TO W40-AD4
               MOVE SOR-PC           TO W40-PC
           ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                    S A L U T A T I O N                    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF SOR-TYPE = 7
               MOVE SOR-TEXT  TO W34-SALUTATION
           ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                          T E X T                          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF SOR-TYPE = 8
               MOVE SOR-TEXT         TO W32-REMARKS(WS-S7)
               ADD 1                 TO WS-S7
           ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                       T R A I L E R                       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               MOVE SOR-SUBTOT       TO W90-DB
               MULTIPLY -1 BY SOR-DSCNT  GIVING W90-TDISC
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
             MOVE WS-LAST            TO SAL-CODE.
             PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
             ADD 1                   TO WS-LAST WS-SALKEY.
      
       GET-SORDER-EXIT.
             EXIT.
      /
       BA000                    SECTION.
       BA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S02.
             DISPLAY S040-TRANS.
           IF LS-DAYS > 65
               COMPUTE WS-NUM = 15 - (LS-DAYS - 65)
               DISPLAY "This package must be registered within " AT 2502 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red  HIGHLIGHT
                        WS-NUM                                           WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT BLINK
                       " days"                                           WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red  HIGHLIGHT.
       BA05.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    GO TO AZ000-EOJ
                 WHEN OTHER      CALL X"E5" CALL X"E5" CALL X"E5"
               END-EVALUATE
               GO TO BA05
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X      TO WS-OPTION
               CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS
           ELSE
               CALL X"E5" CALL X"E5" CALL X"E5"
               GO TO BA05.
             MOVE ZERO               TO W90-BAL.
             PERFORM CLEAR-L50.
      *
      *      THE COPIED FILE W90.SAL HAS A VALUE OF "O" FOR W90-ORG
      *      so the following instruction is now a comment and will be
      *      deleted at a later date.
      *       
      *      MOVE "O"                TO W90-ORG.
             EVALUATE WS-OPTION
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                  E X I T   P R O G R A M                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               WHEN = "E"        GO TO AZ000-EOJ
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                  C R E A T E   O R D E R                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               WHEN = "C"        PERFORM BC000
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                   A M E N D   O R D E R                   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               WHEN = "A"        PERFORM BD000
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                  D E L E T E   O R D E R                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               WHEN = "D"        PERFORM BE000
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³               I N V A L I D   O P T I O N                 ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               WHEN OTHER        CALL X"E5" CALL X"E5" CALL X"E5"
                                 GO TO BA05
             END-EVALUATE.                       
             GO TO BA00.
      /
       BB000               SECTION.
       BB00.
           IF DEB-NCASH = "CASH SALE" OR
                          "KONTANT V"
               GO TO BB06.
       BB05.
      MOVE "Enter client details Y/N  [Y]"
     TO WS-ERR-MES.
      MOVE "Y"   TO WS-OPTION.
      PERFORM OPT-MESSAGE TEST AFTER
       UNTIL WS-OPTION = "N" OR "Y".
    IF WS-OPTION = "N"
               GO TO BB20.
       BB06.
      PERFORM SAVE-SCREEN.
      MOVE SPACES  TO W40-NM1 W40-AD1 W40-AD2
                                    W40-AD3 W40-AD4 W40-HTEL
                                    W40-ID  W40-PC.
             MOVE 0815           TO CPOS.
       BB10.
             DISPLAY
   "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°"
   AT CPOS.
           IF CLIN < 41
               ADD 1                 TO CLIN
               GO TO BB10.
      DISPLAY S40.

       BB15.
             ACCEPT S41.
           IF DEB-NCASH = "CASH SALE" OR "KONTANT V"
               IF (W40-NM1 = SPACES) OR (W40-AD1 = SPACES) OR (W40-HTEL = SPACES)
                   MOVE "Require Name, Address & Telephone"  TO WS-ERR-STRING
                   PERFORM ERROR-MESSAGE
                   GO TO BB15.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO BB15.
           IF DEB-NCASH = "CASH SALE" OR "KONTANT V"
               MOVE W40-NM1          TO W40-NAME
               MOVE W40-AD1          TO W40-ADD1
               MOVE W40-AD2          TO W40-ADD2
               MOVE W40-AD3          TO W40-ADD3
               MOVE W40-AD4          TO W40-ADD4
               MOVE W40-PC           TO W40-PC1.
       BB20.
      PERFORM CLEAR-L50.
       BB29.
             EXIT.
      /
       BB050               SECTION.
       BB50.
             MOVE WS-EXPP        TO REP-DETAIL2.
             MOVE ZERO           TO REP-LN.
             WRITE REP-L2.
             MOVE W95-COMP  TO REP-DETAIL2.
             MOVE 2              TO REP-LN.
             WRITE REP-L2.
            MOVE SPACES  TO REP-DETAIL2.
             MOVE "PICKING SLIP - REFERENCE NO:"
                                 TO REP-H1.
             MOVE W90-PS         TO REP-REF.
             MOVE ZERO           TO REP-LN.
             WRITE REP-L2.
             MOVE 2              TO REP-LN.
            WRITE REP-L2.
           IF W90-PICK > 01
               MOVE SPACES       TO REP-DETAIL2
               MOVE "REPLACE PREVIOUS PICKING SLIP"
                                 TO REP-H1
               MOVE 2            TO REP-LN
             WRITE REP-L2.

       BB60.
             MOVE WS-ECAN        TO REP-DETAIL2.
             MOVE ZERO           TO REP-LN.
             WRITE REP-L2.
             MOVE SPACES         TO REP-DETAIL2.
             MOVE "Account :"    TO REP-AC.
             MOVE W80-ACNO       TO REP-ACNO.
             MOVE W40-NAME       TO REP-NAME.
             MOVE "Date:"        TO REP-DATE.
             MOVE W12-TODAY      TO REP-DTE.
             MOVE 1              TO REP-LN.
            WRITE REP-L2.
             MOVE SPACES         TO REP-DETAIL2.
             MOVE " Contact:"    TO REP-CNTCT.
             MOVE W40-CONTACT    TO REP-CONTACT.
             MOVE "Tel :"        TO REP-AC.
             MOVE W40-TEL        TO REP-TEL.
             MOVE "Time:"        TO REP-DATE.
             ACCEPT W25-TIME FROM TIME.
             MOVE W25-HH         TO W95-HH.
             MOVE W25-MM         TO W95-MM.
             MOVE W25-SS         TO W95-SS.
             MOVE W95-TIME       TO REP-TIME.
      MOVE 1   TO REP-LN.
            WRITE REP-L2.
             MOVE SPACES         TO REP-DETAIL2.
    IF WS-DELIV = "Y"
        IF NOT(W40-PADDRESS = SPACES)
     PERFORM CA110-ADDRESS
     INSPECT W60-ADDRESS REPLACING ALL "*" BY "."
     MOVE 1  TO REP-LN
     MOVE W60-ADDRESS
     TO REP-DETAIL2
     WRITE REP-L2
     MOVE SPACES        TO REP-DETAIL2.
      MOVE ALL "="  TO REP-DETAIL1.
             MOVE 2              TO REP-LN.
            WRITE REP-L2.
             MOVE SPACES         TO REP-DETAIL2.
            MOVE "Item Code"  TO REP-H2.
            MOVE "Description"  TO REP-H3.
            MOVE "  Quantity"  TO REP-H4.
    IF WS-PIC-PRC = "Y"
        MOVE "     Value" TO REP-H5
    ELSE
    IF WS-ITMPRC = "Y"
        MOVE "Item Price" TO REP-H5
    ELSE
        MOVE "Remarks"  TO REP-H5.
      MOVE 2   TO REP-LN.
            WRITE REP-L2.
             MOVE SPACES         TO REP-DETAIL2.
       BB65.
             EXIT.
      /
       BB100               SECTION.
       BB100-MARG.
             MOVE ZERO TO WS-ERROR.
           IF W90-TAX = SPACES
               IF STK-TAX NOT = ZERO
                   COMPUTE W10-SPAPRC = (W10-SPAPRC /
                                        (100.0000 + W05-RATE)) * 100.
    IF WS-PRC = "3"
               IF STK-WSALE NOT > W10-SPAPRC
                   GO TO BB110
               END-IF
           ELSE
    IF WS-PRC = "2"
               IF STK-CASH NOT > W10-SPAPRC
                   GO TO BB110
               END-IF
           ELSE
           IF STK-SELL NOT > W10-SPAPRC
               GO TO BB110.
    IF NOT(W10-SPAPRC = ZERO)
        GO TO BB104.
       BB103.
      MOVE "Is this a no-charge item Y/N"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE UNTIL WS-OPTION = "N" OR "Y".
      GO TO BB110.
       BB104.
             COMPUTE W10-MARG = ((W10-SPAPRC - W10-COST) / W10-SPAPRC)
                                * 100.00.
           IF W10-MARG NOT < W85-MARG
               GO TO BB110.
       BB105.
            MOVE "Margin less than allowed - Override Y/N?"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE UNTIL WS-OPTION = "N" OR "Y".
          IF WS-OPTION = "N"
               MOVE 1 TO WS-ERROR
               GO TO BB110.
       BB106.
            DISPLAY "Enter password" AT 2551
              WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT W40-CSHPASS AT 2567
      WITH BACKGROUND-COLOR Magenta
    FOREGROUND-COLOR Grey HIGHLIGHT
          AUTO NO-ECHO.
            MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W40-CSHPASS
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
            MOVE 1   TO WS-LENGTH.
       BB107.
           IF W40-CSHPASS = "!@$^*)"
               MOVE 10           TO WS-SUPER
           ELSE
    IF W40-CSHPASS = W85-SUPER(1)
               MOVE 1            TO WS-SUPER
           ELSE
    IF W40-CSHPASS = W85-SUPER(9)
               MOVE 9            TO WS-SUPER
           ELSE
               MOVE 1            TO WS-ERROR
               MOVE ZERO         TO WS-SUPER.
           GO TO BB110.
       BB108.
           IF W40-CSHPASS = "!#%&()"
               MOVE 10           TO WS-SUPER
           ELSE
    IF W40-CSHPASS = W85-SUPER(1)
               MOVE 1            TO WS-SUPER
           ELSE
    IF W40-CSHPASS = W85-SUPER(6)
               MOVE 6            TO WS-SUPER
           ELSE
               MOVE 1            TO WS-ERROR
               MOVE ZERO         TO WS-SUPER.
       BB110.
             EXIT.
      /    *************************************************************
      *    ****    P I C K I N G   S L I P S
      *    *************************************************************
       BB200         SECTION.
       BB205.
           IF WS-PGADV = "Y"
        OPEN OUTPUT PRNDSK
        CALL X"91" USING X91-RES X91-FUN PRNDSK.
      MOVE LS-SLS-ORDNO  TO W90-REF W10-SORD.
             PERFORM GET-SORDER.
           IF WS-ERROR NOT = ZERO
        MOVE "Database corrupted"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BB399.
       BB210.
      *    *************************************************************
      *    ****    R E A D   D E T A I L S   A N D   P R I N T   S L I P
      *    *************************************************************
       BB265.
            DISPLAY "Printing Picking Slip" AT 2502
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
            BACKGROUND-COLOR Cyan BLINK.
             MOVE ZERO           TO WS-SALKEY.
             PERFORM AY60 THRU AY999.
       BB270.
      *    *************************************************************
      *    ****    R E A D   O R D E R   F I L E   A N D   P R I N T
      *    *************************************************************
             MOVE W90-REF        TO SOR-REF.
             MOVE ZERO           TO SOR-SQN.
             PERFORM START-AT-ORDER-NO THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
               MOVE "Database corrupted" TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BB399.
       BB275.
             PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
               GO TO BB300.
           IF SOR-REF NOT = W90-REF
               GO TO BB300.
           IF SOR-TYPE = 0
               ADD 1             TO W90-PICK
               PERFORM BB050
               GO TO BB275.
           IF SOR-TYPE = 9
               GO TO BB300.
           IF SOR-TYPE NOT = 1
               IF SOR-TYPE = 2 OR 5
                   GO TO BB275
               ELSE
                   MOVE SOR-DESC TO REP-DESC
                   PERFORM BB280
                   GO TO BB275.
      MOVE SOR-EXT-ITEM  TO PRT-EXT-ITEM.
    IF WS-USE-ITM = "Y"
        MOVE SOR-ITEM  TO REP-ITM1
        MOVE "/"   TO REP-SL
        MOVE SOR-ITM  TO REP-ITM
    ELSE
        MOVE SOR-EXT-ITEM TO REP-EXT-ITEM.
      MOVE SOR-DESC  TO REP-DESC.
             MOVE SOR-QNT        TO REP-QTY.
    IF WS-PIC-PRC = "Y"
        MOVE SOR-VAL  TO REP-PRICE
    ELSE
    IF WS-ITMPRC = "Y"
        MOVE SOR-SELL  TO REP-PRICE.
            PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
           IF WS-F-ERROR = 21
      *  MOVE SOR-QNT   TO REP-QNT
               PERFORM BB280
               GO TO BB275.

       BB280.
    IF LINAGE-COUNTER OF PRNDSK < WS-PRN-LENGTH
               MOVE 1            TO REP-LN
        WRITE REP-L2
           ELSE
               MOVE "PAGE"       TO REP-PG
        WRITE REP-L2
               MOVE WS-EXPP      TO REP-DETAIL2
               MOVE ZERO         TO REP-LN
        WRITE REP-L2
               MOVE "PICKING SLIP - CONTINUED ..."
                                 TO REP-H1
        WRITE REP-L2
               MOVE 2            TO REP-LN
        WRITE REP-L2
               PERFORM BB60 THRU BB65.
             MOVE SPACES         TO REP-DETAIL2.
      *    *************************************************************
      *    ****  P R O D U C T I O N   D E T A I L S
      *    *************************************************************
       BB285.
             MOVE 1              TO WS-S6.
       BB290.
          IF PRT-EXT-CODE(WS-S6) = SPACES
               ADD 1             TO WS-S6
               GO TO BB295.
    IF WS-USE-ITM = "Y"
        MOVE PRT-CODE(WS-S6)
     TO REP-ITM2
        MOVE "/"   TO REP-SL2
        MOVE PRT-PITM(WS-S6)
     TO REP-ITM3
    ELSE
        MOVE PRT-EXT-CODE(WS-S6)
     TO REP-EXT-ITEM2.
            MOVE PRT-EXT-CODE(WS-S6)
     TO W10-EXT-ITEM.
            MULTIPLY PRT-QUANT(WS-S6) BY SOR-QNT
                                 GIVING REP-QNT.
             PERFORM CA000.
           IF WS-F-ERROR = 22
               ADD 1             TO WS-S6
               GO TO BB295.
             MOVE STK-DESC       TO REP-DSCR.
             PERFORM BB280
            ADD 1   TO WS-S6.

       BB295.
           IF WS-S6 < 31
               GO TO BB290.
             GO TO BB275.
      *    *************************************************************
      *    ****    U P D A T E   O R D E R   F I L E
      *    *************************************************************
       BB300.
             MOVE SPACES         TO REP-DETAIL2.
      MOVE 1   TO REP-LN.
      WRITE REP-L2.
      MOVE ALL "="  TO REP-DETAIL1.
            MOVE W90-REF  TO SOR-REF.
             MOVE 100            TO SOR-SQN.
      PERFORM READ-SORDER THRU READ-SORDER-EXIT.
             ADD 1               TO SOR-PICNO.
             PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT.
             PERFORM AY70 THRU AY999.
       BB310.
      MOVE "Y"   TO WS-OPTION.
      MOVE WS-OPTION  TO WS-PGADV.
      MOVE "PAGE"  TO REP-PG.
      WRITE REP-L2.
      CLOSE PRNDSK.
      PERFORM BB400.
             MOVE SPACES         TO REP-DETAIL2.

       BB399.
             EXIT.
      /
       BB400         SECTION.
       BB405.
      OPEN INPUT PRNDSK.
      CALL X"91" USING X91-RES X91-FUN PRNDSK.
       BB410.
             READ PRNDSK
                  AT END
                     CLOSE PRNDSK
                     GO TO BB499.
             MOVE REP-DETAIL2    TO PRN-DETAIL2.
           IF REP-PG = "PAGE"
               WRITE PRN-L2 BEFORE PAGE
           ELSE
               MOVE REP-LN       TO WS-LINES
               WRITE PRN-L1 BEFORE WS-LINES LINES.
             GO TO BB410.
       BB499.
             EXIT.
      /    *************************************************************
      *    ****    O R D E R S
      *    *************************************************************
       BC000         SECTION 50.
       BC00.
      *    *************************************************************
      *    ****    S U P E R V I S O R   P A S S W O R D S
      *    *************************************************************
             MOVE 5              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-PW         TO W85-PASS.
      *    *************************************************************
      *    ****    S A L E S   O R D E R   S C R E E N
      *    *************************************************************
       BC05.
            PERFORM ERASE-SCREEN.
             DISPLAY S30.
             MOVE 11              TO LIN.
             PERFORM BC115 UNTIL LIN > 41.
             MOVE W12-TODAY      TO W90-DTE.
            DISPLAY W90-DTE AT 0570
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            MOVE ZERO   TO W90-BDT W90-LDISC W90-NONTAX
              W90-BAL W90-SALES W90-EXCTOT
              W90-CUR W90-PMETH W40-STATUS.
            MOVE 1   TO WS-FIRST WS-SALPOS WS-LAST.
            MOVE SPACES  TO W90-TAX W90-INSTRUCT W40-SPAR.
             MOVE "N"            TO WS-IN-OPT.
      MOVE 1   TO WS-S9.

       BC08.
            MOVE ZERO   TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
    IF WS-S9 < 12
        ADD 1   TO WS-S9
        GO TO BC08.
      *    *************************************************************
      *    ****    D E B T O R   A C C O U N T   N U M B E R
      *    *************************************************************
       BC10.
            MOVE SPACES  TO W80-ACNO.

       BC11.
            DISPLAY "F1" AT 2502 WITH BACKGROUND-COLOR Cyan
           FOREGROUND-COLOR Brown HIGHLIGHT
       "=Help,"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "F2" WITH BACKGROUND-COLOR Cyan
          FOREGROUND-COLOR Brown HIGHLIGHT
       "=Debtor Lookup,"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "=Create,"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "F9" WITH BACKGROUND-COLOR Cyan
          FOREGROUND-COLOR Brown HIGHLIGHT
       "=Debtor Lookup (Any Word)"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
            ACCEPT S36 AT 0308.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W80-ACNO
       GO TO BC265
         WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
             PERFORM AA50D THRU AA59
   WHEN F8-KEY
             IF LS0-DBLEV > 0
    PERFORM GA000
    MOVE LS0-DBT
     TO W80-ACNO
       END-IF
   WHEN F9-KEY
       MOVE "A"  TO WS-TYP
             PERFORM AA50D THRU AA59
   WHEN OTHER
       CALL X"E5"
       GO TO BC11
        END-EVALUATE
              DISPLAY S36 AT 0308
               IF W80-ACNO = SPACES
                   GO TO BC11.
            PERFORM CLEAR-L50.
           IF W80-ACNO = SPACES
              GO TO BC265.
      MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W80-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.

       BC12.
             PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR = 1
              MOVE "No record"  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BC11.
           IF WS-ERROR = 2
              MOVE "Invalid account"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BC11.
      *    *************************************************************
      *    ****    D I S P L A Y   D E B T O R   D E T A I L S
      *    *************************************************************
       BC15.
            MOVE DEB-NAME  TO W40-NAME.
           IF W80-TYPE = 1
               INSPECT W40-NAME REPLACING ALL "*" BY " "
               INSPECT W40-NAME REPLACING FIRST "!" BY " ".
            DISPLAY W40-NAME AT 0320
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             COMPUTE W100-CUR = DEB-BAL - (DEB-CUR + DEB-30 +
                                DEB-60 + DEB-90 + DEB-120 + DEB-INT).
             PERFORM BC189 THRU BC215.
      *    *************************************************************
      *    ****    I S   T H I S   T H E   C O R R E C T   A C C O U N T
      *    *************************************************************
             PERFORM DEBTOR-MEMO.
             PERFORM SAVE-SCREEN-2.
             MOVE 12                 TO SHADE-ROW.
             MOVE 17                 TO SHADE-COL.
             MOVE 57                 TO SHADE-WIDTH.
             MOVE 12                 TO SHADE-LINES.
             DISPLAY S25.
             PERFORM SCREEN-SHADOW.
      *    *************************************************************
      *    ****   C A T E G O R Y   8 0   -  S U S P E N D E D   A / C
      *    *************************************************************
           IF DEB-CAT = 80
        MOVE "This account has been suspended"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO BC05.
      MOVE "Press any key to continue"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
      PERFORM RESTORE-SCREEN-2.
      *    *************************************************************
      *    ****   U S E R   O P T I O N
      *    *************************************************************
       BC16.
      0       MOVE "'C'ontinue, 'R'ekey, 'E'xit, 'F9' Memo/Remarks" TO WS-OPT-MES.
             MOVE "C"                TO WS-OPTION.
             MOVE "F"                TO LS-INSTR.

       BC18.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = X"F9"
               PERFORM DEBTOR-MEMO
               GO TO BC16.

             MOVE 11              TO LIN.
             PERFORM BC115 UNTIL LIN > 41.

       BC20.
            PERFORM CLEAR-L50.
           IF WS-OPTION = "R"
               GO TO BC05.
           IF WS-OPTION = "E"
              GO TO BC265.
             MOVE "Y"            TO WS-OPTION.
      *    *************************************************************
      *    ****  C H E C K   A R R E A R S   &  C R E D I T   L I M I T
      *    *************************************************************
           IF (DEB-60 > ZERO) OR (DEB-90 > ZERO) OR (DEB-120 > ZERO)
              MOVE "Arrears > 30 days - Override Y/N"
     TO WS-ERR-MES
        MOVE SPACE  TO WS-OPTION
        PERFORM OPT-MESSAGE TEST AFTER
         UNTIL WS-OPTION = "Y" OR "N"
              IF WS-OPTION = "Y"
                   MOVE ZERO     TO WS-ERROR
                   PERFORM BB106
                   PERFORM BB108
                   IF WS-ERROR = 1
                       MOVE "N"  TO WS-OPTION.
          IF WS-OPTION = "N"
               GO TO BC05.
           IF DEB-CRL > ZERO
               IF DEB-BAL > DEB-CRL
           MOVE "Credit limit exceded - Override Y/N"
     TO WS-ERR-MES
     MOVE SPACE  TO WS-OPTION
     PERFORM OPT-MESSAGE UNTIL WS-OPTION = "Y" OR "N"
              IF WS-OPTION = "Y"
                   MOVE ZERO     TO WS-ERROR
                   PERFORM BB106
                   PERFORM BB108
                   IF WS-ERROR = 1
                       MOVE "N"  TO WS-OPTION.
          IF WS-OPTION = "N"
               GO TO BC05.
           IF DEB-NCASH = "CASH SALE" OR
                             "KONTANT V"
               MOVE 1            TO W90-PMETH
        PERFORM SAVE-SCREEN-3
               PERFORM BB000
        PERFORM RESTORE-SCREEN-3.

       BC22.
    IF NOT VALID-PRICE
        MOVE "1"   TO DEB-PRICE.
             MOVE DEB-PRICE      TO WS-PRC.
      MOVE SPACES  TO WS-RESTRICT.
      MOVE "N"   TO WS-RESTCODE.
      *    *************************************************************
      *    ****   D E B T O R S A L E S   R E S T R I C T I O N S
      *    *************************************************************
    IF DEB-RESTRICT = "A" OR "E"
        MOVE DEB-ACNO  TO DBD-ACNO
        PERFORM READ-DEBDEP THRU READ-DEBDEP-EXIT
        IF WS-F-ERROR = 0
     MOVE DEB-RESTRICT
     TO WS-RESTRICT.

       BC25.
             MOVE SPACES         TO W90-ORD W90-REG.

       BC30.
            ACCEPT W90-ORD AT 0368
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
          BACKGROUND-COLOR Magenta UPDATE AUTO.
      *    *************************************************************
      *    ****    S A L E S M A N   N U M B E R
      *    *************************************************************
             MOVE DEB-SMAN       TO W10-SMAN.
             MOVE DEB-CONTACT    TO W40-CONTACT.
    IF NOT(DEB-NCASH = "CASH SALE" OR "KONTANT V")
               IF DEB-TYPE = 1
                   MOVE DEB-H-TEL
                                 TO W40-TEL
               ELSE
                   MOVE DEB-B-TEL
                                 TO W40-TEL.

       BC35.
    IF W10-SMAN = ZERO
              ACCEPT W10-SMAN AT 0411
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
            BACKGROUND-COLOR Magenta UPDATE AUTO.
           IF W10-SMAN > 50
               GO TO BC35.
           IF W10-SMAN = ZERO
               GO TO BC40.
             ADD 100 W10-SMAN    GIVING WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF PAR-SALESMAN = SPACES
              MOVE "Invalid Salesman Number"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BC35.
            DISPLAY PAR-SALESMAN AT 0420
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.

       BC40.
             ACCEPT W40-TEL AT 0466
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
             ACCEPT W40-CONTACT AT 0528
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
    IF DEB-CAT = 79
        MOVE "C"   TO W90-EXP
        MOVE SPACES  TO W90-TAX
        GO TO BC45.
          IF DEB-INV-TYPE = "E"
               MOVE "Y"      TO WS-OPTION
    ELSE
    IF DEB-INV-TYPE = "I"
              MOVE "N"      TO WS-OPTION
    ELSE
        MOVE "X"      TO WS-OPTION.

       BC42.
      MOVE SPACES     TO WS-ERR-MES.
            STRING "TAX Exclusive (Y/N) or eXport?  ["
      DELIMITED SIZE
      WS-OPTION DELIMITED SIZE
      "]" DELIMITED SIZE
        INTO WS-ERR-MES.
            PERFORM OPT-MESSAGE TEST AFTER
       UNTIL WS-OPTION = "N" OR "Y" OR "X".
            MOVE WS-OPTION  TO W90-EXP.
           IF WS-OPTION = "Y"
               MOVE "E"          TO W90-EXP.
           IF WS-OPTION = "N"
               MOVE SPACES       TO W90-TAX
               MOVE "I"          TO W90-EXP
           ELSE
               MOVE W05-VATNO    TO W90-TAX.

       BC45.
            PERFORM CLEAR-L50.

       BC46.
      MOVE "COLLECT"  TO W90-INSTRUCT.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
              GO TO BC11.
             MOVE ZERO           TO W90-DB W90-CR.

       BC50.
             MOVE 11              TO LIN.

       BC55.
            MOVE SPACES  TO W10-DESC W10-EXT-ITEM.
             MOVE ZERO           TO W10-SELL.
             MOVE 1              TO W10-QUANT.

       BC60.
             PERFORM AA300.
      *    *************************************************************
      *    ****    G E T   S T O C K   C O D E
      *    *************************************************************
    IF WS-EXT-STK = "Y"
        ACCEPT S07-EXT AT LINE LIN
    ELSE
      *    *************************************************************
      *        ***** ALPHA-NUMERIC CODE *****
      *    *************************************************************
    IF W10-STCK = "A"
        IF WS-USE-ITM = "Y"
     ACCEPT S07-ITM AT LINE LIN
        ELSE
           ACCEPT S07 AT LINE LIN
        END-IF
    ELSE
      *    *************************************************************
      *    ***** NUMERIC CODES APPLY *****
      *    *************************************************************
    IF W10-SLNGTH = 3
        ACCEPT S07-3 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 4
        ACCEPT S07-4 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 5
        ACCEPT S07-5 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 6
        ACCEPT S07-6 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 7
        ACCEPT S07-7 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 8
        ACCEPT S07-8 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 9
        ACCEPT S07-9 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 10
        ACCEPT S07-10 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 11
        ACCEPT S07-11 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 12
        ACCEPT S07-12 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 13
        ACCEPT S07-13 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 14
        ACCEPT S07-14 AT LINE LIN.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
       GO TO BC60A
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       MOVE "I"  TO W10-ETYPE
   WHEN F3-KEY
       MOVE "A"  TO W10-ETYPE
   WHEN F4-KEY
       MOVE "D"  TO W10-ETYPE
   WHEN F5-KEY
       MOVE "2"  TO W10-ETYPE
   WHEN F6-KEY
       MOVE "X"  TO W10-ETYPE
   WHEN F9-KEY
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Lookup using any word or part in the Stock Description.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       MOVE "W"  TO W10-ETYPE
   WHEN OTHER
       CALL X"E5"
       GO TO BC60
        END-EVALUATE
              PERFORM AA50 THRU AA59
      *    *************************************************************
      *        *****   E X T E N D E D  C O D E   *****
      *    *************************************************************
        IF WS-EXT-STK = "Y"
     DISPLAY S07-EXT AT LINE LIN
        ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
        IF W10-STCK = "A"
     IF WS-USE-ITM = "Y"
         DISPLAY S07-ITM AT LINE LIN
     ELSE
               DISPLAY S07 AT LINE LIN.
              IF W10-EXT-ITEM = SPACES
                   GO TO BC60.

       BC60A.
          IF W10-EXT-ITEM = SPACES
               SUBTRACT 1        FROM WS-LAST
               GO TO BC120.

       BC61.
             PERFORM CA000.
      *    *************************************************************
      *        *****   E X T E N D E D  C O D E   *****
      *    *************************************************************
    IF WS-EXT-STK = "Y"
        DISPLAY S07-EXT AT LINE LIN
    ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
    IF W10-STCK = "A"
        IF WS-USE-ITM = "Y"
     DISPLAY S07-ITM AT LINE LIN
        ELSE
           DISPLAY S07 AT LINE LIN.
          IF WS-F-ERROR = 22
        MOVE "Z"   TO W10-ETYPE
              PERFORM AA50 THRU AA59
      *    *************************************************************
      *        *****   E X T E N D E D  C O D E   *****
      *    *************************************************************
        IF WS-EXT-STK = "Y"
     DISPLAY S07-EXT AT LINE LIN
        ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
        IF W10-STCK = "A"
     IF WS-USE-ITM = "Y"
         DISPLAY S07-ITM AT LINE LIN
     ELSE
               DISPLAY S07 AT LINE LIN
     END-IF
        END-IF
        END-IF
              IF W10-EXT-ITEM = SPACES
                   GO TO BC60
               ELSE
                   GO TO BC61.

       BC62.
    IF STK-IND = 4
        MOVE "Header Record"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO BC60.
      *    *************************************************************
      *    **** C H E C K   F O R   S A L E S R E S T R I C T I O N S
      *    *************************************************************
    IF WS-RESTRICT = "A" OR "E"
        PERFORM MA000
        IF ((WS-RESTCODE = "Y") AND (WS-RESTRICT = "E")) OR
    ((WS-RESTCODE = "N") AND (WS-RESTRICT = "A"))
     MOVE "Restricted for this Department"
                                 TO WS-ERR-STRING
           PERFORM ERROR-MESSAGE
           GO TO BC60.
            MOVE WS-LAST  TO WS-SALKEY.
             INITIALIZE SAL-RECORD1.
             MOVE WS-LAST        TO SAL-CODE.
            MOVE STK-CODE  TO SAL-EXT-ITEM.
      *    *************************************************************
      *    ****    I S   I T   A   F R E E - F O R M A T   I T E M
      *    *************************************************************
       BC65.
            PERFORM CLEAR-L50.
           IF STK-IND = 3
               MOVE ZERO         TO STK-AVGE
              ACCEPT W10-SDSC AT LINE LIN COLUMN 21
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
            BACKGROUND-COLOR Black UPDATE AUTO
               DISPLAY "Index Unit cost" AT 2512
         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
               ACCEPT S26-AVGE AT LINE 25
           ELSE
               MOVE STK-DESC     TO W10-DESC
              DISPLAY W10-SDSC AT LINE LIN COLUMN 21
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Black.
           IF STK-IND = 3
               IF STK-AVGE = ZERO
           MOVE "Unit cost is zero"
     TO WS-ERR-STRING
           PERFORM ERROR-MESSAGE
           GO TO BC65.
      *    *************************************************************
      *    ****    L O G   D E T A I L S   T O   S A L E S   F I L E
      *    *************************************************************
            MOVE W10-EXT-ITEM  TO SAL-EXT-ITEM.
             MOVE W10-DESC       TO SAL-MSER.
             MOVE STK-AVGE       TO SAL-COST W10-COST.
      *    *************************************************************
      *    ****    G E T   Q U A N T I T Y
      *    *************************************************************
      MOVE ZERO   TO W10-LENGTH W10-BREADTH W10-NUMBER.
      MOVE "N"   TO WS-MEASURE.
      MOVE 1   TO WS-P1.
    IF (LS0-LEV < 3) OR
       (STK-USE-PACKS = "N")
        GO TO BC66.
      DISPLAY "µ" AT 0750
        STK-PER WITH BACKGROUND-COLOR Magenta
       FOREGROUND-COLOR Brown HIGHLIGHT
       "Æ".
      GO TO BC66.

       BC65-PACK.
    IF NOT(STK-UNT(WS-P1) = ZERO)
        DISPLAY STK-PR(WS-P1) AT 0751
         WITH BACKGROUND-COLOR Magenta
       FOREGROUND-COLOR Brown HIGHLIGHT
    ELSE
    IF WS-P1 = 1
        DISPLAY "  Each  "  AT 0751
   WITH BACKGROUND-COLOR Magenta
        FOREGROUND-COLOR Brown HIGHLIGHT
    ELSE
        SUBTRACT 1  FROM WS-P1
        GO TO BC65-PACK.

       BC66.
      DISPLAY "F6" AT 2502
        WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Brown HIGHLIGHT
       " to key in "
        WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Blue
       "- Cutting size (Wood, Glass etc) "
        WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Grey HIGHLIGHT
       "or "
        WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Blue
       "Esc" WITH BACKGROUND-COLOR Cyan
    FOREGROUND-COLOR Brown HIGHLIGHT
       "ape" WITH BACKGROUND-COLOR Cyan
    FOREGROUND-COLOR Blue.
    IF LS0-LEV > 2
        IF STK-USE-PACKS = "Y"
     DISPLAY "" AT 2560
       WITH BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Brown HIGHLIGHT
      " for Packs"
       WITH BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Blue.
            ACCEPT S26-QNT AT LINE LIN.
    IF ADIS-FUNC
        IF LS0-LEV < 3
     GO TO BC66
        END-IF
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
       IF WS-P1 > 1
    SUBTRACT 1  FROM WS-P1
    GO TO BC65-PACK
   WHEN DOWN-KEY
       IF WS-P1 < 4
    ADD 1      TO WS-P1
    GO TO BC65-PACK
        END-EVALUATE
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
             DISPLAY WS-INV78 AT LINE LIN COLUMN 2
        WITH FOREGROUND-COLOR Cyan
             GO TO BC55
   WHEN F6-KEY
       PERFORM BB050
             DISPLAY S26-QNT AT LINE LIN
   END-EVALUATE.
          IF W10-QUANT = ZERO
              DISPLAY WS-INV78 AT LINE LIN COLUMN 2
         WITH FOREGROUND-COLOR Cyan BACKGROUND-COLOR Blue
              GO TO BC55.
      ******************************************************************
      *    T O  B E   I N C L U D E D  A T   A   L A T E R   D A T E
      *    *************************************************************
      *    ****    C H E C K   S T O C K   Q U A N T I T I E S
      *    *************************************************************
      *    IF W10-QUANT > STK-QUANT
      *        IF STK-QUANT < .01
      *     MOVE ZERO  TO W100-QNT
      *        ELSE
      *     MOVE STK-QUANT
      *     TO W100-QNT
      *        END-IF
      *        MOVE SPACES  TO WS-ERR-MES
      *        STRING "Available " W100-V2 " "
      *        "Accept/Override A/O [ ]"
      *     INTO WS-ERR-MES
      *        MOVE "A"   TO WS-OPTION
      *        PERFORM OPT-MESSAGE TEST AFTER
      *         UNTIL WS-OPTION = "A" OR "O"
      *        IF (WS-OPTION = "A")
      *     IF (STK-QUANT < .01)
      *         DISPLAY WS-INV78 AT LINE LIN COLUMN 2
      *          WITH FOREGROUND-COLOR Cyan
      *        BACKGROUND-COLOR Blue
      *         GO TO BC55
      *     ELSE
      *         MOVE STK-QUANT
      *    TO W10-QUANT
      *     END-IF
      *        ELSE
      *     PERFORM
      *
      *  GO TO BC325.
      MOVE W10-QUANT  TO W10-QORD.
      MULTIPLY STK-UNT(WS-P1) BY W10-QUANT
     GIVING W10-QNT.
      *    *************************************************************
      *    ****    G E T   S E L L I N G   P R I C E   (ACCOUNT OR CASH)
      *    *************************************************************
       BC66A.
    IF WS-P1 > 1
        MULTIPLY STK-UNT(WS-P1) BY STK-AVGE
     GIVING SAL-COST W10-COST
    ELSE
              MOVE STK-AVGE  TO SAL-COST W10-COST.
      MOVE W90-TAX  TO W10-ENTRY.
      *    *************************************************************
      *    ****    C H A N G E  F O R  M U L T I P L E   R A T E S
      *    *************************************************************
      MOVE STK-TAX  TO WS-VAT-SUB.
          IF STK-IND < 3
               IF W90-TAX NOT = SPACES
      *    *************************************************************
      *    ****    E X C L U S I V E   P R I C E S
      *    *************************************************************
           IF WS-PRC = "1"
               MOVE STK-SL(WS-P1)
                                 TO W10-SELL
           ELSE
           IF WS-PRC = "2"
               MOVE STK-CSH(WS-P1)
                                 TO W10-SELL
           ELSE
               MOVE STK-WSL(WS-P1)
                                 TO W10-SELL
              ELSE
      *    *************************************************************
      *    ****    I N C L U S I V E   P R I C E S
      *    *************************************************************
           IF WS-PRC = "1"
               MOVE W10-VSL(WS-P1)
                                 TO W10-SELL
                   ELSE
           IF WS-PRC = "2"
               MOVE W10-VCSH(WS-P1)
                                 TO W10-SELL
                   ELSE
               MOVE W10-VWSL(WS-P1)
                                 TO W10-SELL.
      MOVE W10-ENTRY  TO W90-TAX.

       BC68.
             MOVE W10-SELL       TO W10-SPAPRC.

       BC70.
             ACCEPT S26-PRC AT LINE LIN.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BC66
        END-EVALUATE.
    IF (W10-SELL < W10-SPAPRC) AND
       ((W10-SPAPRC - W10-SELL) < 0.01)
        MOVE W10-SELL  TO W10-SPAPRC.
    IF W10-SELL < W10-SPAPRC
              PERFORM CLEAR-L50
               COMPUTE WS-DISC ROUNDED = ((W10-SELL - W10-SPAPRC) /
                                           W10-SELL) * 100.00000
              DISPLAY "Discount %" AT 2518
   WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
               DISPLAY S26-DSC AT LINE 25
        DISPLAY "R" AT 2541
   WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
         "ekey or "
   WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
         "ENTER"
   WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
         " to continue"
   WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
        ACCEPT WS-OPTION AT 2568
        WITH BACKGROUND-COLOR Magenta
      FOREGROUND-COLOR Grey HIGHLIGHT AUTO
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
              PERFORM CLEAR-L50
               IF WS-OPTION = "R"
                   GO TO BC70.
    IF W10-SELL < W10-SPAPRC
              MOVE W10-SELL  TO W10-SPAPRC
              PERFORM BB100
              IF WS-ERROR = 1
           MOVE ZERO  TO WS-ERROR
           GO TO BC66A.

       BC75.
             MOVE W10-SELL       TO SAL-SELL.
      *    *************************************************************
      *    ****    C A L C U L A T E   E X T E N D E D   V A L U E
      *    *************************************************************
       BC80.
            MOVE W10-QUANT  TO SAL-QNT.
      *      MOVE W10-QORD  TO SAL-ORD.
      MOVE WS-P1   TO SAL-PACK.
             COMPUTE W25-VALUE ROUNDED = (1.000 * W10-SELL * W10-QUANT).
             MOVE W25-VALUE      TO W100-EXT SAL-VAL.
            DISPLAY W100-EXT AT LINE LIN COLUMN 70
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Black.
      *    *************************************************************
      *    ****    L I N E   D I S C O U N T   ( PERCENTAGE )
      *    *************************************************************
            PERFORM CLEAR-L50.
            DISPLAY "Discount %" AT 2518
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
             MOVE ZERO           TO WS-DISC W90-CDISC.
      *    *************************************************************
      *    ****   I F D I S C O U N T    A P P L I C A B L E
      *    G E T   P E R C E N T A G E F R O M
      *    D E P A R T M E N T F I L E
      *    *************************************************************
          IF (DEB-DISC > 0 AND < 11)
              MOVE STK-LDG  TO DPT-CODE
               PERFORM READ-DEPART THRU READ-DEPART-EXIT
              MOVE DPT-DSETT(DEB-DISC)
           TO WS-DISC
      *    *************************************************************
      *    **  M U S T  T H E  A L T E R N A T E   D I S C O U N T  **
      *    **  B E   U S E D   F O R   T H I S D E B T O R       **
      *    *************************************************************
        IF STK-ALT-DISC = "Y"
     IF DPT-ALT-DD1 = DEB-DISC
         MOVE DPT-DD11
     TO DPT-DSETT(DEB-DISC)
        WS-DISC
     ELSE
     IF DPT-ALT-DD2 = DEB-DISC
         MOVE DPT-DD12
     TO DPT-DSETT(DEB-DISC)
        WS-DISC
     END-IF
        END-IF.

       BC85.
             ACCEPT S26-DSC AT LINE 25.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
             PERFORM CLEAR-L50
       GO TO BC70
        END-EVALUATE.
          IF WS-DISC = ZERO
              PERFORM CLEAR-L50
               GO TO BC90.
             COMPUTE W10-SPAPRC ROUNDED = W10-SELL -
                    (W10-SELL * WS-DISC / 100.00000).
      *    *************************************************************
      *    ****    C H E C K   M A R G I N
      *    *************************************************************
          IF (DEB-DISC > 0 AND < 11)
              IF DPT-DSETT(DEB-DISC) = WS-DISC
                   GO TO BC86.
             PERFORM BB100.
           IF WS-ERROR = 1
               GO TO BC85.

       BC86.
            PERFORM CLEAR-L50.
             MOVE WS-DISC        TO SAL-DSC.
             COMPUTE W90-CDISC ROUNDED =
                   ((W25-VALUE * WS-DISC) / 100.00000).
             MOVE W90-CDISC      TO W100-LBAL SAL-DISC.
            DISPLAY W100-LBAL AT 2540
       WITH BACKGROUND-COLOR Magenta
     FOREGROUND-COLOR Cyan HIGHLIGHT.
      *    *************************************************************
      *    ****    A R E   T H E   D E T A I L S   C O R R E C T
      *    *************************************************************
       BC90.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
              GO TO BC60.
      *    *************************************************************
      *    ***   F R E S H   P R O D U C E   -   F O R   S P A R    ***
      *    *************************************************************
    IF (PRG-NUMBER = 3) AND (STK-LDG = "98  ")
        MOVE "Y"   TO SAL-STATUS W40-SPAR.
      *    *************************************************************
      *    ****    A C C U M U L A T E   TOTAL COST / TOTAL DISCOUNT /
      *            VALUES SUBJECT TO TAX AND NON-TAX.
      *    *************************************************************
       BC95.
          IF (STK-IND < 2) OR (STK-IND = 3)
               COMPUTE W90-BAL = W90-BAL + (W10-QUANT * W10-COST).
             ADD W90-CDISC       TO W90-LDISC.
             SUBTRACT W90-CDISC FROM W25-VALUE
                                 GIVING W90-VALUE.
          IF (STK-TAX = 0) OR (W90-EXP = "X" OR "C")
               ADD W90-VALUE     TO W90-NONTAX
               MOVE W90-VALUE    TO SAL-NONTAX
        MOVE ZERO  TO SAL-VAT-SUB
           ELSE
               MOVE W90-VALUE    TO SAL-TAXVAL
        IF W90-EXP = "I"
     COMPUTE W90-TAXVAL(WS-VAT-SUB) =
      W90-TAXVAL(WS-VAT-SUB) + (W90-VALUE -
    ((W90-VALUE / (100.0000 + W05-RATE)) *
      W05-RATE))
        ELSE
           ADD W90-VALUE TO W90-TAXVAL(WS-VAT-SUB)
        END-IF
        MOVE WS-VAT-SUB  TO SAL-VAT-SUB.
          IF (W90-TAX NOT = SPACES) AND
       (W90-EXP NOT = "C" AND "X")
               COMPUTE SAL-VAT ROUNDED = SAL-TAXVAL * W05-RTE.
    IF (W90-EXP = "I") AND (STK-TAX NOT = 0)
        COMPUTE SAL-VAT ROUNDED = (SAL-TAXVAL /
        (100.0000 + W05-RATE)) * W05-RATE.
    IF SAL-VAT-SUB > 0
        ADD SAL-VAT  TO W90-VAT(SAL-VAT-SUB).
      *    *************************************************************
      *    ****    A C C U M U L A T E   T O T A L   S A L E
      *    *************************************************************
             ADD W25-VALUE       TO W90-DB.

       BC100.
             MOVE W90-DB         TO W90-TOTAL.
             SUBTRACT W90-LDISC  FROM W90-TOTAL.
             MOVE W90-DB         TO W100-BAL.
            DISPLAY W100-BAL AT 2015
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             MULTIPLY -1 BY W90-LDISC
                                 GIVING W100-BAL.
            DISPLAY W100-BAL AT 2115
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      *    *************************************************************
      *    ****    C A L C U L A T E   T A X   ( RUNNING TOTAL )
      *    *************************************************************
          IF (W90-TAX = SPACES) OR
       (W90-EXP = "C" OR "X")
               GO TO BC102.
      MOVE 1   TO WS-S1.
      MOVE ZERO   TO W25-TOTAL.

       BC101.
          IF W90-TAXVAL(WS-S1) NOT = ZERO
              MOVE W05-VAT(WS-S1)
           TO W05-RATE
              COMPUTE W25-TOTAL ROUNDED = W25-TOTAL +
        (W90-TAXVAL(WS-S1) * W05-RTE).
    IF WS-S1 < 6
        ADD 1   TO WS-S1
        GO TO BC101.
            MOVE W25-TOTAL  TO W100-BAL.
            ADD W25-TOTAL  TO W90-TOTAL.
            DISPLAY W100-BAL AT 2215
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.

       BC102.
            MOVE W90-TOTAL  TO W100-BAL.
            DISPLAY W100-BAL AT 2315
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.

       BC105.
            MOVE "'A'ccept or 'C'ancel   [A]"
     TO WS-ERR-MES.
            MOVE "A"   TO WS-OPTION.
      *    *************************************************************
      *    ****    M U S T   T H E   L A S T   E N T R Y   B E
      *            A C C E P T E D.
      *    *************************************************************
       BC110.
      PERFORM OPT-MESSAGE TEST AFTER
       UNTIL WS-OPTION = "A" OR "C".
           IF WS-OPTION = "C"
              DISPLAY WS-INV78 AT LINE LIN COLUMN 2
         WITH FOREGROUND-COLOR Cyan BACKGROUND-COLOR Blue
               PERFORM BC114
               PERFORM BC100 THRU BC102
               GO TO BC55.

       BC111.
             MOVE WS-SUPER       TO SAL-SUPER.
             PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
             ADD 1               TO WS-LAST.
      *    *************************************************************
      *    ****    T E S T   F O R   C U T T I N G   S P E C S
      *    *************************************************************
    IF W10-LENGTH > ZERO AND
       W10-BREADTH > ZERO AND
       W10-NUMBER > ZERO
              IF LIN < 41
           ADD 1  TO LIN
        ELSE
           MOVE 11  TO WS-S7
           PERFORM BC111-SHIFT
        END-IF
              MOVE WS-LAST  TO WS-SALKEY
              INITIALIZE SAL-RECORD1
              MOVE WS-LAST  TO SAL-CODE
        MOVE SPACES  TO W10-REMKS
        MOVE W10-LENGTH  TO W100-LENGTH
        MOVE W10-BREADTH  TO W100-BREADTH
        MOVE W10-NUMBER  TO W100-NUMBER
        STRING W100-LENGTH DELIMITED SIZE
        " by " DELIMITED SIZE
        W100-BREADTH DELIMITED SIZE
        " X " DELIMITED SIZE
        W100-NUMBER DELIMITED SIZE
        INTO W10-REMKS
              MOVE W10-REMKS  TO SAL-MSER
        MOVE WS-SUPER  TO SAL-SUPER
              PERFORM WRITE-SALE THRU WRITE-SALE-EXIT
              DISPLAY W10-REMKS AT LINE LIN COLUMN 21
         WITH FOREGROUND-COLOR Grey HIGHLIGHT
       BACKGROUND-COLOR Black UPDATE AUTO
              ADD 1   TO WS-LAST.
      *    *************************************************************
      *    ****    T E S T   I F   S C R E E N   F U L L
      *    *************************************************************
           IF LIN < 41
               ADD 1             TO LIN
               GO TO BC112.
      PERFORM SAVE-SCREEN-2.
      MOVE 11   TO WS-S7.
      *    *************************************************************
      *    ****    S C R O L L  D E T A I L   L I N E S   U P
      *    *************************************************************
       BC111-SHIFT.
      ADD 1 WS-S7  GIVING WS-S8.
      MOVE W42-LINES(WS-S8)
     TO W42-LINES(WS-S7).
      MOVE W43-LINE(WS-S8)
     TO W43-LINE(WS-S7).
    IF WS-S8 < 41
        ADD 1   TO WS-S7
        GO TO BC111-SHIFT.
      PERFORM RESTORE-SCREEN-2.
      *    *************************************************************
      *    ****    C L E A R   L A S T  D E T A I L   L I N E
      *    *************************************************************
            DISPLAY WS-INV1 AT LINE LIN COLUMN 1
       WITH BACKGROUND-COLOR Black FOREGROUND-COLOR Cyan
       WS-INV78 WITH FOREGROUND-COLOR Cyan
       WS-INV80 WITH BACKGROUND-COLOR Black
       FOREGROUND-COLOR Cyan.
      ADD 1   TO WS-SALPOS.

       BC112.
      MOVE SPACES     TO WS-ERR-MES.
            STRING "'I'nternal notes,'R'emarks,'N'ext or 'E'nd  ["
      DELIMITED SIZE
      WS-IN-OPT DELIMITED SIZE
      "]" DELIMITED SIZE
     INTO WS-ERR-MES.
            MOVE WS-IN-OPT  TO WS-OPTION.
      PERFORM OPT-MESSAGE TEST AFTER
       UNTIL WS-OPTION = "I" OR "R" OR "N" OR "E".
             MOVE WS-OPTION      TO WS-IN-OPT.
           IF WS-OPTION = "N"
               GO TO BC55.
           IF WS-OPTION = "E"
               SUBTRACT 1        FROM WS-LAST
               GO TO BC120.
             MOVE SPACES         TO W10-REMKS.
            ACCEPT W10-REMKS AT LINE LIN COLUMN 21
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
          BACKGROUND-COLOR Black UPDATE AUTO.
           IF W10-REMKS = SPACES
               GO TO BC112.
            MOVE WS-LAST  TO WS-SALKEY.
             INITIALIZE SAL-RECORD1.
             MOVE WS-LAST        TO SAL-CODE.
             MOVE W10-REMKS      TO SAL-MSER.
           IF WS-OPTION = "I"
        DISPLAY "(I)" AT LINE LIN COLUMN 5
   WITH FOREGROUND-COLOR Brown HIGHLIGHT
               MOVE 1            TO SAL-SUPER
           ELSE
        DISPLAY "(R)" AT LINE LIN COLUMN 5
   WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             GO TO BC111.

       BC114.
             SUBTRACT W25-VALUE  FROM W90-DB.
             SUBTRACT W90-CDISC  FROM W90-LDISC.
             SUBTRACT SAL-NONTAX FROM W90-NONTAX.
    IF SAL-VAT-SUB > ZERO
              SUBTRACT SAL-TAXVAL FROM W90-TAXVAL(SAL-VAT-SUB)
        SUBTRACT SAL-VAT  FROM W90-VAT(SAL-VAT-SUB).
          IF (STK-IND < 2) OR (STK-IND = 3)
               COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).
      *    *************************************************************
      *    ****   C L E A R   S C R E E N   L I N E / S
      *    *************************************************************
       BC115.
             DISPLAY S30-MSO-LIN AT LINE LIN.
             ADD 1                   TO LIN.
      *    *************************************************************
      *    ****    W A S   T H E R E   A N   E N T R Y
      *    *************************************************************
       BC120.
          IF WS-LAST = ZERO
               GO TO BC05.
      *    *************************************************************
      *    ****    P R O C E S S   T H E   S A L E
      *    *************************************************************
       BC125.
            MOVE "'P'rocess, 'A'mend, 'C'ancel or 'M'ore items"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.

       BC130.
      PERFORM OPT-MESSAGE
       UNTIL WS-OPTION = "P" OR "A" OR "C" OR "M".
           IF WS-OPTION = "M"
               ADD 1             TO WS-LAST
               GO TO BC112.
           IF WS-OPTION = "A"
              PERFORM BC270A
               GO TO BC125.
           IF WS-OPTION = "C"
              GO TO BC265.
             MOVE ZERO           TO W90-STOTAL WS-TDISC W90-TDISC.
           IF W90-LDISC NOT = ZERO
               MOVE W90-LDISC    TO W90-TDISC
               GO TO BC140.
             MOVE W90-DB         TO W90-DVAL.

       BC140.
             SUBTRACT W90-TDISC FROM W90-DB
                                 GIVING W90-TSELL W90-TOTAL.
             MULTIPLY -1 BY W90-TDISC
                                 GIVING W100-BAL.
            DISPLAY W100-BAL AT 2115
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            MOVE ZERO   TO W90-TTAX
        W90-EXCTOT.
      MOVE 1   TO WS-P1.
          IF (W90-TAX = SPACES) OR (W90-EXP = "C" OR "X")
              GO TO BC143.
      *    *************************************************************
      *    ****    C A L C U L A T E   T O T A L   V A T
      *    *************************************************************
       BC141.
           IF (W90-TAXVAL(WS-P1) = ZERO)
               GO TO BC142.
             MOVE W05-VAT(WS-P1)     TO W05-RATE.
             COMPUTE W90-TTAX ROUNDED = (W90-TAXVAL(WS-P1) * W05-RTE).
             ADD W90-TAXVAL(WS-P1)   TO W90-EXCTOT.
      *    *************************************************************
      *    ****    C H E C K   I F   A L L   V A T   R A T E S
      *     H A V E   B E E N   C A L C U L A T E D
      *    *************************************************************
       BC142.
    IF WS-P1 < 6
        ADD 1   TO WS-P1
        GO TO BC141.
             MOVE W90-TTAX           TO W100-BAL.
             DISPLAY W100-BAL AT 2215 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             ADD W90-TTAX            TO W90-TOTAL.

       BC143.
             MOVE W90-TOTAL  TO W100-BAL.
             DISPLAY W100-BAL AT 2315 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             PERFORM CLEAR-L50.
      *    *************************************************************
      *    ****    P R O C E S S   T H E   O R D E R
      *    *************************************************************
       BC145.
             DISPLAY "Processing Order" AT 2520 WITH
             BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
             MOVE ZERO           TO WS-SALKEY.
      GO TO BC216.
      *    *************************************************************
      *    ****    T E S T   F O R   I N D I V I D U A L
      *    *************************************************************
       BC189.
           IF W80-TYPE NOT = 1
               GO TO BC210.
      *       MOVE DEB-NAME       TO W60-NAME.
      *       PERFORM CA165-NAME.
      *    *************************************************************
      *    ****    T I T L E
      *    *************************************************************
           IF DEB-LANG = "A"
              MOVE W75-ATITLE(DEB-TITLE)
                                 TO W60-NAME
           ELSE
              MOVE W75-ETITLE(DEB-TITLE)
                                 TO W60-NAME.
             MOVE 7              TO WS-S6.

       BC190.
          IF W60-NCHAR(WS-S6) NOT = SPACE
               GO TO BC195.
             SUBTRACT 1          FROM WS-S6.
           IF WS-S6 > 0
               GO TO BC190.
      *    *************************************************************
      *    ****    F I R S T   I N I T I A L
      *    *************************************************************
       BC195.
            MOVE 1   TO WS-S5.
             ADD 2               TO WS-S6.
            MOVE W40-NCHAR(25) TO W60-NCHAR(WS-S6).
             ADD 1               TO WS-S6.
            MOVE "."   TO W60-NCHAR(WS-S6).
          IF W40-NCHAR(49) = SPACE
               GO TO BC200.
      *    *************************************************************
      *    ****    S E C O N D   I N I T I A L
      *    *************************************************************
             ADD 1               TO WS-S6.
            MOVE W40-NCHAR(49) TO W60-NCHAR(WS-S6).
             ADD 1               TO WS-S6.
            MOVE "."   TO W60-NCHAR(WS-S6).

       BC200.
             ADD 2               TO WS-S6.
      *    *************************************************************
      *    ****    S U R N A M E
      *    *************************************************************
       BC205.
            MOVE W40-NCHAR(WS-S5)
           TO W60-NCHAR(WS-S6).
           IF WS-S5 < 24
               ADD 1             TO WS-S5
               ADD 1             TO WS-S6
               GO TO BC205.
             MOVE W60-NAME       TO W40-NAME.

       BC210.
            MOVE ZERO   TO WS-PAGE.
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
       BC215.
        EXIT.
      *    *************************************************************
      *    ****    S T A R T   R E C O V E R Y   L O G G I N G
      *    *************************************************************
       BC216.
            PERFORM AY60 THRU AY999.
    IF W70-ORD-SYS = "Y"
        MOVE LS0-NO  TO USE-NO
        PERFORM READ-SYSUSER-LOCK THRU READ-SYSUSER-EXIT
        MOVE USE-NO  TO W90-USER
        MOVE USE-ORD  TO W90-SEQ
    ELSE
              MOVE W70-ORDER  TO W90-REF.
            DISPLAY W90-REF AT 0510
       WITH FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Magenta.
    IF W70-ORD-SYS = "Y"
        IF USE-ORD = 99999
     MOVE 1  TO USE-ORD
        ELSE
     ADD 1  TO USE-ORD
        END-IF
        PERFORM REWRITE-SYSUSER-UNLOCK THRU WRITE-SYSUSER-EXIT
    ELSE
          IF W70-ORDER = 99999999
              MOVE 1   TO W70-ORDER
          ELSE
              ADD 1   TO W70-ORDER.
      *    *************************************************************
      *    ****    S T A R T   O R D E R   L O G G I N G
      *    *************************************************************
       BC220.
             MOVE W90-REF            TO SOR-REF.
             MOVE 100                TO SOR-SQN.
             MOVE ZERO               TO SOR-TYPE.
             MOVE W80-ACNO           TO SOR-AC.
             MOVE W40-NAME           TO SOR-NAME.
             MOVE W12-T-YMD          TO SOR-DTE.
             MOVE ZERO               TO SOR-PICNO.
             MOVE W90-ORD            TO SOR-ORD.
             MOVE W10-SMAN           TO SOR-SMAN.
             MOVE "O"                TO SOR-DOC.
             MOVE ZERO               TO SOR-PAGE.
             MOVE W90-REG            TO SOR-OREF.
             MOVE W90-EXP            TO SOR-EXP.
             MOVE WS-PRC             TO SOR-PRC.
             MOVE W40-TEL            TO SOR-TEL.
             MOVE W40-CONTACT        TO SOR-CONTACT.
           IF W40-AD1 NOT = SPACES
               PERFORM BC225
               MOVE 5                TO SOR-TYPE
               ADD 1                 TO SOR-SEQ
               MOVE W40-AD1          TO SOR-AD1
               MOVE W40-AD2          TO SOR-AD2
               MOVE W40-AD3          TO SOR-AD3
               MOVE W40-AD4          TO SOR-AD4
               MOVE W40-PC           TO SOR-PC.
      *    *************************************************************
      *    ****    L O G   R E C O R D   T O   O R D E R   F I L E
      *    *************************************************************
       BC225.
             PERFORM WRITE-SORDER THRU WRITE-SORDER-EXIT.
             MOVE 1                  TO WS-ACTION.
             PERFORM AY17 THRU AY59.
      *    *************************************************************
      *    ****    R E A D   T H E   S A L E   F I L E   A N D
      *            L O G   T O   O R D E R   F I L E
      *    *************************************************************
       BC235.
             ADD 1                   TO WS-SALKEY.
           IF WS-SALKEY > WS-LAST
               GO TO BC255.
             PERFORM READ-SALE THRU READ-SALE-EXIT.
          IF SAL-EXT-ITEM = SPACES
               GO TO BC247.
             MOVE 1                  TO SOR-TYPE.
             MOVE SAL-EXT-ITEM       TO SOR-EXT-ITEM.
             MOVE SAL-DESC           TO SOR-DESC.
             MOVE SAL-QNT            TO SOR-QNT.
      *        SOR-QORD.
             MOVE SAL-COST           TO SOR-COST.
             MOVE SAL-VAT            TO SOR-VAT.
             MOVE SAL-VAT-SUB        TO SOR-VAT-SUB.
             MOVE SAL-SELL           TO SOR-SELL.
             MOVE SAL-VAL            TO SOR-VAL.
             MOVE SAL-DISC           TO SOR-DISC.
             MOVE SAL-DSC            TO SOR-DSC.
             MOVE SAL-INCTAX         TO SOR-INCTAX.
             MOVE SAL-NONTAX         TO SOR-NONTAX.
             MOVE SAL-TAXVAL         TO SOR-TAXVAL.
             MOVE SAL-PACK           TO SOR-PACK.
             MOVE SPACES             TO SOR-SERNO.
             ADD 1                   TO SOR-SEQ.
             PERFORM BC225.
      *    *************************************************************
      *    ****    U P D A T E  P I C K I N G  S L I P   Q T Y
      *     O N  S T O C K   R E C O R D
      *    *************************************************************
             MOVE SAL-EXT-ITEM        TO STK-CODE.
             MOVE 0                   TO WS-ACTION.
             PERFORM AY05 THRU AY59.
             SUBTRACT SOR-QNT  FROM STK-QUANT.
             ADD SOR-QNT              TO STK-QPIC.
             PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
             PERFORM BC248 THRU BC254.
             GO TO BC235.

       BC247.
           IF SAL-SUPER = 1
               MOVE 4            TO SOR-TYPE
           ELSE
               MOVE 3            TO SOR-TYPE.
             INITIALIZE SOR-DET2.
             MOVE SAL-MSER       TO SOR-SERIAL.
             ADD 1               TO SOR-SEQ.
             GO TO BC225.
       BC248.
      MOVE SOR-EXT-ITEM  TO PRT-EXT-ITEM.
            PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
           IF WS-F-ERROR = 21
               GO TO BC254.
      *    *************************************************************
      *    ****  P R O D U C T I O N   D E T A I L S
      *    *************************************************************
             MOVE 1              TO WS-S6.
       BC249.
          IF PRT-EXT-CODE(WS-S6) = SPACES
               ADD 1             TO WS-S6
               GO TO BC250.
            MOVE PRT-EXT-CODE(WS-S6)
     TO W10-EXT-ITEM.
             PERFORM CA000.
           IF WS-F-ERROR = 22
               ADD 1             TO WS-S6
               GO TO BC250.
             MOVE 2              TO SOR-TYPE.
             INITIALIZE SOR-DET2.
      MOVE STK-CODE  TO SOR-EXT-ITEM.
             MOVE STK-DESC       TO SOR-DESC.
      MULTIPLY SAL-QNT BY PRT-QUANT(WS-S6)
                                 GIVING SOR-QNT.
             ADD 1               TO SOR-SEQ.
      PERFORM BC225.
            ADD 1   TO WS-S6.
       BC250.
           IF WS-S6 < 31
               GO TO BC249.
       BC254.
             EXIT.
      *    *************************************************************
      *    ****    O R D E R   T O T A L S
      *    *************************************************************
       BC255.
            INITIALIZE SOR-DET3.
             MOVE 9              TO SOR-TYPE.
             MOVE W90-DB         TO SOR-SUBTOT.
             MULTIPLY -1 BY W90-TDISC
                                 GIVING SOR-DSCNT
            MOVE W90-EXCTOT  TO SOR-TAXABLE.
             MOVE W90-NONTAX     TO SOR-NON-TXBL.
             MOVE W90-TTAX       TO SOR-TAX.
             MOVE W90-TOTAL      TO SOR-TOT.
             MOVE W90-BAL        TO SOR-BAL.
      MOVE W90-PMETH  TO SOR-PMETH.
             MOVE W40-STATUS     TO SOR-STATUS.
             MOVE W40-HTEL       TO SOR-HTEL.
             MOVE W40-ID         TO SOR-ID.
            MOVE 99900   TO SOR-SQN.
             MOVE W90-INSTRUCT   TO SOR-INSTRUCT.
             PERFORM BC225.
      *    *************************************************************
      *    ****    E N D   O F F   T H E   R E C O V E R Y   L O G
      *    *************************************************************
             PERFORM AY70 THRU AY999.

       BC260.
            MOVE "Print picking slip (Y/N)?  [ ]"
     TO WS-ERR-MES.
            MOVE "Y"   TO WS-OPTION.
      PERFORM OPT-MESSAGE TEST AFTER
       UNTIL WS-OPTION = "Y" OR "N".
           IF WS-OPTION = "Y"
               PERFORM ERASE-SCREEN
              DISPLAY "NEW ORDER" AT 0236
         WITH FOREGROUND-COLOR Grey HIGHLIGHT
               DISPLAY "Printing picking slip" AT 1212
         WITH FOREGROUND-COLOR Brown HIGHLIGHT
              MOVE W90-REF  TO LS-SLS-ORDNO
               PERFORM BB200.
      *    *************************************************************
      *    ****    C L O S E   F I L E S
      *            ( FORCE SYSTEM TO WRITE DETAILS TO DISK )
      *    *************************************************************
             CLOSE SORDER
                   SALE.
             OPEN OUTPUT SALE.
             CLOSE SALE.
             OPEN I-O SALE
                      SORDER.
            MOVE "ENTER to continue - E to exit"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF WS-OPTION = "E"
              GO TO BC265.
             GO TO BC05.

       BC265.
      EXIT.
      *    *************************************************************
      *    ****    A M E N D   S A L E S   D E T A I L S
      *    *************************************************************
       BC270A  SECTION 50.
       BC270.
      MOVE "Amend - 'H'eader, 'S'ales details or 'E'xit"
     TO WS-ERR-MES.
      MOVE "S"   TO WS-OPTION.

       BC271.
      PERFORM OPT-MESSAGE TEST AFTER
       UNTIL WS-OPTION = "E" OR "H" OR "S".
    IF WS-OPTION = "S"
               GO TO BC272
           ELSE
           IF WS-OPTION = "E"
        GO TO BC582.

       BC271A.
      MOVE "Amend-'O'rd No,'S'alesman,'T'el.,'C'ont.,'E'xit"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.

       BC271B.
      PERFORM OPT-MESSAGE
       UNTIL WS-OPTION = "C" OR "E" OR "O" OR "S" OR "T".
           IF WS-OPTION = "E"
        GO TO BC582.
           IF WS-OPTION = "C"
               ACCEPT W40-CONTACT AT 0528
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta UPDATE AUTO
               GO TO BC271A
           ELSE
           IF WS-OPTION = "O"
               ACCEPT W90-ORD AT 0368
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta UPDATE AUTO
               GO TO BC271A
           ELSE
           IF WS-OPTION = "T"
               ACCEPT W40-TEL AT 0466
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta UPDATE AUTO
               GO TO BC271A.

       BC271C.
            ACCEPT W10-SMAN AT 0411
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
          BACKGROUND-COLOR Magenta UPDATE AUTO.
           IF W10-SMAN > 50
               GO TO BC271C.
           IF W10-SMAN = ZERO
        DISPLAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" AT 0420
                       WITH FOREGROUND-COLOR Cyan
               GO TO BC271A.
            PERFORM CLEAR-L50.
             ADD 100 W10-SMAN    GIVING WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF PAR-SALESMAN = SPACES
              MOVE "Invalid Salesman Number"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
              GO TO BC271C.
            DISPLAY PAR-SALESMAN AT 0420
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      MOVE W10-SMAN  TO SOR-SMAN.
             PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT.
      GO TO BC271A.

       BC272.
             MOVE 11                 TO LIN.
             PERFORM BC115 UNTIL LIN > 41
             MOVE 11                 TO LIN.
             MOVE 1              TO WS-LINES WS-FIRST.

       BC275.
            MOVE WS-FIRST  TO WS-SALKEY WS-SALPOS.
      *    *************************************************************
      *    ****    H A V E   A L L   T H E   E N T R I E S   B E E N
      *            D I S P L A Y E D
      *    *************************************************************
       BC280.
          IF WS-SALKEY > WS-LAST
              GO TO BC284.
             PERFORM READ-SALE THRU READ-SALE-EXIT.
      MOVE SAL-EXT-ITEM  TO W10-EXT-ITEM.
      *    *************************************************************
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *    *************************************************************
          IF SAL-EXT-ITEM = SPACES
              DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
         WITH FOREGROUND-COLOR Grey HIGHLIGHT
       BACKGROUND-COLOR Black
               IF SAL-SUPER = 1
     DISPLAY "(I)" AT LINE LIN COLUMN 5
       WITH FOREGROUND-COLOR Brown HIGHLIGHT
               ELSE
     DISPLAY "(R)" AT LINE LIN COLUMN 5
       WITH FOREGROUND-COLOR Grey HIGHLIGHT
               END-IF
               GO TO BC282.
      *    *************************************************************
      *        *****   E X T E N D E D  C O D E   *****
      *    *************************************************************
    IF WS-EXT-STK = "Y"
        DISPLAY S07-EXT AT LINE LIN
    ELSE
      *    *************************************************************
      *    *****   A L P H A - N U M E R I C   C O D E  *****
      *    *************************************************************
    IF W10-STCK = "A"
        IF WS-USE-ITM = "Y"
     DISPLAY S07-ITM AT LINE LIN
        ELSE
           DISPLAY S07 AT LINE LIN.
            DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Black.
             MOVE SAL-QNT        TO W10-QUANT.
      *      MOVE SAL-ORD  TO W10-QORD.
             DISPLAY S26-QNT AT LINE LIN.
             MOVE SAL-SELL       TO W10-SELL.
             DISPLAY S26-PRC AT LINE LIN.
             MOVE SAL-VAL        TO W100-EXT.
            DISPLAY W100-EXT AT LINE LIN COLUMN 70
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Black.
      *    *************************************************************
      *    ****    I S   T H E   S C R E E N   F U L L
      *    *************************************************************
       BC282.
           IF LIN < 41
               ADD 1             TO LIN WS-SALKEY WS-LINES
               GO TO BC280.

       BC284.
             MOVE 11                 TO LIN.
             PERFORM CLEAR-L50.
      *    *************************************************************
      *    ****    D I S P L A Y   O P T I O N S
      *    *************************************************************
       BC285.
             DISPLAY " " AT 2503 WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Cyan
                         " highlight," WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan
                                    "PgUp" WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Cyan
                                    "/" WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan
                                    "PgDn" WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Cyan
                                    "," WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan
                                    "A" WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Cyan
                                    "mend," WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan
                                    "D" WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Cyan
                                    "elete or " WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan
                                    "Esc" WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Cyan
                                       "ape" WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan.
      *    *************************************************************
      *    ****    S A V E   C U R R E N T   P A G E
      *    *************************************************************
             PERFORM SAVE-SCREEN-2.
      *    *************************************************************
      *    ****    S C R E E N  A T T R I B U T E S
      *    *************************************************************
       BC289.
             MOVE 3920               TO W41-LENGTH.
             CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS W42-ATTRIB2 W41-LENGTH RETURNING WS-STATUS.
             MOVE 4000               TO W41-LENGTH.
      *    *************************************************************
      *    ****    G E T   U S E R   O P T I O N
      *    *************************************************************
       BC290.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY    GO TO BC292
                 WHEN DOWN-KEY  GO TO BC293
                 WHEN OTHER     CALL X"E5"
               END-EVALUATE
               GO TO BC290
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       PERFORM CLEAR-L50
       GO TO BC582
      *    *************************************************************
      *    ****    I S   N E X T   S C R E E N   R E Q U I R E D
      *    *************************************************************
   WHEN PAGE-DOWN
             GO TO BC305
      *    *************************************************************
      *    ****    I S   P R E V I O U S   S C R E E N   R E Q U I R E D
      *    *************************************************************
   WHEN PAGE-UP
             GO TO BC300
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO BC290
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
        IF (WS-OPTION = "A" OR "D") AND
    (LIN > 10)
     PERFORM CLEAR-L50
     GO TO BC310.
      CALL X"E5".
            GO TO BC290.
      *    *************************************************************
      *    ****    U P  K E Y  ( M O V E   H I G H L I G H T  U P )
      *    *************************************************************
       BC292.
    IF (WS-SALPOS = 1) OR (LIN < 11)
        GO TO BC294.
      SUBTRACT 1   FROM WS-SALPOS.
    IF LIN > 11
        SUBTRACT 1  FROM LIN
        GO TO BC294.
      MOVE 41   TO WS-S7.
      *    *************************************************************
      *    ****     M O V E   T H E   D E T A I L S   D O W N
      *      O N E   L I N E
      *    *************************************************************
       BC292-LOOP.
      SUBTRACT 1 FROM WS-S7
     GIVING WS-S8.
      MOVE W42-LINES(WS-S8)
     TO W42-LINES(WS-S7).
      MOVE W43-LINE(WS-S8)
     TO W43-LINE(WS-S7).
    IF WS-S7 > 9
        SUBTRACT 1  FROM WS-S7
        GO TO BC292-LOOP.
      MOVE WS-SALPOS  TO WS-SALKEY WS-FIRST.
      *    *************************************************************
      *    ****    C L E A R   F I R S T   O R  L A S T
      *     D E T A I L  L I N E.
      *    *************************************************************
       BC292-DISPLAY.
      PERFORM RESTORE-SCREEN-2.
            DISPLAY WS-INV1 AT LINE LIN COLUMN 1
       WITH BACKGROUND-COLOR Black FOREGROUND-COLOR Cyan
       WS-INV78 WITH FOREGROUND-COLOR Cyan
       WS-INV80 WITH BACKGROUND-COLOR Black
       FOREGROUND-COLOR Cyan.
      *    *************************************************************
      *    ****    G E T   N E X T   O R   P R E V I O U S
      *     D E T A I L  L I N E.
      *    *************************************************************
            PERFORM READ-SALE THRU READ-SALE-EXIT.
      MOVE SAL-EXT-ITEM  TO W10-EXT-ITEM.
      *    *************************************************************
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *    *************************************************************
          IF SAL-EXT-ITEM = SPACES
              DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
         WITH FOREGROUND-COLOR Grey HIGHLIGHT
       BACKGROUND-COLOR Black
        IF SAL-SUPER = 1
     DISPLAY "(I)" AT LINE LIN COLUMN 5
       WITH FOREGROUND-COLOR Brown HIGHLIGHT
               ELSE
     DISPLAY "(R)" AT LINE LIN COLUMN 5
       WITH FOREGROUND-COLOR Grey HIGHLIGHT
        END-IF
              GO TO BC292-END-DISP.
      *    *************************************************************
      *        *****   E X T E N D E D  C O D E   *****
      *    *************************************************************
    IF WS-EXT-STK = "Y"
        DISPLAY S07-EXT AT LINE LIN
    ELSE
      *    *************************************************************
      *    *****   A L P H A - N U M E R I C   C O D E  *****
      *    *************************************************************
    IF W10-STCK = "A"
        IF WS-USE-ITM = "Y"
     DISPLAY S07-ITM AT LINE LIN
        ELSE
           DISPLAY S07 AT LINE LIN.
            DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Black.
             MOVE SAL-QNT        TO W10-QUANT.
      *      MOVE SAL-ORD  TO W10-QORD.
             DISPLAY S26-QNT AT LINE LIN.
             MOVE SAL-SELL       TO W10-SELL.
             DISPLAY S26-PRC AT LINE LIN.
             MOVE SAL-VAL        TO W100-EXT.
            DISPLAY W100-EXT AT LINE LIN COLUMN 70
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Black.

       BC292-END-DISP.
      PERFORM SAVE-SCREEN-2.
      GO TO BC294.
      *    *************************************************************
      *    ****    D O W N   K E Y   ( M O V E  H I G H L I G H T
      *           D O W N )
      *    *************************************************************
       BC293.
    IF (WS-SALPOS = WS-LAST) AND (LIN > 10)
        GO TO BC294.
      *    *************************************************************
      *    ****    WS-SALPOS CONTAINS KEY OF HIGHLIGHTED ITEM
      *    *************************************************************
    IF LIN > 10
        ADD 1   TO WS-SALPOS.
    IF LIN < 41
        ADD 1   TO LIN
        GO TO BC294.
      MOVE 11   TO WS-S7.
      *    *************************************************************
      *    ****     M O V E   T H E   D E T A I L S   U P   O N E
      *      L I N E
      *    *************************************************************
       BC293-LOOP.
      ADD 1 WS-S7  GIVING WS-S8.
      MOVE W42-LINES(WS-S8)
     TO W42-LINES(WS-S7).
      MOVE W43-LINE(WS-S8)
     TO W43-LINE(WS-S7).
    IF WS-S8 < 41
        ADD 1   TO WS-S7
        GO TO BC293-LOOP.
      ADD 1   TO WS-FIRST.
      MOVE WS-SALPOS  TO WS-SALKEY.
      GO TO BC292-DISPLAY.
      *    *************************************************************
      *    ****    H I G H L I G H T   C U R R E N T   L I N E
      *    *************************************************************
       BC294.
            PERFORM BC289.
    IF LIN > 10
              DISPLAY W43-DET(LIN) AT LINE LIN COLUMN 1
         WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
            GO TO BC290.
      *    *************************************************************
      *    ****    C L E A R   S C R E E N
      *    *************************************************************
       BC295.
             MOVE 11                 TO LIN.
             PERFORM BC115 UNTIL LIN > 41
             MOVE 11                 TO LIN.
             MOVE 1              TO WS-LINES.
      *    *************************************************************
      *    ****   P R E V I O U S   S C R E E N   R E Q U I R E D
      *    *************************************************************
       BC300.
    IF WS-FIRST = 1
        GO TO BC290.
    IF WS-FIRST > 31
        SUBTRACT 31  FROM WS-FIRST
    ELSE
        MOVE 1   TO WS-FIRST.
      PERFORM BC295.
      GO TO BC275.
      *    *************************************************************
      *    ****    N E X T   S C R E E N   R E Q U I R E D
      *    *************************************************************
       BC305.
    IF (WS-FIRST + 31) > WS-LAST
        GO TO BC290.
      ADD 31   TO WS-FIRST.
      PERFORM BC295.
      GO TO BC275.
      *    *************************************************************
      *    ****    M U S T   A N   I T E M   B E   A M E N D E D
      *    *************************************************************
       BC310.
          IF WS-OPTION = "A"
              GO TO BC320.
      *    *************************************************************
      *    ****    M A K E   S U R E   T H A T   A N   I T E M
      *            M U S T   B E   D E L E T E D
      *    *************************************************************
       BC315.
            MOVE "Are you sure Y/N?  [ ]"
     TO WS-ERR-MES.
            MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE
       UNTIL WS-OPTION = "N" OR "Y".
           IF WS-OPTION = "N"
               GO TO BC285.
             MOVE "D"            TO WS-OPTION.
      *    *************************************************************
      *    ****    R E A D   T H E   D E T A I L S   F R O M   T H E
      *            S A L E   F I L E
      *    *************************************************************
       BC320.
            MOVE WS-SALPOS  TO WS-SALKEY.
             PERFORM READ-SALE THRU READ-SALE-EXIT.
          IF SAL-EXT-ITEM = SPACES
              GO TO BC322.
            MOVE SAL-EXT-ITEM  TO W10-EXT-ITEM STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
             MOVE SAL-QNT        TO W10-QUANT.
      *      MOVE SAL-ORD  TO W10-QORD.
             MOVE SAL-COST       TO W10-COST.
             MOVE SAL-SELL       TO W10-SELL.
             MOVE SAL-DISC       TO W90-CDISC.
             MOVE SAL-DSC        TO WS-DISC.
             MOVE SAL-VAL        TO W25-VALUE.
      MOVE SAL-VAT-SUB  TO WS-VAT-SUB.
      MOVE SAL-PACK  TO WS-P1.
      *    *************************************************************
      *    ****    R E D U C E   T O T A L S
      *    *************************************************************
             SUBTRACT W25-VALUE  FROM W90-DB.
             SUBTRACT W90-CDISC  FROM W90-LDISC.
             SUBTRACT SAL-NONTAX FROM W90-NONTAX.
    IF WS-VAT-SUB > ZERO
              SUBTRACT SAL-TAXVAL FROM W90-TAXVAL(WS-VAT-SUB)
        SUBTRACT SAL-VAT  FROM W90-VAT(SAL-VAT-SUB).
          IF (STK-IND < 2) OR (STK-IND = 3)
               COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).
      *    *************************************************************
      *    ****    I S   I T   A N   A M E N D M E N T
      *    *************************************************************
       BC322.
      PERFORM BC289.
           IF WS-OPTION = "A"
               GO TO BC325.
      *    *************************************************************
      *    ****    R E - D I S P L A Y   T O T A L S
      *    *************************************************************
          IF SAL-EXT-ITEM NOT = SPACES
               PERFORM BC100 THRU BC102.
      *    *************************************************************
      *    ****    D E L E T E   I T E M   F R O M   S A L E   F I L E
      *    *************************************************************
       BC324.
            MOVE WS-SALKEY  TO WS-SALSTR.
             ADD 1               TO WS-SALKEY.
           IF WS-SALKEY > WS-LAST
               SUBTRACT 1        FROM WS-LAST
               GO TO BC272.
             PERFORM READ-SALE THRU READ-SALE-EXIT.
             SUBTRACT 1          FROM SAL-CODE.
             MOVE WS-SALSTR      TO WS-SALKEY.
             PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
             ADD 1               TO WS-SALKEY.
            GO TO BC324.

       BC325.
      PERFORM BC289.
          IF SAL-EXT-ITEM = SPACES
              ACCEPT SAL-SDSC AT LINE LIN COLUMN 21
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
            BACKGROUND-COLOR Black UPDATE AUTO
              GO TO BC581.

       BC330.
            PERFORM AA300.
      *    *************************************************************
      *    ****    G E T   S T O C K   C O D E
      *    *************************************************************
    IF WS-EXT-STK = "Y"
        ACCEPT S07-EXT AT LINE LIN
    ELSE
      *    *************************************************************
      *        ***** ALPHA-NUMERIC CODE *****
      *    *************************************************************
    IF W10-STCK = "A"
        IF WS-USE-ITM = "Y"
     ACCEPT S07-ITM AT LINE LIN
        ELSE
           ACCEPT S07 AT LINE LIN
        END-IF
    ELSE
      *    *************************************************************
      *    ***** NUMERIC CODES APPLY *****
      *    *************************************************************
    IF W10-SLNGTH = 3
        ACCEPT S07-3 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 4
        ACCEPT S07-4 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 5
        ACCEPT S07-5 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 6
        ACCEPT S07-6 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 7
        ACCEPT S07-7 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 8
        ACCEPT S07-8 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 9
        ACCEPT S07-9 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 10
        ACCEPT S07-10 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 11
        ACCEPT S07-11 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 12
        ACCEPT S07-12 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 13
        ACCEPT S07-13 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 14
        ACCEPT S07-14 AT LINE LIN.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       MOVE "I"  TO W10-ETYPE
   WHEN F3-KEY
       MOVE "A"  TO W10-ETYPE
   WHEN F4-KEY
       MOVE "D"  TO W10-ETYPE
   WHEN F5-KEY
       MOVE "2"  TO W10-ETYPE
   WHEN F6-KEY
       MOVE "X"  TO W10-ETYPE
   WHEN F9-KEY
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Lookup using any word or part in the Stock Description.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       MOVE "W"  TO W10-ETYPE
   WHEN OTHER
                     CALL X"E5"
       GO TO BC330
               END-EVALUATE
              PERFORM AA50 THRU AA59
      *    *************************************************************
      *        *****   E X T E N D E D  C O D E   *****
      *    *************************************************************
        IF WS-EXT-STK = "Y"
     DISPLAY S07-EXT AT LINE LIN
        ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
        IF W10-STCK = "A"
     IF WS-USE-ITM = "Y"
         DISPLAY S07-ITM AT LINE LIN
     ELSE
               DISPLAY S07 AT LINE LIN.
              IF W10-EXT-ITEM = SPACES
     MOVE SAL-EXT-ITEM
     TO W10-EXT-ITEM
           GO TO BC330.

       BC335.
             PERFORM CA000.
      *    *************************************************************
      *        *****   E X T E N D E D  C O D E   *****
      *    *************************************************************
    IF WS-EXT-STK = "Y"
        DISPLAY S07-EXT AT LINE LIN
    ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
    IF W10-STCK = "A"
        IF WS-USE-ITM = "Y"
     DISPLAY S07-ITM AT LINE LIN
        ELSE
           DISPLAY S07 AT LINE LIN.
          IF WS-F-ERROR = 22
        MOVE "Z"   TO W10-ETYPE
              PERFORM AA50 THRU AA59
      *    *************************************************************
      *        *****   E X T E N D E D  C O D E   *****
      *    *************************************************************
        IF WS-EXT-STK = "Y"
     DISPLAY S07-EXT AT LINE LIN
        ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
        IF W10-STCK = "A"
     IF WS-USE-ITM = "Y"
         DISPLAY S07-ITM AT LINE LIN
     ELSE
               DISPLAY S07 AT LINE LIN
     END-IF
        END-IF
              IF W10-EXT-ITEM = SPACES
     MOVE SAL-EXT-ITEM
     TO W10-EXT-ITEM
           GO TO BC330
               ELSE
     GO TO BC335.
    IF STK-IND = 4
        MOVE "Header Record"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO BC330.
      *    *************************************************************
      *    **** C H E C K   F O R   S A L E S R E S T R I C T I O N S
      *    *************************************************************
    IF WS-RESTRICT = "A" OR "E"
        PERFORM MA000
        IF ((WS-RESTCODE = "Y") AND (WS-RESTRICT = "E")) OR
    ((WS-RESTCODE = "N") AND (WS-RESTRICT = "A"))
     MOVE "Restricted for this Department"
                                 TO WS-ERR-STRING
           PERFORM ERROR-MESSAGE
           GO TO BC330.
            MOVE STK-CODE  TO SAL-EXT-ITEM.
            PERFORM CLEAR-L50.
      *    *************************************************************
      *    ****    I S   I T   A   F R E E - F O R M A T   I T E M
      *    *************************************************************
       BC340.
           IF STK-IND = 3
               MOVE ZERO         TO STK-AVGE
              ACCEPT W10-SDSC AT LINE LIN COLUMN 21
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
            BACKGROUND-COLOR Black UPDATE AUTO
               DISPLAY "Index Unit cost" AT 2512
         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
               ACCEPT S26-AVGE AT LINE 25
           ELSE
               MOVE STK-DESC     TO W10-DESC
              DISPLAY W10-SDSC AT LINE LIN COLUMN 21
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Black.
           IF STK-IND = 3
               IF STK-AVGE = ZERO
           MOVE "Unit cost is zero"
     TO WS-ERR-STRING
           PERFORM ERROR-MESSAGE
           GO TO BC340.
            PERFORM CLEAR-L50.
      *    *************************************************************
      *    ****    L O G   D E T A I L S   T O   S A L E S   F I L E
      *    *************************************************************
            MOVE W10-EXT-ITEM  TO SAL-EXT-ITEM.
             MOVE W10-DESC       TO SAL-MSER.
             MOVE STK-AVGE       TO SAL-COST W10-COST.
      *    *************************************************************
      *    ****    A M E N D   Q U A N T I T Y
      *    *************************************************************
             ACCEPT S26-QNT AT LINE LIN.
           IF W10-QUANT = ZERO
              MOVE "Quantity MAY NOT be zero"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               MOVE SAL-QNT      TO W10-QUANT
               GO TO BC325.
      *      MOVE W10-QUANT  TO W10-QORD.
      *    *************************************************************
      *    ****    A M E N D   S E L L I N G   P R I C E
      *    *************************************************************
      PERFORM BC66A THRU BC75.
      *    *************************************************************
      *    ****    R E C A L C U L A T E   V A L U E S
      *    *************************************************************
             MULTIPLY W10-SELL BY W10-QUANT
                                 GIVING W25-VALUE.
             MOVE W25-VALUE      TO W100-EXT.
            DISPLAY (LIN, 70) W100-EXT
        WITH FOREGROUND-COLOR Cyan HIGHLIGHT
      BACKGROUND-COLOR Black.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO BC325.
      *    *************************************************************
      *    ****    U P D A T E  S A L E   F I L E   R E C O R D    ****
      *    *************************************************************
             MOVE W10-QUANT      TO SAL-QNT.
      *      MOVE W10-QORD  TO SAL-ORD.
             MOVE W10-SELL       TO SAL-SELL.
             MOVE W25-VALUE      TO SAL-VAL.
           IF WS-DISC = ZERO
              GO TO BC580.
             COMPUTE W90-CDISC = (W25-VALUE * WS-DISC) / 100.
             MOVE W90-CDISC      TO SAL-DISC.
      *    *************************************************************
      *    * R E - C A L C U L A T E   &   D I S P L A Y   T O T A L S *
      *    *************************************************************
       BC580.
            PERFORM BC95 THRU BC102.

       BC581.
             PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
             GO TO BC272.
      *    *************************************************************
      *    ****    C L E A R   D E T A I L   L I N E S      ****
      *    *************************************************************
       BC582.
      PERFORM BC295.
      *    *************************************************************
      *    ****    D I S P L A Y   L A S T   P A G E      ****
      *    *************************************************************
    IF WS-LAST > 10
        SUBTRACT 9 FROM WS-LAST
     GIVING WS-SALKEY
    ELSE
        MOVE 1   TO WS-SALKEY.

       BC583.
            PERFORM READ-SALE THRU READ-SALE-EXIT.
      MOVE SAL-EXT-ITEM  TO W10-EXT-ITEM.
      *    *************************************************************
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S     ****
      *    *************************************************************
          IF SAL-EXT-ITEM = SPACES
              DISPLAY (LIN, 21) SAL-SDSC
   WITH FOREGROUND-COLOR Grey HIGHLIGHT
        BACKGROUND-COLOR Black
        IF SAL-SUPER = 1
     DISPLAY (LIN, 5) "(I)" WITH FOREGROUND-COLOR Brown HIGHLIGHT
               ELSE
     DISPLAY (LIN, 5) "(R)"
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
        END-IF
              GO TO BC584.
      *    *************************************************************
      *    ****     E X T E N D E D   C O D E      ****
      *    *************************************************************
    IF WS-EXT-STK = "Y"
        DISPLAY S07-EXT AT LINE LIN
    ELSE
      *    *************************************************************
      *    *****       A L P H A - N U M E R I C   C O D E    *****
      *    *************************************************************
    IF W10-STCK = "A"
        IF WS-USE-ITM = "Y"
     DISPLAY S07-ITM AT LINE LIN
        ELSE
           DISPLAY S07 AT LINE LIN.
            DISPLAY ( , 21) SAL-SDSC
        WITH FOREGROUND-COLOR Cyan HIGHLIGHT
      BACKGROUND-COLOR Black.
            MOVE SAL-QNT  TO W10-QUANT.
             DISPLAY S26-QNT AT LINE LIN.
             MOVE SAL-SELL       TO W10-SELL.
             DISPLAY S26-PRC AT LINE LIN.
             MOVE SAL-VAL        TO W100-EXT.
            DISPLAY ( , 70) W100-EXT
        WITH FOREGROUND-COLOR Cyan HIGHLIGHT
      BACKGROUND-COLOR Black.
       BC584.
      ADD 1   TO LIN.
          IF WS-SALKEY < WS-LAST
        ADD 1   TO WS-SALKEY
              GO TO BC583.

       BC590.
            EXIT.
      /    *************************************************************
      *    ****  G E T  O V E R R I D E   F O R   Q U A N T I T Y  ****
      *    *************************************************************
       BC600      SECTION 50.
       BC601.
      /    *************************************************************
      *    ****    A M E N D   O R D E R
      *    *************************************************************
       BD000      SECTION 51.
       BD00.
      *    *************************************************************
      *    ****    S U P E R V I S O R   P A S S W O R D S
      *    *************************************************************
             MOVE 5              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-PW         TO W85-PASS.
       BD05.
      *    *************************************************************
      *    ****    S A L E S   O R D E R   S C R E E N
      *    *************************************************************
             PERFORM ERASE-SCREEN.
             DISPLAY S30.
      *    *************************************************************
      *            C H A N G E   S C R E E N   H E A D I N G
      *    *************************************************************
             DISPLAY S31.
             MOVE 11              TO LIN.
             PERFORM BD115 UNTIL LIN > 41.
             MOVE W12-TODAY      TO W90-DTE.
            DISPLAY W90-DTE AT 0570
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             MOVE ZERO           TO W90-BDT W90-NONTAX W90-LDISC
              W90-BAL W90-SALES
              W90-CUR W90-EXCTOT.
            MOVE 1   TO WS-FIRST WS-SALPOS WS-LAST.
             MOVE ZERO           TO W10-SORD.
             MOVE "N"            TO WS-IN-OPT.
      MOVE 1   TO WS-S9.

       BD08.
            MOVE ZERO   TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
    IF WS-S9 < 12
        ADD 1   TO WS-S9
        GO TO BD08.

       BD10.
             PERFORM AA400.
             ACCEPT S30A AT 0510.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE ZERO  TO W10-SORD
       GO TO BD268
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       MOVE "O"  TO WS-ETYPE
   WHEN F3-KEY
       MOVE "A"  TO WS-ETYPE
   WHEN F4-KEY
       MOVE "T"  TO WS-ETYPE
   WHEN OTHER
       CALL X"E5"
       GO TO BD10
        END-EVALUATE
              PERFORM AA50O THRU AA59
               DISPLAY S30A AT 0510
               IF W10-SORD = ZERO
                   GO TO BD10.
            PERFORM CLEAR-L50.
           IF W10-SORD = ZERO
              GO TO BD268.
             MOVE W10-SORD       TO W90-REF.

       BD12.
             PERFORM GET-SORDER.
           IF WS-ERROR = 1
              MOVE WS-ERR2  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BD10.
           IF WS-ERROR = 2
              MOVE "Not an order"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BD10.
           IF WS-ERROR = 3
              MOVE "Order corrupted"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BD10.
           IF WS-ERROR = 4
              MOVE "Invalid a/c no"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BD10.
             INITIALIZE W14-DELETE.
           IF W90-EXP = "E" OR "X"
               MOVE W05-VATNO    TO W90-TAX
           ELSE
               MOVE SPACES       TO W90-TAX.
             DISPLAY S36 AT 0308
            DISPLAY W40-NAME AT 0320
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            DISPLAY W90-ORD AT 0368
       WITH FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            DISPLAY W10-SMAN AT 0411
       WITH FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta.
           IF W10-SMAN NOT = ZERO
               ADD 100 W10-SMAN    GIVING WS-PARKEY
               PERFORM READ-PARAM THRU READ-PARAM-EXIT
              DISPLAY PAR-SALESMAN AT 0420
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta.
      DISPLAY W40-TEL AT 0466
       WITH FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      DISPLAY W40-CONTACT AT 0528
       WITH FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             MOVE 1              TO WS-FIRST WS-SALKEY.
             SUBTRACT 1          FROM WS-LAST.
      MOVE SPACES  TO WS-RESTRICT.
      MOVE "N"   TO WS-RESTCODE.
      *    *************************************************************
      *    ****   D E B T O R   S A L E S   R E S T R I C T I O N S
      *    *************************************************************
    IF DEB-RESTRICT = "A" OR "E"
        MOVE DEB-ACNO  TO DBD-ACNO
        PERFORM READ-DEBDEP THRU READ-DEBDEP-EXIT
        IF WS-F-ERROR = 0
     MOVE DEB-RESTRICT
     TO WS-RESTRICT.
            PERFORM BD100 THRU BD102.

       BD15.
            PERFORM BD270A.
            PERFORM CLEAR-L50.

       BD20.
             ADD 1               TO WS-LAST.
             GO TO BD112.

       BD50.
             MOVE 11              TO LIN.

       BD55.
            MOVE SPACES  TO W10-DESC W10-EXT-ITEM.
             MOVE ZERO           TO W10-SELL.
             MOVE 1              TO W10-QUANT.
       BD60.
             PERFORM AA300.
      *    *************************************************************
      *    ****    G E T   S T O C K   C O D E
      *    *************************************************************
    IF WS-EXT-STK = "Y"
        ACCEPT S07-EXT AT LINE LIN
    ELSE
      *    *************************************************************
      *   ***** ALPHA-NUMERIC CODE *****
      *    *************************************************************
    IF W10-STCK = "A"
        IF WS-USE-ITM = "Y"
     ACCEPT S07-ITM AT LINE LIN
        ELSE
           ACCEPT S07 AT LINE LIN
        END-IF
    ELSE
      *    *************************************************************
      *   ***** NUMERIC CODES APPLY *****
      *    *************************************************************
    IF W10-SLNGTH = 3
        ACCEPT S07-3 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 4
        ACCEPT S07-4 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 5
        ACCEPT S07-5 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 6
        ACCEPT S07-6 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 7
        ACCEPT S07-7 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 8
        ACCEPT S07-8 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 9
        ACCEPT S07-9 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 10
        ACCEPT S07-10 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 11
        ACCEPT S07-11 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 12
        ACCEPT S07-12 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 13
        ACCEPT S07-13 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 14
        ACCEPT S07-14 AT LINE LIN.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W10-ITEM
       GO TO BD60A
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       MOVE "I"  TO W10-ETYPE
   WHEN F3-KEY
       MOVE "A"  TO W10-ETYPE
   WHEN F4-KEY
       MOVE "D"  TO W10-ETYPE
   WHEN F5-KEY
       MOVE "2"  TO W10-ETYPE
   WHEN F6-KEY
       MOVE "X"  TO W10-ETYPE
   WHEN F9-KEY
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Lookup using any word or part in the Stock Description.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       MOVE "W"  TO W10-ETYPE
   WHEN OTHER
       CALL X"E5"
       GO TO BD60
        END-EVALUATE
              PERFORM AA50 THRU AA59
      *    *************************************************************
      *       *****   E X T E N D E D   C O D E  *****
      *    *************************************************************
        IF WS-EXT-STK = "Y"
     DISPLAY S07-EXT AT LINE LIN
        ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
        IF W10-STCK = "A"
     IF WS-USE-ITM = "Y"
         DISPLAY S07-ITM AT LINE LIN
     ELSE
               DISPLAY S07 AT LINE LIN.
              IF W10-EXT-ITEM = SPACES
                   GO TO BD60.

       BD60A.
          IF W10-EXT-ITEM = SPACES
               SUBTRACT 1        FROM WS-LAST
               GO TO BD120.
       BD61.
             PERFORM CA000.
           IF WS-F-ERROR = 22
        MOVE "Z"   TO W10-ETYPE
              PERFORM AA50 THRU AA59
      *    *************************************************************
      *        *****   E X T E N D E D  C O D E   *****
      *    *************************************************************
        IF WS-EXT-STK = "Y"
     DISPLAY S07-EXT AT LINE LIN
        ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
        IF W10-STCK = "A"
     IF WS-USE-ITM = "Y"
         DISPLAY S07-ITM AT LINE LIN
     ELSE
               DISPLAY S07 AT LINE LIN
     END-IF
        END-IF
              IF W10-EXT-ITEM = SPACES
                   GO TO BD60
               ELSE
                   GO TO BD61.
    IF STK-IND = 4
        MOVE "Header Record"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO BD60.
      *    *************************************************************
      *    **** C H E C K   F O R   S A L E S   R E S T R I C T I O N S
      *    *************************************************************
    IF WS-RESTRICT = "A" OR "E"
        PERFORM MA000
        IF ((WS-RESTCODE = "Y") AND (WS-RESTRICT = "E")) OR
    ((WS-RESTCODE = "N") AND (WS-RESTRICT = "A"))
     MOVE "Restricted for this Department"
                                 TO WS-ERR-STRING
           PERFORM ERROR-MESSAGE
           GO TO BD60.
            MOVE WS-LAST  TO WS-SALKEY.
             INITIALIZE SAL-RECORD1.
             MOVE WS-LAST        TO SAL-CODE.
            MOVE STK-CODE  TO SAL-EXT-ITEM.
            PERFORM CLEAR-L50.
      *    *************************************************************
      *    ****    I S   I T   A   F R E E - F O R M A T   I T E M
      *    *************************************************************
       BD65.
           IF STK-IND = 3
               MOVE ZERO         TO STK-AVGE
               MOVE SPACES       TO W10-DESC
              ACCEPT W10-SDSC AT LINE LIN COLUMN 21
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
            BACKGROUND-COLOR Black UPDATE AUTO
               DISPLAY "Index Unit cost" AT 2512
         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
               ACCEPT S26-AVGE AT LINE 25
           ELSE
               MOVE STK-DESC     TO W10-DESC
              DISPLAY W10-SDSC AT LINE LIN COLUMN 21
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Black.
           IF STK-IND = 3
               IF STK-AVGE = ZERO
           MOVE "Unit cost is zero"
     TO WS-ERR-STRING
           PERFORM ERROR-MESSAGE
           GO TO BD65.
      *    *************************************************************
      *    ****    L O G   D E T A I L S   T O   S A L E S   F I L E
      *    *************************************************************
            MOVE W10-EXT-ITEM  TO SAL-EXT-ITEM.
             MOVE W10-DESC       TO SAL-MSER.
             MOVE STK-AVGE       TO SAL-COST W10-COST.
      *    *************************************************************
      *    ****    G E T   Q U A N T I T Y
      *    *************************************************************
      MOVE ZERO   TO W10-LENGTH W10-BREADTH W10-NUMBER.
      MOVE "N"   TO WS-MEASURE.
      MOVE 1   TO WS-P1.
    IF (LS0-LEV < 3) OR
       (STK-USE-PACKS = "N")
        GO TO BD66.
      DISPLAY "µ" AT 0750
        STK-PER WITH BACKGROUND-COLOR Magenta
       FOREGROUND-COLOR Brown HIGHLIGHT
       "Æ".
      GO TO BD66.

       BD65-PACK.
    IF NOT (STK-UNT(WS-P1) = ZERO)
        DISPLAY STK-PR(WS-P1) AT 0751
         WITH BACKGROUND-COLOR Magenta
       FOREGROUND-COLOR Brown HIGHLIGHT
    ELSE
    IF WS-P1 = 1
        DISPLAY "  Each  "  AT 0751
   WITH BACKGROUND-COLOR Magenta
        FOREGROUND-COLOR Brown HIGHLIGHT
    ELSE
        SUBTRACT 1  FROM WS-P1
        GO TO BD65-PACK.

       BD66.
      DISPLAY "F6" AT 2502
        WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Brown HIGHLIGHT
       " to key in "
        WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Blue
       "- Cutting size (Wood, Glass etc) "
        WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Grey HIGHLIGHT
       "or "
        WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Blue
       "Esc" WITH BACKGROUND-COLOR Cyan
    FOREGROUND-COLOR Brown HIGHLIGHT
       "ape" WITH BACKGROUND-COLOR Cyan
    FOREGROUND-COLOR Blue.
    IF LS0-LEV > 2
        IF STK-USE-PACKS = "Y"
     DISPLAY "" AT 2560
       WITH BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Brown HIGHLIGHT
      " for Packs"
       WITH BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Blue.
            ACCEPT S26-QNT AT LINE LIN.
    IF ADIS-FUNC
        IF LS0-LEV < 3
     GO TO BD66
        END-IF
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
       IF WS-P1 > 1
    SUBTRACT 1  FROM WS-P1
    GO TO BD65-PACK
   WHEN DOWN-KEY
       IF WS-P1 < 4
    ADD 1      TO WS-P1
    GO TO BD65-PACK
        END-EVALUATE
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
             DISPLAY (LIN, 2) WS-INV78 WITH FOREGROUND-COLOR Cyan
             GO TO BD55
   WHEN F6-KEY
       PERFORM BB050
             DISPLAY S26-QNT AT LINE LIN
   END-EVALUATE.
          IF W10-QUANT = ZERO
              DISPLAY (LIN, 2) WS-INV78 WITH FOREGROUND-COLOR Cyan
              GO TO BD55.
      *    *************************************************************
      *    ****    G E T   S E L L I N G   P R I C E   (ACCOUNT OR CASH)
      *    *************************************************************
       BD66A.
    IF WS-P1 > 1
        MULTIPLY STK-UNT(WS-P1) BY STK-AVGE
     GIVING SAL-COST W10-COST
    ELSE
              MOVE STK-AVGE  TO SAL-COST W10-COST.
      MOVE W90-TAX  TO W10-ENTRY.
      *    *************************************************************
      *    ****    C H A N G E  F O R  M U L T I P L E   R A T E S
      *    *************************************************************
      MOVE STK-TAX  TO WS-VAT-SUB.
          IF STK-IND < 3
               IF W90-TAX NOT = SPACES
      *    *************************************************************
      *    ****    E X C L U S I V E   P R I C E S
      *    *************************************************************
           IF WS-PRC = "1"
               MOVE STK-SL(WS-P1)
                                 TO W10-SELL
           ELSE
           IF WS-PRC = "2"
               MOVE STK-CSH(WS-P1)
                                 TO W10-SELL
           ELSE
               MOVE STK-WSL(WS-P1)
                                 TO W10-SELL
              ELSE
      *    *************************************************************
      *    ****    I N C L U S I V E   P R I C E S
      *    *************************************************************
           IF WS-PRC = "1"
               MOVE W10-VSL(WS-P1)
                                 TO W10-SELL
                   ELSE
           IF WS-PRC = "2"
               MOVE W10-VCSH(WS-P1)
                                 TO W10-SELL
                   ELSE
               MOVE W10-VWSL(WS-P1)
                                 TO W10-SELL.
      MOVE W10-ENTRY  TO W90-TAX.

       BD68.
             MOVE W10-SELL       TO W10-SPAPRC.

       BD70.
             ACCEPT S26-PRC AT LINE LIN.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BD66
        END-EVALUATE.
    IF (W10-SELL < W10-SPAPRC) AND
       ((W10-SPAPRC - W10-SELL) < 0.01)
        MOVE W10-SELL  TO W10-SPAPRC.
    IF W10-SELL < W10-SPAPRC
              PERFORM CLEAR-L50
               COMPUTE WS-DISC ROUNDED = ((W10-SELL - W10-SPAPRC) /
                                           W10-SELL) * 100.00000
              DISPLAY "Discount %" AT 2518
   WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
               DISPLAY S26-DSC AT LINE 25
        DISPLAY "R" AT 2541
   WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
         "ekey or "
   WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
         "ENTER"
   WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
         " to continue"
   WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
        ACCEPT WS-OPTION AT 2568
        WITH BACKGROUND-COLOR Magenta
      FOREGROUND-COLOR Grey HIGHLIGHT AUTO
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
              PERFORM CLEAR-L50
               IF WS-OPTION = "R"
                   GO TO BD70.
    IF W10-SELL < W10-SPAPRC
              MOVE W10-SELL  TO W10-SPAPRC
              PERFORM BB100
              IF WS-ERROR = 1
           MOVE ZERO  TO WS-ERROR
           GO TO BD66A.

       BD75.
             MOVE W10-SELL           TO SAL-SELL.
      *    *************************************************************
      *    ****    C A L C U L A T E   E X T E N D E D   V A L U E
      *    *************************************************************
       BD80.
             MOVE W10-QUANT          TO SAL-QNT.
             MOVE "N"                TO SAL-INSTRUCT.
             MOVE WS-P1              TO SAL-PACK.
             COMPUTE W25-VALUE ROUNDED = (1.000 * W10-SELL * W10-QUANT).
             MOVE W25-VALUE      TO W100-EXT SAL-VAL.
             DISPLAY (LIN, 70) W100-EXT WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Black.
      *    *************************************************************
      *    ****    L I N E   D I S C O U N T   ( PERCENTAGE )
      *    *************************************************************
             PERFORM CLEAR-L50.
             DISPLAY "Discount %" AT 2518 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
             MOVE ZERO               TO WS-DISC W90-CDISC.
      *    *************************************************************
      *    ****   I F D I S C O U N T    A P P L I C A B L E
      *    G E T   P E R C E N T A G E F R O M
      *    D E P A R T M E N T F I L E
      *    *************************************************************
          IF (DEB-DISC > 0 AND < 11)
              MOVE STK-LDG  TO DPT-CODE
               PERFORM READ-DEPART THRU READ-DEPART-EXIT
              MOVE DPT-DSETT(DEB-DISC)
           TO WS-DISC
      *    *************************************************************
      *    **  M U S T  T H E  A L T E R N A T E   D I S C O U N T  **
      *    **  B E   U S E D   F O R   T H I S D E B T O R       **
      *    *************************************************************
        IF STK-ALT-DISC = "Y"
     IF DPT-ALT-DD1 = DEB-DISC
         MOVE DPT-DD11
     TO DPT-DSETT(DEB-DISC)
        WS-DISC
     ELSE
     IF DPT-ALT-DD2 = DEB-DISC
         MOVE DPT-DD12
     TO DPT-DSETT(DEB-DISC)
        WS-DISC
     END-IF
        END-IF.

       BD85.
             ACCEPT S26-DSC AT LINE 25.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
             PERFORM CLEAR-L50
       GO TO BD70
        END-EVALUATE.
          IF WS-DISC = ZERO
              PERFORM CLEAR-L50
               GO TO BD90.
            COMPUTE W10-SPAPRC ROUNDED = W10-SELL -
            (W10-SELL * WS-DISC / 100.0000).
      *    *************************************************************
      *    ****    C H E C K   M A R G I N
      *    *************************************************************
          IF (DEB-DISC > 0 AND < 11)
              IF DPT-DSETT(DEB-DISC) = WS-DISC
                   GO TO BD86.
             PERFORM BB100.
           IF WS-ERROR = 1
               GO TO BD85.

       BD86.
            PERFORM CLEAR-L50.
             MOVE WS-DISC        TO SAL-DSC.
            COMPUTE W90-CDISC ROUNDED =
     ((W25-VALUE * WS-DISC) / 100.00000).
             MOVE W90-CDISC      TO W100-LBAL SAL-DISC.
            DISPLAY W100-LBAL AT 2540
       WITH BACKGROUND-COLOR Magenta
     FOREGROUND-COLOR Cyan HIGHLIGHT.
      *    *************************************************************
      *    ****    A R E   T H E   D E T A I L S   C O R R E C T
      *    *************************************************************
       BD90.
      PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO BD60.
      *    *************************************************************
      *    ****    A C C U M U L A T E   TOTAL COST / TOTAL DISCOUNT /
      *            VALUES SUBJECT TO TAX AND NON-TAX.
      *    *************************************************************
       BD95.
          IF (STK-IND < 2) OR (STK-IND = 3)
               COMPUTE W90-BAL = W90-BAL + (W10-QUANT * W10-COST).
             ADD W90-CDISC       TO W90-LDISC.
             SUBTRACT W90-CDISC FROM W25-VALUE
                                 GIVING W90-VALUE.
          IF (STK-TAX = 0) OR (W90-EXP = "X" OR "C")
               ADD W90-VALUE     TO W90-NONTAX
               MOVE W90-VALUE    TO SAL-NONTAX
        MOVE ZERO  TO SAL-VAT-SUB
           ELSE
               MOVE W90-VALUE    TO SAL-TAXVAL
              ADD W90-VALUE  TO W90-TAXVAL(WS-VAT-SUB)
        MOVE WS-VAT-SUB  TO SAL-VAT-SUB.
          IF (W90-TAX NOT = SPACES) AND
       (W90-EXP NOT = "C" AND "X")
        COMPUTE SAL-VAT ROUNDED = SAL-TAXVAL * W05-RTE.
    IF (W90-EXP = "I") AND (STK-TAX NOT = 0)
        COMPUTE SAL-VAT ROUNDED = (SAL-TAXVAL /
        (100.0000 + W05-RATE)) * W05-RATE.
    IF SAL-VAT-SUB > 0
        ADD SAL-VAT  TO W90-VAT(SAL-VAT-SUB).
      *    *************************************************************
      *    ****    A C C U M U L A T E   T O T A L   S A L E
      *    *************************************************************
             ADD W25-VALUE       TO W90-DB.

       BD100.
             MOVE W90-DB         TO W90-TOTAL.
             SUBTRACT W90-LDISC  FROM W90-TOTAL.
             MOVE W90-DB         TO W100-BAL.
            DISPLAY W100-BAL AT 2015
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             MULTIPLY -1 BY W90-LDISC
                                 GIVING W100-BAL.
            DISPLAY W100-BAL AT 2115
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      *    *************************************************************
      *    ****    C A L C U L A T E   T A X   ( RUNNING TOTAL )
      *    *************************************************************
          IF (W90-TAX = SPACES) OR
       (W90-EXP = "C" OR "X")
               GO TO BD102.
      MOVE 1   TO WS-S1.
      MOVE ZERO   TO W25-TOTAL.

       BD101.
          IF W90-TAXVAL(WS-S1) NOT = ZERO
              MOVE W05-VAT(WS-S1)
           TO W05-RATE
              COMPUTE W25-TOTAL ROUNDED = W25-TOTAL +
        (W90-TAXVAL(WS-S1) * W05-RTE).
    IF WS-S1 < 6
        ADD 1   TO WS-S1
        GO TO BD101.
            MOVE W25-TOTAL  TO W100-BAL.
            ADD W25-TOTAL  TO W90-TOTAL.
            DISPLAY W100-BAL AT 2215
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.

       BD102.
            MOVE W90-TOTAL  TO W100-BAL.
            DISPLAY W100-BAL AT 2315
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
       BD105.
            MOVE "'A'ccept or 'C'ancel   [A]"
     TO WS-ERR-MES.
            MOVE "A"   TO WS-OPTION.
      *    *************************************************************
      *    ****    M U S T   T H E   L A S T   E N T R Y   B E
      *            A C C E P T E D.
      *    *************************************************************
       BD110.
      PERFORM OPT-MESSAGE TEST AFTER
       UNTIL WS-OPTION = "A" OR "C".
          IF WS-OPTION = "C"
              DISPLAY (LIN, 2) WS-INV78
   WITH FOREGROUND-COLOR Cyan BACKGROUND-COLOR Blue
               PERFORM BD114
               PERFORM BD100 THRU BD102
               GO TO BD55.

       BD111.
             MOVE WS-SUPER       TO SAL-SUPER.
             PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
             ADD 1               TO WS-LAST.
      *    *************************************************************
      *    ****    T E S T   F O R   C U T T I N G   S P E C S
      *    *************************************************************
    IF W10-LENGTH > ZERO AND
       W10-BREADTH > ZERO AND
       W10-NUMBER > ZERO
              IF LIN < 41
           ADD 1  TO LIN
        ELSE
           MOVE 11  TO WS-S7
           PERFORM BD111-SHIFT
        END-IF
              MOVE WS-LAST  TO WS-SALKEY
              INITIALIZE SAL-RECORD1
              MOVE WS-LAST  TO SAL-CODE
        MOVE SPACES  TO W10-REMKS
        MOVE W10-LENGTH  TO W100-LENGTH
        MOVE W10-BREADTH  TO W100-BREADTH
        MOVE W10-NUMBER  TO W100-NUMBER
        STRING W100-LENGTH DELIMITED SIZE
        " by " DELIMITED SIZE
        W100-BREADTH DELIMITED SIZE
        " X " DELIMITED SIZE
        W100-NUMBER DELIMITED SIZE
        INTO W10-REMKS
              MOVE W10-REMKS  TO SAL-MSER
        MOVE WS-SUPER  TO SAL-SUPER
              PERFORM WRITE-SALE THRU WRITE-SALE-EXIT
              DISPLAY (LIN, 21) W10-REMKS
   WITH FOREGROUND-COLOR Grey HIGHLIGHT
        BACKGROUND-COLOR Black UPDATE AUTO
              ADD 1   TO WS-LAST.
      *    *************************************************************
      *    ****    T E S T   I F   S C R E E N   F U L L
      *    *************************************************************
           IF LIN < 41
               ADD 1             TO LIN
               GO TO BD112.
      PERFORM SAVE-SCREEN-2.
      MOVE 11   TO WS-S7.
      *    *************************************************************
      *    ****    S C R O L L  D E T A I L   L I N E S   U P
      *    *************************************************************
       BD111-SHIFT.
      ADD 1 WS-S7  GIVING WS-S8.
      MOVE W42-LINES (WS-S8)
     TO W42-LINES (WS-S7).
      MOVE W43-LINE (WS-S8)
     TO W43-LINE (WS-S7).
    IF WS-S8 < 41
        ADD 1   TO WS-S7
        GO TO BD111-SHIFT.
      PERFORM RESTORE-SCREEN-2.
      *    *************************************************************
      *    ****    C L E A R   L A S T  D E T A I L   L I N E
      *    *************************************************************
            DISPLAY (LIN, 1) WS-INV1 WITH BACKGROUND-COLOR Black
        FOREGROUND-COLOR Cyan
         WS-INV78 WITH FOREGROUND-COLOR Cyan
         WS-INV80 WITH BACKGROUND-COLOR Black
         FOREGROUND-COLOR Cyan.
      ADD 1   TO WS-SALPOS.

       BD112.
      MOVE SPACES     TO WS-ERR-MES.
            STRING "'I'nternal notes,'R'emarks,'N'ext or 'E'nd  ["
      DELIMITED SIZE
      WS-IN-OPT DELIMITED SIZE
      "]" DELIMITED SIZE
     INTO WS-ERR-MES.
            MOVE WS-IN-OPT  TO WS-OPTION.
      PERFORM OPT-MESSAGE TEST AFTER
       UNTIL WS-OPTION = "I" OR "R" OR "N" OR "E" OR "C".
      *    *************************************************************
      *    ****  "C"   M A Y   B E   U S E D   T O   C H A N G E
      *    C L I E N T D E T A I L S
      *    *************************************************************
    IF WS-OPTION = "C"
        PERFORM BD591A
        GO TO BD112.
      MOVE WS-OPTION  TO WS-IN-OPT.
           IF WS-OPTION = "N"
               GO TO BD55.
           IF WS-OPTION = "E"
               SUBTRACT 1        FROM WS-LAST
               GO TO BD120.
             MOVE SPACES         TO W10-REMKS.
            ACCEPT W10-REMKS AT LINE LIN COLUMN 21
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
          BACKGROUND-COLOR Black UPDATE AUTO.
           IF W10-REMKS = SPACES
               GO TO BD112.
            MOVE WS-LAST  TO WS-SALKEY.
             INITIALIZE SAL-RECORD1.
             MOVE WS-LAST        TO SAL-CODE.
             MOVE W10-REMKS      TO SAL-MSER.
           IF WS-OPTION = "I"
        DISPLAY "(I)" AT LINE LIN COLUMN 5
   WITH FOREGROUND-COLOR Brown HIGHLIGHT
               MOVE 1            TO SAL-SUPER
           ELSE
        DISPLAY "(R)" AT LINE LIN COLUMN 5
   WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             GO TO BD111.

       BD114.
             SUBTRACT W25-VALUE  FROM W90-DB.
             SUBTRACT W90-CDISC  FROM W90-LDISC.
             SUBTRACT SAL-NONTAX FROM W90-NONTAX.
    IF SAL-VAT-SUB > ZERO
              SUBTRACT SAL-TAXVAL FROM W90-TAXVAL(SAL-VAT-SUB)
        SUBTRACT SAL-VAT  FROM W90-VAT(SAL-VAT-SUB).
          IF (STK-IND < 2) OR (STK-IND = 3)
               COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).
      *    *************************************************************
      *    ****   C L E A R   S C R E E N   L I N E / S
      *    *************************************************************
       BD115.
            DISPLAY (LIN, 1) WS-INV1 WITH BACKGROUND-COLOR Black
        FOREGROUND-COLOR Cyan
         WS-INV78 WITH FOREGROUND-COLOR Cyan
         WS-INV80 WITH BACKGROUND-COLOR Black
         FOREGROUND-COLOR Cyan.
            ADD 1   TO LIN.
      *    *************************************************************
      *    ****    W A S   T H E R E   A N   E N T R Y
      *    *************************************************************
       BD120.
          IF WS-LAST = ZERO
               GO TO BD05.
      *    *************************************************************
      *    ****    P R O C E S S   T H E   O R D E R
      *    *************************************************************
       BD125.
            MOVE "'U'pdate,'P'rint,'A'mend,'C'ancel,'M'ore items"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.

       BD130.
      PERFORM OPT-MESSAGE
       UNTIL WS-OPTION = "U" OR "A" OR "C" OR "M" OR "P".
          IF WS-OPTION = "M"
               ADD 1             TO WS-LAST
               GO TO BD112.
           IF WS-OPTION = "A"
              PERFORM BD270A
               GO TO BD125.
           IF WS-OPTION = "C"
              GO TO BD268.
             MOVE ZERO           TO W90-STOTAL WS-TDISC W90-TDISC.
           IF W90-LDISC NOT = ZERO
               MOVE W90-LDISC    TO W90-TDISC
               GO TO BD140.
             MOVE W90-DB         TO W90-DVAL.

       BD140.
             SUBTRACT W90-TDISC FROM W90-DB
                                 GIVING W90-TSELL W90-TOTAL.
             MULTIPLY -1 BY W90-TDISC
                                 GIVING W100-BAL.
      *    *************************************************************
      *    ****         D  I  S  C  O  U  N  T
      *    *************************************************************
            DISPLAY W100-BAL AT 2115
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            MOVE ZERO   TO W90-TTAX
        W90-EXCTOT.
      MOVE 1   TO WS-P1.
      *    *************************************************************
      *    ****    V A T   ?
      *    *************************************************************
          IF (W90-TAX = SPACES) OR (W90-EXP = "C" OR "X")
              GO TO BD143.
      *    *************************************************************
      *    ****    C A L C U L A T E   T O T A L   V A T
      *    *************************************************************
       BD141.
          IF (W90-TAXVAL(WS-P1) = ZERO)
              GO TO BD142.
            MOVE W05-VAT(WS-P1) TO W05-RATE.
            COMPUTE W90-TTAX ROUNDED =
      (W90-TAXVAL(WS-P1) * W05-RTE).
      ADD W90-TAXVAL(WS-P1)
     TO W90-EXCTOT.
      *    *************************************************************
      *    ****    C H E C K   I F   A L L   V A T   R A T E S
      *     H A V E   B E E N   C A L C U L A T E D
      *    *************************************************************
       BD142.
    IF WS-P1 < 6
        ADD 1   TO WS-P1
        GO TO BD141.
            MOVE W90-TTAX  TO W100-BAL.
      *    *************************************************************
      *    ****         V  A  T
      *    *************************************************************
            DISPLAY W100-BAL AT 2215
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             ADD W90-TTAX        TO W90-TOTAL.

       BD143.
            MOVE W90-TOTAL  TO W100-BAL.
      *    *************************************************************
      *    ****         T  O  T  A  L
      *    *************************************************************
            DISPLAY W100-BAL AT 2315
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            PERFORM CLEAR-L50.
      *    *************************************************************
      *    ****    U P D A T E  O R D E R   D E T A I L S
      *    *************************************************************
       BD145.
            DISPLAY "Updating Order" AT 2520 WITH
             BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
             MOVE ZERO           TO WS-SALKEY.
             PERFORM AY60 THRU AY999.
      *    *************************************************************
      *    ****    U P D A T E   O R D E R   F I L E
      *    *************************************************************
       BD190.
            MOVE W90-REF  TO SOR-REF.
           IF W14-SUB = ZERO
               GO TO BD205.
      *    *************************************************************
      *    ****    D E L E T E   R E D U N D A N T   R E C O R D S
      *    *************************************************************
            MOVE 1   TO WS-S1.

       BD195.
    IF W14-SQN (WS-S1) NOT = ZERO
               MOVE W14-SQN (WS-S1)
                                 TO SOR-SQN
              PERFORM READ-SORDER THRU READ-SORDER-EXIT
               MOVE 2            TO WS-ACTION
               PERFORM AY17 THRU AY59
              PERFORM DELETE-SORDER-REC THRU WRITE-SORDER-EXIT
        IF SOR-TYPE = 1
                   PERFORM BD196 THRU BD199.
          IF WS-S1 < W14-SUB
               ADD 1             TO WS-S1
               GO TO BD195.
             GO TO BD205.

       BD196.
      MOVE SOR-EXT-ITEM  TO PRT-EXT-ITEM.

       BD197.
             PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
           IF WS-F-ERROR = 21
               GO TO BD199.
      *    *************************************************************
      *    ****  P R O D U C T I O N   D E T A I L S
      *    *************************************************************
       BD198.
               PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT
           IF SOR-TYPE NOT = 2
               GO TO BD199.
             MOVE 2              TO WS-ACTION.
             PERFORM AY17 THRU AY59.
             PERFORM DELETE-SORDER-REC THRU WRITE-SORDER-EXIT.
             GO TO BD198.
       BD199.
             EXIT.

       BD200.
             PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT.
      *    *************************************************************
      *    ****    R E A D   T H E   S A L E   F I L E   A N D
      *            L O G   T O   O R D E R   F I L E
      *    *************************************************************
       BD205.
            ADD 1   TO WS-SALKEY.
           IF WS-SALKEY > WS-LAST
               GO TO BD255.
             PERFORM READ-SALE THRU READ-SALE-EXIT.
           IF SAL-SQN = ZERO
               SUBTRACT 1        FROM WS-SALKEY
               GO TO BD235.
             MOVE SAL-SQN        TO SOR-SQN.
             PERFORM READ-SORDER THRU READ-SORDER-EXIT.
             MOVE 0              TO WS-ACTION.
             PERFORM AY17 THRU AY59.
           IF SOR-TYPE NOT = 1
               GO TO BD210.
      *    *************************************************************
      *    ****    U P D A T E  P I C K I N G  S L I P   Q T Y
      *     O N  S T O C K   R E C O R D
      *    *************************************************************
            MOVE SAL-EXT-ITEM  TO STK-CODE.
      MOVE 0   TO WS-ACTION.
      PERFORM AY05 THRU AY59.
      SUBTRACT SOR-QNT  FROM STK-QPIC.
      ADD SOR-QNT  TO STK-QUANT.
      MOVE SAL-EXT-ITEM  TO SOR-EXT-ITEM.
      MOVE SAL-MSER  TO SOR-DESC.
            MOVE SAL-QNT  TO SOR-QNT.
      *        SOR-QORD.
             MOVE SAL-COST       TO SOR-COST.
             MOVE SAL-VAT        TO SOR-VAT.
            MOVE SAL-VAT-SUB  TO SOR-VAT-SUB.
             MOVE SAL-SELL       TO SOR-SELL.
             MOVE SAL-VAL        TO SOR-VAL.
             MOVE SAL-DISC       TO SOR-DISC.
             MOVE SAL-DSC        TO SOR-DSC.
             MOVE SAL-INCTAX     TO SOR-INCTAX.
             MOVE SAL-NONTAX     TO SOR-NONTAX.
             MOVE SAL-TAXVAL     TO SOR-TAXVAL.
      SUBTRACT SOR-QNT  FROM STK-QUANT.
      ADD SOR-QNT  TO STK-QPIC.
            PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
            PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT.
             MOVE SPACE          TO WS-OPTION.
      *    *************************************************************
      *    ****  P R O D U C T I O N   D E T A I L S
      *    *************************************************************
       BD206.
             PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
           IF SOR-TYPE NOT = 2
               GO TO BD206A.
             MOVE "P"            TO WS-OPTION.
             MOVE 2              TO WS-ACTION.
             PERFORM AY17 THRU AY59.
      MOVE SOR-EXT-ITEM  TO STK-CODE.
      MOVE 0   TO WS-ACTION.
      PERFORM AY05 THRU AY59.
      SUBTRACT SOR-QNT  FROM STK-QPIC.
      ADD SOR-QNT  TO STK-QUANT.
            PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
             PERFORM DELETE-SORDER-REC THRU WRITE-SORDER-EXIT.
             GO TO BD206.

       BD206A.
             MOVE SAL-SQN        TO SOR-SQN.
             PERFORM READ-SORDER THRU READ-SORDER-EXIT.
           IF WS-OPTION NOT = "P"
               GO TO BD205.
             PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
           IF WS-F-ERROR = 21
               GO TO BD209.
      *    *************************************************************
      *    ****  P R O D U C T I O N   D E T A I L S
      *    *************************************************************
             MOVE 1              TO WS-S6.

       BD207.
           IF PRT-CODE (WS-S6) = SPACES
               ADD 1             TO WS-S6
               GO TO BD208.
            MOVE PRT-EXT-CODE(WS-S6)
     TO W10-EXT-ITEM.
             PERFORM CA000.
           IF WS-F-ERROR = 22
               ADD 1             TO WS-S6
               GO TO BD208.
             MOVE 2              TO SOR-TYPE.
             INITIALIZE SOR-DET2.
      MOVE STK-CODE  TO SOR-EXT-ITEM.
             MOVE STK-DESC       TO SOR-DESC.
      MULTIPLY SAL-QNT BY PRT-QUANT(WS-S6)
                                 GIVING SOR-QNT.
             ADD 1               TO SOR-SEQ.
      MOVE 0   TO WS-ACTION.
      PERFORM AY05 THRU AY59.
      SUBTRACT SOR-QNT  FROM STK-QUANT.
      ADD SOR-QNT  TO STK-QPIC.
            PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
      PERFORM BD225.
            ADD 1   TO WS-S6.

       BD208.
           IF WS-S6 < 31
               GO TO BD207.

       BD209.
             GO TO BD205.

       BD210.
             MOVE SAL-DESC       TO SOR-DESC.
             GO TO BD200.
      *    *************************************************************
      *    ****    L O G   R E C O R D   T O   O R D E R   F I L E
      *    *************************************************************
       BD225.
             PERFORM WRITE-SORDER THRU WRITE-SORDER-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY17 THRU AY59.
      *    *************************************************************
      *    ****    R E A D   T H E   S A L E   F I L E   A N D
      *            L O G   T O   O R D E R   F I L E
      *    *************************************************************
       BD235.
            ADD 1   TO WS-SALKEY.
           IF WS-SALKEY > WS-LAST
               GO TO BD255.
             PERFORM READ-SALE THRU READ-SALE-EXIT.
          IF SAL-EXT-ITEM = SPACES
               GO TO BD247.
             MOVE 1              TO SOR-TYPE.
            MOVE SAL-EXT-ITEM  TO SOR-EXT-ITEM.
             MOVE SAL-DESC       TO SOR-DESC.
            MOVE SAL-QNT  TO SOR-QNT.
      *        SOR-QORD.
             MOVE SAL-COST       TO SOR-COST.
             MOVE SAL-VAT        TO SOR-VAT.
            MOVE SAL-VAT-SUB  TO SOR-VAT-SUB.
             MOVE SAL-SELL       TO SOR-SELL.
             MOVE SAL-VAL        TO SOR-VAL.
             MOVE SAL-DISC       TO SOR-DISC.
             MOVE SAL-DSC        TO SOR-DSC.
             MOVE SAL-INCTAX     TO SOR-INCTAX.
             MOVE SAL-NONTAX     TO SOR-NONTAX.
             MOVE SAL-TAXVAL     TO SOR-TAXVAL.
      MOVE SAL-PACK  TO SOR-PACK.
             MOVE SPACES         TO SOR-SERNO.
             ADD 1               TO SOR-SEQ.
             MOVE ZERO           TO SOR-SUB.
             PERFORM BD225.
      *    *************************************************************
      *    ****    U P D A T E  P I C K I N G  S L I P   Q T Y
      *     O N  S T O C K   R E C O R D
      *    *************************************************************
            MOVE SAL-EXT-ITEM  TO STK-CODE.
      MOVE 0   TO WS-ACTION.
      PERFORM AY05 THRU AY59.
      SUBTRACT SOR-QNT  FROM STK-QUANT.
      ADD SOR-QNT  TO STK-QPIC.
            PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
      PERFORM BD248 THRU BD254.
             GO TO BD235.

       BD247.
           IF SAL-SUPER = 1
               MOVE 4            TO SOR-TYPE
           ELSE
               MOVE 3            TO SOR-TYPE.
             INITIALIZE SOR-DET2.
             MOVE SAL-DESC       TO SOR-DESC.
             ADD 1               TO SOR-SEQ.
             MOVE ZERO           TO SOR-SUB.
             GO TO BD225.
       BD248.
      MOVE SOR-EXT-ITEM  TO PRT-EXT-ITEM.
             PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
           IF WS-F-ERROR = 21
               GO TO BD254.
      *    *************************************************************
      *    ****  P R O D U C T I O N   D E T A I L S
      *    *************************************************************
             MOVE 1              TO WS-S6.
       BD249.
          IF PRT-EXT-CODE(WS-S6) = SPACES
               ADD 1             TO WS-S6
               GO TO BD250.
            MOVE PRT-EXT-CODE(WS-S6)
     TO W10-EXT-ITEM.
             PERFORM CA000.
           IF WS-F-ERROR = 22
               ADD 1             TO WS-S6
               GO TO BD250.
             MOVE 2              TO SOR-TYPE.
             INITIALIZE SOR-DET2.
      MOVE STK-CODE  TO SOR-EXT-ITEM.
             MOVE STK-DESC       TO SOR-DESC.
      MULTIPLY SAL-QNT BY PRT-QUANT(WS-S6)
                                 GIVING SOR-QNT.
             ADD 1               TO SOR-SEQ.
             MOVE ZERO           TO SOR-SUB.
             PERFORM BD225.
             ADD 1               TO WS-S6.
       BD250.
           IF WS-S6 < 31
               GO TO BD249.

       BD254.
             EXIT.
      *    *************************************************************
      *    ****    C H E C K / U P D A T E   C L I E N T   D E T A I L S
      *    *************************************************************
       BD255.
    IF W40-AD1 = SPACES
               GO TO BD256.
           IF W35-KEY = SPACES
               MOVE 5            TO SOR-TYPE
               ADD 1             TO SOR-SEQ
           ELSE
               MOVE W35-KEY      TO SOR-KEY
              PERFORM READ-SORDER THRU READ-SORDER-EXIT
               MOVE 0            TO WS-ACTION
               PERFORM AY17 THRU AY59.
             MOVE W40-AD1        TO SOR-AD1.
             MOVE W40-AD2        TO SOR-AD2.
             MOVE W40-AD3        TO SOR-AD3.
             MOVE W40-AD4        TO SOR-AD4.
             MOVE W40-PC         TO SOR-PC.
           IF W35-KEY = SPACES
               PERFORM WRITE-SORDER THRU WRITE-SORDER-EXIT
               MOVE 1            TO WS-ACTION
               PERFORM AY17 THRU AY59
           ELSE
               PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT.
      *    *************************************************************
      *    ****    O R D E R   T O T A L S
      *    *************************************************************
       BD256.
             MOVE 99900          TO SOR-SQN.
            PERFORM READ-SORDER THRU READ-SORDER-EXIT.
             MOVE 0              TO WS-ACTION.
             PERFORM AY17 THRU AY59.
             MOVE W90-DB         TO SOR-SUBTOT.
             MULTIPLY -1 BY W90-TDISC
                                 GIVING SOR-DSCNT
            MOVE W90-EXCTOT  TO SOR-TAXABLE.
             MOVE W90-NONTAX     TO SOR-NON-TXBL.
             MOVE W90-TTAX       TO SOR-TAX.
             MOVE W90-TOTAL      TO SOR-TOT.
             MOVE W90-BAL        TO SOR-BAL.
      MOVE W90-INSTRUCT  TO SOR-INSTRUCT.
            PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT.
      *    *************************************************************
      *    ****    E N D   O F F   T H E   R E C O V E R Y   L O G
      *    *************************************************************
             PERFORM AY70 THRU AY999.
       BD260.
            MOVE "Print picking slip (Y/N)?  [ ]"
     TO WS-ERR-MES.
            MOVE "Y"   TO WS-OPTION.
      PERFORM OPT-MESSAGE
       UNTIL WS-OPTION = "Y" OR "N".
           IF WS-OPTION = "Y"
               PERFORM ERASE-SCREEN
              DISPLAY "AMEND ORDER" AT 0235
   WITH FOREGROUND-COLOR Grey HIGHLIGHT
               DISPLAY "Printing picking slip" AT 1212
                        WITH FOREGROUND-COLOR Brown HIGHLIGHT
              MOVE W90-REF  TO LS-SLS-ORDNO
               PERFORM BB200.
      *    *************************************************************
      *    ****    C L O S E   F I L E S
      *            ( FORCE SYSTEM TO WRITE DETAILS TO DISK )
      *    *************************************************************
             CLOSE SALE.
             OPEN OUTPUT SALE.
             CLOSE SALE
                   SORDER.
             OPEN I-O SORDER
                      SALE.
            MOVE "ENTER to continue - E to exit"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF WS-OPTION = "E"
              GO TO BD268.
             GO TO BD05.

       BD268.
      EXIT.
      *    *************************************************************
      *    ****    A M E N D   S A L E S   D E T A I L S
      *    *************************************************************
       BD270A  SECTION 51.
       BD270.
      MOVE "Amend - 'H'eader, 'S'ales details or 'E'xit"
     TO WS-ERR-MES.
      MOVE "S"   TO WS-OPTION.

       BD271.
      PERFORM OPT-MESSAGE TEST AFTER
       UNTIL WS-OPTION = "E" OR "H" OR "S".
           IF WS-OPTION = "S"
               GO TO BD272
           ELSE
           IF WS-OPTION = "E"
        GO TO BD590.
             MOVE W10-SORD       TO SOR-REF.
             MOVE 100            TO SOR-SQN.
             MOVE ZERO           TO WS-ERROR.
             PERFORM READ-SORDER THRU READ-SORDER-EXIT.

       BD271A.
      MOVE "Amend-'O'rd No,'S'alesman,'T'el.,'C'ont.,'E'xit"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.

       BD271B.
      PERFORM OPT-MESSAGE
       UNTIL WS-OPTION = "C" OR "E" OR "O" OR "S" OR "T".
           IF WS-OPTION = "E"
        GO TO BD582.
            PERFORM READ-SORDER THRU READ-SORDER-EXIT.
           IF WS-OPTION = "C"
               ACCEPT W40-CONTACT AT 0528
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta UPDATE AUTO
               MOVE W40-CONTACT  TO SOR-CONTACT
        PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT
               GO TO BD271A
           ELSE
           IF WS-OPTION = "O"
               ACCEPT W90-ORD AT 0368
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta UPDATE AUTO
               MOVE W90-ORD      TO SOR-ORD
        PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT
               GO TO BD271A
           ELSE
           IF WS-OPTION = "T"
               ACCEPT W40-TEL AT 0466
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta UPDATE AUTO
               MOVE W40-TEL      TO SOR-TEL
        PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT
               GO TO BD271A.
       BD271C.
            ACCEPT W10-SMAN AT 0411
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
           IF W10-SMAN > 50
               GO TO BD271C.
           IF W10-SMAN = ZERO
        DISPLAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" AT 0420
                       WITH FOREGROUND-COLOR Cyan
               MOVE ZERO         TO SOR-SMAN
        PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT
              GO TO BD271A.
             ADD 100 W10-SMAN    GIVING WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF PAR-SALESMAN = SPACES
              MOVE "Invalid Salesman Number"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BD271C.
            DISPLAY PAR-SALESMAN AT 0420
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             MOVE W10-SMAN       TO SOR-SMAN.
      PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT.
             GO TO BD271A.

       BD272.
             MOVE 11              TO LIN.
             PERFORM BD115 UNTIL LIN > 41.
             MOVE 11              TO LIN.
             MOVE 1              TO WS-LINES WS-FIRST.

       BD275.
            MOVE WS-FIRST  TO WS-SALKEY WS-SALPOS.
      *    *************************************************************
      *    ****    H A V E   A L L   T H E   E N T R I E S   B E E N
      *            D I S P L A Y E D
      *    *************************************************************
       BD280.
          IF WS-SALKEY > WS-LAST
        SUBTRACT 1  FROM WS-SALKEY
              GO TO BD284.
             PERFORM READ-SALE THRU READ-SALE-EXIT.
      MOVE SAL-EXT-ITEM  TO W10-EXT-ITEM.
      *    *************************************************************
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *    *************************************************************
          IF SAL-EXT-ITEM = SPACES
              DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
         WITH FOREGROUND-COLOR Grey HIGHLIGHT
       BACKGROUND-COLOR Black
        IF SAL-SUPER = 1
     DISPLAY "(I)" AT LINE LIN COLUMN 5
       WITH FOREGROUND-COLOR Brown HIGHLIGHT
               ELSE
     DISPLAY "(R)" AT LINE LIN COLUMN 5
       WITH FOREGROUND-COLOR Grey HIGHLIGHT
               END-IF
               GO TO BD282.
      *    *************************************************************
      *        *****   E X T E N D E D  C O D E   *****
      *    *************************************************************
    IF WS-EXT-STK = "Y"
        DISPLAY S07-EXT AT LINE LIN
    ELSE
      *    *************************************************************
      *    *****   A L P H A - N U M E R I C   C O D E  *****
      *    *************************************************************
    IF W10-STCK = "A"
        IF WS-USE-ITM = "Y"
     DISPLAY S07-ITM AT LINE LIN
        ELSE
           DISPLAY S07 AT LINE LIN.
            DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Black.
             MOVE SAL-QNT        TO W10-QUANT.
             DISPLAY S26-QNT AT LINE LIN.
             MOVE SAL-SELL       TO W10-SELL.
             DISPLAY S26-PRC AT LINE LIN.
             MOVE SAL-VAL        TO W100-EXT.
            DISPLAY W100-EXT AT LINE LIN COLUMN 70
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Black.
      *    *************************************************************
      *    ****    I S   T H E   S C R E E N   F U L L
      *    *************************************************************
       BD282.
           IF LIN < 41
               ADD 1             TO LIN WS-SALKEY WS-LINES
               GO TO BD280.

       BD284.
      MOVE 11   TO LIN.
      PERFORM CLEAR-L50.

       BD285.
      *    *************************************************************
      *    ****    D I S P L A Y   O P T I O N S
      *    *************************************************************
            DISPLAY " " AT 2503 WITH FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Cyan
             " highlight," WITH FOREGROUND-COLOR Blue
       BACKGROUND-COLOR Cyan
       "PgUp" WITH FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Cyan
       "/" WITH FOREGROUND-COLOR Blue
         BACKGROUND-COLOR Cyan
       "PgDn" WITH FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Cyan
       "," WITH FOREGROUND-COLOR Blue
    BACKGROUND-COLOR Cyan
             "A" WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Cyan
             "mend," WITH FOREGROUND-COLOR Blue
           BACKGROUND-COLOR Cyan
             "D" WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Cyan
             "elete or " WITH FOREGROUND-COLOR Blue
           BACKGROUND-COLOR Cyan
             "Esc" WITH FOREGROUND-COLOR Brown HIGHLIGHT
    BACKGROUND-COLOR Cyan
       "ape" WITH FOREGROUND-COLOR Blue
    BACKGROUND-COLOR Cyan.
      *    *************************************************************
      *    ****    S A V E   C U R R E N T   P A G E
      *    *************************************************************
      PERFORM SAVE-SCREEN-2.
      *    *************************************************************
      *    ****    S C R E E N  A T T R I B U T E S
      *    *************************************************************
       BD289.
             MOVE 3920           TO W41-LENGTH.
      CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
           W42-ATTRIB2
           W41-LENGTH
     RETURNING WS-STATUS.
      MOVE 4000   TO W41-LENGTH.
      *    *************************************************************
      *    ****    G E T   U S E R   O P T I O N
      *    *************************************************************
       BD290.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
       GO TO BD292
   WHEN DOWN-KEY
             GO TO BD293
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO BD290
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       PERFORM CLEAR-L50
       GO TO BD582
      *    *************************************************************
      *    ****    I S   N E X T   S C R E E N   R E Q U I R E D
      *    *************************************************************
   WHEN PAGE-DOWN
             GO TO BD305
      *    *************************************************************
      *    ****    I S   P R E V I O U S   S C R E E N   R E Q U I R E D
      *    *************************************************************
   WHEN PAGE-UP
             GO TO BD300
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO BD290
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
        IF (WS-OPTION = "A" OR "D") AND
    (LIN > 10)
     PERFORM CLEAR-L50
     GO TO BD310.
      CALL X"E5".
            GO TO BD290.
      *    *************************************************************
      *    ****    U P  K E Y  ( M O V E   H I G H L I G H T  U P )
      *    *************************************************************
       BD292.
    IF (WS-SALPOS = 1) OR (LIN < 11)
        GO TO BD294.
      SUBTRACT 1   FROM WS-SALPOS.
    IF LIN > 11
        SUBTRACT 1  FROM LIN
        GO TO BD294.
      MOVE 41   TO WS-S7.
      *    *************************************************************
      *    ****     M O V E   T H E   D E T A I L S   D O W N
      *      O N E   L I N E
      *    *************************************************************
       BD292-LOOP.
      SUBTRACT 1 FROM WS-S7
     GIVING WS-S8.
      MOVE W42-LINES(WS-S8)
     TO W42-LINES(WS-S7).
      MOVE W43-LINE(WS-S8)
     TO W43-LINE(WS-S7).
    IF WS-S7 > 12
        SUBTRACT 1  FROM WS-S7
        GO TO BD292-LOOP.
      MOVE WS-SALPOS  TO WS-SALKEY WS-FIRST.
      *    *************************************************************
      *    ****    C L E A R   F I R S T   O R  L A S T
      *     D E T A I L  L I N E.
      *    *************************************************************
       BD292-DISPLAY.
      PERFORM RESTORE-SCREEN-2.
            DISPLAY WS-INV1 AT LINE LIN COLUMN 1
       WITH BACKGROUND-COLOR Black FOREGROUND-COLOR Cyan
       WS-INV78 WITH FOREGROUND-COLOR Cyan
       WS-INV80 WITH BACKGROUND-COLOR Black
       FOREGROUND-COLOR Cyan.
      *    *************************************************************
      *    ****    G E T   N E X T   O R   P R E V I O U S
      *     D E T A I L  L I N E.
      *    *************************************************************
            PERFORM READ-SALE THRU READ-SALE-EXIT.
      MOVE SAL-EXT-ITEM  TO W10-EXT-ITEM.
      *    *************************************************************
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *    *************************************************************
          IF SAL-EXT-ITEM = SPACES
              DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
         WITH FOREGROUND-COLOR Grey HIGHLIGHT
       BACKGROUND-COLOR Black
        IF SAL-SUPER = 1
     DISPLAY "(I)" AT LINE LIN COLUMN 5
       WITH FOREGROUND-COLOR Brown HIGHLIGHT
               ELSE
     DISPLAY "(R)" AT LINE LIN COLUMN 5
       WITH FOREGROUND-COLOR Grey HIGHLIGHT
        END-IF
              GO TO BD292-END-DISP.
      *    *************************************************************
      *        *****   E X T E N D E D  C O D E   *****
      *    *************************************************************
    IF WS-EXT-STK = "Y"
        DISPLAY S07-EXT AT LINE LIN
    ELSE
      *    *************************************************************
      *    *****   A L P H A - N U M E R I C   C O D E  *****
      *    *************************************************************
    IF W10-STCK = "A"
        IF WS-USE-ITM = "Y"
     DISPLAY S07-ITM AT LINE LIN
        ELSE
           DISPLAY S07 AT LINE LIN.
            DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Black.
             MOVE SAL-QNT        TO W10-QUANT.
             DISPLAY S26-QNT AT LINE LIN.
             MOVE SAL-SELL       TO W10-SELL.
             DISPLAY S26-PRC AT LINE LIN.
             MOVE SAL-VAL        TO W100-EXT.
            DISPLAY W100-EXT AT LINE LIN COLUMN 70
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Black.

       BD292-END-DISP.
      PERFORM SAVE-SCREEN-2.
      GO TO BD294.
      *    *************************************************************
      *    ****    D O W N   K E Y   ( M O V E  H I G H L I G H T
      *           D O W N )
      *    *************************************************************
       BD293.
    IF (WS-SALPOS = WS-LAST) AND (LIN > 11)
        GO TO BD294.
      *    *************************************************************
      *    ****    WS-SALPOS CONTAINS KEY OF HIGHLIGHTED ITEM
      *    *************************************************************
    IF LIN > 11
        ADD 1   TO WS-SALPOS.
    IF LIN < 41
        ADD 1   TO LIN
        GO TO BD294.
      MOVE 11   TO WS-S7.
      *    *************************************************************
      *    ****     M O V E   T H E   D E T A I L S   U P   O N E
      *      L I N E
      *    *************************************************************
       BD293-LOOP.
      ADD 1 WS-S7  GIVING WS-S8.
      MOVE W42-LINES(WS-S8)
     TO W42-LINES(WS-S7).
      MOVE W43-LINE(WS-S8)
     TO W43-LINE(WS-S7).
    IF WS-S8 < 41
        ADD 1   TO WS-S7
        GO TO BD293-LOOP.
      ADD 1   TO WS-FIRST.
      MOVE WS-SALPOS  TO WS-SALKEY.
      GO TO BD292-DISPLAY.
      *    *************************************************************
      *    ****    H I G H L I G H T   C U R R E N T   L I N E
      *    *************************************************************
       BD294.
            PERFORM BD289.
    IF LIN > 10
              DISPLAY W43-DET(LIN) AT LINE LIN COLUMN 1
         WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
            GO TO BD290.
      *    *************************************************************
      *    ****    C L E A R   S C R E E N
      *    *************************************************************
       BD295.
            MOVE 11   TO LIN.
            PERFORM BD115 UNTIL LIN > 41.
            MOVE 11   TO LIN.
             MOVE 1              TO WS-LINES.
      *    *************************************************************
      *    ****   P R E V I O U S   S C R E E N   R E Q U I R E D
      *    *************************************************************
       BD300.
    IF WS-FIRST = 1
        GO TO BD290.
    IF WS-FIRST > 31
        SUBTRACT 31  FROM WS-FIRST
    ELSE
        MOVE 1   TO WS-FIRST.
      PERFORM BD295.
      GO TO BD275.
      *    *************************************************************
      *    ****    N E X T   S C R E E N   R E Q U I R E D
      *    *************************************************************
       BD305.
    IF (WS-FIRST + 31) > WS-LAST
        GO TO BD290.
      ADD 31   TO WS-FIRST.
      PERFORM BD295.
      GO TO BD275.
      *    *************************************************************
      *    ****    M U S T   A N   I T E M   B E   A M E N D E D
      *    *************************************************************
       BD310.
          IF WS-OPTION = "A"
              GO TO BD320.
      *    *************************************************************
      *    ****    M A K E   S U R E   T H A T   A N   I T E M
      *            M U S T   B E   D E L E T E D
      *    *************************************************************
       BD315.
            MOVE "Are you sure Y/N?  [ ]"
     TO WS-ERR-MES.
            MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE
       UNTIL WS-OPTION = "N" OR "Y".
           IF WS-OPTION = "N"
              GO TO BD285.
             MOVE "D"            TO WS-OPTION.
      *    *************************************************************
      *    ****    R E A D   T H E   D E T A I L S   F R O M   T H E
      *            S A L E   F I L E
      *    *************************************************************
       BD320.
            MOVE WS-SALPOS  TO WS-SALKEY.
             PERFORM READ-SALE THRU READ-SALE-EXIT.
          IF SAL-EXT-ITEM = SPACES
              GO TO BD322.
            MOVE SAL-EXT-ITEM  TO W10-EXT-ITEM STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
             MOVE SAL-QNT        TO W10-QUANT.
             MOVE SAL-COST       TO W10-COST.
             MOVE SAL-SELL       TO W10-SELL.
             MOVE SAL-DISC       TO W90-CDISC.
             MOVE SAL-DSC        TO WS-DISC.
             MOVE SAL-VAL        TO W25-VALUE.
      MOVE SAL-VAT-SUB  TO WS-VAT-SUB.
      MOVE SAL-PACK  TO WS-P1.
      *    *************************************************************
      *    ****    R E D U C E   T O T A L S
      *    *************************************************************
             SUBTRACT W25-VALUE  FROM W90-DB.
             SUBTRACT W90-CDISC  FROM W90-LDISC.
             SUBTRACT SAL-NONTAX FROM W90-NONTAX.
    IF WS-VAT-SUB > ZERO
              SUBTRACT SAL-TAXVAL FROM W90-TAXVAL(WS-VAT-SUB)
        SUBTRACT SAL-VAT  FROM W90-VAT(SAL-VAT-SUB).
          IF (STK-IND < 2) OR (STK-IND = 3)
               COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).
      *    *************************************************************
      *    ****    I S   I T   A N   A M E N D M E N T
      *    *************************************************************
       BD322.
           IF WS-OPTION = "A"
               GO TO BD325.
            PERFORM BD289.
      *    *************************************************************
      *    ****   U P D A T E   S T O C K   Q U A N T I T Y
      *    *************************************************************
          IF NOT(SAL-EXT-ITEM = SPACES)
        IF SAL-INSTRUCT = "Y"
     ADD SAL-QNT  TO STK-QUANT
     SUBTRACT SAL-QNT
     FROM STK-QPIC
           PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
      *    *************************************************************
      *    ****    R E - D I S P L A Y   T O T A L S
      *    *************************************************************
          IF SAL-EXT-ITEM NOT = SPACES
               PERFORM BD100 THRU BD102.
      *    *************************************************************
      *    ****    D E L E T E   I T E M   F R O M   O R D E R   F I L E
      *    *************************************************************
             ADD 1               TO W14-SUB.
            MOVE SAL-SQN  TO W14-SQN(W14-SUB).
      *    *************************************************************
      *    ****    D E L E T E   I T E M   F R O M   S A L E   F I L E
      *    *************************************************************
       BD324.
            MOVE WS-SALKEY  TO WS-SALSTR.
             ADD 1               TO WS-SALKEY.
           IF WS-SALKEY > WS-LAST
               SUBTRACT 1        FROM WS-LAST
               GO TO BD272.
             PERFORM READ-SALE THRU READ-SALE-EXIT.
             SUBTRACT 1          FROM SAL-CODE.
             MOVE WS-SALSTR      TO WS-SALKEY.
             PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
             ADD 1               TO WS-SALKEY.
            GO TO BD324.

       BD325.
      PERFORM BD289.
          IF SAL-EXT-ITEM = SPACES
              ACCEPT SAL-SDSC AT LINE LIN COLUMN 21
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
            BACKGROUND-COLOR Black UPDATE AUTO
              GO TO BD581.

       BD330.
            PERFORM AA300.
      *    *************************************************************
      *    ****    G E T   S T O C K   C O D E
      *    *************************************************************
    IF WS-EXT-STK = "Y"
        ACCEPT S07-EXT AT LINE LIN
    ELSE
      *    *************************************************************
      *        ***** ALPHA-NUMERIC CODE *****
      *    *************************************************************
    IF W10-STCK = "A"
        IF WS-USE-ITM = "Y"
     ACCEPT S07-ITM AT LINE LIN
        ELSE
           ACCEPT S07 AT LINE LIN
        END-IF
    ELSE
      *    *************************************************************
      *    ***** NUMERIC CODES APPLY *****
      *    *************************************************************
    IF W10-SLNGTH = 3
        ACCEPT S07-3 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 4
        ACCEPT S07-4 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 5
        ACCEPT S07-5 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 6
        ACCEPT S07-6 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 7
        ACCEPT S07-7 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 8
        ACCEPT S07-8 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 9
        ACCEPT S07-9 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 10
        ACCEPT S07-10 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 11
        ACCEPT S07-11 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 12
        ACCEPT S07-12 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 13
        ACCEPT S07-13 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 14
        ACCEPT S07-14 AT LINE LIN.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       MOVE "I"  TO W10-ETYPE
   WHEN F3-KEY
       MOVE "A"  TO W10-ETYPE
   WHEN F4-KEY
       MOVE "D"  TO W10-ETYPE
   WHEN F5-KEY
       MOVE "2"  TO W10-ETYPE
   WHEN F6-KEY
       MOVE "X"  TO W10-ETYPE
   WHEN F9-KEY
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Lookup using any word or part in the Stock Description.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       MOVE "W"  TO W10-ETYPE
   WHEN OTHER
                     CALL X"E5"
       GO TO BD330
               END-EVALUATE
              PERFORM AA50 THRU AA59
      *    *************************************************************
      *        *****   E X T E N D E D  C O D E   *****
      *    *************************************************************
        IF WS-EXT-STK = "Y"
     DISPLAY S07-EXT AT LINE LIN
        ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
        IF W10-STCK = "A"
     IF WS-USE-ITM = "Y"
         DISPLAY S07-ITM AT LINE LIN
     ELSE
               DISPLAY S07 AT LINE LIN.
              IF W10-EXT-ITEM = SPACES
     MOVE SAL-EXT-ITEM
     TO W10-EXT-ITEM
           GO TO BD330.

       BD335.
             PERFORM CA000.
      *    *************************************************************
      *        *****   E X T E N D E D  C O D E   *****
      *    *************************************************************
    IF WS-EXT-STK = "Y"
        DISPLAY S07-EXT AT LINE LIN
    ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
    IF W10-STCK = "A"
        IF WS-USE-ITM = "Y"
     DISPLAY S07-ITM AT LINE LIN
        ELSE
           DISPLAY S07 AT LINE LIN.
          IF WS-F-ERROR = 22
        MOVE "Z"   TO W10-ETYPE
              PERFORM AA50 THRU AA59
      *    *************************************************************
      *        *****   E X T E N D E D  C O D E   *****
      *    *************************************************************
        IF WS-EXT-STK = "Y"
     DISPLAY S07-EXT AT LINE LIN
        ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
        IF W10-STCK = "A"
     IF WS-USE-ITM = "Y"
         DISPLAY S07-ITM AT LINE LIN
     ELSE
               DISPLAY S07 AT LINE LIN
     END-IF
        END-IF
              IF W10-EXT-ITEM = SPACES
     MOVE SAL-EXT-ITEM
     TO W10-EXT-ITEM
           GO TO BD330
               ELSE
     GO TO BD335.
            MOVE STK-CODE  TO SAL-EXT-ITEM.
            PERFORM CLEAR-L50.
      *    *************************************************************
      *    ****    I S   I T   A   F R E E - F O R M A T   I T E M
      *    *************************************************************
       BD340.
           IF STK-IND = 3
               MOVE ZERO         TO STK-AVGE
              ACCEPT W10-SDSC AT LINE LIN COLUMN 21
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
            BACKGROUND-COLOR Black UPDATE AUTO
               DISPLAY "Index Unit cost" AT 2512
         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
               ACCEPT S26-AVGE AT LINE 25
           ELSE
               MOVE STK-DESC     TO W10-DESC
              DISPLAY W10-SDSC AT LINE LIN COLUMN 21
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Black.
           IF STK-IND = 3
               IF STK-AVGE = ZERO
           MOVE "Unit cost is zero"
     TO WS-ERR-STRING
           PERFORM ERROR-MESSAGE
           GO TO BD340.
            PERFORM CLEAR-L50.
      *    *************************************************************
      *    ****    L O G   D E T A I L S   T O   S A L E S   F I L E
      *    *************************************************************
            MOVE W10-EXT-ITEM  TO SAL-EXT-ITEM.
            MOVE W10-DESC  TO SAL-MSER.
             MOVE STK-AVGE       TO SAL-COST W10-COST.
      *    *************************************************************
      *    ****    A M E N D   Q U A N T I T Y
      *    *************************************************************
       BD345.
             ACCEPT S26-QNT AT LINE LIN.
           IF W10-QUANT = ZERO
              MOVE "Quantity MAY NOT be zero"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               MOVE SAL-QNT      TO W10-QUANT
              GO TO BD345.
      *    *************************************************************
      *    ****    A M E N D   S E L L I N G   P R I C E
      *    *************************************************************
      PERFORM BD66A THRU BD75.
      *    *************************************************************
      *    ****    R E C A L C U L A T E   V A L U E S
      *    *************************************************************
             MULTIPLY W10-SELL BY W10-QUANT
                                 GIVING W25-VALUE.
             MOVE W25-VALUE      TO W100-EXT.
            DISPLAY W100-EXT AT LINE LIN COLUMN 70
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Black.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
              GO TO BD325.
      *    *************************************************************
      *    ****    U P D A T E   S A L E   F I L E   R E C O R D
      *    *************************************************************
             MOVE W10-QUANT      TO SAL-QNT.
             MOVE W10-SELL       TO SAL-SELL.
             MOVE W25-VALUE      TO SAL-VAL.
           IF WS-DISC = ZERO
              GO TO BD580.
             COMPUTE W90-CDISC = (W25-VALUE * WS-DISC) / 100.
             MOVE W90-CDISC      TO SAL-DISC.
      *    *************************************************************
      *    ****    R E - C A L C U L A T E   &   D I S P L A Y
      *            T O T A L S
      *    *************************************************************
       BD580.
            PERFORM BD95 THRU BD102.

       BD581.
             PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
             GO TO BD272.
      *    *************************************************************
      *    ****    C L E A R   D E T A I L   L I N E S
      *    *************************************************************
       BD582.
      PERFORM BD295.
      *    *************************************************************
      *    ****    D I S P L A Y   L A S T   P A G E
      *    *************************************************************
    IF WS-LAST > 10
        SUBTRACT 9 FROM WS-LAST
     GIVING WS-SALKEY
    ELSE
        MOVE 1   TO WS-SALKEY.

       BD583.
            PERFORM READ-SALE THRU READ-SALE-EXIT.
      MOVE SAL-EXT-ITEM  TO W10-EXT-ITEM.
      *    *************************************************************
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *    *************************************************************
          IF SAL-EXT-ITEM = SPACES
              DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
         WITH FOREGROUND-COLOR Grey HIGHLIGHT
       BACKGROUND-COLOR Black
        IF SAL-SUPER = 1
     DISPLAY "(I)" AT LINE LIN COLUMN 5
       WITH FOREGROUND-COLOR Brown HIGHLIGHT
               ELSE
     DISPLAY "(R)" AT LINE LIN COLUMN 5
       WITH FOREGROUND-COLOR Grey HIGHLIGHT
        END-IF
              GO TO BD584.
      *    *************************************************************
      *        *****   E X T E N D E D  C O D E   *****
      *    *************************************************************
    IF WS-EXT-STK = "Y"
        DISPLAY S07-EXT AT LINE LIN
    ELSE
      *    *************************************************************
      *    *****   A L P H A - N U M E R I C   C O D E  *****
      *    *************************************************************
    IF W10-STCK = "A"
        IF WS-USE-ITM = "Y"
     DISPLAY S07-ITM AT LINE LIN
        ELSE
           DISPLAY S07 AT LINE LIN.
            DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Black.
            MOVE SAL-QNT  TO W10-QUANT.
             DISPLAY S26-QNT AT LINE LIN.
             MOVE SAL-SELL       TO W10-SELL.
             DISPLAY S26-PRC AT LINE LIN.
             MOVE SAL-VAL        TO W100-EXT.
            DISPLAY W100-EXT AT LINE LIN COLUMN 70
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Black.
       BD584.
      ADD 1   TO LIN.
          IF WS-SALKEY < WS-LAST
        ADD 1   TO WS-SALKEY
              GO TO BD583.

       BD590.
             EXIT.

       BD591A        SECTION 51.
       BD591.
      PERFORM SAVE-SCREEN.
      MOVE 0815   TO CPOS.
       BD592.
             DISPLAY
   "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°"
                 AT CPOS.
           IF CLIN < 41
               ADD 1             TO CLIN
        GO TO BD592.
             DISPLAY S40.
             PERFORM BB15 THRU BB29.
      PERFORM RESTORE-SCREEN.
       BD593.
             EXIT.
      /    *************************************************************
      *    ****    D E L E T E   O R D E R
      *    *************************************************************
       BE000         SECTION 52.
       BE00.
      *    *************************************************************
      *    ****    S U P E R V I S O R   P A S S W O R D S
      *    *************************************************************
             MOVE 5              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-PW         TO W85-PASS.
       BE05.
      *    *************************************************************
      *    ****    S A L E S   O R D E R   S C R E E N
      *    *************************************************************
             PERFORM ERASE-SCREEN.
             DISPLAY S30.
      *    *************************************************************
      *            C H A N G E   S C R E E N   H E A D I N G
      *    *************************************************************
             DISPLAY S31A.
             MOVE 11              TO LIN.
             PERFORM BE115 UNTIL LIN > 18.
             MOVE W12-TODAY      TO W90-DTE.
            DISPLAY W90-DTE AT 0570
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             MOVE ZERO           TO W90-BDT W90-NONTAX W90-LDISC
              W90-BAL W90-SALES
              W90-CUR W90-EXCTOT.
            MOVE 1   TO WS-FIRST WS-SALPOS WS-LAST.
             MOVE ZERO           TO W10-SORD.

       BE10.
             PERFORM AA400.
             ACCEPT S30A AT 0510.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE ZERO  TO W10-SORD
       GO TO BE599
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       MOVE "O"  TO WS-ETYPE
   WHEN F3-KEY
       MOVE "A"  TO WS-ETYPE
   WHEN F4-KEY
       MOVE "T"  TO WS-ETYPE
   WHEN OTHER
       CALL X"E5"
       GO TO BE10
        END-EVALUATE
              PERFORM AA50O THRU AA59
               DISPLAY S30A AT 0510
               IF W10-SORD = ZERO
                   GO TO BE10.
            PERFORM CLEAR-L50.
           IF W10-SORD = ZERO
               GO TO BE599.
             MOVE W10-SORD       TO W90-REF.
       BE12.
             PERFORM GET-SORDER.
           IF WS-ERROR = 1
              MOVE WS-ERR2  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BE10.
           IF WS-ERROR = 2
              MOVE "Not an order"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BE10.
           IF WS-ERROR = 3
              MOVE "Order corrupted"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BE10.
           IF WS-ERROR = 4
              MOVE "Invalid a/c no"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BE10.
           IF W90-EXP = "E" OR "X"
               MOVE W05-VATNO    TO W90-TAX
           ELSE
               MOVE SPACES       TO W90-TAX.
             DISPLAY S36 AT 0308
            DISPLAY W40-NAME AT 0320
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            DISPLAY W90-ORD AT 0368
       WITH FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            DISPLAY W10-SMAN AT 0411
       WITH FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta.
           IF W10-SMAN NOT = ZERO
               ADD 100 W10-SMAN    GIVING WS-PARKEY
               PERFORM READ-PARAM THRU READ-PARAM-EXIT
              DISPLAY PAR-SALESMAN AT 0420
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta.
      DISPLAY W40-TEL AT 0466
       WITH FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      DISPLAY W40-CONTACT AT 0528
       WITH FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             MOVE 1              TO WS-SALKEY.
             SUBTRACT 1          FROM WS-LAST.
       BE15.
             MOVE W90-DB         TO W90-TOTAL.
             SUBTRACT W90-LDISC  FROM W90-TOTAL.
             MOVE W90-DB         TO W100-BAL.
            DISPLAY W100-BAL AT 2015
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             MULTIPLY -1 BY W90-LDISC
                                 GIVING W100-BAL.
            DISPLAY W100-BAL AT 2115
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      *    *************************************************************
      *    ****    C A L C U L A T E   T A X   ( RUNNING TOTAL )
      *    *************************************************************
          IF W90-TAX = SPACES
               GO TO BE20.
          IF W90-EXCTOT NOT = ZERO
              MOVE W90-TTAX  TO W100-BAL W25-TOTAL
              DISPLAY W100-BAL AT 2215
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta
               ADD W25-TOTAL     TO W90-TOTAL.
       BE20.
            MOVE W90-TOTAL  TO W100-BAL.
            DISPLAY W100-BAL AT 2315
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      *    *************************************************************
      *    ****    D I S P L A Y   S A L E S   D E T A I L S
      *    *************************************************************
       BE25.
             MOVE 11              TO LIN.
             PERFORM BD115 UNTIL LIN > 41.
             MOVE 11              TO LIN.
             MOVE 1              TO WS-LINES WS-FIRST.
       BE30.
            MOVE WS-FIRST  TO WS-SALKEY.
       BE35.
      *    *************************************************************
      *    H A V E   A L L   E N T R I E S   B E E N   D I S P L A Y E D
      *    *************************************************************
           IF WS-SALKEY > WS-LAST
               GO TO BE45.
             PERFORM READ-SALE THRU READ-SALE-EXIT.
      MOVE SAL-EXT-ITEM  TO W10-EXT-ITEM.
      *    *************************************************************
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *    *************************************************************
          IF SAL-EXT-ITEM = SPACES
              DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
         WITH FOREGROUND-COLOR Grey HIGHLIGHT
       BACKGROUND-COLOR Black
               IF SAL-SUPER = 1
     DISPLAY "(I)" AT LINE LIN COLUMN 5
       WITH FOREGROUND-COLOR Brown HIGHLIGHT
               ELSE
     DISPLAY "(R)" AT LINE LIN COLUMN 5
       WITH FOREGROUND-COLOR Grey HIGHLIGHT
               END-IF
               GO TO BE40.
      *    *************************************************************
      *        *****   E X T E N D E D  C O D E   *****
      *    *************************************************************
    IF WS-EXT-STK = "Y"
        DISPLAY S07-EXT AT LINE LIN
    ELSE
      *    *************************************************************
      *    *****   A L P H A - N U M E R I C   C O D E  *****
      *    *************************************************************
    IF W10-STCK = "A"
        IF WS-USE-ITM = "Y"
     DISPLAY S07-ITM AT LINE LIN
        ELSE
           DISPLAY S07 AT LINE LIN.
            DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Black.
             MOVE SAL-QNT        TO W10-QUANT.
             DISPLAY S26-QNT AT LINE LIN.
             MOVE SAL-SELL       TO W10-SELL.
             DISPLAY S26-PRC AT LINE LIN.
             MOVE SAL-VAL        TO W100-EXT.
            DISPLAY W100-EXT AT LINE LIN COLUMN 70
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Black.
      *    *************************************************************
      *    ****    I S   T H E   S C R E E N   F U L L
      *    *************************************************************
       BE40.
           IF LIN < 41
               ADD 1             TO LIN WS-SALKEY WS-LINES
               GO TO BE35.
       BE45.
             EXIT.
       BE50.
      *    *************************************************************
      *    ****    D I S P L A Y   O P T I O N S
      *    *************************************************************
           IF WS-LINES < 11
               IF WS-FIRST = 1
           MOVE "'D'elete order - 'E'xit"
     TO WS-ERR-MES
              ELSE
           MOVE "'D'elete order - 'P'revious screen - 'E'xit"
     TO WS-ERR-MES
           MOVE 2  TO WS-LINES
           ELSE
           IF WS-SALKEY < WS-LAST
               IF WS-FIRST = 1
           MOVE  "'D'elete order - 'N'ext screen - 'E'xit"
     TO WS-ERR-MES
           MOVE 1  TO WS-LINES
               ELSE
           MOVE  "'D'elete ord, 'N'ext,'P'revious screen,'E'xit"
     TO WS-ERR-MES
           MOVE 3  TO WS-LINES
           ELSE
               MOVE 2            TO WS-LINES
              MOVE "'D'elete order - 'P'revious screen - 'E'xit"
     TO WS-ERR-MES.
       BE55.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE
       UNTIL WS-OPTION = "D" OR "E" OR "N" OR "P".
           IF WS-OPTION = "E"
               GO TO BE05.
           IF WS-OPTION = "D"
               GO TO BE65.
      *    *************************************************************
      *    ****    I S   P R E V I O U S   S C R E E N   R E Q U I R E D
      *    *************************************************************
           IF WS-LINES = 2 OR 3
               IF WS-OPTION = "P"
                   PERFORM BE60
                   SUBTRACT 31   FROM WS-FIRST
                   GO TO BE30.
      *    *************************************************************
      *    ****    I S   N E X T   S C R E E N   R E Q U I R E D
      *    *************************************************************
           IF WS-LINES = 1 OR 3
               IF WS-OPTION = "N"
                   PERFORM BE60
                   ADD 31        TO WS-FIRST
                   GO TO BE30.
             GO TO BE55.
       BE60.
      *    *************************************************************
      *    ****    C L E A R   S C R E E N
      *    *************************************************************
             MOVE 11              TO LIN.
             PERFORM BE115 UNTIL LIN > 18.
             MOVE 11              TO LIN.
             MOVE 1              TO WS-LINES.
      *    *************************************************************
      *    ****    D E L E T E   R E C O R D S   F R O M   O R D E R
      *            F I L E
      *    *************************************************************
       BE65.
            DISPLAY "Deleting Order" AT 2520 WITH
             BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
             MOVE ZERO           TO WS-SALKEY.
             PERFORM AY60 THRU AY999.
       BE70.
      *    *************************************************************
      *    ****    U P D A T E   O R D E R   F I L E
      *    *************************************************************
             MOVE W90-REF        TO SOR-REF.
             MOVE ZERO           TO SOR-SQN.
             PERFORM START-AT-ORDER-NO THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
               GO TO BE80.
       BE75.
            PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
               GO TO BE80.
           IF SOR-REF NOT = W90-REF
               GO TO BE80.
    IF SOR-TYPE = 1 OR 2
       PERFORM BE90 THRU BE95.
             MOVE 2              TO WS-ACTION.
             PERFORM AY17 THRU AY59.
            PERFORM DELETE-SORDER-REC THRU WRITE-SORDER-EXIT.
             GO TO BE75.
       BE80.
             PERFORM AY70 THRU AY999.
             CLOSE SORDER
                   SALE.
             OPEN I-O SORDER.
             OPEN OUTPUT SALE.
             CLOSE SALE.
             OPEN I-O SALE.
            GO TO BE05.

       BE90.
    IF SOR-EXT-ITEM = SPACES
        GO TO BE95.
      MOVE SOR-EXT-ITEM  TO STK-CODE.
      MOVE 0   TO WS-ACTION.
      PERFORM AY05 THRU AY59.
      ADD SOR-QNT  TO STK-QUANT.
      SUBTRACT SOR-QNT  FROM STK-QPIC.
            PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.

       BE95.
      EXIT.

       BE115.
            DISPLAY WS-INV1 AT LINE LIN COLUMN 1
       WITH BACKGROUND-COLOR Black FOREGROUND-COLOR Cyan
       WS-INV78 WITH FOREGROUND-COLOR Cyan
       WS-INV80 WITH BACKGROUND-COLOR Black
       FOREGROUND-COLOR Cyan.
            ADD 1   TO LIN.

       BE599.
             EXIT.
      *    *************************************************************
      *    ****    C A L L   D E B T O R   C R E A T I O N
      *    *************************************************************
       GA000        SECTION 53.
       GA00.
      PERFORM SAVE-SCREEN.
            CALL "DTP\DTP001"
     USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
      CANCEL "DTP\DTP001".
      PERFORM RESTORE-SCREEN.
       GA99.
             EXIT.
      /    *************************************************************
      *    ****  S A L E S   R E S T R I C T I O N S     ****
      *    *************************************************************
       MA000        SECTION 54.
       MA00.
      MOVE 1   TO WS-S3.
      MOVE "N"   TO WS-RESTCODE.

       MA05.
    IF STK-LDG = DBD-DEPT-CDE(WS-S3)
        MOVE "Y"   TO WS-RESTCODE
    ELSE
    IF WS-S3 < 10
        ADD 1   TO WS-S3
        GO TO MA05.

       MA999.
      EXIT.
      /    *************************************************************
      *   I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT        SECTION 90.
       ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
             MOVE LS-PARID           TO WS-PARID.
      *      MOVE LS-L-OR-N          TO W02-L-OR-N.
      *      MOVE WS-SYS-ID          TO W02-SYSID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY.
             MOVE LS-USUB            TO WS-USUB.
             PERFORM ERASE-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".
       ZA00A-CONTINUE.
            MOVE 3   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
    IF NOT(PAR-STOCK = "A" OR "N")
        MOVE "A"   TO PAR-STOCK.
      MOVE PAR-STOCK  TO W10-STCK.
    IF PAR-STOCK = "N"
        MOVE PAR-SLNGTH  TO W10-SLNGTH
    ELSE
        MOVE 14   TO W10-SLNGTH.
    IF W10-SLNGTH < 3
        MOVE 3   TO W10-SLNGTH
    ELSE
    IF W10-SLNGTH > 14
        MOVE 14   TO W10-SLNGTH.
      MOVE PAR-INV  TO PRG-NUMBER.
      MOVE PAR-PACKS  TO WS-USE-PACKS.
      MOVE PAR-USE-ITM  TO WS-USE-ITM.
      MOVE PAR-EXT-STK  TO WS-EXT-STK.
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
    IF TRM-PICP(WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
        MOVE ZERO  TO WS-PRN-NO
           ELSE
    IF TRM-PICP(WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
        MOVE 1   TO WS-PRN-NO
           ELSE
    IF TRM-PICP(WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
        MOVE 2   TO WS-PRN-NO
           ELSE
    IF TRM-PICP(WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
    IF TRM-PICP(WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
    IF TRM-PICP(WS-S6) = 9
        MOVE LS-USER  TO W02-USER.
      ADD 501 TRM-PRN1(WS-S6)
                                 GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S6.
             ADD 1               TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE TRM-CONP(WS-S6)
                                 TO WS-COND.
      MOVE TRM-NORP(WS-S6)
                                 TO WS-NORM.
      MOVE TRM-EXPP(WS-S6)
     TO WS-EXPP.
      MOVE TRM-ECAN(WS-S6)
                                 TO WS-ECAN.
      MOVE TRM-8LPI(WS-S6)
                                 TO WS-8LPI.
      MOVE TRM-6LPI(WS-S6)
                                 TO WS-6LPI.
      MOVE TRM-10CPI(WS-S6)
                                 TO WS-10CPI.
      MOVE TRM-12CPI(WS-S6)
                                 TO WS-12CPI.
      MOVE TRM-17CPI(WS-S6)
                                 TO WS-17CPI.
      MOVE TRM-LENGTH(WS-S6)
     TO WS-PRN-LENGTH.
      MOVE TRM-PAGE(WS-S6)
     TO WS-PGE-LENGTH.

       ZA01B.
             MOVE PAR-VAT-REG    TO W05-VATNO.
      OPEN I-O DEBDEP.
          IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
        IF IDX-CORRUPT
     MOVE 33  TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
              MOVE 33   TO WS-F-ERROR
        IF WS-STATUS = "39"
           GO TO ZA50
        ELSE
        IF NOT (WS-STATUS = "41")
           PERFORM OPEN-ERROR.
            OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
            OPEN OUTPUT SALE.
             CLOSE SALE.
             OPEN I-O SALE.
            MOVE 5   TO WS-PARKEY.
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
      MOVE PAR-PIC-PRC  TO WS-PIC-PRC.
      MOVE PAR-DELIV  TO WS-DELIV.
      MOVE PAR-PS-PRC  TO WS-ITMPRC.
            MOVE 8   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      INSPECT WS-CS-HD REPLACING LEADING "-" BY " ".
      INSPECT WS-WS-HD REPLACING LEADING "-" BY " ".
      INSPECT WS-RT-HD REPLACING LEADING "-" BY " ".
      MOVE WS-CS-HD  TO W15-PNAME.

       ZA05-NAME-LOOP.
    IF W15-CHECK = SPACE
        MOVE W15-REST  TO W15-PNAME
        GO TO ZA05-NAME-LOOP.

       ZA05-EXIT.
      MOVE W15-PNAME  TO WS-CS-HD.
      MOVE WS-WS-HD  TO W15-PNAME.
      PERFORM ZA05-NAME-LOOP.
      MOVE W15-PNAME  TO WS-WS-HD.
      MOVE WS-RT-HD  TO W15-PNAME.
      PERFORM ZA05-NAME-LOOP.
      MOVE W15-PNAME  TO WS-RT-HD.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
             MOVE PAR-COMPANY    TO W95-COMP.
             MOVE PAR-TELEPHONE  TO W95-TEL.
            ADD 1   TO WS-PARKEY.
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
             MOVE 7              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-CASH-AC    TO WS-CASH-AC.
    IF WS-PRN-NO < 3
        PERFORM ZA210 THRU ZA215
        IF WS-ERROR = 1
     GO TO ZA205
    ELSE
        DISPLAY "Waiting for printer response" AT 1412
   WITH FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
            OPEN OUTPUT PRNREP.
      CALL X"91" USING X91-RES X91-FUN PRNREP.
      MOVE WS-10CPI  TO PRN-DETAIL2.
      WRITE PRN-L1 BEFORE 0 LINES.
      MOVE SPACES  TO PRN-DETAIL2.
            MOVE 1   TO WS-NETKEY.
            PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE NET-DEBTOR     TO W70-DEBT.
      *    *************************************************************
      *    ****   A R E   S E P E R A T E   O R D E R N U M B E R S
      *    P E R   U S E R   B E I N G U S E D ?
      *    *************************************************************
    IF W70-ORD-SYS = "Y"
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³        Get the hard drive ID         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
        CALL "PC_READ_DRIVE" USING WS-LOCAL-DRIVE
        RETURNING WS-STATUS
        CALL "CBL_TOUPPER" USING WS-LOCAL-DRIVE
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the local drive is being used change the order file-id ³
      *    ³ to SORD??? (??? = USER NO)           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
        IF LS-DSKDRV = "L"
      *    *************************************************************
      *    As local drives are not always C, the following test has been
      *    replaced with the instruction above which will be more reli-
      *    able if the workstation happens to have apac on a local drive
      *    other than C.
      *    *************************************************************
      *        IF WS-LOCAL-DRIVE = "C"
     CLOSE SORDER
     MOVE W02-SORDER
     TO WS-SORDER-STR
     MOVE LS0-NO  TO WS-USER-ID
     MOVE WS-LOCAL-ID
     TO W02-SORDER
     MOVE WS-LOCAL-DRIVE
     TO WS-LOCAL-DRIVE2
     MOVE WS-LOCAL-ID2
     TO W02-SYSUSER
     OPEN I-O SORDER
     OPEN I-O SYSUSER.
            GO TO ZA999.

       COPY "ZA49.PRO".

       ZA50.
          IF WS-F-ERROR = 33
              MOVE "DEBDEP file"
           TO WS-FILE
    END-IF.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Press any key to continue" AT 1012.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
       ZA51.
             EXIT PROGRAM.

      *    *************************************************************
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *    *************************************************************
       ZA55.
            MOVE 1   TO WS-S1.
            MOVE SPACES  TO WS-MD-LINE.

       ZA60.
            MOVE WS-G1   TO WS-TCA(WS-S1)   WS-BCA(WS-S1).
          IF WS-S1 < 80
               ADD 1 TO WS-S1
               GO TO ZA60.
            MOVE WS-G14  TO WS-TCA(1)  WS-BCA(1).
            MOVE WS-G13  TO WS-TCA(80) WS-BCA(80).
            MOVE WS-G2   TO WS-TCA(10) WS-TCA(17)
              WS-TCA(47) WS-TCA(56)
              WS-TCA(59) WS-TCA(68).
            MOVE WS-G3   TO WS-MCA(1)  WS-MCA(80)
              WS-MCA(10) WS-MCA(17)
              WS-MCA(45) WS-MCA(56)
              WS-MCA(59) WS-MCA(68).
            MOVE WS-G4   TO WS-BCA(10) WS-BCA(17)
              WS-BCA(47) WS-BCA(56)
              WS-BCA(59) WS-BCA(68).
            MOVE LS-COMPANY  TO WS-TOP-COMP.
    IF LS-USER = LS-SYS-ID
        MOVE "SupervisorÄ"  TO WS-WRKHD
    ELSE
        MOVE "Workstation"  TO WS-WRKHD
        MOVE LS-USER    TO WS-WRKST.

       ZA200.

       COPY "LOCKED.PRO".

       ZA205.
             EXIT PROGRAM.

       ZA210.
      MOVE ZERO   TO WS-ERROR
        WS-S1 WS-S2 WS-S3 WS-S4
        WS-S5 WS-S6 WS-S7 WS-S8.
      CALL "PC_TEST_PRINTER" USING WS-PRN-NO
       WS-PRN-STAT
        RETURNING WS-STATUS.
    IF WS-PRN-STAT > 127
        MOVE 1   TO WS-S1
        SUBTRACT 128  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 63
        MOVE 1   TO WS-S2
        SUBTRACT 64  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 31
        MOVE 1   TO WS-S3
        SUBTRACT 32  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 15
        MOVE 1   TO WS-S4
        SUBTRACT 16  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 7
        MOVE 1   TO WS-S5
        SUBTRACT 8  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 3
        MOVE 1   TO WS-S6
        SUBTRACT 4  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 1
        MOVE 1   TO WS-S7
        SUBTRACT 2  FROM WS-PRN-STAT.
      MOVE WS-PRN-STAT  TO WS-S8.
    IF WS-S3 = 0 AND
       WS-S5 = 0 AND
       WS-S8 = 0
        GO TO ZA215.
      DISPLAY "Check printer - " AT 2304
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "R" WITH FOREGROUND-COLOR Grey HIGHLIGHT
       "etry - "
       "A" WITH FOREGROUND-COLOR Grey HIGHLIGHT
       "bort".
       ZA212.
      PERFORM TEST AFTER
       UNTIL WS-OPTION = "A" OR "R"
        ACCEPT WS-OPTION AT 2334
        WITH FOREGROUND-COLOR Grey HIGHLIGHT AUTO
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
      END-PERFORM.
    IF WS-OPTION = "R"
        GO TO ZA210.
      MOVE 1   TO WS-ERROR.

       ZA215.
      EXIT.

       ZA999.
             EXIT.

       I-O-ERRORS      SECTION  91.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 2          MOVE W02-NETWORK  TO WS-FILE
                               MOVE WS-NETKEY    TO WS-KEY
               WHEN 6          MOVE W02-DEBTOR   TO WS-FILE
                               MOVE DEB-ACNO     TO WS-KEYX
               WHEN 7          MOVE W02-DEPART   TO WS-FILE
                               MOVE DPT-CODE     TO WS-KEYX
               WHEN 15         MOVE WS-PARID     TO WS-FILE
                               MOVE WS-PARKEY    TO WS-KEY
               WHEN 18         MOVE W02-RECOVER  TO WS-FILE
                               MOVE WS-RECKEY    TO WS-KEY
               WHEN 19         MOVE W02-SALE     TO WS-FILE
                               MOVE WS-SALKEY    TO WS-KEY
               WHEN 20         MOVE W02-SORDER   TO WS-FILE
                               MOVE ZERO         TO WS-KEY
               WHEN 21         MOVE W02-SPARTS   TO WS-FILE
                               MOVE PRT-EXT-ITEM TO WS-KEYX
               WHEN 22         MOVE W02-STOCKF   TO WS-FILE
                               MOVE STK-CODE     TO WS-KEYX
               WHEN 25         MOVE W02-STKMEM   TO WS-FILE
                               MOVE SME-KEY      TO WS-KEYX
               WHEN 33         MOVE W02-DEBDEP   TO WS-FILE
                               MOVE DBD-ACNO     TO WS-KEYX
               WHEN 37         MOVE W02-SHARED   TO WS-FILE
                               MOVE WS-SHARED    TO WS-KEY
               WHEN 47         MOVE W02-DEBALT   TO WS-FILE
                               MOVE DAL-KEY      TO WS-KEYX
               WHEN 64         MOVE W02-SYSUSER  TO WS-FILE
                               MOVE USE-NO       TO WS-KEY
               WHEN OTHER      MOVE "ERROR"      TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
