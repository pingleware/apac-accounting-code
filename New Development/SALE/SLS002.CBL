      $set LINKCOUNT"596" GNT"SLS002.GNT"
      ******************************************************************
      *                                                                *
      *    ******   **         ******      ****      ****     ******   *
      *   **    **  **        **    **    **  **    **  **   **    **  *
      *   **        **        **         **    **  **    **       **   *
      *    ******   **         ******    **    **  **    **     **     *
      *         **  **              **   **    **  **    **   **       *
      *   **    **  **        **    **    **  **    **  **   **        *
      *    ******   ********   ******      ****      ****    ********  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *      P R I N T   P I C K I N G   S L I P   P R O G R A M       *
      *                                                                *
      *       Version 9.04.05 - June 2018                              *
      *                                                                *
      ******************************************************************
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
       PROGRAM-ID.     SLS002.
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

       COPY "DEBTOR.SL".

       COPY "DEPART.SL".

       COPY "PARAM.SL".

       COPY "SALE.SL".

       COPY "SHARED.SL".

       COPY "SORDER.SL".

       COPY "SPARTS.SL".

       COPY "STOCK.SL".

           SELECT PRNDSK  ASSIGN W02-PRNDISK
                          ORGANIZATION LINE SEQUENTIAL.

           SELECT PRNREP  ASSIGN W02-PRINTER
                          ORGANIZATION LINE SEQUENTIAL.
      /
       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "DEBTOR.FDE".

       COPY "DEPART.FDE".

       COPY "PARAM.FDE".

       COPY "SALE.FD".

       COPY "SHARED.FDE".

       COPY "SORDER.FDE".

       COPY "SPARTS.FDE".

       COPY "STOCK.FDE".

       FD  PRNDSK    LABEL RECORD OMITTED
             LINAGE IS WS-PGE-LENGTH.
       01  REP-L1.
    03  REP-PG.
        05  REP-LN      PIC  9(04).
          03  REP-DETAIL2     PIC  X(96).

       01  REP-L2.
    03  FILLER        PIC  X(04).
          03  REP-DETAIL1     PIC  X(80).

       01  REP-L3.
    03  FILLER        PIC  X(04).
          03  REP-AC        PIC  X(10).
          03  REP-ACNO        PIC  X(06).
    03  FILLER        PIC  X(01).
          03  REP-NAME        PIC  X(40).
    03  FILLER        PIC  X(06).
          03  REP-DATE        PIC  X(06).
           03  REP-TIME.
              05  REP-DTE     PIC  Z9/99/9999.

       01  REP-L3A.
    03  FILLER        PIC  X(04).
    03  FILLER        PIC  X(10).
    03  REP-TEL        PIC  X(14).
    03  REP-CNTCT       PIC  X(10).
    03  REP-CONTACT     PIC  X(30).
    03  FILLER        PIC  X(32).

       01  REP-L4.
    03  FILLER        PIC  X(04).
          03  REP-H1        PIC  X(29).
          03  REP-REF        PIC  9(08)/99.
          03  FILLER        PIC  X(40).

       01  REP-L5.
    03  FILLER        PIC  X(04).
          03  REP-H2        PIC  X(20).
          03  REP-H3        PIC  X(32).
          03  REP-H4        PIC  X(12).
          03  REP-H5        PIC  X(16).

       01  REP-L6.
    03  FILLER        PIC  X(04).
    03  REP-EXT-ITEM.
              05  REP-ITM1    PIC  X(14).
        05  REP-SL      PIC  X(01).
        05  REP-ITM     PIC  X(03).
    03  FILLER        PIC  X(02).
          03  REP-DESC        PIC  X(30).
    03  FILLER        PIC  X(02).
    03  REP-QTY        PIC  Z(05)9.999 BLANK WHEN ZERO.
    03  FILLER        PIC  X(02).
    03  REP-PRICE       PIC  Z(06)9.999.
    03  FILLER        PIC  X(05).

       01  REP-L7.
    03  FILLER        PIC  X(24).
    03  REP-EXT-ITEM2.
              05  REP-ITM2    PIC  X(14).
        05  REP-SL2     PIC  X(01).
        05  REP-ITM3    PIC  X(03).
          03  FILLER        PIC  X(01).
    03  REP-DSCR        PIC  X(30).
          03  FILLER        PIC  X(01).
          03  REP-QNT        PIC  Z(05)9.999.
    03  FILLER        PIC  X(12).

       FD  PRNREP    LABEL RECORD OMITTED
             LINAGE IS WS-PGE-LENGTH.
       01  PRN-L1.
           03  PRN-DETAIL2     PIC X(96).
       01  PRN-L2.
           03  PRN-DETAIL1     PIC X(80).
      /
       WORKING-STORAGE SECTION.
       77  WS-CHECK                PIC  X(18)    VALUE "aeWlimemnomLalismJ".
       77  WS-DISC                 PIC 99V99  COMP-3.
       77  WS-TDISC                PIC  99V99    COMP-3.
       77  WS-CASH-AC              PIC  X(06).
       77  WS-SUB                  PIC  9(04)    COMP-5.
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
       77  WS-KEYSTR               PIC 9(06)  COMP-5.
       77  WS-SALSTR               PIC  9(04)    COMP-5.
       77  WS-SALKEY               PIC  9(04)    COMP-5.
       77  WS-NETKEY               PIC  9(04)    COMP-5.
       77  WS-AUDKEY               PIC  9(06)    COMP-5.
       77  WS-RECKEY               PIC  9(06)    COMP-5.
       77  WS-RECOVER              PIC  9(06)    COMP-5.
       77  WS-SHARED               PIC 9(04)  COMP-5 VALUE 1.
       77  WS-TRANS                PIC  9(06)    COMP-5 VALUE 1.
       77  WS-STOCK                PIC X(01).
       77  WS-PRN1                 PIC 9(01)    VALUE 0.
       77  WS-PRN2                 PIC  9(01)    VALUE 0.
       77  WS-USE-PACKS            PIC X(01).
       77  WS-USE-ITM              PIC X(01).
       77  WS-EXT-STK              PIC X(01).
       77  WS-PIC-PRC              PIC X(01).
       77  WS-START                PIC 9(08).
       77  WS-END                  PIC  9(08).
       77  WS-TME2                 PIC  9(08).
       77  WS-PORT                 PIC  9(05)    VALUE 1004.
       77  WS-OPEN                 PIC  X(01)    VALUE X"01".
       77  WS-CLOSE                PIC  X(01)    VALUE X"00".
       77  WS-PAR                  PIC  9(01)    VALUE 0.
       77  WS-INO                  PIC 9(03)    VALUE 0.
       77  WS-LINES                PIC  9(02)    VALUE 0.
       77  WS-FIRST                PIC 9(03)    VALUE 0.
       77  WS-LAST                 PIC 9(03)    VALUE 0.
       77  WS-LASTINV              PIC  9(04)    COMP-5.
       77  WS-LINE                 PIC  9(04)    COMP-5.
       77  WS-PAGE                 PIC  9(02)    COMP-5.
       77  WS-L1                   PIC  9(04)    COMP-5.
       77  WS-PGE-LENGTH           PIC 9(02)  VALUE 66.
       77  WS-PRN-LENGTH           PIC 9(02)  VALUE 62.
       77  WS-COND                 PIC X(06).
       77  WS-NORM                 PIC X(06).
       77  WS-EXPP                 PIC X(06).
       77  WS-ECAN                 PIC X(06).
       77  WS-8LPI                 PIC X(06).
       77  WS-6LPI                 PIC X(06).
       77  WS-10CPI                PIC X(06).
       77  WS-12CPI                PIC X(06).
       77  WS-17CPI                PIC X(06).
       77  WS-OPTION               PIC X(01).
       77  WS-PGADV                PIC X(01)    VALUE "Y".
       77  WS-SKIP                 PIC  X(01)    VALUE " ".
       77  WS-LOOK                 PIC  X(01)    VALUE "S".
       77  WS-ETYPE                PIC  X(01)    VALUE "I".
       77  WS-ERROR                PIC  9(01).
       77  WS-PRN-NO               PIC X(01)  COMP-X VALUE 9.
       77  WS-PRN-STAT             PIC X(01)  COMP-X.
       77  WS-CHAR1                PIC X(01).
       77  WS-CHAR2                PIC  X(01).
       77  WS-COMP                 PIC  9(01).
       77  WS-UPDATE               PIC  9(01)    VALUE ZERO.
       77  WS-CONREC               PIC  9(01)    VALUE ZERO.
       77  WS-DELIV                PIC X(01).
       77  WS-ITMPRC               PIC X(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-AGE                  PIC  X(01).
       77  WS-PRC                  PIC  X(01).
       77  WS-BATCH                PIC 9(05).
       77  WS-ERR2                 PIC X(09)    VALUE "No Record".
       77  WS-PER                  PIC  Z(02)9.99.
       77  WS-CR                   PIC  X(01).
       77  WS-TAXRES               PIC  S9(07)V9(08) COMP-3.
       77  TODAY-DDMMYY            PIC 9(08)    COMP-5.
       77  WS-USUB                 PIC 9(04)    COMP-5.
      /

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *      C = Clear comment line (50)
      *      E = Clear lines (any line or lines between 5 and 46)
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "SLS002".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "SALES - PRINT A PICKING SLIPS".
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
                  07  WS-SRT-LINE  PIC  X(50).
                  07  FILLER       PIC  X(10).
               05  FILLER          PIC  X(19).
           03  WS-MM-LINE.
               05  FILLER          PIC  X(01) VALUE "³".
               05  WS-SPACE        PIC  X(60) VALUE SPACES.
               05  FILLER          PIC  X(01) VALUE "³".
           03  WS-HME-LINE         PIC X(60) VALUE ALL "Í".
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
           03  WS-ENT0     PIC  X(08).
           03  WS-TYPE0    PIC  X(01).
           03  WS-KEY0     PIC  9(06)    COMP-5.

       01  WS-ENTRY1.
           03  WS-ENT1     PIC  X(08).
           03  WS-TYPE1    PIC  X(01).
           03  WS-KEY1     PIC  9(06)    COMP-5.

       COPY "WS.WS".

       01  WS-HELP.
    03  WS-MODULE       PIC  X(03) VALUE "SLS".
    03  WS-PROG        PIC  X(03) VALUE "002".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  WS-LOCAL-ID.
    03  WS-LOCAL-DRIVE  PIC  X(01).
    03  FILLER        PIC  X(14) VALUE ":\APACSHR\SORD".
    03  WS-USER-ID      PIC  X(03).
    03  FILLER        PIC  X(04) VALUE ".ORD".

       01  WS-SORDER-STR.
    03  FILLER        PIC  X(64).

       01  W00-TRAN-CODES.
           03  W00-T-VAT   PIC S9(09)V99 COMP-3.
           03  W00-E-DESC  PIC  X(12).
           03  W00-A-DESC  PIC  X(12).
           03  W00-ACTION  PIC  9(01).
           03  W00-T-CODE  PIC  9(02)    COMP-5.
           03  W00-T-GL    PIC  9(06)    COMP-3.
           03  W00-T-DAY   PIC  9(06)    COMP-5.
           03  W00-T-VAL   PIC S9(09)V99 COMP-3.
           03  W00-T-MTD   PIC  9(06)    COMP-5.
           03  W00-T-MTDV  PIC S9(09)V99 COMP-3.
           03  W00-T-YTD   PIC  9(06)    COMP-5.
           03  W00-T-YTDV  PIC S9(09)V99 COMP-3.

       01  W02-FID.

       COPY "PRNDISK.ID".

       01  W02-FILE-IDS.
           03  W02-PRINTER.
               05  W02-REPORT PIC X(07) VALUE "SLS002.".
               05  W02-USER   PIC X(05) VALUE SPACES.

       COPY "W05.VAT".

       COPY "W10.SAL".

       01  W11-PARTS.
           03  W11-PART-DETAILS OCCURS 30.
               05  W11-CODE   PIC X(14).
               05  W11-DESC   PIC X(30).
               05  W11-QUANT  PIC S9(05)V9999 COMP-3.

       COPY "W12.WS".

       01  W14-ORDER.
           03  W14-REF         PIC Z(08).
           03  W14-AC          PIC X(06).
           03  W14-NAME.
        05  W14-NME     PIC X(27).
        05  FILLER      PIC X(13).
    03  W14-DATE        PIC Z9/99/9999.
           03  W14-PS          PIC 9(02).
           03  W14-DOC         PIC X(01).
           03  W14-DDOC.
               05  W14-INV     PIC Z(08).
           03  W14-VALUE       PIC Z(07)9.99.

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
           03  W25-TIME        PIC 9(08).
           03  W25-TRED REDEFINES  W25-TIME.
               05  W25-HH      PIC 9(02).
               05  W25-MM      PIC 9(02).
               05  W25-SS      PIC 9(02).
               05  FILLER      PIC 9(02).

       01  W30-MESSAGE.
           03  W30-PERC        PIC Z9.99.
           03  FILLER          PIC  X(40) VALUE
               "% Settlement discount if paid in 30 DAYS".

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

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-S2         PIC 9(02) COMP-5.
           03  W75-DNO        PIC X(06) OCCURS 18.
    03  W75-SORD       PIC 9(08) OCCURS 18.

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

       COPY "W90.SAL".

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
           03  W95-TIME.
               05  W95-HH      PIC 99.
               05  FILLER      PIC X(01) VALUE ":".
               05  W95-MM      PIC 99.
               05  FILLER      PIC X(01) VALUE ":".
               05  W95-SS      PIC 99.

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
           03  W100-V2.
               05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V4 REDEFINES W100-V2.
               05  W100-EXT    PIC Z(06)9.99.

       LINKAGE SECTION.

       COPY "CHAIN.LS".

      /
       SCREEN SECTION.

       COPY "BLANK.CRT".

       01  S02.
          03  LINE 2  COLUMN 27 FOREGROUND-COLOR 7 HIGHLIGHT
                                 VALUE "SALES/ORDER PROGRAM CONTROL".

       COPY "SORLUP.CRT".

       01  S07.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
     PIC X(14) USING W10-ITEM AUTO.

       01  S07-ITM.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC X(14) USING W10-ITEM AUTO.
    03        COLUMN 17 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC X(03) USING W10-ITM AUTO.

       01  S07-EXT.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC X(18) USING W10-EXT-ITEM AUTO.

       01  S26-QNT.
          03        COLUMN 51 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
                                 PIC Z(04)9.99 USING W10-QUANT AUTO.

       01  S26-PRC.
          03        COLUMN 60 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
                                 PIC Z(05)9.99 USING W10-SELL AUTO.

       01  S26-DSC.
          03        COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z9.99 USING WS-DISC AUTO.

       01  S26-TDSC.
          03        COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
                                 PIC Z9.99 USING WS-TDISC AUTO.

       01  S26-AVGE.
          03        COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
           PIC Z(04)9.999 USING STK-AVGE AUTO.

       01  S28.
          03  LINE 25 COLUMN 25 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC ZZ9 USING WS-SALKEY AUTO.
       01  S30.
          03  LINE  2 COLUMN 35 FOREGROUND-COLOR 7 HIGHLIGHT
                                 VALUE "PICKING SLIP".
           03  LINE  3 COLUMN  2 VALUE "Ac No".
           03          COLUMN 15 VALUE "Name".
           03          COLUMN 61 VALUE "Ord No             ".
          03  LINE  4 COLUMN  2 VALUE "Assisted".
           03          COLUMN 61 VALUE "Tel.".
           03  LINE  5 COLUMN  2 VALUE "Ref. No".
           03          COLUMN 20 VALUE "Contact".
          03        COLUMN 65 VALUE "Date".
          03  LINE  6 COLUMN  2 REVERSE-VIDEO VALUE "Item Code      I".
          03        COLUMN 18 REVERSE-VIDEO VALUE "TM Description  ".
           03          COLUMN 33 REVERSE-VIDEO VALUE "                ".
           03          COLUMN 49 REVERSE-VIDEO VALUE "  Quantity ".
           03          COLUMN 60 REVERSE-VIDEO VALUE "Unit Price  ".
           03          COLUMN 72 REVERSE-VIDEO VALUE "Extended".
          03  LINE  7 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
          03  LINE 19 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ

      -         "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
          03  LINE 20 COLUMN  3 VALUE "Sub Total".
           03  LINE 21 COLUMN  3 VALUE "Discount".
           03  LINE 22 COLUMN  3 VALUE "T A X -VAT".
           03  LINE 23 COLUMN  3 VALUE "AMOUNT DUE".

       01  S30A.
    03  FOREGROUND-COLOR 3 HIGHLIGHT
      PIC Z(08) USING W10-SORD AUTO.

       01  S36.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC X(06) USING W80-ACNO AUTO.

       COPY "ERROR.CRT".

       COPY "OPT.CRT".

      /
       PROCEDURE DIVISION
   USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000-MAIN        SECTION.
       AA000-INIT.
          IF LS0-SALEV < 1
        MOVE "Not Authorised"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO AA49.
            PERFORM ZA000-INIT.
            GO TO BA00.

       AA49.
      EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
       AA50O.
             MOVE "O"            TO WS-LOOK.
       AA50.
            PERFORM SAVE-SCREEN.
             MOVE 4              TO W44-FUNCTION.
            PERFORM SCREEN-CONTENTS.
      PERFORM SORDER-LOOKUP.
            PERFORM RESTORE-SCREEN.
       AA99.
             EXIT.

       COPY "FUNCTION.SCR".

       COPY "ERROR.SCR".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     OPT-MESSAGE          ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine is used to allow the OPERATOR to respond to  ³
      *    ³ a request for an option, so that the correct action can   ³
      *    ³ be performed by the program. The routine will display the ³
      *    ³ message in a pop-up window and allow the OPERATOR to      ³
      *    ³ respond to the 'question'.           ³
      *    ³              ³
      *    ³ The option request must be placed in WS-ERR-MES and may   ³
      *    ³ not exceed 48 characters. The message will be centred in  ³
      *    ³ the window. An example of a message request follows:      ³
      *    ³              ³
      *    ³   MOVE "Print transactions (Y/N) [ ]" TO WS-ERR-MES.      ³
      *    ³   PERFORM OPT-MESSAGE (Use this format if function or any ³
      *    ³        other keys are to be tested on exit)        ³
      *    ³        OR           ³
      *    ³   PERFORM OPT-MESSAGE UNTIL WS-OPTION = "N" OR "Y".       ³
      *    ³   (Use TEST AFTER if WS-OPTION is preloaded with a        ³
      *    ³   default option)            ³
      *    ³              ³
      *    ³ This would be displayed as:          ³
      *    ³ ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿     ³
      *    ³ ³    Print transactions (Y/N) [ ]   ³°°   ³
      *    ³ ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°   ³
      *    ³   °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°   ³
      *    ³              ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³              ³
      *    ³ To display the request message higher or lower (default   ³
      *    ³ is line 13) move the line number to be used to SLIN and   ³
      *    ³ then PERFORM OPT-SETUP THRU OPT-EXIT.         ³
      *    ³ (The UNTIL and TEST AFTER options also apply here)        ³
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
      *    ³     MOVE 8              TO CRT-START.                     ³
      *    ³     MOVE 28             TO CRT-END.                       ³
      *    ³     PERFORM ERASE-SCREEN.                                 ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³             CLEAR MESSAGE LINE (Line 50)                  ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³                                                           ³
      *    ³     PERFORM CLEAR-L50                                     ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³             CHANGE THE TIME ON THE SCREEN                 ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³                                                           ³
      *    ³     PERFORM CHANGE-TIME.                                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "CLEAR.CRT".

      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S
      *
       AA400              SECTION.
       AA401.
            DISPLAY "F1" AT 2502
        WITH BACKGROUND-COLOR 3
      FOREGROUND-COLOR 6 HIGHLIGHT
       "=Help,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F2" WITH BACKGROUND-COLOR 3
          FOREGROUND-COLOR 6 HIGHLIGHT
             " Order no./ "
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "F3" WITH BACKGROUND-COLOR 3
          FOREGROUND-COLOR 6 HIGHLIGHT
             " Account no./ "
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F4" WITH BACKGROUND-COLOR 3
          FOREGROUND-COLOR 6 HIGHLIGHT
                     " From specified date Lookup"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
       AA405.
             EXIT.
      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³        R E A D  F I L E S   R O U T I N E S        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AC000    SECTION.

       COPY "CONTROL.RD".

       COPY "DEBTOR.RD".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "SALE.RD".

       COPY "SHARED.RD".

       COPY "SORDER.RD".

       COPY "SPARTS.RD".

       COPY "STOCK.RD".

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   & W R I T E   F I L E S R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AD000-WRITE        SECTION.

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

       COPY "SALE.WR".

       COPY "SORDER.WR".

       COPY "SORDER.LUP".

       COPY "APAC.HLP".

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY000           SECTION.
       AY60.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Read the 5th record in the SHARED file and lock it. This  ³
      *    ³ will cause all other programs that need to update the     ³
      *    ³ data base to wait until this record is unlocked.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
            MOVE 1   TO WS-COUNT.
            MOVE 5   TO WS-SHARED.
            PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
            GO TO AY999.
       AY70.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ End the transaction and unlock any locked records.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             COMMIT.
       AY999.
             EXIT.
      /
       AZ000-END               SECTION.
       AZ000-EOJ.
             CLOSE SALE
                   PRNREP.
    IF W70-ORD-SYS = "Y"
        IF WS-LOCAL-DRIVE = "C"
     CLOSE SORDER
     MOVE WS-SORDER-STR
     TO W02-SORDER
     OPEN I-O SORDER.

       AZ100.
             EXIT PROGRAM.

       COPY "CA000.STK".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     Read debtor record - using account number.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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

       COPY "CA200.STK".

       COPY "CA500.STK".

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
               MOVE SOR-NAME     TO W40-NAME
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
              MOVE SOR-EXT-ITEM TO SAL-EXT-ITEM
        MOVE SOR-DESC  TO SAL-DESC
               MOVE SOR-QNT      TO SAL-QNT
      *        MOVE SOR-QORD  TO SAL-ORD
               MOVE SOR-COST     TO SAL-COST
               MOVE SOR-SELL     TO SAL-SELL
               MOVE SOR-VAT      TO SAL-VAT
              MOVE SOR-VAT-SUB  TO SAL-VAT-SUB
               MOVE SOR-VAL      TO SAL-VAL
               MOVE SOR-INCTAX   TO SAL-INCTAX
               MOVE SOR-NONTAX   TO SAL-NONTAX
               MOVE SOR-TAXVAL   TO SAL-TAXVAL
               MOVE SOR-DSC      TO SAL-DSC
               MOVE SOR-DISC     TO SAL-DISC
        MOVE SOR-PACK  TO SAL-PACK
               MOVE SOR-SQN      TO SAL-SQN
        IF SAL-VAT-SUB > ZERO
     ADD SOR-TAXVAL
     TO W90-TAXVAL(SAL-VAT-SUB)
        END-IF
        PERFORM GET-SORD-WRITE-SALE
           ELSE
      *                      P A R T S   D E T A I L S
           IF SOR-TYPE = 2
               INITIALIZE SAL-RECORD1
              MOVE SOR-EXT-ITEM TO SAL-EXT-ITEM
        MOVE SOR-DESC  TO SAL-DESC
               MOVE SOR-SQN      TO SAL-SQN
    ELSE
      *                      R E M A R K S
           IF SOR-TYPE = 3 OR 4
               INITIALIZE SAL-RECORD1
               MOVE SOR-SERIAL   TO SAL-MSER
               MOVE SOR-SQN      TO SAL-SQN
               PERFORM GET-SORD-WRITE-SALE
           ELSE
      *                      A D D R E S S
           IF SOR-TYPE = 5
               MOVE SOR-AD1      TO W40-AD1
               MOVE SOR-AD2      TO W40-AD2
               MOVE SOR-AD3      TO W40-AD3
               MOVE SOR-AD4      TO W40-AD4
               MOVE SOR-PC       TO W40-PC
           ELSE
      *        S A L U T A T I O N
          IF SOR-TYPE = 7
        MOVE SOR-TEXT  TO W34-SALUTATION
    ELSE
      *        T E X T
          IF SOR-TYPE = 8
        MOVE SOR-TEXT  TO W32-REMARKS (WS-S7)
        ADD 1   TO WS-S7
    ELSE
      *        T R A I L E R
               MOVE SOR-SUBTOT   TO W90-DB
               MULTIPLY -1 BY SOR-DSCNT
                                 GIVING W90-TDISC
               MOVE W90-TDISC    TO W90-LDISC
               MOVE SOR-NON-TXBL TO W90-NONTAX
              MOVE SOR-TAXABLE  TO W90-EXCTOT
        MOVE SOR-TAX  TO W90-TTAX
               MOVE SOR-TOT      TO W90-TOTAL
               MOVE SOR-BAL      TO W90-BAL
        MOVE SOR-INSTRUCT TO W90-INSTRUCT
               MOVE SOR-PMETH    TO W90-PMETH
               MOVE SOR-STATUS   TO W40-STATUS
               MOVE SOR-HTEL     TO W40-HTEL
               MOVE SOR-ID       TO W40-ID
        GO TO GET-SORDER-EXIT.
             GO TO GET-SORD-NEXT.

       GET-SORD-WRITE-SALE.
             MOVE WS-LAST        TO SAL-CODE.
             PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
             ADD 1               TO WS-LAST WS-SALKEY.

       GET-SORDER-EXIT.
             EXIT.
      /
       BA000                    SECTION.
       BA00.
    IF LS-SLS-ORDNO NOT = SPACES
               PERFORM BD000
               GO TO AZ000-EOJ.
       BA05.
             PERFORM ERASE-SCREEN.
             DISPLAY S02.
            DISPLAY "P" AT 1212
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
                     "rint a picking slip or " WITH FOREGROUND-COLOR 7
       "E" WITH FOREGROUND-COLOR 6 HIGHLIGHT
                     "xit > <" WITH FOREGROUND-COLOR 7.
             MOVE " "            TO WS-OPTION.
       BA10.
      PERFORM TEST AFTER
       UNTIL WS-OPTION = "P" OR "E"
              ACCEPT WS-OPTION AT 1242
        WITH FOREGROUND-COLOR 7 HIGHLIGHT UPDATE AUTO
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
      END-PERFORM.
      *       E X I T   P R O G R A M
           IF WS-OPTION = "E"
               GO TO AZ000-EOJ.
      *                            P R I N T   P I C K I N G   S L I P
             PERFORM BC000
             GO TO BA05.
      /
       BB050               SECTION.
       BB50.
             MOVE WS-EXPP        TO REP-DETAIL2.
             MOVE ZERO           TO REP-LN.
             WRITE REP-L2 BEFORE 0 LINES.
             MOVE W95-COMP       TO REP-DETAIL2.
             MOVE 2              TO REP-LN.
             WRITE REP-L2 BEFORE 2 LINES.
             MOVE SPACES         TO REP-DETAIL2.
             MOVE "PICKING SLIP - REFERENCE NO:"
                                 TO REP-H1.
             MOVE W90-PS         TO REP-REF.
             MOVE ZERO           TO REP-LN.
             WRITE REP-L2 BEFORE 0 LINES.
             MOVE 2              TO REP-LN.
             WRITE REP-L2 BEFORE 2 LINES.
           IF W90-PICK > 01
               MOVE SPACES       TO REP-DETAIL2
               MOVE "REPLACE PREVIOUS PICKING SLIP"
                                 TO REP-H1
               MOVE 2            TO REP-LN
               WRITE REP-L2 BEFORE 2 LINES.

       BB60.
             MOVE WS-ECAN        TO REP-DETAIL2.
             MOVE ZERO           TO REP-LN.
             WRITE REP-L2 BEFORE 0 LINES.
             MOVE SPACES         TO REP-DETAIL2.
             MOVE "Account :"    TO REP-AC.
             MOVE W80-ACNO       TO REP-ACNO.
             MOVE W40-NAME       TO REP-NAME.
             MOVE "Date:"        TO REP-DATE.
             MOVE W12-TODAY      TO REP-DTE.
             MOVE 1              TO REP-LN.
             WRITE REP-L2 BEFORE 1 LINES.
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
            WRITE REP-L2 BEFORE 1 LINES.
            MOVE SPACES  TO REP-DETAIL2.
    IF WS-DELIV = "Y"
        IF NOT(DEB-PADDR = SPACES)
     MOVE DEB-PADDR
     TO W60-ADDRESS
     INSPECT W60-ADDRESS REPLACING ALL "*" BY "."
     MOVE 1  TO REP-LN
     MOVE W60-ADDRESS
     TO REP-DETAIL2
     WRITE REP-L2 BEFORE 1 LINES
     MOVE SPACES        TO REP-DETAIL2.
             MOVE ALL "="        TO REP-DETAIL1.
             MOVE 2              TO REP-LN.
             WRITE REP-L2 BEFORE 2 LINES.
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
             WRITE REP-L2 BEFORE 2 LINES.
             MOVE SPACES         TO REP-DETAIL2.
       BB65.
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
      /
      *    ****    P I C K I N G   S L I P S
      *
       BC000      SECTION 5.
       BC05.
      *
      *    ****    P I C K I N G   S L I P   S C R E E N
      *
             PERFORM ERASE-SCREEN.
             DISPLAY S30.
             MOVE 8              TO LIN.
             PERFORM BC115 UNTIL LIN > 18.
             MOVE W12-TODAY      TO W90-DTE.
            DISPLAY W90-DTE AT 0570
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
            MOVE ZERO   TO W90-BDT W90-LDISC W90-NONTAX
              W90-BAL W90-SALES W90-EXCTOT
              W90-CUR W90-PMETH W40-STATUS.
            MOVE 1   TO WS-FIRST WS-LAST.
            MOVE SPACES  TO W90-TAX W90-INSTRUCT.
      MOVE 1   TO WS-S9.

       BC08.
            MOVE ZERO   TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
    IF WS-S9 < 12
        ADD 1   TO WS-S9
        GO TO BC08.
      MOVE ZERO   TO W10-SORD.

       BC10.
             PERFORM AA400.
             ACCEPT S30A AT 0510.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                     MOVE ZERO   TO W10-SORD
                     GO TO BC599
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
                     GO TO BC10
               END-EVALUATE
              PERFORM AA50O THRU AA50
               DISPLAY S30A AT 0510
               IF W10-SORD = ZERO
                   GO TO BC10.
            PERFORM CLEAR-L50.
           IF W10-SORD = ZERO
               GO TO BC599.
             MOVE W10-SORD       TO W90-REF.

       BC12.
             PERFORM GET-SORDER.
           IF WS-ERROR = 1
        MOVE WS-ERR2  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BC10.
           IF WS-ERROR = 2
        MOVE "Not an order"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BC10.
           IF WS-ERROR = 3
        MOVE "Order corrupted"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BC10.
           IF WS-ERROR = 4
        MOVE "Invalid a/c no"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BC10.
           IF W90-EXP = "E" OR "X"
               MOVE W05-VATNO    TO W90-TAX
           ELSE
               MOVE SPACES       TO W90-TAX.
             DISPLAY S36 AT 0308
            DISPLAY W40-NAME AT 0320
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
            DISPLAY W90-ORD AT 0368
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
           BACKGROUND-COLOR 5.
            DISPLAY W10-SMAN AT 0411
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
           BACKGROUND-COLOR 5.
           IF W10-SMAN NOT = ZERO
               ADD 100 W10-SMAN    GIVING WS-PARKEY
               PERFORM READ-PARAM THRU READ-PARAM-EXIT
              DISPLAY PAR-SALESMAN AT 0420
         WITH FOREGROUND-COLOR 3 HIGHLIGHT
       BACKGROUND-COLOR 5.
      DISPLAY W40-TEL AT 0466
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5.
      DISPLAY W40-CONTACT AT 0528
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5.
             MOVE 1              TO WS-SALKEY.
             SUBTRACT 1          FROM WS-LAST.

       BC15.
             MOVE W90-DB         TO W90-TOTAL.
             SUBTRACT W90-LDISC  FROM W90-TOTAL.
             MOVE W90-DB         TO W100-BAL.
            DISPLAY W100-BAL AT 2015
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
             MULTIPLY -1 BY W90-LDISC
                                 GIVING W100-BAL.
            DISPLAY W100-BAL AT 2115
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
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
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.

       BC20.
            MOVE W90-TOTAL  TO W100-BAL.
            DISPLAY W100-BAL AT 2315
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      *
      *    ****    D I S P L A Y   S A L E S   D E T A I L S
      *
       BC25.
             MOVE 8              TO LIN.
             PERFORM BC115 UNTIL LIN > 18.
             MOVE 8              TO LIN.
             MOVE 1              TO WS-LINES WS-FIRST.

       BC30.
             MOVE WS-FIRST TO WS-SALKEY.
      *
      *    ****    H A V E   A L L   T H E   E N T R I E S   B E E N
      *            D I S P L A Y E D
      *
       BC35.
          IF WS-SALKEY > WS-LAST
               GO TO BC45.
             PERFORM READ-SALE THRU READ-SALE-EXIT.
      MOVE SAL-EXT-ITEM  TO W10-EXT-ITEM.
      *
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *
          IF SAL-EXT-ITEM = SPACES
              DISPLAY (LIN, 21) SAL-SDSC
   WITH FOREGROUND-COLOR 7 HIGHLIGHT
        BACKGROUND-COLOR 0
              GO TO BC40.
      *
      *        *****   E X T E N D E D  C O D E   *****
      *
    IF WS-EXT-STK = "Y"
        DISPLAY S07-EXT AT LINE LIN
    ELSE
      *
      *    *****   A L P H A - N U M E R I C   C O D E  *****
      *
    IF W10-STCK = "A"
        IF WS-USE-ITM = "Y"
     DISPLAY S07-ITM AT LINE LIN
        ELSE
           DISPLAY S07 AT LINE LIN.
            DISPLAY (LIN, 21) SAL-SDSC
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 0.
            MOVE SAL-QNT  TO W10-QUANT.
             DISPLAY S26-QNT AT LINE LIN.
             MOVE SAL-SELL       TO W10-SELL.
             DISPLAY S26-PRC AT LINE LIN.
             MOVE SAL-VAL        TO W100-EXT.
            DISPLAY ( , 70) W100-EXT
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 0.
      *
      *    ****    I S   T H E   S C R E E N   F U L L
      *
       BC40.
           IF LIN < 18
               ADD 1             TO LIN WS-SALKEY WS-LINES
               GO TO BC35.

       BC45.
             EXIT.
      *
      *    ****    D I S P L A Y   O P T I O N S
      *
       BC50.
          IF WS-LINES < 11
               IF WS-FIRST = 1
           MOVE "Print P/'S'lip - 'E'xit"
     TO WS-ERR-MES
     MOVE ZERO  TO WS-LINES
               ELSE
           MOVE "Print P/'S'lip - 'P'revious screen - 'E'xit"
     TO WS-ERR-MES
           MOVE 2  TO WS-LINES
           ELSE
           IF WS-SALKEY < WS-LAST
               IF WS-FIRST = 1
           MOVE "Print P/'S'lip - 'N'ext screen - 'E'xit"
     TO WS-ERR-MES
           MOVE 1  TO WS-LINES
               ELSE
           MOVE "Print P/'S'lip,'N'ext,'P'revious screen,'E'xit"
     TO WS-ERR-MES
           MOVE 3  TO WS-LINES
           ELSE
               MOVE 2            TO WS-LINES
              MOVE "Print P/'S'lip - 'P'revious screen - 'E'xit"
     TO WS-ERR-MES.
       BC55.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE TEST AFTER
       UNTIL WS-OPTION = "S" OR "E" OR "N" OR "P".
           IF WS-OPTION = "E"
               GO TO BC05.
           IF WS-OPTION = "S"
               GO TO BC65.
      *
      *    ****    I S   P R E V I O U S   S C R E E N   R E Q U I R E D
      *
           IF WS-LINES = 2 OR 3
               IF WS-OPTION = "P"
                   PERFORM BC60
                   SUBTRACT 11   FROM WS-FIRST
                   GO TO BC30.
      *
      *    ****    I S   N E X T   S C R E E N   R E Q U I R E D
      *
           IF WS-LINES = 1 OR 3
               IF WS-OPTION = "N"
                   PERFORM BC60
                   ADD 11        TO WS-FIRST
                   GO TO BC30.
             GO TO BC55.
       BC60.
      *
      *    ****    C L E A R   S C R E E N
      *
             MOVE 8              TO LIN.
             PERFORM BC115 UNTIL LIN > 18.
             MOVE 8              TO LIN.
             MOVE 1              TO WS-LINES.
      *
      *    ****    R E A D   D E T A I L S   A N D   P R I N T   S L I P
      *
       BC65.
    IF WS-PGADV = "Y"
        OPEN OUTPUT PRNDSK
        CALL X"91" USING X91-RES X91-FUN PRNDSK.
      PERFORM CLEAR-L50.
            DISPLAY "Printing Picking Slip" AT 2502
        WITH BACKGROUND-COLOR 3
      FOREGROUND-COLOR 6 HIGHLIGHT BLINK.
             MOVE ZERO           TO WS-SALKEY.
             PERFORM AY60 THRU AY999.
      *
      *    ****    R E A D   O R D E R   F I L E   A N D   P R I N T
      *
       BC70.
            MOVE W90-REF  TO SOR-REF.
             MOVE ZERO           TO SOR-SQN.
             PERFORM START-AT-ORDER-NO THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
        MOVE "Database corrupted"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BC599.
       BC75.
             PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
               GO TO BC100.
           IF SOR-REF NOT = W90-REF
               GO TO BC100.
           IF SOR-TYPE = 0
               ADD 1             TO W90-PICK
               PERFORM BB050
               GO TO BC75.
           IF SOR-TYPE = 9
               GO TO BC100.
           IF SOR-TYPE NOT = 1
        IF SOR-TYPE = 2 OR 5
                   GO TO BC75
               ELSE
                   MOVE SOR-DESC TO REP-DESC
                   PERFORM BC80
                   GO TO BC75.
      MOVE SOR-EXT-ITEM  TO PRT-EXT-ITEM.
    IF WS-USE-ITM = "Y"
        MOVE SOR-ITEM  TO REP-ITM1
        MOVE "/"   TO REP-SL
        MOVE SOR-ITM  TO REP-ITM
    ELSE
        MOVE SOR-EXT-ITEM TO REP-EXT-ITEM.
      MOVE SOR-DESC  TO REP-DESC.
      MOVE SOR-QNT  TO REP-QTY.
    IF WS-PIC-PRC = "Y"
        MOVE SOR-VAL  TO REP-PRICE
    ELSE
    IF WS-ITMPRC = "Y"
        MOVE SOR-SELL  TO REP-PRICE.
             PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
           IF WS-F-ERROR = 21
      *  MOVE SOR-QNT   TO REP-QNT
               PERFORM BC80
               GO TO BC75.

       BC80.
    IF LINAGE-COUNTER OF PRNDSK < WS-PRN-LENGTH
               MOVE 1            TO REP-LN
               WRITE REP-L2 BEFORE 1 LINES
           ELSE
               MOVE "PAGE"       TO REP-PG
               WRITE REP-L2 BEFORE PAGE
               MOVE WS-EXPP      TO REP-DETAIL2
               MOVE ZERO         TO REP-LN
               WRITE REP-L2 BEFORE 0 LINES
               MOVE "PICKING SLIP - CONTINUED ..."
                                 TO REP-H1
               WRITE REP-L2 BEFORE 0 LINES
               MOVE 2            TO REP-LN
               WRITE REP-L2 BEFORE 2 LINES
        PERFORM BB60 THRU BB65
        IF WS-TYPE = "P"
     MOVE WS-12CPI TO REP-DETAIL2
     MOVE ZERO  TO REP-LN
     WRITE REP-L2 BEFORE 0 LINES
     MOVE 1  TO REP-LN.
             MOVE SPACES         TO REP-DETAIL2.
      *
      *    ****  P R O D U C T I O N   D E T A I L S
      *
       BC85.
      MOVE "P"   TO WS-TYPE.
      MOVE WS-12CPI  TO REP-DETAIL2.
      MOVE ZERO   TO REP-LN.
      WRITE REP-L2 BEFORE 0 LINES.
      MOVE SPACES  TO REP-DETAIL2.
      MOVE 1   TO REP-LN.
            MOVE 1   TO WS-S6.

       BC90.
           IF PRT-CODE (WS-S6) = SPACES
               ADD 1             TO WS-S6
               GO TO BC95.
    IF WS-USE-ITM = "Y"
        MOVE PRT-CODE(WS-S6)
     TO REP-ITM2
        MOVE "/"   TO REP-SL2
        MOVE PRT-PITM(WS-S6)
     TO REP-ITM3
    ELSE
        MOVE PRT-EXT-CODE(WS-S6)
     TO REP-EXT-ITEM2.
            MULTIPLY PRT-QUANT(WS-S6) BY SOR-QNT
                                 GIVING REP-QNT.
             PERFORM CA000.
           IF WS-F-ERROR = 22
               ADD 1             TO WS-S6
               GO TO BC95.
             MOVE STK-DESC       TO REP-DSCR.
             PERFORM BC80
            ADD 1   TO WS-S6.

       BC95.
           IF WS-S6 < 31
               GO TO BC90.
      MOVE "I"   TO WS-TYPE.
      MOVE WS-NORM  TO REP-DETAIL2.
      MOVE ZERO   TO REP-LN.
      WRITE REP-L2 BEFORE 0 LINES.
      MOVE SPACES  TO REP-DETAIL2.
      MOVE 1   TO REP-LN.
      WRITE REP-L2 BEFORE 1 LINES.
      MOVE 2   TO REP-LN.
      GO TO BC75.
      *
      *    ****    U P D A T E   O R D E R   F I L E
      *
       BC100.
             MOVE SPACES         TO REP-DETAIL2.
             MOVE 1              TO REP-LN
             WRITE REP-L2 BEFORE 1 LINES.
             MOVE ALL "="        TO REP-DETAIL1.
             MOVE W90-REF        TO SOR-REF.
             MOVE 100            TO SOR-SQN.
             PERFORM READ-SORDER-LOCK THRU READ-SORDER-EXIT.
             ADD 1               TO SOR-PICNO.
             PERFORM REWRITE-SORDER-UNLOCK THRU WRITE-SORDER-EXIT.
             PERFORM AY70 THRU AY999.
       BC110.
      *      MOVE "Advance to next page (Y/N)  [Y]"
      *     TO WS-ERR-MES.
             MOVE "Y"            TO WS-OPTION.
      *      PERFORM OPT-MESSAGE.
      *    IF NOT (WS-OPTION = "N" OR "Y")
      *        GO TO BC110.
             MOVE WS-OPTION      TO WS-PGADV.
      *    IF WS-OPTION = "N"
      *        IF LINAGE-COUNTER OF PRNDSK < (WS-PRN-LENGTH - 8)
      *      MOVE 2   TO REP-LN
      *      WRITE REP-L2 BEFORE 2 LINES
      *  ELSE
      *      MOVE "PAGE"   TO REP-PG
      *      WRITE REP-L2 BEFORE PAGE
      *    ELSE
               MOVE "PAGE"       TO REP-PG
               WRITE REP-L2 BEFORE PAGE
               CLOSE PRNDSK
               PERFORM BB400.
             MOVE SPACES         TO REP-DETAIL2.
             GO TO BC05.
       BC115.
            DISPLAY (LIN, 1) "³" WITH BACKGROUND-COLOR 0
           FOREGROUND-COLOR 3
         WS-INV78 WITH FOREGROUND-COLOR 3
         "³" WITH BACKGROUND-COLOR 0
           FOREGROUND-COLOR 3.
            ADD 1   TO LIN.
       BC599.
             EXIT.
      /
      *    ****    P I C K I N G   S L I P S
      *
       BD000      SECTION 5.
       BD05.
      MOVE LS-SLS-ORDNO  TO W90-REF W10-SORD.
             PERFORM GET-SORDER.
           IF WS-ERROR NOT = ZERO
        MOVE "Database corrupted"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BD599.
      *
      *    ****    R E A D   D E T A I L S   A N D   P R I N T   S L I P
      *
       BD65.
           DISPLAY "Printing Picking Slip" AT 2502
       WITH BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6 HIGHLIGHT BLINK.
             MOVE ZERO           TO WS-SALKEY.
             PERFORM AY60 THRU AY999.
      *
      *    ****    R E A D   O R D E R   F I L E   A N D   P R I N T
      *
       BD70.
            MOVE W90-REF  TO SOR-REF.
             MOVE ZERO           TO SOR-SQN.
             PERFORM START-AT-ORDER-NO THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
        MOVE "Database corrupted"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BD599.

       BD75.
             PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
               GO TO BD100.
           IF SOR-REF NOT = W90-REF
               GO TO BD100.
           IF SOR-TYPE = 0
               ADD 1             TO W90-PICK
               PERFORM BB050
               GO TO BD75.
           IF SOR-TYPE = 9
               GO TO BD100.
           IF SOR-TYPE NOT = 1
        IF SOR-TYPE = 2 OR 5
                   GO TO BD75
               ELSE
                   MOVE SOR-DESC TO REP-DESC
                   PERFORM BD80
                   GO TO BD75.
      MOVE SOR-EXT-ITEM  TO PRT-EXT-ITEM.
    IF WS-USE-ITM = "Y"
        MOVE SOR-ITEM  TO REP-ITM1
        MOVE "/"   TO REP-SL
        MOVE SOR-ITM  TO REP-ITM
    ELSE
        MOVE SOR-EXT-ITEM TO REP-EXT-ITEM.
      MOVE SOR-DESC  TO REP-DESC.
      MOVE SOR-QNT  TO REP-QTY.
    IF WS-PIC-PRC = "Y"
        MOVE SOR-VAL  TO REP-PRICE
    ELSE
    IF WS-ITMPRC = "Y"
        MOVE SOR-SELL  TO REP-PRICE.
            PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
           IF WS-F-ERROR = 21
      *  MOVE SOR-QNT   TO REP-QNT
               PERFORM BD80
               GO TO BD75.

       BD80.
    IF LINAGE-COUNTER OF PRNDSK < WS-PRN-LENGTH
               MOVE 1            TO REP-LN
               WRITE REP-L2 BEFORE 1 LINES
           ELSE
               MOVE "PAGE"       TO REP-PG
               WRITE REP-L2 BEFORE PAGE
               MOVE WS-EXPP      TO REP-DETAIL2
               MOVE ZERO         TO REP-LN
               WRITE REP-L2 BEFORE 0 LINES
               MOVE "PICKING SLIP - CONTINUED ..."
                                 TO REP-H1
               WRITE REP-L2 BEFORE 0 LINES
               MOVE 2            TO REP-LN
               WRITE REP-L2 BEFORE 2 LINES
               PERFORM BB60 THRU BB65.
             MOVE SPACES         TO REP-DETAIL2.
      *
      *    ****  P R O D U C T I O N   D E T A I L S
      *
       BD85.
             MOVE 1              TO WS-S6.

       BD90.
           IF PRT-CODE (WS-S6) = SPACES
               ADD 1             TO WS-S6
               GO TO BD95.
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
            MULTIPLY PRT-QUANT (WS-S6) BY SOR-QNT
                                 GIVING REP-QNT.
             PERFORM CA000.
           IF WS-F-ERROR = 22
               ADD 1             TO WS-S6
               GO TO BD95.
             MOVE STK-DESC       TO REP-DSCR.
             PERFORM BD80
             ADD 1 TO WS-S6.

       BD95.
           IF WS-S6 < 31
               GO TO BD90.
             GO TO BD75.
      *
      *    ****    U P D A T E   O R D E R   F I L E
      *
       BD100.
             MOVE SPACES         TO REP-DETAIL2.
             MOVE 1              TO REP-LN
             WRITE REP-L2 BEFORE 1 LINES.
             MOVE ALL "="        TO REP-DETAIL1.
             MOVE W90-REF        TO SOR-REF.
             MOVE 100            TO SOR-SQN.
             PERFORM READ-SORDER-LOCK THRU READ-SORDER-EXIT.
             ADD 1               TO SOR-PICNO.
             PERFORM REWRITE-SORDER-UNLOCK THRU WRITE-SORDER-EXIT.
             PERFORM AY70 THRU AY999.

       BD110.
      MOVE "Advance to next page (Y/N)  [Y]"
     TO WS-ERR-MES.
             MOVE "Y"            TO WS-OPTION.
      PERFORM OPT-MESSAGE TEST AFTER
       UNTIL WS-OPTION = "N" OR "Y".
           IF WS-OPTION = "N"
        IF LINAGE-COUNTER OF PRNDSK < (WS-PRN-LENGTH - 8)
                   MOVE 2        TO REP-LN
                   WRITE REP-L2 BEFORE 2 LINES
               ELSE
                   MOVE "PAGE"   TO REP-PG
                   WRITE REP-L2 BEFORE PAGE
           ELSE
               MOVE "PAGE"       TO REP-PG
               WRITE REP-L2 BEFORE PAGE
               CLOSE PRNDSK
               PERFORM BB400.
             MOVE SPACES         TO REP-DETAIL2.

       BD599.
             EXIT.
      /
       ZA000-INIT        SECTION 9.
       ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
      MOVE LS-PARID  TO WS-PARID.
      MOVE LS-L-OR-N  TO W02-L-OR-N.
      MOVE WS-SYS-ID  TO W02-SYSID.
      MOVE LS-TODAY-DDMMYY
     TO TODAY-DDMMYY.
      MOVE LS-USUB  TO WS-USUB.
            PERFORM ERASE-SCREEN.
      *
      *    ****    S E T   U P   T H E   F U N C T I O N   K E Y S
      *
             MOVE 1              TO USER-ACTION
                                    USER-SETTING.
      *
      *    ESC, F1 to F10 keys
      *
             MOVE ZERO           TO USER-NUMBER.
             MOVE 11             TO USER-KEYS.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    PAGE-UP AND PAGE-DOWN KEYS
      *
             MOVE 53             TO USER-NUMBER.
             MOVE 2              TO USER-KEYS.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    A C T I V A T E   M O U S E
      *
             MOVE 64             TO MOUSE-FUNC.
             MOVE 1              TO MOUSE-PARAM.
             CALL X"AF" USING MOUSE-FUNC
                              MOUSE-PARAM.
           IF MOUSE-FUNC NOT = 255
               MOVE "Y"          TO MOUSE-ATTACHED
               MOVE 66           TO MOUSE-FUNC
               MOVE 0            TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC
                                MOUSE-PARAM
           ELSE
               GO TO ZA00A.
      *
      *    ****    S E T   M O U S E   K E Y   T O   A C T
      *            A S   F U N C T I O N   K E Y
      *
             MOVE 3              TO USER-ACTION.
             MOVE 27             TO USER-NUMBER.
             MOVE 2              TO USER-KEYS.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
       WS-STATUS DELIMITED SIZE
       INTO WS-ERR-MES.
      PERFORM ERROR-LENGTH THRU ERROR-EXIT.
            STOP RUN.

       ZA00-READ-APACFIDS-EXIT.
      EXIT.

       ZA00A-CONTINUE.
      MOVE AFID-PATH  TO W02-PARAM.
      MOVE "DEBTOR"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-DEBTOR.
      MOVE "DEPART"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-DEPART.
      *    *************************************************************
      *    ****      C O N T R O L   F I L E      ****
      *    *************************************************************
      MOVE "NETWORK"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-NETWORK.
      MOVE "PRNDISK"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      INSPECT AFID-PATH REPLACING FIRST "XXX"
          BY LS-USER.
      MOVE AFID-PATH  TO W02-PRNDISK.
      MOVE "SALE"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      INSPECT AFID-PATH REPLACING FIRST "XXX"
          BY LS-USER.
      MOVE AFID-PATH  TO W02-SALE.
      MOVE "SHARED"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-SHARED.
      MOVE "SORDER"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-SORDER.
      MOVE "SPARTS"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-SPARTS.
      MOVE "STOCKF"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-STOCKF.
            MOVE 3   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
    IF NOT (PAR-STOCK = "A" OR "N")
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

       ZA02.
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
   WITH FOREGROUND-COLOR 6 HIGHLIGHT BLINK.
            OPEN OUTPUT PRNREP.
      CALL X"91" USING X91-RES X91-FUN PRNREP.
      MOVE WS-10CPI  TO PRN-DETAIL2.
      WRITE PRN-L1 BEFORE 0 LINES.
      MOVE SPACES  TO PRN-DETAIL2.
            MOVE 1   TO WS-NETKEY.
            PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE NET-DEBTOR     TO W70-DEBT.
    IF W70-ORD-SYS = "Y"
      *        GET HARD DRIVE ID AND IF LOCAL DRIVE (C)
      *        CHANGE ORDER FILE-ID TO SORD??? (??? = USER NO)
        CALL "PC_READ_DRIVE" USING WS-LOCAL-DRIVE
        RETURNING WS-STATUS
        CALL "CBL_TOUPPER" USING WS-LOCAL-DRIVE
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
        IF WS-LOCAL-DRIVE = "C"
     CLOSE SORDER
     MOVE W02-SORDER
     TO WS-SORDER-STR
     MOVE LS0-NO  TO WS-USER-ID
     MOVE WS-LOCAL-ID
     TO W02-SORDER
     OPEN I-O SORDER.
            GO TO ZA999.

       COPY "ZA49.PRO".

      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
            MOVE 1   TO WS-S1.
            MOVE SPACES  TO WS-MD-LINE.

       ZA60.
            MOVE WS-G1   TO WS-TCA(WS-S1) WS-BCA(WS-S1).
          IF WS-S1 < 80
              ADD 1   TO WS-S1
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
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
       "R" WITH FOREGROUND-COLOR 7 HIGHLIGHT
       "etry - "
       "A" WITH FOREGROUND-COLOR 7 HIGHLIGHT
       "bort".
       ZA212.
      PERFORM TEST AFTER
       UNTIL WS-OPTION = "A" OR "R"
        ACCEPT WS-OPTION AT 2334
        WITH FOREGROUND-COLOR 7 HIGHLIGHT AUTO
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
      END-PERFORM.
    IF WS-OPTION = "A"
  MOVE 1   TO WS-ERROR
  GO TO ZA215.
        GO TO ZA210.

       ZA215.
      EXIT.

       ZA999.
             EXIT.

       I-O-ERRORS      SECTION 9.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
           IF WS-F-ERROR = 2
               MOVE W02-NETWORK TO WS-FILE
               MOVE WS-NETKEY TO WS-KEY
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
           IF WS-F-ERROR = 15
               MOVE WS-PARID     TO WS-FILE
               MOVE WS-PARKEY    TO WS-KEY
           ELSE
           IF WS-F-ERROR = 19
              MOVE W02-SALE  TO WS-FILE
              MOVE WS-SALKEY  TO WS-KEY
           ELSE
          IF WS-F-ERROR = 20
               MOVE W02-SORDER   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE SOR-KEY      TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 21
               MOVE W02-SPARTS   TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE PRT-EXT-ITEM TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 22
               MOVE W02-STOCKF   TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE STK-CODE  TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 37
              MOVE W02-SHARED  TO WS-FILE
              MOVE WS-SHARED  TO WS-KEY.

       COPY "DISPERR.PRO".
