      $set LINKCOUNT"712" GNT"SLS003.GNT"
      ******************************************************************
      *                                                                *
      *    ******   **         ******      ****      ****     ******   *
      *   **    **  **        **    **    **  **    **  **   **    **  *
      *   **        **        **         **    **  **    **        **  *
      *    ******   **         ******    **    **  **    **     ****   *
      *         **  **              **   **    **  **    **        **  *
      *   **    **  **        **    **    **  **    **  **   **    **  *
      *    ******   ********   ******      ****      ****     ******   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *      P R O C E S S   S A L E S   O R D E R   P R O G R A M     *
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
       PROGRAM-ID.     SLS003.
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

       COPY "GARTEE.SL".

       COPY "PARAM.SL".

       COPY "SALE.SL".

       COPY "SHARED.SL".

       COPY "SORDER.SL".

       COPY "SPARTS.SL".

       COPY "STOCK.SL".

      /
       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "DEBTOR.FDE".

       COPY "DEPART.FDE".

       COPY "GARTEE.FDE".

       COPY "PARAM.FDE".

       COPY "SALE.FD".

       COPY "SHARED.FDE".

       COPY "SORDER.FDE".

       COPY "SPARTS.FDE".

       COPY "STOCK.FDE".

      /
       WORKING-STORAGE SECTION.
       77  WS-CHECK                PIC  X(18)    VALUE "aeWlimemnomLalismJ".
       77  WS-DISC                 PIC 99V99  COMP-3.
       77  WS-TDISC                PIC  99V99    COMP-3.
       77  WS-CASH-AC              PIC  X(06).
       77  WS-SUB                  PIC  9(04)    COMP-5.
       77  WS-P1                   PIC 9(04)  COMP-5.
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
       77  WS-PRN1                 PIC 9(01)    VALUE 0.
       77  WS-PRN2                 PIC  9(01)    VALUE 0.
       77  WS-USE-PACKS            PIC X(01).
       77  WS-USE-ITM              PIC X(01).
       77  WS-EXT-STK              PIC X(01).
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
       77  WS-OPTION               PIC  X(01).
       77  WS-SKIP                 PIC X(01)    VALUE " ".
       77  WS-LOOK                 PIC  X(01)    VALUE "S".
       77  WS-MULTI                PIC  X(01)    VALUE "N".
       77  WS-ETYPE                PIC  X(01)    VALUE "I".
       77  WS-ERROR                PIC  9(01).
       77  WS-COMP                 PIC  9(01).
       77  WS-NOSELL               PIC X(01)    VALUE "N".
       77  WS-HEAD                 PIC  X(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-PRC                  PIC  X(01).
       77  WS-PASS                 PIC X(08).
       77  WS-A                    PIC  X(19)    VALUE "Any key to continue".
       77  WS-E                    PIC  X(15)    VALUE "**END** - ENTER".
       77  WS-ERR1                 PIC  X(22)    VALUE "Invalid Account Number".
       77  WS-ERR2                 PIC  X(09)    VALUE "No Record".
       77  WS-PER                  PIC  Z(02)9.99.
       77  WS-CR                   PIC  X(01).
       77  WS-TAXRES               PIC  S9(07)V9(08) COMP-3.
       77  TODAY-DDMMYY            PIC 9(08)    COMP-5.
       77  WS-USUB                 PIC 9(04)    COMP-5.

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
           03  CRT-PROGRAM         PIC  X(15) VALUE "SLS003".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "SALES - PROCESS ORDER".
      *
      *    Company Name
      *
           03  CRT-COMPANY         PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-WRKHD           PIC  X(11).
           03  CRT-WRKST           PIC  X(03).

       01  PRG-INVOICE.
    03  FILLER    PIC X(08) VALUE "SLP\OINV".
    03  PRG-NUMBER  PIC 9(02) VALUE ZERO.
      /
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

       COPY "WS.WS".

       01  WS-HELP.
    03  WS-MODULE       PIC  X(03) VALUE "SLS".
    03  WS-PROG        PIC  X(03) VALUE "003".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

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

       COPY "W05.VAT".

       COPY "W10.SAL".

       01  W11-PARTS.
           03  W11-PART-DETAILS OCCURS 30.
               05  W11-CODE   PIC X(14).
               05  W11-DESC   PIC X(30).
               05  W11-QUANT  PIC S9(05)V9999 COMP-3.

       COPY "W12.WS".

       01  W14-ENTRIES.
           03  W14-SUB         PIC 9(03)     COMP-3.
           03  W14-SQN         PIC 9(05)     COMP-3 OCCURS 200.

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
           03  W25-RESULT      PIC 9(02)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
               05  W25-WHOLE   PIC 9(02).
               05  W25-DECIMAL PIC 9(02).
           03  W25-TOTAL       PIC 9(07)V99  COMP-3.
           03  W25-VALUE       PIC S9(07)V99 COMP-3.
           03  W25-CREDIT      PIC S9(07)V99 COMP-3.

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
               05  W95-DTE     PIC Z9/99/99.

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

       LINKAGE SECTION.

       COPY "CHAIN.LS".

      /
       SCREEN SECTION.

       01  S02.
          03  LINE 2  COLUMN 27 FOREGROUND-COLOR 7 HIGHLIGHT
                                 VALUE "SALES/ORDER PROGRAM CONTROL".

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

       COPY "SORLUP.CRT".

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

       01  S30.
          03  LINE  2 COLUMN 35 FOREGROUND-COLOR 7 HIGHLIGHT
                                 VALUE "SALES ORDER".
           03  LINE  3 COLUMN  2 VALUE "Ac No".
           03          COLUMN 15 VALUE "Name".
    03        COLUMN 20 BACKGROUND-COLOR 5
     PIC  X(40) FROM WS-SPACE.
          03        COLUMN 61 VALUE "Ord No°".
    03        COLUMN 68 BACKGROUND-COLOR 5
     PIC  X(12) FROM WS-SPACE.
          03  LINE  4 COLUMN  2 VALUE "Assisted".
    03        COLUMN 11 BACKGROUND-COLOR 5
     PIC  X(02) FROM WS-SPACE.
    03        COLUMN 61 VALUE "Tel.".
    03        COLUMN 66 BACKGROUND-COLOR 5
     PIC  X(14) FROM WS-SPACE.
          03  LINE  5 COLUMN  2 VALUE "Slip No".
    03        COLUMN 10 BACKGROUND-COLOR 0
     PIC  X(08) FROM WS-SPACE.
    03        COLUMN 20 VALUE "Contact".
    03        COLUMN 29 BACKGROUND-COLOR 5
     PIC  X(14) FROM WS-SPACE.
          03        COLUMN 65 VALUE "Date".
    03        COLUMN 70 BACKGROUND-COLOR 5
     PIC  X(08) FROM WS-SPACE.
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
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
          03  LINE 20 COLUMN  3 VALUE "Sub Total".
           03  LINE 21 COLUMN  3 VALUE "Discount".
           03  LINE 22 COLUMN  3 VALUE "T A X -VAT".
           03  LINE 23 COLUMN  3 VALUE "AMOUNT DUE".

       01  S30A.
    03  FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z(08) USING W10-SORD AUTO.

       01  S36.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC X(06) USING W80-ACNO AUTO.

       01  S40.
    03  LINE  9 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "Ú".
    03        COLUMN 16 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
                                 PIC X(50) USING WS-SRT-LINE.
    03        COLUMN 66 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "¿".
          03  LINE 10 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     PIC  X(18) FROM WS-SPACE.
    03        COLUMN 34 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "CLIENT DETAILS".
    03        COLUMN 48 BACKGROUND-COLOR 2
     PIC  X(18) FROM WS-SPACE.
          03        COLUMN 66 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
          03  LINE 11 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "Name    >".
    03        COLUMN 65 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".
          03        COLUMN 66 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
          03  LINE 12 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "Address >".
    03        COLUMN 55 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".
    03        COLUMN 56 BACKGROUND-COLOR 2
     PIC  X(10) FROM WS-SPACE.
          03        COLUMN 66 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
          03  LINE 13 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     PIC  X(08) FROM WS-SPACE.
    03        COLUMN 24 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE ">".
    03        COLUMN 55 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".
    03        COLUMN 56 BACKGROUND-COLOR 2
     PIC  X(10) FROM WS-SPACE.
          03        COLUMN 66 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
          03  LINE 14 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     PIC  X(08) FROM WS-SPACE.
    03        COLUMN 24 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE ">".
    03        COLUMN 55 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".
    03        COLUMN 56 BACKGROUND-COLOR 2
     PIC  X(10) FROM WS-SPACE.
          03        COLUMN 66 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
          03  LINE 15 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     PIC  X(08) FROM WS-SPACE.
    03        COLUMN 24 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE ">".
    03        COLUMN 55 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".
    03        COLUMN 56 BACKGROUND-COLOR 2
     PIC  X(10) FROM WS-SPACE.
          03        COLUMN 66 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
          03  LINE 16 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     PIC  X(08) FROM WS-SPACE.
    03        COLUMN 24 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE ">".
    03        COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".
    03        COLUMN 30 BACKGROUND-COLOR 2
     PIC  X(36) FROM WS-SPACE.
          03        COLUMN 66 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
    03  LINE 17 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "À".
    03        COLUMN 16 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
                                 PIC X(50) USING WS-SRT-LINE.
    03        COLUMN 66 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "Ù".

       01  S41.
    03  LINE 11 COLUMN 25 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC X(40) USING W40-NM1 AUTO.
    03  LINE 12 COLUMN 25 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC X(30) USING W40-AD1 AUTO.
    03  LINE 13 COLUMN 25 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC X(30) USING W40-AD2 AUTO.
    03  LINE 14 COLUMN 25 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC X(30) USING W40-AD3 AUTO.
    03  LINE 15 COLUMN 25 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC X(30) USING W40-AD4 AUTO.
    03  LINE 16 COLUMN 25 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC X(04) USING W40-PC AUTO.

      /
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS.
       AA000-MAIN        SECTION.
       AA000-INIT.
    IF LS-DSKDRV = "L"
        MOVE "Not available on Local drive"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO AA49.
          IF LS0-SALEV < 1
        MOVE "Not Authorised"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO AA49.
            PERFORM ZA000-INIT.
            GO TO BA00.

       AA49.
      EXIT PROGRAM.

       AA50-LOOKUP       SECTION.
       AA50.
            PERFORM SAVE-SCREEN.
             MOVE 4              TO W44-FUNCTION.
            PERFORM SCREEN-CONTENTS.
      PERFORM SORDER-LOOKUP.
            PERFORM RESTORE-SCREEN.
       AA99.
             EXIT.
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
      *    ³ ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿   ³
      *    ³ ³    Print transactions (Y/N) [ ]         ³°° ³
      *    ³ ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°° ³
      *    ³   °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° ³
      *    ³                                                           ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³                                                           ³
      *    ³ To display the request message higher or lower (default   ³
      *    ³ is line 20) move the line number to be used to            ³
      *    ³ WS-MES-LINE.                                              ³
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
       AA400             SECTION.
       AA401.
            DISPLAY "F1" AT 2502
        WITH BACKGROUND-COLOR 3
      FOREGROUND-COLOR 6 HIGHLIGHT "=Help,"
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
      /
      *    ****    READ FILES ROUTINES
      *
       AC000             SECTION.

       COPY "CONTROL.RD".

       COPY "DEBTOR.RD".

       COPY "DEPART.RD".

       COPY "GARTEE.RD".

       COPY "PARAM.RD".

       COPY "SALE.RD".

       COPY "SHARED.RD".

       COPY "SORDER.RD".

       COPY "SPARTS.RD".

       COPY "STOCK.RD".

      /
       AD000-WRITE       SECTION.

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

       COPY "SALE.WR".

       COPY "SORDER.WR".

       COPY "SORDER.LUP".

       COPY "APAC.HLP".

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
       AY000             SECTION.
       AY60.
      *
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
            MOVE 1   TO WS-COUNT.
            MOVE 5   TO WS-SHARED.
            PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
            GO TO AY999.
       AY70.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 1.
      *
             COMMIT.
       AY999.
             EXIT.
      /
       AZ000-END         SECTION.
       AZ000-EOJ.
            CLOSE SALE.
       AZ100.
             EXIT PROGRAM.

       COPY "CA000.STK".

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

       COPY "CA200.STK.

       COPY "CA500.STK.

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
       GET-SORDER        SECTION 5.

       GET-SORD-ORDNO.
             INITIALIZE W14-ENTRIES.
      MOVE 1   TO W14-SUB WS-S7.
             MOVE SPACES         TO W40-NM1 W40-AD1 W40-AD2
                                    W40-AD3 W40-AD4 W40-PC.
             MOVE W10-SORD       TO SOR-REF.
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
           IF WS-MULTI NOT = "D"
               GO TO GET-SORD-TEST-TYPE.

       GET-SORD-NEXT.
      PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
               MOVE 3            TO WS-ERROR
               GO TO GET-SORDER-EXIT.
           IF WS-MULTI NOT = "D"
               GO TO GET-SORD-TEST-TYPE.
           IF SOR-TYPE = 9
               GO TO GET-SORDER-EXIT.
           IF SOR-TYPE = 3
               IF SOR-SUB > ZERO
                   PERFORM DELETE-SORDER-REC THRU WRITE-SORDER-EXIT.
             GO TO GET-SORD-NEXT.

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
               MOVE SOR-ORD      TO W90-ORD
               MOVE SOR-SMAN     TO W10-SMAN
               MOVE SOR-EXP      TO W90-EXP
               MOVE SOR-PRC      TO WS-PRC
        MOVE SOR-CONTACT  TO W40-CONTACT
               MOVE SOR-TEL      TO W40-TEL
           ELSE
      *                      I T E M   D E T A I L S
           IF SOR-TYPE = 1
               PERFORM GET-CHECK-STOCK-FILE
               IF WS-F-ERROR = 0
                   INITIALIZE SAL-RECORD1
     MOVE SOR-EXT-ITEM TO SAL-EXT-ITEM
                   MOVE SOR-DESC     TO SAL-MSER
                   MOVE SOR-QNT      TO SAL-QNT
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
     MOVE SOR-PACK     TO SAL-PACK
     MOVE SOR-SQN      TO SAL-SQN
     IF SAL-VAT-SUB > ZERO
         ADD SOR-TAXVAL
     TO W90-TAXVAL(SAL-VAT-SUB)
     END-IF
     IF (PRG-NUMBER = 3) AND (STK-LDG = "98  ")
         MOVE "Y"  TO  W40-SPAR
     END-IF
     PERFORM GET-SORD-WRITE-SALE
               ELSE
                   PERFORM GET-CHANGE-TO-REMKS
           ELSE
      *                      P A R T S   D E T A I L S
           IF SOR-TYPE = 2
               INITIALIZE SAL-RECORD1
        MOVE SOR-EXT-ITEM TO SAL-EXT-ITEM
               MOVE SOR-DESC     TO SAL-DESC
               MOVE SOR-QNT      TO SAL-QNT
               MOVE SOR-SERNO    TO SAL-SERNO
               MOVE SOR-SQN      TO SAL-SQN
               PERFORM GET-SORD-WRITE-SALE
           ELSE
      *                      R E M A R K S
           IF SOR-TYPE = 3
               IF SOR-SUB > ZERO
                   GO TO GET-SORD-NEXT
               ELSE
                   INITIALIZE SAL-RECORD1
                   MOVE SOR-DESC     TO SAL-MSER
                   MOVE SOR-SQN      TO SAL-SQN
                   PERFORM GET-SORD-WRITE-SALE
           ELSE
      *                      I N T E R N A L   N O T E S
           IF SOR-TYPE = 4
               GO TO GET-SORD-NEXT
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
        MOVE SOR-TEXT  TO W32-REMARKS(WS-S7)
        ADD 1   TO WS-S7
    ELSE
      *                      T R A I L E R
               MOVE SOR-SUBTOT   TO W90-DB
               MULTIPLY -1 BY SOR-DSCNT
                                 GIVING W90-TDISC
               MOVE W90-TDISC    TO W90-LDISC
               MOVE SOR-NON-TXBL TO W90-NONTAX
        MOVE SOR-TAXABLE  TO W90-EXCTOT
               MOVE SOR-TAX      TO W90-TTAX
               MOVE SOR-TOT      TO W90-TOTAL
               MOVE SOR-BAL      TO W90-BAL
        MOVE SOR-INSTRUCT TO W90-INSTRUCT
               MOVE SOR-PMETH    TO W90-PMETH
               MOVE SOR-STATUS   TO W40-STATUS
               MOVE SOR-HTEL     TO W40-HTEL
               MOVE SOR-ID       TO W40-ID
               IF W14-SUB > 1
                   GO TO GET-ADJUST-TOTALS
               ELSE
                   GO TO GET-SORDER-EXIT.
             GO TO GET-SORD-NEXT.

       GET-SORD-WRITE-SALE.
             MOVE WS-LAST        TO SAL-CODE.
             PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
             ADD 1               TO WS-LAST WS-SALKEY.

       GET-CHECK-STOCK-FILE.
      MOVE SOR-EXT-ITEM  TO STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.

       GET-CHANGE-TO-REMKS.
             INITIALIZE SAL-RECORD1.
      STRING SOR-EXT-ITEM DELIMITED BY SIZE
                    SOR-DESC DELIMITED BY SIZE
                                 INTO SAL-DESC.
      MOVE SOR-SQN  TO SAL-SQN W14-SQN(W14-SUB).
             ADD 1               TO W14-SUB.
             PERFORM GET-SORD-WRITE-SALE.
             GO TO GET-SORD-NEXT.

       GET-ADJUST-TOTALS.
             SUBTRACT 1 FROM W14-SUB
                                 GIVING WS-SUB.
             MOVE 1              TO W14-SUB.

       GET-ADJUST-TOTALS-LOOP.
      MOVE W14-SQN(W14-SUB)
                                 TO SOR-SQN.
             PERFORM READ-SORDER-LOCK THRU READ-SORDER-EXIT.
             SUBTRACT SOR-VAL    FROM W90-DB.
             SUBTRACT SOR-DISC   FROM W90-LDISC.
             SUBTRACT SOR-NONTAX FROM W90-NONTAX.
    IF NOT(SOR-VAT-SUB = ZERO)
              SUBTRACT SOR-TAXVAL FROM W90-TAXVAL(WS-VAT-SUB).
             COMPUTE W90-BAL = W90-BAL - (SOR-COST * SOR-QNT).
             MOVE 3              TO SOR-TYPE.
      STRING SOR-EXT-ITEM DELIMITED BY SIZE
                    SOR-DESC DELIMITED BY SIZE
                                 INTO SOR-DESC.
      MOVE SPACES  TO SOR-EXT-ITEM SOR-SERNO.
             MOVE ZERO           TO SOR-QNT SOR-COST SOR-INCTAX
                                    SOR-VAT SOR-SELL SOR-NONTAX
                                    SOR-VAL SOR-DISC SOR-TAXVAL
                                    SOR-DSC.
             PERFORM REWRITE-SORDER-UNLOCK THRU WRITE-SORDER-EXIT.
           IF W14-SUB < WS-SUB
               ADD 1             TO W14-SUB
               GO TO GET-ADJUST-TOTALS-LOOP.

       GET-SORDER-EXIT.
             EXIT.
      /
       BA000             SECTION.
       BA00.
      *                            P R O C E S S   O R D E R
      *
      *      THE COPIED FILE W90.SAL HAS A VALUE OF "O" FOR W90-ORG
      *      so the following instruction is now a comment and will be
      *      deleted at a later date.
      *       
      *      MOVE "O"                TO W90-ORG.
             PERFORM BC000.
      *                            E X I T   P R O G R A M
             GO TO AZ000-EOJ.

      /
       BB000             SECTION 6.
       BB00.
    IF DEB-NCASH = "CASH SALE" OR "KONTANT V"
               GO TO BB06.

       BB05.
      MOVE "Enter client details Y/N  [Y]"
     TO WS-ERR-MES.
      MOVE "Y"   TO WS-OPTION.
      PERFORM OPT-MESSAGE TEST AFTER
       UNTIL WS-OPTION = "N" OR "Y".
    IF WS-OPTION = "N"
        GO TO BB29.

       BB06.
      PERFORM SAVE-SCREEN-2.
      MOVE SPACES  TO W40-NAME W40-ADD1 W40-ADD2
                                    W40-ADD3 W40-ADD4 W40-PC1.
             MOVE 0915           TO CPOS.

       BB10.
             DISPLAY
                 "                                                    "
                 AT CPOS.
           IF CLIN < 17
               ADD 1             TO CLIN
               GO TO BB10.
      MOVE 10   TO SHADE-ROW.
      MOVE 17   TO SHADE-COL.
      MOVE 50   TO SHADE-WIDTH.
      MOVE 8   TO SHADE-LINES.
      DISPLAY S40.
      PERFORM SCREEN-SHADOW.
       BB15.
      ACCEPT S41.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO BB15.
    IF DEB-NCASH = "CASH SALE" OR "KONTANT V"
               IF (W40-NM1 = SPACES) OR
    (W40-AD1 = SPACES)
     MOVE "Require Name & Address"
     TO WS-ERR-STRING
           PERFORM ERROR-MESSAGE
     GO TO BB15.
    IF DEB-NCASH = "CASH SALE"
               MOVE W40-NM1      TO W40-NAME
               MOVE W40-AD1      TO W40-ADD1
               MOVE W40-AD2      TO W40-ADD2
               MOVE W40-AD3      TO W40-ADD3
               MOVE W40-AD4      TO W40-ADD4
               MOVE W40-PC       TO W40-PC1.
       BB20.
            PERFORM RESTORE-SCREEN-2.
      PERFORM CLEAR-L50.
       BB29.
             EXIT.
      /
       BB030             SECTION 7.
       BB30.
             MOVE ZERO           TO WS-ERROR.
             MOVE 1              TO WS-S1.
             MOVE W10-QUANT      TO WS-S2.
             PERFORM SAVE-SCREEN.
      ADD 1 LIN   GIVING CLIN.
       BB40.
      MOVE 43   TO CCOL.
      DISPLAY "Serial No:" AT CPOS
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
      BACKGROUND-COLOR 7
   "                    (   )"
    WITH FOREGROUND-COLOR 7 HIGHLIGHT
         BACKGROUND-COLOR 7.
       BB41.
             MOVE WS-S1          TO W100-SER.
             MOVE 74             TO CCOL.
      DISPLAY W100-SER AT CPOS
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 7.
       BB42.
      MOVE 54   TO CCOL.
      ACCEPT W43-SERNO(WS-S1) AT CPOS
                    WITH FOREGROUND-COLOR 0
                         BACKGROUND-COLOR 7 UPDATE AUTO.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                     MOVE SPACES TO W43-SERIALS
                     GO TO BB45
                 WHEN OTHER
                     CALL X"E5"
                     GO TO BB42
               END-EVALUATE.
      PERFORM CLEAR-L50.
    IF W43-SERNO(WS-S1) = SPACES
               GO TO BB42.
             MOVE 10             TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W43-SERNO(WS-S1)
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
    IF W43-SERNO(WS-S1) = "NONE"
        MOVE SPACES  TO W43-SERNO(1)
               MOVE 1            TO WS-S2
               GO TO BB45.
             MOVE STK-CODE       TO GUA-CODE.
      MOVE W43-SERNO(WS-S1)
                                 TO GUA-SERIAL.
             PERFORM READ-GARTEE THRU READ-GARTEE-EXIT.
           IF WS-F-ERROR = ZERO
        MOVE "Serial number duplicated"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BB47.

       BB44.
           IF WS-S1 < WS-S2
               ADD 1             TO WS-S1
               GO TO BB41.

       BB45.
             PERFORM RESTORE-SCREEN.
      GO TO BB49.

       BB47.
            MOVE "'C'ontinue or 'R'ekey  [R]"
     TO WS-ERR-MES.
      MOVE "R"   TO WS-OPTION.

       BB48.
      PERFORM OPT-MESSAGE TEST AFTER
       UNTIL WS-OPTION = "C" OR "R".
      PERFORM CLEAR-L50.
           IF WS-OPTION = "C"
               GO TO BB44.
             GO TO BB42.
       BB49.
             EXIT.
      /
      *    ****    P R O C E S S   O R D E R S
      *
       BC000             SECTION 5.
      *
      *    ****    S A L E S   O R D E R   S C R E E N
      *
       BC05.
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
             MOVE 1              TO WS-FIRST WS-LAST.
            MOVE SPACES  TO W90-TAX W90-INSTRUCT W40-SPAR.
      MOVE "N"   TO WS-NOSELL.
      MOVE 1   TO WS-S9.

       BC08.
            MOVE ZERO   TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
    IF WS-S9 < 12
        ADD 1   TO WS-S9
        GO TO BC08.
      MOVE ZERO   TO W10-SORD.
             MOVE "N"            TO WS-MULTI.
             INITIALIZE W14-ENTRIES.

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
              PERFORM AA50-LOOKUP
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
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
           BACKGROUND-COLOR 5.
            DISPLAY W10-SMAN AT 0411
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
           BACKGROUND-COLOR 5.
          IF W10-SMAN NOT = ZERO
               ADD 100 W10-SMAN    GIVING WS-PARKEY
               PERFORM READ-PARAM THRU READ-PARAM-EXIT
              DISPLAY PAR-SALESMAN AT 0420
         WITH FOREGROUND-COLOR 3 HIGHLIGHT
       BACKGROUND-COLOR 5.
      DISPLAY W40-TEL AT 0466
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      DISPLAY W40-CONTACT AT 0528
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      MOVE 1   TO WS-FIRST WS-SALKEY.
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
            MOVE WS-FIRST  TO WS-SALKEY.
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
              DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
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
            DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 0.
            MOVE SAL-QNT  TO W10-QUANT.
             DISPLAY S26-QNT AT LINE LIN.
             MOVE SAL-SELL       TO W10-SELL.
             DISPLAY S26-PRC AT LINE LIN.
             MOVE SAL-VAL        TO W100-EXT.
            DISPLAY W100-EXT AT COLUMN 70
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 0.
      MOVE SPACES  TO W43-SERIALS.
           IF STK-GUAR = ZERO
               GO TO BC42.
           IF SAL-SERNO NOT = SPACE
               MOVE 1            TO WS-S1
               MOVE W10-QUANT    TO WS-S2
               GO TO BC42.
             PERFORM BB030.
           IF WS-ERROR = 1
               GO TO BC599.
             PERFORM AY60 THRU AY999.
             MOVE SAL-SQN        TO SOR-SQN.
             MOVE W10-SORD       TO SOR-REF.
      PERFORM READ-SORDER-LOCK THRU READ-SORDER-EXIT.
             MOVE "Y"            TO SOR-SERNO
                                    SAL-SERNO.
             PERFORM REWRITE-SORDER-UNLOCK THRU WRITE-SORDER-EXIT.
             PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
             MOVE "Y"            TO WS-MULTI.
             MOVE W10-QUANT      TO WS-S2.
             MOVE 1              TO WS-S1.
             MOVE 3              TO SOR-TYPE.

       BC36.
             MOVE 1              TO WS-S3.
             MOVE SPACES         TO SOR-DETAIL.

       BC37.
      MOVE W43-SERNO(WS-S1)
     TO SOR-SNO(WS-S3).
             ADD 1               TO WS-S1.
           IF WS-S1 > WS-S2
               PERFORM BC38
               GO TO BC40.
    IF WS-S3 < 2
               ADD 1             TO WS-S3
               GO TO BC37.

       BC38.
             ADD 1               TO SOR-SUB.
             PERFORM WRITE-SORDER THRU WRITE-SORDER-EXIT.

       BC39.
             ADD 1               TO WS-S1.
           IF WS-S1 NOT > WS-S2
               GO TO BC36.
      *
      *    ****    I S   T H E   S C R E E N   F U L L
      *
       BC40.
             PERFORM AY70 THRU AY999.
       BC42.
           IF LIN < 18
               ADD 1             TO LIN WS-SALKEY WS-LINES
               GO TO BC35.
      MOVE "Press any key to continue"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
      PERFORM BC25.
             GO TO BC35.

       BC45.
             EXIT.
      *
      *    ****    D I S P L A Y   O P T I O N S
      *
       BC50.
            MOVE "Print 'I'nvoice - 'E'xit"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.

       BC55.
      PERFORM OPT-MESSAGE TEST AFTER
       UNTIL WS-OPTION = "E" OR "P" OR "I" OR "*".
           IF WS-OPTION = "E"
               IF WS-MULTI = "Y"
                   MOVE "D"      TO WS-MULTI
                   PERFORM GET-SORDER
               END-IF
               GO TO BC05.
      *
      *        R U M A R C   -  O N L Y
      *
    IF WS-OPTION = "*"
        PERFORM MA000.
    IF DEB-TERMS = "3"
        PERFORM BB000.
      *
      *    ****    C L E A R   S C R E E N
      *
       BC60.
            MOVE 8   TO LIN.
             PERFORM BC115 UNTIL LIN > 18.
             MOVE 8              TO LIN.
             MOVE 1              TO WS-LINES.
            MOVE ZERO   TO W90-TTAX.
      MOVE 1   TO WS-P1.
      *
      *    ****    C A L C U L A T E   T O T A L   V A T
      *
       BC61.
          IF (W90-TAXVAL(WS-P1) = ZERO)
              GO TO BC62.
            MOVE W05-VAT(WS-P1) TO W05-RATE.
            COMPUTE W90-TTAX ROUNDED = (W90-TTAX +
      (W90-TAXVAL(WS-P1) * W05-RTE)).
      *
      *    ****    C H E C K   I F   A L L   V A T   R A T E S
      *     H A V E   B E E N   C A L C U L A T E D
      *
       BC62.
    IF WS-P1 < 6
        ADD 1   TO WS-P1
        GO TO BC61.
      *
      *    ****    P R I N T   I N V O I C E
      *
       BC65.
             PERFORM ERASE-SCREEN.
      DISPLAY "PROCESS INVOICE" AT 0236
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
             DISPLAY "Printing invoice" AT 1212
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
      MOVE W90-REF  TO LS-SLS-ORDNO.
      MOVE WS-NOSELL  TO W40-NOSELL.
      CLOSE SALE.
            CALL PRG-INVOICE USING LS-PARID
        LS-USER-ID
        LS0-PROGRAMS
        W40-COMPANY
        W90-TRAN
    ON EXCEPTION
       GO TO BC100
      END-CALL.
      CANCEL PRG-INVOICE.
             OPEN OUTPUT SALE.
             CLOSE SALE.
            OPEN I-O SALE.
            MOVE "ENTER to continue - E to exit"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT(WS-OPTION = "E")
               GO TO BC05.
             GO TO BC599.

       BC100.
      MOVE SPACES     TO WS-ERR-MES.
      STRING "Program- " DELIMITED SIZE
       PRG-INVOICE DELIMITED " "
       " not on disk, press ANY key"
       DELIMITED SIZE
       INTO WS-ERR-MES.
      PERFORM ERROR-LENGTH THRU ERROR-EXIT.
      GO TO BC599.
       BC115.
            DISPLAY "³" AT LINE LIN COLUMN 1
    WITH BACKGROUND-COLOR 0 FOREGROUND-COLOR 3
        WS-INV78 WITH FOREGROUND-COLOR 3
       "³" WITH BACKGROUND-COLOR 0 FOREGROUND-COLOR 3.
            ADD 1   TO LIN.
       BC599.
             EXIT.
      /
       MA000             SECTION 8.
       MA00.
      PERFORM SAVE-SCREEN.
      MOVE SPACES  TO WS-PASS.
      MOVE 12   TO SHADE-ROW.
      MOVE 29   TO SHADE-COL.
      MOVE 25   TO SHADE-WIDTH.
      MOVE 2   TO SHADE-LINES.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1127
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 7
       "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
            DISPLAY "³ " AT 1227 WITH FOREGROUND-COLOR 7 HIGHLIGHT
           BACKGROUND-COLOR 7
       "Enter Password          "
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 7
       "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
      DISPLAY "À" AT 1327 WITH FOREGROUND-COLOR 7 HIGHLIGHT
          BACKGROUND-COLOR 7
       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
      PERFORM SCREEN-SHADOW.
            ACCEPT WS-PASS AT 1244
      WITH BACKGROUND-COLOR 5
    FOREGROUND-COLOR 3 HIGHLIGHT AUTO NO-ECHO.
      MOVE 8   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING WS-PASS
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
      PERFORM RESTORE-SCREEN.
    IF WS-PASS = "AVOIDING"
        MOVE "Y"   TO WS-NOSELL
    ELSE
    IF WS-PASS = "PRINTING"
        MOVE "P"   TO WS-NOSELL
    ELSE
        MOVE "N"   TO WS-NOSELL.

       MA99.
      EXIT.
      /
       ZA000-INIT        SECTION 9.
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

             COPY ""FUNCTION.PRO".

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
      MOVE PAR-PACKS  TO WS-USE-PACKS.
      MOVE PAR-USE-ITM  TO WS-USE-ITM.
      MOVE PAR-EXT-STK  TO WS-EXT-STK.
      MOVE PAR-VAT-REG  TO W05-VATNO.
            MOVE 3   TO WS-PARKEY.
      READ PARAM.
    IF PAR-INV = ZERO
        MOVE 1   TO PRG-NUMBER
    ELSE
        MOVE PAR-INV  TO PRG-NUMBER.
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
             MOVE 7              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-CASH-AC    TO WS-CASH-AC.
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
        MOVE "SupervisorÄ"
     TO WS-WRKHD
    ELSE
        MOVE "Workstation"
     TO WS-WRKHD
        MOVE LS-USER  TO WS-WRKST.
       ZA200.

       COPY "LOCKED.PRO".

       ZA205.
             EXIT PROGRAM.

       ZA999.
             EXIT.

       I-O-ERRORS        SECTION 10.

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
               WHEN 9          MOVE W02-GARTEE   TO WS-FILE
                               MOVE GUA-KEY      TO WS-KEYX
               WHEN 15         MOVE WS-PARID     TO WS-FILE
                               MOVE WS-PARKEY    TO WS-KEY
               WHEN 19         MOVE W02-SALE     TO WS-FILE
                               MOVE WS-SALKEY    TO WS-KEY
               WHEN 20         MOVE W02-SORDER   TO WS-FILE
                               MOVE ZERO         TO WS-KEY
               WHEN 21         MOVE W02-SPARTS   TO WS-FILE
                               MOVE PRT-EXT-ITEM TO WS-KEYX
               WHEN 22         MOVE W02-STOCKF   TO WS-FILE
                               MOVE STK-CODE     TO WS-KEYX
               WHEN 37         MOVE W02-SHARED   TO WS-FILE
                               MOVE WS-SHARED    TO WS-KEY
               WHEN OTHER      MOVE "ERROR"      TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
