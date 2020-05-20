      $set LINKCOUNT"596" GNT"SLS004.GNT"
      ******************************************************************
      *                                                                *
      *    ******   **         ******      ****      ****        ***   *
      *   **    **  **        **    **    **  **    **  **      * **   *
      *   **        **        **         **    **  **    **    *  **   *
      *    ******   **         ******    **    **  **    **   *   **   *
      *         **  **              **   **    **  **    **  ********  *
      *   **    **  **        **    **    **  **    **  **        **   *
      *    ******   ********   ******      ****      ****         **   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *    K E Y - I N   S E R I A L   N U M B E R S   P R O G R A M   *
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
       PROGRAM-ID.     SLS004.
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
       77  WS-START                PIC 9(08).
       77  WS-USE-PACKS            PIC X(01).
       77  WS-USE-ITM              PIC X(01).
       77  WS-EXT-STK              PIC X(01).
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
       77  WS-UPDATE               PIC  9(01)    VALUE ZERO.
       77  WS-CONREC               PIC  9(01)    VALUE ZERO.
       77  WS-HEAD                 PIC  X(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-AGE                  PIC  X(01).
       77  WS-PRC                  PIC  X(01).
       77  WS-A                    PIC  X(19)    VALUE "Any key to continue".
       77  WS-E                    PIC  X(15)    VALUE "**END** - ENTER".
       77  WS-ERR1                 PIC  X(22)    VALUE "Invalid Account Number".
       77  WS-ERR2                 PIC  X(09)    VALUE "No Record".
       77  WS-PER                  PIC  Z(02)9.99.
       77  WS-CR                   PIC  X(01).
       77  WS-TAXRES               PIC  S9(07)V9(08) COMP-3.
       77  TODAY-DDMMYY            PIC 9(08)    COMP-5.
       77  WS-USUB                 PIC 9(04)    COMP-5.
      /

      *    ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
      *    ณ Used by calling program (WORKING-STORAGE) and called program ณ
      *    ณ (LINKAGE SECTION) for Main screen layout and headings.   ณ
      *    ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "SLS004".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "SALES - CAPTURE SERIAL NUMBERS".
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
              05  WS-TCHAR        PIC  X(80) VALUE "ฦออออออออออออออัอออัอออออออออออออออออออออออออออออัออออออออัอออออออออัออออออออออต".
           03  WS-TOP-LNE2.
              05  WS-TCR          PIC  X(80) VALUE "ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ".
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER          PIC  X(01).
               05  WS-TOP-COMP     PIC  X(40).
               05  FILLER          PIC  X(23).
               05  WS-WRKHD        PIC  X(11).
              05  FILLER          PIC  X(01).
               05  WS-WRKST        PIC  X(03).
              05  FILLER          PIC  X(01).
           03  WS-MID-LINE.
              05  WS-MCHAR        PIC  X(80) VALUE "ณ              ณ   ณ                             ณ        ณ         ณ          ณ".
           03  WS-MINV REDEFINES WS-MID-LINE.
               05  WS-INV1         PIC  X(01).
               05  WS-INV78        PIC  X(78).
               05  WS-INV80        PIC  X(01).
           03  WS-MID-LNE2.
               05  FILLER          PIC  X(01) VALUE "ณ".
               05  WS-BLNK78       PIC  X(78) VALUE ALL "ฐ".
               05  FILLER          PIC  X(01) VALUE "ณ".
           03  WS-BOT-LINE.
              05  WS-BCHAR        PIC  X(80) VALUE "ฦออออออออออออออฯอออฯอออออออออออออออออออออออออออออฯออออออออฯอออออออออฯออออออออออต".
           03  WS-BOT-LNE2.
              05  WS-BCR          PIC  X(80) VALUE "ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู".
           03  WS-B-LINE REDEFINES WS-BOT-LNE2.
               05  FILLER          PIC  X(01).
               05  WS-MEM-LINE.
                  07  WS-SRT-LINE  PIC  X(50).
                  07  FILLER       PIC  X(10).
               05  FILLER          PIC  X(19).
           03  WS-MM-LINE.
               05  FILLER          PIC  X(01) VALUE "ณ".
               05  WS-SPACE        PIC  X(60) VALUE SPACES.
               05  FILLER          PIC  X(01) VALUE "ณ".
           03  WS-HME-LINE         PIC X(60) VALUE ALL "อ".
           03  WS-TOP-LNE.
              05  WS-TCHR         PIC  X(80) VALUE "ฦออออออออออออออออัออออออออออออออออออออออออออออออัออออออออออัอออออออออัอออออออออต".
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER          PIC  X(01).
               05  WS-H-LINE       PIC  X(78).
               05  FILLER          PIC  X(01).
           03  WS-MID-LNE.
              05  WS-MCHR         PIC  X(80) VALUE "ณ                ณ                              ณ          ณ         ณ         ณ".
           03  WS-MST-LNE REDEFINES WS-MID-LNE.
               05  WS-MST1         PIC  X(01).
               05  WS-MST78        PIC  X(78).
               05  WS-MST80        PIC  X(01).
           03  WS-BOT-LNE.
              05  WS-BCHR         PIC  X(80) VALUE "ฦออออออออออออออออฯออออออออออออออออออออออออออออออฯออออออออออฯอออออออออฯอออออออออต".
           03  WS-TP-LNE.
              05  WS-TCH          PIC  X(80) VALUE "ฦออออออัออออออออออออออออออออออออออออออออออออออออัอออออออออออัออออออออออัอออออออต".
           03  WS-MD-LNE.
              05  WS-MCH          PIC  X(80) VALUE "ณ      ณ                                        ณ           ณ          ณ       ณ".
           03  WS-MDDT-LNE REDEFINES WS-MD-LNE.
               05  WS-MDT1         PIC  X(01).
               05  WS-MDT78        PIC  X(78).
               05  WS-MDT80        PIC  X(01).
           03  WS-BT-LNE.
              05  WS-BCH          PIC  X(80) VALUE "ฦออออออฯออออออออออออออออออออออออออออออออออออออออฯอออออออออออฯออออออออออฯอออออออต".
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
    03  WS-PROG        PIC  X(03) VALUE "004".

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

       01  W14-ENTRIES.
           03  W14-SUB         PIC 9(03)     COMP-3.
           03  W14-SQN         PIC 9(05)     COMP-3 OCCURS 200.

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
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(08)9-.

       LINKAGE SECTION.

       COPY "CHAIN.LS".

      /
       SCREEN SECTION.

       COPY "BLANK.CRT".

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
           03          COLUMN 51 FOREGROUND-COLOR 11
                                 PIC Z(07)9 USING W10-QUANT AUTO.
       01  S26-PRC.
           03          COLUMN 60 FOREGROUND-COLOR 11
                                 PIC Z(05)9.99 USING W10-SELL AUTO.
       01  S30.
           03  LINE  2 COLUMN 28 FOREGROUND-COLOR 15
                                 VALUE "SALES ORDER - SERIAL NUMBERS".
           03  LINE  3 COLUMN  2 VALUE "Ac No".
           03          COLUMN 15 VALUE "Name".
           03          COLUMN 61 VALUE "Cl-Ref             ".
          03  LINE  4 COLUMN  2 VALUE "Assisted".
           03          COLUMN 61 VALUE "Tel.".
           03  LINE  5 COLUMN  2 VALUE "Slip No".
           03          COLUMN 20 VALUE "Contact".
          03        COLUMN 65 VALUE "Date".
           03  LINE  6 COLUMN  2 REVERSE-VIDEO VALUE "No Item Code    ".
           03          COLUMN 17 REVERSE-VIDEO VALUE "   Description  ".
           03          COLUMN 33 REVERSE-VIDEO VALUE "                ".
           03          COLUMN 49 REVERSE-VIDEO VALUE "  Quantity ".
           03          COLUMN 60 REVERSE-VIDEO VALUE "Unit Price  ".
           03          COLUMN 72 REVERSE-VIDEO VALUE "Extended".
          03  LINE  7 COLUMN  1 BACKGROUND-COLOR 0 VALUE "ฦ".
          03        COLUMN  2 VALUE "อออัออออออออออออออัอออออออออออออ
      -        "ออออออออออออออออัออออออออัอออออออออัออออออออออ".
    03        COLUMN 80 BACKGROUND-COLOR 0 VALUE "ต".
          03  LINE 19 COLUMN  1 BACKGROUND-COLOR 0 VALUE "ฦ".
          03        COLUMN  2 VALUE "อออฯออออออออออออออฯอออออออออออออ
      -        "ออออออออออออออออฯออออออออฯอออออออออฯออออออออออ".
    03        COLUMN 80 BACKGROUND-COLOR 0 VALUE "ต".
          03  LINE 20 COLUMN  3 VALUE "Sub Total".
           03  LINE 21 COLUMN  3 VALUE "Discount".
           03  LINE 22 COLUMN  3 VALUE "T A X -VAT".
          03        COLUMN 30 VALUE "-AD VALOREM".
           03  LINE 23 COLUMN  3 VALUE "AMOUNT DUE".
       01  S30A.
           03  FOREGROUND-COLOR 11 PIC Z(08) USING W10-SORD AUTO.
       01  S36.
           03  FOREGROUND-COLOR 11 PIC X(06) USING W80-ACNO AUTO.

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
             MOVE "S" TO WS-LOOK.
       AA99.
             EXIT.

       COPY "FUNCTION.SCR".

       COPY "ERROR.SCR".

       COPY "OPTION.CRT".

       COPY "LOCKED.RC2".

      *    ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
      *    ณ           ERASE SCREEN FROM SPECIFIED LOCATION            ณ
      *    รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด
      *    ณ Uses CRT-START as starting line and increases and CRT-END ณ
      *    ณ as the ending line. The screen is cleared with Column 1   ณ
      *    ณ and 80 containing "ณ" and columns 2 to 79 containing      ณ
      *    ณ spaces.                                                   ณ
      *    ณ                                                           ณ
      *    ณ  eg.                                                      ณ
      *    ณ     MOVE 8              TO CRT-START.                     ณ
      *    ณ     MOVE 28             TO CRT-END.                       ณ
      *    ณ     PERFORM ERASE-SCREEN.                                 ณ
      *    รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด
      *    ณ             CLEAR MESSAGE LINE (Line 50)                  ณ
      *    รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด
      *    ณ                                                           ณ
      *    ณ     PERFORM CLEAR-L50                                     ณ
      *    รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด
      *    ณ             CHANGE THE TIME ON THE SCREEN                 ณ
      *    รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด
      *    ณ                                                           ณ
      *    ณ     PERFORM CHANGE-TIME.                                  ณ
      *    ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

       COPY "CLEAR.CRT".

      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S
      *
       AA400              SECTION.
       AA401.
            DISPLAY "F1" AT 2502
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Help,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             " Order no./ "
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "F3" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             " Account no./ "
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F4" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
                     " From specified date Lookup"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
       AA405.
             EXIT.
      /    ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
      *    ณ        R E A D  F I L E S   R O U T I N E S        ณ
      *    ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
       AC000    SECTION.

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

      /    ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
      *    ณR E W R I T E   & W R I T E   F I L E S R O U T I N E Sณ
      *    ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
       AD000-WRITE        SECTION.

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

       COPY "SALE.WR".

       COPY "SORDER.WR".

       COPY "SORDER.LUP".

       COPY "APAC.HLP".

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
       AY000           SECTION.
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
       AZ000-END               SECTION.
       AZ000-EOJ.
            CLOSE SALE.
       AZ100.
             EXIT PROGRAM.
      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   C O D E
      *
       CA000         SECTION.
       CA00.
             MOVE 14             TO W50-LENGTH.
       CA05.
             MOVE W10-ITEM       TO W50-DATA.
           IF W50-1 = SPACE
               MOVE W50-13       TO W10-ITEM
               GO TO CA05.
      CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
            MOVE W50-ITEM  TO W10-ITEM.
       CA06.
             MOVE W50-ITEM       TO STK-CODE.
       CA10.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 0
               PERFORM CA500.
       CA99.
             EXIT.
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
      *    ****    Calculate the inclusive prices
      *
       CA500         SECTION.
       CA505.
      *    ****    A V E R A G E   C O S T
    IF STK-QUANT < 0.001
        MOVE STK-COST  TO STK-AVGE.
          IF STK-WSALE = ZERO
               IF STK-CASH NOT = ZERO
                   MOVE STK-CASH TO STK-WSALE
               ELSE
                   MOVE STK-SELL TO STK-WSALE.
           IF STK-CASH = ZERO
               MOVE STK-SELL     TO STK-CASH.
             MOVE STK-CASH       TO W10-VCASH
             MOVE STK-SELL       TO W10-VSELL
             MOVE STK-WSALE      TO W10-VWSALE.
             MOVE ZERO           TO W05-RATE.
           IF STK-TAX > 0
               MOVE W05-VAT (STK-TAX)
                                 TO W05-RATE.
             COMPUTE W10-VCASH ROUNDED
                   = W10-VCASH + (W10-VCASH * W05-RTE).
             COMPUTE W10-VWSALE ROUNDED
                   = W10-VWSALE + (W10-VWSALE * W05-RTE).
             COMPUTE W10-VSELL ROUNDED
                   = W10-VSELL + (W10-VSELL * W05-RTE).
       CA599.
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
             INITIALIZE W14-ENTRIES.
      MOVE 1   TO W14-SUB WS-S7
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
      *    ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
      *    ณ  New amendment to system to allow for back-orders.     ณ
      *    ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
      *     MOVE SOR-QORD     TO SAL-ORD
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
     PERFORM GET-SORD-WRITE-SALE
               ELSE
                   PERFORM GET-CHANGE-TO-REMKS
           ELSE
      *                      P A R T S   D E T A I L S
           IF SOR-TYPE = 2
               INITIALIZE SAL-RECORD1
        MOVE SOR-EXT-ITEM TO SAL-EXT-ITEM
        MOVE SOR-DESC  TO SAL-DESC
               MOVE SOR-QNT      TO SAL-QNT
               MOVE SOR-SERNO    TO SAL-SERNO
               MOVE SOR-SQN      TO SAL-SQN
               PERFORM GET-SORD-WRITE-SALE
           ELSE
      *                      R E M A R K S
           IF SOR-TYPE = 3
               GO TO GET-SORD-NEXT
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
        MOVE SOR-TEXT  TO W32-REMARKS (WS-S7)
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
    IF NOT (SOR-VAT-SUB = ZERO)
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
      *    ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
      *    ณ  New amendment to system to allow for back-orders.     ณ
      *    ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
      *         SOR-QORD.
             PERFORM REWRITE-SORDER-UNLOCK THRU WRITE-SORDER-EXIT.
           IF W14-SUB < WS-SUB
               ADD 1             TO W14-SUB
               GO TO GET-ADJUST-TOTALS-LOOP.

       GET-SORDER-EXIT.
             EXIT.
      /
       BA000                    SECTION.
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
       BB030           SECTION.
       BB30.
             MOVE ZERO           TO WS-ERROR.
             MOVE 1              TO WS-S1.
             MOVE W10-QUANT      TO WS-S2.
             PERFORM SAVE-SCREEN.
      ADD 1 LIN   GIVING CLIN.
       BB40.
      MOVE 43   TO CCOL.
             DISPLAY "Serial No:" AT CPOS WITH FOREGROUND-COLOR 14
                                               BACKGROUND-COLOR 7
   "                    (   )"
    WITH FOREGROUND-COLOR 15
         BACKGROUND-COLOR 7.
       BB41.
             MOVE WS-S1          TO W100-SER.
             MOVE 74             TO CCOL.
             DISPLAY W100-SER AT CPOS WITH FOREGROUND-COLOR 15
                                           BACKGROUND-COLOR 7.
       BB42.
      MOVE 54   TO CCOL.
             ACCEPT W43-SERNO (WS-S1) AT CPOS
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
           IF W43-SERNO (WS-S1) = SPACES
               GO TO BB42.
             MOVE 10             TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W43-SERNO (WS-S1)
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
           IF W43-SERNO (WS-S1) = "NONE"
               MOVE SPACES       TO W43-SERNO (1)
               MOVE 1            TO WS-S2
               GO TO BB45.
             MOVE STK-CODE       TO GUA-CODE.
             MOVE W43-SERNO (WS-S1)
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
      PERFORM OPT-MESSAGE.
    IF NOT (WS-OPTION = "C" OR "R")
        GO TO BB47.
           IF WS-OPTION = "C"
               GO TO BB44.
             GO TO BB42.
       BB49.
             EXIT.
      /
      *    ****    K E Y I N   S E R I A L   N U M B E R S
      *
       BC000      SECTION 5.
       BC05.
      *
      *    ****    S A L E S   O R D E R   S C R E E N
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
             MOVE 1              TO WS-FIRST WS-LAST.
            MOVE SPACES  TO W90-TAX W90-INSTRUCT.
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
      MOVE SAL-EXT-ITEM  TO STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
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
      MOVE SPACES  TO W43-SERIALS.
           IF STK-GUAR = ZERO
               GO TO BC42.
           IF SAL-SERNO NOT = SPACES
               MOVE 1            TO WS-S1
               MOVE W10-QUANT    TO WS-S2
               MOVE SAL-SQN      TO SOR-SQN
               MOVE W10-SORD     TO SOR-REF
               PERFORM BC120 THRU BC130.
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
             MOVE 1              TO WS-S1.
             MOVE W10-QUANT      TO WS-S2.
             MOVE 3              TO SOR-TYPE.

       BC36.
             MOVE 1              TO WS-S3.
             MOVE SPACES         TO SOR-DETAIL.
             ADD 1               TO SOR-SUB.
             PERFORM READ-SORDER-LOCK THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = ZERO
               MOVE 1            TO WS-UPDATE
           ELSE
               MOVE ZERO         TO WS-UPDATE.

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
           IF WS-UPDATE = 1
               PERFORM REWRITE-SORDER-UNLOCK THRU WRITE-SORDER-EXIT
           ELSE
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
       BC50.
      CLOSE SALE.
             OPEN OUTPUT SALE.
             CLOSE SALE.
      OPEN I-O SALE.
            MOVE "'E'xit or any other key to continue"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "E")
               GO TO BC05.
             GO TO BC599.
       BC115.
            DISPLAY (LIN, 1) "ณ" WITH BACKGROUND-COLOR 0
           FOREGROUND-COLOR 3
          WS-INV78 WITH FOREGROUND-COLOR 3
         "ณ" WITH BACKGROUND-COLOR 0
           FOREGROUND-COLOR 3.
            ADD 1   TO LIN.
       BC120.
             ADD 1               TO SOR-SUB.
             PERFORM READ-SORDER-LOCK THRU READ-SORDER-EXIT.
           IF WS-F-ERROR NOT = ZERO
               GO TO BC130.
             MOVE 1              TO WS-S3.
       BC125.
             MOVE SOR-SNUM (WS-S3)
                                 TO W43-SERNO (WS-S1).
             ADD 1               TO WS-S1.
           IF WS-S3 < 3
               ADD 1             TO WS-S3
               GO TO BC125.
             MOVE SOR-SERNO      TO W43-SERNO (WS-S1).
             ADD 1               TO WS-S1.
             PERFORM DELETE-SORDER-REC-UNLOCK THRU WRITE-SORDER-EXIT.
             GO TO BC120.
       BC130.
             EXIT.
       BC599.
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
      MOVE "GARTEE"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-GARTEE.
      MOVE "NETWORK"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-NETWORK.
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
      MOVE PAR-VAT-REG  TO W05-VATNO.
      MOVE PAR-PACKS  TO WS-USE-PACKS.
      MOVE PAR-USE-ITM  TO WS-USE-ITM.
      MOVE PAR-EXT-STK  TO WS-EXT-STK.
            OPEN OUTPUT SALE.
             CLOSE SALE.
             OPEN I-O SALE.
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
      *    *************************************************************
      *    ****   F I L E   L I M I T E X C E E D E D
      *    (Config.sys Dos/Win/Windows 95/98/NT/2000)
      *    (Config.nt Windows XP)
      *    (System.ini Windows.me)
      *    *************************************************************
       ZA49.
            DISPLAY "Too many files OPEN" AT 0708
                      WITH FOREGROUND-COLOR 14.
            DISPLAY "Check the FILES option in CONFIG.SYS (Dos,Windows
      -       "3/95/98/NT/2000)" AT 0908.
      DISPLAY "CONFIG.NT  (Windows XP)" AT 1034.
      DISPLAY "SYSTEM.INI (Windows ME)" AT 1134.
            DISPLAY "Press any key to continue" AT 1308.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      GO TO ZA205.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
             MOVE 1 TO WS-S1.
            MOVE SPACES  TO WS-MD-LINE.
       ZA60.
            MOVE WS-G1   TO WS-TCA (WS-S1)   WS-BCA (WS-S1).
          IF WS-S1 < 80
               ADD 1 TO WS-S1
               GO TO ZA60.
            MOVE WS-G14  TO WS-TCA   (1)  WS-BCA   (1).
            MOVE WS-G13  TO WS-TCA   (80) WS-BCA   (80).
            MOVE WS-G2   TO WS-TCA   (10) WS-TCA   (17)
                                    WS-TCA   (47) WS-TCA   (56)
              WS-TCA   (59) WS-TCA   (68).
            MOVE WS-G3   TO WS-MCA   (1)  WS-MCA   (80)
                                    WS-MCA   (10) WS-MCA   (17)
              WS-MCA   (45) WS-MCA   (56)
              WS-MCA   (59) WS-MCA   (68).
            MOVE WS-G4   TO WS-BCA   (10) WS-BCA   (17)
                                    WS-BCA   (47) WS-BCA   (56)
              WS-BCA   (59) WS-BCA   (68).
            MOVE LS-COMPANY  TO WS-TOP-COMP.
    IF LS-USER = LS-SYS-ID
        MOVE "Supervisorฤ"  TO WS-WRKHD
    ELSE
        MOVE "Workstation"  TO WS-WRKHD
        MOVE LS-USER    TO WS-WRKST.
       ZA200.
             DISPLAY "Files locked - Try later" AT 2312
                      WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
             DISPLAY "Press any key" AT 2512
              WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
            ACCEPT WS-OPTION AT 2526
      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
       ZA999.
             EXIT.

       I-O-ERRORS      SECTION 9.
       OPEN-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
             PERFORM DISPLAY-FILE-NAME.
      
       READ-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
             PERFORM DISPLAY-FILE-NAME.
      
       WRITE-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
             PERFORM DISPLAY-FILE-NAME.
      

       DISPLAY-FILE-NAME.
           IF WS-F-ERROR = 2
               MOVE W02-NETWORK TO WS-FILE
               MOVE WS-NETKEY TO WS-KEY
           ELSE
           IF WS-F-ERROR = 6
              MOVE W02-DEBTOR  TO WS-FILE
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
           ELSE
           IF WS-F-ERROR = 15
               MOVE WS-PARID     TO WS-FILE
               MOVE WS-PARKEY    TO WS-KEY
           ELSE
           IF WS-F-ERROR = 19
               MOVE W02-SALE TO WS-FILE
               MOVE WS-SALKEY TO WS-KEY
           ELSE
           IF WS-F-ERROR = 20
               MOVE W02-SORDER   TO WS-FILE
               MOVE ZERO         TO WS-KEY
           ELSE
           IF WS-F-ERROR = 21
               MOVE W02-SPARTS   TO WS-FILE
               MOVE ZERO         TO WS-KEY
           ELSE
           IF WS-F-ERROR = 22
               MOVE W02-STOCKF   TO WS-FILE
              MOVE ZERO  TO WS-KEY
          ELSE
          IF WS-F-ERROR = 37
              MOVE W02-SHARED  TO WS-FILE
              MOVE WS-SHARED  TO WS-KEY.
           IF WS-STATUS = "10"
               MOVE "End of FILE" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "22"
               MOVE "Duplicate record number" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "23"
               MOVE "Invalid record number" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "24"
               MOVE "DISK full" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "30"
               MOVE "DISK error" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "94"
               MOVE "FILE locked" TO WS-STAT-MESSAGE.
             DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
             DISPLAY "Status " AT 1212
                      WS-STATUS WITH FOREGROUND-COLOR 11
                     ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
           IF WS-STATUS NOT = "94"
               DISPLAY "Key    " AT 1412
                        WS-KEY WITH FOREGROUND-COLOR 11
               DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
               DISPLAY "Please make a note of these details" AT 1812.
             STOP RUN.
