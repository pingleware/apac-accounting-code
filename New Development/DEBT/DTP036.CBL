      $SET LINKCOUNT"384" GNT"DTP036.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****     ******    ******    *
      *   **    **     **     **    **   **  **   **    **  **    **   *
      *   **    **     **     **    **  **    **        **  **         *
      *   **    **     **     *******   **    **     ****   *******    *
      *   **    **     **     **        **    **        **  **    **   *
      *   **    **     **     **         **  **   **    **  **    **   *
      *   *******      **     **          ****     ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R   C O N T R A C T   D E L E T E                *
      *                                                                *
      *       Version 9.00.81 - November 2015                          *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  May 2006 - New field included in Accounts Receivable          *
      *             for recording number of outstanding Jobs           *
      *             linked to the account. Additional filler           *
      *             added for future expansion.                        *
      *                                                                *
      *  Jan 2008 - New file (DEBALT)- Debtor alternate index included *
      *             for lookups, using any word contained in the       *
      *             Debtor name.                                       *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     DTP036.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   February 1985.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1985 - 2016
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

       COPY "PARAM.SL".

       COPY "DEBTOR.SL".

       COPY "DBFOTO.SL".

       COPY "DEBMEM.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "PARAM.FDE".

       COPY "DEBTOR.FDE".

       COPY "DBFOTO.FDE".

       COPY "DEBMEM.FDE".

       COPY "RECOVER.FDE".

       COPY "SHARED.FDE".

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
       77  WS-COPYRIGHT            PIC  X(18)     VALUE "aeWlimemnomLalismJ".
       77  WS-SUB                  PIC  9(04)     COMP-5.
       77  WS-SUB-1                PIC  9(04)     COMP-5.
       77  WS-SUB-2                PIC  9(04)     COMP-5.
       77  WS-SUB-3                PIC  9(04)     COMP-5.
       77  WS-SUB-4                PIC  9(04)     COMP-5.
       77  WS-SUB-5                PIC  9(04)     COMP-5.
       77  WS-SUB-6                PIC  9(04)     COMP-5.
       77  WS-S9                   PIC  9(04)     COMP-5.
       77  WS-IXS1                 PIC  9(04)     COMP-5.
       77  WS-IXS2                 PIC  9(04)     COMP-5.
       77  WS-IXS3                 PIC  9(04)     COMP-5.
       77  WS-IXS4                 PIC  9(04)     COMP-5.
       77  WS-MSUB                 PIC  9(04)     COMP-5.
       77  WS-PARKEY               PIC  9(04)     COMP-5.
       77  WS-NETKEY               PIC  9(06)     COMP-5.
       77  WS-RECKEY               PIC  9(06)     COMP-5.
       77  WS-WRKEY                PIC  9(04)     COMP-5.
       77  WS-RECOVER              PIC  9(06)     COMP-5.
       77  WS-SHARED               PIC  9(04)     COMP-5 VALUE 1.
       77  WS-TRANS                PIC  9(04)     COMP-5 VALUE 1.
       77  WS-NTP                  PIC  9(01).
       77  WS-LINES                PIC  9(02).
       77  WS-PAGE                 PIC  9(04)     COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-AGE                  PIC  X(01).
       77  WS-SEQ                  PIC  X(01).
       77  WS-CREDIT               PIC  X(01).
       77  WS-ZERO                 PIC  X(01).
       77  WS-SKIP                 PIC  X(01)     VALUE "N".
       77  WS-NUM                  PIC  Z9.
       77  WS-ADDR                 PIC  X(01).
       77  WS-ERROR                PIC  9(01).
       77  WS-ERROR1               PIC  9(01).
       77  WS-EXPAND               PIC  X(01)     VALUE " ".
       77  WS-COMP                 PIC  9(01).
       77  WS-IND                  PIC  9(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-HEAD                 PIC  X(01).
       77  WS-MESS                 PIC  X(02).
       77  WS-ACNO                 PIC  X(06).
       77  WS-S1                   PIC  9(04)     COMP-5.
       77  WS-S2                   PIC  9(04)     COMP-5.
       77  WS-S3                   PIC  9(04)     COMP-5.
       77  WS-S4                   PIC  9(04)     COMP-5.
       77  WS-S5                   PIC  9(04)     COMP-5.
       77  WS-S6                   PIC  9(04)     COMP-5.
       77  WS-S7                   PIC  9(04)     COMP-5.
       77  WS-S8                   PIC  9(04)     COMP-5.
       77  WS-ERR1                 PIC  X(22)     VALUE "Invalid account number".
       77  WS-ERR2                 PIC  X(09)     VALUE "No record".
       77  PRG-DEBT-LUP            PIC  X(12)     VALUE "DTP\DTPLOOK".
       77  TODAY-DDMMYY            PIC  9(08)     COMP-5.
       77  WS-USUB                 PIC  9(04)     COMP-5.
      /
       01  WS-DB-LINE.
           03  WS-TOP-LNE.
               05  WS-TCHR PIC X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
           03  WS-TOP-LNE2.
               05  WS-TCH  PIC X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
              05  FILLER      PIC  X(23).
        05  WS-WRKHD    PIC  X(11).
              05  FILLER      PIC  X(01).
        05  WS-WRKST    PIC  X(03).
              05  FILLER      PIC  X(01).
          03  WS-MID-LNE.
               05  WS-MCHR PIC X(01) OCCURS 80.
    03  WS-MDDT-LNE REDEFINES WS-MID-LNE.
        05  WS-MDT1     PIC  X(01).
        05  WS-MDT78    PIC  X(78).
        05  WS-MDT80    PIC  X(01).
          03  WS-MID-LNE2.
        05  FILLER      PIC  X(01) VALUE "³".
        05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
        05  FILLER      PIC  X(01) VALUE "³".
          03  WS-BOT-LNE.
               05  WS-BCHR PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC X(01).
               05  WS-F-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
           03  WS-BOT-LNE2.
               05  WS-BCH  PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE2.
               05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
                  07  WS-SRT-LINE
                           PIC X(50).
                  07  FILLER
                           PIC X(10).
               05  FILLER  PIC X(19).
           03  WS-MM-LINE.
               05  FILLER  PIC  X(01) VALUE X"B3".
               05  FILLER  PIC  X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

       COPY "WS.WS".

       01  WS-HELP.
    03  WS-MODULE       PIC  X(03) VALUE "DTP".
    03  WS-PROG        PIC  X(03) VALUE "031".

       01  W00-TRAN-CODES.
           03  W00-TRAN-TYPES  OCCURS 100.
               05  W00-TRAN-CODE PIC 9(02)   COMP-5.
               05  W00-ACTION  PIC 9(02)     COMP-5.
               05  W00-E-DESC  PIC X(12).
               05  W00-A-DESC  PIC X(12).

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W05-LIST.
           03  W05-OPT         PIC 9(03).
           03  W05-TYPES REDEFINES W05-OPT.
               05  W05-COMP    PIC 9(01).
               05  W05-INDV    PIC 9(01).
               05  W05-INT     PIC 9(01).

       01  W10-SPARES.
          03  W10-DATE     PIC X(10).
           03  W10-RDATE REDEFINES W10-DATE.
        05  W10-DTE     PIC 99/99/9999.

      *
      *    Date routines for accepting checking and formatting have
      *    been removed from each program and the DateCheck program
      *    will be called to handle these routines.
      *
      *    The following copy 'DateVariables' now includes the 'W12.WS'
      *    and 'W20.WS' variables and is passed to DateCheck in the CALL
      *    statement.
      *
       COPY DateVariables.

       COPY "W15.WS".

       01  W24-MONTHS.
           03  W24-MONTH-NAMES.
               05  FILLER      PIC  X(09) VALUE " January ".
               05  FILLER      PIC  X(09) VALUE " February".
               05  FILLER      PIC  X(09) VALUE "  March  ".
               05  FILLER      PIC  X(09) VALUE "  April  ".
               05  FILLER      PIC  X(09) VALUE "   May   ".
               05  FILLER      PIC  X(09) VALUE "   June  ".
               05  FILLER      PIC  X(09) VALUE "   July  ".
               05  FILLER      PIC  X(09) VALUE "  August ".
               05  FILLER      PIC  X(09) VALUE "September".
               05  FILLER      PIC  X(09) VALUE " October ".
               05  FILLER      PIC  X(09) VALUE " November".
               05  FILLER      PIC  X(09) VALUE " December".
           03  W24-EACH-MONTH REDEFINES W24-MONTH-NAMES.
               05  W24-MONTH   PIC  X(09) OCCURS 12.

       01  W25-CALCS.
           03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
               05  W25-WHOLE   PIC 9(05).
               05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(04).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).
           03  W25-TOTAL       PIC S9(07)V99 COMP-3.
           03  W25-VALUE       PIC S9(07)V99 COMP-3.

       01  W30-DBFOTO.
    03  W30-KEY.
              05  W30-AC      PIC  X(06).
        05  W30-CONT    PIC  X(08).
    03  W30-DESC        PIC  X(30).
          03  W30-DATE        PIC  9(08)     COMP-3.
    03  W30-RENTAL      PIC S9(07)V99  COMP-3.
    03  W30-TYPE        PIC  X(01).
    03  W30-FREQ        PIC  X(01).
    03  W30-ADATE       PIC  9(08)     COMP-3.
    03  W30-ESC        PIC  9(03)V99  COMP-3.
    03  W30-RATE        PIC S9(06)V999 COMP-3.
          03  W30-RDTE        PIC  9(08)     COMP-3.
          03  W30-STRDNG      PIC  9(08)     COMP-5.
          03  W30-LSTRDNG     PIC  9(08)     COMP-5.
          03  W30-READING     PIC  9(08)     COMP-5.
          03  W30-VALUE       PIC S9(07)V99  COMP-3.
          03  W30-TAX        PIC S9(05)V99  COMP-3.
    03  W30-TCODE       PIC  X(01)     COMP-X.
    03  W30-VG.
        05  W30-VAT     PIC  9(01).
    03  FILLER        PIC  X(15).

       COPY "W40.DBT".

       COPY "W40.WS".

       01  W45-TRAN.
           03  W45-CODE        PIC 9(02).
           03  W45-ENG         PIC X(12).
           03  W45-AFR         PIC X(12).
           03  W45-ACT         PIC X(01).

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W55-STATS.
           03  W55-H1         PIC  X(09).
           03  W55-H2         PIC  X(09).
           03  W55-H3         PIC  X(09).
           03  W55-H4         PIC  X(09).
           03  W55-H5         PIC  X(09).
           03  W55-H6         PIC  X(09).
           03  W55-MONTHS.
               05  W55-MTH1   PIC  9(02) COMP-5.
               05  W55-MTH2   PIC  9(02) COMP-5.
               05  W55-MTH3   PIC  9(02) COMP-5.
               05  W55-MTH4   PIC  9(02) COMP-5.
               05  W55-MTH5   PIC  9(02) COMP-5.
               05  W55-MTH6   PIC  9(02) COMP-5.
           03  W55-VALUES.
               05  W55-VAL1   PIC S9(09)V99 COMP-3.
               05  W55-VAL2   PIC S9(09)V99 COMP-3.
               05  W55-VAL3   PIC S9(09)V99 COMP-3.
               05  W55-VAL4   PIC S9(09)V99 COMP-3.
               05  W55-VAL5   PIC S9(09)V99 COMP-3.
               05  W55-VAL6   PIC S9(09)V99 COMP-3.

       01  W60-NME-ADD.
           03  W60-NAME.
               05  W60-NCHAR OCCURS 40 PIC X(01).
           03  W60-ADDRESS.
               05  W60-ACHAR OCCURS 60 PIC X(01).

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-DNO        PIC X(06) OCCURS 18.

       01  W75-TITLES.
           03  W75-DESCRIP.
               05  FILLER      PIC X(28)       VALUE
                           "Mr     Mnr    Mrs    Mev    ".
               05  FILLER      PIC X(28)       VALUE
                           "Miss   Mej    Ms     Ms     ".
               05  FILLER      PIC X(28)       VALUE
                           "Dr     Dr     Rev    Ds     ".
               05  FILLER      PIC X(28)       VALUE
                           "Prof   Prof   The HonSy Edel".
               05  FILLER      PIC X(28)       VALUE
                           "                            ".
           03  W75-CODES REDEFINES W75-DESCRIP.
               05  W75-TITLE  OCCURS 10.
                   07  W75-ETITLE  PIC X(07).
                   07  W75-ATITLE  PIC X(07).

       01  W80-ACCOUNT.
           03  W80-ACNO        PIC X(06).
           03  W80-NUMD REDEFINES W80-ACNO.
               05  W80-SERIAL  PIC 9(06).
           03  W80-TYPE        PIC 9(01).

       01  W85-BATCHES.
               05  W85-BATCH   PIC 9(05).

       01  W95-STM.
           03  W95-COMP        PIC X(40).

       01  W100-EDIT.
           03  W100-LBAL       PIC Z(06)9.99-.
           03  W100-LARR       PIC Z(06)9.99-.
           03  W100-BAL        PIC Z(08)9.99-.
           03  W100-CUR        PIC Z(08)9.99-.
           03  W100-30         PIC Z(08)9.99-.
           03  W100-60         PIC Z(08)9.99-.
           03  W100-90         PIC Z(08)9.99-.
           03  W100-120        PIC Z(08)9.99-.
           03  W100-INT        PIC Z(08)9.99.
           03  W100-CRL        PIC Z(06)9.
           03  W100-KEY        PIC Z(05)9.

       01  W200-MEMO.
           03  W200-MEM-FLDS.
               05  W200-MEM1   PIC  X(60).
               05  W200-MEM2   PIC  X(60).
               05  W200-MEM3   PIC  X(60).
               05  W200-MEM4   PIC  X(60).
               05  W200-MEM5   PIC  X(60).
               05  W200-MEM6   PIC  X(60).
               05  W200-MEM7   PIC  X(60).
               05  W200-MEM8   PIC  X(60).
               05  W200-MEM9   PIC  X(60).
               05  W200-MEM10  PIC  X(60).
               05  W200-MEM11  PIC  X(60).
               05  W200-MEM12  PIC  X(60).
           03  W200-MEM-FOC REDEFINES W200-MEM-FLDS.
               05  W200-MEM    PIC  X(60) OCCURS 12.

       LINKAGE SECTION.

       COPY "CHAIN.LS".

       COPY "FILE.IDS".

      /
       SCREEN SECTION.

       01  S01.
    03  LINE  2 COLUMN 25 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
    "DELETE RENTAL AGREEMENT DETAILS".
    03  LINE  4 COLUMN 10 VALUE "Account No     :".
    03  LINE  5 COLUMN 10 VALUE "Contract No    :".
    03  LINE  6 COLUMN 10 VALUE "Description    :".
    03  LINE  7 COLUMN 10 VALUE "Contract Date  :".
    03        COLUMN 38 VALUE "(".
    03        COLUMN 39 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "DD".
    03        COLUMN 41 VALUE "/".
    03        COLUMN 42 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "MM".
    03        COLUMN 44 VALUE "/".
    03        COLUMN 45 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "CCYY".
    03        COLUMN 49 VALUE ")".
    03  LINE  8 COLUMN 10 VALUE "Payment Amount :".
    03        COLUMN 38 VALUE "(Basic)".
          03        COLUMN 46 VALUE "Tax :".
    03        COLUMN 54 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "0".
    03        COLUMN 55 VALUE "=No VAT, ".
    03        COLUMN 64 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "1".
    03        COLUMN 65 VALUE "=Apply VAT".
    03  LINE  9 COLUMN 10 VALUE "Transact. Type :".
    03        COLUMN 38 VALUE "(".
    03        COLUMN 39 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "R".
    03        COLUMN 40 VALUE "ental/".
    03        COLUMN 46 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "S".
    03        COLUMN 47 VALUE "ervice Contract/".
    03        COLUMN 63 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "O".
    03        COLUMN 64 VALUE "ther)".
    03  LINE 10 COLUMN 10 VALUE "Frequency      :".
    03        COLUMN 38 VALUE "(".
    03        COLUMN 39 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "1".
    03        COLUMN 40 VALUE "/".
    03        COLUMN 41 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "2".
    03        COLUMN 42 VALUE "/".
    03        COLUMN 43 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "3".
    03        COLUMN 44 VALUE "/".
    03        COLUMN 45 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "4".
    03        COLUMN 46 VALUE "/".
    03        COLUMN 47 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "6".
    03        COLUMN 48 VALUE " monthly/".
    03        COLUMN 57 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "A".
    03        COLUMN 58 VALUE "nnual)".
    03  LINE 11 COLUMN 10 VALUE "Escalation Rate:".
    03        COLUMN 38 VALUE "(Annual escalation factor %)".
    03  LINE 12 COLUMN 10 VALUE "Reading        :".
    03        COLUMN 38 VALUE "(Reading at start of contract)".
    03  LINE 13 COLUMN 10 VALUE "Unit Rate      :".
    03        COLUMN 38 VALUE
     "(Monthly usage - unit/page rate)".
    03  LINE 14 COLUMN 10 VALUE "Transaction Cde:".
    03        COLUMN 38 VALUE
     "(Debtor transaction code to use)".
    03  LINE 15 COLUMN 10 VALUE "Next Payment   :".
    03        COLUMN 38 VALUE
     "(The next payment date)".

       01  S02.
    02  S02A.
    03  LINE  4 COLUMN 27 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC X(06) USING W30-AC AUTO.
    02  S02B.
    03  LINE  5 COLUMN 27 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC X(08) USING W30-CONT AUTO.
    02  S02C.
    03  LINE  6 COLUMN 27 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC X(30) USING W30-DESC AUTO.
    03  LINE  7 COLUMN 27 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC 99/99/9999 USING W30-DATE AUTO.
    03  LINE  8 COLUMN 27 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC Z(06)9.99 USING W30-RENTAL AUTO.
    03        COLUMN 52 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC 9(01) USING W30-VAT AUTO.
    03  LINE  9 COLUMN 27 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC X(01) USING W30-TYPE AUTO.
    03  LINE 10 COLUMN 27 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC X(01) USING W30-FREQ AUTO.
    03  LINE 11 COLUMN 27 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC ZZ9.99 USING W30-ESC
     BLANK ZERO AUTO.
    03  LINE 12 COLUMN 27 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC Z(08)9 USING W30-STRDNG
     BLANK ZERO AUTO.
    03  LINE 13 COLUMN 27 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC Z(04)9.999 USING W30-RATE
     BLANK ZERO AUTO.
    03  LINE 14 COLUMN 27 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC Z9 USING W30-TCODE
     BLANK ZERO AUTO.
    03  LINE 15 COLUMN 27 BACKGROUND-COLOR 5
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC 99/99/9999 USING W30-RDTE AUTO.

       COPY "DEBLUP.CRT".

       01  S-DEB-MEM.
           03  LINE  7 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7 VALUE
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E
      -    "".
       01  S-MEM.
           03  LINE  6 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G9.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G10.
           03  LINE  7 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G3.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7 VALUE
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E
      -    "".
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G3.
           03  LINE  8 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G14.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-HME-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G13.
           03  LINE  9 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 10 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 11 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 12 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 13 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 14 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 15 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 16 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 17 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 18 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 19 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 20 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 21 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G11.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G12.
       01  SA-MEM.
           03  LINE  9 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM1 AUTO.
           03  LINE 10 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM2 AUTO.
           03  LINE 11 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM3 AUTO.
           03  LINE 12 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM4 AUTO.
           03  LINE 13 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM5 AUTO.
           03  LINE 14 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM6 AUTO.
           03  LINE 15 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM7 AUTO.
           03  LINE 16 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM8 AUTO.
           03  LINE 17 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM9 AUTO.
           03  LINE 18 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM10 AUTO.
           03  LINE 19 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM11 AUTO.
           03  LINE 20 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM12 AUTO.

      /
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS.
       AA00-MAIN       SECTION.
       AA00-INIT.
           IF LS0-DBLEV < 4
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA00-INIT.
             PERFORM DA000.
             CLOSE DBFOTO RECOVER.

       AA49.
             EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
       AA50.
      PERFORM SAVE-SCREEN.
            MOVE 4   TO W44-FUNCTION.
            PERFORM SCREEN-CONTENTS.
      PERFORM DEBTOR-LOOKUP.
      PERFORM RESTORE-SCREEN.
       AA99.
             EXIT.

       COPY "DATE.CHK".

       COPY "FUNCTION.CRT".

       COPY "OPTION.CRT".

       COPY "LOCKED.REC".

       COPY "CLEAR.CRT".

      /
      *    ****    READ FILES ROUTINES
      *
       AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "DEBTOR.RD1".

       COPY "DBFOTO.RD".

       COPY "DEBMEM.RD".

       COPY "SHARED.RD".

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
       AD000             SECTION.

       COPY "CONTROL.WR".

       COPY "DBFOTO.WR".

       COPY "DEBTOR.WR".

       COPY "PARAM.WR".

       COPY "DEBTOR.LUP".

       COPY "APAC.HLP".

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
       AY000           SECTION.
       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.
       AY42.
            MOVE 42   TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
            MOVE DBF-RECORD1  TO REC-DBFOTO.
             GO TO AY50.
       AY49.
             MOVE 99             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE SPACES         TO REC-DETAIL.
             PERFORM AY50 THRU AY59.
             ADD 1               TO WS-TRANS.
           IF WS-RECOVER > 95
               CLOSE RECOVER
               OPEN OUTPUT RECOVER
               CLOSE RECOVER
               OPEN I-O RECOVER
               MOVE ZERO         TO WS-RECOVER.
             GO TO AY59.
       AY50.
            ADD 1   TO WS-RECOVER.
             MOVE WS-RECOVER     TO WS-RECKEY.
             MOVE WS-TRANS       TO REC-TRAN.
             WRITE REC-RECORD.
           IF WS-STATUS NOT = "00"
               DISPLAY "Write error Recovery file - Status " AT 2212
         WITH FOREGROUND-COLOR 6 HIGHLIGHT
   WS-STATUS
         WITH FOREGROUND-COLOR 7 HIGHLIGHT
               STOP RUN.
       AY59.
             EXIT.
      *
      *    ****    Start processing transaction
      *
       AY60.
            MOVE 1   TO WS-COUNT.
            MOVE 5   TO WS-SHARED.
            PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      *      MOVE SHR-STOCK  TO WS-STOCK.
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
            DISPLAY "WAITING" AT 5051
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12.
            COMMIT.
      ACCEPT WS-STIME FROM TIME.
      MOVE 400   TO WS-WAIT.
            PERFORM LOCK-REC-LOOP.
            DISPLAY SPACE AT 5051
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.
             GO TO AY60.

       AY61.
            MOVE "DB"   TO PAR-PROG(WS-USUB).
            MOVE LS-USER  TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
            MOVE 1   TO WS-NETKEY.
            PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             PERFORM AY39 THRU AY59.
            GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N P R O G R E S S
      *
       AY62.
    IF NOT (PAR-PROG(WS-SUB) = SPACES OR
     PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R O R   S T O C K   F I L E S
      *    B E I N G   U P D A T E D
      *
        IF NOT (PAR-PROG(WS-SUB) = SPACES)
           IF PAR-PROG(WS-SUB) = "DB" OR "DS"
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
    IF NOT (PAR-USR(WS-SUB) = LS-USER)
        IF WS-SUB < 24
     ADD 1  TO WS-SUB
     GO TO AY62.

       AY62-CHECK.
    IF WS-WAIT > ZERO
        COMMIT
        DISPLAY "Waiting" AT 5072
   WITH BACKGROUND-COLOR 3
        FOREGROUND-COLOR 14 BLINK
        ACCEPT WS-STIME FROM TIME
        PERFORM LOCK-REC-LOOP
        DISPLAY "Waiting" AT 5072
   WITH BACKGROUND-COLOR 3
        FOREGROUND-COLOR 14 BLINK
        GO TO AY60.
            DISPLAY SPACE AT 5072
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.

       AY63.
            MOVE WS-SUB  TO WS-USUB.
            GO TO AY61.

       AY70.
            MOVE 4   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *     NETWORK record 2.
      *
            PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
            PERFORM AY49 THRU AY59.
      MOVE 1   TO WS-USUB.

       AY72.
    IF NOT (PAR-USR(WS-USUB) = LS-USER)
        IF WS-USUB < 24
     ADD 1  TO WS-USUB
     GO TO AY72
        ELSE
     GO TO AY85.

       AY75.
            MOVE SPACES  TO PAR-PROG(WS-USUB)
              PAR-USR(WS-USUB).
    IF WS-USUB < 24
        ADD 1 WS-USUB  GIVING WS-SUB
    ELSE
        GO TO AY80.
    IF NOT (PAR-PROG(WS-SUB) = SPACES)
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

       AY85.
            COMMIT.

       AY999.
             EXIT.
      /
       CA155-GET-DEBTOR        SECTION 2.
       CA155-ACCOUNT.
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

       CA165-NAME              SECTION 2.
       CA165-EXTRACT.
             MOVE ZERO TO WS-ERROR.
             UNSTRING W60-NAME DELIMITED BY "*" OR "!"
               INTO W40-SURNAME W40-FNAME W40-SNAME
               OVERFLOW
                 MOVE 1 TO WS-ERROR.
       CA170-EXIT.
             EXIT.

       CA175-ADDRESS           SECTION 2.
       CA175-EXTRACT.
             MOVE 0 TO WS-ERROR.
             UNSTRING W60-ADDRESS DELIMITED BY "*"
               INTO W40-ADD1 W40-ADD2 W40-ADD3 W40-ADD4
               OVERFLOW
                 MOVE 1 TO WS-ERROR.
       CA180-EXIT.
             EXIT.

      /
      *    ****    D E B T O R  R E N T A L   M A I N T E N A N C E
      *
       DA000        SECTION 5.
       DA00.
             PERFORM ERASE-SCREEN.
            DISPLAY S01.
      INITIALIZE W30-DBFOTO.
      DISPLAY S02.
             MOVE SPACES         TO W80-ACNO.
       DA05.
      DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Help,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       " to do Debtor Lookup "
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       " to exit"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
            ACCEPT S02A.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W30-AC
       GO TO DA999
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
             PERFORM AA50
       MOVE W80-ACNO TO W30-AC
   WHEN OTHER
       CALL X"E5"
       GO TO DA05
        END-EVALUATE
              DISPLAY S02A
              IF W30-AC = SPACES
           GO TO DA05.
      PERFORM CLEAR-L50.
          IF W30-AC = SPACES
              GO TO DA999.
             MOVE 6              TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W30-AC
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
      MOVE W30-AC  TO DEB-ACNO W80-ACNO.
             PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR = 1
              MOVE WS-ERR2  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO DA05.
           IF WS-ERROR = 2
              MOVE WS-ERR1  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO DA05.
            MOVE DEB-NAME  TO W40-NAME.
           IF W80-TYPE = 1
               INSPECT W40-NAME REPLACING ALL "*" BY " "
               INSPECT W40-NAME REPLACING FIRST "!" BY " ".
            DISPLAY W40-NAME AT 0434
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO DA05.

       DA10.
      ACCEPT S02B.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W30-AC
       GO TO DA00
        END-EVALUATE.
    IF W30-CONT = SPACES
        GO TO DA00.
      MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W30-CONT
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
      MOVE W30-AC  TO DBF-AC.
      MOVE W30-CONT  TO DBF-CONT.
      PERFORM READ-DBFOTO THRU READ-DBFOTO-EXIT.
    IF WS-F-ERROR = 42
        MOVE "No record"  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO DA10.
      MOVE DBF-RECORD1  TO W30-DBFOTO.

       DA15.
      DISPLAY S02C.
            MOVE "Delete this contract Y/N?  [ ]"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
    IF NOT (WS-OPTION = "N" OR "Y")
        GO TO DA15.
          IF WS-OPTION = "N"
        GO TO DA10.
      PERFORM AY60 THRU AY999.
      MOVE 2   TO WS-ACTION.
      PERFORM AY42 THRU AY59.
      PERFORM DELETE-DBFOTO-REC THRU WRITE-DBFOTO-EXIT.
      PERFORM AY70 THRU AY999.
      GO TO DA00.

       DA999.
      EXIT.

      /
       ZA00-INIT       SECTION 9.
       ZA00-OPEN.
             PERFORM ZA55 THRU ZA60.
      MOVE LS-PARID  TO WS-PARID.
      MOVE LS-L-OR-N  TO W02-L-OR-N.
      MOVE WS-SYS-ID  TO W02-SYSID.
      MOVE LS-TODAY-DDMMYY
     TO TODAY-DDMMYY.
            PERFORM ERASE-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

            OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
            OPEN I-O DBFOTO.
           IF WS-STATUS = "05"
        CLOSE DBFOTO
        OPEN OUTPUT DBFOTO
        CLOSE DBFOTO
        OPEN I-O DBFOTO.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
     MOVE 42  TO WS-F-ERROR
                   GO TO ZA50.
       ZA02.
            MOVE 11   TO WS-PARKEY.
            MOVE 1   TO WS-SUB-2.
       ZA15-READ-TRANS.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE 1   TO WS-SUB-1.
       ZA20-TRAN-LOOP.
            MOVE PAR-T-CODE (WS-SUB-1)
     TO W00-TRAN-CODE (WS-SUB-2).
            MOVE PAR-E-DESC (WS-SUB-1)
     TO W00-E-DESC (WS-SUB-2).
            MOVE PAR-A-DESC (WS-SUB-1)
     TO W00-A-DESC (WS-SUB-2).
            MOVE PAR-ACTION (WS-SUB-1)
     TO W00-ACTION (WS-SUB-2).
            ADD 1   TO WS-SUB-2.
           IF WS-SUB-1 < 2
              ADD 1   TO WS-SUB-1
               GO TO ZA20-TRAN-LOOP.
           IF WS-PARKEY < 60
              ADD 1   TO WS-PARKEY
               GO TO ZA15-READ-TRANS.
       ZA25.
      MOVE 4   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-AGE  TO WS-AGE.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-COMPANY  TO W95-COMP.
            MOVE PAR-DMY  TO W12-TODAY.
            MOVE PAR-YMD  TO W12-T-YMD.
            MOVE 1   TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             GO TO ZA999-EXIT.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.

       ZA50.
          IF WS-F-ERROR = 42
              MOVE "RENTAL file"
     TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR 14.
             DISPLAY "Press any key to continue" AT 1012.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
       ZA51.
             EXIT PROGRAM.
       ZA55.
             MOVE 1              TO WS-S1.
            MOVE SPACES  TO WS-MID-LNE.
       ZA60.
             MOVE WS-G1          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8          TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
           IF WS-S1 < 80
               ADD 1             TO WS-S1
               GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G2          TO WS-TCHR (8)  WS-TCHR (49)
                                    WS-TCHR (61) WS-TCHR (72).
             MOVE WS-G3          TO WS-MCHR (8)  WS-MCHR (49)
                                    WS-MCHR (61) WS-MCHR (72)
              WS-MCHR (1)  WS-MCHR (80).
             MOVE WS-G4          TO WS-BCHR (8)  WS-BCHR (49)
                                    WS-BCHR (61) WS-BCHR (72).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
      MOVE LS-COMPANY  TO WS-TOP-COMP.
    IF LS-USER = LS-SYS-ID
        MOVE "SupervisorÄ"  TO WS-WRKHD
    ELSE
        MOVE "Workstation"  TO WS-WRKHD
        MOVE LS-USER    TO WS-WRKST.
       ZA200.
             DISPLAY "Files locked - Try later" AT 2312
                      WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
            DISPLAY "Press any key" AT 5012
              WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
            ACCEPT WS-OPTION AT 5026
      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
       ZA999-EXIT.
             EXIT.
      /
       ZB000       SECTION 9.

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
               MOVE W02-NETWORK  TO WS-FILE
               MOVE WS-NETKEY    TO WS-KEY
          ELSE
           IF WS-F-ERROR = 6
               MOVE W02-DEBTOR   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 15
               MOVE WS-PARID     TO WS-FILE
              MOVE WS-PARKEY  TO WS-KEY
           ELSE
           IF WS-F-ERROR = 27
               MOVE W02-DEBMEM   TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE DME-KEY  TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 37
              MOVE W02-SHARED  TO WS-FILE
              MOVE WS-SHARED  TO WS-KEY
           ELSE
          IF WS-F-ERROR = 42
              MOVE W02-DBFOTO  TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE DBF-KEY  TO WS-KEYX.
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
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
               DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
               DISPLAY "Please make a note of these details" AT 1812.
             STOP RUN.
