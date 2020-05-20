      $SET LINKCOUNT"512" GNT"DTPPRC.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  ******* *******   *******    ******    *
      *   ** **     **     **    ** **    **  ** **  **   **   *
      *   ** **     **     **    ** **    **  ** **  **        *
      *   ** **     **     ******* *******   *******   **        *
      *   ** **     **     ** **   **  **    **        *
      *   ** **     **     ** **   **   **   **   **   *
      *   *******      **     ** **   ** **   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *   D E B T O R S   E X P O R T P R I C E   L I S T   P R G    *
      *                                                                *
      *     VERSION 9.00.00 - August 2012          *
      *               *
      ******************************************************************
      *               *
      *  May 2006 - New field included in Accounts Receivable    *
      *     for recording number of outstanding Jobs     *
      *     linked to the account. Additional filler     *
      *     added for future expansion.         *
      *               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     DTPPRC.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   SEPTEMBER 2000.

                COPYRIGHT NOTICE: COPYRIGHT (C) 2000 - 2016
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

       COPY "DEBMEM.SL".

       COPY "DEBTOR.SL".

       COPY "DEPART.SL".

       COPY "PARAM.SL".

       COPY "PRICES.SL".

       COPY "STOCK.SL".

       DATA DIVISION.
       FILE SECTION.

       COPY "DEBMEM.FDE".

       COPY "DEBTOR.FDE".

       COPY "DEPART.FDE".

       COPY "PARAM.FDE".

       COPY "PRICES.FDE".

       COPY "STOCK.FDE".

      /
       WORKING-STORAGE SECTION.
       77  WS-CHECK    PIC X(18)  VALUE
      "aeWlimemnomLalismJ".
       77  WS-SUB    PIC 9(04)  COMP-5.
       77  WS-S1    PIC 9(04)  COMP-5.
       77  WS-S2    PIC 9(04)  COMP-5.
       77  WS-S3    PIC 9(04)  COMP-5.
       77  WS-S4    PIC 9(04)  COMP-5.
       77  WS-S5    PIC 9(04)  COMP-5.
       77  WS-S6    PIC 9(04)  COMP-5.
       77  WS-S7    PIC 9(04)  COMP-5.
       77  WS-S8    PIC 9(04)  COMP-5.
       77  WS-IXS    PIC 9(04)  COMP-5.
       77  WS-IXS1    PIC 9(04)  COMP-5.
       77  WS-IXS2    PIC 9(04)  COMP-5.
       77  WS-IXS3    PIC 9(04)  COMP-5.
       77  WS-IXS4    PIC 9(04)  COMP-5.
       77  WS-PARKEY    PIC 9(06)  COMP-5.
       77  WS-NETKEY    PIC 9(06)  COMP-5.
       77  WS-TRANS    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-KEYSTR    PIC 9(04)  COMP-5.
       77  WS-MONTHS    PIC S9(01)V99 COMP-3.
       77  WS-DISC    PIC 99V99  COMP-3.
       77  WS-PRC    PIC 9(01).
       77  WS-ETYPE    PIC X(01).
       77  WS-OPTION    PIC X(01).
       77  WS-PACKS    PIC X(01).
       77  WS-ADJ    PIC X(01).
       77  WS-ERROR    PIC 9(01).
       77  WS-AMEND    PIC 9(01)  VALUE 0.
       77  WS-CONREC    PIC 9(01) VALUE ZERO.
       77  WS-HYPH    PIC X(14)  VALUE "-".
       77  WS-COMP    PIC 9(01).
       77  WS-TYPE    PIC X(01).
       77  WS-ERR1    PIC X(22) VALUE "Invalid Account Number".
       77  WS-ERR2         PIC  X(09) VALUE "No Record".
       77  WS-PASSWORD    PIC X(08).
       77  WS-PRINT    PIC 9(01).
       77  WS-PASS    PIC X(08).
       77  PRG-DEBT-LUP            PIC  X(12)    VALUE "DTP\DTPLOOK".
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.
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
    03  WS-PROG        PIC  X(03) VALUE "PRC".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       COPY "W05.VAT".

       01  W10-PRICES.
    03  W10-SELL.
        05  W10-PRICE   PIC S9(08)V999 COMP-3.
        05  W10-PRICE-2 PIC S9(08)V999 COMP-3.
        05  W10-PRICE-3 PIC S9(08)V999 COMP-3.
        05  W10-PRICE-4 PIC S9(08)V999 COMP-3.
    03  W10-SELL-2 REDEFINES W10-SELL.
        05  W10-PRC     PIC S9(08)V999 COMP-3
          OCCURS 4.

       01  W10-CHECK-DATES.
          03  W10-DATE.
        05  W10-EDTE  PIC  99/99/9999.

      *
      *    Date routines for accepting checking and formatting have
      *    been removed from each program and the DateCheck program
      *    will be called to handle these routines.
      *
      *    The following copy 'DateVariables' now includes the 'W12.WS'
      *    and 'W20.WS' variables and is passed to DateCheck in the CALL
      *    statement.
      *
       COPY "DateVariables".

       01  W25-CALCS.
           03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
               05  W25-WHOLE   PIC 9(05).
               05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).

       COPY "W40.DBT".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W55-MONTHS.
    03  W55-MONTH-NAMES.
        05  FILLER      PIC  X(09) VALUE "January".
        05  FILLER      PIC  X(09) VALUE "February".
        05  FILLER      PIC  X(09) VALUE "March".
        05  FILLER      PIC  X(09) VALUE "April".
        05  FILLER      PIC  X(09) VALUE "May".
        05  FILLER      PIC  X(09) VALUE "June".
        05  FILLER      PIC  X(09) VALUE "July".
        05  FILLER      PIC  X(09) VALUE "August".
               05  FILLER      PIC  X(09) VALUE "September".
        05  FILLER      PIC  X(09) VALUE "October".
        05  FILLER      PIC  X(09) VALUE "November".
        05  FILLER      PIC  X(09) VALUE "December".
    03  W55-EACH-MONTH REDEFINES W55-MONTH-NAMES.
        05  W55-MONTH   PIC  X(09) OCCURS 12.

       01  W60-MTH-YR.
    03  W60-MMYY.
        05  W60-MY     PIC  X(01) OCCURS 14.
    03  W60-CCYY.
        05  W60-CY     PIC  X(01) OCCURS 4.

       01  W60-NME-ADD.
          03  W60-ADDRESS.
               05  W60-ACHAR OCCURS 70 PIC X(01).
           03  W60-ADDR REDEFINES W60-ADDRESS.
               05  W60-ADD     PIC X(60).
               05  FILLER      PIC X(10).
           03  W60-ADDRESS1.
               05  W60-PAD     PIC X(60).
               05  FILLER      PIC X(10).

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
    03  W75-DNO       PIC X(06) OCCURS 18.

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
                   07  W75-ETITLE  PIC X(07).
                   07  W75-ATITLE  PIC X(07).

       01  W80-ACCOUNT.
           03  W80-ACNO        PIC X(06).
           03  W80-TYPE        PIC 9(01).

       01  W80-EDIT.
           03  W80-CNO        PIC 9(04).
           03  W80-V1.
              05  W80-S9V2   PIC Z(08)9.99-.
           03  W80-V2.
               05  W80-S5V2   PIC Z(04)9.99-.
           03  W80-DTE.
              05  W80-DATE   PIC Z9/99/9999.
          03  W80-LIM.
              05  W80-7      PIC Z(06)9.
           03  W80-DEL.
               05  W80-2N      PIC 9(02).

       01  W90-BAL             PIC S9(07)V99 COMP-3.

       01  W95-COMP            PIC X(40).

       01  W100-EDIT.
           03  W100-1.
               05  W100-N      PIC  9(01).
    03  W100-2.
               05  W100-N2     PIC  9(02).
    03  W100-4.
        05  W100-N4     PIC  9(04).
    03  W100-7.
        05  W100-NO2    PIC  Z(06)9.
    03  W100-S7.
        05  W100-NO3    PIC  -(07)9.
    03  W100-S7M REDEFINES W100-S7.
        05  W100-NO4    PIC  -(07)9.
    03  W100-S9V2.
        05  W100-V1     PIC  -(09)9.99.
           03  W100-9V2.
        05  W100-V2     PIC  -(09)9.99.
    03  W100-S5V2.
        05  W100-V3     PIC  -(05)9.99.
    03  W100-DATE.
               05  W100-DTE    PIC  9(06).
          03  W100-LBAL       PIC -(07)9.99.
          03  W100-LARR       PIC Z(06)9.99.
           03  W100-CRL        PIC Z(06)9.

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

       01  S-PROG.
          03  LINE  2 COLUMN 28 FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "DEBTOR PRICE LIST PROGRAM".

       01  S01.
    03  LINE  6 COLUMN  3 VALUE "Account No.".
          03  LINE  7 COLUMN  3 VALUE "Name       ".
          03  LINE  8 COLUMN 10 VALUE "Postal Address".
           03          COLUMN 49 VALUE "Alternate/Delivery Address".
          03  LINE  9 COLUMN  3 VALUE "Line 1".
          03        COLUMN 42 VALUE "Line 1".
          03  LINE 10 COLUMN  3 VALUE "     2".
          03        COLUMN 42 VALUE "     2".
          03  LINE 11 COLUMN  3 VALUE "     3".
          03        COLUMN 42 VALUE "     3".
          03  LINE 12 COLUMN  3 VALUE "     4".
          03        COLUMN 42 VALUE "     4".
          03  LINE 13 COLUMN  3 VALUE "P/Code".
          03        COLUMN 42 VALUE "P/Code".
          03  LINE 15 COLUMN  3 VALUE "Telephone numbers".
    03        COLUMN 54 VALUE "Fax".

       01  S02.
          03  LINE  7 COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC X(40) USING W40-NAME AUTO.
          03  LINE  9 COLUMN 10 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC X(30) USING W40-ADD1 AUTO.
          03  LINE 10 COLUMN 10 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC X(30) USING W40-ADD2 AUTO.
          03  LINE 11 COLUMN 10 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC X(30) USING W40-ADD3 AUTO.
          03  LINE 12 COLUMN 10 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC X(30) USING W40-ADD4 AUTO.
          03  LINE 13 COLUMN 10 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC 9(04) USING W40-POST AUTO.
          03  LINE  9 COLUMN 49 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC X(30) USING W40-PADD1 AUTO.
          03  LINE 10 COLUMN 49 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC X(30) USING W40-PADD2 AUTO.
          03  LINE 11 COLUMN 49 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC X(30) USING W40-PADD3 AUTO.
          03  LINE 12 COLUMN 49 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC X(30) USING W40-PADD4 AUTO.
          03  LINE 13 COLUMN 49 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC 9(04) USING W40-PPOST AUTO.
          03  LINE 15 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC X(14) USING W40-TEL AUTO.
          03        COLUMN 38 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC X(14) USING W40-HTEL AUTO.
          03        COLUMN 59 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC X(14) USING W40-FAX AUTO.

       01  S03.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC X(06) USING W80-ACNO AUTO.

       01  S07.
           03  LINE 16 COLUMN  2 VALUE "Exporting:".

       01  S08.
    03  LINE 16 COLUMN 13 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(06) FROM DEB-ACNO.
    03        COLUMN 20 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(40) FROM DEB-NAME.

       01  S09.
    03  LINE 11 COLUMN 42 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 1
     PIC  Z9/99/9999 USING W22-DTE3 AUTO.

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
           "   M E M O / R E M A R K S / S P E C I F I C A T I O N S
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
       AA000-MAIN        SECTION.
       AA000-INIT.
           IF LS0-DBLEV < 4
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
           IF WS-STATUS NOT = "00"
               PERFORM OPEN-ERROR.
             PERFORM BA000.
             GO TO AZ000-EOJ.

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

       COPY "FUNCTION.CRT".

       COPY "LOCKED.RC2".

       COPY "CLEAR.CRT".

       COPY "OPTION.CRT".

      *
      *    ****    G E T   T H E   P R I C E S  F I L E   I D
      *
       AA100           SECTION.
       AA101.
             DISPLAY "Keyin the <pathname>/<filename> " AT 2202.
      DISPLAY "PRICES file :" AT 2302.
             MOVE ZERO           TO WS-ERROR.

       AA105.
      ACCEPT W02-PRICES AT 2316
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE 9  TO WS-ERROR
       GO TO AA199
   WHEN OTHER
       CALL X"E5"
       GO TO AA105
        END-EVALUATE.
      MOVE 64   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W02-PRICES
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
             MOVE 1              TO WS-LENGTH.
      DISPLAY W02-PRICES AT 2316
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5 UPDATE AUTO.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO AA105.
      PERFORM CLEAR-L50.
      OPEN OUTPUT PRICES.
           IF WS-STATUS = "00"
        PERFORM AA600
               GO TO AA199.
      MOVE "Invalid file name"
     TO WS-ERR-MES.
            PERFORM ERROR-MESSAGE.
      GO TO AA105.

       AA199.
             EXIT.
      *
      *    ****    S E C U R I T Y   V I O L A T I O N
      *
       AA500              SECTION.
       AA501.
             DISPLAY "Insufficient security" AT 0412
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Press ENTER to continue" AT 0612
                      WITH FOREGROUND-COLOR 14.
             ACCEPT WS-OPTION AT 0636 WITH FOREGROUND-COLOR 15 AUTO.
       AA505.
             EXIT.
      *
      *    ****   H E A D E R S
      *
       AA600    SECTION.
       AA601.
      MOVE SPACES  TO PRC-CODE.

       AA605.
      MOVE W95-COMP  TO PRC-COMP.
      MOVE "Price List as at"
     TO PRC-LIST.
      MOVE W12-DAY  TO PRC-DAY.
      MOVE W55-MONTH(W12-MONTH)
     TO W60-MMYY.
      MOVE W12-CCYY  TO W60-CCYY.
      MOVE 9   TO WS-S1.
      MOVE 1   TO WS-S2.

       AA610.
    IF W60-MY(WS-S1) = SPACE
        SUBTRACT 1  FROM WS-S1
        GO TO AA610.
      ADD 2   TO WS-S1.

       AA615.
      MOVE W60-CY(WS-S2)  TO W60-MY(WS-S1).
    IF WS-S2 < 4
        ADD 1   TO WS-S1 WS-S2
        GO TO AA615.
      MOVE W60-MMYY  TO PRC-MONTH.
            MOVE W12-DAY  TO W22-DD3.
            MOVE W12-MONTH  TO W22-MM3.
      MOVE W12-CENT  TO W22-CC3.
            MOVE W12-YEAR  TO W22-YY3.
    IF W22-MM3 < 12
        ADD 1   TO W22-MM3
    ELSE
        MOVE 1   TO W22-MM3
        ADD 1   TO W22-YY3.
      PERFORM SAVE-SCREEN-3.
      MOVE 11   TO SHADE-ROW.
      MOVE 31   TO SHADE-COL.
      MOVE 22   TO SHADE-WIDTH.
      MOVE 2   TO SHADE-LINES.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1029
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 1
       "¿" WITH FOREGROUND-COLOR 1 HIGHLIGHT
         BACKGROUND-COLOR 1.
      DISPLAY "³" AT 1129 WITH FOREGROUND-COLOR 0
          BACKGROUND-COLOR 1
       "Expiry date           "
        WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1
       "³" WITH FOREGROUND-COLOR 1 HIGHLIGHT
         BACKGROUND-COLOR 1.
      DISPLAY "À" AT 1229 WITH FOREGROUND-COLOR 0
          BACKGROUND-COLOR 1
       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
        WITH FOREGROUND-COLOR 1 HIGHLIGHT
      BACKGROUND-COLOR 1.
      PERFORM SCREEN-SHADOW.

       AA620.
      ACCEPT S09.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE 9  TO WS-ERROR
       GO TO AA699
   WHEN OTHER
       CALL X"E5"
       GO TO AA620
        END-EVALUATE.
      MOVE W22-DTE3  TO W10-EDTE.
      PERFORM CHECK-DATE.
    IF WS-ERROR = 1
        MOVE "Date invalid"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO AA620.
      PERFORM RESTORE-SCREEN-3.
      MOVE W22-DD3  TO W20-DAY.
      MOVE W22-MM3  TO W20-MONTH.
      MOVE W22-CC3  TO W20-CENT.
      MOVE W22-YY3  TO W20-YEAR.
      MOVE W20-DTE  TO PRC-EXPIRE.
            MOVE 3   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-PACKS  TO PRC-PACKS WS-PACKS.
      MOVE DEB-ACNO  TO PRC-ACNO.
      MOVE DEB-NAME  TO PRC-DEBTOR.
          IF DEB-TYPE = 1
              INSPECT PRC-DEBTOR REPLACING ALL "*" BY " "
              INSPECT PRC-DEBTOR REPLACING FIRST "!" BY " ".
      PERFORM WRITE-PRICES THRU WRITE-PRICES-EXIT.

       AA699.
             EXIT.
      /
      *    ****    READ FILES ROUTINES
      *
       AC000-READ              SECTION.

       COPY "DEBMEM.RD".

       COPY "DEBTOR.RD".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "STOCK.RD".

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
       AD000-WRITE             SECTION.

       WRITE-PRICES.
            WRITE PRC-RECORD1.
          IF WS-STAT1 NOT = "0"
               MOVE 99           TO WS-F-ERROR
              PERFORM WRITE-ERROR.
      
       WRITE-PRICES-EXIT.
             EXIT.

       COPY "DATE.CHK".

       COPY "DEBTOR.LUP".

       COPY "APAC.HLP".

      /
       AZ000-END               SECTION.
       AZ000-EOJ.
      MOVE ZERO   TO WS-S1.
      CLOSE STOCK.

       AZ010.
             EXIT PROGRAM.
      /
      *    ****    P R O G R A M   M E N U / C O N T R O L
      *
       BA000           SECTION.
       BA00.
      PERFORM ERASE-SCREEN.
      DISPLAY S-PROG.

       BA05.
      DISPLAY S01.
             MOVE SPACES         TO W80-ACNO.
      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S
      *
       BA10.
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
            ACCEPT S03 AT 0615.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W80-ACNO
       GO TO BA999
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA50
   WHEN OTHER
       CALL X"E5"
       GO TO BA10
        END-EVALUATE
        DISPLAY S03 AT 0615
               IF W80-ACNO = SPACES
           GO TO BA10.
            DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
      PERFORM CLEAR-L50.
           IF W80-ACNO = SPACES
              GO TO BA999.
             MOVE 6              TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W80-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
             MOVE W80-ACNO       TO DEB-ACNO.
             PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR = 2
              MOVE WS-ERR1  TO WS-ERR-MES
        PERFORM ERROR-MESSAGE
              GO TO BA10.
           IF WS-ERROR = 1
              MOVE WS-ERR2  TO WS-ERR-MES
        PERFORM ERROR-MESSAGE
              GO TO BA10.
      UNLOCK DEBTOR.
            MOVE DEB-NAME  TO W40-NAME.
            MOVE DEB-ADDRESS  TO W60-ADDRESS.
            MOVE DEB-PADDR  TO W60-ADDRESS1.
            MOVE DEB-POST  TO W40-POST.
             MOVE DEB-PPOST      TO W40-PPOST.
            MOVE SPACES  TO W40-ADDRESS1 W40-PADDRESS.
             MOVE DEB-H-TEL      TO W40-HTEL.
             MOVE DEB-B-TEL      TO W40-TEL.
             MOVE DEB-FAX        TO W40-FAX.
    IF W80-TYPE > 1
        MOVE DEB-B-TEL  TO W40-TEL
    ELSE
        MOVE DEB-H-TEL  TO W40-TEL.
          IF W80-TYPE > 1
              GO TO BA15.
            INSPECT W40-NAME REPLACING ALL "*" BY " ".
            INSPECT W40-NAME REPLACING FIRST "!" BY " ".

       BA15.
            PERFORM CA175-ADDRESS.
             PERFORM CA180-ADDRESS.
      DISPLAY S02.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO BA00.
      PERFORM AA100.
    IF WS-ERROR = 9
        GO TO BA999.
      DISPLAY "Extracting:" AT 1803.
            MOVE "!"   TO STK-CODE.
      PERFORM START-AT-STOCK-CODE THRU READ-STOCK-EXIT.
    IF WS-F-ERROR = 22
        MOVE "No data to extract"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BA999.

       BA20.
      PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
    IF WS-F-ERROR = 22
        GO TO BA30.
    IF STK-IND > 2
        GO TO BA20.
      DISPLAY STK-CODE AT 1815 WITH FOREGROUND-COLOR 3 HIGHLIGHT.
      MOVE ZERO   TO WS-DISC.
            MOVE STK-LDG  TO DPT-CODE.
            PERFORM READ-DEPART THRU READ-DEPART-EXIT.
          IF DEB-DISC > ZERO
              MOVE DPT-DSETT(DEB-DISC)
                                 TO WS-DISC.
    IF NOT VALID-PRICE
        MOVE 1   TO DEB-PRICE.
      MOVE DEB-PRICE  TO WS-PRC.
    IF DEB-PRICE = "1"
        MOVE STK-SELL  TO W10-PRICE
        MOVE STK-SELL-2  TO W10-PRICE-2
        MOVE STK-SELL-3  TO W10-PRICE-3
        MOVE STK-SELL-4  TO W10-PRICE-4
    ELSE
    IF DEB-PRICE = "2"
        MOVE STK-CASH  TO W10-PRICE
        MOVE STK-CASH-2  TO W10-PRICE-2
        MOVE STK-CASH-3  TO W10-PRICE-3
        MOVE STK-CASH-4  TO W10-PRICE-4
    ELSE
        MOVE STK-WSALE  TO W10-PRICE
        MOVE STK-WSALE-2  TO W10-PRICE-2
        MOVE STK-WSALE-3  TO W10-PRICE-3
        MOVE STK-WSALE-4  TO W10-PRICE-4.
      MOVE STK-CODE  TO PRC-CODE.
      MOVE DPT-DESC  TO PRC-DEPART.
      MOVE STK-ACODE  TO PRC-ACODE.
      MOVE STK-XREF  TO PRC-XREF.
      MOVE STK-DESC  TO PRC-DESC.
      MOVE STK-DESC2  TO PRC-DESC2.
      MOVE STK-QUANT  TO PRC-QUANT.
      MOVE 1   TO WS-S7.

       BA25.
      MOVE STK-UNT(WS-S7) TO PRC-UNT(WS-S7).
      MOVE STK-PR(WS-S7)  TO PRC-PR(WS-S7).
    IF WS-DISC = ZERO
        MOVE W10-PRC(WS-S7)
     TO PRC-PRC(WS-S7)
    ELSE
        COMPUTE PRC-PRC(WS-S7) ROUNDED = W10-PRC(WS-S7) -
              (W10-PRC(WS-S7) * WS-DISC / 100.00000).
    IF WS-S7 < 4
        ADD 1   TO WS-S7
        GO TO BA25.
      MOVE STK-TAX  TO PRC-TAX.
    IF PRC-TAX = ZERO
        MOVE ZERO  TO PRC-VAT
    ELSE
        MOVE W05-VAT(PRC-TAX)
     TO PRC-VAT.
      PERFORM WRITE-PRICES THRU WRITE-PRICES-EXIT.
      GO TO BA20.

       BA30.
      CLOSE PRICES.
      MOVE "Price List extracted - Press any key"
     TO WS-ERR-MES.
      PERFORM OPT-MESSAGE.

       BA999.
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
      *
       CA180-ADDRESS     SECTION 2.
       CA180.
             UNSTRING W60-ADDRESS1 DELIMITED BY "*"
                                 INTO W40-PADD1
                                      W40-PADD2
                                      W40-PADD3
                                      W40-PADD4
             OVERFLOW
                 MOVE 1          TO WS-ERROR.
       CA180-EXIT.
             EXIT.
      /
       ZA000-INIT        SECTION 8.
       ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
      MOVE LS-PARID  TO WS-PARID.
      MOVE LS-L-OR-N  TO W02-L-OR-N.
      MOVE WS-SYS-ID  TO W02-SYSID.
      MOVE LS-TODAY-DDMMYY
     TO TODAY-DDMMYY.
      MOVE LS-USUB  TO WS-USUB.
            PERFORM ERASE-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DMY        TO W12-TODAY.
            MOVE PAR-COMPANY  TO W95-COMP.

       ZA05.
            OPEN I-O STOCK.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
        IF IDX-CORRUPT
                   MOVE 22       TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
               MOVE 22           TO WS-F-ERROR
        IF WS-STATUS = "39"
           GO TO ZA50
        ELSE
        IF NOT (WS-STATUS = "41")
           PERFORM OPEN-ERROR.
            MOVE 5   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
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
            GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
       ZA50.
    IF WS-F-ERROR = 22
        MOVE "STOCK file" TO WS-FILE.
            DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR 14.
             DISPLAY "Press any key to continue" AT 1012.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
       ZA51.
             EXIT PROGRAM.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
             MOVE 1              TO WS-S1.
             MOVE SPACES         TO WS-MID-LNE.
       ZA60.
            MOVE WS-G1   TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
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
       ZB000-ERROR        SECTION 8.

       OPEN-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
             GO TO DISPLAY-FILE-NAME.

       READ-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
             GO TO DISPLAY-FILE-NAME.

       WRITE-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
             GO TO DISPLAY-FILE-NAME.

       DISPLAY-FILE-NAME.
           IF WS-F-ERROR = 6
               MOVE W02-DEBTOR TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
           ELSE
41850   IF WS-F-ERROR = 15
               MOVE WS-PARID     TO WS-FILE
               MOVE WS-PARKEY    TO WS-KEY
          ELSE
           IF WS-F-ERROR = 27
               MOVE W02-DEBMEM   TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE DME-KEY  TO WS-KEYX
          ELSE
           IF WS-F-ERROR = 99
              MOVE W02-PRICES  TO WS-FILE
               MOVE ZERO         TO WS-KEY.
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
