      $SET LINKCOUNT"512" GNT"DTPPROF.GNT"
      ******************************************************************
      *                                                                *
      *  *******  ******** *******  *******  *******   ******  ******* *
      *  **    **    **    **  ** **   ** **    ** **    ** **      *
      *  **    **    **    **  ** **   ** **    ** **    ** **      *
      *  **    **    **    *******  *******  *******  **    ** *****   *
      *  **    **    **    **     **      **    ** **    ** **      *
      *  **    **    **    **     **      **    ** **    ** **      *
      *  *******     **    **     **      **    **  ******  **      *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R   S A L E S   P E R   A C C O U N T        *
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
       PROGRAM-ID.     DTPPROF.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   June 1992.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1992 - 2016
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

       COPY "CARDEX.SL".

       COPY "CONTROL.SL".

       COPY "PARAM.SL".

       COPY "DEBTOR.SL".

       COPY "DEBMEM.SL".

       COPY "DEBTOT.SL".

       COPY "DEPART.SL".

       DATA DIVISION.
       FILE SECTION.

       COPY "CARDEX.FDE".

       COPY "CONTROL.FDE".

       COPY "PARAM.FDE".

       COPY "DEBTOR.FDE".

       COPY "DEBMEM.FDE".

       COPY "DEBTOT.FDE".

       COPY "DEPART.FDE".

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
       77  WS-COPYRIGHT    PIC X(18)  VALUE
      "aeWlimemnomLalismJ".
       77  WS-S1    PIC 9(04)   COMP-5.
       77  WS-S2    PIC 9(04)   COMP-5.
       77  WS-S3    PIC 9(04)   COMP-5.
       77  WS-S4    PIC 9(04)   COMP-5.
       77  WS-S5    PIC 9(04)   COMP-5.
       77  WS-S6    PIC 9(04)   COMP-5.
       77  WS-S7    PIC 9(04)   COMP-5.
       77  WS-S8    PIC 9(04)   COMP-5.
       77  WS-IXS1    PIC 9(04)   COMP-5.
       77  WS-IXS2    PIC 9(04)   COMP-5.
       77  WS-IXS3    PIC 9(04)   COMP-5.
       77  WS-IXS4    PIC 9(04)   COMP-5.
       77  WS-MSUB    PIC 9(04)   COMP-5.
       77  WS-PARKEY    PIC 9(04)   COMP-5.
       77  WS-DEPKEY    PIC 9(04)   COMP-5.
       77  WS-DEBKEY    PIC 9(06)   COMP-5.
       77  WS-NETKEY    PIC 9(06)   COMP-5.
       77  WS-WRKEY    PIC 9(04)   COMP-5.
       77  WS-ETYPE    PIC X(01).
       77  WS-CARDEX    PIC X(01).
       77  WS-NTP    PIC 9(01).
       77  WS-LINES    PIC 9(02).
       77  WS-PAGE    PIC 9(04)   COMP-5.
       77  WS-OPTION    PIC X(01).
       77  WS-EXTR    PIC X(01).
       77  WS-ACT    PIC X(01).
       77  WS-STK    PIC X(01).
       77  WS-DEB    PIC X(01).
       77  WS-CREDIT    PIC X(01).
       77  WS-SKIP    PIC X(01).
       77  WS-NUM    PIC Z9.
       77  WS-ADDR    PIC X(01).
       77  WS-ERROR    PIC 9(02)  COMP-X VALUE 0.
       77  WS-ERROR1    PIC 9(01).
       77  WS-EXPAND    PIC X(01)  VALUE " ".
       77  WS-CHAR1    PIC X(01).
       77  WS-CHAR2    PIC X(01).
       77  WS-COMP    PIC 9(01).
       77  WS-STM    PIC 9(01).
       77  WS-TYPE    PIC X(01).
       77  WS-HEAD    PIC X(01).
       77  WS-BATCH    PIC 9(05).
       77  WS-MESS    PIC X(02).
       77  WS-ACNO    PIC X(06).
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
    03  WS-PROG        PIC  X(03) VALUE "PRF".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-PRINTER-DETAILS.
    03  W02-PRINTER     PIC  X(12).
          03  W02-PGE-LENGTH  PIC  9(03).
          03  W02-PRN-LENGTH  PIC  9(03).
    03  W02-LINAGE      PIC  9(03).
    03  W02-PRN-STATUS  PIC  X(01) VALUE "C".
      *
      *    ****    D  =  Detail line
      *     C  =  Condensed print
      *     E  =  Expanded print
      *     H  =  Header line
      *     X  =  Cancel expanded print
      *     1  =  10 Characters per inch
      *     2  =  12 Characters per inch
      *     3  =  17 Characters per inch
      *     6  =  6 Lines per inch
      *     8  =  8 Lines per inch
      *
    03  W02-PRN-TYPE    PIC  X(01).
    03  W02-PRN-LINE    PIC  X(136).

    03  R-SL1 REDEFINES W02-PRN-LINE.
        05  R-SLA       PIC  X(78).
        05  FILLER      PIC  X(58).
    03  R-SL2 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(58).
        05  R-SLB       PIC  X(78).
          03  REP-LINE1 REDEFINES W02-PRN-LINE.
              05  REP-DETAIL1 PIC  X(136).
          03  REP-LINE2 REDEFINES W02-PRN-LINE.
              05  REP-DETAIL2 PIC  X(80).
          03  COM-LINE1 REDEFINES W02-PRN-LINE.
              05  COM-PAGE    PIC  X(06).
              05  COM-P-NO    PIC  Z(03)9.
              05  FILLER      PIC  X(04).
              05  COM-COMPANY PIC  X(30).
              05  COM-HEAD    PIC  X(18).
              05  FILLER      PIC  X(02).
              05  COM-DATE    PIC  X(06).
              05  COM-DTE     PIC  Z9/99/9999.

          03  SLD-LINE1 REDEFINES W02-PRN-LINE.
              05  SLD-DET     PIC  X(132).

          03  SLD-LINE6 REDEFINES W02-PRN-LINE.
              05  SLD-PAGE    PIC  X(06).
              05  SLD-P-NO    PIC  Z(03)9.
              05  FILLER      PIC  X(24).
              05  SLD-RESTART.
           07  SLD-COMPANY  PIC  X(40).
           07  SLD-HEAD     PIC  X(24).
              05  FILLER      PIC  X(17).
              05  SLD-DATE    PIC  X(06).
              05  SLD-DTE     PIC  Z9/99/9999.

    03  SLD-LINE7 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(06).
        05  SLD-CO      PIC  X(30).
        05  SLD-EXHEAD  PIC  X(24).
        05  FILLER      PIC  X(08).

    03  SLD-LINE8 REDEFINES W02-PRN-LINE.
        05  SLD-H1      PIC  X(08).
        05  SLD-H2      PIC  X(88).

    03  SLD-LINE8B REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(24).
        05  SLD-H3      PIC  X(19).
        05  SLD-H4      PIC  X(13).
        05  SLD-H5      PIC  X(17).
        05  SLD-H6      PIC  X(17).
        05  SLD-H7      PIC  X(06).

    03  SLD-LINE9 REDEFINES W02-PRN-LINE.
        05  SLD-AC      PIC  X(06).
        05  FILLER      PIC  X(02).
        05  SLD-NME     PIC  X(40).
        05  FILLER      PIC  X(48).

    03  SLD-LINE10 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(24).
        05  SLD-MONTH.
     07  SLD-MNTH  PIC  X(10).
     07  SLD-YEAR  PIC  X(04).
        05  FILLER      PIC  X(02).
        05  SLD-COST    PIC  Z(09)9.99-.
        05  FILLER      PIC  X(02).
        05  SLD-SELL    PIC  Z(09)9.99-.
        05  FILLER      PIC  X(02).
        05  SLD-PROF    PIC  Z(09)9.99-.
        05  FILLER      PIC  X(02).
        05  SLD-GP      PIC  Z(03)9.99-.

       01  W05-LIST.
          03  W05-OPT        PIC  9(03).
           03  W05-TYPES REDEFINES W05-OPT.
              05  W05-COMP    PIC  9(01).
              05  W05-INDV    PIC  9(01).
              05  W05-INT     PIC  9(01).

       COPY "W05.VAT".

       01  W08-EXPD.
    03  W08-CHAR        PIC  X(06).
          03  W08-DEC REDEFINES W08-CHAR
          PIC  X(01) COMP-X OCCURS 6.

       COPY "W10.STK".

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

       COPY "W15.WS".

       01  W23-PERIOD.
           03  FILLER          PIC X(17) VALUE "For the period : ".
    03  W23-DTE1        PIC Z9/99/9999.
           03  FILLER          PIC X(04) VALUE " to ".
    03  W23-DTE2        PIC Z9/99/9999.

       01  W25-CALCS.
          03  W25-RESULT      PIC  9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
              05  W25-WHOLE   PIC  9(05).
              05  W25-DECIMAL PIC  9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
        05  W25-KEY     PIC  9(04).
        05  W25-SUB     PIC  9(01).
        05  FILLER      PIC  9(02).
          03  W25-TOTAL       PIC S9(09)V99 COMP-3.
          03  W25-VALUE       PIC S9(09)V99 COMP-3.
    03  W25-TRANVAL     PIC S9(09)V99 COMP-3.
          03  W25-G-TOTAL     PIC S9(09)V99 COMP-3.
          03  W25-G-VALUE     PIC S9(09)V99 COMP-3.

       01  W30-MONTHS.
    03  W30-CURR        PIC  9(02).
    03  W30-START       PIC  9(02).
    03  W30-END        PIC  9(02).
           03  W30-MONTH-NAMES.
              05  W30-ENG.
           07  FILLER  PIC  X(09) VALUE "January  ".
     07  FILLER  PIC  X(09) VALUE "February ".
     07  FILLER  PIC  X(09) VALUE "March   ".
     07  FILLER  PIC  X(09) VALUE "April    ".
     07  FILLER  PIC  X(09) VALUE "May      ".
     07  FILLER  PIC  X(09) VALUE "June     ".
     07  FILLER  PIC  X(09) VALUE "July     ".
     07  FILLER  PIC  X(09) VALUE "August   ".
     07  FILLER  PIC  X(09) VALUE "September".
     07  FILLER  PIC  X(09) VALUE "October  ".
     07  FILLER  PIC  X(09) VALUE "November ".
     07  FILLER  PIC  X(09) VALUE "December ".
           03  W30-MONTH REDEFINES W30-MONTH-NAMES
                PIC  X(09)  OCCURS 12.

       01  W32-MONTHS.
          03  W32-MONTH-NAMES.
              05  W32-ENG     PIC X(36)     VALUE
                               "JanFebMarAprMayJunJulAugSepOctNovDec".
          03  W32-MONTH REDEFINES W32-MONTH-NAMES
                               PIC X(03)     OCCURS 12.

       01  W35-EDIT.
    03  W35-COST        PIC  Z(08)9.99- BLANK WHEN ZERO.
    03  W35-SELL        PIC  Z(08)9.99- BLANK WHEN ZERO.
    03  W35-PROF        PIC  Z(08)9.99- BLANK WHEN ZERO.
    03  W35-GP        PIC  Z(02)9.99- BLANK WHEN ZERO.

       COPY "W40.DBT".

       COPY "W40.WS".

       01  W45-TRAN.
          03  W45-CODE        PIC  9(02).
          03  W45-ENG        PIC  X(12).
          03  W45-AFR        PIC  X(12).
          03  W45-ACT        PIC  X(01).

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
              05  W60-NCHAR OCCURS 40 PIC  X(01).
           03  W60-ADDRESS.
              05  W60-ACHAR OCCURS 60 PIC  X(01).

       01  W75-KEYS.
    03  W75-S       PIC  9(02) COMP-5.
    03  W75-S1       PIC  9(02) COMP-5.
    03  W75-S2       PIC  9(02) COMP-5.
    03  W75-DNO       PIC  X(06) OCCURS 18.
    03  W75-ITEMS OCCURS 18.
        05  W75-CODE   PIC  X(14).
        05  W75-ITM    PIC  X(03).
        05  W75-SELL   PIC  9(07)V99 COMP-3.
        05  W75-VSELL  PIC  9(07)V99 COMP-3.
        05  W75-CASH   PIC  9(07)V99 COMP-3.
        05  W75-VCASH  PIC  9(07)V99 COMP-3.
        05  W75-WSALE  PIC  9(07)V99 COMP-3.
        05  W75-VWSALE PIC  9(07)V99 COMP-3.

       01  W80-ACCOUNT.
          03  W80-ACNO        PIC  X(06).
           03  W80-NUMD REDEFINES W80-ACNO.
              05  W80-SERIAL  PIC  9(06).
          03  W80-TYPE        PIC  9(01).

       01  W95-STM.
           03  W95-COMP        PIC  X(40).

       01  W100-HEAD.
          03  FILLER        PIC  X(29)    VALUE
              "ACCOUNTS RECEIVABLE LISTING -".
          03  W100-DESC       PIC  X(11).

       01  W100-EDIT.
          03  W100-LBAL       PIC  Z(06)9.99-.
          03  W100-LARR       PIC  Z(06)9.99-.
          03  W100-BAL        PIC  Z(08)9.99-.
          03  W100-CUR        PIC  Z(08)9.99-.
          03  W100-30        PIC  Z(08)9.99-.
          03  W100-60        PIC  Z(08)9.99-.
          03  W100-90        PIC  Z(08)9.99-.
          03  W100-120        PIC  Z(08)9.99-.
          03  W100-INT        PIC  Z(08)9.99.
          03  W100-CRL        PIC  Z(06)9.
          03  W100-KEY        PIC  Z(05)9.
          03  W100-V2.
               05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.

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
          03  LINE  2 COLUMN 22 FOREGROUND-COLOR 7
     HIGHLIGHT VALUE
               "DEBTORS - PROFIT FOR SELECTED PERIOD".

       01  S04.
    03  LINE  6 COLUMN 12 VALUE "Accounts Required:".
    03  LINE  8 COLUMN 12 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "A".
    03        COLUMN 13 VALUE "ll or ".
    03        COLUMN 19 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "S".
    03        COLUMN 20 VALUE "elective accounts - press ".
    03        COLUMN 46 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "Esc".
    03        COLUMN 49 VALUE "ape to exit".

       01  S05D.
    03  LINE  4 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 6
         VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
    03  LINE  5 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 6
         VALUE "³     Purchases for Period.    ³".
    03  LINE  6 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 6
         VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
    03  LINE  7 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 6
         VALUE "³ Starting Month:              ³".
    03  LINE  8 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 6
         VALUE "³ Ending Month  :              ³".
    03  LINE  9 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 6
         VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S05A.
    03  LINE  7 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC 99 USING W30-START AUTO.
    03  LINE  8 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC 99 USING W30-END AUTO.

       01  S10D.
    03  LINE  4 COLUMN  3 VALUE "Account No.".

       01  S10.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC X(06) USING W80-ACNO AUTO.

       COPY "DEBLUP.CRT".

       COPY "ERROR.CRT".

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

       01  S-PROF.
    03  LINE  4 COLUMN 7 FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 7 VALUE
        "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÂÄÄÄÄÄÄÄ¿".
    03  LINE  5 COLUMN 7 FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 7 VALUE
        "³    Month     ³  Total Cost ³Total Selling³   Total GP
      -        " ³  GP % ³".
    03  LINE  6 COLUMN 7 FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 7 VALUE
        "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÅÄÄÄÄÄÄÄ´".
    03  LINE 19 COLUMN 7 FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 7 VALUE
        "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÅÄÄÄÄÄÄÄ´".
    03  LINE 20 COLUMN 7 FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 7 VALUE
        "³    TOTAL     ³             ³             ³
      -        " ³       ³".
    03  LINE 21 COLUMN 7 FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 7 VALUE
        "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÁÄÄÄÄÄÄÄÙ".

      /
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS.
       AA00-MAIN       SECTION.
       AA00-INIT.
      *    ************************************************************
      *    ****    Debtor security level must be greater than 3    ****
      *    ************************************************************
           IF LS0-DBLEV < 4
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA00-INIT.
           IF NOT(WS-CARDEX = "Y")
               MOVE "Sales logging not selected" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM BA000.
             CLOSE CARDEX DEBTOT.
      *    ************************************************************
      *    **  M U S T  T H E  P R I N T E R  B E   C L O S E D ? **
      *    ************************************************************
    IF W02-PRN-STATUS = "O"
        MOVE "C"   TO WS-COMMAND
        PERFORM CALL-PRINTUTL.

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

       COPY "DATE.CNV".

       COPY "FUNCTION.CRT".

       COPY "OPTION.CRT".

       COPY "LOCKED.REC".

       COPY "CLEAR.CRT".

       COPY "ABORT.PRN".

       COPY "PRINTUTL.AP5".

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³        R E A D  F I L E S   R O U T I N E S        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AC000-READ        SECTION.

       COPY "CARDEX.RD".

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "DEBTOR.RD1".

       COPY "DEBTOT.RD".

       COPY "DEBMEM.RD".

       COPY "DEPART.RD".

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   & W R I T E   F I L E S R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AD000   SECTION.

       COPY "DEBTOT.WR".

       COPY "DEBTOR.ILU".

       COPY "APAC.HLP".

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
            MOVE ZERO   TO WS-ERROR.
             UNSTRING W60-NAME DELIMITED BY "*" OR "!"
           INTO W40-SURNAME
          W40-FNAME
          W40-SNAME
               OVERFLOW
         MOVE 1   TO WS-ERROR.

       CA170-EXIT.
             EXIT.

       CA175-ADDRESS           SECTION 2.
       CA175-EXTRACT.
            MOVE 0   TO WS-ERROR.
             UNSTRING W60-ADDRESS DELIMITED BY "*"
           INTO W40-ADD1 W40-ADD2
          W40-ADD3 W40-ADD4
               OVERFLOW
         MOVE 1   TO WS-ERROR.

       CA180-EXIT.
             EXIT.
      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  P R O F I T   F O R   S P E C I F I E D A C C O U N T  ³
      *    ³  F O R   T H E   S E L E C T E D P E R I O D        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA000         SECTION 58.
       BA0.
    IF WS-EXTR = "Y"
        GO TO BA15.

       BA00.
            PERFORM ERASE-SCREEN.
      DISPLAY S-PROG.

       BA05.
      MOVE 5   TO SHADE-ROW.
      MOVE 27   TO SHADE-COL.
      MOVE 30   TO SHADE-WIDTH.
      MOVE 5   TO SHADE-LINES.
      DISPLAY S05D.
      PERFORM SCREEN-SHADOW.
      MOVE W12-MONTH  TO W30-END W30-CURR.
    IF W12-MONTH = 12
        MOVE 1   TO W30-START
    ELSE
        ADD 1 W12-MONTH  GIVING W30-START.

       BA10.
      DISPLAY "Press " AT 5052
        WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
       "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
    BACKGROUND-COLOR 3
       "ape to Exit"
        WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
      ACCEPT S05A.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BA499
   WHEN OTHER  CALL X"E5"
       GO TO BA10
        END-EVALUATE.
    IF (W30-START < 01 OR > 12) OR
       (W30-END < 01 OR > 12)
        MOVE "Months must be 01 to 12"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BA10.
      DISPLAY W30-MONTH(W30-START) AT 0747
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      DISPLAY W30-MONTH(W30-END) AT 0847
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO BA10.
      PERFORM CLEAR-L50.
      MOVE W12-TODAY  TO W15-START W15-END.
      MOVE W30-START  TO W15-SMNTH.
      MOVE W30-END  TO W15-EMNTH.
      MOVE 01   TO W15-SDAY.
      MOVE 31   TO W15-EDAY.
      *    ************************************************************
      *    ****   I S S T A R T   M O N T H -   PREVIOUS YEAR  ****
      *    ************************************************************
    IF W30-START > W30-CURR
        SUBTRACT 1  FROM W15-SCCYY
        IF W30-END > W30-CURR
     SUBTRACT 1  FROM W15-ECCYY
        END-IF
    ELSE
    IF W30-END < W30-START
        SUBTRACT 1  FROM W15-SCCYY.
    IF W15-EMNTH = 04 OR 06 OR 09 OR 11
        MOVE 30   TO W15-EDAY
    ELSE
    IF W15-EMNTH = 02
      *    ************************************************************
      *    ****    C H E C K   F O R   L E A P  Y E A R
      *    ************************************************************
              DIVIDE W15-ECCYY BY 4 GIVING W25-RESULT
              IF W25-DECIMAL = 0
     MOVE 29  TO W15-EDAY
        ELSE
     MOVE 28  TO W15-EDAY.
      *    ************************************************************
      *    ****    SWITCH START DATE TO YEAR/MONTH/DAY
      *    ************************************************************
      MOVE W15-SDAY  TO W15-SDD.
      MOVE W15-SMNTH  TO W15-SMM.
      MOVE W15-SCENT  TO W15-SCC.
      MOVE W15-SYEAR  TO W15-SYY.
      *    ************************************************************
      *    ****    SWITCH END DATE TO YEAR/MONTH/DAY
      *    ************************************************************
      MOVE W15-EDAY  TO W15-EDD.
      MOVE W15-EMNTH  TO W15-EMM.
      MOVE W15-ECENT  TO W15-ECC.
      MOVE W15-EYEAR  TO W15-EYY.

       BA15.
      PERFORM BA00.
      DISPLAY S04.

       BA20.
            ACCEPT WS-OPTION AT 0861
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO BA499
   WHEN OTHER
       CALL X"E5"
       GO TO BA20
        END-EVALUATE.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-OPTION = "A" OR "S")
        GO TO BA20.
      MOVE WS-OPTION  TO WS-ACT.
      MOVE ZERO   TO WS-PAGE
        W25-VALUE
        W25-TOTAL.
      PERFORM SAVE-SCREEN-3.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *   **  *   *   * *   *  *  **  *   *   *     * *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *   *  **   *     *   *  *  *  **   *   *     * *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** * *
      *
      PERFORM OPEN-PRINTER.
      MOVE "P"   TO WS-COMMAND.
      *    ************************************************************
      *    ****   T E S T   I F   O U T P U T T O   P R I N T E R
      *    O R S P O O L   F I L E
      *    ************************************************************
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE 2   TO W02-PRN-TYPE
        DISPLAY "Debtor account:" AT 1412
        PERFORM CALL-PRINTUTL.
      PERFORM RESTORE-SCREEN-3.
    IF WS-ACT = "A"
        GO TO BA90.
      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  P R O F I T   F O R   S P E C I F I E D A C C O U N T  ³
      *    ³  F O R   T H E   S E L E C T E D P E R I O D        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA25.
            PERFORM RESTORE-SCREEN-3.
      DISPLAY S10D.

       BA30.
            DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Help,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             " to do Account Lookup"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       BA35.
            ACCEPT S10 AT 0415.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W80-ACNO
       GO TO BA40
         WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
             PERFORM AA50
   WHEN OTHER
       CALL X"E5"
       GO TO BA30
        END-EVALUATE
              DISPLAY S10 AT 0415
               IF W80-ACNO = SPACES
           GO TO BA30.

       BA40.
            PERFORM CLEAR-L50.
          IF W80-ACNO = SPACES
              GO TO BA499.
      MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W80-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.

       BA45.
            PERFORM CLEAR-L50.
             PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR = 1
              MOVE "No record"  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BA30.
           IF WS-ERROR = 2
              MOVE "Invalid account"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BA30.
      *    ************************************************************
      *    ****    D I S P L A Y   D E B T O R   D E T A I L S
      *    ************************************************************
            MOVE DEB-NAME  TO W40-NAME.
           IF W80-TYPE = 1
               INSPECT W40-NAME REPLACING ALL "*" BY " "
               INSPECT W40-NAME REPLACING FIRST "!" BY " ".
            DISPLAY W40-NAME AT 0315
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        PERFORM CLEAR-L50
        GO TO BA30.

       BA50.
      MOVE W80-ACNO  TO CRD-AC.
      MOVE W15-S-YMD  TO CRD-DTE.
      PERFORM START-AT-CRDX-ACNT THRU READ-CARDEX-EXIT.
          IF WS-F-ERROR = 43
        MOVE "No details for period requested"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BA499.
      CLOSE DEBTOT.
      OPEN OUTPUT DEBTOT.
      CLOSE DEBTOT.
      OPEN I-O DEBTOT.
      DISPLAY "Extracting Data:" AT 1112.
      MOVE SPACES  TO TOT-AC   TOT-MTH.
      MOVE ZERO   TO TOT-YEAR TOT-TCOST
        TOT-MNTH TOT-TSELL
        TOT-EMTH.

       BA55.
            PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
    IF WS-F-ERROR = 43
        GO TO BA60.
    IF NOT (CRD-AC = W80-ACNO)
        GO TO BA60.
    IF CRD-DTE < W15-S-YMD
        GO TO BA55.
    IF CRD-DTE > W15-E-YMD
        GO TO BA60.
      DISPLAY CRD-AC AT 1129
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
       CRD-DTE AT 1136
       WITH FOREGROUND-COLOR 6 HIGHLIGHT.
      MOVE CRD-AC  TO TOT-AC.
      MOVE CRD-DTE  TO W20-DTE.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    Move 4 digit year CCYY to totals file record        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      MOVE W20-CNYR  TO TOT-YEAR.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³      Move 2 digit month to totals file record        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      MOVE W20-MONTH  TO TOT-MNTH.
      PERFORM READ-DEBTOT THRU READ-DEBTOT-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Check if there has been a record created for this account ³
      *    ³ for the selected year and month.          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
    IF WS-F-ERROR = "55"
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³   C R E A T E   N E W   R E C O R D        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
        MOVE W30-MONTH(TOT-MNTH)
     TO TOT-MTH
        MOVE W15-EMM  TO TOT-EMTH
        IF (CRD-QNT < ZERO)
     COMPUTE TOT-TCOST = (CRD-COST * CRD-QNT) * -1
     COMPUTE TOT-TSELL = (CRD-SELL * CRD-QNT) * -1
        ELSE
     MULTIPLY CRD-COST BY CRD-QNT
         GIVING TOT-TCOST
     MULTIPLY CRD-SELL BY CRD-QNT
         GIVING TOT-TSELL
        END-IF
        PERFORM WRITE-DEBTOT THRU WRITE-DEBTOT-EXIT
    ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     U P D A T E   E X I S T I N G   R E C O R D        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
        IF (CRD-QNT < ZERO)
     COMPUTE TOT-TCOST = TOT-TCOST +
         ((CRD-COST * CRD-QNT) * -1)
     COMPUTE TOT-TSELL = TOT-TSELL +
         ((CRD-SELL * CRD-QNT) * -1)
        ELSE
     COMPUTE TOT-TCOST = TOT-TCOST + (CRD-COST * CRD-QNT)
     COMPUTE TOT-TSELL = TOT-TSELL + (CRD-SELL * CRD-QNT)
        END-IF
        PERFORM REWRITE-DEBTOT THRU WRITE-DEBTOT-EXIT.
      GO TO BA55.

       BA60.
      EXIT.

       BA62.
      DISPLAY WS-BLNK78 AT 1102 WITH FOREGROUND-COLOR 3.
      CLOSE DEBTOT.
      OPEN I-O DEBTOT.
      DISPLAY S-PROF.
      MOVE 0707   TO CPOS.
      PERFORM BA70-CRT-EDGE UNTIL CLIN = 19.
      MOVE W80-ACNO  TO TOT-AC.
      MOVE ZERO   TO TOT-YEAR  TOT-MNTH
        W25-VALUE W25-TOTAL.
      PERFORM START-AT-DEBTOT-AC THRU READ-DEBTOT-EXIT.
          IF WS-F-ERROR = 55
        GO TO BA75.
      MOVE 0700   TO CPOS.

       BA65.
      PERFORM READ-DEBTOT-NEXT THRU READ-DEBTOT-EXIT.
          IF WS-F-ERROR = 55
        GO TO BA75.
      MOVE TOT-TCOST  TO W35-COST.
      MOVE TOT-TSELL  TO W35-SELL.
      SUBTRACT TOT-TCOST FROM TOT-TSELL
     GIVING W10-VSELL.
      COMPUTE W10-ADJUST ROUNDED = (W10-VSELL / TOT-TSELL) *
        100.000.
      MOVE W10-VSELL  TO W35-PROF.
      MOVE W10-ADJUST  TO W35-GP.
      ADD TOT-TCOST  TO W25-VALUE.
      ADD TOT-TSELL  TO W25-TOTAL.
      MOVE 08   TO CCOL.
      DISPLAY TOT-MTH AT CPOS
       WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
      MOVE 18   TO CCOL.
      DISPLAY TOT-YEAR AT CPOS
       WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
      MOVE 23   TO CCOL.
      DISPLAY W35-COST AT CPOS
       WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
      MOVE 37   TO CCOL.
      DISPLAY W35-SELL AT CPOS
       WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
      MOVE 51   TO CCOL.
      DISPLAY W35-PROF AT CPOS
       WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
      MOVE 65   TO CCOL.
      DISPLAY W35-GP AT CPOS
       WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
      ADD 1   TO CLIN.
      GO TO BA65.

       BA70-CRT-EDGE.
      DISPLAY "³              ³             ³             ³
      -       "       ³       ³" AT CPOS
        WITH BACKGROUND-COLOR 7
      FOREGROUND-COLOR 7 HIGHLIGHT.
            ADD 1   TO CLIN.

       BA75.
      MOVE 2023   TO CPOS.
      MOVE W25-VALUE  TO W35-COST.
      MOVE W25-TOTAL  TO W35-SELL.
      SUBTRACT W25-VALUE FROM W25-TOTAL
     GIVING W10-VSELL.
      COMPUTE W10-ADJUST ROUNDED = (W10-VSELL / W25-TOTAL) *
        100.000.
      MOVE W10-VSELL  TO W35-PROF.
      MOVE W10-ADJUST  TO W35-GP.
      DISPLAY W35-COST AT CPOS
       WITH BACKGROUND-COLOR 7
     FOREGROUND-COLOR 5.
      MOVE 37   TO CCOL.
      DISPLAY W35-SELL AT CPOS
       WITH BACKGROUND-COLOR 7
     FOREGROUND-COLOR 5.
      MOVE 51   TO CCOL.
      DISPLAY W35-PROF AT CPOS
       WITH BACKGROUND-COLOR 7
     FOREGROUND-COLOR 5.
      MOVE 65   TO CCOL.
      DISPLAY W35-GP AT CPOS
       WITH BACKGROUND-COLOR 7
     FOREGROUND-COLOR 5.

       BA80.
      DISPLAY "Press any key to continue" AT 2322
        WITH FOREGROUND-COLOR 6 HIGHLIGHT.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BA85.
            MOVE "Change 'A'ccount,'P'eriod or 'E'xit"
     TO WS-ERR-MES.
      MOVE "A"   TO WS-OPTION.
            PERFORM OPT-MESSAGE.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-OPTION = "A" OR "E" OR "P")
        GO TO BA85.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BA499
        END-EVALUATE.
    IF WS-OPTION = "A"
        GO TO BA15.
    IF WS-OPTION = "P"
        GO TO BA00.
      GO TO BA499.

       BA90.
      MOVE " !"   TO DEB-NAME.
             PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.
    IF WS-F-ERROR = 6
        MOVE "No DEBTORS on file"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BA499.
      DISPLAY "Extracting Data:" AT 1112.

       BA95.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              GO TO BA499.
            PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
        GO TO BA100.
    IF NOT (WS-PRINTER = "S")
        DISPLAY DEB-ACNO AT 1428
         WITH FOREGROUND-COLOR 7 HIGHLIGHT
       BACKGROUND-COLOR 5.
      MOVE DEB-ACNO  TO W80-ACNO
        CRD-AC.
      MOVE W15-S-YMD  TO CRD-DTE.
      PERFORM START-AT-CRDX-ACNT THRU READ-CARDEX-EXIT.
          IF WS-F-ERROR = 43
        GO TO BA95.
      MOVE SPACES  TO TOT-AC   TOT-MTH.
      MOVE ZERO   TO TOT-YEAR TOT-TCOST
        TOT-MNTH TOT-TSELL
        TOT-EMTH.
      PERFORM BA55 THRU BA60.
      GO TO BA95.
      *    ************************************************************
      *    ****    E X T R A C T   C O M P L E T E
      *    ************************************************************
       BA100.
      CLOSE DEBTOT.
      OPEN I-O DEBTOT.
      MOVE "     !"  TO TOT-AC.
      MOVE ZERO   TO TOT-YEAR
        TOT-MNTH
        WS-PAGE
        W25-G-TOTAL
        W25-G-VALUE.
      MOVE SPACES  TO W80-ACNO.
      PERFORM START-AT-DEBTOT-AC THRU READ-DEBTOT-EXIT.
          IF WS-F-ERROR = 55
        MOVE "No data extracted"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BA499.

       BA105.
      MOVE "H"   TO W02-PRN-TYPE.
             MOVE SPACES         TO REP-DETAIL1.
            MOVE "PAGE:"  TO SLD-PAGE.
            MOVE "DATE:"  TO SLD-DATE.
            ADD 1   TO WS-PAGE.
            MOVE W12-TODAY  TO SLD-DTE.
            MOVE WS-PAGE  TO SLD-P-NO.
      *    ************************************************************
      *    ****   T E S T   I F   O U T P U T T O   P R I N T E R
      *    O R S P O O L   F I L E
      *    ************************************************************
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO SLD-LINE1.
      *    ************************************************************
      *    ****   T E S T   I F   O U T P U T T O   P R I N T E R
      *    O R S P O O L   F I L E
      *    ************************************************************
    IF NOT (WS-PRINTER = "S")
        MOVE "E"   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO SLD-LINE1.
      MOVE SPACES  TO SLD-LINE1.
            MOVE W95-COMP  TO SLD-CO.
            MOVE " - G.PROF. PER ACCOUNT"
     TO SLD-EXHEAD.
      MOVE "H"   TO W02-PRN-TYPE.
      *    ************************************************************
      *    ****   T E S T   I F   O U T P U T T O   P R I N T E R
      *    O R S P O O L   F I L E
      *    ************************************************************
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO SLD-LINE1.
      *    ************************************************************
      *    ****   T E S T   I F   O U T P U T T O   P R I N T E R
      *    O R S P O O L   F I L E
      *    ************************************************************
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE "X"   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
              MOVE 2   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE "Ac.No."  TO SLD-H1.
      MOVE "Account Name" TO SLD-H2.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE "Month     Year"
     TO SLD-H3.
      MOVE "Total Cost"  TO SLD-H4.
      MOVE "Total Selling"
     TO SLD-H5.
      MOVE "Gross Profit" TO SLD-H6.
      MOVE "G.P.%"  TO SLD-H7.
      PERFORM CALL-PRINTUTL.
      MOVE ALL "="  TO SLD-LINE8.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE "D"   TO W02-PRN-TYPE.

       BA110.
      PERFORM READ-DEBTOT-NEXT THRU READ-DEBTOT-EXIT.
          IF WS-F-ERROR = 55
        PERFORM BA501
        GO TO BA115.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              GO TO BA499.
    IF NOT (TOT-AC = W80-ACNO)
        PERFORM BA500.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
        MOVE 99   TO WS-ADVANCE
        GO TO BA999.
      ADD TOT-TCOST  TO W25-VALUE.
      ADD TOT-TSELL  TO W25-TOTAL.
      MOVE TOT-MTH  TO SLD-MNTH.
      MOVE TOT-YEAR  TO SLD-YEAR.
      MOVE TOT-TCOST  TO SLD-COST.
      MOVE TOT-TSELL  TO SLD-SELL.
      SUBTRACT TOT-TCOST FROM TOT-TSELL
     GIVING W25-TRANVAL.
      MOVE W25-TRANVAL  TO SLD-PROF.
      COMPUTE SLD-GP ROUNDED = (W25-TRANVAL / TOT-TSELL) *
        100.000.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        PERFORM BA105.
      GO TO BA110.
      *    ************************************************************
      *    ****   P R I N T   G R A N D   T O T A L S
      *    ************************************************************
       BA115.
      MOVE "GRAND TOTAL"  TO SLD-MONTH.
      MOVE W25-G-VALUE  TO SLD-COST.
      MOVE W25-G-TOTAL  TO SLD-SELL.
      SUBTRACT W25-G-VALUE FROM W25-G-TOTAL
     GIVING W25-TRANVAL.
      MOVE W25-TRANVAL  TO SLD-PROF.
      COMPUTE SLD-GP ROUNDED = (W25-TRANVAL / W25-G-TOTAL) *
        100.000.
      MOVE 99   TO WS-ADVANCE.
      MOVE "E"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.

       BA499.
      EXIT.
      *    ************************************************************
      *    ****   I S T H I S   T H E   F I R S T   A C C O U N T
      *    ************************************************************
       BA500        SECTION 58.
       BA501.
    IF W80-ACNO = SPACES
        GO TO BA505.
      ADD W25-VALUE  TO W25-G-VALUE.
      ADD W25-TOTAL  TO W25-G-TOTAL.
      *    ************************************************************
      *    ****   P R I N T   A C C O U N T   T O T A L S
      *    ************************************************************
      MOVE "Account Total"
     TO SLD-MONTH.
      MOVE W25-VALUE  TO SLD-COST.
      MOVE W25-TOTAL  TO SLD-SELL.
      SUBTRACT W25-VALUE FROM W25-TOTAL
     GIVING W25-TRANVAL.
      MOVE W25-TRANVAL  TO SLD-PROF.
      COMPUTE SLD-GP ROUNDED = (W25-TRANVAL / W25-TOTAL) *
        100.000.
    IF W02-LINAGE < (W02-PRN-LENGTH - 3)
        MOVE 2   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        PERFORM BA105.

       BA505.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE ZERO   TO W25-VALUE W25-TOTAL.
      MOVE TOT-AC  TO W80-ACNO SLD-AC.
      PERFORM CA155-GET-DEBTOR.
    IF NOT (WS-ERROR = 0)
        MOVE "INVALID ACCOUNT NUMBER"
     TO SLD-NME
    ELSE
        MOVE DEB-NAME  TO SLD-NME
        IF DEB-TYPE = 1
           INSPECT SLD-NME REPLACING FIRST "*" BY ","
           INSPECT SLD-NME REPLACING FIRST "*" BY " "
           INSPECT SLD-NME REPLACING FIRST "!" BY " ".
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.

       BA999.
      EXIT.

      /
       ZA00-INIT       SECTION 90.
       ZA00-OPEN.
             PERFORM ZA55 THRU ZA60.
      MOVE LS-PARID  TO WS-PARID.
      MOVE LS-L-OR-N  TO W02-L-OR-N.
      MOVE WS-SYS-ID  TO W02-SYSID.
      MOVE LS-TODAY-DDMMYY
     TO TODAY-DDMMYY W12-TODAY.
      MOVE LS-USUB  TO WS-USUB.
            PERFORM ERASE-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ D o   n o t   D I S P L A Y   E R R O R   M E S S A G E S ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      MOVE 44   TO USER-NUMBER.
      MOVE 1   TO USER-SETTING.
      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

            MOVE 3   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-CRDX  TO WS-CARDEX.
            OPEN I-O CARDEX.
    IF WS-STATUS = "05"
        CLOSE CARDEX
        OPEN OUTPUT CARDEX
        CLOSE CARDEX
        OPEN I-O CARDEX.
          IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
           GO TO ZA49
               ELSE
        IF IDX-CORRUPT
     MOVE 43  TO WS-F-ERROR
     GO TO ZA50.
           IF WS-STATUS NOT = "00"
              MOVE 43   TO WS-F-ERROR
        IF WS-STATUS = "39"
           GO TO ZA50
        ELSE
        IF NOT (WS-STATUS = "41")
           PERFORM OPEN-ERROR.
            OPEN I-O DEBTOT.
    IF WS-STATUS = "00"
        GO TO ZA30.
    IF WS-STATUS = "05"
        CLOSE DEBTOT
        OPEN OUTPUT DEBTOT
        CLOSE DEBTOT
        OPEN I-O DEBTOT.

       ZA01.
          IF RUNTIME-ERROR
              IF FLE-LOCKED
           GO TO ZA200
              ELSE
              IF FLE-LIMIT
           GO TO ZA49
        ELSE
        IF IDX-CORRUPT
     CLOSE DEBTOT
     OPEN OUTPUT DEBTOT
     CLOSE DEBTOT
     OPEN I-O DEBTOT.
          IF WS-STATUS NOT = "00"
              MOVE 55   TO WS-F-ERROR
        IF WS-STATUS = "39"
           CLOSE DEBTOT
     OPEN OUTPUT DEBTOT
     CLOSE DEBTOT
     OPEN I-O DEBTOT
        ELSE
           PERFORM OPEN-ERROR.

       ZA02.
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

       ZA25.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-COMPANY  TO W95-COMP.
            MOVE PAR-DMY  TO W12-TODAY.
            MOVE PAR-YMD  TO W12-T-YMD.
           IF PAR-STMINV = "S" OR "X"
              MOVE "N"   TO WS-HEAD
           ELSE
              MOVE "Y"   TO WS-HEAD.
            MOVE 1   TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
            GO TO ZA999-EXIT.

       ZA30.
      MOVE "Use the existing extracted data Y/N? [ ]"
     TO WS-ERR-MES.
            PERFORM OPT-MESSAGE.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE 9  TO WS-ERROR
       GO TO ZA999-EXIT
        END-EVALUATE.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-OPTION = "Y" OR "N")
        GO TO ZA30.
      MOVE WS-OPTION  TO WS-EXTR.
    IF WS-OPTION = "Y"
        GO TO ZA02.
            CLOSE DEBTOT.
      OPEN OUTPUT DEBTOT.
      CLOSE DEBTOT.
      OPEN I-O DEBTOT.
      GO TO ZA01.
      *    *************************************************************
      *    ****   F I L E   L I M I T E X C E E D E D
      *    (Config.sys Dos/Win/Windows 95/98/NT/2000)
      *    (Config.nt Windows XP)
      *    (System.ini Windows.me)
      *    *************************************************************
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.

       ZA50.
          IF WS-F-ERROR = 43
              MOVE "CARDEX file"
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
       ZB000       SECTION 91.

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
          IF WS-F-ERROR = 7
              MOVE W02-DEPART  TO WS-FILE
               MOVE ZERO         TO WS-KEY
              MOVE DPT-CODE  TO WS-KEYX
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
          IF WS-F-ERROR = 43
              MOVE W02-CARDEX  TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE CRD-KEY  TO WS-KEYX.
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
