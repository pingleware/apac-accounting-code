      $SET LINKCOUNT"512" GNT"DTP035.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****     ******   ********   *
      *   **    **     **     **    **   **  **   **    **  **         *
      *   **    **     **     **    **  **    **        **  *******    *
      *   **    **     **     *******   **    **     ****         **   *
      *   **    **     **     **        **    **        **        **   *
      *   **    **     **     **         **  **   **    **  **    **   *
      *   *******      **     **          ****     ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     P O S T   C O N T R A C T   T R A N S A C T I O N S        *
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
       PROGRAM-ID.     DTP035.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   February 1985.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1985 - 2018
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

       COPY "DBFOTO.SL".

       COPY "DBTRAN.SL".

       COPY "DEBTOR.SL".

       COPY "DEPART.SL".

       COPY "LEDTRF.SL".

       COPY "PARAM.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

       COPY "TXTRAN.SL".

          SELECT PRNREP  ASSIGN W02-PRINTER
                          ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "DBFOTO.FDE".

       COPY "DBTRAN.FDE".

       COPY "DEBTOR.FDE".

       COPY "DEPART.FDE".

       COPY "LEDTRF.FDE".

       COPY "PARAM.FDE".

       COPY "RECOVER.FDE".

       COPY "SHARED.FDE".

       COPY "TXTRAN.FDE".

      /
       FD  PRNREP    LABEL RECORD OMITTED
             LINAGE IS WS-PGE-LENGTH.
       01  REP-LINE1.
          03  REP-DETAIL1     PIC X(132).
       01  REP-L1.
          03  FILLER        PIC X(12).
          03  BAL-REP        PIC X(108).
          03  FILLER        PIC X(12).
       01  REP-LINE2.
          03  REP-DETAIL2     PIC X(132).
       01  INV-L1.
    03  INV-EXP        PIC  X(01).
          03  FILLER        PIC  X(03).
    03  INV-EXPNAME     PIC  X(40).
    03  FILLER        PIC  X(88).
       01  INV-L2.
    03  FILLER        PIC  X(06).
          03  INV-NADD        PIC  X(44).
           03  INV-RADD      REDEFINES INV-NADD.
              05  INV-PC      PIC  X(04).
              05  FILLER      PIC  X(40).
    03  FILLER        PIC  X(40).
          03  INV-H2        PIC  X(16).
          03  INV-TEL        PIC  X(14).
    03  INV-GNO REDEFINES INV-TEL
          PIC  X(14).
    03  FILLER        PIC  X(12).
       01  INV-L3.
          03  FILLER        PIC  X(06).
          03  INV-NAME        PIC  X(40).
           03  INV-DADD      REDEFINES INV-NAME.
              05  INV-PC1     PIC  X(04).
              05  FILLER      PIC  X(36).
          03  FILLER        PIC  X(06).
    03  INV-H3        PIC  X(08).
          03  INV-DADD2.
              05  INV-PC2     PIC  X(30).
          03  FILLER        PIC  X(10).
    03  INV-H4        PIC  X(19).
          03  INV-ORD.
        05  INV-TERMS.
     07  FILLER  PIC  X(05).
     07  INV-AC.
         09  FILLER  PIC X(03).
         09  INV-PGE PIC ZZ9.
        05  FILLER      PIC  X(01).
    03  FILLER        PIC  X(01).
       01  INV-L4.
          03  FILLER        PIC  X(01).
          03  INV-H5        PIC  X(17).
          03  INV-H6        PIC  X(32).
          03  INV-H7        PIC  X(14).
    03  INV-H8        PIC  X(13).
    03  INV-H9        PIC  X(13).
    03  INV-H10        PIC  X(15).
    03  INV-H11        PIC  X(14).
    03  INV-H12        PIC  X(13).
       01  INV-L5.
          03  FILLER        PIC  X(01).
          03  INV-ITM        PIC  X(14).
    03  FILLER        PIC  X(03).
          03  INV-MES.
              05  INV-DESC    PIC  X(30).
    03  FILLER        PIC  X(03).
          03  INV-QNT        PIC  Z(08)9.
          03  FILLER        PIC  X(02).
          03  INV-PRICE       PIC  Z(05)9.999-.
    03  FILLER        PIC  X(04).
          03  INV-DSC        PIC  Z9.99.
    03  FILLER        PIC  X(04).
    03  INV-EXTEND      PIC  Z(08)9.99-.
    03  FILLER        PIC  X(02).
          03  INV-TAX        PIC  Z(06)9.99- BLANK WHEN ZERO.
          03  FILLER        PIC  X(03).
          03  INV-VAL        PIC  Z(09)9.99-.
          03  FILLER        PIC  X(01).
       01  INV-L6.
    03  FILLER        PIC  X(09).
           03  INV-HD          PIC  X(10).
           03  INV-SERNOS.
               05  FILLER      PIC  X(01).
               05  INV-SERNO   PIC  X(10).
        03  FILLER      PIC  X(25).
    03  FILLER        PIC  X(77).
       01  INV-L7.
          03  FILLER        PIC  X(18).
          03  INV-REMKS.
              05  INV-RMKS.
   07  FILLER    PIC  X(40).
          03  FILLER        PIC  X(15).
    03  INV-H13        PIC  X(12).
    03  FILLER        PIC  X(01).
    03  INV-LINE        PIC  X(45).
    03  FILLER        PIC  X(01).
       01  INV-L8.
    03  INV-EXP2        PIC  X(01).
          03  FILLER        PIC  X(15).
    03  INV-INVOICE.
        05  FILLER      PIC  X(03).
        05  INV-CRNOTE.
     07  FILLER  PIC  X(06).
     07  INV-COPY
          PIC  X(12).
     07  FILLER  PIC  X(06).
        05  FILLER      PIC  X(03).
    03  FILLER        PIC  X(03).
    03  INV-H14        PIC  X(08).
    03  INV-REF.
        05  INV-DTE     PIC  99/99/9999.
       01  R-L1.
           03  R-DET1          PIC  X(132).
       01  R-L2.
           03  R-DET2          PIC  X(80).
       01  R-L3.
           03  R-DATE          PIC  X(06).
          03  R-DTE        PIC  Z9/99/9999.
          03  FILLER        PIC  X(04).
           03  R-H5            PIC  X(46).
           03  R-H6            PIC  X(07).
           03  R-BATCH         PIC  Z(05)9.
           03  FILLER          PIC  X(07).
           03  R-PAGE          PIC  X(06).
           03  R-P-NO          PIC  Z(03)9.
       01  R-L3A.
           03  FILLER          PIC  X(04).
           03  R-CO            PIC  X(40).
           03  FILLER          PIC  X(04).
       01  R-L4.
           03  FILLER          PIC  X(11).
           03  R-H1            PIC  X(04).
           03  FILLER          PIC  X(06).
           03  R-H2            PIC  X(15).
           03  FILLER          PIC  X(06).
           03  R-H3            PIC  X(15).
           03  FILLER          PIC  X(06).
           03  R-H4            PIC  X(06).
           03  FILLER          PIC  X(11).
       01  R-L5.
           03  FILLER          PIC  X(12).
           03  R-TRAN          PIC  9(02).
           03  FILLER          PIC  X(09).
           03  R-ENG           PIC  X(12).
           03  FILLER          PIC  X(09).
           03  R-AFR           PIC  X(12).
           03  FILLER          PIC  X(10).
           03  R-ACT           PIC  X(01).
           03  FILLER          PIC  X(13).
       01  R-L7.
           03  R-H7            PIC  X(07).
           03  R-H7A           PIC  X(27).
           03  R-H8            PIC  X(07).
           03  R-H10           PIC  X(09).
          03  R-H11        PIC  X(11).
          03  R-H12        PIC  X(15).
          03  R-H13        PIC  X(10).
          03  R-H14        PIC  X(10).
       01  R-L8.
           03  R-TAC           PIC  X(06).
           03  FILLER          PIC  X(01).
           03  R-DNME          PIC  X(24).
           03  FILLER          PIC  X(01).
           03  R-TDTE          PIC  X(08).
           03  FILLER          PIC  X(01).
           03  R-TDESC         PIC  X(08).
           03  FILLER          PIC  X(01).
           03  R-TREF          PIC  X(08).
           03  FILLER          PIC  X(01).
           03  R-VAL           PIC  Z(07)9.99-.
           03  FILLER          PIC  X(01).
           03  R-TAX           PIC  Z(06)9.99-.
           03  FILLER          PIC  X(01).
           03  R-TOT           PIC  Z(07)9.99-.
       01  R-L9.
           03  FILLER          PIC  X(54).
           03  R-BTOT          PIC  Z(06)9.99-.
           03  FILLER          PIC  X(15).
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
       77  WS-NETKEY    PIC 9(06)  COMP-5.
       77  WS-RECKEY    PIC 9(06)  COMP-5.
       77  WS-WRKEY        PIC 9(04)     COMP-5.
       77  WS-RECOVER    PIC 9(06)  COMP-5.
       77  WS-SHARED    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
       77  WS-NTP    PIC 9(01).
       77  WS-LINES        PIC 9(02).
       77  WS-PAGE         PIC 9(04)     COMP-5.
       77  WS-SKIP         PIC  X(01) VALUE "N".
       77  WS-OPTION       PIC X(01).
       77  WS-AGE          PIC X(01).
       77  WS-SEQ          PIC X(01).
       77  WS-CREDIT       PIC X(01).
       77  WS-ZERO         PIC X(01).
       77  WS-NUM          PIC Z9.
       77  WS-ADDR         PIC X(01).
       77  WS-ERROR        PIC 9(01).
       77  WS-ERROR1       PIC 9(01).
       77  WS-PGE-LENGTH   PIC 9(02)  VALUE 66.
       77  WS-PRN-LENGTH   PIC 9(02)  VALUE 62.
       77  WS-COND    PIC X(06).
       77  WS-NORM    PIC X(06).
       77  WS-EXPP    PIC X(06).
       77  WS-ECAN    PIC X(06).
       77  WS-8LPI    PIC X(06).
       77  WS-6LPI    PIC X(06).
       77  WS-10CPI    PIC X(06).
       77  WS-12CPI    PIC X(06).
       77  WS-17CPI    PIC X(06).
       77  WS-EXPAND    PIC X(01)  VALUE " ".
       77  WS-PRN-NO    PIC X(01)  COMP-X VALUE 9.
       77  WS-PRN-STAT    PIC X(01)  COMP-X.
       77  WS-CHAR1    PIC X(01).
       77  WS-CHAR2        PIC X(01).
       77  WS-COMP         PIC 9(01).
       77  WS-IND    PIC 9(01).
       77  WS-TYPE         PIC X(01).
       77  WS-HEAD         PIC X(01).
       77  WS-BATCH        PIC 9(05).
       77  WS-MESS         PIC X(02).
       77  WS-ACNO         PIC X(06).
       77  WS-S1           PIC 9(04)     COMP-5.
       77  WS-S2           PIC 9(04)     COMP-5.
       77  WS-S3           PIC 9(04)     COMP-5.
       77  WS-S4           PIC 9(04)     COMP-5.
       77  WS-S5           PIC 9(04)     COMP-5.
       77  WS-S6           PIC 9(04)     COMP-5.
       77  WS-S7           PIC 9(04)     COMP-5.
       77  WS-S8           PIC 9(04)     COMP-5.
       77  WS-ERR1         PIC X(22)   VALUE
                           "Invalid account number".
       77  WS-ERR2         PIC X(09) VALUE "No record".
       77  WS-A            PIC X(19) VALUE "Any key to continue".
       77  WS-C            PIC X(07) VALUE "COMPANY".
       77  WS-O            PIC X(08) VALUE "INTERNAL".
       77  WS-AC           PIC X(07) VALUE "Account".
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
              05  WS-TCH  PIC X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
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
           03  WS-PROG        PIC  X(03) VALUE "035".

       COPY "LEDTRF.WS".

       01  W00-TRAN-CODES.
           03  W00-T-VAT   PIC  9(09)V99 COMP-3.
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

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-FILE-IDS.
           03  W02-PRINTER.
               05  W02-REPORT PIC X(07) VALUE "DTP035.".
               05  W02-USER   PIC X(05) VALUE SPACES.

       01  W05-LIST.
           03  W05-OPT         PIC 9(03).
           03  W05-TYPES REDEFINES W05-OPT.
               05  W05-COMP    PIC 9(01).
               05  W05-INDV    PIC 9(01).
               05  W05-INT     PIC 9(01).

       COPY "W05.VAT".

       01  W05-DISC.
           03  W05-DISCOUNT.
               05  W05-DSETT   PIC S9(03)V99 COMP-3 OCCURS 10.

       01  W08-EXPD.
           03  W08-DEC             PIC 9(03) OCCURS 6.

       01  W10-SPARES.
           03  W10-DATE            PIC X(10).
           03  W10-RDATE                         REDEFINES W10-DATE.
               05  W10-DTE         PIC 99/99/9999.

       01  W10-REF.
           03  W10-NUM             PIC  9(08).

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

           03  W15-END-CYMD                      REDEFINES W15-E-YMD.
               05  W15-ECY         PIC  9(04).
               05  FILLER          PIC  9(04).

       01  W25-CALCS.
           03  W25-RESULT          PIC  9(05)V99.
           03  W25-RESULT1                       REDEFINES W25-RESULT.
               05  W25-WHOLE       PIC  9(05).
               05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2                       REDEFINES W25-RESULT.
               05  W25-KEY         PIC  9(04).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).
           03  W25-TOTAL           PIC S9(07)V99 COMP-3.
           03  W25-VALUE           PIC S9(07)V99 COMP-3.

       01  W30-DBFOTO.
           03  W30-KEY.
               05  W30-AC          PIC  X(06).
               05  W30-CONT        PIC  X(08).
           03  W30-DESC            PIC  X(30).
           03  W30-DATE            PIC  9(08)     COMP-3.
           03  W30-RENTAL          PIC S9(07)V99  COMP-3.
           03  W30-TYPE            PIC  X(01).
           03  W30-FREQ            PIC  X(01).
           03  W30-ADATE           PIC  9(08)     COMP-3.
           03  W30-ESC             PIC  9(03)V99  COMP-3.
           03  W30-RATE            PIC S9(06)V999 COMP-3.
           03  W30-RDTE            PIC  9(08)     COMP-3.
           03  W30-STRDNG          PIC  9(08)     COMP-5.
           03  W30-LSTRDNG         PIC  9(08)     COMP-5.
           03  W30-READING         PIC  9(08)     COMP-5.
           03  W30-VALUE           PIC S9(07)V99  COMP-3.
           03  W30-TAX             PIC S9(05)V99  COMP-3.
           03  W30-TCODE           PIC  X(01)     COMP-X.
           03  W30-VG.
               05  W30-VAT         PIC  9(01).
           03  FILLER              PIC  X(15).

       01  W30-AGE-DATES.
           03  W30-120             PIC  9(06) COMP-3.
           03  W30-90              PIC  9(06) COMP-3.
           03  W30-60              PIC  9(06) COMP-3.
           03  W30-30              PIC  9(06) COMP-3.
           03  W30-CUR             PIC  9(06) COMP-3.

       COPY "W40.DBT".

       COPY "W40.WS".

       01  W45-TRAN.
           03  W45-CODE        PIC 9(02).
           03  W45-ENG         PIC X(12).
           03  W45-AFR         PIC X(12).
           03  W45-ACT         PIC X(01).

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W60-NME-ADD.
           03  W60-NAME.
               05  W60-NCHAR OCCURS 40 PIC X(01).
           03  W60-ADDRESS.
               05  W60-ACHAR OCCURS 60 PIC X(01).
          03  W60-ADDRESS1.
               05  W60-PAD     PIC X(60).
               05  FILLER      PIC X(10).

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
           03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
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

       COPY "W90.DBT".

           03  W90-PRICE       PIC S9(08)V999 COMP-3.
           03  W90-UNITS       PIC  9(08)    COMP-3.
           03  W90-DEXC        PIC S9(09)V99 COMP-3.
           03  W90-DTAX        PIC S9(09)V99 COMP-3.
           03  W90-DTOT        PIC S9(09)V99 COMP-3.

       01  W95-STM.
           03  W95-COMP            PIC X(40).
           03  W95-ADD1            PIC X(30).
           03  W95-ADD2            PIC X(30).
           03  W95-ADD3            PIC X(30).
           03  W95-ADD4            PIC X(30).
           03  W95-PC.
               05  W95-POST        PIC 9(04).

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

       01  S27.
          03  LINE  5 COLUMN 12 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
                                 VALUE "This routine does not include a
      -                          "recovery procedure. Security".
          03  LINE  6 COLUMN 12 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
                                 VALUE "copies  of the data files should
      -                          " be made as a power  failure".
          03  LINE  7 COLUMN 12 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
                                 VALUE "or any other problem during this
      -                          " procedure would require the".
          03  LINE  8 COLUMN 12 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Red
                                 VALUE "DATA FILES from the previous bac
      -                          "kup to be restored !!!      ".
           03  LINE 10 COLUMN 12 VALUE "Do you wish to continue (Y/N)?".

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
       AA00-MAIN       SECTION.
       AA00-INIT.
           IF LS0-DBLEV < 3
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA00-INIT.
             PERFORM DA000.
             MOVE WS-10CPI  TO REP-DETAIL2.
             WRITE REP-LINE2 BEFORE 0 LINES.
             CLOSE DBFOTO RECOVER PRNREP.

       AA49.
            EXIT PROGRAM.

       COPY "DATE.CHK".

       COPY "FUNCTION.CRT".

       COPY "LOCKED.REC".

       COPY "CLEAR.CRT".

       COPY "DEBTOR.TRN".

      /
      *    ****    READ FILES ROUTINES
      *
       AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "DEBTOR.RD".

       COPY "DBTRAN.RD".

       COPY "DBFOTO.RD".

       COPY "DEPART.RD".

       COPY "LEDTRF.RD".

       COPY "SHARED.RD".

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
       AD000             SECTION.

       COPY "CONTROL.WR".

       COPY "DBFOTO.WR".

       COPY "DBTRAN.WR".

       COPY "DEBTOR.WR".

       COPY "PARAM.WR".

       COPY "DEPART.WR".

       COPY "LEDTRF.WR".

       COPY "TXTRAN.WR".

       COPY "APAC.HLP".

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
       AY000           SECTION.
       AY01.
             MOVE 01             TO REC-FILE.
             MOVE WS-PARKEY      TO REC-KEY.
             MOVE PAR-RECORD1    TO REC-PARAM.
             GO TO AY50.
       AY02.
             MOVE 02             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-DEBTOR-LOCK THRU READ-DEBTOR-EXIT.
            MOVE DEB-RECORD1  TO REC-DEBTOR.
             GO TO AY50.
       AY04.
             MOVE 04             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
             MOVE TRA-RECORD1    TO REC-DBTRAN.
             GO TO AY50.
       AY19.
             MOVE 19             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
             MOVE TAX-RECORD1    TO REC-TXTRAN.
             GO TO AY50.

       AY38.
      MOVE 38   TO REC-FILE.
            MOVE ZERO   TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
            PERFORM READ-DEPART THRU READ-DEPART-EXIT.
             MOVE DPT-RECORD     TO REC-DEPART.
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
                        WITH FOREGROUND-COLOR Brown HIGHLIGHT WS-STATUS
                        WITH FOREGROUND-COLOR Grey HIGHLIGHT
               STOP RUN.
       AY59.
             EXIT.
      *
      *    ****    Start processing transaction
      *
       AY60.
          IF WS-SKIP = "A"
               GO TO AY999.
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
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red HIGHLIGHT.
            COMMIT.
      ACCEPT WS-STIME FROM TIME.
      MOVE 400   TO WS-WAIT.
            PERFORM LOCK-REC-LOOP.
            DISPLAY SPACE AT 5051
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.
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
   WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT BLINK
        ACCEPT WS-STIME FROM TIME
        PERFORM LOCK-REC-LOOP
        DISPLAY "Waiting" AT 5072
   WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT BLINK
        GO TO AY60.
            DISPLAY SPACE AT 5072
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.

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
       BB050        SECTION.
       BB50.
             MOVE SPACES             TO REP-DETAIL2.
             WRITE INV-L1 BEFORE 2 LINES.
             MOVE X"0E"              TO INV-EXP2.
             MOVE "TAX INVOICE/BELASTING FAKTUUR" TO INV-INVOICE.
             WRITE INV-L1 BEFORE 0 LINES.
             WRITE INV-L1 BEFORE 2 LINES.
             MOVE SPACES             TO REP-DETAIL2.
             MOVE X"0E"              TO INV-EXP2.
             MOVE "  RENTAL,SERVICE/HUUR,DIENS" TO INV-INVOICE.
             WRITE INV-L1 BEFORE 0 LINES.
             WRITE INV-L1 BEFORE 2 LINES.
             MOVE SPACES             TO REP-DETAIL2.
             MOVE X"0E"              TO INV-EXP.
             MOVE W95-COMP           TO INV-EXPNAME.
             WRITE INV-L1 BEFORE 1 LINES.
             MOVE WS-ECAN            TO REP-DETAIL2.
             WRITE INV-L1 BEFORE 0 LINES.
             MOVE SPACES             TO REP-DETAIL2.
             MOVE W95-ADD1           TO INV-NADD.
           IF W05-VATNO NOT = SPACES
               MOVE "VAT No/BTW Nr :" TO INV-H2
               MOVE W05-VATNO        TO INV-GNO.
             WRITE INV-L1 BEFORE 1 LINES.
             MOVE SPACES             TO REP-DETAIL2.
             MOVE W95-ADD2           TO INV-NADD.
           IF W95-TEL NOT = SPACES
               MOVE "TELEPHONE/FOON:" TO INV-H2
               MOVE W95-TEL          TO INV-TEL.
             WRITE INV-L1 BEFORE 1 LINES.
             MOVE SPACES             TO REP-DETAIL2.
             MOVE W95-ADD3           TO INV-NADD.
             WRITE INV-L1 BEFORE 1 LINES.
             MOVE W95-ADD4           TO INV-NADD.
             WRITE INV-L1 BEFORE 1 LINES.

       BB55.
             MOVE SPACES             TO REP-DETAIL2.
             MOVE W95-PC             TO INV-PC.
             WRITE INV-L1 BEFORE 2 LINES.
             MOVE SPACES             TO REP-DETAIL2.
             MOVE "All items appearing on this invoice will be shown on the statement as they are for individual contracts"
     TO BAL-REP.
            WRITE INV-L1 BEFORE 1 LINES.
            MOVE "Alle items wat op hierdie faktuur verskyn sal ook op die staat verskyn omdat hulle verskulende kontrakte is"
     TO BAL-REP.
            WRITE INV-L1 BEFORE 2 LINES.
      MOVE SPACES  TO REP-DETAIL2.

       BB60.
             EXIT.
      /
       BC000        SECTION 2.
       BC189.
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
               MOVE W75-ATITLE (DEB-TITLE)
                                 TO W60-NAME
           ELSE
               MOVE W75-ETITLE (DEB-TITLE)
                                 TO W60-NAME.
             MOVE 7              TO WS-S6.
       BC190.
          IF W60-NCHAR(WS-S6) NOT = SPACE
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
            MOVE W40-NCHAR(25)  TO W60-NCHAR(WS-S6).
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
             MOVE SPACES         TO W40-PC1 W40-PC2.
             MOVE DEB-ADDRESS    TO W60-ADDRESS.
             PERFORM CA175-ADDRESS.
           IF NOT (DEB-POST = ZERO OR 9999)
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
           IF NOT (DEB-PPOST = ZERO OR 9999)
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

       COPY "DEBTOR.TRA".

       COPY "LEDTRF.UPD".

      /
      *    ****    P O S T   R E N T A L   &   S E R V I C E   T R A N S
      *
       DA000        SECTION 5.
       DA00.
             PERFORM ERASE-SCREEN.
      DISPLAY "POST RENTAL AND SERVICE CONTRACT DEBITS" AT 0221
        WITH FOREGROUND-COLOR Grey HIGHLIGHT.
            DISPLAY S27.

       DA05.
            ACCEPT WS-OPTION AT 1043
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO DA999
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN OTHER
       CALL X"E5"
       GO TO DA05
        END-EVALUATE.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "Y" OR "N")
              GO TO DA05.
           IF WS-OPTION = "N"
              GO TO DA999.
      DISPLAY "Processing :" AT 1212.
      DISPLAY "Reference  :" AT 1412.
      DISPLAY "Contract   :" AT 1612.
             MOVE ZERO           TO W90-DEBIT W90-CREDIT W90-VATTOT.
            MOVE ZERO   TO W90-BDT W90-30
                                    W90-BCR W90-60
                                    W90-CUR W90-90
                                    W90-INT W90-DISC
                                    W90-120 W40-SDISC.

       DA08.
             MOVE WS-17CPI           TO REP-DETAIL1.
             WRITE REP-LINE1 BEFORE 0 LINES.

       DA10.
             MOVE SPACES             TO W80-ACNO DEB-ACNO.
             MOVE ZERO               TO W90-CODE.
             MOVE W12-TODAY          TO W90-DTE W90-DATE TO W22-DATE.
             PERFORM DATE-CHECK THRU CHECK-DATE-EXIT.
      *
      *    ****  As W12-TODAY was checked at the start of the session
      *          it will be assumed to be correct and the above routine
      *          will have switched the date to CCYYMMDD in W20-DTE.
      *
      *    IF NOT(W22-ERROR = 0)
      *        EVALUATE W22-ERROR
      *          WHEN 1  MOVE "Invalid Order DATE"  TO WS-ERR-MES
      *          WHEN 2  MOVE "Invalid Order DAY"   TO WS-ERR-MES
      *          WHEN 3  MOVE "Invalid Order MONTH" TO WS-ERR-MES
      *        END-EVALUATE
      *        PERFORM ERROR-MESSAGE

       DA15.
             MOVE ZERO               TO W90-DB W90-CR W90-EX.
             MOVE SPACES             TO W90-REF W90-REG.
             MOVE 1                  TO WS-S9.

       DA17.
             MOVE ZERO               TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
           IF WS-S9 < 12
               ADD 1                 TO WS-S9
               GO TO DA17.
             MOVE " !"               TO DBF-AC DBF-CONT.
             PERFORM START-AT-DBFT-KEY THRU READ-DBFOTO-EXIT.
           IF WS-F-ERROR = 42
               GO TO DA999.

       DA20.
             PERFORM READ-DBFOTO-NEXT THRU READ-DBFOTO-EXIT.
           IF WS-F-ERROR = 42
               IF NOT(W80-ACNO = SPACES)
            PERFORM DA50
               END-IF
               GO TO DA100.
             MOVE DBF-RECORD1        TO W30-DBFOTO.
           IF (W30-VG < "0" OR > "6")
               MOVE 0                TO W30-VAT.
             MOVE W30-RDTE           TO W15-START.
             MOVE W15-SDAY           TO W15-SDD.
             MOVE W15-SMNTH          TO W15-SMM.
             MOVE W15-SCENT          TO W15-SCC.
             MOVE W15-SYEAR          TO W15-SYY.
           IF (W12-T-YMD < W15-S-YMD) AND ((W12-MM < W15-SMM) OR (W12-YY < W15-SYY))
               GO TO DA20.
             DISPLAY DBF-AC   AT 1225 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY DBF-CONT AT 1625 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Brown HIGHLIGHT.
           IF DBF-AC = DEB-ACNO
               GO TO DA25.
           IF NOT(W80-ACNO = SPACES)
               PERFORM DA50.
             MOVE 0                  TO WS-PAGE.
             MOVE ZERO               TO W90-DEXC W90-DTAX W90-DTOT W90-EX.
             MOVE DBF-AC             TO W80-ACNO DEB-ACNO.
             PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR = 1 OR 2
               GO TO DA20.
             MOVE DEB-ACNO           TO W40-ACNO.
             MOVE DEB-NAME           TO W40-NAME.
           IF W80-TYPE = 1
               INSPECT W40-NAME REPLACING ALL "*" BY " "
               INSPECT W40-NAME REPLACING FIRST "!" BY " ".
             DISPLAY W40-NAME AT 1232 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             PERFORM BC000.
             PERFORM BB050.
             MOVE 1                  TO WS-NETKEY.
             PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             MOVE DEB-INVOICE        TO W90-REF.
             DISPLAY W90-REF AT 1425 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Brown HIGHLIGHT.
           IF DEB-INVOICE = 99999999
               MOVE 1                TO DEB-INVOICE
           ELSE
               ADD 1                 TO DEB-INVOICE.
             PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
             PERFORM DA40.

       DA25.
             MOVE ZERO               TO W30-VALUE W30-TAX.
             MOVE W30-TCODE          TO W90-CODE.
             PERFORM GET-DEBTOR-TRAN-DESC.
           IF W00-T-CODE = ZERO
               GO TO DA20.
             MOVE W00-E-DESC         TO W90-DESC.
             MOVE W30-CONT           TO W90-REG.
           IF W30-RENTAL > ZERO
               MOVE W30-RENTAL       TO W90-VAL W90-PRICE
               MOVE 1                TO W90-UNITS
               PERFORM CG000
               ADD W90-DB            TO W30-VALUE W90-DTOT
               ADD W90-VAT(WS-VAT-SUB) TO W30-TAX W90-DTAX
               ADD W90-VAL           TO W90-DEXC.
           IF W30-READING = ZERO
               GO TO DA35.
             SUBTRACT W30-LSTRDNG FROM W30-READING GIVING W90-UNITS.
             COMPUTE W90-VAL ROUNDED = W30-RATE * W90-UNITS.
           IF W90-VAL = ZERO
               GO TO DA35.
             MOVE W30-RATE           TO W90-PRICE.
             PERFORM CG000.
             ADD W90-DB              TO W30-VALUE W90-DTOT.
             ADD W90-VAT(WS-VAT-SUB) TO W30-TAX W90-DTAX.
             ADD W90-VAL             TO W90-DEXC.

       DA35.
             MOVE W30-RDTE           TO W15-START.
           IF W30-FREQ = "A"
               ADD 1                 TO W15-SYEAR
           ELSE
               MOVE W30-FREQ         TO WS-NTP
             ADD WS-NTP              TO W15-SMNTH.
           IF W15-SMNTH > 12
               SUBTRACT 12           FROM W15-SMNTH
               ADD 1                 TO W15-SYEAR.
             MOVE W15-START          TO W30-RDTE.
             MOVE W30-ADATE          TO W15-START.
             MOVE W15-SDAY           TO W15-SDD.
             MOVE W15-SMNTH          TO W15-SMM.
             MOVE W15-SCENT          TO W15-SCC.
             MOVE W15-SYEAR          TO W15-SYY.
             MOVE W12-T-YMD          TO W15-E-YMD.
             MOVE ZERO               TO W15-EDD.
             ADD 1                   TO W15-EMM.
           IF W15-EMM > 12
               SUBTRACT 12           FROM W15-EMM
               ADD 1                 TO W15-ECY.
           IF NOT(W15-E-YMD < W15-S-YMD)
               IF W30-ESC NOT = ZERO
                   COMPUTE W30-RENTAL ROUNDED = W30-RENTAL + (W30-RENTAL * (W30-ESC / 100.0000))
             ADD 1                   TO W15-SYEAR
             MOVE W15-START          TO W30-ADATE.
             MOVE W30-READING        TO W30-LSTRDNG.
             MOVE W30-DBFOTO         TO DBF-RECORD1.
             PERFORM REWRITE-DBFOTO THRU WRITE-DBFOTO-EXIT.
             GO TO DA20.

       DA40.
      *
      *    ****    P R I N T   I N V O I C E   H E A D I N G S
      *
             MOVE SPACES             TO REP-DETAIL2.
             MOVE X"0E"              TO INV-EXP.
             MOVE "REF No:"          TO INV-H14.
             MOVE W90-REF            TO INV-REF.
             WRITE INV-L1 BEFORE 0 LINES.
             WRITE INV-L1 BEFORE 2 LINES.
             MOVE SPACES             TO REP-DETAIL2.
             MOVE X"0E"              TO INV-EXP.
             MOVE "DATE  :"          TO INV-H14.
             MOVE W12-TODAY          TO INV-DTE.
             WRITE INV-L1 BEFORE 0 LINES.
             WRITE INV-L1 BEFORE 1 LINES.
             MOVE WS-ECAN            TO REP-DETAIL2.
             WRITE INV-L1 BEFORE 0 LINES.
             MOVE SPACES             TO REP-DETAIL2.
             MOVE "DELIVERY / AFLEWRING" TO INV-DADD2.
             WRITE INV-L1 BEFORE 1 LINES.
             MOVE SPACES             TO REP-DETAIL2.
             MOVE W40-NAME           TO INV-NAME.
             MOVE "PAGE / BLADSY"    TO INV-H4.
             ADD 1                   TO WS-PAGE.
             MOVE WS-PAGE            TO INV-PGE.
             WRITE INV-L1 BEFORE 1 LINES.
             MOVE SPACES             TO REP-DETAIL2.
             MOVE W40-ADD1           TO INV-NAME.
             MOVE W40-PADD1          TO INV-DADD2.
             WRITE INV-L1 BEFORE 1 LINES.
             MOVE W40-ADD2           TO INV-NAME.
             MOVE W40-PADD2          TO INV-DADD2.
             MOVE "ACCOUNT / REKENING" TO INV-H4.
             MOVE W80-ACNO           TO INV-AC.
             WRITE INV-L1 BEFORE 1 LINES.
             MOVE SPACES             TO REP-DETAIL2.
             MOVE W40-ADD3           TO INV-NAME.
             MOVE W40-PADD3          TO INV-DADD2.
             WRITE INV-L1 BEFORE 1 LINES.
             MOVE SPACES             TO REP-DETAIL2.
             MOVE W40-ADD4           TO INV-NAME.
             MOVE W40-PADD4          TO INV-DADD2.
      *
      *    ****    P R I N T   T E R M S   O N   T H E   I N V O I C E
      *
             MOVE "TERMS / TERME"    TO INV-H4.
           IF DEB-TERMS = "0"
               MOVE "7 Days/Dae"     TO INV-TERMS
           ELSE
           IF DEB-TERMS = "2"
               MOVE "60 Days/Dae"    TO INV-TERMS
           ELSE
           IF DEB-TERMS = "1"
               MOVE "30 Days/Dae"    TO INV-TERMS
           ELSE
               MOVE "COD/KBA"        TO INV-TERMS.
             WRITE INV-L1 BEFORE 1 LINES.
             MOVE SPACES             TO REP-DETAIL2.
             MOVE W40-PC1            TO INV-PC1.
             MOVE W40-PC2            TO INV-PC2.
             WRITE INV-L1 BEFORE 2 LINES.
             MOVE SPACES             TO REP-DETAIL2.
             MOVE "CONTRACT NUMBER"  TO INV-H5.
             MOVE "  QUANTITY"       TO INV-H7.
             MOVE "DISCOUNT"         TO INV-H9.
             MOVE "EXCLUSIVE"        TO INV-H10.
             MOVE "NETT AMOUNT"      TO INV-H12.
             WRITE INV-L1 BEFORE 0 LINES.
             WRITE INV-L1 BEFORE 1 LINES.
             MOVE SPACES             TO REP-DETAIL2.
             MOVE " KONTRAK NOMMER"  TO INV-H5.
             MOVE "DESCRIPTION/BESRYWING" TO INV-H6.
             MOVE "HOEWEELHEID"      TO INV-H7.
             MOVE "PRICE/PRYS"       TO INV-H8.
             MOVE " AFSLAG"          TO INV-H9.
             MOVE "EKSKLUSIEF"       TO INV-H10.
             MOVE "VAT/BTW"          TO INV-H11.
             MOVE "NETO BEDRAG"      TO INV-H12.
             WRITE INV-L1 BEFORE 0 LINES.
             WRITE INV-L1 BEFORE 2 LINES.
             MOVE SPACES             TO REP-DETAIL2.

       DA45.
             EXIT.

       DA50.
             SUBTRACT LINAGE-COUNTER FROM WS-PRN-LENGTH GIVING WS-LINES.
             MOVE SPACES             TO REP-DETAIL2.
             WRITE INV-L1 BEFORE WS-LINES LINES.
             MOVE ALL "-"            TO INV-LINE.
      *
      *    ****    P R I N T   S U B - T O T A L
      *
             MOVE "Please check your invoice." TO INV-REMKS.
             WRITE INV-L1 BEFORE 1 LINES.
             MOVE SPACES             TO REP-DETAIL2.
             MOVE "TOTAL/TOTAAL"     TO INV-H13.
             MOVE "Gaan asseblief u faktuur na." TO INV-REMKS.
             MOVE W90-DEXC           TO INV-EXTEND.
             MOVE W90-DTAX           TO INV-TAX.
             MOVE W90-DTOT           TO INV-VAL.
             WRITE INV-L1 BEFORE 0 LINES.
             MOVE SPACES             TO INV-REMKS.
             WRITE INV-L1 BEFORE 1 LINES.
             MOVE SPACES             TO REP-DETAIL2.
             MOVE ALL "="            TO INV-LINE.
             WRITE INV-L1 BEFORE PAGE.
      *      WRITE INV-L1 BEFORE 3 LINES.
             MOVE SPACES             TO REP-DETAIL2.
             MOVE SPACES             TO W80-ACNO.

       DA55.
             EXIT.

       DA60.
             WRITE INV-L1 BEFORE 1 LINES.
             MOVE SPACES             TO REP-DETAIL2.
             MOVE "C/FWD - OORGEDRA" TO INV-DADD2.
             MOVE W90-DEXC           TO INV-EXTEND.
             MOVE W90-DTAX           TO INV-TAX.
             MOVE W90-DTOT           TO INV-VAL.
             WRITE INV-L1 BEFORE 1 LINES.
             MOVE SPACES             TO REP-DETAIL2.
             MOVE "Please check your invoice." TO INV-REMKS.
             WRITE INV-L1 BEFORE 1 LINES.
             MOVE "Gaan aseblief u faktuur na." TO INV-REMKS.
             WRITE INV-L1 BEFORE 4 LINES.
             PERFORM BB050.
             PERFORM DA40.
             MOVE "B/FWD - O/B"      TO INV-DESC.
             MOVE W90-DEXC           TO INV-EXTEND.
             MOVE W90-DTAX           TO INV-TAX.
             MOVE W90-DTOT           TO INV-VAL.
             WRITE INV-L1 BEFORE 1 LINES.
             MOVE SPACES             TO REP-DETAIL2.

       DA100.
             MOVE 0                  TO WS-PAGE.
             MOVE WS-12CPI           TO R-DET1.
             WRITE R-L1 BEFORE 0 LINES.
             MOVE SPACES             TO R-DET1.
             MOVE "PAGE:"            TO R-PAGE.
             MOVE "DATE:"            TO R-DATE.
             ADD 1                   TO WS-PAGE.
             MOVE W12-TODAY          TO R-DTE.
             MOVE WS-PAGE            TO R-P-NO.
             MOVE "DEBTOR RENTAL AND SERVICE CONTRACTS" TO R-H5.
             WRITE R-L1 BEFORE 2 LINES.
             MOVE SPACES             TO R-DET1.
             MOVE WS-EXPP            TO R-L1.
             WRITE R-L1 BEFORE 0 LINES.
             MOVE SPACES             TO R-DET1.
             MOVE W95-COMP           TO R-CO.
             WRITE R-L1 BEFORE 0 LINES.
             WRITE R-L1 BEFORE 0 LINES.
             MOVE WS-ECAN            TO R-L1.
             WRITE R-L1 BEFORE 2 LINES.
             MOVE "Exclusive"        TO R-H12.
             MOVE "V.A.T."           TO R-H13.
             MOVE "Inclusive"        TO R-H14.
             WRITE R-L1 BEFORE 1 LINES.
             MOVE "Total"            TO R-TDESC.
             COMPUTE R-VAL = W90-DEBIT - W90-VATTOT.
             MOVE W90-VATTOT         TO R-TAX.
             MOVE W90-DEBIT          TO R-TOT.
             WRITE R-L8 BEFORE PAGE.

       DA999.
             EXIT.

       CG000        SECTION 6.
       CG00.
      MOVE W30-VAT  TO WS-VAT-SUB.
    IF W30-VAT > 0
        IF W22-DTE1 < WS-VAT-DATE(W30-VAT)
     ADD 6  TO WS-VAT-SUB.
    IF WS-VAT-SUB > 0
        MOVE W05-VAT(WS-VAT-SUB)
     TO W05-RATE
        COMPUTE W90-VAT(WS-VAT-SUB) ROUNDED = (W90-VAL * W05-RTE)
        MOVE W90-VAL  TO W90-TAXVAL(WS-VAT-SUB)
    ELSE
        MOVE W90-VAL  TO W90-EX
        MOVE ZERO  TO W90-VAT(WS-VAT-SUB).
      ADD W90-VAL W90-VAT(WS-VAT-SUB)
     GIVING W90-DB.
      PERFORM GET-DEBTOR-TRAN THRU GET-DEBTOR-TRAN-EXIT.
      PERFORM AY60 THRU AY999.
      MOVE 0   TO WS-ACTION.
      PERFORM AY01 THRU AY59.
      PERFORM AY02 THRU AY59.
      MOVE "A"   TO WS-SKIP.
            PERFORM GENERATE-DEBTOR-TRAN.
      MOVE "N"   TO WS-SKIP.
            ADD W90-DB        TO W90-BDT W90-DEBIT.
      ADD 121 DEB-CAT   GIVING W90-RESULT.
      DIVIDE W90-RESULT BY 2
          GIVING W90-CATKEY
          REMAINDER W90-CS.
      ADD 1        TO W90-CS.
            MOVE W90-CATKEY   TO WS-PARKEY.
            PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      PERFORM AY01 THRU AY59.
            ADD W90-DB        TO PAR-CDRMTD (W90-CS)
            PAR-CDRYTD (W90-CS).
            PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
            MOVE W90-REG  TO INV-ITM.
            MOVE W30-DESC  TO INV-DESC.
      MOVE W90-UNITS  TO INV-QNT.
      MOVE W90-PRICE  TO INV-PRICE.
      MOVE W90-VAL  TO INV-EXTEND.
      MOVE W90-VAT(WS-VAT-SUB)
     TO INV-TAX.
            MOVE W90-DB  TO INV-VAL.
          IF LINAGE-COUNTER < (WS-PRN-LENGTH - 2)
               WRITE R-L8 BEFORE 1 LINES
           ELSE
        PERFORM DA60
              PERFORM DA10 THRU DA15.
            MOVE SPACES  TO REP-DETAIL1.
             PERFORM REWRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.
            ADD W25-VALUE  TO DEB-OUT.
             ADD W90-BDT         TO DEB-DT.
             ADD W90-BCR         TO DEB-CR.
             SUBTRACT W90-120    FROM DEB-FOUR.
             SUBTRACT W90-90     FROM DEB-THREE.
             SUBTRACT W90-60     FROM DEB-TWO.
             SUBTRACT W90-30     FROM DEB-ONE.
             SUBTRACT W90-CUR    FROM DEB-MTD.
             SUBTRACT W90-INT    FROM DEB-INTEREST.

       CG70A.
             MOVE 9              TO WS-PARKEY.
            PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             PERFORM AY01 THRU AY59.
    IF W30-VAT > 0
        STRING "VAT" DELIMITED SIZE
         W30-VG DELIMITED SIZE
     INTO DPT-CODE
        PERFORM READ-DEPART THRU READ-DEPART-EXIT
        PERFORM AY38 THRU AY59.
           IF W90-VAL NOT = ZERO
              IF W90-TX = ZERO
                   IF W90-DB = ZERO
                       ADD W90-VAL
                                 TO PAR-CNGS-DAY
                                    PAR-CNGS-MTD
                                    PAR-CNGS-YTD
                   ELSE
                       ADD W90-VAL
                                 TO PAR-DNGS-DAY
                                    PAR-DNGS-MTD
                                    PAR-DNGS-YTD
               ELSE
               IF W90-DB = ZERO
                   ADD W90-VAL   TO PAR-CVAT-DAY
                                    PAR-CVAT-MTD
                                    PAR-CVAT-YTD
        DPT-V-RET-DAY
        DPT-V-RET-MTD
        DPT-V-RET-YTD
     ADD W90-TX  TO DPT-O-VRET-DAY
        DPT-O-VRET-MTD
        DPT-O-VRET-YTD
     IF WX-INT = "Y"
         COMPUTE WX-VALUE = W90-TX * -1
         MOVE DPT-O-VAT-GL
     TO WX-AC
         MOVE 11  TO WX-TYPE
                       PERFORM GL-TRF-UPDATE
                   END-IF
               ELSE
                   ADD W90-VAL   TO PAR-DVAT-DAY
                                    PAR-DVAT-MTD
                                    PAR-DVAT-YTD
        DPT-V-SALES-DAY
        DPT-V-SALES-MTD
        DPT-V-SALES-YTD
     ADD W90-TX  TO DPT-O-VAT-DAY
        DPT-O-VAT-MTD
        DPT-O-VAT-YTD
     IF WX-INT = "Y"
         COMPUTE WX-VALUE = W90-TX * -1
         MOVE DPT-O-VAT-GL
         TO WX-AC
                       MOVE 10       TO WX-TYPE
                       PERFORM GL-TRF-UPDATE.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
    IF W30-VAT > 0
              PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
             PERFORM AY70 THRU AY999.

       CG999.
      EXIT.
      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
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

            MOVE 3                   TO WS-PARKEY.
            PERFORM READ-PARAM MTHRU READ-PARAM-EXIT.
            MOVE PAR-VAT-REG  TO W05-VATNO.
             OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
             OPEN I-O DBFOTO.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 42           TO WS-F-ERROR
                   GO TO ZA50.
           IF LS-USER NUMERIC
               IF LS-USE > ZERO AND < 110
                   MOVE LS-USE       TO WS-USUB
                   GO TO ZA01A.
             MOVE 110                TO WS-USUB.

       ZA01A.
             ADD 2549 WS-USUB        GIVING W25-RESULT.
             MOVE W25-KEY            TO WS-PARKEY.
             ADD 1 W25-SUB           GIVING WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF TRM-STDP (WS-S6) = 1
               MOVE "LPT1"           TO W02-PRINTER
               MOVE ZERO             TO WS-PRN-NO
           ELSE
           IF TRM-STDP (WS-S6) = 2
               MOVE "LPT2"           TO W02-PRINTER
               MOVE 1                TO WS-PRN-NO
           ELSE
           IF TRM-STDP (WS-S6) = 3
               MOVE "LPT3"           TO W02-PRINTER
               MOVE 2                TO WS-PRN-NO
           ELSE
           IF TRM-STDP (WS-S6) = 4
               MOVE "COM1"           TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 5
               MOVE "COM2"           TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 9
               MOVE LS-USER          TO W02-USER.
             ADD 501 TRM-PRN4(WS-S6) GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2  GIVING WS-PARKEY REMAINDER WS-S6.
             ADD 1                   TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE TRM-CONP(WS-S6)    TO WS-COND.
             MOVE TRM-NORP(WS-S6)    TO WS-NORM.
             MOVE TRM-EXPP(WS-S6)    TO WS-EXPP W08-EXPD.
             MOVE TRM-ECAN(WS-S6)    TO WS-ECAN.
             MOVE TRM-8LPI(WS-S6)    TO WS-8LPI.
             MOVE TRM-6LPI(WS-S6)    TO WS-6LPI.
             MOVE TRM-10CPI(WS-S6)   TO WS-10CPI.
             MOVE TRM-12CPI(WS-S6)   TO WS-12CPI.
             MOVE TRM-17CPI(WS-S6)   TO WS-17CPI.
             MOVE TRM-LENGTH(WS-S6)  TO WS-PRN-LENGTH.
             MOVE TRM-PAGE(WS-S6)    TO WS-PGE-LENGTH.
           IF TRM-PRN-TYPE (WS-S6) = "H"
               MOVE WS-12CPI         TO WS-ECAN
               MOVE 048              TO W08-DEC(4)
               MOVE 054              TO W08-DEC(5)
               MOVE W08-EXPD         TO WS-EXPP.

       ZA02.
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-AGE            TO WS-AGE.
             MOVE 1                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY        TO W95-COMP.
             MOVE PAR-TELEPHONE      TO W95-TEL.
             MOVE PAR-DMY            TO W12-TODAY.
             MOVE PAR-YMD            TO W12-T-YMD.
             ADD 1                   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-ADDRESS1       TO W95-ADD1.
             MOVE PAR-ADDRESS2       TO W95-ADD2.
             MOVE PAR-ADDRESS3       TO W95-ADD3.
             MOVE PAR-ADDRESS4       TO W95-ADD4.
             MOVE PAR-POST-CDE       TO W95-POST.
           IF W95-ADD4 = SPACES
               MOVE W95-PC           TO W95-ADD4
               MOVE SPACES           TO W95-PC.
           IF W95-ADD3 = SPACES
               MOVE W95-ADD4         TO W95-ADD3
               MOVE SPACES           TO W95-ADD4.
           IF W95-ADD2 = SPACES
               MOVE W95-ADD3         TO W95-ADD2
               MOVE SPACES           TO W95-ADD3.
           IF W95-ADD1 = SPACES
               MOVE W95-ADD2         TO W95-ADD1
               MOVE SPACES           TO W95-ADD2.
             MOVE 5                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DISCOUNT       TO W05-DISCOUNT.
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
             MOVE 1                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             DISPLAY "Waiting for printer response" AT 1412 WITH FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
             OPEN OUTPUT PRNREP.
             CALL X"91" USING X91-RES X91-FUN PRNREP.
             MOVE WS-10CPI           TO REP-DETAIL1.
             WRITE REP-LINE1 BEFORE 0 LINES.
             MOVE SPACES             TO REP-DETAIL1.
      *
      *    ****    S E T   U P   A G E   D A T E S
      *
             MOVE W12-T-YMD      TO W22-DTE2.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ The month end dates are stored in the Control file in the ³
      *    ³ first record as Cur, 30d, 60d, 90d and 120d and the next  ³
      *    ³ routine is used since Ver 8.11.06.                        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             MOVE 1                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE DEB-CUR-DTE        TO W30-CUR.
             MOVE DEB-30D-DTE        TO W30-30.
             MOVE DEB-60D-DTE        TO W30-60.
             MOVE DEB-90D-DTE        TO W30-90.
             MOVE DEB-120-DTE        TO W30-120.
             GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             GO TO ZA205.
       ZA50.
           IF WS-F-ERROR = 42
               MOVE "RENTAL file"    TO WS-FILE.
             DISPLAY "Rebuild " AT 0812 WS-FILE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Press any key to continue" AT 1012.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
       ZA51.
             EXIT PROGRAM.
       ZA55.
             MOVE 1              TO WS-S1.
            MOVE SPACES  TO WS-MID-LNE.
       ZA60.
            MOVE WS-G1   TO WS-TCHR(WS-S1) WS-BCHR(WS-S1).
            MOVE WS-G8   TO WS-BCH(WS-S1).
           IF WS-S1 < 80
               ADD 1             TO WS-S1
               GO TO ZA60.
      MOVE WS-G11  TO WS-BCH (1) .
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR(1)   WS-BCHR(1) .
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G2          TO WS-TCHR (8)  WS-TCHR (49)
                                    WS-TCHR (61) WS-TCHR (72).
             MOVE WS-G3          TO WS-MCHR (8)  WS-MCHR (49)
                                    WS-MCHR (61) WS-MCHR (72)
              WS-MCHR(1)   WS-MCHR (80).
             MOVE WS-G4          TO WS-BCHR (8)  WS-BCHR (49)
                                    WS-BCHR (61) WS-BCHR (72).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G14         TO WS-TCHR(1)   WS-BCHR(1) .
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

       ZA999-EXIT.
             EXIT.
      /
       ZB000       SECTION 9.

       OPEN-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             PERFORM DISPLAY-FILE-NAME.

       READ-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             PERFORM DISPLAY-FILE-NAME.

       WRITE-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             PERFORM DISPLAY-FILE-NAME.

       DISPLAY-FILE-NAME.
               WHEN 2     MOVE W02-NETWORK  TO WS-FILE
                          MOVE WS-NETKEY    TO WS-KEY
          ELSE
           IF WS-F-ERROR = 5
               MOVE W02-DBTRAN TO WS-FILE
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
          IF WS-F-ERROR = 15
               MOVE WS-PARID     TO WS-FILE
              MOVE WS-PARKEY  TO WS-KEY
          ELSE
           IF WS-F-ERROR = 18
               MOVE W02-RECOVER TO WS-FILE
               MOVE WS-RECKEY TO WS-KEY
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
             DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY "Status " AT 1212
                      WS-STATUS WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                     ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
           IF WS-STATUS NOT = "94"
               DISPLAY "Key    " AT 1412
                        WS-KEY WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                        " " WS-KEYX WITH FOREGROUND-COLOR Cyan HIGHLIGHT
               DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
               DISPLAY "Please make a note of these details" AT 1812.
             STOP RUN.
