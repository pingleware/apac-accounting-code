      $SET LINKCOUNT"640" GNT"DTPHIS.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  ******* ********  ** **  ********   *
      *   ** **     **     **    ** **    **  **      **      *
      *   ** **     **     **    ** **     ****       **      *
      *   ** **     **     ******* *****      **        **      *
      *   ** **     **     ** **     ****       **      *
      *   ** **     **     ** **    **  **      **      *
      *   *******      **     ** ********  ** **     **      *
      *               *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R ' S   S T O C K   S A L E S   P R O G R A M    *
      *                                                                *
      *     VERSION 9.00.00 - August 2012          *
      *               *
      ******************************************************************
      *               *
      *  Jan 2008 - New file (DEBALT) - Debtor alternate index   *
      *     included for lookups, using any word con-    *
      *     tained in the Debtor name.         *
      *               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     DTPHIS.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   OCTOBER 1982.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1982 - 2016
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

       COPY "DEBALT.SL".

       COPY "DEBMEM.SL".

       COPY "DEBTEX.SL".

       COPY "DEBTOR.SL".

       COPY "DEPART.SL".

       COPY "INVOIC.SL".

       COPY "PARAM.SL".

       COPY "STOCK.SL".

       COPY "STKMEM.SL".

       DATA DIVISION.
       FILE SECTION.

       COPY "CARDEX.FDE".

       COPY "CONTROL.FDE".

       COPY "DEBALT.FDE".

       COPY "DEBMEM.FDE".

       COPY "DEBTEX.FDE".

       COPY "DEBTOR.FDE".

       COPY "DEPART.FDE".

       COPY "INVOIC.FDE".

       COPY "PARAM.FDE".

       COPY "STOCK.FDE".

       COPY "STKMEM.FDE".

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
       77  WS-CHECK    PIC X(18)  VALUE
      "aeWlimemnomLalismJ".
       77  WS-SUB    PIC 9(04)  COMP-5.
       77  WS-S            PIC  9(04)    COMP-5.
       77  WS-S1           PIC  9(04)    COMP-5.
       77  WS-S2           PIC  9(04)    COMP-5.
       77  WS-S3           PIC  9(04)    COMP-5.
       77  WS-S4           PIC  9(04)    COMP-5.
       77  WS-S5           PIC  9(04)    COMP-5.
       77  WS-S6           PIC  9(04)    COMP-5.
       77  WS-S7           PIC  9(04)    COMP-5.
       77  WS-S8           PIC  9(04)    COMP-5.
       77  WS-S9           PIC  9(04)    COMP-5.
       77  WS-IXS1         PIC  9(04)    COMP-5.
       77  WS-ENQPOS    PIC 9(06)  COMP-5.
       77  WS-ENQEND       PIC  9(06)    COMP-5.
       77  WS-NETKEY    PIC 9(06)  COMP-5.
       77  WS-PARKEY       PIC  9(06)    COMP-5.
       77  WS-KEYSTR       PIC  9(06)    COMP-5.
       77  WS-LOOK    PIC X(01) VALUE "S".
       77  WS-OPTION       PIC  X(01).
       77  WS-USE-ITM    PIC X(01).
       77  WS-EXT-STK    PIC X(01).
       77  WS-ENQ    PIC X(01).
       77  WS-MONTH    PIC 9(02).
       77  WS-ETYPE    PIC X(01).
       77  WS-TYP    PIC X(01).
       77  WS-ERROR        PIC  9(01) VALUE ZERO.
       77  WS-ERR2         PIC  X(09) VALUE "No Record".
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.
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
              05  WS-MCHR  PIC  X(80) VALUE "³                ³
      -        "                      ³          ³         ³         ³".
    03  WS-MST-LNE REDEFINES WS-MID-LNE.
        05  WS-MST1     PIC  X(01).
        05  WS-MST78    PIC  X(78).
        05  WS-MST80    PIC  X(01).
          03  WS-MD-LNE.
              05  WS-MCH   PIC  X(80) VALUE "³      ³
      -        "                      ³           ³          ³       ³".
    03  WS-MDDT-LNE REDEFINES WS-MD-LNE.
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
    03  WS-PROG        PIC  X(03) VALUE "SAL".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       COPY "W05.VAT".

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

       01  W15-ALLOCATIONS.
           03  W15-DETAIL.
             05  W15-TRAN OCCURS 15.
               07  W15-VALUE   PIC  Z(06)9.99- BLANK WHEN ZERO.
               07  W15-ALLOC   PIC  Z(06)9.99- BLANK WHEN ZERO.
        07  W15-REC     PIC  X(08).
           03  W15-DISPLAY REDEFINES W15-DETAIL.
               05  W15-BAL1    PIC  X(11).
               05  W15-ALL1    PIC  X(11).
        05  W15-REC1    PIC  X(08).
               05  W15-BAL2    PIC  X(11).
               05  W15-ALL2    PIC  X(11).
        05  W15-REC2    PIC  X(08).
               05  W15-BAL3    PIC  X(11).
               05  W15-ALL3    PIC  X(11).
        05  W15-REC3    PIC  X(08).
               05  W15-BAL4    PIC  X(11).
               05  W15-ALL4    PIC  X(11).
        05  W15-REC4    PIC  X(08).
               05  W15-BAL5    PIC  X(11).
               05  W15-ALL5    PIC  X(11).
        05  W15-REC5    PIC  X(08).
               05  W15-BAL6    PIC  X(11).
               05  W15-ALL6    PIC  X(11).
        05  W15-REC6    PIC  X(08).
               05  W15-BAL7    PIC  X(11).
               05  W15-ALL7    PIC  X(11).
        05  W15-REC7    PIC  X(08).
               05  W15-BAL8    PIC  X(11).
               05  W15-ALL8    PIC  X(11).
        05  W15-REC8    PIC  X(08).
               05  W15-BAL9    PIC  X(11).
               05  W15-ALL9    PIC  X(11).
        05  W15-REC9    PIC  X(08).
               05  W15-BAL10   PIC  X(11).
               05  W15-ALL10   PIC  X(11).
        05  W15-REC10   PIC  X(08).
               05  W15-BAL11   PIC  X(11).
               05  W15-ALL11   PIC  X(11).
        05  W15-REC11   PIC  X(08).
               05  W15-BAL12   PIC  X(11).
               05  W15-ALL12   PIC  X(11).
        05  W15-REC12   PIC  X(08).
               05  W15-BAL13   PIC  X(11).
               05  W15-ALL13   PIC  X(11).
        05  W15-REC13   PIC  X(08).
               05  W15-BAL14   PIC  X(11).
               05  W15-ALL14   PIC  X(11).
        05  W15-REC14   PIC  X(08).
               05  W15-BAL15   PIC  X(11).
               05  W15-ALL15   PIC  X(11).
        05  W15-REC15   PIC  X(08).

       01  W25-CALCS.
           03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
               05  W25-WHOLE   PIC 9(05).
               05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(04).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).
           03  W25-TOTAL       PIC S9(09)V99 COMP-3.
           03  W25-VALUE       PIC S9(09)V99 COMP-3.

       01  W30-MONTHS.
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

       COPY "W40.DBT".

       01  W40-BALANCES.
           03  W40-A-MTD       PIC S9(09)V99 COMP-3.
           03  W40-A-CUR       PIC S9(09)V99 COMP-3.
           03  W40-A-30        PIC S9(09)V99 COMP-3.
           03  W40-A-60        PIC S9(09)V99 COMP-3.
           03  W40-A-90        PIC S9(09)V99 COMP-3.
           03  W40-A-120       PIC S9(09)V99 COMP-3.
           03  W40-A-INT       PIC S9(09)V99 COMP-3.
           03  W40-D-MTD       PIC S9(09)V99 COMP-3.
           03  W40-D-CUR       PIC S9(09)V99 COMP-3.
           03  W40-D-30        PIC S9(09)V99 COMP-3.
           03  W40-D-60        PIC S9(09)V99 COMP-3.
           03  W40-D-90        PIC S9(09)V99 COMP-3.
           03  W40-D-120       PIC S9(09)V99 COMP-3.
           03  W40-D-INT       PIC S9(09)V99 COMP-3.
           03  W40-T-MTD       PIC S9(09)V99 COMP-3.
           03  W40-T-CUR       PIC S9(09)V99 COMP-3.
           03  W40-T-30        PIC S9(09)V99 COMP-3.
           03  W40-T-60        PIC S9(09)V99 COMP-3.
           03  W40-T-90        PIC S9(09)V99 COMP-3.
           03  W40-T-120       PIC S9(09)V99 COMP-3.
           03  W40-T-INT       PIC S9(09)V99 COMP-3.

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
    03  W75-S2       PIC 9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
           03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
           03  W75-DNO        PIC X(06) OCCURS 18.
    03  W75-RNO       PIC X(08) OCCURS 16.
    03  W75-DATE       PIC 9(08) OCCURS 16.
    03  W75-SM       PIC 9(02) OCCURS 16.

       01  W75-STORE.
          03  W75-SNAME      PIC  X(08).
    03  W75-SREF       PIC  X(08).
    03  W75-SACN       PIC  X(06).
    03  W75-SDTE       PIC  9(08).
    03  W75-SSM       PIC  9(02).

       01  W80-ACCOUNT.
           03  W80-ACNO        PIC X(06).
           03  W80-TYPE        PIC 9(01).

       01  W95-STM.
           03  W95-COMP        PIC X(40).
           03  W95-D.
               05  W95-DTE     PIC Z9/99/9999.

       01  W100-EDIT.
          03  W100-LBAL       PIC Z(06)9.99-.
          03  W100-LARR       PIC Z(06)9.99.
    03  W100-COST       PIC Z(06)9.99.
          03  W100-SELL       PIC Z(06)9.99.
           03  W100-CUR        PIC Z(08)9.99-.
           03  W100-30         PIC Z(08)9.99-.
           03  W100-60         PIC Z(08)9.99-.
           03  W100-90         PIC Z(08)9.99-.
           03  W100-120        PIC Z(08)9.99-.
           03  W100-INT        PIC Z(08)9.99.
           03  W100-CRL        PIC Z(06)9.
           03  W100-KEY        PIC Z(05)9.
    03  W100-SER        PIC ZZ9.
          03  W100-PRC        PIC Z(06)9.99-.
          03  W100-VAL        PIC Z(08)9.99-.
           03  W100-V2.
               05  W100-S6V2   PIC Z(05)9.99-.
          03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
          03  W100-V4 REDEFINES W100-V2.
               05  W100-EXT    PIC Z(06)9.99.
           03  W100-V5 REDEFINES W100-V2.
        05  FILLER      PIC  X(02).
        05  W100-QNT    PIC Z(04)9.99.

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
          03  LINE  2 COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
               "DEBTORS - PURCHASE HISTORY STATISTICS".
    03  LINE  3 COLUMN  3 VALUE "A/C.".
    03        COLUMN 56 VALUE "Item°°°°°°°°°°°°°°°°°°°°".

       01  S10.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC X(06) USING W80-ACNO AUTO.

       01  S11.
    03  LINE  6 COLUMN 24 VALUE "Details Required:".
    03  LINE  8 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "A".
    03        COLUMN 25 VALUE "ll Items".
    03  LINE  9 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "L".
    03        COLUMN 25 VALUE "abour only".
    03  LINE 10 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "S".
    03        COLUMN 25 VALUE "pecific Item Code".
    03  LINE 12 COLUMN 24 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 1
     VALUE "Select ".
    03        COLUMN 31 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "A".
    03        COLUMN 32 BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
     VALUE " or ".
    03        COLUMN 36 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "S".
    03        COLUMN 37 BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
     VALUE "- press ".
    03        COLUMN 45 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "Esc".
    03        COLUMN 48 BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
     VALUE "ape to exit".

       01  S36-ITM.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC X(14)/X(03)
      USING W10-EXT-ITEM AUTO.

       01  S36-EXT.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC X(18) USING W10-EXT-ITEM AUTO.

       01  S36.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC X(14) USING W10-ITEM AUTO.

       01  S36-3.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
           PIC Z(03) USING W10-I4 AUTO.

       01  S36-4.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
           PIC Z(04) USING W10-I4 AUTO.

       01  S36-5.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
           PIC Z(05) USING W10-I5 AUTO.

       01  S36-6.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
           PIC Z(06) USING W10-I6 AUTO.

       01  S36-7.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
           PIC Z(07) USING W10-I7 AUTO.

       01  S36-8.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
           PIC Z(08) USING W10-I8 AUTO.

       01  S36-9.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
           PIC Z(09) USING W10-I9 AUTO.

       01  S36-10.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
           PIC Z(10) USING W10-I10 AUTO.

       01  S36-11.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
           PIC Z(11) USING W10-I11 AUTO.

       01  S36-12.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
           PIC Z(12) USING W10-I12 AUTO.

       01  S36-13.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
           PIC Z(13) USING W10-I13 AUTO.

       01  S36-14.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
           PIC Z(14) USING W10-I14 AUTO.

       COPY "DEBLUP.CRT".

       COPY "STKLUP.CRT".

       01  S-MOV.
           03  LINE  4 COLUMN  2 REVERSE-VIDEO VALUE
              "    Item Code              Description              Mont
      -        "h   Quantity  Av.Price".
          03  LINE  5 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
          03  LINE 22 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".

       01  SA-ACT.
          03  LINE  3 COLUMN 17 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
           PIC  X(06) USING W80-ACNO AUTO.

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
       AA000-MAIN        SECTION.
       AA000-INIT.
             PERFORM ZA000-INIT.
      PERFORM BA000.
      CLOSE CARDEX
     STOCK.
       AA49.
             EXIT PROGRAM.

       COPY "AA50DS.LUP".

       COPY "FUNCTION.CRT".

       COPY "LOCKED.RC2".

       COPY "CLEAR.CRT".

       COPY "DATE.CHK".

       COPY "AA300.STK".

      /
      *    ****    READ FILES ROUTINES
      *
       AC000              SECTION.

       COPY "CARDEX.RD".

       COPY "CONTROL.RD".

       COPY "DEBALT.RD".

       COPY "DEBMEM.RD".

       COPY "DEBTEX.RD".

       COPY "DEBTOR.RD".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "INVOIC.RD".

       COPY "STOCK.RD".

       COPY "STKMEM.RD".

       AD000   SECTION.

       COPY "DEBTEX.WR".

      /
      *    ****    M O V E M E N T   L O O K U P
      *
       MOVEMENT-LOOKUP   SECTION.

       MOVE-LUP-SCREEN.
            DISPLAY S-MOV.
      INITIALIZE CRD-REC1.
    IF WS-ENQ = "S"
        MOVE W10-ITEM  TO CRD-CODE.
      MOVE W80-ACNO  TO CRD-AC.
      MOVE W15-S-YMD  TO CRD-DTE.

       MOVE-LUP-PERFORM ERASE-SCREEN.
             MOVE 0601           TO CPOS.
            PERFORM MOVE-CRT-EDGE UNTIL CLIN = 22.

       MOVE-LUP-CONTINUE.
            MOVE 6   TO CLIN.
             MOVE 1              TO W75-S W75-S1.

       MOVE-LUP-TAB-LOOP.
      MOVE SPACES  TO W75-DNO (W75-S1)
        W75-RNO (W75-S1).
      MOVE ZERO   TO W75-DATE (W75-S1)
        W75-SM (W75-S1).
    IF W75-S1 < 16
               ADD 1             TO W75-S1
        GO TO MOVE-LUP-TAB-LOOP.
             MOVE ZERO           TO W75-S1.

       MOVE-LUP-START-CRDX.
      MOVE SPACES  TO WS-OPTION.
    IF WS-ENQ = "S"
        PERFORM START-AT-CRDX-KEY THRU READ-CARDEX-EXIT
    ELSE
              PERFORM START-AT-CRDX-ACNT THRU READ-CARDEX-EXIT.
          IF WS-F-ERROR = 43
        GO TO MOVE-NO-RECORD.

       MOVE-GET-CARDEX-REC.
           IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
        GO TO MOVE-SKIP-CRDX-READ.
            PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
    IF WS-F-ERROR = 43
               SUBTRACT 1        FROM W75-S
        GO TO MOVE-FULL-SCREEN.

       MOVE-LUP-DISP-ITEM.
    IF NOT (CRD-AC = DEB-ACNO)
        GO TO MOVE-FULL-SCREEN.
    IF (CRD-DTE > W15-E-YMD)
        GO TO MOVE-FULL-SCREEN.
    IF (WS-ENQ = "S") AND
       (W10-ITEM NOT = CRD-CODE)
        GO TO MOVE-FULL-SCREEN.
    IF (WS-ENQ = "L") AND
       (CRD-SUPP NOT = "-LAB-")
        GO TO MOVE-GET-CARDEX-REC.
    IF NOT (CRD-CODE = STK-CODE)
        MOVE CRD-CODE  TO STK-CODE
        PERFORM READ-STOCK THRU READ-STOCK-EXIT.

       MOVE-SKIP-CRDX-READ.
            DISPLAY CRD-REF AT LINE CLIN COLUMN 2
       WITH FOREGROUND-COLOR 11.
      MOVE CRD-DTE  TO W22-DTE2.
      MOVE W22-CC2  TO W22-CC3.
      MOVE W22-YY2  TO W22-YY3.
      MOVE W22-MM2  TO W22-MM3.
      MOVE W22-DD2  TO W22-DD3.
      MOVE W22-DTE3  TO W95-DTE.
            DISPLAY W95-DTE AT LINE CLIN COLUMN 11
       WITH FOREGROUND-COLOR 11.
    IF CRD-SUPP = "-LAB-"
        DISPLAY CRD-SUPP AT LINE CLIN COLUMN 22
         WITH FOREGROUND-COLOR 6 HIGHLIGHT
    ELSE
              DISPLAY CRD-SUPP AT LINE CLIN COLUMN 22
         WITH FOREGROUND-COLOR 11.
      *      DISPLAY CRD-SMAN AT LINE CLIN COLUMN 28
      *       WITH FOREGROUND-COLOR 11.
      MOVE CRD-COST  TO W100-COST.
      MOVE CRD-SELL  TO W100-SELL.
      MOVE CRD-QNT  TO W100-QNT.
            DISPLAY W100-COST AT LINE CLIN COLUMN 2
       WITH FOREGROUND-COLOR 11.
      DISPLAY W100-SELL AT LINE CLIN COLUMN 2
       WITH FOREGROUND-COLOR 11.
      DISPLAY W100-QNT AT LINE CLIN COLUMN 2
       WITH FOREGROUND-COLOR 11.
      DISPLAY STK-CODE AT LINE CLIN COLUMN 2
       WITH FOREGROUND-COLOR 11.
    IF CRD-SELL < ZERO
        DISPLAY "C" AT LINE CLIN COLUMN 2
   WITH FOREGROUND-COLOR 11
    ELSE
        DISPLAY "I" AT LINE CLIN COLUMN 2
   WITH FOREGROUND-COLOR 11.
           IF CLIN < 21
               ADD 1             TO CLIN W75-S
              GO TO MOVE-GET-CARDEX-REC.
      
       MOVE-FULL-SCREEN.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
001020      DISPLAY " " AT 2303 WITH FOREGROUND-COLOR 14
             " - " WITH FOREGROUND-COLOR 7
       "PgUp" WITH FOREGROUND-COLOR 14
                     "/" WITH FOREGROUND-COLOR 7
       "PgDn" WITH FOREGROUND-COLOR 14
       " - " WITH FOREGROUND-COLOR 7
             "Esc" WITH FOREGROUND-COLOR 14
       "ape." WITH FOREGROUND-COLOR 7.

       MOVE-SAVE-FULL-SCREEN.
      PERFORM SAVE-SCREEN-2.
            MOVE 0   TO W75-S1.
             MOVE 0601           TO CPOS.

       MOVE-CRT-ATTRIB.
      MOVE 3920   TO W41-LENGTH.
      CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
           W42-ATTRIB2
           W41-LENGTH
     RETURNING WS-STATUS.
            MOVE 4000   TO W41-LENGTH.

       MOVE-GET-USER-OPT.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM CLEAR-L50.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
             GO TO MOVE-ADJ-NO-KEYS
   WHEN DOWN-KEY
             GO TO MOVE-CHK-NO-KEYS
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO MOVE-GET-USER-OPT
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO MOVEMENT-LOOKUP-EXIT
   WHEN PAGE-DOWN
             GO TO MOVE-GET-NEXT-PAGE
   WHEN PAGE-UP
             GO TO MOVE-GET-PREV-PAGE
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO MOVE-GET-USER-OPT.
      CALL X"E5".
            GO TO MOVE-GET-USER-OPT.

       MOVE-GET-NEXT-PAGE.
           IF W75-S < 16
              GO TO MOVE-GET-USER-OPT.

       MOVE-NEW-PAGE.
             MOVE 0601           TO CPOS.
            PERFORM MOVE-CRT-EDGE UNTIL CLIN = 22.
             MOVE 6              TO CLIN.
             MOVE LOW-VALUES     TO W75-KEYS.
             MOVE 1              TO W75-S W75-S1.
      PERFORM MOVE-LUP-TAB-LOOP.
            GO TO MOVE-GET-CARDEX-REC.

       MOVE-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
       MOVE "J"    TO WS-OPTION.

       MOVE-GET-PREV-LOOP.
      PERFORM READ-CARDEX-PREV THRU READ-CARDEX-EXIT.
    IF WS-F-ERROR = 43
        MOVE SPACE  TO WS-OPTION
        GO TO MOVE-NEW-PAGE.
    IF NOT (CRD-AC = DEB-ACNO)
        PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT
        GO TO MOVE-NEW-PAGE.
    IF WS-IXS1 < 32
        ADD 1   TO WS-IXS1
        GO TO MOVE-GET-PREV-LOOP.
      GO TO MOVE-NEW-PAGE.

       MOVE-NO-RECORD.
      PERFORM CLEAR-L50.
            DISPLAY "No record - Press " AT 5012
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "ANY" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             " key"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
            GO TO MOVEMENT-LOOKUP-EXIT.

       MOVE-ADJ-NO-KEYS.
           IF W75-S1 > 1
               SUBTRACT 1        FROM W75-S1 CLIN.
            GO TO MOVE-HIGHLIGHT.

       MOVE-CHK-NO-KEYS.
           IF W75-S1 = ZERO
               MOVE 1            TO W75-S1
              GO TO MOVE-HIGHLIGHT.
           IF W75-S1 < W75-S
               ADD 1             TO W75-S1 CLIN.

       MOVE-HIGHLIGHT.
            PERFORM MOVE-CRT-ATTRIB.
             DISPLAY W43-DET (CLIN) AT CPOS WITH REVERSE-VIDEO.
            GO TO MOVE-GET-USER-OPT.

       MOVE-CRT-EDGE.
            DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR 0
          FOREGROUND-COLOR 3
       "        ³          ³      ³          ³          ³
      -       "       ³                  ³ "
        WITH FOREGROUND-COLOR 3
       "³" WITH BACKGROUND-COLOR 0
         FOREGROUND-COLOR 3.
            ADD 1   TO CLIN.


       MOVEMENT-LOOKUP-EXIT.
             EXIT.

       COPY "DEBTOR.ALU".

       COPY "DEBTOR.ILU".

       COPY "APAC.HLP".

      /
      *  ****  E N Q U I R Y   ( S A L E S   T R A C K I N G )
      *
       BA000         SECTION 50.
       BA00.
             PERFORM ERASE-SCREEN.
      DISPLAY S10D.

       BA05.
            DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Help,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             " to do Debtor Lookup"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
            ACCEPT S10 AT 0308.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W80-ACNO
       GO TO BA10
         WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
             PERFORM AA50D THRU AA59
   WHEN OTHER
       CALL X"E5"
       GO TO BA05
        END-EVALUATE
              DISPLAY S10 AT 0308
               IF W80-ACNO = SPACES
           GO TO BA05.

       BA10.
            PERFORM CLEAR-L50.
          IF W80-ACNO = SPACES
              GO TO BA999.
      MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W80-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.

       BA15.
            PERFORM CLEAR-L50.
             PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR = 1
              MOVE "No record"  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BA05.
           IF WS-ERROR = 2
              MOVE "Invalid account"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BA05.
      *
      *    ****    D I S P L A Y   D E B T O R   D E T A I L S
      *
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
        GO TO BA05.
      DISPLAY S11.

       BA20.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
             PERFORM CLEAR-L50
       GO TO BA05
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO BA20
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
        IF NOT (WS-OPTION = "A" OR "L" OR "S")
     CALL X"E5"
     GO TO BA20
        END-IF
    ELSE
        CALL X"E5"
        GO TO BA20.
      MOVE 0401   TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
      MOVE SPACES  TO W10-ITEM.
    IF W10-STCK = "N"
        IF W10-SLNGTH = 3
     MOVE ZERO  TO W10-I3
        ELSE
        IF W10-SLNGTH = 4
     MOVE ZERO  TO W10-I4
        ELSE
        IF W10-SLNGTH = 5
     MOVE ZERO  TO W10-I5
        ELSE
        IF W10-SLNGTH = 6
     MOVE ZERO  TO W10-I6
        ELSE
        IF W10-SLNGTH = 7
     MOVE ZERO  TO W10-I7
        ELSE
        IF W10-SLNGTH = 8
     MOVE ZERO  TO W10-I8
        ELSE
        IF W10-SLNGTH = 9
     MOVE ZERO  TO W10-I9
        ELSE
        IF W10-SLNGTH = 10
     MOVE ZERO  TO W10-I10
        ELSE
        IF W10-SLNGTH = 11
     MOVE ZERO  TO W10-I11
        ELSE
        IF W10-SLNGTH = 12
     MOVE ZERO  TO W10-I12
        ELSE
        IF W10-SLNGTH = 13
     MOVE ZERO  TO W10-I13
        ELSE
     MOVE ZERO  TO W10-I14.
      MOVE WS-OPTION  TO WS-ENQ.
      MOVE W12-MONTH  TO WS-MONTH.
    IF WS-ENQ = "A" OR "L"
        GO TO BA40.

       BA25.
            PERFORM AA300.
      *
      *    ****    G E T   S T O C K   C O D E
      *
    IF W10-STCK = "A"
        IF WS-EXT-STK = "Y"
     ACCEPT S36-EXT AT 0361
        ELSE
        IF WS-USE-ITM = "Y"
     ACCEPT S36-ITM AT 0361
        ELSE
           ACCEPT S36 AT 0361
        END-IF
    ELSE
    IF W10-SLNGTH = 3
        ACCEPT S36-3 AT 0361
    ELSE
    IF W10-SLNGTH = 4
        ACCEPT S36-4 AT 0361
    ELSE
    IF W10-SLNGTH = 5
        ACCEPT S36-5 AT 0361
    ELSE
    IF W10-SLNGTH = 6
        ACCEPT S36-6 AT 0361
    ELSE
    IF W10-SLNGTH = 7
        ACCEPT S36-7 AT 0361
    ELSE
    IF W10-SLNGTH = 8
        ACCEPT S36-8 AT 0361
    ELSE
    IF W10-SLNGTH = 9
        ACCEPT S36-9 AT 0361
    ELSE
    IF W10-SLNGTH = 10
        ACCEPT S36-10 AT 0361
    ELSE
    IF W10-SLNGTH = 11
        ACCEPT S36-11 AT 0361
    ELSE
    IF W10-SLNGTH = 12
        ACCEPT S36-12 AT 0361
    ELSE
    IF W10-SLNGTH = 13
        ACCEPT S36-13 AT 0361
    ELSE
    IF W10-SLNGTH = 14
        ACCEPT S36-14 AT 0361.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W10-ITEM
             PERFORM CLEAR-L50
       GO TO BA05
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       MOVE "I"  TO WS-ETYPE
   WHEN F3-KEY
       MOVE "A"  TO WS-ETYPE
   WHEN F4-KEY
       MOVE "D"  TO WS-ETYPE
   WHEN F5-KEY
       MOVE "X"  TO WS-ETYPE
   WHEN OTHER
       CALL X"E5"
       GO TO BA25
        END-EVALUATE
        MOVE "S"   TO WS-LOOK
              PERFORM AA50 THRU AA59
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-EXT-STK = "Y"
         DISPLAY S36-EXT AT 0361
     ELSE
     IF WS-USE-ITM = "Y"
         DISPLAY S36-ITM AT 0361
     ELSE
         DISPLAY S36 AT 0361
     END-IF
        ELSE
     DISPLAY S36 AT 0361.

       BA30.
          IF W10-ITEM = SPACES
              GO TO BA25.

       BA35.
             PERFORM CA000.
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
    IF W10-STCK = "A"
        IF WS-EXT-STK = "Y"
     DISPLAY S36-EXT AT 0361
        ELSE
        IF WS-USE-ITM = "Y"
     DISPLAY S36-ITM AT 0361
        ELSE
           DISPLAY S36 AT 0361
        END-IF
    ELSE
        DISPLAY S36 AT 0361.
          IF WS-F-ERROR = 22
        MOVE "Z"   TO WS-ETYPE
              PERFORM AA50 THRU AA59
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-EXT-STK = "Y"
         DISPLAY S36-EXT AT 0361
     ELSE
     IF WS-USE-ITM = "Y"
         DISPLAY S36-ITM AT 0361
     ELSE
         DISPLAY S36 AT 0361
     END-IF
        ELSE
     DISPLAY S36 AT 0361
        END-IF
              IF W10-ITEM = SPACES
           GO TO BA25
               ELSE
     GO TO BA35.
      DISPLAY "Item Description" AT 0612.
      DISPLAY STK-DESC AT 0629
       WITH BACKGROUND-COLOR 5
     FOREGROUND-COLOR 3 HIGHLIGHT.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO BA25.

       BA40.
      PERFORM SAVE-SCREEN-3.
      MOVE 5   TO SHADE-ROW.
      MOVE 27   TO SHADE-COL.
      MOVE 30   TO SHADE-WIDTH.
      MOVE 5   TO SHADE-LINES.
      DISPLAY S05D.
      PERFORM SCREEN-SHADOW.
      MOVE W12-MONTH  TO W30-START W30-END.

       BA45.
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
       GO TO BA999
   WHEN OTHER  CALL X"E5"
       GO TO BA45
        END-EVALUATE.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO BA45.
      PERFORM CLEAR-L50.
    IF (W30-START < 01 OR > 12) OR
       (W30-END < 01 OR > 12)
        MOVE "Months must be 01 to 12"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BA45.
      MOVE W12-TODAY  TO W15-START W15-END.
      MOVE W30-START  TO W15-SMNTH.
      MOVE W30-END  TO W15-EMNTH.
      MOVE 01   TO W15-SDAY.
      MOVE 31   TO W15-EDAY.
      *
      *    ****   I S S T A R T   M O N T H -   PREVIOUS YEAR
      *
    IF W30-END < W30-START
        SUBTRACT 1  FROM W15-SCCYY.
    IF W15-EMNTH = 04 OR 06 OR 09 OR 11
        MOVE 30   TO W15-EDAY
    ELSE
    IF W15-EMNTH = 02
      *
      *    ****    C H E C K   F O R   L E A P  Y E A R
      *
              DIVIDE W15-ECCYY BY 4 GIVING W25-RESULT
              IF W25-DECIMAL = 0
     MOVE 29  TO W15-EDAY
        ELSE
     MOVE 28  TO W15-EDAY.
      *
      *    ****    SWITCH START DATE TO YEAR/MONTH/DAY
      *
      MOVE W15-SDAY  TO W15-SDD.
      MOVE W15-SMNTH  TO W15-SMM.
      MOVE W15-SCENT  TO W15-SCC.
      MOVE W15-SYEAR  TO W15-SYY.
      *
      *    ****    SWITCH END DATE TO YEAR/MONTH/DAY
      *
      MOVE W15-EDAY  TO W15-EDD.
      MOVE W15-EMNTH  TO W15-EMM.
      MOVE W15-ECENT  TO W15-ECC.
      MOVE W15-EYEAR  TO W15-EYY.
      PERFORM RESTORE-SCREEN-3.
      PERFORM MOVEMENT-LOOKUP.
      MOVE SPACES  TO W80-ACNO.
      GO TO BA00.

       BA999.
             EXIT.

      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   C O D E
      *
       CA000      SECTION 50.
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
      *    ****   G E T   S T O C K   R E C O R D   U S I N G
      *           A L T E R N A T E   C O D E
      *
       CA100      SECTION 50.
       CA101.
             MOVE 10             TO W50-LENGTH.
       CA105.
             MOVE W10-ACODE      TO W50-DATA.
           IF W50-1 = SPACE
               MOVE W50-9        TO W10-ACODE
               GO TO CA105.
      CALL "CBL_TOUPPER" USING W50-DATA
    BY VALUE W50-LENGTH
    RETURNING W50-STATUS.
            MOVE W50-ACODE  TO STK-ACODE W10-ACODE.
       CA110.
             PERFORM START-AT-ALT-CODE THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO CA119.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 0
               PERFORM CA500.
       CA119.
             EXIT.
      *
      *    ****    Read debtor record - using account number.
      *
       CA155-GET-DEBTOR  SECTION 50.
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
       CA165-NAME  SECTION 50.
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
       CA175-ADDRESS  SECTION 50.
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
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   X R E F
      *
       CA200      SECTION 50.
       CA201.
             MOVE 14             TO W50-LENGTH.
       CA205.
             MOVE W10-CODE2      TO W50-DATA.
           IF W50-1 = SPACE
               MOVE W50-13       TO W10-CODE2
               GO TO CA205.
      CALL "CBL_TOUPPER" USING W50-DATA
    BY VALUE W50-LENGTH
    RETURNING W50-STATUS.
            MOVE W50-ITEM  TO W10-CODE2.
       CA206.
             MOVE W50-ITEM       TO STK-XREF.
       CA210.
             PERFORM START-AT-STOCK-XREF THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO CA299.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
             PERFORM CA500.
       CA299.
             EXIT.
      *
      *    ****    Calculate the inclusive prices
      *
       CA500      SECTION 50.
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
              MOVE W05-VAT (WS-VAT-SUB)
                                 TO W05-RATE.
             COMPUTE W10-VCASH ROUNDED
                   = W10-VCASH + (W10-VCASH * W05-RTE).
             COMPUTE W10-VWSALE ROUNDED
                   = W10-VWSALE + (W10-VWSALE * W05-RTE).
             COMPUTE W10-VSELL ROUNDED
                   = W10-VSELL + (W10-VSELL * W05-RTE).
       CA599.
             EXIT.
      /
      *    ****    I N I T I A L I Z E   P R O G R A M
      *
       ZA000-INIT        SECTION 80.
       ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
      MOVE LS-PARID  TO WS-PARID.
      MOVE LS-L-OR-N  TO W02-L-OR-N.
      MOVE WS-SYS-ID  TO W02-SYSID.
      MOVE LS-TODAY-DDMMYY
     TO TODAY-DDMMYY W12-TODAY.
      MOVE LS-USUB  TO WS-USUB.
            PERFORM ERASE-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

            MOVE 3   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-VAT-REG  TO W05-VATNO.
    IF NOT (PAR-STOCK = "A" OR "N")
        MOVE "A"   TO PAR-STOCK.
      MOVE PAR-STOCK  TO W10-STCK.
      MOVE PAR-USE-ITM  TO WS-USE-ITM.
      MOVE PAR-EXT-STK  TO WS-EXT-STK.
    IF PAR-STOCK = "N"
        MOVE PAR-SLNGTH  TO W10-SLNGTH
    ELSE
        MOVE 14   TO W10-SLNGTH.
    IF W10-SLNGTH < 3
        MOVE 3   TO W10-SLNGTH
    ELSE
    IF W10-SLNGTH > 14
        MOVE 14   TO W10-SLNGTH.
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

       ZA02.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY    TO W95-COMP.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
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
           PERFORM OPEN-ERROR.
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
               PERFORM OPEN-ERROR.
            OPEN I-O DEBTEX.
    IF WS-STATUS = "05"
        CLOSE DEBTEX
        OPEN OUTPUT DEBTEX
        CLOSE DEBTEX
        OPEN I-O DEBTEX.
          IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
           GO TO ZA49
               ELSE
        IF IDX-CORRUPT
     CLOSE DEBTEX
     OPEN OUTPUT DEBTEX
     CLOSE DEBTEX
     OPEN I-O DEBTEX.
          IF WS-STATUS NOT = "00"
              MOVE 52   TO WS-F-ERROR
        IF WS-STATUS = "39"
           CLOSE DEBTEX
     OPEN OUTPUT DEBTEX
     CLOSE DEBTEX
     OPEN I-O DEBTEX
        ELSE
           PERFORM OPEN-ERROR.
            GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
            GO TO ZA205.
       ZA50.
          IF WS-F-ERROR = 22
              MOVE "STOCK File"
     TO WS-FILE
           ELSE
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
      *    ****    I / O   E R R O R   M E S S A G E S
      *
       ZB000-ERROR      SECTION 81.

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
       I-O-ERROR-EXIT.
             EXIT.

       DISPLAY-FILE-NAME.
           IF WS-F-ERROR = 2
               MOVE W02-NETWORK TO WS-FILE
               MOVE WS-NETKEY TO WS-KEY
           ELSE
           IF WS-F-ERROR = 6
               MOVE W02-DEBTOR TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
          ELSE
           IF WS-F-ERROR = 7
               MOVE W02-DEPART TO WS-FILE
               MOVE WS-DEPKEY TO WS-KEY
          ELSE
           IF WS-F-ERROR = 12
               MOVE W02-INVOIC   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE DOC-KEY      TO WS-KEYX
          ELSE
           IF WS-F-ERROR = 15
               MOVE WS-PARID TO WS-FILE
               MOVE WS-PARKEY TO WS-KEY
          ELSE
           IF WS-F-ERROR = 22
               MOVE W02-STOCKF   TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE STK-CODE  TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 25
               MOVE W02-STKMEM   TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE SME-KEY  TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 27
               MOVE W02-DEBMEM   TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE DME-KEY  TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 43
              MOVE W02-CARDEX  TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE CRD-KEY  TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 47
              MOVE W02-DEBALT  TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE DAL-KEY  TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 52
              MOVE W02-DEBTEX  TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE DXT-KEY  TO WS-KEYX.
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
