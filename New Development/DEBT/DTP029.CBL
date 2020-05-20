      $SET LINKCOUNT"576" GNT"DTP029.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****     ******    ******    *
      *   **    **     **     **    **   **  **   **    **  **    **   *
      *   **    **     **     **    **  **    **       **   **    **   *
      *   **    **     **     *******   **    **     **      *******   *
      *   **    **     **     **        **    **   **             **   *
      *   **    **     **     **         **  **   **        **    **   *
      *   *******      **     **          ****    ********   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R   S A L E S   P E R   A C C O U N T            *
      *                                                                *
      *       Version 9.00.82 - April 2016                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  May 2006 - New field included in Accounts Receivable          *
      *             for recording number of outstanding Jobs           *
      *             linked to the account. Additional filler           *
      *             added for future expansion.                        *
      *                                                                *
      *  Jan 2008 - New file (DEBALT) - Debtor alternate index         *
      *             included for lookups, using any word con-          *
      *             tained in the Debtor name.                         *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     DTP029.
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

       COPY "DEBALT.SL".

       COPY "DEBTOR.SL".

       COPY "DEPART.SL".

       COPY "PARAM.SL".

       COPY "STOCK.SL".

       DATA DIVISION.
      *
      *          ********  ********  **        ******** 
      *          **           **     **        **
      *          **           **     **        ** 
      *          *****        **     **        *****
      *          **           **     **        **  
      *          **           **     **        **
      *          **        ********  ********  ********
      *
       FILE SECTION.

       COPY "CARDEX.FDE".

       COPY "CONTROL.FDE".

       COPY "DEBALT.FDE".

       COPY "DEBTOR.FDE".

       COPY "DEPART.FDE".

       COPY "PARAM.FDE".

       COPY "STOCK.FDE".

      *
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   **
      *         **    *    **   **    **   **   **    **  **
      *         **   ***   **   **    **   ******     *****
      *          ** ** ** **    **    **   **   **    **  **
      *           ***   ***     **    **   **    **   **   **
      *            *     *       ******    **    **   **    **
      *
       WORKING-STORAGE SECTION.
       77  WS-CHECK               PIC  X(18)     VALUE "aeWlimemnomLalismJ".
       77  WS-SUB-1                PIC  9(04)     COMP-5.
       77  WS-SUB-2                PIC  9(04)     COMP-5.
       77  WS-SUB-3                PIC  9(04)     COMP-5.
       77  WS-SUB-4                PIC  9(04)     COMP-5.
       77  WS-SUB-5                PIC  9(04)     COMP-5.
       77  WS-SUB-6                PIC  9(04)     COMP-5.
       77  WS-IXS1                 PIC  9(04)     COMP-5.
       77  WS-IXS2                 PIC  9(04)     COMP-5.
       77  WS-IXS3                 PIC  9(04)     COMP-5.
       77  WS-IXS4                 PIC  9(04)     COMP-5.
       77  WS-MSUB                 PIC  9(04)     COMP-5.
       77  WS-PARKEY    PIC 9(04)   COMP-5.
       77  WS-DEPKEY    PIC 9(04)   COMP-5.
       77  WS-DEBKEY    PIC 9(06)   COMP-5.
       77  WS-NETKEY    PIC 9(06)   COMP-5.
       77  WS-WRKEY    PIC 9(04)   COMP-5.
       77  WS-ETYPE     PIC X(01).
       77  WS-LOOK         PIC  X(01) VALUE "S".
       77  WS-CARDEX    PIC X(01).
       77  WS-USE-CASES    PIC X(01).
       77  WS-USE-PACKS    PIC X(01).
       77  WS-USE-ITM    PIC X(01).
       77  WS-EXT-STK    PIC X(01).
       77  WS-NTP    PIC 9(01).
       77  WS-LINES    PIC 9(02).
       77  WS-PAGE    PIC 9(04)   COMP-5.
       77  WS-OPTION    PIC X(01).
       77  WS-ACT    PIC X(01).
       77  WS-STK    PIC X(01).
       77  WS-DEB    PIC X(01).
       77  WS-CREDIT    PIC X(01).
       77  WS-SKIP    PIC X(01).
       77  WS-NUM    PIC Z9.
       77  WS-ADDR    PIC X(01).
       77  WS-ERROR    PIC 9(01).
       77  WS-ERROR1    PIC 9(01).
       77  WS-EXPAND    PIC X(01)  VALUE " ".
       77  WS-CHAR1    PIC X(01).
       77  WS-CHAR2    PIC X(01).
       77  WS-COMP    PIC 9(01).
       77  WS-STM    PIC 9(01).
       77  WS-TYP    PIC X(01).
       77  WS-TYPE    PIC X(01).
       77  WS-HEAD    PIC X(01).
       77  WS-BATCH    PIC 9(05).
       77  WS-MESS    PIC X(02).
       77  WS-ACNO    PIC X(06).
       77  WS-S1    PIC 9(04)   COMP-5.
       77  WS-S2    PIC 9(04)   COMP-5.
       77  WS-S3    PIC 9(04)   COMP-5.
       77  WS-S4    PIC 9(04)   COMP-5.
       77  WS-S5    PIC 9(04)   COMP-5.
       77  WS-S6    PIC 9(04)   COMP-5.
       77  WS-S7    PIC 9(04)   COMP-5.
       77  WS-S8    PIC 9(04)   COMP-5.
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
    03  WS-PROG        PIC  X(03) VALUE "029".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-PRINTER-DETAILS.
           03  W02-PRINTER           PIC  X(12).
           03  W02-PGE-LENGTH        PIC  9(03).
           03  W02-PRN-LENGTH        PIC  9(03).
           03  W02-LINAGE            PIC  9(03).
           03  W02-PRN-STATUS        PIC  X(01)    VALUE "C".
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
           03  W02-PRN-TYPE          PIC  X(01).
           03  W02-PRN-LINE          PIC  X(136).

           03  REP-LINE1                          REDEFINES W02-PRN-LINE.
               05  REP-DETAIL1     PIC  X(136).
                                   
           03  SLD-LINE1                          REDEFINES W02-PRN-LINE.
               05  SLD-DET         PIC  X(132).
                                   
           03  SLD-LINE6                          REDEFINES W02-PRN-LINE.
               05  SLD-PAGE        PIC  X(06).
               05  SLD-P-NO        PIC  Z(03)9.
               05  FILLER          PIC  X(24).
               05  SLD-RESTART.
                   07  SLD-COMPANY PIC  X(40).
                   07  SLD-HEAD    PIC  X(24).
               05  FILLER          PIC  X(17).
               05  SLD-DATE        PIC  X(06).
               05  SLD-DTE         PIC  Z9/99/9999.
                                   
           03  SLD-LINE7                          REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(06).
               05  SLD-CO          PIC  X(30).
               05  SLD-EXHEAD      PIC  X(24).
               05  FILLER          PIC  X(08).
                                   
           03  SLD-LINE8                          REDEFINES W02-PRN-LINE.
               05  SLD-H1          PIC  X(08).
               05  SLD-H2          PIC  X(18).
               05  SLD-H3          PIC  X(08).
               05  SLD-H4          PIC  X(03).
               05  SLD-H5          PIC  X(10).
               05  SLD-H6          PIC  X(16).
               05  SLD-H7          PIC  X(34).
               05  SLD-H8          PIC  X(11).
               05  SLD-H9          PIC  X(15).
               05  SLD-H10         PIC  X(09).
                                   
           03  SLD-LINE9                          REDEFINES W02-PRN-LINE.
               05  SLD-AC          PIC  X(06).
               05  FILLER          PIC  X(02).
               05  SLD-NME         PIC  X(40).
               05  FILLER          PIC  X(84).
                                   
           03  SLD-LINE10                         REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(23).
               05  SLD-DATE1       PIC  Z9/99/9999.
               05  FILLER          PIC  X(01).
               05  SLD-DOC         PIC  X(01).
               05  FILLER          PIC  X(02).
               05  SLD-REF         PIC  X(08).
               05  FILLER          PIC  X(02).
               05  SLD-ITEM        PIC  X(14).
               05  FILLER          PIC  X(02).
               05  SLD-DESC        PIC  X(30).
               05  FILLER          PIC  X(01).
               05  SLD-QTY         PIC  Z(08)9.99-.
               05  FILLER          PIC  X(01).
               05  SLD-SELL        PIC  Z(06)9.99-.
               05  FILLER          PIC  X(01).
               05  SLD-TOTAL       PIC  Z(08)9.99-.

       01  W05-LIST.
          03  W05-OPT        PIC  9(03).
           03  W05-TYPES REDEFINES W05-OPT.
              05  W05-COMP    PIC  9(01).
              05  W05-INDV    PIC  9(01).
              05  W05-INT     PIC  9(01).

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
       COPY "DateVariables.cpy".

       COPY "W15.WS".

       01  W23-PERIOD.
           03  FILLER          PIC X(17) VALUE "For the period : ".
    03  W23-DTE1        PIC Z9/99/9999.
           03  FILLER          PIC X(04) VALUE " to ".
    03  W23-DTE2        PIC Z9/99/9999.

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
    03  W25-QNT        PIC S9(09)V99 COMP-3.
    03  W25-TOTQNT      PIC S9(09)V99 COMP-3.

       COPY "W40.DBT".

       COPY "W40.WS".

       01  W45-TRAN.
          03  W45-CODE             PIC  9(02).
          03  W45-ENG              PIC  X(12).
          03  W45-AFR              PIC  X(12).
          03  W45-ACT              PIC  X(01).
      *
      *    STORAGE FOR PRINTING OF LABELS
      *
       01  W46-LABELS.
           03  W46-LABEL OCCURS 2.
               05  W46-NAME        PIC  X(40).
               05  W46-ADD1        PIC  X(30).
               05  W46-ADD2        PIC  X(30).
               05  W46-ADD3        PIC  X(30).
               05  W46-ADD4        PIC  X(30).
               05  W46-POST.
           07  W46-PC              PIC  9(04).

       COPY "FUNCTION.WS".

       01  W50-STOCK.
           03  W50-STATUS                      PIC  X(02).
           03  W50-LENGTH                      PIC  9(08)   COMP-5.
           03  W50-S1                          PIC  9(02)   COMP-5.
           03  W50-S2                          PIC  9(02)   COMP-5.
           03  W50-DATA.
               05  W50-EXT-ITEM.
                   07  W50-ITEM.
                       09  W50-1               PIC  X(01).
                       09  W50-13.
                           11  W50-9.
                               13  W50-5.
                                   15  W50-I23 PIC  X(02).
                                   15  FILLER  PIC  X(03).
                               13  FILLER      PIC  X(04).
                           11  FILLER          PIC  X(04).
                   07  W50-ITM                 PIC  X(04).
               05  W50-EX-CDE                               REDEFINES W50-EXT-ITEM.
                   07  FILLER                  PIC  X(01).
                   07  W50-17                  PIC  X(17).
           
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
          03  FILLER        PIC  X(26)    VALUE
                             "DEBTOR ACCOUNTS LISTING -".
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

      *
      *    **        ********  **    **  **    **    ****     ******   ********
      *    **           **     ***   **  **   **    **  **   **    **  **
      *    **           **     ****  **  **  **    **    **  **        ** 
      *    **           **     ** ** **  *****     ********  **        *****
      *    **           **     **  ****  **  **    **    **  **   ***  **  
      *    **           **     **   ***  **   **   **    **  **    **  **
      *    ********  ********  **    **  **    **  **    **   ******   ********
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

       01  S04.
    03  LINE  6 COLUMN 12 VALUE "Debtors Required:".
    03  LINE  8 COLUMN 12 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "A".
    03        COLUMN 13 VALUE "ll or ".
    03        COLUMN 19 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "S".
    03        COLUMN 20 VALUE "elective accounts - press ".
    03        COLUMN 46 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "Esc".
    03        COLUMN 49 VALUE "ape to exit".

       01  S05D.
    03  LINE  7 COLUMN 25 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown
         VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
    03  LINE  8 COLUMN 25 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown
         VALUE "³   Transactions for Period.   ³".
    03  LINE  9 COLUMN 25 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown
         VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
    03  LINE 10 COLUMN 25 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown
         VALUE "³  Starting Date :             ³".
    03  LINE 11 COLUMN 25 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown
         VALUE "³  Ending Date   :             ³".
    03  LINE 12 COLUMN 25 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown
         VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
    03  LINE 13 COLUMN 25 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown
         VALUE "³Dates to be keyed in: ".
    03        COLUMN 48 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE "DDMMCCYY".
    03        COLUMN 56 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE "³".
    03  LINE 14 COLUMN 25 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown
         VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
       01  S05A.
    03  LINE 10 COLUMN 44 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC 99/99/9999 USING W15-START AUTO.
    03  LINE 11 COLUMN 44 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC 99/99/9999 USING W15-END AUTO.

       01  S10D.
    03  LINE 10 COLUMN 12 VALUE "Account No.".

       01  S10.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC X(06) USING W80-ACNO AUTO.

       01  S11.
    03  LINE 12 COLUMN 12 VALUE "Details Required:".
    03  LINE 14 COLUMN 12 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "A".
    03        COLUMN 13 VALUE "ll or ".
    03        COLUMN 19 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "S".
    03        COLUMN 20 VALUE "elective Items - press ".
    03        COLUMN 43 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "Esc".
    03        COLUMN 46 VALUE "ape to exit".

       01  S11D.
    03  LINE 16 COLUMN 12 VALUE "Item Code".

       01  S19.
          03  LINE  2 COLUMN 28 FOREGROUND-COLOR Grey HIGHLIGHT
           VALUE "PRINTING SALES STATISTICS".

       01 S36-ITEM.
          03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
              05  S36.
                  07  PIC X(14) USING W10-ITEM AUTO.
              05  S36-3.
                  07  PIC Z(03) USING W10-I4   AUTO.
              05  S36-4.
                  07  PIC Z(04) USING W10-I4   AUTO.
              05  S36-5.
                  07  PIC Z(05) USING W10-I5   AUTO.
              05  S36-6.
                  07  PIC Z(06) USING W10-I6   AUTO.
              05  S36-7.
                  07  PIC Z(07) USING W10-I7   AUTO.
              05  S36-8.
                  07  PIC Z(08) USING W10-I8   AUTO.
              05  S36-9.
                  07  PIC Z(09) USING W10-I9   AUTO.
              05  S36-10.
                  07  PIC Z(10) USING W10-I10  AUTO.
              05  S36-11.
                  07  PIC Z(11) USING W10-I11  AUTO.
              05  S36-12.
                  07  PIC Z(12) USING W10-I12  AUTO.
              05  S36-13.
                  07  PIC Z(13) USING W10-I13  AUTO.
              05  S36-14.
                  07  PIC Z(14) USING W10-I14  AUTO.

       COPY "DEBLUP.CRT".

       COPY "STKLUP.CRT".

      *
      *        *******   *******    ******    ******   ********  *******   **     **  *******    ******** 
      *        **    **  **    **  **    **  **    **  **        **    **  **     **  **    **   **
      *        **    **  **   **   **    **  **        **        **    **  **     **  **   **    **
      *        *******   ******    **    **  **        *****     **    **  **     **  ******     *****
      *        **        **   **   **    **  **        **        **    **  **     **  **   **    **
      *        **        **    **  **    **  **    **  **        **    **  **     **  **    **   **
      *        **        **    **   ******    ******   ********  *******    *******   **    **   ********
      *
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS.
       AA00-MAIN       SECTION.
       AA00-INIT.
           IF LS0-DBLEV < 2
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
            PERFORM ZA00-INIT.
    IF NOT (WS-CARDEX = "Y")
              MOVE "Sales logging not selected"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO AA49.
      PERFORM FA000.
      CLOSE CARDEX
     STOCK.
      *
      *    ****    M U S T   T H E   P R I N T E R   B E   C L O S E D ?
      *
    IF W02-PRN-STATUS = "O"
        MOVE "C"   TO WS-COMMAND
        PERFORM CALL-PRINTUTL.

       AA49.
            EXIT PROGRAM.

       COPY "AA50DS.LUP".

       COPY "DATE.CHK".

       COPY "DATE.CNV".

      *    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *           S C R E E N ,   K E Y B O A R D   &   M O U S E
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³      SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3       ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³                      SCREEN-SHADOW                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To routine is used to display a shadow down the right and ³
      *    ³ along the bottom of a pop-up box. The parameters that are ³
      *    ³ required:                                                 ³
      *    ³          SHADE-ROW   - Top line of the box.               ³
      *    ³          SHADE-COL   - Left line of box.                  ³
      *    ³          SHADE-WIDTH - Width of the box.                  ³
      *    ³          SHADE-LINES - Height of box.                     ³
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
      *       D I S P L A Y   I N F O R M A T I O N   M E S S A G E S
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Move the message to WS-DSP-MES and the top line of the   ³
      *    ³  message box to WS-MES-LINE. If WS-MES-LINE is zero the   ³
      *    ³  default will be 20.                                      ³
      *    ³                  PERFORM DISPLAY-MESSAGE                  ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ****   T H I S   R O U T I N E   I S   U S E D   T O
      *            D I S P L A Y   E R R O R   M E S S A G E S
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

       COPY "LOCKED.REC".

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

       COPY "ABORT.PRN".

       COPY "PRINTUTL.AP5".

      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S
      *
       AA300              SECTION.
       AA301.
            DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT 
       "=Help,"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "F2" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT 
             " Item/ "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "F3" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT 
             " Alt/ "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "F4" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT 
             " Desc/ "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "F5" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT 
             " X/Ref Lookup,"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "Esc" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT 
       "ape"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
       AA305.
             EXIT.
             
       COPY "AA900.ALM".
       
      /
      *    ****    READ FILES ROUTINES
      *
       AC000-READ              SECTION.

       COPY "CARDEX.RD".

       COPY "CONTROL.RD".

       COPY "DEBALT.RD".

       COPY "DEBTOR.RD1".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "STOCK.RD".

       COPY "DEBTOR.ILU".

       COPY "DEBTOR.ALU".

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
      /
       FA000         SECTION 5.
       FA00-INIT.
             PERFORM ERASE-SCREEN.
             DISPLAY S19.

       FA05-ACCEPT.
             MOVE "Are you sure Y/N" TO WS-OPT-MES.
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
          IF WS-OPTION = "N"
               GO TO FA999-EXIT.
      DISPLAY S04.

       FA10-ACCEPT.
            ACCEPT WS-OPTION AT 0861
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO FA999-EXIT
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO FA10-ACCEPT
        END-EVALUATE.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-OPTION = "A" OR "S")
        GO TO FA10-ACCEPT.
      MOVE WS-OPTION  TO WS-ACT.
      MOVE ZERO   TO WS-PAGE
        W25-QNT
        W25-TOTQNT
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
      *
      *    ****   T E S T   I F   O U T P U T T O   P R I N T E R
      *    O R S P O O L  F I L E
      *
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE 3   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
      PERFORM RESTORE-SCREEN-3.

       FA15.
      PERFORM SAVE-SCREEN-3.
      MOVE 8   TO SHADE-ROW.
      MOVE 27   TO SHADE-COL.
      MOVE 30   TO SHADE-WIDTH.
      MOVE 7   TO SHADE-LINES.
      DISPLAY S05D.
      PERFORM SCREEN-SHADOW.
      MOVE W12-TODAY  TO W15-START W15-END.

       FA20.
      DISPLAY "Press " AT 5052
        WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan
       "Esc" WITH FOREGROUND-COLOR Brown HIGHLIGHT
    BACKGROUND-COLOR Cyan
       "ape to Exit"
        WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan.
      ACCEPT S05A.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO FA999-EXIT
   WHEN OTHER  PERFORM AA900-ALARM
       GO TO FA20
        END-EVALUATE.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO FA20.
      PERFORM CLEAR-L50.
      MOVE W15-START  TO W10-EDTE.
             PERFORM CHECK-DATE.
           IF WS-ERROR = 1
              MOVE "Start date" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO FA20.
             MOVE W20-DTE        TO W15-S-YMD.
      MOVE W15-END  TO W10-EDTE.
             PERFORM CHECK-DATE.
           IF WS-ERROR = 1
              MOVE "End date"  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO FA20.
             MOVE W20-DTE        TO W15-E-YMD.
    IF W15-E-YMD < W15-S-YMD
        MOVE "End date is before start date"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO FA20.
      PERFORM RESTORE-SCREEN-3.
      MOVE SPACES  TO W80-ACNO.

       FA25.
    IF WS-ACT = "A"
        GO TO FA40.
      DISPLAY S10D.
      DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT 
       "=Help,"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "F2" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT 
       " to do Account Lookup,"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "F9" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT 
       " Debtor Lookup (Any Word),"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "Esc" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT 
       " to exit"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.

       FA30.
      ACCEPT S10 AT 1024.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W80-ACNO
       IF WS-PAGE > 0
    GO TO FA110
       END-IF
       GO TO FA35
         WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       MOVE SPACE  TO WS-TYP
             PERFORM AA50D THRU AA59
   WHEN F9-KEY
       MOVE "A"  TO WS-TYP
       PERFORM AA50D THRU AA59
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO FA30
        END-EVALUATE
              DISPLAY S10 AT 1024
               IF W80-ACNO = SPACES
           GO TO FA30.

       FA35.
            PERFORM CLEAR-L50.
          IF W80-ACNO = SPACES
              GO TO FA999-EXIT.
      MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W80-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
      PERFORM CA155-GET-DEBTOR.
          IF WS-ERROR = 1
              MOVE "No record"  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO FA30.
           IF WS-ERROR = 2
              MOVE "Invalid account"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO FA30.
      *
      *    ****    D I S P L A Y   D E B T O R   D E T A I L S
      *
            MOVE DEB-NAME  TO W40-NAME.
           IF W80-TYPE = 1
               INSPECT W40-NAME REPLACING ALL "*" BY " "
               INSPECT W40-NAME REPLACING FIRST "!" BY " ".
            DISPLAY W40-NAME AT 1031
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        MOVE SPACES  TO W40-NAME
        DISPLAY W40-NAME AT 1031
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta
        GO TO FA25.

       FA40.
      DISPLAY S11.

       FA45.
            ACCEPT WS-OPTION AT 1458
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO FA999-EXIT
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO FA45
        END-EVALUATE.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-OPTION = "A" OR "S")
        GO TO FA45.
      MOVE WS-OPTION  TO WS-STK.
      MOVE SPACES  TO W10-EXT-ITEM.
      MOVE ZERO   TO W25-TOTAL W25-TOTQNT.

       FA50.
    IF WS-STK = "A"
        GO TO FA60.
      DISPLAY S11D.
            PERFORM AA300.
      *
      *    ****    G E T   S T O C K   C O D E
      *
    IF W10-STCK = "A"
              ACCEPT S36 AT 1622
    ELSE
    IF W10-SLNGTH = 3
        ACCEPT S36-3 AT 1622
    ELSE
    IF W10-SLNGTH = 4
        ACCEPT S36-4 AT 1622
    ELSE
    IF W10-SLNGTH = 5
        ACCEPT S36-5 AT 1622
    ELSE
    IF W10-SLNGTH = 6
        ACCEPT S36-6 AT 1622
    ELSE
    IF W10-SLNGTH = 7
        ACCEPT S36-7 AT 1622
    ELSE
    IF W10-SLNGTH = 8
        ACCEPT S36-8 AT 1622
    ELSE
    IF W10-SLNGTH = 9
        ACCEPT S36-9 AT 1622
    ELSE
    IF W10-SLNGTH = 10
        ACCEPT S36-10 AT 1622
    ELSE
    IF W10-SLNGTH = 11
        ACCEPT S36-11 AT 1622
    ELSE
    IF W10-SLNGTH = 12
        ACCEPT S36-12 AT 1622
    ELSE
    IF W10-SLNGTH = 13
        ACCEPT S36-13 AT 1622
    ELSE
    IF W10-SLNGTH = 14
        ACCEPT S36-14 AT 1622.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
             PERFORM CLEAR-L50
       GO TO FA15
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
       PERFORM AA900-ALARM
       GO TO FA50
        END-EVALUATE
        MOVE "S"   TO WS-LOOK
              PERFORM AA50 THRU AA59
        IF W10-EXT-ITEM = SPACES
     GO TO FA50
        END-IF
              DISPLAY S36 AT 1622.
          IF W10-EXT-ITEM = SPACES
              GO TO FA15.

       FA55.
            PERFORM GA000.
           IF WS-F-ERROR = 22
        MOVE "Z"   TO WS-ETYPE
              PERFORM AA50 THRU AA59
              DISPLAY S36 AT 1622
              IF W10-EXT-ITEM = SPACES
           GO TO FA50
               ELSE
     GO TO FA55.
      DISPLAY STK-DESC AT 1637
       WITH BACKGROUND-COLOR Magenta
     FOREGROUND-COLOR Cyan HIGHLIGHT.
      *
      *    ****    D I S P L A Y   C O N F I R M A T I O N   W I N D O W
      *     B E L O W   I T E M  D E S C R I P T I O N
      *
      MOVE 18   TO SLIN.
      PERFORM CHECK-SETUP THRU CHECK-EXIT.
          IF WS-OPTION = "N"
        GO TO FA50.

       FA60.
    IF WS-ACT = "S"
        MOVE W80-ACNO  TO CRD-AC
    ELSE
        MOVE "!"   TO CRD-AC.
    IF NOT (WS-PRINTER = "S")
        DISPLAY "Reading" AT 1812.
      MOVE ZERO   TO W25-VALUE W25-QNT.

       FA63.
      MOVE W15-S-YMD  TO CRD-DTE.
      PERFORM START-AT-CRDX-ACNT THRU READ-CARDEX-EXIT.
          IF WS-F-ERROR = 43
        GO TO FA85.
      MOVE "N"   TO WS-DEB.

       FA65.
            PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
    IF WS-F-ERROR = 43
        IF (WS-ACT = "S") AND (WS-DEB = "N")
     GO TO FA85
        END-IF
        MOVE "Y"   TO WS-SKIP
        GO TO FA90.
    IF NOT (WS-PRINTER = "S")
        DISPLAY CRD-KEY AT 1820
         WITH FOREGROUND-COLOR Brown HIGHLIGHT.
    IF WS-ACT = "S"
        IF NOT (CRD-AC = W80-ACNO)
     GO TO FA100.
    IF WS-ACT = "A"
        IF W80-ACNO = SPACES
     MOVE CRD-AC  TO W80-ACNO
     MOVE "N"  TO WS-DEB
     GO TO FA63.
      *
      *    ****    H A S   A C C O U N T   C H A N G E D   ?
      *
    IF NOT (CRD-AC = W80-ACNO)
        GO TO FA90.
      *
      *    ****    E N D   O F  E X T R A C T I O N   P E R I O D   ?
      *
    IF CRD-DTE > W15-E-YMD
        GO TO FA90.
      *
      *    ****    O N L Y   P R I N T  S E L E C T E D   I T E M   ?
      *
    IF WS-STK = "S"
        IF NOT (CRD-CODE = W10-EXT-ITEM)
     GO TO FA65.
      *
      *    ****    M U S T   F I R S T  P A G E   H E A D E R  B E
      *     P R I N T E D   ?
      *
    IF WS-PAGE > ZERO
        GO TO FA80.

       FA70.
      MOVE "H"   TO W02-PRN-TYPE.
             MOVE SPACES         TO REP-DETAIL1.
            MOVE "PAGE:"  TO SLD-PAGE.
            MOVE "DATE:"  TO SLD-DATE.
            ADD 1   TO WS-PAGE.
            MOVE W12-TODAY  TO SLD-DTE.
            MOVE WS-PAGE  TO SLD-P-NO.
      *
      *    ****   T E S T   I F   O U T P U T T O   P R I N T E R
      *    O R S P O O L  F I L E
      *
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO SLD-LINE1.
      *
      *    ****   T E S T   I F   O U T P U T T O   P R I N T E R
      *    O R S P O O L  F I L E
      *
    IF NOT (WS-PRINTER = "S")
        MOVE "E"   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO SLD-LINE1.
            MOVE W95-COMP  TO SLD-CO.
            MOVE " - SALES PER DEBTOR"
     TO SLD-EXHEAD.
      MOVE "H"   TO W02-PRN-TYPE.
      *
      *    ****   T E S T   I F   O U T P U T T O   P R I N T E R
      *    O R S P O O L  F I L E
      *
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO SLD-LINE1.
      *
      *    ****   T E S T   I F   O U T P U T T O   P R I N T E R
      *    O R S P O O L  F I L E
      *
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE "X"   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
              MOVE 3   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE "Ac.No."  TO SLD-H1.
      MOVE "Debtors Name" TO SLD-H2.
      MOVE "Date"  TO SLD-H3.
      MOVE "T"   TO SLD-H4.
      MOVE "Refer"  TO SLD-H5.
      MOVE "Item code"  TO SLD-H6.
      MOVE "Description"  TO SLD-H7.
      MOVE "Quantity"  TO SLD-H8.
      MOVE "Sell price"  TO SLD-H9.
      MOVE "Extended"  TO SLD-H10.
      PERFORM CALL-PRINTUTL.
      MOVE ALL "="  TO SLD-LINE1.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE "D"   TO W02-PRN-TYPE.
      *
      *    ****    M U S T   D E B T O R   D E T A I L S   B E
      *     P R I N T E D   ?
      *
       FA80.
    IF WS-DEB = "N"
      *
      *    ****    N E W   P A G E   ?
      *
        IF W02-LINAGE > (W02-PRN-LENGTH - 3)
     MOVE 99  TO WS-ADVANCE
     PERFORM CALL-PRINTUTL
           PERFORM FA70
        END-IF
        MOVE "Y"   TO WS-DEB
        MOVE CRD-AC  TO SLD-AC
        MOVE CRD-NAME  TO SLD-NME
        MOVE 0   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE CRD-DTE  TO W22-DTE2.
      PERFORM SWITCH-DATE-TO-DMCY.
      MOVE W22-DTE3  TO SLD-DATE1.
      COMPUTE W25-TRANVAL = CRD-QNT * CRD-SELL.
    IF CRD-SELL < ZERO
        MOVE "C"   TO SLD-DOC
        COMPUTE W25-TRANVAL = W25-TRANVAL * -1
    ELSE
        MOVE "I"   TO SLD-DOC.
      MOVE CRD-REF  TO SLD-REF.
      MOVE CRD-CODE  TO SLD-ITEM STK-CODE.
      PERFORM READ-STOCK THRU READ-STOCK-EXIT.
      MOVE STK-DESC  TO SLD-DESC.
      MOVE CRD-QNT  TO SLD-QTY.
      MOVE CRD-SELL  TO SLD-SELL.
      MOVE W25-TRANVAL  TO SLD-TOTAL.
      COMPUTE W25-VALUE = W25-VALUE + W25-TRANVAL.
      COMPUTE W25-QNT = W25-QNT + CRD-QNT.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM FA70
        MOVE "N"   TO WS-DEB.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
        GO TO FA115.
             MOVE SPACES         TO REP-DETAIL1.
      GO TO FA65.

       FA85.
    IF WS-ACT = "S"
        MOVE "No data to report"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE.

       FA88.
      PERFORM RESTORE-SCREEN-3.
      MOVE 0901   TO CPOS.
      PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 17.
      PERFORM SAVE-SCREEN-3.
      GO TO FA25.

       FA90.
    IF WS-DEB = "Y"
        ADD W25-QNT  TO W25-TOTQNT
        ADD W25-VALUE  TO W25-TOTAL
        MOVE "Account Total"
     TO SLD-DESC
        MOVE W25-QNT  TO SLD-QTY
        MOVE W25-VALUE  TO SLD-TOTAL
        MOVE 0   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        MOVE SPACES  TO REP-DETAIL1
        MOVE ZERO  TO W25-VALUE
        MOVE ZERO  TO W25-QNT
        MOVE ALL "="  TO SLD-LINE1
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.

       FA95.
    IF WS-ACT = "S"
        MOVE "N"   TO WS-SKIP
        GO TO FA88.
    IF WS-SKIP = "Y"
        GO TO FA110.
      GO TO FA105.

       FA100.
      PERFORM FA90.
    IF WS-ACT = "S"
        GO TO FA88.
      *
      *    ****    F I N D   N E X T   A C C O U N T
      *
       FA105.
    IF NOT (CRD-AC = W80-ACNO)
        MOVE CRD-AC  TO W80-ACNO
        GO TO FA63.
            PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
    IF WS-F-ERROR = 43
        GO TO FA110.
      GO TO FA105.

       FA110.
      MOVE "Total for all Accounts"
     TO SLD-DESC.
      MOVE 2   TO WS-ADVANCE.
      MOVE W25-TOTQNT  TO SLD-QTY.
      MOVE W25-TOTAL  TO SLD-TOTAL.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL1.
      MOVE "****    E N D   O F   R E P O R T    ****"
     TO SLD-NME.

       FA115.
             MOVE 99                 TO WS-ADVANCE.
             MOVE "E"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             MOVE "P"                TO WS-COMMAND.
             MOVE "C"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.

       FA999-EXIT.
             EXIT.
      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   C O D E
      *
       GA000      SECTION 50.
       GA00.
             MOVE 18                 TO W50-LENGTH.
       GA05.
           IF WS-EXT-STK = "Y"
               MOVE W10-EXT-ITEM     TO W50-DATA
           ELSE
               MOVE W10-ITEM         TO W50-DATA
               MOVE W10-ITM          TO W50-ITM.
           IF W50-1 = SPACE
               IF WS-EXT-STK = "Y"
                   MOVE W50-17       TO W10-EXT-ITEM
               ELSE
                   MOVE W50-13       TO W10-ITEM
               END-IF
               GO TO GA05.
             CALL "CBL_TOUPPER" USING W50-DATA BY VALUE W50-LENGTH RETURNING W50-STATUS.
             MOVE W50-EXT-ITEM       TO W10-EXT-ITEM.

       GA06.
             MOVE W50-EXT-ITEM       TO STK-CODE.

       GA10.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
           IF WS-STATUS = "23"
               MOVE W50-EXT-ITEM     TO W10-EXT-CODE2
               PERFORM GA200
               IF NOT(STK-XREF = W10-EXT-ITEM)
                   MOVE 22           TO WS-F-ERROR
               ELSE
                   MOVE STK-CODE     TO W10-EXT-ITEM.
           IF WS-F-ERROR = 0
               PERFORM GA500.

       GA99.
             EXIT.
      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G
      *           A L T E R N A T E   C O D E
      *
       GA100      SECTION 50.
       GA101.
             MOVE 10             TO W50-LENGTH.
       GA105.
             MOVE W10-ACODE      TO W50-DATA.
           IF W50-1 = SPACE
               MOVE W50-9        TO W10-ACODE
              GO TO GA105.
      CALL "CBL_TOUPPER" USING W50-DATA
    BY VALUE W50-LENGTH
    RETURNING W50-STATUS.
            MOVE W50-ACODE  TO STK-ACODE W10-ACODE.
       GA110.
             PERFORM START-AT-ALT-CODE THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
              GO TO GA119.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 0
              PERFORM GA500.
       GA119.
             EXIT.
      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   X R E F
      *
       GA200      SECTION 50.
       GA201.
            MOVE 18   TO W50-LENGTH.

       GA205.
            MOVE W10-EXT-CODE2  TO W50-DATA.
           IF W50-1 = SPACE
        MOVE W50-17  TO W10-EXT-CODE2
        GO TO GA205.
      CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
            MOVE W50-EXT-ITEM  TO W10-EXT-CODE2.

       GA206.
            MOVE W50-EXT-ITEM   TO STK-XREF.

       GA210.
             PERFORM START-AT-STOCK-XREF THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
        GO TO GA299.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
    IF NOT (STK-XREF = W50-EXT-ITEM)
        GO TO GA299.
      PERFORM GA500.

       GA299.
             EXIT.
      *
      *    ****    Calculate the inclusive prices
      *
       GA500      SECTION 50.
       GA505.
      *     Adjust the average cost for negative
      *     or zero quantity
    IF STK-QUANT < 0.001
        MOVE STK-COST  TO STK-AVGE.
      MOVE 1   TO W50-S1.

       GA510.
    IF STK-WSL(W50-S1) = ZERO
        IF STK-CSH(W50-S1) NOT = ZERO
     MOVE STK-CSH(W50-S1)
     TO STK-WSL(W50-S1)
               ELSE
     MOVE STK-SL(W50-S1)
     TO STK-WSL(W50-S1).
          IF STK-CSH(W50-S1) = ZERO
              MOVE STK-SL(W50-S1)
     TO STK-CSH(W50-S1).
      MOVE STK-SL(W50-S1) TO W10-SL(W50-S1).
      MOVE STK-CSH(W50-S1)
     TO W10-CSH(W50-S1).
      MOVE STK-WSL(W50-S1)
     TO W10-WSL(W50-S1).
      MOVE ZERO   TO W05-RATE.
           IF STK-TAX > 0
        MOVE W05-VAT(STK-TAX)
     TO W05-RATE.
      COMPUTE W10-VCSH(W50-S1) ROUNDED
     = W10-CSH(W50-S1) + (W10-CSH(W50-S1) * W05-RTE).
      COMPUTE W10-VWSL(W50-S1) ROUNDED
     = W10-WSL(W50-S1) + (W10-WSL(W50-S1) * W05-RTE).
      COMPUTE W10-VSL(W50-S1) ROUNDED
     = W10-SL(W50-S1) + (W10-SL(W50-S1) * W05-RTE).
    IF W50-S1 < 4
        ADD 1   TO W50-S1
        GO TO GA510.
    IF STK-PRC = ZERO
        MOVE 1   TO STK-PRC.

       GA599.
             EXIT.

      /
       ZA00-INIT       SECTION 90.
       ZA00-OPEN.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ D o   n o t   d i s p l a y   E R R O R   m e s s a g e s ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *      MOVE 44                 TO USER-NUMBER.
      *      MOVE 1                  TO USER-SETTING.
      *      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

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
             MOVE 1                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY        TO W95-COMP.
             MOVE PAR-DMY            TO W12-TODAY.
             MOVE PAR-YMD            TO W12-T-YMD.
           IF PAR-STMINV = "S" OR "X"
              MOVE "N"   TO WS-HEAD
           ELSE
              MOVE "Y"   TO WS-HEAD.
            MOVE 3   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
    IF NOT (PAR-STOCK = "A" OR "N")
        MOVE "A"   TO PAR-STOCK.
      MOVE PAR-STOCK  TO W10-STCK.
      MOVE PAR-PACKS  TO WS-USE-PACKS.
      MOVE PAR-CASES  TO WS-USE-CASES.
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
             MOVE 1                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             GO TO ZA999-EXIT.

       COPY "ZA49.PRO".

       ZA50.
           IF WS-F-ERROR = 22
               MOVE "STOCK file"     TO WS-FILE
           ELSE
           IF WS-F-ERROR = 25
               MOVE "STOCK MEMO"     TO WS-FILE
           ELSE
           IF WS-F-ERROR = 43
               MOVE "CARDEX file"    TO WS-FILE.
             DISPLAY "Rebuild "                  AT 0812 WS-FILE WITH FOREGROUND-COLOR Brown HIGHLIGHT .
             DISPLAY "Press any key to continue" AT 1012.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
       ZA51.
             EXIT PROGRAM.

       ZA200.

       COPY "LOCKED.PRO".

       ZA205.
             EXIT PROGRAM.
       ZA999-EXIT.
             EXIT.
      /
       ZB000       SECTION 9.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 2          MOVE W02-NETWORK   TO WS-FILE
                               MOVE WS-NETKEY     TO WS-KEY
               WHEN 6          MOVE W02-DEBTOR    TO WS-FILE
                               MOVE DEB-ACNO      TO WS-KEYX
               WHEN 7          MOVE W02-DEPART    TO WS-FILE
                               MOVE DPT-CODE      TO WS-KEYX
               WHEN 15         MOVE WS-PARID      TO WS-FILE
                               MOVE WS-PARKEY     TO WS-KEY
               WHEN 22         MOVE W02-STOCKF    TO WS-FILE
                               MOVE STK-CODE      TO WS-KEYX
               WHEN 43         MOVE W02-CARDEX    TO WS-FILE
                               MOVE CRD-KEY       TO WS-KEYX
               WHEN 47         MOVE W02-DEBALT    TO WS-FILE
                               MOVE DAL-KEY       TO WS-KEYX
               WHEN OTHER      MOVE "ERROR"       TO WS-FILE
                               MOVE "DTP029"      TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
