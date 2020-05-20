      $SET LINKCOUNT"512" GNT"DTPIMP.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  ******* ********  ** **  *******    *
      *   ** **     **     **    **    **   ***  ***  **   **   *
      *   ** **     **     **    **    **   ********  **   **   *
      *   ** **     **     *******    **   ** ** **  *******    *
      *   ** **     **     **    **   ** **  **        *
      *   ** **     **     **    **   ** **  **        *
      *   *******      **     ** ********  ** **  **        *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R S   I M P O R T   P R O G R A M         *
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
       PROGRAM-ID.     DTPIMP.
       AUTHOR.         J W LEMMON (CSS).
       DATE-WRITTEN.   SEPTEMBER 1992.

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
                       CONSOLE IS CRT.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY "CONTROL.SL".

       COPY "PARAM.SL".

       COPY "DEBTOR.SL".

    SELECT IMPORT  ASSIGN DISK
           STATUS WS-STATUS
                          ACCESS SEQUENTIAL
                          ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "PARAM.FDE".

       COPY "DEBTOR.FDE".

       FD  IMPORT    LABEL RECORD STANDARD
             VALUE OF FILE-ID W02-IMPORT.
       01  IMP-REC1.
    03  IMP-DATA     PIC  X(1024).

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
       77  WS-SELKEY    PIC 9(06)  COMP-5.
       77  WS-SPAKEY    PIC 9(06)  COMP-5.
       77  WS-PARKEY    PIC 9(06)  COMP-5.
       77  WS-DEPKEY    PIC 9(06)  COMP-5.
       77  WS-NETKEY    PIC 9(06)  COMP-5.
       77  WS-SINKEY    PIC 9(06)  COMP-5.
       77  WS-PRCKEY    PIC 9(06)  COMP-5.
       77  WS-RECKEY    PIC 9(06)  COMP-5.
       77  WS-RECOVER    PIC 9(06)  COMP-5.
       77  WS-TRANS    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-KEYSTR    PIC 9(04)  COMP-5.
       77  WS-GST-RATE    PIC S9(03)V99 COMP-3.
       77  WS-MONTHS    PIC S9(01)V99 COMP-3.
       77  WS-KEY1    PIC 9(04)  COMP-5.
       77  WS-KSTORE    PIC 9(04)  COMP-5.
       77  WS-ETYPE    PIC X(01).
       77  WS-VAT-GST    PIC X(01).
       77  WS-PAGE    PIC 9(04)  COMP-5.
       77  WS-NORM         PIC  X(04).
       77  WS-COND         PIC  X(04).
       77  WS-EXPP         PIC  X(04).
       77  WS-ECAN         PIC  X(04).
       77  WS-8LPI         PIC  X(04).
       77  WS-6LPI         PIC  X(04).
       77  WS-10CPI        PIC  X(04).
       77  WS-12CPI        PIC  X(04).
       77  WS-17CPI        PIC  X(04).
       77  WS-DBLP         PIC  X(04).
       77  WS-DCAN         PIC  X(04).
       77  WS-OPTION    PIC X(01).
       77  WS-ASSIGN    PIC X(01).
       77  WS-SKIP    PIC X(01).
       77  WS-ADJ    PIC X(01).
       77  WS-ERROR    PIC 9(01).
       77  WS-IND    PIC 9(01)  VALUE 0.
       77  WS-INDB    PIC 9(01)  VALUE 0.
       77  WS-INDP    PIC 9(01)  VALUE 0.
       77  WS-INDS    PIC 9(01)  VALUE 0.
       77  WS-AMEND    PIC 9(01)  VALUE 0.
       77  WS-CONREC    PIC 9(01) VALUE ZERO.
       77  WS-LEDG    PIC 9(03).
       77  WS-LEDG1    PIC 9(03).
       77  WS-HYPH    PIC X(14)  VALUE "-".
       77  WS-CHAR1    PIC X(01).
       77  WS-CHAR2    PIC X(01).
       77  WS-COMP    PIC 9(01).
       77  WS-WEEK    PIC 9(01).
       77  WS-TYPE    PIC X(01).
       77  WS-PASSWORD    PIC X(08).
       77  WS-PRINT    PIC 9(01).
       77  WS-PASS    PIC X(08).
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
    03  WS-PROG        PIC  X(03) VALUE "IMP".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-FID.

           03  W02-IMPORT.
               05  W02-IFILE   PIC X(64) VALUE SPACES.

       01  W10.
          03  W10-DATE     PIC X(10).
          03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.

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

       01  W50-NUMERIC-FIELDS.
    03  W50-FIELD-TYPE  PIC  X(01).
    03  W50-DECIMALS    PIC  9(01).
    03  W50-S3        PIC  9(02)  COMP-5.
    03  W50-SIGN        PIC  X(01).
    03  W50-NUMB        PIC S9(14)V9(04).
    03  W50-N-FIELD.
        05  W50-N-CHAR  PIC  X(01) OCCURS 18.
    03  W50-N-FLD1 REDEFINES W50-N-FIELD.
        05  W50-NUMBER  PIC S9(18).
    03  W50-N-FLD2 REDEFINES W50-N-FIELD.
        05  W50-1DEC    PIC S9(17)V9.
    03  W50-N-FLD3 REDEFINES W50-N-FIELD.
        05  W50-2DEC    PIC S9(16)V99.
    03  W50-N-FLD4 REDEFINES W50-N-FIELD.
        05  W50-3DEC    PIC S9(15)V9(03).
    03  W50-N-FLD5 REDEFINES W50-N-FIELD.
        05  W50-4DEC    PIC S9(14)V9(04).
    03  W50-N-FLD6 REDEFINES W50-N-FIELD.
        05  W50-5DEC    PIC S9(13)V9(05).
    03  W50-N-FLD7 REDEFINES W50-N-FIELD.
        05  W50-6DEC    PIC S9(12)V9(06).
    03  W50-N-FLD8 REDEFINES W50-N-FIELD.
        05  W50-7DEC    PIC S9(11)V9(07).
    03  W50-N-FLD9 REDEFINES W50-N-FIELD.
        05  W50-8DEC    PIC S9(10)V9(08).

       01  W55-IMPORT.
           03  W55-S1          PIC  9(04) COMP-5.
    03  W55-IMP-DATA.
        05  W55-IMP     PIC  X(01) OCCURS 1024.

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

       01  W60-FIELDS.
           03  W60-S           PIC  9(02) COMP-5.
          03  W60-FNO        PIC  9(02) COMP-5 OCCURS 60.

       01  W65-FIELDS.
           03  W65-S           PIC  9(02) COMP-5.
           03  W65-FIELD       PIC  X(22).
           03  W65-FLDS.
               05  FILLER      PIC X(21) VALUE "Account Number".
               05  FILLER      PIC X(21) VALUE "Account Type".
               05  FILLER      PIC X(21) VALUE "Account Category".
               05  FILLER      PIC X(21) VALUE "Title Code".
               05  FILLER      PIC X(21) VALUE "Debtor Name".
              05  FILLER      PIC X(21) VALUE "Post. Address Line 1".
              05  FILLER      PIC X(21) VALUE "Post. Address Line 2".
              05  FILLER      PIC X(21) VALUE "Post. Address Line 3".
              05  FILLER      PIC X(21) VALUE "Post. Address Line 4".
              05  FILLER      PIC X(21) VALUE "Post. Address P.Code".
              05  FILLER      PIC X(21) VALUE "Deliv. Addr. Line 1".
              05  FILLER      PIC X(21) VALUE "Deliv. Addr. Line 2".
              05  FILLER      PIC X(21) VALUE "Deliv. Addr. Line 3".
              05  FILLER      PIC X(21) VALUE "Deliv. Addr. Line 4".
              05  FILLER      PIC X(21) VALUE "Deliv. Addr. P.Code".
               05  FILLER      PIC X(21) VALUE "Telephone No 1".
               05  FILLER      PIC X(21) VALUE "Telephone No 2".
               05  FILLER      PIC X(21) VALUE "Fax Number".
               05  FILLER      PIC X(21) VALUE "Id/Reg Number".
               05  FILLER      PIC X(21) VALUE "Contact Person".
               05  FILLER      PIC X(21) VALUE "Date Last Active".
              05  FILLER      PIC X(21) VALUE "Statement Balance".
              05  FILLER      PIC X(21) VALUE "Outstanding Balance".
              05  FILLER      PIC X(21) VALUE "Current".
              05  FILLER      PIC X(21) VALUE "30 Days".
              05  FILLER      PIC X(21) VALUE "60 Days".
              05  FILLER      PIC X(21) VALUE "90 Days".
              05  FILLER      PIC X(21) VALUE "120 Days".
              05  FILLER      PIC X(21) VALUE "Interest".
              05  FILLER      PIC X(21) VALUE "Current Debits".
              05  FILLER      PIC X(21) VALUE "Current Credits".
              05  FILLER      PIC X(21) VALUE "Value Last Payment".
              05  FILLER      PIC X(21) VALUE "Purchases YTD".
              05  FILLER      PIC X(21) VALUE "Purchases Prev Year".
              05  FILLER      PIC X(21) VALUE "Credit Limit".
              05  FILLER      PIC X(21) VALUE "Arrear Interest Ind.".
              05  FILLER      PIC X(21) VALUE "Terms".
              05  FILLER      PIC X(21) VALUE "Bad debt indicator".
              05  FILLER      PIC X(21) VALUE "Language Preference".
              05  FILLER      PIC X(21) VALUE "Date Last Payment".
              05  FILLER      PIC X(21) VALUE "Invoice Type".
              05  FILLER      PIC X(21) VALUE "Address Label Ind.".
              05  FILLER      PIC X(21) VALUE "Statement Type".
              05  FILLER      PIC X(21) VALUE "Price Indicator".
              05  FILLER      PIC X(21) VALUE "Settlement Discount".
              05  FILLER      PIC X(21) VALUE "Discount Code".
              05  FILLER      PIC X(21) VALUE "Sales Person Code".
              05  FILLER      PIC X(21) VALUE "Consolidation Account".
              05  FILLER      PIC X(21) VALUE "Purchases January".
              05  FILLER      PIC X(21) VALUE "Purchases February".
              05  FILLER      PIC X(21) VALUE "Purchases March".
              05  FILLER      PIC X(21) VALUE "Purchases April".
              05  FILLER      PIC X(21) VALUE "Purchases May".
              05  FILLER      PIC X(21) VALUE "Purchases June".
              05  FILLER      PIC X(21) VALUE "Purchases July".
              05  FILLER      PIC X(21) VALUE "Purchases August".
              05  FILLER      PIC X(21) VALUE "Purchases September".
              05  FILLER      PIC X(21) VALUE "Purchases October".
              05  FILLER      PIC X(21) VALUE "Purchases November".
              05  FILLER      PIC X(21) VALUE "Purchases December".
           03  W65-RFLDS REDEFINES W65-FLDS.
        05  W65-FLD     PIC  X(21) OCCURS 60.

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
           03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
           03  W75-DNO        PIC X(06) OCCURS 18.

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
          03  W80-NO.
               05  W80-5N     PIC 9(05).
               05  W80-REC REDEFINES W80-5N
                              PIC ZZZZ9.

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
        05  W100-DTE    PIC  9(08).
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

       01  MENU-INSTRUCT.
    03  LINE 25 COLUMN  2 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 1
     VALUE "Use ".
    03        COLUMN  6 HIGHLIGHT
     BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6
     VALUE "".
    03        COLUMN  7 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 1
     VALUE " or ".
    03        COLUMN 11 HIGHLIGHT
     BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6
     VALUE "".
    03        COLUMN 12 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 1
     VALUE " to point to an item, and ".
    03        COLUMN 38 HIGHLIGHT
     BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6
     VALUE "<Enter>".
    03        COLUMN 45 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 1
     VALUE " to Select ".
    03        COLUMN 56 HIGHLIGHT
     BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6
     VALUE "<Esc>".
    03        COLUMN 61 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 1
     VALUE " to Exit".

       01  S-PROG.
          03  LINE  2 COLUMN 30 FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "DEBTOR IMPORT PROGRAM".

       01  S05.
    03  LINE  4 COLUMN  2 VALUE
        "Select the fields to be Imported".
    03  LINE  6 COLUMN  2 VALUE
        "Fields must be in the sequence they appear on the input
      -        "file".
    03  LINE  8 COLUMN  2 VALUE "Field :".
    03  LINE 16 COLUMN  2 FOREGROUND-COLOR 6 HIGHLIGHT
        VALUE "The first record is assumed to be a header".

       01  S06.
    03  LINE 10 COLUMN  2 VALUE "Fields:".
    03        COLUMN 10 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (1).
    03        COLUMN 13 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (2).
    03        COLUMN 16 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (3).
    03        COLUMN 19 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (4).
    03        COLUMN 22 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (5).
    03        COLUMN 25 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (6).
    03        COLUMN 28 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (7).
    03        COLUMN 31 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (8).
    03        COLUMN 34 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (9).
    03        COLUMN 37 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (10).
    03        COLUMN 40 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (11).
    03        COLUMN 43 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (12).
    03        COLUMN 46 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (13).
    03        COLUMN 49 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (14).
    03        COLUMN 52 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (15).
    03        COLUMN 55 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (16).
    03        COLUMN 58 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (17).
    03        COLUMN 61 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (18).
    03        COLUMN 64 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (19).
    03        COLUMN 67 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (20).
    03        COLUMN 70 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (21).
    03        COLUMN 73 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (22).
    03  LINE 12 COLUMN 10 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (23).
    03        COLUMN 13 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (24).
    03        COLUMN 16 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (25).
    03        COLUMN 19 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (26).
    03        COLUMN 22 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (27).
    03        COLUMN 25 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (28).
    03        COLUMN 28 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (29).
    03        COLUMN 31 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (30).
    03        COLUMN 34 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (31).
    03        COLUMN 37 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (32).
    03        COLUMN 40 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (33).
    03        COLUMN 43 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (34).
    03        COLUMN 46 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (35).
    03        COLUMN 49 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (36).
    03        COLUMN 52 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (37).
    03        COLUMN 55 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (38).
    03        COLUMN 58 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (39).
    03        COLUMN 61 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (40).
    03        COLUMN 64 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC ZZ FROM W60-FNO (41).
    03        COLUMN 67 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC ZZ FROM W60-FNO (42).
    03        COLUMN 70 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC ZZ FROM W60-FNO (43).
    03        COLUMN 73 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC ZZ FROM W60-FNO (44).
    03  LINE 14 COLUMN 10 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC ZZ FROM W60-FNO (45).
    03        COLUMN 13 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC ZZ FROM W60-FNO (46).
    03        COLUMN 16 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC ZZ FROM W60-FNO (47).
    03        COLUMN 19 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC ZZ FROM W60-FNO (48).
    03        COLUMN 22 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC ZZ FROM W60-FNO (49).
    03        COLUMN 25 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC ZZ FROM W60-FNO (50).
    03        COLUMN 28 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC ZZ FROM W60-FNO (51).
    03        COLUMN 31 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC ZZ FROM W60-FNO (52).
    03        COLUMN 34 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC ZZ FROM W60-FNO (53).
    03        COLUMN 37 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC ZZ FROM W60-FNO (54).
    03        COLUMN 40 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC ZZ FROM W60-FNO (55).
    03        COLUMN 43 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC ZZ FROM W60-FNO (56).
    03        COLUMN 46 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC ZZ FROM W60-FNO (57).
    03        COLUMN 49 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC ZZ FROM W60-FNO (58).
    03        COLUMN 52 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC ZZ FROM W60-FNO (59).
    03        COLUMN 55 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC ZZ FROM W60-FNO (60).

       01  S07.
    03  LINE 18 COLUMN  2 VALUE "Importing:".

       01  S08.
    03  LINE 18 COLUMN 13 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(06) FROM DEB-ACNO.
    03        COLUMN 20 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC  X(40) FROM DEB-NAME.

       01  S09.
    03  LINE  8 COLUMN 10 FOREGROUND-COLOR 6 HIGHLIGHT
                                 PIC 99 FROM W65-S.
    03        COLUMN 13 FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC  X(21) FROM W65-FIELD.

      /
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS.
       AA000-MAIN        SECTION.
       AA000-INIT.
           IF LS0-DBLEV < 9
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM BA000.
             CLOSE IMPORT.

       AA49.
             EXIT PROGRAM.

       COPY "FUNCTION.CRT".

       COPY "OPTION.CRT".

       COPY "LOCKED.RC2".

       COPY "CLEAR.CRT".

      *
      *    ****    G E T   T H E   I M P O R T  F I L E   I D
      *
       AA100           SECTION.
       AA101.
             DISPLAY "Keyin the <pathname>/<filename> " AT 2202.
      DISPLAY "IMPORT file :" AT 2302.
             MOVE ZERO           TO WS-ERROR.
       AA105.
      ACCEPT W02-IMPORT AT 2316
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
      CALL "CBL_TOUPPER" USING W02-IMPORT
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
      DISPLAY W02-IMPORT AT 2316
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5 UPDATE AUTO.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO AA105.
      OPEN INPUT IMPORT.
           IF WS-STATUS = "00"
        GO TO AA199.
      MOVE "Invalid file name"
     TO WS-ERR-MES.
            PERFORM ERROR-MESSAGE.
             GO TO AA105.
       AA199.
             EXIT.
      *
      *    ****    E X T R A C T   I M P O R T
      *
      *     W50-DATA   : EXTRACTED FIELD FROM IMPORTED DATA
      *            W50-LENGTH : LENGTH OF DATA
      *            W50-EXP    : FIELD TERMINATION CHARACTER
      *
       AA200           SECTION.
       AA201.
      MOVE SPACES  TO W50-DATA.
             MOVE W50-LENGTH     TO W50-S1.
      MOVE 1   TO W50-S2.
      *
      *    ****    M O V E   F I E L D  T O   W O R K  A R E A
      *
       AA205.
    IF NOT (W55-IMP (W55-S1) = W50-EXP OR QUOTE)
        MOVE W55-IMP (W55-S1)
     TO W50-CCHAR (W50-S2)
        ADD 1   TO W50-S2 W55-S1
               GO TO AA205.
      *
      *   ****   S O M E   P R O G R A M S   E N C L O S E   A L P H A -
      *    N U M E R I C   F I E L D S W I T H   "
      *
    IF W55-IMP (W55-S1) = QUOTE
        ADD 1   TO W55-S1
        GO TO AA205.
      ADD 1   TO W55-S1.
      *
      *    ****    I F  A L P H A - N U M E R I C   T H E N   E X I T
      *
    IF W50-FIELD-TYPE = "A"
        GO TO AA299.
      MOVE 18   TO W50-S3.
      MOVE 0   TO WS-S3 W50-DECIMALS W50-NUMBER.
    IF W50-S2 = 1
        GO TO AA220.
      *
      *    ****    D E T E R M I N E   T H E   S I G N  O F   T H E
      *     N U M E R I C   F I E L D
      *
      MOVE "+"   TO W50-SIGN.
      *
      *    ****    M O V E   D A T A   T O   N U M B E R S
      *     W O R K   F I E L D
      *
       AA210.
    IF W50-CCHAR (W50-S2) = "-"
        MOVE "-"   TO W50-SIGN
        GO TO AA215.
    IF W50-CCHAR (W50-S2) = "."
        MOVE WS-S3  TO W50-DECIMALS
        GO TO AA215.
    IF W50-CCHAR (W50-S2) NOT NUMERIC
        GO TO AA215.
      MOVE W50-CCHAR (W50-S2)
     TO W50-N-CHAR (W50-S3).
      SUBTRACT 1   FROM W50-S3.
      *
      *    ****    D E T E R M I N E   T H E   D I G I T S   A F T E R
      *     T H E   D E C I M A L   P O I N T
      *
    IF W50-DECIMALS = 0
        ADD 1   TO WS-S3.
      *
      *    ****    E N D   O F  F I E L D   C O N T R O L   L O O P
      *
       AA215.
    IF W50-S2 > 1
        SUBTRACT 1  FROM W50-S2
        GO TO AA210.
      *
      *    ****    S T O R E   N U M B E R   A S   S9(14)V9(04).
      *
       AA220.
    IF W50-DECIMALS = 0
        MOVE W50-NUMBER  TO W50-NUMB
    ELSE
    IF W50-DECIMALS = 1
        MOVE W50-1DEC  TO W50-NUMB
    ELSE
    IF W50-DECIMALS = 2
        MOVE W50-2DEC  TO W50-NUMB
    ELSE
    IF W50-DECIMALS = 3
        MOVE W50-3DEC  TO W50-NUMB
    ELSE
    IF W50-DECIMALS = 4
        MOVE W50-4DEC  TO W50-NUMB
    ELSE
    IF W50-DECIMALS = 5
        MOVE W50-5DEC  TO W50-NUMB
    ELSE
    IF W50-DECIMALS = 6
        MOVE W50-6DEC  TO W50-NUMB
    ELSE
    IF W50-DECIMALS = 7
        MOVE W50-7DEC  TO W50-NUMB
    ELSE
    IF W50-DECIMALS = 8
        MOVE W50-8DEC  TO W50-NUMB
    ELSE
        MOVE ZERO  TO W50-NUMB.
      *
      *    ****    C H E C K   I F   V A L U E  M U S T   B E
      *     S W I T C H E D   T O   N E G A T I V E
      *
    IF W50-SIGN = "-"
        MULTIPLY -1  BY W50-NUMB.
       AA299.
             EXIT.
      *
      *    ****    M O V E   I N D V I D U A L   F I E L D S
      *
      *            W50-BIN : FIELD NUMBER TO MOVE
      *
       AA300              SECTION.
       AA301.
    IF W50-BIN < 01 OR > 41
               GO TO AA399.
      MOVE "N"   TO W50-FIELD-TYPE.
      *
      *        A C C O U N T   N U M B E R
      *
           IF W50-BIN = 01
        MOVE 6   TO W50-LENGTH
        MOVE "A"   TO W50-FIELD-TYPE
               PERFORM AA200
        MOVE W50-DATA  TO W80-ACNO
               GO TO AA399.
      *
      *        A C C O U N T   T Y P E
      *
           IF W50-BIN = 02
        MOVE 1   TO W50-LENGTH
               PERFORM AA200
        MOVE W50-NUMB  TO W80-TYPE
        GO TO AA399.
      *
      *        D E B T O R   C A T E G O R Y
      *
           IF W50-BIN = 03
        MOVE 2   TO W50-LENGTH
               PERFORM AA200
        MOVE W50-NUMB  TO W40-CAT
               GO TO AA399.
      *
      *        L A N G U A G E
      *
           IF W50-BIN = 04
        MOVE 1   TO W50-LENGTH
        MOVE "A"   TO W50-FIELD-TYPE
               PERFORM AA200
        MOVE W50-DATA  TO W40-LANG
        GO TO AA399.
      *
      *        N A M E
      *
           IF W50-BIN = 05
        MOVE 40   TO W50-LENGTH
        MOVE "A"   TO W50-FIELD-TYPE
               PERFORM AA200
        MOVE W50-DATA  TO W40-NAME
      *
      *        DISPLAY W40-NAME AT 2403
      *        CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS
      *
               GO TO AA399.
      *
      *        P O S T A L   A D D R E S S - L I N E   1
      *
           IF W50-BIN = 06
        MOVE 30   TO W50-LENGTH
        MOVE "A"   TO W50-FIELD-TYPE
               PERFORM AA200
        MOVE W50-DATA  TO W40-ADD1
               GO TO AA399.
      *
      *        P O S T A L   A D D R E S S - L I N E   2
      *
    IF W50-BIN = 07
        MOVE 30   TO W50-LENGTH
        MOVE "A"   TO W50-FIELD-TYPE
               PERFORM AA200
        MOVE W50-DATA  TO W40-ADD2
               GO TO AA399.
      *
      *        P O S T A L   A D D R E S S - L I N E   3
      *
    IF W50-BIN = 08
        MOVE 30   TO W50-LENGTH
        MOVE "A"   TO W50-FIELD-TYPE
               PERFORM AA200
        MOVE W50-DATA  TO W40-ADD3
               GO TO AA399.
      *
      *        P O S T A L   A D D R E S S - L I N E   4
      *
    IF W50-BIN = 09
        MOVE 30   TO W50-LENGTH
        MOVE "A"   TO W50-FIELD-TYPE
               PERFORM AA200
        MOVE W50-DATA  TO W40-ADD4
               GO TO AA399.
      *
      *        P O S T A L   A D D R E S S  -  P O S T  C O D E
      *
    IF W50-BIN = 10
        MOVE 4   TO W50-LENGTH
               PERFORM AA200
        MOVE W50-NUMB  TO W40-POST
               GO TO AA399.
      *
      *        D E L I V E R Y  A D D R E S S -  L I N E   1
      *
    IF W50-BIN = 11
        MOVE 30   TO W50-LENGTH
        MOVE "A"   TO W50-FIELD-TYPE
               PERFORM AA200
        MOVE W50-DATA  TO W40-PADD1
               GO TO AA399.
      *
      *        D E L I V E R Y  A D D R E S S -  L I N E   2
      *
    IF W50-BIN = 12
        MOVE 30   TO W50-LENGTH
        MOVE "A"   TO W50-FIELD-TYPE
               PERFORM AA200
        MOVE W50-DATA  TO W40-PADD2
               GO TO AA399.
      *
      *        D E L I V E R Y  A D D R E S S -  L I N E   3
      *
    IF W50-BIN = 13
        MOVE 30   TO W50-LENGTH
        MOVE "A"   TO W50-FIELD-TYPE
               PERFORM AA200
        MOVE W50-DATA  TO W40-PADD3
               GO TO AA399.
      *
      *        D E L I V E R Y  A D D R E S S -  L I N E   4
      *
    IF W50-BIN = 14
        MOVE 30   TO W50-LENGTH
        MOVE "A"   TO W50-FIELD-TYPE
               PERFORM AA200
        MOVE W50-DATA  TO W40-PADD4
               GO TO AA399.
      *
      *        D E L I V E R Y  A D D R E S S -  P O S T   C O D E
      *
    IF W50-BIN = 15
        MOVE 4   TO W50-LENGTH
               PERFORM AA200
        MOVE W50-NUMB  TO W40-PPOST
        GO TO AA399.
      *
      *        H O M E  T E L E P H O N E
      *
    IF W50-BIN = 16
        MOVE 14   TO W50-LENGTH
        MOVE "A"   TO W50-FIELD-TYPE
               PERFORM AA200
        MOVE W50-DATA  TO W40-HTEL
               GO TO AA399.
      *
      *        B U S I N E S S  T E L E P H O N E
      *
    IF W50-BIN = 17
        MOVE 14   TO W50-LENGTH
        MOVE "A"   TO W50-FIELD-TYPE
               PERFORM AA200
        MOVE W50-DATA  TO W40-TEL
               GO TO AA399.
      *
      *        F A X   N U M B E R
      *
    IF W50-BIN = 18
        MOVE 14   TO W50-LENGTH
        MOVE "A"   TO W50-FIELD-TYPE
               PERFORM AA200
        MOVE W50-DATA  TO W40-FAX
               GO TO AA399.
      *
      *        I D E N T I T Y  /   R E G I S T R A T I O N
      *
    IF W50-BIN = 19
        MOVE 14   TO W50-LENGTH
        MOVE "A"   TO W50-FIELD-TYPE
               PERFORM AA200
        MOVE W50-DATA  TO W40-ID
               GO TO AA399.
      *
      *        C O N T A C T   P E R S O N
      *
    IF W50-BIN = 20
        MOVE 30   TO W50-LENGTH
        MOVE "A"   TO W50-FIELD-TYPE
               PERFORM AA200
        MOVE W50-DATA  TO W40-CONTACT
               GO TO AA399.
      *
      *        D A T E  L A S T   A C T I V E
      *
    IF W50-BIN = 21
        MOVE 8   TO W50-LENGTH
               PERFORM AA200
        MOVE W50-NUMB  TO W40-ACTIVE
               GO TO AA399.
      *
      *        S T A T E M E N T   B A L A N C E
      *
    IF W50-BIN = 22
        MOVE 13   TO W50-LENGTH
               PERFORM AA200
        MOVE W50-NUMB  TO W40-STM
               GO TO AA399.
      *
      *        O U T S T A N D I N G   B A L A N C E
      *
    IF W50-BIN = 23
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
        MOVE W50-NUMB  TO W40-BAL
               GO TO AA399.
      *
    IF W50-BIN = 24
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
        MOVE W50-NUMB  TO W40-CUR
               GO TO AA399.
      *
    IF W50-BIN = 25
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
        MOVE W50-NUMB  TO W40-30
               GO TO AA399.
      *
    IF W50-BIN = 26
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
        MOVE W50-NUMB  TO W40-60
               GO TO AA399.
      *
    IF W50-BIN = 27
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
        MOVE W50-NUMB  TO W40-90
               GO TO AA399.
      *
    IF W50-BIN = 28
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
        MOVE W50-NUMB  TO W40-120
               GO TO AA399.
      *
    IF W50-BIN = 29
        MOVE 9   TO W50-LENGTH
               PERFORM AA200
        MOVE W50-NUMB  TO W40-INT
               GO TO AA399.
      *
    IF W50-BIN = 30
        MOVE 13   TO W50-LENGTH
               PERFORM AA200
        MOVE W50-NUMB  TO W40-DEBT
               GO TO AA399.
      *
    IF W50-BIN = 31
        MOVE 13   TO W50-LENGTH
               PERFORM AA200
        MOVE W50-NUMB  TO W40-CRED
               GO TO AA399.
      *
    IF W50-BIN = 32
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
        MOVE W50-NUMB  TO W40-LAST-PMT
               GO TO AA399.
      *
    IF W50-BIN = 33
        MOVE 13   TO W50-LENGTH
               PERFORM AA200
        MOVE W50-NUMB  TO W40-PURYTD
               GO TO AA399.
      *
    IF W50-BIN = 34
        MOVE 13   TO W50-LENGTH
               PERFORM AA200
        MOVE W50-NUMB  TO W40-PURPRV
               GO TO AA399.
      *
    IF W50-BIN = 35
        MOVE 7   TO W50-LENGTH
               PERFORM AA200
        MOVE W50-NUMB  TO W40-CRL
               GO TO AA399.
      *
    IF W50-BIN = 36
        MOVE 1   TO W50-LENGTH
        MOVE "A"   TO W50-FIELD-TYPE
               PERFORM AA200
        MOVE W50-DATA  TO W40-FIN
               GO TO AA399.
      *
    IF W50-BIN = 37
        MOVE 1   TO W50-LENGTH
               PERFORM AA200
        MOVE W50-NUMB  TO W40-TERMS
               GO TO AA399.
      *
    IF W50-BIN = 38
        MOVE 1   TO W50-LENGTH
        PERFORM AA200
        MOVE W50-NUMB  TO W40-BDT
               GO TO AA399.
      *
    IF W50-BIN = 39
        MOVE 1   TO W50-LENGTH
        MOVE "A"   TO W50-FIELD-TYPE
               PERFORM AA200
        MOVE W50-DATA  TO W40-LANG
               GO TO AA399.
      *
    IF W50-BIN = 40
        MOVE 8   TO W50-LENGTH
        PERFORM AA200
        MOVE W50-NUMB  TO W40-PMT
               GO TO AA399.
      *
    IF W50-BIN = 41
        MOVE 1   TO W50-LENGTH
        MOVE "A"   TO W50-FIELD-TYPE
               PERFORM AA200
        MOVE W50-DATA  TO W40-INV-TYPE
               GO TO AA399.
      *
    IF W50-BIN = 42
        MOVE 1   TO W50-LENGTH
        MOVE "A"   TO W50-FIELD-TYPE
               PERFORM AA200
        MOVE W50-DATA  TO W40-LAB
               GO TO AA399.
      *
    IF W50-BIN = 43
        MOVE 1   TO W50-LENGTH
        MOVE "A"   TO W50-FIELD-TYPE
        PERFORM AA200
        MOVE W50-DATA  TO W40-OI
               GO TO AA399.
      *
    IF W50-BIN = 44
        MOVE 1   TO W50-LENGTH
        MOVE "A"   TO W50-FIELD-TYPE
        PERFORM AA200
        MOVE W50-DATA  TO W40-PRICE
               GO TO AA399.
      *
    IF W50-BIN = 45
        MOVE 2   TO W50-LENGTH
               PERFORM AA200
        MOVE W50-NUMB  TO W40-SDISC
               GO TO AA399.
      *
    IF W50-BIN = 46
        MOVE 2   TO W50-LENGTH
               PERFORM AA200
        MOVE W50-NUMB  TO W40-DISC
               GO TO AA399.
      *
    IF W50-BIN = 47
        MOVE 2   TO W50-LENGTH
               PERFORM AA200
        MOVE W50-NUMB  TO W40-SMAN
               GO TO AA399.
      *
    IF W50-BIN = 48
        MOVE 6   TO W50-LENGTH
        MOVE "A"   TO W50-FIELD-TYPE
               PERFORM AA200
        MOVE W50-DATA  TO W40-CONAC
               GO TO AA399.
      *
    IF W50-BIN = 49
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
        MOVE W50-NUMB  TO W40-MTHPUR (1)
        GO TO AA399.
      *
    IF W50-BIN = 50
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
        MOVE W50-NUMB  TO W40-MTHPUR (2)
        GO TO AA399.
      *
    IF W50-BIN = 51
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
        MOVE W50-NUMB  TO W40-MTHPUR (3)
        GO TO AA399.
      *
    IF W50-BIN = 52
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
        MOVE W50-NUMB  TO W40-MTHPUR (4)
        GO TO AA399.
      *
    IF W50-BIN = 53
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
        MOVE W50-NUMB  TO W40-MTHPUR (5)
        GO TO AA399.
      *
    IF W50-BIN = 54
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
        MOVE W50-NUMB  TO W40-MTHPUR (6)
        GO TO AA399.
      *
    IF W50-BIN = 55
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
        MOVE W50-NUMB  TO W40-MTHPUR (7)
        GO TO AA399.
      *
    IF W50-BIN = 56
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
        MOVE W50-NUMB  TO W40-MTHPUR (8)
        GO TO AA399.
      *
    IF W50-BIN = 57
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
        MOVE W50-NUMB  TO W40-MTHPUR (9)
        GO TO AA399.
      *
    IF W50-BIN = 58
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
        MOVE W50-NUMB  TO W40-MTHPUR (10)
        GO TO AA399.
      *
    IF W50-BIN = 59
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
        MOVE W50-NUMB  TO W40-MTHPUR (11)
        GO TO AA399.
      *
    IF W50-BIN = 60
        MOVE 13   TO W50-LENGTH
        PERFORM AA200
        MOVE W50-NUMB  TO W40-MTHPUR (12).

       AA399.
               EXIT.
      *
      *    ****    P R O C E S S   T H E   S E L E C T E D   F I E L D S
      *
       AA400           SECTION.
       AA401.
             MOVE 1              TO W60-S.
      *
      *    ****    S E T U P   I M P O R T   D A T A   A R E A
      *
      MOVE IMP-DATA  TO W55-IMP-DATA.
             MOVE 1              TO W55-S1.
       AA405.
           IF W60-FNO (W60-S) = ZERO
               GO TO AA499.
             MOVE W60-FNO (W60-S)
                                 TO W50-BIN.
             PERFORM AA300.
    IF W60-S < 60
               ADD 1             TO W60-S
               GO TO AA405.
       AA499.
             EXIT.

       COPY "DATE.CHK".

      /
      *    ****    READ FILES ROUTINES
      *
       AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "DEBTOR.RD".

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
       AD000-WRITE             SECTION.

       COPY "CONTROL.WR".

       COPY "DEBTOR.WR".

       BA000        SECTION 1.
       BA00.
             PERFORM ERASE-SCREEN.
      DISPLAY S-PROG.
             DISPLAY S05.
      INITIALIZE W60-FIELDS.
             MOVE 1              TO W60-S.
001020      DISPLAY " " AT 1803 WITH FOREGROUND-COLOR 14
                     " to view the fields available "
                      WITH FOREGROUND-COLOR 7
                     "S" WITH FOREGROUND-COLOR 14
                     "elect/" WITH FOREGROUND-COLOR 7
                     "A" WITH FOREGROUND-COLOR 14
       "ccept - " WITH FOREGROUND-COLOR 7
                     "Esc" WITH FOREGROUND-COLOR 14
                     "ape" WITH FOREGROUND-COLOR 7.
             MOVE 1              TO W65-S.

       BA05.
             MOVE W65-FLD (W65-S)
                                 TO W65-FIELD.
             DISPLAY S09.

       BA10.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      DISPLAY SPACE AT 5041
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
       IF W65-S = 1
    GO TO BA10
       ELSE
    SUBTRACT 1  FROM W65-S
    GO TO BA05
       END-IF
   WHEN DOWN-KEY
       IF W65-S NOT < 60
    GO TO BA10
       ELSE
    ADD 1  TO W65-S
    GO TO BA05
       END-IF
   WHEN ENTER-KEY
   WHEN CR-KEY
       GO TO BA15
   WHEN OTHER
       CALL X"E5"
        END-EVALUATE
        GO TO BA10
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BA999
   WHEN OTHER
       CALL X"E5"
        END-EVALUATE
        GO TO BA10
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
           IF NOT (WS-OPTION = "A" OR "S")
        GO TO BA10.
           IF WS-OPTION = "A"
        GO TO BA30.

       BA15.
             MOVE 1              TO WS-S1.

       BA20.
           IF WS-S1 = W60-S
        GO TO BA25.
           IF W60-FNO (WS-S1) = W65-S
        GO TO BA10.
             ADD 1               TO WS-S1.
      GO TO BA20.

       BA25.
             MOVE W65-S          TO W60-FNO (W60-S).
             DISPLAY S06.
             ADD 1               TO W60-S.
    IF W65-S < 60
               ADD 1             TO W65-S.
      GO TO BA05.

       BA30.
             PERFORM AA100.
    IF WS-ERROR = 9
        GO TO BA999.
      READ IMPORT.
             DISPLAY S07.
      MOVE ZERO   TO WS-PRCKEY WS-RECKEY.

       BA35.
           IF WS-RECKEY = 100
        MOVE ZERO  TO WS-RECKEY
              CLOSE DEBTOR
              OPEN I-O DEBTOR.
      READ IMPORT AT END
    GO TO BA40.
      INITIALIZE W40-DEBTREC.
      MOVE "E"   TO W40-LANG.
             MOVE "E"            TO W40-INV-TYPE.
             MOVE "Y"            TO W40-FIN.
             MOVE "Y"            TO W40-LAB.
             MOVE "O"            TO W40-OI.
      MOVE "1"   TO W40-PRICE.
      MOVE 01   TO W40-CAT.
      MOVE SPACES  TO W80-ACNO.
      MOVE 2   TO W80-TYPE.
      MOVE 1   TO WS-S1.
      MOVE ZERO   TO W40-CRL
        W40-BAL
        W40-STM
        W40-ONE
        W40-MTD
        W40-CUR
        W40-30
        W40-60
        W40-90
        W40-120
        W40-INT
        W40-DEBT
        W40-CRED
        W40-LAST-PMT
        W40-PURYTD
        W40-PURPRV
        W40-ACTIVE
        W40-PMT.

       BA36.
      MOVE ZERO   TO W40-MTHPUR (WS-S1).
    IF WS-S1 < 12
        ADD 1   TO WS-S1
        GO TO BA36.
      PERFORM AA400.
    IF W80-ACNO = SPACES AND
       W40-NAME = SPACES
        GO TO BA35.
    IF W80-ACNO = SPACES
        MOVE "Y"   TO WS-ASSIGN
    ELSE
        MOVE "N"   TO WS-ASSIGN.
            MOVE 1   TO WS-NETKEY.
             PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.

       BA38.
    IF WS-ASSIGN = "Y"
        IF DEB-AC-NO = ZERO
     ADD 1 TO DEB-AC-NO
        END-IF
        MOVE DEB-AC-NO TO DEB-ACNO
              PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT
        IF WS-F-ERROR = 0
     IF DEB-AC-NO = 999999
         MOVE 1 TO DEB-AC-NO
         GO TO BA38
     ELSE
         ADD 1 TO DEB-AC-NO
         GO TO BA38
     END-IF
        ELSE
     MOVE DEB-ACNO TO W80-ACNO.
             INITIALIZE DEB-RECORD1.
      ADD 1   TO WS-PRCKEY WS-RECKEY.
      MOVE WS-PRCKEY  TO W80-REC.
      DISPLAY W80-NO AT 1813 WITH FOREGROUND-COLOR 11.
      PERFORM CA155-GET-DEBTOR.
    IF WS-F-ERROR = 6
        PERFORM DA000.
      GO TO BA35.

       BA40.
      DISPLAY "Update complete - Press " AT 5012
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
      BACKGROUND-COLOR 3 "ENTER"
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 3.
      ACCEPT WS-OPTION AT 5042 WITH FOREGROUND-COLOR 15 AUTO.

       BA999.
             EXIT.
      *
      *    ****    Pack the debtor address for storage in the
      *            debtor record
      *
       CA110-ADDRESS       SECTION 2.
       CA110.
             MOVE ZERO           TO WS-ERROR.
             MOVE 1              TO WS-S1 WS-S2.
             MOVE 30             TO WS-S3.
             MOVE SPACES         TO W60-ADDRESS.

       CA115.
           IF WS-S6 NOT = 99
               IF W40-ACHAR (WS-S3) NOT = SPACE
                   GO TO CA120
               ELSE
                   NEXT SENTENCE
           ELSE
           IF W40-PCHAR (WS-S3) NOT = SPACE
               GO TO CA120.
           IF WS-S3 > WS-S2
               SUBTRACT 1        FROM WS-S3
               GO TO CA115.

       CA120.
           IF WS-S6 NOT = 99
               MOVE W40-ACHAR (WS-S2)
                                 TO W60-ACHAR (WS-S1)
           ELSE
               MOVE W40-PCHAR (WS-S2)
                                 TO W60-ACHAR (WS-S1).
           IF WS-S2 = WS-S3
               GO TO CA125.
             ADD 1               TO WS-S1.
             ADD 1               TO WS-S2.
           IF WS-S1 > 70
               MOVE 1            TO WS-ERROR
           ELSE
               GO TO CA120.

       CA125.
           IF WS-ERROR = 1
               GO TO CA130.
           IF WS-S1 < 70
               ADD 1             TO WS-S1
               MOVE "*"          TO W60-ACHAR (WS-S1).

       CA130.
             ADD 1               TO WS-S1.

       CA135.
             MOVE 31             TO WS-S2.
             MOVE 60             TO WS-S3.
             PERFORM CA115 THRU CA130.
           IF WS-ERROR = 1
               GO TO CA140-EXIT.
             MOVE 61             TO WS-S2.
             MOVE 90             TO WS-S3.
             PERFORM CA115 THRU CA130.
           IF WS-ERROR = 1
               GO TO CA140-EXIT.
             MOVE 91             TO WS-S2.
             MOVE 120            TO WS-S3.
             PERFORM CA115 THRU CA130.

       CA140-EXIT.
             MOVE ZERO TO WS-S6.

       CA140.
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
      /
      *    ****   C R E A T E N E W D E B T O R
      *
       DA000        SECTION 50.
       DA00.
      MOVE 40   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W40-NAME
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W80-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      CALL "CBL_TOUPPER" USING W40-CONAC
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1        TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W40-LANG
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      CALL "CBL_TOUPPER" USING W40-FIN
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      CALL "CBL_TOUPPER" USING W40-INV-TYPE
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      CALL "CBL_TOUPPER" USING W40-OI
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      CALL "CBL_TOUPPER" USING W40-LAB
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      CALL "CBL_TOUPPER" USING W40-PRICE
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF W40-TITLE < 0 OR > 10
              MOVE 01   TO W40-TITLE.
           IF NOT (W40-LANG = "A" OR "E")
              MOVE "E"   TO W40-LANG.
            PERFORM CA110-ADDRESS.
            MOVE W60-ADDRESS  TO W60-ADDRESS1.
            MOVE 99   TO WS-S6.
             PERFORM CA110-ADDRESS.
           IF W40-CAT < 1
        MOVE 1   TO W40-CAT.
          IF W40-CAT > 80
              MOVE 01   TO W40-CAT.
          IF W40-TERMS > 03 OR < 01
              MOVE 0   TO W40-TERMS.
          IF W40-SMAN > 50 OR < 01
              MOVE ZERO  TO W40-SMAN.
          IF W40-DISC > 10 OR < 01
              MOVE ZERO  TO W40-DISC.
          IF W40-SDISC > 10 OR < 01
              MOVE ZERO  TO W40-SDISC.
          IF NOT (W40-INV-TYPE = "I" OR "E" OR "X")
              MOVE "E"   TO W40-INV-TYPE.
           IF NOT (W40-OI = "O" OR "B")
              MOVE "O"   TO W40-OI.
           IF NOT (W40-LAB = "Y" OR "N")
              MOVE "Y"   TO W40-LAB.
          IF NOT (W40-PRICE = "1" OR "2" OR "3")
              MOVE "1"   TO W40-PRICE.
          IF W80-TYPE < 1 OR > 3
              MOVE 2   TO W80-TYPE.
             MOVE W80-ACNO       TO DEB-ACNO.
            MOVE W40-NAME  TO DEB-NAME.
             MOVE W40-CONTACT    TO DEB-CONTACT.
             MOVE W60-ADDRESS1   TO DEB-ADDRESS.
             MOVE W60-ADDRESS    TO DEB-PADDR.
             MOVE W40-POST       TO DEB-POST.
             MOVE W40-PPOST      TO DEB-PPOST.
             MOVE W40-LANG       TO DEB-LANG.
             MOVE W40-TEL        TO DEB-B-TEL.
             MOVE W40-HTEL       TO DEB-H-TEL.
             MOVE W40-FAX        TO DEB-FAX.
            MOVE W40-ID  TO DEB-ID.
             MOVE W40-CAT        TO DEB-CAT.
             MOVE W40-TITLE      TO DEB-TITLE.
             MOVE W40-INV-TYPE   TO DEB-INV-TYPE.
             MOVE W80-TYPE       TO DEB-TYPE.
             MOVE W40-CRL        TO DEB-CRL.
             MOVE W40-OI         TO DEB-OI.
             MOVE W40-LAB        TO DEB-LAB.
             MOVE W40-PRICE      TO DEB-PRICE.
             MOVE W40-FIN        TO DEB-FIN.
             MOVE W40-TERMS      TO DEB-TERMS.
             MOVE W40-DISC       TO DEB-DISC.
             MOVE W40-SDISC      TO DEB-SDISC.
             MOVE W40-SMAN       TO DEB-SMAN.
      MOVE W40-CONAC  TO DEB-CONAC.
      MOVE W40-STM  TO DEB-STM.
      MOVE W40-BAL  TO DEB-BAL.
      MOVE W40-CUR  TO DEB-CUR.
      MOVE W40-30  TO DEB-30.
      MOVE W40-60  TO DEB-60.
      MOVE W40-90  TO DEB-90.
      MOVE W40-120  TO DEB-120.
      MOVE W40-DEBT  TO DEB-DEBT.
      MOVE W40-CRED  TO DEB-CRED.
      MOVE W40-PURYTD  TO DEB-PURYTD.
      MOVE W40-PURPRV  TO DEB-PURPRV.
      MOVE W40-INT  TO DEB-INT.
      MOVE W40-BDT  TO DEB-BDT.
      MOVE W40-LAST-PMT  TO DEB-LAST-PMT.
      MOVE W40-PURMTH  TO DEB-PURMTH.
      MOVE ZERO   TO DEB-JOBS.
             PERFORM WRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.
             ADD 1               TO DEB-ACCOUNTS.
             PERFORM REWRITE-CONTROL-UNLOCK THRU WRITE-CONTROL-EXIT.

       DA999.
             EXIT.

      /
       ZA000-INIT        SECTION 88.
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
             MOVE ","            TO W50-EXP.
             GO TO ZA999-EXIT.

      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
             MOVE 1              TO WS-S1.
             MOVE SPACES         TO WS-MID-LNE.
       ZA60.
             MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
           IF WS-S1 < 80
               ADD 1 TO WS-S1
               GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G2          TO WS-TCHR (18) WS-TCHR (49)
                                    WS-TCHR (60) WS-TCHR (70).
             MOVE WS-G3          TO WS-MCHR (18) WS-MCHR (49)
                                    WS-MCHR (60) WS-MCHR (70)
        WS-MCHR (1)  WS-MCHR (80).
             MOVE WS-G4          TO WS-BCHR (18) WS-BCHR (49)
                                    WS-BCHR (60) WS-BCHR (70).
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
       ZB000-ERROR        SECTION 89.

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
          IF WS-F-ERROR = 2
              MOVE W02-NETWORK  TO WS-FILE
              MOVE WS-NETKEY  TO WS-KEY
          ELSE
          IF WS-F-ERROR = 6
              MOVE W02-DEBTOR  TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
           ELSE
41850    IF WS-F-ERROR = 15
               MOVE WS-PARID     TO WS-FILE
              MOVE WS-PARKEY  TO WS-KEY.
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
