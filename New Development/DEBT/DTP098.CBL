      $SET LINKCOUNT"384" GNT"DTP098.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****     ******    ******    *
      *   **    **     **     **    **   **  **   **    **  **    **   *
      *   **    **     **     **    **  **    **  **    **  **    **   *
      *   **    **     **     *******   **    **   *******   ******    *
      *   **    **     **     **        **    **        **  **    **   *
      *   **    **     **     **         **  **   **    **  **    **   *
      *   *******      **     **          ****     ******    ******    *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *     D E B T O R   P A T C H   T R A N S A C T I O N            *
      *                                                                *
      *       Version 9.00.70 - March 2015                             *
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
       PROGRAM-ID.     DTP098.
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

       COPY "CONTROL.SL".

       COPY "DBTRAN.SL".

       COPY "DEBTOR.SL".

       COPY "DEBMEM.SL".

       COPY "PARAM.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "DBTRAN.FDE".

       COPY "DEBTOR.FDE".

       COPY "DEBMEM.FDE".

       COPY "PARAM.FDE".

       COPY "RECOVER.FD".

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
       77  WS-CHECK                PIC  X(18)    VALUE "aeWlimemnomLalismJ".
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
       77  WS-IXS2         PIC  9(04)    COMP-5.
       77  WS-IXS3         PIC  9(04)    COMP-5.
       77  WS-IXS4         PIC  9(04)    COMP-5.
       77  WS-ENQPOS    PIC 9(06)  COMP-5.
       77  WS-ENQEND       PIC  9(06)    COMP-5.
       77  WS-NETKEY       PIC  9(06)    COMP-5.
       77  WS-PARKEY       PIC  9(06)    COMP-5.
       77  WS-RECKEY       PIC  9(06)    COMP-5.
       77  WS-RECOVER      PIC  9(06)    COMP-5.
       77  WS-SHARED    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
       77  WS-PERIOD    PIC 9(02).
       77  WS-NTP          PIC  X(01).
       77  WS-KEYSTR    PIC 9(06)  COMP-5.
       77  WS-OPTION       PIC  X(01).
       77  WS-SKIP         PIC  X(01) VALUE "N".
       77  WS-TAX          PIC  X(01).
       77  WS-TYPE         PIC  X(01).
       77  WS-ERROR        PIC  9(01) VALUE ZERO.
       77  WS-EKEY         PIC  9(06)    COMP-5.
       77  WS-CHAR1        PIC  X(01).
       77  WS-CHAR2        PIC  X(01).
       77  WS-COMP         PIC  9(01).
       77  WS-BATCH        PIC  9(06) COMP-5.
       77  PRG-DEBT-LUP            PIC  X(12)    VALUE "DTP\DTPLOOK".
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.

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
           03  CRT-START           PIC  9(02).
           03  CRT-END             PIC  9(02).
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15) VALUE "DTP098".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "ACCOUNTS RECEIVABLE - PATCH TRANSATIONS".
      *
      *    Company Name
      *
           03  CRT-COMPANY         PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-WRKHD           PIC  X(11).
           03  CRT-WRKST           PIC  X(03).

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
    03  WS-PROG        PIC  X(03) VALUE "098".

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

       01  W10-SPARES.
          03  W10-DATE     PIC X(10).
          03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.

       01  W10-REF.
           03  W10-NUM         PIC  9(08).

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
       01  W30-AGE-DATES.
           03  W30-120         PIC  9(06) COMP-3.
           03  W30-90          PIC  9(06) COMP-3.
           03  W30-60          PIC  9(06) COMP-3.
           03  W30-30          PIC  9(06) COMP-3.
           03  W30-CUR         PIC  9(06) COMP-3.

       COPY "W40.DBT".

       COPY "W40.WS".

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

       01  W85-PASS.
           03  W85-SUPER       PIC  X(06)    OCCURS 9.
           03  W85-ENTRY       PIC  9(02)    COMP-3.
           03  W85-MARG        PIC S9(02)V99.

       01  W90-TRAN.
          03  W90-DATE        PIC  9(08)    COMP-3.
           03  W90-CODE        PIC  9(02).
          03  W90-RESULT      PIC  9(04)    COMP-5.
           03  W90-TRNKEY      PIC  9(04)    COMP-5.
           03  W90-TS          PIC  9(02)    COMP-5.
           03  W90-CATKEY      PIC  9(04)    COMP-5.
           03  W90-CS          PIC  9(02)    COMP-5.
          03  W90-REF        PIC  X(08).
           03  W90-ALLOCATED   PIC S9(09)V99 COMP-3.
           03  W90-VAL         PIC S9(09)V99 COMP-3.
           03  W90-DESC        PIC  X(12).
           03  W90-DISC        PIC S9(03)V99 COMP-3.
           03  W90-EXP         PIC S9(09)V99 COMP-3.
           03  W90-TAX         PIC S9(09)V99 COMP-3.
          03  W90-REC        PIC  X(08).
    03  W90-AGE        PIC  9(02).
    03  W90-TYPE        PIC  X(01).
    03  W90-REG        PIC  X(09).
    03  W90-ORD        PIC  X(12).

       01  W95-STM.
           03  W95-COMP        PIC X(40).

       01  W100-EDIT.
           03  W100-LBAL       PIC Z(06)9.99-.
           03  W100-LARR       PIC Z(06)9.99.
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

       SCREEN SECTION.

       01  S05.
    03  LINE  4 COLUMN  3 VALUE "Account No ".
    03  LINE  6 COLUMN  3 VALUE "Date       ".
    03  LINE  7 COLUMN  3 VALUE "Code       ".
    03  LINE  8 COLUMN  3 VALUE "Reference  ".
    03  LINE  9 COLUMN  3 VALUE "Type       ".
    03  LINE 10 COLUMN  3 VALUE "Other Ref  ".
    03  LINE 11 COLUMN  3 VALUE "Order No   ".
    03  LINE 12 COLUMN  3 VALUE "Value      ".
    03  LINE 13 COLUMN  3 VALUE "Taxfree    ".
    03  LINE 14 COLUMN  3 VALUE "Tax        ".
    03  LINE 15 COLUMN  3 VALUE "Discount   ".
    03  LINE 16 COLUMN  3 VALUE "Allocation ".
    03  LINE 17 COLUMN  3 VALUE "Rec/C-Note ".
    03  LINE 18 COLUMN  3 VALUE "Age Code   ".
    03  LINE  6 COLUMN 41 FOREGROUND-COLOR Black BACKGROUND-COLOR Green
     VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
    03  LINE  7 COLUMN 41 FOREGROUND-COLOR Black BACKGROUND-COLOR Green
     VALUE "³CODE: See debtor transaction³".
    03  LINE  8 COLUMN 41 FOREGROUND-COLOR Black BACKGROUND-COLOR Green
     VALUE "³      codes (PARAMETERS)    ³".
    03  LINE  9 COLUMN 41 FOREGROUND-COLOR Black BACKGROUND-COLOR Green
     VALUE "³TYPE:               ³".
    03  LINE 10 COLUMN 41 FOREGROUND-COLOR Black BACKGROUND-COLOR Green
     VALUE "³      DEBIT TRANSACTIONS    ³".
    03  LINE 11 COLUMN 41 FOREGROUND-COLOR Black BACKGROUND-COLOR Green
     VALUE "³       U = Unpaid.      ³".
    03  LINE 12 COLUMN 41 FOREGROUND-COLOR Black BACKGROUND-COLOR Green
     VALUE "³       P = Paid.      ³".
    03  LINE 13 COLUMN 41 FOREGROUND-COLOR Black BACKGROUND-COLOR Green
     VALUE "³       X = Partial payment. ³".
    03  LINE 14 COLUMN 41 FOREGROUND-COLOR Black BACKGROUND-COLOR Green
     VALUE "³      CREDIT TRANSACTIONS   ³".
    03  LINE 15 COLUMN 41 FOREGROUND-COLOR Black BACKGROUND-COLOR Green
     VALUE "³       U = Unallocated.     ³".
    03  LINE 16 COLUMN 41 FOREGROUND-COLOR Black BACKGROUND-COLOR Green
     VALUE "³       A = Allocated.       ³".
    03  LINE 17 COLUMN 41 FOREGROUND-COLOR Black BACKGROUND-COLOR Green
     VALUE "³       X = Partial allocat. ³".
    03  LINE 18 COLUMN 41 FOREGROUND-COLOR Black BACKGROUND-COLOR Green
     VALUE "³AGE CODE:                 ³".
    03  LINE 19 COLUMN 41 FOREGROUND-COLOR Black BACKGROUND-COLOR Green
     VALUE "³      0=MTD, 1=CUR, 2=30D   ³".
    03  LINE 20 COLUMN 41 FOREGROUND-COLOR Black BACKGROUND-COLOR Green
     VALUE "³      3=60D, 4=90D, 5=120D  ³".
    03  LINE 21 COLUMN 41 FOREGROUND-COLOR Black BACKGROUND-COLOR Green
     VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
       01  S06.
    03  LINE  6 COLUMN 15 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC 99/99/9999 USING W90-DATE AUTO.
    03  LINE  7 COLUMN 15 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC 99 USING W90-CODE AUTO.
    03        COLUMN 18 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(12) FROM W00-E-DESC.
    03  LINE  8 COLUMN 15 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC X(08) USING W90-REF AUTO.
    03  LINE  9 COLUMN 15 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC X(01) USING W90-TYPE AUTO.
    03  LINE 10 COLUMN 15 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC X(09) USING W90-REG AUTO.
    03  LINE 11 COLUMN 15 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC X(12) USING W90-ORD AUTO.
    03  LINE 12 COLUMN 15 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC Z(08)9.99- USING W90-VAL AUTO.
    03  LINE 13 COLUMN 15 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC Z(08)9.99- USING W90-EXP AUTO.
    03  LINE 14 COLUMN 15 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC Z(06)9.99- USING W90-TAX AUTO.
    03  LINE 15 COLUMN 15 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ9.99- USING W90-DISC AUTO.
    03  LINE 16 COLUMN 15 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta PIC Z(08)9.99-
     USING W90-ALLOCATED AUTO.
    03  LINE 17 COLUMN 15 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC X(08) USING W90-REC AUTO.
    03  LINE 18 COLUMN 15 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC 99 USING W90-AGE AUTO.

       COPY "DEBLUP.CRT".

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
          03  LINE 10 COLUMN 12 VALUE "Do you wish to continue (".
    03        COLUMN 37 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "Y".
    03        COLUMN 38 VALUE "/".
    03        COLUMN 39 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "N".
    03        COLUMN 40 VALUE ")?".

       01  S36.
          03  FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC X(06) USING W80-ACNO AUTO.

       01  S-DEB-MEM.
           03  LINE  7 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey VALUE
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E
      -    "".
       01  S-MEM.
           03  LINE  6 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G9.
           03          COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G10.
           03  LINE  7 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G3.
           03          COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey VALUE
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E
      -    "".
           03          COLUMN 78 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G3.
           03  LINE  8 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G14.
           03          COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING WS-HME-LINE.
           03          COLUMN 78 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G13.
           03  LINE  9 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 10 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 11 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 12 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 13 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 14 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 15 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 16 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 17 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 18 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 19 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 20 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 21 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G11.
           03          COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G12.
       01  SA-MEM.
           03  LINE  9 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM1 AUTO.
           03  LINE 10 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM2 AUTO.
           03  LINE 11 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM3 AUTO.
           03  LINE 12 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM4 AUTO.
           03  LINE 13 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM5 AUTO.
           03  LINE 14 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM6 AUTO.
           03  LINE 15 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM7 AUTO.
           03  LINE 16 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM8 AUTO.
           03  LINE 17 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM9 AUTO.
           03  LINE 18 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM10 AUTO.
           03  LINE 19 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM11 AUTO.
           03  LINE 20 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM12 AUTO.

      /
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS.
       AA000         SECTION.
       AA00.
           IF LS0-DBLEV < 9
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM HA000.
             CLOSE RECOVER.
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

       COPY "OPTION.CRT".

       COPY "LOCKED.REC".

       COPY "CLEAR.CRT".

       COPY "DATE.CHK".

      /
      *    ****    READ FILES ROUTINES
      *
       AC000              SECTION.

       COPY "CONTROL.RD".

       COPY "DBTRAN.RD1".

       COPY "DEBTOR.RD".

       COPY "DEBMEM.RD".

       COPY "PARAM.RD".

       COPY "SHARED.RD".

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
       AD000             SECTION.

       COPY "CONTROL.WR".

       COPY "DBTRAN.WR".

       COPY "DEBTOR.WR".

       COPY "PARAM.WR".

       COPY "DEBTOR.LUP".

       COPY "DEBTOR.TRN".

       COPY "APAC.HLP".

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
       AY000           SECTION.
       AY04.
             MOVE 04             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
             MOVE TRA-RECORD1    TO REC-DBTRAN.
             GO TO AY50.
       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
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
      *    ****    P A T C H   D E B T O R   T R A N S A C T I O N
      *
       HA000                   SECTION 57.
       HA00.
             PERFORM ERASE-SCREEN.
             DISPLAY "PATCH TRANSACTION RECORD" AT 0228
        WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             PERFORM HA50.
           IF WS-ERROR NOT = ZERO
               GO TO HA999.
       HA05.
             PERFORM ERASE-SCREEN.
             DISPLAY "PATCH TRANSACTION RECORD" AT 0228
        WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY S05.
             MOVE SPACES         TO W80-ACNO.
      DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "=Help,"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "F2" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       " to do Debtor Lookup "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "Esc" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       " to exit"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
       HA10.
             ACCEPT S36 AT 0415.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W80-ACNO
       GO TO HA999
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
             PERFORM AA50
   WHEN OTHER
       CALL X"E5"
       GO TO HA10
        END-EVALUATE
              DISPLAY S36 AT 0415
              IF W80-ACNO = SPACES
           GO TO HA10.
    IF W80-ACNO = SPACES
        GO TO HA05.
      PERFORM CLEAR-L50.
      MOVE W80-ACNO  TO DEB-ACNO.
             PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
        MOVE "Invalid Account Number"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO HA10.
             MOVE DEB-NAME       TO W40-NAME.
           IF DEB-TYPE = 1
               INSPECT W40-NAME REPLACING ALL "*" BY " "
               INSPECT W40-NAME REPLACING FIRST "!" BY " ".
             DISPLAY W40-NAME AT 0424 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO HA10.
             MOVE ZERO           TO WS-ENQEND WS-ENQPOS.
             INITIALIZE TRA-KEY.
             MOVE W80-ACNO       TO TRA-AC.
             PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
           IF WS-F-ERROR = 5
               GO TO HA30.
       HA15.
             PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
           IF (WS-F-ERROR = 5) OR
              (DEB-ACNO NOT = TRA-AC)
               MOVE WS-ENQPOS    TO WS-ENQEND
               GO TO HA35.
             ADD 1               TO WS-ENQPOS.
       HA16.
             MOVE TRA-DATE       TO W22-DTE2.
      MOVE W22-CC2  TO W22-CC3.
             MOVE W22-YY2        TO W22-YY3.
             MOVE W22-MM2        TO W22-MM3.
             MOVE W22-DD2        TO W22-DD3.
             MOVE W22-DTE3       TO W90-DATE.
             MOVE TRA-CODE       TO W90-CODE.
             MOVE TRA-REF        TO W90-REF.
             MOVE TRA-TYPE       TO W90-TYPE.
             MOVE TRA-REG        TO W90-REG.
             MOVE TRA-ORD        TO W90-ORD.
             MOVE TRA-VALUE      TO W90-VAL.
             MOVE TRA-TAXFREE    TO W90-EXP.
             MOVE TRA-TAX        TO W90-TAX.
             MOVE TRA-DISC       TO W90-DISC.
             MOVE TRA-ALLOCATED  TO W90-ALLOCATED.
             MOVE TRA-REC        TO W90-REC.
             MOVE TRA-AGE        TO W90-AGE.
      PERFORM GET-DEBTOR-TRAN-DESC.
      DISPLAY S06.
       HA20.
      MOVE "'N'ext, 'P'revious, 'E'nd or 'A'mend  [ ]"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "A" OR "E" OR "N" OR "P")
               GO TO HA20.
    IF WS-OPTION = "E"
        GO TO HA05.
           IF WS-OPTION = "N"
               IF WS-ENQPOS NOT = WS-ENQEND
                   GO TO HA15
               ELSE
                   GO TO HA20.
           IF WS-OPTION = "P"
               IF WS-ENQPOS = 1
                   GO TO HA20
               ELSE
                   PERFORM READ-DBTRAN-PREV THRU READ-DBTRAN-EXIT
                   SUBTRACT 1    FROM WS-ENQPOS
                   GO TO HA16.
             PERFORM AY60 THRU AY999.
      MOVE 0   TO WS-ACTION.
             PERFORM AY04 THRU AY59.

       HA25.
             ACCEPT S06.
      PERFORM GET-DEBTOR-TRAN-DESC.
      DISPLAY S06.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO HA25.
             MOVE W90-DATE       TO W22-DTE3.
      MOVE W22-CC3  TO W22-CC2.
             MOVE W22-YY3        TO W22-YY2.
             MOVE W22-MM3        TO W22-MM2.
             MOVE W22-DD3        TO W22-DD2.
             MOVE W22-DTE2       TO TRA-DATE.
             MOVE W90-CODE       TO TRA-CODE.
             MOVE W90-REF        TO TRA-REF.
             MOVE W90-TYPE       TO TRA-TYPE.
             MOVE W90-REG        TO TRA-REG.
             MOVE W90-ORD        TO TRA-ORD.
             MOVE W90-VAL        TO TRA-VALUE.
             MOVE W90-EXP        TO TRA-TAXFREE.
             MOVE W90-TAX        TO TRA-TAX.
             MOVE W90-DISC       TO TRA-DISC.
             MOVE W90-ALLOCATED  TO TRA-ALLOCATED.
             MOVE W90-REC        TO TRA-REC.
             MOVE W90-AGE        TO TRA-AGE.
             PERFORM REWRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
             PERFORM AY70 THRU AY999.
           IF WS-ENQPOS NOT = WS-ENQEND
               GO TO HA15.
             GO TO HA16.
       HA30.
      MOVE "No transactions on file"
     TO WS-ERR-MES.
            PERFORM ERROR-MESSAGE.
             GO TO HA05.
       HA35.
      MOVE "End of transaction list"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
             GO TO HA05.
       HA50.
             MOVE 5              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-PW         TO W85-PASS.
            DISPLAY "Enter password >      <" AT 5051
              WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT W40-CSHPASS AT 5067
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT
          AUTO NO-ECHO.
             MOVE 6 TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W40-CSHPASS
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
            MOVE 1 TO WS-LENGTH.
           IF NOT (W40-CSHPASS = W85-SUPER (1) OR
                                 W85-SUPER (9) OR
                                 "PATCHT")
               MOVE 1            TO WS-ERROR
           ELSE
               MOVE ZERO         TO WS-ERROR.
            DISPLAY "                       " AT 5051
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.
       HA999.
             EXIT.
      /
      *    ****    I N I T I A L I Z E   P R O G R A M
      *
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

            OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
             MOVE 1              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY    TO W95-COMP.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
      *
      *    ****    S E T   U P   A G E   D A T E S
      *
             MOVE W12-T-YMD      TO W22-DTE2.
    IF W22-DD2 > 27
                ADD 1            TO W22-MM2.
       ZA47.
           IF W22-MM2 > 01
               SUBTRACT 1        FROM W22-MM2
           ELSE
               MOVE 12           TO W22-MM2
               SUBTRACT 1        FROM W22-YY2.
           IF W22-MM2 = 02
               MOVE 28           TO W22-DD2
           ELSE
           IF W22-MM2 = 04 OR 06 OR 09 OR 11
               MOVE 30           TO W22-DD2
           ELSE
               MOVE 31           TO W22-DD2.
       ZA48.
           IF W12-MM = W22-MM2
               IF W12-DD < W22-DD2
                   MOVE W12-DD   TO W22-DD2.
             MOVE W22-DTE2       TO W30-CUR.
      PERFORM ZA47.
             MOVE W22-DTE2       TO W30-30.
      PERFORM ZA47.
             MOVE W22-DTE2       TO W30-60.
      PERFORM ZA47.
             MOVE W22-DTE2       TO W30-90.
      PERFORM ZA47.
             MOVE W22-DTE2       TO W30-120.
            GO TO ZA999-EXIT.
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
       ZA999-EXIT.
             EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *
       ZB000-ERROR      SECTION 8.

       OPEN-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             GO TO DISPLAY-FILE-NAME.

       READ-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             GO TO DISPLAY-FILE-NAME.

       WRITE-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             GO TO DISPLAY-FILE-NAME.
       I-O-ERROR-EXIT.
             EXIT.

       DISPLAY-FILE-NAME.
           IF WS-F-ERROR = 2
               MOVE W02-NETWORK TO WS-FILE
               MOVE WS-NETKEY TO WS-KEY
           ELSE
           IF WS-F-ERROR = 5
               MOVE W02-DBTRAN TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE TRA-KEY      TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 6
               MOVE W02-DEBTOR TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
          ELSE
           IF WS-F-ERROR = 15
               MOVE WS-PARID TO WS-FILE
               MOVE WS-PARKEY TO WS-KEY
           ELSE
           IF WS-F-ERROR = 18
               MOVE W02-RECOVER TO WS-FILE
              MOVE WS-RECKEY TO WS-KEY
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
