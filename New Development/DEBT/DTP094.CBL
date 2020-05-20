      $SET LINKCOUNT"384" GNT"DTP094.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****     ******       **     *
      *   **    **     **     **    **   **  **   **    **     ***     *
      *   **    **     **     **    **  **    **  **    **    * **     *
      *   **    **     **     *******   **    **   *******   *  **     *
      *   **    **     **     **        **    **        **  *******    *
      *   **    **     **     **         **  **   **    **      **     *
      *   *******      **     **          ****     ******      ****    *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *       D E B T O R   B A L A N C I N G   U T I L I T Y          *
      *                                                                *
      *       Version 9.00.70 - March 2015                             *
      *                                                                *
      * This program is still under development and is scheduled to be *
      * completed during 2015 (This is a nice to have, but not a must  *
      * have or critical requirement). The G/L balancing program has   *
      * been copied and the necessary amendments are being made to     *
      * adapt it for Debtors/Accounts receivable.                      *
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
       PROGRAM-ID.     DTP094.
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

          SELECT DTBTCH  ASSIGN DISK
           LOCK MANUAL
                          STATUS WS-STATUS
                          ACCESS DYNAMIC
                          ORGANIZATION INDEXED
           RECORD BCH-KEY
     ALTERNATE RECORD BCH-VALUE DUPLICATES.

       COPY "PARAM.SL".

          SELECT PRNREP  ASSIGN W02-PRINTER
                          ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "DBTRAN.FDE".

       COPY "DEBTOR.FDE".

       COPY "DEBMEM.FDE".

      /
       FD  DTBTCH    LABEL RECORD STANDARD
             VALUE OF FILE-ID W02-DTBTCH.
       01  BCH-RECORD1.
    03  BCH-KEY.
              05  BCH-AC      PIC  X(06).
              05  BCH-DATE    PIC  9(06)    COMP.
              05  BCH-CODE    PIC  9(02).
              05  BCH-REF     PIC  X(08).
          03  BCH-VALUE       PIC S9(09)V99 COMP.

       COPY "PARAM.FDE".

       FD  PRNREP    LABEL RECORD OMITTED
             LINAGE IS WS-PGE-LENGTH.
       01  R-L1.
           03  R-DET       PIC X(132).
       01  R-L2.
           03  R-H1        PIC X(06).
           03  R-DTE1      PIC Z9/99/99.
           03  FILLER      PIC X(10).
           03  R-H2        PIC X(40).
           03  R-H2A       PIC X(27).
           03  FILLER      PIC X(10).
           03  R-H3        PIC X(07).
           03  R-BTCH      PIC Z(04)9.
           03  FILLER      PIC X(10).
           03  R-H4        PIC X(06).
           03  R-PGE       PIC ZZ9.
       01  R-L3.
          03  LED-H1        PIC  X(07).
           03  FILLER          PIC  X(02).
           03  LED-H2          PIC  X(12).
           03  FILLER          PIC  X(06).
           03  LED-H2A         PIC  X(10).
           03  FILLER          PIC  X(02).
           03  LED-H3          PIC  X(04).
           03  FILLER          PIC  X(04).
           03  LED-H4          PIC  X(04).
           03  FILLER          PIC  X(03).
           03  LED-H4A         PIC  X(05).
           03  FILLER          PIC  X(04).
           03  LED-H5          PIC  X(05).
           03  FILLER          PIC  X(02).
           03  LED-H6          PIC  X(15).
           03  FILLER          PIC  X(28).
           03  LED-H7          PIC  X(05).
           03  FILLER          PIC  X(06).
           03  LED-H8          PIC  X(07).
           03  FILLER          PIC  X(01).
       01  R-L4.
           03  R-ACC       PIC 9(04)/99.
          03  FILLER    PIC X(02).
           03  R-ANME      PIC X(24).
           03  FILLER      PIC X(04).
           03  R-DTE2      PIC Z9/99/99.
          03  FILLER    PIC X(02).
          03  R-CDE    PIC X(04).
    03  FILLER    PIC X(02).
    03  R-BATCH    PIC Z(06).
          03  FILLER    PIC X(02).
          03  R-REF    PIC Z(07).
          03  FILLER    PIC X(02).
           03  R-NAR       PIC X(36).
           03  FILLER      PIC X(01).
           03  R-DBT       PIC Z(08)9.99.
    03  FILLER    PIC X(01).
          03  R-CRD    PIC Z(07)9.99-.
       01  R-L5.
           03  RI-H1       PIC X(06).
           03  RI-H2       PIC X(40).
           03  RI-H3       PIC X(39).
           03  RI-H4       PIC X(42).
           03  RI-H5       PIC X(05).
       01  R-L6.
           03  RI-NO       PIC Z(04)9.
           03  FILLER      PIC X(01).
           03  RI-ACDT     PIC 9999/99.
           03  FILLER      PIC X(01).
           03  RI-DTNAME   PIC X(30).
           03  FILLER      PIC X(02).
           03  RI-ACCR     PIC 9999/99.
           03  FILLER      PIC X(01).
           03  RI-CRNAME   PIC X(30).
           03  FILLER      PIC X(01).
           03  RI-NAR      PIC X(36).
           03  RI-VAL      PIC Z(07)9.99.

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
       77  WS-DBTKEY       PIC  9(06)    COMP-5.
       77  WS-DEBKEY       PIC  9(06)    COMP-5.
       77  WS-DEPKEY       PIC  9(06)    COMP-5.
       77  WS-ENQPOS       PIC  9(06)    COMP-5.
       77  WS-ENQEND       PIC  9(06)    COMP-5.
       77  WS-NETKEY       PIC  9(06)    COMP-5.
       77  WS-PARKEY       PIC  9(06)    COMP-5.
       77  WS-RECKEY       PIC  9(06)    COMP-5.
       77  WS-RECOVER      PIC  9(06)    COMP-5.
       77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
       77  WS-GST-RATE     PIC S9(03)V99 COMP-3.
       77  WS-FIN-RATE     PIC S9(03)V99 COMP-3.
       77  WS-PERIOD       PIC  9(02).
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
       77  WS-ERR1         PIC  X(22) VALUE "Invalid Account Number".
       77  WS-ERR2         PIC  X(09) VALUE "No Record".
       77  WS-A            PIC  X(19) VALUE "Any key to continue".
       77  WS-E            PIC  X(15) VALUE "**END** - ENTER".
       77  WS-AC           PIC  X(07) VALUE "ACCOUNT".
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "DTP094".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "ACCOUNTS RECEIVABLE - BALANCING UTILITY".
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
    03  WS-PROG        PIC  X(03) VALUE "094".

       01  WS-PARID.
           03  WS-SYS-ID       PIC  X(03).

           03  W02-DTBTCH.
               05  FILLER      PIC  X(05) VALUE "DBAL_".
               05  W02-EXT5B   PIC  X(03).
               05  FILLER      PIC  X(01) VALUE ".".
               05  W02-USE5B   PIC  X(03) VALUE "APC".

       01  W02-FILE-IDS.
           03  W02-PRINTER.
        05  W02-REPORT PIC X(07) VALUE "DTP094.".
               05  W02-USER   PIC X(05) VALUE SPACES.

       01  W10-SPARES.
          03  W10-DATE     PIC X(10).
          03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.

       01  W10-REF.
           03  W10-NUM         PIC  9(08).

       01  W12-DATE.
          03  W12-TODAY      PIC 9(08).
           03  W12-DATE-DMY REDEFINES W12-TODAY.
               05  W12-DAY    PIC 9(02).
               05  W12-MONTH  PIC 9(02).
        05  W12-CENT   PIC 9(02).
               05  W12-YEAR   PIC 9(02).
          03  W12-T-YMD      PIC 9(08).
           03  W12-DATE-YMD REDEFINES W12-T-YMD.
              05  W12-CC     PIC 9(02).
               05  W12-YY     PIC 9(02).
               05  W12-MM     PIC 9(02).
               05  W12-DD     PIC 9(02).

       01  W20-DATE.
    03  W20-CCYY.
        05  W20-CC.
     07  W20-CENT PIC  9(02).
              05  W20-YY.
           07  W20-YEAR PIC  9(02).
    03  W20-CNYR REDEFINES W20-CCYY
          PIC  9(04).
           03  W20-MM.
              05  W20-MONTH   PIC  9(02).
           03  W20-DD.
              05  W20-DAY     PIC  9(02).
       01  W20-DTE1 REDEFINES W20-DATE.
          03  W20-DTE        PIC  9(08).

       01  W22-DATES.
          03  W22-DTE1        PIC  9(08).
          03  W22-DTE2        PIC  9(08).
           03  W22-DATE2 REDEFINES W22-DTE2.
        05  W22-CC2     PIC  9(02).
              05  W22-YY2     PIC  9(02).
              05  W22-MM2     PIC  9(02).
              05  W22-DD2     PIC  9(02).
          03  W22-DATE3 REDEFINES W22-DTE2.
        05  W22-CY2     PIC  9(04).
        05  FILLER      PIC  9(04).
          03  W22-DTE3        PIC  9(08).
           03  W22-DATE3 REDEFINES W22-DTE3.
              05  W22-DD3     PIC  9(02).
              05  W22-MM3     PIC  9(02).
        05  W22-CC3     PIC  9(02).
              05  W22-YY3     PIC  9(02).

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
    03  W30-120        PIC  9(08) COMP-3.
    03  W30-90        PIC  9(08) COMP-3.
    03  W30-60        PIC  9(08) COMP-3.
    03  W30-30        PIC  9(08) COMP-3.
    03  W30-CUR        PIC  9(08) COMP-3.

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

       01  W90-VALUES.
          03  W90-DEBIT       PIC S9(09)V99 COMP-3.
          03  W90-CREDIT      PIC S9(09)V99 COMP-3.

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

       01  S11.
           03  LINE  8 COLUMN 12 VALUE "Balance an ".
           03          COLUMN 23 VALUE "A"             FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 24 VALUE "ccount, ".
           03          COLUMN 32 VALUE "T"             FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 33 VALUE "ran. code or ".
           03          COLUMN 46 VALUE "D"             FOREGROUND-COLOR Brown HIGHLIGHT.   
           03          COLUMN 47 VALUE "ate > < ".
           03          COLUMN 55 VALUE "Esc"           FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 58 VALUE "ape to Exit".

       01  S12.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC X(06) USING W80-ACNO AUTO.

       01  S13.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC 9(02) USING W40-CODE AUTO.

       01  S14.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC 9(06) USING W40-DATE AUTO.

       COPY "DEBLUP.CRT".

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
       AA000        SECTION.
       AA00.
           IF LS0-DBLEV < 9
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000.
             PERFORM DA000.
             CLOSE DTBTCH PRNREP.

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

       COPY "LOCKED.REC".

       COPY "CLEAR.CRT".

       COPY "DATE.CHK".

      /
      *    ****    READ FILES ROUTINES
      *
       AC000              SECTION.

       COPY "CONTROL.RD".

       COPY "DBTRAN.RD1".

      *
      *    ****  ***** ****  *****  ***  *   *   ***** *** *   *****
      *    *   *   *   *   *   *   *   * *   *   *  *  *   *
      *    *   *   *   *   *   *   *  *   *   *  *  *   *
      *    *   *   *   ****    *   *  *****   ***  *  *   ***
      *    *   *   *   *   *   *   *  *   *   *  *  *   *
      *    *   *   *   *   *   *   *   * *   *   *  *  *   *
      *    ****    *   ****    *    ***  *   *   * *** ***** *****
      *
       READ-DTBTCH.
            READ DTBTCH
        KEY IS BCH-KEY.
           IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
              GO TO READ-DTBTCH-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
              MOVE 55   TO WS-F-ERROR
              GO TO READ-DTBTCH-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 55   TO WS-F-ERROR
               PERFORM READ-ERROR.
           IF RECORD-LOCKED
        MOVE W02-DTBTCH  TO WS-FILE
               PERFORM LOCKED-RECORD
              GO TO READ-DTBTCH.
            MOVE 55   TO WS-F-ERROR.
             PERFORM READ-ERROR.
      GO TO READ-DTBTCH-EXIT.

       READ-DTBTCH-NEXT.
            READ DTBTCH NEXT.
           IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
              GO TO READ-DTBTCH-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
              MOVE 55   TO WS-F-ERROR
              GO TO READ-DTBTCH-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 55   TO WS-F-ERROR
               PERFORM READ-ERROR.
           IF RECORD-LOCKED
        MOVE W02-DTBTCH  TO WS-FILE
               PERFORM LOCKED-RECORD
              GO TO READ-DTBTCH-NEXT.
            MOVE 55   TO WS-F-ERROR.
             PERFORM READ-ERROR.
      GO TO READ-DTBTCH-EXIT.

       READ-DTBTCH-PREV.
            READ DTBTCH PREVIOUS.
           IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
              GO TO READ-DTBTCH-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
              MOVE 55   TO WS-F-ERROR
              GO TO READ-DTBTCH-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 55   TO WS-F-ERROR
               PERFORM READ-ERROR.
           IF RECORD-LOCKED
        MOVE W02-DTBTCH  TO WS-FILE
               PERFORM LOCKED-RECORD
              GO TO READ-DTBTCH-PREV.
            MOVE 55   TO WS-F-ERROR.
             PERFORM READ-ERROR.
      GO TO READ-DTBTCH-EXIT.

       START-AT-DTBTCH-KEY.
            START DTBTCH
        KEY >= BCH-KEY.
           IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
              GO TO READ-DTBTCH-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
              MOVE 55   TO WS-F-ERROR
              GO TO READ-DTBTCH-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 55   TO WS-F-ERROR
               PERFORM READ-ERROR.
      GO TO READ-DTBTCH-EXIT.

       START-AT-DTBTCH-VAL.
            START DTBTCH
        KEY >= BCH-VAL.
           IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
              GO TO READ-DTBTCH-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
              MOVE 55   TO WS-F-ERROR
              GO TO READ-DTBTCH-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
              MOVE 55   TO WS-F-ERROR
               PERFORM READ-ERROR.

       READ-DTBTCH-EXIT.
             EXIT.

       COPY "DEBTOR.RD".

       COPY "DEBMEM.RD".

       COPY "PARAM.RD".

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
       AD000             SECTION.

       WRITE-DTBTCH.
            WRITE BCH-RECORD1.
           IF WS-STAT1 NOT = "0"
              MOVE 55   TO WS-F-ERROR
               PERFORM WRITE-ERROR.

       WRITE-DTBTCH-EXIT.
             EXIT.

       COPY "APAC.HLP".

      /
       DA000      SECTION 5.
       DA00.
             MOVE "S"                TO CRT-TYPE.
             CALL "UTP\CRTHEAD" USING CRT-HEADINGS LS-USER-ID.
             DISPLAY S11.
             MOVE "T"              TO WS-OPTION.

       DA02.
      ACCEPT WS-OPTION AT 0852
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO DA999
   WHEN OTHER
       CALL X"E5"
       GO TO DA02
        END-EVALUATE.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      EVALUATE WS-OPTION
        WHEN "A"

        WHEN "D"

        WHEN "T"

        WHEN OTHER
     CALL X"E5" CALL X"E5" CALL X"E5"
     GO TO DA02
      END-EVALUATE.





    IF WS-OPTION = "B"
        GO TO DA15.

       DA03.
      DISPLAY "Account Number" AT 0612.
            MOVE SPACES  TO W80-ACNO.

       DA05.
      DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "=Help,"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "F2" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       " to do Account Lookup - "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "Esc" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "ape to exit"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
            ACCEPT S17 AT 0628.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W80-ACNO
       GO TO DA999
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA50
   WHEN OTHER
       CALL X"E5"
       GO TO DA05
        END-EVALUATE
        DISPLAY S17 AT 0628
        IF W80-ACNO = SPACES
           GO TO DA05.
      PERFORM CLEAR-L50.
          IF W80-ACNO = SPACES
              GO TO DA999.
      MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W80-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
             MOVE W80-ACNO       TO DEB-ACNO.
             PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
            PERFORM CLEAR-L50.
          IF WS-F-ERROR NOT = ZERO
              MOVE "No Record"  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO DA05.
      MOVE DEB-NAME  TO W4O-NAME.
    IF DEB-TYPE = 1
              INSPECT W40-NAME REPLACING ALL "*" BY " "
               INSPECT W40-NAME REPLACING FIRST "!" BY " ".
            DISPLAY W40-NAME AT 0636
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
              GO TO DA03.
      INITIALIZE TRA-RECORD1.
      MOVE W80-ACNO  TO TRA-AC.
      PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
    IF WS-F-ERROR = 5
        MOVE "No transactions for this account"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO DA00.
      MOVE ZERO   TO W90-DEBIT W90-CREDIT.
      DISPLAY "Reading :" AT 0812.

       DA10.
            PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
      COMMIT.
           IF (WS-F-ERROR = 11) OR
              (T-AC NOT = G-AC)
        MOVE "A"   TO WS-BAL-TYPE
        GO TO DA35.
      DISPLAY T-KEY AT 0822
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      *
      *    ****    I G N O R E  S E C O N D   N A R R A T I V E
      *
    IF T-SUB = 5
        GO TO DA10.
      MOVE T-KEY   TO B-KEY.
            MOVE T-NAR   TO B-NAR.
            MOVE T-VAL   TO B-VAL.
            MOVE T-FLAG  TO B-FLAG.
      MOVE T-BATCH  TO B-BATCH.
      PERFORM WRITE-GLBTCH THRU WRITE-GLBTCH-EXIT.
    IF T-VAL < ZERO
        ADD T-VAL  TO W20-CREDIT
    ELSE
        ADD T-VAL  TO W20-DEBIT.
      GO TO DA10.

       DA15.
      DISPLAY "Batch Number" AT 0612.
            MOVE ZERO   TO W15-ACCOUNT.
      MOVE "B"   TO WS-BAL-TYPE.
       DA20.
            ACCEPT S17 AT 0626.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO DA999
   WHEN OTHER
       CALL X"E5"
       GO TO DA20
        END-EVALUATE.
    IF W15-ACCOUNT = ZERO
        GO TO DA999.
      MOVE W15-ACCOUNT  TO WS-BATCH.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
              GO TO DA20.
      INITIALIZE T-REC.
      PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
           IF WS-F-ERROR = 11
        MOVE "No transactions on file" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO DA00.
      MOVE ZERO   TO W20-DEBIT W20-CREDIT.
      DISPLAY "Reading :" AT 0812.

       DA25.
            PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
      COMMIT.
    IF WS-F-ERROR = 11
        GO TO DA30.
      DISPLAY T-KEY AT 0822
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
    IF T-BATCH NOT = WS-BATCH
        GO TO DA25.
      *
      *    ****    I G N O R E  S E C O N D   N A R R A T I V E
      *
    IF T-SUB = 5
        GO TO DA25.
      MOVE T-KEY   TO B-KEY.
            MOVE T-NAR   TO B-NAR.
            MOVE T-VAL   TO B-VAL.
            MOVE T-FLAG  TO B-FLAG.
      MOVE T-BATCH  TO B-BATCH.
      PERFORM WRITE-GLBTCH THRU WRITE-GLBTCH-EXIT.
    IF T-VAL < ZERO
        ADD T-VAL  TO W20-CREDIT
    ELSE
        ADD T-VAL  TO W20-DEBIT.
      GO TO DA25.

       DA30.
    IF (W20-DEBIT + W20-CREDIT) = ZERO
        DISPLAY "Batch is in balance - " AT 5002
   WITH FOREGROUND-COLOR Blue
        BACKGROUND-COLOR Cyan
         "P" WITH BACKGROUND-COLOR Cyan
    FOREGROUND-COLOR Brown HIGHLIGHT
         "rint, " WITH BACKGROUND-COLOR Cyan
         FOREGROUND-COLOR Blue
         "N" WITH BACKGROUND-COLOR Cyan
    FOREGROUND-COLOR Brown HIGHLIGHT
         "ext or " WITH BACKGROUND-COLOR Cyan
          FOREGROUND-COLOR Blue
         "E" WITH BACKGROUND-COLOR Cyan
    FOREGROUND-COLOR Brown HIGHLIGHT
         "xit " WITH BACKGROUND-COLOR Cyan
       FOREGROUND-COLOR Blue
        CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS
        IF USER-FUNC
     EVALUATE KEY-CODE-1
       WHEN ESC-KEY
     GO TO DA999
       WHEN OTHER  CALL X"E5"
     END-EVALUATE
     GO TO DA30
        ELSE
        IF DATA-8BIT
     MOVE KEY-CODE-1X  TO WS-OPTION
     IF NOT (WS-OPTION = "E" OR "N" OR "P")
         CALL X"E5"
         GO TO DA30
     ELSE
     PERFORM CLEAR-L50
     IF WS-OPTION = "E"
         GO TO DA999
     ELSE
     IF WS-OPTION = "N"
         GO TO DA15
     ELSE
         GO TO DA35
        ELSE
     CALL X"E5"
     GO TO DA30.

       DA35.
            MOVE ZERO   TO WS-PAGE.
      MOVE ZERO   TO W15-ACCOUNT.
    IF WS-BAL-TYPE = "B"
              MOVE "G/L BATCH LISTING"
     TO W10-HEAD
    ELSE
        MOVE "G/L ACCOUNT TRANSACTIONS"
     TO W10-HEAD.
             MOVE WS-COND        TO R-DET.
             WRITE R-L1 BEFORE 0 LINES.
      MOVE "V"   TO WS-OPTION.
      DISPLAY "D" AT 1012 WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "ate or " "V" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "alue sequence".

       DA38.
      ACCEPT WS-OPTION AT 1036
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "D" OR "V")
        GO TO DA38.

       DA40.
            MOVE SPACES  TO R-DET.
             MOVE WS-H1          TO R-H1.
             MOVE W90-COMP       TO R-H2.
             MOVE W10-H2         TO R-H2A.
             MOVE WS-H4          TO R-H4.
             ADD 1               TO WS-PAGE.
             MOVE WS-PAGE        TO R-PGE.
    IF WS-BAL-TYPE = "B"
              MOVE WS-H3  TO R-H3
              MOVE WS-BATCH  TO R-BTCH.
             MOVE W05-TODAY      TO R-DTE1.
             WRITE R-L1 BEFORE 2 LINES.
             MOVE SPACES         TO R-DET.
            MOVE "ACNT NO"  TO LED-H1.
             MOVE "ACCOUNT NAME" TO LED-H2.
             MOVE "DATE"         TO LED-H3.
             MOVE "CODE"         TO LED-H4.
             MOVE "BATCH"        TO LED-H4A.
             MOVE "REFER"        TO LED-H5.
             MOVE "PAYEE/NARRATIVE"
                                 TO LED-H6.
            MOVE "DEBIT"  TO LED-H7.
            MOVE " CREDIT"  TO LED-H8.
            WRITE R-L1 BEFORE 2 LINES.
             MOVE SPACES         TO R-DET.
    IF WS-BAL-TYPE = "A"
        MOVE G-AC  TO R-ACC
              MOVE G-NAME  TO R-ANME W15-ACCOUNT.

       DA45.
      INITIALIZE B-REC.
    IF WS-OPTION = "V"
        MOVE ZERO  TO B-VAL
        PERFORM START-AT-GLBTCH-VAL THRU READ-GLBTCH-EXIT
    ELSE
        PERFORM START-AT-GLBTCH-KEY THRU READ-GLBTCH-EXIT.
    IF WS-F-ERROR = 61
        GO TO DA55.

       DA50.
      PERFORM READ-GLBTCH-NEXT THRU READ-GLBTCH-EXIT.
    IF WS-F-ERROR = 61
        GO TO DA55.
    IF WS-BAL-TYPE = "B"
        IF B-AC NOT = W15-ACCOUNT
     MOVE B-AC  TO W15-ACCOUNT
     PERFORM AD000
     MOVE G-AC  TO R-ACC
     MOVE G-NAME  TO R-ANME.
            MOVE B-DATE  TO W15-YMD
      MOVE W15-YR  TO W15-YY.
             MOVE W15-MT         TO W15-MM.
             MOVE W15-DY         TO W15-DD.
            MOVE W15-DATE  TO R-DTE2.
          IF B-CODE = 01 OR 02
              MOVE "JNL"  TO R-CDE
           ELSE
          IF B-CODE = 03 OR 04
              MOVE "CHEQ"  TO R-CDE
           ELSE
          IF B-CODE = 06 OR 07
              MOVE "DEP"  TO R-CDE.
    IF WS-BAL-TYPE = "A"
        MOVE B-BATCH  TO R-BATCH.
            MOVE B-NAR   TO R-NAR.
            MOVE B-REFER  TO R-REF.
          IF B-VAL < ZERO
              MOVE B-VAL  TO R-CRD
           ELSE
              MOVE B-VAL  TO R-DBT.
          IF LINAGE-COUNTER < WS-PRN-LENGTH
               WRITE R-L1 BEFORE 1 LINES
           ELSE
               WRITE R-L1 BEFORE PAGE
              PERFORM DA40
        IF WS-BAL-TYPE = "B"
     MOVE ZERO  TO W15-ACCOUNT.
            MOVE SPACES TO R-DET.
      GO TO DA50.

       DA55.
            WRITE R-L1 BEFORE 1 LINES
      MOVE "****  E N D   R E P O R T  ****"
     TO R-NAR.
      MOVE W20-DEBIT  TO R-DBT.
      MOVE W20-CREDIT  TO R-CRD.
            WRITE R-L1 BEFORE PAGE.
      MOVE SPACES  TO R-DET.
      CLOSE GLBTCH.
      OPEN OUTPUT GLBTCH.
      CLOSE GLBTCH.
      OPEN I-O GLBTCH.
      GO TO DA00.

       DA999.
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

            MOVE WS-SYS-ID  TO W02-PTH5B.
            MOVE LS-USER  TO W02-USE5B.
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
    IF TRM-STDP (WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
        MOVE ZERO  TO WS-PRN-NO
           ELSE
           IF TRM-STDP (WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
        MOVE 1   TO WS-PRN-NO
           ELSE
           IF TRM-STDP (WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
        MOVE 2   TO WS-PRN-NO
    ELSE
           IF TRM-STDP (WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 9
        MOVE LS-USER  TO W02-USER.
             ADD 501 TRM-PRN3 (WS-S6)
                                 GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S6.
             ADD 1               TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE TRM-CONP (WS-S6)
                                 TO WS-COND.
             MOVE TRM-NORP (WS-S6)
                                 TO WS-NORM.
             MOVE TRM-EXPP (WS-S6)
                                 TO WS-EXPP.
             MOVE TRM-ECAN (WS-S6)
                                 TO WS-ECAN.
             MOVE TRM-8LPI (WS-S6)
                                 TO WS-8LPI.
             MOVE TRM-6LPI (WS-S6)
                                 TO WS-6LPI.
             MOVE TRM-10CPI (WS-S6)
                                 TO WS-10CPI.
             MOVE TRM-12CPI (WS-S6)
                                 TO WS-12CPI.
             MOVE TRM-17CPI (WS-S6)
                                 TO WS-17CPI.
      MOVE TRM-LENGTH (WS-S6)
     TO WS-PRN-LENGTH.
      MOVE TRM-PAGE (WS-S6)
     TO WS-PGE-LENGTH.
       ZA02.
      OPEN OUTPUT DTBTCH.
          IF RUNTIME-ERROR
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS NOT = "00"
              MOVE 55   TO WS-F-ERROR
               PERFORM OPEN-ERROR.
      CLOSE DTBTCH.
      OPEN I-O DTBTCH.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY    TO W95-COMP.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
    IF WS-PRN-NO < 3
        PERFORM ZA210 THRU ZA215
        IF WS-ERROR = 1
     GO TO ZA205
    ELSE
        DISPLAY "Waiting for printer response" AT 1412
   WITH FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
            OPEN OUTPUT PRNREP.
            GO TO ZA999-EXIT.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             GO TO ZA205.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Setup the Screen Graphics Characters: ³ Ä Ú ¿ À Ù Â Á etc ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
            DISPLAY "Files locked - Try later" AT 5002
              WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       " " WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       WS-STATUS
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT
             " Press any key"
              WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT WS-OPTION AT 5045
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
       ZA205.
             EXIT PROGRAM.
       ZA210.
      MOVE ZERO   TO WS-ERROR
        WS-S1 WS-S2 WS-S3 WS-S4
        WS-S5 WS-S6 WS-S7 WS-S8.
      CALL "PC_TEST_PRINTER" USING WS-PRN-NO
       WS-PRN-STAT
        RETURNING WS-STATUS.
    IF WS-PRN-STAT > 127
        MOVE 1   TO WS-S1
        SUBTRACT 128  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 63
        MOVE 1   TO WS-S2
        SUBTRACT 64  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 31
        MOVE 1   TO WS-S3
        SUBTRACT 32  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 15
        MOVE 1   TO WS-S4
        SUBTRACT 16  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 7
        MOVE 1   TO WS-S5
        SUBTRACT 8  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 3
        MOVE 1   TO WS-S6
        SUBTRACT 4  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 1
        MOVE 1   TO WS-S7
        SUBTRACT 2  FROM WS-PRN-STAT.
      MOVE WS-PRN-STAT  TO WS-S8.
    IF WS-S3 = 0 AND
       WS-S5 = 0 AND
       WS-S8 = 0
        GO TO ZA215.
      DISPLAY "Check printer - " AT 2304
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "R" WITH FOREGROUND-COLOR Grey HIGHLIGHT
       "etry - "
       "A" WITH FOREGROUND-COLOR Grey HIGHLIGHT
       "bort".
       ZA212.
      ACCEPT WS-OPTION AT 2334
      WITH FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      IF NOT (WS-OPTION = "A" OR "R")
   GO TO ZA212.
      IF WS-OPTION = "A"
   MOVE 1   TO WS-ERROR
   GO TO ZA215.
        GO TO ZA210.
       ZA215.
      EXIT.
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
           IF WS-F-ERROR = 12
               MOVE W02-INVOIC TO WS-FILE
               MOVE ZERO      TO WS-KEY
               MOVE DOC-KEY      TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 15
               MOVE WS-PARID TO WS-FILE
               MOVE WS-PARKEY TO WS-KEY
           ELSE
           IF WS-F-ERROR = 18
               MOVE W02-RECOVER TO WS-FILE
               MOVE WS-RECKEY TO WS-KEY
           ELSE
           IF WS-F-ERROR = 27
               MOVE W02-DEBMEM   TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE DME-KEY  TO WS-KEYX.
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
