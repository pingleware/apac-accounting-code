      $SET LINKCOUNT"384" GNT"DTPCOL.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ******    ******   **        *
      *   **    **     **     **    **   **    **  **    **  **        *
      *   **    **     **     **    **  **         **    **  **        *
      *   **    **     **     *******   **         **    **  **        *
      *   **    **     **     **        **         **    **  **        *
      *   **    **     **     **         **    **  **    **  **        *
      *   *******      **     **          ******    ******   ********  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R ' S   C O L L E C T I O N S   P R O G R A M    *
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
       PROGRAM-ID.     DTPCOL.
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

       COPY "DEBCOL.SL".

       COPY "DEBTOR.SL".

       COPY "DEBMEM.SL".

       COPY "PARAM.SL".

           SELECT PRNREP  ASSIGN W02-PRINTER
                          ORGANIZATION LINE SEQUENTIAL.

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

       COPY "CONTROL.FDE".

       COPY "DBTRAN.FDE".

       COPY "DEBCOL.FDE".

       COPY "DEBTOR.FDE".

       COPY "DEBMEM.FDE".

       COPY "PARAM.FDE".

      /
       FD  PRNREP    LABEL RECORD OMITTED
             LINAGE IS WS-PGE-LENGTH.
       01  R-L1.
           03  R-DET1          PIC  X(132).

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
       77  WS-CHECK                  PIC  X(18)    VALUE "aeWlimemnomLalismJ".
       77  WS-SUB                    PIC  9(04)    COMP-5.
       77  WS-S                      PIC  9(04)    COMP-5.
       77  WS-S1                     PIC  9(04)    COMP-5.
       77  WS-S2                     PIC  9(04)    COMP-5.
       77  WS-S3                     PIC  9(04)    COMP-5.
       77  WS-S4                     PIC  9(04)    COMP-5.
       77  WS-S5                     PIC  9(04)    COMP-5.
       77  WS-S6                     PIC  9(04)    COMP-5.
       77  WS-S7                     PIC  9(04)    COMP-5.
       77  WS-S8                     PIC  9(04)    COMP-5.
       77  WS-S9                     PIC  9(04)    COMP-5.
       77  WS-IXS1                   PIC  9(04)    COMP-5.
       77  WS-ENQPOS                 PIC  9(06)    COMP-5.
       77  WS-ENQEND                 PIC  9(06)    COMP-5.
       77  WS-NETKEY                 PIC  9(06)    COMP-5.
       77  WS-PARKEY                 PIC  9(06)    COMP-5.
       77  WS-PAGE                   PIC  9(04)    COMP-5.
       77  WS-PGE-LENGTH             PIC  9(02)    VALUE 66.
       77  WS-PRN-LENGTH             PIC  9(02)    VALUE 62.
       77  WS-COND                   PIC  X(06).
       77  WS-NORM                   PIC  X(06).
       77  WS-EXPP                   PIC  X(06).
       77  WS-ECAN                   PIC  X(06).
       77  WS-8LPI                   PIC  X(06).
       77  WS-6LPI                   PIC  X(06).
       77  WS-10CPI                  PIC  X(06).
       77  WS-12CPI                  PIC  X(06).
       77  WS-17CPI                  PIC  X(06).
       77  WS-KEYSTR                 PIC  9(06)    COMP-5.
       77  WS-OPTION                 PIC  X(01).
       77  WS-ETYPE                  PIC  X(01).
       77  WS-ERROR                  PIC  9(01)    VALUE ZERO.
       77  WS-ERR1                   PIC  X(22)    VALUE "Invalid Account Number".
       77  WS-ERR2                   PIC  X(09)    VALUE "No Record".
       77  PRG-DEBT-LUP              PIC  X(12)    VALUE "DTP\DTPLOOK".
       77  TODAY-DDMMYY              PIC  9(08)    COMP-5.
       77  WS-USUB                   PIC  9(04)    COMP-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *         C = Clear comment line (50)
      *         E = Clear lines (any line or lines between 5 and 46)
      *         H = Change heading
      *         S = Clear the screen and display headings
      *
           03  CRT-TYPE              PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START             PIC  9(02).
           03  CRT-END               PIC  9(02).
      *
      *    Calling Program
      *
           03  CRT-PROGRAM           PIC  X(15) VALUE "DTPCOL".
      *
      *    Screen Heading
      *
           03  CRT-HEADING           PIC  X(40) VALUE "ACCOUNTS RECEIVABLE - COLLECTIONS".
      *
      *    Company Name
      *
           03  CRT-COMPANY           PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-WRKHD             PIC  X(11).
           03  CRT-WRKST             PIC  X(03).

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
    03  WS-PROG        PIC  X(03) VALUE "ENQ".

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
        05  W02-REPORT PIC X(07) VALUE "DTPCOL.".
               05  W02-USER   PIC X(05) VALUE SPACES.

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

       01  W90-TRAN.
          03  W90-DTE        PIC  99/99/9999.
           03  W90-CODE        PIC  9(02).
           03  W90-RESULT      PIC  9(04)    COMP-5.
           03  W90-TRNKEY      PIC  9(04)    COMP-5.
           03  W90-TS          PIC  9(02)    COMP-5.
           03  W90-CATKEY      PIC  9(04)    COMP-5.
           03  W90-CS          PIC  9(02)    COMP-5.
          03  W90-CDESC       PIC  X(30).
          03  W90-SMAN        PIC  X(30).
    03  W90-TERMS       PIC  X(12).
    03  W90-TYPE        PIC  X(12).
    03  W90-PRICE       PIC  X(12).
    03  W90-120        PIC S9(09)V99 COMP-3.
           03  W90-90          PIC S9(09)V99 COMP-3.
           03  W90-60          PIC S9(09)V99 COMP-3.
           03  W90-30          PIC S9(09)V99 COMP-3.
           03  W90-CUR         PIC S9(09)V99 COMP-3.
           03  W90-BAL         PIC S9(09)V99 COMP-3.
           03  W90-INT         PIC S9(09)V99 COMP-3.

       01  W95-STM.
           03  W95-COMP        PIC X(40).
           03  W95-V2.
               05  W95-B       PIC Z(06)9.99-.
           03  W95-D.
               05  W95-DTE     PIC Z9/99/9999.
           03  W95-POST        PIC 9(04).

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

       01  W200-KEYS.
    03  W200-KEY        PIC  X(10) OCCURS 100.

       01  W205-MEMO.
    03  W205-MEM-DET.
        05  W205-MEM    PIC  X(60) OCCURS 100.
    03  W205-S        PIC  9(04) COMP-5.
    03  W205-S1        PIC  9(04) COMP-5.
    03  W205-S2        PIC  9(04) COMP-5.
    03  W205-LAST       PIC  9(04) COMP-5.

      /
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

      /
      *   ******    ******   *******   ********  ********  **     **
      *  **    **  **    **  **    **  **        **        ***    **
      *  **        **        **   **   **        **        ** *   **
      *   ******   **        ******    *****     *****     **  *  **
      *        **  **        **   **   **        **        **   * **
      *  **    **  **    **  **    **  **        **        **    ***
      *   ******    ******   **    **  ********  ********  **     **
      *
       SCREEN SECTION.

       01  S08.
          03  LINE  2 COLUMN 34 FOREGROUND-COLOR Grey HIGHLIGHT HIGHLIGHT
           VALUE "DEBTOR ENQUIRY".
           03  LINE  3 COLUMN  2 VALUE "Account No:".
           03          COLUMN 21 VALUE "Name:".
          03        COLUMN 66 VALUE "°°Language:°°°".
           03  LINE  4 COLUMN  2 VALUE "Address   :".
           03  LINE  5 COLUMN  2 VALUE "Telephone :".
           03          COLUMN 33 VALUE "Contact :".
           03  LINE  6 COLUMN  2 VALUE "Fax number:".
           03          COLUMN 33 VALUE "Purchases  YTD :".
           03          COLUMN 62 VALUE "Prev :".
          03  LINE  7 COLUMN  5 VALUE "Date".
          03        COLUMN 13 VALUE "Description".
          03        COLUMN 26 VALUE "Ref No".
          03        COLUMN 40 VALUE "Value".

       01  S09.
           03  LINE  7 COLUMN 48 PIC X(01) USING WS-G3.
           03          COLUMN 51 VALUE "Opening balance :".
           03  LINE  8 COLUMN 48 PIC X(01) USING WS-G3.
           03          COLUMN 51 VALUE "Debits          :".
           03  LINE  9 COLUMN 48 PIC X(01) USING WS-G3.
           03          COLUMN 51 VALUE "Credits         :".
           03  LINE 10 COLUMN 48 PIC X(01) USING WS-G3.
           03          COLUMN 51 VALUE "Closing balance :".
           03  LINE 11 COLUMN 48 PIC X(01) USING WS-G3.
           03  LINE 12 COLUMN 48 PIC X(01) USING WS-G3.
           03          COLUMN 51 VALUE "Last active     :".
           03  LINE 13 COLUMN 48 PIC X(01) USING WS-G3.
           03          COLUMN 51 VALUE "Last payment    :".
           03  LINE 14 COLUMN 48 PIC X(01) USING WS-G3.
          03        COLUMN 51 VALUE "Age Analysis     ".
           03  LINE 15 COLUMN 48 PIC X(01) USING WS-G3.
           03          COLUMN 51 VALUE "    Current     :".
           03  LINE 16 COLUMN 48 PIC X(01) USING WS-G3.
           03          COLUMN 51 VALUE "    30 days     :".
           03  LINE 17 COLUMN 48 PIC X(01) USING WS-G3.
           03          COLUMN 51 VALUE "    60 days     :".
           03  LINE 18 COLUMN 48 PIC X(01) USING WS-G3.
           03          COLUMN 51 VALUE "    90 days     :".
           03  LINE 19 COLUMN 48 PIC X(01) USING WS-G3.
           03          COLUMN 51 VALUE "    120 days +  :".
           03  LINE 20 COLUMN 48 PIC X(01) USING WS-G3.
           03          COLUMN 51 VALUE "    Interest    :".
           03  LINE 21 COLUMN 48 PIC X(01) USING WS-G3.
           03          COLUMN 51 VALUE "Arrears         :".
           03  LINE 22 COLUMN 48 PIC X(01) USING WS-G3.
           03          COLUMN 51 VALUE "Credit limit    :".

       01  S09A.
           03  LINE  7 COLUMN 45 BACKGROUND-COLOR Grey HIGHLIGHT
     PIC  X(34) FROM W40-SPACES.
           03  LINE  8 COLUMN 45 BACKGROUND-COLOR Grey HIGHLIGHT
     PIC  X(34) FROM W40-SPACES.
           03  LINE  9 COLUMN 45 BACKGROUND-COLOR Grey HIGHLIGHT
     PIC  X(34) FROM W40-SPACES.
           03  LINE 10 COLUMN 45 BACKGROUND-COLOR Grey HIGHLIGHT
     PIC  X(34) FROM W40-SPACES.
           03  LINE 11 COLUMN 45 BACKGROUND-COLOR Grey HIGHLIGHT
     PIC  X(34) FROM W40-SPACES.
           03  LINE 12 COLUMN 45 BACKGROUND-COLOR Grey HIGHLIGHT
     PIC  X(34) FROM W40-SPACES.
           03  LINE 13 COLUMN 45 BACKGROUND-COLOR Grey HIGHLIGHT
     PIC  X(34) FROM W40-SPACES.
           03  LINE 14 COLUMN 45 BACKGROUND-COLOR Grey HIGHLIGHT
     PIC  X(34) FROM W40-SPACES.
           03  LINE 15 COLUMN 45 BACKGROUND-COLOR Grey HIGHLIGHT
     PIC  X(34) FROM W40-SPACES.
           03  LINE 16 COLUMN 45 BACKGROUND-COLOR Grey HIGHLIGHT
     PIC  X(34) FROM W40-SPACES.
           03  LINE 17 COLUMN 45 BACKGROUND-COLOR Grey HIGHLIGHT
     PIC  X(34) FROM W40-SPACES.
           03  LINE 18 COLUMN 45 BACKGROUND-COLOR Grey HIGHLIGHT
     PIC  X(34) FROM W40-SPACES.
           03  LINE 19 COLUMN 45 BACKGROUND-COLOR Grey HIGHLIGHT
     PIC  X(34) FROM W40-SPACES.
           03  LINE 20 COLUMN 45 BACKGROUND-COLOR Grey HIGHLIGHT
     PIC  X(34) FROM W40-SPACES.
           03  LINE 21 COLUMN 45 BACKGROUND-COLOR Grey HIGHLIGHT
     PIC  X(34) FROM W40-SPACES.
           03  LINE 22 COLUMN 45 BACKGROUND-COLOR Grey HIGHLIGHT
     PIC  X(34) FROM W40-SPACES.
           03  LINE  7 COLUMN 47 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT VALUE "Allocated".
           03          COLUMN 61 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT VALUE "Balance".
    03        COLUMN 70 BACKGROUND-COLOR Grey HIGHLIGHT
     FOREGROUND-COLOR Black HIGHLIGHT VALUE "Ref. No.".
           03  LINE  8 COLUMN 46 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-ALL1.
           03          COLUMN 58 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-BAL1.
    03        COLUMN 70 BACKGROUND-COLOR Grey HIGHLIGHT
     FOREGROUND-COLOR Black HIGHLIGHT
     PIC  X(08) FROM W15-REC1.
           03  LINE  9 COLUMN 46 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-ALL2.
           03          COLUMN 58 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-BAL2.
    03        COLUMN 70 BACKGROUND-COLOR Grey HIGHLIGHT
     FOREGROUND-COLOR Black HIGHLIGHT
     PIC  X(08) FROM W15-REC2.
    03  LINE 10 COLUMN 46 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-ALL3.
           03          COLUMN 58 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-BAL3.
    03        COLUMN 70 BACKGROUND-COLOR Grey HIGHLIGHT
     FOREGROUND-COLOR Black HIGHLIGHT
     PIC  X(08) FROM W15-REC3.
    03  LINE 11 COLUMN 46 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-ALL4.
           03          COLUMN 58 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-BAL4.
    03        COLUMN 70 BACKGROUND-COLOR Grey HIGHLIGHT
     FOREGROUND-COLOR Black HIGHLIGHT
     PIC  X(08) FROM W15-REC4.
    03  LINE 12 COLUMN 46 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-ALL5.
           03          COLUMN 58 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-BAL5.
    03        COLUMN 70 BACKGROUND-COLOR Grey HIGHLIGHT
     FOREGROUND-COLOR Black HIGHLIGHT
     PIC  X(08) FROM W15-REC5.
    03  LINE 13 COLUMN 46 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-ALL6.
           03          COLUMN 58 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-BAL6.
    03        COLUMN 70 BACKGROUND-COLOR Grey HIGHLIGHT
     FOREGROUND-COLOR Black HIGHLIGHT
     PIC  X(08) FROM W15-REC6.
    03  LINE 14 COLUMN 46 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-ALL7.
           03          COLUMN 58 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-BAL7.
    03        COLUMN 70 BACKGROUND-COLOR Grey HIGHLIGHT
     FOREGROUND-COLOR Black HIGHLIGHT
     PIC  X(08) FROM W15-REC7.
    03  LINE 15 COLUMN 46 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-ALL8.
           03          COLUMN 58 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-BAL8.
    03        COLUMN 70 BACKGROUND-COLOR Grey HIGHLIGHT
     FOREGROUND-COLOR Black HIGHLIGHT
     PIC  X(08) FROM W15-REC8.
    03  LINE 16 COLUMN 46 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-ALL9.
           03          COLUMN 58 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-BAL9.
    03        COLUMN 70 BACKGROUND-COLOR Grey HIGHLIGHT
     FOREGROUND-COLOR Black HIGHLIGHT
     PIC  X(08) FROM W15-REC9.
    03  LINE 17 COLUMN 46 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-ALL10.
           03          COLUMN 58 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-BAL10.
    03        COLUMN 70 BACKGROUND-COLOR Grey HIGHLIGHT
     FOREGROUND-COLOR Black HIGHLIGHT
     PIC  X(08) FROM W15-REC10.
    03  LINE 18 COLUMN 46 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-ALL11.
           03          COLUMN 58 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-BAL11.
    03        COLUMN 70 BACKGROUND-COLOR Grey HIGHLIGHT
     FOREGROUND-COLOR Black HIGHLIGHT
     PIC  X(08) FROM W15-REC11.
    03  LINE 19 COLUMN 46 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-ALL12.
           03          COLUMN 58 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-BAL12.
    03        COLUMN 70 BACKGROUND-COLOR Grey HIGHLIGHT
     FOREGROUND-COLOR Black HIGHLIGHT
     PIC  X(08) FROM W15-REC12.
    03  LINE 20 COLUMN 46 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-ALL13.
           03          COLUMN 58 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-BAL13.
    03        COLUMN 70 BACKGROUND-COLOR Grey HIGHLIGHT
     FOREGROUND-COLOR Black HIGHLIGHT
     PIC  X(08) FROM W15-REC13.
    03  LINE 21 COLUMN 46 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-ALL14.
           03          COLUMN 58 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-BAL14.
    03        COLUMN 70 BACKGROUND-COLOR Grey HIGHLIGHT
     FOREGROUND-COLOR Black HIGHLIGHT
     PIC  X(08) FROM W15-REC14.
    03  LINE 22 COLUMN 46 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-ALL15.
           03          COLUMN 58 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 PIC  X(11) FROM W15-BAL15.
    03        COLUMN 70 BACKGROUND-COLOR Grey HIGHLIGHT
     FOREGROUND-COLOR Black HIGHLIGHT
     PIC  X(08) FROM W15-REC15.

       01  S09B.
           03  LINE  7 COLUMN 51 BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC  X(28) FROM W40-TOP.
           03  LINE  8 COLUMN 51 BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC  X(28) FROM W40-MID.
           03  LINE  9 COLUMN 51 BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC  X(28) FROM W40-MID.
           03  LINE 10 COLUMN 51 BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC  X(28) FROM W40-MID.
           03  LINE 11 COLUMN 51 BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC  X(28) FROM W40-MID.
           03  LINE 12 COLUMN 51 BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC  X(28) FROM W40-MID.
           03  LINE 13 COLUMN 51 BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC  X(28) FROM W40-MID.
           03  LINE 14 COLUMN 51 BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC  X(28) FROM W40-MID.
           03  LINE 15 COLUMN 51 BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC  X(28) FROM W40-MID.
           03  LINE 16 COLUMN 51 BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC  X(28) FROM W40-MID.
           03  LINE 17 COLUMN 51 BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC  X(28) FROM W40-MID.
           03  LINE 18 COLUMN 51 BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC  X(28) FROM W40-MID.
           03  LINE 19 COLUMN 51 BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC  X(28) FROM W40-MID.
           03  LINE 20 COLUMN 51 BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC  X(28) FROM W40-MID.
           03  LINE 21 COLUMN 51 BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC  X(28) FROM W40-BOT.
           03  LINE  8 COLUMN 53 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 VALUE "PURCHASES LAST 12 MONTHS".
           03  LINE  9 COLUMN 53 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 VALUE "January   :".
           03          COLUMN 65 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
     PIC  Z(08)9.99- FROM DEB-MTHPUR(1).
           03  LINE 10 COLUMN 53 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 VALUE "February  :".
           03          COLUMN 65 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
     PIC  Z(08)9.99- FROM DEB-MTHPUR(2).
           03  LINE 11 COLUMN 53 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 VALUE "March     :".
           03          COLUMN 65 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
     PIC  Z(08)9.99- FROM DEB-MTHPUR(3).
           03  LINE 12 COLUMN 53 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 VALUE "April     :".
           03          COLUMN 65 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
     PIC  Z(08)9.99- FROM DEB-MTHPUR(4).
           03  LINE 13 COLUMN 53 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 VALUE "May       :".
           03          COLUMN 65 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
     PIC  Z(08)9.99- FROM DEB-MTHPUR(5).
           03  LINE 14 COLUMN 53 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 VALUE "June      :".
           03          COLUMN 65 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
     PIC  Z(08)9.99- FROM DEB-MTHPUR(6).
           03  LINE 15 COLUMN 53 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 VALUE "July      :".
           03          COLUMN 65 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
     PIC  Z(08)9.99- FROM DEB-MTHPUR(7).
           03  LINE 16 COLUMN 53 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 VALUE "August    :".
           03          COLUMN 65 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
     PIC  Z(08)9.99- FROM DEB-MTHPUR(8).
           03  LINE 17 COLUMN 53 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 VALUE "September :".
           03          COLUMN 65 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
     PIC  Z(08)9.99- FROM DEB-MTHPUR(9).
           03  LINE 18 COLUMN 53 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 VALUE "October   :".
           03          COLUMN 65 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
     PIC  Z(08)9.99- FROM DEB-MTHPUR(10).
           03  LINE 19 COLUMN 53 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 VALUE "November  :".
           03          COLUMN 65 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
     PIC  Z(08)9.99- FROM DEB-MTHPUR(11).
           03  LINE 20 COLUMN 53 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
                                 VALUE "December  :".
           03          COLUMN 65 BACKGROUND-COLOR Grey HIGHLIGHT
                                 FOREGROUND-COLOR Black HIGHLIGHT
     PIC  Z(08)9.99- FROM DEB-MTHPUR(12).
       01  S10.
    03  LINE  8 COLUMN  5 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT VALUE
       "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
    03  LINE  9 COLUMN  5 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT
     VALUE "³ Delivery Address : ".
    03        COLUMN 25 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Grey HIGHLIGHT HIGHLIGHT
     PIC  X(30) USING W40-PADD1.
    03        COLUMN 55 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT
     VALUE " ³".
    03  LINE 10 COLUMN  5 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT
     VALUE "³                    ".
    03        COLUMN 25 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Grey HIGHLIGHT HIGHLIGHT
     PIC  X(30) USING W40-PADD2.
    03        COLUMN 55 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT
     VALUE " ³".
    03  LINE 11 COLUMN  5 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT
     VALUE "³                    ".
    03        COLUMN 25 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Grey HIGHLIGHT HIGHLIGHT
     PIC  X(30) USING W40-PADD3.
    03        COLUMN 55 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT
     VALUE " ³".
    03  LINE 12 COLUMN  5 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT
     VALUE "³                    ".
    03        COLUMN 25 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Grey HIGHLIGHT HIGHLIGHT
     PIC  X(30) USING W40-PADD4.
    03        COLUMN 55 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT
     VALUE " ³".
    03  LINE 13 COLUMN  5 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT
     VALUE "³ Postal Code        ".
    03        COLUMN 25 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Grey HIGHLIGHT HIGHLIGHT
     PIC  9(04) USING W40-PPOST.
    03        COLUMN 29 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT VALUE
      "                           ³".
    03  LINE 14 COLUMN  5 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT
     VALUE "³ Category       ".
    03        COLUMN 22 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Black HIGHLIGHT
     PIC  99B USING DEB-CAT.
    03        COLUMN 25 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Grey HIGHLIGHT HIGHLIGHT
     PIC  X(30) USING W90-CDESC.
    03        COLUMN 55 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT
     VALUE " ³".
    03  LINE 15 COLUMN  5 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT
     VALUE "³ Terms          ".
    03        COLUMN 22 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Black HIGHLIGHT
     PIC  X(03) USING DEB-TERMS.
    03        COLUMN 25 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Grey HIGHLIGHT HIGHLIGHT
     PIC  X(12) USING W90-TERMS.
    03        COLUMN 37 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT VALUE
     "                   ³".
    03  LINE 16 COLUMN  5 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT
     VALUE "³ Price Code     ".
    03        COLUMN 22 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Black HIGHLIGHT
     PIC  X(03) USING DEB-PRICE.
    03        COLUMN 25 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Grey HIGHLIGHT HIGHLIGHT
     PIC  X(12) USING W90-PRICE.
    03        COLUMN 37 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT VALUE
     "                   ³".
    03  LINE 17 COLUMN  5 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT
     VALUE "³ Sales Person   ".
    03        COLUMN 22 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Black HIGHLIGHT
     PIC  99B USING DEB-SMAN.
    03        COLUMN 25 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Grey HIGHLIGHT HIGHLIGHT
     PIC  X(30) USING W90-SMAN.
    03        COLUMN 55 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT
     VALUE " ³".
    03  LINE 18 COLUMN  5 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT
     VALUE "³ Invoice Type   ".
    03        COLUMN 22 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Black HIGHLIGHT
     PIC  X(03) USING DEB-INV-TYPE.
    03        COLUMN 25 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Grey HIGHLIGHT HIGHLIGHT
     PIC  X(12) USING W90-TYPE.
    03        COLUMN 37 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT VALUE
     "                   ³".
    03  LINE 19 COLUMN  5 BACKGROUND-COLOR Green HIGHLIGHT
     FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT VALUE
       "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S36.
          03  FOREGROUND-COLOR Grey HIGHLIGHT HIGHLIGHT BACKGROUND-COLOR Magenta HIGHLIGHT
      PIC X(06) USING W80-ACNO AUTO.

       01  S-DEB.
          03  LINE  2 COLUMN 30 FOREGROUND-COLOR Grey HIGHLIGHT HIGHLIGHT
           VALUE "DEBTOR COLLECTION LOOKUP".
          03  LINE  3 COLUMN  2 VALUE "Start No/Name:".
          03        COLUMN 23 VALUE "/".
           03  LINE  4 COLUMN  2 REVERSE-VIDEO VALUE
              "Ac/No  Customer name                            Tot.Bala
      -        "nce  Past Due  T  Days".
          03  LINE  5 COLUMN  1 BACKGROUND-COLOR Black HIGHLIGHT VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍÑÍÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR Black HIGHLIGHT VALUE "µ".
          03  LINE 22 COLUMN  1 BACKGROUND-COLOR Black HIGHLIGHT VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍÏÍÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR Black HIGHLIGHT VALUE "µ".
           03  LINE 23 COLUMN  2 FOREGROUND-COLOR Grey HIGHLIGHT VALUE
             "Key in at least the first character of the name or press".
          03        COLUMN 59 FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT
     VALUE "Esc".
           03          COLUMN 62 FOREGROUND-COLOR Grey HIGHLIGHT VALUE " to exit".
          03  LINE 25 COLUMN  2 BACKGROUND-COLOR Cyan HIGHLIGHT
     FOREGROUND-COLOR Blue HIGHLIGHT VALUE "Pressing '".
          03        COLUMN 12 BACKGROUND-COLOR Cyan HIGHLIGHT
     FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT
     VALUE "ENTER".
          03        COLUMN 17 BACKGROUND-COLOR Cyan HIGHLIGHT
     FOREGROUND-COLOR Blue HIGHLIGHT VALUE
                                 "' key only, will start from the beginn
      -                          "ing of the debtor file".
       01  SA-ACT.
          03  LINE  3 COLUMN 17 FOREGROUND-COLOR Grey HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT
           PIC  X(06) USING W80-ACNO AUTO.
       01  SA-DEB.
          03  LINE  3 COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT
                                 PIC  X(40) USING W40-NAME AUTO.

       01  S-COL.
    03  LINE  3 COLUMN  2 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green HIGHLIGHT VALUE "ACNO " .
    03        COLUMN  7 FOREGROUND-COLOR Grey HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT
     PIC  X(06) FROM DEB-ACNO.
    03        COLUMN 13 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green HIGHLIGHT VALUE " NAME ".
    03        COLUMN 19 FOREGROUND-COLOR Grey HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT
     PIC  X(40) FROM W40-NAME.
    03        COLUMN 60 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green HIGHLIGHT VALUE "  TEL ".
    03        COLUMN 66 FOREGROUND-COLOR Grey HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT
     PIC  X(14) FROM W40-TEL.
    03  LINE  4 COLUMN  2 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green HIGHLIGHT VALUE " MTD ".
    03        COLUMN  7 FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT
     PIC ZZZZZZZZ9.99- FROM W40-A-MTD.
    03        COLUMN 20 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green HIGHLIGHT VALUE "   CUR ".
    03        COLUMN 27 FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT
     PIC ZZZZZZZZ9.99- FROM W40-A-CUR.
    03        COLUMN 40 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green HIGHLIGHT VALUE "   30D ".
    03        COLUMN 47 FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT
     PIC ZZZZZZZZ9.99- FROM W40-A-30.
    03        COLUMN 60 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green HIGHLIGHT VALUE "  60D ".
    03        COLUMN 66 FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT
     PIC ZZZZZZZZZ9.99- FROM W40-A-60.
    03  LINE  5 COLUMN  2 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green HIGHLIGHT VALUE " 90D ".
    03        COLUMN  7 FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT
     PIC ZZZZZZZZ9.99- FROM W40-A-90.
    03        COLUMN 20 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green HIGHLIGHT VALUE "  120D ".
    03        COLUMN 27 FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT
     PIC ZZZZZZZZ9.99- FROM W40-A-120.
    03        COLUMN 40 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green HIGHLIGHT VALUE "   INT ".
    03        COLUMN 47 FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT
     PIC ZZZZZZZZ9.99- FROM W40-A-INT.
    03        COLUMN 60 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Green HIGHLIGHT VALUE "  DUE ".
    03        COLUMN 66 FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT
     PIC ZZZZZZZZZ9.99- FROM W40-D-MTD.
    03  LINE  6 COLUMN 17 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(01) USING WS-G9.
    03        COLUMN 18 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(60) USING WS-MEM-LINE.
    03        COLUMN 78 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(01) USING WS-G10.
    03  LINE  7 COLUMN 17 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(01) USING WS-G3.
    03        COLUMN 18 FOREGROUND-COLOR Black HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT VALUE
    "   C O L L E C T I O N   N O T E S   A M E N D / V I E W
      -    "".
    03        COLUMN 78 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(01) USING WS-G3.
    03  LINE  8 COLUMN 17 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(01) USING WS-G14.
    03        COLUMN 18 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(60) USING WS-HME-LINE.
    03        COLUMN 78 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(01) USING WS-G13.
    03  LINE  9 COLUMN 17 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(62) USING WS-MM-LINE.
    03  LINE 10 COLUMN 17 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
     PIC X(62) USING WS-MM-LINE.
    03  LINE 11 COLUMN 17 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(62) USING WS-MM-LINE.
    03  LINE 12 COLUMN 17 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(62) USING WS-MM-LINE.
    03  LINE 13 COLUMN 17 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(62) USING WS-MM-LINE.
    03  LINE 14 COLUMN 17 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(62) USING WS-MM-LINE.
    03  LINE 15 COLUMN 17 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(62) USING WS-MM-LINE.
    03  LINE 16 COLUMN 17 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(62) USING WS-MM-LINE.
    03  LINE 17 COLUMN 17 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(62) USING WS-MM-LINE.
    03  LINE 18 COLUMN 17 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(62) USING WS-MM-LINE.
    03  LINE 19 COLUMN 17 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(62) USING WS-MM-LINE.
    03  LINE 20 COLUMN 17 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(62) USING WS-MM-LINE.
    03  LINE 21 COLUMN 17 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(01) USING WS-G11.
    03        COLUMN 18 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(60) USING WS-MEM-LINE.
    03        COLUMN 78 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(01) USING WS-G12.
       01  SA-COL.
    03  LINE  9 COLUMN 18 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(60) USING W200-MEM1 AUTO.
    03  LINE 10 COLUMN 18 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(60) USING W200-MEM2 AUTO.
    03  LINE 11 COLUMN 18 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(60) USING W200-MEM3 AUTO.
    03  LINE 12 COLUMN 18 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(60) USING W200-MEM4 AUTO.
    03  LINE 13 COLUMN 18 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(60) USING W200-MEM5 AUTO.
    03  LINE 14 COLUMN 18 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(60) USING W200-MEM6 AUTO.
    03  LINE 15 COLUMN 18 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(60) USING W200-MEM7 AUTO.
    03  LINE 16 COLUMN 18 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(60) USING W200-MEM8 AUTO.
    03  LINE 17 COLUMN 18 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(60) USING W200-MEM9 AUTO.
    03  LINE 18 COLUMN 18 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(60) USING W200-MEM10 AUTO.
    03  LINE 19 COLUMN 18 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(60) USING W200-MEM11 AUTO.
    03  LINE 20 COLUMN 18 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(60) USING W200-MEM12 AUTO.


       01  S-DEB-MEM.
           03  LINE  7 COLUMN 18 FOREGROUND-COLOR Black HIGHLIGHT
                                 BACKGROUND-COLOR Grey HIGHLIGHT VALUE
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E   ".

       01  S-MEM.
           03  LINE  6 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(01) USING WS-G9.
           03          COLUMN 18 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(01) USING WS-G10.
           03  LINE  7 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(01) USING WS-G3.
           03          COLUMN 18 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey VALUE "   M E M O / R E M A R K S / C O R R E S P O N D E N C E".
           03          COLUMN 78 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(01) USING WS-G3.
           03  LINE  8 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(01) USING WS-G14.
           03          COLUMN 18 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(60) USING WS-HME-LINE.
           03          COLUMN 78 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(01) USING WS-G13.
           03  LINE  9 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(62) USING WS-MM-LINE.
           03  LINE 10 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(62) USING WS-MM-LINE.
           03  LINE 11 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(62) USING WS-MM-LINE.
           03  LINE 12 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(62) USING WS-MM-LINE.
           03  LINE 13 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(62) USING WS-MM-LINE.
           03  LINE 14 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(62) USING WS-MM-LINE.
           03  LINE 15 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(62) USING WS-MM-LINE.
           03  LINE 16 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(62) USING WS-MM-LINE.
           03  LINE 17 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(62) USING WS-MM-LINE.
           03  LINE 18 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(62) USING WS-MM-LINE.
           03  LINE 19 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(62) USING WS-MM-LINE.
           03  LINE 20 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(62) USING WS-MM-LINE.
           03  LINE 21 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(01) USING WS-G11.
           03          COLUMN 18 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(01) USING WS-G12.

       01  SA-MEM.
           03  LINE  9 COLUMN 18 FOREGROUND-COLOR Black HIGHLIGHT BACKGROUND-COLOR Grey HIGHLIGHT PIC X(60) USING W200-MEM1 AUTO.
           03  LINE 10 COLUMN 18 FOREGROUND-COLOR Black HIGHLIGHT BACKGROUND-COLOR Grey HIGHLIGHT PIC X(60) USING W200-MEM2 AUTO.
           03  LINE 11 COLUMN 18 FOREGROUND-COLOR Black HIGHLIGHT BACKGROUND-COLOR Grey HIGHLIGHT PIC X(60) USING W200-MEM3 AUTO.
           03  LINE 12 COLUMN 18 FOREGROUND-COLOR Black HIGHLIGHT BACKGROUND-COLOR Grey HIGHLIGHT PIC X(60) USING W200-MEM4 AUTO.
           03  LINE 13 COLUMN 18 FOREGROUND-COLOR Black HIGHLIGHT BACKGROUND-COLOR Grey HIGHLIGHT PIC X(60) USING W200-MEM5 AUTO.
           03  LINE 14 COLUMN 18 FOREGROUND-COLOR Black HIGHLIGHT BACKGROUND-COLOR Grey HIGHLIGHT PIC X(60) USING W200-MEM6 AUTO.
           03  LINE 15 COLUMN 18 FOREGROUND-COLOR Black HIGHLIGHT BACKGROUND-COLOR Grey HIGHLIGHT PIC X(60) USING W200-MEM7 AUTO.
           03  LINE 16 COLUMN 18 FOREGROUND-COLOR Black HIGHLIGHT BACKGROUND-COLOR Grey HIGHLIGHT PIC X(60) USING W200-MEM8 AUTO.
           03  LINE 17 COLUMN 18 FOREGROUND-COLOR Black HIGHLIGHT BACKGROUND-COLOR Grey HIGHLIGHT PIC X(60) USING W200-MEM9 AUTO.
           03  LINE 18 COLUMN 18 FOREGROUND-COLOR Black HIGHLIGHT BACKGROUND-COLOR Grey HIGHLIGHT PIC X(60) USING W200-MEM10 AUTO.
           03  LINE 19 COLUMN 18 FOREGROUND-COLOR Black HIGHLIGHT BACKGROUND-COLOR Grey HIGHLIGHT PIC X(60) USING W200-MEM11 AUTO.
           03  LINE 20 COLUMN 18 FOREGROUND-COLOR Black HIGHLIGHT BACKGROUND-COLOR Grey HIGHLIGHT PIC X(60) USING W200-MEM12 AUTO.

      /
      *        *******   *******    ******    ******   ********  *******   **     **  *******    ******** 
      *        **    **  **    **  **    **  **    **  **        **    **  **     **  **    **   **
      *        **    **  **   **   **    **  **        **        **    **  **     **  **   **    **
      *        *******   ******    **    **  **        *****     **    **  **     **  ******     *****
      *        **        **   **   **    **  **        **        **    **  **     **  **   **    **
      *        **        **    **  **    **  **    **  **        **    **  **     **  **    **   **
      *        **        **    **   ******    ******   ********  *******    *******   **    **   ********
      *
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS.
       AA000-MAIN        SECTION.
       AA000-INIT.
           IF LS0-DBLEV < 2
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM BA000.
             CLOSE DEBCOL.

       AA49.
            EXIT PROGRAM.

      /    *************************************************************
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

       COPY "LOCKED.RC2".

       COPY "CLEAR.CRT".

      /
      *    ****    READ FILES ROUTINES
      *
       AC000              SECTION.

       COPY "CONTROL.RD".

       COPY "DBTRAN.RD".

       COPY "DEBCOL.RD".

       COPY "DEBTOR.RD".

       COPY "DEBMEM.RD".

       COPY "PARAM.RD".

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
       AD000             SECTION.

       COPY "DEBCOL.WR".

      *
      *    G E T   A   D E B T O R   T R A N S A C T I O N   R E C O R D
      *
      *    PERFORM THIS ROUTINE WITH THE TRANSACTION CODE IN W90-CODE
      *
      *    ON EXITING THE TRANSACTION CODE DETAILS ARE IN W00-
      *    WHILE W90-TRNKEY CONTAINS THE KEY TO THE PARAMETER FILE
      *    WHERE THIS TRANSACTION CODE IS FOUND AND W90-TS IS THE
      *    TRANSACTION CODE SUBSCRIPT IN THE RECORD.
      *
       GET-DEBTOR-TRAN-RECORD SECTION.

       GET-DEBTOR-TRAN.
             ADD 21 W90-CODE     GIVING W90-RESULT.
             DIVIDE W90-RESULT BY 2
                                 GIVING W90-TRNKEY
                                 REMAINDER W90-TS.
             ADD 1               TO W90-TS.

       GET-DEBTOR-TRAN-CODE.
             MOVE W90-TRNKEY     TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.

       STORE-DEBTOR-TRAN-DESC.
            MOVE PAR-E-DESC(W90-TS)
                                 TO W00-E-DESC.
            MOVE PAR-A-DESC(W90-TS)
                                 TO W00-A-DESC.
            MOVE PAR-ACTION(W90-TS)
                                 TO W00-ACTION.
            MOVE PAR-T-CODE(W90-TS)
                                 TO W00-T-CODE.
            MOVE PAR-T-GL(W90-TS)
                                 TO W00-T-GL.

       STORE-DEBTOR-TRAN-STATS.
            MOVE PAR-T-VAT(W90-TS)
                                 TO W00-T-VAT.
            MOVE PAR-T-DAY(W90-TS)
                                 TO W00-T-DAY.
            MOVE PAR-T-VAL(W90-TS)
                                 TO W00-T-VAL.
            MOVE PAR-T-MTD(W90-TS)
                                 TO W00-T-MTD.
            MOVE PAR-T-MTDV(W90-TS)
                                 TO W00-T-MTDV.
            MOVE PAR-T-YTD(W90-TS)
                                 TO W00-T-YTD.
            MOVE PAR-T-YTDV(W90-TS)
                                 TO W00-T-YTDV.

       GET-DEBTOR-TRAN-EXIT.
             EXIT.

       GET-DEBTOR-TRAN-DESC SECTION.

       GET-DEBT-TRAN-DESC.
             PERFORM GET-DEBTOR-TRAN.
             MOVE W90-TRNKEY     TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             PERFORM STORE-DEBTOR-TRAN-DESC.

       GET-DEBT-TRAN-DESC-EXIT.
             EXIT.

      /
      *    ****    D E B T O R  C O L L E C T I O N   L O O K U P
      *
       DEBTOR-LOOKUP           SECTION.

       DEBT-LUP-SCREEN.
             PERFORM ERASE-SCREEN.
            DISPLAY S-DEB.
             MOVE SPACE          TO WS-OPTION.

       DEBT-LUP-PERFORM ERASE-SCREEN.
             MOVE 0601           TO CPOS.
             PERFORM DEBT-CRT-EDGE UNTIL CLIN = 22.
            MOVE SPACES  TO W40-NAME W80-ACNO.
      MOVE "N"   TO WS-ETYPE.

       DEBT-LUP-NAME.
            ACCEPT SA-ACT.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
             MOVE SPACES TO W80-ACNO
             GO TO DEBTOR-LOOKUP-EXIT
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO DEBT-LUP-NAME.
    IF NOT (W80-ACNO = SPACES)
        GO TO DEBT-LUP-CONTINUE.
            ACCEPT SA-DEB.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
             MOVE SPACES TO W80-ACNO
             GO TO DEBTOR-LOOKUP-EXIT
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO DEBT-LUP-NAME.

       DEBT-LUP-CONTINUE.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            PERFORM CLEAR-L50.
             MOVE 6              TO CLIN.
             MOVE 1              TO W75-S W75-S1.

       DEBT-LUP-TAB-LOOP.
      MOVE SPACES  TO W75-DNO(W75-S1).
           IF W75-S1 < 18
               ADD 1             TO W75-S1
               GO TO DEBT-LUP-TAB-LOOP.
             MOVE ZERO           TO W75-S1.

       DEBT-LUP-ENTRY-TYPE.
          IF (W40-NAME = SPACES) AND (W80-ACNO = SPACES)
        MOVE " !"  TO W40-NAME
    ELSE
    IF W40-NAME = SPACES
              MOVE "A"   TO WS-ETYPE
              MOVE 6   TO WS-LENGTH
        CALL "CBL_TOUPPER" USING W80-ACNO
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
        MOVE 1   TO WS-LENGTH
    ELSE
        MOVE 40   TO WS-LENGTH
        CALL "CBL_TOUPPER" USING W40-NAME
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
        MOVE 1   TO WS-LENGTH.
    IF WS-ETYPE = "N"
        MOVE W40-NAME  TO DEB-NAME
        PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT
    ELSE
        MOVE W80-ACNO  TO DEB-ACNO
        PERFORM START-AT-DEBT-AC THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               GO TO DEBT-NO-RECORD.

       DEBT-GET-DEBTOR-REC.
           IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
               GO TO DEBT-SKIP-DEBT-READ.
             PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               SUBTRACT 1        FROM W75-S
               GO TO DEBT-FULL-SCREEN.

       DEBT-SKIP-DEBT-READ.
      MOVE DEB-ACNO  TO W75-DNO(W75-S).
             MOVE DEB-NAME       TO W40-NAME.
           IF DEB-TYPE = 1
               INSPECT W40-NAME REPLACING ALL "*" BY " "
               INSPECT W40-NAME REPLACING FIRST "!" BY " ".
            DISPLAY DEB-ACNO AT LINE CLIN COLUMN 2
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT.
            DISPLAY W40-NAME AT LINE CLIN COLUMN 9
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT.
             MOVE DEB-BAL        TO W100-LBAL.
            DISPLAY W100-LBAL AT LINE CLIN COLUMN 50
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT.
    IF DEB-TERMS = 0 OR 3
              COMPUTE W100-LARR = DEB-30 + DEB-60 + DEB-90 + DEB-120
     + DEB-INT
    ELSE
    IF DEB-TERMS = 1
              COMPUTE W100-LARR = DEB-60 + DEB-90 + DEB-120 + DEB-INT
    ELSE
    IF DEB-TERMS = 2
              COMPUTE W100-LARR = DEB-90 + DEB-120 + DEB-INT.
            DISPLAY W100-LARR AT LINE CLIN COLUMN 62
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT.
            DISPLAY DEB-TERMS AT LINE CLIN COLUMN 73
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT.
    IF DEB-120 > ZERO
        DISPLAY "120" AT LINE CLIN COLUMN 76
   WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
    ELSE
    IF DEB-90 > ZERO
        DISPLAY " 90" AT LINE CLIN COLUMN 76
   WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
    ELSE
    IF DEB-60 > ZERO
        DISPLAY " 60" AT LINE CLIN COLUMN 76
   WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
    ELSE
    IF DEB-30 > ZERO
        DISPLAY " 30" AT LINE CLIN COLUMN 76
   WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
    ELSE
        DISPLAY "CUR" AT LINE CLIN COLUMN 76
   WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT.
           IF CLIN < 21
               ADD 1             TO CLIN W75-S
               GO TO DEBT-GET-DEBTOR-REC.
      
       DEBT-FULL-SCREEN.
001020      DISPLAY " " AT 2303 WITH FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT
             " - " WITH FOREGROUND-COLOR Grey HIGHLIGHT
       "PgUp" WITH FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT
                     "/" WITH FOREGROUND-COLOR Grey HIGHLIGHT
       "PgDn" WITH FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT
       " - " WITH FOREGROUND-COLOR Grey HIGHLIGHT
             "ENTER" WITH FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT
             " view - " WITH FOREGROUND-COLOR Grey HIGHLIGHT
             "Esc" WITH FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT
       "ape - " WITH FOREGROUND-COLOR Grey HIGHLIGHT
       "F8" WITH FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT
       " Collect.Notes - "
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
       "F9" WITH FOREGROUND-COLOR Brown HIGHLIGHT HIGHLIGHT
       " memo/remarks"
        WITH FOREGROUND-COLOR Grey HIGHLIGHT.

       DEBT-SAVE-FULL-SCREEN.
      PERFORM SAVE-SCREEN-2.
            MOVE 0   TO W75-S1.
             MOVE 0601           TO CPOS.

       DEBT-CRT-ATTRIB.
      MOVE 3920   TO W41-LENGTH.
      CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
           W42-ATTRIB2
           W41-LENGTH
     RETURNING WS-STATUS.
            MOVE 4000   TO W41-LENGTH.

       DEBT-GET-USER-OPT.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM CLEAR-L50.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
             GO TO DEBT-ADJ-NO-KEYS
   WHEN DOWN-KEY
             GO TO DEBT-CHK-NO-KEYS
   WHEN ENTER-KEY
   WHEN CR-KEY
       MOVE WS1-CURS
     TO CRT-LINE
       PERFORM CD000
       MOVE CRT-LINE
     TO WS1-CURS
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO DEBT-GET-USER-OPT
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W80-ACNO
       GO TO DEBTOR-LOOKUP-EXIT
   WHEN PAGE-DOWN
             GO TO DEBT-GET-NEXT-PAGE
   WHEN PAGE-UP
             GO TO DEBT-GET-PREV-PAGE
   WHEN F8-KEY
       PERFORM DEBT-COLLECTIONS THRU DEBT-COL-EXIT
       PERFORM DEBT-FULL-SCREEN
             GO TO DEBT-GET-USER-OPT
   WHEN F9-KEY
       PERFORM DEBT-ADD-SPECS THRU DEBT-ADD-EXIT
       PERFORM DEBT-FULL-SCREEN
             GO TO DEBT-GET-USER-OPT
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO DEBT-GET-USER-OPT.
      CALL X"E5".
            GO TO DEBT-GET-USER-OPT.

       DEBT-GET-NEXT-PAGE.
           IF W75-S < 16
               GO TO DEBT-GET-USER-OPT.

       DEBT-NEW-PAGE.
             MOVE 0601           TO CPOS.
             PERFORM DEBT-CRT-EDGE UNTIL CLIN = 22.
             MOVE 6              TO CLIN.
             MOVE LOW-VALUES     TO W75-KEYS.
             MOVE 1              TO W75-S W75-S1.
             PERFORM DEBT-LUP-TAB-LOOP.
             GO TO DEBT-GET-DEBTOR-REC.

       DEBT-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
             MOVE "J"            TO WS-OPTION.

       DEBT-GET-PREV-LOOP.
             PERFORM READ-DEBTOR-PREV THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
        MOVE SPACE  TO WS-OPTION
               GO TO DEBT-NEW-PAGE.
           IF WS-IXS1 < 32
               ADD 1             TO WS-IXS1
               GO TO DEBT-GET-PREV-LOOP.
             GO TO DEBT-NEW-PAGE.

       DEBT-NO-RECORD.
            DISPLAY "No record - " AT 5012
        WITH BACKGROUND-COLOR Cyan HIGHLIGHT FOREGROUND-COLOR Blue HIGHLIGHT
             "N" WITH BACKGROUND-COLOR Cyan HIGHLIGHT FOREGROUND-COLOR Brown HIGHLIGHT
             "ext or "
        WITH BACKGROUND-COLOR Cyan HIGHLIGHT FOREGROUND-COLOR Blue HIGHLIGHT
             "Q" WITH BACKGROUND-COLOR Cyan HIGHLIGHT FOREGROUND-COLOR Brown HIGHLIGHT
             "uit > <"
        WITH BACKGROUND-COLOR Cyan HIGHLIGHT FOREGROUND-COLOR Blue HIGHLIGHT.
      
       DEBT-GET-USER-OPT-2.
            ACCEPT WS-OPTION AT 5038
      WITH BACKGROUND-COLOR Cyan HIGHLIGHT FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "N" OR "Q")
               GO TO DEBT-GET-USER-OPT-2.
           IF WS-OPTION = "N"
               GO TO DEBT-LUP-SCREEN.
             MOVE SPACES         TO W80-ACNO.
            GO TO DEBTOR-LOOKUP-EXIT.

       DEBT-ADJ-NO-KEYS.
           IF W75-S1 > 1
               SUBTRACT 1        FROM W75-S1 CLIN.
             GO TO DEBT-HIGHLIGHT.

       DEBT-CHK-NO-KEYS.
           IF W75-S1 = ZERO
               MOVE 1            TO W75-S1
               GO TO DEBT-HIGHLIGHT.
           IF W75-S1 < W75-S
               ADD 1             TO W75-S1 CLIN.

       DEBT-HIGHLIGHT.
             PERFORM DEBT-CRT-ATTRIB.
            DISPLAY W43-DET(CLIN) AT CPOS WITH REVERSE-VIDEO.
             GO TO DEBT-GET-USER-OPT.

       DEBT-ADD-SPECS.
           IF W75-S1 = ZERO
              GO TO DEBT-ADD-EXIT.
            MOVE W75-DNO(W75-S1)
                                 TO DME-AC W80-ACNO.
             MOVE 1              TO DME-SEQ.
             PERFORM READ-DEBMEM THRU READ-DEBMEM-EXIT.
           IF WS-F-ERROR NOT = ZERO
        DISPLAY "No Memo or remarks" AT 5002
   WITH BACKGROUND-COLOR Cyan HIGHLIGHT FOREGROUND-COLOR Brown HIGHLIGHT
        CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS
               GO TO DEBT-ADD-EXIT.
             INITIALIZE W200-MEM-FLDS.
      MOVE 1   TO WS-S1.

       DEBT-ADD-LOOP.
      MOVE DME-DATA  TO W200-MEM(WS-S1).
    IF WS-S1 > 11
        GO TO DEBT-DISP-ADD.

       DEBT-ADD-READ.
      PERFORM READ-DEBMEM-NEXT THRU READ-DEBMEM-EXIT.
           IF WS-F-ERROR = ZERO
               IF DME-AC = W80-ACNO
     ADD 1  TO WS-S1
                   GO TO DEBT-ADD-LOOP.
    IF WS-S1 = ZERO
        GO TO DEBT-ADD-EXIT.
      MOVE 99   TO WS-S1.

       DEBT-DISP-ADD.
      PERFORM SAVE-SCREEN-3.
      MOVE 7   TO SHADE-ROW.
      MOVE 19   TO SHADE-COL.
      MOVE 60   TO SHADE-WIDTH.
      MOVE 15   TO SHADE-LINES.
      DISPLAY S-MEM.
      PERFORM SCREEN-SHADOW.
      DISPLAY S-DEB-MEM.
             DISPLAY SA-MEM.
            DISPLAY "ENTER to continue or ESCape" AT 5012
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
        PERFORM RESTORE-SCREEN-3
        END-EVALUATE
        GO TO DEBT-ADD-EXIT.
      PERFORM RESTORE-SCREEN-3.
    IF NOT (WS-S1 = 99)
        MOVE 0   TO WS-S1
        GO TO DEBT-ADD-READ.

       DEBT-ADD-EXIT.
             EXIT.

       DEBT-COLLECTIONS.
          IF W75-S1 = ZERO
              GO TO DEBT-COL-EXIT.
            MOVE W75-DNO(W75-S1)
     TO DMC-AC W80-ACNO.
      PERFORM CA155-GET-DEBTOR.
    IF WS-ERROR NOT = ZERO
        GO TO DEBT-COL-RESET.
      UNLOCK DEBTOR.
            MOVE DEB-NAME  TO W40-NAME.
            MOVE DEB-ADDRESS  TO W60-ADDRESS.
    IF W80-TYPE > 1
        MOVE DEB-B-TEL  TO W40-TEL
    ELSE
        MOVE DEB-H-TEL  TO W40-TEL.
             INSPECT W60-ADDRESS REPLACING ALL "*" BY ".".
           IF W80-TYPE > 1
              GO TO DEBT-COL-DUE.
            INSPECT W40-NAME REPLACING ALL "*" BY " ".
            INSPECT W40-NAME REPLACING FIRST "!" BY " ".

       DEBT-COL-DUE.
    IF DEB-TERMS = 0 OR 3
              COMPUTE W40-D-MTD = DEB-30 + DEB-60 + DEB-90 + DEB-120
     + DEB-INT
    ELSE
    IF DEB-TERMS = 1
              COMPUTE W40-D-MTD = DEB-60 + DEB-90 + DEB-120 + DEB-INT
    ELSE
    IF DEB-TERMS = 2
              COMPUTE W40-D-MTD = DEB-90 + DEB-120 + DEB-INT.
            COMPUTE W40-A-MTD = DEB-BAL - (DEB-CUR + DEB-30 +
           DEB-60 + DEB-90 + DEB-120 + DEB-INT).
      MOVE DEB-CUR  TO W40-A-CUR.
      MOVE DEB-30  TO W40-A-30.
      MOVE DEB-60  TO W40-A-60.
      MOVE DEB-90  TO W40-A-90.
      MOVE DEB-120  TO W40-A-120.
      MOVE DEB-INT  TO W40-A-INT.
      PERFORM SAVE-SCREEN-3.
      MOVE 7   TO SHADE-ROW.
      MOVE 19   TO SHADE-COL.
      MOVE 60   TO SHADE-WIDTH.
      MOVE 15   TO SHADE-LINES.
      DISPLAY S-COL.
      PERFORM SCREEN-SHADOW.
             INITIALIZE W200-MEM-FLDS.
      MOVE W80-ACNO  TO DMC-AC.
      MOVE 1   TO DMC-SEQ.
      PERFORM READ-DEBCOL-LOCK THRU READ-DEBCOL-EXIT.
           IF WS-F-ERROR NOT = ZERO
        GO TO DEBT-COL-ACCEPT.

       DEBT-COL-DELETE.
      MOVE DMC-SEQ  TO WS-S1.
      MOVE DMC-DATA  TO W200-MEM(WS-S1).
      PERFORM DELETE-DEBCOL-REC THRU WRITE-DEBCOL-EXIT.
      PERFORM READ-DEBCOL-NEXT-LOCK THRU READ-DEBCOL-EXIT.
           IF WS-F-ERROR = ZERO
        IF DMC-AC = W80-ACNO
     GO TO DEBT-COL-DELETE.

       DEBT-COL-ACCEPT.
      ACCEPT SA-COL.
            DISPLAY S99.
            ACCEPT WS-OPTION AT 5060
      WITH BACKGROUND-COLOR Magenta HIGHLIGHT FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
            DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
           IF WS-OPTION = "N"
        GO TO DEBT-COL-ACCEPT.
             MOVE 1              TO WS-S1.
      INITIALIZE DMC-RECORD1.
      MOVE W80-ACNO  TO DMC-AC.
      MOVE 1   TO DMC-SEQ.

       DEBT-COL-UPDATE.
    IF W200-MEM(WS-S1) NOT = SPACES
        MOVE W200-MEM(WS-S1)
     TO DMC-DATA
        PERFORM WRITE-DEBCOL THRU WRITE-DEBCOL-EXIT
        ADD 1   TO DMC-SEQ.
           IF WS-S1 < 12
               ADD 1             TO WS-S1
        GO TO DEBT-COL-UPDATE.

       DEBT-COL-RESET.
      MOVE 18   TO W75-S.

       DEBT-COL-FIND-LAST.
    IF W75-DNO(W75-S) = SPACES
        SUBTRACT 1  FROM W75-S
        GO TO DEBT-COL-FIND-LAST.
            MOVE W75-DNO(W75-S)
           TO W80-ACNO.
            PERFORM CA155-GET-DEBTOR.
    IF WS-ETYPE = "A"
        GO TO DEBT-COL-FIND-EXIT.
      PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.

       DEBT-COL-READ-LOOP.
             PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
    IF NOT (W80-ACNO = DEB-ACNO)
        GO TO DEBT-COL-READ-LOOP.

       DEBT-COL-FIND-EXIT.
            PERFORM RESTORE-SCREEN-3.

       DEBT-COL-EXIT.
      EXIT.

       DEBT-CRT-EDGE.
            DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR Black HIGHLIGHT
          FOREGROUND-COLOR Cyan HIGHLIGHT
       "      ³                                        ³
      -       "         ³          ³ ³     "
        WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       "³" WITH BACKGROUND-COLOR Black HIGHLIGHT
         FOREGROUND-COLOR Cyan HIGHLIGHT.
            ADD 1   TO CLIN.


       DEBTOR-LOOKUP-EXIT.
             EXIT.

       COPY "APAC.HLP".

      /
      *  ****  E N Q U I R Y   ( C O L L E C T I O N S )
      *
       BA000         SECTION 5.
       BA000-ENQUIRY.
             MOVE "S"                TO CRT-TYPE.
             CALL "UTP\CRTHEAD" USING CRT-HEADINGS LS-USER-ID.
             PERFORM DEBTOR-LOOKUP.

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
      /
      *    ****    D E B T O R   E N Q U I R Y
      *
       CD000   SECTION 5.
       CD00.
          IF W75-S1 = ZERO
              GO TO CD999.
            PERFORM SAVE-SCREEN.
             MOVE 4              TO W44-FUNCTION.
             PERFORM SCREEN-CONTENTS.
            PERFORM ERASE-SCREEN.
            DISPLAY S08.
            MOVE W75-DNO(W75-S1)
           TO W80-ACNO.

       CD02.
             DISPLAY S09.

       CD03.
             PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR = 2
              MOVE WS-ERR1  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO CD60.
           IF WS-ERROR = 1
              MOVE WS-ERR2  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO CD60.
             UNLOCK DEBTOR.
             MOVE DEB-NAME       TO W60-NAME.
            MOVE DEB-ADDRESS  TO W60-ADDRESS.
    IF W80-TYPE > 1
        MOVE DEB-B-TEL  TO W40-TEL
    ELSE
        MOVE DEB-H-TEL  TO W40-TEL.
             INSPECT W60-ADDRESS REPLACING ALL "*" BY ".".
           IF W80-TYPE > 1
               GO TO CD010.
             INSPECT W60-NAME REPLACING ALL "*" BY " ".
             INSPECT W60-NAME REPLACING FIRST "!" BY " ".

       CD010.
      ADD 121 DEB-CAT  GIVING W90-RESULT.
      DIVIDE W90-RESULT BY 2
                                 GIVING W90-CATKEY
     REMAINDER W90-CS.
      ADD 1   TO W90-CS.
            MOVE W90-CATKEY  TO WS-PARKEY.
            PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-CDESC(W90-CS)
     TO W90-CDESC.
    IF DEB-CAT = 80
        MOVE "Account Suspended"
     TO W90-CDESC.
    IF W90-CDESC = SPACES
        MOVE "** I N V A L I D   C O D E **"
     TO W90-CDESC.
    IF DEB-SMAN >  ZERO AND < 51
              ADD 100 DEB-SMAN  GIVING WS-PARKEY
              PERFORM READ-PARAM THRU READ-PARAM-EXIT
        MOVE PAR-SALESMAN TO W90-SMAN
    ELSE
        MOVE SPACES  TO W90-SMAN.
            UNSTRING DEB-PADDR DELIMITED BY "*"
                                 INTO W40-PADD1
                                      W40-PADD2
                                      W40-PADD3
                                      W40-PADD4
             OVERFLOW
                 MOVE 1          TO WS-ERROR.
      MOVE DEB-PPOST  TO W40-PPOST.
    IF DEB-TERMS = "1"
        MOVE "30 Days"  TO W90-TERMS
    ELSE
    IF DEB-TERMS = "2"
        MOVE "60 Days"  TO W90-TERMS
    ELSE
    IF DEB-TERMS = "3"
        MOVE "C.O.D."  TO W90-TERMS
    ELSE
        MOVE "7 Days"  TO W90-TERMS.
    IF DEB-INV-TYPE = "I"
        MOVE "Inclusive"  TO W90-TYPE
    ELSE
    IF DEB-INV-TYPE = "E"
        MOVE "Exclusive"  TO W90-TYPE
    ELSE
        MOVE "Export"  TO W90-TYPE.
    IF DEB-PRICE = "R"
        MOVE "Retail"  TO W90-PRICE
    ELSE
    IF DEB-PRICE = "C"
        MOVE "Cash"  TO W90-PRICE
    ELSE
        MOVE "Wholesale"  TO W90-PRICE.
            DISPLAY W60-NAME AT 0327
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.
            DISPLAY DEB-LANG AT 0378
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.
            DISPLAY W60-ADD AT 0414
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.
            DISPLAY W40-TEL AT 0514
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.
            DISPLAY DEB-CONTACT AT 0544
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.
             MOVE DEB-POST       TO W95-POST.
            DISPLAY W95-POST AT 0476
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.
             COMPUTE W90-CUR = DEB-BAL - (DEB-30 + DEB-60 +
                               DEB-90 + DEB-120 + DEB-INT).
           IF W90-CUR < ZERO
               MOVE ZERO         TO W90-CUR.
             COMPUTE W90-90 = DEB-BAL - (W90-CUR + DEB-INT).
           IF W90-90 < ZERO
               MOVE ZERO         TO W90-90.
             MOVE DEB-FAX        TO W40-FAX.
            DISPLAY W40-FAX AT 0614
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.
             MOVE DEB-PURYTD     TO W95-B.
            DISPLAY W95-V2 AT 0650
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.
             MOVE DEB-PURPRV     TO W95-B.
            DISPLAY W95-V2 AT 0669
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.
             MOVE ZERO           TO WS-ENQEND WS-ENQPOS.

       CD12.
             MOVE DEB-STM        TO W95-B.
            DISPLAY W95-V2 AT 0769
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.
             MOVE DEB-DEBT       TO W95-B.
            DISPLAY W95-V2 AT 0869
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.
             MOVE DEB-CRED       TO W95-B.
            DISPLAY W95-V2 AT 0969
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.
             MOVE DEB-BAL        TO W95-B.
            DISPLAY W95-V2 AT 1069
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.
            MOVE DEB-ACTIVE  TO W22-DTE1.
      MOVE W22-CC1  TO W22-CC2.
      MOVE W22-YY1  TO W22-YY2.
      MOVE W22-MM1  TO W22-MM2.
      MOVE W22-DD1  TO W22-DD2.
      MOVE W22-DTE2  TO W95-DTE.
            DISPLAY W95-D AT 1269
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.
            MOVE DEB-PMT  TO W22-DTE1.
      MOVE W22-CC1  TO W22-CC2.
      MOVE W22-YY1  TO W22-YY2.
      MOVE W22-MM1  TO W22-MM2.
      MOVE W22-DD1  TO W22-DD2.
      MOVE W22-DTE2  TO W95-DTE.
            DISPLAY W95-D AT 1369
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.
             MOVE W90-CUR        TO W95-B.
            DISPLAY W95-V2 AT 1569
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.
             MOVE DEB-30         TO W95-B.
            DISPLAY W95-V2 AT 1669
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.
             MOVE DEB-60         TO W95-B.
            DISPLAY W95-V2 AT 1769
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.
             MOVE DEB-90         TO W95-B.
            DISPLAY W95-V2 AT 1869
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.
             MOVE DEB-120        TO W95-B.
            DISPLAY W95-V2 AT 1969
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.
             MOVE DEB-INT        TO W95-B.
            DISPLAY W95-V2 AT 2069
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.
             MOVE W90-90         TO W95-B.
            DISPLAY W95-V2 AT 2169
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.
             MOVE DEB-CRL        TO W95-B.
            DISPLAY W95-V2 AT 2269
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.

       CD14.
             MOVE DEB-ACNO       TO TRA-KEY.
             PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
    IF WS-F-ERROR = 5
        GO TO CD21.
             MOVE SPACES         TO W40-SPACES.

       CD015.
             MOVE 8              TO CLIN.
             INITIALIZE W15-ALLOCATIONS.
             MOVE ZERO           TO WS-S9.

       CD020.
             PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
           IF (WS-F-ERROR = 5) OR
              (TRA-AC NOT = DEB-ACNO)
               MOVE WS-ENQPOS    TO WS-ENQEND
               GO TO CD21.
             ADD 1               TO WS-ENQPOS.

       CD20A.
             ADD 1               TO WS-S9.
             MOVE TRA-CODE       TO W90-CODE.
             PERFORM GET-DEBTOR-TRAN-DESC.
            MOVE TRA-DATE  TO W22-DTE1.
      MOVE W22-CC1  TO W22-CC2.
      MOVE W22-YY1  TO W22-YY2.
      MOVE W22-MM1  TO W22-MM2.
      MOVE W22-DD1  TO W22-DD2.
      MOVE W22-DTE2  TO W90-DTE.
             MOVE 02             TO CCOL.
            DISPLAY W90-DTE AT CPOS
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.
      MOVE 13   TO CCOL.
            DISPLAY W00-E-DESC AT CPOS
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.
      MOVE 26   TO CCOL.
            DISPLAY TRA-REF AT CPOS
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.
             MOVE TRA-VALUE      TO W95-B.
           IF TRA-VALUE > ZERO
               MULTIPLY TRA-ALLOCATED BY -1
     GIVING W15-ALLOC(WS-S9)
               SUBTRACT TRA-ALLOCATED FROM TRA-VALUE
     GIVING W15-VALUE(WS-S9)
        MOVE TRA-REC  TO W15-REC(WS-S9)
           ELSE
               MOVE TRA-ALLOCATED
     TO W15-ALLOC(WS-S9)
               ADD TRA-VALUE TRA-PAID
     GIVING W15-VALUE(WS-S9)
        MOVE SPACES  TO W15-REC(WS-S9).
      MOVE 35   TO CCOL.
            DISPLAY W95-V2 AT CPOS
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT HIGHLIGHT
     BACKGROUND-COLOR Magenta HIGHLIGHT.
           IF CLIN < 22
               ADD 1             TO CLIN
               GO TO CD020.
             COMMIT.

       CD21.
            DISPLAY "PgDn" AT 5002
        WITH BACKGROUND-COLOR Cyan HIGHLIGHT FOREGROUND-COLOR Brown HIGHLIGHT
       ", " WITH BACKGROUND-COLOR Cyan HIGHLIGHT FOREGROUND-COLOR Blue HIGHLIGHT
       "PgUp" WITH BACKGROUND-COLOR Cyan HIGHLIGHT FOREGROUND-COLOR Brown HIGHLIGHT
       ", " WITH BACKGROUND-COLOR Cyan HIGHLIGHT FOREGROUND-COLOR Blue HIGHLIGHT
       "Esc" WITH BACKGROUND-COLOR Cyan HIGHLIGHT FOREGROUND-COLOR Brown HIGHLIGHT
       "ape, " WITH BACKGROUND-COLOR Cyan HIGHLIGHT FOREGROUND-COLOR Blue HIGHLIGHT
       "F6" WITH BACKGROUND-COLOR Cyan HIGHLIGHT FOREGROUND-COLOR Brown HIGHLIGHT
       " purchases, "
        WITH BACKGROUND-COLOR Cyan HIGHLIGHT FOREGROUND-COLOR Blue HIGHLIGHT
       "F7" WITH BACKGROUND-COLOR Cyan HIGHLIGHT FOREGROUND-COLOR Brown HIGHLIGHT
       " allocations, "
        WITH BACKGROUND-COLOR Cyan HIGHLIGHT FOREGROUND-COLOR Blue HIGHLIGHT
       "F8" WITH BACKGROUND-COLOR Cyan HIGHLIGHT FOREGROUND-COLOR Brown HIGHLIGHT
       " screen 2, "
       "F9" WITH BACKGROUND-COLOR Cyan HIGHLIGHT FOREGROUND-COLOR Brown HIGHLIGHT
       " Memo/Remarks"
        WITH BACKGROUND-COLOR Cyan HIGHLIGHT FOREGROUND-COLOR Blue HIGHLIGHT.

       CD022.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO CD30
   WHEN F6-KEY  GO TO CD40
   WHEN F7-KEY
       IF OPEN-ITEM
    GO TO CD35
       ELSE
    GO TO CD022
       END-IF
   WHEN F8-KEY  GO TO CD50
   WHEN F9-KEY  GO TO CD22
   WHEN PAGE-UP
       IF WS-ENQPOS = 0 OR 1 OR 15
    GO TO CD21
       ELSE
    GO TO CD025
       END-IF
   WHEN PAGE-DOWN  GO TO CD023
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO CD022.
      CALL X"E5".
      GO TO CD022.

       CD22.
            PERFORM CLEAR-L50.
             MOVE W80-ACNO       TO DME-AC.
             MOVE 1              TO DME-SEQ.
             PERFORM READ-DEBMEM THRU READ-DEBMEM-EXIT.
           IF WS-F-ERROR NOT = ZERO
               GO TO CD21.
             INITIALIZE W200-MEM-FLDS.

       CD23.
             MOVE DME-SEQ        TO WS-S1.
      MOVE DME-DATA  TO W200-MEM(WS-S1).
             PERFORM READ-DEBMEM-NEXT THRU READ-DEBMEM-EXIT.
           IF WS-F-ERROR = ZERO
               IF DME-AC = W80-ACNO
     GO TO CD23.
      PERFORM SAVE-SCREEN-3.
      MOVE 7   TO SHADE-ROW.
      MOVE 19   TO SHADE-COL.
      MOVE 60   TO SHADE-WIDTH.
      MOVE 15   TO SHADE-LINES.
      DISPLAY S-MEM.
      PERFORM SCREEN-SHADOW.
             DISPLAY SA-MEM.
            PERFORM CLEAR-L50.
            DISPLAY "ENTER to continue" AT 5024
        WITH BACKGROUND-COLOR Cyan HIGHLIGHT FOREGROUND-COLOR Brown HIGHLIGHT.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM RESTORE-SCREEN-3.

       CD23-E.
             GO TO CD21.

       CD023.
           IF WS-ENQPOS = WS-ENQEND
               GO TO CD022.
             MOVE 08             TO CLIN.
             MOVE 01             TO CCOL.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
             PERFORM CD02.
             PERFORM CD12.
             GO TO CD015.

       CD025.
             MOVE 1              TO WS-IXS1.

       CD25.
             PERFORM READ-DBTRAN-PREV THRU READ-DBTRAN-EXIT.
           IF WS-F-ERROR = 5
               GO TO CD26.
    IF TRA-AC NOT = DEB-ACNO
               PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT
               GO TO CD26.
      SUBTRACT 1   FROM WS-ENQPOS.
             ADD 1               TO WS-IXS1.
           IF WS-IXS1 < 30
               IF WS-ENQPOS > 0
                   GO TO CD25.

       CD26.
             MOVE 08             TO CLIN.
             MOVE 01             TO CCOL.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
             PERFORM CD02.
             PERFORM CD12.
             PERFORM CD015.
             ADD 1               TO WS-ENQPOS.
             GO TO CD20A.

       CD30.
             COMMIT.
            GO TO CD60.

       CD35.
      PERFORM SAVE-SCREEN-3.
      MOVE 8   TO SHADE-ROW.
      MOVE 47   TO SHADE-COL.
      MOVE 32   TO SHADE-WIDTH.
      MOVE 15   TO SHADE-LINES.
      DISPLAY S09A.
      PERFORM SCREEN-SHADOW.
            PERFORM CLEAR-L50.
            DISPLAY "ENTER to continue" AT 5024
        WITH BACKGROUND-COLOR Cyan HIGHLIGHT FOREGROUND-COLOR Brown HIGHLIGHT.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM RESTORE-SCREEN-3.

       CD35-E.
             GO TO CD21.

       CD40.
      MOVE "Ú"   TO W40-T1.
      MOVE "¿"   TO W40-T28.
      MOVE ALL "Ä"  TO W40-T2-27
                                    W40-B2-27.
      MOVE "³"   TO W40-M1 W40-M28
             MOVE SPACES         TO W40-M2-27.
      MOVE "À"   TO W40-B1.
      MOVE "Ù"   TO W40-B28.
      PERFORM SAVE-SCREEN-3.
      MOVE 8   TO SHADE-ROW.
      MOVE 53   TO SHADE-COL.
      MOVE 26   TO SHADE-WIDTH.
      MOVE 14   TO SHADE-LINES.
      DISPLAY S09B.
      PERFORM SCREEN-SHADOW.
            PERFORM CLEAR-L50.
            DISPLAY "ENTER to continue" AT 5024
        WITH BACKGROUND-COLOR Cyan HIGHLIGHT FOREGROUND-COLOR Brown HIGHLIGHT.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM RESTORE-SCREEN-3.

       CD45-E.
             GO TO CD21.

       CD50.
      PERFORM SAVE-SCREEN-3.
      MOVE 9   TO SHADE-ROW.
      MOVE 7   TO SHADE-COL.
      MOVE 50   TO SHADE-WIDTH.
      MOVE 11   TO SHADE-LINES.
      DISPLAY S10.
      PERFORM SCREEN-SHADOW.
            PERFORM CLEAR-L50.
            DISPLAY "ENTER to continue" AT 5024
        WITH BACKGROUND-COLOR Cyan HIGHLIGHT FOREGROUND-COLOR Brown HIGHLIGHT.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM RESTORE-SCREEN-3.

       CD50-E.
             GO TO CD21.

       CD60.
      MOVE 18   TO W75-S.

       CD65.
    IF W75-DNO(W75-S) = SPACES
        SUBTRACT 1  FROM W75-S
        GO TO CD65.
            MOVE W75-DNO(W75-S)
           TO W80-ACNO.
            PERFORM CA155-GET-DEBTOR.
    IF WS-ETYPE = "A"
        GO TO CD75.
      PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.

       CD70.
             PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
    IF NOT (W80-ACNO = DEB-ACNO)
        GO TO CD70.

       CD75.
            PERFORM RESTORE-SCREEN.

       CD999.
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

             COPY ""FUNCTION.PRO".

       ZA00A-CONTINUE.
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
    IF TRM-STDP(WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
           ELSE
    IF TRM-STDP(WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
           ELSE
    IF TRM-STDP(WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
           ELSE
    IF TRM-STDP(WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
    IF TRM-STDP(WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
    IF TRM-STDP(WS-S6) = 9
        MOVE LS-USER  TO W02-USER.
      ADD 501 TRM-PRN3(WS-S6)
                                 GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S6.
             ADD 1               TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE TRM-CONP(WS-S6)
                                 TO WS-COND.
      MOVE TRM-NORP(WS-S6)
                                 TO WS-NORM.
      MOVE TRM-EXPP(WS-S6)
                                 TO WS-EXPP.
      MOVE TRM-ECAN(WS-S6)
                                 TO WS-ECAN.
      MOVE TRM-8LPI(WS-S6)
                                 TO WS-8LPI.
      MOVE TRM-6LPI(WS-S6)
                                 TO WS-6LPI.
      MOVE TRM-10CPI(WS-S6)
                                 TO WS-10CPI.
      MOVE TRM-12CPI(WS-S6)
                                 TO WS-12CPI.
      MOVE TRM-17CPI(WS-S6)
                                 TO WS-17CPI.
      MOVE TRM-LENGTH(WS-S6)
     TO WS-PRN-LENGTH.
      MOVE TRM-PAGE(WS-S6)
     TO WS-PGE-LENGTH.
       ZA02.
            OPEN I-O DEBCOL.
           IF WS-STATUS = "05"
        CLOSE DEBCOL
        OPEN OUTPUT DEBCOL
        CLOSE DEBCOL
        OPEN I-O DEBCOL.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
     MOVE 49  TO WS-F-ERROR
                   GO TO ZA50.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY    TO W95-COMP.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
      *       DISPLAY "Waiting for printer response" AT 1412
      *         WITH FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
      *       OPEN OUTPUT PRNREP.
            GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
            GO TO ZA205.
       ZA50.
          IF WS-F-ERROR = 49
              MOVE "DEBTOR COLL"
     TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Press any key to continue" AT 1012.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
       ZA51.
             EXIT PROGRAM.

       ZA55.
             MOVE 1              TO WS-S1.
            MOVE SPACES  TO WS-MID-LNE.
       ZA60.
            MOVE WS-G1   TO WS-TCHR(WS-S1) WS-BCHR(WS-S1).
      MOVE WS-G8   TO WS-TCH(WS-S1) WS-BCH(WS-S1).
           IF WS-S1 < 80
               ADD 1             TO WS-S1
               GO TO ZA60.
      MOVE WS-G9   TO WS-TCH(1).
      MOVE WS-G10  TO WS-TCH(80).
      MOVE WS-G11  TO WS-BCH(1).
      MOVE WS-G12  TO WS-BCH(80).
      MOVE WS-G14  TO WS-TCHR(1) WS-BCHR(1).
      MOVE WS-G13  TO WS-TCHR(80) WS-BCHR(80).
            MOVE WS-G2   TO WS-TCHR(8) WS-TCHR(49)
              WS-TCHR(61) WS-TCHR(72).
            MOVE WS-G3   TO WS-MCHR(8) WS-MCHR(49)
              WS-MCHR(61) WS-MCHR(72)
              WS-MCHR(1) WS-MCHR(80).
            MOVE WS-G4   TO WS-BCHR(8) WS-BCHR(49)
              WS-BCHR(61) WS-BCHR(72).
      MOVE WS-G13  TO WS-TCHR(80) WS-BCHR(80).
      MOVE WS-G14  TO WS-TCHR(1) WS-BCHR(1).
      MOVE LS-COMPANY  TO WS-TOP-COMP.
    IF LS-USER = LS-SYS-ID
        MOVE "SupervisorÄ"  TO WS-WRKHD
    ELSE
        MOVE "Workstation"  TO WS-WRKHD
        MOVE LS-USER    TO WS-WRKST.
       ZA200.
             DISPLAY "Files locked - Try later" AT 2312
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     " " WS-STATUS WITH FOREGROUND-COLOR Grey HIGHLIGHT.
            DISPLAY "Press any key" AT 5012
              WITH BACKGROUND-COLOR Cyan HIGHLIGHT FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT WS-OPTION AT 5026
      WITH BACKGROUND-COLOR Cyan HIGHLIGHT FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
       ZA205.
             EXIT PROGRAM.
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
