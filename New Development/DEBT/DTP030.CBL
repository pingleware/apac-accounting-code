      $SET LINKCOUNT"512" GNT"DTP030.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****     ******     ****     *
      *   **    **     **     **    **   **  **   **    **   **  **    *
      *   **    **     **     **    **  **    **        **  **    **   *
      *   **    **     **     *******   **    **     ****   **    **   *
      *   **    **     **     **        **    **        **  **    **   *
      *   **    **     **     **         **  **   **    **   **  **    *
      *   *******      **     **          ****     ******     ****     *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R   D E T A I L E D   A C C O U N T S   L I S T  *
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
      *  Jan 2008 - New file (DEBALT)- Debtor alternate index included *
      *             for lookups, using any word contained in the       *
      *             Debtor name.                                       *
      *                                                                *
      *  October 2008 - Amended Accounts receivable ageing to create   *
      *                 duplicate files of the accounts and trans-     *
      *                 actions for printing of copy satements when    *
      *                 and if required. The file names are AR_Ayymm   *
      *                 and AR_Tyymm, where yymm is replaced with      *
      *                 the year and month.                            *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     DTP030.
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

       COPY "PARAM.SL".

       COPY "DBHIST.SL".

       COPY "DEBTOR.SL".

       COPY "DBTRAN.SL".

       COPY "DTHIST.SL".

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

       COPY "PARAM.FDE".

       COPY "DBHIST.FDE".

       COPY "DEBTOR.FDE".

       COPY "DBTRAN.FDE".

       COPY "DTHIST.FDE".

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
       77  WS-NETKEY    PIC 9(06)   COMP-5.
       77  WS-WRKEY    PIC 9(04)   COMP-5.
       77  WS-NTP    PIC 9(01).
       77  WS-LINES    PIC 9(02).
       77  WS-PAGE    PIC 9(04)   COMP-5.
       77  WS-OPTION    PIC X(01).
       77  WS-AGE    PIC X(01).
       77  WS-SEQ    PIC X(01).
       77  WS-CREDIT    PIC X(01).
       77  WS-ZERO    PIC X(01).
       77  WS-NUM    PIC Z9.
       77  WS-ADDR    PIC X(01).
       77  WS-REF    PIC X(01).
       77  WS-ERROR    PIC 9(01).
       77  WS-ERROR1    PIC 9(01).
       77  WS-EXPAND    PIC X(01)  VALUE " ".
       77  WS-COMP    PIC 9(01).
       77  WS-BDT    PIC 9(01).
       77  WS-BAD    PIC X(01).
       77  WS-STM    PIC 9(01).
       77  WS-TYPE    PIC X(01).
       77  WS-PERIOD    PIC X(01).
       88  PREVIOUS-PERIOD   VALUE "P".
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
       77  WS-AC    PIC X(07) VALUE "Account".
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
    03  WS-MODULE       PIC  X(03) VALUE "STM".
    03  WS-PROG        PIC  X(03) VALUE "F06".

       01  W00-TRAN-CODES.
           03  W00-TRAN-TYPES  OCCURS 100.
               05  W00-TRAN-CODE PIC 9(02)   COMP-5.
               05  W00-ACTION  PIC 9(02)     COMP-5.
               05  W00-E-DESC  PIC X(12).
               05  W00-A-DESC  PIC X(12).

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-PRINTER-DETAILS.
           03  W02-PRINTER         PIC  X(12).
           03  W02-PGE-LENGTH      PIC  9(03).
           03  W02-PRN-LENGTH      PIC  9(03).
           03  W02-LINAGE          PIC  9(03).
           03  W02-PRN-STATUS      PIC  X(01)    VALUE "C".
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
           03  W02-PRN-TYPE        PIC  X(01).
           03  W02-PRN-LINE        PIC  X(136).

           03  REP-LINE1                          REDEFINES W02-PRN-LINE.
               05  REP-DETAIL1     PIC  X(136).

           03  TRL-LINE1                          REDEFINES W02-PRN-LINE.
               05  TRL-DET         PIC  X(136).

           03  TRL-LINE2                          REDEFINES W02-PRN-LINE.
               05  TRL-HEAD1       PIC  X(07).
               05  TRL-HEAD2       PIC  X(34).
               05  TRL-HEAD3       PIC  X(08).
               05  TRL-HEAD4       PIC  X(07).
               05  TRL-HEAD5       PIC  X(14).
               05  TRL-HEAD6       PIC  X(10).
               05  TRL-HEAD7       PIC  X(14).
               05  TRL-HEAD8       PIC  X(14).
               05  TRL-HEAD9       PIC  X(11).
               05  TRL-HEAD10      PIC  X(09).
               05  TRL-HEAD11      PIC  X(08).

           03  TRL-LINE3                          REDEFINES W02-PRN-LINE.
               05  TRL-AC          PIC  X(06).
               05  FILLER          PIC  X(01).
               05  TRL-NME         PIC  X(31).
               05  FILLER          PIC  X(01).
               05  TRL-ACT         PIC  Z9/99/9999.
               05  FILLER          PIC  X(01).
               05  TRL-MTH         PIC  9(01).
               05  FILLER          PIC  X(02).
               05  TRL-PD          PIC  Z(07)9.99-.
               05  FILLER          PIC  X(01).
               05  TRL-BAL         PIC  Z(07)9.99-.
               05  FILLER          PIC  X(02).
               05  TRL-STM         PIC  Z(07)9.99-.
               05  FILLER          PIC  X(02).
               05  TRL-LST-PAY     PIC  Z(07)9.99-.
               05  FILLER          PIC  X(01).
               05  TRL-PAY         PIC  Z9/99/9999.
               05  FILLER          PIC  X(02).
               05  TRL-INT         PIC  Z(04)9.99.
               05  FILLER          PIC  X(01).
               05  TRL-LIMIT       PIC  Z(06)9.
               05  TRL-EXL         PIC  X(01).

           03  TRL-LINE4                          REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(10).
               05  TRL-HEAD12      PIC  X(07).
               05  TRL-MTD         PIC  Z(07)9.99-.
               05  FILLER          PIC  X(01).
               05  TRL-HEAD13      PIC  X(09).
               05  TRL-CUR         PIC  Z(07)9.99-.
               05  FILLER          PIC  X(01).
               05  TRL-HEAD14      PIC  X(09).
               05  TRL-30          PIC  Z(07)9.99.
               05  FILLER          PIC  X(01).
               05  TRL-HEAD15      PIC  X(09).
               05  TRL-60          PIC  Z(07)9.99.
               05  FILLER          PIC  X(01).
               05  TRL-HEAD16      PIC  X(09).
               05  TRL-90          PIC  Z(07)9.99.
               05  FILLER          PIC  X(01).
               05  TRL-HEAD17      PIC  X(10).
               05  TRL-120         PIC  Z(07)9.99.

           03  TRL-LINE5                          REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(28).
               05  TRL-HEADER      PIC  X(24).
               05  FILLER          PIC  X(28).

           03  TRL-LINE6                          REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(10).
               05  TRL-SUM         PIC  X(20).
               05  TRL-VAL         PIC  Z(06)9.99-.
               05  FILLER          PIC  X(39).

           03  TRL-LINE7                          REDEFINES W02-PRN-LINE.
               05  TRL-PAGE        PIC  X(06).
               05  TRL-P-NO        PIC  Z(03)9.
               05  FILLER          PIC  X(24).
               05  TRL-RESTART.
                   07  TRL-COMPANY PIC  X(40).
                   07  TRL-HEAD    PIC  X(24).
               05  FILLER          PIC  X(17).
               05  TRL-DATE        PIC  X(06).
               05  TRL-DTE         PIC  Z9/99/9999.

           03  TRL-LINE7A                         REDEFINES W02-PRN-LINE.
               05  TRL-EXPP        PIC  X(06).
               05  FILLER          PIC  X(06).
               05  TRL-CO          PIC  X(30).
               05  TRL-EXHEAD      PIC  X(24).
               05  TRL-EXPC        PIC  X(06).

           03  TRL-LINE8                          REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(07).
               05  TRL-PCDE        PIC  9(04).
               05  FILLER          PIC  X(02).
               05  TRL-HEAD18      PIC  X(10).
               05  TRL-HTEL        PIC  X(14).
               05  FILLER          PIC  X(02).
               05  TRL-BTEL        PIC  X(14).
               05  FILLER          PIC  X(79).

           03  ST7-LINE1                          REDEFINES W02-PRN-LINE.
               05  ST7-DET         PIC  X(96).
           03  ST7-LINE2                          REDEFINES W02-PRN-LINE.
               05  ST7-COMP.
                   07  ST7-EXCOMP  PIC  X(36).
                   07  FILLER      PIC  X(36).
               05  FILLER          PIC  X(24).
           03  ST7-LINE2A                         REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(15).
               05  ST7-HEADER      PIC  X(17).
               05  FILLER          PIC  X(64).

           03  ST7-LINE3                          REDEFINES W02-PRN-LINE.
               05  ST7-ADD         PIC  X(30).
               05  FILLER          PIC  X(12).
               05  ST7-TEL         PIC  X(19).
               05  ST7-TNO         PIC  X(14).
               05  FILLER          PIC  X(21).

           03  ST7-LINE4                          REDEFINES W02-PRN-LINE.
               05  ST7-PC1         PIC  9(04)     BLANK WHEN ZERO.
               05  ST7-PC2         PIC  9(04)     BLANK WHEN ZERO.
               05  ST7-AH1         PIC  X(12).
               05  ST7-AC1         PIC  X(06).
               05  FILLER          PIC  X(17).
               05  ST7-AH2         PIC  X(06).
               05  ST7-DATE        PIC  Z9/99/9999.
               05  FILLER          PIC  X(16).
               05  ST7-AH3         PIC  X(09).
               05  ST7-PAGE        PIC  ZZZ9.
               05  FILLER          PIC  X(08).

           03  ST7-LINE5                          REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(04).
               05  ST7-NAME        PIC  X(40).
               05  FILLER          PIC  X(08).
               05  ST7-MESS        PIC  X(42).
               05  FILLER          PIC  X(02).

           03  ST7-LINE6                          REDEFINES W02-PRN-LINE.
               05  FILLER            PIC  X(03).
               05  ST7-HD1.
                   07  FILLER        PIC  X(01).
                   07  ST7-TDTE      PIC  Z9/99/9999.
                   07  FILLER        PIC  X(04).
                   07  ST7-BFWD.
                       09  ST7-TDESC PIC  X(12).
                       09  FILLER    PIC  X(04).
                       09  ST7-TREF  PIC  X(08).
                   07  FILLER        PIC  X(04).
                   07  ST7-V1.
                       09  ST7-TDBT  PIC  Z(08)9.99CR.
                   07  ST7-PD        PIC  X(02).
                   07  ST7-V2.
                       09  ST7-TCRD  PIC  Z(08)9.99CR.
                   07  FILLER        PIC  X(02).
                   07  ST7-V3.
                       09  ST7-TBAL  PIC  Z(08)9.99CR.

           03  ST7-LINE7                          REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(03).
               05  ST7-A1          PIC  X(12).
               05  ST7-A2          PIC  X(10).
               05  ST7-A3          PIC  X(12).
               05  ST7-A4          PIC  X(12).
               05  ST7-A5          PIC  X(12).
               05  FILLER          PIC  X(35).

           03  ST7-LINE8                          REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(01).
               05  ST7-INT         PIC  Z(06)9.99-.
               05  FILLER          PIC  X(01).
               05  ST7-3MTHS       PIC  Z(06)9.99-.
               05  FILLER          PIC  X(01).
               05  ST7-2MTHS       PIC  Z(06)9.99-.
               05  FILLER          PIC  X(01).
               05  ST7-1MTH        PIC  Z(06)9.99-.
               05  FILLER          PIC  X(01).
               05  ST7-CUR         PIC  Z(06)9.99-.
               05  FILLER          PIC  X(01).
               05  ST7-DUE         PIC  Z(06)9.99-.
               05  FILLER          PIC  X(12).
               05  ST7-DUE2        PIC  Z(06)9.99-.

       01  W05-DEB-HISTORY.
    03  W05-DBT-HIS.
        05  W05-DBT  PIC  X(04) VALUE "AR_T".
        05  W05-TYY  PIC  9(02).
        05  W05-TMM  PIC  9(02).
        05  FILLER   PIC  X(04) VALUE ".DAT".
    03  W05-DEB-HIS.
        05  W05-DEB  PIC  X(04) VALUE "AR_A".
        05  W05-DEBH-ID.
     07  W05-DYY PIC  9(02).
     07  W05-DMM PIC  9(02).
        05  W05-HIST-ID REDEFINES W05-DEBH-ID PIC  9(04).
        05  FILLER   PIC  X(04) VALUE ".DAT".

       01  W05-LIST.
           03  W05-OPT         PIC 9(03).
           03  W05-TYPES REDEFINES W05-OPT.
               05  W05-COMP    PIC 9(01).
               05  W05-INDV    PIC 9(01).
               05  W05-INT     PIC 9(01).

       01  W10-SPARES.
          03  W10-DATE        PIC  X(10).
    03  W10-RDATE REDEFINES W10-DATE.
        05  W10-DTE     PIC  99/99/9999.

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

       01  W40-COMPANY.
           02  W40-NAME1.
               03  W40-NAME.
                   05  W40-ENTRY PIC X(07).
                   05  FILLER  PIC X(33).
               03  FILLER      PIC X(32).
           02  W40-NAME2 REDEFINES W40-NAME1.
               03  W40-SURNAME PIC X(24).
               03  W40-FNAME   PIC X(24).
               03  W40-SNAME   PIC X(24).
           02  W40-NAME3 REDEFINES W40-NAME1.
               03  W40-NCHAR OCCURS 72 PIC X(01).
           02  W40-ADDRESS1.
               03  W40-ADD1    PIC X(30).
               03  W40-ADD2    PIC X(30).
               03  W40-ADD3    PIC X(30).
               03  W40-ADD4    PIC X(30).
           02  W40-ADDRESS2 REDEFINES W40-ADDRESS1.
               03  W40-ACHAR OCCURS 120 PIC X(01).
           02  W40-POST        PIC 9(04).
           02  W40-TEL         PIC X(14).
           02  W40-HTEL        PIC X(14).
           02  W40-TAX REDEFINES W40-HTEL.
               03  W40-TAXNO   PIC 9(14)     COMP-3.
           02  W40-TITLE       PIC 9(04)     COMP-5.
           02  W40-LANG        PIC X(01).

       COPY "W40.WS".

       01  W45-TRAN.
           03  W45-CODE        PIC 9(02).
           03  W45-ENG         PIC X(12).
           03  W45-AFR         PIC X(12).
           03  W45-ACT         PIC X(01).

       COPY "FUNCTION.WS".

      *COPY "W50.WS".

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

       COPY "W90.DBT".

          03  W90-MTD        PIC S9(09)V99 COMP-3.
    03  W90-DEBT-TOTAL  PIC S9(11)V99 COMP-3.

       01  W95-STM.
           03  W95-COMP        PIC  X(40).

       01  W100-EDIT.
           03  W100-LBAL       PIC Z(06)9.99-.
           03  W100-LARR       PIC Z(06)9.99-.
           03  W100-BAL        PIC Z(08)9.99-.
           03  W100-CRL        PIC Z(06)9.

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

       01  S14.
          03  LINE  3 COLUMN 30 FOREGROUND-COLOR Grey HIGHLIGHT
           VALUE "DETAILED ACCOUNT LIST".
           03  LINE  5 COLUMN 12 VALUE "Printing for".

       01  S15.
          03  LINE   9 COLUMN 12 VALUE
               "Do you want the Debtor address printed (".
    03        COLUMN 52 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "Y".
    03        COLUMN 53 VALUE "/".
    03        COLUMN 54 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "N".
    03        COLUMN 55 VALUE ")".

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

       01  S80-YEAR.
    03  LINE 08 COLUMN 40 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  Z9 USING W05-DYY AUTO.
       01  S80-MONTH.
    03  LINE 09 COLUMN 40 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  Z9 USING W05-DMM AUTO.

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
             PERFORM YA000.
             PERFORM XA000.
           IF PREVIOUS-PERIOD
               CLOSE DBHIST DTHIST.

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

       COPY "AA900.ALM".
       
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
       " to do Debtor Lookup "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "Esc" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT 
       " to exit"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
       AA305.
             EXIT.

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

       COPY "OPTION.CRT".

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

      /
      *    ****    READ FILES ROUTINES
      *
       AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "DBHIST.RD".

       COPY "DEBTOR.RD1".

       COPY "DBTRAN.RD".

       COPY "DEBTOR.LUP".

       COPY "DTHIST.RD".

       COPY "APAC.HLP".

      /
       CA155-GET-DEBTOR        SECTION 2.
       CA155-ACCOUNT.
    IF PREVIOUS-PERIOD
        MOVE W80-ACNO  TO DBH-ACNO
        PERFORM READ-DBHIST THRU READ-DBHIST-EXIT
        MOVE DBH-RECORD1  TO DEB-RECORD1
    ELSE
              MOVE W80-ACNO  TO DEB-ACNO
              PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
    IF WS-F-ERROR = 6 OR 56
        MOVE 1   TO WS-ERROR
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
      *    ****    D E T A I L E D   A C C O U N T   L I S T
      *
       XA000        SECTION 5.

       XA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S14.
      MOVE ZERO   TO WS-BDT.
      MOVE "N"   TO WS-BAD.

       XA05.
             DISPLAY W95-COMP AT 0525 WITH FOREGROUND-COLOR Cyan HIGHLIGHT .
             MOVE "Are you sure Y/N" TO WS-OPT-MES.
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
          IF WS-OPTION = "N"
               GO TO XA999-EXIT.
            DISPLAY S15.
            ACCEPT WS-ADDR AT 0957
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
      CALL "CBL_TOUPPER" USING WS-ADDR
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
       XA10.
            MOVE SPACE   TO WS-CREDIT.
             MOVE "N"            TO WS-ZERO.

       XA15.
      DISPLAY "Print accounts with zero balance (" AT 1112
       "Y" WITH FOREGROUND-COLOR Brown HIGHLIGHT "/"
       "N" WITH FOREGROUND-COLOR Brown HIGHLIGHT ")".
      ACCEPT WS-ZERO AT 1151
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-ZERO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-ZERO = "N" OR "Y")
               GO TO XA15.
             MOVE 1              TO WS-SUB-2.
            MOVE 0   TO WS-PAGE W90-BAL W90-CUR
        W90-30  W90-60  W90-90
              W90-120 W90-BDT W90-BCR
        W90-INT W90-MTD W90-DEBT-TOTAL.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *   **  *   *   * *   *  *  **  *   *   *     * *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *   *  **   *     *   *  *  *  **   *   *     * *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** * *
      *
      PERFORM OPEN-PRINTER.
      MOVE "P"   TO WS-COMMAND.
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE 3   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.

       XA20.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE SPACES  TO REP-DETAIL1.
            MOVE "PAGE:"  TO TRL-PAGE.
            MOVE "DATE:"  TO TRL-DATE.
            ADD 1   TO WS-PAGE.
            MOVE W12-TODAY  TO TRL-DTE.
            MOVE WS-PAGE  TO TRL-P-NO.
      MOVE 0   TO WS-ADVANCE.
      *
      *    ****   T E S T   I F   O U T P U T T O   P R I N T E R
      *    O R S P O O L  F I L E
      *
    IF NOT (WS-PRINTER = "S")
        PERFORM CALL-PRINTUTL
        MOVE SPACES  TO TRL-LINE1
        MOVE "E"   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
            MOVE W95-COMP  TO TRL-CO.
            MOVE " - DETAILED ACCOUNTS"
     TO TRL-EXHEAD.
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
      MOVE SPACES  TO TRL-LINE1.
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE "X"   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
              MOVE 3   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
      MOVE "H"   TO W02-PRN-TYPE.
    IF WS-BDT = 1
        MOVE SPACES  TO TRL-DET
        MOVE 2   TO WS-ADVANCE
        MOVE "B A D   A N D   D O U B T F U L L   D E B T S"
     TO TRL-RESTART
        PERFORM CALL-PRINTUTL
    ELSE
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO TRL-DET.
      MOVE 2   TO WS-ADVANCE.

       XA25.
            MOVE "AC-NO."  TO TRL-HEAD1.
            MOVE "NAME"  TO TRL-HEAD2.
            MOVE "ACTIVE"  TO TRL-HEAD3.
            MOVE "MTH"   TO TRL-HEAD4.
      MOVE "PAST DUE"  TO TRL-HEAD5.
            MOVE "BALANCE"  TO TRL-HEAD6.
      MOVE "STM-BALANCE"  TO TRL-HEAD7.
      MOVE "LAST-PAYMNT"  TO TRL-HEAD8.
      MOVE "DATE-PAID"  TO TRL-HEAD9.
      MOVE "INTEREST"  TO TRL-HEAD10.
      MOVE "C-LIMIT"  TO TRL-HEAD11.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO TRL-DET.
      MOVE "D"   TO W02-PRN-TYPE.

       XA30.
    IF WS-BDT = 1
        DISPLAY "Busy with Bad & Doubtfull Debts" AT 1812
        GO TO XA45.
      *
      *    ****   T E S T   I F   O U T P U T T O   S C R E E N
      *
    IF WS-PRINTER = "S"
        MOVE SPACES  TO WS-ACNO
        GO TO XA45.
            DISPLAY "Is this a restart (" AT 1312
       "Y" WITH FOREGROUND-COLOR Brown HIGHLIGHT "/"
       "N" WITH FOREGROUND-COLOR Brown HIGHLIGHT ")".

       XA35.
            ACCEPT WS-OPTION AT 1336
            WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "N" OR "Y")
               GO TO XA35.
            MOVE SPACES  TO WS-ACNO.
           IF WS-OPTION = "N"
               GO TO XA45.

       XA40.
             MOVE 2001           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
            DISPLAY "Account No:" AT 2012.
            ACCEPT W80-ACNO AT 2024
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
           IF W80-ACNO = SPACES
               GO TO XA45.
             PERFORM CA155-GET-DEBTOR.
             MOVE 23                 TO CRT-START CRT-END.
             PERFORM ERASE-SCREEN.
           IF WS-ERROR NOT = ZERO
              MOVE "Invalid account number"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO XA40.
            MOVE DEB-NAME  TO W40-NAME.
           IF DEB-TYPE = 1
               INSPECT W40-NAME REPLACING ALL "*" BY "."
               INSPECT W40-NAME REPLACING FIRST "!" BY " ".
            DISPLAY W40-NAME AT 2030
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO XA40.
            MOVE W80-ACNO  TO WS-ACNO.
             MOVE "RESTART  RESTART  RESTART  RESTART RESTART RESTART"
           TO TRL-RESTART.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO TRL-DET.

       XA45.
             DISPLAY "Debtor account:" AT 2212.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Start reading from the first account in the file.     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
    IF PREVIOUS-PERIOD
        MOVE " !"  TO DBH-NAME
        PERFORM START-AT-DEBH-NAME THRU READ-DBHIST-EXIT
    ELSE
        MOVE " !"  TO DEB-NAME
        PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.
          IF WS-F-ERROR = 6 OR 56
              GO TO XA70.

       XA50.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              GO TO XA999-EXIT.
    IF PREVIOUS-PERIOD
              PERFORM READ-DBHIST-NEXT THRU READ-DBHIST-EXIT
        MOVE DBH-RECORD1  TO  DEB-RECORD1
    ELSE
              PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
          IF WS-F-ERROR = 6 OR 56
        GO TO XA65.
      *
      *    ****   T E S T   I F   O U T P U T T O   P R I N T E R
      *    O R S P O O L  F I L E
      *
    IF NOT (WS-PRINTER = "S")
        DISPLAY DEB-ACNO AT 2228 WITH FOREGROUND-COLOR Cyan HIGHLIGHT .
           IF WS-ACNO = DEB-ACNO
              MOVE SPACES  TO WS-ACNO.
           IF WS-ZERO = "N"
               IF DEB-BAL = ZERO
                   GO TO XA50.
    IF DEB-BDT = 1
        IF WS-BDT = 0
     MOVE "Y"  TO WS-BAD
     GO TO XA50.
    IF NOT (DEB-BDT = 1)
        IF WS-BDT = 1
     GO TO XA50.
           IF WS-ERROR1 = 1
               GO TO XA50.
            MOVE DEB-NAME  TO W40-NAME.
           IF DEB-TYPE = 1
               INSPECT W40-NAME REPLACING ALL "*" BY "."
               INSPECT W40-NAME REPLACING FIRST "!" BY " ".
            MOVE DEB-ACNO  TO TRL-AC.
            MOVE W40-NAME  TO TRL-NME.
            MOVE DEB-ACTIVE  TO W22-DTE2.
      PERFORM SWITCH-DATE-TO-DMCY.
      MOVE W22-DTE3  TO TRL-ACT.
    IF DEB-120 > 0
        MOVE 4   TO TRL-MTH
          IF DEB-90 > 0
              MOVE 3   TO TRL-MTH
           ELSE
           IF DEB-60 > 0
              MOVE 2   TO TRL-MTH
           ELSE
           IF DEB-30 > 0
              MOVE 1   TO TRL-MTH.
            ADD DEB-30 DEB-60 DEB-90 DEB-120
     GIVING W25-VALUE.
            MOVE W25-VALUE  TO TRL-PD.
            ADD DEB-BAL  TO W90-DEBT-TOTAL.
            ADD DEB-CUR  TO W90-CUR.
            ADD DEB-30   TO W90-30.
            ADD DEB-60   TO W90-60.
            ADD DEB-90   TO W90-90.
            ADD DEB-120  TO W90-120.
            ADD DEB-INT  TO W90-INT.
          IF DEB-BAL > 0
              ADD DEB-BAL  TO W90-BDT
           ELSE
              ADD DEB-BAL  TO W90-BCR.
            SUBTRACT W25-VALUE FROM DEB-BAL
     GIVING W25-TOTAL.
            SUBTRACT DEB-CUR  FROM W25-TOTAL.
            ADD W25-TOTAL  TO W90-MTD.
            MOVE DEB-BAL  TO TRL-BAL.
      MOVE DEB-STM  TO TRL-STM.
      MOVE DEB-LAST-PMT  TO TRL-LST-PAY.
      MOVE DEB-PMT  TO W22-DTE2.
      PERFORM SWITCH-DATE-TO-DMCY.
      MOVE W22-DTE3  TO TRL-PAY.
            MOVE DEB-INT  TO TRL-INT.
            MOVE DEB-CRL  TO TRL-LIMIT.
          IF DEB-CRL > ZERO
               IF DEB-BAL > DEB-CRL
           MOVE "*"  TO TRL-EXL.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM XA20 THRU XA25.
            MOVE SPACES  TO TRL-DET.
      MOVE "M.T.D:"  TO TRL-HEAD12.
            COMPUTE TRL-MTD = DEB-BAL - (DEB-INT + DEB-CUR + DEB-30
            + DEB-60 + DEB-90 + DEB-120)
            MOVE "CURRENT:"  TO TRL-HEAD13.
      MOVE DEB-CUR  TO TRL-CUR.
            MOVE "30 DAYS:"  TO TRL-HEAD14.
            MOVE DEB-30  TO TRL-30.
            MOVE "60 DAYS:"  TO TRL-HEAD15.
            MOVE DEB-60  TO TRL-60.
            MOVE "90 DAYS:"  TO TRL-HEAD16.
            MOVE DEB-90  TO TRL-90.
            MOVE "120 DAYS:"  TO TRL-HEAD17.
            MOVE DEB-120  TO TRL-120.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM XA20 THRU XA25.
            MOVE SPACES  TO TRL-DET.
          IF WS-ADDR = "Y"
               GO TO XA55.

       XA52.
      PERFORM XF000.
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE 3   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
      MOVE "D"   TO W02-PRN-TYPE.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              GO TO XA999-EXIT.
      MOVE SPACES  TO REP-DETAIL1.
    IF NOT (W02-LINAGE < (W02-PRN-LENGTH - 4))
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM XA20 THRU XA25.
            GO TO XA50.

       XA55.
            MOVE SPACES  TO TRL-DET.
            MOVE DEB-ADDRESS  TO W60-ADDRESS.
             PERFORM CA175-ADDRESS.
            MOVE W40-ADD1  TO TRL-NME.
      PERFORM CALL-PRINTUTL.
           IF W40-ADD2 = SPACES
               GO TO XA60.
            MOVE W40-ADD2  TO TRL-NME.
      PERFORM CALL-PRINTUTL.
          IF W40-ADD3 = SPACES
               GO TO XA60.
            MOVE W40-ADD3  TO TRL-NME.
      PERFORM CALL-PRINTUTL.
          IF W40-ADD4 = SPACES
               GO TO XA60.
            MOVE W40-ADD4  TO TRL-NME.
      PERFORM CALL-PRINTUTL.

       XA60.
            MOVE SPACES  TO TRL-DET.
            MOVE DEB-POST  TO TRL-PCDE.
            MOVE "TEL NO'S:"  TO TRL-HEAD18.
            MOVE DEB-H-TEL  TO TRL-HTEL.
            MOVE DEB-B-TEL  TO TRL-BTEL.
    IF W02-LINAGE < (W02-PRN-LENGTH - 4)
        MOVE 2   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
          ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM XA20 THRU XA25.
            MOVE SPACES  TO TRL-DET.
             PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              GO TO XA999-EXIT.
            GO TO XA52.

       XA65.
      MOVE "S"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
      MOVE "P"   TO WS-COMMAND.
             PERFORM XA20.
            MOVE "DEBTORS ACCOUNTS SUMMARY"
     TO TRL-HEADER.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO TRL-DET.
             MOVE "GRAND TOTALS" TO TRL-SUM.
      PERFORM CALL-PRINTUTL.
      MOVE "D"   TO W02-PRN-TYPE.
           IF WS-AGE = "Y"
              MOVE "30 DAYS"  TO TRL-SUM
           ELSE
              MOVE "CURRENT"  TO TRL-SUM.
            MOVE W90-CUR  TO TRL-VAL.
      PERFORM CALL-PRINTUTL.
    IF WS-AGE = "Y"
              MOVE "60 DAYS"  TO TRL-SUM
           ELSE
              MOVE "30 DAYS"  TO TRL-SUM.
            MOVE W90-30  TO TRL-VAL.
      PERFORM CALL-PRINTUTL.
    IF WS-AGE = "Y"
              MOVE "90 DAYS"  TO TRL-SUM
           ELSE
              MOVE "60 DAYS"  TO TRL-SUM.
            MOVE W90-60  TO TRL-VAL.
      PERFORM CALL-PRINTUTL.
    IF WS-AGE = "Y"
              MOVE "120 DAYS"  TO TRL-SUM
           ELSE
              MOVE "90 DAYS"  TO TRL-SUM.
            MOVE W90-90  TO TRL-VAL.
      PERFORM CALL-PRINTUTL.
            MOVE "INTEREST"  TO TRL-SUM.
            MOVE W90-INT  TO TRL-VAL.
      PERFORM CALL-PRINTUTL.
            MOVE "CREDIT BALANCES"
     TO TRL-SUM.
            MOVE W90-BCR  TO TRL-VAL.
      PERFORM CALL-PRINTUTL.
            MOVE "DEBIT BALANCES"
     TO TRL-SUM.
            MOVE W90-BDT  TO TRL-VAL.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE "DEBTOR TOTAL" TO TRL-SUM.
            MOVE W90-DEBT-TOTAL TO TRL-VAL.
      MOVE "E"   TO WS-COMMAND.
      MOVE 99   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
    IF WS-BDT = 1
        GO TO XA70.
    IF WS-BAD = "N"
        GO TO XA70.
      MOVE 1   TO WS-BDT.
            MOVE 0   TO W90-BAL W90-CUR W90-30  W90-60
              W90-90  W90-120 W90-BDT W90-BCR
        W90-INT.
      GO TO XA20.

       XA70.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
            GO TO XA999-EXIT.

       XA999-EXIT.
             EXIT.
      /
       XF000        SECTION 6.
       XF00-INIT.
      *
      *    ****    1 2  C P I
      *
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE 2   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
      MOVE "D"   TO W02-PRN-TYPE.
            MOVE SPACES  TO REP-DETAIL1.

       XF05.
      MOVE "   Date        Tran-Type      Reference      Tran.Value      Allocation    Tran.Balance" TO ST7-HD1
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.

       XF75-BFWD.
             MOVE 0              TO W25-WHOLE W90-BAL.
    IF OPEN-ITEM
        GO TO XF80-TRAN.
          IF DEB-STM = 0
               GO TO XF80-TRAN.
           IF DEB-LANG = "A"
               MOVE "REK GELEWER"
     TO ST7-BFWD
           ELSE
               MOVE "A/C RENDERED"
     TO ST7-BFWD.
            MOVE DEB-STM  TO ST7-TBAL W90-BAL.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM XA20 THRU XA25
        PERFORM XF05.
            MOVE SPACES  TO REP-DETAIL1.

       XF80-TRAN.
    IF PREVIOUS-PERIOD
              MOVE DEB-ACNO  TO TRH-KEY
        PERFORM START-AT-DTRH-KEY THRU READ-DTHIST-EXIT
    ELSE
              MOVE DEB-ACNO  TO TRA-KEY
        PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
    IF WS-F-ERROR = 5 OR 55
               GO TO XF90-SEND.

       XF85-TLOOP.
    IF PREVIOUS-PERIOD
        PERFORM READ-DTHIST-NEXT THRU READ-DTHIST-EXIT
        MOVE TRH-RECORD1  TO TRA-RECORD1
    ELSE
              PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
    IF (WS-F-ERROR = 5 OR 55) OR
              (TRA-AC NOT = DEB-ACNO)
               GO TO XF90-SEND.
           IF TRA-VALUE = ZERO
               GO TO XF85-TLOOP.
            MOVE TRA-DATE  TO W22-DTE2.
      MOVE W22-CC2  TO W22-CC3.
      MOVE W22-YY2  TO W22-YY3.
      MOVE W22-MM2  TO W22-MM3.
      MOVE W22-DD2  TO W22-DD3.
      MOVE W22-DTE3  TO ST7-TDTE.
    IF WS-REF = "O"
       IF NOT (TRA-REG = SPACES)
    MOVE TRA-REG  TO ST7-TREF
       ELSE
    MOVE TRA-REF     TO ST7-TREF
       END-IF
    ELSE
          IF NOT (TRA-REF = "00000000" OR SPACES)
              MOVE TRA-REF  TO ST7-TREF.
           IF DEB-LANG = "A"
              MOVE W00-A-DESC (TRA-CODE) TO ST7-TDESC
           ELSE
              MOVE W00-E-DESC (TRA-CODE) TO ST7-TDESC.
            MOVE TRA-VALUE  TO ST7-TDBT.
    IF NOT (TRA-ALLOCATED = ZERO)
        IF TRA-VALUE > ZERO
     MULTIPLY -1 BY TRA-ALLOCATED
     GIVING ST7-TCRD
     SUBTRACT TRA-ALLOCATED FROM TRA-VALUE
     GIVING ST7-TBAL
        ELSE
     MOVE TRA-ALLOCATED
     TO ST7-TCRD
     ADD TRA-VALUE TRA-PAID
     GIVING ST7-TBAL
        END-IF
           ELSE
              MOVE TRA-VALUE  TO ST7-TBAL.
             ADD TRA-VALUE       TO W90-BAL.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM XA20 THRU XA25
        PERFORM XF05.
            MOVE SPACES  TO REP-DETAIL1.
            GO TO XF85-TLOOP.

       XF90-SEND.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM XA20 THRU XA25.

       XF999-EXIT.
             EXIT.
      /
       YA000        SECTION 80.
       YA00.
             PERFORM ERASE-SCREEN.
      DISPLAY "DEBTORS (ACCOUNTS RECEIVABLE) DETAILED LIST"
        AT 0319 WITH FOREGROUND-COLOR Grey HIGHLIGHT.

       YA05.
      MOVE "'P'revious or 'C'urrent period  [C]"
     TO WS-ERR-MES.
      MOVE "C"   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
      MOVE WS-OPTION  TO WS-PERIOD.
          IF NOT (WS-PERIOD = "C" OR "P")
        GO TO YA05.
    IF NOT PREVIOUS-PERIOD
        GO TO YA999.

       YA10.
            PERFORM ERASE-SCREEN.
      DISPLAY "DEBTORS (A/R) PREVIOUS DETAILED LIST" AT 0323
       WITH FOREGROUND-COLOR Grey HIGHLIGHT
      *
      *         ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *         ³ PREVIOUS PERIOD  ³²²
      *         ³ Year : YY (00-99)³²²
      *         ³ Month: MM (01-12)³²²
      *         ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ²²
      *    ²²²²²²²²²²²²²²²²²²²²
      *

      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0631
        WITH FOREGROUND-COLOR Magenta HIGHLIGHT
      BACKGROUND-COLOR Magenta
       "¿" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Magenta.
      DISPLAY "³ " AT 0731 WITH FOREGROUND-COLOR Magenta HIGHLIGHT
           BACKGROUND-COLOR Magenta
       "PREVIOUS PERIOD  "
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta
       "³" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Magenta.
      DISPLAY "³ " AT 0831 WITH FOREGROUND-COLOR Magenta HIGHLIGHT
           BACKGROUND-COLOR Magenta
       "Year :    (00-99)" WITH FOREGROUND-COLOR Green
           BACKGROUND-COLOR Magenta
       "³" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Magenta.
      DISPLAY "³ " AT 0931 WITH FOREGROUND-COLOR Magenta HIGHLIGHT
           BACKGROUND-COLOR Magenta
       "Month:    (01-12)" WITH FOREGROUND-COLOR Green
           BACKGROUND-COLOR Magenta
       "³" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Magenta.
      DISPLAY "À" AT 1031 WITH FOREGROUND-COLOR Magenta HIGHLIGHT
          BACKGROUND-COLOR Magenta
       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" WITH FOREGROUND-COLOR Black
      BACKGROUND-COLOR Magenta.
      MOVE 7   TO SHADE-ROW.
      MOVE 33   TO SHADE-COL.
      MOVE 18   TO SHADE-WIDTH.
      MOVE 4   TO SHADE-LINES.
      PERFORM SCREEN-SHADOW.
      MOVE W12-YEAR  TO W05-DYY.
      MOVE W12-MONTH  TO W05-DMM.
      *    ************************************************************
      *    ****    Y E A R   O F   P R E V I O U S   P E R I O D
      *    ************************************************************
       YA15.
      ACCEPT S80-YEAR.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE "C"  TO WS-PERIOD
       GO TO YA00
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO YA15
        END-EVALUATE.
    IF W05-DYY > W12-YEAR
        MOVE "Year can not be > Current Year"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO YA15.
      *    ************************************************************
      *    ***    M O N T H   O F   P R E V I O U S   P E R I O D
      *    ************************************************************
       YA20.
      ACCEPT S80-MONTH.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO YA15
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO YA20
        END-EVALUATE.
    IF W05-DYY > W12-YEAR
        MOVE "Month can not be < 01 or > 12"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO YA20.
            PERFORM ERASE-SCREEN.
      DISPLAY "DEBTORS (A/R) PREVIOUS DETAILED LIST" AT 0322
       WITH FOREGROUND-COLOR Grey HIGHLIGHT.
      *    *************************************************************
      *    ****    Create paths and file names for history files    ****
      *    *************************************************************
      MOVE W05-DYY  TO W05-TYY.
      MOVE W05-DMM  TO W05-TMM.
      INSPECT W02-DBHIST REPLACING FIRST "AR_ACNT.DAT"
          BY "           ".
      MOVE W02-DBHIST  TO W60-ADDRESS.
      STRING W60-ADDRESS DELIMITED BY " "
      W05-DEB-HIS DELIMITED SIZE
      INTO W02-DBHIST.
      INSPECT W02-DTHIST REPLACING FIRST "AR_TRAN.DAT"
          BY "           ".
      MOVE W02-DTHIST  TO W60-ADDRESS.
      STRING W60-ADDRESS DELIMITED BY " "
      W05-DBT-HIS DELIMITED SIZE
      INTO W02-DTHIST.
      OPEN I-O DBHIST.
          IF NOT(WS-STATUS = "00")
        PERFORM YA200
        GO TO YA00.
      OPEN I-O DTHIST.
          IF NOT(WS-STATUS = "00")
        PERFORM YA200
        GO TO YA00.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Set up the run / age dates applicable to the history files³
      *    ³ when the statements and ageing of balances was executed by³
      *    ³ the system. These dates are stored in the first record of ³
      *    ³ the history account file, which is a dummy account with   ³
      *    ³ an account number of "     !" and a name "  !".        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      MOVE "     !"  TO DBH-ACNO.
      PERFORM READ-DBHIST THRU READ-DBHIST-EXIT.
    IF WS-F-ERROR = 56
        GO TO YA200.
      MOVE DBH-STM-DTE  TO W12-TODAY.
      GO TO YA999.

       YA200.
      MOVE "Details for period requested not available"
     TO WS-ERR-MES.
            PERFORM ERROR-MESSAGE.
      CLOSE DBHIST
     DTHIST.

       YA999.
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
             MOVE 11 TO WS-PARKEY.
             MOVE 1 TO WS-SUB-2.

       ZA15-READ-TRANS.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE 1 TO WS-SUB-1.

       ZA20-TRAN-LOOP.
             MOVE PAR-T-CODE (WS-SUB-1) TO W00-TRAN-CODE (WS-SUB-2).
             MOVE PAR-E-DESC (WS-SUB-1) TO W00-E-DESC (WS-SUB-2).
             MOVE PAR-A-DESC (WS-SUB-1) TO W00-A-DESC (WS-SUB-2).
             MOVE PAR-ACTION (WS-SUB-1) TO W00-ACTION (WS-SUB-2).
             ADD 1 TO WS-SUB-2.
           IF WS-SUB-1 < 2
               ADD 1 TO WS-SUB-1
               GO TO ZA20-TRAN-LOOP.
           IF WS-PARKEY < 60
               ADD 1 TO WS-PARKEY
               GO TO ZA15-READ-TRANS.
       ZA25.
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-AGE            TO WS-AGE.
             MOVE 1                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY        TO W95-COMP.
             MOVE PAR-DMY            TO W12-TODAY.
             MOVE PAR-YMD            TO W12-T-YMD.
             MOVE 1                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             GO TO ZA999-EXIT.

       COPY "ZA49.PRO".

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
               WHEN 5          MOVE W02-DBTRAN    TO WS-FILE
                               MOVE TRA-KEY       TO WS-KEYX
               WHEN 6          MOVE W02-DEBTOR    TO WS-FILE
                               MOVE DEB-ACNO      TO WS-KEYX
               WHEN 15         MOVE WS-PARID      TO WS-FILE
                               MOVE WS-PARKEY     TO WS-KEY
               WHEN 55         MOVE W02-DBTOLD    TO WS-FILE
                               MOVE TRO-KEY       TO WS-KEYX
               WHEN 56         MOVE W02-DBHIST    TO WS-FILE
                               MOVE DBH-ACNO      TO WS-KEYX
               WHEN OTHER      MOVE "ERROR"       TO WS-FILE
                               MOVE "DTP030"      TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
