      $SET LINKCOUNT"512" GNT"STMF08.GNT"
      ******************************************************************
      *                                                                *
      *    ******   ********  **     **  *******    ****     ******    *
      *   **    **     **     ***   ***  **        **  **   **    **   *
      *   **           **     ** * * **  **       **    **  **    **   *
      *    ******      **     **  *  **  *****    **    **   ******    *
      *         **     **     **     **  **       **    **  **    **   *
      *   **    **     **     **     **  **        **  **   **    **   *
      *    ******      **     **     **  **         ****     ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *           A C C O U N T S   R E C E I V A B L E /              *
      *                                                                *
      *       D E B T O R   S T A T E M E N T S   P R O G R A M        *
      *                                                                *
      *       Version 9.00.82 - April 2016                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  May 2006     - New field included in Accounts Receivable      *
      *                 for recording number of outstanding Jobs       *
      *                 linked to the account. Additional filler       *
      *                 added for future expansion.                    *
      *                                                                *
      *  October 2006 - Amended Accounts receivable ageing from a      *
      *                 specific date to the actual date on which      *
      *                 statements and month-end is run.               *
      *                                                                *
      *  Feb 2007     - New file (DEBDEP/AR_REST.DAT) - Debtor sales   *
      *                 restrictions file has been included which      *
      *                 will allow for departments to de excluded or   *
      *                 only selected departments allowed. This will   *
      *                 allow the user the abillity to allow or bar    *
      *                 sales of selected departments. An indicator    *
      *                    03  DEB-RESTRICT              PIC  X(01).   *
      *                        88  SALES-EXCLUDED        VALUE "E".    *
      *                        88  SALES-ALLOWED         VALUE "A".    *
      *                        88  NO-SALES-RESTRICTIONS VALUE "N".    *
      *                 has been included in the Debtor Record.        *
      *                                                                *
      *  Dec 2007     - Program changed to allow the user to print     *
      *                 statements for a range of Debtor accounts.     *
      *                 The main objective is to allow statements to   *
      *                 printed on more than one work station simil-   *
      *                 taneously, allowing a shared work load to be   *
      *                 used to print large debtor files.              *
      *                 See SECTION'S XB300, XB400 AND XB500.          *
      *                                                                *
      *                 November 2014.                                 *
      *                 The above sections have been moved to program  *
      *                 DTPReadAR which is called by any program that  *
      *                 requires a range of accounts or a starting     *
      *                 account or details from a previous period.     *
      *                                                                *
      *  October 2008 - Amended Accounts receivable ageing to create   *
      *                 duplicate files of the accounts and trans-     *
      *                 actions for printing of copy satements when    *
      *                 and if required. The file names are AR_Ayymm   *
      *                 and AR_Tyymm, where yymm is replaced with      *
      *                 the year and month.                            *
      *                                                                *
      *  March 2010   - Include facility to print to a USB printer     *
      *                 via the Windows print using the relavent       *
      *                 print API's supplied with MF COBOL. An entry   *
      *                 in the APACFIDS file has been included for     *
      *                 the system to access the bitmap file that is   *
      *                 used for the statement and is accessed using   *
      *                 "STMBMP" as the key.                           *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     STMF08.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   DECEMBER 1994.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1994 - 2016
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

       COPY "DBHIST.SL".

       COPY "DBTCON.SL".

       COPY "DBTRAN.SL".

       COPY "DEBMEM.SL".

       COPY "DEBTOR.SL".

       COPY "DTHIST.SL".

       COPY "PARAM.SL".

       COPY "SHARED.SL".

          SELECT STMREP  ASSIGN W02-STATEMENT
                          ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "DBHIST.FDE".

       COPY "DBTCON.FDE".

       COPY "DBTRAN.FDE".

       COPY "DEBMEM.FDE".

       COPY "DEBTOR.FDE".

       COPY "DTHIST.FDE".

       COPY "PARAM.FDE".

       COPY "SHARED.FDE".

      /
       FD  STMREP    LABEL RECORD OMITTED
             LINAGE 51.
       01  STM-LINE1.
           03  STM-DET         PIC X(96).
       01  STM-LINE2.
    03  FILLER        PIC  X(01).
           03  STM-COMP.
              05  STM-EXCOMP  PIC  X(22).
              05  FILLER      PIC  X(20).
          03  FILLER        PIC  X(01).
          03  FILLER        PIC  X(29).
       01  STM-LINE3.
    03  FILLER        PIC  X(01).
          03  STM-ADD        PIC  X(37).
          03  FILLER        PIC  X(01).
    03  STM-TERMS.
              05  STM-DATE    PIC  Z9/99/9999.
    03  FILLER        PIC  X(04).
          03  STM-RDATE       PIC  Z9/99/9999.
    03  FILLER        PIC  X(05).
          03  STM-AC2        PIC  X(06).
    03  FILLER        PIC  X(06).
       01  STM-LINE4.
    03  FILLER        PIC  X(01).
          03  STM-NAME        PIC  X(35).
          03  FILLER        PIC  X(04).
          03  STM-AC1        PIC  X(06).
          03  FILLER        PIC  X(06).
          03  STM-NADD        PIC  X(27).
          03  FILLER        PIC  X(01).
       01  STM-LINE5.
    03  FILLER        PIC  X(01).
          03  STM-PC1        PIC  X(08).
          03  FILLER        PIC  X(06).
          03  STM-TEL        PIC  X(05).
          03  STM-TNO        PIC  X(14).
          03  FILLER        PIC  X(05).
    03  STM-PHD        PIC  X(06).
          03  STM-PAGE        PIC  Z9.
          03  FILLER        PIC  X(33).
       01  STM-LINE6.
          03  STM-TDTE        PIC  Z9/99/9999.
          03  FILLER        PIC  X(01).
           03  STM-TDESC.
              05  S-TABR      PIC  X(01).
              05  FILLER      PIC  X(01).
              05  S-TREF      PIC  X(08).
           03  STM-V1.
              05  STM-TDT     PIC  Z(09)9.99-.
          03  FILLER        PIC  X(01).
           03  STM-V2.
              05  STM-TCR     PIC  Z(09)9.99-.
    03  STM-REF2        PIC  X(08).
          03  FILLER        PIC  X(01).
    03  STM-CODE        PIC  X(01).
          03  FILLER        PIC  X(01).
          03  STM-PD        PIC  X(04).
    03  STM-TVAL        PIC  ZZZZZZZ9.99-.
    03  STM-CB        PIC  X(01).
    03  FILLER        PIC  X(03).
       01  STM-LINE7.
          03  FILLER        PIC X(04).
           03  STM-2MTHS       PIC Z(06)9.99-.
          03  FILLER        PIC X(05).
           03  STM-1MTH        PIC Z(06)9.99-.
          03  FILLER        PIC X(05).
           03  STM-CUR         PIC Z(06)9.99-.
          03  FILLER        PIC X(33).
       01  STM-LINE8.
    03  FILLER        PIC  X(04).
          03  STM-A1        PIC  X(16).
          03  STM-A2        PIC  X(16).
          03  STM-A3        PIC  X(14).
          03  FILLER        PIC  X(30).
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
       77  WS-CHECK                PIC  X(18)         VALUE "aeWlimemnomLalismJ".
       77  WS-SUB-1                PIC  9(04)  COMP-5.
       77  WS-SUB-2                PIC  9(04)  COMP-5.
       77  WS-SUB-3                PIC  9(04)  COMP-5.
       77  WS-SUB-4                PIC  9(04)  COMP-5.
       77  WS-SUB-5                PIC  9(04)  COMP-5.
       77  WS-SUB-6                PIC  9(04)  COMP-5.
       77  WS-IXS1                 PIC  9(04)  COMP-5.
       77  WS-IXS2                 PIC  9(04)  COMP-5.
       77  WS-IXS3                 PIC  9(04)  COMP-5.
       77  WS-IXS4                 PIC  9(04)  COMP-5.
       77  WS-MSUB                 PIC  9(04)  COMP-5.
       77  WS-PARKEY               PIC  9(04)  COMP-5.
       77  WS-DEPKEY               PIC  9(04)  COMP-5.
       77  WS-NETKEY               PIC  9(06)  COMP-5.
       77  WS-SHARED               PIC  9(04)  COMP-5 VALUE 1.
       77  WS-WRKEY                PIC  9(04)  COMP-5.
       77  WS-NTP                  PIC  9(01).
       77  WS-LINES                PIC  9(02)         VALUE 34.
       77  WS-PAGE                 PIC  9(04)  COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-AGE                  PIC  X(01).
       77  WS-SEQ                  PIC  X(01).
       77  WS-CREDIT               PIC  X(01).
       77  WS-ZERO                 PIC  X(01).
       77  WS-RANGE                PIC  X(01).
       77  WS-NUM                  PIC  Z9.
       77  WS-ADDR                 PIC  X(01).
       77  WS-END                  PIC  X(01)         VALUE "N".
       77  WS-REF                  PIC  X(01).
       77  WS-STMT                 PIC  X(01).
           88  PREVIOUS-STATEMENT                     VALUE "P".
       77  WS-ERROR                PIC  9(01).
       77  WS-ERROR1               PIC  9(01).
       77  WS-PGE-LENGTH           PIC  9(02)         VALUE 66.
       77  WS-PRN-LENGTH           PIC  9(02)         VALUE 62.
       77  WS-COND                 PIC  X(06).
       77  WS-NORM                 PIC  X(06).
       77  WS-EXPP                 PIC  X(06).
       77  WS-ECAN                 PIC  X(06).
       77  WS-8LPI                 PIC  X(06).
       77  WS-6LPI                 PIC  X(06).
       77  WS-10CPI                PIC  X(06).
       77  WS-12CPI                PIC  X(06).
       77  WS-17CPI                PIC  X(06).
       77  WS-EXPAND               PIC  X(01)         VALUE " ".
       77  WS-COMP                 PIC  9(01).
       77  WS-STM                  PIC  9(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-HEAD                 PIC  X(01).
       77  WS-MESS                 PIC  X(02).
       77  WS-ACNO                 PIC  X(06).
       77  WS-ASTART               PIC  X(06).
       77  WS-AEND                 PIC  X(06).
       77  WS-S1                   PIC  9(04)  COMP-5.
       77  WS-S2                   PIC  9(04)  COMP-5.
       77  WS-S3                   PIC  9(04)  COMP-5.
       77  WS-S4                   PIC  9(04)  COMP-5.
       77  WS-S5                   PIC  9(04)  COMP-5.
       77  WS-S6                   PIC  9(04)  COMP-5.
       77  WS-S7                   PIC  9(04)  COMP-5.
       77  WS-S8                   PIC  9(04)  COMP-5.
       77  WS-ERR1                 PIC  X(22)         VALUE "Invalid account number".
       77  WS-ERR2                 PIC  X(09)         VALUE "No record".
       77  WS-AC                   PIC  X(07)         VALUE "Account".
       77  PRG-DEBT-LUP            PIC  X(12)         VALUE "DTP\DTPLOOK".
       77  TODAY-DDMMYY            PIC  9(08)  COMP-5.
       77  WS-USUB                 PIC  9(04)  COMP-5.
       77  WS-CHK                  PIC X(33)          VALUE "ENTER if correct - N if incorrect".
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *          C = Clear comment line (50)
      *          E = Clear lines (any line or lines between 5 and 46)
      *          H = Change heading
      *          S = Clear the screen and display headings
      *
           03  CRT-TYPE              PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START             PIC  9(02) VALUE 5.
           03  CRT-END               PIC  9(02) VALUE 46.
      *
      *    Calling Program
      *
           03  CRT-PROGRAM           PIC  X(15) VALUE "STMF08".
      *
      *    Screen Heading
      *
           03  CRT-HEADING           PIC  X(40) VALUE "ACCOUNTS RECEIVABLE - STATEMENTS".
      *
      *    Company Name
      *
           03  CRT-COMPANY           PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-TERMINAL.
               05  CRT-WRKHD         PIC  X(11).
               05  CRT-WRKST         PIC  X(03).

       COPY "WS.WS".

       01  WS-HELP.
           03  WS-MODULE           PIC  X(03)    VALUE "STM".
           03  WS-PROG             PIC  X(03)    VALUE "F01".

       01  W00-TRAN-CODES.
           03  W00-TRAN-TYPES                    OCCURS 100.
               05  W00-TRAN-CODE   PIC  9(02)    COMP-5.
               05  W00-ACTION      PIC  9(02)    COMP-5.
               05  W00-E-DESC      PIC  X(12).
               05  W00-A-DESC      PIC  X(12).

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

       01  W02-FILE-IDS.
           03  W02-STATEMENT.
               05  W02-STMNT       PIC X(07)     VALUE "STMF07.".
               05  W02-USE         PIC X(05)     VALUE SPACES.

       01  W05-DEB-HISTORY.
           03  W05-DBT-HIS.
               05  W05-DBT         PIC  X(04)    VALUE "AR_T".
               05  W05-TYY         PIC  9(02).
               05  W05-TMM         PIC  9(02).
               05  FILLER          PIC  X(04)    VALUE ".DAT".
           03  W05-DEB-HIS.
               05  W05-DEB         PIC  X(04)    VALUE "AR_A".
               05  W05-DEBH-ID.
                   07  W05-DYY     PIC  9(02).
                   07  W05-DMM     PIC  9(02).
               05  W05-HIST-ID     PIC  9(04)    REDEFINES W05-DEBH-ID.
               05  FILLER          PIC  X(04)    VALUE ".DAT".

       01  W05-LIST.
           03  W05-OPT             PIC  9(03).
           03  W05-TYPES                             REDEFINES W05-OPT.
               05  W05-COMP        PIC  9(01).
               05  W05-INDV        PIC  9(01).
               05  W05-INT         PIC  9(01).
      *
      *    **** Consolodated Statements Y or N
      *
           03  W05-CON-STM         PIC  X(01).
      *
      *    **** Consolodation Type A=All transactions, S=Summary
      *
           03  W05-CON-TYPE        PIC  X(01).
      *
      *    **** Statements to be printed Y or N
      *
           03  W05-PRINT-CS        PIC  X(01).

      *01  W10-SPARES.
      *    03  W10-DATE            PIC  X(10).
      *    03  W10-RDATE                             REDEFINES W10-DATE.
      *        05  W10-DTE         PIC  99/99/9999.
       01  W10-REF.
           03  W10-NUM             PIC  9(08).

       COPY "DateVariables.cpy".

       01  W25-CALCS.
           03  W25-RESULT          PIC  9(05)V99.
           03  W25-RESULT1                           REDEFINES W25-RESULT.
               05  W25-WHOLE       PIC  9(05).
               05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2                           REDEFINES W25-RESULT.
               05  W25-KEY         PIC  9(04).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).
           03  W25-TOTAL           PIC S9(07)V99 COMP-3.
           03  W25-VALUE           PIC S9(07)V99 COMP-3.

       01  W30-MESSAGES.
           03  W30-MESS6.
               05  W30-MESS                          OCCURS 2.
                   07  W30-SMESS   PIC  X(42)        OCCURS 4.
           03  W30-MESS2                             REDEFINES W30-MESS6.
               05  W30-MESS-A                        OCCURS 2.
                   07  W30-SMES    PIC  X(42)        OCCURS 4.
          03  W30-AMESSAGES.
              05  FILLER           PIC  X(42)        VALUE "Ons het nog nie u betaling vir VERLEDE".
              05  FILLER           PIC  X(42)        VALUE "MAAND ontvang nie. Indien daar 'n rede is,".
              05  FILLER           PIC  X(42)        VALUE "word u vriendelik versoek om ons in kennis".
              05  FILLER           PIC  X(42)        VALUE "te stel.".

              05  FILLER           PIC  X(42)        VALUE "U rekening is nou TWEE MAANDE agterstallig".
              05  FILLER           PIC  X(42)        VALUE "en u word versoek om dringend aandag daar-".
              05  FILLER           PIC  X(42)        VALUE "aan te skenk".
              05  FILLER           PIC  X(42)        VALUE SPACES.

              05  FILLER           PIC  X(42)        VALUE "U rekening is nou MEER AS TWEE MAANDE".
              05  FILLER           PIC  X(42)        VALUE "agterstallig! Tensy u rekening nie op".
              05  FILLER           PIC  X(42)        VALUE "datum gebring word nie, sal GEREGTILIKE".
              05  FILLER           PIC  X(42)        VALUE " STAPPE teen u geneem word.".

              05  FILLER           PIC  X(42)        VALUE "We do not appear to have received your".
              05  FILLER           PIC  X(42)        VALUE "payment for LAST MONTH. You are kindly".
              05  FILLER           PIC  X(42)        VALUE "requested to contact us if there is a".
              05  FILLER           PIC  X(42)        VALUE "problem with your account.".

              05  FILLER           PIC  X(42)        VALUE "Your account is now TWO MONTHS in arrear".
              05  FILLER           PIC  X(42)        VALUE "and we request that you give this matter".
              05  FILLER           PIC  X(42)        VALUE "your urgent attention".
              05  FILLER           PIC  X(42)        VALUE SPACES.

              05  FILLER           PIC  X(42)        VALUE "Your account is now MORE THAN TWO MONTHS".
              05  FILLER           PIC  X(42)        VALUE "in arrear! Unless your account is brought".
              05  FILLER           PIC  X(42)        VALUE "up-to-date, LEGAL ACTION will be".
              05  FILLER           PIC  X(42)        VALUE "instituted.".

           03  W30-AMESS                             REDEFINES W30-AMESSAGES.
               05  W30-ARRM                          OCCURS 2.
                   07  W30-30D     PIC  X(168).
                   07  W30-60D     PIC  X(168).
                   07  W30-90D     PIC  X(168).
           03  W30-STM.
               05  W30-STM6.
                   07  W30-STML    PIC  X(42)        OCCURS 4.
               05  W30-STM2                          REDEFINES W30-STM6.
               05  W30-STL         PIC  X(42)        OCCURS 4.

       01  W30-AGE-DATES.
           03  W30-120             PIC  9(08)    COMP-3.
           03  W30-90              PIC  9(08)    COMP-3.
           03  W30-60              PIC  9(08)    COMP-3.
           03  W30-30              PIC  9(08)    COMP-3.
           03  W30-CUR             PIC  9(08)    COMP-3.

       01  W35-AGE-VALUES.
           03  W35-CUR             PIC S9(09)V99 COMP-3.
           03  W35-30              PIC S9(09)V99 COMP-3.
           03  W35-60              PIC S9(09)V99 COMP-3.
           03  W35-90              PIC S9(09)V99 COMP-3.
           03  W35-120             PIC S9(09)V99 COMP-3.
           03  W35-150             PIC S9(09)V99 COMP-3.
           03  W35-INT             PIC S9(09)V99 COMP-3.

       COPY "W40.DBT".

       COPY "W40.WS".

       01  W45-TRAN.
           03  W45-CODE            PIC 9(02).
           03  W45-ENG             PIC X(12).
           03  W45-AFR             PIC X(12).
           03  W45-ACT             PIC X(01).

       COPY "FUNCTION.WS".

      *COPY "W50.WS".

       01  W55-STATS.
           03  W55-H1              PIC  X(09).
           03  W55-H2              PIC  X(09).
           03  W55-H3              PIC  X(09).
           03  W55-H4              PIC  X(09).
           03  W55-H5              PIC  X(09).
           03  W55-H6              PIC  X(09).
           03  W55-MONTHS.
               05  W55-MTH1        PIC  9(02)    COMP-5.
               05  W55-MTH2        PIC  9(02)    COMP-5.
               05  W55-MTH3        PIC  9(02)    COMP-5.
               05  W55-MTH4        PIC  9(02)    COMP-5.
               05  W55-MTH5        PIC  9(02)    COMP-5.
               05  W55-MTH6        PIC  9(02)    COMP-5.
           03  W55-VALUES.
               05  W55-VAL1        PIC S9(09)V99 COMP-3.
               05  W55-VAL2        PIC S9(09)V99 COMP-3.
               05  W55-VAL3        PIC S9(09)V99 COMP-3.
               05  W55-VAL4        PIC S9(09)V99 COMP-3.
               05  W55-VAL5        PIC S9(09)V99 COMP-3.
               05  W55-VAL6        PIC S9(09)V99 COMP-3.

      *
      *    November 2014 DTPNAME and DTPADDR introduced to handle the
      *    new Debtor Record layout, so these variables are no longer
      *    required. See W40.DBT copy file.
      *
      *01  W60-NME-ADD.
      *    03  W60-NAME.
      *        05  W60-NCHAR OCCURS 40 PIC  X(01).
      *    03  W60-ADDRESS.
      *        05  W60-ACHAR OCCURS 60 PIC  X(01).

       01  W75-KEYS.
           03  W75-S               PIC  9(02)    COMP-5.
           03  W75-S1              PIC  9(02)    COMP-5.
           03  W75-DNO             PIC  X(06)    OCCURS 18.

       01  W75-TITLES.
           03  W75-DESCRIP.
               05  FILLER          PIC  X(28)    VALUE "Mr     Mnr    Mrs    Mev    ".
               05  FILLER          PIC  X(28)    VALUE "Miss   Mej    Ms     Ms     ".
               05  FILLER          PIC  X(28)    VALUE "Dr     Dr     Rev    Ds     ".
               05  FILLER          PIC  X(28)    VALUE "Prof   Prof   The HonSy Edel".
               05  FILLER          PIC  X(28)    VALUE "                            ".
           03  W75-CODES                         REDEFINES W75-DESCRIP.
               05  W75-TITLE                     OCCURS 10.
                   07  W75-ETITLE  PIC  X(07).
                   07  W75-ATITLE  PIC  X(07).

       01  W80-ACCOUNT.
           03  W80-ACNO            PIC  X(06).
           03  W80-NUMD                          REDEFINES W80-ACNO.
               05  W80-SERIAL      PIC  9(06).
           03  W80-ACNO2           PIC  X(06).
           03  W80-TYPE            PIC  9(01).

       COPY "W90.DBT".

       COPY "W95.STM".

       01  W100-EDIT.
           03  W100-LBAL           PIC  Z(06)9.99-.
           03  W100-LARR           PIC  Z(06)9.99-.
           03  W100-BAL            PIC  Z(08)9.99-.
           03  W100-CRL            PIC  Z(06)9.

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
      *   ******    ******   *******   ********  ********  **     **
      *  **    **  **    **  **    **  **        **        ***    **
      *  **        **        **   **   **        **        ** *   **
      *   ******   **        ******    *****     *****     **  *  **
      *        **  **        **   **   **        **        **   * **
      *  **    **  **    **  **    **  **        **        **    ***
      *   ******    ******   **    **  ********  ********  **     **
      *
       SCREEN SECTION.

       01  S05D.
           03  FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Brown.
               05  LINE 12 COLUMN 13 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
               05  LINE 13 COLUMN 13 VALUE "³       Select the Range of accounts to print         ³".
               05  LINE 14 COLUMN 13 VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
               05  LINE 15 COLUMN 13 VALUE "³ Start A/C:                                          ³".
               05  LINE 16 COLUMN 13 VALUE "³                                                     ³".
               05  LINE 17 COLUMN 13 VALUE "³ End A/C:                                            ³".
               05  LINE 18 COLUMN 13 VALUE "³                                                     ³".
               05  LINE 19 COLUMN 13 VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
               05  LINE 20 COLUMN 13 VALUE "³    Statements will be printed in name sequence.     ³".
               05  LINE 21 COLUMN 13 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S15B.
           03  LINE  6 COLUMN 31 VALUE "PRINTING STATEMENTS" FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE  9 COLUMN 12 VALUE "Are you sure (Y/N)?".

       01  S20D.
           03  BACKGROUND-COLOR Red FOREGROUND-COLOR Red HIGHLIGHT.
               05  LINE 09 COLUMN 12 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" FOREGROUND-COLOR Black.
               05          COLUMN 68 VALUE                                                         "¿".
               05  LINE 10 COLUMN 12 VALUE "³ "                                                       FOREGROUND-COLOR Black.
               05          COLUMN 14 VALUE   "This routine will generate consolodating transactions " FOREGROUND-COLOR Grey  HIGHLIGHT.
               05          COLUMN 68 VALUE                                                         "³".
               05  LINE 11 COLUMN 12 VALUE "³ "                                                       FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE   "and as there is no recovery procedure, backups of the " FOREGROUND-COLOR Grey  HIGHLIGHT.
               05          COLUMN 68 VALUE                                                         "³".
               05  LINE 12 COLUMN 12 VALUE "³ "                                                       FOREGROUND-COLOR Black.
               05          COLUMN 12 VALUE   "data files should be made before proceeding.          " FOREGROUND-COLOR Grey  HIGHLIGHT.
               05          COLUMN 68 VALUE                                                         "³".
               05  LINE 13 COLUMN 12 VALUE "³            "                                            FOREGROUND-COLOR Black.
               05          COLUMN 25 VALUE              "Have backups been made "                     FOREGROUND-COLOR Grey  HIGHLIGHT.
               05          COLUMN 48 VALUE                                     "Y"                    FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 49 VALUE                                      "/"                   FOREGROUND-COLOR Grey  HIGHLIGHT.
               05          COLUMN 50 VALUE                                       "N"                  FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 51 VALUE                                        "                 ³".
               05  LINE 14 COLUMN 12 VALUE "À"                                                        FOREGROUND-COLOR Black.
               05          COLUMN 13 VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
       
       COPY "DEBLUP.CRT".

       01  S25.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Magenta HIGHLIGHT.
               05  LINE  9 COLUMN  8 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"  FOREGROUND-COLOR Black.            
               05          COLUMN 71 VALUE                                                                "¿".
               05  LINE 10 COLUMN  8 VALUE "³              "                                                  FOREGROUND-COLOR Black.          
               05          COLUMN 23 VALUE                "EXTRACTING CONSOLODATED STATEMENTS              "  FOREGROUND-COLOR Grey  HIGHLIGHT.   
               05          COLUMN 71 VALUE                                                                "³".
               05  LINE 11 COLUMN  8 VALUE "Ã"                                                                FOREGROUND-COLOR Black.            
               05          COLUMN  9 VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"  FOREGROUND-COLOR Cyan.             
               05          COLUMN 71 VALUE                                                                "´".
               05  LINE 12 COLUMN  8 VALUE "³ "                                                               FOREGROUND-COLOR Black.            
               05          COLUMN 10 VALUE   "Debtor Ac. : "                                                  FOREGROUND-COLOR Grey  HIGHLIGHT.   
               05          COLUMN 23 VALUE                "                                                ³".
               05  LINE 13 COLUMN  8 VALUE "³ "                                                               FOREGROUND-COLOR Black.            
               05          COLUMN 10 VALUE   "Transaction: "                                                  FOREGROUND-COLOR Grey  HIGHLIGHT   
               05          COLUMN 23 VALUE                "                                                ³".
               05  LINE 14 COLUMN  8 VALUE "À"                                                                FOREGROUND-COLOR Black.           
               05          COLUMN  9 VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
              
       01  S25D.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
               05  LINE 12 COLUMN 23 PIC  X(06) FROM W40-ACNO.
               05          COLUMN 30 PIC  X(40) FROM W40-DSP-NAME.
                         
       01  S25T.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
               05  LINE 13 COLUMN 23 PIC  X(06)      FROM TRA-AC   FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 30 PIC  9999/99/99 FROM TRA-DATE.
               05          COLUMN 41 PIC  X(02)      FROM TRA-CODE FOREGROUND-COLOR Grey  HIGHLIGHT. 
               05          COLUMN 44 PIC  X(08)      FROM TRA-REF  FOREGROUND-COLOR Brown HIGHLIGHT.
                                
       01  S80D.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Magenta HIGHLIGHT.
               05  LINE 08 COLUMN 31 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
               05          COLUMN 50 VALUE                    "¿" FOREGROUND-COLOR Black.
               05  LINE 09 COLUMN 31 VALUE "³".
               05          COLUMN 32 VALUE  "PREVIOUS STATEMENT".
               05          COLUMN 50 VALUE                    "³" FOREGROUND-COLOR Black.
               05  LINE 10 COLUMN 31 VALUE "³ ".
               05          COLUMN 33 VALUE   "Year :    (00-99)"  FOREGROUND-COLOR Green.
               05          COLUMN 50 VALUE                    "³" FOREGROUND-COLOR Black.
               05  LINE 11 COLUMN 31 VALUE "³ ". 
               05          COLUMN 33 VALUE   "Month:    (01-12)"  FOREGROUND-COLOR Green.
               05          COLUMN 50 VALUE                    "³" FOREGROUND-COLOR Black.
               05  LINE 12 COLUMN 31 VALUE "À".
               05          COLUMN 32 VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" FOREGROUND-COLOR Black.
 
       01  S80A.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
               05  S80-YEAR.
                   07  LINE 10 COLUMN 40 PIC  Z9 USING W05-DYY AUTO.
               05  S80-MONTH.
                   07  LINE 11 COLUMN 40 PIC  Z9 USING W05-DMM AUTO.

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
           IF LS-DSKDRV = "L"
               MOVE "Not available on Local drive" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO AA49.
           IF LS0-DBLEV < 2
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA00-INIT.
             PERFORM YA000.
             PERFORM XB000.
      *    IF W05-PRINT-CS = "Y"
      *        PERFORM XC000.
           IF PREVIOUS-STATEMENT
               CLOSE DBHIST DTHIST.

       AA49.
             EXIT PROGRAM.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ This routine is used to call the Debtor / Ac's Receivable ³
      *    ³ lookup program and cancel it when control is returned.    ³
      *    ³ The  relevant  error  message  will be  displayed  if  an ³
      *    ³ exception occurs. The usual  parameters are passed to the ³
      *    ³ lookup program as well W40-COMPANY which will contain the ³
      *    ³ details of the account that has been selected when control³
      *    ³ is returned to the calling program.                       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AA50-LOOKUP     SECTION.
             PERFORM SAVE-SCREEN.
             MOVE 4                  TO W44-FUNCTION.
             PERFORM SCREEN-CONTENTS.
             CALL PRG-DEBT-LUP USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS W40-COMPANY
                  ON EXCEPTION GO TO AA55
             END-CALL.
             CANCEL PRG-DEBT-LUP.
             GO TO AA58.

       AA55.
             MOVE SPACE              TO WS-ERR-MES.
             STRING "Program- " DELIMITED SIZE PRG-DEBT-LUP DELIMITED " " " not on disk, press ANY key" DELIMITED SIZE INTO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.

       AA58.
             PERFORM RESTORE-SCREEN.

       AA99.
             EXIT.

      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S
      *
       AA300     SECTION.
       AA301.
             DISPLAY "F1"                                                              AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                       "=Help,"                                                                WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                             "F2"                                                              WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                               " to do Account Lookup,"                                        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                                     "F9"                                      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                                       " Debtor Lookup (Any Word),"            WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                                                                 "Esc"         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                                                                    " to exit" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
      
       AA305.
             EXIT.

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

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                      OPT-MESSAGE                          ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine is used to allow the OPERATOR to respond to  ³
      *    ³ a request for an option, so that the correct action can   ³
      *    ³ be performed by the program. The routine will display the ³
      *    ³ message in a pop-up window and allow the OPERATOR to      ³
      *    ³ respond to the 'question'.                                ³
      *    ³                                                           ³
      *    ³ The option request must be placed in WS-OPT-MES and may   ³
      *    ³ not exceed 48 characters. The message will be centred in  ³
      *    ³ the window. An example of a message request follows:      ³
      *    ³                                                           ³
      *    ³   MOVE "Print transactions (Y/N) [ ]" TO WS-OPT-MES.      ³
      *    ³   PERFORM OPT-MESSAGE.                                    ³
      *    ³                                                           ³
      *    ³ This would be displayed as:                               ³
      *    ³    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿     ³
      *    ³    ³          Print transactions (Y/N) [ ]          ³°°   ³
      *    ³    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°   ³
      *    ³      °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°   ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³                                                           ³
      *    ³ The system will display the message box with the top line ³
      *    ³ at line as the value of WS-MES-LINE. If WS-MES-LINE is    ³
      *    ³ zero the default will be 20.                              ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

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

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³        R E A D  F I L E S   R O U T I N E S        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AC000-READ        SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "DBHIST.RD".

       COPY "DBTCON.RD".

       COPY "DBTRAN.RD".

       COPY "DEBMEM.RD".

       COPY "DEBTOR.RD1".

       COPY "DTHIST.RD".

       COPY "SHARED.RD".

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   & W R I T E   F I L E S R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AD000-WRITE        SECTION.

       COPY "DBTCON.WR".

       COPY "DBTRAN.WR".

       COPY "DEBTOR.WR".

       COPY "SHARED.WR".

       COPY "DEBTOR.LUP".

       COPY "APAC.HLP".

      /
       CA155-GET-DEBTOR        SECTION 2.
       CA155-ACCOUNT.
    IF PREVIOUS-STATEMENT
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
      *
      *    DTPNAME and DTPADDR programs introduced November 2014 to
      *    process new Debtor Record layout for storage, display and
      *    print purposes. These routines are no longer required and
      *    will be removed on the next source code cleanup.
      *
      *CA165-NAME              SECTION 2.
      *CA165-EXTRACT.
      *     MOVE ZERO   TO WS-ERROR.
      *      UNSTRING W60-NAME DELIMITED BY "*" OR "!"
      *        INTO W40-SURNAME W40-FNAME W40-SNAME
      *        OVERFLOW
      *  MOVE 1   TO WS-ERROR.
      *CA170-EXIT.
      *      EXIT.

      *CA175-ADDRESS           SECTION 2.
      *CA175-EXTRACT.
      *      MOVE 0                  TO WS-ERROR.
      *      UNSTRING W60-ADDRESS DELIMITED BY "*" INTO W40-ADD1 W40-ADD2 W40-ADD3 W40-ADD4
      *        OVERFLOW MOVE 1       TO WS-ERROR.
      *CA180-EXIT.
      *      EXIT.
      /    *************************************************************
      *    ****    E X T R A C T   C O N S O L O D A T E D
      *     A C C O U N T   T R A N S A C T I O N S
      *    *************************************************************
       XA000        SECTION 6.
       XA00.
             PERFORM SAVE-SCREEN-3.
      *    *************************************************************
      *           D I S P L A Y   W A R N I N G   M E S S A G E
      *    *************************************************************
      *   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *   ³ This routine will generate consolodating transactions ³°°
      *   ³ and as there is no recovery procedure, backups of the ³°°
      *   ³ data files should be made before proceeding.          ³°°
      *   ³            Have backups been made Y/N                 ³°°
      *   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°
      *     °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
             MOVE  9                 TO SHADE-ROW.
             MOVE 12                 TO SHADE-COL.
             MOVE 57                 TO SHADE-WIDTH.
             MOVE 6                  TO SHADE-LINES.
             DISPLAY S20D.
             PERFORM SCREEN-SHADOW.
             MOVE "N"                TO WS-OPTION.
             MOVE 13                 TO WS-MES-LINE.
             MOVE 52                 TO WS-MES-COL.
      *
      *      ACCEPT the option at the position specified above
      *
             MOVE X"FA"              TO WS-MES-CHAR(48).
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "N"
               MOVE 9                TO WS-STM
               GO TO XA999-EXIT.
           IF W05-CON-TYPE = "A"
               OPEN OUTPUT DBTCON
               CLOSE DBTCON
               OPEN I-O DBTCON.
      *ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *³              EXTRACTING CONSOLODATED STATEMENTS              ³°°
      *ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´°°
      *³ Debtor Ac. : AAAAAA NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN ³°°
      *³ Transaction: AAAAAA CCCCMMYY TT RRRRRRRR                     ³°°
      *ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°
      *  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
             MOVE 9                  TO SHADE-ROW.
             MOVE 8                  TO SHADE-COL.
             MOVE 64                 TO SHADE-WIDTH.
             MOVE 6                  TO SHADE-LINES.
             DISPLAY S25.
             PERFORM SCREEN-SHADOW.

       XA05.
             PERFORM PRINT-ABORT.
           IF WS-OPTION = "Y"
               MOVE 9                TO WS-STM
               GO TO XA999-EXIT.
            PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
          IF WS-F-ERROR = 6
              PERFORM RESTORE-SCREEN-3
              IF W05-CON-TYPE = "A"
                  CLOSE DBTCON
              END-IF
              GO TO XA999-EXIT.
           IF DEB-CONAC = SPACES
               GO TO XA05.
      *
      *    ****    Move the Debtor Record to working storage area
      *
             MOVE DEB-RECORD1        TO W40-DEBTREC.
      *    *************************************************************
      *    ****         T E S T   F O R   I N D I V I D U A L       ****
      *    *************************************************************
           IF W40-TYPE = 1
               MOVE "D"              TO W40-INSTR
               CALL "DTP\DTPNAME" USING W40-COMPANY LS-USER-ID
           ELSE
               MOVE W40-NAME         TO W40-DSP-NAME.
             DISPLAY S25D.
           IF DEB-CONAC = DEB-ACNO
               GO TO XA20.
      *    *************************************************************
      *    ****  C H E C K   T H A T   C O N S O L O D A T E D
      *          A C C O U N T   N U M B E R   I S   V A L I D
      *    *************************************************************
             MOVE DEB-ACNO           TO W80-ACNO2.
             MOVE DEB-CONAC          TO W80-ACNO.
             PERFORM CA155-GET-DEBTOR.
          IF NOT(WS-ERROR = ZERO)
              PERFORM XA10 THRU XA15
              GO TO XA05.

       XA10.
             MOVE W80-ACNO2          TO W80-ACNO.
             PERFORM CA155-GET-DEBTOR.
             PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.

       XA15.
             PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
           IF NOT(DEB-ACNO = W80-ACNO2)
               GO TO XA15.

       XA20.
           IF NOT(DEB-ACNO = DEB-CONAC)
               IF NOT(DEB-BAL = ZERO)
                   PERFORM XA30.
           IF W05-CON-TYPE = "S"
               GO TO XA05.
             MOVE DEB-ACNO           TO TRA-KEY.
             PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
           IF WS-F-ERROR = 5
               GO TO XA05.

       XA25.
             PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
           IF (WS-F-ERROR = 5) OR (TRA-AC NOT = DEB-ACNO)
               GO TO XA05.
             DISPLAY S25T.
             MOVE TRA-RECORD1        TO CON-TRAN.
             MOVE DEB-CONAC          TO DBT-CON.
           IF DEB-CONAC = TRA-AC
               MOVE "00"             TO DBT-IND
           ELSE
               MOVE "01"             TO DBT-IND.
             PERFORM WRITE-DBTCON THRU WRITE-DBTCON-EXIT.
             MOVE "Y"                TO W05-PRINT-CS.
             GO TO XA25.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Generate transfer transactions to CONSOLODATED account.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       XA30.
             MOVE 5                  TO WS-SHARED.
             PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
             INITIALIZE TRA-RECORD1.
      *    *************************************************************
      *    **** T R F   T O   C O N S O L O D A T E D   A C C O U N T
      *    *************************************************************
             MOVE DEB-CONAC          TO TRA-AC.
             MOVE W12-T-YMD          TO TRA-DATE.
           IF DEB-BAL > ZERO
               MOVE 81               TO TRA-CODE
           ELSE
               MOVE 82               TO TRA-CODE.
             MOVE DEB-ACNO           TO TRA-REF TRA-REG TRA-ORD.
             MOVE DEB-BAL            TO TRA-VALUE TRA-TAXFREE.
             MOVE ZERO               TO TRA-TAX TRA-DISC TRA-PAID TRA-AGE.
             MOVE SPACES             TO TRA-REC.
             MOVE DEB-ACNO           TO W80-ACNO2.
             MOVE DEB-CONAC          TO W80-ACNO.
             PERFORM CA155-GET-DEBTOR.
             ADD TRA-VALUE           TO DEB-BAL.
             PERFORM REWRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.
             PERFORM WRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
      *    *************************************************************
      *    ****  T R F   F R O M   S U B O R D O N A T E   A C C O U N T
      *    *************************************************************
             PERFORM XA10 THRU XA15.
             MOVE DEB-ACNO           TO TRA-AC.
           IF TRA-CODE = 82
               MOVE 81               TO TRA-CODE
           ELSE
               MOVE 82               TO TRA-CODE.
             MOVE DEB-CONAC          TO TRA-REF TRA-REG TRA-ORD.
             MULTIPLY -1 BY             TRA-VALUE.
             MULTIPLY -1 BY             TRA-TAXFREE.
             ADD TRA-VALUE           TO DEB-BAL.
             PERFORM REWRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.
             PERFORM WRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
             UNLOCK SHARED.

       XA999-EXIT.
             EXIT.
      *    *************************************************************
      *    ****    P R I N T   S T A T E M E N T S
      *    *************************************************************
       XB000        SECTION 50.
       XB00-INIT.
             PERFORM ERASE-SCREEN.
             DISPLAY S15B.
             MOVE "N"                TO WS-OPTION.
             MOVE 9                  TO WS-MES-LINE.
             MOVE 32                 TO WS-MES-COL.
      *
      *      ACCEPT the option at the position specified above
      *
             MOVE X"FA"              TO WS-MES-CHAR(48).
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
          IF WS-OPTION = "N"
              GO TO XB299-EXIT.
             DISPLAY "Insert stationery - ENTER" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
             DISPLAY "Waiting for printer response" AT 1412 WITH FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
             OPEN OUTPUT STMREP.
             CALL X"91" USING X91-RES X91-FUN STMREP.
      *
      *    Clear line 14 of the screen
      *
             MOVE 14                 TO CRT-START CRT-END.
             PERFORM ERASE-SCREEN.
       XB05-TEST.
            MOVE SPACES  TO STM-DET.
             WRITE STM-LINE1 BEFORE 1 LINES.
            MOVE WS-EXPP  TO STM-LINE1.
            WRITE STM-LINE1 BEFORE 0 LINES.
            MOVE SPACES  TO STM-DET.
            MOVE ALL "X"  TO STM-COMP.
            WRITE STM-LINE1 BEFORE 1 LINES.
            MOVE WS-ECAN  TO STM-LINE1.
            WRITE STM-LINE1 BEFORE 2 LINES.
            MOVE SPACES  TO STM-DET.
            MOVE W12-TODAY  TO STM-DATE.
            WRITE STM-LINE1 BEFORE 4 LINES.
            MOVE SPACES  TO STM-DET.
            MOVE ALL "X"  TO STM-AC1 STM-NADD.
            WRITE STM-LINE1 BEFORE 4 LINES.
            MOVE SPACES  TO STM-DET.
            MOVE ALL "X"  TO STM-AC2.
            MOVE W12-TODAY  TO STM-RDATE.
            WRITE STM-LINE1 BEFORE 6 LINES.
            MOVE SPACES  TO STM-DET.
            MOVE W12-TODAY  TO STM-TDTE.
            MOVE ALL "X"  TO S-TABR S-TREF STM-REF2 STM-CODE.
      MOVE -9999999.99  TO STM-TDT STM-TCR STM-TVAL.
            WRITE STM-LINE1 BEFORE 26 LINES.
            MOVE SPACES  TO STM-DET.
            MOVE 9999999999.99  TO STM-TCR STM-TVAL.
             WRITE STM-LINE1 BEFORE 1 LINES.
            MOVE SPACES  TO STM-DET.
             WRITE STM-LINE1 BEFORE PAGE.
             MOVE 1201           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
            DISPLAY WS-CHK AT 1212.
            ACCEPT WS-OPTION AT 1246
            WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "N")
               GO TO XB10-COMP.
            DISPLAY WS-A AT 1412.
            ACCEPT WS-OPTION AT 1432
            WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
             GO TO XB05-TEST.
       XB10-COMP.
             MOVE 0401           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
            MOVE "N"   TO WS-EXPAND.
            DISPLAY (3, 2)
               "If the Company name is not more than 22 characters it "
              "may be printed using    ".
            DISPLAY "expanded characters" AT 0402.
            DISPLAY (6, 2)
              "Do you wish to print using expanded characters (Y/N)?".
            ACCEPT WS-EXPAND AT 0656
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta AUTO.
      CALL "CBL_TOUPPER" USING WS-EXPAND
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
            DISPLAY "Statement message" AT 0802.
            DISPLAY "English" AT 1002.
            DISPLAY "Afrikaans" AT 1041.
             MOVE SPACES TO W30-MESS (1) W30-MESS (2).

       XB11.
            ACCEPT W30-SMESS (2, 1) AT 1009
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
          BACKGROUND-COLOR Magenta UPDATE AUTO.
            ACCEPT W30-SMESS (2, 2) AT 1109
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
          BACKGROUND-COLOR Magenta UPDATE AUTO.
            ACCEPT W30-SMESS (2, 3) AT 1209
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
          BACKGROUND-COLOR Magenta UPDATE AUTO.
            ACCEPT W30-SMESS (1, 1) AT 1050
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
          BACKGROUND-COLOR Magenta UPDATE AUTO.
            ACCEPT W30-SMESS (1, 2) AT 1150
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
          BACKGROUND-COLOR Magenta UPDATE AUTO.
            ACCEPT W30-SMESS (1, 3) AT 1250
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
          BACKGROUND-COLOR Magenta UPDATE AUTO.
            DISPLAY WS-CHK AT 1402.
            ACCEPT  WS-OPTION AT 1439
             WITH FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF WS-OPTION = "N"
               GO TO XB11.
            DISPLAY (16, 2) "Print keyed message on accounts < (30), "
                             "(60), (90) days >  < -else STD Message".
            DISPLAY (18, 2) "XX = Ignore all standard messages, print"
                              WITH FOREGROUND-COLOR Brown HIGHLIGHT.
            DISPLAY (18, 43) "only the keyed message"
                               WITH FOREGROUND-COLOR Brown HIGHLIGHT.
      MOVE 2   TO WS-LENGTH.

       XB11A.
            ACCEPT WS-MESS AT 1659
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
      CALL "CBL_TOUPPER" USING WS-MESS
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-MESS = "30" OR "60" OR "90" OR "XX")
               GO TO XB11A.
      MOVE 1   TO WS-LENGTH.

       XB11B.
      MOVE "Print 'I'nvoice or 'O'ther reference  [I]"
     TO WS-ERR-MES.
      MOVE "I"   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
      MOVE WS-OPTION  TO WS-REF.
          IF NOT (WS-REF = "I" OR "O")
        GO TO XB11B.

       XB12.
            MOVE 1   TO WS-SUB-3.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-COMPANY  TO W95-COMP.
            MOVE PAR-TELEPHONE  TO W95-TEL.
            ADD 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-ADDRESS1  TO W95-ADD1.
            MOVE PAR-ADDRESS2  TO W95-ADD2.
            MOVE PAR-ADDRESS3  TO W95-ADD3.
            MOVE PAR-ADDRESS4  TO W95-ADD4.
            MOVE PAR-POST-CDE  TO W95-POST.
   IF W95-ADD4 = SPACES
       MOVE W95-POST  TO W95-ADD4
       MOVE SPACES  TO W95-PC.
   IF W95-ADD3 = SPACES
       MOVE W95-ADD4  TO W95-ADD3
       MOVE SPACES  TO W95-ADD4.
   IF W95-ADD2 = SPACES
       MOVE W95-ADD3  TO W95-ADD2
       MOVE SPACES  TO W95-ADD3.
      MOVE 0301   TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
            DISPLAY "Print accounts that became zero this month ("
        AT 0612
       "Y" WITH FOREGROUND-COLOR Brown HIGHLIGHT "/"
       "N" WITH FOREGROUND-COLOR Brown HIGHLIGHT ")".
            ACCEPT WS-OPTION AT 0661
            WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF WS-OPTION = "N"
              MOVE 1   TO WS-S8
           ELSE
              MOVE ZERO  TO WS-S8.
            DISPLAY "Selective print (" AT 0812
       "Y" WITH FOREGROUND-COLOR Brown HIGHLIGHT "/"
       "N" WITH FOREGROUND-COLOR Brown HIGHLIGHT ")".

       XB13.
            ACCEPT WS-OPTION AT 0834
            WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "N" OR "Y")
               GO TO XB13.
           IF WS-OPTION = "Y"
              MOVE 1   TO WS-NTP
               GO TO XB125
           ELSE
              MOVE ZERO  TO WS-NTP.
            DISPLAY "Is this a restart (" AT 1012
       "Y" WITH FOREGROUND-COLOR Brown HIGHLIGHT "/"
       "N" WITH FOREGROUND-COLOR Brown HIGHLIGHT ")".

       XB13A.
            ACCEPT WS-OPTION AT 1036
            WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "Y" OR "N")
               GO TO XB13A.
           IF WS-OPTION = "Y"
               GO TO XB130.

       XB14.
      MOVE "'A'll statements or 'R'ange of statements [A]"
     TO WS-ERR-MES.
      MOVE "A"   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
    IF NOT(WS-OPTION = "A" OR "R")
        GO TO XB14.
      MOVE WS-OPTION  TO WS-RANGE.
      DISPLAY "Debtor account:" AT 1412.
             MOVE 1              TO WS-SUB-2.

       XB14-RANGE-CHECK.
    IF WS-RANGE = "R"
        MOVE SPACES  TO WS-ASTART WS-AEND
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Save screen details and display the get range panel.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
        PERFORM XB300
        MOVE 0726  TO COL-AND-LINE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³   First account in the range that must be printed.     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
        PERFORM XB500
        IF WS-ERROR = 9
     GO TO XB299-EXIT
        END-IF
        MOVE W80-ACNO  TO WS-ASTART
        MOVE 0926  TO SPOS
        MOVE SPOS  TO COL-AND-LINE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³   Last account in the range that must be printed.      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
        PERFORM XB500
        IF WS-ERROR = 9
     GO TO XB299-EXIT
        END-IF
        MOVE W80-ACNO  TO WS-AEND
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Start reading from the first account in the range.    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
        PERFORM XB400
        GO TO XB20-LOOP
    END-IF.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Start reading from the first account in the file.     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       XB15-INDEX.
    IF PREVIOUS-STATEMENT
        MOVE " !"  TO DBH-NAME
        PERFORM START-AT-DEBH-NAME THRU READ-DBHIST-EXIT
    ELSE
        MOVE " !"  TO DEB-NAME
        PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.
          IF WS-F-ERROR = 6 OR 56
              GO TO XB299-EXIT.
      *
      *    The printing of Consolodated statements needs to be changed
      *    and tested extensively.
      *    As the extracting and transferring has already been done
      *    when the statements were originally printed and these rou-
      *    tines need to be by-passed when printing history statements.
      *    The code will be commented out of the running programs until
      *    the testing has been completed.
      *
      *    IF NOT PREVIOUS-STATEMENT
      *        IF (W05-CON-STM = "Y") AND (WS-STM = 0)
      *     MOVE 1  TO WS-STM
      *     PERFORM XA000
      *     IF WS-STM = 9
      *         GO TO XB299-EXIT
      *     ELSE
      *         GO TO XB15-INDEX.

       XB20-LOOP.
             PERFORM PRINT-ABORT.
           IF WS-OPTION = "Y"
               MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO STM-DET
               WRITE STM-LINE1 BEFORE PAGE
              GO TO XB299-EXIT.
           IF WS-RANGE = "R"
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Check if last account in the range has been printed.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               IF WS-END = "Y"
                   GO TO XB200-END.
           IF PREVIOUS-STATEMENT
               PERFORM READ-DBHIST-NEXT THRU READ-DBHIST-EXIT
               MOVE DBH-RECORD1      TO W40-DEBTREC
           ELSE
               PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT
               MOVE DEB-RECORD1      TO W40-DEBTREC.
           IF WS-F-ERROR = 6 OR 56
               GO TO XB200-END.
           IF WS-RANGE = "R"
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Check if this is the last account in range to be printed. ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               IF DEB-ACNO = WS-AEND
                   MOVE "Y"          TO WS-END.
             DISPLAY DEB-ACNO AT 1428 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.

       XB21.
           IF W40-BAL = ZERO
               IF WS-S8 = 1
                   GO TO XB100-CHECK
               ELSE
               IF (W40-DEBT = ZERO) AND (W40-CRED = ZERO)
                   GO TO XB100-CHECK.
             PERFORM XB110 THRU XB115.
             MOVE SPACES             TO W40-NAME1.
           IF W40-BAL NOT > ZERO
               MOVE ZERO             TO W90-CUR
           ELSE
               COMPUTE W90-CUR = W40-BAL - (W40-CUR + W40-30 + W40-60 + W40-90 + W40-120 + W40-INT).
      *    *************************************************************
      *    ****         T E S T   F O R   I N D I V I D U A L       ****
      *    *************************************************************
           IF W40-TYPE = 1
               MOVE "P"              TO W40-INSTR
               CALL "DTP\DTPNAME" USING W40-COMPANY LS-USER-ID
           ELSE
               MOVE W40-NAME         TO W40-DSP-NAME.
      *
      *    Extract and format Addresses for printing
      *
             MOVE "P"                TO W40-INSTR.
             CALL "DTP\DTPADDR" USING W40-COMPANY.
           IF W40-PST-ADDRESS = SPACES
               MOVE W40-STR-ADDRESS  TO W40-PST-ADDRESS.
             MOVE 0                  TO WS-PAGE.

       XB50-STM.
            MOVE SPACES  TO STM-DET.
             WRITE STM-LINE1 BEFORE 1 LINES.
           IF WS-EXPAND = "Y"
              MOVE WS-EXPP  TO STM-LINE1
               WRITE STM-LINE1 BEFORE 0 LINES
              MOVE SPACES  TO STM-LINE1
              MOVE W95-COMP  TO STM-EXCOMP
               WRITE STM-LINE1 BEFORE 0 LINES
              MOVE WS-ECAN  TO STM-LINE1
               WRITE STM-LINE1 BEFORE 0 LINES
           ELSE
              MOVE W95-COMP  TO STM-COMP.
             WRITE STM-LINE1 BEFORE 1 LINES.
            MOVE SPACES  TO STM-DET.
            MOVE W95-ADD1  TO STM-ADD.
             WRITE STM-LINE1 BEFORE 1 LINES.
            MOVE SPACES  TO STM-DET.
            MOVE W95-ADD2  TO STM-ADD.
             WRITE STM-LINE1 BEFORE 1 LINES.
            MOVE SPACES  TO STM-DET.
            MOVE W95-ADD3  TO STM-ADD.
            MOVE W12-TODAY  TO STM-DATE.
             WRITE STM-LINE1 BEFORE 1 LINES.
            MOVE SPACES  TO STM-DET.
            MOVE W95-ADD4  TO STM-ADD.
             WRITE STM-LINE1 BEFORE 1 LINES.
            MOVE SPACES  TO STM-DET.
            MOVE W95-POST  TO STM-ADD.
          IF NOT (W95-TEL = SPACES)
              MOVE "TEL:"  TO STM-TEL
              MOVE W95-TEL  TO STM-TNO.
            WRITE STM-LINE1 BEFORE 2 LINES.
            MOVE SPACES  TO STM-DET.
       XB60-DEBT.
            MOVE W40-NAME  TO STM-NAME STM-NADD.
            MOVE DEB-ACNO  TO STM-AC1.
             WRITE STM-LINE1 BEFORE 1 LINES.
            MOVE SPACES  TO STM-DET.
            MOVE W40-ADD1  TO STM-ADD.
             WRITE STM-LINE1 BEFORE 1 LINES.
            MOVE SPACES  TO STM-DET.
            MOVE W40-ADD2  TO STM-ADD.
             WRITE STM-LINE1 BEFORE 1 LINES.
            MOVE SPACES  TO STM-DET.
            MOVE W40-ADD3  TO STM-ADD.
            WRITE STM-LINE1 BEFORE 1 LINES.
            MOVE SPACES  TO STM-DET.
            MOVE W40-ADD4  TO STM-ADD.
            ADD 1   TO WS-PAGE.
      MOVE "Page:"  TO STM-PHD.
            MOVE WS-PAGE  TO STM-PAGE.
            MOVE W12-TODAY  TO STM-RDATE.
            MOVE DEB-ACNO  TO STM-AC2.
            WRITE STM-LINE1 BEFORE 1 LINES.
            MOVE SPACES  TO STM-DET.
          IF DEB-LANG = "A"
               IF DEB-TERMS = "1"
                   MOVE "30 DAE" TO STM-TERMS
               ELSE
               IF DEB-TERMS = "2"
                   MOVE "60 DAE" TO STM-TERMS
               ELSE
           MOVE "7 DAE"  TO STM-TERMS
           ELSE
           IF DEB-TERMS = "1"
              MOVE "30 DAYS"  TO STM-TERMS
           ELSE
           IF DEB-TERMS = "2"
              MOVE "60 DAYS"  TO STM-TERMS
           ELSE
              MOVE "7 DAYS"  TO STM-TERMS.
            MOVE W40-POST  TO STM-ADD.
            WRITE STM-LINE1 BEFORE 5 LINES.
            MOVE SPACES  TO STM-DET.

       XB75-BFWD.
            MOVE ZERO   TO W25-WHOLE
        W25-TOTAL
                                    W90-BAL.
           IF OPEN-ITEM
        GO TO XB76.
          IF DEB-STM = 0
               GO TO XB80-TRAN.
            MOVE "B"   TO S-TABR STM-CODE.
            MOVE DEB-STM  TO STM-TCR STM-TVAL W25-TOTAL.
             WRITE STM-LINE1 BEFORE 1 LINES.
            MOVE SPACES  TO STM-DET.
            ADD 1   TO W25-WHOLE.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ For accounts that are Brought Forward skip the recalcula- ³
      *    ³ tion of age analysis.           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      GO TO XB80-TRAN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Calculate the age analysis for this account (OPEN ITEM)  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       XB76.
      MOVE ZERO   TO W35-CUR
        W35-30
        W35-60
        W35-90
        W35-120
        W35-150
        W35-INT.
    IF PREVIOUS-STATEMENT
        MOVE DEB-ACNO  TO TRH-AC
        MOVE ZERO  TO TRH-DATE TRH-CODE
        MOVE SPACES  TO TRH-REF
    ELSE
        MOVE DEB-ACNO  TO TRA-AC
        MOVE ZERO  TO TRA-DATE TRA-CODE
        MOVE SPACES  TO TRA-REF.
    IF PREVIOUS-STATEMENT
        PERFORM START-AT-DTRH-KEY THRU READ-DTHIST-EXIT
    ELSE
        PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
    IF WS-F-ERROR = 5 OR 55
        GO TO XB80-TRAN.

       XB78.
    IF PREVIOUS-STATEMENT
        PERFORM READ-DTHIST-NEXT THRU READ-DTHIST-EXIT
        MOVE TRH-RECORD1  TO TRA-RECORD1
    ELSE
        PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
    IF (WS-F-ERROR = 5 OR 55) OR
       (DEB-ACNO NOT = TRA-AC)
        GO TO XB80-TRAN.
    IF TRA-VALUE < ZERO
               IF (TRA-VALUE + TRA-ALLOCATED) = ZERO
     GO TO XB78
               ELSE
                   ADD TRA-ALLOCATED
                                 TO TRA-VALUE
                   MOVE ZERO     TO TRA-ALLOCATED
           ELSE
               IF TRA-VALUE = TRA-PAID
     GO TO XB78
               ELSE
                   SUBTRACT TRA-PAID
                                 FROM TRA-VALUE
                   MOVE ZERO     TO TRA-PAID.
    IF TRA-VALUE > ZERO
        IF NOT (TRA-DATE > W30-120)
     MOVE 5  TO TRA-AGE
        ELSE
        IF NOT (TRA-DATE > W30-90)
     MOVE 4  TO TRA-AGE
        ELSE
        IF NOT (TRA-DATE > W30-60)
     MOVE 3  TO TRA-AGE
        ELSE
        IF NOT (TRA-DATE > W30-30)
     MOVE 2  TO TRA-AGE
        ELSE
        IF NOT (TRA-DATE > W30-CUR)
     MOVE 1  TO TRA-AGE
        ELSE
     MOVE ZERO  TO TRA-AGE.
    IF TRA-CODE = 17 OR 20
        ADD TRA-VALUE  TO W35-INT
           ELSE
           IF TRA-AGE = 5
        ADD TRA-VALUE  TO W35-150
           ELSE
           IF TRA-AGE = 4
        ADD TRA-VALUE  TO W35-120
           ELSE
           IF TRA-AGE = 3
        ADD TRA-VALUE  TO W35-90
           ELSE
           IF TRA-AGE = 2
        ADD TRA-VALUE  TO W35-60
           ELSE
           IF TRA-AGE = 1
        ADD TRA-VALUE  TO W35-30
    ELSE
    IF TRA-AGE = ZERO
        ADD TRA-VALUE  TO W35-CUR.
      GO TO XB78.

       XB80-TRAN.
    IF PREVIOUS-STATEMENT
              MOVE DEB-ACNO  TO TRH-KEY
        PERFORM START-AT-DTRH-KEY THRU READ-DTHIST-EXIT
    ELSE
              MOVE DEB-ACNO  TO TRA-KEY
        PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
    IF WS-F-ERROR = 5 OR 55
              GO TO XB90-SEND.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Get the next transaction to print on the statement.      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       XB85-TLOOP.
    IF PREVIOUS-STATEMENT
              PERFORM READ-DTHIST-NEXT THRU READ-DTHIST-EXIT
        MOVE TRH-RECORD1  TO TRA-RECORD1
    ELSE
              PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
    IF (WS-F-ERROR = 5 OR 55) OR
       (TRA-AC NOT = DEB-ACNO)
               GO TO XB90-SEND.
           IF TRA-VALUE = ZERO
               GO TO XB85-TLOOP.
          IF W25-WHOLE > 23
              PERFORM XB90-SEND
              PERFORM XB50-STM THRU XB60-DEBT
               MOVE ZERO         TO W25-WHOLE.
            MOVE TRA-DATE  TO W22-DTE2.
      MOVE W22-CC2  TO W22-CC3.
      MOVE W22-YY2  TO W22-YY3.
      MOVE W22-MM2  TO W22-MM3.
      MOVE W22-DD2  TO W22-DD3.
      MOVE W22-DTE3  TO STM-TDTE.
          IF TRA-CODE = 01
        MOVE "I"   TO S-TABR STM-CODE
    ELSE
          IF TRA-CODE = 02
        MOVE "P"   TO S-TABR STM-CODE
    ELSE
          IF TRA-CODE = 19
        MOVE "A"   TO S-TABR STM-CODE
    ELSE
          IF TRA-CODE = 17 OR 20
        MOVE "F"   TO S-TABR STM-CODE
    ELSE
           IF TRA-VALUE NEGATIVE
        MOVE "C"   TO S-TABR STM-CODE
    ELSE
        MOVE "D"   TO S-TABR STM-CODE.
    IF WS-REF = "I"
              MOVE TRA-REF  TO S-TREF STM-REF2
    ELSE
        MOVE TRA-REG  TO S-TREF STM-REF2.
       XB87.
            MOVE TRA-VALUE  TO STM-TDT.
      ADD TRA-VALUE  TO W25-TOTAL.
      MOVE W25-TOTAL  TO STM-TCR.
      *
      *    ****    A L L O C A T I O N S
      *
    IF TRA-VALUE > ZERO
        IF NOT (TRA-ALLOCATED = ZERO)
                   MULTIPLY -1 BY TRA-ALLOCATED
     GIVING STM-TVAL
     MOVE " Pd["  TO STM-PD
     MOVE "]"  TO STM-CB
        ELSE
     MOVE TRA-VALUE TO STM-TVAL
        END-IF
           ELSE
              MOVE TRA-VALUE  TO STM-TVAL.
            WRITE STM-LINE1 BEFORE 1 LINES.
            MOVE SPACES  TO STM-DET.
            ADD 1   TO W25-WHOLE.
             GO TO XB85-TLOOP.
       XB90-SEND.
            MOVE 25   TO W25-DECIMAL.
            SUBTRACT W25-WHOLE  FROM W25-DECIMAL.
             WRITE STM-LINE1 BEFORE W25-DECIMAL LINES.
             MOVE SPACES TO STM-DET.
            MOVE W30-STML (1)  TO STM-NAME.
            WRITE STM-LINE1 BEFORE 1 LINES.
            MOVE SPACES  TO STM-DET.
            MOVE W30-STML (2)  TO STM-NAME.
            MOVE DEB-BAL  TO STM-TCR STM-TVAL.
            WRITE STM-LINE1 BEFORE 1 LINES.
            MOVE SPACES  TO STM-DET.
            MOVE W30-STML (3)  TO STM-NAME.
            WRITE STM-LINE1 BEFORE 1 LINES.
            MOVE SPACES  TO STM-DET.
           IF WS-AGE = "Y"
              MOVE "  150+ DAYS" TO STM-A1
              MOVE "   120 DAYS" TO STM-A2
              MOVE "    90 DAYS" TO STM-A3
           ELSE
              MOVE "  121+ DAYS" TO STM-A1
              MOVE "91-120 DAYS" TO STM-A2
              MOVE " 61-90 DAYS" TO STM-A3.
             WRITE STM-LINE1 BEFORE 1 LINES.
            MOVE SPACES  TO STM-DET.
            ADD DEB-90 DEB-120  GIVING STM-2MTHS.
            MOVE DEB-60  TO STM-1MTH.
            MOVE DEB-30  TO STM-CUR.
            WRITE STM-LINE1 BEFORE 1 LINES.
            MOVE SPACES  TO STM-DET.
           IF WS-AGE = "Y"
              MOVE "    60 DAYS" TO STM-A1
              MOVE "    30 DAYS" TO STM-A2
           ELSE
              MOVE " 31-60 DAYS" TO STM-A1
              MOVE " 01-30 DAYS" TO STM-A2.
            MOVE "   INTEREST"  TO STM-A3.
             WRITE STM-LINE1 BEFORE 1 LINES.
            MOVE SPACES  TO STM-DET.
            MOVE DEB-CUR  TO STM-2MTHS.
            MOVE W90-CUR  TO STM-1MTH.
            MOVE DEB-INT  TO STM-CUR.
             WRITE STM-LINE1 BEFORE PAGE.
            MOVE SPACES  TO STM-DET.

       XB100-CHECK.
           IF WS-NTP = 1
               GO TO XB125.
             GO TO XB20-LOOP.
       XB110.
           IF DEB-LANG = "A"
              MOVE 1   TO WS-MSUB
           ELSE
              MOVE 2   TO WS-MSUB.
           IF WS-MESS = "XX"
               GO TO XB112.
           IF (DEB-90 > ZERO) OR (DEB-120 > ZERO)
               MOVE W30-90D (WS-MSUB) TO W30-STM
               GO TO XB115.
           IF WS-MESS = "90"
               GO TO XB112.
           IF DEB-60 > ZERO
               MOVE W30-60D (WS-MSUB) TO W30-STM
               GO TO XB115.
           IF WS-MESS = "60"
               GO TO XB112.
           IF DEB-30 > ZERO
               MOVE W30-30D (WS-MSUB) TO W30-STM
               GO TO XB115.
       XB112.
             MOVE W30-MESS (WS-MSUB) TO W30-STM.
       XB115.
             EXIT.

       XB125.
             MOVE 1001           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
            DISPLAY "Account No:" AT 1012.
      PERFORM AA300.

       XB126.
            ACCEPT W80-ACNO AT 1024
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
      MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W80-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W80-ACNO
       GO TO XB127
         WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
             PERFORM AA50
   WHEN OTHER
       CALL X"E5"
       GO TO XB126
        END-EVALUATE
              DISPLAY W80-ACNO AT 1024
         WITH FOREGROUND-COLOR Grey HIGHLIGHT
       BACKGROUND-COLOR Magenta
               IF W80-ACNO = SPACES
           GO TO XB126.
            PERFORM CLEAR-L50.

       XB127.
          IF W80-ACNO = SPACES
              GO TO XB200-END.
             PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR NOT = ZERO
               GO TO XB125.
      DISPLAY DEB-NAME AT 1031
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             DISPLAY (12, 2) WS-CHK.
            ACCEPT WS-OPTION AT 1239
            WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF WS-OPTION = "N"
               GO TO XB125.
      DISPLAY WS-BLNK78 AT 1202 WITH FOREGROUND-COLOR Cyan.
             GO TO XB21.

       XB130.
             MOVE 1201           TO CPOS.
      PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
            DISPLAY "Account No:" AT 1212.
      PERFORM AA300.

       XB131.
            ACCEPT W80-ACNO AT 1224
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
      MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W80-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W80-ACNO
       GO TO XB132
         WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
             PERFORM AA50
   WHEN OTHER
       CALL X"E5"
       GO TO XB131
        END-EVALUATE
              DISPLAY W80-ACNO AT 1224
         WITH FOREGROUND-COLOR Grey HIGHLIGHT
       BACKGROUND-COLOR Magenta
               IF W80-ACNO = SPACES
           GO TO XB131.
            PERFORM CLEAR-L50.

       XB132.
           IF W80-ACNO = SPACES
              GO TO XB200-END.
             PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR NOT = ZERO
               GO TO XB130.
             DISPLAY "Debtor account:" AT 1412.
    IF PREVIOUS-STATEMENT
        PERFORM START-AT-DEBH-NAME THRU READ-DBHIST-EXIT
    ELSE
        PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.
    IF WS-F-ERROR = 6 OR 56
        GO TO XB130.
      PERFORM XB14.
    IF WS-RANGE = "R"
        MOVE SPACES  TO WS-AEND
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Save screen details and display the get range panel.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
        PERFORM XB300
        MOVE 0726  TO COL-AND-LINE SPOS
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Display the restart account in the range to printed.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
        DISPLAY W80-ACNO AT SPOS
         WITH FOREGROUND-COLOR Grey HIGHLIGHT
        BACKGROUND-COLOR Magenta AUTO
        ADD 1   TO SLIN
        DISPLAY DEB-NAME AT SPOS
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta
        MOVE W80-ACNO  TO WS-ASTART
        MOVE 0926  TO SPOS
        MOVE SPOS  TO COL-AND-LINE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³   Last account in the range that must be printed.      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
        PERFORM XB500
        IF WS-ERROR = 9
     GO TO XB299-EXIT
        END-IF
        MOVE W80-ACNO  TO WS-AEND
        PERFORM XB400
    END-IF.

       XB135.
    IF PREVIOUS-STATEMENT
        PERFORM READ-DBHIST-NEXT THRU READ-DBHIST-EXIT
        MOVE DBH-RECORD1  TO DEB-RECORD1
    ELSE
        PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
    IF WS-F-ERROR = 6 OR 56
        GO TO XB130.
           IF W80-ACNO NOT = DEB-ACNO
               GO TO XB135.
             GO TO XB21.

       XB200-END.
             CLOSE STMREP.

       XB299-EXIT.
            EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    Save screen details and display the get range panel.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       XB300        SECTION 5.
       XB305.
             PERFORM SAVE-SCREEN-3.
             MOVE 13                 TO SHADE-ROW.
             MOVE 15                 TO SHADE-COL.
             MOVE 53                 TO SHADE-WIDTH.
             MOVE 9                  TO SHADE-LINES.
             DISPLAY S05D.
             PERFORM SCREEN-SHADOW.

       XB399.
             EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³Start reading from the first account in the range to print.³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       XB400        SECTION 51.
       XB405.
            MOVE WS-ASTART  TO W80-ACNO.
            PERFORM CA155-GET-DEBTOR.
    IF PREVIOUS-STATEMENT
        PERFORM START-AT-DEBH-NAME THRU READ-DBHIST-EXIT
    ELSE
        PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.
      PERFORM RESTORE-SCREEN-3.

       XB499.
      EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Get account numbers in the range that must be printed.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       XB500        SECTION 51.
       XB505.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Display the USER options available for this input screen. ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      PERFORM AA300.

       XB515.
      MOVE COL-AND-LINE  TO SPOS.
            ACCEPT W80-ACNO AT SPOS
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W80-ACNO
       GO TO XB520
         WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
             PERFORM AA50
   WHEN OTHER
       CALL X"E5" CALL X"E5"
       GO TO XB515
        END-EVALUATE
              DISPLAY W80-ACNO AT SPOS
         WITH FOREGROUND-COLOR Grey HIGHLIGHT
       BACKGROUND-COLOR Magenta
               IF W80-ACNO = SPACES
           GO TO XB515.
            PERFORM CLEAR-L50.

       XB520.
          IF W80-ACNO = SPACES
        MOVE 9   TO WS-ERROR
              GO TO XB599.
      MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W80-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
            PERFORM CLEAR-L50.
            PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR NOT = ZERO
              GO TO XB515.
      ADD 1   TO SLIN.
      DISPLAY DEB-NAME AT SPOS
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO XB505.

       XB599.
      EXIT.
      /    *************************************************************
      *    * P R I N T  C O N S O L O D A T E D   S T A T E M E N T S *
      *    *************************************************************
       XC000        SECTION 51.
       XC00.
      PERFORM SAVE-SCREEN-3.
      MOVE 10   TO SHADE-ROW.
      MOVE 19   TO SHADE-COL.
      MOVE 46   TO SHADE-WIDTH.
      MOVE 2   TO SHADE-LINES.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
        AT 0917 WITH FOREGROUND-COLOR Magenta HIGHLIGHT
       BACKGROUND-COLOR Magenta
       "¿" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Magenta.
      DISPLAY "³ " AT 1017 WITH FOREGROUND-COLOR Magenta HIGHLIGHT
           BACKGROUND-COLOR Magenta
       "Printing Consolodated Statements:       "
        WITH FOREGROUND-COLOR Cyan HIGHLIGHT
      BACKGROUND-COLOR Magenta
       " ³" WITH BACKGROUND-COLOR Black FOREGROUND-COLOR Magenta.
      DISPLAY "À" AT 1117 WITH FOREGROUND-COLOR Magenta HIGHLIGHT
          BACKGROUND-COLOR Magenta
       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
        WITH BACKGROUND-COLOR Black FOREGROUND-COLOR Magenta.
      PERFORM SCREEN-SHADOW.
      OPEN I-O DBTCON.
      *      MOVE ZERO   TO WS-CON.
      MOVE " !"   TO DBT-CON.
      MOVE SPACES  TO DBT-IND
        DBT-AC
        DBT-REF.
      MOVE ZERO   TO DBT-DATE
        DBT-CODE.
      PERFORM START-AT-DBT-KEY THRU READ-DBTCON-EXIT.
    IF WS-F-ERROR = 64
        MOVE "No transactions for Consolodated Statements"
     TO WS-ERR-MES
        PERFORM ERROR-LENGTH THRU ERROR-EXIT
        .
      *        GO TO XC900.

       XC05.
      PERFORM READ-DBTCON-NEXT THRU READ-DBTCON-EXIT.
      *    *************************************************************
      *     E N D   O F   C O N S O L O D A T E D   S T A T E M E N T S
      *    *************************************************************
    IF WS-F-ERROR = 64
        GO TO XC700.
      *    *************************************************************
      *     I S   T H I S   T H E   F I R S T C O N S O L O D A T E D
      *     S T A T E M E N T T O   B E   P R I N T E D   ?
      *    *************************************************************

      *    IF WS-CON = ZERO
        MOVE DBT-CON  TO W80-ACNO
        W80-ACNO2
        PERFORM CA155-GET-DEBTOR
        .
      *        PERFORM XC500 THRU XC560.
      *    *************************************************************
      *      H A S   T H E   C O N S O L O D A T I O N  A C C O U N T
      *      C H A N G E D   ?
      *    *************************************************************

      *    IF NOT(DBT-CON = W80-ACNO)
      *        GO TO XC************

      *    *************************************************************
      *   H A S  T H E  S U B O R N A T E   A C C O U N T
      *   C H A N G E D  ?
      *    *************************************************************

      *    IF DBT-AC = W80-ACNO2
      *        GO TO XC***************


          IF DBT-VALUE = ZERO
              GO TO XC05.
            MOVE DBT-DATE  TO W22-DTE2.
      MOVE W22-CC2  TO W22-CC3.
      MOVE W22-YY2  TO W22-YY3.
      MOVE W22-MM2  TO W22-MM3.
      MOVE W22-DD2  TO W22-DD3.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     **** END OF CONSOLODATED TRANSACTIONS ****        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       XC700.

       XC999-EXIT.
      EXIT.
      /
       YA000        SECTION 80.
       YA00.
             PERFORM ERASE-SCREEN.
      DISPLAY "DEBTORS (ACCOUNTS RECEIVABLE) STATEMENTS" AT 0321
       WITH FOREGROUND-COLOR Grey HIGHLIGHT.

       YA05.
      MOVE "'P'revious or 'C'urrent statements  [C]"
     TO WS-ERR-MES.
      MOVE "C"   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
      MOVE WS-OPTION  TO WS-STMT.
          IF NOT (WS-STMT = "C" OR "P")
        GO TO YA05.
    IF NOT PREVIOUS-STATEMENT
        GO TO YA999.

       YA10.
            PERFORM ERASE-SCREEN.
      DISPLAY "DEBTORS (A/R) PREVIOUS STATEMENTS" AT 0325
       WITH FOREGROUND-COLOR Grey HIGHLIGHT
      *
      *         ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *         ³PREVIOUS STATEMENT³²²
      *         ³ Year : YY (00-99)³²²
      *         ³ Month: MM (01-12)³²²
      *         ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ²²
      *    ²²²²²²²²²²²²²²²²²²²²
      *

      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0631
        WITH FOREGROUND-COLOR Magenta HIGHLIGHT
      BACKGROUND-COLOR Magenta
       "¿" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Magenta.
      DISPLAY "³" AT 0731 WITH FOREGROUND-COLOR Magenta HIGHLIGHT
          BACKGROUND-COLOR Magenta
       "PREVIOUS STATEMENT"
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
      *    ****  Y E A R   O F  P R E V I O U S   S T A T E M E N T
      *    ************************************************************
       YA15.
      ACCEPT S80-YEAR.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE "C"  TO WS-STMT
       GO TO YA00
   WHEN OTHER
       CALL X"E5" CALL X"E5" CALL X"E5" CALL X"E5"
       GO TO YA15
        END-EVALUATE.
    IF W05-DYY > W12-YEAR
        MOVE "Year can not be > Current Year"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO YA15.
      *    ************************************************************
      *    *** M O N T H   O F  P R E V I O U S   S T A T E M E N T
      *    ************************************************************
       YA20.
      ACCEPT S80-MONTH.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO YA15
   WHEN OTHER
       CALL X"E5" CALL X"E5" CALL X"E5" CALL X"E5"
       GO TO YA20
        END-EVALUATE.
    IF W05-DYY > W12-YEAR
        MOVE "Month can not be < 01 or > 12"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO YA20.
            PERFORM ERASE-SCREEN.
      DISPLAY "DEBTORS (A/R) PREVIOUS STATEMENTS" AT 0325
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
      MOVE DBH-W30-AGE-DATES
     TO W30-AGE-DATES.
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
             PERFORM ZA55 THRU ZA60.
      MOVE LS-PARID  TO WS-PARID.
      MOVE LS-L-OR-N  TO W02-L-OR-N.
      MOVE WS-SYS-ID  TO W02-SYSID.
      MOVE LS-TODAY-DDMMYY
     TO TODAY-DDMMYY.
      MOVE LS-USUB  TO WS-USUB.
            PERFORM ERASE-SCREEN.
      *
      *    ****    D O  N O T  D I S P L A Y  E R R O R
      *     M E S S A G E S
      *
      MOVE 44   TO USER-NUMBER.
      MOVE 1   TO USER-SETTING.
      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      
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
    IF TRM-STMP (WS-S6) = 1
               MOVE "LPT1"       TO W02-STATEMENT
           ELSE
           IF TRM-STMP (WS-S6) = 2
               MOVE "LPT2"       TO W02-STATEMENT
           ELSE
           IF TRM-STMP (WS-S6) = 3
               MOVE "LPT3"       TO W02-STATEMENT
           ELSE
           IF TRM-STMP (WS-S6) = 4
               MOVE "COM1"       TO W02-STATEMENT
           ELSE
           IF TRM-STMP (WS-S6) = 5
               MOVE "COM2"       TO W02-STATEMENT
           ELSE
           IF TRM-STMP (WS-S6) = 9
        MOVE LS-USER  TO W02-USE.
             ADD 501 TRM-PRN4 (WS-S6)
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
      MOVE 6   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
          IF NOT(PAR-CON-STM = "N" OR "Y")
              MOVE "N"   TO PAR-CON-STM.
    IF NOT(PAR-CON-TYPE = "A" OR "S")
        MOVE "S"   TO PAR-CON-TYPE.
      MOVE PAR-CON-STM  TO W05-CON-STM.
      MOVE PAR-CON-TYPE  TO W05-CON-TYPE.
            MOVE 11   TO WS-PARKEY.
            MOVE 1   TO WS-SUB-2.

       ZA15-READ-TRANS.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE 1   TO WS-SUB-1.

       ZA20-TRAN-LOOP.
            MOVE PAR-T-CODE(WS-SUB-1)
     TO W00-TRAN-CODE(WS-SUB-2).
            MOVE PAR-E-DESC(WS-SUB-1)
     TO W00-E-DESC(WS-SUB-2).
            MOVE PAR-A-DESC(WS-SUB-1)
     TO W00-A-DESC(WS-SUB-2).
            MOVE PAR-ACTION(WS-SUB-1)
     TO W00-ACTION(WS-SUB-2).
            ADD 1   TO WS-SUB-2.
           IF WS-SUB-1 < 2
              ADD 1   TO WS-SUB-1
               GO TO ZA20-TRAN-LOOP.
           IF WS-PARKEY < 60
              ADD 1   TO WS-PARKEY
               GO TO ZA15-READ-TRANS.

       ZA25.
      MOVE 4   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-AGE  TO WS-AGE.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-COMPANY  TO W95-COMP.
            MOVE PAR-DMY  TO W12-TODAY.
            MOVE PAR-YMD  TO W12-T-YMD.
           IF PAR-STMINV = "S" OR "X"
              MOVE "N"   TO WS-HEAD
           ELSE
              MOVE "Y"   TO WS-HEAD.
      *    *************************************************************
      *    ****   G E T   D E B T O R M O N T H - E N D   D A Y   ****
      *    *************************************************************
            MOVE 1   TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
      *
      *    ****    S E T   U P   A G E   D A T E S
      *
      MOVE W12-T-YMD  TO W22-DTE2.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ The month end dates are stored in the Control file in the ³
      *    ³ first record as Cur, 30d, 60d, 90d and 120d and the next  ³
      *    ³ routine is used since Ver 8.11.06.          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      MOVE DEB-CUR-DTE  TO W30-CUR.
      MOVE DEB-30D-DTE  TO W30-30.
      MOVE DEB-60D-DTE  TO W30-60.
      MOVE DEB-90D-DTE  TO W30-90.
      MOVE DEB-120-DTE  TO W30-120.
      OPEN OUTPUT DBTCON.
      CLOSE DBTCON.
      OPEN I-O DBTCON.
             GO TO ZA999-EXIT.
      *    *************************************************************
      *    ****   F I L E   L I M I T E X C E E D E D
      *    (Config.sys Dos/Win/Windows 95/98/NT/2000)
      *    (Config.nt Windows XP)
      *    (System.ini Windows.me)
      *    *************************************************************
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
            DISPLAY "Check the FILES option in CONFIG.SYS (Dos,Windows
      -       "3/95/98/NT/2000)" AT 0908.
      DISPLAY "CONFIG.NT  (Windows XP)" AT 1034.
      DISPLAY "SYSTEM.INI (Windows ME)" AT 1134.
            DISPLAY "Press any key to continue" AT 1308.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      GO TO ZA205.
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
        MOVE "SupervisorÄ"
     TO WS-WRKHD
    ELSE
        MOVE "Workstation"
     TO WS-WRKHD
        MOVE LS-USER
     TO WS-WRKST.
       ZA200.
             DISPLAY "Files locked - Try later" AT 2312
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     " " WS-STATUS WITH FOREGROUND-COLOR Grey HIGHLIGHT.
            DISPLAY "Press any key" AT 5012
              WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT WS-OPTION AT 5026
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
       ZA205.
             EXIT PROGRAM.
       ZA999-EXIT.
             EXIT.
      /
       ZB000       SECTION 91.

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
           IF WS-F-ERROR = 2
               MOVE W02-NETWORK  TO WS-FILE
               MOVE WS-NETKEY    TO WS-KEY
           ELSE
           IF WS-F-ERROR = 5
               MOVE W02-DBTRAN   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE TRA-KEY      TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 6
               MOVE W02-DEBTOR   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 15
               MOVE WS-PARID     TO WS-FILE
               MOVE WS-PARKEY    TO WS-KEY
           ELSE
           IF WS-F-ERROR = 27
               MOVE W02-DEBMEM   TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE DME-KEY  TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 37
              MOVE W02-SHARED  TO WS-FILE
              MOVE WS-SHARED  TO WS-KEY
           ELSE
           IF WS-F-ERROR = 55
              MOVE W02-DTHIST  TO WS-FILE
              MOVE ZERO  TO WS-KEY
        MOVE TRH-KEY  TO WS-KEYX
           ELSE
          IF WS-F-ERROR = 56
              MOVE W02-DBHIST  TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE DBH-ACNO  TO WS-KEYX.
           IF WS-STATUS = "10"
              MOVE "End of FILE"
     TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "22"
              MOVE "Duplicate record number"
     TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "23"
              MOVE "Invalid record number"
     TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "24"
              MOVE "DISK full"  TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "30"
               MOVE "DISK error" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "94"
              MOVE "FILE locked"
     TO WS-STAT-MESSAGE.
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
