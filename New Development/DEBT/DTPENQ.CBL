      $SET LINKCOUNT"586" GNT"DTPENQ.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******   ********  **    **   ******    *
      *   **    **     **     **    **  **        ***   **  **    **   *
      *   **    **     **     **    **  **        ****  **  **    **   *
      *   **    **     **     *******   ****      ** ** **  **    **   *
      *   **    **     **     **        **        **  ****  **   ***   *
      *   **    **     **     **        **        **   ***   ******    *
      *   *******      **     **        ********  **    **        **   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R ' S   E N Q U I R Y   P R O G R A M            *
      *                                                                *
      *       Version 9.00.80 - August 2015                            *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  May 2006 - New field included in Accounts Receivable for      *
      *             recording number of outstanding Jobs linked to the *
      *             to the account. Additional filler added for future *
      *             expansion.                                         *
      *                                                                *
      *  Feb 2007 - New file (DEBDEP/AR_REST.DAT) - Debtor sales re-   *
      *             strictions file has been included which will allow *
      *             for departments to be excluded or only selected    *
      *             departments allowed. This will allow the user the  *
      *             abillity to allow or bar sales of selected         *
      *             departments.                                       *
      *                                                                *
      *  Jan 2008 - New file (DEBALT): Debtor alternate index included *
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
      *  November 2008 - Allow for enquiry into a previous period.     *
      *                                                                *
      *  July 2012 - Added email address field - DEB-EMAIL             *
      *              (W40-EMAIL) PIC X(64)                             *
      *                                                                *
      *  Nov 2012 - Changed DEB-ADDRESS and DEB-PADDR from group       *
      *             fields with 70 characters to group fields          *
      *             containing DEB-ADD-L1/2/3/4 and DEB-PADD-L1/2/3/4  *
      *             respectively. Each of these new fields are defined *
      *             as PIC X(30). As new PC's and Servers contain huge *
      *             amounts of disk space this has been changed, which *
      *             also results in simplifying the code for handling  *
      *             addresses in Data capture, enquiries, reports etc. *
      *             This also removes the restriction on the number of *
      *             characters that an address may contain.            *
      *                                                                *
      *  Jan 2013 - Program DTPMEMO introduced to handle memo lookups  *
      *             reducing code in all programs and removing file    *
      *             DEBMEM from programs. Copy file 'DEBMEM.PRO' now   *
      *             copied into all programs that require lookups for  *
      *             memo/remarks on a Debtor account. The Debtor code/ *
      *             account number is moved to W80-ACCOUNT and then    *
      *             PERFORM DEBTOR-MEMO.                               *
      *                                                                *
      *  May 2013 - Added Cell phone number field DEB-C-TEL            *
      *             (W40-C-TEL) PIC X(14).                             *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     DTPENQ.
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

       COPY "DEBDEP.SL".

       COPY "DEBTOR.SL".

       COPY "DEPART.SL".

       COPY "INVOIC.SL".

       COPY "PARAM.SL".

       COPY "SALE.SL".

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

       COPY "DEBDEP.FDE".

       COPY "DEBTOR.FDE".

       COPY "DEPART.FDE".

       COPY "INVOIC.FDE".

       COPY "PARAM.FDE".

       COPY "SALE.FDE".

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
       77  WS-CHECK                PIC  X(18)    VALUE "aeWlimemnomLalismJ".
       77  WS-SUB                  PIC  9(04)    COMP-5.
       77  WS-S                    PIC  9(04)    COMP-5.
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S5                   PIC  9(04)    COMP-5.
       77  WS-S6                   PIC  9(04)    COMP-5.
       77  WS-S7                   PIC  9(04)    COMP-5.
       77  WS-S8                   PIC  9(04)    COMP-5.
       77  WS-S9                   PIC  9(04)    COMP-5.
       77  WS-IXS1                 PIC  9(04)    COMP-5.
       77  WS-ENQPOS               PIC  9(06)    COMP-5.
       77  WS-ENQEND               PIC  9(06)    COMP-5.
       77  WS-NETKEY               PIC  9(06)    COMP-5.
       77  WS-PARKEY               PIC  9(06)    COMP-5.
       77  WS-SALKEY               PIC  9(04)    COMP-5.
       77  WS-PAGE                 PIC  9(04)    COMP-5.
       77  WS-NORM                 PIC  X(04).
       77  WS-COND                 PIC  X(04).
       77  WS-EXPP                 PIC  X(04).
       77  WS-ECAN                 PIC  X(04).
       77  WS-8LPI                 PIC  X(04).
       77  WS-6LPI                 PIC  X(04).
       77  WS-10CPI                PIC  X(04).
       77  WS-12CPI                PIC  X(04).
       77  WS-17CPI                PIC  X(04).
       77  WS-DBLP                 PIC  X(04).
       77  WS-DCAN                 PIC  X(04).
       77  WS-KEYSTR               PIC  9(06)    COMP-5.
       77  WS-LINES                PIC  9(02)    VALUE 0.
       77  WS-FIRST                PIC  9(03)    VALUE 0.
       77  WS-LAST                 PIC  9(03)    VALUE 0.
       77  WS-LASTINV              PIC  9(04)    COMP-5.
       77  WS-LINE                 PIC  9(04)    COMP-5.
       77  WS-L1                   PIC  9(04)    COMP-5.
       77  WS-INSTR                PIC  X(01).
       77  WS-OPTION               PIC  X(01).
       77  WS-DTPNAME              PIC  X(01)    VALUE "N".
       77  WS-PRC                  PIC  X(01).
       77  WS-PROF                 PIC  X(01).
           88  PROF-INV                          VALUE "Y".
       77  WS-TYP                  PIC  X(01).
       77  WS-SEQ                  PIC  X(01).
       77  WS-ERROR                PIC  9(01)    VALUE ZERO.
       77  WS-ERR1                 PIC  X(22)    VALUE "Invalid Account Number".
       77  WS-ERR2                 PIC  X(09)    VALUE "No Record".
       77  WS-A                    PIC  X(19)    VALUE "Any key to continue".
       77  WS-E                    PIC  X(15)    VALUE "**END** - ENTER".
       77  WS-CR                   PIC  X(01).
       77  PRG-NAME                PIC  X(12)    VALUE SPACES.
       77  PRG-DEBT-LUP            PIC  X(12)    VALUE "DTP\DTPLOOK".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  TODAY-YYMMDD            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.

      *    здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Used by calling program (WORKING-STORAGE) and called program Ё
      *    Ё (LINKAGE SECTION) for Main screen layout and headings.       Ё
      *    юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *       C = Clear comment line (50)
      *       E = Clear lines (any line or lines between 5 and 46)
      *       H = Change heading
      *       S = Clear the screen and display headings
      *
           03  CRT-TYPE            PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START           PIC  9(02) VALUE 5.
           03  CRT-END             PIC  9(02) VALUE 46.
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15) VALUE "DTPENQ".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "ACCOUNTS RECEIVABLE - ENQUIRY (CURRENT)".
      *
      *    Company Name
      *
           03  CRT-COMPANY         PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-TERMINAL.
               05  CRT-WRKHD       PIC  X(11).
               05  CRT-WRKST       PIC  X(03).

       COPY "WS.WS".

       01  WS-HELP.
           03  WS-MODULE           PIC  X(03) VALUE "DTP".
           03  WS-PROG             PIC  X(03) VALUE "ENQ".

       01  W00-TRAN-CODES.
           03  W00-T-VAT           PIC  9(09)V99 COMP-3.
           03  W00-E-DESC          PIC  X(12).
           03  W00-A-DESC          PIC  X(12).
           03  W00-ACTION          PIC  9(01).
           03  W00-T-CODE          PIC  9(02)    COMP-5.
           03  W00-T-GL            PIC  9(06)    COMP-3.
           03  W00-T-DAY           PIC  9(06)    COMP-5.
           03  W00-T-VAL           PIC S9(09)V99 COMP-3.
           03  W00-T-MTD           PIC  9(06)    COMP-5.
           03  W00-T-MTDV          PIC S9(09)V99 COMP-3.
           03  W00-T-YTD           PIC  9(06)    COMP-5.
           03  W00-T-YTDV          PIC S9(09)V99 COMP-3.

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

       01  W02-FILE-IDS.
           03  W02-PRINTER.
               05  W02-REPORT      PIC  X(07) VALUE "DTPENQ.".
               05  W02-USER        PIC  X(05) VALUE SPACES.

       COPY "W05.VAT".

       COPY "W10.SAL".

       01  W10-REF.
           03  W10-NUM             PIC  9(08).

       COPY "DateVariables.cpy".

       01  W15-ALLOCATIONS.
           03  W15-DETAIL.
               05  W15-TRAN        OCCURS 34.
                   07  W15-VALUE   PIC  Z(06)9.99- BLANK WHEN ZERO.
                   07  W15-ALLOC   PIC  Z(06)9.99- BLANK WHEN ZERO.
                   07  W15-REC     PIC  X(08).
           03  W15-DISPLAY         REDEFINES W15-DETAIL.
               05  W15-BAL1        PIC  X(11).
               05  W15-ALL1        PIC  X(11).
               05  W15-REC1        PIC  X(08).
               05  W15-BAL2        PIC  X(11).
               05  W15-ALL2        PIC  X(11).
               05  W15-REC2        PIC  X(08).
               05  W15-BAL3        PIC  X(11).
               05  W15-ALL3        PIC  X(11).
               05  W15-REC3        PIC  X(08).
               05  W15-BAL4        PIC  X(11).
               05  W15-ALL4        PIC  X(11).
               05  W15-REC4        PIC  X(08).
               05  W15-BAL5        PIC  X(11).
               05  W15-ALL5        PIC  X(11).
               05  W15-REC5        PIC  X(08).
               05  W15-BAL6        PIC  X(11).
               05  W15-ALL6        PIC  X(11).
               05  W15-REC6        PIC  X(08).
               05  W15-BAL7        PIC  X(11).
               05  W15-ALL7        PIC  X(11).
               05  W15-REC7        PIC  X(08).
               05  W15-BAL8        PIC  X(11).
               05  W15-ALL8        PIC  X(11).
               05  W15-REC8        PIC  X(08).
               05  W15-BAL9        PIC  X(11).
               05  W15-ALL9        PIC  X(11).
               05  W15-REC9        PIC  X(08).
               05  W15-BAL10       PIC  X(11).
               05  W15-ALL10       PIC  X(11).
               05  W15-REC10       PIC  X(08).
               05  W15-BAL11       PIC  X(11).
               05  W15-ALL11       PIC  X(11).
               05  W15-REC11       PIC  X(08).
               05  W15-BAL12       PIC  X(11).
               05  W15-ALL12       PIC  X(11).
               05  W15-REC12       PIC  X(08).
               05  W15-BAL13       PIC  X(11).
               05  W15-ALL13       PIC  X(11).
               05  W15-REC13       PIC  X(08).
               05  W15-BAL14       PIC  X(11).
               05  W15-ALL14       PIC  X(11).
               05  W15-REC14       PIC  X(08).
               05  W15-BAL15       PIC  X(11).
               05  W15-ALL15       PIC  X(11).
               05  W15-REC15       PIC  X(08).
               05  W15-BAL16       PIC  X(11).
               05  W15-ALL16       PIC  X(11).
               05  W15-REC16       PIC  X(08).
               05  W15-BAL17       PIC  X(11).
               05  W15-ALL17       PIC  X(11).
               05  W15-REC17       PIC  X(08).
               05  W15-BAL18       PIC  X(11).
               05  W15-ALL18       PIC  X(11).
               05  W15-REC18       PIC  X(08).
               05  W15-BAL19       PIC  X(11).
               05  W15-ALL19       PIC  X(11).
               05  W15-REC19       PIC  X(08).
               05  W15-BAL20       PIC  X(11).
               05  W15-ALL20       PIC  X(11).
               05  W15-REC20       PIC  X(08).
               05  W15-BAL21       PIC  X(11).
               05  W15-ALL21       PIC  X(11).
               05  W15-REC21       PIC  X(08).
               05  W15-BAL22       PIC  X(11).
               05  W15-ALL22       PIC  X(11).
               05  W15-REC22       PIC  X(08).
               05  W15-BAL23       PIC  X(11).
               05  W15-ALL23       PIC  X(11).
               05  W15-REC23       PIC  X(08).
               05  W15-BAL24       PIC  X(11).
               05  W15-ALL24       PIC  X(11).
               05  W15-REC24       PIC  X(08).
               05  W15-BAL25       PIC  X(11).
               05  W15-ALL25       PIC  X(11).
               05  W15-REC25       PIC  X(08).
               05  W15-BAL26       PIC  X(11).
               05  W15-ALL26       PIC  X(11).
               05  W15-REC26       PIC  X(08).
               05  W15-BAL27       PIC  X(11).
               05  W15-ALL27       PIC  X(11).
               05  W15-REC27       PIC  X(08).
               05  W15-BAL28       PIC  X(11).
               05  W15-ALL28       PIC  X(11).
               05  W15-REC28       PIC  X(08).
               05  W15-BAL29       PIC  X(11).
               05  W15-ALL29       PIC  X(11).
               05  W15-REC29       PIC  X(08).
               05  W15-BAL30       PIC  X(11).
               05  W15-ALL30       PIC  X(11).
               05  W15-REC30       PIC  X(08).
               05  W15-BAL31       PIC  X(11).
               05  W15-ALL31       PIC  X(11).
               05  W15-REC31       PIC  X(08).
               05  W15-BAL32       PIC  X(11).
               05  W15-ALL32       PIC  X(11).
               05  W15-REC32       PIC  X(08).
               05  W15-BAL33       PIC  X(11).
               05  W15-ALL33       PIC  X(11).
               05  W15-REC33       PIC  X(08).
               05  W15-BAL34       PIC  X(11).
               05  W15-ALL34       PIC  X(11).
               05  W15-REC34       PIC  X(08).
      *        05  W15-BAL35       PIC  X(11).
      *        05  W15-ALL35       PIC  X(11).
      *        05  W15-REC35       PIC  X(08).

       01  W25-CALCS.
           03  W25-RESULT          PIC  9(05)V99.
           03  W25-RESULT1         REDEFINES W25-RESULT.
               05  W25-WHOLE       PIC  9(05).
               05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2         REDEFINES W25-RESULT.
               05  W25-KEY         PIC  9(04).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).
           03  W25-TOTAL           PIC S9(09)V99 COMP-3.
           03  W25-VALUE           PIC S9(09)V99 COMP-3.

       01  W30-KEY.
           03  W30-AC              PIC  X(06).
           03  W30-DATE            PIC  9(08)    COMP.
           03  W30-CODE            PIC  9(02).
           03  W30-REF             PIC  X(08).

       COPY "W40.DBT".

       01  W40-BALANCES.
           03  W40-A-MTD           PIC S9(09)V99 COMP-3.
           03  W40-A-CUR           PIC S9(09)V99 COMP-3.
           03  W40-A-30            PIC S9(09)V99 COMP-3.
           03  W40-A-60            PIC S9(09)V99 COMP-3.
           03  W40-A-90            PIC S9(09)V99 COMP-3.
           03  W40-A-120           PIC S9(09)V99 COMP-3.
           03  W40-A-INT           PIC S9(09)V99 COMP-3.
           03  W40-D-MTD           PIC S9(09)V99 COMP-3.
           03  W40-D-CUR           PIC S9(09)V99 COMP-3.
           03  W40-D-30            PIC S9(09)V99 COMP-3.
           03  W40-D-60            PIC S9(09)V99 COMP-3.
           03  W40-D-90            PIC S9(09)V99 COMP-3.
           03  W40-D-120           PIC S9(09)V99 COMP-3.
           03  W40-D-INT           PIC S9(09)V99 COMP-3.
           03  W40-T-MTD           PIC S9(09)V99 COMP-3.
           03  W40-T-CUR           PIC S9(09)V99 COMP-3.
           03  W40-T-30            PIC S9(09)V99 COMP-3.
           03  W40-T-60            PIC S9(09)V99 COMP-3.
           03  W40-T-90            PIC S9(09)V99 COMP-3.
           03  W40-T-120           PIC S9(09)V99 COMP-3.
           03  W40-T-INT           PIC S9(09)V99 COMP-3.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

      *COPY "W50.WS".

       01  W80-ACCOUNT.
           03  W80-ACNO            PIC  X(06).
           03  W80-TYPE            PIC  9(01).

       COPY "W90.DBT".
           03  W90-CDESC           PIC  X(30).
           03  W90-SMAN            PIC  X(30).
           03  W90-TRMS            PIC  X(12).
           03  W90-TYPE            PIC  X(12).
           03  W90-PRICE           PIC  X(12).

       01  W95-STM.
           03  W95-COMP            PIC  X(40).
           03  W95-V2.
               05  W95-B           PIC  Z(06)9.99- BLANK WHEN ZERO.
           03  W95-D.
               05  W95-DTE         PIC  Z9/99/9999.
           03  W95-POST            PIC  X(08).

       01  W100-EDIT.
           03  W100-LBAL           PIC  Z(06)9.99-.
           03  W100-LARR           PIC  Z(06)9.99.
           03  W100-BAL            PIC  Z(08)9.99-.
           03  W100-CUR            PIC  Z(08)9.99-.
           03  W100-30             PIC  Z(08)9.99-.
           03  W100-60             PIC  Z(08)9.99-.
           03  W100-90             PIC  Z(08)9.99-.
           03  W100-120            PIC  Z(08)9.99-.
           03  W100-INT            PIC  Z(08)9.99.
           03  W100-VAL            PIC  Z(08)9.99-.
           03  W100-V2.
               05  W100-S6V2       PIC  Z(05)9.99-.
           03  W100-V3             REDEFINES W100-V2.
               05  W100-PRICE      PIC  X(09).
               05  FILLER          PIC  X(01).
           03  W100-V4             REDEFINES W100-V2.
               05  W100-EXT        PIC  Z(06)9.99.
           03  W100-V5             REDEFINES W100-V2.
               05  W100-QNT        PIC  Z(05)9.99-.
           03  W100-CRL            PIC  Z(06)9.
           03  W100-KEY            PIC  Z(05)9.

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

       01  S08.
           03  LINE  6 COLUMN  2 VALUE "Account          Name                                           ╟╟Language╟╟╟╟".
           03  LINE  7 COLUMN  2 VALUE "Address ".
           03  LINE  8 COLUMN  2 VALUE "eMail   ".
           03  LINE  9 COLUMN  2 VALUE "Phone No                         Contact  ".
           03  LINE 10 COLUMN  2 VALUE "Fax                              Purchases  YTD              Prev ".
           03  LINE 11 COLUMN  5 VALUE    "Date    Description  Ref No        Value".

       01  S08D.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
               05  LINE  6 COLUMN 25 PIC  X(40) FROM W40-DSP-NAME.
               05          COLUMN 78 PIC  X(01) FROM W40-LANG.
               05  LINE  7 COLUMN 11 PIC  X(60) FROM W40-DSP-ADDRESS.
               05          COLUMN 72 PIC  X(08) FROM W40-POST.
               05  LINE  9 COLUMN 11 PIC  X(14) FROM W40-TEL.
               05          COLUMN 46 PIC  X(30) FROM W40-CONTACT.
               05  LINE 10 COLUMN 11 PIC  X(14) FROM W40-FAX.
               05          COLUMN 50 PIC  Z(06)9.99- BLANK WHEN ZERO FROM W40-PURYTD.
               05          COLUMN 68 PIC  Z(06)9.99- BLANK WHEN ZERO FROM W40-PURPRV.

       01  S08T.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
               05  COLUMN 02 PIC Z9/99/9999                 FROM W22-DTE3.
               05  COLUMN 13 PIC X(12)                      FROM W00-E-DESC.
               05  COLUMN 26 PIC X(08)                      FROM TRA-REF.
               05  COLUMN 35 PIC Z(06)9.99- BLANK WHEN ZERO FROM TRA-VALUE.

       01  S09.
           03  LINE 11 COLUMN 48 VALUE                                                "Ё  Opening balance ".
           03  LINE 12 COLUMN 48 VALUE                                                "Ё  Debits          ".
           03  LINE 13 COLUMN 48 VALUE                                                "Ё  Credits         ".
           03  LINE 14 COLUMN 48 VALUE                                                "Ё  Closing balance ".
           03  LINE 15 COLUMN 48 VALUE                                                "Ё".
           03  LINE 16 COLUMN 48 VALUE                                                "Ё  Last active     ".
           03  LINE 17 COLUMN 48 VALUE                                                "Ё  Last payment    ".
           03  LINE 18 COLUMN 48 VALUE                                                "Ё  Age Analysis    ".
           03  LINE 19 COLUMN 48 VALUE                                                "Ё      Current     ".
           03  LINE 20 COLUMN 48 VALUE                                                "Ё      30 days     ".
           03  LINE 21 COLUMN 48 VALUE                                                "Ё      60 days     ".
           03  LINE 22 COLUMN 48 VALUE                                                "Ё      90 days     ".
           03  LINE 23 COLUMN 48 VALUE                                                "Ё      120 days +  ".
           03  LINE 24 COLUMN 48 VALUE                                                "Ё      Interest    ".
           03  LINE 25 COLUMN 48 VALUE                                                "Ё  Arrears         ".
           03  LINE 26 COLUMN 48 VALUE                                                "Ё  Credit limit    ".

       01  S09D.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
               05  LINE 11 COLUMN 68 PIC  Z(06)9.99- BLANK WHEN ZERO FROM W40-STM.
               05  LINE 12 COLUMN 68 PIC  Z(06)9.99- BLANK WHEN ZERO FROM W40-DEBT.
               05  LINE 13 COLUMN 68 PIC  Z(06)9.99- BLANK WHEN ZERO FROM W40-CRED.
               05  LINE 14 COLUMN 68 PIC  Z(06)9.99- BLANK WHEN ZERO FROM W40-BAL.
               05  LINE 15 COLUMN 68 PIC  Z9/99/9999                 FROM W22-DTE1.
               05  LINE 16 COLUMN 68 PIC  Z9/99/9999                 FROM W22-DTE3.
               05  LINE 19 COLUMN 68 PIC  Z(06)9.99- BLANK WHEN ZERO FROM W40-MTD.
               05  LINE 20 COLUMN 68 PIC  Z(06)9.99- BLANK WHEN ZERO FROM W40-30.
               05  LINE 21 COLUMN 68 PIC  Z(06)9.99- BLANK WHEN ZERO FROM W40-60.
               05  LINE 22 COLUMN 68 PIC  Z(06)9.99- BLANK WHEN ZERO FROM W40-90.
               05  LINE 23 COLUMN 68 PIC  Z(06)9.99- BLANK WHEN ZERO FROM W40-120.
               05  LINE 24 COLUMN 68 PIC  Z(06)9.99- BLANK WHEN ZERO FROM W40-INTEREST.
               05  LINE 25 COLUMN 68 PIC  Z(06)9.99- BLANK WHEN ZERO FROM W40-ARREARS.
               05  LINE 26 COLUMN 68 PIC  Z(06)9.99- BLANK WHEN ZERO FROM W40-CRL.
       
       01  S09A.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE 11 COLUMN 45 VALUE "  Allocated     Balance  Ref. No. ".
               05  LINE 12 COLUMN 45 VALUE "                                  ".
               05  LINE 13 COLUMN 45 VALUE "                                  ".
               05  LINE 14 COLUMN 45 VALUE "                                  ".
               05  LINE 15 COLUMN 45 VALUE "                                  ".
               05  LINE 16 COLUMN 45 VALUE "                                  ".
               05  LINE 17 COLUMN 45 VALUE "                                  ".
               05  LINE 18 COLUMN 45 VALUE "                                  ".
               05  LINE 19 COLUMN 45 VALUE "                                  ".
               05  LINE 20 COLUMN 45 VALUE "                                  ".
               05  LINE 21 COLUMN 45 VALUE "                                  ".
               05  LINE 22 COLUMN 45 VALUE "                                  ".
               05  LINE 23 COLUMN 45 VALUE "                                  ".
               05  LINE 24 COLUMN 45 VALUE "                                  ".
               05  LINE 25 COLUMN 45 VALUE "                                  ".
               05  LINE 26 COLUMN 45 VALUE "                                  ".
               05  LINE 27 COLUMN 45 VALUE "                                  ".
               05  LINE 28 COLUMN 45 VALUE "                                  ".
               05  LINE 29 COLUMN 45 VALUE "                                  ".
               05  LINE 30 COLUMN 45 VALUE "                                  ".
               05  LINE 31 COLUMN 45 VALUE "                                  ".
               05  LINE 32 COLUMN 45 VALUE "                                  ".
               05  LINE 33 COLUMN 45 VALUE "                                  ".
               05  LINE 34 COLUMN 45 VALUE "                                  ".
               05  LINE 35 COLUMN 45 VALUE "                                  ".
               05  LINE 36 COLUMN 45 VALUE "                                  ".
               05  LINE 37 COLUMN 45 VALUE "                                  ".
               05  LINE 38 COLUMN 45 VALUE "                                  ".
               05  LINE 39 COLUMN 45 VALUE "                                  ".
               05  LINE 40 COLUMN 45 VALUE "                                  ".
               05  LINE 41 COLUMN 45 VALUE "                                  ".
               05  LINE 42 COLUMN 45 VALUE "                                  ".
               05  LINE 43 COLUMN 45 VALUE "                                  ".
               05  LINE 44 COLUMN 45 VALUE "                                  ".
               05  LINE 45 COLUMN 45 VALUE "                                  ".
      *        05  LINE 46 COLUMN 45 VALUE "                                  ".
               05  LINE 12 COLUMN 46 PIC  X(11) FROM W15-ALL1.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL1.
               05          COLUMN 70 PIC  X(08) FROM W15-REC1.
               05  LINE 13 COLUMN 46 PIC  X(11) FROM W15-ALL2.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL2.
               05          COLUMN 70 PIC  X(08) FROM W15-REC2.
               05  LINE 14 COLUMN 46 PIC  X(11) FROM W15-ALL3.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL3.
               05          COLUMN 70 PIC  X(08) FROM W15-REC3.
               05  LINE 15 COLUMN 46 PIC  X(11) FROM W15-ALL4.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL4.
               05          COLUMN 70 PIC  X(08) FROM W15-REC4.
               05  LINE 16 COLUMN 46 PIC  X(11) FROM W15-ALL5.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL5.
               05          COLUMN 70 PIC  X(08) FROM W15-REC5.
               05  LINE 17 COLUMN 46 PIC  X(11) FROM W15-ALL6.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL6.
               05          COLUMN 70 PIC  X(08) FROM W15-REC6.
               05  LINE 18 COLUMN 46 PIC  X(11) FROM W15-ALL7.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL7.
               05          COLUMN 70 PIC  X(08) FROM W15-REC7.
               05  LINE 19 COLUMN 46 PIC  X(11) FROM W15-ALL8.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL8.
               05          COLUMN 70 PIC  X(08) FROM W15-REC8.
               05  LINE 20 COLUMN 46 PIC  X(11) FROM W15-ALL9.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL9.
               05          COLUMN 70 PIC  X(08) FROM W15-REC9.
               05  LINE 21 COLUMN 46 PIC  X(11) FROM W15-ALL10.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL10.
               05          COLUMN 70 PIC  X(08) FROM W15-REC10.
               05  LINE 22 COLUMN 46 PIC  X(11) FROM W15-ALL11.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL11.
               05          COLUMN 70 PIC  X(08) FROM W15-REC11.
               05  LINE 23 COLUMN 46 PIC  X(11) FROM W15-ALL12.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL12.
               05          COLUMN 70 PIC  X(08) FROM W15-REC12.
               05  LINE 24 COLUMN 46 PIC  X(11) FROM W15-ALL13.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL13.
               05          COLUMN 70 PIC  X(08) FROM W15-REC13.
               05  LINE 25 COLUMN 46 PIC  X(11) FROM W15-ALL14.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL14.
               05          COLUMN 70 PIC  X(08) FROM W15-REC14.
               05  LINE 26 COLUMN 46 PIC  X(11) FROM W15-ALL15.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL15.
               05          COLUMN 70 PIC  X(08) FROM W15-REC15.
               05  LINE 27 COLUMN 46 PIC  X(11) FROM W15-ALL16.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL16.
               05          COLUMN 70 PIC  X(08) FROM W15-REC16.
               05  LINE 28 COLUMN 46 PIC  X(11) FROM W15-ALL17.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL17.
               05          COLUMN 70 PIC  X(08) FROM W15-REC17.
               05  LINE 29 COLUMN 46 PIC  X(11) FROM W15-ALL18.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL18.
               05          COLUMN 70 PIC  X(08) FROM W15-REC18.
               05  LINE 30 COLUMN 46 PIC  X(11) FROM W15-ALL19.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL19.
               05          COLUMN 70 PIC  X(08) FROM W15-REC19.
               05  LINE 31 COLUMN 46 PIC  X(11) FROM W15-ALL20.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL20.
               05          COLUMN 70 PIC  X(08) FROM W15-REC20.
               05  LINE 32 COLUMN 46 PIC  X(11) FROM W15-ALL21.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL21.
               05          COLUMN 70 PIC  X(08) FROM W15-REC21.
               05  LINE 33 COLUMN 46 PIC  X(11) FROM W15-ALL22.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL22.
               05          COLUMN 70 PIC  X(08) FROM W15-REC22.
               05  LINE 34 COLUMN 46 PIC  X(11) FROM W15-ALL23.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL23.
               05          COLUMN 70 PIC  X(08) FROM W15-REC23.
               05  LINE 35 COLUMN 46 PIC  X(11) FROM W15-ALL24.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL24.
               05          COLUMN 70 PIC  X(08) FROM W15-REC24.
               05  LINE 36 COLUMN 46 PIC  X(11) FROM W15-ALL25.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL25.
               05          COLUMN 70 PIC  X(08) FROM W15-REC25.
               05  LINE 37 COLUMN 46 PIC  X(11) FROM W15-ALL26.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL26.
               05          COLUMN 70 PIC  X(08) FROM W15-REC26.
               05  LINE 38 COLUMN 46 PIC  X(11) FROM W15-ALL27.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL27.
               05          COLUMN 70 PIC  X(08) FROM W15-REC27.
               05  LINE 39 COLUMN 46 PIC  X(11) FROM W15-ALL28.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL28.
               05          COLUMN 70 PIC  X(08) FROM W15-REC28.
               05  LINE 40 COLUMN 46 PIC  X(11) FROM W15-ALL29.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL29.
               05          COLUMN 70 PIC  X(08) FROM W15-REC29.
               05  LINE 41 COLUMN 46 PIC  X(11) FROM W15-ALL30.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL30.
               05          COLUMN 70 PIC  X(08) FROM W15-REC30.
               05  LINE 42 COLUMN 46 PIC  X(11) FROM W15-ALL31.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL31.
               05          COLUMN 70 PIC  X(08) FROM W15-REC31.
               05  LINE 43 COLUMN 46 PIC  X(11) FROM W15-ALL32.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL32.
               05          COLUMN 70 PIC  X(08) FROM W15-REC32.
               05  LINE 44 COLUMN 46 PIC  X(11) FROM W15-ALL33.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL33.
               05          COLUMN 70 PIC  X(08) FROM W15-REC33.
               05  LINE 45 COLUMN 46 PIC  X(11) FROM W15-ALL34.
               05          COLUMN 58 PIC  X(11) FROM W15-BAL34.
               05          COLUMN 70 PIC  X(08) FROM W15-REC34.
      *        05  LINE 46 COLUMN 46 PIC  X(11) FROM W15-ALL35.
      *        05          COLUMN 58 PIC  X(11) FROM W15-BAL35.
      *        05          COLUMN 70 PIC  X(08) FROM W15-REC35.

       01  S09B.
           03  S09B-1.
               05  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
                   07  LINE 11 COLUMN 51 VALUE "здддддддддддддддддддддддддд©".
                   07  LINE 12 COLUMN 51 VALUE "Ё PURCHASES LAST 12 MONTHS Ё".
                   07  LINE 13 COLUMN 51 VALUE "Ё January   :              Ё".
                   07  LINE 14 COLUMN 51 VALUE "Ё February  :              Ё".
                   07  LINE 15 COLUMN 51 VALUE "Ё March     :              Ё".
                   07  LINE 16 COLUMN 51 VALUE "Ё April     :              Ё".
                   07  LINE 17 COLUMN 51 VALUE "Ё May       :              Ё".
                   07  LINE 18 COLUMN 51 VALUE "Ё June      :              Ё".
                   07  LINE 19 COLUMN 51 VALUE "Ё July      :              Ё".
                   07  LINE 20 COLUMN 51 VALUE "Ё August    :              Ё".
                   07  LINE 21 COLUMN 51 VALUE "Ё September :              Ё".
                   07  LINE 22 COLUMN 51 VALUE "Ё October   :              Ё".
                   07  LINE 23 COLUMN 51 VALUE "Ё November  :              Ё".
                   07  LINE 24 COLUMN 51 VALUE "Ё December  :              Ё".
                   07  LINE 25 COLUMN 51 VALUE "юдддддддддддддддддддддддддды".
 
           03  S09B-2.
               05  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
                   07  LINE 13 COLUMN 65 PIC  Z(08)9.99- FROM W40-MTHPUR(1).
                   07  LINE 14 COLUMN 65 PIC  Z(08)9.99- FROM W40-MTHPUR(2).
                   07  LINE 15 COLUMN 65 PIC  Z(08)9.99- FROM W40-MTHPUR(3).
                   07  LINE 16 COLUMN 65 PIC  Z(08)9.99- FROM W40-MTHPUR(4).
                   07  LINE 17 COLUMN 65 PIC  Z(08)9.99- FROM W40-MTHPUR(5).
                   07  LINE 18 COLUMN 65 PIC  Z(08)9.99- FROM W40-MTHPUR(6).
                   07  LINE 19 COLUMN 65 PIC  Z(08)9.99- FROM W40-MTHPUR(7).
                   07  LINE 20 COLUMN 65 PIC  Z(08)9.99- FROM W40-MTHPUR(8).
                   07  LINE 21 COLUMN 65 PIC  Z(08)9.99- FROM W40-MTHPUR(9).
                   07  LINE 22 COLUMN 65 PIC  Z(08)9.99- FROM W40-MTHPUR(10).
                   07  LINE 23 COLUMN 65 PIC  Z(08)9.99- FROM W40-MTHPUR(11).
                   07  LINE 24 COLUMN 65 PIC  Z(08)9.99- FROM W40-MTHPUR(12).

       01  S10.
           03  BACKGROUND-COLOR Green FOREGROUND-COLOR Brown HIGHLIGHT.
               05  LINE 12 COLUMN  5 VALUE "здддддддддддддддддддддддддддддддддддддддддддддддддд©".
               05  LINE 13 COLUMN  5 VALUE "Ё Delivery Address : ".
               05          COLUMN 25                                PIC  X(30) USING W40-PADD1    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 55 VALUE                                                   " Ё".
               05  LINE 14 COLUMN  5 VALUE "Ё                    ".
               05          COLUMN 25                                PIC  X(30) USING W40-PADD2    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 55 VALUE                                                  " Ё".
               05  LINE 15 COLUMN  5 VALUE "Ё                    ".
               05          COLUMN 25                                PIC  X(30) USING W40-PADD3    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 55 VALUE                                                  " Ё".
               05  LINE 16 COLUMN  5 VALUE "Ё                    ".
               05          COLUMN 25                                PIC  X(30) USING W40-PADD4    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 55 VALUE                                                  " Ё".
               05  LINE 17 COLUMN  5 VALUE "Ё Postal Code        ".
               05          COLUMN 25                                PIC  X(08) USING W40-PPOST    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 33 VALUE                             "  Jobs outstanding ".
               05          COLUMN 52                                PIC  ZZZ   USING W40-JOBS     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 55 VALUE                                                  " Ё".
               05  LINE 18 COLUMN  5 VALUE "Ё Category       ".
               05          COLUMN 22                                PIC  99B   USING W40-CAT      FOREGROUND-COLOR Black.
               05          COLUMN 25                                PIC  X(30) USING W90-CDESC    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 55 VALUE                                                  " Ё".
               05  LINE 19 COLUMN  5 VALUE "Ё Terms          ".
               05          COLUMN 22                                PIC  X(03) USING W40-TERMS    FOREGROUND-COLOR Black.
               05          COLUMN 25                                PIC  X(12) USING W90-TRMS     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 37 VALUE                                "                   Ё".
               05  LINE 20 COLUMN  5 VALUE "Ё Price Code     ".
               05          COLUMN 22                                PIC  X(03) USING W40-PRICE    FOREGROUND-COLOR Black.
               05          COLUMN 25                                PIC  X(12) USING W90-PRICE    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 37 VALUE                                "                   Ё".
               05  LINE 21 COLUMN  5 VALUE "Ё Sales Person   ".
               05          COLUMN 22                                PIC  99B   USING W40-SMAN     FOREGROUND-COLOR Black.
               05          COLUMN 25                                PIC  X(30) USING W90-SMAN     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 55 VALUE                                                  " Ё".
               05  LINE 22 COLUMN  5 VALUE "Ё Invoice Type   ".
               05          COLUMN 22                                PIC  X(03) USING W40-INV-TYPE FOREGROUND-COLOR Black.
               05          COLUMN 25                                PIC  X(12) USING W90-TYPE     FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 37 VALUE                                "                   Ё".
               05  LINE 23 COLUMN  5 VALUE "юдддддддддддддддддддддддддддддддддддддддддддддддддды".

       COPY "INVOICE.CRT".

       01  S26-REF.
           03  LINE 16 COLUMN 69 PIC Z(08) USING W10-NUM       FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.

       01  S26-QNT.
           03          COLUMN 51 PIC Z(04)9.99 USING W10-QUANT FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Black   AUTO.

       01  S26-PRC.
           03          COLUMN 60 PIC Z(05)9.99 USING W10-SELL  FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Black   AUTO.

       01  S027-CNOTE.
           03  LINE  5 COLUMN 35 VALUE "CREDIT NOTE"           FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE  7 COLUMN 61 VALUE " Inv. Date".

       01  S36.
           03  PIC X(06) USING W80-ACNO                        FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.

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
       AA000-MAIN        SECTION.
       AA000-INIT.
             PERFORM ZA000-INIT.
             PERFORM CD000.
             CLOSE SALE DEBDEP.

       AA49.
             EXIT PROGRAM.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё This routine is used to call the Debtor / Ac's Receivable Ё
      *    Ё lookup program and cancel it when control is returned.    Ё
      *    Ё The  relevant  error  message  will be  displayed  if  an Ё
      *    Ё exception occurs. The usual  parameters are passed to the Ё
      *    Ё lookup program as well W40-COMPANY which will contain the Ё
      *    Ё details of the account that has been selected when controlЁ
      *    Ё is returned to the calling program.                       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       AA50-LOOKUP     SECTION.
       AA50.
             PERFORM SAVE-SCREEN.
             MOVE 4                  TO W44-FUNCTION.
             PERFORM SCREEN-CONTENTS.
             MOVE PRG-DEBT-LUP       TO PRG-NAME.
             CALL PRG-NAME USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS W40-COMPANY
                  ON EXCEPTION GO TO AA55
             END-CALL.
             CANCEL PRG-NAME.
             MOVE W40-ACNO           TO W80-ACNO.
             GO TO AA58.

       AA55.
             MOVE SPACE              TO WS-ERR-MES.
             STRING "Program- " DELIMITED SIZE PRG-NAME DELIMITED " " " not on disk, press ANY key" DELIMITED SIZE INTO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.

       AA58.
             PERFORM RESTORE-SCREEN.

       AA99.
             EXIT.
             
       COPY "AA900.ALM".

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                       DEBTOR-MEMO                         Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё The account number of the Debtor record for which the MEMOЁ
      *    Ё details are to be displayed must be in W80-ACNO.          Ё
      *    Ё PERFORM DEBTOR-MEMO.                                      Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "DEBMEM.PRO".

      *    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *           S C R E E N ,   K E Y B O A R D   &   M O U S E
      *    *************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё      SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3       Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё                      SCREEN-SHADOW                        Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To routine is used to display a shadow down the right and Ё
      *    Ё along the bottom of a pop-up box. The parameters that are Ё
      *    Ё required:                                                 Ё
      *    Ё          SHADE-ROW   - Top line of the box.               Ё
      *    Ё          SHADE-COL   - Left line of box.                  Ё
      *    Ё          SHADE-WIDTH - Width of the box.                  Ё
      *    Ё          SHADE-LINES - Height of box.                     Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё                      CHECK-CORRECT                        Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё This routine displays a pop-up window with the message    Ё
      *    Ё           "Press Y if correct - N if incorrect"           Ё
      *    Ё                                                           Ё
      *    Ё The response is returned in WS-OPTION (in upper case).    Ё
      *    Ё                                                           Ё
      *    Ё To start with the pop-up window higher or lower than row  Ё
      *    Ё 20 (default); move another value to WS-MES-LINE.          Ё
      *    *************************************************************
      *            T H I S   R O U T I N E   I S   U S E D   T O
      *       D I S P L A Y   I N F O R M A T I O N   M E S S A G E S
      *    *************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Move the message to WS-DSP-MES and the top line of the   Ё
      *    Ё  message box to WS-MES-LINE. If WS-MES-LINE is zero the   Ё
      *    Ё  default will be 20.                                      Ё
      *    Ё                  PERFORM DISPLAY-MESSAGE                  Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    ****   T H I S   R O U T I N E   I S   U S E D   T O
      *            D I S P L A Y   E R R O R   M E S S A G E S
      *    *************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                      ERROR-MESSAGE                        Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To display the error message higher or lower (default is  Ё
      *    Ё line 20) Move the line number which must be used as the   Ё
      *    Ё top line to WS-MES-LINE. The message to be displayed must Ё
      *    Ё be in WS-ERR-MES. PERFORM ERROR-MESSAGE.                  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "FUNCTION.CRT".

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                      OPT-MESSAGE                          Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё This routine is used to allow the OPERATOR to respond to  Ё
      *    Ё a request for an option, so that the correct action can   Ё
      *    Ё be performed by the program. The routine will display the Ё
      *    Ё message in a pop-up window and allow the OPERATOR to      Ё
      *    Ё respond to the 'question'.                                Ё
      *    Ё                                                           Ё
      *    Ё The option request must be placed in WS-OPT-MES and may   Ё
      *    Ё not exceed 48 characters. The message will be centred in  Ё
      *    Ё the window. An example of a message request follows:      Ё
      *    Ё                                                           Ё
      *    Ё   MOVE "Print transactions (Y/N) [ ]" TO WS-OPT-MES.      Ё
      *    Ё   PERFORM OPT-MESSAGE.                                    Ё
      *    Ё                                                           Ё
      *    Ё This would be displayed as:                               Ё
      *    Ё      здддддддддддддддддддддддддддддддддддддддддддддддд©   Ё
      *    Ё      Ё          Print transactions (Y/N) [ ]          Ё   Ё
      *    Ё      юдддддддддддддддддддддддддддддддддддддддддддддддды   Ё
      *    Ё                                                           Ё
      *    Ё The response is returned in WS-OPTION (in upper case).    Ё
      *    Ё                                                           Ё
      *    Ё The system will display the message box with the top line Ё
      *    Ё at line as the value of WS-MES-LINE. If WS-MES-LINE is    Ё
      *    Ё zero the default will be 20.                              Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "OPTION.CRT".

       COPY "LOCKED.REC".

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё           ERASE SCREEN FROM SPECIFIED LOCATION            Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё Uses CRT-START as starting line and increases and CRT-END Ё
      *    Ё as the ending line. The screen is cleared with Column 1   Ё
      *    Ё and 80 containing "Ё" and columns 2 to 79 containing      Ё
      *    Ё spaces.                                                   Ё
      *    Ё                                                           Ё
      *    Ё  eg.                                                      Ё
      *    Ё     MOVE 8              TO CRT-START.                     Ё
      *    Ё     MOVE 28             TO CRT-END.                       Ё
      *    Ё     PERFORM ERASE-SCREEN.                                 Ё
      *    Ё                                                           Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё             CLEAR MESSAGE LINE (Line 50)                  Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё                                                           Ё
      *    Ё     PERFORM CLEAR-L50                                     Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё             CHANGE THE TIME ON THE SCREEN                 Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё                                                           Ё
      *    Ё     PERFORM CHANGE-TIME.                                  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "CLEAR.CRT".

      /    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё           R E A D   F I L E S   R O U T I N E S           Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       AC000            SECTION.

       COPY "CONTROL.RD".

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Special start for displaying transactions descending Seq. Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       START-AT-DTRN-KEY-DESC.
             START DBTRAN KEY < TRA-KEY.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
               GO TO READ-DBTRAN-EXIT.
           IF WS-STATUS = "23"
               MOVE 5            TO WS-F-ERROR
               GO TO READ-DBTRAN-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 5            TO WS-F-ERROR
               PERFORM READ-ERROR.
             GO TO READ-DBTRAN-EXIT.

       COPY "DBTRAN.RD".

      *COPY "DEBALT.RD".

       COPY "DEBDEP.RD".

       COPY "DEBTOR.RD".

       COPY "DEPART.RD".

       COPY "INVOIC.RD".

       COPY "PARAM.RD".

       COPY "SALE.RD".

      /    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    ЁR E W R I T E   &   W R I T E   F I L E S   R O U T I N E SЁ
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       AD000-WRITE        SECTION.

       COPY "SALE.WR".

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
             ADD 21 W90-CODE         GIVING W90-RESULT.
             DIVIDE W90-RESULT BY 2  GIVING W90-TRNKEY REMAINDER W90-TS.
             ADD 1                   TO W90-TS.

       GET-DEBTOR-TRAN-CODE.
             MOVE W90-TRNKEY         TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.

       STORE-DEBTOR-TRAN-DESC.
            MOVE PAR-E-DESC(W90-TS) TO W00-E-DESC.
            MOVE PAR-A-DESC(W90-TS) TO W00-A-DESC.
            MOVE PAR-ACTION(W90-TS) TO W00-ACTION.
            MOVE PAR-T-CODE(W90-TS) TO W00-T-CODE.
            MOVE PAR-T-GL(W90-TS)   TO W00-T-GL.

       STORE-DEBTOR-TRAN-STATS.
            MOVE PAR-T-VAT(W90-TS)  TO W00-T-VAT.
            MOVE PAR-T-DAY(W90-TS)  TO W00-T-DAY.
            MOVE PAR-T-VAL(W90-TS)  TO W00-T-VAL.
            MOVE PAR-T-MTD(W90-TS)  TO W00-T-MTD.
            MOVE PAR-T-MTDV(W90-TS) TO W00-T-MTDV.
            MOVE PAR-T-YTD(W90-TS)  TO W00-T-YTD.
            MOVE PAR-T-YTDV(W90-TS) TO W00-T-YTDV.

       GET-DEBTOR-TRAN-EXIT.
             EXIT.

       GET-DEBTOR-TRAN-DESC SECTION.

       GET-DEBT-TRAN-DESC.
             PERFORM GET-DEBTOR-TRAN.
             MOVE W90-TRNKEY         TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             PERFORM STORE-DEBTOR-TRAN-DESC.

       GET-DEBT-TRAN-DESC-EXIT.
             EXIT.

       COPY "APAC.HLP".

      *
      *    ****    Read debtor record - using account number.
      *
       CA155-GET-DEBTOR  SECTION 2.
       CA155.
             MOVE W80-ACNO           TO DEB-ACNO.
             PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               MOVE 1                TO WS-ERROR
           ELSE
           IF DEB-ACNO NOT = W80-ACNO
               MOVE 2                TO WS-ERROR
           ELSE
               MOVE 0                TO WS-ERROR
               MOVE DEB-TYPE         TO W80-TYPE.
           IF DEB-CONAC = LOW-VALUES
               MOVE SPACES           TO DEB-CONAC.

       CA160-EXIT.
             EXIT.
      *
      *    ****    D E B T O R   E N Q U I R Y
      *
       CD000   SECTION 5.
       CD00.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
             DISPLAY S08.

       CD02.
             DISPLAY S09.

       CD03.
             MOVE SPACES             TO W80-ACNO.

       CD005.
             DISPLAY "F1"                                                             AT 5002 WITH FOREGROUND-COLOR Brown HIGHLIGHT
                       "=Help,"                                                               WITH FOREGROUND-COLOR Blue
                             "F2"                                                             WITH FOREGROUND-COLOR Brown HIGHLIGHT
                               " to do Debtor Lookup,"                                        WITH FOREGROUND-COLOR Blue
                                                    "F9"                                      WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                                      " Debtor Lookup (Any Word),"            WITH FOREGROUND-COLOR Blue
                                                                                "Esc"         WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                                                                   " to exit" WITH FOREGROUND-COLOR Blue.
             ACCEPT S36 AT 0611.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE SPACES     TO W80-ACNO
                                 GO TO CD999
                 WHEN F1-KEY     PERFORM HELP-ROUTINE
                 WHEN F2-KEY     MOVE SPACE      TO WS-TYP
                                 PERFORM AA50
                 WHEN F9-KEY     MOVE "A"        TO WS-TYP
                                 PERFORM AA50
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO CD005
               END-EVALUATE
               DISPLAY S36 AT 0611
               IF W80-ACNO = SPACES
                   GO TO CD005.
             PERFORM CLEAR-L50.
           IF W80-ACNO = SPACES
               GO TO CD999.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W80-ACNO BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             MOVE W80-ACNO           TO DEB-ACNO.
             PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR = 2
               MOVE WS-ERR1          TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
              GO TO CD005.
           IF WS-ERROR = 1
               MOVE WS-ERR2          TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CD005.
             UNLOCK DEBTOR.
      *
      *    ****    Move the Debtor Record to working storage area
      *
             MOVE DEB-RECORD1        TO W40-DEBTREC.
      *
      *    Format name for display purposes.
      *
           IF W40-TYPE = 1
               MOVE "Y"              TO WS-DTPNAME
               MOVE "D"              TO W40-INSTR
               CALL "DTP\DTPNAME" USING W40-COMPANY LS-USER-ID
           ELSE
               MOVE W40-NAME         TO W40-DSP-NAME.
             MOVE "S"                TO W40-INSTR.
             CALL "DTP\DTPADDR" USING W40-COMPANY.
           IF W80-TYPE > 1
               MOVE DEB-B-TEL        TO W40-TEL
           ELSE
               MOVE DEB-H-TEL        TO W40-TEL.
           IF W80-TYPE > 1
               GO TO CD10.

       CD10.
             ADD 121 DEB-CAT         GIVING W90-RESULT.
             DIVIDE W90-RESULT BY 2  GIVING W90-CATKEY REMAINDER W90-CS.
             ADD 1                   TO W90-CS.
             MOVE W90-CATKEY         TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-CDESC(W90-CS)  TO W90-CDESC.
           IF DEB-CAT = 80
               MOVE "Account Suspended" TO W90-CDESC.
           IF W90-CDESC = SPACES
               MOVE "** I N V A L I D   C O D E **" TO W90-CDESC.
           IF DEB-SMAN >  ZERO AND < 51
               ADD 100 DEB-SMAN      GIVING WS-PARKEY
               PERFORM READ-PARAM THRU READ-PARAM-EXIT
               MOVE PAR-SALESMAN     TO W90-SMAN
           ELSE
               MOVE SPACES           TO W90-SMAN.
             EVALUATE DEB-TERMS
               WHEN "1"      MOVE "30 Days"    TO W90-TRMS
               WHEN "2"      MOVE "60 Days"    TO W90-TRMS
               WHEN "3"      MOVE "C.O.D."     TO W90-TRMS
               WHEN OTHER    MOVE "7 Days"     TO W90-TRMS
             END-EVALUATE.
             EVALUATE DEB-INV-TYPE
               WHEN "I"      MOVE "Inclusive"  TO W90-TYPE
               WHEN "E"      MOVE "Exclusive"  TO W90-TYPE
               WHEN OTHER    MOVE "Export"     TO W90-TYPE
             END-EVALUATE.
             EVALUATE DEB-PRICE
               WHEN "R"      MOVE "Retail"     TO W90-PRICE
               WHEN "C"      MOVE "Cash"       TO W90-PRICE
               WHEN OTHER    MOVE "Wholesale"  TO W90-PRICE
             END-EVALUATE.
             COMPUTE W40-MTD = DEB-BAL - (DEB-30 + DEB-60 + DEB-90 + DEB-120 + DEB-INT).
           IF W40-MTD < ZERO
               MOVE ZERO             TO W40-MTD.
             COMPUTE W40-ARREARS = DEB-BAL - (W40-MTD + DEB-INT).
           IF W40-ARREARS < ZERO
               MOVE ZERO             TO W40-ARREARS.
           IF LS0-DBLEV < 3
               MOVE ZERO             TO DEB-PURYTD.
             MOVE ZERO               TO WS-ENQEND WS-ENQPOS.
             DISPLAY S08D.
             
       CD12.
             MOVE W40-ACTIVE         TO W22-DTE2.
             MOVE W22-CC2            TO W22-CC3.
             MOVE W22-YY2            TO W22-YY3.
             MOVE W22-MM2            TO W22-MM3.
             MOVE W22-DD2            TO W22-DD3.
             MOVE W22-DTE3           TO W22-DTE1.
             MOVE DEB-PMT            TO W22-DTE2.
             MOVE W22-CC2            TO W22-CC3.
             MOVE W22-YY2            TO W22-YY3.
             MOVE W22-MM2            TO W22-MM3.
             MOVE W22-DD2            TO W22-DD3.
             DISPLAY S09D.

       CD13.
             MOVE "Ascending/Descending Tran Date seq (A/D)  [A]" TO WS-OPT-MES.
             MOVE "A"                TO WS-OPTION.
             MOVE "J"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
      *
      *    ****  Was the Escape key pressed?
      *
           IF WS-OPTION = X"EC"
               GO TO CD00.
             MOVE WS-OPTION          TO WS-SEQ.

       CD14.
             MOVE DEB-ACNO           TO TRA-AC.
           IF WS-SEQ = "A"
               MOVE ZERO             TO TRA-DATE TRA-CODE
               MOVE SPACES           TO TRA-REF
               PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT
           ELSE
               MOVE TODAY-YYMMDD     TO TRA-DATE
               MOVE 99               TO TRA-CODE
               MOVE "ZZZZZZZZ"       TO TRA-REF
               PERFORM START-AT-DTRN-KEY-DESC THRU READ-DBTRAN-EXIT.
           IF WS-F-ERROR = 5
               GO TO CD21.

       CD015.
             MOVE 12                 TO CLIN.
             INITIALIZE W15-ALLOCATIONS.
             MOVE ZERO               TO WS-S9.

       CD020.
           IF WS-SEQ = "A"
               PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT
           ELSE
               PERFORM READ-DBTRAN-PREV THRU READ-DBTRAN-EXIT.
           IF (WS-F-ERROR = 5) OR (TRA-AC NOT = DEB-ACNO)
               MOVE WS-ENQPOS        TO WS-ENQEND
               MOVE W30-KEY          TO TRA-KEY
               PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT
               GO TO CD21.
             MOVE TRA-KEY            TO W30-KEY.
             ADD 1                   TO WS-ENQPOS.

       CD20A.
             ADD 1                   TO WS-S9.
             MOVE TRA-CODE           TO W90-CODE.
             PERFORM GET-DEBTOR-TRAN-DESC.
      *
      *    ****    S W I T C H   D A T E   T O   D D M M C C Y Y
      *
             MOVE TRA-DATE           TO W22-DTE2.
             MOVE W22-CC2            TO W22-CC3.
             MOVE W22-YY2            TO W22-YY3.
             MOVE W22-MM2            TO W22-MM3.
             MOVE W22-DD2            TO W22-DD3.
           IF TRA-VALUE > ZERO
               MULTIPLY TRA-ALLOCATED BY -1 GIVING W15-ALLOC(WS-S9)
               SUBTRACT TRA-ALLOCATED FROM TRA-VALUE GIVING W15-VALUE(WS-S9)
               MOVE TRA-REC          TO W15-REC(WS-S9)
           ELSE
               MOVE TRA-ALLOCATED    TO W15-ALLOC(WS-S9)
               ADD TRA-VALUE TRA-PAID GIVING W15-VALUE(WS-S9)
               MOVE SPACES           TO W15-REC(WS-S9).
             DISPLAY S08T AT LINE CLIN.
           IF CLIN < 45
               ADD 1                 TO CLIN
               GO TO CD020.
             COMMIT.

       CD21.
             DISPLAY "PgDn"                                                                  AT 5002 WITH FOREGROUND-COLOR Brown HIGHLIGHT
                         ","                                                                         WITH FOREGROUND-COLOR Blue
                          "PgUp"                                                                     WITH FOREGROUND-COLOR Brown HIGHLIGHT
                              ","                                                                    WITH FOREGROUND-COLOR Blue
                               "Esc"                                                                 WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                  ","                                                                WITH FOREGROUND-COLOR Blue
                                   "F4"                                                              WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                     " S-Rest,"                                                      WITH FOREGROUND-COLOR Blue
                                             "F5"                                                    WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                               " Inv/Crn,"                                           WITH FOREGROUND-COLOR Blue
                                                        "F6"                                         WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                                          " purch,"                                  WITH FOREGROUND-COLOR Blue
                                                                 "F7"                                WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                                                   " alloc,"                         WITH FOREGROUND-COLOR Blue
                                                                          "F8"                       WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                                                            " screen 2,"             WITH FOREGROUND-COLOR Blue
                                                                                      "F9"           WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                                                                        " Memo/Rmks" WITH FOREGROUND-COLOR Blue.

       CD022.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    GO TO CD30
                 WHEN F4-KEY     PERFORM EA000
                                 GO TO CD022
                 WHEN F5-KEY     PERFORM DA000
                                 PERFORM RESTORE-SCREEN-3
                                 GO TO CD022
                 WHEN F6-KEY     GO TO CD40
                 WHEN F7-KEY     IF OPEN-ITEM
                                     GO TO CD35
                                 ELSE
                                     GO TO CD022
                                 END-IF
                 WHEN F8-KEY     GO TO CD50
                 WHEN F9-KEY     MOVE DEB-ACNO TO W80-ACNO
                                 PERFORM DEBTOR-MEMO
                 WHEN PAGE-UP    IF WS-ENQPOS = 0 OR 1 OR 35
                                     GO TO CD21
                                 ELSE
                                     GO TO CD025
                                 END-IF
                 WHEN PAGE-DOWN  GO TO CD023
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO CD022.
             PERFORM AA900-ALARM.
             GO TO CD022.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                     P A G E - D O W N                     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       CD023.
           IF WS-ENQPOS = WS-ENQEND
               GO TO CD022.
             MOVE 12                 TO CRT-START.
             PERFORM ERASE-SCREEN.
             PERFORM CD02.
             PERFORM CD12.
             GO TO CD015.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                      P A G E - U P                        Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       CD025.
             MOVE 1                  TO WS-IXS1.

       CD25.
           IF WS-SEQ = "A"
               PERFORM READ-DBTRAN-PREV THRU READ-DBTRAN-EXIT
           ELSE
               PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
           IF WS-F-ERROR = 5
               GO TO CD26.
           IF TRA-AC NOT = DEB-ACNO
               IF WS-SEQ = "A"
                   PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT
               ELSE
                   PERFORM READ-DBTRAN-PREV THRU READ-DBTRAN-EXIT
               END-IF
               GO TO CD26.
             SUBTRACT 1              FROM WS-ENQPOS.
             MOVE TRA-KEY            TO W30-KEY.
             ADD 1                   TO WS-IXS1.
           IF WS-IXS1 < 70
               IF WS-ENQPOS > 0
                   GO TO CD25.

       CD26.
             MOVE 12                 TO CRT-START.
             PERFORM ERASE-SCREEN.
             PERFORM CD02.
             PERFORM CD12.
             PERFORM CD015.
             ADD 1                   TO WS-ENQPOS.
             GO TO CD20A.

       CD30.
             COMMIT.
             GO TO CD00.

       CD35.
             PERFORM SAVE-SCREEN.
             MOVE 11                 TO SHADE-ROW.
             MOVE 45                 TO SHADE-COL.
             MOVE 34                 TO SHADE-WIDTH.
             MOVE 35                 TO SHADE-LINES.
             DISPLAY S09A.
             PERFORM SCREEN-SHADOW.
             PERFORM CLEAR-L50.
             DISPLAY "ENTER to continue" AT 5024 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             PERFORM RESTORE-SCREEN.

       CD35-E.
             GO TO CD21.

       CD40.
             PERFORM SAVE-SCREEN.
             MOVE 11                 TO SHADE-ROW.
             MOVE 51                 TO SHADE-COL.
             MOVE 28                 TO SHADE-WIDTH.
             MOVE 15                 TO SHADE-LINES.
           IF LS0-DBLEV < 2
               DISPLAY S09B-1
               DISPLAY "Not Authorised" AT 1459 WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Grey
           ELSE
               DISPLAY S09B.
             PERFORM SCREEN-SHADOW.
             PERFORM CLEAR-L50.
             DISPLAY "ENTER to continue" AT 5024 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             PERFORM RESTORE-SCREEN.

       CD45-E.
             GO TO CD21.

       CD50.
             PERFORM SAVE-SCREEN.
             MOVE 12                 TO SHADE-ROW.
             MOVE 5                  TO SHADE-COL.
             MOVE 52                 TO SHADE-WIDTH.
             MOVE 12                 TO SHADE-LINES.
             DISPLAY S10.
             PERFORM SCREEN-SHADOW.
             PERFORM CLEAR-L50.
             DISPLAY "ENTER to continue" AT 5024 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             PERFORM RESTORE-SCREEN.
       CD50-E.
             GO TO CD21.

       CD999.
            EXIT.
      /    *************************************************************
      *    * L O O K U P   I N V O I C E   O R   C R E D I T   N O T E *
      *    *************************************************************
       DA000         SECTION 5.
       DA00.
             PERFORM SAVE-SCREEN-3.
             MOVE ZERO               TO W10-NUM.
             MOVE 15                 TO SHADE-ROW.
             MOVE 52                 TO SHADE-COL.
             MOVE 27                 TO SHADE-WIDTH.
             MOVE 3                  TO SHADE-LINES.
             DISPLAY "зддддддддддддддддддддддддддд" AT 1450 WITH FOREGROUND-COLOR Black          BACKGROUND-COLOR Grey
                                                 "©"        WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey.
             DISPLAY "Ё"                            AT 1550 WITH FOREGROUND-COLOR Black          BACKGROUND-COLOR Grey
                      "    INVOICE/CREDIT NOTE    Ё"        WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey
             DISPLAY "Ё Reference Number          " AT 1650 WITH FOREGROUND-COLOR Black          BACKGROUND-COLOR Grey
                                                 "Ё"        WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey.
             DISPLAY "ю"                            AT 1750 WITH FOREGROUND-COLOR Black          BACKGROUND-COLOR Grey
                      "ддддддддддддддддддддддддддды"        WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey.
             PERFORM SCREEN-SHADOW.

       DA05.
             ACCEPT S26-REF.
             PERFORM RESTORE-SCREEN-3.
           IF W10-NUM = ZERO
               GO TO DA199.
             MOVE W10-NUM            TO DOC-REF.
             MOVE ZERO               TO DOC-SQN DOC-TYPE.
             PERFORM START-AT-INVOIC-REF THRU READ-INVOIC-EXIT.
           IF WS-F-ERROR = 12
               GO TO DA15.

       DA10.
             PERFORM READ-INVOIC-NEXT THRU READ-INVOIC-EXIT.
           IF WS-F-ERROR = 12
               GO TO DA15.
           IF (WS-F-ERROR NOT = ZERO) OR (DOC-REF NOT = W10-NUM)
               GO TO DA15.
           IF DOC-TYPE NOT = 0
               GO TO DA10.
             GO TO DA100.

       DA15.
             MOVE "Document not on File" TO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.
             GO TO DA00.

       DA100.
             MOVE DOC-AC             TO W80-ACNO W40-ACNO.
             PERFORM CA155-GET-DEBTOR.
             MOVE 1                  TO WS-S9.

       DA108.
             MOVE ZERO               TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
           IF WS-S9 < 12
               ADD 1                 TO WS-S9
               GO TO DA108.
             MOVE ZERO               TO W90-BDT W90-SALES W90-BAL W90-LDISC W90-CUR W90-NONTAX W90-EXCTOT.
             PERFORM DA200.
             PERFORM ERASE-SCREEN.
             DISPLAY S26.
           IF WS-CR = "C"
               DISPLAY S027-CNOTE.
             MOVE 11                 TO LIN.
             PERFORM DA180 UNTIL LIN > 41.
             DISPLAY W90-REF         AT 0810 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Black.
             MOVE W20-DTE            TO W22-DTE2.
             MOVE W22-CC2            TO W22-CC3.
             MOVE W22-YY2            TO W22-YY3.
             MOVE W22-MM2            TO W22-MM3.
             MOVE W22-DD2            TO W22-DD3.
             MOVE W22-DTE3           TO W90-DTE.
             DISPLAY W90-DTE         AT 0870 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Black.
             MOVE 1                  TO WS-FIRST WS-SALKEY.
           IF W90-EXP = "E" OR "X"
               MOVE W05-VATNO        TO W90-TAX
           ELSE
               MOVE SPACES           TO W90-TAX.
             DISPLAY S36 AT 0608.
             DISPLAY W40-DSP-NAME    AT 0620 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Black.
             DISPLAY W90-ORD         AT 0668 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Black.
           IF W10-SMAN NOT = ZERO
               ADD 100 W10-SMAN      GIVING WS-PARKEY
               PERFORM READ-PARAM THRU READ-PARAM-EXIT
               DISPLAY W10-SMAN      AT 0711 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Black
               DISPLAY PAR-SALESMAN  AT 0720 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Black
               MOVE PAR-SALESMAN     TO W40-ASSIST
           ELSE
               MOVE SPACES           TO W40-ASSIST.
           IF DEB-B-TEL = SPACES
               MOVE DEB-H-TEL        TO W40-TEL
           ELSE
               MOVE DEB-B-TEL        TO W40-TEL.
             DISPLAY W40-TEL         AT 0766 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Black.
             DISPLAY W90-CONTACT     AT 0829 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Black.
             DISPLAY W90-REG         AT 0854 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Black.
             MOVE 1                  TO WS-SALKEY.
             MOVE W90-DB             TO W90-TOTAL.
             SUBTRACT W90-LDISC      FROM W90-TOTAL.
             MOVE W90-DB             TO W100-VAL.
             DISPLAY W100-VAL        AT 4315 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Black.
             MULTIPLY -1 BY W90-LDISC GIVING W100-VAL.
             DISPLAY W100-VAL        AT 4415 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Black.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё       C A L C U L A T E   T A X   ( RUNNING TOTAL )       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
           IF W90-TAX = SPACES
               GO TO DA110.
           IF W90-TTAX NOT = ZERO
               MOVE W90-TTAX         TO W25-TOTAL
               MOVE W25-TOTAL        TO W100-VAL
               DISPLAY W100-VAL      AT 4515 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Black
               ADD W25-TOTAL         TO W90-TOTAL.

       DA110.
             MOVE W90-TOTAL          TO W100-VAL.
             DISPLAY W100-VAL        AT 4615 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Black.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё        D I S P L A Y   S A L E S   D E T A I L S          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       DA115.
             MOVE 11                 TO LIN.
             PERFORM DA180 UNTIL LIN > 41.
             MOVE 11                 TO LIN.
             MOVE 1                  TO WS-LINES WS-FIRST.

       DA120.
             MOVE WS-FIRST           TO WS-SALKEY.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё            Have all the entries been displayed?           Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       DA125.
           IF WS-SALKEY > WS-LAST
               GO TO DA135.
             PERFORM READ-SALE THRU READ-SALE-EXIT.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё         D I S P L A Y   E N T R Y   D E T A I L S         Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
           IF SAL-ITEM = SPACES
               IF SAL-TYPE = "R"
                   DISPLAY SAL-MSER  AT LINE LIN COLUMN 10 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Black
               ELSE
                   DISPLAY SAL-SDSC  AT LINE LIN COLUMN 21 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Black
               END-IF
               GO TO DA130.
             DISPLAY SAL-ITEM        AT LINE LIN COLUMN 2  WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Black.
             DISPLAY "   "           AT LINE LIN COLUMN 17 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Black.
             DISPLAY SAL-SDSC        AT LINE LIN COLUMN 21 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Black.
             MOVE SAL-QNT            TO W10-QUANT.
             DISPLAY S26-QNT         AT LINE LIN.
             MOVE SAL-SELL           TO W10-SELL.
             DISPLAY S26-PRC         AT LINE LIN.
             MOVE SAL-VAL            TO W100-EXT.
             DISPLAY W100-EXT        AT LINE LIN COLUMN 70 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Black.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё             I S   T H E   S C R E E N   F U L L           Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       DA130.
           IF LIN < 41
               ADD 1                 TO LIN WS-SALKEY WS-LINES
               GO TO DA125.
             MOVE "'N'ext page or 'E'xit [N]"  TO WS-OPT-MES.
             MOVE "N"                TO WS-OPTION.
             MOVE "I"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "E"
               GO TO DA199.
             PERFORM DA115.
             GO TO DA125.

       DA135.
             EXIT.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё               D I S P L A Y   O P T I O N S               Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       DA140.
             MOVE "Press ANY key to continue"  TO WS-OPT-MES.
             MOVE "Z"                          TO WS-INSTR.

       DA145.
             PERFORM OPT-MESSAGE.
             GO TO DA199.

       DA180.
             DISPLAY S26-MIV-LIN AT LINE LIN.
             ADD 1                   TO LIN.

       DA199.
             EXIT.

       DA200             SECTION 5.
       DA201.
             MOVE ZERO               TO DOC-TYPE.
             MOVE 100                TO DOC-SQN.
             PERFORM READ-INVOIC THRU READ-INVOIC-EXIT.
           IF WS-F-ERROR = 12
               GO TO DA295.
             MOVE 1                  TO WS-LAST WS-SALKEY.
             GO TO DA210.

       DA205.
             PERFORM READ-INVOIC-NEXT THRU READ-INVOIC-EXIT.
           IF WS-F-ERROR = 12
               GO TO DA295.

       DA210.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                 H E A D E R   R E C O R D                 Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
           IF DOC-TYPE = 0
               MOVE DOC-REF          TO W90-REF
               MOVE DOC-NAME         TO W40-DSP-NAME
               MOVE DOC-DTE          TO W20-DTE
               MOVE DOC-ORD          TO W90-ORD
               MOVE DOC-SMAN         TO W10-SMAN
               MOVE DOC-DOC          TO WS-CR
               MOVE DOC-OREF         TO W90-REG
               MOVE DOC-EXP          TO W90-EXP
               MOVE DOC-PRC          TO WS-PRC
               MOVE DOC-PROF         TO WS-PROF
           ELSE
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                  I T E M   D E T A I L S                  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
           IF DOC-TYPE = 1
               INITIALIZE SAL-RECORD1
               IF PROF-INV
                   MOVE DOC-REMKS    TO SAP-DESC
                   MOVE DOC-PSELL    TO SAP-SELL
                   MOVE DOC-PVAT     TO SAP-VAT
                   MOVE DOC-PVAL     TO SAP-VAL
                   MOVE DOC-PINCTAX  TO SAP-INCTAX
                   MOVE DOC-PNONTAX  TO SAP-NONTAX
                   MOVE DOC-PTAXVAL  TO SAP-TAXVAL
                   MOVE DOC-LEDG     TO SAP-LEDG
                   MOVE DOC-SQN      TO SAL-SQN
                   PERFORM DA215
               ELSE
                   MOVE "S"          TO SAL-TYPE
                   MOVE DOC-EXT-ITEM TO SAL-EXT-ITEM
                   MOVE DOC-DESC     TO SAL-DESC
                   MOVE DOC-QNT      TO SAL-QNT
                   MOVE DOC-CASES    TO SAL-CASES
                   MOVE DOC-COST     TO SAL-COST
                   MOVE DOC-SELL     TO SAL-SELL
                   MOVE DOC-VAT      TO SAL-VAT
                   MOVE DOC-VAL      TO SAL-VAL
                   MOVE DOC-VAT-SUB  TO SAL-VAT-SUB
                   MOVE DOC-INCTAX   TO SAL-INCTAX
                   MOVE DOC-NONTAX   TO SAL-NONTAX
                   MOVE DOC-TAXVAL   TO SAL-TAXVAL
                   MOVE DOC-DSC      TO SAL-DSC
                   MOVE DOC-DISC     TO SAL-DISC
                   MOVE DOC-SQN      TO SAL-SQN
                   IF NOT (SAL-VAT-SUB = ZERO)
                       ADD SAL-VAT     TO W90-VAT(SAL-VAT-SUB)
                       ADD SAL-TAXVAL  TO W90-TAXVAL(SAL-VAT-SUB)
                   END-IF
                   PERFORM DA215
               END-IF
           ELSE
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё              D I S C O U N T   D E T A I L S              Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
           IF DOC-TYPE = 2
               INITIALIZE SAL-RECORD1
               MOVE DOC-DSC          TO SAL-DSC
               MOVE DOC-DISC         TO SAL-DISC
               MOVE DOC-SQN          TO SAL-SQN
              PERFORM DA215
           ELSE
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                       R E M A R K S                       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
           IF DOC-TYPE = 3
               INITIALIZE SAL-RECORD1
               MOVE DOC-MSER         TO SAL-MSER
               MOVE DOC-SQN          TO SAL-SQN
              PERFORM DA215
           ELSE
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё              D E L I V E R Y   A D D R E S S              Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
           IF DOC-TYPE = 5
               MOVE DOC-AD1          TO W40-PADD1
               MOVE DOC-AD2          TO W40-PADD2
               MOVE DOC-AD3          TO W40-PADD3
               MOVE DOC-AD4          TO W40-PADD4
               MOVE DOC-PC           TO W40-PC2
           ELSE
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё            R E M A R K S   ( A D V A N C E D )            Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
           IF DOC-TYPE = 6
               INITIALIZE SAL-RECORD1
               MOVE "R"              TO SAL-TYPE
               MOVE DOC-MSER         TO SAL-MSER
               MOVE DOC-SQN          TO SAL-SQN
               PERFORM DA215
          ELSE
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                       T R A I L E R                       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
          IF DOC-TYPE = 9
               MOVE DOC-SUBTOT       TO W90-DB
               MULTIPLY -1 BY DOC-DSCNT  GIVING W90-TDISC
               MOVE W90-TDISC        TO W90-LDISC
               MOVE DOC-NON-TXBL     TO W90-NONTAX
               MOVE DOC-TAXABLE      TO W90-EXCTOT
               MOVE DOC-TAX          TO W90-TTAX
               MOVE DOC-TOT          TO W90-TOTAL
               MOVE DOC-BAL          TO W90-BAL
               MOVE DOC-TERMS        TO W90-TERMS
               MOVE DOC-CONTACT      TO W90-CONTACT
               SUBTRACT 1            FROM WS-LAST
               GO TO DA299.
             GO TO DA205.

       DA215.
             MOVE WS-LAST            TO SAL-CODE.
             PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
             ADD 1                   TO WS-LAST WS-SALKEY.

       DA295.
             MOVE "Data base corrupted"  TO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.
             MOVE 1                      TO WS-ERROR.

       DA299.
             EXIT.
      /    *************************************************************
      *    *  V I E W   D E P A R T M E N T   R E S T R I C T I O N S  *
      *    *************************************************************
       EA000         SECTION 52.
       EA00.
           IF NO-SALES-RESTRICTIONS
               GO TO EA200.
      *
      *             DEPARTMENTS EXCLUDED
      *     зддддддддддддддддддддддддддддддддддд©
      *     Ё        DEPARTMENTS ALLOWED        Ё╟╟
      *     цддддбдддддддддддддддддддддддддддддд╢╟╟
      *     ЁCodeЁ         Description          Ё╟╟
      *     цддддедддддддддддддддддддддддддддддд╢╟╟
      *     Ё    Ё                              Ё╟╟
      *     Ё    Ё                              Ё╟╟
      *     Ё    Ё                              Ё╟╟
      *     Ё    Ё                              Ё╟╟
      *     Ё    Ё                              Ё╟╟
      *     Ё    Ё                              Ё╟╟
      *     Ё    Ё                              Ё╟╟
      *     Ё    Ё                              Ё╟╟
      *     Ё    Ё                              Ё╟╟
      *     Ё    Ё                              Ё╟╟
      *     юддддадддддддддддддддддддддддддддддды╟╟
      *       ╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟
      *
             PERFORM SAVE-SCREEN-3.
             MOVE 13                 TO SHADE-ROW.
             MOVE 42                 TO SHADE-COL.
             MOVE 35                 TO SHADE-WIDTH.
             MOVE 25                 TO SHADE-LINES.
             DISPLAY "зддддддддддддддддддддддддддддддддддд"   AT 1240 WITH FOREGROUND-COLOR Black          BACKGROUND-COLOR Grey
                     "©"                                              WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey.
             DISPLAY "Ё"                                      AT 1340 WITH FOREGROUND-COLOR Black          BACKGROUND-COLOR Grey.
           IF SALES-ALLOWED
               DISPLAY "        DEPARTMENTS ALLOWED        Ё" AT 1341 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey
           ELSE
               DISPLAY "        DEPARTMENTS EXCLUDED       Ё" AT 1341 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey.
             DISPLAY "цддддбдддддддддддддддддддддддддддддд"   AT 1440 WITH FOREGROUND-COLOR Black          BACKGROUND-COLOR Grey
                     "╢"                                              WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey.
             DISPLAY "ЁCodeЁ     Description           "      AT 1540 WITH FOREGROUND-COLOR Black          BACKGROUND-COLOR Grey
                     "Ё"                                              WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey.
             DISPLAY "цддддедддддддддддддддддддддддддддддд"   AT 1640 WITH FOREGROUND-COLOR Black          BACKGROUND-COLOR Grey
                     "╢"                                              WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey.
             MOVE 1740               TO CPOS.
             PERFORM EA100 UNTIL CLIN > 36.
             DISPLAY "ю"                                      AT 3740 WITH FOREGROUND-COLOR Black          BACKGROUND-COLOR Grey
                     "ддддадддддддддддддддддддддддддддддды"           WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey.
             PERFORM SCREEN-SHADOW.
             MOVE DEB-ACNO           TO DBD-ACNO.
             PERFORM READ-DEBDEP THRU READ-DEBDEP-EXIT.
           IF WS-F-ERROR = 33
               PERFORM RESTORE-SCREEN-3
               GO TO EA200.
             MOVE 1                  TO WS-S.
             MOVE 1741               TO CPOS.

       EA05.
           IF DBD-DEPT-CDE(WS-S) = SPACES
               GO TO EA15.
             MOVE DBD-DEPT-CDE(WS-S) TO DPT-CODE.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
           IF WS-F-ERROR = 7
               GO TO EA10.
             DISPLAY DPT-CODE        AT CPOS WITH FOREGROUND-COLOR Grey    HIGHLIGHT BACKGROUND-COLOR Grey.
             MOVE 46                 TO CCOL. 
             DISPLAY DPT-DESC        AT CPOS WITH FOREGROUND-COLOR Magenta           BACKGROUND-COLOR Grey.
             ADD 1                   TO CLIN.
             MOVE 41                 TO CCOL.

       EA10.
           IF WS-S < 20
               ADD 1                 TO WS-S
               GO TO EA05.

       EA15.
             PERFORM CLEAR-L50.
             DISPLAY "ENTER to continue" AT 5024 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             PERFORM RESTORE-SCREEN-3.
             GO TO EA999.

       EA100.
             DISPLAY "Ё    Ё                              " AT CPOS WITH FOREGROUND-COLOR Black          BACKGROUND-COLOR Grey
                     "Ё"                                            WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey.
             ADD 1                   TO CLIN.

       EA199.
             EXIT.

       EA200.
             MOVE "No Sales Restrictions for account"  TO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.

       EA999.
             EXIT.

      *
      *    ****    I N I T I A L I Z E   P R O G R A M
      *
       ZA000-INIT        SECTION 8.
       ZA000-OPEN.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W22-DTE3.
             MOVE W22-DD3            TO W22-DD2.
             MOVE W22-MM3            TO W22-MM2.
             MOVE W22-CC3            TO W22-CC2.
             MOVE W22-YY3            TO W22-YY2.
             MOVE W22-DTE2           TO TODAY-YYMMDD.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Setup various keys and the mouse for use with menus and  Ё
      *    Ё  other required user interface functions and procedures.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

             COPY "FUNCTION.PRO".

       ZA02.
             MOVE 1                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY        TO W95-COMP.
             MOVE PAR-DMY            TO W12-TODAY.
             MOVE PAR-YMD            TO W12-T-YMD.
             OPEN OUTPUT SALE.
             CLOSE SALE.
             OPEN I-O SALE.
             OPEN INPUT DEBDEP.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 33           TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
               MOVE 33               TO WS-F-ERROR
               IF WS-STATUS = "39"
                   GO TO ZA50
               ELSE
               IF NOT(WS-STATUS = "41")
                   PERFORM OPEN-ERROR.
                   GO TO ZA999-EXIT.

       COPY "ZA49.PRO".

       ZA50.
             DISPLAY "Rebuild "                  AT 0812
                     "DEBDEP file"               WITH FOREGROUND-COLOR Brown HIGHLIGHT.
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
      *    ****    I / O   E R R O R   M E S S A G E S
      *
       ZB000-ERROR      SECTION 8.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 2          MOVE W02-NETWORK  TO WS-FILE
                               MOVE WS-NETKEY    TO WS-KEY
               WHEN 5          MOVE W02-DBTRAN   TO WS-FILE
                               MOVE TRA-KEY      TO WS-KEYX
               WHEN 7          MOVE W02-DEPART   TO WS-FILE
                               MOVE DPT-CODE     TO WS-KEYX
               WHEN 15         MOVE WS-PARID     TO WS-FILE
                               MOVE WS-PARKEY    TO WS-KEY
               WHEN 33         MOVE W02-DEBDEP   TO WS-FILE
                               MOVE DBD-ACNO     TO WS-KEYX
      *        WHEN 47         MOVE W02-DEBALT   TO WS-FILE
      *                        MOVE DAL-KEY      TO WS-KEYX
               WHEN OTHER      MOVE "ERROR"      TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
