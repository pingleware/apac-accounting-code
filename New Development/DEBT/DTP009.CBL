      $SET LINKCOUNT"468" GNT"DTP009.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****      ****     ******    *
      *   **    **     **     **    **   **  **    **  **   **    **   *
      *   **    **     **     **    **  **    **  **    **  **    **   *
      *   **    **     **     *******   **    **  **    **   *******   *
      *   **    **     **     **        **    **  **    **        **   *
      *   **    **     **     **         **  **    **  **   **    **   *
      *   *******      **     **          ****      ****     ******    *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *          A C C O U N T S   R E C E I V A B L E   /             *
      *                                                                *
      *     D E B T O R   C R E D I T   N O T E   A N A L I S E D      *
      *                                                                *
      *       Version 9.04.04 - February/March 2018                    *
      *                                                                *
      ******************************************************************
      *                                                                *
      * May 2006 - New field included in Accounts Receivable for       *
      *            recording number of outstanding Jobs linked to the  *
      *            account. Additional filler added for future         *
      *            expansion.                                          *
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
      *             memo/remarks on a Debtor account. Move the Debtor  *
      *             code/account number to W80-ACCOUNT and then        *
      *             PERFORM DEBTOR-MEMO. The Debtor lookup program -   *
      *             DTPLOOK now includes a Memo Lookup so unless there *
      *             is a requirement to lookup memo details after this *
      *             lookup, the MEMO lookup can be removed from all    *
      *             DEBTOR programs.                                   *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     DTP009.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   OCTOBER 1982.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1982 - 2018
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

       COPY "DEPART.SL".

       COPY "LEDTRF.SL".

       COPY "PARAM.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

       COPY "TXTRAN.SL".

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

       COPY "DEBTOR.FDE".

       COPY "DEPART.FDE".

       COPY "LEDTRF.FDE".

       COPY "PARAM.FDE".

       COPY "RECOVER.FD".

       COPY "SHARED.FDE".

       COPY "TXTRAN.FDE".

      *
      *         **        **    *****    ******    **   **
      *         **        **   **   **   **   **   **  **
      *         **        **   **   **   **  **    ** **
      *         **   **   **   **   **   *****     ****
      *          ** **** **    **   **   **  **    ** **
      *           ***  ***     **   **   **   **   **  **
      *            *    *       *****    **   **   **   **
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
       77  WS-IXS2                 PIC  9(04)    COMP-5.
       77  WS-IXS3                 PIC  9(04)    COMP-5.
       77  WS-IXS4                 PIC  9(04)    COMP-5.
       77  WS-ENQPOS               PIC  9(06)    COMP-5.
       77  WS-ENQEND               PIC  9(06)    COMP-5.
       77  WS-NETKEY               PIC  9(06)    COMP-5.
       77  WS-PARKEY               PIC  9(06)    COMP-5.
       77  WS-RECKEY               PIC  9(06)    COMP-5.
       77  WS-RECOVER              PIC  9(06)    COMP-5.
       77  WS-SHARED               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-TRANS                PIC  9(04)    COMP-5 VALUE 1.
       77  WS-FIN-RATE             PIC S9(03)V99 COMP-3.
       77  WS-PERIOD               PIC  9(02).
       77  WS-NTP                  PIC  X(01).
       77  WS-PAGE                 PIC  9(04)    COMP-5.
       77  WS-KEYSTR               PIC  9(06)    COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-SKIP                 PIC  X(01)           VALUE "N".
       77  WS-TAX                  PIC  X(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-ERROR                PIC  9(01)           VALUE ZERO.
       77  WS-EKEY                 PIC  9(06)    COMP-5.
       77  WS-CHAR1                PIC  X(01).
       77  WS-CHAR2                PIC  X(01).
       77  WS-COMP                 PIC  9(01).
       77  WS-BATCH                PIC  9(06)    COMP-5.
       77  WS-ERR1                 PIC  X(22)           VALUE "Invalid Account Number".
       77  WS-ERR2                 PIC  X(09)           VALUE "No Record".
       77  WS-E                    PIC  X(15)           VALUE "**END** - ENTER".
       77  WS-M                    PIC  X(17)           VALUE "New batch (Y/N) ?".
       77  WS-AC                   PIC  X(07)           VALUE "ACCOUNT".
       77  PRG-DEBT-LUP            PIC  X(12)           VALUE "DTP\DTPLOOK".
       77  TODAY-DDMMYY            PIC  9(08) COMP-5.
       77  WS-USUB                 PIC  9(04) COMP-5.
      *
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "DTP009".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "ACCOUNTS RECEIVABLE -ANALISED CREDIT NTE".
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
           03  WS-PROG             PIC  X(03) VALUE "009".

       COPY "LEDTRF.WS".

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

           03  R-L1                              REDEFINES W02-PRN-LINE.
               05  R-DET1          PIC  X(136).
           03  R-L3                              REDEFINES W02-PRN-LINE.
               05  R-DATE          PIC  X(06).
               05  R-DTE           PIC  Z9/99/9999.
               05  FILLER          PIC  X(04).
               05  R-H5            PIC  X(46).
               05  R-H6            PIC  X(07).
               05  R-BATCH         PIC  Z(05)9.
               05  FILLER          PIC  X(07).
               05  R-PAGE          PIC  X(06).
               05  R-P-NO          PIC  Z(03)9.
           03  R-L3A                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(04).
               05  R-CO            PIC  X(40).
               05  FILLER          PIC  X(04).
           03  R-L10                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(10).
               05  I-H8            PIC  X(09).
               05  I-TOTTAX        PIC  Z(07)9.99-.
               05  FILLER          PIC  X(02).
               05  I-H9            PIC  X(08).
               05  I-TOTEXE        PIC  Z(07)9.99-.
               05  FILLER          PIC  X(02).
               05  I-H10           PIC  X(08).
               05  I-TOTGST        PIC  Z(07)9.99-.
               05  FILLER          PIC  X(02).
               05  I-H11           PIC  X(07).
               05  I-TOTBCH        PIC  Z(07)9.99-.
           03  TL-L3                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(01).
               05  TL-LEDG         PIC  X(04).
               05  FILLER          PIC  X(01).
               05  TL-NME          PIC  X(37).
               05  FILLER          PIC  X(01).
               05  TL-ACT          PIC  99/99/9999.
               05  FILLER          PIC  X(01).
               05  TL-PD           PIC  Z(05)9.99-.
               05  FILLER          PIC  X(01).
               05  TL-MTH          PIC  9(01).
               05  FILLER          PIC  X(02).
               05  TL-BAL          PIC  Z(05)9.99-.
           03  I-L1                              REDEFINES W02-PRN-LINE.
               05 I-H1             PIC  X(07).
               05 I-H2             PIC  X(35).
               05 I-H3             PIC  X(09).
               05 I-H4             PIC  X(14).
               05 I-H5             PIC  X(14).
               05 I-H6             PIC  X(11).
               05 I-H7             PIC  X(06).
           03  I-L8                              REDEFINES W02-PRN-LINE.
               05 I-AC             PIC  X(06).
               05 FILLER           PIC  X(01).
               05 I-NAME           PIC  X(31).
               05 FILLER           PIC  X(01).
               05 I-DATE           PIC  Z9/99/9999.
               05 FILLER           PIC  X(01).
               05 I-REF            PIC  X(08).
               05 FILLER           PIC  X(01).
               05 I-CAP            PIC  Z(07)9.99-.
               05 FILLER           PIC  X(01).
               05 I-VAT            PIC  Z(06)9.99-.
               05 FILLER           PIC  X(01).
               05 I-TOT            PIC  Z(07)9.99-.

       COPY "W05.VAT".

       01  W05-DISC.
           03  W05-DISCOUNT.
               05  W05-DSETT       PIC S9(03)V99   COMP-3 OCCURS 10.

      *01  W08-EXPD.
      *    03  W08-CHAR              PIC  X(06).
      *    03  W08-RCHAR                           REDEFINES W08-CHAR.
      *        05 W08-DEC            PIC  X(01)    COMP-X OCCURS 6.

       01  W10-SPARES.
           03  W10-DATE            PIC  X(10).
           03  W10-RDATE                           REDEFINES W10-DATE.
               05  W10-EDTE        PIC  99/99/9999.

       01  W10-REF.
           03  W10-NUM             PIC  9(08).
           03  W10-REF1                            REDEFINES W10-NUM.
               05  FILLER          PIC  X(07).
               05  W10-N1.
                   07  W10-NUM1    PIC  9(01).
           03  W10-REF2                            REDEFINES W10-NUM.
               05  FILLER          PIC  X(06).
               05  W10-N2.
                   07  W10-NUM2    PIC  9(02).
           03  W10-REF3                            REDEFINES W10-NUM.
               05  FILLER          PIC  X(05).
               05  W10-N3.
                   07  W10-NUM3    PIC  9(03).

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

       01  W15-CONTROL.
           03  W15-LEDG.
               05  W15-LDG         PIC  9(04)    COMP.

       01  W25-CALCS.
           03  W25-RESULT          PIC  9(05)V99.
           03  W25-RESULT1                       REDEFINES W25-RESULT.
               05  W25-WHOLE       PIC  9(05).
               05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2                       REDEFINES W25-RESULT.
               05  W25-KEY         PIC  9(04).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).
           03  W25-TOTAL           PIC S9(09)V99 COMP-3.
           03  W25-VALUE           PIC S9(09)V99 COMP-3.

       01  W30-AGE-DATES.
           03  W30-120             PIC  9(08) COMP-3.
           03  W30-90              PIC  9(08) COMP-3.
           03  W30-60              PIC  9(08) COMP-3.
           03  W30-30              PIC  9(08) COMP-3.
           03  W30-CUR             PIC  9(08) COMP-3.

       COPY "W40.DBT".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

      *COPY "W50.WS".

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
    03  W75-S       PIC  9(02)    COMP-5.
    03  W75-S1       PIC  9(02)    COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
           03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
    03  W75-DNO       PIC  X(06) OCCURS 18.
    03  W75-SLEDG      PIC  9(03)    COMP-3 OCCURS 10.

       COPY "W75.WS".

       01  W80-ACCOUNT.
           03  W80-ACNO        PIC X(06).
      *    03  W80-TYPE        PIC 9(01).

       01  W85-PASS.
           03  W85-SUPER       PIC  X(06)    OCCURS 9.
           03  W85-ENTRY       PIC  9(02)    COMP-3.
           03  W85-MARG        PIC S9(02)V99.

       01  W85-BATCHES.
               05  W85-BATCH   PIC  9(06)    COMP-5.

       COPY "W90.DBT".

       01  W95-STM.
           03  W95-COMP        PIC X(40).
           03  W95-V2.
               05  W95-B             PIC Z(06)9.99-.

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

       COPY "DEPART.CRT".

       01  S35.
           03  PIC Z(06)9.99- USING W40-INT FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.

       01  S36.
           03  PIC X(06) USING W80-ACNO FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.

       01  S50.
           03  BACKGROUND-COLOR Blue FOREGROUND-COLOR Cyan.
               05  LINE  5 COLUMN  2 VALUE "Account No:".
               05          COLUMN 22 VALUE                     "Name:".
               05  LINE  7 COLUMN  2 VALUE "Date:".
               05          COLUMN 22 VALUE                     "Invoice No:".
               05  LINE  9 COLUMN 12 VALUE           "Taxable:".
               05  LINE 10 COLUMN 12 VALUE           "Exempt :".
               05  LINE 11 COLUMN 12 VALUE           "Tax    :".
               05  LINE 13 COLUMN 12 VALUE           "Total  :".

       01  S50-DATA.
           03  BACKGROUND-COLOR Magenta.
               05  LINE  5 COLUMN 28 PIC  X(40) FROM WS-SPC.
               05  LINE  7 COLUMN  8 PIC  X(10) FROM WS-SPC.
               05          COLUMN 34 PIC  X(08) FROM WS-SPC.
               05  LINE  9 COLUMN 21 PIC  X(12) FROM WS-SPC.
               05  LINE 10 COLUMN 21 PIC  X(12) FROM WS-SPC.
               05  LINE 11 COLUMN 21 PIC  X(12) FROM WS-SPC.
               05  LINE 13 COLUMN 21 PIC  X(12) FROM WS-SPC.

       01  S51.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               05  S51A.
                   07  LINE  7 COLUMN  8 PIC Z9/99/9999 USING W90-IDTE AUTO.
                   07          COLUMN 34 PIC X(08)      USING W90-REF  AUTO.
                   07  LINE  9 COLUMN 21 PIC Z(08)9.99  USING W90-CAP  AUTO.
                   07  LINE 10 COLUMN 21 PIC Z(08)9.99  USING W90-EX   AUTO.
               05  S51AB.
                   07  LINE 11 COLUMN 23 PIC Z(06)9.99 USING W90-TX    AUTO.
               05  S51B.
                   07  LINE 13 COLUMN 21 PIC Z(08)9.99 USING W90-DB    AUTO.
               05  S51C.
                   07  LINE 13 COLUMN 21 PIC Z(08)9.99 USING W90-CR    AUTO BACKGROUND-COLOR Red.

       01  S52.
           03  BACKGROUND-COLOR Blue FOREGROUND-COLOR Cyan.
               05  LINE  16 COLUMN 22 VALUE "Balance  :".
               05  LINE  17 COLUMN 22 VALUE "Current  :".
               05  LINE  18 COLUMN 22 VALUE " 30 days :".
               05  LINE  19 COLUMN 22 VALUE " 60 days :".
               05  LINE  20 COLUMN 22 VALUE " 90 days :".
               05  LINE  21 COLUMN 22 VALUE "120 days :".
               05  LINE  22 COLUMN 22 VALUE "Interest :".

       01  S52-DATA.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT
               05  LINE  16 COLUMN 33 PIC Z(08)9.99- USING DEB-BAL.
               05  LINE  17 COLUMN 33 PIC Z(08)9.99- USING W40-CUR.
               05  LINE  18 COLUMN 33 PIC Z(08)9.99- USING DEB-30.
               05  LINE  19 COLUMN 33 PIC Z(08)9.99- USING DEB-60.
               05  LINE  20 COLUMN 33 PIC Z(08)9.99- USING DEB-90.
               05  LINE  21 COLUMN 33 PIC Z(08)9.99- USING DEB-120.
               05  LINE  22 COLUMN 33 PIC Z(08)9.99- USING DEB-INT.

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
       AA000         SECTION.
       AA00.
           IF LS0-DBLEV < 3
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *   **  *   *   * *   *  *  **  *   *   *     * *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *   *  **   *     *   *  *  *  **   *   *     * *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** * *
      *
             PERFORM OPEN-PRINTER.
             MOVE "P"                TO WS-COMMAND.
             PERFORM CK000.
             CLOSE RECOVER.
             MOVE "C"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.

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
       AA50.
             PERFORM SAVE-SCREEN.
      *      MOVE 4                  TO W44-FUNCTION.
             PERFORM SCREEN-CONTENTS.
             CALL PRG-DEBT-LUP USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS W40-COMPANY
                  ON EXCEPTION GO TO AA55
             END-CALL.
             CANCEL PRG-DEBT-LUP.
             MOVE W40-ACNO           TO W80-ACNO.
             GO TO AA58.

       AA55.
             MOVE SPACE              TO WS-ERR-MES.
             STRING "Program- " DELIMITED SIZE PRG-DEBT-LUP DELIMITED " " " not on disk, press ANY key" DELIMITED SIZE INTO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.

       AA58.
             PERFORM RESTORE-SCREEN.

       AA99.
             EXIT.

       COPY "AA900.ALM".

      *    
      *            ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *           S C R E E N ,   K E Y B O A R D   &   M O U S E
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
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *            T H I S   R O U T I N E   I S   U S E D   T O
      *       D I S P L A Y   I N F O R M A T I O N   M E S S A G E S
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Move the message to WS-DSP-MES and the top line of the   ³
      *    ³  message box to WS-MES-LINE. If WS-MES-LINE is zero the   ³
      *    ³  default will be 20.                                      ³
      *    ³                  PERFORM DISPLAY-MESSAGE                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *           T H I S   R O U T I N E   I S   U S E D   T O
      *            D I S P L A Y   E R R O R   M E S S A G E S
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
      *    ³                        OPT-MESSAGE                        ³
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
      *    ³   MOVE Instruction    TO WS-INSTR.                        ³
      *    ³       [see Accptopt for instruction values]               ³
      *    ³   PERFORM OPT-MESSAGE.                                    ³
      *    ³                                                           ³
      *    ³ This would be displayed as:                               ³
      *    ³      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿   ³
      *    ³      ³          Print transactions (Y/N) [ ]          ³°° ³
      *    ³      ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°° ³
      *    ³        °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° ³
      *    ³                                                           ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³                                                           ³
      *    ³ The system will display the message box with the top line ³
      *    ³ as the value of WS-MES-LINE. If WS-MES-LINE is zero the   ³
      *    ³ default will be 20.                                       ³
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
      *    ³                                                           ³
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

       COPY "CLEARW.CRT".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                       DEBTOR-MEMO                         ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ The account number of the Debtor record for which the MEMO³
      *    ³ details are to be displayed must be in W80-ACNO.          ³
      *    ³ PERFORM DEBTOR-MEMO.                                      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "DEBMEM.PRO".

       COPY "DEBTOR.TRN".

       COPY "DATE.CHK".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                                                           ³
      *    ³           R E A D   F I L E S   R O U T I N E S           ³
      *    ³                                                           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AC000              SECTION.

       COPY "CONTROL.RD".

       COPY "DBTRAN.RD1".

       COPY "DEBTOR.RD".

       COPY "DEPART.RD".

       COPY "LEDTRF.RD".

       COPY "PARAM.RD".

       COPY "SHARED.RD".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                                                           ³
      *    ³R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S³
      *    ³                                                           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AD000             SECTION.

       COPY "CONTROL.WR".

       COPY "DBTRAN.WRR".

       COPY "DEBTOR.WR".

       COPY "DEPART.WR".

       COPY "LEDTRF.WR".

       COPY "PARAM.WR".

       COPY "TXTRAN.WR".

       COPY "DEPART.LUP".

       COPY "APAC.HLP".

       COPY "PRINTUTL.AS5".

      *
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
       AY000           SECTION.
      *    *************************************************************
      *    ****             P A R A M E T E R   F I L E             ****
      *    *************************************************************
       AY01.
             MOVE 01                 TO REC-FILE.
             MOVE WS-PARKEY          TO REC-KEY.
             MOVE PAR-RECORD1        TO REC-PARAM.
             GO TO AY50.

       AY02.
             MOVE 02                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-DEBTOR-LOCK THRU READ-DEBTOR-EXIT.
             MOVE DEB-RECORD1        TO REC-DEBTOR.
             GO TO AY50.

       AY04.
             MOVE 04                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE.
             MOVE TRA-RECORD1        TO REC-DBTRAN.
             GO TO AY50.

       AY19.
             MOVE 19                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE.
             MOVE TAX-RECORD1        TO REC-TXTRAN.
             GO TO AY50.

       AY38.
             MOVE 38                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
             MOVE DPT-RECORD         TO REC-DEPART.
             GO TO AY50.

       AY39.
             MOVE 39                 TO REC-FILE.
             MOVE WS-NETKEY          TO REC-KEY.
             MOVE NET-RECORD         TO REC-NETWORK.
             GO TO AY50.

       AY40.
             MOVE 40                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
             MOVE XFR-REC            TO REC-LEDTRF.
             GO TO AY50.

       AY49.
             MOVE 99                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE SPACES             TO REC-DETAIL.
             PERFORM AY50 THRU AY59.
             ADD 1                   TO WS-TRANS.
           IF WS-RECOVER > 95
               CLOSE RECOVER
               OPEN OUTPUT RECOVER
               CLOSE RECOVER
               OPEN I-O RECOVER
               MOVE ZERO             TO WS-RECOVER.
             GO TO AY59.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³         W R I T E   R E C O V E R Y   R E C O R D         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY50.
             ADD 1                   TO WS-RECOVER.
             MOVE WS-RECOVER         TO WS-RECKEY.
             MOVE WS-TRANS           TO REC-TRAN.
             WRITE REC-RECORD.
           IF WS-STATUS NOT = "00"
               MOVE SPACES           TO WS-ERR-MES
               STRING "Write error Recovery file - Status " DELIMITED SIZE WS-STATUS DELIMITED SIZE INTO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               STOP RUN.

       AY59.
             EXIT.
      *    *************************************************************
      *    ****        Start processing transaction
      *    *************************************************************
       AY60.
             MOVE 1                  TO WS-COUNT.
             MOVE 5                  TO WS-SHARED.
             PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      *    *************************************************************
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *    *************************************************************
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-USERS < 24
               MOVE 1                TO WS-SUB
               MOVE ZERO             TO WS-WAIT
               GO TO AY62.
      *    *************************************************************
      *    ****   Q   F U L L  -  W A I T   F O R   4 S E C O N D S
      *    *************************************************************
             DISPLAY "WAITING" AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red HIGHLIGHT.
             COMMIT.
             ACCEPT WS-STIME FROM TIME.
             MOVE 400                TO WS-WAIT.
             PERFORM LOCK-REC-LOOP.
             DISPLAY SPACE AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.
             GO TO AY60.

       AY61.
             MOVE "DB"               TO PAR-PROG(WS-USUB).
             MOVE LS-USER            TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
             MOVE 1                  TO WS-NETKEY.
             PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             PERFORM AY39 THRU AY59.
             GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N   P R O G R E S S
      *
       AY62.
           IF NOT(PAR-PROG(WS-SUB) = SPACES OR PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R   O R   S T O C K   F I L E S
      *                     B E I N G   U P D A T E D
      *
               IF NOT(PAR-PROG(WS-SUB) = SPACES)
                   IF PAR-PROG(WS-SUB) = "DB" OR "DS"
      *
      *    ****   Y E S   -   S E T   W A I T P E R I O D
      *
                       GO TO AY62-WAIT
                   ELSE
                       ADD 1         TO WS-SUB
                       GO TO AY62
                   END-IF
               ELSE
      *
      *    ****   I S   T H I S   P R O G R A M   I N   T H E   Q
      *
               IF PAR-USR(WS-SUB) = LS-USER
      *
      *    ****   I S   I T   N E X T   I N   L I N E   T O   P R O C E S S
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
             MOVE LS-USER            TO PAR-USR(WS-SUB).
             MOVE WS-SUB             TO PAR-USERS.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             GO TO AY62-CHECK.
      *
      *    ****   S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
             MOVE 300                TO WS-WAIT.
           IF NOT(PAR-USR(WS-SUB) = LS-USER)
               IF WS-SUB < 24
                   ADD 1             TO WS-SUB
                   GO TO AY62.

       AY62-CHECK.
           IF WS-WAIT > ZERO
               COMMIT
               DISPLAY "Waiting" AT 5072 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK
               ACCEPT WS-STIME FROM TIME
               PERFORM LOCK-REC-LOOP
               DISPLAY "Waiting" AT 5072 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK
               GO TO AY60.
             DISPLAY SPACE AT 5072 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.

       AY63.
             MOVE WS-SUB             TO WS-USUB.
             GO TO AY61.

       AY70.
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 2.
      *
             PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
             PERFORM AY49 THRU AY59.
             MOVE 1                  TO WS-USUB.

       AY72.
           IF NOT(PAR-USR(WS-USUB) = LS-USER)
               IF WS-USUB < 24
                   ADD 1  TO WS-USUB
                   GO TO AY72
               ELSE
                   GO TO AY85.

       AY75.
             MOVE SPACES             TO PAR-PROG(WS-USUB) PAR-USR(WS-USUB).
           IF WS-USUB < 24
               ADD 1 WS-USUB         GIVING WS-SUB
           ELSE
               GO TO AY80.
           IF NOT(PAR-PROG(WS-SUB) = SPACES)
               MOVE PAR-PROG(WS-SUB) TO PAR-PROG(WS-USUB)
               MOVE PAR-USR(WS-SUB)  TO PAR-USR(WS-USUB)
               ADD 1                 TO WS-USUB
               GO TO AY75.

       AY80.
             SUBTRACT 1 FROM WS-USUB GIVING PAR-USERS.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.

       AY85.
             COMMIT.

       AY999.
             EXIT.
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
      *        MOVE DEB-TYPE         TO W80-TYPE
      *
      *    ****    Move the Debtor Record to working storage area
      *
               MOVE DEB-RECORD1      TO W40-DEBTREC.

       CA160-EXIT.
             EXIT.

      *
      *    ****    G E N E R A T E   A   D E B T O R
      *            T R A N S A C T I O N
      *
       GENERATE-DEBTOR-TRAN SECTION 2.

       DEBTOR-TRAN.
             PERFORM AY60 THRU AY999.
             MOVE 0                  TO WS-ACTION.
             PERFORM AY02 THRU AY59.

       LINK-DEBTOR-TRANS.
             INITIALIZE TRA-RECORD1.
             MOVE W20-DTE            TO TRA-DATE.
           IF W90-CODE = 01 OR 02
               MOVE W20-DTE          TO DEB-ACTIVE.
           IF W90-CODE = 02
               MOVE W20-DTE          TO DEB-PMT
               MOVE W90-CR           TO DEB-LAST-PMT.
             MOVE W90-CODE           TO TRA-CODE.
             MOVE W90-REF            TO TRA-REF TRA-REG.
             MOVE DEB-ACNO           TO TRA-AC.
             SUBTRACT W90-CR FROM W90-DB GIVING TRA-VALUE.
           IF NOT(W90-CODE = 02 OR 15 OR 16 OR 20)
               IF W00-ACTION = 1
                   MULTIPLY -1 BY W90-VAT(1) GIVING TRA-TAX
                   MULTIPLY -1 BY W90-EX     GIVING TRA-TAXFREE
               ELSE
                   MOVE W90-EX       TO TRA-TAXFREE
                   MOVE W90-VAT(1)   TO TRA-TAX
               END-IF
               ADD TRA-TAX           TO W00-T-VAT W90-VATTOT.
               MOVE TRA-TAX          TO W90-TX.
           IF OPEN-ITEM
               IF TRA-VALUE < ZERO
                   MOVE W75-ALLOCATED TO TRA-ALLOCATED
           ELSE
               IF NOT(TRA-DATE > W30-120)
                   MOVE 5            TO TRA-AGE
               ELSE
               IF NOT(TRA-DATE > W30-90)
                   MOVE 4            TO TRA-AGE
               ELSE
               IF NOT(TRA-DATE > W30-60)
                   MOVE 3            TO TRA-AGE
               ELSE
               IF NOT(TRA-DATE > W30-30)
                   MOVE 2            TO TRA-AGE
               ELSE
               IF NOT(TRA-DATE > W30-CUR)
                   MOVE 1            TO TRA-AGE.
           IF TRA-VALUE > ZERO
               IF NOT(TRA-CODE = 17 OR 20) 
                   IF NOT(TRA-AGE = ZERO)
                       IF TRA-AGE = 5
                           ADD TRA-VALUE      TO DEB-120
                           SUBTRACT TRA-VALUE FROM W90-120
                       ELSE
                       IF TRA-AGE = 4
                           ADD TRA-VALUE      TO DEB-90
                           SUBTRACT TRA-VALUE FROM W90-90
                       ELSE
                       IF TRA-AGE = 3
                           ADD TRA-VALUE      TO DEB-60
                           SUBTRACT TRA-VALUE FROM W90-60
                       ELSE
                       IF TRA-AGE = 2
                           ADD TRA-VALUE      TO DEB-30
                           SUBTRACT TRA-VALUE FROM W90-30
                       ELSE
                       IF TRA-AGE = 1
                           ADD TRA-VALUE      TO DEB-CUR
                           SUBTRACT TRA-VALUE FROM W90-CUR.
             PERFORM WRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
             MOVE 1                  TO WS-ACTION.
             PERFORM AY04 THRU AY59.
           IF NOT(TRA-TAX = ZERO) OR (TRA-CODE = 01) OR (TRA-CODE > 02 AND < 05) OR (TRA-CODE = 20)
               MOVE 1                TO TAX-ACTYPE
               MOVE TRA-CODE         TO TAX-CODE
               MOVE TRA-DATE         TO TAX-DATE
               MOVE TRA-REF          TO TAX-REF
               MOVE TRA-VALUE        TO TAX-VALUE
               MOVE TRA-TAXFREE      TO TAX-TAXFREE
               MOVE 1                TO WS-S9
               PERFORM DEBTOR-TAX
               MOVE TRA-AC           TO TAX-AC
               MOVE ZERO             TO TAX-SEQ
               IF W90-CR > ZERO
                   MOVE "1"          TO TAX-TYPE
               ELSE
                   MOVE "2"          TO TAX-TYPE
              END-IF
              PERFORM WRITE-TXTRAN THRU WRITE-TXTRAN-EXIT
              MOVE 1                 TO WS-ACTION
              PERFORM AY19 THRU AY59.
              ADD TRA-VALUE          TO W00-T-VAL W00-T-MTDV W00-T-YTDV.
              ADD 1                  TO W00-T-DAY W00-T-MTD  W00-T-YTD.
           IF WX-INT = "Y"
               PERFORM DEBTOR-INTEGRATE.
             PERFORM UPDATE-TRAN-RECORD.
           IF W90-CR > ZERO
               GO TO DEBTOR-CR-TRANS.
             ADD W90-DB              TO DEB-DEBT.
             ADD W90-DB              TO DEB-BAL.
           IF W90-CODE = 01
               ADD W90-DB            TO DEB-PURYTD DEB-MTHPUR(W20-MONTH)
           ELSE
           IF W90-CODE = 20
               ADD W90-DB            TO DEB-INT.
             GO TO DEBTOR-TRAN-EXIT.

       DEBTOR-TAX.
             MOVE W90-TAXVAL(WS-S9)  TO TAX-VAL(WS-S9).
             MOVE W90-VAT(WS-S9)     TO TAX-VAT(WS-S9).
           IF WS-S9 < 12
               ADD 1                 TO WS-S9
               GO TO DEBTOR-TAX.

       DEBTOR-INTEGRATE.
             MOVE W20-DTE            TO WX-DATE.
             MOVE "  DA"             TO WX-LEDG.
             MOVE W90-CODE           TO WX-TRN.
             MOVE W00-E-DESC         TO WX-DESC.
             MOVE "- DEBTORS"        TO WX-BOOK.
             MOVE TRA-VALUE          TO WX-VALUE.
             MOVE WX-DEBGL           TO WX-AC.
             MOVE 01                 TO WX-TYPE.
             PERFORM GL-TRF-UPDATE.

       DEBTOR-CR-TRANS.
             ADD W90-CR              TO DEB-CRED.
           IF W90-CODE = 04
               SUBTRACT W90-CR       FROM DEB-PURYTD DEB-MTHPUR(W20-MONTH).
           IF W90-CODE = 17
               SUBTRACT W90-CR       FROM DEB-INT
               SUBTRACT W90-CR       FROM DEB-BAL.
             MOVE W90-CR             TO W25-VALUE.
             GO TO DEBTOR-TRAN-EXIT.
      
       DEBTOR-CREDITS.
             SUBTRACT W90-CR         FROM DEB-BAL.
           IF W25-VALUE < DEB-120
               SUBTRACT W25-VALUE    FROM DEB-120
               ADD W25-VALUE         TO W90-120
               GO TO DEBTOR-TRAN-EXIT.
             SUBTRACT DEB-120        FROM W25-VALUE.
             ADD DEB-120             TO W90-120.
             MOVE ZERO               TO DEB-120.
           IF W25-VALUE < DEB-90
               SUBTRACT W25-VALUE    FROM DEB-90
               ADD W25-VALUE         TO W90-90
               GO TO DEBTOR-TRAN-EXIT.
             SUBTRACT DEB-90         FROM W25-VALUE.
             ADD DEB-90              TO W90-90.
             MOVE ZERO               TO DEB-90.
           IF W25-VALUE < DEB-60
               SUBTRACT W25-VALUE    FROM DEB-60
               ADD W25-VALUE         TO W90-60
               GO TO DEBTOR-TRAN-EXIT.
             SUBTRACT DEB-60         FROM W25-VALUE.
             ADD DEB-60              TO W90-60.
             MOVE ZERO               TO DEB-60.
           IF W25-VALUE < DEB-30
               SUBTRACT W25-VALUE    FROM DEB-30
               ADD W25-VALUE         TO W90-30
               GO TO DEBTOR-TRAN-EXIT.
             SUBTRACT DEB-30         FROM W25-VALUE.
             ADD DEB-30              TO W90-30.
             MOVE ZERO               TO DEB-30.
           IF W25-VALUE < DEB-CUR
               SUBTRACT W25-VALUE    FROM DEB-CUR
               ADD W25-VALUE         TO W90-CUR
               GO TO DEBTOR-TRAN-EXIT.
             SUBTRACT DEB-CUR        FROM W25-VALUE.
             ADD DEB-CUR             TO W90-CUR.
             MOVE ZERO               TO DEB-CUR.
           IF W90-CODE = 02
               IF W25-VALUE < DEB-INT
                   SUBTRACT W25-VALUE FROM DEB-INT
                   ADD W25-VALUE     TO W90-INT
                   GO TO DEBTOR-TRAN-EXIT
               ELSE
                   ADD DEB-INT       TO W90-INT
                   MOVE ZERO         TO DEB-INT.

       DEBTOR-TRAN-EXIT.
             EXIT.

       COPY "LEDTRF.UPD".

      *
      *    ****    D E B T O R  C R E D I T   N O T E  A N A L I S E D
      *
       CK000        SECTION 5.
       CK00.
             PERFORM ERASE-SCREEN.
             MOVE 4                  TO W90-CODE.
             PERFORM GET-DEBTOR-TRAN-DESC.

       CK04.
             PERFORM ERASE-SCREEN.
             DISPLAY S50.
             DISPLAY S50-DATA.
             MOVE SPACES             TO W80-ACNO.

       CK05.
      *
      *    ****    Update the BATCH number
      *
             DISPLAY "Batch:" AT 0502.
             MOVE 1                  TO WS-NETKEY.
             PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             MOVE DEB-BATCH          TO W85-BATCH.
           IF DEB-BATCH = 99999
               MOVE 1                TO DEB-BATCH
           ELSE
               ADD 1                 TO DEB-BATCH.
             MOVE W85-BATCH          TO WS-BATCH.
             PERFORM REWRITE-CONTROL-UNLOCK THRU WRITE-CONTROL-EXIT.
             DISPLAY WS-BATCH AT 0509 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
      *
      *    ****    Clear the working storage fields
      *
             MOVE ZERO               TO W90-TTAX W90-TBDT W90-TCAP W90-TBCR W90-TEXP W90-VATTOT.

       CK06.
             MOVE ZERO               TO W90-30 W90-CUR W90-60 W90-BCR W90-90 W90-BDT W90-120.
             MOVE 0                  TO WS-PAGE.
             MOVE 0                  TO WS-ADVANCE.
             MOVE 2                  TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.

       CK10.
      *
      *    ****    Print the audit trail HEADING
      *
             MOVE "H"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DET1.
             MOVE "PAGE:"            TO R-PAGE.
             MOVE "DATE:"            TO R-DATE.
             ADD 1                   TO WS-PAGE.
             MOVE W12-TODAY          TO R-DTE.
             MOVE WS-PAGE            TO R-P-NO.
             MOVE "DEBTOR CREDIT NOTES - AUDIT TRAIL" TO R-H5.
             MOVE "BATCH:"           TO R-H6.
             MOVE WS-BATCH           TO R-BATCH.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET1.
             MOVE 0                  TO WS-ADVANCE.
             MOVE "E"                TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET1.
             MOVE W95-COMP           TO R-CO.
             MOVE "H"                TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE "X"                TO W02-PRN-TYPE.
             MOVE 0                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.

       CK15.
             MOVE "H"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DET1.
             MOVE "AC/NO"            TO I-H1.
             MOVE "NAME"             TO I-H2.
             MOVE "DATE"             TO I-H3.
             MOVE "REF"              TO I-H4.
             MOVE "VALUE"            TO I-H5.
             MOVE "TAX"              TO I-H6.
             MOVE "TOTAL"            TO I-H7.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET1.
             MOVE "D"                TO W02-PRN-TYPE.

       CK20.
             MOVE 1                  TO WS-S9.

       CK22.
             MOVE ZERO               TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
           IF WS-S9 < 12
               ADD 1                 TO WS-S9
               GO TO CK22.
             MOVE ZERO               TO W90-DB W90-CAP W90-DISC W90-CR W90-TX W90-EX W40-SDISC.
             MOVE SPACES             TO W90-REF W80-ACNO.
             MOVE W12-TODAY          TO W90-IDTE.

       CK25.
             DISPLAY "F1"                        AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     "=Help,"                            WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                     "F2"                                WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     " to do Account Lookup,"            WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                     "F9"                                WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     " Debtor Lookup (Any Word),"        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                     "Esc"                               WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     " to exit"                          WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             ACCEPT S36 AT 0514.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY   MOVE SPACES TO W80-ACNO
                                GO TO CK75
                 WHEN F1-KEY    PERFORM HELP-ROUTINE
                 WHEN F2-KEY    MOVE SPACE     TO W40-LUP
                                PERFORM AA50
                 WHEN F9-KEY    MOVE "A"       TO W40-LUP
                                PERFORM AA50
                 WHEN OTHER     AA900-ALARM
                                GO TO CK25
              END-EVALUATE
              DISPLAY S36 AT 0515
              IF W80-ACNO = SPACES
                  GO TO CK25.
             PERFORM CLEAR-L50.
           IF W80-ACNO = SPACES
               GO TO CK75.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W80-ACNO BY VALUE WS-LENGTH
                  RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             MOVE W80-ACNO           TO DEB-ACNO.
             PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR = 1
               MOVE WS-ERR2          TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CK25.
           IF WS-ERROR = 2
               MOVE WS-ERR1          TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CK25.
      *    *************************************************************
      *    ****         T E S T   F O R   I N D I V I D U A L       ****
      *    *************************************************************
           IF W40-TYPE = 1
               MOVE "D"              TO W40-INSTR
               CALL "DTP\DTPNAME" USING W40-COMPANY LS-USER-ID
           ELSE
               MOVE W40-NAME         TO W40-DSP-NAME.
             DISPLAY W40-DSP-NAME AT 0528 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             COMPUTE W40-CUR = DEB-BAL - (DEB-30 + DEB-60 + DEB-90 + DEB-120).
             DISPLAY S52.
             DISPLAY S52-DATA.
             DISPLAY "F9"                             AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                       " Memo/Rmks,"                          WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                  "Any"                       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                     " other key to continue" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN F9-KEY     MOVE DEB-ACNO TO W80-ACNO
                                 PERFORM DEBTOR-MEMO
               END-EVALUATE.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO CK20.

       CK30.
             ACCEPT S51A.
             MOVE W90-IDTE           TO W10-EDTE.
             PERFORM CHECK-DATE.
           IF WS-ERROR = 1
               MOVE "Date"           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CK30.
           IF W90-REF = SPACES
               MOVE "Ref No"         TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CK30.
           IF W22-DTE1 < WS-VAT-DATE(1)
               MOVE 7                TO WS-VAT-SUB
           ELSE
               MOVE 1                TO WS-VAT-SUB.
             MOVE W05-VAT(WS-VAT-SUB) TO W05-RATE.
             MOVE ZERO               TO W90-TX.
           IF W90-CAP NOT > ZERO AND W90-EX NOT > ZERO
               PERFORM CK80 THRU CK90
               IF WS-ERROR = 1
                   MOVE "Value"      TO WS-ERR-MES
                   PERFORM ERROR-MESSAGE
                   GO TO CK25
               END-IF
           ELSE
               COMPUTE W90-TX ROUNDED = (W90-CAP * W05-RTE).
             DISPLAY S51AB.
             MOVE W90-CAP            TO W90-CR W90-TAXVAL(WS-VAT-SUB).
             ADD W90-EX              TO W90-CR.
             ADD W90-TX              TO W90-CR.
             MOVE W90-TX             TO W90-VAT(WS-VAT-SUB).
             DISPLAY S51C.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO CK30.
           IF (W90-EX = ZERO) AND (W90-CAP = ZERO)
               MOVE "N"              TO WS-OPTION
               GO TO CK38.
           IF DEB-SDISC = ZERO
               GO TO CK32.

       CK31.
             MOVE "Does settlement discount apply Y/N?  [Y]" TO WS-OPT-MES.
             MOVE "Y"                TO WS-OPTION.
             PERFORM OPT-MESSAGE.
           IF NOT(WS-OPTION = "Y" OR "N")
               GO TO CK31.
           IF WS-OPTION = "Y"
               MOVE DEB-SDISC        TO W40-SDISC
               MOVE W05-DSETT(W40-SDISC) TO W90-DISC.

       CK32.
             MOVE "Allocate to selected Sales Analyses Y/N  [ ]" TO WS-OPT-MES.
             DISPLAY "Press "      AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                           "Esc"           WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                              " to CANCEL" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.

       CK35.
             PERFORM OPT-MESSAGE.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  GO TO CK20
                 WHEN OTHER    AA900-ALARM
                               GO TO CK32
               END-EVALUATE.
           IF NOT(WS-OPTION = "N" OR "Y")
              GO TO CK32.

       CK38.
             MOVE 1401   TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
             PERFORM CLEAR-L50.
             MOVE DEB-ACNO           TO I-AC.
             MOVE W60-NAME           TO I-NAME.
             MOVE W90-IDTE           TO I-DATE.
             MOVE W90-REF            TO I-REF.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET1.
             ADD W90-EX W90-CAP GIVING W90-INT.
             DISPLAY "Sales"  AT 1410.
             DISPLAY "Ledger        Description                                 Value" AT 1510.
             PERFORM AY60 THRU AY999.
           IF WS-OPTION = "N"
               IF (W90-EX = ZERO) AND (W90-CAP = ZERO)
                   GO TO CK60
               ELSE
                   MOVE "XXXX"       TO DPT-CODE
                   MOVE SPACES       TO W40-LEDG
                   GO TO CK50.
       CK40.
             DISPLAY "Balance :" AT 1910.
             MOVE W90-INT            TO W95-B.
             DISPLAY W95-B AT 1920 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             MOVE ZERO               TO W40-INT.
             MOVE SPACES             TO W40-LEDG.
             DISPLAY "F2"                                               AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                       "=Sales Ledger Lookup, "                                 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                             "Esc"                              WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                                " to post as UNSPECIFIED SALES" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
       CK45.
             ACCEPT W40-LEDG AT 1712 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY   MOVE SPACES TO W40-LEDG
                 WHEN F2-KEY    PERFORM SAVE-SCREEN
                                PERFORM DEPART-VIEW
                                PERFORM RESTORE-SCREEN
                                GO TO CK45
                 WHEN OTHER     AA900-ALARM
                                GO TO CK45
               END-EVALUATE.
           IF (W40-LEDG = "0000" OR "YYYY") OR (W40-SCDE = "VAT")
               GO TO CK45.
             PERFORM CLEAR-L50.
           IF W40-LEDG = SPACES
               MOVE "XXXX"           TO DPT-CODE
           ELSE
               MOVE W40-LEDG         TO DPT-CODE.

       CK50.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
           IF DPT-DESC = SPACES
               MOVE "No Record"      TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CK45.
             DISPLAY DPT-DESC AT 1724 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO CK40.
       CK52.
           IF NOT(W40-LEDG = ZERO)
               ACCEPT S35 AT 1862
           ELSE
               MOVE W90-INT          TO W40-INT
               DISPLAY S35 AT 1862.
           IF W40-INT > W90-INT
               MOVE "Value allocated exceeds Tran-balance" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CK52.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               IF W40-LEDG = SPACES
                   GO TO CK45
               ELSE
                   GO TO CK52.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
             MOVE W40-LEDG           TO TL-LEDG.
             MOVE DPT-DESC           TO TL-NME.
             PERFORM AY38 THRU AY59.
             MULTIPLY -1 BY W40-INT  GIVING I-CAP
             ADD W40-INT             TO DPT-RET-DAY DPT-RET-MTD DPT-RET-YTD.
      *
      *    G/LEDGER INTEGRATION
      *
           IF WX-INT = "Y"
               MOVE W20-DTE          TO WX-DATE
               MOVE W40-LEDG         TO WX-LEDG
               MOVE "Credit Note"    TO WX-DESC
               MOVE "- DEBTORS"      TO WX-BOOK
               MOVE W40-INT          TO WX-VALUE
               MOVE 05               TO WX-TYPE
               MOVE DPT-SALE-GL      TO WX-AC
               PERFORM GL-TRF-UPDATE.
             PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
           IF W02-LINAGE < (W02-PRN-LENGTH - 2)
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM CK10 THRU CK15.
             MOVE SPACES             TO R-DET1.
             SUBTRACT W40-INT        FROM W90-INT.
           IF W90-INT > ZERO
               MOVE 1601             TO CPOS
               PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24
               GO TO CK40.

       CK60.
             PERFORM GET-DEBTOR-TRAN-CODE THRU GET-DEBTOR-TRAN-EXIT.
             PERFORM AY01 THRU AY59.
             MOVE 0                  TO WS-ACTION.
             PERFORM AY02 THRU AY59.
             MOVE "A"                TO WS-SKIP.
      *      MOVE W90-TX  TO W90-VAT(WS-VAT-SUB).
             PERFORM GENERATE-DEBTOR-TRAN.
             ADD W90-CR              TO W90-TBCR.
             MOVE "N"                TO WS-SKIP.

       CK60A.
             MOVE "'M'td,'C'ur, '3'0, '6'0, '9'0 or '1'20days  [M]" TO WS-OPT-MES.
             MOVE "M"                TO WS-OPTION.
             PERFORM OPT-MESSAGE.
           IF NOT(WS-OPTION = "M" OR "C" OR "3" OR "6" OR "9" OR "1")
               GO TO CK60A.
             SUBTRACT W90-CR         FROM DEB-BAL.
             ADD W90-CR              TO W90-BCR.
             MULTIPLY -1 BY W90-CR   GIVING W90-INT.
           IF WS-OPTION = "M"
               NEXT SENTENCE
           ELSE
           IF WS-OPTION = "C"
               ADD W90-INT           TO DEB-CUR W90-CUR
           ELSE
           IF WS-OPTION = "3"
               ADD W90-INT           TO DEB-30 W90-30
           ELSE
           IF WS-OPTION = "6"
               ADD W90-INT           TO DEB-60 W90-60
           ELSE
           IF WS-OPTION = "9"
               ADD W90-INT           TO DEB-90 W90-90
           ELSE
           IF WS-OPTION = "1"
               ADD W90-INT           TO DEB-120 W90-120.

       CK60B.
             ADD 121 DEB-CAT         GIVING W90-RESULT.
             DIVIDE W90-RESULT BY 2  GIVING W90-CATKEY REMAINDER W90-CS.
             ADD 1                   TO W90-CS.
             MOVE W90-CATKEY         TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             PERFORM AY01 THRU AY59.
             ADD W90-CR              TO PAR-CCRMTD(W90-CS) PAR-CCRYTD(W90-CS).
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.

       CK61.
             ADD W90-DB              TO W90-TBDT.
             ADD W90-CAP             TO W90-TCAP.
             ADD W90-EX              TO W90-TEXP.
             ADD W90-TX              TO W90-TTAX.
             MOVE "TOTAL"            TO I-NAME.
             MOVE W90-REF            TO I-REF.
             COMPUTE I-CAP = (W90-CAP + W90-EX) * -1.
             MULTIPLY -1 BY W90-TX   GIVING I-VAT.
             MULTIPLY -1 BY W90-CR   GIVING I-TOT.
           IF W02-LINAGE < (W02-PRN-LENGTH - 2)
               MOVE 2                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99   TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM CK10 THRU CK15.
             MOVE SPACES             TO R-DET1.
             PERFORM REWRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.
             PERFORM CK70-NEW THRU CK71.
             PERFORM AY70 THRU AY999.

       CK62.
             PERFORM ERASE-SCREEN.
             DISPLAY S50.
             DISPLAY S50-DATA.
             MULTIPLY -1 BY W90-TBCR GIVING W100-BAL.
             DISPLAY "Batch Total: " AT 2112 W100-BAL WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             MOVE ZERO               TO W90-30 W90-CUR W90-60 W90-BCR W90-90 W90-BDT W90-120.
             GO TO CK20.

       CK70-NEW.
             MULTIPLY W90-BCR BY -1  GIVING W25-VALUE.
             ADD W25-VALUE           TO DEB-OUT.
             ADD W90-BDT             TO DEB-DT.
             ADD W90-BCR             TO DEB-CR.
             ADD W90-120             TO DEB-FOUR.
             ADD W90-90              TO DEB-THREE.
             ADD W90-60              TO DEB-TWO.
             ADD W90-30              TO DEB-ONE.
             ADD W90-CUR             TO DEB-MTD.

       CK71.
             MOVE 9                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             PERFORM AY01 THRU AY59.
             MOVE WS-VAT-SUB         TO W05-V-RATE.
             MOVE W05-VAT-CODE       TO DPT-CODE.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
             PERFORM AY38 THRU AY59.
      *----------------------------------------------------------------*
      *                                                                *
      *  This routine must be changed to cater for multiple VAT Rates  *
      *  See "BD185-VAT" in credit notes program "CRNF07".             *
      *                                                                *
      *----------------------------------------------------------------*
             ADD W90-EX              TO PAR-CNGS-DAY PAR-CNGS-MTD PAR-CNGS-YTD.
             ADD W90-CAP             TO PAR-CVAT-DAY PAR-CVAT-MTD PAR-CVAT-YTD DPT-V-RET-DAY DPT-V-RET-MTD DPT-V-RET-YTD.
             ADD W90-TX              TO DPT-O-VRET-DAY DPT-O-VRET-MTD DPT-O-VRET-YTD.
           IF (WX-INT = "Y") 
               IF NOT(W90-TX = ZERO)
                   MOVE W90-TX       TO WX-VALUE
                   MOVE DPT-O-VAT-GL TO WX-AC
                   MOVE 11           TO WX-TYPE
                   PERFORM GL-TRF-UPDATE.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
       CK75.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE "TAXABLE:"         TO I-H8.
             MOVE "EXEMPT:"          TO I-H9.
             MOVE "V.A.T.:"          TO I-H10.
             MOVE "TOTAL:"           TO I-H11.
             MULTIPLY -1 BY W90-TCAP GIVING I-TOTTAX.
             MULTIPLY -1 BY W90-TEXP GIVING I-TOTEXE.
             MULTIPLY -1 BY W90-TTAX GIVING I-TOTGST.
             MULTIPLY -1 BY W90-TBCR GIVING I-TOTBCH.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET1.
             MOVE "S"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             MOVE 0701               TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
             MOVE "New Batch Y/N?  [ ]" TO WS-OPT-MES.
             MOVE SPACE              TO WS-OPTION.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "N"
               GO TO CK999.
             GO TO CK00.

       CK80.
             MOVE "Do you want to adjust - TAX Y/N?  [ ]" TO WS-OPT-MES.
             MOVE SPACE              TO WS-OPTION.
             PERFORM OPT-MESSAGE.
           IF NOT(WS-OPTION = "N" OR "Y")
               GO TO CK80.
           IF WS-OPTION = "N"
               MOVE 1                TO WS-ERROR
               GO TO CK90.
             MOVE ZERO               TO WS-ERROR.

       CK85.
             ACCEPT S51AB.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO CK85.
       CK90.
             EXIT.

       CK999.
             EXIT.
      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT        SECTION 8.
       ZA000-OPEN.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W12-TODAY.
             MOVE LS-TODAY-YYMMDD    TO W12-T-YMD.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

       ZA02.
             OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
             MOVE 1                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY        TO W95-COMP.
      *      MOVE PAR-DMY            TO W12-TODAY.
      *      MOVE PAR-YMD            TO W12-T-YMD.
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

       ZA999-EXIT.
             EXIT.
      *
      *    ****    I / O   E R R O R   M E S S A G E S
      *
       ZB000-ERROR      SECTION 8.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 2          MOVE W02-NETWORK  TO WS-FILE
                               MOVE WS-NETKEY    TO WS-KEY
               WHEN 6          MOVE W02-DEBTOR   TO WS-FILE
                               MOVE DEB-ACNO     TO WS-KEYX
               WHEN 7          MOVE W02-DEPART   TO WS-FILE
                               MOVE DPT-CODE     TO WS-KEYX
               WHEN 15         MOVE WS-PARID     TO WS-FILE
                               MOVE WS-PARKEY    TO WS-KEY
               WHEN 18         MOVE W02-RECOVER  TO WS-FILE
                               MOVE WS-RECKEY    TO WS-KEY
               WHEN 32         MOVE W02-TXTRAN   TO WS-FILE
                               MOVE TAX-KEY      TO WS-KEYX
               WHEN 37         MOVE W02-SHARED   TO WS-FILE
                               MOVE WS-SHARED    TO WS-KEY
               WHEN 40         MOVE W02-LEDTRF   TO WS-FILE
                               MOVE XFR-KEY      TO WS-KEYX
               WHEN OTHER      MOVE "ERROR"      TO WS-FILE
                               MOVE "DTP009"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
