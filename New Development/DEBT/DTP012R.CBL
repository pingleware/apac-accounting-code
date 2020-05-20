      $SET LINKCOUNT"512" GNT"DTP012R.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****       **     ******     *
      *   **    **     **     **    **   **  **     ***    **    **    *
      *   **    **     **     **    **  **    **     **         **     *
      *   **    **     **     *******   **    **     **       **       *
      *   **    **     **     **        **    **     **     **         *
      *   **    **     **     **         **  **      **    **          *
      *   *******      **     **          ****    ******   ********    *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *             A C C O U N T S   R E C E I V A B L E  /           *
      *                                                                *
      *         D E B T O R   R E C E I P T   R E V E R S A L S        *
      *                                                                *
      *       Version 9.04.01 - June 2016                              *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  May 2006 - New field included in Accounts Receivable          *
      *             for recording number of outstanding Jobs           *
      *             linked to the account. Additional filler           *
      *             added for future expansion.                        *
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
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     DTP012R.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   JULY 2004.

                COPYRIGHT NOTICE: COPYRIGHT (C) 2004 - 2017
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

       COPY "AUDIT.SL".

       COPY "CHEQUE.SL".

       COPY "CONTROL.SL".

       COPY "DBTRAN.SL".

       COPY "DEBTOR.SL".

       COPY "DEBTRN.SL".

       COPY "DEPART.SL".

       COPY "LEDTRF.SL".

       COPY "PARAM.SL".

       COPY "RECEIPTS.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

       COPY "TXTRAN.SL".

          SELECT CSHDRW  ASSIGN W02-CSHDRW
                          ORGANIZATION SEQUENTIAL
                          ACCESS SEQUENTIAL.
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

       COPY "AUDIT.FDE".

       COPY "CHEQUE.FD".

       COPY "CONTROL.FDE".

       COPY "DBTRAN.FDE".

       COPY "DEBTOR.FDE".

       COPY "DEBTRN.FD".

       COPY "DEPART.FDE".

       COPY "LEDTRF.FDE".

       COPY "PARAM.FDE".

       COPY "RECEIPTS.FDE".

       COPY "RECOVER.FD".

       COPY "SHARED.FDE".

       COPY "TXTRAN.FDE".

       FD  CSHDRW    LABEL RECORD OMITTED.
       01  CSH-REC.
           03  CSH-OPEN        PIC  X(06).
      *
       FD  PRNREP    LABEL RECORD OMITTED
             LINAGE IS WS-PGE-LENGTH.
       01  R-L1.
           03  R-DET2          PIC  X(80).
       01  CSH-L.
           03  CSH-DATA        PIC  X(80).
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
       77  WS-AUDKEY               PIC  9(06)    COMP-5.
       77  WS-DBTKEY               PIC  9(06)    COMP-5.
       77  WS-NETKEY               PIC  9(06)    COMP-5.
       77  WS-PARKEY               PIC  9(06)    COMP-5.
       77  WS-RECKEY               PIC  9(06)    COMP-5.
       77  WS-RECOVER              PIC  9(06)    COMP-5.
       77  WS-SHARED               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-TRANS                PIC  9(04)    COMP-5 VALUE 1.
       77  WS-MTHEND               PIC  9(02)    COMP-5.
       77  WS-CLS-AUD              PIC  X(01)           VALUE "Y".
       77  WS-TME2                 PIC  9(08).
       77  WS-PORT                 PIC  9(05)           VALUE 259.
      * 1004.
       77  WS-DRAW1                PIC  9(05)           VALUE 260.
       77  WS-OPEN                 PIC  X(01)           VALUE X"01".
       77  WS-CLOSE                PIC  X(01)           VALUE X"00".
       77  WS-FIN-RATE             PIC S9(03)V99 COMP-3.
       77  WS-PERIOD               PIC  9(02).
       77  WS-NTP                  PIC  9(01).
       77  WS-PAGE                 PIC  9(04)    COMP-5.
       77  WS-PGE-LENGTH           PIC  9(02)           VALUE 66.
       77  WS-PRN-LENGTH           PIC  9(02)           VALUE 62.
       77  WS-COND                 PIC  X(06).
       77  WS-NORM                 PIC  X(06).
       77  WS-EXPP                 PIC  X(06).
       77  WS-ECAN                 PIC  X(06).
       77  WS-8LPI                 PIC  X(06).
       77  WS-6LPI                 PIC  X(06).
       77  WS-10CPI                PIC  X(06).
       77  WS-12CPI                PIC  X(06).
       77  WS-17CPI                PIC  X(06).
       77  WS-KEYSTR               PIC  9(06)    COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-ALLOCATE             PIC  X(01).
       77  WS-CSLIP                PIC  X(01).
       77  WS-BNK-IND              PIC  X(01).
       77  WS-SKIP                 PIC  X(01)           VALUE "N".
       77  WS-TAX                  PIC  X(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-ERROR                PIC  9(01)           VALUE ZERO.
       77  WS-EKEY                 PIC  9(06)    COMP-5.
       77  WS-CHAR1                PIC  X(01).
       77  WS-CHAR2                PIC  X(01).
       77  WS-COMP                 PIC  9(01).
       77  WS-INDS                 PIC  9(01)           VALUE ZERO.
       77  WS-INDD                 PIC  9(01)           VALUE ZERO.
       77  WS-INDI                 PIC  9(01)           VALUE ZERO.
       77  WS-BATCH                PIC  9(06)    COMP-5.
       77  WS-ERR1                 PIC  X(22)           VALUE "Invalid Account Number".
       77  WS-ERR2                 PIC  X(09)           VALUE "No Record".
       77  WS-E                    PIC  X(15)           VALUE "**END** - ENTER".
       77  WS-AC                   PIC  X(07)           VALUE "ACCOUNT".
       77  PRG-DEBT-LUP            PIC  X(12)           VALUE "DTP\DTPLOOK".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.
       77  WS-DRAW                 PIC  9(02).
      *
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.       ³
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
           03  CRT-PROGRAM           PIC  X(15)    VALUE "DTP012".
      *
      *    Screen Heading
      *
           03  CRT-HEADING           PIC  X(40)    VALUE "ACCOUNTS RECEIVABLE - PAYMENTS REVERSAL".
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

      *
       01  WS-ENTRY.
           03  WS-ENT0             PIC  X(08).
           03  WS-TYPE0            PIC  X(01).
           03  WS-KEY0             PIC  9(06)    COMP-5.
       01  WS-ENTRY1.
           03  WS-ENT1             PIC  X(08).
           03  WS-TYPE1            PIC  X(01).
           03  WS-KEY1             PIC  9(06)    COMP-5.

       COPY "WS.WS".

       01  WS-HELP.
           03  WS-MODULE           PIC  X(03) VALUE "DTP".
           03  WS-PROG             PIC  X(03) VALUE "012".

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

       01  W02-FILE-IDS.
           03  W02-PRINTER.
               05  W02-REPORT      PIC X(07) VALUE "DTP012.".
               05  W02-USER        PIC X(05) VALUE SPACES.
           03  W02-CSHDRW          PIC X(04) VALUE "COM1".

       COPY "W05.VAT".

       01  W05-DISC.
           03  W05-DISCOUNT.
               05  W05-DSETT       PIC S9(03)V99 COMP-3 OCCURS 10.

       01  W10-SPARES.
           03  W10-DATE            PIC X(10).
           03  W10-EDTE            PIC 99/99/9999 REDEFINES W10-DATE.

       01  W10-REF.
           03  W10-NUM             PIC  9(08).
           03  W10-REF1                           REDEFINES W10-NUM.
               05  FILLER          PIC  X(07).
               05  W10-N1.
                   07  W10-NUM1    PIC  9(01).
           03  W10-REF2                           REDEFINES W10-NUM.
               05  FILLER          PIC  X(06).
               05  W10-N2.
               07  W10-NUM2        PIC  9(02).
           03  W10-REF3                           REDEFINES W10-NUM.
               05  FILLER          PIC  X(05).
               05  W10-N3.
               07  W10-NUM3        PIC  9(03).

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

       01  W15-CASH-SLIP.
           03  W15-L1.
               05  W15-ESC1        PIC  X(01).
               05  W15-DBLE        PIC  X(01).
               05  W15-EXPP1       PIC  X(01).
               05  W15-COMP        PIC  X(32).
               05  W15-ECAN1       PIC  X(01).
           03  W15-L1A.
               05  W15-ESC5        PIC X(01).
               05  W15-DBLE3       PIC X(01).
               05  W15-EXPP4       PIC X(01).
               05  W15-DOCTYP      PIC X(18).
               05  W15-ECAN4       PIC X(01).
           03  W15-L2.
               05  W15-ESC2        PIC  X(01).
               05  W15-DCAN        PIC  X(01).
               05  W15-ADD         PIC  X(32)     VALUE SPACES.
           03  W15-L3.
               05  FILLER          PIC  X(14)     VALUE "Telephone  :".
               05  W15-TEL         PIC  X(14)     VALUE SPACES.
           03  W15-L4.
               05  FILLER          PIC  X(18)     VALUE "Date       :".
               05  W15-DATE        PIC  Z9/99/9999.
           03  W15-L5.
               05  FILLER          PIC  X(08)     VALUE "Assist:".
               05  W15-SMAN        PIC  X(24).
           03  W15-L6.
               05  FILLER          PIC  X(22)     VALUE "Receipt No.:".
               05  W15-RECNO       PIC  X(08).
           03  W15-L7.
               05  FILLER          PIC  X(22)     VALUE "Invoice No.:".
               05  W15-INVNO       PIC  Z(05)9.
           03  W15-L7A.
               05  FILLER          PIC  X(22)     VALUE "Credit Note:".
               05  W15-CRNO        PIC  Z(05)9.
           03  W15-L7B.
               05  FILLER          PIC  X(22)     VALUE "Reference No.:".
               05  W15-REFNO       PIC  X(06).
           03  W15-L8.
               05  FILLER          PIC  X(22)     VALUE "Account No.:".
               05  W15-ACCNO       PIC  X(06).
           03  W15-L9.
               05  W15-ESC3        PIC  X(01).
               05  W15-DBLE2       PIC  X(01).
               05  W15-EXPP2       PIC  X(01).
               05  FILLER          PIC  X(16)     VALUE "    THANK YOU".
               05  W15-ECAN2       PIC  X(01).
           03  W15-L11.
               05  W15-COND1       PIC  X(01).
               05  W15-ITM         PIC  X(12).
               05  FILLER          PIC  X(01)     VALUE SPACE.
               05  W15-QUANT       PIC  Z9.99.
               05  FILLER          PIC  X(01)     VALUE SPACE.
               05  W15-DESC        PIC  X(24).
               05  FILLER          PIC  X(01)     VALUE SPACE.
               05  W15-PRICE       PIC  Z(06)9.99-.
               05  FILLER          PIC  X(01)     VALUE SPACE.
               05  W15-NORM1       PIC  X(01).
           03  W15-L12                            REDEFINES W15-L11.
               05  FILLER          PIC  X(01).
               05  W15-DISCOUNT.
                   07 FILLER       PIC  X(19).
                   07  W15-DISC    PIC  X(19).
                   07  W15-DSC     PIC  Z9.99.
               05  FILLER          PIC  X(14).
           03  W15-L13.
               05  W15-REMK        PIC  X(21).
               05  W15-VAL         PIC  Z(06)9.99-.

       01  W25-CALCS.
           03  W25-RESULT          PIC  9(05)V99.
           03  W25-RESULT1                        REDEFINES W25-RESULT.
               05  W25-WHOLE       PIC  9(05).
               05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2                        REDEFINES W25-RESULT.
               05  W25-KEY         PIC  9(04).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).
           03  W25-TOTAL           PIC S9(09)V99 COMP-3.
           03  W25-VALUE           PIC S9(09)V99 COMP-3.

       COPY "W40.DBT".

       COPY "W40.WS".

       01  W45-TRAN.
           03  W45-CODE            PIC 9(02).
           03  W45-ENG             PIC X(12).
           03  W45-AFR             PIC X(12).
           03  W45-ACT             PIC X(01).

       COPY "FUNCTION.WS".

      *COPY "W50.WS".

       01  W75-KEYS.
           03  W75-S               PIC  9(02)    COMP-5.
           03  W75-S1              PIC  9(02)    COMP-5.
           03  W75-KEY             PIC  9(06)    COMP-5.
           03  W75-ALLOCATED       PIC S9(09)V99 COMP-3.
           03  W75-BALANCE         PIC S9(09)V99 COMP-3.
           03  W75-DNO             PIC  X(06)    OCCURS 18.

       COPY "W75.WS".

       COPY "W85.WS".

       01  W85-BATCHES.
               05  W85-BATCH       PIC  9(06)    COMP-5.

       COPY "W90.DBT".

       01  W95-STM.
           03  W95-COMP            PIC  X(40).
           03  W95-V1.
               05  W95-T           PIC  Z(04)9.99.
           03  W95-V2.
               05  W95-B           PIC  Z(08)9.99-.
           03  W95-D.
               05  W95-DTE         PIC  Z9/99/9999.
           03  W95-POST            PIC  9(04).
           03  W95-TEL             PIC  X(08).

       01  W100-EDIT.
           03  W100-BAL            PIC  Z(08)9.99-.


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

       01  S08.
           03  LINE  6 COLUMN 33 VALUE "RECEIPT REVERSAL" FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE  8 COLUMN  6 VALUE "Account No."
           03          COLUMN 33 VALUE                            "Name:".
           03  LINE  9 COLUMN  6 VALUE "Date       "
           03          COLUMN 33 VALUE                            "Bal :".
           03  LINE 10 COLUMN  6 VALUE "Receipt No.".
           03  LINE 11 COLUMN  6 VALUE "Amount     ".
           03  LINE 13 COLUMN  6 VALUE "Cash       ".
           03  LINE 14 COLUMN  6 VALUE "Cheque     ".
           03  LINE 15 COLUMN  6 VALUE "Bank Trf.  ".
           03  LINE 17 COLUMN  6 VALUE "TOTAL  :".

       01  S10.
           03  LINE 10 COLUMN 50 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"         BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
           03          COLUMN 73 VALUE                        "¿"        BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03  LINE 11 COLUMN 50 VALUE "³ Mtd  : "                       BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
           03          COLUMN 59 PIC             Z(08)9.99- FROM W40-ONE BACKGROUND-COLOR Grey FOREGROUND-COLOR Magenta.
           03          COLUMN 72 VALUE                       " ³"        BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03  LINE 12 COLUMN 50 VALUE "³ Cur  : "                       BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
           03          COLUMN 59 PIC             Z(08)9.99- FROM DEB-CUR BACKGROUND-COLOR Grey FOREGROUND-COLOR Magenta.
           03          COLUMN 72 VALUE                       " ³"        BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03  LINE 13 COLUMN 50 VALUE "³ 30d  : "                       BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
           03          COLUMN 59 PIC             Z(08)9.99- FROM DEB-30  BACKGROUND-COLOR Grey FOREGROUND-COLOR Magenta.
           03          COLUMN 72 VALUE                       " ³"        BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03  LINE 14 COLUMN 50 VALUE "³ 60d  : "                       BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
           03          COLUMN 59 PIC             Z(08)9.99- FROM DEB-60  BACKGROUND-COLOR Grey FOREGROUND-COLOR Magenta.
           03          COLUMN 72 VALUE                       " ³"        BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03  LINE 15 COLUMN 50 VALUE "³ 90d  : "                       BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
           03          COLUMN 59 PIC             Z(08)9.99- FROM DEB-90  BACKGROUND-COLOR Grey FOREGROUND-COLOR Magenta.
           03          COLUMN 72 VALUE                       " ³"        BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03  LINE 16 COLUMN 50 VALUE "³ 120d : "                       BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
           03          COLUMN 59 PIC             Z(08)9.99- FROM DEB-120 BACKGROUND-COLOR Grey FOREGROUND-COLOR Magenta.
           03          COLUMN 72 VALUE                       " ³"        BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03  LINE 17 COLUMN 50 VALUE "³ Int  : "                       BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
           03          COLUMN 59 PIC             Z(08)9.99- FROM DEB-INT BACKGROUND-COLOR Grey FOREGROUND-COLOR Magenta.
           03          COLUMN 72 VALUE                       " ³"        BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03  LINE 18 COLUMN 50 VALUE "À"                               BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
           03          COLUMN 51 VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"        BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.

       01  S12.
           03  LINE  9 COLUMN 11 VALUE "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»" BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03  LINE 10 COLUMN 11 VALUE "º    BALANCE FORWARD ALLOCATIONS    º" BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03  LINE 11 COLUMN 11 VALUE "ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶" BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03  LINE 12 COLUMN 11 VALUE "º Mtd  : "                             BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03          COLUMN 20 PIC             Z(08)9.99- FROM W40-ONE       BACKGROUND-COLOR Grey FOREGROUND-COLOR Cyan HIGHLIGHT.
           03          COLUMN 32 VALUE                      "               º" BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03  LINE 13 COLUMN 11 VALUE "º Cur  : "                             BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03          COLUMN 20 PIC             Z(08)9.99- FROM DEB-CUR       BACKGROUND-COLOR Grey FOREGROUND-COLOR Cyan HIGHLIGHT.
           03          COLUMN 32 VALUE                      "               º" BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03  LINE 14 COLUMN 11 VALUE "º 30d  : "                             BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03          COLUMN 20 PIC             Z(08)9.99- FROM DEB-30        BACKGROUND-COLOR Grey FOREGROUND-COLOR Cyan HIGHLIGHT.
           03          COLUMN 32 VALUE                      "               º" BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03  LINE 15 COLUMN 11 VALUE "º 60d  : "                             BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03          COLUMN 20 PIC             Z(08)9.99- FROM DEB-60        BACKGROUND-COLOR Grey FOREGROUND-COLOR Cyan HIGHLIGHT.
           03          COLUMN 32 VALUE                      "               º" BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03  LINE 16 COLUMN 11 VALUE "º 90d  : "                             BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03          COLUMN 20 PIC             Z(08)9.99- FROM DEB-90        BACKGROUND-COLOR Grey FOREGROUND-COLOR Cyan HIGHLIGHT.
           03          COLUMN 32 VALUE                      "               º" BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03  LINE 17 COLUMN 11 VALUE "º 120d : "                             BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03          COLUMN 20 PIC             Z(08)9.99- FROM DEB-120       BACKGROUND-COLOR Grey FOREGROUND-COLOR Cyan HIGHLIGHT.
           03          COLUMN 32 VALUE                      "               º" BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03  LINE 18 COLUMN 11 VALUE "º Int  : "                             BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03          COLUMN 20 PIC             Z(08)9.99- FROM DEB-INT       BACKGROUND-COLOR Grey FOREGROUND-COLOR Cyan HIGHLIGHT.
           03          COLUMN 32 VALUE                      "               º" BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03  LINE 19 COLUMN 11 VALUE "º "                                    BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03          COLUMN 13 VALUE   "Transaction balance:"                BACKGROUND-COLOR Grey FOREGROUND-COLOR Blue.
           03          COLUMN 33 VALUE                               "      º" BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
           03  LINE 20 COLUMN 11 VALUE "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼" BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.

       01  S18-S37.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               05  S18.
                   07  PIC 99/99/9999 USING W90-DATE AUTO.
               05  S18R.
                   07  PIC  X(08) USING W90-REF AUTO.
               05  S19.
                   07  PIC Z(08)9.99 USING W90-DB AUTO.
               05  S20.
                   07   PIC Z(08)9.99 USING W90-CASH AUTO.
               05  S21.
                   07  PIC Z(08)9.99 USING W90-CHQ AUTO.
               05  S21TRF.
                   07  PIC Z(08)9.99 USING W90-CARD AUTO.
               05  S22.
                   07  PIC X(24) USING W40-CONTACT AUTO.
               05  S23.
                   07  PIC X(14) USING W40-TEL AUTO.
               05  S24.
                   07  PIC 99/99/9999 USING W90-IDTE AUTO.
               05  S29.
                   07  PIC Z(08)9.99- USING W40-MTD AUTO.
               05  S30.
                   07  PIC Z(08)9.99- USING W40-CUR AUTO.
               05  S31.
                   07  PIC Z(08)9.99- USING W40-30 AUTO.
               05  S32.
                   07  PIC Z(08)9.99- USING W40-60 AUTO.
               05  S33.
                   07  PIC Z(08)9.99- USING W40-90 AUTO.
               05  S34.
                   07  PIC Z(08)9.99- USING W40-120 AUTO.
               05  S35.
                   07  PIC Z(06)9.99- USING W40-INT AUTO.
               05  S36.
                   07  PIC X(06) USING W80-ACNO AUTO.
               05  S37.
                   07  LINE 22 COLUMN 6 VALUE "Total Receipts (Batch): ".
                   07                   PIC                            Z(08)9.99- USING W25-TOTAL AUTO FOREGROUND-COLOR Cyan HIGHLIGHT.

       01  S-ALLOC.
           03  LINE 21 COLUMN 14 VALUE "Un-allocated balance : ".
           03          COLUMN 37 PIC Z(08)9.99 FROM W75-BALANCE   BACKGROUND-COLOR Magenta FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 51 VALUE "Allocated : ".
           03          COLUMN 63 PIC Z(08)9.99 FROM W75-ALLOCATED BACKGROUND-COLOR Magenta FOREGROUND-COLOR Brown HIGHLIGHT.

       01  S-DTRN.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
               05  COLUMN  6 PIC X(08)      FROM  DBT-REF.
               05  COLUMN 15 PIC 9999/99/99 FROM  DBT-DATE.
               05  COLUMN 26 PIC X(12)      FROM  W00-E-DESC.
               05  COLUMN 39 PIC Z(08)9.99  FROM  DBT-VALUE.
               05  COLUMN 52 PIC Z(08)9.99  FROM  DBT-OUTSTD.
               05  COLUMN 65 PIC Z(08)9.99  USING DBT-ALLOC AUTO.

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
           IF LS0-DBLEV < 2
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA05.
             PERFORM ZA000-INIT.
             MOVE ZERO               TO W25-TOTAL.
             PERFORM DA000.
             CLOSE CHEQUE RECEIPTS RECOVER.
           IF WS-CLS-AUD = "Y"
               CLOSE AUDIT.
       AA05.
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
       AA50-LOOKUP       SECTION.
       AA50.
             PERFORM SAVE-SCREEN.
      *      MOVE 4                  TO W44-FUNCTION.
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
      *    ³   MOVE 8         TO CRT-START.                            ³
      *    ³   MOVE 28        TO CRT-END.                              ³
      *    ³   PERFORM ERASE-SCREEN.                                   ³
      *    ³                                                           ³
      *    ³ To clear the entire screen and Display a new screen with  ³
      *    ³ headings (program/date/time/company):                     ³
      *    ³                                                           ³
      *    ³    CRT-PROGRAM (calling program)                          ³
      *    ³    CRT-HEADING (Screen heading)                           ³
      *    ³    CRT-COMPANY (Company name)                             ³
      *    ³    CRT-TERMINAL (consists of two fields:                  ³
      *    ³                  CRT-WRKHD and CRT-WRKST. See CRTHEAD)    ³
      *    ³    These fields should be populated at the start of the   ³
      *    ³    program.                                               ³
      *    ³    PERFORM HEADING-AND-CLEAR-SCREEN.                      ³
      *    ³                                                           ³
      *    ³ To change the screen heading:                             ³
      *    ³    MOVE "The new heading" TO CRT-HEADING.                 ³
      *    ³    PERFORM CHANGE-HEADING.                                ³
      *    ³                                                           ³
      *    ³ To change the time on the screen:                         ³
      *    ³    PERFORM CHANGE-TIME                                    ³
      *    ³                                                           ³
      *    ³ To clear line 50:                                         ³
      *    ³    PERFORM CLEAR-L50                                      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "CLEAR.CRT".

      *    
      *          G E T   D E T A I L S   F O R   A   D E B T O R   
      *                  T R A N S A C T I O N   C O D E
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ PERFORM THIS ROUTINE WITH THE TRANSACTION CODE IN W90-CODE³
      *    ³                                                           ³
      *    ³ ON EXITING THE TRANSACTION CODE DETAILS ARE IN W00-       ³
      *    ³ WHILE W90-TRNKEY CONTAINS THE KEY TO THE PARAMETER FILE   ³
      *    ³ WHERE THIS TRANSACTION CODE IS FOUND AND W90-TS IS THE    ³
      *    ³ TRANSACTION CODE SUBSCRIPT IN THE RECORD.                 ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "DEBTOR.TRN".

      *    *************************************************************
      *    ****        D A T E  C H E C K   R O U T I N E
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Perform this routine with the date in W10-DATE (PIC      ³
      *    ³  99/99/9999) with the format as DD/MM/CCYY         ³
      *    ³              ³
      *    ³  On exiting - WS-ERROR will be zero (0) if the date is    ³
      *    ³  correct or one (1) if the date is incorrect.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "DATE.CHK".

       COPY "LEDTRF.UPD".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                                                           ³
      *    ³           R E A D   F I L E S   R O U T I N E S           ³
      *    ³                                                           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AC000    SECTION.

       COPY "AUDIT.RD".

       COPY "CHEQUE.RD".

       COPY "CONTROL.RD".

       COPY "DBTRAN.RD1".

       COPY "DEBTOR.RD".

       COPY "DEBTRN.RD".

       COPY "DEPART.RD".

       COPY "LEDTRF.RD".

       COPY "PARAM.RD".

       COPY "SHARED.RD".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   & W R I T E   F I L E S R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AD000   SECTION.

       COPY "AUDIT.WR".

       COPY "CHEQUE.WR".

       COPY "CONTROL.WR".

       COPY "DBTRAN.WR".

       COPY "DEBTOR.WR".

       COPY "DEBTRN.WR".

       COPY "DEPART.WR".

       COPY "LEDTRF.WR".

       COPY "PARAM.WR".

       COPY "RECEIPTS.WR".

       COPY "TXTRAN.WR".

       COPY "CASHDRAW.OPN".

       COPY "DEBTOR.LUP".

       COPY "APAC.HLP".

      *    *************************************************************
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *    *************************************************************
       AY000             SECTION.
      *    *************************************************************
      *    ****             P A R A M E T E R   F I L E             ****
      *    *************************************************************
       AY01.
             MOVE 01                 TO REC-FILE.
             MOVE WS-PARKEY          TO REC-KEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
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

       AY16.
             MOVE 16                 TO REC-FILE.
             MOVE WS-AUDKEY          TO REC-KEY.
             MOVE AUD-REC1           TO REC-AUDITF.
             GO TO AY50.

       AY19.
             MOVE 19                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE.
             MOVE TAX-RECORD1        TO REC-TXTRAN.
             GO TO AY50.

       AY36.
             MOVE 36                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE.
             MOVE CHQ-RECORD1        TO REC-CHEQUE.
             GO TO AY50.

       AY38.
             MOVE 38                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³              End transaction in recovery log              ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY49.
             MOVE 99                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE SPACES             TO REC-DETAIL.
             PERFORM AY50 THRU AY59.
             ADD 1                   TO WS-TRANS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If recovery records exceed 95 - Clear (Close and Open     ³
      *    ³ new). This allows for quicker recoveries when required.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
               DISPLAY "Write error Recovery file - Status " AT 4512 WITH FOREGROUND-COLOR Brown HIGHLIGHT
                        WS-STATUS                                    WITH FOREGROUND-COLOR Grey  HIGHLIGHT
               STOP RUN.
             CLOSE RECOVER.
             OPEN I-O RECOVER.

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
             DISPLAY SPACE AT 5051     WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.
             GO TO AY60.

       AY61.
             MOVE "DB"               TO PAR-PROG(WS-USUB).
             MOVE LS-USER            TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *    *************************************************************
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *    *************************************************************
             MOVE 1                  TO WS-NETKEY.
             PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             PERFORM AY39 THRU AY59.
             GO TO AY999.
      *    *************************************************************
      *    ****   A R E   A N Y   U P D A T E S   I N   P R O G R E S S
      *    *************************************************************
       AY62.
           IF NOT(PAR-PROG(WS-SUB) = SPACES OR PAR-USR(WS-SUB) = SPACES)
      *    *************************************************************
      *    ****   A R E   D E B T O R   O R   S T O C K   F I L E S
      *                     B E I N G   U P D A T E D
      *    *************************************************************
               IF NOT(PAR-PROG(WS-SUB) = SPACES)
                   IF PAR-PROG(WS-SUB) = "DB" OR "DS"
      *    *************************************************************
      *    ****   Y E S   -   S E T   W A I T   P E R I O D
      *    *************************************************************
                       GO TO AY62-WAIT
                   ELSE
                       ADD 1         TO WS-SUB
                       GO TO AY62
                   END-IF
               ELSE
      *    *************************************************************
      *    ****   I S   T H I S   P R O G R A M   I N   T H E   Q
      *    *************************************************************
               IF PAR-USR(WS-SUB) = LS-USER
      *    *************************************************************
      *    ** I S   I T   N E X T   I N   L I N E   T O   P R O C E S S
      *    *************************************************************
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
      *    *************************************************************
      *    ****   S E T   W A I T   P E R I O D
      *    *************************************************************
       AY62-WAIT.
             MOVE 300                TO WS-WAIT.
          IF NOT(PAR-USR(WS-SUB) = LS-USER)
              IF WS-SUB < 24
                  ADD 1              TO WS-SUB
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
      *    *************************************************************
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 2.
      *    *************************************************************
             PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
             PERFORM AY49 THRU AY59.
             MOVE 1                  TO WS-USUB.

       AY72.
           IF NOT(PAR-USR(WS-USUB) = LS-USER)
               IF WS-USUB < 24
                   ADD 1             TO WS-USUB
                   GO TO AY72
               ELSE
                   GO TO AY85.

       AY75.
             MOVE SPACES             TO PAR-PROG(WS-USUB) PAR-USR(WS-USUB).
           IF WS-USUB < 24
               ADD 1 WS-USUB  GIVING WS-SUB
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

      *    *************************************************************
      *    ****    Read debtor record - using account number.
      *    *************************************************************
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
      *        MOVE DEB-TYPE     TO W80-TYPE
      *
      *    ****    Move the Debtor Record to working storage area
      *
               MOVE DEB-RECORD1      TO W40-DEBTREC.

       CA160-EXIT.
             EXIT.

      *    *************************************************************
      *    ****    G E N E R A T E   A   D E B T O R
      *            T R A N S A C T I O N
      *    *************************************************************
       GENERATE-DEBTOR-TRAN SECTION 2.
      
       DEBTOR-TRAN.
      
       LINK-DEBTOR-TRANS.
             INITIALIZE TRA-RECORD1.
             MOVE W20-DTE        TO TRA-DATE.
             MOVE W90-CODE       TO TRA-CODE.
             MOVE W90-REF        TO TRA-REF TRA-REG.
             MOVE DEB-ACNO       TO TRA-AC.
             SUBTRACT W90-CR FROM W90-DB GIVING TRA-VALUE.
             PERFORM WRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY04 THRU AY59.
             ADD TRA-VALUE       TO W00-T-VAL W00-T-MTDV W00-T-YTDV.
             ADD 1               TO W00-T-DAY W00-T-MTD  W00-T-YTD.
           IF WX-INT = "Y"
               PERFORM DEBTOR-INTEGRATE.
             PERFORM UPDATE-TRAN-RECORD.
             ADD W90-DB          TO DEB-DEBT.
             ADD W90-DB          TO DEB-BAL.
            GO TO DEBTOR-TRAN-EXIT.

       DEBTOR-INTEGRATE.
             MOVE W20-DTE        TO WX-DATE.
             MOVE W90-CODE       TO WX-LEDG.
             MOVE W00-E-DESC     TO WX-DESC.
             MOVE "- DEBTORS"    TO WX-BOOK.
             MOVE TRA-VALUE      TO WX-VALUE.
             MOVE WX-DEBGL       TO WX-AC.
             MOVE 01             TO WX-TYPE.
             PERFORM GL-TRF-UPDATE.
             MOVE 14             TO WX-TYPE.
             COMPUTE WX-VALUE = TRA-VALUE * -1.
             MOVE W00-T-GL       TO WX-AC.
             PERFORM GL-TRF-UPDATE.

       DEBTOR-TRAN-EXIT.
             EXIT.

      *    *************************************************************
      *    ****    PROCESS A DEBTOR RECEIPT REVERSAL
      *    *************************************************************
       DA000         SECTION 50.
       DA00.
             MOVE 1                  TO WS-S9.

       DA02.
             MOVE ZERO               TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
           IF WS-S9 < 12
               ADD 1                 TO WS-S9
               GO TO DA02.
             MOVE ZERO               TO W90-90  W90-120 W90-CUR W90-BCR W90-60  W90-BDT W90-30  W90-INT W90-EX  W90-VAL W90-TX  W90-DISCOUNT.

       DA05.
             PERFORM ERASE-SCREEN.
             DISPLAY S08.
           IF NOT(W25-TOTAL = ZERO)
               DISPLAY S37.
             MOVE SPACES             TO W80-ACNO.
             MOVE 02                 TO W90-CODE.
             MOVE W85-CSHDTE         TO W90-DATE.
             MOVE W12-T-YMD          TO W20-DATE.

       DA10.
             MOVE ZERO               TO W90-DB W90-CR.

       DA15.
             DISPLAY "F1"                 AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     "=Help,"                     WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                     "F2"                         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     " to do Debtor Lookup "      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                     "F9"                         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     " Debtor Lookup (Any Word)," WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                     "Esc"                        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     " to exit"                   WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             ACCEPT S36 AT 0819.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    CLOSE PRNREP
                                 GO TO DA999

                 WHEN F1-KEY     PERFORM HELP-ROUTINE

                 WHEN F2-KEY     MOVE SPACE      TO W40-LUP
                                 PERFORM AA50

                 WHEN F9-KEY     MOVE "A"        TO W40-LUP
                                 PERFORM AA50

                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO DA15
               END-EVALUATE
               DISPLAY S36 AT 0819
               IF W80-ACNO = SPACES
                   GO TO DA15.
             PERFORM CLEAR-L50.
           IF W80-ACNO = SPACES
               GO TO DA75.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W80-ACNO BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR = 1
              MOVE WS-ERR2  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO DA15.
           IF WS-ERROR = 2
               MOVE WS-ERR1  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO DA15.
      *    *************************************************************
      *    ****         T E S T   F O R   I N D I V I D U A L       ****
      *    *************************************************************
           IF W40-TYPE = 1
               MOVE "D"              TO W40-INSTR
               CALL "DTP\DTPNAME" USING W40-COMPANY LS-USER-ID
           ELSE
               MOVE W40-NAME         TO W40-DSP-NAME.
             DISPLAY W40-DSP-NAME AT 0839 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             MOVE DEB-BAL            TO W100-BAL.
             DISPLAY W100-BAL AT 0939 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO DA10.
             MOVE DEB-ACNO           TO W15-ACCNO.
             DISPLAY "Esc"      AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     " to exit"         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
      *    *************************************************************
      *                       R E C E I P T   D A T E
      *    *************************************************************
       DA16.
             ACCEPT S18 AT 0919.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  GO TO DA75
                 
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO DA16
               END-EVALUATE.
             MOVE W90-DATE           TO W10-EDTE W15-DATE.
             PERFORM CHECK-DATE.
           IF WS-ERROR = 1
               MOVE "Invalid date"   TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO DA16.

      *    *************************************************************
      *     R E C E I P T   N U M B E R
      *    *************************************************************
       DA17.
             ACCEPT S18R AT 1019.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY   GO TO DA75
                 WHEN OTHER     PERFORM AA900-ALARM
                                GO TO DA17
               END-EVALUATE.
           IF W90-REF = SPACES
               MOVE "Invalid Reference" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO DA17.
      *    *************************************************************
      *                   R E V E R S A L   A M O U N T
      *    *************************************************************
       DA20.
             ACCEPT S19 AT 1119.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  GO TO DA75

                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO DA20
               END-EVALUATE.
           IF W90-DB = ZERO
               MOVE "Amount may not be zero"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO DA20.

       DA22.
             MOVE "'C'ontinue or 'R'ekey  [ ]" TO WS-OPT-MES.
             MOVE "C"                TO WS-OPTION.
             MOVE "9"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "R"
               GO TO DA20.

       DA25.
             MOVE ZERO               TO W90-CASH W90-CHQ W90-CARD W90-PMT W90-CHANGE.
             MOVE W90-CR             TO W90-DUE.
             DISPLAY "Amount due " AT 1206.

       DA30.
             COMPUTE W90-CRD = W90-DUE - W90-PMT.
             MOVE W90-CRD        TO W100-BAL.
             DISPLAY W100-BAL AT 1219 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             MOVE W90-PMT            TO W100-BAL.
             DISPLAY W100-BAL AT 1719 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
      *    *************************************************************
      *                     C A S H   T E N D E R E D
      *    *************************************************************
       DA35.
             ACCEPT S20 AT 1319.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  GO TO DA75
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO DA35
               END-EVALUATE.
             MOVE W90-CASH           TO W90-PMT.
           IF W90-PMT NOT < W90-DUE
               GO TO DA45.
             PERFORM DA30.
      *    *************************************************************
      *                    C H E Q U E   T E N D E R E D
      *    *************************************************************
       DA40.
             ACCEPT S21 AT 1419.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  GO TO DA75

                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO DA40
               END-EVALUATE.
             ADD W90-CHQ             TO W90-PMT.
           IF W90-PMT NOT < W90-DUE
               GO TO DA45.
             PERFORM DA30.
      *    *************************************************************
      *                     B A N K   T R A N S F E R
      *    *************************************************************
       DA42.
             ACCEPT S21TRF AT 1519.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  GO TO DA75

                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO DA40
               END-EVALUATE.
             ADD W90-CARD            TO W90-PMT.
           IF W90-PMT NOT < W90-DUE
               GO TO DA45.
             PERFORM DA30.
             GO TO DA55.

       DA45.
             PERFORM DA30.
             MOVE "'C'ontinue or 'R'ekey reversal  [C]" TO WS-OPT-MES.
             MOVE "C"                TO WS-OPTION.
             MOVE "9"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "R"
               GO TO DA25.

       DA54.
             PERFORM AY60 THRU AY999.
             MOVE ZERO               TO WS-ACTION.
             PERFORM AY02 THRU AY59.
             GO TO DA58.

       DA55.
             MOVE "Short paid - REKEY 'P'ayment or 'R'eversal amount" TO WS-ERR-MES.
             MOVE "P"                TO WS-OPTION.
             PERFORM ERROR-MESSAGE.
          IF NOT(WS-OPTION = "P" OR "R")
              GO TO DA55.
           IF WS-OPTION = "P"
               MOVE ZERO             TO W90-PMT
               GO TO DA25.
             GO TO DA20.

       DA58.
             MOVE "Print a reversal Y/N?  [Y]" TO WS-OPT-MES.
             MOVE "Y"                TO WS-OPTION.
             MOVE "1"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
             MOVE WS-OPTION          TO WS-CSLIP.
             MOVE 02                 TO W90-CODE.
             PERFORM GET-DEBTOR-TRAN THRU GET-DEBTOR-TRAN-EXIT.
             PERFORM AY01 THRU AY59.
             PERFORM GENERATE-DEBTOR-TRAN.
      *    *************************************************************
      *    ****         S A V E   P A Y M E N T   V A L U E
      *    *************************************************************
             MOVE W90-DB             TO W90-PMT.

       DA65B.
             ADD 121 DEB-CAT         GIVING W90-RESULT.
             DIVIDE W90-RESULT BY 2  GIVING W90-CATKEY REMAINDER W90-CS.
             ADD 1                   TO W90-CS.
             MOVE W90-CATKEY         TO WS-PARKEY.
             ADD W90-PMT             TO W25-TOTAL.
             PERFORM AY01 THRU AY59.
             ADD W90-DB              TO PAR-CDRMTD(W90-CS) PAR-CDRYTD(W90-CS)
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
           IF WS-CSLIP = "N"
               GO TO DA73.
             MOVE W15-COND1          TO CSH-DATA.
             WRITE CSH-L.
             MOVE "RECEIPT REVERSAL" TO W15-DOCTYP.
             MOVE W15-L1A            TO CSH-DATA.
             WRITE CSH-L.
             MOVE W15-L1             TO CSH-DATA.
             WRITE CSH-L.
             MOVE W15-L2             TO CSH-DATA.
             WRITE CSH-L.
             MOVE W15-L3             TO CSH-DATA.
             WRITE CSH-L.
             MOVE W15-L4             TO CSH-DATA.
             WRITE CSH-L.
             MOVE W90-REF            TO W15-RECNO.
             MOVE W15-L6             TO CSH-DATA.
             WRITE CSH-L.
             MOVE W15-L8             TO CSH-DATA.
             WRITE CSH-L.
             MOVE W40-DSP-NAME       TO CSH-DATA.
             WRITE CSH-L.
             MOVE SPACE              TO CSH-DATA.
             WRITE CSH-L.
             MOVE "Payment reversed" TO W15-REMK.
             MOVE W90-PMT            TO W15-VAL.
             MOVE W15-L13            TO CSH-DATA.
             WRITE CSH-L.
             MOVE "Outstanding Balance" TO W15-REMK.
             MOVE DEB-BAL            TO W15-VAL.
             MOVE W15-L13            TO CSH-DATA.
             WRITE CSH-L.
             MOVE SPACE              TO CSH-DATA.
             WRITE CSH-L.
           IF W90-CASH > ZERO
               MOVE "CASH / KONTANT   :" TO W15-REMK
               MOVE W90-CASH         TO W15-VAL
               MOVE W15-L13          TO CSH-DATA
               WRITE CSH-L.
           IF W90-CHQ > ZERO
               MOVE "CHEQUE / TJEK    :" TO W15-REMK
               MOVE W90-CHQ          TO W15-VAL
               MOVE W15-L13          TO CSH-DATA
               WRITE CSH-L.
           IF W90-CARD > ZERO
               MOVE "BNK,TRF/BNK.OORP :" TO W15-REMK
               MOVE W90-CARD         TO W15-VAL
               MOVE W15-L13          TO CSH-DATA
               WRITE CSH-L.
             MOVE SPACES             TO CSH-DATA.
             WRITE CSH-L.
             MOVE W15-L9             TO CSH-DATA.
             WRITE CSH-L.
             MOVE SPACES             TO W15-ADD.
             MOVE W15-L2             TO CSH-DATA.
             WRITE CSH-L.
             MOVE W15-NORM1          TO CSH-DATA.
             WRITE CSH-L.
             MOVE SPACES             TO CSH-DATA.
             MOVE 1                  TO WS-S9.
       DA72.
             WRITE CSH-L.
           IF WS-S9 < W85-ADVANCE
               ADD 1                 TO WS-S9
              GO TO DA72.

       DA73.
             PERFORM OPEN-CASH-DRAWER THRU OPEN-CASH-DRAWER-EXIT.
             PERFORM REWRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.
             ADD 150 WS-DRAW         GIVING WS-PARKEY.
             PERFORM AY01 THRU AY59.
             SUBTRACT W90-CASH       FROM PAR-CASH.
             SUBTRACT W90-CHQ        FROM PAR-CHEQUES.
             SUBTRACT W90-CARD       FROM PAR-BANKTRF.
             COMPUTE PAR-RECEIPTS = PAR-RECEIPTS - W90-DB.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             MOVE PAR-RECORD151      TO W85-RECORD151.
             ADD W90-DB              TO DEB-OUT.
             ADD W90-DB              TO DEB-DT.
      *    *************************************************************
      *    ****    L O G   D E T A I L S   T O  A U D I T   F I L E
      *    *************************************************************
             MOVE DEB-AUDIT          TO WS-AUDKEY.
             MOVE LOW-VALUES         TO AUD-REC1.
             PERFORM AY16 THRU AY59.
             MOVE 11                 TO AUD-TYPE
             MOVE DEB-ACNO           TO AUD-AC.
             MOVE WS-DRAW            TO AUD-SUB.
             MOVE W90-REF            TO AUD-REF.
             COMPUTE AUD-VAL = W90-PMT * -1.
             MOVE W90-CASH           TO AUD-R-CSH.
             MOVE W90-CHQ            TO AUD-R-CHQ.
             MOVE W90-CARD           TO AUD-R-TRF.
      *    *************************************************************
      *    ****   L O G   D E T A I L S   T O R E C E I P T F I L E
      *    *************************************************************
             MOVE W22-DTE1           TO RCT-DATE.
             MOVE DEB-ACNO           TO RCT-AC.
             MOVE W90-REF            TO RCT-REF.
             MOVE ZERO               TO RCT-REF-EXT.
             MOVE W40-NAME           TO RCT-NAME.
           IF NOT(W90-CASH = ZERO)
               MOVE "C"              TO RCT-TYPE
           ELSE
           IF NOT(W90-CHQ = ZERO)
               MOVE "Q"              TO RCT-TYPE
           ELSE
               MOVE "T"              TO RCT-TYPE.
             MOVE W90-PMT            TO RCT-VAL.
             MOVE W90-CASH           TO RCT-CSH.
             MOVE W90-CHQ            TO RCT-CHQ.
             MOVE W90-CARD           TO RCT-TRF.
             PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
             ADD 1                   TO DEB-AUDIT.
             MOVE 1                  TO WS-AUDKEY.
             PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
             PERFORM AY16 THRU AY59.
             MOVE 1                  TO AUD-REP11.
             PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
             PERFORM WRITE-RECEIPTS THRU WRITE-RECEIPTS-EXIT.
             PERFORM AY70 THRU AY999.
             CLOSE AUDIT DEBTOR NETWORK PARAM RECEIPTS.
             OPEN I-O AUDIT DEBTOR NETWORK PARAM RECEIPTS.

       DA75.
             MOVE "'ENTER' to continue - 'E' to exit" TO WS-OPT-MES.
             MOVE SPACE              TO WS-OPTION.
             MOVE "Z"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "E"
               CLOSE PRNREP
               GO TO DA999.
             GO TO DA00.

       DA999.
             EXIT.
      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT        SECTION 88.
       ZA000-OPEN.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W12-TODAY.
             MOVE LS-TODAY-YYMMDD    TO W12-T-YMD.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-DRAW            TO WS-DRAW.
             MOVE LS-COMPANY         TO CRT-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
           IF WS-DRAW = ZERO
               MOVE 40           TO WS-DRAW.
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

       ZA00A-CONTINUE.
             MOVE 3                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-BANK-IND       TO WS-BNK-IND.
           IF LS-USER NUMERIC
               IF LS-USE > ZERO AND < 110
                   MOVE LS-USE       TO WS-USUB
                   GO TO ZA01A.
             MOVE 110                TO WS-USUB.

       ZA01A.
             ADD 2549 WS-USUB        GIVING W25-RESULT.
             MOVE W25-KEY            TO WS-PARKEY.
             ADD 1 W25-SUB           GIVING WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             EVALUATE TRM-STDP(WS-S6)
               WHEN 1  MOVE "LPT1"   TO W02-PRINTER

               WHEN 2  MOVE "LPT2"   TO W02-PRINTER

               WHEN 3  MOVE "LPT3"   TO W02-PRINTER

               WHEN 4  MOVE "COM1"   TO W02-PRINTER

               WHEN 5  MOVE "COM2"   TO W02-PRINTER

               WHEN 9  MOVE LS-USER  TO W02-USER
             END-EVALUATE.
             ADD 501 TRM-PRN3(WS-S6) GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2  GIVING WS-PARKEY REMAINDER WS-S6.
             ADD 1                   TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE TRM-CONP(WS-S6)    TO WS-COND.
             MOVE TRM-NORP(WS-S6)    TO WS-NORM.
             MOVE TRM-EXPP(WS-S6)    TO WS-EXPP.
             MOVE TRM-ECAN(WS-S6)    TO WS-ECAN.
             MOVE TRM-8LPI(WS-S6)    TO WS-8LPI.
             MOVE TRM-6LPI(WS-S6)    TO WS-6LPI.
             MOVE TRM-10CPI(WS-S6)   TO WS-10CPI.
             MOVE TRM-12CPI(WS-S6)   TO WS-12CPI.
             MOVE TRM-17CPI(WS-S6)   TO WS-17CPI.
             MOVE TRM-LENGTH(WS-S6)  TO WS-PRN-LENGTH.
             MOVE TRM-PAGE(WS-S6)    TO WS-PGE-LENGTH.

       ZA02.
             OPEN I-O AUDIT.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS NOT = "00"
               IF WS-STATUS = "41"
                   MOVE "N"          TO WS-CLS-AUD
               ELSE
                   MOVE 1            TO WS-F-ERROR
                   PERFORM OPEN-ERROR.
             OPEN I-O CHEQUE.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 36           TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
               MOVE 36               TO WS-F-ERROR
               PERFORM OPEN-ERROR.

       ZA03.
             OPEN I-O RECEIPTS.
           IF WS-STATUS = "05"
               CLOSE RECEIPTS
               OPEN OUTPUT RECEIPTS
               CLOSE RECEIPTS
               GO TO ZA03.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS NOT = "00"
               OPEN OUTPUT RECEIPTS
               CLOSE RECEIPTS
               GO TO ZA03.
             OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
             MOVE 1                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF PAR-CSHEAD = SPACES
               DISPLAY "Setup cash slip headings - press ENTER" AT 1212 WITH FOREGROUND-COLOR Brown HIGHLIGHT
               ACCEPT WS-OPTION AT 1251 WITH AUTO
               GO TO ZA205.
      *      MOVE PAR-DMY            TO W12-TODAY.
      *      MOVE PAR-YMD            TO W12-T-YMD.
             MOVE PAR-CSHEAD         TO W15-COMP.
             MOVE PAR-CSHADD         TO W15-ADD.
             MOVE PAR-TELEPHONE      TO W15-TEL.
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
             ADD 150 WS-DRAW         GIVING WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE W12-TODAY          TO PAR-CSHDTE
             MOVE PAR-RECORD151      TO W85-RECORD151.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             MOVE 32                 TO WS-S1.

       ZA05.
           IF W40-NCHAR (WS-S1) = SPACE
               SUBTRACT 1            FROM WS-S1
               GO TO ZA05.
           IF WS-S1 > 16
               MOVE W85-NUL          TO W15-EXPP1 W15-ECAN1
           ELSE
               MOVE W85-SO           TO W15-EXPP1
               MOVE W85-CSO          TO W15-ECAN1.
             MOVE W85-ESC            TO W15-ESC1 W15-ESC2 W15-ESC3 W15-ESC5.
             MOVE W85-SO             TO W15-EXPP2 W15-EXPP4.
             MOVE W85-CSO            TO W15-ECAN2 W15-ECAN4.
             MOVE W85-DBL            TO W15-DBLE W15-DBLE2 W15-DBLE3.
             MOVE W85-CDBL           TO W15-DCAN.
             MOVE W85-SI             TO W15-COND1.
             MOVE W85-CSI            TO W15-NORM1.
             EVALUATE W85-CPRN
               WHEN 1   MOVE "LPT1"  TO W02-PRINTER
               WHEN 2   MOVE "LPT2"  TO W02-PRINTER
               WHEN 3   MOVE "LPT3"  TO W02-PRINTER
               WHEN 4   MOVE "COM1"  TO W02-PRINTER
               WHEN 5   MOVE "COM2"  TO W02-PRINTER
               WHEN 9   MOVE LS-USER TO W02-USER
             END-EVALUATE.
             OPEN OUTPUT PRNREP.
             CALL X"91" USING X91-RES X91-FUN PRNREP.
      *    *************************************************************
      *    ****      O L D   S T Y L E   C A S H   D R A W E R      ****
      *    *************************************************************
           IF W85-DRAWER  = "F"
               EVALUATE W85-PORT
                 WHEN 2  MOVE "COM2" TO W02-CSHDRW
                 WHEN 3  MOVE "COM3" TO W02-CSHDRW
                 WHEN 4  MOVE "COM4" TO W02-CSHDRW
               END-EVALUATE
               OPEN OUTPUT CSHDRW
               MOVE ALL "A"          TO CSH-OPEN.
      *    *************************************************************
      *    ****   G E T   D E B T O R   M O N T H - E N D   D A Y   ****
      *    *************************************************************
             MOVE 1                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
           IF DEB-MTH-END < 21 OR > 31 AND < 99
               MOVE 99               TO DEB-MTH-END.
             MOVE DEB-MTH-END        TO WS-MTHEND.
             GO TO ZA999-EXIT.

       COPY "ZA49.PRO".

       ZA50.
           IF WS-F-ERROR = 36
               MOVE "PD CHEQUE file" TO WS-FILE.
             DISPLAY "Rebuild "                  AT 0812 WS-FILE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Press any key to continue" AT 1012.
             ACCEPT WS-OPTION                    AT 1038 WITH FOREGROUND-COLOR Grey HIGHLIGHT.

       ZA51.
             EXIT PROGRAM.

       ZA200.

       COPY "LOCKED.PRO".

       ZA205.
             EXIT PROGRAM.

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
               WHEN 1          MOVE W02-AUDITF    TO WS-FILE
                               MOVE WS-AUDKEY     TO WS-KEY
               WHEN 2          MOVE W02-NETWORK   TO WS-FILE
                               MOVE WS-NETKEY     TO WS-KEY
               WHEN 5          MOVE W02-DBTRAN    TO WS-FILE
                               MOVE TRA-KEY       TO WS-KEYX
               WHEN 6          MOVE W02-DEBTOR    TO WS-FILE
                               MOVE DEB-ACNO      TO WS-KEYX
               WHEN 7          MOVE W02-DEPART    TO WS-FILE
                               MOVE DPT-CODE      TO WS-KEYX
               WHEN 15         MOVE WS-PARID      TO WS-FILE
                               MOVE WS-PARKEY     TO WS-KEY
               WHEN 18         MOVE W02-RECOVER   TO WS-FILE
                               MOVE WS-RECKEY     TO WS-KEY
               WHEN 31         MOVE W02-DEBTRN    TO WS-FILE
                               MOVE WS-DBTKEY     TO WS-KEY
               WHEN 32         MOVE W02-TXTRAN    TO WS-FILE
                               MOVE TAX-KEY       TO WS-KEYX
               WHEN 36         MOVE W02-CHEQUE    TO WS-FILE
                               MOVE CHQ-TR-NUMBER TO WS-KEYX
               WHEN 37         MOVE W02-SHARED    TO WS-FILE
                               MOVE WS-SHARED     TO WS-KEY
               WHEN 40         MOVE W02-LEDTRF    TO WS-FILE
                               MOVE XFR-KEY       TO WS-KEYX
               WHEN 90         MOVE W02-RECEIPTS  TO WS-FILE
                               MOVE RCT-REF       TO WS-KEYX
               WHEN OTHER      MOVE "ERROR"       TO WS-FILE
                               MOVE "DTP012R"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
