      $SET LINKCOUNT"512" GNT"DTP014.GNT"
      ******************************************************************
      *                                                                *
      *    *******   ********  *******     ****       **        **     *
      *    **    **     **     **    **   **  **     ***       ***     *
      *    **    **     **     **    **  **    **     **      * **     *
      *    **    **     **     *******   **    **     **     *  **     *
      *    **    **     **     **        **    **     **    *******    *
      *    **    **     **     **         **  **      **        **     *
      *    *******      **     **          ****     ******     ****    *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *         A C C O U N T S   R E C E I V A B L E /                *
      *                                                                *
      *   D E B T O R   B A T C H   T R A N S A C T I O N S   C O D E  *
      *                                                                *
      *       Version 9.04.02 - August 2017                            *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  May 2006 - New field included in Accounts Receivable          *
      *             for recording number of outstanding Jobs           *
      *            linked to the account. Additional filler            *
      *            added for future expansion.                         *
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
       PROGRAM-ID.     DTP014.
       AUTHOR.         J.W.LEMMON.
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

       COPY "DEBTRN.SL".

       COPY "DEPART.SL".

       COPY "DRBTCH.SL".

       COPY "LEDTRF.SL".

       COPY "PARAM.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

       COPY "TXTRAN.SL".

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

       COPY "DEBTOR.FDE".

       COPY "DEBTRN.FD".

       COPY "DEPART.FDE".

       COPY "DRBTCH.FDE".

       COPY "LEDTRF.FDE".

       COPY "PARAM.FDE".

       COPY "RECOVER.FD".

       COPY "SHARED.FDE".

       COPY "TXTRAN.FDE".

      *
       FD  PRNREP    LABEL RECORD OMITTED
             LINAGE IS WS-PGE-LENGTH.
       01  R-L1.
           03  R-DET1          PIC  X(132).
       01  R-L2.
           03  R-DET2          PIC  X(80).
       01  R-L3.
           03  R-DATE          PIC  X(06).
          03  R-DTE        PIC  Z9/99/9999.
          03  FILLER        PIC  X(05).
           03  R-H5            PIC  X(46).
           03  R-H6            PIC  X(07).
    03  R-USER.
              05  R-BATCH     PIC  Z(05)9.
          03  FILLER        PIC  X(06).
           03  R-PAGE          PIC  X(06).
           03  R-P-NO          PIC  Z(03)9.
       01  R-L3A.
           03  FILLER          PIC  X(04).
           03  R-CO            PIC  X(40).
           03  FILLER          PIC  X(04).
       01  R-L4.
           03  FILLER          PIC  X(11).
           03  R-H1            PIC  X(04).
           03  FILLER          PIC  X(06).
           03  R-H2            PIC  X(15).
           03  FILLER          PIC  X(06).
           03  R-H3            PIC  X(15).
           03  FILLER          PIC  X(06).
           03  R-H4            PIC  X(06).
           03  FILLER          PIC  X(11).
       01  R-L5.
           03  FILLER          PIC  X(12).
           03  R-TRAN          PIC  9(02).
           03  FILLER          PIC  X(09).
           03  R-ENG           PIC  X(12).
           03  FILLER          PIC  X(09).
           03  R-AFR           PIC  X(12).
           03  FILLER          PIC  X(10).
           03  R-ACT           PIC  X(01).
           03  FILLER          PIC  X(13).
       01  R-L7.
           03  R-H7            PIC  X(07).
           03  R-H7A           PIC  X(27).
           03  R-H8            PIC  X(07).
           03  R-H10           PIC  X(09).
           03  R-H11           PIC  X(15).
           03  R-H12           PIC  X(14).
           03  R-H13           PIC  X(11).
           03  R-H14           PIC  X(06).
       01  R-L8.
           03  R-TAC           PIC  X(06).
           03  FILLER          PIC  X(01).
           03  R-DNME          PIC  X(24).
           03  FILLER          PIC  X(01).
           03  R-TDTE          PIC  X(08).
           03  FILLER          PIC  X(01).
           03  R-TDESC         PIC  X(08).
           03  FILLER          PIC  X(01).
           03  R-TREF          PIC  X(08).
           03  FILLER          PIC  X(01).
           03  R-VAL           PIC  Z(07)9.99-.
           03  FILLER          PIC  X(01).
           03  R-TAX           PIC  Z(06)9.99-.
           03  FILLER          PIC  X(01).
           03  R-TOT           PIC  Z(07)9.99-.
       01  R-L9.
           03  FILLER          PIC  X(54).
           03  R-BTOT          PIC  Z(06)9.99-.
           03  FILLER          PIC  X(15).
       01  R-L10.
           03  FILLER          PIC  X(10).
           03  I-H8            PIC  X(09).
           03  I-TOTTAX        PIC  Z(07)9.99-.
           03  FILLER          PIC  X(02).
           03  I-H9            PIC  X(08).
           03  I-TOTEXE        PIC  Z(07)9.99-.
           03  FILLER          PIC  X(02).
           03  I-H10           PIC  X(08).
           03  I-TOTGST        PIC  Z(07)9.99-.
           03  FILLER          PIC  X(02).
           03  I-H11           PIC  X(07).
           03  I-TOTBCH        PIC  Z(07)9.99-.
       01  TL-L3.
          03  FILLER        PIC  X(01).
           03  TL-LEDG         PIC  9(03).
           03  FILLER          PIC  X(01).
           03  TL-NME          PIC  X(40).
           03  FILLER          PIC  X(01).
          03  TL-ACT        PIC  99/99/9999.
           03  TL-PD           PIC  Z(05)9.99-.
           03  FILLER          PIC  X(01).
           03  TL-MTH          PIC  9(01).
          03  FILLER        PIC  X(01).
           03  TL-BAL          PIC  Z(05)9.99-.
       01  I-L1.
           03 I-H1             PIC  X(07).
           03 I-H2             PIC  X(35).
           03 I-H3             PIC  X(09).
           03 I-H4             PIC  X(14).
           03 I-H5             PIC  X(14).
           03 I-H6             PIC  X(11).
           03 I-H7             PIC  X(06).
       01  I-L8.
           03 I-AC             PIC  X(06).
           03 FILLER           PIC  X(01).
          03 I-NAME        PIC  X(31).
           03 FILLER           PIC  X(01).
          03 I-DATE        PIC  Z9/99/9999.
          03 FILLER        PIC  X(01).
           03 I-REF            PIC  X(08).
           03 FILLER           PIC  X(01).
           03 I-CAP            PIC  Z(07)9.99-.
           03 FILLER           PIC  X(01).
           03 I-GST            PIC  Z(06)9.99-.
           03 FILLER           PIC  X(01).
           03 I-TOT            PIC  Z(07)9.99-.
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
       77  WS-CHECK                PIC  X(18)           VALUE "aeWlimemnomLalismJ".
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
       77  WS-DBTKEY               PIC  9(06)    COMP-5.
       77  WS-PARKEY               PIC  9(06)    COMP-5.
       77  WS-RECKEY               PIC  9(06)    COMP-5.
       77  WS-RECOVER              PIC  9(06)    COMP-5.
       77  WS-SHARED               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-TRANS                PIC  9(04)    COMP-5 VALUE 1.
       77  WS-FIN-RATE             PIC S9(03)V99 COMP-3.
       77  WS-PERIOD               PIC  9(02).
       77  WS-NTP                  PIC  X(01).
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
       77  WS-ALLOCATE             PIC  X(01)           VALUE "M".
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
       77  WS-AC                   PIC  X(07)           VALUE "ACCOUNT".
       77  PRG-NAME                PIC  X(12)           VALUE SPACES.
       77  PRG-DEBT-LUP            PIC  X(12)           VALUE "DTP\DTPLOOK".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "DTP014".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "ACCOUNTS RECEIVABLE - BATCH TRANSACTIONS".
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
           03  WS-MODULE       PIC  X(03) VALUE "DTP".
           03  WS-PROG         PIC  X(03) VALUE "014".

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
               05  W02-REPORT      PIC X(07)     VALUE "DTP014.".
               05  W02-USER        PIC X(05)     VALUE SPACES.

       COPY "W05.VAT".

       01  W05-DISC.
          03  W05-DISCOUNT.
               05  W05-DSETT       PIC S9(03)V99 COMP-3 OCCURS 10.

       01  W08-EXPD.
           03  W08-CHAR            PIC  X(06).
           03  W08-DEC             PIC  X(01)    COMP-X OCCURS 6 REDEFINES W08-CHAR.

       01  W10-SPARES.
          03  W10-DATE             PIC X(10).
          03  W10-EDTE                           REDEFINES W10-DATE PIC 99/99/9999.

       01  W10-REF.
           03  W10-NUM             PIC  9(08).

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
           03  W25-RESULT          PIC 9(05)V99.
           03  W25-RESULT1                       REDEFINES W25-RESULT.
               05  W25-WHOLE       PIC 9(05).
               05  W25-DECIMAL     PIC 9(02).
           03  W25-RESULT2                       REDEFINES W25-RESULT.
               05  W25-KEY         PIC 9(04).
               05  W25-SUB         PIC 9(01).
               05  FILLER          PIC 9(02).
           03  W25-TOTAL           PIC S9(09)V99 COMP-3.
           03  W25-VALUE           PIC S9(09)V99 COMP-3.

       01  W30-AGE-DATES.
           03  W30-120             PIC  9(08)    COMP-3.
           03  W30-90              PIC  9(08)    COMP-3.
           03  W30-60              PIC  9(08)    COMP-3.
           03  W30-30              PIC  9(08)    COMP-3.
           03  W30-CUR             PIC  9(08)    COMP-3.

       COPY "W40.DBT".

       COPY "W40.WS".

       01  W45-TRAN.
           03  W45-CODE            PIC  9(02).
           03  W45-ENG             PIC  X(12).
           03  W45-AFR             PIC  X(12).
           03  W45-ACT             PIC  X(01).

       COPY "FUNCTION.WS".

      *COPY "W50.WS".

       01  W75-KEYS.
           03  W75-S               PIC  9(02)    COMP-5.
           03  W75-S1              PIC  9(02)    COMP-5.
           03  W75-KEY             PIC  9(06)    COMP-5.
           03  W75-ALLOCATED       PIC S9(09)V99 COMP-3.
           03  W75-BALANCE         PIC S9(09)V99 COMP-3.
           03  W75-DNO             PIC  X(06)    OCCURS 18.
           03  W75-CODE            PIC  Z9.

       01  W80-ACCOUNT.
           03  W80-ACNO            PIC  X(06).
      *    03  W80-TYPE            PIC  9(01).

       01  W85-PASS.
           03  W85-SUPER           PIC  X(06)    OCCURS 9.
           03  W85-ENTRY           PIC  9(02)    COMP-3.
           03  W85-MARG            PIC S9(02)V99.

       01  W85-BATCHES.
               05  W85-BATCH       PIC  9(06)    COMP-5.

       COPY "W90.DBT".

       01  W95-STM.
           03  W95-COMP        PIC X(40).
           03  W95-V2.
               05  W95-B       PIC Z(06)9.99-.

       01  W100-EDIT.
           03  W100-LBAL       PIC Z(06)9.99-.
           03  W100-LARR       PIC Z(06)9.99.
           03  W100-BAL        PIC Z(08)9.99-.
           03  W100-CUR        PIC Z(08)9.99-.
           03  W100-30         PIC Z(08)9.99-.
           03  W100-60         PIC Z(08)9.99-.
           03  W100-90         PIC Z(08)9.99-.
           03  W100-120        PIC Z(08)9.99-.
           03  W100-INT        PIC Z(08)9.99B.
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

       COPY "DEBLUP.CRT".

       COPY "TRNLUP.CRT".

       01  S08A.
      *                                     1         2         3         4         5         6         7         8
      *                            12345678901234567890123456789012345678901234567890123456789012345678901234567890
      *
           03  LINE 16 COLUMN  6 VALUE "Allocation:".
           03  LINE 17 COLUMN  6 VALUE "Ref. No.".
           03          COLUMN 18 VALUE             "Date".
           03          COLUMN 26 VALUE                     "Transaction".
           03          COLUMN 46 VALUE                                         "Value".
           03          COLUMN 57 VALUE                                                    "Balance".
           03          COLUMN 69 VALUE                                                                "Allocate".

       01  S12.
           03  S12A.
               05  LINE  9 COLUMN 12 VALUE "Name :".
           03  S12B.
               05  LINE 11 COLUMN 12 VALUE "Bal  :".
               05  LINE 12 COLUMN 12 VALUE "Mtd  :".
               05  LINE 13 COLUMN 12 VALUE "Cur  :".
               05  LINE 14 COLUMN 12 VALUE "30d  :".
               05  LINE 15 COLUMN 12 VALUE "60d  :".
               05  LINE 16 COLUMN 12 VALUE "90d  :".
               05  LINE 17 COLUMN 12 VALUE "120d :".
               05  LINE 18 COLUMN 12 VALUE "Int  :".
           03  S12C.
               05  LINE 19 COLUMN 12 VALUE "Limit:".

       01  S12-DATA.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
               05  LINE 11 COLUMN 20 PIC  Z(08)9.99- FROM W40-BAL.
               05  LINE 12 COLUMN 20 PIC  Z(08)9.99- FROM W40-ONE.
               05  LINE 13 COLUMN 20 PIC  Z(08)9.99- FROM W40-CUR.
               05  LINE 14 COLUMN 20 PIC  Z(08)9.99- FROM W40-30 .
               05  LINE 15 COLUMN 20 PIC  Z(08)9.99- FROM W40-60 .
               05  LINE 16 COLUMN 20 PIC  Z(08)9.99- FROM W40-90 .
               05  LINE 17 COLUMN 20 PIC  Z(08)9.99- FROM W40-120.
               05  LINE 18 COLUMN 20 PIC  Z(08)9.99- FROM W40-INTEREST.
               05  LINE 19 COLUMN 22 PIC  Z(06)9     FROM W40-CRL.
                         
       01  S26.
      *                                         1         2         3         4         5         6         7         8
      *                                12345678901234567890123456789012345678901234567890123456789012345678901234567890
      *
           03  LINE 6 COLUMN  2  VALUE "Batch:".
           03  LINE 7 COLUMN  2  VALUE "Account".
           03         COLUMN 12  VALUE           "Date".
           03         COLUMN 20  VALUE                   "Cde/description".
           03         COLUMN 38  VALUE                                     "Ref".
           03         COLUMN 51  VALUE                                                  "Value".
           03         COLUMN 64  VALUE                                                               "TAX".
           03         COLUMN 75  VALUE                                                                          "Total".

       01  S26-DATA.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
             04  S26-DSP1.
               05  S26-DTE.
                   07  LINE  9 COLUMN  9 PIC  99/99/9999 USING W90-DATE AUTO.

               05  S26-CDE.
                   07  LINE  9 COLUMN 20 PIC  9(02)     USING W90-CODE AUTO.
                   
               05  S26-DESC.
                   07  LINE  9 COLUMN 23 PIC  X(12)     FROM W90-DESC FOREGROUND-COLOR Cyan HIGHLIGHT.
                   
               05  S26-REF.
                   07  LINE  9 COLUMN 36 PIC  X(08)     USING W90-REF AUTO.    
                    
               05  S26-VAL.
                   07  LINE  9 COLUMN 45 PIC  Z(07)9.99 USING W90-VAL AUTO.

               05  S26-VAT.
                   07  LINE  9 COLUMN 57 PIC  Z(06)9.99 USING WS-VAT-VALUE AUTO.

             04  S26-DSP2.
               05  S26-DT.
                   07  LINE  9 COLUMN 68 PIC  Z(08)9.99 USING W90-DB AUTO.

               05  S26-CR.
                   07  LINE  9 COLUMN 68 PIC  Z(08)9.99 USING W90-CR AUTO.

       01  S29-S36.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
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
                   07  PIC Z(08)9.99- USING W40-INT AUTO.
               
               05  S36.
                   07  PIC X(06) USING W80-ACNO AUTO.

       01  S-ALLOC.
      *                             1         2         3         4         5         6         7         8
      *                    12345678901234567890123456789012345678901234567890123456789012345678901234567890
      *
           03  LINE 21 COLUMN 14 VALUE "Un-allocated balance : ".
           03          COLUMN 37 PIC                           Z(08)9.99                            FROM W75-BALANCE   BACKGROUND-COLOR Magenta FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 51 VALUE                                       "Allocated : ".
           03          COLUMN 63 PIC                                                      Z(08)9.99 FROM W75-ALLOCATED BACKGROUND-COLOR Magenta FOREGROUND-COLOR Brown HIGHLIGHT.

       01  S-DTRN.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
      *                              1         2         3         4         5         6         7         8
      *                     12345678901234567890123456789012345678901234567890123456789012345678901234567890
      *
               05  COLUMN  6 PIC X(08)                                                                 FROM  DBT-REF.
               05  COLUMN 15 PIC          9999/99/99                                                   FROM  DBT-DATE.
               05  COLUMN 26 PIC                     X(12)                                             FROM  W00-E-DESC.
               05  COLUMN 39 PIC                               Z(08)9.99                               FROM  DBT-VALUE.
               05  COLUMN 52 PIC                                            Z(08)9.99                  FROM  DBT-OUTSTD.
               05  COLUMN 65 PIC                                                            Z(08)9.99  USING DBT-ALLOC AUTO.

       01  S-OPT.
           03  BACKGROUND-COLOR Black FOREGROUND-COLOR Green.
               05  LINE 22 COLUMN 21 VALUE "Press ".
               05          COLUMN 27 VALUE       " "                             FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 30 VALUE           " to view,".
               05          COLUMN 39 VALUE                    "A"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 40 VALUE                     "mend,".
               05          COLUMN 45 VALUE                          "D"                FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 46 VALUE                           "elete or ".
               05          COLUMN 55 VALUE                                    "Esc"    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 58 VALUE                                       "ape".
       
       01  S-VIEW.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
               05  LINE 50 COLUMN  2 VALUE "F6"                                         FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN  4 VALUE   "=View Transaction Codes, ".
               05          COLUMN 29 VALUE                            "Esc"             FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 32 VALUE                               "ape to Cancel".
               
       01  S-USE-OPT.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
               05  LINE 50 COLUMN  2 VALUE "F1"                                          FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN  4 VALUE   "=Help,".
               05          COLUMN 10 VALUE         "F2"                                  FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 12 VALUE           "=Debtor Lookup,".
               05          COLUMN 27 VALUE                          "F8"                 FOREGROUND-COLOR Red HIGHLIGHT.
               05          COLUMN 29 VALUE                            "=Create,".
               05          COLUMN 37 VALUE                                    "Esc"      FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 40 VALUE                                       "=Exit".
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


       AA05.
             PERFORM ERASE-SCREEN.
             DISPLAY "DEBTORS - BATCH TRANSACTIONS" AT 0325 WITH FOREGROUND-COLOR Grey HIGHLIGHT.

       AA10.
             MOVE "'C'apture,'R'eview,'L'ist,'P'ost,'E'xit  [ ]" TO WS-OPT-MES.
             MOVE SPACE              TO WS-OPTION.
             MOVE "W"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
             EVALUATE WS-OPTION
               WHEN "C"    PERFORM CG000
               WHEN "L"    PERFORM DA000
               WHEN "P"    PERFORM FA000
               WHEN "R"    PERFORM EA000
               WHEN "E"    GO TO AA30
             END-EVALUATE.
             GO TO AA05.

       AA30.
            CLOSE DRBTCH RECOVER.

       AA49.
             EXIT PROGRAM.

       COPY "AA50D.LUP".
      
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
      *    ³                                                           ³
      *    ³ If character 48 of the message contains a value of X"FA"  ³
      *    ³ then do not display the message in a window, only get the ³
      *    ³ User reponse at the position specified by WS-MES-LINE and ³
      *    ³ WS-MES-COL.                                               ³
      *    ³                                                           ³
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

       COPY "DATE.CHK".

       COPY "DATE.CNV".

       COPY "DEBTOR.TRN".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000              SECTION.

       COPY "CONTROL.RD".

       COPY "DBTRAN.RD1".

       COPY "DEBTOR.RD".

       COPY "DEBTRN.RD".

       COPY "DEPART.RD".

       COPY "DRBTCH.RD".

       COPY "LEDTRF.RD".

       COPY "PARAM.RD".

       COPY "SHARED.RD".

      *
      *   R E W R I T E   &    W R I T E   F I L E S   R O U T I N E S
      *
       AD000             SECTION.

       COPY "CONTROL.WR".

       COPY "DBTRAN.WR".

       COPY "DEBTOR.WR".

       COPY "DEBTRN.WR".

       COPY "DEPART.WR".

       COPY "DRBTCH.WR".

       COPY "LEDTRF.WR".

       COPY "PARAM.WR".

       COPY "TXTRAN.WR".

       COPY "DEBTOR.ALL".

       COPY "DBTRAN.LUP".

       COPY "APAC.HLP".

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

       AY19.
             MOVE 19                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE.
             MOVE TAX-RECORD1        TO REC-TXTRAN.
             GO TO AY50.
      *    *************************************************************
      *    ****            D E P A R T M E N T   F I L E            ****
      *    *************************************************************
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
      *    ****    Start processing transaction
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
      *     Q   F U L L  -  W A I T   F O R   A  C O U N T  O F   400
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
      *                          NETWORK record 2.
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
      *    ****      Read debtor record - using account number.
      *    *************************************************************
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

       COPY "DEBTOR.TRA".

       COPY "LEDTRF.UPD".

       COPY "DEBTOR.ALL".

      *
      *    ****    D E B T O R  T R A N S A C T I O N S
      *               -   C A P T U R E   B A T C H
      *
       CG000         SECTION 5.
       CG00.
             PERFORM ERASE-SCREEN.
             DISPLAY S26.
       CG05.
             MOVE ZERO               TO W90-DEBIT W90-CREDIT W90-DISCOUNT W90-VATTOT.
       CG06.
             MOVE ZERO               TO W90-CUR W90-30 W90-60 W90-90 W90-120 W90-INT W90-BCR W90-BDT W90-DISC W40-SDISC.
             DISPLAY "Accumulating Totals" AT 1212.

       CG10.
             PERFORM READ-DRBTCH-NEXT THRU READ-DRBTCH-EXIT.
           IF WS-F-ERROR = 42
               MOVE 12               TO CRT-START CRT-END
               PERFORM ERASE-SCREEN
               GO TO CG020-TRAN.
             DISPLAY DRB-AC AT 1232 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
           IF DRB-ACTION = 1
               ADD DRB-TOTAL         TO W90-CREDIT
           ELSE
               ADD DRB-TOTAL         TO W90-DEBIT.
             GO TO CG10.

       CG020-TRAN.
             MOVE SPACES             TO W80-ACNO.
             MOVE ZERO               TO W90-CODE.
             MOVE W12-TODAY          TO W90-DTE W90-DATE.

       CG22.
             MOVE ZERO               TO W90-DB W90-CR W90-EX.
             MOVE SPACES             TO W90-REF.
             MOVE 1                  TO WS-S9.

       CG23.
             MOVE ZERO               TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
           IF WS-S9 < 12
               ADD 1                 TO WS-S9
               GO TO CG23.

       CG025-ACCEPT.
             DISPLAY S-USE-OPT.
             ACCEPT S36 AT 0702.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY   MOVE SPACES TO W80-ACNO
                                GO TO CG999
                 WHEN F1-KEY    PERFORM HELP-ROUTINE
                 WHEN F2-KEY    PERFORM AA50
                 WHEN F8-KEY    PERFORM GA000
                                MOVE LS-ACNO TO W80-ACNO
                 WHEN OTHER     PERFORM AA900-ALARM
                                GO TO CG025-ACCEPT
               END-EVALUATE
               DISPLAY S36 AT 0702
               IF W80-ACNO = SPACES
                   GO TO CG025-ACCEPT.
             PERFORM CLEAR-L50.
           IF W80-ACNO = SPACES
               GO TO CG999.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W80-ACNO BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             MOVE W80-ACNO           TO DEB-ACNO.
             PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR = 1
               MOVE WS-ERR2          TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CG025-ACCEPT.
           IF WS-ERROR = 2
               MOVE WS-ERR1          TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CG025-ACCEPT.
      *    *************************************************************
      *    ****         T E S T   F O R   I N D I V I D U A L       ****
      *    *************************************************************
           IF W40-TYPE = 1
               MOVE "D"              TO W40-INSTR
               CALL "DTP\DTPNAME" USING W40-COMPANY LS-USER-ID
           ELSE
               MOVE W40-NAME         TO W40-DSP-NAME.
             MOVE 9                  TO CRT-START.
             MOVE 19                 TO CRT-END.
             PERFORM ERASE-SCREEN.
             DISPLAY S12.
             DISPLAY W40-DSP-NAME AT 0920 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             COMPUTE W40-ONE = W40-BAL - (W40-CUR + W40-30 + W40-60 + W40-90 + W40-120 + W40-INT).
             DISPLAY W12-DATA.
           IF WS-OPTION = "N"
               UNLOCK DEBTOR
               GO TO CG22.
            PERFORM CLEAR-L50.
            DISPLAY "Press " AT 5002      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                          "Esc"           WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                             " to CANCEL" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.

       CG030-ACCEPT.
             MOVE W90-DATE           TO W22-DATE.
             MOVE 0909               TO DATEPOS.
             PERFORM DATE-CHECK-ACCEPT-PLUS THRU CHECK-DATE-EXIT.
           IF W22-INSTR = X"EC"
               PERFORM CG047
               GO TO CG22.
             MOVE 1                  TO WS-VAT-SUB.
           IF W20-DTE < WS-VAT-DATE(WS-VAT-SUB)
               ADD 6                 TO WS-VAT-SUB.

       CG035-ACCEPT.
             DISPLAY S-VIEW.
             ACCEPT W90-CODE AT 0920 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT UPDATE AUTO.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY   PERFORM CG047
                                GO TO CG22
                 WHEN F6-KEY    PERFORM SAVE-SCREEN
                                PERFORM TRANCDE-VIEW
                                PERFORM RESTORE-SCREEN
                                GO TO CG035-ACCEPT
                 WHEN OTHER     PERFORM AA900-ALARM
                                GO TO CG035-ACCEPT
               END-EVALUATE.
           IF W90-CODE = ZERO
               GO TO CG025-ACCEPT.
           IF W90-CODE < 01 OR > 99
               GO TO CG040-ERROR.
             PERFORM GET-DEBTOR-TRAN-DESC.
           IF W00-T-CODE = ZERO
               GO TO CG040-ERROR.
             MOVE W00-E-DESC         TO W90-DESC.
             DISPLAY W90-DESC AT 0923 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             GO TO CG042-REF.

       CG040-ERROR.
             MOVE "Code"             TO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.
             GO TO CG035-ACCEPT.

       CG042-REF.
             ACCEPT W90-REF AT 0936 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT UPDATE AUTO.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY   PERFORM CG047
                                GO TO CG22
                 WHEN OTHER     PERFORM AA900-ALARM
                                GO TO CG042-REF
               END-EVALUATE.

       CG045-VALUE.
             PERFORM CLEAR-L50.
           IF W00-T-CODE = 02 OR 05 OR 06 OR 17 OR 20
               MOVE "N"              TO WS-TAX
           ELSE
               PERFORM CG046 THRU CG048.
           IF W00-ACTION = 1
               MOVE ZERO             TO W90-DB
               GO TO CG055-CREDIT.
             GO TO CG050-DEBIT.

       CG046.
             MOVE "VAT: 'N'o, 'I'ncluded, 'K'ey in, 'C'alculate [I]" TO WS-OPT-MES.
             MOVE "I"                TO WS-OPTION.
             MOVE "M"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
             MOVE WS-OPTION          TO WS-TAX.
             GO TO CG048.

       CG047.
             MOVE 9                  TO CRT-START CRT-END
             PERFORM ERASE-SCREEN

       CG048.
             EXIT.
      *
      *   ****    D E B I T   T R A N S A C T I O N
      *
       CG050-DEBIT.
           IF W00-T-CODE = 13
               ACCEPT S26-VAT
               MOVE W90-VAT(1)       TO W90-DB
               DISPLAY S26-DT
           ELSE
               EVALUATE WS-TAX
                 WHEN "N"    ACCEPT S26-DT
                             MOVE W90-DB            TO W90-VAL W90-EX
                             DISPLAY S26-VAL
                 WHEN "C"    ACCEPT S26-VAL
                             PERFORM CG52
                             ADD W90-VAL W90-VAT(1) GIVING W90-DB
                             DISPLAY S26-VAT
                             DISPLAY S26-DT
                 WHEN "K"    ACCEPT S26-VAL
                             ACCEPT S26-VAT
                             ADD W90-VAL W90-VAT(1) GIVING W90-DB
                             DISPLAY S26-DT
                 WHEN "I"    ACCEPT S26-DT
                             PERFORM CG51
                             DISPLAY S26-VAL
                             DISPLAY S26-VAT
               END-EVALUATE.
           IF W90-DB = ZERO
               GO TO CG060-CORRECT.
           IF NOT((W90-CODE = 01) 
               IF NOT(DEB-SDISC = ZERO))
                   GO TO CG50B.

       CG50A.
             MOVE "Does settlement discount apply Y/N?  [Y]" TO WS-OPT-MES.
             MOVE "Y"                TO WS-OPTION.
             MOVE "1"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "Y"
               MOVE DEB-SDISC            TO W40-SDISC
               MOVE W05-DSETT(W40-SDISC) TO W90-DISC.
       CG50B.
             MOVE ZERO               TO W90-CR.
             GO TO CG060-CORRECT.

       CG51.
             MOVE W05-VAT(WS-VAT-SUB) TO W05-RATE.
             COMPUTE W90-VAT(1) ROUNDED = ((W90-DB / (100.000 + W05-RATE)) * W05-RATE)
             SUBTRACT W90-VAT(1) FROM W90-DB GIVING W90-VAL.
       CG52.
             MOVE W05-VAT(WS-VAT-SUB) TO W05-RATE.
             COMPUTE W90-VAT(1) ROUNDED = (W90-VAL * W05-RTE).

       CG53.
             MOVE W05-VAT(WS-VAT-SUB) TO W05-RATE.
             COMPUTE W90-VAT(1) ROUNDED = ((W90-CR / (100.000 + W05-RATE)) * W05-RATE)
             SUBTRACT W90-VAT(1) FROM W90-CR GIVING W90-VAL.
      *
      *   ****    C R E D I T   T R A N S A C T I O N
      *
       CG055-CREDIT.
           IF W00-T-CODE = 14
               ACCEPT S26-VAT
               MOVE W90-VAT(1)       TO W90-CR
               DISPLAY S26-CR
           ELSE
               EVALUATE WS-TAX
                 WHEN "N"    ACCEPT S26-CR
                             MOVE W90-CR  TO W90-VAL W90-EX
                             DISPLAY S26-VAL
                 WHEN "C"    ACCEPT S26-VAL
                             PERFORM CG52
                             ADD W90-VAL W90-VAT(1) GIVING W90-CR
                             DISPLAY S26-VAT
                             DISPLAY S26-CR
                 WHEN "K"    ACCEPT S26-VAL
                             ACCEPT S26-VAT
                             ADD W90-VAL W90-VAT(1) GIVING W90-CR
                             DISPLAY S26-CR
                 WHEN "I"    ACCEPT S26-CR
                             PERFORM CG53
                             DISPLAY S26-VAL
                             DISPLAY S26-VAT
               END-EVALUATE.

       CG060-CORRECT.
           IF W90-CR = 0 AND W90-DB = 0
               MOVE "Value"          TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CG025-ACCEPT.
           IF W90-CODE NOT = 17
               GO TO CG060A.
           IF W90-CR NOT > DEB-INT
               GO TO CG060A.
             MOVE "Credit > Interest on account"    TO WS-ERR-MES.
             MOVE "'ENTER' to change - 'X' to exit" TO WS-ERR-MES-2.
             PERFORM ERROR-MESSAGE-2.
           IF WS-OPTION = "X"
               GO TO CG025-ACCEPT.
             GO TO CG045-VALUE.

       CG060A.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO CG025-ACCEPT.
             INITIALIZE DRB-REC.
             MOVE W20-DTE            TO DRB-DATE.
             MOVE W90-CODE           TO DRB-CODE.
             MOVE W90-REF            TO DRB-REF.
             MOVE DEB-ACNO           TO DRB-AC.
             MOVE W40-DSP-NAME       TO DRB-NAME.
             MOVE W00-E-DESC         TO DRB-DESC.
             MOVE W90-VAT(1)         TO DRB-TAX.
             MOVE W90-VAL            TO DRB-VALUE.
             MOVE W90-EX             TO DRB-TAXFREE.
             MOVE W00-ACTION         TO DRB-ACTION.
           IF W00-ACTION = 1
              MOVE W90-CR            TO DRB-TOTAL
           ELSE
               MOVE W90-DB           TO DRB-TOTAL.
             MOVE WS-TAX             TO DRB-VAT.
             MOVE ZERO               TO WS-F-ERROR.
             PERFORM WRITE-DRBTCH THRU WRITE-DRBTCH-EXIT.
           IF WS-F-ERROR = 42
               MOVE "Reference duplicated" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CG042-REF.
             ADD W90-DB              TO W90-BDT W90-DEBIT.
             ADD W90-CR              TO W90-BCR W90-CREDIT.
             MOVE 09                 TO CRT-START.
             MOVE 26                 TO CRT-END.
             PERFORM ERASE-SCREEN.
             MOVE W90-DEBIT          TO W100-BAL.
             MOVE W90-CREDIT         TO W100-CUR.
             DISPLAY "Debit total:"  AT 2112.
             DISPLAY W100-BAL        AT 2125 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY "Credit total:" AT 2139.
             DISPLAY W100-CUR        AT 2153 WITH BACKGROUND-COLOR Red     FOREGROUND-COLOR Cyan HIGHLIGHT.
             GO TO CG22.

       CG999.
             EXIT.
      *
      *    ****    D E B T O R   T R A N S A C T I O N S
      *                   -   L I S T  B A T C H
      *
       DA000         SECTION 51.
       DA00.
             PERFORM ERASE-SCREEN.
             INITIALIZE DRB-REC.
             PERFORM START-AT-DRBTCH-KEY THRU READ-DRBTCH-EXIT.
           IF WS-F-ERROR = 42
               MOVE "No transactions to be listed" TO WS-ERR-MES
               MOVE SPACE            TO WS-OPTION
               PERFORM OPT-MESSAGE
               GO TO DA999.
             DISPLAY S26.
             DISPLAY "LIST DEBTOR TRANSACTIONS" AT 0529 WITH FOREGROUND-COLOR Grey HIGHLIGHT.

       DA05.
             MOVE ZERO               TO W90-DEBIT W90-CREDIT W90-VATTOT.

       DA06.
             MOVE ZERO               TO W90-CUR W90-30 W90-60 W90-90 W90-120 W90-INT W90-BCR W90-BDT W90-DISC W40-SDISC.

       DA08.
             MOVE 0                  TO WS-PAGE.
             MOVE WS-12CPI           TO R-DET1.
             WRITE R-L1 BEFORE 0 LINES.

       DA10.
             MOVE SPACES             TO R-DET1.
             MOVE "PAGE:"            TO R-PAGE.
             MOVE "DATE:"            TO R-DATE.
             ADD 1                   TO WS-PAGE.
             MOVE W12-TODAY          TO R-DTE.
             MOVE WS-PAGE            TO R-P-NO.
             MOVE "DEBTOR TRANSACTIONS - BATCH LIST" TO R-H5.
             MOVE "W/STAT."          TO R-H6.
             MOVE LS-USER            TO R-USER.
             WRITE R-L1 BEFORE 2 LINES.
             MOVE SPACES             TO R-DET1.
             MOVE WS-EXPP            TO R-L1.
             WRITE R-L1 BEFORE 0 LINES.
             MOVE SPACES             TO R-DET1.
             MOVE W95-COMP           TO R-CO.
             WRITE R-L1 BEFORE 0 LINES.
             WRITE R-L1 BEFORE 0 LINES.
             MOVE WS-ECAN            TO R-L1.
       DA15.
             WRITE R-L1 BEFORE 2 LINES.
             MOVE SPACES             TO R-DET1.
             MOVE "AC.NO"            TO R-H7.
             MOVE "DEBTOR NAME"      TO R-H7A.
             MOVE "DATE"             TO R-H8.
             MOVE "TRAN TYPE"        TO R-H10.
             MOVE "REF"              TO R-H11.
             MOVE "VALUE"            TO R-H12.
             MOVE "VAT"              TO R-H13.
             MOVE "TOTAL"            TO R-H14.
             WRITE R-L1 BEFORE 2 LINES.
             MOVE SPACES             TO R-DET1.
             MOVE 1                  TO WS-S9.

       DA20.
            MOVE ZERO                TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
          IF WS-S9 < 12
              ADD 1                  TO WS-S9
              GO TO DA20.

       DA22.
             MOVE ZERO               TO W90-DB W90-CR W90-EX.
             MOVE SPACES             TO W90-REF.
             PERFORM READ-DRBTCH-NEXT THRU READ-DRBTCH-EXIT.
           IF WS-F-ERROR = 42
               GO TO DA72.
             MOVE DRB-DATE           TO W22-DTE2.
             PERFORM SWITCH-DATE-TO-DMCY.
             MOVE W22-DTE3           TO W90-DATE.
             MOVE DRB-CODE           TO W90-CODE.
             MOVE DRB-REF            TO W90-REF.
             MOVE DRB-AC             TO W80-ACNO.
             MOVE DRB-NAME           TO W40-DSP-NAME.
             MOVE DRB-TAX            TO W90-VAT(1).
             MOVE DRB-VALUE          TO W90-VAL.
             MOVE DRB-TAXFREE        TO W90-EX.
           IF DRB-ACTION = 1
               MOVE DRB-TOTAL        TO W90-CR
           ELSE
               MOVE DRB-TOTAL        TO W90-DB.
             MOVE DRB-VAT            TO WS-TAX.
             PERFORM GET-DEBTOR-TRAN-DESC.
             MOVE W00-E-DESC         TO W90-DESC.
             DISPLAY S36 AT 0902.
             DISPLAY W40-DSP-NAME AT 1102 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             DISPLAY S26-DSP1.
      *      DISPLAY S26-DTE.
      *      DISPLAY W90-CODE     AT 0920 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.
      *      DISPLAY W90-DESC     AT 0923 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
      *      DISPLAY W90-REF      AT 0936 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.
      *      DISPLAY S26-VAL.
      *      DISPLAY S26-VAT.
           IF W00-ACTION = 1
               DISPLAY S26-CR
           ELSE
               DISPLAY S26-DT.
             ADD W90-DB              TO W90-BDT W90-DEBIT.
             ADD W90-CR              TO W90-BCR W90-CREDIT.
             ADD W90-VAT(1)          TO W90-VATTOT.
             MOVE W80-ACNO           TO R-TAC.
             MOVE W40-DSP-NAME       TO R-DNME.
             MOVE W90-DATE           TO W90-DTE.
             MOVE W90-DTE            TO R-TDTE.
             MOVE W90-DESC           TO R-TDESC.
             MOVE W90-REF            TO R-TREF.
             MOVE W90-VAL            TO R-VAL.
             MOVE W90-VAT(1)         TO R-TAX.
           IF W90-DB > 0
               MOVE W90-DB           TO R-TOT
           ELSE
               MULTIPLY -1 BY W90-CR GIVING R-TOT.
          IF LINAGE-COUNTER < (WS-PRN-LENGTH - 2)
               WRITE R-L8 BEFORE 1 LINES
           ELSE
               WRITE R-L8 BEFORE PAGE
               PERFORM DA10 THRU DA15.
             MOVE SPACES             TO R-DET1.
             GO TO DA22.

       DA72.
             WRITE R-L8 BEFORE 1 LINES.
             MOVE "Total"            TO R-TDESC.
             MOVE W90-VATTOT         TO R-TAX.
             COMPUTE R-TOT = W90-DEBIT - W90-CREDIT.
             WRITE R-L8 BEFORE PAGE.

       DA999.
             EXIT.
      *
      *    ****    D E B T O R   T R A N S A C T I O N S
      *                 -   R E V I E W   B A T C H
      *
       EA000         SECTION 5.
       EA00.
             PERFORM ERASE-SCREEN.
             INITIALIZE DRB-REC.
             PERFORM START-AT-DRBTCH-KEY THRU READ-DRBTCH-EXIT.
           IF WS-F-ERROR = 42
               MOVE "No batch outstanding" TO WS-ERR-MES
               MOVE SPACE            TO WS-OPTION
               PERFORM OPT-MESSAGE
               GO TO EA999.
             DISPLAY S26.

       EA05.
             MOVE ZERO               TO W90-CUR W90-30 W90-60 W90-90 W90-120 W90-INT W90-DISC W90-BCR W90-BDT W40-SDISC W90-CREDIT W90-DEBIT.
             DISPLAY "Accumulating Totals" AT 1212.

       EA10.
             PERFORM READ-DRBTCH-NEXT THRU READ-DRBTCH-EXIT.
           IF WS-F-ERROR = 42
               MOVE 12               TO CRT-START CRT-END
               PERFORM ERASE-SCREEN
               GO TO EA15.
             DISPLAY DRB-AC AT 1232 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
           IF DRB-ACTION = 1
               ADD DRB-TOTAL         TO W90-CREDIT
           ELSE
               ADD DRB-TOTAL         TO W90-DEBIT.
             GO TO EA10.

       EA15.
             MOVE W90-DEBIT          TO W100-BAL.
             MOVE W90-CREDIT         TO W100-CUR.
             DISPLAY "Debit total:"  AT 2112.
             DISPLAY W100-BAL        AT 2125 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY "Credit total:" AT 2139.
             DISPLAY W100-CUR        AT 2153 WITH BACKGROUND-COLOR Red     FOREGROUND-COLOR Cyan HIGHLIGHT.
             INITIALIZE DRB-REC.
             PERFORM START-AT-DRBTCH-KEY THRU READ-DRBTCH-EXIT.

       EA20.
             PERFORM READ-DRBTCH-NEXT THRU READ-DRBTCH-EXIT.
           IF WS-F-ERROR = 42
               GO TO EA22.
             GO TO EA25.

       EA22.
             PERFORM READ-DRBTCH-PREV THRU READ-DRBTCH-EXIT.
           IF WS-F-ERROR = 42
               GO TO EA20.
             MOVE 1                  TO WS-S9.

       EA24.
             MOVE ZERO               TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
          IF WS-S9 < 12
              ADD 1                  TO WS-S9
              GO TO EA24.

       EA25.
             MOVE ZERO               TO W90-DB W90-CR W90-EX.
             MOVE SPACES             TO W90-REF.
             MOVE DRB-DATE           TO W22-DTE2.
             PERFORM SWITCH-DATE-TO-DMCY.
             MOVE W22-DTE3           TO W90-DATE.
             MOVE DRB-CODE           TO W90-CODE.
             MOVE DRB-REF            TO W90-REF.
             MOVE DRB-AC             TO W80-ACNO.
             MOVE DRB-NAME           TO W40-DSP-NAME.
             MOVE DRB-TAX            TO W90-VAT(1).
             MOVE DRB-VALUE          TO W90-VAL.
             MOVE DRB-TAXFREE        TO W90-EX.
           IF DRB-ACTION = 1
               MOVE DRB-TOTAL        TO W90-CR
           ELSE
               MOVE DRB-TOTAL        TO W90-DB.
             MOVE DRB-VAT            TO WS-TAX.
             MOVE W80-ACNO           TO DEB-ACNO.
             PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
      *      MOVE DEB-TYPE           TO W80-TYPE.
             DISPLAY S36          AT 0902.
             PERFORM GET-DEBTOR-TRAN-DESC.
             MOVE W00-E-DESC         TO W90-DESC.
             DISPLAY W40-DSP-NAME AT 1120 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY S26-DSP1.
      *      DISPLAY S26-DTE.
      *      DISPLAY W90-CODE     AT 0920 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.
      *      DISPLAY W90-DESC     AT 0923 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
      *      DISPLAY W90-REF      AT 0936 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.
      *      DISPLAY S26-VAL.
      *      DISPLAY S26-VAT.
           IF W00-ACTION = 1
               DISPLAY S26-CR
           ELSE
               DISPLAY S26-DT.

       EA30.
             DISPLAY S-OPT.
             MOVE SPACE   TO WS-OPTION.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    GO TO EA999
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO EA30
               END-EVALUATE
           ELSE
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     GO TO EA22
                 WHEN DOWN-KEY   GO TO EA20
               END-EVALUATE
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X      TO WS-OPTION.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-OPTION = "A" OR "D")
               GO TO EA30.
             MOVE 22                 TO CRT-START CRT-END.
             PERFORM ERASE-SCREEN.
           IF DRB-ACTION = 1
               SUBTRACT DRB-TOTAL    FROM W90-CREDIT
           ELSE
               SUBTRACT DRB-TOTAL    FROM W90-DEBIT.
             PERFORM DELETE-DRBTCH-REC THRU WRITE-DRBTCH-EXIT.
           IF WS-OPTION = "D"
               GO TO EA20.

       EA030-ACCEPT.
             MOVE 0909               TO DATEPOS.
             PERFORM DATE-CHECK-ACCEPT-PLUS THRU CHECK-DATE-EXIT.
           IF W22-INSTR = X"EC"
               PERFORM EA049
               GO TO EA060A.
             MOVE 1                  TO WS-VAT-SUB.
           IF W20-DTE < WS-VAT-DATE(WS-VAT-SUB)
               ADD 6                 TO WS-VAT-SUB.

       EA035-ACCEPT.
             DISPLAY S-VIEW.
             ACCEPT W90-CODE AT 0920 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    PERFORM EA049
                                 GO TO EA060A
                 WHEN F6-KEY     PERFORM SAVE-SCREEN
                                 PERFORM TRANCDE-VIEW
                                 PERFORM RESTORE-SCREEN
                                 GO TO EA035-ACCEPT
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO EA035-ACCEPT
               END-EVALUATE.
           IF W90-CODE = ZERO
               GO TO EA035-ACCEPT.
           IF W90-CODE < 01 OR > 99
               GO TO EA040-ERROR.
             PERFORM GET-DEBTOR-TRAN-DESC.
           IF W00-T-CODE = ZERO
               GO TO EA040-ERROR.
             MOVE W00-E-DESC         TO W90-DESC.
             DISPLAY W90-DESC AT 0923 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             GO TO  EA042-REF.

       EA040-ERROR.
             MOVE "Code"             TO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.
             GO TO EA035-ACCEPT.

       EA042-REF.
             ACCEPT W90-REF AT 0936 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT UPDATE AUTO.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    PERFORM EA049
                                 GO TO EA060A
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO EA042-REF
               END-EVALUATE.

       EA045-VALUE.
             PERFORM CLEAR-L50.
           IF W00-T-CODE = 02 OR 05 OR 06 OR 17 OR 20
               MOVE "N"              TO WS-TAX
           ELSE
               PERFORM EA046.
           IF W00-ACTION = 1
               MOVE ZERO             TO W90-DB
               GO TO EA055-CREDIT.
             GO TO EA050-DEBIT.

       EA046.
             MOVE "VAT: 'N'o, 'I'ncluded, 'K'ey in, 'C'alculate [I]" TO WS-OPT-MES.
             MOVE "I"                TO WS-OPTION.
             MOVE "N"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
             MOVE WS-OPTION          TO WS-TAX.

       EA048.
             EXIT.

       EA049.
             MOVE 9                  TO CRT-START CRT-END
             PERFORM ERASE-SCREEN

       
      *
      *   ****    D E B I T   T R A N S A C T I O N
      *
       EA050-DEBIT.
           IF W00-T-CODE = 13
               ACCEPT S26-VAT
               MOVE W90-VAT(1)       TO W90-DB
               DISPLAY S26-DT
           ELSE
               EVALUATE WS-TAX
                 WHEN "N"    ACCEPT S26-DT
                             MOVE W90-DB            TO W90-VAL W90-EX
                             DISPLAY S26-VAL
                 WHEN "C"    ACCEPT S26-VAL
                             PERFORM EA52
                             ADD W90-VAL W90-VAT(1) GIVING W90-DB
                             DISPLAY S26-VAT
                             DISPLAY S26-DT
                 WHEN "K"    ACCEPT S26-VAL
                             ACCEPT S26-VAT
                             ADD W90-VAL W90-VAT(1) GIVING W90-DB
                             DISPLAY S26-DT
                 WHEN "I"    ACCEPT S26-DT
                             PERFORM EA51
                             DISPLAY S26-VAL
                             DISPLAY S26-VAT
               END-EVALUATE.
           IF W90-DB = ZERO
               GO TO EA060-CORRECT.
           IF NOT((W90-CODE = 01) 
               IF NOT(DEB-SDISC = ZERO))
                   GO TO EA50B.

       EA50A.
             MOVE "Does settlement discount apply Y/N?  [Y]" TO WS-OPT-MES.
             MOVE "Y"                TO WS-OPTION.
             MOVE "1"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "Y"
               MOVE DEB-SDISC            TO W40-SDISC
               MOVE W05-DSETT(W40-SDISC) TO W90-DISC.
       EA50B.
             MOVE ZERO               TO W90-CR.
             GO TO EA060-CORRECT.

       EA51.
             MOVE W05-VAT(WS-VAT-SUB) TO W05-RATE.
             COMPUTE W90-VAT(1) ROUNDED = ((W90-DB / (100.000 + W05-RATE)) * W05-RATE)
             SUBTRACT W90-VAT(1) FROM W90-DB GIVING W90-VAL.

       EA52.
             MOVE W05-VAT(WS-VAT-SUB) TO W05-RATE.
             COMPUTE W90-VAT(1) ROUNDED = (W90-VAL * W05-RTE).

       EA53.
             MOVE W05-VAT(WS-VAT-SUB) TO W05-RATE.
             COMPUTE W90-VAT(1) ROUNDED = ((W90-CR / (100.000 + W05-RATE)) * W05-RATE)
             SUBTRACT W90-VAT(1) FROM W90-CR GIVING W90-VAL.
      *
      *   ****    C R E D I T   T R A N S A C T I O N
      *
       EA055-CREDIT.
           IF W00-T-CODE = 14
               ACCEPT S26-VAT
               MOVE W90-VAT(1)       TO W90-CR
               DISPLAY S26-CR
           ELSE
               EVALUATE WS-TAX
                 WHEN "N"    ACCEPT S26-CR
                             MOVE W90-CR            TO W90-VAL W90-EX
                             DISPLAY S26-VAL
                 WHEN "C"    ACCEPT S26-VAL
                             PERFORM EA52
                             ADD W90-VAL W90-VAT(1) GIVING W90-CR
                             DISPLAY S26-VAT
                             DISPLAY S26-CR
                 WHEN "K"    ACCEPT S26-VAL
                             ACCEPT S26-VAT
                             ADD W90-VAL W90-VAT(1) GIVING W90-CR
                             DISPLAY S26-CR
                 WHEN "I"    ACCEPT S26-CR
                             PERFORM EA53
                             DISPLAY S26-VAL
                             DISPLAY S26-VAT
               END-EVALUATE.

       EA060-CORRECT.
           IF W90-CR = 0 AND W90-DB = 0
               MOVE "Value"          TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO EA045-VALUE.
           IF W90-CODE NOT = 17
               GO TO EA060A.
           IF W90-CR NOT > DEB-INT
               GO TO EA060A.
             MOVE "Credit > Interest on account" TO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.
             MOVE 20                 TO CRT-START.
             MOVE 24                 TO CRT-END.
             PERFORM ERASE-SCREEN.
             GO TO EA045-VALUE.

       EA060A.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO EA030-ACCEPT.
             INITIALIZE DRB-REC.
             MOVE W20-DTE            TO DRB-DATE.
             MOVE W90-CODE           TO DRB-CODE.
             MOVE DEB-ACNO           TO DRB-AC.
             MOVE W90-REF            TO DRB-REF.
             MOVE W40-DSP-NAME       TO DRB-NAME.
             MOVE W00-E-DESC         TO DRB-DESC.
             MOVE W90-VAT(1)         TO DRB-TAX.
             MOVE W90-VAL            TO DRB-VALUE.
             MOVE W90-EX             TO DRB-TAXFREE.
             MOVE W00-ACTION         TO DRB-ACTION.
           IF W00-ACTION = 1
               MOVE W90-CR           TO DRB-TOTAL
           ELSE
               MOVE W90-DB           TO DRB-TOTAL.
             MOVE WS-TAX             TO DRB-VAT.
             PERFORM WRITE-DRBTCH THRU WRITE-DRBTCH-EXIT.
           IF WS-F-ERROR = 42
               MOVE "Reference duplicated" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO EA042-REF.
             ADD W90-DB              TO W90-BDT W90-DEBIT.
             ADD W90-CR              TO W90-BCR W90-CREDIT.
             MOVE 09                 TO CRT-START.
             MOVE 24                 TO CRT-END.
             PERFORM ERASE-SCREEN.
             MOVE W90-DEBIT          TO W100-BAL.
             MOVE W90-CREDIT         TO W100-CUR.
             DISPLAY "Debit total:"  AT 2112.
             DISPLAY W100-BAL        AT 2125 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY "Credit total:" AT 2139.
             DISPLAY W100-CUR        AT 2153 WITH BACKGROUND-COLOR Red     FOREGROUND-COLOR Cyan HIGHLIGHT.
             GO TO EA20.

       EA999.
             EXIT.
      *
      *    ****    D E B T O R   T R A N S A C T I O N S
      *                -   P O S T   B A T C H
      *
       FA000         SECTION 5.
       FA00.
             PERFORM ERASE-SCREEN.
             INITIALIZE DRB-REC.
             PERFORM START-AT-DRBTCH-KEY THRU READ-DRBTCH-EXIT.
           IF WS-F-ERROR = 42
               MOVE "No transactions to be posted" TO WS-ERR-MES
               MOVE SPACE                          TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO FA999.
             DISPLAY S26.
       FA05.
             MOVE 1                  TO WS-NETKEY.
             PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             MOVE DEB-BATCH          TO W85-BATCH.
           IF DEB-BATCH = 999999
               MOVE 1                TO DEB-BATCH
           ELSE
               ADD 1                 TO DEB-BATCH.
             MOVE W85-BATCH          TO WS-BATCH.
             PERFORM REWRITE-CONTROL-UNLOCK THRU WRITE-CONTROL-EXIT.
             DISPLAY WS-BATCH AT 0609 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE ZERO               TO W90-DEBIT W90-CREDIT W90-VATTOT.
       FA06.
             MOVE ZERO               TO W90-CUR W90-30 W90-60 W90-90 W90-120 W90-INT W90-DISC W90-BCR W90-BDT W40-SDISC.

       FA08.
             MOVE 0                  TO WS-PAGE.
             MOVE WS-12CPI           TO R-DET1.
             WRITE R-L1 BEFORE 0 LINES.

       FA10.
             MOVE SPACES             TO R-DET1.
             MOVE "PAGE:"            TO R-PAGE.
             MOVE "DATE:"            TO R-DATE.
             ADD 1                   TO WS-PAGE.
             MOVE W12-TODAY          TO R-DTE.
             MOVE WS-PAGE            TO R-P-NO.
             MOVE "DEBTOR TRANSACTIONS - AUDIT TRAIL" TO R-H5.
             MOVE "BATCH:"           TO R-H6.
             MOVE WS-BATCH           TO R-BATCH.
             WRITE R-L1 BEFORE 2 LINES.
             MOVE SPACES             TO R-DET1.
             MOVE WS-EXPP            TO R-L1.
             WRITE R-L1 BEFORE 0 LINES.
             MOVE SPACES             TO R-DET1.
             MOVE W95-COMP           TO R-CO.
             WRITE R-L1 BEFORE 0 LINES.
             WRITE R-L1 BEFORE 0 LINES.
             MOVE WS-ECAN            TO R-L1.
       FA15.
             WRITE R-L1 BEFORE 2 LINES.
             MOVE SPACES             TO R-DET1.
             MOVE "AC.NO"            TO R-H7.
             MOVE "DEBTOR NAME"      TO R-H7A.
             MOVE "DATE"             TO R-H8.
             MOVE "TRAN TYPE"        TO R-H10.
             MOVE "REF"              TO R-H11.
             MOVE "VALUE"            TO R-H12.
             MOVE "VAT"              TO R-H13.
             MOVE "TOTAL"            TO R-H14.
             WRITE R-L1 BEFORE 2 LINES.
             MOVE SPACES             TO R-DET1.
             MOVE 1                  TO WS-S9.

       FA20.
             MOVE ZERO               TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
           IF WS-S9 < 12
               ADD 1                 TO WS-S9
               GO TO FA20.

       FA22.
             MOVE ZERO               TO W90-DB W90-CR W90-EX.
             MOVE SPACES             TO W90-REF.
             PERFORM READ-DRBTCH-NEXT THRU READ-DRBTCH-EXIT.
           IF WS-F-ERROR = 42
               GO TO FA72.
             MOVE DRB-DATE           TO W22-DTE2.
             PERFORM SWITCH-DATE-TO-DMCY.
             MOVE W22-DTE3           TO W90-DATE.
             MOVE DRB-CODE           TO W90-CODE.
             MOVE DRB-REF            TO W90-REF.
             MOVE DRB-AC             TO W80-ACNO.
             MOVE DRB-NAME           TO W40-DSP-NAME.
             MOVE DRB-TAX            TO W90-VAT(1).
             MOVE DRB-VALUE          TO W90-VAL.
             MOVE DRB-TAXFREE        TO W90-EX.
           IF DRB-ACTION = 1
              MOVE DRB-TOTAL         TO W90-CR
           ELSE
               MOVE DRB-TOTAL        TO W90-DB.
             MOVE DRB-VAT            TO WS-TAX.
             MOVE W80-ACNO           TO DEB-ACNO.
             PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
      *      MOVE DEB-TYPE           TO W80-TYPE.
             DISPLAY S36 AT 0902.
             PERFORM GET-DEBTOR-TRAN-DESC.
             MOVE W00-E-DESC         TO W90-DESC.
             DISPLAY W40-DSP-NAME AT 1120 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY S26-DSP1.
      *      DISPLAY S26-DTE.
      *      DISPLAY W90-CODE     AT 0920 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.
      *      DISPLAY W90-DESC     AT 0923 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
      *      DISPLAY W90-REF      AT 0936 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.
      *      DISPLAY S26-VAL.
      *      DISPLAY S26-VAT.
           IF W00-ACTION = 1
               DISPLAY S26-CR
           ELSE
               DISPLAY S26-DT.

       FA060.
             MOVE ZERO               TO W75-KEY W75-ALLOCATED.
           IF NOT(W90-CR = ZERO)
               IF OPEN-ITEM
                   MOVE 1301         TO CPOS
                   PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24
                   DISPLAY S08A
                   MOVE 19           TO CLIN
                   MOVE ZERO         TO W90-ALLOCATED W90-DISCOUNT
                   PERFORM ALLOCATE-CREDIT
                   IF WS-ERROR = 9
                       MOVE ZERO     TO W75-KEY.
             PERFORM GET-DEBTOR-TRAN THRU GET-DEBTOR-TRAN-EXIT.
             PERFORM AY01 THRU AY59.
             PERFORM GENERATE-DEBTOR-TRAN.
             ADD W90-DB              TO W90-BDT W90-DEBIT.
             ADD W90-CR              TO W90-BCR W90-CREDIT.

       FA60A.
           IF W90-CODE = 01 OR 03 OR 04 OR 19
               ADD 121 DEB-CAT       GIVING W90-RESULT
               DIVIDE W90-RESULT BY 2 GIVING W90-CATKEY REMAINDER W90-CS
               ADD 1                 TO W90-CS
               MOVE W90-CATKEY       TO WS-PARKEY
               PERFORM READ-PARAM THRU READ-PARAM-EXIT
               PERFORM AY01 THRU AY59
               ADD W90-DB            TO PAR-CDRMTD(W90-CS) PAR-CDRYTD(W90-CS)
               ADD W90-CR            TO PAR-CCRMTD(W90-CS) PAR-CCRYTD(W90-CS)
               PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
           IF W90-CODE = 17 OR 20
              GO TO FA61.
           IF OPEN-ITEM
               SUBTRACT W90-CR       FROM DEB-BAL
               GO TO FA61.
             MOVE "Allocate to selected age-analysis (Y/N)  [ ]" TO WS-OPT-MES.
             MOVE SPACE              TO WS-OPTION.
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
             MOVE ZERO               TO W40-MTD W40-CUR W40-30 W40-60 W40-120 W40-INT W40-90.
           IF NOT(WS-OPTION = "Y")
               IF W00-ACTION = 1
                   PERFORM DEBTOR-CREDITS THRU DEBTOR-TRAN-EXIT
                   GO TO FA61
               ELSE
                   GO TO FA61.
             DISPLAY S12B.
             MOVE DEB-BAL            TO W100-BAL.
             COMPUTE W40-ONE = DEB-BAL - (DEB-CUR + DEB-30 + DEB-60 + DEB-90 + DEB-120 + DEB-INT).
             DISPLAY W100-BAL AT 1120 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE W40-ONE            TO W100-CUR.
             DISPLAY W100-CUR AT 1220 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE DEB-CUR            TO W100-CUR.
             DISPLAY W100-CUR AT 1320 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE DEB-30             TO W100-30.
             DISPLAY W100-30  AT 1420 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE DEB-60             TO W100-60.
             DISPLAY W100-60  AT 1520 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE DEB-90             TO W100-90.
             DISPLAY W100-90  AT 1620 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE DEB-120            TO W100-120.
             DISPLAY W100-120 AT 1720 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE DEB-INT            TO W100-INT.
             DISPLAY W100-INT AT 1820 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY "Allocation" AT 1135.
           IF W00-ACTION = ZERO
               MOVE W90-DB           TO W25-VALUE
               GO TO FA60K.
       FA60AB.
             MOVE 1234               TO CPOS.

       FA60AL.
            DISPLAY "             " AT CPOS WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
           IF CLIN < 18
               ADD 1                 TO CLIN
               GO TO FA60AL.
             DISPLAY "Transaction balance:" AT 2012 WITH FOREGROUND-COLOR Cyan.

       FA60B.
             MOVE W25-VALUE          TO W95-B.
             DISPLAY W95-V2 AT 2034 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             PERFORM CLEAR-L50.

       FA60C.
             ACCEPT S29 AT 1234.
           IF W40-MTD > W25-VALUE
              PERFORM FA60H
              GO TO FA60C.
           IF NOT(W40-MTD = ZERO)
               IF W40-MTD > W40-ONE
                   IF W40-MTD < DEB-BAL
                       PERFORM FA60I
                       GO TO FA60C.
             SUBTRACT W40-MTD        FROM W25-VALUE.
             PERFORM FA60B.
           IF W25-VALUE = ZERO
              GO TO FA60J.

       FA60D.
            ACCEPT S30 AT 1334.
           IF W40-CUR > W25-VALUE
               PERFORM FA60H
               GO TO FA60D.
           IF NOT(W40-CUR = ZERO)
               IF W40-CUR > DEB-CUR
                   PERFORM FA60I
                   GO TO FA60D.
             SUBTRACT W40-CUR        FROM W25-VALUE.
             PERFORM FA60B.
           IF W25-VALUE = ZERO
               GO TO FA60J.

       FA60E.
             ACCEPT S31 AT 1434.
           IF W40-30 > W25-VALUE
               PERFORM FA60H
               GO TO FA60E.
           IF W40-30 > DEB-30
               PERFORM FA60I
               GO TO FA60E.
             SUBTRACT W40-30         FROM W25-VALUE.
             PERFORM FA60B.
           IF W25-VALUE = ZERO
              GO TO FA60J.

       FA60F.
             ACCEPT S32 AT 1534.
           IF W40-60 > W25-VALUE
               PERFORM FA60H
               GO TO FA60F.
           IF W40-60 > DEB-60
               PERFORM FA60I
               GO TO FA60F.
             SUBTRACT W40-60         FROM W25-VALUE.
             PERFORM FA60B.
           IF W25-VALUE = ZERO
              GO TO FA60J.

       FA60G.
             ACCEPT S33 AT 1634.
           IF W40-90 > W25-VALUE
               PERFORM FA60H
               GO TO FA60G.
           IF W40-90 > DEB-90
               PERFORM FA60I
               GO TO FA60G.
             SUBTRACT W40-90         FROM W25-VALUE.
             PERFORM FA60B.
           IF W25-VALUE = ZERO
              GO TO FA60J.

       FA60GA.
             ACCEPT S34 AT 1734.
           IF W40-120 > W25-VALUE
               PERFORM FA60H
               GO TO FA60GA.
           IF W40-120 > DEB-120
               PERFORM FA60I
               GO TO FA60G.
             SUBTRACT W40-120        FROM W25-VALUE.
             PERFORM FA60B.
           IF W25-VALUE = ZERO
              GO TO FA60J.
           IF W25-VALUE NOT > DEB-INT
               MOVE W25-VALUE        TO W40-INT
           ELSE
               MOVE DEB-INT          TO W40-INT.
             MOVE W40-INT            TO W95-B.
             DISPLAY W95-V2 AT 1834 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             SUBTRACT W40-INT        FROM W25-VALUE.
             PERFORM FA60B.
           IF W25-VALUE = ZERO
              GO TO FA60J.
             MOVE "Total not allocated - 'R'edo,'A'uto-allocation" TO WS-ERR-MES.
             MOVE SPACE              TO WS-OPTION.
             PERFORM ERROR-LENGTH THRU ERROR-EXIT.
           IF WS-OPTION = "A"
               PERFORM DEBTOR-CREDITS THRU DEBTOR-TRAN-EXIT
               GO TO FA61.
             MOVE ZERO               TO W40-CUR W40-30 W40-60 W40-90 W40-120 W40-INT.
             MOVE W90-CR             TO W25-VALUE.
             GO TO FA60AB.

       FA60H.
             MOVE "Value allocated > Receipt value" TO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.

       FA60I.
             MOVE "Value allocated > Analysis value" TO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.

       FA60J.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               MOVE W90-CR           TO W25-VALUE
               MOVE ZERO             TO W40-CUR W40-30 W40-60 W40-INT W40-90 W40-120
               GO TO FA60AB.
             SUBTRACT W40-30         FROM DEB-30.
             SUBTRACT W40-60         FROM DEB-60.
             SUBTRACT W40-90         FROM DEB-90.
             SUBTRACT W40-120        FROM DEB-120.
             SUBTRACT W40-INT        FROM DEB-INT.
             SUBTRACT W90-CR         FROM DEB-BAL.
             ADD W40-30              TO W90-30.
             ADD W40-60              TO W90-60.
             ADD W40-90              TO W90-90.
             ADD W40-120             TO W90-120.
             ADD W40-INT             TO W90-INT.
           IF W40-CUR < DEB-CUR
               SUBTRACT W40-CUR      FROM DEB-CUR
               ADD W40-CUR           TO W90-CUR
           ELSE
               ADD DEB-CUR           TO W90-CUR
               MOVE ZERO             TO DEB-CUR.
             GO TO FA61.

       FA60K.
             MOVE 1234               TO CPOS.

       FA60KL.
            DISPLAY "             " AT CPOS WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
           IF CLIN < 18
               ADD 1                 TO CLIN
               GO TO FA60KL.
             DISPLAY "Transaction balance:" AT 2012 WITH FOREGROUND-COLOR Cyan.

       FA60L.
             MOVE W25-VALUE          TO W95-B.
             DISPLAY W95-V2 AT 2034 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             PERFORM CLEAR-L50.

       FA60M.
             ACCEPT S35 AT 1234.
           IF W40-INT > W25-VALUE
               PERFORM FA60H
               GO TO FA60M.
             SUBTRACT W40-INT        FROM W25-VALUE.
             PERFORM FA60L.
           IF W25-VALUE = ZERO
               GO TO FA60R.

       FA60N.
             ACCEPT S30 AT 1334.
           IF W40-CUR > W25-VALUE
               PERFORM FA60H
               GO TO FA60N.
             SUBTRACT W40-CUR        FROM W25-VALUE.
             PERFORM FA60L.
           IF W25-VALUE = ZERO
              GO TO FA60R.

       FA60O.
             ACCEPT S31 AT 1434.
           IF W40-30 > W25-VALUE
               PERFORM FA60H
               GO TO FA60O.
             SUBTRACT W40-30         FROM W25-VALUE.
             PERFORM FA60L.
           IF W25-VALUE = ZERO
               GO TO FA60R.

       FA60P.
             ACCEPT S32 AT 1534.
           IF W40-60 > W25-VALUE
               PERFORM FA60H
               GO TO FA60P.
             SUBTRACT W40-60         FROM W25-VALUE.
             PERFORM FA60L.
           IF W25-VALUE = ZERO
               GO TO FA60R.

       FA60Q.
             ACCEPT S33 AT 1634.
           IF W40-90 > W25-VALUE
               PERFORM FA60H
               GO TO FA60Q.
             SUBTRACT W40-90         FROM W25-VALUE.
             PERFORM FA60L.
           IF W25-VALUE = ZERO
               GO TO FA60R.
           IF W25-VALUE = ZERO
               GO TO FA60R.

       FA60QA.
             ACCEPT S34 AT 1734.
           IF W40-120 > W25-VALUE
               PERFORM FA60H
               GO TO FA60QA.
             SUBTRACT W40-120        FROM W25-VALUE.
             PERFORM FA60L.
           IF W25-VALUE = ZERO
               GO TO FA60R.
             MOVE "Total must be allocated" TO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.
             MOVE ZERO               TO W40-CUR W40-30 W40-90 W40-INT W40-60 W40-120.
             MOVE W90-DB             TO W25-VALUE.
             GO TO FA60K.

       FA60R.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               MOVE W90-DB           TO W25-VALUE
               MOVE ZERO             TO W40-CUR W40-30 W40-90 W40-INT W40-60 W40-120
               GO TO FA60K.
             ADD W40-CUR             TO DEB-CUR.
             ADD W40-30              TO DEB-30.
             ADD W40-60              TO DEB-60.
             ADD W40-90              TO DEB-90.
             ADD W40-120             TO DEB-120.
             SUBTRACT W40-CUR        FROM W90-CUR.
             SUBTRACT W40-30         FROM W90-30.
             SUBTRACT W40-60         FROM W90-60.
             SUBTRACT W40-90         FROM W90-90.
             SUBTRACT W40-120        FROM W90-120.

       FA61.
             MOVE DEB-ACNO           TO R-TAC.
             MOVE W40-DSP-NAME       TO R-DNME.
             MOVE W90-DATE           TO W90-DTE.
             MOVE W90-DTE            TO R-TDTE.
             MOVE W90-DESC           TO R-TDESC.
             MOVE W90-REF            TO R-TREF.
             MOVE W90-VAL            TO R-VAL.
             MOVE W90-VAT(1)         TO R-TAX.
           IF W90-DB > 0
               MOVE W90-DB           TO R-TOT
           ELSE
               MULTIPLY -1 BY W90-CR GIVING R-TOT.
          IF LINAGE-COUNTER < (WS-PRN-LENGTH - 2)
               WRITE R-L8 BEFORE 1 LINES
           ELSE
               WRITE R-L8 BEFORE PAGE
               PERFORM FA10 THRU FA15.
             MOVE SPACES             TO R-DET1.
           IF NOT(W90-CR = ZERO)
               IF OPEN-ITEM
                   PERFORM FA80 THRU FA90
                   CLOSE DEBTRN.
             PERFORM REWRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.

       FA62.
             MOVE 0701               TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
             MOVE W90-DEBIT          TO W100-BAL.
             MOVE W90-CREDIT         TO W100-CUR.
             DISPLAY "Debit total:" AT 2112.
             DISPLAY W100-BAL AT 2125 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY "Credit total:" AT 2139.
             DISPLAY W100-CUR AT 2153 WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Cyan HIGHLIGHT.
             PERFORM FA070-NEW THRU FA70B.
             PERFORM FA06.
             GO TO FA22.

       FA070-NEW.
             SUBTRACT W90-BCR FROM W90-BDT GIVING W25-VALUE.
             ADD W25-VALUE           TO   DEB-OUT.
             ADD W90-BDT             TO   DEB-DT.
             ADD W90-BCR             TO   DEB-CR.
             SUBTRACT W90-120        FROM DEB-FOUR.
             SUBTRACT W90-90         FROM DEB-THREE.
             SUBTRACT W90-60         FROM DEB-TWO.
             SUBTRACT W90-30         FROM DEB-ONE.
             SUBTRACT W90-CUR        FROM DEB-MTD.
             SUBTRACT W90-INT        FROM DEB-INTEREST.

       FA70A.
           IF WS-TAX = "N"
               GO TO FA70B.
             MOVE 9                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             PERFORM AY01 THRU AY59.
             MOVE "VAT1"             TO DPT-CODE.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
             PERFORM AY38 THRU AY59.
           IF NOT(W90-VAL = ZERO)
               IF W90-TX = ZERO
                   IF W90-DB = ZERO
                       ADD W90-VAL   TO PAR-CNGS-DAY PAR-CNGS-MTD PAR-CNGS-YTD
                   ELSE
                       ADD W90-VAL   TO PAR-DNGS-DAY PAR-DNGS-MTD PAR-DNGS-YTD
               ELSE
               IF W90-DB = ZERO
                   ADD W90-VAL       TO PAR-CVAT-DAY   PAR-CVAT-MTD   PAR-CVAT-YTD
                                        DPT-V-RET-DAY  DPT-V-RET-MTD  DPT-V-RET-YTD
                   ADD W90-TX        TO DPT-O-VRET-DAY DPT-O-VRET-MTD DPT-O-VRET-YTD
                   IF WX-INT = "Y"
                       COMPUTE WX-VALUE = W90-TX * -1
                       MOVE DPT-O-VAT-GL TO WX-AC
                       MOVE 11           TO WX-TYPE
                       PERFORM GL-TRF-UPDATE
                   END-IF
               ELSE
                   ADD W90-VAL       TO PAR-DVAT-DAY    PAR-DVAT-MTD    PAR-DVAT-YTD
                                        DPT-V-SALES-DAY DPT-V-SALES-MTD DPT-V-SALES-YTD
                   ADD W90-TX        TO DPT-O-VAT-DAY   DPT-O-VAT-MTD   DPT-O-VAT-YTD
                   IF WX-INT = "Y"
                       COMPUTE WX-VALUE = W90-TX * -1
                       MOVE DPT-O-VAT-GL TO WX-AC
                       MOVE 10       TO WX-TYPE
                       PERFORM GL-TRF-UPDATE.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.

       FA70B.
           IF W00-T-CODE = 19
               MOVE "XXXX"           TO DPT-CODE
               PERFORM READ-DEPART THRU READ-DEPART-EXIT
               PERFORM AY38 THRU AY59
               ADD W90-VAL           TO DPT-SALES-DAY DPT-SALES-MTD DPT-SALES-YTD
               PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
             PERFORM AY70 THRU AY999.

       FA72.
             WRITE R-L8 BEFORE 1 LINES.
             MOVE "Total"            TO R-TDESC.
             MOVE W90-VATTOT         TO R-TAX.
             COMPUTE R-TOT = W90-DEBIT - W90-CREDIT.
             WRITE R-L8 BEFORE PAGE.

       FA75.
             MOVE 0701               TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
             PERFORM CLEAR-L50.
             CLOSE DRBTCH.
             OPEN OUTPUT DRBTCH.
             CLOSE DRBTCH.
             OPEN I-O DRBTCH.
             GO TO FA999.

       FA80.
           IF W75-KEY < WS-DBTKEY
               GO TO FA90.
             PERFORM READ-DEBTRN THRU READ-DEBTRN-EXIT.
           IF DBT-UPDATE = SPACE
               GO TO FA85.
             MOVE DBT-TRA            TO TRA-RECORD1.
             MOVE 0                  TO WS-ACTION.
             PERFORM AY04 THRU AY59.
             ADD DBT-ALLOC           TO DBT-PAID.
           IF DBT-VALUE > DBT-PAID
               MOVE "X"              TO DBT-TYPE
           ELSE
               MOVE "P"              TO DBT-TYPE.
           IF DBT-CODE = 17
               SUBTRACT DBT-ALLOC    FROM DEB-INT
               ADD DBT-ALLOC         TO W90-INT
           ELSE
           IF DBT-AGE = 5
               SUBTRACT DBT-ALLOC    FROM DEB-120
               ADD DBT-ALLOC         TO W90-120
           ELSE
           IF DBT-AGE = 4
               SUBTRACT DBT-ALLOC    FROM DEB-90
               ADD DBT-ALLOC         TO W90-90
           ELSE
           IF DBT-AGE = 3
               SUBTRACT DBT-ALLOC    FROM DEB-60
               ADD DBT-ALLOC         TO W90-60
           ELSE
           IF DBT-AGE = 2
               SUBTRACT DBT-ALLOC    FROM DEB-30
               ADD DBT-ALLOC         TO W90-30
           ELSE
           IF DBT-AGE = 1
               SUBTRACT DBT-ALLOC    FROM DEB-CUR
               ADD DBT-ALLOC         TO W90-CUR.
             MOVE DBT-TRA            TO TRA-RECORD1.
             PERFORM REWRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.

       FA85.
             ADD 1                   TO WS-DBTKEY.
             GO TO FA80.

       FA90.
             EXIT.

       FA999.
             EXIT.
      *
      *    ****    C A L L   D E B T O R   C R E A T I O N
      *
       GA000        SECTION 54.
       GA00.
             PERFORM SAVE-SCREEN.
             CLOSE RECOVER.
             CALL "DTP\DTP001" USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
             CANCEL "DTP\DTP001".
             OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
             MOVE ZERO               TO WS-RECOVER.
             PERFORM RESTORE-SCREEN.

       GA99.
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

           IF LS-USER NUMERIC
               IF LS-USE > ZERO AND < 110
                   MOVE LS-USE       TO WS-USUB
               END-IF
           ELSE        
               MOVE 110              TO WS-USUB.
             ADD 2549 WS-USUB        GIVING W25-RESULT.
             MOVE W25-KEY            TO WS-PARKEY.
             ADD 1 W25-SUB           GIVING WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE TRM-STDP(WS-S6)    TO WS-PRINTER-NO.
             ADD 501 TRM-PRN3(WS-S6) GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2  GIVING WS-PARKEY REMAINDER WS-S6.
             ADD 1                   TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE TRM-CONP(WS-S6)    TO WS-COND.
             MOVE TRM-NORP(WS-S6)    TO WS-NORM.
             MOVE TRM-EXPP(WS-S6)    TO WS-EXPP W08-EXPD.
             MOVE TRM-ECAN(WS-S6)    TO WS-ECAN.
             MOVE TRM-8LPI(WS-S6)    TO WS-8LPI.
             MOVE TRM-6LPI(WS-S6)    TO WS-6LPI.
             MOVE TRM-10CPI(WS-S6)   TO WS-10CPI.
             MOVE TRM-12CPI(WS-S6)   TO WS-12CPI.
             MOVE TRM-17CPI(WS-S6)   TO WS-17CPI.
             MOVE TRM-LENGTH(WS-S6)  TO WS-PRN-LENGTH.
             MOVE TRM-PAGE(WS-S6)    TO WS-PGE-LENGTH.
           IF TRM-PRN-TYPE(WS-S6) = "H"
               MOVE WS-12CPI         TO WS-ECAN
               MOVE 048              TO W08-DEC(4)
               MOVE 054              TO W08-DEC(5)
               MOVE W08-EXPD         TO WS-EXPP.
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
             OPEN I-O DRBTCH.
           IF WS-STATUS = "05"
               CLOSE DRBTCH OPEN OUTPUT DRBTCH
               CLOSE DRBTCH OPEN I-O DRBTCH.
       ZA03.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               IF IDX-CORRUPT
                   CLOSE DRBTCH OPEN OUTPUT DRBTCH
                   CLOSE DRBTCH OPEN I-O DRBTCH
                   GO TO ZA03.
           IF WS-STATUS NOT = "00"
               MOVE 42               TO WS-F-ERROR
               PERFORM OPEN-ERROR.
             DISPLAY "Waiting for printer response" AT 1412 WITH FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
             OPEN OUTPUT PRNREP.
             CALL X"91" USING X91-RES X91-FUN PRNREP.
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
             GO TO ZA999-EXIT.

       COPY "ZA49.PRO".

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
               WHEN 2          MOVE W02-NETWORK   TO WS-FILE
                               MOVE WS-NETKEY     TO WS-KEY
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
               WHEN 37         MOVE W02-SHARED    TO WS-FILE
                               MOVE WS-SHARED     TO WS-KEY
               WHEN 40         MOVE W02-LEDTRF    TO WS-FILE
                               MOVE XFR-KEY       TO WS-KEYX
               WHEN 42         MOVE W02-DRBTCH    TO WS-FILE
                               MOVE DRB-KEY       TO WS-KEYX
               WHEN OTHER      MOVE "ERROR"       TO WS-FILE
                               MOVE "DTP014"      TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
