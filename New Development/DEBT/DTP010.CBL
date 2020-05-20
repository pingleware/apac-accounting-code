      $SET LINKCOUNT"468" GNT"DTP010.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****       **      ****      *
      *   **    **     **     **    **   **  **     ***     **  **     *
      *   **    **     **     **    **  **    **     **    **    **    *
      *   **    **     **     *******   **    **     **    **    **    *
      *   **    **     **     **        **    **     **    **    **    *
      *   **    **     **     **         **  **      **     **  **     *
      *   *******      **     **          ****     ******    ****      *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *            A C C O U N T S   R E C E I V A B L E   /           *
      *                                                                *
      *   D E B T O R   T R A N S A C T I O N S   U S I N G   C O D E  *
      *                                                                *
      *       Version 9.04.02 - August 2017                            *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  May 2006 - New field included in Accounts Receivable for      *
      *             recording number of outstanding Jobs linked to the *
      *             account. Additional filler added for future        *
      *             expansion.                                         *
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
       PROGRAM-ID.     DTP010.
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

       COPY "DEBTRN.SL".

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

       COPY "DEBTRN.FD".

       COPY "DEPART.FDE".

       COPY "LEDTRF.FDE".

       COPY "PARAM.FDE".

       COPY "RECOVER.FD".

       COPY "SHARED.FDE".

       COPY "TXTRAN.FDE".

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
       77  WS-CHECK                PIC  X(18)         VALUE "aeWlimemnomLalismJ".
       77  WS-SUB                  PIC  9(04)  COMP-5.
       77  WS-S                    PIC  9(04)  COMP-5.
       77  WS-S1                   PIC  9(04)  COMP-5.
       77  WS-S2                   PIC  9(04)  COMP-5.
       77  WS-S3                   PIC  9(04)  COMP-5.
       77  WS-S4                   PIC  9(04)  COMP-5.
       77  WS-S5                   PIC  9(04)  COMP-5.
       77  WS-S6                   PIC  9(04)  COMP-5.
       77  WS-S7                   PIC  9(04)  COMP-5.
       77  WS-S8                   PIC  9(04)  COMP-5.
       77  WS-S9                   PIC  9(04)  COMP-5.
       77  WS-IXS1                 PIC  9(04)  COMP-5.
       77  WS-IXS2                 PIC  9(04)  COMP-5.
       77  WS-IXS3                 PIC  9(04)  COMP-5.
       77  WS-IXS4                 PIC  9(04)  COMP-5.
       77  WS-ENQPOS               PIC  9(06)  COMP-5.
       77  WS-ENQEND               PIC  9(06)  COMP-5.
       77  WS-DBTKEY               PIC  9(06)  COMP-5.
       77  WS-NETKEY               PIC  9(06)  COMP-5.
       77  WS-PARKEY               PIC  9(06)  COMP-5.
       77  WS-RECKEY               PIC  9(06)  COMP-5.
       77  WS-RECOVER              PIC  9(06)  COMP-5.
       77  WS-SHARED               PIC  9(04)  COMP-5 VALUE 1.
       77  WS-TRANS                PIC  9(04)  COMP-5 VALUE 1.
       77  WS-PERIOD               PIC  9(02).
       77  WS-NTP                  PIC  X(01).
       77  WS-MTHEND               PIC  9(02)  COMP-5.
       77  WS-PAGE                 PIC  9(04)  COMP-5.
      *77  WS-PGE-LENGTH           PIC  9(02)  VALUE 66.
      *77  WS-PRN-LENGTH           PIC  9(02)  VALUE 62.
       77  WS-KEYSTR               PIC  9(06)  COMP-5.
       77  WS-INSTR                PIC  X(01).
       77  WS-OPTION               PIC  X(01).
       77  WS-ALLOCATE             PIC  X(01)         VALUE "M".
       77  WS-SKIP                 PIC  X(01)         VALUE "N".
       77  WS-TAX                  PIC  X(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-ERROR                PIC  9(01)         VALUE ZERO.
       77  WS-EKEY                 PIC  9(06)  COMP-5.
       77  WS-CHAR1                PIC  X(01).
       77  WS-CHAR2                PIC  X(01).
       77  WS-COMP                 PIC  9(01).
       77  WS-BATCH                PIC  9(06)  COMP-5.
       77  WS-VAT-CODE             PIC  X(04).
       77  WS-VAT-VALUE            PIC S9(09)V99 COMP-3.
       77  WS-ERR1                 PIC  X(22)         VALUE "Invalid Account Number".
       77  WS-ERR2                 PIC  X(09)         VALUE "No Record".
       77  WS-E                    PIC  X(15)         VALUE "**END** - ENTER".
       77  WS-AC                   PIC  X(07)         VALUE "ACCOUNT".
       77  PRG-NAME                PIC  X(12)         VALUE SPACES.
       77  PRG-DEBT-LUP            PIC  X(12)         VALUE "DTP\DTPLOOK".
       77  TODAY-DDMMYY            PIC  9(08)  COMP-5.
       77  WS-USUB                 PIC  9(04)  COMP-5.
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
           03  CRT-START           PIC  9(02).
           03  CRT-END             PIC  9(02).
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15)    VALUE "DTP010".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40)    VALUE "ACCOUNTS RECEIVABLE - SUNDRY TRANSACTION".
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
           03  WS-MODULE           PIC  X(03)    VALUE "DTP".
           03  WS-PROG             PIC  X(03)    VALUE "010".

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
           03  R-L2                              REDEFINES W02-PRN-LINE.
               05  R-DET2          PIC  X(80).
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
           03  R-L4                              REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(11).
               05  R-H1            PIC  X(04).
               05  FILLER          PIC  X(06).
               05  R-H2            PIC  X(15).
               05  FILLER          PIC  X(06).
               05  R-H3            PIC  X(15).
               05  FILLER          PIC  X(06).
               05  R-H4            PIC  X(06).
               05  FILLER          PIC  X(11).
           03  R-L7                              REDEFINES W02-PRN-LINE.
               05  R-H7            PIC  X(07).
               05  R-H7A           PIC  X(27).
               05  R-H8            PIC  X(07).
               05  R-H10           PIC  X(09).
               05  R-H11           PIC  X(15).
               05  R-H12           PIC  X(14).
               05  R-H13           PIC  X(11).
               05  R-H14           PIC  X(06).
           03  R-L8                              REDEFINES W02-PRN-LINE.
               05  R-TAC           PIC  X(06).
               05  FILLER          PIC  X(01).
               05  R-DNME          PIC  X(23).
               05  FILLER          PIC  X(01).
               05  R-TDTE          PIC  X(10).
               05  FILLER          PIC  X(01).
               05  R-TDESC         PIC  X(07).
               05  FILLER          PIC  X(01).
               05  R-TREF          PIC  X(08).
               05  FILLER          PIC  X(01).
               05  R-VAL           PIC  Z(07)9.99-.
               05  FILLER          PIC  X(01).
               05  R-TAX           PIC  Z(06)9.99-.
               05  FILLER          PIC  X(01).
               05  R-TOT           PIC  Z(07)9.99-.
           03  R-L9                              REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(54).
               05  R-BTOT          PIC  Z(06)9.99-.
               05  FILLER          PIC  X(15).

       COPY "W05.VAT".

       01  W05-DISC.
           03  W05-DISCOUNT.
               05  W05-DSETT         PIC S9(03)V99 COMP-3 OCCURS 10.

      *01  W08-EXPD.
      *    03  W08-CHAR              PIC  X(06).
      *    03  W08-RCHAR                           REDEFINES W08-CHAR.
      *        05 W08-DEC            PIC  X(01)    COMP-X OCCURS 6.

       01  W10-SPARES.
           03  W10-DATE              PIC X(10).
           03  W10-RDATE                           REDEFINES W10-DATE.
               05  W10-EDTE          PIC 99/99/9999.

       01  W10-REF.
           03  W10-NUM               PIC  9(08).
           03  W10-REF1                            REDEFINES W10-NUM.
               05  FILLER            PIC  X(07).
               05  W10-N1.
                   07  W10-NUM1      PIC  9(01).
           03  W10-REF2                            REDEFINES W10-NUM.
               05  FILLER            PIC  X(06).
               05  W10-N2.
                   07  W10-NUM2      PIC  9(02).
           03  W10-REF3                            REDEFINES W10-NUM.
               05  FILLER            PIC  X(05).
               05  W10-N3.
                   07  W10-NUM3      PIC  9(03).

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
           03  W25-RESULT            PIC  9(05)V99.
           03  W25-RESULT1                         REDEFINES W25-RESULT.
               05  W25-WHOLE         PIC  9(05).
               05  W25-DECIMAL       PIC  9(02).
           03  W25-RESULT2                         REDEFINES W25-RESULT.
               05  W25-KEY           PIC  9(04).
               05  W25-SUB           PIC  9(01).
               05  FILLER            PIC  9(02).
           03  W25-TOTAL             PIC S9(09)V99 COMP-3.
           03  W25-VALUE             PIC S9(09)V99 COMP-3.

       01  W30-AGE-DATES.
           03  W30-120               PIC  9(08)    COMP-3.
           03  W30-90                PIC  9(08)    COMP-3.
           03  W30-60                PIC  9(08)    COMP-3.
           03  W30-30                PIC  9(08)    COMP-3.
           03  W30-CUR               PIC  9(08)    COMP-3.

       COPY "W40.DBT".

       COPY "W40.WS".

       01  W45-TRAN.
           03  W45-CODE              PIC  9(02).
           03  W45-ENG               PIC  X(12).
           03  W45-AFR               PIC  X(12).
           03  W45-ACT               PIC  X(01).

       COPY "FUNCTION.WS".

      *COPY "W50.WS".

       01  W75-KEYS.
           03  W75-S                 PIC  9(02)    COMP-5.
           03  W75-S1                PIC  9(02)    COMP-5.
           03  W75-KEY               PIC  9(06)    COMP-5.
           03  W75-ALLOCATED         PIC S9(09)V99 COMP-3.
           03  W75-BALANCE           PIC S9(09)V99 COMP-3.
           03  W75-DNO               PIC  X(06)    OCCURS 18.
           03  W75-CODE              PIC  Z9.

       01  W80-ACCOUNT.
           03  W80-ACNO              PIC  X(06).
      *    03  W80-TYPE              PIC  9(01).

       01  W85-PASS.
           03  W85-SUPER             PIC  X(06)    OCCURS 9.
           03  W85-ENTRY             PIC  9(02)    COMP-3.
           03  W85-MARG              PIC S9(02)V99.

       01  W85-BATCHES.
           03  W85-BATCH             PIC  9(06)    COMP-5.

       COPY "W90.DBT".

       01  W95-STM.
           03  W95-COMP              PIC  X(40).
           03  W95-V2.
               05  W95-B             PIC  Z(06)9.99-.

       01  W100-EDIT.
           03  W100-LBAL             PIC  Z(06)9.99-.
           03  W100-LARR             PIC  Z(06)9.99.
           03  W100-BAL              PIC  Z(08)9.99-.
           03  W100-CUR              PIC  Z(08)9.99-.
           03  W100-30               PIC  Z(08)9.99-.
           03  W100-60               PIC  Z(08)9.99-.
           03  W100-90               PIC  Z(08)9.99-.
           03  W100-120              PIC  Z(08)9.99-.
           03  W100-INT              PIC  Z(08)9.99B.
           03  W100-CRL              PIC  Z(06)9.
           03  W100-KEY              PIC  Z(05)9.

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

       COPY "TRNLUP.CRT".

       01  S08A.
           03  LINE 16 COLUMN  6 VALUE "Allocation:".
           03  LINE 17 COLUMN  6 VALUE "Ref. No.".
           03          COLUMN 18 VALUE "Date".
           03          COLUMN 26 VALUE "Transaction".
           03          COLUMN 46 VALUE "Value".
           03          COLUMN 57 VALUE "Balance".
           03          COLUMN 69 VALUE "Allocate".

       01  S12.
           03  LINE 11 COLUMN 12 VALUE "Name :".
           03  LINE 13 COLUMN 12 VALUE "Bal  :".
           03  LINE 14 COLUMN 12 VALUE "Mtd  :".
           03  LINE 15 COLUMN 12 VALUE "Cur  :".
           03  LINE 16 COLUMN 12 VALUE "30d  :".
           03  LINE 17 COLUMN 12 VALUE "60d  :".
           03  LINE 18 COLUMN 12 VALUE "90d  :".
           03  LINE 19 COLUMN 12 VALUE "120d :".
           03  LINE 20 COLUMN 12 VALUE "Int  :".
           03  LINE 21 COLUMN 12 VALUE "Limit:".

       01  S26.
           03  LINE 6 COLUMN  2  VALUE "Batch:".
           03  LINE 7 COLUMN  2  VALUE "Account".
           03         COLUMN 12  VALUE "Date".
           03         COLUMN 20  VALUE "Cde/description".
           03         COLUMN 38  VALUE "Ref".
           03         COLUMN 51  VALUE "Value".
           03         COLUMN 64  VALUE "TAX".
           03         COLUMN 75  VALUE "Total".

       01 S26-DATA.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.
      *       05  S26-DTE.
      *           07  LINE  9 COLUMN  9 PIC 99/99/9999 USING W90-DATE AUTO.

              05  S26-DT.
                  07  LINE  9 COLUMN 68 PIC Z(08)9.99 USING W90-DB AUTO.

              05  S26-CR.
                  07  LINE  9 COLUMN 68 PIC Z(08)9.99 USING W90-CR AUTO.

              05  S26-VAL.
                  07  LINE  9 COLUMN 45 PIC Z(07)9.99 USING W90-VAL AUTO.

              05  S26-VAT.
                  07  LINE  9 COLUMN 57 PIC Z(06)9.99 USING WS-VAT-VALUE AUTO.

       01  S29-S36.
           03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.
               05  S29.
                   07  PIC Z(08)9.99- USING W40-MTD AUTO.
               05  S30.
                   07  PIC Z(08)9.99- USING W40-CURW AUTO.
               05  S31.
                   07  PIC Z(08)9.99- USING W40-30W AUTO.
               05  S32.
                   07  PIC Z(08)9.99- USING W40-60W AUTO.
               05  S33.
                   07  PIC Z(08)9.99- USING W40-90W AUTO.
               05  S34.
                   07  PIC Z(08)9.99- USING W40-120W AUTO.
               05  S35.
                   07  PIC Z(08)9.99- USING W40-INT AUTO.
               05  S36.
                   07  PIC X(06)      USING W80-ACNO AUTO.

       01  S40.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
               05  LINE 50 COLUMN  2 VALUE "F1"                                                                      FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN  4 VALUE   "=Help,".
               05          COLUMN 10 VALUE         "F2"                                                              FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 12 VALUE           "=Debtor Lookup,".
               05          COLUMN 27 VALUE                          "F8"                                             FOREGROUND-COLOR Red   HIGHLIGHT.
               05          COLUMN 29 VALUE                            "=Create,".
               05          COLUMN 37 VALUE                                    "F9"                                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 39 VALUE                                      "=Debtor Lookup (Any Word),".
               05          COLUMN 65 VALUE                                                                "Esc"      FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 68 VALUE                                                                   "=Exit".

       01  S45.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
               05  LINE 11 COLUMN 20 PIC  X(40)      FROM W40-DSP-NAME.
               05  LINE 13 COLUMN 20 PIC  Z(08)9.99- FROM DEB-BAL.
               05  LINE 14 COLUMN 20 PIC  Z(08)9.99- FROM W40-ONE.
               05  LINE 15 COLUMN 20 PIC  Z(08)9.99- FROM DEB-CUR.
               05  LINE 16 COLUMN 20 PIC  Z(08)9.99- FROM DEB-30.
               05  LINE 17 COLUMN 20 PIC  Z(08)9.99- FROM DEB-60.
               05  LINE 18 COLUMN 20 PIC  Z(08)9.99- FROM DEB-90.
               05  LINE 19 COLUMN 20 PIC  Z(08)9.99- FROM DEB-120.
               05  LINE 20 COLUMN 20 PIC  Z(08)9.99B FROM DEB-INT.
               05  LINE 21 COLUMN 20 VALUE "  "                   FOREGROUND-COLOR Cyan.
               05          COLUMN 22 PIC  Z(06)9     FROM DEB-CRL.         
               05          COLUMN 27 VALUE      "    "            FOREGROUND-COLOR Cyan.

       01  S50.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
               05  LINE 50 COLUMN  2 VALUE "F9"                                     FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN  4 VALUE   " Memo/Rmks,".
               05          COLUMN 15 VALUE              "Any"                       FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 18 VALUE                 " other key to continue".

       01  S55.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
               05  LINE 50 COLUMN  2 VALUE "Press ".
               05          COLUMN  8 VALUE       "Esc"           FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 11 VALUE          " to CANCEL".

       01  S60.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
               05  LINE 50 COLUMN  2 VALUE "F6"                                          FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN  4 VALUE   "=View Transaction Codes, ".
               05          COLUMN 29 VALUE                            "Esc"              FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 32 VALUE                               "ape to Cancel".

       01  S-ALLOC.
           03  LINE 21 COLUMN 14 VALUE "Un-allocated balance : ".
           03          COLUMN 37 PIC Z(08)9.99 FROM W75-BALANCE FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Magenta.
           03          COLUMN 51 VALUE "Allocated : ".
           03          COLUMN 63 PIC Z(08)9.99 FROM W75-ALLOCATED FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Magenta.

       01  S-DTRN.
           03  FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
               05  COLUMN  6 PIC X(08)      FROM DBT-REF.
               05  COLUMN 15 PIC 9999/99/99 FROM DBT-DATE.
               05  COLUMN 26 PIC X(12)      FROM W00-E-DESC.
               05  COLUMN 39 PIC Z(08)9.99  FROM DBT-VALUE.
               05  COLUMN 52 PIC Z(08)9.99  FROM DBT-OUTSTD.
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
      * *   * *   * *     **  *   *   * *   *  *  **  *   *   *     *   *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *     *  **   *     *   *  *  *  **   *   *     *   *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** *   *
      *
             PERFORM OPEN-PRINTER.
             MOVE "P"                TO WS-COMMAND.
             PERFORM CG000.
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

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                       DEBTOR-MEMO                         ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ The account number of the Debtor record for which the MEMO³
      *    ³ details are to be displayed must be in W80-ACNO.          ³
      *    ³ PERFORM DEBTOR-MEMO.                                      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "DEBMEM.PRO".

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
      *    ³ The option request must be placed in WS-ERR-MES and may   ³
      *    ³ not exceed 48 characters. The message will be centred in  ³
      *    ³ the window. An example of a message request follows:      ³
      *    ³                                                           ³
      *    ³   MOVE "Print transactions (Y/N) [ ]" TO WS-ERR-MES.      ³
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
      *    ³ To clear lines 46 and 50:                                 ³
      *    ³    PERFORM CLEAR-L46-AND-50                               ³
      *    ³                                                           ³
      *    ³ To clear line 50:                                         ³
      *    ³    PERFORM CLEAR-L50                                      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "CLEAR.CRT".

       COPY "DATE.CHK".

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

       COPY "DBTRAN.LUP".

       COPY "DEBTOR.TRA".

       COPY "LEDTRF.UPD".

       COPY "DEBTOR.ALL".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                                                           ³
      *    ³           R E A D   F I L E S   R O U T I N E S           ³
      *    ³                                                           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AC000              SECTION.

       COPY "CONTROL.RD".

       COPY "DBTRAN.RD1".

       COPY "DEBTOR.RD".

       COPY "DEBTRN.RD".

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

       COPY "DEBTRN.WR".

       COPY "DEPART.WR".

       COPY "LEDTRF.WR".

       COPY "PARAM.WR".

       COPY "TXTRAN.WR".

       COPY "APAC.HLP".

      *
      *            ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *                PRINTER SUCH AS OPEN AND PRINT
      *
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³             PERFORM OPEN-PRINT-FILE                       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *
      *         PRINT A LINE, ADVANCE LINE/PAGE, CHANGE STYLE ETC
      *     The following variables need to be set before calling this
      *     routine.
      *     
      *    COMMAND: C = Close printer
      *             E = End of report
      *             O = Open printer
      *             P = Print a line
      *             S = Skip to new page
      *
      *       WS-COMMAND      PIC  X(01).
      *
      *    PRINTER: D = Disk
      *             S = Screen
      *             1 = Lpt1
      *             2 = Lpt2
      *             3 = Lpt3
      *             4 = Com1
      *             5 = Com2
      *             8 = USB/WINDOWS
      *
      *       WS-PRINTER.
      *         WS-PRINT-NO PIC  9(01).
      *
      *    ADVANCE:   0 = Print - No advance
      *             1-3 = Print advance lines specified
      *             4-? = Print advance 1 line, then use a loop
      *                   to advance the remaining lines.
      *              99 = Print and advance to a new page.
      *
      *       WS-ADVANCE      PIC  9(02).
      *
      *    REPORT: 1 = Picking Slips
      *            2 = Invoices/Credit Notes
      *            3 = General Reports (Stock)
      *            4 = General Reports (Creditors)
      *            5 = General Reports (Debtors)
      *            6 = General Reports (G/Ledger)
      *            7 = Statements
      *            8 = Labels
      *            9 = Quotations
      *
      *      WS-REPORT           PIC  9(01).
      *
      *      See W02-PRN-TYPE for details of Print style.
      *
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³               PERFORM CALL-PRINTUTL                       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´

       COPY "PRINTUTL.AS5".

      *    *************************************************************
      *          THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *    *************************************************************
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
      *    *************************************************************
      *    ****       V A L U E   A D D E D   T A X   F I L E       ****
      *    *************************************************************
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
             MOVE ZERO               TO REC-TYPE.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
           IF WS-F-ERROR = 7
               MOVE "XXXX"           TO DPT-CODE
               GO TO AY38.
             MOVE DPT-RECORD         TO REC-DEPART.
             GO TO AY50.
             
       AY39.
             MOVE 39                 TO REC-FILE.
             MOVE WS-NETKEY          TO REC-KEY.
             MOVE NET-RECORD         TO REC-NETWORK.
             GO TO AY50.
      *    *************************************************************
      *    ****           I N T E G R A T I O N   F I L E           ****
      *    *************************************************************
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
      *    ****   Q   F U L L  -  W A I T   F O R   4   S E C O N D S
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
                       ADD 1  TO WS-SUB
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
      *    ****           S E T   W A I T   P E R I O D
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
      *
      *    ****    D E B T O R   T R A N S A C T I O N S
      *
       CG000         SECTION 5.
       CG00.
             PERFORM ERASE-SCREEN.
             DISPLAY S26.

       CG05.
             MOVE 1                  TO WS-NETKEY.
             PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             MOVE DEB-BATCH          TO W85-BATCH.
           IF DEB-BATCH = 999999
               MOVE 1                TO DEB-BATCH
           ELSE
               ADD 1                 TO DEB-BATCH.
             MOVE W85-BATCH          TO WS-BATCH.
             PERFORM REWRITE-CONTROL-UNLOCK THRU WRITE-CONTROL-EXIT.
             DISPLAY WS-BATCH AT 0609 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             MOVE ZERO               TO W90-DEBIT  W90-CREDIT W90-VATTOT.

       CG10.
             MOVE ZERO               TO W90-BDT W90-30 W90-BCR W90-60 W90-CUR W90-90 W90-INT W90-DISC W90-120 W40-SDISC WS-VAT-VALUE.
             MOVE 1                  TO WS-S9.

       CG12.
             MOVE ZERO               TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
           IF WS-S9 < 12
               ADD 1   TO WS-S9
               GO TO CG12.

       CG15.
             MOVE 0                  TO WS-PAGE WS-ADVANCE.
             MOVE 2                  TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.

       CG20.
             PERFORM HA000.

       CG30-TRAN.
             MOVE SPACES             TO W80-ACNO.
             MOVE ZERO               TO W90-CODE.
             MOVE W12-TODAY          TO W90-DTE W90-DATE.

       CG35.
             MOVE ZERO               TO W90-DB W90-CR WS-VAT-VALUE W90-EX.
             MOVE SPACES             TO W90-REF.

       CG40-ACCEPT.
      *
      *    Display User Options
      *
             DISPLAY S40.
             ACCEPT S36 AT 0902.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE SPACES TO W80-ACNO
                                 GO TO CG280
                                 
                 WHEN F1-KEY     PERFORM HELP-ROUTINE
                 
                 WHEN F2-KEY     MOVE SPACE      TO W40-LUP
                                 PERFORM AA50
                                 
                 WHEN F8-KEY     PERFORM GA000
                                 GO TO CG40-ACCEPT
                                 
                 WHEN F9-KEY     MOVE "A"        TO W40-LUP
                                 PERFORM AA50
                                 
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO CG40-ACCEPT
               END-EVALUATE
               DISPLAY S36 AT 0902
               IF W80-ACNO = SPACES
                   GO TO CG40-ACCEPT.
             PERFORM CLEAR-L50.
           IF W80-ACNO = SPACES
                GO TO CG280.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W80-ACNO BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             MOVE W80-ACNO           TO DEB-ACNO.
             PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR = 1
               MOVE WS-ERR2          TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CG40-ACCEPT.
           IF WS-ERROR = 2
               MOVE WS-ERR1          TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CG40-ACCEPT.
             MOVE 10                 TO CRT-START.
             MOVE 21                 TO CRT-END.
             PERFORM ERASE-SCREEN.
             DISPLAY S12.
      *    *************************************************************
      *    ****         T E S T   F O R   I N D I V I D U A L       ****
      *    *************************************************************
           IF W40-TYPE = 1
               MOVE "D"              TO W40-INSTR
               CALL "DTP\DTPNAME" USING W40-COMPANY LS-USER-ID
           ELSE
               MOVE W40-NAME         TO W40-DSP-NAME.
             COMPUTE W40-ONE = W40-BAL - (W40-CUR + W40-30 + W40-60 + W40-90 + W40-120 + W40-INTEREST).
             DISPLAY S45.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               UNLOCK DEBTOR
               GO TO CG35.
             PERFORM CLEAR-L50.
             DISPLAY S50.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN F9-KEY     MOVE DEB-ACNO TO W80-ACNO
                                 PERFORM DEBTOR-MEMO
               END-EVALUATE.
             PERFORM CLEAR-L50.
             DISPLAY S55.
             
       CG45-ACCEPT.
             MOVE 0909               TO DATEPOS.
             PERFORM DATE-CHECK-ACCEPT-PLUS THRU CHECK-DATE-EXIT.
           IF W22-INSTR = X"EC"
               MOVE 9                TO CRT-START CRT-END
               PERFORM ERASE-SCREEN
               GO TO CG35.
           IF W20-DATE < WS-VAT-DATE(WS-VAT-SUB)
               ADD 6                 TO WS-VAT-SUB.

       CG50-ACCEPT.
             DISPLAY S60.
             ACCEPT W90-CODE AT 0920 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE 9    TO CRT-START CRT-END
                                 PERFORM ERASE-SCREEN
                                 GO TO CG35
                                 
                 WHEN F6-KEY     PERFORM SAVE-SCREEN
                                 PERFORM TRANCDE-VIEW
                                 PERFORM RESTORE-SCREEN
                                 GO TO CG50-ACCEPT
                                 
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO CG50-ACCEPT
               END-EVALUATE.
           IF W90-CODE = ZERO
               GO TO CG40-ACCEPT.
           IF W90-CODE < 01 OR > 99
               GO TO CG55-ERROR.
             PERFORM GET-DEBTOR-TRAN-DESC.
           IF W00-T-CODE = ZERO
               GO TO CG55-ERROR.
             MOVE W00-E-DESC         TO W90-DESC.
             DISPLAY W90-DESC AT 0923 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             GO TO  CG60-REF.

       CG55-ERROR.
             MOVE "Code"             TO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.
             GO TO CG50-ACCEPT.

       CG60-REF.
             ACCEPT W90-REF AT 0936 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE 9    TO CRT-START CRT-END
                                 PERFORM ERASE-SCREEN
                                 GO TO CG35
                                 
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO CG60-REF
               END-EVALUATE.

       CG65-VALUE.
             PERFORM CLEAR-L50.
           IF W00-T-CODE = 02 OR 05 OR 06 OR 17 OR 20
               MOVE "N"              TO WS-TAX
           ELSE
               PERFORM CG70 THRU CG85.
           IF W00-ACTION = 1
               MOVE ZERO             TO W90-DB
               GO TO CG120-CREDIT.
             GO TO CG90-DEBIT.

       CG70.
             MOVE "I"                TO WS-OPTION.
             MOVE "M"                TO WS-INSTR.
             MOVE "VAT: 'N'o, 'I'ncluded, 'K'ey in, 'C'alculate [I]" TO WS-OPT-MES.
             PERFORM OPT-MESSAGE.
             MOVE WS-OPTION          TO WS-TAX.
          IF WS-TAX = "N"
              GO TO CG85.
            MOVE "1"                 TO WS-OPTION.

       CG80.
      *      MOVE "VAT Rate code 1 - 6 [1]"
      *     TO WS-ERR-MES.
      *      PERFORM OPT-MESSAGE.
      *    IF WS-OPTION < "1" OR > "6"
      *        GO TO CG80.
      *      STRING "VAT" DELIMITED SIZE WS-OPTION DELIMITED SIZE INTO WS-VAT-CODE.
             MOVE "VAT1"              TO WS-VAT-CODE.
             MOVE WS-OPTION           TO WS-VAT-SUB.

       CG85.
             EXIT.
      *
      *   ****    D E B I T   T R A N S A C T I O N
      *
       CG90-DEBIT.
           IF W00-T-CODE = 13
               ACCEPT S26-VAT
               MOVE WS-VAT-VALUE TO W90-VAT(WS-VAT-SUB) W90-DB
               DISPLAY S26-DT
           ELSE
               EVALUATE WS-TAX
      *                                      CALCULATE TAX
                 WHEN "C"  ACCEPT S26-VAL
                           PERFORM CG110
                           ADD W90-VAL W90-VAT(WS-VAT-SUB) GIVING W90-DB
                           DISPLAY S26-VAT
                           DISPLAY S26-DT
      *                                      INCLUSIVE TAX
                 WHEN "I"  ACCEPT S26-DT
                           PERFORM CG105
                           DISPLAY S26-VAL
                           DISPLAY S26-VAT
      *                                      KEY IN THE TAX
                 WHEN "K"  ACCEPT S26-VAL
                           ACCEPT S26-VAT
                           MOVE WS-VAT-VALUE TO W90-VAT(WS-VAT-SUB)
                           ADD W90-VAL WS-VAT-VALUE  GIVING W90-DB
                           DISPLAY S26-DT
      *                                      NO TAX
                 WHEN  "N" ACCEPT S26-DT
                           MOVE W90-DB  TO W90-VAL W90-EX
                           DISPLAY S26-VAL
                END-EVALUATE.
           IF W90-DB = ZERO
               GO TO CG125-CORRECT.
           IF NOT((W90-CODE = 01) AND (DEB-SDISC NOT = ZERO))
               GO TO CG100.

       CG95.
             MOVE "Does settlement discount apply Y/N?  [Y]" TO WS-OPT-MES.
             MOVE "Y"                TO WS-OPTION.
             MOVE "1"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "Y"
               MOVE DEB-SDISC              TO W40-SDISC
               MOVE W05-DSETT (W40-SDISC)  TO W90-DISC.
       CG100.
             MOVE ZERO               TO W90-CR.
             GO TO CG125-CORRECT.

       CG105.
             MOVE W05-VAT(WS-VAT-SUB)  TO W05-RATE.
             COMPUTE W90-VAT(WS-VAT-SUB) ROUNDED = ((W90-DB / (100.000 + W05-RATE)) * W05-RATE)
             SUBTRACT W90-VAT(WS-VAT-SUB) FROM W90-DB  GIVING W90-VAL.
             MOVE W90-VAT(WS-VAT-SUB)  TO WS-VAT-VALUE.
             MOVE W90-VAL              TO W90-TAXVAL(WS-VAT-SUB).

       CG110.
             MOVE W05-VAT(WS-VAT-SUB)  TO W05-RATE.
             COMPUTE W90-VAT(WS-VAT-SUB) ROUNDED = (W90-VAL * W05-RTE).
             MOVE W90-VAT(WS-VAT-SUB)  TO WS-VAT-VALUE.
             MOVE W90-VAL              TO W90-TAXVAL(WS-VAT-SUB).

       CG115.
             MOVE W05-VAT(WS-VAT-SUB)  TO W05-RATE.
             COMPUTE W90-VAT(WS-VAT-SUB) ROUNDED = ((W90-CR / (100.000 + W05-RATE)) * W05-RATE)
             SUBTRACT W90-VAT(WS-VAT-SUB) FROM W90-CR  GIVING W90-VAL.
             MOVE W90-VAT(WS-VAT-SUB)  TO WS-VAT-VALUE.
             MOVE W90-VAL              TO W90-TAXVAL(WS-VAT-SUB).
      *
      *   ****    C R E D I T   T R A N S A C T I O N
      *
       CG120-CREDIT.
           IF W00-T-CODE = 14
               ACCEPT S26-VAT
               MOVE WS-VAT-VALUE     TO W90-VAT(WS-VAT-SUB) W90-CR
               DISPLAY S26-CR
           ELSE
               EVALUATE WS-TAX
      *                                      CALCULATE TAX
                 WHEN "C"  ACCEPT S26-VAL
                           PERFORM CG110
                           ADD W90-VAL W90-VAT(WS-VAT-SUB) GIVING W90-CR
                           DISPLAY S26-VAT
                           DISPLAY S26-CR
      *                                      INCLUSIVE TAX
                 WHEN "I"  ACCEPT S26-CR
                           PERFORM CG115
                           DISPLAY S26-VAL
                           DISPLAY S26-VAT
      *                                      KEY IN THE TAX
                 WHEN "K"  ACCEPT S26-VAL
                           ACCEPT S26-VAT
                           MOVE WS-VAT-VALUE TO W90-VAT(WS-VAT-SUB)
                           ADD W90-VAL WS-VAT-VALUE  GIVING W90-CR
                           DISPLAY S26-CR
      *                                      NO TAX
                 WHEN "N"  ACCEPT S26-CR
                           MOVE W90-CR       TO W90-VAL W90-EX
                           DISPLAY S26-VAL
               END-EVALUATE.

       CG125-CORRECT.
           IF W90-CR = 0 AND W90-DB = 0
               MOVE "Value"          TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CG40-ACCEPT.
           IF W90-CODE NOT = 17
               GO TO CG130.
           IF W90-CR NOT > DEB-INT
               GO TO CG130.
             MOVE "Credit > Interest on account"     TO WS-ERR-MES.
             MOVE "'ENTER' to change - 'X' to exit" TO WS-ERR-MES-2.
             PERFORM ERROR-MESSAGE.
             MOVE 22                 TO CRT-START.
             MOVE 24                 TO CRT-END.
             PERFORM ERASE-SCREEN.
           IF WS-OPTION = "X"
              GO TO CG40-ACCEPT.
            GO TO CG65-VALUE.

       CG130.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO CG40-ACCEPT.

       CG135.
             MOVE ZERO               TO W75-KEY W75-ALLOCATED.
           IF W90-CR NOT = ZERO
               IF OPEN-ITEM
                   MOVE 13           TO CRT-START.
                   MOVE 24           TO CRT-END.
                   PERFORM ERASE-SCREEN.
                   DISPLAY S08A
                   MOVE 19           TO CLIN
                   MOVE ZERO         TO W90-ALLOCATED W90-DISCOUNT
                   PERFORM ALLOCATE-CREDIT
                   IF WS-ERROR = 9
                       GO TO CG260.
             PERFORM GET-DEBTOR-TRAN THRU GET-DEBTOR-TRAN-EXIT.
             PERFORM AY01 THRU AY59.
             PERFORM GENERATE-DEBTOR-TRAN.
             ADD W90-DB              TO W90-BDT W90-DEBIT.
             ADD W90-CR              TO W90-BCR W90-CREDIT.

       CG140.
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
              GO TO CG255.
           IF OPEN-ITEM
               SUBTRACT W90-CR       FROM DEB-BAL
               GO TO CG255.
             MOVE "Allocate to selected age-analysis (Y/N)  [ ]" TO WS-OPT-MES.
             MOVE SPACE              TO WS-OPTION.
             PERFORM OPT-MESSAGE.
             MOVE ZERO               TO W40-MTD W40-CURW W40-30W W40-60W W40-90W W40-120W W40-INT.
           IF NOT(WS-OPTION = "Y")
               IF W00-ACTION = 1
                   PERFORM DEBTOR-CREDITS THRU DEBTOR-TRAN-EXIT
                   GO TO CG255
               ELSE
                   GO TO CG255.
             DISPLAY "Allocation" AT 1135.
           IF W00-ACTION = ZERO
               MOVE W90-DB           TO W25-VALUE
               GO TO CG205.

       CG145.
             MOVE 1234               TO CPOS.

       CG150.
             DISPLAY "             " AT CPOS WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
           IF CLIN < 18
               ADD 1                 TO CLIN
               GO TO CG150.
             DISPLAY "Transaction balance:" AT 2012 WITH FOREGROUND-COLOR Cyan.

       CG155.
             MOVE W25-VALUE          TO W95-B.
             DISPLAY W95-V2 AT 2034 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             PERFORM CLEAR-L50.

       CG160.
             ACCEPT S29 AT 1234.
           IF W40-MTD > W25-VALUE
              PERFORM CG190
              GO TO CG160.
           IF W40-MTD NOT = ZERO
               IF W40-MTD > W40-ONE
                   IF W40-MTD < DEB-BAL
                       PERFORM CG195
                       GO TO CG160.
             SUBTRACT W40-MTD        FROM W25-VALUE.
             PERFORM CG155.
           IF W25-VALUE = ZERO
               GO TO CG200.

       CG165.
             ACCEPT S30 AT 1334.
           IF W40-CURW > W25-VALUE
               PERFORM CG190
               GO TO CG165.
           IF W40-CURW NOT = ZERO
               IF W40-CURW > DEB-CUR
                   PERFORM CG195
                   GO TO CG165.
             SUBTRACT W40-CURW       FROM W25-VALUE.
             PERFORM CG155.
           IF W25-VALUE = ZERO
               GO TO CG200.

       CG170.
             ACCEPT S31 AT 1434.
           IF W40-30W > W25-VALUE
               PERFORM CG190
               GO TO CG170.
           IF W40-30W > DEB-30
               PERFORM CG195
               GO TO CG170.
             SUBTRACT W40-30W        FROM W25-VALUE.
             PERFORM CG155.
           IF W25-VALUE = ZERO
              GO TO CG200.

       CG175.
             ACCEPT S32 AT 1534.
           IF W40-60W > W25-VALUE
               PERFORM CG190
               GO TO CG175.
           IF W40-60W > DEB-60
               PERFORM CG195
               GO TO CG175.
             SUBTRACT W40-60W        FROM W25-VALUE.
             PERFORM CG155.
           IF W25-VALUE = ZERO
               GO TO CG200.

       CG180.
             ACCEPT S33 AT 1634.
           IF W40-90W > W25-VALUE
               PERFORM CG190
               GO TO CG180.
           IF W40-90W > DEB-90
               PERFORM CG195
               GO TO CG180.
             SUBTRACT W40-90W        FROM W25-VALUE.
             PERFORM CG155.
           IF W25-VALUE = ZERO
               GO TO CG200.

       CG185.
             ACCEPT S34 AT 1734.
           IF W40-120W > W25-VALUE
               PERFORM CG190
               GO TO CG185.
           IF W40-120W > DEB-120
               PERFORM CG195
               GO TO CG180.
             SUBTRACT W40-120W       FROM W25-VALUE.
             PERFORM CG155.
           IF W25-VALUE = ZERO
               GO TO CG200.
           IF W25-VALUE NOT > DEB-INT
               MOVE W25-VALUE        TO W40-INT
           ELSE
               MOVE DEB-INT          TO W40-INT.
             MOVE W40-INT            TO W95-B.
             DISPLAY W95-V2 AT 1834 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             SUBTRACT W40-INT        FROM W25-VALUE.
             PERFORM CG155.
           IF W25-VALUE = ZERO
               GO TO CG200.
             MOVE "Total not allocated - 'R'edo,'A'uto-allocation" TO WS-ERR-MES.
             MOVE SPACE              TO WS-OPTION.
             PERFORM ERROR-MESSAGE.
           IF WS-OPTION = "A"
               PERFORM DEBTOR-CREDITS THRU DEBTOR-TRAN-EXIT
               GO TO CG255.
      *
      *    Assume "R" (Default)
      *
             MOVE ZERO               TO W40-CUR W40-30 W40-60  W40-90 W40-120 W40-INT.
             MOVE W90-CR             TO W25-VALUE.
             GO TO CG145.

       CG190.
             MOVE "Value allocated > Receipt value"  TO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.

       CG195.
             MOVE "Value allocated > Analysis value" TO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.

       CG200.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               MOVE W90-CR           TO W25-VALUE
               MOVE ZERO             TO W40-CURW W40-30W W40-60W W40-90W W40-120W W40-INT
               GO TO CG145.
             SUBTRACT W40-30W        FROM DEB-30.
             SUBTRACT W40-60W        FROM DEB-60.
             SUBTRACT W40-90W        FROM DEB-90.
             SUBTRACT W40-120W       FROM DEB-120.
             SUBTRACT W40-INT        FROM DEB-INT.
             SUBTRACT W90-CR         FROM DEB-BAL.
             ADD W40-30W             TO W90-30.
             ADD W40-60W             TO W90-60.
             ADD W40-90W             TO W90-90.
             ADD W40-120W            TO W90-120.
             ADD W40-INT             TO W90-INT.
           IF W40-CURW < DEB-CUR
               SUBTRACT W40-CURW     FROM DEB-CUR
               ADD W40-CURW          TO W90-CUR
           ELSE
               ADD DEB-CUR           TO W90-CUR
               MOVE ZERO             TO DEB-CUR.
               GO TO CG255.

       CG205.
             MOVE 1234               TO CPOS.

       CG210.
             DISPLAY "             " AT CPOS WITH FOREGROUND-COLOR Cyan.
           IF CLIN < 18
               ADD 1                 TO CLIN
               GO TO CG210.
             DISPLAY "Transaction balance:" AT 2012 WITH FOREGROUND-COLOR Cyan.

       CG215.
             MOVE W25-VALUE          TO W95-B.
             DISPLAY W95-V2 AT 2035 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             PERFORM CLEAR-L50.

       CG220.
             ACCEPT S35 AT 1235.
           IF W40-INT > W25-VALUE
               PERFORM CG190
               GO TO CG220.
             SUBTRACT W40-INT        FROM W25-VALUE.
             PERFORM CG215.
           IF W25-VALUE = ZERO
               GO TO CG250.

       CG225.
             ACCEPT S30 AT 1335.
           IF W40-CURW > W25-VALUE
               PERFORM CG190
               GO TO CG225.
             SUBTRACT W40-CURW       FROM W25-VALUE.
             PERFORM CG215.
           IF W25-VALUE = ZERO
               GO TO CG250.

       CG230.
             ACCEPT S31 AT 1435.
           IF W40-30W > W25-VALUE
               PERFORM CG190
               GO TO CG230.
             SUBTRACT W40-30W        FROM W25-VALUE.
             PERFORM CG215.
           IF W25-VALUE = ZERO
               GO TO CG250.

       CG235.
             ACCEPT S32 AT 1535.
           IF W40-60W > W25-VALUE
               PERFORM CG190
               GO TO CG235.
             SUBTRACT W40-60W        FROM W25-VALUE.
             PERFORM CG215.
           IF W25-VALUE = ZERO
               GO TO CG250.

       CG240.
             ACCEPT S33 AT 1635.
           IF W40-90W > W25-VALUE
               PERFORM CG190
               GO TO CG240.
             SUBTRACT W40-90W        FROM W25-VALUE.
             PERFORM CG215.
           IF W25-VALUE = ZERO
               GO TO CG250.
           IF W25-VALUE = ZERO
               GO TO CG250.

       CG245.
             ACCEPT S34 AT 1735.
           IF W40-120W > W25-VALUE
               PERFORM CG190
               GO TO CG245.
             SUBTRACT W40-120W       FROM W25-VALUE.
             PERFORM CG215.
           IF W25-VALUE = ZERO
               GO TO CG250.
             MOVE "Total must be allocated" TO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.
             MOVE ZERO               TO W40-CURW W40-30W W40-60W W40-90W W40-120W W40-INT.
             MOVE W90-DB             TO W25-VALUE.
             GO TO CG205.

       CG250.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               MOVE W90-DB           TO W25-VALUE
               MOVE ZERO             TO W40-CURW W40-30W W40-60W W40-90W W40-120W W40-INT
               GO TO CG205.
             ADD W40-CURW            TO DEB-CUR.
             ADD W40-30W             TO DEB-30.
             ADD W40-60W             TO DEB-60.
             ADD W40-90W             TO DEB-90.
             ADD W40-120W            TO DEB-120.
             SUBTRACT W40-CURW       FROM W90-CUR.
             SUBTRACT W40-30W        FROM W90-30.
             SUBTRACT W40-60W        FROM W90-60.
             SUBTRACT W40-90W        FROM W90-90.
             SUBTRACT W40-120W       FROM W90-120.

       CG255.
             MOVE DEB-ACNO           TO R-TAC.
             MOVE W40-DSP-NAME       TO R-DNME.
             MOVE W90-DTE            TO R-TDTE.
             MOVE W90-DESC           TO R-TDESC.
             MOVE W90-REF            TO R-TREF.
             COMPUTE W90-TTAX = W90-VAT(1) + W90-VAT(2) + W90-VAT(3) + W90-VAT(4) + W90-VAT(5) + W90-VAT(6) + W90-VAT(7) + W90-VAT(8) + W90-VAT(9) + W90-VAT(10) + W90-VAT(11) + W90-VAT(12).
           IF W90-DB > 0
               MOVE W90-VAL          TO R-VAL
               MOVE W90-TTAX         TO R-TAX
               MOVE W90-DB           TO R-TOT
           ELSE
               MULTIPLY -1 BY W90-VAL  GIVING R-VAL
               MULTIPLY -1 BY W90-TTAX GIVING R-TAX
               MULTIPLY -1 BY W90-CR   GIVING R-TOT.
           IF W02-LINAGE < (W02-PRN-LENGTH - 2)
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM HA000.
             MOVE SPACES             TO R-DET1.
           IF W90-CR NOT = ZERO
               IF OPEN-ITEM
                   PERFORM CG290-OPEN
                   CLOSE DEBTRN.
             PERFORM REWRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.

       CG260.
             MOVE  7                 TO CRT-START.
             MOVE 24                 TO CRT-END.
             PERFORM ERASE-SCREEN.
             MOVE W90-DEBIT          TO W100-BAL.
             MOVE W90-CREDIT         TO W100-CUR.
             DISPLAY "Debit total:"  AT 2112.
             DISPLAY W100-BAL        AT 2125 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             DISPLAY "Credit total:" AT 2139.
             DISPLAY W100-CUR        AT 2153 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Red.
             PERFORM CG265-NEW THRU CG275.
             PERFORM CG10 THRU CG12.
             GO TO CG35.

       CG265-NEW.
             SUBTRACT W90-BCR FROM W90-BDT GIVING W25-VALUE.
             ADD W25-VALUE           TO DEB-OUT.
             ADD W90-BDT             TO DEB-DT.
             ADD W90-BCR             TO DEB-CR.
             SUBTRACT W90-120        FROM DEB-FOUR.
             SUBTRACT W90-90         FROM DEB-THREE.
             SUBTRACT W90-60         FROM DEB-TWO.
             SUBTRACT W90-30         FROM DEB-ONE.
             SUBTRACT W90-CUR        FROM DEB-MTD.
             SUBTRACT W90-INT        FROM DEB-INTEREST.

       CG270.
           IF WS-TAX = "N"
               GO TO CG275.
             MOVE 9                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             PERFORM AY01 THRU AY59.
             MOVE WS-VAT-CODE        TO DPT-CODE.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
             PERFORM AY38 THRU AY59.
           IF W90-VAL NOT = ZERO
               IF W90-TX = ZERO
                   IF W90-DB = ZERO
                       ADD W90-VAL   TO PAR-CNGS-DAY PAR-CNGS-MTD PAR-CNGS-YTD
                   ELSE
                       ADD W90-VAL   TO PAR-DNGS-DAY PAR-DNGS-MTD PAR-DNGS-YTD
               ELSE
               IF W90-DB = ZERO
                   ADD W90-VAL       TO PAR-CVAT-DAY PAR-CVAT-MTD PAR-CVAT-YTD DPT-V-RET-DAY DPT-V-RET-MTD DPT-V-RET-YTD
                   ADD W90-TX        TO DPT-O-VRET-DAY DPT-O-VRET-MTD DPT-O-VRET-YTD
                   IF WX-INT = "Y"
                       COMPUTE WX-VALUE = W90-TX * -1
                       MOVE DPT-O-VAT-GL  TO WX-AC
                       MOVE 11            TO WX-TYPE
                       PERFORM GL-TRF-UPDATE
                   END-IF
               ELSE
                   ADD W90-VAL       TO PAR-DVAT-DAY PAR-DVAT-MTD PAR-DVAT-YTD DPT-V-SALES-DAY DPT-V-SALES-MTD DPT-V-SALES-YTD
                   ADD W90-TX        TO DPT-O-VAT-DAY DPT-O-VAT-MTD DPT-O-VAT-YTD
                   IF WX-INT = "Y"
                       COMPUTE WX-VALUE = W90-TX * -1
                       MOVE DPT-O-VAT-GL TO WX-AC
                       MOVE 10           TO WX-TYPE
                       PERFORM GL-TRF-UPDATE.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.

       CG275.
           IF W00-T-CODE = 19
               MOVE "XXXX"           TO DPT-CODE
               PERFORM READ-DEPART THRU READ-DEPART-EXIT
               PERFORM AY38 THRU AY59
               ADD W90-VAL           TO DPT-SALES-DAY DPT-SALES-MTD DPT-SALES-YTD
               PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
             PERFORM AY70 THRU AY999.

       CG280.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE "Total"            TO R-TDESC.
             MOVE W90-VATTOT         TO R-TAX.
             COMPUTE R-TOT = W90-DEBIT - W90-CREDIT.
             MOVE 99                 TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE 7                  TO CRT-START.
             MOVE 24                 TO CRT-END.
             PERFORM ERASE-SCREEN.
             PERFORM CLEAR-L50.

       CG285.
             MOVE "New Batch Y/N?  [ ]" TO WS-ERR-MES.
             MOVE SPACE              TO WS-OPTION.
             PERFORM OPT-MESSAGE.
           IF NOT(WS-OPTION = "Y" OR "N")
               GO TO CG285.
           IF WS-OPTION = "N"
               GO TO CG999.
             GO TO CG00.

       CG999.
            EXIT.

       CG290-OPEN      SECTION 5.
       CG290.
           IF W75-KEY < WS-DBTKEY
               GO TO CG300.
             PERFORM READ-DEBTRN THRU READ-DEBTRN-EXIT.
           IF DBT-UPDATE = SPACE
               GO TO CG295.
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

       CG295.
             ADD 1                   TO WS-DBTKEY.
             GO TO CG290.

       CG300.
             EXIT.

       HA000        SECTION 6.
       HA00.
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
             MOVE "DEBTOR TRANSACTIONS - AUDIT TRAIL" TO R-H5.
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

       HA05.
             MOVE "H"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DET1.
             MOVE "AC.NO"            TO R-H7.
             MOVE "DEBTOR NAME"      TO R-H7A.
             MOVE "DATE"             TO R-H8.
             MOVE "TRAN TYPE"        TO R-H10.
             MOVE "REF"              TO R-H11.
             MOVE "VALUE"            TO R-H12.
             MOVE "VAT"              TO R-H13.
             MOVE "TOTAL"            TO R-H14.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET1.
             MOVE "D"                TO W02-PRN-TYPE.

       HA99.
             EXIT.
      *
      *    ****    C A L L   D E B T O R   C R E A T I O N
      *
       GA000        SECTION 7.
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
      *    *************************************************************
      *    ****   G E T   D E B T O R   M O N T H - E N D   D A Y   ****
      *    *************************************************************
             MOVE 1                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
           IF DEB-MTH-END < 21 OR > 31 AND < 99
               MOVE 99               TO DEB-MTH-END.
             MOVE DEB-MTH-END        TO WS-MTHEND.

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
        
               WHEN OTHER      MOVE "ERROR"       TO WS-FILE
                               MOVE "DTP010"      TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
