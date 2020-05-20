      $SET LINKCOUNT"512" GNT"DTP005.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****      ****    ********   *
      *   **    **     **     **    **   **  **    **  **   **         *
      *   **    **     **     **    **  **    **  **    **  *******    *
      *   **    **     **     *******   **    **  **    **        **   *
      *   **    **     **     **        **    **  **    **        **   *
      *   **    **     **     **         **  **    **  **   **    **   *
      *   *******      **     **          ****      ****     ******    *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *            A C C O U N T S   R E C E I V A B L E   /           *
      *                                                                *
      *       D E B T O R   M E M O / R E M A R K S   M A I N T        *
      *                                                                *
      *       Version 9.04.01 - May 2016                               *
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
      * Dec 2012 - Increased the number of Memo entries, of 60 bytes,  *
      *            allowed from 100 to 200 per account.                *
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
       PROGRAM-ID.     DTP005.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   February 1990.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1990 - 2017
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

       COPY "DEBTOR.SL".

       COPY "DEBMEM.SL".

       COPY "MEMDEB.SL".

      *COPY "PARAM.SL".

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

       COPY "DEBTOR.FDE".

       COPY "DEBMEM.FDE".

       COPY "MEMDEB.FDE".

      *COPY "PARAM.FDE".

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
       77  WS-PARKEY               PIC  9(06)    COMP-5.
       77  WS-RECKEY               PIC  9(06)    COMP-5.
       77  WS-TRANS                PIC  9(04)    COMP-5 VALUE 1.
       77  WS-FIRST                PIC  9(04)    COMP-5 VALUE 0.
       77  WS-LAST                 PIC  9(04)    COMP-5 VALUE 0.
       77  WS-MEMPOS               PIC  9(04)    COMP-5 VALUE 0.
       77  WS-FIN-RATE             PIC S9(03)V99 COMP-3.
       77  WS-PERIOD               PIC  9(02).
       77  WS-NTP                  PIC  X(01).
       77  WS-KEYSTR               PIC  9(06)    COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-SKIP                 PIC  X(01)           VALUE "N".
       77  WS-TAX                  PIC  X(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-ERROR                PIC  9(01)           VALUE ZERO.
       77  WS-EKEY                 PIC  9(06)    COMP-5.
       77  WS-COMP                 PIC  9(01).
       77  WS-RT-HD                PIC  X(10)           VALUE "-Retail   ".
       77  WS-WS-HD                PIC  X(10)           VALUE "-Wholesale".
       77  WS-CS-HD                PIC  X(10)           VALUE "-Cash sale".
       77  WS-ERR1                 PIC  X(22)           VALUE "Invalid Account Number".
       77  WS-ERR2                 PIC  X(09)           VALUE "No Record".
       77  WS-E                    PIC  X(15)           VALUE "**END** - ENTER".
       77  WS-AC                   PIC  X(07)           VALUE "ACCOUNT".
       77  PRG-DEBT-LUP            PIC  X(12)           VALUE "DTP\DTPLOOK".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.

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
           03  CRT-PROGRAM         PIC  X(15) VALUE "DTP005".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "ACCOUNTS RECEIVABLE - MEMO MAINTENANCE".
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
           03  WS-MODULE             PIC  X(03) VALUE "DTP".
           03  WS-PROG               PIC  X(03) VALUE "005".

       01  WS-PARID.
           03  WS-SYS-ID             PIC  X(03).

       01  W10-SPARES.
           03  W10-DATE              PIC X(10).
           03  W10-RDATE                           REDEFINES W10-DATE.
               05  W10-EDTE          PIC 99/99/9999.

       01  W10-REF.
           03  W10-NUM               PIC  9(08).

      *
      *    Date routines for accepting checking and formatting have
      *    been removed from each program and the DateCheck program
      *    will be called to handle these routines.
      *
      *    The following copy 'DateVariables.CPY' now includes the
      *    'W12.WS'and 'W20.WS' variables and is passed to DateCheck
      *    in the CALL statement.
      *
       COPY "DateVariables.cpy".

       COPY "W40.DBT".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W75-KEYS.
           03  W75-S                 PIC  9(02)    COMP-5.
           03  W75-S1                PIC  9(02)    COMP-5.
           03  W75-KEY               PIC  9(06)    COMP-5.
           03  W75-ALLOCATED         PIC S9(09)V99 COMP-3.
           03  W75-BALANCE           PIC S9(09)V99 COMP-3.
           03  W75-DNO               PIC  X(06)    OCCURS 18.

       COPY "W75.WS".

       01  W80-ACCOUNT.
           03  W80-ACNO              PIC  X(06).
      *    03  W80-TYPE              PIC  9(01).

       01  W85-PASS.
           03  W85-SUPER             PIC  X(06)    OCCURS 9.
           03  W85-ENTRY             PIC  9(02)    COMP-3.
           03  W85-MARG              PIC S9(02)V99.

       01  W95-STM.
           03  W95-COMP              PIC  X(40).

       01  W100-EDIT.
           03  W100-LBAL             PIC  Z(06)9.99-.
           03  W100-LARR             PIC  Z(06)9.99.
           03  W100-BAL              PIC  Z(08)9.99-.
           03  W100-CUR              PIC  Z(08)9.99-.
           03  W100-30               PIC  Z(08)9.99-.
           03  W100-60               PIC  Z(08)9.99-.
           03  W100-90               PIC  Z(08)9.99-.
           03  W100-120              PIC  Z(08)9.99-.
           03  W100-INT              PIC  Z(08)9.99.
           03  W100-CRL              PIC  Z(06)9.
           03  W100-KEY              PIC  Z(05)9.
      *
      *    W200-MEMO has been included to make the displaying and data
      *    capture (Screen Handling) of the memo data easier. The data
      *    to be displayed, amended or added will be in this RECORD in
      *    WORKING storage.
      *
       01  W200-MEMO.
           03  W200-MEM-FLDS.
               05  W200-SEQ1         PIC  9(04).
               05  W200-MEM1         PIC  X(60).
               05  W200-SEQ2         PIC  9(04).
               05  W200-MEM2         PIC  X(60).
               05  W200-SEQ3         PIC  9(04).
               05  W200-MEM3         PIC  X(60).
               05  W200-SEQ4         PIC  9(04).
               05  W200-MEM4         PIC  X(60).
               05  W200-SEQ5         PIC  9(04).
               05  W200-MEM5         PIC  X(60).
               05  W200-SEQ6         PIC  9(04).
               05  W200-MEM6         PIC  X(60).
               05  W200-SEQ7         PIC  9(04).
               05  W200-MEM7         PIC  X(60).
               05  W200-SEQ8         PIC  9(04).
               05  W200-MEM8         PIC  X(60).
               05  W200-SEQ9         PIC  9(04).
               05  W200-MEM9         PIC  X(60).
               05  W200-SEQ10        PIC  9(04).
               05  W200-MEM10        PIC  X(60).
               05  W200-SEQ11        PIC  9(04).
               05  W200-MEM11        PIC  X(60).
               05  W200-SEQ12        PIC  9(04).
               05  W200-MEM12        PIC  X(60).
               05  W200-SEQ13        PIC  9(04).
               05  W200-MEM13        PIC  X(60).
               05  W200-SEQ14        PIC  9(04).
               05  W200-MEM14        PIC  X(60).
               05  W200-SEQ15        PIC  9(04).
               05  W200-MEM15        PIC  X(60).
               05  W200-SEQ16        PIC  9(04).
               05  W200-MEM16        PIC  X(60).
               05  W200-SEQ17        PIC  9(04).
               05  W200-MEM17        PIC  X(60).
               05  W200-SEQ18        PIC  9(04).
               05  W200-MEM18        PIC  X(60).
               05  W200-SEQ19        PIC  9(04).
               05  W200-MEM19        PIC  X(60).
               05  W200-SEQ20        PIC  9(04).
               05  W200-MEM20        PIC  X(60).
               05  W200-SEQ21        PIC  9(04).
               05  W200-MEM21        PIC  X(60).
               05  W200-SEQ22        PIC  9(04).
               05  W200-MEM22        PIC  X(60).
               05  W200-SEQ23        PIC  9(04).
               05  W200-MEM23        PIC  X(60).
               05  W200-SEQ24        PIC  9(04).
               05  W200-MEM24        PIC  X(60).
               05  W200-SEQ25        PIC  9(04).
               05  W200-MEM25        PIC  X(60).
               05  W200-SEQ26        PIC  9(04).
               05  W200-MEM26        PIC  X(60).
               05  W200-SEQ27        PIC  9(04).
               05  W200-MEM27        PIC  X(60).
               05  W200-SEQ28        PIC  9(04).
               05  W200-MEM28        PIC  X(60).
               05  W200-SEQ29        PIC  9(04).
               05  W200-MEM29        PIC  X(60).
               05  W200-SEQ30        PIC  9(04).
               05  W200-MEM30        PIC  X(60).
               05  W200-SEQ31        PIC  9(04).
               05  W200-MEM31        PIC  X(60).
               05  W200-SEQ32        PIC  9(04).
               05  W200-MEM32        PIC  X(60).
               05  W200-SEQ33        PIC  9(04).
               05  W200-MEM33        PIC  X(60).
               05  W200-SEQ34        PIC  9(04).
               05  W200-MEM34        PIC  X(60).
           03  W200-MEM-FOC REDEFINES W200-MEM-FLDS.
               05  W200-MEM-DET      OCCURS 34.
                   07  W200-MSQ      PIC  9(04).
                   07  W200-MEM      PIC  X(60).
      *
      *    All current keys for MEMO data for the account that is
      *    being worked on will be in this RECORD W200-KEYS while
      *    the data will be in RECORD W205-MEMO.
      *
       01  W200-KEYS.
           03  W200-KEY              OCCURS 200.
               05  W200-AC           PIC  X(06).
               05  W200-SEQ          PIC  9(04).

       01  W205-MEMO.
           03  W205-MEM-DET.
               05  W205-MEM          PIC  X(60) OCCURS 200.
           03  W205-S                PIC  9(04) COMP-5.
           03  W205-S1               PIC  9(04) COMP-5.
           03  W205-S2               PIC  9(04) COMP-5.
           03  W205-LAST             PIC  9(04) COMP-5.

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

       01  S06.
           03  LINE  5 COLUMN 37 VALUE "INTERNAL" FOREGROUND-COLOR Grey HIGHLIGHT.

       COPY "DEBTOR.CRT".

       01  S36.
           03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta PIC X(06) USING W80-ACNO AUTO.


       01  S-MEM.
           02  FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      *
      *                              1         2         3         4         5         6         7         8
      *                     12345678901234567890123456789012345678901234567890123456789012345678901234567890
      *
           03  LINE  8 COLUMN 13 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
           03  LINE  9 COLUMN 13 VALUE "³Lin    M E M O / R E M A R K S / C O R R E S P O N D E N C E    ³".
           03  LINE 10 COLUMN 13 VALUE "ÆÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ".
           03  LINE 11 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 12 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 13 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 14 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 15 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 16 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 17 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 18 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 19 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 20 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 21 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 22 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 23 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 24 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 25 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 26 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 27 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 28 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 29 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 30 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 31 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 32 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 33 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 34 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 35 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 36 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 37 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 38 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 39 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 40 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 41 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 42 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 43 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 44 COLUMN 13 VALUE "³   ³                                                            ³".
           03  LINE 45 COLUMN 13 VALUE "ÀÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
      *
      *    U S E D   T O   D I S P L A Y   M E M O   D A T A
      *
       01  SD-MEM.
           02  FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
           03  LINE 11 COLUMN 14 PIC ZZ9   USING W200-SEQ1  FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM1.
           03  LINE 12 COLUMN 14 PIC ZZ9   USING W200-SEQ2  FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM2.
           03  LINE 13 COLUMN 14 PIC ZZ9   USING W200-SEQ3  FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM3.
           03  LINE 14 COLUMN 14 PIC ZZ9   USING W200-SEQ4  FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM4.
           03  LINE 15 COLUMN 14 PIC ZZ9   USING W200-SEQ5  FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM5.
           03  LINE 16 COLUMN 14 PIC ZZ9   USING W200-SEQ6  FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM6.
           03  LINE 17 COLUMN 14 PIC ZZ9   USING W200-SEQ7  FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM7.
           03  LINE 18 COLUMN 14 PIC ZZ9   USING W200-SEQ8  FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM8.
           03  LINE 19 COLUMN 14 PIC ZZ9   USING W200-SEQ9  FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM9.
           03  LINE 20 COLUMN 14 PIC ZZ9   USING W200-SEQ10 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM10.
           03  LINE 21 COLUMN 14 PIC ZZ9   USING W200-SEQ11 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM11.
           03  LINE 22 COLUMN 14 PIC ZZ9   USING W200-SEQ12 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM12.
           03  LINE 23 COLUMN 14 PIC ZZ9   USING W200-SEQ13 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM13.
           03  LINE 24 COLUMN 14 PIC ZZ9   USING W200-SEQ14 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM14.
           03  LINE 25 COLUMN 14 PIC ZZ9   USING W200-SEQ15 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM15.
           03  LINE 26 COLUMN 14 PIC ZZ9   USING W200-SEQ16 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM16.
           03  LINE 27 COLUMN 14 PIC ZZ9   USING W200-SEQ17 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM17.
           03  LINE 28 COLUMN 14 PIC ZZ9   USING W200-SEQ18 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM18.
           03  LINE 29 COLUMN 14 PIC ZZ9   USING W200-SEQ19 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM19.
           03  LINE 30 COLUMN 14 PIC ZZ9   USING W200-SEQ20 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM20.
           03  LINE 31 COLUMN 14 PIC ZZ9   USING W200-SEQ21 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM21.
           03  LINE 32 COLUMN 14 PIC ZZ9   USING W200-SEQ22 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM22.
           03  LINE 33 COLUMN 14 PIC ZZ9   USING W200-SEQ23 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM23.
           03  LINE 34 COLUMN 14 PIC ZZ9   USING W200-SEQ24 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM24.
           03  LINE 35 COLUMN 14 PIC ZZ9   USING W200-SEQ25 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM25.
           03  LINE 36 COLUMN 14 PIC ZZ9   USING W200-SEQ26 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM26.
           03  LINE 37 COLUMN 14 PIC ZZ9   USING W200-SEQ27 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM27.
           03  LINE 38 COLUMN 14 PIC ZZ9   USING W200-SEQ28 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM28.
           03  LINE 39 COLUMN 14 PIC ZZ9   USING W200-SEQ29 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM29.
           03  LINE 40 COLUMN 14 PIC ZZ9   USING W200-SEQ30 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM30.
           03  LINE 41 COLUMN 14 PIC ZZ9   USING W200-SEQ31 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM31.
           03  LINE 42 COLUMN 14 PIC ZZ9   USING W200-SEQ32 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM32.
           03  LINE 43 COLUMN 14 PIC ZZ9   USING W200-SEQ33 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM33.
           03  LINE 44 COLUMN 14 PIC ZZ9   USING W200-SEQ34 FOREGROUND-COLOR Green.
           03          COLUMN 18 PIC X(60) USING W200-MEM34.
      *
      *    U S E D   T O   C A P T U R E   O R   A M E N D   M E M O   D A T A
      *
       01  SA-MEM.
           03  FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(60) USING W200-MEM(WS-S2) AUTO.

       01  SD-SEQ.
           03  FOREGROUND-COLOR Green BACKGROUND-COLOR Grey PIC ZZ9 USING W200-MSQ(WS-S2) AUTO.

       01  SD-MEM-OPT.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
               05  LINE 50 COLUMN 03 VALUE " "                                                       FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 06 VALUE   ",".
               05          COLUMN 07 VALUE    "PgUp"                                                       FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 11 VALUE        "/".
               05          COLUMN 12 VALUE         "PgDn"                                                  FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 16 VALUE             ",".
               05          COLUMN 17 VALUE              "Esc"                                              FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 20 VALUE                 "ape,".
               05          COLUMN 24 VALUE                     "F2"                                        FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 26 VALUE                       "=Insert line,".
               05          COLUMN 39 VALUE                                    "F3"                         FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 41 VALUE                                      "=Delete line,".
               05          COLUMN 54 VALUE                                                   "F10"         FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 57 VALUE                                                       "=Update" .
               
       01  SD-OPTIONS.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
               05  LINE 50 COLUMN 02 VALUE "F1"                                                                     FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 04 VALUE   "=Help,".
               05          COLUMN 10 VALUE         "F2"                                                             FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 12 VALUE           " to do Account Lookup,".
               05          COLUMN 34 VALUE                                 "F9"                                     FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 36 VALUE                                   " Debtor Lookup (Any Word),".
               05          COLUMN 62 VALUE                                                             "Esc"        FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 65 VALUE                                                               " to exit".

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
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM CJ000.

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
             PERFORM SCREEN-CONTENTS.
             CALL PRG-DEBT-LUP USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS W40-COMPANY
                  ON EXCEPTION GO TO AA55
             END-CALL.
             MOVE W40-ACNO           TO W80-ACNO.
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

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000              SECTION.

       COPY "CONTROL.RD".

       COPY "DEBTOR.RD".

       COPY "DEBMEM.RD".

       COPY "MEMDEB.RD".

      *COPY "PARAM.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AD000             SECTION.

       COPY "CONTROL.WR".

       COPY "DEBTOR.WR".

       COPY "DEBMEM.WR".

       COPY "MEMDEB.WR".

      *COPY "PARAM.WR".

       COPY "APAC.HLP".

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
      *        MOVE DEB-TYPE     TO W80-TYPE
      *
      *    ****    Move the Debtor Record to working storage area
      *
               MOVE DEB-RECORD1      TO W40-DEBTREC.

       CA160-EXIT.
             EXIT.
      *
      *    ****    D E B T O R   M E M O   M A I N T E N A N C E
      *
       CJ000        SECTION 5.
       CJ00.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
             DISPLAY "Account :" AT 0602.
             MOVE SPACES             TO W80-ACNO.

       CJ05.
             DISPLAY SD-OPTIONS.
             ACCEPT S36 AT 0612.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE SPACES    TO W80-ACNO
                                 GO TO CJ999

                 WHEN F1-KEY     PERFORM HELP-ROUTINE

                 WHEN F2-KEY     MOVE SPACE     TO W40-LUP
                                 PERFORM AA50-LOOKUP

                 WHEN F9-KEY     MOVE "A"       TO W40-LUP
                                 PERFORM AA50-LOOKUP

                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO CJ05
               END-EVALUATE
               DISPLAY S36 AT 0612
               IF W80-ACNO = SPACES
                   GO TO CJ05.
      *      MOVE 12                 TO CRT-START CRT-END.
      *      PERFORM ERASE-SCREEN.
             PERFORM CLEAR-L50.
           IF W80-ACNO = SPACES
               GO TO CJ999.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W80-ACNO BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             MOVE W80-ACNO           TO DEB-ACNO.
             PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR = 1
               MOVE WS-ERR2          TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CJ05.
           IF WS-ERROR = 2
               MOVE WS-ERR1          TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CJ05.
             MOVE 6                  TO CRT-START CRT-END.
             PERFORM ERASE-SCREEN.
           IF W40-TYPE NOT = 1
               GO TO CJ15.
             DISPLAY S22.
             DISPLAY S36 AT 0615.

       CJ10.
             DISPLAY S25.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO CJ00.
             GO TO CJ25.

       CJ15.
             DISPLAY S23.
           IF W40-TYPE = 3
               DISPLAY S06.
             DISPLAY S36 AT 0615.

       CJ20.
             DISPLAY S24.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO CJ00.
      *
      *    ****   M E M O   W I N D O W
      *
       CJ25.
             PERFORM SAVE-SCREEN-3.
             MOVE 9                  TO SHADE-ROW.
             MOVE 15                 TO SHADE-COL.
             MOVE 64                 TO SHADE-WIDTH.
             MOVE 37                 TO SHADE-LINES.
             DISPLAY S-MEM.
             PERFORM SCREEN-SHADOW.
      *
      *     S U B S C R I P T S   A N D   P O I N T E R S
      *
      *     WS-S1   - Used for moving data to and from the Screen area
      *               W200 to Work W205 or Work File MEM-DATA.
      *     WS-S2   - Current MEMO line on screen.
      *     WS-S6   - Used when moving date up or down in the work area.
      *     WS-S7   - Used to store W205-S1 when inserting or deleting
      *               lines of data.
      *     WS-S8   - Used to store W205-S2 when inserting or deleting
      *               lines of data.
      *     WS-S9   - Pointer for Work Area and Keys
      *     W205-S  - Pointing to the W205-MEM-DET entry that is the
      *               first entry in the memo window on the screen.
      *     W205-S1 - Used as a pointer when moving data from or to
      *               W205-MEM-DET and W200-MEM-FLDS (Screen).
      *     W205-S2 - Pointer to the current line in W205-MEM-DET
      *     W205-LAST - The last entry in the Work Area W205-MEM-DET
      *
      *
      *     C H E C K   I F   T H E R E   I S   A N Y   D A T A
      *
             INITIALIZE W200-MEM-FLDS.
             MOVE ZERO               TO W205-LAST W205-S1 W205-S2.
             MOVE W80-ACNO           TO DME-AC.
             MOVE 1                  TO DME-SEQ.
             MOVE 1                  TO WS-FIRST WS-MEMPOS WS-LAST.
             PERFORM READ-DEBMEM THRU READ-DEBMEM-EXIT.
      *
      *     I F   T H E R E   I S   N O   D A T A   -   B Y P A S S
      *
           IF NOT(WS-F-ERROR = ZERO)
               OPEN OUTPUT MEMDEB CLOSE MEMDEB OPEN I-O MEMDEB
               MOVE ZERO             TO WS-S9
               MOVE SPACES           TO W205-MEM-DET W200-KEYS
               MOVE 1                TO W205-S W205-S2 W200-SEQ1
               DISPLAY SD-SEQ AT LINE CLIN COLUMN 14
               GO TO CJ40.
      *
      *     G E T   E X I S T I N G   M E M O   D A T A
      *
             PERFORM GET-DEBMEM-DETAILS.
             MOVE 1                  TO W205-S.
      *
      *     S E T   S T A R T   P O S I T I O N   F O R   D I S P L A Y 
      *     A N D   A C C E P T
      *
       CJ28.
           IF W205-S2 > W205-LAST
               MOVE W205-S2          TO W205-LAST.
             MOVE 1                  TO WS-S1.
             MOVE W205-S             TO W205-S1 W205-S2.
             INITIALIZE W200-MEM-FLDS.
      *
      *     S T O R E   M E M O   D E T A I L S   I N   D I S P L A Y   
      *     A N D   A C C E P T   A R E A
      *
       CJ30.
             MOVE W205-MEM(W205-S1)  TO W200-MEM(WS-S1).
             MOVE W200-SEQ(W205-S1)  TO W200-MSQ(WS-S1).
           IF WS-S1 > 33
               GO TO CJ40.
      *
      *     C H E C K   F O R   E N D   O F   M E M O   D A T A
      *
       CJ35.
           IF W205-S1 < W205-LAST
               ADD 1                 TO WS-S1 W205-S1
               GO TO CJ30.

       CJ40.
             DISPLAY SD-MEM.
             DISPLAY SD-MEM-OPT.

       CJ45.
      *
      *      CLIN = 11  CCOL = 18
      *
             MOVE 1118               TO CPOS.
             MOVE 1                  TO WS-S2.

       CJ50.
             ACCEPT SA-MEM AT CPOS.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
      *
      *        M O V E   U P   O N E   L I N E   (SCREEN)
      *
                 WHEN UP-KEY     IF W205-S = 1
                                     GO TO CJ45
                                 ELSE
                                     SUBTRACT 1 FROM W205-S WS-S2
                                 END-IF
                                 GO TO CJ28
      *
      *       M O V E   D O W N   O N E   L I N E   (SCREEN)
      *
                 WHEN DOWN-KEY   IF W205-S2 < 200
                                     ADD 1              TO W205-S2
                                     IF W205-S2 > W205-LAST
                                         MOVE W205-S2   TO W205-LAST
                                         MOVE W80-ACNO  TO W200-AC(W205-S2)
                                         MOVE W205-S2   TO W200-SEQ(W205-S2)
                                         DISPLAY SD-SEQ AT LINE CLIN COLUMN 14
                                     END-IF
                                 END-IF
                                 IF WS-S2 < 34
                                     ADD 1              TO WS-S2 CLIN
                                 ELSE
      *
      *     On the bottom line of the screen window: Move the data up
      *     and get the next line from the work area (W205)
      *
                                     PERFORM CP000
                                 END-IF
                                       GO TO CJ50
      *
      *       I N V A L I D   K E Y   S T R O K E
      *
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO CJ45
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
      *
      *              E X I T
      *
                 WHEN ESC-KEY    GO TO CJ110
      *
      *              P A G E   D O W N
      *
                 WHEN PAGE-DOWN  PERFORM CK000
                                 IF W205-S > 165
                                     MOVE 167    TO W205-S
                                 ELSE
                                     ADD 34      TO W205-S
                                 END-IF
      *
      *              P A G E   U P
      *
                 WHEN PAGE-UP    PERFORM CK000
                                 IF W205-S < 35
                                     MOVE 1       TO W205-S
                                 ELSE
                                     SUBTRACT 34  FROM W205-S
                                 END-IF
      *
      *              I N S E R T   A   L I N E
      *
                 WHEN F2-KEY     PERFORM CK000
                                 IF WS-LAST < 200
                                     PERFORM CL000
                                 END-IF
      *
      *              D E L E T E   A   L I N E
      *
                 WHEN F3-KEY     MOVE SPACES     TO W200-MEM1
                                 PERFORM CK000
                                 MOVE W205-S2    TO WS-S6
                                 PERFORM CM000
                                 PERFORM CJ28 THRU CJ40
                                 MOVE WS-S6      TO W205-S2
                                 GO TO CJ45
      *
      *              E N D   I N P U T
      *
                 WHEN F10-KEY    PERFORM CK000
                                 ADD 33 W205-S    GIVING W205-S2
                                 IF W205-S2 > W205-LAST
                                     MOVE W205-S2 TO W205-LAST
                                 END-IF
                                 PERFORM CHECK-CORRECT
                                 IF WS-OPTION = "N"
                                     GO TO CJ45
                                 END-IF
                                 PERFORM CN000
                                 GO TO CJ110
      *
      *              I N V A L I D   K E Y   S T R O K E
      *
                 WHEN OTHER PERFORM AA900-ALARM
               END-EVALUATE
               GO TO CJ28.
      *
      *     U P D A T E   W 2 0 5   W I T H   A D D E D / A M E N D E D
      *            D A T A   F R O M   S C R E E N   A R E A
      *
             MOVE W200-MEM(WS-S2)    TO W205-MEM(W205-S2).
           IF WS-S2 < 34
               IF W205-S2 > W205-LAST
                   MOVE W205-S2      TO W205-LAST
               END-IF
               ADD 1                 TO W205-S2 WS-S2 CLIN
               IF W205-S2 > W205-LAST
                   MOVE W80-ACNO     TO W200-AC(W205-S2)
                   MOVE W205-S2      TO W200-SEQ(W205-S2) W200-MSQ(WS-S2)
                   DISPLAY SD-SEQ AT LINE CLIN COLUMN 14
               END-IF
           ELSE
               PERFORM CP000
           END-IF.
             GO TO CJ50.

       CJ110.
             PERFORM RESTORE-SCREEN-3.
             CLOSE MEMDEB.
             GO TO CJ00.

       CJ999.
             EXIT.
      *
      *     M O V E   D A T A   F R O M   A C C E P T   A R E A   T O
      *     W 2 0 5   A N D   W R I T E   T H E S E   D E T A I L S
      *     T O   T H E   W O R K   F I L E
      *
       CK000        SECTION 5.
       CK00.
             MOVE W205-S             TO W205-S1.
             MOVE 1                  TO WS-S1.
             MOVE DEB-ACNO           TO MEM-AC.

       CK05.
             MOVE W205-S1            TO MEM-SEQ.
             MOVE W200-MEM(WS-S1)    TO W205-MEM(W205-S1) MEM-DATA.
             PERFORM REWRITE-MEMDEB THRU WRITE-MEMDEB-EXIT.
           IF WS-F-ERROR = 62
               MOVE ZERO             TO WS-F-ERROR
               IF NOT(MEM-DATA = SPACES)
                   PERFORM WRITE-MEMDEB THRU WRITE-MEMDEB-EXIT.
           IF WS-S1 < 12
               ADD 1                 TO WS-S1 W205-S1
               GO TO CK05.

       CK999.
             EXIT.
      *
      *     I N S E R T   A   B L A N K   L I N E
      *
       CL000        SECTION 5.
       CL00.
             MOVE W205-S1            TO WS-S7.
             MOVE W205-S2            TO WS-S8.
             MOVE W205-LAST          TO W205-S1.
             ADD 1                   TO W205-LAST.
             MOVE W205-LAST          TO W205-S2.
             MOVE DEB-ACNO           TO MEM-AC.
      *
      *     M O V E   D A T A   U P   I N   W O R K   A R E A   A N D
      *               I N   T H E   W O R K   F I L E
      *
       CL05.
             MOVE W205-S2            TO MEM-SEQ.
             MOVE W205-MEM(W205-S1)  TO W205-MEM(W205-S2) MEM-DATA.
             MOVE MEM-KEY            TO W200-KEY(W205-S2).
           IF W205-S2 = W205-LAST
               PERFORM WRITE-MEMDEB THRU WRITE-MEMDEB-EXIT
               IF WS-F-ERROR = 62
                   PERFORM REWRITE-MEMDEB THRU WRITE-MEMDEB-EXIT
                   MOVE ZERO         TO WS-F-ERROR
               END-IF
           ELSE
               PERFORM REWRITE-MEMDEB THRU WRITE-MEMDEB-EXIT.
           IF W205-S1 > WS-S8
               SUBTRACT 1            FROM W205-S1 W205-S2
               GO TO CL05.
             MOVE SPACES             TO W205-MEM(W205-S1) MEM-DATA.
             MOVE W205-S1            TO MEM-SEQ.

       CL10.
             PERFORM REWRITE-MEMDEB THRU WRITE-MEMDEB-EXIT.
             MOVE WS-S7              TO W205-S1.
             MOVE WS-S8              TO W205-S2.

       CL999.
             EXIT.
      *
      *     D E L E T E   T H E   L I N E   B E I N G   E D I T E D
      *
       CM000        SECTION 5.
       CM00.
             MOVE W205-S1            TO WS-S7.
             MOVE W205-S2            TO WS-S8.
             MOVE DEB-ACNO           TO MEM-AC.

       CM02.
           IF W205-MEM(W205-LAST) = SPACES
               IF W205-S2 = W205-LAST
                    GO TO CM999
               ELSE
               IF W205-S2 < W205-LAST
                   SUBTRACT 1        FROM W205-LAST
                   GO TO CM02.
           IF W205-S2 = W205-LAST
               MOVE SPACES           TO W205-MEM(W205-S2)
               GO TO CM10.
             ADD 1 W205-S2           GIVING W205-S1.
      *
      *    M O V E   D A T A   D O W N   I N   W O R K   A R E A   A N D
      *    I N   T H E   W O R K   F I L E
      *
       CM05.
             MOVE W205-S2            TO MEM-SEQ.
             MOVE W205-MEM(W205-S1)  TO W205-MEM(W205-S2) MEM-DATA.
             MOVE MEM-KEY            TO W200-KEY(W205-S2).
             PERFORM REWRITE-MEMDEB THRU WRITE-MEMDEB-EXIT.
             MOVE SPACES             TO W205-MEM(W205-S1).
           IF W205-S1 < W205-LAST
               ADD 1                 TO W205-S1 W205-S2
               GO TO CM05.

       CM10.
             MOVE SPACES             TO MEM-DATA.
             MOVE W205-LAST          TO MEM-SEQ.
             PERFORM READ-MEMDEB THRU READ-MEMDEB-EXIT.
             PERFORM DELETE-MEMDEB-REC THRU WRITE-MEMDEB-EXIT.
             SUBTRACT 1              FROM W205-LAST.
             MOVE WS-S7              TO W205-S1.
             MOVE WS-S8              TO W205-S2.
           IF W205-S2 > W205-LAST
               MOVE W205-LAST        TO W205-S2 WS-S6.

       CM999.
             EXIT.
      *
      *     U P D A T E   T H E   M E M O   F I L E
      *
       CN000        SECTION 5.
       CN00.
             MOVE 1                  TO W205-S1 WS-S6.
      *
      *     F I N D   L A S T   L I N E   W I T H   D A T A
      *
       CN02.
           IF W205-MEM(W205-LAST) = SPACES
               SUBTRACT 1            FROM W205-LAST
               GO TO CN02.
      *
      *     R E M O V E   B L A N K   L I N E S
      *
       CN05.
           IF W205-MEM(W205-S1) = SPACES
               PERFORM CO000
               GO TO CN05.
           IF W205-S1 < W205-LAST
               ADD 1                 TO W205-S1
               GO TO CN05.
             MOVE 1                  TO W205-S1.
             MOVE DEB-ACNO           TO DME-AC.
      *
      *     U P D A T E   M E M O   F I L E
      *
       CN10.
             MOVE W205-S1            TO DME-SEQ.
             MOVE W205-MEM(W205-S1)  TO DME-DATA.
      *
      *     F I N D   K E Y   I N   K E Y   T A B L E
      *
       CN15.
           IF DME-KEY < W200-KEY(WS-S6)
               GO TO CN20.
           IF DME-KEY = W200-KEY(WS-S6)
               GO TO CN25.
           IF WS-S6 < W205-LAST
               ADD 1   TO WS-S6
               GO TO CN15.
      *
      *     A D D   N E W   M E M O   R E C O R D
      *
       CN20.
             PERFORM WRITE-DEBMEM THRU WRITE-DEBMEM-EXIT.
             GO TO CN30.
      *
      *     U P D A T E  E X I S T I N G   M E M O   R E C O R D
      *
       CN25.
             PERFORM REWRITE-DEBMEM THRU WRITE-DEBMEM-EXIT.
      *
      *     C L E A R   K E Y S   O F   U S E D   R E C O R D S
      *
             MOVE SPACES             TO W200-KEY(WS-S6).

       CN30.
           IF W205-S1 < W205-LAST
               ADD 1                 TO W205-S1
               GO TO CN10.
      *
      *     D E L E T E  R E D U N D A N T   R E C O R D S   F R O M
      *     M E M O   F I L E
      *
             MOVE 1                  TO WS-S6.
      *
      *     F I N D   R E D U N D A N T   K E Y S
      *
       CN35.
           IF NOT(W200-KEY(WS-S6) = SPACES)
               MOVE W200-KEY(WS-S6)  TO DME-KEY
               PERFORM READ-DEBMEM THRU READ-DEBMEM-EXIT
               PERFORM DELETE-DEBMEM-REC THRU WRITE-DEBMEM-EXIT.
           IF WS-S6 < WS-S9
               ADD 1                 TO WS-S6
               GO TO CN35.

       CN999.
             EXIT.
      *
      *     M O V E   D A T A   D O W N   I N   W O R K   A R E A
      *
       CO000        SECTION 5.
       CO00.
             MOVE W205-S1            TO WS-S7.
             ADD 1 WS-S7             GIVING WS-S8.

       CO05.
             MOVE W205-MEM(WS-S8)    TO W205-MEM(WS-S7).
             MOVE SPACES             TO W205-MEM(WS-S8).
           IF WS-S8 < W205-LAST
               ADD 1                 TO WS-S8 WS-S7
               GO TO CO05.
             SUBTRACT 1              FROM W205-LAST.

       CO999.
             EXIT.
      *
      *     M O V E   D A T A   U P   1   L I N E   O N   T H E
      *     S C R E E N   A N D   G E T   T H E   N E X T   L I N E
      *     F R O M   T H E   W O R K   A R E A   W 2 0 5
      *
       CP000        SECTION 5.
       CP00.
           IF W205-S > 165
               MOVE 167              TO W205-S
           ELSE
               ADD 1                 TO W205-S.
             INITIALIZE W200-MEM-FLDS.
             MOVE 1                  TO WS-S1.
             MOVE W205-S             TO W205-S1 W205-S2.
             PERFORM TEST AFTER UNTIL WS-S1 > 34
                 MOVE W205-MEM(W205-S1)  TO W200-MEM(WS-S1)
                 MOVE W200-SEQ(W205-S1)  TO W200-MSQ(WS-S1)
                 IF W205-S1 < W205-LAST
                     ADD 1           TO W205-S1 WS-S1
                 ELSE
                     MOVE 99         TO WS-S1
                 END-IF
             END-PERFORM.
      *
      *     I S   T H E   L A S T   L I N E   I N   T H E   M E M O
      *     W I N D O W   A   B L A N K   L I N E ?
      *
           IF W200-MSQ(34) = ZERO
               SUBTRACT 1 FROM W205-S1 GIVING W200-MSQ(34).
             DISPLAY SD-MEM.

       CP999.
             EXIT.

       GET-DEBMEM-DETAILS SECTION 5.
       OPEN-MEMDEB.
             OPEN OUTPUT MEMDEB.
             CLOSE MEMDEB.
             OPEN I-O MEMDEB.
             MOVE ZERO               TO WS-S9.
             MOVE SPACES             TO W205-MEM-DET W200-KEYS.
             MOVE DME-AC             TO MEM-AC.

       WRITE-MEMO-DETAILS.
             MOVE DME-SEQ            TO MEM-SEQ WS-LAST.
             ADD 1                   TO WS-S9.
             MOVE DME-DATA           TO MEM-DATA W205-MEM(WS-S9).
             MOVE DME-KEY            TO W200-KEY(WS-S9).
             PERFORM WRITE-MEMDEB THRU WRITE-MEMDEB-EXIT.
             PERFORM READ-DEBMEM-NEXT THRU READ-DEBMEM-EXIT.
           IF WS-F-ERROR = ZERO
               IF DME-AC = W80-ACNO
                   GO TO WRITE-MEMO-DETAILS.
             MOVE WS-S9              TO W205-LAST.

       GET-DEBMEM-DETAILS-EXIT.
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
             MOVE LS-COMPANY         TO CRT-COMPANY W95-COMP.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *
      *     D O   N O T   D I S P L A Y   E R R O R   M E S S A G E S
      *
             MOVE 44                 TO USER-NUMBER.
             MOVE 1                  TO USER-SETTING.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.PRO".

       ZA999-EXIT.
             EXIT.
      *
      *     I / O   E R R O R   M E S S A G E S
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

      *        WHEN 15         MOVE W02-PARAM    TO WS-FILE
      *                        MOVE WS-PARKEY    TO WS-KEY

               WHEN 27         MOVE W02-DEBMEM   TO WS-FILE
                               MOVE DME-KEY      TO WS-KEYX

               WHEN 62         MOVE W02-MEMDEB   TO WS-FILE
                               MOVE MEM-KEY      TO WS-KEYX

               WHEN OTHER      MOVE "ERROR"      TO WS-FILE
                               MOVE "DTP005"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
