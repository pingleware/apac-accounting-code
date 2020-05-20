      $SET LINKCOUNT"384" GNT"CRP005.GNT"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******     ****      ****    ********   *
      *   **    **  **    **  **    **   **  **    **  **   **         *
      *   **        **    **  **    **  **    **  **    **  *******    *
      *   **        ******    *******   **    **  **    **        **   *
      *   **        **  **    **        **    **  **    **        **   *
      *   **    **  **   **   **         **  **    **  **   **    **   *
      *    ******   **    **  **          ****      ****     ******    *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *       C R E D I T O R   M E M O / R E M A R K S   M A I N T    *
      *                                                                *
      *       Version 9.04.05 - June 2018                              *
      *                                                                *
      ******************************************************************
      *                                                                *
      * August 2011 - Amended MEMO file to have 6 character Account    *
      *               Reference code.Increased sequence to 4 digits.   *
      *               Included Recovery routines and file locking for  *
      *               this program.                                    *
      *                                                                *
      * Dec 2012 - Increased the number of Memo entries, of 60 bytes,  *
      *            allowed from 100 to 200 per account.                *
      *                                                                *
      *  Jan 2013 - Program CRPMEMO introduced to handle memo lookups  *
      *             reducing code in all programs and removing file    *
      *             CRDMEM from programs. Copy file 'CRDMEM.PRO' now   *
      *             copied into all programs that require lookups for  *
      *             memo/remarks on a Creditor account. Move the Cred- *
      *             itor code/account number to W15-ACNO and then      *
      *             PERFORM CREDITOR-MEMO. The Creditor lookup program *
      *             CRPLOOK now includes a Memo Lookup so unless there *
      *             is a requirement to lookup memo details after this *
      *             lookup, the MEMO lookup can be removed from all    *
      *             CREDITOR programs.                                 *
      *                                                                *
      * June 2018 - Added program CRPRDWR to handle IO for Creditor    *
      *             file and COPY file CA155.CRP to call and handlw IO *
      *             for the CREDITOR file. SEE CA155.CRP and CRPRDWR   *
      *             for details.                                       *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     CRP005.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   OCTOBER 1983.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2017
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

       COPY "CREDIT.SL".

       COPY "CRDMEM.SL".

       COPY "MEMCRD.SL".

       COPY "PARAM.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

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

       COPY "CREDIT.FDE".

       COPY "CRDMEM.FDE".

       COPY "MEMCRD.FDE".

       COPY "PARAM.FDE".

       COPY "RECOVER.FD".

       COPY "SHARED.FDE".

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
       77  WS-RECOVER              PIC  9(04)    COMP-5.
       77  WS-SHARED               PIC  9(04)    COMP-5 VALUE 1.
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
       77  WS-ERR1                 PIC  X(22)           VALUE "Invalid Account Number".
       77  WS-ERR2                 PIC  X(09)           VALUE "No Record".
       77  WS-E                    PIC  X(15)           VALUE "**END** - ENTER".
       77  WS-ER8                  PIC  X(09)           VALUE "No record".
       77  PRG-NAME                PIC  X(12)           VALUE SPACES.
       77  PRG-CRED-LUP            PIC  X(12)           VALUE "CRP\CRPLOOK".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.
      *
      *    ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
      *    ณ Used by calling program (WORKING-STORAGE) and called program ณ
      *    ณ (LINKAGE SECTION) for Main screen layout and headings.       ณ
      *    ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *         C =  Clear comment line (50)
      *         E =  Clear lines (any line or lines between 5 and 46)
      *         H =  Change heading
      *         S =  Clear the screen
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "CRP005".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "CREDITOR MEMO/REMARKS MAINTENANCE".
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
           03  WS-MODULE           PIC  X(03) VALUE "CRP".
           03  WS-PROG             PIC  X(03) VALUE "005".

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

       01  W10-SPARES.
           03  W10-DATE            PIC  X(08).
           03  W10-EDTE            PIC  99/99/9999 REDEFINES W10-DATE.

       01  W10-REF.
           03  W10-NUM             PIC  9(08).

       COPY "W12.WS".

       COPY "W15.CRP".

      *01  W40-NADD.
      *    03  W40-ADD.
      *        05  W40-ACHAR       PIC  X(01) OCCURS 70.
      *    03  W40-ADDR                       REDEFINES W40-ADD.
      *        05  W40-ADDRS       PIC  X(60).
      *        05  FILLER          PIC  X(10).
      *    03  W40-NAME.
      *        05  W40-NCHAR       PIC  X(01) OCCURS 40.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

      *01  W60-NME-ADD.
      *    03  W60-NAME.
      *        05  W60-NCHAR       PIC  X(01) OCCURS 40.
      *    03  W60-ADDRESS.
      *        05  W60-ACHAR       PIC  X(01) OCCURS 70.
      *    03  W60-ADDR                       REDEFINES W60-ADDRESS.
      *        05  W60-ADD         PIC  X(60).
      *        05  FILLER          PIC  X(10).
      *    03  W60-ADDRESS1.
      *        05  W60-PAD         PIC  X(60).
      *        05  FILLER          PIC  X(10).

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
           03  W200-MEM-FOC                            REDEFINES W200-MEM-FLDS.
               05  W200-MEM-DET                        OCCURS 34.
                   07  W200-MSQ      PIC  9(04).
                   07  W200-MEM      PIC  X(60).
      *
      *    All current keys for MEMO data for the account that is
      *    being worked on will be in this RECORD W200-KEYS while
      *    the data will be in RECORD W205-MEMO.
      *
       01  W200-KEYS.
           03  W200-KEY                                OCCURS 200.
               05  W200-AC           PIC  X(06).
               05  W200-SEQ          PIC  9(04).

       01  W205-MEMO.
           03  W205-MEM-DET.
               05  W205-MEM          PIC  X(60)        OCCURS 200.
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

       COPY "CREDITOR.CRT".

       01  S55.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
               05  LINE 50 COLUMN  2 VALUE "F1"                                           FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN  4 VALUE   "=Help,".
               05          COLUMN 10 VALUE         "F2"                                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 12 VALUE           " to do Creditor Lookup ".
               05          COLUMN 35 VALUE                                  "Esc"         FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 38 VALUE                                     " to exit".

      *COPY "CRDLUP.CRT".

       01  S-MEM.
           02  FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      *
      *                              1         2         3         4         5         6         7         8
      *                     12345678901234567890123456789012345678901234567890123456789012345678901234567890
      *
           03  LINE  8 COLUMN 13 VALUE "ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ".
           03  LINE  9 COLUMN 13 VALUE "ณLin    M E M O / R E M A R K S / C O R R E S P O N D E N C E    ณ".
           03  LINE 10 COLUMN 13 VALUE "ฦอออัออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออต".
           03  LINE 11 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 12 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 13 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 14 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 15 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 16 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 17 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 18 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 19 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 20 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 21 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 22 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 23 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 24 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 25 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 26 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 27 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 28 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 29 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 30 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 31 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 32 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 33 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 34 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 35 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 36 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 37 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 38 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 39 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 40 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 41 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 42 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 43 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 44 COLUMN 13 VALUE "ณ   ณ                                                            ณ".
           03  LINE 45 COLUMN 13 VALUE "ภฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู".
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
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      *         Check the security level of the USER.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
           IF LS0-CRLEV < 2
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM CJ000.

       AA49.
             EXIT PROGRAM.

       COPY "AA50C.LUP".
       
      *    
      *            ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *           S C R E E N ,   K E Y B O A R D   &   M O U S E
      *    ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
      *    ณ      SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3       ณ
      *    ฦอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออต
      *    ณ                      SCREEN-SHADOW                        ณ
      *    รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด
      *    ณ To routine is used to display a shadow down the right and ณ
      *    ณ along the bottom of a pop-up box. The parameters that are ณ
      *    ณ required:                                                 ณ
      *    ณ          SHADE-ROW   - Top line of the box.               ณ
      *    ณ          SHADE-COL   - Left line of box.                  ณ
      *    ณ          SHADE-WIDTH - Width of the box.                  ณ
      *    ณ          SHADE-LINES - Height of box.                     ณ
      *    ฦอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออต
      *    ณ                      CHECK-CORRECT                        ณ
      *    รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด
      *    ณ This routine displays a pop-up window with the message    ณ
      *    ณ           "Press Y if correct - N if incorrect"           ณ
      *    ณ                                                           ณ
      *    ณ The response is returned in WS-OPTION (in upper case).    ณ
      *    ณ                                                           ณ
      *    ณ To start with the pop-up window higher or lower than row  ณ
      *    ณ 20 (default); move another value to WS-MES-LINE.          ณ
      *    ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
      *            T H I S   R O U T I N E   I S   U S E D   T O
      *       D I S P L A Y   I N F O R M A T I O N   M E S S A G E S
      *    ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
      *    ณ  Move the message to WS-DSP-MES and the top line of the   ณ
      *    ณ  message box to WS-MES-LINE. If WS-MES-LINE is zero the   ณ
      *    ณ  default will be 20.                                      ณ
      *    ณ                  PERFORM DISPLAY-MESSAGE                  ณ
      *    ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
      *           T H I S   R O U T I N E   I S   U S E D   T O
      *            D I S P L A Y   E R R O R   M E S S A G E S
      *    ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
      *    ณ                      ERROR-MESSAGE                        ณ
      *    รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด
      *    ณ To display the error message higher or lower (default is  ณ
      *    ณ line 20) Move the line number which must be used as the   ณ
      *    ณ top line to WS-MES-LINE. The message to be displayed must ณ
      *    ณ be in WS-ERR-MES. PERFORM ERROR-MESSAGE.                  ณ
      *    ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

       COPY "FUNCTION.CRT".

       COPY "LOCKED.REC".

      *    ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
      *    ณ           ERASE SCREEN FROM SPECIFIED LOCATION            ณ
      *    รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด
      *    ณ Uses CRT-START as starting line and increases and CRT-END ณ
      *    ณ as the ending line. The screen is cleared with Column 1   ณ
      *    ณ and 80 containing "ณ" and columns 2 to 79 containing      ณ
      *    ณ spaces.                                                   ณ
      *    ณ                                                           ณ
      *    ณ  eg.                                                      ณ
      *    ณ   MOVE 8         TO CRT-START.                            ณ
      *    ณ   MOVE 28        TO CRT-END.                              ณ
      *    ณ   PERFORM ERASE-SCREEN.                                   ณ
      *    ณ                                                           ณ
      *    ณ To clear the entire screen and Display a new screen with  ณ
      *    ณ headings (program/date/time/company):                     ณ
      *    ณ                                                           ณ
      *    ณ    CRT-PROGRAM (calling program)                          ณ
      *    ณ    CRT-HEADING (Screen heading)                           ณ
      *    ณ    CRT-COMPANY (Company name)                             ณ
      *    ณ    CRT-TERMINAL (consists of two fields:                  ณ
      *    ณ                  CRT-WRKHD and CRT-WRKST. See CRTHEAD)    ณ
      *    ณ    These fields should be populated at the start of the   ณ
      *    ณ    program.                                               ณ
      *    ณ    PERFORM HEADING-AND-CLEAR-SCREEN.                      ณ
      *    ณ                                                           ณ
      *    ณ To change the screen heading:                             ณ
      *    ณ    MOVE "The new heading" TO CRT-HEADING.                 ณ
      *    ณ    PERFORM CHANGE-HEADING.                                ณ
      *    ณ                                                           ณ
      *    ณ To change the time on the screen:                         ณ
      *    ณ    PERFORM CHANGE-TIME                                    ณ
      *    ณ                                                           ณ
      *    ณ To clear lines 46 and 50:                                 ณ
      *    ณ    PERFORM CLEAR-L46-AND-50                               ณ
      *    ณ                                                           ณ
      *    ณ To clear line 50:                                         ณ
      *    ณ    PERFORM CLEAR-L50                                      ณ
      *    ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

       COPY "CLEAR.CRT".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000              SECTION.

       COPY "CONTROL.RD".

       COPY "CREDIT.RD".

       COPY "CRDMEM.RD".

       COPY "MEMCRD.RD".

       COPY "PARAM.RD".

       COPY "SHARED.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AD000             SECTION.

       COPY "CONTROL.WR".

       COPY "CREDIT.WR".

       COPY "CRDMEM.WR".

       COPY "MEMCRD.WR".

       COPY "PARAM.WR".

      *COPY "CREDIT.LUP".

       COPY "APAC.HLP".

      *
      *       ****   *****   ***    ***   *   *  *****  ****   *   *
      *       *   *  *      *   *  *   *  *   *  *      *   *  *   *
      *       ****   ***    *      *   *  *   *  ***    ****    * *
      *       *   *  *      *   *  *   *   * *   *      *   *    *
      *       *   *  *****   ***    ***     *    *****  *   *    *
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      *          THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      *        ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
      *        ณ ORIGINAL ACTION (REC-TYPE)  ฤฤ  RECOVERY PROCESS ณ
      *        ณ 0 = RECORD CHANGED          ฤฤ     (REWRITE)     ณ
      *        ณ 1 = RECORD WAS ADDED        ฤฤ     (DELETE)      ณ
      *        ณ 2 = RECORD WAS DELETED      ฤฤ     (WRITE)       ณ
      *        ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
       AY000        SECTION.
       AY06.
             MOVE 06                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-CRDMEM-LOCK THRU READ-CRDMEM-EXIT.
             MOVE CME-RECORD1        TO REC-CRDMEM.
             GO TO AY50.
      *    *************************************************************
      *    ****               C O N T R O L   F I L E               ****
      *    *************************************************************
       AY39.
             MOVE 39                 TO REC-FILE.
             MOVE WS-NETKEY          TO REC-KEY.
             MOVE ZERO               TO REC-TYPE.
             MOVE NET-RECORD         TO REC-NETWORK.
             GO TO AY50.
      *    ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
      *    ณ              End transaction in recovery log              ณ
      *    ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
       AY40.
             MOVE 99                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE SPACES             TO REC-DETAIL.
             PERFORM AY50.
             ADD 1                   TO WS-TRANS.
      *    ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
      *    ณ If recovery records exceed 195 - Clear (Close and Open    ณ
      *    ณ new). This allows for quicker recoveries when required.   ณ
      *    ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
           IF WS-RECOVER > 195
               CLOSE RECOVER
               OPEN OUTPUT RECOVER
               CLOSE RECOVER
               OPEN I-O RECOVER
               MOVE ZERO             TO WS-RECOVER.
             GO TO AY59.
      *    ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
      *    ณ         W R I T E   R E C O V E R Y   R E C O R D         ณ
      *    ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
       AY50.
             ADD 1                   TO WS-RECOVER.
             MOVE WS-RECOVER         TO WS-RECKEY.
             MOVE WS-TRANS           TO REC-TRAN.
             WRITE REC-RECORD.
           IF WS-STATUS NOT = "00"
               DISPLAY "Write error Recovery file - Status " AT 2212 WITH FOREGROUND-COLOR Brown HIGHLIGHT
               DISPLAY WS-STATUS                             AT 2247 WITH FOREGROUND-COLOR Grey HIGHLIGHT
               STOP RUN.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
       AY59.
             EXIT.
      *
      *    ****    Start processing transaction
      *
       AY60.
             MOVE 1                  TO WS-COUNT.
             MOVE 5                  TO WS-SHARED.
             PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      *      MOVE SHR-STOCK          TO WS-STOCK.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-USERS < 24
               MOVE 1                TO WS-SUB
               MOVE ZERO             TO WS-WAIT
               GO TO AY62.
      *
      *    ****   Q   F U L L  -  W A I T   F O R   4   S E C O N D S
      *
             DISPLAY "WAITING" AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red HIGHLIGHT.
             COMMIT.
             ACCEPT WS-STIME FROM TIME.
             MOVE 400                TO WS-WAIT.
             PERFORM LOCK-REC-LOOP.
             DISPLAY SPACE AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.
             GO TO AY60.

       AY61.
      *
      *    ****    Updating Creditor Files
      *
             MOVE "CR"               TO PAR-PROG(WS-USUB).
             MOVE LS-USER            TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the CREDITOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
             MOVE 2                  TO WS-NETKEY.
             PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             PERFORM AY39 THRU AY59.
             GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N   P R O G R E S S
      *
       AY62.
           IF NOT(PAR-PROG(WS-SUB) = SPACES OR PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   C R E D I T O R   F I L E S   B E I N G   U P D A T E D
      *
               IF NOT(PAR-PROG(WS-SUB) = SPACES)
                   IF PAR-PROG(WS-SUB) = "CR" OR "CS" OR "CG"
      *
      *    ****   Y E S   -   S E T   W A I T   P E R I O D
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
      *
      *    ****    End transaction
      *
       AY70.
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 2.
      *
             PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
             PERFORM AY40 THRU AY59.
             MOVE 1                  TO WS-USUB.

       AY72.
           IF NOT(PAR-USR(WS-USUB) = LS-USER)
               ADD 1                 TO WS-USUB
               GO TO AY72.

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
             COMMIT.

       AY999.
             EXIT.
      *
      *    ****    Read/Write Creditor record
      *
      *    This routine is performed and the following are the variables
      *    that are required to have been set up.
      *
      *          VARIABLES USED FOR CRPRDWR as instructions and data
      *
      *    03  W15-INSTR             PIC  X(01).
      *
      *        88  ADD-ACCOUNT                     VALUE "A".
      *
      *            W15-CREDREC needs to contain details for the account 
      *                        that must be added
      *
      *        88  DELETE-ACT                      VALUE "D".
      *
      *            W15-CREDREC needs to contain the details for the 
      *                        account to be deleted
      *
      *        88  GET-ACCOUNT                     VALUE "G".
      *
      *            W15-LINCOL must have the screen location of where the
      *                       User will key in the account number. 
      *                            eg. 0617 = Line 6 Column 17.
      *
      *            W15-BGRND  The Background colour that must be used.
      *            W15-FGRND  The Foreground colour that must be used.
      *                       The folloeing values are available in the
      *                       COPY file "WS.WS" that is included in the
      *                       WORKING-STORAGE SECTION.
      *
      *                           78  Black     value 0.
      *                           78  Blue      value 1.
      *                           78  Green     value 2.
      *                           78  Cyan      value 3.
      *                           78  Red       value 4.
      *                           78  Magenta   value 5.
      *                           78  Brown     value 6.
      *                           78  Grey      value 7.
      *
      *                       If the screen colours for the calling 
      *                       program are Black on White(Grey) then
      *                       that would be setup as follows:
      *
      *                           MOVE Grey  TO W15-BGRND.
      *                           MOVE Black TO W15-FGRND.
      *
      *        88  READ-LOCK                       VALUE "L".
      *
      *            W15-ACNO must contain the Account Number of the 
      *                     account record to be read and locked.
      *
      *        88  READ-RECORD                     VALUE "R".
      *
      *            W15-ACNO must contain the Account Number of the 
      *                     account record to be read.
      *
      *        88  UPDATE-ACT                      VALUE "U".
      *
      *            W15-CREDREC needs to contain the details for the 
      *                        account record to be updated.
      *
      *        88  UPDATE-UNL                      VALUE "X".
      *
      *            W15-CREDREC must contain the the details for the  
      *                        account record to be updated and unlocked.
      *
      *    This variable informs the called program to write details of
      *    the recoosd to the RECOVERY file.
      * 
      *    03  W15-RECOVER.
      *        05  W15-RECV        PIC  X(01)      VALUE "N".
      *            88  ADD-TO-RECV                 VALUE "Y".
      * 
      *    If the record must be written to the RECOVERY file; the 
      *    calling program must MOVE "Y" TO W15-RECV before calling 
      *    CRPRDWR. CRPRDWR will always change it back to "N" before 
      *    returning control to the calling program.
      *
      *    When a read CREDITOR account has been requested WITH W15-RECV
      *    havinge a value of "N" then the program will exit with the
      *    account record in W15-CREDREC. If W15_RECV = "Y" then 
      *    W15-CREDREC remains unchanged.
      *
      *    On exiting W15-ERROR will be 0 if successful or contain and 
      *    error code for unsuccessful requests:
      *               0 = successful
      *               1 = No Record
      *               2 = account number on File not the same as requested
      *               9 = CRPRDWR called with incorrect instruction
      *
              
       COPY "CA155.CRP".

      *
      *    ****    C R E D I T O R   M E M O   M A I N T E N A N C E
      *
       CJ000        SECTION 5.
       CJ00.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
             DISPLAY S05.
             MOVE SPACES             TO W15-ACNO.

       CJ05.
             DISPLAY S55.
             ACCEPT S06A.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY MOVE SPACES    TO W15-ACNO
                              GO TO CJ999
                 WHEN F1-KEY  PERFORM HELP-ROUTINE
                 WHEN F2-KEY  PERFORM AA50-LOOKUP
                 WHEN OTHER   PERFORM AA900-ALARM
                              GO TO CJ05
               END-EVALUATE
               DISPLAY S06A
               IF W15-ACNO = SPACES
                   GO TO CJ05.
      *      MOVE 12                 TO CRT-START CRT-END.
      *      PERFORM ERASE-SCREEN.
             PERFORM CLEAR-L50.
           IF W15-ACNO = SPACES
               GO TO CJ999.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W15-ACNO BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             PERFORM CA155-GET-CREDITOR.
           IF WS-ERROR = 1
               MOVE WS-ERR2          TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CJ05.
           IF WS-ERROR = 2
               MOVE WS-ERR1          TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CJ05.
             MOVE P-REC              TO W15-CREDREC.

       CJ10.
             DISPLAY S06B.
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
      *        S U B S C R I P T S   A N D   P O I N T E R S
      *
      *        WS-S1   - Used for moving data to and from the Screen area
      *                  W200 to Work W205 or Work File MEM-DATA.
      *        WS-S2   - Current MEMO line on screen.
      *        WS-S6   - Used when moving date up or down in the work area.
      *        WS-S7   - Used to store W205-S1 when inserting or deleting
      *                  lines of data.
      *        WS-S8   - Used to store W205-S2 when inserting or deleting
      *                  lines of data.
      *        WS-S9   - Pointer for Work Area and Keys
      *        W205-S  - Pointing to the W205-MEM-DET entry that is the
      *                  first entry in the memo window on the screen.
      *        W205-S1 - Used as a pointer when moving data from or to
      *                  W205-MEM-DET and W200-MEM-FLDS (Screen).
      *        W205-S2 - Pointer to the current line in W205-MEM-DET
      *        W205-LAST - The last entry in the Work Area W205-MEM-DET
      *
      *
      *    ****   C H E C K   I F   T H E R E   I S   A N Y   D A T A
      *
             INITIALIZE W200-MEM-FLDS.
             MOVE ZERO               TO W205-LAST W205-S1 W205-S2.
             MOVE W15-ACNO           TO CME-AC.
             MOVE 1                  TO CME-SEQ.
             MOVE 1                  TO WS-FIRST WS-MEMPOS WS-LAST.
             PERFORM READ-CRDMEM THRU READ-CRDMEM-EXIT.
      *
      *    ****    I F  T H E R E   I S   N O   D A T A   B Y P A S S
      *
           IF WS-F-ERROR NOT = ZERO
               OPEN OUTPUT MEMCRD CLOSE MEMCRD OPEN I-O MEMCRD
               MOVE ZERO             TO WS-S9
               MOVE SPACES           TO W205-MEM-DET W200-KEYS
               MOVE 1                TO W205-S W205-S2 W200-SEQ1
               DISPLAY SD-SEQ AT LINE CLIN COLUMN 14
               GO TO CJ40.
      *
      *    ****   G E T   E X I S T I N G   M E M O   D A T A
      *
             PERFORM GET-CRDMEM-DETAILS.
             MOVE 1                  TO W205-S.
      *
      *    ****   S E T   S T A R T   P O S I T I O N   F O R
      *           D I S P L A Y   A N D   A C C E P T
      *
       CJ28.
           IF W205-S2 > W205-LAST
               MOVE W205-S2          TO W205-LAST.
             MOVE 1                  TO WS-S1.
             MOVE W205-S             TO W205-S1 W205-S2.
             INITIALIZE W200-MEM-FLDS.
      *
      *    ****   S T O R E   M E M O   D E T A I L S   I N
      *           D I S P L A Y   A N D   A C C E P T   A R E A
      *
       CJ30.
             MOVE W205-MEM(W205-S1)  TO W200-MEM(WS-S1).
             MOVE W200-SEQ(W205-S1)  TO W200-MSQ(WS-S1).
           IF WS-S1 > 33
               GO TO CJ40.
      *
      *    ****   C H E C K   F O R   E N D   O F   M E M O   D A T A
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
                                         MOVE W15-ACNO  TO W200-AC(W205-S2)
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
                   MOVE W15-ACNO     TO W200-AC(W205-S2)
                   MOVE W205-S2      TO W200-SEQ(W205-S2) W200-MSQ(WS-S2)
                   DISPLAY SD-SEQ AT LINE CLIN COLUMN 14
               END-IF
           ELSE
               PERFORM CP000
           END-IF.
             GO TO CJ50.

       CJ110.
             PERFORM RESTORE-SCREEN-3.
             CLOSE MEMCRD.
             GO TO CJ00.

       CJ999.
             EXIT.
      *
      *     M O V E   D A T A   F R O M   A C C E P T   A R E A   T O
      *     W205   A N D   W R I T E   T H E S E   D E T A I L S   T O   
      *     T H E   W O R K   F I L E
      *
       CK000        SECTION 5.
       CK00.
             MOVE W205-S             TO W205-S1.
             MOVE 1                  TO WS-S1.
             MOVE P-NUMBER           TO MEM-AC.

       CK05.
             MOVE W205-S1            TO MEM-SEQ.
             MOVE W200-MEM(WS-S1)    TO W205-MEM(W205-S1) MEM-DATA.
             PERFORM REWRITE-MEMCRD THRU WRITE-MEMCRD-EXIT.
           IF WS-F-ERROR = 62
               MOVE ZERO             TO WS-F-ERROR
               IF NOT(MEM-DATA = SPACES)
                   PERFORM WRITE-MEMCRD THRU WRITE-MEMCRD-EXIT.
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
             MOVE P-NUMBER           TO MEM-AC.
      *
      *     M O V E   D A T A   U P   I N   W O R K   A R E A   A N D
      *               I N   T H E   W O R K   F I L E
      *
       CL05.
             MOVE W205-S2            TO MEM-SEQ.
             MOVE W205-MEM(W205-S1)  TO W205-MEM(W205-S2) MEM-DATA.
             MOVE MEM-KEY            TO W200-KEY(W205-S2).
           IF W205-S2 = W205-LAST
               PERFORM WRITE-MEMCRD THRU WRITE-MEMCRD-EXIT
               IF WS-F-ERROR = 62
                   PERFORM REWRITE-MEMCRD THRU WRITE-MEMCRD-EXIT
                   MOVE ZERO         TO WS-F-ERROR
               END-IF
           ELSE
               PERFORM REWRITE-MEMCRD THRU WRITE-MEMCRD-EXIT.
           IF W205-S1 > WS-S8
               SUBTRACT 1            FROM W205-S1 W205-S2
               GO TO CL05. 
             MOVE SPACES             TO W205-MEM(W205-S1) MEM-DATA.
             MOVE W205-S1            TO MEM-SEQ.

       CL10.
             PERFORM REWRITE-MEMCRD THRU WRITE-MEMCRD-EXIT.
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
             MOVE P-NUMBER           TO MEM-AC.

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
             PERFORM REWRITE-MEMCRD THRU WRITE-MEMCRD-EXIT.
             MOVE SPACES             TO W205-MEM(W205-S1).
           IF W205-S1 < W205-LAST
               ADD 1                 TO W205-S1 W205-S2
               GO TO CM05.

       CM10.
             MOVE SPACES             TO MEM-DATA.
             MOVE W205-LAST          TO MEM-SEQ.
             PERFORM READ-MEMCRD THRU READ-MEMCRD-EXIT.
             PERFORM DELETE-MEMCRD-REC THRU WRITE-MEMCRD-EXIT.
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
             MOVE P-NUMBER           TO CME-AC.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      *             S T A R T   R E C O V E R Y   L O G G I N G
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
             PERFORM AY60 THRU AY999.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      *               U P D A T E   T H E   M E M O   F I L E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       CN10.
             MOVE W205-S1            TO CME-SEQ.
           IF CME-KEY = W200-KEY(WS-S6)
               MOVE 0                TO WS-ACTION
               PERFORM AY06 THRU AY59.
             MOVE W205-MEM(W205-S1)  TO CME-DATA.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      *              F I N D   K E Y   I N   K E Y   T A B L E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       CN15.
           IF CME-KEY < W200-KEY(WS-S6)
               GO TO CN20.
           IF CME-KEY = W200-KEY(WS-S6)
               GO TO CN25.
           IF WS-S6 < W205-LAST
               ADD 1                 TO WS-S6
               GO TO CN15.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      *                 A D D   N E W   M E M O  R E C O R D
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       CN20.
             PERFORM WRITE-CRDMEM THRU WRITE-CRDMEM-EXIT.
             MOVE 1                  TO WS-ACTION.
             PERFORM AY06 THRU AY59.
             GO TO CN30.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      *        U P D A T E   E X I S T I N G   M E M O  R E C O R D
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       CN25.
             PERFORM REWRITE-CRDMEM THRU WRITE-CRDMEM-EXIT.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      *         C L E A R   K E Y S   O F   U S E D   R E C O R D S
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
             MOVE SPACES             TO W200-KEY(WS-S6).

       CN30.
           IF W205-S1 < W205-LAST
               ADD 1                 TO W205-S1
               GO TO CN10.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      *      D E L E T E   R E D U N D A N T   R E C O R D S   F R O M
      *                         M E M O   F I L E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
             MOVE 1                  TO WS-S6.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      *         F I N D   R E D U N D A N T   K E Y S
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       CN35.
           IF NOT(W200-KEY(WS-S6) = SPACES)
               MOVE W200-KEY(WS-S6)  TO CME-KEY
               MOVE 2                TO WS-ACTION
               PERFORM AY06 THRU AY59
               PERFORM DELETE-CRDMEM-REC THRU WRITE-CRDMEM-EXIT.
           IF WS-S6 < WS-S9
               ADD 1                 TO WS-S6
               GO TO CN35.

       CN900.
             PERFORM AY70 THRU AY999.

       CN999.
             EXIT.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      *        M O V E   D A T A   D O W N   I N   W O R K   A R E A
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
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
      *    M O V E   D A T A   U P   1   L I N E   O N   T H E
      *    S C R E E N   A N D   G E T   T H E   N E X T   L I N E
      *    F R O M   T H E   W O R K   A R E A   W 2 0 5
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
      *    I S   T H E   L A S T   L I N E   I N   T H E   M E M O
      *    W I N D O W   A   B L A N K   L I N E ?
      *
           IF W200-MSQ(34) = ZERO
               SUBTRACT 1 FROM W205-S1 GIVING W200-MSQ(34).
             DISPLAY SD-MEM.

       CP999.
             EXIT.

      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      *      G E T   T H E   E X I S T I N G   M E M O  D E T A I L S
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       GET-CRDMEM-DETAILS SECTION 5.
       OPEN-MEMCRD.
             OPEN OUTPUT MEMCRD.
             CLOSE MEMCRD.
             OPEN I-O MEMCRD.
             MOVE ZERO               TO WS-S9.
             MOVE SPACES             TO W205-MEM-DET W200-KEYS.
             MOVE CME-AC             TO MEM-AC.

       WRITE-MEMO-DETAILS.
             MOVE CME-SEQ            TO MEM-SEQ WS-LAST.
             ADD 1                   TO WS-S9.
             MOVE CME-DATA           TO MEM-DATA W205-MEM(WS-S9).
             MOVE CME-KEY            TO W200-KEY(WS-S9).
             PERFORM WRITE-MEMCRD THRU WRITE-MEMCRD-EXIT.
             PERFORM READ-CRDMEM-NEXT THRU READ-CRDMEM-EXIT.
           IF WS-F-ERROR = ZERO
               IF CME-AC = W15-ACNO
                   GO TO WRITE-MEMO-DETAILS.
             MOVE WS-S9              TO W205-LAST.

       GET-CRDMEM-DETAILS-EXIT.
             EXIT.

      *    *************************************************************
      *                 I N I T I A L I S E   P R O G R A M
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
      *    ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
      *    ณ  Setup various keys and the mouse for use with menus and  ณ
      *    ณ  other required user interface functions and procedures.  ณ
      *    ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

             COPY "FUNCTION.PRO".

             OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.

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
               WHEN 2          MOVE W02-NETWORK TO WS-FILE
                               MOVE WS-NETKEY   TO WS-KEY
               WHEN 3          MOVE W02-CREDIT  TO WS-FILE
                               MOVE P-NUMBER    TO WS-KEYX
               WHEN 15         MOVE WS-PARID    TO WS-FILE
                               MOVE WS-PARKEY   TO WS-KEY
               WHEN 18         MOVE W02-RECOVER TO WS-FILE
                               MOVE WS-RECKEY   TO WS-KEY
               WHEN 24         MOVE W02-CRDMEM  TO WS-FILE
                               MOVE CME-KEY     TO WS-KEYX
               WHEN 37         MOVE W02-SHARED  TO WS-FILE
                               MOVE WS-SHARED   TO WS-KEY
               WHEN 62         MOVE W02-MEMCRD  TO WS-FILE
                               MOVE MEM-KEY     TO WS-KEYX
               WHEN OTHER      MOVE "ERROR"     TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
