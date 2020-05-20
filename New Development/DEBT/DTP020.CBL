      $SET LINKCOUNT"512" GNT"DTP020.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****     ******     ****     *
      *   **    **     **     **    **   **  **   **    **   **  **    *
      *   **    **     **     **    **  **    **       **   **    **   *
      *   **    **     **     *******   **    **     **     **    **   *
      *   **    **     **     **        **    **   **       **    **   *
      *   **    **     **     **         **  **   **         **  **    *
      *   *******      **     **          ****    ********    ****     *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *    D E B T O R   T R I A L   B A L A N C E   R E P O R T       *
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
      *  October 2008 - Amended Accounts receivable ageing to create   *
      *                 duplicate files of the accounts and trans-     *
      *                 actions for printing of copy satements when    *
      *                 and if required. The file names are AR_Ayymm   *
      *                 and AR_Tyymm, where yymm is replaced with      *
      *                 the year and month.                            *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     DTP020.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   April 1982.

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
       77  WS-CHECK                PIC  X(18)     VALUE "aeWlimemnomLalismJ".
       77  WS-PARKEY               PIC  9(04)     COMP-5.
       77  WS-NETKEY               PIC  9(06)     COMP-5.
       77  WS-NTP                  PIC  9(01).
       77  WS-LINES                PIC  9(02).
       77  WS-PAGE                 PIC  9(04)     COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-AGE                  PIC  X(01).
       77  WS-SEQ                  PIC  X(01).
       77  WS-CREDIT               PIC  X(01).
       77  WS-ZERO                 PIC  X(01).
       77  WS-NUM                  PIC  Z9.
       77  WS-ADDR                 PIC  X(01).
       77  WS-ERROR                PIC  9(01).
       77  WS-ERROR1               PIC  9(01).
       77  WS-PGE-LENGTH           PIC  9(02)     VALUE 66.
       77  WS-PRN-LENGTH           PIC  9(02)     VALUE 62.
       77  WS-EXPAND               PIC  X(01)     VALUE " ".
       77  WS-PRN-NO               PIC  X(01)     COMP-X VALUE 9.
       77  WS-PRN-STAT             PIC  X(01)     COMP-X.
       77  WS-COMP                 PIC  9(01).
       77  WS-STM                  PIC  9(01).
       77  WS-BDT                  PIC  9(01).
       77  WS-BAD                  PIC  X(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-PERIOD               PIC  X(01).
           88  PREVIOUS-PERIOD                    VALUE "P".
       77  WS-MESS                 PIC  X(02).
       77  WS-ACNO                 PIC  X(06)     VALUE SPACES.
       77  WS-S1                   PIC  9(04)     COMP-5.
       77  WS-S2                   PIC  9(04)     COMP-5.
       77  WS-S3                   PIC  9(04)     COMP-5.
       77  WS-S4                   PIC  9(04)     COMP-5.
       77  WS-S5                   PIC  9(04)     COMP-5.
       77  WS-S6                   PIC  9(04)     COMP-5.
       77  WS-S7                   PIC  9(04)     COMP-5.
       77  WS-S8                   PIC  9(04)     COMP-5.
       77  WS-IXS1                 PIC  9(04)     COMP-5.
       77  WS-ERR1                 PIC  X(22)     VALUE "Invalid account number".
       77  WS-ERR2                 PIC  X(09)     VALUE "No record".
       77  WS-A                    PIC  X(19)     VALUE "Any key to continue".
       77  WS-AC                   PIC  X(07)     VALUE "Account".
       77  PRG-DEBT-LUP            PIC  X(12)     VALUE "DTP\DTPLOOK".
       77  TODAY-DDMMYY            PIC  9(08)     COMP-5.
       77  WS-USUB                 PIC  9(04)     COMP-5.
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "DTP020".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "ACCOUNTS RECEIVABLE - TRIAL BALANCE".
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

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-PRINTER-DETAILS.
           03  W02-PRINTER         PIC  X(12).
           03  W02-PGE-LENGTH      PIC  9(03).
           03  W02-PRN-LENGTH      PIC  9(03).
           03  W02-LINAGE          PIC  9(03).
           03  W02-PRN-STATUS      PIC  X(01)     VALUE "C".
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

          03  REP-LINE1                           REDEFINES W02-PRN-LINE.
              05  REP-DETAIL1      PIC  X(136).

          03  TRL-LINE1                           REDEFINES W02-PRN-LINE.
              05  TRL-DET          PIC  X(132).

          03  TRL-LINE2                           REDEFINES W02-PRN-LINE.
              05  TRL-HEAD1        PIC  X(07).
              05  TRL-HEAD2        PIC  X(34).
              05  TRL-HEAD3        PIC  X(08).
              05  TRL-HEAD4        PIC  X(06).
              05  TRL-HEAD5        PIC  X(11).
              05  TRL-HEAD6        PIC  X(11).
              05  TRL-HEAD7        PIC  X(09).
              05  TRL-HEAD8        PIC  X(09).
              05  TRL-HEAD9        PIC  X(09).
              05  TRL-HEAD10       PIC  X(10).
              05  TRL-HEAD11       PIC  X(10).
              05  TRL-HEAD12       PIC  X(08).

          03  TRL-LINE3                           REDEFINES W02-PRN-LINE.
              05  TRL-AC           PIC  X(06).
              05  FILLER           PIC  X(01).
              05  TRL-NME          PIC  X(31).
              05  FILLER           PIC  X(01).
              05  TRL-ACT          PIC  Z9/99/9999.
              05  FILLER           PIC  X(01).
              05  TRL-MTH          PIC  9(01).
              05  FILLER           PIC  X(02).
              05  TRL-CUR          PIC  Z(05)9.99-.
              05  FILLER           PIC  X(01).
              05  TRL-30           PIC  Z(05)9.99-.
              05  FILLER           PIC  X(01).
              05  TRL-60           PIC  Z(05)9.99.
              05  FILLER           PIC  X(01).
              05  TRL-90           PIC  Z(05)9.99.
              05  FILLER           PIC  X(01).
              05  TRL-INT          PIC  Z(03)9.99.
              05  FILLER           PIC  X(01).
              05  TRL-PD           PIC  Z(05)9.99-.
              05  FILLER           PIC  X(01).
              05  TRL-LIMIT        PIC  Z(05)9.
              05  TRL-EXL          PIC  X(01).
              05  TRL-BAL          PIC  Z(06)9.99-.

          03  TRL-LINE4                           REDEFINES W02-PRN-LINE.
              05  FILLER           PIC  X(33).
              05  TRL-HEADER       PIC  X(13).
              05  FILLER           PIC  X(34).

          03  TRL-LINE5                           REDEFINES W02-PRN-LINE.
              05  FILLER           PIC  X(10).
              05  TRL-SUM          PIC  X(20).
              05  TRL-VAL          PIC  Z(06)9.99-.
              05  FILLER           PIC  X(39).

          03  TRL-LINE6                           REDEFINES W02-PRN-LINE.
              05  TRL-PAGE         PIC  X(06).
              05  TRL-P-NO         PIC  Z(03)9.
              05  FILLER           PIC  X(24).
              05  TRL-RESTART.
                  07  TRL-COMPANY  PIC  X(40).
                  07  TRL-HEAD     PIC  X(24).
              05  FILLER           PIC  X(18).
              05  TRL-DATE         PIC  X(06).
              05  TRL-DTE          PIC  Z9/99/9999.

          03  TRL-LINE6A                          REDEFINES W02-PRN-LINE.
              05  TRL-EXPP         PIC  X(06).
              05  FILLER           PIC  X(05).
              05  TRL-CO           PIC  X(30).
              05  TRL-EXHEAD       PIC  X(24).
              05  TRL-EXPC         PIC  X(07).

          03  TRL-LINE7                           REDEFINES W02-PRN-LINE.
              05  FILLER           PIC  X(07).
              05  TRL-PCDE         PIC  9(04).
              05  FILLER           PIC  X(02).
              05  TRL-HEAD13       PIC  X(10).
              05  TRL-HTEL         PIC  X(14).
              05  FILLER           PIC  X(02).
              05  TRL-BTEL         PIC  X(14).
              05  FILLER           PIC  X(79).

       01  W05-DEB-HISTORY.
           03  W05-DBT-HIS.
               05  W05-DBT         PIC  X(04)     VALUE "AR_T".
               05  W05-TYY         PIC  9(02).
               05  W05-TMM         PIC  9(02).
               05  FILLER          PIC  X(04)     VALUE ".DAT".
           03  W05-DEB-HIS.
               05  W05-DEB         PIC  X(04)     VALUE "AR_A".
               05  W05-DEBH-ID.
                   07  W05-DYY     PIC  9(02).
                   07  W05-DMM     PIC  9(02).
               05  W05-HIST-ID     PIC  9(04)     REDEFINES W05-DEBH-ID.
               05  FILLER          PIC  X(04)     VALUE ".DAT".

       01  W05-LIST.
           03  W05-OPT             PIC  9(03).
           03  W05-TYPES                          REDEFINES W05-OPT.
               05  W05-COMP        PIC  9(01).
               05  W05-INDV        PIC  9(01).
               05  W05-INT         PIC  9(01).

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
           03  W25-RESULT          PIC  9(05)V99.
           03  W25-RESULT1                        REDEFINES W25-RESULT.
               05  W25-WHOLE       PIC  9(05).
               05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2                        REDEFINES W25-RESULT.
               05  W25-KEY         PIC  9(04).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).
           03  W25-TOTAL           PIC S9(07)V99 COMP-3.
           03  W25-VALUE           PIC S9(07)V99 COMP-3.

       COPY "W40.DBT".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

      *COPY "W50.WS".

       01  W60-NME-ADD.
           03  W60-NAME. 
               05  W60-NCHAR       PIC  X(01)     OCCURS 40.
           03  W60-ADDRESS.
               05  W60-ACHAR       PIC  X(01)     OCCURS 60.

       01  W75-KEYS.
           03  W75-S               PIC  9(02)     COMP-5.
           03  W75-S1              PIC  9(02)     COMP-5.
           03  W75-DNO             PIC  X(06)     OCCURS 18.

       COPY "W75.WS".

       01  W80-ACCOUNT.
           03  W80-ACNO            PIC  X(06).
           03  W80-NUMD                           REDEFINES W80-ACNO.
               05  W80-SERIAL      PIC  9(06).
           03  W80-TYPE            PIC  9(01).

       01  W90-TRAN.
           03  W90-DATE            PIC  X(10).
           03  W90-DTE             PIC  99/99/9999 REDEFINES W90-DATE.
           03  W90-CODE            PIC  9(02).
           03  W90-REF             PIC  X(08).
           03  W90-DB              PIC S9(07)V99   COMP-3.
           03  W90-CR              PIC S9(07)V99   COMP-3.
           03  W90-BDT             PIC S9(07)V99   COMP-3.
           03  W90-BCR             PIC S9(07)V99   COMP-3.
           03  W90-DESC            PIC  X(12).
           03  W90-120             PIC S9(07)V99   COMP-3.
           03  W90-90              PIC S9(07)V99   COMP-3.
           03  W90-60              PIC S9(07)V99   COMP-3.
           03  W90-30              PIC S9(07)V99   COMP-3.
           03  W90-CUR             PIC S9(07)V99   COMP-3.
           03  W90-MTD             PIC S9(07)V99   COMP-3.
           03  W90-BAL             PIC S9(07)V99   COMP-3.
           03  W90-IN              PIC  9(04)      COMP-5.
           03  W90-IV              PIC S9(07)V99   COMP-3.
           03  W90-RN              PIC  9(04)      COMP-5.
           03  W90-RV              PIC S9(07)V99   COMP-3.
           03  W90-DN              PIC  9(04)      COMP-5.
           03  W90-DV              PIC S9(07)V99   COMP-3.
           03  W90-CN              PIC  9(04)      COMP-5.
           03  W90-CV              PIC S9(07)V99   COMP-3.
           03  W90-INT             PIC S9(07)V99   COMP-3.
           03  W90-RESULT          PIC  9(04)      COMP-5.
           03  W90-CATKEY          PIC  9(04)      COMP-5.
           03  W90-CS              PIC  9(02)      COMP-5.

       01  W95-STM.
           03  W95-COMP        PIC X(40).

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

       01  S15.
           03  LINE 10 COLUMN 12 VALUE "Do you want the Debtor address printed (".
           03          COLUMN 52 VALUE                                         "Y"   FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 53 VALUE                                          "/".
           03          COLUMN 54 VALUE                                           "N" FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 55 VALUE                                            ")".

       01  S15A.
           03  LINE 12 COLUMN 12 VALUE "Selective print for credit control (".
           03          COLUMN 48 VALUE                                     "Y"   FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 49 VALUE                                      "/".
           03          COLUMN 50 VALUE                                       "N" FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 51 VALUE                                        ")".

       01  S15B.
           03  LINE 14 COLUMN 12 VALUE "Select ".
           03          COLUMN 19 VALUE        "1"                          WITH FOREGROUND-COLOR Brown HIGHLIGHT. 
           03          COLUMN 20 VALUE         " = 60d, ".
           03          COLUMN 28 VALUE                 "2"                 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 29 VALUE                  " = 90d, ".
           03          COLUMN 37 VALUE                          "3"        WITH FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 38 VALUE                           " = >60d".
       
       01  S15C.
           03  LINE 16 COLUMN 12 VALUE "Print accounts with zero balance (".
           03          COLUMN 46 VALUE                                   "Y"    WITH FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 47 VALUE                                    "/".
           03          COLUMN 48 VALUE                                     "N"  WITH FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 49 VALUE                                      ")".

       01  S15D.
           03  LINE 18 COLUMN 12 VALUE "Is this a restart (".
           03          COLUMN 31 VALUE                    "Y"     WITH FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 32 VALUE                     "/".
           03          COLUMN 33 VALUE                      "N"   WITH FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 34 VALUE                       ")".
           
       COPY "DEBLUP.CRT".

       01  S80-YEAR.
           03  LINE 12 COLUMN 40 PIC  Z9 USING W05-DYY FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.
       01  S80-MONTH.
           03  LINE 13 COLUMN 40 PIC  Z9 USING W05-DMM FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.

      *
      *    *******   *******    ******    ******   ********  *******   **     **  *******    ******** 
      *    **    **  **    **  **    **  **    **  **        **    **  **     **  **    **   **
      *    **    **  **   **   **    **  **        **        **    **  **     **  **   **    **
      *    *******   ******    **    **  **        *****     **    **  **     **  ******     *****
      *    **        **   **   **    **  **        **        **    **  **     **  **   **    **
      *    **        **    **  **    **  **    **  **        **    **  **     **  **    **   **
      *    **        **    **   ******    ******   ********  *******    *******   **    **   ********
      *
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS.
       AA00-MAIN       SECTION.
       AA00-INIT.
           IF LS0-DBLEV < 3
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
               CLOSE DBHIST.

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

      *
      *    ****    READ FILES ROUTINES
      *
       AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "DBHIST.RD".

       COPY "DEBTOR.RD1".

       COPY "DEBTOR.LUP".

      *
      *    ****    Read debtor record - using account number.
      *
       CA155-GET-DEBTOR        SECTION 2.
       CA155-ACCOUNT.
           IF PREVIOUS-PERIOD
               MOVE W80-ACNO         TO DBH-ACNO
               PERFORM READ-DBHIST THRU READ-DBHIST-EXIT
               MOVE DBH-RECORD1      TO DEB-RECORD1
           ELSE
               MOVE W80-ACNO         TO DEB-ACNO
               PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6 OR 56
               MOVE 1                TO WS-ERROR
           ELSE
           IF DEB-ACNO NOT = W80-ACNO
               MOVE 2                TO WS-ERROR
           ELSE
               MOVE 0                TO WS-ERROR
               MOVE DEB-TYPE         TO W80-TYPE.

       CA160-EXIT.
            EXIT.

      /
      *    ****    T R I A L   B A L A N C E
      *
       XA000        SECTION 5.

       XA00.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
             MOVE ZERO               TO WS-BDT.
             MOVE "N"                TO WS-BAD.
             MOVE "Are you sure Y/N" TO WS-OPT-MES.
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
          IF WS-OPTION = "N"
               GO TO XA999-EXIT.
      *
      *    Do you want the Debtor address printed
      *
             DISPLAY S15.
             MOVE "N"                TO WS-OPTION.
             MOVE 10                 TO WS-MES-LINE.
             MOVE 57                 TO WS-MES-COL.
      *
      *      ACCEPT the option at the position specified above
      *
             MOVE X"FA"              TO WS-MES-CHAR(48).
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
             MOVE WS-OPTION          TO WS-ADDR.
      *
      *    Selective print for credit control
      *
             DISPLAY S15A.
             MOVE "N"                TO WS-OPTION.
             MOVE 12                 TO WS-MES-LINE.
             MOVE 53                 TO WS-MES-COL.
      *
      *      ACCEPT the option at the position specified above
      *
             MOVE X"FA"              TO WS-MES-CHAR(48).
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
             MOVE SPACE              TO WS-CREDIT.
             MOVE "N"                TO WS-ZERO.
           IF WS-OPTION = "Y"
               GO TO XA75.
       XA05.
      *
      *    Print accounts with zero balance
      *
             DISPLAY S15C.
             MOVE "N"                TO WS-OPTION.
             MOVE 16                 TO WS-MES-LINE.
             MOVE 51                 TO WS-MES-COL.
      *
      *      ACCEPT the option at the position specified above
      *
             MOVE X"FA"              TO WS-MES-CHAR(48).
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
             MOVE WS-OPTION          TO WS-ZERO.
             MOVE 0                  TO WS-PAGE W90-BAL W90-CUR W90-30 W90-60 W90-90 W90-120 W90-BDT W90-BCR W90-INT.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *     **  *   *   * *   *  *  **  *   *   *     *   *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *     *  **   *     *   *  *  *  **   *   *     *  *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** *   *
      *
             PERFORM OPEN-PRINTER.
             MOVE "P"                TO WS-COMMAND.
           IF NOT(WS-PRINTER = "S")
               MOVE 0                TO WS-ADVANCE
               MOVE 3                TO W02-PRN-TYPE
               PERFORM CALL-PRINTUTL.

       XA20.
             MOVE "H"                TO W02-PRN-TYPE.
             MOVE SPACES             TO REP-DETAIL1.
             MOVE "PAGE:"            TO TRL-PAGE.
             MOVE "DATE:"            TO TRL-DATE.
             ADD 1                   TO WS-PAGE.
             MOVE W12-TODAY          TO TRL-DTE.
             MOVE WS-PAGE            TO TRL-P-NO.
             MOVE 0                  TO WS-ADVANCE.
      *
      *    ****   T E S T   I F   O U T P U T   T O   P R I N T E R
      *                     O R   S P O O L   F I L E
      *
           IF NOT(WS-PRINTER = "S")
               PERFORM CALL-PRINTUTL
               MOVE SPACES           TO TRL-LINE1
               MOVE "E"              TO W02-PRN-TYPE
               PERFORM CALL-PRINTUTL.
             MOVE W95-COMP           TO TRL-CO.
             MOVE " - TRIAL BALANCE" TO TRL-EXHEAD.
             MOVE "H"                TO W02-PRN-TYPE.
      *
      *    ****   T E S T   I F   O U T P U T   T O   P R I N T E R
      *                     O R   S P O O L   F I L E
      *
           IF NOT(WS-PRINTER = "S")
               MOVE 0                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO TRL-LINE1.
           IF NOT(WS-PRINTER = "S")
               MOVE 0                TO WS-ADVANCE
               MOVE "X"              TO W02-PRN-TYPE
               PERFORM CALL-PRINTUTL
               MOVE 3                TO W02-PRN-TYPE
               PERFORM CALL-PRINTUTL.
             MOVE "H"                TO W02-PRN-TYPE.
           IF WS-BDT = 1
               MOVE SPACES           TO TRL-DET
               MOVE 2                TO WS-ADVANCE
               MOVE "B A D   A N D   D O U B T F U L L   D E B T S" TO TRL-RESTART
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO TRL-DET.
             MOVE 2                  TO WS-ADVANCE.

       XA25.
             MOVE "AC-NO."           TO TRL-HEAD1.
             MOVE "NAME"             TO TRL-HEAD2.
             MOVE "ACTIVE"           TO TRL-HEAD3.
             MOVE "MTH"              TO TRL-HEAD4.
           IF WS-AGE = "Y"
               MOVE "30 DAYS"        TO TRL-HEAD5
               MOVE "60 DAYS"        TO TRL-HEAD6
               MOVE "90 DAYS"        TO TRL-HEAD7
               MOVE "120 DAYS"       TO TRL-HEAD8
           ELSE
               MOVE "CURRENT"        TO TRL-HEAD5
               MOVE "30 DAYS"        TO TRL-HEAD6
               MOVE "60 DAYS"        TO TRL-HEAD7
               MOVE " 90 DAYS"       TO TRL-HEAD8.
             MOVE "INTEREST"         TO TRL-HEAD9.
             MOVE "PAST DUE"         TO TRL-HEAD10.
             MOVE "C-LIMIT"          TO TRL-HEAD11.
             MOVE "BALANCE"          TO TRL-HEAD12.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO TRL-DET.
             MOVE "D"                TO W02-PRN-TYPE.

       XA30.
           IF WS-BDT = 1
               DISPLAY "Busy with Bad & Doubtfull Debts" AT 1812
               GO TO XA45.
      *
      *    ****   T E S T   I F   O U T P U T   T O   S C R E E N
      *
           IF WS-PRINTER = "S"
               GO TO XA45.
      *
      *    Is this a restart
      *
             DISPLAY S15D.
             MOVE "N"                TO WS-OPTION.
             MOVE 18                 TO WS-MES-LINE.
             MOVE 36                 TO WS-MES-COL.
      *
      *      ACCEPT the option at the position specified above
      *
             MOVE X"FA"              TO WS-MES-CHAR(48).
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
             MOVE SPACES             TO WS-ACNO.
           IF WS-OPTION = "N"
               GO TO XA45.

       XA40.
             MOVE 20                 TO CRT-START.
             MOVE 24                 TO CRT-END.
             PERFORM ERASE-SCREEN.
             DISPLAY "Account No:" AT 2012.
             ACCEPT W80-ACNO AT 2024 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.
           IF W80-ACNO = SPACES
               GO TO XA45.
             PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR NOT = ZERO
               MOVE "Invalid account number" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO XA40.
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
             DISPLAY W40-DSP-NAME AT 2030 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO XA40.
             MOVE W80-ACNO           TO WS-ACNO.
             MOVE "RESTART  RESTART  RESTART  RESTART RESTART RESTART" TO TRL-RESTART.
            MOVE 2                   TO WS-ADVANCE.
            PERFORM CALL-PRINTUTL.
            MOVE SPACES              TO TRL-DET.

       XA45.
             DISPLAY "Debtor account:" AT 2212.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     Start reading from the first account in the file.     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF PREVIOUS-PERIOD
               MOVE " !"             TO DBH-NAME
               PERFORM START-AT-DEBH-NAME THRU READ-DBHIST-EXIT
           ELSE
               MOVE " !"             TO DEB-NAME
               PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6 OR 56
               GO TO XA70.

       XA50.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****" TO REP-DETAIL1
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               GO TO XA999-EXIT.
           IF PREVIOUS-PERIOD
               PERFORM READ-DBHIST-NEXT THRU READ-DBHIST-EXIT
               MOVE DBH-RECORD1      TO  DEB-RECORD1
           ELSE
              PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
          IF WS-F-ERROR = 6 OR 56
              GO TO XA65.
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
      *
      *    ****   T E S T   I F   O U T P U T   T O   P R I N T E R
      *                     O R   S P O O L  F I L E
      *
           IF NOT(WS-PRINTER = "S")
               DISPLAY DEB-ACNO AT 2228 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
           IF WS-ACNO = DEB-ACNO
               MOVE SPACES           TO WS-ACNO.
           IF WS-ZERO = "N"
               IF DEB-BAL = ZERO
                   GO TO XA50.
           IF DEB-BDT = 1
               IF WS-BDT = 0
                   MOVE "Y"          TO WS-BAD
                   GO TO XA50.
           IF NOT(DEB-BDT = 1)
               IF WS-BDT = 1
                   GO TO XA50.
             PERFORM XA80 THRU XA85.
           IF WS-ERROR1 = 1
               GO TO XA50.
             MOVE DEB-ACNO           TO TRL-AC.
             MOVE W40-DSP-NAME       TO TRL-NME.
             MOVE DEB-ACTIVE         TO W22-DTE2.
             PERFORM SWITCH-DATE-TO-DMCY.
             MOVE W22-DTE3           TO TRL-ACT.
             ADD DEB-30 DEB-60 DEB-90 DEB-120 GIVING W25-VALUE.
             ADD DEB-BAL             TO W90-BAL.
             ADD DEB-CUR             TO W90-CUR.
             ADD DEB-30              TO W90-30.
             ADD DEB-60              TO W90-60.
             ADD DEB-90              TO W90-90.
             ADD DEB-120             TO W90-90.
             ADD DEB-INT             TO W90-INT.
           IF DEB-BAL > 0
               ADD DEB-BAL           TO W90-BDT
           ELSE
               ADD DEB-BAL           TO W90-BCR.
      *    IF DEB-BAL > 0
             SUBTRACT W25-VALUE FROM DEB-BAL GIVING W25-TOTAL.
             SUBTRACT DEB-CUR        FROM W25-TOTAL.
             ADD W25-TOTAL           TO W90-CUR.
             MOVE W25-VALUE          TO TRL-PD.
             MOVE DEB-BAL            TO TRL-BAL.
           IF (DEB-90 > 0) OR (DEB-120 > 0)
               MOVE 3                TO TRL-MTH
           ELSE
           IF DEB-60 > 0
               MOVE 2                TO TRL-MTH
           ELSE
           IF DEB-30 > 0
               MOVE 1                TO TRL-MTH.
             COMPUTE TRL-CUR = DEB-BAL - (DEB-INT + DEB-30 + DEB-60 + DEB-90 +DEB-120)
             MOVE DEB-30             TO TRL-30.
             MOVE DEB-60             TO TRL-60.
             ADD DEB-90 DEB-120      GIVING TRL-90.
             MOVE DEB-INT            TO TRL-INT.
             MOVE DEB-CRL            TO TRL-LIMIT.
           IF DEB-CRL > ZERO
               IF DEB-BAL > DEB-CRL
                   MOVE "*"          TO TRL-EXL.
           IF WS-ACNO NOT = SPACES
               GO TO XA50.
           IF WS-ADDR = "Y"
               GO TO XA55.
           IF W02-LINAGE < W02-PRN-LENGTH
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM XA20 THRU XA25.
             MOVE SPACES             TO TRL-DET.
             GO TO XA50.

       XA55.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO TRL-DET.
             MOVE "P"                TO W40-INSTR.
             CALL "DTP\DTPADDR" USING W40-COMPANY.
             MOVE W40-ADD-1          TO TRL-NME.
             PERFORM CALL-PRINTUTL.
           IF W40-ADD-2 = SPACES
               GO TO XA60.
             MOVE W40-ADD-2          TO TRL-NME.
             PERFORM CALL-PRINTUTL.
          IF W40-ADD-3 = SPACES
               GO TO XA60.
             MOVE W40-ADD-3          TO TRL-NME.
             PERFORM CALL-PRINTUTL.
          IF W40-ADD-4 = SPACES
               GO TO XA60.
             MOVE W40-ADD-2          TO TRL-NME.
             PERFORM CALL-PRINTUTL.

       XA60.
             MOVE SPACES             TO TRL-DET.
             MOVE DEB-POST           TO TRL-PCDE.
             MOVE "TEL NO'S:"        TO TRL-HEAD13.
             MOVE DEB-H-TEL          TO TRL-HTEL.
             MOVE DEB-B-TEL          TO TRL-BTEL.
           IF W02-LINAGE < (W02-PRN-LENGTH - 4)
               MOVE 2                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM XA20 THRU XA25.
             MOVE SPACES             TO TRL-DET.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****" TO REP-DETAIL1
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               GO TO XA999-EXIT.
             GO TO XA50.

       XA65.
             MOVE "S"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             MOVE "P"                TO WS-COMMAND.
             PERFORM XA20.
             MOVE "T/B SUMMARY"      TO TRL-HEADER.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO TRL-DET.
             MOVE "GRAND TOTALS"     TO TRL-SUM.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
           IF WS-AGE = "Y"
               MOVE "30 DAYS"        TO TRL-SUM
           ELSE
               MOVE "CURRENT"        TO TRL-SUM.
             MOVE W90-CUR            TO TRL-VAL.
             PERFORM CALL-PRINTUTL.
           IF WS-AGE = "Y"
               MOVE "60 DAYS"        TO TRL-SUM
           ELSE
               MOVE "30 DAYS"        TO TRL-SUM.
             MOVE W90-30             TO TRL-VAL.
             PERFORM CALL-PRINTUTL.
           IF WS-AGE = "Y"
               MOVE "90 DAYS"        TO TRL-SUM
           ELSE
               MOVE "60 DAYS"        TO TRL-SUM.
             MOVE W90-60             TO TRL-VAL.
             PERFORM CALL-PRINTUTL.
           IF WS-AGE = "Y"
               MOVE "120 DAYS"       TO TRL-SUM
           ELSE
               MOVE "90 DAYS"        TO TRL-SUM.
             MOVE W90-90             TO TRL-VAL.
             PERFORM CALL-PRINTUTL.
             MOVE "INTEREST"         TO TRL-SUM.
             MOVE W90-INT            TO TRL-VAL.
             PERFORM CALL-PRINTUTL.
             MOVE "CREDIT BALANCES"  TO TRL-SUM.
             MOVE W90-BCR  TO TRL-VAL.
             PERFORM CALL-PRINTUTL.
             MOVE "DEBIT BALANCES"   TO TRL-SUM.
             MOVE W90-BDT            TO TRL-VAL.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE "DEBTOR TOTAL"     TO TRL-SUM.
             MOVE W90-BAL            TO TRL-VAL.
             MOVE "E"                TO WS-COMMAND.
             MOVE 99                 TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
           IF WS-BDT = 1
               GO TO XA70.
           IF WS-BAD = "N"
               GO TO XA70.
             MOVE 1                  TO WS-BDT.
             MOVE 0                  TO W90-BAL W90-CUR W90-30  W90-60 W90-90 W90-120 W90-BDT W90-BCR W90-INT.
             GO TO XA20.

       XA70.
             MOVE "C"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             GO TO XA999-EXIT.
      *
      *    Select Period to report
      *
       XA75.
             DISPLAY S15B.
             MOVE "N"                TO WS-OPTION.
             MOVE 14                 TO WS-MES-LINE.
             MOVE 46                 TO WS-MES-COL.
      *
      *      ACCEPT the option at the position specified above
      *
             MOVE X"FA"              TO WS-MES-CHAR(48).
             MOVE 0                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
             MOVE WS-OPTION          TO WS-CREDIT.
             GO TO XA05.

       XA80.
           IF NOT(WS-CREDIT = "1" OR "2" OR "3")
               MOVE ZERO             TO WS-ERROR1
               GO TO XA85.
           IF WS-CREDIT = "1"
               IF DEB-60 NOT = ZERO
                   MOVE ZERO         TO WS-ERROR1
                   GO TO XA85
               ELSE
                   MOVE 1            TO WS-ERROR1
                   GO TO XA85.
           IF WS-CREDIT = "2"
               IF (DEB-90 NOT = ZERO) OR (DEB-120 NOT = ZERO)
                   MOVE ZERO         TO WS-ERROR1
                   GO TO XA85
               ELSE
                   MOVE 1            TO WS-ERROR1
                   GO TO XA85.
           IF (DEB-60 NOT = ZERO) OR (DEB-90 NOT = ZERO) OR (DEB-120 NOT = ZERO)
               MOVE ZERO             TO WS-ERROR1
           ELSE
               MOVE 1                TO WS-ERROR1.

       XA85.
             EXIT.

       XA999-EXIT.
             EXIT.
      /
       YA000        SECTION 8.
       YA00.
      *       PERFORM ERASE-SCREEN.
      *       DISPLAY "DEBTORS (ACCOUNTS RECEIVABLE) TRIAL BALANCE" AT 0319 WITH FOREGROUND-COLOR Grey HIGHLIGHT.

       YA05.
             MOVE "'P'revious or 'C'urrent period  [C]" TO WS-OPT-MES.
             MOVE "C"                TO WS-OPTION.
             MOVE "P"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
             MOVE WS-OPTION          TO WS-PERIOD.
           IF NOT PREVIOUS-PERIOD
               GO TO YA999.

       YA10.
      *      PERFORM ERASE-SCREEN.
      *      DISPLAY "DEBTORS (A/R) PREVIOUS TRIAL BALANCE" AT 0323  WITH FOREGROUND-COLOR Grey HIGHLIGHT
             MOVE "DEBTORS (A/R) PREVIOUS TRIAL BALANCE" TO CRT-HEADING.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *
      *         ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *         ³ PREVIOUS PERIOD  ³²²
      *         ³ Year : YY (00-99)³²²
      *         ³ Month: MM (01-12)³²²
      *         ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ²²
      *           ²²²²²²²²²²²²²²²²²²²²
      *

             DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"  AT 1031 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Magenta HIGHLIGHT
                                        "¿"         WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Black.
             DISPLAY "³ "                   AT 1131 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Magenta HIGHLIGHT
                       "PREVIOUS PERIOD  "          WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey    HIGHLIGHT   
                                        "³"         WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Black.
             DISPLAY "³ "                   AT 1231 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Magenta HIGHLIGHT
                       "Year :    (00-99)"          WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Green            
                                        "³"         WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Black.
             DISPLAY "³ "                   AT 1331 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Magenta HIGHLIGHT
                       "Month:    (01-12)"          WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Green            
                                        "³"         WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Black.
             DISPLAY "À"                    AT 1431 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Magenta HIGHLIGHT
                      "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"         WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Black.
             MOVE 11                 TO SHADE-ROW.
             MOVE 33                 TO SHADE-COL.
             MOVE 18                 TO SHADE-WIDTH.
             MOVE 4                  TO SHADE-LINES.
             PERFORM SCREEN-SHADOW.
             MOVE W12-YEAR           TO W05-DYY.
             MOVE W12-MONTH          TO W05-DMM.
      *    ************************************************************
      *    ****    Y E A R   O F   P R E V I O U S   P E R I O D
      *    ************************************************************
       YA15.
             ACCEPT S80-YEAR.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE "C"  TO WS-PERIOD
                                 GO TO YA00
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO YA15
               END-EVALUATE.
           IF W05-DYY > W12-YEAR
               MOVE "Year can not be > Current Year" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO YA15.
      *    ************************************************************
      *    ***    M O N T H   O F   P R E V I O U S   P E R I O D
      *    ************************************************************
       YA20.
             ACCEPT S80-MONTH.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    GO TO YA15
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO YA20
               END-EVALUATE.
           IF W05-DYY > W12-YEAR
               MOVE "Month can not be < 01 or > 12" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO YA20.
             PERFORM ERASE-SCREEN.
             DISPLAY "DEBTORS (A/R) PREVIOUS TRL BALANCE" AT 0323 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
      *    *************************************************************
      *    ****    Create paths and file names for history files    ****
      *    *************************************************************
             MOVE W05-DYY            TO W05-TYY.
             MOVE W05-DMM            TO W05-TMM.
             INSPECT W02-DBHIST REPLACING FIRST "AR_ACNT.DAT" BY "           ".
             MOVE W02-DBHIST         TO W60-ADDRESS.
             STRING W60-ADDRESS DELIMITED BY " " W05-DEB-HIS DELIMITED SIZE INTO W02-DBHIST.
             OPEN I-O DBHIST.
           IF NOT(WS-STATUS = "00")
               PERFORM YA200
               GO TO YA00.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Set up the run / age dates applicable to the history files³
      *    ³ when the statements and ageing of balances was executed by³
      *    ³ the system. These dates are stored in the first record of ³
      *    ³ the history account file, which is a dummy account with   ³
      *    ³ an account number of "     !" and a name "  !".           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             MOVE "     !"           TO DBH-ACNO.
             PERFORM READ-DBHIST THRU READ-DBHIST-EXIT.
           IF WS-F-ERROR = 56
               GO TO YA200.
             MOVE DBH-STM-DTE        TO W12-TODAY.
             GO TO YA999.

       YA200.
             MOVE "Details for period requested not available" TO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.
             CLOSE DBHIST.

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
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

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
               WHEN 6          MOVE W02-DEBTOR    TO WS-FILE
                               MOVE DEB-ACNO      TO WS-KEYX
               WHEN 15         MOVE WS-PARID      TO WS-FILE
                               MOVE WS-PARKEY     TO WS-KEY
               WHEN 56         MOVE W02-DBHIST    TO WS-FILE
                               MOVE DBH-ACNO      TO WS-KEYX
               WHEN OTHER      MOVE "ERROR"       TO WS-FILE
                               MOVE "DTP020"      TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
