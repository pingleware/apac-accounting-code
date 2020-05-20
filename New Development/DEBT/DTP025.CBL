      $SET LINKCOUNT"384" GNT"DTP025.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****     ******   ********   *
      *   **    **     **     **    **   **  **   **    **  **         *
      *   **    **     **     **    **  **    **       **   *******    *
      *   **    **     **     *******   **    **     **           **   *
      *   **    **     **     **        **    **   **             **   *
      *   **    **     **     **         **  **   **        **    **   *
      *   *******      **     **          ****    ********   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R   T R A N S A C T I O N   J O U R N A L        *
      *                                                                *
      *       Version 9.00.82 - April 2016                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *    May 2006 - Include option to print only Accounts with       *
      *               payment terms as selected by the User.           *
      *             - See also DTP026: Age analysis.                   *
      *                                                                *
      *             - New field included in Accounts Receivable        *
      *               for recording number of outstanding Jobs         *
      *               linked to the account. Additional filler         *
      *               added for future expansion.                      *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     DTP025.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   July 1983.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2016
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

       COPY "DEBTOR.SL".

       COPY "DBTRAN.SL".

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

       COPY "DEBTOR.FDE".

       COPY "DBTRAN.FDE".

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
       77  WS-PARKEY               PIC  9(04)     COMP-5.
       77  WS-DEPKEY               PIC  9(04)     COMP-5.
       77  WS-DEBKEY               PIC  9(06)     COMP-5.
       77  WS-NETKEY               PIC  9(06)     COMP-5.
       77  WS-WRKEY                PIC  9(04)     COMP-5.
       77  WS-GST-RATE             PIC S9(03)V99  COMP-3.
       77  WS-NTP                  PIC  9(01).
       77  WS-LINES                PIC  9(02).
       77  WS-PAGE                 PIC  9(04)     COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-AGE                  PIC  X(01).
       77  WS-SEQ                  PIC  X(01).
       77  WS-TERMS                PIC  X(01).
       77  WS-ZERO                 PIC  X(01).
       77  WS-NUM                  PIC  Z9.
       77  WS-ADDR                 PIC  X(01).
       77  WS-ERROR                PIC  9(01).
       77  WS-ERROR1               PIC  9(01).
       77  WS-EXPAND               PIC  X(01)     VALUE " ".
       77  WS-CHAR1                PIC  X(01).
       77  WS-CHAR2                PIC  X(01).
       77  WS-COMP                 PIC  9(01).
       77  WS-STM                  PIC  9(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-HEAD                 PIC  X(01).
       77  WS-REF                  PIC  9(08).
       77  WS-MESS                 PIC  X(02).
       77  WS-ACNO                 PIC  X(06).
       77  WS-S1                   PIC  9(04)     COMP-5.
       77  WS-S2                   PIC  9(04)     COMP-5.
       77  WS-S3                   PIC  9(04)     COMP-5.
       77  WS-S4                   PIC  9(04)     COMP-5.
       77  WS-S5                   PIC  9(04)     COMP-5.
       77  WS-S6                   PIC  9(04)     COMP-5.
       77  WS-S7                   PIC  9(04)     COMP-5.
       77  WS-S8                   PIC  9(04)     COMP-5.
       77  WS-ERR1                 PIC  X(22)     VALUE "Invalid account number".
       77  WS-ERR2                 PIC  X(09)     VALUE "No record".
       77  WS-A                    PIC  X(19)     VALUE "Any key to continue".
       77  WS-AC                   PIC  X(07)     VALUE "Account".
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "DTP025".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "ACCOUNTS RECEIVABLE: TRANSACTION JOURNAL".
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

       01  W00-TRAN-CODES.
           03  W00-TRAN-TYPES                     OCCURS 100.
               05  W00-TRAN-CODE   PIC  9(02)     COMP-5.
               05  W00-ACTION      PIC  9(02)     COMP-5.
               05  W00-E-DESC      PIC  X(12).
               05  W00-A-DESC      PIC  X(12).

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

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

           03  REP-LINE1                          REDEFINES W02-PRN-LINE.
               05  REP-DETAIL1     PIC  X(136).
           
           03  REP-LINE7                          REDEFINES W02-PRN-LINE.
               05  REP-HEAD7       PIC  X(07).
               05  REP-HEAD8       PIC  X(44).
               05  REP-HEAD9       PIC  X(09).
               05  REP-HEAD10      PIC  X(14).
               05  REP-HEAD11      PIC  X(06).
           
           03  REP-LINE8                          REDEFINES W02-PRN-LINE.
               05  REP-TAC         PIC  X(06).
               05  FILLER          PIC  X(01).
               05  REP-TNAME       PIC  X(40).
               05  FILLER          PIC  X(01).
               05  REP-TDTE        PIC  Z9/99/9999.
               05  FILLER          PIC  X(01).
               05  REP-TREF        PIC  X(08).
               05  FILLER          PIC  X(01).
               05  REP-SEQ         PIC  X(01).
               05  REP-TVAL        PIC  Z(06)9.99-.
           
           03  COM-LINE1                          REDEFINES W02-PRN-LINE.
               05  COM-PAGE        PIC  X(06).
               05  COM-P-NO        PIC  Z(03)9.
               05  FILLER          PIC  X(04).
               05  COM-COMPANY     PIC  X(30).
               05  COM-HEAD        PIC  X(18).
               05  FILLER          PIC  X(02).
               05  COM-DATE        PIC  X(06).
               05  COM-DTE         PIC  Z9/99/9999.
           
           03  CAT-L1                             REDEFINES W02-PRN-LINE.
               05  CAT-H1          PIC  X(10).
               05  CAT-CODE        PIC  Z9.
               05  FILLER          PIC  X(02).
               05  CAT-DESC.
                   07  FILLER      PIC  X(13).
                   07  CAT-ACS     PIC  X(17).
               05  FILLER          PIC  X(88).

       01  W05-LIST.
           03  W05-OPT         PIC 9(03).
           03  W05-TYPES REDEFINES W05-OPT.
               05  W05-COMP    PIC 9(01).
               05  W05-INDV    PIC 9(01).
               05  W05-INT     PIC 9(01).

       01  W10-SPARES.
          03  W10-DATE        PIC X(10).
           03  W10-RDATE REDEFINES W10-DATE.
        05  W10-DTE     PIC 99/99/9999.

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

       COPY "W15.WS".

       01  W23-PERIOD.
           03  FILLER          PIC X(17) VALUE "For the period : ".
    03  W23-DTE1        PIC Z9/99/9999.
           03  FILLER          PIC X(04) VALUE " to ".
    03  W23-DTE2        PIC Z9/99/9999.

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

       COPY "W40.DBT".

       COPY "W40.WS".

       01  W45-TRAN.
           03  W45-CODE        PIC 9(02).
           03  W45-ENG         PIC X(12).
           03  W45-AFR         PIC X(12).
           03  W45-ACT         PIC X(01).
      *
      *    STORAGE FOR PRINTING OF LABELS
      *
       01  W46-LABELS.
           03  W46-LABEL OCCURS 2.
               05  W46-NAME    PIC X(40).
               05  W46-ADD1    PIC X(30).
               05  W46-ADD2    PIC X(30).
               05  W46-ADD3    PIC X(30).
               05  W46-ADD4    PIC X(30).
               05  W46-POST.
                   07  W46-PC  PIC 9(04).

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
       01  W65-ENTRY.
           03  W65-FOUR.
               05  W65-CHAR OCCURS 8 PIC X(01).
           03  W65-PREFIX REDEFINES W65-FOUR.
               05  W65-PREF    PIC X(02).
               05  FILLER      PIC X(06).
       01  W65-INDEX.
           03  W65-INPUT.
               05  W65-ICHAR OCCURS 8 PIC X(01).
       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-DNO        PIC X(06) OCCURS 18.

       COPY "W75.WS".

       01  W80-ACCOUNT.
           03  W80-ACNO        PIC X(06).
           03  W80-NUMD REDEFINES W80-ACNO.
               05  W80-SERIAL  PIC 9(06).
           03  W80-TYPE        PIC 9(01).
       01  W85-BATCHES.
               05  W85-BATCH   PIC 9(05).
       01  W90-TRAN.
          03  W90-DATE        PIC X(10).
           03  W90-DTE REDEFINES W90-DATE
                PIC 99/99/9999.
          03  W90-CODE        PIC 9(02).
           03  W90-REF         PIC X(08).
           03  W90-DB          PIC S9(07)V99 COMP-3.
           03  W90-CR          PIC S9(07)V99 COMP-3.
           03  W90-BDT         PIC S9(07)V99 COMP-3.
           03  W90-BCR         PIC S9(07)V99 COMP-3.
           03  W90-DESC        PIC X(12).
           03  W90-120         PIC S9(07)V99 COMP-3.
           03  W90-90          PIC S9(07)V99 COMP-3.
           03  W90-60          PIC S9(07)V99 COMP-3.
           03  W90-30          PIC S9(07)V99 COMP-3.
           03  W90-CUR         PIC S9(07)V99 COMP-3.
           03  W90-MTD         PIC S9(07)V99 COMP-3.
           03  W90-BAL         PIC S9(07)V99 COMP-3.
           03  W90-IN          PIC 9(04)     COMP-5.
           03  W90-IV          PIC S9(07)V99 COMP-3.
           03  W90-RN          PIC 9(04)     COMP-5.
           03  W90-RV          PIC S9(07)V99 COMP-3.
           03  W90-DN          PIC 9(04)     COMP-5.
           03  W90-DV          PIC S9(07)V99 COMP-3.
           03  W90-CN          PIC 9(04)     COMP-5.
           03  W90-CV          PIC S9(07)V99 COMP-3.
           03  W90-INT         PIC S9(07)V99 COMP-3.
           03  W90-RESULT      PIC  9(04)    COMP-5.
           03  W90-CATKEY      PIC  9(04)    COMP-5.
           03  W90-CS          PIC  9(02)    COMP-5.

       01  W95-STM.
           03  W95-COMP        PIC X(40).

       01  W100-HEAD.
           03  FILLER          PIC X(26)    VALUE
                             "DEBTOR ACCOUNTS LISTING -".
           03  W100-DESC       PIC X(11).
       01  W100-EDIT.
           03  W100-LBAL       PIC Z(06)9.99-.
           03  W100-LARR       PIC Z(06)9.99-.
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

       01  S10.
          03  LINE  3 COLUMN 12 FOREGROUND-COLOR Grey HIGHLIGHT
                                 VALUE "TRANSACTION JOURNAL".
           03  LINE  5 COLUMN 12 VALUE "Printing for :".

       01  S11.
          03  LINE 12 COLUMN 12 VALUE "Key in Transaction code".

       01  S12.
          03  LINE 14 COLUMN 12 VALUE "For Terms: ".
    03        COLUMN 23 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "A".
    03        COLUMN 24 VALUE "ll, ".
    03        COLUMN 28 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "7".
    03        COLUMN 29 VALUE "-Days, ".
    03        COLUMN 36 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "3".
    03        COLUMN 37 VALUE "0-Days, ".
    03        COLUMN 45 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "6".
    03        COLUMN 46 VALUE "0-Days, ".
    03        COLUMN 54 FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "C".
    03        COLUMN 55 VALUE ".O.D.".

       01  S18.
    03  LINE  8 COLUMN 26 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC 99/99/9999 USING W15-START AUTO.
    03  LINE  9 COLUMN 26 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC 99/99/9999 USING W15-END AUTO.

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
      PERFORM EA000.

       AA49.
            EXIT PROGRAM.

       COPY "DATE.CHK".

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

       COPY "DEBTOR.RD1".

       COPY "DBTRAN.RD".

      /
       CA155-GET-DEBTOR        SECTION 2.
       CA155-ACCOUNT.
             MOVE W80-ACNO       TO DEB-ACNO.
             PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               MOVE 1            TO WS-ERROR
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
            MOVE ZERO   TO WS-ERROR.
             UNSTRING W60-NAME DELIMITED BY "*" OR "!"
           INTO W40-SURNAME W40-FNAME W40-SNAME
               OVERFLOW
         MOVE 1   TO WS-ERROR.
       CA170-EXIT.
             EXIT.

       CA175-ADDRESS           SECTION 2.
       CA175-EXTRACT.
            MOVE 0   TO WS-ERROR.
             UNSTRING W60-ADDRESS DELIMITED BY "*"
           INTO W40-ADD1 W40-ADD2
          W40-ADD3 W40-ADD4
               OVERFLOW
         MOVE 1   TO WS-ERROR.
       CA180-EXIT.
             EXIT.

      /
       EA000         SECTION 5.
       EA00-INIT.
             PERFORM ERASE-SCREEN.
             DISPLAY S10.

       EA05-ACCEPT.
            DISPLAY W95-COMP AT 0527
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE "Are you sure Y/N" TO WS-OPT-MES.
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
          IF WS-OPTION = "N"
               GO TO EA999-EXIT.
      DISPLAY WS-BLNK78 AT 0702
       WITH BACKGROUND-COLOR Blue FOREGROUND-COLOR Cyan.
             DISPLAY "For the period." AT 0712.
             DISPLAY "Start date :" AT 0812.
             DISPLAY "End date   :" AT 0912.
      DISPLAY "Dates to be keyed in as DDMMCCYY" AT 1012
        WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE ZERO           TO W15-START W15-END.

       EA06.
             ACCEPT S18.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO EA06.
      MOVE W15-START  TO W10-DTE.
             PERFORM CHECK-DATE.
           IF WS-ERROR = 1
              MOVE "Start date" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO EA06.
             MOVE W20-DTE        TO W15-S-YMD.
             MOVE W15-END        TO W10-DTE.
             PERFORM CHECK-DATE.
           IF WS-ERROR = 1
              MOVE "End date"  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO EA06.
             MOVE W20-DTE        TO W15-E-YMD.
      MOVE W15-START  TO W23-DTE1.
      MOVE W15-END  TO W23-DTE2.
             DISPLAY S11.
      PERFORM SAVE-SCREEN-3.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *   **  *   *   * *   *  *  **  *   *   *     * *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *   *  **   *     *   *  *  *  **   *   *     * *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** * *
      *
      PERFORM OPEN-PRINTER.
      MOVE "P"   TO WS-COMMAND.

       EA07.
            ACCEPT W45-CODE AT 1236
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
           IF W45-CODE = ZERO
              GO TO EA990-CLOSE.
      *
      *    ****   Additional security to block the Invoices from
      *    being printed for anyone with a security level
      *    for accounts receivable (debtors) less than 3.
      *
          IF (LS0-DBLEV < 3) AND (W45-CODE = 01)
        MOVE "Not Authorised for this code"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO EA07.
          IF (W45-CODE > 99) OR (W00-E-DESC(W45-CODE) = SPACES)
              MOVE "Invalid code"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO EA07.
            DISPLAY W00-E-DESC(W45-CODE) AT 1244
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO EA07.
      DISPLAY S12.
      MOVE "A"   TO WS-TERMS.

       EA07A.
      PERFORM TEST AFTER
       UNTIL WS-TERMS = "A" OR "C" OR "3" OR "6" OR "7"
              ACCEPT WS-TERMS AT 1461
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta UPDATE AUTO
        CALL "CBL_TOUPPER" USING WS-TERMS
      BY VALUE WS-LENGTH
     RETURNING WS-STATUS
      END-PERFORM.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Convert user friendly options as accepted on screen to    ³
      *    ³ code as stored on the account record (DEB-TERMS).        ³
      *    ³ 0 = & Days, 1 = 30 Days, 2 = 60 Days and 3 = C.O.D.       ³
      *    ³ For 'A'll leave the option as A.          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
    IF WS-TERMS = "7"
        MOVE "0"  TO WS-TERMS
    ELSE
    IF WS-TERMS = "3"
        MOVE "1"  TO WS-TERMS
    ELSE
    IF WS-TERMS = "6"
        MOVE "2"  TO WS-TERMS
    ELSE
    IF WS-TERMS = "C"
        MOVE "3"  TO WS-TERMS.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO EA07.
    IF W45-CODE = 02
        MOVE ZERO  TO WS-REF.

       EA08.
             MOVE 1              TO WS-SUB-2.
            MOVE 0   TO WS-PAGE W90-BAL W90-BDT W90-BCR.

       EA10-HEADING.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE SPACES  TO REP-DETAIL1.
            MOVE "PAGE:"  TO COM-PAGE.
            MOVE "DATE:"  TO COM-DATE.
            ADD 1   TO WS-PAGE.
            MOVE W12-TODAY  TO COM-DTE.
            MOVE WS-PAGE  TO COM-P-NO.
            MOVE W95-COMP  TO COM-COMPANY.
            MOVE "-TRANSACTION JOURNAL"
     TO COM-HEAD.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO COM-LINE1.

       EA12-HEADING.
            MOVE "TRN TYPE:"  TO CAT-H1.
            MOVE W45-CODE  TO CAT-CODE.
            MOVE W00-E-DESC(W45-CODE)
     TO CAT-DESC.
    IF WS-TERMS = "0"
        MOVE "(7 Day Accounts)"
     TO CAT-ACS
    ELSE
    IF WS-TERMS = "1"
        MOVE "(30 Day Accounts)"
     TO CAT-ACS
    ELSE
    IF WS-TERMS = "2"
        MOVE "(60 Day Accounts)"
     TO CAT-ACS
    ELSE
    IF WS-TERMS = "3"
        MOVE "(C.O.D. Accounts)"
     TO CAT-ACS
    ELSE
        MOVE "(ALL Accounts)"
     TO CAT-ACS.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
            MOVE "AC-NO"  TO REP-HEAD7.
            MOVE "NAME"  TO REP-HEAD8.
            MOVE "DATE"  TO REP-HEAD9.
            MOVE "REFER"  TO REP-HEAD10.
            MOVE "VALUE"  TO REP-HEAD11.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO TRL-DET.

       EA15-INDEX.
             MOVE W45-CODE       TO TRA-CODE.
             MOVE W15-S-YMD      TO TRA-DATE.
             PERFORM START-AT-DTRN-CODE THRU READ-DBTRAN-EXIT.
           IF WS-F-ERROR = 5
             GO TO EA25-CHR.

       EA22.
             PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
           IF (WS-F-ERROR = 5) OR
              (TRA-CODE NOT = W45-CODE) OR
              (TRA-DATE > W15-E-YMD)
               GO TO EA25-CHR.
      *    **************************************************************
      *      The Account record is now read prior to any processing of
      *      the transaction record.
      *    **************************************************************
            MOVE TRA-AC  TO W80-ACNO DEB-ACNO.
             PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
      *    **************************************************************
      *     Only print transactions for accounts with the User selected
      *     payment terms.
      *    **************************************************************
    IF NOT(WS-TERMS = "A")
        IF NOT(WS-TERMS = DEB-TERMS)
     GO TO EA22.
    IF TRA-CODE = 02
        IF WS-REF = ZERO
     MOVE TRA-REF  TO WS-REF
        ELSE
     ADD 1  TO WS-REF.
          IF TRA-VALUE = ZERO
               GO TO EA22.
            MOVE TRA-DATE  TO W22-DTE2.
      PERFORM SWITCH-DATE-TO-DMCY.
      MOVE W22-DTE3  TO REP-TDTE.
            MOVE TRA-REF  TO REP-TREF.
            MOVE TRA-VALUE  TO REP-TVAL.
            ADD TRA-VALUE  TO W90-BDT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ May 2006 - As the new option to selectively print for a   ³
      *    ³ specific or all Payment terms, the account details will   ³
      *    ³ now be read before any processing of the transaction      ³
      *    ³ record is performed.           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *      MOVE TRA-AC  TO W80-ACNO DEB-ACNO.
      *       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
            MOVE DEB-NAME  TO W40-NAME.
           IF DEB-TYPE = 1
               INSPECT W40-NAME REPLACING ALL "*" BY "."
               INSPECT W40-NAME REPLACING FIRST "!" BY " ".
            MOVE DEB-ACNO  TO REP-TAC.
            MOVE W40-NAME  TO REP-TNAME.
    IF TRA-CODE = 02
        IF NOT(WS-REF = TRA-REF)
     MOVE "*"  TO REP-SEQ
     IF WS-REF < TRA-REF
         MOVE TRA-REF
     TO WS-REF
     ELSE
         SUBTRACT 1
     FROM WS-REF.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM EA10-HEADING THRU EA12-HEADING.
            MOVE SPACES  TO REP-DETAIL1.
             PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              GO TO EA990-CLOSE.
             GO TO EA22.

       EA25-CHR.
            MOVE "TRANSACTION TOTAL"
     TO REP-TNAME.
            MOVE W90-BDT  TO REP-TVAL.
      MOVE 99   TO WS-ADVANCE.
      MOVE "E"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
      MOVE "P"   TO WS-COMMAND.

       EA26.
    IF WS-PRINTER = "S"
        PERFORM RESTORE-SCREEN-3.
             GO TO EA07.

       EA990-CLOSE.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.

       EA999-EXIT.
             EXIT.

      /
       ZA00-INIT       SECTION 9.
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

       ZA02.
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
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-AGE            TO WS-AGE.
             MOVE 1                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY        TO W95-COMP.
             MOVE PAR-DMY            TO W12-TODAY.
             MOVE PAR-YMD            TO W12-T-YMD.
           IF PAR-STMINV = "S" OR "X"
              MOVE "N"   TO WS-HEAD
           ELSE
              MOVE "Y"   TO WS-HEAD.
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
               WHEN 5          MOVE W02-DBTRAN    TO WS-FILE
                               MOVE TRA-KEY       TO WS-KEYX
               WHEN 6          MOVE W02-DEBTOR    TO WS-FILE
                               MOVE DEB-ACNO      TO WS-KEYX
               WHEN 15         MOVE WS-PARID      TO WS-FILE
                               MOVE WS-PARKEY     TO WS-KEY
               WHEN OTHER      MOVE "ERROR"       TO WS-FILE
                               MOVE "DTP025"      TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
