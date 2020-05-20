      $SET LINKCOUNT"512" GNT"DTP022.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****     ******    ******    *
      *   **    **     **     **    **   **  **   **    **  **    **   *
      *   **    **     **     **    **  **    **       **        **    *
      *   **    **     **     *******   **    **     **        **      *
      *   **    **     **     **        **    **   **        **        *
      *   **    **     **     **         **  **   **        **         *
      *   *******      **     **          ****    ********  ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R   A C C O U N T   L I S T   P R O G R A M      *
      *                                                                *
      *       Version 9.00.82 - April 2016                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  May 2006 - New field included in Accounts Receivable          *
      *             for recording number of outstanding Jobs           *
      *             linked to the account. Additional filler           *
      *             added for future expansion.                        *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     DTP022.
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
       77  WS-CHECK               PIC  X(18)     VALUE "aeWlimemnomLalismJ".
       77  WS-SUB-1                PIC  9(04)     COMP-5.
       77  WS-SUB-2                PIC  9(04)     COMP-5.
       77  WS-IXS1                 PIC  9(04)     COMP-5.
       77  WS-IXS2                 PIC  9(04)     COMP-5.
       77  WS-IXS3                 PIC  9(04)     COMP-5.
       77  WS-IXS4                 PIC  9(04)     COMP-5.
       77  WS-MSUB                 PIC  9(04)     COMP-5.
       77  WS-PARKEY               PIC  9(04)     COMP-5.
       77  WS-NETKEY               PIC  9(06)     COMP-5.
       77  WS-WRKEY                PIC  9(04)     COMP-5.
       77  WS-NTP                  PIC  9(01).
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
       77  WS-EXPAND               PIC  X(01)     VALUE " ".
       77  WS-COMP                 PIC  9(01).
       77  WS-STM                  PIC  9(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-HEAD                 PIC  X(01).
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "DTP022".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "ACCOUNTS RECEIVABLE: LIST OF ACCOUNTS".
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
               05  W00-TRAN-CODE   PIC 9(02)      COMP-5.
               05  W00-ACTION      PIC 9(02)      COMP-5.
               05  W00-E-DESC      PIC X(12).
               05  W00-A-DESC      PIC X(12).

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

       01  W02-PRINTER-DETAILS.
           03  W02-PRINTER         PIC  X(12).
           03  W02-PGE-LENGTH      PIC  9(03).
           03  W02-PRN-LENGTH      PIC  9(03).
           03  W02-LINAGE          PIC  9(03).
           03  W02-PRN-STATUS      PIC  X(01) VALUE "C".
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
           03  REP-LINE2                          REDEFINES W02-PRN-LINE.
               05  REP-DETAIL2     PIC  X(80).
           
           03  REP-LINE3                          REDEFINES W02-PRN-LINE.
               05  REP-PAGE        PIC  X(06).
               05  REP-P-NO        PIC  Z(03)9.
               05  FILLER          PIC  X(10).
               05  REP-COMPANY     PIC  X(40).
               05  FILLER          PIC  X(04).
               05  REP-DATE        PIC  X(06).
               05  REP-DTE         PIC  Z9/99/9999.
           
           03  REP-LINE3A                         REDEFINES W02-PRN-LINE.
               05  REP-CO          PIC  X(40).
           
           03  LST-L1                             REDEFINES W02-PRN-LINE.
               05  LST-H1          PIC  X(07).
               05  LST-H2          PIC  X(09).
               05  FILLER          PIC  X(06).
               05  LST-H3          PIC  X(37).
               05  FILLER          PIC  X(21).

           03  LST-L2                             REDEFINES W02-PRN-LINE.
               05  LST-AC          PIC  X(06).
               05  FILLER          PIC  X(10).
               05  LST-NME         PIC  X(42).
               05  LST-H4          PIC  X(06).
               05  LST-LANG        PIC  X(02).
               05  LST-H5          PIC  X(07).
               05  LST-TIT         PIC  X(07).

           03  LST-L3                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(16).
               05  LST-ADD         PIC  X(60).
               05  FILLER          PIC  X(04).

           03  LST-L4                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(16).
               05  LST-PC          PIC  9(04).
               05  FILLER          PIC  X(01).
               05  LST-H6          PIC  X(07).
               05  LST-TEL         PIC  X(13).
               05  LST-H7          PIC  X(07).
               05  LST-HTEL        PIC  X(13).
               05  LST-H8          PIC  X(06).
               05  LST-ID          PIC  X(13).

           03  LST-L5                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(41).
               05  LST-H9          PIC  X(05).
               05  LST-TAX         PIC  X(14).
               05  FILLER          PIC  X(20).

       01  W05-LIST.
           03  W05-OPT         PIC 9(03).
           03  W05-TYPES REDEFINES W05-OPT.
               05  W05-COMP    PIC 9(01).
               05  W05-INDV    PIC 9(01).
               05  W05-INT     PIC 9(01).

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

       01  W24-MONTHS.
           03  W24-MONTH-NAMES.
               05  FILLER      PIC  X(09) VALUE " January ".
               05  FILLER      PIC  X(09) VALUE " February".
               05  FILLER      PIC  X(09) VALUE "  March  ".
               05  FILLER      PIC  X(09) VALUE "  April  ".
               05  FILLER      PIC  X(09) VALUE "   May   ".
               05  FILLER      PIC  X(09) VALUE "   June  ".
               05  FILLER      PIC  X(09) VALUE "   July  ".
               05  FILLER      PIC  X(09) VALUE "  August ".
               05  FILLER      PIC  X(09) VALUE "September".
               05  FILLER      PIC  X(09) VALUE " October ".
               05  FILLER      PIC  X(09) VALUE " November".
               05  FILLER      PIC  X(09) VALUE " December".
           03  W24-EACH-MONTH REDEFINES W24-MONTH-NAMES.
               05  W24-MONTH   PIC  X(09) OCCURS 12.

       01  W25-CALCS.
          03  W25-RESULT      PIC  9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
              05  W25-WHOLE   PIC  9(05).
              05  W25-DECIMAL PIC  9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
        05  W25-KEY     PIC  9(04).
        05  W25-SUB     PIC  9(01).
        05  FILLER      PIC  9(02).
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

       01  MENU-INSTRUCT.
    03  LINE 25 COLUMN  2 BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Blue
     VALUE "Use ".
    03        COLUMN  6 HIGHLIGHT
     BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Brown
     VALUE "".
    03        COLUMN  7 BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Blue
     VALUE " or ".
    03        COLUMN 11 HIGHLIGHT
     BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Brown
     VALUE "".
    03        COLUMN 12 BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Blue
     VALUE " to point to an item, and ".
    03        COLUMN 38 HIGHLIGHT
     BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Brown
     VALUE "<Enter>".
    03        COLUMN 45 BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Blue
     VALUE " to Select ".
    03        COLUMN 56 HIGHLIGHT
     BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Brown
     VALUE "<Esc>".
    03        COLUMN 61 BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Blue
     VALUE " to Exit".

       01  S-PROG.
          03  LINE  2 COLUMN 33 FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "DEBTOR REPORTS".

       01  S-MENU.
    03  LINE  4 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
    03  LINE  5 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³   ".
    03        COLUMN 33 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "DEBTOR LIST OPTIONS".
    03        COLUMN 52 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "    ³".
    03  LINE  6 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "1".
    03        COLUMN 32 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " All Accounts           ³".
    03  LINE  7 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "2".
    03        COLUMN 32 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " Company Accounts       ³".
          03  LINE  8 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "3".
    03        COLUMN 32 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " Individuals/Personal   ³".
          03  LINE  9 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "4".
    03        COLUMN 32 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " Internal Accounts      ³".
          03  LINE 10 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "0".
    03        COLUMN 32 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " Exit                   ³".
    03  LINE 11 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".


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
             PERFORM LA000.

       AA49.
            EXIT PROGRAM.

       COPY "AA900.ALM".
       
       COPY "FUNCTION.SCR".

       COPY "ERROR.SCR".

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

      /
      *    ****    READ FILES ROUTINES
      *
       AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "DEBTOR.RD1".

       COPY "PRINTUTL.AP5".

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
             MOVE ZERO TO WS-ERROR.
             UNSTRING W60-NAME DELIMITED BY "*" OR "!"
               INTO W40-SURNAME W40-FNAME W40-SNAME
               OVERFLOW
                 MOVE 1 TO WS-ERROR.
       CA170-EXIT.
             EXIT.

       CA175-ADDRESS           SECTION 2.
       CA175-EXTRACT.
             MOVE 0 TO WS-ERROR.
             UNSTRING W60-ADDRESS DELIMITED BY "*"
               INTO W40-ADD1 W40-ADD2 W40-ADD3 W40-ADD4
               OVERFLOW
                 MOVE 1 TO WS-ERROR.
       CA180-EXIT.
             EXIT.
      /
       LA000         SECTION 5.
       LA00.
    IF MOUSE-ATTACHED = "Y"
        MOVE 66   TO MOUSE-FUNC
        MOVE 1   TO MOUSE-PARAM
        CALL X"AF" USING MOUSE-FUNC
    MOUSE-PARAM.
             PERFORM ERASE-SCREEN.
      DISPLAY S-PROG.
      MOVE 5   TO SHADE-ROW.
      MOVE 31   TO SHADE-COL.
      MOVE 26   TO SHADE-WIDTH.
      MOVE 7   TO SHADE-LINES.
      DISPLAY S-MENU.
      DISPLAY MENU-INSTRUCT.
      PERFORM SCREEN-SHADOW.
       LA01.
      MOVE 5   TO TOP-ROW.
      MOVE 9   TO BOTTOM-ROW.
      MOVE 4   TO SCREEN-LIN.
      MOVE 31   TO SCREEN-COL.
      MOVE 24   TO STRING-LENGTH.
      PERFORM SAVE-ATTR.

       LA03.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       LA04.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY  PERFORM MOVE-BLOCK-UP
   WHEN DOWN-KEY  PERFORM MOVE-BLOCK-DOWN
   WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
     IF MOUSE-ENTER = "Y"
         MOVE "N"  TO MOUSE-ENTER
         GO TO LA05
     END-IF
     IF MOUSE-Y = 24
         IF MOUSE-X = 5
      MOVE 5  TO KEY-CODE-1
      GO TO LA04
         ELSE
         IF MOUSE-X = 10
      MOVE 6  TO KEY-CODE-1
      GO TO LA04
         ELSE
         IF MOUSE-X > 36 AND < 44
      MOVE 0  TO KEY-CODE-1
      GO TO LA04
         ELSE
         IF MOUSE-X > 54 AND < 60
      MOVE 0  TO KEY-CODE-1
      MOVE 1  TO KEY-TYPE
      GO TO LA04
         END-IF
     END-IF
   WHEN ENTER-KEY  GO TO LA05
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO LA03
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE "0"  TO WS-OPTION
       GO TO LA07
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO LA03
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
        IF (WS-OPTION < "0" OR > "4")
     PERFORM AA900-ALARM
     GO TO LA03
        END-IF
        GO TO LA06
    ELSE
        PERFORM AA900-ALARM
        GO TO LA03.

       LA05.
    IF SCREEN-LIN < 5
        PERFORM AA900-ALARM
        GO TO LA03
    ELSE
    IF SCREEN-LIN = 5
        MOVE "1"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 6
        MOVE "2"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 7
        MOVE "3"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 8
        MOVE "4"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 9
        MOVE "0"   TO WS-OPTION.
    GO TO LA07.

       LA06.
      PERFORM CLEAR-BLOCK.
    IF WS-OPTION = "0"
        MOVE 9   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "1"
        MOVE 5   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "2"
        MOVE 6   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "3"
        MOVE 7   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "4"
        MOVE 8   TO SCREEN-LIN.
      PERFORM SAVE-ATTR.
      PERFORM MARK-BLOCK.

       LA07.
    IF MOUSE-ATTACHED = "Y"
        MOVE 66   TO MOUSE-FUNC
        MOVE 0   TO MOUSE-PARAM
        CALL X"AF" USING MOUSE-FUNC
    MOUSE-PARAM.
           IF WS-OPTION = "0"
               GO TO LA999.
           IF WS-OPTION = "1"
               PERFORM LB000
               GO TO LA00.
           IF WS-OPTION = "2"
               PERFORM LC000
               GO TO LA00.
           IF WS-OPTION = "3"
               PERFORM LD000
               GO TO LA00.
           IF WS-OPTION = "4"
               PERFORM LE000
               GO TO LA00.
             GO TO LA05.
       LA999.
             EXIT.
      /
       LB000         SECTION 5.
       LB00.
            MOVE 111   TO W05-OPT.
             PERFORM LF000.
       LB999.
             EXIT.
       LC000         SECTION 5.
       LC00.
            MOVE 100   TO W05-OPT.
             PERFORM LF000.
       LC999.
             EXIT.
       LD000         SECTION 5.
       LD00.
            MOVE 010   TO W05-OPT.
             PERFORM LF000.
       LD999.
             EXIT.
       LE000         SECTION 5.
       LE00.
            MOVE 001   TO W05-OPT.
             PERFORM LF000.
       LE999.
             EXIT.
      /
       LF000         SECTION 5.
       LF00.
             PERFORM ERASE-SCREEN.
             DISPLAY "Busy printing Debtors listing" AT 0820.
             DISPLAY "A" AT 1212 WITH FOREGROUND-COLOR Cyan HIGHLIGHT 
                     "ccount or " WITH FOREGROUND-COLOR Cyan
                     "N" WITH FOREGROUND-COLOR Cyan HIGHLIGHT 
                     "ame sequence" WITH FOREGROUND-COLOR Cyan.
             MOVE "N"            TO WS-SEQ.

       LF02.
      ACCEPT WS-SEQ AT 1237
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-SEQ
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-SEQ = "A" OR "N")
               GO TO LF02.
             DISPLAY "Debtor account:" AT 1412.
            MOVE ZERO   TO WS-PAGE.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *     **  *   *   * *   *  *  **  *   *   *     *   *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *     *  **   *     *   *  *  *  **   *   *     *   *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** *   *
      *
      PERFORM OPEN-PRINTER.

       LF05.
           IF W05-COMP = 1
              MOVE "COMPANIES"  TO W100-DESC
              MOVE ZERO  TO W05-COMP
              MOVE 2   TO WS-NTP
           ELSE
           IF W05-INDV = 1
              MOVE "INDIVIDUALS"
     TO W100-DESC
              MOVE 1   TO WS-NTP
              MOVE ZERO  TO W05-INDV
           ELSE
           IF W05-INT = 1
              MOVE "INTERNAL"  TO W100-DESC
              MOVE ZERO  TO W05-INT
              MOVE 3   TO WS-NTP
           ELSE
              GO TO LF995.

       LF10.
      MOVE "P"   TO WS-COMMAND.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE SPACES  TO REP-DETAIL1.
            MOVE "PAGE:"  TO REP-PAGE.
            MOVE "DATE:"  TO REP-DATE.
            ADD 1   TO WS-PAGE.
            MOVE W12-TODAY  TO REP-DTE.
            MOVE WS-PAGE  TO REP-P-NO.
            MOVE W100-HEAD  TO LST-H3.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL1.
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE "E"   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE W95-COMP  TO REP-CO.
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
             MOVE SPACES TO REP-DETAIL1.
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE "X"   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE "AC-NO"  TO LST-H1.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE "D"   TO W02-PRN-TYPE.

       LF15.
             MOVE " !"           TO DEB-NAME DEB-ACNO.
           IF WS-SEQ = "A"
               PERFORM START-AT-DEBT-AC THRU READ-DEBTOR-EXIT
           ELSE
               PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              GO TO LF995.

       LF20.
             PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               GO TO LF30.
           IF DEB-TYPE NOT = WS-NTP
               GO TO LF20.
    IF NOT (WS-PRINTER = "S")
        DISPLAY DEB-ACNO AT 1428
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta.
            MOVE DEB-ACNO  TO LST-AC.
            MOVE "NAME   :"  TO LST-H2.
            MOVE "LANG:"  TO LST-H4.
           IF WS-NTP = 1
              MOVE "TITLE:"  TO LST-H5
               INSPECT DEB-NAME REPLACING FIRST "*" BY ","
               INSPECT DEB-NAME REPLACING FIRST "*" BY " "
               INSPECT DEB-NAME REPLACING FIRST "!" BY " ".
            MOVE DEB-NAME  TO LST-NME.
            MOVE DEB-LANG  TO LST-LANG.
           IF WS-NTP = 1
               IF DEB-LANG = "A"
           MOVE W75-ATITLE (DEB-TITLE)
     TO LST-TIT
               ELSE
           MOVE W75-ETITLE (DEB-TITLE)
     TO LST-TIT.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
            MOVE "ADDRESS:"  TO LST-H2.
             INSPECT DEB-ADDRESS REPLACING ALL "*" BY ".".
            MOVE DEB-ADDRESS  TO LST-ADD.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
           IF DEB-PADDR = SPACES
               GO TO LF25.
            MOVE "A/ADDR :"  TO LST-H2.
             INSPECT DEB-PADDR REPLACING ALL "*" BY ".".
            MOVE DEB-PADDR  TO LST-ADD.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.

       LF25.
            MOVE "P-CODE :"  TO LST-H2.
            MOVE "B-TEL:"  TO LST-H6.
           IF WS-NTP = 1
              MOVE "H-TEL:"  TO LST-H7
              MOVE DEB-H-TEL  TO LST-HTEL
           ELSE
           IF WS-NTP = 2
              MOVE "TAX:"  TO LST-H9
              MOVE DEB-TAX  TO LST-TAX.
           IF WS-NTP = 1
              MOVE "IDNO:"  TO LST-H8
              MOVE DEB-ID  TO LST-ID.
            MOVE DEB-POST  TO LST-PC.
            MOVE DEB-B-TEL  TO LST-TEL.
          IF W02-LINAGE < (W02-PRN-LENGTH - 4)
        MOVE 2   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM LF10.
            MOVE SPACES  TO REP-DETAIL1.
             PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              GO TO LF995.
             GO TO LF20.

       LF30.
      MOVE "E"   TO WS-COMMAND.
      MOVE 99   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
             GO TO LF05.

       LF995.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.

       LF999.
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
            MOVE PAR-T-CODE (WS-SUB-1)
     TO W00-TRAN-CODE (WS-SUB-2).
            MOVE PAR-E-DESC (WS-SUB-1)
     TO W00-E-DESC (WS-SUB-2).
            MOVE PAR-A-DESC (WS-SUB-1)
     TO W00-A-DESC (WS-SUB-2).
            MOVE PAR-ACTION (WS-SUB-1)
     TO W00-ACTION (WS-SUB-2).
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
               WHEN 6          MOVE W02-DEBTOR    TO WS-FILE
                               MOVE DEB-ACNO      TO WS-KEYX
               WHEN 15         MOVE WS-PARID     TO WS-FILE
                               MOVE WS-PARKEY    TO WS-KEY
               WHEN OTHER      MOVE "ERROR"       TO WS-FILE
                               MOVE "DTP022"      TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
