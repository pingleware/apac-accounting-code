      $SET LINKCOUNT"512" GNT"DTP024.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****     ******       ***    *
      *   **    **     **     **    **   **  **   **    **     ****    *
      *   **    **     **     **    **  **    **       **     ** **    *
      *   **    **     **     *******   **    **     **      **  **    *
      *   **    **     **     **        **    **   **       ********   *
      *   **    **     **     **         **  **   **             **    *
      *   *******      **     **          ****    ********      ****   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R   C A T E G O R Y   L I S T   P R O G R A M    *
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
       PROGRAM-ID.     DTP024.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   August 1983.

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
       77  WS-IXS1                 PIC  9(04)     COMP-5.
       77  WS-IXS2                 PIC  9(04)     COMP-5.
       77  WS-IXS3                 PIC  9(04)     COMP-5.
       77  WS-IXS4                 PIC  9(04)     COMP-5.
       77  WS-MSUB                 PIC  9(04)     COMP-5.
       77  WS-PARKEY       PIC 9(04)     COMP-5.
       77  WS-DEPKEY       PIC 9(04)     COMP-5.
       77  WS-DEBKEY       PIC 9(06)     COMP-5.
       77  WS-NETKEY       PIC 9(06)     COMP-5.
       77  WS-WRKEY        PIC 9(04)     COMP-5.
       77  WS-GST-RATE     PIC S9(03)V99 COMP-3.
       77  WS-NTP          PIC 9(01).
       77  WS-LINES        PIC 9(02).
       77  WS-PAGE         PIC 9(04)     COMP-5.
       77  WS-OPTION       PIC X(01).
       77  WS-AGE          PIC X(01).
       77  WS-SEQ          PIC X(01).
       77  WS-CREDIT       PIC X(01).
       77  WS-ZERO         PIC X(01).
       77  WS-NUM          PIC Z9.
       77  WS-ADDR         PIC X(01).
       77  WS-ERROR        PIC 9(01).
       77  WS-ERROR1       PIC 9(01).
       77  WS-PGE-LENGTH   PIC 9(02)  VALUE 66.
       77  WS-PRN-LENGTH   PIC 9(02)  VALUE 62.
       77  WS-EXPAND    PIC X(01)  VALUE " ".
       77  WS-PRN-NO    PIC X(01)  COMP-X VALUE 9.
       77  WS-PRN-STAT    PIC X(01)  COMP-X.
       77  WS-CHAR1    PIC X(01).
       77  WS-CHAR2        PIC X(01).
       77  WS-COMP         PIC 9(01).
       77  WS-STM          PIC 9(01).
       77  WS-TYPE         PIC X(01).
       77  WS-HEAD         PIC X(01).
       77  WS-BATCH    PIC 9(05).
       77  WS-MESS         PIC X(02).
       77  WS-ACNO         PIC X(06).
       77  WS-S1           PIC 9(04)     COMP-5.
       77  WS-S2           PIC 9(04)     COMP-5.
       77  WS-S3           PIC 9(04)     COMP-5.
       77  WS-S4           PIC 9(04)     COMP-5.
       77  WS-S5           PIC 9(04)     COMP-5.
       77  WS-S6           PIC 9(04)     COMP-5.
       77  WS-S7           PIC 9(04)     COMP-5.
       77  WS-S8           PIC 9(04)     COMP-5.
       77  WS-ERR1    PIC X(22)   VALUE
                           "Invalid account number".
       77  WS-ERR2         PIC X(09) VALUE "No record".
       77  WS-A            PIC X(19) VALUE "Any key to continue".
       77  WS-AC           PIC X(07) VALUE "Account".
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "DTP024".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "ACCOUNTS RECEIVABLE: LIST PER CATEGORY".
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
           03  W02-PRINTER           PIC  X(12).
           03  W02-PGE-LENGTH        PIC  9(03).
           03  W02-PRN-LENGTH        PIC  9(03).
           03  W02-LINAGE            PIC  9(03).
           03  W02-PRN-STATUS        PIC  X(01)    VALUE "C".
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

           03  TRL-LINE1                          REDEFINES W02-PRN-LINE.
               05  TRL-DET         PIC  X(132).

           03  TRL-LINE2                          REDEFINES W02-PRN-LINE.
               05  TRL-HEAD1       PIC  X(07).
               05  TRL-HEAD2       PIC  X(34).
               05  TRL-HEAD3       PIC  X(08).
               05  TRL-HEAD4       PIC  X(06).
               05  TRL-HEAD5       PIC  X(11).
               05  TRL-HEAD6       PIC  X(11).
               05  TRL-HEAD7       PIC  X(09).
               05  TRL-HEAD8       PIC  X(09).
               05  TRL-HEAD9       PIC  X(09).
               05  TRL-HEAD10      PIC  X(10).
               05  TRL-HEAD11      PIC  X(10).
               05  TRL-HEAD12      PIC  X(08).

           03  TRL-LINE3                          REDEFINES W02-PRN-LINE.
               05  TRL-AC          PIC  X(06).
               05  FILLER          PIC  X(01).
               05  TRL-NME         PIC  X(31).
               05  FILLER          PIC  X(01).
               05  TRL-ACT         PIC  Z9/99/9999.
               05  FILLER          PIC  X(01).
               05  TRL-MTH         PIC  9(01).
               05  FILLER          PIC  X(02).
               05  TRL-CUR         PIC  Z(05)9.99-.
               05  FILLER          PIC  X(01).
               05  TRL-30          PIC  Z(05)9.99-.
               05  FILLER          PIC  X(01).
               05  TRL-60          PIC  Z(05)9.99.
               05  FILLER          PIC  X(01).
               05  TRL-90          PIC  Z(05)9.99.
               05  FILLER          PIC  X(01).
               05  TRL-INT         PIC  Z(03)9.99.
               05  FILLER          PIC  X(01).
               05  TRL-PD          PIC  Z(05)9.99-.
               05  FILLER          PIC  X(01).
               05  TRL-LIMIT       PIC  Z(05)9.
               05  TRL-EXL         PIC  X(01).
               05  TRL-BAL         PIC  Z(06)9.99-.
           
           03  TRL-LINE4                          REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(33).
               05  TRL-HEADER      PIC  X(13).
               05  FILLER          PIC  X(34).
           
           03  TRL-LINE5                          REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(10).
               05  TRL-SUM         PIC  X(20).
               05  TRL-VAL         PIC  Z(06)9.99-.
               05  FILLER          PIC  X(39).
           
           03  TRL-LINE6                          REDEFINES W02-PRN-LINE.
               05  TRL-PAGE        PIC  X(06).
               05  TRL-P-NO        PIC  Z(03)9.
               05  FILLER          PIC  X(24).
               05  TRL-RESTART.
                   07  TRL-COMPANY PIC  X(40).
                   07  TRL-HEAD    PIC  X(24).
               05  FILLER          PIC  X(18).
               05  TRL-DATE        PIC  X(06).
               05  TRL-DTE         PIC  Z9/99/9999.
           
           03  TRL-LINE6A                         REDEFINES W02-PRN-LINE.
               05  TRL-EXPP        PIC  X(06).
               05  FILLER          PIC  X(05).
               05  TRL-CO          PIC  X(30).
               05  TRL-EXHEAD      PIC  X(24).
               05  TRL-EXPC        PIC  X(07).
           
           03  TRL-LINE7                          REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(07).
               05  TRL-PCDE        PIC  9(04).
               05  FILLER          PIC  X(02).
               05  TRL-HEAD13      PIC  X(10).
               05  TRL-HTEL        PIC  X(14).
               05  FILLER          PIC  X(02).
               05  TRL-BTEL        PIC  X(14).
               05  FILLER          PIC  X(79).

           03  CAT-L1                             REDEFINES W02-PRN-LINE.
               05  CAT-H1          PIC  X(10).
               05  CAT-CODE        PIC  Z9.
               05  FILLER          PIC  X(02).
               05  CAT-DESC        PIC  X(30).
               05  FILLER          PIC  X(88).

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

       01  S06.
          03  LINE  3 COLUMN 12 FOREGROUND-COLOR Grey HIGHLIGHT
                                 VALUE "DEBTORS PER CATEGORY".
           03  LINE  5 COLUMN 12 VALUE  "Printing for :".

       01  S08.
           03  LINE 10 COLUMN 12 VALUE "Do you want the debtor address printed (".
           03          COLUMN 52 VALUE                                         "Y"   FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 53 VALUE                                          "/".
           03          COLUMN 54 VALUE                                           "N" FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 55 VALUE                                            ")".

       01  S09.
           03  LINE 12 COLUMN 12 VALUE "Keyin Debtor Category code".

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
      PERFORM DA000.

       AA49.
            EXIT PROGRAM.

       COPY "DATE.CNV.

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
            MOVE 0   TO WS-ERROR.
             UNSTRING W60-ADDRESS DELIMITED BY "*"
               INTO W40-ADD1 W40-ADD2 W40-ADD3 W40-ADD4
               OVERFLOW
         MOVE 1   TO WS-ERROR.
       CA180-EXIT.
             EXIT.
      /
       DA000         SECTION 5.
       DA00-INIT.
             PERFORM ERASE-SCREEN.
             DISPLAY S06.

       DA05-ACCEPT.
             DISPLAY W95-COMP AT 0527 WITH FOREGROUND-COLOR Cyan HIGHLIGHT .
             MOVE "Are you sure Y/N" TO WS-OPT-MES.
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
          IF WS-OPTION = "N"
               GO TO DA999-EXIT.
             DISPLAY S08.
             ACCEPT WS-ADDR AT 1057 WITH FOREGROUND-COLOR Cyan HIGHLIGHT  AUTO.
      CALL "CBL_TOUPPER" USING WS-ADDR
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
            DISPLAY S09.
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
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE 3   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
      PERFORM RESTORE-SCREEN-3.

       DA06.
             ACCEPT W45-CODE AT 1240 WITH FOREGROUND-COLOR Cyan HIGHLIGHT  AUTO.
           IF W45-CODE = ZERO
              GO TO DA990-CLOSE.
           IF W45-CODE > 80
              MOVE "Invalid code"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO DA06.
             ADD 121 W45-CODE    GIVING W90-RESULT.
             DIVIDE W90-RESULT BY 2
                                 GIVING W90-CATKEY
                                 REMAINDER W90-CS.
             ADD 1               TO W90-CS.
             MOVE W90-CATKEY     TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             DISPLAY PAR-CDESC (W90-CS) AT 1244
                     WITH FOREGROUND-COLOR Cyan HIGHLIGHT .
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO DA06.
             DISPLAY "A" AT 1412 WITH FOREGROUND-COLOR Cyan HIGHLIGHT 
                     "ccount or " WITH FOREGROUND-COLOR Cyan
                     "N" WITH FOREGROUND-COLOR Cyan HIGHLIGHT 
                     "ame sequence" WITH FOREGROUND-COLOR Cyan.
             MOVE "N"            TO WS-SEQ.
       DA07.
             ACCEPT WS-SEQ AT 1437 WITH FOREGROUND-COLOR Grey HIGHLIGHT  UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-SEQ
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-SEQ = "A" OR "N")
               GO TO DA07.
             DISPLAY "Debtor account:" AT 1612.

       DA08.
             MOVE 0 TO WS-PAGE W90-BAL W90-CUR W90-30 W90-60
                               W90-90 W90-120 W90-BDT W90-BCR.

       DA10-HEADING.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE SPACES  TO REP-DETAIL1.
            MOVE "PAGE:"  TO TRL-PAGE.
            MOVE "DATE:"  TO TRL-DATE.
            ADD 1   TO WS-PAGE.
            MOVE W12-TODAY  TO TRL-DTE.
            MOVE WS-PAGE  TO TRL-P-NO.
      *
      *    ****   T E S T   I F   O U T P U T T O   P R I N T E R
      *    O R S P O O L  F I L E
      *
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        MOVE SPACES  TO TRL-LINE1
        MOVE "E"   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
        MOVE SPACES  TO TRL-LINE1.
            MOVE W95-COMP  TO TRL-CO.
             MOVE " - DEBTORS PER CATEGORY" TO TRL-EXHEAD.
      MOVE "H"   TO W02-PRN-TYPE.
      *
      *    ****   T E S T   I F   O U T P U T T O   P R I N T E R
      *    O R S P O O L  F I L E
      *
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO TRL-LINE1.
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE "X"   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
              MOVE 3   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE SPACES  TO TRL-DET.

       DA12-HEADING.
            MOVE "CATEGORY:"  TO CAT-H1.
            MOVE W45-CODE  TO CAT-CODE.
            MOVE PAR-CDESC (W90-CS)
     TO CAT-DESC.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO TRL-DET.
            MOVE "AC-NO"  TO TRL-HEAD1.
            MOVE "NAME"  TO TRL-HEAD2.
            MOVE "ACTIVE"  TO TRL-HEAD3.
            MOVE "MTH"   TO TRL-HEAD4.
           IF WS-AGE = "Y"
              MOVE "30 DAYS"  TO TRL-HEAD5
              MOVE "60 DAYS"  TO TRL-HEAD6
              MOVE "90 DAYS"  TO TRL-HEAD7
              MOVE "120 DAYS"  TO TRL-HEAD8
           ELSE
              MOVE "CURRENT"  TO TRL-HEAD5
              MOVE "30 DAYS"  TO TRL-HEAD6
              MOVE "60 DAYS"  TO TRL-HEAD7
              MOVE " 90 DAYS"  TO TRL-HEAD8.
            MOVE "INTEREST"  TO TRL-HEAD9.
            MOVE "PAST DUE"  TO TRL-HEAD10.
            MOVE "C-LIMIT"  TO TRL-HEAD11.
            MOVE "BALANCE"  TO TRL-HEAD12.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO TRL-DET.
      MOVE "D"   TO W02-PRN-TYPE.

       DA15-INDEX.
             MOVE " !"           TO DEB-NAME DEB-ACNO.
           IF WS-SEQ = "A"
               PERFORM START-AT-DEBT-AC THRU READ-DEBTOR-EXIT
           ELSE
               PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
              GO TO DA26.

       DA20.
             PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
              GO TO DA25.
      *
      *    ****   T E S T   I F   O U T P U T T O   P R I N T E R
      *    O R S P O O L  F I L E
      *
    IF NOT (WS-PRINTER = "S")
        DISPLAY DEB-ACNO AT 1628 WITH FOREGROUND-COLOR Cyan HIGHLIGHT .
           IF W45-CODE NOT = DEB-CAT
               GO TO DA20.
            MOVE DEB-NAME  TO W40-NAME.
           IF DEB-TYPE = 1
               INSPECT W40-NAME REPLACING ALL "*" BY "."
               INSPECT W40-NAME REPLACING FIRST "!" BY " ".
            MOVE DEB-ACNO  TO TRL-AC.
            MOVE W40-NAME  TO TRL-NME.
            MOVE DEB-ACTIVE  TO W22-DTE2.
      PERFORM SWITCH-DATE-TO-DMCY.
      MOVE W22-DTE3  TO TRL-ACT.
            ADD DEB-30 DEB-60 DEB-90 DEB-120
     GIVING W25-VALUE.
            ADD DEB-BAL  TO W90-BAL.
            ADD DEB-CUR  TO W90-CUR.
            ADD DEB-30   TO W90-30.
            ADD DEB-60   TO W90-60.
            ADD DEB-90   TO W90-90.
            ADD DEB-120  TO W90-90.
            ADD DEB-INT  TO W90-INT.
           IF DEB-BAL > 0
              ADD DEB-BAL  TO W90-BDT
           ELSE
              ADD DEB-BAL  TO W90-BCR.
           IF DEB-BAL > 0
              SUBTRACT W25-VALUE FROM DEB-BAL
     GIVING W25-TOTAL
              SUBTRACT DEB-CUR  FROM W25-TOTAL
              ADD W25-TOTAL  TO W90-CUR.
            MOVE W25-VALUE  TO TRL-PD.
            MOVE DEB-BAL  TO TRL-BAL.
           IF (DEB-90 > 0) OR (DEB-120 > 0)
              MOVE 3   TO TRL-MTH
           ELSE
           IF DEB-60 > 0
              MOVE 2   TO TRL-MTH
           ELSE
           IF DEB-30 > 0
              MOVE 1   TO TRL-MTH.
             COMPUTE TRL-CUR = DEB-BAL - (DEB-INT + DEB-30
                                         + DEB-60 + DEB-90 + DEB-120)
            MOVE DEB-30  TO TRL-30.
            MOVE DEB-60  TO TRL-60.
            ADD DEB-90 DEB-120  GIVING TRL-90.
            MOVE DEB-INT  TO TRL-INT.
            MOVE DEB-CRL  TO TRL-LIMIT.
           IF DEB-CRL > ZERO
               IF DEB-BAL > DEB-CRL
           MOVE "*"  TO TRL-EXL.
           IF WS-ADDR = "Y"
               GO TO DA22.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM DA10-HEADING THRU DA12-HEADING.
            MOVE SPACES  TO TRL-DET.
             PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              GO TO DA990-CLOSE.
             GO TO DA20.

       DA22.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO TRL-DET.
            MOVE DEB-ADDRESS  TO W60-ADDRESS.
             PERFORM CA175-ADDRESS.
            MOVE W40-ADD1  TO TRL-NME.
      PERFORM CALL-PRINTUTL.
          IF W40-ADD2 = SPACES
               GO TO DA22-PCDE.
            MOVE W40-ADD2  TO TRL-NME.
      PERFORM CALL-PRINTUTL.
          IF W40-ADD3 = SPACES
               GO TO DA22-PCDE.
            MOVE W40-ADD3  TO TRL-NME.
      PERFORM CALL-PRINTUTL.
          IF W40-ADD4 = SPACES
               GO TO DA22-PCDE.
            MOVE W40-ADD4  TO TRL-NME.
      PERFORM CALL-PRINTUTL.

       DA22-PCDE.
            MOVE SPACES  TO TRL-DET.
            MOVE DEB-POST  TO TRL-PCDE.
            MOVE "TEL NO'S:"  TO TRL-HEAD13.
            MOVE DEB-H-TEL  TO TRL-HTEL.
            MOVE DEB-B-TEL  TO TRL-BTEL.
    IF W02-LINAGE < (W02-PRN-LENGTH - 4)
        MOVE 2   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
          ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM DA10-HEADING THRU DA12-HEADING.
            MOVE SPACES  TO TRL-DET.
             PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              GO TO DA990-CLOSE.
             GO TO DA20.

       DA25.
      MOVE "S"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
      MOVE "P"   TO WS-COMMAND.
            PERFORM DA10-HEADING.
            MOVE "SUMMARY"  TO TRL-HEADER.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO TRL-DET.
           IF WS-AGE = "Y"
              MOVE "30 DAYS"  TO TRL-SUM
           ELSE
              MOVE "CURRENT"  TO TRL-SUM.
            MOVE W90-CUR  TO TRL-VAL.
      PERFORM CALL-PRINTUTL.
      MOVE "D"   TO W02-PRN-TYPE.
    IF WS-AGE = "Y"
              MOVE "60 DAYS"  TO TRL-SUM
           ELSE
              MOVE "30 DAYS"  TO TRL-SUM.
            MOVE W90-30  TO TRL-VAL.
      PERFORM CALL-PRINTUTL.
    IF WS-AGE = "Y"
              MOVE "90 DAYS"  TO TRL-SUM
           ELSE
              MOVE "60 DAYS"  TO TRL-SUM.
            MOVE W90-60  TO TRL-VAL.
      PERFORM CALL-PRINTUTL.
    IF WS-AGE = "Y"
              MOVE "120 DAYS"  TO TRL-SUM
           ELSE
              MOVE "90 DAYS"  TO TRL-SUM.
            MOVE W90-90  TO TRL-VAL.
      PERFORM CALL-PRINTUTL.
            MOVE "INTEREST"  TO TRL-SUM.
            MOVE W90-INT  TO TRL-VAL.
      PERFORM CALL-PRINTUTL.
            MOVE "CREDIT BALANCES"
     TO TRL-SUM.
            MOVE W90-BCR  TO TRL-VAL.
      PERFORM CALL-PRINTUTL.
            MOVE "DEBIT BALANCES"
     TO TRL-SUM.
            MOVE W90-BDT  TO TRL-VAL.
      MOVE 3   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE "CATEGORY TOTAL"
     TO TRL-SUM.
            MOVE W90-BAL  TO TRL-VAL.
      MOVE 99   TO WS-ADVANCE.
      MOVE "E"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
      MOVE "P"   TO WS-COMMAND.

       DA26.
      PERFORM RESTORE-SCREEN-3.
             GO TO DA06.

       DA990-CLOSE.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.

       DA999-EXIT.
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
               WHEN 15         MOVE WS-PARID      TO WS-FILE
                               MOVE WS-PARKEY     TO WS-KEY
               WHEN OTHER      MOVE "ERROR"       TO WS-FILE
                               MOVE "DTP024"      TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
