      $set LINKCOUNT"320" GNT"GLP019.GNT"
      ******************************************************************
      *                                                                *
      *    ******   **        *******     ****       **      ******    *
      *   **    **  **        **    **   **  **     ***     **    **   *
      *   **        **        **    **  **    **     **     **    **   *
      *   **        **        *******   **    **     **      *******   *
      *   **   ***  **        **        **    **     **           **   *
      *   **    **  **        **         **  **      **     **    **   *
      *    ******   ********  **          ****     ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     P R I N T   C H E Q U E S   I N   N U M B E R   S E Q      *
      *                                                                *
      *       Version 9.04.02 - June 2017                              *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.             GLP019.
       AUTHOR.                 J.W. LEMMON (APAC).
       DATE-WRITTEN.           JUNE 1983.

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

       COPY "PARAM.SL".

       COPY "GACCNT.SL".

       COPY "GLTRAN.SL".

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

       COPY "GACCNT.FDE".

       COPY "GLTRAN.FDE".

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
       77  WS-PARKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-GLKEY                PIC  9(04)    COMP-5 VALUE 1.
       77  WS-GTRKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-GINKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-NETKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-KSTORE               PIC  9(04)    COMP-5.
       77  WS-ISUB                 PIC  9(04)    COMP-5.
       77  WS-CHEQUE               PIC  9(04)    COMP-5.
       77  WS-PAGE                 PIC  9(04)    COMP-5.
       77  WS-LINES                PIC  9(02)           VALUE 66.
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S5                   PIC  9(04)    COMP-5.
       77  WS-S6                   PIC  9(04)    COMP-5.
       77  WS-S7                   PIC  9(04)    COMP-5.
       77  WS-S8                   PIC  9(04)    COMP-5.
       77  WS-S9                   PIC  9(04)    COMP-5.
       77  WS-S10                  PIC  9(04)    COMP-5.
       77  WS-S11                  PIC  9(04)    COMP-5.
       77  WS-IXS1                 PIC  9(04)    COMP-5.
       77  WS-OPEN                 PIC  9(02)           VALUE ZERO.
       77  WS-EXS                  PIC  9(01).
       77  WS-NUM                  PIC  Z9.
       77  WS-SCH                  PIC  X(01).
       77  WS-RECON                PIC  X(01).
       77  WS-ZERO                 PIC  9(01).
       77  WS-MM                   PIC  9(02)    COMP-5 VALUE ZERO.
       77  WS-MTH                  PIC  9(02)    COMP-5.
       77  WS-F-YR                 PIC  9(02).
       77  WS-E-YR                 PIC  9(02).
       77  WS-OPTION               PIC  X(01).
       77  WS-ERROR                PIC  9(01)           VALUE ZERO.
       77  WS-PGE-LENGTH           PIC  9(02)           VALUE 66.
       77  WS-PRN-LENGTH           PIC  9(02)           VALUE 62.
       77  WS-TNEXT                PIC  9(04)    COMP-5.
       77  WS-PRN-NO               PIC  X(01)    COMP-X VALUE 9.
       77  WS-PRN-STAT             PIC  X(01)    COMP-X.
       77  WS-H1                   PIC  X(05)           VALUE "DATE:".
       77  WS-H3                   PIC  X(06)           VALUE "BATCH:".
       77  WS-H4                   PIC  X(05)           VALUE "PAGE:".
       77  WS-SPC                  PIC  X(30)           VALUE SPACES.
       77  WS-ER1                  PIC  X(07)           VALUE "Account".
       77  WS-ER3                  PIC  X(04)           VALUE "Date".
       77  WS-ER4                  PIC  X(05)           VALUE "Value".
       77  WS-ER5                  PIC  X(13)           VALUE "Cheque number".
       77  WS-ER6                  PIC  X(09)           VALUE "No Record".
       77  WS-ER7                  PIC  X(13)           VALUE "Group Heading".
       77  PRG-NAME                PIC  X(12)           VALUE SPACES.
       77  PRG-GLED-LUP            PIC  X(12)           VALUE "GLP\GLPLOOK".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *         C =  Clear comment line (50)
      *         E =  Clear lines (any line or lines between 5 and 46)
      *         H =  Change heading
      *         S =  Clear the screen and display headings
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "GLP019".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "GENERAL LEDGER -PRINT CHEQUES IN NO SEQ".
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
    03  WS-MODULE       PIC  X(03) VALUE "GLP".
    03  WS-PROG        PIC  X(03) VALUE "019".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

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

           03  R-L1                              REDEFINES W02-PRN-LINE.
               05  R-DET           PIC  X(140).

           03  R-L2                              EDEFINES W02-PRN-LINE.
               05  R-H1            PIC  X(06).
               05  R-DTE1          PIC  Z9/99/9999.
               05  FILLER          PIC  X(29).
               05  R-H2            PIC  X(40).
               05  R-DESC          PIC  X(38).
               05  R-H4            PIC  X(06).
               05  R-PGE           PIC  ZZ9.

           03  LDG-L2                            REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(40).
               05  LDG-H3          PIC  X(52).
               05  FILLER          PIC  X(40).

           03  LED-L1                            REDEFINES W02-PRN-LINE.
               05  LED-H1          PIC  X(07).
               05  FILLER          PIC  X(02).
               05  LED-H2          PIC  X(12).
               05  FILLER          PIC  X(06).
               05  LED-H2A         PIC  X(10).
               05  FILLER          PIC  X(02).
               05  LED-H3          PIC  X(04).
               05  FILLER          PIC  X(04).
               05  LED-H4          PIC  X(04).
               05  FILLER          PIC  X(03).
               05  LED-H4A         PIC  X(05).
               05  FILLER          PIC  X(04).
               05  LED-H5          PIC  X(05).
               05  FILLER          PIC  X(02).
               05  LED-H6          PIC  X(15).
               05  FILLER          PIC  X(28).
               05  LED-H7          PIC  X(05).
               05  FILLER          PIC  X(06).
               05  LED-H8          PIC  X(07).
               05  FILLER          PIC  X(01).

           03  LED-L2                            REDEFINES W02-PRN-LINE.
               05  LED-AC          PIC  9(04)/99.
               05  FILLER          PIC  X(02).
               05  LED-NME         PIC  X(24).
               05  FILLER          PIC  X(04).
               05  LED-TRAN.
                   07  LED-DTE     PIC  Z9/99/9999.
                   07  FILLER      PIC  X(01).
                   07  LED-CDE     PIC  X(04).
                   07  FILLER      PIC  X(01).
                   07  LED-BTCH    PIC  Z(06).
                   07  FILLER      PIC  X(02).
                   07  LED-REF     PIC  Z(07).
                   07  FILLER      PIC  X(02).
                   07  LED-NAR     PIC  X(36).
                   07  FILLER      PIC  X(01).
                   07  LED-VAL     PIC  Z(07)9.99-.
                   07  FILLER      PIC  X(01).
                   07  LED-BAL     PIC  Z(07)9.99-.

           03  REC-L1                            REDEFINES W02-PRN-LINE.
               05  REC-AC          PIC  9(04)/99.
               05  FILLER          PIC  X(02).
               05  REC-NME         PIC  X(40).
               05  FILLER          PIC  X(83).

           03  REC-L2                            REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(32).
               05  REC-REC         PIC  X(01).
               05  FILLER          PIC  X(04).
               05  REC-TRAN.
                   07  REC-DTE     PIC  Z9/99/9999.
                   07  FILLER      PIC  X(01).
                   07  REC-CDE     PIC  X(04).
                   07  FILLER      PIC  X(01).
                   07  REC-BTCH    PIC  Z(06).
                   07  FILLER      PIC  X(02).
                   07  REC-REFER.
                   09  REC-REF     PIC  Z(07).
                   07  FILLER      PIC  X(02).
                   07  REC-NAR     PIC  X(36).
                   07  FILLER      PIC  X(01).
                   07  REC-VAL     PIC  Z(07)9.99-.
                   07  FILLER      PIC  X(01).
                   07  REC-BAL     PIC  Z(07)9.99-.

       COPY "W05.GL".

       01  W10-H2.
           03  FILLER              PIC  X(27).
           03  W10-HEAD            PIC  X(13).

       COPY "W15.GL".

       01  W20-TOTALS.
           03  W20-DEBIT       PIC S9(09)V99 COMP-3.
           03  W20-CREDIT      PIC S9(09)V99 COMP-3.
           03  W20-DR          PIC S9(09)V99 COMP-3.
           03  W20-CR          PIC S9(09)V99 COMP-3.
           03  W20-RESULT      PIC S9(09)V99.
           03  W20-RES1 REDEFINES W20-RESULT.
               05  W20-WHOLE   PIC 9(09).
               05  W20-DEC     PIC S9(02).
           03  W20-TOTAL       PIC S9(09)V99 COMP-3.

       COPY "W20.WS".

       01  W25-CALCS.
           03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
               05  W25-WHOLE   PIC 9(05).
               05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(04).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).

       01  W25-EDIT.
           03  W25-7.
               05  W25-1       PIC ZZZZZZZ9-.
           03  W25-S7V2.
               05  W25-V1      PIC Z(07)9.99CR.
          03  W25-S8V2.
              05  W25-VG1     PIC Z(07)9.99CR.
          03  W25-DATE.
              05  W25-DTE     PIC Z9/99/9999.
           03  W25-REF.
               05  W25-REFER   PIC Z(07).

       01  W30-INACTS.
           02  W30-TAB1.
           03  W30-INCOME-TABLE OCCURS 50.
               05  W30-INACNO  PIC 9(04).
               05  W30-INKEY   PIC 9(05)  COMP-0.
           02  W30-TAB2 REDEFINES W30-TAB1.
               05  W30-ING OCCURS 50.
                   07 W30-INGRP PIC 9(02).
                   07 W30-INSAC PIC 9(04).
           02  W30-TAB3 REDEFINES W30-TAB1.
               05  W30-INGS OCCURS 50.
                   07  FILLER  PIC 9(01).
                   07 W30-INGSUB PIC 9(03).
                   07  FILLER  PIC 9(02).

       01  W30-COACTS.
           02  W30-TAB4.
           03  W30-COST-TABLE OCCURS 50.
               05  W30-COACNO  PIC 9(04).
               05  W30-COKEY   PIC 9(05)  COMP-0.
           02  W30-TAB5 REDEFINES W30-TAB4.
               05  W30-CNG OCCURS 50.
                   07 W30-COGRP  PIC 9(02).
                   07 W30-COSAC  PIC 9(04).
           02  W30-TAB6 REDEFINES W30-TAB4.
               05  W30-COGS OCCURS 50.
                   07 FILLER     PIC 9(01).
                   07 W30-COGSUB PIC 9(03).
                   07 FILLER     PIC 9(02).
       01  W30-MONTHS.
           03  W30-MONTH-NAMES.
               05  W30-ENG     PIC  X(36)    VALUE
                               "JanFebMarAprMayJunJulAugSepOctNovDec".
           03  W30-MONTH REDEFINES W30-MONTH-NAMES
                               PIC  X(03)  OCCURS 12.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50-70.GL".

       COPY "W70.WS".

       01  W80-HEAD.
           03  W80-H1          PIC X(22).
           03  FILLER          PIC X(18) VALUE "FOR PERIOD ENDING".
          03  W80-DATE        PIC Z9/99/9999.

       01  W90-TOTALS.
           03  W90-ACCUM.
               05  W90-V1      PIC S9(09)V99 COMP-3.
               05  W90-V2      PIC S9(09)V99 COMP-3.
               05  W90-V3      PIC S9(09)V99 COMP-3.
               05  W90-V4      PIC S9(09)V99 COMP-3.
               05  W90-V5      PIC S9(09)V99 COMP-3.
               05  W90-V6      PIC S9(09)V99 COMP-3.
               05  W90-V7      PIC S9(09)V99 COMP-3.
               05  W90-V8      PIC S9(09)V99 COMP-3.
               05  W90-V9      PIC S9(09)V99 COMP-3.
               05  W90-V10     PIC S9(09)V99 COMP-3.
               05  W90-V11     PIC S9(09)V99 COMP-3.
               05  W90-V12     PIC S9(09)V99 COMP-3.
               05  W90-V13     PIC S9(09)V99 COMP-3.
               05  W90-V14     PIC S9(09)V99 COMP-3.
               05  W90-V15     PIC S9(09)V99 COMP-3.
               05  W90-V16     PIC S9(09)V99 COMP-3.
               05  W90-V17     PIC S9(09)V99 COMP-3.
               05  W90-V18     PIC S9(09)V99 COMP-3.
           03  W90-TOT  REDEFINES W90-ACCUM.
               05  W90-VAL     PIC S9(09)V99 COMP-3 OCCURS 18.
           03  W90-PER         PIC 9(03)V9(06) COMP-3.
       01  W90-COMP.
           03  W90-CNAME       PIC X(40).

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
       01  SA-INPUT.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.

               05  S16.
                   07  PIC Z(06)9 USING W15-REFER AUTO.

               05  S17.
                   07  PIC 9(06) USING W15-ACCOUNT AUTO.

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
       AA000        SECTION.
       AA00.
           IF LS0-GLLEV < 1
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA35.
             PERFORM ZA000.
             PERFORM IA000.

       AA35.
             EXIT PROGRAM.

       AA50.
             MOVE "H"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DET.
             MOVE WS-H1              TO R-H1.
             MOVE W90-COMP           TO R-H2.
             MOVE WS-H4              TO R-H4.
             ADD 1                   TO WS-PAGE.
             MOVE WS-PAGE            TO R-PGE.
             MOVE W05-TODAY          TO R-DTE1.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.

       AA55.
             EXIT.

       COPY "FUNCTION.SCR".

       COPY "ERROR.SCR".

       COPY "OPTION.CRT".

       COPY "LOCKED.REC".

       COPY "CLEAR.CRT".

       COPY "ABORT.PRN".

       COPY "PRINTUTL.AP6".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ This routine is used to call  the General Ledger Accounts ³
      *    ³ lookup program and cancel it when control is returned.    ³
      *    ³ The  relevant  error  message  will be  displayed  if  an ³
      *    ³ exception occurs. The usual  parameters are passed to the ³
      *    ³ lookup program as well W15-GLEDGER which will contain the ³
      *    ³ details of the account that has been selected when control³
      *    ³ is returned to the calling program.                       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AA350-LOOKUP    SECTION.
       AA350.
             PERFORM SAVE-SCREEN.
             PERFORM SCREEN-CONTENTS.
             CALL PRG-GLED-LUP USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS W15-GLEDGER
                  ON EXCEPTION GO TO AA355
             END-CALL.
             CANCEL PRG-GLED-LUP.
             GO TO AA358.

       AA355.
             MOVE SPACE              TO WS-ERR-MES.
             STRING "Program- " DELIMITED SIZE PRG-GLED-LUP DELIMITED " " " not on disk, press ANY key" DELIMITED SIZE INTO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.

       AA358.
             PERFORM RESTORE-SCREEN.

       AA399.
             EXIT.

       COPY "AA900.ALM".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AB000           SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "GACCNT.RD1".

       COPY "GLTRAN.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AC000           SECTION.

       COPY "CONTROL.WR".

       COPY "GACCNT.LUP".

       COPY "APAC.HLP".

      /
      *    ****    C H E Q U E S   I N   N U M B E R   S E Q U E N C E
      *
       IA000      SECTION 5.
       IA00.
             PERFORM ERASE-SCREEN.
             DISPLAY "CHEQUES IN NUMBER SEQUENCE" AT 0228
              WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             MOVE "CHEQUES : NUMBER SEQ- " TO W80-H1.
             MOVE W05-TODAY      TO W80-DATE.
             MOVE W70-BNKGL      TO W15-ACCOUNT.
             DISPLAY "Start at Cheque Number:" AT 0412.

       IA02.
            ACCEPT S16 AT 0436.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO IA50
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
       GO TO IA02
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO IA02
        END-EVALUATE.
          IF W15-REFER = ZERO
              GO TO IA50.
      GO TO IA10.

       IA05.
             PERFORM AA50 THRU AA55.
             MOVE W80-HEAD           TO LDG-H3.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE ALL "-"            TO LDG-H3.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE "ACNT NO"          TO LED-H1.
             MOVE "ACCOUNT NAME"     TO LED-H2.
             MOVE "RECONCILED"       TO LED-H2A.
             MOVE "DATE"             TO LED-H3.
             MOVE "CODE"             TO LED-H4.
             MOVE "BATCH"            TO LED-H4A.
             MOVE "REFER"            TO LED-H5.
             MOVE "PAYEE/NARRATIVE"  TO LED-H6.
             MOVE "DEBIT"            TO LED-H7.
             MOVE " CREDIT"          TO LED-H8.
             PERFORM CALL-PRINTUTL.
             MOVE ALL "-"            TO LED-H1 LED-H2 LED-H2A LED-H3 LED-H4 LED-H4A LED-H5 LED-H6 LED-H7 LED-H8.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE "D"                TO W02-PRN-TYPE.

       IA10.
             DISPLAY "Account:" AT 0812.
      PERFORM SAVE-SCREEN-3.
    IF NOT (W02-PRN-STATUS = "O")
        PERFORM OPEN-PRINTER
        MOVE "P"   TO WS-COMMAND
      *
      *    ****   1 7 C P I ( C O N D E N S E D   P R I N T )
      *
        MOVE 0   TO WS-ADVANCE
        MOVE 3   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
            MOVE ZERO   TO WS-PAGE.
      PERFORM RESTORE-SCREEN-3.

       IA12.
      DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "=Help,"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "F2" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       " to do Account Lookup - "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "Esc" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "ape to exit"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
            ACCEPT S17 AT 0821.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE ZERO  TO W15-ACCOUNT
       GO TO IA50
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA350
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO IA12
        END-EVALUATE
              DISPLAY S17 AT 0821
               IF W15-ACCOUNT = ZERO
                   GO TO IA12.
            PERFORM CLEAR-L50.
           IF W15-ACCOUNT = ZERO
               GO TO IA50.
          IF W15-SER = ZERO
        MOVE "Group Heading"
           TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
                GO TO IA12.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
              MOVE "No Record"  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO IA12.
      DISPLAY G-NAME AT 0828
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
       IA15.
      MOVE "'C'ontinue or 'R'ekey  [C]"
     TO WS-ERR-MES.
             MOVE "C"            TO WS-OPTION.
      PERFORM OPT-MESSAGE.
    IF NOT (WS-OPTION = "C" OR "R")
               GO TO IA15.
           IF WS-OPTION = "R"
               GO TO IA12.

       IA20.
             PERFORM IA05.

       IA23.
             MOVE G-AC           TO REC-AC.
             MOVE G-NAME         TO REC-NME.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.

       IA24.
      MOVE ZERO   TO W20-DR W20-CR.
      INITIALIZE T-REC.
             MOVE 04             TO T-CODE.
      MOVE W15-REFER  TO T-REF.
             PERFORM START-AT-GLTRAN-AKEY THRU READ-GLTRAN-EXIT.
    IF WS-F-ERROR = 11
               GO TO IA35.
    IF NOT (WS-PRINTER = "S")
        DISPLAY "Processing Transaction :" AT 1212.
       IA25.
             PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
          IF (WS-F-ERROR = 11) OR
       (T-CODE > 04)
              GO TO IA35.
    IF NOT (T-AC = G-AC)
        GO TO IA25.

       IA26.
    IF NOT (WS-PRINTER = "S")
        DISPLAY T-KEY AT 1237
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta.
           IF T-SUB = 5
               GO TO IA30.
             MOVE T-DATE         TO W15-YMD.
      MOVE W15-CN  TO W15-CC.
             MOVE W15-YR         TO W15-YY.
             MOVE W15-MT         TO W15-MM.
             MOVE W15-DY         TO W15-DD.
             MOVE W15-DATE       TO REC-DTE.
            MOVE "CHEQ"  TO REC-CDE.
             MOVE T-BATCH        TO REC-BTCH.
             MOVE T-REF          TO REC-REF.
           IF T-FLAG = "Y"
        MOVE "*"   TO REC-REC.
    IF T-VAL > ZERO
        ADD T-VAL  TO W20-DR
        MOVE T-VAL  TO REC-VAL
           ELSE
        MOVE T-VAL  TO REC-BAL
        ADD T-VAL  TO W20-CR.
       IA30.
             MOVE T-NARATIVE     TO REC-NAR.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM IA05
               PERFORM IA23.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
     TO R-DET
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        GO TO IA50.
            MOVE SPACES  TO R-DET.
             GO TO IA25.

       IA35.
          IF W02-LINAGE < (W02-PRN-LENGTH - 4)
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM IA05
               PERFORM IA23.
            MOVE "*****  E N D   O F   L I S T  *****"
                                 TO REC-NAR.
            MOVE W20-DR  TO REC-VAL.
      MOVE W20-CR  TO REC-BAL.
      MOVE 99   TO WS-ADVANCE.
      MOVE "E"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
      PERFORM RESTORE-SCREEN-3.

       IA50.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.

       IA999.
             EXIT.
      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000      SECTION 9.
       ZA00.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W05-TODAY.
             MOVE LS-TODAY-YYMMDD    TO W05-TYMD.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY W90-CNAME.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *
      *    ****    D O   N O T   D I S P L A Y   E R R O R
      *            M E S S A G E S
      *
      *      MOVE 44   TO USER-NUMBER.
      *      MOVE 1    TO USER-SETTING.
      *      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.PRO".

       ZA02.
             MOVE 6                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-UNPROF         TO W70-UNPROF.
             MOVE PAR-BNKGL          TO W70-BNKGL.
             MOVE 4                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE G-OPEN             TO WS-OPEN.
             ADD 1 G-YEAR-END        GIVING WS-F-YR.
           IF WS-F-YR > 12
               SUBTRACT 12           FROM WS-F-YR.
             MOVE G-YEAR-END         TO WS-E-YR.
             MOVE 1                  TO WS-S1.
       ZA05.
             MOVE ZERO               TO WS-ERROR.
           IF W70-BNKGL = ZERO
               DISPLAY (20, 20) "Parameters not Set up - ENTER"
                                 WITH FOREGROUND-COLOR Brown HIGHLIGHT
               ACCEPT (20, 50) WS-OPTION WITH FOREGROUND-COLOR Grey HIGHLIGHT AUTO
              MOVE 1   TO WS-ERROR.
      DISPLAY SPACES AT 5025
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
            GO TO ZA999.

      *COPY "ZA49.PRO".
      *
      *ZA200.
      *
      *COPY "LOCKED.PRO".
      *
      *ZA205.
      *      EXIT PROGRAM.

       ZA999.
             EXIT.
      *
      *     I / O   E R R O R   M E S S A G E S
      *
       ZB000      SECTION 9.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 2          MOVE W02-NETWORK  TO WS-FILE
                               MOVE WS-NETKEY    TO WS-KEY
               WHEN 8          MOVE W02-GACCNT   TO WS-FILE
                               MOVE G-AC         TO WS-KEYX
               WHEN 11         MOVE W02-GLTRAN   TO WS-FILE
                               MOVE T-KEY        TO WS-KEYX
               WHEN 15         MOVE WS-PARID     TO WS-FILE
                               MOVE WS-PARKEY    TO WS-KEY
               WHEN OTHER      MOVE "ERROR"      TO WS-FILE
                               MOVE "GLP019"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
