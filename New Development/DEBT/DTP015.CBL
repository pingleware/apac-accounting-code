      $SET LINKCOUNT"512" GNT"DTP015.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****       **     ********   *
      *   **    **     **     **    **   **  **     ***     **         *
      *   **    **     **     **    **  **    **     **     *******    *
      *   **    **     **     *******   **    **     **           **   *
      *   **    **     **     **        **    **     **           **   *
      *   **    **     **     **         **  **      **     **    **   *
      *   *******      **     **          ****    ********   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *  D E B T O R   U N A L L O C A T E D   C R E D I T S   P R O G *
      *                                                                *
      *  This program finds the unallocated credits in the Accounts    *
      *  Receivable transaction file. Accounts that have unallocated   *
      *  credits are displayed and the User is then able to have these *
      *  transactions allocated.                                       *
      *                                                                *
      *  STILL TO BE INCLUDED.                                         *
      *  If the balance is zero, the program will allow the User to    *
      *  delete the account.                                           *
      *                                                                *
      *       Version 9.00.82 - April 2016                             *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     DTP015.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   December 2009.

                COPYRIGHT NOTICE: COPYRIGHT (C) 2009 - 2016
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

       COPY "DBTRAN.FDE".

       COPY "DEBTOR.FDE".

       COPY "DEBTRN.FD".

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
       77  WS-SUB-1                PIC  9(04)    COMP-5.
       77  WS-SUB-2                PIC  9(04)    COMP-5.
       77  WS-SUB-3                PIC  9(04)    COMP-5.
       77  WS-SUB-4                PIC  9(04)    COMP-5.
       77  WS-SUB-5                PIC  9(04)    COMP-5.
       77  WS-SUB-6                PIC  9(04)    COMP-5.
       77  WS-IXS1                 PIC  9(04)    COMP-5.
       77  WS-IXS2                 PIC  9(04)    COMP-5.
       77  WS-IXS3                 PIC  9(04)    COMP-5.
       77  WS-IXS4                 PIC  9(04)    COMP-5.
       77  WS-MSUB                 PIC  9(04)    COMP-5.
       77  WS-DBTKEY               PIC  9(06)    COMP-5.
       77  WS-NETKEY               PIC  9(06)    COMP-5.
       77  WS-PARKEY               PIC  9(04)    COMP-5.
       77  WS-RECKEY               PIC  9(06)    COMP-5.
       77  WS-RECOVER              PIC  9(06)    COMP-5.
       77  WS-SHARED               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-SEL                  PIC  X(01)           VALUE "N".
       77  WS-TRANS                PIC  9(04)    COMP-5 VALUE 1.
       77  WS-LINES                PIC  9(02).
       77  WS-PAGE                 PIC  9(04)    COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-ACT                  PIC  X(01)           VALUE SPACE.
       77  WS-ALLOCATE             PIC  X(01)           VALUE "M".
       77  WS-SEQ                  PIC  X(01).
       77  WS-CREDIT               PIC  X(01).
       77  WS-ERROR                PIC  9(01).
       77  WS-ERR1                 PIC  X(22)           VALUE "Invalid Account Number".
       77  WS-ERR2                 PIC  X(09)           VALUE "No Record".
       77  WS-COMP                 PIC  9(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-HEAD                 PIC  X(01).
       77  WS-MESS                 PIC  X(02).
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S5                   PIC  9(04)    COMP-5.
       77  WS-S6                   PIC  9(04)    COMP-5.
       77  WS-S7                   PIC  9(04)    COMP-5.
       77  WS-S8                   PIC  9(04)    COMP-5.
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "DTP015".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "ACCOUNTS RECEIVABLE -UNALLOCATED CREDITS".
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
           03  WS-MODULE           PIC  X(03) VALUE "DTP".
           03  WS-PROG             PIC  X(03) VALUE "015".

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
          03  WS-SYS-ID            PIC  X(03).

       01  W10-TEMP.
           03  W10-DATE            PIC  X(10).
           03  W10-RDATE                         REDEFINES W10-DATE.
               05  W10-DTE         PIC  99/99/9999.

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

       COPY "W15.WS".

       01  W24-MONTHS.
           03  W24-MONTH-NAMES.
               05  FILLER          PIC  X(09)    VALUE " January ".
               05  FILLER          PIC  X(09)    VALUE " February".
               05  FILLER          PIC  X(09)    VALUE "  March  ".
               05  FILLER          PIC  X(09)    VALUE "  April  ".
               05  FILLER          PIC  X(09)    VALUE "   May   ".
               05  FILLER          PIC  X(09)    VALUE "   June  ".
               05  FILLER          PIC  X(09)    VALUE "   July  ".
               05  FILLER          PIC  X(09)    VALUE "  August ".
               05  FILLER          PIC  X(09)    VALUE "September".
               05  FILLER          PIC  X(09)    VALUE " October ".
               05  FILLER          PIC  X(09)    VALUE " November".
               05  FILLER          PIC  X(09)    VALUE " December".
           03  W24-EACH-MONTH                    REDEFINES W24-MONTH-NAMES.
               05  W24-MONTH       PIC  X(09)    OCCURS 12.

       01  W25-CALCS.
           03  W25-RESULT          PIC  9(05)V99.
           03  W25-RESULT1                       REDEFINES W25-RESULT.
               05  W25-WHOLE       PIC  9(05).
               05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2                       REDEFINES W25-RESULT.
               05  W25-KEY         PIC  9(04).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).
           03  W25-TOTAL           PIC S9(07)V99 COMP-3.
           03  W25-VALUE           PIC S9(07)V99 COMP-3.

       COPY "W40.DBT".

       COPY "W40.WS".

       01  W45-TRAN.
           03  W45-CODE            PIC  9(02).
           03  W45-ENG             PIC  X(12).
           03  W45-AFR             PIC  X(12).
           03  W45-ACT             PIC  X(01).

       COPY "FUNCTION.WS".

      *COPY "W50.WS".

       01  W55-STATS.
           03  W55-H1              PIC  X(09).
           03  W55-H2              PIC  X(09).
           03  W55-H3              PIC  X(09).
           03  W55-H4              PIC  X(09).
           03  W55-H5              PIC  X(09).
           03  W55-H6              PIC  X(09).
           03  W55-MONTHS.
               05  W55-MTH1        PIC  9(02)    COMP-5.
               05  W55-MTH2        PIC  9(02)    COMP-5.
               05  W55-MTH3        PIC  9(02)    COMP-5.
               05  W55-MTH4        PIC  9(02)    COMP-5.
               05  W55-MTH5        PIC  9(02)    COMP-5.
               05  W55-MTH6        PIC  9(02)    COMP-5.
           03  W55-VALUES.
               05  W55-VAL1        PIC S9(09)V99 COMP-3.
               05  W55-VAL2        PIC S9(09)V99 COMP-3.
               05  W55-VAL3        PIC S9(09)V99 COMP-3.
               05  W55-VAL4        PIC S9(09)V99 COMP-3.
               05  W55-VAL5        PIC S9(09)V99 COMP-3.
               05  W55-VAL6        PIC S9(09)V99 COMP-3.

       01  W60-NME-ADD.
           03  W60-NAME.
               05  W60-NCHAR       PIC  X(01)    OCCURS 40.
           03  W60-ADDRESS.
               05  W60-ACHAR       PIC  X(01)    OCCURS 60.

       01  W65-ENTRY.
           03  W65-FOUR.
               05  W65-CHAR        PIC  X(01)    OCCURS 8.
           03  W65-PREFIX                        REDEFINES W65-FOUR.
               05  W65-PREF        PIC  X(02).
               05  FILLER          PIC  X(06).

       01  W65-INDEX.
           03  W65-INPUT.
               05  W65-ICHAR       PIC X(01)     OCCURS 8.

       01  W75-KEYS.
           03  W75-S               PIC  9(02)    COMP-5.
           03  W75-S1              PIC  9(02)    COMP-5.
           03  W75-KEY             PIC  9(06)    COMP-5.
           03  W75-ALLOCATED       PIC S9(09)V99 COMP-3.
           03  W75-BALANCE         PIC S9(09)V99 COMP-3.
           03  W75-DNO             PIC  X(06)    OCCURS 18.

       COPY "W75.WS".

       01  W80-ACCOUNT.
           03  W80-ACNO            PIC  X(06).
           03  W80-NUMD                          REDEFINES W80-ACNO.
               05  W80-SERIAL      PIC  9(06).
           03  W80-TYPE            PIC  9(01).

       COPY "W90.DBT".

       01  W91-TRAN.
           03  W91-KEY.
               05  W91-AC          PIC  X(06).
               05  W91-DATE        PIC  9(08)    COMP.
               05  W91-CODE        PIC  9(02).
               05  W91-REF         PIC  X(08).
      *
      *    TYPE: "U" = Unpaid.         \
      *          "P" = Paid.            >  DEBIT TRANSACTIONS
      *          "X" = Partial payment./
      *
      *          "U" = Unallocated.    \
      *          "A" = Allocated.       >  CREDIT TRANSACTIONS
      *          "X" = Partial allocat./
      *
           03  W91-TYPE            PIC  X(01).
           03  W91-REG             PIC  X(09).
           03  W91-ORD             PIC  X(12).
           03  W91-VALUE           PIC S9(09)V99 COMP-3.
           03  W91-TAXFREE         PIC S9(09)V99 COMP-3.
           03  W91-TAX             PIC S9(07)V99 COMP-3.
           03  W91-DISC            PIC S9(03)V99 COMP-3.
           03  W91-PAID            PIC S9(09)V99 COMP-3.
           03  W91-RED-PAID                      REDEFINES W91-PAID
               05  W91-ALLOCATED   PIC S9(09)V99 COMP-3.
           03  W91-REC             PIC  X(08).
           03  W91-AGE             PIC  9(02)    COMP-5.
           03  FILLER              PIC  X(01).

       01  W95-STM.
           03  W95-COMP            PIC  X(40).
           03  W95-V2.
               05  W95-B           PIC  Z(06)9.99-.
           03  W95-TEL             PIC  X(14).

       01  W100-HEAD.
           03  FILLER              PIC X(40)    VALUE "ACCOUNTS RECEIVABLE- UNALLOCATED CREDITS".

       01  W100-EDIT.
           03  W100-LBAL           PIC  Z(06)9.99-.
           03  W100-LARR           PIC  Z(06)9.99-.
           03  W100-BAL            PIC  Z(08)9.99-.
           03  W100-CUR            PIC  Z(08)9.99-.
           03  W100-30             PIC  Z(08)9.99-.
           03  W100-60             PIC  Z(08)9.99-.
           03  W100-90             PIC  Z(08)9.99-.
           03  W100-120            PIC  Z(08)9.99-.
           03  W100-INT            PIC  Z(08)9.99.
           03  W100-CRL            PIC  Z(06)9.
           03  W100-KEY            PIC  Z(05)9.

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

       01  S18.
           03  FOREGROUND-COLOR Cyan HIGHLIGHT.
               05  LINE  8 COLUMN 26 PIC 99/99/9999 USING W15-START AUTO.
               05  LINE  9 COLUMN 26 PIC 99/99/9999 USING W15-END   AUTO.

       01  SD-TRAN.
           03  FOREGROUND-COLOR Cyan HIGHLIGHT.
               05  COLUMN 15 PIC  Z9/99/9999  FROM W22-DTE3.
               05  COLUMN 27 PIC  X(08)       FROM TRA-REF.
               05  COLUMN 39 PIC  99          FROM TRA-CODE.
               05  COLUMN 44 PIC  X(12)       FROM W00-E-DESC.
               05  COLUMN 58 PIC  Z(07)9.99CR FROM W25-VALUE.

       01  S08A.
           03  LINE 16 COLUMN  6 VALUE "Allocation:".
           03  LINE 17 COLUMN  6 VALUE "Ref. No.    Date    Transaction         Value      Balance     Allocate".

       01  S36.
           03  PIC X(06) USING W80-ACNO BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT AUTO.

       01  S-ALLOC.
    03  LINE 21 COLUMN 14 VALUE "Un-allocated balance : ".
    03        COLUMN 37 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC Z(08)9.99 FROM W75-BALANCE.
    03        COLUMN 51 VALUE "Allocated : ".
    03        COLUMN 63 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC Z(08)9.99 FROM W75-ALLOCATED.

       01  S-DTRN.
    03  COLUMN  6 FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta
                                   PIC X(08) FROM DBT-REF.
    03  COLUMN 15 FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta
       PIC 9999/99/99 FROM DBT-DATE.
    03  COLUMN 26 FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta
                                   PIC X(12) FROM W00-E-DESC.
    03  COLUMN 39 FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta
                                   PIC Z(08)9.99 FROM DBT-VALUE.
    03  COLUMN 52 FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta
                                   PIC Z(08)9.99 FROM DBT-OUTSTD.
    03  COLUMN 65 FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta
                                   PIC Z(08)9.99 USING DBT-ALLOC AUTO.

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
             PERFORM BA000.
             CLOSE RECOVER.

       AA49.
            EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
       AA50.
            PERFORM SAVE-SCREEN.
            MOVE 4                   TO W44-FUNCTION.
            PERFORM SCREEN-CONTENTS.
            PERFORM DEBTOR-LOOKUP.
            PERFORM RESTORE-SCREEN.
       AA99.
             EXIT.

       COPY "APAC.HLP".

       COPY "CLEAR.CRT".

       COPY "DATE.CHK".

      * ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      * ³ G E T   A   D E B T O R   T R A N S A C T I O N   R E C O R D³
      * ³                                                              ³
      * ³  Perform this routine with the transaction code in W90-CODE  ³
      * ³                                                              ³
      * ³  On exiting, the transaction code details are in W00-......  ³
      * ³  while W90-TRNKEY contains the key to the PARAMETER file     ³
      * ³  where this transaction code is found and W90-TS is the      ³
      * ³  transaction code subscript in the record.                   ³
      * ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       GET-DEBTOR-TRAN-RECORD SECTION.

       GET-DEBTOR-TRAN.
             ADD 21 W90-CODE         GIVING W90-RESULT.
             DIVIDE W90-RESULT BY 2  GIVING W90-TRNKEY REMAINDER W90-TS.
             ADD 1                   TO W90-TS.

       GET-DEBTOR-TRAN-CODE.
             MOVE W90-TRNKEY         TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.

       STORE-DEBTOR-TRAN-DESC.
             MOVE PAR-E-DESC(W90-TS) TO W00-E-DESC.
             MOVE PAR-A-DESC(W90-TS) TO W00-A-DESC.
             MOVE PAR-ACTION(W90-TS) TO W00-ACTION.
             MOVE PAR-T-CODE(W90-TS) TO W00-T-CODE.
             MOVE PAR-T-GL(W90-TS)   TO W00-T-GL.

       STORE-DEBTOR-TRAN-STATS.
             MOVE PAR-T-VAT(W90-TS)  TO W00-T-VAT.
             MOVE PAR-T-DAY(W90-TS)  TO W00-T-DAY.
             MOVE PAR-T-VAL(W90-TS)  TO W00-T-VAL.
             MOVE PAR-T-MTD(W90-TS)  TO W00-T-MTD.
             MOVE PAR-T-MTDV(W90-TS) TO W00-T-MTDV.
             MOVE PAR-T-YTD(W90-TS)  TO W00-T-YTD.
             MOVE PAR-T-YTDV(W90-TS) TO W00-T-YTDV.

       GET-DEBTOR-TRAN-EXIT.
             EXIT.

       GET-DEBTOR-TRAN-DESC SECTION.

       GET-DEBT-TRAN-DESC.
             PERFORM GET-DEBTOR-TRAN.
             MOVE W90-TRNKEY         TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             PERFORM STORE-DEBTOR-TRAN-DESC.

       GET-DEBT-TRAN-DESC-EXIT.
             EXIT.
      
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

       COPY "OPTION.CRT".

      *
      *    ****    READ FILES ROUTINES
      *
       AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "DBTRAN.RD1".

       COPY "DEBTOR.RD".

       COPY "DEBTRN.RD".

       COPY "PARAM.RD".

       COPY "SHARED.RD".

      *
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
       AD000             SECTION.

       COPY "CONTROL.WR".

       COPY "DBTRAN.WR".

       COPY "DEBTOR.WR".

       COPY "DEBTRN.WR".

       COPY "PARAM.WR".

       COPY "DEBTOR.LUP".

       COPY "DEBTOR.ALL".

      *
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
       AY000           SECTION.
       AY01.
             MOVE 01             TO REC-FILE.
             MOVE WS-PARKEY      TO REC-KEY.
             MOVE PAR-RECORD1    TO REC-PARAM.
             GO TO AY50.

       AY02.
             MOVE 02             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-DEBTOR-LOCK THRU READ-DEBTOR-EXIT.
             MOVE DEB-RECORD1    TO REC-DEBTOR.
             GO TO AY50.

       AY04.
             MOVE 04             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
             MOVE TRA-RECORD1    TO REC-DBTRAN.
             GO TO AY50.

       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.

       AY49.
             MOVE 99             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE SPACES         TO REC-DETAIL.
             PERFORM AY50 THRU AY59.
             ADD 1               TO WS-TRANS.
           IF WS-RECOVER > 95
               CLOSE RECOVER
               OPEN OUTPUT RECOVER
               CLOSE RECOVER
               OPEN I-O RECOVER
               MOVE ZERO         TO WS-RECOVER.
             GO TO AY59.

       AY50.
            ADD 1   TO WS-RECOVER.
             MOVE WS-RECOVER     TO WS-RECKEY.
             MOVE WS-TRANS       TO REC-TRAN.
             WRITE REC-RECORD.
           IF WS-STATUS NOT = "00"
               DISPLAY "Write error Recovery file - Status " AT 2212
                        WITH FOREGROUND-COLOR Brown HIGHLIGHT WS-STATUS
                        WITH FOREGROUND-COLOR Grey HIGHLIGHT
               STOP RUN.

       AY59.
             EXIT.
      *
      *    ****    Start processing transaction
      *
       AY60.
            MOVE 1   TO WS-COUNT.
            MOVE 5   TO WS-SHARED.
            PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      *      MOVE SHR-STOCK  TO WS-STOCK.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
            MOVE 4   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-USERS < 24
               MOVE 1            TO WS-SUB
        MOVE ZERO  TO WS-WAIT
               GO TO AY62.
      *
      *    ****   Q   F U L L  -  W A I T   F O R   4 S E C O N D S
      *
            DISPLAY "WAITING" AT 5051
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red HIGHLIGHT.
            COMMIT.
      ACCEPT WS-STIME FROM TIME.
      MOVE 400   TO WS-WAIT.
            PERFORM LOCK-REC-LOOP.
            DISPLAY SPACE AT 5051
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.
             GO TO AY60.

       AY61.
            MOVE "DB"   TO PAR-PROG(WS-USUB).
            MOVE LS-USER  TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
            MOVE 1   TO WS-NETKEY.
            PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             PERFORM AY39 THRU AY59.
            GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N P R O G R E S S
      *
       AY62.
    IF NOT (PAR-PROG(WS-SUB) = SPACES OR
     PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R O R   S T O C K   F I L E S
      *    B E I N G   U P D A T E D
      *
        IF NOT (PAR-PROG(WS-SUB) = SPACES)
           IF PAR-PROG(WS-SUB) = "DB" OR "DS"
      *
      *    ****   Y E S   -   S E T   W A I T P E R I O D
      *
         GO TO AY62-WAIT
     ELSE
         ADD 1  TO WS-SUB
         GO TO AY62
     END-IF
        ELSE
      *
      *    ****   I S T H I S   P R O G R A M   I N T H E  Q
      *
        IF PAR-USR(WS-SUB) = LS-USER
      *
      *    ****   I S I T   N E X T I N   L I N E T O   P R O C E S S
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
      MOVE LS-USER  TO PAR-USR(WS-SUB).
      MOVE WS-SUB  TO PAR-USERS.
      PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
      GO TO AY62-CHECK.
      *
      *    ****   S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
      MOVE 300   TO WS-WAIT.
    IF NOT (PAR-USR(WS-SUB) = LS-USER)
        IF WS-SUB < 24
     ADD 1  TO WS-SUB
     GO TO AY62.

       AY62-CHECK.
    IF WS-WAIT > ZERO
        COMMIT
        DISPLAY "Waiting" AT 5072
   WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT BLINK
        ACCEPT WS-STIME FROM TIME
        PERFORM LOCK-REC-LOOP
        DISPLAY "Waiting" AT 5072
   WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT BLINK
        GO TO AY60.
            DISPLAY SPACE AT 5072
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.

       AY63.
            MOVE WS-SUB  TO WS-USUB.
            GO TO AY61.

       AY70.
            MOVE 4   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *     NETWORK record 2.
      *
            PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
            PERFORM AY49 THRU AY59.
      MOVE 1   TO WS-USUB.

       AY72.
    IF NOT (PAR-USR(WS-USUB) = LS-USER)
        IF WS-USUB < 24
     ADD 1  TO WS-USUB
     GO TO AY72
        ELSE
     GO TO AY85.

       AY75.
            MOVE SPACES  TO PAR-PROG(WS-USUB)
              PAR-USR(WS-USUB).
    IF WS-USUB < 24
        ADD 1 WS-USUB  GIVING WS-SUB
    ELSE
        GO TO AY80.
    IF NOT (PAR-PROG(WS-SUB) = SPACES)
        MOVE PAR-PROG(WS-SUB)
     TO PAR-PROG(WS-USUB)
        MOVE PAR-USR(WS-SUB)
     TO PAR-USR(WS-USUB)
        ADD 1   TO WS-USUB
        GO TO AY75.

       AY80.
            SUBTRACT 1 FROM WS-USUB
     GIVING PAR-USERS.
            PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.

       AY85.
            COMMIT.

       AY999.
             EXIT.
      *
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
               MOVE DEB-TYPE     TO W80-TYPE
      *
      *    ****    Move the Debtor Record to working storage area
      *
               MOVE DEB-RECORD1      TO W40-DEBTREC.

       CA160-EXIT.
             EXIT.

       CA165-NAME        SECTION 2.
       CA165-EXTRACT.
            MOVE ZERO   TO WS-ERROR.
             UNSTRING W60-NAME DELIMITED BY "*" OR "!"
               INTO W40-SURNAME W40-FNAME W40-SNAME
               OVERFLOW
         MOVE 1   TO WS-ERROR.

       CA170-EXIT.
             EXIT.

      *
       BA000        SECTION 5.
       BA00.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
             MOVE "View and allocate Credits Y/N  [Y]" TO WS-ERR-MES.
      MOVE "Y"   TO WS-OPTION.

       BA05.
      PERFORM OPT-MESSAGE.
    IF NOT(WS-OPTION = "Y" OR "N")
        GO TO BA05.
    IF WS-OPTION = "N"
        GO TO BA999.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Start at the beginning of the Debtor account file     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA10.
      MOVE " !"   TO DEB-ACNO.
      PERFORM START-AT-DEBT-AC THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
              GO TO BA999.
      *
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³Account No XXXXXX NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN³°°
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°
      *      °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
      *
             MOVE 19                 TO SHADE-ROW.
             MOVE 12                 TO SHADE-COL.
             MOVE 58                 TO SHADE-WIDTH.
             MOVE 2                  TO SHADE-LINES.
             DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1810 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black
                                                                                "¿"        WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "³Account No"                                                 AT 1910 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black
                                "                                                ³"        WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "À"                                                           AT 2010 WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Black
                      "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"        WITH BACKGROUND-COLOR Grey FOREGROUND-COLOR Grey HIGHLIGHT.
             PERFORM SCREEN-SHADOW.

       BA15.
             DISPLAY "Ac.No.  Name"                                             AT 0402 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ UNALLOCATED CREDITS ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0715 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "   Date     Reference  Code  Description         Value  " AT 0915 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             MOVE 10                 TO CLIN.  
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³          G E T   T H E   N E X T   A C C O U N T          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA20.
            PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                  E N D   O F   F I L E ?                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF WS-F-ERROR = 6
               GO TO BA980.
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Display the account number and name of each account read. ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             DISPLAY DEB-ACNO     AT 1922 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
             DISPLAY W40-DSP-NAME AT 1929 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
           IF NOT OPEN-ITEM
               GO TO BA20.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ No & Name NOT displayed for unallocated credits yet!   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             MOVE SPACE              TO WS-ACT.
             MOVE DEB-ACNO           TO TRA-KEY.

       BA25.
             PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³       No transactions on file?         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF WS-F-ERROR = 5
               GO TO BA20.

       BA30.
             PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³   End of transaction file or account number has changed?  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF (WS-F-ERROR = 5) OR
              (TRA-AC NOT = DEB-ACNO)
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Have credits details been displayed for the previous      ³
      *    ³ account? Allow User to allocate credits before continuing.³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               IF WS-ACT = "Y"
                   PERFORM CL000
                   DISPLAY WS-BLNK78 AT 0502 WITH FOREGROUND-COLOR Cyan BACKGROUND-COLOR Blue
                   MOVE 1001         TO CPOS
                   PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 18
                   IF WS-ERROR = 9
                       GO TO BA999
                   END-IF
               END-IF
               GO TO BA20.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Is this transaction a credit? If not then go and read the ³
      *    ³ next transaction.                                         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF TRA-VALUE NOT < ZERO
               GO TO BA30.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Has the credit value been allocated yet? If yes then go   ³
      *    ³ and read the next transaction.                            ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF TRA-VALUE < ZERO
               IF (TRA-VALUE + TRA-ALLOCATED) = ZERO
                   GO TO BA30.
           IF WS-ACT = SPACE
               MOVE "Y"              TO WS-ACT
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Display the account number and name of the account with   ³
      *    ³ unallocated credit transactions.                          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             DISPLAY DEB-ACNO AT 0503 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
             PERFORM BA35 THRU BA60
             DISPLAY W40-NAME AT 0511 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey
             MOVE 10                 TO CLIN.
             MOVE TRA-CODE           TO W90-CODE.
             PERFORM GET-DEBTOR-TRAN-DESC.
             MOVE TRA-DATE           TO W22-DTE2.
             MOVE W22-CC2            TO W22-CC3.
             MOVE W22-YY2            TO W22-YY3.
             MOVE W22-MM2            TO W22-MM3.
             MOVE W22-DD2            TO W22-DD3.
             COMPUTE W25-VALUE = (TRA-VALUE + TRA-ALLOCATED).
             DISPLAY SD-TRAN AT LINE CLIN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Limit the number of lines used to display unallocated     ³
      *    ³ credit transactions.                                      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF CLIN < 16
               ADD 1                 TO CLIN.
             GO TO BA30.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the account is for an individual EXTRACT the initials  ³
      *    ³ and surname and add the title.          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA35.
          IF DEB-TYPE NOT = 1
              MOVE DEB-NAME  TO W40-NAME
              GO TO BA60.
            MOVE DEB-NAME  TO W60-NAME.
             PERFORM CA165-NAME.
            MOVE W75-ETITLE (DEB-TITLE) TO W60-NAME.
            MOVE 7   TO WS-SUB-6.

       BA40-TITLE.
           IF W60-NCHAR (WS-SUB-6) NOT = SPACE
              GO TO BA45-INIT.
            SUBTRACT 1   FROM WS-SUB-6.
           IF WS-SUB-6 > 0
              GO TO BA40-TITLE.

       BA45-INIT.
            MOVE 1   TO WS-SUB-5.
            ADD 2   TO WS-SUB-6.
             MOVE W40-NCHAR (25) TO W60-NCHAR (WS-SUB-6).
            ADD 1   TO WS-SUB-6.
            MOVE "."   TO W60-NCHAR (WS-SUB-6).
           IF W40-NCHAR (49) = SPACE
              GO TO BA50-SURNAME.
            ADD 1   TO WS-SUB-6.
             MOVE W40-NCHAR (49) TO W60-NCHAR (WS-SUB-6).
            ADD 1   TO WS-SUB-6.
            MOVE "."   TO W60-NCHAR (WS-SUB-6).

       BA50-SURNAME.
            ADD 2   TO WS-SUB-6.

       BA55-LOOP.
            MOVE W40-NCHAR (WS-SUB-5)
     TO W60-NCHAR (WS-SUB-6).
           IF WS-SUB-5 < 24
              ADD 1   TO WS-SUB-5
              ADD 1   TO WS-SUB-6
              GO TO BA55-LOOP.
            MOVE W60-NAME  TO W40-NAME.

       BA60.
      EXIT.

       BA980.
      MOVE "End of List reached"
     TO WS-ERR-MES.
      PERFORM ERROR-MESSAGE.

       BA999.
             EXIT.
      *
      *    ****    A L L O C A T E   C R E D I T S
      *
       CL000        SECTION 51.
       CL00.
      PERFORM SAVE-SCREEN-3.
      MOVE "Y"   TO WS-OPTION.

       CL05.
             MOVE "Allocate credits for this Account Y/N? [Y]" TO WS-OPT-MES.
             PERFORM OPT-MESSAGE.
           IF NOT(WS-OPTION = "Y" OR "N")
               GO TO CL05.
           IF WS-OPTION = "N"
               GO TO CL998.
             PERFORM ERASE-SCREEN.
             DISPLAY "CREDIT TRANSACTION ALLOCATION" AT 0226 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "Account :" AT 0302.
             MOVE DEB-ACNO           TO W80-ACNO.
             PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR = 1
               MOVE WS-ERR2          TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CL998.
           IF WS-ERROR = 2
               MOVE WS-ERR1          TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CL998.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.

      *    *************************************************************
      *    ****         T E S T   F O R   I N D I V I D U A L       ****
      *    *************************************************************
           IF W40-TYPE = 1
               MOVE "D"              TO W40-INSTR
               CALL "DTP\DTPNAME" USING W40-COMPANY LS-USER-ID
           ELSE
               MOVE W40-NAME         TO W40-DSP-NAME.


            DISPLAY W40-DSP-NAME AT 0319 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
            DISPLAY "Date" AT 0505
             "Cde/description" AT 0513
             "Ref. No." AT 0529
             "Value" AT 0543.
             MOVE DEB-ACNO           TO TRA-KEY.

       CL08.
             PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
           IF WS-F-ERROR = 5
               GO TO CL35.

       CL10.
             PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
           IF (WS-F-ERROR = 5) OR
              (TRA-AC NOT = DEB-ACNO)
               GO TO CL35.
           IF TRA-VALUE NOT < ZERO
               GO TO CL10.
           IF TRA-VALUE < ZERO
               IF (TRA-VALUE + TRA-ALLOCATED) = ZERO
                   GO TO CL10.
             MOVE TRA-CODE           TO W90-CODE.
             PERFORM GET-DEBTOR-TRAN-DESC.
             MOVE TRA-DATE           TO W22-DTE2.
             MOVE W22-CC2            TO W22-CC3.
             MOVE W22-YY2            TO W22-YY3.
             MOVE W22-MM2            TO W22-MM3.
             MOVE W22-DD2            TO W22-DD3.
             MOVE W22-DTE3           TO W90-DTE.
             MOVE TRA-REF            TO W90-REF.
             DISPLAY W90-DTE    AT 0702 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY W90-CODE   AT 0713 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY W00-E-DESC AT 0716 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY TRA-REF    AT 0729 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE TRA-VALUE          TO W95-B.
             DISPLAY W95-V2     AT 0738 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             COMPUTE W90-CR = (TRA-VALUE + TRA-ALLOCATED) * -1.
             MOVE TRA-RECORD1        TO W91-TRAN.
             MOVE ZERO               TO W90-ALLOCATED W90-DISCOUNT.
             DISPLAY S08A.
             MOVE 19                 TO CLIN.
             PERFORM ALLOCATE-CREDIT.
           IF (WS-ERROR = 9) OR
              (W75-ALLOCATED = W91-ALLOCATED)
               GO TO CL30.
             PERFORM AY60 THRU AY999.
             MOVE 0                  TO WS-ACTION.
             PERFORM AY02 THRU AY59.
             MOVE ZERO               TO W90-90 W90-120 W90-CUR W90-60 W90-INT W90-30.

       CL15.
           IF W75-KEY < WS-DBTKEY
               GO TO CL25.
             PERFORM READ-DEBTRN THRU READ-DEBTRN-EXIT.
           IF DBT-UPDATE = SPACE
               GO TO CL20.
             ADD DBT-ALLOC           TO DBT-PAID.
           IF DBT-VALUE > DBT-PAID
               MOVE "X"              TO DBT-TYPE
           ELSE
               MOVE "P"              TO DBT-TYPE.
           IF DBT-CODE = 20
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
             MOVE DBT-KEY            TO TRA-KEY.
             PERFORM READ-DBTRAN THRU READ-DBTRAN-EXIT.
             MOVE 0                  TO WS-ACTION.
             PERFORM AY04 THRU AY59.
             MOVE DBT-TRA            TO TRA-RECORD1.
             MOVE W90-REF            TO TRA-REC.
             PERFORM REWRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
       CL20.
             ADD 1                   TO WS-DBTKEY.
             GO TO CL15.
       CL25.
             MOVE W91-TRAN           TO TRA-RECORD1.
             MOVE 0                  TO WS-ACTION.
             PERFORM AY04 THRU AY59.
             ADD W75-ALLOCATED       TO TRA-ALLOCATED.
             PERFORM REWRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
             PERFORM REWRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.
             SUBTRACT W90-120        FROM DEB-FOUR.
             SUBTRACT W90-90         FROM DEB-THREE.
             SUBTRACT W90-60         FROM DEB-TWO.
             SUBTRACT W90-30         FROM DEB-ONE.
             SUBTRACT W90-CUR        FROM DEB-MTD.
             SUBTRACT W90-INT        FROM DEB-INTEREST.
             PERFORM AY70 THRU AY999.
       CL30.
             CLOSE DEBTRN.

       CL32.
             MOVE "'C'ontinue or 'E'xit  [C]" TO WS-OPT-MES.
             MOVE "C"                TO WS-OPTION.
             MOVE "V"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "E"
               COMMIT
               GO TO CL00.
             MOVE W91-KEY            TO TRA-KEY.
             PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
           IF WS-F-ERROR = 5
               GO TO CL35.
             PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
             GO TO CL10.

       CL35.
             COMMIT.
             MOVE "No credits to allocate" TO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.
             PERFORM RESTORE-SCREEN-3.

       CL998.
             MOVE "Continue with Allocate credits Y/N? [Y]" TO WS-OPT-MES.
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "N"
               MOVE 9                TO WS-ERROR.

       CL999.
             EXIT.
      *
       ZA00-INIT              SECTION 90.
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
      *      MOVE 44   TO USER-NUMBER.
      *      MOVE 1   TO USER-SETTING.
      *      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

       ZA02.
            OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.

       ZA25.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-COMPANY  TO W95-COMP.
            MOVE PAR-TELEPHONE  TO W95-TEL.
            MOVE PAR-DMY  TO W12-TODAY.
            MOVE PAR-YMD  TO W12-T-YMD.
            MOVE 1   TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             GO TO ZA999-EXIT.

       ZA200.

       COPY "LOCKED.PRO".

       ZA205.
             EXIT PROGRAM.

       ZA999-EXIT.
             EXIT.
      *
       ZB000                 SECTION 91.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 2          MOVE W02-NETWORK  TO WS-FILE
                               MOVE WS-NETKEY    TO WS-KEY
               WHEN 5          MOVE W02-DBTRAN   TO WS-FILE
                               MOVE DEB-ACNO     TO WS-KEYX
               WHEN 6          MOVE W02-DEBTOR   TO WS-FILE
                               MOVE DEB-ACNO     TO WS-KEYX
               WHEN 15         MOVE WS-PARID     TO WS-FILE
                               MOVE WS-PARKEY    TO WS-KEY
               WHEN 18         MOVE W02-RECOVER  TO WS-FILE
                               MOVE WS-RECKEY    TO WS-KEY
               WHEN 31         MOVE W02-DEBTRN   TO WS-FILE
                               MOVE WS-DBTKEY    TO WS-KEY
               WHEN 37         MOVE W02-SHARED   TO WS-FILE
                               MOVE WS-SHARED    TO WS-KEY
               WHEN OTHER      MOVE "ERROR"      TO WS-FILE
                               MOVE "DTP015"      TO WS-KEYX
             END EVALUATE.

       COPY "DISPERR.PRO".
