      $SET LINKCOUNT"458" GNT"CRP094.GNT"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******     ****     ******       **     *
      *   **    **  **    **  **    **   **  **   **    **     ***     *
      *   **        **    **  **    **  **    **  **    **    * **     *
      *   **        ******    *******   **    **   *******   *  **     *
      *   **        **   **   **        **    **        **  *******    *
      *   **    **  **    **  **         **  **   **    **      **     *
      *    ******   **    **  **          ****     ******      ****    *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *       C R E D I T O R   D E L E T E   O R D E R S              *
      *                                                                *
      *       Version 9.04.05 - June 2018                              *
      *                                                                *
      ******************************************************************
      *                                                                *
      * June 2018 - Added program CRPRDWR to handle IO for Creditor    *
      *             file and COPY file CA155.CRP to call and handlw IO *
      *             for the CREDITOR file. SEE CA155.CRP and CRPRDWR   *
      *             for details.                                       *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     CRP094.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   OCTOBER 1985.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1985 - 2017
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

       COPY "PARAM.SL".

       COPY "PORDER.SL".

       COPY "STOCK.SL".

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

       COPY "PARAM.FDE".

       COPY "PORDER.FDE".

       COPY "STOCK.FDE".

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
       77  WS-COPYRIGHT            PIC  X(18)    VALUE "aeWlimemnomLalismJ".
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
       77  WS-RECOVER              PIC  9(06)    COMP-5.
       77  WS-TRANS                PIC  9(04)    COMP-5 VALUE 1.
       77  WS-PERIOD               PIC  9(02).
       77  WS-NTP                  PIC  X(01).
       77  WS-KEYSTR               PIC  9(06)    COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-SKIP                 PIC  X(01)    VALUE "N".
       77  WS-TAX                  PIC  X(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-ERROR                PIC  9(01)    VALUE ZERO.
       77  WS-EKEY                 PIC  9(06)    COMP-5.
       77  WS-COMP                 PIC  9(01).
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.
      /
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
           03  CRT-PROGRAM         PIC  X(15)           VALUE "CRP032".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40)           VALUE "CREDITOR - DELETE ORDERS".
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

       01  W10-REF.
           03  W10-NUM         PIC  9(08).

      *
      *    Date routines for accepting checking and formatting have
      *    been removed from each program and the DateCheck program
      *    will be called to handle these routines.
      *
      *    The following copy 'DateVariables' now includes the 'W12.WS'
      *    and 'W20.WS' variables and is passed to DateCheck in the CALL
      *    statement.
      *
       COPY "DateVariables".

          03  W15-SETT      PIC  9(06).
           03  W15-SET       REDEFINES W15-SETT.
               05  W15-30    PIC  9(01).
               05  W15-60    PIC  9(01).
               05  W15-90    PIC  9(01).
               05  W15-120   PIC  9(01).
               05  W15-150   PIC  9(01).
               05  W15-180   PIC  9(01).
           03  W15-PAY       REDEFINES W15-SETT.
               05  W15-MONTH PIC  9(01)     OCCURS 6.

       01  W25-CALCS.
           03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
               05  W25-WHOLE   PIC 9(05).
               05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
        05  W25-KEY     PIC 9(04).
        05  W25-SUB     PIC 9(01).
        05  FILLER      PIC 9(02).
           03  W25-TOTAL       PIC S9(09)V99 COMP-3.
           03  W25-VALUE       PIC S9(09)V99 COMP-3.

       01  W25-VALUES.
           03  W25-CFLOW OCCURS 6
                             PIC S9(07)V99 COMP-3.
           03  W25-V1        PIC S9(07)V99 COMP-3.
           03  W25-V2        PIC S9(07)V99 COMP-3.

       01  W30-AGE-DATES.
    03  W30-120        PIC  9(08) COMP-3.
    03  W30-90        PIC  9(08) COMP-3.
    03  W30-60        PIC  9(08) COMP-3.
    03  W30-30        PIC  9(08) COMP-3.
    03  W30-CUR        PIC  9(08) COMP-3.

       COPY "W40.DBT".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W60-NME-ADD.
           03  W60-NAME.
               05  W60-NCHAR OCCURS 40 PIC X(01).
           03  W60-ADDRESS.
               05  W60-ACHAR OCCURS 70 PIC X(01).
           03  W60-ADDR REDEFINES W60-ADDRESS.
               05  W60-ADD     PIC X(60).
               05  FILLER      PIC X(10).
           03  W60-ADDRESS1.
               05  W60-PAD     PIC X(60).
               05  FILLER      PIC X(10).

       01  W75-KEYS.
    03  W75-S        PIC  9(02) COMP-5.
    03  W75-S1        PIC  9(02) COMP-5.
    03  W75-KEY        PIC  9(06)    COMP-5.
    03  W75-ALLOCATED   PIC S9(09)V99 COMP-3.
    03  W75-BALANCE     PIC S9(09)V99 COMP-3.
    03  W75-DNO        PIC  X(06) OCCURS 18.

       01  W80-ACCOUNT.
          03  W80-ACNO        PIC  X(06).
          03  W80-TYPE        PIC  9(01).

       01  W85-PASS.
           03  W85-SUPER       PIC  X(06)    OCCURS 9.
           03  W85-ENTRY       PIC  9(02)    COMP-3.
           03  W85-MARG        PIC S9(02)V99.

       01  W90-TRAN.
           03  W90-120         PIC S9(09)V99 COMP-3.
           03  W90-90          PIC S9(09)V99 COMP-3.
           03  W90-60          PIC S9(09)V99 COMP-3.
           03  W90-30          PIC S9(09)V99 COMP-3.
           03  W90-CUR         PIC S9(09)V99 COMP-3.
           03  W90-BAL         PIC S9(09)V99 COMP-3.
           03  W90-IDTE        PIC 9(06)     COMP-3.
           03  W90-INT         PIC S9(09)V99 COMP-3.

       01  W95-STM.
           03  W95-COMP        PIC X(40).

       01  W100-EDIT.
           03  W100-LBAL       PIC Z(06)9.99-.
           03  W100-LARR       PIC Z(06)9.99.
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

       01  S27.
           03  BACKGROUND-COLOR Red  FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE  6 COLUMN 12 VALUE "This routine does not include a recovery procedure. Security".
               05  LINE  7 COLUMN 12 VALUE "copies  of the data files should be made as a power  failure".
               05  LINE  8 COLUMN 12 VALUE "or any other problem during this procedure would require the".
               05  LINE  9 COLUMN 12 VALUE "DATA FILES from the previous backup to be restored !!!      ".
               05  LINE 11 COLUMN 12 VALUE "Do you wish to continue (Y/N)?".

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
           IF LS0-CRLEV < 9
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM CM000.

       AA49.
             EXIT PROGRAM.

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
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.SCR".

      *    *************************************************************
      *    ****   T H I S   R O U T I N E   I S   U S E D   T O
      *    D I S P L A Y   E R R O R   M E S S A G E S
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                      ERROR-MESSAGE                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To display the error message higher or lower (default is  ³
      *    ³ line 20) Move the line number which must be used as the   ³
      *    ³ top line to WS-MES-LINE. The message to be displayed must ³
      *    ³ be in WS-ERR-MES. PERFORM ERROR-MESSAGE.                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

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

      /
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000              SECTION.

       COPY "CONTROL.RD".

       COPY "CREDIT.RD".

       COPY "PARAM.RD".

       COPY "PORDER.RD".

       COPY "STKMEM.RD".

       COPY "STOCK.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AD000             SECTION.

       COPY "CONTROL.WR".

       COPY "CREDIT.WR".

       COPY "PARAM.WR".

       COPY "PORDER.WR".

       COPY "STOCK.WR".

      *
      *       ****   *****  ***    ***   *   *  *****  ****   *   *
      *       *   *  *     *   *  *   *  *   *  *      *   *  *   *
      *       ****   ***   *      *   *  *   *  ***    ****    * *
      *       *   *  *     *   *  *   *   * *   *      *   *    *
      *       *   *  *****  ***    ***     *    *****  *   *    *
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *          THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *        ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *        ³ ORIGINAL ACTION (REC-TYPE)  ÄÄ  RECOVERY PROCESS ³
      *        ³ 0 = RECORD CHANGED          ÄÄ      (REWRITE)    ³
      *        ³ 1 = RECORD WAS ADDED        ÄÄ      (DELETE)     ³
      *        ³ 2 = RECORD WAS DELETED      ÄÄ      (WRITE)      ³
      *        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY000           SECTION.
       AY60.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
             MOVE 4                  TO WS-PARKEY.
             MOVE 3                  TO WS-COUNT.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-USERS < 24
               MOVE 1                TO WS-SUB
               GO TO AY62.
      *
      *    ****   Q   F U L L  -  W A I T   F O R   2   S E C O N D S
      *
             DISPLAY "WAITING" AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red HIGHLIGHT.
             UNLOCK PARAM.
             ACCEPT WS-STIME FROM TIME.
             MOVE 200                TO WS-WAIT.
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
      *            and lock it.
      *
             MOVE 2                  TO WS-NETKEY.
             PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             GO TO AY999.
       AY62.
           IF PAR-PROG(WS-SUB) = "CR" OR "CS" OR "CG"
      *
      *    ****    Test if this is a program error first
      *
               IF LS-USER = PAR-USR(WS-USUB)
                   SUBTRACT 1        FROM PAR-USERS
                   MOVE SPACES       TO PAR-PROG(WS-USUB) PAR-USR (WS-USUB)
               ELSE
                   UNLOCK PARAM
                   DISPLAY "Waiting" AT 5072 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK
                   ACCEPT WS-STIME FROM TIME
                   MOVE 200          TO WS-WAIT
                   PERFORM LOCK-REC-LOOP
                   DISPLAY "Waiting" AT 5072 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK
                   GO TO AY60.
           IF WS-SUB < 24
               ADD 1                 TO WS-SUB
               GO TO AY62.
             DISPLAY SPACE AT 5072 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT.
             MOVE 1                  TO WS-SUB.

       AY63.
           IF PAR-USR(WS-SUB) = SPACES
               MOVE WS-SUB           TO WS-USUB
               ADD 1                 TO PAR-USERS
               GO TO AY61.
             ADD 1                   TO WS-SUB.
             GO TO AY63.

       AY70.
             MOVE 4                  TO WS-PARKEY.
             MOVE 1                  TO WS-COUNT.
             PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 2.
      *
             PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
             MOVE SPACES             TO PAR-PROG(WS-USUB) PAR-USR(WS-USUB).
             SUBTRACT 1              FROM PAR-USERS.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             COMMIT.

       AY999.
             EXIT.
      /
      *    ****    R E - C A L C U L A T E   B A L A N C E S
      *
       CM000         SECTION 5.
       CM00.
             PERFORM ERASE-SCREEN.
            DISPLAY "DELETE PURCHASE ORDERS" AT 0229
                        WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            DISPLAY "This routine will delete orders prior to the date keyed in" AT 0408.
             DISPLAY "Are you sure Y/N?" AT 0608
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.

       CM05.
            ACCEPT WS-OPTION AT 0626
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT(WS-OPTION = "Y" OR "N")
        GO TO CM05.
           IF WS-OPTION = "N"
        GO TO CM999.


            DISPLAY S27.

       CM-CHK.
            ACCEPT WS-OPTION AT 1043
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT(WS-OPTION = "Y" OR "N")
               GO TO CM-CHK.
           IF WS-OPTION = "N"
               GO TO CM999.
             MOVE 1              TO WS-S3.
             DISPLAY "Account :" AT 1812.
             MOVE "N"            TO WS-SKIP.
             PERFORM AY60 THRU AY999.
      MOVE ZERO   TO W90-CUR W90-BAL W90-30 W90-60
                                    W90-120 W90-INT W90-90.
      MOVE "     !"  TO P-NUMBER.
      PERFORM START-AT-CRED-AC THRU READ-CREDIT-EXIT.
    IF WS-F-ERROR = 3
               GO TO CM85.
       CM20.
            PERFORM READ-CREDIT-NEXT-LOCK THRU READ-CREDIT-EXIT.
    IF WS-F-ERROR = 3
               GO TO CM85.
            DISPLAY DEB-ACNO AT 1822
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
           IF OPEN-ITEM
              MOVE ZERO  TO DEB-BAL DEB-CRED DEB-DEBT
           ELSE
               MOVE DEB-STM      TO DEB-BAL.
             MOVE DEB-ACNO       TO TRA-AC.
             MOVE ZERO           TO TRA-DATE TRA-CODE.
             MOVE SPACES         TO TRA-REF.
      PERFORM START-AT-DTRN-KEY THRU READ-CRTRAN-EXIT.
    IF WS-F-ERROR = 4
               GO TO CM80.
       CM70.
      PERFORM READ-CRTRAN-NEXT THRU READ-CRTRAN-EXIT.
    IF (WS-F-ERROR = 4) OR
              (DEB-ACNO NOT = TRA-AC)
               GO TO CM80.
             ADD TRA-VALUE       TO DEB-BAL.
           IF NOT OPEN-ITEM
               GO TO CM70.
           IF TRA-VALUE > ZERO
               MULTIPLY -1       BY TRA-ALLOCATED.
             ADD TRA-ALLOCATED   TO DEB-BAL.
             GO TO CM70.
       CM80.
           IF OPEN-ITEM
        PERFORM CI000
               COMPUTE DEB-STM = DEB-BAL - DEB-DEBT + DEB-CRED.
      ADD DEB-BAL  TO W90-BAL.
             ADD DEB-CUR         TO W90-CUR.
             ADD DEB-30          TO W90-30.
             ADD DEB-60          TO W90-60.
             ADD DEB-90          TO W90-90.
             ADD DEB-120         TO W90-120.
             ADD DEB-INT         TO W90-INT.
      PERFORM REWRITE-CREDIT-UNLOCK THRU WRITE-CREDIT-EXIT.
             GO TO CM20.
       CM85.
      MOVE W90-BAL  TO DEB-OUT.
             MOVE W90-CUR        TO DEB-MTD.
             MOVE W90-30         TO DEB-ONE.
             MOVE W90-60         TO DEB-TWO.
             MOVE W90-90         TO DEB-THREE.
             MOVE W90-120        TO DEB-FOUR.
             MOVE W90-INT        TO DEB-INTEREST.
      PERFORM AY70 THRU AY999.
       CM999.
             EXIT.
      *    *************************************************************
      *                 I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT    SECTION 8.
       ZA000-START.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W12-TODAY.
             MOVE LS-TODAY-YYMMDD    TO W12-T-YMD.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY W95-COMP.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.PRO".

      *
      *    ****    S E T   U P   A G E   D A T E S
      *
             MOVE W12-T-YMD      TO W22-DTE2.
      *    IF W22-DD2 > 27
      *        ADD 1   TO W22-MM2.
       ZA47.
           IF W22-MM2 > 01
               SUBTRACT 1        FROM W22-MM2
           ELSE
               MOVE 12           TO W22-MM2
        SUBTRACT 1  FROM W22-CY2.
    IF W22-MM2 = 02
               MOVE 28           TO W22-DD2
           ELSE
           IF W22-MM2 = 04 OR 06 OR 09 OR 11
               MOVE 30           TO W22-DD2
           ELSE
               MOVE 31           TO W22-DD2.
       ZA48.
           IF W12-MM = W22-MM2
               IF W12-DD < W22-DD2
                   MOVE W12-DD   TO W22-DD2.
       MOVE W22-DTE2   TO W30-CUR.
      PERFORM ZA47.
             MOVE W22-DTE2       TO W30-30.
      PERFORM ZA47.
             MOVE W22-DTE2       TO W30-60.
      PERFORM ZA47.
             MOVE W22-DTE2       TO W30-90.
      PERFORM ZA47.
             MOVE W22-DTE2       TO W30-120.

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
               WHEN 4          MOVE W02-CRTRAN  TO WS-FILE
                               MOVE CT-KEY      TO WS-KEYX
               WHEN 15         MOVE WS-PARID    TO WS-FILE
                               MOVE WS-PARKEY   TO WS-KEY
               WHEN OTHER      MOVE "ERROR"     TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
