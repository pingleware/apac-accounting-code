      $SET LINKCOUNT"512" GNT"CRPTRN.GNT"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******  ********  *******   **    **    *
      *   **    **  **    **  **    **    **     **    **  ***   **    *
      *   **        **   **   **    **    **     **   **   ****  **    *
      *   **        ******    *******     **     ******    ** ** **    *
      *   **        **   **   **          **     **   **   **  ****    *
      *   **    **  **    **  **          **     **    **  **   ***    *
      *    ******   **    **  **          **     **    **  **    **    *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *    C R E D I T O R   D E L E T E   T R A N S A C T I O N S     *
      *                                                                *
      *       Version 9.04.05 - June 2018                              *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     CRPTRN.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   OCTOBER 2006.

                COPYRIGHT NOTICE: COPYRIGHT (C) 2006 - 2017
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

           SELECT CRTOLD  ASSIGN DISK
                          LOCK EXCLUSIVE
                          STATUS WS-STATUS
                          ACCESS SEQUENTIAL
                          ORGANIZATION SEQUENTIAL.

       COPY "CRTRAN.SL".

       COPY "PARAM.SL".

       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "CREDIT.FDE".

       COPY "CRTOLD.FD".

       COPY "CRTRAN.FDE".

       COPY "PARAM.FDE".

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   **
      *         **    *    **   **    **   **   **    **  **
      *         **   ***   **   **    **   ******     *****
      *          ** ** ** **    **    **   **   **    **  **
      *           ***   ***     **    **   **    **   **   **
      *            *     *       ******    **    **   **    **
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
      *       C = Clear comment line (50)
      *       E = Clear lines (any line or lines between 5 and 46)
      *       H = Change heading
      *       S = Clear the screen and display headings
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "CRPTRN".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "ACCOUNTS PAYABLE - DELETE TRANSACTIONS".
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
           03  WS-SYS-ID           PIC  X(03).

       01  W10-REF.
           03  W10-NUM             PIC  9(08).

       01  W10-DATES.
          03  W10-DATE             PIC X(10).
          03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.

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

           03  W15-SETT            PIC  9(06).
           03  W15-SET             REDEFINES W15-SETT.
               05  W15-30          PIC  9(01).
               05  W15-60          PIC  9(01).
               05  W15-90          PIC  9(01).
               05  W15-120         PIC  9(01).
               05  W15-150         PIC  9(01).
               05  W15-180         PIC  9(01).
           03  W15-PAY             REDEFINES W15-SETT.
               05  W15-MONTH       PIC  9(01)     OCCURS 6.

       01  W25-CALCS.
           03  W25-RESULT          PIC  9(05)V99.
           03  W25-RESULT1         REDEFINES W25-RESULT.
               05  W25-WHOLE       PIC  9(05).
               05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2         REDEFINES W25-RESULT.
               05  W25-KEY         PIC  9(04).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).
           03  W25-TOTAL           PIC S9(09)V99 COMP-3.
           03  W25-VALUE           PIC S9(09)V99 COMP-3.

       01  W25-VALUES.
           03  W25-CFLOW           PIC S9(07)V99 COMP-3 OCCURS 6.
           03  W25-V1              PIC S9(07)V99 COMP-3.
           03  W25-V2              PIC S9(07)V99 COMP-3.

       01  W30-AGE-DATES.
           03  W30-120             PIC  9(08) COMP-3.
           03  W30-90              PIC  9(08) COMP-3.
           03  W30-60              PIC  9(08) COMP-3.
           03  W30-30              PIC  9(08) COMP-3.
           03  W30-CUR             PIC  9(08) COMP-3.

       COPY "W40.DBT".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W60-NME-ADD.
           03  W60-NAME.
               05  W60-NCHAR       PIC  X(01) OCCURS 40.
           03  W60-ADDRESS.
               05  W60-ACHAR       PIC  X(01) OCCURS 70.
           03  W60-ADDR REDEFINES W60-ADDRESS.
               05  W60-ADD         PIC  X(60).
               05  FILLER          PIC  X(10).
           03  W60-ADDRESS1.
               05  W60-PAD         PIC  X(60).
               05  FILLER          PIC  X(10).

       01  W75-KEYS.
           03  W75-S               PIC  9(02)    COMP-5.
           03  W75-S1              PIC  9(02)    COMP-5.
           03  W75-KEY             PIC  9(06)    COMP-5.
           03  W75-ALLOCATED       PIC S9(09)V99 COMP-3.
           03  W75-BALANCE         PIC S9(09)V99 COMP-3.
           03  W75-DNO             PIC  X(06)    OCCURS 18.

       01  W80-ACCOUNT.
           03  W80-ACNO            PIC  X(06).
           03  W80-TYPE            PIC  9(01).

       01  W85-PASS.
           03  W85-SUPER           PIC  X(06)    OCCURS 9.
           03  W85-ENTRY           PIC  9(02)    COMP-3.
           03  W85-MARG            PIC S9(02)V99.

       01  W90-TRAN.
           03  W90-120             PIC S9(09)V99 COMP-3.
           03  W90-90              PIC S9(09)V99 COMP-3.
           03  W90-60              PIC S9(09)V99 COMP-3.
           03  W90-30              PIC S9(09)V99 COMP-3.
           03  W90-CUR             PIC S9(09)V99 COMP-3.
           03  W90-BAL             PIC S9(09)V99 COMP-3.
           03  W90-IDTE            PIC  9(08)    COMP-3.
           03  W90-INT             PIC S9(09)V99 COMP-3.

       01  W95-STM.
           03  W95-COMP            PIC  X(40).

       01  W100-EDIT.
           03  W100-LBAL           PIC  Z(06)9.99-.
           03  W100-LARR           PIC  Z(06)9.99.
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

       01  S00.
           03  LINE 18 COLUMN  2 VALUE "Reading :".
           03  LINE 20 COLUMN  2 VALUE "Writing :".

       01  S27.
           03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Red.
               05  LINE  8 COLUMN 12 VALUE "This routine does not include a recovery procedure. Security".
               05  LINE  9 COLUMN 12 VALUE "copies  of the data files should be made as a power  failure".
               05  LINE 10 COLUMN 12 VALUE "or any other problem during this procedure would require the".
               05  LINE 11 COLUMN 12 VALUE "DATA FILES from the previous backup to be restored !!!      ".
               05  LINE 12 COLUMN 12 VALUE "Do you wish to continue (".
               05          COLUMN 37 VALUE "Y"                                                            FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 38 VALUE "/".
               05          COLUMN 39 VALUE "N"                                                            FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 40 VALUE ")?".

       01  S28.
           03  LINE  4 COLUMN 40 PIC Z9/99/9999 USING W90-IDTE FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.

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
             DISPLAY "AA00" AT 2403.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
           IF LS0-CRLEV < 9
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM HA000.
           IF WS-ERROR = 9
               GO TO AA49.
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

      *           T H I S   R O U T I N E   I S   U S E D   T O
      *            D I S P L A Y   E R R O R   M E S S A G E S
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                      ERROR-MESSAGE                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To display the error message higher or lower (default is  ³
      *    ³ line 20) Move the line number which must be used as the   ³
      *    ³ top line to WS-MES-LINE. The message to be displayed must ³
      *    ³ be in WS-ERR-MES. PERFORM ERROR-MESSAGE.                  ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³                  ERROR-MESSAGE (2 LINES)                  ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ See above for details of the first line and the second    ³
      *    ³ line of the message must be in WS-ERR-MES-2.              ³
      *    ³               PERFORM ERROR-MESSAGE-2.                    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "ERROR.SCR".

       COPY "LOCKED.REC".

       COPY "DATE.CHK".

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
      *    ****    READ FILES ROUTINES
      *
       AC000              SECTION.

       COPY "CONTROL.RD".

       COPY "CREDIT.RD".

       COPY "CRTRAN.RD".

       COPY "PARAM.RD".

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
       AD000             SECTION.

       COPY "CONTROL.WR".

       COPY "CREDIT.WR".

       COPY "CRTRAN.WR".

       COPY "PARAM.WR".

      /
      *    THIS ROUTINE WRITES THE DETAILS TO THE NEW FILE.
      *
       AW000           SECTION.
       AW01.
             MOVE CTO-REC            TO CT-REC.
             DISPLAY CT-CRED AT 2012 WITH FOREGROUND-COLOR Brown HIGHLIGHT " "
                     CT-SDTE         WITH FOREGROUND-COLOR Brown HIGHLIGHT " "
                     CT-CODE         WITH FOREGROUND-COLOR Brown HIGHLIGHT " "
                     CT-SINV         WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             WRITE CT-REC.
           IF WS-STAT1 NOT = "0"
               MOVE 6                TO WS-F-ERROR
               PERFORM WRITE-ERROR.

       AW999.
             EXIT.
      /
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
      *        ³ 0 = RECORD CHANGED          ÄÄ     (REWRITE)     ³
      *        ³ 1 = RECORD WAS ADDED        ÄÄ     (DELETE)      ³
      *        ³ 2 = RECORD WAS DELETED      ÄÄ     (WRITE)       ³
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
             DISPLAY SPACE AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
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
      *
      *    ****   A R E   C R E D I T O R   F I L E S   B E I N G   U P D A T E D
      *
       AY62.
           IF PAR-PROG(WS-SUB) = "CR" OR "CS" OR "CG"
      *
      *    ****    Test if this is a program error first
      *
               IF LS-USER = PAR-USR(WS-USUB)
                   SUBTRACT 1        FROM PAR-USERS
                   MOVE SPACES       TO PAR-PROG(WS-USUB) PAR-USR(WS-USUB)
               ELSE
                   UNLOCK PARAM
                   DISPLAY "Waiting" AT 5072 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK
                   ACCEPT WS-STIME   FROM TIME
                   MOVE 200          TO WS-WAIT
                   PERFORM LOCK-REC-LOOP
                   GO TO AY60.
           IF WS-SUB < 24
               ADD 1                 TO WS-SUB
               GO TO AY62.
             DISPLAY SPACE AT 5072 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
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
             DISPLAY "DELETE CREDITOR TRANSACTIONS" AT 0227 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY S27.

       CM-CHK.
             ACCEPT WS-OPTION AT 1243 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT (WS-OPTION = "Y" OR "N")
               GO TO CM-CHK.
           IF WS-OPTION = "N"
               GO TO CM999.
             PERFORM ERASE-SCREEN.
             DISPLAY "DELETE CREDITOR TRANSACTIONS"      AT 0227 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "Keep transactions from (DDMMYYYY)" AT 0404.
             MOVE W12-TODAY          TO W90-IDTE.

       CM10-DATE.
             ACCEPT S28.
             MOVE W90-IDTE           TO W10-EDTE.
             PERFORM CHECK-DATE.
           IF WS-ERROR NOT = ZERO
               GO TO CM10-DATE.

       CM13.
             DISPLAY "Busy clearing BALANCES" AT 0620.
             PERFORM AY60 THRU AY999.
             MOVE ZERO               TO P-DUE-NOW P-DUE-30 P-DUE-150 P-DUE-60 P-DUE-180 P-DUE-90 P-DUE-120 P-OPEN P-CRED P-CLOSE P-DEBT.
             MOVE ZERO               TO W90-CUR   W90-BAL  W90-30    W90-60   W90-120   W90-INT  W90-90.
             DISPLAY "Account :" AT 0812.
             MOVE "     !"           TO P-NUMBER.
             PERFORM START-AT-CRED-AC THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
               GO TO CM18.

       CM15.
             PERFORM READ-CREDIT-NEXT-LOCK THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
               GO TO CM18.
             DISPLAY P-NUMBER AT 0822 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             MOVE ZERO               TO P-BAL P-OUT P-30D P-120D P-60D P-150D P-90D P-180D P-DT  P-CR P-VAT P-PBAL.
             PERFORM REWRITE-CREDIT-UNLOCK THRU WRITE-CREDIT-EXIT.
             GO TO CM15.

       CM18.
             MOVE 1201           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
             DISPLAY "Busy clearing transactions" AT 1220.
             DISPLAY S00.

       CM19.
             READ CRTOLD AT END GO TO CM20.
           IF CTO-SDTE < W22-DTE1
               GO TO CM19.
             DISPLAY CTO-CRED AT 1812 WITH FOREGROUND-COLOR Cyan HIGHLIGHT " "
                     CTO-SDTE         WITH FOREGROUND-COLOR Cyan HIGHLIGHT " "
                     CTO-CODE         WITH FOREGROUND-COLOR Cyan HIGHLIGHT " "
                     CTO-SINV         WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             PERFORM AW000.
             GO TO CM19.

       CM20.
             CLOSE CRTOLD CTRANS.
             CALL "CBL_DELETE_FILE" USING W02-CRTOLD RETURNING WS-STATUS.
             OPEN I-O CTRANS.
             PERFORM ERASE-SCREEN.
             DISPLAY "DELETE CREDITOR TRANSACTIONS" AT 0227 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY "Recalculate balances" AT 1012.
             MOVE "    !"            TO P-NAME P-NUMBER.
             PERFORM START-AT-CRED-AC THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR NOT = ZERO
               GO TO CM999.

       CM25.
             PERFORM READ-CREDIT-NEXT THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR NOT = ZERO
               GO TO CM90.
             MOVE P-NUMBER           TO CT-CRED.
             MOVE ZERO               TO CT-SDTE CT-CODE CT-SEQ.
             MOVE SPACES             TO CT-SINV.
             PERFORM START-AT-CTRN-KEY THRU READ-CRTRAN-EXIT.
           IF WS-F-ERROR = 4
               GO TO CM25.

       CM30.
             PERFORM READ-CRTRAN-NEXT THRU READ-CRTRAN-EXIT.
           IF (WS-F-ERROR = 4) OR (CT-CRED NOT = P-NUMBER)
               GO TO CM25.
             ADD CT-COST             TO P-BAL P-OUT.
             PERFORM REWRITE-CREDIT THRU WRITE-CREDIT-EXIT.
             GO TO CM30.

       CM90.
             PERFORM AY70 THRU AY999.

       CM999.
             EXIT.
      /
       HA000      SECTION 5.
       HA00.
             PERFORM ERASE-SCREEN.
             MOVE 5                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-PW             TO W85-PASS.
             MOVE 12                 TO SHADE-ROW.
             MOVE 29                 TO SHADE-COL.
             MOVE 23                 TO SHADE-WIDTH.
             MOVE 2                  TO SHADE-LINES.
             DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1127 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey
                     "¿"                                WITH FOREGROUND-COLOR Black          BACKGROUND-COLOR Grey.
             DISPLAY "³ "                       AT 1227 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey
                     "Enter Password        "           WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey
                     "³"                                WITH FOREGROUND-COLOR Black          BACKGROUND-COLOR Grey.
             DISPLAY "À"                        AT 1327 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey
                     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"         WITH FOREGROUND-COLOR Black          BACKGROUND-COLOR Grey.
             PERFORM SCREEN-SHADOW.
             ACCEPT W40-CSHPASS                 AT 1244 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta AUTO NO-ECHO.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W40-CSHPASS BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
           IF NOT(W40-CSHPASS = W85-SUPER(1) OR W85-SUPER(3) OR "ZEROCR")
               MOVE 9                TO WS-ERROR
           ELSE
               MOVE 0                TO WS-ERROR.

       HA999.
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
             MOVE LS-COMPANY         TO CRT-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.PRO".

             MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY    TO W95-COMP.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
      CLOSE CTRANS.
             CALL "CBL_RENAME_FILE" USING W02-CRTRAN W02-CRTOLD RETURNING WS-STATUS.
    IF NOT (WS-STATUS = X"0000")
        MOVE "Unable to Rebuild TRANSACTION file"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        MOVE 9   TO WS-ERROR
        GO TO ZA999-EXIT.
            OPEN INPUT CRTOLD.
           IF WS-STAT1 = "9"
               IF FLE-LOCKED
                   GO TO ZA200.
           IF WS-STATUS NOT = "00"
              MOVE 4   TO WS-F-ERROR
               PERFORM OPEN-ERROR.
            OPEN OUTPUT CTRANS.
            GO TO ZA999-EXIT.
       ZA55.
             MOVE 1                  TO WS-S1.
             MOVE SPACES             TO WS-MID-LNE.
       ZA60.
             MOVE WS-G1              TO WS-TCHR(WS-S1) WS-BCHR(WS-S1).
             MOVE WS-G8              TO WS-TCH(WS-S1)  WS-BCH(WS-S1).
           IF WS-S1 < 80
               ADD 1                 TO WS-S1
               GO TO ZA60.
             MOVE WS-G9              TO WS-TCH(1).
             MOVE WS-G10             TO WS-TCH(80).
             MOVE WS-G11             TO WS-BCH(1).
             MOVE WS-G12             TO WS-BCH(80).
             MOVE WS-G14             TO WS-TCHR(1)  WS-BCHR(1).
             MOVE WS-G13             TO WS-TCHR(80) WS-BCHR(80).
             MOVE WS-G2              TO WS-TCHR(8)  WS-TCHR(49) WS-TCHR(61) WS-TCHR(72).
             MOVE WS-G3              TO WS-MCHR(8)  WS-MCHR(49) WS-MCHR(61) WS-MCHR(72) WS-MCHR(1) WS-MCHR(80).
             MOVE WS-G4              TO WS-BCHR(8)  WS-BCHR(49) WS-BCHR(61) WS-BCHR(72).
             MOVE WS-G13             TO WS-TCHR(80) WS-BCHR(80).
             MOVE WS-G14             TO WS-TCHR(1)  WS-BCHR(1).
             MOVE LS-COMPANY         TO WS-TOP-COMP.
           IF LS-USER = LS-SYS-ID
               MOVE "SupervisorÄ"    TO WS-WRKHD
           ELSE
               MOVE "Workstation"    TO WS-WRKHD
             MOVE LS-USER            TO WS-WRKST.

       ZA200.
       
       COPY "LOCKED.PRO".

       ZA205.
             EXIT PROGRAM.

       ZA999-EXIT.
             EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *
       ZB000-ERROR      SECTION 9.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 2          MOVE W02-NETWORK  TO WS-FILE
                               MOVE WS-NETKEY    TO WS-KEY
               WHEN 3          MOVE W02-CREDIT   TO WS-FILE
                               MOVE P-NUMBER     TO WS-KEYX
               WHEN 4          MOVE W02-CRTRAN   TO WS-FILE
                               MOVE CT-KEY       TO WS-KEYX
               WHEN 15         MOVE WS-PARID     TO WS-FILE
                               MOVE WS-PARKEY    TO WS-KEY
             END-EVALUATE.
              
       COPY "DISPERR.PRO".
