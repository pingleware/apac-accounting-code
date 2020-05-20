      $set LINKCOUNT"512" GNT"CRP091.GNT"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******     ****     ******      **      *
      *   **    **  **    **  **    **   **  **   **    **    ***      *
      *   **        **    **  **    **  **    **  **    **     **      *
      *   **        ******    *******   **    **   *******     **      *
      *   **        **   **   **        **    **        **     **      *
      *   **    **  **    **  **         **  **   **    **     **      *
      *    ******   **    **  **          ****     ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R   C L E A R   B A L A N C E S              *
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
       PROGRAM-ID.      CRP091.
       AUTHOR.          J.W.LEMMON (CSS).
       DATE-WRITTEN.    JUNE, 1983.

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

       COPY "CRTRAN.SL".

       COPY "PARAM.SL".

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

       COPY "CRTRAN.FDE".

       COPY "PARAM.FDE".

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
       77  WS-CREKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-CTRKEY               PIC  9(06)    COMP-5 VALUE 1.
       77  WS-NETKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-ERROR                PIC  9(01)           VALUE 0.
       77  WS-FC                   PIC  9(01)           VALUE 0.
       77  WS-SUB                  PIC  9(04)    COMP-5.
       77  WS-S                    PIC  9(04)    COMP-5.
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S5                   PIC  9(04)    COMP-5.
       77  WS-S6                   PIC  9(04)    COMP-5.
       77  WS-S7                   PIC  9(04)    COMP-5.
       77  WS-IXS1                 PIC  9(04)    COMP-5.
       77  WS-IXS2                 PIC  9(04)    COMP-5.
       77  WS-IXS3                 PIC  9(04)    COMP-5.
       77  WS-IXS4                 PIC  9(04)    COMP-5.
       77  WS-ENT                  PIC  X(08).
       77  WS-ENT1                 PIC  X(08).
       77  WS-KEYSTR               PIC  9(04)    COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-NUM                  PIC  Z9.
       77  WS-KEY1                 PIC  9(04)    COMP-5.
       77  WS-CONREC               PIC  9(01)           VALUE ZERO.
       77  WS-INDS                 PIC  9(01)           VALUE 0.
       77  WS-INDC                 PIC  9(01)           VALUE 0.
       77  WS-INDEX-STOR           PIC  X(08).
       77  WS-INDEX-NAME           PIC  X(08).
       77  WS-SKIP                 PIC  X(01)           VALUE " ".
       77  WS-CHAR1                PIC  X(01).
       77  WS-LIN                  PIC  9(02)           VALUE ZERO.
       77  WS-LINES                PIC  9(04)    COMP-5 VALUE 66.
       77  WS-PAGE                 PIC  9(04)    COMP-5.
       77  WS-SEASON               PIC  X(01).
       77  WS-ER8                  PIC  X(09)           VALUE "No record".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.
      /
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *       C = Clear comment line (50)
      *       E = Clear lines (any line or lines between 5 and 46)
      *       H = Change heading
      *       S = Clear the screen and display headings
      *
           03  CRT-TYPE              PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START             PIC  9(02).
           03  CRT-END               PIC  9(02).
      *
      *    Calling Program
      *
           03  CRT-PROGRAM           PIC  X(15)         VALUE "CRP091".
      *
      *    Screen Heading
      *
           03  CRT-HEADING           PIC  X(40)         VALUE "CREDITOR - CLEAR BALANCES".
      *
      *    Company Name
      *
           03  CRT-COMPANY           PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-TERMINAL.
               05  CRT-WRKHD       PIC  X(11).
               05  CRT-WRKST       PIC  X(03).

       COPY "WS.WS".

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

       COPY "W05.WS".

       01  W15-ACCOUNT-VALUES.
           03  W15-BAL           PIC S9(09)V99  COMP-3 VALUE ZERO.
           03  W15-DT            PIC S9(09)V99  COMP-3 VALUE ZERO.
           03  W15-CR            PIC S9(09)V99  COMP-3 VALUE ZERO.
           03  W15-PBAL          PIC S9(09)V99  COMP-3 VALUE ZERO.
           03  W15-30D           PIC S9(09)V99  COMP-3 VALUE ZERO.
           03  W15-60D           PIC S9(09)V99  COMP-3 VALUE ZERO.
           03  W15-90D           PIC S9(09)V99  COMP-3 VALUE ZERO.
           03  W15-120D          PIC S9(09)V99  COMP-3 VALUE ZERO.
           03  W15-150D          PIC S9(09)V99  COMP-3 VALUE ZERO.
           03  W15-180D          PIC S9(09)V99  COMP-3 VALUE ZERO.
           03  W15-OUT           PIC S9(09)V99  COMP-3 VALUE ZERO.
           03  W15-PVAT          PIC S9(09)V99  COMP-3 VALUE ZERO.

       01  W20-CALCS.
           03  W20-RESULT          PIC  9(06)V99.
           03  W20-RESULT-1                      REDEFINES W20-RESULT.
               05  W20-WHOLE       PIC  9(06).
               05  W20-DECIMAL     PIC  9(02).
           03  W20-ORD             PIC  9(07)    COMP-3.
           03  W21-DATE.
               05  W21-YY          PIC  99.
               05  W21-MM          PIC  99.
               05  W21-DD          PIC  99.
           03  W21-DTE             PIC  9(06)    REDEFINES W21-DATE.
       01  W25-VALUES.
           03  W25-OUT             PIC S9(09)V99 COMP-3.
           03  W25-30D             PIC S9(09)V99 COMP-3.
           03  W25-60D             PIC S9(09)V99 COMP-3.
           03  W25-90D             PIC S9(09)V99 COMP-3.
           03  W25-120D            PIC S9(09)V99 COMP-3.
           03  W25-150D            PIC S9(09)V99 COMP-3.
           03  W25-180D            PIC S9(09)V99 COMP-3.
           03  W25-BAL             PIC S9(09)V99 COMP-3.
           03  W25-V1              PIC S9(09)V99 COMP-3.
           03  W25-V2              PIC S9(09)V99 COMP-3.
       01  W25-EDIT.
           03  W25-5.
               05  W25-1           PIC  ZZZZ9.
           03  W25-3V2.
               05  W25-2           PIC  ZZ9.99.
           03  W25-7V2.
               05  W25-3           PIC  Z(06)9.99.
           03  W25-S7V2.
               05  W25-VAL         PIC  Z(07)9.99-.
           03  W25-3N.
               05  W25-4           PIC  ZZ9.
           03  W25-S5V2.
               05  W25-CVAL        PIC  Z(06)9.99-.
           03  W25-DTE.
               05  W25-DATE        PIC  Z9/99/99.
           03  W25-DEL.
               05  W25-2N          PIC  9(02).
           03  W25-6.
               05  W25-NO          PIC  Z(07)9.
       01  W30-DATE.
           03  W30-MM              PIC  X(03).
           03  FILLER              PIC  X(01)    VALUE ",".
           03  W30-YY              PIC  9(02).
       01  W40-NADD.
           03  W40-ADD.
               05  W40-ACHAR       PIC  X(01)    OCCURS 70.
           03  W40-ADDR                          REDEFINES W40-ADD.
               05  W40-ADDRS       PIC  X(60).
               05  FILLER          PIC  X(10).
           03  W40-NAME.
               05  W40-NCHAR       PIC  X(01)    OCCURS 40.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       COPY "W70CRED.WS".

       01  W75-KEYS.
           03  W75-S               PIC  9(02)    COMP-5.
           03  W75-S1              PIC  9(02)    COMP-5.
           03  W75-CNO             PIC  X(05)    OCCURS 18.

       01  W85-PASS.
           03  W85-SUPER           PIC  X(06)    OCCURS 9.
           03  W85-ENTRY           PIC  9(02)    COMP-3.
           03  W85-MARG            PIC S9(02)V99.

       01  W95-COMPANY.
           03  W95-COMP            PIC  X(40).

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

       01  S12.
          03  LINE  2 COLUMN 29 FOREGROUND-COLOR Grey HIGHLIGHT
           VALUE "CLEARING ACCOUNT BALANCES".

       01  S39.
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
       AA000-START-UP  SECTION.
       AA000.
           IF LS0-CRLEV < 9
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM HA000.
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

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AB00-READ      SECTION.

       COPY "CONTROL.RD".

       COPY "CREDIT.RD".

       COPY "PARAM.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AC00-WRITE    SECTION.

       COPY "CONTROL.WR".

       COPY "CREDIT.WR".

       COPY "PARAM.WR".

      *
      *    ****    Read Creditor record - using account number.
      *
      *CA155-GET-CREDITOR      SECTION.
      *CA155-ACCOUNT.
      *      MOVE W15-ACNO           TO P-NUMBER.
      *      PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
      *    IF WS-F-ERROR = 3
      *        MOVE 1                TO WS-ERROR
      *    ELSE
      *    IF P-NUMBER NOT = W15-ACNO
      *        MOVE 2                TO WS-ERROR
      *    ELSE
      *        MOVE 0                TO WS-ERROR.
	  *
      *CA160-EXIT.
      *       EXIT.

      /
       HA000      SECTION 5.
       HA00.
             PERFORM ERASE-SCREEN.
           IF LS-DEM = 6
               MOVE 9                TO LS-DEM
               GO TO HA03.
             MOVE 5                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-PW             TO W85-PASS.
             MOVE 12                 TO SHADE-ROW.
             MOVE 29                 TO SHADE-COL.
             MOVE 23                 TO SHADE-WIDTH.
             MOVE 2                  TO SHADE-LINES.
             DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1127 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey
                                             "¿"        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
             DISPLAY "³ "                       AT 1227 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey
                       "Enter Password        "         WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey
                                             "³"        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
             DISPLAY "À"                        AT 1327 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Grey
                      "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
             PERFORM SCREEN-SHADOW.
             ACCEPT W40-CSHPASS AT 1244 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT AUTO NO-ECHO.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W40-CSHPASS BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
           IF NOT(W40-CSHPASS = W85-SUPER (1) OR W85-SUPER (3) OR "ZEROCR")
               GO TO HA999.
             DISPLAY "                       " AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT.
             DISPLAY S12.
             DISPLAY S39.
       HA02.
             ACCEPT WS-OPTION AT 1043 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-OPTION = "N" OR "Y")
               GO TO HA02.
           IF WS-OPTION = "N"
               GO TO HA999.

       HA03.
             MOVE 2                  TO WS-NETKEY.
             PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             MOVE ZERO               TO P-DUE-NOW P-DUE-30 P-DUE-150 P-DUE-60 P-DUE-180 P-DUE-90 P-DUE-120 P-OPEN P-CRED P-CLOSE P-DEBT.
             MOVE "  !"              TO P-NUMBER.
             DISPLAY "Account :" AT 1620.
             PERFORM START-AT-CRED-AC THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
               GO TO HA10.

       HA05.
             PERFORM READ-CREDIT-NEXT-LOCK THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
               GO TO HA10.
             DISPLAY P-NUMBER AT 1630 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             MOVE W15-ACCOUNT-VALUES TO P-ACCOUNT-VALUES.
             PERFORM REWRITE-CREDIT-UNLOCK THRU WRITE-CREDIT-EXIT.
             GO TO HA05.

       HA10.
             MOVE 1201               TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
             DISPLAY "Busy clearing transactions" AT 1220.
             MOVE 1                  TO P-CRTRAN.
             CLOSE CTRANS.
             OPEN OUTPUT CTRANS.
             CLOSE CTRANS.
             OPEN I-O CTRANS.
             PERFORM REWRITE-CONTROL-UNLOCK THRU WRITE-CONTROL-EXIT.

       HA999.
             EXIT.
      *    *************************************************************
      *                 I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT    SECTION 8.
       ZA000-START.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W05-DATE.
             MOVE LS-TODAY-YYMMDD    TO W05-DATE-YMD.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY W95-COMP.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

             MOVE 2                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             GO TO ZA999-EXIT.

      *COPY "ZA49.PRO".
      *
      *ZA200.
      *
      *COPY "LOCKED.PRO".
      *
      *ZA205.
      *      EXIT PROGRAM.

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
               WHEN OTHER      MOVE "ERROR"     TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
