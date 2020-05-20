      $set LINKCOUNT"384" GNT"CRP022.GNT"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******     ****     ******    ******    *
      *   **    **  **    **  **    **   **  **   **    **  **    **   *
      *   **        **    **  **    **  **    **       **        **    *
      *   **        ******    *******   **    **     **        **      *
      *   **        **   **   **        **    **    **        **       *
      *   **    **  **    **  **         **  **    **        **        *
      *    ******   **    **  **          ****    ********  ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R ' S   C A S H   F L O W   P R O G R A M    *
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
       PROGRAM-ID.      CRP022.
       AUTHOR.          J.W. LEMMON (APAC).
       DATE-WRITTEN.    AUGUST, 1983.

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

       COPY "PARAM.SL".

       COPY "PDCHEQ.SL".

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

       COPY "PDCHEQ.FDE".

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
       77  WS-NETKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-PARKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-ERROR                PIC  9(01)           VALUE 0.
       77  WS-S                    PIC  9(04)    COMP-5.
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S6                   PIC  9(04)    COMP-5.
       77  WS-IXS1                 PIC  9(04)    COMP-5.
       77  WS-IXS2                 PIC  9(04)    COMP-5.
       77  WS-IXS3                 PIC  9(04)    COMP-5.
       77  WS-IXS4                 PIC  9(04)    COMP-5.
       77  WS-KEY1                 PIC  9(04)    COMP-5.
       77  WS-CHAR1                PIC  X(01).
       77  WS-ITM1                 PIC  X(12).
       77  WS-ORD                  PIC  X(01).
       77  WS-SEQ                  PIC  X(01).
       77  WS-OPTION               PIC  X(01).
       77  WS-LINES                PIC  9(04)    COMP-5 VALUE 0.
       77  WS-PAGE                 PIC  9(04)    COMP-5 VALUE 0.
       77  WS-IND                  PIC  9(01).
       77  WS-PGE-LENGTH           PIC  9(02)           VALUE 66.
       77  WS-PRN-LENGTH           PIC  9(02)           VALUE 62.
       77  WS-H1                   PIC  X(05)           VALUE "DATE:".
       77  WS-H3                   PIC  X(05)           VALUE "PAGE:".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.

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
           03  CRT-PROGRAM         PIC  X(15) VALUE "CRP022".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "CREDITOR - CASH FLOW REPORT".
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

           03  R-L2                              REDEFINES W02-PRN-LINE.
               05  R-H1            PIC  X(06).
               05  R-DTE1          PIC  Z9/99/9999.
               05  FILLER          PIC  X(18).
               05  R-H2            PIC  X(63).
               05  FILLER          PIC  X(26).
               05  R-H3            PIC  X(06).
               05  R-PGE           PIC  ZZ9.

           03  R-L3                              REDEFINES W02-PRN-LINE.
               05  R-H4            PIC  X(06).
               05  R-H5            PIC  X(34).
               05  R-H6            PIC  X(12).
               05  R-H7            PIC  X(12).
               05  R-H8            PIC  X(12).
               05  R-H9            PIC  X(12).
               05  R-H10           PIC  X(11).
               05  R-H11           PIC  X(12).
               05  R-H12           PIC  X(12).
               05  R-H13           PIC  X(09).

           03  R-L4                              REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(64).
               05  R-H14           PIC  X(12).
               05  R-H15           PIC  X(12).
               05  R-H16           PIC  X(12).
               05  R-H17           PIC  X(12).
               05  R-H18           PIC  X(12).
               05  R-H19           PIC  X(08).

           03  R-L5                              REDEFINES W02-PRN-LINE.
               05  CF-SNO          PIC  X(06).
               05  FILLER          PIC  X(01).
               05  CF-SUPP         PIC  X(29).
               05  CF-BAL          PIC  Z(07)9.99-.
               05  CF-AREAR        PIC  Z(07)9.99-.
               05  CF-30D          PIC  Z(07)9.99-.
               05  CF-60D          PIC  Z(07)9.99-.
               05  CF-90D          PIC  Z(07)9.99-.
               05  CF-120D         PIC  Z(07)9.99-.
               05  CF-150D         PIC  Z(07)9.99-.
               05  CF-180D         PIC  Z(07)9.99-.

           03  R-L11                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(02).
               05  CF-BNO          PIC  9(02).
               05  FILLER          PIC  X(03).
               05  CF-BRANCH       PIC  X(24).
               05  FILLER          PIC  X(07).
               05  CF-SDEB         PIC  Z(06)9.99.
               05  FILLER          PIC  X(01).
               05  CF-SCRE         PIC  Z(06)9.99.
               05  FILLER          PIC  X(07).
               05  CF-CDEB         PIC  Z(06)9.99.
               05  FILLER          PIC  X(01).
               05  CF-CCRE         PIC  Z(06)9.99.
               05  FILLER          PIC  X(07).
               05  CF-URPD         PIC  Z(06)9.99.
               05  FILLER          PIC  X(01).
               05  CF-URPC         PIC  Z(06)9.99.
               05  FILLER          PIC  X(08).
               05  CF-PERC2        PIC  Z(02)9.99.
               05  FILLER          PIC  X(03).

       01  W10-MONTHS.
           03  W10-MONTH-NAMES     PIC  X(36)    VALUE "JanFebMarAprMayJunJulAugSepOctNovDec".
           03  W10-RMONTH                        REDEFINES W10-MONTH-NAMES.
               05  W10-MONTH       PIC  X(03)    OCCURS 12.
       01  W10-STOCK.
           03  W10-VSELL           PIC S9(07)V99 COMP-3.
           03  W10-VCASH           PIC S9(07)V99 COMP-3.
           03  W10-VWSALE          PIC S9(07)V99 COMP-3.
       01  W20-CALCS.
           03  W20-RESULT          PIC  9(06)V99.
           03  W20-RESULT-1                      REDEFINES W20-RESULT.
               05  W20-WHOLE       PIC  9(06).
               05  W20-DECIMAL     PIC  9(02).
           03  W20-ORD             PIC  9(07)    COMP-3.

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
           03  W25-RESULT1                       REDEFINES W25-RESULT.
               05  W25-WHOLE       PIC  9(05).
               05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2                       REDEFINES W25-RESULT.
               05  W25-KEY         PIC  9(04).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).
       01  W25-VALUES.
           03  W25-CFLOW           PIC S9(07)V99 COMP-3 OCCURS 6.
           03  W25-V1              PIC S9(09)V99 COMP-3.
           03  W25-V2              PIC S9(09)V99 COMP-3.
       01  W25-EDIT.
           03  W25-5.
               05  W25-1           PIC  ZZZZ9.
           03  W25-3V2.
               05  W25-2           PIC  ZZ9.99.
           03  W25-7V2.
               05  W25-3           PIC  Z(06)9.99.
           03  W25-3N.
               05  W25-4           PIC  ZZ9.
           03  W25-DTE.
               05  W25-DATE        PIC  99/99/99.
           03  W25-6.
               05  W25-NO          PIC  Z(05)9.
           03  W25-5V2.
               05  W25-VAL         PIC  Z(04)9.99.
           03  W25-S5V2.
               05  W25-SVAL        PIC  Z(04)9.99DB.
           03  W25-S7V2.
               05  W25-VAL2        PIC  Z(06)9.99DB.
       01  W30-DATE.
           03  W30-MM              PIC  X(03).
           03  FILLER              PIC  X(01)    VALUE ",".
           03  W30-YY              PIC  9(02).

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W50-H.
           03  FILLER              PIC  X(01)    VALUE "(".
           03  W50-MONTH           PIC  X(03).
           03  FILLER              PIC  X(01)    VALUE ")".

       01  W55-DEL.
           03  W55-MONTH           PIC  X(03).
           03  FILLER              PIC  X(01)    VALUE ",".
           03  W55-YY              PIC  9(02).

       01  W60-ITMCODE.
           03  W60-ITM.
               05  W60-ICHAR       PIC  X(01)    OCCURS 12.

       01  W60-H2.
           03  W60-COMPANY         PIC  X(40).
           03  FILLER              PIC  X(02)    VALUE "-".
           03  W60-DESC            PIC  X(27).

       01  W65-CREDITOR.
           03  W65-CFLOW           PIC S9(09)V99 COMP-3 OCCURS 6.

       01  W70-VALUES.
           03  W70-BAL             PIC S9(09)V99 COMP-3.
           03  W70-AREAR           PIC S9(09)V99 COMP-3.
           03  W70-30D             PIC S9(09)V99 COMP-3.
           03  W70-60D             PIC S9(09)V99 COMP-3.
           03  W70-90D             PIC S9(09)V99 COMP-3.
           03  W70-120D            PIC S9(09)V99 COMP-3.
           03  W70-150D            PIC S9(09)V99 COMP-3.
           03  W70-180D            PIC S9(09)V99 COMP-3.

       COPY "W70CRED.WS".

       01  W90-TABLE.
           03  W90-ENTRIES.
               05  W90-ENTRY                     OCCURS 80.
                   07  W90-NAME    PIC  X(04).
                   07  W90-KEY     PIC  9(04)    COMP-5.
           03  W90-R1                            REDEFINES W90-ENTRIES.
               05  W90-F20         PIC  X(120).
               05  W90-L60         PIC  X(360).
           03  W90-R2                            REDEFINES W90-ENTRIES.
               05  W90-F60         PIC  X(360).
               05  W90-L20         PIC  X(120).
           03  W90-R3                            REDEFINES W90-ENTRIES.
               05  W90-20          PIC  X(120).
               05  W90-40          PIC  X(120).
               05  W90-60          PIC  X(120).
               05  W90-80          PIC  X(120).

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
          IF LS0-CRLEV < 2
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA105.
             PERFORM ZA000-INIT.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *     **  *   *   * *   *  *  **  *   *   *     *   *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *     *  **   *     *   *  *  *  **   *   *     *  *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** *   *
      *
             PERFORM OPEN-PRINTER.
             MOVE "P"                TO WS-COMMAND.
             PERFORM CA000.
             GO TO AA100.

       AA050.
             MOVE "H"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DET.
             MOVE WS-H1              TO R-H1.
             MOVE W60-H2             TO R-H2.
             MOVE WS-H3              TO R-H3.
             ADD 1                   TO WS-PAGE.
             MOVE TODAY-DDMMYY       TO R-DTE1.
             MOVE WS-PAGE            TO R-PGE.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.

       AA100.
             MOVE "C"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             PERFORM ERASE-SCREEN.

       AA105.
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

       COPY "ABORT.PRN".

      *
      *            ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *                PRINTER SUCH AS OPEN AND PRINT
      *
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³             PERFORM OPEN-PRINT-FILE                       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *
      *         PRINT A LINE, ADVANCE LINE/PAGE, CHANGE STYLE ETC
      *     The following variables need to be set before calling this
      *     routine.
      *     
      *    COMMAND: C = Close printer
      *             E = End of report
      *             O = Open printer
      *             P = Print a line
      *             S = Skip to new page
      *
      *       WS-COMMAND      PIC  X(01).
      *
      *    PRINTER: D = Disk
      *             S = Screen
      *             1 = Lpt1
      *             2 = Lpt2
      *             3 = Lpt3
      *             4 = Com1
      *             5 = Com2
      *             8 = USB/WINDOWS
      *
      *       WS-PRINTER.
      *         WS-PRINT-NO PIC  9(01).
      *
      *    ADVANCE:   0 = Print - No advance
      *             1-3 = Print advance lines specified
      *             4-? = Print advance 1 line, then use a loop
      *                   to advance the remaining lines.
      *              99 = Print and advance to a new page.
      *
      *       WS-ADVANCE      PIC  9(02).
      *
      *    REPORT: 1 = Picking Slips
      *            2 = Invoices/Credit Notes
      *            3 = General Reports (Stock)
      *            4 = General Reports (Creditors)
      *            5 = General Reports (Debtors)
      *            6 = General Reports (G/Ledger)
      *            7 = Statements
      *            8 = Labels
      *            9 = Quotations
      *
      *      WS-REPORT           PIC  9(01).
      *
      *      See W02-PRN-TYPE for details of Print style.
      *
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³               PERFORM CALL-PRINTUTL                       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´

       COPY "PRINTUTL.AP4".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AB00-READ SECTION.

       COPY "CONTROL.RD".

       COPY "CREDIT.RD".

       COPY "PARAM.RD".

       COPY "PDCHEQ.RD".

      *
      *       *** ****  **** ****  *** *****  ***  **** 
      *      *    *   * *    *   *  *    *   *   * *   *
      *      *    ****  ***  *   *  *    *   *   * **** 
      *      *    *   * *    *   *  *    *   *   * *   *
      *       *** *   * **** ****  ***   *    ***  *   *
      * 
      *                 ***  ***   ***  *   *
      *                *    *   * *     *   *
      *                *    *****  ***  *****
      *                *    *   *     * *   *
      *                 *** *   *  ***  *   *
      * 
      *               **** *      ***  *     *
      *               *    *     *   * *     *
      *               ***  *     *   * *  *  *
      *               *    *     *   * * * * *
      *               *    *****  ***   *   * 
      * 
       CA000      SECTION 5.
       CA00.
             PERFORM ERASE-SCREEN.
             DISPLAY "Busy printing creditor Cash Flow" AT 0620
                      WITH FOREGROUND-COLOR Grey HIGHLIGHT.

       CA02.
             MOVE "'A'ccount or 'N'ame sequence  [N]"  TO WS-OPT-MES.
             MOVE "N"                TO WS-OPTION.
             MOVE "R"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
             MOVE WS-OPTION          TO WS-SEQ.

       CA03.
             DISPLAY "Account :" AT 0804.
             MOVE "Print accounts with zero balances Y/N?  [N]" TO WS-OPT-MES.
             MOVE "N"                TO WS-OPTION.
             MOVE "1"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
             MOVE WS-OPTION          TO WS-ORD.
             MOVE ZERO               TO WS-PAGE.
      *
      *    ****   1 7 C P I ( C O N D E N S E D   P R I N T )
      *
             MOVE 0                  TO WS-ADVANCE.
             MOVE 3                  TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             MOVE "CREDITORS CASH FLOW" TO W60-DESC.
             MOVE ZERO               TO W70-BAL W70-60D W70-30D W70-AREAR01 W70-90D W70-120D W70-150D W70-180D.

       CA05.
             PERFORM AA050.
             MOVE "SUPP"             TO R-H4.
             MOVE "CURRENT"          TO R-H6.
             MOVE "PMT-DUE"          TO R-H7.
             MOVE "30 DAYS"          TO R-H8.
             MOVE "60 DAYS"          TO R-H9.
             MOVE "90 DAYS"          TO R-H10.
             MOVE "120 DAYS"         TO R-H11.
             MOVE "150 DAYS"         TO R-H12.
             MOVE "180 DAYS"         TO R-H13.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE "AC-NO"            TO R-H4.
             MOVE "SUPPLIER NAME"    TO R-H5.
             MOVE "BALANCE"          TO R-H6.
             MOVE W12-MONTH          TO WS-S2.

       CA10.
             ADD 1                   TO WS-S2.
           IF WS-S2 > 12
               SUBTRACT 12           FROM WS-S2.
             MOVE W10-MONTH(WS-S2)   TO W50-MONTH.

       CA15.
             MOVE W50-H              TO R-H14.
             PERFORM CA10.
             MOVE W50-H              TO R-H15.
             PERFORM CA10.
             MOVE W50-H              TO R-H16.
             PERFORM CA10.
             MOVE W50-H              TO R-H17.
             PERFORM CA10.
             MOVE W50-H              TO R-H18.
             PERFORM CA10.
             MOVE W50-H              TO R-H19.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE "D"                TO W02-PRN-TYPE.

       CA20.
             MOVE "    !"        TO P-NAME P-NUMBER.
           IF WS-SEQ = "A"
               PERFORM START-AT-CRED-AC THRU READ-CREDIT-EXIT
           ELSE
               PERFORM START-AT-CRED-NAME THRU READ-CREDIT-EXIT.
           IF NOT(WS-F-ERROR = ZERO)
               GO TO CA35.

       CA25.
             PERFORM READ-CREDIT-NEXT THRU READ-CREDIT-EXIT.
           IF NOT(WS-F-ERROR = ZERO)
               GO TO CA30.
           IF NOT(WS-PRINTER = "S")
               DISPLAY P-NUMBER AT 0814 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta
                              "°"
                                P-NAME  WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.
           IF WS-ORD = "N"
               IF P-BAL = ZERO
                   GO TO CA25.
             MOVE P-NUMBER           TO CF-SNO.
             MOVE P-NAME             TO CF-SUPP.
             MOVE P-BAL              TO CF-BAL.
             ADD P-BAL               TO W70-BAL.
           IF P-OUT < ZERO
               ADD P-OUT             TO P-30D
               MOVE ZERO             TO P-OUT.
           IF P-30D < ZERO
               ADD P-30D             TO P-60D
               MOVE ZERO             TO P-30D.
           IF P-60D < ZERO
               ADD P-60D             TO P-90D
               MOVE ZERO             TO P-60D.
           IF P-90D < ZERO
               ADD P-90D             TO P-120D
               MOVE ZERO             TO P-90D.
           IF P-120D < ZERO
               ADD P-120D            TO P-150D
               MOVE ZERO             TO P-120D.
           IF P-150D < ZERO
               ADD P-150D            TO P-180D
               MOVE ZERO             TO P-150D.
           IF P-180D < ZERO
               ADD P-180D            TO P-OUT
               MOVE ZERO             TO P-180D.
             MOVE P-OUT              TO CF-AREAR.
             ADD P-OUT               TO W70-AREAR.
             MOVE P-30D              TO CF-30D.
             ADD P-30D               TO W70-30D.
             MOVE P-60D              TO CF-60D.
             ADD P-60D               TO W70-60D.
             MOVE P-90D              TO CF-90D.
             ADD P-90D               TO W70-90D.
             MOVE P-120D             TO CF-120D.
             ADD P-120D              TO W70-120D.
             MOVE P-150D             TO CF-150D.
             ADD P-150D              TO W70-150D.
             MOVE P-180D             TO CF-180D.
             ADD P-180D              TO W70-180D.
           IF W02-LINAGE < W02-PRN-LENGTH
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM CA05 THRU CA15.
             MOVE SPACES             TO R-DET.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****" TO R-DET
               GO TO CA40.
             GO TO CA25.

       CA30.
           IF W02-LINAGE > (W02-PRN-LENGTH - 2)
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               IF WS-PRINTER = "S"
                   MOVE 2            TO WS-ADVANCE
               ELSE
                   MOVE 99           TO WS-ADVANCE
               END-IF
               PERFORM CALL-PRINTUTL
               PERFORM CA05 THRU CA15.
             MOVE "CREDITOR TOTALS"  TO CF-SUPP.
             MOVE W70-BAL            TO CF-BAL.
             MOVE W70-AREAR          TO CF-AREAR.
             MOVE W70-30D            TO CF-30D.
             MOVE W70-60D            TO CF-60D.
             MOVE W70-90D            TO CF-90D.
             MOVE W70-120D           TO CF-120D.
             MOVE W70-150D           TO CF-150D.
             MOVE W70-180D           TO CF-180D.

       CA35.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL. 
             MOVE "******  END  REPORT  ******" TO  R-DET.

       CA40.
             MOVE 99                 TO WS-ADVANCE.
             MOVE "E"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             MOVE "P"                TO WS-COMMAND.

       CA999.
             EXIT.

      *    *************************************************************
      *                 I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT    SECTION 8.
       ZA000-START.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W12-TODAY.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY W60-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

       ZA05.
             MOVE 2                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
      *      PERFORM ERASE-SCREEN.
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
               WHEN 44         MOVE W02-PDCHEQ  TO WS-FILE
                               MOVE PDT-KEY     TO WS-KEYX
               WHEN OTHER      MOVE "ERROR"     TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
