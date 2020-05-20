      $set LINKCOUNT"320" GNT"CRP028.GNT"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******     ****     ******    ******    *
      *   **    **  **    **  **    **   **  **   **    **  **    **   *
      *   **        **    **  **    **  **    **       **   **    **   *
      *   **        ******    *******   **    **     **      ******    *
      *   **        **   **   **        **    **    **      **    **   *
      *   **    **  **    **  **         **  **    **       **    **   *
      *    ******   **    **  **          ****    ********   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *       C R E D I T O R ' S   D I S C O U N T   L I S T I N G    *
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
       PROGRAM-ID.      CRP028.
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
           03  CRT-PROGRAM         PIC  X(15)           VALUE "CRP028".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40)           VALUE "CREDITOR - DISCOUNT LISTING".
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
           03  W02-PRN-STATUS      PIC  X(01)           VALUE "C".
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
               05  R-H2.
                   07  FILLER      PIC  X(11).
                   07  R-H2B       PIC  X(41).
                   07  FILLER      PIC  X(11).
               05  FILLER          PIC  X(26).
               05  R-H3            PIC  X(06).
               05  R-PGE           PIC  ZZ9.

           03  R-L19                             REDEFINES W02-PRN-LINE.
               05  A-H1            PIC  X(06).
               05  A-H2            PIC  X(41).
               05  A-H3            PIC  X(61).
               05  A-H4            PIC  X(05).
               05  A-H5            PIC  X(19).

           03  R-L20                             REDEFINES W02-PRN-LINE.
               05  A-ACNO          PIC  X(06).
               05  FILLER          PIC  X(01).
               05  A-NAME          PIC  X(39).
               05  FILLER          PIC  X(01).
               05  A-ADDRESS       PIC  X(60).
               05  FILLER          PIC  X(01).
               05  A-PC            PIC  9(04).
               05  FILLER          PIC  X(01).
               05  A-30            PIC  9(01).
               05  FILLER          PIC  X(02).
               05  A-60            PIC  9(01).
               05  FILLER          PIC  X(02).
               05  A-90            PIC  9(01).
               05  FILLER          PIC  X(03).
               05  A-120           PIC  9(01).
               05  FILLER          PIC  X(03).
               05  A-150           PIC  9(01).
               05  FILLER          PIC  X(03).
               05  A-180           PIC  9(01).

       01  W10-STOCK.
           03  W10-VSELL      PIC S9(07)V99  COMP-3.
           03  W10-VCASH      PIC S9(07)V99  COMP-3.
           03  W10-VWSALE     PIC S9(07)V99  COMP-3.

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
           03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
               05  W25-WHOLE   PIC 9(05).
               05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).

       01  W25-VALUES.
           03  W25-CFLOW OCCURS 6 PIC S9(07)V99 COMP-3.
          03  W25-V1        PIC S9(09)V99 COMP-3.
          03  W25-V2        PIC S9(09)V99 COMP-3.

       01  W25-EDIT.
           03  W25-5.
               05  W25-1       PIC ZZZZ9.
           03  W25-3V2.
               05  W25-2       PIC ZZ9.99.
           03  W25-7V2.
               05  W25-3       PIC Z(06)9.99.
           03  W25-3N.
               05  W25-4       PIC ZZ9.
           03  W25-DTE.
               05  W25-DATE    PIC 99/99/99.
           03  W25-6.
               05  W25-NO      PIC Z(05)9.
           03  W25-5V2.
               05  W25-VAL     PIC Z(04)9.99.
           03  W25-S5V2.
               05  W25-SVAL    PIC Z(04)9.99DB.
           03  W25-S7V2.
               05  W25-VAL2    PIC Z(06)9.99DB.

       01  W30-DATE.
           03  W30-MM          PIC X(03).
           03  FILLER          PIC X(01)     VALUE ",".
           03  W30-YY          PIC 9(02).

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W50-H.
           03  FILLER          PIC X(01)     VALUE "(".
           03  W50-MONTH       PIC X(03).
           03  FILLER          PIC X(01)     VALUE ")".

       01  W55-DEL.
           03  W55-MONTH       PIC X(03).
           03  FILLER          PIC X(01)     VALUE ",".
           03  W55-YY          PIC 9(02).

       01  W60-ITMCODE.
           03  W60-ITM.
               05  W60-ICHAR OCCURS 12 PIC X(01).

       01  W60-H2.
           03  W60-COMPANY     PIC X(40).
           03  FILLER          PIC X(02)     VALUE "-".
           03  W60-DESC        PIC X(27).

       01  W65-CREDITOR.
           03  W65-CFLOW       PIC S9(07)V99 COMP-3 OCCURS 6.

       01  W70-VALUES.
           03  W70-BAL         PIC S9(07)V99 COMP-3.
           03  W70-AREAR       PIC S9(07)V99 COMP-3.
           03  W70-30D         PIC S9(07)V99 COMP-3.
           03  W70-60D         PIC S9(07)V99 COMP-3.
           03  W70-90D         PIC S9(07)V99 COMP-3.
           03  W70-120D        PIC S9(07)V99 COMP-3.
           03  W70-150D        PIC S9(07)V99 COMP-3.
           03  W70-180D        PIC S9(07)V99 COMP-3.

       COPY "W70CRED.WS".

       01  W90-TABLE.
           03  W90-ENTRIES.
               05  W90-ENTRY    OCCURS 80.
                   07  W90-NAME PIC X(04).
                   07  W90-KEY  PIC 9(04)     COMP-5.
           03  W90-R1           REDEFINES W90-ENTRIES.
               05  W90-F20      PIC X(120).
               05  W90-L60      PIC X(360).
           03  W90-R2           REDEFINES W90-ENTRIES.
               05  W90-F60      PIC X(360).
               05  W90-L20      PIC X(120).
           03  W90-R3           REDEFINES W90-ENTRIES.
               05  W90-20       PIC X(120).
               05  W90-40       PIC X(120).
               05  W90-60       PIC X(120).
               05  W90-80       PIC X(120).

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
          IF LS0-CRLEV < 1
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
             PERFORM BB000.
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

      /
       BB000        SECTION 5.
       BB00.
             PERFORM ERASE-SCREEN.
            DISPLAY "Busy printing creditor discount list" AT 0420
                      WITH FOREGROUND-COLOR Grey HIGHLIGHT.

       BB10.
             DISPLAY "A"                AT 0604 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                      "ccount or "              WITH FOREGROUND-COLOR Cyan
                                "N"             WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                                 "ame sequence" WITH FOREGROUND-COLOR Cyan.
             MOVE "N"                TO WS-SEQ.

       BB15.
             ACCEPT WS-SEQ AT 0629 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO.
             CALL "CBL_TOUPPER" USING WS-SEQ BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-SEQ = "A" OR "N")
               GO TO BB15.
             DISPLAY "Account :" AT 0804.
             MOVE ZERO               TO WS-PAGE.
      *
      *    ****   1 7 C P I ( C O N D E N S E D   P R I N T )
      *
             MOVE 0                  TO WS-ADVANCE.
             MOVE 3                  TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             MOVE "CREDITORS DISCOUNT LIST" TO W60-DESC.
       BB95.
             PERFORM AA050.
             MOVE "ACNO"                TO A-H1.
             MOVE "CREDITORS NAME"      TO A-H2.
             MOVE "CREDITORS ADDRESS"   TO A-H3.
             MOVE "P/C"                 TO A-H4.
             MOVE "30 60   30D%   60D%" TO A-H5.
             MOVE 2                     TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE "D"                TO W02-PRN-TYPE.

       BB100.
             MOVE "    !"            TO P-NAME P-NUMBER.
           IF WS-SEQ = "A"
               PERFORM START-AT-CRED-AC THRU READ-CREDIT-EXIT
           ELSE
               PERFORM START-AT-CRED-NAME THRU READ-CREDIT-EXIT.
           IF NOT(WS-F-ERROR = ZERO)
               GO TO BB115.
       BB105.
             PERFORM READ-CREDIT-NEXT THRU READ-CREDIT-EXIT.
           IF NOT(WS-F-ERROR = ZERO)
               GO TO BB115.
           IF NOT(WS-PRINTER = "S")
               DISPLAY P-NUMBER          AT 0814 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta 
                               "°" P-NAME        WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.
             MOVE P-NUMBER           TO A-ACNO.
             MOVE P-NAME             TO A-NAME.
             MOVE P-ADDR             TO A-ADDRESS.
             INSPECT A-ADDRESS REPLACING ALL "*" BY ".".
             MOVE P-PC               TO A-PC.
           IF NOT(P-30-DISC = ZERO)
               MOVE "1"              TO A-30
               MOVE P-30-DISC        TO A-30-DISC.
           IF NOT(P-60-DISC = ZERO)
               MOVE "1"              TO A-60
               MOVE P-60-DISC        TO A-60-DISC.
           IF W02-LINAGE < W02-PRN-LENGTH
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM BB95.
             MOVE SPACES             TO R-DET.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****" TO R-DET
               GO TO BB120.
             GO TO BB105.

       BB115.
            MOVE 1                   TO WS-ADVANCE.
            PERFORM CALL-PRINTUTL.
            MOVE "******  END REPORT  ******" TO R-DET.

       BB120.
             MOVE 99                 TO WS-ADVANCE.
             MOVE "E"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             MOVE "P"                TO WS-COMMAND.

       BB999.
             EXIT.

      *    *************************************************************
      *                 I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT    SECTION 8.
       ZA000-START.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY.
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
             PERFORM ERASE-SCREEN.
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
