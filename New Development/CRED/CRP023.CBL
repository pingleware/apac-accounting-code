      $set LINKCOUNT"512" GNT"CRP023.GNT"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******     ****     ******    ******    *
      *   **    **  **    **  **    **   **  **   **    **  **    **   *
      *   **        **    **  **    **  **    **       **         **   *
      *   **        ******    *******   **    **     **        ****    *
      *   **        **   **   **        **    **    **            **   *
      *   **    **  **    **  **         **  **    **       **    **   *
      *    ******   **    **  **          ****    ********   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R ' S   P U R C H A S E S   J O U R N A L    *
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
       PROGRAM-ID.      CRP023.
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
       77  WS-PERIOD               PIC  X(01).
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
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.   ³
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "CRP02".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "CREDITOR - PURCHASE JOURNAL REPORT".
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
               05  R-H2.
                   07  FILLER      PIC  X(11).
                   07  R-H2B       PIC  X(41).
                   07  FILLER      PIC  X(11).
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

           03  R-L6                              REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(40).
               05  R-H20           PIC  X(07).
               05  R-H21           PIC  X(20).
               05  R-H22.
                   07  FILLER      PIC  X(04).
                   07  R-H22A      PIC  X(16).
               05  R-H23           PIC  X(16).
               05  R-H24           PIC  X(14).
               05  R-H25           PIC  X(15).

           03  R-L7                              REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(05).
               05  R-H26           PIC  X(07).
               05  PJ-BN01         PIC  9(02).
               05  FILLER          PIC  X(01).
               05  PJ-BRANCH1      PIC  X(25).
               05  R-H27           PIC  X(17).
               05  PJ-BN02         PIC  9(02).
               05  FILLER          PIC  X(01).
               05  PJ-BRANCH2      PIC  X(24).
               05  FILLER          PIC  X(48).

           03  R-L8                              REDEFINES W02-PRN-LINE.
               05  PJ-SNO          PIC  X(06).
               05  FILLER          PIC  X(01).
               05  PJ-SUPP         PIC  X(32).
               05  FILLER          PIC  X(01).
               05  PJ-TRN          PIC  X(03).
               05  FILLER          PIC  X(04).
               05  PJ-GRNN.
                   07  PJ-GRNNO    PIC  Z(05)9.
               05  FILLER          PIC  X(01).
               05  PJ-GRNDT.
                   07  PJ-GRND     PIC  Z9/99/9999.
               05  FILLER          PIC  X(01).
               05  PJ-INVN         PIC  Z(07)9.
               05  FILLER          PIC  X(01).
               05  PJ-INVD         PIC  Z9/99/9999.
               05  FILLER          PIC  X(03).
               05  PJ-COST         PIC  Z(06)9.99DB.
               05  FILLER          PIC  X(02).
               05  PJ-SELL         PIC  Z(06)9.99DB.
               05  FILLER          PIC  X(02).
               05  PJ-URP          PIC  Z(06)9.99.
               05  FILLER          PIC  X(02).
               05  PJ-PERC         PIC  Z(02)9.99.
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

       COPY "W15.CRP".

       COPY "W15.WS".

           03  W15-YMD             PIC  9(08).
           03  W15-RYMD                          REDEFINES W15-YMD.
               05  W15-CN          PIC  9(02).
               05  W15-YR          PIC  9(02).
               05  W15-MT          PIC  9(02).
               05  W15-DY          PIC  9(02).
          03  W15-VALUE            PIC  9(07)V99.
          03  W15-VAL2             PIC  9(07)V99.

       01  W20-CALCS.
           03  W20-RESULT          PIC  9(06)V99.
           03  W20-RESULT-1                      REDEFINES W20-RESULT.
               05  W20-WHOLE       PIC  9(06).
               05  W20-DECIMAL     PIC  9(02).
           03  W20-ORD             PIC  9(07)    COMP-3.

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
          03  W25-V1               PIC S9(09)V99 COMP-3.
          03  W25-V2               PIC S9(09)V99 COMP-3.

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
              05  W25-DATE         PIC  99/99/9999.
           03  W25-6.
               05  W25-NO          PIC  Z(05)9.
           03  W25-5V2.
               05  W25-VAL         PIC  Z(04)9.99.
           03  W25-S5V2.
               05  W25-SVAL        PIC  Z(04)9.99DB.
           03  W25-S7V2.
               05  W25-VAL2        PIC  Z(06)9.99DB.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W60-H2.
           03  W60-COMPANY         PIC  X(40).
           03  FILLER              PIC  X(02)     VALUE "-".
           03  W60-DESC            PIC  X(27).

       01  W65-CREDITOR.
           03  W65-CFLOW           PIC S9(07)V99 COMP-3 OCCURS 6.

       01  W70-VALUES.
           03  W70-BAL             PIC S9(07)V99 COMP-3.
           03  W70-AREAR           PIC S9(07)V99 COMP-3.
           03  W70-30D             PIC S9(07)V99 COMP-3.
           03  W70-60D             PIC S9(07)V99 COMP-3.
           03  W70-90D             PIC S9(07)V99 COMP-3.
           03  W70-120D            PIC S9(07)V99 COMP-3.
           03  W70-150D            PIC S9(07)V99 COMP-3.
           03  W70-180D            PIC S9(07)V99 COMP-3.

       COPY "W70CRED.WS".

       01  W80-HEAD.
           03  FILLER              PIC  X(17) VALUE "For the period : ".
           03  W80-DATE1           PIC  Z9/99/9999.
           03  FILLER              PIC  X(04) VALUE " to ".
           03  W80-DATE2           PIC  Z9/99/9999.

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

       01  S05D.
           03  LINE  6 COLUMN 29 VALUE "PURCHASE JOURNAL REPORT"          FOREGROUND-COLOR Grey  HIGHLIGHT.
           03  BACKGROUND-COLOR Brown FOREGROUND-COLOR Brown HIGHLIGHT.
               05  LINE  8 COLUMN 25 VALUE "                                ".
               05  LINE  9 COLUMN 25 VALUE "    Transactions for Period.    ".
               05  LINE 10 COLUMN 25 VALUE "                                ".
               05  LINE 11 COLUMN 25 VALUE "    Starting Date :             ".
               05  LINE 12 COLUMN 25 VALUE "    Ending Date   :             ".
               05  LINE 13 COLUMN 25 VALUE "                                ".
               05  LINE 14 COLUMN 25 VALUE " Date formats must be: DDMMYYYY "                               .
               05  LINE 15 COLUMN 25 VALUE "                                ".

       01  S-TRAN.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
               05  LINE 19 COLUMN 34 PIC  X(06) FROM CT-CRED.
               05          COLUMN 40 PIC  X(03) FROM PJ-TRN.
               05          COLUMN 43 PIC  X(10) FROM PJ-GRNDT.
               05          COLUMN 53 PIC  X(06) FROM PJ-GRNN.
 
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
           IF LS0-CRLEV < 3
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
             PERFORM DA000
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
           IF WS-PERIOD = "A"
               MOVE 2                TO WS-ADVANCE
           ELSE
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE SPACES           TO R-DET
               MOVE W80-HEAD         TO R-H2B.
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
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *            T H I S   R O U T I N E   I S   U S E D   T O
      *       D I S P L A Y   I N F O R M A T I O N   M E S S A G E S
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Move the message to WS-DSP-MES and the top line of the   ³
      *    ³  message box to WS-MES-LINE. If WS-MES-LINE is zero the   ³
      *    ³  default will be 20.                                      ³
      *    ³                  PERFORM DISPLAY-MESSAGE                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *           T H I S   R O U T I N E   I S   U S E D   T O
      *            D I S P L A Y   E R R O R   M E S S A G E S
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                      ERROR-MESSAGE                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To display the error message higher or lower (default is  ³
      *    ³ line 20) Move the line number which must be used as the   ³
      *    ³ top line to WS-MES-LINE. The message to be displayed must ³
      *    ³ be in WS-ERR-MES. PERFORM ERROR-MESSAGE.                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.CRT".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                        OPT-MESSAGE                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine is used to allow the OPERATOR to respond to  ³
      *    ³ a request for an option, so that the correct action can   ³
      *    ³ be performed by the program. The routine will display the ³
      *    ³ message in a pop-up window and allow the OPERATOR to      ³
      *    ³ respond to the 'question'.                                ³
      *    ³                                                           ³
      *    ³ The option request must be placed in WS-OPT-MES and may   ³
      *    ³ not exceed 48 characters. The message will be centred in  ³
      *    ³ the window. An example of a message request follows:      ³
      *    ³                                                           ³
      *    ³   MOVE "Print transactions (Y/N) [ ]" TO WS-OPT-MES.      ³
      *    ³   MOVE Instruction    TO WS-INSTR.                        ³
      *    ³       [see Accptopt for instruction values]               ³
      *    ³   PERFORM OPT-MESSAGE.                                    ³
      *    ³                                                           ³
      *    ³ This would be displayed as:                               ³
      *    ³      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿   ³
      *    ³      ³          Print transactions (Y/N) [ ]          ³°° ³
      *    ³      ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°° ³
      *    ³        °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° ³
      *    ³                                                           ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³                                                           ³
      *    ³ The system will display the message box with the top line ³
      *    ³ as the value of WS-MES-LINE. If WS-MES-LINE is zero the   ³
      *    ³ default will be 20.                                       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "OPTION.CRT".

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

       COPY "DATE.CHK".

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

       COPY "CRTRAN.RD".

       COPY "CREDIT.RD".

       COPY "PARAM.RD".

      *
      * ****  *   * ****   *** *   *   *    ***  ****
      * *   * *   * *   * *    *   *  * *  *     *   
      * ****  *   * ****  *    ***** *****  ***  *** 
      * *     *   * *   * *    *   * *   *     * *   
      * *      ***  *   *  *** *   * *   *  ***  ****
      * 
      *    *****  ***  *   * ****  *   *   *   *    
      *      *   *   * *   * *   * **  *  * *  *    
      *      *   *   * *   * ****  * * * ***** *    
      *      *   *   * *   * *   * *  ** *   * *    
      *    **     ***   ***  *   * *   * *   * *****  
      * 
       DA000      SECTION 5.
       DA00.
             PERFORM EA000.
             DISPLAY "Busy printing purchases journal" AT 1720 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             MOVE ZERO               TO WS-PAGE.
      *
      *    ****   1 7 C P I ( C O N D E N S E D   P R I N T )
      *
             MOVE 0                  TO WS-ADVANCE.
             MOVE 3                  TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"   TO W02-PRN-TYPE.
             MOVE "PURCHASE JOURNAL" TO W60-DESC.
             MOVE 6                  TO WS-PARKEY.
       DA05.
             PERFORM AA050.
             MOVE "CRED"             TO R-H4.
             MOVE "REFER."           TO R-H7.
             MOVE "INVOICE"          TO R-H22A.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE "AC-NO"            TO R-H4.
             MOVE "CREDITOR NAME"    TO R-H5.
             MOVE "TRN"              TO R-H20.
             MOVE "NUMBER   DATE"    TO R-H21 R-H22.
             MOVE "COST PRICE"       TO R-H23.
             MOVE "SELLING"          TO R-H24.
             MOVE "U.R.P.     PERC"  TO R-H25.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE "D"                TO W02-PRN-TYPE.

       DA10.
             MOVE SPACES             TO CT-CRED.
           IF WS-PERIOD = "A"
               MOVE ZERO             TO CT-SDTE
           ELSE
               MOVE W15-S-YMD        TO CT-SDTE.
             MOVE ZERO               TO CT-CODE.
             MOVE "I"                TO CT-PJ.
             PERFORM START-AT-CTRN-PJ THRU READ-CRTRAN-EXIT.
           IF WS-F-ERROR = 4
               GO TO DA20.
             PERFORM READ-CRTRAN-NEXT THRU READ-CRTRAN-EXIT.
           IF (WS-F-ERROR = 4) OR (CT-PJ NOT = "I")
               GO TO DA20.
           IF WS-PERIOD = "S"
               IF (CT-SDTE > W15-E-YMD)
                   GO TO DA20.
                   MOVE 1            TO WS-S4.

       DA12.
             MOVE ZERO               TO W65-CFLOW(WS-S4).
           IF WS-S4 < 6
               ADD 1                 TO WS-S4
               GO TO DA12.
             DISPLAY "Transaction :" AT 1920.

       DA15.
             MOVE CT-CRED            TO P-NUMBER.
             PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
             MOVE P-NUMBER           TO PJ-SNO.
             MOVE P-NAME             TO PJ-SUPP.
             EVALUATE CT-CODE
               WHEN 01 OR 11  MOVE "INV" TO PJ-TRN
               WHEN 09 OR 10  MOVE "J/E" TO PJ-TRN
               WHEN 02 OR 12  MOVE "D/N" TO PJ-TRN
             END-EVALUATE.
           IF CT-REF NUMERIC
               MOVE CT-REFNO         TO PJ-GRNNO
           ELSE
               MOVE CT-REF           TO PJ-GRNN.
             MOVE CT-DTE             TO W15-YMD.
             MOVE W15-DY             TO W15-DD.
             MOVE W15-MT             TO W15-MM.
             MOVE W15-CN             TO W15-CC.
             MOVE W15-YR             TO W15-YY.
             MOVE W15-DATE           TO PJ-GRND.
           IF CT-SINV NUMERIC
               MOVE CT-SINN          TO PJ-INVNO
           ELSE
               MOVE CT-SINV          TO PJ-INVN.
           IF NOT(CT-SDTE = ZERO)
               MOVE CT-SDTE          TO W15-YMD
               MOVE W15-DY           TO W15-DD
               MOVE W15-MT           TO W15-MM
               MOVE W15-CN           TO W15-CC
               MOVE W15-YR           TO W15-YY
               MOVE W15-DATE         TO PJ-INVD.
             DISPLAY S-TRAN.
             MOVE CT-COST            TO PJ-COST W15-TCOST.
             MOVE CT-SELL            TO PJ-SELL W15-TSELL.
           IF CT-CODE = 09 OR 02 OR 12
               MULTIPLY -1 BY CT-COST  GIVING W15-TCOST
               MULT IPLY -1 BY CT-SELL GIVING W15-TSELL.
             SUBTRACT W15-TCOST FROM W15-TSELL GIVING W15-USELL.
             MOVE W15-USELL          TO PJ-URP.
             COMPUTE PJ-PERC = W15-USELL / W15-TSELL * 100.
           IF CT-CODE = 09 OR 02 OR 12
               ADD W15-TCOST         TO W65-CFLOW(1)
               ADD W15-TSELL         TO W65-CFLOW(3)
               ADD W15-USELL         TO W65-CFLOW(5)
           ELSE
               ADD W15-TCOST         TO W65-CFLOW(2)
               ADD W15-TSELL         TO W65-CFLOW(4)
               ADD W15-USELL         TO W65-CFLOW(6).
           IF W02-LINAGE < W02-PRN-LENGTH
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
              PERFORM DA05.
             MOVE SPACES             TO R-DET.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****" TO R-DET
               GO TO DA25.
             PERFORM READ-CRTRAN-NEXT THRU READ-CRTRAN-EXIT.
           IF (WS-F-ERROR = 4) OR (CT-PJ NOT = "I")
               GO TO DA20.
           IF WS-PERIOD = "S"
               IF (CT-SDTE > W15-E-YMD)
                   GO TO DA20.
             GO TO DA15.

       DA20.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE "TOTALS"           TO R-H27.
             SUBTRACT W65-CFLOW(1) FROM W65-CFLOW(2) GIVING W15-VALUE.
             SUBTRACT W65-CFLOW(3) FROM W65-CFLOW(4) GIVING W15-VAL2.
             MOVE W15-VALUE          TO PJ-COST.
             MOVE W15-VAL2           TO PJ-SELL.
           IF W15-VALUE < ZERO
               MULTIPLY -1 BY W15-VALUE GIVING W15-TCOST
           ELSE
               MOVE W15-VALUE        TO W15-TCOST.
           IF W15-VAL2 < ZERO
               MULTIPLY -1 BY W15-VAL2 GIVING W15-TSELL
           ELSE
               MOVE W15-VAL2         TO W15-TSELL.
             SUBTRACT W15-TCOST FROM W15-TSELL GIVING W15-USELL.
             MOVE W15-USELL          TO PJ-URP.
             COMPUTE PJ-PERC = W15-USELL / W15-TSELL * 100.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE "****** END REPORT ******" TO R-DET.

       DA25.
             MOVE 99                 TO WS-ADVANCE.
             MOVE "E"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             MOVE "P"                TO WS-COMMAND.

       DA999.
             EXIT.

      *              
      *    ****      *****      ***  ****  ***** ***  ***  *   *  *** 
      *    *   *       *       *   * *   *   *    *  *   * **  * *    
      *    ****  ***   *       *   * ****    *    *  *   * * * *  *** 
      *    *           *       *   * *       *    *  *   * *  **     *
      *    *         **         ***  *       *   ***  ***  *   *  *** 
      * 
       EA000      SECTION 5.
       EA00.
             MOVE "'A'll or 'S'elected for a period (A/S)  [A]" TO WS-OPT-MES.
             MOVE "e"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
             MOVE WS-OPTION          TO WS-PERIOD.
             MOVE W12-TODAY          TO W15-START W15-END.
           IF WS-PERIOD = "A"
               GO TO EA999.
             MOVE 8                  TO SHADE-ROW.
             MOVE 25                 TO SHADE-COL.
             MOVE 32                 TO SHADE-WIDTH.
             MOVE 8                  TO SHADE-LINES.
             DISPLAY S05D.
             PERFORM SCREEN-SHADOW.

       EA05.
             DISPLAY "Press "       AT 5052 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue           
                           "Esc"            WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                              "ape to Exit" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             MOVE W15-START          TO W22-DATE.
             MOVE 1145               TO DATEPOS.
             PERFORM DATE-CHECK-ACCEPT-PLUS THRU CHECK-DATE-EXIT.
      *      CALL "UTP\DateCheck" USING W-DATES.
           IF W22-INSTR = X"EC"
               GO TO EA999.           
             MOVE W20-DTE            TO W15-S-YMD.
             MOVE W22-DTE            TO W80-DATE1 W15-START.
             MOVE W15-END            TO W22-DATE.
             MOVE 1245               TO DATEPOS.
             PERFORM DATE-CHECK-ACCEPT-PLUS THRU CHECK-DATE-EXIT.
      *      CALL "UTP\DateCheck" USING W-DATES.
           IF W22-INSTR = X"EC"
               GO TO EA999.           
             MOVE W20-DTE            TO W15-E-YMD.
             MOVE W22-DTE            TO W80-DATE2 W15-END.
           IF W15-E-YMD < W15-S-YMD
               MOVE "End date is before start date" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO EA05.

       EA999.
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
             MOVE 2              TO WS-NETKEY.
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
               WHEN 4          MOVE W02-CRTRAN  TO WS-FILE
                               MOVE CT-KEY      TO WS-KEYX
               WHEN 15         MOVE WS-PARID    TO WS-FILE
                               MOVE WS-PARKEY   TO WS-KEY
               WHEN OTHER      MOVE "ERROR"     TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
