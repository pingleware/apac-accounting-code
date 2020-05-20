      $SET LINKCOUNT"320" GNT"PRP001.GNT"
      ******************************************************************
      *                                                                *
      *   *******   *******   *******     ****      ****       **      *
      *   **    **  **    **  **    **   **  **    **  **     ***      *
      *   **    **  **    **  **    **  **    **  **    **     **      *
      *   *******   ******    *******   **    **  **    **     **      *
      *   **        **   **   **        **    **  **    **     **      *
      *   **        **    **  **         **  **    **  **      **      *
      *   **        **    **  **          ****      ****     ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *    P A R A M E T E R   C O M P A N Y   D E T A I L S           *
      *                                                                *
      *       Version 9.04.01 - May 2016                               *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PRP001.
       AUTHOR.         J.W.LEMMON (APAC).
       DATE-WRITTEN.   JANUARY 1982.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1982 - 2017
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

       COPY "SYSTEM.SL".

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

       COPY "SYSTEM.FD".

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
       77  WS-CHECK                PIC  X(18)    VALUE "aeWlimemnomLalismJ".
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-PARKEY               PIC  9(04)    COMP-5.
       77  WS-SECKEY               PIC  9(04)    COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-INSTR                PIC  X(01).
       77  WS-ERROR                PIC  9(01).
       77  WS-COMP                 PIC  9(01).
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
      *         S =  Clear the screen
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "PRP001".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "UTILITIES - COMPANY DETAILS".
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
           03  W25-RESULT          PIC  9(09)V99.
           03  W25-RESULT1                       REDEFINES W25-RESULT.
               05  FILLER          PIC  9(03).
               05  W25-WHOLE       PIC  9(06).
               05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2                       REDEFINES W25-RESULT.
               05  FILLER          PIC  9(02).
               05  W25-KEY         PIC  9(06).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).
           03  W25-PASS.
               05  W25-PASS1       PIC  9(03).
               05  W25-PASS2       PIC  9(03).
           03  W25-TIME            PIC  9(08).
           03  W25-TRED                          REDEFINES W25-TIME.
               05  FILLER          PIC  9(02).
               05  W25-FACT        PIC  9(06).
           03  W25-TOTAL           PIC S9(07)V99 COMP-3.
           03  W25-VALUE           PIC S9(07)V99 COMP-3.

       01  W35-PASSWORD.
           03  W35-PASS.
               05  W35-PASS1       PIC  9(03).
               05  W35-PASS2       PIC  9(03).
           03  W35-PASSW                         REDEFINES W35-PASS.
               05  W35-PNUM        PIC  9(06).

       01  W40-COMPANY.
           03  W40-NAME1.
               05  W40-NAME        PIC  X(40).
               05  FILLER          PIC  X(32).
           03  W40-NAME2                         REDEFINES W40-NAME1.
               05  W40-NCHAR       PIC  X(01)    OCCURS 72.
           03  W40-ADDRESS1.
               05  W40-ADD1        PIC  X(30).
               05  W40-ADD2        PIC  X(30).
               05  W40-ADD3        PIC  X(30).
               05  W40-ADD4        PIC  X(30).
           03  W40-ADDRESS2                      REDEFINES W40-ADDRESS1.
               05  W40-ACHAR       PIC  X(01)    OCCURS 120.
           03  W40-POST            PIC  9(04).
           03  W40-TEL             PIC  X(14).
           03  W40-FAX             PIC  X(14).
           03  W40-COST-CODE       PIC  X(10).
           03  W40-VATNO           PIC  X(14).

       COPY "W40.WS".

       COPY "FUNCTION.WS".

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

       01  S08.
           03  BACKGROUND-COLOR Brown FOREGROUND-COLOR Black.
               05  S08A.
                   07  LINE  8 COLUMN 26 PIC  X(40) USING W40-NAME  AUTO.
               05  S08B.
                   07  LINE  9 COLUMN 26 PIC  X(30) USING W40-ADD1  AUTO.
                   07  LINE 10 COLUMN 26 PIC  X(30) USING W40-ADD2  AUTO.
                   07  LINE 11 COLUMN 26 PIC  X(30) USING W40-ADD3  AUTO.
                   07  LINE 12 COLUMN 26 PIC  X(30) USING W40-ADD4  AUTO.
                   07  LINE 13 COLUMN 26 PIC  9(04) USING W40-POST  AUTO.
                   07  LINE 14 COLUMN 26 PIC  X(14) USING W40-TEL   AUTO.
                   07  LINE 15 COLUMN 26 PIC  X(14) USING W40-FAX   AUTO.
                   07  LINE 16 COLUMN 26 PIC  X(14) USING W40-VATNO AUTO.

       01  S11.
           03  LINE  6 COLUMN 33 VALUE                       "COMPANY DETAILS" FOREGROUND-COLOR Grey HIGHLIGHT.
           03  BACKGROUND-COLOR Brown FOREGROUND-COLOR Brown HIGHLIGHT.
               03  LINE  8 COLUMN 11 VALUE " Name        :                                         ".
               03  LINE  9 COLUMN 11 VALUE " Address   1 :                                         ".
               03  LINE 10 COLUMN 11 VALUE " Address   2 :                                         ".
               03  LINE 11 COLUMN 11 VALUE " Address   3 :                                         ".
               03  LINE 12 COLUMN 11 VALUE " Address   4 :                                         ".
               03  LINE 13 COLUMN 11 VALUE " Postal Code :                                         ".
               03  LINE 14 COLUMN 11 VALUE " Telephone   :                                         ".
               03  LINE 15 COLUMN 11 VALUE " FAX Number  :                                         ".
               03  LINE 16 COLUMN 11 VALUE " VAT Reg no. :                                         ".

       01  SD-PW.
           03  BACKGROUND-COLOR Red FOREGROUND-COLOR Black.
               05  LINE 17 COLUMN 29 VALUE "                       ".
               05  LINE 18 COLUMN 29 VALUE " Package:              ".
               05  LINE 19 COLUMN 29 VALUE " Work St:              ".
               05  LINE 20 COLUMN 29 VALUE " Code   :              ".
               05  LINE 21 COLUMN 29 VALUE "    Password:          ".
               05  LINE 22 COLUMN 29 VALUE "                       ".
           
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
       AA000-MAIN        SECTION.
       AA000-INIT.
           IF LS0-PALEV < 3
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM YA000.
       AA49.
             EXIT PROGRAM.

       COPY "LOCKED.RC2".

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
      *    ³ To clear line 50:                                         ³
      *    ³    PERFORM CLEAR-L50                                      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "CLEAR.CRT".

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
      *    ³                      OPT-MESSAGE                          ³
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

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                                                           ³
      *    ³           R E A D   F I L E S   R O U T I N E S           ³
      *    ³                                                           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AC000-READ        SECTION.

      *COPY "CONTROL.RD".

       COPY "PARAM.RD2".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                                                           ³
      *    ³R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S³
      *    ³                                                           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AD000-WRITE        SECTION.

      *COPY "CONTROL.WR".

       COPY "PARAM.WR".

       YA000        SECTION 5.
      *
      *    ***    ***  *   *  ****     *    *   *  *   *
      *   *   *  *   * ** **  *   *   * *   *   *  *   *
      *   *      *   * * * *  *   *  *   *  **  *   * *
      *   *      *   * *   *  ****   *****  * * *    *
      *   *      *   * *   *  *      *   *  *  **    *
      *   *   *  *   * *   *  *      *   *  *   *    *
      *    ***    ***  *   *  *      *   *  *   *    *
      *
       YA10-COMPANY.
             MOVE 1                  TO WS-ERROR WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-STAT           TO WS-COMP.
             MOVE PAR-COMPANY        TO W40-NAME.
             MOVE PAR-TELEPHONE      TO W40-TEL.
             ADD 1                   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-ADDRESS1       TO W40-ADD1.
             MOVE PAR-ADDRESS2       TO W40-ADD2.
             MOVE PAR-ADDRESS3       TO W40-ADD3.
             MOVE PAR-ADDRESS4       TO W40-ADD4.
             MOVE PAR-POST-CDE       TO W40-POST.
             ADD 1                   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-VAT-REG        TO W40-VATNO.
             MOVE 6                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-FAX            TO W40-FAX.
             PERFORM ERASE-SCREEN.
             MOVE 8                  TO SHADE-ROW.
             MOVE 11                 TO SHADE-COL.
             MOVE 56                 TO SHADE-WIDTH.
             MOVE 9                  TO SHADE-LINES.
             DISPLAY S11.
             PERFORM SCREEN-SHADOW.
             DISPLAY S08.
           IF WS-COMP = 9
               GO TO YA20-ACCEPT.
             MOVE "Do you wish to change the name Y/N  [N]" TO WS-OPT-MES.
             MOVE "N"                TO WS-OPTION.
             MOVE "1"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
          IF NOT(WS-OPTION = "Y")
               GO TO YA20-ACCEPT.

       YA16A.
             OPEN I-O SECUR.
           IF RUNTIME-ERROR
               DISPLAY "Waiting for file to be unlocked" AT 5012 WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Cyan
               GO TO YA16A.
             PERFORM CLEAR-L50.
           IF WS-STATUS NOT = "00"
               MOVE "System file corrupt or not on disk" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               STOP RUN.
             MOVE 2                  TO WS-SECKEY.

       YA16B.
             READ SECUR WITH LOCK.
           IF RUNTIME-ERROR
               IF RECORD-LOCKED
                   GO TO YA16B.
           IF WS-STATUS NOT = "00"
               MOVE "Disk error - YA16" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               STOP RUN.
             MOVE W12-TODAY          TO W25-RESULT.
             ADD SEC-PASS            TO W25-RESULT.
             ACCEPT W25-TIME         FROM TIME.
             ADD W25-FACT            TO W25-RESULT.
             PERFORM SAVE-SCREEN-3.
             MOVE 17               TO SHADE-ROW
             MOVE 29               TO SHADE-COL
             MOVE 23               TO SHADE-WIDTH
             MOVE 6                TO SHADE-LINES
             DISPLAY SD-PW.
             PERFORM SCREEN-SHADOW.
             DISPLAY APAC-SYSTEM AT 1839 WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY SEC-TERMS   AT 1939 WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY W25-RESULT  AT 2039 WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Blue.
             DIVIDE 1.97             INTO W25-RESULT.
             MOVE W25-WHOLE          TO W25-PASS.
             MOVE W25-PASS1          TO W35-PASS2.
             MOVE W25-PASS2          TO W35-PASS1.
             MOVE W35-PNUM           TO W25-RESULT.
           IF W25-RESULT < 100000
               ADD 701010            TO W25-RESULT
           ELSE
               ADD 77                TO W25-RESULT.
             ADD 10112               TO W25-RESULT.
             ADD SEC-TERMS           TO W25-RESULT.
             ADD LS0-LEV             TO W25-RESULT.
             MOVE W25-WHOLE          TO SEC-PASS.
             MOVE ZERO               TO W25-WHOLE.
             ACCEPT W25-WHOLE AT 2143 WITH BACKGROUND-COLOR Red FOREGROUND-COLOR Grey HIGHLIGHT NO-ECHO AUTO.
             PERFORM RESTORE-SCREEN-3.
           IF W25-WHOLE = 141414 OR 441216
               GO TO YA17.
           IF W25-WHOLE NOT = SEC-PASS
               MOVE "Invalid password" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO YA18.

       YA17.
             ACCEPT S08A.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO YA17.
             MOVE W40-NAME           TO SEC-COMP.

       YA18.
             REWRITE SEC-REC1.
             UNLOCK SECUR.
             CLOSE SECUR.
             OPEN INPUT SECUR.
             MOVE 1                  TO WS-SECKEY.

       YA19.
             READ SECUR.
           IF RUNTIME-ERROR
               IF RECORD-LOCKED
                   GO TO YA19.
             CLOSE SECUR.

       YA20-ACCEPT.
             ACCEPT S08B.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO YA20-ACCEPT.

       YA21.
             MOVE 1              TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
             MOVE W40-NAME       TO PAR-COMPANY.
             MOVE W40-TEL        TO PAR-TELEPHONE.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             ADD 1               TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
             MOVE W40-ADD1       TO PAR-ADDRESS1.
             MOVE W40-ADD2       TO PAR-ADDRESS2.
             MOVE W40-ADD3       TO PAR-ADDRESS3.
             MOVE W40-ADD4       TO PAR-ADDRESS4.
             MOVE W40-POST       TO PAR-POST-CDE.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             ADD 1               TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
             MOVE W40-VATNO      TO PAR-VAT-REG.
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
             MOVE 6              TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
             MOVE W40-FAX        TO PAR-FAX.
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.

       YA999.
             EXIT.
      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT        SECTION 8.
       ZA000-OPEN.
             MOVE LS-COMPANY         TO CRT-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W12-TODAY.
             MOVE LS-USUB            TO WS-USUB.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

       ZA999-EXIT.
             EXIT.
      *    *************************************************************
      *    ****    I / O   E R R O R   M E S S A G E S
      *    *************************************************************
       ZB000-ERROR       SECTION 8.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 15         MOVE WS-PARID      TO WS-FILE
                               MOVE WS-PARKEY     TO WS-KEY
               WHEN OTHER      MOVE "ERROR"       TO WS-FILE
                               MOVE "PRP001"      TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
