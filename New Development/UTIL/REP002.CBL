      $SET LINKCOUNT"256" GNT"REP002.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****      ****     ******    *
      *   **    **  **        **    **   **  **    **  **   **    **   *
      *   **    **  **        **    **  **    **  **    **        **   *
      *   ******    *****     *******   **    **  **    **      **     *
      *   **   **   **        **        **    **  **    **    **       *
      *   **    **  **        **         **  **    **  **    **        *
      *   **    **  ********  **          ****      ****    ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *       M O N T H - E N D   C L E A R   P R O G R A M            *
      *                                                                *
      *       Version 9.04.04 - February/March 2018                    *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     REP002.
       AUTHOR.         James William Lemmon.
       DATE-WRITTEN.   AUGUST 1982.

               COPYRIGHT NOTICE: COPYRIGHT (C) 1982 - 2018
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

       COPY "CONTROL.SLE".

       COPY "DEPART.SLE".

       COPY "PARAM.SLE".

       COPY "SORDER.SLE".

       COPY "STOCK.SLE".

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

       COPY "CONTROL.FD".

       COPY "DEPART.FD".

       COPY "PARAM.FD".

       COPY "SORDER.FD".

       COPY "STOCK.FD".

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
       77  WS-CHECK                PIC  X(18)         VALUE "aeWlimemnomLalismJ".
       77  WS-CLEAR                PIC  9(01)         VALUE ZERO.
       77  WS-AUD                  PIC  9(06)  COMP-5 VALUE ZERO.
       77  WS-S1                   PIC  9(04)  COMP-5.
       77  WS-S2                   PIC  9(04)  COMP-5.
       77  WS-S3                   PIC  9(04)  COMP-5.
       77  WS-S4                   PIC  9(04)  COMP-5.
       77  WS-S5                   PIC  9(04)  COMP-5.
       77  WS-S6                   PIC  9(04)  COMP-5.
       77  WS-S7                   PIC  9(04)  COMP-5.
       77  WS-NO                   PIC  9(04)  COMP-5.
       77  WS-PARKEY               PIC  9(04)  COMP-5.
       77  WS-NETKEY               PIC  9(04)  COMP-5.
       77  WS-AUDKEY               PIC  9(06)  COMP-5.
       77  WS-PAGE                 PIC  9(04)  COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-ERROR                PIC  9(01).
       77  TODAY-DDMMYY            PIC  9(08)  COMP-5.
       77  WS-USUB                 PIC  9(04)  COMP-5.

       COPY "WS.WS".

       01  WS-PARID.
           03  WS-SYS-ID       PIC  X(03).

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
           03  CRT-PROGRAM         PIC  X(15) VALUE "REP002".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "MONTH-END CLEAR".
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

       COPY "W12.WS".

       COPY "W15.WS".

       COPY "DateVariables.cpy".

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

       COPY "FILE-PEND.IDS".

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

       COPY "MENUINST.CRT".

       01  S05B.
          03  LINE  5 COLUMN 28 VALUE "STATISTICS MONTH-END CLEAR" FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S05C.
           03  BACKGROUND-COLOR Green FOREGROUND-COLOR Black.
               05  LINE  7 COLUMN 29 VALUE "                      ".
               05  LINE  8 COLUMN 29 VALUE " 1"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 31 VALUE   " Debtors            ".
               05  LINE  9 COLUMN 29 VALUE " 2"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 31 VALUE   " Creditors          ".
               05  LINE 10 COLUMN 29 VALUE " 3"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 31 VALUE   " Stock              ".
               05  LINE 11 COLUMN 29 VALUE " 4"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 31 VALUE   " Salesman           ".
               05  LINE 12 COLUMN 29 VALUE " 5"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 31 VALUE   " All the above      ".
               05  LINE 13 COLUMN 29 VALUE " 6"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 31 VALUE   " Quotations         ".
               05  LINE 14 COLUMN 29 VALUE " 7"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 31 VALUE   " Sales Orders       ".
               05  LINE 15 COLUMN 29 VALUE " 8"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 31 VALUE   " Converted Orders   ".
               05  LINE 16 COLUMN 29 VALUE " 0"                      FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 31 VALUE   " Period-End Control ".
               03  LINE 17 COLUMN 29 VALUE "                      ".

       01  SD-LOCKED.
           03  BACKGROUND-COLOR Red FOREGROUND-COLOUR Black.
               05  LINE 12 COLUMN  8 VALUE "This program can only be run when no other workstations are logged".
               05  LINE 13 COLUMN  8 VALUE "into the accounting system.  All other  workstations should return".
               05  LINE 14 COLUMN  8 VALUE "to the program  control  menu or exit  from  the accounting system".
               05  LINE 15 COLUMN  8 VALUE "before you may continue.                                          ".
               05  LINE 16 COLUMN  8 VALUE "Files locked - Try later     Status                               ".
               05  LINE 16 COLUMN 44 PIC  X(02) FROM                            WS-STATUS                        FOREGROUND-COLOR Grey HIGHLIGHT.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Red HIGHLIGHT.
               05  LINE 19 COLUMN 32 VALUE "               ".
               05  LINE 20 COLUMN 32 VALUE " Press any key ".
               05  LINE 21 COLUMN 32 VALUE "               ".

      *
      *      ******   ******    *****    *****   ******  ******   **   **  ******    ****** 
      *      **   **  **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **   **  **  **   **   **  **       **      **   **  **   **  **  **    **
      *      ******   *****    **   **  **       *****   **   **  **   **  *****     *****
      *      **       **  **   **   **  **       **      **   **  **   **  **  **    **
      *      **       **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **       **   **   *****    *****   ******  ******    *****   **   **   ******
      *
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY PEND-FILE-IDS.
       AA00M           SECTION.
       AA00.
             PERFORM ZA000-INIT.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     P E R F O R M   M O N T H - E N D   S E C T I O N     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             PERFORM DA00M.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                    C L O S E   D O W N                    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             CLOSE NETWORK DEPART DEBTOR PARAM SORDER STOCK.

       AA55.
             EXIT PROGRAM.

       COPY "AA900.ALM".

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

       COPY "DATE.CHK".

      *    *************************************************************
      *            T H I S   R O U T I N E   I S   U S E D   T O
      *    D I S P L A Y   I N F O R M A T I O N   M E S S A G E S
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Move the message to WS-ERR-MES and the top line of the   ³
      *    ³  message box to WS-MES-LINE. If WS-MES-LINE is zero the   ³
      *    ³  default will be 20.                                      ³
      *    ³                  PERFORM DISPLAY-MESSAGE                  ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´

       COPY "DISPMESS.SCR".

       COPY "LOCKED.REC".

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
       AN00-READ        SECTION.
      *
      *          ***   ***  *   * ***** ****   ***  *             *
      *         *   * *   * *   *   *   *   * *   * *            *
      *         *     *   * **  *   *   *   * *   * *           *
      *         *     *   * * * *   *   ****  *   * *          *
      *         *     *   * *  **   *   *   * *   * *         *
      *         *   * *   * *   *   *   *   * *   * *        *
      *          ***   ***  *   *   *   *   *  ***  *****   *
      *
      *  *   * ***** ***** *   *  ***  ****  *   *     ***** *** *     *****
      *  *   * *       *   *   * *   * *   * *  *      *      *  *     *
      *  **  * *       *   *   * *   * *   * * *       *      *  *     *
      *  * * * ***     *   * * * *   * ****  **        ***    *  *     ***
      *  *  ** *       *   * * * *   * *   * * *       *      *  *     *
      *  *   * *       *   * * * *   * *   * *  *      *      *  *     *
      *  *   * *****   *    * *   ***  *   * *   *     *     *** ***** *****
      *
       READ-CONTROL.
             READ NETWORK.
           IF WS-STATUS = "00"
               MOVE ZERO             TO WS-F-ERROR
               GO TO READ-CONTROL-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 2                TO WS-F-ERROR
               PERFORM READ-ERROR.
           IF RECORD-LOCKED
               PERFORM LOCKED-RECORD
               GO TO READ-CONTROL.
             MOVE 2                  TO WS-F-ERROR.
             PERFORM READ-ERROR.

       READ-CONTROL-EXIT.
             EXIT.
      *
      *    ****  ***** ****    *   ****  *****    ***** *** *     *****
      *    *   * *     *   *  * *  *   *   *      *      *  *     *
      *    *   * *     *   * *   * *   *   *      *      *  *     *
      *    *   * ***   ****  ***** ****    *      ***    *  *     ***
      *    *   * *     *     *   * *  *    *      *      *  *     *
      *    *   * *     *     *   * *   *   *      *      *  *     *
      *    ****  ***** *     *   * *   *   *      *     *** ***** *****
      *
       READ-DEPART.
            READ DEPART  WITH IGNORE LOCK KEY IS DPT-CODE.
             GO TO READ-DEPART-STATUS.

       READ-DEPART-NEXT.
             READ DEPART NEXT.
           IF RECORD-LOCKED
               PERFORM LOCKED-RECORD
               GO TO READ-DEPART-NEXT.
           IF WS-STAT1 = "1"
               MOVE 7                TO WS-F-ERROR
               GO TO READ-DEPART-EXIT.
             GO TO READ-DEPART-STATUS.

       START-AT-DEPT-CODE.
            START DEPART KEY >= DPT-CODE.

       READ-DEPART-STATUS.
           IF WS-STATUS = "00"
               MOVE ZERO             TO WS-F-ERROR
               GO TO READ-DEPART-EXIT.
           IF WS-STATUS = "23"
               MOVE 7                TO WS-F-ERROR
               GO TO READ-DEPART-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 7                TO WS-F-ERROR
               PERFORM READ-ERROR.
      
       READ-DEPART-EXIT.
             EXIT.
      *
      *     ****   ***  ****   ***  *   *    ***** *** *     *****
      *     *   * *   * *   * *   * ** **    *      *  *     *
      *     *   * *   * *   * *   * * * *    *      *  *     *
      *     ****  ***** ****  ***** *   *    ***    *  *     ***
      *     *     *   * *   * *   * *   *    *      *  *     *
      *     *     *   * *   * *   * *   *    *      *  *     *
      *     *     *   * *   * *   * *   *    *     *** ***** *****
      *
       READ-PARAM.
             READ PARAM.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
               GO TO READ-PARAM-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 15           TO WS-F-ERROR
               PERFORM READ-ERROR.
           IF RECORD-LOCKED
               PERFORM LOCKED-RECORD
               GO TO READ-PARAM.
             MOVE 15             TO WS-F-ERROR.
             PERFORM READ-ERROR.

       READ-PARAM-EXIT.
             EXIT.
      *
      *     ***   ***  ****  ****  ***** ****      ***** *** *     *****
      *    *   * *   * *   * *   * *     *   *     *      *  *     *
      *    *     *   * *   * *   * *     *   *     *      *  *     *
      *     ***  *   * ****  *   * ***   ****      ***    *  *     ***
      *        * *   * *   * *   * *     *   *     *      *  *     *
      *    *   * *   * *   * *   * *     *   *     *      *  *     *
      *     ***   ***  *   * ****  ***** *   *     *     *** ***** *****
      *
       READ-SORDER-NEXT.
             READ SORDER NEXT.
           IF RECORD-LOCKED
               PERFORM LOCKED-RECORD
               GO TO READ-SORDER-NEXT.
             GO TO READ-SORDER-STATUS.

       START-AT-ORDER-NO.
             START SORDER KEY >= SOR-KEY.

       READ-SORDER-STATUS.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
               GO TO READ-SORDER-EXIT.
           IF WS-STATUS = "23"
               MOVE 20           TO WS-F-ERROR
               GO TO READ-SORDER-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 20           TO WS-F-ERROR
               PERFORM READ-ERROR.

       READ-SORDER-EXIT.
             EXIT.
      *
      *     ***  *****  ***   ***  *   *     ***** ***** *     *****
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *    *       *   *   * *     * *       *       *   *     *
      *     ***    *   *   * *     **        ***     *   *     ***
      *        *   *   *   * *     * *       *       *   *     *
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *     ***    *    ***   ***  *   *     *     ***** ***** *****
      *
       READ-STOCK.
             READ STOCK WITH IGNORE LOCK KEY IS STK-CODE.
           IF WS-STATUS = "00"
               IF STK-DISC NOT = 9
                   MOVE ZERO         TO WS-F-ERROR
                   GO TO READ-STOCK-EXIT
               ELSE
                   MOVE 22           TO WS-F-ERROR
                   GO TO READ-STOCK-EXIT.
             GO TO STOCK-CHECK-STATUS.

       READ-STOCK-NEXT.
             READ STOCK NEXT WITH IGNORE LOCK.
           IF WS-STATUS = "00"
               IF STK-DISC NOT = 9
                   MOVE ZERO         TO WS-F-ERROR
                   GO TO READ-STOCK-EXIT
               ELSE
                   GO TO READ-STOCK-NEXT.
             GO TO STOCK-CHECK-STATUS.

       START-AT-STOCK-CODE.
             START STOCK KEY >= STK-CODE.
             GO TO STOCK-CHECK-STATUS.

       STOCK-CHECK-STATUS.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
               GO TO READ-STOCK-EXIT.
           IF (WS-STATUS = "23") OR (WS-STAT1 = "1")
               MOVE 22           TO WS-F-ERROR
               GO TO READ-STOCK-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 22           TO WS-F-ERROR
               PERFORM READ-ERROR.

       READ-STOCK-EXIT.
             EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AO00-WRITE        SECTION.

       COPY "CONTROL.WR".

       COPY "DEPART.WR".

       COPY "DEBTOR.WR".

       COPY "PARAM.WR".

       COPY "SORDER.WR".

       COPY "STOCK.WR".

      /
      *    ****    MONTH-END CLEAR (STATS)    ****
      *
       DA00M                  SECTION 5.
       DA00.
             MOVE "MONTH-END CLEAR - STATISTICS" TO CRT-HEADING.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
             DISPLAY S05B.
             MOVE 7                  TO SHADE-ROW.
             MOVE 29                 TO SHADE-COL.
             MOVE 22                 TO SHADE-WIDTH.
             MOVE 11                 TO SHADE-LINES.
             DISPLAY S05C.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
             PERFORM SAVE-SCREEN-3.

       DA00A.
             PERFORM RESTORE-SCREEN-3.
             MOVE 7                  TO TOP-ROW.
             MOVE 15                 TO BOTTOM-ROW.
             MOVE 7                  TO SCREEN-LIN.
             MOVE 30                 TO SCREEN-COL.
             MOVE 19                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       DA00A-5.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       DA00A-10.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY      PERFORM MOVE-BLOCK-UP
                 WHEN DOWN-KEY    PERFORM MOVE-BLOCK-DOWN
                 WHEN MOUSE-KEY   PERFORM POSITION-MOUSE
                                  IF MOUSE-ENTER = "Y"
                                      MOVE "N"   TO MOUSE-ENTER
                                      GO TO DA00A-15
                                  END-IF
                                  IF MOUSE-Y = 49
                                      IF MOUSE-X = 5
                                          MOVE 5 TO KEY-CODE-1
                                          GO TO DA00A-10
                                      ELSE
                                      IF MOUSE-X = 10
                                          MOVE 6 TO KEY-CODE-1
                                          GO TO DA00A-10
                                      ELSE
                                      IF MOUSE-X > 36 AND < 44
                                          MOVE 0 TO KEY-CODE-1
                                          GO TO DA00A-10
                                      ELSE
                                      IF MOUSE-X > 54 AND < 60
                                          MOVE 0  TO KEY-CODE-1
                                          MOVE 1  TO KEY-TYPE
                                          GO TO DA00A-10
                                      END-IF
                                  END-IF
                 WHEN ENTER-KEY   GO TO DA00A-15
                 WHEN OTHER       PERFORM AA900-ALARM
               END-EVALUATE
               GO TO DA00A-5
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  MOVE "0"  TO WS-OPTION
                     GO TO DA00AF
                 WHEN OTHER  PERFORM AA900-ALARM
               END-EVALUATE
               GO TO DA00A-5
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X      TO WS-OPTION
               CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS
               IF WS-OPTION < "0" OR > "8"
                   PERFORM AA900-ALARM
                   GO TO DA00A-5
               END-IF
               GO TO DA00A-16
           ELSE
               PERFORM AA900-ALARM
               GO TO DA00A-5.

       DA00A-15.
             EVALUATE SCREEN-LIN
               WHEN 7      MOVE "1"  TO WS-OPTION
               WHEN 8      MOVE "2"  TO WS-OPTION
               WHEN 9      MOVE "3"  TO WS-OPTION
               WHEN 10     MOVE "4"  TO WS-OPTION
               WHEN 11     MOVE "5"  TO WS-OPTION
               WHEN 12     MOVE "6"  TO WS-OPTION
               WHEN 13     MOVE "7"  TO WS-OPTION
               WHEN 14     MOVE "8"  TO WS-OPTION
               WHEN 15     MOVE "0"  TO WS-OPTION
               WHEN OTHER  GO TO DA00A-5
             END-EVALUATE.
             GO TO DA00AF.

       DA00A-16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "0"    MOVE 15   TO SCREEN-LIN
               WHEN "1"    MOVE 7    TO SCREEN-LIN
               WHEN "2"    MOVE 8    TO SCREEN-LIN
               WHEN "3"    MOVE 9    TO SCREEN-LIN
               WHEN "4"    MOVE 10   TO SCREEN-LIN
               WHEN "5"    MOVE 11   TO SCREEN-LIN
               WHEN "6"    MOVE 12   TO SCREEN-LIN
               WHEN "7"    MOVE 13   TO SCREEN-LIN
               WHEN "8"    MOVE 14   TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       DA00AF.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66               TO MOUSE-FUNC
               MOVE 0                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
               MOVE WS-OPTION        TO WS-REPLY.
           IF WS-REPLY = "0"
               GO TO DA999.
           IF WS-REPLY = "6"
               GO TO DA40.
           IF WS-REPLY = "7"
               GO TO DA65.
           IF WS-REPLY = "8"
               GO TO DA75.
           IF NOT(WS-REPLY = "1" OR "2" OR "3" OR "4" OR "5")
               GO TO DA00A.
           IF NOT(WS-REPLY = "1" OR "5")
               GO TO DA01.
      *
      *   As this version of the system includes all modules the
      *   following tests are no longer required. 
      *   These will be removed with the next cleanup of code.
      *
      *    IF LS0-DTP NOT = 2
      *        MOVE "Not Installed"  TO WS-ERR-MES
      *        PERFORM ERROR-MESSAGE
      *        GO TO DA01.
             MOVE 11                 TO WS-PARKEY.
             DISPLAY "Clearing DEBTOR transaction statistics" AT 2206 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Param record :" AT 2006.

       DA00B.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE WS-PARKEY          TO W40-KEY.
             DISPLAY W40-KEY AT 2022 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE 1                  TO WS-S1.
       DA00C.
             MOVE ZERO               TO PAR-T-MTD(WS-S1) PAR-T-MTDV(WS-S1) PAR-T-VAT(WS-S1).
           IF WS-S1 < 2
               ADD 1                 TO WS-S1
               GO TO DA00C.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
           IF WS-PARKEY < 60
               ADD 1                 TO WS-PARKEY
               GO TO DA00B.
       DA01.
           IF NOT(WS-REPLY = "2" OR "5")
               GO TO DA02.
      *
      *   As this version of the system includes all modules the
      *   following tests are no longer required. 
      *   These will be removed with the next cleanup of code.
      *
      *    IF LS0-CRP NOT = 2
      *        MOVE "Not Installed"  TO WS-ERR-MES
      *        PERFORM ERROR-MESSAGE
      *        GO TO DA02.
             MOVE 191                TO WS-PARKEY.
             PERFORM DA62-CLEAR-SCREEN.
             DISPLAY "Clearing CREDITOR transaction statistics" AT 2206 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Param record :" AT 2006.

       DA01A.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE WS-PARKEY          TO W40-KEY.
             DISPLAY W40-KEY AT 2022 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE 1                  TO WS-S1.

       DA01B.
             MOVE ZERO               TO PAR-C-MTD(WS-S1) PAR-C-MTDV(WS-S1).
           IF WS-S1 < 2
               ADD 1                 TO WS-S1
               GO TO DA01B.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
           IF WS-PARKEY < 200
               ADD 1                 TO WS-PARKEY
               GO TO DA01A.
             MOVE 6                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE ZERO               TO PAR-RAILD PAR-RAILM PAR-DISCD PAR-DISCM.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             DISPLAY "Cleared" AT 0856 WITH FOREGROUND-COLOR Brown HIGHLIGHT.

       DA02.
           IF NOT(WS-REPLY = "1" OR "5")
               MOVE "  !"            TO DPT-CODE
               GO TO DA08.
             PERFORM DA62-CLEAR-SCREEN.
             DISPLAY "Clearing sales and cost of sales" AT 2206 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "PARAM record :" AT 2006.
             MOVE 7                  TO WS-PARKEY.

       DA04.
             MOVE WS-PARKEY          TO W40-KEY.
             DISPLAY W40-KEY AT 2022 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.

       DA04A.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE ZERO               TO PAR-CASH-MTH PAR-CASH-RMTH.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             MOVE 8                  TO WS-PARKEY.

       DA05.
             PERFORM DA04.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE ZERO               TO PAR-CMTD PAR-ADJMTD.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             ADD 1                   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE ZERO               TO PAR-DVAT-MTD PAR-CVAT-MTD PAR-DGST-MTD PAR-CGST-MTD PAR-DNGS-MTD PAR-CNGS-MTD.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             DISPLAY "Sales record :" AT 2006.
             MOVE "   !"             TO DPT-CODE.

       DA08.
             PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.
           IF WS-F-ERROR = 7
               GO TO DA12.

       DA10.
             PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
           IF WS-F-ERROR = 7
               GO TO DA12.
             DISPLAY DPT-CODE AT 2022 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
           IF WS-REPLY = "1" OR "5"
               IF DPT-SCDE = "VAT"
                   MOVE ZERO         TO DPT-V-SALES-MTD DPT-V-RET-MTD DPT-O-VAT-MTD DPT-O-VRET-MTD
               ELSE
                   MOVE ZERO         TO DPT-SALES-MTD DPT-RET-MTD DPT-DISC-MTD DPT-ADJ-MTD DPT-COST-MTD DPT-CRET-MTD
               END-IF
           IF WS-REPLY = "2" OR "5"
               IF DPT-SCDE = "VAT"
                   MOVE ZERO         TO DPT-V-PUR-MTD DPT-V-PRET-MTD DPT-I-VAT-MTD DPT-I-VRET-MTD
               ELSE
                   MOVE ZERO         TO DPT-PUR-MTD DPT-PRET-MTD DPT-TRF-MTD
               END-IF
             PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
             GO TO DA10.

       DA12.
           IF NOT(WS-REPLY = "3" OR "5")
               GO TO DA16.
      *
      *   As this version of the system includes all modules the
      *   following tests are no longer required. 
      *   These will be removed with the next cleanup of code.
      *
      *    IF LS0-STP NOT = 2
      *        MOVE "Not Installed"  TO WS-ERR-MES
      *        PERFORM ERROR-MESSAGE
      *        GO TO DA16.
           IF WS-OPTION NOT = "Y"
               GO TO DA15A.
             MOVE "!"                TO STK-CODE.
             PERFORM START-AT-STOCK-CODE THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO DA15A.
             PERFORM DA62-CLEAR-SCREEN.
             DISPLAY "Clearing STOCK sales statistics" AT 2206 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Stock record :" AT 2006.

       DA15.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO DA15A.
             DISPLAY STK-CODE AT 2022 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE ZERO               TO STK-YTD STK-YTDC STK-YTDV.
             PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
             GO TO DA15.
       DA15A.
             MOVE 3                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE ZERO               TO STK-CMTD STK-ADJMTD STK-SMTD STK-REDMTD.
             PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
             DISPLAY "Cleared" AT 0956 WITH FOREGROUND-COLOR Brown HIGHLIGHT.

       DA16.
           IF NOT(WS-REPLY = "1" OR "5")
               GO TO DA30.
             MOVE 61                 TO WS-PARKEY.
             PERFORM DA62-CLEAR-SCREEN.
             DISPLAY "Clearing DEBTOR category statistics" AT 2206 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Param record :" AT 2006.

       DA18.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             PERFORM DA04.
             MOVE 1                  TO WS-S1.

       DA20.
             MOVE ZERO               TO PAR-CDRMTD(WS-S1) PAR-CCRMTD(WS-S1).
           IF WS-S1 < 2
               ADD 1                 TO WS-S1
               GO TO DA20.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
           IF WS-PARKEY < 100
               ADD 1                 TO WS-PARKEY
               GO TO DA18.
             DISPLAY "Cleared" AT 0756 WITH FOREGROUND-COLOR Brown HIGHLIGHT.

       DA30.
           IF NOT(WS-REPLY = "4" OR "5")
               GO TO DA38.
             MOVE 101                TO WS-PARKEY.
             PERFORM DA62-CLEAR-SCREEN.
             DISPLAY "Clearing Salesman statistics" AT 2206 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Param record :" AT 2006.

       DA35.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             PERFORM DA04.
             MOVE ZERO               TO PAR-SL-MTD PAR-CT-MTD PAR-VC-MTD.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
           IF WS-PARKEY < 150
               ADD 1                 TO WS-PARKEY
               GO TO DA35.
             DISPLAY "Cleared" AT 1056 WITH FOREGROUND-COLOR Brown HIGHLIGHT.

       DA38.
             MOVE 101                TO WS-PARKEY.
             PERFORM DA62-CLEAR-SCREEN.
             MOVE "P R E S S"        TO WS-DSP-MES.
             PERFORM DISPLAY-MESSAGE.
             GO TO DA00A.
      *
      *    ****    R E D U N D A N T   Q U O T E S
      *
       DA40.
             MOVE "CLEAR REDUNDANT QUOTATIONS" TO CRT-HEADING.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
             DISPLAY "This routine will clear redundant quotations" AT 1008.
             DISPLAY "Are you sure Y/N?"                            AT 1208 WITH FOREGROUND-COLOR Brown HIGHLIGHT.

       DA45.
             ACCEPT WS-OPTION AT 1226 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-OPTION = "Y" OR "N")
               GO TO DA45.
           IF WS-OPTION = "N"
               GO TO DA00.
             MOVE "Q"                TO WS-TYPE.
             DISPLAY "Enter Date from which QUOTATIONS are to be kept DD/MM/YYYY" AT 1408.

       DA50.
             DISPLAY "Order No:" AT 1608.
             MOVE 1467               TO DATEPOS.
             PERFORM DATE-CHECK-ACCEPT-PLUS THRU CHECK-DATE-EXIT.
      *      CALL "UTP\DateCheck" USING W-DATES.
           IF W22-INSTR = X"EC"
               GO TO DA00.
             MOVE W20-DTE            TO W15-E-YMD.
             MOVE "00000001"         TO SOR-REF
             MOVE ZERO               TO SOR-SEQ
             PERFORM START-AT-ORDER-NO THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
               GO TO DA00.
       DA55.
             PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
               GO TO DA00.
       DA58.
           IF NOT(SOR-TYPE = ZERO)
               GO TO DA55.
             DISPLAY SOR-REF AT 1618 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
           IF SOR-DOC = WS-TYPE
               IF SOR-DTE < W15-E-YMD
                   MOVE SOR-REF  TO WS-REF
                   GO TO DA60.
             GO TO DA55.

       DA60.
             PERFORM DELETE-SORDER-REC THRU WRITE-SORDER-EXIT.
             PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
               GO TO DA00.
           IF SOR-REF NOT = WS-REF
               GO TO DA58.
             GO TO DA60.

       DA62-CLEAR-SCREEN.
             MOVE 20                 TO SRT-START.
             MOVE 25                 TO CRT-END.
             PERFORM ERASE-SCREEN.

       DA62-CLEAR-SCREEN-EXIT.
             EXIT.
      *
      *    ****    R E D U N D A N T   O R D E R S
      *
       DA65.
             MOVE "CLEAR REDUNDANT SALES ORDERS" TO CRT-HEADING.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
             DISPLAY "This routine will clear redundant sales orders" AT 1008.
             DISPLAY "Are you sure Y/N?"                              AT 1208 WITH FOREGROUND-COLOR Brown HIGHLIGHT.

       DA70.
             ACCEPT WS-OPTION AT 1226 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-OPTION = "Y" OR "N")
               GO TO DA70.
           IF WS-OPTION = "N"
               GO TO DA00.
             MOVE "O"                TO WS-TYPE.
             DISPLAY "Enter Date from which SLS-ORDERS are to be kept DD/MM/YYYY" AT 1408.
             GO TO DA50.
      *
      *    ****    C O N V E R T E D   O R D E R S
      *
       DA75.
             MOVE "CLEAR CONVERTED ORDERS" TO CRT-HEADING.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
             DISPLAY "This routine will clear converted orders" AT 1008.
             DISPLAY "Are you sure Y/N?"                        AT 1208 WITH FOREGROUND-COLOR Brown HIGHLIGHT.

       DA80.
             ACCEPT WS-OPTION AT 0626 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-OPTION = "Y" OR "N")
               GO TO DA80.
           IF WS-OPTION = "N"
               GO TO DA00.
             MOVE "I"                TO WS-TYPE.
             DISPLAY "Enter Date from which CNV-ORDERS are to be kept DD/MM/YYYY" AT 1408.
             GO TO DA50.

       DA999.
             EXIT.

      *    *************************************************************
      *                 I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT       SECTION 8.
       ZA00-OPEN.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-L-OR-N          TO W02-L-OR-N.
             MOVE WS-SYS-ID          TO W02-SYSID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W12-TODAY.
             MOVE LS-TODAY-YYMMDD    TO W12-T-YMD.
             MOVE LS-USUB            TO WS-USUB.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

       ZA00A-CONTINUE.
      *
      *     ****   ***  ****   ***  *   *    ***** *** *     *****
      *     *   * *   * *   * *   * ** **    *      *  *     *
      *     *   * *   * *   * *   * * * *    *      *  *     *
      *     ****  ***** ****  ***** *   *    ***    *  *     ***
      *     *     *   * *   * *   * *   *    *      *  *     *
      *     *     *   * *   * *   * *   *    *      *  *     *
      *     *     *   * *   * *   * *   *    *     *** ***** *****
      *
             OPEN I-O PARAM LOCK.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS NOT = "00"
               MOVE 15               TO WS-F-ERROR
               PERFORM OPEN-ERROR.
      *
      *          ***   ***  *   * ***** ****   ***  *             *
      *         *   * *   * *   *   *   *   * *   * *            *
      *         *     *   * **  *   *   *   * *   * *           *
      *         *     *   * * * *   *   ****  *   * *          *
      *         *     *   * *  **   *   *   * *   * *         *
      *         *   * *   * *   *   *   *   * *   * *        *
      *          ***   ***  *   *   *   *   *  ***  *****   *
      *
      *  *   * ***** ***** *   *  ***  ****  *   *   ***** *** *     *****
      *  *   * *       *   *   * *   * *   * *  *    *      *  *     *
      *  **  * *       *   *   * *   * *   * * *     *      *  *     *
      *  * * * ***     *   * * * *   * ****  **      ***    *  *     ***
      *  *  ** *       *   * * * *   * *   * * *     *      *  *     *
      *  *   * *       *   * * * *   * *   * *  *    *      *  *     *
      *  *   * *****   *    * *   ***  *   * *   *   *     *** ***** *****
      *
             OPEN I-O NETWORK.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS NOT = "00"
               MOVE 2                TO WS-F-ERROR
               PERFORM OPEN-ERROR.
             DISPLAY CLEAR-L50.
      *
      *     ***  *****  ***   ***  *   *     ***** ***** *     *****
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *    *       *   *   * *     * *       *       *   *     *
      *     ***    *   *   * *     **        ***     *   *     ***
      *        *   *   *   * *     * *       *       *   *     *
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *     ***    *    ***   ***  *   *     *     ***** ***** *****
      *
             OPEN I-O STOCK.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 22           TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
               MOVE 22               TO WS-F-ERROR
               PERFORM OPEN-ERROR.
       ZA05.
      *
      *    ****  ***** ****    *   ****  *****    ***** *** *     *****
      *    *   * *     *   *  * *  *   *   *      *      *  *     *
      *    *   * *     *   * *   * *   *   *      *      *  *     *
      *    *   * ***   ****  ***** ****    *      ***    *  *     ***
      *    *   * *     *   * *   * *  *    *      *      *  *     *
      *    *   * *     *   * *   * *   *   *      *      *  *     *
      *    ****  ***** *   * *   * *   *   *      *     *** ***** *****
      *
             OPEN I-O DEPART.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS NOT = "00"
               MOVE 7                TO WS-F-ERROR
               PERFORM OPEN-ERROR.
      *
      *     ***   ***  ****  ****  ***** ****      ***** ***** *     *****
      *    *   * *   * *   * *   * *     *   *     *       *   *     *
      *    *     *   * *   * *   * *     *   *     *       *   *     *
      *     ***  *   * ****  *   * ***   ****      ***     *   *     ***
      *        * *   * *   * *   * *     *  *      *       *   *     *
      *    *   * *   * *   * *   * *     *   *     *       *   *     *
      *     ***   ***  *   * ****  ***** *   *     *     ***** ***** *****
      *
             OPEN I-O SORDER.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 20           TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
               MOVE 20               TO WS-F-ERROR
               PERFORM OPEN-ERROR.

       ZA10.
             MOVE 1                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      *      MOVE PAR-DMY            TO W12-TODAY.
      *      MOVE PAR-YMD            TO W12-T-YMD.
             MOVE PAR-COMPANY        TO CRT-COMPANY.
      *
      *   As this version of the system includes all modules the
      *   following tests are no longer required. 
      *   These will be removed with the next cleanup of code.
      *
      *    IF LS0-DTP NOT = 2
      *        GO TO ZA999.
      *    IF LS0-STP NOT = 2
      *        GO TO ZA999.
      *      GO TO ZA999.

       COPY "ZA49.PRO".

       ZA50.
             EVALUATE WS-F-ERROR
               WHEN 20    MOVE "SALES ORDER"    TO WS-FILE
               WHEN 22    MOVE "STOCK FILE"     TO WS-FILE
               WHEN OTHER MOVE "INVALID FILE REF" TO WS-FILE
             END-EVALUATE.
             MOVE SPACES             TO WS-ERR-MES.
             STRING "Rebuild " DELIMITED SIZE WS-FILE INTO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.

       ZA51.
             EXIT PROGRAM.

       ZA200.
             MOVE 12                 TO SHADE-ROW.
             MOVE 8                  TO SHADE-COL.
             MOVE 66                 TO SHADE-WIDTH.
             MOVE 5                  TO SHADE-LINES.
             DISPLAY SD-LOCKED.
             PERFORM SCREEN-SHADOW.
             MOVE 19                 TO SHADE-ROW.
             MOVE 32                 TO SHADE-COL.
             MOVE 15                 TO SHADE-WIDTH.
             MOVE 3                  TO SHADE-LINES.
             PERFORM SCREEN-SHADOW.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       ZA205.
             EXIT PROGRAM.

       ZA999.
             EXIT.

      *
      *    ****    I / O   E R R O R   M E S S A G E S
      *
       ZB00-ERROR      SECTION 8.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 2      MOVE W02-NETWORK  TO WS-FILE
                           MOVE WS-NETKEY    TO WS-KEY
               WHEN 7      MOVE W02-DEPART   TO WS-FILE
                           MOVE DPT-CODE     TO WS-KEYX
               WHEN 15     MOVE WS-PARID     TO WS-FILE
                           MOVE WS-PARKEY    TO WS-KEY
               WHEN 20     MOVE W02-SORDER   TO WS-FILE
                           MOVE SOR-KEY      TO WS-KEYX
               WHEN 22     MOVE W02-STOCKF   TO WS-FILE
                           MOVE STK-CODE     TO WS-KEYX
               WHEN OTHER  MOVE "ERROR"      TO WS-FILE
                           MOVE "REP002"     TO WS-KEYX
             END EVALUATE.

       COPY "DISPERR.PRO".
