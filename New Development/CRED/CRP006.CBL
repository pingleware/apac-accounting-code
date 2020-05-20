      $set LINKCOUNT"512" GNT"CRP006.GNT"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******     ****      ****     ******    *
      *   **    **  **    **  **    **   **  **    **  **   **    **   *
      *   **        **    **  **    **  **    **  **    **  **         *
      *   **        ******    *******   **    **  **    **  *******    *
      *   **        **   **   **        **    **  **    **  **    **   *
      *   **    **  **    **  **         **  **    **  **   **    **   *
      *    ******   **    **  **          ****      ****     ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R   M A I N T E N A N C E   P R O G R A M    *
      *                                                                *
      *     A G E   A C C O U N T S                                    *
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
       PROGRAM-ID.        CRP006.
       AUTHOR.            J.W.LEMMON (CSS).
       DATE-WRITTEN.      JUNE, 1983.


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

       COPY "CRDMEM.SL".

       COPY "CREDIT.SL".

       COPY "CRTRAN.SL".

           SELECT CRTOLD  ASSIGN DISK
                          LOCK EXCLUSIVE
                          STATUS WS-STATUS
                          ACCESS SEQUENTIAL
                          ORGANIZATION SEQUENTIAL.

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

       COPY "CRDMEM.FDE".

       COPY "CREDIT.FDE".

       COPY "CRTRAN.FDE".

       COPY "CRTOLD.FD".

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
       77  WS-PARKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-CTOKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-CTRKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-CREKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-NETKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-RECKEY               PIC  9(04)    COMP-5.
       77  WS-RECOVER              PIC  9(04)    COMP-5.
       77  WS-SHARED               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-TRANS                PIC  9(04)    COMP-5 VALUE 1.
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
       77  WS-S8                   PIC  9(04)    COMP-5.
       77  WS-IXS1                 PIC  9(04)    COMP-5.
       77  WS-IXS2                 PIC  9(04)    COMP-5.
       77  WS-IXS3                 PIC  9(04)    COMP-5.
       77  WS-IXS4                 PIC  9(04)    COMP-5.
       77  WS-ENT                  PIC  X(08).
       77  WS-ENT1                 PIC  X(08).
       77  WS-KEYSTR               PIC  9(04)    COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-REST                 PIC  X(01).
       77  WS-NUM                  PIC  Z9.
       77  WS-KEY1                 PIC  9(04)    COMP-5.
       77  WS-CONREC               PIC  9(01)           VALUE ZERO.
       77  WS-INDS                 PIC  9(01)           VALUE 0.
       77  WS-INDC                 PIC  9(01)           VALUE 0.
       77  WS-SKIP                 PIC  X(01)           VALUE " ".
       77  WS-CHAR1                PIC  X(01).
       77  WS-LIN                  PIC  9(02)           VALUE ZERO.
       77  WS-LINES                PIC  9(04)    COMP-5 VALUE 66.
       77  WS-PAGE                 PIC  9(04)    COMP-5.
       77  WS-SEASON               PIC  X(01).
       77  PRG-NAME                PIC  X(12)           VALUE SPACES.
       77  PRG-CRED-LUP            PIC  X(12)           VALUE "CRP\CRPLOOK".
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
      *        C =  Clear comment line (50)
      *        E =  Clear lines (any line or lines between 5 and 46)
      *        H =  Change heading
      *        S =  Clear the screen
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "CRP006".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "CREDITOR - AGE ACCOUNTS".
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
           03  WS-MODULE           PIC  X(03) VALUE "CRP".
           03  WS-PROG             PIC  X(03) VALUE "006".

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

       COPY "W05.WS".

       COPY "W15.CRP".

       01  W20-CALCS.
           03  W20-RESULT          PIC  9(06)V99.
           03  W20-RESULT-1                             REDEFINES W20-RESULT.
               05  W20-WHOLE       PIC  9(06).
               05  W20-DECIMAL     PIC  9(02).
           03  W20-ORD             PIC  9(07)    COMP-3.
           03  W20-DATE.
               05  W20-CC          PIC  99.
               05  W20-YY          PIC  99.
               05  W20-MM          PIC  99.
               05  W20-DD          PIC  99.
           03  W20-DTE             PIC  9(08)           REDEFINES W20-DATE.

      *01  W40-NADD.
      *    03  W40-ADD.
      *        05  W40-ACHAR PIC  X(01) OCCURS 70.
      *    03  W40-ADDR REDEFINES W40-ADD.
      *        05  W40-ADDRS PIC  X(60).
      *        05  FILLER    PIC  X(10).
      *    03  W40-NAME.
      *        05  W40-NCHAR PIC  X(01) OCCURS 40.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       COPY "W70CRED.WS".

      *01  W95-COMPANY.
      *    03  W95-COMP      PIC  X(40).

      *
      *    **        ********  **    **  **    **    ****     ******   ********
      *    **           **     ***   **  **   **    **  **   **    **  **
      *    **           **     ****  **  **  **    **    **  **        ** 
      *    **           **     ** ** **  *****     ********  **        *****
      *    **           **     **  ****  **  **    **    **  **   ***  **  
      *    **           **     **   ***  **   **   **    **  **    **  **
      *    ********  ********  **    **  **    **  **    **   ******   ********
      *
       LINKAGE SECTION.

       COPY "CHAIN.LS".

       COPY "FILE.IDS".

      * 
      *   ******    ******   *******   ********  ********  **     **
      *  **    **  **    **  **    **  **        **        ***    **
      *  **        **        **   **   **        **        ** *   **
      *   ******   **        ******    *****     *****     **  *  **
      *        **  **        **   **   **        **        **   * **
      *  **    **  **    **  **    **  **        **        **    ***
      *   ******    ******   **    **  ********  ********  **     **
      *
       SCREEN SECTION.

       01  S39.
           03  BACKGROUND-COLOR Red  FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE  4 COLUMN 10 VALUE " This routine does not include a recovery procedure. Security ".
               05  LINE  5 COLUMN 10 VALUE " copies  of the data files should be made as a power  failure ".
               05  LINE  6 COLUMN 10 VALUE " or any other problem during this procedure would require the ".
               05  LINE  7 COLUMN 10 VALUE " DATA FILES from the previous backup to be restored !!!       ".
           03  BACKGROUND-COLOR Blue FOREGROUND-COLOR Cyan.
               05  LINE  9 COLUMN 12 VALUE " Do you wish to continue (Y/N)? ".

       01  S55.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
               05  LINE 50 COLUMN  2 VALUE "F1"                                           FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN  4 VALUE   "=Help,".
               05          COLUMN 10 VALUE         "F2"                                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 12 VALUE           " to do Creditor Lookup ".
               05          COLUMN 35 VALUE                                  "Esc"         FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 38 VALUE                                     " to exit".

       01  S56.
           05  LINE  8 COLUMN 12 VALUE "Transaction file will now be rebuilt"
           05  LINE 10 COLUMN 12 VALUE "NB"                                           FOREGROUND-COLOR Brown HIGHLIGHT
           05          COLUMN 14 VALUE   " - ALL other workstations must EXIT from ".
           05  LINE 12 COLUMN 12 VALUE "CREDITORS before continuing ".
           05          COLUMN 40 VALUE                             "C"                FOREGROUND-COLOR Grey HIGHLIGHT
           05          COLUMN 41 VALUE                              "ontinue [ ]".

      *COPY "CRDLUP.CRT".

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
       AA000-START-UP  SECTION.
       AA000.
          IF LS0-CRLEV < 3
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
        GO TO AA49.
            PERFORM ZA000-INIT.
            PERFORM HA000.
            CLOSE RECOVER.

       AA49.
             EXIT PROGRAM.

       COPY "AA50C.LUP".
       
       COPY "AA900.ALM".

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

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                        OPT-MESSAGE                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine is used to allow the OPERATOR to respond to  ³
      *    ³ a request for an option, so that the correct action can   ³
      *    ³ be performed by the program. The routine will display the ³
      *    ³ message in a pop-up window and allow the OPERATOR to      ³
      *    ³ respond to the 'question'.                                ³
      *    ³                                                           ³
      *    ³ The option request must be placed in WS-ERR-MES and may   ³
      *    ³ not exceed 48 characters. The message will be centred in  ³
      *    ³ the window. An example of a message request follows:      ³
      *    ³                                                           ³
      *    ³   MOVE "Print transactions (Y/N) [ ]" TO WS-ERR-MES.      ³
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

       COPY "APAC.HLP".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AB00-READ      SECTION.

       COPY "CONTROL.RD".

       COPY "CRDMEM.RD".

       COPY "CREDIT.RD".

       COPY "CRTRAN.RD".

       COPY "PARAM.RD".

       COPY "SHARED.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AC00-WRITE    SECTION.

       COPY "CONTROL.WR".

       COPY "CREDIT.WR".

       COPY "CRTRAN.WR".

       COPY "PARAM.WR".

      *COPY "CREDIT.LUP".

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
       AY000        SECTION.
      *    *************************************************************
      *    ****             P A R A M E T E R   F I L E             ****
      *    *************************************************************
      *AY01.
      *      MOVE 01                 TO REC-FILE.
      *      MOVE WS-PARKEY          TO REC-KEY.
      *      MOVE ZERO               TO REC-TYPE
      *      PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      *      MOVE PAR-RECORD1        TO REC-PARAM.
      *      GO TO AY50.
      *    *************************************************************
      *    ****              C R E D I T O R   F I L E              ****
      *    *************************************************************
      *AY07.
      *      MOVE 07                 TO REC-FILE.
      *      MOVE ZERO               TO REC-KEY.
      *      MOVE WS-ACTION          TO REC-TYPE
      *    IF WS-ACTION = 0 OR 2
      *        PERFORM READ-CREDIT-LOCK THRU READ-CREDIT-EXIT.
      *      MOVE P-REC              TO REC-CREDIT.
      *      GO TO AY50.
      *    *************************************************************
      *    ****  C R E D I T O R   T R A N S A C T I O N   F I L E  ****
      *    *************************************************************
      *AY08.
      *     MOVE 08                  TO REC-FILE.
      *     MOVE WS-CTRKEY           TO REC-KEY.
      *     MOVE CT-REC              TO REC-CRTRAN.
      *      GO TO AY50.
      *    *************************************************************
      *    ****               C O N T R O L   F I L E               ****
      *    *************************************************************
       AY39.
             MOVE 39                 TO REC-FILE.
             MOVE WS-NETKEY          TO REC-KEY.
             MOVE ZERO               TO REC-TYPE.
             MOVE NET-RECORD         TO REC-NETWORK.
             GO TO AY50.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³              End transaction in recovery log              ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY40.
             MOVE 99                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE SPACES             TO REC-DETAIL.
             PERFORM AY50.
             ADD 1                   TO WS-TRANS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If recovery records exceed 195 - Clear (Close and Open    ³
      *    ³ new). This allows for quicker recoveries when required.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF WS-RECOVER > 195
               CLOSE RECOVER
               OPEN OUTPUT RECOVER
               CLOSE RECOVER
               OPEN I-O RECOVER
               MOVE ZERO             TO WS-RECOVER.
             GO TO AY59.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³         W R I T E   R E C O V E R Y   R E C O R D         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY50.
             ADD 1                   TO WS-RECOVER.
             MOVE WS-RECOVER         TO WS-RECKEY.
             MOVE WS-TRANS           TO REC-TRAN.
             WRITE REC-RECORD.
           IF WS-STATUS NOT = "00"
               DISPLAY "Write error Recovery file - Status " AT 2212 WITH FOREGROUND-COLOR Brown HIGHLIGHT
               DISPLAY WS-STATUS                             AT 2247 WITH FOREGROUND-COLOR Grey HIGHLIGHT
               STOP RUN.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
       AY59.
             EXIT.
      *
      *    ****    Start processing transaction
      *
       AY60.
             MOVE 1                  TO WS-COUNT.
             MOVE 5                  TO WS-SHARED.
             PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      *      MOVE SHR-STOCK          TO WS-STOCK.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-USERS < 24
               MOVE 1                TO WS-SUB
               MOVE ZERO             TO WS-WAIT
               GO TO AY62.
      *
      *    ****   Q   F U L L  -  W A I T   F O R   4   S E C O N D S
      *
             DISPLAY "WAITING" AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red HIGHLIGHT.
             COMMIT.
             ACCEPT WS-STIME FROM TIME.
             MOVE 400                TO WS-WAIT.
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
      *            and lock it. Log details to the RECOVERY file.
      *
             MOVE 2                  TO WS-NETKEY.
             PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             PERFORM AY39 THRU AY59.
             GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N   P R O G R E S S
      *
       AY62.
           IF NOT(PAR-PROG(WS-SUB) = SPACES OR PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   C R E D I T O R   F I L E S   B E I N G   U P D A T E D
      *
               IF NOT(PAR-PROG(WS-SUB) = SPACES)
                   IF PAR-PROG(WS-SUB) = "CR" OR "CS" OR "CG"
      *
      *    ****   Y E S   -   S E T   W A I T   P E R I O D
      *
                       GO TO AY62-WAIT
                   ELSE
                       ADD 1         TO WS-SUB
                       GO TO AY62
                   END-IF
               ELSE
      *
      *    ****   I S   T H I S   P R O G R A M   I N   T H E   Q
      *
               IF PAR-USR(WS-SUB) = LS-USER
      *
      *    ****   I S   I T   N E X T   I N   L I N E   T O   P R O C E S S
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
             MOVE LS-USER            TO PAR-USR(WS-SUB).
             MOVE WS-SUB             TO PAR-USERS.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             GO TO AY62-CHECK.
      *
      *    ****   S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
             MOVE 300                TO WS-WAIT.
           IF NOT(PAR-USR(WS-SUB) = LS-USER)
               IF WS-SUB < 24
                   ADD 1             TO WS-SUB
                   GO TO AY62.

       AY62-CHECK.
           IF WS-WAIT > ZERO
               COMMIT
               DISPLAY "Waiting" AT 5072 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK
               ACCEPT WS-STIME FROM TIME
               PERFORM LOCK-REC-LOOP
               DISPLAY "Waiting" AT 5072 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK
               GO TO AY60.
             DISPLAY SPACE AT 5072 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.

       AY63.
             MOVE WS-SUB             TO WS-USUB.
             GO TO AY61.
      *
      *    ****    End transaction
      *
       AY70.
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 2.
      *
             PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
             PERFORM AY40 THRU AY59.
             MOVE 1                  TO WS-USUB.

       AY72.
           IF NOT(PAR-USR(WS-USUB) = LS-USER)
               ADD 1                 TO WS-USUB
               GO TO AY72.

       AY75.
             MOVE SPACES             TO PAR-PROG(WS-USUB) PAR-USR(WS-USUB).
           IF WS-USUB < 24
               ADD 1 WS-USUB         GIVING WS-SUB
           ELSE
               GO TO AY80.
           IF NOT(PAR-PROG(WS-SUB) = SPACES)
               MOVE PAR-PROG(WS-SUB) TO PAR-PROG(WS-USUB)
               MOVE PAR-USR(WS-SUB)  TO PAR-USR(WS-USUB)
               ADD 1                 TO WS-USUB
               GO TO AY75.

       AY80.
            SUBTRACT 1 FROM WS-USUB  GIVING PAR-USERS.
            PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
            COMMIT.


       AY999.
             EXIT.
      *
      *    ****    Read/Write Creditor record
      *
      *    This routine is performed and the following are the variables
      *    that are required to have been set up.
      *
      *          VARIABLES USED FOR CRPRDWR as instructions and data
      *
      *    03  W15-INSTR             PIC  X(01).
      *
      *        88  ADD-ACCOUNT                     VALUE "A".
      *
      *            W15-CREDREC needs to contain details for the account 
      *                        that must be added
      *
      *        88  DELETE-ACT                      VALUE "D".
      *
      *            W15-CREDREC needs to contain the details for the 
      *                        account to be deleted
      *
      *        88  GET-ACCOUNT                     VALUE "G".
      *
      *            W15-LINCOL must have the screen location of where the
      *                       User will key in the account number. 
      *                            eg. 0617 = Line 6 Column 17.
      *
      *            W15-BGRND  The Background colour that must be used.
      *            W15-FGRND  The Foreground colour that must be used.
      *                       The folloeing values are available in the
      *                       COPY file "WS.WS" that is included in the
      *                       WORKING-STORAGE SECTION.
      *
      *                           78  Black     value 0.
      *                           78  Blue      value 1.
      *                           78  Green     value 2.
      *                           78  Cyan      value 3.
      *                           78  Red       value 4.
      *                           78  Magenta   value 5.
      *                           78  Brown     value 6.
      *                           78  Grey      value 7.
      *
      *                       If the screen colours for the calling 
      *                       program are Black on White(Grey) then
      *                       that would be setup as follows:
      *
      *                           MOVE Grey  TO W15-BGRND.
      *                           MOVE Black TO W15-FGRND.
      *
      *        88  READ-LOCK                       VALUE "L".
      *
      *            W15-ACNO must contain the Account Number of the 
      *                     account record to be read and locked.
      *
      *        88  READ-RECORD                     VALUE "R".
      *
      *            W15-ACNO must contain the Account Number of the 
      *                     account record to be read.
      *
      *        88  UPDATE-ACT                      VALUE "U".
      *
      *            W15-CREDREC needs to contain the details for the 
      *                        account record to be updated.
      *
      *        88  UPDATE-UNL                      VALUE "X".
      *
      *            W15-CREDREC must contain the the details for the  
      *                        account record to be updated and unlocked.
      *
      *    This variable informs the called program to write details of
      *    the recoosd to the RECOVERY file.
      * 
      *    03  W15-RECOVER.
      *        05  W15-RECV        PIC  X(01)      VALUE "N".
      *            88  ADD-TO-RECV                 VALUE "Y".
      * 
      *    If the record must be written to the RECOVERY file; the 
      *    calling program must MOVE "Y" TO W15-RECV before calling 
      *    CRPRDWR. CRPRDWR will always change it back to "N" before 
      *    returning control to the calling program.
      *
      *    When a read CREDITOR account has been requested WITH W15-RECV
      *    havinge a value of "N" then the program will exit with the
      *    account record in W15-CREDREC. If W15_RECV = "Y" then 
      *    W15-CREDREC remains unchanged.
      *
      *    On exiting W15-ERROR will be 0 if successful or contain and 
      *    error code for unsuccessful requests:
      *               0 = successful
      *               1 = No Record
      *               2 = account number on File not the same as requested
      *               9 = CRPRDWR called with incorrect instruction
      *
              
       COPY "CA155.CRP".

      *
      *    ****    A G E   C R E D I T O R   A C C O U N T S
      *
       HA000      SECTION 5.
       HA00.
             ADD 1 W12-MONTH         GIVING WS-S8.
           IF WS-S8 > 12
               MOVE 1                TO WS-S8.
             PERFORM SAVE-SCREEN-3.
             DISPLAY S39.

       HA05.
             PERFORM TEST AFTER UNTIL WS-OPTION = "N" OR "Y"
                     ACCEPT WS-OPTION AT 0944 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO
                     CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS
             END-PERFORM.
           IF WS-OPTION = "N"
               GO TO HA999.
             DISPLAY "Have BACKUP copies of files been made (Y/N)" AT 1112.

       HA10.
             PERFORM TEST AFTER UNTIL WS-OPTION = "N" OR "Y"
               ACCEPT WS-OPTION AT 1156 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO
               CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS
             END-PERFORM.
           IF WS-OPTION = "N"
               GO TO HA999.
             DISPLAY "Have Remittance Advices been printed (Y/N)" AT 1312.

       HA15.
             PERFORM TEST AFTER UNTIL WS-OPTION = "N" OR "Y"
               ACCEPT WS-OPTION AT 1355 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO
               CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS
             END-PERFORM.
          IF WS-OPTION = "N"
              GO TO HA999.
             DISPLAY "Are you sure (Y/N)" AT 1512.

       HA20.
             PERFORM TEST AFTER UNTIL WS-OPTION = "N" OR "Y"
               ACCEPT WS-OPTION AT 1531 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO
               CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS
             END-PERFORM.
           IF WS-OPTION = "N"
              GO TO HA999.
             DISPLAY "Is this a restart (Y/N)" AT 1712.
             MOVE "N"                TO WS-REST.

       HA22.
             PERFORM TEST AFTER UNTIL WS-REST = "N" OR "Y"
               ACCEPT WS-REST AT 1736 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO
               CALL "CBL_TOUPPER" USING WS-REST BY VALUE WS-LENGTH RETURNING WS-STATUS
             END-PERFORM.
           IF WS-REST = "Y"
               PERFORM HA90 THRU HA100.
             MOVE ZERO               TO W70-DUE-NOW W70-DUE-30 W70-DUE-60 W70-DUE-90 W70-DUE-120 W70-DUE-150 W70-DUE-180
                                        W70-OPEN W70-CLOSE W70-DEBT
             PERFORM AY60 THRU AY999.
             MOVE "  !"              TO P-NUMBER.
             DISPLAY "Account :" AT 1920.
             PERFORM START-AT-CRED-AC THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
               GO TO HA40.

       HA25.
             PERFORM READ-CREDIT-NEXT-LOCK THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
               GO TO HA40.
             DISPLAY P-NUMBER   AT 1930 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                             " " P-NAME WITH FOREGROUND-COLOR Brown HIGHLIGHT.
           IF WS-REST = "Y"
               IF P-NUMBER < W15-ACNO
                   ADD P-BAL         TO W70-OPEN W70-CLOSE
                   ADD P-30D         TO W70-DUE-30
                   ADD P-60D         TO W70-DUE-60
                   ADD P-90D         TO W70-DUE-90
                   ADD P-120D        TO W70-DUE-120
                   ADD P-150D        TO W70-DUE-150
                   ADD P-OUT         TO W70-DUE-NOW
                   GO TO HA35
               ELSE
                   MOVE "N"          TO WS-REST.
      *    IF OPEN-ITEM
      *        GO TO HA70.
             ADD P-30D               TO P-OUT.
             MOVE P-60D              TO P-30D.
             MOVE P-90D              TO P-60D.
             MOVE P-120D             TO P-90D.
             MOVE P-150D             TO P-120D.
             MOVE P-180D             TO P-150D.
             MOVE ZERO               TO P-180D P-DT P-CR.
             MOVE P-BAL              TO P-PBAL.
             COMPUTE P-OUT = P-BAL - (P-30D  + P-60D  + P-90D + P-120D + P-150D + P-180D).
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
             ADD P-BAL               TO W70-OPEN W70-CLOSE.
             ADD P-30D               TO W70-DUE-30.
             ADD P-60D               TO W70-DUE-60.
             ADD P-90D               TO W70-DUE-90.
             ADD P-120D              TO W70-DUE-120.
             ADD P-150D              TO W70-DUE-150.
             ADD P-OUT               TO W70-DUE-NOW.
             MOVE P-NUMBER           TO CT-CRED.
             MOVE ZERO               TO CT-CRED CT-SDTE CT-CODE CT-SEQ.
             MOVE SPACES             TO CT-SINV.
             PERFORM START-AT-CTRN-KEY THRU READ-CRTRAN-EXIT.
           IF WS-F-ERROR = 4
               GO TO HA35.

       HA30.
             PERFORM READ-CRTRAN-NEXT THRU READ-CRTRAN-EXIT.
           IF (WS-F-ERROR = 4) OR (CT-CRED NOT = P-NUMBER)
               GO TO HA35.
             PERFORM DELETE-CRTRAN-REC THRU WRITE-CRTRAN-EXIT.
             GO TO HA30.

       HA35.
             PERFORM REWRITE-CREDIT-UNLOCK THRU WRITE-CREDIT-EXIT.
             GO TO HA25.

       HA40.
             PERFORM RESTORE-SCREEN-3.
             DISPLAY S56.

       HA45.
             MOVE 12                 TO WS-MES-LINE.
             MOVE 50                 TO WS-MES-COL.
             MOVE SPACE              TO WS-OPTION.
             MOVE "i"                TO WS-INSTR.
      *
      *      ACCEPT the option at the position specified above
      *
             MOVE X"FA"              TO WS-MES-CHAR(48).
             PERFORM OPT-MESSAGE.
             CLOSE CTRANS.
             DISPLAY "Reading :" AT 1412.
             DISPLAY "Writing :" AT 1612.
      *
      *    ****    R E N A M E   C R T R A N   F I L E
      *
             CALL "CBL_RENAME_FILE" USING W02-CRTRAN W02-CRTOLD RETURNING WS-STATUS.

       HA50.
             OPEN INPUT CRTOLD.
           IF WS-STAT1 = "9"
               IF FLE-LOCKED
                   DISPLAY "Waiting" AT 5062 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK
                   PERFORM LOCK-REC-LOOP THRU LOCK-REC-EXIT
                   GO TO HA50.
           IF NOT(WS-STATUS = "00")
               MOVE 4                TO WS-F-ERROR
               PERFORM OPEN-ERROR.
             PERFORM CLEAR-L50.

       HA55.
             OPEN OUTPUT CTRANS.
           IF WS-STAT1 = "9"
               IF FLE-LOCKED
                   DISPLAY "Waiting" AT 5062 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK
                   PERFORM LOCK-REC-LOOP THRU LOCK-REC-EXIT
                   GO TO HA50.
           IF NOT(WS-STATUS = "00")
               MOVE 4                TO WS-F-ERROR
               PERFORM OPEN-ERROR.
             PERFORM CLEAR-L50.

       HA60.
             READ CRTOLD AT END GO TO HA65.
             DISPLAY CTO-CRED                             AT 1422 WITH FOREGROUND-COLOR Cyan HIGHLIGHT 
                             " " CTO-SDTE                         WITH FOREGROUND-COLOR Cyan HIGHLIGHT 
                                         " " CTO-CODE             WITH FOREGROUND-COLOR Cyan HIGHLIGHT 
                                                     " " CTO-SINV WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      *
      *    THIS ROUTINE WRITES THE DETAILS TO THE NEW FILE.
      *
             MOVE CTO-REC            TO CT-REC.
             DISPLAY CT-CRED                          AT 1622 WITH FOREGROUND-COLOR Brown HIGHLIGHT 
                            " " CT-SDTE                       WITH FOREGROUND-COLOR Brown HIGHLIGHT 
                                       " " CT-CODE            WITH FOREGROUND-COLOR Brown HIGHLIGHT 
                                                  " " CT-SINV WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             PERFORM WRITE-CRTRAN THRU WRITE-CRTRAN-EXIT.
           IF WS-STAT1 NOT = "0"
               MOVE 4                TO WS-F-ERROR
               PERFORM WRITE-ERROR.
               GO TO HA60.

       HA65.
             CLOSE CTRANS CRTOLD.
      *
      *    ****    D E L E T E   O L D   C R T R A N   F I L E
      *
             CALL "CBL_DELETE_FILE" USING W02-CRTOLD RETURNING WS-STATUS.
             OPEN I-O CTRANS.
      *
      *    ****    End Recovery Logging.
      *
             PERFORM AY70 THRU AY999.
             GO TO HA999.

      *HA70.
      *      DISPLAY "Transaction :" AT 1912.
      *      MOVE ZERO   TO P-OUT
      *        P-30D
      *        P-60D
      *        P-90D
      *        P-120D
      *        P-150D
      *        P-180D.
      *      MOVE P-NUMBER  TO CT-CRED.
      *      MOVE ZERO   TO CT-CRED
      *        CT-SDTE
      *        CT-CODE
      *        CT-SEQ.
      *      MOVE SPACES  TO CT-SINV.
      *      PERFORM START-AT-CTRN-KEY THRU READ-CRTRAN-EXIT.
      *    IF WS-F-ERROR = 4
      *        GO TO HA********

      *HA75.
      *      PERFORM READ-CRTRAN-NEXT THRU READ-CRTRAN-EXIT.
      *    IF (WS-F-ERROR = 4) OR
      *       (CT-CRED NOT = P-NUMBER)
      *        GO TO HA*******
      *      DISPLAY CT-CRED AT 1926
      *       WITH FOREGROUND-COLOR Brown HIGHLIGHT " "
      *       CT-SDTE
      *       WITH FOREGROUND-COLOR Brown HIGHLIGHT " "
      *       CT-CODE
      *       WITH FOREGROUND-COLOR Brown HIGHLIGHT " "
      *       CT-SINV
      *       WITH FOREGROUND-COLOR Brown HIGHLIGHT.
      *    IF CT-COST

       HA90.
             DISPLAY "Restart at A/C No." AT 2012.
             MOVE SPACES             TO W15-ACNO.

       HA95.
             DISPLAY S55.
             ACCEPT W15-ACNO AT 2031 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY MOVE SPACES TO W15-ACNO
                              GO TO HA100
                 WHEN F1-KEY  PERFORM HELP-ROUTINE
                 WHEN F2-KEY  PERFORM AA50-LOOKUP
                 WHEN OTHER   PERFORM AA900-ALARM
                              GO TO HA95
               END-EVALUATE
               DISPLAY W15-ACNO AT 2031 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
               IF W15-ACNO = SPACES
                   GO TO HA95.
           IF W15-ACNO = SPACES
               GO TO HA100.
             PERFORM CLEAR-L50.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W15-ACNO BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             PERFORM CA155-GET-CREDITOR.
           IF WS-ERROR NOT = ZERO
               GO TO HA95.
             DISPLAY P-NAME AT 2038 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.

       HA96.
             MOVE "'C'ontinue or 'R'ekey  [ ]" TO WS-ERR-MES.
             MOVE "C"                TO WS-OPTION.
             PERFORM OPT-MESSAGE TEST AFTER
                 UNTIL WS-OPTION = "C" OR "R".
           IF WS-OPTION = "R"
               GO TO HA95.

       HA100.
             EXIT.

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

             OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
             MOVE 2                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             GO TO ZA999-EXIT.

      *COPY ZA49.PRO.

       ZA999-EXIT.
             EXIT.
      *
      *    ****    I / O   E R R O R   M E S S A G E S
      *
       ZB000-I-O-ERROR SECTION 9.

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
               WHEN 18         MOVE W02-RECOVER TO WS-FILE
                               MOVE WS-RECKEY   TO WS-KEY
               WHEN 24         MOVE W02-CRDMEM  TO WS-FILE
                               MOVE CME-KEY     TO WS-KEYX
               WHEN 37         MOVE W02-SHARED  TO WS-FILE
                               MOVE WS-SHARED   TO WS-KEY
               WHEN OTHER      MOVE "ERROR"     TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
