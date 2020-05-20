      $set LINKCOUNT"384" GNT"CRP002.GNT"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******     ****      ****     ******    *
      *   **    **  **    **  **    **   **  **    **  **   **    **   *
      *   **        **    **  **    **  **    **  **    **       **    *
      *   **        ******    *******   **    **  **    **     **      *
      *   **        **   **   **        **    **  **    **   **        *
      *   **    **  **    **  **         **  **    **  **   **         *
      *    ******   **    **  **          ****      ****    ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *   C R E D I T O R   A M E N D   A C C O U N T   D E T A I L S  *
      *                                                                *
      *       Version 9.04.05 - June 2018                              *
      *                                                                *
      ******************************************************************
      *                                                                *
      * November 2012 - Change Address field from P-ADDR PIC X(70) to  *
      *                 to a group level containing four new fields    *
      *                 P-ADD-L1/2/3/4 each as PIC x(30). As new PC's  *
      *                 and Servers contain huge amounts of disk space *
      *                 this has been changed, which also results in   *
      *                 simplifying the code for handling addresses in *
      *                 Data capture, enquiries, reports and other     *
      *                 procedures. This also removes the restriction  *
      *                 as to the number of characters that may be     *
      *                 used for an address.                           *
      *                                                                *
      *                 Included a new field P-EMAIL PIC X(64) for an  *
      *                 eMail address.                                 *
      *                                                                *
      * Added Street address as a group field containing P-SADD-L1/2/3 *
      * /4 each as PIC  X(30) and a postal code field as PIC  X(08).   * 
      *                                                                *
      * Added a second telephone number P-TEL2 PIC  X(14)              *
      *                                                                *
      * Added a Cell(Mobile) phone P-CELL  PIC  X(14)                  *
      *                                                                *
      * Deleted P-TLK (No longer used)                                 *
      *                                                                *
      * June 2018 - Added program CRPRDWR to handle IO for Creditor    *
      *             file and COPY file CA155.CRP to call and handlw IO *
      *             for the CREDITOR file. SEE CA155.CRP and CRPRDWR   *
      *             for details.                                       *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        CRP002.
       AUTHOR.            J.W.LEMMON (CSS).
       DATE-WRITTEN.      JUNE, 1983.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2018
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

       COPY "CRDMEM.SL".

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

       COPY "CREDIT.FDE".

       COPY "CRDMEM.FDE".

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
       77  WS-NETKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-RECKEY               PIC  9(04)    COMP-5.
       77  WS-SHARED               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-RECOVER              PIC  9(04)    COMP-5.
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
       77  WS-SKIP                 PIC  X(01)           VALUE " ".
       77  WS-CHAR1                PIC  X(01).
       77  WS-LIN                  PIC  9(02)           VALUE ZERO.
       77  WS-LINES                PIC  9(04)    COMP-5 VALUE 66.
       77  WS-PAGE                 PIC  9(04)    COMP-5.
       77  WS-SEASON               PIC  X(01).
       77  WS-ER1                  PIC  X(23)           VALUE "Address > 57 characters".
       77  WS-ER8                  PIC  X(09)           VALUE "No record".
       77  WS-ER11                 PIC  X(15)           VALUE "Data incomplete".
       77  WS-H1                   PIC  X(05)           VALUE "DATE:".
       77  WS-H3                   PIC  X(05)           VALUE "PAGE:".
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
           03  CRT-PROGRAM         PIC  X(15)           VALUE "CRP002".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40)           VALUE "AMEND CREDITOR ACCOUNT".
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
           03  WS-MODULE           PIC  X(03)           VALUE "CRP".
           03  WS-PROG             PIC  X(03)           VALUE "002".

       01  WS-PARID.
          03  WS-SYS-ID            PIC  X(03).
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

       01  W20-CALCS.
           03  W20-RESULT          PIC  9(06)V99.
           03  W20-RESULT-1        REDEFINES W20-RESULT.
               05  W20-WHOLE       PIC  9(06).
               05  W20-DECIMAL     PIC  9(02).
           03  W20-ORD             PIC  9(07)    COMP-3.

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

      *01  W95-COMPANY.
      *    03  W95-COMP      PIC  X(40).

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

       COPY "CREDITOR.CRT".

       01  S55.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
      *                                             1         2         3         4      
      *                                    1234567890123456789012345678901234567890123456
      *
               05  LINE 50 COLUMN  2 VALUE "F1"                                           FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN  4 VALUE   "=Help,".
               05          COLUMN 10 VALUE         "F2"                                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 12 VALUE           " to do Creditor Lookup ".
               05          COLUMN 35 VALUE                                  "Esc"         FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 38 VALUE                                     " to exit".

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
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM CC000.
             CLOSE RECOVER.
       AA49.
             EXIT PROGRAM.

       COPY "AA50C.LUP".
       
       COPY "AA900.ALM".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                      CREDITOR-MEMO                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ The account number of the Debtor record for which the MEMO³
      *    ³ details are to be displayed must be in W15-ACNO.          ³
      *    ³ PERFORM CREDITOR-MEMO.                                    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "CRDMEM.PRO".

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

       COPY "CRDMEM.RD".

       COPY "PARAM.RD".

       COPY "SHARED.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AC00-WRITE    SECTION.

       COPY "CONTROL.WR".

       COPY "CREDIT.WR".

       COPY "PARAM.WR".

      *COPY "CREDIT.LUP".

       COPY "APAC.HLP".

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
      *    ****              C R E D I T O R   F I L E              ****
      *    *************************************************************
       AY07.
             MOVE 07                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-CREDIT-LOCK THRU READ-CREDIT-EXIT.
             MOVE P-REC              TO REC-CREDIT.
             GO TO AY50.
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
             SUBTRACT 1 FROM WS-USUB GIVING PAR-USERS.
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
      *    ****    A M E N D   C R E D I T O R   D E T A I L S
      *
       CC000      SECTION 5.
       CC00.
             PERFORM HEADING-AND-CLEAR-SCREEN.
             DISPLAY S05.
             MOVE SPACES         TO W15-ACNO.

       CC02.
             DISPLAY S55.
             ACCEPT S06A.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY MOVE SPACES TO W15-ACNO
                              GO TO CC999
                 WHEN F1-KEY  PERFORM HELP-ROUTINE
                 WHEN F2-KEY  PERFORM AA50-LOOKUP
                 WHEN OTHER   PERFORM AA900-ALARM
                              GO TO CC02
               END-EVALUATE
               DISPLAY S06A
               IF W15-ACNO = SPACES
                  GO TO CC02.
             PERFORM CLEAR-L50.
           IF W15-ACNO = SPACES
               GO TO CC999.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W15-ACNO BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             PERFORM PERFORM CA155-IO-CREDITOR.
           IF WS-ERROR NOT = ZERO
               MOVE WS-ER8           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CC02.

       CC005.
             MOVE P-REC              TO W15-CREDREC.
      *      MOVE P-NAME             TO W15-NAME.
      *      MOVE P-ADDR             TO W40-ADD.
      *      MOVE P-CONTACT          TO W15-CONTACT.
      *      MOVE P-TEL              TO W15-TEL.
      *      MOVE P-FAX              TO W15-FAX.
      *      MOVE P-30               TO W15-30.
      *      MOVE P-60               TO W15-60.
      *      MOVE P-90               TO W15-90.
      *      MOVE P-120              TO W15-120.
      *      MOVE P-150              TO W15-150.
      *      MOVE P-180              TO W15-180.
      *      MOVE P-PC               TO W15-PCODE.
      *      MOVE P-TAX              TO W15-TAX.
      *      MOVE P-PMNT             TO W15-PMNT.
      *      MOVE P-30-DISC          TO W15-DISC1.
      *      MOVE P-60-DISC          TO W15-DISC2.
      *      MOVE P-STM              TO W15-STM.
      *      UNSTRING W40-ADD DELIMITED BY "*" INTO W15-ADDR1 W15-ADDR2 W15-ADDR3 W15-ADDR4 OVERFLOW MOVE 1  TO WS-ERROR.

       CC010.
             ACCEPT S06B.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO CC010.
             PERFORM CLEAR-L50.
           IF W15-NAME = SPACES
               MOVE WS-ER11          TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CC010.
           IF W15-ADDRESS = SPACES
               MOVE WS-ER11          TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CC010.
      *      PERFORM CE000.
           IF WS-ERROR > ZERO
               MOVE WS-ER1           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CC010.
             CALL "CBL_TOUPPER" USING W15-TAX BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(W15-TAX = "E" OR "I" OR "N")
               MOVE "VAT code must be E, I or N" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CC010.
      *    IF NOT(W15-STM = "O" OR "B")
      *        MOVE "Account type must be O, or B" TO WS-ERR-MES
      *        GO TO CC010.
             PERFORM AY60 THRU AY999.
             MOVE 0                  TO WS-ACTION.
             PERFORM AY07 THRU AY59.
             MOVE W15-CREDREC        TO P-REC.
      *      MOVE W15-NAME           TO P-NAME.
      *      MOVE W15-TEL            TO P-TEL.
      *      MOVE W15-FAX            TO P-FAX.
      *      MOVE W15-CONTACT        TO P-CONTACT.
      *    IF W15-30 > 0
      *        MOVE 1                TO W15-30.
      *    IF W15-60 > 0
      *        MOVE 1                TO W15-60.
      *    IF W15-90 > 0
      *        MOVE 1                TO W15-90.
      *    IF W15-120 > 0
      *        MOVE 1                TO W15-120.
      *    IF W15-150 > 0
      *        MOVE 1                TO W15-150.
      *    IF W15-180 > 0
      *        MOVE 1                TO W15-180.
      *        MOVE W15-30           TO P-30.
      *        MOVE W15-60           TO P-60.
      *        MOVE W15-90           TO P-90.
      *        MOVE W15-120          TO P-120.
      *        MOVE W15-150          TO P-150.
      *        MOVE W15-180          TO P-180.
      *      MOVE W40-ADD            TO P-ADDR.
      *      MOVE W15-PCODE          TO P-PC.
      *      MOVE W15-TAX            TO P-TAX.
      *      MOVE W15-PMNT           TO P-PMNT.
      *      MOVE W15-STM            TO P-STM.
      *      MOVE W15-DISC1          TO P-30-DISC.
      *      MOVE W15-DISC2          TO P-60-DISC.
             PERFORM REWRITE-CREDIT THRU WRITE-CREDIT-EXIT.
             PERFORM AY70 THRU AY999.
             GO TO CC00.

       CC999.
             EXIT.
      *
      *    ****    P A C K   A D D R E S S
      *
      *CE000      SECTION 5.
      *CE00.
      *      MOVE ZERO TO WS-ERROR.
      *      MOVE 1                  TO WS-S1 WS-S2.
      *      MOVE 30 TO WS-S3.
      *      MOVE SPACES TO W40-ADD.
      *
      *CE05.
      *   IF W15-ACHAR(WS-S3) NOT = SPACE
      *        GO TO CE10.
      *    IF WS-S3 > WS-S2
      *        SUBTRACT 1 FROM WS-S3
      *        GO TO CE05.
      *
      *CE10.
      *     MOVE W15-ACHAR(WS-S2) TO W40-ACHAR(WS-S1).
      *    IF WS-S2 = WS-S3
      *        GO TO CE15.
      *      ADD 1 TO WS-S1.
      *      ADD 1 TO WS-S2.
      *    IF WS-S1 > 70
      *        MOVE 1 TO WS-ERROR
      *    ELSE
      *        GO TO CE10.
      *
      *CE15.
      *    IF WS-ERROR = 1
      *        GO TO CE20.
      *    IF WS-S1 < 70
      *        ADD 1 TO WS-S1
      *       MOVE "*" TO W40-ACHAR(WS-S1).
      *
      *CE20.
      *      ADD 1 TO WS-S1.
      *
      *CE25.
      *      MOVE 31 TO WS-S2.
      *      MOVE 60 TO WS-S3.
      *      PERFORM CE05 THRU CE20.
      *    IF WS-ERROR = 1
      *        GO TO CE999.
      *      MOVE 61 TO WS-S2.
      *      MOVE 90 TO WS-S3.
      *      PERFORM CE05 THRU CE20.
      *    IF WS-ERROR = 1
      *        GO TO CE999.
      *      MOVE 91 TO WS-S2.
      *      MOVE 120 TO WS-S3.
      *      PERFORM CE05 THRU CE20.
      *
      *CE999.
      *      EXIT.

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
             MOVE 2              TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
      *      GO TO ZA999-EXIT.
      *
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
       ZB000-I-O-ERROR SECTION 9.

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
               WHEN 18         MOVE W02-RECOVER TO WS-FILE
                               MOVE WS-RECKEY   TO WS-KEY
               WHEN 24         MOVE W02-CRDMEM  TO WS-FILE
                               MOVE CME-KEY     TO WS-KEYX
               WHEN 37         MOVE W02-SHARED  TO WS-FILE
                               MOVE WS-SHARED   TO WS-KEY
               WHEN OTHER      MOVE "ERROR"     TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
