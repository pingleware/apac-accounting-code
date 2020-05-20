      $SET LINKCOUNT"512" GNT"DTP006.GNT"
      *
      *     ONLY DELETE WHEN ALL THE BELOW IS COMPLETED
      *     ===========================================
      *
      *  Screen co-ordinates must be revised for new layout
      *
      *  Screen Section                            Completed  
      *                                               ___
      *                    Layout screen co-ordinates|   |
      *                                               ---
      *                                               ___
      *                    Input screen co-ordinates |   |
      *                                               ---
      *                                               ___
      *                    Output screen co-ordinates|   |
      *                                               ---
      *  Procedure Division 
      *                                               ___
      *                    Screen Heading Format     |   |
      *                                               ---
      *                                               ___
      *                    Error Messages            |   |
      *                                               ---
      *                                               ___
      *                    User information messages |   |
      *                                               ---
      *                                               ___
      *                    Accept User options       |   |
      *                                               ---
      *                                               ___
      *                    Display co-ordinates      |   |
      *                                               ---
      *                                               ___
      *                    Accept co-ordinates       |   |
      *                                               ---
      *  Lookups and Memo
      *                                               ___       
      *         Change to the new method and Calls   |   |
      *                                               ---
      *  Data Division.
      *                                               ___
      *         Line up Pic, Comp, Redefines etc.    | x |
      *                                               ---
      *                                               ___
      *         Remove unused variables and insert   | x |
      *            all the New Layouts                ---
      *
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****      ****     ******    *
      *   **    **     **     **    **   **  **    **  **   **    **   *
      *   **    **     **     **    **  **    **  **    **  **         *
      *   **    **     **     *******   **    **  **    **  *******    *
      *   **    **     **     **        **    **  **    **  **    **   *
      *   **    **     **     **         **  **    **  **   **    **   *
      *   *******      **     **          ****      ****     ******    *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *          A C C O U N T S   R E C E I V A B L E   /             *
      *                                                                *
      *       D E B T O R   A G E   A C C O U N T S   P R O G R A M    *
      *                                                                *
      *       Version 9.04.04 - February/March 2018                    *
      *                                                                *
      ******************************************************************
      *                                                                *
      * May 2006 - New field included in Accounts Receivable for       *
      *            recording number of outstanding Jobs linked to the  *
      *            account. Additional filler added for future         *
      *            expansion.                                          *
      *                                                                *
      *  October 2006 - Amended Accounts receivable ageing from a      *
      *                 specific date to the actual date on which      *
      *                 statements and month-end is run.               *
      *                                                                *
      *  October 2008 - Amended Accounts receivable ageing to create   *
      *                 duplicate files of the accounts and trans-     *
      *                 actions for printing of copy satements when    *
      *                 and if required. The file names are AR_Ayymm   *
      *                 and AR_Tyymm, where yymm is replaced with      *
      *                 the year and month.                            *
      *                                                                *
      *  Jan 2013 - Program DTPMEMO introduced to handle memo lookups  *
      *             reducing code in all programs and removing file    *
      *             DEBMEM from programs. Copy file 'DEBMEM.PRO' now   *
      *             copied into all programs that require lookups for  *
      *             memo/remarks on a Debtor account. Move the Debtor  *
      *             code/account number to W80-ACCOUNT and then        *
      *             PERFORM DEBTOR-MEMO. The Debtor lookup program -   *
      *             DTPLOOK now includes a Memo Lookup so unless there *
      *             is a requirement to lookup memo details after this *
      *             lookup, the MEMO lookup can be removed from all    *
      *             DEBTOR programs.                                   *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     DTP006.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   March 1982.

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

       COPY "CONTROL.SL".

           SELECT DBTOLD  ASSIGN DISK
                          LOCK EXCLUSIVE
                          STATUS WS-STATUS
                          ACCESS SEQUENTIAL
                          ORGANIZATION SEQUENTIAL.

       COPY "DBTRAN.SL".

       COPY "DTHIST.SL".

       COPY "DBHIST.SL".

       COPY "DEBTOR.SL".

       COPY "INVOIC.SL".

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

       COPY "DBTOLD.FD".

       COPY "DBTRAN.FDE".

       COPY "DTHIST.FDE".

       COPY "DBHIST.FDE".

       COPY "DEBTOR.FDE".

       COPY "INVOIC.FDE".

       COPY "PARAM.FDE".

       COPY "RECOVER.FD".

       COPY "SHARED.FDE".

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
       77  WS-SHARED               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-TRANS                PIC  9(04)    COMP-5 VALUE 1.
       77  WS-GST-RATE             PIC S9(03)V99 COMP-3.
       77  WS-FIN-RATE             PIC S9(03)V99 COMP-3.
       77  WS-PERIOD               PIC  9(02).
       77  WS-NTP                  PIC  X(01).
       77  WS-KEYSTR               PIC  9(06)    COMP-5.
       77  WS-INSTR                PIC  X(01).
       77  WS-OPTION               PIC  X(01).
       77  WS-SKIP                 PIC  X(01)           VALUE "N".
       77  WS-TAX                  PIC  X(01).
       77  WS-REPLY                PIC  X(01).
       77  WS-REST                 PIC  X(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-ERROR                PIC  9(01)           VALUE ZERO.
       77  WS-EKEY                 PIC  9(06)    COMP-5.
       77  WS-CHAR1                PIC  X(01).
       77  WS-CHAR2                PIC  X(01).
       77  WS-COMP                 PIC  9(01).
       77  WS-BATCH                PIC  9(06)    COMP-5.
       77  WS-ERR1                 PIC  X(22)           VALUE "Invalid Account Number".
       77  WS-ERR2                 PIC  X(09)           VALUE "No Record".
       77  WS-E                    PIC  X(15)           VALUE "**END** - ENTER".
       77  WS-AC                   PIC  X(07)           VALUE "ACCOUNT".
       77  PRG-DEBT-LUP            PIC  X(12)           VALUE "DTP\DTPLOOK".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.
      *
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
           03  CRT-START           PIC  9(02).
           03  CRT-END             PIC  9(02).
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15) VALUE "DTP006".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "ACCOUNTS RECEIVABLE - AGE ACCOUNTS".
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
           03  WS-MODULE           PIC  X(03)    VALUE "DTP".
           03  WS-PROG             PIC  X(03)    VALUE "006".

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

       01  W05-DEB-HISTORY.
           03  W05-DBT-HIS.
               05  W05-DBT         PIC  X(04)    VALUE "AR_T".
               05  W05-TYY         PIC  9(02).
               05  W05-TMM         PIC  9(02).
               05  FILLER          PIC  X(04)    VALUE ".DAT".
           03  W05-DEB-HIS.
               05  W05-DEB         PIC  X(04)    VALUE "AR_A".
               05  W05-DEBH-ID.
                   07  W05-DYY     PIC  9(02).
                   07  W05-DMM     PIC  9(02).
               05  W05-HIST-ID                   REDEFINES W05-DEBH-ID PIC  9(04).
               05  FILLER          PIC  X(04)    VALUE ".DAT".

       01  W10-SPARES.
           03  W10-DATE            PIC  X(10).
           03  W10-RDATE                         REDEFINES W10-DATE.
               05  W10-EDTE        PIC 99/99/9999.

       01  W10-REF.
           03  W10-NUM             PIC  9(08).

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

       01  W25-CALCS.
           03  W25-RESULT          PIC  9(05)V99.
           03  W25-RESULT1                       REDEFINES W25-RESULT.
               05  W25-WHOLE       PIC  9(05).
               05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2                       REDEFINES W25-RESULT.
               05  W25-KEY         PIC  9(04).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).
           03  W25-TOTAL           PIC S9(09)V99 COMP-3.
           03  W25-VALUE           PIC S9(09)V99 COMP-3.

       01  W30-AGE-DATES.
           03  W30-120             PIC  9(08)    COMP-3.
           03  W30-90              PIC  9(08)    COMP-3.
           03  W30-60              PIC  9(08)    COMP-3.
           03  W30-30              PIC  9(08)    COMP-3.
           03  W30-CUR             PIC  9(08)    COMP-3.

       COPY "W40.DBT".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

      *COPY "W50.WS".

       01  W60-HIST.
           03 W60-PATH             PIC  X(64).

       01  W75-KEYS.
           03  W75-S               PIC  9(02)    COMP-5.
           03  W75-S1              PIC  9(02)    COMP-5.
           03  W75-KEY             PIC  9(06)    COMP-5.
           03  W75-ALLOCATED       PIC S9(09)V99 COMP-3.
           03  W75-BALANCE         PIC S9(09)V99 COMP-3.
           03  W75-DNO             PIC  X(06)    OCCURS 18.

       COPY "W75.WS".

       01  W80-ACCOUNT.
           03  W80-ACNO            PIC X(06).
      *    03  W80-TYPE            PIC 9(01).

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
       01  S25.
           03  BACKGROUND-COLOR Magenta  FOREGROUND-COLOR Magenta.
               05  LINE  9 COLUMN 14 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"                         HIGHLIGHT
               05          COLUMN 67 VALUE                                                      "¿" FOREGROUND-COLOR Black.
               05  LINE 10 COLUMN 14 VALUE "³               "                                                              HIGHLIGHT.
               05          COLUMN 30 VALUE                 "CREATING HISTORY FILES"                 FOREGROUND-COLOR Grey  HIGHLIGHT.
               05          COLUMN 52 VALUE                                       "               ³" FOREGROUND-COLOR Black.
               05  LINE 11 COLUMN 14 VALUE "³"                                                                             HIGHLIGHT.
               05          COLUMN 15 VALUE  "Account: "                                             FOREGROUND-COLOR Green HIGHLIGHT.
               05          COLUMN 24 VALUE           "                                           ³" FOREGROUND-COLOR Black.
               05  LINE 12 COLUMN 14 VALUE "³"                                                                             HIGHLIGHT.
               05          COLUMN 15 VALUE  "Transaction: "                                         FOREGROUND-COLOR Green HIGHLIGHT.
               05          COLUMN 29 VALUE               "                                       ³" FOREGROUND-COLOR Black.
               05  LINE 13 COLUMN 14 VALUE "À"                                                                             HIGHLIGHT.
               05          COLUMN 15 VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" FOREGROUND-COLOR Black.

       01  S27.
           03  BACKGROUND-COLOR Red  FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE  6 COLUMN 12 VALUE "This routine does not include a recovery procedure. Security".
               05  LINE  7 COLUMN 12 VALUE "copies  of the data files should be made as a power  failure".
               05  LINE  8 COLUMN 12 VALUE "or any other problem during this procedure would require the".
               05  LINE  9 COLUMN 12 VALUE "DATA FILES from the previous backup to be restored !!!      ".
               05  LINE 11 COLUMN 12 VALUE "Do you wish to continue (Y/N)?".

       01  S28.
           03  LINE  7 COLUMN 67 PIC Z9/99/9999 USING W90-IDTE FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.

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
           IF LS0-DBLEV < 4
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM CI000.
             CLOSE RECOVER.
       AA49.
             EXIT PROGRAM.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ This routine is used to call the Debtor / Ac's Receivable ³
      *    ³ lookup program and cancel it when control is returned.    ³
      *    ³ The  relevant  error  message  will be  displayed  if  an ³
      *    ³ exception occurs. The usual  parameters are passed to the ³
      *    ³ lookup program as well W40-COMPANY which will contain the ³
      *    ³ details of the account that has been selected when control³
      *    ³ is returned to the calling program.                       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AA50-LOOKUP     SECTION.
       AA50.
             PERFORM SAVE-SCREEN.
             PERFORM SCREEN-CONTENTS.
             CALL PRG-DEBT-LUP USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS W40-COMPANY
                  ON EXCEPTION GO TO AA55
             END-CALL.
             CANCEL PRG-DEBT-LUP.
             MOVE W40-ACNO           TO W80-ACNO.
             GO TO AA58.

       AA55.
             MOVE SPACE              TO WS-ERR-MES.
             STRING "Program- " DELIMITED SIZE PRG-DEBT-LUP DELIMITED " " " not on disk, press ANY key" DELIMITED SIZE INTO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.

       AA58.
             PERFORM RESTORE-SCREEN.

       AA99.
             EXIT.

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
      *    ³ To clear line 50:                                         ³
      *    ³    PERFORM CLEAR-L50                                      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "CLEAR.CRT".

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

       COPY "DATE.CHK".

      *
      *      READ FILES ROUTINES
      *
       AC000              SECTION.

       COPY "CONTROL.RD".

       COPY "DBTRAN.RD1".

       COPY "DEBTOR.RD".

       COPY "INVOIC.RD".

       COPY "PARAM.RD".

       COPY "SHARED.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AD000             SECTION.

       COPY "CONTROL.WR".

       COPY "DBTRAN.WR".

       COPY "DEBTOR.WR".

       COPY "INVOIC.WR".

       COPY "PARAM.WR".

       COPY "APAC.HLP".

      *
      *       ****   *****   ***    ***   *   *  ***** ****   *   *
      *       *   *  *      *   *  *   *  *   *  *     *   *  *   *
      *       ****   ***    *      *   *  *   *  ***   ****    * *
      *       *   *  *      *   *  *   *   * *   *     *   *    *
      *       *   *  *****   ***    ***     *    ***** *   *    *
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
       AY39.
             MOVE 39                 TO REC-FILE.
             MOVE WS-NETKEY          TO REC-KEY.
             MOVE NET-RECORD         TO REC-NETWORK.
             GO TO AY50.
       AY49.
             MOVE 99                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE SPACES             TO REC-DETAIL.
             PERFORM AY50 THRU AY59.
             ADD 1                   TO WS-TRANS.
           IF WS-RECOVER > 95
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
               DISPLAY "Write error Recovery file - Status " AT 2212 WITH FOREGROUND-COLOR Brown HIGHLIGHT WS-STATUS WITH FOREGROUND-COLOR Grey HIGHLIGHT
               STOP RUN.
       AY59.
             EXIT.
      *    *************************************************************
      *    ****        Start processing transaction
      *    *************************************************************
       AY60.
             MOVE 1                  TO WS-COUNT.
             MOVE 5                  TO WS-SHARED.
             PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      *    *************************************************************
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *    *************************************************************
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-USERS < 24
               MOVE 1                TO WS-SUB
               MOVE ZERO             TO WS-WAIT
               GO TO AY62.
      *    *************************************************************
      *    ****   Q   F U L L  -  W A I T   F O R   4 S E C O N D S
      *    *************************************************************
             DISPLAY "WAITING" AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red HIGHLIGHT.
             COMMIT.
             ACCEPT WS-STIME FROM TIME.
             MOVE 400                TO WS-WAIT.
             PERFORM LOCK-REC-LOOP.
             DISPLAY SPACE AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.
             GO TO AY60.

       AY61.
             MOVE "DB"               TO PAR-PROG(WS-USUB).
             MOVE LS-USER            TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *      Read the DEBTOR control record in the NETWORK file and lock
      *      it. Log details to the RECOVERY file.
      *
             MOVE 1                  TO WS-NETKEY.
             PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             PERFORM AY39 THRU AY59.
             GO TO AY999.
      *
      *     A R E   A N Y   U P D A T E S   I N   P R O G R E S S
      *
       AY62.
           IF NOT(PAR-PROG(WS-SUB) = SPACES OR PAR-USR(WS-SUB) = SPACES)
      *
      *     A R E   D E B T O R O R   S T O C K   F I L E S   B E I N G
      *     U P D A T E D
      *
               IF NOT(PAR-PROG(WS-SUB) = SPACES)
                   IF PAR-PROG(WS-SUB) = "DB" OR "DS"
      *
      *     Y E S   -   S E T   W A I T   P E R I O D
      *
                       GO TO AY62-WAIT
                   ELSE
                       ADD 1         TO WS-SUB
                       GO TO AY62
                   END-IF
               ELSE
      *
      *     I S   T H I S   P R O G R A M   I N   T H E   Q
      *
               IF PAR-USR(WS-SUB) = LS-USER
      *
      *     I S   I T   N E X T   I N   L I N E   T O   P R O C E S S
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
      *     S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
             MOVE 300   TO WS-WAIT.
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

       AY70.
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *      Write links back and unlock PARAM record 4 and NETWORK
      *      record 1.
      *
            PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
            PERFORM AY49 THRU AY59.
            MOVE 1                   TO WS-USUB.

       AY72.
           IF NOT(PAR-USR(WS-USUB) = LS-USER)
               IF WS-USUB < 24
                   ADD 1             TO WS-USUB
                   GO TO AY72
               ELSE
                   GO TO AY85.

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

       AY85.
             COMMIT.

       AY999.
             EXIT.
      *
      *      A G E   A C C O U N T S
      *
       CI000         SECTION 5.
       CI00.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
             ADD 1 W12-MONTH         GIVING WS-S8.
           IF WS-S8 > 12
               MOVE 1                TO WS-S8.
             MOVE WS-OPTION          TO WS-NTP.
             DISPLAY S27.

       CI-CHK.
             ACCEPT WS-OPTION AT 1143 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-OPTION = "Y" OR "N")
               GO TO CI-CHK.
           IF WS-OPTION = "N"
               GO TO CI999.
             DISPLAY "Have BACKUP copies of files been made (Y/N)" AT 1312.

       CI05.
             ACCEPT WS-OPTION AT 1356 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-OPTION = "Y" OR "N")
               GO TO CI05.
           IF WS-OPTION = "N"
               GO TO CI999.
             DISPLAY "Have Statements been printed (Y/N)" AT 1512.

       CI10.
             ACCEPT WS-OPTION AT 1547 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-OPTION = "Y" OR "N")
               GO TO CI10.
           IF WS-OPTION = "N"
               GO TO CI999.
             MOVE "Are you sure Y/N" TO WS-OPT-MES.
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "N"
               GO TO CI999.
             DISPLAY "Is this a year end (Y/N)" AT 1912.
             MOVE "N"                TO WS-OPTION.

       CI13.
             ACCEPT WS-OPTION AT 1937 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-OPTION = "Y" OR "N")
               GO TO CI13.
             DISPLAY "Is this a restart (Y/N)" AT 2112.
             MOVE "N"                TO WS-REST.
       CI14.
             ACCEPT WS-REST AT 2136 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO.
             CALL "CBL_TOUPPER" USING WS-REST BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-REST = "Y" OR "N")
               GO TO CI14.
           IF WS-REST = "Y"
               PERFORM CI90 THRU CI100.

       CI15A.
             MOVE ZERO               TO W90-CUR W90-BAL W90-30 W90-60 W90-120 W90-INT W90-90.
             MOVE 1                  TO WS-S3.
             DISPLAY "Account :" AT 2312.
             PERFORM AY60 THRU AY999.
             MOVE "     !"           TO DEB-ACNO.
             PERFORM START-AT-DEBT-AC THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               GO TO CI025.

       CI20.
             PERFORM READ-DEBTOR-NEXT-LOCK THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               GO TO CI025.
             DISPLAY DEB-ACNO AT 2322 WITH FOREGROUND-COLOR Cyan HIGHLIGHT " " DEB-NAME WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             MOVE 19                 TO CRT-START CRT-END.
             PERFORM ERASE-SCREEN.
           IF WS-REST = "Y"
               IF DEB-ACNO < W80-ACNO
                   ADD DEB-BAL       TO W90-BAL
                   ADD DEB-CUR       TO W90-CUR
                   ADD DEB-30        TO W90-30
                   ADD DEB-60        TO W90-60
                   ADD DEB-90        TO W90-90
                   ADD DEB-120       TO W90-120
                   ADD DEB-INT       TO W90-INT
                   GO TO CI20X
               ELSE
                   MOVE "N"          TO WS-REST.
           IF OPEN-ITEM OF DEB-RECORD1
               GO TO CI065.
             ADD DEB-90              TO DEB-120.
             MOVE DEB-60             TO DEB-90.
             MOVE DEB-30             TO DEB-60.
             MOVE DEB-CUR            TO DEB-30.
           IF DEB-BAL NOT > ZERO
               MOVE ZERO             TO DEB-CUR DEB-30 DEB-60 DEB-120 DEB-90
           ELSE
               COMPUTE DEB-CUR = DEB-BAL - (DEB-30 + DEB-60 + DEB-90 + DEB-120 + DEB-INT).
           IF DEB-CUR < ZERO
               ADD DEB-CUR           TO DEB-30
               MOVE ZERO             TO DEB-CUR.
           IF DEB-30 < ZERO
               ADD DEB-30            TO DEB-60
               MOVE ZERO             TO DEB-30.
           IF DEB-60 < ZERO
               ADD DEB-60            TO DEB-90
               MOVE ZERO             TO DEB-60.
           IF DEB-90 < ZERO
               ADD DEB-90            TO DEB-120
               MOVE ZERO             TO DEB-90.
           IF DEB-120 < ZERO
               MOVE ZERO             TO DEB-120.

       CI20-AGE.
             MOVE ZERO               TO DEB-DEBT DEB-CRED.
             MOVE DEB-BAL            TO DEB-STM.
             ADD DEB-BAL             TO W90-BAL.
             ADD DEB-CUR             TO W90-CUR.
             ADD DEB-30              TO W90-30.
             ADD DEB-60              TO W90-60.
             ADD DEB-90              TO W90-90.
             ADD DEB-120             TO W90-120.
             ADD DEB-INT             TO W90-INT.
           IF WS-OPTION = "Y"
               MOVE DEB-PURYTD       TO DEB-PURPRV
               MOVE ZERO             TO DEB-PURYTD.

       CI20A.
             MOVE DEB-ACNO           TO TRA-AC.
             MOVE ZERO               TO TRA-DATE TRA-CODE.
             MOVE SPACES             TO TRA-REF.
             PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
           IF WS-F-ERROR = 5
               GO TO CI20X.

       CI20B.
             PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
           IF (WS-F-ERROR = 5) OR
              (DEB-ACNO NOT = TRA-AC)
               GO TO CI20X.
             PERFORM DELETE-DBTRAN-REC THRU WRITE-DBTRAN-EXIT.
             GO TO CI20B.

       CI20X.
             MOVE ZERO               TO DEB-MTHPUR (WS-S8).
             PERFORM REWRITE-DEBTOR-UNLOCK THRU WRITE-DEBTOR-EXIT.
             GO TO CI20.

       CI025.
             MOVE W90-BAL            TO DEB-OUT.
             MOVE W90-CUR            TO DEB-MTD.
             MOVE W90-30             TO DEB-ONE.
             MOVE W90-60             TO DEB-TWO.
             MOVE W90-90             TO DEB-THREE.
             MOVE W90-120            TO DEB-FOUR.
             MOVE W90-INT            TO DEB-INTEREST.
             MOVE DEB-OUT            TO DEB-OPEN.
             MOVE ZERO               TO DEB-DT DEB-CR.
             MOVE W30-120            TO DEB-120-DTE.
             MOVE W30-90             TO DEB-90D-DTE.
             MOVE W30-60             TO DEB-60D-DTE.
             MOVE W30-30             TO DEB-30D-DTE.
             MOVE W30-CUR            TO DEB-CUR-DTE.
             PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
             PERFORM ERASE-SCREEN.
             DISPLAY "Transaction file will now be rebuilt" AT 0812.
             DISPLAY "NB" AT 1012 WITH FOREGROUND-COLOR Grey HIGHLIGHT
                     " - ALL other workstations must EXIT from ".
             DISPLAY "DEBTORS before continuing " AT 1212
                     "C" WITH FOREGROUND-COLOR Grey HIGHLIGHT "ontinue > <".

       CI26.
             ACCEPT WS-OPTION AT 1248 WITH FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-OPTION = "C")
               GO TO CI26.
             CLOSE DBTRAN.
             DISPLAY "Reading :" AT 1412.
             DISPLAY "Writing :" AT 1612.
      *
      *      R E N A M E   D B T R A N   F I L E
      *
             CALL "CBL_RENAME_FILE" USING W02-DBTRAN W02-DBTOLD RETURNING WS-STATUS.
           IF NOT(WS-STATUS = X"0000")
               MOVE "Unable to Rebuild TRANSACTION file" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CI999.

       CI28.
             OPEN INPUT DBTOLD.
           IF WS-STAT1 = "9"
               IF FLE-LOCKED
                   DISPLAY "Waiting" AT 5062 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK
                   GO TO CI28.
           IF WS-STATUS NOT = "00"
               MOVE 55               TO WS-F-ERROR
               PERFORM OPEN-ERROR.
             PERFORM CLEAR-L50.

       CI29.
             OPEN OUTPUT DBTRAN.
           IF WS-STAT1 = "9"
               IF FLE-LOCKED
                   DISPLAY "Waiting" AT 5062 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK
                   GO TO CI29.
           IF WS-STATUS NOT = "00"
               MOVE 5                TO WS-F-ERROR
               PERFORM OPEN-ERROR.
             PERFORM CLEAR-L50.

       CI30.
             READ DBTOLD AT END GO TO CI32.
             DISPLAY TRO-AC       AT 1422 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                     " " TRO-DATE         WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      *
      *    THIS ROUTINE WRITES THE DETAILS TO THE NEW FILE.
      *
             MOVE TRO-RECORD1        TO TRA-RECORD1.
             DISPLAY TRA-AC       AT 1622 WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     " " TRA-DATE         WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             PERFORM WRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
             GO TO CI30.

       CI32.
             CLOSE DBTRAN
                   DBTOLD.
      *
      *      D E L E T E   O L D   D B T R A N   F I L E
      *
             CALL "CBL_DELETE_FILE" USING W02-DBTOLD RETURNING WS-STATUS.

       CI33.
             OPEN I-O DBTRAN.
             PERFORM AY70 THRU AY999.
             PERFORM ERASE-SCREEN.
             DISPLAY "Delete entries from INVOICE Log file Y/N > <" AT 0510.
             MOVE "N"                TO WS-OPTION.

       CI040.
             ACCEPT WS-OPTION AT 0552 WITH FOREGROUND-COLOR Cyan HIGHLIGHT AUTO.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-OPTION = "Y" OR "N")
               GO TO CI040.
           IF WS-OPTION = "N"
               GO TO CI999.
             DISPLAY "Enter Date from which INVOICES are to be kept DD/MM/CCYY" AT 0710.

       CI045.
             ACCEPT S28.
             MOVE W90-IDTE           TO W10-EDTE.
             PERFORM CHECK-DATE.
           IF (WS-ERROR = 1) OR (W22-DTE1 > W12-T-YMD)
               MOVE "Invalid Date"   TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO CI045.
             DISPLAY "Invoice :" AT 0910.
             DISPLAY "Deleting Invoices no longer required" AT 2310 WITH FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
             PERFORM AY60 THRU AY999.
             OPEN I-O INVOICE.
             MOVE 00000001           TO DOC-DTE.
             PERFORM START-AT-INVOIC-DATE THRU READ-INVOIC-EXIT.
           IF WS-F-ERROR = 12
               GO TO CI060.

       CI055.
             PERFORM READ-INVOIC-NEXT THRU READ-INVOIC-EXIT.
           IF WS-F-ERROR = 12
               GO TO CI060.
           IF NOT(DOC-DTE < W22-DTE1)
               GO TO CI060.
             DISPLAY DOC-REF AT 0920 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             PERFORM DELETE-INVOIC-REC THRU WRITE-INVOIC-EXIT.
             GO TO CI055.

       CI060.
             PERFORM AY70 THRU AY999.
             GO TO CI999.

       CI065.
             DISPLAY "Transaction :" AT 1912.
             MOVE ZERO               TO DEB-CUR DEB-30 DEB-60 DEB-90 DEB-120 DEB-INT.
             MOVE DEB-ACNO           TO TRA-AC.
             MOVE ZERO               TO TRA-DATE TRA-CODE.
             MOVE SPACES             TO TRA-REF.
             PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
           IF WS-F-ERROR = 5
               GO TO CI80.

       CI70.
             PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
           IF (WS-F-ERROR = 5) OR
              (DEB-ACNO NOT = TRA-AC)
               GO TO CI80.
             DISPLAY     TRA-DATE AT 1926 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                     " " TRA-CODE         WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     " " TRA-REF          WITH FOREGROUND-COLOR Grey HIGHLIGHT.
           IF TRA-VALUE < ZERO
               IF (TRA-VALUE + TRA-ALLOCATED) = ZERO
                   GO TO CI75
               ELSE
                   ADD TRA-ALLOCATED TO TRA-VALUE
                   MOVE ZERO         TO TRA-ALLOCATED
           ELSE
               IF TRA-VALUE = TRA-PAID
                   GO TO CI75
               ELSE
                   SUBTRACT TRA-PAID FROM TRA-VALUE
                   MOVE ZERO         TO TRA-PAID.
           IF TRA-VALUE > ZERO
               IF NOT(TRA-DATE > W30-120)
                   MOVE 5            TO TRA-AGE
               ELSE
               IF NOT(TRA-DATE > W30-90)
                   MOVE 4            TO TRA-AGE
               ELSE
               IF NOT(TRA-DATE > W30-60)
                   MOVE 3            TO TRA-AGE
               ELSE
               IF NOT(TRA-DATE > W30-30)
                   MOVE 2            TO TRA-AGE
               ELSE
               IF NOT(TRA-DATE > W30-CUR)
                   MOVE 1            TO TRA-AGE
               ELSE
                   MOVE ZERO         TO TRA-AGE.
           IF TRA-CODE = 17 OR 20
               ADD TRA-VALUE         TO DEB-INT
           ELSE
               EVALUATE TRA-AGE
                 WHEN 5        ADD TRA-VALUE     TO DEB-120
                 
                 WHEN 4        ADD TRA-VALUE     TO DEB-90
                 
                 WHEN 3        ADD TRA-VALUE     TO DEB-60
                 
                 WHEN 2        ADD TRA-VALUE     TO DEB-30
                 
                 WHEN 1        ADD TRA-VALUE     TO DEB-CUR
               END-EVALUATE.
             PERFORM REWRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
             GO TO CI70.

       CI75.
             PERFORM DELETE-DBTRAN-REC THRU WRITE-DBTRAN-EXIT.
             GO TO CI70.

       CI80.
             PERFORM CI20-AGE.
             GO TO CI20X.

       CI90.
             DISPLAY "Restart at A/C No." AT 2012.
             MOVE SPACES             TO W80-ACNO.

       CI95.
             DISPLAY "F1"                        AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     "=Help,"                            WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                     "F2"                                WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     " to do Account Lookup,"            WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                     "F9"                                WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     " Debtor Lookup (Any Word),"        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                     "Esc"                               WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     " to exit"                          WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             ACCEPT W80-ACNO AT 2031 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  MOVE SPACES       TO W80-ACNO
                               GO TO CI100

                 WHEN F1-KEY   PERFORM HELP-ROUTINE

                 WHEN F2-KEY   MOVE SPACE        TO W40-LUP
                               PERFORM AA50-LOOKUP

                 WHEN F9-KEY   MOVE "A"          TO W40-LUP
                               PERFORM AA50-LOOKUP

                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO CI95
               END-EVALUATE
               DISPLAY W80-ACNO AT 2031 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
               IF W80-ACNO = SPACES
                   GO TO CI95.
           IF W80-ACNO = SPACES
               GO TO CI100.
             PERFORM CLEAR-L50.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W80-ACNO BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             MOVE W80-ACNO           TO DEB-ACNO.
             PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               GO TO CI95.
             DISPLAY DEB-NAME AT 2038 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.

       CI96.
             MOVE "'C'ontinue or 'R'ekey  [ ]" TO WS-OPT-MES.
             MOVE "C"                TO WS-OPTION.
             PERFORM OPT-MESSAGE.
           IF NOT(WS-OPTION = "C" OR "R")
               GO TO CI96.
           IF WS-OPTION = "R"
               GO TO CI95.

       CI100.
             EXIT.

       CI999.
             EXIT.
      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT        SECTION 8.
       ZA000-OPEN.
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

       ZA00A-CONTINUE.
             MOVE 1                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY        TO W95-COMP.
      *      MOVE PAR-DMY            TO W12-TODAY.
      *      MOVE PAR-YMD            TO W12-T-YMD.
      *    *************************************************************
      *    ****    Create paths and file names for history files    ****
      *    *************************************************************
             MOVE W12-YEAR           TO W05-TYY W05-DYY.
             MOVE W12-MONTH          TO W05-TMM W05-DMM.
             INSPECT W02-DBHIST REPLACING FIRST "AR_ACNT.DAT"  BY "           ".
             MOVE W02-DBHIST         TO W60-PATH.
             STRING W60-PATH DELIMITED BY " " W05-DEB-HIS DELIMITED SIZE INTO W02-DBHIST.
             INSPECT W02-DTHIST REPLACING FIRST "AR_TRAN.DAT"  BY "           ".
             MOVE W02-DTHIST         TO W60-PATH.
             STRING W60-PATH DELIMITED BY " " W05-DBT-HIS DELIMITED SIZE INTO W02-DTHIST.
      *
      *      G E T   D E B T O R   M O N T H - E N D   D A Y
      *
             MOVE 1                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ The month end dates are stored in the Control file in the ³
      *    ³ first record as Cur, 30d, 60d, 90d and 120d and the next  ³
      *    ³ routine is used since Ver 8.11.06.          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *    *************************************************************
      *      Check the Current age date to see if it has already been
      *      updated.
      *    *************************************************************
           IF DEB-CUR-DTE = W12-T-YMD
               MOVE DEB-CUR-DTE      TO W30-CUR
               MOVE DEB-30D-DTE      TO W30-30
               MOVE DEB-60D-DTE      TO W30-60
               MOVE DEB-90D-DTE      TO W30-90
               MOVE DEB-120-DTE      TO W30-120
           ELSE
               MOVE DEB-90D-DTE      TO W30-120
               MOVE DEB-60D-DTE      TO W30-90
               MOVE DEB-30D-DTE      TO W30-60
               MOVE DEB-CUR-DTE      TO W30-30
               MOVE W12-T-YMD        TO W30-CUR.
               PERFORM ZA10 THRU ZA30.
             GO TO ZA999-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Check if the history files were created on a previous     ³
      *    ³ execution of this program. The details will have been     ³
      *    ³ changed and so can not be created again.                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       ZA10.
             MOVE 2                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF PAR-HIST-ID = W05-HIST-ID
               GO TO ZA30.
             OPEN OUTPUT DBHIST.
             CLOSE DBHIST.
             OPEN I-O DBHIST.
             OPEN OUTPUT DTHIST.
             CLOSE DTHIST.
             OPEN I-O DTHIST.
             MOVE " !"   TO DEB-NAME.
             PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               GO TO ZA30.
             PERFORM SAVE-SCREEN-3.
             MOVE 10                 TO SHADE-ROW.
             MOVE 16                 TO SHADE-COL.
             MOVE 52                 TO SHADE-WIDTH.
             MOVE 4                  TO SHADE-LINES.
             DISPLAY S25.
             PERFORM SCREEN-SHADOW.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Ceate a dummy account on history file with details for   ³
      *    ³  printing copy statements and other reports. This will be ³
      *    ³  first record in the history account file.                ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             INITIALIZE DBH-RECORD1.
             MOVE "  !"              TO DBH-NAME.
             MOVE "     !"           TO DBH-ACNO.
             MOVE W30-AGE-DATES      TO DBH-W30-AGE-DATES.
             MOVE W12-TODAY          TO DBH-STM-DTE.
             WRITE DBH-RECORD1.

       ZA15.
             PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               GO TO ZA25.
             DISPLAY DEB-ACNO AT 1124 WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Magenta.
             DISPLAY DEB-NAME AT 1131 WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Magenta.
             MOVE DEB-RECORD1        TO DBH-RECORD1.
             WRITE DBH-RECORD1.
           IF WS-STAT1 NOT = "0"
               MOVE 56               TO WS-F-ERROR
               PERFORM WRITE-ERROR.
             MOVE DEB-ACNO           TO TRA-AC.
             MOVE ZERO               TO TRA-DATE TRA-CODE.
             MOVE SPACES             TO TRA-REF.
             PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
           IF WS-F-ERROR = 5
               GO TO ZA15.

       ZA20.
             PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
           IF (WS-F-ERROR = 5) OR (TRA-AC NOT = DEB-ACNO)
               GO TO ZA15.
             DISPLAY TRA-DATE AT 1228 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             DISPLAY TRA-CODE AT 1237 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             DISPLAY TRA-REF  AT 1240 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             MOVE TRA-RECORD1        TO TRH-RECORD1
             WRITE TRH-RECORD1.
           IF WS-STAT1 NOT = "0"
               MOVE 55               TO WS-F-ERROR
               PERFORM WRITE-ERROR.
             GO TO ZA20.

       ZA25.
             MOVE W05-HIST-ID        TO PAR-HIST-ID.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             CLOSE DBHIST DTHIST.
             PERFORM RESTORE-SCREEN-3.

       ZA30.
             EXIT.

       ZA999-EXIT.
             EXIT.
      *
      *      I / O   E R R O R   M E S S A G E S
      *
       ZB000-ERROR      SECTION 8.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 2          MOVE W02-NETWORK  TO WS-FILE
                               MOVE WS-NETKEY    TO WS-KEY

               WHEN 5          MOVE W02-DBTRAN   TO WS-FILE
                               MOVE TRA-KEY      TO WS-KEYX

               WHEN 6          MOVE W02-DEBTOR   TO WS-FILE
                               MOVE DEB-ACNO     TO WS-KEYX

               WHEN 12         MOVE W02-INVOIC   TO WS-FILE
                               MOVE DOC-KEY      TO WS-KEYX

               WHEN 15         MOVE W02-PARAM    TO WS-FILE
                               MOVE WS-PARKEY    TO WS-KEY

               WHEN 18         MOVE W02-RECOVER  TO WS-FILE
                               MOVE WS-RECKEY    TO WS-KEY

               WHEN 37         MOVE W02-SHARED   TO WS-FILE
                               MOVE WS-SHARED    TO WS-KEY

               WHEN 55         MOVE W02-DBTOLD   TO WS-FILE
                               MOVE TRO-KEY      TO WS-KEYX

               WHEN 56         MOVE W02-DBHIST   TO WS-FILE
                               MOVE DBH-ACNO     TO WS-KEYX

               WHEN OTHER      MOVE "ERROR"      TO WS-FILE
                               MOVE "DTP006"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
