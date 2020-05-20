      $set LINKCOUNT"384" GNT"STPALT.GNT"
      *
      *       ONLY DELETE WHEN ALL BELOW IS COMPLETED  
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
      *  Accept and Display of Stk-code allows
      *                                               ___
      *         Display ITM, EXT and 14 Characters   |   |
      *                                               ---
      *                                               ___
      *         Accept ITM, EXT, 14 Characters and   |   |
      *            all the Numeric Formats 3 to 14    ---
      *
      *  Stock file
      *                                               ___       
      *         Use the new layout and W10.STK       |   |
      *                                               ---
      ******************************************************************
      *                                                                *
      *    ******    ********   *******      ****     **      ******** *
      *   **    **      **      **    **    **  **    **         **    *
      *   **            **      **    **   **    **   **         **    *
      *    ******       **      *******    ********   **         **    *
      *         **      **      **         **    **   **         **    *
      *   **    **      **      **         **    **   **         **    *
      *    ******       **      **         **    **   *******    **    *
      *                                                                *
      *   ENGLISH                                                      *
      *                                                                *
      *   This program is used to keep the Stock Alternate Index up-   *
      *   dated (Individual words in stock Description). Add new items *
      *   update existing entries and delete obsolete entries.         *
      *                                                                *
      *       Version 9.04.03 - September 2017                         *

      *                                                                *
      ******************************************************************
      *                                                                *
      * August 2008   - New file (STKALT) - Stock alternate index      *
      *                 included for lookups, using any word con-      *
      *                 tained in the Stock description.               *
      *                                                                *
      * November 2009 - Include words from Description 2 and from      *
      *                 the Item code (Some item codes are comprised   *
      *                 of individual words and these will be          *
      *                 included in the alternate Index)               *
      *                                                                *
      * Jan 2010  - Allow for two discounts for a Debtor on the same   *
      *             Sales Ledger code. Included two additional Debtor  *
      *             discount codes (11 and 12) each with a field to    *
      *             define which debtor discount code uses this as an  *
      *             alternate discount. The relevant Stock Item will   *
      *             have an indicator field to instruct the system to  *
      *             use the alternate discount. The alternate discount *
      *             is only applicable if the discount code for which  *
      *             this alternate discount is applicable is the code  *
      *             on the Debtor account record.                      *
      *                                                                *
      * Feb 2017 - Increased the number of Memo entries, of 60 bytes,  *
      *            allowed from 12 to 34 per stock record.             *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     STPALT.
       AUTHOR.         J.W.LEMMON.
       DATE-WRITTEN.   July 2007.

                COPYRIGHT NOTICE: COPYRIGHT (C) 2007 - 2018
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

       COPY "RECOVER.SL".

       COPY "STKALT.SL".

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
      *
      *   Using External file as the program is called by programs
      *   that are writing recovery details to the RECOVERY file
      *   at the time this program is required to update the entries
      *   for the Alternate Index file.
      *
       COPY "RECOVER.FDE".

       COPY "STKALT.FDE".

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
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-RECKEY               PIC  9(06)    COMP-5.
       77  WS-OPTION               PIC  X(01).

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
           03  CRT-TYPE            PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START           PIC  9(02) VALUE 5.
           03  CRT-END             PIC  9(02) VALUE 46.
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15) VALUE "STPALT".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY UPDATE ALTERNATE INDEX".
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

       COPY "W25.STK".

           03  W25-TIME            PIC  9(08).
           03  W25-TRED                          REDEFINES  W25-TIME.
               05  FILLER          PIC  9(02).
               05  W25-FACT        PIC  9(06).
               05  W25-DATE                      REDEFINES W25-FACT.
                   07  W25-YY      PIC  9(02).
                   07  W25-MM      PIC  9(02).
                   07  W25-DD      PIC  9(02).
           03  W25-CUR-CC          PIC  9(02)    COMP-5.
           03  W25-PRV-CC          PIC  9(02)    COMP-5.
           03  W25-NXT-CC          PIC  9(02)    COMP-5.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

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

       77  WS-ERROR                  PIC  9(01).
       77  WS-RECOVER                PIC  9(06)    COMP-5.
       77  WS-TRANS                  PIC  9(04)    COMP-5.
       77  W02-RECOVER               PIC  X(64).
       77  W02-STKALT                PIC  X(64).

       COPY "CHAIN.LS".

       01  STK-CODE.
           03  STK-ITEM.
               05  STK-PLU           PIC  9(14).
           03  STK-ITM               PIC  X(03).
           03  FILLER                PIC  X(01).

       01  STK-DESC.
           03  STK-DSC26.
               05  STK-DKEY          PIC  X(14).
               05  FILLER            PIC  X(12).
           03  FILLER                PIC  X(04).

       01  STK-DESC2.
           03  STK-DKEY2             PIC  X(14).
           03  FILLER                PIC  X(16).


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
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY W02-STKALT W02-RECOVER WS-RECOVER WS-TRANS STK-CODE STK-DESC STK-DESC2 WS-ERROR.
       AA000-MAIN              SECTION.
       AA000-INIT.
             PERFORM ZA000-INIT.
           IF WS-ERROR = 9
               GO TO AA99.
      *
      *      LS-INSTR:
      *               A = Amend entries
      *               C = Create new entries
      *               D = Delete existing entries
      *
             EVALUATE LS-INSTR
               WHEN "A"
                    PERFORM DELETE-STOCK-ALT-INDEX
                    PERFORM CREATE-STOCK-ALT-INDEX
               WHEN "C"
                    PERFORM CREATE-STOCK-ALT-INDEX
               WHEN "D"
                    PERFORM DELETE-STOCK-ALT-INDEX
             END-EVALUATE.

       AA99.
             EXIT PROGRAM.

      *    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *     S C R E E N ,   K E Y B O A R D   &  M O U S E
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3    ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³                      SCREEN-SHADOW                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To routine is used to display a shadow down the right and ³
      *    ³ along the bottom of a pop-up box. The parameters that are ³
      *    ³ required:                                                 ³
      *    ³          SHADE-ROW   - Top line of the box + 1.           ³
      *    ³          SHADE-COL   - Left line of box + 2.              ³
      *    ³          SHADE-WIDTH - Width of the box - 2.              ³
      *    ³          SHADE-LINES - Hight of box - 1.                  ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³                      CHECK-CORRECT                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine displays a pop-up window with the message    ³
      *    ³           "Press Y if correct - N if incorrect"           ³
      *    ³                                                           ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³                                                           ³
      *    ³ To start with the pop-up window higher or lower than row  ³
      *    ³ 13 (default); move another value to SLIN and PERFORM      ³
      *    ³ CHECK-SETUP THRU CHECK-EXIT.                              ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ****   T H I S   R O U T I N E   I S   U S E D   T O
      *         D I S P L A Y   E R R O R   M E S S A G E S
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
       AC000-READ              SECTION.

       COPY "STKALT.RD".

      *
      *   R E W R I T E   &    W R I T E   F I L E S   R O U T I N E S
      *
       AD000-WRITE             SECTION.

       COPY "STKALT.WR".

      *
      *       ****   *****   ***    ***   *   *  *****  ****   *   *
      *       *   *  *      *   *  *   *  *   *  *      *   *  *   *
      *       ****   ***    *      *   *  *   *  ***    ****    * *
      *       *   *  *      *   *  *   *   * *   *      *   *    *
      *       *   *  *****   ***    ***     *    *****  *   *    *
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *   THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *        ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *        ³ ORIGINAL ACTION (REC-TYPE)  ÄÄ  RECOVERY PROCESS ³
      *        ³ 0 = RECORD CHANGED          ÄÄ     (REWRITE)     ³
      *        ³ 1 = RECORD WAS ADDED        ÄÄ     (DELETE)      ³
      *        ³ 2 = RECORD WAS DELETED      ÄÄ     (WRITE)       ³
      *        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY000           SECTION.

       AY49S.
             MOVE 49                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
             MOVE STKA-RECORD1       TO REC-STKALT.

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

      *
      *    ****    Generate the Stock Alternate Index entries
      *
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ This routine is  used to  create  an index of the various ³
      *    ³ words that are present in the Stock description.          ³
      *    ³ The first record will be used for the first 12 characters ³
      *    ³ of the Stock description as it is for description index   ³
      *    ³ of the Stock record. All Stock records types will have an ³
      *    ³ entry for each word that appears in the description. The  ³
      *    ³ following procedure will be applied;                      ³
      *    ³    Find the next word and move the next 12 characters to  ³
      *    ³    index key. Repeat until the end of the description has ³
      *    ³    been reached. This procedure allows for words such as; ³
      *    ³    the, and, etc to have the word following as part of    ³
      *    ³    the key which reduces the duplicate keys and gives a   ³
      *    ³    better index spread.                                   ³
      *    ³ This routine can be altered as required.                  ³
      *    ³ See also STOCK.DSC (Does not log to recovery file).       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *                                                                *
      *  Aug 2008 - New file (STKALT) - Stock alternate index included *
      *     for lookups, using any word contained in the Stock *
      *     description.                                       *
      *                                                                *
      ******************************************************************
       CREATE-STOCK-ALT-INDEX   SECTION 6.
       CREATE-ALT-STOCK-INDEX.
             DISPLAY "Generating Alternate Description Index" AT 5018 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
      *
      *    ****    Initialize the extended alpha index record.
      *
             MOVE STK-CODE           TO STKA-CODE.
             MOVE ZERO               TO STKA-NO.
             MOVE STK-DESC           TO W50-DATA STKA-DESC STKA-DKEY.
      *
      *    ****    Write the first entry of the Alternate Index.
      *            This is the same as the Alpha Index entry for the
      *            Stock record.
      *
             PERFORM WRITE-STKALT THRU WRITE-STKALT-EXIT.
      *
      *    ****   Log to recovery file.
      *
             MOVE 1                  TO WS-ACTION.
             PERFORM AY49S THRU AY59.
             MOVE 1                  TO WS-S1.
             MOVE 30                 TO WS-S3.
      *
      *    ****    Find the length of the Stock description (characters).
      *            Look for first non space character - starting with 30
      *            and working back. (WS-S3)
      *
       STOCK-FIND-LENGTH.
           IF W50-CCHAR(WS-S3) = SPACE
               SUBTRACT 1            FROM WS-S3
               GO TO STOCK-FIND-LENGTH.
             MOVE ZERO               TO WS-S4.
      *
      *    ****    Find the next word in the description.
      *
       STOCK-NEXT-WORD.
      *
      *    ****    Check if end of word reached and add 1 to the number
      *            of words found (WS-S4). This is used, together with
      *            the Item code, to provide a unique key for the
      *            STKALT records.
      *
           IF W50-CCHAR(1) = SPACE
      *
      *    ****    Move data, one character to the left and reduce the
      *            number of characters left in the description.
      *
               MOVE W50-D39          TO W50-DATA
               SUBTRACT 1            FROM WS-S3
               ADD 1                 TO WS-S4
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If it is necessary to exclude words from the alternate    ³
      *    ³ index, such as '(PTY)', 'LTD' etc, then this would be the ³
      *    ³ the point to perform the routine to exclude the word.     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               GO TO STOCK-STORE-NEXT.
      *
      *    ****    Move data, one character to the left and reduce the
      *            number of characters left in the description.
      *
             MOVE W50-D39            TO W50-DATA.
             SUBTRACT 1              FROM WS-S3.
           IF WS-S3 > 0
               GO TO STOCK-NEXT-WORD.
      *
      *    ****  No more words left in the description.
      *
             GO TO STOCK-DESC-INDEX-EXIT.

       STOCK-STORE-NEXT.
             MOVE WS-S4              TO STKA-NO.
             MOVE W50-DATA           TO STKA-DKEY.
      *
      *    ****    Write an entry in the Alternate Index.
      *            This is an additional Alpha Index entry for the
      *            Stock Record.
      *
             PERFORM WRITE-STKALT THRU WRITE-STKALT-EXIT.
      *
      *    ****   Log to recovery file.
      *
             MOVE 1                  TO WS-ACTION.
             PERFORM AY49S THRU AY59.
           IF WS-S3 > 0
               GO TO STOCK-NEXT-WORD.

       STOCK-DESC-INDEX-EXIT.
             EXIT.


       DELETE-STOCK-ALT-INDEX   SECTION 6.
       DELETE-ALT-STOCK-INDEX.
      *
      *    ****    C H E C K   T H E   A L T E R N A T E   I N D E X
      *
             MOVE ZERO               TO WS-ERROR.
             MOVE STK-CODE           TO STKA-CODE.
             MOVE ZERO               TO STKA-NO.
             PERFORM READ-STKALT THRU READ-STKALT-EXIT.
           IF WS-F-ERROR = 49
               MOVE 9                TO WS-ERROR
               GO TO DELETE-ALT-STOCK-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If  either of the two descriptions or Item code has  been ³
      *    ³ amended, then  the existing Alternate Index  entries will ³
      *    ³ be deleted and new entries generated.                     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       DELETE-ALT-STOCK-LOOP.
             MOVE 2                  TO WS-ACTION.
             PERFORM AY49S THRU AY59.
             PERFORM DELETE-STKALT-REC THRU WRITE-STKALT-EXIT.
             PERFORM READ-STKALT-NEXT THRU READ-STKALT-EXIT.
           IF NOT(WS-F-ERROR = 49)
               IF (STKA-CODE = STK-CODE)
              GO TO DELETE-ALT-STOCK-LOOP.

       DELETE-ALT-STOCK-EXIT.
             EXIT.

      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT        SECTION.
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

       DISPLAY-FILE-NAME.
           IF WS-F-ERROR = 49
               MOVE W02-STKALT       TO WS-FILE
               MOVE ZERO             TO WS-KEY
               MOVE STKA-KEY         TO WS-KEYX
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STPALT"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
