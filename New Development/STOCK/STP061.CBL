      $set LINKCOUNT"512" GNT"STP061.GNT"
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
      *    ******   ********  *******     ****     ******      **      *
      *   **    **     **     **    **   **  **   **    **    ***      *
      *   **           **     **    **  **    **  **           **      *
      *    ******      **     *******   **    **  ********     **      *
      *         **     **     **        **    **  **    **     **      *
      *   **    **     **     **         **  **   **    **     **      *
      *    ******      **     **          ****     ******   ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S T O C K   A D D   A   L O C A T I O N   W / H O U S E    *
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
       PROGRAM-ID.     STP061.
       AUTHOR.         J.W.LEMMON.
       DATE-WRITTEN.   APRIL 1996.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1996 - 2018
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

       COPY "APACFIDS.SL".

       COPY "CONTROL.SL".

       COPY "PARAM.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

       COPY "WARHSE.SL".

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

       COPY "APACFIDS.FDE".

       COPY "CONTROL.FDE".

       COPY "PARAM.FDE".

       COPY "RECOVER.FD".

       COPY "SHARED.FDE".

       COPY "WARHSE.FDE".

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
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S5                   PIC  9(04)    COMP-5.
       77  WS-S6                   PIC  9(04)    COMP-5.
       77  WS-S7                   PIC  9(04)    COMP-5.
       77  WS-S8                   PIC  9(04)    COMP-5.
       77  WS-IXS                  PIC  9(04)    COMP-5.
       77  WS-IXS1                 PIC  9(04)    COMP-5.
       77  WS-IXS2                 PIC  9(04)    COMP-5.
       77  WS-IXS3                 PIC  9(04)    COMP-5.
       77  WS-IXS4                 PIC  9(04)    COMP-5.
       77  WS-PARKEY               PIC  9(06)    COMP-5.
       77  WS-NETKEY               PIC  9(06)    COMP-5.
       77  WS-RECKEY               PIC  9(06)    COMP-5.
       77  WS-RECOVER              PIC  9(06)    COMP-5.
       77  WS-RECORDS              PIC  9(06)    COMP-5.
       77  WS-SHARED               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-TRANS                PIC  9(06)    COMP-5 VALUE 1.
       77  WS-STOCK                PIC  X(01).
       77  WS-ETYPE                PIC  X(01).
       77  WS-OPTION               PIC  X(01).
       77  WS-SKIP                 PIC  X(01).
       77  WS-ADJ                  PIC  X(01).
       77  WS-ERROR                PIC  9(01).
       77  WS-AMEND                PIC  9(01)            VALUE 0.
       77  WS-CONREC               PIC  9(01)            VALUE ZERO.
       77  WS-LEDG                 PIC  X(04).
       77  WS-COMP                 PIC  9(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-PASSWORD             PIC  X(08).
       77  WS-PRINT                PIC  9(01).
       77  WS-PASS                 PIC  X(08).
       77  WS-DESC-H1              PIC  X(14)            VALUE "Description".
       77  WS-DESC-H2              PIC  X(14)            VALUE "Description 2".
       77  WS-RT-HD                PIC  X(10)            VALUE "-Retail   ".
       77  WS-WS-HD                PIC  X(10)            VALUE "-Wholesale".
       77  WS-CS-HD                PIC  X(10)            VALUE "-Cash sale".
       77  WS-ERR3                 PIC  X(15)            VALUE "Data incomplete".
       77  WS-ERR7                 PIC  X(23)            VALUE "Address > 67 characters".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.
       77  PRG-NAME                PIC  X(12)            VALUE SPACES.
       77  PRG-WARHS-LUP           PIC  X(12)            VALUE "STP\WARLOOK".
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.       ³
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STP061".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY ADD A W/HOUSE LOCATION".
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
           03  WS-MODULE           PIC  X(03) VALUE "STP".
           03  WS-PROG             PIC  X(03) VALUE "061".

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

       01  W10-LOOKUP.
           03  W10-ETYPE           PIC  X(01).

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

       COPY "W40.WS".

       01  W40-WAREHOUSE.
           03  W40-NAME1.
               05  W40-NAME        PIC  X(40).
           03  W40-NAME2                      REDEFINES W40-NAME1.
               05  W40-NCHAR       PIC  X(01) OCCURS 40.
           03  W40-CONTACT         PIC  X(30).
           03  W40-CODE            PIC  X(06).
           03  W40-ADD.
               05  W40-ADDRESS1.
                   07  W40-ADD1    PIC  X(30).
                   07  W40-ADD2    PIC  X(30).
                   07  W40-ADD3    PIC  X(30).
                   07  W40-ADD4    PIC  X(30).
               05  W40-ADDRESS2               REDEFINES W40-ADDRESS1.
                   07  W40-ACHAR   PIC  X(01) OCCURS 120.
           03  W40-POST            PIC  9(04).
           03  W40-TEL             PIC  X(14).
           03  W40-FAX             PIC  X(14).

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W60-ADD.
           03  W60-ADDRESS.
               05  W60-ACHAR       PIC  X(01) OCCURS 70.
           03  W60-ADDR                       REDEFINES W60-ADDRESS.
               05  W60-ADD         PIC  X(60).
               05  FILLER          PIC  X(10).

       COPY "W75.WAR".

       01  W95-COMP                PIC  X(40).

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

       01  S01.
           03  LINE  2 COLUMN 32 VALUE                            "WAREHOUSE DETAILS" FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE  4 COLUMN  5 VALUE "Code     ".
           03  LINE  5 COLUMN  5 VALUE "Name     ".
           03  LINE  6 COLUMN  5 VALUE "Contact  ".
           03  LINE  7 COLUMN 15 VALUE           "Address  ".
           03  LINE  8 COLUMN  5 VALUE "Line 1   ".
           03  LINE  9 COLUMN  5 VALUE "     2   ".
           03  LINE 10 COLUMN  5 VALUE "     3   ".
           03  LINE 11 COLUMN  5 VALUE "     4   ".
           03  LINE 12 COLUMN  5 VALUE "P/Code   ".
           03  LINE 13 COLUMN  5 VALUE "Telephone".
           03  LINE 14 COLUMN  5 VALUE "Fax      ".

       01  S02.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               05 PIC X(06) USING W40-CODE AUTO.

       01  S03.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE  5 COLUMN 15 PIC X(40) USING W40-NAME    AUTO.
               05  LINE  6 COLUMN 15 PIC X(30) USING W40-CONTACT AUTO.
               05  LINE  8 COLUMN 15 PIC X(30) USING W40-ADD1    AUTO.
               05  LINE  9 COLUMN 15 PIC X(30) USING W40-ADD2    AUTO.
               05  LINE 10 COLUMN 15 PIC X(30) USING W40-ADD3    AUTO.
               05  LINE 11 COLUMN 15 PIC X(30) USING W40-ADD4    AUTO.
               05  LINE 12 COLUMN 15 PIC 9(04) USING W40-POST    AUTO.
               05  LINE 13 COLUMN 15 PIC X(14) USING W40-TEL     AUTO.
               05  LINE 14 COLUMN 15 PIC X(14) USING W40-FAX     AUTO.

       01  S-WLUP.
           03   BACKGROUND-COLOR Cyan FOREGROUND-COLOR BLUE.
                05  LINE 50 COLUMN 02 VALUE  "F1".                                            FOREGROUND-COLOR Brown HIGHLIGHT.
                05          COLUMN 04 VALUE    "=Help,".
                05          COLUMN 02 VALUE          "F2"                                     FOREGROUND-COLOR Brown HIGHLIGHT.
                05          COLUMN 02 VALUE            " to do Warehouse Lookup,".
                05          COLUMN 02 VALUE                                    "Esc"          FOREGROUND-COLOR Brown HIGHLIGHT.
                05          COLUMN 02 VALUE                                       " to exit".

       COPY "WARLUP.CRT".

       COPY "S99.CRT".

       COPY "ERROR.CRT".

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
      *
      *   **** Check the User's security level for Stock/Inventory
      *
           IF LS0-STLEV < 3
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM CB000.
             CLOSE RECOVER.
       AA49.
             EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
       AA50.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³          Save the screen contents and attributes.         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             PERFORM SAVE-SCREEN.
      *      MOVE 4                  TO W44-FUNCTION.
             PERFORM SCREEN-CONTENTS.
             MOVE PRG-WARHS-LUP     TO PRG-NAME.
             CALL PRG-NAME USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS W15-INPUT-AREA
	                 ON EXCEPTION GO TO AA55
             END-CALL.
             CANCEL PRG-NAME.
             GO TO AA58.

       AA55.
             MOVE SPACES             TO WS-ERR-MES.
             STRING "Program- " DELIMITED SIZE PRG-NAME DELIMITED " " " not on disk, press ANY key" DELIMITED SIZE INTO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.

       AA58.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³       Restore the screen contents and attributes.         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             PERFORM RESTORE-SCREEN.

       AA59.
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
       AC000              SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "SHARED.RD".

       COPY "WARHSE.RD".

      *
      *   R E W R I T E   &    W R I T E   F I L E S   R O U T I N E S
      *
       AD000             SECTION.

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

       COPY "SHARED.WR".

       COPY "WARHSE.WR".

       COPY "WARHSE.LUP".

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
      *        ³ 0 = RECORD CHANGED          ÄÄ     (REWRITE)     ³
      *        ³ 1 = RECORD WAS ADDED        ÄÄ     (DELETE)      ³
      *        ³ 2 = RECORD WAS DELETED      ÄÄ     (WRITE)       ³
      *        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY000           SECTION.
      *    *************************************************************
      *    ****             P A R A M E T E R   F I L E             ****
      *    *************************************************************
       AY01.
             MOVE 01                 TO REC-FILE.
             MOVE WS-PARKEY          TO REC-KEY.
             MOVE PAR-RECORD1        TO REC-PARAM.
             GO TO AY50.
      *    *************************************************************
      *    ****             W A R E H O U S E   F I L E             ****
      *    *************************************************************
       AY051.
             MOVE 51                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-WARHSE-LOCK THRU READ-WARHSE-EXIT.
               MOVE WAR-RECORD1      TO REC-WARHSE.
             GO TO AY50.
      *    *************************************************************
      *    ****       N E T W O R K   (S H A R E D)   F I L E       ****
      *    *************************************************************
       AY37.
             MOVE 37                 TO REC-FILE.
             MOVE WS-SHARED          TO REC-KEY.
             MOVE SHR-RECORD         TO REC-NETWORK.
             GO TO AY50.
      *    *************************************************************
      *    ****               C O N T R O L   F I L E               ****
      *    *************************************************************
       AY39.
             MOVE 39                 TO REC-FILE.
             MOVE WS-NETKEY          TO REC-KEY.
             MOVE NET-RECORD         TO REC-NETWORK.
             GO TO AY50.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³              End transaction in recovery log              ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY49.
             MOVE 99                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE SPACES             TO REC-DETAIL.
             PERFORM AY50 THRU AY59.
             ADD 1                   TO WS-TRANS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If recovery records exceed 95 - Clear (Close and Open     ³
      *    ³ new). This allows for quicker recoveries when required.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
             MOVE SHR-STOCK          TO WS-STOCK.
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
      *     Q   F U L L  -  W A I T   F O R   A  C O U N T  O F   400
      *    *************************************************************
             DISPLAY "WAITING" AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red HIGHLIGHT.
             COMMIT.
             ACCEPT WS-STIME FROM TIME.
             MOVE 400                TO WS-WAIT.
             PERFORM LOCK-REC-LOOP.
             DISPLAY SPACE AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.
             GO TO AY60.

       AY61.
             MOVE "ST"               TO PAR-PROG(WS-USUB).
             MOVE LS-USER            TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the STOCK control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
           IF WS-STOCK = "Y"
               MOVE 3                TO WS-SHARED
               PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT
               PERFORM AY37 THRU AY59
               MOVE SHR-RECORD       TO NET-RECORD
           ELSE
               MOVE 3                TO WS-NETKEY
               PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT
               PERFORM AY39 THRU AY59.
               GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N P R O G R E S S
      *
       AY62.
           IF NOT(PAR-PROG(WS-SUB) = SPACES OR PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R   O R   S T O C K   F I L E S
      *           B E I N G   U P D A T E D
      *
               IF NOT(PAR-PROG(WS-SUB) = SPACES)
                   IF PAR-PROG(WS-SUB) = "DB" OR "DS" OR "CS" OR "ST"
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

       AY70.
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 3.
      *
           IF WS-STOCK = "Y"
               MOVE NET-RECORD       TO SHR-RECORD
               PERFORM REWRITE-SHARED THRU WRITE-SHARED-EXIT
           ELSE
               PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
               PERFORM AY49 THRU AY59.
             MOVE 1                  TO WS-USUB.

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
           IF LS0-POS NOT = 2
               CLOSE RECOVER
               OPEN I-O RECOVER.
             COMMIT.

       AY999.
             EXIT.
      *
      *    ****   G E T   W A R E H O U S E   R E C O R D
      *
       CA000         SECTION.
       CA00.
             MOVE 6                  TO W50-LENGTH.

       CA05.
             MOVE W40-CODE           TO W50-DATA.
           IF W50-1 = SPACE
               MOVE W50-5            TO W40-CODE
               GO TO CA05.
             CALL "CBL_TOUPPER" USING W50-DATA BY VALUE W50-LENGTH RETURNING W50-STATUS.
             MOVE W50-DENT           TO W40-CODE.

       CA06.
             MOVE W50-DENT           TO WAR-CODE.

       CA10.
             PERFORM READ-WARHSE THRU READ-WARHSE-EXIT.

       CA99.
             EXIT.
      *
      *    ****    Pack the warehouse address for storage in the
      *            warehouse record
      *
       CA110-ADDRESS       SECTION 2.
       CA110.
             MOVE ZERO               TO WS-ERROR.
             MOVE 1                  TO WS-S1 WS-S2.
             MOVE 30                 TO WS-S3.
             MOVE SPACES             TO W60-ADDRESS.

       CA115.
          IF W40-ACHAR (WS-S3) NOT = SPACE
              GO TO CA120.
           IF WS-S3 > WS-S2
               SUBTRACT 1            FROM WS-S3
               GO TO CA115.

       CA120.
             MOVE W40-ACHAR(WS-S2)   TO W60-ACHAR(WS-S1).
           IF WS-S2 = WS-S3
               GO TO CA125.
             ADD 1                   TO WS-S1O WS-S2.
           IF WS-S1 > 70
               MOVE 1                TO WS-ERROR
           ELSE
               GO TO CA120.

       CA125.
           IF WS-ERROR = 1
               GO TO CA130.
           IF WS-S1 < 70
               ADD 1                 TO WS-S1
               MOVE "*"              TO W60-ACHAR(WS-S1).

       CA130.
             ADD 1                   TO WS-S1.

       CA135.
             MOVE 31                 TO WS-S2.
             MOVE 60                 TO WS-S3.
             PERFORM CA115 THRU CA130.
           IF WS-ERROR = 1
               GO TO CA140-EXIT.
             MOVE 61                 TO WS-S2.
             MOVE 90                 TO WS-S3.
             PERFORM CA115 THRU CA130.
           IF WS-ERROR = 1
               GO TO CA140-EXIT.
             MOVE 91                 TO WS-S2.
             MOVE 120                TO WS-S3.
             PERFORM CA115 THRU CA130.

       CA140-EXIT.
             MOVE ZERO               TO WS-S6.

       CA140.
             EXIT.
      *
      *    ****   C R E A T E   N E W   W A R E H O U S E   R E C O R D
      *
       CB000   SECTION 50.
       CB00.
            PERFORM ERASE-SCREEN.
            DISPLAY S01.
            MOVE SPACES              TO W40-NAME1 W40-CONTACT W40-CODE W40-ADDRESS1 W40-TEL W40-FAX.
            MOVE ZERO                TO W40-POST.
            DISPLAY S03.
            DISPLAY S-WLUP.

       CB05.
             ACCEPT S02 AT 0415.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  GO TO CB999
                 WHEN F1-KEY   PERFORM HELP-ROUTINE
                 WHEN F2-KEY   PERFORM AA50
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO CB05
               END-EVALUATE
               DISPLAY S02 AT 0415
               GO TO CB05.
            PERFORM CLEAR-L50.
          IF W40-CODE = SPACE
              GO TO CB999.
      PERFORM CA000.
          IF WS-F-ERROR = ZERO
              MOVE "Item exists" TO WS-ERR-MES
        PERFORM ERROR-MESSAGE
              GO TO CB05.

       CB10.
            ACCEPT S03.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
       MOVE SPACES TO W40-CODE
       GO TO CB999
                 WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN OTHER
                     PERFORM AA900-ALARM
               END-EVALUATE
              GO TO CB10.
            PERFORM CLEAR-L50.
             PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
              GO TO CB05.
    IF W40-NAME = SPACES OR
              W40-ADD1 = SPACE OR
              W40-POST = ZERO
              MOVE WS-ERR3  TO WS-ERR-MES
        PERFORM ERROR-MESSAGE
              GO TO CB10.
      MOVE 40   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W40-NAME
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
             PERFORM CA110-ADDRESS.
          IF WS-ERROR = 1
              MOVE WS-ERR7  TO WS-ERR-MES
        PERFORM ERROR-MESSAGE
              GO TO CB10.
      INITIALIZE WAR-RECORD1.
             MOVE W40-CODE  TO WAR-CODE.
             MOVE W40-NAME  TO WAR-NAME.
             MOVE W40-CONTACT  TO WAR-CONTACT.
             MOVE W60-ADDRESS  TO WAR-ADDRESS.
             MOVE W40-POST  TO WAR-POST.
             MOVE W40-TEL  TO WAR-TEL.
             MOVE W40-FAX  TO WAR-FAX.
             PERFORM AY60 THRU AY999.
             PERFORM WRITE-WARHSE THRU WRITE-WARHSE-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY051 THRU AY59.
             PERFORM AY70 THRU AY999.
             GO TO CB00.

       CB999.
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
             OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
             MOVE 1              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY    TO W95-COMP.
      *      MOVE PAR-DMY        TO W12-TODAY.
      *      MOVE PAR-YMD        TO W12-T-YMD.

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
               WHEN 2     MOVE W02-NETWORK  TO WS-FILE
                          MOVE WS-NETKEY    TO WS-KEY
               WHEN 15    MOVE WS-PARID     TO WS-FILE
                          MOVE WS-PARKEY    TO WS-KEY
               WHEN 18    MOVE W02-RECOVER  TO WS-FILE
                          MOVE WS-RECKEY    TO WS-KEY
               WHEN 37    MOVE W02-SHARED   TO WS-FILE
                          MOVE WS-SHARED    TO WS-KEY
               WHEN 51    MOVE W02-WARHSE   TO WS-FILE
                          MOVE WAR-CODE     TO WS-KEYX
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STP061"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
